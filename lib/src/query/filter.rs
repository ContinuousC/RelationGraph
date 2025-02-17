/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    convert::Infallible,
    fmt::Display,
    ops::{BitAndAssign, BitOrAssign, ControlFlow},
};

use replace_with::replace_with;
use serde_json::json;
use wrapper::Wrapper;

use crate::{
    ids::{ItemTypeId, RelationTypeId, TplVarId},
    items::resolved::{ItemRef, RelationRef},
    serial::StatusInfo,
    status::Status,
    Absolute, Augment, Augmented, ItemId, RelationId, RunQueryError, TplVarDef, TplVarStrings,
    TplVarValues,
};

use super::resolved::{
    Filters, QueryResultItems, TplVarItemIds, TplVarItemTypes, TplVarRelationIds,
    TplVarRelationTypes, TplVarValueRef,
};

/* Filter and matching traits. */

/// Trait implemented by the template filter.
pub(crate) trait Filter<'a, R: MatchResult> {
    /// Check wether a value matches the filter. The `match` field of
    /// the result can be undecided if the filter is not active.
    fn matches(&self, var: &'a TplVarId, value: Option<TplVarValueRef<'a>>) -> R;
}

/// Trait implemented by the filter on the wrapper element.
pub trait WrapperFilter<W: Wrapper, R: MatchResult> {
    fn is_filtered(&self) -> bool;
    fn matches<T>(&self, item: &W::Wrap<T>) -> R;
}

#[allow(unused_variables)]
pub(crate) trait RunState<R: MatchResult, W: Wrapper> {
    type Error;

    fn add_item(&mut self, item_id: ItemId, item_ref: ItemRef<W>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn add_relation(
        &mut self,
        rel_id: RelationId,
        rel_ref: RelationRef<W>,
        invert: bool,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn add_match(&mut self, result: R) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub trait MatchResult: Default + From<Option<bool>> + Sized {
    fn short_circuit_and(self) -> ControlFlow<Self, Self> {
        ControlFlow::Continue(self)
    }

    fn perform_and(self, other: Self) -> Self;
    fn neutral_and() -> Self;

    fn short_circuit_or(self) -> ControlFlow<Self, Self> {
        ControlFlow::Continue(self)
    }

    fn perform_or(self, other: Self) -> Self;
    fn neutral_or() -> Self;

    fn not(self) -> Self;

    fn matches(&self) -> bool;
    fn filtered(&self) -> bool;
    fn wrapper_filtered(&self) -> bool;

    fn any<T>(mut iter: T) -> Self
    where
        T: Iterator<Item = Self>,
    {
        match iter.try_fold(Self::neutral_or(), |a, b| {
            ControlFlow::Continue(a.perform_or(b.short_circuit_or()?))
        }) {
            ControlFlow::Continue(v) | ControlFlow::Break(v) => v,
        }
    }

    fn or<F>(self, f: F) -> Self
    where
        F: FnOnce() -> Self,
    {
        match self.short_circuit_or() {
            ControlFlow::Continue(a) => a.perform_or(f()),
            ControlFlow::Break(r) => r,
        }
    }

    fn all<T>(mut iter: T) -> Self
    where
        T: Iterator<Item = Self>,
    {
        match iter.try_fold(Self::neutral_and(), |a, b| {
            ControlFlow::Continue(a.perform_and(b.short_circuit_and()?))
        }) {
            ControlFlow::Continue(v) | ControlFlow::Break(v) => v,
        }
    }

    fn and<F>(self, f: F) -> Self
    where
        F: FnOnce() -> Self,
    {
        match self.short_circuit_and() {
            ControlFlow::Continue(a) => a.perform_and(f()),
            ControlFlow::Break(r) => r,
        }
    }
}

pub(super) trait MatchIter: Iterator<Item: MatchResult> + Sized {
    fn match_all(self) -> Self::Item {
        Self::Item::all(self)
    }

    fn match_any(self) -> Self::Item {
        Self::Item::any(self)
    }
}

impl<T> MatchIter for T where T: Iterator<Item: MatchResult> {}

/* Filter implementations. */

/// Dummy filter: always return undecided, unfiltered.
impl<'a, R: MatchResult> Filter<'a, R> for () {
    fn matches(&self, _var: &'a TplVarId, _value: Option<TplVarValueRef<'a>>) -> R {
        None.into()
    }
}

/// Boolean match for template filters.
impl<'a> Filter<'a, Option<bool>> for Filters {
    fn matches(&self, var: &'a TplVarId, value: Option<TplVarValueRef<'a>>) -> Option<bool> {
        self.get(var).map(|vs| value.is_some_and(|v| vs.matches(v)))
    }
}

/// Template filters: return match, or undecided if filter is not
/// present.
impl<'a> Filter<'a, Match> for Filters {
    fn matches(&self, var: &'a TplVarId, value: Option<TplVarValueRef<'a>>) -> Match {
        if let Some(vs) = self.get(var) {
            Match::new(Some(value.is_some_and(|v| vs.matches(v)))).set_filtered(true)
        } else {
            None.into()
        }
    }
}

/// Template filters: return match, or undecided if filter is not
/// present.
impl<'a> Filter<'a, Matches<'a>> for Filters {
    fn matches(&self, var: &'a TplVarId, value: Option<TplVarValueRef<'a>>) -> Matches<'a> {
        if let Some(vs) = self.get(var) {
            Matches {
                matches: Match::new(Some(value.is_some_and(|v| vs.matches(v)))).set_filtered(true),
                opts: BTreeMap::new(),
            }
        } else {
            Matches {
                matches: None.into(),
                opts: BTreeMap::from_iter([(var, TplOpts::In(BTreeSet::from_iter(value)))]),
            }
        }
    }
}

/// Template filters with one filter var masked out.
pub(super) struct TplFilter<'a> {
    pub(super) filters: &'a Filters,
    pub(super) var: &'a TplVarId,
}

impl<'a> TplFilter<'a> {
    pub(super) fn new(filters: &'a Filters, var: &'a TplVarId) -> Self {
        Self { filters, var }
    }
}

impl<'a> Filter<'a, TplMatches<'a>> for TplFilter<'a> {
    fn matches(&self, var: &'a TplVarId, value: Option<TplVarValueRef<'a>>) -> TplMatches<'a> {
        if var == self.var {
            TplMatches {
                matches: Match::new(None).set_filtered(true),
                opts: TplOpts::In(BTreeSet::from_iter(value)),
            }
        } else {
            TplMatches {
                matches: self.filters.matches(var, value),
                opts: TplOpts::NotIn(BTreeSet::new()),
            }
        }
    }
}

/* WrapperFilter implementations. */

/// Dummy filter: always return undecided, unfiltered.
impl<W: Wrapper, R: MatchResult> WrapperFilter<W, R> for () {
    fn is_filtered(&self) -> bool {
        false
    }

    fn matches<T>(&self, _item: &W::Wrap<T>) -> R {
        None.into()
    }
}

impl<R: MatchResult + From<Match>> WrapperFilter<Augment<StatusInfo>, R> for Status {
    fn is_filtered(&self) -> bool {
        *self != Status::Ok
    }

    fn matches<T>(&self, item: &Augmented<T, StatusInfo>) -> R {
        let m = item
            .info
            .individual_status
            .as_ref()
            .map_or(Status::Ok, |s| s.value.status)
            >= *self;
        Match::new(None).set_wrapper_filtered(m).into()
    }
}

/* MatchResult implementations. */

/// Producing a bool result. In this mode, matches can short-circuit.
impl MatchResult for Option<bool> {
    fn short_circuit_and(self) -> ControlFlow<Self, Self> {
        match self {
            Some(false) => ControlFlow::Break(self),
            _ => ControlFlow::Continue(self),
        }
    }

    fn perform_and(self, other: Self) -> Self {
        match (self, other) {
            (Some(a), Some(b)) => Some(a && b),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        }
    }

    fn neutral_and() -> Self {
        None
    }

    fn short_circuit_or(self) -> ControlFlow<Self, Self> {
        match self {
            Some(true) => ControlFlow::Break(self),
            _ => ControlFlow::Continue(self),
        }
    }

    fn perform_or(self, other: Self) -> Self {
        match (self, other) {
            (Some(a), Some(b)) => Some(a || b),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        }
    }

    fn neutral_or() -> Self {
        None
    }

    fn not(self) -> Self {
        self.map(|v| !v)
    }

    fn matches(&self) -> bool {
        self.unwrap_or(true)
    }

    fn filtered(&self) -> bool {
        false
    }

    fn wrapper_filtered(&self) -> bool {
        false
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Default, Clone, Copy, Debug)]
pub struct Match {
    pub(crate) matches: Option<bool>,
    pub(crate) filtered: bool,
    pub(crate) wrapper_filtered: bool,
}

impl Match {
    pub(crate) fn new(matches: Option<bool>) -> Self {
        Self {
            matches,
            filtered: false,
            wrapper_filtered: false,
        }
    }

    pub(crate) fn set_filtered(mut self, filtered: bool) -> Self {
        self.filtered = filtered;
        self
    }

    pub(crate) fn set_wrapper_filtered(mut self, filtered: bool) -> Self {
        self.wrapper_filtered = filtered;
        self
    }
}

impl MatchResult for Match {
    fn neutral_and() -> Self {
        Self::new(None)
    }

    fn short_circuit_and(self) -> ControlFlow<Self, Self> {
        match self {
            Match {
                matches: Some(false),
                filtered: true,
                wrapper_filtered: true,
            } => ControlFlow::Break(self),
            _ => ControlFlow::Continue(self),
        }
    }

    fn perform_and(self, other: Self) -> Self {
        Self {
            matches: self.matches.perform_and(other.matches),
            filtered: self.filtered || other.filtered,
            wrapper_filtered: self.wrapper_filtered || other.wrapper_filtered,
        }
    }

    fn neutral_or() -> Self {
        Self::new(None)
    }

    fn short_circuit_or(self) -> ControlFlow<Self, Self> {
        match self {
            Match {
                matches: Some(true),
                filtered: true,
                wrapper_filtered: true,
            } => ControlFlow::Break(self),
            _ => ControlFlow::Continue(self),
        }
    }

    fn perform_or(self, other: Self) -> Self {
        Self {
            matches: self.matches.perform_or(other.matches),
            filtered: self.filtered || other.filtered,
            wrapper_filtered: self.wrapper_filtered || other.wrapper_filtered,
        }
    }

    fn not(self) -> Self {
        Self {
            matches: self.matches.map(|m| !m),
            filtered: self.filtered,
            wrapper_filtered: self.wrapper_filtered,
        }
    }

    fn matches(&self) -> bool {
        self.matches.unwrap_or(true)
    }

    fn filtered(&self) -> bool {
        self.filtered
    }

    fn wrapper_filtered(&self) -> bool {
        self.wrapper_filtered
    }
}

impl From<Option<bool>> for Match {
    fn from(matches: Option<bool>) -> Self {
        Self::new(matches)
    }
}

impl Display for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.matches {
            Some(true) => write!(f, "matched")?,
            Some(false) => write!(f, "unmatched")?,
            None => write!(f, "undecided")?,
        };
        if self.filtered {
            write!(f, ", filtered")?;
        }
        if self.wrapper_filtered {
            write!(f, ", wrapper filtered")?;
        }
        Ok(())
    }
}

#[derive(Default)]
pub struct Matches<'a> {
    matches: Match,
    opts: BTreeMap<&'a TplVarId, TplOpts<'a>>,
}

impl MatchResult for Matches<'_> {
    fn neutral_and() -> Self {
        Self {
            matches: Match::new(None),
            opts: BTreeMap::new(),
        }
    }

    fn neutral_or() -> Self {
        Self {
            matches: Match::new(None),
            opts: BTreeMap::new(),
        }
    }

    fn perform_and(self, other: Self) -> Self {
        Self {
            matches: self.matches.perform_and(other.matches),
            opts: {
                fn run<'a>(
                    mut a: BTreeMap<&'a TplVarId, TplOpts<'a>>,
                    b: BTreeMap<&'a TplVarId, TplOpts<'a>>,
                ) -> BTreeMap<&'a TplVarId, TplOpts<'a>> {
                    b.into_iter().for_each(|(v, b)| match a.entry(v) {
                        Entry::Vacant(ent) => {
                            ent.insert(b);
                        }
                        Entry::Occupied(mut ent) => {
                            *ent.get_mut() &= b;
                        }
                    });
                    a
                }
                if self.opts.len() < other.opts.len() {
                    run(other.opts, self.opts)
                } else {
                    run(self.opts, other.opts)
                }
            },
        }
    }

    fn perform_or(self, other: Self) -> Self {
        Self {
            matches: self.matches.perform_or(other.matches),
            opts: {
                fn run<'a>(
                    mut a: BTreeMap<&'a TplVarId, TplOpts<'a>>,
                    b: BTreeMap<&'a TplVarId, TplOpts<'a>>,
                ) -> BTreeMap<&'a TplVarId, TplOpts<'a>> {
                    b.into_iter().for_each(|(v, b)| match a.entry(v) {
                        Entry::Vacant(ent) => {
                            ent.insert(b);
                        }
                        Entry::Occupied(mut ent) => {
                            *ent.get_mut() |= b;
                        }
                    });
                    a
                }
                if self.opts.len() < other.opts.len() {
                    run(other.opts, self.opts)
                } else {
                    run(self.opts, other.opts)
                }
            },
        }
    }

    fn not(mut self) -> Self {
        Self {
            matches: self.matches.not(),
            opts: {
                self.opts.values_mut().for_each(|opts| {
                    replace_with(
                        opts,
                        || TplOpts::In(BTreeSet::new()),
                        |opts| match opts {
                            TplOpts::In(xs) => TplOpts::NotIn(xs),
                            TplOpts::NotIn(xs) => TplOpts::In(xs),
                        },
                    )
                });
                self.opts
            },
        }
    }

    fn matches(&self) -> bool {
        self.matches.matches()
    }

    fn filtered(&self) -> bool {
        self.matches.filtered()
    }

    fn wrapper_filtered(&self) -> bool {
        self.matches.wrapper_filtered()
    }
}

impl From<Match> for Matches<'_> {
    fn from(matches: Match) -> Self {
        Self {
            matches,
            opts: BTreeMap::new(),
        }
    }
}

impl From<Option<bool>> for Matches<'_> {
    fn from(value: Option<bool>) -> Self {
        Self {
            matches: Match::new(value),
            opts: BTreeMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct TplMatches<'a> {
    matches: Match,
    opts: TplOpts<'a>,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum TplOpts<'a> {
    In(BTreeSet<TplVarValueRef<'a>>),
    NotIn(BTreeSet<TplVarValueRef<'a>>),
}

impl Default for TplMatches<'_> {
    fn default() -> Self {
        Self {
            matches: Match::new(None),
            opts: TplOpts::NotIn(BTreeSet::new()),
        }
    }
}

impl TplOpts<'_> {
    pub(super) fn get_values(&self, typ: TplVarDef) -> TplVarValues {
        match typ {
            TplVarDef::Strings => TplVarValues::Strings(TplVarStrings(match self {
                TplOpts::In(vs) | TplOpts::NotIn(vs) => vs
                    .iter()
                    .filter_map(|v| v.get_string().map(|s| s.to_string()))
                    .collect(),
            })),
            TplVarDef::ItemTypes => TplVarValues::ItemTypes(TplVarItemTypes(match self {
                TplOpts::In(vs) | TplOpts::NotIn(vs) => vs
                    .iter()
                    .filter_map(|v| v.get_item_type().cloned())
                    .collect(),
            })),
            TplVarDef::RelationTypes => {
                TplVarValues::RelationTypes(TplVarRelationTypes(match self {
                    TplOpts::In(vs) | TplOpts::NotIn(vs) => vs
                        .iter()
                        .filter_map(|v| v.get_relation_type().cloned())
                        .collect(),
                }))
            }
            TplVarDef::ItemIds => TplVarValues::ItemIds(TplVarItemIds(match self {
                TplOpts::In(vs) | TplOpts::NotIn(vs) => {
                    vs.iter().filter_map(|v| v.get_item_id().cloned()).collect()
                }
            })),
            TplVarDef::RelationIds => TplVarValues::RelationIds(TplVarRelationIds(match self {
                TplOpts::In(vs) | TplOpts::NotIn(vs) => vs
                    .iter()
                    .filter_map(|v| v.get_relation_id().cloned())
                    .collect(),
            })),
        }
    }

    fn add_template_matches(&mut self, opts: Self) {
        match (self, opts) {
            (TplOpts::In(xs), TplOpts::In(ys)) => {
                xs.extend(ys);
            }
            (TplOpts::NotIn(xs), TplOpts::NotIn(ys)) => {
                xs.extend(ys);
            }
            (TplOpts::In(_), TplOpts::NotIn(_)) => {}
            (xs @ TplOpts::NotIn(_), mut ys @ TplOpts::In(_)) => std::mem::swap(xs, &mut ys),
        }
    }
}

impl From<Match> for TplMatches<'_> {
    fn from(matches: Match) -> Self {
        Self {
            matches,
            opts: TplOpts::NotIn(BTreeSet::new()),
        }
    }
}

impl<'a, W: Wrapper> RunState<TplMatches<'a>, W> for TplOpts<'a> {
    type Error = Infallible;

    // Add matches to the result, prefering positive matches.
    fn add_match(&mut self, result: TplMatches<'a>) -> Result<(), Infallible> {
        self.add_template_matches(result.opts);
        Ok(())
    }
}

impl BitOrAssign for TplOpts<'_> {
    fn bitor_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (TplOpts::NotIn(xs), TplOpts::NotIn(mut ys)) => {
                if xs.len() > ys.len() {
                    std::mem::swap(xs, &mut ys);
                }
                xs.retain(|v| ys.contains(v));
            }
            (TplOpts::In(xs), TplOpts::In(mut ys)) => {
                if xs.len() < ys.len() {
                    std::mem::swap(xs, &mut ys);
                }
                xs.extend(ys);
            }
            (TplOpts::NotIn(xs), TplOpts::In(ys)) => {
                if xs.len() < ys.len() {
                    xs.retain(|v| !ys.contains(v));
                } else {
                    ys.iter().for_each(|v| {
                        xs.remove(v);
                    });
                }
            }
            (xs @ TplOpts::In(_), mut ys @ TplOpts::NotIn(_)) => {
                std::mem::swap(xs, &mut ys);
                let (TplOpts::NotIn(xs), TplOpts::In(ys)) = (xs, ys) else {
                    unreachable!();
                };
                if xs.len() < ys.len() {
                    xs.retain(|v| !ys.contains(v));
                } else {
                    ys.iter().for_each(|v| {
                        xs.remove(v);
                    });
                }
            }
        }
    }
}

impl BitAndAssign for TplOpts<'_> {
    fn bitand_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (TplOpts::In(xs), TplOpts::In(mut ys)) => {
                if xs.len() > ys.len() {
                    std::mem::swap(xs, &mut ys);
                }
                xs.retain(|v| ys.contains(v));
            }
            (TplOpts::NotIn(xs), TplOpts::NotIn(mut ys)) => {
                if ys.len() > xs.len() {
                    std::mem::swap(xs, &mut ys);
                }
                xs.extend(ys);
            }
            (TplOpts::In(xs), TplOpts::NotIn(ys)) => {
                if xs.len() > ys.len() {
                    ys.into_iter().for_each(|v| {
                        xs.remove(&v);
                    });
                } else {
                    xs.retain(|v| !ys.contains(v));
                }
            }
            (xs @ TplOpts::NotIn(_), mut ys @ TplOpts::In(_)) => {
                std::mem::swap(xs, &mut ys);
                let (TplOpts::In(xs), TplOpts::NotIn(ys)) = (xs, ys) else {
                    unreachable!();
                };
                if xs.len() > ys.len() {
                    ys.into_iter().for_each(|v| {
                        xs.remove(&v);
                    });
                } else {
                    xs.retain(|v| !ys.contains(v));
                }
            }
        }
    }
}

impl MatchResult for TplMatches<'_> {
    fn neutral_and() -> Self {
        Self {
            matches: Match::neutral_and(),
            opts: TplOpts::NotIn(BTreeSet::new()),
        }
    }

    fn neutral_or() -> Self {
        Self {
            matches: Match::neutral_and(),
            opts: TplOpts::In(BTreeSet::new()),
        }
    }

    fn perform_and(mut self, other: Self) -> Self {
        Self {
            matches: self.matches.perform_and(other.matches),
            opts: {
                self.opts &= other.opts;
                self.opts
            },
        }
    }

    fn short_circuit_and(self) -> ControlFlow<Self, Self> {
        match &self {
            Self {
                matches:
                    Match {
                        matches: Some(false),
                        filtered: true,
                        wrapper_filtered: true,
                    },
                opts: TplOpts::In(vs),
            } if vs.is_empty() => ControlFlow::Break(self),
            _ => ControlFlow::Continue(self),
        }
    }

    fn short_circuit_or(self) -> ControlFlow<Self, Self> {
        match &self {
            Self {
                matches:
                    Match {
                        matches: Some(true),
                        filtered: true,
                        wrapper_filtered: true,
                    },
                opts: TplOpts::NotIn(vs),
            } if vs.is_empty() => ControlFlow::Break(self),
            _ => ControlFlow::Continue(self),
        }
    }

    fn perform_or(mut self, other: Self) -> Self {
        Self {
            matches: self.matches.perform_or(other.matches),
            opts: {
                self.opts |= other.opts;
                self.opts
            },
        }
    }

    fn not(self) -> Self {
        Self {
            matches: self.matches.not(),
            opts: match self.opts {
                TplOpts::In(xs) => TplOpts::NotIn(xs),
                TplOpts::NotIn(xs) => TplOpts::In(xs),
            },
        }
    }

    fn matches(&self) -> bool {
        self.matches.matches()
    }

    fn filtered(&self) -> bool {
        self.matches.filtered()
    }

    fn wrapper_filtered(&self) -> bool {
        self.matches.wrapper_filtered()
    }
}

impl From<Option<bool>> for TplMatches<'_> {
    fn from(value: Option<bool>) -> Self {
        Self {
            matches: value.into(),
            opts: TplOpts::NotIn(BTreeSet::new()),
        }
    }
}

/* RunState implementations. */

impl<W: Wrapper> RunState<Match, W> for QueryResultItems<W> {
    type Error = Infallible;

    fn add_item(&mut self, item_id: ItemId, item_ref: ItemRef<W>) -> Result<(), Infallible> {
        self.items.insert(item_id, item_ref);
        Ok(())
    }

    fn add_relation(
        &mut self,
        rel_id: RelationId,
        rel_ref: RelationRef<W>,
        invert: bool,
    ) -> Result<(), Infallible> {
        self.relations.insert(rel_id, (rel_ref, invert));
        Ok(())
    }
}

impl<'a, W: Wrapper> RunState<Matches<'a>, W> for BTreeMap<&'a TplVarId, TplOpts<'a>> {
    type Error = Infallible;

    fn add_match(&mut self, result: Matches<'a>) -> Result<(), Self::Error> {
        result.opts.into_iter().for_each(|(var, opts)| {
            if let Some(vals) = self.get_mut(var) {
                vals.add_template_matches(opts);
            }
        });
        Ok(())
    }
}

impl<'a, W: Wrapper> RunState<Matches<'a>, W>
    for (
        &mut Result<Limited<QueryResultItems<W>>, RunQueryError>,
        &mut BTreeMap<&'a TplVarId, TplOpts<'a>>,
    )
{
    type Error = Infallible;

    fn add_item(&mut self, item_id: ItemId, item_ref: ItemRef<W>) -> Result<(), Self::Error> {
        if let Ok(state) = self.0 {
            if let Err(e) = state.add_item(item_id, item_ref) {
                *self.0 = Err(e);
            }
        }
        Ok(())
    }

    fn add_relation(
        &mut self,
        rel_id: RelationId,
        rel_ref: RelationRef<W>,
        invert: bool,
    ) -> Result<(), Self::Error> {
        if let Ok(state) = self.0 {
            if let Err(e) = state.add_relation(rel_id, rel_ref, invert) {
                *self.0 = Err(e);
            }
        }
        Ok(())
    }

    fn add_match(&mut self, result: Matches<'a>) -> Result<(), Self::Error> {
        result.opts.into_iter().for_each(|(var, opts)| {
            if let Some(vals) = self.1.get_mut(var) {
                vals.add_template_matches(opts);
            }
        });
        Ok(())
    }
}

pub(crate) struct Limited<T> {
    limit: Option<usize>,
    state: T,
}

impl<T> Limited<T> {
    pub(crate) fn new(limit: Option<usize>, state: T) -> Self {
        Self { limit, state }
    }

    pub(crate) fn into_result<X, E>(self, res: Result<X, E>) -> Result<T, E> {
        res.map(|_| self.state)
    }

    pub(crate) fn into_state(self) -> T {
        self.state
    }
}

impl<T: CheckLimit> Limited<T> {
    fn check_limit(&self) -> Result<(), T::Error> {
        if let Some(limit) = self.limit {
            self.state.check_limit(limit)
        } else {
            Ok(())
        }
    }
}

pub(crate) trait CheckLimit {
    type Error;
    fn check_limit(&self, limit: usize) -> Result<(), Self::Error>;
}

impl<T: RunState<R, W, Error = Infallible> + CheckLimit, R: MatchResult, W: Wrapper> RunState<R, W>
    for Limited<T>
{
    type Error = <T as CheckLimit>::Error;

    fn add_item(&mut self, item_id: ItemId, item_ref: ItemRef<W>) -> Result<(), Self::Error> {
        self.state.add_item(item_id, item_ref).unwrap();
        self.check_limit()
    }

    fn add_relation(
        &mut self,
        rel_id: RelationId,
        rel_ref: RelationRef<W>,
        invert: bool,
    ) -> Result<(), Self::Error> {
        self.state.add_relation(rel_id, rel_ref, invert).unwrap();
        self.check_limit()
    }

    fn add_match(&mut self, result: R) -> Result<(), Self::Error> {
        self.state.add_match(result).unwrap();
        Ok(())
    }
}

/* Implementation for ElasticQueryResult. */

#[derive(Clone, Default, Debug)]
pub struct EsQueryResult<'a> {
    // None means all item types.
    pub items: Option<BTreeSet<&'a Absolute<ItemTypeId>>>,
    // None means all relation types.
    pub relations: Option<BTreeSet<&'a Absolute<RelationTypeId>>>,
}

impl<'a> EsQueryResult<'a> {
    pub(crate) fn empty() -> Self {
        Self {
            items: Some(BTreeSet::new()),
            relations: Some(BTreeSet::new()),
        }
    }

    pub fn into_filter(self) -> dbschema::Filter {
        dbschema::Filter::Any(Vec::from_iter([
            dbschema::Filter::at(
                dbschema::FilterPath::new().field("item_type").some(),
                match self.items {
                    Some(ts) => dbschema::Filter::In(ts.into_iter().map(|t| json!(t)).collect()),
                    None => dbschema::Filter::All(Vec::new()),
                },
            ),
            dbschema::Filter::at(
                dbschema::FilterPath::new().field("relation_type").some(),
                match self.relations {
                    Some(ts) => dbschema::Filter::In(ts.into_iter().map(|t| json!(t)).collect()),
                    None => dbschema::Filter::All(Vec::new()),
                },
            ),
        ]))
    }

    pub(crate) fn add_item_type(&mut self, item_type: &'a Absolute<ItemTypeId>) {
        if let Some(item_types) = self.items.as_mut() {
            item_types.insert(item_type);
        }
    }

    pub(crate) fn add_relation_type(&mut self, relation_type: &'a Absolute<RelationTypeId>) {
        if let Some(relation_types) = self.relations.as_mut() {
            relation_types.insert(relation_type);
        }
    }

    pub(crate) fn add_all_item_types(&mut self) {
        self.items = None;
    }

    pub(crate) fn add_all_relation_types(&mut self) {
        self.relations = None;
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use serde_json::json;

    use crate::{
        query::resolved::{Filters, TplVarValueRef},
        TplVarId,
    };

    use super::{Filter, Match, TplOpts};

    #[test]
    fn match_matching() {
        let namespace: TplVarId = serde_json::from_value(json!("namespace")).unwrap();
        let filters: Filters = serde_json::from_value(json!({
            "namespace": {"strings": ["default"]}
        }))
        .unwrap();

        let m: Match = filters.matches(
            &namespace,
            Some(TplVarValueRef::String(&String::from("default"))),
        );
        assert_eq!(m, Match::new(Some(true)).set_filtered(true));
    }

    #[test]
    fn match_non_matching() {
        let namespace: TplVarId = serde_json::from_value(json!("namespace")).unwrap();
        let filters: Filters = serde_json::from_value(json!({
            "namespace": {"strings": ["default"]}
        }))
        .unwrap();

        let m: Match = filters.matches(
            &namespace,
            Some(TplVarValueRef::String(&String::from("argocd"))),
        );
        assert_eq!(m, Match::new(Some(false)).set_filtered(true));
    }

    #[test]
    fn match_unfiltered() {
        let container: TplVarId = serde_json::from_value(json!("container")).unwrap();
        let filters: Filters = serde_json::from_value(json!({
            "namespace": {"strings": ["default"]}
        }))
        .unwrap();

        let m: Match = filters.matches(
            &container,
            Some(TplVarValueRef::String(&String::from("argocd"))),
        );
        assert_eq!(m, Match::new(None));
    }

    #[test]
    fn tplopts_union() {
        fn union<'a>(mut a: TplOpts<'a>, b: TplOpts<'a>) -> TplOpts<'a> {
            a |= b;
            a
        }

        let a = TplVarValueRef::String("a");
        let b = TplVarValueRef::String("b");
        let c = TplVarValueRef::String("c");

        assert_eq!(
            union(
                TplOpts::In(BTreeSet::from_iter([a])),
                TplOpts::In(BTreeSet::from_iter([b, c]))
            ),
            TplOpts::In(BTreeSet::from_iter([a, b, c]))
        );
        assert_eq!(
            union(
                TplOpts::In(BTreeSet::from_iter([a, b])),
                TplOpts::In(BTreeSet::from_iter([c]))
            ),
            TplOpts::In(BTreeSet::from_iter([a, b, c]))
        );
        assert_eq!(
            union(
                TplOpts::In(BTreeSet::from_iter([a, b])),
                TplOpts::In(BTreeSet::from_iter([b, c]))
            ),
            TplOpts::In(BTreeSet::from_iter([a, b, c]))
        );

        assert_eq!(
            union(
                TplOpts::In(BTreeSet::from_iter([a, b])),
                TplOpts::NotIn(BTreeSet::from_iter([b, c]))
            ),
            TplOpts::NotIn(BTreeSet::from_iter([c]))
        );
        assert_eq!(
            union(
                TplOpts::NotIn(BTreeSet::from_iter([b, c])),
                TplOpts::In(BTreeSet::from_iter([a, b]))
            ),
            TplOpts::NotIn(BTreeSet::from_iter([c]))
        );

        assert_eq!(
            union(
                TplOpts::NotIn(BTreeSet::from_iter([a, b])),
                TplOpts::NotIn(BTreeSet::from_iter([b, c]))
            ),
            TplOpts::NotIn(BTreeSet::from_iter([b]))
        );
    }

    #[test]
    fn tplopts_intersection() {
        fn intersection<'a>(mut a: TplOpts<'a>, b: TplOpts<'a>) -> TplOpts<'a> {
            a &= b;
            a
        }

        let a = TplVarValueRef::String("a");
        let b = TplVarValueRef::String("b");
        let c = TplVarValueRef::String("c");

        assert_eq!(
            intersection(
                TplOpts::In(BTreeSet::from_iter([a, b])),
                TplOpts::In(BTreeSet::from_iter([b, c]))
            ),
            TplOpts::In(BTreeSet::from_iter([b]))
        );
        assert_eq!(
            intersection(
                TplOpts::In(BTreeSet::from_iter([a, b])),
                TplOpts::In(BTreeSet::from_iter([c]))
            ),
            TplOpts::In(BTreeSet::from_iter([]))
        );
        assert_eq!(
            intersection(
                TplOpts::In(BTreeSet::from_iter([c])),
                TplOpts::In(BTreeSet::from_iter([a, b]))
            ),
            TplOpts::In(BTreeSet::from_iter([]))
        );

        assert_eq!(
            intersection(
                TplOpts::In(BTreeSet::from_iter([a, b, c])),
                TplOpts::NotIn(BTreeSet::from_iter([a, b]))
            ),
            TplOpts::In(BTreeSet::from_iter([c]))
        );
        assert_eq!(
            intersection(
                TplOpts::NotIn(BTreeSet::from_iter([a, b])),
                TplOpts::In(BTreeSet::from_iter([a, b, c]))
            ),
            TplOpts::In(BTreeSet::from_iter([c]))
        );

        assert_eq!(
            intersection(
                TplOpts::NotIn(BTreeSet::from_iter([a, b])),
                TplOpts::NotIn(BTreeSet::from_iter([b, c]))
            ),
            TplOpts::NotIn(BTreeSet::from_iter([a, b, c]))
        );
    }
}
