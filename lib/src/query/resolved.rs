/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    convert::Infallible,
    fmt::Display,
    ops::Not,
};

use ambassador::Delegate;
use graph::{BTreeGraph, Ref, RefBy, RefMap};
use serde::{Deserialize, Serialize};
use serde_json::json;
use wrapper::{Identity, Wrapped, Wrapper};

use crate::{
    delegatable::ambassador_impl_Iterator,
    error::{Error, Result},
    ids::{
        Absolute, ItemId, ItemTypeId, PackageId, PropertyId, RelationId, RelationTypeId, Relative,
    },
    items::{
        resolved::{
            Item, ItemRef, ItemRefMap, Items, ItemsRefMap, Relation, RelationRef, WrappedItem,
            WrappedRelation,
        },
        serial::PropertyValue,
    },
    types::resolved::{ItemType, RelationType, Types},
    QueryElemId, TplVarId,
};

use super::{
    filter::{
        CheckLimit, EsQueryResult, Filter, Limited, Match, MatchIter, MatchResult, Matches,
        RunState, TplFilter, TplMatches, TplOpts, WrapperFilter,
    },
    selector::Selector,
    serial::{self, ItemIdSelector, RelationIdSelector},
};

impl Display for TplVarDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TplVarDef::Strings => write!(f, "strings"),
            TplVarDef::ItemTypes => write!(f, "item_types"),
            TplVarDef::RelationTypes => write!(f, "relation_types"),
            TplVarDef::ItemIds => write!(f, "item_ids"),
            TplVarDef::RelationIds => write!(f, "relation_ids"),
        }
    }
}

pub struct Query {
    pub(crate) root: RefBy<QueryElemId, QueryElem>,
    pub(crate) elements: QueryElems,
    pub(crate) template: TemplateDef,
    /// Default filter values.
    pub(crate) filters: Filters,
}

pub type QueryElems = BTreeGraph<QueryElemId, QueryElem>;
pub type TemplateDef = BTreeGraph<TplVarId, TplVarDef>;
pub type Filters = BTreeMap<TplVarId, TplVarValues>;

pub struct QueryResult<W: Wrapper = Identity> {
    pub(crate) result: std::result::Result<QueryResultItems<W>, RunQueryError>,
    pub(crate) template: Filters,
}

pub struct QueryResultItems<W: Wrapper = Identity> {
    pub(crate) items: ItemRefMap<W>,
    pub(crate) relations: BTreeMap<RelationId, (RelationRef<W>, bool)>,
}

pub(crate) struct ItemArgs<'a, W: Wrapper> {
    item_id: &'a ItemId,
    item: &'a Item<W>,
}

impl<'a, W: Wrapper> ItemArgs<'a, W> {
    pub(crate) fn new(item_id: &'a ItemId, item: &'a Item<W>) -> Self {
        Self { item_id, item }
    }
}

pub(crate) struct RelArgs<'a, W: Wrapper> {
    relation_id: &'a RelationId,
    relation: &'a Relation<W>,
    endpoint: Endpoint,
}

impl<'a, W: Wrapper> RelArgs<'a, W> {
    pub(crate) fn new(
        relation_id: &'a RelationId,
        relation: &'a Relation<W>,
        endpoint: Endpoint,
    ) -> Self {
        Self {
            relation_id,
            relation,
            endpoint,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, thiserror::Error, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum RunQueryError {
    #[error("query result exceeds limit of {0} items & relations")]
    Limited(usize),
}

pub struct QueryElem {
    items: ItemSelector,
    follow: Vec<Follow>,
}

pub struct ItemSelector(Selector<MatchItem>);
pub struct RelationSelector(Selector<MatchRelation>);

pub struct MatchItem {
    item_id: Option<ItemIdSelector>,
    item_type: Option<ItemTypeSelector>,
    parent: Option<Box<ItemSelector>>,
    properties: Option<PropertySelector>,
    relations: Option<Box<RelationSelector>>,
}

pub struct MatchRelation {
    relation_id: Option<RelationIdSelector>,
    relation_type: Option<RelationTypeSelector>,
    endpoint: Option<Endpoint>,
    properties: Option<PropertySelector>,
    item: Option<Box<ItemSelector>>,
}

pub enum ItemTypeSelector {
    Is(RefBy<Absolute<ItemTypeId>, ItemType>),
    In(Vec<RefBy<Absolute<ItemTypeId>, ItemType>>),
    Template(TplVarId),
    // Not(Box<ItemTypeSelector>),
    // Any(Vec<ItemTypeSelector>),
    // All(Vec<ItemTypeSelector>),
}

pub enum RelationTypeSelector {
    Is(RefBy<Absolute<RelationTypeId>, RelationType>),
    In(Vec<RefBy<Absolute<RelationTypeId>, RelationType>>),
    Template(TplVarId),
    // Not(Box<RelationTypeSelector>),
    // Any(Vec<RelationTypeSelector>),
    // All(Vec<RelationTypeSelector>),
}

pub struct PropertySelector(pub(crate) BTreeMap<Absolute<PropertyId>, ValueSelector>);

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum Endpoint {
    Source,
    Target,
}

pub struct Follow {
    relation: RelationSelector,
    element: RefBy<QueryElemId, QueryElem>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum ValueSelector {
    String(StringSelector),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum StringSelector {
    Equals(String),
    In(BTreeSet<String>),
    Template(TplVarId),
    Not(Box<StringSelector>),
    All(Vec<StringSelector>),
    Any(Vec<StringSelector>),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
	// Why do we need to force this type to string?
	tsify(from_wasm_abi, into_wasm_abi, type = "string")
)]
#[serde(rename_all = "snake_case")]
pub enum TplVarDef {
    Strings,
    ItemTypes,
    RelationTypes,
    ItemIds,
    RelationIds,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum TplVarValues {
    Strings(TplVarStrings),
    ItemTypes(TplVarItemTypes),
    RelationTypes(TplVarRelationTypes),
    ItemIds(TplVarItemIds),
    RelationIds(TplVarRelationIds),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum TplVarValueRef<'a> {
    String(&'a str),
    ItemType(&'a Absolute<ItemTypeId>),
    RelationType(&'a Absolute<RelationTypeId>),
    ItemId(&'a ItemId),
    RelationId(&'a RelationId),
}

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TplVarStrings(pub BTreeSet<String>);

struct QueryContext<'a: 'b, 'b, W: Wrapper> {
    elements: &'a QueryElems,
    filtered: BTreeSet<&'a QueryElemId>,
    mctx: &'b MatchContext<'a, W>,
}

pub(crate) struct MatchContext<'a, W: Wrapper> {
    pub(crate) items: &'a Items<W>,
    pub(crate) types: &'a Types,
}

impl<'a, W: Wrapper> MatchContext<'a, W> {
    pub(crate) fn new(items: &'a Items<W>, types: &'a Types) -> Self {
        Self { items, types }
    }
}

impl Display for TplVarStrings {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        iter.next().into_iter().try_for_each(|v| write!(f, "{v}"))?;
        iter.try_for_each(|v| write!(f, ", {v}"))
    }
}

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TplVarItemTypes(pub(super) BTreeSet<Absolute<ItemTypeId>>);

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TplVarRelationTypes(pub(super) BTreeSet<Absolute<RelationTypeId>>);

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TplVarItemIds(pub(super) BTreeSet<ItemId>);

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TplVarRelationIds(pub(super) BTreeSet<RelationId>);

#[derive(Serialize, Deserialize, Default)]
pub struct ElasticQuery {
    pub item_types: Option<BTreeSet<Absolute<ItemTypeId>>>,
    pub relation_types: Option<BTreeSet<Absolute<RelationTypeId>>>,
    // properties: BTreeMap<Absolute<PropertyId>, BTreeSet<String>>,
}

impl TplVarValues {
    pub fn to_serial(&self) -> serde_json::Value {
        match self {
            TplVarValues::Strings(vs) => serde_json::to_value(vs).unwrap(),
            TplVarValues::ItemTypes(vs) => serde_json::to_value(vs).unwrap(),
            TplVarValues::RelationTypes(vs) => serde_json::to_value(vs).unwrap(),
            TplVarValues::ItemIds(vs) => serde_json::to_value(vs).unwrap(),
            TplVarValues::RelationIds(vs) => serde_json::to_value(vs).unwrap(),
        }
    }

    pub fn as_refs(&self) -> BTreeSet<TplVarValueRef> {
        match self {
            TplVarValues::Strings(vs) => vs.0.iter().map(|v| TplVarValueRef::String(v)).collect(),
            TplVarValues::ItemTypes(vs) => vs.0.iter().map(TplVarValueRef::ItemType).collect(),
            TplVarValues::RelationTypes(vs) => {
                vs.0.iter().map(TplVarValueRef::RelationType).collect()
            }
            TplVarValues::ItemIds(vs) => vs.0.iter().map(TplVarValueRef::ItemId).collect(),
            TplVarValues::RelationIds(vs) => vs.0.iter().map(TplVarValueRef::RelationId).collect(),
        }
    }

    pub fn strings(&self) -> Option<&BTreeSet<String>> {
        match self {
            TplVarValues::Strings(TplVarStrings(vs)) => Some(vs),
            _ => None,
        }
    }

    pub fn from_refs(typ: TplVarDef, values: &BTreeSet<TplVarValueRef>) -> Self {
        match typ {
            TplVarDef::Strings => Self::Strings(TplVarStrings(
                values
                    .iter()
                    .filter_map(|v| Some(v.get_string()?.to_string()))
                    .collect(),
            )),
            TplVarDef::ItemTypes => Self::ItemTypes(TplVarItemTypes(
                values
                    .iter()
                    .filter_map(|v| v.get_item_type().cloned())
                    .collect(),
            )),
            TplVarDef::RelationTypes => Self::RelationTypes(TplVarRelationTypes(
                values
                    .iter()
                    .filter_map(|v| v.get_relation_type().cloned())
                    .collect(),
            )),
            TplVarDef::ItemIds => Self::ItemIds(TplVarItemIds(
                values
                    .iter()
                    .filter_map(|v| v.get_item_id().cloned())
                    .collect(),
            )),
            TplVarDef::RelationIds => Self::RelationIds(TplVarRelationIds(
                values
                    .iter()
                    .filter_map(|v| v.get_relation_id().cloned())
                    .collect(),
            )),
        }
    }

    pub(crate) fn matches(&self, value: TplVarValueRef<'_>) -> bool {
        match (self, value) {
            (Self::Strings(vs), TplVarValueRef::String(v)) => vs.0.contains(v),
            (Self::ItemIds(vs), TplVarValueRef::ItemId(v)) => vs.0.contains(v),
            (Self::RelationIds(vs), TplVarValueRef::RelationId(v)) => vs.0.contains(v),
            (Self::ItemTypes(vs), TplVarValueRef::ItemType(v)) => vs.0.contains(v),
            (Self::RelationTypes(vs), TplVarValueRef::RelationType(v)) => vs.0.contains(v),
            _ => false,
        }
    }

    // pub(crate) fn extend_from_refs<'a, T: IntoIterator<Item = TplVarValueRef<'a>>>(
    //     &mut self,
    //     values: T,
    // ) {
    //     match self {
    //         Self::Strings(vs) => values
    //             .into_iter()
    //             .filter_map(|v| match v {
    //                 TplVarValueRef::String(s) => Some(s),
    //                 _ => None,
    //             })
    //             .for_each(|s| {
    // 				/* Note: micro-benchmark showed test-then-clone+insert to be faster than clone+insert. */
    // 				if !vs.0.contains(s) {
    //                     vs.0.insert(s.to_string());
    //                 }
    //             }),
    //         Self::ItemTypes(vs) => values
    //             .into_iter()
    //             .filter_map(|v| match v {
    //                 TplVarValueRef::ItemType(s) => Some(s),
    //                 _ => None,
    //             })
    //             .for_each(|s| {
    // 				/* Note: micro-benchmark showed test-then-clone+insert to be faster than clone+insert. */
    // 				if !vs.0.contains(s) {
    //                     vs.0.insert(s.clone());
    //                 }
    //             }),
    //         Self::RelationTypes(vs) => values
    //             .into_iter()
    //             .filter_map(|v| match v {
    //                 TplVarValueRef::RelationType(s) => Some(s),
    //                 _ => None,
    //             })
    //             .for_each(|s| {
    // 				/* Note: micro-benchmark showed test-then-clone+insert to be faster than clone+insert. */
    // 				if !vs.0.contains(s) {
    //                     vs.0.insert(s.clone());
    //                 }
    //             }),
    //         Self::ItemIds(vs) => values
    //             .into_iter()
    //             .filter_map(|v| match v {
    //                 TplVarValueRef::ItemId(s) => Some(s),
    //                 _ => None,
    //             })
    //             .for_each(|s| {
    // 				/* Note: micro-benchmark showed test-then-clone+insert to be faster than clone+insert. */
    // 				if !vs.0.contains(s) {
    //                     vs.0.insert(s.clone());
    //                 }
    //             }),
    //         Self::RelationIds(vs) => values
    //             .into_iter()
    //             .filter_map(|v| match v {
    //                 TplVarValueRef::RelationId(s) => Some(s),
    //                 _ => None,
    //             })
    //             .for_each(|s| {
    // 				/* Note: micro-benchmark showed test-then-clone+insert to be faster than clone+insert. */
    // 				if !vs.0.contains(s) {
    //                     vs.0.insert(s.clone());
    //                 }
    //             }),
    //     }
    // }
}

type Seen<'a> = SeenMap<(&'a QueryElemId, &'a ItemId), bool>;

pub(crate) struct RunQueryResult<'a, W: Wrapper> {
    pub(super) items: BTreeMap<&'a ItemId, &'a ItemRef<W>>,
    pub(super) relations: BTreeMap<&'a RelationId, (&'a RelationRef<W>, bool)>,
}

impl<W: Wrapper> RunQueryResult<'_, W> {
    fn new() -> Self {
        Self::default()
    }
}

impl<W: Wrapper> Default for RunQueryResult<'_, W> {
    fn default() -> Self {
        Self {
            items: Default::default(),
            relations: Default::default(),
        }
    }
}

impl<W: Wrapper> std::fmt::Debug for RunQueryResult<'_, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RunQueryResult")
            .field("items", &self.items.len())
            .field("relations", &self.relations.len())
            .finish()
    }
}

impl Query {
    pub(super) fn from_serial(
        serial: serial::Query,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        let elements = serial
            .elements
            .into_iter()
            .map(|(id, elem)| {
                Ok((
                    id,
                    QueryElem::from_serial(elem, types, &serial.template, pkg)?,
                ))
            })
            .collect::<Result<_>>()?;
        let filters = serial
            .filters
            .into_iter()
            .map(|(id, flt)| {
                let typ = serial
                    .template
                    .get(&id)
                    .ok_or_else(|| Error::MissingTplVarDef(id.clone()))?;
                Ok((id, TplVarValues::from_value(typ, flt, pkg)?))
            })
            .collect::<Result<_>>()?;

        Ok(Self {
            root: RefBy::dangling(serial.root),
            template: serial.template.into_iter().collect(),
            elements,
            filters,
        })
    }

    pub(super) fn resolve(&mut self, types: &Types) -> Result<()> {
        self.root
            .resolve(&self.elements)
            .map_err(Error::MissingQueryElem)?;
        let index = self.elements.index().clone();
        self.elements
            .values_mut()
            .try_for_each(|elem| elem.resolve(types, &index))
    }

    pub fn filters_from_serial(
        &self,
        filters: BTreeMap<TplVarId, serde_json::Value>,
        pkg: Option<&PackageId>,
    ) -> Result<Filters> {
        filters
            .into_iter()
            .map(|(id, value)| {
                let typ = self
                    .template
                    .get(&id)
                    .ok_or_else(|| Error::MissingTplVarDef(id.clone()))?;
                let value = TplVarValues::from_value(typ, value, pkg)?;
                Ok((id, value))
            })
            .collect()
    }

    pub fn template(&self) -> &TemplateDef {
        &self.template
    }

    pub fn default_filters(&self) -> &Filters {
        &self.filters
    }

    // Currently only selects on item and relation types.
    pub fn elastic_query<'a>(&'a self, types: &'a Types) -> EsQueryResult<'a> {
        let mut result = EsQueryResult::empty();
        self.elements
            .values()
            .for_each(|elem| elem.elastic_query(types, &mut result));
        result
    }

    pub fn search_domain<W: Wrapper>(&self, items: &Items<W>, types: &Types) -> ItemsRefMap<W> {
        self.search_domain_with_filters(items, types, &Filters::new())
    }

    pub fn search_domain_with_filters<W: Wrapper>(
        &self,
        items: &Items<W>,
        types: &Types,
        filters: &Filters,
    ) -> ItemsRefMap<W> {
        let mut filtered = BTreeMap::new();
        let filtered = self
            .elements
            .iter()
            .filter(|(elem_id, elem)| {
                let r = elem.is_filtered_on(filters, &self.elements, &mut filtered);
                filtered.insert(elem_id, r);
                r
            })
            .map(|(elem_id, _)| elem_id)
            .collect();

        let match_ctx = MatchContext::new(items, types);
        let ctx = QueryContext {
            elements: &self.elements,
            mctx: &match_ctx,
            filtered,
        };

        let mut domain = RunQueryResult::new();

        self.elements.borrow(&self.root).get_search_domain(
            &mut domain,
            self.root.key(),
            filters,
            &mut SeenMap::new(),
            &ctx,
        );

        // let parents = domain
        //     .items
        //     .values()
        //     .flat_map(|item| items.borrow_item(item).parent_refs(items))
        //     .collect::<BTreeMap<_, _>>();

        ItemsRefMap {
            items: domain
                .items
                .into_iter()
                // .chain(parents)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
            relations: domain
                .relations
                .into_iter()
                .map(|(k, (v, _))| (k.clone(), v.clone()))
                .collect(),
        }
    }

    pub fn run<'a, W: Wrapper>(&'a self, items: &'a Items<W>, types: &'a Types) -> QueryResult<W> {
        self.run_with_filters(items, types, &self.filters, &(), Some(100))
    }

    pub fn run_with_filters<'a, W, Q>(
        &'a self,
        items: &'a Items<W>,
        types: &'a Types,
        filters: &'a Filters,
        wrapper_filter: &'a Q,
        limit: Option<usize>,
    ) -> QueryResult<W>
    where
        W: Wrapper,
        Q: WrapperFilter<W, Match>
            + WrapperFilter<W, Matches<'a>>
            + WrapperFilter<W, TplMatches<'a>>,
    {
        let mut filtered = BTreeMap::new();
        let filtered = self
            .elements
            .iter()
            .filter(|(elem_id, elem)| {
                let r = elem.is_filtered_on(filters, &self.elements, &mut filtered);
                filtered.insert(elem_id, r);
                r
            })
            .map(|(elem_id, _)| elem_id)
            .collect();

        let match_ctx = MatchContext::new(items, types);
        let ctx = QueryContext {
            elements: &self.elements,
            mctx: &match_ctx,
            filtered,
        };
        let root = self.elements.borrow(&self.root);

        /* Find matching items and relations. */
        let mut items = Limited::new(limit, QueryResultItems::new());

        /* Find disabled filter matches. Since filter options are
         * found by collecting matches with the filter in question
         * disabled, options for all disabled filters can be collected
         * in one pass (since disabling them has no effect on the
         * result). */
        let mut matches = self
            .template
            .keys()
            .filter(|var| !filters.contains_key(var))
            .map(|var| (var, TplOpts::NotIn(BTreeSet::new())))
            .collect::<BTreeMap<_, _>>();

        let result = if matches.is_empty() {
            /* If no matches need to be collected, we can use an
             * optimized, short-circuiting match implementation. */

            let res = root.run::<_, Q, Match, _, _>(
                self.root.key(),
                filters,
                wrapper_filter,
                &mut Seen::new(),
                &ctx,
                &mut items,
            );
            items.into_result(res)
        } else {
            /* If matches need to be collected (at least one filter is
             * disabled), it can be done more efficiently by
             * collecting both the items & relations and the matches
             * in a single pass. In this case, the match is not
             * short-circuiting. */

            let mut result = Ok(items);
            let _shown = root
                .run::<_, Q, Matches<'a>, _, _>(
                    self.root.key(),
                    filters,
                    wrapper_filter,
                    &mut Seen::new(),
                    &ctx,
                    &mut (&mut result, &mut matches),
                )
                // Unwrap Result<_, Infallible>.
                .unwrap();
            result.map(|r| r.into_state())
        };

        /* Find template options. */
        let template = self
            .template
            .iter()
            .map(|(var_id, var_def)| {
                if let Some(opts) = matches.remove(var_id) {
                    (var_id.clone(), opts.get_values(*var_def))
                } else {
                    let filters = TplFilter::new(filters, var_id);
                    let mut result = TplOpts::In(BTreeSet::new());
                    let _shown = root
                        .run(
                            self.root.key(),
                            &filters,
                            wrapper_filter,
                            &mut Seen::new(),
                            &ctx,
                            &mut result,
                        )
                        // Unwrap Result<_, Infallible>.
                        .unwrap();
                    (var_id.clone(), result.get_values(*var_def))
                }
            })
            .collect::<BTreeMap<_, _>>();

        QueryResult { result, template }
    }
}

impl<W: Wrapper> QueryResult<W> {
    pub fn get_result(&self) -> std::result::Result<&QueryResultItems<W>, RunQueryError> {
        match &self.result {
            Ok(r) => Ok(r),
            Err(e) => Err(e.clone()),
        }
    }

    pub fn filter_opts(&self) -> &Filters {
        &self.template
    }
}

impl<W: Wrapper> QueryResultItems<W> {
    fn new() -> Self {
        Self {
            items: RefMap::new(),
            relations: BTreeMap::new(),
        }
    }

    pub fn item_keys(&self) -> impl Iterator<Item = &ItemId> {
        self.items.keys()
    }

    pub fn relation_keys(&self) -> impl Iterator<Item = &RelationId> {
        self.relations.keys()
    }

    pub fn iter_items<'b>(
        &'b self,
        items: &'b Items<W>,
    ) -> impl Iterator<Item = (&'b ItemId, &'b WrappedItem<W>)> {
        self.items.iter(&items.items)
    }

    pub fn iter_relations<'b>(
        &'b self,
        items: &'b Items<W>,
    ) -> impl Iterator<Item = (&'b RelationId, &'b WrappedRelation<W>, bool)> {
        self.relations
            .iter()
            .map(|(rel_id, (rel_ref, invert))| (rel_id, items.relations.borrow(rel_ref), *invert))
    }

    pub fn contains_item(&self, id: &ItemId) -> bool {
        self.items.contains_key(id)
    }

    pub fn contains_relation(&self, id: &RelationId) -> bool {
        self.relations.contains_key(id)
    }

    pub fn is_relation_reversed(&self, id: &RelationId) -> Option<bool> {
        self.relations.get(id).map(|(_, reversed)| *reversed)
    }
}

impl<W: Wrapper> CheckLimit for QueryResultItems<W> {
    type Error = RunQueryError;
    fn check_limit(&self, limit: usize) -> std::result::Result<(), Self::Error> {
        (self.items.len() + self.relations.len() <= limit)
            .then_some(())
            .ok_or(RunQueryError::Limited(limit))
    }
}

impl QueryElem {
    fn from_serial(
        serial: serial::QueryElem,
        types: &Types,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self {
            items: ItemSelector::from_serial(serial.items, types, template, pkg)?,
            follow: serial
                .follow
                .into_iter()
                .map(|f| Follow::from_serial(f, types, template, pkg))
                .collect::<Result<_>>()?,
        })
    }

    fn resolve(
        &mut self,
        types: &Types,
        query: &BTreeMap<QueryElemId, Ref<QueryElem>>,
    ) -> Result<()> {
        self.items.resolve(types)?;
        self.follow
            .iter_mut()
            .try_for_each(|follow| follow.resolve(types, query))
    }

    fn elastic_query<'a>(&'a self, types: &'a Types, result: &mut EsQueryResult<'a>) {
        self.items.elastic_query(types, result);
        self.follow
            .iter()
            .for_each(|follow| follow.relation.elastic_query(types, result));
    }

    fn get_search_domain<'a, W: Wrapper>(
        &'a self,
        domain: &mut RunQueryResult<'a, W>,
        elem_id: &'a QueryElemId,
        filters: &'a Filters,
        seen: &mut SeenMap<(&'a QueryElemId, &'a ItemId), ()>,
        ctx: &QueryContext<'a, '_, W>,
    ) {
        self.items
            .prefilter(ctx.mctx, Some(filters))
            .for_each(|(item_id, item_ref)| {
                if let Some(item) = ctx.mctx.items.borrow_item(item_ref).try_get_wrapped() {
                    let m: Match = self.items.matches::<_, Match, _>(
                        &ItemArgs { item_id, item },
                        ctx.mctx,
                        filters,
                    );
                    if m.matches() {
                        domain.items.insert(item_id, item_ref);
                        self.items
                            .get_search_domain(domain, item, ctx.mctx, filters);
                        seen.run((elem_id, item_id), |seen| {
                            self.get_search_domain_follows(domain, item, filters, seen, ctx);
                        });
                    }
                }
            });
    }

    fn get_search_domain_follows<'a, W: Wrapper>(
        &'a self,
        domain: &mut RunQueryResult<'a, W>,
        item: &'a Item<W>,
        filters: &'a Filters,
        seen: &mut SeenMap<(&'a QueryElemId, &'a ItemId), ()>,
        ctx: &QueryContext<'a, '_, W>,
    ) {
        self.follow
            .iter()
            .for_each(|follow| follow.get_search_domain(domain, item, filters, seen, ctx));
    }

    fn run<'a: 'b, 'b, F, Q, R, S, W>(
        &'a self,
        elem_id: &'a QueryElemId,
        filters: &'b F,
        wrapper_filter: &'b Q,
        seen: &mut Seen<'a>,
        ctx: &QueryContext<'a, '_, W>,
        state: &mut S,
    ) -> std::result::Result<bool, S::Error>
    where
        F: Filter<'a, R>,
        Q: WrapperFilter<W, R>,
        R: MatchResult,
        S: RunState<R, W>,
        W: Wrapper,
    {
        //eprintln!("run({elem_id})");
        #[allow(clippy::unnecessary_fold)]
        let show = self
            .items
            // No prefilter on filters because we need those items to
            // find all template options!
            .prefilter(ctx.mctx, None)
            .filter_map(|(item_id, item_ref)| {
                let wrapped_item = ctx.mctx.items.borrow_item(item_ref);
                let item = wrapped_item.try_get_wrapped()?;
                let m = wrapper_filter.matches(wrapped_item).and(|| {
                    self.items
                        .matches(&ItemArgs { item_id, item }, ctx.mctx, filters)
                });
                let shown = if m.matches() {
                    let follow_shown = match seen.try_run((elem_id, item_id), |seen| {
                        self.run_follows(item, filters, wrapper_filter, seen, ctx, state)
                    }) {
                        Ok(shown) => shown,
                        Err(e) => return Some(Err(e)),
                    };

                    if follow_shown
                        || ((m.filtered() || !ctx.filtered.contains(elem_id))
                            && (!wrapper_filter.is_filtered() || m.wrapper_filtered()))
                    {
                        if let Err(e) = state.add_match(m) {
                            return Some(Err(e));
                        }
                        if let Err(e) = state.add_item(item_id.clone(), item_ref.clone()) {
                            return Some(Err(e));
                        }
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

                Some(Ok(shown))
            })
            .try_fold(false, |a, br| {
                let b = br?;
                Ok(a || b)
            })?;
        //eprintln!("...run({elem_id}) -> {r:?}");

        Ok(show)
    }

    fn run_follows<'a: 'b, 'b, F, Q, R, S, W>(
        &'a self,
        item: &'a Item<W>,
        filters: &'b F,
        wrapper_filter: &'b Q,
        seen: &mut Seen<'a>,
        ctx: &QueryContext<'a, '_, W>,
        state: &mut S,
    ) -> std::result::Result<bool, S::Error>
    where
        F: Filter<'a, R>,
        Q: WrapperFilter<W, R>,
        R: MatchResult,
        S: RunState<R, W>,
        W: Wrapper,
    {
        #[allow(clippy::unnecessary_fold)]
        self.follow
            .iter()
            .map(|follow| follow.run(item, filters, wrapper_filter, seen, ctx, state))
            /* Note: do not use `any`, because it short-circuits! */
            .try_fold(false, |a, br| {
                let b = br?;
                Ok(a || b)
            })
    }

    fn is_filtered_on<'a>(
        &'a self,
        filters: &Filters,
        elems: &'a QueryElems,
        memo: &mut BTreeMap<&'a QueryElemId, bool>,
    ) -> bool {
        self.items.is_filtered_on(filters)
            || self
                .follow
                .iter()
                .any(|follow| follow.is_filtered_on(filters, elems, memo))
    }
}

impl ItemSelector {
    pub(crate) fn to_serial(&self, pkg: Option<&PackageId>) -> serial::ItemSelector {
        serial::ItemSelector(self.0.map_ref(|i| i.to_serial(pkg)))
    }

    pub(crate) fn from_serial(
        serial: serial::ItemSelector,
        types: &Types,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self(serial.0.try_map(&mut |m| {
            MatchItem::from_serial(m, types, template, pkg)
        })?))
    }

    pub(crate) fn resolve(&mut self, types: &Types) -> Result<()> {
        self.0.try_modify(|m| m.resolve(types))
    }

    fn is_filtered_on(&self, filters: &Filters) -> bool {
        self.0.is_filtered_on(filters, &MatchItem::is_filtered_on)
    }

    // Note: in transactional context (W = Txs), the returned
    // Ref<Tx<Item<Txs>> may be empty because it can originate from
    // the by_type index, which is shared between the committed
    // objects and any running transactions. Therefore, the item
    // should be retrieved using try_get_wrapped instead of
    // get_wrapped.
    fn prefilter<'a: 'b, 'b, W: Wrapper>(
        &'b self,
        ctx: &MatchContext<'a, W>,
        filters: Option<&'a Filters>,
    ) -> impl Iterator<Item = (&'a ItemId, &'a ItemRef<W>)> + 'b {
        if let Some(item_id) = self.0.find_required(|m| match m {
            MatchItem {
                item_id: Some(ItemIdSelector::Is(item_id)),
                ..
            } => Some(item_id),
            _ => None,
        }) {
            PrefilteredItemSelector::Item(ctx.items.items.get_entry(item_id).into_iter())
        } else if let Some(item_ids) = self.0.find_required(|m| match m {
            MatchItem {
                item_id: Some(ItemIdSelector::In(item_ids)),
                ..
            } => Some(item_ids),
            _ => None,
        }) {
            PrefilteredItemSelector::Items(
                item_ids
                    .iter()
                    .filter_map(|item_id| ctx.items.items.get_entry(item_id)),
            )
        } else if let Some(vals) = self.0.find_required(|m| match m {
            MatchItem {
                item_id: Some(ItemIdSelector::Template(var)),
                ..
            } => filters?.get(var),
            _ => None,
        }) {
            let TplVarValues::ItemIds(TplVarItemIds(item_ids)) = vals else {
                panic!("invalid template variable type")
            };
            PrefilteredItemSelector::ItemsTpl(
                item_ids
                    .iter()
                    .filter_map(|item_id| ctx.items.items.get_entry(item_id)),
            )
        } else if let Some(item_type) = self.0.find_required(|m| match m {
            MatchItem {
                item_type: Some(ItemTypeSelector::Is(typ)),
                ..
            } => Some(typ),
            _ => None,
        }) {
            PrefilteredItemSelector::Type(
                ctx.types
                    .iter_type_and_subtypes(item_type)
                    .flat_map(|(id, _)| ctx.items.by_type.get(id))
                    .flat_map(|m| m.iter_ref()),
            )
        } else if let Some(item_types) = self.0.find_required(|m| match m {
            MatchItem {
                item_type: Some(ItemTypeSelector::In(typs)),
                ..
            } => Some(typs),
            _ => None,
        }) {
            PrefilteredItemSelector::Types(
                item_types
                    .iter()
                    .flat_map(|typ| ctx.types.iter_type_and_subtypes(typ))
                    .flat_map(|(id, _)| ctx.items.by_type.get(id))
                    .flat_map(|m| m.iter_ref()),
            )
        } else if let Some(vals) = self.0.find_required(|m| match m {
            MatchItem {
                item_type: Some(ItemTypeSelector::Template(var)),
                ..
            } => filters?.get(var),
            _ => None,
        }) {
            let TplVarValues::ItemTypes(TplVarItemTypes(typs)) = vals else {
                panic!("invalid template variable type")
            };
            PrefilteredItemSelector::TypesTpl(
                typs.iter()
                    .filter_map(|typ| ctx.types.items.get_entry(typ))
                    .flat_map(|(key, typ)| ctx.types.iter_type_and_subtypes_by_entry(key, typ))
                    .flat_map(|(id, _)| ctx.items.by_type.get(id))
                    .flat_map(|m| m.iter_ref()),
            )
        } else {
            PrefilteredItemSelector::All(ctx.items.items.iter_ref())
        }
    }

    fn elastic_query<'a>(&'a self, types: &'a Types, result: &mut EsQueryResult<'a>) {
        self.0.for_each(|m| m.elastic_query(types, result));
    }

    pub(crate) fn matches<'a, F: Filter<'a, R>, R: MatchResult, W: Wrapper>(
        &'a self,
        item: &ItemArgs<'a, W>,
        ctx: &MatchContext<'a, W>,
        filter: &F,
    ) -> R {
        self.0.matches(|m| m.matches(item, ctx, filter))
    }

    pub(crate) fn get_search_domain<'a, W: Wrapper>(
        &self,
        domain: &mut RunQueryResult<'a, W>,
        item: &'a Item<W>,
        ctx: &MatchContext<'a, W>,
        filter: &Filters,
    ) {
        self.0
            .for_each(|m| m.get_search_domain(domain, item, ctx, filter))
    }
}

impl RelationSelector {
    pub(crate) fn to_serial(&self, pkg: Option<&PackageId>) -> serial::RelationSelector {
        serial::RelationSelector(self.0.map_ref(|s| s.to_serial(pkg)))
    }

    pub(crate) fn from_serial(
        serial: serial::RelationSelector,
        types: &Types,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self(serial.0.try_map(&mut |m| {
            MatchRelation::from_serial(m, types, template, pkg)
        })?))
    }

    pub(super) fn resolve(&mut self, types: &Types) -> Result<()> {
        self.0.try_modify(|m| m.resolve(types))
    }

    fn is_filtered_on(&self, filters: &Filters) -> bool {
        self.0
            .is_filtered_on(filters, &MatchRelation::is_filtered_on)
    }

    pub(crate) fn prefilter_relation_type(&self) -> Option<&Absolute<RelationTypeId>> {
        self.0.find_required(|m| match m {
            MatchRelation {
                relation_type: Some(RelationTypeSelector::Is(t)),
                ..
            } => Some(t.key()),
            _ => None,
        })
    }

    pub(crate) fn prefilter_endpoint(&self) -> Option<Endpoint> {
        self.0.find_required(|m| match m {
            MatchRelation {
                endpoint: Some(ep), ..
            } => Some(*ep),
            _ => None,
        })
    }

    fn elastic_query<'a>(&'a self, types: &'a Types, result: &mut EsQueryResult<'a>) {
        self.0.for_each(|m| m.elastic_query(types, result))
    }

    pub(crate) fn matches<'a, F: Filter<'a, R>, R: MatchResult, W: Wrapper>(
        &'a self,
        rel: &RelArgs<'a, W>,
        ctx: &MatchContext<'a, W>,
        filter: &F,
    ) -> R {
        self.0.matches(|m| m.matches(rel, ctx, filter))
    }

    pub(crate) fn get_search_domain<'a, W: Wrapper>(
        &self,
        domain: &mut RunQueryResult<'a, W>,
        relation: &'a Relation<W>,
        endpoint: Endpoint,
        ctx: &MatchContext<'a, W>,
        filter: &Filters,
    ) {
        self.0
            .for_each(|m| m.get_search_domain(domain, relation, endpoint, ctx, filter))
    }
}

/* Trick to avoid dynamic dispatch (can be >10x speed improvement
 * depending on the query). */

#[derive(Delegate)]
#[delegate(Iterator)]
enum PrefilteredItemSelector<A, B, C, D, E, F, G> {
    Item(A),
    Items(B),
    ItemsTpl(C),
    Type(D),
    Types(E),
    TypesTpl(F),
    All(G),
}

impl MatchItem {
    fn to_serial(&self, pkg: Option<&PackageId>) -> serial::MatchItem {
        serial::MatchItem {
            item_id: self.item_id.clone(),
            item_type: self.item_type.as_ref().map(|s| s.to_serial(pkg)),
            parent: self.parent.as_ref().map(|s| Box::new(s.to_serial(pkg))),
            properties: self.properties.as_ref().map(|s| s.to_serial(pkg)),
            relations: self.relations.as_ref().map(|s| Box::new(s.to_serial(pkg))),
        }
    }

    fn from_serial(
        serial: serial::MatchItem,
        types: &Types,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self {
            item_id: serial.item_id,
            item_type: serial
                .item_type
                .map(|m| ItemTypeSelector::from_serial(m, template, pkg))
                .transpose()?,
            parent: serial
                .parent
                .map(|m| {
                    Ok(Box::new(ItemSelector::from_serial(
                        *m, types, template, pkg,
                    )?))
                })
                .transpose()?,
            properties: serial
                .properties
                .map(|m| PropertySelector::from_serial(m, types, template, pkg))
                .transpose()?,
            relations: serial
                .relations
                .map(|m| {
                    Ok(Box::new(RelationSelector::from_serial(
                        *m, types, template, pkg,
                    )?))
                })
                .transpose()?,
        })
    }

    fn resolve(&mut self, types: &Types) -> Result<()> {
        if let Some(m) = &mut self.item_type {
            m.resolve(types)?;
        }
        if let Some(m) = &mut self.parent {
            m.resolve(types)?;
        }
        if let Some(m) = &mut self.relations {
            m.resolve(types)?;
        }
        Ok(())
    }

    fn is_filtered_on(&self, filters: &Filters) -> bool {
        self.item_type
            .as_ref()
            .is_some_and(|m| m.is_filtered_on(filters))
            || self
                .parent
                .as_ref()
                .is_some_and(|m| m.is_filtered_on(filters))
            || self
                .properties
                .as_ref()
                .is_some_and(|m| m.is_filtered_on(filters))
            || self
                .relations
                .as_ref()
                .is_some_and(|m| m.is_filtered_on(filters))
    }

    fn elastic_query<'a>(&'a self, types: &'a Types, result: &mut EsQueryResult<'a>) {
        if let Some(m) = self.item_type.as_ref() {
            m.elastic_query(types, result);
        } else {
            result.add_all_item_types();
        }
        self.parent
            .iter()
            .for_each(|m| m.elastic_query(types, result));
        self.relations
            .iter()
            .for_each(|m| m.elastic_query(types, result));
    }

    fn matches<'a, F: Filter<'a, R>, R: MatchResult, W: Wrapper>(
        &'a self,
        item: &ItemArgs<'a, W>,
        ctx: &MatchContext<'a, W>,
        filter: &F,
    ) -> R {
        [
            self.item_id
                .as_ref()
                .map_or(None.into(), |m| m.matches(item.item_id, filter)),
            self.item_type
                .as_ref()
                .map_or(None.into(), |m| m.matches(item.item, ctx.types, filter)),
            self.parent.as_ref().map_or(None.into(), |m| {
                item.item
                    .parent
                    .as_ref()
                    .map_or(Some(false).into(), |parent_ref| {
                        m.matches(
                            &ItemArgs {
                                item_id: parent_ref.key(),
                                item: ctx.items.items.borrow(parent_ref).get_wrapped(),
                            },
                            ctx,
                            filter,
                        )
                    })
            }),
            self.properties
                .iter()
                .map(|m| m.matches(&item.item.properties, filter))
                .match_all(),
            self.relations
                .iter()
                .map(|m| {
                    item.item
                        .relations(m.prefilter_relation_type(), m.prefilter_endpoint())
                        .map(|((relation_id, relation), endpoint)| {
                            m.matches(
                                &RelArgs::new(
                                    relation_id,
                                    ctx.items.borrow_relation(relation).get_wrapped(),
                                    endpoint,
                                ),
                                ctx,
                                filter,
                            )
                        })
                        .match_any()
                })
                .match_all(),
        ]
        .into_iter()
        .match_all()
    }

    pub(crate) fn get_search_domain<'a, W: Wrapper>(
        &self,
        domain: &mut RunQueryResult<'a, W>,
        item: &'a Item<W>,
        ctx: &MatchContext<'a, W>,
        filter: &Filters,
    ) {
        if let Some(m) = &self.parent {
            if let Some(parent) = &item.parent {
                domain.items.insert(parent.key(), parent.value_ref());
                m.get_search_domain(
                    domain,
                    ctx.items.borrow_item(parent).get_wrapped(),
                    ctx,
                    filter,
                );
            }
        }
        if let Some(m) = &self.relations {
            item.relations(m.prefilter_relation_type(), m.prefilter_endpoint())
                .for_each(|((rel_id, rel_ref), ep)| {
                    let rel = ctx.items.borrow_relation(rel_ref).get_wrapped();
                    //if m.matches(rel, ep, ctx, filter).matches {
                    domain
                        .relations
                        .insert(rel_id, (rel_ref, matches!(ep, Endpoint::Target)));
                    m.get_search_domain(domain, rel, ep, ctx, filter);
                    //}
                });
        }
    }
}

impl MatchRelation {
    fn to_serial(&self, pkg: Option<&PackageId>) -> serial::MatchRelation {
        serial::MatchRelation {
            relation_id: self.relation_id.clone(),
            relation_type: self.relation_type.as_ref().map(|s| s.to_serial(pkg)),
            endpoint: self.endpoint,
            properties: self.properties.as_ref().map(|s| s.to_serial(pkg)),
            item: self.item.as_ref().map(|s| Box::new(s.to_serial(pkg))),
        }
    }

    fn from_serial(
        serial: serial::MatchRelation,
        types: &Types,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self {
            relation_id: serial.relation_id,
            relation_type: serial
                .relation_type
                .map(|m| RelationTypeSelector::from_serial(m, template, pkg))
                .transpose()?,
            endpoint: serial.endpoint,
            properties: serial
                .properties
                .map(|m| PropertySelector::from_serial(m, types, template, pkg))
                .transpose()?,
            item: serial
                .item
                .map(|m| {
                    Ok(Box::new(ItemSelector::from_serial(
                        *m, types, template, pkg,
                    )?))
                })
                .transpose()?,
        })
    }

    fn resolve(&mut self, types: &Types) -> Result<()> {
        if let Some(m) = &mut self.relation_type {
            m.resolve(types)?;
        }
        if let Some(m) = &mut self.item {
            m.resolve(types)?;
        }
        Ok(())
    }

    fn is_filtered_on(&self, filters: &Filters) -> bool {
        self.relation_type
            .as_ref()
            .is_some_and(|m| m.is_filtered_on(filters))
            || self
                .properties
                .as_ref()
                .is_some_and(|m| m.is_filtered_on(filters))
            || self
                .item
                .as_ref()
                .is_some_and(|m| m.is_filtered_on(filters))
    }

    fn elastic_query<'a>(&'a self, types: &'a Types, result: &mut EsQueryResult<'a>) {
        if let Some(m) = self.relation_type.as_ref() {
            m.elastic_query(result);
        } else {
            result.add_all_relation_types();
        }
        self.item
            .iter()
            .for_each(|m| m.elastic_query(types, result));
    }

    fn matches<'a, F: Filter<'a, R>, R: MatchResult, W: Wrapper>(
        &'a self,
        rel: &RelArgs<'a, W>,
        ctx: &MatchContext<'a, W>,
        filter: &F,
    ) -> R {
        [
            self.relation_id
                .as_ref()
                .map_or(None.into(), |m| m.matches(rel.relation_id, filter)),
            self.relation_type
                .as_ref()
                .map_or(None.into(), |m| m.matches(rel.relation, filter)),
            self.endpoint.as_ref().map(|m| m == &rel.endpoint).into(),
            self.properties
                .iter()
                .map(|m| m.matches(&rel.relation.properties, filter))
                .match_all(),
            self.item.as_ref().map_or(None.into(), |m| {
                let item_ref = rel.relation.endpoint(!rel.endpoint);
                m.matches(
                    &ItemArgs {
                        item_id: item_ref.key(),
                        item: ctx.items.borrow_item(item_ref).get_wrapped(),
                    },
                    ctx,
                    filter,
                )
            }),
        ]
        .into_iter()
        .match_all()
    }

    pub(crate) fn get_search_domain<'a, W: Wrapper>(
        &self,
        domain: &mut RunQueryResult<'a, W>,
        relation: &'a Relation<W>,
        endpoint: Endpoint,
        ctx: &MatchContext<'a, W>,
        filter: &Filters,
    ) {
        if let Some(m) = &self.item {
            let item_ref = relation.endpoint(!endpoint);
            let item = ctx.items.borrow_item(item_ref).get_wrapped();
            //if m.matches(item, ctx, filter).matches {
            domain.items.insert(item_ref.key(), item_ref.value_ref());
            m.get_search_domain(domain, item, ctx, filter);
            //}
        }
    }
}

impl ItemIdSelector {
    fn matches<'a, F: Filter<'a, R>, R: MatchResult>(
        &'a self,
        item_id: &'a ItemId,
        filter: &F,
    ) -> R {
        match self {
            Self::Is(id) => Some(id == item_id).into(),
            Self::In(ids) => Some(ids.contains(item_id)).into(),
            Self::Template(var) => {
                let value = TplVarValueRef::ItemId(item_id);
                filter.matches(var, Some(value))
            }
        }
    }
}

impl RelationIdSelector {
    fn matches<'a, F: Filter<'a, R>, R: MatchResult>(
        &'a self,
        relation_id: &'a RelationId,
        filter: &F,
    ) -> R {
        match self {
            Self::Is(id) => Some(id == relation_id).into(),
            Self::In(ids) => Some(ids.contains(relation_id)).into(),
            Self::Template(var) => {
                let value = TplVarValueRef::RelationId(relation_id);
                filter.matches(var, Some(value))
            }
        }
    }
}

impl ItemTypeSelector {
    fn to_serial(&self, pkg: Option<&PackageId>) -> serial::ItemTypeSelector {
        match self {
            ItemTypeSelector::Is(v) => serial::ItemTypeSelector::Is(v.key().to_relative_opt(pkg)),
            ItemTypeSelector::In(vs) => serial::ItemTypeSelector::In(
                vs.iter().map(|v| v.key().to_relative_opt(pkg)).collect(),
            ),
            ItemTypeSelector::Template(v) => serial::ItemTypeSelector::Template(v.clone()),
        }
    }

    fn from_serial(
        serial: serial::ItemTypeSelector,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(match serial {
            serial::ItemTypeSelector::Is(t) => Self::Is(RefBy::dangling(t.resolve_opt(pkg))),
            serial::ItemTypeSelector::In(ts) => Self::In(
                ts.into_iter()
                    .map(|t| RefBy::dangling(t.resolve_opt(pkg)))
                    .collect(),
            ),
            serial::ItemTypeSelector::Template(id) => {
                check_tplvar_type(template, &id, TplVarDef::ItemTypes)?;
                Self::Template(id)
            }
        })
    }

    fn resolve(&mut self, types: &Types) -> Result<()> {
        match self {
            Self::Is(m) => m.resolve(&types.items).map_err(Error::MissingItemType),
            Self::In(ms) => ms
                .iter_mut()
                .try_for_each(|m| m.resolve(&types.items).map_err(Error::MissingItemType)),
            /* checked in from_serial */
            Self::Template(_) => Ok(()),
        }
    }

    fn is_filtered_on(&self, filters: &Filters) -> bool {
        match self {
            Self::Template(var) => filters.contains_key(var),
            Self::Is(_) | Self::In(_) => false,
        }
    }

    fn elastic_query<'a>(&'a self, types: &'a Types, result: &mut EsQueryResult<'a>) {
        match self {
            Self::Is(t) => {
                types
                    .iter_type_and_subtypes(t)
                    .for_each(|(t, _)| result.add_item_type(t));
            }
            Self::In(ts) => ts
                .iter()
                .flat_map(|t| types.iter_type_and_subtypes(t))
                .for_each(|(t, _)| result.add_item_type(t)),
            Self::Template(_) => {}
        }
    }

    fn matches<'a, F: Filter<'a, R>, R: MatchResult, W: Wrapper>(
        &'a self,
        item: &'a Item<W>,
        types: &Types,
        filter: &F,
    ) -> R {
        match self {
            Self::Is(m) => Some(
                m.key() == item.item_type.key()
                    || types
                        .items
                        .borrow(&item.item_type)
                        .implements(m.key(), types),
            )
            .into(),
            Self::In(ms) => Some(ms.iter().any(|m| {
                m.key() == item.item_type.key()
                    || types
                        .items
                        .borrow(&item.item_type)
                        .implements(m.key(), types)
            }))
            .into(),
            Self::Template(var) => {
                let value = TplVarValueRef::ItemType(item.item_type.key());
                filter.matches(var, Some(value))
            }
        }
    }
}

impl RelationTypeSelector {
    fn to_serial(&self, pkg: Option<&PackageId>) -> serial::RelationTypeSelector {
        match self {
            RelationTypeSelector::Is(v) => {
                serial::RelationTypeSelector::Is(v.key().to_relative_opt(pkg))
            }
            RelationTypeSelector::In(vs) => serial::RelationTypeSelector::In(
                vs.iter().map(|v| v.key().to_relative_opt(pkg)).collect(),
            ),
            RelationTypeSelector::Template(v) => serial::RelationTypeSelector::Template(v.clone()),
        }
    }

    fn from_serial(
        serial: serial::RelationTypeSelector,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(match serial {
            serial::RelationTypeSelector::Is(t) => Self::Is(RefBy::dangling(t.resolve_opt(pkg))),
            serial::RelationTypeSelector::In(ts) => Self::In(
                ts.into_iter()
                    .map(|t| RefBy::dangling(t.resolve_opt(pkg)))
                    .collect(),
            ),
            serial::RelationTypeSelector::Template(id) => {
                check_tplvar_type(template, &id, TplVarDef::RelationTypes)?;
                Self::Template(id)
            }
        })
    }

    fn resolve(&mut self, types: &Types) -> Result<()> {
        match self {
            Self::Is(m) => m
                .resolve(&types.relations)
                .map_err(Error::MissingRelationType),
            Self::In(ms) => ms.iter_mut().try_for_each(|m| {
                m.resolve(&types.relations)
                    .map_err(Error::MissingRelationType)
            }),
            Self::Template(_) => Ok(()), /* checked in from_serial */
                                         // Self::Not(m) => m.resolve(types),
                                         // Self::Any(ms) | Self::All(ms) => ms.iter_mut().try_for_each(|m| m.resolve(types)),
        }
    }

    fn is_filtered_on(&self, filters: &Filters) -> bool {
        match self {
            Self::Template(var) => filters.contains_key(var),
            Self::Is(_) | Self::In(_) => false,
        }
    }

    fn elastic_query<'a>(&'a self, result: &mut EsQueryResult<'a>) {
        match self {
            Self::Is(t) => result.add_relation_type(t.key()),
            Self::In(ts) => ts.iter().for_each(|t| result.add_relation_type(t.key())),
            Self::Template(_) => {}
        }
    }

    fn matches<'a, F: Filter<'a, R>, R: MatchResult, W: Wrapper>(
        &'a self,
        relation: &'a Relation<W>,
        filter: &F,
    ) -> R {
        match self {
            Self::Is(m) => Some(m.key() == relation.relation_type.key()).into(),
            Self::In(ms) => Some(ms.iter().any(|m| m.key() == relation.relation_type.key())).into(),
            Self::Template(var) => {
                let value = TplVarValueRef::RelationType(relation.relation_type.key());
                filter.matches(var, Some(value))
            }
        }
    }
}

impl PropertySelector {
    fn to_serial(&self, pkg: Option<&PackageId>) -> serial::PropertySelector {
        serial::PropertySelector(
            self.0
                .iter()
                .map(|(p, s)| (p.to_relative_opt(pkg), s.clone()))
                .collect(),
        )
    }

    fn from_serial(
        serial: serial::PropertySelector,
        types: &Types,
        _template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(PropertySelector(
            serial
                .0
                .into_iter()
                .map(|(id, value)| {
                    let id = id.resolve_opt(pkg);
                    let typ = &types
                        .properties
                        .get(&id)
                        .ok_or_else(|| Error::MissingProperty(id.clone()))?
                        .value;
                    typ.verify_selector(&value)?;
                    Ok((id, value))
                })
                .collect::<Result<_>>()?,
        ))
    }

    fn is_filtered_on(&self, filters: &Filters) -> bool {
        self.0.values().any(|sel| sel.is_filtered_on(filters))
    }

    fn matches<'a, F: Filter<'a, R>, R: MatchResult>(
        &'a self,
        properties: &'a BTreeMap<Absolute<PropertyId>, PropertyValue>,
        filter: &F,
    ) -> R {
        self.0
            .iter()
            .map(|(key, sel)| sel.matches(properties.get(key), filter))
            .match_all()
    }
}

impl ValueSelector {
    fn is_filtered_on(&self, filters: &Filters) -> bool {
        match self {
            ValueSelector::String(m) => m.is_filtered_on(filters),
        }
    }

    fn matches<'a, F: Filter<'a, R>, R: MatchResult>(
        &'a self,
        value: Option<&'a PropertyValue>,
        filter: &F,
    ) -> R {
        match self {
            Self::String(m) => m.matches(
                value.and_then(|value| match value {
                    PropertyValue::String(s) => Some(s.as_str()),
                    _ => None,
                }),
                filter,
            ),
        }
    }
}

impl StringSelector {
    fn is_filtered_on(&self, filters: &Filters) -> bool {
        match self {
            Self::Template(var) => filters.contains_key(var),
            Self::Equals(_) | Self::In(_) => false,
            Self::Not(m) => m.is_filtered_on(filters),
            Self::All(ms) | Self::Any(ms) => ms.iter().any(|m| m.is_filtered_on(filters)),
        }
    }

    fn matches<'a, F: Filter<'a, R>, R: MatchResult>(
        &'a self,
        value: Option<&'a str>,
        filter: &F,
    ) -> R {
        match self {
            Self::Equals(v) => Some(value == Some(v.as_str())).into(),
            Self::In(vs) => Some(value.is_some_and(|v| vs.contains(v))).into(),
            Self::Template(var) => {
                let value = value.map(TplVarValueRef::String);
                filter.matches(var, value)
            }
            Self::Not(m) => m.matches(value, filter).not(),
            Self::All(cs) => cs.iter().map(|c| c.matches(value, filter)).match_all(),
            Self::Any(cs) => cs.iter().map(|c| c.matches(value, filter)).match_any(),
        }
    }
}

impl Not for Endpoint {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::Source => Self::Target,
            Self::Target => Self::Source,
        }
    }
}

impl Follow {
    fn from_serial(
        serial: serial::Follow,
        types: &Types,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self {
            relation: RelationSelector::from_serial(serial.relation, types, template, pkg)?,
            element: RefBy::dangling(serial.element),
        })
    }

    fn resolve(
        &mut self,
        types: &Types,
        query: &BTreeMap<QueryElemId, Ref<QueryElem>>,
    ) -> Result<()> {
        self.relation.resolve(types)?;
        self.element.resolve(query).map_err(Error::MissingQueryElem)
    }

    fn get_search_domain<'a, W: Wrapper>(
        &'a self,
        domain: &mut RunQueryResult<'a, W>,
        item: &'a Item<W>,
        filters: &'a Filters,
        seen: &mut SeenMap<(&'a QueryElemId, &'a ItemId), ()>,
        ctx: &QueryContext<'a, '_, W>,
    ) {
        let next_elem = ctx.elements.borrow(&self.element);
        item.relations(
            self.relation.prefilter_relation_type(),
            self.relation.prefilter_endpoint(),
        )
        .for_each(|((relation_id, rel_ref), endpoint)| {
            let relation = ctx.mctx.items.borrow_relation(rel_ref).get_wrapped();
            let (item_id, item_ref) = relation.endpoint(!endpoint).pair();
            let item = ctx.mctx.items.borrow_item(item_ref).get_wrapped();
            let m: Match = self.matches(
                next_elem,
                &RelArgs::new(relation_id, relation, endpoint),
                &ItemArgs::new(item_id, item),
                filters,
                ctx,
            );
            if m.matches() {
                domain
                    .relations
                    .insert(relation_id, (rel_ref, matches!(endpoint, Endpoint::Target)));
                domain.items.insert(item_id, item_ref);
                self.relation
                    .get_search_domain(domain, relation, endpoint, ctx.mctx, filters);
                next_elem
                    .items
                    .get_search_domain(domain, item, ctx.mctx, filters);
                seen.run((self.element.key(), item_id), |seen| {
                    next_elem.get_search_domain_follows(domain, item, filters, seen, ctx)
                });
            }
        });
    }

    fn run<'a: 'b, 'b, F, Q, R, S, W>(
        &'a self,
        item: &'a Item<W>,
        filters: &'b F,
        wrapper_filter: &'b Q,
        seen: &mut Seen<'a>,
        ctx: &QueryContext<'a, '_, W>,
        result: &mut S,
    ) -> std::result::Result<bool, S::Error>
    where
        F: Filter<'a, R>,
        Q: WrapperFilter<W, R>,
        R: MatchResult,
        S: RunState<R, W>,
        W: Wrapper,
    {
        let next_elem = ctx.elements.borrow(&self.element);
        #[allow(clippy::unnecessary_fold)]
        let shown = item
            .relations(
                self.relation.prefilter_relation_type(),
                self.relation.prefilter_endpoint(),
            )
            .filter_map(|((relation_id, rel_ref), endpoint)| {
                let wrapped_rel = ctx.mctx.items.borrow_relation(rel_ref);
                let relation = wrapped_rel.try_get_wrapped()?;
                let (item_id, item_ref) = relation.endpoint(!endpoint).pair();
                let wrapped_item = ctx.mctx.items.borrow_item(item_ref);
                let item = wrapped_item.try_get_wrapped()?;

                // eprintln!(
                //     "Follow::run: run_matches on {} -> {}",
                //     ShowRel(rel),
                //     ShowItem(item)
                // );

                let m = wrapper_filter
                    .matches(wrapped_rel)
                    .or(|| wrapper_filter.matches(wrapped_item))
                    .and(|| {
                        self.matches(
                            next_elem,
                            &RelArgs::new(relation_id, relation, endpoint),
                            &ItemArgs::new(item_id, item),
                            filters,
                            ctx,
                        )
                    });
                // let (m, opts) = run_matches!(
                //     ctx,
                //     filters,
                //     flt,
                //     self.matches(next_elem, rel_id, rel, ep, item_id, item, flt, ctx)
                // )?;

                let shown = if m.matches() {
                    // eprintln!(
                    //     "Follow::run: matched {} -> {} ({m})",
                    //     ShowRel(rel),
                    //     ShowItem(item)
                    // );

                    let follow_shown = match seen.try_run((self.element.key(), item_id), |seen| {
                        next_elem.run_follows(item, filters, wrapper_filter, seen, ctx, result)
                    }) {
                        Ok(shown) => shown,
                        Err(e) => return Some(Err(e)),
                    };

                    if follow_shown
                        || ((!ctx.filtered.contains(self.element.key()) || m.filtered())
                            && (!wrapper_filter.is_filtered() || m.wrapper_filtered()))
                    {
                        if let Err(e) = result.add_match(m) {
                            return Some(Err(e));
                        }
                        if let Err(e) = result.add_relation(
                            relation_id.clone(),
                            rel_ref.clone(),
                            endpoint == Endpoint::Target,
                        ) {
                            return Some(Err(e));
                        }
                        if let Err(e) = result.add_item(item_id.clone(), item_ref.clone()) {
                            return Some(Err(e));
                        }
                        // opts.into_iter().for_each(|(var_id, var_values)| {
                        //     match result.template.entry(var_id.clone()) {
                        //         Entry::Occupied(mut ent) => {
                        //             *ent.get_mut() |= var_values;
                        //         }
                        //         Entry::Vacant(ent) => {
                        //             ent.insert(var_values.cloned());
                        //         }
                        //     }
                        // });

                        // eprintln!(
                        //     "{} required if {:?}",
                        //     self.element.key(),
                        //     self.required_if(ctx)
                        // );

                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

                Some(Ok(shown))
            })
            /* Note: must be fold since any short-circuits! */
            .try_fold(false, |a, br| {
                let b = br?;
                Ok(a || b)
            })?;

        Ok(shown)
    }

    fn matches<'a, F: Filter<'a, R>, R: MatchResult, W: Wrapper>(
        &'a self,
        elem: &'a QueryElem,
        rel: &RelArgs<'a, W>,
        item: &ItemArgs<'a, W>,
        filters: &F,
        ctx: &QueryContext<'a, '_, W>,
    ) -> R {
        self.relation
            .matches(rel, ctx.mctx, filters)
            .and(|| elem.items.matches(item, ctx.mctx, filters))
    }

    fn is_filtered_on<'a>(
        &'a self,
        filters: &Filters,
        elems: &'a QueryElems,
        memo: &mut BTreeMap<&'a QueryElemId, bool>,
    ) -> bool {
        self.relation.is_filtered_on(filters)
            || match memo.entry(self.element.key()) {
                Entry::Occupied(ent) => *ent.get(),
                Entry::Vacant(ent) => {
                    ent.insert(false);
                    let r = elems
                        .borrow(&self.element)
                        .is_filtered_on(filters, elems, memo);
                    memo.insert(self.element.key(), r);
                    r
                }
            }
    }
}

// impl TplVarDef {
// 	fn empty_value_set(&self) -> TplVarValues {
// 		match self {
// 			TplVarDef::Strings => TplVarValues::Strings(TplVarStrings::default()),
// 			TplVarDef::ItemTypes => TplVarValues::ItemTypes(TplVarItemTypes::default()),
// 			TplVarDef::RelationTypes => TplVarValues::RelationTypes(TplVarRelationTypes::default()),
// 			TplVarDef::ItemIds => TplVarValues::ItemIds(TplVarItemIds::default()),
// 			TplVarDef::RelationIds => TplVarValues::RelationIds(TplVarRelationIds::default()),
// 		}
// 	}
// }

impl TplVarValues {
    pub(super) fn from_value(
        typ: &TplVarDef,
        value: serde_json::Value,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        match typ {
            TplVarDef::Strings => Ok(TplVarValues::Strings(
                serde_json::from_value(value).map_err(Error::InvalidTplVarValue)?,
            )),
            TplVarDef::ItemTypes => Ok(TplVarValues::ItemTypes(TplVarItemTypes(
                serde_json::from_value::<BTreeSet<Relative<ItemTypeId>>>(value)
                    .map_err(Error::InvalidTplVarValue)?
                    .into_iter()
                    .map(|v| v.resolve_opt(pkg))
                    .collect(),
            ))),
            TplVarDef::RelationTypes => Ok(TplVarValues::RelationTypes(TplVarRelationTypes(
                serde_json::from_value::<BTreeSet<Relative<RelationTypeId>>>(value)
                    .map_err(Error::InvalidTplVarValue)?
                    .into_iter()
                    .map(|v| v.resolve_opt(pkg))
                    .collect(),
            ))),
            TplVarDef::ItemIds => Ok(TplVarValues::ItemIds(TplVarItemIds(
                serde_json::from_value::<BTreeSet<ItemId>>(value)
                    .map_err(Error::InvalidTplVarValue)?,
            ))),
            TplVarDef::RelationIds => Ok(TplVarValues::RelationIds(TplVarRelationIds(
                serde_json::from_value::<BTreeSet<RelationId>>(value)
                    .map_err(Error::InvalidTplVarValue)?,
            ))),
        }
    }
}

impl<W: Wrapper> QueryResult<W> {
    pub fn to_serial_unwrapped(
        &self,
        items: &Items<W>,
        pkg: Option<&PackageId>,
    ) -> serial::QueryResult<Identity> {
        serial::QueryResult {
            result: self
                .result
                .as_ref()
                .map(|r| r.to_serial_unwrapped(items, pkg))
                .map_err(|e| e.clone()),
            template: serialize_template(&self.template, pkg),
        }
    }

    pub fn to_serial_wrapped(
        &self,
        items: &Items<W>,
        pkg: Option<&PackageId>,
    ) -> serial::QueryResult<W>
    where
        WrappedItem<W>: Clone,
        WrappedRelation<W>: Clone,
    {
        serial::QueryResult {
            result: self
                .result
                .as_ref()
                .map(|r| r.to_serial_wrapped(items, pkg))
                .map_err(|e| e.clone()),
            template: serialize_template(&self.template, pkg),
        }
    }
}

fn serialize_template(
    template: &BTreeMap<TplVarId, TplVarValues>,
    pkg: Option<&PackageId>,
) -> BTreeMap<TplVarId, serde_json::Value> {
    template
        .iter()
        .map(|(id, vs)| {
            (
                id.clone(),
                serde_json::Value::Array(match vs {
                    TplVarValues::Strings(vs) => vs.0.iter().map(|v| json!(v)).collect(),
                    TplVarValues::ItemIds(vs) => vs.0.iter().map(|v| json!(v)).collect(),
                    TplVarValues::RelationIds(vs) => vs.0.iter().map(|v| json!(v)).collect(),
                    TplVarValues::ItemTypes(vs) => {
                        vs.0.iter()
                            .map(|v| json!(v.to_relative_opt(pkg).to_string()))
                            .collect()
                    }
                    TplVarValues::RelationTypes(vs) => {
                        vs.0.iter()
                            .map(|v| json!(v.to_relative_opt(pkg).to_string()))
                            .collect()
                    }
                }),
            )
        })
        .collect()
}

impl<W: Wrapper> QueryResultItems<W> {
    pub fn to_serial_unwrapped(
        &self,
        items: &Items<W>,
        pkg: Option<&PackageId>,
    ) -> serial::QueryResultItems<Identity> {
        serial::QueryResultItems {
            items: self
                .items
                .iter_ref()
                .map(|(id, item)| {
                    (
                        id.clone(),
                        items.borrow_item(item).get_wrapped().to_serial(pkg),
                    )
                })
                .collect(),
            relations: self
                .relations
                .iter()
                .map(|(id, (rel, invert))| {
                    let mut rel = items.borrow_relation(rel).get_wrapped().to_serial(pkg);
                    if *invert {
                        let rel = rel.get_wrapped_mut();
                        std::mem::swap(&mut rel.source, &mut rel.target);
                    }
                    (id.clone(), rel)
                })
                .collect(),
        }
    }

    pub fn to_serial_wrapped(
        &self,
        items: &Items<W>,
        pkg: Option<&PackageId>,
    ) -> serial::QueryResultItems<W>
    where
        WrappedItem<W>: Clone,
        WrappedRelation<W>: Clone,
    {
        serial::QueryResultItems {
            items: self
                .items
                .iter_ref()
                .map(|(id, item)| {
                    (
                        id.clone(),
                        items
                            .borrow_item(item)
                            .clone()
                            .map_wrapped(|item| item.to_serial(pkg)),
                    )
                })
                .collect(),
            relations: self
                .relations
                .iter()
                .map(|(id, (rel, invert))| {
                    let mut rel = items
                        .borrow_relation(rel)
                        .clone()
                        .map_wrapped(|rel| rel.to_serial(pkg));
                    if *invert {
                        let rel = rel.get_wrapped_mut();
                        std::mem::swap(&mut rel.source, &mut rel.target);
                    }
                    (id.clone(), rel)
                })
                .collect(),
        }
    }

    pub fn get_item_types(
        &self,
        types: &Types,
        items: &Items<W>,
    ) -> BTreeSet<Absolute<ItemTypeId>> {
        fn get_implements(
            item_type_id: &Absolute<ItemTypeId>,
            item_type_ref: &Ref<ItemType>,
            item_types: &mut BTreeSet<Absolute<ItemTypeId>>,
            types: &Types,
        ) {
            if item_types.insert(item_type_id.clone()) {
                let item_type = types.items.borrow(item_type_ref);
                item_type
                    .implements
                    .iter_ref()
                    .for_each(|(item_type_id, item_type_ref)| {
                        get_implements(item_type_id, item_type_ref, item_types, types)
                    });
            }
        }

        let mut item_types = BTreeSet::new();
        self.items
            .value_refs()
            .map(|item| items.borrow_item(item).get_wrapped().item_type.pair())
            .for_each(|(item_type_id, item_type_ref)| {
                get_implements(item_type_id, item_type_ref, &mut item_types, types)
            });
        item_types
    }
}

impl TplVarValueRef<'_> {
    pub(crate) fn get_string(&self) -> Option<&str> {
        match self {
            TplVarValueRef::String(s) => Some(s),
            _ => None,
        }
    }

    pub(crate) fn get_item_id(&self) -> Option<&ItemId> {
        match self {
            TplVarValueRef::ItemId(id) => Some(id),
            _ => None,
        }
    }

    pub(crate) fn get_relation_id(&self) -> Option<&RelationId> {
        match self {
            TplVarValueRef::RelationId(id) => Some(id),
            _ => None,
        }
    }

    pub(crate) fn get_item_type(&self) -> Option<&Absolute<ItemTypeId>> {
        match self {
            TplVarValueRef::ItemType(t) => Some(t),
            _ => None,
        }
    }

    pub(crate) fn get_relation_type(&self) -> Option<&Absolute<RelationTypeId>> {
        match self {
            TplVarValueRef::RelationType(t) => Some(t),
            _ => None,
        }
    }

    pub fn to_values(self) -> TplVarValues {
        match self {
            TplVarValueRef::String(s) => {
                TplVarValues::Strings(TplVarStrings(BTreeSet::from_iter([s.to_string()])))
            }
            TplVarValueRef::ItemType(v) => {
                TplVarValues::ItemTypes(TplVarItemTypes(BTreeSet::from_iter([v.clone()])))
            }
            TplVarValueRef::RelationType(v) => {
                TplVarValues::RelationTypes(TplVarRelationTypes(BTreeSet::from_iter([v.clone()])))
            }
            TplVarValueRef::ItemId(v) => {
                TplVarValues::ItemIds(TplVarItemIds(BTreeSet::from_iter([v.clone()])))
            }
            TplVarValueRef::RelationId(v) => {
                TplVarValues::RelationIds(TplVarRelationIds(BTreeSet::from_iter([v.clone()])))
            }
        }
    }
}

struct SeenMap<K, V>(BTreeMap<K, SeenState<V>>);

enum SeenState<T> {
    Calculating,
    Ready(T),
}

impl<K: Ord + Clone, V: Default + Clone> SeenMap<K, V> {
    fn new() -> Self {
        Self(BTreeMap::new())
    }

    fn run<F>(&mut self, key: K, f: F) -> V
    where
        F: FnOnce(&mut Self) -> V,
    {
        self.try_run::<Infallible, _>(key, |this| Ok(f(this)))
            .unwrap()
    }

    fn try_run<E, F>(&mut self, key: K, f: F) -> std::result::Result<V, E>
    where
        F: FnOnce(&mut Self) -> std::result::Result<V, E>,
    {
        match self.0.entry(key.clone()) {
            Entry::Vacant(ent) => {
                ent.insert(SeenState::Calculating);
                let r = f(self)?;
                self.0.insert(key, SeenState::Ready(r.clone()));
                Ok(r)
            }
            Entry::Occupied(mut ent) => match ent.get_mut() {
                SeenState::Ready(r) => Ok(r.clone()),
                /* Recursive calculation. */
                r @ SeenState::Calculating => {
                    let v = V::default();
                    *r = SeenState::Ready(v.clone());
                    Ok(v)
                }
            },
        }
    }
}

fn check_tplvar_type(
    template: &BTreeMap<TplVarId, TplVarDef>,
    id: &TplVarId,
    expected: TplVarDef,
) -> Result<()> {
    let typ = *template
        .get(id)
        .ok_or_else(|| Error::MissingTplVarDef(id.clone()))?;
    (typ == expected)
        .then_some(())
        .ok_or_else(|| Error::InvalidTplVarType(id.clone(), expected, typ))
}

// pub(crate) struct ShowItem<'a, W: Wrapper>(pub(crate) &'a Item<W>);
// pub(crate) struct ShowRel<'a, W: Wrapper>(pub(crate) &'a Relation<W>);

// impl<W: Wrapper> Display for ShowItem<'_, W> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let name: Absolute<PropertyId> =
//             Absolute::<PropertyId>::from_str("kubernetes/name").unwrap();
//         write!(
//             f,
//             "{} {}",
//             self.0.item_type.key(),
//             self.0.properties.get(&name).unwrap()
//         )
//     }
// }

// impl<W: Wrapper> Display for ShowRel<'_, W> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.0.relation_type.key())
//     }
// }

#[cfg(test)]
mod tests {
    use std::{collections::BTreeSet, path::Path, str::FromStr};

    use serde_json::json;
    use wrapper::Identity;

    use crate::{
        ids::PackageId,
        items::{
            resolved::Items,
            serial::{self, PropertyValue},
        },
        types::{resolved::Types, serial::Packages},
        Query,
    };

    use super::MatchContext;

    fn load_test_case(name: &str) -> (PackageId, Types, Items<Identity>) {
        let pkgid: PackageId = PackageId::from_str(name).unwrap();
        let types = Packages::load_sync(Path::new(&format!("../tests/{name}/pkgs")))
            .unwrap()
            .types()
            .unwrap();
        let items = serial::Items::load(Path::new(&format!("../tests/{name}/items.json")))
            .unwrap()
            .resolve(&types, Some(&pkgid))
            .unwrap();
        (pkgid, types, items)
    }

    fn jaeger_query(types: &Types) -> Query {
        let mut query = Query::from_serial(
            serde_json::from_value(json!({
                "root": "service",
                "elements": {
                    "service": {
                        "items": {
                            "match": {
                                "item_type": { "is": "jaeger/service" },
                                "properties": {
                                    "jaeger/service_namespace": {
                                        "string": {
                                            "template": "namespace"
                                        }
                                    },
                                    "jaeger/service_name": {
                                        "string": {
                                            "template": "service"
                                        }
                                    },
                                    "jaeger/service_instance_id": {
                                        "string": {
                                            "template": "instance"
                                        }
                                    }
                                }
                            }
                        },
                        "follow": [
                            {
                                "relation": {
                                    "match": {
                                        "relation_type": {
                                            "in": ["jaeger/service_invokes"]
                                        },
                                        "endpoint": "source"
                                    }
                                },
                                "element": "service"
                            }
                        ]
                    }
                },
                "template": {
                    "namespace": "strings",
                    "service": "strings",
                    "instance": "strings"
                }
            }))
            .unwrap(),
            types,
            None,
        )
        .unwrap();
        query.resolve(types).unwrap();
        query
    }

    fn kubernetes_workloads_query(types: &Types) -> Query {
        let mut query = Query::from_serial(
            serde_json::from_value(json!({
                "root": "cluster",
                "elements": {
                    "cluster": {
                        "items": {
                            "match": {
                                "item_type": {
                                    "in": ["kubernetes/cluster"]
                                },
                                "properties": {
                                    "kubernetes/name": {
                                        "string": {
                                            "template": "cluster"
                                        }
                                    }
                                }
                            }
                        },
                        "follow": [
                            {
                                "relation": {
                                    "match": {
                                        "relation_type": {
                                            "in": ["kubernetes/cluster-namespace"]
                                        }
                                    }
                                },
                                "element": "namespace"
                            }
                        ]
                    },
                    "namespace": {
                        "items": {
                            "match": {
                                "item_type": {
                                    "in": ["kubernetes/namespace"]
                                },
                                "properties": {
                                    "kubernetes/name": {
                                        "string": {
                                            "template": "namespace"
                                        }
                                    }
                                }
                            }
                        },
                        "follow": [
                            {
                                "relation": {
                                    "match": {
                                        "relation_type": {
                                            "in": [
                                                "kubernetes/namespace-stateful_set",
                                                "kubernetes/namespace-deployment",
                                                "kubernetes/namespace-daemon_set",
                                                "kubernetes/namespace-cron_job",
                                                "kubernetes/namespace-job"
                                            ]
                                        }
                                    }
                                },
                                "element": "pod_controller"
                            }
                        ]
                    },
                    "pod_controller": {
                        "items": {
                            "match": {
                                "item_type": {
                                    "in": [
                                        "kubernetes/stateful_set",
                                        "kubernetes/deployment",
                                        "kubernetes/daemon_set",
                                        "kubernetes/cron_job",
                                        "kubernetes/job"
                                    ]
                                },
                                "properties": {
                                    "kubernetes/name": {
                                        "string": {
                                            "template": "pod_controller"
                                        }
                                    }
                                }
                            }
                        },
                        "follow": [
                            {
                                "relation": {
                                    "match": {
                                        "relation_type": {
                                            "in": [
                                                "kubernetes/stateful_set-pod",
                                                "kubernetes/deployment-pod",
                                                "kubernetes/daemon_set-pod",
                                                "kubernetes/cron_job-pod",
                                                "kubernetes/job-pod"
                                            ]
                                        }
                                    }
                                },
                                "element": "pod"
                            }
                        ]
                    },
                    "pod": {
                        "items": {
                            "match": {
                                "item_type": {
                                    "is": "kubernetes/pod"
                                },
                                "properties": {
                                    "kubernetes/name": {
                                        "string": {
                                            "template": "pod"
                                        }
                                    }
                                }
                            }
                        },
                        "follow": [
                            {
                                "relation": {
                                    "match": {
                                        "relation_type": {
                                            "in": ["kubernetes/service-pod"]
                                        }
                                    }
                                },
                                "element": "service"
                            }
                        ]
                    },
                    "service": {
                        "items": {
                            "match": {
                                "item_type": {
                                    "is": "kubernetes/service"
                                },
                                "properties": {
                                    "kubernetes/name": {
                                        "string": {
                                            "template": "service"
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                "template": {
                    "cluster": "strings",
                    "namespace": "strings",
                    "pod_controller": "strings",
                    "pod": "strings",
                    "service": "strings"
                }
            }))
            .unwrap(),
            types,
            None,
        )
        .unwrap();
        query.resolve(types).unwrap();
        query
    }

    #[test]
    fn prefilter_jaeger_services() {
        let (_jaeger, types, items) = load_test_case("jaeger");
        let query = jaeger_query(&types);

        let ctx = MatchContext::new(&items, &types);
        let filters = serde_json::from_value(json!({})).unwrap();

        let services = query
            .elements
            .get(&"service".parse().unwrap())
            .unwrap()
            .items
            .prefilter(&ctx, Some(&filters))
            .map(|(_, item_ref)| items.items.borrow(item_ref))
            .filter_map(|item| item.property(&"jaeger/service_name".parse().unwrap()))
            .filter_map(|value| match value {
                PropertyValue::String(v) => Some(v.as_str()),
                _ => None,
            })
            .collect::<BTreeSet<_>>();

        assert_eq!(
            &services,
            &BTreeSet::from_iter([
                "cortex-alertmanager",
                "cortex-distributor",
                "cortex-ingester",
                "cortex-querier",
                "cortex-query-frontend",
                "cortex-ruler",
                "dbdaemon",
                "relation-graph-engine",
                "continuousc-frontend"
            ])
        );
    }

    #[test]
    fn query_jaeger_services() {
        let (_jaeger, types, items) = load_test_case("jaeger");
        let query = jaeger_query(&types);

        let filters = serde_json::from_value(json!({})).unwrap();

        let result = query.run_with_filters(&items, &types, &filters, &(), None);
        let result_items = result.result.unwrap();

        let services = result_items
            .items
            .values(&items.items)
            .filter_map(|item| item.property(&"jaeger/service_name".parse().unwrap()))
            .filter_map(|value| match value {
                PropertyValue::String(v) => Some(v.as_str()),
                _ => None,
            })
            .collect::<BTreeSet<_>>();

        assert_eq!(
            &services,
            &BTreeSet::from_iter([
                "cortex-alertmanager",
                "cortex-distributor",
                "cortex-ingester",
                "cortex-querier",
                "cortex-query-frontend",
                "cortex-ruler",
                "dbdaemon",
                "relation-graph-engine",
                "continuousc-frontend"
            ])
        );

        let tplvar_services = result
            .template
            .get(&"service".parse().unwrap())
            .unwrap()
            .strings()
            .unwrap()
            .iter()
            .map(|s| s.as_str())
            .collect::<BTreeSet<_>>();

        assert_eq!(
            &tplvar_services,
            &BTreeSet::from_iter([
                "cortex-alertmanager",
                "cortex-distributor",
                "cortex-ingester",
                "cortex-querier",
                "cortex-query-frontend",
                "cortex-ruler",
                "dbdaemon",
                "relation-graph-engine",
                "continuousc-frontend"
            ])
        );

        // panic!(
        //     "Result: {}",
        //     serde_json::to_string_pretty(&result.to_serial_unwrapped(&items, None)).unwrap()
        // );
    }

    #[test]
    fn query_kubernetes_workloads() {
        let (_k8s, types, items) = load_test_case("jaeger");
        let query = kubernetes_workloads_query(&types);

        let filters = serde_json::from_value(json!({
            "namespace": {"strings": ["default"]}
        }))
        .unwrap();
        let result = query.run_with_filters(&items, &types, &filters, &(), None);
        //let result_items = result.result.unwrap();

        let tplvar_pods = result
            .template
            .get(&"pod".parse().unwrap())
            .unwrap()
            .strings()
            .unwrap()
            .iter()
            .map(|s| s.as_str())
            .collect::<BTreeSet<_>>();

        assert_eq!(
            &tplvar_pods,
            &BTreeSet::from_iter(["etcd-0", "etcd-1", "etcd-2",])
        );

        let tplvar_services = result
            .template
            .get(&"service".parse().unwrap())
            .unwrap()
            .strings()
            .unwrap()
            .iter()
            .map(|s| s.as_str())
            .collect::<BTreeSet<_>>();

        assert_eq!(
            &tplvar_services,
            &BTreeSet::from_iter(["etcd", "etcd-cluster"])
        );
    }
}
