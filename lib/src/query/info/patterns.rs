/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{borrow::Cow, fmt::Display, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, space0},
    combinator::{eof, map, map_opt, map_res, value},
    multi::many1,
    sequence::{delimited, terminated, tuple},
    Finish, IResult, Parser,
};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::{Absolute, ItemTypeId, RelationTypeId};

#[derive(SerializeDisplay, DeserializeFromStr, PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct QueryItemPattern {
    pub(super) item_type: Option<Absolute<ItemTypeId>>,
}

pub(super) struct QueryItemPatternParams<'a> {
    pub(super) item_type: &'a Absolute<ItemTypeId>,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
pub(super) struct QueryItemPatternRef<'a> {
    pub(super) item_type: Option<&'a Absolute<ItemTypeId>>,
}

impl QueryItemPattern {
    pub(super) fn as_ref(&self) -> QueryItemPatternRef {
        QueryItemPatternRef {
            item_type: self.item_type.as_ref(),
        }
    }
}

impl<'a> QueryItemPatternParams<'a> {
    pub(super) fn iter_patterns(&self) -> impl Iterator<Item = QueryItemPatternRef<'a>> {
        [
            QueryItemPatternRef {
                item_type: Some(self.item_type),
            },
            QueryItemPatternRef { item_type: None },
        ]
        .into_iter()
    }
}

impl Display for QueryItemPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", EscapePattern(self.item_type.as_ref()))
    }
}

impl FromStr for QueryItemPattern {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(terminated(
            map(parse_pattern_elem::<Absolute<ItemTypeId>>, |item_type| {
                Self { item_type }
            }),
            eof,
        )
        .parse(s)
        .finish()
        .map_err(|e| e.to_string())?
        .1)
    }
}

#[derive(SerializeDisplay, DeserializeFromStr, PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct QueryRelationPattern {
    pub(super) relation_type: Option<Absolute<RelationTypeId>>,
    pub(super) source: Option<Absolute<ItemTypeId>>,
    pub(super) target: Option<Absolute<ItemTypeId>>,
}

pub(super) struct QueryRelationPatternParams<'a> {
    pub(super) relation_type: &'a Absolute<RelationTypeId>,
    pub(super) source: &'a Absolute<ItemTypeId>,
    pub(super) target: &'a Absolute<ItemTypeId>,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
pub(super) struct QueryRelationPatternRef<'a> {
    pub(super) relation_type: Option<&'a Absolute<RelationTypeId>>,
    pub(super) source: Option<&'a Absolute<ItemTypeId>>,
    pub(super) target: Option<&'a Absolute<ItemTypeId>>,
}

impl QueryRelationPattern {
    pub(super) fn as_ref(&self) -> QueryRelationPatternRef {
        QueryRelationPatternRef {
            relation_type: self.relation_type.as_ref(),
            source: self.source.as_ref(),
            target: self.target.as_ref(),
        }
    }
}

impl<'a> QueryRelationPatternParams<'a> {
    pub(super) fn iter_patterns(&self) -> impl Iterator<Item = QueryRelationPatternRef<'a>> {
        [
            QueryRelationPatternRef {
                relation_type: Some(self.relation_type),
                source: Some(self.source),
                target: Some(self.target),
            },
            QueryRelationPatternRef {
                relation_type: None,
                source: Some(self.source),
                target: Some(self.target),
            },
            QueryRelationPatternRef {
                relation_type: Some(self.relation_type),
                source: None,
                target: Some(self.target),
            },
            QueryRelationPatternRef {
                relation_type: None,
                source: None,
                target: Some(self.target),
            },
            QueryRelationPatternRef {
                relation_type: Some(self.relation_type),
                source: Some(self.source),
                target: None,
            },
            QueryRelationPatternRef {
                relation_type: None,
                source: Some(self.source),
                target: None,
            },
            QueryRelationPatternRef {
                relation_type: Some(self.relation_type),
                source: None,
                target: None,
            },
            QueryRelationPatternRef {
                relation_type: None,
                source: None,
                target: None,
            },
        ]
        .into_iter()
    }
}

impl Display for QueryRelationPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} : {} -> {}",
            EscapePattern(self.relation_type.as_ref()),
            EscapePattern(self.source.as_ref()),
            EscapePattern(self.target.as_ref())
        )
    }
}

impl FromStr for QueryRelationPattern {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(terminated(
            alt((
                map(
                    tuple((
                        terminated(
                            parse_pattern_elem::<Absolute<RelationTypeId>>,
                            delimited(space0, char(':'), space0),
                        ),
                        terminated(
                            parse_pattern_elem::<Absolute<ItemTypeId>>,
                            delimited(space0, tag("->"), space0),
                        ),
                        parse_pattern_elem::<Absolute<ItemTypeId>>,
                    )),
                    |(relation_type, source, target)| Self {
                        relation_type,
                        source,
                        target,
                    },
                ),
                map(
                    tuple((
                        terminated(
                            parse_pattern_elem::<Absolute<ItemTypeId>>,
                            delimited(space0, tag("->"), space0),
                        ),
                        parse_pattern_elem::<Absolute<ItemTypeId>>,
                    )),
                    |(source, target)| Self {
                        relation_type: None,
                        source,
                        target,
                    },
                ),
                map(
                    parse_pattern_elem::<Absolute<RelationTypeId>>,
                    |relation_type| Self {
                        relation_type,
                        source: None,
                        target: None,
                    },
                ),
            )),
            eof,
        )
        .parse(s)
        .finish()
        .map_err(|e| e.to_string())?
        .1)
    }
}

struct EscapePattern<T>(Option<T>);

impl<T: Display> Display for EscapePattern<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(inner) = &self.0 {
            inner.to_string().chars().try_for_each(|c| match c {
                'A'..='Z' | 'a'..='z' | '0'..='9' | '/' | '-' | '_' => write!(f, "{c}"),
                _ => write!(f, "{}", c.escape_unicode()),
            })
        } else {
            write!(f, "*")
        }
    }
}

fn parse_pattern_elem<T: FromStr + Clone>(input: &str) -> IResult<&str, Option<T>> {
    alt((
        value(None, char('*')),
        map_res(
            many1(alt((
                map(
                    take_while1(|c| matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '/'| '-'|'_')),
                    Cow::Borrowed,
                ),
                map_opt(
                    delimited(
                        tag("\\u{"),
                        take_while1(|c: char| c.is_ascii_hexdigit()),
                        char('}'),
                    ),
                    |s| {
                        Some(Cow::Owned(
                            char::from_u32(u32::from_str_radix(s, 16).ok()?)?.to_string(),
                        ))
                    },
                ),
            ))),
            |s| {
                if s.len() == 1 {
                    T::from_str(s[0].as_ref())
                } else {
                    T::from_str(&s.concat())
                }
                .map(Some)
            },
        ),
    ))(input)
}
