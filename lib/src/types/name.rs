/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, fmt::Display, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{anychar, char},
    combinator::{eof, map, map_res, recognize, value},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated},
    Finish, IResult,
};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::{items::serial::PropertyValue, Absolute, Error, PackageId, PropertyId, Relative};

#[derive(
    SerializeDisplay, DeserializeFromStr, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "string"))]
pub struct RelativeNameTemplate(
    #[cfg_attr(feature = "schemars", schemars(with = "String"))]
    Vec<TemplateElem<Relative<PropertyId>>>,
);

#[derive(
    SerializeDisplay, DeserializeFromStr, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "string"))]
pub struct AbsoluteNameTemplate(
    #[cfg_attr(feature = "schemars", schemars(with = "String"))]
    Vec<TemplateElem<Absolute<PropertyId>>>,
);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug)]
enum TemplateElem<T> {
    Literal(String),
    Separator(String),
    Property(T),
}

impl RelativeNameTemplate {
    pub fn resolve(self, pkg: Option<&PackageId>) -> AbsoluteNameTemplate {
        AbsoluteNameTemplate(
            self.0
                .into_iter()
                .map(|elem| match elem {
                    TemplateElem::Literal(s) => TemplateElem::Literal(s),
                    TemplateElem::Separator(s) => TemplateElem::Separator(s),
                    TemplateElem::Property(p) => TemplateElem::Property(p.resolve_opt(pkg)),
                })
                .collect(),
        )
    }
}

impl AbsoluteNameTemplate {
    pub fn to_relative(&self, pkg: Option<&PackageId>) -> RelativeNameTemplate {
        RelativeNameTemplate(
            self.0
                .iter()
                .map(|elem| match elem {
                    TemplateElem::Literal(s) => TemplateElem::Literal(s.clone()),
                    TemplateElem::Separator(s) => TemplateElem::Separator(s.clone()),
                    TemplateElem::Property(p) => TemplateElem::Property(p.to_relative_opt(pkg)),
                })
                .collect(),
        )
    }

    pub fn into_relative(self, pkg: Option<&PackageId>) -> RelativeNameTemplate {
        RelativeNameTemplate(
            self.0
                .into_iter()
                .map(|elem| match elem {
                    TemplateElem::Literal(s) => TemplateElem::Literal(s),
                    TemplateElem::Separator(s) => TemplateElem::Separator(s),
                    TemplateElem::Property(p) => TemplateElem::Property(p.to_relative_opt(pkg)),
                })
                .collect(),
        )
    }

    pub fn render<'a>(
        &'a self,
        properties: &'a BTreeMap<Absolute<PropertyId>, PropertyValue>,
    ) -> impl Display + 'a {
        Render(&self.0, properties)
    }
}

struct Render<'a, T>(&'a [TemplateElem<T>], &'a BTreeMap<T, PropertyValue>);

impl<T: Ord + Eq> Render<'_, T> {
    fn separate(&self, i: usize) -> bool {
        i.checked_sub(1).and_then(|i| self.0.get(i)).map_or(
            true,
            |elem| matches!(elem, TemplateElem::Property(p) if self.1.contains_key(p)),
        ) && i.checked_add(1).and_then(|i| self.0.get(i)).map_or(
            true,
            |elem| matches!(elem, TemplateElem::Property(p) if self.1.contains_key(p)),
        )
    }
}

impl<T: Ord + Eq> Display for Render<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0
            .iter()
            .enumerate()
            .try_for_each(|(i, elem)| match elem {
                TemplateElem::Literal(s) => write!(f, "{s}"),
                TemplateElem::Separator(s) => match self.separate(i) {
                    true => write!(f, "{s}"),
                    false => Ok(()),
                },
                TemplateElem::Property(p) => match self.1.get(p) {
                    Some(v) => write!(f, "{v}"),
                    None => Ok(()),
                },
            })
    }
}

impl FromStr for RelativeNameTemplate {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            terminated(parse_template, eof)(s)
                .finish()
                .map_err(|_| Error::ParseNameTemplate(s.to_string()))?
                .1,
        ))
    }
}

impl FromStr for AbsoluteNameTemplate {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            terminated(parse_template, eof)(s)
                .finish()
                .map_err(|_| Error::ParseNameTemplate(s.to_string()))?
                .1,
        ))
    }
}

fn parse_template<T: FromStr>(s: &str) -> IResult<&str, Vec<TemplateElem<T>>> {
    many0(alt((
        map(parse_literal, TemplateElem::Literal),
        map(parse_property, TemplateElem::Property),
        map(parse_separator, TemplateElem::Separator),
    )))(s)
}

fn parse_literal(s: &str) -> IResult<&str, String> {
    let (s, elems) = many1(alt((
        take_while1(|c| c != '{' && c != '}' && c != '[' && c != ']'),
        value("{", tag("{{")),
        value("}", tag("}}")),
        value("[", tag("[[")),
        value("]", tag("]]")),
    )))(s)?;
    Ok((s, elems.concat()))
}

fn parse_property<T: FromStr>(s: &str) -> IResult<&str, T> {
    map_res(
        delimited(
            tag("{"),
            many1(alt((take_while1(|c| c != '}'), backslash_escape))),
            tag("}"),
        ),
        |keys: Vec<&str>| keys.concat().parse(),
    )(s)
}

fn parse_separator(s: &str) -> IResult<&str, String> {
    map(
        delimited(
            tag("["),
            many1(alt((take_while1(|c| c != ']'), backslash_escape))),
            tag("]"),
        ),
        |seps| seps.concat(),
    )(s)
}

fn backslash_escape(s: &str) -> IResult<&str, &str> {
    preceded(char('\\'), recognize(anychar))(s)
}

impl Display for RelativeNameTemplate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().try_for_each(|elem| write!(f, "{elem}"))
    }
}

impl Display for AbsoluteNameTemplate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().try_for_each(|elem| write!(f, "{elem}"))
    }
}

impl<T: Display> Display for TemplateElem<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateElem::Literal(s) => write!(f, "{}", EscapeLiteral(s)),
            TemplateElem::Separator(s) => write!(f, "[{}]", EscapeSeparator(s)),
            TemplateElem::Property(p) => write!(f, "{{{}}}", p),
        }
    }
}

struct EscapeLiteral<'a>(&'a str);

impl Display for EscapeLiteral<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.chars().try_for_each(|c| match c {
            '{' => write!(f, "{{"),
            '}' => write!(f, "}}"),
            '[' => write!(f, "[["),
            ']' => write!(f, "]]"),
            _ => write!(f, "{c}"),
        })
    }
}

struct EscapeSeparator<'a>(&'a str);

impl Display for EscapeSeparator<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.chars().try_for_each(|c| match c {
            ']' => write!(f, "\\]"),
            _ => write!(f, "{c}"),
        })
    }
}
