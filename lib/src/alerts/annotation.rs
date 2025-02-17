/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, fmt::Display, str::FromStr};

use prometheus_expr::{ParamName, ParamType, ParamValue, ParamValues};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use dbschema::HasSchema;
use unit::{Dimension, Unit, NEUTRAL_UNIT};

use crate::alerts::{rule::ParamTypeSpec, ParamKind};

use super::rule::ParamSpec;

#[derive(Serialize, Deserialize, HasSchema, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct Annotations<T = String> {
    summary: Option<T>,
    description: Option<T>,
    runbook_url: Option<T>,
}

#[derive(SerializeDisplay, DeserializeFromStr, PartialEq, Eq, Default, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi))]
pub struct AnnotationTemplate(
    #[cfg_attr(feature = "schemars", schemars(with = "String"))]
    #[cfg_attr(feature = "tsify", tsify(type = "string"))]
    Vec<AnnotationTemplateElem>,
);

#[derive(PartialEq, Eq, Clone, Debug)]
enum AnnotationTemplateElem {
    Literal(String),
    Value(Option<Unit>),
    Param(ParamName, Option<Unit>),
}

impl AnnotationTemplate {
    #[cfg(test)]
    fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    fn literal<T: Into<String>>(mut self, s: T) -> Self {
        self.0.push(AnnotationTemplateElem::Literal(s.into()));
        self
    }

    #[cfg(test)]
    fn value(mut self) -> Self {
        self.0.push(AnnotationTemplateElem::Value(None));
        self
    }

    #[cfg(test)]
    fn value_unit(mut self, output: Unit) -> Self {
        self.0.push(AnnotationTemplateElem::Value(Some(output)));
        self
    }

    #[cfg(test)]
    fn param(mut self, name: ParamName) -> Self {
        self.0.push(AnnotationTemplateElem::Param(name, None));
        self
    }

    #[cfg(test)]
    fn param_unit(mut self, name: ParamName, output: Unit) -> Self {
        self.0
            .push(AnnotationTemplateElem::Param(name, Some(output)));
        self
    }

    fn parse(s: &str) -> Result<Self, AnnotationTemplateParseError> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_while1},
            character::complete::{char, space0},
            combinator::{cut, eof, map, map_res, opt},
            multi::fold_many0,
            sequence::{delimited, preceded, terminated, tuple},
            Finish,
        };

        fn some_param<
            'a,
            Elem,
            NameParserFactory,
            NameParser,
            Name,
            ArgsParser,
            Args,
            Error,
            ParamError,
        >(
            elem: Elem,
            // Note: this is a factory to allow using the parser twice. Reuse of some parsers
            // is possible through a shared reference (making them `Copy`), but not for parsers
            // built using a `nom` combinator function. These functions take parser arguments of
            // type `P: nom::Parser<_,_,_>`, which is implemented for `FnMut` parsers. Therefore,
            // they cannot return `impl Fn` parsers like most primitives, but return `impl FnMut`
            // parsers. Since `FnMut` can only be implemented for `&mut FnMut`, not for `&FnMut`,
            // these parsers cannot be shared, even though in their concrete type they probably
            // could.
            mut name: NameParserFactory,
            args: ArgsParser,
        ) -> impl nom::Parser<&'a str, AnnotationTemplateElem, Error>
        where
            Elem: Fn(Name, Option<Args>) -> Result<AnnotationTemplateElem, ParamError> + Copy,
            NameParserFactory: FnMut() -> NameParser,
            NameParser: nom::Parser<&'a str, Name, Error>,
            ArgsParser: nom::Parser<&'a str, Args, Error>,
            Error: nom::error::ParseError<&'a str>
                + nom::error::FromExternalError<&'a str, ParamError>,
        {
            alt((
                map_res(preceded(char('$'), name()), move |name| elem(name, None)),
                map_res(
                    delimited(
                        tuple((tag("${"), space0)),
                        tuple((
                            terminated(name(), space0),
                            opt(delimited(
                                tuple((char('('), space0)),
                                cut(args),
                                cut(tuple((space0, char(')'), space0))),
                            )),
                        )),
                        cut(char('}')),
                    ),
                    move |(name, args)| elem(name, args),
                ),
            ))
        }

        let param = alt((
            some_param(
                &|_, args: Option<&str>| {
                    std::result::Result::<_, unit::UnitError>::Ok(AnnotationTemplateElem::Value(
                        args.map(Unit::from_str).transpose()?,
                    ))
                },
                || tag::<_, _, nom::error::Error<&str>>("value"),
                take_while1(|c: char| c != ',' && c != ')'),
            ),
            some_param(
                &|name: &str, args: Option<&str>| {
                    std::result::Result::<_, unit::UnitError>::Ok(AnnotationTemplateElem::Param(
                        ParamName::from_str(name).unwrap(),
                        args.map(Unit::from_str).transpose()?,
                    ))
                },
                || {
                    preceded(
                        tag("params."),
                        cut(take_while1(|c: char| c.is_alphanumeric())),
                    )
                },
                take_while1(|c: char| c != ',' && c != ')'),
            ),
        ));

        enum Elem<'a> {
            String(&'a str),
            Char(char),
            Param(AnnotationTemplateElem),
        }

        let parser = fold_many0(
            alt((
                map(take_while1(|c| c != '$'), Elem::String),
                map(tag("$$"), |_| Elem::Char('$')),
                map(param, Elem::Param),
            )),
            || (Vec::new(), String::new()),
            |(mut elems, mut lit), elem| {
                match elem {
                    Elem::String(s) => lit.push_str(s),
                    Elem::Char(c) => lit.push(c),
                    Elem::Param(p) => {
                        if !lit.is_empty() {
                            let lit = std::mem::take(&mut lit);
                            elems.push(AnnotationTemplateElem::Literal(lit));
                        }
                        elems.push(p);
                    }
                }
                (elems, lit)
            },
        );

        match terminated(parser, eof)(s).finish() {
            Ok((_, (mut elems, lit))) => {
                if !lit.is_empty() {
                    elems.push(AnnotationTemplateElem::Literal(lit));
                }
                Ok(Self(elems))
            }
            Err(e) => Err(AnnotationTemplateParseError(e.to_string())),
        }
    }

    pub fn verify(
        &self,
        value: Dimension,
        params: &BTreeMap<ParamName, ParamType>,
    ) -> Result<(), AnnotationTemplateVerificationError> {
        self.0
            .iter()
            .try_for_each(|elem| elem.verify(value, params))
    }

    fn render(
        &self,
        value_unit: Unit,
        spec: &BTreeMap<ParamName, ParamSpec>,
        params: &ParamValues,
        thresholds: &ParamValues,
    ) -> Result<String, AnnotationTemplateRenderError> {
        use std::fmt::Write;
        self.0.iter().try_fold(String::new(), |mut s, elem| {
            match elem {
                AnnotationTemplateElem::Literal(lit) => write!(s, "{lit}").unwrap(),
                AnnotationTemplateElem::Value(output) => {
                    let base =
                        output.map_or_else(|| value_unit.normalize(), |unit| unit.normalize());
                    let factor = value_unit.convert(&base, 1.0).map_err(|e| {
                        AnnotationTemplateRenderError::ParamUnitConversion(
                            ParamName::from_str("$value").unwrap(),
                            e,
                        )
                    })?;
                    let humanize = match base {
                        Unit::Information(unit::InformationUnit::Byte(_))
                        | Unit::AvgOpSize(unit::InformationUnit::Byte(_), _) => " | humanize1024",
                        Unit::Time(_) | Unit::IOLatency(_, _) => " | humanizeDuration",
                        Unit::TimeSquare(_)
                        | Unit::Temperature(_)
                        | Unit::Area(_)
                        | Unit::Volume(_)
                        | Unit::FanSpeed(_)
                        | Unit::Dimensionless(_) => "",

                        _ => " | humanize",
                    };
                    write!(s, "{{{{ $value").unwrap();
                    if factor != 1.0 {
                        write!(s, " * {factor}").unwrap();
                    }
                    write!(s, "{humanize} }}}}{base}").unwrap();
                }
                AnnotationTemplateElem::Param(name, unit) => {
                    let spec = spec
                        .get(name)
                        .ok_or_else(|| AnnotationTemplateRenderError::MissingParam(name.clone()))?;
                    let value = match spec.kind {
                        ParamKind::Param => params,
                        ParamKind::Threshold => thresholds,
                    }
                    .0
                    .get(name)
                    .ok_or_else(|| AnnotationTemplateRenderError::MissingParam(name.clone()))?;
                    match &spec.r#type {
                        ParamTypeSpec::Int {} => {
                            let val = match value {
                                ParamValue::Int(v) => *v,
                                _ => {
                                    return Err(AnnotationTemplateRenderError::InvalidParamValue(
                                        name.clone(),
                                        value.clone(),
                                    ))
                                }
                            };
                            let val = match unit {
                                Some(unit) => {
                                    NEUTRAL_UNIT.convert(unit, val as f64).map_err(|e| {
                                        AnnotationTemplateRenderError::ParamUnitConversion(
                                            name.clone(),
                                            e,
                                        )
                                    })?
                                }
                                None => val as f64,
                            };
                            write!(s, "{val}").unwrap();
                        }
                        ParamTypeSpec::Quantity {
                            dimension,
                            decimals,
                            ..
                        } => {
                            let val = match value {
                                ParamValue::Int(v) => unit::Quantity::new(*v as f64, NEUTRAL_UNIT),
                                ParamValue::Float(v) => unit::Quantity::new(v.0, NEUTRAL_UNIT),
                                ParamValue::Quantity(v) => unit::Quantity::new(v.value.0, v.unit),
                                _ => {
                                    return Err(AnnotationTemplateRenderError::InvalidParamValue(
                                        name.clone(),
                                        value.clone(),
                                    ))
                                }
                            };
                            (val.dimension() == *dimension)
                                .then_some(())
                                .ok_or_else(|| {
                                    AnnotationTemplateRenderError::InvalidValueUnit(
                                        name.clone(),
                                        val.1,
                                    )
                                })?;
                            let val = if let Some(u) = unit {
                                val.convert(u)
                                    .map_err(|e| {
                                        AnnotationTemplateRenderError::ParamUnitConversion(
                                            name.clone(),
                                            e,
                                        )
                                    })?
                                    .autoscale()
                                    .map_err(|e| {
                                        AnnotationTemplateRenderError::ParamUnitConversion(
                                            name.clone(),
                                            e,
                                        )
                                    })?
                            } else {
                                val.autoscale().map_err(|e| {
                                    AnnotationTemplateRenderError::ParamUnitConversion(
                                        name.clone(),
                                        e,
                                    )
                                })?
                            };

                            write!(s, "{:.2$}{}", val.0, val.1, decimals.unwrap_or(2) as usize)
                                .unwrap();
                        }
                        ParamTypeSpec::PromDuration {} => {
                            let val = match value {
                                ParamValue::PromDuration(v) => *v,
                                _ => {
                                    return Err(AnnotationTemplateRenderError::InvalidParamValue(
                                        name.clone(),
                                        value.clone(),
                                    ))
                                }
                            };
                            write!(s, "{val}").unwrap();
                        }
                    }
                }
            };
            Ok(s)
        })
    }
}

impl AnnotationTemplateElem {
    pub fn verify(
        &self,
        value: Dimension,
        params: &BTreeMap<ParamName, ParamType>,
    ) -> Result<(), AnnotationTemplateVerificationError> {
        match self {
            AnnotationTemplateElem::Literal(_) => Ok(()),
            AnnotationTemplateElem::Value(unit) => match *unit {
                Some(unit) if value == unit.dimension() => {
                    Err(AnnotationTemplateVerificationError::InvalidValueUnit(unit))
                }
                _ => Ok(()),
            },
            AnnotationTemplateElem::Param(name, unit) => {
                let typ = params.get(name).ok_or_else(|| {
                    AnnotationTemplateVerificationError::MissingParam(name.clone())
                })?;
                match (*typ, *unit) {
                    (ParamType::Int, Some(unit))
                        if unit.dimension() != Dimension::Dimensionless =>
                    {
                        Err(AnnotationTemplateVerificationError::InvalidParamUnit(
                            name.clone(),
                            unit,
                        ))
                    }
                    (ParamType::Quantity(dimension), Some(unit))
                        if unit.dimension() != dimension =>
                    {
                        Err(AnnotationTemplateVerificationError::InvalidParamUnit(
                            name.clone(),
                            unit,
                        ))
                    }
                    _ => Ok(()),
                }
            }
        }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("failed to parse annotation template: {0}")]
pub struct AnnotationTemplateParseError(String);

#[derive(thiserror::Error, Debug)]
pub enum AnnotationTemplateVerificationError {
    #[error("unknown parameter: {0}")]
    MissingParam(ParamName),
    #[error("invalid unit for $value: {0}")]
    InvalidValueUnit(Unit),
    #[error("invalid unit for parameter: {0}: {1}")]
    InvalidParamUnit(ParamName, Unit),
}

#[derive(thiserror::Error, Debug)]
pub enum AnnotationTemplateRenderError {
    #[error("unknown parameter: {0}")]
    MissingParam(ParamName),
    #[error("invalid value for parameter: {0}: {1:?}")]
    InvalidParamValue(ParamName, ParamValue),
    #[error("invalid unit for parameter: {0}: {1}")]
    InvalidValueUnit(ParamName, Unit),
    #[error("unit conversion error: {0}: {1}")]
    ParamUnitConversion(ParamName, unit::UnitError),
}

impl Annotations<AnnotationTemplate> {
    pub(crate) fn render(
        &self,
        value_unit: Unit,
        spec: &BTreeMap<ParamName, ParamSpec>,
        params: &ParamValues,
        thresholds: &ParamValues,
    ) -> Result<Annotations<String>, AnnotationTemplateRenderError> {
        let resolve = |t: &AnnotationTemplate| t.render(value_unit, spec, params, thresholds);
        Ok(Annotations {
            summary: self.summary.as_ref().map(resolve).transpose()?,
            description: self.description.as_ref().map(resolve).transpose()?,
            runbook_url: self.runbook_url.as_ref().map(resolve).transpose()?,
        })
    }
}

impl FromStr for AnnotationTemplate {
    type Err = AnnotationTemplateParseError;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Self::parse(s)
    }
}

impl Display for AnnotationTemplate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().try_for_each(|elem| write!(f, "{elem}"))
    }
}

impl Display for AnnotationTemplateElem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnnotationTemplateElem::Literal(lit) => lit.chars().try_for_each(|c| match c {
                '$' => write!(f, "$$"),
                c => write!(f, "{c}"),
            }),
            AnnotationTemplateElem::Value(output) => match output {
                Some(output) => {
                    write!(f, "${{value({output})}}")
                }
                None => write!(f, "$value"),
            },
            AnnotationTemplateElem::Param(name, unit) => match unit {
                Some(unit) => write!(f, "${{params.{name}({unit})}}"),
                None => write!(f, "$params.{name}"),
            },
        }
    }
}

#[cfg(test)]
mod test {

    use std::collections::BTreeMap;

    use ordered_float::OrderedFloat;
    use prometheus_expr::{ParamValue, ParamValues, Quantity};
    use unit::{BinPrefix, Dimension, Unit, NEUTRAL_UNIT};

    use crate::alerts::{
        annotation::AnnotationTemplate,
        rule::{ParamSpec, ParamTypeSpec},
        ParamKind,
    };

    const KILOBYTE: Unit =
        unit::Unit::Information(unit::InformationUnit::Byte(unit::BinPrefix::Kilo));
    const MEGABIT: Unit =
        unit::Unit::Information(unit::InformationUnit::Bit(unit::DecPrefix::Mega));

    #[test]
    fn parse_empty_template() {
        assert_eq!(
            AnnotationTemplate::parse("").unwrap(),
            AnnotationTemplate::new()
        );
    }

    #[test]
    fn parse_param_name() {
        assert_eq!(
            AnnotationTemplate::parse("$params.myParamName").unwrap(),
            AnnotationTemplate::new().param("myParamName".parse().unwrap())
        );
    }

    #[test]
    fn parse_value() {
        assert_eq!(
            AnnotationTemplate::parse("$value > $params.min!").unwrap(),
            AnnotationTemplate::new()
                .value()
                .literal(" > ")
                .param("min".parse().unwrap())
                .literal("!")
        );
    }

    #[test]
    fn parse_escaped_dollar() {
        assert_eq!(
            AnnotationTemplate::parse("$$value > $params.min!").unwrap(),
            AnnotationTemplate::new()
                .literal("$value > ")
                .param("min".parse().unwrap())
                .literal("!")
        );
    }

    #[test]
    fn parse_unit() {
        assert_eq!(
            AnnotationTemplate::parse("${value(kB)} > ${params.min(kB)}!").unwrap(),
            AnnotationTemplate::new()
                .value_unit(KILOBYTE)
                .literal(" > ")
                .param_unit("min".parse().unwrap(), KILOBYTE)
                .literal("!")
        );
    }

    #[test]
    fn render_simple_template() {
        assert_eq!(
            &AnnotationTemplate::parse("$value > $params.min!")
                .unwrap()
                .render(
                    NEUTRAL_UNIT,
                    &BTreeMap::from_iter([(
                        "min".parse().unwrap(),
                        ParamSpec {
                            kind: ParamKind::Threshold,
                            r#type: ParamTypeSpec::Quantity {
                                dimension: Dimension::Dimensionless,
                                units: None,
                                decimals: None
                            }
                        }
                    )]),
                    &ParamValues(BTreeMap::new()),
                    &ParamValues(BTreeMap::from_iter([(
                        "min".parse().unwrap(),
                        ParamValue::Float(OrderedFloat(42.0))
                    )]))
                )
                .unwrap(),
            "{{ $value }} > 42.00!"
        );
    }

    #[test]
    fn render_template_with_units() {
        assert_eq!(
            &AnnotationTemplate::parse("${value(B)} * $params.factor > ${params.min(b)}!")
                .unwrap()
                .render(
                    MEGABIT,
                    &BTreeMap::from_iter([
                        (
                            "factor".parse().unwrap(),
                            ParamSpec {
                                kind: ParamKind::Param,
                                r#type: ParamTypeSpec::Quantity {
                                    dimension: Dimension::Dimensionless,
                                    units: None,
                                    decimals: None
                                }
                            }
                        ),
                        (
                            "min".parse().unwrap(),
                            ParamSpec {
                                kind: ParamKind::Threshold,
                                r#type: ParamTypeSpec::Quantity {
                                    dimension: Dimension::Information,
                                    units: None,
                                    decimals: None
                                }
                            }
                        )
                    ]),
                    &ParamValues(BTreeMap::from_iter([(
                        "factor".parse().unwrap(),
                        ParamValue::Float(OrderedFloat(8.0))
                    )])),
                    &ParamValues(BTreeMap::from_iter([(
                        "min".parse().unwrap(),
                        ParamValue::Quantity(Quantity {
                            value: OrderedFloat(2048.0),
                            unit: Unit::Information(unit::InformationUnit::Byte(BinPrefix::Mega))
                        })
                    )]))
                )
                .unwrap(),
            "{{ $value * 125000 | humanize1024 }}B * 8.00 > 17.18Gb!"
        );
    }
}
