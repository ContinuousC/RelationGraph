/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;

use itertools::{Itertools, MinMaxResult};
use serde::{Deserialize, Serialize};
use wrapper::Wrapper;

use crate::{
    error::Result,
    ids::{Absolute, PropertyId},
    items::resolved::{Item, Items, Relation},
    query::{
        filter::MatchResult,
        resolved::{ItemArgs, ItemSelector, MatchContext, RelArgs, RelationSelector},
    },
    types::resolved::Types,
    Endpoint, ExprName, InfoQueryResult, ItemId, RelationId,
};

use super::serial;

pub struct Stylesheet {
    items: Vec<ItemStyleRule>,
    relations: Vec<RelationStyleRule>,
}

pub struct ItemStyleRule {
    selector: ItemSelector,
    style: ItemStyleSetting,
}

pub struct RelationStyleRule {
    selector: RelationSelector,
    style: RelationStyleSetting,
}

#[derive(Default)]
pub struct ItemStyle<'a> {
    pub icon: Option<&'a ByTheme<String>>,
    pub label_color: Option<&'a ByTheme<GraphColor>>,
    pub label: Option<&'a Vec<Absolute<PropertyId>>>,
    pub metric_size: Option<&'a MetricStyle<GraphNodeSize>>,
    pub metric_badge: Option<&'a MetricStyle<GraphMetricColor>>,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ItemStyleSetting {
    icon: Option<ByTheme<String>>,
    label_color: Option<ByTheme<GraphColor>>,
    label: Option<Vec<Absolute<PropertyId>>>,
    metric_size: Option<MetricStyle<GraphNodeSize>>,
    metric_badge: Option<MetricStyle<GraphMetricColor>>,
}

//Add metric_label
#[derive(Default)]
pub struct RelationStyle<'a> {
    pub metric_size: Option<&'a MetricStyle<GraphEdgeSize>>,
    pub metric_color: Option<&'a MetricStyle<GraphMetricColor>>,
    pub metric_label: Option<&'a MetricStyle<GraphMetricColor>>,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct RelationStyleSetting {
    metric_size: Option<MetricStyle<GraphEdgeSize>>,
    metric_color: Option<MetricStyle<GraphMetricColor>>,
    metric_label: Option<MetricStyle<GraphMetricColor>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct GraphNodeSize(pub f64);

#[derive(Serialize, Deserialize, Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct GraphEdgeSize(pub f64);

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct GraphMetricColor(pub GraphColor);

#[derive(Serialize, Deserialize, Debug, Clone)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(
    feature = "schemars",
    schemars(bound = "T: schemars::JsonSchema, StyleRange<T>: Default + schemars::JsonSchema")
)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(bound(deserialize = "StyleRange<T>: Default + serde::de::Deserialize<'de>"))]
pub struct MetricStyle<T> {
    pub name: String,
    pub expr_name: ExprName,
    pub title: Option<String>,
    #[serde(default = "bool_false")]
    pub default_enabled: bool,
    #[serde(default)]
    pub metric_range: MetricRange,
    #[serde(default)]
    pub style_range: StyleRange<T>,
}

fn bool_false() -> bool {
    false
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct MetricRange {
    pub min: MetricBound,
    pub max: MetricBound,
    pub scale: MetricRangeScale,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(
    feature = "schemars",
    schemars(bound = "T: schemars::JsonSchema + Default")
)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct StyleRange<T> {
    pub range: Vec<T>,
    #[serde(default)]
    pub default: T,
    pub interpolate: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum MetricBound {
    #[default]
    Auto,
    Fixed(f64),
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum MetricRangeScale {
    #[default]
    Linear,
    Logarithmic {
        base: f64,
    },
}

pub trait Interpolate {
    fn interpolate(x: f64, left: &Self, right: &Self, scale: MetricRangeScale) -> Self;
}

impl Interpolate for f64 {
    fn interpolate(x: f64, left: &Self, right: &Self, scale: MetricRangeScale) -> Self {
        scale.interpolate(x, *left, *right)
    }
}

impl Interpolate for u8 {
    fn interpolate(x: f64, left: &Self, right: &Self, scale: MetricRangeScale) -> Self {
        scale.interpolate(x, *left as f64, *right as f64) as u8
    }
}

impl Interpolate for GraphColor {
    fn interpolate(x: f64, left: &Self, right: &Self, scale: MetricRangeScale) -> Self {
        let (lr, lg, lb) = left.rgb();
        let (rr, rg, rb) = right.rgb();
        GraphColor::Custom(
            scale.interpolate(x, lr as f64, rr as f64) as u8,
            scale.interpolate(x, lg as f64, rg as f64) as u8,
            scale.interpolate(x, lb as f64, rb as f64) as u8,
        )
    }
}

impl Interpolate for GraphMetricColor {
    fn interpolate(x: f64, left: &Self, right: &Self, scale: MetricRangeScale) -> Self {
        Self(GraphColor::interpolate(x, &left.0, &right.0, scale))
    }
}

impl Interpolate for GraphNodeSize {
    fn interpolate(x: f64, left: &Self, right: &Self, scale: MetricRangeScale) -> Self {
        Self(scale.interpolate(x, left.0, right.0))
    }
}

impl Interpolate for GraphEdgeSize {
    fn interpolate(x: f64, left: &Self, right: &Self, scale: MetricRangeScale) -> Self {
        Self(scale.interpolate(x, left.0, right.0))
    }
}

impl<T: Interpolate + Clone> MetricStyle<T> {
    pub fn get_item_style<'a>(
        &'a self,
        item_id: &ItemId,
        info: Option<&InfoQueryResult>,
        min_max_cache: &mut BTreeMap<&'a ExprName, MinMaxResult<f64>>,
    ) -> T {
        info.and_then(|info| {
            let value = info.get_item_metric(&self.expr_name, item_id)?;
            let (min, max) = self.metric_range.get_metric_range(
                &self.expr_name,
                info.item_metrics(&self.expr_name),
                min_max_cache,
            )?;
            Some(self.get_style(value, min, max))
        })
        .unwrap_or_else(|| self.style_range.default.clone())
    }

    pub fn get_relation_style<'a>(
        &'a self,
        rel_id: &RelationId,
        info: Option<&InfoQueryResult>,
        min_max_cache: &mut BTreeMap<&'a ExprName, MinMaxResult<f64>>,
    ) -> T {
        info.and_then(|info| {
            let value = info.get_relation_metric(&self.expr_name, rel_id)?;
            let (min, max) = self.metric_range.get_metric_range(
                &self.expr_name,
                info.relation_metrics(&self.expr_name),
                min_max_cache,
            )?;
            Some(self.get_style(value, min, max))
        })
        .unwrap_or_else(|| self.style_range.default.clone())
    }

    fn get_style(&self, value: f64, min: f64, max: f64) -> T {
        if value.is_nan()
            || self.style_range.range.is_empty()
            || min.is_nan()
            || max.is_nan()
            || min >= max
        {
            self.style_range.default.clone()
        } else {
            let last = self.style_range.range.len() - 1;
            let normalized = (value.clamp(min, max) - min) / (max - min);
            let index = self
                .metric_range
                .scale
                .interpolate(normalized, 0.0, last as f64);
            if self.style_range.interpolate {
                let left = (index.floor() as usize).clamp(0, last);
                let right = (index.ceil() as usize).clamp(0, last);
                if left == right {
                    self.style_range.range[left].clone()
                } else {
                    T::interpolate(
                        match self.metric_range.scale {
                            MetricRangeScale::Linear => index.fract(),
                            MetricRangeScale::Logarithmic { base } => {
                                let normalized_left =
                                    (base.powf(left as f64) - 1.0) / (base.powf(last as f64) - 1.0);
                                let normalized_right = (base.powf(right as f64) - 1.0)
                                    / (base.powf(last as f64) - 1.0);
                                (normalized - normalized_left)
                                    / (normalized_right - normalized_left)
                            }
                        },
                        &self.style_range.range[left],
                        &self.style_range.range[right],
                        self.metric_range.scale,
                    )
                }
            } else {
                let index = (index.round() as usize).clamp(0, last);
                self.style_range.range[index].clone()
            }
        }
    }
}

impl MetricRange {
    fn get_metric_range<'a>(
        &self,
        expr: &'a ExprName,
        metrics: impl Iterator<Item = f64>,
        min_max_cache: &mut BTreeMap<&'a ExprName, MinMaxResult<f64>>,
    ) -> Option<(f64, f64)> {
        let min_max = || {
            *min_max_cache
                .entry(expr)
                .or_insert_with(|| metrics.filter(|v| !v.is_nan()).minmax())
        };
        match (self.min, self.max) {
            (MetricBound::Fixed(min), MetricBound::Fixed(max)) => Some((min, max)),
            (MetricBound::Fixed(min), MetricBound::Auto) => match min_max() {
                MinMaxResult::MinMax(_, max) | MinMaxResult::OneElement(max) => Some((min, max)),
                MinMaxResult::NoElements => None,
            },
            (MetricBound::Auto, MetricBound::Fixed(max)) => match min_max() {
                MinMaxResult::MinMax(min, _) | MinMaxResult::OneElement(min) => Some((min, max)),
                MinMaxResult::NoElements => None,
            },
            (MetricBound::Auto, MetricBound::Auto) => match min_max() {
                MinMaxResult::MinMax(min, max) => Some((min, max)),
                MinMaxResult::OneElement(min) => Some((min, min)),
                MinMaxResult::NoElements => None,
            },
        }
    }
}

impl MetricRangeScale {
    /// 0.0 <= x <= 1.0
    fn interpolate(self, x: f64, left: f64, right: f64) -> f64 {
        match self {
            MetricRangeScale::Linear => x * (right - left) + left,
            MetricRangeScale::Logarithmic { base } => {
                (x * (base.powf(right) - base.powf(left)) + base.powf(left)).log(base)
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(untagged)]
pub enum ByTheme<T> {
    Fixed(T),
    Select { dark: T, light: T },
}

impl<T> ByTheme<T> {
    pub fn select(&self, dark_mode: bool) -> &T {
        match self {
            ByTheme::Fixed(v) => v,
            ByTheme::Select { dark, light } => match dark_mode {
                true => dark,
                false => light,
            },
        }
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "string"))]
#[serde(untagged)]
pub enum GraphColor {
    Predefined(PredefinedColor),
    #[cfg_attr(feature = "schemars", schemars(with = "String"))]
    #[serde(
        serialize_with = "serialize_custom_color",
        deserialize_with = "deserialize_custom_color"
    )]
    Custom(u8, u8, u8),
}

impl GraphColor {
    pub fn rgb(&self) -> (u8, u8, u8) {
        match self {
            Self::Predefined(c) => c.rgb(),
            Self::Custom(r, g, b) => (*r, *g, *b),
        }
    }
    pub fn critical() -> Self {
        Self::Custom(0xff, 0x00, 0x00)
    }
    pub fn major() -> Self {
        Self::Custom(0xff, 0xaa, 0x00)
    }
    pub fn warning() -> Self {
        Self::Custom(0xee, 0xee, 0x00)
    }
    pub fn minor() -> Self {
        Self::Custom(0xee, 0xaa, 0x88)
    }
    pub fn ok() -> Self {
        Self::Custom(0x00, 0xaa, 0x00)
    }
    pub fn primary() -> Self {
        Self::Custom(0x00, 0x6e, 0xff)
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "string"))]
#[serde(rename_all = "camelCase")]
pub enum PredefinedColor {
    White,
    Black,
    Green,
    Red,
    Gray,
    Yellow,
    Orange,
    Blue,
}

impl PredefinedColor {
    pub fn rgb(&self) -> (u8, u8, u8) {
        match self {
            PredefinedColor::White => (0xff, 0xff, 0xff),
            PredefinedColor::Black => (0x00, 0x00, 0x00),
            PredefinedColor::Green => (0x00, 0x80, 0x00),
            PredefinedColor::Red => (0xff, 0x00, 0x00),
            PredefinedColor::Gray => (0x80, 0x80, 0x80),
            PredefinedColor::Yellow => (0xff, 0xff, 0x00),
            PredefinedColor::Orange => (0xff, 0xa5, 0x00),
            PredefinedColor::Blue => (0x00, 0x00, 0xff),
        }
    }
}

fn serialize_custom_color<S>(
    r: &u8,
    g: &u8,
    b: &u8,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_str(&format!("#{r:02x}{g:02x}{b:02x}"))
}

fn deserialize_custom_color<'de, D>(deserializer: D) -> std::result::Result<(u8, u8, u8), D::Error>
where
    D: serde::Deserializer<'de>,
{
    let invalid = || <D::Error as serde::de::Error>::custom("invalid custom color");
    let s = String::deserialize(deserializer)?;
    let s = s.strip_prefix('#').ok_or_else(invalid)?;
    if s.len() == 6 {
        let r = u8::from_str_radix(&s[0..2], 16).map_err(|_| invalid())?;
        let g = u8::from_str_radix(&s[2..4], 16).map_err(|_| invalid())?;
        let b = u8::from_str_radix(&s[4..6], 16).map_err(|_| invalid())?;
        Ok((r, g, b))
    } else {
        Err(invalid())
    }
}

impl Stylesheet {
    pub(super) fn from_serial(serial: serial::Stylesheet, types: &Types) -> Result<Self> {
        Ok(Self {
            items: serial
                .items
                .into_iter()
                .map(|rule| ItemStyleRule::from_serial(rule, types))
                .collect::<Result<_>>()?,
            relations: serial
                .relations
                .into_iter()
                .map(|rule| RelationStyleRule::from_serial(rule, types))
                .collect::<Result<_>>()?,
        })
    }

    pub fn for_item<'a, W: Wrapper>(
        &'a self,
        item_id: &ItemId,
        item: &Item<W>,
        items: &Items<W>,
        types: &Types,
    ) -> ItemStyle<'a> {
        self.items
            .iter()
            .filter(|rule| {
                let m: Option<bool> = rule.selector.matches(
                    &ItemArgs::new(item_id, item),
                    &MatchContext::new(items, types),
                    &(),
                );
                m.matches()
            })
            .fold(ItemStyle::default(), |mut item_style, rule| {
                item_style.apply(&rule.style);
                item_style
            })
    }

    pub fn for_relation<W: Wrapper>(
        &self,
        relation_id: &RelationId,
        relation: &Relation<W>,
        items: &Items<W>,
        types: &Types,
    ) -> RelationStyle {
        self.relations
            .iter()
            .filter(|rule| {
                rule.selector
                    .matches::<_, Option<bool>, _>(
                        &RelArgs::new(relation_id, relation, Endpoint::Target),
                        &MatchContext::new(items, types),
                        &(),
                    )
                    .matches()
                    || rule
                        .selector
                        .matches::<_, Option<bool>, _>(
                            &RelArgs::new(relation_id, relation, Endpoint::Source),
                            &MatchContext::new(items, types),
                            &(),
                        )
                        .matches()
            })
            .fold(RelationStyle::default(), |mut relation_style, rule| {
                relation_style.apply(&rule.style);
                relation_style
            })
    }
}

impl ItemStyleRule {
    fn from_serial(serial: serial::ItemStyleRule, types: &Types) -> Result<Self> {
        Ok(Self {
            selector: ItemSelector::from_serial(serial.selector, types, &BTreeMap::new(), None)?,
            style: serial.style,
        })
    }
}

impl RelationStyleRule {
    fn from_serial(serial: serial::RelationStyleRule, types: &Types) -> Result<Self> {
        Ok(Self {
            selector: RelationSelector::from_serial(
                serial.selector,
                types,
                &BTreeMap::new(),
                None,
            )?,
            style: serial.style,
        })
    }
}

impl<'a> ItemStyle<'a> {
    fn apply(&mut self, setting: &'a ItemStyleSetting) {
        if let Some(icon) = &setting.icon {
            if self.icon.is_none() {
                self.icon = Some(icon);
            }
        }

        if let Some(color) = &setting.label_color {
            if self.label_color.is_none() {
                self.label_color = Some(color);
            }
        }

        if let Some(ids) = &setting.label {
            if self.label.is_none() {
                self.label = Some(ids);
            }
        }

        if let Some(metric_size) = &setting.metric_size {
            if self.metric_size.is_none() {
                self.metric_size = Some(metric_size);
            }
        }

        if let Some(metric_badge) = &setting.metric_badge {
            if self.metric_badge.is_none() {
                self.metric_badge = Some(metric_badge);
            }
        }
    }
}

impl<'a> RelationStyle<'a> {
    fn apply(&mut self, setting: &'a RelationStyleSetting) {
        if let Some(metric_size) = &setting.metric_size {
            if self.metric_size.is_none() {
                self.metric_size = Some(metric_size);
            }
        }
        if let Some(metric_color) = &setting.metric_color {
            if self.metric_color.is_none() {
                self.metric_color = Some(metric_color);
            }
        }
        if let Some(metric_label) = &setting.metric_label {
            if self.metric_label.is_none() {
                self.metric_label = Some(metric_label);
            }
        }
    }
}

impl Default for GraphColor {
    fn default() -> GraphColor {
        GraphColor::Custom(0, 0, 255)
    }
}

impl Default for GraphNodeSize {
    fn default() -> Self {
        Self(35.0)
    }
}

impl Default for GraphEdgeSize {
    fn default() -> GraphEdgeSize {
        GraphEdgeSize(2.0)
    }
}

impl Default for GraphMetricColor {
    fn default() -> Self {
        Self(GraphColor::Predefined(PredefinedColor::Gray))
    }
}

impl Default for StyleRange<GraphNodeSize> {
    fn default() -> Self {
        Self {
            range: Vec::from_iter([GraphNodeSize(35.0), GraphNodeSize(80.0)]),
            default: GraphNodeSize::default(),
            interpolate: true,
        }
    }
}

impl Default for StyleRange<GraphEdgeSize> {
    fn default() -> Self {
        Self {
            range: Vec::from_iter([GraphEdgeSize(1.0), GraphEdgeSize(5.0)]),
            default: GraphEdgeSize::default(),
            interpolate: true,
        }
    }
}

impl Default for StyleRange<GraphMetricColor> {
    fn default() -> Self {
        Self {
            range: Vec::from_iter([
                GraphMetricColor(GraphColor::Custom(131, 175, 240)),
                GraphMetricColor(GraphColor::Custom(0, 0, 255)),
            ]),
            default: GraphMetricColor::default(),
            interpolate: true,
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use serde_json::json;

    use crate::{GraphColor, GraphEdgeSize, MetricStyle};

    #[test]
    fn serialize_custom_graph_color() {
        let color = GraphColor::Custom(0x11, 0x22, 0x33);
        let serial = serde_json::to_string(&color).unwrap();
        assert_eq!(serial, "\"#112233\"")
    }

    #[test]
    fn deserialize_custom_graph_color() {
        let color = serde_json::from_str::<GraphColor>("\"#112233\"").unwrap();
        assert_eq!(color, GraphColor::Custom(0x11, 0x22, 0x33));
    }

    #[test]
    fn interpolate_metric_style_linear() {
        let s = serde_json::from_value::<MetricStyle<GraphEdgeSize>>(json!({
            "name": "test",
            "expr_name": "test",
            "metric_range": {
                "min": { "fixed": 0 },
                "max": { "fixed": 10 },
                "scale": "linear"
            }
        }))
        .unwrap();
        let values = Vec::from_iter([-5.0, 0.0, 5.0, 10.0, 15.0, f64::NAN]);

        let (min, max) = s
            .metric_range
            .get_metric_range(&s.expr_name, values.iter().copied(), &mut BTreeMap::new())
            .unwrap();

        assert_eq!((min, max), (0.0, 10.0));
        assert_eq!(
            values
                .iter()
                .map(|v| s.get_style(*v, min, max))
                .collect::<Vec<_>>(),
            Vec::from_iter([
                GraphEdgeSize(1.0),
                GraphEdgeSize(1.0),
                GraphEdgeSize(3.0),
                GraphEdgeSize(5.0),
                GraphEdgeSize(5.0),
                GraphEdgeSize(2.0)
            ])
        );
    }

    #[test]
    fn interpolate_metric_style_logarithmic() {
        let s = serde_json::from_value::<MetricStyle<GraphEdgeSize>>(json!({
            "name": "test",
            "expr_name": "test",
            "metric_range": {
                "min": { "fixed": 0 },
                "max": { "fixed": 100 },
                "scale": { "logarithmic": { "base": 10 } }
            }
        }))
        .unwrap();
        let values = Vec::from_iter([0.0, 4.0, 10.0, 50.0, 100.0, 120.0, f64::NAN]);

        let (min, max) = s
            .metric_range
            .get_metric_range(&s.expr_name, values.iter().copied(), &mut BTreeMap::new())
            .unwrap();

        assert_eq!((min, max), (0.0, 100.0));
        assert_eq!(
            values
                .iter()
                .map(|v| s.get_style(*v, min, max))
                .collect::<Vec<_>>(),
            Vec::from_iter([
                GraphEdgeSize(1.0),
                GraphEdgeSize(3.603101049314055),
                GraphEdgeSize(4.00039068924991),
                GraphEdgeSize(4.699013431612881),
                GraphEdgeSize(5.0),
                GraphEdgeSize(5.0),
                GraphEdgeSize(2.0)
            ])
        );
    }
}
