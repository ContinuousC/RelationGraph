/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::sync::Arc;
use std::{collections::BTreeMap, fmt::Display, str::FromStr};

use jaeger_anomaly_detection::{
    CombinationFactor, ImmediateInterval, ReferenceInterval, TraceMetric,
};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use unit::{Unit, NEUTRAL_UNIT};

use crate::ids::{Absolute, DashboardId, ItemTypeId};
use prometheus_api::GenericLabels;
use prometheus_expr::{ExprSpec, SelectItem};
use prometheus_schema::QualifiedItemName;

use crate::alerts::AlertRuleTemplateName;

#[derive(Serialize, Deserialize, Default, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Dashboards(
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: DashboardId]: OverviewDashboard }")
    )]
    BTreeMap<DashboardId, Arc<OverviewDashboard>>,
);

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Dashboard {
    pub type_range: Option<TypeRangeDashboard>,
    pub instance: Option<InstanceDashboard>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "camelCase")]
pub struct OverviewDashboard {
    name: String,
    panels: Vec<Vec<PanelOverview>>,
}

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct DashboardMetricId(String);

impl Display for DashboardMetricId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for DashboardMetricId {
    type Err = ParseDashboardMetricId;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut cs = s.chars();
        cs.next()
            .map(|c| match c {
                'A'..='Z' | 'a'..='z' | '_' | '-' => Ok(()),
                c => Err(ParseDashboardMetricId::InvalidFirstCharacter(c)),
            })
            .transpose()?;
        cs.try_for_each(|c| match c {
            'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '-' => Ok(()),
            c => Err(ParseDashboardMetricId::InvalidCharacter(c)),
        })?;
        Ok(Self(s.to_string()))
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParseDashboardMetricId {
    #[error("dashboard metric id cannot start with character '{0}'")]
    InvalidFirstCharacter(char),
    #[error("dashboard metric id cannot contain character '{0}'")]
    InvalidCharacter(char),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TypeRangeDashboard {
    panels: Vec<Panel<TypeRangeWidget>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct InstanceDashboard {
    panels: Vec<Panel<InstanceWidget>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "camelCase")]
#[serde(tag = "graph")]
pub enum InstanceWidget {
    Range(ItemRangeWidget),
    Instant(InstantWidget),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Panel<T> {
    name: String,
    documentation: Option<String>,
    widgets: Vec<Widget<T>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct PanelOverview {
    name: String,
    documentation: Option<String>,
    dimension: Option<u8>,
    widgets: Vec<Vec<Widget<WidgetOverview>>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(from_wasm_abi, into_wasm_abi, type = "WidgetInfo & T")
)]
pub struct Widget<T> {
    #[serde(flatten)]
    info: WidgetInfo,
    #[serde(flatten)]
    widget: T,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
struct WidgetInfo {
    name: String,
    documentation: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "camelCase")]
#[cfg_attr(
    feature = "tsify",
    tsify(
        from_wasm_abi,
        into_wasm_abi,
        type = "Widget<OverviewWidget> & {itemType: ItempTypeId}"
    )
)]
pub struct WidgetOverview {
    item_type: Absolute<ItemTypeId>,
    #[serde(flatten)]
    widget: OverviewWidget,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(tag = "widget_type", rename_all = "snake_case")]
pub enum InstantWidget {
    Number(NumberWidget),
    Meter(MeterWidget),
    Bar(BarWidget),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(tag = "widget_type", rename_all = "snake_case")]
pub enum TypeRangeWidget {
    Lines(LineGraph),
    AnomalyTraces(AnomalyTracesGraph),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(tag = "widget_type", rename_all = "snake_case")]
pub enum ItemRangeWidget {
    Lines(ItemLineGraph),
    AnomalyTraces(AnomalyTracesGraph),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(tag = "widget_type", rename_all = "snake_case")]
pub enum OverviewWidget {
    Status(StatusCard),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct DashboardMetric {
    pub name: String,
    pub documentation: Option<String>,
    pub item: QualifiedItemName,
    pub expr: ExprSpec,
    #[serde(default)]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: string }"))]
    pub thresholds: BTreeMap<AlertRuleTemplateName, ExprSpec>,
    #[serde(default = "neutral_unit")]
    pub unit: Unit,
    pub display_unit: Option<Unit>,
}

const fn neutral_unit() -> Unit {
    NEUTRAL_UNIT
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct NumberWidget {
    metric: DashboardMetricId,
}

/// Ant Design Chart: gauge
/// TODO: min and max should be an expression
/// for example get info from properties to know the maximum cpu limit
#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct MeterWidget {
    metric: DashboardMetricId,
    #[serde(default = "zero")]
    min: f64,
    max: f64,
}

/// Ant Design Chart: bullet
/// Not yet implement in frontend!!
/// TODO: min and max should be an expression
#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct BarWidget {
    metrics: Vec<DashboardMetricId>,
    #[serde(default = "zero")]
    min: f64,
    max: f64,
}

fn zero() -> f64 {
    0.0
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct LineGraph {
    metric: DashboardMetricId,
    select: SelectItem,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ItemLineGraph {
    metric: DashboardMetricId,
    #[serde(default = "default_true")]
    min_max: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "camelCase")]
pub struct StatusCard {}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct AnomalyTracesGraph {
    pub graph_type: AnomalyTracesGraphType,
    pub labels: Option<GenericLabels>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(tag = "graph", rename_all = "snake_case")]
pub enum AnomalyTracesGraphType {
    Metric(AnomalyMetricGraph),
    Score(AnomalyScoreGraph),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct AnomalyMetricGraph {
    pub metric: TraceMetric,
    #[serde(default = "default_immediate_interval")]
    pub immediate_interval: ImmediateInterval,
    pub reference_interval: Option<ReferenceInterval>,
    #[serde(default = "default_true")]
    pub confidence_interval: bool,
    #[serde(default = "default_top")]
    pub top: u64,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct AnomalyScoreGraph {
    pub metric: TraceMetric,
    #[serde(default = "default_immediate_interval")]
    pub immediate_interval: ImmediateInterval,
    #[serde(default = "default_reference_interval")]
    pub reference_interval: ReferenceInterval,
    /// A number between 0.0 and 1.0 that controls the effect of the total number of operation types
    /// on the aggregated anomaly score. An aggregation factor of zero means anomalies are summed,
    /// while an aggregation factor of one means the average is taken.
    #[serde(default)]
    pub aggr_factor: CombinationFactor,
    pub top: Option<u64>,
}

impl Dashboards {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn insert(&mut self, id: DashboardId, dashboard: OverviewDashboard) {
        self.0.insert(id, Arc::new(dashboard));
    }

    pub fn get(&self, id: &DashboardId) -> Option<&OverviewDashboard> {
        self.0.get(id).map(|dashboard| dashboard.as_ref())
    }
}

const fn default_immediate_interval() -> ImmediateInterval {
    ImmediateInterval::I15m
}

const fn default_reference_interval() -> ReferenceInterval {
    ReferenceInterval::R7d
}

// const fn default_quantile() -> f64 {
//     0.99
// }

const fn default_true() -> bool {
    true
}

const fn default_top() -> u64 {
    5
}
