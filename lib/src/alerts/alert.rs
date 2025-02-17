/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;

use dbschema::HasSchema;
use serde::{Deserialize, Serialize};

use prometheus_core::LabelName;

use super::{rule::AlertName, AlertConfigName, AlertRuleTemplateName};
use crate::EntityInfo;

#[derive(Serialize, Deserialize, HasSchema)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct AlertDoc {
    #[serde(flatten)]
    pub entity: Option<EntityInfo>,
    #[serde(flatten)]
    pub alert: AlertInfo,
}

#[derive(Serialize, Deserialize, HasSchema, Clone)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct AlertInfo {
    pub alert_rule: AlertRuleTemplateName,
    pub alert_name: AlertName,
    pub alert_config: AlertConfigName,
    pub alert_state: AlertState,
    pub severity: Severity,
    pub labels: BTreeMap<LabelName, String>,
    pub annotations: RenderedAnnotations,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct AlertMetric {
    #[serde(rename = "__name__")]
    pub metric: AlertMetricName,
    pub alertstate: AlertState,
    pub alertname: AlertName,
    pub alertrule: AlertRuleTemplateName,
    pub alertconfig: AlertConfigName,
    pub severity: Severity,
    #[serde(flatten)]
    pub labels: BTreeMap<LabelName, String>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum AlertMetricName {
    #[serde(rename = "ALERTS")]
    Alerts,
}

#[derive(
    Serialize, Deserialize, HasSchema, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Default, Debug,
)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct RenderedAnnotations {
    pub summary: Option<String>,
    pub description: Option<String>,
    pub runbook_url: Option<String>,
}

#[derive(Serialize, Deserialize, HasSchema, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(rename_all = "snake_case")]
#[dbschema(format = "tag_string")]
pub enum AlertState {
    Pending,
    Firing,
}

#[derive(
    Serialize, Deserialize, HasSchema, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
#[dbschema(format = "tag_string")]
pub enum Severity {
    Minor,
    Warning,
    Major,
    Critical,
}

impl Severity {
    pub fn severities() -> impl Iterator<Item = Self> {
        [
            Severity::Minor,
            Severity::Warning,
            Severity::Major,
            Severity::Critical,
        ]
        .into_iter()
    }
}

// impl HasSchema for AlertDoc {
//     pub fn dbschema() -> DbSchema {
//         StructSchema::new()
//             .field("entity", OptionSchema::new(EntityInfo::schema()))
//             .serde_flatten("entity")
//             .unwrap()
//             .field("alert_rule", StringSchema::new())
//             .field("alert_name", StringSchema::new())
//             .field("alert_config", StringSchema::new())
//             .field("alert_state", StringSchema::new())
//             .field("severity", StringSchema::new())
//             .field("labels", DictionarySchema::new(StringSchema::new()))
//             .field(
//                 "annotations",
//                 StructSchema::new()
//                     .field("summary", OptionSchema::new(StringSchema::new()))
//                     .field("description", OptionSchema::new(StringSchema::new()))
//                     .field("runbook_url", OptionSchema::new(StringSchema::new())),
//             )
//             .into()
//     }
// }
