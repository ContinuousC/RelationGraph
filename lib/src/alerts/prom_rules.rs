/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

// Subset of data returned from
// GET /api/prom/api/v1/alerts

use std::collections::BTreeMap;

use prometheus_core::LabelName;
use serde::{Deserialize, Serialize};

use super::{
    AlertConfigName, AlertInfo, AlertName, AlertRuleTemplateName, AlertState, RenderedAnnotations,
    Severity,
};

#[derive(Serialize, Deserialize, Debug)]
pub struct PromRules {
    groups: Vec<AlertGroupStatus>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct PromAlerts {
    alerts: Vec<AlertStatus>,
}

#[derive(Serialize, Deserialize, Debug)]
struct AlertGroupStatus {
    // name: String,
    // file: String,
    rules: Vec<AlertRuleStatus>,
}

#[derive(Serialize, Deserialize, Debug)]
struct AlertRuleStatus {
    // name: String,
    // state: String,
    // query: String,
    // duration: u64,
    // labels: BTreeMap<LabelName, String>,
    // annotations: Annotations,
    alerts: Vec<AlertStatus>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AlertStatus {
    pub labels: AlertLabels,
    pub annotations: RenderedAnnotations,
    pub state: AlertState,
    // pub active_at: DateTime<Utc>,
    // pub value: f64,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AlertLabels {
    pub alertname: Option<AlertName>,
    pub alertrule: Option<AlertRuleTemplateName>,
    pub alertconfig: Option<AlertConfigName>,
    pub severity: Option<ExternalSeverity>,
    #[serde(flatten)]
    pub other: BTreeMap<LabelName, String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum ExternalSeverity {
    Known(Severity),
    Unknown(String),
}

impl PromRules {
    pub fn firing_alerts(self) -> impl Iterator<Item = AlertInfo> {
        self.groups
            .into_iter()
            .flat_map(|group| group.rules)
            .flat_map(|rule| rule.alerts)
            .filter(|alert| matches!(alert.state, AlertState::Firing))
            .filter_map(|alert| alert.alert())
    }
}

impl PromAlerts {
    pub fn firing_alerts(self) -> impl Iterator<Item = AlertInfo> {
        self.alerts
            .into_iter()
            .filter(|alert| matches!(alert.state, AlertState::Firing))
            .filter_map(|alert| alert.alert())
    }
}

impl AlertStatus {
    pub fn alert(self) -> Option<AlertInfo> {
        Some(AlertInfo {
            alert_rule: self.labels.alertrule?,
            alert_name: self.labels.alertname?,
            alert_config: self.labels.alertconfig?,
            alert_state: self.state,
            severity: self.labels.severity?.known()?,
            labels: self.labels.other,
            annotations: self.annotations,
        })
    }
}

impl ExternalSeverity {
    fn known(self) -> Option<Severity> {
        match self {
            ExternalSeverity::Known(severity) => Some(severity),
            ExternalSeverity::Unknown(_) => None,
        }
    }
}
