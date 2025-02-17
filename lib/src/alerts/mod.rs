/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

mod alert;
mod alert_map;
mod annotation;
mod prom_rules;
mod rule;
mod state;

pub use alert::{
    AlertDoc, AlertInfo, AlertMetric, AlertMetricName, AlertState, RenderedAnnotations, Severity,
};
pub use alert_map::{AlertMap, VersionedAlertMap};
pub use annotation::AnnotationTemplate;
pub use prom_rules::{AlertStatus, PromAlerts, PromRules};
pub(crate) use rule::AlertRuleError;
pub use rule::{
    AlertConfigName, AlertName, AlertRule, AlertRuleConfigs, AlertRuleForms, AlertRuleTemplateName,
    AlertRuleTemplates, AlertmanagerConfig, AlertmanagerConfigReceiver, AlertmanagerConfigRoute,
    AlertmanagerRequestBody, ParamKind, ReceiverName, RecordRule, RuleGroup,
    UnverifiedAlertRuleForms,
};
pub use state::{AlertRuleState, AlertRulesState, PutAlertRuleForm, PutAlertRuleFormResult};
