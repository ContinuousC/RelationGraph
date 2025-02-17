/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt::{Debug, Display},
    str::FromStr,
    sync::LazyLock,
};

use ambassador::Delegate;
use dbschema::{
    DbSchema, DictionarySchema, EnumSchema, HasSchema, IntegerSchema, JsonSchema, StringSchema,
    StructSchema,
};
use itertools::Itertools;
use ordered_float::{NotNan, OrderedFloat};
use serde::{de::DeserializeOwned, Deserialize, Serialize};

use jaeger_anomaly_detection::{
    CombineScores, ImmediateInterval, OperationFilter, ReferenceInterval, ServiceFilter, TraceAggr,
    TraceExpr, TraceMetric, TraceObject,
};
use prometheus_api::InstantQueryParams;
use prometheus_core::{LabelName, MetricName};
use prometheus_expr::{
    Expr, ExprSpec, ParamName, ParamType, ParamValue, ParamValues, PromDuration,
};
use prometheus_schema::{MetricSelector, QualifiedItemName};
use unit::{Dimension, Unit, NEUTRAL_UNIT};

use super::{
    annotation::{AnnotationTemplate, AnnotationTemplateRenderError, Annotations},
    Severity,
};
use crate::delegatable::ambassador_impl_Iterator;
use crate::{Error, Result};

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct UnverifiedAlertRuleForms(AlertRuleForms);

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum AlertRuleForms {
    Custom(AlertRuleForm<CustomAlertRuleForm>),
    FixedTraces(AlertRuleForm<FixedTracesAlertRuleForm>),
    DynamicTraces(AlertRuleForm<DynamicTracesAlertRuleForm>),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum AlertRuleTemplates {
    Custom(AlertRuleTemplate<CustomAlertRuleForm>),
    FixedTraces(AlertRuleTemplate<FixedTracesAlertRuleForm>),
    DynamicTraces(AlertRuleTemplate<DynamicTracesAlertRuleForm>),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum AlertRuleConfigs {
    Custom(AlertRuleConfig<CustomAlertRuleForm>),
    FixedTraces(AlertRuleConfig<FixedTracesAlertRuleForm>),
    DynamicTraces(AlertRuleConfig<DynamicTracesAlertRuleForm>),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct AlertRuleForm<T: AlertRuleFormSpecific> {
    pub template: AlertRuleTemplate<T>,
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: AlertConfigName]: AlertRuleConfig<T> }")
    )]
    pub configs: BTreeMap<AlertConfigName, AlertRuleConfig<T>>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct AlertRuleTemplate<T: AlertRuleFormSpecific + ?Sized> {
    alert: AlertRuleTemplateName,
    annotations: Annotations<AnnotationTemplate>,
    #[cfg_attr(feature = "tsify", tsify(type = "T['spec']"))]
    spec: T::Spec,
    default: AlertRuleConfig<T>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct AlertRuleConfig<T: AlertRuleFormSpecific + ?Sized> {
    r#for: PromDuration,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: LabelName]: string }"))]
    labels: BTreeMap<LabelName, String>,
    selectors: prometheus_schema::MetricSelector,
    #[cfg_attr(feature = "tsify", tsify(type = "T['config']", type_params = "T"))]
    params: T::Config,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(type = "{ spec: CustomAlertRuleSpec, config: CustomAlertRuleConfig }")
)]
pub struct CustomAlertRuleForm;

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct CustomAlertRuleSpec {
    item: QualifiedItemName,
    expr: ExprSpec,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: ParamName]: ParamSpec }"))]
    params: BTreeMap<ParamName, ParamSpec>,
    #[serde(default = "neutral_unit")]
    value_unit: Unit,
}

const fn neutral_unit() -> Unit {
    NEUTRAL_UNIT
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ParamSpec {
    pub kind: ParamKind,
    pub r#type: ParamTypeSpec,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Copy, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum ParamKind {
    Param,
    Threshold,
}

impl Display for ParamKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParamKind::Param => write!(f, "param"),
            ParamKind::Threshold => write!(f, "threshold"),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum ParamTypeSpec {
    Int {},
    Quantity {
        dimension: Dimension,
        units: Option<Vec<Unit>>,
        decimals: Option<u8>,
    },
    PromDuration {},
}

impl ParamTypeSpec {
    fn param_type(&self) -> ParamType {
        match self {
            ParamTypeSpec::Int {} => ParamType::Int,
            ParamTypeSpec::Quantity { dimension, .. } => ParamType::Quantity(*dimension),
            ParamTypeSpec::PromDuration {} => ParamType::PromDuration,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct CustomAlertRuleConfig {
    instance: InstanceAction,
    params: ParamValues,
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key in Severity]?:  ParamValues }")
    )]
    thresholds: BTreeMap<Severity, ParamValues>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum InstanceAction {
    Individual,
    Worst,
    Best,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(type = "{ spec: FixedTraceAlertRuleSpec, config: FixedTraceAlertRuleConfig }")
)]
pub struct FixedTracesAlertRuleForm;

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct FixedTraceAlertRuleSpec {
    item: TraceItem,
    metric: TraceMetric,
    param: TraceParameter,
    interval: ImmediateInterval,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct FixedTraceAlertRuleConfig {
    #[cfg_attr(feature = "schemars", schemars(with = "String"))]
    #[cfg_attr(feature = "tsify", tsify(type = "number"))]
    quantile: NotNan<f64>,
    #[cfg_attr(feature = "schemars", schemars(with = "BTreeMap<Severity, String>"))]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key in Severity]?: number }"))]
    thresholds: BTreeMap<Severity, NotNan<f64>>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(type = "{ spec: DynamicTraceAlertRuleSpec, config: DynamicTraceAlertRuleConfig }")
)]
pub struct DynamicTracesAlertRuleForm;

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct DynamicTraceAlertRuleSpec {
    item: TraceItem,
    metric: TraceMetric,
    // short_term_param: TraceParameter,
    short_term_interval: ImmediateInterval,
    // long_term_param: TraceParameter,
    long_term_interval: ReferenceInterval,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct DynamicTraceAlertRuleConfig {
    #[cfg_attr(feature = "schemars", schemars(with = "f64"))]
    #[cfg_attr(feature = "tsify", tsify(type = "number"))]
    offset: NotNan<f64>,
    #[cfg_attr(feature = "schemars", schemars(with = "BTreeMap<Severity, f64>"))]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key in Severity]?: number }"))]
    thresholds: BTreeMap<Severity, NotNan<f64>>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum TraceItem {
    Operation,
    Service(CombineScores),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum TraceParameter {
    Mean,
    LowerBound,
    HigherBound,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AlertmanagerRequestBody {
    pub alertmanager_config: String,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub template_files: BTreeMap<String, String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AlertmanagerConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub global: Option<AlertmanagerConfigGlobal>,
    pub route: AlertmanagerConfigRoute,
    pub receivers: Vec<AlertmanagerConfigReceiver>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub templates: Option<Vec<String>>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AlertmanagerConfigGlobal {}

#[derive(Serialize, Deserialize, Debug)]
pub struct AlertmanagerConfigRoute {
    pub receiver: ReceiverName,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AlertmanagerConfigReceiver {
    pub name: ReceiverName,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ReceiverName(String);

#[derive(Serialize, Deserialize, PartialEq, Eq)]
pub struct RuleGroup<T> {
    pub name: String,
    pub rules: Vec<T>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq)]
pub struct AlertRule {
    alert: AlertName,
    annotations: Annotations,
    expr: Expr,
    r#for: Option<PromDuration>,
    labels: BTreeMap<LabelName, String>,
}

#[derive(Serialize, Deserialize)]
pub struct RecordRule {
    pub record: MetricName,
    pub expr: Expr,
    pub labels: BTreeMap<LabelName, String>,
}

#[derive(Serialize, Deserialize, HasSchema, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct AlertRuleTemplateName(String);

#[derive(Serialize, Deserialize, HasSchema, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct AlertConfigName(pub(crate) String);

#[derive(Serialize, Deserialize, HasSchema, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct AlertName(String);

#[derive(Delegate)]
#[delegate(Iterator)]
enum GenericAlertEnum<A, B, C> {
    Custom(A),
    FixedTraces(B),
    DynamicTraces(C),
}

pub struct PromAlertRuleArgs<'a> {
    severity: Severity,
    value_unit: Unit,
    params_spec: &'a BTreeMap<ParamName, ParamSpec>,
    params: Cow<'a, ParamValues>,
    thresholds: Cow<'a, ParamValues>,
    expr: Expr,
}

pub trait AlertRuleFormSpecific {
    #[cfg(feature = "schemars")]
    type Spec: Serialize
        + DeserializeOwned
        + schemars::JsonSchema
        + PartialEq
        + Eq
        + Clone
        + Debug
        + AlertRuleSpecSpecific;
    #[cfg(feature = "schemars")]
    type Config: Serialize
        + DeserializeOwned
        + schemars::JsonSchema
        + PartialEq
        + Eq
        + Clone
        + Debug;

    #[cfg(not(feature = "schemars"))]
    type Spec: Serialize + DeserializeOwned + PartialEq + Eq + Clone + Debug + AlertRuleSpecSpecific;
    #[cfg(not(feature = "schemars"))]
    type Config: Serialize + DeserializeOwned + PartialEq + Eq + Clone + Debug;

    fn exprs<'a>(
        schema: &prometheus_schema::Universe,
        template: &'a AlertRuleTemplate<Self>,
        config: &'a AlertRuleConfig<Self>,
    ) -> Result<Vec<PromAlertRuleArgs<'a>>>;
}

pub trait AlertRuleSpecSpecific {
    fn item(&self) -> QualifiedItemName;
}

impl HasSchema for AlertRuleTemplates {
    fn schema() -> DbSchema {
        let common_template = StructSchema::new()
            .field("alert", StringSchema::new())
            .field("annotations", Annotations::<String>::schema());
        let common_default_config = StructSchema::new()
            .field("for", StringSchema::new())
            .field("labels", DictionarySchema::new(StringSchema::new()))
            .field("selectors", JsonSchema::new());
        EnumSchema::new()
            .option(
                "custom",
                common_template
                    .clone()
                    .field(
                        "spec",
                        StructSchema::new()
                            .field("item", StringSchema::new())
                            .field("expr", StringSchema::new())
                            .field("value_unit", StringSchema::new())
                            .field("params", DictionarySchema::new(JsonSchema::new())),
                    )
                    .field(
                        "default",
                        common_default_config.clone().field(
                            "params",
                            StructSchema::new()
                                .field("instance", StringSchema::new())
                                .field("params", JsonSchema::new())
                                .field("thresholds", JsonSchema::new()),
                        ),
                    ),
            )
            .option(
                "fixed_traces",
                common_template
                    .clone()
                    .field(
                        "spec",
                        StructSchema::new()
                            .field("item", JsonSchema::new())
                            .field("metric", StringSchema::new())
                            .field("param", JsonSchema::new())
                            .field("interval", StringSchema::new()),
                    )
                    .field(
                        "default",
                        common_default_config.clone().field(
                            "params",
                            StructSchema::new()
                                .field("quantile", IntegerSchema::new())
                                .field("thresholds", JsonSchema::new()),
                        ),
                    ),
            )
            .option(
                "dynamic_traces",
                common_template
                    .field(
                        "spec",
                        StructSchema::new()
                            .field("item", JsonSchema::new())
                            .field("metric", StringSchema::new())
                            .field("short_term_param", JsonSchema::new())
                            .field("short_term_interval", StringSchema::new())
                            .field("long_term_param", JsonSchema::new())
                            .field("long_term_interval", StringSchema::new()),
                    )
                    .field(
                        "default",
                        common_default_config.field(
                            "params",
                            StructSchema::new()
                                .field("offset", IntegerSchema::new())
                                .field("thresholds", JsonSchema::new()),
                        ),
                    ),
            )
            .into()
    }
}

impl HasSchema for AlertRuleConfigs {
    fn schema() -> DbSchema {
        let common_default_config = StructSchema::new()
            .field("for", StringSchema::new())
            .field("labels", DictionarySchema::new(StringSchema::new()))
            .field("selectors", JsonSchema::new());
        EnumSchema::new()
            .option(
                "custom",
                common_default_config.clone().field(
                    "params",
                    StructSchema::new()
                        .field("instance", StringSchema::new())
                        .field("params", JsonSchema::new())
                        .field("thresholds", JsonSchema::new()),
                ),
            )
            .option(
                "fixed_traces",
                common_default_config.clone().field(
                    "params",
                    StructSchema::new()
                        .field("quantile", IntegerSchema::new())
                        .field("thresholds", JsonSchema::new()),
                ),
            )
            .option(
                "dynamic_traces",
                common_default_config.field(
                    "params",
                    StructSchema::new()
                        .field("offset", IntegerSchema::new())
                        .field("thresholds", JsonSchema::new()),
                ),
            )
            .into()
    }
}

impl UnverifiedAlertRuleForms {
    pub fn new(
        template: AlertRuleTemplates,
        configs: BTreeMap<AlertConfigName, AlertRuleConfigs>,
    ) -> Option<Self> {
        match template {
            AlertRuleTemplates::Custom(template) => {
                let configs = configs
                    .into_iter()
                    .filter_map(|(config_name, config)| match config {
                        AlertRuleConfigs::Custom(config) => Some((config_name, config)),
                        _ => {
                            log::warn!("Mismatch between alert type for template and config");
                            None
                        }
                    })
                    .collect();
                Some(Self(AlertRuleForms::Custom(AlertRuleForm {
                    template,
                    configs,
                })))
            }
            AlertRuleTemplates::FixedTraces(template) => {
                let configs = configs
                    .into_iter()
                    .filter_map(|(config_name, config)| match config {
                        AlertRuleConfigs::FixedTraces(config) => Some((config_name, config)),
                        _ => {
                            log::warn!("Mismatch between alert type for template and config");
                            None
                        }
                    })
                    .collect();
                Some(Self(AlertRuleForms::FixedTraces(AlertRuleForm {
                    template,
                    configs,
                })))
            }
            AlertRuleTemplates::DynamicTraces(template) => {
                let configs = configs
                    .into_iter()
                    .filter_map(|(config_name, config)| match config {
                        AlertRuleConfigs::DynamicTraces(config) => Some((config_name, config)),
                        _ => {
                            log::warn!("Mismatch between alert type for template and config");
                            None
                        }
                    })
                    .collect();
                Some(Self(AlertRuleForms::DynamicTraces(AlertRuleForm {
                    template,
                    configs,
                })))
            }
        }
    }

    pub fn load(
        templates: BTreeMap<AlertRuleTemplateName, AlertRuleTemplates>,
        template_configs: BTreeMap<
            AlertRuleTemplateName,
            BTreeMap<AlertConfigName, AlertRuleConfigs>,
        >,
    ) -> BTreeMap<AlertRuleTemplateName, Self> {
        templates
            .into_iter()
            .filter_map(|(template_name, template)| {
                let configs = template_configs
                    .get(&template_name)
                    .unwrap_or(&BTreeMap::<AlertConfigName, AlertRuleConfigs>::new())
                    .to_owned();
                let unverified_rule_form = Self::new(template, configs)?;
                Some((template_name, unverified_rule_form))
            })
            .collect()
    }

    pub fn verify(
        self,
        name: &AlertRuleTemplateName,
        strict_config_check: bool,
    ) -> Result<AlertRuleForms> {
        AlertRuleForms::verify(name, self.0, strict_config_check)
    }
}

impl AlertRuleForms {
    pub fn verify(
        name: &AlertRuleTemplateName,
        rule_form: Self,
        strict_config_check: bool,
    ) -> Result<Self> {
        if rule_form.get_name() != name {
            return Err(Error::MismatchAlertName(name.clone()));
        }
        match rule_form {
            Self::Custom(rule_form) => {
                let template = rule_form.template.verify()?;
                let configs = rule_form
                    .configs
                    .into_iter()
                    .filter_map(|(config_name, config)| match config.verify(&template) {
                        Ok(config_verified) => Some(Ok((config_name, config_verified))),
                        Err(error) => {
                            if strict_config_check {
                                Some(Err(error))
                            } else {
                                None
                            }
                        }
                    })
                    .collect::<Result<_>>()?;
                Ok(Self::Custom(AlertRuleForm { template, configs }))
            }
            _ => Ok(rule_form),
        }
    }

    pub fn rules(&self, schema: &prometheus_schema::Universe) -> Result<Vec<AlertRule>> {
        match self {
            Self::Custom(rule_form) => rule_form.rules(schema),
            Self::FixedTraces(rule_form) => rule_form.rules(schema),
            Self::DynamicTraces(rule_form) => rule_form.rules(schema),
        }
    }

    pub fn item(&self) -> QualifiedItemName {
        match self {
            Self::Custom(rule_form) => rule_form.item(),
            Self::FixedTraces(rule_form) => rule_form.item(),
            Self::DynamicTraces(rule_form) => rule_form.item(),
        }
    }

    pub fn get_template(&self) -> AlertRuleTemplates {
        match self {
            Self::Custom(rule_form) => AlertRuleTemplates::Custom(rule_form.template.clone()),
            Self::FixedTraces(rule_form) => {
                AlertRuleTemplates::FixedTraces(rule_form.template.clone())
            }
            Self::DynamicTraces(rule_form) => {
                AlertRuleTemplates::DynamicTraces(rule_form.template.clone())
            }
        }
    }

    pub fn get_name(&self) -> &AlertRuleTemplateName {
        match self {
            Self::Custom(rule_form) => &rule_form.template.alert,
            Self::FixedTraces(rule_form) => &rule_form.template.alert,
            Self::DynamicTraces(rule_form) => &rule_form.template.alert,
        }
    }

    pub fn iter_config_names(&self) -> impl Iterator<Item = &AlertConfigName> + '_ {
        match self {
            AlertRuleForms::Custom(rule) => GenericAlertEnum::Custom(rule.configs.keys()),
            AlertRuleForms::FixedTraces(rule) => GenericAlertEnum::FixedTraces(rule.configs.keys()),
            AlertRuleForms::DynamicTraces(rule) => {
                GenericAlertEnum::DynamicTraces(rule.configs.keys())
            }
        }
    }

    pub fn get_configs(&self) -> BTreeMap<AlertConfigName, AlertRuleConfigs> {
        match self {
            Self::Custom(rule_form) => rule_form
                .configs
                .clone()
                .into_iter()
                .map(|(name, config)| (name, AlertRuleConfigs::Custom(config)))
                .collect(),
            Self::FixedTraces(rule_form) => rule_form
                .configs
                .clone()
                .into_iter()
                .map(|(name, config)| (name, AlertRuleConfigs::FixedTraces(config)))
                .collect(),
            Self::DynamicTraces(rule_form) => rule_form
                .configs
                .clone()
                .into_iter()
                .map(|(name, config)| (name, AlertRuleConfigs::DynamicTraces(config)))
                .collect(),
        }
    }

    pub fn get_param_values(
        &self,
        severity: Severity,
        labels: &BTreeMap<LabelName, String>,
    ) -> Option<ParamValues> {
        match self {
            Self::Custom(rule_form) => {
                let config = rule_form
                    .configs
                    .values()
                    .find(|config| config.matches(labels));
                Some(match config {
                    Some(config) => config.params.params_for(&rule_form.template.spec, severity),
                    None => rule_form.template.default_params_for(severity),
                })
            }
            _ => None,
        }
    }
}

impl AlertRuleTemplates {
    pub fn get_name(&self) -> &AlertRuleTemplateName {
        match self {
            Self::Custom(template) => &template.alert,
            Self::FixedTraces(template) => &template.alert,
            Self::DynamicTraces(template) => &template.alert,
        }
    }
}

impl<T: AlertRuleFormSpecific> AlertRuleForm<T> {
    pub fn rules(&self, schema: &prometheus_schema::Universe) -> Result<Vec<AlertRule>> {
        [(
            &AlertConfigName::new("default".to_string()),
            &self.template.default,
        )]
        .into_iter()
        .chain(&self.configs)
        .flat_map(|(name, config)| {
            std::iter::once(T::exprs(schema, &self.template, config))
                .flatten_ok()
                .map(|args| {
                    self.get_alert_rule(name, config, args?)
                        .map_err(Error::GenerateAlertRule)
                })
        })
        .collect()
    }

    pub fn item(&self) -> QualifiedItemName {
        self.template.spec.item()
    }

    fn get_alert_rule(
        &self,
        name: &AlertConfigName,
        config: &AlertRuleConfig<T>,
        args: PromAlertRuleArgs<'_>,
    ) -> std::result::Result<AlertRule, AlertRuleError> {
        Ok(AlertRule {
            alert: AlertName(format!(
                "{}-{}-{}",
                self.template.alert, name, args.severity
            )),
            annotations: self
                .template
                .annotations
                .render(
                    args.value_unit,
                    args.params_spec,
                    &args.params,
                    &args.thresholds,
                )
                .map_err(AlertRuleError::Template)?,
            expr: args.expr,
            // Note: mutually exclusive severities do not
            // play well with the "for" setting, se we
            // need to filter the alerts after they are
            // generated instead...
            // config
            //     .thresholds
            //.range((Bound::Excluded(severity), Bound::Unbounded))
            // .try_fold(
            //     expr_combine_instances(
            //         instances, config, &self.expr, &self.item, item, thresholds,
            //     )?,
            //     |expr, (_, thresholds)| {
            //         Ok(PromExpr::Binary(
            //             BinOp::Unless,
            //             None,
            //             Box::new(expr),
            //             Box::new(expr_combine_instances(
            //                 instances, config, &self.expr, &self.item, item,
            //                 thresholds,
            //             )?),
            //         ))
            //     },
            // )?,
            r#for: Some(config.r#for),
            labels: config
                .labels
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .chain([
                    (
                        LabelName::new_static("alertrule"),
                        self.template.alert.to_string(),
                    ),
                    (LabelName::new_static("alertconfig"), name.to_string()),
                    (LabelName::new_static("severity"), args.severity.to_string()),
                    (
                        LabelName::new_static("prometheus_item"),
                        self.item().to_string(),
                    ),
                ])
                .collect(),
        })
    }
}

#[derive(thiserror::Error, Debug)]
pub enum AlertRuleError {
    #[error("failed to render template: {0}")]
    Template(AnnotationTemplateRenderError),
}

//Custom

impl AlertRuleFormSpecific for CustomAlertRuleForm {
    type Spec = CustomAlertRuleSpec;
    type Config = CustomAlertRuleConfig;

    fn exprs<'a>(
        schema: &prometheus_schema::Universe,
        template: &'a AlertRuleTemplate<Self>,
        config: &'a AlertRuleConfig<Self>,
    ) -> Result<Vec<PromAlertRuleArgs<'a>>> {
        let item = schema
            .lookup_item(&template.spec.item)
            .ok_or_else(|| Error::MissingPromItem(template.spec.item.clone()))?;
        let instances = item.svc_queries(&template.spec.item, schema);
        let instances = &instances;
        config
            .params
            .thresholds
            .iter()
            .map(move |(severity, thresholds)| {
                let expr = CustomAlertRuleForm::expr_combine_instances(
                    template, config, thresholds, instances, item,
                )?;
                Ok(PromAlertRuleArgs {
                    severity: *severity,
                    value_unit: template.spec.value_unit,
                    params_spec: &template.spec.params,
                    params: Cow::Borrowed(&config.params.params),
                    thresholds: Cow::Borrowed(thresholds),
                    expr,
                })
            })
            .collect()
    }
}

impl AlertRuleSpecSpecific for CustomAlertRuleSpec {
    fn item(&self) -> QualifiedItemName {
        self.item.clone()
    }
}

impl AlertRuleTemplate<CustomAlertRuleForm> {
    fn default_params_for(&self, severity: Severity) -> ParamValues {
        ParamValues(
            self.default
                .params
                .params
                .0
                .iter()
                .chain(
                    self.default
                        .params
                        .thresholds
                        .get(&severity)
                        .into_iter()
                        .flat_map(|vs| vs.0.iter()),
                )
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        )
    }
}

impl CustomAlertRuleConfig {
    fn params_for(&self, _template: &CustomAlertRuleSpec, severity: Severity) -> ParamValues {
        ParamValues(
            self.params
                .0
                .iter()
                .chain(
                    self.thresholds
                        .get(&severity)
                        .into_iter()
                        .flat_map(|vs| vs.0.iter()),
                )
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        )
    }
}

impl AlertRuleTemplate<CustomAlertRuleForm> {
    pub fn verify(self) -> Result<Self> {
        self.spec
            .expr
            .get_param_types()
            .map_err(Error::ParamType)?
            .into_iter()
            .try_for_each(|(expr_param_name, expr_param_type)| {
                let param_spec = self
                    .spec
                    .params
                    .get(expr_param_name)
                    .ok_or_else(|| Error::MissingParamSpec(expr_param_name.clone()))?;
                let param_type = param_spec.r#type.param_type();
                (expr_param_type == param_type)
                    .then_some(())
                    .ok_or_else(|| {
                        Error::ParamTypeMismatch(
                            expr_param_name.clone(),
                            expr_param_type,
                            param_type,
                        )
                    })
            })?;
        Ok(Self {
            default: self.default.clone().verify(&self)?,
            alert: self.alert,
            annotations: self.annotations,
            spec: self.spec,
        })
    }
}

impl AlertRuleConfig<CustomAlertRuleForm> {
    pub fn verify(self, template: &AlertRuleTemplate<CustomAlertRuleForm>) -> Result<Self> {
        let params = ParamValues(
            self.params
                .params
                .0
                .into_iter()
                .map(|(name, value)| {
                    let param_spec = template
                        .spec
                        .params
                        .get(&name)
                        .ok_or_else(|| Error::MissingParamConfig(name.clone()))?;
                    (param_spec.kind == ParamKind::Param)
                        .then_some(())
                        .ok_or_else(|| {
                            Error::ParamKindMismatch(
                                name.clone(),
                                param_spec.kind,
                                ParamKind::Param,
                            )
                        })?;
                    let typ = param_spec.r#type.param_type();
                    let value = value
                        .into_type(typ)
                        .map_err(|e| Error::ParamValue(name.clone(), e))?;
                    Ok((name, value))
                })
                .collect::<Result<_>>()?,
        );
        let thresholds = self
            .params
            .thresholds
            .into_iter()
            .map(|(severity, values)| {
                let values = values
                    .0
                    .into_iter()
                    .map(|(name, value)| {
                        let param_spec = template
                            .spec
                            .params
                            .get(&name)
                            .ok_or_else(|| Error::MissingParamConfig(name.clone()))?;
                        (param_spec.kind == ParamKind::Threshold)
                            .then_some(())
                            .ok_or_else(|| {
                                Error::ParamKindMismatch(
                                    name.clone(),
                                    param_spec.kind,
                                    ParamKind::Threshold,
                                )
                            })?;
                        let typ = param_spec.r#type.param_type();
                        let value = value
                            .into_type(typ)
                            .map_err(|e| Error::ParamValue(name.clone(), e))?;
                        Ok((name, value))
                    })
                    .collect::<Result<_>>()?;
                Ok((severity, ParamValues(values)))
            })
            .collect::<Result<_>>()?;
        Ok(Self {
            r#for: self.r#for,
            labels: self.labels,
            selectors: self.selectors,
            params: CustomAlertRuleConfig {
                instance: self.params.instance,
                params,
                thresholds,
            },
        })
    }
}

impl CustomAlertRuleForm {
    fn expr_combine_instances(
        template: &AlertRuleTemplate<Self>,
        config: &AlertRuleConfig<Self>,
        thresholds: &ParamValues,
        instances: &BTreeMap<String, MetricSelector>,
        item: &prometheus_schema::Item,
    ) -> Result<Expr> {
        let remove_labels = match &config.params.instance {
            InstanceAction::Best | InstanceAction::Worst if instances.len() > 1 => {
                let mut iter = instances.values();
                let first = iter
                    .next()
                    .ok_or_else(|| Error::NoInstances(template.spec.item.clone()))?;
                Some(
                    iter.fold(first.iter().collect::<BTreeMap<_, _>>(), |mut a, b| {
                        a.retain(|label, selector| (b.get(*label) != Some(*selector)));
                        a
                    })
                    .keys()
                    .copied()
                    .cloned()
                    .collect::<Vec<_>>(),
                )
            }
            _ => None,
        };
        instances
            .values()
            .map(|instance_selector| {
                let selectors = (*instance_selector).clone() & &config.selectors;
                let params = config
                    .params
                    .params
                    .0
                    .iter()
                    .chain(&thresholds.0)
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();
                let expr = template
                    .spec
                    .expr
                    .with_params(&template.spec.item, item, &selectors, &params)
                    .map_err(Error::SpecResolve)?;

                Ok(match remove_labels.as_ref() {
                    None => expr,
                    Some(labels) => expr.sum_without(labels.clone()),
                })
            })
            .reduce(|a, b| {
                Ok(match &config.params.instance {
                    InstanceAction::Individual | InstanceAction::Worst => a?.or(b?),
                    InstanceAction::Best => a?.and(b?),
                })
            })
            .ok_or_else(|| Error::NoInstances(template.spec.item.clone()))?
    }
}

//Fixed Traces
impl AlertRuleFormSpecific for FixedTracesAlertRuleForm {
    type Spec = FixedTraceAlertRuleSpec;
    type Config = FixedTraceAlertRuleConfig;

    fn exprs<'a>(
        _: &prometheus_schema::Universe,
        template: &AlertRuleTemplate<Self>,
        config: &'a AlertRuleConfig<Self>,
    ) -> Result<Vec<PromAlertRuleArgs<'a>>> {
        let object = match &template.spec.item {
            TraceItem::Operation => TraceObject::builder()
                .operation()
                .multiple(None)
                .item(operation_filter_from_labels(&config.labels)),
            TraceItem::Service(_) => return Err(Error::InvalidTraceItem),
        };
        let expr = match template.spec.param {
            TraceParameter::Mean => {
                let mean_aggr = TraceAggr::mean(template.spec.interval, object);
                TraceExpr::new(template.spec.metric, mean_aggr)
                    .expr(&InstantQueryParams { time: None })
            }
            TraceParameter::LowerBound => {
                let mean_aggr = TraceAggr::mean(template.spec.interval, object.clone());
                let mean_expr = TraceExpr::new(template.spec.metric, mean_aggr)
                    .expr(&InstantQueryParams { time: None });
                let ci_aggr = TraceAggr::ci(template.spec.interval, object);
                let ci_expr = TraceExpr::new(template.spec.metric, ci_aggr)
                    .expr(&InstantQueryParams { time: None });
                mean_expr - ci_expr
            }
            TraceParameter::HigherBound => {
                let mean_aggr = TraceAggr::mean(template.spec.interval, object.clone());
                let mean_expr = TraceExpr::new(template.spec.metric, mean_aggr)
                    .expr(&InstantQueryParams { time: None });
                let aggr = TraceAggr::ci(template.spec.interval, object);
                let ci_expr = TraceExpr::new(template.spec.metric, aggr)
                    .expr(&InstantQueryParams { time: None });
                mean_expr + ci_expr
            }
        };

        Ok(config
            .params
            .thresholds
            .iter()
            .map(|(severity, threshold)| PromAlertRuleArgs {
                severity: *severity,
                value_unit: template.spec.metric.unit(),
                params_spec: &FIXED_TRACES_PARAMS,
                params: Cow::Owned(ParamValues(BTreeMap::new())),
                thresholds: Cow::Owned(ParamValues(BTreeMap::from_iter([(
                    ParamName::from_str("min").unwrap(),
                    ParamValue::Float(OrderedFloat(threshold.into_inner())),
                )]))),
                expr: expr.clone().is_gt(threshold.into_inner()),
            })
            .collect::<Vec<_>>())
    }
}

static FIXED_TRACES_PARAMS: LazyLock<BTreeMap<ParamName, ParamSpec>> = LazyLock::new(|| {
    BTreeMap::from_iter([(
        ParamName::from_str("min").unwrap(),
        ParamSpec {
            kind: ParamKind::Threshold,
            r#type: ParamTypeSpec::Quantity {
                dimension: Dimension::Dimensionless,
                units: None,
                decimals: None,
            },
        },
    )])
});

impl AlertRuleSpecSpecific for FixedTraceAlertRuleSpec {
    fn item(&self) -> QualifiedItemName {
        QualifiedItemName::from_str("jaeger-anomaly-detection:default").unwrap()
    }
}

//Dynamic traces
impl AlertRuleFormSpecific for DynamicTracesAlertRuleForm {
    type Spec = DynamicTraceAlertRuleSpec;
    type Config = DynamicTraceAlertRuleConfig;

    fn exprs<'a>(
        _: &prometheus_schema::Universe,
        template: &AlertRuleTemplate<Self>,
        config: &'a AlertRuleConfig<Self>,
    ) -> Result<Vec<PromAlertRuleArgs<'a>>> {
        let object = match &template.spec.item {
            TraceItem::Operation => TraceObject::builder()
                .operation()
                .multiple(None)
                .item(operation_filter_from_labels(&config.labels)),
            TraceItem::Service(combine) => TraceObject::builder()
                .service(combine.clone())
                .multiple(None)
                .item(service_filter_from_labels(&config.labels)),
        };

        let aggr = TraceAggr::score(
            template.spec.short_term_interval,
            template.spec.long_term_interval,
            object,
        );

        let expr =
            TraceExpr::new(template.spec.metric, aggr).expr(&InstantQueryParams { time: None });

        Ok(config
            .params
            .thresholds
            .iter()
            .map(|(severity, threshold)| PromAlertRuleArgs {
                severity: *severity,
                value_unit: NEUTRAL_UNIT,
                params_spec: &DYNAMIC_TRACES_PARAMS,
                params: Cow::Owned(ParamValues(BTreeMap::new())),
                thresholds: Cow::Owned(ParamValues(BTreeMap::from_iter([(
                    ParamName::from_str("min").unwrap(),
                    ParamValue::Float(OrderedFloat(threshold.into_inner())),
                )]))),
                expr: expr.clone().is_gt(threshold.into_inner()),
            })
            .collect::<Vec<_>>())
    }
}

static DYNAMIC_TRACES_PARAMS: LazyLock<BTreeMap<ParamName, ParamSpec>> = LazyLock::new(|| {
    BTreeMap::from_iter([(
        ParamName::from_str("min").unwrap(),
        ParamSpec {
            kind: ParamKind::Threshold,
            r#type: ParamTypeSpec::Quantity {
                dimension: Dimension::Dimensionless,
                units: None,
                decimals: None,
            },
        },
    )])
});

fn service_filter_from_labels(labels: &BTreeMap<LabelName, String>) -> ServiceFilter {
    ServiceFilter::new()
        .opt_service_name(labels.get(&LabelName::new_static("service_name")))
        .opt_namespace(labels.get(&LabelName::new_static("service_namespace")))
        .opt_instance_id(labels.get(&LabelName::new_static("service_instance_id")))
}

fn operation_filter_from_labels(labels: &BTreeMap<LabelName, String>) -> OperationFilter {
    OperationFilter::new()
        .service(service_filter_from_labels(labels))
        .opt_operation_name(labels.get(&LabelName::new_static("operation_name")))
}

impl AlertRuleSpecSpecific for DynamicTraceAlertRuleSpec {
    fn item(&self) -> QualifiedItemName {
        QualifiedItemName::from_str("jaeger-anomaly-detection:default").unwrap()
    }
}

impl<T: AlertRuleFormSpecific> AlertRuleConfig<T> {
    pub fn matches(&self, labels: &BTreeMap<LabelName, String>) -> bool {
        self.selectors.matches(labels)
    }
}

impl Display for AlertName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for AlertRuleTemplateName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for AlertConfigName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for ReceiverName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AlertName {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl AlertRuleTemplateName {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl AlertConfigName {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl ReceiverName {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Minor => write!(f, "minor"),
            Severity::Warning => write!(f, "warning"),
            Severity::Critical => write!(f, "critical"),
            Severity::Major => write!(f, "major"),
        }
    }
}

impl FromStr for Severity {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        match s {
            "minor" => Ok(Self::Minor),
            "warning" => Ok(Self::Warning),
            "critical" => Ok(Self::Critical),
            "major" => Ok(Self::Major),
            _ => Err(Error::InvalidSeverity(s.to_string())),
        }
    }
}
