/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use relation_graph::{ItemId, RunQueryError};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    RelationGraph(#[from] relation_graph::Error),
    #[error(transparent)]
    LoadPrometheusSchema(#[from] prometheus_schema::LoadSchemaError),
    #[error("failed to build elastic query: {0}")]
    EsFilter(dbschema_elastic::FilterError),
    #[error("invalid file name for alert rule spec")]
    AlertRuleName,
    #[error("failed to read rule spec: {0}")]
    ReadRuleSpec(std::io::Error),
    #[error("failed to read rule config: {0}")]
    ReadRuleConfig(std::io::Error),
    #[error("failed to read rule spec: {0}")]
    DecodeRuleSpec(serde_yaml::Error),
    #[error("failed to read rule config: {0}")]
    DecodeRuleConfig(serde_yaml::Error),
    // #[error("failed to parse prometheus expression specification: {0}")]
    // ParsePromExprSpec(String),
    // #[error("failed to parse parameter")]
    // ParseParam,
    // #[error("failed to parse parameter: {0}")]
    // ParseParamValue(std::num::ParseFloatError),
    #[error("failed to generate alert rules: {0}")]
    AlertRules(relation_graph::Error),
    #[error("missing item: {0}")]
    MissingItem(ItemId),
    #[error("Mistmach template and config types of rule form")]
    TemplateConfigtypeMistmatch,
    #[error("Failed to run query: {0}")]
    RunQuery(RunQueryError),
}
