/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::path::PathBuf;

use prometheus_core::MetricName;
use prometheus_expr::{ParamName, ParamType, ParamTypeError, SpecResolveError};
use prometheus_schema::{ModuleName, ModuleVersion, ModuleVersionReq, QualifiedItemName};
use value::Type;

use crate::{
    alerts::{AlertRuleError, AlertRuleTemplateName, ParamKind},
    ids::{
        Absolute, ConnectionsPackageId, ItemId, ItemTypeId, PackageId, PropertyId, QueryElemId,
        RelationId, RelationTypeId, Relative, TplVarId,
    },
    query::resolved::TplVarDef,
    types::serial::{PackageVersion, PackageVersionReq, PropertyValueType},
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    // #[error("failed to read state from {0}: {1}")]
    // ReadState(PathBuf, std::io::Error),
    // #[error("failed to write state to {0}: {1}")]
    // WriteState(PathBuf, std::io::Error),
    // #[error("failed to read state from {0}: {1}")]
    // DecodeState(PathBuf, serde_json::Error),
    // #[error("failed to write state to {0}: {1}")]
    // EncodeState(PathBuf, serde_json::Error),
    // #[error("failed to bind socket: {0}")]
    // Bind(std::io::Error),
    // #[error("server error: {0}")]
    // Server(std::io::Error),
    #[error("failed to read package directory: {0}: {1}")]
    ReadPkgs(PathBuf, std::io::Error),
    #[error("failed to read package file: {0}: {1}")]
    ReadPkg(PathBuf, std::io::Error),
    #[error("connections package already loaded: {0}")]
    ConnectionsPackageAlreadyLoaded(ConnectionsPackageId),
    #[error("failed to read items file: {0}: {1}")]
    ReadItems(PathBuf, std::io::Error),
    #[error("failed to read query file: {0}: {1}")]
    ReadQuery(PathBuf, std::io::Error),
    #[error("failed to decode package file: {0}: {1}")]
    DecodePkg(PathBuf, serde_json::Error),
    #[error("failed to decode items file: {0}: {1}")]
    DecodeItems(PathBuf, serde_json::Error),
    #[error("failed to decode query file: {0}: {1}")]
    DecodeQuery(PathBuf, serde_json::Error),
    #[error("invalid absolute id (missing package name): {0}")]
    InvalidAbsoluteId(String),
    #[error("invalid item type id: {0}")]
    InvalidItemTypeId(String),
    #[error("invalid package id: {0}")]
    InvalidPackageId(String),
    #[error("invalid property id: {0}")]
    InvalidPropertyId(String),
    #[error("invalid relation type id: {0}")]
    InvalidRelationTypeId(String),
    #[error("invalid item id: {0}: {1}")]
    InvalidItemId(String, uuid::Error),
    #[error("invalid relation id: {0}: {1}")]
    InvalidRelationId(String, uuid::Error),
    #[error("invalid transaction id: {0}: {1}")]
    InvalidTransactionId(String, uuid::Error),
    #[error("invalid value for property: {1}")]
    InvalidPropertyValueJson(PropertyValueType, #[source] serde_json::Error),
    #[error("invalid value for property")]
    InvalidPropertyValue(PropertyValueType),
    #[error("invalid value for property: {0}: {1}")]
    InvalidPropertyValueWithId(Absolute<PropertyId>, #[source] Box<Error>),
    #[error("invalid value selector")]
    InvalidValueSelector,
    #[error("invalid value selector")]
    InvalidValueSelectorJson(serde_json::Error),
    #[error("invalid value selector: unsupported type: {0}")]
    UnsupportedValueSelector(Type),
    #[error("invalid template variable value: {0}")]
    InvalidTplVarValue(serde_json::Error),
    #[error("template variable {0} with type {2} used where type {1} was expected")]
    InvalidTplVarType(TplVarId, TplVarDef, TplVarDef),
    #[error("missing item type: {0}")]
    MissingItemType(Absolute<ItemTypeId>),
    #[error("missing property type: {0}")]
    MissingProperty(Absolute<PropertyId>),
    #[error("missing relation type: {0}")]
    MissingRelationType(Absolute<RelationTypeId>),
    #[error("missing item: {0}")]
    MissingItem(ItemId),
    #[error("missing relation: {0}")]
    MissingRelation(RelationId),
    #[error("missing query element: {0}")]
    MissingQueryElem(QueryElemId),
    #[error("missing template variable definition: {0}")]
    MissingTplVarDef(TplVarId),
    #[error("missing child link from {0} to {1}")]
    MissingChild(ItemId, ItemId),
    #[error("missing item in item type index")]
    MissingInItemTypeIndex,
    #[error("prometheus schema is not available")]
    MissingPrometheusSchema,
    #[error("{0}: prometheus module is not available: {0}")]
    MissingPrometheusModule(ConnectionsPackageId, ModuleName),
    #[error("prometheus module is not compatible: {0}: found {1} version {2}, required {3}")]
    IncompatiblePrometheusModule(
        ConnectionsPackageId,
        ModuleName,
        Box<ModuleVersion>,
        Box<ModuleVersionReq>,
    ),
    #[error("missing prometheus item: {0}")]
    MissingPrometheusItem(prometheus_schema::QualifiedItemName),
    #[error("failed to load prometheus schema tree: {0}")]
    PromResolve(prometheus_schema::schema::ResolveError),
    #[error("trace item \"service\" is not supported for trace fixed threshold rules")]
    InvalidTraceItem,
    #[error("invalid link between item {0} and relation {1}")]
    InvalidRelationLink(ItemId, RelationId),
    #[error("item {0} is not in domain: type {1} is not included")]
    ItemTypeNotInDomain(ItemId, Absolute<ItemTypeId>),
    #[error("relation {0} is not in domain: type {1} is not included")]
    RelationTypeNotInDomain(RelationId, Absolute<RelationTypeId>),
    #[error("item {0} is not in domain: not a child of the root")]
    ItemNotInDomain(ItemId),
    #[error("relation {0} is not in domain: not connected to an item in the domain")]
    RelationNotInDomain(RelationId),
    #[error("item {0} collides with another one outside domain")]
    ItemInDomainCollision(ItemId),
    #[error("relation {0} collides with another one outside domain")]
    RelationInDomainCollision(RelationId),
    #[error("unknown transaction")]
    UnknownTransaction,
    #[error("retry transaction")]
    RetryTransaction,
    #[error("item doc is missing item_type field")]
    DbMissingItemType,
    #[error("relation doc is missing relation_type field")]
    DbMissingRelationType,
    #[error("relation doc is missing source field")]
    DbMissingSource,
    #[error("relation doc is missing target field")]
    DbMissingTarget,
    #[error("package {0} {1} requires {2} {3}, but the package was not found")]
    MissingRequirement(
        PackageId,
        Box<PackageVersion>,
        PackageId,
        Box<PackageVersionReq>,
    ),
    #[error("package {0} {1} requires {2} {4}, but got {3}")]
    IncompatibleRequirement(
        PackageId,
        Box<PackageVersion>,
        PackageId,
        Box<PackageVersion>,
        Box<PackageVersionReq>,
    ),
    #[error("connections package {0} {1} requires {2} {3}, but the package was not found")]
    ConnectionsMissingRequirement(
        ConnectionsPackageId,
        Box<PackageVersion>,
        PackageId,
        Box<PackageVersionReq>,
    ),
    #[error("connections package {0} {1} requires {2} {4}, but got {3}")]
    ConnectionsIncompatibleRequirement(
        ConnectionsPackageId,
        Box<PackageVersion>,
        PackageId,
        Box<PackageVersion>,
        Box<PackageVersionReq>,
    ),
    #[error("failed to parse prometheus expr spec: {0}")]
    ParseExprSpec(String),
    #[error("missing rule name for alert")]
    MissingAlertRuleName,
    #[error("missing severity for alert")]
    MissingAlertSeverity,
    #[error("metric selector without metric name")]
    MissingMetricName,
    #[error("metric {1} not found for item {0}")]
    MissingMetric(QualifiedItemName, MetricName),
    #[error("missing prometheus item {0}")]
    MissingPromItem(QualifiedItemName),
    #[error("no instances for item {0}")]
    NoInstances(QualifiedItemName),
    #[error("missing alert rule: {0}")]
    MissingAlertRule(AlertRuleTemplateName),
    #[error("missing parameter: {0}")]
    MissingParam(ParamName),
    #[error("failed to generate alert rule: {0}")]
    GenerateAlertRule(AlertRuleError),
    #[error("invalid severity: {0}")]
    InvalidSeverity(String),
    #[error("{0} is used both as threshold and as non-threshold parameter")]
    ParamThresholdConflict(ParamName),
    #[error("type mismatch for param {0}: specified as {1}, used as {2}")]
    ParamTypeMismatch(ParamName, ParamType, ParamType),
    #[error("kind mismatch for param {0}: specified as {1}, used as {2}")]
    ParamKindMismatch(ParamName, ParamKind, ParamKind),
    #[error("param {0}: is used both as {1} and {2}")]
    ParamTypeConflict(ParamName, ParamType, ParamType),
    #[error("param {0} is missing in specification")]
    MissingParamSpec(ParamName),
    #[error("param {0} is missing in config")]
    MissingParamConfig(ParamName),
    #[error("invalid value for param {0}: {1}")]
    ParamValue(ParamName, prometheus_expr::ValueError),
    #[error("invalid name template: {0}")]
    ParseNameTemplate(String),
    #[error("param type error: {0}")]
    ParamType(ParamTypeError),
    #[error("failed to resolve expr specification: {0}")]
    SpecResolve(SpecResolveError),
    #[error("cannot update provisioned alert rule template {0}")]
    ProvisionedAlertRule(AlertRuleTemplateName),
    #[error("Rule form version conflict {0}")]
    AlertRuleFormVersionConflict(AlertRuleTemplateName),
    #[error("Mismatch template name and rule form name {0}")]
    MismatchAlertName(AlertRuleTemplateName),
    #[error("item type {0} already defined in another view")]
    ViewSameItemType(Relative<ItemTypeId>),
    #[error("relation type {0} already defined in another view")]
    ViewSameRelationType(Relative<RelationTypeId>),
    #[error("item type {0} is not found in view")]
    ItemTypeView(Relative<ItemTypeId>),
}
