/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, ffi::OsString, path::PathBuf};

use actix_web::{http::StatusCode, ResponseError};
use apistos::ApiErrorComponent;
use dbschema::ObjectId;
use prometheus_api::DecodeQueryError;
use prometheus_expr::SpecResolveError;
use prometheus_schema::{schema::ResolveError, QualifiedItemName};
use relation_graph::{
    alerts::AlertRuleTemplateName, metrics::SourceId, Absolute, ConnectionsPackageId, DashboardId,
    ItemId, ItemTypeId, PackageId, PropertyId, RelationId, RelationTypeId, ViewId,
};
use reqwest::header::InvalidHeaderValue;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("failed to initialize crypto provider")]
    InitCrypto,
    #[error("failed to initialize tracer : {0}")]
    OpenTelemetry(opentelemetry::trace::TraceError),
    // #[error("failed to read state from {0}: {1}")]
    // ReadState(PathBuf, std::io::Error),
    // #[error("failed to write state to {0}: {1}")]
    // WriteState(PathBuf, std::io::Error),
    #[error("failed to read elastic ca certificate {0}: {1}")]
    ReadEsCaCert(PathBuf, std::io::Error),
    #[error("failed to read elastic client certificate {0}: {1}")]
    ReadEsCert(PathBuf, std::io::Error),
    #[error("failed to read elastic client key {0}: {1}")]
    ReadEsKey(PathBuf, std::io::Error),
    // #[error("failed to read provisioned packages: {0}: {1}")]
    // ReadPkgs(PathBuf, relation_graph::Error),
    #[error("failed to read provisioned alert rules: {0}: {1}")]
    ReadAlertRules(PathBuf, std::io::Error),
    #[error("failed to decode elastic ca certificate {0}: {1}")]
    DecodeEsCaCert(PathBuf, reqwest::Error),
    #[error("failed to decode elastic client certificate and key {0} / {1}: {2}")]
    DecodeEsCert(PathBuf, PathBuf, reqwest::Error),
    #[error("failed to deserialize package {0}: {1}")]
    DecodePackage(PackageId, serde_json::Error),
    #[error("failed to deserialize item {0}: {1}")]
    DecodeItem(ObjectId, serde_json::Error),
    // #[error("failed to deserialize alert rule {0}: {1}")]
    // DecodeAlertRule(ObjectId, serde_json::Error),
    #[error("failed to deserialize alert specification {0}: {1}")]
    DecodeAlertRuleSpecYaml(PathBuf, serde_yaml::Error),
    #[error("failed to build elastic client: {0}")]
    BuildEsClient(reqwest::Error),
    #[error("invalid elastic url: {0}")]
    InvalidEsUrl(url::ParseError),
    #[error("invalid alert rule file name: {0:?}")]
    InvalidAlertRuleFileName(OsString),
    // #[error("failed to write state to {0}: {1}")]
    // EncodeState(PathBuf, serde_json::Error),
    #[error("failed to bind socket: {0}")]
    Bind(std::io::Error),
    #[error("server error: {0}")]
    Server(std::io::Error),
    #[error(transparent)]
    Lib(#[from] relation_graph::Error),
    #[error("failed to initialize tls config: {0}")]
    TlsConfig(rpc::Error),
    #[error("failed to initialize tls config: {0}")]
    TlsConfigDnsName(rustls::pki_types::InvalidDnsNameError),
    #[error("dbdaemon error: {0}")]
    DbRpc(rpc::Error),
    #[error("dbdaemon error: {0}")]
    Db(String),
    #[error("package not found: {0}")]
    PackageNotFound(PackageId),
    #[error("connections package not found: {0}")]
    ConnectionsPackageNotFound(ConnectionsPackageId),
    #[error("item type not found: {0}")]
    ItemTypeNotFound(Absolute<ItemTypeId>),
    #[error("relation type not found: {0}")]
    RelationTypeNotFound(Absolute<RelationTypeId>),
    #[error("property not found: {0}")]
    PropertyNotFound(Absolute<PropertyId>),
    #[error("item not found: {0}")]
    ItemNotFound(ItemId),
    #[error("relation not found: {0}")]
    RelationNotFound(RelationId),
    #[error("error requesting events over time: {0}")]
    ReqEvents(reqwest_middleware::Error),
    #[error("error decoding events over time: {0}")]
    DecodeEvents(reqwest::Error),
    #[error("error decoding event: {0}")]
    DecodeEvent(ObjectId, serde_json::Error),
    #[error("error requesting alert counts: {0}")]
    ReqAlertCounts(reqwest::Error),
    #[error("error requesting alert counts: {0}")]
    ReqAlertCountsMw(reqwest_middleware::Error),
    #[error("error decoding alert counts: {0}")]
    DecodeAlertCountsRes(reqwest::Error),
    #[error("prometheus schema is not available")]
    MissingPromSchema,
    #[error("prometheus metrics not loaded")]
    MissingPromMetrics,
    #[error("failed to load prometheus schema: {0}")]
    PromResolve(ResolveError),
    #[error("failed to build prometheus client: {0}")]
    PromClientBuild(reqwest::Error),
    #[error("invalid prometheus tenant: {0}")]
    PromTenantInvalid(InvalidHeaderValue),
    #[error("prometheus request failed: {0}")]
    PromRequestMw(reqwest_middleware::Error),
    #[error("prometheus request failed: {0}")]
    PromRequest(reqwest::Error),
    #[error("failed to decode prometheus response: {0}")]
    PromDecode(reqwest::Error),
    // #[error("failed to decode prometheus response: {0}")]
    // PromDecodeJson(serde_json::Error),
    #[error("failed to decode prometheus response: {0}")]
    PromDecodeYaml(serde_yaml::Error),
    // #[error("prometheus query failed: {0}")]
    // PromQuery(String),
    #[error("prometheus returned error: {0}")]
    Prometheus(prometheus_api::ErrorResponse),
    #[error("failed to lookup prometheus item: {0}")]
    PromWalk(#[from] prometheus_schema::WalkError),
    #[error("got unexpected result from prometheus query: {0}")]
    PromUnexpectedResult(DecodeQueryError),
    #[error("view not found: {0}")]
    ViewNotFound(ViewId),
    #[error("failed to read provisioned views: {0}: {1}")]
    ReadViews(PathBuf, std::io::Error),
    #[error("invalid view file name: {0:?}")]
    InvalidViewFileName(OsString),
    #[error("failed to deserialize view {0}: {1}")]
    DecodeView(ViewId, serde_json::Error),
    #[error("view not found: {0}")]
    DashboardNotFound(DashboardId),
    #[error("failed to read provisioned dashboards: {0}: {1}")]
    ReadDashboards(PathBuf, std::io::Error),
    #[error("invalid dashboard file name: {0:?}")]
    InvalidDashboardFileName(OsString),
    #[error("failed to deserialize dashboard {0}: {1}")]
    DecodeDashboard(DashboardId, serde_json::Error),
    // #[error("missing entity type: {0}")]
    // MissingEntityType(AbsEntityTypeId),
    // #[error("missing entity: {0}")]
    // MissingEntity(EntityId),
    #[error("missing source: {0}")]
    MissingSource(SourceId),
    #[error("missing alert rule: {0}")]
    MissingAlertRule(AlertRuleTemplateName),
    // #[error("missing alert rule config: {0}: {1}")]
    // MissingAlertConfig(AlertRuleName, AlertConfigName),
    #[error("invalid rule group name: {0}")]
    InvalidRuleGroupName(String),
    // #[error("missing item: {0}")]
    // MissingItem(ItemId),
    // #[error("missing relation: {0}")]
    // MissingRelation(RelationId),
    // #[error("invalid item or relation id: {0}: {1}")]
    // InvalidItemOrRelationId(ObjectId, uuid::Error),
    #[error("invalid alert value: {0}")]
    DecodeAlertDoc(serde_json::Error),
    #[error("invalid status change value: {0}")]
    DecodeStatusDoc(serde_json::Error),
    #[error("url parse error: {0}")]
    Url(#[from] url::ParseError),
    #[error("failed to deserialize Elasticsearch reponse: {0}")]
    DecodeESResponse(serde_json::Error),
    #[error("unknown prometheus module: {0}")]
    MissingPromModule(prometheus_schema::ModuleName),
    #[error("unknown prometheus item: {0}")]
    MissingPromItem(prometheus_schema::QualifiedItemName),
    #[error("time calculations failed")]
    TimeHandling,
    #[error("failed to resolve expression: {0}")]
    ResolveExprSpec(SpecResolveError),
    #[error("prometheus item mismatch: {0} vs {1}")]
    PromItemMismatch(QualifiedItemName, QualifiedItemName),
    #[error("invalid prometheus module name: {0}")]
    InvalidPromModuleName(prometheus_schema::ParseIdError),
    #[error("invalid qualified prometheus item name: {0}")]
    InvalidQualifiedItemName(prometheus_schema::ParseIdError),
    #[error("item type not found in view")]
    ItemTypeView,
    #[error("failed to generate schema")]
    GenerateSchema,
    #[error("failed to join schema generator thread: {0}")]
    GenerateSchemaJoin(tokio::task::JoinError),
}

impl Error {
    pub(crate) fn http_status_code(&self) -> StatusCode {
        match self {
            Error::DecodePackage(_, _) | Error::DecodeItem(_, _) => StatusCode::NOT_ACCEPTABLE,
            Error::PackageNotFound(_)
            | Error::ItemTypeNotFound(_)
            | Error::RelationTypeNotFound(_)
            | Error::PropertyNotFound(_)
            | Error::ItemNotFound(_)
            | Error::RelationNotFound(_)
            | Error::MissingAlertRule(_)
            | Error::MissingPromModule(_)
            | Error::MissingPromItem(_)
            | Error::PromWalk(_)
            | Error::MissingSource(_) => StatusCode::NOT_FOUND,
            Error::Lib(e) => match e {
                relation_graph::Error::UnknownTransaction => StatusCode::NOT_FOUND,
                relation_graph::Error::RetryTransaction => StatusCode::CONFLICT,
                _ => StatusCode::NOT_ACCEPTABLE,
            },
            _ => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

impl ResponseError for crate::Error {
    fn status_code(&self) -> StatusCode {
        self.http_status_code()
    }
}

impl ApiErrorComponent for crate::Error {
    fn schemas_by_status_code() -> std::collections::BTreeMap<
        String,
        (String, apistos::reference_or::ReferenceOr<apistos::Schema>),
    > {
        BTreeMap::new()
    }

    fn error_responses() -> Vec<(String, apistos::paths::Response)> {
        Vec::new()
    }
}
