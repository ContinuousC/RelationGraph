/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

mod alerts;
mod app;
mod changes;
mod connector;
mod dashboards;
mod error;
mod events;
mod items;
mod metrics;
mod packages;
mod prom_schema;
mod queries;
mod status;
mod transactions;
mod types;
mod views;

mod auth;
mod roles;

mod app_data;
pub(crate) mod utils;

use std::{
    collections::{BTreeMap, BTreeSet},
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};

use actix_web::{
    http::StatusCode,
    middleware,
    web::{Data, JsonConfig},
    App, HttpResponse, HttpResponseBuilder, HttpServer,
};
use apistos::{
    app::OpenApiWrapper,
    info::Info,
    server::{Server, ServerVariable},
    spec::Spec,
    web::scope,
};
use app_data::Locks;
use async_recursion::async_recursion;
use clap::Parser;
use dbdaemon_api::{BackendDbProto, DbClient};
use dbdaemon_types::Operation;
use dbschema::{DbTable, DbTableId, HasSchema, SingleVersioned, VersioningType};
use parking_lot::RwLock;
use prom_schema::PromSchemaState;
use reqwest::{
    header::{HeaderMap, HeaderValue},
    Certificate,
};
use roles::EditorRole;
use rpc::AsyncClient;
use rustls::pki_types::ServerName;
use serde::{Deserialize, Serialize};
use tap::pipe::Pipe;
use tokio::sync::Mutex as AsyncMutex;

use prometheus_schema::Universe;
use relation_graph::{
    alerts::{
        AlertConfigName, AlertDoc, AlertRuleConfigs, AlertRuleState, AlertRuleTemplateName,
        AlertRuleTemplates, AlertRulesState, UnverifiedAlertRuleForms, VersionedAlertMap,
    },
    db::{DbItem, DbItems},
    status::{StatusDoc, VersionedStatusMap},
    ConnectionsPackages, DashboardId, Dashboards, OverviewDashboard, Package, PackageId, Packages,
    State, Updates, View, ViewId, Views,
};

use error::{Error, Result};
use opentelemetry::trace::TracerProvider;
use tracing::Instrument;
use tracing_actix_web::TracingLogger;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use mimalloc::MiMalloc;

pub(crate) use app_data::AppData;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Parser)]
struct Args {
    #[clap(long)]
    spec: bool,
    #[clap(long)]
    schemas: bool,
    #[clap(long, env, help = "The application's instance id, for tracing.")]
    instance_id: Option<String>,
    #[clap(long, env, help = "The name of the Node.")]
    k8s_node_name: Option<String>,
    #[clap(
        long,
        env,
        help = "The name of the namespace that the pod is running in."
    )]
    k8s_namespace_name: Option<String>,
    #[clap(long, env, help = "The name of the Pod.")]
    k8s_pod_name: Option<String>,
    #[clap(long, env, help = "The UID of the Pod.")]
    k8s_pod_uid: Option<String>,
    #[clap(
        long,
        env,
        help = "The name of the Container from Pod specification, must be unique within a Pod."
    )]
    k8s_container_name: Option<String>,
    #[clap(long, default_value = "localhost:9999")]
    bind: String,
    #[clap(long, default_value = "localhost:9997")]
    db: String,
    #[clap(long, default_value = "/usr/share/smartm/certs/continuousc-dev")]
    certs: PathBuf,
    #[clap(long, default_value = "ca.crt")]
    ca: PathBuf,
    #[clap(long, default_value = "backend.crt")]
    db_cert: PathBuf,
    #[clap(long, default_value = "backend.key")]
    db_key: PathBuf,
    #[clap(long, default_value = "server.crt")]
    server_cert: PathBuf,
    #[clap(long, default_value = "server.key")]
    server_key: PathBuf,
    #[clap(long, default_value = "/api")]
    prefix: String,
    #[clap(long, default_value = "https://opensearch-cluster-master:9200/")]
    elastic_url: reqwest::Url,
    #[clap(long, default_value = "/usr/share/continuousc/certs/opensearch/ca.crt")]
    elastic_ca: PathBuf,
    #[clap(
        long,
        default_value = "/usr/share/continuousc/certs/opensearch/tls.crt"
    )]
    elastic_cert: PathBuf,
    #[clap(
        long,
        default_value = "/usr/share/continuousc/certs/opensearch/tls.key"
    )]
    elastic_key: PathBuf,
    #[clap(long, default_value = "http://cortex-query-frontend.cortex:8080")]
    prometheus_url: reqwest::Url,
    #[clap(long)]
    prometheus_tenant: Option<String>,
    #[clap(long, default_value = "http://cortex-ruler.cortex:8080")]
    ruler_url: reqwest::Url,
    #[clap(long, default_value = "http://cortex-alertmanager.cortex:8080")]
    alertmanager_url: reqwest::Url,
    /// Base path for all provisioned files.
    #[clap(long, default_value = "/usr/share/continuousc")]
    provisioned: PathBuf,
    /// Provisioned packages path.
    #[clap(long, default_value = "packages")]
    packages: PathBuf,
    /// Provisioned connections packages path.
    #[clap(long, default_value = "connections")]
    connections: PathBuf,
    /// Provisioned prometheus schema path (including the root for now)
    #[clap(long, default_value = "prometheus/prometheus.root.yaml")]
    prom_schema: PathBuf,
    /// Provisioned alert rules path.
    #[clap(long, default_value = "alertrules")]
    alert_rules: PathBuf,
    /// Provisioned views path.
    #[clap(long, default_value = "views")]
    views: PathBuf,
    /// Provisioned dashboards path.
    #[clap(long, default_value = "dashboards")]
    dashboards: PathBuf,
    /// App version.
    #[clap(long, env = "APP_VERSION")]
    app_version: String,
}

#[derive(Serialize, Deserialize, HasSchema)]
struct PackageDoc {
    #[dbschema(json)]
    package: Package,
}

static PKGS_TABLE: DbTableId = DbTableId::from_static("packages");
static ITEMS_TABLE: DbTableId = DbTableId::from_static("items");
static ALERT_RULES_TABLE: DbTableId = DbTableId::from_static("alert-rules");
static ALERT_CONFIG_TABLE: DbTableId = DbTableId::from_static("alert-config");
static ALERTS_TABLE: DbTableId = DbTableId::from_static("alerts");
static STATUS_TABLE: DbTableId = DbTableId::from_static("status");

#[actix_web::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    if args.spec || args.schemas {
        return main_traced(args).await;
    }

    // env_logger::init();

    rustls::crypto::aws_lc_rs::default_provider()
        .install_default()
        .map_err(|_| Error::InitCrypto)?;

    let exporter = opentelemetry_otlp::SpanExporter::builder()
        .with_tonic()
        .build()
        .map_err(Error::OpenTelemetry)?;

    let mut attrs = vec![
        opentelemetry::KeyValue::new("service.namespace", "continuousc"),
        opentelemetry::KeyValue::new("service.name", "relation-graph-engine"),
        opentelemetry::KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
    ];

    for (attr, value) in [
        ("service.instance.id", &args.instance_id),
        ("k8s.node.name", &args.k8s_node_name),
        ("k8s.namespace.name", &args.k8s_namespace_name),
        ("k8s.pod.name", &args.k8s_pod_name),
        ("k8s.pod.uid", &args.k8s_pod_uid),
        ("k8s.container.name", &args.k8s_container_name),
    ] {
        if let Some(value) = value.as_ref() {
            attrs.push(opentelemetry::KeyValue::new(attr, value.to_string()));
        }
    }

    let tracer = opentelemetry_sdk::trace::TracerProvider::builder()
        .with_batch_exporter(exporter, opentelemetry_sdk::runtime::Tokio)
        // .with_sampler(opentelemetry_sdk::Sampler::AlwaysOn)
        // .with_id_generator(opentelemetry_sdk::RandomIdGenerator::default())
        // .with_max_events_per_span(64)
        // .with_max_attributes_per_span(16)
        // .with_max_events_per_span(16)
        .with_resource(opentelemetry_sdk::Resource::new(attrs))
        .build()
        .tracer("Relation Graph Engine");

    let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);

    tracing_subscriber::FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish()
        .with(telemetry)
        .init();

    opentelemetry::global::set_text_map_propagator(
        opentelemetry_sdk::propagation::TraceContextPropagator::new(),
    );

    let r = main_traced(args)
        .instrument(tracing::span!(tracing::Level::INFO, "main"))
        .await;

    if let Err(e) = &r {
        log::error!("{e}");
    }

    //opentelemetry::global::shutdown_tracer_provider();
    r
}

async fn main_traced(args: Args) -> Result<()> {
    if args.schemas {
        let schema = schemars::schema_for!(AlertRuleTemplates);
        println!("{}", serde_json::to_string_pretty(&schema).unwrap());
        return Ok(());
    }

    let http_app = |prefix: &str, data: Option<&Data<AppData>>| {
        App::new()
            .document(Spec {
                info: Info {
                    title: String::from("Relation Graph API"),
                    version: String::from(env!("CARGO_PKG_VERSION")),
                    ..Default::default()
                },
                servers: vec![Server {
                    url: String::from("https://{tenant}.continuousc.contc"),
                    variables: BTreeMap::from_iter([(
                        String::from("tenant"),
                        ServerVariable {
                            default: String::from("tenant-demo"),
                            _enum: vec![String::from("tenant-demo"), String::from("tenant-mdp")],
                            ..Default::default()
                        },
                    )]),
                    ..Default::default()
                }],
                ..Default::default()
            })
            .wrap(TracingLogger::default())
            .wrap(middleware::Compress::default())
            .service({
                scope(prefix)
                    .app_data(JsonConfig::default().limit(50 * (1 << 20)))
                    .pipe(|app| match data {
                        Some(data) => app.app_data(data.clone()),
                        None => app,
                    })
                    .service(packages::packages_svc())
                    .service(packages::package_svc())
                    .service(packages::connections_package_svc())
                    .service(types::service())
                    .service(items::item_svc())
                    .service(items::relation_svc())
                    .service(items::items_svc())
                    .service(transactions::service())
                    .service(queries::query_svc())
                    .service(queries::info_query_svc())
                    .service(queries::search_domain_svc())
                    .service(queries::elements_svc())
                    .service(changes::service())
                    .service(metrics::service())
                    .service(alerts::alert_rules_svc())
                    .service(alerts::alerts_svc())
                    .service(status::service())
                    .service(views::service())
                    .service(app::service())
                    .service(dashboards::service())
                    .service(prom_schema::service())
                // #[cfg(debug)]
                // let scope = scope.service(development::service());
            })
            .build_spec()
    };

    if args.spec {
        let (_, spec) = http_app(&args.prefix, None);
        println!("{}", serde_json::to_string_pretty(&spec).unwrap());
        return Ok(());
    }

    log::info!("Creating es client");

    let pem = tokio::fs::read(&args.elastic_ca)
        .await
        .map_err(|e| Error::ReadEsCaCert(args.elastic_ca.clone(), e))?;
    let ca = Certificate::from_pem(&pem)
        .map_err(|e| Error::DecodeEsCaCert(args.elastic_ca.clone(), e))?;

    let pem = tokio::fs::read(&args.elastic_cert)
        .await
        .map_err(|e| Error::ReadEsCert(args.elastic_cert.clone(), e))?;
    let key = tokio::fs::read(&args.elastic_key)
        .await
        .map_err(|e| Error::ReadEsKey(args.elastic_key.clone(), e))?;
    let id = reqwest::Identity::from_pkcs8_pem(&pem, &key)
        .map_err(|e| Error::DecodeEsCert(args.elastic_cert.clone(), args.elastic_key.clone(), e))?;

    let es = reqwest::Client::builder()
        .add_root_certificate(ca)
        .identity(id)
        .build()
        .map_err(Error::BuildEsClient)?;
    let es = reqwest_middleware::ClientBuilder::new(es)
        .with(reqwest_tracing::TracingMiddleware::<
            reqwest_tracing::SpanBackendWithUrl,
        >::new())
        .build();

    log::info!("Connecting to dbdaemon");

    let db = DbClient::new(
        AsyncClient::<BackendDbProto, serde_cbor::Value, ()>::builder()
            .tcp(args.db.clone())
            .await
            .tls(
                rpc::tls_client_config(
                    &args.certs.join(&args.ca),
                    &args.certs.join(&args.db_cert),
                    &args.certs.join(&args.db_key),
                )
                .await
                .map_err(Error::TlsConfig)?,
                ServerName::try_from(args.db.split(':').next().unwrap().to_string())
                    .map_err(Error::TlsConfigDnsName)?,
            )
            .json(),
    );

    db.inner()
        .connected(std::time::Duration::from_secs(3))
        .await
        .map_err(Error::DbRpc)?;

    log::info!("Successfully connected to dbdaemon");

    /* Load packages. */

    log::info!("Loading packages");

    db.register_table(
        PKGS_TABLE.clone(),
        DbTable {
            versioning: VersioningType::SingleTimeline,
            schema: PackageDoc::schema().into_named_field_schema().unwrap(),
            force_update: false,
        },
    )
    .await
    .map_err(Error::Db)?;

    let pkgs = Packages::load_async(&args.provisioned.join(&args.packages)).await?;

    let pkgs = db
        .query_discovery_objects(PKGS_TABLE.clone(), dbschema::Filter::All(Vec::new()))
        .await
        .map_err(Error::Db)?
        .into_iter()
        .try_fold(pkgs, |mut pkgs, (object_id, versioned)| {
            let pkg_id = PackageId::from_str(object_id.as_str())?;
            let pkg = serde_json::from_value::<PackageDoc>(versioned.value)
                .map_err(|e| Error::DecodePackage(pkg_id.clone(), e))?;
            pkgs.insert(pkg_id, pkg.package);
            Result::Ok(pkgs)
        })?;
    let types = pkgs.types()?;

    log::info!("Successfully loaded packages");

    /* Load views. */

    log::info!("Loading views");

    let viewsdir = args.provisioned.join(&args.views);
    let mut views = Views::new();
    let mut dir = tokio::fs::read_dir(&viewsdir)
        .await
        .map_err(|e| Error::ReadViews(viewsdir.clone(), e))?;

    while let Some(ent) = dir
        .next_entry()
        .await
        .map_err(|e| Error::ReadViews(viewsdir.clone(), e))?
    {
        if !is_file(ent.path(), &mut Vec::new())
            .await
            .map_err(|e| Error::ReadViews(ent.path(), e))?
        {
            continue;
        }
        let name = ent
            .file_name()
            .into_string()
            .map_err(Error::InvalidViewFileName)?;
        let Some(id) = name.strip_suffix(".json") else {
            continue;
        };
        let id = ViewId::from_str(id).map_err(|_| Error::InvalidViewFileName(ent.file_name()))?;
        let data = tokio::fs::read(ent.path())
            .await
            .map_err(|e| Error::ReadViews(ent.path(), e))?;
        let view =
            serde_json::from_slice::<View>(&data).map_err(|e| Error::DecodeView(id.clone(), e))?;
        views.insert(id, view)?;
    }

    log::info!("Successfully loaded views");

    /* Load dashboards. */

    log::info!("Loading dashboards");

    let dashboardsdir = args.provisioned.join(&args.dashboards);
    let mut dashboards = Dashboards::new();
    let mut dir = tokio::fs::read_dir(&dashboardsdir)
        .await
        .map_err(|e| Error::ReadDashboards(dashboardsdir.clone(), e))?;

    while let Some(ent) = dir
        .next_entry()
        .await
        .map_err(|e| Error::ReadDashboards(dashboardsdir.clone(), e))?
    {
        if !is_file(ent.path(), &mut Vec::new())
            .await
            .map_err(|e| Error::ReadDashboards(ent.path(), e))?
        {
            continue;
        }
        let name = ent
            .file_name()
            .into_string()
            .map_err(Error::InvalidDashboardFileName)?;
        let Some(id) = name.strip_suffix(".json") else {
            continue;
        };
        let id = DashboardId::from_str(id)
            .map_err(|_| Error::InvalidDashboardFileName(ent.file_name()))?;
        let data = tokio::fs::read(ent.path())
            .await
            .map_err(|e| Error::ReadDashboards(ent.path(), e))?;
        let dashboard = serde_json::from_slice::<OverviewDashboard>(&data)
            .map_err(|e| Error::DecodeDashboard(id.clone(), e))?;
        dashboards.insert(id, dashboard);
    }

    log::info!("Successfully loaded dashboards");

    /* Load items. */

    log::info!("Loading items and relations");

    db.register_table(
        ITEMS_TABLE.clone(),
        DbTable {
            versioning: VersioningType::SingleTimeline,
            schema: DbItem::schema(&types).into_named_field_schema().unwrap(),
            force_update: false,
        },
    )
    .await
    .map_err(Error::Db)?;

    let items = db
        .query_discovery_objects(ITEMS_TABLE.clone(), dbschema::Filter::All(Vec::new()))
        .await
        .map_err(Error::Db)?
        .into_iter()
        .map(|(object_id, versioned)| {
            let item = serde_json::from_value::<DbItem>(versioned.value)
                .map_err(|e| Error::DecodeItem(object_id.clone(), e))?;
            Ok((object_id, item))
        })
        .collect::<Result<DbItems>>()?
        .cleanup_partial()
        .resolve_tx(&types)?;

    log::info!("Successfully loaded items and relations");

    /* Load provisioned alert rules and config. */

    log::info!("Loading provisioned alert templates...");

    let alertdir = args.provisioned.join(&args.alert_rules);
    let mut alert_rules_provisioned = BTreeSet::new();
    let mut alert_rules = BTreeMap::new();
    let mut alert_configs = BTreeMap::new();
    let mut dir = tokio::fs::read_dir(&alertdir)
        .await
        .map_err(|e| Error::ReadAlertRules(alertdir.clone(), e))?;
    while let Some(ent) = dir
        .next_entry()
        .await
        .map_err(|e| Error::ReadAlertRules(alertdir.clone(), e))?
    {
        if !is_file(ent.path(), &mut Vec::new())
            .await
            .map_err(|e| Error::ReadAlertRules(ent.path(), e))?
        {
            continue;
        }
        let name = ent
            .file_name()
            .into_string()
            .map_err(Error::InvalidAlertRuleFileName)?;
        let Some(rule_name) = name.strip_suffix(".spec.yaml") else {
            continue;
        };
        let id = AlertRuleTemplateName::new(rule_name.to_string());
        let data = tokio::fs::read(ent.path())
            .await
            .map_err(|e| Error::ReadAlertRules(ent.path(), e))?;
        let template = serde_yaml::from_slice::<AlertRuleTemplates>(&data)
            .map_err(|e| Error::DecodeAlertRuleSpecYaml(ent.path(), e))?;
        alert_rules_provisioned.insert(id.clone());
        alert_rules.insert(id.clone(), template);
        alert_configs.insert(id, BTreeMap::new());
    }

    /* Load alert rules. */

    log::info!("Loading alert templates...");

    db.register_table(
        ALERT_RULES_TABLE.clone(),
        DbTable {
            versioning: VersioningType::SingleTimeline,
            schema: AlertRuleTemplates::schema()
                .into_named_field_schema()
                .unwrap(),
            force_update: false,
        },
    )
    .await
    .map_err(Error::Db)?;

    db.query_discovery_objects(ALERT_RULES_TABLE.clone(), dbschema::Filter::All(Vec::new()))
        .await
        .map_err(Error::Db)?
        .into_iter()
        .filter_map(|(object_id, versioned)| {
            let name = AlertRuleTemplateName::new(object_id.to_string());
            match serde_json::from_value::<AlertRuleTemplates>(versioned.value) {
                Ok(template) => Some((name, template)),
                Err(e) => {
                    log::error!("ignoring undecodeable alert template: {name}: {e}");
                    None
                }
            }
        })
        .for_each(|(id, template)| {
            alert_rules.insert(id.clone(), template);
            alert_configs.insert(id, BTreeMap::new());
        });

    /* Load alert config. */

    log::info!("Loading alert config...");

    db.register_table(
        ALERT_CONFIG_TABLE.clone(),
        DbTable {
            versioning: VersioningType::SingleTimeline,
            schema: AlertRuleConfigs::schema()
                .into_named_field_schema()
                .unwrap(),
            force_update: false,
        },
    )
    .await
    .map_err(Error::Db)?;

    db.query_discovery_objects(
        ALERT_CONFIG_TABLE.clone(),
        dbschema::Filter::All(Vec::new()),
    )
    .await
    .map_err(Error::Db)?
    .into_iter()
    .filter_map(|(object_id, versioned)| {
        let (rule_name, config_name) = match object_id.as_str().split_once(":") {
            Some((rule_name, config_name)) => Some((
                AlertRuleTemplateName::new(rule_name.to_string()),
                AlertConfigName::new(config_name.to_string()),
            )),
            None => {
                log::error!("invalid alert rule config id {object_id}");
                None
            }
        }?;
        match alert_rules.get(&rule_name) {
            Some(_) => match serde_json::from_value::<AlertRuleConfigs>(versioned.value) {
                Ok(configs) => Some((rule_name, config_name, configs)),
                Err(e) => {
                    log::error!("ignoring undecodeable alert config: {config_name}: {e}");
                    None
                }
            },
            None => {
                log::error!("ignoring alert config for non-existent rule {rule_name}");
                None
            }
        }
    })
    .for_each(|(rule_name, config_name, config)| {
        if let Some(rule) = alert_configs.get_mut(&rule_name) {
            rule.insert(config_name, config);
        };
    });

    let unverified_alert_rule_forms = UnverifiedAlertRuleForms::load(alert_rules, alert_configs);
    let alert_rules_state = unverified_alert_rule_forms
        .into_iter()
        .filter_map(
            |(name, alert_rule_form)| match alert_rule_form.verify(&name, false) {
                Ok(alert_rule_form) => {
                    let alert_rule_state = AlertRuleState::new(
                        alert_rule_form,
                        alert_rules_provisioned.contains(&name),
                    );
                    Some((name, alert_rule_state))
                }
                Err(e) => {
                    log::error!("Verification failed for rue form {name}: {e}");
                    None
                }
            },
        )
        .collect::<BTreeMap<_, _>>();

    log::info!("Finish loading alert rules and configs");

    /* Load alerts. */

    log::info!("Loading alerts...");

    db.register_table(
        ALERTS_TABLE.clone(),
        DbTable {
            versioning: VersioningType::SingleTimeline,
            schema: AlertDoc::schema().into_named_field_schema().unwrap(),
            force_update: false,
        },
    )
    .await
    .map_err(Error::Db)?;

    let alert_map = db
        .query_discovery_objects(ALERTS_TABLE.clone(), dbschema::Filter::All(Vec::new()))
        .await
        .map_err(Error::Db)?
        .into_iter()
        .map(|(object_id, versioned)| {
            let alert = serde_json::from_value::<AlertDoc>(versioned.value)
                .map_err(Error::DecodeAlertDoc)?;
            Ok((
                object_id,
                SingleVersioned {
                    version: versioned.version,
                    value: alert,
                },
            ))
        })
        .collect::<Result<_>>()?;

    let (alert_map, selfheal_remove) = VersionedAlertMap::new(alert_map);

    if !selfheal_remove.is_empty() {
        log::warn!("Removing ({}) duplicate alert(s)...", selfheal_remove.len());
        db.bulk_update_discovery_objects(
            ALERTS_TABLE.clone(),
            selfheal_remove
                .into_iter()
                .map(|object_id| (object_id, Operation::Remove))
                .collect(),
        )
        .await
        .map_err(Error::Db)?;
    }

    /* Load status. */

    log::info!("Loading status...");

    db.register_table(
        STATUS_TABLE.clone(),
        DbTable {
            versioning: VersioningType::SingleTimeline,
            schema: StatusDoc::dbschema().into_named_field_schema().unwrap(),
            force_update: false,
        },
    )
    .await
    .map_err(Error::Db)?;

    let status_map = db
        .query_discovery_objects(STATUS_TABLE.clone(), dbschema::Filter::All(Vec::new()))
        .await
        .map_err(Error::Db)?
        .into_iter()
        .map(|(object_id, versioned)| {
            let status = serde_json::from_value::<StatusDoc>(versioned.value)
                .map_err(Error::DecodeStatusDoc)?;
            Ok((
                object_id,
                SingleVersioned {
                    version: versioned.version,
                    value: status,
                },
            ))
        })
        .collect::<Result<_>>()?;

    let mut state = State::new(pkgs, types, items)
        .with_alert_rules(AlertRulesState::new(alert_rules_state))
        .with_alert_state(VersionedStatusMap::new(status_map), alert_map)
        .with_view_state(views, dashboards);

    let locks = Locks {
        alert_rules: RwLock::new(
            state
                .alert_rules
                .get_rule_form_names()
                .map(|rule_name| (rule_name.clone(), Arc::new(AsyncMutex::new(()))))
                .collect(),
        ),
    };

    /* Load prometheus schema. */

    log::info!("Loading connection packages...");
    match ConnectionsPackages::load_async(&args.provisioned.join(&args.connections)).await {
        Ok(connections) => {
            log::info!("Loading prometheus schema...");
            match Universe::load_sources_async(&args.provisioned.join(&args.prom_schema)).await {
                Ok((root, modules)) => match state.load_connections(connections, root, modules) {
                    Ok(()) => log::info!("Successfully loaded connections and prometheus schema"),
                    Err(e) => log::warn!("Failed to load connections: {e}"),
                },
                Err(e) => log::warn!("Failed to load prometheus schema: {e}"),
            }
        }
        Err(e) => log::warn!("Failed to load connection packages: {e}"),
    }

    let mut prom_headers = HeaderMap::new();
    if let Some(tenant) = &args.prometheus_tenant {
        prom_headers.insert(
            "X-Scope-OrgID",
            HeaderValue::try_from(tenant).map_err(Error::PromTenantInvalid)?,
        );
    }
    let prom = reqwest::Client::builder()
        .default_headers(prom_headers)
        .build()
        .map_err(Error::PromClientBuild)?;
    let prom = reqwest_middleware::ClientBuilder::new(prom)
        .with(reqwest_tracing::TracingMiddleware::<
            reqwest_tracing::SpanBackendWithUrl,
        >::new())
        .build();

    /* Start http server. */

    let data = Data::new(AppData {
        db,
        es,
        prom,
        es_url: args.elastic_url,
        prom_url: args.prometheus_url,
        ruler_url: args.ruler_url,
        alertmanager_url: args.alertmanager_url,
        state: RwLock::new(state),
        locks,
        app_version: args.app_version,
        prom_schema: PromSchemaState::default(),
    });
    //let appdata = (*data).clone();

    let (term_sender, term_receiver) = tokio::sync::watch::channel(false);

    log::info!("Init alertmanager storage block...");
    if let Err(e) = alerts::update_alertmanager(&data).await {
        log::warn!("failed to init alertmanager storage block: {e}");
    }

    log::info!("Synchronizing alert rules...");

    if let Err(e) = alerts::update_ruler(EditorRole::service(), &data).await {
        log::warn!("failed to synchronize alert rules: {e}");
    }

    log::info!("Starting status runner...");

    let status_runner = tokio::spawn(status::status_runner(
        EditorRole::service(),
        data.clone(),
        std::time::Duration::from_secs(60),
        term_receiver.clone(),
    ));

    log::info!("Starting connector runner...");

    let connector_runner = tokio::spawn(connector::connector_runner(
        EditorRole::service(),
        data.clone(),
        std::time::Duration::from_secs(60),
        term_receiver,
    ));

    log::info!("Starting http server...");

    HttpServer::new(move || http_app(&args.prefix, Some(&data)).0)
        .bind_auto_h2c(&args.bind)
        .map_err(Error::Bind)?
        .run()
        .await
        .map_err(Error::Server)?;

    if let Err(e) = term_sender.send(true) {
        log::warn!("failed to send termination signal: {e}");
    }

    if let Err(e) = status_runner.await {
        log::warn!("failed to join status writer: {e}");
    }

    if let Err(e) = connector_runner.await {
        log::warn!("failed to join connector runner: {e}");
    }

    // let appdata = Arc::into_inner(appdata).unwrap().into_inner();
    // state.save(state_path).await

    log::info!("Http server stopped");

    Ok(())
}

fn http_response<T: Serialize>(response: Result<T>) -> HttpResponse {
    match response {
        Ok(res) => HttpResponseBuilder::new(StatusCode::OK).json(res),
        Err(e) => HttpResponseBuilder::new(e.http_status_code()).json(e.to_string()),
    }
}

async fn run_updates(data: &AppData, updates: Updates) -> Result<()> {
    if updates.0.is_empty() {
        Ok(())
    } else {
        data.db
            .bulk_update_discovery_objects(
                ITEMS_TABLE.clone(),
                updates
                    .0
                    .into_iter()
                    .map(|(id, value)| {
                        let operation = value.map_or(Operation::Remove, |value| {
                            Operation::CreateOrUpdate(serde_json::to_value(value).unwrap())
                        });
                        (id, operation)
                    })
                    .collect(),
            )
            .await
            .map_err(Error::Db)
    }
}

#[async_recursion]
async fn is_file(
    entry_path: PathBuf,
    visited: &mut Vec<PathBuf>,
) -> std::result::Result<bool, tokio::io::Error> {
    if visited.contains(&entry_path) {
        return Ok(false);
    }
    let metadata = tokio::fs::metadata(&entry_path).await?;
    if metadata.is_file() {
        return Ok(true);
    }
    if metadata.is_symlink() {
        visited.push(entry_path.clone());
        let symlink_path = entry_path.read_link()?;
        let symlink_metadata = tokio::fs::metadata(&symlink_path).await?;
        if symlink_metadata.is_file() {
            return Ok(true);
        }
        if symlink_metadata.is_symlink() {
            return is_file(symlink_path, visited).await;
        }
    }
    Ok(false)
}
