/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use actix_web::{
    web::{Data, Json, Path, Query},
    HttpResponse,
};

use apistos::{
    api_operation,
    web::{post, Resource, Scope},
};
use chrono::{DateTime, TimeDelta, Utc};
use dbdaemon_api::DbClient;
use dbschema::SingleVersioned;
use futures::StreamExt;
use prometheus_api::InstantQueryParams;
use prometheus_schema::MetricSelector;
use relation_graph::{
    alerts::{AlertDoc, VersionedAlertMap},
    db::{DbItem, DbItems},
    serial::{self, Elements},
    status::{StatusDoc, VersionedStatusMap},
    Augment, InfoQueryMetrics, InfoQueryParams, State,
};
use serde::Deserialize;
use tracing::instrument;

use crate::{
    auth::Auth, http_response, roles::ViewerRole, utils::bitcode::Bitcode, AppData, Error, Result,
    ALERTS_TABLE, ITEMS_TABLE, STATUS_TABLE,
};

pub(crate) fn query_svc() -> Scope {
    Scope::new("/query")
        .service(Resource::new("").route(post().to(post_query)))
        .service(Resource::new("/{time}").route(post().to(post_query_at)))
}

pub(crate) fn info_query_svc() -> Scope {
    Scope::new("/info-query")
        .service(Resource::new("/metrics").route(post().to(post_info_query_metrics)))
}

pub(crate) fn search_domain_svc() -> Scope {
    Scope::new("/search-domain")
        .service(Resource::new("").route(post().to(post_search_domain)))
        .service(Resource::new("/{time}").route(post().to(post_search_domain_at)))
}

pub(crate) fn elements_svc() -> Scope {
    Scope::new("/elements")
        .service(Resource::new("").route(post().to(post_elements)))
        .service(Resource::new("/{time}").route(post().to(post_elements_at)))
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct Timestamp(DateTime<Utc>);

// #[post("")]
#[instrument]
#[api_operation(summary = "Run a query")]
async fn post_query(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    query: Json<serial::Query>,
) -> HttpResponse {
    let state = data.read_state(role.into_inner());
    http_response(run_query(&state, query.into_inner()))
}

// #[post("/{timestamp}")]
#[instrument]
#[api_operation(summary = "Run a query at a point in time")]
async fn post_query_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    query: Json<serial::Query>,
    timestamp: Path<Timestamp>,
) -> HttpResponse {
    http_response(
        run_query_at(
            role.into_inner(),
            &data,
            query.into_inner(),
            timestamp.into_inner().0,
        )
        .await,
    )
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
struct InfoQueryBody {
    query: serial::InfoQuery,
    params: InfoQueryParams,
}

// #[post("/metrics")]
#[instrument]
#[api_operation(summary = "Get info query metrics")]
async fn post_info_query_metrics(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    query: Json<InfoQueryBody>,
) -> HttpResponse {
    let InfoQueryBody { query, params } = query.into_inner();
    http_response(run_info_query_metrics(role.into_inner(), &data, query, params).await)
}

// #[post("")]
#[instrument]
#[api_operation(summary = "Run a search-domain query")]
async fn post_search_domain(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    query: Json<serial::Query>,
) -> HttpResponse {
    let state = data.read_state(role.into_inner());
    http_response(run_search_domain(&state, query.into_inner(), false))
}

// #[post("/{timestamp}")]
#[instrument]
#[api_operation(summary = "Run a search-domain query at a point in time")]
async fn post_search_domain_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    query: Json<serial::Query>,
    timestamp: Path<Timestamp>,
) -> HttpResponse {
    http_response(
        run_search_domain_at(
            role.into_inner(),
            &data,
            query.into_inner(),
            timestamp.into_inner().0,
            false,
        )
        .await,
    )
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct ElementsOptions {
    include_aggregated_status: bool,
}

// #[post("")]
#[instrument]
#[api_operation(summary = "Get element types")]
async fn post_elements(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    options: Query<ElementsOptions>,
    elements: Json<serial::Elements>,
) -> Result<Bitcode<serial::Items<Augment<serial::StatusInfo>>>> {
    let state = data.read_state(role.into_inner());
    run_elements(
        &state,
        elements.into_inner(),
        options.include_aggregated_status,
    )
    .map(Bitcode)
}

// #[post("/{timestamp}")]
#[instrument]
#[api_operation(summary = "Get element types at a point in time")]
async fn post_elements_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    options: Query<ElementsOptions>,
    elements: Json<serial::Elements>,
    timestamp: Path<Timestamp>,
) -> Result<Bitcode<serial::Items<Augment<serial::StatusInfo>>>> {
    run_elements_at(
        role.into_inner(),
        &data,
        elements.into_inner(),
        timestamp.into_inner().0,
        options.include_aggregated_status,
    )
    .await
    .map(Bitcode)
}

fn run_query(state: &State, query: serial::Query) -> Result<serial::QueryResult> {
    Ok(query
        .resolve(&state.types, None)?
        .run(&state.items.items, &state.types)
        .to_serial_unwrapped(&state.items.items, None))
}

async fn run_info_query_metrics(
    role: ViewerRole,
    data: &AppData,
    query: serial::InfoQuery,
    params: InfoQueryParams,
) -> Result<InfoQueryMetrics> {
    let now = Utc::now() - TimeDelta::minutes(1);
    let params = InfoQueryParams {
        timestamp: params.timestamp.min(now),
        from: params.from.min(now),
        to: params.to.min(now),
    };
    let exprs = {
        let state = data.read_state(role);
        let schema = state.get_prometheus_schema()?;
        query
            .resolve(&state.types)?
            .exprs(schema, &params, &MetricSelector::new())
    };

    let metrics = futures::stream::iter(exprs)
        .map(|(expr_name, expr)| async move {
            let metrics = match expr {
                Ok(expr) => super::metrics::run_query(
                    role,
                    data,
                    expr.to_string(),
                    InstantQueryParams {
                        time: Some(params.timestamp),
                    },
                )
                .await
                .map_err(|e| e.to_string()),
                Err(e) => Err(e),
            };
            (expr_name.clone(), metrics)
        })
        .buffer_unordered(15)
        .collect()
        .await;

    Ok(InfoQueryMetrics(metrics))
}

fn run_search_domain(
    state: &State,
    query: serial::Query,
    aggregated_status: bool,
) -> Result<serial::Items<Augment<serial::StatusInfo>>> {
    Ok(query
        .resolve(&state.types, None)?
        .search_domain(&state.items.items, &state.types)
        .to_serial_unwrapped(&state.items.items, None)
        .augment_status(&state.status, &state.alerts, aggregated_status))
}

fn run_elements(
    state: &State,
    elements: Elements,
    aggregated_status: bool,
) -> Result<serial::Items<Augment<serial::StatusInfo>>> {
    run_search_domain(
        state,
        serial::Query::from_elements(elements)?,
        aggregated_status,
    )
}

async fn run_elements_at(
    role: ViewerRole,
    data: &AppData,
    elements: Elements,
    timestamp: DateTime<Utc>,
    aggregated_status: bool,
) -> Result<serial::Items<Augment<serial::StatusInfo>>> {
    run_search_domain_at(
        role,
        data,
        serial::Query::from_elements(elements)?,
        timestamp,
        aggregated_status,
    )
    .await
}

// async fn get_types_at(db: &DbClient, timestamp: DateTime<Utc>) -> Result<Types> {
//     Ok(db
//         .query_discovery_objects_at(
//             PKGS_TABLE.clone(),
//             dbschema::Filter::All(Vec::new()),
//             timestamp,
//         )
//         .await
//         .map_err(Error::Db)?
//         .into_iter()
//         .try_fold(Packages::new(), |mut pkgs, (object_id, versioned)| {
//             let pkg_id = PackageId::from_str(object_id.as_str())?;
//             let pkg = serde_json::from_value::<PackageDoc>(versioned.value)
//                 .map_err(|e| Error::DecodePackage(pkg_id.clone(), e))?;
//             pkgs.insert(pkg_id, pkg.package);
//             Result::Ok(pkgs)
//         })?
//         .types()?)
// }

#[instrument(skip(db))]
async fn get_items_at(
    db: &DbClient,
    timestamp: DateTime<Utc>,
    filter: dbschema::Filter,
) -> Result<DbItems> {
    let data = db
        .query_discovery_objects_at(ITEMS_TABLE.clone(), filter, timestamp)
        .await
        .map_err(Error::Db)?;
    let _guard = tracing::span!(tracing::Level::INFO, "processing", n_items = data.len()).entered();
    data.into_iter()
        .map(|(object_id, versioned)| {
            let dbitem = serde_json::from_value::<DbItem>(versioned.value)
                .map_err(|e| Error::DecodeItem(object_id.clone(), e))?;
            Ok((object_id, dbitem))
        })
        .collect::<Result<DbItems>>()
}

#[instrument(skip(db))]
async fn get_status_at(
    db: &DbClient,
    timestamp: DateTime<Utc>,
    filter: dbschema::Filter,
) -> Result<VersionedStatusMap> {
    let status = db
        .query_discovery_objects_at(STATUS_TABLE.clone(), filter, timestamp)
        .await
        .map_err(Error::Db)?
        .into_iter()
        .map(|(object_id, versioned)| {
            let value = serde_json::from_value::<StatusDoc>(versioned.value)
                .map_err(Error::DecodeStatusDoc)?;
            Ok((
                object_id,
                SingleVersioned {
                    version: versioned.version,
                    value,
                },
            ))
        })
        .collect::<Result<_>>()?;
    Ok(VersionedStatusMap::new(status))
}

#[instrument(skip(db))]
async fn get_alerts_at(
    db: &DbClient,
    timestamp: DateTime<Utc>,
    filter: dbschema::Filter,
) -> Result<VersionedAlertMap> {
    let alerts = db
        .query_discovery_objects_at(ALERTS_TABLE.clone(), filter, timestamp)
        .await
        .map_err(Error::Db)?
        .into_iter()
        .map(|(object_id, versioned)| {
            let value = serde_json::from_value::<AlertDoc>(versioned.value)
                .map_err(Error::DecodeStatusDoc)?;
            Ok((
                object_id,
                SingleVersioned {
                    version: versioned.version,
                    value,
                },
            ))
        })
        .collect::<Result<_>>()?;
    Ok(VersionedAlertMap::new(alerts).0)
}

#[instrument(skip(data))]
async fn run_query_at(
    role: ViewerRole,
    data: &AppData,
    query: serial::Query,
    timestamp: DateTime<Utc>,
) -> Result<serial::QueryResult> {
    if timestamp >= Utc::now() {
        let state = data.read_state(role);
        run_query(&state, query)
    } else {
        let filter = {
            let state = data.read_state(role);
            let query = query.clone().resolve(&state.types, None)?;
            query.elastic_query(&state.types).into_filter()
        };
        let items = get_items_at(&data.db, timestamp, filter).await?;
        let state = data.read_state(role);
        let items = items.resolve(&state.types)?;
        Ok(
            query // Note: need to resolve again since we dropped the lock above!
                .resolve(&state.types, None)?
                .run(&items, &state.types)
                .to_serial_unwrapped(&items, None),
        )
    }
}

#[instrument(skip(data))]
async fn run_search_domain_at(
    role: ViewerRole,
    data: &AppData,
    query: serial::Query,
    timestamp: DateTime<Utc>,
    aggregated_status: bool,
) -> Result<serial::Items<Augment<serial::StatusInfo>>> {
    if timestamp >= Utc::now() {
        let state = data.read_state(role);
        run_search_domain(&state, query, aggregated_status)
    } else {
        let filter = {
            let state = data.read_state(role);
            let query = query.clone().resolve(&state.types, None)?;
            query.elastic_query(&state.types).into_filter()
            // dbschema::Filter::All(Vec::new())
        };
        let items = get_items_at(&data.db, timestamp, filter.clone())
            .await?
            .cleanup_partial();
        let status = get_status_at(&data.db, timestamp, filter.clone()).await?;
        let alerts = get_alerts_at(&data.db, timestamp, filter).await?;
        let state = data.read_state(role);
        let items = items.resolve(&state.types)?;
        Ok(query
            .resolve(&state.types, None)?
            .search_domain(&items, &state.types)
            .to_serial_unwrapped(&items, None)
            .augment_status(&status, &alerts, aggregated_status))
    }
}
