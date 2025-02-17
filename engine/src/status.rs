/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use actix_web::{
    web::{Data, Json, Path},
    HttpResponse,
};
use apistos::{
    api_operation,
    web::{get, post, Resource, Scope},
};
use chrono::{DateTime, Utc};
use dbdaemon_types::Operation;
use dbschema::SingleVersioned;
use relation_graph::{
    alerts::{AlertMap, PromAlerts},
    status::{Status, StatusDoc, StatusMap, VersionedStatusMap},
};
use serde::Deserialize;
use tracing::{instrument, span, Instrument, Level};

use crate::{
    auth::Auth,
    events::{run_bins_query, run_events_query, BinnedEvents, BinsParams, EventType},
    roles::{EditorRole, ViewerRole},
};
use crate::{http_response, AppData, Error, Result, ALERTS_TABLE, STATUS_TABLE};

pub(crate) fn service() -> Scope {
    Scope::new("/status")
        .service(Resource::new("").route(get().to(get_status)))
        .service(Resource::new("/bins").route(post().to(post_bins)))
        .service(Resource::new("/events").route(post().to(post_events)))
        .service(Resource::new("/{time}").route(get().to(get_status_at)))
    // .service(get_item_status)
    // .service(get_relation_status)
    // .service(get_item_status_at)
    // .service(get_relation_status_at)
}

/* Status. */

// #[get("")]
#[instrument]
#[api_operation(summary = "Get status map")]
async fn get_status(role: Auth<ViewerRole>, data: Data<AppData>) -> HttpResponse {
    http_response(run_get_status(role.into_inner(), &data, None).await)
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct GetStatusAt(DateTime<Utc>);

// #[get("{time}")]
#[instrument]
#[api_operation(summary = "Get status map at a point in time")]
async fn get_status_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<GetStatusAt>,
) -> HttpResponse {
    http_response(run_get_status(role.into_inner(), &data, Some(path.into_inner().0)).await)
}

// #[post("/bins")]
#[instrument]
#[api_operation(summary = "Query status change bins")]
async fn post_bins(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<BinsParams>,
) -> HttpResponse {
    let data = data.into_inner();
    let params = params.into_inner();
    http_response(run_bins_query(role.into_inner(), &data, params, &STATUS_TABLE).await)
}

// #[post("/events")]
#[instrument]
#[api_operation(summary = "Query status changes")]
async fn post_events(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<BinsParams>,
) -> Result<Json<BinnedEvents<StatusDoc>>> {
    let data = data.into_inner();
    let params = params.into_inner();
    let res = run_events_query::<StatusDoc, _>(
        role.into_inner(),
        &data,
        params,
        STATUS_TABLE.clone(),
        |event| match event.change_type {
            EventType::Added | EventType::Modified => event
                .value
                .as_ref()
                .is_some_and(|v| v.change.previous.is_some() || v.change.status > Status::Ok),
            EventType::Removed => event
                .value
                .as_ref()
                .is_some_and(|v| v.change.status > Status::Ok),
        },
    )
    .await?;
    Ok(Json(res))
}

async fn run_get_status(
    role: ViewerRole,
    data: &AppData,
    time: Option<DateTime<Utc>>,
    //items: Option<BTreeSet<Uuid>>,
) -> Result<VersionedStatusMap> {
    match time {
        None => {
            let state = data.read_state(role);
            Ok(state.status.clone())
        }
        Some(t) => {
            let items = data
                .db
                .query_discovery_objects_at(
                    STATUS_TABLE.clone(),
                    dbschema::Filter::All(Vec::new()),
                    t,
                )
                .await
                .map_err(Error::Db)?;
            let status_map = items
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
            Ok(VersionedStatusMap::new(status_map))
        }
    }
}

pub(crate) async fn status_runner(
    role: EditorRole,
    data: Data<AppData>,
    interval: std::time::Duration,
    mut term_receiver: tokio::sync::watch::Receiver<bool>,
) {
    let mut interval = tokio::time::interval(interval);
    interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

    loop {
        tokio::select! {
            _ = term_receiver.changed() => if *term_receiver.borrow() {
                break
            },
            _ = interval.tick() => async {
                log::info!("running status update...");
                match update_status(role, &data).await {
                    Ok((nstatus,nalerts)) => {
                        log::info!("succesfully updated {nstatus} statusses, {nalerts} alerts");
                    },
                    Err(e) => {
                        log::warn!("failed to update status: {e}");
                    }
                }
            }.instrument(span!(Level::INFO, "updating status")).await
        }
    }
}

#[instrument]
async fn update_status(role: EditorRole, data: &AppData) -> Result<(usize, usize)> {
    let (status, alerts) = get_status_map(role.into(), data).await?;
    let now = chrono::Utc::now();
    let (status_updates, alert_updates) = {
        let mut state = data.write_state(role);
        (
            state.status.update(now, status),
            state.alerts.update(now, alerts),
        )
    };

    let nstatus = status_updates.len();
    let nalerts = alert_updates.len();

    data.db
        .bulk_update_discovery_objects(
            ALERTS_TABLE.clone(),
            alert_updates
                .into_iter()
                .map(|(object_id, change)| {
                    (
                        object_id,
                        match change {
                            Some(change) => {
                                Operation::CreateOrUpdate(serde_json::to_value(change).unwrap())
                            }
                            None => Operation::Remove,
                        },
                    )
                })
                .collect(),
        )
        .await
        .map_err(Error::Db)?;

    data.db
        .bulk_update_discovery_objects(
            STATUS_TABLE.clone(),
            status_updates
                .into_iter()
                .map(|(object_id, change)| {
                    (
                        object_id,
                        match change {
                            Some(change) => {
                                Operation::CreateOrUpdate(serde_json::to_value(change).unwrap())
                            }
                            None => Operation::Remove,
                        },
                    )
                })
                .collect(),
        )
        .await
        .map_err(Error::Db)?;

    Ok((nstatus, nalerts))
}

async fn get_status_map(role: ViewerRole, data: &AppData) -> Result<(StatusMap, AlertMap)> {
    let res = data
        .prom
        .get(data.ruler_url.join("api/prom/api/v1/alerts")?)
        .send()
        .await
        .map_err(Error::PromRequestMw)?
        .error_for_status()
        .map_err(Error::PromRequest)?
        .json::<prometheus_api::Response<PromAlerts>>()
        .await
        .map_err(Error::PromDecode)?
        .into_result()
        .map_err(Error::Prometheus)?;

    // log::debug!(
    //     "read alerts: {}",
    //     serde_json::to_string_pretty(&res.data).unwrap()
    // );
    let alerts = res.data.firing_alerts().collect::<Vec<_>>();
    log::debug!("read {} firing alerts", alerts.len());

    let state = data.read_state(role);
    let alert_map = AlertMap::from_alerts(&state, alerts);
    let status_map = StatusMap::from_alert_map(&state, &alert_map);

    Ok((status_map, alert_map))
}
