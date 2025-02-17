/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use actix_web::{
    web::{Data, Json},
    HttpResponse,
};
use apistos::{
    api_operation,
    web::{post, Resource, Scope},
};
use relation_graph::db::DbItem;
use tracing::instrument;

use crate::{
    auth::Auth,
    events::{run_bins_query, run_events_query, BinnedEvents, BinsParams},
    roles::ViewerRole,
    Result,
};
use crate::{http_response, AppData, ITEMS_TABLE};

pub(crate) fn service() -> Scope {
    Scope::new("/changes")
        .service(Resource::new("/bins").route(post().to(post_bins)))
        .service(Resource::new("/events").route(post().to(post_events)))
}

// #[post("/bins")]
#[instrument]
#[api_operation(summary = "Get binned changes")]
async fn post_bins(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<BinsParams>,
) -> HttpResponse {
    let data = data.into_inner();
    let params = params.into_inner();
    http_response(run_bins_query(role.into_inner(), &data, params, &ITEMS_TABLE).await)
}

// #[post("/events")]
#[instrument]
#[api_operation(summary = "Get individual changes")]
async fn post_events(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<BinsParams>,
) -> Result<Json<BinnedEvents<DbItem>>> {
    let data = data.into_inner();
    let params = params.into_inner();
    let res = run_events_query::<DbItem, _>(
        role.into_inner(),
        &data,
        params,
        ITEMS_TABLE.clone(),
        |_| true,
    )
    .await?;
    Ok(Json(res))
}
