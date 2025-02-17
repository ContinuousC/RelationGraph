/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use actix_web::{
    web::{Data, Path},
    HttpResponse,
};
use apistos::{
    api_operation,
    web::{get, Resource, Scope},
};
use tracing::instrument;

use crate::{auth::Auth, http_response, roles::ViewerRole, AppData, Error};

use relation_graph::DashboardId;

pub(crate) fn service() -> Scope {
    Scope::new("/dashboards")
        .service(Resource::new("").route(get().to(get_dashboards)))
        .service(Resource::new("/{dashboard}").route(get().to(get_dashboard)))
}

#[instrument]
#[api_operation(summary = "Get list of dashboards")]
async fn get_dashboards(role: Auth<ViewerRole>, data: Data<AppData>) -> HttpResponse {
    http_response(Ok(&data.read_state(role.into_inner()).dashboards))
}

#[instrument]
#[api_operation(summary = "Get a dashboard definition")]
async fn get_dashboard(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    dashboard_id: Path<DashboardId>,
) -> HttpResponse {
    http_response(
        data.read_state(role.into_inner())
            .dashboards
            .get(dashboard_id.as_ref())
            .ok_or_else(|| Error::DashboardNotFound(dashboard_id.as_ref().clone())),
    )
}
