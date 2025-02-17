/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use actix_web::{web::Data, HttpResponse};
use apistos::{
    api_operation,
    web::{get, Resource, Scope},
};
use tracing::instrument;

use crate::{auth::Auth, http_response, roles::ViewerRole, AppData};

pub(crate) fn service() -> Scope {
    Scope::new("/version").service(Resource::new("").route(get().to(get_version)))
}

// #[get("")]
#[instrument]
#[api_operation(summary = "Get the application version")]
async fn get_version(_role: Auth<ViewerRole>, data: Data<AppData>) -> HttpResponse {
    http_response(Ok(&data.app_version))
}
