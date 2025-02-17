/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use actix_web::{
    web::{Data, Json, Path},
    HttpResponse,
};
use apistos::{
    api_operation,
    web::{get, put, Resource, Scope},
};
use tracing::instrument;

use crate::{
    auth::Auth,
    http_response,
    roles::{EditorRole, ViewerRole},
    AppData, Error,
};

use relation_graph::{View, ViewId, Views};

pub(crate) fn service() -> Scope {
    Scope::new("/views")
        .service(
            Resource::new("")
                .route(get().to(get_views))
                .route(put().to(put_views)),
        )
        .service(
            Resource::new("/{view}")
                .route(get().to(get_view))
                .route(put().to(put_view)),
        )
}

// #[get("")]
#[instrument]
#[api_operation(summary = "Get list of views")]
async fn get_views(role: Auth<ViewerRole>, data: Data<AppData>) -> HttpResponse {
    http_response(Ok(&data.read_state(role.into_inner()).views))
}

// #[get("/{view}")]
#[instrument]
#[api_operation(summary = "Get a view definition")]
async fn get_view(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    view_id: Path<ViewId>,
) -> HttpResponse {
    http_response(
        data.read_state(role.into_inner())
            .views
            .get(view_id.as_ref())
            .ok_or_else(|| Error::ViewNotFound(view_id.as_ref().clone())),
    )
}

// #[put("")]
#[instrument(skip(views))]
#[api_operation(summary = "Update view definitions")]
async fn put_views(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    views: Json<Views>,
) -> HttpResponse {
    data.write_state(role.into_inner()).views = views.into_inner();
    http_response(Ok(()))
}

// #[put("/{view}")]
#[instrument(skip(view))]
#[api_operation(summary = "Create or update a view definition")]
async fn put_view(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    view_id: Path<ViewId>,
    view: Json<View>,
) -> HttpResponse {
    let _ = data
        .write_state(role.into_inner())
        .views
        .insert(view_id.into_inner(), view.into_inner());
    http_response(Ok(()))
}
