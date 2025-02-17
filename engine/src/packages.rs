/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, fmt::Display, sync::Arc};

use actix_web::web::{Data, Json, Path};

use apistos::{
    api_operation,
    web::{delete, get, put, Resource, Scope},
};
use dbschema::{DbTable, ObjectId, VersioningType};
use serde::Serialize;
use serde_with::SerializeDisplay;
use tracing::instrument;

use relation_graph::{
    db::DbItem, ConnectionsPackage, ConnectionsPackageId, Package, PackageData, PackageId,
    PackageVersion,
};

use crate::{
    auth::Auth,
    roles::{AdminRole, EditorRole, ViewerRole},
    utils::{bitcode::Bitcode, json::JsonFile},
    AppData, Error, PackageDoc, Result, ITEMS_TABLE, PKGS_TABLE,
};

pub(crate) fn packages_svc() -> Scope {
    Scope::new("/packages").service(
        Resource::new("")
            .name("get_packages")
            .route(get().to(get_packages)),
    )
    // .service(get_packages)
}

pub(crate) fn package_svc() -> Scope {
    Scope::new("/package")
        .service(Resource::new("").route(get().to(list_packages)))
        .service(
            Resource::new("/{package}")
                .route(get().to(get_package))
                .route(put().to(put_package))
                .route(delete().to(delete_package)),
        )
    // .service(Resource::new("/{package}/info").route(get().to(get_package_info)))
}

pub(crate) fn connections_package_svc() -> Scope {
    Scope::new("/connections-package")
        .service(Resource::new("").route(get().to(list_connections_packages)))
        .service(
            Resource::new("/{package}")
                .route(get().to(get_connections_package))
                .route(put().to(put_connections_package))
                .route(delete().to(delete_connections_package)),
        )
}

//#[get("")]
#[instrument]
#[api_operation(summary = "Download all loaded packages")]
async fn get_packages(role: Auth<ViewerRole>, data: Data<AppData>) -> Result<Bitcode<PackageData>> {
    let data = data.read_state(role.into_inner()).package_data();
    Ok(Bitcode(data))
}

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent)]
struct PackageInfo(BTreeMap<PackageId, PackageVersion>);

//#[get("")]
#[instrument]
#[api_operation(summary = "List loaded packages")]
async fn list_packages(role: Auth<ViewerRole>, data: Data<AppData>) -> Json<PackageInfo> {
    Json(PackageInfo(
        data.read_state(role.into_inner())
            .packages
            .iter()
            .map(|(id, pkg)| (id.clone(), pkg.version.clone()))
            .collect::<BTreeMap<_, _>>(),
    ))
}

// #[get("/{package}")]
#[instrument]
#[api_operation(summary = "Download a single package")]
async fn get_package(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    pkg_id: Path<PackageId>,
) -> Result<JsonFile<Package>> {
    Ok(JsonFile(
        format!("{pkg_id}.json"),
        data.read_state(role.into_inner())
            .packages
            .get(pkg_id.as_ref())
            .ok_or_else(|| Error::PackageNotFound(pkg_id.as_ref().clone()))?
            .clone(),
    ))
}

// #[get("/{package}/info")]
// async fn get_package_info(data: Data<AppData>, pkg_id: Path<PackageId>) -> HttpResponse {
//     let state = data.state.read();
//     match state.types.packages.get(pkg_id.as_ref()) {
//         Some(pkg) => HttpResponseBuilder::new(StatusCode::OK).json(pkg),
//         None => HttpResponseBuilder::new(StatusCode::NOT_FOUND).json(json!("not found")),
//     }
// }

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent)]
struct ConnectionsPackageInfo(BTreeMap<ConnectionsPackageId, PackageVersion>);

//#[get("")]
#[instrument]
#[api_operation(summary = "List loaded connections packages")]
async fn list_connections_packages(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
) -> Json<ConnectionsPackageInfo> {
    Json(ConnectionsPackageInfo(
        data.read_state(role.into_inner())
            .connections
            .iter()
            .flat_map(|pkgs| pkgs.0.iter())
            .map(|(id, pkg)| (id.clone(), pkg.version.clone()))
            .collect::<BTreeMap<_, _>>(),
    ))
}

// #[get("/{package}")]
#[instrument]
#[api_operation(summary = "Download a single connections package")]
async fn get_connections_package(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    pkg_id: Path<ConnectionsPackageId>,
) -> Result<JsonFile<ConnectionsPackage>> {
    Ok(JsonFile(
        format!("{pkg_id}.json"),
        data.read_state(role.into_inner())
            .connections
            .as_ref()
            .and_then(|pkgs| pkgs.0.get(pkg_id.as_ref()))
            .ok_or_else(|| Error::ConnectionsPackageNotFound(pkg_id.as_ref().clone()))?
            .clone(),
    ))
}

#[derive(SerializeDisplay, schemars::JsonSchema, apistos::ApiComponent)]
struct Done;

impl Display for Done {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "done")
    }
}

//#[put("/{package}")]
#[instrument]
#[api_operation(summary = "Load a new package")]
async fn put_package(
    role: Auth<AdminRole>,
    data: Data<AppData>,
    pkg_id: Path<PackageId>,
    pkg: Json<Package>,
) -> Result<Json<Done>> {
    async fn run(
        role: AdminRole,
        data: Arc<AppData>,
        pkg_id: PackageId,
        pkg: Package,
    ) -> Result<()> {
        let new_items_schema = {
            let mut state = data.write_state(role.into());
            state.load_package(pkg_id.clone(), pkg.clone())?;
            DbItem::schema(&state.types)
        };

        data.db
            .create_or_update_discovery_object(
                PKGS_TABLE.clone(),
                ObjectId::from(pkg_id.to_string()),
                serde_json::to_value(PackageDoc { package: pkg }).unwrap(),
            )
            .await
            .map_err(Error::Db)?;

        data.db
            .register_table(
                ITEMS_TABLE.clone(),
                DbTable {
                    versioning: VersioningType::SingleTimeline,
                    schema: new_items_schema.into_named_field_schema().unwrap(),
                    force_update: false,
                },
            )
            .await
            .map_err(Error::Db)?;

        Ok(())
    }

    run(
        role.into_inner(),
        data.into_inner(),
        pkg_id.into_inner(),
        pkg.into_inner(),
    )
    .await?;
    Ok(Json(Done))
}

// Note: unloading packages is not acceptible due to schema
// incompatibilities ("unlisted removed fields" in properties).
//#[delete("/{package}")]
#[instrument]
#[api_operation(summary = "Delete a package")]
async fn delete_package(
    role: Auth<AdminRole>,
    data: Data<AppData>,
    pkg_id: Path<PackageId>,
) -> Result<Json<Done>> {
    async fn run(role: AdminRole, data: Arc<AppData>, pkg_id: PackageId) -> Result<()> {
        let new_items_schema = {
            let mut state = data.write_state(role.into());
            state.unload_package(&pkg_id)?;
            DbItem::schema(&state.types)
        };

        data.db
            .remove_discovery_object(PKGS_TABLE.clone(), ObjectId::from(pkg_id.to_string()))
            .await
            .map_err(Error::Db)?;

        data.db
            .register_table(
                ITEMS_TABLE.clone(),
                DbTable {
                    versioning: VersioningType::SingleTimeline,
                    schema: new_items_schema.into_named_field_schema().unwrap(),
                    force_update: false,
                },
            )
            .await
            .map_err(Error::Db)?;

        Ok(())
    }

    run(role.into_inner(), data.into_inner(), pkg_id.into_inner()).await?;
    Ok(Json(Done))
}

//#[put("/{package}")]
#[instrument]
#[api_operation(summary = "Create or update a connections package")]
async fn put_connections_package(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    pkg_id: Path<ConnectionsPackageId>,
    pkg: Json<ConnectionsPackage>,
) -> Result<Json<Done>> {
    let mut state = data.write_state(role.into_inner());
    state.update_connections(pkg_id.clone(), pkg.clone())?;
    Ok(Json(Done))
}

//#[delete("/{package}")]
#[instrument]
#[api_operation(summary = "Remove a connections package")]
async fn delete_connections_package(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    pkg_id: Path<ConnectionsPackageId>,
) -> Result<Json<Done>> {
    let mut state = data.write_state(role.into_inner());
    state.remove_connections(&pkg_id)?;
    Ok(Json(Done))
}
