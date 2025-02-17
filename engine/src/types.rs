/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeSet;

use actix_web::{
    web::{Data, Json, Path},
    HttpResponse,
};

use apistos::{
    api_operation,
    web::{get, post, Resource, Scope},
};
use serde::{Deserialize, Serialize};

use relation_graph::{
    Absolute, ItemTypeId, ItemTypeInfo, PackageId, PackageType, PropertyId, PropertyTypeInfo,
    RelationTypeId, RelationTypeInfo,
};
use tracing::instrument;

use crate::{auth::Auth, http_response, roles::ViewerRole, AppData, Error, Result};

pub(crate) fn service() -> Scope {
    Scope::new("/types")
        /* Item type info. */
        .service(Resource::new("/item").route(get().to(list_item_types)))
        .service(Resource::new("/item/{package}").route(get().to(list_pkg_item_types)))
        .service(Resource::new("/item/{package}/{type}").route(get().to(get_item_type)))
        /* Relations info. */
        .service(Resource::new("/relation").route(get().to(list_relation_types)))
        .service(Resource::new("/relation/{package}").route(get().to(list_pkg_relation_types)))
        .service(Resource::new("/relation/{package}/{type}").route(get().to(get_relation_type)))
        /* Property info. */
        .service(Resource::new("/property").route(get().to(list_property_types)))
        .service(Resource::new("/property/{package}").route(get().to(list_pkg_property_types)))
        .service(Resource::new("/property/{package}/{type}").route(get().to(get_property_type)))
        /* Selector context info. */
        .service(Resource::new("/selector").route(post().to(post_selector_context_info)))
}

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent)]
struct AbsItemTypeIds(BTreeSet<Absolute<ItemTypeId>>);

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent)]
struct ItemTypeIds(BTreeSet<ItemTypeId>);

//#[get("/item")]
#[instrument]
#[api_operation(summary = "List item types")]
async fn list_item_types(role: Auth<ViewerRole>, data: Data<AppData>) -> Json<AbsItemTypeIds> {
    Json(AbsItemTypeIds(
        data.read_state(role.into_inner())
            .types
            .items
            .keys()
            .cloned()
            .collect(),
    ))
}

// #[get("/item/{package}")]
#[instrument]
#[api_operation(summary = "List item types for a package")]
async fn list_pkg_item_types(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    pkg: Path<PackageId>,
) -> Result<Json<ItemTypeIds>> {
    let pkg = pkg.into_inner();
    Ok(Json(ItemTypeIds(
        data.read_state(role.into_inner())
            .packages
            .get(&pkg)
            .ok_or_else(|| Error::PackageNotFound(pkg.clone()))?
            .items
            .keys()
            .cloned()
            .collect(),
    )))
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
struct ItemTypePath {
    package: PackageId,
    r#type: ItemTypeId,
}

// #[get("/item/{package}/{type}")]
#[instrument]
#[api_operation(summary = "Get info on an item type")]
async fn get_item_type(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ItemTypePath>,
) -> Result<Json<ItemTypeInfo>> {
    let ItemTypePath { package, r#type } = path.into_inner();
    let item_type_id = Absolute::new(package.clone(), r#type);
    let state = data.read_state(role.into_inner());
    let item_type = state
        .types
        .items
        .get(&item_type_id)
        .ok_or_else(|| Error::ItemTypeNotFound(item_type_id.clone()))?;
    Ok(Json(item_type.info(Some(&package), &state.types)))
}

// #[get("/property")]
#[instrument]
#[api_operation(summary = "List property types")]
async fn list_property_types(role: Auth<ViewerRole>, data: Data<AppData>) -> HttpResponse {
    http_response(Ok(data
        .read_state(role.into_inner())
        .types
        .properties
        .keys()
        .collect::<Vec<_>>()))
}

// #[get("/property/{package}")]
#[instrument]
#[api_operation(summary = "List property types for a package")]
async fn list_pkg_property_types(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    pkg: Path<PackageId>,
) -> HttpResponse {
    with_package(role.into_inner(), &data, &pkg, |pkg| {
        pkg.properties.keys().collect()
    })
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
struct PropertyTypePath {
    package: PackageId,
    r#type: PropertyId,
}

// #[get("/item/{package}/{property}")]
#[instrument]
#[api_operation(summary = "Get info on a property type")]
async fn get_property_type(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<PropertyTypePath>,
) -> Result<Json<PropertyTypeInfo>> {
    let PropertyTypePath { package, r#type } = path.into_inner();
    let property_id = Absolute::new(package, r#type);
    let state = data.read_state(role.into_inner());
    let prop_type = state
        .types
        .properties
        .get(&property_id)
        .ok_or_else(|| Error::PropertyNotFound(property_id.clone()))?;
    Ok(Json(prop_type.info()))
}

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent)]
struct AbsRelationTypeIds(BTreeSet<Absolute<RelationTypeId>>);

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent)]
struct RelationTypeIds(BTreeSet<RelationTypeId>);

// #[get("/relation")]
#[instrument]
#[api_operation(summary = "List relation types")]
async fn list_relation_types(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
) -> Json<AbsRelationTypeIds> {
    Json(AbsRelationTypeIds(
        data.read_state(role.into_inner())
            .types
            .relations
            .keys()
            .cloned()
            .collect(),
    ))
}

// #[get("/relation/{package}")]
#[instrument]
#[api_operation(summary = "List item types for a package")]
async fn list_pkg_relation_types(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    pkg: Path<PackageId>,
) -> Result<Json<RelationTypeIds>> {
    let pkg = pkg.into_inner();
    Ok(Json(RelationTypeIds(
        data.read_state(role.into_inner())
            .packages
            .get(&pkg)
            .ok_or_else(|| Error::PackageNotFound(pkg.clone()))?
            .relations
            .keys()
            .cloned()
            .collect(),
    )))
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
struct RelationTypePath {
    package: PackageId,
    r#type: RelationTypeId,
}

// #[get("/item/{package}/{type}")]
#[instrument]
#[api_operation(summary = "Get info on an item type")]
async fn get_relation_type(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<RelationTypePath>,
) -> Result<Json<RelationTypeInfo>> {
    let RelationTypePath { package, r#type } = path.into_inner();
    let rel_type_id = Absolute::new(package.clone(), r#type);
    let state = data.read_state(role.into_inner());
    let rel_type = state
        .types
        .relations
        .get(&rel_type_id)
        .ok_or_else(|| Error::RelationTypeNotFound(rel_type_id.clone()))?;
    Ok(Json(rel_type.info(Some(&package), &state.types)))
}

fn with_package<F, R>(role: ViewerRole, data: &AppData, pkg: &PackageId, f: F) -> HttpResponse
where
    F: for<'a> FnOnce(&'a PackageType) -> Vec<&'a R>,
    R: Serialize,
{
    http_response(
        data.read_state(role)
            .types
            .packages
            .get(pkg)
            .ok_or_else(|| Error::PackageNotFound(pkg.clone()))
            .map(f),
    )
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
#[serde(rename_all = "snake_case")]
enum SelectorCtx {
    Item(Option<BTreeSet<Absolute<ItemTypeId>>>),
    Relation(Option<BTreeSet<Absolute<RelationTypeId>>>),
}

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent, Default, Debug)]
struct SelectorCtxInfo {
    relations: BTreeSet<Absolute<RelationTypeId>>,
    parents: BTreeSet<Absolute<ItemTypeId>>,
    items: BTreeSet<Absolute<ItemTypeId>>,
    sources: BTreeSet<Absolute<ItemTypeId>>,
    targets: BTreeSet<Absolute<ItemTypeId>>,
    properties: BTreeSet<Absolute<PropertyId>>,
}

// #[post("/selector")]
#[instrument]
#[api_operation(summary = "Get info for selector autocompletion")]
async fn post_selector_context_info(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    ctx: Json<SelectorCtx>,
) -> Json<SelectorCtxInfo> {
    let ctx = ctx.into_inner();
    let state = data.read_state(role.into_inner());
    let info = match &ctx {
        SelectorCtx::Item(None) | SelectorCtx::Relation(None) => {
            let item_types = state.types.items.keys().cloned().collect();
            let relation_types = || state.types.relations.keys().cloned().collect();
            let properties = state.types.properties.keys().cloned().collect();
            match ctx {
                SelectorCtx::Item(_) => SelectorCtxInfo {
                    relations: relation_types(),
                    parents: item_types,
                    properties,
                    items: BTreeSet::new(),
                    sources: BTreeSet::new(),
                    targets: BTreeSet::new(),
                },
                SelectorCtx::Relation(_) => SelectorCtxInfo {
                    items: item_types.clone(),
                    sources: item_types.clone(),
                    targets: item_types,
                    properties,
                    parents: BTreeSet::new(),
                    relations: BTreeSet::new(),
                },
            }
        }
        SelectorCtx::Item(Some(items)) => items
            .iter()
            .filter_map(|item_type_id| state.types.items.get(item_type_id))
            .fold(SelectorCtxInfo::default(), |mut info, item_type| {
                if let Some(parents) = item_type.parents(&state.types) {
                    info.parents.extend(parents.keys().cloned());
                }
                info.relations.extend(
                    std::iter::once(item_type)
                        .chain(
                            item_type
                                .supertypes(&state.types)
                                .map(|(_, item_type)| item_type),
                        )
                        .flat_map(|item_type| {
                            item_type.source_of.keys().chain(item_type.target_of.keys())
                        })
                        .cloned(),
                );
                info.properties.extend(
                    item_type
                        .properties(&state.types)
                        .map(|(prop_id, _)| prop_id.clone()),
                );
                info
            }),
        SelectorCtx::Relation(Some(relations)) => relations
            .iter()
            .filter_map(|rel_type_id| state.types.relations.get(rel_type_id))
            .fold(SelectorCtxInfo::default(), |mut info, rel_type| {
                info.sources.insert(rel_type.source.key().clone());
                info.targets.insert(rel_type.target.key().clone());
                info.items.insert(rel_type.source.key().clone());
                info.items.insert(rel_type.target.key().clone());
                info.properties.extend(rel_type.properties.keys().cloned());
                info
            }),
    };
    Json(info)
}
