/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, sync::Arc};

use actix_web::{
    web::{Data, Json, Path, Query},
    HttpResponse,
};

use apistos::{
    api_operation,
    web::{delete, get, put, Resource, Scope},
};
use chrono::{DateTime, Utc};
use dbschema::{Filter, FilterPath};
use itertools::Itertools;
use relation_graph::{
    db::DbItem,
    serial,
    status::{Status, StatusDoc},
    Absolute, EntityInfo, ItemId, ItemTypeId, PackageId, RelationId, ViewId,
};
use serde::{Deserialize, Serialize};
use serde_json::json;
use tracing::instrument;

use crate::{
    auth::Auth,
    http_response,
    roles::{EditorRole, ViewerRole},
    run_updates, AppData, Error, Result, ITEMS_TABLE, STATUS_TABLE,
};

pub(crate) fn item_svc() -> Scope {
    Scope::new("/item")
        .service(
            Resource::new("")
                .route(get().to(list_items))
                .route(put().to(put_item)),
        )
        .service(
            Resource::new("/{id}")
                .route(get().to(show_item))
                .route(put().to(put_item_with_id))
                .route(delete().to(delete_item)),
        )
        .service(Resource::new("/view/{package}/{item_type}").route(get().to(get_item_type_view)))
}

pub(crate) fn relation_svc() -> Scope {
    Scope::new("/relation")
        .service(
            Resource::new("")
                .route(get().to(list_relations))
                .route(put().to(put_relation)),
        )
        .service(
            Resource::new("/{id}")
                .route(put().to(put_relation_with_id))
                .route(delete().to(delete_relation)),
        )
}

pub(crate) fn items_svc() -> Scope {
    Scope::new("/items")
        .service(Resource::new("").route(put().to(put_items)))
        .service(Resource::new("/{pkg}").route(put().to(put_pkg_items)))
        .service(
            Resource::new("/count/descendants/{item_id}").route(get().to(get_descendant_counts)),
        )
        .service(
            Resource::new("/count/descendants/{item_id}/{time}")
                .route(get().to(get_descendant_counts_at)),
        )
        .service(
            Resource::new("/count/aggr-status/item-type/{item_type}")
                .route(get().to(get_aggr_status_counts)),
        )
        .service(
            Resource::new("/count/aggr-status/item-type/{item_type}/{time}")
                .route(get().to(get_aggr_status_counts_at)),
        )
}

#[instrument]
#[api_operation(summary = "List all items")]
async fn list_items(role: Auth<ViewerRole>, data: Data<AppData>) -> HttpResponse {
    http_response(Ok(data
        .read_state(role.into_inner())
        .items
        .iter_items()
        .map(|(id, item)| (id, item.to_serial(None)))
        .collect::<BTreeMap<_, _>>()))
}

#[instrument]
#[api_operation(summary = "Get an item by id")]
async fn show_item(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    item_id: Path<ItemId>,
) -> HttpResponse {
    http_response(Ok(data
        .read_state(role.into_inner())
        .items
        .get_item(item_id.as_ref())
        .map(|item| item.to_serial(None))))
}

//#[put("")]
#[instrument]
#[api_operation(summary = "Create a new item")]
async fn put_item(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    item: Json<serial::Item>,
) -> HttpResponse {
    let id = ItemId::new();
    http_response(
        run_put_item(
            role.into_inner(),
            data.into_inner(),
            id.clone(),
            item.into_inner(),
        )
        .await
        .map(|_| id),
    )
}

//#[put("/{id}")]
#[instrument]
#[api_operation(summary = "Create or update an item with id")]
async fn put_item_with_id(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    item_id: Path<ItemId>,
    item: Json<serial::Item>,
) -> HttpResponse {
    http_response(
        run_put_item(
            role.into_inner(),
            data.into_inner(),
            item_id.into_inner(),
            item.into_inner(),
        )
        .await,
    )
}

async fn run_put_item(
    role: EditorRole,
    data: Arc<AppData>,
    id: ItemId,
    item: serial::Item,
) -> Result<()> {
    let updates = {
        let mut state = data.write_state(role);
        state.insert_item(id, item, None)?
    };
    run_updates(&data, updates).await
}

//#[delete("/{id}")]
#[instrument]
#[api_operation(summary = "Delete an item")]
async fn delete_item(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    item_id: Path<ItemId>,
) -> HttpResponse {
    async fn run(role: EditorRole, data: Arc<AppData>, id: ItemId) -> Result<()> {
        let updates = {
            let mut state = data.write_state(role);
            state.remove_item(&id)
        };
        run_updates(&data, updates).await
    }

    http_response(run(role.into_inner(), data.into_inner(), item_id.into_inner()).await)
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct ItemTypeIdPath(PackageId, ItemTypeId);

#[instrument]
#[api_operation(summary = "Get item type unique view")]
async fn get_item_type_view(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ItemTypeIdPath>,
) -> HttpResponse {
    let ItemTypeIdPath(package, item_type) = path.into_inner();
    http_response(get_view_from_item_type(
        role.into_inner(),
        &data,
        package,
        item_type,
    ))
}

pub fn get_view_from_item_type(
    role: ViewerRole,
    data: &AppData,
    package: PackageId,
    item_type: ItemTypeId,
) -> Result<ViewId> {
    let relative_item_type = Absolute::new(package, item_type).to_relative_opt(None);
    let view_id = data
        .read_state(role)
        .views
        .find_item_type_view(relative_item_type)
        .ok_or_else(|| Error::ItemTypeView)?;
    Ok(view_id)
}

#[instrument]
#[api_operation(summary = "List all relations")]
async fn list_relations(role: Auth<ViewerRole>, data: Data<AppData>) -> HttpResponse {
    http_response(Ok(data
        .read_state(role.into_inner())
        .items
        .iter_relations()
        .map(|(id, item)| (id, item.to_serial(None)))
        .collect::<BTreeMap<_, _>>()))
}

// #[put("")]
#[instrument]
#[api_operation(summary = "Create a relation")]
async fn put_relation(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    relation: Json<serial::Relation>,
) -> HttpResponse {
    let id = RelationId::new();
    http_response(
        run_put_relation(
            role.into_inner(),
            data.into_inner(),
            id.clone(),
            relation.into_inner(),
        )
        .await
        .map(|_| id),
    )
}

// #[put("/{id}")]
#[instrument]
#[api_operation(summary = "Create or update a relation by id")]
async fn put_relation_with_id(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    rel_id: Path<RelationId>,
    relation: Json<serial::Relation>,
) -> HttpResponse {
    http_response(
        run_put_relation(
            role.into_inner(),
            data.into_inner(),
            rel_id.into_inner(),
            relation.into_inner(),
        )
        .await,
    )
}

async fn run_put_relation(
    role: EditorRole,
    data: Arc<AppData>,
    id: RelationId,
    relation: serial::Relation,
) -> Result<()> {
    let updates = {
        let mut state = data.write_state(role);
        state.insert_relation(id, relation, None)?
    };
    run_updates(&data, updates).await
}

// #[delete("/{id}")]
#[instrument]
#[api_operation(summary = "Delete a relation")]
async fn delete_relation(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    item_id: Path<ItemId>,
) -> HttpResponse {
    async fn run(role: EditorRole, data: Arc<AppData>, id: ItemId) -> Result<()> {
        let updates = {
            let mut state = data.write_state(role);
            state.remove_item(&id)
        };
        run_updates(&data, updates).await
    }

    http_response(run(role.into_inner(), data.into_inner(), item_id.into_inner()).await)
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent)]
pub(super) struct Items {
    pub(super) domain: serial::Domain,
    pub(super) items: serial::Items,
}

// #[put("")]
#[instrument(skip(data, items))]
#[api_operation(summary = "Create or update multiple items and relations")]
async fn put_items(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    items: Json<Items>,
) -> HttpResponse {
    let items = items.into_inner();
    http_response(
        run_put_items(
            role.into_inner(),
            data.into_inner(),
            items.domain,
            items.items,
            None,
        )
        .await,
    )
}

// #[put("/{pkg}")]
#[instrument(skip(data, items))]
#[api_operation(summary = "Create or update multiple items and relations relative to a package")]
async fn put_pkg_items(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    pkg: Path<PackageId>,
    items: Json<Items>,
) -> HttpResponse {
    let items = items.into_inner();
    http_response(
        run_put_items(
            role.into_inner(),
            data.into_inner(),
            items.domain,
            items.items,
            Some(&pkg.into_inner()),
        )
        .await,
    )
}

async fn run_put_items(
    role: EditorRole,
    data: Arc<AppData>,
    domain: serial::Domain,
    items: serial::Items,
    pkg: Option<&PackageId>,
) -> Result<()> {
    let updates = {
        let mut state = data.write_state(role);
        state.insert_items(domain, items, pkg)?
    };
    run_updates(&data, updates).await
}

// #[get("count/descendants/{item_id}")]
#[instrument]
#[api_operation(summary = "Get descendant counts")]
async fn get_descendant_counts(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ItemId>,
) -> HttpResponse {
    let item_id = path.into_inner();
    http_response(run_descendant_counts_at(&data, &item_id, None).await)
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
struct GetCountsAt(ItemId, DateTime<Utc>);

// #[get("count/descendants/{item_id}/{time}")]
#[instrument]
#[api_operation(summary = "Get historical descendant counts")]
async fn get_descendant_counts_at(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<GetCountsAt>,
) -> HttpResponse {
    let GetCountsAt(item_id, time) = path.into_inner();
    http_response(run_descendant_counts_at(&data, &item_id, Some(time)).await)
}

async fn run_descendant_counts_at(
    data: &AppData,
    item_id: &ItemId,
    time: Option<DateTime<Utc>>,
) -> Result<BTreeMap<Absolute<ItemTypeId>, u64>> {
    #[derive(Deserialize)]
    struct EsDescendantCountsRes {
        aggregations: EsAggr,
    }

    #[derive(Deserialize)]
    struct EsAggr {
        descendants_by_type: EsBuckets,
    }

    #[derive(Deserialize)]
    struct EsBuckets {
        buckets: Vec<EsBucket>,
    }

    #[derive(Deserialize)]
    struct EsBucket {
        key: Absolute<ItemTypeId>,
        doc_count: u64,
    }

    let time_query = json!(match time {
        Some(t) => json!({
            "bool": {
                "must": [
                    {
                        "range": {
                            "@active.from": { "lte": t }
                        }
                    },
                    {
                        "bool": {
                            "should": [
                                {
                                    "range": {
                                        "@active.to": { "gt": t }
                                    }
                                },
                                {
                                    "bool": {
                                        "must_not": {
                                            "exists": {
                                                "field": "@active.to"
                                            }
                                        }
                                    }
                                }
                            ]
                        }
                    }
                ]
            }
        }),
        None => json!({
            "bool": {
                "must_not": {
                    "exists": {
                        "field": "@active.to"
                    }
                }
            }
        }),
    });

    let counts = data
        .es
        .post(
            data.es_url
                .join("continuousc-items/_search")
                .map_err(Error::InvalidEsUrl)?,
        )
        .json(&json!({
            "query": {
                "bool": {
                    "must": [
                        time_query,
                        { "term": { "parents.keyword": { "value": item_id } } }
                    ]
                }
            },
            "aggs": {
                "descendants_by_type": {
                    "terms": {
                        "field": "item_type.keyword"
                    }
                }
            },
            "size": 0
        }))
        .send()
        .await
        .map_err(Error::ReqAlertCountsMw)?
        .error_for_status()
        .map_err(Error::ReqAlertCounts)?
        .json::<EsDescendantCountsRes>()
        .await
        .map_err(Error::DecodeAlertCountsRes)?;

    Ok(counts
        .aggregations
        .descendants_by_type
        .buckets
        .into_iter()
        .map(|bucket| (bucket.key, bucket.doc_count))
        .collect())
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
struct GetStatusCountsParams(Absolute<ItemTypeId>);

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
#[serde(rename_all = "camelCase")]
struct GetStatusCountsQuery {
    #[serde(default = "number_of_items")]
    number_of_items: usize,
}

const fn number_of_items() -> usize {
    25
}

// #[get("count/item_type/{item_type}")]
#[instrument]
#[api_operation(summary = "Get aggregated status counts for an item type")]
async fn get_aggr_status_counts(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Path<GetStatusCountsParams>,
    query: Query<GetStatusCountsQuery>,
) -> HttpResponse {
    let GetStatusCountsParams(item_type) = params.into_inner();
    http_response(run_aggr_status_counts(&data, &item_type, None, query.0.number_of_items).await)
}

#[derive(Deserialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
struct GetStatusCountsAtParams(Absolute<ItemTypeId>, DateTime<Utc>);

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
struct AggrStatusCounts {
    count: u64,
    status: BTreeMap<Status, u64>,
    unknown: u64,
    items: Vec<AggrItemInfo>,
}

#[derive(Serialize, schemars::JsonSchema, Debug)]
struct AggrItemInfo {
    id: ItemId,
    r#type: Absolute<ItemTypeId>,
    name: Vec<String>,
    status: Status,
    since: DateTime<Utc>, //active: Anchor,
}

// #[get("count/descendants/{item_id}/{time}")]
#[instrument]
#[api_operation(summary = "Get historical Get aggregated status counts for an item type")]
async fn get_aggr_status_counts_at(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Path<GetStatusCountsAtParams>,
    query: Query<GetStatusCountsQuery>,
) -> HttpResponse {
    let GetStatusCountsAtParams(item_id, time) = params.into_inner();
    http_response(
        run_aggr_status_counts(&data, &item_id, Some(time), query.0.number_of_items).await,
    )
}

async fn run_aggr_status_counts(
    data: &AppData,
    item_type: &Absolute<ItemTypeId>,
    time: Option<DateTime<Utc>>,
    number_of_items: usize,
) -> Result<AggrStatusCounts> {
    let filter = FilterPath::new().field("item_type").eq(json!(item_type));

    let items = match time {
        Some(time) => data
            .db
            .query_discovery_objects_at(ITEMS_TABLE.clone(), filter.clone(), time)
            .await
            .map_err(Error::Db)?,
        None => data
            .db
            .query_discovery_objects(ITEMS_TABLE.clone(), filter)
            .await
            .map_err(Error::Db)?,
    }
    .into_iter()
    .map(|(obj_id, value)| {
        let item = serde_json::from_value::<DbItem>(value.value)
            .map_err(|e| Error::DecodeItem(obj_id, e))?;
        match item.entity {
            EntityInfo::Item { item } => Ok(Some((
                item.item_id.clone(),
                (value.version.active.from, item),
            ))),
            EntityInfo::Relation { .. } => Ok(None),
        }
    })
    .filter_map(Result::transpose)
    .collect::<Result<BTreeMap<_, _>>>()?;

    let filter = FilterPath::new()
        .field("item_id")
        .some()
        .eq_any(items.keys().map(|v| json!(v)).collect())
        .or(FilterPath::new()
            .field("parents")
            .some()
            .filter(Filter::AnyElem(Box::new(Filter::In(
                items.keys().map(|v| json!(v)).collect(),
            )))));

    let stati = match time {
        Some(time) => data
            .db
            .query_discovery_objects_at(STATUS_TABLE.clone(), filter, time)
            .await
            .map_err(Error::Db)?,
        None => data
            .db
            .query_discovery_objects(STATUS_TABLE.clone(), filter)
            .await
            .map_err(Error::Db)?,
    }
    .into_values()
    .map(|value| serde_json::from_value::<StatusDoc>(value.value).map_err(Error::DecodeStatusDoc))
    .collect::<Result<Vec<_>>>()?;

    let item_status = stati
        .iter()
        .filter_map(|s| match &s.entity {
            EntityInfo::Item { item } => {
                let parent = items
                    .contains_key(&item.item_id)
                    .then_some(&item.item_id)
                    .or_else(|| {
                        item.parents
                            .iter()
                            .find(|parent| items.contains_key(parent))
                    })?;
                Some((parent, s.change.status))
            }
            EntityInfo::Relation { .. } => None,
        })
        .fold(BTreeMap::<_, Status>::new(), |mut map, (parent, status)| {
            map.entry(parent)
                .and_modify(|s| {
                    *s = (*s).max(status);
                })
                .or_insert(status);
            map
        });

    let status_counts = item_status
        .values()
        .fold(BTreeMap::new(), |mut map, status| {
            *map.entry(*status).or_insert(0) += 1;
            map
        });

    let filtered_items = item_status
        .iter()
        .filter_map(|(item_id, status)| {
            let (active, item) = items.get(item_id)?;
            Some((status, active, item))
        })
        .k_largest_by_key(number_of_items, |(status, since, item)| {
            (*status, *since, &item.item_name)
        })
        .map(|(status, active, item)| AggrItemInfo {
            id: item.item_id.clone(),
            r#type: item.item_type.clone(),
            name: item.item_name.clone(),
            status: *status,
            since: *active,
        })
        .collect::<Vec<_>>();

    Ok(AggrStatusCounts {
        count: items.len() as u64,
        unknown: (items.len() as u64).saturating_sub(status_counts.values().sum::<u64>()),
        status: status_counts,
        items: filtered_items,
    })
}
