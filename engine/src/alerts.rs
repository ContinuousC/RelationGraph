/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    str::FromStr,
    sync::Arc,
};

use actix_web::{
    web::{Data, Json, Path, Query},
    HttpResponse,
};
use apistos::{
    api_operation,
    web::{delete, get, post, put, Resource, Scope},
    ApiComponent,
};
use chrono::{DateTime, Utc};
use dbdaemon_types::Operation;
use dbschema::{Filter, FilterPath, ObjectId, SingleVersioned, Versioned};
use prometheus_core::LabelName;
use relation_graph::{
    alerts::{
        AlertDoc, AlertRule, AlertRuleState, AlertRuleTemplateName, AlertmanagerConfig,
        AlertmanagerConfigReceiver, AlertmanagerConfigRoute, AlertmanagerRequestBody,
        PutAlertRuleForm, ReceiverName, RuleGroup, Severity,
    },
    db::DbItem,
    Absolute, EntityInfo, ItemId, ItemTypeId, PackageId, RelationId,
};
use reqwest::StatusCode;
use schemars::JsonSchema;
use serde::Deserialize;
use serde_json::json;
use serde_with::{DeserializeFromStr, SerializeDisplay};
use tokio::sync::Mutex as AsyncMutex;
use tracing::instrument;

use crate::{
    auth::Auth,
    events::{run_bins_query, run_events_query, BinsParams},
    roles::{EditorRole, ViewerRole},
    ITEMS_TABLE,
};
use crate::{
    http_response, AppData, Error, Result, ALERTS_TABLE, ALERT_CONFIG_TABLE, ALERT_RULES_TABLE,
};

pub(crate) fn alert_rules_svc() -> Scope {
    Scope::new("/alert_rules")
        .service(Resource::new("").route(get().to(get_alert_rules)))
        .service(
            Resource::new("/{rule}")
                .route(get().to(get_alert_rule))
                .route(put().to(put_alert_rule))
                .route(delete().to(delete_alert_rule)),
        )
        .service(Resource::new("/{rule}/selectors").route(get().to(get_rule_selectors)))
        .service(
            Resource::new("/{rule}/selectors/{label}").route(get().to(get_rule_selector_values)),
        )
}

// pub(crate) fn alert_config_svc() -> Scope {
//     Scope::new("/alert_config")
//         .service(get_alert_configs)
//         .service(get_alert_config)
//         .service(delete_alert_configs)
// }

pub(crate) fn alerts_svc() -> Scope {
    Scope::new("/alerts")
        .service(Resource::new("/bins").route(post().to(post_bins)))
        .service(Resource::new("/events").route(post().to(post_events)))
        .service(Resource::new("/item/{item}").route(get().to(get_item_alerts)))
        .service(Resource::new("/relation/{rel}").route(get().to(get_relation_alerts)))
        .service(Resource::new("/item/{item}/{time}").route(get().to(get_item_alerts_at)))
        .service(Resource::new("/relation/{rel}/{time}").route(get().to(get_relation_alerts_at)))
        .service(Resource::new("/item_types").route(post().to(get_item_types_alerts)))
        .service(Resource::new("/item_types/{time}").route(post().to(get_item_types_alerts_at)))
        .service(Resource::new("/all").route(post().to(get_all_alerts)))
        .service(Resource::new("/all/{time}").route(post().to(get_all_alerts_at)))
        .service(
            Resource::new("/items/{package}/{item_type}").route(get().to(get_item_type_alerts)),
        )
        .service(
            Resource::new("/items/{package}/{item_type}/{time}")
                .route(get().to(get_item_type_alerts_at)),
        )
        .service(Resource::new("/count/item/{item_id}").route(get().to(get_alert_counts)))
        .service(Resource::new("/count/item/{item_id}/{time}").route(get().to(get_alert_counts_at)))
}

pub(crate) async fn update_alertmanager(data: &AppData) -> Result<()> {
    let response = data
        .prom
        .get(data.alertmanager_url.join("api/v1/alerts")?)
        .send()
        .await
        .map_err(Error::PromRequestMw)?;
    if response.status() == StatusCode::NOT_FOUND {
        let receiver_name = ReceiverName::new("default-receiver".to_string());
        data.prom
            .post(data.alertmanager_url.join("api/v1/alerts")?)
            .header("Content-Type", "application/x-yaml")
            .body(
                serde_yaml::to_string(&AlertmanagerRequestBody {
                    alertmanager_config: serde_yaml::to_string(&AlertmanagerConfig {
                        global: None,
                        route: AlertmanagerConfigRoute {
                            receiver: receiver_name.clone(),
                        },
                        receivers: vec![AlertmanagerConfigReceiver {
                            name: receiver_name,
                        }],
                        templates: None,
                    })
                    .unwrap(),
                    template_files: BTreeMap::new(),
                })
                .unwrap(),
            )
            .send()
            .await
            .map_err(Error::PromRequestMw)?
            .error_for_status()
            .map_err(Error::PromRequest)?;
    };

    Ok(())
}

#[derive(SerializeDisplay, DeserializeFromStr, PartialOrd, Ord, PartialEq, Eq, Hash)]
struct RuleGroupName(AlertRuleTemplateName);

pub(crate) async fn update_ruler(role: EditorRole, data: &AppData) -> Result<()> {
    let response = data
        .prom
        .get(data.ruler_url.join("api/v1/rules")?)
        .send()
        .await
        .map_err(Error::PromRequestMw)?;
    let current = if response.status() == StatusCode::NOT_FOUND {
        BTreeMap::new()
    } else {
        let payload = response
            .error_for_status()
            .map_err(Error::PromRequest)?
            .bytes()
            .await
            .map_err(Error::PromRequest)?;
        serde_yaml::from_slice::<BTreeMap<RuleGroupName, Vec<RuleGroup<AlertRule>>>>(&payload)
            .map_err(Error::PromDecodeYaml)?
    };

    let desired = {
        let state = data.read_state(role.into());
        state
            .alert_rules
            .rule_groups(state.get_prometheus_schema()?)
            .filter_map(|(name, result)| match result {
                Ok(group) => Some((RuleGroupName(name), group)),
                Err(e) => {
                    log::warn!("failed to evaluate rule config for group {name}: {e}");
                    None
                }
            })
            .collect::<BTreeMap<_, _>>()
    };

    log::debug!(
        "Current rule groups: {}",
        current
            .keys()
            .map(|name| name.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    );
    log::debug!(
        "Desired rule groups: {}",
        desired
            .keys()
            .map(|name| name.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    );

    for name in current.keys().filter(|name| !desired.contains_key(*name)) {
        log::debug!("removing rule group {name}");
        remove_rule_group(data, name).await?;
    }

    for (name, group) in desired.iter().filter(|(name, group)| {
        current
            .get(name)
            .map_or(true, |value| value.len() != 1 || &value[0] != *group)
    }) {
        log::debug!("adding / updating rule group {name}");
        update_rule_group(data, name, group).await?;
    }

    Ok(())
}

async fn update_rule_group(
    data: &AppData,
    group_name: &RuleGroupName,
    rule_group: &RuleGroup<AlertRule>,
) -> Result<()> {
    if rule_group.rules.is_empty() {
        let _ = remove_rule_group(data, group_name).await;
        Ok(())
    } else {
        log::debug!("Updating rule group {group_name}...");
        data.prom
            .post(data.ruler_url.join(&format!("api/v1/rules/{group_name}"))?)
            .header("Content-Type", "application/yaml")
            .body(serde_yaml::to_string(&rule_group).unwrap())
            .send()
            .await
            .map_err(Error::PromRequestMw)?
            .error_for_status()
            .map_err(Error::PromRequest)?
            .json::<prometheus_api::Response<()>>()
            .await
            .map_err(Error::PromDecode)?;
        Ok(())
    }
}

async fn remove_rule_group(data: &AppData, group_name: &RuleGroupName) -> Result<()> {
    log::debug!("Removing rule group {group_name}...");
    let res = data
        .prom
        .delete(data.ruler_url.join(&format!("api/v1/rules/{group_name}"))?)
        .send()
        .await
        .map_err(Error::PromRequestMw)?;
    if res.status() == StatusCode::NOT_FOUND {
        let err = res.error_for_status_ref().unwrap_err();
        let msg = res.text().await.map_err(Error::PromRequest)?;
        if msg.as_str() != "group namespace does not exist" {
            return Err(Error::PromRequest(err));
        }
    } else {
        res.error_for_status()
            .map_err(Error::PromRequest)?
            .json::<prometheus_api::Response<()>>()
            .await
            .map_err(Error::PromDecode)?
            .into_result()
            .map_err(Error::Prometheus)?;
    }
    Ok(())
}

/*** Alert rules. ***/

// #[get("")]
#[instrument]
#[api_operation(summary = "Get a list of alert rule names")]
async fn get_alert_rules(role: Auth<ViewerRole>, data: Data<AppData>) -> HttpResponse {
    http_response(Ok(data
        .read_state(role.into_inner())
        .alert_rules
        .get_rule_form_names()
        .collect::<Vec<_>>()))
}

// #[get("/{rule}")]
#[instrument]
#[api_operation(summary = "Get an alert rule specification and config")]
async fn get_alert_rule(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<AlertRuleTemplateName>,
) -> HttpResponse {
    http_response(run_get_alert_rule(role.into_inner(), &data, &path))
}

fn run_get_alert_rule(
    role: ViewerRole,
    data: &AppData,
    rule_name: &AlertRuleTemplateName,
) -> Result<AlertRuleState> {
    data.read_state(role)
        .alert_rules
        .get_rule_form(rule_name)
        .cloned()
        .ok_or_else(|| Error::MissingAlertRule(rule_name.clone()))
}

// #[get("/{rule}/selectors")]
#[instrument]
#[api_operation(summary = "Get an alert rules selectors")]
async fn get_rule_selectors(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<AlertRuleTemplateName>,
) -> HttpResponse {
    let rule_name = path.into_inner();
    http_response(run_get_rule_labels(role.into_inner(), &data, &rule_name))
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct GetSelectorValues(AlertRuleTemplateName, LabelName);

// #[get("/{rule}/selectors/{label}")]
#[instrument]
#[api_operation(summary = "Get an alert rule selectors values")]
async fn get_rule_selector_values(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<GetSelectorValues>,
) -> HttpResponse {
    let GetSelectorValues(rule_name, label_name) = path.into_inner();
    http_response(
        run_get_rule_label_values(role.into_inner(), &data, &rule_name, &label_name).await,
    )
}

fn run_get_rule_labels(
    role: ViewerRole,
    data: &AppData,
    rule_name: &AlertRuleTemplateName,
) -> Result<BTreeSet<LabelName>> {
    let state = data.read_state(role);
    let schema = state.get_prometheus_schema()?;
    let prom_item_name = state
        .alert_rules
        .get_rule_form(rule_name)
        .ok_or_else(|| Error::MissingAlertRule(rule_name.clone()))?
        .rule_form
        .value
        .item();
    let prom_item = schema
        .lookup_item(&prom_item_name)
        .ok_or_else(|| Error::MissingPromItem(prom_item_name))?;

    Ok(prom_item
        .parents_recursive(schema)
        .map(|(_, parent)| parent)
        .chain(std::iter::once(prom_item))
        .flat_map(|item| item.labels())
        //.filter(|(_, sel)| !matches!(sel, prometheus_schema::LabelSelector::Eq(_)))
        .map(|(name, _)| name.clone())
        .collect())
}

async fn run_get_rule_label_values(
    role: ViewerRole,
    data: &AppData,
    rule_name: &AlertRuleTemplateName,
    label_name: &LabelName,
) -> Result<BTreeSet<String>> {
    let instances = {
        let state = data.read_state(role);
        let schema = state.get_prometheus_schema()?;
        let prom_item_name = state
            .alert_rules
            .get_rule_form(rule_name)
            .ok_or_else(|| Error::MissingAlertRule(rule_name.clone()))?
            .rule_form
            .value
            .item();
        let prom_item = schema
            .lookup_item(&prom_item_name)
            .ok_or_else(|| Error::MissingPromItem(prom_item_name.clone()))?;
        prom_item.svc_queries(&prom_item_name, schema)
    };

    Ok(data
        .prom
        .get(
            data.prom_url
                .join(&format!("api/prom/api/v1/label/{label_name}/values"))
                .map_err(Error::Url)?,
        )
        .query(
            &instances
                .into_values()
                .map(|m| ("match[]", m.to_string()))
                .collect::<Vec<_>>(),
        )
        .send()
        .await
        .map_err(Error::PromRequestMw)?
        .error_for_status()
        .map_err(Error::PromRequest)?
        .json::<prometheus_api::Response<BTreeSet<String>>>()
        .await
        .map_err(Error::PromDecode)?
        .into_result()
        .map_err(Error::Prometheus)?
        .data)
}

// #[put("/{rule}")]
#[instrument]
#[api_operation(summary = "Create or update an alert rule")]
async fn put_alert_rule(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<AlertRuleTemplateName>,
    body: Json<PutAlertRuleForm>,
) -> HttpResponse {
    http_response(run_update_rule_spec(role.into_inner(), &data, path.into_inner(), body).await)
}

// #[delete("/{rule}")]
#[instrument]
#[api_operation(summary = "Delete an alert rule")]
async fn delete_alert_rule(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<AlertRuleTemplateName>,
) -> HttpResponse {
    http_response(run_delete_rule_spec(role.into_inner(), &data, path.into_inner()).await)
}

async fn run_update_rule_spec(
    role: EditorRole,
    data: &AppData,
    rule_name: AlertRuleTemplateName,
    rule_form: Json<PutAlertRuleForm>,
) -> Result<()> {
    let lock = data
        .locks
        .alert_rules
        .write()
        .entry(rule_name.clone())
        .or_insert_with(|| Arc::new(AsyncMutex::new(())))
        .clone();
    let _guard = lock.lock().await;

    let rule = rule_form.into_inner();
    let result = {
        let state = data.read_state(role.into());
        state.verify_put_rule_form(rule_name.clone(), rule)?
    };
    if result.template_updated {
        let template =
            serde_json::to_value(result.rule_state.rule_form.value.get_template()).unwrap();
        data.db
            .create_or_update_discovery_object(
                ALERT_RULES_TABLE.clone(),
                ObjectId::from(rule_name.to_string()),
                template,
            )
            .await
            .map_err(Error::Db)?;
    }
    if !result.added_configs.is_empty()
        || !result.updated_configs.is_empty()
        || !result.removed_configs.is_empty()
    {
        let config_updates = result
            .removed_configs
            .iter()
            .map(|config_name| {
                (
                    ObjectId::from(format!("{rule_name}:{config_name}")),
                    Operation::Remove,
                )
            })
            .chain(result.added_configs.iter().map(|(config_name, current)| {
                (
                    ObjectId::from(format!("{rule_name}:{config_name}")),
                    Operation::CreateOrUpdate(serde_json::to_value(current).unwrap()),
                )
            }))
            .chain(result.updated_configs.iter().map(|(config_name, current)| {
                (
                    ObjectId::from(format!("{rule_name}:{config_name}")),
                    Operation::CreateOrUpdate(serde_json::to_value(current).unwrap()),
                )
            }))
            .collect();
        data.db
            .bulk_update_discovery_objects(ALERT_CONFIG_TABLE.clone(), config_updates)
            .await
            .map_err(Error::Db)?;
    }
    if result.template_updated
        || !result.added_configs.is_empty()
        || !result.updated_configs.is_empty()
        || !result.removed_configs.is_empty()
    {
        if let Some(rule_group) = result.rule_group {
            update_rule_group(data, &RuleGroupName(rule_name.clone()), &rule_group).await?;
        }
        let mut state = data.write_state(role);
        state.alert_rules.0.insert(rule_name, result.rule_state);
    }
    Ok(())
}

async fn run_delete_rule_spec(
    role: EditorRole,
    data: &AppData,
    rule_name: AlertRuleTemplateName,
) -> Result<()> {
    let lock = data
        .locks
        .alert_rules
        .read()
        .get(&rule_name)
        .ok_or_else(|| Error::MissingAlertRule(rule_name.clone()))?
        .clone();
    let _guard = lock.lock().await;

    let config_updates = {
        let state = data.read_state(role.into());
        let names = state.alert_rules.get_rule_form_configs_names(&rule_name)?;
        names
            .map(|config_name| {
                let obj_id = ObjectId::from(format!("{rule_name}:{config_name}"));
                (obj_id, Operation::Remove)
            })
            .collect::<HashMap<ObjectId, Operation>>()
    };

    data.db
        .remove_discovery_object(
            ALERT_RULES_TABLE.clone(),
            ObjectId::from(rule_name.to_string()),
        )
        .await
        .map_err(Error::Db)?;

    if !config_updates.is_empty() {
        data.db
            .bulk_update_discovery_objects(ALERT_CONFIG_TABLE.clone(), config_updates)
            .await
            .map_err(Error::Db)?;
    }

    remove_rule_group(data, &RuleGroupName(rule_name.clone())).await?;

    let mut state = data.write_state(role);
    state.alert_rules.0.remove(&rule_name);
    data.locks.alert_rules.write().remove(&rule_name);

    Ok(())
}

/*** Alert config. ***/

// #[get("/{rule}")]
// #[instrument]
// async fn get_alert_configs(data: Data<AppData>, path: Path<AlertRuleName>) -> HttpResponse {
//     let rule_name = path.into_inner();
//     http_response(
//         data.state
//             .read()
//             .alert_rules
//             .get_rule_config(&rule_name)
//             .ok_or_else(|| Error::MissingAlertRule(rule_name.clone())),
//     )
// }

// #[get("/{rule}/{config}")]
// #[instrument]
// async fn get_alert_config(
//     data: Data<AppData>,
//     path: Path<(AlertRuleName, AlertConfigName)>,
// ) -> HttpResponse {
//     let (rule_name, config_name) = path.into_inner();
//     http_response(
//         data.state
//             .read()
//             .alert_rules
//             .get_rule_config(&rule_name)
//             .ok_or_else(|| Error::MissingAlertRule(rule_name.clone()))
//             .and_then(|config| {
//                 config.0.get(&config_name).ok_or_else(|| {
//                     Error::MissingAlertConfig(rule_name.clone(), config_name.clone())
//                 })
//             }),
//     )
// }

// #[delete("/{rule}")]
// #[instrument]
// async fn delete_alert_configs(data: Data<AppData>, path: Path<AlertRuleName>) -> HttpResponse {
//     http_response(run_delete_alert_configs(&data, path.into_inner()).await)
// }

// async fn run_delete_alert_configs(data: &AppData, rule_name: AlertRuleName) -> Result<()> {
//     let lock = data
//         .locks
//         .alert_rules
//         .read()
//         .get(&rule_name)
//         .ok_or_else(|| Error::MissingAlertRule(rule_name.clone()))?
//         .clone();
//     let _guard = lock.lock().await;

//     let (rule_group, updates) = {
//         let mut state = data.state.write();
//         let updates = state
//             .alert_rules
//             .get_rule_configs(&rule_name)
//             .ok_or_else(|| Error::MissingAlertRule(rule_name.clone()))?
//             .0
//             .keys()
//             .map(|config_name| {
//                 (
//                     ObjectId::from(format!("{rule_name}:{config_name}")),
//                     Operation::Remove,
//                 )
//             })
//             .collect();
//         (state.remove_rule_configs(&rule_name)?, updates)
//     };

//     // Note: this could be improved if a function was to be added to
//     //  remove discovery objects by query.
//     data.db
//         .bulk_update_discovery_objects(ALERT_CONFIG_TABLE.clone(), updates)
//         .await
//         .map_err(Error::Db)?;

//     update_rule_group(data, &RuleGroupName(rule_name), &rule_group).await?;

//     Ok(())
// }

/* Alerts. */

// #[post("/bins")]
#[instrument]
#[api_operation(summary = "Query binned alerts")]
async fn post_bins(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<BinsParams>,
) -> HttpResponse {
    let data = data.into_inner();
    let params = params.into_inner();
    http_response(run_bins_query(role.into_inner(), &data, params, &ALERTS_TABLE).await)
}

// #[post("/events")]
#[instrument]
#[api_operation(summary = "Query alert changes in a time range")]
async fn post_events(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<BinsParams>,
) -> HttpResponse {
    let data = data.into_inner();
    let params = params.into_inner();
    http_response(
        run_events_query::<AlertDoc, _>(
            role.into_inner(),
            &data,
            params,
            ALERTS_TABLE.clone(),
            |_| true,
        )
        .await,
    )
}

#[derive(Deserialize, JsonSchema, ApiComponent, Debug)]
struct ItemAlertsQuery {
    #[serde(default)]
    include_children: bool,
}

// #[get("item/{item}")]
#[instrument]
#[api_operation(summary = "Query open alerts for an item")]
async fn get_item_alerts(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ItemId>,
    query: Query<ItemAlertsQuery>,
) -> HttpResponse {
    let item_id = path.into_inner();

    let mut filter = FilterPath::new().field("item_id").some().eq(json!(item_id));

    if query.0.include_children {
        filter = filter.or(FilterPath::new()
            .field("parents")
            .some()
            .filter(Filter::any_elem(Filter::Eq(json!(item_id)))));
    }

    http_response(run_get_alerts_at(role.into_inner(), &data, filter, None).await)
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct GetItemAlerts(ItemId, DateTime<Utc>);

// #[get("item/{item}/{time}")]
#[instrument]
#[api_operation(summary = "Query open alerts for an item at a point in time")]
async fn get_item_alerts_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<GetItemAlerts>,
    query: Query<ItemAlertsQuery>,
) -> HttpResponse {
    let GetItemAlerts(item_id, time) = path.into_inner();

    let mut filter = FilterPath::new().field("item_id").some().eq(json!(item_id));

    if query.0.include_children {
        filter = filter.or(FilterPath::new()
            .field("parents")
            .some()
            .filter(Filter::any_elem(Filter::Eq(json!(item_id)))));
    }

    http_response(run_get_alerts_at(role.into_inner(), &data, filter, Some(time)).await)
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct ItemTypeAlertBody(Vec<Absolute<ItemTypeId>>);
#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct Timestamp(DateTime<Utc>);

// #[post("item_types")]
#[instrument]
#[api_operation(summary = "Query open alerts for multiple item types")]
async fn get_item_types_alerts(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    query: Query<ItemAlertsQuery>,
    body: Json<ItemTypeAlertBody>,
) -> HttpResponse {
    let filter = FilterPath::new()
        .field("item_type")
        .some()
        .eq_any(body.into_inner().0.into_iter().map(|v| json!(v)).collect());
    http_response(match query.0.include_children {
        true => run_get_item_type_alerts_at(role.into_inner(), &data, filter, None).await,
        false => run_get_alerts_at(role.into_inner(), &data, filter, None).await,
    })
}

// #[post("item_types/{time}")]
#[instrument]
#[api_operation(summary = "Query open alerts for multipe item types at a point in time")]
async fn get_item_types_alerts_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<Timestamp>,
    query: Query<ItemAlertsQuery>,
    body: Json<ItemTypeAlertBody>,
) -> HttpResponse {
    let filter = FilterPath::new()
        .field("item_type")
        .some()
        .eq_any(body.into_inner().0.into_iter().map(|v| json!(v)).collect());
    let t = Some(path.into_inner().0);
    http_response(match query.0.include_children {
        true => run_get_item_type_alerts_at(role.into_inner(), &data, filter, t).await,
        false => run_get_alerts_at(role.into_inner(), &data, filter, t).await,
    })
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct AllAlertsQuery {
    #[serde(default)]
    include_unmatched: bool,
}

// #[get("all")]
#[instrument]
#[api_operation(summary = "Query all open alerts")]
async fn get_all_alerts(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    query: Query<AllAlertsQuery>,
) -> HttpResponse {
    let filter = match query.include_unmatched {
        true => Filter::All(vec![]),
        false => FilterPath::new()
            .field("item_id")
            .some()
            .ne_any(Vec::new())
            .or(FilterPath::new()
                .field("relation_id")
                .some()
                .ne_any(Vec::new())),
    };
    http_response(run_get_alerts_at(role.into_inner(), &data, filter, None).await)
}

// #[get("all/{time}")]
#[instrument]
#[api_operation(summary = "Query all open alertsat a point in time")]
async fn get_all_alerts_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<Timestamp>,
    query: Query<AllAlertsQuery>,
) -> HttpResponse {
    let filter = match query.include_unmatched {
        true => Filter::All(vec![]),
        false => FilterPath::new()
            .field("item_id")
            .some()
            .ne_any(Vec::new())
            .or(FilterPath::new()
                .field("relation_id")
                .some()
                .ne_any(Vec::new())),
    };
    http_response(
        run_get_alerts_at(role.into_inner(), &data, filter, Some(path.into_inner().0)).await,
    )
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct GetItemTypeAlerts(PackageId, ItemTypeId);

// #[get("items/{package}/{item_type}")]
#[instrument]
#[api_operation(summary = "Query open alerts for an item type")]
async fn get_item_type_alerts(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<GetItemTypeAlerts>,
    query: Query<ItemAlertsQuery>,
) -> HttpResponse {
    let GetItemTypeAlerts(package, item_type) = path.into_inner();
    let absolute_item_type = Absolute::new(package, item_type);
    let filter = FilterPath::new()
        .field("item_type")
        .some()
        .eq(serde_json::Value::String(absolute_item_type.to_string()));
    http_response(match query.0.include_children {
        true => run_get_item_type_alerts_at(role.into_inner(), &data, filter, None).await,
        false => run_get_alerts_at(role.into_inner(), &data, filter, None).await,
    })
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct GetItemTypeAlertsAt(PackageId, ItemTypeId, DateTime<Utc>);

// #[get("items/{package}/{item_type}/{time}")]
#[instrument]
#[api_operation(summary = "Query open alerts for an item type at a point in time")]
async fn get_item_type_alerts_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<GetItemTypeAlertsAt>,
    query: Query<ItemAlertsQuery>,
) -> HttpResponse {
    let GetItemTypeAlertsAt(package, item_type, time) = path.into_inner();
    let absolute_item_type = Absolute::new(package, item_type);
    let filter = FilterPath::new()
        .field("item_type")
        .some()
        .eq(serde_json::Value::String(absolute_item_type.to_string()));
    http_response(match query.0.include_children {
        true => run_get_item_type_alerts_at(role.into_inner(), &data, filter, Some(time)).await,
        false => run_get_alerts_at(role.into_inner(), &data, filter, Some(time)).await,
    })
}

// #[get("relation/{rel_id}")]
#[instrument]
#[api_operation(summary = "Query open alerts for a relation")]
async fn get_relation_alerts(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<RelationId>,
) -> HttpResponse {
    let rel_id = path.into_inner();
    http_response(
        run_get_alerts_at(
            role.into_inner(),
            &data,
            FilterPath::new()
                .field("relation_id")
                .some()
                .eq(serde_json::Value::String(rel_id.to_string())),
            None,
        )
        .await,
    )
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct GetRelationAlertsAt(RelationId, DateTime<Utc>);

// #[get("relation/{rel_id}/{time}")]
#[instrument]
#[api_operation(summary = "Query open alerts for a relation at a point in time")]
async fn get_relation_alerts_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<GetRelationAlertsAt>,
) -> HttpResponse {
    let GetRelationAlertsAt(rel_id, time) = path.into_inner();
    http_response(
        run_get_alerts_at(
            role.into_inner(),
            &data,
            FilterPath::new()
                .field("relation_id")
                .some()
                .eq(serde_json::Value::String(rel_id.to_string())),
            Some(time),
        )
        .await,
    )
}

// #[get("count/item/{item_id}")]
#[instrument]
#[api_operation(summary = "Get alert counts for an item and its descendants")]
async fn get_alert_counts(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ItemId>,
) -> HttpResponse {
    let item_id = path.into_inner();
    http_response(run_alert_counts_at(role.into_inner(), &data, &item_id, None).await)
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct GetAlertCountsAt(ItemId, DateTime<Utc>);

// #[get("count/item/{item_id}/{time}")]
#[instrument]
#[api_operation(summary = "Get alert counts for an item and its descendants at a point in time")]
async fn get_alert_counts_at(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<GetAlertCountsAt>,
) -> HttpResponse {
    let GetAlertCountsAt(item_id, time) = path.into_inner();
    http_response(run_alert_counts_at(role.into_inner(), &data, &item_id, Some(time)).await)
}

async fn run_get_item_type_alerts_at(
    role: ViewerRole,
    data: &AppData,
    filter: Filter,
    time: Option<DateTime<Utc>>,
) -> Result<Vec<SingleVersioned<AlertDoc>>> {
    let items = if let Some(time) = time {
        data.db
            .query_discovery_objects_at(ITEMS_TABLE.clone(), filter, time)
            .await
            .map_err(Error::Db)?
    } else {
        data.db
            .query_discovery_objects(ITEMS_TABLE.clone(), filter)
            .await
            .map_err(Error::Db)?
    }
    .into_iter()
    .map(|(obj_id, value)| {
        let item = serde_json::from_value::<DbItem>(value.value)
            .map_err(|e| Error::DecodeItem(obj_id, e))?;
        match item.entity {
            EntityInfo::Item { item } => Ok(Some(json!(item.item_id))),
            EntityInfo::Relation { .. } => Ok(None),
        }
    })
    .filter_map(Result::transpose)
    .collect::<Result<_>>()?;

    run_get_alerts_at(
        role,
        data,
        FilterPath::new().field("item_id").some().eq_any(items),
        time,
    )
    .await
}

async fn run_get_alerts_at(
    _role: ViewerRole,
    data: &AppData,
    filter: Filter,
    time: Option<DateTime<Utc>>,
) -> Result<Vec<SingleVersioned<AlertDoc>>> {
    let data = if let Some(time) = time {
        data.db
            .query_discovery_objects_at(ALERTS_TABLE.clone(), filter, time)
            .await
            .map(|vs| {
                vs.into_iter()
                    .map(|(id, versioned)| {
                        (
                            id,
                            Versioned {
                                version: versioned.version,
                                value: versioned.value,
                            },
                        )
                    })
                    .collect()
            })
            .map_err(Error::Db)?
    } else {
        data.db
            .query_discovery_objects(ALERTS_TABLE.clone(), filter)
            .await
            .map_err(Error::Db)?
    };
    data.into_values()
        .map(|versioned| {
            let alert = serde_json::from_value::<AlertDoc>(versioned.value)
                .map_err(Error::DecodeAlertDoc)?;
            Ok(SingleVersioned {
                version: versioned.version,
                value: alert,
            })
        })
        .collect()
}

async fn run_alert_counts_at(
    _role: ViewerRole,
    data: &AppData,
    item_id: &ItemId,
    time: Option<DateTime<Utc>>,
) -> Result<BTreeMap<Severity, u64>> {
    #[derive(Deserialize)]
    struct EsAlertCountsRes {
        aggregations: EsAggr,
    }

    #[derive(Deserialize)]
    struct EsAggr {
        alerts_by_severity: EsBuckets,
    }

    #[derive(Deserialize)]
    struct EsBuckets {
        buckets: Vec<EsBucket>,
    }

    #[derive(Deserialize)]
    struct EsBucket {
        key: Severity,
        doc_count: u64,
    }

    let counts = data
        .es
        .post(
            data.es_url
                .join("continuousc-alerts/_search")
                .map_err(Error::InvalidEsUrl)?,
        )
        .json(&json!({
            "query": {
                "bool": {
                    "must": [
                        match time {
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
                            })
                        },
                        {
                            "bool": {
                                "should": [
                                    { "term": { "item_id.keyword": { "value": item_id } } },
                                    { "term": { "parents.keyword": { "value": item_id } } },
                                ]
                            }
                        }
                    ]
                }
            },
            "aggs": {
                "alerts_by_severity": {
                    "terms": {
                        "field": "severity"
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
        .json::<EsAlertCountsRes>()
        .await
        .map_err(Error::DecodeAlertCountsRes)?;

    Ok(counts
        .aggregations
        .alerts_by_severity
        .buckets
        .into_iter()
        .map(|bucket| (bucket.key, bucket.doc_count))
        .collect())
}

/* Helper types and functions. */

impl Display for RuleGroupName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.yaml", self.0)
    }
}

impl FromStr for RuleGroupName {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(Self(AlertRuleTemplateName::new(
            s.strip_suffix(".yaml")
                .ok_or_else(|| Error::InvalidRuleGroupName(s.to_string()))?
                .to_string(),
        )))
    }
}
