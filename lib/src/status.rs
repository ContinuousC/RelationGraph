/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;
use std::fmt::Display;
use std::str::FromStr;

use chrono::{DateTime, Utc};
use dbschema::{HasSchema, ObjectId, OptionSchema, SingleVersioned, StringSchema, StructSchema};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use uuid::Uuid;

use crate::alerts::{AlertMap, Severity};
use crate::ids::{ItemInfo, RelationInfo};
use crate::{EntityInfo, State};
use crate::{Error, ItemId, RelationId, Result};

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Copy, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(from_wasm_abi, into_wasm_abi, type = "\"ok\" | Severity")
)]
pub enum Status {
    Ok,
    Nok(Severity),
}

impl Status {
    pub fn as_severity(self) -> Option<Severity> {
        match self {
            Status::Nok(severity) => Some(severity),
            Status::Ok => None,
        }
    }
}

impl Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Status::Ok => write!(f, "ok"),
            Status::Nok(s) => write!(f, "{s}"),
        }
    }
}
impl FromStr for Status {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        match s {
            "ok" => Ok(Self::Ok),
            _ => Ok(Self::Nok(Severity::from_str(s)?)),
        }
    }
}

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
pub struct VersionedStatusMap {
    pub items: BTreeMap<ItemId, SingleVersioned<StatusChange>>,
    pub relations: BTreeMap<RelationId, SingleVersioned<StatusChange>>,
}

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct StatusMap {
    items: BTreeMap<ItemId, (EntityInfo, Status)>,
    relations: BTreeMap<RelationId, (EntityInfo, Status)>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "EntityInfo & StatusChange"))]
pub struct StatusDoc {
    #[serde(flatten)]
    pub entity: EntityInfo,
    #[serde(flatten)]
    pub change: StatusChange,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct StatusChange {
    pub previous: Option<Status>,
    pub status: Status,
}

impl VersionedStatusMap {
    pub fn new(status: BTreeMap<ObjectId, SingleVersioned<StatusDoc>>) -> Self {
        let mut items = BTreeMap::new();
        let mut relations = BTreeMap::new();
        for (_, versioned) in status {
            match versioned.value.entity {
                EntityInfo::Item {
                    item: ItemInfo { item_id: id, .. },
                    ..
                } => {
                    items.insert(
                        id,
                        SingleVersioned {
                            version: versioned.version,
                            value: versioned.value.change,
                        },
                    );
                }
                EntityInfo::Relation {
                    relation:
                        RelationInfo {
                            relation_id: id, ..
                        },
                    ..
                } => {
                    relations.insert(
                        id,
                        SingleVersioned {
                            version: versioned.version,
                            value: versioned.value.change,
                        },
                    );
                }
            }
        }
        Self { items, relations }
    }

    // pub fn iter(&self) -> impl Iterator<Item = (&EntityId, &SingleVersioned<StatusChange>)> {
    //     self.0.iter()
    // }

    pub fn update(
        &mut self,
        // This "now" value will not exactly correspond to the one in
        // db...
        now: DateTime<Utc>,
        status: StatusMap,
    ) -> BTreeMap<ObjectId, Option<StatusDoc>> {
        let mut changes = BTreeMap::new();

        update_status(now, &mut self.items, status.items, &mut changes, |id| {
            id.uuid()
        });
        update_status(
            now,
            &mut self.relations,
            status.relations,
            &mut changes,
            |id| id.uuid(),
        );

        changes
    }
}

fn update_status<K: Ord + Eq, F: Fn(&K) -> Uuid>(
    now: DateTime<Utc>,
    dest: &mut BTreeMap<K, SingleVersioned<StatusChange>>,
    new: BTreeMap<K, (EntityInfo, Status)>,
    changes: &mut BTreeMap<ObjectId, Option<StatusDoc>>,
    uuid: F,
) {
    let mut prev = std::mem::take(dest);
    for (id, (entity, new_status)) in new {
        match prev.remove(&id) {
            Some(prev) if prev.value.status == new_status => {
                dest.insert(id, prev);
            }
            Some(prev) => {
                let change = StatusChange {
                    previous: Some(prev.value.status),
                    status: new_status,
                };
                changes.insert(
                    ObjectId::from(uuid(&id).to_string()),
                    Some(StatusDoc {
                        entity,
                        change: change.clone(),
                    }),
                );
                dest.insert(id, prev.update(now, change));
            }
            None => {
                let change = StatusChange {
                    previous: None,
                    status: new_status,
                };
                changes.insert(
                    ObjectId::from(uuid(&id).to_string()),
                    Some(StatusDoc {
                        entity,
                        change: change.clone(),
                    }),
                );
                dest.insert(id, SingleVersioned::new(now, change));
            }
        }
    }

    for (id, _removed) in prev {
        changes.insert(ObjectId::from(uuid(&id).to_string()), None);
    }
}

impl StatusMap {
    pub fn from_alert_map(state: &State, alerts: &AlertMap) -> Self {
        Self {
            items: state
                .items
                .iter_items()
                .map(|(item_id, item)| {
                    let entity_info =
                        item.entity_info(item_id.clone(), &state.items.items, &state.types);
                    let status = alerts
                        .items
                        .get(item_id)
                        .into_iter()
                        .flat_map(|alerts| alerts.values())
                        .flat_map(|alerts| alerts.values())
                        .map(|alert| alert.alert.severity)
                        .max()
                        .map_or(Status::Ok, Status::Nok);
                    (item_id.clone(), (entity_info, status))
                })
                .collect(),
            relations: state
                .items
                .iter_relations()
                .map(|(rel_id, rel)| {
                    let entity_info =
                        rel.entity_info(rel_id.clone(), &state.items.items, &state.types);
                    let status = alerts
                        .relations
                        .get(rel_id)
                        .into_iter()
                        .flat_map(|alerts| alerts.values())
                        .flat_map(|alerts| alerts.values())
                        .map(|alert| alert.alert.severity)
                        .max()
                        .map_or(Status::Ok, Status::Nok);
                    (rel_id.clone(), (entity_info, status))
                })
                .collect(),
        }
    }

    // pub fn from_alert_metrics(state: &State, alerts: &[AlertMetric]) -> Self {
    //     log::debug!("building status map from {} alerts", alerts.len());

    //     let alerts_by_item = alerts
    //         .iter()
    //         // Rule could have been deleted in the meantime. This
    //         // should not be handled as an error.
    //         .filter_map(|alert| {
    //             Some((
    //                 &state.alert_rules.get_rule_spec(&alert.alertrule)?.item,
    //                 alert,
    //             ))
    //         })
    //         .fold(BTreeMap::new(), |mut map, (item, alert)| {
    //             map.entry(item).or_insert_with(Vec::new).push(alert);

    //             map
    //         });

    //     let item_status = state.items.iter_items().map(|(item_id, item)| {
    //         (
    //             EntityId::Item {
    //                 item_id: item_id.clone(),
    //                 item_type: item.item_type_id().clone(),
    //             },
    //             item.item_type(&state.types)
    //                 .prometheus_metrics
    //                 .iter()
    //                 .filter_map(|(item_name, (_, link))| {
    //                     Some((alerts_by_item.get(item_name)?, link))
    //                 })
    //                 .flat_map(|(alerts, link)| {
    //                     let select = link.prometheus_query(item, &state.items, &state.types);
    //                     alerts.iter().filter(move |alert| {
    //                         select.0.iter().all(|(label, selector)| {
    //                             let value = alert.labels.get(label).map_or("", |s| s.as_str());
    //                             selector.matches(value)
    //                         })
    //                     })
    //                 })
    //                 .map(|alert| alert.severity)
    //                 .max()
    //                 .map_or(Status::Ok, Status::Nok),
    //         )
    //     });
    //     let relation_status = state.items.iter_relations().map(|(rel_id, rel)| {
    //         (
    //             EntityId::Relation {
    //                 relation_id: rel_id.clone(),
    //                 relation_type: rel.relation_type_id().clone(),
    //             },
    //             rel.relation_type(&state.types)
    //                 .prometheus_metrics
    //                 .iter()
    //                 .filter_map(|(item_name, (_, link))| {
    //                     Some((alerts_by_item.get(item_name)?, link))
    //                 })
    //                 .flat_map(|(alerts, link)| {
    //                     let select = link.prometheus_query(rel, &state.items, &state.types);
    //                     alerts.iter().filter(move |alert| {
    //                         select.0.iter().all(|(label, selector)| {
    //                             let value = alert.labels.get(label).map_or("", |s| s.as_str());
    //                             selector.matches(value)
    //                         })
    //                     })
    //                 })
    //                 .map(|alert| alert.severity)
    //                 .max()
    //                 .map_or(Status::Ok, Status::Nok),
    //         )
    //     });
    //     let status_map: BTreeMap<EntityId, Status> = item_status.chain(relation_status).collect();
    //     log::debug!("built status map with {} statusses", status_map.len());
    //     Self(status_map)
    // }
}

impl StatusDoc {
    pub fn dbschema() -> dbschema::DbSchema {
        StructSchema::new()
            .field("entity", EntityInfo::schema())
            .serde_flatten("entity")
            .unwrap()
            .field("previous", OptionSchema::new(StringSchema::new()))
            .field("status", StringSchema::new())
            .into()
    }
}
