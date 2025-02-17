/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use dbschema::{Identified, ObjectId, SingleVersioned};
use prometheus_core::LabelName;

use crate::{
    ids::{ItemInfo, RelationInfo},
    EntityInfo, ItemId, RelationId, State,
};

use super::{AlertDoc, AlertInfo, AlertRuleTemplateName};

#[derive(Serialize, Deserialize, Default)]
pub struct VersionedAlertMap {
    pub items: BTreeMap<ItemId, AlertsByRule<Identified<SingleVersioned<AlertInfo>>>>,
    pub relations: BTreeMap<RelationId, AlertsByRule<Identified<SingleVersioned<AlertInfo>>>>,
    pub unmatched: AlertsByRule<Identified<SingleVersioned<AlertInfo>>>,
}

#[derive(Serialize, Deserialize)]
pub struct AlertMap {
    pub items: BTreeMap<ItemId, AlertsByRule<AlertDoc>>,
    pub relations: BTreeMap<RelationId, AlertsByRule<AlertDoc>>,
    pub unmatched: AlertsByRule<AlertDoc>,
}

pub type AlertsByRule<T> = BTreeMap<AlertRuleTemplateName, AlertsByLabels<T>>;
pub type AlertsByLabels<T> = BTreeMap<BTreeMap<LabelName, String>, T>;

impl VersionedAlertMap {
    /// Load versioned alert map from database. Returns the map and
    /// the ids of duplicate objects that should be removed for
    /// self-healing.
    pub fn new(
        alerts: BTreeMap<ObjectId, SingleVersioned<AlertDoc>>,
    ) -> (Self, BTreeSet<ObjectId>) {
        let mut items = BTreeMap::new();
        let mut relations = BTreeMap::new();
        let mut unmatched = BTreeMap::new();
        let mut selfheal = BTreeSet::new();

        for (object_id, versioned) in alerts {
            match versioned.value.entity {
                Some(EntityInfo::Item {
                    item: ItemInfo { item_id: id, .. },
                    ..
                }) => {
                    if let Some(old) = items
                        .entry(id)
                        .or_insert_with(BTreeMap::new)
                        .entry(versioned.value.alert.alert_rule.clone())
                        .or_insert_with(BTreeMap::new)
                        .insert(
                            versioned.value.alert.labels.clone(),
                            Identified {
                                object_id,
                                value: SingleVersioned {
                                    version: versioned.version,
                                    value: versioned.value.alert,
                                },
                            },
                        )
                    {
                        selfheal.insert(old.object_id);
                    }
                }
                Some(EntityInfo::Relation {
                    relation:
                        RelationInfo {
                            relation_id: id, ..
                        },
                    ..
                }) => {
                    if let Some(old) = relations
                        .entry(id)
                        .or_insert_with(BTreeMap::new)
                        .entry(versioned.value.alert.alert_rule.clone())
                        .or_insert_with(BTreeMap::new)
                        .insert(
                            versioned.value.alert.labels.clone(),
                            Identified {
                                object_id,
                                value: SingleVersioned {
                                    version: versioned.version,
                                    value: versioned.value.alert,
                                },
                            },
                        )
                    {
                        selfheal.insert(old.object_id);
                    }
                }

                None => {
                    if let Some(old) = unmatched
                        .entry(versioned.value.alert.alert_rule.clone())
                        .or_insert_with(BTreeMap::new)
                        .insert(
                            versioned.value.alert.labels.clone(),
                            Identified {
                                object_id,
                                value: SingleVersioned {
                                    version: versioned.version,
                                    value: versioned.value.alert,
                                },
                            },
                        )
                    {
                        selfheal.insert(old.object_id);
                    }
                }
            }
        }

        (
            Self {
                items,
                relations,
                unmatched,
            },
            selfheal,
        )
    }

    pub fn update(
        &mut self,
        now: DateTime<Utc>,
        alert_map: AlertMap,
    ) -> BTreeMap<ObjectId, Option<AlertDoc>> {
        let mut changes = BTreeMap::new();
        update_alerts(now, &mut self.items, alert_map.items, &mut changes);
        update_alerts(now, &mut self.relations, alert_map.relations, &mut changes);
        update_unmatched(now, &mut self.unmatched, alert_map.unmatched, &mut changes);
        changes
    }
}

fn update_alerts<K: Ord + Eq>(
    now: DateTime<Utc>,
    dest: &mut BTreeMap<K, AlertsByRule<Identified<SingleVersioned<AlertInfo>>>>,
    new: BTreeMap<K, AlertsByRule<AlertDoc>>,
    changes: &mut BTreeMap<ObjectId, Option<AlertDoc>>,
) {
    let mut prev = std::mem::take(dest);

    for (id, new_alerts_by_rule) in new {
        let mut prev_alerts_by_rule = prev.get_mut(&id);
        let alerts_by_rule = dest.entry(id).or_default();
        for (rule_name, new_alerts_by_labels) in new_alerts_by_rule {
            let mut prev_alerts_by_labels = prev_alerts_by_rule
                .as_mut()
                .and_then(|alerts| alerts.get_mut(&rule_name));
            let alerts_by_labels = alerts_by_rule.entry(rule_name).or_default();
            for (labels, new_alert) in new_alerts_by_labels {
                match prev_alerts_by_labels
                    .as_mut()
                    .and_then(|alerts| alerts.remove(&labels))
                {
                    Some(prev_alert) => {
                        let id = prev_alert.object_id;
                        alerts_by_labels.insert(
                            labels,
                            Identified {
                                object_id: id.clone(),
                                value: prev_alert.value.update(now, new_alert.alert.clone()),
                            },
                        );
                        changes.insert(id, Some(new_alert));
                    }
                    None => {
                        let id = ObjectId::new();
                        alerts_by_labels.insert(
                            labels,
                            Identified {
                                object_id: id.clone(),
                                value: SingleVersioned::new(now, new_alert.alert.clone()),
                            },
                        );
                        changes.insert(id, Some(new_alert));
                    }
                }
            }
        }
    }

    for (_, removed_alerts_by_rule) in prev {
        for (_, removed_alerts_by_labels) in removed_alerts_by_rule {
            for (_, removed_alert) in removed_alerts_by_labels {
                changes.insert(removed_alert.object_id, None);
            }
        }
    }
}

fn update_unmatched(
    now: DateTime<Utc>,
    dest: &mut AlertsByRule<Identified<SingleVersioned<AlertInfo>>>,
    new: AlertsByRule<AlertDoc>,
    changes: &mut BTreeMap<ObjectId, Option<AlertDoc>>,
) {
    let mut prev = std::mem::take(dest);

    for (rule_name, new_alerts_by_labels) in new {
        let mut prev_alerts_by_labels = prev.get_mut(&rule_name);
        let alerts_by_labels = dest.entry(rule_name).or_default();
        for (labels, new_alert) in new_alerts_by_labels {
            match prev_alerts_by_labels
                .as_mut()
                .and_then(|alerts| alerts.remove(&labels))
            {
                Some(prev_alert) => {
                    let id = prev_alert.object_id;
                    alerts_by_labels.insert(
                        labels,
                        Identified {
                            object_id: id.clone(),
                            value: prev_alert.value.update(now, new_alert.alert.clone()),
                        },
                    );
                    changes.insert(id, Some(new_alert));
                }
                None => {
                    let id = ObjectId::new();
                    alerts_by_labels.insert(
                        labels,
                        Identified {
                            object_id: id.clone(),
                            value: SingleVersioned::new(now, new_alert.alert.clone()),
                        },
                    );
                    changes.insert(id, Some(new_alert));
                }
            }
        }
    }

    for (_, removed_alerts_by_labels) in prev {
        for (_, removed_alert) in removed_alerts_by_labels {
            changes.insert(removed_alert.object_id, None);
        }
    }
}

impl AlertMap {
    pub fn from_alerts(state: &State, alerts: impl IntoIterator<Item = AlertInfo>) -> Self {
        //log::debug!("building alert map from {} alerts", alerts.len());
        let mut alerts_by_item = alerts
            .into_iter()
            .filter_map(|alert| {
                // Rule could have been deleted in the meantime. This
                // should not be handled as an error.
                let item = state
                    .alert_rules
                    .get_rule_form(&alert.alert_rule)?
                    .rule_form
                    .value
                    .item();
                Some((item, alert))
            })
            .fold(
                BTreeMap::new(),
                |mut map: BTreeMap<_, Vec<_>>, (item, alert)| {
                    map.entry(item).or_default().push(alert);
                    map
                },
            );

        let mut items = BTreeMap::new();
        state.items.iter_items().for_each(|(item_id, item)| {
            state
                .types
                .iter_type_and_supertypes(item.item_type_ref())
                .for_each(|(_, item_type)| {
                    item_type
                        .prometheus_metrics
                        .iter()
                        .for_each(|(prom_item, (_, link))| {
                            if let Some(alerts) = alerts_by_item.get_mut(prom_item) {
                                let m =
                                    link.prometheus_query(item, &state.items.items, &state.types);
                                std::mem::take(alerts).into_iter().for_each(|alert| {
                                    if m.matches(&alert.labels) {
                                        match items
                                            .entry(item_id.clone())
                                            .or_insert_with(BTreeMap::new)
                                            .entry(alert.alert_rule.clone())
                                            .or_insert_with(BTreeMap::new)
                                            .entry(alert.labels.clone())
                                        {
                                            Entry::Vacant(ent) => {
                                                ent.insert(AlertDoc {
                                                    entity: Some(item.entity_info(
                                                        item_id.clone(),
                                                        &state.items.items,
                                                        &state.types,
                                                    )),
                                                    alert,
                                                });
                                            }
                                            Entry::Occupied(mut ent) => {
                                                if alert.severity > ent.get().alert.severity {
                                                    ent.insert(AlertDoc {
                                                        entity: Some(item.entity_info(
                                                            item_id.clone(),
                                                            &state.items.items,
                                                            &state.types,
                                                        )),
                                                        alert,
                                                    });
                                                }
                                            }
                                        }
                                    } else {
                                        alerts.push(alert)
                                    }
                                });
                            }
                        });
                })
        });

        let mut relations = BTreeMap::new();
        state.items.iter_relations().for_each(|(rel_id, rel)| {
            rel.relation_type(&state.types)
                .prometheus_metrics
                .iter()
                .for_each(|(prom_item, (_, link))| {
                    if let Some(alerts) = alerts_by_item.get_mut(prom_item) {
                        let m = link.prometheus_query(rel, &state.items.items, &state.types);
                        std::mem::take(alerts).into_iter().for_each(|alert| {
                            if m.matches(&alert.labels) {
                                match relations
                                    .entry(rel_id.clone())
                                    .or_insert_with(BTreeMap::new)
                                    .entry(alert.alert_rule.clone())
                                    .or_insert_with(BTreeMap::new)
                                    .entry(alert.labels.clone())
                                {
                                    Entry::Vacant(ent) => {
                                        ent.insert(AlertDoc {
                                            entity: Some(rel.entity_info(
                                                rel_id.clone(),
                                                &state.items.items,
                                                &state.types,
                                            )),
                                            alert,
                                        });
                                    }
                                    Entry::Occupied(mut ent) => {
                                        if alert.severity > ent.get().alert.severity {
                                            ent.insert(AlertDoc {
                                                entity: Some(rel.entity_info(
                                                    rel_id.clone(),
                                                    &state.items.items,
                                                    &state.types,
                                                )),
                                                alert,
                                            });
                                        }
                                    }
                                }
                            } else {
                                alerts.push(alert);
                            }
                        });
                    }
                });
        });

        let mut unmatched = BTreeMap::new();

        alerts_by_item.into_values().flatten().for_each(|alert| {
            match unmatched
                .entry(alert.alert_rule.clone())
                .or_insert_with(BTreeMap::new)
                .entry(alert.labels.clone())
            {
                Entry::Vacant(ent) => {
                    ent.insert(AlertDoc {
                        entity: None,
                        alert,
                    });
                }
                Entry::Occupied(mut ent) => {
                    if alert.severity > ent.get().alert.severity {
                        ent.insert(AlertDoc {
                            entity: None,
                            alert,
                        });
                    }
                }
            }
        });

        log::debug!(
            "built alert map with {} item alerts, {} relation alerts, {} unmatched alerts",
            items
                .values()
                .map(|alerts| alerts.values().map(|alerts| alerts.len()).sum::<usize>())
                .sum::<usize>(),
            relations
                .values()
                .map(|alerts| alerts.values().map(|alerts| alerts.len()).sum::<usize>())
                .sum::<usize>(),
            unmatched.values().map(|alerts| alerts.len()).sum::<usize>()
        );

        Self {
            items,
            relations,
            unmatched,
        }
    }
}
