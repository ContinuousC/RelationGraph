/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    str::FromStr,
};

use dbschema::{DbSchema, HasSchema, ObjectId, OptionSchema, StringSchema, StructSchema};
use serde::{Deserialize, Deserializer, Serialize};
use wrapper::Identity;

use crate::{EntityInfo, EntityTypeRef, ItemId, PackageId, PropertyId, Result, Types};

use super::resolved;

#[derive(Serialize, Deserialize)]
pub struct DbItems(pub HashMap<ObjectId, DbItem>);

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct DbItem {
    #[serde(flatten)]
    pub entity: EntityInfo,
    pub parent: Option<ItemId>,
    #[serde(deserialize_with = "deserialize_properties")]
    pub properties: BTreeMap<PackageId, BTreeMap<PropertyId, serde_json::Value>>,
}

// #[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
// #[serde(rename_all = "snake_case")]
// pub enum DbItemType {
//     Item,
//     Relation,
// }

impl DbItems {
    pub fn resolve(self, types: &Types) -> Result<resolved::Items<Identity>> {
        let mut items = resolved::Items::from_db(self, types)?;
        items.resolve()?;
        Ok(items)
    }

    pub fn resolve_tx(self, types: &Types) -> Result<resolved::TxItems> {
        let mut items = resolved::TxItems::from_db(self, types)?;
        items.resolve()?;
        Ok(items)
    }

    /// Clean unknown parents and relations to unknown items, to allow
    /// loading partial item sets.
    pub fn cleanup_partial(self) -> Self {
        let items = self
            .0
            .iter()
            .filter_map(|(id, item)| {
                matches!(item.entity, EntityInfo::Item { .. }).then_some(())?;
                ItemId::from_str(&id.to_string()).ok()
            })
            .collect::<HashSet<_>>();

        Self(
            self.0
                .into_iter()
                .filter(|(_, item)| match &item.entity {
                    EntityInfo::Item { .. } => true,
                    EntityInfo::Relation { source, target, .. } => {
                        items.contains(&source.item_id) && items.contains(&target.item_id)
                    }
                })
                .map(|(id, mut item)| {
                    if item
                        .parent
                        .as_ref()
                        .is_some_and(|parent| !items.contains(parent))
                    {
                        item.parent.take();
                    }
                    (id, item)
                })
                .collect(),
        )
    }
}

impl FromIterator<(ObjectId, DbItem)> for DbItems {
    fn from_iter<T: IntoIterator<Item = (ObjectId, DbItem)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl DbItem {
    pub fn schema(types: &Types) -> DbSchema {
        StructSchema::new()
            .field("entity", EntityInfo::schema())
            .serde_flatten("entity")
            .unwrap()
            .field("parent", OptionSchema::new(StringSchema::new()))
            .field("properties", OptionSchema::new(types.property_schema()))
            .into()
    }

    // pub fn from_serial_item(serial: serial::Item, pkg: Option<&PackageId>) -> Self {
    //     Self {
    //         entity: EntityInfo::Item {
    //             item: ItemInfo {
    //                 item_id: todo!(),
    //                 item_name: todo!(),
    //                 item_type: serial.item_type.resolve_opt(pkg),
    //             },
    //         },
    //         parent: serial.parent,
    //         properties: serial.properties.into_iter().fold(
    //             BTreeMap::new(),
    //             |mut props, (prop_id, prop)| {
    //                 let prop_id = prop_id.resolve_opt(pkg);
    //                 props
    //                     .entry(prop_id.package().clone())
    //                     .or_default()
    //                     .insert(prop_id.local().clone(), prop);
    //                 props
    //             },
    //         ),
    //     }
    // }

    // pub fn from_serial_relation(serial: serial::Relation, pkg: Option<&PackageId>) -> Self {
    //     Self {
    //         r#type: DbItemType::Relation,
    //         relation_type: Some(serial.relation_type.resolve_opt(pkg)),
    //         source: Some(serial.source),
    //         target: Some(serial.target),
    //         properties: serial.properties.into_iter().fold(
    //             BTreeMap::new(),
    //             |mut props, (prop_id, prop)| {
    //                 let prop_id = prop_id.resolve_opt(pkg);
    //                 props
    //                     .entry(prop_id.package().clone())
    //                     .or_default()
    //                     .insert(prop_id.local().clone(), prop);
    //                 props
    //             },
    //         ),
    //         item_type: None,
    //         parent: None,
    //     }
    // }

    pub fn get_entity_type<'a>(&self, types: &'a Types) -> Option<EntityTypeRef<'a>> {
        types.get_entity_type(&self.entity.entity_type_id())
    }
}
type PropMap = BTreeMap<PackageId, BTreeMap<PropertyId, serde_json::Value>>;
type OptPropMap = BTreeMap<PackageId, Option<BTreeMap<PropertyId, Option<serde_json::Value>>>>;

fn deserialize_properties<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<PropMap, D::Error> {
    let value: Option<OptPropMap> = Deserialize::deserialize(deserializer)?;
    Ok(value.map_or_else(BTreeMap::new, |value| {
        value
            .into_iter()
            .filter_map(|(key, value)| {
                Some((
                    key,
                    value?
                        .into_iter()
                        .filter_map(|(key, value)| Some((key, value?)))
                        .collect(),
                ))
            })
            .collect()
    }))
}
