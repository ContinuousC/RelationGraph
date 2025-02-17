/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Display;
#[cfg(not(target_family = "wasm"))]
use std::path::Path;

use chrono::{DateTime, Utc};
use dbschema::SingleVersioned;
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use serde_json::json;
use wrapper::{Identity, Wrapper};

use crate::alerts::VersionedAlertMap;
use crate::db::DbItem;
#[cfg(not(target_family = "wasm"))]
use crate::error::Error;
use crate::error::Result;
use crate::ids::{
    ItemId, ItemInfo, ItemTypeId, PackageId, PropertyId, RelationId, RelationInfo, RelationTypeId,
    Relative,
};
use crate::status::{Status, StatusChange, VersionedStatusMap};
use crate::types::resolved::Types;
use crate::{Absolute, EntityInfo};

use super::augmented::{Augment, Augmented};
use super::resolved;

#[derive(Serialize, Deserialize, Debug)]
#[serde(bound(
    serialize = "W::Wrap<Item>: Serialize, W::Wrap<Relation>: Serialize",
    deserialize = "W::Wrap<Item>: Deserialize<'de>, W::Wrap<Relation>: Deserialize<'de>"
))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Items<W: Wrapper = Identity> {
    #[serde(default)] //, skip_serializing_if = "HashMap::is_empty")]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: ItemId]: Item }"))]
    pub items: HashMap<ItemId, W::Wrap<Item>>,
    #[serde(default)] //, skip_serializing_if = "HashMap::is_empty")]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: RelationId]: Relation }"))]
    pub relations: HashMap<RelationId, W::Wrap<Relation>>,
}

impl<W: Wrapper> Clone for Items<W>
where
    W::Wrap<Item>: Clone,
    W::Wrap<Relation>: Clone,
{
    fn clone(&self) -> Self {
        let Items { items, relations } = self;
        Items {
            items: items.clone(),
            relations: relations.clone(),
        }
    }
}

impl<W: Wrapper> PartialEq for Items<W>
where
    W::Wrap<Item>: PartialEq,
    W::Wrap<Relation>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let Items {
            items: items_self,
            relations: relations_self,
        } = self;
        let Items {
            items: items_other,
            relations: relations_other,
        } = other;
        items_self.eq(items_other) && relations_self.eq(relations_other)
    }
}

impl<W: Wrapper> Eq for Items<W>
where
    W::Wrap<Item>: Eq,
    W::Wrap<Relation>: Eq,
{
}

#[cfg(feature = "schemars")]
// Adapted from schemars::JsonSchema auto-derive macro
impl<W> schemars::JsonSchema for Items<W>
where
    W: Wrapper,
    W::Wrap<Item>: schemars::JsonSchema,
    W::Wrap<Relation>: schemars::JsonSchema,
{
    fn schema_name() -> String {
        format!("Items_for_{}", W::Wrap::<Item>::schema_name())
    }

    fn schema_id() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Owned(format!(
            std::concat!(std::module_path!(), "::", "Items_for_{}"),
            W::Wrap::<Item>::schema_id()
        ))
    }

    // Copied from schemars::JsonSchema auto-derive macro
    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        {
            let mut schema_object = schemars::schema::SchemaObject {
                instance_type: Some(schemars::schema::InstanceType::Object.into()),
                ..Default::default()
            };
            let object_validation = schema_object.object();
            {
                schemars::_private::insert_object_property::<HashMap<ItemId, W::Wrap<Item>>>(
                    object_validation,
                    "items",
                    true,
                    false,
                    schemars::_private::metadata::add_default(
                        gen.subschema_for::<HashMap<ItemId, W::Wrap<Item>>>(),
                        {
                            let default = <HashMap<ItemId, W::Wrap<Item>>>::default();
                            if HashMap::is_empty(&default) {
                                None
                            } else {
                                Some(default)
                            }
                        }
                        .and_then(|d| schemars::_schemars_maybe_to_value!(d)),
                    ),
                );
            }
            {
                schemars::_private::insert_object_property::<HashMap<RelationId, W::Wrap<Relation>>>(
                    object_validation,
                    "relations",
                    true,
                    false,
                    schemars::_private::metadata::add_default(
                        gen.subschema_for::<HashMap<RelationId, W::Wrap<Relation>>>(),
                        {
                            let default = <HashMap<RelationId, W::Wrap<Relation>>>::default();
                            if HashMap::is_empty(&default) {
                                None
                            } else {
                                Some(default)
                            }
                        }
                        .and_then(|d| schemars::_schemars_maybe_to_value!(d)),
                    ),
                );
            }
            schemars::schema::Schema::Object(schema_object)
        }
    }
}

#[cfg(feature = "apistos")]
// Adapted from auto-derived impl.
impl<W> apistos::ApiComponent for Items<W>
where
    W: Wrapper,
    W::Wrap<Item>: schemars::JsonSchema,
    W::Wrap<Relation>: schemars::JsonSchema,
{
    fn child_schemas() -> Vec<(String, apistos::reference_or::ReferenceOr<apistos::Schema>)> {
        let settings = schemars::gen::SchemaSettings::openapi3();
        let gen = settings.into_generator();
        let schema: apistos::RootSchema = gen.into_root_schema_for::<Self>();
        let mut schemas: Vec<(String, apistos::reference_or::ReferenceOr<apistos::Schema>)> =
            vec![];
        for (def_name, mut def) in schema.definitions {
            match &mut def {
                schemars::schema::Schema::Bool(_) => {}

                schemars::schema::Schema::Object(schema) => {
                    if let Some(one_of) = schema.subschemas.as_mut().and_then(|s| s.one_of.as_mut())
                    {
                        for s in &mut *one_of {
                            match s {
                                schemars::schema::Schema::Bool(_) => {}

                                schemars::schema::Schema::Object(sch_obj) => {
                                    if let Some(obj) = sch_obj.object.as_mut() {
                                        if obj.properties.len() == 1 {
                                            if let Some((prop_name, _)) =
                                                obj.properties.iter().next()
                                            {
                                                match sch_obj.metadata.as_mut() {
                                                    None => {
                                                        sch_obj.metadata = Some(Box::new(
                                                            schemars::schema::Metadata {
                                                                title: Some(prop_name.clone()),
                                                                ..Default::default()
                                                            },
                                                        ));
                                                    }
                                                    Some(m) => {
                                                        m.title = m
                                                            .title
                                                            .clone()
                                                            .or_else(|| Some(prop_name.clone()))
                                                    }
                                                };
                                            }
                                        } else if let Some(enum_values) =
                                            obj.properties.iter_mut().find_map(|(_, p)| match p {
                                                schemars::schema::Schema::Bool(_) => None,
                                                schemars::schema::Schema::Object(sch_obj) => {
                                                    sch_obj.enum_values.as_mut()
                                                }
                                            })
                                        {
                                            if enum_values.len() == 1 {
                                                if let Some(schemars::_serde_json::Value::String(
                                                    prop_name,
                                                )) = enum_values.first()
                                                {
                                                    match sch_obj.metadata.as_mut() {
                                                        None => {
                                                            sch_obj.metadata = Some(Box::new(
                                                                schemars::schema::Metadata {
                                                                    title: Some(prop_name.clone()),
                                                                    ..Default::default()
                                                                },
                                                            ));
                                                        }
                                                        Some(m) => {
                                                            m.title = m
                                                                .title
                                                                .clone()
                                                                .or_else(|| Some(prop_name.clone()))
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    } else if let Some(enum_values) = sch_obj.enum_values.as_mut() {
                                        if enum_values.len() == 1 {
                                            if let Some(schemars::_serde_json::Value::String(
                                                prop_name,
                                            )) = enum_values.first()
                                            {
                                                match sch_obj.metadata.as_mut() {
                                                    None => {
                                                        sch_obj.metadata = Some(Box::new(
                                                            schemars::schema::Metadata {
                                                                title: Some(prop_name.clone()),
                                                                ..Default::default()
                                                            },
                                                        ));
                                                    }
                                                    Some(m) => {
                                                        m.title = m
                                                            .title
                                                            .clone()
                                                            .or_else(|| Some(prop_name.clone()))
                                                    }
                                                }
                                            }
                                        }
                                    };
                                }
                            }
                        }
                    }
                }
            }
            schemas.push((def_name, apistos::reference_or::ReferenceOr::Object(def)));
        }
        schemas
    }
    fn schema() -> Option<(String, apistos::reference_or::ReferenceOr<apistos::Schema>)> {
        let (name, schema) = {
            let schema_name = <Self as schemars::JsonSchema>::schema_name();
            let settings = schemars::gen::SchemaSettings::openapi3();
            let gen = settings.into_generator();
            let mut schema: apistos::RootSchema = gen.into_root_schema_for::<Self>();
            if let Some(one_of) = schema
                .schema
                .subschemas
                .as_mut()
                .and_then(|s| s.one_of.as_mut())
            {
                for s in &mut *one_of {
                    match s {
                        schemars::schema::Schema::Bool(_) => {}

                        schemars::schema::Schema::Object(sch_obj) => {
                            if let Some(obj) = sch_obj.object.as_mut() {
                                if obj.properties.len() == 1 {
                                    if let Some((prop_name, _)) = obj.properties.iter().next() {
                                        match sch_obj.metadata.as_mut() {
                                            None => {
                                                sch_obj.metadata =
                                                    Some(Box::new(schemars::schema::Metadata {
                                                        title: Some(prop_name.clone()),
                                                        ..Default::default()
                                                    }));
                                            }
                                            Some(m) => {
                                                m.title = m
                                                    .title
                                                    .clone()
                                                    .or_else(|| Some(prop_name.clone()))
                                            }
                                        };
                                    }
                                } else if let Some(enum_values) =
                                    obj.properties.iter_mut().find_map(|(_, p)| match p {
                                        schemars::schema::Schema::Bool(_) => None,
                                        schemars::schema::Schema::Object(sch_obj) => {
                                            sch_obj.enum_values.as_mut()
                                        }
                                    })
                                {
                                    if enum_values.len() == 1 {
                                        if let Some(schemars::_serde_json::Value::String(
                                            prop_name,
                                        )) = enum_values.first()
                                        {
                                            match sch_obj.metadata.as_mut() {
                                                None => {
                                                    sch_obj.metadata = Some(Box::new(
                                                        schemars::schema::Metadata {
                                                            title: Some(prop_name.clone()),
                                                            ..Default::default()
                                                        },
                                                    ));
                                                }
                                                Some(m) => {
                                                    m.title = m
                                                        .title
                                                        .clone()
                                                        .or_else(|| Some(prop_name.clone()))
                                                }
                                            }
                                        }
                                    }
                                }
                            } else if let Some(enum_values) = sch_obj.enum_values.as_mut() {
                                if enum_values.len() == 1 {
                                    if let Some(schemars::_serde_json::Value::String(prop_name)) =
                                        enum_values.first()
                                    {
                                        match sch_obj.metadata.as_mut() {
                                            None => {
                                                sch_obj.metadata =
                                                    Some(Box::new(schemars::schema::Metadata {
                                                        title: Some(prop_name.clone()),
                                                        ..Default::default()
                                                    }));
                                            }
                                            Some(m) => {
                                                m.title = m
                                                    .title
                                                    .clone()
                                                    .or_else(|| Some(prop_name.clone()))
                                            }
                                        }
                                    }
                                }
                            };
                        }
                    }
                }
            }
            (
                schema_name,
                apistos::reference_or::ReferenceOr::Object(schemars::schema::Schema::Object(
                    schema.schema,
                )),
            )
        };
        Some((name, schema))
    }
}

impl<W: Wrapper> Default for Items<W> {
    fn default() -> Self {
        Self {
            items: HashMap::default(),
            relations: HashMap::default(),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Item {
    #[cfg_attr(feature = "tsify", tsify(type = "string"))]
    pub item_type: Relative<ItemTypeId>,
    #[serde(default)] //, skip_serializing_if = "Option::is_none")]
    #[cfg_attr(feature = "tsify", tsify(type = "ItemId | null", optional))]
    pub parent: Option<ItemId>,
    // #[serde(default, skip_serializing_if = "BTreeSet::is_empty")]
    // #[cfg_attr(feature = "tsify", tsify(type = "[ItemId]"))]
    // pub children: BTreeSet<ItemId>,
    #[serde(default)] //, skip_serializing_if = "BTreeMap::is_empty")]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: PropertyId]: any }"))]
    pub properties: BTreeMap<Relative<PropertyId>, PropertyValue>,
    // #[serde(default)]
    // #[cfg_attr(
    //     feature = "tsify",
    //     tsify(type = "{ [key: RelationTypeId]: RelationId[] }")
    // )]
    // pub source_of: BTreeMap<Absolute<RelationTypeId>, BTreeSet<RelationId>>,
    // #[serde(default)]
    // #[cfg_attr(
    //     feature = "tsify",
    //     tsify(type = "{ [key: RelationTypeId]: RelationId[] }")
    // )]
    // pub target_of: BTreeMap<Absolute<RelationTypeId>, BTreeSet<RelationId>>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum PropertyValue {
    String(String),
    Integer(i64),
    Float(#[cfg_attr(feature = "schemars", schemars(with = "f64"))] OrderedFloat<f64>),
    Time(DateTime<Utc>),
    Map(BTreeMap<String, PropertyValue>),
    List(Vec<PropertyValue>),
}

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Relation {
    #[cfg_attr(feature = "tsify", tsify(type = "string"))]
    pub relation_type: Relative<RelationTypeId>,
    #[serde(default)] //, skip_serializing_if = "BTreeMap::is_empty")]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: PropertyId]: any }"))]
    pub properties: BTreeMap<Relative<PropertyId>, serde_json::Value>,
    #[cfg_attr(feature = "tsify", tsify(type = "ItemId", optional))]
    pub source: ItemId,
    #[cfg_attr(feature = "tsify", tsify(type = "ItemId", optional))]
    pub target: ItemId,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Elements {
    pub item_types: BTreeSet<Relative<ItemTypeId>>,
    pub relation_types: BTreeSet<Relative<RelationTypeId>>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct StatusInfo {
    pub alerts: usize,
    pub individual_status: Option<SingleVersioned<StatusChange>>,
    pub aggregated_status: Option<Status>,
}

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Domain {
    #[serde(default)]
    pub roots: Option<BTreeSet<ItemId>>,
    pub types: TypeSet,
}

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TypeSet {
    pub items: BTreeSet<Relative<ItemTypeId>>,
    pub relations: BTreeSet<Relative<RelationTypeId>>,
}

impl<W: Wrapper> Items<W> {
    #[cfg(not(target_family = "wasm"))]
    pub fn load(path: &Path) -> Result<Self>
    where
        Self: serde::de::DeserializeOwned,
    {
        let data =
            std::fs::read_to_string(path).map_err(|e| Error::ReadItems(path.to_path_buf(), e))?;
        serde_json::from_str::<Self>(&data).map_err(|e| Error::DecodeItems(path.to_path_buf(), e))
    }

    pub fn resolve(self, types: &Types, pkg: Option<&PackageId>) -> Result<resolved::Items<W>> {
        let mut items = resolved::Items::from_serial(self, types, pkg)?;
        items.resolve()?;
        Ok(items)
    }
}

impl Items<Identity> {
    pub fn resolve_tx(self, types: &Types, pkg: Option<&PackageId>) -> Result<resolved::TxItems> {
        let mut items = resolved::TxItems::from_serial(self, types, pkg)?;
        items.resolve()?;
        Ok(items)
    }
}

impl Items<Identity> {
    /// Add status info to the item map. If `aggregated_status` is
    /// set, the items must include all children in order to get
    /// the correct result.
    pub fn augment_status(
        self,
        status: &VersionedStatusMap,
        alerts: &VersionedAlertMap,
        aggregated_status: bool,
    ) -> Items<Augment<StatusInfo>> {
        let aggregated_item_status = aggregated_status.then(|| {
            status
                .items
                .iter()
                .fold(BTreeMap::new(), |mut map, (item_id, status)| {
                    let mut maybe_item = self.items.get(item_id).map(|item| (item_id, item));
                    while let Some((item_id, item)) = maybe_item {
                        if self.items.contains_key(item_id) {
                            match map.entry(item_id.clone()) {
                                Entry::Vacant(ent) => {
                                    ent.insert(status.value.status);
                                }
                                Entry::Occupied(mut ent) => {
                                    ent.insert((*ent.get()).max(status.value.status));
                                }
                            };
                        }
                        maybe_item = item
                            .parent
                            .as_ref()
                            .and_then(|item_id| Some((item_id, self.items.get(item_id)?)));
                    }
                    map
                })
        });

        Items {
            items: self
                .items
                .into_iter()
                .map(|(item_id, item)| {
                    let info = StatusInfo {
                        alerts: alerts
                            .items
                            .get(&item_id)
                            .map(|alerts| alerts.values().map(|alerts| alerts.len()).sum::<usize>())
                            .unwrap_or(0),
                        individual_status: status.items.get(&item_id).cloned(),
                        aggregated_status: aggregated_item_status
                            .as_ref()
                            .and_then(|status| status.get(&item_id))
                            .copied(),
                    };
                    (item_id, Augmented { value: item, info })
                })
                .collect(),
            relations: self
                .relations
                .into_iter()
                .map(|(rel_id, rel)| {
                    let info = StatusInfo {
                        alerts: alerts
                            .relations
                            .get(&rel_id)
                            .map(|alerts| alerts.values().map(|alerts| alerts.len()).sum::<usize>())
                            .unwrap_or(0),
                        individual_status: status.relations.get(&rel_id).cloned(),
                        aggregated_status: aggregated_status.then(|| {
                            status
                                .relations
                                .get(&rel_id)
                                .map(|status| status.value.status)
                                .unwrap_or(Status::Ok)
                        }),
                    };
                    (rel_id, Augmented { value: rel, info })
                })
                .collect(),
        }
    }
}

impl Item {
    pub fn into_db(self, item_id: ItemId, parents: Vec<ItemId>, item_name: Vec<String>) -> DbItem {
        DbItem {
            entity: EntityInfo::Item {
                item: ItemInfo {
                    item_id,
                    item_name,
                    parents,
                    item_type: self.item_type.resolve_opt(None),
                },
            },
            parent: self.parent,
            properties: self.properties.into_iter().fold(
                BTreeMap::new(),
                |mut props, (id, prop)| {
                    let id = id.resolve_opt(None);
                    props
                        .entry(id.package().clone())
                        .or_default()
                        .insert(id.local().clone(), prop.to_json());
                    props
                },
            ),
        }
    }
}

pub struct ItemInfoArgs {
    parents: Vec<ItemId>,
    item_name: Vec<String>,
    item_type: Absolute<ItemTypeId>,
}

impl ItemInfoArgs {
    pub fn new(
        item_name: Vec<String>,
        item_type: Absolute<ItemTypeId>,
        parents: Vec<ItemId>,
    ) -> Self {
        Self {
            item_name,
            item_type,
            parents,
        }
    }
}

impl Relation {
    pub fn into_db(
        self,
        relation_id: RelationId,
        source: ItemInfoArgs,
        target: ItemInfoArgs,
    ) -> DbItem {
        DbItem {
            entity: EntityInfo::Relation {
                relation: RelationInfo {
                    relation_id,
                    relation_type: self.relation_type.resolve_opt(None),
                },
                source: ItemInfo {
                    item_id: self.source,
                    item_name: source.item_name,
                    item_type: source.item_type,
                    parents: source.parents,
                },
                target: ItemInfo {
                    item_id: self.target,
                    item_name: target.item_name,
                    item_type: target.item_type,
                    parents: target.parents,
                },
            },
            properties: self.properties.into_iter().fold(
                BTreeMap::new(),
                |mut props, (id, prop)| {
                    let id = id.resolve_opt(None);
                    props
                        .entry(id.package().clone())
                        .or_default()
                        .insert(id.local().clone(), prop);
                    props
                },
            ),
            parent: None,
        }
    }
}

impl Domain {
    pub(crate) fn resolve<W: Wrapper>(
        self,
        types: &Types,
        items: &resolved::Items<W>,
        pkg: Option<&PackageId>,
    ) -> Result<resolved::Domain<W>> {
        let mut domain = resolved::Domain::from_serial(self, pkg);
        domain.resolve(types, items)?;
        Ok(domain)
    }
}

impl PropertyValue {
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            PropertyValue::String(s) => json!(s),
            PropertyValue::Integer(n) => json!(n),
            PropertyValue::Float(n) => json!(n),
            PropertyValue::Time(ts) => serde_json::to_value(ts).unwrap(),
            PropertyValue::Map(map) => serde_json::Value::Object(
                map.iter().map(|(k, v)| (k.clone(), v.to_json())).collect(),
            ),
            PropertyValue::List(elems) => {
                serde_json::Value::Array(elems.iter().map(|v| v.to_json()).collect())
            }
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            PropertyValue::String(s) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            PropertyValue::Integer(v) => Some(*v as f64),
            PropertyValue::Float(v) => Some(v.0),
            _ => None,
        }
    }

    pub fn as_date_time(&self) -> Option<DateTime<Utc>> {
        match self {
            PropertyValue::Time(v) => Some(*v),
            _ => None,
        }
    }
}

impl Display for PropertyValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PropertyValue::String(s) => write!(f, "{s}"),
            PropertyValue::Integer(n) => write!(f, "{n}"),
            PropertyValue::Float(n) => write!(f, "{n:.2}"),
            PropertyValue::Time(ts) => write!(f, "{ts}"),
            PropertyValue::Map(map) => {
                write!(f, "{{")?;
                map.iter().try_for_each(|(k, v)| write!(f, "{k}: {v}, "))?;
                write!(f, "}}")
            }
            PropertyValue::List(elems) => elems.iter().try_for_each(|v| write!(f, "{v}, ")),
        }
    }
}
