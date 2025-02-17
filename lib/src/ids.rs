/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{fmt::Display, str::FromStr};

use dbschema::{HasSchema, ObjectId, OptionSchema, StringSchema, StructSchema};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use uuid::Uuid;

use crate::error::{Error, Result};

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi, type = "T"))]
pub struct Absolute<T>(PackageId, T);

impl<T> HasSchema for Absolute<T> {
    fn schema() -> dbschema::DbSchema {
        StringSchema::new().into()
    }
}

#[cfg(feature = "schemars")]
impl<T: schemars::JsonSchema> schemars::JsonSchema for Absolute<T> {
    fn schema_name() -> String {
        format!("Absolute_{}", T::schema_name())
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        T::json_schema(gen)
    }
}

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi, type = "T"))]
pub struct Relative<T>(Option<PackageId>, T);

impl<T> HasSchema for Relative<T> {
    fn schema() -> dbschema::DbSchema {
        StringSchema::new().into()
    }
}

#[cfg(feature = "schemars")]
impl<T: schemars::JsonSchema> schemars::JsonSchema for Relative<T> {
    fn schema_name() -> String {
        format!("Relative_{}", T::schema_name())
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        T::json_schema(gen)
    }
}

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct PackageId(String);

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ConnectionsPackageId(String);

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct PropertyId(String);

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct RelationTypeId(String);

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ItemTypeId(String);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum AbsEntityTypeId {
    ItemTypeId(Absolute<ItemTypeId>),
    RelationTypeId(Absolute<RelationTypeId>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum AbsEntityTypeIdRef<'a> {
    ItemTypeId(&'a Absolute<ItemTypeId>),
    RelationTypeId(&'a Absolute<RelationTypeId>),
}

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ItemId(#[cfg_attr(feature = "tsify", tsify(type = "string"))] Uuid);

impl HasSchema for ItemId {
    fn schema() -> dbschema::DbSchema {
        StringSchema::new().into()
    }
}

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct RelationId(#[cfg_attr(feature = "tsify", tsify(type = "string"))] Uuid);

impl HasSchema for RelationId {
    fn schema() -> dbschema::DbSchema {
        StringSchema::new().into()
    }
}

#[derive(SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct QueryElemId(String);

#[derive(SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TplVarId(String);

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TransactionId(#[cfg_attr(feature = "tsify", tsify(type = "string"))] Uuid);

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ViewId(String);

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TopologyId(String);

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct DashboardId(String);

#[derive(Clone, Debug)]
pub enum EntityId {
    Item(ItemId),
    Relation(RelationId),
}

impl EntityId {
    pub fn to_object_id(&self) -> ObjectId {
        match self {
            EntityId::Item(item_id) => ObjectId::from(item_id.to_string()),
            EntityId::Relation(relation_id) => ObjectId::from(relation_id.to_string()),
        }
    }
}

impl Display for EntityId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EntityId::Item(item_id) => write!(f, "{item_id}"),
            EntityId::Relation(relation_id) => write!(f, "{relation_id}"),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum EntityInfo {
    Item {
        // See notes in ItemInfo
        #[serde(flatten)]
        item: ItemInfo,
    },
    Relation {
        // Immutable
        #[serde(flatten)]
        relation: RelationInfo,
        // Source id cannot change, but see notes in ItemInfo
        source: ItemInfo,
        // Target id cannot change, but see notes in ItemInfo
        target: ItemInfo,
    },
}

impl EntityInfo {
    pub fn entity_type_id(&self) -> AbsEntityTypeIdRef {
        match self {
            Self::Item { item } => AbsEntityTypeIdRef::ItemTypeId(&item.item_type),
            EntityInfo::Relation { relation, .. } => {
                AbsEntityTypeIdRef::RelationTypeId(&relation.relation_type)
            }
        }
    }
}

impl HasSchema for EntityInfo {
    fn schema() -> dbschema::DbSchema {
        StructSchema::new()
            .field("type", StringSchema::new())
            .field("item", OptionSchema::new(ItemInfo::schema()))
            .serde_flatten("item")
            .unwrap()
            .field("relation", OptionSchema::new(RelationInfo::schema()))
            .serde_flatten("relation")
            .unwrap()
            .field("source", OptionSchema::new(ItemInfo::schema()))
            .field("target", OptionSchema::new(ItemInfo::schema()))
            .into()
    }
}

#[derive(Serialize, Deserialize, HasSchema, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct ItemInfo {
    // Immutable
    pub item_id: ItemId,
    // Immutable
    pub item_type: Absolute<ItemTypeId>,
    // Immutable
    //#[serde(default)]
    pub parents: Vec<ItemId>,
    // Informational: item name when object was last updated
    //#[serde(default)]
    pub item_name: Vec<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct RelationInfo {
    // Immutable
    pub relation_id: RelationId,
    // Immutable
    pub relation_type: Absolute<RelationTypeId>,
}

impl HasSchema for RelationInfo {
    fn schema() -> dbschema::DbSchema {
        StructSchema::new()
            .field("relation_id", StringSchema::new())
            .field("relation_type", StringSchema::new())
            .into()
    }
}

impl PackageId {
    pub fn builtin() -> Self {
        Self(String::from("builtin"))
    }
    pub fn is_builtin(&self) -> bool {
        self.0 == "builtin"
    }
}

impl Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for PackageId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        (!s.contains('/'))
            .then(|| Self(s.to_string()))
            .ok_or_else(|| Error::InvalidPackageId(s.to_string()))
    }
}

impl Display for ConnectionsPackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ConnectionsPackageId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        (!s.contains('/'))
            .then(|| Self(s.to_string()))
            .ok_or_else(|| Error::InvalidPackageId(s.to_string()))
    }
}

impl Display for PropertyId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for PropertyId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        (!s.contains('/'))
            .then(|| Self(s.to_string()))
            .ok_or_else(|| Error::InvalidPropertyId(s.to_string()))
    }
}

impl Display for RelationTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for RelationTypeId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        (!s.contains('/'))
            .then(|| Self(s.to_string()))
            .ok_or_else(|| Error::InvalidRelationTypeId(s.to_string()))
    }
}

impl Display for ItemTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ItemTypeId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        (!s.contains('/'))
            .then(|| Self(s.to_string()))
            .ok_or_else(|| Error::InvalidItemTypeId(s.to_string()))
    }
}

impl Display for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ItemId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(
            Uuid::from_str(s).map_err(|e| Error::InvalidItemId(s.to_string(), e))?,
        ))
    }
}

impl ItemId {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }

    pub fn from_uuid(uuid: Uuid) -> Self {
        Self(uuid)
    }

    pub fn uuid(&self) -> Uuid {
        self.0
    }
}

impl Display for RelationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for RelationId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(
            Uuid::from_str(s).map_err(|e| Error::InvalidRelationId(s.to_string(), e))?,
        ))
    }
}

impl RelationId {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }

    pub fn from_uuid(uuid: Uuid) -> Self {
        Self(uuid)
    }

    pub fn uuid(&self) -> Uuid {
        self.0
    }
}

impl QueryElemId {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl Display for AbsEntityTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ItemTypeId(item_type_id) => write!(f, "{item_type_id}"),
            Self::RelationTypeId(rel_type_id) => write!(f, "{rel_type_id}"),
        }
    }
}

impl AbsEntityTypeIdRef<'_> {
    pub fn to_owned(&self) -> AbsEntityTypeId {
        match self {
            AbsEntityTypeIdRef::ItemTypeId(item_type_id) => {
                AbsEntityTypeId::ItemTypeId((*item_type_id).clone())
            }
            AbsEntityTypeIdRef::RelationTypeId(rel_type_id) => {
                AbsEntityTypeId::RelationTypeId((*rel_type_id).clone())
            }
        }
    }
}

impl Display for QueryElemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for QueryElemId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(s.to_string()))
    }
}

impl TplVarId {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl Display for TplVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for TplVarId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(s.to_string()))
    }
}

impl Display for TransactionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for TransactionId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(Uuid::from_str(s).map_err(|e| {
            Error::InvalidTransactionId(s.to_string(), e)
        })?))
    }
}

impl TransactionId {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Display for ViewId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ViewId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(s.to_string()))
    }
}

impl Display for TopologyId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for TopologyId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(s.to_string()))
    }
}

impl Display for DashboardId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for DashboardId {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(s.to_string()))
    }
}

// impl EntityId {
//     pub(crate) fn uuid(&self) -> &Uuid {
//         match self {
//             EntityId::Item { item_id, .. } => &item_id.0,
//             EntityId::Relation { relation_id, .. } => &relation_id.0,
//         }
//     }
// }

impl<T: Display> Display for Absolute<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.0, self.1)
    }
}

impl<T: FromStr<Err = Error>> FromStr for Absolute<T> {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        match s.split_once('/') {
            Some((pkg, id)) => Ok(Self(PackageId::from_str(pkg)?, T::from_str(id)?)),
            None => Err(Error::InvalidAbsoluteId(s.to_string())),
        }
    }
}

impl<T: Display> Display for Relative<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Some(pkg) => write!(f, "{pkg}/{}", self.1),
            None => write!(f, "{}", self.1),
        }
    }
}

impl<T: FromStr<Err = Error>> FromStr for Relative<T> {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        match s.split_once('/') {
            Some((pkg, id)) => Ok(Self(Some(PackageId::from_str(pkg)?), T::from_str(id)?)),
            None => Ok(Self(None, T::from_str(s)?)),
        }
    }
}

impl<T> Absolute<T> {
    pub fn new(pkgid: PackageId, id: T) -> Self {
        Self(pkgid, id)
    }

    pub fn package(&self) -> &PackageId {
        &self.0
    }

    pub fn local(&self) -> &T {
        &self.1
    }

    pub fn to_relative(&self, pkg: &PackageId) -> Relative<T>
    where
        T: Clone,
    {
        Relative((pkg != &self.0).then(|| self.0.clone()), self.1.clone())
    }

    pub fn to_relative_opt(&self, pkg: Option<&PackageId>) -> Relative<T>
    where
        T: Clone,
    {
        Relative(
            match pkg {
                Some(pkg) if pkg == &self.0 => None,
                None if self.0.is_builtin() => None,
                _ => Some(self.0.clone()),
            },
            self.1.clone(),
        )
    }
}

impl<T> Relative<T> {
    pub fn resolve(self, pkg: &PackageId) -> Absolute<T> {
        Absolute(self.0.unwrap_or_else(|| pkg.clone()), self.1)
    }

    pub fn resolve_opt(self, pkg: Option<&PackageId>) -> Absolute<T> {
        Absolute(
            self.0
                .or_else(|| pkg.cloned())
                .unwrap_or_else(PackageId::builtin),
            self.1,
        )
    }
}
