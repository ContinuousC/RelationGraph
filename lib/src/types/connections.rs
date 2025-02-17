/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::{BTreeMap, BTreeSet};
#[cfg(not(target_family = "wasm"))]
use std::os::unix::prelude::OsStrExt;
#[cfg(not(target_family = "wasm"))]
use std::path::Path;

use graph::{BTreeGraph, RefBy};
use prometheus_core::LabelName;
use prometheus_schema::{ModuleName, ModuleVersionReq, QualifiedItemName};
use serde::{Deserialize, Serialize};

use crate::{
    ids::ConnectionsPackageId, query::serial, types::serial::PackageVersionReq, Absolute, Error,
    ItemTypeId, PackageId, PropertyId, RelationTypeId, Relative, Result,
};

use super::{resolved, serial::PackageVersion};

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ConnectionsPackages(
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: ConnectionsPackageId]: ConnectionsPackage }")
    )]
    #[cfg_attr(
        feature = "schemars",
        schemars(with = "BTreeMap<ConnectionsPackageId, ConnectionsPackage>")
    )]
    pub BTreeGraph<ConnectionsPackageId, ConnectionsPackage>,
);

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ConnectionsPackage {
    pub version: PackageVersion,
    #[serde(default)]
    pub requires: ConnectionsRequires,
    #[serde(default)]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: String]: ItemConnections }"))]
    pub items: BTreeMap<Absolute<ItemTypeId>, ItemConnections>,
    #[serde(default)]
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: String]: RelationConnections }")
    )]
    pub relations: BTreeMap<Absolute<RelationTypeId>, RelationConnections>,
}

#[derive(Serialize, Deserialize, Clone, Default, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ConnectionsRequires {
    #[serde(default)]
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: PackageId]: PackageVersionReq }")
    )]
    pub discovery: BTreeMap<PackageId, PackageVersionReq>,
    #[serde(default)]
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: ModuleName]: PackageVersionReq }")
    )]
    pub prometheus: BTreeMap<ModuleName, ModuleVersionReq>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ItemConnections {
    pub prometheus: BTreeMap<QualifiedItemName, ItemMetrics>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct RelationConnections {
    pub prometheus: BTreeMap<QualifiedItemName, RelationMetrics>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ItemMetrics {
    #[cfg_attr(feature = "tsify", tsify(type = "string[]"))]
    pub group_by: Option<BTreeSet<LabelName>>,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: ItemKeySelector }"))]
    pub keys: BTreeMap<LabelName, ItemKeySelector>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct RelationMetrics {
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: string]: RelationKeySelector }")
    )]
    pub keys: BTreeMap<LabelName, RelationKeySelector>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum ItemKeySelector {
    Property(Relative<PropertyId>),
    Parent(Box<FollowParent>),
    Relation(Box<FollowRelation>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum RelationKeySelector {
    Property(Relative<PropertyId>),
    Source(Box<FollowItem>),
    Target(Box<FollowItem>),
    Item(Box<FollowItem>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct FollowItem {
    pub item: serial::ItemSelector,
    pub key: ItemKeySelector,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct FollowParent {
    pub item: serial::ItemSelector,
    pub key: ItemKeySelector,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct FollowRelation {
    pub relation: serial::RelationSelector,
    pub key: RelationKeySelector,
}

impl ConnectionsPackages {
    pub fn new() -> Self {
        Self(BTreeGraph::new())
    }

    #[cfg(not(target_family = "wasm"))]
    pub fn load_sync(pkgs_path: &Path) -> Result<Self> {
        use std::str::FromStr;

        let mut pkgs = Self::new();

        std::fs::read_dir(pkgs_path)
            .map_err(|e| Error::ReadPkgs(pkgs_path.to_path_buf(), e))?
            .map(|ent| {
                let ent = ent?;
                std::io::Result::Ok(
                    (ent.file_name().as_bytes().ends_with(b".json")).then(|| ent.path()),
                )
            })
            .filter_map(std::io::Result::transpose)
            .try_for_each(|path| {
                let pkg_path = path.map_err(|e| Error::ReadPkgs(pkgs_path.to_path_buf(), e))?;
                let id = ConnectionsPackageId::from_str(
                    &pkg_path.file_stem().unwrap().to_string_lossy(),
                )?;
                let pkg = serde_json::from_str(
                    &std::fs::read_to_string(&pkg_path)
                        .map_err(|e| Error::ReadPkg(pkg_path.clone(), e))?,
                )
                .map_err(|e| Error::DecodePkg(pkg_path.clone(), e))?;
                pkgs.0.insert(id, pkg);
                Result::Ok(())
            })?;

        Ok(pkgs)
    }

    #[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
    pub async fn load_async(pkgs_path: &Path) -> Result<Self> {
        use std::str::FromStr;
        use tokio_stream::StreamExt;

        let mut pkgs = Self::new();

        tokio_stream::wrappers::ReadDirStream::new(
            tokio::fs::read_dir(pkgs_path)
                .await
                .map_err(|e| Error::ReadPkgs(pkgs_path.to_path_buf(), e))?,
        )
        .then(|ent| {
            Box::pin(async {
                let ent = ent?;
                std::io::Result::Ok(
                    (ent.file_name().as_bytes().ends_with(b".json")).then(|| ent.path()),
                )
            })
        })
        .filter_map(std::io::Result::transpose)
        // .try_for_each(...)
        .then(|path| {
            Box::pin(async {
                let pkg_path = path.map_err(|e| Error::ReadPkgs(pkgs_path.to_path_buf(), e))?;
                let id = ConnectionsPackageId::from_str(
                    &pkg_path.file_stem().unwrap().to_string_lossy(),
                )?;
                let pkg = serde_json::from_str(
                    &tokio::fs::read_to_string(&pkg_path)
                        .await
                        .map_err(|e| Error::ReadPkg(pkg_path.clone(), e))?,
                )
                .map_err(|e| Error::DecodePkg(pkg_path.clone(), e))?;
                Result::Ok((id, pkg))
            })
        })
        .filter_map(|r| match r {
            Ok((id, pkg)) => {
                pkgs.0.insert(id, pkg);
                None
            }
            Err(e) => Some(e),
        })
        .next()
        .await
        .map_or(Ok(()), Err)?;

        Ok(pkgs)
    }
}

impl Default for ConnectionsPackages {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for ConnectionsPackages {
    fn clone(&self) -> Self {
        Self(
            self.0
                .iter()
                .map(|(name, package)| (name.clone(), package.clone()))
                .collect(),
        )
    }
}

impl ItemMetrics {
    pub(crate) fn types(
        &self,
        pkg: Option<&PackageId>,
        cpkg: &RefBy<ConnectionsPackageId, resolved::ConnectionsPackageType>,
        types: &resolved::Types,
    ) -> Result<resolved::ItemMetrics> {
        Ok(resolved::ItemMetrics {
            group_by: self.group_by.as_ref().cloned(),
            keys: self
                .keys
                .iter()
                .map(|(label, selector)| Ok((label.clone(), selector.types(pkg, types)?)))
                .collect::<Result<_>>()?,
            package: cpkg.clone(),
        })
    }
}

impl RelationMetrics {
    pub(crate) fn types(
        &self,
        pkg: Option<&PackageId>,
        cpkg: &RefBy<ConnectionsPackageId, resolved::ConnectionsPackageType>,
        types: &resolved::Types,
    ) -> Result<resolved::RelationMetrics> {
        Ok(resolved::RelationMetrics {
            keys: self
                .keys
                .iter()
                .map(|(label, selector)| Ok((label.clone(), selector.types(pkg, types)?)))
                .collect::<Result<_>>()?,
            package: cpkg.clone(),
        })
    }
}

impl ItemKeySelector {
    pub(crate) fn types(
        &self,
        pkg: Option<&PackageId>,
        types: &resolved::Types,
    ) -> Result<resolved::ItemKeySelector> {
        match self {
            Self::Property(prop) => {
                let prop = prop.clone().resolve_opt(pkg);
                let prop = types
                    .properties
                    .get_ref_by(&prop)
                    .ok_or_else(|| Error::MissingProperty(prop.clone()))?;
                Ok(resolved::ItemKeySelector::Property(prop))
            }
            Self::Parent(rel) => Ok(resolved::ItemKeySelector::Parent(Box::new(
                rel.types(pkg, types)?,
            ))),
            Self::Relation(rel) => Ok(resolved::ItemKeySelector::Relation(Box::new(
                rel.types(pkg, types)?,
            ))),
        }
    }
}

impl RelationKeySelector {
    pub(crate) fn types(
        &self,
        pkg: Option<&PackageId>,
        types: &resolved::Types,
    ) -> Result<resolved::RelationKeySelector> {
        match self {
            Self::Property(prop) => {
                let prop = prop.clone().resolve_opt(pkg);
                let prop = types
                    .properties
                    .get_ref_by(&prop)
                    .ok_or_else(|| Error::MissingProperty(prop.clone()))?;
                Ok(resolved::RelationKeySelector::Property(prop))
            }
            Self::Source(item) => Ok(resolved::RelationKeySelector::Source(Box::new(
                item.types(pkg, types)?,
            ))),
            Self::Target(item) => Ok(resolved::RelationKeySelector::Target(Box::new(
                item.types(pkg, types)?,
            ))),
            Self::Item(item) => Ok(resolved::RelationKeySelector::Item(Box::new(
                item.types(pkg, types)?,
            ))),
        }
    }
}

impl FollowItem {
    fn types(
        &self,
        pkg: Option<&PackageId>,
        types: &resolved::Types,
    ) -> Result<resolved::FollowItem> {
        Ok(resolved::FollowItem {
            item: self.item.clone().resolve(types, &BTreeMap::new(), pkg)?,
            key: self.key.types(pkg, types)?,
        })
    }
}

impl FollowParent {
    fn types(
        &self,
        pkg: Option<&PackageId>,
        types: &resolved::Types,
    ) -> Result<resolved::FollowParent> {
        Ok(resolved::FollowParent {
            item: self.item.clone().resolve(types, &BTreeMap::new(), pkg)?,
            key: self.key.types(pkg, types)?,
        })
    }
}

impl FollowRelation {
    fn types(
        &self,
        pkg: Option<&PackageId>,
        types: &resolved::Types,
    ) -> Result<resolved::FollowRelation> {
        Ok(resolved::FollowRelation {
            relation: self
                .relation
                .clone()
                .resolve(types, &BTreeMap::new(), pkg)?,
            key: self.key.types(pkg, types)?,
        })
    }
}
