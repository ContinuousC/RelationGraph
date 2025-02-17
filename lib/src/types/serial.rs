/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

#[cfg(not(target_family = "wasm"))]
use std::os::unix::prelude::OsStrExt;
#[cfg(not(target_family = "wasm"))]
use std::path::Path;
use std::str::FromStr;
use std::{
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
    fmt::Display,
    sync::Arc,
};

use dbschema::{
    DateTimeSchema, DbSchema, DictionarySchema, DoubleSchema, IntegerSchema, ListSchema,
    StringSchema,
};
use graph::{BTreeGraph, Ref, RefBy, RefMap};
use prometheus_schema::QualifiedItemName;
use serde::{Deserialize, Serialize};

use crate::items::serial::PropertyValue;
use crate::query::resolved::ValueSelector;
use crate::{
    error::{Error, Result},
    ids::{
        Absolute, ConnectionsPackageId, ItemTypeId, PackageId, PropertyId, RelationTypeId, Relative,
    },
    serial, TplVarDef, TplVarId,
};

use super::{connector, name::RelativeNameTemplate, resolved};

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Packages(
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: PackageId]: Package }"))]
    #[cfg_attr(feature = "schemars", schemars(with = "BTreeMap<PackageId, Package>"))]
    BTreeGraph<PackageId, Arc<Package>>,
);

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Package {
    pub version: PackageVersion,
    #[serde(default)]
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: PackageId]: PackageVersionReq }")
    )]
    pub requires: BTreeMap<PackageId, PackageVersionReq>,
    #[serde(default)]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: ItemTypeId]: ItemType }"))]
    pub items: BTreeMap<ItemTypeId, ItemType>,
    #[serde(default)]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: PropertyId]: PropertyType }"))]
    pub properties: BTreeMap<PropertyId, PropertyType>,
    #[serde(default)]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: RelationId]: RelationType }"))]
    pub relations: BTreeMap<RelationTypeId, RelationType>,
}

// #[cfg(feature = "schemars")]
// impl schemars::JsonSchema for Package {
//     fn schema_name() -> String {
//         "Package".to_owned()
//     }

//     fn schema_id() -> std::borrow::Cow<'static, str> {
//         std::borrow::Cow::Borrowed(std::concat!(std::module_path!(), "::", "PackageId"))
//     }

//     fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
//         gen.subschema_for::<String>()
//     }
// }

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct PackageVersion(
    #[cfg_attr(feature = "schemars", schemars(with = "String"))]
    #[cfg_attr(feature = "tsify", tsify(type = "string"))]
    semver::Version,
);

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct PackageVersionReq(
    #[cfg_attr(feature = "schemars", schemars(with = "String"))]
    #[cfg_attr(feature = "tsify", tsify(type = "string"))]
    semver::VersionReq,
);

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ItemType {
    pub name: ItemTypeName,
    pub name_template: Option<RelativeNameTemplate>,
    #[serde(default)] //, skip_serializing_if = "BTreeSet::is_empty")]
    pub keys: BTreeSet<Relative<PropertyId>>,
    #[serde(default)] //, skip_serializing_if = "BTreeSet::is_empty")]
    pub parents: BTreeSet<Relative<ItemTypeId>>,
    #[serde(default)] //, skip_serializing_if = "BTreeSet::is_empty")]
    pub implements: BTreeSet<Relative<ItemTypeId>>,
    #[serde(default)] //, skip_serializing_if = "BTreeSet::is_empty")]
    pub properties: BTreeSet<Relative<PropertyId>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ItemTypeInfo {
    pub name: ItemTypeName,
    pub name_template: Option<RelativeNameTemplate>,
    pub keys: BTreeSet<Relative<PropertyId>>,
    pub implements: BTreeSet<Relative<ItemTypeId>>,
    pub implementors: BTreeSet<Relative<ItemTypeId>>,
    pub parents: BTreeSet<Relative<ItemTypeId>>,
    pub children: BTreeSet<Relative<ItemTypeId>>,
    pub properties: BTreeMap<Relative<PropertyId>, PropertyType>,
    pub source_of: BTreeSet<Relative<RelationTypeId>>,
    pub target_of: BTreeSet<Relative<RelationTypeId>>,
    pub prometheus_metrics:
        BTreeMap<ConnectionsPackageId, BTreeMap<QualifiedItemName, crate::serial::ItemMetrics>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ItemTypeName {
    pub singular: String,
    pub plural: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct PropertyType {
    pub name: String,
    #[serde(default)]
    pub description: Option<String>,
    #[cfg_attr(feature = "schemars", schemars(with = "serde_json::Value"))]
    #[cfg_attr(feature = "tsify", tsify(type = "any"))]
    pub value: PropertyValueType,
}

pub type PropertyTypeInfo = PropertyType;

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum PropertyValueType {
    String,
    Integer,
    Float,
    Time,
    Map(Box<Self>),
    List(Box<Self>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct RelationType {
    pub name: String,
    //pub inverse_name: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
    pub multiplicity: Multiplicity,
    #[serde(default)]
    pub bidirectional: bool,
    #[serde(default)]
    pub properties: BTreeSet<Relative<PropertyId>>,
    pub source: Relative<ItemTypeId>,
    pub target: Relative<ItemTypeId>,
    #[serde(default)] //, skip_serializing_if = "Option::is_none")]
    pub connector: Option<Connector>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct RelationTypeInfo {
    pub name: String,
    //pub inverse_name: Option<String>,
    pub description: Option<String>,
    pub multiplicity: Multiplicity,
    pub bidirectional: bool,
    pub properties: BTreeMap<Relative<PropertyId>, PropertyType>,
    pub source: Relative<ItemTypeId>,
    pub target: Relative<ItemTypeId>,
    pub connector: Option<Connector>,
    pub prometheus_metrics:
        BTreeMap<ConnectionsPackageId, BTreeMap<QualifiedItemName, crate::serial::RelationMetrics>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(
    feature = "tsify",
	// Why do we need to force this type to string?
    tsify(from_wasm_abi, into_wasm_abi, type = "string")
)]
#[serde(rename_all = "kebab-case")]
pub enum Multiplicity {
    OneToOne,
    OneToMany,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Connector {
    pub groups: BTreeMap<ConnectorRuleGroupName, ConnectorRuleGroup>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ConnectorRuleGroupName(String);

impl Display for ConnectorRuleGroupName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ConnectorRuleGroupName {
    type Err = Infallible;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(Self(s.to_string()))
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ConnectorRuleGroup {
    pub rules: Vec<ConnectorRule>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ConnectorRule {
    pub template: BTreeMap<TplVarId, TplVarDef>,
    pub source: serial::ItemSelector,
    pub target: serial::ItemSelector,
    //properties: PropertyTemplate,
}

impl Display for PackageVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for PackageVersionReq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Packages {
    pub fn new() -> Self {
        Self(BTreeGraph::new())
    }

    #[cfg(not(target_family = "wasm"))]
    pub fn load_sync(pkgs_path: &Path) -> Result<Self> {
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
                let id = PackageId::from_str(&pkg_path.file_stem().unwrap().to_string_lossy())?;
                let pkg = serde_json::from_str(
                    &std::fs::read_to_string(&pkg_path)
                        .map_err(|e| Error::ReadPkg(pkg_path.clone(), e))?,
                )
                .map_err(|e| Error::DecodePkg(pkg_path.clone(), e))?;
                pkgs.insert(id, pkg);
                Result::Ok(())
            })?;

        Ok(pkgs)
    }

    #[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
    pub async fn load_async(pkgs_path: &Path) -> Result<Self> {
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
                log::debug!("checking package file: {}", ent.path().display());
                std::io::Result::Ok(
                    (ent.file_name().as_bytes().ends_with(b".json")).then(|| ent.path()),
                )
            })
        })
        .filter_map(std::io::Result::transpose)
        .then(|path| {
            Box::pin(async {
                let pkg_path = path.map_err(|e| Error::ReadPkgs(pkgs_path.to_path_buf(), e))?;
                let id = PackageId::from_str(&pkg_path.file_stem().unwrap().to_string_lossy())?;
                let pkg = serde_json::from_str(
                    &std::fs::read_to_string(&pkg_path)
                        .map_err(|e| Error::ReadPkg(pkg_path.clone(), e))?,
                )
                .map_err(|e| Error::DecodePkg(pkg_path.clone(), e))?;
                Result::Ok((id, pkg))
            })
        })
        .filter_map(|r| match r {
            Ok((id, pkg)) => {
                pkgs.insert(id, pkg);
                None
            }
            Err(e) => Some(e),
        })
        .next()
        .await
        .map_or(Ok(()), Err)?;

        Ok(pkgs)
    }

    pub fn get(&self, id: &PackageId) -> Option<&Package> {
        self.0.get(id).map(|pkg| pkg.as_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&PackageId, &Package)> {
        self.0.iter().map(|(id, pkg)| (id, pkg.as_ref()))
    }

    pub fn insert(&mut self, id: PackageId, pkg: Package) {
        self.0.insert(id, Arc::new(pkg));
    }

    pub fn remove(&mut self, id: &PackageId) {
        self.0.remove(id);
    }

    pub fn borrow(&self, pkg_ref: &Ref<Arc<Package>>) -> &Package {
        self.0.borrow(pkg_ref)
    }

    pub fn types(&self) -> Result<resolved::Types> {
        self.verify_reqs()?;

        let mut types = resolved::Types::new();

        /* Promise all objects that will be defined. */

        self.0.iter().for_each(|(pkg_id, pkg)| {
            types.packages.promise(pkg_id.clone());
            pkg.items.keys().for_each(|id| {
                types
                    .items
                    .promise(Absolute::new(pkg_id.clone(), id.clone()));
            });
            pkg.properties.keys().for_each(|id| {
                types
                    .properties
                    .promise(Absolute::new(pkg_id.clone(), id.clone()));
            });
            pkg.relations.keys().for_each(|id| {
                types
                    .relations
                    .promise(Absolute::new(pkg_id.clone(), id.clone()));
            });
        });

        /* Create all objects (except backlinks). This may fail if
         * there are missing link targets. */

        self.0.iter_ref_by().try_for_each(|pkg_ref| {
            let pkg = self.0.borrow(pkg_ref.value_ref()).as_ref();
            let pkg_id = pkg_ref.key();

            let pkg_ref_by = types.packages.get_ref_by(pkg_id).unwrap().clone();
            types.packages.create(
                pkg_ref_by.value_ref(),
                resolved::PackageType {
                    definition: pkg_ref.value_ref().clone(),
                    items: RefMap::new(),
                    properties: RefMap::new(),
                    relations: RefMap::new(),
                },
            );

            pkg.items.iter().try_for_each(|(id, item)| {
                let id = Absolute::new(pkg_id.clone(), id.clone());
                let node = types.items.get_ref(&id).unwrap().clone();
                types.items.create(&node, item.types(&pkg_ref_by, &types)?);
                Ok(())
            })?;

            pkg.properties.iter().try_for_each(|(id, prop)| {
                let id = Absolute::new(pkg_id.clone(), id.clone());
                let node = types.properties.get_ref(&id).unwrap().clone();
                types
                    .properties
                    .create(&node, prop.types(&pkg_ref_by, &types)?);
                Ok(())
            })?;

            Ok(())
        })?;

        /* Resolve connectors. Note: properties must exist at this point. */
        self.0.iter_ref_by().try_for_each(|pkg_ref| {
            let pkg = self.0.borrow(pkg_ref.value_ref()).as_ref();
            let pkg_id = pkg_ref.key();
            let pkg_ref_by = types.packages.get_ref_by(pkg_id).unwrap().clone();

            // Note: properties must exist at this point.
            pkg.relations.iter().try_for_each(|(id, rel)| {
                let id = Absolute::new(pkg_id.clone(), id.clone());
                let node = types.relations.get_ref(&id).unwrap().clone();
                types
                    .relations
                    .create(&node, rel.types(&pkg_ref_by, &types)?);
                Ok(())
            })
        })?;

        /* Create backlinks. */

        let mut implementors = Vec::new();
        let mut children = Vec::new();

        types.items.iter_ref().for_each(|(item_id, item_ref)| {
            let item = types.items.borrow(item_ref);

            types
                .packages
                .borrow_mut(item.package.value_ref())
                .items
                .insert(item_id.local().clone(), item_ref.clone());

            item.properties.iter_ref().for_each(|(_prop_id, prop_ref)| {
                let prop = types.properties.borrow_mut(prop_ref);
                prop.items.insert(item_id.clone(), item_ref.clone());
            });

            implementors.extend(
                item.implements
                    .iter_ref()
                    .map(|(_, impl_ref)| (impl_ref.clone(), item_id.clone(), item_ref.clone())),
            );

            children.extend(
                item.parents
                    .iter_ref()
                    .map(|(_, parent_ref)| (parent_ref.clone(), item_id.clone(), item_ref.clone())),
            );
        });

        implementors
            .into_iter()
            .for_each(|(impl_ref, item_id, item_ref)| {
                types
                    .items
                    .borrow_mut(&impl_ref)
                    .implementors
                    .insert(item_id, item_ref);
            });

        children
            .into_iter()
            .for_each(|(parent_ref, item_id, item_ref)| {
                types
                    .items
                    .borrow_mut(&parent_ref)
                    .children
                    .insert(item_id, item_ref);
            });

        types.properties.iter_ref().for_each(|(prop_id, prop_ref)| {
            let prop = types.properties.borrow(prop_ref);

            types
                .packages
                .borrow_mut(prop.package.value_ref())
                .properties
                .insert(prop_id.local().clone(), prop_ref.clone());
        });

        types.relations.iter_ref().for_each(|(rel_id, rel_ref)| {
            let rel = types.relations.borrow(rel_ref);

            types
                .items
                .borrow_mut(rel.source.value_ref())
                .source_of
                .insert(rel_id.clone(), rel_ref.clone());

            types
                .items
                .borrow_mut(rel.target.value_ref())
                .target_of
                .insert(rel_id.clone(), rel_ref.clone());

            types
                .packages
                .borrow_mut(rel.package.value_ref())
                .relations
                .insert(rel_id.local().clone(), rel_ref.clone());
        });

        fn add_supertypes(
            item: &resolved::ItemType,
            types: &resolved::Types,
            supertypes: &mut Vec<RefBy<Absolute<ItemTypeId>, resolved::ItemType>>,
        ) {
            item.implements.iter_ref().for_each(|(item_id, item_ref)| {
                supertypes.push(RefBy::new(item_id.clone(), item_ref.clone()));
                add_supertypes(types.items.borrow(item_ref), types, supertypes);
            });
        }

        fn add_subtypes(
            item: &resolved::ItemType,
            types: &resolved::Types,
            subtypes: &mut Vec<RefBy<Absolute<ItemTypeId>, resolved::ItemType>>,
        ) {
            item.implementors
                .iter_ref()
                .for_each(|(item_id, item_ref)| {
                    subtypes.push(RefBy::new(item_id.clone(), item_ref.clone()));
                    add_subtypes(types.items.borrow(item_ref), types, subtypes);
                });
        }

        fn add_ancestors(
            item: &resolved::ItemType,
            types: &resolved::Types,
            ancestors: &mut Vec<RefBy<Absolute<ItemTypeId>, resolved::ItemType>>,
        ) {
            if let Some(parents) = item.parents(types) {
                parents.iter_ref().for_each(|(item_id, item_ref)| {
                    add_ancestors(types.items.borrow(item_ref), types, ancestors);

                    ancestors.push(RefBy::new(item_id.clone(), item_ref.clone()));
                });
            }
        }

        types.items.iter_mut().for_each(|(_item_id, item)| {
            let mut supertypes = Vec::new();
            add_supertypes(item, &types, &mut supertypes);

            let mut subtypes = Vec::new();
            add_subtypes(item, &types, &mut subtypes);

            let mut ancestors = Vec::new();
            add_ancestors(item, &types, &mut ancestors);

            item.supertypes = supertypes;
            item.subtypes = subtypes;
            item.ancestors = ancestors;
        });

        Ok(types)
    }

    pub fn verify_reqs(&self) -> Result<()> {
        self.0.iter().try_for_each(|(pkgid, pkg)| {
            pkg.requires.iter().try_for_each(|(reqid, req)| {
                let reqver = &self
                    .0
                    .get(reqid)
                    .ok_or_else(|| {
                        Error::MissingRequirement(
                            pkgid.clone(),
                            Box::new(pkg.version.clone()),
                            reqid.clone(),
                            Box::new(req.clone()),
                        )
                    })?
                    .version;
                req.verify(reqver).then_some(()).ok_or_else(|| {
                    Error::IncompatibleRequirement(
                        pkgid.clone(),
                        Box::new(pkg.version.clone()),
                        reqid.clone(),
                        Box::new(reqver.clone()),
                        Box::new(req.clone()),
                    )
                })
            })
        })
    }
}

impl Default for Packages {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for Packages {
    fn clone(&self) -> Self {
        Self(
            self.0
                .iter()
                .map(|(id, pkg)| (id.clone(), pkg.clone()))
                .collect(),
        )
    }
}

impl PackageVersionReq {
    pub fn verify(&self, version: &PackageVersion) -> bool {
        self.0.matches(&version.0)
    }
}

impl ItemType {
    fn types(
        &self,
        pkg: &RefBy<PackageId, resolved::PackageType>,
        types: &resolved::Types,
    ) -> Result<resolved::ItemType> {
        Ok(resolved::ItemType {
            name: self.name.clone(),
            name_template: self
                .name_template
                .as_ref()
                .cloned()
                .map(|t| t.resolve(Some(pkg.key()))),
            keys: resolve_map(
                &self.keys,
                &types.properties,
                pkg.key(),
                Error::MissingProperty,
            )?,
            implements: resolve_map(
                &self.implements,
                &types.items,
                pkg.key(),
                Error::MissingItemType,
            )?,
            parents: resolve_map(
                &self.parents,
                &types.items,
                pkg.key(),
                Error::MissingItemType,
            )?,
            properties: resolve_map(
                &self.properties,
                &types.properties,
                pkg.key(),
                Error::MissingProperty,
            )?,
            implementors: RefMap::new(),
            children: RefMap::new(),
            source_of: RefMap::new(),
            target_of: RefMap::new(),
            package: pkg.clone(),
            prometheus_metrics: BTreeMap::new(),

            /* Calculated in Types::types. */
            supertypes: Vec::new(),
            subtypes: Vec::new(),
            ancestors: Vec::new(),
        })
    }
}

impl RelationType {
    fn types(
        &self,
        pkg: &RefBy<PackageId, resolved::PackageType>,
        types: &resolved::Types,
    ) -> Result<resolved::RelationType> {
        Ok(resolved::RelationType {
            name: self.name.clone(),
            description: self.description.clone(),
            multiplicity: self.multiplicity.clone(),
            bidirectional: self.bidirectional,
            source: resolve_ref(
                &self.source,
                &types.items,
                pkg.key(),
                Error::MissingItemType,
            )?,
            target: resolve_ref(
                &self.target,
                &types.items,
                pkg.key(),
                Error::MissingItemType,
            )?,
            properties: resolve_map(
                &self.properties,
                &types.properties,
                pkg.key(),
                Error::MissingProperty,
            )?,
            package: pkg.clone(),
            prometheus_metrics: BTreeMap::new(),
            connector: self
                .connector
                .as_ref()
                .map(|serial| {
                    let mut connector =
                        connector::Connector::from_serial(serial.clone(), types, Some(pkg.key()))?;
                    connector.resolve(types)?;
                    Ok(connector)
                })
                .transpose()?,
        })
    }
}

impl PropertyType {
    fn types(
        &self,
        pkg: &RefBy<PackageId, resolved::PackageType>,
        _types: &resolved::Types,
    ) -> Result<resolved::PropertyType> {
        Ok(resolved::PropertyType {
            name: self.name.clone(),
            description: self.description.clone(),
            value: self.value.clone(),
            items: RefMap::new(),
            package: pkg.clone(),
        })
    }
}

impl PropertyValueType {
    pub fn value_from_json(&self, value: serde_json::Value) -> Result<PropertyValue> {
        let err = |e| Error::InvalidPropertyValueJson(self.clone(), e);
        match self {
            PropertyValueType::String => Ok(PropertyValue::String(
                serde_json::from_value(value).map_err(err)?,
            )),
            PropertyValueType::Integer => Ok(PropertyValue::Integer(
                serde_json::from_value(value).map_err(err)?,
            )),
            PropertyValueType::Float => Ok(PropertyValue::Float(
                serde_json::from_value(value).map_err(err)?,
            )),
            PropertyValueType::Time => Ok(PropertyValue::Time(
                serde_json::from_value(value).map_err(err)?,
            )),
            /* TODO: decode in one pass. */
            PropertyValueType::Map(t) => {
                let map: BTreeMap<String, serde_json::Value> =
                    serde_json::from_value(value).map_err(err)?;
                let map = map
                    .into_iter()
                    .map(|(k, v)| Ok((k, t.value_from_json(v)?)))
                    .collect::<Result<_>>()?;
                Ok(PropertyValue::Map(map))
            }
            /* TODO: decode in one pass. */
            PropertyValueType::List(t) => {
                let elems: Vec<serde_json::Value> = serde_json::from_value(value).map_err(err)?;
                let elems = elems
                    .into_iter()
                    .map(|v| t.value_from_json(v))
                    .collect::<Result<_>>()?;
                Ok(PropertyValue::List(elems))
            }
        }
    }

    #[allow(unused)]
    pub(crate) fn verify_value(&self, value: &PropertyValue) -> Result<()> {
        match (self, value) {
            (Self::String, PropertyValue::String(_)) => Ok(()),
            (Self::Integer, PropertyValue::Integer(_)) => Ok(()),
            (Self::Float, PropertyValue::Float(_)) => Ok(()),
            (Self::Time, PropertyValue::Time(_)) => Ok(()),
            (Self::Map(t), PropertyValue::Map(map)) => {
                map.values().try_for_each(|v| t.verify_value(v))
            }
            (Self::List(t), PropertyValue::List(elems)) => {
                elems.iter().try_for_each(|v| t.verify_value(v))
            }
            _ => Err(Error::InvalidPropertyValue(self.clone())),
        }
    }

    pub(crate) fn verify_selector(&self, selector: &ValueSelector) -> Result<()> {
        match (self, selector) {
            (Self::String, ValueSelector::String(_)) => Ok(()),
            _ => Err(Error::InvalidValueSelector),
        }
    }

    pub(crate) fn dbschema(&self) -> DbSchema {
        match self {
            PropertyValueType::String => StringSchema::new().into(),
            PropertyValueType::Integer => IntegerSchema::new().into(),
            PropertyValueType::Float => DoubleSchema::new().into(),
            PropertyValueType::Time => DateTimeSchema::new().into(),
            PropertyValueType::Map(t) => DictionarySchema::new(t.dbschema()).into(),
            PropertyValueType::List(t) => ListSchema::new(t.dbschema()).into(),
        }
    }
}

fn resolve_map<K, V, F>(
    ids: &BTreeSet<Relative<K>>,
    graph: &BTreeGraph<Absolute<K>, V>,
    pkg: &PackageId,
    err: F,
) -> Result<RefMap<Absolute<K>, V>>
where
    K: Ord + Clone + std::fmt::Debug,
    F: Fn(Absolute<K>) -> Error,
{
    ids.iter()
        .map(|id| {
            let id = id.clone().resolve(pkg);
            let value = graph.get_ref(&id).ok_or_else(|| err(id.clone()))?;
            Ok((id, value.clone()))
        })
        .collect()
}

fn resolve_ref<K, V, F>(
    id: &Relative<K>,
    graph: &BTreeGraph<Absolute<K>, V>,
    pkg: &PackageId,
    err: F,
) -> Result<RefBy<Absolute<K>, V>>
where
    K: Ord + Clone,
    F: Fn(Absolute<K>) -> Error,
{
    let id = id.clone().resolve(pkg);
    let value = graph.get_ref(&id).ok_or_else(|| err(id.clone()))?;
    Ok(RefBy::new(id, value.clone()))
}
