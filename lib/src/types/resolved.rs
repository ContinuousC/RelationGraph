/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use dbschema::{DbSchema, OptionSchema, StructSchema};
use graph::{BTreeGraph, Ref, RefBy, RefMap};
use prometheus_core::LabelName;
use prometheus_schema::{LabelSelector, QualifiedItemName, Universe};
use wrapper::{Wrapped, Wrapper};

use crate::{
    ids::{
        AbsEntityTypeIdRef, Absolute, ConnectionsPackageId, ItemTypeId, PackageId, PropertyId,
        RelationTypeId,
    },
    items::serial::PropertyValue,
    query::{
        self,
        filter::MatchResult,
        resolved::{ItemArgs, MatchContext, RelArgs},
    },
    types::serial::{ItemTypeName, Multiplicity, Package},
    Endpoint, EntityRef, Error, Item, Items, Packages, Relation, Result,
};

use super::{
    connections::{self, ConnectionsPackage, ConnectionsPackages},
    connector::Connector,
    name::AbsoluteNameTemplate,
    serial::{self, ItemTypeInfo, PropertyValueType, RelationTypeInfo},
};

// #[derive(Serialize)]
pub struct Types {
    pub packages: BTreeGraph<PackageId, PackageType>,
    pub connections: BTreeGraph<ConnectionsPackageId, ConnectionsPackageType>,
    pub items: BTreeGraph<Absolute<ItemTypeId>, ItemType>,
    pub properties: BTreeGraph<Absolute<PropertyId>, PropertyType>,
    pub relations: BTreeGraph<Absolute<RelationTypeId>, RelationType>,
}

pub struct PackageType {
    pub definition: Ref<Arc<Package>>,
    pub items: RefMap<ItemTypeId, ItemType>,
    pub properties: RefMap<PropertyId, PropertyType>,
    pub relations: RefMap<RelationTypeId, RelationType>,
}

pub struct ConnectionsPackageType {
    pub definition: Ref<ConnectionsPackage>,
}

pub struct ItemType {
    pub name: ItemTypeName,
    // From definition.
    pub name_template: Option<AbsoluteNameTemplate>,
    // From definition.
    pub keys: RefMap<Absolute<PropertyId>, PropertyType>,
    // From definition.
    pub implements: RefMap<Absolute<ItemTypeId>, ItemType>,
    // Backlinks from ItemType::implements.
    pub implementors: RefMap<Absolute<ItemTypeId>, ItemType>,
    // From definition.
    pub parents: RefMap<Absolute<ItemTypeId>, ItemType>,
    // Backlinks from ItemType::parents.
    pub children: RefMap<Absolute<ItemTypeId>, ItemType>,
    // From definition.
    pub properties: RefMap<Absolute<PropertyId>, PropertyType>,
    // Backlinks from Relation::source.
    pub source_of: RefMap<Absolute<RelationTypeId>, RelationType>,
    // Backlinks from Relation::target.
    pub target_of: RefMap<Absolute<RelationTypeId>, RelationType>,
    // Links to metrics schema.
    pub prometheus_metrics:
        BTreeMap<QualifiedItemName, (Ref<prometheus_schema::Item>, ItemMetrics)>,
    pub package: RefBy<PackageId, PackageType>,

    // Cached list of supertypes.
    pub(super) supertypes: Vec<RefBy<Absolute<ItemTypeId>, ItemType>>,
    // Cached list of subtypes.
    pub(super) subtypes: Vec<RefBy<Absolute<ItemTypeId>, ItemType>>,
    // Cached list of ancestors.
    pub(super) ancestors: Vec<RefBy<Absolute<ItemTypeId>, ItemType>>,
}

pub struct RelationType {
    pub name: String,
    pub description: Option<String>,
    pub multiplicity: Multiplicity,
    pub bidirectional: bool,
    pub source: RefBy<Absolute<ItemTypeId>, ItemType>,
    pub target: RefBy<Absolute<ItemTypeId>, ItemType>,
    pub properties: RefMap<Absolute<PropertyId>, PropertyType>,
    pub prometheus_metrics:
        BTreeMap<QualifiedItemName, (Ref<prometheus_schema::Item>, RelationMetrics)>,
    pub package: RefBy<PackageId, PackageType>,
    pub connector: Option<Connector>,
}

pub struct PropertyType {
    pub name: String,
    pub description: Option<String>,
    pub value: PropertyValueType,
    // Backlinks from ItemType::properties.
    pub items: RefMap<Absolute<ItemTypeId>, ItemType>,
    pub package: RefBy<PackageId, PackageType>,
}

pub struct ItemMetrics {
    pub(super) keys: BTreeMap<LabelName, ItemKeySelector>,
    pub(super) group_by: Option<BTreeSet<LabelName>>,
    pub(super) package: RefBy<ConnectionsPackageId, ConnectionsPackageType>,
}

pub struct RelationMetrics {
    pub(super) keys: BTreeMap<LabelName, RelationKeySelector>,
    pub(super) package: RefBy<ConnectionsPackageId, ConnectionsPackageType>,
}

pub enum ItemKeySelector {
    Property(RefBy<Absolute<PropertyId>, PropertyType>),
    Parent(Box<FollowParent>),
    Relation(Box<FollowRelation>),
}

pub enum RelationKeySelector {
    Property(RefBy<Absolute<PropertyId>, PropertyType>),
    Source(Box<FollowItem>),
    Target(Box<FollowItem>),
    Item(Box<FollowItem>),
}

pub struct FollowItem {
    pub(super) item: query::resolved::ItemSelector,
    pub(super) key: ItemKeySelector,
}

pub struct FollowParent {
    pub(super) item: query::resolved::ItemSelector,
    pub(super) key: ItemKeySelector,
}

pub struct FollowRelation {
    pub(super) relation: query::resolved::RelationSelector,
    pub(super) key: RelationKeySelector,
}

pub enum EntityTypeRef<'a> {
    ItemType(&'a ItemType),
    RelationType(&'a RelationType),
}

pub enum EntityMetricsRef<'a> {
    ItemMetrics(&'a ItemMetrics),
    RelationMetrics(&'a RelationMetrics),
}

impl Types {
    pub fn new() -> Self {
        Self {
            items: BTreeGraph::new(),
            properties: BTreeGraph::new(),
            relations: BTreeGraph::new(),
            packages: BTreeGraph::new(),
            connections: BTreeGraph::new(),
        }
    }

    pub fn resolve_connections(
        &mut self,
        connections: &ConnectionsPackages,
        packages: &Packages,
        prom_schema: &Universe,
    ) -> Result<()> {
        /* Verify requirements. */

        connections.0.iter().try_for_each(|(name, connections)| {
            connections
                .requires
                .discovery
                .iter()
                .try_for_each(|(pkg_id, pkg_req)| {
                    self.packages.get(pkg_id).map_or_else(
                        || {
                            Err(Error::ConnectionsMissingRequirement(
                                name.clone(),
                                Box::new(connections.version.clone()),
                                pkg_id.clone(),
                                Box::new(pkg_req.clone()),
                            ))
                        },
                        |pkg| {
                            let pkg_def = packages.borrow(&pkg.definition);
                            pkg_req
                                .verify(&pkg_def.version)
                                .then_some(())
                                .ok_or_else(|| {
                                    Error::ConnectionsIncompatibleRequirement(
                                        name.clone(),
                                        Box::new(connections.version.clone()),
                                        pkg_id.clone(),
                                        Box::new(pkg_def.version.clone()),
                                        Box::new(pkg_req.clone()),
                                    )
                                })
                        },
                    )
                })?;

            connections
                .requires
                .prometheus
                .iter()
                .try_for_each(|(mod_name, mod_req)| {
                    let module = prom_schema.lookup_module(mod_name).ok_or_else(|| {
                        Error::MissingPrometheusModule(name.clone(), mod_name.clone())
                    })?;
                    mod_req
                        .verify(&module.version)
                        .then_some(())
                        .ok_or_else(|| {
                            Error::IncompatiblePrometheusModule(
                                name.clone(),
                                mod_name.clone(),
                                Box::new(module.version.clone()),
                                Box::new(mod_req.clone()),
                            )
                        })
                })?;

            Ok(())
        })?;

        /* Clear existing connections. */

        self.items.values_mut().for_each(|item| {
            item.prometheus_metrics.clear();
        });
        self.relations.values_mut().for_each(|rel| {
            rel.prometheus_metrics.clear();
        });
        let _ = std::mem::replace(&mut self.connections, BTreeGraph::new());

        /* Add new connections. Todo: should we avoid early returns
         * here? As it is, the connections maps may be partially
         * initialized on error. */

        connections
            .0
            .iter_ref()
            .try_for_each(|(name, connections_ref)| {
                let cpkg_ref = self.connections.insert(
                    name.clone(),
                    ConnectionsPackageType {
                        definition: connections_ref.clone(),
                    },
                );
                let cpkg = RefBy::new(name.clone(), cpkg_ref);

                let connections = connections.0.borrow(connections_ref);

                connections
                    .items
                    .iter()
                    .try_for_each(|(item_id, item_conns)| {
                        let item_ref = self
                            .items
                            .get_ref(item_id)
                            .ok_or_else(|| Error::MissingItemType(item_id.clone()))?
                            .clone();
                        item_conns.prometheus.iter().try_for_each(
                            |(prom_item_name, item_metrics)| {
                                let prom_item_ref =
                                    prom_schema.lookup_item_ref(prom_item_name).ok_or_else(
                                        || Error::MissingPrometheusItem(prom_item_name.clone()),
                                    )?;
                                let item_metrics = item_metrics.types(
                                    Some(self.items.borrow(&item_ref).package.key()),
                                    &cpkg,
                                    self,
                                )?;
                                self.items
                                    .borrow_mut(&item_ref)
                                    .prometheus_metrics
                                    .insert(prom_item_name.clone(), (prom_item_ref, item_metrics));
                                Result::Ok(())
                            },
                        )?;
                        Ok(())
                    })?;

                connections
                    .relations
                    .iter()
                    .try_for_each(|(rel_id, rel_conns)| {
                        let rel_ref = self
                            .relations
                            .get_ref(rel_id)
                            .ok_or_else(|| Error::MissingRelationType(rel_id.clone()))?
                            .clone();
                        rel_conns.prometheus.iter().try_for_each(
                            |(prom_item_name, rel_metrics)| {
                                let prom_item_ref =
                                    prom_schema.lookup_item_ref(prom_item_name).ok_or_else(
                                        || Error::MissingPrometheusItem(prom_item_name.clone()),
                                    )?;
                                let rel_metrics = rel_metrics.types(
                                    Some(self.relations.borrow(&rel_ref).package.key()),
                                    &cpkg,
                                    self,
                                )?;
                                self.relations
                                    .borrow_mut(&rel_ref)
                                    .prometheus_metrics
                                    .insert(prom_item_name.clone(), (prom_item_ref, rel_metrics));
                                Result::Ok(())
                            },
                        )?;
                        Ok(())
                    })?;

                Ok(())
            })
    }

    #[deprecated = "use Types::iter_type_and_subtypes"]
    pub fn get_item_types<'a>(
        &'a self,
        type_ref: &'a RefBy<Absolute<ItemTypeId>, ItemType>,
    ) -> impl Iterator<Item = (&'a Absolute<ItemTypeId>, &'a ItemType)> {
        self.iter_type_and_subtypes(type_ref)
    }

    pub fn iter_parents<'a>(
        &'a self,
        type_ref: &'a Ref<ItemType>,
    ) -> impl Iterator<Item = (&'a Absolute<ItemTypeId>, &'a ItemType)> + 'a {
        self.items
            .borrow(type_ref)
            .ancestors
            .iter()
            .map(|item_ref| (item_ref.key(), self.items.borrow(item_ref)))
    }

    pub fn iter_type_and_subtypes<'a>(
        &'a self,
        type_ref: &'a RefBy<Absolute<ItemTypeId>, ItemType>,
    ) -> impl Iterator<Item = (&'a Absolute<ItemTypeId>, &'a ItemType)> {
        self.iter_type_and_subtypes_by_entry(type_ref.key(), type_ref.value_ref())
    }

    pub fn iter_type_and_subtypes_by_entry<'a>(
        &'a self,
        key: &'a Absolute<ItemTypeId>,
        type_ref: &'a Ref<ItemType>,
    ) -> impl Iterator<Item = (&'a Absolute<ItemTypeId>, &'a ItemType)> {
        let typ = self.items.borrow(type_ref);
        std::iter::once((key, typ)).chain(typ.subtypes(self))
    }

    pub fn iter_type_and_supertypes<'a>(
        &'a self,
        type_ref: &'a RefBy<Absolute<ItemTypeId>, ItemType>,
    ) -> impl Iterator<Item = (&'a Absolute<ItemTypeId>, &'a ItemType)> {
        self.iter_type_and_supertypes_by_entry(type_ref.key(), type_ref.value_ref())
    }

    pub fn iter_type_and_supertypes_by_entry<'a>(
        &'a self,
        key: &'a Absolute<ItemTypeId>,
        type_ref: &'a Ref<ItemType>,
    ) -> impl Iterator<Item = (&'a Absolute<ItemTypeId>, &'a ItemType)> {
        let typ = self.items.borrow(type_ref);
        std::iter::once((key, typ)).chain(typ.supertypes(self))
    }

    pub fn item_type_implements(
        &self,
        item_type: &RefBy<Absolute<ItemTypeId>, ItemType>,
        interface: &Absolute<ItemTypeId>,
    ) -> bool {
        interface == item_type.key()
            || self
                .items
                .borrow(item_type.value_ref())
                .implements(interface, self)
    }

    pub fn property_schema(&self) -> DbSchema {
        self.packages
            .iter()
            .fold(StructSchema::new(), |schema, (pkg_id, pkg)| {
                schema.field(
                    pkg_id.to_string(),
                    OptionSchema::new(pkg.properties.iter(&self.properties).fold(
                        StructSchema::new(),
                        |schema, (prop_id, prop)| {
                            schema.field(
                                prop_id.to_string(),
                                OptionSchema::new(prop.value.dbschema()),
                            )
                        },
                    )),
                )
            })
            .into()
    }

    pub fn get_entity_type<'a>(&'a self, id: &AbsEntityTypeIdRef) -> Option<EntityTypeRef<'a>> {
        match id {
            AbsEntityTypeIdRef::ItemTypeId(item_type_id) => {
                self.items.get(item_type_id).map(EntityTypeRef::ItemType)
            }
            AbsEntityTypeIdRef::RelationTypeId(rel_type_id) => self
                .relations
                .get(rel_type_id)
                .map(EntityTypeRef::RelationType),
        }
    }
}

impl Default for Types {
    fn default() -> Self {
        Self::new()
    }
}

impl ItemType {
    pub fn implements(&self, typ: &Absolute<ItemTypeId>, types: &Types) -> bool {
        self.implements.contains_key(typ)
            || self
                .implements
                .values(&types.items)
                .any(|subtype| subtype.implements(typ, types))
    }

    pub fn supertypes<'a>(
        &'a self,
        types: &'a Types,
    ) -> impl Iterator<Item = (&'a Absolute<ItemTypeId>, &'a ItemType)> + 'a {
        self.supertypes
            .iter()
            .map(|r| (r.key(), types.items.borrow(r)))
    }

    pub fn subtypes<'a>(
        &'a self,
        types: &'a Types,
    ) -> impl Iterator<Item = (&'a Absolute<ItemTypeId>, &'a ItemType)> + 'a {
        self.subtypes
            .iter()
            .map(|r| (r.key(), types.items.borrow(r)))
    }

    pub fn parents<'a>(
        &'a self,
        types: &'a Types,
    ) -> Option<&'a RefMap<Absolute<ItemTypeId>, ItemType>> {
        std::iter::once(&self.parents)
            .chain(self.supertypes(types).map(|(_, typ)| &typ.parents))
            .find(|ps| !ps.is_empty())
    }

    pub fn keys<'a>(
        &'a self,
        types: &'a Types,
    ) -> Option<&'a RefMap<Absolute<PropertyId>, PropertyType>> {
        std::iter::once(&self.keys)
            .chain(self.supertypes(types).map(|(_, typ)| &typ.keys))
            .find(|ks| !ks.is_empty())
    }

    pub fn properties<'a>(
        &'a self,
        types: &'a Types,
    ) -> impl Iterator<Item = (&'a Absolute<PropertyId>, &'a PropertyType)> {
        self.properties.iter(&types.properties).chain(
            self.supertypes(types)
                .flat_map(|(_, typ)| typ.properties.iter(&types.properties)),
        )
    }

    pub fn properties_with_item<'a>(
        &'a self,
        id: &'a Absolute<ItemTypeId>,
        types: &'a Types,
    ) -> impl Iterator<
        Item = (
            &'a Absolute<ItemTypeId>,
            &'a ItemType,
            &'a Absolute<PropertyId>,
            &'a PropertyType,
        ),
    > {
        self.properties
            .iter(&types.properties)
            .map(move |(prop_id, prop)| (id, self, prop_id, prop))
            .chain(self.supertypes(types).flat_map(|(item_type, typ)| {
                typ.properties
                    .iter(&types.properties)
                    .map(move |(prop_id, prop)| (item_type, typ, prop_id, prop))
            }))
    }

    pub fn get_property<'a>(
        &self,
        id: &Absolute<PropertyId>,
        types: &'a Types,
    ) -> Option<&'a PropertyType> {
        std::iter::once(self)
            .chain(self.supertypes(types).map(|(_, typ)| typ))
            .find_map(|item| item.properties.get(id, &types.properties))
    }

    pub fn has_property(&self, id: &Absolute<PropertyId>, types: &Types) -> bool {
        std::iter::once(self)
            .chain(self.supertypes(types).map(|(_, typ)| typ))
            .any(|item| item.properties.contains_key(id))
    }

    pub fn prometheus_items<'a>(
        &'a self,
        types: &'a Types,
        schema: &'a Universe,
    ) -> impl Iterator<
        Item = (
            &'a QualifiedItemName,
            &'a prometheus_schema::Item,
            &'a ItemMetrics,
        ),
    > {
        self.prometheus_metrics
            .iter()
            .chain(
                self.supertypes(types)
                    .flat_map(|(_, typ)| typ.prometheus_metrics.iter()),
            )
            .map(|(item_name, (item_ref, metrics))| {
                let prom_item = schema.get_item(item_ref);
                (item_name, prom_item, metrics)
            })
    }

    pub fn prometheus_metrics<'a>(
        &'a self,
        prom_item_name: &QualifiedItemName,
        types: &'a Types,
        schema: &'a Universe,
    ) -> Option<(&'a prometheus_schema::Item, &'a ItemMetrics)> {
        let (prom_item_ref, item_metrics) = std::iter::once(&self.prometheus_metrics)
            .chain(
                self.supertypes(types)
                    .map(|(_, typ)| &typ.prometheus_metrics),
            )
            .find_map(|prom_items| prom_items.get(prom_item_name))?;
        Some((schema.get_item(prom_item_ref), item_metrics))
    }

    // pub fn prometheus_queries(
    //     &self,
    //     types: &Types,
    //     schema: &prometheus::Universe,
    // ) -> Vec<prometheus::MetricSelector> {
    //     self.prometheus_items(types, schema)
    //         .flat_map(|(_, prom_item, _)| {
    //             prom_item
    //                 .svc_queries(schema)
    //                 .into_iter()
    //                 .flat_map(move |base_query| {
    //                     let metrics: BTreeMap<_, BTreeSet<_>> = prom_item.all_metrics(schema).fold(
    //                         BTreeMap::new(),
    //                         |mut m, (metric_name, metric)| {
    //                             m.entry(metric.labels().collect::<BTreeMap<_, _>>())
    //                                 .or_default()
    //                                 .insert(metric_name.to_string());
    //                             m
    //                         },
    //                     );

    //                     metrics.into_iter().map(move |(labels, metrics)| {
    //                         let mut q = std::iter::once((
    //                             LabelName::new(String::from("__name__")),
    //                             LabelSelector::In(metrics),
    //                         ))
    //                         .chain(
    //                             labels
    //                                 .into_iter()
    //                                 .map(|(name, value)| (name.clone(), value.clone())),
    //                         )
    //                         .collect();
    //                         q &= &base_query;
    //                         q
    //                     })
    //                 })
    //         })
    //         .collect()
    // }

    // pub fn prometheus_queries_for_item(
    //     &self,
    //     item: &Item,
    //     items: &Items,
    //     types: &Types,
    //     schema: &prometheus::Universe,
    // ) -> Vec<prometheus::MetricSelector> {
    //     self.prometheus_items(types, schema)
    //         .flat_map(|(_, prom_item, metrics)| {
    //             let item_query = metrics.prometheus_query(item, items, types);
    //             prom_item
    //                 .svc_queries(schema)
    //                 .into_iter()
    //                 .flat_map(move |mut base_query| {
    //                     base_query &= &item_query;

    //                     let metrics: BTreeMap<_, BTreeSet<_>> = prom_item.all_metrics(schema).fold(
    //                         BTreeMap::new(),
    //                         |mut m, (metric_name, metric)| {
    //                             m.entry(metric.labels().collect::<BTreeMap<_, _>>())
    //                                 .or_default()
    //                                 .insert(metric_name.to_string());
    //                             m
    //                         },
    //                     );

    //                     metrics.into_iter().map(move |(labels, metrics)| {
    //                         let mut q = std::iter::once((
    //                             LabelName::new(String::from("__name__")),
    //                             LabelSelector::In(metrics),
    //                         ))
    //                         .chain(
    //                             labels
    //                                 .into_iter()
    //                                 .map(|(name, value)| (name.clone(), value.clone())),
    //                         )
    //                         .collect();
    //                         q &= &base_query;
    //                         q
    //                     })
    //                 })
    //         })
    //         .collect()
    // }

    pub fn info(&self, pkg: Option<&PackageId>, types: &Types) -> ItemTypeInfo {
        ItemTypeInfo {
            name: self.name.clone(),
            name_template: self.name_template.as_ref().map(|t| t.to_relative(pkg)),
            keys: self.keys.keys().map(|k| k.to_relative_opt(pkg)).collect(),
            implements: self
                .implements
                .keys()
                .map(|k| k.to_relative_opt(pkg))
                .collect(),
            implementors: self
                .implementors
                .keys()
                .map(|k| k.to_relative_opt(pkg))
                .collect(),
            parents: self
                .parents
                .keys()
                .map(|k| k.to_relative_opt(pkg))
                .collect(),
            children: self
                .children
                .keys()
                .map(|k| k.to_relative_opt(pkg))
                .collect(),
            properties: self
                .properties
                .iter(&types.properties)
                .map(|(id, prop)| (id.to_relative_opt(pkg), prop.to_serial()))
                .collect(),
            source_of: self
                .source_of
                .keys()
                .map(|k| k.to_relative_opt(pkg))
                .collect(),
            target_of: self
                .target_of
                .keys()
                .map(|k| k.to_relative_opt(pkg))
                .collect(),
            prometheus_metrics: self.prometheus_metrics.iter().fold(
                BTreeMap::new(),
                |mut map, (item, (_, metrics))| {
                    map.entry(metrics.package.key().clone())
                        .or_default()
                        .insert(item.clone(), metrics.to_serial(pkg));
                    map
                },
            ),
        }
    }
}

impl RelationType {
    pub fn prometheus_items<'a>(
        &'a self,
        schema: &'a Universe,
    ) -> impl Iterator<
        Item = (
            &'a QualifiedItemName,
            &'a prometheus_schema::Item,
            &'a RelationMetrics,
        ),
    > {
        self.prometheus_metrics
            .iter()
            .map(|(item_name, (item_ref, metrics))| {
                let prom_item = schema.get_item(item_ref);
                (item_name, prom_item, metrics)
            })
    }

    pub fn prometheus_metrics<'a>(
        &'a self,
        prom_item_name: &QualifiedItemName,
        schema: &'a Universe,
    ) -> Option<(&'a prometheus_schema::Item, &'a RelationMetrics)> {
        let (prom_item_ref, rel_metrics) = self.prometheus_metrics.get(prom_item_name)?;
        Some((schema.get_item(prom_item_ref), rel_metrics))
    }

    // pub fn prometheus_queries(
    //     &self,
    //     schema: &prometheus::Universe,
    // ) -> Vec<prometheus::MetricSelector> {
    //     self.prometheus_metrics
    //         .iter()
    //         .flat_map(|(_item_name, (item_ref, _metrics))| {
    //             let prom_item = schema.get_item(item_ref);
    //             prom_item
    //                 .svc_queries(schema)
    //                 .into_iter()
    //                 .flat_map(move |base_query| {
    //                     let metrics: BTreeMap<_, BTreeSet<_>> = prom_item.all_metrics(schema).fold(
    //                         BTreeMap::new(),
    //                         |mut m, (metric_name, metric)| {
    //                             m.entry(metric.labels().collect::<BTreeMap<_, _>>())
    //                                 .or_default()
    //                                 .insert(metric_name.to_string());
    //                             m
    //                         },
    //                     );

    //                     metrics.into_iter().map(move |(labels, metrics)| {
    //                         let mut q = std::iter::once((
    //                             LabelName::new(String::from("__name__")),
    //                             LabelSelector::In(metrics),
    //                         ))
    //                         .chain(
    //                             labels
    //                                 .into_iter()
    //                                 .map(|(name, value)| (name.clone(), value.clone())),
    //                         )
    //                         .collect();
    //                         q &= &base_query;
    //                         q
    //                     })
    //                 })
    //         })
    //         .collect()
    // }

    // pub fn prometheus_queries_for_relation(
    //     &self,
    //     relation: &Relation,
    //     items: &Items,
    //     types: &Types,
    //     schema: &prometheus::Universe,
    // ) -> Vec<prometheus::MetricSelector> {
    //     self.prometheus_metrics
    //         .iter()
    //         .flat_map(|(_item_name, (item_ref, metrics))| {
    //             let prom_item = schema.get_item(item_ref);
    //             let item_query = metrics.prometheus_query(relation, items, types);
    //             prom_item
    //                 .svc_queries(schema)
    //                 .into_iter()
    //                 .flat_map(move |mut base_query| {
    //                     base_query &= &item_query;

    //                     let metrics: BTreeMap<_, BTreeSet<_>> = prom_item.all_metrics(schema).fold(
    //                         BTreeMap::new(),
    //                         |mut m, (metric_name, metric)| {
    //                             m.entry(metric.labels().collect::<BTreeMap<_, _>>())
    //                                 .or_default()
    //                                 .insert(metric_name.to_string());
    //                             m
    //                         },
    //                     );

    //                     metrics.into_iter().map(move |(labels, metrics)| {
    //                         let mut q = std::iter::once((
    //                             LabelName::new(String::from("__name__")),
    //                             LabelSelector::In(metrics),
    //                         ))
    //                         .chain(
    //                             labels
    //                                 .into_iter()
    //                                 .map(|(name, value)| (name.clone(), value.clone())),
    //                         )
    //                         .collect();
    //                         q &= &base_query;
    //                         q
    //                     })
    //                 })
    //         })
    //         .collect()
    // }

    pub fn info(&self, pkg: Option<&PackageId>, types: &Types) -> RelationTypeInfo {
        RelationTypeInfo {
            name: self.name.clone(),
            description: self.description.clone(),
            multiplicity: self.multiplicity.clone(),
            bidirectional: self.bidirectional,
            properties: self
                .properties
                .iter(&types.properties)
                .map(|(id, prop)| (id.to_relative_opt(pkg), prop.to_serial()))
                .collect(),
            source: self.source.key().to_relative_opt(pkg),
            target: self.target.key().to_relative_opt(pkg),
            connector: self.connector.as_ref().map(|c| c.to_serial(pkg)),
            prometheus_metrics: self.prometheus_metrics.iter().fold(
                BTreeMap::new(),
                |mut map, (item, (_, metrics))| {
                    map.entry(metrics.package.key().clone())
                        .or_default()
                        .insert(item.clone(), metrics.to_serial(pkg));
                    map
                },
            ),
        }
    }
}

impl PropertyType {
    fn to_serial(&self) -> serial::PropertyType {
        serial::PropertyType {
            name: self.name.clone(),
            description: self.description.clone(),
            value: self.value.clone(),
        }
    }

    pub fn value_from_json(
        &self,
        id: Absolute<PropertyId>,
        value: serde_json::Value,
    ) -> Result<(Absolute<PropertyId>, PropertyValue)> {
        match self.value.value_from_json(value) {
            Ok(value) => Ok((id, value)),
            Err(e) => Err(Error::InvalidPropertyValueWithId(id, Box::new(e))),
        }
    }

    pub fn info(&self) -> serial::PropertyType {
        self.to_serial()
    }
}

impl ItemMetrics {
    fn to_serial(&self, pkg: Option<&PackageId>) -> connections::ItemMetrics {
        connections::ItemMetrics {
            group_by: self.group_by.clone(),
            keys: self
                .keys
                .iter()
                .map(|(label, selector)| (label.clone(), selector.to_serial(pkg)))
                .collect(),
        }
    }

    pub fn labels(&self) -> impl Iterator<Item = &LabelName> {
        self.keys.keys()
    }

    pub fn group_by(&self) -> Option<impl Iterator<Item = &LabelName>> {
        self.group_by.as_ref().map(|labels| labels.iter())
    }

    pub fn metric<W: Wrapper>(
        &self,
        item: &Item<W>,
        items: &Items<W>,
        types: &Types,
    ) -> BTreeMap<LabelName, String> {
        self.keys
            .iter()
            .map(|(label, sel)| {
                let value = sel.value(item, items, types).unwrap_or_else(String::new);
                (label.clone(), value)
            })
            .collect()
    }

    pub fn prometheus_query<W: Wrapper>(
        &self,
        item: &Item<W>,
        items: &Items<W>,
        types: &Types,
    ) -> prometheus_schema::MetricSelector {
        self.keys
            .iter()
            .map(|(label, sel)| (label.clone(), sel.prometheus_query(item, items, types)))
            .collect()
    }
}

impl RelationMetrics {
    fn to_serial(&self, pkg: Option<&PackageId>) -> connections::RelationMetrics {
        connections::RelationMetrics {
            keys: self
                .keys
                .iter()
                .map(|(label, selector)| (label.clone(), selector.to_serial(pkg)))
                .collect(),
        }
    }

    pub fn labels(&self) -> impl Iterator<Item = &LabelName> {
        self.keys.keys()
    }

    pub fn metric<W: Wrapper>(
        &self,
        relation: &Relation<W>,
        items: &Items<W>,
        types: &Types,
    ) -> BTreeMap<LabelName, String> {
        self.keys
            .iter()
            .filter_map(|(label, sel)| {
                Some((label.clone(), sel.value(relation, None, items, types)?))
            })
            .collect()
    }

    pub fn prometheus_query<W: Wrapper>(
        &self,
        relation: &Relation<W>,
        items: &Items<W>,
        types: &Types,
    ) -> prometheus_schema::MetricSelector {
        self.keys
            .iter()
            .map(|(label, sel)| {
                (
                    label.clone(),
                    sel.prometheus_query(relation, None, items, types),
                )
            })
            .collect()
    }
}

impl ItemKeySelector {
    fn to_serial(&self, pkg: Option<&PackageId>) -> connections::ItemKeySelector {
        match self {
            ItemKeySelector::Property(p) => {
                connections::ItemKeySelector::Property(p.key().to_relative_opt(pkg))
            }
            ItemKeySelector::Parent(p) => {
                connections::ItemKeySelector::Parent(Box::new(p.to_serial(pkg)))
            }
            ItemKeySelector::Relation(r) => {
                connections::ItemKeySelector::Relation(Box::new(r.to_serial(pkg)))
            }
        }
    }

    pub fn value<W: Wrapper>(
        &self,
        item: &Item<W>,
        items: &Items<W>,
        types: &Types,
    ) -> Option<String> {
        match self {
            Self::Property(prop) => item.property(prop.key()).map(|v| v.to_string()),
            Self::Parent(follow) => follow.value(item, items, types),
            Self::Relation(follow) => follow.value(item, items, types),
        }
    }

    pub fn prometheus_query<W: Wrapper>(
        &self,
        item: &Item<W>,
        items: &Items<W>,
        types: &Types,
    ) -> LabelSelector {
        self.value(item, items, types)
            .map_or(LabelSelector::Unset, LabelSelector::Eq)
    }
}

impl RelationKeySelector {
    fn to_serial(&self, pkg: Option<&PackageId>) -> connections::RelationKeySelector {
        match self {
            RelationKeySelector::Property(p) => {
                connections::RelationKeySelector::Property(p.key().to_relative_opt(pkg))
            }
            RelationKeySelector::Source(i) => {
                connections::RelationKeySelector::Source(Box::new(i.to_serial(pkg)))
            }
            RelationKeySelector::Target(i) => {
                connections::RelationKeySelector::Target(Box::new(i.to_serial(pkg)))
            }
            RelationKeySelector::Item(i) => {
                connections::RelationKeySelector::Item(Box::new(i.to_serial(pkg)))
            }
        }
    }

    pub fn value<W: Wrapper>(
        &self,
        relation: &Relation<W>,
        endpoint: Option<Endpoint>,
        items: &Items<W>,
        types: &Types,
    ) -> Option<String> {
        match self {
            Self::Property(prop) => relation.property(prop.key()).map(|v| v.to_string()),
            Self::Source(follow) => follow.value(relation, Some(Endpoint::Target), items, types),
            Self::Target(follow) => follow.value(relation, Some(Endpoint::Source), items, types),
            Self::Item(follow) => follow.value(relation, endpoint, items, types),
        }
    }

    pub fn prometheus_query<W: Wrapper>(
        &self,
        relation: &Relation<W>,
        endpoint: Option<Endpoint>,
        items: &Items<W>,
        types: &Types,
    ) -> LabelSelector {
        self.value(relation, endpoint, items, types)
            .map_or(LabelSelector::Unset, LabelSelector::Eq)
    }
}

impl FollowItem {
    fn to_serial(&self, pkg: Option<&PackageId>) -> connections::FollowItem {
        connections::FollowItem {
            item: self.item.to_serial(pkg),
            key: self.key.to_serial(pkg),
        }
    }

    fn value<W: Wrapper>(
        &self,
        relation: &Relation<W>,
        endpoint: Option<Endpoint>,
        items: &Items<W>,
        types: &Types,
    ) -> Option<String> {
        [Endpoint::Source, Endpoint::Target]
            .into_iter()
            .filter(|ep| (endpoint != Some(*ep)))
            .find_map(|ep| {
                let item_ref = relation.endpoint(ep);
                let item = items.borrow_item(item_ref).get_wrapped();
                let ctx = MatchContext::new(items, types);
                let m: Option<bool> =
                    self.item
                        .matches(&ItemArgs::new(item_ref.key(), item), &ctx, &());
                m.matches().then_some(())?;
                self.key.value(item, items, types)
            })
    }
}

impl FollowParent {
    fn to_serial(&self, pkg: Option<&PackageId>) -> connections::FollowParent {
        connections::FollowParent {
            item: self.item.to_serial(pkg),
            key: self.key.to_serial(pkg),
        }
    }

    fn value<W: Wrapper>(&self, item: &Item<W>, items: &Items<W>, types: &Types) -> Option<String> {
        item.parent
            .as_ref()
            .map(|parent_ref| {
                (
                    parent_ref.key(),
                    items.borrow_item(parent_ref).get_wrapped(),
                )
            })
            .filter(|(parent_id, parent)| {
                let ctx = MatchContext::new(items, types);
                let m: Option<bool> =
                    self.item
                        .matches(&ItemArgs::new(parent_id, parent), &ctx, &());
                m.matches()
            })
            .and_then(|(_, parent)| self.key.value(parent, items, types))
    }
}

impl FollowRelation {
    fn to_serial(&self, pkg: Option<&PackageId>) -> connections::FollowRelation {
        connections::FollowRelation {
            relation: self.relation.to_serial(pkg),
            key: self.key.to_serial(pkg),
        }
    }

    fn value<W: Wrapper>(&self, item: &Item<W>, items: &Items<W>, types: &Types) -> Option<String> {
        item.relations(
            self.relation.prefilter_relation_type(),
            self.relation.prefilter_endpoint(),
        )
        .find_map(|((relation_id, rel_ref), endpoint)| {
            let relation = items.borrow_relation(rel_ref).get_wrapped();
            let ctx = MatchContext::new(items, types);
            let m: Option<bool> =
                self.relation
                    .matches(&RelArgs::new(relation_id, relation, endpoint), &ctx, &());
            m.matches().then_some(())?;
            self.key.value(relation, Some(endpoint), items, types)
        })
    }
}

impl<'a> EntityTypeRef<'a> {
    pub fn prometheus_metrics(
        &self,
        prom_item_name: &QualifiedItemName,
        types: &'a Types,
        schema: &'a Universe,
    ) -> Option<(&'a prometheus_schema::Item, EntityMetricsRef<'a>)> {
        match self {
            EntityTypeRef::ItemType(item_type) => {
                let (prom_item, prom_metrics) =
                    item_type.prometheus_metrics(prom_item_name, types, schema)?;
                Some((prom_item, EntityMetricsRef::ItemMetrics(prom_metrics)))
            }
            EntityTypeRef::RelationType(rel_type) => {
                let (prom_item, prom_metrics) =
                    rel_type.prometheus_metrics(prom_item_name, schema)?;
                Some((prom_item, EntityMetricsRef::RelationMetrics(prom_metrics)))
            }
        }
    }
}

impl EntityMetricsRef<'_> {
    pub fn prometheus_query<W: Wrapper>(
        &self,
        entity: &EntityRef<'_, W>,
        items: &Items<W>,
        types: &Types,
    ) -> prometheus_schema::MetricSelector {
        match (self, entity) {
            (EntityMetricsRef::ItemMetrics(item_metrics), EntityRef::Item(item)) => {
                item_metrics.prometheus_query(item, items, types)
            }
            (EntityMetricsRef::RelationMetrics(rel_metrics), EntityRef::Relation(rel)) => {
                rel_metrics.prometheus_query(rel, items, types)
            }
            _ => panic!("got wrong kind of entity for EntityMetricsRef::prometheus_query"),
        }
    }
}
