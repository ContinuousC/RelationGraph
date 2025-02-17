/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;

use prometheus_schema as prometheus;
use serde::{Deserialize, Serialize};

use crate::{
    alerts::{
        AlertRuleTemplateName, AlertRulesState, PutAlertRuleForm, PutAlertRuleFormResult,
        VersionedAlertMap,
    },
    error::{Error, Result},
    ids::{ItemId, PackageId},
    items::{
        resolved::{TxItems, Txs, Updates},
        serial,
    },
    status::VersionedStatusMap,
    types::{
        resolved::Types,
        serial::{Package, Packages},
    },
    Absolute, ConnectionsPackage, ConnectionsPackageId, ConnectionsPackages, Dashboards, Relation,
    RelationId, RelationTypeId, TransactionId, Views,
};

pub struct State {
    pub packages: Packages,
    pub types: Types,
    pub items: TxItems,
    pub connections: Option<ConnectionsPackages>,
    pub prometheus: Option<PromSchema>,
    pub views: Views,
    pub alert_rules: AlertRulesState,
    pub status: VersionedStatusMap,
    pub alerts: VersionedAlertMap,
    pub dashboards: Dashboards,
}

pub struct PromSchema {
    pub resolved: prometheus::Universe,
    pub root: prometheus::serial::Root,
    pub modules: BTreeMap<prometheus::ModuleName, prometheus::serial::Module>,
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct PackageData {
    pub packages: Packages,
    pub connections: Option<ConnectionsPackages>,
    pub prom_schema: Option<PromSchemaData>,
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct PromSchemaData {
    pub root: prometheus_schema::serial::Root,
    pub modules: BTreeMap<prometheus_schema::ModuleName, prometheus_schema::serial::Module>,
}

impl State {
    pub fn new(packages: Packages, types: Types, items: TxItems) -> Self {
        Self {
            packages,
            types,
            items,
            connections: None,
            prometheus: None,
            alert_rules: AlertRulesState::default(),
            status: VersionedStatusMap::default(),
            alerts: VersionedAlertMap::default(),
            views: Views::default(),
            dashboards: Dashboards::default(),
        }
    }

    pub fn with_alert_rules(mut self, alert_rules: AlertRulesState) -> Self {
        self.alert_rules = alert_rules;
        self
    }

    pub fn with_alert_state(
        mut self,
        status: VersionedStatusMap,
        alerts: VersionedAlertMap,
    ) -> Self {
        self.status = status;
        self.alerts = alerts;
        self
    }

    pub fn with_view_state(mut self, views: Views, dashboards: Dashboards) -> Self {
        self.views = views;
        self.dashboards = dashboards;
        self
    }

    // pub async fn load(path: &std::path::Path) -> Result<Self> {
    //     match path.exists() {
    //         true => {
    //             let data = tokio::fs::read(path)
    //                 .await
    //                 .map_err(|e| Error::ReadState(path.to_path_buf(), e))?;
    //             let state = serde_json::from_slice::<StateSerial>(&data)
    //                 .map_err(|e| Error::DecodeState(path.to_path_buf(), e))?;
    //             let types = state.packages.types()?;
    //             Ok(Self {
    //                 packages: state.packages,
    //                 items: state.items,
    //                 types,
    //             })
    //         }
    //         false => Ok(Self::new()),
    //     }
    // }

    // pub async fn save(self, path: &std::path::Path) -> Result<()> {
    //     let data = serde_json::to_vec(&StateSerial {
    //         packages: self.packages,
    //         items: self.items,
    //     })
    //     .map_err(|e| Error::EncodeState(path.to_path_buf(), e))?;
    //     tokio::fs::write(path, data)
    //         .await
    //         .map_err(|e| Error::WriteState(path.to_path_buf(), e))
    // }

    pub fn load_package(&mut self, pkg_id: PackageId, pkg: Package) -> Result<()> {
        let mut packages = self.packages.clone();
        packages.insert(pkg_id, pkg);

        let types = packages.types()?;

        // TODO: re-resolve items!

        self.packages = packages;
        self.types = types;

        Ok(())
    }

    pub fn unload_package(&mut self, pkg_id: &PackageId) -> Result<()> {
        let mut packages = self.packages.clone();
        packages.remove(pkg_id);

        let types = packages.types()?;

        // TODO: re-resolve items!

        self.packages = packages;
        self.types = types;

        Ok(())
    }

    pub fn load_connections(
        &mut self,
        connections: ConnectionsPackages,
        prom_root: prometheus::serial::Root,
        prom_modules: BTreeMap<prometheus::ModuleName, prometheus::serial::Module>,
    ) -> Result<()> {
        let old_connections = self.connections.take();
        let old_schema = self.prometheus.take();

        self.reload_connections(
            old_schema,
            old_connections,
            connections,
            prom_root,
            prom_modules,
        )
    }

    pub fn update_connections(
        &mut self,
        pkg_id: ConnectionsPackageId,
        pkg: ConnectionsPackage,
    ) -> Result<()> {
        let old_schema = self
            .prometheus
            .take()
            .ok_or(Error::MissingPrometheusSchema)?;
        let old_connections = self.connections.take().unwrap_or_default();

        let prom_root = old_schema.root.clone();
        let prom_modules = old_schema.modules.clone();

        let mut connections = old_connections.clone();
        connections.0.insert(pkg_id, pkg);

        self.reload_connections(
            Some(old_schema),
            Some(old_connections),
            connections,
            prom_root,
            prom_modules,
        )
    }

    pub fn remove_connections(&mut self, pkg_id: &ConnectionsPackageId) -> Result<()> {
        let old_schema = self
            .prometheus
            .take()
            .ok_or(Error::MissingPrometheusSchema)?;
        let old_connections = self.connections.take().unwrap_or_default();

        let prom_root = old_schema.root.clone();
        let prom_modules = old_schema.modules.clone();

        let mut connections = old_connections.clone();
        connections.0.remove(pkg_id);

        self.reload_connections(
            Some(old_schema),
            Some(old_connections),
            connections,
            prom_root,
            prom_modules,
        )
    }

    fn reload_connections(
        &mut self,
        old_schema: Option<PromSchema>,
        old_connections: Option<ConnectionsPackages>,
        connections: ConnectionsPackages,
        prom_root: prometheus::serial::Root,
        prom_modules: BTreeMap<prometheus::ModuleName, prometheus::serial::Module>,
    ) -> Result<()> {
        let prom_schema = prom_root
            .clone() // TODO: avoid clone?
            .resolve(prom_modules.clone())
            .map_err(Error::PromResolve)?;

        match self
            .types
            .resolve_connections(&connections, &self.packages, &prom_schema)
        {
            Ok(()) => {
                self.connections = Some(connections);
                self.prometheus = Some(PromSchema {
                    root: prom_root,
                    modules: prom_modules,
                    resolved: prom_schema,
                });
                Ok(())
            }
            Err(e) => {
                if let (Some(schema), Some(connections)) = (old_schema, old_connections) {
                    self.types.resolve_connections(
                        &connections,
                        &self.packages,
                        &schema.resolved,
                    )?;
                    self.connections = Some(connections);
                    self.prometheus = Some(schema);
                }
                Err(e)
            }
        }
    }

    pub fn package_data(&self) -> PackageData {
        PackageData {
            packages: self.packages.clone(),
            connections: self.connections.clone(),
            prom_schema: self.prometheus.as_ref().map(|schema| PromSchemaData {
                root: schema.root.clone(),
                modules: schema.modules.clone(),
            }),
        }
    }

    pub fn get_prometheus_schema(&self) -> Result<&prometheus::Universe> {
        Ok(&self
            .prometheus
            .as_ref()
            .ok_or(Error::MissingPrometheusSchema)?
            .resolved)
    }

    pub fn insert_item(
        &mut self,
        id: ItemId,
        item: serial::Item,
        pkg: Option<&PackageId>,
    ) -> Result<Updates> {
        self.items.insert_item(id, item, &self.types, pkg)
    }

    pub fn remove_item(&mut self, id: &ItemId) -> Updates {
        self.items.remove_item(id, &self.types)
    }

    pub fn insert_relation(
        &mut self,
        id: RelationId,
        relation: serial::Relation,
        pkg: Option<&PackageId>,
    ) -> Result<Updates> {
        self.items.insert_relation(id, relation, &self.types, pkg)
    }

    pub fn remove_relation(&mut self, id: &RelationId) -> Updates {
        self.items.remove_relation(id, &self.types)
    }

    pub fn insert_items(
        &mut self,
        domain: serial::Domain,
        items: serial::Items,
        pkg: Option<&PackageId>,
    ) -> Result<Updates> {
        self.items.insert_items(domain, items, &self.types, pkg)
    }

    pub fn tx_insert_item(
        &mut self,
        tx: TransactionId,
        id: ItemId,
        item: serial::Item,
        pkg: Option<&PackageId>,
    ) -> Result<()> {
        self.items.tx_insert_item(tx, id, item, &self.types, pkg)
    }

    pub fn tx_insert_relation(
        &mut self,
        tx: TransactionId,
        id: RelationId,
        relation: serial::Relation,
        pkg: Option<&PackageId>,
    ) -> Result<()> {
        self.items
            .tx_insert_relation(tx, id, relation, &self.types, pkg)
    }

    pub fn tx_insert_items(
        &mut self,
        tx: TransactionId,
        domain: serial::Domain,
        items: serial::Items,
        pkg: Option<&PackageId>,
    ) -> Result<()> {
        self.items
            .tx_insert_items(tx, domain, items, &self.types, pkg)
    }

    /// Used for declarative relations.
    pub fn replace_resolved_relations(
        &mut self,
        relations: BTreeMap<Absolute<RelationTypeId>, Vec<Relation<Txs>>>,
    ) -> Result<(Updates, usize, usize)> {
        self.items
            .replace_resolved_relations(relations, &self.types)
    }

    pub fn commit_transaction(&mut self, id: TransactionId) -> Result<Updates> {
        self.items.commit_transaction(id, &self.types)
    }

    pub fn load_views(&mut self, views: Views) {
        self.views = views;
    }

    pub fn verify_put_rule_form(
        &self,
        name: AlertRuleTemplateName,
        rule_form: PutAlertRuleForm,
    ) -> Result<PutAlertRuleFormResult> {
        let schema = &self
            .prometheus
            .as_ref()
            .ok_or(Error::MissingPrometheusSchema)?
            .resolved;
        self.alert_rules
            .verify_put_rule_form(name, rule_form, schema)
    }
}
