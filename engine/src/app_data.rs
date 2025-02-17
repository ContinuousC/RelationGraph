/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, fmt::Debug, sync::Arc};

use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use reqwest::Url;
use reqwest_middleware::ClientWithMiddleware;
use tokio::sync::Mutex as AsyncMutex;

use dbdaemon_api::DbClient;
use relation_graph::{alerts::AlertRuleTemplateName, State};

use crate::{
    prom_schema::PromSchemaState,
    roles::{EditorRole, ViewerRole},
};

pub(crate) struct AppData {
    pub(crate) db: DbClient,
    pub(crate) es: ClientWithMiddleware,
    pub(crate) prom: ClientWithMiddleware,
    pub(crate) es_url: Url,
    pub(crate) prom_url: Url,
    pub(crate) ruler_url: Url,
    pub(crate) alertmanager_url: Url,
    pub(crate) state: RwLock<State>,
    pub(crate) prom_schema: PromSchemaState,
    pub(crate) locks: Locks,
    pub(crate) app_version: String,
}

pub(crate) struct Locks {
    /// Protects the access to the alert rule spec and config table in
    /// the database and the definitions sent to prometheus ruler.
    pub(crate) alert_rules: RwLock<BTreeMap<AlertRuleTemplateName, Arc<AsyncMutex<()>>>>,
}

impl AppData {
    pub(crate) fn read_state(&self, _role: ViewerRole) -> RwLockReadGuard<State> {
        self.state.read()
    }

    pub(crate) fn write_state(&self, _role: EditorRole) -> RwLockWriteGuard<State> {
        self.state.write()
    }
}

#[derive(Debug)]
enum AppDataDebug {
    DbClient,
    State,
}

impl Debug for AppData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AppData")
            .field("db", &AppDataDebug::DbClient)
            .field("es", &self.es)
            .field("prom", &self.prom)
            .field("es_url", &self.es_url)
            .field("prom_url", &self.prom_url)
            .field("ruler_url", &self.ruler_url)
            .field("alertmanager_url", &self.alertmanager_url)
            .field("state", &AppDataDebug::State)
            .finish()
    }
}
