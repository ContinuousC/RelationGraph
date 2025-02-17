/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::{atomic::AtomicBool, Arc},
};

use actix_web::web::{Data, Json, Path};
use apistos::{api_operation, ApiComponent};
use chrono::{DateTime, TimeDelta, Utc};
use parking_lot::RwLock;
use prometheus_core::LabelName;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use tokio::sync::{OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock as AsyncRwLock};
use tracing::instrument;

use prometheus_api::{DataPoint2, GenericMetric, InstantQueryParams, Value};
use prometheus_schema::{
    generate::{Choice, Choose, GenInfo, Hints, SplitBy},
    schema::Branch,
    serial::{Item, Module, Root},
    ItemName, ItemRef, LabelSelector, MetricSelector, ModuleName, ModuleVersionReq,
    QualifiedItemName, Universe,
};

use crate::{
    auth::Auth,
    roles::{EditorRole, ViewerRole},
    utils::{json::JsonFile, yaml::YamlFile},
    AppData, Error, Result,
};

use super::browse::{
    build_tree, collect_info, tree_items, tree_metrics, Items, ItemsParams, MetricsData,
    MetricsParams, Tree, TreeInfo, TreeInfoItem, TreeInfoMetric,
};

#[derive(Default)]
pub(crate) struct PromSchemaState(RwLock<BTreeMap<ModuleName, Arc<AsyncRwLock<GenerateState>>>>);

#[derive(Default)]
pub(crate) struct GenerateState {
    metrics: Option<Arc<LoadedMetrics>>,
    geninfo: GenInfo,
    schema: Option<GeneratedSchema>,
}

struct GeneratedSchema {
    universe: Universe,
    generated: prometheus_schema::generate::GeneratedSchema,
}

struct LoadedMetrics {
    time: DateTime<Utc>,
    metrics: Vec<GenericMetric<DataPoint2<Value>>>,
}

#[derive(Deserialize, JsonSchema, ApiComponent, Clone, Debug)]
pub(crate) struct LoadMetricsParams {
    query: MetricSelector,
    #[serde(default)]
    force: bool,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
pub(crate) struct ModuleInfo {
    metrics: Option<LoadedMetricsInfo>,
    schema: Option<SchemaInfo>,
    hints: Hints,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
pub(crate) struct SchemaInfo {
    nitems: usize,
    root: ItemName,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
pub(crate) struct LoadMetricsInfo {
    cached: bool,
    info: LoadedMetricsInfo,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
pub(crate) struct LoadedMetricsInfo {
    query: MetricSelector,
    nseries: usize,
    time: DateTime<Utc>,
    common_by_value: BTreeMap<LabelName, String>,
    common_by_presence: BTreeSet<LabelName>,
}

/* Module list info and manipulation. */

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
pub(crate) struct ModuleList(BTreeSet<ModuleName>);

#[instrument]
#[api_operation(summary = "List prometheus schema modules in process of generation")]
pub(crate) async fn get_modules(_role: Auth<ViewerRole>, data: Data<AppData>) -> Json<ModuleList> {
    let state = data.prom_schema.0.read();
    Json(ModuleList(state.keys().cloned().collect()))
}

#[instrument]
#[api_operation(
    summary = "Get information about a prometheus schema module in process of generation"
)]
pub(crate) async fn get_module(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
) -> Option<Json<ModuleInfo>> {
    let name = path.into_inner();
    let state = data.prom_schema.read(&name).await?;
    Some(Json(state.info()))
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Created {
    Created,
}

#[instrument]
#[api_operation(summary = "Create a prometheus schema module for schema generation")]
pub(crate) async fn put_module(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
) -> Option<Json<Created>> {
    let name = path.into_inner();
    data.prom_schema.0.write().insert(name, Default::default());
    Some(Json(Created::Created))
}

#[derive(Deserialize, JsonSchema, ApiComponent, Debug)]
pub(crate) struct RenameModuleParams {
    to: ModuleName,
}

#[instrument]
#[api_operation(summary = "Rename a prometheus schema module for schema generation")]
pub(crate) async fn post_rename_module(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
    params: Json<RenameModuleParams>,
) -> Result<Json<Renamed>> {
    let name = path.into_inner();
    let new_name = params.into_inner().to;
    let mut state = data.prom_schema.0.write();
    let module = state
        .remove(&name)
        .ok_or_else(|| Error::MissingPromModule(name))?;
    state.insert(new_name, module);
    Ok(Json(Renamed::Renamed))
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Deleted {
    Deleted,
}

#[instrument]
#[api_operation(summary = "Delete a prometheus schema module in process of generation")]
pub(crate) async fn delete_module(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
) -> Option<Json<Deleted>> {
    let name = path.into_inner();
    data.prom_schema.0.write().remove(&name);
    Some(Json(Deleted::Deleted))
}

#[instrument]
#[api_operation(summary = "Get info about loaded prometheus metrics for schema generation")]
pub(crate) async fn get_loaded_metrics(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
) -> Option<Json<LoadedMetricsInfo>> {
    let name = path.into_inner();
    let state = data.prom_schema.read(&name).await?;
    Some(Json(state.metrics_info()?))
}

#[instrument]
#[api_operation(summary = "Load prometheus metrics for schema generation")]
pub(crate) async fn post_load_metrics(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
    params: Json<LoadMetricsParams>,
) -> Result<Json<LoadMetricsInfo>> {
    let name = path.into_inner();
    let now = Utc::now();
    let mut state = data.prom_schema.write_or_create(name.clone()).await;

    let cached = if !params.force
        && state.metrics.as_ref().is_some_and(|metrics| {
            state.geninfo.query == params.query && now - metrics.time < TimeDelta::minutes(15)
        }) {
        true
    } else {
        let metrics = crate::metrics::run_query::<_, _, Vec<GenericMetric<DataPoint2<Value>>>>(
            role.into_inner().into(),
            &data,
            params.query.to_string(),
            InstantQueryParams { time: None },
        )
        .await?;

        state.metrics = Some(Arc::new(LoadedMetrics { time: now, metrics }));
        state.geninfo.query = params.query.clone();
        state.generate_schema(&name).await?;
        false
    };

    Ok(Json(LoadMetricsInfo {
        cached,
        info: state.metrics_info().unwrap(),
    }))
}

#[instrument]
#[api_operation(summary = "Get generated module tree")]
pub(crate) async fn get_tree(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
) -> Result<Json<Tree>> {
    let module = path.into_inner();
    let state = data
        .prom_schema
        .read(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = state.schema()?;
    Ok(Json(Tree(vec![
        build_tree(schema, schema.root_name(), schema.root(), 0).0,
    ])))
}

#[derive(Deserialize, ApiComponent, JsonSchema, Debug)]
pub struct TreeInfoPath {
    pub module: ModuleName,
    pub path: Vec<String>,
}

#[derive(Deserialize, ApiComponent, JsonSchema, Debug)]
pub struct ItemPath {
    pub module: ModuleName,
    pub item: ItemName,
}

#[derive(Serialize, ApiComponent, JsonSchema, Clone)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum GenerateTreeInfo {
    Item(GenerateTreeInfoItem),
    Metric(TreeInfoMetric),
}

#[derive(Serialize, JsonSchema, Clone)]
pub struct GenerateTreeInfoItem {
    pub split_by: BTreeSet<LabelName>,
    pub splittable: BTreeSet<LabelName>,
    pub choice: Choice,
    #[serde(flatten)]
    pub browse: TreeInfoItem,
}

#[instrument]
#[api_operation(summary = "Get info at path in the generated module tree")]
pub(crate) async fn get_tree_info(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<TreeInfoPath>,
) -> Result<Json<GenerateTreeInfo>> {
    let TreeInfoPath { module, path } = path.into_inner();
    let state = data
        .prom_schema
        .read(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let gen = state.generated()?;
    let info = match super::browse::tree_info(&gen.universe, &path)? {
        TreeInfo::Item(browse) => {
            let full_query =
                browse
                    .collected
                    .iter()
                    .skip(1)
                    .fold(MetricSelector::new(), |mut m, info| {
                        m &= &info.query;
                        m
                    });
            let all_keys = browse
                .collected
                .iter()
                .flat_map(|info| &info.keys)
                .collect::<BTreeSet<_>>();
            let split_by = state
                .geninfo
                .hints
                .split_by
                .iter()
                .filter(|split| full_query.is_subset(&split.query))
                .map(|split| &split.label)
                .cloned()
                .collect();
            let splittable = all_keys.into_iter().cloned().collect();
            let choice = gen
                .generated
                .choices
                .iter()
                .find(|c| c.query == full_query)
                .unwrap()
                .clone();

            GenerateTreeInfo::Item(GenerateTreeInfoItem {
                split_by,
                splittable,
                browse,
                choice,
            })
        }
        TreeInfo::Metric(browse) => GenerateTreeInfo::Metric(browse),
    };
    Ok(Json(info))
}

#[instrument]
#[api_operation(summary = "Get prometheus items at path")]
pub(crate) async fn post_tree_items(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<TreeInfoPath>,
    params: Json<ItemsParams>,
) -> Result<Json<Items>> {
    let TreeInfoPath { module, path } = path.into_inner();
    let state = data
        .prom_schema
        .read(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = state.schema()?;
    let info = collect_info(schema, &path)?.0;
    let items = tree_items(role.into_inner(), &data, &info, &params).await?;
    Ok(Json(items))
}

#[instrument]
#[api_operation(summary = "Get prometheus metrics at path")]
pub(crate) async fn post_tree_metrics(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<TreeInfoPath>,
    params: Json<MetricsParams>,
) -> Result<Json<MetricsData>> {
    let TreeInfoPath { module, path } = path.into_inner();
    let (info, tree) = {
        let state = data
            .prom_schema
            .read(&module)
            .await
            .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
        let schema = state.schema()?;
        let (info, branch) = collect_info(schema, &path)?;
        let branch = match branch {
            Branch::Item(name, item) => item.tree(schema, name),
            Branch::Metric(name, metric) => metric.tree(name),
        };
        (info, branch.tree)
    };
    let metrics = tree_metrics(role.into_inner(), &data, info, tree, &params).await?;
    Ok(Json(metrics))
}

#[derive(Deserialize, JsonSchema, ApiComponent, Debug)]
pub(crate) struct RenameItemParams {
    to: ItemName,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Renamed {
    Renamed,
}

#[instrument]
#[api_operation(summary = "Rename a prometheus item")]
pub(crate) async fn put_rename_item(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<ItemPath>,
    params: Json<RenameItemParams>,
) -> Result<Json<Renamed>> {
    let ItemPath { module, item } = path.into_inner();
    let name = QualifiedItemName::new(module.clone(), item.clone());
    let mut state = data
        .prom_schema
        .write(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = state.schema()?;
    let item = schema
        .lookup_item(&name)
        .ok_or_else(|| Error::MissingPromItem(name.clone()))?;
    let paths = item.with_paths(&name, schema, |path| {
        path.fold(MetricSelector::new(), |mut m, (_, item)| {
            m &= &item.query;
            m
        })
    });
    paths.into_iter().for_each(|path| {
        state
            .geninfo
            .hints
            .rename
            .insert_updating_all(params.to.clone(), path);
    });
    state.generate_schema(&module).await?;
    Ok(Json(Renamed::Renamed))
}

#[instrument]
#[api_operation(summary = "Unrename a prometheus item")]
pub(crate) async fn delete_rename_item(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<ItemPath>,
) -> Result<Json<Done>> {
    let ItemPath { module, item } = path.into_inner();
    let name = QualifiedItemName::new(module.clone(), item.clone());
    let mut state = data
        .prom_schema
        .write(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = state.schema()?;
    let item = schema
        .lookup_item(&name)
        .ok_or_else(|| Error::MissingPromItem(name.clone()))?;
    let paths = item.with_paths(&name, schema, |path| {
        path.fold(MetricSelector::new(), |mut m, (_, item)| {
            m &= &item.query;
            m
        })
    });
    paths.into_iter().for_each(|path| {
        state.geninfo.hints.rename.remove_right(&path);
    });
    state.generate_schema(&module).await?;
    Ok(Json(Done::Done))
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Done {
    Done,
}

#[derive(Deserialize, JsonSchema, ApiComponent, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) struct ChooseParams {
    choice: BTreeSet<LabelName>,
}

#[instrument]
#[api_operation(summary = "Make a choice at path")]
pub(crate) async fn put_choose_at_path(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<TreeInfoPath>,
    params: Json<ChooseParams>,
) -> Result<Json<Done>> {
    let TreeInfoPath { module, path } = path.into_inner();
    let ChooseParams { choice } = params.into_inner();
    let mut state = data
        .prom_schema
        .write(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = state.schema()?;
    let (info, _) = collect_info(schema, &path)?;
    let query = info
        .iter()
        .skip(1)
        .fold(MetricSelector::new(), |mut q, info| {
            q &= &info.query;
            q
        });
    state
        .geninfo
        .hints
        .choose
        .retain(|choose| choose.query != query);
    state.geninfo.hints.choose.push(Choose { query, choice });
    state.generate_schema(&module).await?;
    Ok(Json(Done::Done))
}

#[instrument]
#[api_operation(summary = "Revert to default choice at and below path")]
pub(crate) async fn delete_choose_at_path(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<TreeInfoPath>,
) -> Result<Json<Done>> {
    let TreeInfoPath { module, path } = path.into_inner();
    let mut state = data
        .prom_schema
        .write(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = state.schema()?;
    let (info, _) = collect_info(schema, &path)?;
    let query = info
        .iter()
        .skip(1)
        .fold(MetricSelector::new(), |mut q, info| {
            q &= &info.query;
            q
        });
    state
        .geninfo
        .hints
        .choose
        .retain(|choose| choose.query.is_superset(&query));
    state.generate_schema(&module).await?;
    Ok(Json(Done::Done))
}

#[derive(Deserialize, JsonSchema, ApiComponent, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) struct SplitByParams {
    label: LabelName,
}

#[derive(Deserialize, JsonSchema, ApiComponent, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) struct SplitByQueryParams {
    label: LabelName,
    query: MetricSelector,
}

#[instrument]
#[api_operation(summary = "Split by label at an item")]
pub(crate) async fn put_split_by_label(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<TreeInfoPath>,
    params: Json<SplitByParams>,
) -> Result<Json<Done>> {
    let TreeInfoPath { module, path } = path.into_inner();
    let SplitByParams { label } = params.into_inner();
    let mut state = data
        .prom_schema
        .write(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = state.schema()?;
    let (info, _) = collect_info(schema, &path)?;
    let query = info
        .iter()
        .skip(1)
        .fold(MetricSelector::new(), |mut q, info| {
            q &= &info.query;
            q
        });
    state
        .geninfo
        .hints
        .split_by
        .retain(|split| !(split.label == label && split.query.is_subset(&query)));
    state.geninfo.hints.split_by.push(SplitBy { query, label });
    state.generate_schema(&module).await?;
    Ok(Json(Done::Done))
}

#[instrument]
#[api_operation(summary = "Unsplit by label at an item")]
pub(crate) async fn delete_split_by_label(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<TreeInfoPath>,
    params: Json<SplitByParams>,
) -> Result<Json<Done>> {
    let TreeInfoPath { module, path } = path.into_inner();
    let SplitByParams { label } = params.into_inner();
    let mut state = data
        .prom_schema
        .write(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = state.schema()?;
    let (info, _) = collect_info(schema, &path)?;
    let query = info
        .iter()
        .skip(1)
        .fold(MetricSelector::new(), |mut q, info| {
            q &= &info.query;
            q
        });
    state
        .geninfo
        .hints
        .split_by
        .retain(|split| !(split.label == label && split.query.is_superset(&query)));
    state.generate_schema(&module).await?;
    Ok(Json(Done::Done))
}

#[instrument]
#[api_operation(summary = "Split by label at a query")]
pub(crate) async fn put_split_by_label_query(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    module: Path<ModuleName>,
    params: Json<SplitByQueryParams>,
) -> Result<Json<Done>> {
    let module = module.into_inner();
    let SplitByQueryParams { label, query } = params.into_inner();
    let mut state = data
        .prom_schema
        .write(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    state
        .geninfo
        .hints
        .split_by
        .retain(|split| !(split.label == label && split.query.is_subset(&query)));
    state.geninfo.hints.split_by.push(SplitBy { label, query });
    state.generate_schema(&module).await?;
    Ok(Json(Done::Done))
}

#[instrument]
#[api_operation(summary = "Unsplit by label at a query")]
pub(crate) async fn delete_split_by_label_query(
    _role: Auth<EditorRole>,
    data: Data<AppData>,
    module: Path<ModuleName>,
    params: Json<SplitByQueryParams>,
) -> Result<Json<Done>> {
    let module = module.into_inner();
    let SplitByQueryParams { label, query } = params.into_inner();
    let mut state = data
        .prom_schema
        .write(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    state
        .geninfo
        .hints
        .split_by
        .retain(|split| !(split.label == label && split.query.is_subset(&query)));
    state.generate_schema(&module).await?;
    Ok(Json(Done::Done))
}

#[instrument]
#[api_operation(summary = "Download module")]
pub(crate) async fn get_module_download(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
) -> Result<YamlFile<Module>> {
    let module = path.into_inner();
    let state = data
        .prom_schema
        .read(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let schema = &state
        .schema
        .as_ref()
        .ok_or(Error::MissingPromSchema)?
        .generated
        .module;
    Ok(YamlFile(format!("{module}.schema.yaml"), schema.clone()))
}

#[derive(Serialize, JsonSchema, ApiComponent, Debug)]
pub(crate) struct DownloadMetrics(Vec<GenericMetric<DataPoint2<Value>>>);

#[instrument]
#[api_operation(summary = "Download metrics")]
pub(crate) async fn get_metrics_download(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
) -> Result<JsonFile<DownloadMetrics>> {
    let module = path.into_inner();
    let state = data
        .prom_schema
        .read(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    let metrics = state.metrics.as_ref().ok_or(Error::MissingPromMetrics)?;
    Ok(JsonFile(
        format!("{module}.metrics.json"),
        DownloadMetrics(metrics.metrics.clone()),
    ))
}

#[instrument]
#[api_operation(summary = "Download module hints")]
pub(crate) async fn get_hints_download(
    _role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ModuleName>,
) -> Result<YamlFile<GenInfo>> {
    let module = path.into_inner();
    let state = data
        .prom_schema
        .read(&module)
        .await
        .ok_or_else(|| Error::MissingPromModule(module.clone()))?;
    Ok(YamlFile(
        format!("{module}.generate.yaml"),
        state.geninfo.clone(),
    ))
}

impl PromSchemaState {
    async fn read(&self, name: &ModuleName) -> Option<OwnedRwLockReadGuard<GenerateState>> {
        let state = self.0.read().get(name)?.clone();
        Some(state.read_owned().await)
    }

    async fn write(&self, name: &ModuleName) -> Option<OwnedRwLockWriteGuard<GenerateState>> {
        let state = self.0.read().get(name)?.clone();
        Some(state.write_owned().await)
    }

    async fn write_or_create(&self, name: ModuleName) -> OwnedRwLockWriteGuard<GenerateState> {
        let state = self.0.write().entry(name).or_default().clone();
        state.write_owned().await
    }
}

impl GenerateState {
    fn info(&self) -> ModuleInfo {
        ModuleInfo {
            schema: self
                .schema
                .as_ref()
                .map(|gen| SchemaInfo::new(&gen.generated.module, &self.geninfo.hints)),
            metrics: self.metrics_info(),
            hints: self.geninfo.hints.clone(),
        }
    }

    fn generated(&self) -> Result<&GeneratedSchema> {
        self.schema.as_ref().ok_or(Error::MissingPromSchema)
    }

    fn schema(&self) -> Result<&Universe> {
        Ok(&self.generated()?.universe)
    }

    async fn generate_schema(&mut self, name: &ModuleName) -> Result<()> {
        let metrics = self
            .metrics
            .as_ref()
            .ok_or(Error::MissingPromMetrics)?
            .clone();
        let geninfo = self.geninfo.clone();
        let term = Arc::new(AtomicBool::new(false));
        let mut task = tokio::task::spawn_blocking({
            let term = term.clone();
            move || {
                prometheus_schema::generate::generate_schema(
                    &metrics.metrics,
                    &geninfo,
                    &term,
                    2,
                    1,
                )
            }
        });
        let res = tokio::select! {
            r = &mut task => r,
            _ = tokio::time::sleep(std::time::Duration::from_secs(60)) => {
                term.store(true, std::sync::atomic::Ordering::Release);
                task.await
            }
        };
        let generated = res
            .map_err(Error::GenerateSchemaJoin)?
            .ok_or(Error::GenerateSchema)?;

        let query = &self.geninfo.query;
        let root = Root {
            root: ItemRef::new(None, ItemName::new("root")),
            requires: BTreeMap::from_iter([(
                name.clone(),
                ModuleVersionReq::new("^0.1.0".parse().unwrap()),
            )]),
            items: BTreeMap::from_iter([(
                ItemName::new("root"),
                Item {
                    query: query.clone(),
                    keys: query
                        .0
                        .iter()
                        .filter_map(|(label, selector)| match selector {
                            LabelSelector::Opt
                            | LabelSelector::Set
                            | LabelSelector::Ne(_)
                            | LabelSelector::NotIn(_) => Some(label.clone()),
                            LabelSelector::In(vals) if vals.len() > 1 => Some(label.clone()),
                            LabelSelector::Unset | LabelSelector::Eq(_) | LabelSelector::In(_) => {
                                None
                            }
                        })
                        .collect(),
                    items: BTreeSet::from_iter([ItemRef::new(
                        Some(name.clone()),
                        self.geninfo.hints.root_name(),
                    )]),
                    ..Default::default()
                },
            )]),
        };
        let schema = root
            .resolve(BTreeMap::from_iter([(
                name.clone(),
                generated.module.clone(),
            )]))
            .map_err(Error::PromResolve)?;

        self.schema = Some(GeneratedSchema {
            universe: schema,
            generated,
        });
        Ok(())
    }

    fn metrics_info(&self) -> Option<LoadedMetricsInfo> {
        let metrics = self.metrics.as_ref()?;

        let mut metrics_iter = metrics.metrics.iter().map(|metric| &metric.metric);
        let init = metrics_iter
            .next()
            .map_or_else(BTreeMap::new, |metric| metric.iter().collect());
        let common_by_value = metrics_iter.fold(init, |mut m, metric| {
            m.retain(|label, value| (metric.get(*label) == Some(*value)));
            m
        });

        let mut metrics_iter = metrics.metrics.iter().map(|metric| &metric.metric);
        let init = metrics_iter
            .next()
            .map_or_else(BTreeSet::new, |metric| metric.keys().collect());
        let common_by_presence = metrics_iter.fold(init, |mut m, metric| {
            m.retain(|label| metric.contains_key(*label));
            m
        });

        static SPECIAL: &[&str] = &["__name__", "le", "quantile"];

        Some(LoadedMetricsInfo {
            query: self.geninfo.query.clone(),
            nseries: metrics.metrics.len(),
            time: metrics.time,
            common_by_presence: common_by_presence
                .into_iter()
                .filter(|label| !SPECIAL.contains(&label.as_str()))
                .cloned()
                .collect(),
            common_by_value: common_by_value
                .into_iter()
                .filter(|(label, _)| !SPECIAL.contains(&label.as_str()))
                .map(|(label, value)| (label.clone(), value.clone()))
                .collect(),
        })
    }
}

impl SchemaInfo {
    fn new(module: &Module, hints: &Hints) -> Self {
        Self {
            nitems: module.items.len(),
            root: hints.root_name(),
        }
    }
}
