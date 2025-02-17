/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::{BTreeMap, BTreeSet};

use actix_web::web::{Data, Json, Path};
use apistos::{api_operation, ApiComponent};

use prometheus_api::{DataPoint2, GenericLabels, GenericMetric, InstantQueryParams, Value};
use prometheus_core::{LabelName, MetricName};
use prometheus_schema::{
    schema::Branch, serial::Metric as SchemaMetric, Item, ItemName, LabelSelector, MetricSelector,
    ModuleName, ModuleVersion, ModuleVersionReq, QualifiedItemName, Universe,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::{auth::Auth, roles::ViewerRole, AppData, Error, Result};

#[derive(Serialize, JsonSchema, ApiComponent, Clone, Default, Debug)]
pub(crate) struct Tree(pub(crate) Vec<Node>);

#[derive(Serialize, JsonSchema, Clone, Debug)]
pub(crate) struct Node {
    id: String,
    label: String,
    #[serde(skip_serializing_if = "Tree::is_empty")]
    children: Tree,
}

impl Tree {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[instrument]
#[api_operation(summary = "Get prometheus schema tree")]
pub(crate) async fn get_tree(role: Auth<ViewerRole>, data: Data<AppData>) -> Result<Json<Tree>> {
    let state = data.read_state(role.into_inner());
    let schema = state.get_prometheus_schema()?;
    Ok(Json(Tree(vec![
        build_tree(schema, schema.root_name(), schema.root(), 0).0,
    ])))
}

pub(crate) fn build_tree(
    schema: &Universe,
    item_name: &QualifiedItemName,
    item: &Item,
    next_id: u64,
) -> (Node, u64) {
    let id = next_id.to_string();
    let (children, next_id) = item.items.iter().fold(
        (Vec::new(), next_id + 1),
        |(mut vs, id), (child_name, child_ref)| {
            let (child, id) = build_tree(schema, child_name, schema.get_item(child_ref), id);
            vs.push(child);
            (vs, id)
        },
    );
    (
        Node {
            id,
            label: item_name.to_string(),
            children: Tree(children),
        },
        next_id,
    )
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone)]
#[serde(tag = "type", rename_all = "snake_case")]
pub(crate) enum TreeInfo {
    Item(TreeInfoItem),
    Metric(TreeInfoMetric),
}

#[derive(Serialize, JsonSchema, Clone)]
pub(crate) struct TreeInfoItem {
    //pub(crate) hash: Sha256,
    pub(crate) name: QualifiedItemName,
    pub(crate) paths: Vec<String>,
    pub(crate) query: MetricSelector,
    pub(crate) keys: BTreeSet<LabelName>,
    pub(crate) items: BTreeSet<QualifiedItemName>,
    pub(crate) metrics: BTreeSet<MetricName>,
    pub(crate) collected: Vec<CollectedInfo>,
    pub(crate) errors: Vec<String>,
}

#[derive(Serialize, JsonSchema, Clone)]
pub(crate) struct TreeInfoMetric {
    pub(crate) name: MetricName,
    pub(crate) paths: Vec<String>,
    pub(crate) metric: SchemaMetric,
    pub(crate) collected: Vec<CollectedInfo>,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone)]
pub(crate) struct Modules(BTreeMap<ModuleName, ModuleVersion>);

#[derive(Serialize, JsonSchema, ApiComponent, Clone)]
pub(crate) struct ModInfo {
    requires: BTreeMap<ModuleName, ModuleVersionReq>,
    items: Vec<ItemName>,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone)]
pub(crate) struct ItemInfo {
    paths: Vec<String>,
    query: MetricSelector,
    keys: BTreeSet<LabelName>,
    items: BTreeSet<QualifiedItemName>,
    metrics: BTreeSet<MetricName>,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone)]
pub(crate) struct CollectedInfo {
    pub(crate) elem: String,
    pub(crate) query: MetricSelector,
    pub(crate) keys: BTreeSet<LabelName>,
}

#[derive(Deserialize, JsonSchema, ApiComponent, Debug)]
pub(crate) struct TreePath {
    pub(crate) path: Vec<String>,
}

#[instrument]
#[api_operation(summary = "Get prometheus schema info")]
pub(crate) async fn get_tree_info(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<TreePath>,
) -> Result<Json<TreeInfo>> {
    let TreePath { path } = path.into_inner();
    let state = data.read_state(role.into_inner());
    let schema = state.get_prometheus_schema()?;
    Ok(Json(tree_info(schema, &path)?))
}

pub(crate) fn tree_info(schema: &Universe, path: &[String]) -> Result<TreeInfo> {
    let (collected, item) = collect_info(schema, path)?;
    let info = match item {
        Branch::Item(name, item) => TreeInfo::Item(TreeInfoItem {
            // hash: item.digest_sha256(0, (schema, name.module())),
            name: name.clone(),
            paths: item.paths(name, schema),
            query: item.query.clone(),
            keys: item.keys.clone(),
            items: item.items.keys().cloned().collect(),
            metrics: item.metrics.keys().cloned().collect(),
            errors: item
                .tree(schema, name)
                .tree
                .verify()
                .err()
                .into_iter()
                .flatten()
                .map(|e| e.to_string())
                .collect(),
            collected,
        }),
        Branch::Metric(name, metric) => TreeInfo::Metric(TreeInfoMetric {
            name: name.clone(),
            paths: metric.paths(name, schema),
            metric: metric.to_serial(),
            collected,
        }),
    };
    Ok(info)
}

#[instrument]
#[api_operation(summary = "Get prometheus module list")]
pub(crate) async fn get_modules(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
) -> Result<Json<Modules>> {
    let state = data.read_state(role.into_inner());
    let schema = state.get_prometheus_schema()?;
    let modules = schema
        .iter_modules()
        .map(|(name, module)| (name.clone(), module.version.clone()))
        .collect();
    Ok(Json(Modules(modules)))
}

#[instrument]
#[api_operation(summary = "Get prometheus module info")]
pub(crate) async fn get_mod_info(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<String>,
) -> Result<Json<ModInfo>> {
    let name = path
        .into_inner()
        .parse()
        .map_err(Error::InvalidPromModuleName)?;
    let state = data.read_state(role.into_inner());
    let schema = state.get_prometheus_schema()?;
    let module = schema
        .lookup_module(&name)
        .ok_or_else(|| Error::MissingPromModule(name.clone()))?;
    Ok(Json(ModInfo {
        requires: module
            .requires
            .iter()
            .map(|(name, (_, req))| (name.clone(), req.clone()))
            .collect(),
        items: module.items.keys().cloned().collect(),
    }))
}

#[instrument]
#[api_operation(summary = "Get prometheus item info")]
pub(crate) async fn get_item_info(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<String>,
) -> Result<Json<ItemInfo>> {
    let name = path
        .into_inner()
        .parse()
        .map_err(Error::InvalidQualifiedItemName)?;
    let state = data.read_state(role.into_inner());
    let schema = state.get_prometheus_schema()?;
    let item = schema
        .lookup_item(&name)
        .ok_or_else(|| Error::MissingPromItem(name.clone()))?;
    Ok(Json(ItemInfo {
        paths: item.paths(&name, schema),
        query: item.query.clone(),
        keys: item.keys.clone(),
        items: item.items.keys().cloned().collect(),
        metrics: item.metrics.keys().cloned().collect(),
    }))
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone)]
pub(crate) struct PromItemList(BTreeMap<ModuleName, BTreeSet<ItemName>>);

#[instrument]
#[api_operation(summary = "Get prometheus item list")]
pub(crate) async fn get_prom_items(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
) -> Result<Json<PromItemList>> {
    let state = data.read_state(role.into_inner());
    let schema = state.get_prometheus_schema()?;
    Ok(Json(PromItemList(
        schema
            .iter_modules()
            .map(|(mod_name, module)| (mod_name.clone(), module.items.keys().cloned().collect()))
            .collect(),
    )))
}

// fn paths_tree(name: &QualifiedItemName, item: &Item, schema: &Universe) -> Option<Tree> {
//     fn build<'a, S: IntoIterator<Item = T>, T: Iterator<Item = &'a str>, F: FnMut() -> String>(
//         paths: S,
//         id: &mut F,
//     ) -> Tree {
//         let paths = paths
//             .into_iter()
//             .fold(BTreeMap::new(), |mut map, mut path| {
//                 if let Some(elem) = path.next() {
//                     map.entry(elem).or_insert_with(Vec::new).push(path);
//                 }
//                 map
//             });

//         let nodes = paths
//             .into_iter()
//             .map(|(elem, paths)| Node {
//                 id: id(),
//                 label: elem.to_string(),
//                 children: build(paths, id),
//             })
//             .collect();
//         Tree(nodes)
//     }

//     let paths = item.paths(name, schema);

//     let mut id = 0u64;
//     let mut id = move || {
//         let s = id.to_string();
//         id += 1;
//         s
//     };

//     if paths.is_empty() {
//         None
//     } else {
//         Some(Tree(vec![Node {
//             id: id(),
//             label: String::from("root"),
//             children: build(paths.iter().map(|path| path.split('/').skip(1)), &mut id),
//         }]))
//     }
// }

#[derive(Serialize, JsonSchema, ApiComponent, Clone)]
pub(crate) struct Items {
    path: BTreeMap<String, ItemList>,
    cols: Vec<(String, Vec<LabelName>)>,
    rows: Vec<Series>,
}

#[derive(Deserialize, JsonSchema, ApiComponent, Clone, Debug)]
pub(crate) struct ItemsParams {
    #[serde(default)]
    filters: BTreeMap<String, GenericLabels>,
}

#[derive(Serialize, JsonSchema, Clone)]
struct ItemList {
    keys: BTreeSet<LabelName>,
    items: Vec<Series>,
}

#[derive(Serialize, JsonSchema, Clone)]
struct Series {
    keys: GenericLabels,
    count: u64,
}

#[instrument]
#[api_operation(summary = "Get prometheus items")]
pub(crate) async fn post_tree_items(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<TreePath>,
    params: Json<ItemsParams>,
) -> Result<Json<Items>> {
    let TreePath { path } = path.into_inner();
    let info = {
        let state = data.read_state(role.into_inner());
        let schema = state.get_prometheus_schema()?;
        collect_info(schema, &path)?.0
    };
    let items = tree_items(role.into_inner(), &data, &info, &params).await?;
    Ok(Json(items))
}

pub(crate) async fn tree_items(
    role: ViewerRole,
    data: &AppData,
    info: &[CollectedInfo],
    params: &ItemsParams,
) -> Result<Items> {
    let query = info
        .iter()
        .fold(MetricSelector::new(), |q, item| q & &item.query);

    let query = params.filters.values().fold(query, |mut q, labels| {
        q.0.extend(
            labels
                .iter()
                .map(|(label, value)| (label.clone(), LabelSelector::Eq(value.clone()))),
        );
        q
    });

    let keys = info
        .iter()
        .flat_map(|item| item.keys.iter())
        .cloned()
        .collect::<BTreeSet<_>>();

    let metrics = if query.0.is_empty() {
        Vec::new()
    } else {
        crate::metrics::run_query::<_, _, Vec<GenericMetric<DataPoint2<Value>>>>(
            role,
            data,
            query.to_string(),
            InstantQueryParams { time: None },
        )
        .await?
    };

    let series_by = |keys: &BTreeSet<LabelName>| {
        metrics
            .iter()
            .fold(BTreeMap::new(), |mut map, m| {
                let key = m
                    .metric
                    .iter()
                    .filter(|(label, _)| keys.contains(*label))
                    .map(|(label, value)| (label.clone(), value.clone()))
                    .collect();
                *map.entry(key).or_insert(0) += 1;
                map
            })
            .into_iter()
            .map(|(keys, count)| Series { keys, count })
            .collect()
    };

    Ok(Items {
        path: info
            .iter()
            .filter(|item| !item.keys.is_empty())
            .map(|item| {
                (
                    item.elem.to_string(),
                    ItemList {
                        keys: item.keys.clone(),
                        items: series_by(&item.keys),
                    },
                )
            })
            .collect(),
        cols: info
            .iter()
            .map(|item| (item.elem.to_string(), item.keys.iter().cloned().collect()))
            .collect(),
        rows: series_by(&keys),
    })
}

#[derive(Deserialize, JsonSchema, ApiComponent, Debug)]
pub(crate) struct MetricsParams {
    item: GenericLabels,
}

#[derive(Serialize, JsonSchema, ApiComponent, Clone)]
pub(crate) struct MetricsData {
    cols: Vec<LabelName>,
    rows: Vec<MetricData>,
    errors: Vec<String>,
}

#[derive(Serialize, JsonSchema, Clone)]
struct MetricData {
    labels: GenericLabels,
    value: f64,
}

#[instrument]
#[api_operation(summary = "Get prometheus metrics")]
pub(crate) async fn post_tree_metrics(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<TreePath>,
    params: Json<MetricsParams>,
) -> Result<Json<MetricsData>> {
    let TreePath { path } = path.into_inner();
    let (info, tree) = {
        let state = data.read_state(role.into_inner());
        let schema = state.get_prometheus_schema()?;
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

pub(crate) async fn tree_metrics(
    role: ViewerRole,
    data: &AppData,
    info: Vec<CollectedInfo>,
    tree: prometheus_schema::tree::Tree,
    params: &MetricsParams,
) -> Result<MetricsData> {
    let mut query = info
        .iter()
        .fold(MetricSelector::new(), |q, item| q & &item.query);

    let keys = info
        .iter()
        .flat_map(|item| item.keys.iter())
        .collect::<BTreeSet<_>>();

    query.0.extend(keys.iter().copied().map(|key| {
        let selector = match params.item.get(key) {
            Some(value) => LabelSelector::Eq(value.clone()),
            None => LabelSelector::Unset,
        };
        (key.clone(), selector)
    }));

    let res = if query.0.is_empty() {
        Vec::new()
    } else {
        crate::metrics::run_query::<_, _, Vec<GenericMetric<DataPoint2<Value>>>>(
            role,
            data,
            query.to_string(),
            InstantQueryParams { time: None },
        )
        .await?
    };

    let (errors, _, _) = prometheus_schema::tree::Tree::Branch(BTreeMap::from_iter([(
        String::from("path"),
        prometheus_schema::tree::Branch {
            query,
            assert: MetricSelector::default(),
            keys: BTreeSet::new(),
            item: None,
            tree,
        },
    )]))
    .sieve(&res);

    let mut cols = BTreeSet::new();
    let rows = res
        .into_iter()
        .map(|m| MetricData {
            labels: m
                .metric
                .into_iter()
                .filter(|(label, _)| !keys.contains(label))
                .inspect(|(label, _)| {
                    cols.insert(label.clone());
                })
                .collect(),
            value: m.value.value.0,
        })
        .collect();

    Ok(MetricsData {
        cols: cols.into_iter().collect(),
        rows,
        errors,
    })
}

pub(crate) fn collect_info<'a>(
    schema: &'a Universe,
    path: &[String],
) -> Result<(Vec<CollectedInfo>, Branch<'a>)> {
    let elems = path
        .iter()
        .map(|s| s.as_str())
        .filter(|elem| !elem.is_empty()); //.map(urldescape);

    let mut last_branch = None;
    let info = schema
        .walk_from_root(elems)
        .map(|branch| {
            let branch = branch?;
            last_branch = Some(branch);
            match branch {
                Branch::Item(name, item) => Ok(CollectedInfo {
                    elem: name.to_string(),
                    query: item.query.clone(),
                    keys: item.keys.clone(),
                }),
                Branch::Metric(name, _metric) => {
                    let metric = LabelName::new_static("__name__");
                    Ok(CollectedInfo {
                        elem: name.to_string(),
                        query: std::iter::once((
                            metric.clone(),
                            LabelSelector::Eq(name.to_string()),
                        ))
                        .collect(),
                        keys: BTreeSet::new(),
                    })
                }
            }
        })
        .collect::<Result<Vec<_>>>()?;

    Ok((info, last_branch.unwrap()))
}
