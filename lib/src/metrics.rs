/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, fmt::Display};

use serde::{Deserialize, Serialize};

use prometheus_api::ScalarOr;
use prometheus_core::{LabelName, MetricName, METRIC_LABEL};
use prometheus_expr::{ExprSpec, SelectItem};
use prometheus_schema::{LabelSelector, MetricSelector, QualifiedItemName};

use crate::{
    alerts::{AlertRuleTemplateName, Severity},
    Absolute, Error, ItemId, ItemTypeId, RelationId, RelationTypeId, Result, State,
};

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Metrics<R> {
    pub series: Vec<Metric<R>>,
    pub series_min: Option<Vec<Metric<R>>>,
    pub series_max: Option<Vec<Metric<R>>>,
    pub thresholds: ThresholdsByRule<ScalarOr<Vec<Metric<R>>>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct Metric<T> {
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: LabelName]: string }"))]
    pub metric: BTreeMap<LabelName, String>,
    pub value: T,
}

pub type ThresholdsByRule<R> =
    BTreeMap<AlertRuleTemplateName, std::result::Result<ThresholdsBySeverity<R>, String>>;
pub type ThresholdsBySeverity<R> = BTreeMap<Severity, R>;

impl<T> From<prometheus_api::GenericMetric<T>> for Metric<T> {
    fn from(metric: prometheus_api::GenericMetric<T>) -> Self {
        Metric {
            metric: metric.metric,
            value: metric.value,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct QueryMetric {
    pub item: QualifiedItemName,
    pub expr: ExprSpec,
    #[serde(default)]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: string }"))]
    pub thresholds: BTreeMap<AlertRuleTemplateName, ExprSpec>,
    pub select: Option<SelectItem>,
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(
        from_wasm_abi,
        into_wasm_abi,
        type = "{ [key: string]: TypeSourceQueries<Id> }"
    )
)]
pub struct TypeQueries<Id: Ord>(pub BTreeMap<QualifiedItemName, TypeSourceQueries<Id>>);

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi,))]
pub struct TypeSourceQueries<Id: Ord> {
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: MetricSelector }"))]
    pub items: BTreeMap<Id, MetricSelector>,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: MetricSelector }"))]
    pub instances: BTreeMap<String, MetricSelector>,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: MetricSelector }"))]
    pub metrics: BTreeMap<MetricName, MetricSelector>,
}

pub fn item_type_queries(
    state: &State,
    item_type_id: &Absolute<ItemTypeId>,
) -> Result<TypeQueries<ItemId>> {
    let schema = &state
        .prometheus
        .as_ref()
        .ok_or(Error::MissingPrometheusSchema)?
        .resolved;
    let item_type = state
        .types
        .items
        .get(item_type_id)
        .ok_or_else(|| Error::MissingItemType(item_type_id.clone()))?;
    Ok(TypeQueries(
        item_type
            .prometheus_items(&state.types, schema)
            .map(|(name, prom_item, metric)| {
                (
                    name.clone(),
                    TypeSourceQueries {
                        items: state
                            .items
                            .iter_items_by_type(item_type_id)
                            .map(|(item_id, item)| {
                                (
                                    item_id.clone(),
                                    metric.prometheus_query(item, &state.items.items, &state.types),
                                )
                            })
                            .collect(),
                        instances: prom_item.svc_queries(name, schema),
                        metrics: prom_item.metric_queries(schema),
                    },
                )
            })
            .collect(),
    ))
}

pub fn relation_type_queries(
    state: &State,
    relation_type_id: &Absolute<RelationTypeId>,
) -> Result<TypeQueries<RelationId>> {
    let schema = &state
        .prometheus
        .as_ref()
        .ok_or(Error::MissingPrometheusSchema)?
        .resolved;
    let relation_type = state
        .types
        .relations
        .get(relation_type_id)
        .ok_or_else(|| Error::MissingRelationType(relation_type_id.clone()))?;
    Ok(TypeQueries(
        relation_type
            .prometheus_items(schema)
            .map(|(name, prom_item, metric)| {
                (
                    name.clone(),
                    TypeSourceQueries {
                        items: state
                            .items
                            .iter_relations()
                            .filter(|(_, rel)| rel.relation_type_id() == relation_type_id)
                            .map(|(rel_id, rel)| {
                                (
                                    rel_id.clone(),
                                    metric.prometheus_query(rel, &state.items.items, &state.types),
                                )
                            })
                            .collect(),
                        instances: prom_item.svc_queries(name, schema),
                        metrics: prom_item.metric_queries(schema),
                    },
                )
            })
            .collect(),
    ))
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(
        from_wasm_abi,
        into_wasm_abi,
        type = "{ [key: string]: ItemSourceQueries }"
    )
)]
pub struct ItemQueries(pub BTreeMap<QualifiedItemName, ItemSourceQueries>);

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ItemSourceQueries {
    pub item: MetricSelector,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: MetricSelector }"))]
    pub instances: BTreeMap<String, MetricSelector>,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: MetricSelector }"))]
    pub metrics: BTreeMap<MetricName, MetricSelector>,
}

pub fn item_queries(state: &State, item_id: &ItemId) -> Result<ItemQueries> {
    let schema = state.get_prometheus_schema()?;
    let item = state
        .items
        .get_item(item_id)
        .ok_or_else(|| Error::MissingItem(item_id.clone()))?;
    let item_type = state.types.items.borrow(&item.item_type);
    Ok(ItemQueries(
        item_type
            .prometheus_items(&state.types, schema)
            .map(|(name, prom_item, metrics)| {
                (
                    name.clone(),
                    ItemSourceQueries {
                        item: metrics.prometheus_query(item, &state.items.items, &state.types),
                        instances: prom_item.svc_queries(name, schema),
                        metrics: prom_item.metric_queries(schema),
                    },
                )
            })
            .collect(),
    ))
}

pub fn relation_queries(state: &State, rel_id: &RelationId) -> Result<ItemQueries> {
    let schema = state.get_prometheus_schema()?;
    let relation = state
        .items
        .get_relation(rel_id)
        .ok_or_else(|| Error::MissingRelation(rel_id.clone()))?;
    let relation_type = state
        .types
        .relations
        .get(relation.relation_type_id())
        .ok_or_else(|| Error::MissingRelationType(relation.relation_type_id().clone()))?;
    Ok(ItemQueries(
        relation_type
            .prometheus_items(schema)
            .map(|(name, prom_item, metrics)| {
                (
                    name.clone(),
                    ItemSourceQueries {
                        item: metrics.prometheus_query(relation, &state.items.items, &state.types),
                        instances: prom_item.svc_queries(name, schema),
                        metrics: prom_item.metric_queries(schema),
                    },
                )
            })
            .collect(),
    ))
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ClassifiedItemMetrics<T> {
    pub sources: MetricSources,
    pub metrics: ClassifiedMetrics<T>,
}

impl<T> Default for ClassifiedItemMetrics<T> {
    fn default() -> Self {
        Self {
            sources: Default::default(),
            metrics: Default::default(),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ClassifiedTypeMetrics<T> {
    pub sources: MetricSources,
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: string]: ClassifiedMetrics<T> }")
    )]
    pub metrics: BTreeMap<String, ClassifiedMetrics<T>>,
}

impl<T> Default for ClassifiedTypeMetrics<T> {
    fn default() -> Self {
        Self {
            sources: Default::default(),
            metrics: Default::default(),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(
        from_wasm_abi,
        into_wasm_abi,
        type = "{ [key: string]: ClassifiedMetricsForItem<T> }"
    )
)]
pub struct ClassifiedMetrics<T>(pub BTreeMap<QualifiedItemName, ClassifiedMetricsForItem<T>>);

impl<T> Default for ClassifiedMetrics<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

#[derive(Serialize, Deserialize, Default)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ClassifiedMetricsForItem<T> {
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: string }  "))]
    pub labels: BTreeMap<LabelName, String>,
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: string]: { [key: string]: ClassifiedMetric<T> } }")
    )]
    pub metrics: BTreeMap<MetricName, BTreeMap<SourceId, ClassifiedMetric<T>>>,
}

#[derive(Serialize, Deserialize, Default, Clone)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(
        from_wasm_abi,
        into_wasm_abi,
        type = "{ [key: string]: { [key: string]: Instance } }"
    )
)]
pub struct MetricSources(pub BTreeMap<QualifiedItemName, BTreeMap<SourceId, Instance>>);

#[derive(Serialize, Deserialize, Clone)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Instance {
    pub path: String,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: string }"))]
    pub labels: BTreeMap<LabelName, String>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct SourceId(String);

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ClassifiedMetric<T> {
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: string }"))]
    pub labels: BTreeMap<LabelName, String>,
    pub value: T,
}

impl SourceId {
    pub fn new(path: String) -> Self {
        Self(path)
    }
}

impl Display for SourceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// pub fn classify_item_metrics<T: DecodeQueryResult>(
//     queries: &ItemQueries,
//     metrics: Vec<GenericMetric<T>>,
// ) -> ClassifiedItemMetrics<T::Output> {
//     metrics
//         .into_iter()
//         .fold(ClassifiedItemMetrics::default(), |mut m, metric| {
//             let Some(value) = metric.value.extract() else {
//                 return m;
//             };

//             let Some(metric_name) = metric.metric.get("__name__") else {
//                 return m;
//             };

//             let Some((name, item_labels, svc_path, svc_labels)) = queries
//                 .0
//                 .iter()
//                 .flat_map(|(name, qs)| qs.instances.iter().map(move |svc| (name, &qs.item, svc)))
//                 .find_map(|(name, item, (path, svc))| {
//                     Some((
//                         name,
//                         item.matched_labels(&metric.metric)?,
//                         path,
//                         svc.matched_labels(&metric.metric)?,
//                     ))
//                 })
//             else {
//                 return m;
//             };

//             let Some(labels) = queries
//                 .0
//                 .values()
//                 .filter_map(|qs| qs.metrics.get(metric_name.as_str()))
//                 .find_map(|ms| ms.matched_labels(&metric.metric))
//             else {
//                 return m;
//             };

//             let source_id = SourceId(svc_path.to_string());

//             m.sources.0.entry(name.clone()).or_default().insert(
//                 source_id.clone(),
//                 Instance {
//                     path: svc_path.clone(),
//                     labels: svc_labels,
//                 },
//             );

//             m.metrics
//                 .0
//                 .entry(name.clone())
//                 .or_insert_with(|| ClassifiedMetricsForItem {
//                     labels: item_labels,
//                     metrics: BTreeMap::new(),
//                 })
//                 .metrics
//                 .entry(MetricName::new(metric_name.clone()))
//                 .or_default()
//                 .insert(source_id, ClassifiedMetric { labels, value });

//             m
//         })
// }

// pub fn classify_type_metrics<T: DecodeQueryResult, Id: Ord + Eq + std::fmt::Display>(
//     queries: &TypeQueries<Id>,
//     metrics: Vec<GenericMetric<T>>,
// ) -> ClassifiedTypeMetrics<T::Output> {
//     metrics
//         .into_iter()
//         .fold(ClassifiedTypeMetrics::default(), |mut m, metric| {
//             let Some(value) = metric.value.extract() else {
//                 return m;
//             };

//             let Some(metric_name) = metric.metric.get("__name__") else {
//                 return m;
//             };

//             let Some((name, (item_id, item_labels), svc_path, svc_labels)) = queries
//                 .0
//                 .iter()
//                 .flat_map(|(name, qs)| qs.instances.iter().map(move |svc| (name, &qs.items, svc)))
//                 .find_map(|(name, items, (path, svc))| {
//                     Some((
//                         name,
//                         items.iter().find_map(|(id, item)| {
//                             Some((id, item.matched_labels(&metric.metric)?))
//                         })?,
//                         path,
//                         svc.matched_labels(&metric.metric)?,
//                     ))
//                 })
//             else {
//                 return m;
//             };

//             let Some(labels) = queries
//                 .0
//                 .values()
//                 .filter_map(|qs| qs.metrics.get(metric_name.as_str()))
//                 .find_map(|ms| ms.matched_labels(&metric.metric))
//             else {
//                 return m;
//             };

//             let source_id = SourceId(svc_path.clone());

//             m.sources.0.entry(name.clone()).or_default().insert(
//                 source_id.clone(),
//                 Instance {
//                     path: svc_path.clone(),
//                     labels: svc_labels,
//                 },
//             );

//             m.metrics
//                 .entry(item_id.to_string())
//                 .or_default()
//                 .0
//                 .entry(name.clone())
//                 .or_insert_with(|| ClassifiedMetricsForItem {
//                     labels: item_labels,
//                     metrics: BTreeMap::new(),
//                 })
//                 .metrics
//                 .entry(MetricName::new(metric_name.clone()))
//                 .or_default()
//                 .insert(source_id, ClassifiedMetric { labels, value });

//             m
//         })
// }

pub fn metric_query(name: &MetricName, labels: &MetricSelector) -> MetricSelector {
    std::iter::once((METRIC_LABEL.clone(), LabelSelector::Eq(name.to_string())))
        .chain(
            labels
                .iter()
                .map(|(name, selector)| (name.clone(), selector.clone())),
        )
        .collect()
}
