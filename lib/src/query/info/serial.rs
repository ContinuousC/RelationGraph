/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use prometheus_core::LabelName;
use std::{collections::BTreeMap, fmt::Display, str::FromStr};

use chrono::{DateTime, Utc};
use ordered_float::OrderedFloat;
use prometheus_api::{DataPoint2, GenericMetric, Value};
use serde::{Deserialize, Serialize};

use prometheus_expr::{Expr, ExprSpec, ParamName, ParamValue};
use prometheus_schema::{MetricSelector, QualifiedItemName, Universe};

use crate::{
    types::connections::{ItemKeySelector, RelationKeySelector},
    ItemId, RelationId, Types,
};

use super::patterns::{QueryItemPattern, QueryRelationPattern};

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct InfoQuery {
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub items: BTreeMap<QueryItemPattern, QueryItemInfo>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub relations: BTreeMap<QueryRelationPattern, QueryRelationInfo>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi))]
pub struct InfoQueryParams {
    pub timestamp: DateTime<Utc>,
    pub from: DateTime<Utc>,
    pub to: DateTime<Utc>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct QueryItemInfo {
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub(super) metrics: BTreeMap<ExprName, QueryPromInfo<ItemKeySelector>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct QueryRelationInfo {
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub(super) metrics: BTreeMap<ExprName, QueryPromInfo<RelationKeySelector>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct QueryPromInfo<T> {
    pub(super) item: QualifiedItemName,
    pub(super) expr: ExprSpec,
    pub(super) labels: BTreeMap<LabelName, T>,
}

#[derive(Serialize, Deserialize, PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ExprName(String);

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct InfoQueryResult {
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub(super) items: BTreeMap<ItemId, InfoQueryItemResult>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub(super) relations: BTreeMap<RelationId, InfoQueryRelationResult>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct InfoQueryItemResult {
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub(super) metrics: BTreeMap<ExprName, Result<GenericMetric<DataPoint2<Value>>, String>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct InfoQueryRelationResult {
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub(super) metrics: BTreeMap<ExprName, Result<GenericMetric<DataPoint2<Value>>, String>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct InfoQueryMetrics(pub BTreeMap<ExprName, InfoQueryMetricResult>);

pub type InfoQueryMetricResult = std::result::Result<Vec<GenericMetric<DataPoint2<Value>>>, String>;

impl InfoQuery {
    pub fn resolve(self, types: &Types) -> crate::Result<super::resolved::InfoQuery> {
        super::resolved::InfoQuery::from_serial(self, types)
    }
}

impl<T> QueryPromInfo<T> {
    pub(super) fn resolve(
        &self,
        schema: &Universe,
        params: &BTreeMap<ParamName, ParamValue>,
        select: &MetricSelector,
    ) -> Result<Expr, ExprError> {
        let item = schema
            .lookup_item(&self.item)
            .ok_or_else(|| ExprError::MissingItem(self.item.clone()))?;
        item.svc_queries(&self.item, schema)
            .into_values()
            .try_fold::<Option<Expr>, _, _>(None, |a, mut source| {
                source &= select;
                let b = self
                    .expr
                    .with_params(&self.item, item, &source, params)
                    .map_err(ExprError::Resolve)?;
                Ok(Some(match a {
                    Some(a) => a.or(b),
                    None => b,
                }))
            })?
            .ok_or_else(|| ExprError::MissingSource(self.item.clone()))
    }
}

impl InfoQueryParams {
    pub(super) fn prom_params(&self) -> BTreeMap<ParamName, ParamValue> {
        fn param(name: &str, value: DateTime<Utc>) -> (ParamName, ParamValue) {
            (
                ParamName::from_str(name).unwrap(),
                ParamValue::Float(OrderedFloat(
                    value.timestamp() as f64 + value.timestamp_subsec_nanos() as f64 / 1e9,
                )),
            )
        }
        BTreeMap::from_iter([
            param("timestamp", self.timestamp),
            param("from", self.from),
            param("to", self.to),
        ])
    }
}

impl InfoQueryResult {
    pub fn get_item_metric(&self, expr_name: &ExprName, item_id: &ItemId) -> Option<f64> {
        let res = self.items.get(item_id)?.metrics.get(expr_name)?;
        let metric = res.as_ref().ok()?.value.value.0;
        Some(metric)
    }

    pub fn get_relation_metric(&self, expr_name: &ExprName, rel_id: &RelationId) -> Option<f64> {
        let res = self.relations.get(rel_id)?.metrics.get(expr_name)?;
        let metric = res.as_ref().ok()?.value.value.0;
        Some(metric)
    }

    pub fn item_metrics<'a>(&'a self, expr_name: &'a ExprName) -> impl Iterator<Item = f64> + 'a {
        self.items
            .values()
            .filter_map(|info| info.metrics.get(expr_name))
            .filter_map(|res| res.as_ref().ok())
            .map(|metric| metric.value.value.0)
    }

    pub fn relation_metrics<'a>(
        &'a self,
        expr_name: &'a ExprName,
    ) -> impl Iterator<Item = f64> + 'a {
        self.relations
            .values()
            .filter_map(|info| info.metrics.get(expr_name))
            .filter_map(|res| res.as_ref().ok())
            .map(|metric| metric.value.value.0)
    }
}

impl Display for ExprName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ExprError {
    #[error("missing prometheus item: {0}")]
    MissingItem(QualifiedItemName),
    #[error("no source for item: {0}")]
    MissingSource(QualifiedItemName),
    #[error("failed to resolve expression: {0}")]
    Resolve(prometheus_expr::SpecResolveError),
}
