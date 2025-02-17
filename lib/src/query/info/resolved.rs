/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;

use prometheus_expr::Expr;
use prometheus_schema::{MetricSelector, Universe};
use wrapper::{Wrapped, Wrapper};

use crate::{
    query::resolved::QueryResultItems,
    serial,
    types::resolved::{ItemKeySelector, RelationKeySelector},
    InfoQueryMetrics, Items, Result, Types,
};

use super::{
    patterns::{
        QueryItemPattern, QueryItemPatternParams, QueryRelationPattern, QueryRelationPatternParams,
    },
    serial::{
        ExprName, InfoQueryItemResult, InfoQueryParams, InfoQueryRelationResult, InfoQueryResult,
        QueryPromInfo,
    },
};

pub struct InfoQuery {
    items: BTreeMap<QueryItemPattern, QueryItemInfo>,
    relations: BTreeMap<QueryRelationPattern, QueryRelationInfo>,
}

pub struct QueryItemInfo {
    metrics: BTreeMap<ExprName, QueryPromInfo<ItemKeySelector>>,
}

pub struct QueryRelationInfo {
    metrics: BTreeMap<ExprName, QueryPromInfo<RelationKeySelector>>,
}

impl InfoQuery {
    pub(super) fn from_serial(serial: super::serial::InfoQuery, types: &Types) -> Result<Self> {
        Ok(Self {
            items: serial
                .items
                .into_iter()
                .map(|(pattern, info)| {
                    Result::Ok((pattern, QueryItemInfo::from_serial(info, types)?))
                })
                .collect::<Result<_>>()?,
            relations: serial
                .relations
                .into_iter()
                .map(|(pattern, info)| {
                    Result::Ok((pattern, QueryRelationInfo::from_serial(info, types)?))
                })
                .collect::<Result<_>>()?,
        })
    }

    pub fn exprs(
        &self,
        schema: &Universe,
        params: &InfoQueryParams,
        select: &MetricSelector,
    ) -> BTreeMap<ExprName, std::result::Result<Expr, String>> {
        let prom_params = params.prom_params();
        self.items
            .values()
            .flat_map(|item| &item.metrics)
            .map(|(expr_name, metrics)| {
                let result = metrics
                    .resolve(schema, &prom_params, select)
                    .map_err(|e| e.to_string());
                (expr_name.clone(), result)
            })
            .chain(self.relations.values().flat_map(|item| &item.metrics).map(
                |(expr_name, metrics)| {
                    let result = metrics
                        .resolve(schema, &prom_params, select)
                        .map_err(|e| e.to_string());
                    (expr_name.clone(), result)
                },
            ))
            .collect()
    }

    pub fn run<W: Wrapper>(
        &self,
        query_result: &QueryResultItems<W>,
        items: &Items<W>,
        types: &Types,
        metrics: &InfoQueryMetrics,
    ) -> InfoQueryResult {
        let item_series = self
            .items
            .iter()
            .map(|(pattern, info)| {
                let indexed = info
                    .metrics
                    .iter()
                    .filter_map(|(expr_name, info)| {
                        let index = metrics.0.get(expr_name)?.as_ref().map(|ms| {
                            ms.iter()
                                .map(|m| {
                                    let keys = info
                                        .labels
                                        .keys()
                                        .filter_map(|key| Some((key, m.metric.get(key)?)))
                                        .map(|(label, value)| (label.clone(), value.clone()))
                                        .collect::<BTreeMap<_, _>>();
                                    (keys, m)
                                })
                                .collect::<BTreeMap<_, _>>()
                        });
                        Some((expr_name, (info, index)))
                    })
                    .collect::<BTreeMap<_, _>>();
                (pattern.as_ref(), indexed)
            })
            .collect::<BTreeMap<_, _>>();

        let relation_series = self
            .relations
            .iter()
            .map(|(pattern, info)| {
                let indexed = info
                    .metrics
                    .iter()
                    .filter_map(|(expr_name, info)| {
                        let index = metrics.0.get(expr_name)?.as_ref().map(|ms| {
                            ms.iter()
                                .map(|m| {
                                    let keys = info
                                        .labels
                                        .keys()
                                        .filter_map(|key| Some((key, m.metric.get(key)?)))
                                        .map(|(label, value)| (label.clone(), value.clone()))
                                        .collect::<BTreeMap<_, _>>();
                                    (keys, m)
                                })
                                .collect::<BTreeMap<_, _>>()
                        });
                        Some((expr_name, (info, index)))
                    })
                    .collect::<BTreeMap<_, _>>();
                (pattern.as_ref(), indexed)
            })
            .collect::<BTreeMap<_, _>>();

        let item_metrics = query_result
            .items
            .iter(&items.items)
            .map(|(item_id, item)| {
                let item = item.get_wrapped();
                let metrics = QueryItemPatternParams {
                    item_type: item.item_type.key(),
                }
                .iter_patterns()
                .filter_map(|pattern| item_series.get(&pattern))
                .flat_map(|metrics| {
                    metrics.iter().filter_map(|(expr_name, (info, result))| {
                        let series = result
                            .as_ref()
                            .map(|index| {
                                let keys = info
                                    .labels
                                    .iter()
                                    .filter_map(|(label, selector)| {
                                        Some((label.clone(), selector.value(item, items, types)?))
                                    })
                                    .collect();
                                index.get(&keys)
                            })
                            .transpose()?
                            .copied()
                            .cloned()
                            .map_err(|e| (*e).clone());
                        Some(((*expr_name).clone(), series))
                    })
                })
                .collect();
                (item_id.clone(), InfoQueryItemResult { metrics })
            })
            .collect();

        let relation_metrics = query_result
            .relations
            .iter()
            .map(|(rel_id, (rel_ref, _invert))| {
                let rel = items.relations.borrow(rel_ref).get_wrapped();
                let metrics = QueryRelationPatternParams {
                    relation_type: rel.relation_type.key(),
                    source: items
                        .items
                        .borrow(&rel.source)
                        .get_wrapped()
                        .item_type
                        .key(),
                    target: items
                        .items
                        .borrow(&rel.target)
                        .get_wrapped()
                        .item_type
                        .key(),
                }
                .iter_patterns()
                .filter_map(|pattern| relation_series.get(&pattern))
                .flat_map(|metrics| {
                    metrics.iter().filter_map(|(expr_name, (info, result))| {
                        let series = result
                            .as_ref()
                            .map(|index| {
                                let keys = info
                                    .labels
                                    .iter()
                                    .filter_map(|(label, sel)| {
                                        let value = sel.value(rel, None, items, types)?;
                                        Some((label.clone(), value))
                                    })
                                    .collect();
                                index.get(&keys)
                            })
                            .transpose()?
                            .copied()
                            .cloned()
                            .map_err(|e| (*e).clone());
                        Some(((*expr_name).clone(), series))
                    })
                })
                .collect();
                (rel_id.clone(), InfoQueryRelationResult { metrics })
            })
            .collect();

        InfoQueryResult {
            items: item_metrics,
            relations: relation_metrics,
        }
    }
}

impl QueryItemInfo {
    fn from_serial(serial: super::serial::QueryItemInfo, types: &Types) -> Result<Self> {
        Ok(Self {
            metrics: serial
                .metrics
                .into_iter()
                .map(|(expr_name, info)| {
                    Ok((
                        expr_name,
                        QueryPromInfo::<ItemKeySelector>::from_serial(info, types)?,
                    ))
                })
                .collect::<Result<_>>()?,
        })
    }
}

impl QueryRelationInfo {
    fn from_serial(serial: super::serial::QueryRelationInfo, types: &Types) -> Result<Self> {
        Ok(Self {
            metrics: serial
                .metrics
                .into_iter()
                .map(|(expr_name, info)| {
                    Ok((
                        expr_name,
                        QueryPromInfo::<RelationKeySelector>::from_serial(info, types)?,
                    ))
                })
                .collect::<Result<_>>()?,
        })
    }
}

impl QueryPromInfo<ItemKeySelector> {
    fn from_serial(serial: QueryPromInfo<serial::ItemKeySelector>, types: &Types) -> Result<Self> {
        Ok(Self {
            item: serial.item,
            expr: serial.expr,
            labels: serial
                .labels
                .into_iter()
                .map(|(label, selector)| Ok((label, selector.types(None, types)?)))
                .collect::<Result<_>>()?,
        })
    }
}

impl QueryPromInfo<RelationKeySelector> {
    fn from_serial(
        serial: QueryPromInfo<serial::RelationKeySelector>,
        types: &Types,
    ) -> Result<Self> {
        Ok(Self {
            item: serial.item,
            expr: serial.expr,
            labels: serial
                .labels
                .into_iter()
                .map(|(label, selector)| Result::Ok((label, selector.types(None, types)?)))
                .collect::<Result<_>>()?,
        })
    }
}
