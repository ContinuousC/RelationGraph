/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
};

use actix_web::{
    web::{Data, Json, Path},
    HttpResponse,
};

use apistos::{
    api_operation,
    web::{get, post, Resource, Scope},
};
use futures::{StreamExt, TryStreamExt};
use prometheus_api::{
    DataPoint2, DecodeQueryError, GenericMetric, GenericQueryResponse, InstantQueryParams,
    PromQueryParams, Query, QueryResult, RangeQueryParams, ScalarOr, Value,
};
use prometheus_core::LabelName;
use prometheus_expr::{Expr, ExprSpec, PromDuration, PromSelect};
use prometheus_schema::{LabelSelector, MetricSelector, QualifiedItemName};
use relation_graph::metrics::{Metric, Metrics, QueryMetric, SourceId};
use relation_graph::{
    alerts::{AlertRuleTemplateName, Severity},
    State,
};
use serde::{Deserialize, Serialize};
use tracing::instrument;

use crate::{
    auth::Auth,
    error::{Error, Result},
    roles::ViewerRole,
};
use crate::{http_response, AppData};

pub(crate) fn service() -> Scope {
    Scope::new("/metrics")
        .service(Resource::new("/sources/{promitem}").route(get().to(get_sources)))
        .service(Resource::new("/range").route(post().to(post_range_metrics)))
        .service(Resource::new("/bulk/range").route(post().to(post_range_metrics_bulk)))
        .service(Resource::new("/instant").route(post().to(post_instant_metrics)))
        .service(Resource::new("/bulk/instant").route(post().to(post_instant_metrics_bulk)))
        .service(Resource::new("/range_raw").route(post().to(post_range_query)))
}

#[derive(Serialize, Deserialize, Clone, Debug, schemars::JsonSchema, apistos::ApiComponent)]
//#[apistos(bound = "P: schemars::JsonSchema")]
pub struct QueryParams<P: schemars::JsonSchema> {
    #[serde(flatten)]
    metric: QueryMetric,
    source: Option<SourceId>,
    item_query: Option<BTreeMap<LabelName, String>>,
    item_keys: Option<BTreeSet<LabelName>>,
    #[serde(flatten)]
    params: P,
}

#[derive(Serialize, Deserialize, Clone, Debug, schemars::JsonSchema, apistos::ApiComponent)]
pub struct BulkQueryParams<P: schemars::JsonSchema> {
    #[serde(flatten)]
    params: P,
    metrics: BTreeMap<String, QueryMetric>,
    #[serde(default)]
    sources: BTreeMap<QualifiedItemName, SourceId>,
    item_queries: Option<BTreeMap<QualifiedItemName, BTreeMap<LabelName, String>>>,
    item_keys: Option<BTreeMap<QualifiedItemName, BTreeSet<LabelName>>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, schemars::JsonSchema, apistos::ApiComponent)]
//#[apistos(bound = "P: schemars::JsonSchema")]
pub struct QueryRawParams<P: schemars::JsonSchema> {
    query: String,
    #[serde(flatten)]
    params: P,
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
struct ItemName(#[schemars(with = "String")] QualifiedItemName);

// #[get("/sources/{promitem}")]
#[instrument]
#[api_operation(summary = "Get sources for a prometheus item")]
async fn get_sources(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    path: Path<ItemName>,
) -> HttpResponse {
    http_response(run_get_sources(
        role.into_inner(),
        &data,
        &path.into_inner().0,
    ))
}

fn run_get_sources(
    role: ViewerRole,
    data: &AppData,
    prom_item_name: &QualifiedItemName,
) -> Result<Vec<SourceId>> {
    let state = data.read_state(role);
    let schema = state.get_prometheus_schema()?;
    let prom_item = schema
        .lookup_item(prom_item_name)
        .ok_or_else(|| Error::MissingPromItem(prom_item_name.clone()))?;
    Ok(prom_item
        .svc_queries(prom_item_name, schema)
        .into_keys()
        .map(SourceId::new)
        .collect())
}

// #[post("/range")]
#[instrument]
#[api_operation(summary = "Query metrics over a time range")]
async fn post_range_metrics(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<QueryParams<RangeQueryParams>>,
) -> HttpResponse {
    let params = params.into_inner();
    let params = QueryParams::<RangeQueryParams> {
        metric: params.metric,
        source: params.source,
        item_query: params.item_query,
        item_keys: params.item_keys,
        params: RangeQueryParams {
            start: params.params.start,
            end: params.params.end,
            step: params.params.step.round(),
        },
    };
    http_response(
        run_get_graph_metrics::<_, Vec<DataPoint2<Value>>>(role.into_inner(), &data, params).await,
    )
}

// #[post("/bulk/range")]
#[instrument]
#[api_operation(summary = "Bulk query metrics over a time range")]
async fn post_range_metrics_bulk(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<BulkQueryParams<RangeQueryParams>>,
) -> HttpResponse {
    http_response(
        run_get_graph_metrics_bulk::<_, Vec<DataPoint2<Value>>>(
            role.into_inner(),
            &data,
            params.into_inner(),
        )
        .await,
    )
}

// #[post("/instant")]
#[instrument]
#[api_operation(summary = "Query metrics at a timestamp")]
async fn post_instant_metrics(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<QueryParams<InstantQueryParams>>,
) -> HttpResponse {
    http_response(
        run_get_graph_metrics::<_, DataPoint2<Value>>(
            role.into_inner(),
            &data,
            params.into_inner(),
        )
        .await,
    )
}

// #[post("/bulk/instant")]
#[instrument]
#[api_operation(summary = "Bulk query metrics at a timestamp")]
async fn post_instant_metrics_bulk(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<BulkQueryParams<InstantQueryParams>>,
) -> HttpResponse {
    http_response(
        run_get_graph_metrics_bulk::<_, DataPoint2<Value>>(
            role.into_inner(),
            &data,
            params.into_inner(),
        )
        .await,
    )
}

// #[post("/range_raw")]
#[instrument]
#[api_operation(summary = "Query query range")]
async fn post_range_query(
    role: Auth<ViewerRole>,
    data: Data<AppData>,
    params: Json<QueryRawParams<RangeQueryParams>>,
) -> HttpResponse {
    http_response(run_get_raw_query(role.into_inner(), &data, params.into_inner()).await)
}

async fn run_get_raw_query(
    role: ViewerRole,
    data: &AppData,
    params: QueryRawParams<RangeQueryParams>,
) -> Result<Vec<Metric<Vec<DataPoint2<Value>>>>> {
    let result = run_query::<_, _, Vec<GenericMetric<Vec<DataPoint2<Value>>>>>(
        role,
        data,
        params.query,
        params.params,
    )
    .await?;
    Ok(result.into_iter().map(|m| m.into()).collect())
}

async fn run_get_graph_metrics<P, R>(
    role: ViewerRole,
    data: &AppData,
    params: QueryParams<P>,
) -> Result<Metrics<R>>
where
    P: PromQueryParams + PromSelect + Clone + Debug + schemars::JsonSchema,
    Vec<GenericMetric<R>>: TryFrom<QueryResult, Error = DecodeQueryError>,
{
    let (metrics_query, threshold_queries) = {
        let state = data.read_state(role);
        let schema = state.get_prometheus_schema()?;

        let prom_item = schema
            .lookup_item(&params.metric.item)
            .ok_or_else(|| Error::MissingPromItem(params.metric.item.clone()))?;

        let mut selectors = MetricSelector::new();

        if let Some(item_query) = &params.item_query {
            let item_selector = item_query
                .iter()
                .map(|(label, value)| (label.clone(), LabelSelector::Eq(value.clone())))
                .collect::<MetricSelector>();
            selectors &= &item_selector;
        }

        if let Some(source) = &params.source {
            let source_queries = prom_item.svc_queries(&params.metric.item, schema);
            selectors &= source_queries
                .get(&source.to_string())
                .ok_or_else(|| Error::MissingSource(source.clone()))?;
        }

        let metrics_query = params
            .metric
            .expr
            .with_params(&params.metric.item, prom_item, &selectors, &BTreeMap::new())
            .map_err(Error::ResolveExprSpec)?;

        let threshold_queries = if let Some(item_labels) = &params.item_query {
            params
                .metric
                .thresholds
                .iter()
                .map(|(rule_id, expr)| {
                    let queries = get_threshold_queries(
                        &state,
                        &params.metric.item,
                        prom_item,
                        rule_id,
                        &selectors,
                        item_labels,
                        expr,
                    );
                    (rule_id, queries)
                })
                .collect::<BTreeMap<_, _>>()
        } else {
            // Would take too many queries. Need to write
            // pre-calculated thresholds to prometheus if we want
            // this.
            BTreeMap::new()
        };

        (metrics_query, threshold_queries)
    };

    let metrics_query = if let Some(keys) = params.item_keys.as_ref() {
        metrics_query.avg_by(keys.iter().cloned().collect())
    } else {
        metrics_query
    };

    let (query, query_min, query_max) = if let Some(select) = &params.metric.select {
        (params.params.select(select, metrics_query), None, None)
    } else if let Some(step) = params.params.get_step() {
        let range = PromDuration::zero()
            .with_milliseconds((step * 1000.0) as u64)
            .unwrap_or(PromDuration::MAX);
        let resolution = PromDuration::zero()
            .with_milliseconds((step.min(60.0) * 1000.0) as u64)
            .unwrap_or(PromDuration::MAX);
        let base_query = metrics_query
            .sum()
            .queried_over_range(range, Some(resolution));

        let query = base_query.clone().avg_over_time();
        let query_min = base_query.clone().min_over_time();
        let query_max = base_query.max_over_time();
        (query, Some(query_min), Some(query_max))
    } else {
        (metrics_query, None, None)
    };

    Ok(Metrics {
        series: run_query::<_, _, Vec<GenericMetric<R>>>(
            role,
            data,
            query.to_string(),
            params.params.clone(),
        )
        .await?
        .into_iter()
        .map(|m| m.into())
        .collect(),
        series_min: if let Some(query) = query_min {
            Some(
                run_query::<_, _, Vec<GenericMetric<R>>>(role, data, query, params.params.clone())
                    .await?
                    .into_iter()
                    .map(|m| m.into())
                    .collect(),
            )
        } else {
            None
        },
        series_max: if let Some(query) = query_max {
            Some(
                run_query::<_, _, Vec<GenericMetric<R>>>(role, data, query, params.params.clone())
                    .await?
                    .into_iter()
                    .map(|m| m.into())
                    .collect(),
            )
        } else {
            None
        },
        thresholds: futures::stream::iter(threshold_queries)
            .then(|(rule_id, queries)| {
                let params = params.params.clone();
                async move {
                    let results = match queries {
                        Ok(queries) => {
                            futures::stream::iter(queries)
                                .then(|(severity, query)| {
                                    let params = params.clone();
                                    async move {
                                        let result = run_query::<
                                            _,
                                            _,
                                            ScalarOr<Vec<GenericMetric<R>>>,
                                        >(
                                            role, data, query.to_string(), params
                                        )
                                        .await
                                        .map_err(|e| e.to_string())?;
                                        let result = match result {
                                            ScalarOr::Scalar(s) => ScalarOr::Scalar(s),
                                            ScalarOr::Series(ms) => ScalarOr::Series(
                                                ms.into_iter().map(|m| m.into()).collect(),
                                            ),
                                        };
                                        Ok((severity, result))
                                    }
                                })
                                .try_collect::<BTreeMap<_, _>>()
                                .await
                        }
                        Err(e) => Err(e.to_string()),
                    };
                    (rule_id.clone(), results)
                }
            })
            .collect()
            .await,
    })
}

fn get_threshold_queries(
    state: &State,
    item_name: &QualifiedItemName,
    item: &prometheus_schema::Item,
    rule_id: &AlertRuleTemplateName,
    selectors: &MetricSelector,
    item_labels: &BTreeMap<LabelName, String>,
    expr: &ExprSpec,
) -> Result<BTreeMap<Severity, Expr>> {
    let rule_form = &state
        .alert_rules
        .get_rule_form(rule_id)
        .ok_or_else(|| Error::MissingAlertRule(rule_id.clone()))?
        .rule_form
        .value;
    (item_name == &rule_form.item())
        .then_some(())
        .ok_or_else(|| Error::PromItemMismatch(item_name.clone(), rule_form.item()))?;
    let queries = Severity::severities()
        .filter_map(|severity| {
            let params = rule_form.get_param_values(severity, item_labels)?;
            let query = expr
                .with_params(item_name, item, selectors, &params.0)
                .ok()?;
            Some((severity, query))
        })
        .collect();

    Ok(queries)
}

async fn run_get_graph_metrics_bulk<P, R>(
    role: ViewerRole,
    data: &AppData,
    params: BulkQueryParams<P>,
) -> Result<BTreeMap<String, Metrics<R>>>
where
    P: PromQueryParams + PromSelect + Clone + Debug + schemars::JsonSchema,
    Vec<GenericMetric<R>>: TryFrom<QueryResult, Error = DecodeQueryError>,
{
    let results = futures::stream::iter(params.metrics)
        .map(|(id, metric)| {
            let params = QueryParams {
                metric: metric.clone(),
                params: params.params.clone(),
                source: params.sources.get(&metric.item).cloned(),
                item_query: params
                    .item_queries
                    .as_ref()
                    .and_then(|queries| queries.get(&metric.item))
                    .cloned(),
                item_keys: params
                    .item_keys
                    .as_ref()
                    .and_then(|keys| keys.get(&metric.item))
                    .cloned(),
            };
            async move {
                let result = run_get_graph_metrics(role, data, params).await?;
                Result::Ok((id, result))
            }
        })
        .buffer_unordered(15)
        .try_collect::<BTreeMap<_, _>>()
        .await?;
    Ok(results)
}

pub(crate) async fn run_query<P, Q, R>(
    _role: ViewerRole,
    data: &AppData,
    query: Q,
    params: P,
) -> Result<R>
where
    P: PromQueryParams,
    Q: Serialize + Display,
    R: TryFrom<QueryResult, Error = DecodeQueryError>,
{
    let url = data
        .prom_url
        .join(&format!("api/prom/api/v1/{}", P::PATH))?;
    tracing::debug!(
        %query,
        url = url.as_str(),
        method = "POST",
        "Prometheus query"
    );
    let query = Query { query, params };
    let res = data
        .prom
        .post(url)
        .form(&query)
        .send()
        .await
        .map_err(Error::PromRequestMw)?
        .error_for_status()
        .map_err(Error::PromRequest)?
        .json::<GenericQueryResponse>()
        .await
        .map_err(Error::PromDecode)?
        .into_result()
        .map_err(Error::Prometheus)?;
    res.data.try_into().map_err(Error::PromUnexpectedResult)
}
