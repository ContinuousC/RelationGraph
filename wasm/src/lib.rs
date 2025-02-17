/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

#![allow(non_snake_case)]
use itertools::{Itertools, MinMaxResult};
use jaeger_anomaly_detection::{
    CombineScores, OperationFilter, OperationKey, OperationOrService, ServiceFilter, ServiceKey,
    TraceAggr, TraceExpr, TraceObject,
};
use std::{
    cell::OnceCell,
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Write,
    sync::Arc,
};
use unit::{Dimension, Unit};
use wasm_bindgen_futures::JsFuture;
use web_sys::{
    js_sys::{ArrayBuffer, Uint8Array},
    Blob, Request, RequestInit, Response,
};

use chrono::{DateTime, Utc};
use graph::{Ref, RefBy};
use log::Level;

use prometheus_api::{DataPoint2, RangeQueryParams, ScalarOr};
use prometheus_core::LabelName;
use prometheus_expr::{Expr, ExprSpec, ParamName, ParamType};
use prometheus_schema::{QualifiedItemName, Universe};
use relation_graph::{
    alerts::{AnnotationTemplate, Severity},
    metrics::Metrics,
    serial::{self, Elements, PropertyValue, StatusInfo},
    status::Status,
    Absolute, AnomalyTracesGraph, AnomalyTracesGraphType, Augment, Augmented, ConnectionsPackages,
    DashboardMetric, DashboardMetricId, Endpoint, ExprName, GraphColor, InfoQuery,
    InfoQueryMetrics, InfoQueryResult, Item, ItemId, ItemMetrics, ItemStyle, ItemType, ItemTypeId,
    ItemTypeName, Items, PackageData, PackageId, Packages, PredefinedColor, PropertyId,
    PropertyValueType, Query, QueryResult, QueryResultItems, RelationId, RelationStyle,
    RelationTypeId, Relative, Stylesheet, Table, TableColumn, Topology, TplVarId, Types,
};
use serde::{de::DeserializeOwned, ser::SerializeSeq, Deserialize, Serialize};
use wasm_bindgen::{prelude::wasm_bindgen, JsCast, JsError, JsValue};

#[wasm_bindgen(start)]
pub fn init() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
    console_log::init_with_level(Level::Debug).unwrap();
}

#[wasm_bindgen(typescript_custom_section)]
const TS_APPEND_CONTENT: &'static str = r#"
export interface SingleVersioned<T> {
   version: SingleVersionInfo;
   value: T;
}
export interface SingleVersionInfo {
   active: Anchor;
}
export interface Anchor {
   from: string;
   to: string | null;
   created: string;
}
export type ThresholdsByRule<R> = { [key: AlertRuleName]: { Ok: ThresholdsBySeverity<R> } | { Err: string } }
export type ThresholdsBySeverity<R> = { [key in Severity]: R }
export type OrderedFloat<T> = T;
export type DateTime<T> = string;
export type Utc = string;
export interface Bin {
    from: string;
    to: string;
    count: number;
};
export type ItemAugmented = Item & StatusInfo;
export type RelationAugmented = Relation & StatusInfo;
export interface ItemsAugmented {
  items?: { [key: ItemId]: ItemAugmented };
  relations?: { [key: RelationId]: RelationAugmented };
}
export type EventType = "added" | "modified" | "removed";
export type Event<T> = {
    timestamp: string;
    object_id: ItemId;
    change_type: EventType;
} & T;
export type AlertEvent = Event<AlertDoc>;
export type ChangeEvent = Event<EntityInfo>;
export type StatusEvent = Event<StatusDoc>;
export interface AggrStatusCounts {
    count: number;
    status: {[key in Status]: number};
    unknown: number;
    items: AggrItemInfo[]
};
export interface AggrItemInfo {
    id: ItemId;
    type: ItemTypeId;
    name: string[];
    status: Status;
    active: DateTime<Utc>
};
type ColumnOutputHashableTypes =
  | "binarystring"
  | "unicodestring"
  | "string"
  | "integer"
  | "float"
  | "boolean"
  | "time"
  | "age"
  | "macaddr"
  | "ipv4addr"
  | "ipv6addr";

export type ColumnOutputTypes = {props: ColumnOutputHashableTypes  | { quantity: Dimension } | { map: [ColumnOutputHashableTypes, ColumnOutputHashableTypes] } } | { metric: [Unit, Unit | undefined]};
"#;

#[wasm_bindgen]
pub struct JsTypes {
    types: Arc<Types>,
    // packages: Arc<Packages>,
    // connections: Arc<ConnectionsPackages>,
    // prom_schema: Arc<Universe>,
}

#[wasm_bindgen]
pub struct JsPromSchema {
    schema: Arc<Universe>,
}

#[wasm_bindgen]
pub struct JsExprSpec(ExprSpec);

#[wasm_bindgen]
pub struct JsAnnotationTemplate(AnnotationTemplate);

#[wasm_bindgen]
pub struct JsUnit(Unit);

#[wasm_bindgen]
pub struct JsDimension(Dimension);

#[wasm_bindgen]
pub struct JsAlertRuleTypes {
    value: Dimension,
    params: BTreeMap<ParamName, ParamType>,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct ParamTypeMap(BTreeMap<ParamName, ParamType>);

#[derive(Serialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct DimensionsInfo(Vec<DimensionInfo>);

#[derive(Serialize, tsify::Tsify)]
struct DimensionInfo {
    id: Dimension,
    name: &'static str,
    symbol: &'static str,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct Units(Vec<Unit>);

#[wasm_bindgen]
pub struct JsItemsData(serial::Items<Augment<StatusInfo>>);

#[wasm_bindgen]
pub struct JsItems {
    types: Arc<Types>,
    items: Arc<Items<Augment<StatusInfo>>>,
    metric_index: BTreeMap<QualifiedItemName, OnceCell<ItemMetricIndex>>,
}

#[wasm_bindgen]
pub struct JsQuery {
    types: Arc<Types>,
    query: Query,
}

#[wasm_bindgen]
pub struct JsQueryResult {
    types: Arc<Types>,
    items: Arc<Items<Augment<StatusInfo>>>,
    result: Arc<QueryResult<Augment<StatusInfo>>>,
}

#[wasm_bindgen]
pub struct JsQueryResultItems {
    types: Arc<Types>,
    items: Arc<Items<Augment<StatusInfo>>>,
    // Checked QueryResult: can be unwrapped without panicking
    result: Arc<QueryResult<Augment<StatusInfo>>>,
}

#[wasm_bindgen]
pub struct JsInfoQuery {
    types: Arc<Types>,
    query: InfoQuery,
}

#[wasm_bindgen]
pub struct JsStylesheet {
    styles: Stylesheet,
}

// Workaround for missing type parameters in wasmabi typescript decls.
#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct AbsItemTypeId(Absolute<ItemTypeId>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct AbsItemTypeIdVec(Vec<Absolute<ItemTypeId>>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct AbsItemTypeIds(BTreeSet<Absolute<ItemTypeId>>);

// Workaround for missing type parameters in wasmabi typescript decls.
#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct AbsRelationTypeId(Absolute<RelationTypeId>);

// Workaround for missing type parameters in wasmabi typescript decls.
#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct AbsPropertyId(Absolute<PropertyId>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct DateTimeUtc(DateTime<Utc>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi, type = "string[]")]
pub struct ItemTypeIds(BTreeSet<Absolute<ItemTypeId>>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct Ids {
    #[tsify(type = "ItemId[]")]
    items: BTreeSet<ItemId>,
    #[tsify(type = "RelationId[]")]
    relations: BTreeSet<RelationId>,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct ItemIds(#[tsify(type = "ItemId[]")] BTreeSet<ItemId>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi, into_wasm_abi, type = "{ [key: string]: string[] }")]
pub struct Filters(BTreeMap<TplVarId, serde_json::Value>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi, into_wasm_abi, type = "ItemRow[]")]
pub struct ItemRows(Vec<ItemRow>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
pub struct ItemRow {
    item_id: ItemId,
    #[serde(flatten)]
    item: serial::Item,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct ColumnDefs(Vec<ColumnDef>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub struct ColumnDef {
    header: String,
    title: Option<String>,
    r#type: ColumnDefType,
    enableHiding: Option<bool>,
    enableColumnDragging: Option<bool>,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub enum ColumnDefType {
    Value(ColumnDefValue),
    Columns(ColumnDefs),
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub struct ColumnDefValue {
    #[tsify(type = " \"item_id\" | \"item_type\" | \"props.${string}\" | \"metrics.${string}\"")]
    accessor_key: String,
    #[tsify(type = "ColumnOutputTypes")]
    r#type: ColumnDefValueType,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub enum ColumnDefValueType {
    Props(PropertyValueType),
    Metric(Unit, Option<Unit>),
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct TableData(Vec<RowData>);

#[derive(Serialize, Deserialize, tsify::Tsify, Debug)]
pub struct RowData {
    item_id: ItemId,
    item_name: String,
    item_type: Absolute<ItemTypeId>,
    status_info: StatusInfo,
    #[tsify(type = "{ [key: Absolute<PropertyId>]: any }")]
    props: HashMap<Absolute<PropertyId>, serde_json::Value>,
    #[tsify(type = "{ [key: string]: number | null }")]
    metrics: BTreeMap<DashboardMetricId, Option<f64>>,
}

#[derive(Serialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct GraphData {
    nodes: Vec<GraphNode>,
    edges: Vec<GraphEdge>,
}

#[derive(Serialize, tsify::Tsify)]
pub struct GraphNode {
    id: String,
    style: GraphNodeStyle,
    states: Vec<GraphNodeState>,
}

#[derive(Serialize, tsify::Tsify)]
pub struct GraphEdge {
    source: String,
    target: String,
    style: GraphEdgeStyle,
}

#[derive(Serialize, tsify::Tsify)]
#[serde(rename_all = "lowercase")]
pub enum GraphNodeState {
    Disabled,
}

#[derive(Serialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub struct GraphNodeStyle {
    #[serde(skip_serializing_if = "Option::is_none")]
    size: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    stroke: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    fill: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    fill_opacity: Option<f64>,
    label_text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    label_fill: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    label_word_wrap: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    label_font_size: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    halo: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    halo_stroke: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    halo_fill: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    icon: Option<bool>,
    icon_src: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    icon_width: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    icon_height: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    badge: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    badges: Option<Vec<GraphBadge>>,
}

#[derive(Serialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub struct GraphEdgeStyle {
    #[serde(skip_serializing_if = "Option::is_none")]
    stroke: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    line_width: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    label_stroke: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    label_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    label_background: Option<bool>,
}

#[derive(Serialize, Clone, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub struct GraphBadge {
    #[serde(skip_serializing_if = "Option::is_none")]
    placement: Option<GraphBadgePlacement>,
    text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    background: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    background_fill: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    background_width: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    background_height: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    fill: Option<GraphColor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    font_size: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    word_wrap: Option<bool>,
}

#[derive(Serialize, Clone, tsify::Tsify)]
#[serde(rename_all = "kebab-case")]
pub enum GraphBadgePlacement {
    LeftTop,
    LeftBottom,
    RightTop,
    RightBottom,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

#[derive(Serialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct GridItemData(Vec<ItemData>);

#[derive(Serialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub struct ItemData {
    item_id: ItemId,
    item_name: String,
    item_type: Absolute<ItemTypeId>,
    item_type_name: ItemTypeName,
    status_info: StatusInfo,
}

#[derive(Serialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct ItemExtendedData {
    data: ItemData,
    meta_data: ItemMetaData,
}

#[derive(Serialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub struct ItemMetaData {
    properties: ItemProperties,
}

#[derive(Serialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
pub struct ItemProperties {
    #[tsify(type = "{ [key: Absolute<PropertyId>]: any }")]
    data: HashMap<Absolute<PropertyId>, serde_json::Value>,
    definition: ColumnDefs,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi, type = "{ [key: DashboardMetricId]: DashboardMetric }")]
pub struct DashboardMetrics(BTreeMap<DashboardMetricId, DashboardMetric>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct RangeMetricData(Metrics<Vec<DataPoint2<prometheus_api::Value>>>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct InstantMetricData(Metrics<DataPoint2<prometheus_api::Value>>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(
    from_wasm_abi,
    type = "{ [key: DashboardMetricId]: Metrics<DataPoint2<Value>> }"
)]
pub struct TableMetricData(BTreeMap<DashboardMetricId, Metrics<DataPoint2<prometheus_api::Value>>>);

#[derive(tsify::Tsify)]
#[tsify(into_wasm_abi, type = "[string[], ...[string, ...number[]][]]")]
pub struct JsRangeMetrics {
    names: Vec<String>,
    rows: BTreeMap<DateTime<Utc>, Vec<Option<f64>>>,
}

struct JsRangeMetricHeaders<'a>(&'a [String]);
struct JsRangeMetricRow<'a>(&'a DateTime<Utc>, &'a [Option<f64>]);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsDataPoints(Vec<JsDataPoint>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
pub struct JsDataPoint {
    id: Option<String>,
    name: Option<String>,
    #[tsify(type = "string")]
    timestamp: DateTime<Utc>,
    value: f64,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi, type = "{ [key:string]: string }")]
pub struct ItemQuery(BTreeMap<LabelName, String>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi, type = "string[]")]
pub struct ItemKeys(BTreeSet<LabelName>);

type ItemMetricIndex = BTreeMap<BTreeMap<LabelName, String>, RefBy<ItemId, AugmentedItem>>;
type AugmentedItem = Augmented<Item<Augment<StatusInfo>>, StatusInfo>;

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[serde(rename_all = "camelCase")]
#[tsify(into_wasm_abi)]
pub enum AnomalyTracesExpression {
    Metric(AnomalyTracesExpressionOperation),
    MetricTop(Expr),
    Score(Expr),
    ScoreTop(Expr),
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi)]
pub struct AnomalyTracesExpressionOperation {
    mean: Expr,
    #[serde(skip_serializing_if = "Option::is_none")]
    mean_reference_interval: Option<Expr>,
    #[serde(skip_serializing_if = "Option::is_none")]
    confidence_interval: Option<Expr>,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(into_wasm_abi, type = "string[]")]
pub struct PromItemList(Vec<QualifiedItemName>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi, into_wasm_abi)]
pub struct RelativePropertyIds(Vec<Relative<PropertyId>>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi, into_wasm_abi)]
pub struct TableDefinitions {
    columns: ColumnDefs,
    column_visibility: ColumnVisibility,
    column_order: ColumnOrder,
}

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi, into_wasm_abi, type = "{[key: string]: boolean}")]
pub struct ColumnVisibility(BTreeMap<String, bool>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi, into_wasm_abi)]
pub struct ColumnOrder(Vec<String>);

#[derive(Serialize, Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct DashboardMetricIds(Vec<DashboardMetricId>);

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = "MRT_Cell")]
    type Cell;

    #[wasm_bindgen(method, js_name = "getValue")]
    fn get_value(this: &Cell) -> JsValue;
}

pub type WasmError = JsError;

// fn benchmark<F: FnMut() -> R, R>(name: &str, n: usize, mut f: F) -> R {
//     let window = web_sys::window().expect("should have a window in this context");
//     let performance = window
//         .performance()
//         .expect("performance should be available");

//     let mut sum = 0.0;
//     let mut sumsq = 0.0;
//     let mut r = None;

//     for _ in 0..n {
//         let start = performance.now();
//         let _ = r.insert(f());
//         let t = performance.now() - start;
//         sum += t;
//         sumsq += t * t;
//     }

//     let mean = sum / n as f64;
//     let sd = ((sumsq - sum * sum / n as f64) / (n - 1) as f64).powf(0.5);
//     let err = 2.0 * sd;
//     log::debug!("{name}: {mean:3.2}ms ± {err:3.2}ms");

//     r.unwrap()
// }

async fn fetch<T: DeserializeOwned>(
    name: &str,
    method: &str,
    url: &str,
    body: Option<&JsValue>,
) -> Result<T, FetchError> {
    let window = web_sys::window().expect("should have a window in this context");

    let opts = RequestInit::new();
    opts.set_method(method);
    if let Some(body) = body {
        opts.set_body(body);
    }

    let request = Request::new_with_str_and_init(url, &opts).map_err(FetchError::InitRequest)?;
    let headers = request.headers();
    if body.is_some() {
        headers
            .set("Content-Type", "application/json")
            .map_err(FetchError::SetHeader)?;
    }
    headers
        .set("Accept", "application/x-bitcode")
        .map_err(FetchError::SetHeader)?;

    let response = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(FetchError::Fetch)?;
    let response: Response = response.dyn_into().map_err(FetchError::Fetch)?;

    if response.status() != 200 {
        let msg = JsFuture::from(response.text().map_err(FetchError::RetrieveErrorMsg)?)
            .await
            .map_err(FetchError::RetrieveErrorMsg)?;
        return Err(FetchError::ErrorMsg(msg));
    }

    // let text = JsFuture::from(response.text()?).await?;
    let blob: Blob = JsFuture::from(response.blob().map_err(FetchError::RetrieveResponse)?)
        .await
        .map_err(FetchError::RetrieveResponse)?
        .dyn_into()
        .map_err(FetchError::RetrieveResponse)?;
    let buffer: ArrayBuffer = JsFuture::from(blob.array_buffer())
        .await
        .map_err(FetchError::RetrieveResponse)?
        .dyn_into()
        .map_err(FetchError::RetrieveResponse)?;

    // let text = text.as_string().expect("not a string");
    let bytes = Uint8Array::new(&buffer).to_vec();

    bitcode::deserialize(&bytes).map_err(|e| FetchError::Bitcode(name.to_string(), e))
}

#[derive(thiserror::Error, Debug)]
enum FetchError {
    #[error("failed to initialize request: {0:?}")]
    InitRequest(JsValue),
    #[error("failed to set request header: {0:?}")]
    SetHeader(JsValue),
    #[error("failed to deserialize {0}: {1}")]
    Bitcode(String, bitcode::Error),
    #[error("failed to retrieve error message")]
    RetrieveErrorMsg(JsValue),
    #[error("received error: {0:?}")]
    ErrorMsg(JsValue),
    #[error("failed to retrieve response")]
    RetrieveResponse(JsValue),
    #[error("failed to fetch: {0:?}")]
    Fetch(JsValue),
}

async fn get<T: DeserializeOwned>(name: &str, url: &str) -> Result<T, FetchError> {
    fetch(name, "GET", url, None).await
}

async fn post<S: Serialize, T: DeserializeOwned>(
    name: &str,
    url: &str,
    body: &S,
) -> Result<T, FetchError> {
    let body = serde_json::to_string(body).unwrap();
    fetch(name, "POST", url, Some(&JsValue::from(body))).await
}

#[wasm_bindgen]
impl JsTypes {
    #[wasm_bindgen(constructor)]
    pub fn new(
        pkgs: Packages,
        conns: Option<ConnectionsPackages>,
        prom_schema: Option<JsPromSchema>,
    ) -> Result<JsTypes, WasmError> {
        let mut types = pkgs.types()?;
        if let (Some(conns), Some(prom_schema)) = (conns.as_ref(), prom_schema.as_ref()) {
            types.resolve_connections(conns, &pkgs, &prom_schema.schema)?;
        }
        Ok(Self {
            types: Arc::new(types),
            // packages: Arc::new(pkgs),
            // connections: Arc::new(conns),
            // prom_schema: prom_schema.schema.clone(),
        })
    }

    pub async fn download() -> Result<Self, WasmError> {
        let pkgs: PackageData = get("packages", "/api/packages").await?;
        let schema = pkgs
            .prom_schema
            .map(|schema| JsPromSchema::new(schema.root, PromModules(schema.modules)))
            .transpose()?;
        Self::new(pkgs.packages, pkgs.connections, schema)
    }

    #[wasm_bindgen(js_name = "getTableDefs")]
    pub fn get_table_defs(
        &self,
        item_type: AbsItemTypeId,
        metrics_definitions: DashboardMetrics,
        table_definitions: Table,
    ) -> Result<TableDefinitions, JsValue> {
        let typ_ref = self
            .types
            .items
            .get_ref(&item_type.0)
            .ok_or_else(|| JsValue::from("item type not found"))?;
        let typ = self.types.items.borrow(typ_ref);
        let package_id = typ.package.key();
        let columns = vec![
            ColumnDef {
                header: String::from("Common"),
                title: None,
                r#type: ColumnDefType::Columns(ColumnDefs(vec![
                    ColumnDef {
                        header: String::from("Object Name"),
                        title: Some(String::from("The name of the item")),
                        r#type: ColumnDefType::Value(ColumnDefValue {
                            accessor_key: String::from("item_name"),
                            r#type: ColumnDefValueType::Props(PropertyValueType::String),
                        }),
                        enableHiding: Some(false),
                        enableColumnDragging: Some(false),
                    },
                    ColumnDef {
                        header: String::from("Managed Object Type"),
                        title: Some(String::from("The type of the item")),
                        r#type: ColumnDefType::Value(ColumnDefValue {
                            accessor_key: String::from("item_type"),
                            r#type: ColumnDefValueType::Props(PropertyValueType::String),
                        }),
                        enableHiding: Some(false),
                        enableColumnDragging: Some(false),
                    },
                ])),
                enableHiding: None,
                enableColumnDragging: None,
            },
            ColumnDef {
                header: String::from("Properties"),
                title: None,
                r#type: ColumnDefType::Columns(ColumnDefs(
                    typ.properties(&self.types)
                        .sorted_by(|property_a, property_b| {
                            property_a
                                .1
                                .name
                                .to_lowercase()
                                .cmp(&property_b.1.name.to_lowercase())
                        })
                        .map(|(property_id, property_def)| ColumnDef {
                            header: property_def.name.clone(),
                            title: property_def.description.clone(),
                            r#type: ColumnDefType::Value(ColumnDefValue {
                                accessor_key: format!("props.{property_id}"),
                                r#type: ColumnDefValueType::Props(property_def.value.clone()),
                            }),
                            enableHiding: None,
                            enableColumnDragging: None,
                        })
                        .collect(),
                )),
                enableHiding: None,
                enableColumnDragging: None,
            },
            ColumnDef {
                header: String::from("Metrics"),
                title: None,
                r#type: ColumnDefType::Columns(ColumnDefs(
                    metrics_definitions
                        .0
                        .iter()
                        .map(|(metric_id, metric_def)| ColumnDef {
                            header: metric_def.name.clone(),
                            title: metric_def.documentation.clone(),
                            r#type: ColumnDefType::Value(ColumnDefValue {
                                accessor_key: format!("metrics.{metric_id}"),
                                r#type: ColumnDefValueType::Metric(
                                    metric_def.unit,
                                    metric_def.display_unit,
                                ),
                            }),
                            enableHiding: None,
                            enableColumnDragging: None,
                        })
                        .collect(),
                )),
                enableHiding: None,
                enableColumnDragging: None,
            },
        ];
        let column_visibility = ColumnVisibility(
            table_definitions
                .column_visibility
                .map(|column_visibility| {
                    let (visible_property_columns, visible_metric_columns) =
                        column_visibility.iter().fold(
                            (BTreeSet::new(), BTreeSet::new()),
                            |(mut props, mut metrics), column| {
                                match column {
                                    TableColumn::Property(property_id) => {
                                        props.insert(property_id.clone().resolve(package_id));
                                    }
                                    TableColumn::Metric(metric) => {
                                        metrics.insert(metric);
                                    }
                                }
                                (props, metrics)
                            },
                        );
                    typ.properties(&self.types)
                        .map(|(property_id, _)| {
                            let visible = visible_property_columns.contains(property_id);
                            (format!("props.{property_id}"), visible)
                        })
                        .chain(metrics_definitions.0.keys().map(|metric_id| {
                            let visible = visible_metric_columns.contains(metric_id);
                            (format!("metrics.{metric_id}"), visible)
                        }))
                        .collect()
                })
                .unwrap_or_default(),
        );
        let column_order = ColumnOrder(
            table_definitions
                .column_order
                .map(|column_order| {
                    column_order
                        .iter()
                        .filter_map(|column| match column {
                            TableColumn::Property(property_id) => {
                                let property_id_abs = property_id.clone().resolve(package_id);
                                typ.has_property(&property_id_abs, &self.types)
                                    .then(|| format!("props.{property_id}"))
                            }
                            TableColumn::Metric(metric_id) => metrics_definitions
                                .0
                                .contains_key(metric_id)
                                .then(|| format!("metrics.{metric_id}")),
                        })
                        .collect()
                })
                .unwrap_or_default(),
        );
        Ok(TableDefinitions {
            columns: ColumnDefs(columns),
            column_visibility,
            column_order,
        })
    }

    #[wasm_bindgen(js_name = "getItemDefaultTopology")]
    pub fn get_default_item_topology(item_id: ItemId) -> Topology {
        Topology::item_default(item_id)
    }

    #[wasm_bindgen(js_name = "getPromItems")]
    pub fn get_prom_items(&self) -> PromItemList {
        PromItemList(
            self.types
                .items
                .values()
                .flat_map(|item| item.prometheus_metrics.keys())
                .cloned()
                .collect(),
        )
    }
}

#[derive(Deserialize, tsify::Tsify)]
#[tsify(from_wasm_abi)]
pub struct PromModules(BTreeMap<prometheus_schema::ModuleName, prometheus_schema::serial::Module>);

#[wasm_bindgen]
impl JsPromSchema {
    #[wasm_bindgen(constructor)]
    pub fn new(
        root: prometheus_schema::serial::Root,
        modules: PromModules,
    ) -> Result<JsPromSchema, WasmError> {
        Ok(Self {
            schema: Arc::new(Universe::load_data(root, modules.0)?),
        })
    }
}

#[wasm_bindgen]
impl JsAlertRuleTypes {
    #[wasm_bindgen(constructor)]
    pub fn new(value: Dimension, params: ParamTypeMap) -> Self {
        Self {
            value,
            params: params.0,
        }
    }
}

#[wasm_bindgen]
impl JsExprSpec {
    #[wasm_bindgen(constructor)]
    pub fn new(spec: ExprSpec) -> Self {
        Self(spec)
    }

    pub fn verify(&self, types: &JsAlertRuleTypes) -> Result<(), WasmError> {
        self.0.verify(&types.params)?;
        Ok(())
    }
}

#[wasm_bindgen]
impl JsAnnotationTemplate {
    #[wasm_bindgen(constructor)]
    pub fn new(tmpl: AnnotationTemplate) -> Self {
        Self(tmpl)
    }

    pub fn verify(&self, types: &JsAlertRuleTypes) -> Result<(), WasmError> {
        self.0.verify(types.value, &types.params)?;
        Ok(())
    }
}

#[wasm_bindgen]
impl JsUnit {
    #[wasm_bindgen(constructor)]
    pub fn new(unit: Unit) -> Self {
        Self(unit)
    }

    pub fn format_value(&self, value: f64, display_unit: Option<Unit>) -> String {
        value::Value::Quantity(unit::Quantity(value, self.0))
            .format(&value::FormatOpts {
                autoscale: true,
                precision: Some(2),
                unit: display_unit,
            })
            .unwrap_or_else(|_| String::from("-"))
    }

    pub fn convert(&self, other: Unit, val: f64) -> Result<f64, WasmError> {
        Ok(self.0.convert(&other, val)?)
    }

    pub fn dimension(&self) -> Dimension {
        self.0.dimension()
    }
}

#[wasm_bindgen]
impl JsDimension {
    #[wasm_bindgen(constructor)]
    pub fn new(dimension: Dimension) -> JsDimension {
        Self(dimension)
    }

    pub fn dimensions() -> DimensionsInfo {
        DimensionsInfo(
            Dimension::LIST
                .iter()
                .map(|dimension| DimensionInfo {
                    id: *dimension,
                    name: dimension.name(),
                    symbol: dimension.symbol(),
                })
                .collect(),
        )
    }

    pub fn reference_unit(&self) -> Unit {
        self.0.reference_unit()
    }

    pub fn units(&self) -> Units {
        Units(self.0.units().to_vec())
    }
}

#[wasm_bindgen]
impl JsItemsData {
    pub async fn download(elems: Elements, time: Option<DateTimeUtc>) -> Result<Self, WasmError> {
        let items: serial::Items<Augment<StatusInfo>> = if let Some(t) = time {
            post(
                "elements",
                &format!("/api/elements/{}?include_aggregated_status=true", t.0),
                &elems,
            )
            .await?
        } else {
            post(
                "elements",
                "/api/elements?include_aggregated_status=true",
                &elems,
            )
            .await?
        };
        Ok(Self(items))
    }

    pub fn equals(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[wasm_bindgen]
impl JsItems {
    #[wasm_bindgen(constructor)]
    pub fn new(
        types: &JsTypes,
        items: serial::Items<Augment<StatusInfo>>,
        pkg: Option<PackageId>,
    ) -> Result<JsItems, WasmError> {
        let items = items.resolve(&types.types, pkg.as_ref())?;
        let metric_index = types
            .types
            .items
            .values()
            .flat_map(|item_type| item_type.prometheus_metrics.keys())
            .map(|item_name| (item_name.clone(), OnceCell::new()))
            .collect();
        Ok(Self {
            types: types.types.clone(),
            items: Arc::new(items),
            metric_index,
        })
    }

    pub fn resolve(types: &JsTypes, items: &JsItemsData) -> Result<Self, WasmError> {
        Self::new(types, items.0.clone(), None)
    }

    /// Get all item types, including implemented ones.
    #[wasm_bindgen(js_name = "getItemTypes")]
    pub fn get_item_types(&self) -> ItemTypeIds {
        ItemTypeIds(self.items.get_item_types(&self.types))
    }

    #[wasm_bindgen(js_name = "getIds")]
    pub fn get_ids(&self) -> Ids {
        Ids {
            items: self.items.item_keys().cloned().collect(),
            relations: self.items.relation_keys().cloned().collect(),
        }
    }

    #[wasm_bindgen(js_name = "getItemIdsImplementingType")]
    pub fn get_item_ids_implementing_type(&self, item_type: AbsItemTypeId) -> ItemIds {
        ItemIds(
            self.iter_items_implementing_type(&item_type.0)
                .map(|(id, _)| id.clone())
                .collect(),
        )
    }

    fn iter_items_implementing_type<'a>(
        &'a self,
        item_type: &'a Absolute<ItemTypeId>,
    ) -> impl Iterator<Item = (&'a ItemId, &'a AugmentedItem)> {
        self.types
            .items
            .get_ref(item_type)
            .map(|type_ref| {
                self.types
                    .iter_type_and_subtypes_by_entry(item_type, type_ref)
                    .flat_map(|(typ, _)| self.items.iter_items_by_type(typ))
                    .collect::<BTreeMap<_, _>>()
            })
            .unwrap_or_default()
            .into_iter()
    }

    #[wasm_bindgen(js_name = "getTableData")]
    pub fn get_table_data(
        &self,
        item_type_id: AbsItemTypeId,
        metrics_definitions: DashboardMetrics,
        metric_data: Option<TableMetricData>,
    ) -> Result<TableData, JsValue> {
        let item_type = self
            .types
            .items
            .get(&item_type_id.0)
            .ok_or_else(|| JsValue::from(format!("unknown item type: {}", &item_type_id.0)))?;
        let item_type_ref = self
            .types
            .items
            .get_ref(&item_type_id.0)
            .ok_or_else(|| JsValue::from(format!("unknown item type: {}", &item_type_id.0)))?;
        let metric_index = self.metric_index(&item_type_id.0, item_type_ref);
        let metric_items = match metric_data {
            Some(metric_data) => metrics_definitions
                .0
                .iter()
                .filter_map(|(metric_id, metric)| {
                    Some((metric_id, metric, metric_data.0.get(metric_id)?))
                })
                .try_fold(
                    BTreeMap::<_, BTreeMap<_, _>>::new(),
                    |mut map, (metric_id, metric, data)| {
                        let (item_metrics, item_index) =
                            metric_index.get(&metric.item).ok_or_else(|| {
                                JsValue::from(format!("no metrics found for {}", metric.item))
                            })?;
                        data.series
                            .iter()
                            .filter_map(|data| {
                                let item_key = item_metrics
                                    .labels()
                                    .map(|label| {
                                        let value = data
                                            .metric
                                            .get(label)
                                            .cloned()
                                            .unwrap_or_else(String::new);
                                        (label.clone(), value)
                                    })
                                    .collect::<BTreeMap<_, _>>();
                                let item = item_index.get(&item_key)?;
                                let item_id = item.key().clone();
                                Some((item_id, data))
                            })
                            .for_each(|(item_id, data)| {
                                map.entry(item_id)
                                    .or_default()
                                    .insert(metric_id.clone(), data.value.value.0);
                            });
                        Result::<_, JsValue>::Ok(map)
                    },
                )?,
            None => BTreeMap::new(),
        };
        Ok(TableData(
            self.iter_items_implementing_type(&item_type_id.0)
                .map(|(id, item)| {
                    let metrics = metric_items.get(id);
                    RowData {
                        item_id: id.clone(),
                        item_name: item.value.name_chain(&self.items, &self.types).join(" ▸ "),
                        item_type: item.value.item_type_id().clone(),
                        status_info: item.info.clone(),
                        props: item_type
                            .properties(&self.types)
                            .map(|(id, _)| {
                                let value = item
                                    .value
                                    .property(id)
                                    .map(|v| v.to_json())
                                    .unwrap_or(serde_json::Value::Null);
                                // matches!(value.get_type(), Type::UnicodeString)
                                //     .then_some(())?;
                                (id.clone(), value)
                            })
                            .collect(),
                        metrics: metrics_definitions
                            .0
                            .keys()
                            .map(|metric_id| {
                                let value = metrics.and_then(|m| m.get(metric_id).cloned());
                                (metric_id.clone(), value)
                            })
                            .collect(),
                    }
                })
                .collect(),
        ))
    }

    #[wasm_bindgen(js_name = "getGridData")]
    pub fn get_grid_data(&self, item_types: AbsItemTypeIdVec) -> Result<GridItemData, JsValue> {
        fn get_data<'a, T: Iterator<Item = (&'a ItemId, &'a AugmentedItem)>>(
            items: &JsItems,
            iter_items: T,
        ) -> Vec<ItemData> {
            iter_items
                .map(|(id, item)| ItemData {
                    item_id: id.clone(),
                    item_name: item
                        .value
                        .name_chain(&items.items, &items.types)
                        .join(" ▸ "),
                    item_type: item.value.item_type_id().clone(),
                    item_type_name: item.value.item_type(&items.types).name.clone(),
                    status_info: item.info.clone(),
                })
                .collect()
        }
        Ok(GridItemData(
            item_types
                .0
                .into_iter()
                .flat_map(|item_type| get_data(self, self.iter_items_implementing_type(&item_type)))
                .collect(),
        ))
    }

    #[wasm_bindgen(js_name = "getItemInfo")]
    pub fn get_item_info(&self, item_id: ItemId) -> Result<ItemExtendedData, JsValue> {
        let item = self
            .items
            .get_item(&item_id)
            .ok_or_else(|| JsValue::from("item not found"))?;
        let typ = self
            .types
            .items
            .get(item.value.item_type_id())
            .ok_or_else(|| JsValue::from("item type not found"))?;
        Ok(ItemExtendedData {
            data: ItemData {
                item_id: item_id.clone(),
                item_name: item.value.name_chain(&self.items, &self.types).join(" ▸ "),
                item_type: item.value.item_type_id().clone(),
                item_type_name: typ.name.clone(),
                status_info: item.info.clone(),
            },
            meta_data: ItemMetaData {
                properties: ItemProperties {
                    data: typ
                        .properties(&self.types)
                        .map(|(id, _)| {
                            let value = item
                                .value
                                .property(id)
                                .map(|v| v.to_json())
                                .unwrap_or(serde_json::Value::Null);
                            (id.clone(), value)
                        })
                        .collect(),
                    definition: ColumnDefs(
                        typ.properties(&self.types)
                            .map(|(prop_id, prop)| ColumnDef {
                                header: prop.name.clone(),
                                title: prop.description.clone(),
                                r#type: ColumnDefType::Value(ColumnDefValue {
                                    accessor_key: format!("props.{prop_id}"),
                                    r#type: ColumnDefValueType::Props(prop.value.clone()),
                                }),
                                enableHiding: None,
                                enableColumnDragging: None,
                            })
                            .collect(),
                    ),
                },
            },
        })
    }

    #[wasm_bindgen(js_name = "getPrometheusQuery")]
    pub fn get_prometheus_query(
        &self,
        item_id: ItemId,
        prom_item_name: QualifiedItemName,
    ) -> Result<ItemQuery, JsValue> {
        let item = self
            .items
            .get_item(&item_id)
            .ok_or_else(|| JsValue::from(format!("item not found: {item_id}")))?;
        let item_type = item.value.item_type(&self.types);
        let (_, item_metrics) = item_type
            .prometheus_metrics
            .get(&prom_item_name)
            .ok_or_else(|| JsValue::from(format!("no metrics found for: {prom_item_name}")))?;
        Ok(ItemQuery(item_metrics.metric(
            &item.value,
            &self.items,
            &self.types,
        )))
    }

    #[wasm_bindgen(js_name = "getMetricKeys")]
    pub fn get_metric_keys(
        &self,
        item_type_id: AbsItemTypeId,
        prom_item_name: QualifiedItemName,
    ) -> Result<ItemKeys, JsValue> {
        let item_type_ref = self
            .types
            .items
            .get_ref(&item_type_id.0)
            .ok_or_else(|| JsValue::from(format!("unknown item type: {}", &item_type_id.0)))?;
        let item_type = self.types.items.borrow(item_type_ref);
        let (_, item_metrics) = item_type
            .prometheus_metrics
            .get(&prom_item_name)
            .ok_or_else(|| JsValue::from(format!("no metrics found for {prom_item_name}")))?;
        Ok(ItemKeys(
            item_metrics
                .labels()
                .chain(item_metrics.group_by().into_iter().flatten())
                .cloned()
                .collect(),
        ))
    }

    fn metric_index<'a>(
        &'a self,
        item_type_id: &'a Absolute<ItemTypeId>,
        item_type_ref: &'a Ref<ItemType>,
    ) -> BTreeMap<&'a QualifiedItemName, (&'a ItemMetrics, &'a ItemMetricIndex)> {
        self.types
            .items
            .borrow(item_type_ref)
            .prometheus_metrics
            .iter()
            .map(|(item_name, (_, item_metrics))| {
                let item_index =
                    self.item_metric_index(item_name, item_type_id, item_type_ref, item_metrics);
                (item_name, (item_metrics, item_index))
            })
            .collect()
    }

    fn item_metric_index<'a>(
        &'a self,
        item_name: &'a QualifiedItemName,
        item_type_id: &Absolute<ItemTypeId>,
        item_type_ref: &Ref<ItemType>,
        item_metrics: &'a ItemMetrics,
    ) -> &'a ItemMetricIndex {
        self.metric_index.get(item_name).unwrap().get_or_init(|| {
            self.types
                .iter_type_and_subtypes_by_entry(item_type_id, item_type_ref)
                .flat_map(|(item_type_id, _)| self.items.iter_item_refs_by_type(item_type_id))
                .map(|(item_id, item_ref)| {
                    let item = self.items.borrow_item(item_ref);
                    let key = item_metrics.metric(&item.value, &self.items, &self.types);
                    (key, RefBy::new(item_id.clone(), item_ref.clone()))
                })
                .collect()
        })
    }

    #[wasm_bindgen(js_name = "getInstantMetrics")]
    pub fn get_instant_metrics(
        &self,
        item_type_id: AbsItemTypeId,
        prom_item_name: QualifiedItemName,
        metrics: InstantMetricData,
    ) -> Result<JsDataPoints, JsValue> {
        let item_type_ref = self
            .types
            .items
            .get_ref(&item_type_id.0)
            .ok_or_else(|| JsValue::from(format!("unknown item type: {}", &item_type_id.0)))?;
        let item_type = self.types.items.borrow(item_type_ref);
        let (_, item_metrics) = item_type
            .prometheus_metrics
            .get(&prom_item_name)
            .ok_or_else(|| JsValue::from(format!("no metrics found for {prom_item_name}")))?;
        let item_index = self.item_metric_index(
            &prom_item_name,
            &item_type_id.0,
            item_type_ref,
            item_metrics,
        );

        Ok(JsDataPoints(
            metrics
                .0
                .series
                .into_iter()
                .map(|metric| {
                    let item_key = item_metrics
                        .labels()
                        .map(|label| {
                            let value = metric
                                .metric
                                .get(label)
                                .cloned()
                                .unwrap_or_else(String::new);
                            (label.clone(), value)
                        })
                        .collect::<BTreeMap<_, _>>();
                    let item = item_index.get(&item_key);
                    let id = item.map(|item_ref| item_ref.key().to_string());
                    let name = item.map(|item_ref| {
                        self.items
                            .borrow_item(item_ref)
                            .value
                            .name_chain(&self.items, &self.types)
                            .join(" ▸ ")
                    });
                    JsDataPoint {
                        timestamp: metric.value.timestamp,
                        id: id.clone(),
                        name: name.clone(),
                        value: metric.value.value.0,
                    }
                })
                .collect(),
        ))
    }

    #[wasm_bindgen(js_name = "getRangeMetrics")]
    pub fn get_type_range_metrics(
        &self,
        item_type_id: AbsItemTypeId,
        prom_item_name: QualifiedItemName,
        data: RangeMetricData,
    ) -> Result<JsRangeMetrics, JsValue> {
        let item_type_ref = self
            .types
            .items
            .get_ref(&item_type_id.0)
            .ok_or_else(|| JsValue::from(format!("unknown item type: {}", &item_type_id.0)))?;
        let item_type = self.types.items.borrow(item_type_ref);
        let (_, item_metrics) = item_type
            .prometheus_metrics
            .get(&prom_item_name)
            .ok_or_else(|| JsValue::from(format!("no metrics found for {prom_item_name}")))?;
        let item_index = self.item_metric_index(
            &prom_item_name,
            &item_type_id.0,
            item_type_ref,
            item_metrics,
        );

        let names = data
            .0
            .series
            .iter()
            .map(|metric| {
                let item_key = item_metrics
                    .labels()
                    .map(|label| {
                        let value = metric
                            .metric
                            .get(label)
                            .cloned()
                            .unwrap_or_else(String::new);
                        (label.clone(), value)
                    })
                    .collect::<BTreeMap<_, _>>();
                let item = item_index.get(&item_key);
                let mut item_name = item
                    .map(|item_ref| {
                        self.items
                            .borrow_item(item_ref)
                            .value
                            .name_chain(&self.items, &self.types)
                            .join(" ▸ ")
                    })
                    .unwrap_or_else(|| {
                        let mut s = String::new();
                        write!(s, "(").unwrap();
                        item_key.iter().enumerate().for_each(|(i, (label, value))| {
                            if i > 0 {
                                write!(s, ", ").unwrap();
                            }
                            write!(s, "{label}={value}").unwrap();
                        });
                        write!(s, ")").unwrap();
                        s
                    });
                if let Some(labels) = item_metrics.group_by() {
                    let mut first = true;
                    write!(item_name, " (").unwrap();
                    labels.for_each(|label| {
                        if let Some(value) = metric.metric.get(label) {
                            if first {
                                first = false;
                            } else {
                                write!(item_name, ", ").unwrap();
                            }
                            write!(item_name, "{label}={value}").unwrap();
                        }
                    });
                    write!(item_name, ")").unwrap();
                }
                item_name
            })
            .collect::<Vec<_>>();
        let rows =
            data.0
                .series
                .iter()
                .enumerate()
                .fold(BTreeMap::new(), |mut rows, (i, metric)| {
                    metric.value.iter().for_each(|datapoint| {
                        let row = rows.entry(datapoint.timestamp).or_insert_with(|| {
                            std::iter::repeat(None)
                                .take(names.len())
                                .collect::<Vec<Option<f64>>>()
                        });
                        row[i] = Some(datapoint.value.0);
                    });
                    rows
                });

        Ok(JsRangeMetrics { names, rows })
    }

    #[wasm_bindgen(js_name = "getItemRangeMetrics")]
    pub fn get_item_range_metrics(&self, data: RangeMetricData) -> Result<JsRangeMetrics, JsValue> {
        let mut names = vec!["average".to_string()];
        if data.0.series_min.is_some() & data.0.series_max.is_some() {
            names.push("min".to_string());
            names.push("max".to_string());
            names.push("upperBound".to_string());
        }
        data.0.thresholds.iter().for_each(|(name, threshold)| {
            if let Ok(t) = threshold {
                t.keys().for_each(|severity| {
                    let name = format!("{name} - {severity}");
                    names.push(name);
                })
            }
        });

        let mut rows: BTreeMap<DateTime<Utc>, Vec<Option<f64>>> =
            data.0
                .series
                .iter()
                .fold(BTreeMap::new(), |mut rows, metric| {
                    metric.value.iter().for_each(|datapoint| {
                        let row = rows.entry(datapoint.timestamp).or_insert_with(|| {
                            std::iter::repeat(None)
                                .take(names.len())
                                .collect::<Vec<Option<f64>>>()
                        });
                        row[0] = Some(datapoint.value.0);
                    });
                    rows
                });
        if let Some(series) = data.0.series_min {
            series.iter().for_each(|metric| {
                metric.value.iter().for_each(|datapoint| {
                    if let Some(row) = rows.get_mut(&datapoint.timestamp) {
                        row[1] = Some(datapoint.value.0);
                    }
                });
            })
        }
        if let Some(series) = data.0.series_max {
            series.iter().for_each(|metric| {
                metric.value.iter().for_each(|datapoint| {
                    if let Some(row) = rows.get_mut(&datapoint.timestamp) {
                        row[2] = Some(datapoint.value.0);
                        row[3] = if let (Some(max), Some(min)) = (row[2], row[1]) {
                            Some(max - min)
                        } else {
                            None
                        }
                    }
                });
            })
        }
        data.0.thresholds.iter().for_each(|(name, threshold)| {
            if let Ok(t) = threshold {
                t.iter().for_each(|(severity, threshold)| {
                    let thresholdName = format!("{name} - {severity}");
                    if let Some((i, _)) = names.iter().find_position(|name| name == &&thresholdName)
                    {
                        match threshold {
                            ScalarOr::Scalar(value) => {
                                rows.values_mut().for_each(|row| row[i] = Some(value.0))
                            }
                            ScalarOr::Series(series) => series.iter().for_each(|metric| {
                                metric.value.iter().for_each(|datapoint| {
                                    if let Some(row) = rows.get_mut(&datapoint.timestamp) {
                                        row[i] = Some(datapoint.value.0);
                                    }
                                });
                            }),
                        }
                    }
                })
            }
        });

        Ok(JsRangeMetrics { names, rows })
    }

    #[wasm_bindgen(js_name = "getAnomalyPromExpression")]
    pub fn get_anomaly_prom_expression(
        &self,
        item_id: ItemId,
        definition: AnomalyTracesGraph,
        start: DateTimeUtc,
        end: DateTimeUtc,
    ) -> Result<AnomalyTracesExpression, JsValue> {
        let params = RangeQueryParams {
            start: start.0,
            end: end.0,
            step: ((end.0 - start.0) / 400).num_milliseconds() as f64 / 1000.0,
        };
        match &definition.graph_type {
            AnomalyTracesGraphType::Metric(graph) => {
                match self.get_operation_or_service(&item_id)? {
                    OperationOrService::Operation(key) => {
                        let object = TraceObject::builder().operation().single().item(key);
                        Ok(AnomalyTracesExpression::Metric(
                            AnomalyTracesExpressionOperation {
                                confidence_interval: graph.confidence_interval.then(|| {
                                    let aggr =
                                        TraceAggr::ci(graph.immediate_interval, object.clone());
                                    TraceExpr::new(graph.metric, aggr).expr(&params)
                                }),
                                mean_reference_interval: graph.reference_interval.map(
                                    |reference_interval| {
                                        let aggr =
                                            TraceAggr::mean(reference_interval, object.clone());
                                        TraceExpr::new(graph.metric, aggr).expr(&params)
                                    },
                                ),
                                mean: {
                                    let aggr = TraceAggr::mean(graph.immediate_interval, object);
                                    TraceExpr::new(graph.metric, aggr).expr(&params)
                                },
                            },
                        ))
                    }
                    OperationOrService::Service(key) => {
                        let object = TraceObject::builder()
                            .operation()
                            .multiple(Some(graph.top))
                            .item(OperationFilter::new().service(key.into_filter()));
                        let aggr = TraceAggr::mean(graph.immediate_interval, object);
                        let expr = TraceExpr::new(graph.metric, aggr).expr(&params);
                        Ok(AnomalyTracesExpression::MetricTop(expr))
                    }
                }
            }
            AnomalyTracesGraphType::Score(graph) => {
                match self.get_operation_or_service(&item_id)? {
                    OperationOrService::Operation(key) => {
                        let object = TraceObject::builder().operation().single().item(key);
                        let aggr = TraceAggr::score(
                            graph.immediate_interval,
                            graph.reference_interval,
                            object,
                        );
                        let expr = TraceExpr::new(graph.metric, aggr).expr(&params);
                        Ok(AnomalyTracesExpression::Score(expr))
                    }
                    OperationOrService::Service(key) => match graph.top {
                        Some(top) => {
                            let object = TraceObject::builder()
                                .operation()
                                .multiple(Some(top))
                                .item(OperationFilter::new().service(key.into_filter()));
                            let aggr = TraceAggr::score(
                                graph.immediate_interval,
                                graph.reference_interval,
                                object,
                            );
                            let expr = TraceExpr::new(graph.metric, aggr).expr(&params);
                            Ok(AnomalyTracesExpression::ScoreTop(expr))
                        }
                        None => {
                            let object = TraceObject::builder()
                                .service(CombineScores::new(graph.aggr_factor))
                                .single()
                                .item(key);
                            let aggr = TraceAggr::score(
                                graph.immediate_interval,
                                graph.reference_interval,
                                object,
                            );
                            let expr = TraceExpr::new(graph.metric, aggr).expr(&params);
                            Ok(AnomalyTracesExpression::Score(expr))
                        }
                    },
                }
            }
        }
    }

    #[wasm_bindgen(js_name = "getAnomalyPromExpressionType")]
    pub fn get_anomaly_prom_expression_type(
        &self,
        item_type: AbsItemTypeId,
        definition: AnomalyTracesGraph,
        start: DateTimeUtc,
        end: DateTimeUtc,
    ) -> Result<AnomalyTracesExpression, JsValue> {
        let params = RangeQueryParams {
            start: start.0,
            end: end.0,
            step: ((end.0 - start.0) / 400).num_milliseconds() as f64 / 1000.0,
        };
        match definition.graph_type {
            AnomalyTracesGraphType::Metric(graph) => {
                if item_type.0 == "jaeger/operation".parse().unwrap() {
                    let object = TraceObject::builder()
                        .operation()
                        .multiple(Some(graph.top))
                        .item(OperationFilter::new());
                    let aggr = TraceAggr::mean(graph.immediate_interval, object);
                    let expr = TraceExpr::new(graph.metric, aggr).expr(&params);
                    Ok(AnomalyTracesExpression::MetricTop(expr))
                } else {
                    Err(JsValue::from("invalid item type for trace expression"))
                }
            }
            AnomalyTracesGraphType::Score(graph) => {
                if item_type.0 == "jaeger/operation".parse().unwrap() {
                    let object = TraceObject::builder()
                        .operation()
                        .multiple(Some(graph.top.unwrap_or(5)))
                        .item(OperationFilter::new());
                    let aggr = TraceAggr::score(
                        graph.immediate_interval,
                        graph.reference_interval,
                        object,
                    );
                    let expr = TraceExpr::new(graph.metric, aggr).expr(&params);
                    Ok(AnomalyTracesExpression::ScoreTop(expr))
                } else if item_type.0 == "jaeger/service".parse().unwrap() {
                    let object = TraceObject::builder()
                        .service(CombineScores::new(graph.aggr_factor))
                        .multiple(Some(graph.top.unwrap_or(5)))
                        .item(ServiceFilter::new());
                    let aggr = TraceAggr::score(
                        graph.immediate_interval,
                        graph.reference_interval,
                        object,
                    );
                    let expr = TraceExpr::new(graph.metric, aggr).expr(&params);
                    Ok(AnomalyTracesExpression::ScoreTop(expr))
                } else {
                    Err(JsValue::from("invalid item type for trace expression"))
                }
            }
        }
        // AnomalyTracesExpression::item_type(definition, item_type.0, start, end)
    }

    fn get_operation_or_service(
        &self,
        item_id: &ItemId,
    ) -> Result<OperationOrService<OperationKey, ServiceKey>, JsValue> {
        let get_string_prop = |item: &AugmentedItem, prop: &Absolute<PropertyId>| {
            item.value
                .property(prop)
                .and_then(|p| p.as_str())
                .map(|s| s.to_string())
        };
        let get_service_key = |item: &AugmentedItem| -> Result<ServiceKey, JsValue> {
            let service_name = get_string_prop(item, &"jaeger/service_name".parse().unwrap())
                .ok_or_else(|| JsValue::from("missing service name"))?;
            let namespace = get_string_prop(item, &"jaeger/service_namespace".parse().unwrap());
            let instance_id = get_string_prop(item, &"jaeger/service_instance_id".parse().unwrap());
            Ok(ServiceKey::new(service_name)
                .opt_namespace(namespace)
                .opt_instance_id(instance_id))
        };
        let get_operation_key = |item: &AugmentedItem,
                                 parent: &AugmentedItem|
         -> Result<OperationKey, JsValue> {
            let operation_name = get_string_prop(item, &"jaeger/operation_name".parse().unwrap())
                .ok_or_else(|| JsValue::from("missing operation name"))?;
            let service_key = get_service_key(parent)?;
            Ok(OperationKey::new(service_key, operation_name))
        };

        let item = self
            .items
            .get_item(item_id)
            .ok_or_else(|| JsValue::from(format!("item not found: {item_id}")))?;

        if item.value.item_type_id() == &"jaeger/operation".parse().unwrap() {
            let parent = item
                .value
                .parent(&self.items)
                .ok_or_else(|| JsValue::from("missing service parent"))?;

            let key = get_operation_key(item, parent)?;
            Ok(OperationOrService::Operation(key))
        } else if item.value.item_type_id() == &"jaeger/service".parse().unwrap() {
            let key = get_service_key(item)?;
            Ok(OperationOrService::Service(key))
        } else {
            Err(JsValue::from("invalid item type for trace expression"))
        }
    }
}

#[wasm_bindgen]
impl JsQuery {
    #[wasm_bindgen(constructor)]
    pub fn new(
        types: &JsTypes,
        query: serial::Query,
        pkg: Option<PackageId>,
    ) -> Result<JsQuery, WasmError> {
        let query = query.resolve(&types.types, pkg.as_ref())?;
        Ok(Self {
            types: types.types.clone(),
            query,
        })
    }

    pub fn run(&self, items: &JsItems) -> JsQueryResult {
        assert!(
            Arc::ptr_eq(&self.types, &items.types),
            "running query on items from different type universe!"
        );

        let result = self.query.run(&items.items, &self.types);
        JsQueryResult {
            types: items.types.clone(),
            items: items.items.clone(),
            result: Arc::new(result),
        }
    }

    #[wasm_bindgen(js_name = "runWithFilters")]
    pub fn run_with_filters(
        &self,
        items: &JsItems,
        filters: Filters,
        min_status: Status,
        limit: usize,
    ) -> Result<JsQueryResult, WasmError> {
        assert!(
            Arc::ptr_eq(&self.types, &items.types),
            "running query on items from different type universe!"
        );

        let filters = self.query.filters_from_serial(filters.0, None)?;
        let result = self.query.run_with_filters(
            &items.items,
            &self.types,
            &filters,
            &min_status,
            Some(limit),
        );
        Ok(JsQueryResult {
            types: items.types.clone(),
            items: items.items.clone(),
            result: Arc::new(result),
        })
    }
}

#[wasm_bindgen]
impl JsInfoQuery {
    #[wasm_bindgen(constructor)]
    pub fn new(types: &JsTypes, query: serial::InfoQuery) -> Result<JsInfoQuery, WasmError> {
        let query = query.resolve(&types.types)?;
        Ok(Self {
            types: types.types.clone(),
            query,
        })
    }

    pub fn run(
        &self,
        query_result: &JsQueryResultItems,
        metrics: InfoQueryMetrics,
    ) -> InfoQueryResult {
        assert!(
            Arc::ptr_eq(&self.types, &query_result.types),
            "running info query on items from different type universe!"
        );

        self.query.run(
            query_result.get_result(),
            &query_result.items,
            &query_result.types,
            &metrics,
        )
    }
}

#[wasm_bindgen]
impl JsStylesheet {
    #[wasm_bindgen(constructor)]
    pub fn new(styles: serial::Stylesheet, types: &JsTypes) -> Result<JsStylesheet, WasmError> {
        Ok(Self {
            styles: styles.resolve(&types.types)?,
        })
    }
}

#[wasm_bindgen]
impl JsQueryResult {
    #[wasm_bindgen(js_name = "toJs")]
    pub fn to_js(&self, pkg: Option<PackageId>) -> serial::QueryResult<Augment<StatusInfo>> {
        self.result.to_serial_wrapped(&self.items, pkg.as_ref())
    }

    #[wasm_bindgen(js_name = "getError")]
    pub fn get_error(&self) -> Option<String> {
        self.result.get_result().err().map(|s| s.to_string())
    }

    #[wasm_bindgen(js_name = "getItems")]
    pub fn get_items(&self) -> Option<JsQueryResultItems> {
        self.result
            .get_result()
            .is_ok()
            .then(|| JsQueryResultItems {
                types: self.types.clone(),
                items: self.items.clone(),
                result: self.result.clone(),
            })
    }

    #[wasm_bindgen(js_name = "getTemplateOptions")]
    pub fn get_template_options(&self) -> Filters {
        Filters(
            self.result
                .filter_opts()
                .iter()
                .map(|(k, v)| (k.clone(), v.to_serial()))
                .collect(),
        )
    }
}

#[wasm_bindgen]
impl JsQueryResultItems {
    fn get_result(&self) -> &QueryResultItems<Augment<StatusInfo>> {
        let result = self.result.get_result();
        // Checked in JsQueryResult::get_items.
        assert!(result.is_ok(), "Oops: JsQueryResultItems result is not ok!");
        result.unwrap()
    }

    /// Get all item types, including implemented ones.
    #[wasm_bindgen(js_name = "getItemTypes")]
    pub fn get_item_types(&self) -> ItemTypeIds {
        ItemTypeIds(self.get_result().get_item_types(&self.types, &self.items))
    }

    #[wasm_bindgen(js_name = "getIds")]
    pub fn get_ids(&self) -> Ids {
        Ids {
            items: self.get_result().item_keys().cloned().collect(),
            relations: self.get_result().relation_keys().cloned().collect(),
        }
    }

    #[wasm_bindgen(js_name = "getGraphData")]
    pub fn get_graph_data(
        &self,
        style: &JsStylesheet,
        info: Option<InfoQueryResult>,
        dark_mode: bool,
        external_types: AbsItemTypeIds,
    ) -> GraphData {
        let graph_args = GraphArgs {
            result: self.get_result(),
            items: &self.items,
            types: &self.types,
            dark_mode,
            info: info.as_ref(),
        };
        let mut min_max_cache_node = BTreeMap::new();
        let mut min_max_cache_edge = BTreeMap::new();
        GraphData {
            nodes: self
                .get_result()
                .iter_items(&self.items)
                .map(|(id, item)| {
                    let style = style
                        .styles
                        .for_item(id, &item.value, &self.items, &self.types);
                    let external = external_types
                        .0
                        .iter()
                        .any(|item_type| item_type == item.value.item_type_id());
                    GraphNode::new(
                        id,
                        item,
                        &style,
                        external,
                        &graph_args,
                        &mut min_max_cache_node,
                    )
                })
                .collect(),
            edges: self
                .get_result()
                .iter_relations(&self.items)
                .map(|(rel_id, relation, invert)| {
                    let style = style.styles.for_relation(
                        rel_id,
                        &relation.value,
                        &self.items,
                        &self.types,
                    );
                    GraphEdge::new(
                        rel_id,
                        relation.value.source_id().to_string(),
                        relation.value.target_id().to_string(),
                        &style,
                        invert,
                        &graph_args,
                        &mut min_max_cache_edge,
                    )
                })
                .collect(),
        }
    }
}

struct GraphArgs<'a> {
    result: &'a QueryResultItems<Augment<StatusInfo>>,
    items: &'a Items<Augment<StatusInfo>>,
    types: &'a Types,
    dark_mode: bool,
    info: Option<&'a InfoQueryResult>,
}

impl GraphNode {
    fn new<'a>(
        id: &ItemId,
        item: &AugmentedItem,
        style: &ItemStyle<'a>,
        external: bool,
        graph_args: &GraphArgs<'_>,
        min_max_cache: &mut BTreeMap<&'a ExprName, MinMaxResult<f64>>,
    ) -> Self {
        let label_text = Self::get_labels(
            id,
            item,
            graph_args.result,
            graph_args.items,
            graph_args.types,
            style,
        );
        let node_color = Self::get_node_color(item);
        let mut badges = Vec::<GraphBadge>::new();
        if let Some(badge) = Self::get_status_badge(item) {
            badges.push(badge)
        }

        let node_size = style
            .metric_size
            .map(|styler| styler.get_item_style(id, graph_args.info, min_max_cache))
            .unwrap_or_default()
            .0;

        if let Some(styler) = style.metric_badge {
            if let Some(value) = graph_args
                .info
                .and_then(|info| info.get_item_metric(&styler.expr_name, id))
            {
                badges.push(GraphBadge {
                    placement: Some(GraphBadgePlacement::RightBottom),
                    text: format!("{value:.2}"),
                    background: Some(true),
                    background_fill: Some(
                        styler.get_item_style(id, graph_args.info, min_max_cache).0,
                    ),
                    background_height: None,
                    background_width: None,
                    fill: Some(GraphColor::Predefined(PredefinedColor::White)),
                    font_size: Some(8.0),
                    word_wrap: Some(true),
                });
            }
        }

        let icon_size = node_size * 0.90;
        GraphNode {
            id: id.to_string(),
            style: GraphNodeStyle {
                size: Some(node_size),
                stroke: None,
                fill: node_color,
                fill_opacity: Some(1.0),
                label_text,
                label_fill: Some(style.label_color.as_ref().map_or_else(
                    || match graph_args.dark_mode {
                        true => GraphColor::Predefined(PredefinedColor::White),
                        false => GraphColor::Predefined(PredefinedColor::Black),
                    },
                    |color| color.select(graph_args.dark_mode).clone(),
                )),
                label_word_wrap: Some(true),
                label_font_size: Some(6.0),
                halo: None,
                halo_fill: None,
                halo_stroke: None,
                icon: Some(true),
                icon_src: match &style.icon {
                    Some(icon) => format!("/icons/{}", icon.select(graph_args.dark_mode)),
                    None => format!("/icons/{}.svg", item.value.item_type_id()),
                },
                icon_width: Some(icon_size),
                icon_height: Some(icon_size),
                badge: match badges.len() {
                    0 => Some(false),
                    _ => Some(true),
                },
                badges: Some(badges),
            },
            states: match external {
                true => vec![GraphNodeState::Disabled],
                false => Vec::new(),
            },
        }
    }

    fn get_node_color(item: &AugmentedItem) -> Option<GraphColor> {
        match item
            .info
            .individual_status
            .as_ref()
            .map(|versioned| versioned.value.status)
        {
            None => Some(GraphColor::primary()),
            Some(Status::Ok) => Some(GraphColor::primary()),
            Some(Status::Nok(Severity::Critical)) => Some(GraphColor::critical()),
            Some(Status::Nok(Severity::Major)) => Some(GraphColor::major()),
            Some(Status::Nok(Severity::Minor)) => Some(GraphColor::minor()),
            Some(Status::Nok(Severity::Warning)) => Some(GraphColor::warning()),
        }
    }

    fn get_labels(
        id: &ItemId,
        item: &AugmentedItem,
        result: &QueryResultItems<Augment<StatusInfo>>,
        items: &Items<Augment<StatusInfo>>,
        types: &Types,
        style: &ItemStyle,
    ) -> String {
        match &style.label {
            Some(ids) => ids
                .iter()
                .enumerate()
                .fold(String::new(), |mut r, (i, id)| {
                    if i > 0 {
                        write!(&mut r, " ").unwrap();
                    }
                    match item.value.property(id) {
                        None => write!(r, "(missing value)").unwrap(),
                        Some(v) => match v {
                            PropertyValue::String(s) => write!(r, "{}", s).unwrap(),
                            PropertyValue::Integer(n) => write!(&mut r, "{n}").unwrap(),
                            PropertyValue::Float(n) => write!(&mut r, "{n:.02}").unwrap(),
                            _ => write!(&mut r, "unsupported type").unwrap(),
                        },
                    };
                    r
                }),
            None => {
                fn get_sources<'a>(
                    item: &'a AugmentedItem,
                    result: &'a QueryResultItems<Augment<StatusInfo>>,
                    items: &'a Items<Augment<StatusInfo>>,
                    sources: &mut BTreeSet<&'a ItemId>,
                ) {
                    item.value
                        .sources(None)
                        .filter(|(rel_id, _)| {
                            result.is_relation_reversed(rel_id).is_some_and(|r| !r)
                        })
                        .map(|(_, rel)| items.borrow_relation(rel).value.endpoint(Endpoint::Source))
                        .chain(
                            item.value
                                .targets(None)
                                .filter(|(rel_id, _)| {
                                    result.is_relation_reversed(rel_id).unwrap_or(false)
                                })
                                .map(|(_, rel)| {
                                    items.borrow_relation(rel).value.endpoint(Endpoint::Target)
                                }),
                        )
                        .for_each(|item_ref| {
                            if sources.insert(item_ref.key()) {
                                get_sources(items.borrow_item(item_ref), result, items, sources);
                            }
                        })
                }

                fn name_context<'a>(
                    item: &'a AugmentedItem,
                    items: &'a Items<Augment<StatusInfo>>,
                    sources: &'a BTreeSet<&'a ItemId>,
                ) -> Box<dyn Iterator<Item = &'a AugmentedItem> + 'a> {
                    match item.value.parent_ref() {
                        Some(parent) if !sources.contains(parent.key()) => {
                            let parent = items.borrow_item(parent);
                            Box::new(
                                name_context(parent, items, sources).chain(std::iter::once(item)),
                            )
                        }
                        _ => Box::new(std::iter::once(item)),
                    }
                }
                let mut sources = BTreeSet::new();
                get_sources(item, result, items, &mut sources);
                let sources = sources
                    .into_iter()
                    .flat_map(|item_id| {
                        std::iter::once(item_id).chain(
                            items
                                .get_item(id)
                                .unwrap()
                                .value
                                .parents(items)
                                .map(|(parent_id, _)| parent_id),
                        )
                    })
                    .collect();
                name_context(item, items, &sources)
                    .map(|item| item.value.name(types))
                    .collect::<Vec<_>>()
                    .join(" ▸ ")
            }
        }
    }

    fn get_status_badge(item: &AugmentedItem) -> Option<GraphBadge> {
        match item.info.alerts.cmp(&0) {
            Ordering::Greater => Some(GraphBadge {
                placement: Some(GraphBadgePlacement::RightTop),
                text: match item.info.alerts {
                    n @ 1..=99 => n.to_string(),
                    _ => "99+".to_string(),
                },
                background: Some(true),
                background_fill: Some(GraphColor::primary()),
                background_height: Some(15.0),
                background_width: Some(15.0),
                fill: Some(GraphColor::Predefined(PredefinedColor::White)),
                font_size: Some(8.0),
                word_wrap: None,
            }),
            Ordering::Equal => None,
            Ordering::Less => None,
        }
    }
}

impl GraphEdge {
    fn new<'a>(
        id: &RelationId,
        source: String,
        target: String,
        style: &RelationStyle<'a>,
        invert: bool,
        graph_args: &GraphArgs<'_>,
        min_max_cache: &mut BTreeMap<&'a ExprName, MinMaxResult<f64>>,
    ) -> Self {
        let (source, target) = match invert {
            false => (source, target),
            true => (target, source),
        };

        let stroke = style.metric_color.map(|styler| {
            styler
                .get_relation_style(id, graph_args.info, min_max_cache)
                .0
        });
        let line_width = style.metric_size.map(|styler| {
            styler
                .get_relation_style(id, graph_args.info, min_max_cache)
                .0
        });
        let (label_text, label_stroke) = style
            .metric_label
            .map(|styler| {
                let text = graph_args
                    .info
                    .and_then(|info| info.get_relation_metric(&styler.expr_name, id))
                    .map(|value| format!("{value:.2}"));
                let stroke = styler
                    .get_relation_style(id, graph_args.info, min_max_cache)
                    .0;
                (text, Some(stroke))
            })
            .unwrap_or_default();

        Self {
            source,
            target,
            style: GraphEdgeStyle {
                stroke,
                line_width,
                label_stroke,
                label_text,
                label_background: Some(true),
            },
        }
    }
}

impl Serialize for JsRangeMetrics {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut rows = serializer.serialize_seq(Some(self.rows.len() + 1))?;
        rows.serialize_element(&JsRangeMetricHeaders(&self.names))?;
        self.rows.iter().try_for_each(|(time, values)| {
            rows.serialize_element(&JsRangeMetricRow(time, values))
        })?;
        rows.end()
    }
}

impl Serialize for JsRangeMetricHeaders<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut cols = serializer.serialize_seq(Some(self.0.len() + 1))?;
        cols.serialize_element(&"time")?;
        self.0
            .iter()
            .try_for_each(|name| cols.serialize_element(name))?;
        cols.end()
    }
}

impl Serialize for JsRangeMetricRow<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut cols = serializer.serialize_seq(Some(self.1.len() + 1))?;
        cols.serialize_element(&self.0)?;
        self.1
            .iter()
            .try_for_each(|value| cols.serialize_element(value))?;
        cols.end()
    }
}
