/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

#![recursion_limit = "128"]

pub mod alerts;
pub mod metrics;
pub mod status;

mod delegatable;
pub(crate) mod error;
pub(crate) mod ids;
pub(crate) mod items;
pub(crate) mod query;
pub(crate) mod state;
pub(crate) mod stylesheet;
pub(crate) mod types;
pub(crate) mod views;

pub use error::{Error, Result};
pub use ids::{
    AbsEntityTypeId, AbsEntityTypeIdRef, Absolute, ConnectionsPackageId, DashboardId, EntityId,
    EntityInfo, ItemId, ItemInfo, ItemTypeId, PackageId, PropertyId, QueryElemId, RelationId,
    RelationInfo, RelationTypeId, Relative, TplVarId, TransactionId, ViewId,
};

pub use items::augmented::{Augment, Augmented};
pub use items::resolved::{EntityRef, Item, Items, Relation, TxItems, Updates};
pub use query::info::resolved::InfoQuery;
pub use query::info::serial::{
    ExprName, InfoQueryMetricResult, InfoQueryMetrics, InfoQueryParams, InfoQueryResult,
};
pub use query::resolved::{
    Endpoint, ItemSelector, Query, QueryElem, QueryResult, QueryResultItems, RelationSelector,
    RunQueryError, TplVarDef, TplVarStrings, TplVarValues,
};
pub use state::resolved::{PackageData, PromSchemaData, State};
pub use stylesheet::resolved::{
    ByTheme, GraphColor, GraphEdgeSize, GraphMetricColor, GraphNodeSize, Interpolate, ItemStyle,
    MetricBound, MetricRangeScale, MetricStyle, PredefinedColor, RelationStyle, StyleRange,
    Stylesheet,
};
pub use types::connections::{
    ConnectionsPackage, ConnectionsPackages, ItemConnections, RelationConnections,
};
pub use types::resolved::{
    EntityTypeRef, ItemMetrics, ItemType, PackageType, RelationMetrics, RelationType, Types,
};
pub use types::serial::{
    ItemTypeInfo, ItemTypeName, Package, PackageVersion, Packages, PropertyType, PropertyTypeInfo,
    PropertyValueType, RelationTypeInfo,
};
pub use views::dashboard::{
    AnomalyTracesGraph, AnomalyTracesGraphType, Dashboard, DashboardMetric, DashboardMetricId,
    Dashboards, OverviewDashboard, Panel, Widget,
};
pub use views::view::{
    AutocompleteFilter, Table, TableColumn, Topology, TopologyFilterType, TopologyQueryFilter,
    View, Views,
};

pub mod serial {
    pub use crate::items::serial::{
        Domain, Elements, Item, ItemInfoArgs, Items, PropertyValue, Relation, StatusInfo, TypeSet,
    };
    pub use crate::query::info::serial::{InfoQuery, InfoQueryResult};
    pub use crate::query::serial::{
        Follow, ItemSelector, ItemTypeSelector, MatchItem, MatchRelation, PropertySelector, Query,
        QueryElem, QueryResult, RelationSelector, RelationTypeSelector,
    };
    pub use crate::state::serial::State;
    pub use crate::stylesheet::serial::Stylesheet;
    pub use crate::types::connections::{
        ItemKeySelector, ItemMetrics, RelationKeySelector, RelationMetrics,
    };
    pub use crate::types::serial::{Package, Packages};
}

pub mod db {
    pub use crate::items::db::{DbItem, DbItems};
}
