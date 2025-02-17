/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::ids::{Absolute, ItemTypeId, Relative, TopologyId, TplVarId, ViewId};
use crate::items::serial::Elements;
use crate::query::serial::Query;
use crate::serial::InfoQuery;
use crate::stylesheet::serial::Stylesheet;
use crate::{Error, ItemId, Result};

use super::dashboard::{Dashboard, DashboardMetric, DashboardMetricId};
use crate::PropertyId;

#[derive(Serialize, Deserialize, Default, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Views(
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: ViewId]: View }"))]
    BTreeMap<ViewId, Arc<View>>,
);

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "camelCase")]
pub struct View {
    name: String,
    svg_source: Option<String>,
    svg_source_dark: Option<String>,
    elements: ViewElements,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: Topology }"))]
    topologies: BTreeMap<TopologyId, Topology>,
    default_topology: Option<TopologyId>,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: any }"))]
    default_topology_filters: BTreeMap<TplVarId, serde_json::Value>,
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: ItemTypeId]: ItemTypeDefinition }")
    )]
    item_types: BTreeMap<Absolute<ItemTypeId>, ItemTypeDefinition>,
    default_item_type: Option<Absolute<ItemTypeId>>,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "camelCase")]
pub struct ViewElements {
    internal: Elements,
    external: Elements,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "camelCase")]
pub struct Topology {
    query: Query,
    info_query: Option<InfoQuery>,
    query_filters: Vec<TopologyQueryFilter>,
    stylesheet: Stylesheet,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "camelCase")]
pub struct ItemTypeDefinition {
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ [key: DashboardMetricId]: DashboardMetric }")
    )]
    metrics: BTreeMap<DashboardMetricId, DashboardMetric>,
    dashboards: Dashboard,
    table: Table,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: string]: Topology }"))]
    topologies: Option<BTreeMap<TopologyId, Topology>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Table {
    pub column_order: Option<Vec<TableColumn>>,
    pub column_visibility: Option<Vec<TableColumn>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "camelCase")]
pub enum TableColumn {
    Property(Relative<PropertyId>),
    Metric(DashboardMetricId),
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct TopologyQueryFilter {
    name: String,
    label: String,
    filter: TopologyFilterType,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum TopologyFilterType {
    Autocomplete(AutocompleteFilter),
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "camelCase")]
pub struct AutocompleteFilter {
    multiple_selection: bool,
}

impl Views {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn insert(&mut self, id: ViewId, view: View) -> Result<()> {
        for view_other in self.0.values() {
            for item_type in &view.elements.internal.item_types {
                if view_other.elements.internal.item_types.contains(item_type) {
                    return Err(Error::ViewSameItemType(item_type.clone()));
                }
            }
            for relation_type in &view.elements.internal.relation_types {
                if view_other
                    .elements
                    .internal
                    .relation_types
                    .contains(relation_type)
                {
                    return Err(Error::ViewSameRelationType(relation_type.clone()));
                }
            }
        }
        self.0.insert(id, Arc::new(view));
        Ok(())
    }

    pub fn get(&self, id: &ViewId) -> Option<&View> {
        self.0.get(id).map(|view| view.as_ref())
    }

    pub fn find_item_type_view(&self, item_type: Relative<ItemTypeId>) -> Option<ViewId> {
        for view in self.0.iter() {
            if view.1.elements.internal.item_types.contains(&item_type) {
                return Some(view.0.clone());
            }
        }
        None
    }
}

impl Topology {
    pub fn item_default(item_id: ItemId) -> Self {
        Self {
            query: Query::get_item_context(item_id),
            info_query: None,
            query_filters: vec![TopologyQueryFilter {
                name: String::from("item"),
                label: String::from("item"),
                filter: TopologyFilterType::Autocomplete(AutocompleteFilter {
                    multiple_selection: true,
                }),
            }],
            stylesheet: Stylesheet::empty(),
        }
    }
}
