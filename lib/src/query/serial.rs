/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

#![allow(non_snake_case)]
use std::collections::{BTreeMap, BTreeSet};
#[cfg(not(target_family = "wasm"))]
use std::path::Path;

use serde::{Deserialize, Serialize};
use wrapper::{Identity, Wrapper};

#[cfg(not(target_family = "wasm"))]
use crate::error::Error;
use crate::{
    error::Result,
    ids::{
        ItemId, ItemTypeId, PackageId, PropertyId, QueryElemId, RelationId, RelationTypeId,
        Relative, TplVarId,
    },
    items::serial::{Item, Relation},
    serial::Elements,
    types::resolved::Types,
};

use super::resolved::{self, Endpoint, RunQueryError, TplVarDef};
use super::selector::Selector;

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Query {
    pub root: QueryElemId,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: QueryElemId]: QueryElem }"))]
    pub elements: BTreeMap<QueryElemId, QueryElem>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: TplVarId]: TplVarDef }"))]
    pub template: BTreeMap<TplVarId, TplVarDef>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: TplVarId]: any }"))]
    pub filters: BTreeMap<TplVarId, serde_json::Value>,
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(type_params = "", from_wasm_abi, into_wasm_abi)
)]
#[serde(bound(
    serialize = "W::Wrap<Item>: Serialize, W::Wrap<Relation>: Serialize",
    deserialize = "W::Wrap<Item>: Deserialize<'de>, W::Wrap<Relation>: Deserialize<'de>"
))]
pub struct QueryResult<W: Wrapper = Identity> {
    #[cfg_attr(
        feature = "tsify",
        tsify(type = "{ Ok: QueryResultItems } | { Err: RunQueryError } ")
    )]
    pub result: std::result::Result<QueryResultItems<W>, RunQueryError>,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: TplVarId]: string[] }"))]
    pub template: BTreeMap<TplVarId, serde_json::Value>,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(bound(
    serialize = "W::Wrap<Item>: Serialize, W::Wrap<Relation>: Serialize",
    deserialize = "W::Wrap<Item>: Deserialize<'de>, W::Wrap<Relation>: Deserialize<'de>"
))]
pub struct QueryResultItems<W: Wrapper = Identity> {
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: ItemId]: Item }"))]
    pub items: BTreeMap<ItemId, W::Wrap<Item>>,
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: RelationId]: Relation }"))]
    pub relations: BTreeMap<RelationId, W::Wrap<Relation>>,
}

impl<W: Wrapper> PartialEq for QueryResult<W>
where
    W::Wrap<Item>: PartialEq,
    W::Wrap<Relation>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.result == other.result && self.template == other.template
    }
}

impl<W: Wrapper> PartialEq for QueryResultItems<W>
where
    W::Wrap<Item>: PartialEq,
    W::Wrap<Relation>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items && self.relations == other.relations
    }
}

impl<W> std::fmt::Debug for QueryResult<W>
where
    W: Wrapper,
    QueryResultItems<W>: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let QueryResult { result, template } = self;
        f.debug_struct("QueryResult")
            .field("result", &result)
            .field("template", &template)
            .finish()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct QueryElem {
    pub items: ItemSelector,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub follow: Vec<Follow>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct ItemSelector(pub(super) Selector<MatchItem>);

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct RelationSelector(pub(super) Selector<MatchRelation>);

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct MatchItem {
    pub item_id: Option<ItemIdSelector>,
    pub item_type: Option<ItemTypeSelector>,
    pub parent: Option<Box<ItemSelector>>,
    pub properties: Option<PropertySelector>,
    pub relations: Option<Box<RelationSelector>>,
}

#[derive(Serialize, Deserialize, Default, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub struct MatchRelation {
    pub relation_id: Option<RelationIdSelector>,
    pub relation_type: Option<RelationTypeSelector>,
    pub endpoint: Option<Endpoint>,
    pub properties: Option<PropertySelector>,
    pub item: Option<Box<ItemSelector>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum ItemIdSelector {
    Is(#[cfg_attr(feature = "tsify", tsify(type = "string"))] ItemId),
    In(#[cfg_attr(feature = "tsify", tsify(type = "string[]"))] BTreeSet<ItemId>),
    Template(TplVarId),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum RelationIdSelector {
    Is(#[cfg_attr(feature = "tsify", tsify(type = "string"))] RelationId),
    In(#[cfg_attr(feature = "tsify", tsify(type = "string[]"))] BTreeSet<RelationId>),
    Template(TplVarId),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum ItemTypeSelector {
    Is(#[cfg_attr(feature = "tsify", tsify(type = "string"))] Relative<ItemTypeId>),
    In(#[cfg_attr(feature = "tsify", tsify(type = "string[]"))] BTreeSet<Relative<ItemTypeId>>),
    Template(TplVarId),
    // Not(Box<ItemTypeSelector>),
    // Any(Vec<ItemTypeSelector>),
    // All(Vec<ItemTypeSelector>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum RelationTypeSelector {
    Is(#[cfg_attr(feature = "tsify", tsify(type = "string"))] Relative<RelationTypeId>),
    In(#[cfg_attr(feature = "tsify", tsify(type = "string[]"))] BTreeSet<Relative<RelationTypeId>>),
    Template(TplVarId),
    // Not(Box<RelationTypeSelector>),
    // Any(Vec<RelationTypeSelector>),
    // All(Vec<RelationTypeSelector>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct PropertySelector(
    #[cfg_attr(feature = "tsify", tsify(type = "{ [key: PropertyId]: any }"))]
    pub  BTreeMap<Relative<PropertyId>, resolved::ValueSelector>,
);

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Follow {
    pub relation: RelationSelector,
    #[cfg_attr(feature = "tsify", tsify(type = "QueryElemId"))]
    pub element: QueryElemId,
}

impl Query {
    #[cfg(not(target_family = "wasm"))]
    pub fn load(path: &Path) -> Result<Self> {
        let data =
            std::fs::read_to_string(path).map_err(|e| Error::ReadQuery(path.to_path_buf(), e))?;
        serde_json::from_str::<Self>(&data).map_err(|e| Error::DecodeQuery(path.to_path_buf(), e))
    }

    pub fn resolve(self, types: &Types, pkg: Option<&PackageId>) -> Result<resolved::Query> {
        let mut query = resolved::Query::from_serial(self, types, pkg)?;
        query.resolve(types)?;
        Ok(query)
    }

    pub fn from_elements(elements: Elements) -> Result<Self> {
        let root_elem_id = QueryElemId::new(String::from("root"));
        Ok(Self {
            root: root_elem_id.clone(),
            elements: BTreeMap::from_iter([(
                QueryElemId::new(String::from("root")),
                QueryElem {
                    items: ItemSelector(Selector::Match(MatchItem {
                        item_type: Some(ItemTypeSelector::In(elements.item_types)),
                        ..MatchItem::default()
                    })),
                    follow: Vec::from_iter([Follow {
                        relation: RelationSelector(Selector::Match(MatchRelation {
                            relation_type: Some(RelationTypeSelector::In(elements.relation_types)),
                            ..MatchRelation::default()
                        })),
                        element: root_elem_id,
                    }]),
                },
            )]),
            template: BTreeMap::from_iter([]),
            filters: BTreeMap::from_iter([]),
        })
    }

    pub fn get_item_context(item_id: ItemId) -> Query {
        Query {
            root: QueryElemId::new(String::from("root")),
            elements: BTreeMap::from_iter([
                (
                    QueryElemId::new(String::from("root")),
                    QueryElem {
                        items: ItemSelector(Selector::Match(MatchItem {
                            item_id: Some(ItemIdSelector::Is(item_id)),
                            ..MatchItem::default()
                        })),
                        follow: vec![Follow {
                            relation: RelationSelector(Selector::Match(MatchRelation {
                                relation_type: Some(RelationTypeSelector::Template(TplVarId::new(
                                    String::from("relation"),
                                ))),
                                ..MatchRelation::default()
                            })),
                            element: QueryElemId::new(String::from("item")),
                        }],
                    },
                ),
                (
                    QueryElemId::new(String::from("item")),
                    QueryElem {
                        items: ItemSelector(Selector::Match(MatchItem {
                            item_type: Some(ItemTypeSelector::Template(TplVarId::new(
                                String::from("item"),
                            ))),
                            ..MatchItem::default()
                        })),
                        follow: vec![],
                    },
                ),
            ]),
            template: BTreeMap::from_iter([
                (
                    TplVarId::new(String::from("relation")),
                    TplVarDef::RelationTypes,
                ),
                (TplVarId::new(String::from("item")), TplVarDef::ItemTypes),
            ]),
            filters: BTreeMap::from_iter([]),
        }
    }
}

impl ItemSelector {
    pub fn resolve(
        self,
        types: &Types,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<resolved::ItemSelector> {
        let mut selector = resolved::ItemSelector::from_serial(self, types, template, pkg)?;
        selector.resolve(types)?;
        Ok(selector)
    }
}

impl RelationSelector {
    pub fn resolve(
        self,
        types: &Types,
        template: &BTreeMap<TplVarId, TplVarDef>,
        pkg: Option<&PackageId>,
    ) -> Result<resolved::RelationSelector> {
        let mut selector = resolved::RelationSelector::from_serial(self, types, template, pkg)?;
        selector.resolve(types)?;
        Ok(selector)
    }
}
