/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use serde::{Deserialize, Serialize};

use crate::{
    error::Result,
    query::serial::{ItemSelector, RelationSelector},
    types::resolved::Types,
};

use super::resolved::{self, ItemStyleSetting, RelationStyleSetting};

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct Stylesheet {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub items: Vec<ItemStyleRule>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub relations: Vec<RelationStyleRule>,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ItemStyleRule {
    pub selector: ItemSelector,
    pub style: ItemStyleSetting,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct RelationStyleRule {
    pub selector: RelationSelector,
    pub style: RelationStyleSetting,
}

impl Stylesheet {
    pub fn resolve(self, types: &Types) -> Result<resolved::Stylesheet> {
        resolved::Stylesheet::from_serial(self, types)
    }
    pub fn empty() -> Self {
        Self {
            items: vec![],
            relations: vec![],
        }
    }
}
