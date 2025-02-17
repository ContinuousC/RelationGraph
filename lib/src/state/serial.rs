/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use serde::{Deserialize, Serialize};

use crate::{items::serial, types::serial::Packages};

#[derive(Serialize, Deserialize)]
pub struct State {
    #[serde(default)]
    pub packages: Packages,
    #[serde(default)]
    pub items: serial::Items,
}
