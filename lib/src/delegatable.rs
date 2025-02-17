/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use ambassador::delegatable_trait_remote;

#[delegatable_trait_remote]
pub trait Iterator {
    type Item;
    fn next(&mut self) -> Option<<Self as Iterator>::Item>;
}
