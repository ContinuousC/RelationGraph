/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};
use wrapper::{Wrapped, Wrapper};

#[derive(Debug)]
pub struct Augment<I>(PhantomData<I>);

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi, type = "T & I"))]
pub struct Augmented<T, I> {
    //#[serde(flatten)]
    pub value: T,
    //#[serde(flatten)]
    pub info: I,
}

impl<I> Wrapper for Augment<I> {
    type Wrap<T> = Augmented<T, I>;
}

impl<T, I> Wrapped<Augment<I>, T> for Augmented<T, I> {
    fn get_wrapped(&self) -> &T {
        &self.value
    }

    fn try_get_wrapped(&self) -> Option<&T> {
        Some(&self.value)
    }

    fn get_wrapped_mut(&mut self) -> &mut T {
        &mut self.value
    }

    fn unwrap_wrapped(self) -> T {
        self.value
    }

    fn map_wrapped<F, U>(self, f: F) -> Augmented<U, I>
    where
        F: FnOnce(T) -> U,
    {
        Augmented {
            value: f(self.value),
            info: self.info,
        }
    }

    fn try_map_wrapped<F, U, E>(self, f: F) -> Result<Augmented<U, I>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        Ok(Augmented {
            value: f(self.value)?,
            info: self.info,
        })
    }
}
