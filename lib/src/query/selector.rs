/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use serde::{Deserialize, Serialize};

use crate::query::filter::MatchIter;

use super::{filter::MatchResult, resolved::Filters};

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub(crate) enum Selector<T> {
    Not(Box<Self>),
    All(Vec<Self>),
    Any(Vec<Self>),
    // Deserialize implemented as: #[serde(untagged, alias(tagged))]
    //#[serde(untagged)]
    Match(T),
}

#[cfg(feature = "schemars")]
impl<T: schemars::JsonSchema> schemars::JsonSchema for Selector<T> {
    fn schema_id() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Owned(Self::schema_name())
    }

    fn schema_name() -> String {
        format!("Selector_for_{}", T::schema_name())
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        // TODO!
        T::json_schema(gen)
    }
}

impl<T> Selector<T> {
    // pub(crate) fn map<F: FnMut(T) -> U, U>(self, mut f: &mut F) -> Selector<U> {
    //     match self {
    //         Selector::Not(sel) => Selector::Not(Box::new(sel.map(f))),
    //         Selector::All(sels) => {
    //             Selector::All(sels.into_iter().map(|sel| sel.map(f)).collect())
    //         }
    //         Selector::Any(sels) => {
    //             Selector::Any(sels.into_iter().map(|sel| sel.map(f)).collect())
    //         }
    //         Selector::Match(m) => Selector::Match(f(m)),
    //     }
    // }

    pub(crate) fn map_ref<F: FnMut(&T) -> U, U>(&self, mut f: F) -> Selector<U> {
        fn run<T, F: FnMut(&T) -> U, U>(sel: &Selector<T>, f: &mut F) -> Selector<U> {
            match sel {
                Selector::Not(sel) => Selector::Not(Box::new(run(sel, f))),
                Selector::All(sels) => Selector::All(sels.iter().map(|sel| run(sel, f)).collect()),
                Selector::Any(sels) => Selector::Any(sels.iter().map(|sel| run(sel, f)).collect()),
                Selector::Match(m) => Selector::Match(f(m)),
            }
        }
        run(self, &mut f)
    }

    pub(crate) fn try_map<F: FnMut(T) -> Result<U, E>, U, E>(
        self,
        mut f: F,
    ) -> Result<Selector<U>, E> {
        fn run<F: FnMut(T) -> Result<U, E>, T, U, E>(
            sel: Selector<T>,
            f: &mut F,
        ) -> Result<Selector<U>, E> {
            match sel {
                Selector::Not(sel) => Ok(Selector::Not(Box::new(run(*sel, f)?))),
                Selector::All(sels) => Ok(Selector::All(
                    sels.into_iter()
                        .map(|sel| run(sel, f))
                        .collect::<Result<_, E>>()?,
                )),
                Selector::Any(sels) => Ok(Selector::Any(
                    sels.into_iter()
                        .map(|sel| run(sel, f))
                        .collect::<Result<_, E>>()?,
                )),
                Selector::Match(m) => Ok(Selector::Match(f(m)?)),
            }
        }
        run(self, &mut f)
    }

    pub(crate) fn try_modify<F: FnMut(&mut T) -> Result<(), E>, E>(
        &mut self,
        mut f: F,
    ) -> Result<(), E> {
        fn run<F: FnMut(&mut T) -> Result<(), E>, E, T>(
            sel: &mut Selector<T>,
            f: &mut F,
        ) -> Result<(), E> {
            match sel {
                Selector::Not(sel) => run(sel, f),
                Selector::All(sels) | Selector::Any(sels) => {
                    sels.iter_mut().try_for_each(|sel| run(sel, f))
                }
                Selector::Match(m) => f(m),
            }
        }
        run(self, &mut f)
    }

    pub(crate) fn for_each<'a, F: FnMut(&'a T)>(&'a self, mut f: F) {
        fn run<'a, T, F: FnMut(&'a T)>(sel: &'a Selector<T>, f: &mut F) {
            match sel {
                Selector::Not(sel) => run(sel, f),
                Selector::All(sels) | Selector::Any(sels) => {
                    sels.iter().for_each(|sel| run(sel, f))
                }
                Selector::Match(m) => f(m),
            }
        }
        run(self, &mut f)
    }

    // pub(crate) fn map_reduce<'a, M, S, R, U>(&'a self, map: M, init: S, reduce: R) -> U
    // where
    //     M: Fn(&'a T) -> U,
    //     S: Fn() -> U,
    //     R: Fn(U, U) -> U,
    // {
    //     match self {
    //         Selector::Not(sel) => sel.map_reduce(map, init, reduce),
    //         Selector::All(sels) | Selector::Any(sels) => sels
    //             .iter()
    //             .map(|sel| sel.map_reduce(&map, &init, &reduce))
    //             .fold(init(), &reduce),
    //         Selector::Match(m) => map(m),
    //     }
    // }

    pub(crate) fn is_filtered_on<F: Fn(&T, &Filters) -> bool>(
        &self,
        filters: &Filters,
        f: &F,
    ) -> bool {
        match self {
            Selector::Match(m) => f(m, filters),
            Selector::Not(sel) => sel.is_filtered_on(filters, f),
            Selector::All(sels) | Selector::Any(sels) => {
                sels.iter().any(|sel| sel.is_filtered_on(filters, f))
            }
        }
    }

    pub(crate) fn find_required<'a, F: FnMut(&'a T) -> Option<U>, U>(
        &'a self,
        mut f: F,
    ) -> Option<U> {
        fn run<'a, F: FnMut(&'a T) -> Option<U>, T, U>(
            sel: &'a Selector<T>,
            f: &mut F,
        ) -> Option<U> {
            match sel {
                Selector::Match(m) => f(m),
                Selector::All(sels) => sels.iter().find_map(|sel| run(sel, f)),
                Selector::Any(sels) if sels.len() == 1 => run(&sels[0], f),
                Selector::Not(_) | Selector::Any(_) => None,
            }
        }
        run(self, &mut f)
    }

    pub(crate) fn matches<'a, R: MatchResult, F: FnMut(&'a T) -> R>(&'a self, mut f: F) -> R {
        fn run<'a, T, R: MatchResult, F: FnMut(&'a T) -> R>(sel: &'a Selector<T>, f: &mut F) -> R {
            match sel {
                Selector::Not(sel) => run(sel, f).not(),
                Selector::All(sels) => sels.iter().map(|sel| run(sel, f)).match_all(),
                Selector::Any(sels) => sels.iter().map(|sel| run(sel, f)).match_any(),
                Selector::Match(m) => f(m),
            }
        }
        run(self, &mut f)
    }
}

// impl<'de, T: Deserialize<'de>> Deserialize<'de> for Selector<T> {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where
//         D: Deserializer<'de>,
//     {
//         if deserializer.is_human_readable() {
//             struct Visitor<T>(PhantomData<T>);

//             impl<'de, T: Deserialize<'de>> serde::de::Visitor<'de> for Visitor<T> {
//                 type Value = Selector<T>;

//                 fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
//                     write!(formatter, "a selector")
//                 }

//                 fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
//                 where
//                     A: MapAccess<'de>,
//                 {
//                     match map.next_key::<String>()?.as_deref() {
//                         Some("not") => {
//                             let sel = map.next_value()?;
//                             map.next_key::<&str>()?
//                                 .is_none()
//                                 .then_some(())
//                                 .ok_or_else(|| {
//                                     A::Error::custom("expected a single tag for enum")
//                                 })?;
//                             Ok(Selector::Not(sel))
//                         }
//                         Some("all") => {
//                             let val = map.next_value()?;
//                             map.next_key::<&str>()?
//                                 .is_none()
//                                 .then_some(())
//                                 .ok_or_else(|| {
//                                     A::Error::custom("expected a single tag for enum")
//                                 })?;
//                             Ok(Selector::All(val))
//                         }
//                         Some("any") => {
//                             let val = map.next_value()?;
//                             map.next_key::<&str>()?
//                                 .is_none()
//                                 .then_some(())
//                                 .ok_or_else(|| {
//                                     A::Error::custom("expected a single tag for enum")
//                                 })?;
//                             Ok(Selector::Any(val))
//                         }
//                         Some("match") => {
//                             let val = map.next_value()?;
//                             map.next_key::<&str>()?
//                                 .is_none()
//                                 .then_some(())
//                                 .ok_or_else(|| {
//                                     A::Error::custom("expected a single tag for enum")
//                                 })?;
//                             Ok(Selector::Match(val))
//                         }
//                         Some(key) => {
//                             let val = T::deserialize(MatchDeserializer(Some(key), &mut map))?;
//                             Ok(Selector::Match(val))
//                         }
//                         None => Ok(Selector::Match(T::deserialize(MapDeserializer::new(
//                             std::iter::empty::<(&str, &str)>(),
//                         ))?)),
//                     }
//                 }
//             }
//             deserializer.deserialize_map(Visitor(PhantomData))
//         // } else {
//         //     #[derive(Deserialize)]
//         //     enum DeSelector<T> {
//         //         Not(Box<Selector<T>>),
//         //         All(Vec<Selector<T>>),
//         //         Any(Vec<Selector<T>>),
//         //         Match(T),
//         //     }
//         //     Ok(match DeSelector::deserialize(deserializer)? {
//         //         DeSelector::Not(v) => Selector::Not(v),
//         //         DeSelector::All(v) => Selector::All(v),
//         //         DeSelector::Any(v) => Selector::Any(v),
//         //         DeSelector::Match(v) => Selector::Match(v),
//         //     })
//         // }
//     }
// }

// struct MatchDeserializer<'a, A>(Option<&'a str>, &'a mut A);

// impl<'a, 'de, A: MapAccess<'de>> Deserializer<'de> for MatchDeserializer<'a, A> {
//     type Error = A::Error;

//     fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
//     where
//         V: serde::de::Visitor<'de>,
//     {
//         visitor.visit_map(self)
//     }

//     forward_to_deserialize_any!(bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64
// 								char str string bytes byte_buf option unit unit_struct
// 								newtype_struct seq tuple tuple_struct map struct enum
// 								identifier ignored_any);
// }

// impl<'a, 'de, A: MapAccess<'de>> MapAccess<'de> for MatchDeserializer<'a, A> {
//     type Error = A::Error;

//     fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
//     where
//         K: DeserializeSeed<'de>,
//     {
//         if let Some(key) = self.0.take() {
//             Ok(Some(seed.deserialize(StrDeserializer::new(key))?))
//         } else {
//             self.1.next_key_seed(seed)
//         }
//     }

//     fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
//     where
//         V: DeserializeSeed<'de>,
//     {
//         self.1.next_value_seed(seed)
//     }
// }
