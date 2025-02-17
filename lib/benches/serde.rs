/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::path::Path;

use criterion::{criterion_group, criterion_main, Criterion};
use relation_graph::{serial::Items, Packages};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use wrapper::Identity;

trait Serde {
    const NAME: &str;
    fn serialize<T: Serialize>(value: &T) -> Vec<u8>;
    fn deserialize<'de, T: Deserialize<'de>>(bytes: &'de [u8]) -> T;
}

struct Json;

impl Serde for Json {
    const NAME: &str = "json";

    fn serialize<T: Serialize>(value: &T) -> Vec<u8> {
        serde_json::to_vec(value).expect("failed to serialize to json")
    }

    fn deserialize<'de, T: Deserialize<'de>>(bytes: &'de [u8]) -> T {
        serde_json::from_slice(bytes).expect("failed to deserialize from json")
    }
}

struct Bitcode;

impl Serde for Bitcode {
    const NAME: &str = "bitcode";

    fn serialize<T: Serialize>(value: &T) -> Vec<u8> {
        bitcode::serialize(value).expect("failed to serialize to bitcode")
    }

    fn deserialize<'de, T: Deserialize<'de>>(bytes: &'de [u8]) -> T {
        bitcode::deserialize(bytes).expect("failed to deserialize from bitcode")
    }
}

struct Bincode;

impl Serde for Bincode {
    const NAME: &str = "bincode";

    fn serialize<T: Serialize>(value: &T) -> Vec<u8> {
        bincode::serialize(value).expect("failed to serialize to bincode")
    }

    fn deserialize<'de, T: Deserialize<'de>>(bytes: &'de [u8]) -> T {
        bincode::deserialize(bytes).expect("failed to deserialize from bincode")
    }
}

fn bench_deserialize<T: Serialize + DeserializeOwned>(c: &mut Criterion, id: &str, value: &T) {
    fn bench_deserialize_serde<T: Serialize + DeserializeOwned, S: Serde>(
        c: &mut Criterion,
        id: &str,
        value: &T,
    ) {
        c.bench_function(&format!("deserialize_{id}_{}", S::NAME), |b| {
            let data = S::serialize(value);
            b.iter(|| {
                let _value: T = S::deserialize(&data);
            })
        });
    }

    bench_deserialize_serde::<T, Json>(c, id, value);
    bench_deserialize_serde::<T, Bitcode>(c, id, value);
    bench_deserialize_serde::<T, Bincode>(c, id, value);
}

fn bench_deserialize_pkgs(c: &mut Criterion) {
    let types = Packages::load_sync(Path::new("../tests/kubernetes/pkgs")).unwrap();
    bench_deserialize(c, "pkgs", &types);
}

fn bench_deserialize_items(c: &mut Criterion) {
    let items = Items::<Identity>::load(Path::new("../tests/kubernetes/items.json")).unwrap();
    bench_deserialize(c, "items", &items);
}

criterion_group!(
    name = benches;
    config = Criterion::default(); //.sample_size(500).measurement_time(std::time::Duration::from_secs(15));
    targets = bench_deserialize_pkgs, bench_deserialize_items
);
criterion_main!(benches);
