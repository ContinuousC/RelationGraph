/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::path::Path;

use criterion::{criterion_group, criterion_main, Criterion};
use relation_graph::{
    serial::{Items, Query},
    Packages,
};
use serde_json::json;
use wrapper::Identity;

fn bench_query(c: &mut Criterion, test_name: &str, query_name: &str) {
    // tests/kubernetes/pkgs tests/kubernetes/items.json tests/kubernetes/query_computing.json
    let types = Packages::load_sync(Path::new("../tests/kubernetes/pkgs"))
        .unwrap()
        .types()
        .unwrap();
    let items = Items::<Identity>::load(Path::new("../tests/kubernetes/items.json"))
        .unwrap()
        .resolve(&types, None)
        .unwrap();
    let query = Query::load(Path::new(&format!("../tests/kubernetes/{query_name}.json")))
        .unwrap()
        .resolve(&types, None)
        .unwrap();
    let filters = query
        .filters_from_serial(
            serde_json::from_value(json!({
                "namespace": ["default"]
            }))
            .unwrap(),
            None,
        )
        .unwrap();

    c.bench_function(test_name, |b| {
        b.iter(|| {
            let _ = query.run_with_filters(&items, &types, &filters, &(), Some(1000));
        })
    });
}

fn bench_kubernetes_computing_query(c: &mut Criterion) {
    bench_query(c, "kubernetes_computing_query", "query_computing")
}

fn bench_kubernetes_workloads_query(c: &mut Criterion) {
    bench_query(c, "kubernetes_workloads_query", "query_workloads")
}

criterion_group!(
    name = benches;
    config = Criterion::default().sample_size(500).measurement_time(std::time::Duration::from_secs(30));
    targets = bench_kubernetes_computing_query, bench_kubernetes_workloads_query
);
criterion_main!(benches);
