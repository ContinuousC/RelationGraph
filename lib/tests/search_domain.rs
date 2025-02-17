/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use relation_graph::{Items, Query, Types};
use wrapper::Identity;

mod cases;

define_tests!(
    search_domain,
    [
        (networking, "networking", "query.json"),
        (kubernetes_computing, "kubernetes", "query_computing.json"),
        (kubernetes_workloads, "kubernetes", "query_workloads.json")
    ]
);

fn search_domain(types: &Types, items: &Items<Identity>, query: &Query) {
    let search_domain = query
        .search_domain(items, types)
        .to_serial_unwrapped(items, None)
        .resolve(types, None)
        .unwrap();

    let expected = query.run(items, types).to_serial_unwrapped(items, None);
    let result = query
        .run(&search_domain, types)
        .to_serial_unwrapped(&search_domain, None);

    assert_eq!(expected, result);
}
