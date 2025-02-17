/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;

use relation_graph::{serial, Items, Query, Types};
use wrapper::Identity;

mod cases;

define_tests!(
    elastic_query,
    [
        (networking, "networking", "query.json"),
        (kubernetes_computing, "kubernetes", "query_computing.json"),
        (kubernetes_workloads, "kubernetes", "query_workloads.json")
    ]
);

fn elastic_query(types: &Types, items: &Items<Identity>, query: &Query) {
    let esquery = query.elastic_query(types);
    let serial = items.to_serial(None);

    let mut filtered_items = match esquery.items {
        Some(ts) => serial
            .items
            .into_iter()
            .filter(|(_, item)| ts.contains(&item.item_type.clone().resolve_opt(None)))
            .collect(),
        None => serial.items,
    };
    let missing_parents = filtered_items
        .iter()
        .filter_map(|(id, item)| {
            (!filtered_items.contains_key(item.parent.as_ref()?)).then_some(id)
        })
        .cloned()
        .collect::<Vec<_>>();
    missing_parents.iter().for_each(|id| {
        // Aargh! Need some kind of item subset...
        filtered_items.get_mut(id).unwrap().parent.take();
    });

    let filtered_relations = match esquery.relations {
        Some(ts) => serial
            .relations
            .into_iter()
            .filter(|(_, rel)| ts.contains(&rel.relation_type.clone().resolve_opt(None)))
            .filter(|(_, rel)| {
                filtered_items.contains_key(&rel.source) && filtered_items.contains_key(&rel.target)
            })
            .collect(),
        None => serial
            .relations
            .into_iter()
            .filter(|(_, rel)| {
                filtered_items.contains_key(&rel.source) && filtered_items.contains_key(&rel.target)
            })
            .collect(),
    };

    let filtered: Items = serial::Items {
        items: filtered_items,
        relations: filtered_relations,
    }
    .resolve(types, None)
    .unwrap();

    let expected = query
        .run_with_filters(items, types, &BTreeMap::new(), &(), None)
        .get_result()
        .unwrap()
        .to_serial_unwrapped(items, None);
    let result = query
        .run_with_filters(&filtered, types, &BTreeMap::new(), &(), None)
        .get_result()
        .unwrap()
        .to_serial_unwrapped(&filtered, None);

    let missing_items = expected
        .items
        .iter()
        .filter(|(id, _)| !result.items.contains_key(id))
        .collect::<BTreeMap<_, _>>();

    let extra_items = result
        .items
        .iter()
        .filter(|(id, _)| !expected.items.contains_key(id))
        .collect::<BTreeMap<_, _>>();

    let missing_relations = expected
        .relations
        .iter()
        .filter(|(id, _)| !result.relations.contains_key(id))
        .collect::<BTreeMap<_, _>>();

    let extra_relations = result
        .relations
        .iter()
        .filter(|(id, _)| !expected.relations.contains_key(id))
        .collect::<BTreeMap<_, _>>();

    assert!(
        missing_items.is_empty()
            && missing_relations.is_empty()
            && extra_items.is_empty()
            && extra_relations.is_empty(),
        "Missing items: {}\n\
         Missing relations: {}\n\
         Extra items: {}\n\
         Extra relations: {}",
        serde_json::to_string(&missing_items).unwrap(),
        serde_json::to_string(&missing_relations).unwrap(),
        serde_json::to_string(&extra_items).unwrap(),
        serde_json::to_string(&extra_relations).unwrap()
    );
}
