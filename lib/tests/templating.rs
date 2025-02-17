/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeSet;

use relation_graph::{Items, Query, TplVarValues, Types};

mod cases;

define_tests!(
    templating,
    [
        (kubernetes_computing, "kubernetes", "query_computing.json"),
        (kubernetes_workloads, "kubernetes", "query_workloads.json"),
        (networking, "networking", "query.json")
    ]
);

fn templating(types: &Types, items: &Items, query: &Query) {
    let result = query.run_with_filters(items, types, query.default_filters(), &(), Some(5000));
    let result_items = result.get_result().unwrap();
    let orig_items = result_items.item_keys().collect::<BTreeSet<_>>();
    let orig_rels = result_items.relation_keys().collect::<BTreeSet<_>>();
    assert!(
        !orig_items.is_empty(),
        "default filters yielded empty result"
    );

    result.filter_opts().iter().for_each(|(var, vals)|  {
		let typ = *query.template().get(var).unwrap();
		let vs = vals.as_refs();
        match query.default_filters().get(var) {
            None => {
                vs.iter().for_each(|v| {
                    let mut filters = query.default_filters().clone();
                    filters.insert(
                        var.clone(),
                        v.to_values(),
                    );
                    let res = query.run_with_filters(items, types, &filters, &(), None);
					let res_items = res.get_result().unwrap();

                    let new_items = res_items.item_keys().collect::<BTreeSet<_>>();
                    let new_rels = res_items.relation_keys().collect::<BTreeSet<_>>();
                    let new_vs = res.filter_opts().get(var).unwrap().as_refs();

                    assert!(
                        !new_items.is_empty(),
                        "adding filter {var} = [{v:?}] yielded empty result"
                    );
                    assert!(
                        new_items.is_subset(&orig_items) && new_rels.is_subset(&orig_rels),
                        "new result contains extra items when adding filter {var} = [{v:?}]"
                    );
                    // assert!(
                    //     (&new_items, &new_rels) != (&orig_items, &orig_rels),
                    //     "new result not strictly smaller when adding filter {var} = [{v}]"
                    // );
                    assert!(new_vs == vs, "template values influenced by adding filter {var} = [{v:?}]");
                });

				{
					let mut filters = query.default_filters().clone();
                    filters.insert(var.clone(), TplVarValues::from_refs(typ, &vs));
                    let res = query.run_with_filters(items, types, &filters,&(), None);
					// let res_items = res.get_result().unwrap();

                    // let new_items = res_items.item_keys().collect::<BTreeSet<_>>();
                    // let new_rels = res_items.relation_keys().collect::<BTreeSet<_>>();
                    let new_vs = res.filter_opts().get(var).unwrap().as_refs();

					// assert!(
					// 	(&new_items, &new_rels) == (&orig_items, &orig_rels),
					// 	"result influenced by adding filter {var} containing all options"
					// );
                    assert!(new_vs == vs, "template values influenced by adding filter {var}");
				}
            }
            Some(fs) => {
				let fs = fs.as_refs();
                assert!(
                    vs.is_superset(&fs),
                    "not all default filter values where present in template options for {var}: {vs:?} vs {fs:?} (orig_items: {orig_items:?})"
                );
                (&vs - &fs).iter().for_each(|v| {
                    let mut filters = query.default_filters().clone();
                    filters.insert(
                        var.clone(),
                        TplVarValues::from_refs(
							typ,
                            &(&fs | &BTreeSet::from_iter([*v])),
                        ),
                    );
                    let res = query.run_with_filters(items, types, &filters,&(),None);
					let res_items = res.get_result().unwrap();

                    let new_items = res_items.item_keys().collect::<BTreeSet<_>>();
                    let new_rels = res_items.relation_keys().collect::<BTreeSet<_>>();
                    let new_vs = res.filter_opts().get(var).unwrap().as_refs();

                    assert!(
                        new_items.is_superset(&orig_items) && new_rels.is_superset(&orig_rels),
                        "new result does not include all original items when extending filter {var} with {v:?}"
                    );
                    assert!(
                        (&new_items, &new_rels) != (&orig_items, &orig_rels),
                        "new result not strictly larger when extending filter {var} with {v:?}"
                    );
                    assert!(new_vs == vs, "template values influenced by extending filter {var} with {v:?}");
                });
				fs.iter().for_each(|v| {
                    let mut filters = query.default_filters().clone();
                    filters.insert(
                        var.clone(),
                        TplVarValues::from_refs(
							typ,
                            &(&fs - &BTreeSet::from_iter([*v])),
                        ),
                    );
                    let res = query.run_with_filters(items, types, &filters, &(), None);
					let res_items = res.get_result().unwrap();

                    let new_items = res_items.item_keys().collect::<BTreeSet<_>>();
                    let new_rels = res_items.relation_keys().collect::<BTreeSet<_>>();
                    let new_vs = res.filter_opts().get(var).unwrap().as_refs();

					assert!(
                        new_items.is_subset(&orig_items) && new_rels.is_subset(&orig_rels),
                        "extra items added when shrinking filter {var} with {v:?}"
                    );
                    assert!(
                        (&new_items, &new_rels) != (&orig_items, &orig_rels),
                        "new result not strictly smaller when shrinking filter {var} with {v:?}"
                    );
                    assert!(new_vs == vs, "template values influenced by shrinking filter {var} with {v:?}");
				});

				{
					let mut filters = query.default_filters().clone();
                    filters.remove(var);
                    let res = query.run_with_filters(items, types, &filters,&(),None);
					let res_items = res.get_result().unwrap();

                    let new_items = res_items.item_keys().collect::<BTreeSet<_>>();
                    let new_rels = res_items.relation_keys().collect::<BTreeSet<_>>();
                    let new_vs = res.filter_opts().get(var).unwrap().as_refs();

					if fs == vs {
						assert!(
							(&new_items, &new_rels) == (&orig_items, &orig_rels),
							"result influenced by removing filter {var} containing all options"
						);
					} else {
						assert!(
							new_items.is_superset(&orig_items) && new_rels.is_superset(&orig_rels),
							"extra items added when removing filter {var}"
						);
						// assert!(
						// 	(&new_items, &new_rels) != (&orig_items, &orig_rels),
						// 	"new result not strictly larger when removing filter {var}"
						// );
					}
                    assert!(new_vs == vs, "template values influenced by removing filter {var}");
				}
            }
		}
    });
}
