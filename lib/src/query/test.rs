/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    str::FromStr,
};

use serde_json::json;

use crate::{
    ids::PackageId,
    items::{Items, ItemsSerial},
    package::Packages,
    query::serial,
    types::Types,
};

static BUILTIN: &str = include_str!("../../../tests/networking/pkgs/builtin.json");
static NETWORKING: &str = include_str!("../../../tests/networking/pkgs/networking.json");
static ITEMS: &str = include_str!("../../../tests/networking/items.json");

fn test_data() -> (Types, Items) {
    let mut pkgs = Packages::new();
    pkgs.insert(
        PackageId::from_str("builtin").unwrap(),
        serde_json::from_str(BUILTIN).unwrap(),
    );
    pkgs.insert(
        PackageId::from_str("networking").unwrap(),
        serde_json::from_str(NETWORKING).unwrap(),
    );
    let types = pkgs.types().unwrap();
    let items = serde_json::from_str::<ItemsSerial>(ITEMS)
        .unwrap()
        .resolve(&types, None)
        .unwrap();
    (types, items)
}

// #[test]
// fn select_items() {
//     let (types, items) = test_data();
//     let selector = serde_json::from_value::<serial::ItemSelector>(json!({
//         "match": {
//             "item_type": { "is": "builtin/host" },
//             "properties": {
//                 "match": {
//                     "builtin/hostname": { "in": [ "mndev02", "mnvmdev02", "router1" ] }
//                 }
//             },
//             "relations": {
//                 "match": {
//                     "networking/layer2": {
//                         "match": {
//                             "item_type": { "is": "networking/switch" },
//                             "properties": {
//                                 "match": {
//                                     "networking/vendor": { "equals": "cisco" }
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//     }))
//     .unwrap()
//     .resolve(&types, &BTreeMap::new(), None)
//     .unwrap();

//     let res = selector
//         .prefilter(&items, &types)
//         .filter(|(_, item)| {
//             selector.matches(items.items.borrow(item), &items, &types, &BTreeMap::new())
//         })
//         .map(|(k, _)| k.to_string())
//         .collect::<BTreeSet<_>>();

//     assert_eq!(
//         res,
//         BTreeSet::from_iter([
//             String::from("db3a96ee-3dcd-11ee-b711-2fda1b3f5b67"),
//             String::from("8e3d6068-3dcf-11ee-a9a5-97dad9b31119")
//         ])
//     );
// }

#[test]
fn query() {
    let (types, items) = test_data();
    let query = serde_json::from_value::<serial::Query>(json!({
        "root": "root",
        "elements": {
            "root": {
                "items": { "match": {
                    "item_type": { "is": "builtin/host" },
                    "properties": { "match": {
                        "builtin/hostname": { "all": [
                            { "in": ["mndev02", "mnvmdev02"] },
                            { "template": "host" }
                        ]}
                    }}
                }},
                "follow": [{
                    "relation": { "match": {
                        "relation_type": { "is": "networking/layer2" }
                    }},
                    "element": "netw"
                }]
            },
            "netw": {
                "items": { "match": {
                    "item_type": { "in": [ "networking/switch",
                                            "networking/router" ]},
                    "properties": { "match": {
                        "networking/vendor": { "template": "vendor" }
                    }}
                }},
                "follow": [{
                    "relation": { "match": {
                        "relation_type": { "is": "networking/layer2" }
                    }},
                    "element": "netw"
                }]
            }
        },
        "template": {
            "host": "strings",
            "vendor": "strings"
        },
        "filters": {
            "host": [ "mndev02" ],
            //"vendor": [ "cisco" ],
        }
    }))
    .unwrap()
    .resolve(&types, None)
    .unwrap();

    let res = query.run(&items, &types);

    // panic!("{}", serde_json::to_string_pretty(&res).unwrap());

    let ids = res
        .items
        .keys()
        .map(|k| k.to_string())
        .collect::<BTreeSet<_>>();
    assert_eq!(
        ids,
        BTreeSet::from_iter([
            String::from("db3a96ee-3dcd-11ee-b711-2fda1b3f5b67"),
            String::from("8cbb2c9e-3dd8-11ee-827d-00155dd6f13c"),
            String::from("8e3d6068-3dcf-11ee-a9a5-97dad9b31119"),
        ])
    );

    let tpl = res
        .template
        .iter()
        .map(|(k, vs)| {
            (
                k.to_string(),
                vs.strings()
                    .unwrap()
                    .0
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<BTreeSet<_>>(),
            )
        })
        .collect::<BTreeMap<_, _>>();
    assert_eq!(
        tpl,
        BTreeMap::from_iter([
            (
                String::from("host"),
                BTreeSet::from_iter([String::from("mndev02"), String::from("mnvmdev02")])
            ),
            (
                String::from("vendor"),
                BTreeSet::from_iter([String::from("cisco"), String::from("fortinet")])
            )
        ])
    );
}
