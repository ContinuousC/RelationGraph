/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    str::FromStr,
};

use relation_graph::{
    serial::{self, ItemInfoArgs},
    Error, ItemId, PackageId, Packages, PropertyValueType, RelationId, TxItems, Types, Updates,
};
use serde_json::json;

fn test_types() -> Types {
    let mut pkgs = Packages::new();
    pkgs.insert(
        PackageId::from_str("test").unwrap(),
        serde_json::from_value(json!({
            "version": "0.1.0",
            "items": {
                "host": {
                    "name": {
                        "singular": "host",
                        "plural": "hosts"
                    },
                    "name_template": "{hostname}",
                    "keys": ["hostname"],
                    "properties": ["hostname", "ip_address"]
                },
                "router": {
                    "name": {
                        "singular": "router",
                        "plural": "routers"
                    },
                    "implements": ["host"],
                    "properties": ["vendor"]
                },
                "switch": {
                    "name": {
                        "singular": "switch",
                        "plural": "switches"
                    },
                    "implements": ["host"],
                    "properties": ["vendor"]
                },
                "interface": {
                    "name": {
                        "singular": "interface",
                        "plural": "interfaces"
                    },
                    "name_template": "{ifname}",
                    "keys": ["ifname"],
                    "parents": ["router", "switch"],
                    "properties": ["ifindex", "ifname"]
                }
            },
            "properties": {
                "hostname":{
                    "name": "Hostname",
                    "value": "string"
                },
                "ip_address": {
                    "name": "IP Address",
                    "value": "string"
                },
                "vendor": {
                    "name": "Vendor",
                    "value": "string"
                },
                "ifindex": {
                    "name": "Index",
                    "value": "integer"
                },
                "ifname": {
                    "name": "Name",
                    "value": "string"
                }
            },
            "relations": {
                "switches-for": {
                    "name": "switches for",
                    "multiplicity": "one-to-many",
                    "bidirectional": false,
                    "source": "switch",
                    "target": "host"
                },
                "is-gateway-for": {
                    "name": "is gateway for",
                    "multiplicity": "one-to-many",
                    "bidirectional": false,
                    "source": "router",
                    "target": "host"
                }
            }
        }))
        .unwrap(),
    );
    pkgs.types().unwrap()
}

const SW1: &str = "95a37704-5dee-11ee-940a-00155d779b71";
const SW2: &str = "a416dd08-5dee-11ee-a1bf-00155d779b71";
const SW3: &str = "dd0fca34-6109-11ee-a04e-00155db22cc5";
const IF0: &str = "aded7350-5dee-11ee-b629-00155d779b71";
const SW1_SW2: &str = "bdb3363a-5dee-11ee-b79f-00155d779b71";
const SW1_SW3: &str = "efcc0656-6109-11ee-bba1-00155db22cc5";

fn item_sw1() -> (ItemId, serial::Item, Vec<String>) {
    (
        ItemId::from_str(SW1).unwrap(),
        serde_json::from_value(json!({
            "item_type": "test/switch",
            "properties": {
                "test/hostname": { "string": "sw1" },
                "test/vendor": { "string": "cisco" }
            }
        }))
        .unwrap(),
        vec![String::from("sw1")],
    )
}

fn item_sw2() -> (ItemId, serial::Item, Vec<String>) {
    (
        ItemId::from_str(SW2).unwrap(),
        serde_json::from_value(json!({
            "item_type": "test/switch",
            "properties": {
                "test/hostname": { "string": "sw2" },
                "test/vendor": { "string": "cisco" }
            }
        }))
        .unwrap(),
        vec![String::from("sw2")],
    )
}

fn item_sw3() -> (ItemId, serial::Item, Vec<String>) {
    (
        ItemId::from_str(SW3).unwrap(),
        serde_json::from_value(json!({
            "item_type": "test/switch",
            "properties": {
                "test/hostname": { "string": "sw3" },
                "test/vendor": { "string": "extreme" }
            }
        }))
        .unwrap(),
        vec![String::from("sw3")],
    )
}

fn item_if0() -> (ItemId, serial::Item, Vec<String>) {
    (
        ItemId::from_str(IF0).unwrap(),
        serde_json::from_value(json!({
            "item_type": "test/interface",
            "parent": SW2,
            "properties": {
                // To be improved: value equality
                //"test/ifindex": 1,
                "test/ifname": { "string": "if0" }
            }
        }))
        .unwrap(),
        vec![String::from("sw2"), String::from("if0")],
    )
}

fn relation_sw1_sw2() -> (RelationId, serial::Relation) {
    (
        RelationId::from_str(SW1_SW2).unwrap(),
        serde_json::from_value(json!({
            "relation_type": "test/switches-for",
            "source": SW1,
            "target": SW2
        }))
        .unwrap(),
    )
}

fn relation_sw1_sw3() -> (RelationId, serial::Relation) {
    (
        RelationId::from_str(SW1_SW3).unwrap(),
        serde_json::from_value(json!({
            "relation_type": "test/switches-for",
            "source": SW1,
            "target": SW3
        }))
        .unwrap(),
    )
}

fn test_items() -> (Types, TxItems, Updates) {
    let types = test_types();
    let mut items = TxItems::new();

    let tx = items.create_transaction();

    let (sw1, value, _) = item_sw1();
    items
        .tx_insert_item(tx.clone(), sw1, value, &types, None)
        .unwrap();

    let (sw2, value, _) = item_sw2();
    items
        .tx_insert_item(tx.clone(), sw2, value, &types, None)
        .unwrap();

    let (if0, value, _) = item_if0();
    items
        .tx_insert_item(tx.clone(), if0, value, &types, None)
        .unwrap();

    let (sw1_sw2, value) = relation_sw1_sw2();
    items
        .tx_insert_relation(tx.clone(), sw1_sw2, value, &types, None)
        .unwrap();

    let updates = items.commit_transaction(tx, &types).unwrap();
    (types, items, updates)
}

#[test]
fn create_items() {
    let (_types, _items, updates) = test_items();
    assert_eq!(
        updates,
        serde_json::from_value({
            let (sw1_id, sw1, sw1_name) = item_sw1();
            let (sw2_id, sw2, sw2_name) = item_sw2();
            let (if0_id, if0, if0_name) = item_if0();
            let (sw1_sw2_id, sw1_sw2) = relation_sw1_sw2();
            json!({
                SW1_SW2: sw1_sw2.into_db(sw1_sw2_id,
                                         ItemInfoArgs::new(
                                             sw1_name.clone(),
                                             sw1.item_type.clone().resolve_opt(None),
                                             Vec::new()
                                         ),
                                         ItemInfoArgs::new(
                                             sw2_name.clone(),
                                             sw2.item_type.clone().resolve_opt(None),
                                             Vec::new()
                                         )),
                SW1: sw1.into_db(sw1_id, vec![], sw1_name),
                SW2: sw2.into_db(sw2_id.clone(), vec![], sw2_name),
                IF0: if0.into_db(if0_id, vec![sw2_id], if0_name),
            })
        })
        .unwrap()
    );
}

#[test]
fn update_item_property() {
    let (types, mut items, _) = test_items();

    let (sw2, mut item_sw2, sw2_name) = item_sw2();
    *item_sw2
        .properties
        .get_mut(&FromStr::from_str("test/vendor").unwrap())
        .unwrap() = PropertyValueType::String
        .value_from_json(json!("extreme"))
        .unwrap();

    let updates = items
        .insert_item(sw2.clone(), item_sw2.clone(), &types, None)
        .unwrap();

    assert_eq!(
        updates,
        serde_json::from_value(json!({
            SW2: item_sw2.into_db(sw2, vec![], sw2_name),
        }))
        .unwrap()
    );
}

#[test]
fn update_item_type() {
    let (types, mut items, _) = test_items();

    let (sw2, mut item_sw2, sw2_name) = item_sw2();
    item_sw2.item_type = FromStr::from_str("test/router").unwrap();

    let updates = items
        .insert_item(sw2.clone(), item_sw2.clone(), &types, None)
        .unwrap();

    assert_eq!(
        updates,
        serde_json::from_value(json!({
            SW2: item_sw2.into_db(sw2, vec![], sw2_name),
        }))
        .unwrap()
    );
}

#[test]
fn update_item_parent() {
    let (types, mut items, _) = test_items();

    let (if0, mut value, _) = item_if0();
    value.parent = Some(ItemId::from_str(SW1).unwrap());

    let updates = items
        .insert_item(if0.clone(), value.clone(), &types, None)
        .unwrap();

    assert_eq!(
        updates,
        serde_json::from_value(json!({
            IF0: value.into_db(if0, vec![ItemId::from_str(SW1).unwrap()],
                               vec![String::from("sw1"), String::from("if0")]),
        }))
        .unwrap()
    );
}

#[test]
fn update_relation_type() {
    let (types, mut items, _) = test_items();

    let (sw1_sw2, mut relation_sw1_sw2) = relation_sw1_sw2();
    relation_sw1_sw2.relation_type = FromStr::from_str("test/is-gateway-for").unwrap();

    let updates = items
        .insert_relation(sw1_sw2.clone(), relation_sw1_sw2.clone(), &types, None)
        .unwrap();

    assert_eq!(
        updates,
        serde_json::from_value({
            let (_, sw1, sw1_name) = item_sw1();
            let (_, sw2, sw2_name) = item_sw2();
            json!({
                SW1_SW2: relation_sw1_sw2.into_db(
                    sw1_sw2,
                    ItemInfoArgs::new(
                        sw1_name,
                        sw1.item_type.resolve_opt(None),
                        Vec::new()
                    ),
                    ItemInfoArgs::new(
                        sw2_name,
                        sw2.item_type.resolve_opt(None),
                        Vec::new()
                    ))
            })
        })
        .unwrap()
    );
}

#[test]
fn invert_relation() {
    let (types, mut items, _) = test_items();

    let (sw1_sw2, mut relation_sw1_sw2) = relation_sw1_sw2();
    std::mem::swap(&mut relation_sw1_sw2.source, &mut relation_sw1_sw2.target);

    let updates = items
        .insert_relation(sw1_sw2.clone(), relation_sw1_sw2.clone(), &types, None)
        .unwrap();

    assert_eq!(
        updates,
        serde_json::from_value({
            let (_, sw1, sw1_name) = item_sw1();
            let (_, sw2, sw2_name) = item_sw2();
            json!({
                SW1_SW2: relation_sw1_sw2.into_db(
                    sw1_sw2,
                    ItemInfoArgs::new(
                        sw2_name,
                        sw2.item_type.resolve_opt(None),
                        Vec::new()
                    ),
                    ItemInfoArgs::new(
                        sw1_name,
                        sw1.item_type.resolve_opt(None),
                        Vec::new()
                    )
                )
            })
        })
        .unwrap()
    );
}

#[test]
fn remove_item() {
    let (types, mut items, _) = test_items();
    let updates = items.remove_item(&ItemId::from_str(SW2).unwrap(), &types);
    assert_eq!(
        updates,
        serde_json::from_value(json!({
            SW2: null,
            IF0: null,
            SW1_SW2: null
        }))
        .unwrap()
    );
}

#[test]
fn remove_relation() {
    let (types, mut items, _) = test_items();
    let updates = items.remove_relation(&RelationId::from_str(SW1_SW2).unwrap(), &types);
    assert_eq!(
        updates,
        serde_json::from_value(json!({
            SW1_SW2: null
        }))
        .unwrap()
    );
}

#[test]
fn bulk_insert() {
    let (types, mut items, _) = test_items();
    let updates = items
        .insert_items(
            serde_json::from_value(json!({
                "roots": [SW1, SW2, SW3],
                "types": {
                    "items": ["test/switch", "test/router"],
                    "relations": ["test/switches-for", "test/is-gateway-for"]
                }
            }))
            .unwrap(),
            serde_json::from_value(json!({
                "items": {
                    SW1: item_sw1().1,
                    SW3: item_sw3().1,
                },
                "relations": {
                    SW1_SW3: relation_sw1_sw3().1
                }
            }))
            .unwrap(),
            &types,
            None,
        )
        .unwrap();
    assert_updates_eq(
        updates,
        serde_json::from_value({
            let (_sw1_id, sw1, sw1_name) = item_sw1();
            let (sw3_id, sw3, sw3_name) = item_sw3();
            let (sw1_sw3_id, sw1_sw3) = relation_sw1_sw3();
            json!({
                SW1_SW3: sw1_sw3.into_db(
                    sw1_sw3_id,
                    ItemInfoArgs::new(
                        sw1_name,
                        sw1.item_type.resolve_opt(None),
                        Vec::new()
                    ),
                    ItemInfoArgs::new(
                        sw3_name.clone(),
                        sw3.item_type.clone().resolve_opt(None),
                        Vec::new()
                    )
                ),
                SW2: null,
                IF0: null,
                SW1_SW2: null,
                SW3: sw3.into_db(sw3_id, vec![], sw3_name),
            })
        })
        .unwrap(),
    )
}

fn assert_updates_eq(actual: Updates, expected: Updates) {
    assert_eq!(
        actual
            .0
            .keys()
            .filter(|key| !expected.0.contains_key(key))
            .collect::<BTreeSet<_>>(),
        BTreeSet::new(),
        "extra keys"
    );
    assert_eq!(
        expected
            .0
            .keys()
            .filter(|key| !actual.0.contains_key(key))
            .collect::<BTreeSet<_>>(),
        BTreeSet::new(),
        "missing keys"
    );
    assert_eq!(
        actual
            .0
            .iter()
            .filter_map(|(key, actual)| {
                let expected = expected.0.get(key)?;
                (actual != expected).then_some((key, (actual, expected)))
            })
            .collect::<BTreeMap<_, _>>(),
        BTreeMap::new(),
        "unexpected values"
    );
}

#[test]
fn concurrent_reads() {
    let (types, mut items, _) = test_items();

    let tx1 = items.create_transaction();
    let tx2 = items.create_transaction();
    let (if0, value, _) = item_if0();

    let v1 = items
        .tx_read_item(tx1.clone(), &if0)
        .unwrap()
        .unwrap()
        .to_serial(None);

    let v2 = items
        .tx_read_item(tx2.clone(), &if0)
        .unwrap()
        .unwrap()
        .to_serial(None);

    assert_eq!(v1, value);
    assert_eq!(v2, value);

    let empty: Updates = serde_json::from_value(json!({})).unwrap();
    assert_eq!(items.commit_transaction(tx1, &types).unwrap(), empty);
    assert_eq!(items.commit_transaction(tx2, &types).unwrap(), empty);
}

#[test]
fn conflicting_writes_on_item() {
    let (types, mut items, _) = test_items();

    let tx1 = items.create_transaction();
    let tx2 = items.create_transaction();
    let (if0, mut value, _) = item_if0();
    *value
        .properties
        .get_mut(&FromStr::from_str("test/ifname").unwrap())
        .unwrap() = PropertyValueType::String
        .value_from_json(json!("ifx"))
        .unwrap();

    items
        .tx_insert_item(tx1.clone(), if0.clone(), value.clone(), &types, None)
        .unwrap();

    items
        .tx_insert_item(tx2.clone(), if0.clone(), value.clone(), &types, None)
        .unwrap();

    assert_eq!(
        items.commit_transaction(tx2, &types).unwrap(),
        serde_json::from_value(json!({
            IF0: value.into_db(if0, vec![ItemId::from_str(SW2).unwrap()],
                               vec![String::from("sw2"), String::from("ifx")])
        }))
        .unwrap()
    );
    assert!(matches!(
        items.commit_transaction(tx1, &types).unwrap_err(),
        Error::RetryTransaction
    ));
}

#[test]
fn conflicting_writes_on_relation_and_item() {
    let (types, mut items, _) = test_items();

    let tx1 = items.create_transaction();
    let tx2 = items.create_transaction();

    let (sw1_sw2, mut rel_value) = relation_sw1_sw2();
    let (sw1, item_value, _) = item_sw1();

    std::mem::swap(&mut rel_value.source, &mut rel_value.target);

    let mut updated_item = item_value.clone();
    *updated_item
        .properties
        .get_mut(&FromStr::from_str("test/hostname").unwrap())
        .unwrap() = PropertyValueType::String
        .value_from_json(json!("sw001"))
        .unwrap();

    items
        .tx_insert_relation(tx1.clone(), sw1_sw2.clone(), rel_value, &types, None)
        .unwrap();

    items
        .tx_insert_item(tx2.clone(), sw1.clone(), updated_item.clone(), &types, None)
        .unwrap();

    let err = items
        .tx_read_item(tx1.clone(), &ItemId::from_str(SW2).unwrap())
        .map(|_| ())
        .unwrap_err();
    assert!(matches!(err, Error::RetryTransaction));

    assert_eq!(
        items.commit_transaction(tx2, &types).unwrap(),
        serde_json::from_value(json!({
            SW1: updated_item.into_db(sw1, vec![], vec![String::from("sw001")])
        }))
        .unwrap()
    );
}

#[test]
fn conflicting_read_write() {
    let (types, mut items, _) = test_items();

    let tx1 = items.create_transaction();
    let tx2 = items.create_transaction();
    let (if0, mut value, _) = item_if0();

    let v = items
        .tx_read_item(tx1.clone(), &if0)
        .unwrap()
        .unwrap()
        .to_serial(None);
    assert_eq!(v, value);

    *value
        .properties
        .get_mut(&FromStr::from_str("test/ifname").unwrap())
        .unwrap() = PropertyValueType::String
        .value_from_json(json!("ifx"))
        .unwrap();

    items
        .tx_insert_item(tx2.clone(), if0.clone(), value.clone(), &types, None)
        .unwrap();

    assert_eq!(
        items.commit_transaction(tx2, &types).unwrap(),
        serde_json::from_value(json!({
            IF0: value.into_db(if0, vec![ItemId::from_str(SW2).unwrap()],
                               vec![String::from("sw2"), String::from("ifx")])
        }))
        .unwrap()
    );
    assert!(matches!(
        items.commit_transaction(tx1, &types).unwrap_err(),
        Error::RetryTransaction
    ));
}

#[test]
fn conflicting_write_read() {
    let (types, mut items, _) = test_items();

    let tx1 = items.create_transaction();
    let tx2 = items.create_transaction();
    let (if0, value, _) = item_if0();

    let mut updated = value.clone();
    *updated
        .properties
        .get_mut(&FromStr::from_str("test/ifname").unwrap())
        .unwrap() = PropertyValueType::String
        .value_from_json(json!("ifx"))
        .unwrap();

    items
        .tx_insert_item(tx1.clone(), if0.clone(), updated.clone(), &types, None)
        .unwrap();

    let v = items
        .tx_read_item(tx2.clone(), &if0)
        .unwrap()
        .unwrap()
        .to_serial(None);
    assert_eq!(v, value);

    assert_eq!(
        items.commit_transaction(tx2, &types).unwrap(),
        serde_json::from_value(json!({})).unwrap()
    );
    assert!(matches!(
        items.commit_transaction(tx1, &types).unwrap_err(),
        Error::RetryTransaction
    ));
}

#[test]
fn query_while_writing() {
    let (types, mut items, _) = test_items();

    let tx = items.create_transaction();

    let id = ItemId::new();
    items
        .tx_insert_item(
            tx.clone(),
            id.clone(),
            serde_json::from_value(json!({
                "item_type": "test/switch",
                "properties": {
                    "test/hostname": { "string": "sw5" },
                    "test/vendor": { "string": "service integrators" }
                }
            }))
            .unwrap(),
            &types,
            None,
        )
        .unwrap();

    let query = serde_json::from_value::<serial::Query>(json!({
        "root": "switches",
        "elements": {
            "switches": {
                "items": {
                    "match": {
                        "item_type": {
                            "in": [
                                "test/switch"
                            ]
                        }
                    }
                }
            }
        }
    }))
    .unwrap()
    .resolve(&types, None)
    .unwrap();

    // Should not panic.
    let _res = query.run(&items.items, &types);
}
