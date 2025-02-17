# Data Model #

The relation graph consists of items and relations identified by
UUIDs. Each item has an item type and an optional parent item and a
set of properties. Each relations has a relation type, a source item,
a target item and a set of properties. Available item types, relation
types and property types, as well as the connections between them, are
defined by packages.

## Types: packages ##

Packages contain type information for the relation graph. They are identified by
a unique package name and a semver version number. They can be represented in
JSON format, in a file named `package-name.json`. The rust types for the
serialized version of the packages are defines in `lib/src/types/serial.rs`.

### Dependencies ###

The `version` key specifies the semver version of the package. A
package can depend on other packages through the `requires` key, which
contains a map of package ids to semver requirements. Only elements
defined by packages specified in `requires` can be referenced in the
package.

A package can also depend on prometheus schemas through the
`prometheus_requires` key. Only schemas present in this map can be
used for linking metrics to item types.

### References ###

References to item types, relation types and property types in the
package are relative to the current package. This means they can
either be local when referring to an element in the current package,
specifying only the name of the element (e.g. `host`) or absolute,
specifying the package name and the name of the element, separated by
a slash (e.g. `network/host`).

Outside of the package definition, references will either be absolute
(always requiring a package name to be specified) or resolved relative
to a specified package.

Example:

```json
{
	"version": "0.1.0",
	"requires": {
		"network": "^0.1"
	},
	"prometheus_requires": {},
	"items" {},
	"relations": {},
	"properties": {}
}
```

### Item Types ###

The `items` key of a package should contain a map of item type names
to the package's item types. Each item type has a `name`, specifying
both the `singular` and the `plural` of the item type's name. The
`name_template` specifies how the (human-readable) name for instances
of the item type should be generated. The `properties` key contains
the set of available properties for the item.

The `parents` key contains a set of allowable parent types. When
unset, an item of this type cannot have a parent (i.e. it is a
root). When set, an item of this type must have exactly one parent of
an allowable parent type (or any of their descendent types). This
parent / child relationship defines a strict hierarchy. Together with
the `keys` key, containing a set of property names, this is used for
identification of items. The set of keys for an item should be unique
under its parent.

An item type can optionally specify a set of parent item types it
`implements`, allowing us to define an item type hierarchy. An item of
a certain type is considered to also be an instance of all the
ancestor types. The item type inherits the properties of all the item
types it implements, while keys, and allowable parents are inherited
by default, but overridden when set in the type definition.

The `prometheus_metrics` key specifies how metrics can be linked to
items of this type.

Example:

```json
{
	"name": {
		"singular": "Host",
		"plural": "Hosts"
	},
	"description": "A host on the network",
	"name_template": "{hostname} ({ip_addr})",
	"keys": ["hostname"],
	"properties": ["hostname", "ip_addr"],
}
```

### Relation Types ###

The package's `relation` key contains a map of relation type ids to
relation type definitions. A relation type has a `name`, an optional
`description`, a `multiplicity` (`one-to-one` or `one-to-many`), a
`bidirectional` flag (defaulting to false), a `source` type, a
`target` type and a set of allowed `properties`. It can also specify
`prometheus_metrics` and a `connector` definition for declarative,
rule-based relation generation.

A relation, instance of a relation type, connects one an item of the
source type (or a descendant type) to an item of the target type (or a
descendant type). If the relation's multiplicity is "one-to-many",
multiple relations of the same type are allowed to start from one
item.

In contrast to item types, there is currently no relation type hierarchy.

Example:

```json
{
	"name": "route",
	"description": "This hosts routes traffic to another host.",
	"multiplicity": "one-to-many",
	"source": "host",
	"target": "host"
}
```

### Property Types ###

The `properties` key of the package, finally, contains a map of
property ids to property type definitions. The property definition
includes the property's `name`, an optional `description` and a
`value` type. Available types are:

- `string`: a unicode string
- `integer`: a signed 64-bit integer
- `float`: a double precision (64-bit) float
- `time`: a timestamp
- `list<T>`: a list of values of a given property type
- `map<T>`: a map of strings to a given property type

## Types: Connection Packages

Connection packages define the link between discovery objects and external data
such as prometheus metrics. (This can include other types of metrics, logs, etc.
in the future.) Connection package structures are defined in
`lib/src/types/connections.rs`. Analogous to discovery packages, a connection
package contains a semver version number (`version`) and a map of dependencies
to semver requirements (`requires`). The actual connections are specified in the
`items` and `relations` fields.

The `items` and `relations` fields contain a mapping from absolute discovery
item or relation ids, respectively, to a connection specification. The
connection specification currently only has a `prometheus` field, containing a
mapping of qualified item names in the prometheus schema to an `ItemMetrics` or
`RelationMetrics` structure that specifies a mapping of prometheus label names
to a selector on the relation graph structure.

The following example creates a connection between the
"kubernetes/cluster"discovery item type and the
"kubelet-metrics-cadvisor:systemNamespace" prometheus item with its
corresponding metrics. The connection is made by setting the prometheus label
"cluster" to the value of the "name" property on the "cluster" discovery item.

```json
{
  "version": "0.1.0",
  "requires": {
    "discovery": {
      "kubernetes": "^0.1"
    },
    "prometheus": {
      "kubelet-metrics-cadvisor": "^0.1"
    }
  },
  "items": {
    "kubernetes/cluster": {
      "prometheus": {
        "kubelet-metrics-cadvisor:systemNamespace": {
          "group_by": null,
          "keys": {
            "cluster": {
              "property": "kubernetes/name"
            }
          }
        }
      }
    }
  }
}
```

## Types: declarative relations

Declarative relations, defined in the discovery packages, instruct the Relation
Graph Engine to create certain relations based on rules, rather than relying on
relations provided by the discovery services explicitly. Relations of types with
a connector defined, should not be generated by discovery services.

The following connector definition instructs the Relations Graph Engine to
automatically create relations from `jaeger/service` to `kubernetes/container`
by matching the `k8s_container_name` and `k8s_pod_uid` properties on the service
to the `name` property on the container and the `uid` property on the
container's parent `pod`, repectively.

```json
{
  "groups": {
    "container": {
      "rules": [
        {
          "template": {
            "podUid": "strings",
            "containerName": "strings"
          },
          "source": {
            "match": {
              "item_type": {
                "is": "jaeger/service"
              },
              "properties": {
                "k8s_pod_uid": {
                  "string": {
                    "template": "podUid"
                  }
                },
                "k8s_container_name": {
                  "string": {
                    "template": "containerName"
                  }
                }
              }
            }
          },
          "target": {
            "match": {
              "item_type": {
                "is": "kubernetes/container"
              },
              "parent": {
                "match": {
                  "item_type": {
                    "is": "kubernetes/pod"
                  },
                  "properties": {
                    "kubernetes/uid": {
                      "string": {
                        "template": "podUid"
                      }
                    }
                  }
                }
              },
              "properties": {
                "kubernetes/name": {
                  "string": {
                    "template": "containerName"
                  }
                }
              }
            }
          }
        }
      ]
    }
  }
}
```

See also: [analysis document](Declarative%20relations.pptx)

## Types: prometheus metrics schema

See [analysis document](Prometheus%20Metrics%20Schema.pptx)

## Data: items and relations

The actual discovery data is expressed as instances of items and relations that
comply with the item type and relation type definitions given in the package
files. The rust types corresponding to the serialized (file) form of this data
are defined in `lib/src/items/serial.rs`. A separate database format, used for
saving into Elasticsearch / Opensearch, is defined in `lib/src/items/db.rs`.

### Identification

Items and relations are identified by Universally Unique Identifiers. Discovery
services are responsible for assigning these identifiers, separating this
concern from the data model logic implemented in the Relation Graph Engine. This
allows, for example, to make identification configurable for specific
sub-domains. Even though the combination of keys specified in the item type
definition is supposed to be unique, this is currently not enforced.

In the source code, correct use of items ids and relation ids is enforced by
using newtype wrappers `ItemId` and `RelationId` around `Uuid`.

### Items

An Item instance contains the `item_type` id (optionally relative to a
externally specified current package), an optional `parent` item id and a map of
`properties`, limited to the properties for the item type and the item types it
implements, as specified in the package.

Example:

```json
{
  "item_type": "host",
  "parent": "a7824e4a-da3a-11ef-a398-1fe5f3536220",
  "properties": {
    "hostname": "gateway",
    "ip_addr": "192.168.0.1"
  }
}
```

### Relations

A Relation instance contains the `relation_type` id, the `source` item id, the
`target` item id and a map of `properties`, limited to the properties for the
relation type as specified in the package.

Example:

```json
{
  "relation_type": "route",
  "source": "2a5cf9f0-da45-11ef-b1b7-fbbdce09e20e",
  "target": "fbea6dd2-da44-11ef-8123-17b59deb2fb5",
  "properties": { }
}
```

### Database representation

In the database, items and relations are saved to a single index. With uuids
being used for item and relation ids, both can be unified into a single object
id for use in the database, with negligible risk of collisions (as long as the
ids are randomly generated). The object value identifies whether we are dealing
with an item or a relation variant.

In addition to the keys for the serialized form of items and relations, the
database representation contains extra information indexed by the database, to
be used in queries. Most of these fields are not allowed to change during the
life-cycle of an object (see comments in the code; i.e. if a discovery service
intends to changce them, it should instead remove the item and create a new item
with a new id).

An example of the use of the index fields, is to query for objects containing a
specific item id in their `parents` field, allowing to retrieve a full subtree
from the database in one request.

Currently the `item_name` fields, calculated from the item name templates
specified in the package, are the only fields in `ItemInfo` that can change
during the life of an object. It is not automatically updated in child items
when a parent's source's or target's name changes.

The database representation is defined as follows:

```rust
pub struct DbItem {
    #[serde(flatten)]
    pub entity: EntityInfo,
    pub parent: Option<ItemId>,
    #[serde(deserialize_with = "deserialize_properties")]
    pub properties: BTreeMap<PackageId, BTreeMap<PropertyId, serde_json::Value>>,
}

pub enum EntityInfo {
    Item {
        // See notes in ItemInfo
        #[serde(flatten)]
        item: ItemInfo,
    },
    Relation {
        // Immutable
        #[serde(flatten)]
        relation: RelationInfo,
        // Source id cannot change, but see notes in ItemInfo
        source: ItemInfo,
        // Target id cannot change, but see notes in ItemInfo
        target: ItemInfo,
    },
}

pub struct ItemInfo {
    // Immutable
    pub item_id: ItemId,
    // Immutable
    pub item_type: Absolute<ItemTypeId>,
    // Immutable
    //#[serde(default)]
    pub parents: Vec<ItemId>,
    // Informational: item name when object was last updated
    //#[serde(default)]
    pub item_name: Vec<String>,
}

pub struct RelationInfo {
    // Immutable
    pub relation_id: RelationId,
    // Immutable
    pub relation_type: Absolute<RelationTypeId>,
}
```

Example item:

```json
{
  "type": "item",
  "item_id": "fbea6dd2-da44-11ef-8123-17b59deb2fb5",
  "item_type": "networking/host",
  "parents": ["a7824e4a-da3a-11ef-a398-1fe5f3536220"],
  "item_name": ["gateway (192.168.0.1)"],
  "parent": "a7824e4a-da3a-11ef-a398-1fe5f3536220",
  "properties": {
    "hostname": "gateway",
    "ip_addr": "192.168.0.1"
  }
}
```

Example relation:

```json
{
  "type": "relation",
  "relation_id": "72d19f24-da45-11ef-829f-5b04f99b2c16",
  "relation_type": "networking/route",
  "source": {
    "item_id": "2a5cf9f0-da45-11ef-b1b7-fbbdce09e20e",
    "item_type": "networking/host",
    "parents": ["a7824e4a-da3a-11ef-a398-1fe5f3536220"],
    "item_name": ["host (192.168.0.123)"]
  },
  "target": {
    "item_id": "fbea6dd2-da44-11ef-8123-17b59deb2fb5",
    "item_type": "networking/host",
    "parents": ["a7824e4a-da3a-11ef-a398-1fe5f3536220"],
    "item_name": ["gateway (192.168.0.1)"]
  },
  "properties": { }
}
```
