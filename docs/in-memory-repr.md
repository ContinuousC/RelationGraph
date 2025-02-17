# In-memory representation

After loading packages or data from files or from the database, it is
transformed into a separate in-memory representation. This is done to allow
verifying and pre-resolving references between objects, as well as to save
backreferences in the structure.

## Reference handling

The internally developed `graph` crate defines a `Graph<T>` structure that
contains a `typed_arena::Arena` of `Option<T>` and a unique identifier `Gen`.
When a node is added to the graph, a clonable `Ref<T>` is returned, containing a
pointer to the node (`NonNull<Option<T>>`) and a copy of the graph's `Gen`. This
reference can be used for efficient lookup of the node in the graph. Memory
safety is guaranteed by verifying on lookup that the reference's generation
corresponds to the one of the graph (i.e. that the reference points to a node
that belongs to the graph).

The `Gen` currently wraps a `u64` taken from an atomic counter, with `0` reserved
for the "invalid" generation. It could alternatively be implemented as a random
number (e.g. using a `Uuid`) to avoid the atomic operation, at the cost of
requiring a larger size to minimize the risk of collisions. Note that uniqueness
of the generation number is crucial to ensure memory safety.

The graph supports insertion (`insert`, `promise` and `create`), lookup
(`borrow`, `borrow_mut` and `borrow_many_mut`), removal (`remove`) and mutable
iteration `iter_mut`. Promising a node returns a reference to a new empty slot
in the graph, that can subsequantly be filled through the `create` method. This
is useful when constructing the graph. Removal is implemented by leaving the
slot empty. On lookup, the `Option<T>` in the slot is unwrapped to return a
reference to the node `T`.

### Panic freedom

From the above it should be clear that, while `Graph` intends to guarantee
memory safety when used through its public API, it does not provide panic
safety. It is the application's responsibility to ensure logical correctness of
its operations on the graph. To help in this endeavour, the Relation Graph
Engine runs verification functions on its structures after each operation when
compiled in debug mode.

When nodes are removed from the graph, memory is not reclaimed. To free up the
memory, it would be necessary to implement and periodically execute a garbage
collection step that rebuilds new graph structures from the old ones,
re-resolving all references.

## Indexed graphs

In addition to the `Graph` type, the `graph` crate defines indexed graph types
`BTreeGraph` and `HashGraph`, containing a `BTreeMap` or a `HashMap` index,
respectively. The indexed graph types require a key on insertion and removal and
additionally support lookup by key (`get`, `get_key_value`, `get_mut`,
`get_key_value_mut`, `entry`) and shared iteration (`iter_ref`, `iter_ref_by`,
`iter`, `keys`, `values_ref`, `values`). In addition to `Ref`, their methods
accept the `RefBy<K, V>` type, containing an owned key and a `Ref<V>`.

The Relation Graph Engine stores its objects in indexed graphs and maintains
links between objects through keyed references. In microbenchmarks, lookups
through the graph `Ref` types was found to be about ten times faster than
`HashMap` or `BTreeMap` lookups.

## Self-referencing graphs

Two methods can be used to construct self-referencing graphs. The first one is
to pre-allocate nodes through the `promise` method, and provide their value
later through a call to `create`. This allows resolving references to the
promised nodes before their actual value is stored in the graph.

The second method is to insert nodes with "invalid" references (constructed
using `Ref::dangling()` or `RefBy::dangling()`) first, and resolve these
references afterwards.

## (De)serialization and reference resolution

The indexed graph types support serialization and deserialization through
`serde`, but references in the nodes will be dangling after deserialization.
Deserialization must be followed by a manual resolution of the references. It
might be possible to automate this step in the future using a derive macro for a
`Resolve` trait that would traverse the structure and fallibly resolve any
references using the corresponding entry in a passed-in typemap of graphs.

In the Relation Graph Engine, the structures are resolved to separate types that
additionally resolve relative references to absolute references, and that
include backreferences for use in queries and other request handlers.

An interesting excercise would be to try to remove the separation between serial
and resolved types, which would most propbably improve serialization and
deserializion efficiency. In addition to deserializing with dangling references
as explained above, skipping backreference fields in serialization and
deserialization (filling in default values as placeholders) and resolving these
references and backreferences in the resolve step, this would involve unifying
the representation of relative and absolute ids (or removing support for
relative ids altogether).
