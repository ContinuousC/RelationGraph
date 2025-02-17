/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::hash_map::RandomState;
use std::collections::{BTreeMap, BTreeSet, HashMap};

use dbschema::ObjectId;
use itertools::Itertools;
use serde::{Deserialize, Serialize};

use graph::{HashGraph, IndexBy, OptRefMap, Ref, RefBy, RefMap};
use wrapper::{Identity, Wrapped, Wrapper};

use crate::error::{Error, Result};
use crate::ids::{
    Absolute, ItemId, ItemInfo, ItemTypeId, PackageId, PropertyId, RelationId, RelationInfo,
    RelationTypeId,
};
use crate::query::resolved::Endpoint;
use crate::types::resolved::EntityTypeRef;
use crate::{EntityId, EntityInfo, ItemType, RelationType, TransactionId, Types};

use super::db::DbItems;
use super::serial::PropertyValue;
use super::transaction::{self, Change};
use super::{db::DbItem, serial};

pub struct Items<W: Wrapper = Identity> {
    pub(crate) items: HashGraph<ItemId, WrappedItem<W>, RandomState>,
    pub(crate) relations: HashGraph<RelationId, WrappedRelation<W>, RandomState>,
    pub(crate) by_type: ItemTypeIndex<W>,
}

pub type WrappedItem<W> = <W as Wrapper>::Wrap<Item<W>>;
pub type WrappedRelation<W> = <W as Wrapper>::Wrap<Relation<W>>;
pub type ItemRef<W> = Ref<WrappedItem<W>>;
pub type RelationRef<W> = Ref<WrappedRelation<W>>;
pub type NamedItemRef<W> = RefBy<ItemId, WrappedItem<W>>;
//pub type NamedRelationRef<W> = RefBy<RelationId, WrappedRelation<W>>;
pub type ItemRefMap<W> = RefMap<ItemId, WrappedItem<W>>;
pub type RelationRefMap<W> = RefMap<RelationId, WrappedRelation<W>>;

pub type ItemTypeIndex<W> = BTreeMap<Absolute<ItemTypeId>, ItemRefMap<W>>;
pub type RelationTypeIndex<W> = BTreeMap<Absolute<RelationTypeId>, RelationRefMap<W>>;

pub struct TxItems {
    pub items: Items<Txs>,
    pub(crate) transactions: HashGraph<TransactionId, Option<Transaction>>,
}

#[derive(Debug)]
pub struct Txs;

impl Wrapper for Txs {
    type Wrap<T> = Tx<T>;
}

// Conditions for mutable access and map?
impl<T> wrapper::Wrapped<Txs, T> for Tx<T> {
    fn get_wrapped(&self) -> &T {
        self.current().unwrap()
    }

    fn try_get_wrapped(&self) -> Option<&T> {
        self.current()
    }

    fn get_wrapped_mut(&mut self) -> &mut T {
        self.committed_mut()
    }

    fn unwrap_wrapped(self) -> T {
        self.unwrap_committed()
    }

    fn map_wrapped<F, U>(self, f: F) -> Tx<U>
    where
        F: FnOnce(T) -> U,
    {
        Tx::new_committed(f(self.unwrap_committed()))
    }

    fn try_map_wrapped<F, U, E>(self, f: F) -> std::result::Result<Tx<U>, E>
    where
        F: FnOnce(T) -> std::result::Result<U, E>,
    {
        Ok(Tx::new_committed(f(self.unwrap_committed())?))
    }
}

pub(crate) type TxRef = transaction::TxRef<TransactionId, Transaction>;
pub type Tx<T> = transaction::Tx<T, TransactionId, Transaction>;
type TxMap<K, V> = transaction::TxMap<K, V, TransactionId, Transaction>;
type TxRead<'a, T> = transaction::TxRead<'a, T, TransactionId, Transaction>;
type TxWrite<'a, T> = transaction::TxWrite<'a, T, TransactionId, Transaction>;

pub struct Transaction {
    items: TxMap<ItemId, Item<Txs>>,
    relations: TxMap<RelationId, Relation<Txs>>,
}

pub enum EntityRef<'a, W: Wrapper = Identity> {
    Item(&'a Item<W>),
    Relation(&'a Relation<W>),
}

#[derive(PartialEq)]
pub struct Item<W: Wrapper = Identity> {
    pub(crate) item_type: RefBy<Absolute<ItemTypeId>, ItemType>,
    pub(crate) parent: Option<NamedItemRef<W>>,
    pub(crate) children: ItemRefMap<W>,
    pub(crate) properties: BTreeMap<Absolute<PropertyId>, PropertyValue>,
    pub(crate) source_of: RelationTypeIndex<W>,
    pub(crate) target_of: RelationTypeIndex<W>,
}

impl<W: Wrapper> Clone for Item<W> {
    fn clone(&self) -> Self {
        Self {
            item_type: self.item_type.clone(),
            parent: self.parent.clone(),
            children: self.children.clone(),
            properties: self.properties.clone(),
            source_of: self.source_of.clone(),
            target_of: self.target_of.clone(),
        }
    }
}

#[derive(PartialEq)]
pub struct Relation<W: Wrapper = Identity> {
    pub(crate) relation_type: RefBy<Absolute<RelationTypeId>, RelationType>,
    pub(crate) properties: BTreeMap<Absolute<PropertyId>, PropertyValue>,
    pub(crate) source: NamedItemRef<W>,
    pub(crate) target: NamedItemRef<W>,
}

impl<W: Wrapper> Clone for Relation<W> {
    fn clone(&self) -> Self {
        Self {
            relation_type: self.relation_type.clone(),
            properties: self.properties.clone(),
            source: self.source.clone(),
            target: self.target.clone(),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct Updates(pub HashMap<ObjectId, Option<super::db::DbItem>>);

pub(crate) struct Domain<W: Wrapper = Identity> {
    roots: Option<OptRefMap<ItemId, WrappedItem<W>>>,
    types: TypeSet,
}

#[derive(Default)]
struct TypeSet {
    items: RefMap<Absolute<ItemTypeId>, ItemType>,
    relations: RefMap<Absolute<RelationTypeId>, RelationType>,
}

#[derive(Default)]
pub struct ItemMap<W: Wrapper = Identity> {
    pub(crate) items: HashMap<ItemId, Item<W>>,
    pub(crate) relations: HashMap<RelationId, Relation<W>>,
}

pub struct ItemsRefMap<W: Wrapper = Identity> {
    pub(crate) items: ItemRefMap<W>,
    pub(crate) relations: RelationRefMap<W>,
}

impl<W: Wrapper> Default for ItemsRefMap<W> {
    fn default() -> Self {
        Self {
            items: Default::default(),
            relations: Default::default(),
        }
    }
}

type NewParent<W> = (NamedItemRef<W>, ItemId, ItemRef<W>);

enum Preserve<'a> {
    None,
    Item(&'a ItemId),
    Relation(&'a RelationId),
}

impl TxItems {
    pub fn new() -> Self {
        Self {
            items: Items::new(),
            transactions: HashGraph::with_hasher(RandomState::default()),
        }
    }

    pub(super) fn from_serial(
        serial: serial::Items,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self {
            items: Items::from_serial_tx(serial, types, pkg)?,
            transactions: HashGraph::new(),
        })
    }

    pub(super) fn from_db(db: DbItems, types: &Types) -> Result<Self> {
        Ok(Self {
            items: Items::from_db_tx(db, types)?,
            transactions: HashGraph::new(),
        })
    }

    pub(super) fn resolve(&mut self) -> Result<()> {
        self.items.resolve()?;
        #[cfg(debug_assertions)]
        self.verify(None);
        Ok(())
    }

    pub fn get_entity(&self, id: &EntityId) -> Option<EntityRef<Txs>> {
        match id {
            EntityId::Item(item_id) => Some(EntityRef::Item(self.get_item(item_id)?)),
            EntityId::Relation(rel_id) => Some(EntityRef::Relation(self.get_relation(rel_id)?)),
        }
    }

    pub fn get_item(&self, id: &ItemId) -> Option<&Item<Txs>> {
        self.items.items.get(id)?.current()
    }

    pub fn get_relation(&self, id: &RelationId) -> Option<&Relation<Txs>> {
        self.items.relations.get(id)?.current()
    }

    // pub(crate) fn borrow_item<T: AsRef<Ref<Tx<Item<Txs>>>>>(&self, item_ref: &T) -> &Item<Txs> {
    //     self.items.items.borrow(item_ref).current().unwrap()
    // }

    // pub(crate) fn borrow_relation<T: AsRef<Ref<Tx<Relation<Txs>>>>>(
    //     &self,
    //     rel_ref: &T,
    // ) -> &Relation<Txs> {
    //     self.items.relations.borrow(rel_ref).current().unwrap()
    // }

    pub fn iter_items(&self) -> impl Iterator<Item = (&ItemId, &Item<Txs>)> {
        self.items
            .items
            .iter()
            .filter_map(|(id, item)| Some((id, item.current()?)))
    }

    pub fn iter_relations(&self) -> impl Iterator<Item = (&RelationId, &Relation<Txs>)> {
        self.items
            .relations
            .iter()
            .filter_map(|(id, rel)| Some((id, rel.current()?)))
    }

    pub fn iter_items_by_type(
        &self,
        item_type: &Absolute<ItemTypeId>,
    ) -> impl Iterator<Item = (&ItemId, &Item<Txs>)> {
        self.items
            .iter_items_by_type(item_type)
            .filter_map(|(id, item)| Some((id, item.current()?)))
    }

    pub fn create_transaction(&mut self) -> TransactionId {
        self.create_transaction_internal().key().clone()
    }

    pub fn commit_transaction(&mut self, id: TransactionId, types: &Types) -> Result<Updates> {
        let tx = self.get_transaction_ref(id)?;
        Ok(self.commit_transaction_internal(&tx, types))
    }

    pub fn abort_transaction(&mut self, id: TransactionId) {
        if let Ok(tx) = self.get_transaction_ref(id) {
            self.abort_transaction_internal(&tx, Preserve::None);
        }
    }

    /// Create a new transaction.
    fn create_transaction_internal(&mut self) -> TxRef {
        let id = TransactionId::new();
        let tx = self
            .transactions
            .insert(id.clone(), Some(Transaction::new()));
        RefBy::new(id, tx)
    }

    /// Retrieve transaction.
    fn get_transaction_ref(&mut self, id: TransactionId) -> Result<TxRef> {
        let tx = self
            .transactions
            .get_ref(&id)
            .ok_or(Error::UnknownTransaction)?;
        if self.transactions.borrow(tx).is_some() {
            Ok(RefBy::new(id, tx.clone()))
        } else {
            self.transactions.remove(&id);
            Err(Error::RetryTransaction)
        }
    }

    /// Abort a transaction that conflicts with the current
    /// transaction and set the transaction object to None. The next
    /// call to get_transaction for the id will return
    /// Error::RetryTransaction and remove the transaction object.
    fn abort_transaction_internal(&mut self, tx_ref: &TxRef, preserve: Preserve) {
        let tx = self.transactions.borrow_mut(tx_ref).take().unwrap();
        tx.abort(tx_ref, &mut self.items, preserve);
        #[cfg(debug_assertions)]
        self.verify(None);
    }

    /// Commit a transaction and remove the transaction object.
    fn commit_transaction_internal(&mut self, tx_ref: &TxRef, types: &Types) -> Updates {
        let tx = self.transactions.remove(tx_ref.key()).unwrap().unwrap();
        let updates = tx.commit(tx_ref, &mut self.items, types);
        #[cfg(debug_assertions)]
        self.verify(None);
        updates
    }

    /// Get or create an items transaction object.
    fn get_item_tx(&mut self, id: &ItemId) -> Ref<Tx<Item<Txs>>> {
        self.items
            .items
            .get_ref(id)
            .cloned()
            .unwrap_or_else(|| self.items.items.insert(id.clone(), Tx::new_empty()))
    }

    /// Get or create an items transaction object.
    fn get_relation_tx(&mut self, id: &RelationId) -> Ref<Tx<Relation<Txs>>> {
        self.items
            .relations
            .get_ref(id)
            .cloned()
            .unwrap_or_else(|| self.items.relations.insert(id.clone(), Tx::new_empty()))
    }

    fn read_item_tx(
        &mut self,
        tx: &TxRef,
        item_id: &ItemId,
        item_ref: &Ref<Tx<Item<Txs>>>,
    ) -> TxRead<Item<Txs>> {
        if let Some(txi) = self.items.items.borrow(item_ref).conflicting_for_read(tx) {
            self.abort_transaction_internal(&txi, Preserve::Item(item_id));
        }

        self.transactions
            .borrow_mut(tx)
            .as_mut()
            .unwrap()
            .items
            .insert(item_id.clone(), item_ref.clone());

        self.items.items.borrow_mut(item_ref).read(tx)
    }

    /// Get or create an items transaction object for writing,
    /// aborting any conflicting transactions.
    fn write_item_tx(
        &mut self,
        tx: &TxRef,
        item_id: &ItemId,
        item_ref: &Ref<Tx<Item<Txs>>>,
    ) -> TxWrite<Item<Txs>> {
        if let Some(txs) = self.items.items.borrow(&item_ref).conflicting_for_write(tx) {
            txs.into_iter().for_each(|(id, tx)| {
                self.abort_transaction_internal(&RefBy::new(id, tx), Preserve::Item(item_id))
            });
        }
        self.transactions
            .borrow_mut(tx)
            .as_mut()
            .unwrap()
            .items
            .insert(item_id.clone(), item_ref.clone());
        self.items.items.borrow_mut(item_ref).write(tx)
    }

    fn read_relation_tx(
        &mut self,
        tx: &TxRef,
        rel_id: &RelationId,
        rel_ref: &Ref<Tx<Relation<Txs>>>,
    ) -> TxRead<Relation<Txs>> {
        if let Some(txi) = self
            .items
            .relations
            .borrow(rel_ref)
            .conflicting_for_read(tx)
        {
            self.abort_transaction_internal(&txi, Preserve::Relation(rel_id));
        }

        self.transactions
            .borrow_mut(tx)
            .as_mut()
            .unwrap()
            .relations
            .insert(rel_id.clone(), rel_ref.clone());

        self.items.relations.borrow_mut(rel_ref).read(tx)
    }

    fn write_relation_tx(
        &mut self,
        tx: &TxRef,
        rel_id: &RelationId,
        rel_ref: &Ref<Tx<Relation<Txs>>>,
    ) -> TxWrite<Relation<Txs>> {
        if let Some(txs) = self
            .items
            .relations
            .borrow(&rel_ref)
            .conflicting_for_write(tx)
        {
            txs.into_iter().for_each(|(id, tx)| {
                self.abort_transaction_internal(&RefBy::new(id, tx), Preserve::Relation(rel_id))
            });
        }
        self.transactions
            .borrow_mut(tx)
            .as_mut()
            .unwrap()
            .relations
            .insert(rel_id.clone(), rel_ref.clone());
        self.items.relations.borrow_mut(rel_ref).write(tx)
    }

    pub fn tx_read_item(&mut self, tx: TransactionId, id: &ItemId) -> Result<Option<&Item<Txs>>> {
        let tx = self.get_transaction_ref(tx)?;
        Ok(self.read_item_internal(&tx, id))
    }

    pub fn tx_read_relation(
        &mut self,
        tx: TransactionId,
        id: &RelationId,
    ) -> Result<Option<&Relation<Txs>>> {
        let tx = self.get_transaction_ref(tx)?;
        Ok(self.read_relation_internal(&tx, id))
    }

    fn read_item_internal(&mut self, tx: &TxRef, id: &ItemId) -> Option<&Item<Txs>> {
        let item_ref = self.get_item_tx(id);
        self.read_item_tx(tx, id, &item_ref).updated()
    }

    fn read_relation_internal(&mut self, tx: &TxRef, id: &RelationId) -> Option<&Relation<Txs>> {
        let rel_ref = self.get_relation_tx(id);
        self.read_relation_tx(tx, id, &rel_ref).updated()
    }

    pub fn insert_item(
        &mut self,
        id: ItemId,
        item: serial::Item,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Updates> {
        let mut item = Item::from_serial(item, types, pkg)?;
        item.resolve(self.items.items.index())?;

        let tx = self.create_transaction_internal();
        self.insert_item_internal(&tx, id, item);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok(self.commit_transaction_internal(&tx, types))
    }

    pub fn tx_insert_item(
        &mut self,
        tx: TransactionId,
        id: ItemId,
        item: serial::Item,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<()> {
        let mut item = Item::from_serial(item, types, pkg)?;
        item.resolve(self.items.items.index())?;

        let tx = self.get_transaction_ref(tx)?;
        self.insert_item_internal(&tx, id, item);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok(())
    }

    fn insert_item_internal(&mut self, tx: &TxRef, id: ItemId, item: Item<Txs>) {
        if let Some(new_parent) = self.insert_item_internal0(tx, id, item) {
            self.insert_item_internal1(tx, new_parent);
        }
    }

    fn insert_item_internal0(
        &mut self,
        tx: &TxRef,
        id: ItemId,
        mut item: Item<Txs>,
    ) -> Option<NewParent<Txs>> {
        let item_ref = self.get_item_tx(&id);
        let mut item_tx = self.write_item_tx(tx, &id, &item_ref);

        let current = item_tx.current();
        let updated = item_tx.updated();

        if let Some(updated) = updated {
            item.children = updated.children.clone();
            item.source_of = updated.source_of.clone();
            item.target_of = updated.target_of.clone();
        }

        let old_parent = updated.and_then(|item| item.parent.as_ref());
        let new_parent = item.parent.as_ref();
        let parent_change =
            (old_parent != new_parent).then(|| (old_parent.cloned(), new_parent.cloned()));

        let old_type = updated
            .map(|item| item.item_type.key())
            .filter(|typ| current.map_or(true, |current| current.item_type.key() != *typ));
        let new_type = Some(item.item_type.key())
            .filter(|typ| current.map_or(true, |current| current.item_type.key() != *typ));
        let type_change = (old_type != new_type).then(|| (old_type.cloned(), new_type.cloned()));

        item_tx.update(item);

        if let Some((old, new)) = type_change {
            if let Some(typ) = old {
                self.items
                    .by_type
                    .get_mut(&typ)
                    .unwrap()
                    .remove(&id)
                    .unwrap();
            }
            if let Some(typ) = new {
                self.items
                    .by_type
                    .entry(typ)
                    .or_default()
                    .insert(id.clone(), item_ref.clone());
            }
        }

        if let Some((old, new)) = parent_change {
            if let Some(parent) = old {
                let mut parent = self.write_item_tx(tx, parent.key(), parent.value_ref());
                parent.modify().unwrap().children.remove(&id).unwrap();
            }
            new.map(|parent| (parent, id, item_ref))
        } else {
            None
        }
    }

    fn insert_item_internal1(
        &mut self,
        tx: &TxRef,
        (parent_ref, item_id, item_ref): NewParent<Txs>,
    ) {
        let mut parent = self.write_item_tx(tx, parent_ref.key(), parent_ref.value_ref());
        let children = &mut parent.modify().unwrap().children;
        let r = children.insert(item_id.clone(), item_ref.clone());
        debug_assert!(r.is_none());
    }

    pub fn remove_item(&mut self, id: &ItemId, types: &Types) -> Updates {
        let tx = self.create_transaction_internal();
        self.remove_item_internal(&tx, id);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        self.commit_transaction_internal(&tx, types)
    }

    pub fn tx_remove_item(&mut self, tx: TransactionId, id: &ItemId) -> Result<()> {
        let tx = self.get_transaction_ref(tx)?;
        self.remove_item_internal(&tx, id);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok(())
    }

    fn remove_item_internal(&mut self, tx: &TxRef, id: &ItemId) {
        let item_ref = self.get_item_tx(id);
        let item_tx = self.read_item_tx(tx, id, &item_ref);
        if item_tx.updated().is_some() {
            self.remove_item_ref_internal(tx, id, &item_ref);
        }
    }

    fn remove_item_ref_internal(&mut self, tx: &TxRef, id: &ItemId, item_ref: &Ref<Tx<Item<Txs>>>) {
        let item_tx = self.write_item_tx(tx, id, item_ref);

        item_tx
            .updated()
            .unwrap()
            .children
            .clone()
            .into_iter()
            .for_each(|(child_id, child_ref)| {
                self.remove_item_ref_internal(tx, &child_id, &child_ref);
            });

        let mut item_tx = self.write_item_tx(tx, id, item_ref);
        let current = item_tx.current();
        let updated = item_tx.updated().unwrap();
        let parent = updated.parent.clone();
        let item_type = current.and_then(|item| {
            (item.item_type != updated.item_type).then(|| updated.item_type.key().clone())
        });
        let source_of = updated.source_of.clone();
        let target_of = updated.target_of.clone();

        item_tx.remove();

        if let Some(parent) = parent {
            let mut parent = self.write_item_tx(tx, parent.key(), parent.value_ref());
            parent.modify().unwrap().children.remove(id).unwrap();
        }

        if let Some(typ) = item_type {
            self.items
                .by_type
                .get_mut(&typ)
                .unwrap()
                .remove(id)
                .unwrap();
        }

        source_of.into_iter().for_each(|(rel_type, rels)| {
            rels.into_iter().for_each(|(rel_id, rel_ref)| {
                let mut rel_tx = self.write_relation_tx(tx, &rel_id, &rel_ref);
                let target = rel_tx.updated().unwrap().target.clone();
                rel_tx.remove();

                let mut target = self.write_item_tx(tx, target.key(), target.value_ref());
                target
                    .modify()
                    .unwrap()
                    .target_of
                    .get_mut(&rel_type)
                    .unwrap()
                    .remove(&rel_id)
                    .unwrap();
            });
        });

        target_of.into_iter().for_each(|(rel_type, rels)| {
            rels.into_iter().for_each(|(rel_id, rel_ref)| {
                let mut rel_tx = self.write_relation_tx(tx, &rel_id, &rel_ref);
                let source = rel_tx.updated().unwrap().source.clone();
                rel_tx.remove();

                let mut source = self.write_item_tx(tx, source.key(), source.value_ref());
                source
                    .modify()
                    .unwrap()
                    .source_of
                    .get_mut(&rel_type)
                    .unwrap()
                    .remove(&rel_id)
                    .unwrap();
            });
        });
    }

    pub fn insert_relation(
        &mut self,
        id: RelationId,
        relation: serial::Relation,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Updates> {
        let mut relation = Relation::from_serial(relation, types, pkg)?;
        relation.resolve(self.items.items.index())?;

        let tx = self.create_transaction_internal();
        self.insert_relation_internal(&tx, id, relation);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok(self.commit_transaction_internal(&tx, types))
    }

    pub fn tx_insert_relation(
        &mut self,
        tx: TransactionId,
        id: RelationId,
        relation: serial::Relation,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<()> {
        let mut relation = Relation::from_serial(relation, types, pkg)?;
        relation.resolve(self.items.items.index())?;

        let tx = self.get_transaction_ref(tx)?;
        self.insert_relation_internal(&tx, id, relation);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok(())
    }

    fn insert_relation_internal(&mut self, tx: &TxRef, id: RelationId, relation: Relation<Txs>) {
        let rel_ref = self.get_relation_tx(&id);
        let mut rel_tx = self.write_relation_tx(tx, &id, &rel_ref);
        let updated = rel_tx.updated();

        let new_type = relation.relation_type.key();
        let type_change_from = updated
            .filter(|rel| rel.relation_type != relation.relation_type)
            .map(|rel| rel.relation_type.key().clone());

        let remove_old_source = updated
            .filter(|rel| rel.source != relation.source)
            .map(|rel| (rel.source.clone(), rel.relation_type.key().clone()));
        let add_new_source = updated
            .map_or(true, |rel| {
                rel.source != relation.source || rel.relation_type != relation.relation_type
            })
            .then(|| (relation.source.clone(), new_type.clone()));

        let remove_old_target = updated
            .filter(|rel| rel.target != relation.target)
            .map(|rel| (rel.target.clone(), rel.relation_type.key().clone()));
        let add_new_target = updated
            .map_or(true, |rel| {
                rel.target != relation.target || rel.relation_type != relation.relation_type
            })
            .then(|| (relation.target.clone(), new_type.clone()));

        rel_tx.update(relation);

        if let Some((old_source, old_type)) = remove_old_source {
            let mut source = self.write_item_tx(tx, old_source.key(), old_source.value_ref());
            source
                .modify()
                .unwrap()
                .source_of
                .get_mut(&old_type)
                .unwrap()
                .remove(&id)
                .unwrap();
        }

        if let Some((old_target, old_type)) = remove_old_target {
            let mut target = self.write_item_tx(tx, old_target.key(), old_target.value_ref());
            target
                .modify()
                .unwrap()
                .target_of
                .get_mut(&old_type)
                .unwrap()
                .remove(&id)
                .unwrap();
        }

        if let Some((new_source, new_type)) = add_new_source {
            let mut source = self.write_item_tx(tx, new_source.key(), new_source.value_ref());
            let source_of = &mut source.modify().unwrap().source_of;

            if let Some(old_type) = &type_change_from {
                source_of.get_mut(old_type).unwrap().remove(&id).unwrap();
            }

            let r = source_of
                .entry(new_type)
                .or_default()
                .insert(id.clone(), rel_ref.clone());
            debug_assert!(r.is_none());
        }

        if let Some((new_target, new_type)) = add_new_target {
            let mut target = self.write_item_tx(tx, new_target.key(), new_target.value_ref());
            let target_of = &mut target.modify().unwrap().target_of;

            if let Some(old_type) = &type_change_from {
                target_of.get_mut(old_type).unwrap().remove(&id).unwrap();
            }

            let r = target_of
                .entry(new_type)
                .or_default()
                .insert(id.clone(), rel_ref.clone());
            debug_assert!(r.is_none());
        }
    }

    pub fn remove_relation(&mut self, id: &RelationId, types: &Types) -> Updates {
        let tx = self.create_transaction_internal();
        self.remove_relation_internal(&tx, id);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        self.commit_transaction_internal(&tx, types)
    }

    pub fn tx_remove_relation(&mut self, tx: TransactionId, id: &RelationId) -> Result<()> {
        let tx = self.get_transaction_ref(tx)?;
        self.remove_relation_internal(&tx, id);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok(())
    }

    fn remove_relation_internal(&mut self, tx: &TxRef, id: &RelationId) {
        let rel_ref = self.get_relation_tx(id);
        let rel_tx = self.read_relation_tx(tx, id, &rel_ref);

        if rel_tx.updated().is_some() {
            self.remove_relation_ref_internal(tx, id, &rel_ref);
        }
    }

    fn remove_relation_ref_internal(
        &mut self,
        tx: &TxRef,
        id: &RelationId,
        rel_ref: &Ref<Tx<Relation<Txs>>>,
    ) {
        let mut rel_tx = self.write_relation_tx(tx, id, rel_ref);

        let updated = rel_tx.updated().unwrap();
        let rel_type = updated.relation_type.key().clone();
        let source = updated.source.clone();
        let target = updated.target.clone();

        rel_tx.remove();

        let mut source = self.write_item_tx(tx, source.key(), source.value_ref());
        source
            .modify()
            .unwrap()
            .source_of
            .get_mut(&rel_type)
            .unwrap()
            .remove(id)
            .unwrap();

        let mut target = self.write_item_tx(tx, target.key(), target.value_ref());
        target
            .modify()
            .unwrap()
            .target_of
            .get_mut(&rel_type)
            .unwrap()
            .remove(id)
            .unwrap();
    }

    pub fn insert_items(
        &mut self,
        domain: serial::Domain,
        items: serial::Items,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Updates> {
        let domain = domain.resolve(types, &self.items, pkg)?;
        let items = ItemMap::from_serial(items, types, pkg)?; // Item refs unresolved!
        let current = ItemsRefMap::from_domain_tx(&domain, &self.items, None);
        items.in_domain(&domain, &current, &self.items, None)?;
        items.resolveable(&current, &self.items, None)?;

        let tx = self.create_transaction_internal();
        self.replace_items_internal(&tx, &current, items);
        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok(self.commit_transaction_internal(&tx, types))
    }

    pub fn tx_insert_items(
        &mut self,
        tx: TransactionId,
        domain: serial::Domain,
        items: serial::Items,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<()> {
        let domain = domain.resolve(types, &self.items, pkg)?;
        let items = ItemMap::from_serial(items, types, pkg)?; // Item refs unresolved!
        let tx = self.get_transaction_ref(tx)?;
        let current = ItemsRefMap::from_domain_tx(&domain, &self.items, Some(&tx));
        items.in_domain(&domain, &current, &self.items, Some(&tx))?;
        items.resolveable(&current, &self.items, Some(&tx))?;

        self.replace_items_internal(&tx, &current, items);

        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok(())
    }

    /// Replace relations of a specific types by already resolved
    /// relations. This is used when updating connectors (declarative,
    /// rule-based relations).
    pub fn replace_resolved_relations(
        &mut self,
        relations: BTreeMap<Absolute<RelationTypeId>, Vec<Relation<Txs>>>,
        types: &Types,
    ) -> Result<(Updates, usize, usize)> {
        let (add, remove) = {
            let current_rels = self
                .items
                .relations
                .iter_ref()
                .filter_map(|(rel_id, rel_ref)| {
                    let rel = self.items.relations.borrow(rel_ref).current()?;
                    relations.contains_key(rel.relation_type.key()).then(|| {
                        (
                            (rel.relation_type.key(), rel.source.key(), rel.target.key()),
                            (rel_id, rel_ref),
                        )
                    })
                })
                .collect::<BTreeMap<_, _>>();

            let new_rels = relations
                .iter()
                .flat_map(|(rel_type, rels)| {
                    rels.iter()
                        .map(move |rel| ((rel_type, rel.source.key(), rel.target.key()), rel))
                })
                .collect::<BTreeMap<_, _>>();

            let remove = current_rels
                .iter()
                .filter(|(key, _)| !new_rels.contains_key(key))
                .map(|(_, (rel_id, rel_ref))| ((*rel_id).clone(), (*rel_ref).clone()))
                .collect::<Vec<_>>();
            let add = new_rels
                .iter()
                .filter(|(key, _)| !current_rels.contains_key(key))
                .map(|(_, rel)| (RelationId::new(), (*rel).clone()))
                .collect::<Vec<_>>();

            (add, remove)
        };

        let (added, removed) = (add.len(), remove.len());
        let tx = self.create_transaction_internal();

        remove
            .into_iter()
            .for_each(|(id, rel_ref)| self.remove_relation_ref_internal(&tx, &id, &rel_ref));
        add.into_iter()
            .for_each(|(id, rel)| self.insert_relation_internal(&tx, id, rel));

        #[cfg(debug_assertions)]
        self.verify_transaction(&tx);
        Ok((self.commit_transaction_internal(&tx, types), added, removed))
    }

    /// Replace `current` with `items`. Items must (have been checked
    /// to) be resolveable (panics otherwise), but cannot be resolved
    /// until they are actually inserted.
    fn replace_items_internal(
        &mut self,
        tx: &TxRef,
        current: &ItemsRefMap<Txs>,
        items: ItemMap<Txs>,
    ) {
        let removed_items = current
            .items
            .iter_ref()
            .filter(|(item_id, _)| !items.items.contains_key(item_id))
            .collect::<Vec<_>>();

        let removed_rels = current
            .relations
            .iter_ref()
            .filter(|(rel_id, _)| !items.relations.contains_key(rel_id))
            .collect::<Vec<_>>();

        items.items.keys().for_each(|item_id| {
            self.items
                .items
                .entry(item_id.clone())
                .or_insert_with(Tx::new_empty);
        });

        let parents = items
            .items
            .into_iter()
            .filter_map(|(item_id, mut item)| {
                item.resolve(self.items.items.index()).unwrap();
                self.insert_item_internal0(tx, item_id, item)
            })
            .collect::<Vec<_>>();

        parents.into_iter().for_each(|new_parent| {
            self.insert_item_internal1(tx, new_parent);
        });

        items.relations.into_iter().for_each(|(rel_id, mut rel)| {
            rel.resolve(self.items.items.index()).unwrap();
            self.insert_relation_internal(tx, rel_id, rel);
        });

        removed_rels.into_iter().for_each(|(rel_id, rel_ref)| {
            self.remove_relation_ref_internal(tx, rel_id, rel_ref);
        });

        removed_items.into_iter().for_each(|(item_id, item_ref)| {
            // Could have been already removed as child of another item.
            if self
                .items
                .items
                .borrow(&item_ref)
                .current_for(Some(tx))
                .is_some()
            {
                self.remove_item_ref_internal(tx, item_id, item_ref);
            }
        });
    }

    #[cfg(debug_assertions)]
    fn verify_transaction(&self, tx: &TxRef) {
        self.verify(Some(tx));
        self.verify(None);
    }

    #[cfg(debug_assertions)]
    fn verify(&self, tx: Option<&TxRef>) {
        self.items
            .items
            .iter()
            .filter_map(|(id, item)| Some((id, item.current_for(tx)?)))
            .for_each(|(item_id, item)| {
                if let Some(parent) = &item.parent {
                    self.items
                        .items
                        .borrow(parent.value_ref())
                        .current_for(tx)
                        .unwrap()
                        .children
                        .contains_key(item_id)
                        .then_some(())
                        .unwrap_or_else(|| panic!("missing child"));
                }

                item.source_of.iter().for_each(|(rel_type, rels)| {
                    rels.iter(&self.items.relations).for_each(|(rel_id, rel)| {
                        let rel = rel.current_for(tx).unwrap();
                        (rel.relation_type.key() == rel_type && rel.source.key() == item_id)
                            .then_some(())
                            .unwrap_or_else(|| {
                                panic!("invalid link between item {item_id} and relation {rel_id}")
                            });
                    })
                });

                item.target_of.iter().for_each(|(rel_type, rels)| {
                    rels.iter(&self.items.relations).for_each(|(rel_id, rel)| {
                        let rel = rel.current_for(tx).unwrap();
                        (rel.relation_type.key() == rel_type && rel.target.key() == item_id)
                            .then_some(())
                            .unwrap_or_else(|| {
                                panic!("invalid link between item {item_id} and relation {rel_id}")
                            })
                    })
                });

                self.items
                    .by_type
                    .get(item.item_type.key())
                    .expect("missing item in item type index")
                    .contains_key(item_id)
                    .then_some(())
                    .expect("missing item in item type index");
            });

        self.items
            .relations
            .iter()
            .filter_map(|(id, rel)| Some((id, rel.current_for(tx)?)))
            .for_each(|(rel_id, rel)| {
                self.items
                    .items
                    .borrow(&rel.source)
                    .current_for(tx)
                    .unwrap()
                    .source_of
                    .get(rel.relation_type.key())
                    .unwrap_or_else(|| {
                        let rel_type = rel.relation_type.key();
                        panic!("missing source_of link for {rel_type}");
                    })
                    .contains_key(rel_id)
                    .then_some(())
                    .unwrap_or_else(|| {
                        let source = rel.source.key();
                        panic!("invalid link between item {source} and relation {rel_id}",);
                    });

                self.items
                    .items
                    .borrow(&rel.target)
                    .current_for(tx)
                    .unwrap()
                    .target_of
                    .get(rel.relation_type.key())
                    .unwrap_or_else(|| {
                        let rel_type = rel.relation_type.key();
                        panic!("missing target_of link for {rel_type}");
                    })
                    .contains_key(rel_id)
                    .then_some(())
                    .unwrap_or_else(|| {
                        let target = rel.target.key();
                        panic!("invalid link between item {target} and relation {rel_id}",);
                    });
            });
    }
}

impl Default for TxItems {
    fn default() -> Self {
        Self::new()
    }
}

impl Items<Identity> {
    pub(crate) fn from_db(db: DbItems, types: &Types) -> Result<Self> {
        let mut items = HashGraph::new();
        let mut relations = HashGraph::new();
        db.0.into_iter().try_for_each(|(_, item)| {
            match &item.entity {
                EntityInfo::Item {
                    item: ItemInfo { item_id, .. },
                } => {
                    items.insert(item_id.clone(), Item::from_db(item, types)?);
                }
                EntityInfo::Relation {
                    relation: RelationInfo { relation_id, .. },
                    ..
                } => {
                    relations.insert(relation_id.clone(), Relation::from_db(item, types)?);
                }
            }
            Ok(())
        })?;

        Ok(Self {
            items,
            relations,
            // initialized in Self::resolve(...)
            by_type: BTreeMap::new(),
        })
    }
}

impl Items<Txs> {
    fn from_serial_tx(
        serial: serial::Items,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Items {
            items: serial
                .items
                .into_iter()
                .map(|(item_id, item)| {
                    let item = Tx::new_committed(Item::from_serial(item, types, pkg)?);
                    Ok((item_id, item))
                })
                .collect::<Result<_>>()?,
            relations: serial
                .relations
                .into_iter()
                .map(|(rel_id, rel)| {
                    let rel = Tx::new_committed(Relation::from_serial(rel, types, pkg)?);
                    Ok((rel_id, rel))
                })
                .collect::<Result<_>>()?,
            // initialized in Self::resolve(...)
            by_type: BTreeMap::new(),
        })
    }

    pub(crate) fn from_db_tx(db: DbItems, types: &Types) -> Result<Self> {
        let mut items = HashGraph::new();
        let mut relations = HashGraph::new();
        db.0.into_iter().try_for_each(|(_, item)| {
            match &item.entity {
                EntityInfo::Item {
                    item: ItemInfo { item_id, .. },
                } => {
                    items.insert(
                        item_id.clone(),
                        Tx::new_committed(Item::from_db(item, types)?),
                    );
                }
                EntityInfo::Relation {
                    relation: RelationInfo { relation_id, .. },
                    ..
                } => {
                    relations.insert(
                        relation_id.clone(),
                        Tx::new_committed(Relation::from_db(item, types)?),
                    );
                }
            }
            Ok(())
        })?;

        Ok(Self {
            items,
            relations,
            by_type: BTreeMap::new(),
        })
    }
}

impl<W: Wrapper> Items<W> {
    pub fn new() -> Self {
        Self {
            items: HashGraph::with_hasher(RandomState::default()),
            relations: HashGraph::with_hasher(RandomState::default()),
            by_type: BTreeMap::new(),
        }
    }

    pub(super) fn from_serial(
        serial: serial::Items<W>,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Items {
            items: serial
                .items
                .into_iter()
                .map(|(item_id, item)| {
                    let item = item.try_map_wrapped(|item| Item::from_serial(item, types, pkg))?;
                    Ok((item_id, item))
                })
                .collect::<Result<_>>()?,
            relations: serial
                .relations
                .into_iter()
                .map(|(rel_id, rel)| {
                    let rel = rel.try_map_wrapped(|rel| Relation::from_serial(rel, types, pkg))?;
                    Ok((rel_id, rel))
                })
                .collect::<Result<_>>()?,
            // initialized in Self::resolve(...)
            by_type: BTreeMap::new(),
        })
    }

    pub(super) fn resolve(&mut self) -> Result<()> {
        let items = self.items.index().clone();

        /* Resolve references in items and relations. */
        self.items
            .values_mut()
            .try_for_each(|item| item.get_wrapped_mut().resolve(&items))?;
        self.relations
            .values_mut()
            .try_for_each(|rel| rel.get_wrapped_mut().resolve(&items))?;

        /* Add Item::children. */
        let children = self
            .items
            .iter_ref()
            .filter_map(|(item_id, item_ref)| {
                Some((
                    (item_id.clone(), item_ref.clone()),
                    self.items
                        .borrow(item_ref)
                        .get_wrapped()
                        .parent
                        .as_ref()?
                        .clone(),
                ))
            })
            .collect::<Vec<_>>();
        children
            .into_iter()
            .for_each(|((child_id, child_ref), parent)| {
                self.items
                    .borrow_mut(&parent)
                    .get_wrapped_mut()
                    .children
                    .insert(child_id, child_ref);
            });

        /* Add Item::source_of and Item::target_of. */

        self.relations.iter_ref().for_each(|(rel_id, rel_ref)| {
            let rel = self.relations.borrow(rel_ref).get_wrapped();
            self.items
                .borrow_mut(&rel.source)
                .get_wrapped_mut()
                .source_of
                .entry(rel.relation_type.key().clone())
                .or_default()
                .insert(rel_id.clone(), rel_ref.clone());
            self.items
                .borrow_mut(&rel.target)
                .get_wrapped_mut()
                .target_of
                .entry(rel.relation_type.key().clone())
                .or_default()
                .insert(rel_id.clone(), rel_ref.clone());
        });

        /* Populate Items:: by_type index.*/
        self.items.iter_ref().for_each(|(item_id, item_ref)| {
            self.by_type
                .entry(
                    self.items
                        .borrow(item_ref)
                        .get_wrapped()
                        .item_type
                        .key()
                        .clone(),
                )
                .or_default()
                .insert(item_id.clone(), item_ref.clone());
        });
        Ok(())
    }

    pub fn to_serial(&self, pkg: Option<&PackageId>) -> serial::Items<W>
    where
        WrappedItem<W>: Clone,
        WrappedRelation<W>: Clone,
    {
        serial::Items {
            items: self
                .iter_items()
                .map(|(item_id, item)| {
                    (
                        item_id.clone(),
                        item.clone().map_wrapped(|item| item.to_serial(pkg)),
                    )
                })
                .collect(),
            relations: self
                .iter_relations()
                .map(|(relation_id, relation)| {
                    (
                        relation_id.clone(),
                        relation
                            .clone()
                            .map_wrapped(|relation| relation.to_serial(pkg)),
                    )
                })
                .collect(),
        }
    }

    pub fn to_db(&self, types: &Types) -> DbItems {
        DbItems(
            self.iter_items()
                .map(|(item_id, item)| {
                    (
                        ObjectId::from(item_id.to_string()),
                        item.get_wrapped().to_db(item_id.clone(), self, types),
                    )
                })
                .chain(self.iter_relations().map(|(relation_id, relation)| {
                    (
                        ObjectId::from(relation_id.to_string()),
                        relation
                            .get_wrapped()
                            .to_db(relation_id.clone(), self, types),
                    )
                }))
                .collect(),
        )
    }

    pub fn get_item(&self, id: &ItemId) -> Option<&WrappedItem<W>> {
        self.items.get(id)
    }

    pub fn get_relation(&self, id: &RelationId) -> Option<&WrappedRelation<W>> {
        self.relations.get(id)
    }

    pub fn borrow_item<T: AsRef<ItemRef<W>>>(&self, item_ref: &T) -> &WrappedItem<W> {
        self.items.borrow(item_ref)
    }

    pub fn borrow_relation<T: AsRef<RelationRef<W>>>(&self, rel_ref: &T) -> &WrappedRelation<W> {
        self.relations.borrow(rel_ref)
    }

    pub fn iter_items(&self) -> impl Iterator<Item = (&ItemId, &WrappedItem<W>)> {
        self.items.iter()
    }

    pub fn iter_relations(&self) -> impl Iterator<Item = (&RelationId, &WrappedRelation<W>)> {
        self.relations.iter()
    }

    pub fn iter_items_by_type(
        &self,
        item_type: &Absolute<ItemTypeId>,
    ) -> impl Iterator<Item = (&ItemId, &WrappedItem<W>)> {
        self.by_type
            .get(item_type)
            .into_iter()
            .flat_map(|index| index.iter(&self.items))
    }

    pub fn iter_item_refs_by_type(
        &self,
        item_type: &Absolute<ItemTypeId>,
    ) -> impl Iterator<Item = (&ItemId, &Ref<WrappedItem<W>>)> {
        self.by_type
            .get(item_type)
            .into_iter()
            .flat_map(|index| index.iter_ref())
    }

    pub fn item_keys(&self) -> impl Iterator<Item = &ItemId> {
        self.items.keys()
    }

    pub fn relation_keys(&self) -> impl Iterator<Item = &RelationId> {
        self.relations.keys()
    }

    pub fn get_item_types(&self, types: &Types) -> BTreeSet<Absolute<ItemTypeId>> {
        fn get_implements(
            item_type_id: &Absolute<ItemTypeId>,
            item_type_ref: &Ref<ItemType>,
            item_types: &mut BTreeSet<Absolute<ItemTypeId>>,
            types: &Types,
        ) {
            if item_types.insert(item_type_id.clone()) {
                let item_type = types.items.borrow(item_type_ref);
                item_type
                    .implements
                    .iter_ref()
                    .for_each(|(item_type_id, item_type_ref)| {
                        get_implements(item_type_id, item_type_ref, item_types, types)
                    });
            }
        }

        let mut item_types = BTreeSet::new();
        self.iter_items()
            .map(|(_, item)| item.get_wrapped().item_type.pair())
            .for_each(|(item_type_id, item_type_ref)| {
                get_implements(item_type_id, item_type_ref, &mut item_types, types)
            });
        item_types
    }
}

impl<W: Wrapper> Default for Items<W> {
    fn default() -> Self {
        Self::new()
    }
}

impl Transaction {
    fn new() -> Self {
        Self {
            items: TxMap::new(),
            relations: TxMap::new(),
        }
    }

    fn abort(self, tx: &TxRef, items: &mut Items<Txs>, preserve: Preserve<'_>) {
        self.items.changes(&items.items).for_each(|(id, change)| {
            if let Some(typ) = match change {
                Change::Created(v) => Some(v.item_type.key()),
                Change::Updated(a, b) if a.item_type != b.item_type => Some(b.item_type.key()),
                _ => None,
            } {
                items.by_type.get_mut(typ).unwrap().remove(id).unwrap();
            }
        });
        self.items.abort(tx, &mut items.items, preserve.item());
        self.relations
            .abort(tx, &mut items.relations, preserve.relation());
    }

    fn commit(self, tx: &TxRef, items: &mut Items<Txs>, types: &Types) -> Updates {
        self.items.changes(&items.items).for_each(|(id, change)| {
            if let Some(typ) = match change {
                Change::Removed(v) => Some(v.item_type.key()),
                Change::Updated(a, b) if a.item_type != b.item_type => Some(a.item_type.key()),
                _ => None,
            } {
                items.by_type.get_mut(typ).unwrap().remove(id).unwrap();
            }
        });
        let item_updates = self.items.changes(&items.items).filter_map(|(id, change)| {
            Some((
                ObjectId::from(id.to_string()),
                match change {
                    Change::Created(v) => Some(v.to_db_tx(id.clone(), tx, items, types)),
                    Change::Removed(_) => None,
                    Change::Updated(p, v) => {
                        let p = p.to_db_tx(id.clone(), tx, items, types);
                        let v = v.to_db_tx(id.clone(), tx, items, types);
                        (p != v).then_some(())?;
                        Some(v)
                    }
                },
            ))
        });
        let relation_updates =
            self.relations
                .changes(&items.relations)
                .filter_map(|(id, change)| {
                    Some((
                        ObjectId::from(id.to_string()),
                        match change {
                            Change::Created(v) => Some(v.to_db_tx(id.clone(), tx, items, types)),
                            Change::Removed(_) => None,
                            Change::Updated(p, v) => {
                                let p = p.to_db_tx(id.clone(), tx, items, types);
                                let v = v.to_db_tx(id.clone(), tx, items, types);
                                (p != v).then_some(())?;
                                Some(v)
                            }
                        },
                    ))
                });
        let updates = item_updates.chain(relation_updates).collect();

        self.items.commit(tx, &mut items.items);
        self.relations.commit(tx, &mut items.relations);
        Updates(updates)
    }
}

impl Item<Txs> {
    fn to_db_tx(&self, id: ItemId, tx: &TxRef, items: &Items<Txs>, types: &Types) -> DbItem {
        DbItem {
            entity: self.entity_info_tx(id, tx, items, types),
            parent: self.parent.as_ref().map(|parent| parent.key().clone()),
            properties: self.properties.iter().fold(
                BTreeMap::new(),
                |mut props, (prop_id, prop)| {
                    props
                        .entry(prop_id.package().clone())
                        .or_default()
                        .insert(prop_id.local().clone(), prop.to_json());
                    props
                },
            ),
        }
    }

    fn entity_info_tx(
        &self,
        id: ItemId,
        tx: &TxRef,
        items: &Items<Txs>,
        types: &Types,
    ) -> EntityInfo {
        EntityInfo::Item {
            item: self.item_info_tx(id, tx, items, types),
        }
    }

    fn item_info_tx(&self, id: ItemId, tx: &TxRef, items: &Items<Txs>, types: &Types) -> ItemInfo {
        ItemInfo {
            item_id: id,
            item_name: self.name_chain_tx(tx, items, types),
            item_type: self.item_type_id().clone(),
            parents: self
                .parents_tx(tx, items)
                .map(|(parent_id, _)| parent_id.clone())
                .collect(),
        }
    }

    fn name_chain_tx(&self, tx: &TxRef, items: &Items<Txs>, types: &Types) -> Vec<String> {
        self.parents_tx(tx, items)
            .map(|(_, item)| item)
            .chain(std::iter::once(self))
            .map(|item| item.name(types))
            .collect()
    }

    fn parents_tx<'a>(
        &'a self,
        tx: &'a TxRef,
        items: &'a Items<Txs>,
    ) -> Box<dyn Iterator<Item = (&'a ItemId, &'a Item<Txs>)> + 'a> {
        Box::new(self.parent.iter().flat_map(|parent_ref| {
            let parent = items
                .items
                .borrow(parent_ref)
                .current_for(Some(tx))
                .unwrap();
            parent
                .parents_tx(tx, items)
                .chain(std::iter::once((parent_ref.key(), parent)))
        }))
    }
}

impl<W: Wrapper> Item<W> {
    fn from_serial(serial: serial::Item, types: &Types, pkg: Option<&PackageId>) -> Result<Self> {
        let item_type_id = serial.item_type.resolve_opt(pkg);
        let item_type_ref = types
            .items
            .get_ref_by(&item_type_id)
            .ok_or(Error::MissingItemType(item_type_id))?;
        let item_type = types.items.borrow(&item_type_ref);
        Ok(Self {
            item_type: item_type_ref,
            parent: serial.parent.map(RefBy::dangling),
            children: RefMap::new(),
            properties: serial
                .properties
                .into_iter()
                .filter_map(|(prop_id, value)| {
                    let prop_id = prop_id.resolve_opt(pkg);
                    item_type
                        .get_property(&prop_id, types)
                        .map(|_prop| (prop_id, value))
                })
                .collect(),
            source_of: BTreeMap::new(),
            target_of: BTreeMap::new(),
        })
    }

    fn from_db(db: DbItem, types: &Types) -> Result<Self> {
        let item_type_id = match db.entity {
            EntityInfo::Item {
                item: ItemInfo { item_type, .. },
            } => item_type,
            EntityInfo::Relation { .. } => return Err(Error::DbMissingItemType),
        };
        let item_type_ref = types
            .items
            .get_ref_by(&item_type_id)
            .ok_or(Error::MissingItemType(item_type_id))?;
        let item_type = types.items.borrow(&item_type_ref);
        Ok(Self {
            item_type: item_type_ref,
            parent: db.parent.map(RefBy::dangling),
            properties: db
                .properties
                .into_iter()
                .flat_map(|(pkg_id, props)| {
                    props.into_iter().map(move |(prop_id, value)| {
                        (Absolute::new(pkg_id.clone(), prop_id), value)
                    })
                })
                .filter_map(|(prop_id, value)| {
                    item_type
                        .get_property(&prop_id, types)
                        .map(|prop| prop.value_from_json(prop_id, value))
                })
                .collect::<Result<_>>()?,
            children: RefMap::new(),
            source_of: BTreeMap::new(),
            target_of: BTreeMap::new(),
        })
    }

    fn resolve<T: IndexBy<ItemId, WrappedItem<W>>>(&mut self, items: &T) -> Result<()> {
        self.parent
            .iter_mut()
            .try_for_each(|parent| parent.resolve(items).map_err(Error::MissingItem))?;
        Ok(())
    }

    pub fn to_serial(&self, pkg: Option<&PackageId>) -> serial::Item {
        serial::Item {
            item_type: self.item_type.key().to_relative_opt(pkg),
            parent: self.parent.as_ref().map(|p| p.key().clone()),
            properties: self
                .properties
                .iter()
                .map(|(id, val)| (id.to_relative_opt(pkg), val.clone()))
                .collect(),
        }
    }

    pub fn to_db(&self, id: ItemId, items: &Items<W>, types: &Types) -> DbItem {
        DbItem {
            entity: self.entity_info(id, items, types),
            parent: self.parent.as_ref().map(|parent| parent.key().clone()),
            properties: self.properties.iter().fold(
                BTreeMap::new(),
                |mut props, (prop_id, prop)| {
                    props
                        .entry(prop_id.package().clone())
                        .or_default()
                        .insert(prop_id.local().clone(), prop.to_json());
                    props
                },
            ),
        }
    }

    pub fn entity_info(&self, id: ItemId, items: &Items<W>, types: &Types) -> EntityInfo {
        EntityInfo::Item {
            item: self.item_info(id, items, types),
        }
    }

    fn item_info(&self, id: ItemId, items: &Items<W>, types: &Types) -> ItemInfo {
        ItemInfo {
            item_id: id,
            item_name: self.name_chain(items, types),
            item_type: self.item_type_id().clone(),
            parents: self
                .parents(items)
                .map(|(parent_id, _)| parent_id.clone())
                .collect(),
        }
    }

    pub fn item_type_ref(&self) -> &RefBy<Absolute<ItemTypeId>, ItemType> {
        &self.item_type
    }

    pub fn item_type_id(&self) -> &Absolute<ItemTypeId> {
        self.item_type.key()
    }

    pub fn item_type<'a>(&'a self, types: &'a Types) -> &'a ItemType {
        types.items.borrow(&self.item_type)
    }

    pub fn name_chain(&self, items: &Items<W>, types: &Types) -> Vec<String> {
        self.parents(items)
            .map(|(_, item)| item)
            .chain(std::iter::once(self))
            .map(|item| item.name(types))
            .collect()
    }

    pub fn name(&self, types: &Types) -> String {
        let typ = types.items.borrow(&self.item_type);
        std::iter::once(typ)
            .chain(typ.supertypes(types).map(|(_, t)| t))
            .find_map(|typ| typ.name_template.as_ref())
            .map_or_else(
                || {
                    typ.keys(types).map_or_else(String::new, |keys| {
                        keys.keys()
                            .filter_map(|key| self.properties.get(key))
                            .map(|val| val.to_string())
                            .join(" ")
                    })
                },
                |tmpl| tmpl.render(&self.properties).to_string(),
            )
    }

    pub fn properties(&self) -> &BTreeMap<Absolute<PropertyId>, PropertyValue> {
        &self.properties
    }

    pub fn property(&self, id: &Absolute<PropertyId>) -> Option<&PropertyValue> {
        self.properties.get(id)
    }

    pub(crate) fn relations(
        &self,
        relation_type: Option<&Absolute<RelationTypeId>>,
        endpoint: Option<Endpoint>,
    ) -> impl Iterator<Item = ((&RelationId, &RelationRef<W>), Endpoint)> {
        endpoint
            .map_or(true, |ep| ep == Endpoint::Source)
            .then(|| {
                self.targets(relation_type)
                    .map(|rel| (rel, Endpoint::Source))
            })
            .into_iter()
            .flatten()
            .chain(
                endpoint
                    .map_or(true, |ep| ep == Endpoint::Target)
                    .then(|| {
                        self.sources(relation_type)
                            .map(|rel| (rel, Endpoint::Target))
                    })
                    .into_iter()
                    .flatten(),
            )
    }

    pub fn sources(
        &self,
        relation_type: Option<&Absolute<RelationTypeId>>,
    ) -> impl Iterator<Item = (&RelationId, &RelationRef<W>)> {
        relation_type
            .and_then(|typ| self.target_of.get(typ))
            .into_iter()
            .chain(
                relation_type
                    .is_none()
                    .then(|| self.target_of.values())
                    .into_iter()
                    .flatten(),
            )
            .flat_map(|m| m.iter_ref())
    }

    pub fn targets(
        &self,
        relation_type: Option<&Absolute<RelationTypeId>>,
    ) -> impl Iterator<Item = (&RelationId, &RelationRef<W>)> {
        relation_type
            .and_then(|typ| self.source_of.get(typ))
            .into_iter()
            .chain(
                relation_type
                    .is_none()
                    .then(|| self.source_of.values())
                    .into_iter()
                    .flatten(),
            )
            .flat_map(|m| m.iter_ref())
    }

    pub fn parent_id(&self) -> Option<&ItemId> {
        self.parent_ref().map(|r| r.key())
    }

    pub fn parent_ref(&self) -> Option<&NamedItemRef<W>> {
        self.parent.as_ref()
    }

    pub fn parent<'a>(&self, items: &'a Items<W>) -> Option<&'a W::Wrap<Item<W>>> {
        self.parent_ref().map(|r| items.items.borrow(r))
    }

    pub fn parents<'a>(
        &'a self,
        items: &'a Items<W>,
    ) -> Box<dyn Iterator<Item = (&'a ItemId, &'a Item<W>)> + 'a> {
        Box::new(self.parent.iter().flat_map(|parent_ref| {
            let parent = items.items.borrow(parent_ref).get_wrapped();
            parent
                .parents(items)
                .chain(std::iter::once((parent_ref.key(), parent)))
        }))
    }

    #[allow(unused)]
    pub(crate) fn parent_refs<'a>(
        &'a self,
        items: &'a Items<W>,
    ) -> Box<dyn Iterator<Item = (&'a ItemId, &'a ItemRef<W>)> + 'a> {
        Box::new(self.parent.iter().flat_map(|parent_ref| {
            let parent = items.items.borrow(parent_ref).get_wrapped();
            parent
                .parent_refs(items)
                .chain(std::iter::once((parent_ref.key(), parent_ref.value_ref())))
        }))
    }

    // pub fn prometheus_queries(
    //     &self,
    //     items: &Items,
    //     types: &Types,
    //     schema: &prometheus::Universe,
    // ) -> Vec<prometheus::MetricSelector> {
    //     types
    //         .items
    //         .borrow(&self.item_type)
    //         .prometheus_queries_for_item(self, items, types, schema)
    // }
}

impl Relation<Txs> {
    fn to_db_tx(&self, id: RelationId, tx: &TxRef, items: &Items<Txs>, types: &Types) -> DbItem {
        DbItem {
            entity: self.entity_info_tx(id, tx, items, types),
            parent: None,
            properties: self.properties.iter().fold(
                BTreeMap::new(),
                |mut props, (prop_id, prop)| {
                    props
                        .entry(prop_id.package().clone())
                        .or_default()
                        .insert(prop_id.local().clone(), prop.to_json());
                    props
                },
            ),
        }
    }

    fn entity_info_tx(
        &self,
        id: RelationId,
        tx: &TxRef,
        items: &Items<Txs>,
        types: &Types,
    ) -> EntityInfo {
        EntityInfo::Relation {
            relation: self.relation_info(id),
            source: self
                .source(items)
                .current_for(Some(tx))
                .unwrap()
                .item_info_tx(self.source_id().clone(), tx, items, types),
            target: self
                .target(items)
                .current_for(Some(tx))
                .unwrap()
                .item_info_tx(self.target_id().clone(), tx, items, types),
        }
    }
}

impl<W: Wrapper> Relation<W> {
    fn from_serial(
        serial: serial::Relation,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        let relation_type_id = serial.relation_type.resolve_opt(pkg);
        let relation_type_ref = types
            .relations
            .get_ref_by(&relation_type_id)
            .ok_or(Error::MissingRelationType(relation_type_id))?;
        let relation_type = types.relations.borrow(&relation_type_ref);
        Ok(Self {
            relation_type: relation_type_ref,
            properties: serial
                .properties
                .into_iter()
                .filter_map(|(prop_id, value)| {
                    let prop_id = prop_id.resolve_opt(pkg);
                    relation_type
                        .properties
                        .get(&prop_id, &types.properties)
                        .map(|prop| prop.value_from_json(prop_id, value))
                })
                .collect::<Result<_>>()?,
            source: RefBy::dangling(serial.source),
            target: RefBy::dangling(serial.target),
        })
    }

    fn from_db(db: DbItem, types: &Types) -> Result<Self> {
        let EntityInfo::Relation {
            relation:
                RelationInfo {
                    relation_type: relation_type_id,
                    ..
                },
            source: ItemInfo {
                item_id: source_id, ..
            },
            target: ItemInfo {
                item_id: target_id, ..
            },
        } = db.entity
        else {
            return Err(Error::DbMissingRelationType);
        };
        let relation_type_ref = types
            .relations
            .get_ref_by(&relation_type_id)
            .ok_or(Error::MissingRelationType(relation_type_id))?;
        let relation_type = types.relations.borrow(&relation_type_ref);
        Ok(Self {
            relation_type: relation_type_ref,
            properties: db
                .properties
                .into_iter()
                .flat_map(|(pkg_id, props)| {
                    props.into_iter().map(move |(prop_id, value)| {
                        (Absolute::new(pkg_id.clone(), prop_id), value)
                    })
                })
                .filter_map(|(prop_id, value)| {
                    relation_type
                        .properties
                        .get(&prop_id, &types.properties)
                        .map(|prop| prop.value_from_json(prop_id, value))
                })
                .collect::<Result<_>>()?,
            source: RefBy::dangling(source_id),
            target: RefBy::dangling(target_id),
        })
    }

    fn resolve<T: IndexBy<ItemId, WrappedItem<W>>>(&mut self, items: &T) -> Result<()> {
        self.source.resolve(items).map_err(Error::MissingItem)?;
        self.target.resolve(items).map_err(Error::MissingItem)?;
        Ok(())
    }

    pub fn to_serial(&self, pkg: Option<&PackageId>) -> serial::Relation {
        serial::Relation {
            relation_type: self.relation_type.key().to_relative_opt(pkg),
            properties: self
                .properties
                .iter()
                .map(|(id, val)| (id.to_relative_opt(pkg), val.to_json()))
                .collect(),
            source: self.source.key().clone(),
            target: self.target.key().clone(),
        }
    }

    pub fn to_db(&self, id: RelationId, items: &Items<W>, types: &Types) -> DbItem {
        DbItem {
            entity: self.entity_info(id, items, types),
            parent: None,
            properties: self.properties.iter().fold(
                BTreeMap::new(),
                |mut props, (prop_id, prop)| {
                    props
                        .entry(prop_id.package().clone())
                        .or_default()
                        .insert(prop_id.local().clone(), prop.to_json());
                    props
                },
            ),
        }
    }

    pub fn entity_info(&self, id: RelationId, items: &Items<W>, types: &Types) -> EntityInfo {
        EntityInfo::Relation {
            relation: self.relation_info(id),
            source: self.source(items).get_wrapped().item_info(
                self.source_id().clone(),
                items,
                types,
            ),
            target: self.target(items).get_wrapped().item_info(
                self.target_id().clone(),
                items,
                types,
            ),
        }
    }

    fn relation_info(&self, id: RelationId) -> RelationInfo {
        RelationInfo {
            relation_id: id,
            relation_type: self.relation_type_id().clone(),
        }
    }

    pub fn relation_type_id(&self) -> &Absolute<RelationTypeId> {
        self.relation_type.key()
    }

    pub fn relation_type_ref(&self) -> &RefBy<Absolute<RelationTypeId>, RelationType> {
        &self.relation_type
    }

    pub fn relation_type<'a>(&'a self, types: &'a Types) -> &'a RelationType {
        types.relations.borrow(&self.relation_type)
    }

    pub fn source_id(&self) -> &ItemId {
        self.source.key()
    }

    pub fn target_id(&self) -> &ItemId {
        self.target.key()
    }

    pub fn source<'a>(&'a self, items: &'a Items<W>) -> &'a WrappedItem<W> {
        items.items.borrow(&self.source)
    }

    pub fn target<'a>(&'a self, items: &'a Items<W>) -> &'a WrappedItem<W> {
        items.items.borrow(&self.target)
    }

    pub fn endpoint(&self, endpoint: Endpoint) -> &NamedItemRef<W> {
        match endpoint {
            Endpoint::Source => &self.source,
            Endpoint::Target => &self.target,
        }
    }

    pub fn property(&self, id: &Absolute<PropertyId>) -> Option<&PropertyValue> {
        self.properties.get(id)
    }

    // pub fn prometheus_queries(
    //     &self,
    //     items: &Items,
    //     types: &Types,
    //     schema: &prometheus::Universe,
    // ) -> Vec<prometheus::MetricSelector> {
    //     types
    //         .relations
    //         .borrow(&self.relation_type)
    //         .prometheus_queries_for_relation(self, items, types, schema)
    // }
}

impl<W: Wrapper> EntityRef<'_, W> {
    pub fn entity_type<'b>(&self, types: &'b Types) -> EntityTypeRef<'b> {
        match self {
            EntityRef::Item(item) => EntityTypeRef::ItemType(types.items.borrow(&item.item_type)),
            EntityRef::Relation(rel) => {
                EntityTypeRef::RelationType(types.relations.borrow(&rel.relation_type))
            }
        }
    }
}

impl<W: Wrapper> Domain<W> {
    pub(super) fn from_serial(serial: serial::Domain, pkg: Option<&PackageId>) -> Self {
        Self {
            roots: serial.roots.map(|rs| {
                rs.into_iter()
                    .map(|root| (root, Some(Ref::dangling())))
                    .collect()
            }),
            types: TypeSet::from_serial(serial.types, pkg),
        }
    }

    pub(super) fn resolve(&mut self, types: &Types, items: &Items<W>) -> Result<()> {
        if let Some(rs) = self.roots.as_mut() {
            rs.resolve(items.items.index())
        }
        self.types.resolve(types)?;
        Ok(())
    }
}

impl TypeSet {
    fn from_serial(serial: serial::TypeSet, pkg: Option<&PackageId>) -> Self {
        Self {
            items: serial
                .items
                .into_iter()
                .map(|id| (id.resolve_opt(pkg), Ref::dangling()))
                .collect(),
            relations: serial
                .relations
                .into_iter()
                .map(|id| (id.resolve_opt(pkg), Ref::dangling()))
                .collect(),
        }
    }

    pub(super) fn resolve(&mut self, types: &Types) -> Result<()> {
        self.items
            .resolve(&types.items)
            .map_err(Error::MissingItemType)?;
        self.relations
            .resolve(&types.relations)
            .map_err(Error::MissingRelationType)?;
        Ok(())
    }
}

// Note: items and relations in this map are NOT resolved!
impl<W: Wrapper> ItemMap<W> {
    fn from_serial(serial: serial::Items, types: &Types, pkg: Option<&PackageId>) -> Result<Self> {
        Ok(Self {
            items: serial
                .items
                .into_iter()
                .map(|(id, serial)| Ok((id, Item::from_serial(serial, types, pkg)?)))
                .collect::<Result<_>>()?,
            relations: serial
                .relations
                .into_iter()
                .map(|(id, serial)| Ok((id, Relation::from_serial(serial, types, pkg)?)))
                .collect::<Result<_>>()?,
        })
    }
}

impl ItemMap<Txs> {
    /// Verify if all items and relations are in domain.
    ///
    /// Currently, these rules do not allow adding relations between
    /// two items not in domain.
    ///
    /// An item is in domain if:
    /// - Its type is found in the type set
    /// - It is a descendant of the root through the items in this map
    ///
    /// A relation is in domain if:
    /// - Its type is found in the type set
    /// - It is connected to an item in the map
    fn in_domain(
        &self,
        domain: &Domain<Txs>,
        current: &ItemsRefMap<Txs>,
        items: &Items<Txs>,
        tx: Option<&TxRef>,
    ) -> Result<()> {
        self.items.iter().try_for_each(|(item_id, item)| {
            /* Check if updated value has an in-domain type. */
            domain
                .types
                .items
                .contains_key(item.item_type.key())
                .then_some(())
                .ok_or_else(|| {
                    Error::ItemTypeNotInDomain(item_id.clone(), item.item_type.key().clone())
                })?;

            /* Check if updated value has a parent in the map or is the root. */
            (domain
                .roots
                .as_ref()
                .map_or(true, |rs| rs.contains_key(item_id))
                || item
                    .parent
                    .as_ref()
                    .is_some_and(|parent| self.items.contains_key(parent.key())))
            .then_some(())
            .ok_or_else(|| Error::ItemNotInDomain(item_id.clone()))?;

            /* Check for collision (item exists and is not in domain). */
            (current.items.contains_key(item_id)
                || items
                    .items
                    .get(item_id)
                    .and_then(|item| item.current_for(tx))
                    .is_none())
            .then_some(())
            .ok_or_else(|| Error::ItemInDomainCollision(item_id.clone()))?;

            Ok(())
        })?;

        self.relations.iter().try_for_each(|(rel_id, rel)| {
            /* Check if updated value has an in-domain type. */
            domain
                .types
                .relations
                .contains_key(rel.relation_type.key())
                .then_some(())
                .ok_or_else(|| {
                    Error::RelationTypeNotInDomain(rel_id.clone(), rel.relation_type.key().clone())
                })?;

            /* Check if updated value is connected to an item in the map. */
            (self.items.contains_key(rel.source.key())
                || self.items.contains_key(rel.target.key()))
            .then_some(())
            .ok_or_else(|| Error::RelationNotInDomain(rel_id.clone()))?;

            /* Check for collision (relation exists and is not in domain). */
            (current.relations.contains_key(rel_id)
                || items
                    .relations
                    .get(rel_id)
                    .and_then(|rel| rel.current_for(tx))
                    .is_none())
            .then_some(())
            .ok_or_else(|| Error::RelationInDomainCollision(rel_id.clone()))?;

            Ok(())
        })?;

        Ok(())
    }

    /// Check whether items and relations in the map are resolveable
    /// if `current` is replaced by this map.
    fn resolveable(
        &self,
        current: &ItemsRefMap<Txs>,
        items: &Items<Txs>,
        tx: Option<&TxRef>,
    ) -> Result<()> {
        let item_exists = |id| {
            (self.items.contains_key(id)
                || (!current.items.contains_key(id)
                    && items
                        .items
                        .get(id)
                        .and_then(|item| item.current_for(tx))
                        .is_some()))
            .then_some(())
            .ok_or_else(|| Error::MissingItem(id.clone()))
        };

        self.items.values().try_for_each(|item| {
            // Check types in from_serial???
            item.parent
                .as_ref()
                .map_or(Ok(()), |parent| item_exists(parent.key()))
        })?;

        self.relations.values().try_for_each(|rel| {
            // Check types in from_serial???
            item_exists(rel.source.key())?;
            item_exists(rel.target.key())?;
            Ok(())
        })?;

        Ok(())
    }
}

impl<W: Wrapper> ItemsRefMap<W> {
    fn new() -> Self {
        Self::default()
    }

    pub fn to_serial_wrapped(&self, items: &Items<W>, pkg: Option<&PackageId>) -> serial::Items<W>
    where
        WrappedItem<W>: Clone,
        WrappedRelation<W>: Clone,
    {
        serial::Items {
            items: self
                .items
                .iter_ref()
                .map(|(id, item)| {
                    let mut item = items
                        .borrow_item(item)
                        .clone()
                        .map_wrapped(|item| item.to_serial(pkg));
                    if item
                        .get_wrapped()
                        .parent
                        .as_ref()
                        .is_some_and(|parent| !self.items.contains_key(parent))
                    {
                        // Hacky? Should return something like a PartialItems with optional parent.
                        item.get_wrapped_mut().parent.take();
                    }
                    (id.clone(), item)
                })
                .collect(),
            relations: self
                .relations
                .iter_ref()
                .filter_map(|(id, rel)| {
                    let rel = items
                        .borrow_relation(rel)
                        .clone()
                        .map_wrapped(|rel| rel.to_serial(pkg));
                    self.items
                        .contains_key(&rel.get_wrapped().source)
                        .then_some(())?;
                    self.items
                        .contains_key(&rel.get_wrapped().target)
                        .then_some(())?;
                    Some((id.clone(), rel))
                })
                .collect(),
        }
    }

    pub fn to_serial_unwrapped(
        &self,
        items: &Items<W>,
        pkg: Option<&PackageId>,
    ) -> serial::Items<Identity> {
        serial::Items {
            items: self
                .items
                .iter_ref()
                .map(|(id, item)| {
                    let mut item = items.borrow_item(item).get_wrapped().to_serial(pkg);
                    if item
                        .get_wrapped()
                        .parent
                        .as_ref()
                        .is_some_and(|parent| !self.items.contains_key(parent))
                    {
                        // Hacky? Should return something like a PartialItems with optional parent.
                        item.get_wrapped_mut().parent.take();
                    }
                    (id.clone(), item)
                })
                .collect(),
            relations: self
                .relations
                .iter_ref()
                .filter_map(|(id, rel)| {
                    let rel = items.borrow_relation(rel).get_wrapped().to_serial(pkg);
                    self.items.contains_key(&rel.source).then_some(())?;
                    self.items.contains_key(&rel.target).then_some(())?;
                    Some((id.clone(), rel))
                })
                .collect(),
        }
    }
}

impl ItemsRefMap<Txs> {
    fn from_domain_tx(domain: &Domain<Txs>, items: &Items<Txs>, tx: Option<&TxRef>) -> Self {
        let mut map = Self::new();
        if let Some(rs) = &domain.roots {
            rs.iter_ref()
                .filter_map(|(root_id, opt_root_ref)| {
                    let root_ref = opt_root_ref.as_ref()?;
                    let root = items.items.borrow(root_ref).current_for(tx)?;
                    Some((root_id, root_ref, root))
                })
                .for_each(|(root_id, root_ref, root)| {
                    map.get_domain_internal(root_id, root_ref, root, &domain.types, items, tx);
                });
        } else {
            domain
                .types
                .items
                .keys()
                .flat_map(|item_type| items.iter_item_refs_by_type(item_type))
                .filter_map(|(item_id, item_ref)| {
                    let item = items.items.borrow(item_ref).current_for(tx)?;
                    Some((item_id, item_ref, item))
                })
                .for_each(|(item_id, item_ref, item)| {
                    if !map.items.contains_key(item_id) {
                        map.get_domain_internal(item_id, item_ref, item, &domain.types, items, tx);
                    }
                });
        }
        map
    }

    fn get_domain_internal(
        &mut self,
        item_id: &ItemId,
        item_ref: &Ref<Tx<Item<Txs>>>,
        item: &Item<Txs>,
        types: &TypeSet,
        items: &Items<Txs>,
        tx: Option<&TxRef>,
    ) {
        if types.items.contains_key(item.item_type.key()) {
            self.items.insert(item_id.clone(), item_ref.clone());
        }

        item.source_of
            .iter()
            .chain(&item.target_of)
            .filter(|(typ, _)| types.relations.contains_key(typ))
            .flat_map(|(_, rels)| rels.iter_ref())
            .for_each(|(rel_id, rel_ref)| {
                self.relations.insert(rel_id.clone(), rel_ref.clone());
            });

        item.children.iter_ref().for_each(|(child_id, child_ref)| {
            let child = items.items.borrow(child_ref).current_for(tx).unwrap();
            self.get_domain_internal(child_id, child_ref, child, types, items, tx);
        });
    }
}

impl Preserve<'_> {
    fn item(&self) -> Option<&ItemId> {
        match self {
            Self::Item(id) => Some(id),
            _ => None,
        }
    }

    fn relation(&self) -> Option<&RelationId> {
        match self {
            Self::Relation(id) => Some(id),
            _ => None,
        }
    }
}
