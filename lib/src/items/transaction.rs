/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::hash::Hash;

use graph::{HashGraph, Ref, RefBy, RefMap};

pub(crate) struct TxMap<K, V, TransactionId, Transaction>(
    RefMap<K, Tx<V, TransactionId, Transaction>>,
);

pub(crate) type TxRef<TransactionId, Transaction> = RefBy<TransactionId, Option<Transaction>>;

pub(crate) struct TxRead<'a, T, TransactionId, Transaction>(&'a Tx<T, TransactionId, Transaction>);
pub(crate) struct TxWrite<'a, T, TransactionId, Transaction>(
    &'a mut Tx<T, TransactionId, Transaction>,
);

pub enum Tx<T, TransactionId, Transaction> {
    Committed {
        current: Option<T>,
    },
    Read {
        transactions: RefMap<TransactionId, Option<Transaction>>,
        current: Option<T>,
    },
    Written {
        transaction: TxRef<TransactionId, Transaction>,
        current: Option<T>,
        updated: Option<T>,
    },
}

pub(crate) enum Change<T> {
    Created(T),
    Removed(T),
    Updated(T, T),
}

impl<K, V, TransactionId, Transaction> TxMap<K, V, TransactionId, Transaction>
where
    TransactionId: Eq + Ord + Hash + Clone,
{
    pub(crate) fn new() -> Self {
        Self(RefMap::new())
    }

    pub(crate) fn insert(
        &mut self,
        key: K,
        value: Ref<Tx<V, TransactionId, Transaction>>,
    ) -> Option<Ref<Tx<V, TransactionId, Transaction>>>
    where
        K: Ord,
    {
        self.0.insert(key, value)
    }

    pub(crate) fn changes<'a>(
        &'a self,
        graph: &'a HashGraph<K, Tx<V, TransactionId, Transaction>>,
    ) -> impl Iterator<Item = (&'a K, Change<&'a V>)> {
        self.0
            .iter(graph)
            .filter_map(|(id, value)| Some((id, value.changed()?)))
    }

    pub(crate) fn commit<'a>(
        self,
        tx: &'a TxRef<TransactionId, Transaction>,
        graph: &'a mut HashGraph<K, Tx<V, TransactionId, Transaction>>,
    ) where
        K: Hash + Eq + Clone,
    {
        self.0.into_iter().for_each(|(id, value_ref)| {
            let value = graph.borrow_mut(&value_ref);
            value.commit(tx);
            if value.is_empty() {
                graph.remove(&id);
            }
        });
    }

    pub(crate) fn abort(
        self,
        tx: &TxRef<TransactionId, Transaction>,
        graph: &mut HashGraph<K, Tx<V, TransactionId, Transaction>>,
        preserve: Option<&K>,
    ) where
        K: Hash + Eq,
    {
        self.0.iter_ref().for_each(|(id, value_ref)| {
            let value = graph.borrow_mut(value_ref);
            value.abort(tx);
            if value.is_empty() && preserve != Some(id) {
                graph.remove(id);
            }
        });
    }
}

impl<T, TransactionId, Transaction> Tx<T, TransactionId, Transaction>
where
    TransactionId: Eq + Ord + Hash + Clone,
{
    pub(crate) fn new_empty() -> Self {
        Tx::Committed { current: None }
    }

    pub(crate) fn new_committed(value: T) -> Self {
        Tx::Committed {
            current: Some(value),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        matches!(self, Self::Committed { current: None })
    }

    /// Get a reference to the committed value. Panics if not committed.
    #[allow(unused)]
    pub(crate) fn committed(&self) -> &T {
        match self {
            Tx::Committed { current } => current.as_ref().unwrap(),
            _ => panic!("Tx::committed called on a non-committed value"),
        }
    }

    /// Get a mutable reference to the committed value. Panics if not committed.
    pub(crate) fn committed_mut(&mut self) -> &mut T {
        match self {
            Tx::Committed { current } => current.as_mut().unwrap(),
            _ => panic!("Tx::committed_mut called on a non-committed value"),
        }
    }

    /// Un wrap the committed value. Panics if not committed.
    pub(crate) fn unwrap_committed(self) -> T {
        match self {
            Tx::Committed { current } => current.unwrap(),
            _ => panic!("Tx::committed_mut called on a non-committed value"),
        }
    }

    /// Get a reference to the current value, disregarding any running transactions.
    pub(crate) fn current(&self) -> Option<&T> {
        match self {
            Tx::Committed { current } | Tx::Read { current, .. } | Tx::Written { current, .. } => {
                current.as_ref()
            }
        }
    }

    /// Get a reference to the value as seen by the given
    /// transaction. This should only be used for verification, not
    /// for reading the value in a transaction.
    pub(crate) fn current_for(&self, tx: Option<&TxRef<TransactionId, Transaction>>) -> Option<&T> {
        match self {
            Tx::Committed { current } | Tx::Read { current, .. } => current.as_ref(),
            Tx::Written {
                transaction,
                updated,
                ..
            } if Some(transaction) == tx => updated.as_ref(),
            Tx::Written { current, .. } => current.as_ref(),
        }
    }

    /// Find conflicting transactions for read operations.
    pub(crate) fn conflicting_for_read(
        &self,
        tx: &TxRef<TransactionId, Transaction>,
    ) -> Option<TxRef<TransactionId, Transaction>> {
        match self {
            Tx::Committed { .. } | Tx::Read { .. } => None,
            Tx::Written { transaction, .. } if transaction == tx => None,
            Tx::Written { transaction, .. } => Some(transaction.clone()),
        }
    }

    /// Find conflicting transactions for write operations.
    pub(crate) fn conflicting_for_write(
        &self,
        tx: &TxRef<TransactionId, Transaction>,
    ) -> Option<RefMap<TransactionId, Option<Transaction>>> {
        match self {
            Tx::Read { transactions, .. }
                if transactions.len() > 1 || transactions.keys().next().unwrap() != tx.key() =>
            {
                Some(
                    transactions
                        .iter_ref()
                        .filter(|(id, _)| *id != tx.key())
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect(),
                )
            }
            Tx::Written { transaction, .. } if transaction != tx => Some(
                std::iter::once((transaction.key().clone(), transaction.value_ref().clone()))
                    .collect(),
            ),
            _ => None,
        }
    }

    pub(crate) fn changed(&self) -> Option<Change<&T>> {
        match self {
            Tx::Committed { .. } | Tx::Read { .. } => None,
            Tx::Written {
                current, updated, ..
            } => match (current, updated) {
                (None, None) => None,
                (None, Some(v)) => Some(Change::Created(v)),
                (Some(v), None) => Some(Change::Removed(v)),
                (Some(a), Some(b)) => Some(Change::Updated(a, b)),
            },
        }
    }

    /// Open a transaction for reading. Before using this, abort any
    /// transactions returned by `conflicting_for_read`.
    pub(crate) fn read(
        &mut self,
        tx: &TxRef<TransactionId, Transaction>,
    ) -> TxRead<T, TransactionId, Transaction> {
        match self {
            Tx::Committed { current } => {
                *self = Tx::Read {
                    transactions: std::iter::once((tx.key().clone(), tx.value_ref().clone()))
                        .collect(),
                    current: current.take(),
                };
            }
            Tx::Read { transactions, .. } => {
                transactions.insert(tx.key().clone(), tx.value_ref().clone());
            }
            Tx::Written { transaction, .. } => {
                debug_assert!(transaction == tx);
            }
        }
        TxRead(self)
    }

    /// Open a transaction for writing. Before using this, abort any
    /// transactions returned by `conflicting_for_write`.
    pub(crate) fn write(
        &mut self,
        tx: &TxRef<TransactionId, Transaction>,
    ) -> TxWrite<T, TransactionId, Transaction> {
        match self {
            Tx::Committed { current } => {
                *self = Tx::Read {
                    transactions: std::iter::once((tx.key().clone(), tx.value_ref().clone()))
                        .collect(),
                    current: current.take(),
                };
            }
            Tx::Read { transactions, .. } => {
                transactions.insert(tx.key().clone(), tx.value_ref().clone());
                debug_assert!(transactions.len() == 1);
            }
            Tx::Written { transaction, .. } => {
                debug_assert!(transaction == tx);
            }
        }
        TxWrite(self)
    }

    pub(crate) fn commit(&mut self, tx: &TxRef<TransactionId, Transaction>) -> Option<T> {
        match self {
            Tx::Committed { .. } => None,
            Tx::Read {
                transactions,
                current,
            } => {
                transactions.remove(tx.key()).unwrap();
                if transactions.is_empty() {
                    *self = Tx::Committed {
                        current: current.take(),
                    };
                }
                None
            }
            Tx::Written {
                transaction,
                current,
                updated,
            } => {
                debug_assert!(transaction == tx);
                let r = current.take();
                *self = Tx::Committed {
                    current: updated.take(),
                };
                r
            }
        }
    }

    pub(crate) fn abort(&mut self, tx: &TxRef<TransactionId, Transaction>) -> Option<T> {
        match self {
            Tx::Committed { .. } => None,
            Tx::Read {
                transactions,
                current,
            } => {
                transactions.remove(tx.key()).unwrap();
                if transactions.is_empty() {
                    *self = Tx::Committed {
                        current: current.take(),
                    };
                }
                None
            }
            Tx::Written {
                transaction,
                current,
                updated,
            } => {
                debug_assert!(transaction == tx);
                let r = updated.take();
                *self = Tx::Committed {
                    current: current.take(),
                };
                r
            }
        }
    }
}

impl<'a, T, TransactionId, Transaction> TxRead<'a, T, TransactionId, Transaction> {
    /// Get a reference to the new value while running in the current
    /// transaction. Panics if the value was not already read or
    /// written.
    pub(crate) fn updated(&self) -> Option<&'a T> {
        match &self.0 {
            Tx::Committed { .. } => panic!("TxRead::updated called on a committed value"),
            Tx::Read { current, .. } => current.as_ref(),
            Tx::Written { updated, .. } => updated.as_ref(),
        }
    }
}

impl<T, TransactionId, Transaction> TxWrite<'_, T, TransactionId, Transaction> {
    pub(crate) fn current(&self) -> Option<&T> {
        match &*self.0 {
            Tx::Committed { .. } => panic!("TxWrite::current called on a committed value"),
            Tx::Read { current, .. } => current.as_ref(),
            Tx::Written { current, .. } => current.as_ref(),
        }
    }

    pub(crate) fn updated(&self) -> Option<&T> {
        match &self.0 {
            Tx::Committed { .. } => panic!("TxWrite::updated called on a committed value"),
            Tx::Read { current, .. } => current.as_ref(),
            Tx::Written { updated, .. } => updated.as_ref(),
        }
    }

    /// Copy the current value for the transaction to the written
    /// value and return a mutable reference.
    pub(crate) fn modify(&mut self) -> Option<&mut T>
    where
        T: Clone,
    {
        match self.0 {
            Tx::Committed { .. } => panic!("TxWrite::modify called on a committed value"),
            Tx::Read {
                transactions,
                current,
            } => {
                let (id, tx) = std::mem::take(transactions).into_iter().next().unwrap();
                *self.0 = Tx::Written {
                    transaction: RefBy::new(id, tx),
                    current: current.clone(),
                    updated: current.take(),
                };
                match self.0 {
                    Tx::Written { updated, .. } => updated.as_mut(),
                    _ => unreachable!(),
                }
            }
            Tx::Written { updated, .. } => updated.as_mut(),
        }
    }

    /// Update the value for the current transaction, returning any
    /// previously updated value.
    pub(crate) fn update(&mut self, value: T) -> Option<T> {
        match &mut self.0 {
            Tx::Committed { .. } => panic!("TxWrite::update called on a committed value"),
            Tx::Read {
                transactions,
                current,
            } => {
                debug_assert!(transactions.len() == 1);
                let (id, tx) = std::mem::take(transactions).into_iter().next().unwrap();
                *self.0 = Tx::Written {
                    transaction: RefBy::new(id, tx),
                    current: current.take(),
                    updated: Some(value),
                };
                None
            }
            Tx::Written { updated, .. } => updated.replace(value),
        }
    }

    /// Remove the value for the current transaction, returning any
    /// previously updated value.
    pub(crate) fn remove(&mut self) -> Option<T> {
        match self.0 {
            Tx::Committed { .. } => panic!("TxWrite::remove called on a committed value"),
            Tx::Read {
                transactions,
                current,
            } => {
                debug_assert!(transactions.len() == 1);
                let (id, tx) = std::mem::take(transactions).into_iter().next().unwrap();
                *self.0 = Tx::Written {
                    transaction: RefBy::new(id, tx),
                    current: current.take(),
                    updated: None,
                };
                None
            }
            Tx::Written { updated, .. } => updated.take(),
        }
    }
}
