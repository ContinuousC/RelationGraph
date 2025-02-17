/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::sync::Arc;

use actix_web::{
    web::{Data, Json, Path},
    HttpResponse,
};

use apistos::{
    api_operation,
    web::{delete, post, put, Resource, Scope},
};
use relation_graph::{
    serial::{self},
    ItemId, PackageId, RelationId, TransactionId,
};
use tracing::instrument;

use crate::{auth::Auth, http_response, roles::EditorRole, run_updates, AppData, Error, Result};

pub(crate) fn service() -> Scope {
    Scope::new("/transaction")
        .service(Resource::new("").route(put().to(put_transaction)))
        .service(Resource::new("/{tx}/commit").route(put().to(put_transaction_commit)))
        .service(Resource::new("/{tx}/abort").route(put().to(put_transaction_abort)))
        .service(
            Resource::new("/{tx}/item/{item_id}")
                .route(post().to(post_transaction_item))
                .route(put().to(put_transaction_item_with_id))
                .route(delete().to(delete_transaction_item)),
        )
        .service(Resource::new("/{tx}/item").route(put().to(put_transaction_item)))
        .service(
            Resource::new("/{tx}/relation/{rel_id}")
                .route(post().to(post_transaction_relation))
                .route(put().to(put_transaction_relation_with_id))
                .route(delete().to(delete_transaction_relation)),
        )
        .service(Resource::new("/{tx}/relation").route(put().to(put_transaction_relation)))
        .service(Resource::new("/{tx}/items").route(put().to(put_transaction_items)))
        .service(Resource::new("/{tx}/items/{pkg}").route(put().to(put_transaction_items_pkg)))
}

// #[put("")]
#[instrument]
#[api_operation(summary = "Create a transaction")]
async fn put_transaction(role: Auth<EditorRole>, data: Data<AppData>) -> HttpResponse {
    http_response(Ok(data
        .write_state(role.into_inner())
        .items
        .create_transaction()))
}

// #[put("/{tx}/commit")]
#[instrument]
#[api_operation(summary = "Commit a transaction")]
async fn put_transaction_commit(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    tx: Path<TransactionId>,
) -> HttpResponse {
    async fn run(role: EditorRole, data: &AppData, tx: TransactionId) -> Result<()> {
        let updates = {
            let mut state = data.write_state(role);
            state.commit_transaction(tx)?
        };
        run_updates(data, updates).await
    }
    http_response(run(role.into_inner(), &data.into_inner(), tx.into_inner()).await)
}

// #[put("/{tx}/abort")]
#[instrument]
#[api_operation(summary = "Abort a transaction")]
async fn put_transaction_abort(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    tx: Path<TransactionId>,
) -> HttpResponse {
    http_response({
        data.write_state(role.into_inner())
            .items
            .abort_transaction(tx.into_inner());
        Ok(())
    })
}

// #[post("/{tx}/item/{item_id}")]
#[instrument]
#[api_operation(summary = "Read an item inside a transaction")]
async fn post_transaction_item(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<(TransactionId, ItemId)>,
) -> HttpResponse {
    let (tx, item_id) = path.into_inner();
    http_response(
        data.write_state(role.into_inner())
            .items
            .tx_read_item(tx, &item_id)
            .map_err(Error::Lib)
            .and_then(|item| {
                item.map(|item| item.to_serial(None))
                    .ok_or_else(|| Error::ItemNotFound(item_id))
            }),
    )
}

// #[put("/{tx}/item")]
#[instrument]
#[api_operation(summary = "Create an item inside a transaction")]
async fn put_transaction_item(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    tx: Path<TransactionId>,
    item: Json<serial::Item>,
) -> HttpResponse {
    let item_id = ItemId::new();
    http_response(run_put_tx_item(
        role.into_inner(),
        data.into_inner(),
        tx.into_inner(),
        item_id,
        item.into_inner(),
    ))
}

// #[put("/{tx}/item/{item_id}")]
#[instrument]
#[api_operation(summary = "Create or update an item inside a transaction")]
async fn put_transaction_item_with_id(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<(TransactionId, ItemId)>,
    item: Json<serial::Item>,
) -> HttpResponse {
    let (tx, item_id) = path.into_inner();
    http_response(run_put_tx_item(
        role.into_inner(),
        data.into_inner(),
        tx,
        item_id,
        item.into_inner(),
    ))
}

fn run_put_tx_item(
    role: EditorRole,
    data: Arc<AppData>,
    tx: TransactionId,
    id: ItemId,
    item: serial::Item,
) -> Result<()> {
    let mut state = data.write_state(role);
    state.tx_insert_item(tx, id, item, None)?;
    Ok(())
}

// #[delete("/{tx}/item/{item_id}")]
#[instrument]
#[api_operation(summary = "Delete an item inside a transaction")]
async fn delete_transaction_item(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<(TransactionId, ItemId)>,
) -> HttpResponse {
    fn run(role: EditorRole, data: &AppData, tx: TransactionId, item_id: ItemId) -> Result<()> {
        let mut state = data.write_state(role);
        state.items.tx_remove_item(tx, &item_id)?;
        Ok(())
    }
    let (tx, item_id) = path.into_inner();
    http_response(run(role.into_inner(), &data.into_inner(), tx, item_id))
}

// #[post("/{tx}/relation/{rel_id}")]
#[instrument]
#[api_operation(summary = "Read a relation inside a transaction")]
async fn post_transaction_relation(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<(TransactionId, RelationId)>,
) -> HttpResponse {
    let (tx, rel_id) = path.into_inner();
    http_response(
        data.write_state(role.into_inner())
            .items
            .tx_read_relation(tx, &rel_id)
            .map_err(Error::Lib)
            .and_then(|rel| {
                rel.map(|rel| rel.to_serial(None))
                    .ok_or(Error::RelationNotFound(rel_id))
            }),
    )
}

// #[put("/{tx}/relation")]
#[instrument]
#[api_operation(summary = "Create a relation inside a transaction")]
async fn put_transaction_relation(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    tx: Path<TransactionId>,
    relation: Json<serial::Relation>,
) -> HttpResponse {
    let rel_id = RelationId::new();
    http_response(run_put_tx_relation(
        role.into_inner(),
        data.into_inner(),
        tx.into_inner(),
        rel_id.clone(),
        relation.into_inner(),
    ))
}

// #[put("/{tx}/relation/{rel_id}")]
#[instrument]
#[api_operation(summary = "Create or update a relation inside a transaction")]
async fn put_transaction_relation_with_id(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<(TransactionId, RelationId)>,
    relation: Json<serial::Relation>,
) -> HttpResponse {
    let (tx, rel_id) = path.into_inner();
    http_response(run_put_tx_relation(
        role.into_inner(),
        data.into_inner(),
        tx,
        rel_id,
        relation.into_inner(),
    ))
}

fn run_put_tx_relation(
    role: EditorRole,
    data: Arc<AppData>,
    tx: TransactionId,
    id: RelationId,
    relation: serial::Relation,
) -> Result<()> {
    let mut state = data.write_state(role);
    state.tx_insert_relation(tx, id, relation, None)?;
    Ok(())
}

// #[delete("/{tx}/relation/{rel_id}")]
#[instrument]
#[api_operation(summary = "Delete a relation inside a transaction")]
async fn delete_transaction_relation(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<(TransactionId, RelationId)>,
) -> HttpResponse {
    fn run(role: EditorRole, data: &AppData, tx: TransactionId, rel_id: RelationId) -> Result<()> {
        let mut state = data.write_state(role);
        state.items.tx_remove_relation(tx, &rel_id)?;
        Ok(())
    }
    let (tx, rel_id) = path.into_inner();
    http_response(run(role.into_inner(), &data.into_inner(), tx, rel_id))
}

// #[put("/{tx}/items")]
#[instrument(skip(items))]
#[api_operation(summary = "Bulk update items and relations within a transaction")]
async fn put_transaction_items(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    tx: Path<TransactionId>,
    items: Json<super::items::Items>,
) -> HttpResponse {
    let items = items.into_inner();
    http_response(run_put_tx_items(
        role.into_inner(),
        data.into_inner(),
        tx.into_inner(),
        items.domain,
        items.items,
        None,
    ))
}

// #[put("/{tx}/items/{pkg}")]
#[instrument(skip(items))]
#[api_operation(
    summary = "Bulk update items and relations within a transaction, relative to a package"
)]
async fn put_transaction_items_pkg(
    role: Auth<EditorRole>,
    data: Data<AppData>,
    path: Path<(TransactionId, PackageId)>,
    items: Json<super::items::Items>,
) -> HttpResponse {
    let (tx, pkg) = path.into_inner();
    let items = items.into_inner();
    http_response(run_put_tx_items(
        role.into_inner(),
        data.into_inner(),
        tx,
        items.domain,
        items.items,
        Some(&pkg),
    ))
}

fn run_put_tx_items(
    role: EditorRole,
    data: Arc<AppData>,
    tx: TransactionId,
    domain: serial::Domain,
    items: serial::Items,
    pkg: Option<&PackageId>,
) -> Result<()> {
    let mut state = data.write_state(role);
    state.tx_insert_items(tx, domain, items, pkg)?;
    Ok(())
}
