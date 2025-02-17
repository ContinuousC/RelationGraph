/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;

use actix_web::web::Data;
use tracing::{instrument, span, Instrument, Level};

use crate::{roles::EditorRole, run_updates, AppData, Result};

pub(crate) async fn connector_runner(
    role: EditorRole,
    data: Data<AppData>,
    interval: std::time::Duration,
    mut term_receiver: tokio::sync::watch::Receiver<bool>,
) {
    let mut interval = tokio::time::interval(interval);
    interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

    loop {
        tokio::select! {
            _ = term_receiver.changed() => if *term_receiver.borrow() {
                break
            },
            _ = interval.tick() => async {
                log::info!("updating declarative relations ...");
                match update_relations(role, &data).await {
                    Ok((found, added,removed)) => {
                        log::info!("succesfully updated declarative relations: found {found} relation(s), added {added} relation(s), removed {removed} relation(s)");
                    },
                    Err(e) => {
                        log::warn!("failed to update declarative relations: {e}");
                    }
                }
            }.instrument(span!(Level::INFO, "updating declarative relations")).await
        }
    }
}

#[instrument]
async fn update_relations(role: EditorRole, data: &AppData) -> Result<(usize, usize, usize)> {
    let ((updates, added, removed), found) = {
        let mut state = data.write_state(role);
        let relations = state
            .types
            .relations
            .iter_ref_by()
            .filter_map(|rel_type_ref| {
                let rel_type = state.types.relations.borrow(&rel_type_ref);
                let connector = rel_type.connector.as_ref()?;
                let relations = connector.run(&state.items.items, &state.types, &rel_type_ref);
                Some((rel_type_ref.key().clone(), relations))
            })
            .collect::<BTreeMap<_, _>>();

        let found = relations.values().map(|rels| rels.len()).sum::<usize>();
        (state.replace_resolved_relations(relations)?, found)
    };

    // log::debug!(
    //     "Declarative relation updates: {}",
    //     serde_json::to_string_pretty(&updates).unwrap()
    // );

    run_updates(data, updates).await?;
    Ok((found, added, removed))
}
