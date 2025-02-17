/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

mod browse;
mod generate;

use apistos::web::{delete, get, post, put, Resource, Scope};

pub(crate) use generate::PromSchemaState;

pub(crate) fn service() -> Scope {
    Scope::new("/prom-schema")
        .service(Resource::new("tree").route(get().to(browse::get_tree)))
        .service(Resource::new("info/tree{path:/.*|$}").route(get().to(browse::get_tree_info)))
        .service(Resource::new("info/module").route(get().to(browse::get_modules)))
        .service(Resource::new("info/module/{mod:[^/]+}").route(get().to(browse::get_mod_info)))
        .service(
            Resource::new("info/item/{item:[^/:]+:[^/]+}").route(get().to(browse::get_item_info)),
        )
        .service(Resource::new("items").route(get().to(browse::get_prom_items)))
        .service(Resource::new("items/tree{path:/.*|$}").route(post().to(browse::post_tree_items)))
        .service(
            Resource::new("metrics/tree{path:/.*|$}").route(post().to(browse::post_tree_metrics)),
        )
        .service(
            Scope::new("generate")
                .service(Resource::new("").route(get().to(generate::get_modules)))
                .service(
                    Resource::new("{module}")
                        .route(get().to(generate::get_module))
                        .route(put().to(generate::put_module))
                        .route(delete().to(generate::delete_module)),
                )
                .service(
                    Resource::new("{module}/rename").route(post().to(generate::post_rename_module)),
                )
                .service(
                    Resource::new("{module}/metrics")
                        .route(get().to(generate::get_loaded_metrics))
                        .route(post().to(generate::post_load_metrics)),
                )
                .service(Resource::new("{module}/tree").route(get().to(generate::get_tree)))
                .service(
                    Resource::new("{module}/info/tree{path:/.*|$}")
                        .route(get().to(generate::get_tree_info)),
                )
                .service(
                    Resource::new("{module}/items/tree{path:/.*|$}")
                        .route(post().to(generate::post_tree_items)),
                )
                .service(
                    Resource::new("{module}/metrics/tree{path:/.*|$}")
                        .route(post().to(generate::post_tree_metrics)),
                )
                .service(
                    Resource::new("{module}/choose/tree{path:/.*|$}")
                        .route(put().to(generate::put_choose_at_path))
                        .route(delete().to(generate::delete_choose_at_path)),
                )
                .service(
                    Resource::new("{module}/rename/{item}")
                        .route(put().to(generate::put_rename_item))
                        .route(delete().to(generate::delete_rename_item)),
                )
                .service(
                    Resource::new("{module}/split/tree{path:/.*|$}")
                        .route(put().to(generate::put_split_by_label))
                        .route(delete().to(generate::delete_split_by_label)),
                )
                .service(
                    Resource::new("{module}/split/query")
                        .route(put().to(generate::put_split_by_label_query))
                        .route(delete().to(generate::delete_split_by_label_query)),
                )
                .service(
                    Resource::new("{module}/download")
                        .route(get().to(generate::get_module_download)),
                )
                .service(
                    Resource::new("{module}/metrics/download")
                        .route(get().to(generate::get_metrics_download)),
                )
                .service(
                    Resource::new("{module}/hints/download")
                        .route(get().to(generate::get_hints_download)),
                ),
        )
}
