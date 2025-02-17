/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{os::unix::prelude::OsStrExt, path::Path, str::FromStr};

use relation_graph::{serial, serial::Packages, Items, PackageId, Query, Types};
use wrapper::Identity;

#[macro_export]
macro_rules! define_tests {
    ($test:ident, [ $( $(#[$attr:meta])* ( $case:ident, $path:literal, $query:literal ) ),+ ]) => {
		$(
			#[test]
			$(#[$attr])*
			fn $case() {
				let (types, items, query) = cases::load($path, $query);
				$test(&types, &items, &query);
			}
		)*
    };
}

pub fn load(path: &str, query: &str) -> (Types, Items<Identity>, Query) {
    let pkg = PackageId::from_str(path).unwrap();
    let path = Path::new("../tests").join(path);

    let pkgs = load_packages(&path.join("pkgs"));
    let types = pkgs.types().unwrap();
    let items = load_items(&types, &path.join("items.json"), Some(&pkg));
    let query = load_query(&types, &path.join(query), Some(&pkg));

    (types, items, query)
}

fn load_packages(pkgs_path: &Path) -> Packages {
    let mut pkgs = Packages::new();

    std::fs::read_dir(pkgs_path)
        .unwrap()
        .filter_map(|ent| {
            let ent = ent.unwrap();
            (ent.file_type().unwrap().is_file() && ent.file_name().as_bytes().ends_with(b".json"))
                .then(|| ent.path())
        })
        .for_each(|path| {
            let id = PackageId::from_str(&path.file_stem().unwrap().to_string_lossy()).unwrap();
            let pkg = serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
            pkgs.insert(id, pkg);
        });

    pkgs
}

fn load_items(types: &Types, path: &Path, pkg: Option<&PackageId>) -> Items<Identity> {
    serde_json::from_str::<serial::Items>(&std::fs::read_to_string(path).unwrap())
        .unwrap()
        .resolve(types, pkg)
        .unwrap()
}

fn load_query(types: &Types, path: &Path, pkg: Option<&PackageId>) -> Query {
    serde_json::from_str::<serial::Query>(&std::fs::read_to_string(path).unwrap())
        .unwrap()
        .resolve(types, pkg)
        .unwrap()
}
