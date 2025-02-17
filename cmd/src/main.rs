/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

mod error;

use std::{
    collections::{BTreeMap, BTreeSet},
    os::unix::prelude::OsStrExt,
    path::PathBuf,
    process::ExitCode,
    time::Instant,
};

use clap::Parser;

use dbschema_elastic::ElasticFilter;
use prometheus_core::{LabelName, MetricName};
use prometheus_expr::{Expr, MetricSelector};
use prometheus_schema::Universe;
use relation_graph::{
    alerts::{
        AlertConfigName, AlertRuleConfigs, AlertRuleTemplateName, AlertRuleTemplates,
        AlertRulesState, RecordRule, RuleGroup, UnverifiedAlertRuleForms, VersionedAlertMap,
    },
    db::DbItem,
    serial::{self},
    status::VersionedStatusMap,
    ConnectionsPackages, Dashboards, Item, ItemId, Items, PackageId, Packages, Query,
    QueryResultItems, State, Types, Views,
};

use error::{Error, Result};

/// Run relation graph operations from the command line.
#[derive(clap::Parser)]
struct Args {
    #[command(subcommand)]
    cmd: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    Query(QueryArgs),
    SearchDomain(QueryArgs),
    EsQuery(EsQueryArgs),
    PromQuery(PromQueryArgs),
    // PromExpr(PromExprArgs),
    AlertRule(AlertRuleArgs),
    StatusRule(StatusRuleArgs),
    Status(StatusArgs),
}

/// Run a query on a relation graph.
#[derive(clap::Args)]
struct QueryArgs {
    #[clap(long)]
    bench: bool,
    #[clap(long, help = "Show graphical representation instead of JSON output")]
    show: bool,
    #[clap(long, help = "Limit the result size")]
    limit: Option<usize>,
    #[clap(short, long)]
    pkg: Option<PackageId>,
    pkgs: PathBuf,
    items: PathBuf,
    query: PathBuf,
}

/// Generate an elasticsearch query for a relation graph query.
#[derive(clap::Args)]
struct EsQueryArgs {
    #[clap(short, long)]
    pkg: Option<PackageId>,
    pkgs: PathBuf,
    query: PathBuf,
}

/// Generate prometheus queries for an item in the relation graph.
#[derive(clap::Args)]
struct PromQueryArgs {
    #[clap(short, long)]
    pkg: Option<PackageId>,
    /// Path to the package definition folder.
    pkgs: PathBuf,
    /// Path to the connections package definition folder.
    conns: PathBuf,
    /// Path to the prometheus schema root.
    root: PathBuf,
    /// Path to the relation graph in json format.
    items: PathBuf,
    /// The id of the item for which to generate queries.
    item: ItemId,
}

/// Parse (our superset of) a prometheus expression.
#[derive(clap::Args)]
struct PromExprArgs {
    #[clap(short, long)]
    selectors: Vec<String>,
    #[clap(short, long)]
    params: Vec<String>,
    spec: String,
}

/// Generate prometheus alert rules.
#[derive(clap::Args)]
struct AlertRuleArgs {
    /// The prometheus schema root.
    root: PathBuf,
    /// The alert rule definition.
    rule: PathBuf,
    /// The alert rule value.
    config: PathBuf,
}

/// Generate prometheus alert rules.
#[derive(clap::Args)]
struct StatusRuleArgs {
    /// Path to the package definition folder.
    pkgs: PathBuf,
    /// Path to the connections package definition folder.
    conns: PathBuf,
    /// The prometheus schema root.
    root: PathBuf,
    /// The alert rule definitions.
    rules: Vec<PathBuf>,
}

/// Query prometheus for an item's status.
#[derive(clap::Args)]
struct StatusArgs {
    #[clap(short, long)]
    pkg: Option<PackageId>,
    /// Path to the package definition folder.
    pkgs: PathBuf,
    /// Path to the connections package definition folder.
    conns: PathBuf,
    /// The prometheus schema root.
    root: PathBuf,
    /// Path to the relation graph in json format.
    items: PathBuf,
    /// The id of the item for which to query status.
    item: ItemId,
    /// Alert rule definitions.
    rules: Vec<PathBuf>,
}

fn main() -> ExitCode {
    env_logger::init();

    let args = Args::parse();

    let res = match &args.cmd {
        Command::Query(args) => query(args),
        Command::SearchDomain(args) => search_domain(args),
        Command::EsQuery(args) => es_query(args),
        Command::PromQuery(args) => prom_query(args),
        // Command::PromExpr(args) => prom_expr(args),
        Command::AlertRule(args) => alert_rule(args),
        Command::StatusRule(args) => status_rule(args),
        Command::Status(args) => query_status(args),
    };

    if let Err(e) = res {
        eprintln!("Error: {e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn benchmark<F, R>(flag: bool, msg: &str, f: F) -> R
where
    F: FnOnce() -> R,
{
    try_benchmark(flag, msg, || Ok(f())).unwrap()
}

fn try_benchmark<F, R>(flag: bool, msg: &str, f: F) -> Result<R>
where
    F: FnOnce() -> Result<R>,
{
    if flag {
        let start = Instant::now();
        let r = f()?;
        eprintln!(
            "{msg} took {:.3}ms",
            Instant::now().duration_since(start).as_micros() as f64 / 1000.0
        );
        Ok(r)
    } else {
        f()
    }
}

fn load_query(args: &QueryArgs) -> Result<(Types, Items, Query)> {
    let pkg = args.pkg.as_ref();

    let packages = try_benchmark(args.bench, "Loading packages", || {
        Ok(Packages::load_sync(&args.pkgs)?)
    })?;
    let types = try_benchmark(args.bench, "Calculating types", || Ok(packages.types()?))?;

    let items = try_benchmark(args.bench, "Loading items", || {
        Ok(serial::Items::load(&args.items)?)
    })?;
    let items = try_benchmark(args.bench, "Resolving items", || {
        Ok(items.resolve(&types, pkg)?)
    })?;

    let query = try_benchmark(args.bench, "Loading query", || {
        Ok(serial::Query::load(&args.query)?.resolve(&types, pkg)?)
    })?;

    Ok((types, items, query))
}

fn query(args: &QueryArgs) -> Result<()> {
    let (types, items, query) = load_query(args)?;

    let result = benchmark(args.bench, "Running query", || {
        query.run_with_filters(&items, &types, query.default_filters(), &(), args.limit)
    });
    let result_items = result.get_result().map_err(Error::RunQuery)?;

    if args.show {
        show_query_result(result_items, &items, &types);
    } else {
        let serialized = benchmark(args.bench, "Serialization", || {
            result.to_serial_unwrapped(&items, args.pkg.as_ref())
        });
        println!("{}", serde_json::to_string_pretty(&serialized).unwrap());
    }

    Ok(())
}

fn show_query_result(result: &QueryResultItems, items: &Items, types: &Types) {
    fn show_item_tree<'a>(
        item_id: &'a ItemId,
        item: &Item,
        result: &QueryResultItems,
        items: &'a Items,
        types: &Types,
        seen: &mut BTreeSet<&'a ItemId>,
        indent: u8,
    ) {
        (0..indent).for_each(|_| print!("   "));
        println!("- {} {}", item.item_type_id(), item.name(types));

        if seen.insert(item_id) {
            for (_, rel_ref) in item
                .targets(None)
                .filter(|(rel_id, _)| result.contains_relation(rel_id))
            {
                let rel = items.borrow_relation(rel_ref);
                show_item_tree(
                    rel.target_id(),
                    rel.target(items),
                    result,
                    items,
                    types,
                    seen,
                    indent + 1,
                );
            }
        }
    }

    let mut seen = BTreeSet::new();

    while let Some((root_id, root)) = result
        .iter_items(items)
        .filter(|(item_id, _)| !seen.contains(item_id))
        .min_by_key(|(_, item)| {
            item.sources(None)
                .filter(|(rel_id, _)| result.contains_relation(rel_id))
                .count()
        })
    {
        show_item_tree(root_id, root, result, items, types, &mut seen, 0);
    }
}

fn search_domain(args: &QueryArgs) -> Result<()> {
    let (types, items, query) = load_query(args)?;

    let result = benchmark(args.bench, "Running query", || {
        query
            .search_domain(&items, &types)
            .to_serial_unwrapped(&items, args.pkg.as_ref())
    });

    println!("{}", serde_json::to_string_pretty(&result).unwrap());

    Ok(())
}

fn es_query(args: &EsQueryArgs) -> Result<()> {
    let pkg = args.pkg.as_ref();
    let types = Packages::load_sync(&args.pkgs)?.types()?;
    let schema = DbItem::schema(&types);
    let query = serial::Query::load(&args.query)?.resolve(&types, pkg)?;

    let result = query.elastic_query(&types);

    if result.items.is_none() {
        eprintln!("Warning: items are unrestricted; try specifying item types for all elements!")
    }

    if result.relations.is_none() {
        eprintln!(
            "Warning: relations are unrestricted; try specifying relation types for all elements!"
        )
    }

    let filter = ElasticFilter::new(&schema, &result.into_filter()).map_err(Error::EsFilter)?;
    println!("{}", serde_json::to_string_pretty(&filter).unwrap());

    Ok(())
}

fn prom_query(args: &PromQueryArgs) -> Result<()> {
    let pkg = args.pkg.as_ref();
    let packages = Packages::load_sync(&args.pkgs)?;
    let types = packages.types()?;
    let connections = ConnectionsPackages::load_sync(&args.conns)?;
    let (root, modules) = Universe::load_sources_sync(&args.root)?;
    //types.resolve_connections(&connections, &packages, &schema)?;
    let items = serial::Items::load(&args.items)?.resolve_tx(&types, pkg)?;
    let mut state = State::new(packages, types, items);
    state.load_connections(connections, root, modules)?;
    let queries = relation_graph::metrics::item_queries(&state, &args.item)?;

    // let item = items
    //     .get_item(&args.item)
    //     .ok_or_else(|| Error::MissingItem(args.item.clone()))?;

    println!("Prometheus queries for {}:", args.item);

    for (prom_item, qs) in &queries.0 {
        println!("- {prom_item}: {}", qs.item);
        for (instance, inst_query) in &qs.instances {
            println!("   - {instance}: {inst_query}");
            for (metric, metric_query) in &qs.metrics {
                println!(
                    "      - {metric} {}",
                    relation_graph::metrics::metric_query(metric, metric_query)
                        & &qs.item
                        & inst_query
                )
            }
        }
    }

    Ok(())
}

// fn prom_expr(args: &PromExprArgs) -> Result<()> {
//     let spec = PromExprSpec::from_str(&args.spec).map_err(Error::ParsePromExprSpec)?;
//     let selectors = args
//         .selectors
//         .iter()
//         .map(|selector| {
//             let (name, value) = selector.split_once('=').ok_or(Error::ParseParam)?;
//             Result::Ok((
//                 LabelName::new(name.to_string()),
//                 LabelSelector::Eq(value.to_string()),
//             ))
//         })
//         .collect::<Result<_>>()?;
//     let params = args
//         .params
//         .iter()
//         .map(|param| {
//             let (name, value) = param.split_once('=').ok_or(Error::ParseParam)?;
//             Result::Ok((
//                 ParamName::new(name.to_string()),
//                 f64::from_str(value).map_err(Error::ParseParamValue)?,
//             ))
//         })
//         .collect::<Result<BTreeMap<_, _>>>()?;
//     let expr = spec.with_params(
//         &selectors,
//         &params
//             .iter()
//             .map(|(param, value)| (param, *value))
//             .collect(),
//     );

//     println!("{expr}");
//     Ok(())
// }

fn alert_rule(args: &AlertRuleArgs) -> Result<()> {
    let schema = Universe::load_sync(&args.root)?;
    AlertRuleTemplateName::new(
        std::str::from_utf8(
            args.rule
                .file_name()
                .ok_or(Error::AlertRuleName)?
                .as_bytes(),
        )
        .map_err(|_| Error::AlertRuleName)?
        .strip_suffix(".spec.yaml")
        .ok_or(Error::AlertRuleName)?
        .to_string(),
    );
    let template = serde_yaml::from_str::<AlertRuleTemplates>(
        &std::fs::read_to_string(&args.rule).map_err(Error::ReadRuleSpec)?,
    )
    .map_err(Error::DecodeRuleSpec)?;
    let config = serde_yaml::from_str::<AlertRuleConfigs>(
        &std::fs::read_to_string(&args.rule).map_err(Error::ReadRuleConfig)?,
    )
    .map_err(Error::DecodeRuleConfig)?;

    let configs = BTreeMap::from([(AlertConfigName::new("mm".to_string()), config)]);
    let name = &template.get_name().clone();
    let rule_form = UnverifiedAlertRuleForms::new(template, configs)
        .ok_or(Error::TemplateConfigtypeMistmatch)?
        .verify(name, true)?;

    let rules = rule_form.rules(&schema).map_err(Error::AlertRules)?;

    println!(
        "{}",
        serde_yaml::to_string(&RuleGroup {
            name: String::from("continuousc-alerts"),
            rules
        })
        .unwrap()
    );

    Ok(())
}

fn status_rule(args: &StatusRuleArgs) -> Result<()> {
    let packages = Packages::load_sync(&args.pkgs)?;
    let connections = ConnectionsPackages::load_sync(&args.conns)?;
    let mut types = packages.types()?;
    let schema = Universe::load_sync(&args.root)?;
    types.resolve_connections(&connections, &packages, &schema)?;

    let mut alerts_by_item =
        args.rules
            .iter()
            .try_fold(BTreeMap::<_, BTreeSet<_>>::new(), |mut map, spec| {
                let template = serde_yaml::from_str::<AlertRuleTemplates>(
                    &std::fs::read_to_string(spec).map_err(Error::ReadRuleSpec)?,
                )
                .map_err(Error::DecodeRuleSpec)?;
                let name = &template.get_name().clone();
                let rule_form = UnverifiedAlertRuleForms::new(template, BTreeMap::new())
                    .ok_or(Error::TemplateConfigtypeMistmatch)?
                    .verify(name, true)?;
                map.entry(rule_form.item())
                    .or_default()
                    .insert(rule_form.get_name().to_string());
                Result::Ok(map)
            })?;

    let rules = types
        .items
        .iter()
        .flat_map(|(_, item)| item.prometheus_items(&types, &schema))
        .filter_map(|(name, _, link)| {
            let alerts = alerts_by_item.remove(name)?;

            let alerts = Expr::metric(
                MetricSelector::new()
                    .metric(MetricName::new_static("ALERTS"))
                    .label_in(LabelName::new_static("alertrule"), &alerts)
                    .label_eq(LabelName::new_static("alertstate"), "firing"),
            );

            Some(RecordRule {
                record: MetricName::new_static("STATUS"),
                labels: std::iter::once((LabelName::new_static("item"), name.to_string()))
                    .collect(),
                expr: alerts.sum_by(
                    link.labels()
                        .cloned()
                        .chain(std::iter::once(LabelName::new_static("severity")))
                        .collect(),
                ),
            })
        })
        .collect();

    println!(
        "{}",
        serde_yaml::to_string(&RuleGroup {
            name: String::from("continuousc-status"),
            rules
        })
        .unwrap()
    );

    Ok(())
}

fn query_status(args: &StatusArgs) -> Result<()> {
    let pkg = args.pkg.as_ref();
    let packages = Packages::load_sync(&args.pkgs)?;
    let types = packages.types()?;
    let connections = ConnectionsPackages::load_sync(&args.conns)?;
    let (root, modules) = Universe::load_sources_sync(&args.root)?;
    let items = serial::Items::load(&args.items)?.resolve_tx(&types, pkg)?;
    let mut state = State {
        packages,
        types,
        items,
        alert_rules: AlertRulesState::default(),
        status: VersionedStatusMap::default(),
        alerts: VersionedAlertMap::default(),
        views: Views::default(),
        dashboards: Dashboards::default(),
        connections: None,
        prometheus: None,
    };
    state.load_connections(connections, root, modules)?;

    let item = state
        .items
        .get_item(&args.item)
        .ok_or_else(|| Error::MissingItem(args.item.clone()))?;
    let prom_items = item
        .item_type(&state.types)
        .prometheus_items(&state.types, state.get_prometheus_schema().unwrap())
        .map(|(name, _, link)| (name, link))
        .collect::<BTreeMap<_, _>>();

    let alerts =
        args.rules
            .iter()
            .try_fold(BTreeMap::<_, BTreeSet<_>>::new(), |mut map, spec| {
                let template = serde_yaml::from_str::<AlertRuleTemplates>(
                    &std::fs::read_to_string(spec).map_err(Error::ReadRuleSpec)?,
                )
                .map_err(Error::DecodeRuleSpec)?;
                let name = &template.get_name().clone();
                let rule_form = UnverifiedAlertRuleForms::new(template, BTreeMap::new())
                    .ok_or(Error::TemplateConfigtypeMistmatch)?
                    .verify(name, true)?;
                if prom_items.contains_key(&rule_form.item()) {
                    map.entry(rule_form.item())
                        .or_default()
                        .insert(rule_form.get_name().to_string());
                }
                Result::Ok(map)
            })?;

    let query = alerts
        .into_iter()
        .map(|(name, alerts)| {
            let link = prom_items.get(&name).unwrap();
            let query = link.prometheus_query(item, &state.items.items, &state.types);
            Expr::metric(
                MetricSelector::new()
                    .metric(MetricName::new_static("ALERTS"))
                    .labels(query)
                    .label_in(LabelName::new_static("alertrule"), &alerts),
            )
        })
        .reduce(Expr::or)
        .map(|expr| expr.sum_by(vec![LabelName::new_static("severity")]));

    if let Some(query) = query {
        println!("Prometheus status query for {}:", args.item);
        println!("- {query}");
    } else {
        println!("No alert rules found for {}", args.item);
    }

    Ok(())
}
