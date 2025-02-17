/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

// import { JsTypes, JsItems, JsQuery, JsItemMetrics, JsTypeMetrics, JsStylesheet } from '../pkg';

// import kubernetes from '../../tests/kubernetes/pkgs/kubernetes.json';
// import items_json from '../../tests/kubernetes/items.json';
// import query_json from '../../tests/kubernetes/query_workloads.json';
// import metrics_json from './pod-metrics.json';


// const types = new JsTypes({ kubernetes });

// const items = new JsItems(types, items_json, "kubernetes");
// const query = new JsQuery(types, query_json, "kubernetes");
// const result = query.run(items);

// console.log(JSON.stringify(items.getItemTypes()))
// console.log(JSON.stringify(result.getItemTypes()))
// console.log(JSON.stringify(types.getColumnDefs("kubernetes/cluster_resource").map((e) => e.accessorKey)))
// console.log(JSON.stringify(result.getTableData("kubernetes/cluster_resource").map((e) => e.props)))
// console.log(JSON.stringify(result.getTemplateOptions(), undefined, "   "));

// const metrics = new JsTypeMetrics(metrics_json);


// const defaultSources = Object.fromEntries(Object.entries(metrics.sources()).map(([itemType, instances]) => [itemType, Object.keys(instances)[0]]))
// console.log(metrics.metrics(defaultSources))


type TimeUnit = "second" | "minute" | "hour";
const timeUnits = { "second": "second", "minute": "minute", "hour": "hour" } as const;

const myParam: string = "second";
const p: TimeUnit | "auto" = timeUnits[myParam] || "auto";
console.log(Object.keys(timeUnits))
