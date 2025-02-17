# Alert and status handling

In order to configure alerts on metric data, we generate prometheus alert
definitions from a set of rules and rule configs and send these to the Cortex
Ruler. The Ruler evaluates these rules every minute, creating alert metrics for
triggered alert rules. Firing alerts are present in the database, while alerts
that are not firing are not.

A process in the Relation Graph Engine that runs at the same frequency as the
Ruler, picks up these triggered alert metrics, filters them using its knowledge
of the original, higher-level, alert config and write the results as alert
changes to an Opensearch index. Alert data is further processed into a
per-object status. Status changes are written to a separate status index in
Opensearch.

## Alert rule templates

Alert rule templates specify the Prometheus item they apply to, a parametrized
Prometheus expression along with templatable alert annotations, an `instance`
setting, the output unit for the expression's value and a map of parameters with
their type, default value and display options. Specialized templates for
application alerting (where the expression is pre-defined based on a number of
settings) are also provided.

Two kinds of parameters can be specified. A simple `parameter` takes a single
value for every alert rule. For `threshold` parameters, an alert rule is allowed
to set different values for every severity (`minor`, `warning`, `major` and
`critical`).

Parameter types are `integer`, `duration` and `quantity`. For the `quantity`
type, a `dimension` must also be specified, with `dimensionless` denoting a
simple floating-point value. `Quantity` parameters can be specified with an
appropriate unit in alert rules, and are converted to the requested unit in
expressions and autoscaled in alert annotations.

The choice for `instance` determines what to in case multiple sources are
available for the selected Prometheus item. A value of `individual` means
individual alert rules are created for each source. If the instance is `best`,
one alert rule is created, with an expression that only fires for time-series
present in all sources (i.e. the expressions for all sources are AND-ed
together). Conversely, if the `worst` `instance` is selected, an alert rule is
generated with an expression that fires for time-series present in at least one
source (i.e. the expressions for all sources are OR-ed together).

### Parametrized Prometheus expressions

Parameterized prometheus expressions follow the syntax of standard prometheus
expressions (though not all features are currently implemented), and extend them
with the following:

- In places where a _number_ is expected, you can additionally refer to a
 `quantity` parameter using the `$param` or `${param}` syntax. In the second
 case, an output unit can be specified as `${param(unit)}`. If no output unit is
 specified, the value is converted into the base value for the parameter's
 dimension. For example:

 ```
 memory_usage_bytes > ${max(B)}
 ```

- In places where a Prometheus-style _duration_ is expected, you can
additionally refer to a `duration` parameter using the `$param` or `${param}`
syntax. In this case no additional argument is possible. For example:

 ```
 sum by (another_label)
    ( metric { label = "value" } [$range] /
      another_metric { label = "value" } [$range] )
 ```

- In places where an _integer_ is expected, you can additionally refer to an
`integer` parameter using the `$param` or `${param}` syntax. In this case no
additional argument is possible. For example:

 ```
 topk($num, metric { label = "value" })
 ```

### Alert annotation templates

Alert rule annotations in the Relation Graph are templates for Prometheus alert
annotation templates. You can refer to the expression's output value at
evaluation time using the syntax `$value` or `${value}`. This is replaced with
an appropriate Prometheus template segment to autoscale the value. The output
already includes the unit. Alternatively, you can specify the output unit using
the `${value(unit)}` syntax, in which case the template segment will not include
the output unit.

Parameter values can be included in the alert annotation using the
`$params.myParam`, `${params.myParam}` or `${params.myParam(unit)}` syntax, with
the same result as above, except that the output will be included literally in
the template instead of a template segment, since the parameter values are
already know when the template is rendered.

To include a dollar sign in the rendered template, it can be escaped as `$$`.
This allows including template segments to be interpreted by the Prometheus
Ruler.

The following example...

```
pod {{ $$labels.namespace }} : {{ $$labels.pod }} has high memory usage ($value > $params.min)
```

...is rendered to a prometheus alert annotation template like...

```
pod {{ $labels.namespace }} : {{ $labels.pod }} has high memory usage ({{ $value | humanize1024 }}B > 5GB)
```

...which is in turn rendered to an alert annotation as...

```
pod cortex : cortex-ingester-1 has high memory usage (6.543GiB > 5.000GB)
```

## Alert rules

For every alert rule template, zero or more rules can be defined. Every rule has
a name and must specify values for the parameters and a minimum duration for a
time series to be seen in the result of the query, before an alert is considered
to be `firing`. Additionally, it can specify threshold values, label selectors
and additional output labels to be included on the alert.

The order of alert rules is currently not important. Alerts fire for all matched
items. This may change in the future.

## Alert rule generation

Whenever the alert rule configuration is updated, the Relation Graph Engine
updates the appropriate alert group for the template in the Prometheus Ruler. To
do so, each alert rule is evaluated to a list of Prometheus alert rules and the
results are concatenated together, forming the alert rule group to be uploaded
to the Prometheus ruler.

For every source of the Prometheus item specified in the alert template,
Prometheus expressions are generated, which are then either included in separate
alert rules, or combined into one, as specified in the template's `instance`
setting. For every source expression or combination of source expressions, one
expression is generated for each severity that has the template-defined
thresholds set.

This means that the number of alert rules generated for each alert rule
configuration will be the product of the number of sources (or one in case of
`instance` is set to `worst` or `best`) and the number of severities for which
the threshold(s) are set.

In addition to the template-specified extra labels, a label `severity` is
automatically added to the generated alert rules, with the appropriate value.
The generated alert name is a concatenation of the alert rule template name, the
alert rule name and the severity, separated by hyphens.

## Alert series

The Prometheus Ruler manages a metric `ALERTS` which is set to a value of `1`
for all series resulting from an alert rule expression. In addition to the
labels resulting from the alert rule expression and the extra labels specified
in the alert rule defintiion, the series include the following labels:

- `alertname`: the alert name specified in the Prometheus alert rule. For alerts
 defined through the Relation Graph Engine, this is a concatenation of
 `alertrule`, `alertconfig` and `severity`.

- `alertstate`: the current state of the alert, either `pending` or `firing`. If
 the state is `pending`, the alert expression started yielding the alert's set
 of labels, but not yet for the duration specified in the alert rule's `for`
 parameter. If the ruler finds the specific set of labels in the alert
 expressions result set for at least the `for` duration, the alert state
 transitions to `firing`. When the set of labels is no longer found in the
 result set (i.e. the metric no longer exceeds the threshold), the corresponding
 alert series is removed. There is no `alertstate` for untriggered alerts.

The following labels are added by the Relation Graph Engine:

- `alertrule`: the name of the alert rule template in the Relation Graph config.

- `alertconfig`: the name of the alert rule in the Relation Graph config.

- `severity`: the severity if this alert rule is triggered.

## Alert filtering

Every minute, the Relation Graph Engine queries Prometheus for firing alerts.
For every firing alert, it looks for the corresponding alert config, and it
ignores any alerts that are not generated from its config. Subsequently, it
looks up the discovery item corresponding to the alert using the relations
between labels and properties defined in the connection packages, again ignoring
any alerts for objects that are not present in the discovery database. Finally,
for every alert rule and discovered item, it keeps the alert with the highest
severity.

Filtering by severity in the alert expression, i.e. before alert rule
evaluation, has been considered, but this is not possible due to the interaction
with the alert rule's `for` setting.

For example, with a `for` setting of five minutes, an alert rule that has been
firing for the `warning` threshold, would need to stay above the `critical`
threshold for five minutes before a `firing` alert with `critical` severity is
created. If we modify the `warning` alert rule to only trigger when the metric
is above the `warning` threshold, but not above the `critical` threshold, the
alert with `warning` `severity` would stop firing as soon as the metric goes
above the `critical` threshold, while the `critical` alert would take five
minutes to start firing.

It is therefore necessary to create overlapping alert rules for the individual
severities, and filter the resulting alerts.

The resulting alert set is compared to the current alert state and any changes
in state are saved to the `alerts` index (versioned on a single timeline through
the DBDaemon). When alert starts firing, a new alert object is created in the
index. If the severity changes, the object is updated (the previous version is
closed and a new version is inserted) with the new severity. If the alert
disappears from Prometheus data (i.e. we would consider the alert status to be
`ok`), the object is removed (the previous version is closed) from the alert
index.


## Filtering by alert rule position (unimplemented)

Filtering by alert rule position, currently not implemented, would be more
involved than filtering by severity due to the lack of an `untriggered` alert
state. To implement this, it would be necessary to find the first rule in the
list that could match the alert, and discard the alert if the matched alert rule
comes before the actual alert rule that triggered the alert. If this where to be
done, it would probably be useful to allow creating multiple parallel alert rule
`queues` or `rulesets`.

## Status calculation

Status is calculated by aggregating alerts by the associated discovery object,
keeping only the worst severity. If no alerts are firing for the object, the
status is considered to be `ok`. As for the alerts themselved, status is saved
to a single-timeline versioned `status` index, only updating the object when the
status changes. This makes it efficient to lookup status changes in the
database, as well as to calculate status percentages over time for availability
reports.

Due to the way status calculation works, a newly-discovered object can only get
a status and alerts as soon as the status runner is triggered, which can take up
to one minute (if metrics where already available).
