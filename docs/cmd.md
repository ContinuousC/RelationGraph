# Command-Line Tool #

The relation graph command line tool provides various helpful commands
that can be used for testing and debugging relatio graph operation
from the command line.

Usage:

```
Run relation graph operations from the command line

Usage: relation-graph-cmd <COMMAND>

Commands:
  query          Run a query on a relation graph
  search-domain  Run a query on a relation graph
  es-query       Generate an elasticsearch query for a relation graph query
  prom-query     Generate prometheus queries for an item in the relation graph
  alert-rule     Generate prometheus alert rules
  status-rule    Generate prometheus alert rules
  status         Query prometheus for an item's status
  help           Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```

## Running a query ##

```
Run a query on a relation graph

Usage: relation-graph-cmd query [OPTIONS] <PKGS> <ITEMS> <QUERY>

Arguments:
  <PKGS>
  <ITEMS>
  <QUERY>

Options:
      --bench
  -p, --pkg <PKG>
  -h, --help       Print help
```

## Querying a search domain ##

```
Run a query on a relation graph

Usage: relation-graph-cmd search-domain [OPTIONS] <PKGS> <ITEMS> <QUERY>

Arguments:
  <PKGS>
  <ITEMS>
  <QUERY>

Options:
      --bench
  -p, --pkg <PKG>
  -h, --help       Print help
```

## Generating an elasticsearch query ##

```
Generate an elasticsearch query for a relation graph query

Usage: relation-graph-cmd es-query [OPTIONS] <PKGS> <QUERY>

Arguments:
  <PKGS>
  <QUERY>

Options:
  -p, --pkg <PKG>
  -h, --help       Print help
```

## Generating prometheus queries ##

```
Generate prometheus queries for an item in the relation graph

Usage: relation-graph-cmd prom-query [OPTIONS] <PKGS> <ROOT> <ITEMS> <ITEM>

Arguments:
  <PKGS>   Path to the package definition folder
  <ROOT>   Path to the prometheus schema root
  <ITEMS>  Path to the relation graph in json format
  <ITEM>   The id of the item for which to generate queries

Options:
  -p, --pkg <PKG>
  -h, --help       Print help
```

## Generating prometheus alert rules ##

```
Generate prometheus alert rules

Usage: relation-graph-cmd alert-rule <ROOT> <RULE> <CONFIG>

Arguments:
  <ROOT>    The prometheus schema root
  <RULE>    The alert rule definition
  <CONFIG>  The alert rule value

Options:
  -h, --help  Print help
```

## Querying prometheus for an item's status ##

```
Query prometheus for an item's status

Usage: relation-graph-cmd status [OPTIONS] <PKGS> <ROOT> <ITEMS> <ITEM> [RULES]...

Arguments:
  <PKGS>      Path to the package definition folder
  <ROOT>      The prometheus schema root
  <ITEMS>     Path to the relation graph in json format
  <ITEM>      The id of the item for which to query status
  [RULES]...  Alert rule definitions

Options:
  -p, --pkg <PKG>
  -h, --help       Print help
```
