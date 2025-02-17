#!/bin/bash

cargo  build --bin relation-graph-cmd 2> /dev/null \
    && ../../target/debug/relation-graph-cmd query --pkg kubernetes pkgs items.json query_namespaces.json \
	| python -c $'
import json,sys
data = json.load(sys.stdin)
json.dump({
   "items": { item_id: item["item_type"] + "/" + item["properties"]["name"] for item_id,item in data["items"].iteritems() },
   "template": { var: ", ".join(vals[0:5]) + ("..." * (len(vals) > 5)) for var,vals in data["template"].items()}
}, sys.stdout, indent = 3)' && echo
