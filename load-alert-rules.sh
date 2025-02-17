#!/bin/bash

cargo run --bin relation-graph-cmd -- alert-rule \
      prometheus/prometheus.root.yaml \
      tests/kubernetes/alerts/cpu-usage.spec.yaml \
      tests/kubernetes/alerts/cpu-usage.config.yaml |
    curl -ki https://cortex.contc/api/v1/rules/continuousc.yaml \
	 -XPOST -HContent-Type:application/yaml --data-binary @-
echo

# cargo run --bin relation-graph-cmd -- status-rule \
#       tests/kubernetes/pkgs \
#       prometheus/prometheus.root.yaml \
#       tests/kubernetes/alerts/*.spec.yaml |
#     curl -ki https://cortex.contc/api/v1/rules/continuousc-status.yaml \
# 	 -XPOST -HContent-Type:application/yaml --data-binary @-
# echo
