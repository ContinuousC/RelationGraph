#!/bin/bash

for tenant in $@; do

    for alert in alerts/*.spec.yaml; do

	alert=${alert#alerts/}
	alert=${alert%.spec.yaml}

	name=$(sed -rne 's/^alert: //p' alerts/$alert.spec.yaml)

	
	curl -ki "https://$tenant.continuousc.contc/api/alert_rules/$name" -XPUT -HContent-Type:application/yaml --data-binary @alerts/$alert.spec.yaml; echo
	curl -ki "https://$tenant.continuousc.contc/api/alert_config/$name" -XPUT -HContent-Type:application/yaml --data-binary @alerts/$alert.config.yaml; echo
	
    done
    
done
