/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::{BTreeMap, BTreeSet};

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::{
    AlertConfigName, AlertRule, AlertRuleConfigs, AlertRuleForms, AlertRuleTemplateName, RuleGroup,
    UnverifiedAlertRuleForms,
};
use crate::{Error, Result};

#[derive(Default)]
pub struct AlertRulesState(pub BTreeMap<AlertRuleTemplateName, AlertRuleState>);

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct AlertRuleState {
    pub version: DateTime<Utc>,
    pub rule_form: Provisionable<AlertRuleForms>,
}

impl AlertRulesState {
    pub fn new(rule_state: BTreeMap<AlertRuleTemplateName, AlertRuleState>) -> Self {
        Self(rule_state)
    }

    pub fn get_rule_form_names(&self) -> impl Iterator<Item = &AlertRuleTemplateName> {
        self.0.keys()
    }

    pub fn get_rule_form(&self, name: &AlertRuleTemplateName) -> Option<&AlertRuleState> {
        self.0.get(name)
    }

    pub fn verify_put_rule_form(
        &self,
        name: AlertRuleTemplateName,
        rule: PutAlertRuleForm,
        schema: &prometheus_schema::Universe,
    ) -> Result<PutAlertRuleFormResult> {
        let rule_form = rule.rule_form.verify(&name, true)?;
        match self.0.get(&name) {
            None if rule.version.is_none() => {
                let rules = rule_form.rules(schema)?;
                let rule_group = if rules.is_empty() {
                    None
                } else {
                    Some(RuleGroup {
                        name: name.to_string(),
                        rules,
                    })
                };
                Ok(PutAlertRuleFormResult {
                    rule_state: AlertRuleState::new(rule_form, false),
                    template_updated: true,
                    added_configs: BTreeMap::new(),
                    updated_configs: BTreeMap::new(),
                    removed_configs: Vec::new(),
                    rule_group,
                })
            }
            Some(rule_state) if rule.version == Some(rule_state.version) => {
                if rule_state.rule_form.provisioned
                    && rule_state.rule_form.value.get_template() != rule_form.get_template()
                {
                    return Err(Error::ProvisionedAlertRule(name));
                }
                let template_updated = if rule_state.rule_form.provisioned {
                    false
                } else {
                    rule_form.get_template() != rule_state.rule_form.value.get_template()
                };
                let configs_current = rule_state.rule_form.value.get_configs();
                let configs_new = rule_form.get_configs();
                let (added_configs, updated_configs, removed_configs) = {
                    let keys_current = configs_current.keys().collect::<BTreeSet<_>>();
                    let keys_new = configs_new.keys().collect::<BTreeSet<_>>();
                    let removed_configs = keys_current
                        .difference(&keys_new)
                        .cloned()
                        .cloned()
                        .collect::<Vec<_>>();
                    let keys_added = keys_new
                        .difference(&keys_current)
                        .cloned()
                        .cloned()
                        .collect::<Vec<_>>();
                    let added_configs = keys_added
                        .into_iter()
                        .filter_map(|name| {
                            let config_new = configs_new.get(&name)?;
                            Some((name, config_new.clone()))
                        })
                        .collect();
                    let updated_configs = keys_current
                        .intersection(&keys_new)
                        .copied()
                        .filter_map(|name| {
                            let config_current = configs_current.get(name)?;
                            let config_new = configs_new.get(name)?;
                            (config_current != config_new)
                                .then(|| (name.clone(), config_new.clone()))
                        })
                        .collect();
                    (added_configs, updated_configs, removed_configs)
                };
                let rules = rule_form.rules(schema)?;
                let rule_group = if rules.is_empty() {
                    None
                } else {
                    Some(RuleGroup {
                        name: name.to_string(),
                        rules,
                    })
                };
                Ok(PutAlertRuleFormResult {
                    rule_state: AlertRuleState::new(rule_form, rule_state.rule_form.provisioned),
                    template_updated,
                    added_configs,
                    updated_configs,
                    removed_configs,
                    rule_group,
                })
            }
            _ => Err(Error::AlertRuleFormVersionConflict(name)),
        }
    }

    pub fn get_rule_form_configs_names(
        &self,
        name: &AlertRuleTemplateName,
    ) -> Result<impl Iterator<Item = &AlertConfigName> + '_> {
        let rule_state = self
            .0
            .get(name)
            .ok_or_else(|| Error::MissingAlertRule(name.clone()))?;
        Ok(rule_state.rule_form.value.iter_config_names())
    }

    pub fn rule_groups<'a>(
        &'a self,
        schema: &'a prometheus_schema::Universe,
    ) -> impl Iterator<Item = (AlertRuleTemplateName, Result<RuleGroup<AlertRule>>)> + 'a {
        self.0
            .iter()
            .filter_map(|(name, state)| match state.rule_form.value.rules(schema) {
                Ok(rules) if rules.is_empty() => None,
                rules => Some((
                    name.clone(),
                    rules.map(|rules| RuleGroup {
                        name: name.to_string(),
                        rules,
                    }),
                )),
            })
    }
}

impl AlertRuleState {
    pub fn new(rule_form: AlertRuleForms, provisioned: bool) -> Self {
        Self {
            version: Utc::now(),
            rule_form: Provisionable {
                provisioned,
                value: rule_form,
            },
        }
    }
}

#[derive(Deserialize, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct PutAlertRuleForm {
    pub version: Option<DateTime<Utc>>,
    pub rule_form: UnverifiedAlertRuleForms,
}

pub struct PutAlertRuleFormResult {
    pub rule_state: AlertRuleState,
    pub template_updated: bool,
    pub added_configs: BTreeMap<AlertConfigName, AlertRuleConfigs>,
    pub removed_configs: Vec<AlertConfigName>,
    pub updated_configs: BTreeMap<AlertConfigName, AlertRuleConfigs>,
    pub rule_group: Option<RuleGroup<AlertRule>>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "{provisioned: boolean} & T"))]
pub struct Provisionable<T> {
    pub provisioned: bool,
    #[serde(flatten)]
    pub value: T,
}
