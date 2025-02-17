/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;

use graph::RefBy;
use wrapper::{Wrapped, Wrapper};

use crate::{
    error::Result,
    items::{
        resolved::{ItemRef, NamedItemRef},
        serial::PropertyValue,
    },
    query::{
        filter::{Filter, MatchResult},
        resolved::{ItemArgs, MatchContext, TemplateDef, TplVarValueRef},
    },
    Absolute, ItemId, ItemSelector, Items, PackageId, PropertyId, Relation, RelationType,
    RelationTypeId, TplVarId, Types,
};

use super::serial::{self, ConnectorRuleGroupName};

pub struct Connector {
    rule_groups: BTreeMap<ConnectorRuleGroupName, ConnectorRuleGroup>,
}

struct ConnectorRuleGroup {
    rules: Vec<ConnectorRule>,
}

struct ConnectorRule {
    template: TemplateDef,
    source: ItemSelector,
    target: ItemSelector,
    //properties: PropertyTemplate,
}

type RelationMap<'a, W> = BTreeMap<
    (&'a ItemId, &'a ItemId),
    (
        NamedItemRef<W>,
        NamedItemRef<W>,
        BTreeMap<Absolute<PropertyId>, PropertyValue>,
    ),
>;

impl Connector {
    pub(crate) fn to_serial(&self, pkg: Option<&PackageId>) -> serial::Connector {
        serial::Connector {
            groups: self
                .rule_groups
                .iter()
                .map(|(name, group)| (name.clone(), group.to_serial(pkg)))
                .collect(),
        }
    }

    pub(crate) fn from_serial(
        serial: serial::Connector,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self {
            //relation_type: RefBy::dangling(serial.relation_type.resolve_opt(pkg)),
            rule_groups: serial
                .groups
                .into_iter()
                .map(|(name, group)| {
                    Ok((name, ConnectorRuleGroup::from_serial(group, types, pkg)?))
                })
                .collect::<Result<_>>()?,
        })
    }

    pub(crate) fn resolve(&mut self, types: &Types) -> Result<()> {
        self.rule_groups
            .iter_mut()
            .try_for_each(|(_, group)| group.resolve(types))
    }

    pub fn run<W: Wrapper>(
        &self,
        items: &Items<W>,
        types: &Types,
        relation_type: &RefBy<Absolute<RelationTypeId>, RelationType>,
    ) -> Vec<Relation<W>> {
        let mut relations = BTreeMap::new();

        for group in self.rule_groups.values() {
            for (source_id, source_item_ref) in items.items.iter_ref() {
                group.run(source_id, source_item_ref, items, types, &mut relations);
            }
        }

        relations
            .into_values()
            .map(|(source, target, properties)| Relation {
                relation_type: relation_type.clone(),
                properties,
                source,
                target,
            })
            .collect()
    }
}

impl ConnectorRuleGroup {
    pub(crate) fn to_serial(&self, pkg: Option<&PackageId>) -> serial::ConnectorRuleGroup {
        serial::ConnectorRuleGroup {
            rules: self.rules.iter().map(|rule| rule.to_serial(pkg)).collect(),
        }
    }

    fn from_serial(
        serial: serial::ConnectorRuleGroup,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        Ok(Self {
            rules: serial
                .rules
                .into_iter()
                .map(|rule| ConnectorRule::from_serial(rule, types, pkg))
                .collect::<Result<_>>()?,
        })
    }

    pub(crate) fn resolve(&mut self, types: &Types) -> Result<()> {
        self.rules
            .iter_mut()
            .try_for_each(|rule| rule.resolve(types))
    }

    fn run<'a, W: Wrapper>(
        &'a self,
        source_id: &'a ItemId,
        source_item_ref: &ItemRef<W>,
        items: &'a Items<W>,
        types: &'a Types,
        relations: &mut RelationMap<'a, W>,
    ) {
        if let Some((ctx, rule, source_vars)) = self.rules.iter().find_map(|rule| {
            let ctx = MatchContext::new(items, types);
            let source_item = ctx.items.borrow_item(source_item_ref).get_wrapped();
            let Matches {
                matches,
                values: source_vars,
            } = rule
                .source
                .matches(&ItemArgs::new(source_id, source_item), &ctx, &rule.template);
            matches.matches().then_some((ctx, rule, source_vars))
        }) {
            let filter = Pattern(source_vars);
            for (target_id, target_item_ref) in
                ctx.items
                    .items
                    .iter_ref()
                    .filter_map(|(target_id, target_item_ref)| {
                        let target_item = ctx.items.borrow_item(target_item_ref).get_wrapped();

                        let m: Option<bool> = rule.target.matches(
                            &ItemArgs::new(target_id, target_item.get_wrapped()),
                            &ctx,
                            &filter,
                        );
                        m.matches().then_some((target_id, target_item_ref))
                    })
            {
                // let properties = rule.properties.build(source_vars | target_vars);
                relations.insert(
                    (source_id, target_id),
                    (
                        RefBy::new(source_id.clone(), source_item_ref.clone()),
                        RefBy::new(target_id.clone(), target_item_ref.clone()),
                        BTreeMap::new(),
                    ),
                );
            }
        }
    }
}

impl ConnectorRule {
    pub(crate) fn to_serial(&self, pkg: Option<&PackageId>) -> serial::ConnectorRule {
        serial::ConnectorRule {
            template: self
                .template
                .iter()
                .map(|(id, tpl)| (id.clone(), *tpl))
                .collect(),
            source: self.source.to_serial(pkg),
            target: self.target.to_serial(pkg),
        }
    }

    fn from_serial(
        serial: serial::ConnectorRule,
        types: &Types,
        pkg: Option<&PackageId>,
    ) -> Result<Self> {
        let source = ItemSelector::from_serial(serial.source, types, &serial.template, pkg)?;
        let target = ItemSelector::from_serial(serial.target, types, &serial.template, pkg)?;
        let template = serial.template.into_iter().collect();
        Ok(Self {
            template,
            source,
            target,
        })
    }

    pub(crate) fn resolve(&mut self, types: &Types) -> Result<()> {
        self.source.resolve(types)?;
        self.target.resolve(types)?;
        Ok(())
    }
}

#[derive(Debug)]
struct Pattern<'a>(BTreeMap<&'a TplVarId, TplVarValueRef<'a>>);

#[derive(PartialEq, Eq, Default, Debug)]
struct Matches<'a> {
    matches: Option<bool>,
    values: BTreeMap<&'a TplVarId, TplVarValueRef<'a>>,
}

impl<'a> Filter<'a, Matches<'a>> for TemplateDef {
    fn matches(&self, var: &'a TplVarId, value: Option<TplVarValueRef<'a>>) -> Matches<'a> {
        if self.get(var).is_some() {
            if let Some(value) = value {
                Matches {
                    matches: Some(true),
                    values: BTreeMap::from_iter([(var, value)]),
                }
            } else {
                Matches {
                    matches: Some(false),
                    values: BTreeMap::new(),
                }
            }
        } else {
            Matches::default()
        }
    }
}

impl From<Option<bool>> for Matches<'_> {
    fn from(value: Option<bool>) -> Self {
        Self {
            matches: value,
            values: BTreeMap::new(),
        }
    }
}

impl MatchResult for Matches<'_> {
    fn neutral_and() -> Self {
        Matches::default()
    }

    fn neutral_or() -> Self {
        Matches::default()
    }

    fn perform_and(mut self, other: Self) -> Self {
        Matches {
            matches: self.matches.perform_and(other.matches),
            values: {
                self.values.extend(other.values);
                self.values
            },
        }
    }

    fn perform_or(mut self, other: Self) -> Self {
        Matches {
            matches: self.matches.perform_or(other.matches),
            values: {
                self.values.extend(other.values);
                self.values
            },
        }
    }

    fn not(self) -> Self {
        Self {
            matches: self.matches.not(),
            values: self.values, // ???
        }
    }

    fn matches(&self) -> bool {
        self.matches.matches()
    }

    fn filtered(&self) -> bool {
        self.matches.filtered()
    }

    fn wrapper_filtered(&self) -> bool {
        self.matches.wrapper_filtered()
    }
}

impl Filter<'_, Option<bool>> for Pattern<'_> {
    fn matches(&self, var: &TplVarId, value: Option<TplVarValueRef>) -> Option<bool> {
        Some(self.0.get(var).copied() == value)
    }
}

#[cfg(test)]
mod test {
    use std::{path::Path, str::FromStr};

    use wrapper::Identity;

    use crate::{
        query::resolved::{ItemArgs, MatchContext},
        serial,
        types::connector::Pattern,
        ItemId, Items, PackageId, Packages, Types,
    };

    use super::Matches;

    fn load_jaeger() -> (PackageId, Types, Items<Identity>) {
        let jaeger: PackageId = PackageId::from_str("jaeger").unwrap();
        let types = Packages::load_sync(Path::new("../tests/jaeger/pkgs"))
            .unwrap()
            .types()
            .unwrap();
        let items = serial::Items::load(Path::new("../tests/jaeger/items.json"))
            .unwrap()
            .resolve(&types, Some(&jaeger))
            .unwrap();
        (jaeger, types, items)
    }

    #[test]
    fn match_template_pattern() {
        let (_jaeger, types, items) = load_jaeger();
        let connector = types
            .relations
            .get(&"jaeger/runs_on_container".parse().unwrap())
            .unwrap()
            .connector
            .as_ref()
            .unwrap()
            .rule_groups
            .get(&"container".parse().unwrap())
            .unwrap()
            .rules
            .first()
            .unwrap();

        let ctx = MatchContext::new(&items, &types);

        let matching_source_item_id: ItemId =
            "50b9cb8b-ce6c-4678-aaf5-7ea149b47e31".parse().unwrap();
        let matching_target_item_id: ItemId =
            "76eb82ab-d57f-4b94-9e2e-9b4670684651".parse().unwrap();
        let non_matching_item_ids: Vec<ItemId> = [
            "2a738955-d3c5-4c21-8429-966d692c3978",
            "0e735294-4202-4267-88f7-a2c1671c8352",
            "24d25ba0-9c1d-42b2-8028-53a200446249",
        ]
        .into_iter()
        .map(|id| id.parse().unwrap())
        .collect();

        let matching_source_item = items.items.get(&matching_source_item_id).unwrap();
        let matching_target_item = items.items.get(&matching_target_item_id).unwrap();
        let non_matching_items = non_matching_item_ids
            .into_iter()
            .map(|id| {
                let item = items.items.get(&id).unwrap();
                (id, item)
            })
            .collect::<Vec<_>>();

        let Matches {
            matches,
            values: source_vars,
        } = connector.source.matches(
            &ItemArgs::new(&matching_source_item_id, matching_source_item),
            &ctx,
            &connector.template,
        );

        assert_eq!(matches, Some(true));
        assert_eq!(source_vars.len(), 2);
        assert_eq!(
            source_vars
                .get(&&"containerName".parse().unwrap())
                .and_then(|v| v.get_string()),
            Some("relation-graph-engine")
        );
        assert_eq!(
            source_vars
                .get(&&"podUid".parse().unwrap())
                .and_then(|v| v.get_string()),
            Some("798c4483-5265-4b51-a2a7-0e397ba4d93d")
        );

        let pattern = Pattern(source_vars);
        let m = connector.target.matches(
            &ItemArgs::new(&matching_target_item_id, matching_target_item),
            &ctx,
            &pattern,
        );
        assert_eq!(m, Some(true));

        for (non_matching_item_id, non_matching_item) in non_matching_items {
            let Matches {
                matches,
                values: source_vars,
            } = connector.source.matches(
                &ItemArgs::new(&non_matching_item_id, non_matching_item),
                &ctx,
                &connector.template,
            );

            eprintln!("Source vars: {source_vars:?}");
            assert_eq!(matches, Some(false));
            assert_eq!(source_vars.len(), 0);

            let m = connector.target.matches(
                &ItemArgs::new(&non_matching_item_id, non_matching_item),
                &ctx,
                &pattern,
            );

            assert_eq!(m, Some(false));
        }
    }
}
