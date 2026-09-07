use serde::{Deserialize, Serialize};
use std::{collections::HashSet, fs, path::Path};

use crate::Result;

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum Action {
    Ignore,
    Uncertain,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct Rule {
    pub id: String,
    pub program: String,
    pub action: Action,
    pub exact: Option<String>,
    pub suffix: Option<String>,
    #[serde(default)]
    pub note: String,
}

impl Rule {
    pub fn matches(&self, program: &[u8], message: &[u8]) -> bool {
        self.program.as_bytes() == program
            && match (&self.exact, &self.suffix) {
                (Some(exact), _) => message == exact.as_bytes(),
                (_, Some(suffix)) => message.ends_with(suffix.as_bytes()),
                _ => false,
            }
    }

    pub fn same_matcher(&self, other: &Self) -> bool {
        self.program == other.program && self.exact == other.exact && self.suffix == other.suffix
    }
}

pub fn load(path: &Path) -> Result<Vec<Rule>> {
    parse(&fs::read_to_string(path).map_err(|error| format!("{}: {error}", path.display()))?)
}

fn parse(source: &str) -> Result<Vec<Rule>> {
    #[derive(Deserialize)]
    #[serde(deny_unknown_fields)]
    struct Configuration {
        rules: Vec<Rule>,
    }
    let configuration: Configuration = toml::from_str(source)?;
    let mut identifiers = HashSet::new();
    for rule in &configuration.rules {
        if rule.id.is_empty()
            || !rule
                .id
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || b"-_".contains(&byte))
            || !identifiers.insert(&rule.id)
        {
            return Err(format!("invalid or duplicate rule ID: {:?}", rule.id).into());
        }
        if rule.program.is_empty() || rule.program.contains(['\n', '\r', '\0']) {
            return Err(format!("{}: invalid program", rule.id).into());
        }
        if rule.exact.is_some() == rule.suffix.is_some()
            || rule
                .exact
                .as_ref()
                .or(rule.suffix.as_ref())
                .is_some_and(String::is_empty)
        {
            return Err(format!(
                "{}: specify exactly one nonempty exact or suffix matcher",
                rule.id
            )
            .into());
        }
    }
    Ok(configuration.rules)
}

pub fn classify<'a>(rules: &'a [Rule], program: &[u8], message: &[u8]) -> Option<&'a Action> {
    rules
        .iter()
        .find(|rule| rule.matches(program, message))
        .map(|rule| &rule.action)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn migrated_rules_preserve_classification() {
        let rules = parse(include_str!("../rules.toml")).unwrap();
        assert_eq!(rules.len(), 10);
        for rule in &rules {
            let message = rule.exact.as_ref().or(rule.suffix.as_ref()).unwrap();
            assert_eq!(
                classify(&rules, rule.program.as_bytes(), message.as_bytes()),
                Some(&rule.action)
            );
            assert_eq!(classify(&rules, b"unrelated", message.as_bytes()), None);
            assert_eq!(
                classify(
                    &rules,
                    rule.program.as_bytes(),
                    format!("{message} changed").as_bytes()
                ),
                None
            );
        }
        assert_eq!(
            classify(
                &rules,
                b"chrome",
                b"[123:456] Unable to connect to ibus: Could not connect: Connection refused"
            ),
            Some(&Action::Uncertain)
        );
    }

    #[test]
    fn invalid_rules_fail_closed() {
        let source = "[[rules]]\nid='one'\nprogram='kernel'\naction='ignore'\nexact='message'\n";
        assert!(parse(&format!("{source}{source}")).is_err());
        assert!(parse(&format!("{source}suffix='message'\n")).is_err());
        assert!(parse(&source.replace("exact=", "excat=")).is_err());
        assert!(parse(&source.replace("'message'", "''")).is_err());
    }
}
