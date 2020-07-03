//! This module is responsible for resolving paths within rules.

use crate::errors::error;
use crate::{parsing, SsrError, SsrPattern, SsrRule};
use ra_syntax::{ast, SyntaxNode};
use rustc_hash::FxHashMap;
use test_utils::mark;

pub(crate) struct ResolvedRule {
    pub(crate) pattern: SsrPattern,
    pub(crate) template: parsing::SsrTemplate,
    // Paths in the search pattern that we've resolved.
    pub(crate) resolved_paths: FxHashMap<SyntaxNode, hir::PathResolution>,
}

impl ResolvedRule {
    pub(crate) fn new(
        rule: SsrRule,
        scope: &hir::SemanticsScope,
        hygiene: &hir::Hygiene,
    ) -> Result<ResolvedRule, SsrError> {
        let mut resolver = Resolver {
            scope,
            resolved_paths: FxHashMap::default(),
            hygiene,
            pattern: &rule.pattern,
        };
        resolver.resolve_rule(&rule)?;
        Ok(ResolvedRule {
            resolved_paths: resolver.resolved_paths,
            pattern: rule.pattern,
            template: rule.template,
        })
    }
}

struct Resolver<'a, 'db> {
    scope: &'a hir::SemanticsScope<'db>,
    resolved_paths: FxHashMap<SyntaxNode, hir::PathResolution>,
    hygiene: &'a hir::Hygiene,
    pattern: &'a SsrPattern,
}

impl Resolver<'_, '_> {
    fn resolve_rule(&mut self, rule: &SsrRule) -> Result<(), SsrError> {
        self.resolve_opt(&rule.pattern.expr)?;
        self.resolve_opt(&rule.pattern.type_ref)?;
        self.resolve_opt(&rule.pattern.item)?;
        self.resolve_opt(&rule.pattern.path)?;
        self.resolve_opt(&rule.pattern.pattern)?;
        Ok(())
    }

    fn resolve_opt(&mut self, node: &Option<SyntaxNode>) -> Result<(), SsrError> {
        if let Some(node) = node.as_ref() {
            return self.resolve(node.clone());
        }
        Ok(())
    }

    fn resolve(&mut self, node: SyntaxNode) -> Result<(), SsrError> {
        use ra_syntax::ast::AstNode;
        if let Some(path) = ast::Path::cast(node.clone()) {
            // Check if this is an appropriate place in the path to resolve. If the path is
            // something like `a::B::<i32>::c` then we want to resolve `a::B`. If the path contains
            // a placeholder. e.g. `a::$b::c` then we want to resolve `a`.
            if !path_contains_type_arguments(path.qualifier())
                && !self.path_contains_placeholder(&path)
            {
                let hir_path = hir::Path::from_src(path, &self.hygiene)
                    .ok_or_else(|| error!("Failed to create HIR path from `{}`", node.text()))?;
                let resolution = self
                    .scope
                    .resolve_hir_path(&hir_path)
                    .ok_or_else(|| error!("Failed to resolve path `{}`", node.text()))?;
                self.resolved_paths.insert(node, resolution);
                return Ok(());
            }
        }
        for node in node.children() {
            self.resolve(node)?;
        }
        Ok(())
    }

    /// Returns whether `path` contains a placeholder, but ignores any placeholders within type
    /// arguments.
    fn path_contains_placeholder(&self, path: &ast::Path) -> bool {
        if let Some(segment) = path.segment() {
            if let Some(name_ref) = segment.name_ref() {
                if self.pattern.placeholders_by_stand_in.contains_key(name_ref.text()) {
                    return true;
                }
            }
        }
        if let Some(qualifier) = path.qualifier() {
            return self.path_contains_placeholder(&qualifier);
        }
        false
    }
}

/// Returns whether `path` or any of its qualifiers contains type arguments.
fn path_contains_type_arguments(path: Option<ast::Path>) -> bool {
    if let Some(path) = path {
        if let Some(segment) = path.segment() {
            if segment.type_arg_list().is_some() {
                mark::hit!(type_arguments_within_path);
                return true;
            }
        }
        return path_contains_type_arguments(path.qualifier());
    }
    false
}
