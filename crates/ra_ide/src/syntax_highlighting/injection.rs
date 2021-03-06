//! Syntax highlighting injections such as highlighting of documentation tests.

use std::{collections::BTreeMap, convert::TryFrom};

use ast::{HasQuotes, HasStringValue};
use hir::Semantics;
use ra_syntax::{ast, AstToken, SyntaxNode, SyntaxToken, TextRange, TextSize};
use stdx::SepBy;

use crate::{call_info::ActiveParameter, Analysis, HighlightTag, HighlightedRange, RootDatabase};

use super::HighlightedRangeStack;

pub(super) fn highlight_injection(
    acc: &mut HighlightedRangeStack,
    sema: &Semantics<RootDatabase>,
    literal: ast::RawString,
    expanded: SyntaxToken,
) -> Option<()> {
    let active_parameter = ActiveParameter::at_token(&sema, expanded)?;
    if !active_parameter.name.starts_with("ra_fixture") {
        return None;
    }
    let value = literal.value()?;
    let (analysis, tmp_file_id) = Analysis::from_single_file(value);

    if let Some(range) = literal.open_quote_text_range() {
        acc.add(HighlightedRange {
            range,
            highlight: HighlightTag::StringLiteral.into(),
            binding_hash: None,
        })
    }

    for mut h in analysis.highlight(tmp_file_id).unwrap() {
        if let Some(r) = literal.map_range_up(h.range) {
            h.range = r;
            acc.add(h)
        }
    }

    if let Some(range) = literal.close_quote_text_range() {
        acc.add(HighlightedRange {
            range,
            highlight: HighlightTag::StringLiteral.into(),
            binding_hash: None,
        })
    }

    Some(())
}

/// Mapping from extracted documentation code to original code
type RangesMap = BTreeMap<TextSize, TextSize>;

/// Extracts Rust code from documentation comments as well as a mapping from
/// the extracted source code back to the original source ranges.
/// Lastly, a vector of new comment highlight ranges (spanning only the
/// comment prefix) is returned which is used in the syntax highlighting
/// injection to replace the previous (line-spanning) comment ranges.
pub(super) fn extract_doc_comments(
    node: &SyntaxNode,
) -> Option<(String, RangesMap, Vec<HighlightedRange>)> {
    // wrap the doctest into function body to get correct syntax highlighting
    let prefix = "fn doctest() {\n";
    let suffix = "}\n";
    // Mapping from extracted documentation code to original code
    let mut range_mapping: RangesMap = BTreeMap::new();
    let mut line_start = TextSize::try_from(prefix.len()).unwrap();
    let mut is_doctest = false;
    // Replace the original, line-spanning comment ranges by new, only comment-prefix
    // spanning comment ranges.
    let mut new_comments = Vec::new();
    let doctest = node
        .children_with_tokens()
        .filter_map(|el| el.into_token().and_then(ast::Comment::cast))
        .filter(|comment| comment.kind().doc.is_some())
        .filter(|comment| {
            if comment.text().contains("```") {
                is_doctest = !is_doctest;
                false
            } else {
                is_doctest
            }
        })
        .map(|comment| {
            let prefix_len = comment.prefix().len();
            let line: &str = comment.text().as_str();
            let range = comment.syntax().text_range();

            // whitespace after comment is ignored
            let pos = if let Some(ws) = line.chars().nth(prefix_len).filter(|c| c.is_whitespace()) {
                prefix_len + ws.len_utf8()
            } else {
                prefix_len
            };

            // lines marked with `#` should be ignored in output, we skip the `#` char
            let pos = if let Some(ws) = line.chars().nth(pos).filter(|&c| c == '#') {
                pos + ws.len_utf8()
            } else {
                pos
            };

            range_mapping.insert(line_start, range.start() + TextSize::try_from(pos).unwrap());
            new_comments.push(HighlightedRange {
                range: TextRange::new(
                    range.start(),
                    range.start() + TextSize::try_from(pos).unwrap(),
                ),
                highlight: HighlightTag::Comment.into(),
                binding_hash: None,
            });
            line_start += range.len() - TextSize::try_from(pos).unwrap();
            line_start += TextSize::try_from('\n'.len_utf8()).unwrap();

            line[pos..].to_owned()
        })
        .sep_by("\n")
        .to_string();

    if doctest.is_empty() {
        return None;
    }

    let doctest = format!("{}{}{}", prefix, doctest, suffix);
    Some((doctest, range_mapping, new_comments))
}

/// Injection of syntax highlighting of doctests.
pub(super) fn highlight_doc_comment(
    text: String,
    range_mapping: RangesMap,
    new_comments: Vec<HighlightedRange>,
    stack: &mut HighlightedRangeStack,
) {
    let (analysis, tmp_file_id) = Analysis::from_single_file(text);

    stack.push();
    for mut h in analysis.highlight(tmp_file_id).unwrap() {
        // Determine start offset and end offset in case of multi-line ranges
        let mut start_offset = None;
        let mut end_offset = None;
        for (line_start, orig_line_start) in range_mapping.range(..h.range.end()).rev() {
            if line_start <= &h.range.start() {
                start_offset.get_or_insert(orig_line_start - line_start);
                break;
            } else {
                end_offset.get_or_insert(orig_line_start - line_start);
            }
        }
        if let Some(start_offset) = start_offset {
            h.range = TextRange::new(
                h.range.start() + start_offset,
                h.range.end() + end_offset.unwrap_or(start_offset),
            );
            stack.add(h);
        }
    }

    // Inject the comment prefix highlight ranges
    stack.push();
    for comment in new_comments {
        stack.add(comment);
    }
    stack.pop_and_inject(false);
    stack.pop_and_inject(true);
}
