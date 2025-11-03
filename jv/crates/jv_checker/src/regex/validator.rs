use super::{PatternConstAnalyzer, PatternConstKind};
use crate::CheckError;
use crate::diagnostics::{self, DiagnosticSeverity, EnhancedDiagnostic, messages};
use jv_ast::{
    Argument, ConcurrencyConstruct, Expression, ForInStatement, LoopStrategy, NumericRangeLoop,
    PatternConstKey, PatternOrigin, Program, RegexLambdaReplacement, RegexLiteral,
    RegexReplacement, ResourceManagement, Span, Statement, StringPart, TryCatchClause,
};
use std::time::Instant;

#[derive(Debug, Clone)]
pub struct RegexAnalysis {
    pub pattern: String,
    pub raw: String,
    pub span: Span,
    pub diagnostics: Vec<EnhancedDiagnostic>,
    pub validation_duration_ms: f64,
    pub origin: PatternOrigin,
    pub const_kind: PatternConstKind,
    pub const_key: Option<PatternConstKey>,
}

#[derive(Debug, Default)]
pub struct RegexValidator {
    analyses: Vec<RegexAnalysis>,
}

impl RegexValidator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn validate_program(&mut self, program: &Program) -> Vec<CheckError> {
        self.analyses.clear();
        let mut errors = Vec::new();
        let mut visitor = RegexValidationVisitor {
            validator: self,
            errors: &mut errors,
        };
        visitor.visit_program(program);
        errors
    }

    pub fn analyses(&self) -> &[RegexAnalysis] {
        &self.analyses
    }

    pub fn take_analyses(&mut self) -> Vec<RegexAnalysis> {
        std::mem::take(&mut self.analyses)
    }

    fn analyze_literal(&mut self, literal: &RegexLiteral, errors: &mut Vec<CheckError>) {
        let start = Instant::now();
        let mut diagnostics = Vec::new();
        let origin = literal
            .origin
            .clone()
            .unwrap_or_else(|| PatternOrigin::literal(literal.span.clone()));
        let const_kind = PatternConstAnalyzer::classify(literal, &origin);
        let const_key = literal.const_key.clone();

        if let Some(diagnostic) = detect_missing_delimiter(literal) {
            let mut diagnostic = adapt_const_diagnostic(literal, diagnostic, const_kind);
            self.record_issue(literal, &mut diagnostics, errors, &mut diagnostic);
        }
        if let Some(diagnostic) = detect_unbalanced_groups(literal) {
            let mut diagnostic = adapt_const_diagnostic(literal, diagnostic, const_kind);
            self.record_issue(literal, &mut diagnostics, errors, &mut diagnostic);
        }
        if let Some(diagnostic) = detect_unsupported_escape(literal) {
            let mut diagnostic = adapt_const_diagnostic(literal, diagnostic, const_kind);
            self.record_issue(literal, &mut diagnostics, errors, &mut diagnostic);
        }

        let duration_ms = start.elapsed().as_secs_f64() * 1_000.0;
        if let Some(mut diagnostic) = detect_complexity_warning(duration_ms) {
            self.record_issue(literal, &mut diagnostics, errors, &mut diagnostic);
        }

        self.analyses.push(RegexAnalysis {
            pattern: literal.pattern.clone(),
            raw: literal.raw.clone(),
            span: literal.span.clone(),
            diagnostics,
            validation_duration_ms: duration_ms,
            origin,
            const_kind,
            const_key,
        });
    }

    fn record_issue(
        &mut self,
        literal: &RegexLiteral,
        collected: &mut Vec<EnhancedDiagnostic>,
        errors: &mut Vec<CheckError>,
        diagnostic: &mut EnhancedDiagnostic,
    ) {
        diagnostic.span = Some(literal.span.clone());
        if diagnostic.severity == DiagnosticSeverity::Error {
            errors.push(CheckError::ValidationError {
                message: format_enhanced_message(diagnostic),
                span: Some(literal.span.clone()),
            });
        }
        collected.push(diagnostic.clone());
    }
}

struct RegexValidationVisitor<'a> {
    validator: &'a mut RegexValidator,
    errors: &'a mut Vec<CheckError>,
}

impl<'a> RegexValidationVisitor<'a> {
    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::ValDeclaration { initializer, .. } => {
                self.visit_expression(initializer);
            }
            Statement::VarDeclaration { initializer, .. } => {
                if let Some(expr) = initializer {
                    self.visit_expression(expr);
                }
            }
            Statement::FunctionDeclaration {
                parameters, body, ..
            } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default);
                    }
                }
                self.visit_expression(body);
            }
            Statement::ClassDeclaration {
                properties,
                methods,
                ..
            }
            | Statement::InterfaceDeclaration {
                properties,
                methods,
                ..
            } => {
                for property in properties {
                    if let Some(initializer) = &property.initializer {
                        self.visit_expression(initializer);
                    }
                    if let Some(getter) = &property.getter {
                        self.visit_expression(getter);
                    }
                    if let Some(setter) = &property.setter {
                        self.visit_expression(setter);
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::DataClassDeclaration { parameters, .. } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default);
                    }
                }
            }
            Statement::ExtensionFunction(extension) => {
                self.visit_statement(&extension.function);
            }
            Statement::Expression { expr, .. } => {
                self.visit_expression(expr);
            }
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.visit_expression(expr);
                }
            }
            Statement::Throw { expr, .. } => {
                self.visit_expression(expr);
            }
            Statement::Assignment { target, value, .. } => {
                self.visit_expression(target);
                self.visit_expression(value);
            }
            Statement::ForIn(for_in) => self.visit_for_in(for_in),
            Statement::Concurrency(construct) => self.visit_concurrency(construct),
            Statement::ResourceManagement(resource) => self.visit_resource_management(resource),
            Statement::Comment(_)
            | Statement::Break(_)
            | Statement::Continue(_)
            | Statement::Import { .. }
            | Statement::Package { .. } => {}
        }
    }

    fn visit_for_in(&mut self, for_in: &ForInStatement) {
        self.visit_expression(&for_in.iterable);
        match &for_in.strategy {
            LoopStrategy::NumericRange(NumericRangeLoop { start, end, .. }) => {
                self.visit_expression(start);
                self.visit_expression(end);
            }
            LoopStrategy::LazySequence { .. } | LoopStrategy::Iterable | LoopStrategy::Unknown => {}
        }
        self.visit_expression(&for_in.body);
    }

    fn visit_concurrency(&mut self, construct: &ConcurrencyConstruct) {
        match construct {
            ConcurrencyConstruct::Spawn { body, .. } | ConcurrencyConstruct::Async { body, .. } => {
                self.visit_expression(body)
            }
            ConcurrencyConstruct::Await { expr, .. } => self.visit_expression(expr),
        }
    }

    fn visit_resource_management(&mut self, resource: &ResourceManagement) {
        match resource {
            ResourceManagement::Use { resource, body, .. } => {
                self.visit_expression(resource);
                self.visit_expression(body);
            }
            ResourceManagement::Defer { body, .. } => self.visit_expression(body),
        }
    }

    fn visit_catch_clause(&mut self, clause: &TryCatchClause) {
        if let Some(parameter) = &clause.parameter {
            if let Some(default) = &parameter.default_value {
                self.visit_expression(default);
            }
        }
        self.visit_expression(clause.body.as_ref());
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Literal(_, _) => {}
            Expression::RegexLiteral(literal) => {
                self.validator.analyze_literal(literal, self.errors);
            }
            Expression::Identifier(..)
            | Expression::This(..)
            | Expression::Super(..)
            | Expression::MultilineString(_)
            | Expression::JsonLiteral(_) => {}
            Expression::Binary { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            }
            Expression::Unary { operand, .. } => self.visit_expression(operand),
            Expression::Call { function, args, .. } => {
                self.visit_expression(function);
                for arg in args {
                    match arg {
                        Argument::Positional(expr) => self.visit_expression(expr),
                        Argument::Named { value, .. } => self.visit_expression(value),
                    }
                }
            }
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. } => self.visit_expression(object),
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                self.visit_expression(object);
                self.visit_expression(index);
            }
            Expression::TypeCast { expr, .. } => self.visit_expression(expr),
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let StringPart::Expression(expr) = part {
                        self.visit_expression(expr);
                    }
                }
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.visit_expression(element);
                }
            }
            Expression::RegexCommand(command) => {
                self.visit_expression(&command.subject);
                if let Some(replacement) = &command.replacement {
                    self.visit_regex_replacement(replacement);
                }
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default);
                    }
                }
                self.visit_expression(body);
            }
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                if let Some(subject) = expr {
                    self.visit_expression(subject);
                }
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.visit_expression(guard);
                    }
                    self.visit_expression(&arm.body);
                }
                if let Some(else_branch) = else_arm {
                    self.visit_expression(else_branch);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_expression(condition);
                self.visit_expression(then_branch);
                if let Some(branch) = else_branch {
                    self.visit_expression(branch);
                }
            }
            Expression::Block { statements, .. } => {
                for statement in statements {
                    self.visit_statement(statement);
                }
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_expression(body);
                for clause in catch_clauses {
                    self.visit_catch_clause(clause);
                }
                if let Some(finally) = finally_block {
                    self.visit_expression(finally);
                }
            }
        }
    }

    fn visit_regex_replacement(&mut self, replacement: &RegexReplacement) {
        match replacement {
            RegexReplacement::Literal(_) => {}
            RegexReplacement::Expression(expr) => self.visit_expression(expr),
            RegexReplacement::Lambda(lambda) => self.visit_regex_lambda(lambda),
        }
    }

    fn visit_regex_lambda(&mut self, lambda: &RegexLambdaReplacement) {
        for param in &lambda.params {
            if let Some(default) = &param.default_value {
                self.visit_expression(default);
            }
        }
        self.visit_expression(&lambda.body);
    }
}

fn adapt_const_diagnostic(
    literal: &RegexLiteral,
    diagnostic: EnhancedDiagnostic,
    const_kind: PatternConstKind,
) -> EnhancedDiagnostic {
    if diagnostic.severity != DiagnosticSeverity::Error {
        return diagnostic;
    }
    if !matches!(const_kind, PatternConstKind::Static) {
        return diagnostic;
    }
    let Some(descriptor) = diagnostics::descriptor("JV_REGEX_E220") else {
        return diagnostic;
    };

    let const_message = format!(
        "{}\n{}",
        messages::regex_const_validation_failure_message(diagnostic.code),
        diagnostic.message
    );

    let mut promoted =
        EnhancedDiagnostic::new(descriptor, const_message, Some(literal.span.clone()));
    if !diagnostic.suggestions.is_empty() {
        promoted.suggestions = diagnostic.suggestions.clone();
    }
    if !diagnostic.help.is_empty()
        && !promoted
            .suggestions
            .iter()
            .any(|entry| entry == diagnostic.help)
    {
        promoted.suggestions.push(diagnostic.help.to_string());
    }
    promoted.related_locations = diagnostic.related_locations.clone();
    if let Some(hint) = diagnostic.learning_hints.clone() {
        promoted.learning_hints = Some(hint);
    }
    promoted
}

fn detect_missing_delimiter(literal: &RegexLiteral) -> Option<EnhancedDiagnostic> {
    if literal.raw.starts_with('/') && literal.raw.ends_with('/') {
        return None;
    }
    let descriptor = diagnostics::descriptor("JV_REGEX_E201")?;
    let message = messages::regex_unterminated_literal_message();
    Some(
        EnhancedDiagnostic::new(descriptor, message, None)
            .with_suggestions(["Add a closing `/` to finish the literal."]),
    )
}

fn detect_unbalanced_groups(literal: &RegexLiteral) -> Option<EnhancedDiagnostic> {
    let descriptor = diagnostics::descriptor("JV_REGEX_E202")?;
    let mut stack: Vec<(char, usize)> = Vec::new();
    let mut escaped = false;
    for (index, ch) in literal.pattern.char_indices() {
        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        match ch {
            '(' => stack.push((')', index)),
            '[' => stack.push((']', index)),
            '{' => stack.push(('}', index)),
            ')' | ']' | '}' => {
                if let Some((expected, open_index)) = stack.pop() {
                    if ch != expected {
                        return Some(unbalanced_group_diagnostic(
                            descriptor,
                            expected,
                            Some((ch, open_index)),
                        ));
                    }
                } else {
                    return Some(stray_closer_diagnostic(descriptor, ch));
                }
            }
            _ => {}
        }
    }

    if let Some((expected, _)) = stack.pop() {
        return Some(unbalanced_group_diagnostic(descriptor, expected, None));
    }

    None
}

fn unbalanced_group_diagnostic(
    descriptor: &'static diagnostics::DiagnosticDescriptor,
    expected: char,
    mismatched: Option<(char, usize)>,
) -> EnhancedDiagnostic {
    let message = if let Some((found, _open_index)) = mismatched {
        messages::regex_group_balance_message(Some(expected), Some(found))
    } else {
        messages::regex_group_balance_message(Some(expected), None)
    };

    EnhancedDiagnostic::new(descriptor, message, None)
        .with_suggestions([format!("Add `{expected}` to balance the regex literal.")])
}

fn stray_closer_diagnostic(
    descriptor: &'static diagnostics::DiagnosticDescriptor,
    ch: char,
) -> EnhancedDiagnostic {
    let message = messages::regex_group_balance_message(None, Some(ch));
    EnhancedDiagnostic::new(descriptor, message, None).with_suggestions([format!(
        "Remove `{ch}` or introduce the corresponding opening bracket."
    )])
}

fn detect_unsupported_escape(literal: &RegexLiteral) -> Option<EnhancedDiagnostic> {
    let descriptor = diagnostics::descriptor("JV_REGEX_E203")?;
    let mut iter = literal.pattern.char_indices().peekable();
    while let Some((index, ch)) = iter.next() {
        if ch != '\\' {
            continue;
        }
        let Some((next_index, next_ch)) = iter.next() else {
            return Some(trailing_escape_diagnostic(descriptor));
        };

        if next_ch == 'u' {
            if !consume_hex_digits(
                &literal.pattern,
                &mut iter,
                4,
                next_index + next_ch.len_utf8(),
            ) {
                let sequence = &literal.pattern[index..next_index + next_ch.len_utf8()];
                return Some(invalid_escape_diagnostic(descriptor, sequence));
            }
            continue;
        }

        if next_ch == 'x' {
            if !consume_hex_digits(
                &literal.pattern,
                &mut iter,
                2,
                next_index + next_ch.len_utf8(),
            ) {
                let sequence = &literal.pattern[index..next_index + next_ch.len_utf8()];
                return Some(invalid_escape_diagnostic(descriptor, sequence));
            }
            continue;
        }

        if !is_allowed_simple_escape(next_ch) {
            let end = next_index + next_ch.len_utf8();
            let sequence = &literal.pattern[index..end];
            return Some(invalid_escape_diagnostic(descriptor, sequence));
        }
    }

    None
}

fn consume_hex_digits(
    pattern: &str,
    iter: &mut std::iter::Peekable<std::str::CharIndices<'_>>,
    required: usize,
    start: usize,
) -> bool {
    let mut digits = 0usize;
    let mut checkpoint = iter.clone();
    let mut end = start;
    while digits < required {
        let Some((index, ch)) = checkpoint.next() else {
            return false;
        };
        if !ch.is_ascii_hexdigit() {
            return false;
        }
        digits += 1;
        end = index + ch.len_utf8();
    }
    for _ in 0..digits {
        iter.next();
    }
    let _ = &pattern[start..end];
    true
}

fn is_allowed_simple_escape(ch: char) -> bool {
    matches!(
        ch,
        '\\' | '/'
            | 'n'
            | 't'
            | 'r'
            | 'd'
            | 'D'
            | 's'
            | 'S'
            | 'w'
            | 'W'
            | 'b'
            | 'B'
            | 'A'
            | 'G'
            | 'Q'
            | 'E'
            | '+'
            | '*'
            | '.'
            | '?'
            | '^'
            | '$'
            | '|'
            | '['
            | ']'
            | '('
            | ')'
            | '{'
            | '}'
            | '<'
            | '>'
            | 'k'
            | 'p'
            | 'P'
            | '0'
            | '1'
            | '2'
            | '3'
            | '4'
            | '5'
            | '6'
            | '7'
            | '8'
            | '9'
    )
}

fn trailing_escape_diagnostic(
    descriptor: &'static diagnostics::DiagnosticDescriptor,
) -> EnhancedDiagnostic {
    let message = messages::regex_trailing_escape_message();
    EnhancedDiagnostic::new(descriptor, message, None)
        .with_suggestions(["Remove the trailing backslash or escape a character after it."])
}

fn invalid_escape_diagnostic(
    descriptor: &'static diagnostics::DiagnosticDescriptor,
    sequence: &str,
) -> EnhancedDiagnostic {
    let message = messages::regex_invalid_escape_sequence_message(sequence);
    EnhancedDiagnostic::new(descriptor, message, None).with_suggestions([format!(
        "Replace `{sequence}` with a supported escape such as `\\n`, `\\t`, or remove the backslash."
    )])
}

fn detect_complexity_warning(duration_ms: f64) -> Option<EnhancedDiagnostic> {
    if duration_ms <= 10.0 {
        return None;
    }
    let descriptor = diagnostics::descriptor("JV_REGEX_I401")?;
    let message = format!(
        "正規表現の検証に時間がかかっています ({duration_ms:.2}ms)。\nRegex validation exceeded the interactive budget ({duration_ms:.2}ms).\n参考資料: https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/regex/Pattern.html\nReference: https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/regex/Pattern.html"
    );
    Some(
        EnhancedDiagnostic::new(descriptor, message, None).with_suggestions([
            "Simplify the pattern or split it into smaller expressions.".to_string(),
        ]),
    )
}

fn format_enhanced_message(diagnostic: &EnhancedDiagnostic) -> String {
    let mut lines = vec![format!("{}: {}", diagnostic.code, diagnostic.message)];
    if !diagnostic.help.is_empty() {
        lines.push(diagnostic.help.to_string());
    }
    for suggestion in &diagnostic.suggestions {
        if suggestion.starts_with("Quick Fix:") {
            lines.push(suggestion.clone());
        } else {
            lines.push(format!("Quick Fix: {}", suggestion));
        }
    }
    if let Some(hint) = &diagnostic.learning_hints {
        lines.push(hint.clone());
    }
    lines.join("\n")
}
