use jv_ast::{types::PrimitiveReturnMetadata, GenericSignature, Program, Statement};
use jv_lexer::{Token, TokenType};

use crate::syntax::support::{merge_spans, span_from_token};

use super::{
    metadata::{collect_raw_directives_from_trivia, primitive_reference_from_segments},
    SemanticsContext, SemanticsPass, SemanticsStatus,
};

#[derive(Default)]
pub struct PrimitiveReturnPass;

impl SemanticsPass for PrimitiveReturnPass {
    fn name(&self) -> &'static str {
        "stage2-primitive-return"
    }

    fn run(&self, context: &mut SemanticsContext<'_>) -> SemanticsStatus {
        let tokens = context.tokens();
        annotate_program_for_primitives(context.program_mut(), tokens);
        SemanticsStatus::Continue
    }
}

#[derive(Default)]
pub struct GenericDirectivePass;

impl SemanticsPass for GenericDirectivePass {
    fn name(&self) -> &'static str {
        "stage2-raw-directives"
    }

    fn run(&self, context: &mut SemanticsContext<'_>) -> SemanticsStatus {
        let tokens = context.tokens();
        annotate_program_for_directives(context.program_mut(), tokens);
        SemanticsStatus::Continue
    }
}

fn annotate_program_for_primitives(program: &mut Program, tokens: &[Token]) {
    for statement in &mut program.statements {
        annotate_statement_for_primitives(statement, tokens);
    }
}

fn annotate_statement_for_primitives(statement: &mut Statement, tokens: &[Token]) {
    match statement {
        Statement::FunctionDeclaration {
            primitive_return,
            return_type,
            span,
            ..
        } => {
            if primitive_return.is_none() && return_type.is_none() {
                if let Some(metadata) = detect_primitive_return(tokens, span) {
                    *primitive_return = Some(metadata);
                }
            }
        }
        Statement::ClassDeclaration { methods, .. } => {
            for method in methods {
                annotate_statement_for_primitives(method, tokens);
            }
        }
        Statement::InterfaceDeclaration { methods, .. } => {
            for method in methods {
                annotate_statement_for_primitives(method, tokens);
            }
        }
        Statement::ExtensionFunction(extension) => {
            annotate_statement_for_primitives(&mut extension.function, tokens);
        }
        Statement::DataClassDeclaration { .. }
        | Statement::ValDeclaration { .. }
        | Statement::VarDeclaration { .. }
        | Statement::Expression { .. }
        | Statement::Return { .. }
        | Statement::Throw { .. }
        | Statement::Assignment { .. }
        | Statement::Import { .. }
        | Statement::ForIn { .. }
        | Statement::Break(_)
        | Statement::Continue(_)
        | Statement::Package { .. }
        | Statement::Comment(_)
        | Statement::Concurrency(_)
        | Statement::ResourceManagement(_) => {}
    }
}

fn detect_primitive_return(
    tokens: &[Token],
    span: &jv_ast::Span,
) -> Option<PrimitiveReturnMetadata> {
    let tokens_in_span = tokens_within_span(tokens, span);
    let fun_index = tokens_in_span
        .iter()
        .position(|token| matches!(token.token_type, TokenType::Fun))?;

    let signature_tokens = tokens_in_span
        .iter()
        .skip(fun_index + 1)
        .take_while(|token| !matches!(token.token_type, TokenType::LeftParen))
        .copied()
        .collect::<Vec<_>>();

    if signature_tokens.is_empty() {
        return None;
    }

    let function_name_index = find_function_name_index(&signature_tokens)?;
    let prefix_tokens = &signature_tokens[..function_name_index];
    let segments = collect_prefix_segments(prefix_tokens)?;
    if segments.is_empty() {
        return None;
    }

    let reference_span = primitive_prefix_span(prefix_tokens)?;

    primitive_reference_from_segments(&segments, &reference_span)
        .map(|reference| PrimitiveReturnMetadata { reference })
}

fn find_function_name_index(tokens: &[&Token]) -> Option<usize> {
    let mut depth = 0usize;

    for (index, token) in tokens.iter().enumerate().rev() {
        match token.token_type {
            TokenType::Greater => depth = depth.saturating_add(1),
            TokenType::Less => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            _ if depth > 0 => continue,
            TokenType::Identifier(_) => return Some(index),
            _ => continue,
        }
    }

    None
}

fn collect_prefix_segments(tokens: &[&Token]) -> Option<Vec<String>> {
    if tokens.is_empty() {
        return None;
    }

    let mut expect_identifier = true;
    let mut segments = Vec::new();

    for token in tokens {
        match (&token.token_type, expect_identifier) {
            (TokenType::Identifier(name), true) => {
                segments.push(name.clone());
                expect_identifier = false;
            }
            (TokenType::Dot, false) => {
                expect_identifier = true;
            }
            _ => return None,
        }
    }

    if expect_identifier {
        return None;
    }

    Some(segments)
}

fn primitive_prefix_span(tokens: &[&Token]) -> Option<jv_ast::Span> {
    let first = tokens.first()?;
    let last = tokens.last()?;
    let start = span_from_token(first);
    let end = span_from_token(last);
    Some(merge_spans(&start, &end))
}

fn tokens_within_span<'a>(tokens: &'a [Token], span: &jv_ast::Span) -> Vec<&'a Token> {
    tokens
        .iter()
        .filter(|token| token_in_span(token, span))
        .collect()
}

fn token_in_span(token: &Token, span: &jv_ast::Span) -> bool {
    let start = (token.line, token.column);
    let span_start = (span.start_line, span.start_column);
    let span_end = (span.end_line, span.end_column);

    start >= span_start && start <= span_end
}

fn annotate_program_for_directives(program: &mut Program, tokens: &[Token]) {
    for statement in &mut program.statements {
        annotate_statement_for_directives(statement, tokens);
    }
}

fn annotate_statement_for_directives(statement: &mut Statement, tokens: &[Token]) {
    match statement {
        Statement::FunctionDeclaration {
            generic_signature, ..
        } => {
            if let Some(signature) = generic_signature {
                populate_raw_directives(signature, tokens);
            }
        }
        Statement::ClassDeclaration {
            generic_signature,
            methods,
            ..
        } => {
            if let Some(signature) = generic_signature {
                populate_raw_directives(signature, tokens);
            }
            for method in methods {
                annotate_statement_for_directives(method, tokens);
            }
        }
        Statement::InterfaceDeclaration {
            generic_signature,
            methods,
            ..
        } => {
            if let Some(signature) = generic_signature {
                populate_raw_directives(signature, tokens);
            }
            for method in methods {
                annotate_statement_for_directives(method, tokens);
            }
        }
        Statement::ExtensionFunction(extension) => {
            annotate_statement_for_directives(&mut extension.function, tokens);
        }
        Statement::DataClassDeclaration { .. }
        | Statement::ValDeclaration { .. }
        | Statement::VarDeclaration { .. }
        | Statement::Expression { .. }
        | Statement::Return { .. }
        | Statement::Throw { .. }
        | Statement::Assignment { .. }
        | Statement::Import { .. }
        | Statement::ForIn { .. }
        | Statement::Break(_)
        | Statement::Continue(_)
        | Statement::Package { .. }
        | Statement::Comment(_)
        | Statement::Concurrency(_)
        | Statement::ResourceManagement(_) => {}
    }
}

fn populate_raw_directives(signature: &mut GenericSignature, tokens: &[Token]) {
    if !signature.raw_directives.is_empty() {
        return;
    }

    let directives = tokens
        .iter()
        .filter(|token| token_in_span(token, &signature.span))
        .filter(|token| matches!(token.token_type, TokenType::Less | TokenType::Greater))
        .flat_map(|token| collect_raw_directives_from_trivia(&token.leading_trivia))
        .collect::<Vec<_>>();

    if !directives.is_empty() {
        signature.raw_directives = directives;
    }
}
