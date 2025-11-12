//! Rowan 構文木で使用するシンボル定義と文法スケルトン。

mod kinds;

pub use kinds::{SyntaxKind, TokenKind};

/// package/import/val/var を中心とした文法スケルトン。
///
/// 今後 `ungrammar` や同等 DSL から自動生成することを想定しており、
/// 初期段階では設計の指針となる文字列表現として保持する。
/// `AsKw` トークンは import alias 支援のためのプレースホルダーで、Lexer への導入は後続タスクで行う。
pub const STATEMENT_GRAMMAR: &str = r#"
Root = PackageDeclaration? ImportDeclaration* StatementList
PackageDeclaration = AnnotationList? PackageKw PackageName
PackageName = QualifiedName
ImportDeclaration = AnnotationList? ImportKw ImportPath ImportClause?
ImportPath = QualifiedName
ImportClause = ImportWildcard | ImportAlias
ImportWildcard = Dot Star
ImportAlias = Identifier Identifier
StatementList = (CommentStatement | UnitTypeDefinition | AssignmentStatement | ValDeclaration | VarDeclaration | FunctionDeclaration | TestDeclaration | ClassDeclaration | WhenStatement | ForStatement | ReturnStatement | ThrowStatement | BreakStatement | ContinueStatement | UseStatement | DeferStatement | SpawnStatement | LogBlockExpression)*
CommentStatement = LineComment | BlockComment
AssignmentStatement = AssignmentTarget Assign Expression
AssignmentTarget = BindingPattern | Identifier Dot Identifier (Dot Identifier)*
ValDeclaration = AnnotationList? ModifierList? ValKw BindingPattern TypeAnnotation? InitializerClause?
VarDeclaration = AnnotationList? ModifierList? VarKw BindingPattern TypeAnnotation? InitializerClause?
BindingPattern = Identifier | BindingTuplePattern | BindingListPattern
BindingTuplePattern = '(' (BindingPattern ((Comma | LayoutComma) BindingPattern)*)? ')'
BindingListPattern = '[' (BindingPattern ((Comma | LayoutComma) BindingPattern)*)? ']'
TypeAnnotation = Colon (UnitTypeAnnotation | Expression)
UnitTypeAnnotation = Expression (At Identifier | At LeftBracket ExpressionToken RightBracket | Identifier | LeftBracket ExpressionToken RightBracket)
UnitTypeDefinition = UnitHeader UnitBody
UnitHeader = At UnitCategory LeftParen UnitBaseType RightParen UnitName UnitDefaultMarker?
UnitCategory = Identifier
UnitBaseType = Expression
UnitName = Identifier
UnitDefaultMarker = Bang
UnitBody = LeftBrace (UnitDependency | UnitRelation | UnitConversionBlock | UnitReverseConversionBlock)* RightBrace
UnitDependency = Identifier Colon Assign Expression
UnitRelation = Identifier Arrow Identifier
UnitConversionBlock = At Identifier Block
UnitReverseConversionBlock = At Identifier Block
UnitLiteral = ExpressionToken
InitializerClause = Assign Expression
AnnotationList = Annotation Annotation*
Annotation = At QualifiedName AnnotationArgumentList?
AnnotationArgumentList = LeftParen (AnnotationArgument (Comma AnnotationArgument)*)? RightParen
AnnotationArgument = Identifier Assign Expression | Expression
ModifierList = Modifier Modifier*
Modifier = Identifier
FunctionDeclaration = AnnotationList? ModifierList? FunKw Identifier TypeParameterList? FunctionParameterList FunctionReturnType? WhereClause? (Block | Assign Expression)
TestDeclaration = AnnotationList? Identifier StringLiteral TestDataset? TestParameterList? Block
TestDataset = LeftBracket (TestDatasetRow ((Comma | LayoutComma) TestDatasetRow)*)? RightBracket
TestDatasetRow = Annotation | Expression
TestParameterList = LeftParen (TestParameter ((Comma | LayoutComma) TestParameter)*)? RightParen
TestParameter = BindingPattern TypeAnnotation?
FunctionParameterList = LeftParen (FunctionParameter (Comma FunctionParameter)*)? RightParen
FunctionParameter = ParameterModifierList? BindingPattern TypeAnnotation?
ParameterModifierList = ParameterModifier ParameterModifier*
ParameterModifier = ValKw | VarKw
FunctionReturnType = Colon Expression
TypeParameterList = Less (TypeParameter (Comma TypeParameter)*)? Greater
TypeParameter = Identifier
WhereClause = WhereKw WherePredicate (Comma WherePredicate)*
WherePredicate = Identifier Colon Expression
ClassDeclaration = AnnotationList? ModifierList? (ClassKw | DataKw) Identifier TypeParameterList? ClassBody?
ClassBody = LeftBrace StatementList RightBrace
Block = LeftBrace StatementList RightBrace
WhenStatement = WhenKw Expression LeftBrace WhenBranch* RightBrace
WhenBranch = (Expression | ElseKw) Arrow (Block | Expression)
ForStatement = ForKw LeftParen BindingPattern InKw Expression RightParen Block
ReturnStatement = ReturnKw Expression?
ThrowStatement = ThrowKw Expression
BreakStatement = BreakKw
ContinueStatement = ContinueKw
UseStatement = UseKw LeftParen Expression RightParen Block
DeferStatement = DeferKw Block
SpawnStatement = SpawnKw Block
QualifiedName = QualifiedNameSegment (Dot QualifiedNameSegment)*
QualifiedNameSegment = Identifier
Expression = ExpressionToken | UnitLiteral | LogBlockExpression | RegexCommand
RegexCommand = ExpressionToken
LogBlockExpression = (LogKw | TraceKw | DebugKw | InfoKw | WarnKw | ErrorKw) Block
StringLiteral = 'STRING_LITERAL'
PackageKw = 'package'
ImportKw = 'import'
ValKw = 'val'
VarKw = 'var'
FunKw = 'fun'
ClassKw = 'class'
DataKw = 'data'
ReturnKw = 'return'
ThrowKw = 'throw'
BreakKw = 'break'
ContinueKw = 'continue'
UseKw = 'use'
DeferKw = 'defer'
SpawnKw = 'spawn'
ElseKw = 'else'
WhenKw = 'when'
LogKw = 'LOG'
TraceKw = 'TRACE'
DebugKw = 'DEBUG'
InfoKw = 'INFO'
WarnKw = 'WARN'
ErrorKw = 'ERROR'
ForKw = 'for'
InKw = 'in'
WhereKw = 'where'
LineComment = 'LINE_COMMENT'
BlockComment = 'BLOCK_COMMENT'
JavaDocComment = 'JAVA_DOC_COMMENT'
At = '@'
Dot = '.'
Star = '*'
Assign = '='
Colon = ':'
Comma = ','
LayoutComma = 'LAYOUT_COMMA'
LeftParen = '('
RightParen = ')'
LeftBrace = '{'
RightBrace = '}'
LeftBracket = '['
RightBracket = ']'
Less = '<'
Greater = '>'
Arrow = '->'
Bang = '!'
Identifier = 'IDENTIFIER'
ExpressionToken = 'EXPRESSION'
Bang = '!'
"#;

#[cfg(test)]
mod tests {
    use super::*;
    use ungrammar::{Grammar, Rule};

    #[test]
    fn statement_grammar_is_ungrammar_compatible() {
        let grammar: Grammar = STATEMENT_GRAMMAR
            .parse()
            .expect("statement grammar should be accepted by ungrammar");

        assert!(
            grammar.iter().any(|node| grammar[node].name == "Root"),
            "ungrammar-parsed grammar must contain a `Root` rule"
        );

        let round_tripped = serialize_grammar(&grammar);
        assert_eq!(
            STATEMENT_GRAMMAR.trim(),
            round_tripped.trim(),
            "round-tripped grammar must match STATEMENT_GRAMMAR verbatim"
        );
    }

    fn serialize_grammar(grammar: &Grammar) -> String {
        use std::collections::HashMap;

        let mut name_to_node: HashMap<&str, _> = HashMap::new();
        for node in grammar.iter() {
            let data = &grammar[node];
            name_to_node.insert(data.name.as_str(), node);
        }

        let ordered_names: Vec<&str> = STATEMENT_GRAMMAR
            .trim()
            .lines()
            .filter_map(|line| line.split_once('='))
            .map(|(name, _)| name.trim())
            .collect();

        let mut buffer = String::new();
        for (index, name) in ordered_names.iter().enumerate() {
            if index > 0 {
                buffer.push('\n');
            }
            let node = name_to_node
                .get(name)
                .unwrap_or_else(|| panic!("grammar missing rule for `{name}`"));
            let data = &grammar[*node];
            buffer.push_str(name);
            buffer.push_str(" = ");
            write_rule(grammar, &data.rule, &mut buffer, Precedence::Lowest);
        }
        buffer
    }

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum Precedence {
        Lowest,
        Alt,
        Seq,
        Postfix,
        Atom,
    }

    fn write_rule(grammar: &Grammar, rule: &Rule, out: &mut String, parent_prec: Precedence) {
        let current_prec = precedence(rule);
        let needs_paren = current_prec < parent_prec;
        if needs_paren {
            out.push('(');
        }
        match rule {
            Rule::Node(node) => out.push_str(&grammar[*node].name),
            Rule::Token(token) => {
                out.push('\'');
                out.push_str(&grammar[*token].name);
                out.push('\'');
            }
            Rule::Alt(rules) => {
                for (idx, child) in rules.iter().enumerate() {
                    if idx > 0 {
                        out.push_str(" | ");
                    }
                    write_rule(grammar, child, out, Precedence::Alt);
                }
            }
            Rule::Seq(rules) => {
                for (idx, child) in rules.iter().enumerate() {
                    if idx > 0 {
                        out.push(' ');
                    }
                    write_rule(grammar, child, out, Precedence::Seq);
                }
            }
            Rule::Opt(rule) => {
                write_rule(grammar, rule, out, Precedence::Postfix);
                out.push('?');
            }
            Rule::Rep(rule) => {
                write_rule(grammar, rule, out, Precedence::Postfix);
                out.push('*');
            }
            Rule::Labeled { label, rule } => {
                out.push_str(label);
                out.push(':');
                write_rule(grammar, rule, out, Precedence::Atom);
            }
        }
        if needs_paren {
            out.push(')');
        }
    }

    fn precedence(rule: &Rule) -> Precedence {
        match rule {
            Rule::Alt(_) => Precedence::Alt,
            Rule::Seq(_) => Precedence::Seq,
            Rule::Opt(_) | Rule::Rep(_) => Precedence::Postfix,
            Rule::Labeled { .. } | Rule::Node(_) | Rule::Token(_) => Precedence::Atom,
        }
    }
}
