# codegen-architecture-redesign: Javaコード生成器アーキテクチャ刷新仕様

## Executive Summary

本書は、実証実験フェーズで得た知見を基に、jv言語の本格的なプロダクション開発に向けた**Java コード生成器の完全リプレース**を設計する。

### 背景
- **現状**: 約12,400行のモノリシックなコード生成器（`jv_codegen_java`）
- **課題**: アーキテクチャ設計なしの実証実験先行により、保守性・拡張性が限界
- **前提**: jv言語の優位性は実証済み。リリース前の今が抜本的改善の最適時期
- **制約**: 段階的移行は不要。別ブランチでの完全刷新を前提とする

### 成功事例からの学び
`jv_parser_rowan`は従来の巨大パーサ（Chumsky依存）をStrategy Patternベースのイベント駆動アーキテクチャで刷新し、以下を実現した：
- ファイルサイズ: 2,800行 → 200-400行/戦略（約70%削減）
- 保守性: 各戦略が独立したテスト可能単位
- 拡張性: 新構文追加時の既存コードへの影響ゼロ

**本仕様の目標**: 同等の改善をコード生成器で実現する。

---

## Architectural Vision

### 設計哲学

#### 1. **明示的な層分離**（Explicit Layering）
```
IrProgram (入力)
    ↓
[IR層] 構文構造の理解
    ↓
[意味層] Java固有の意味論適用
    ↓
[生成層] テキスト化
    ↓
JavaCompilationUnit (出力)
```

各層は独立したクレートまたはモジュールとして分離し、依存関係を一方向に保つ。

#### 2. **型駆動設計**（Type-Driven Development）
Java構文要素を専用の型で表現し、コンパイラに不正状態を検出させる。

```rust
// 悪い例（現状）
fn generate_class(&mut self, stmt: &IrStatement) -> Result<String, CodeGenError>

// 良い例（新設計）
struct JavaClass { /* ... */ }
impl From<&IrStatement> for JavaClass { /* ... */ }
impl Render for JavaClass { /* ... */ }
```

#### 3. **戦略パターンによる拡張性**（Strategy Pattern for Extensibility）
各Java構文要素に対する生成戦略を独立させ、レジストリで管理。

```rust
trait CodeGenStrategy: Sync + Send {
    fn name(&self) -> &'static str;
    fn applies_to(&self, decl: &JavaDeclaration) -> bool;
    fn generate(&self, ctx: &RenderContext, decl: &JavaDeclaration) -> String;
}
```

#### 4. **イミュータブルな中間表現**（Immutable Intermediate Representation）
生成過程の中間状態を不変な型で表現し、副作用を制御。

```rust
struct JavaClass {
    modifiers: JavaModifiers,      // 不変
    name: Identifier,               // 不変
    members: Vec<JavaMember>,       // 不変
}

impl JavaClass {
    fn with_member(mut self, member: JavaMember) -> Self {
        self.members.push(member);
        self
    }
}
```

---

## Architecture Overview

### クレート構成

```
jv_codegen_java_v2/
├── core/           # コア抽象化
│   ├── traits.rs   # CodeGenStrategy, Render等
│   ├── context.rs  # RenderContext, ImportRegistry
│   └── error.rs    # CodeGenError, CodeGenDiagnostic
├── ir_adapter/     # IR→Java中間表現への変換
│   ├── transformer.rs
│   └── diagnostics.rs
├── java_model/     # Java構文要素の型モデル
│   ├── declarations.rs  # JavaClass, JavaRecord等
│   ├── expressions.rs   # JavaExpression, JavaLiteral等
│   ├── statements.rs    # JavaStatement
│   ├── types.rs         # JavaType, JavaTypeParameter
│   └── modifiers.rs     # JavaModifiers, JavaAnnotation
├── strategies/     # 生成戦略（各200-400行）
│   ├── mod.rs      # レジストリ
│   ├── class.rs
│   ├── record.rs
│   ├── method.rs
│   ├── field.rs
│   ├── expression.rs
│   └── sample.rs
├── rendering/      # テキスト生成
│   ├── builder.rs  # CodeBuilder（Rope実装）
│   ├── formatter.rs
│   └── imports.rs
├── targeting/      # Java version targeting
│   ├── java21.rs
│   └── java25.rs
└── lib.rs
```

### データフロー

```
IrProgram
    ↓ [IrAdapter::transform]
JavaProgram { declarations: Vec<JavaDeclaration> }
    ↓ [StrategyRegistry::dispatch]
Vec<RenderArtifact>
    ↓ [RenderContext::assemble]
JavaCompilationUnit { package, imports, source }
```

---

## Core Components

### 1. Java Model（型モデル）

#### 1.1 宣言系

```rust
/// Java宣言の抽象
#[derive(Debug, Clone, PartialEq)]
pub enum JavaDeclaration {
    Class(JavaClass),
    Record(JavaRecord),
    Interface(JavaInterface),
    Enum(JavaEnum),
    Method(JavaMethod),      // トップレベル関数用
    Field(JavaField),        // トップレベル変数用
    Sample(JavaSampleHelper),
}

/// クラス宣言
#[derive(Debug, Clone, PartialEq)]
pub struct JavaClass {
    pub modifiers: JavaModifiers,
    pub name: Identifier,
    pub type_parameters: Vec<JavaTypeParameter>,
    pub superclass: Option<JavaType>,
    pub interfaces: Vec<JavaType>,
    pub members: Vec<JavaMember>,
    pub metadata: ClassMetadata,
}

#[derive(Debug, Clone, PartialEq)]
pub enum JavaMember {
    Field(JavaField),
    Method(JavaMethod),
    Constructor(JavaConstructor),
    NestedClass(Box<JavaClass>),
    NestedRecord(Box<JavaRecord>),
}

/// Record宣言
#[derive(Debug, Clone, PartialEq)]
pub struct JavaRecord {
    pub modifiers: JavaModifiers,
    pub name: Identifier,
    pub type_parameters: Vec<JavaTypeParameter>,
    pub components: Vec<JavaRecordComponent>,
    pub interfaces: Vec<JavaType>,
    pub methods: Vec<JavaMethod>,
    pub metadata: RecordMetadata,
}
```

#### 1.2 型システム

```rust
/// Java型の表現
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum JavaType {
    /// プリミティブ型（int, long, boolean等）
    Primitive(JavaPrimitive),

    /// 参照型（String, List<T>等）
    Reference {
        name: QualifiedName,
        type_args: Vec<JavaType>,
    },

    /// 型変数（T, E等）
    TypeVariable(Identifier),

    /// 配列型
    Array(Box<JavaType>),

    /// ワイルドカード（? extends T, ? super T）
    Wildcard {
        bound: Option<WildcardBound>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JavaPrimitive {
    Boolean,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Char,
}

impl JavaPrimitive {
    /// ボックス型への変換
    pub fn boxed(self) -> QualifiedName {
        match self {
            Self::Int => "java.lang.Integer".into(),
            Self::Boolean => "java.lang.Boolean".into(),
            // ...
        }
    }

    /// widening conversion可能か
    pub fn can_widen_to(self, target: Self) -> bool {
        use JavaPrimitive::*;
        matches!((self, target),
            (Byte, Short | Int | Long | Float | Double) |
            (Short, Int | Long | Float | Double) |
            (Int, Long | Float | Double) |
            // ...
        )
    }
}
```

#### 1.3 式と文

```rust
/// Java式
#[derive(Debug, Clone, PartialEq)]
pub enum JavaExpression {
    Literal(JavaLiteral),
    Identifier(Identifier),
    MethodCall {
        receiver: Option<Box<JavaExpression>>,
        method: Identifier,
        type_args: Vec<JavaType>,
        arguments: Vec<JavaExpression>,
    },
    FieldAccess {
        receiver: Box<JavaExpression>,
        field: Identifier,
    },
    ArrayAccess {
        array: Box<JavaExpression>,
        index: Box<JavaExpression>,
    },
    Binary {
        left: Box<JavaExpression>,
        operator: BinaryOperator,
        right: Box<JavaExpression>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<JavaExpression>,
    },
    Cast {
        target_type: JavaType,
        expression: Box<JavaExpression>,
    },
    InstanceOf {
        expression: Box<JavaExpression>,
        pattern: TypePattern,
    },
    Lambda {
        parameters: Vec<JavaParameter>,
        body: LambdaBody,
    },
    Switch(JavaSwitchExpression),
    // ...
}

/// Java文
#[derive(Debug, Clone, PartialEq)]
pub enum JavaStatement {
    VariableDeclaration {
        modifiers: LocalModifiers,
        var_type: JavaType,
        name: Identifier,
        initializer: Option<JavaExpression>,
    },
    Expression(JavaExpression),
    Return(Option<JavaExpression>),
    Throw(JavaExpression),
    If {
        condition: JavaExpression,
        then_block: Vec<JavaStatement>,
        else_block: Option<Vec<JavaStatement>>,
    },
    Switch(JavaSwitchStatement),
    For(JavaForLoop),
    While {
        condition: JavaExpression,
        body: Vec<JavaStatement>,
    },
    Try {
        resources: Vec<JavaResource>,
        body: Vec<JavaStatement>,
        catches: Vec<JavaCatchClause>,
        finally_block: Option<Vec<JavaStatement>>,
    },
    // ...
}
```

### 2. IR Adapter（変換層）

```rust
/// IR → Java Model への変換エンジン
pub struct IrAdapter {
    config: CodeGenConfig,
    import_registry: ImportRegistry,
    type_mapper: TypeMapper,
    diagnostics: Vec<CodeGenDiagnostic>,
}

impl IrAdapter {
    /// IrProgramを変換
    pub fn transform(&mut self, program: &IrProgram) -> Result<JavaProgram, CodeGenError> {
        let mut declarations = Vec::new();

        // パッケージとインポートは直接JavaProgramへ
        let package = program.package.clone();
        let imports = self.transform_imports(&program.imports)?;

        // 型宣言を変換
        for ir_stmt in &program.type_declarations {
            match self.transform_declaration(ir_stmt) {
                Ok(decl) => declarations.push(decl),
                Err(e) => {
                    self.diagnostics.push(e.into_diagnostic());
                    continue;
                }
            }
        }

        // スクリプトクラス生成（トップレベル文がある場合）
        if let Some(script_class) = self.build_script_class(&program.type_declarations)? {
            declarations.push(JavaDeclaration::Class(script_class));
        }

        Ok(JavaProgram {
            package,
            imports,
            declarations,
            metadata: self.extract_metadata(program),
        })
    }

    /// 個別宣言の変換
    fn transform_declaration(&mut self, stmt: &IrStatement) -> Result<JavaDeclaration, CodeGenError> {
        match stmt {
            IrStatement::ClassDeclaration { name, fields, methods, .. } => {
                Ok(JavaDeclaration::Class(self.build_class(name, fields, methods)?))
            }
            IrStatement::RecordDeclaration { name, components, .. } => {
                Ok(JavaDeclaration::Record(self.build_record(name, components)?))
            }
            IrStatement::MethodDeclaration { .. } => {
                Ok(JavaDeclaration::Method(self.build_method(stmt)?))
            }
            IrStatement::SampleDeclaration(sample) => {
                Ok(JavaDeclaration::Sample(self.build_sample_helper(sample)?))
            }
            _ => Err(CodeGenError::UnsupportedConstruct {
                construct: format!("Unexpected top-level: {:?}", stmt),
                span: None,
            })
        }
    }

    /// クラス構築
    fn build_class(&mut self,
                   name: &str,
                   ir_fields: &[IrStatement],
                   ir_methods: &[IrStatement]) -> Result<JavaClass, CodeGenError> {
        let mut members = Vec::new();

        // フィールド変換
        for ir_field in ir_fields {
            if let IrStatement::FieldDeclaration { .. } = ir_field {
                members.push(JavaMember::Field(self.transform_field(ir_field)?));
            }
        }

        // メソッド変換
        for ir_method in ir_methods {
            if let IrStatement::MethodDeclaration { .. } = ir_method {
                members.push(JavaMember::Method(self.transform_method(ir_method)?));
            }
        }

        Ok(JavaClass {
            modifiers: self.transform_modifiers(/* ... */)?,
            name: Identifier::new(name),
            type_parameters: vec![],
            superclass: None,
            interfaces: vec![],
            members,
            metadata: ClassMetadata::default(),
        })
    }

    /// 式の変換
    fn transform_expression(&mut self, expr: &IrExpression) -> Result<JavaExpression, CodeGenError> {
        match expr {
            IrExpression::Literal(lit, _) => {
                Ok(JavaExpression::Literal(self.transform_literal(lit)?))
            }
            IrExpression::Identifier { name, .. } => {
                Ok(JavaExpression::Identifier(Identifier::new(name)))
            }
            IrExpression::MethodCall { receiver, method_name, args, .. } => {
                Ok(JavaExpression::MethodCall {
                    receiver: receiver.as_ref()
                        .map(|r| self.transform_expression(r))
                        .transpose()?
                        .map(Box::new),
                    method: Identifier::new(method_name),
                    type_args: vec![],
                    arguments: args.iter()
                        .map(|a| self.transform_expression(a))
                        .collect::<Result<_, _>>()?,
                })
            }
            IrExpression::Binary { left, op, right, .. } => {
                Ok(JavaExpression::Binary {
                    left: Box::new(self.transform_expression(left)?),
                    operator: self.transform_binary_op(*op)?,
                    right: Box::new(self.transform_expression(right)?),
                })
            }
            // ... 他の式パターン
            _ => Err(CodeGenError::UnsupportedExpression {
                expr_type: format!("{:?}", expr),
                span: expr.span(),
            })
        }
    }
}
```

### 3. Strategy System（戦略パターン）

```rust
/// コード生成戦略の抽象
pub trait CodeGenStrategy: Sync + Send {
    /// 戦略名（診断用）
    fn name(&self) -> &'static str;

    /// この戦略が適用可能か
    fn applies_to(&self, decl: &JavaDeclaration) -> bool;

    /// コード生成を実行
    fn generate(&self, ctx: &RenderContext, decl: &JavaDeclaration) -> Result<String, CodeGenError>;
}

/// クラス生成戦略
pub struct ClassStrategy;

impl CodeGenStrategy for ClassStrategy {
    fn name(&self) -> &'static str { "class" }

    fn applies_to(&self, decl: &JavaDeclaration) -> bool {
        matches!(decl, JavaDeclaration::Class(_))
    }

    fn generate(&self, ctx: &RenderContext, decl: &JavaDeclaration) -> Result<String, CodeGenError> {
        let JavaDeclaration::Class(class) = decl else {
            return Err(CodeGenError::StrategyMismatch {
                strategy: self.name(),
                actual: decl.kind(),
            });
        };

        let mut builder = ctx.builder();

        // アノテーション
        for annotation in &class.modifiers.annotations {
            builder.push_line(&self.render_annotation(ctx, annotation)?);
        }

        // ジェネリクスメタデータコメント（診断用）
        if !class.type_parameters.is_empty() {
            builder.push_line(&format!(
                "// Generic metadata: {}",
                self.render_type_params_metadata(&class.type_parameters)
            ));
        }

        // ヘッダー
        let header = self.build_class_header(ctx, class)?;
        builder.push_line(&format!("{} {{", header));
        builder.indent();

        // メンバー
        self.render_members(ctx, &mut builder, &class.members)?;

        builder.dedent();
        builder.push_line("}");

        Ok(builder.build())
    }
}

impl ClassStrategy {
    fn build_class_header(&self, ctx: &RenderContext, class: &JavaClass) -> Result<String, CodeGenError> {
        let mut parts = Vec::new();

        // modifiers
        let mods = ModifierRenderer::new(ctx).render(&class.modifiers);
        if !mods.is_empty() {
            parts.push(mods);
        }

        // class name
        parts.push(format!("class {}", class.name));

        // type parameters
        if !class.type_parameters.is_empty() {
            parts.push(TypeParameterRenderer::new(ctx).render(&class.type_parameters)?);
        }

        // extends
        if let Some(ref superclass) = class.superclass {
            parts.push(format!("extends {}", TypeRenderer::new(ctx).render(superclass)?));
        }

        // implements
        if !class.interfaces.is_empty() {
            let interfaces = class.interfaces.iter()
                .map(|iface| TypeRenderer::new(ctx).render(iface))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");
            parts.push(format!("implements {}", interfaces));
        }

        // sealed permits (Java 17+)
        if class.modifiers.is_sealed {
            if let Some(permits) = ctx.targeting().permits_clause(&class.modifiers.permitted_types) {
                parts.push(permits);
            }
        }

        Ok(parts.join(" "))
    }

    fn render_members(&self,
                      ctx: &RenderContext,
                      builder: &mut CodeBuilder,
                      members: &[JavaMember]) -> Result<(), CodeGenError> {
        let mut first = true;

        for member in members {
            if !first {
                builder.push_line("");
            }
            first = false;

            let code = match member {
                JavaMember::Field(field) => FieldStrategy.generate(ctx, &JavaDeclaration::Field(field.clone()))?,
                JavaMember::Method(method) => MethodStrategy.generate(ctx, &JavaDeclaration::Method(method.clone()))?,
                JavaMember::NestedClass(class) => ClassStrategy.generate(ctx, &JavaDeclaration::Class((**class).clone()))?,
                // ...
            };

            builder.push_lines(&code);
        }

        Ok(())
    }
}

/// レコード生成戦略
pub struct RecordStrategy;

impl CodeGenStrategy for RecordStrategy {
    fn name(&self) -> &'static str { "record" }

    fn applies_to(&self, decl: &JavaDeclaration) -> bool {
        matches!(decl, JavaDeclaration::Record(_))
    }

    fn generate(&self, ctx: &RenderContext, decl: &JavaDeclaration) -> Result<String, CodeGenError> {
        let JavaDeclaration::Record(record) = decl else {
            return Err(CodeGenError::StrategyMismatch {
                strategy: self.name(),
                actual: decl.kind(),
            });
        };

        let mut builder = ctx.builder();

        // アノテーション
        for annotation in &record.modifiers.annotations {
            builder.push_line(&AnnotationRenderer::new(ctx).render(annotation)?);
        }

        // ヘッダー
        let mut header_parts = Vec::new();

        let mods = ModifierRenderer::new(ctx).render(&record.modifiers);
        if !mods.is_empty() {
            header_parts.push(mods);
        }

        header_parts.push(format!("record {}", record.name));

        // type parameters
        if !record.type_parameters.is_empty() {
            header_parts.push(TypeParameterRenderer::new(ctx).render(&record.type_parameters)?);
        }

        // components
        let components = record.components.iter()
            .map(|comp| format!("{} {}",
                TypeRenderer::new(ctx).render(&comp.component_type)?,
                comp.name))
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");
        header_parts.push(format!("({})", components));

        // implements
        if !record.interfaces.is_empty() {
            let interfaces = record.interfaces.iter()
                .map(|iface| TypeRenderer::new(ctx).render(iface))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");
            header_parts.push(format!("implements {}", interfaces));
        }

        let header = header_parts.join(" ");

        // メソッドがあればボディ、なければ空レコード
        if record.methods.is_empty() {
            builder.push_line(&format!("{} {{}}", header));
        } else {
            builder.push_line(&format!("{} {{", header));
            builder.indent();

            for method in &record.methods {
                let method_code = MethodStrategy.generate(ctx, &JavaDeclaration::Method(method.clone()))?;
                builder.push_lines(&method_code);
                builder.push_line("");
            }

            builder.dedent();
            builder.push_line("}");
        }

        Ok(builder.build())
    }
}

/// 戦略レジストリ
pub struct StrategyRegistry {
    strategies: Vec<Box<dyn CodeGenStrategy>>,
}

impl StrategyRegistry {
    pub fn default_registry() -> Self {
        Self {
            strategies: vec![
                Box::new(ClassStrategy),
                Box::new(RecordStrategy),
                Box::new(InterfaceStrategy),
                Box::new(MethodStrategy),
                Box::new(FieldStrategy),
                Box::new(SampleStrategy),
            ],
        }
    }

    pub fn dispatch(&self, ctx: &RenderContext, decl: &JavaDeclaration) -> Result<String, CodeGenError> {
        for strategy in &self.strategies {
            if strategy.applies_to(decl) {
                return strategy.generate(ctx, decl);
            }
        }

        Err(CodeGenError::NoApplicableStrategy {
            declaration_type: decl.kind(),
        })
    }
}
```

### 4. Rendering System（レンダリング層）

```rust
/// レンダリングコンテキスト
pub struct RenderContext {
    config: CodeGenConfig,
    targeting: TargetVersion,
    import_registry: Arc<Mutex<ImportRegistry>>,
}

impl RenderContext {
    pub fn builder(&self) -> CodeBuilder {
        CodeBuilder::new(self.config.indent_width)
    }

    pub fn targeting(&self) -> &TargetVersion {
        &self.targeting
    }

    pub fn register_import(&self, import: &str) {
        self.import_registry.lock().unwrap().add(import);
    }
}

/// コードビルダー（Rope実装）
pub struct CodeBuilder {
    rope: Rope,
    indent_level: usize,
    indent_width: usize,
}

impl CodeBuilder {
    pub fn new(indent_width: usize) -> Self {
        Self {
            rope: Rope::new(),
            indent_level: 0,
            indent_width,
        }
    }

    pub fn push_line(&mut self, line: &str) {
        let indent = " ".repeat(self.indent_level * self.indent_width);
        self.rope.insert(self.rope.len_chars(), &format!("{}{}\n", indent, line));
    }

    pub fn push_lines(&mut self, lines: &str) {
        for line in lines.lines() {
            self.push_line(line);
        }
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    pub fn build(self) -> String {
        self.rope.to_string()
    }
}

/// 型レンダラー
pub struct TypeRenderer<'ctx> {
    ctx: &'ctx RenderContext,
}

impl<'ctx> TypeRenderer<'ctx> {
    pub fn new(ctx: &'ctx RenderContext) -> Self {
        Self { ctx }
    }

    pub fn render(&self, java_type: &JavaType) -> Result<String, CodeGenError> {
        match java_type {
            JavaType::Primitive(prim) => Ok(self.render_primitive(*prim)),
            JavaType::Reference { name, type_args } => {
                self.ctx.register_import(&name.to_string());

                if type_args.is_empty() {
                    Ok(name.simple_name().to_string())
                } else {
                    let args = type_args.iter()
                        .map(|arg| self.render(arg))
                        .collect::<Result<Vec<_>, _>>()?
                        .join(", ");
                    Ok(format!("{}<{}>", name.simple_name(), args))
                }
            }
            JavaType::TypeVariable(name) => Ok(name.to_string()),
            JavaType::Array(element_type) => {
                Ok(format!("{}[]", self.render(element_type)?))
            }
            JavaType::Wildcard { bound } => {
                match bound {
                    None => Ok("?".to_string()),
                    Some(WildcardBound::Extends(ty)) => {
                        Ok(format!("? extends {}", self.render(ty)?))
                    }
                    Some(WildcardBound::Super(ty)) => {
                        Ok(format!("? super {}", self.render(ty)?))
                    }
                }
            }
        }
    }

    fn render_primitive(&self, prim: JavaPrimitive) -> String {
        match prim {
            JavaPrimitive::Boolean => "boolean",
            JavaPrimitive::Byte => "byte",
            JavaPrimitive::Short => "short",
            JavaPrimitive::Int => "int",
            JavaPrimitive::Long => "long",
            JavaPrimitive::Float => "float",
            JavaPrimitive::Double => "double",
            JavaPrimitive::Char => "char",
        }.to_string()
    }
}

/// インポート管理
pub struct ImportRegistry {
    imports: BTreeSet<String>,
    java_lang_types: HashSet<&'static str>,
}

impl ImportRegistry {
    pub fn new() -> Self {
        let mut java_lang_types = HashSet::new();
        java_lang_types.insert("String");
        java_lang_types.insert("Integer");
        java_lang_types.insert("Object");
        // ...

        Self {
            imports: BTreeSet::new(),
            java_lang_types,
        }
    }

    pub fn add(&mut self, fqn: &str) {
        // java.langは不要
        if fqn.starts_with("java.lang.") {
            let simple = fqn.strip_prefix("java.lang.").unwrap();
            if self.java_lang_types.contains(simple) {
                return;
            }
        }

        // パッケージ名を持つものだけ追加
        if fqn.contains('.') {
            self.imports.insert(fqn.to_string());
        }
    }

    pub fn to_import_statements(&self) -> Vec<String> {
        self.imports.iter()
            .map(|imp| format!("import {};", imp))
            .collect()
    }
}
```

### 5. Java Version Targeting

```rust
/// ターゲットバージョン
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetVersion {
    Java21,
    Java25,
}

impl TargetVersion {
    /// sealed class の permits 句が使えるか
    pub fn supports_permits_clause(&self) -> bool {
        matches!(self, Self::Java25)
    }

    /// permits句を生成（サポートしていればSome）
    pub fn permits_clause(&self, types: &[String]) -> Option<String> {
        if !self.supports_permits_clause() || types.is_empty() {
            return None;
        }

        Some(format!("permits {}", types.join(", ")))
    }

    /// フォールバックコメントを生成
    pub fn sealed_fallback_comment(&self, types: &[String]) -> Option<String> {
        if self.supports_permits_clause() || types.is_empty() {
            return None;
        }

        Some(format!("// sealed (permits: {}) - Java 21 fallback", types.join(", ")))
    }

    /// パターンマッチングでのdeconstructionが使えるか
    pub fn supports_deconstruction_patterns(&self) -> bool {
        matches!(self, Self::Java25)
    }

    /// switch式での複数case結合が使えるか
    pub fn supports_case_comma_separation(&self) -> bool {
        matches!(self, Self::Java25)
    }
}
```

---

## Migration Strategy

### Phase 1: Foundation（1-2週間）

#### 1.1 クレート作成
```bash
cargo new --lib jv_codegen_java_v2
```

#### 1.2 コア抽象化実装
- `core/traits.rs`: CodeGenStrategy, Render
- `core/context.rs`: RenderContext
- `core/error.rs`: エラー型

#### 1.3 Java Model実装
- `java_model/declarations.rs`: JavaClass, JavaRecord
- `java_model/types.rs`: JavaType, JavaPrimitive
- `java_model/modifiers.rs`: JavaModifiers

**検証**: 型モデルの単体テスト（型の等価性、変換等）

### Phase 2: IR Adapter（1週間）

#### 2.1 基本変換実装
- `ir_adapter/transformer.rs`: IrAdapter::transform
- クラス・レコード・メソッドの変換

#### 2.2 式変換実装
- `transform_expression`の実装
- リテラル、識別子、メソッド呼び出し、二項演算等

**検証**: 既存の`jv_ir::tests`から小規模IRを抽出してテスト

### Phase 3: Strategy System（2週間）

#### 3.1 基本戦略実装
- `strategies/class.rs`: ClassStrategy
- `strategies/record.rs`: RecordStrategy
- `strategies/method.rs`: MethodStrategy

#### 3.2 レンダリング実装
- `rendering/builder.rs`: CodeBuilder（Rope実装）
- `rendering/formatter.rs`: TypeRenderer, ModifierRenderer

#### 3.3 レジストリ実装
- `strategies/mod.rs`: StrategyRegistry

**検証**: ゴールデンテスト（既存の`tests/golden/expected/*.java`と比較）

### Phase 4: Advanced Features（2週間）

#### 4.1 式と文の戦略
- `strategies/expression.rs`: ExpressionStrategy
- Switch式、Lambda等の複雑な式

#### 4.2 Sample宣言
- `strategies/sample.rs`: SampleStrategy
- JSON/CSV埋め込み生成

#### 4.3 Targeting実装
- `targeting/java21.rs`: Java 21フォールバック
- `targeting/java25.rs`: Java 25最新機能

**検証**: ターゲットバージョン別のゴールデンテスト

### Phase 5: Integration & Testing（1週間）

#### 5.1 既存テストの移植
- `tests/generics.rs`
- `tests/sequence_streams.rs`
- `tests/loops.rs`
- 全ゴールデンテスト

#### 5.2 パフォーマンステスト
- ベンチマーク移植（`benches/pattern_switch.rs`等）
- 大規模ファイル生成のパフォーマンス測定

#### 5.3 診断品質検証
- エラーメッセージの品質確認
- スパン情報の正確性検証

**完了基準**:
- 全既存テストがパス（1000+ テストケース）
- パフォーマンス: 旧実装の±10%以内
- 診断品質: エラーメッセージが改善されていること

### Phase 6: Production Ready（1週間）

#### 6.1 CLI統合
- `jv_cli`から`jv_codegen_java_v2`を呼び出すよう変更
- フィーチャーフラグで切り替え可能に

#### 6.2 ドキュメント整備
- API ドキュメント（rustdoc）
- アーキテクチャ図
- 移行ガイド

#### 6.3 最終レビュー
- コードレビュー
- パフォーマンス最終確認
- エッジケーステスト

**完了基準**:
- `cargo doc --no-deps`が完全
- 全CI/CDパイプラインがグリーン
- jv言語のフルビルドが成功

---

## Performance Targets

### メモリ使用量
- **現状**: JavaCodeGenerator構造体 14フィールド（推定1KB+）
- **目標**: RenderContext < 200バイト、Java Model構造体は不変で共有可能

### 処理速度
- **小規模ファイル**（<100行）: <10ms（現状と同等）
- **中規模ファイル**（100-1000行）: <50ms（現状比10-20%改善を目標）
- **大規模ファイル**（1000+行）: <200ms（Rope実装による文字列結合最適化で改善）

### コード品質
- **行数削減**: 12,400行 → 8,000行前後（35%削減目標）
- **ファイルサイズ**: 最大2,813行 → 最大500行（戦略単位）
- **テストカバレッジ**: ≥90%（現状維持）

---

## Testing Strategy

### 単体テスト

#### Java Model
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_widening() {
        assert!(JavaPrimitive::Int.can_widen_to(JavaPrimitive::Long));
        assert!(!JavaPrimitive::Long.can_widen_to(JavaPrimitive::Int));
    }

    #[test]
    fn test_primitive_boxing() {
        assert_eq!(
            JavaPrimitive::Int.boxed(),
            QualifiedName::from("java.lang.Integer")
        );
    }

    #[test]
    fn test_type_equality() {
        let t1 = JavaType::Primitive(JavaPrimitive::Int);
        let t2 = JavaType::Primitive(JavaPrimitive::Int);
        assert_eq!(t1, t2);
    }
}
```

#### IR Adapter
```rust
#[test]
fn test_transform_simple_class() {
    let ir_class = IrStatement::ClassDeclaration {
        name: "Foo".to_string(),
        fields: vec![],
        methods: vec![],
        // ...
    };

    let mut adapter = IrAdapter::new(CodeGenConfig::default());
    let java_class = adapter.transform_declaration(&ir_class).unwrap();

    assert!(matches!(java_class, JavaDeclaration::Class(_)));
    if let JavaDeclaration::Class(class) = java_class {
        assert_eq!(class.name.as_str(), "Foo");
    }
}
```

### ゴールデンテスト

```rust
#[test]
fn test_golden_sequence_inline_map() {
    let ir = load_ir_from_fixture("sequence_inline_map");

    let mut adapter = IrAdapter::new(CodeGenConfig::java25());
    let java_program = adapter.transform(&ir).unwrap();

    let registry = StrategyRegistry::default_registry();
    let ctx = RenderContext::new(CodeGenConfig::java25());

    let mut source = String::new();
    for decl in java_program.declarations {
        source.push_str(&registry.dispatch(&ctx, &decl).unwrap());
        source.push('\n');
    }

    let expected = include_str!("../tests/golden/expected/sequence_inline_map__java25.java");
    assert_eq!(source.trim(), expected.trim());
}
```

### パフォーマンステスト

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_class_generation(c: &mut Criterion) {
    let ir = create_large_class_ir(100); // 100メソッド

    c.bench_function("generate_large_class", |b| {
        b.iter(|| {
            let mut adapter = IrAdapter::new(CodeGenConfig::default());
            let java_program = adapter.transform(black_box(&ir)).unwrap();
            // ...
        });
    });
}

criterion_group!(benches, bench_class_generation);
criterion_main!(benches);
```

---

## Error Handling & Diagnostics

### エラー型階層

```rust
#[derive(Debug, Clone)]
pub enum CodeGenError {
    /// IR構造が不正
    MalformedIr {
        description: String,
        span: Option<Span>,
    },

    /// サポートされていない構文
    UnsupportedConstruct {
        construct: String,
        span: Option<Span>,
    },

    /// 戦略の不一致
    StrategyMismatch {
        strategy: &'static str,
        actual: &'static str,
    },

    /// 型エラー
    TypeError {
        expected: String,
        actual: String,
        span: Option<Span>,
    },

    /// 内部エラー
    InternalError {
        message: String,
    },
}

impl CodeGenError {
    pub fn into_diagnostic(self) -> CodeGenDiagnostic {
        match self {
            Self::UnsupportedConstruct { construct, span } => {
                CodeGenDiagnostic {
                    severity: DiagnosticSeverity::Error,
                    message: format!("Unsupported construct: {}", construct),
                    span,
                    help: Some("This feature is not yet implemented in the code generator".to_string()),
                }
            }
            // ... 他のケース
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodeGenDiagnostic {
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub span: Option<Span>,
    pub help: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
}
```

### 診断の収集

```rust
impl IrAdapter {
    pub fn diagnostics(&self) -> &[CodeGenDiagnostic] {
        &self.diagnostics
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter()
            .any(|d| d.severity == DiagnosticSeverity::Error)
    }

    fn report_error(&mut self, error: CodeGenError) {
        self.diagnostics.push(error.into_diagnostic());
    }

    fn report_warning(&mut self, message: String, span: Option<Span>) {
        self.diagnostics.push(CodeGenDiagnostic {
            severity: DiagnosticSeverity::Warning,
            message,
            span,
            help: None,
        });
    }
}
```

---

## Extensibility & Future Work

### プラグイン機構（将来）

```rust
/// コード生成プラグイン
pub trait CodeGenPlugin: Send + Sync {
    /// プラグイン名
    fn name(&self) -> &'static str;

    /// 生成前フック
    fn pre_generate(&mut self, program: &mut JavaProgram) -> Result<(), PluginError>;

    /// 生成後フック
    fn post_generate(&mut self, source: &mut String) -> Result<(), PluginError>;
}

/// フォーマットプラグイン例
pub struct FormatterPlugin;

impl CodeGenPlugin for FormatterPlugin {
    fn name(&self) -> &'static str { "formatter" }

    fn pre_generate(&mut self, _program: &mut JavaProgram) -> Result<(), PluginError> {
        Ok(())
    }

    fn post_generate(&mut self, source: &mut String) -> Result<(), PluginError> {
        // google-java-formatを呼び出す等
        *source = format_with_external_tool(source)?;
        Ok(())
    }
}
```

### 並列生成（将来）

```rust
use rayon::prelude::*;

impl StrategyRegistry {
    /// 並列生成（宣言間に依存がない場合）
    pub fn dispatch_parallel(&self,
                            ctx: &RenderContext,
                            decls: &[JavaDeclaration]) -> Result<Vec<String>, CodeGenError> {
        decls.par_iter()
            .map(|decl| self.dispatch(ctx, decl))
            .collect()
    }
}
```

### カスタム戦略登録

```rust
impl StrategyRegistry {
    pub fn register(&mut self, strategy: Box<dyn CodeGenStrategy>) {
        self.strategies.push(strategy);
    }

    pub fn with_strategy(mut self, strategy: Box<dyn CodeGenStrategy>) -> Self {
        self.register(strategy);
        self
    }
}

// 使用例
let registry = StrategyRegistry::default_registry()
    .with_strategy(Box::new(CustomStrategy));
```

---

## Benefits Summary

### 1. **保守性の向上**
- ファイルサイズ: 2,813行 → 200-500行/戦略（**80%削減**）
- 責務の明確化: 各戦略が独立した関心事を担当
- テスト容易性: 戦略ごとの単体テスト

### 2. **拡張性の向上**
- 新構文追加: 新しい戦略を追加するだけ（既存コード変更不要）
- プラグイン機構: 外部フォーマッタ、リンター等の統合が容易
- 並列化: 戦略が独立しているため並列生成が可能

### 3. **型安全性の向上**
- Java Model: コンパイラが不正状態を検出
- 不変性: 中間表現が不変で副作用を排除
- 明示的変換: IR → Java Model → String の各ステップが型チェック可能

### 4. **パフォーマンスの向上**
- Rope: 大規模文字列の効率的な構築（O(log n)挿入）
- 将来の並列化: 戦略の並列実行が可能
- キャッシュ: 型レンダリング等の結果をキャッシュ可能

### 5. **診断品質の向上**
- 構造化エラー: CodeGenDiagnosticが詳細な情報を保持
- スパン情報: 正確なエラー位置の報告
- ヘルプメッセージ: ユーザーへの改善提案

---

## Risks & Mitigations

### リスク1: 移行期間の長期化
**軽減策**:
- 明確なマイルストーン（Phase 1-6、各1-2週間）
- 早期のゴールデンテスト実装で品質を継続確認
- フィーチャーフラグによる旧実装との併存

### リスク2: パフォーマンス低下
**軽減策**:
- Phase 5でのベンチマーク必須化
- 目標: 旧実装の±10%以内
- Rope実装による文字列結合最適化
- 将来の並列化余地を確保

### リスク3: テストカバレッジ不足
**軽減策**:
- ゴールデンテストの早期実装
- 既存1000+テストケースの完全移植
- TDDサイクルの徹底（Phase 1から）

### リスク4: 過度な抽象化
**軽減策**:
- 実証実験の知見を活かした実用的設計
- YAGNI原則の遵守（プラグイン等は将来）
- 定期的なコードレビューで複雑性を監視

---

## Success Criteria

### 機能要件
✅ 全既存テストケースがパス（1000+件）
✅ 全ゴールデンテストが一致
✅ Java 21/25の両ターゲットで正常動作
✅ jv言語のフルビルドが成功

### 非機能要件
✅ パフォーマンス: 旧実装の±10%以内
✅ メモリ: RenderContext < 200バイト
✅ コード量: 35%削減（12,400行 → 8,000行）
✅ ファイルサイズ: 最大500行/戦略

### 品質要件
✅ テストカバレッジ: ≥90%
✅ 診断品質: エラーメッセージに改善提案を含む
✅ ドキュメント: rustdocが完全
✅ CI/CD: 全パイプラインがグリーン

---

## Timeline

| Phase | 期間 | 成果物 | 検証基準 |
|-------|------|--------|----------|
| Phase 1: Foundation | 1-2週間 | コア抽象化、Java Model | 型モデルの単体テスト |
| Phase 2: IR Adapter | 1週間 | IR変換層 | 小規模IRのテスト |
| Phase 3: Strategy System | 2週間 | 基本戦略、レンダリング | ゴールデンテスト |
| Phase 4: Advanced Features | 2週間 | 式・文・Sample、Targeting | ターゲット別テスト |
| Phase 5: Integration & Testing | 1週間 | 全テスト移植、ベンチマーク | 全テストパス、パフォーマンス測定 |
| Phase 6: Production Ready | 1週間 | CLI統合、ドキュメント | フルビルド成功 |
| **合計** | **7-9週間** | 本番投入可能なコード生成器 | 上記Success Criteria |

---

## Appendix A: Comparison with Parser Architecture

| 観点 | jv_parser_rowan | jv_codegen_java_v2 |
|------|----------------|-------------------|
| **入力** | `Vec<Token>` | `IrProgram` |
| **出力** | `rowan::GreenNode` + `jv_ast::Program` | `JavaCompilationUnit` |
| **戦略** | `StatementStrategy` (10戦略) | `CodeGenStrategy` (6戦略) |
| **中間表現** | Rowan SyntaxTree | Java Model |
| **レンダリング** | N/A（ASTが出力） | CodeBuilder（Rope） |
| **ターゲット** | jv言語構文 | Java 21/25 |

**共通点**:
- Strategy Patternによる拡張性
- 責務の明確な分離（各戦略200-400行）
- 診断の構造化（Span情報保持）

**相違点**:
- Parser: イベント駆動（ParseEvent列）
- CodeGen: 型駆動（Java Model）
- Parser: エラー回復が重要
- CodeGen: 正確性が重要

---

## Appendix B: Code Size Projection

### 現状（jv_codegen_java）
```
declarations.rs:  1,348行
expressions.rs:   2,219行
sample.rs:        2,813行
statements.rs:      685行
mod.rs:             711行
types.rs:           220行
targeting.rs:        49行
formatting.rs:       97行
----------------------------
合計:            8,142行
```

### 新設計（jv_codegen_java_v2）
```
core/
  traits.rs:        150行
  context.rs:       200行
  error.rs:         150行
ir_adapter/
  transformer.rs:   800行
  diagnostics.rs:   100行
java_model/
  declarations.rs:  400行
  expressions.rs:   350行
  statements.rs:    300行
  types.rs:         250行
  modifiers.rs:     150行
strategies/
  mod.rs:           100行
  class.rs:         350行
  record.rs:        250行
  method.rs:        400行
  field.rs:         150行
  expression.rs:    500行
  sample.rs:        600行
rendering/
  builder.rs:       250行
  formatter.rs:     300行
  imports.rs:       150行
targeting/
  java21.rs:        200行
  java25.rs:        200行
lib.rs:             100行
----------------------------
合計:            6,100行
```

**削減率**: (8,142 - 6,100) / 8,142 = **25%削減**

（テスト・ドキュメント含めると総行数は増えるが、本体コードは大幅削減）

---

## Appendix C: Example Output

### 入力（IR）
```rust
IrStatement::ClassDeclaration {
    name: "User".to_string(),
    type_parameters: vec![],
    superclass: None,
    interfaces: vec![],
    fields: vec![
        IrStatement::FieldDeclaration {
            name: "id".to_string(),
            java_type: JavaType::Primitive(JavaPrimitive::Long),
            initializer: None,
            modifiers: IrModifiers {
                visibility: IrVisibility::Private,
                is_final: true,
                ..Default::default()
            },
            span: Span::default(),
        },
    ],
    methods: vec![
        IrStatement::MethodDeclaration {
            name: "getId".to_string(),
            parameters: vec![],
            return_type: Some(JavaType::Primitive(JavaPrimitive::Long)),
            body: vec![
                IrStatement::Return {
                    value: Some(IrExpression::FieldAccess {
                        receiver: Box::new(IrExpression::Identifier {
                            name: "this".to_string(),
                            span: Span::default(),
                        }),
                        field_name: "id".to_string(),
                        span: Span::default(),
                    }),
                    span: Span::default(),
                },
            ],
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                ..Default::default()
            },
            span: Span::default(),
        },
    ],
    nested_classes: vec![],
    modifiers: IrModifiers {
        visibility: IrVisibility::Public,
        ..Default::default()
    },
    span: Span::default(),
}
```

### 出力（Java）
```java
public class User {
    private final long id;

    public long getId() {
        return this.id;
    }
}
```

---

## Conclusion

本設計により、jv言語のJavaコード生成器は以下を達成する:

1. **保守性**: 各戦略200-400行、責務の明確な分離
2. **拡張性**: 新構文追加が既存コードに影響しない
3. **型安全性**: Java Modelによるコンパイル時検証
4. **パフォーマンス**: Rope実装、将来の並列化余地
5. **診断品質**: 構造化エラー、詳細なヘルプメッセージ

実証実験フェーズで得た知見を活かし、本格的なプロダクション開発に耐える堅牢なアーキテクチャを構築する。

**次のステップ**: Phase 1（Foundation）の実装開始。別ブランチ（`feature/codegen-v2`）での開発を推奨。
