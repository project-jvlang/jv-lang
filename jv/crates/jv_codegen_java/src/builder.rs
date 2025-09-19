use crate::config::JavaCodeGenConfig;

/// Utility that incrementally constructs Java source code with indentation handling.
#[derive(Debug, Default, Clone)]
pub struct JavaSourceBuilder {
    content: String,
    indent_level: usize,
    indent: String,
}

impl JavaSourceBuilder {
    pub fn new(indent: String) -> Self {
        Self {
            content: String::new(),
            indent_level: 0,
            indent,
        }
    }

    pub fn push_line(&mut self, line: &str) {
        self.push_indent();
        self.content.push_str(line);
        self.content.push('\n');
    }

    pub fn push(&mut self, text: &str) {
        self.content.push_str(text);
    }

    pub fn push_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.content.push_str(&self.indent);
        }
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    pub fn build(self) -> String {
        self.content
    }

    pub fn is_empty(&self) -> bool {
        self.content.trim().is_empty()
    }
}

/// Fully-rendered Java compilation unit produced by the generator.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct JavaCompilationUnit {
    pub package_declaration: Option<String>,
    pub imports: Vec<String>,
    pub type_declarations: Vec<String>,
}

impl JavaCompilationUnit {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn to_source(&self, config: &JavaCodeGenConfig) -> String {
        let mut builder = JavaSourceBuilder::new(config.indent.clone());

        if let Some(package) = &self.package_declaration {
            builder.push_line(&format!("package {};", package));
            builder.push_line("");
        }

        if !self.imports.is_empty() {
            for import in &self.imports {
                builder.push_line(&format!("import {};", import));
            }
            builder.push_line("");
        }

        for (index, declaration) in self.type_declarations.iter().enumerate() {
            if index > 0 {
                builder.push_line("");
            }
            builder.push(declaration);
        }

        builder.build()
    }
}
