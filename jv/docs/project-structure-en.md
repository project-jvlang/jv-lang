# jv Project Structure

This guide explains how to organize and structure jv projects for optimal development experience and maintainability.

## Table of Contents

1. [Basic Project Layout](#basic-project-layout)
2. [Configuration Files](#configuration-files)
3. [Source Organization](#source-organization)
4. [Testing Structure](#testing-structure)
5. [Build Artifacts](#build-artifacts)
6. [Best Practices](#best-practices)
7. [Multi-Module Projects](#multi-module-projects)
8. [Integration with Build Tools](#integration-with-build-tools)

## Basic Project Layout

### Simple Project

```
my-jv-project/
├── jv.toml                 # Project configuration
├── README.md               # Project documentation
├── .gitignore              # Git ignore patterns
├── src/                    # Source code
│   ├── main.jv            # Main entry point
│   ├── utils.jv           # Utility functions
│   └── models/            # Model classes
│       ├── User.jv
│       └── Product.jv
├── test/                   # Test files
│   ├── main_test.jv       # Tests for main.jv
│   ├── utils_test.jv      # Tests for utils.jv
│   └── models/            # Tests for models
│       ├── User_test.jv
│       └── Product_test.jv
├── resources/              # Static resources
│   ├── config.properties
│   └── data/
│       └── sample.json
└── out/                    # Build output (generated)
    ├── *.java             # Generated Java files
    ├── *.class            # Compiled bytecode
    └── *.jar              # Packaged JAR
```

### Library Project

```
my-jv-library/
├── jv.toml
├── README.md
├── LICENSE
├── src/
│   ├── lib.jv             # Library entry point
│   ├── core/              # Core functionality
│   │   ├── Parser.jv
│   │   └── Validator.jv
│   ├── utils/             # Utility modules
│   │   ├── StringUtils.jv
│   │   └── DateUtils.jv
│   └── examples/          # Usage examples
│       └── BasicExample.jv
├── test/
│   ├── core/
│   └── utils/
├── docs/                   # Library documentation
│   ├── api.md
│   └── examples.md
└── benchmarks/            # Performance tests
    └── ParsingBenchmark.jv
```

## Configuration Files

### `jv.toml`

The main project configuration file:

```toml
[project]
name = "my-project"
version = "1.0.0"
authors = ["Your Name <you@example.com>"]
description = "A jv project"
license = "MIT"
homepage = "https://github.com/user/my-project"
repository = "https://github.com/user/my-project"
keywords = ["utility", "parser", "tool"]
categories = ["command-line-utilities"]

[build]
jdk_version = "25"
optimization_level = 2
debug = false
target_dir = "out"
source_dir = "src"
test_dir = "test"
resource_dir = "resources"

# jv dependencies from jv registry
[dependencies]
math-utils = "1.2.0"
json-parser = { version = "2.1.0", optional = true }

# Maven dependencies
[dependencies.maven]
"org.apache.commons:commons-lang3" = "3.12.0"
"com.google.guava:guava" = "32.1.0-jre"

# Development dependencies (not included in production)
[dev-dependencies]
junit = "5.9.0"
mockito = "4.6.0"

[dev-dependencies.maven]
"org.junit.jupiter:junit-jupiter" = "5.9.0"

# Feature flags
[features]
default = ["json-support"]
json-support = ["json-parser"]
advanced-parsing = ["math-utils"]

# Toolchain configuration
[toolchain]
jdk = "temurin-25"
prefer_system = false
auto_download = true

# Code formatting
[format]
line_width = 100
indent_size = 4
trailing_comma = true
import_organization = true

# Static analysis
[check]
strict = true
null_safety = true
unused_warnings = true
performance_warnings = true
security_warnings = false

# Build profiles
[profiles.dev]
debug = true
optimization_level = 0

[profiles.release]
debug = false
optimization_level = 3
strip_debug_info = true

[profiles.test]
debug = true
optimization_level = 1
test_coverage = true
```

### `.gitignore`

Standard ignore patterns for jv projects:

```gitignore
# Build artifacts
/out/
/target/
*.class
*.jar

# IDE files
.vscode/
.idea/
*.iml
*.swp
*.swo

# OS files
.DS_Store
Thumbs.db

# Temporary files
*.tmp
*.temp
*~

# Logs
*.log

# jv specific
jv.lock     # Only if you want to gitignore (usually committed)
```

## Source Organization

### Package Structure

Organize source files by functionality:

```
src/
├── main.jv                 # Application entry point
├── app/                    # Application logic
│   ├── App.jv             # Main application class
│   ├── Config.jv          # Configuration handling
│   └── cli/               # Command-line interface
│       ├── Commands.jv
│       └── Arguments.jv
├── core/                   # Core business logic
│   ├── Parser.jv
│   ├── Processor.jv
│   └── Validator.jv
├── models/                 # Data models
│   ├── User.jv
│   ├── Product.jv
│   └── Order.jv
├── services/              # Service layer
│   ├── UserService.jv
│   ├── ProductService.jv
│   └── EmailService.jv
├── utils/                 # Utility functions
│   ├── StringUtils.jv
│   ├── DateUtils.jv
│   └── FileUtils.jv
└── extensions/            # Extension functions
    ├── StringExtensions.jv
    └── CollectionExtensions.jv
```

### Naming Conventions

#### Files and Directories
- Use **PascalCase** for class files: `UserService.jv`
- Use **camelCase** for directories: `userManagement/`
- Use **snake_case** for utility modules: `string_utils.jv`
- Group related functionality in directories

#### Code Organization
```jv
// File header with package and imports
package com.example.myproject

import java.util.List
import java.time.LocalDateTime
import com.example.utils.StringUtils

// Public API first
class UserService {
    fun createUser(name: String): User { ... }
    fun updateUser(id: String, name: String): User { ... }
}

// Private implementations last
private fun validateUserName(name: String): Boolean { ... }
private fun sanitizeInput(input: String): String { ... }
```

## Testing Structure

### Test Organization

Mirror the source structure in the test directory:

```
test/
├── unit/                   # Unit tests
│   ├── core/
│   │   ├── ParserTest.jv
│   │   └── ValidatorTest.jv
│   ├── services/
│   │   └── UserServiceTest.jv
│   └── utils/
│       └── StringUtilsTest.jv
├── integration/            # Integration tests
│   ├── ApiIntegrationTest.jv
│   └── DatabaseTest.jv
├── e2e/                   # End-to-end tests
│   ├── UserWorkflowTest.jv
│   └── OrderProcessingTest.jv
├── performance/           # Performance tests
│   └── ParsingBenchmark.jv
├── fixtures/              # Test data
│   ├── sample_users.json
│   └── test_config.properties
└── helpers/               # Test utilities
    └── TestHelpers.jv
```

### Test Naming Conventions

```jv
// Test class naming: [ClassName]Test
class UserServiceTest {
    
    // Test method naming: test[Action][Scenario]
    fun testCreateUserWithValidData() { ... }
    fun testCreateUserWithInvalidDataThrowsException() { ... }
    fun testUpdateUserNotFoundReturnsNull() { ... }
}
```

### Test Categories

Use comments or annotations to categorize tests:

```jv
class UserServiceTest {
    // @Category("unit")
    fun testCreateUserValidation() { ... }
    
    // @Category("integration") 
    fun testCreateUserWithDatabase() { ... }
    
    // @Category("performance")
    fun testCreateUserPerformance() { ... }
}
```

## Build Artifacts

### Generated Structure

The build process creates the following structure in `out/`:

```
out/
├── java/                   # Generated Java source
│   └── com/example/
│       ├── Main.java
│       ├── UserService.java
│       └── models/
│           └── User.java
├── classes/               # Compiled bytecode
│   └── com/example/
│       ├── Main.class
│       └── UserService.class
├── resources/             # Copied resources
│   ├── config.properties
│   └── data/
├── maps/                  # Source maps
│   ├── Main.java.map
│   └── UserService.java.map
├── docs/                  # Generated documentation
│   └── api/
├── my-project-1.0.0.jar   # Packaged JAR
└── reports/               # Analysis reports
    ├── test-results.xml
    ├── coverage.html
    └── static-analysis.json
```

### Build Configuration

Control build output with `jv.toml`:

```toml
[build]
# Output directories
target_dir = "out"
java_dir = "target"
classes_dir = "out/classes"
resources_dir = "out/resources"

# JAR packaging
jar_name = "${project.name}-${project.version}.jar"
main_class = "com.example.Main"
include_sources = true
include_docs = false

# Documentation generation
generate_docs = true
docs_dir = "out/docs"
```

## Best Practices

### Project Organization

1. **Consistent Structure**: Follow established patterns
2. **Logical Grouping**: Group related files together
3. **Clear Naming**: Use descriptive file and directory names
4. **Minimal Nesting**: Avoid deep directory hierarchies
5. **Separation of Concerns**: Separate business logic, data, and utilities

### File Organization

1. **Single Responsibility**: One main concept per file
2. **Reasonable Size**: Keep files under 500 lines when possible
3. **Clear Dependencies**: Minimize circular dependencies
4. **Public API First**: Put public interfaces at the top

### Configuration Management

1. **Environment-Specific Config**: Use profiles for different environments
2. **Secrets Management**: Don't commit secrets to version control
3. **Default Values**: Provide sensible defaults
4. **Documentation**: Comment configuration options

### Version Control

1. **Ignore Build Artifacts**: Don't commit generated files
2. **Small Commits**: Make focused, atomic commits
3. **Descriptive Messages**: Write clear commit messages
4. **Branch Strategy**: Use feature branches for development

## Multi-Module Projects

### Workspace Structure

For large projects, use multiple modules:

```
my-jv-workspace/
├── jv-workspace.toml       # Workspace configuration
├── common/                 # Shared utilities
│   ├── jv.toml
│   └── src/
├── api/                    # REST API module
│   ├── jv.toml
│   └── src/
├── web/                    # Web interface
│   ├── jv.toml
│   └── src/
├── cli/                    # Command-line tool
│   ├── jv.toml
│   └── src/
└── docs/                   # Shared documentation
```

### Workspace Configuration

`jv-workspace.toml`:
```toml
[workspace]
members = [
    "common",
    "api", 
    "web",
    "cli"
]

# Shared dependencies
[workspace.dependencies]
json-parser = "2.1.0"
logging = "1.5.0"

# Shared configuration
[workspace.profile.dev]
debug = true
optimization_level = 0

[workspace.profile.release]
debug = false
optimization_level = 3
```

Individual module `jv.toml`:
```toml
[project]
name = "my-project-api"
version = "1.0.0"

[dependencies]
# Use workspace version
json-parser = { workspace = true }
# Module-specific dependency
http-server = "3.2.0"
# Inter-module dependency
common = { path = "../common" }
```

### Module Dependencies

```jv
// In api/src/main.jv
import common.utils.StringUtils  // From common module
import java.util.List            // From Java standard library
import external.JsonParser       // From external dependency

class ApiServer {
    fun start() {
        val port = StringUtils.parsePort("8080")  // Using common module
        // ...
    }
}
```

## Integration with Build Tools

### Gradle Integration

`build.gradle.kts`:
```kotlin
plugins {
    id("jv-gradle-plugin") version "1.0.0"
    id("java")
    id("application")
}

jv {
    jdkVersion = "25"
    optimizationLevel = 2
    generateSourceMaps = true
}

dependencies {
    implementation("org.apache.commons:commons-lang3:3.12.0")
    testImplementation("org.junit.jupiter:junit-jupiter:5.9.0")
}

application {
    mainClass.set("com.example.MainKt")
}
```

### Maven Integration

`pom.xml`:
```xml
<build>
    <plugins>
        <plugin>
            <groupId>org.jv-lang</groupId>
            <artifactId>jv-maven-plugin</artifactId>
            <version>1.0.0</version>
            <configuration>
                <jdkVersion>25</jdkVersion>
                <optimizationLevel>2</optimizationLevel>
            </configuration>
            <executions>
                <execution>
                    <goals>
                        <goal>compile</goal>
                    </goals>
                </execution>
            </executions>
        </plugin>
    </plugins>
</build>
```

### IDE Integration

#### VS Code

`.vscode/settings.json`:
```json
{
    "jv.compiler.path": "./target/release/jv",
    "jv.build.automaticBuild": true,
    "files.associations": {
        "*.jv": "jv"
    }
}
```

#### IntelliJ IDEA

Configure the jv plugin for:
- Syntax highlighting
- Error detection
- Auto-completion
- Refactoring support
- Debugging integration

### Continuous Integration

`.github/workflows/ci.yml`:
```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup Java 25
      uses: actions/setup-java@v3
      with:
        java-version: '25'
        distribution: 'temurin'
    
    - name: Install jv
      run: |
        curl -sSL https://install.jv-lang.org | sh
        echo "$HOME/.jv/bin" >> $GITHUB_PATH
    
    - name: Build
      run: jv build
    
    - name: Test
      run: jv test
    
    - name: Check formatting
      run: jv fmt --check
```

This structure provides a solid foundation for jv projects of any size while maintaining consistency and best practices.
