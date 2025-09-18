# Getting Started with jv

This guide will help you install jv and write your first jv program.

## Installation

### Prerequisites

- **Java 25+**: jv targets Java 25 LTS
- **Rust toolchain** (for building from source)

### Install from Source

Currently, jv must be built from source:

```bash
# Clone the repository
git clone https://github.com/project-jvlang/jv-lang.git
cd jv-lang

# Build the compiler
cargo build --release

# The jv binary will be at target/release/jv
./target/release/jv --help
```

### Verify Installation

```bash
jv version
# Output: jv 0.1.0 - Java Sugar Language compiler
```

## Your First jv Program

### 1. Initialize a Project

```bash
mkdir my-jv-project
cd my-jv-project
jv init
```

This creates:
```
my-jv-project/
├── jv.toml          # Project configuration
└── src/
    └── main.jv      # Main source file
```

### 2. Write Some Code

Edit `src/main.jv`:

```jv
val greeting = "Hello, jv!"

fun main() {
    println(greeting)
    
    val numbers = listOf(1, 2, 3, 4, 5)
    val doubled = numbers.map { it * 2 }
    
    println("Original: $numbers")
    println("Doubled: $doubled")
}
```

### 3. Build and Run

```bash
# Build the project (generates Java code and compiles)
jv build

# Run the compiled program
jv run
```

Output:
```
Hello, jv!
Original: [1, 2, 3, 4, 5]
Doubled: [2, 4, 6, 8, 10]
```

## Understanding the Build Process

When you run `jv build`, the following happens:

1. **Lexing**: jv source is tokenized
2. **Parsing**: Tokens are parsed into an Abstract Syntax Tree (AST)
3. **IR Transformation**: AST is converted to Intermediate Representation (IR) 
4. **Java Generation**: IR is transformed into readable Java 25 source code
5. **Java Compilation**: Generated Java is compiled with `javac`

You can see the generated Java code in the `out/` directory:

```bash
# View generated Java
cat out/Main.java
```

## Next Steps

- Read the [Language Guide](language-guide-en.md) to learn jv syntax
- Explore [CLI Reference](cli-reference-en.md) for all available commands
- Check out the [examples directory](../examples/) for more complex programs
- Learn about [Project Structure](project-structure-en.md) for larger projects

## Getting Help

- **Documentation**: Browse the docs in this directory
- **Issues**: Report bugs on [GitHub Issues](https://github.com/project-jvlang/jv-lang/issues)
- **Discussions**: Ask questions in [GitHub Discussions](https://github.com/project-jvlang/jv-lang/discussions)