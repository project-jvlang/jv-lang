use std::fs;
use std::path::Path;
use std::process::Command;

use jv_cli::cli::{Cli, Commands};
use jv_lexer::Lexer;
use jv_parser::Parser;
use jv_ast::Program;
use jv_ir::transform_ast_to_ir;
use jv_codegen_java::JavaCodeGenerator;
use jv_build::{javac_compile, JdkManager};

/// Integration tests for the complete jv compilation pipeline
/// Tests the entire flow: .jv source -> Java source -> .class file -> execution

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_hello_world_compilation() {
        let jv_source = r#"
            fun main() {
                println("Hello, World!")
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        assert!(java_code.contains("public static void main"));
        assert!(java_code.contains("System.out.println"));
        assert!(java_code.contains("Hello, World!"));
    }

    #[test]
    fn test_variable_declaration_compilation() {
        let jv_source = r#"
            fun main() {
                val name = "jv"
                var count = 42
                println("Language: " + name)
                println("Count: " + count)
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        assert!(java_code.contains("final String name"));
        assert!(java_code.contains("int count"));
    }

    #[test]
    fn test_null_safety_compilation() {
        let jv_source = r#"
            fun main() {
                val user: String? = null
                val length = user?.length ?: 0
                println(length)
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        // Should generate null-safe access pattern
        assert!(java_code.contains("user != null"));
    }

    #[test]
    fn test_when_expression_compilation() {
        let jv_source = r#"
            fun main() {
                val x = 1
                val result = when (x) {
                    0 -> "zero"
                    1 -> "one"
                    else -> "other"
                }
                println(result)
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        // Should generate Java switch expression or if-else chain
        assert!(java_code.contains("switch") || java_code.contains("if"));
    }

    #[test]
    fn test_data_class_compilation() {
        let jv_source = r#"
            data class User(val name: String, val age: Int)
            
            fun main() {
                val user = User("Alice", 30)
                println(user.name)
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        // Should generate Java record for immutable data class
        assert!(java_code.contains("record User") || java_code.contains("class User"));
    }

    #[test]
    fn test_string_interpolation_compilation() {
        let jv_source = r#"
            fun main() {
                val name = "jv"
                val version = 1
                println("Welcome to ${name} version ${version}!")
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        // Should generate String.format or string concatenation
        assert!(java_code.contains("String.format") || java_code.contains("+"));
    }

    #[test]
    fn test_function_compilation() {
        let jv_source = r#"
            fun add(a: Int, b: Int): Int {
                return a + b
            }
            
            fun main() {
                val result = add(5, 3)
                println(result)
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        assert!(java_code.contains("public static int add"));
        assert!(java_code.contains("return a + b"));
    }

    #[test]
    fn test_virtual_thread_compilation() {
        let jv_source = r#"
            fun main() {
                spawn {
                    println("Running in virtual thread")
                }
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        // Should generate virtual thread creation
        assert!(java_code.contains("Thread.ofVirtual") || java_code.contains("VirtualThread"));
    }

    #[test]
    fn test_async_await_compilation() {
        let jv_source = r#"
            fun main() {
                val future = async {
                    "Hello from future"
                }
                val result = future.await()
                println(result)
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        // Should generate CompletableFuture usage
        assert!(java_code.contains("CompletableFuture"));
    }

    #[test]
    fn test_extension_function_compilation() {
        let jv_source = r#"
            fun String.reversed(): String {
                return StringBuilder(this).reverse().toString()
            }
            
            fun main() {
                val text = "hello"
                println(text.reversed())
            }
        "#;
        
        let result = compile_jv_to_java(jv_source);
        assert!(result.is_ok());
        
        let java_code = result.unwrap();
        // Should generate static method for extension function
        assert!(java_code.contains("public static String reversed"));
    }

    #[test]
    fn test_compilation_error_handling() {
        let invalid_jv_source = r#"
            fun main( {
                // Missing closing parenthesis - should fail to parse
                println("This won't compile")
        "#;
        
        let result = compile_jv_to_java(invalid_jv_source);
        assert!(result.is_err());
    }

    // End-to-end test that actually compiles and runs Java code
    #[test]
    #[ignore] // Only run with JDK available
    fn test_end_to_end_execution() {
        let jv_source = r#"
            fun main() {
                println("Hello from jv!")
            }
        "#;
        
        // Compile jv to Java
        let java_code = compile_jv_to_java(jv_source).expect("Failed to compile jv to Java");
        
        // Write Java code to temporary file
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let java_file = temp_dir.path().join("Main.java");
        fs::write(&java_file, java_code).expect("Failed to write Java file");
        
        // Compile Java to bytecode using javac
        let jdk_manager = JdkManager::new();
        let compilation_result = javac_compile(&java_file, temp_dir.path(), &jdk_manager);
        assert!(compilation_result.is_ok());
        
        // Execute the compiled Java class
        let output = Command::new("java")
            .arg("-cp")
            .arg(temp_dir.path())
            .arg("Main")
            .output()
            .expect("Failed to execute Java program");
        
        assert!(output.status.success());
        let stdout = String::from_utf8(output.stdout).expect("Invalid UTF-8 in output");
        assert!(stdout.contains("Hello from jv!"));
    }

    // Performance benchmark test
    #[test]
    #[ignore] // Only run in benchmark mode
    fn benchmark_compilation_speed() {
        let jv_source = r#"
            data class Point(val x: Double, val y: Double)
            
            fun distance(p1: Point, p2: Point): Double {
                val dx = p1.x - p2.x
                val dy = p1.y - p2.y
                return Math.sqrt(dx * dx + dy * dy)
            }
            
            fun main() {
                val points = arrayOf(
                    Point(0.0, 0.0),
                    Point(3.0, 4.0),
                    Point(1.0, 1.0)
                )
                
                for (i in 0 until points.size - 1) {
                    val dist = distance(points[i], points[i + 1])
                    println("Distance: ${dist}")
                }
            }
        "#;
        
        let start = std::time::Instant::now();
        let result = compile_jv_to_java(jv_source);
        let duration = start.elapsed();
        
        assert!(result.is_ok());
        // Should compile reasonably fast
        assert!(duration < std::time::Duration::from_secs(1));
        
        println!("Compilation took: {:?}", duration);
    }
}

/// Helper function to compile jv source to Java code
fn compile_jv_to_java(source: &str) -> Result<String, Box<dyn std::error::Error>> {
    // Lexical analysis
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize()?;
    
    // Parsing
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    
    // IR transformation
    let ir_program = transform_ast_to_ir(ast)?;
    
    // Java code generation
    let mut codegen = JavaCodeGenerator::new();
    let java_code = codegen.generate(&ir_program)?;
    
    Ok(java_code)
}

/// Test fixtures for golden file testing
#[cfg(test)]
mod golden_tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn test_golden_files() {
        let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
        
        if !fixtures_dir.exists() {
            // Skip if fixtures directory doesn't exist
            return;
        }
        
        for entry in fs::read_dir(fixtures_dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            
            if path.extension().and_then(|s| s.to_str()) == Some("jv") {
                let jv_source = fs::read_to_string(&path).unwrap();
                let java_file = path.with_extension("java");
                
                if java_file.exists() {
                    let expected_java = fs::read_to_string(&java_file).unwrap();
                    let actual_java = compile_jv_to_java(&jv_source).unwrap();
                    
                    // Normalize whitespace for comparison
                    let normalized_expected = normalize_java_code(&expected_java);
                    let normalized_actual = normalize_java_code(&actual_java);
                    
                    assert_eq!(normalized_actual, normalized_expected, 
                              "Generated Java code doesn't match golden file for {:?}", path);
                }
            }
        }
    }
    
    fn normalize_java_code(code: &str) -> String {
        // Remove extra whitespace and normalize formatting for comparison
        code.lines()
            .map(|line| line.trim())
            .filter(|line| !line.is_empty())
            .collect::<Vec<_>>()
            .join("\n")
    }
}