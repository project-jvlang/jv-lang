use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use jv_cli::pipeline::compute_script_main_class;
use tempfile::tempdir;

const ADDITIONAL_FLAGS_SCRIPT: &str =
    include_str!("../../../tests/fixtures/regex/command_additional_flags.jv");

fn compiled_main_class(src: &Path) -> (PathBuf, String) {
    let cli_path = PathBuf::from(env!("CARGO_BIN_EXE_jv"));
    let workspace = tempdir().expect("一時ディレクトリ作成");
    #[allow(deprecated)]
    let workspace_path = workspace.into_path();
    let main_path = workspace_path.join("script.jv");
    fs::copy(src, &main_path).expect("テストスクリプトをコピー");
    let output_root = workspace_path.join("out");
    let main_class = compute_script_main_class("", &main_path);

    let status = Command::new(&cli_path)
        .arg("build")
        .arg(&main_path)
        .arg("-o")
        .arg(&output_root)
        .status()
        .expect("jv build を実行");
    assert!(status.success(), "jv build が失敗しました: {:?}", status);

    let candidates = [
        output_root.join("java25"),
        output_root.join("java21"),
        output_root.join("java"),
        output_root.clone(),
    ];

    let output_dir = candidates
        .into_iter()
        .find(|path| path.exists())
        .unwrap_or(output_root);

    (output_dir, main_class)
}

fn has_javac() -> bool {
    Command::new("javac")
        .arg("-version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
}

fn has_java_runtime() -> bool {
    Command::new("java")
        .arg("-version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
}

#[test]
fn regex_command_sequence_should_not_scope_error() {
    let cli_path = PathBuf::from(env!("CARGO_BIN_EXE_jv"));

    let workspace = tempdir().expect("一時ディレクトリ作成");
    let script_path = workspace.path().join("regex_command_sequence.jv");
    let source = r#"
val auditLog = """
USER=Akira ACTION=login
USER=Tom ACTION=LOGIN
""".trim()

val allLoginUpper = m/auditLog/'^USER=\w+\sACTION=LOGIN$'/

val hasResult = [match]/auditLog/'ACTION=RESULT'/

println(allLoginUpper)
println(hasResult)
"#;
    fs::write(&script_path, source).expect("再現用スクリプトを書き込み");

    let output = Command::new(&cli_path)
        .arg("check")
        .arg(&script_path)
        .output()
        .expect("jv check を実行");

    assert!(
        output.status.success(),
        "jv check が失敗しました。stdout: {} stderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn regex_command_modes_match_expected_java_execution() {
    if !has_javac() || !has_java_runtime() {
        eprintln!("javac もしくは java が利用できないためテストをスキップします");
        return;
    }

    let workspace = tempdir().expect("一時ディレクトリ作成");
    let script_path = workspace.path().join("regex_command_modes.jv");
    let source = r#"
val auditLog = """
USER=Akira ACTION=login ip=192.168.0.10
USER=Tom ACTION=LOGIN ip=10.0.0.5
USER=Chie ACTION=RESULT ip=172.16.0.77
""".trim()

val maskedAll = a/auditLog/'ip=\d{1,3}(\.\d{1,3}){3}'/'ip=***.***.***.***'/ims

val firstTagged = f/auditLog/'USER=\w+'/{ match ->
    when (match.group()) {
        null -> ""
        else -> "[FIRST-${match.group()}]"
    }
}/

val canonicalLog = /auditLog/'ACTION=LOGIN'/'ACTION=login'/i

val containsAkira = m/auditLog/'.*USER=Akira.*'/

val hasResult = [match]/auditLog/'ACTION=RESULT'/

val splitLines = s/auditLog/'\n'/

val iteratedPairs = i/auditLog/'USER=(\w+)\sACTION=(\w+)'/{ match ->
    "${match.group(1)}:${match.group(2)}"
}/

println("=== replaceAll ===")
println(maskedAll)

println("=== replaceFirst ===")
println(firstTagged)

println("=== implicit replace ===")
println(canonicalLog)

println("=== match implicit ===")
println(containsAkira)

println("=== match explicit ===")
println(hasResult)

println("=== split ===")
for (part in splitLines) {
    println(" • ${part}")
}

println("=== iterate lambda ===")
println(iteratedPairs)
"#;

    fs::write(&script_path, source).expect("正規表現モード網羅スクリプトを書き込み");

    let (output_dir, main_class) = compiled_main_class(&script_path);

    let run_output = Command::new("java")
        .arg("-cp")
        .arg(&output_dir)
        .arg(&main_class)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("java 実行");
    assert!(
        run_output.status.success(),
        "Java 実行が失敗しました: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let stdout = String::from_utf8(run_output.stdout).expect("標準出力はUTF-8");
    let expected = "\
=== replaceAll ===\n\
USER=Akira ACTION=login ip=***.***.***.***\n\
USER=Tom ACTION=LOGIN ip=***.***.***.***\n\
USER=Chie ACTION=RESULT ip=***.***.***.***\n\
=== replaceFirst ===\n\
[FIRST-USER=Akira] ACTION=login ip=192.168.0.10\n\
USER=Tom ACTION=LOGIN ip=10.0.0.5\n\
USER=Chie ACTION=RESULT ip=172.16.0.77\n\
=== implicit replace ===\n\
USER=Akira ACTION=login ip=192.168.0.10\n\
USER=Tom ACTION=login ip=10.0.0.5\n\
USER=Chie ACTION=RESULT ip=172.16.0.77\n\
=== match implicit ===\n\
false\n\
=== match explicit ===\n\
false\n\
=== split ===\n\
 • USER=Akira ACTION=login ip=192.168.0.10\n\
 • USER=Tom ACTION=LOGIN ip=10.0.0.5\n\
 • USER=Chie ACTION=RESULT ip=172.16.0.77\n\
=== iterate lambda ===\n\
Akira:login ip=192.168.0.10\n\
Tom:LOGIN ip=10.0.0.5\n\
Chie:RESULT ip=172.16.0.77\n";

    let actual_lines: Vec<_> = stdout.lines().map(|line| line.trim()).collect();
    let expected_lines: Vec<_> = expected.lines().map(|line| line.trim()).collect();
    assert_eq!(
        actual_lines, expected_lines,
        "正規表現モード網羅テストの出力が想定と一致しません"
    );
}

#[test]
fn regex_command_additional_flags_produce_expected_output() {
    if !has_javac() || !has_java_runtime() {
        eprintln!("javac もしくは java が利用できないためテストをスキップします");
        return;
    }

    let mut home_candidate = std::env::var("JAVA25_HOME")
        .ok()
        .or_else(|| std::env::var("JAVA_HOME").ok());
    let mut inferred_home = None;

    let ensure_home = |path: &Path| {
        let java_bin = path.join("bin").join("java");
        java_bin.exists()
    };

    if let Some(ref home) = home_candidate {
        if !ensure_home(Path::new(home)) {
            home_candidate = None;
        }
    }

    if home_candidate.is_none() {
        let javac_path = Command::new("which")
            .arg("javac")
            .output()
            .ok()
            .and_then(|out| {
                if out.status.success() {
                    let text = String::from_utf8_lossy(&out.stdout).trim().to_string();
                    (!text.is_empty()).then_some(PathBuf::from(text))
                } else {
                    None
                }
            });

        if let Some(path) = javac_path {
            if let Some(bin_dir) = path.parent() {
                let home = bin_dir.parent().unwrap_or(bin_dir);
                if ensure_home(home) {
                    inferred_home = Some(home.to_path_buf());
                    home_candidate = Some(home.display().to_string());
                }
            }
        }
    }

    let Some(home_str) = home_candidate else {
        eprintln!("利用可能な JAVA_HOME が見つからないためテストをスキップします");
        return;
    };

    if !ensure_home(Path::new(&home_str)) {
        eprintln!(
            "設定済みの JAVA_HOME ({}) に java 実行可能ファイルが存在しないためテストをスキップします",
            home_str
        );
        return;
    }

    if let Some(home) = inferred_home {
        unsafe {
            std::env::set_var("JAVA_HOME", &home);
            std::env::set_var("JAVA25_HOME", &home);
        }
    }

    let workspace = tempdir().expect("一時ディレクトリ作成");
    let script_path = workspace.path().join("regex_command_flags.jv");
    fs::write(&script_path, ADDITIONAL_FLAGS_SCRIPT).expect("追加フラグスクリプトを書き込み");

    let (output_dir, main_class) = compiled_main_class(&script_path);

    let run_output = Command::new("java")
        .arg("-cp")
        .arg(&output_dir)
        .arg(&main_class)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("java 実行");

    assert!(
        run_output.status.success(),
        "Java 実行が失敗しました: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let stdout = String::from_utf8(run_output.stdout).expect("標準出力はUTF-8");
    let expected = "unicode-case ascii-changed=true unicode-changed=true\n\
unix-lines baseline-changed=true unix-changed=false\n\
comments baseline-changed=false flag-changed=true\n\
literal regex-changed=true flag-changed=false exact-changed=true\n\
canon-eq baseline-changed=true flag-changed=true\n";

    assert_eq!(stdout, expected, "追加フラグの出力が期待値と一致しません");
}

#[test]
fn concise_regex_sample_runs_end_to_end() {
    if !has_javac() || !has_java_runtime() {
        eprintln!("javac もしくは java が利用できないためテストをスキップします");
        return;
    }

    let workspace = tempdir().expect("一時ディレクトリ作成");
    let script_path = workspace.path().join("concise_regex_sample.jv");
    let source = r#"
val csv = "alice@example.com,bob@example.org,carol@example.net"

val replacedAll = a/csv/'@example\.(com|org|net)'/'@example.dev'/
val firstUpper = f/csv/'\w+'/$ { it.group().toUpperCase() }/
val onlyEmails = m/csv/'^([\w.]+@[\w.]+,?)+$'/
val parts = s/csv/'[,]\s*'/
val iteratedPairs = i/csv/'([\w.]+)@([\w.]+)'/{ match ->
    "${match.group(1)} -> ${match.group(2)}"
}/

println("replaceAll結果: ${replacedAll}")
println("先頭置換: ${firstUpper}")
println("全体マッチ: ${onlyEmails}")
println("split結果:")
for (part in parts) {
    println("  ${part}")
}
println("iterate結果:")
println(("  ${iteratedPairs}").replace(",", "\n  "))
"#;
    fs::write(&script_path, source).expect("簡潔サンプルスクリプトを書き込み");

    let (output_dir, main_class) = compiled_main_class(&script_path);

    let class_file = output_dir.join(format!("{}.class", main_class));
    assert!(
        class_file.exists(),
        "{} が生成されていません",
        class_file.display()
    );

    let run_output = Command::new("java")
        .arg("-cp")
        .arg(&output_dir)
        .arg(&main_class)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("java 実行");
    assert!(
        run_output.status.success(),
        "Java 実行が失敗しました: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let stdout = String::from_utf8(run_output.stdout).expect("標準出力はUTF-8");
    let expected = "\
replaceAll結果: alice@example.dev,bob@example.dev,carol@example.dev
先頭置換: ALICE@example.com,bob@example.org,carol@example.net
全体マッチ: true
split結果:
  alice@example.com
  bob@example.org
  carol@example.net
iterate結果:
  alice -> example.com
  bob -> example.org
  carol -> example.net";

    assert_eq!(
        stdout.trim(),
        expected.trim(),
        "サンプル実行結果が想定と一致しません"
    );
}

#[test]
fn regex_command_example_generates_supplier_guarded_java() {
    if !has_javac() || !has_java_runtime() {
        eprintln!("javac もしくは java が利用できないためテストをスキップします");
        return;
    }

    let example_src = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../examples/regex-command.jv");
    assert!(
        example_src.exists(),
        "examples/regex-command.jv が存在する必要があります"
    );

    let (output_dir, main_class) = compiled_main_class(&example_src);

    let class_path = output_dir.join(format!("{}.class", main_class));
    assert!(
        class_path.exists(),
        "{} が生成されていません",
        class_path.display()
    );

    let java_path = output_dir.join(format!("{}.java", main_class));
    assert!(
        java_path.exists(),
        "{} が生成されていません",
        java_path.display()
    );

    let java_source = fs::read_to_string(&java_path).expect("生成された Java ソースを読み込む");
    assert!(
        java_source.contains("java.util.function.Supplier"),
        "生成された Java は Supplier ベースのガードを含む必要があります:\n{java_source}"
    );
    assert!(
        java_source.contains("if (__jvRegexSubject_"),
        "生成された Java は if ブロックによる instanceof ガードを使用する必要があります:\n{java_source}"
    );
    assert!(
        !java_source.contains("? __jvRegexSubject_"),
        "旧来の三項演算子ベースの instanceof ガードが残っています:\n{java_source}"
    );
}
