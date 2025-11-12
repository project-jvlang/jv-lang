use std::fs;
use std::path::{Path, PathBuf};

use jv_checker::diagnostics::{DiagnosticSeverity, DiagnosticStrategy, EnhancedDiagnostic};

use crate::pipeline::BuildPlan;

const JV1003_CODE: &str = "JV1003";
const JV1003_TITLE: &str = "出力ディレクトリを準備できません";
const JV1003_HELP: &str =
    "jv.toml の project.output 設定と CLI フラグ (--output/--clean) を確認してください。";

pub struct OutputManager;

#[derive(Debug)]
pub struct PreparedOutput {
    plan: BuildPlan,
    base_dir: PathBuf,
    target_dir: PathBuf,
    clean_applied: bool,
}

impl OutputManager {
    pub fn prepare(plan: BuildPlan) -> Result<PreparedOutput, EnhancedDiagnostic> {
        let base_dir = plan.options.output_dir.clone();
        let target_label = format!("java{}", plan.build_config.target.as_str());
        let target_dir = base_dir.join(&target_label);
        if !plan.options.output_override {
            ensure_within_workspace(plan.root.root_dir(), &target_dir)?;
        }

        let mut clean_applied = false;

        if plan.options.clean && target_dir.exists() {
            fs::remove_dir_all(&target_dir)
                .map_err(|error| io_diagnostic(&target_dir, "をクリーン", error))?;
            clean_applied = true;
        }

        fs::create_dir_all(&target_dir)
            .map_err(|error| io_diagnostic(&target_dir, "を作成", error))?;

        let updated_plan = plan.with_output_dir(target_dir.clone());

        Ok(PreparedOutput {
            plan: updated_plan,
            base_dir,
            target_dir,
            clean_applied,
        })
    }
}

impl PreparedOutput {
    pub fn plan(&self) -> &BuildPlan {
        &self.plan
    }

    pub fn target_dir(&self) -> &Path {
        &self.target_dir
    }

    pub fn base_dir(&self) -> &Path {
        &self.base_dir
    }

    pub fn clean_applied(&self) -> bool {
        self.clean_applied
    }

    pub fn mark_success(&mut self) {
        // Outputs are preserved even when builds fail; method retained for API compatibility.
        let _ = self;
    }
}

fn ensure_within_workspace(root: &Path, candidate: &Path) -> Result<(), EnhancedDiagnostic> {
    if path_within(root, candidate) {
        Ok(())
    } else {
        Err(io_diagnostic(
            candidate,
            "がワークスペース外を指しています",
            std::io::Error::new(std::io::ErrorKind::Other, "workspace violation"),
        ))
    }
}

fn path_within(root: &Path, candidate: &Path) -> bool {
    let Some(root_canonical) = canonicalize_allowing_missing(root) else {
        return false;
    };
    let Some(candidate_canonical) = canonicalize_allowing_missing(candidate) else {
        return candidate.starts_with(root);
    };
    candidate_canonical.starts_with(&root_canonical)
}

fn canonicalize_allowing_missing(path: &Path) -> Option<PathBuf> {
    match fs::canonicalize(path) {
        Ok(resolved) => Some(resolved),
        Err(_) => {
            let parent = path.parent()?;
            let mut base = canonicalize_allowing_missing(parent)?;
            let component = path.file_name()?.to_owned();
            base.push(component);
            Some(base)
        }
    }
}

fn io_diagnostic(path: &Path, action: &str, error: std::io::Error) -> EnhancedDiagnostic {
    EnhancedDiagnostic {
        code: JV1003_CODE,
        title: JV1003_TITLE,
        message: format!("{} {} 中にエラーが発生しました: {}", path.display(), action, error),
        help: JV1003_HELP,
        severity: DiagnosticSeverity::Error,
        strategy: DiagnosticStrategy::Immediate,
        span: None,
        related_locations: Vec::new(),
        suggestions: Vec::new(),
        learning_hints: None,
    }
}
