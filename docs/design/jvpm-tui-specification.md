# jvpm TUI Specification

`jvpm` is the Maven-specific entrypoint for the jv ecosystem. When launched as the `jvpm`
binary it operates in **wrapper mode** and implements a lightweight Maven dependency helper that
keeps `pom.xml`, `settings.xml`, and `jv.lock` synchronized without depending on `jv.toml`.

## Wrapper mode behavior

- **Mode detection** runs in `WrapperContext::detect`. If `jv.toml` exists it aborts with
  `NativeProjectDetected`; if both `jv.toml` and `pom.xml` exist it aborts with
  `MixedProjectConfiguration`. Only the scenario “`pom.xml` exists but `jv.toml` does not” boots
  the wrapper pipeline.
- When neither manifest exists, `WrapperContext` generates a minimal `pom.xml`/`settings.xml`
  pair, writes them to the project root, and flags `template_generated`.
- Settings are always refreshed (even on template creation) so `settings.xml` and
  `.jv/repository` are available before resolver work starts.
- `--strategy <pubgrub|breadth-first|maven-compat>` も wrapper モードで受け付けられ、選択されたストラテジ名とアルゴリズムが `jv.lock` に記録されることで結果の再現性を保ちます。

## Supported commands

- `add` – resolves candidate dependencies via `WrapperPipeline`, updates `jv.lock`, and
-   invokes `WrapperIntegrationStrategy` (`wrapper-default`) to write `pom.xml` + `settings.xml`.
-   `WrapperUpdateSummary` で `pom.xml`/`settings.xml`/`jv.lock` の更新フラグが出力され、コミット対象ファイルを確認できます。
- `remove` – removes dependencies from the manifest and performs the same lockfile /
-   Maven artifact re-synchronization.
-   更新結果も `WrapperUpdateSummary` で報告され、意図しないファイル変更がないか確認できます。
- `resolver`/`repo` commands are **blocked** during wrapper mode; users are prompted to run
-  the core `jv` CLI instead, and the error message explicitly instructs the user to rerun
-  the desired subcommand via `jv` (e.g., `jv repo`).
- All other arguments (forwarded to Maven) are proxied to the resolved Maven binary.

## jv.lock role

- Each wrapper run writes `jv.lock` through `WrapperLockfileWriter`. When the manifest is
  missing the lockfile is generated directly from `ResolvedDependencies` and includes the
  effective strategy name and algorithm label.
- The CLI prints which artifacts were touched (`pom.xml`, `settings.xml`, `jv.lock`) based
  on `WrapperUpdateSummary`, guiding users to commit the Maven artifacts they care about.
- `jv.lock` stabilizes dependency order (`scope` then `name`) even if resolver output is
  nondeterministic, ensuring incremental updates are merge-friendly.

## Manual operations

- Use `jvpm` for Maven projects that lack `jv.toml`. For native jv projects use `jv pm`.
- If Maven or JV manifests coexist, `jvpm` errors with a clear message describing which
  CLI to run for each project type.

## CLI ヘルプとドキュメント

- `jvpm --help` はラッパーモードの概要、同期対象ファイル、`--strategy <pubgrub|breadth-first|maven-compat>` の使い方を含み、実行前に期待値を示します。
- この仕様書では `WrapperUpdateSummary` や `WrapperIntegrationStrategy` によるファイル同期、`jv.lock` への戦略記録を説明し、CLI ヘルプと内容が一致するようにしています。
