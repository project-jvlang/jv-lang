# AST→IR Performance Baseline Guide

This guide centralises the operating procedures and targets for the AST→IR performance harness. It helps every developer quickly validate the 3 second / 100 MB budget and refresh the baseline when necessary.

## Baseline Snapshot

| Metric | Budget | Notes |
| --- | --- | --- |
| Cold total (`cold_total_ms`) | ≤ 3,000 ms | Wall-clock time for the first iteration. CLI/CI fail when this budget is exceeded. |
| Warm average (`warm_average_ms`) | ≤ 3,000 ms | Average time after the allocator pool is warm. Reported only when warm sessions exist. |
| Pool reuse ratio | ≥ 0.90 | TransformPools reuse ratio. Review the session and warm session counters alongside the ratio. |
| Peak RSS | ≤ 100 MB | Reported only when the platform exposes RSS information. |

The latest measurements are stored in `target/perf-reports/ast-ir-phase1.json` under the `summary` and `checks` fields.

## Harness Workflow

### Test Harness

```bash
cargo test --package jv_ir --lib
cargo test --package jv_ir -- --ignored perf_phase1
```

- The first command ensures regular `jv_ir` tests still pass.
- The second command runs the phase 1 performance fixture and emits the JSON report.

### CLI Profile

```bash
cargo run --bin jv_cli -- build --perf --quiet
```

- Performs the same profiling as `jv build --perf` and surfaces detailed diagnostics through `PerfCapture` on failure.
- Use the CLI locally as needed; CI runs the same sequence via the `perf_phase1` workflow.

## Baseline Refresh Flow

1. Run both commands above to generate a fresh report.
2. Open `target/perf-reports/ast-ir-phase1.json` and confirm that `timestamp_ms` reflects the current run.
3. Ensure every entry in `checks` is `true` and that the overall `pass` flag is `true`.
4. If values have shifted, update the snapshot table in this guide and share the latest cold / warm / reuse / RSS numbers in your PR description.
5. When CI reports "older than 30 days", regenerate the report. If it still fails to refresh, delete `target/perf-reports` and rerun the harness.

## Triage Guide

- `Performance report not found`: The harness likely failed mid-run. Re-execute `cargo test --package jv_ir -- --ignored perf_phase1` and inspect the console output.
- `pass == false`: Investigate which `checks` entry failed and verify the 3,000 ms / 100 MB / 0.90 budgets. Compare the `runs` entries to spot spikes.
- `Performance report is older than 30 days`: The report exists but is stale. Regenerate it; if the timestamp still does not refresh, run `cargo clean -p jv_ir` before retrying.
