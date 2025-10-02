#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd "${SCRIPT_DIR}/.." && pwd)
OUTPUT_ROOT="${REPO_ROOT}/benchmarks/pattern-switch"
RUN_TIMESTAMP=$(date -u '+%Y-%m-%dT%H-%M-%SZ')
RUN_DIR="${OUTPUT_ROOT}/runs/${RUN_TIMESTAMP}"
RAW_LOG="${RUN_DIR}/raw.log"
SUMMARY_MD="${RUN_DIR}/summary.md"
SUMMARY_JSON="${RUN_DIR}/summary.json"
HISTORY_MD="${OUTPUT_ROOT}/history.md"
COMMAND=(cargo bench --manifest-path "${REPO_ROOT}/jv/Cargo.toml" --bench pattern_switch)

mkdir -p "${RUN_DIR}"

echo "Benchmark timestamp: ${RUN_TIMESTAMP} (UTC)"
echo "Running: ${COMMAND[*]}"

"${COMMAND[@]}" 2>&1 | tee "${RAW_LOG}"

python3 "${SCRIPT_DIR}/summarize_pattern_bench.py" \
  --raw "${RAW_LOG}" \
  --command "${COMMAND[*]}" \
  --timestamp "${RUN_TIMESTAMP}" \
  --summary-md "${SUMMARY_MD}" \
  --summary-json "${SUMMARY_JSON}" \
  --history "${HISTORY_MD}" \
  --base-path "${REPO_ROOT}"

ln -sfn "${RUN_DIR}" "${OUTPUT_ROOT}/latest"

echo "Summary written to ${SUMMARY_MD}"
echo "History updated: ${HISTORY_MD}"
