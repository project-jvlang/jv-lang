# Repository Guidelines

## Project Structure & Module Organization
- Source: `orchestration/` holds the orchestration shell scripts.
- Agents: `orchestration/agents/` contains `top_po_agent.sh`, `domain_lead_agent.sh`, and `worker_agent.sh`.
- Domains: `orchestration/domain_structure.md` documents planned domains and roles.
- Runtime artifacts: logs are written under `orchestration/logs/` when scripts run (not committed to VCS).

## Build, Test, and Development Commands
- `./orchestration/master_orchestrator.sh setup`: Initialize local tooling the system expects.
- `./orchestration/master_orchestrator.sh start`: Launch tmux session and all agents.
- `./orchestration/master_orchestrator.sh status|dashboard|stop|test`: Inspect, visualize, stop, and exercise the system.
- `tmux attach -t jv-orchestration`: Attach to the running session; use `tmux list-windows` to navigate.
- Example (run a single agent): `./orchestration/agents/top_po_agent.sh`

## Coding Style & Naming Conventions
- Language: Bash with `#!/bin/bash` and `set -euo pipefail` at top.
- Indentation: 2 spaces; prefer small, pure functions; guard all external calls.
- Filenames: lowercase with underscores (e.g., `master_orchestrator.sh`).
- Identifiers: constants UPPER_CASE; locals snake_case. Window names use kebab-case.
- Logging: write to `orchestration/logs/agents/<agent>.log` and keep messages structured and terse.
- Lint/format (optional if installed): `bash -n <file>`, `shellcheck <file>`, `shfmt -i 2 -w <paths>`.

## Testing Guidelines
- Workflow: practice TDD where possible; aim for ≥90% coverage as described in the orchestration docs.
- Commands: `./orchestration/master_orchestrator.sh test` for system checks; unit-test shell functions by sourcing scripts in isolated subshells.
- Fast checks: `bash -n <file>` (syntax), `shellcheck` (lint). Add minimal fixtures under a temporary directory; never write outside `orchestration/`.

## Commit & Pull Request Guidelines
- Commits: follow Conventional Commits (e.g., `feat(agents): add domain lead checks`, `fix(orchestration): handle missing tmux`). Keep changes scoped and atomic.
- PRs: include purpose, linked issues, how to run (`commands used`), and logs/screenshots (tmux window list, relevant `orchestration/logs/...`).
- Review: prefer small PRs; add rollback notes if touching startup scripts.

## Agent-Specific Instructions
- New agent: add `orchestration/agents/<name>_agent.sh`, make it executable, log to `orchestration/logs/agents/<name>.log`.
- Registration: update `orchestration/tmux_orchestrator.sh` to spawn the new window and pass required args.
- Safety: validate inputs, fail fast, and trap signals to clean up tmux/windows.

