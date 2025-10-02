#!/usr/bin/env python3
"""Generate structured summaries from cargo bench output for pattern switch benchmarks."""
from __future__ import annotations

import argparse
import json
from dataclasses import dataclass, asdict, field
from pathlib import Path
import re
from typing import Dict, List, Optional

UNIT_MULTIPLIERS = {
    "s": 1.0,
    "ms": 1e-3,
    "us": 1e-6,
    "µs": 1e-6,
    "ns": 1e-9,
}

BUDGET_SECONDS = {
    "java25_when_arms_100": 0.010,
    "java21_when_arms_100": 0.010,
    "java25_when_arms_500": 0.025,
    "java21_when_arms_500": 0.025,
    "java25_when_arms_1000": 0.050,
    "java21_when_arms_1000": 0.050,
    "java25_when_arms_5000": 0.250,
    "java21_when_arms_5000": 0.250,
    "java25_sealed_depth10": 0.100,
    "java21_sealed_depth10": 0.100,
}

SINGLE_RUN_RE = re.compile(
    r"^(?P<name>\S+)\s+single-run:\s+(?P<value>[0-9.]+)(?P<unit>[a-zµ]+)"
    r"(?:\s+\(budget\s+(?P<budget_value>[0-9.]+)(?P<budget_unit>[a-zµ]+)\))?"
)

TIME_RE = re.compile(
    r"^(?P<name>\S+)\s+time:\s+\[(?P<min_val>[0-9.]+)\s*(?P<min_unit>[a-zµ]+)\s+"
    r"(?P<median_val>[0-9.]+)\s*(?P<median_unit>[a-zµ]+)\s+(?P<max_val>[0-9.]+)\s*(?P<max_unit>[a-zµ]+)\]"
)

CHANGE_RE = re.compile(
    r"^change:\s*\[(?P<low>[-+0-9.%]+)\s+(?P<mid>[-+0-9.%]+)\s+(?P<high>[-+0-9.%]+)\]\s*\(p = (?P<pvalue>[^)]+)\)"
)


@dataclass
class ScenarioSummary:
    name: str
    median_seconds: Optional[float] = None
    median_text: Optional[str] = None
    min_text: Optional[str] = None
    max_text: Optional[str] = None
    single_run_seconds: Optional[float] = None
    single_run_text: Optional[str] = None
    budget_seconds: Optional[float] = None
    budget_text: Optional[str] = None
    status: Optional[str] = None
    change_low: Optional[str] = None
    change_mid: Optional[str] = None
    change_high: Optional[str] = None
    change_pvalue: Optional[str] = None
    notes: List[str] = field(default_factory=list)

    def finalize(self) -> None:
        if self.budget_seconds is None and self.name in BUDGET_SECONDS:
            self.budget_seconds = BUDGET_SECONDS[self.name]
            self.budget_text = format_duration(self.budget_seconds)

        if self.median_seconds is not None and self.budget_seconds is not None:
            if self.median_seconds <= self.budget_seconds:
                self.status = "PASS"
            else:
                self.status = "FAIL"
        elif self.median_seconds is not None:
            self.status = "N/A"

        if self.median_text is None and self.median_seconds is not None:
            self.median_text = format_duration(self.median_seconds)

        if self.single_run_text is None and self.single_run_seconds is not None:
            self.single_run_text = format_duration(self.single_run_seconds)

        if self.budget_text is None and self.budget_seconds is not None:
            self.budget_text = format_duration(self.budget_seconds)


def to_seconds(value: str, unit: str) -> float:
    normalized_unit = unit.replace("µ", "u")
    multiplier = UNIT_MULTIPLIERS.get(normalized_unit)
    if multiplier is None:
        raise ValueError(f"Unsupported unit: {unit}")
    return float(value) * multiplier


def format_duration(seconds: float) -> str:
    if seconds >= 1.0:
        return f"{seconds:.3f} s"
    if seconds >= 1e-3:
        return f"{seconds * 1e3:.3f} ms"
    if seconds >= 1e-6:
        return f"{seconds * 1e6:.3f} us"
    return f"{seconds * 1e9:.3f} ns"


def format_pair(value: str, unit: str) -> str:
    return f"{value} {unit.replace('µ', 'u')}"


def render_change_cell(summary: ScenarioSummary) -> str:
    parts: List[str] = []
    if summary.change_mid:
        change_text = summary.change_mid
        if summary.change_low and summary.change_high:
            change_text += f" [{summary.change_low}, {summary.change_high}]"
        if summary.change_pvalue:
            change_text += f" (p={summary.change_pvalue})"
        parts.append(change_text)
    parts.extend(summary.notes)
    if not parts:
        return "-"
    return "<br>".join(parts)


def parse_raw_log(raw_text: str) -> List[ScenarioSummary]:
    summaries: Dict[str, ScenarioSummary] = {}
    current: Optional[ScenarioSummary] = None

    for raw_line in raw_text.splitlines():
        line = raw_line.rstrip()
        if not line:
            continue

        single_match = SINGLE_RUN_RE.match(line)
        if single_match:
            name = single_match.group("name")
            summary = summaries.setdefault(name, ScenarioSummary(name=name))
            summary.single_run_seconds = to_seconds(
                single_match.group("value"), single_match.group("unit")
            )
            summary.single_run_text = format_duration(summary.single_run_seconds)
            budget_val = single_match.group("budget_value")
            budget_unit = single_match.group("budget_unit")
            if budget_val and budget_unit:
                summary.budget_seconds = to_seconds(budget_val, budget_unit)
                summary.budget_text = format_duration(summary.budget_seconds)
            current = summary
            continue

        time_match = TIME_RE.match(line)
        if time_match:
            name = time_match.group("name")
            summary = summaries.setdefault(name, ScenarioSummary(name=name))
            summary.min_text = format_pair(
                time_match.group("min_val"), time_match.group("min_unit")
            )
            summary.median_seconds = to_seconds(
                time_match.group("median_val"), time_match.group("median_unit")
            )
            summary.median_text = format_pair(
                time_match.group("median_val"), time_match.group("median_unit")
            )
            summary.max_text = format_pair(
                time_match.group("max_val"), time_match.group("max_unit")
            )
            current = summary
            continue

        stripped = line.lstrip()
        stripped_ascii = stripped.replace("µ", "u")
        if stripped.startswith("change:") and current is not None:
            change_match = CHANGE_RE.match(stripped)
            if change_match:
                current.change_low = change_match.group("low")
                current.change_mid = change_match.group("mid")
                current.change_high = change_match.group("high")
                current.change_pvalue = change_match.group("pvalue").strip()
            continue

        if current is not None and not line.startswith("Benchmarking") and not line.startswith("Warning:"):
            # capture qualitative comment lines following change section
            current.notes.append(stripped_ascii)

    # finalize
    for summary in summaries.values():
        summary.finalize()

    # preserve deterministic order (alphabetical to make diffs stable)
    return [summaries[name] for name in sorted(summaries)]


def display_path(path: Path, base: Optional[Path]) -> str:
    if base is None:
        return str(path)
    try:
        return str(path.relative_to(base))
    except ValueError:
        return str(path)


def write_summary_md(
    path: Path,
    timestamp: str,
    command: str,
    summaries: List[ScenarioSummary],
    raw_log: Path,
    base_path: Optional[Path],
) -> None:
    lines = [
        "# Pattern Switch Benchmark Summary",
        f"- Timestamp: {timestamp} (UTC)",
        f"- Command: {command}",
        f"- Raw log: {display_path(raw_log, base_path)}",
        "",
        "| Scenario | Median | Budget | Status | Change | Single-run |",
        "|----------|--------|--------|--------|--------|------------|",
    ]
    for summary in summaries:
        change_cell = render_change_cell(summary)
        lines.append(
            "| {name} | {median} | {budget} | {status} | {change} | {single_run} |".format(
                name=summary.name,
                median=summary.median_text or "-",
                budget=summary.budget_text or "-",
                status=summary.status or "-",
                change=change_cell,
                single_run=summary.single_run_text or "-",
            )
        )
    lines.append("")
    path.write_text("\n".join(lines), encoding="utf-8")


def append_history(
    path: Path,
    timestamp: str,
    summaries: List[ScenarioSummary],
    command: str,
    summary_path: Path,
    base_path: Optional[Path],
) -> None:
    if not path.exists():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("# Pattern Switch Benchmark History\n\n", encoding="utf-8")

    lines = [
        f"## {timestamp} (UTC)",
        f"Command: {command}",
        f"Summary: {display_path(summary_path, base_path)}",
        "",
        "| Scenario | Median | Budget | Status | Change | Single-run |",
        "|----------|--------|--------|--------|--------|------------|",
    ]
    for summary in summaries:
        change_cell = render_change_cell(summary)
        lines.append(
            "| {name} | {median} | {budget} | {status} | {change} | {single_run} |".format(
                name=summary.name,
                median=summary.median_text or "-",
                budget=summary.budget_text or "-",
                status=summary.status or "-",
                change=change_cell,
                single_run=summary.single_run_text or "-",
            )
        )
    lines.append("")
    with path.open("a", encoding="utf-8") as fh:
        fh.write("\n".join(lines))


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--raw", required=True, help="Path to raw cargo bench output")
    parser.add_argument("--command", required=True, help="Command executed")
    parser.add_argument("--timestamp", required=True, help="Run timestamp (UTC)")
    parser.add_argument("--summary-md", required=True, help="Output markdown summary path")
    parser.add_argument("--summary-json", required=True, help="Output JSON summary path")
    parser.add_argument("--history", required=True, help="History markdown file path")
    parser.add_argument("--base-path", required=False, help="Base path for relative links")
    args = parser.parse_args()

    raw_path = Path(args.raw)
    raw_text = raw_path.read_text(encoding="utf-8")
    summaries = parse_raw_log(raw_text)

    summary_md_path = Path(args.summary_md)
    summary_md_path.parent.mkdir(parents=True, exist_ok=True)
    base_path = Path(args.base_path).resolve() if args.base_path else None

    write_summary_md(
        summary_md_path, args.timestamp, args.command, summaries, raw_path, base_path
    )

    summary_json_path = Path(args.summary_json)
    summary_json_path.write_text(
        json.dumps([asdict(s) for s in summaries], indent=2, ensure_ascii=False),
        encoding="utf-8",
    )

    history_path = Path(args.history)
    append_history(
        history_path, args.timestamp, summaries, args.command, summary_md_path, base_path
    )

    print(f"Parsed {len(summaries)} scenarios. Summary: {summary_md_path}")


if __name__ == "__main__":
    main()
