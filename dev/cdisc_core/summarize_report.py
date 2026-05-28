#!/usr/bin/env python3
"""
Render a CDISC CORE JSON validation report as a GitHub-flavoured markdown
summary. Reads the JSON path from argv[1], writes markdown to stdout.

Layout (issue #334 phase 2):
  1. Conformance details (standard, version, runtime, engine version)
  2. Datasets validated
  3. Rule execution counts by status (SUCCESS, ISSUE_REPORTED, SKIPPED, ...)
  4. Top issues grouped by rule
  5. Top issues grouped by dataset
  6. First N detailed issues with row pointers
"""
import json
import sys
from collections import Counter, defaultdict

MAX_RULE_ROWS = 25
MAX_DATASET_ROWS = 15
MAX_DETAIL_ROWS = 25


def md_escape(value):
    if value is None:
        return ""
    s = str(value)
    return s.replace("|", "\\|").replace("\n", " ").strip()


def write_table(headers, rows, out):
    out.append("| " + " | ".join(headers) + " |")
    out.append("|" + "|".join(["---"] * len(headers)) + "|")
    for r in rows:
        out.append("| " + " | ".join(md_escape(c) for c in r) + " |")
    out.append("")


def main():
    if len(sys.argv) < 2:
        sys.stderr.write("usage: summarize_report.py <validation_report.json>\n")
        sys.exit(2)

    with open(sys.argv[1]) as f:
        report = json.load(f)

    out = []
    conf = report.get("Conformance_Details", {})
    out.append("## CDISC CORE Validation Summary")
    out.append("")
    out.append(f"- Standard: `{conf.get('Standard', '?')} {conf.get('Version', '?')}`")
    out.append(f"- CORE engine: `{conf.get('CORE_Engine_Version', '?')}`")
    out.append(f"- Runtime: {conf.get('Total_Runtime', '?')}")
    out.append(f"- Report generated: {conf.get('Report_Generation', '?')}")
    out.append("")

    datasets = report.get("Dataset_Details", [])
    out.append("### Datasets validated")
    out.append("")
    if datasets:
        rows = [
            (
                d.get("filename", ""),
                d.get("label", ""),
                d.get("length", ""),
                f"{d.get('size_kb', 0):.2f} KB",
            )
            for d in datasets
        ]
        write_table(["filename", "label", "rows", "size"], rows, out)
    else:
        out.append("_no datasets validated_")
        out.append("")

    rules_report = report.get("Rules_Report", [])
    status_counts = Counter(r.get("status", "UNKNOWN") for r in rules_report)
    out.append("### Rule execution status")
    out.append("")
    out.append(f"- Total rules evaluated: **{len(rules_report)}**")
    for status in sorted(status_counts):
        out.append(f"- {status}: {status_counts[status]}")
    out.append("")

    issue_summary = report.get("Issue_Summary", [])
    issue_details = report.get("Issue_Details", [])
    out.append(
        f"### Issues found: {len(issue_summary)} summaries / "
        f"{len(issue_details)} detailed findings"
    )
    out.append("")

    if issue_summary:
        by_rule = defaultdict(lambda: {"issues": 0, "datasets": set(), "message": ""})
        for s in issue_summary:
            entry = by_rule[s.get("core_id", "")]
            entry["issues"] += int(s.get("issues", 0) or 0)
            entry["datasets"].add(s.get("dataset", ""))
            entry["message"] = s.get("message", entry["message"])
        top_rules = sorted(
            by_rule.items(), key=lambda kv: kv[1]["issues"], reverse=True
        )[:MAX_RULE_ROWS]

        out.append(f"#### Top issues by rule (showing {len(top_rules)})")
        out.append("")
        rows = [
            (
                core_id,
                e["issues"],
                ", ".join(sorted(e["datasets"])),
                e["message"],
            )
            for core_id, e in top_rules
        ]
        write_table(["rule", "issues", "datasets", "message"], rows, out)

        by_dataset = Counter()
        for s in issue_summary:
            by_dataset[s.get("dataset", "")] += int(s.get("issues", 0) or 0)
        out.append(
            f"#### Top issues by dataset (showing {min(len(by_dataset), MAX_DATASET_ROWS)})"
        )
        out.append("")
        rows = [
            (dataset or "(unknown)", count)
            for dataset, count in by_dataset.most_common(MAX_DATASET_ROWS)
        ]
        write_table(["dataset", "issues"], rows, out)

    if issue_details:
        shown = issue_details[:MAX_DETAIL_ROWS]
        out.append(f"#### First {len(shown)} detailed findings")
        out.append("")
        rows = [
            (
                d.get("core_id", ""),
                d.get("dataset", ""),
                d.get("row", ""),
                d.get("USUBJID", ""),
                d.get("variables", ""),
                d.get("message", ""),
            )
            for d in shown
        ]
        write_table(
            ["rule", "dataset", "row", "USUBJID", "variables", "message"], rows, out
        )

    out.append(
        "_Full JSON report uploaded as the `core-validation-report` artifact._"
    )
    out.append("")
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
