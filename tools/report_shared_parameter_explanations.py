"""
Build a report: parameters that appear in the Parameters Reference Table of
at least two Part 1 conv .md files, with each file's guideline explanation text.

Scope: ei docs for analysis/Part 1 conv/*.md (excludes *VALIDATION*.md).
Each .md pairs 1:1 with a Part 1 .docx per manifest.json.

USER_FLD / USR_FLD are omitted from the shared report (DRL narrative is long and
handled elsewhere).

Also writes `SHARED_PARAMETER_EXPLANATIONS_GE3_NO_BACKDAYS_USER_FLD.md` (+ optional
`.docx` / `.json`): parameters in ≥3 files, excluding BACKDAYS and USER_FLD/USR_FLD.
"""
from __future__ import annotations

import argparse
import importlib.util
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

_TOOLS = Path(__file__).resolve().parent
if str(_TOOLS) not in sys.path:
    sys.path.insert(0, str(_TOOLS))
from sap_unified_param_texts import SAP_UNIFIED_EXPLANATION

REPO = Path(__file__).resolve().parents[1]
CONV = REPO / "ei docs for analysis" / "Part 1 conv"

_EXCLUDED_FROM_REPORT = frozenset({"USER_FLD", "USR_FLD"})
# ≥3-file subset export: drop ubiquitous lookback + DRL field names
_GE3_EXPORT_EXCLUDED = frozenset({"BACKDAYS", "USER_FLD", "USR_FLD"})

# Table row: | n | FIELD | ...
RE_TABLE_ROW = re.compile(r"^\|\s*\d+\s*\|\s*([^|]+?)\s*\|")

# Guideline block: FIELDNAME (description...): — description may contain ')' e.g. D/H/M), (from ...)
RE_PARAM_HEAD = re.compile(r"^([A-Z][A-Z0-9_]*)\s*\(.*\)\s*:?\s*$")

RE_GUIDELINES_START = re.compile(r"(?m)^###\s+Parameter Configuration Guidelines\s*$")

# First "Options" block (main prose ends here for comparison)
RE_OPTIONS_LINE = re.compile(
    r"^(?:\*\*)?([A-Z][A-Z0-9_]*)?\s*Options:\s*(?:\*\*)?\s*$|"
    r"^\*\*[A-Z][A-Z0-9_]*\s+Options:\*\*\s*$",
    re.I,
)


def _extract_guidelines_section(md: str) -> str:
    """Body under ### Parameter Configuration Guidelines until the next ### heading."""
    m = RE_GUIDELINES_START.search(md)
    if not m:
        return ""
    rest = md[m.end() :]
    out: list[str] = []
    for line in rest.splitlines():
        stripped = line.strip()
        if stripped.startswith("### ") and "Parameter Configuration Guidelines" not in stripped:
            break
        out.append(line)
    return "\n".join(out)


def _extract_fields(md: str) -> set[str]:
    fields: set[str] = set()
    in_param_table = False
    for line in md.splitlines():
        if "| # | Field |" in line and "Description" in line:
            in_param_table = True
            continue
        if not in_param_table:
            continue
        if line.strip().startswith("| ---") or line.strip().startswith("|---"):
            continue
        if not line.strip().startswith("|"):
            if fields:
                break
            continue
        m = RE_TABLE_ROW.match(line)
        if not m:
            if fields:
                break
            continue
        name = m.group(1).strip()
        if name.isdigit() or name in ("Field", "#"):
            continue
        fields.add(name)
    return fields


def _parse_param_explanations(guidelines_body: str) -> dict[str, str]:
    """Map FIELDNAME -> main explanation (text before first *Options: line)."""
    lines = guidelines_body.splitlines()
    i = 0
    # Skip leading blanks / IMPORTANT
    while i < len(lines):
        s = lines[i].strip()
        if not s or s.upper().startswith("IMPORTANT:"):
            i += 1
            continue
        break
    out: dict[str, str] = {}
    while i < len(lines):
        s = lines[i].strip()
        mh = RE_PARAM_HEAD.match(s)
        if not mh:
            i += 1
            continue
        name = mh.group(1)
        i += 1
        body_lines: list[str] = []
        while i < len(lines):
            s2 = lines[i].strip()
            if RE_PARAM_HEAD.match(s2):
                break
            if RE_OPTIONS_LINE.match(s2):
                break
            body_lines.append(lines[i])
            i += 1
        raw = "\n".join(body_lines).strip()
        one_line = re.sub(r"\s+", " ", raw).strip()
        out[name] = one_line[:8000]
    return out


def _build_rows_for_params(
    param_to_files: dict[str, list[str]],
    file_to_expl: dict[str, dict[str, str]],
    params: list[str],
) -> list[dict[str, str]]:
    rows: list[dict[str, str]] = []
    for param in params:
        for fname in sorted(set(param_to_files[param])):
            expl = file_to_expl.get(fname, {}).get(param, "")
            if not expl:
                expl = "*(no matching guideline block found for this field name)*"
            sug = SAP_UNIFIED_EXPLANATION.get(param, "")
            if not sug:
                sug = "*(no SAP unified text defined for this parameter)*"
            rows.append(
                {
                    "parameter": param,
                    "explanation": expl,
                    "suggested_explanation": sug,
                    "file": fname,
                }
            )
    return rows


def _write_markdown_table(
    *,
    title: str,
    scope_lines: list[str],
    counts_line: str,
    rows: list[dict[str, str]],
    footer_tooling: str,
    out_md: Path,
) -> None:
    def esc_cell(s: str) -> str:
        return s.replace("|", "\\|")

    md_lines = [title, ""] + scope_lines + ["", counts_line, ""] + [
        "| parameter | explanation | suggested_explanation | file |",
        "|-----------|-------------|----------------------|------|",
    ]
    for r in rows:
        md_lines.append(
            f"| {esc_cell(r['parameter'])} | {esc_cell(r['explanation'])} | {esc_cell(r['suggested_explanation'])} | {esc_cell(r['file'])} |"
        )
    md_lines += ["", "---", "", footer_tooling, ""]
    out_md.write_text("\n".join(md_lines), encoding="utf-8")


def main() -> None:
    md_paths = sorted(
        p
        for p in CONV.glob("*.md")
        if "VALIDATION" not in p.name.upper()
        and "SHARED_PARAMETER" not in p.name.upper()
    )
    param_to_files: dict[str, list[str]] = defaultdict(list)
    file_to_expl: dict[str, dict[str, str]] = {}

    for path in md_paths:
        text = path.read_text(encoding="utf-8")
        fields = _extract_fields(text)
        gbody = _extract_guidelines_section(text)
        expl = _parse_param_explanations(gbody) if gbody else {}
        file_to_expl[path.name] = expl
        for f in fields:
            param_to_files[f].append(path.name)

    shared = sorted(
        p
        for p, fs in param_to_files.items()
        if len(set(fs)) >= 2 and p not in _EXCLUDED_FROM_REPORT
    )

    rows = _build_rows_for_params(param_to_files, file_to_expl, shared)

    ge3_params = sorted(
        p
        for p, fs in param_to_files.items()
        if len(set(fs)) >= 3 and p not in _GE3_EXPORT_EXCLUDED
    )
    rows_ge3 = _build_rows_for_params(param_to_files, file_to_expl, ge3_params)

    out_json = CONV / "shared_parameter_explanations.json"
    out_json.write_text(
        json.dumps(
            {
                "scope": "Part 1 conv .md paired with Part 1 .docx (manifest.json); "
                "parameters listed in ≥2 distinct files' Parameters Reference Table.",
                "file_count": len(md_paths),
                "shared_parameter_count": len(shared),
                "rows": rows,
            },
            ensure_ascii=False,
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )

    out_json_ge3 = CONV / "shared_parameter_explanations_ge3_no_backdays_user_fld.json"
    out_json_ge3.write_text(
        json.dumps(
            {
                "scope": "Parameters in ≥3 Part 1 conv files; excludes BACKDAYS, USER_FLD, USR_FLD.",
                "file_count": len(md_paths),
                "parameter_count": len(ge3_params),
                "rows": rows_ge3,
            },
            ensure_ascii=False,
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )

    out_md = CONV / "SHARED_PARAMETER_EXPLANATIONS.md"
    _write_markdown_table(
        title="# Shared parameter explanations (Part 1 conv)",
        scope_lines=[
            "**Scope:** Each row is one `(parameter, file)` pair. **Parameters** are those that appear in the **Parameters Reference Table** of **at least two** converted Markdown files (each file corresponds to one original Part 1 `.docx` per `manifest.json`). **USER_FLD** and **USR_FLD** are excluded from this report.",
            "",
            "**Explanation:** Text taken from `### Parameter Configuration Guidelines` immediately under the `FIELD (Description)` heading, **stopping before** the first `… Options:` / `**… Options:**` subsection for that parameter (so option bullets are omitted for brevity).",
            "",
            "**Suggested (SAP) explanation:** One concise, SAP-technical reading of the parameter name (DDIC / transaction context) intended to be reusable across all listed files—supersedes generic wording where the per-file text drifts.",
        ],
        counts_line=f"**Counts:** {len(md_paths)} EI files scanned; **{len(shared)}** parameters appear in ≥2 files; **{len(rows)}** table rows.",
        rows=rows,
        footer_tooling=(
            "**Tooling:** `python tools/report_shared_parameter_explanations.py [--docx]` — unified SAP strings in `tools/sap_unified_param_texts.py`. "
            "Use `--docx` to write both `.docx` files beside their Markdown sources."
        ),
        out_md=out_md,
    )

    out_md_ge3 = CONV / "SHARED_PARAMETER_EXPLANATIONS_GE3_NO_BACKDAYS_USER_FLD.md"
    _write_markdown_table(
        title="# Shared parameters — ≥3 files (Part 1 conv)",
        scope_lines=[
            "**Scope:** Same columns as the main shared report, but only parameters that appear in the **Parameters Reference Table** of **three or more** Part 1 converted Markdown files (each ↔ one original `.docx`). **BACKDAYS**, **USER_FLD**, and **USR_FLD** are excluded from this extract.",
            "",
            "**Explanation / suggested:** Same extraction rules as the main shared report.",
        ],
        counts_line=(
            f"**Counts:** {len(md_paths)} EI files scanned; **{len(ge3_params)}** parameters appear in ≥3 files (after exclusions); **{len(rows_ge3)}** table rows."
        ),
        rows=rows_ge3,
        footer_tooling=(
            "**Output:** `SHARED_PARAMETER_EXPLANATIONS_GE3_NO_BACKDAYS_USER_FLD.md` / `.docx` plus `shared_parameter_explanations_ge3_no_backdays_user_fld.json`."
        ),
        out_md=out_md_ge3,
    )

    print(f"Wrote {out_json}")
    print(f"Wrote {out_json_ge3}")
    print(f"Wrote {out_md}")
    print(f"Wrote {out_md_ge3}")


def _write_docx(md_path: Path) -> Path:
    md2 = REPO / "scripts" / "pipeline" / "md_to_docx.py"
    spec = importlib.util.spec_from_file_location("md_to_docx", md2)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Cannot load {md2}")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    out_docx = md_path.with_suffix(".docx")
    mod.convert_md_to_docx(md_path, out_docx, skip_spacing_verify=True)
    return out_docx


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--docx",
        action="store_true",
        help="Also write SHARED_PARAMETER_EXPLANATIONS.docx and SHARED_PARAMETER_EXPLANATIONS_GE3_NO_BACKDAYS_USER_FLD.docx.",
    )
    args = ap.parse_args()
    main()
    if args.docx:
        # Write GE3 first: main SHARED_PARAMETER_EXPLANATIONS.docx is often locked if open in Word.
        p_ge3 = _write_docx(CONV / "SHARED_PARAMETER_EXPLANATIONS_GE3_NO_BACKDAYS_USER_FLD.md")
        print(f"Wrote {p_ge3}")
        try:
            p_main = _write_docx(CONV / "SHARED_PARAMETER_EXPLANATIONS.md")
            print(f"Wrote {p_main}")
        except OSError as e:
            print(
                f"Skipped main SHARED_PARAMETER_EXPLANATIONS.docx ({e}). "
                "Close the file in Word and re-run with --docx.",
                file=sys.stderr,
            )
