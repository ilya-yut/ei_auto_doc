"""
Validate 'Default Values' sections in converted EI .md files against explicit defaults in ABAP.

Heuristics for code-side "explicit defaults":
- DATA ... VALUE ...
- CONSTANTS ... VALUE ...
- Assignment lines: LV_* = ... . (typical EI framework)
- PROCESS_ICON = ... , IS_ALERT = ... at init when clearly defaulting
"""
from __future__ import annotations

import json
import re
from pathlib import Path

# Section header variants
SECTION_PAT = re.compile(
    r"(?ms)^(?P<head>#{1,4}\s*Default Values\s*|^Default Values\s*)\n(?P<body>.*?)(?=^#{1,3}\s|\Z)"
)
ABAP_FENCE = re.compile(r"```abap\s*\n(.*?)```", re.DOTALL | re.IGNORECASE)
_FUNC_START = re.compile(r"^\s*FUNCTION\s+", re.MULTILINE | re.IGNORECASE)


# Strip ABAP comments for some scans
def _strip_abap_comments(block: str) -> str:
    lines = []
    for line in block.splitlines():
        if re.match(r"^\s*\*", line):
            continue
        lines.append(line)
    return "\n".join(lines)


def extract_abap(md: str) -> str:
    """Use the ```abap fence that contains FUNCTION ... (conversion may place a spurious fence earlier)."""
    blocks = list(ABAP_FENCE.finditer(md))
    for m in blocks:
        inner = m.group(1)
        if _FUNC_START.search(inner):
            return inner
    if blocks:
        return blocks[0].group(1)
    return ""


# Plain-text section titles in Word export (not always markdown headings)
_PLAIN_STOP_LINES = frozenset(
    {
        "Practical Example of Parameter Configuration",
        "EI Function Structure",
        "ABAP Code",
        "Parameter Relationships",
        "Parameter Configuration Guidelines",
    }
)


def _trim_default_body(body: str) -> str:
    """Stop before next markdown heading or known plain-title section."""
    out_lines: list[str] = []
    for line in body.splitlines():
        stripped = line.strip()
        if stripped.startswith("#") and not stripped.lower().startswith("# default"):
            break
        if stripped in _PLAIN_STOP_LINES:
            break
        if stripped.startswith("Use Case ") and out_lines:
            break
        out_lines.append(line)
    return "\n".join(out_lines).strip()


def extract_default_section(md: str) -> tuple[str | None, str]:
    """Return (section_body or None if missing, raw match note)."""
    for pat in (
        r"(?ms)^###\s*Default Values\s*\n(?P<body>.*?)(?=^###\s|^##\s|\Z)",
        r"(?ms)^##\s*Default Values\s*\n(?P<body>.*?)(?=^##\s|^#\s[^#]|\Z)",
        r"(?ms)^Default Values\s*\n(?P<body>.*?)(?=^#{1,3}\s|\Z)",
    ):
        m = re.search(pat, md)
        if m:
            return _trim_default_body(m.group("body")), ""
    return None, "no Default Values heading found"


def find_explicit_code_defaults(abap: str) -> list[dict]:
    """Collect evidence lines for defaults (not exhaustive for macro-expanded code)."""
    evidence: list[dict] = []
    clean = _strip_abap_comments(abap)

    # DATA ... VALUE ...
    for m in re.finditer(
        r"\bDATA\s+(?:[^.\n]|\.(?!\s))*?\bVALUE\s+([^\.\n]+(?:\.[^\n]+)?)\s*\.",
        clean,
        re.IGNORECASE | re.DOTALL,
    ):
        line = m.group(0).replace("\n", " ")[:500]
        evidence.append({"kind": "DATA_VALUE", "line": line.strip()})

    # CONSTANTS ... VALUE
    for m in re.finditer(
        r"CONSTANTS\s+[^.]+\bVALUE\s+[^.]+\.",
        clean,
        re.IGNORECASE | re.DOTALL,
    ):
        evidence.append({"kind": "CONSTANTS_VALUE", "line": m.group(0).replace("\n", " ").strip()[:500]})

    # LV_* = 'x'.  or LV_* = number.
    for line in clean.splitlines():
        s = line.strip()
        if re.match(r"^LV_[A-Z0-9_]+\s*=\s*.+\.\s*(\".*)?$", s, re.I):
            if re.match(r"^LV_CNT\s*=\s*SY-TFILL", s, re.I):
                continue
            evidence.append({"kind": "LV_ASSIGN", "line": s[:400]})

    # Other single-token field assigns often used as defaults (not in IF)
    for line in clean.splitlines():
        s = line.strip()
        if re.match(
            r"^(PROCESS_ICON|LV_DURATION_UNIT|LV_MANAGE_IN_UTC)\s*=\s*.+\.\s*$",
            s,
            re.I,
        ):
            evidence.append({"kind": "FIELD_ASSIGN", "line": s[:400]})
        if re.match(r"^IS_ALERT\s*=\s*.+\.\s*$", s, re.I):
            evidence.append({"kind": "ALERT_FLAG", "line": s[:400]})

    if re.search(r"IF\s+LV_BACKDAYS\s+IS\s+INITIAL", clean, re.I):
        m = re.search(
            r"IF\s+LV_BACKDAYS\s+IS\s+INITIAL\.\s*\n\s*LV_BACKDAYS\s*=\s*(\d+)\s*\.",
            clean,
            re.I,
        )
        if m:
            evidence.append(
                {
                    "kind": "LV_BACKDAYS_INIT",
                    "line": f"IF LV_BACKDAYS IS INITIAL. LV_BACKDAYS = {m.group(1)}.",
                }
            )

    for m in re.finditer(
        r"BACKDAYS\s*=\s*'?1'?\s*\.\s*(?:\"|---\s*Default)",
        clean,
        re.I,
    ):
        start = max(0, m.start() - 50)
        snippet = clean[start : m.end() + 20].replace("\n", " ").strip()
        evidence.append({"kind": "BACKDAYS_DEFAULT", "line": snippet[:400]})

    if re.search(r"LV_MAXSEL\s*=\s*500", clean, re.I):
        evidence.append({"kind": "MAXSEL_DEFAULT", "line": "LV_MAXSEL = 500 (when LV_MAX_RECORDS initial)"})

    # Dedupe by line
    seen: set[str] = set()
    out: list[dict] = []
    for e in evidence:
        k = e["line"]
        if k not in seen:
            seen.add(k)
            out.append(e)
    return out


def parse_doc_default_bullets(section: str) -> list[str]:
    """Non-empty lines, normalized, from Default Values section."""
    lines = []
    for raw in section.splitlines():
        t = raw.strip()
        if not t:
            continue
        # skip pure decorative bullets only
        if re.match(r"^·\s*$", t):
            continue
        lines.append(t)
    return lines


def analyze_file(md_path: Path) -> dict:
    text = md_path.read_text(encoding="utf-8")
    section, err = extract_default_section(text)
    abap = extract_abap(text)
    code_ev = find_explicit_code_defaults(abap) if abap else []

    return {
        "file": md_path.name,
        "section_found": section is not None,
        "section_error": err if section is None else "",
        "default_section_text": section or "",
        "default_bullets": parse_doc_default_bullets(section) if section else [],
        "code_evidence": code_ev,
        "abap_chars": len(abap),
    }


def main() -> None:
    root = Path(__file__).resolve().parent.parent / "ei docs for analysis" / "Part 1 conv"
    rows = []
    for md in sorted(root.glob("*.md")):
        rows.append(analyze_file(md))
    out = Path(__file__).resolve().parent.parent / "ei docs for analysis" / "Part 1 conv" / "default_values_validation.json"
    out.write_text(json.dumps(rows, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"Wrote {out} ({len(rows)} files)")


if __name__ == "__main__":
    main()
