"""
Extract USER_FLD narrative / optional 'USER_FLD Options' lines and ABAP fence for Part 1 conv .md files.
Writes user_fld_validation.json next to the markdown reports.
"""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CONV = REPO / "ei docs for analysis" / "Part 1 conv"

ABAP_FENCE = re.compile(r"```abap\s*\n(.*?)```", re.DOTALL | re.IGNORECASE)
_FUNC = re.compile(r"^\s*FUNCTION\s+", re.MULTILINE | re.I)


def extract_abap(md: str) -> str:
    for m in ABAP_FENCE.finditer(md):
        b = m.group(1)
        if _FUNC.search(b):
            return b
    m = ABAP_FENCE.search(md)
    return m.group(1) if m else ""


def extract_user_fld_options_block(md: str) -> list[str]:
    """Lines after 'USER_FLD Options:' until a blank line then a line that looks like next parameter glossary."""
    if "USER_FLD Options:" not in md:
        return []
    i = md.index("USER_FLD Options:")
    tail = md[i + len("USER_FLD Options:") :]
    out: list[str] = []
    for line in tail.splitlines():
        t = line.strip()
        if not t:
            if out:
                break
            continue
        if re.match(r"^[A-Z][A-Z0-9_]{2,}\s+\(", t) and not t.upper().startswith("USER_FLD"):
            break
        out.append(t)
    return out


def structure_field_names(md: str) -> set[str]:
    """Rough parse: markdown table rows with /SKN/S_ in first column."""
    names: set[str] = set()
    for m in re.finditer(
        r"^\|\s*(/SKN/S_[^\s|]+)\s*\|\s*([A-Z0-9_]+)\s*\|",
        md,
        re.MULTILINE,
    ):
        names.add(m.group(2).strip().upper())
    return names


def main() -> None:
    if not CONV.is_dir():
        print(f"Missing: {CONV}", file=sys.stderr)
        sys.exit(1)
    rows = []
    for md_path in sorted(CONV.glob("*.md")):
        if "VALIDATION" in md_path.name.upper():
            continue
        text = md_path.read_text(encoding="utf-8")
        has = "USER_FLD" in text.upper()
        opts = extract_user_fld_options_block(text) if has else []
        abap = extract_abap(text)
        abap_has = bool(re.search(r"\bUSER_FLD\b", abap, re.I))
        fields = structure_field_names(text) if has else set()
        rows.append(
            {
                "file": md_path.name,
                "has_user_fld_doc": has,
                "user_fld_options_lines": opts,
                "abap_mentions_user_fld_literal": abap_has,
                "structure_field_count": len(fields),
                "sample_structure_fields": sorted(fields)[:25],
            }
        )
    out = CONV / "user_fld_validation.json"
    out.write_text(json.dumps(rows, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(f"Wrote {out} ({len(rows)} files)")


if __name__ == "__main__":
    main()
