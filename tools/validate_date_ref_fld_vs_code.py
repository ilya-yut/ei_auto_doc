"""Extract DATE_REF_FLD doc options and CASE LV_DATE_REF_FLD WHEN branches from Part 1 conv .md files."""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CONV = REPO / "ei docs for analysis" / "Part 1 conv"

ABAP = re.compile(r"```abap\s*\n(.*?)```", re.DOTALL | re.I)
_FUNC = re.compile(r"^\s*FUNCTION\s+", re.M | re.I)


def _abap(md: str) -> str:
    for m in ABAP.finditer(md):
        b = m.group(1)
        if _FUNC.search(b):
            return b
    m = ABAP.search(md)
    return m.group(1) if m else ""


def _is_option_bullet_line(line: str) -> bool:
    t = line.lstrip()
    if t.startswith(("-", "·", "\u2022")):
        return True
    # Word → Markdown export may replace bullet with U+FFFD or odd leading glyphs
    if re.match(r"^[\s\ufffd\xb7\u2022]*[A-Z][A-Z0-9_]*\s*[—\u2013\-:]", line):
        return True
    return False


def _doc_options(md: str) -> list[str]:
    if "DATE_REF_FLD Options:" not in md:
        return []
    i = md.index("DATE_REF_FLD Options:")
    chunk = md[i : i + 3500]
    lines_out: list[str] = []
    for line in chunk.splitlines()[1:]:
        t = line.strip()
        if not t:
            continue
        if _is_option_bullet_line(line):
            lines_out.append(t)
            continue
        if re.match(r"^[A-Z][A-Z0-9_]{2,}\s+\(", t):
            break
        if lines_out:
            break
    return lines_out


def _case_when_values(abap: str) -> list[str]:
    """Find CASE LV_DATE_REF_FLD ... ENDCASE first block WHEN literals."""
    m = re.search(
        r"CASE\s+LV_DATE_REF_FLD\s*\.(.*?)ENDCASE",
        abap,
        re.DOTALL | re.I,
    )
    if not m:
        return []
    block = m.group(1)
    vals = []
    for wm in re.finditer(r"WHEN\s+'([A-Z0-9_]+)'", block, re.I):
        vals.append(wm.group(1).upper())
    if re.search(r"WHEN\s+OTHERS", block, re.I):
        vals.append("OTHERS")
    return vals


def main() -> None:
    if not CONV.is_dir():
        sys.exit(f"Missing {CONV}")
    rows = []
    for p in sorted(CONV.glob("*.md")):
        if "VALIDATION" in p.name.upper():
            continue
        t = p.read_text(encoding="utf-8")
        has = bool(re.search(r"DATE_REF_FLD", t, re.I))
        ab = _abap(t)
        rows.append(
            {
                "file": p.name,
                "has_date_ref_fld_mention": has,
                "doc_option_lines": _doc_options(t),
                "case_when_values_first_block": _case_when_values(ab),
                "abap_has_case_lv_date_ref": bool(
                    re.search(r"CASE\s+LV_DATE_REF_FLD", ab, re.I)
                ),
                "abap_default_strtdate": bool(
                    re.search(
                        r"IF\s+LV_DATE_REF_FLD\s+IS\s+INITIAL\s*\.\s*\n\s*LV_DATE_REF_FLD\s*=\s*'STRTDATE'",
                        ab,
                        re.I | re.DOTALL,
                    )
                ),
            }
        )
    out = CONV / "date_ref_fld_validation.json"
    out.write_text(json.dumps(rows, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(f"Wrote {out}")


if __name__ == "__main__":
    main()
