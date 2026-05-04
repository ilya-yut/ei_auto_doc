"""Scan EI docs .docx for user_fld / usr_fld; write EI_docs_user_fld_usr_fld.md."""
from __future__ import annotations

import re
import sys
import zipfile
import xml.etree.ElementTree as ET
from pathlib import Path

_REPO = Path(__file__).resolve().parent.parent
_DEFAULT_ROOT = _REPO / "input" / "EI docs"
OUT = _REPO / "EI_docs_user_fld_usr_fld.md"

W = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"
PAT = re.compile(r"user_fld|usr_fld", re.IGNORECASE)

# Parameter rows like "AENAM (Changed by):" or "WP_ACTION (Action)" (colon optional)
FIELD_HDR = re.compile(r"^[A-Z0-9_/]{1,35}\s+\([^)]+\):?\s*$")
# USER_FLD glossary / narrative header
USER_FLD_HDR = re.compile(r"^USER_FLD\s*\(", re.I)
# Alternate heading used in some monitors (e.g. password EI)
USER_FLD_ALT_HDR = re.compile(r"^Dynamic Recipient List \(USER_FLD\):?\s*$", re.I)

SECTION_STOP = re.compile(
    r"^Parameter Relationships?\b|"
    r"^How parameter combinations work together\b|"
    r"^Default Values\b|"
    r"^Practical Example of Parameter Configuration\b|"
    r"^USER_FLD Options\b|"
    r"^ENDFUNCTION\b|"
    r"^\*---|"
    r"^EI Function Structure\b",
    re.I,
)


def docx_paragraphs(path: Path) -> list[str]:
    with zipfile.ZipFile(path) as z:
        root = ET.fromstring(z.read("word/document.xml"))
    out: list[str] = []
    for p in root.iter(W + "p"):
        t = "".join(p.itertext()).strip()
        if t:
            out.append(t)
    return out


def _noise(h: str) -> bool:
    x = h.lower().strip().rstrip(":").strip()
    return x in {"user_fld", "user_fld options", "usr_fld"}


def _split_field_blocks(paras: list[str]) -> list[list[str]]:
    cur: list[str] = []
    blocks: list[list[str]] = []
    for t in paras:
        if FIELD_HDR.match(t) and cur:
            blocks.append(cur)
            cur = [t]
        else:
            cur.append(t)
    if cur:
        blocks.append(cur)
    return blocks


def _section_start(t: str) -> bool:
    s = t.strip()
    return bool(USER_FLD_HDR.match(s) or USER_FLD_ALT_HDR.match(s))


def _trim_user_fld_block(lines: list[str]) -> list[str]:
    if not lines:
        return []
    out = [lines[0]]
    for line in lines[1:]:
        if SECTION_STOP.match(line):
            break
        if (
            FIELD_HDR.match(line)
            and not USER_FLD_HDR.match(line.strip())
            and not USER_FLD_ALT_HDR.match(line.strip())
        ):
            break
        out.append(line)
    return out


def _supplemental_assignment_lines(ps: list[str], start_after: int) -> list[str]:
    """Later short lines with USER_FLD= / key=value examples (e.g. tRFC use cases)."""
    add: list[str] = []
    end = min(len(ps), start_after + 100)
    for j in range(start_after, end):
        t = ps[j]
        if not PAT.search(t) or _noise(t):
            continue
        if re.match(r"^USER_FLD Options\b", t.strip(), re.I):
            continue
        if re.search(r"USER_FLD\s*=", t, re.I) or (
            "=" in t and len(t) < 320 and "USER_FLD" in t.upper()
        ):
            add.append(t)
        if len(add) >= 5:
            break
    return add


def explanation_user_fld(paras: list[str]) -> str:
    """
    Prefer the USER_FLD *parameter section* (glossary block or narrative slice),
    not unrelated table cells or code appended after a giant block merge.
    """
    blocks = _split_field_blocks(paras)
    uf_first = [b for b in blocks if b and _section_start(b[0])]

    if uf_first:
        trimmed = _trim_user_fld_block(uf_first[0])
        body = " ".join(trimmed).strip()
        if body:
            return body

    # No glossary-style USER_FLD-first block: slice from first section header in flat paragraphs
    i0 = None
    for i, t in enumerate(paras):
        if _section_start(t):
            i0 = i
            break
    if i0 is None:
        for i, t in enumerate(paras):
            if PAT.search(t) and not _noise(t):
                i0 = i
                break
    if i0 is None:
        return "(no paragraph text extracted)"

    chunk: list[str] = []
    max_lines = 24
    for j in range(i0, min(len(paras), i0 + max_lines)):
        t = paras[j]
        if j > i0 and SECTION_STOP.match(t):
            break
        if (
            j > i0
            and FIELD_HDR.match(t)
            and not USER_FLD_HDR.match(t.strip())
            and not USER_FLD_ALT_HDR.match(t.strip())
        ):
            break
        chunk.append(t)

    if chunk and _section_start(chunk[0]):
        chunk = _trim_user_fld_block(chunk)

    joined = " ".join(chunk).strip()
    if not joined:
        return "(no paragraph text extracted)"

    tail = _supplemental_assignment_lines(paras, i0 + len(chunk))
    tail = [t for t in tail if t and t not in joined]
    if tail:
        joined = f"{joined} {' '.join(tail)}".strip()
    return joined


def main() -> None:
    root = Path(sys.argv[1]).expanduser().resolve() if len(sys.argv) > 1 else _DEFAULT_ROOT
    if not root.is_dir():
        raise SystemExit(f"Not a directory: {root}")

    rows: list[tuple[str, str, str]] = []
    for f in sorted(root.rglob("*.docx")):
        try:
            paras = docx_paragraphs(f)
        except Exception as e:  # noqa: BLE001
            rows.append((f.parent.name, f.name, f"[error reading docx: {e}]"))
            continue
        joined = "\n".join(paras)
        if not PAT.search(joined):
            continue
        rel_parent = f.parent.relative_to(root)
        sub = str(rel_parent) if str(rel_parent) != "." else "(root)"
        expl = explanation_user_fld(paras)
        rows.append((sub, f.name, expl))

    usr_only = False
    for f in root.rglob("*.docx"):
        try:
            raw = zipfile.ZipFile(f).read("word/document.xml").decode("utf-8", errors="ignore")
        except Exception:
            continue
        if re.search(r"usr_fld", raw, re.I):
            usr_only = True
            break

    lines = [
        "# EI docs: files mentioning `user_fld` or `usr_fld`",
        "",
        f"Source folder: `{root}`",
        "",
        "Scan covered all `.docx` files under the folder (recursive).",
        "",
        "Column 3 is the **USER_FLD / Dynamic Recipient List** subsection from each file: "
        "either the glossary block that begins with a `USER_FLD (...)` heading, or a bounded slice "
        "from that heading until a known section boundary (e.g. Parameter Relationship(s), "
        "Default Values, USER_FLD Options), plus short `USER_FLD=` example lines when they follow that section.",
        "",
    ]
    if usr_only:
        lines.append("Both `user_fld` and `usr_fld` appear (case-insensitive) in at least one file.")
    else:
        lines.append(
            "The literal substring `usr_fld` did not appear in any document; "
            "matches are from `user_fld` (case-insensitive)."
        )
    lines.extend(
        [
            "",
            f"Files matched: **{len(rows)}**",
            "",
            "| Sub-folder | File name | Parameter explanation (from document text) |",
            "|---|---|---|",
        ]
    )
    for sub, name, expl in rows:
        esc = expl.replace("|", " · ").replace("\n", " ")
        lines.append(f"| {sub} | {name} | {esc} |")
    OUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Wrote {OUT} with {len(rows)} rows")


if __name__ == "__main__":
    main()
