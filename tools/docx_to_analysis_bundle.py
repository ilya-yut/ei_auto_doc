"""Convert EI .docx to paired .md + .json (document order; ABAP from 'ABAP Code' through ENDFUNCTION)."""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from docx import Document
from docx.document import Document as DocumentType
from docx.table import Table
from docx.text.paragraph import Paragraph
from docx.oxml.table import CT_Tbl
from docx.oxml.text.paragraph import CT_P


def iter_block_items(parent: DocumentType):
    body = parent.element.body
    for child in body.iterchildren():
        if isinstance(child, CT_P):
            yield Paragraph(child, parent)
        elif isinstance(child, CT_Tbl):
            yield Table(child, parent)


def _heading_level(paragraph: Paragraph) -> int | None:
    name = (paragraph.style.name or "").strip()
    if not name.lower().startswith("heading"):
        return None
    try:
        return int(name.replace("Heading", "").strip())
    except ValueError:
        return None


def _is_abap_code_heading(text: str) -> bool:
    return text.strip().lower() == "abap code"


def _is_endfunction(text: str) -> bool:
    for line in reversed(text.splitlines()):
        s = line.strip().upper().rstrip(".")
        if not s:
            continue
        return s == "ENDFUNCTION"
    return False


def _starts_function(text: str) -> bool:
    first = text.strip().splitlines()[0].strip() if text.strip() else ""
    return first.upper().startswith("FUNCTION ")


def _table_to_markdown(table: Table) -> str:
    rows: list[list[str]] = []
    for row in table.rows:
        cells = [
            cell.text.replace("\n", " ").replace("|", "\\|").strip()
            for cell in row.cells
        ]
        rows.append(cells)
    if not rows:
        return ""
    width = max(len(r) for r in rows)
    norm = [r + [""] * (width - len(r)) for r in rows]
    header = norm[0]
    lines = [
        "| " + " | ".join(header) + " |",
        "| " + " | ".join("---" for _ in header) + " |",
    ]
    for r in norm[1:]:
        lines.append("| " + " | ".join(r[:width]) + " |")
    return "\n".join(lines)


def _paragraph_to_md_heading(p: Paragraph) -> str | None:
    lev = _heading_level(p)
    if lev is None or lev < 1 or lev > 6:
        return None
    text = p.text.strip()
    if not text:
        return None
    return f"{'#' * lev} {text}"


def convert_docx(path: Path) -> tuple[str, dict]:
    doc = Document(str(path))
    md_parts: list[str] = []
    json_blocks: list[dict] = []

    code_mode = False
    code_buf: list[str] = []

    def flush_code() -> None:
        nonlocal code_buf
        if not code_buf:
            return
        body = "\n".join(code_buf).rstrip() + "\n"
        md_parts.append("")
        md_parts.append("```abap")
        md_parts.append(body.rstrip("\n"))
        md_parts.append("```")
        md_parts.append("")
        json_blocks.append({"kind": "abap", "text": body})
        code_buf = []

    for block in iter_block_items(doc):
        if isinstance(block, Paragraph):
            text = block.text
            stripped = text.strip()

            if code_mode:
                if stripped:
                    code_buf.append(text)
                if stripped and _is_endfunction(text):
                    flush_code()
                    code_mode = False
                continue

            flush_code()

            h = _paragraph_to_md_heading(block)
            if h:
                if _is_abap_code_heading(block.text):
                    md_parts.append("")
                    md_parts.append(h)
                    md_parts.append("")
                    json_blocks.append(
                        {
                            "kind": "heading",
                            "level": _heading_level(block),
                            "text": block.text.strip(),
                        }
                    )
                    code_mode = True
                    code_buf = []
                    continue
                md_parts.append("")
                md_parts.append(h)
                md_parts.append("")
                json_blocks.append(
                    {
                        "kind": "heading",
                        "level": _heading_level(block),
                        "text": block.text.strip(),
                    }
                )
                continue

            if stripped and _starts_function(text) and not code_mode:
                code_mode = True
                code_buf = [text]
                if _is_endfunction(text):
                    flush_code()
                    code_mode = False
                continue

            if stripped:
                md_parts.append(text)
                md_parts.append("")
                json_blocks.append({"kind": "paragraph", "text": text})

        elif isinstance(block, Table):
            if code_mode:
                for row in block.rows:
                    for cell in row.cells:
                        for p in cell.paragraphs:
                            if p.text.strip():
                                code_buf.append(p.text)
                continue
            tmd = _table_to_markdown(block)
            if tmd:
                md_parts.append("")
                md_parts.append(tmd)
                md_parts.append("")
                rows_out: list[list[str]] = []
                for row in block.rows:
                    rows_out.append(
                        [c.text.replace("\n", " ").strip() for c in row.cells]
                    )
                json_blocks.append({"kind": "table", "rows": rows_out})

    flush_code()
    if code_mode:
        json_blocks.append(
            {
                "kind": "error",
                "message": "Unclosed ABAP block (missing ENDFUNCTION)",
            }
        )

    md = "\n".join(md_parts).strip() + "\n"
    meta = {
        "source_file": path.name,
        "converter": "docx_to_analysis_bundle.py v1",
        "blocks": json_blocks,
    }
    return md, meta


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=Path)
    parser.add_argument("output_dir", type=Path)
    args = parser.parse_args()

    inp = args.input_dir.expanduser().resolve()
    out = args.output_dir.expanduser().resolve()
    if not inp.is_dir():
        print(f"Not a directory: {inp}", file=sys.stderr)
        sys.exit(1)
    out.mkdir(parents=True, exist_ok=True)

    docx_files = sorted(inp.glob("*.docx"))
    if not docx_files:
        print(f"No .docx in {inp}", file=sys.stderr)
        sys.exit(1)

    manifest: list[dict] = []
    for src in docx_files:
        md_path = out / f"{src.stem}.md"
        js_path = out / f"{src.stem}.json"
        try:
            md, meta = convert_docx(src)
        except Exception as e:  # noqa: BLE001
            print(f"FAIL {src.name}: {e}", file=sys.stderr)
            manifest.append({"source": src.name, "error": str(e)})
            continue
        md_path.write_text(md, encoding="utf-8")
        js_path.write_text(
            json.dumps(meta, ensure_ascii=False, indent=2) + "\n",
            encoding="utf-8",
        )
        manifest.append(
            {
                "source": src.name,
                "markdown": md_path.name,
                "json": js_path.name,
                "markdown_bytes": md_path.stat().st_size,
            }
        )
        print(f"OK {src.name}")

    (out / "manifest.json").write_text(
        json.dumps(manifest, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )
    print(f"Wrote manifest.json ({len(manifest)} entries)")


if __name__ == "__main__":
    main()
