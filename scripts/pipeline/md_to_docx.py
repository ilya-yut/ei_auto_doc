"""
MD → DOCX conversion for the EI doc pipeline.
Uses markdown → HTML (markdown + tables + fenced_code) → BeautifulSoup → python-docx.
No Pandoc or reference document; formatting is applied in code.
Adapted from ei docs / doc generator scripts 5 / scripts / md_to_docx_converter.py.
"""

from pathlib import Path


def _parse_markdown_to_elements(md_content: str):
    """Parse markdown content into structured elements (HTML soup)."""
    import markdown
    from bs4 import BeautifulSoup

    md = markdown.Markdown(extensions=["tables", "fenced_code"])
    html = md.convert(md_content)
    return BeautifulSoup(html, "html.parser")


def _add_code_block(doc, code_text: str) -> None:
    from docx.shared import Pt, RGBColor

    p = doc.add_paragraph()
    run = p.add_run(code_text)
    run.font.name = "Consolas"
    run.font.size = Pt(9)
    run.font.color.rgb = RGBColor(0, 0, 0)


def _add_table_from_html(doc, table_element) -> None:
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    from docx.shared import RGBColor

    BLACK = RGBColor(0, 0, 0)
    rows_el = table_element.find_all("tr")
    if not rows_el:
        return
    first_row = rows_el[0]
    cols = first_row.find_all(["th", "td"])
    num_cols = len(cols)
    if num_cols == 0:
        return

    table = doc.add_table(rows=len(rows_el), cols=num_cols)
    table.style = "Table Grid"

    for row_idx, row in enumerate(rows_el):
        cells = row.find_all(["th", "td"])
        for col_idx, cell in enumerate(cells):
            if col_idx < num_cols:
                table_cell = table.rows[row_idx].cells[col_idx]
                table_cell.text = cell.get_text(strip=True)
                for paragraph in table_cell.paragraphs:
                    for run in paragraph.runs:
                        run.font.color.rgb = BLACK
                        if cell.name == "th":
                            run.bold = True

    def set_cell_border(cell):
        tc = cell._tc
        tcPr = tc.get_or_add_tcPr()
        tcBorders = OxmlElement("w:tcBorders")
        for edge in ("top", "left", "bottom", "right"):
            edge_el = OxmlElement(f"w:{edge}")
            edge_el.set(qn("w:val"), "single")
            edge_el.set(qn("w:sz"), "4")
            edge_el.set(qn("w:space"), "0")
            edge_el.set(qn("w:color"), "000000")
            tcBorders.append(edge_el)
        tcPr.append(tcBorders)

    for row in table.rows:
        for cell in row.cells:
            set_cell_border(cell)
    doc.add_paragraph()


def _process_inline_formatting(paragraph, element, *, strip_dash: bool = False) -> None:
    from bs4 import NavigableString
    from docx.shared import Pt, RGBColor

    BLACK = RGBColor(0, 0, 0)
    first_text = True

    for child in element.children:
        if isinstance(child, NavigableString):
            text = str(child)
            if strip_dash and first_text and text.lstrip().startswith("- "):
                text = text.lstrip().replace("- ", "", 1)
                first_text = False
            if text.strip():
                run = paragraph.add_run(text)
                run.font.color.rgb = BLACK
        elif child.name in ("strong", "b"):
            text = child.get_text()
            if strip_dash and first_text and text.startswith("- "):
                text = text.replace("- ", "", 1)
                first_text = False
            run = paragraph.add_run(text)
            run.bold = True
            run.font.color.rgb = BLACK
        elif child.name in ("em", "i"):
            run = paragraph.add_run(child.get_text())
            run.italic = True
            run.font.color.rgb = BLACK
        elif child.name == "code":
            run = paragraph.add_run(child.get_text())
            run.font.name = "Consolas"
            run.font.size = Pt(10)
            run.font.color.rgb = BLACK
        elif child.name == "a":
            run = paragraph.add_run(child.get_text())
            run.underline = True
            run.font.color.rgb = BLACK
        else:
            _process_inline_formatting(paragraph, child, strip_dash=strip_dash and first_text)
            first_text = False


def convert_md_to_docx(md_path: Path | str, output_path: Path | str | None = None) -> Path:
    """
    Convert a markdown file to a Word document using python-docx (no Pandoc).
    Requires: python-docx, markdown, beautifulsoup4.
    """
    from docx import Document
    from docx.shared import Pt, RGBColor

    md_path = Path(md_path)
    if not md_path.exists():
        raise FileNotFoundError(f"Markdown file not found: {md_path}")

    out = Path(output_path) if output_path is not None else md_path.with_suffix(".docx")

    with open(md_path, "r", encoding="utf-8") as f:
        md_content = f.read()

    doc = Document()
    BLACK = RGBColor(0, 0, 0)

    style = doc.styles["Normal"]
    style.font.name = "Calibri"
    style.font.size = Pt(11)
    style.font.color.rgb = BLACK

    for i in range(1, 10):
        try:
            doc.styles[f"Heading {i}"].font.color.rgb = BLACK
        except KeyError:
            pass

    soup = _parse_markdown_to_elements(md_content)
    no_bullet_sections = ["problem description", "suggested resolution"]
    current_section = ""

    for element in soup.children:
        if element.name is None:
            continue

        if element.name in ("h1", "h2", "h3", "h4", "h5", "h6"):
            level = int(element.name[1])
            heading_text = element.get_text(strip=True)
            doc.add_heading(heading_text, level=min(level, 4))
            if level == 2:
                current_section = heading_text.lower()

        elif element.name == "p":
            strong_tags = element.find_all("strong")
            if strong_tags and len(strong_tags) == 1:
                full_text = element.get_text(strip=True)
                bold_text = strong_tags[0].get_text(strip=True)
                if full_text.rstrip(":").strip() == bold_text.rstrip(":").strip():
                    p = doc.add_paragraph()
                    run = p.add_run(full_text)
                    run.bold = True
                    run.font.color.rgb = BLACK
                    continue

            if strong_tags:
                first_strong = strong_tags[0]
                full_text = element.get_text()
                bold_text = first_strong.get_text()
                if full_text.strip().startswith(bold_text.strip()):
                    remainder = full_text[len(bold_text) :].strip()
                    if remainder.startswith("(") and ":" in remainder:
                        p = doc.add_paragraph()
                        run = p.add_run(bold_text.strip())
                        run.bold = True
                        run.font.color.rgb = BLACK
                        p.add_run(" " + remainder).font.color.rgb = BLACK
                        continue
                    if not remainder or remainder.startswith("-") or remainder.startswith("•"):
                        p = doc.add_paragraph()
                        run = p.add_run(bold_text.strip())
                        run.bold = True
                        run.font.color.rgb = BLACK
                        if remainder and (remainder.startswith("-") or remainder.startswith("•")):
                            for line in remainder.split("\n"):
                                line = line.strip()
                                if not line:
                                    continue
                                text = line[2:] if (line.startswith("- ") or line.startswith("• ")) else line
                                if current_section in no_bullet_sections:
                                    p = doc.add_paragraph()
                                    p.add_run(text).font.color.rgb = BLACK
                                else:
                                    p = doc.add_paragraph(style="List Bullet")
                                    p.add_run(text).font.color.rgb = BLACK
                        continue

            text_content = element.get_text()
            if "\n- " in text_content or text_content.strip().startswith("- "):
                for line in text_content.split("\n"):
                    line = line.strip()
                    if not line:
                        continue
                    if line.startswith("- "):
                        text = line[2:]
                        if current_section in no_bullet_sections:
                            p = doc.add_paragraph()
                            p.add_run(text).font.color.rgb = BLACK
                        else:
                            p = doc.add_paragraph(style="List Bullet")
                            p.add_run(text).font.color.rgb = BLACK
                    else:
                        p = doc.add_paragraph()
                        p.add_run(line).font.color.rgb = BLACK
            else:
                p = doc.add_paragraph()
                _process_inline_formatting(p, element)

        elif element.name == "ul":
            for li in element.find_all("li", recursive=False):
                if current_section in no_bullet_sections:
                    p = doc.add_paragraph()
                else:
                    p = doc.add_paragraph(style="List Bullet")
                _process_inline_formatting(p, li)

        elif element.name == "ol":
            for li in element.find_all("li", recursive=False):
                p = doc.add_paragraph(style="List Number")
                _process_inline_formatting(p, li)

        elif element.name == "table":
            _add_table_from_html(doc, element)

        elif element.name == "pre":
            code = element.find("code")
            _add_code_block(doc, code.get_text() if code else element.get_text())

        elif element.name == "hr":
            pass

    doc.save(str(out))
    return out
