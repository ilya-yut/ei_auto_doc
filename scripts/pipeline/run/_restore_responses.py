"""Restore 01-07_response.md from existing output Explanation_*.md by splitting on section headings."""
from pathlib import Path

# run/ -> pipeline -> scripts -> ei_auto_doc
PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent.parent
OUTPUT_DIR = PROJECT_ROOT / "output"
RUN_DIR = Path(__file__).resolve().parent
manifest = RUN_DIR / "manifest.txt"
basename = ""
for line in manifest.read_text(encoding="utf-8").splitlines():
    if line.startswith("output_basename="):
        basename = line.split("=", 1)[1].strip()
        break
if not basename:
    raise SystemExit("No output_basename in manifest")
out_md = OUTPUT_DIR / f"Explanation_{basename}.md"
if not out_md.exists():
    raise SystemExit(f"Output file not found: {out_md}")
lines = out_md.read_text(encoding="utf-8").splitlines()
# Skip H1 (line 0) and blank; body starts at line 2
# Section start headings and their line indices (0-based)
starts = []
for i, line in enumerate(lines):
    s = line.strip()
    if s == "## General Overview":
        starts.append((1, i))
    elif s == "## Problem Description":
        starts.append((2, i))
    elif s == "### Parameters Reference Table":
        starts.append((3, i))
    elif s == "### Parameter Configuration Guidelines":
        starts.append((4, i))
    elif s == "### Parameter Relationships":
        starts.append((5, i))
    elif s == "### Default Values":
        starts.append((6, i))
    elif s == "## EI Function Structure":
        starts.append((7, i))
starts.sort(key=lambda x: x[1])
# Extract section content: from starts[j] to starts[j+1]-1 (or end for section 7)
for k in range(7):
    num = k + 1
    start_ln = starts[k][1]
    end_ln = starts[k + 1][1] if k + 1 < len(starts) else len(lines)
    content = "\n".join(lines[start_ln:end_ln]).strip()
    (RUN_DIR / f"{num:02d}_response.md").write_text(content, encoding="utf-8")
print("Restored 01-07_response.md")
