"""One-off: split existing output Explanation_*.md back into 01_response.md .. 07_response.md by section headers."""
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent.parent
OUTPUT_DIR = PROJECT_ROOT / "output"
RUN_DIR = Path(__file__).resolve().parent

# Same basename as manifest
basename = "Alternative payee assigned to vendor_200025_001386"
md_path = OUTPUT_DIR / f"Explanation_{basename}.md"
text = md_path.read_text(encoding="utf-8")

# Strip document title (first line and following blank)
if text.startswith("# "):
    first_nl = text.index("\n")
    text = text[first_nl + 1 :].lstrip("\n")

# Section boundaries (header line start -> next section header)
markers = [
    ("## General Overview", "## Problem Description"),
    ("## Problem Description", "### Parameters Reference Table"),
    ("### Parameters Reference Table", "### Parameter Configuration Guidelines"),
    ("### Parameter Configuration Guidelines", "### Parameter Relationships"),
    ("### Parameter Relationships", "### Default Values"),
    ("### Default Values", "## EI Function Structure"),
    ("## EI Function Structure", None),  # to end
]

for i, (start_marker, end_marker) in enumerate(markers):
    num = i + 1
    idx = text.find(start_marker)
    if idx < 0:
        raise SystemExit(f"Marker not found: {start_marker}")
    if end_marker:
        end_idx = text.find(end_marker, idx + 1)
        if end_idx < 0:
            raise SystemExit(f"End marker not found: {end_marker}")
        part = text[idx:end_idx].rstrip()
    else:
        part = text[idx:].rstrip()
    out_path = RUN_DIR / f"{num:02d}_response.md"
    out_path.write_text(part, encoding="utf-8")
    print("Wrote", out_path)

print("Done. Run verify then assemble.")
