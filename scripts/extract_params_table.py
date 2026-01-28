"""Extract Parameters sheet from Available fields Excel and write Parameters Reference Table subsection."""
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
from generate_ei_doc import load_parameters

def main():
    project = Path(__file__).resolve().parent.parent
    avail_path = project / "input" / "Available fields_Exceptional sales documents values – Aggregated_SW_10_01_ORD_VAL_TOT.xlsx"
    if not avail_path.exists():
        print(f"Not found: {avail_path}", file=sys.stderr)
        sys.exit(1)
    params = load_parameters(avail_path)
    header = "| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |"
    sep = "|---|-----------|-------------|------|--------|---------|--------------|--------|"
    rows = []
    for i, x in enumerate(params, 1):
        cells = [
            str(i),
            str(x.get("Parameter", "") or ""),
            str(x.get("Description", "") or ""),
            str(x.get("Type", "") or ""),
            str(x.get("Length", "") or ""),
            str(x.get("Decimal", "") or ""),
            str(x.get("Data Element", "") or ""),
            str(x.get("Domain", "") or ""),
        ]
        rows.append("| " + " | ".join(cells) + " |")
    table = "\n".join([header, sep] + rows)
    intro = """### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

"""
    out = intro + table
    out_dir = project / "test_outputs"
    out_dir.mkdir(exist_ok=True)
    out_file = out_dir / "Parameters_Reference_Table_Exceptional_sales_documents_values.md"
    out_file.write_text(out, encoding="utf-8")
    print(len(params), "parameters written to", out_file)

if __name__ == "__main__":
    main()
