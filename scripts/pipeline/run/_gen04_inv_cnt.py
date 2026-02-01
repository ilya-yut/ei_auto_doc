"""Generate 04_response.md (Parameter Configuration Guidelines) for Inventory count - Single record EI."""
import openpyxl
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent.parent
INPUT_DIR = PROJECT_ROOT / "input"
RUN_DIR = Path(__file__).resolve().parent

params_path = INPUT_DIR / "Available fields_Inventory count - Single record_SW_10_02_INV_CNT_SNG.xlsx"

def c(r, i):
    return str(r[i]).strip() if r and len(r) > i and r[i] is not None else ""

wb = openpyxl.load_workbook(params_path, read_only=True, data_only=True)
ws = wb["Parameters"]
rows = list(ws.iter_rows(min_row=3, max_row=300, values_only=True))
wb.close()

params = []
for r in rows:
    if not r or not c(r, 0):
        continue
    params.append((c(r, 0), c(r, 1)))  # Parameter, Description

N = len(params)
out = []
out.append("### Parameter Configuration Guidelines")
out.append("")
out.append(f"**IMPORTANT:** This section provides configuration guidance for ALL {N} parameters listed in the Parameters Reference Table above.")
out.append("")

# Short main explanation by category; add Options for key params
OPTIONS_PARAMS = {"AGG_LVL", "DURATION_UNIT", "DATE_REF_FLD", "PRESENT_ZERO"}
BOOLEAN_LIKE = {"KWART", "SPERR", "XAMEI", "XBUFI", "XDIFF", "XLOEK", "XNULL", "XNZAE", "XZAEL"}  # CHAR(1) XFELD-like
CONNECTION_PARAMS = {"BACKDAYS", "DATE_REF_FLD", "DURATION", "DURATION_UNIT"}

for param, desc in params:
    out.append(f"**{param}** ({desc}):")
    out.append("")
    # Main explanation (one sentence)
    if param == "AGG_LVL":
        out.append("Aggregation level for count data (e.g. single record vs aggregated). Controls how the EI groups and returns data. Use to switch between item-level and aggregated view.")
        out.append("")
        out.append("**AGG_LVL Options:**")
        out.append("")
        out.append("- Values are function-specific; see code or output structure. Use empty for single-record view or set aggregation key as used by the function.")
        out.append("")
    elif param == "BACKDAYS":
        out.append("Number of days to look back from today. When no date range is supplied, the EI builds the selection using the reference date minus BACKDAYS. Used to limit the physical inventory date window.")
        out.append("")
        out.append("**BACKDAYS and DATE_REF_FLD Connection:**")
        out.append("")
        out.append("DATE_REF_FLD determines which date is used as the reference (e.g. BUDAT, GIDAT, ZLDAT). The selection window is built from that date minus BACKDAYS.")
        out.append("")
    elif param == "DATE_REF_FLD":
        out.append("Name of the date field used as the reference for the time window (e.g. BUDAT = Posting Date, GIDAT = Planned count date, ZLDAT = Count date). Used with BACKDAYS or DURATION/DURATION_UNIT.")
        out.append("")
        out.append("**DATE_REF_FLD Options:** (from code)")
        out.append("")
        out.append("- **BUDAT**: Posting Date in the Document (default in code).")
        out.append("- Other date fields from the output structure (e.g. GIDAT, ZLDAT, BLDAT) may be used depending on function logic.")
        out.append("")
    elif param == "DURATION":
        out.append("Duration in the selected time unit (see DURATION_UNIT) between the reference date and current date. Use with DATE_REF_FLD and DURATION_UNIT when filtering by age in status.")
        out.append("")
        out.append("**DURATION and DURATION_UNIT Connection:**")
        out.append("")
        out.append("DURATION gives the numeric value; DURATION_UNIT gives the unit (e.g. D = days). DATE_REF_FLD determines which date is used as the start. Set all three when filtering by how long documents have been in status.")
        out.append("")
    elif param == "DURATION_UNIT":
        out.append("Unit for duration (e.g. days). Used with DURATION and DATE_REF_FLD for duration-based filtering.")
        out.append("")
        out.append("**DURATION_UNIT Options:** (from code)")
        out.append("")
        out.append("- **D**: Days (default in code).")
        out.append("")
    elif param in BOOLEAN_LIKE:
        out.append(f"Flag or indicator for {desc.lower()}. Filters or populates output accordingly.")
        out.append("")
        out.append(f"**{param} Options:**")
        out.append("")
        out.append("- **X**: Set/active.")
        out.append("- ** ** (space): Not set/inactive.")
        out.append("")
    elif "date" in desc.lower() or "Date" in desc:
        out.append(f"Filters or outputs by {desc.lower()}. Used in selection or output structure.")
        out.append("")
    elif "amount" in desc.lower() or "value" in desc.lower() or "currency" in desc.lower():
        out.append(f"Filters or outputs {desc.lower()}. Used for threshold comparison or value display.")
        out.append("")
    elif "status" in desc.lower() or "Status" in desc:
        out.append(f"Filters or outputs by {desc.lower()}. Used in selection or output.")
        out.append("")
    else:
        out.append(f"Parameter for {desc.lower()}. Used in selection criteria or output structure as defined in the function.")
        out.append("")

(RUN_DIR / "04_response.md").write_text("\n".join(out), encoding="utf-8")
print("Wrote 04_response.md, guidelines:", N)
