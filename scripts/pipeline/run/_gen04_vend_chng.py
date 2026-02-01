"""Generate 04_response.md (Parameter Configuration Guidelines) for Alternative payee assigned to vendor EI.
   Table (03): one row per parameter. Explanatory (04): group KEY1-KEY10, KEY1_DS-KEY10_DS, KEY1_V-KEY10_V into one entry each."""
import openpyxl
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent.parent
INPUT_DIR = PROJECT_ROOT / "input"
RUN_DIR = Path(__file__).resolve().parent

params_path = INPUT_DIR / "Available fields_MD_ Vendors Master General Change Log_SW_10_06_VEND_CHNG.xlsx"

KEY_NAMES = [f"KEY{i}" for i in range(1, 11)]
KEY_DS_NAMES = [f"KEY{i}_DS" for i in range(1, 11)]
KEY_V_NAMES = [f"KEY{i}_V" for i in range(1, 11)]
SERIES_COVERED = set()


def c(r, i):
    return str(r[i]).strip() if r and len(r) > i and r[i] is not None else ""


def in_series(param, series):
    return param in series


def add_block(out, param, desc, text, options=None):
    out.append(f"**{param}** ({desc}):")
    out.append("")
    out.append(text)
    out.append("")
    if options:
        for line in options:
            out.append(line)
        out.append("")


wb = openpyxl.load_workbook(params_path, read_only=True, data_only=True)
ws = wb["Parameters"]
rows = list(ws.iter_rows(min_row=3, max_row=250, values_only=True))
wb.close()

params = []
for r in rows:
    if not r or not c(r, 0):
        continue
    params.append((c(r, 0), c(r, 1)))

N = len(params)
out = []
out.append("### Parameter Configuration Guidelines")
out.append("")
out.append(f"**IMPORTANT:** This section provides configuration guidance for ALL {N} parameters listed in the Parameters Reference Table above.")
out.append("")

BOOLEAN_LIKE = {"CONVERT_KEY", "HEADER_ONLY", "REPETITIVE", "MANAGE_IN_UTC", "CHANGE_IND", "CHNGIND"}
covered = set()

for param, desc in params:
    if param in covered:
        continue
    if in_series(param, KEY_NAMES):
        covered |= set(KEY_NAMES)
        add_block(
            out,
            "KEY1 - KEY10",
            "Field Name - Field Name",
            "Key field names (1–10) for the change document table (TABNAME). Populated when CONVERT_KEY is used; identifies the key components. KEY1_DS–KEY10_DS hold short descriptions; KEY1_V–KEY10_V hold values.",
        )
        continue
    if in_series(param, KEY_DS_NAMES):
        covered |= set(KEY_DS_NAMES)
        add_block(
            out,
            "KEY1_DS - KEY10_DS",
            "Short Description - Short Description",
            "Short descriptions for key fields (1–10). Populated when CONVERT_KEY is used; holds the description of each key component from the change document table.",
        )
        continue
    if in_series(param, KEY_V_NAMES):
        covered |= set(KEY_V_NAMES)
        add_block(
            out,
            "KEY1_V - KEY10_V",
            "Short Description - Short Description",
            "Values for key fields (1–10). Populated when CONVERT_KEY is used; holds the value of each key component (old/new when comparing change document entries).",
        )
        continue
    # Single-parameter entry
    if param == "BACKDAYS":
        add_block(
            out, param, desc,
            "Number of days to look back from the reference date. The EI builds the change document selection window from the reference date minus BACKDAYS.",
        )
    elif param == "REPET_BACKDAYS":
        add_block(
            out, param, desc,
            "Lookback days for repetitive change-document analysis. Used when REPETITIVE is set; defines the window from the repetitive reference date.",
        )
    elif param == "REPETITIVE":
        add_block(
            out, param, desc,
            "When set, the EI uses repetitive-window logic (REPET_BACKDAYS and UDATE_REPET) for change document selection. When not set, standard BACKDAYS logic applies.",
            ["**REPETITIVE Options:**", "", "- **X**: Repetitive window; use REPET_BACKDAYS and UDATE_REPET.", "- ** ** (space): Standard lookback; use BACKDAYS."],
        )
    elif param == "CONVERT_KEY":
        add_block(
            out, param, desc,
            "When set, the EI decomposes the change document key into KEY1–KEY10 (field names), KEY1_V–KEY10_V (values), KEY1_DS–KEY10_DS (descriptions). TABNAME identifies the table.",
            ["**CONVERT_KEY Options:**", "", "- **X**: Decompose key; populate KEY1–KEY10 and related fields.", "- ** ** (space): Do not decompose key."],
        )
    elif param == "HEADER_ONLY":
        add_block(
            out, param, desc,
            "When set, only change document header (no position detail) is returned. When not set, full detail (header and position) is returned.",
            ["**HEADER_ONLY Options:**", "", "- **X**: Header only.", "- ** ** (space): Full detail."],
        )
    elif param == "MANAGE_IN_UTC":
        add_block(
            out, param, desc,
            "When set, dates are managed in UTC. When not set, standard time handling applies.",
            ["**MANAGE_IN_UTC Options:**", "", "- **X**: Manage in UTC.", "- ** ** (space): Standard time."],
        )
    elif param in BOOLEAN_LIKE and param not in ("CONVERT_KEY", "HEADER_ONLY", "REPETITIVE", "MANAGE_IN_UTC"):
        add_block(
            out, param, desc,
            f"Indicator or flag for {desc.lower()}. Filters or populates output accordingly.",
            [f"**{param} Options:**", "", "- **X**: Set/active.", "- ** ** (space): Not set/inactive."],
        )
    else:
        add_block(out, param, desc, f"Parameter for {desc.lower()}. Used in selection criteria or output structure as defined in the function.")

(RUN_DIR / "04_response.md").write_text("\n".join(out), encoding="utf-8")
print("Wrote 04_response.md, entries (grouped):", len([x for x in out if x.startswith("**") and "(" in x and "):" in x]))
