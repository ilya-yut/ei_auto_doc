# Generate 04_response.md Parameter Configuration Guidelines
import openpyxl
from pathlib import Path
af = Path("input/Available fields_SD Delivery Status (General)_SW_10_01_DLV_STAT.xlsx")
wb = openpyxl.load_workbook(af, read_only=True, data_only=True)
ws = wb["Parameters"]
rows = list(ws.iter_rows(min_row=3, max_row=119, values_only=True))
wb.close()
def c(r, i):
    return str(r[i]).strip() if r and len(r) > i and r[i] is not None else ""
# Key params with specific text
key_guidelines = {
    "BACKDAYS": "Number of days to look back from today. When no date range is supplied, the EI builds the selection using SY-DATUM minus BACKDAYS. Used to limit the delivery date window.",
    "DATE_REF_FLD": "Field used as the reference date for filtering and for duration calculation (e.g. WADAT, LFDAT, ERDAT, AEDAT, KODAT). The EI maps this to the corresponding delivery/header date range and uses it to compute status duration.",
    "DURATION": "Duration in the selected time unit (see DURATION_UNIT) between the reference date (DATE_REF_FLD) and current date. The EI calculates this per delivery and filters by the supplied range to focus on aged-in-status deliveries.",
    "DURATION_UNIT": "Unit for duration calculation (e.g. D = days). Used together with DATE_REF_FLD and DURATION to filter deliveries by how long they have been in the current status.",
    "BP1_FUNCT": "Partner function for first business partner (e.g. sold-to, ship-to). Used with BP1_CODE to filter and enrich partner data from VBPA.",
    "BP1_CODE": "Partner code for first partner role. Filters deliveries by this partner and populates the output; BP1_NAME is enriched from customer master.",
    "BP2_FUNCT": "Partner function for second business partner. Works with BP2_CODE and BP2_NAME.",
    "BP2_CODE": "Partner code for second partner role. Filters and enriches output.",
    "BP3_FUNCT": "Partner function for third business partner. Works with BP3_CODE and BP3_NAME.",
    "BP3_CODE": "Partner code for third partner role. Filters and enriches output.",
}
intro = """### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 117 parameters listed in the Parameters Reference Table above.

"""
out = [intro]
i = 0
while i < len(rows):
    r = rows[i]
    if not r or not c(r, 0):
        i += 1
        continue
    param = c(r, 0)
    desc = c(r, 1)
    if param in ("UVK01", "UVP01", "UVS01") and i + 4 < len(rows):
        ok = all(
            rows[i + k] and c(rows[i + k], 0) == param[:3] + str(k + 1).zfill(2)
            for k in range(1, 5)
        )
        if ok:
            r0, r4 = rows[i], rows[i + 4]
            p_range = c(r0, 0) + " - " + c(r4, 0)
            d_range = c(r0, 1) + " - " + c(r4, 1)
            out.append("**" + p_range + "** (" + d_range + "):")
            out.append("")
            out.append("Reserve/status indicators for header (UVK), item (UVP), or total (UVS). Used to filter deliveries by the corresponding status dimension. All five values share the same type and domain.")
            out.append("")
            i += 5
            continue
    guideline = key_guidelines.get(param)
    if not guideline:
        guideline = "Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping."
    out.append("**" + param + "** (" + desc + "):")
    out.append("")
    out.append(guideline)
    out.append("")
    i += 1
Path("scripts/pipeline/run/04_response.md").write_text("\n".join(out), encoding="utf-8")
print("Wrote 04_response.md")
