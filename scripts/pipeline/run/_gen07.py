# Generate 07_response.md EI Function Structure + ABAP Code
import openpyxl
from pathlib import Path
st = Path("input/Structure_Deliveries are blocked due to credit problems_TD_DLV_BLOCK_OBLIGO_SLS_ORG_1200__EI__SW_10_01_DLV_STAT.xlsx")
code_path = Path("input/Code_Deliveries are blocked due to credit problems_TD_DLV_BLOCK_OBLIGO_SLS_ORG_1200__EI__SW_10_01_DLV_STAT.txt")
wb = openpyxl.load_workbook(st, read_only=True, data_only=True)
ws = wb.active
struct_rows = list(ws.iter_rows(min_row=1, max_row=120, values_only=True))
wb.close()
def c(r, i):
    return str(r[i]).strip() if r and len(r) > i and r[i] is not None else ""
header = """## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|"""
lines = [header]
for row in struct_rows[1:]:
    if not row or not c(row, 0):
        continue
    sn = c(row, 0)
    fn = c(row, 1)
    desc = c(row, 2)
    dt = c(row, 3)
    ct = c(row, 4)
    lines.append("| " + sn + " | " + fn + " | " + desc + " | " + dt + " | " + ct + " |")
lines.append("")
lines.append("## ABAP Code")
lines.append("")
lines.append("```abap")
lines.append(code_path.read_text(encoding="utf-8", errors="replace"))
lines.append("```")
Path("scripts/pipeline/run/07_response.md").write_text("\n".join(lines), encoding="utf-8")
print("Wrote 07_response.md")
