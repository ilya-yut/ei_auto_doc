import openpyxl
from pathlib import Path
af = Path("input/Available fields_SD Delivery Status (General)_SW_10_01_DLV_STAT.xlsx")
wb = openpyxl.load_workbook(af, read_only=True, data_only=True)
ws = wb["Parameters"]
rows = list(ws.iter_rows(min_row=3, max_row=119, values_only=True))
wb.close()
def c(r, i):
    return str(r[i]).strip() if r and len(r) > i and r[i] is not None else ""
intro = """### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|"""
out = [intro]
i = 0
num = 1
while i < len(rows):
    r = rows[i]
    if not r or not c(r, 0):
        i += 1
        continue
    param = c(r, 0)
    if param in ("UVK01", "UVP01", "UVS01") and i + 4 < len(rows):
        ok = all(
            rows[i + k] and c(rows[i + k], 0) == param[:3] + str(k + 1).zfill(2)
            for k in range(1, 5)
        )
        if ok:
            r0, r4 = rows[i], rows[i + 4]
            out.append(
                "| "
                + str(num)
                + "-"
                + str(num + 4)
                + " | "
                + c(r0, 0)
                + " - "
                + c(r4, 0)
                + " | "
                + c(r0, 1)
                + " - "
                + c(r4, 1)
                + " | "
                + c(r0, 2)
                + " | "
                + c(r0, 3)
                + " | "
                + c(r0, 4)
                + " | "
                + c(r0, 5)
                + " | "
                + c(r0, 6)
                + " |"
            )
            num += 5
            i += 5
            continue
    out.append(
        "| "
        + str(num)
        + " | "
        + c(r, 0)
        + " | "
        + c(r, 1)
        + " | "
        + c(r, 2)
        + " | "
        + c(r, 3)
        + " | "
        + c(r, 4)
        + " | "
        + c(r, 5)
        + " | "
        + c(r, 6)
        + " |"
    )
    num += 1
    i += 1
Path("scripts/pipeline/run/03_response.md").write_text("\n".join(out), encoding="utf-8")
print("Wrote 03_response.md, param rows:", num - 1)
