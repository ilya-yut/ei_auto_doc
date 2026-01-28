# Copy-paste prompt: Parameters Reference Table section for any EI / function module

Use this in ChatGPT, Claude (web or API), or any LLM. Provide the Parameters (Name (Description)) file — or paste the parameters table — under the heading at the end, then send the whole thing.

---

```
You are documenting the Parameters Reference Table section for an SAP Exception Indicator (EI) / function module. You will receive ONE input: the Parameters (Name (Description)) file — typically the "Parameters" sheet from the Available fields Excel or an equivalent file containing the parameters table.

Your task: Output ONLY the "Parameters Reference Table" subsection for the EI document. Use the fixed intro wording from the benchmark and copy the parameters table from the input file exactly. Do not output a document title, other sections, or preamble.

OUTPUT FORMAT (follow exactly):

### Parameters Reference Table

[Fixed intro — use this wording exactly:]
This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

[Table:]
Add a first column with the parameter number (1, 2, 3, … n). Then copy the parameters table from the input file: use the same column headers and column order as in the input file, with the number column as the first column. If the input has columns Parameter, Description, Type, Length, Decimal, Data Element, Domain, use a markdown table with headers: # (or No.), Parameter, Description, Type, Length, Decimal, Data Element, Domain. Number each parameter row 1 to n. Include every row from the input — the parameter count may be 120–130 or more; do not omit any rows.

Rules:
- Use the fixed intro sentence exactly as stated above. Do not reword or extend it.
- Add a first column with sequential numbers 1, 2, 3, … n for each parameter row. Then copy the parameters table from the input file: same columns (Parameter, Description, Type, Length, Decimal, Data Element, Domain), same order, same row count. Include every parameter row (120–130+ if present).
- Do not add or remove parameter rows; do not infer or generate parameter rows; only copy from the input. The only addition is the number column.
- Output ONLY the subsection: heading "### Parameters Reference Table", the fixed intro paragraph, and the markdown table. No document title, no other sections (e.g. no Parameter Configuration Guidelines, no preamble).
- Tone: professional. The intro is fixed; the table is verbatim from input.

Benchmark format reference (intro + table structure). Source: reference files\Explanation_Credit Memo Monthly volume by Payer_$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.docx

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AGGR_FIELDS | Aggregation Fields |  | 0 | 0 |  |  |
| … | … | (include every row from your input file, numbered 3 to n) |  |  |  |  |  |

Parameters (Name (Description)) file location or paste:
[Provide the file path or paste the parameters table content here]

---

Respond with ONLY the "### Parameters Reference Table" subsection (heading + fixed intro + full parameters table). No other text.
```

---

## How to use

1. Open this file and copy everything **inside** the triple backticks (from "You are documenting…" through "...No other text.").
2. Under **Parameters (Name (Description)) file location or paste:** provide either the file path to the Parameters file (e.g. the "Parameters" sheet from Available fields_*.xlsx) or paste the full parameters table (all columns, all rows — may be 120–130+ rows).
3. Paste the whole block into ChatGPT, Claude (web), or any LLM and send.
4. Use the model's reply as the Parameters Reference Table subsection in your EI document (under ## Parameters).
