# Parameter Configuration Guidelines – prompt for any EI / function module

Use this prompt to generate **only** the "Parameter Configuration Guidelines" subsection for an SAP Exception Indicator (EI) document. Provide the four input files (or their contents) under the headings at the end, then send the whole thing to the LLM.

---

## Role and task

You are documenting the **Parameter Configuration Guidelines** for an SAP Exception Indicator (EI) / function module. You will receive **four** inputs:

1. **Output structure / fields file** – structure (e.g. Excel or table) listing all output fields of the EI.
2. **Parameters (Name (Description)) file** – the **Parameters** sheet from the Available fields Excel for this EI (same as used for the Parameters Reference Table). Contains the full list of parameters for this function.
3. **ABAP code file** – the function module source code.
4. **Selected parameters file** – reusable explanation text for parameters. Path: `C:\vibe code dev\ei_auto_doc\reference\_Parameters.docx`. Search this file by parameter name; if an explanation exists for a parameter, reuse it and adjust only the following to match this function’s code and business logic: (a) table and field names (e.g. VBAK vs EKKO), (b) domain terms (e.g. sales order vs purchase order), (c) examples that refer to other EIs or parameters. Keep the structure and the remainder of the explanation unchanged. Do not copy verbatim if the function differs (e.g. purchase vs sales, different date fields).

**Your task:** For **every** parameter listed in the Parameters (Name (Description)) file, **in the exact order** of that file, produce a configuration guideline entry. Output **only** the "Parameter Configuration Guidelines" subsection. Do not output the Parameters Reference Table, Parameter Relationships, Default Values, Practical Configuration Examples, or any other section.

---

## How to write each parameter

For **each** parameter (in the order of the Parameters file):

1. **Search first in the Selected parameters file** (`_Parameters.docx`): look up the parameter by name.
   - **If an explanation exists:** Copy it and adjust only the following to match this function’s code and business logic: (a) table and field names (e.g. VBAK vs EKKO), (b) domain terms (e.g. sales order vs purchase order), (c) examples that refer to other EIs or parameters. Keep the structure and the remainder of the explanation unchanged. Do not copy verbatim if the function differs (e.g. purchase vs sales, different date fields).
   - **If no explanation exists:** Write the explanation from the function’s **ABAP code** and **business logic**. Use overall **SAP expertise** only when necessary to clarify meaning (e.g. standard tables, domains, transaction codes).

2. **Use the Output structure / fields file:** Consider all fields from this file. Tie each parameter’s semantics to the function’s **output** (e.g. which fields are filtered, aggregated, or populated). If the link is unclear from the code, use SAP expertise to explain.

3. **Parameter references:** In the main explanation and in Connection subsections, reference **only** parameters that appear in the Parameters (Name (Description)) file for this EI. Do not mention parameters that are not in that file (e.g. do not mention BACKDAYS or DATE_REF_FLD if they are not in the Parameters sheet). If the code or Selected parameters file refers to parameters not in the current function’s parameter list, do not use those names; describe behavior using only parameters that exist in the Parameters file.

4. **Structure of each parameter entry:** Every parameter has a **header** and **at least one sub-section** (the main explanation). Some parameters have **one or two additional** sub-sections. So each parameter has **1–3 sub-sections** in total.

   - **Always (1):** Main explanation – what the parameter does, how it is used in this function, which tables/fields it affects, and how it relates to the output. One or more paragraphs.
   - **Optional (2): Possible values / Options** – Include **only** when the parameter has **specific, non-generic** values that users must choose (e.g. DATE_REF_FLD, STATUS, AGGR_PERIOD, DURATION_UNIT, DC_IND). Do **not** add this for generic value parameters (e.g. BUKRS, company code, free-text codes). **Default:** Keep the Options list **concise**: each bullet is the **value key** (e.g. **D**, **C**) plus a **short label** (e.g. "Debtor (customer)", "Creditor (vendor)"). Do **not** put full references (tables, filters, field names) in the Options bullets unless needed for clarity. **Exception 1 — Parameter in Selected parameters file:** If an explanation (including Options) exists in the Selected parameters file for this parameter, follow that file’s structure and level of detail for the Options subsection; the Options list may be longer if the file specifies it. **Exception 2 — Parameter not in Selected parameters file:** Keep Options short (value key + brief label). Add table/filter/field detail in the Options bullets **only when crucial** for a concise, clear explanation; otherwise put that detail in the main explanation above.
   - **Optional (3): Work together / Connection** – Include when this parameter is **designed to work together** with other parameters in most or all cases. Reference **only** parameters from the Parameters (Name (Description)) file (e.g. BACKMONTHS + COMPMONTHS, DURATION + DURATION_UNIT). Explain how they connect and how to use them together. Decide based on the function code, the Selected parameters file, and SAP expertise.

---

## Output format (follow exactly)

Output **strictly** the following. Nothing else.

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL [N] parameters listed in the Parameters Reference Table above.

**[PARAM1]** ([Description from Parameters file]):

[Main explanation – at least one paragraph. Optional: **Possible values / Options** and/or **Work together / Connection** sub-sections if applicable.]

**[PARAM2]** ([Description from Parameters file]):

[Same structure: 1–3 sub-sections.]

[Continue for every parameter in the **exact order** of the Parameters (Name (Description)) file. Do **not** output the Parameters Reference Table, Parameter Relationships, Default Values, Practical Configuration Examples, EI Function Structure, or ABAP Code.]

---

## Rules

- **Order:** Follow the **exact order** of parameters as in the Parameters (Name (Description)) file (same order as in the Parameters Reference Table prompt). Include every parameter; do not skip or merge.
- **Count:** Replace [N] in the IMPORTANT line with the actual number of parameters from the Parameters file.
- **Header format:** For each parameter use bold parameter name and the description in parentheses exactly as in the Parameters file: `**PARAM_NAME** (Description from table):`
- **Sub-sections:** Each parameter has at least the main explanation. Add "Possible values / Options" only for parameters with specific non-generic values. Add "Work together / Connection" only when the parameter clearly works together with others. Use these exact heading patterns: for possible values/options, **[PARAM_NAME] Options:** (e.g. **AGGR_PERIOD Options:**, **DATE_REF_FLD Options:**, **DURATION_UNIT Options:**); for relationship between two parameters, **[PARAM_A] and [PARAM_B] Connection:** (e.g. **BACKDAYS and DATE_REF_FLD Connection:**); for a bullet list of how parameters interact, **How They Work Together:**; for combined examples, **Combined Configuration Examples:** or **Configuration Impact:**. Use the pattern that fits the content; do not invent new heading phrasings.
- **Options brevity:** For **[PARAM_NAME] Options:**, **by default** list only value keys and short explanations (e.g. `- **D**: Debtor (customer)`). Do not put full references (tables, filters, field names) in the Options bullets unless necessary. **If the parameter is documented in the Selected parameters file,** use that file’s structure and level of detail for Options (longer lists allowed). **If the parameter is not in the Selected parameters file,** keep Options short unless a longer Options explanation is **crucial** for clarity.
- **Source priority:** For each parameter, first use the Selected parameters file if an explanation exists; then code and business logic; then SAP expertise only when needed.
- **Tone:** Professional. No implementation details (no internal function names, line numbers, custom namespaces) in the narrative. Standard SAP tables, key fields, and transaction codes are allowed.
- **Non-standard entities:** In the narrative, avoid mentioning non-standard (customer/partner) entities—e.g. names starting with `Z*`, `Y*`, or namespaces like `/SKN/*`, `/ABC4C/*`—unless absolutely necessary to explain the parameter. Prefer standard SAP object names (tables, fields, domains, transaction codes). Mention custom function modules, structures, or namespaces only when the guideline cannot be clear without them.
- **Parameter scope:** In the narrative, mention **only** parameters that appear in the Parameters (Name (Description)) file for this EI. Do not refer to other parameters (e.g. BACKDAYS, DATE_REF_FLD) even if they appear in the ABAP code or in the Selected parameters file. If the code or Selected parameters file refers to parameters not in the current function’s parameter list, do not include those names; describe behavior using only parameters that exist in the Parameters file.
- **No "output only" / "not a filter":** Do not describe any parameter as "output only," "not a filter parameter," "used for output display only," or similar. All parameters in the Parameters (Name (Description)) file can be used for filtering—at the function level (selection criteria) or at frontend/post-processing level on the result set. That distinction is irrelevant; do not state it. Describe what the parameter represents and how it is set or derived; do not classify it as filter vs. non-filter.
- **Currency parameters:** For currency parameters (e.g. WAERS, WAERK, WAERK_FR), describe only **business meaning**: whether the field is document/local currency, foreign currency, transaction currency, or another type. Do not include technical details such as table names, field names, or joins (e.g. do not write "from T001-WAERS" or "populated from the join to T001").
- **Output scope:** Only the "Parameter Configuration Guidelines" subsection. No other sections, no document title, no preamble.

---

## Benchmark reference (structure and style only – do not copy content)

Use the **Parameter Configuration Guidelines** section in the benchmark for structure and level of detail only. Infer content for the function you are given from the four inputs.

**Source:** `reference files\Explanation_Credit Memo Monthly volume by Payer_$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.md` (subsection "### Parameter Configuration Guidelines").

**Structure in benchmark:**
- Intro line: "This section provides configuration guidance for ALL [N] parameters..."
- Each parameter: `**PARAM** (Description):` then 1–3 sub-sections (main; optional Options/Possible values; optional Connection/How they work).
- Simple parameters (e.g. AEDAT, AUART, KUNNR): one main paragraph.
- Parameters with fixed options (e.g. AGGR_PERIOD, DATE_REF_FLD, DURATION_UNIT, DC_IND): main explanation + "Options" with bullet list. **Default:** short bullets (value key + brief label). Longer Options allowed when the parameter comes from the Selected parameters file (follow that file) or when extra detail is crucial for clarity.
- Parameters that work together (e.g. BACKDAYS, DATE_REF_FLD, DURATION, DURATION_UNIT): main + "Connection" / "How They Work Together" / "Combined Configuration Examples" where relevant.

---

## Inputs (provide below)

**1. Output structure / fields file** (path or paste):

[Provide the output structure / fields file path or paste the table content]

**2. Parameters (Name (Description)) file** (path or paste – Parameters sheet from Available fields Excel):

[Provide the Parameters sheet path or paste the parameters table; order of rows defines the order of guidelines]

**3. ABAP code file** (path or paste):

[Provide the code file path or paste the ABAP source]

**4. Selected parameters file** (reusable explanations):

Path: `C:\vibe code dev\ei_auto_doc\reference\_Parameters.docx`. If the LLM cannot read .docx, provide a text/markdown export of the relevant parameter explanations or paste excerpts for parameters that appear in input 2.

---

Respond with **only** the "### Parameter Configuration Guidelines" subsection (heading + IMPORTANT line + all parameter blocks in order). No other sections or text.
