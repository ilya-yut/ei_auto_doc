# Parameter Relationships – prompt for any EI / function module

Use this prompt to generate **only** the "Parameter Relationships" subsection for an SAP Exception Indicator (EI) document. Provide the four input files (or their contents) under the headings at the end, then send the whole thing to the LLM. In most cases this subsection will be relevant; in rare cases the EI may have no parameters that work together — then omit the subsection entirely.

---

## Role and task

You are documenting the **Parameter Relationships** subsection for an SAP Exception Indicator (EI) / function module. You will receive **four** inputs:

1. **Output structure / fields file** – structure (e.g. Excel or table) listing all output fields of the EI.
2. **Parameters (Name (Description)) file** – the **Parameters** sheet from the Available fields Excel for this EI (same as used for the Parameters Reference Table). Contains the full list of parameters for this function.
3. **ABAP code file** – the function module source code.
4. **Selected parameters file** – reusable explanation text for parameters. Path: `C:\vibe code dev\ei_auto_doc\reference\_Parameters.docx`. **You must search this file** for each parameter that appears in the Parameters (Name (Description)) file; where it describes how parameters work together, connections between parameters, or "Work together / Connection" subsections, use that to identify relationship groups and to align wording (adjust table/field/domain terms to match this EI).

**Your task:** Identify which parameters **actually work together** (relationships visible in code or clear business logic). Output **only** the "Parameter Relationships" subsection — or **omit this subsection entirely** if there are no such relationships. This subsection must **not** list all parameters from the Parameters file; only parameters that have a verified relationship belong here. In typical EIs, only a **minority** of parameters have relationships (roughly 20–30% of the full list at most).

---

## How to discover relationship groups

1. **Search the Selected parameters file (_Parameters.docx):** For every parameter in the Parameters (Name (Description)) file, look up that parameter in _Parameters.docx. Where the file describes how parameters work together, connections between parameters, or "Work together / Connection" (or similar), use that to identify which parameters belong in this subsection and how to describe the relationship. Reuse and adjust wording (table/field names, domain terms) to match this EI's code and logic; do not copy verbatim if the function differs.

2. **Time-related parameters:** Always check all time-related parameters from the Parameters (Name (Description)) file against the ABAP code. In most cases some time parameters will have visible relations (e.g. BACKDAYS + date reference field, DURATION + DURATION_UNIT, date field parameters). Verify usage in the code before including.

3. **Aggregation-related parameters:** Always check aggregation-related parameters against the code. Aggregation may connect to other fields or parameters (e.g. AGGR_PERIOD, AGGR_FIELDS, grouping keys). Verify how they interact in the code.

4. **Other relationship groups:** If the code shows relationships beyond time and aggregation (e.g. partner pairs such as BP1_FUNCT + BP1_CODE, threshold filters such as TOT_CNT + TOT_NETWR, key/structure parameters such as KEY1–KEY10 + TABNAME + CONVERT_KEY), add those groups to this subsection.

5. **Parameter scope:** Reference **only** parameters that appear in the Parameters (Name (Description)) file for this EI. Do not mention parameters that are not in that file.

6. **Serial-number series (e.g. KEY1–KEY10, KEY1_DS–KEY10_DS):** When relevant for a relationship group, describe them **as a group** (e.g. "KEY1–KEY10 work with TABNAME to define the key structure"). Describe individually only if business meaning differs strongly despite similar naming.

7. **Verification:** If your analysis yields **0** relationship groups, double-check the code for time and aggregation usage before omitting the subsection. If it yields relationship groups covering **most** of the parameter list, double-check and keep only parameters with **clear, code-visible** relationships (~20–30% max typical).

---

## Output format (follow exactly)

- **When one or more relationship groups exist:** Output the heading `### Parameter Relationships`, then your content. Use **bold sub-section titles** for each relationship group (e.g. **Time-Based Aggregation Parameters:**, **Aggregation Parameters:**, **Business Partner Analysis Parameters:**). Groupings are free-form and depend on the actual relationships you find. Under each group: bullet lists describing how those parameters work together. Optionally add **Example Configuration:** and **Result:** where it helps. No fixed intro sentence; content is free-form.

- **When no relationships exist:** Do **not** output the "Parameter Relationships" subsection at all (omit it entirely).

---

## Rules

- **Search _Parameters.docx:** For every parameter in the Parameters (Name (Description)) file, search the Selected parameters file (_Parameters.docx) for that parameter. Where the file describes how parameters work together, connections, or "Work together / Connection", use that to identify and describe relationship groups. Adjust wording (table/field/domain terms) to match this EI; do not copy verbatim if the function differs.
- **Only parameters with verified relationships:** Include in this subsection **only** parameters that have a relationship visible in the code, in _Parameters.docx, or clear business logic. Do not list every parameter from the Parameters file.
- **Parameter scope:** Mention **only** parameters that appear in the Parameters (Name (Description)) file for this EI.
- **Verification:** If you end up with **0** relationship groups, re-check the code for time and aggregation usage before omitting. If you end up with relationship groups covering **most** of the parameter list, re-check and reduce to only parameters with clear, code-visible relationships (~20–30% max typical).
- **Serial-number series:** When relevant for a group, describe as a group unless business meaning differs strongly by index.
- **Tone:** Professional. No implementation details (no line numbers, internal function names). Standard SAP names allowed.
- **Non-standard entities:** Avoid mentioning non-standard entities (e.g. Z*, Y*, /SKN/*) unless necessary.
- **Output scope:** When relationships exist, output **only** the "### Parameter Relationships" subsection (heading + content). When none exist, output **nothing** for this subsection. No other sections, no document title, no preamble.

---

## Benchmark reference (structure and level of detail – do not copy content)

Use the **Parameter Relationships** subsection in the benchmark for structure and level of detail only. Infer content for the function you are given from the four inputs.

**Source:** `reference files\Explanation_Credit Memo Monthly volume by Payer_$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.md` (subsection "### Parameter Relationships").

**Structure in benchmark:**
- Heading: `### Parameter Relationships`
- No fixed intro sentence; content is free-form.
- **Bold sub-section titles** for each relationship group (e.g. **Time-Based Aggregation Parameters:**, **Multi-Dimensional Aggregation Parameters:**, **Business Partner Analysis Parameters:**, **Threshold Filtering Parameters:**).
- Under each group: bullet lists describing how those parameters work together.
- Optional **Example Configuration:** and **Result:** per group where it helps.
- Only a subset of the full parameter list appears in this subsection (time, aggregation, partner, threshold groups — not every parameter).

---

## Inputs (provide below)

**1. Output structure / fields file** (path or paste):

[Provide the output structure / fields file path or paste the table content]

**2. Parameters (Name (Description)) file** (path or paste – Parameters sheet from Available fields Excel):

[Provide the Parameters sheet path or paste the parameters table]

**3. ABAP code file** (path or paste):

[Provide the code file path or paste the ABAP source]

**4. Selected parameters file** (reusable explanations):

Path: `C:\vibe code dev\ei_auto_doc\reference\_Parameters.docx`. If the LLM cannot read .docx, provide a text/markdown export of the relevant parameter explanations or paste excerpts that describe how parameters work together.

---

Respond with **only** the "### Parameter Relationships" subsection (heading + content) when relationships exist, or with **nothing** for this subsection when none exist. No other sections or text.
