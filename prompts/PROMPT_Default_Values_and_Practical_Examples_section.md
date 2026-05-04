# Default Values and Practical Configuration Examples – prompt for any EI / function module

Use this prompt to generate **only** two subsections of the Parameters section for an SAP Exception Indicator (EI) document: (1) **Default Values**, and (2) **Practical Configuration Examples**. Provide the three input files (or their contents) under the headings at the end, then send the whole thing to the LLM.

---

## Role and task

You are documenting **Default Values** and **Practical Configuration Examples** for an SAP Exception Indicator (EI) / function module. You will receive **three or four** inputs:

1. **Output structure / fields file** – structure (e.g. Excel or table) listing all output fields of the EI.
2. **Parameters (Name (Description)) file** – the **Parameters** sheet from the Available fields Excel for this EI (same as used for the Parameters Reference Table). Contains the full list of parameters for this function.
3. **ABAP code file** – the function module source code.
4. **Additional code (called function)** (optional) – If the pipeline provides this (a path or "Not provided."), it is the source of the function module that this EI calls (e.g. a wrapper calling another FM). When provided, you **must** check **both** the main ABAP code and the additional code for default values; list any explicit defaults from either file.

**Your task:** Produce **two** subsections in order:

1. **Default Values** – **You must always check the ABAP code for default values** before producing this subsection. List (a) every parameter with an **explicit** default in code (assignment before read or when initial), and (b) **initial-runtime parameters** (see below) whenever they appear in the Parameters file—even if the code never assigns a literal before read. Only parameters that appear in the Parameters (Name (Description)) file belong here. If **no** explicit defaults exist **and** none of the initial-runtime parameters appear in the Parameters file, output the subsection with a **short generic sentence** (e.g. "No default values are defined for this EI; all parameters are used as supplied or as initial when not supplied."). If the file contains any initial-runtime parameter, you **cannot** use only that generic sentence—you must document those parameters. **Do not skip the check:** never assume there are no defaults without scanning the code.

2. **Practical Configuration Examples** – **Mandatory.** Produce 1–5 use cases (see rules below) as code blocks with a business-meaning title and a **Purpose:** paragraph per use case (**one blank line** between the title line and **Purpose:**). Each use case must reflect real business scenarios. When the Parameters file contains date-related parameters (e.g. BACKDAYS, DURATION, DURATION_UNIT, DURATION_D, AEDAT, BUDAT, DATE_REF_FLD, UPDDAT, REPET_BACKDAYS), use **a subset** per use case so that business logic stays clear and easily understandable for SAP business/technical professionals; do not use all date-related parameters in a single use case.

---

## How to produce the Default Values subsection

**Mandatory:** You **must always** perform a systematic check of the ABAP code for default values for this EI. Do not output or omit the Default Values subsection without having done this check. Never assume "no defaults" without scanning the code.

1. **Systematic check (required every time):** For every parameter in the Parameters (Name (Description)) file that is read as a **single value** (e.g. via SELECT_SINGLE, DATA_SINGLE, or equivalent), determine:
   - **(a)** Is it **assigned a value before** being read from the caller? (e.g. LV_DATE_REF_FLD = 'BUDAT' before SELECT_SINGLE.)
   - **(b)** Is it **assigned a value when initial**? (e.g. IF LV_BACKDAYS IS INITIAL. LV_BACKDAYS = 1.)
   If **(a)** or **(b)**, include it in Default Values with the literal or described value.

   **Search the code for:** assignments to LV_* variables that correspond to parameters; lines containing "Set default" or "default value"; IF * IS INITIAL followed by an assignment; IF R_DATUM[] IS INITIAL (or similar range) and the block that builds a default date range; SELECT_SINGLE/DATA_SINGLE and any assignment to the same variable before or after. Check the entire code; wrappers that call another function may have fewer defaults in this file, but you must still check. **If "Additional code (called function)" is provided below**, perform the same systematic check on that code as well.

2. **Initial-runtime parameters (mandatory when listed in the Parameters file):** The following parameter names are **always** documented in Default Values **if they appear** in the Parameters sheet for this EI, **even when** the ABAP does not assign a value before read and the technical value is only **initial** (0 or blank): **BACKDAYS**, **DURATION**, **DURATION_UNIT**, **AGGLEVEL**. For each, output **exactly one** bullet in this **fixed shape** (markdown uses a leading hyphen; Word may render it as a bullet dot):

   `- **PARAM** - initial - treated as <value> by code`

   Use a **hyphen**, **en dash (–)**, or **em dash (—)** between `initial` and `treated` if you need to match house style; the pipeline treats them the same. `<value>` is what the ABAP (main or **called** function) effectively uses when the caller leaves the parameter initial—e.g. numeric **1**, **empty**, or literal **M**. **Do not** add long prose, parentheses, semicolons, or multi-sentence explanations after `by code`—only the value and the minimal “treated as … by code” clause. **Exception to “initial-only”:** these four are listed even when the wrapper assigns no literal, because behavior is defined in the callee or selection framework.

   **Clarity rules:** Infer `<value>` from **main + called** code. Do **not** use vague-only phrases (“framework standard lookback”, “template default”, etc.).

   - **BACKDAYS:** e.g. `- **BACKDAYS** - initial - treated as 1 by code`

   - **DURATION:** e.g. `- **DURATION** - initial - treated as unconstrained by code`

   - **DURATION_UNIT:** e.g. `- **DURATION_UNIT** - initial - treated as D by code`

   - **AGGLEVEL:** e.g. `- **AGGLEVEL** - initial - treated as initial by code`

3. **Other parameters:** For any parameter **not** in the initial-runtime list above, apply the usual rule: include only if **(a)** or **(b)** in step 1 applies. Do **not** add extra `initial - treated as …` bullets for other parameters solely because they are initial.

4. **Format (strict):** Bullet list.
   - **Explicit defaults:** `- **PARAM** - value` (value only; no long explanation).
   - **Initial-runtime (step 2):** `- **PARAM** - initial - treated as <value> by code` (see step 2)
   - Do **not** use `?`, `— Default:`, or markdown code ticks around values in Default Values bullets.
   - Avoid line numbers and internal routine names unless essential.

5. **Optional Note:** Omit **Note:** unless absolutely necessary; if present, keep it to **one short sentence** (no multi-clause prose).

6. **If nothing to list:** If there are **no** parameters satisfying step 1 **and** **none** of BACKDAYS / DURATION / DURATION_UNIT / AGGLEVEL appear in the Parameters file, output **### Default Values** and **one short generic sentence** (e.g. "No default values are defined for this EI."). If any of those four names appear in the Parameters file, you **must** include their initial-runtime bullets—you may not use only the generic sentence.

---

## How to produce the Practical Configuration Examples subsection

1. **Mandatory.** This subsection must always be present with at least one use case.

2. **Minimum and varied parameter count per example:** Each practical configuration example must include **at least 2 parameters** in its code block. Do not produce an example with only one parameter. **In addition, at least one use case (and preferably more than one) must include 3–5 parameters** in its code block; do not produce only 2-parameter examples for every use case. Vary parameter count across use cases: some examples with 2 parameters are allowed, but some must have 3, 4, or 5 parameters so readers see richer configurations.

3. **Number of use cases:**
   - **Very few parameters** (e.g. only a counter or 1–2 parameters): **1–2** use cases.
   - **Multiple parameters** (typical EI): **3–4** use cases.
   - **Many very specific parameters** (aggregation, pseudo-SQL fields, fancy date relationships): **4–5** use cases.

4. **Date-related parameters:** When the Parameters file contains multiple date-related parameters (e.g. BACKDAYS, DURATION, DURATION_UNIT, DURATION_D, AEDAT, BUDAT, DATE_REF_FLD, UPDDAT, REPET_BACKDAYS, BACKMONTHS, COMPMONTHS), **do not use them all in one use case**. Use **a subset** per use case so that business logic stays clear and easily understandable for professionals in the respective SAP business/technical domain. Spread date-related parameters across use cases; in each use case include only those that fit the scenario. Do not invent parameters that are not in the Parameters file.

5. **DURATION_UNIT = F:** If **DURATION_UNIT** exists among the function’s parameters (Parameters file), **at least one** practical configuration example must include **DURATION_UNIT = F** (full days for specific day filtering) among its parameters. In that example, **DURATION** must be a **single value** (e.g. `DURATION = 30`), not a range (e.g. not `DURATION = 0–30` or `DURATION = 30–999999`).

6. **Business meaning:** For each use case, choose a **clear business scenario** (e.g. "Monthly High-Value Sales Monitoring", "Weekly Customer Sales Pattern Analysis"). The **Purpose:** paragraph must explain what the configuration achieves in business terms and when it is useful, on its own line after a blank line following the use case title.

7. **Parameter scope:** Use **only** parameters that appear in the Parameters (Name (Description)) file for this EI. Do not reference parameters not in that file.

8. **Format per use case:**
   - **Use Case N: [Business-meaning title]** (end the line after the closing `**`; do not continue on the same line.)
   - One **blank line** (empty line).
   - **Purpose:** One or two sentences on the **next** line describing the business scenario and what the configuration achieves (**before** the code block). Do **not** put `**Purpose:**` on the same line as the use case title.
   - Code block with one parameter per line: `PARAM = value` or `PARAM = low - high` for ranges. For range parameters (e.g. low–high), use a hyphen: **`PARAM = low - high`** (e.g. `SMRATIO = 0 - 0.5`), not `PARAM = low high`. Optionally a short inline comment.

---

## Output format (follow exactly)

Output **strictly** the following, in this order. Nothing else. For each subsection use **only** the `###` heading shown—do **not** repeat the same title as `##` underneath (that duplicates the heading in Word).

### Default Values

- **PARAM1** - value
- **BACKDAYS** - initial - treated as 1 by code
- **AGGLEVEL** - initial - treated as initial by code

**Note:** (Optional; only if useful.)

*(If no explicit defaults and none of BACKDAYS, DURATION, DURATION_UNIT, AGGLEVEL appear in the Parameters file, output ### Default Values followed by one short sentence, e.g. "No default values are defined for this EI.")*

### Practical Example of Parameter Configuration

**Use Case 1: [Business-meaning title]**

**Purpose:** [One or two sentences: business scenario and what the configuration achieves.]
```
PARAM1 = value
PARAM2 = value
...
```

**Use Case 2: [Business-meaning title]**

**Purpose:** ...
```
...
```

*(Continue for 1–5 use cases depending on parameter complexity. This subsection is mandatory; output at least one use case.)*

---

## Rules

- **Default Values (always check):** You **must always** scan the ABAP for explicit defaults (assignment before read or when initial). **Additionally**, for **BACKDAYS**, **DURATION**, **DURATION_UNIT**, and **AGGLEVEL**, if each appears in the Parameters file, you **must** use the **fixed** bullet shape from step 2: `initial - treated as <value> by code`. Do **not** add long explanations, parentheses, or internal ABAP symbols in Default Values. For **all other** parameters, do not add initial-runtime bullets unless step 1 (a)/(b) applies. If there are no explicit defaults **and** none of those four names are in the Parameters file, output the heading plus one short “no defaults” sentence. Never skip the code scan.
- **BACKDAYS vs section 04:** Section **04** must contain the **verbatim** BACKDAYS monitoring-window sentence from the Parameter Configuration Guidelines prompt, and an anchor line **only** when that prompt allows it (see Parameter Configuration Guidelines). **Default Values** only has the `- **BACKDAYS** - …` bullet; do not paste the section 04 verbatim lines into section 06.
- **Practical Examples:** This subsection is **mandatory.** Output at least one use case; 1–5 use cases depending on parameter count and complexity (see above). **Each example must include at least 2 parameters**; do not produce an example with only one parameter. **At least one use case (and preferably more) must include 3–5 parameters** in its code block; do not produce only 2-parameter examples for every use case. Every use case must have a business-meaning title, then **one blank line**, then **Purpose:** on its own line, then the fenced parameter block (pipeline verify requires the blank line before **Purpose:**).
- **Date parameters:** When the Parameters file has multiple date-related parameters (e.g. BACKDAYS, DURATION, DURATION_UNIT, DURATION_D, AEDAT, BUDAT, DATE_REF_FLD, UPDDAT, REPET_BACKDAYS), do not use them all in one use case. Use a subset per use case so business logic stays clear and easily understandable for SAP business/technical professionals.
- **DURATION_UNIT = F:** If **DURATION_UNIT** exists in the Parameters file, **at least one** practical configuration example must have **DURATION_UNIT = F** among its parameters (full days for specific day filtering). In that example, **DURATION** must be a single value (e.g. `DURATION = 30`), not a range.
- **Parameter scope:** Mention **only** parameters that appear in the Parameters (Name (Description)) file for this EI.
- **Range format:** For parameters that accept a range (low–high), write **`PARAM = low - high`** (e.g. `SMRATIO = 0 - 0.5`), not `PARAM = low high`.
- **Tone:** Professional. No implementation details (no line numbers, internal function names). Standard SAP names allowed.
- **Non-standard entities:** Avoid mentioning non-standard entities (e.g. Z*, Y*, /SKN/*) unless necessary.
- **Output scope:** Only the two subsections (Default Values: explicit defaults plus mandatory `initial - treated as … by code` bullets for BACKDAYS/DURATION/DURATION_UNIT/AGGLEVEL when in Parameters, or one short "no default values" sentence when none apply; **### Practical Example of Parameter Configuration** always). No other sections, no document title, no preamble.

---

## Benchmark reference (structure and level of detail – do not copy content)

Use the **Default Values** and **Practical Configuration Examples** subsections in the benchmark for structure and level of detail only. Infer content for the function you are given from the three inputs. (In the benchmark file the first subsection is titled "Default Values and Parameter Options Explicitly Stated in EI Code"; in your output use the heading **### Default Values**.)

**Source:** `reference files\Explanation_Credit Memo Monthly volume by Payer_$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.md` (subsections for default values and practical examples).

**Structure in benchmark:**
- **Default Values:** Bullets `- **PARAM** - value` for explicit code defaults; for BACKDAYS, DURATION, DURATION_UNIT, AGGLEVEL (when in Parameters), add `- **PARAM** - initial - treated as <value> by code`. Optional **Note:** (one short sentence only). No line numbers. Generic “no defaults” sentence only when no explicit defaults and none of those four parameters appear in the Parameters file.
- **Practical Examples:** **Use Case N: [Title]**, blank line, **Purpose:** paragraph, then code block (PARAM = value per line). Subsection heading in pipeline output: **### Practical Example of Parameter Configuration**.

---

## Inputs (provide below)

**1. Output structure / fields file** (path or paste):

[Provide the output structure / fields file path or paste the table content]

**2. Parameters (Name (Description)) file** (path or paste – Parameters sheet from Available fields Excel):

[Provide the Parameters sheet path or paste the parameters table]

**3. ABAP code file** (path or paste):

[Provide the code file path or paste the ABAP source]

**4. Additional code (called function)** (path or paste – only if this EI calls another FM whose source is provided):

[Additional code (called function) path or paste - replaced by prepare]

---

Respond with **only** the two subsections in order: (1) "### Default Values" — explicit defaults plus mandatory `initial - treated as … by code` bullets for BACKDAYS, DURATION, DURATION_UNIT, AGGLEVEL when each appears in the Parameters file; otherwise one short sentence if nothing applies; (2) "### Practical Example of Parameter Configuration" (mandatory, 1–5 use cases). No other sections or text.
