# Verification prompt: 06_response.md (Default Values and Practical Configuration Examples)

Use this prompt to **verify** an existing `06_response.md` file and **fix** the Practical Configuration Examples subsection so that parameter-count rules are met. The output is an updated `06_response.md` (or the two subsections only) that complies with the section‑6 rules.

---

## Role and task

You are verifying and fixing the **Practical Configuration Examples** part of `06_response.md`. You receive the file (path or pasted content).

**Your task:**

1. **Parse** the "### Practical Configuration Examples" subsection and **count** how many parameters (lines like `PARAM = value`) appear in **each** use case’s code block.
2. **Check** the rules:
   - **Minimum per use case:** Each use case code block must contain **at least 2 parameters**. If any use case has 0 or 1 parameter, that is a violation.
   - **At least one rich use case:** **At least one** use case must contain **3–5 parameters** (or more) in its code block. If every use case has only 2 parameters, that is a violation.
3. **Fix** violations:
   - If a use case has fewer than 2 parameters: add 1–2 more parameter lines to that use case’s code block (choose parameters from the same EI’s Parameters file that fit the use case’s business scenario). Keep the **Purpose:** paragraph and title; only extend the code block.
   - If no use case has 3–5 (or more) parameters: either **(a)** add 1–3 more parameter lines to an existing use case so it has 3–5 parameters (preferred), or **(b)** add a new use case whose code block has 3–5 parameters, with a business-meaning title, **one blank line**, then **Purpose:** on its own line, then the code block. Use only parameters from the Parameters (Name (Description)) file for this EI; do not invent parameters.
4. **Output** the **full** corrected content: first "### Default Values" (unchanged unless you need to fix something there; if the subsection contains only a short "no default values" sentence, leave it as is; if the input had no Default Values subsection, add "### Default Values" with one short sentence, e.g. "No default values are defined for this EI."), then "### Practical Configuration Examples" with all use cases in the same order, with corrected code blocks so that: every use case has ≥ 2 parameters, and at least one use case has 3–5 (or more) parameters.

---

## Rules (must be satisfied after fix)

- **Each use case code block:** at least **2** parameter lines (`PARAM = value` or range).
- **At least one use case:** **3–5** (or more) parameter lines in its code block.
- **Parameter scope:** Only parameters that appear in the Parameters (Name (Description)) file for this EI. If you do not have that file, add parameters that are plausible for the EI (e.g. BACKDAYS, DURATION, DURATION_UNIT, company code, purchasing org, plant, vendor, date ranges).
- **Format:** Preserve the structure: **Use Case N: [Title]**, **one blank line**, then **Purpose:** paragraph on the next line, then the code block (one parameter per line). Pipeline verify requires that blank line between title and **Purpose:**. Do not remove or reorder use cases except when adding one.
- **DURATION_UNIT = F:** If the original prompt required at least one example with DURATION_UNIT = F and single-value DURATION, keep that in the corrected output if it was present; otherwise do not add it unless the Parameters file contains DURATION_UNIT and DURATION.

---

## Output format

Output the **entire** corrected 06 content:

1. **### Default Values** (copy from input unless fixes are required). When the Parameters file includes **BACKDAYS**, **DURATION**, **DURATION_UNIT**, or **AGGLEVEL**, each bullet must match: `- **PARAM** - initial - treated as <value> by code` (hyphen/en dash/em dash allowed between `initial` and `treated`). **No long prose:** do not add parentheses, semicolons, or multi-sentence explanations after `by code`. Rewrite any legacy `initial — …` lines into that shape. If input had no Default Values subsection, output the heading and one short sentence, e.g. "No default values are defined for this EI.".
2. **### Practical Configuration Examples** with all use cases; fix only the code blocks (and add one use case if needed) so that:
   - Every use case has ≥ 2 parameters.
   - At least one use case has 3–5 (or more) parameters.

No document title, preamble, or other sections. If the input contained only the two subsections, output only the two subsections.

---

## Input

**06_response.md** (path or paste):

[Provide the path to `scripts/pipeline/run/06_response.md` or paste the full content below.]

---

Respond with the **full** corrected content of the Default Values and Practical Configuration Examples subsections only. No other sections or commentary.
