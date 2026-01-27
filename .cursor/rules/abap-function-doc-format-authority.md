# RULE: ABAP Function / EI Documentation — Format Authority

**Version:** 1.1
**Created:** 2026-01
**Purpose:** When documenting ABAP functions (EI style), the user-provided sample document is the single authority for output structure.

---

## Folder Conventions (Implementation Phase)

- **reference files** — Used only at **planning, verification, and debugging** for temporary files when verifying .cursor. Not used during implementation.

- **input/** — Per-function inputs; add or replace for each next function:
  - Function code (e.g. `Code_*.txt`)
  - Structure (e.g. `Structure_*.xlsx`)
  - Parameter list source: `Available fields_*.xlsx` → sheet **"Parameters"**

- **reference/** — Shared across functions; stays the same:
  - **Sample document** — format authority (e.g. `sample_format.md`)
  - **Parameter reference file** — e.g. `_Parameters.docx`

- **output/** — Generated documentation (e.g. `Explanation_*.md` or `.docx`).

---

## When This Rule Applies

**Task type:** ABAP function (Exception Indicator) documentation from user inputs.

**Inputs (implementation):**
1. From **input/:** Function code, structure, parameter list (Parameters sheet of Available fields_*.xlsx)
2. From **reference/:** Sample document (format authority), parameter reference file

**Output:** One file in **output/** that matches the sample's structure exactly. When a reference output file is provided (e.g. for the same inputs), verification includes comparing generated output to it (structure and, where applicable, content).

---

## Format Authority: User Sample

The **user-provided sample document** (in reference/) is the **single source** for output format.

### Required Behavior

1. **Parse the sample first**
   - Extract section titles and heading levels (e.g. `##`, `###`)
   - Extract section order (no reordering)
   - Extract table shapes (column headers, layout)
   - Note any fixed boilerplate or placeholders (e.g. `[TODO: ...]`)

2. **Produce output that matches**
   - Same sections, same order
   - Same heading hierarchy
   - Same table structures (same columns, no drops or extra columns)
   - No extra sections; no omitted sections

3. **Content sources** (what fills each section)
   - Code file (input/) → ABAP code block(s), logic references
   - Structure file (input/) → Function/structure table(s)
   - Parameter list (input/) → Parameters Reference Table and parameter list in doc
   - Parameter reference file (reference/) → Verbatim text for parameters that appear there; generate only for others
   - EI domain knowledge → General Overview, Problem Description, Suggested Resolution where the sample includes such sections

4. **Scale**
   - The workflow supports **up to 130+ parameters** and **130+ structure fields**.
   - Every parameter and every structure field from the source files must be included in the output (no dropping or truncation).

### Forbidden

- **Do not** add sections not present in the sample
- **Do not** drop or merge sections from the sample
- **Do not** change section order
- **Do not** impose a different template (e.g. fixed EI template) if the sample uses different headings or order

---

## Parameter Text Authority

For **parameter explanations** (e.g. "Parameter Configuration Guidelines", or whatever the sample calls it):

1. **Parameter reference file** (reference/) is the authority for text when a parameter name matches.
2. **If found:** Use that explanation **verbatim** (no rewording, no "improvements").
3. **If not found:** Generate from code analysis and SAP/ABAP domain knowledge.
4. **Never** reference implementation details in parameter narrative (no code line numbers, no custom function/module names in prose).

---

## Where the Sample and Inputs Live

- **Implementation:** Sample and parameter reference in **reference/**; code, structure, and parameter list in **input/**; generated doc in **output/**.
- **Override:** If user specifies other paths, use those. Folder names are conventions; paths and file patterns define the workflow.
- This rule does not require a specific app folder structure beyond the conventions above.

---

## Related Rules

- `.cursor/rules/document-authority.md` — "For ABAP function / EI documentation" hierarchy (sample, then param reference, then code/structure/params)
- `.cursor/rules/anti-hallucination-rules.md` — Verify before claiming; no assumptions as facts
- `.cursor/Agents/THE_DOCUMENTER_AGENT/abap_function_doc_plugin.yaml` — Input files, workflow, format_authority, folder conventions

---

**NO EXCEPTIONS:** For this task type, output structure comes only from the user's sample.
