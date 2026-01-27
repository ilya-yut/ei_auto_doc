---
name: abap-function-doc
description: Document ABAP functions (Exception Indicators) from user inputs. Output matches a user-provided sample format exactly. Use when the user provides function code, structure, parameter list, a sample doc, and a parameter reference file.
---

# ABAP Function (EI) Documentation Skill

Use this skill when the user wants to **document an ABAP function (Exception Indicator)** and has provided:

**Implementation folders:**
- **input/** — Per-function: function code (`Code_*.txt`), structure (`Structure_*.xlsx`), parameter list ("Parameters" sheet of `Available fields_*.xlsx`). Add or replace for each next function.
- **reference/** — Shared: sample document (format authority, e.g. `sample_format.md`), parameter reference file (e.g. `_Parameters.docx`). Same for any function.
- **output/** — Generated documentation (e.g. `Explanation_*.md` or `.docx`).

*(The **reference files** folder is for planning/verification/debug only — temporary files when verifying .cursor; not used in implementation.)*

**Output:** One file in **output/** that matches the sample's structure (sections, order, tables) exactly. Parameter and structure tables can be large (e.g. up to 130 parameters, 130 structure fields); every parameter and every structure field from the sources must be included.

---

## Mandatory Pre-Flight

Before writing any ABAP function doc:

1. **Read** `.cursor/rules/abap-function-doc-format-authority.md`
2. **Read** `.cursor/rules/document-authority.md` — "For ABAP Function / EI Documentation"
3. **Parse the user's sample** — extract section titles, levels, order; table column headers and layout
4. **Identify paths** — default: code/structure/parameter list from **input/**; sample and parameter reference from **reference/**; emit to **output/** (or user-specified paths)

---

## Workflow

1. **Parse sample** — Extract section list, heading hierarchy, table shapes. This is the "format spec" the output must satisfy.
2. **Load content** — Read code file, structure Excel, parameter list (e.g. "Parameters" sheet).
3. **Parameter text** — For each parameter name from the list, search the parameter reference file. If found → use that text verbatim. If not found → generate from code/domain (no line numbers, no custom modules in narrative).
4. **Fill sections** — Map content to sections using the sample's section titles and order. Use EI domain logic for General Overview / Problem Description / Suggested Resolution only where the sample includes such sections.
5. **Emit** — Write one file that strictly follows the parsed format. Same section count and order; same table structures.
6. **Verify** — Read the generated file; confirm structure matches sample and that every parameter and every structure field from the sources is present. For large lists (e.g. up to 130 parameters or 130 structure fields), use a count or checklist so none are missed.

---

## Rules to Follow

- **Format authority:** User's sample only. No extra or reordered sections.
- **Parameter authority:** Parameter reference file for verbatim text when name matches.
- **Anti-hallucination:** Only document what exists in code/structure/params; verify after write.
- **No implementation details** in parameter explanations (no "line 77", no "/SKN/..." in narrative).

---

## Related

- **Rule:** `.cursor/rules/abap-function-doc-format-authority.md`
- **Prompt:** `.cursor/prompts/PROMPT FOR ABAP FUNCTION DOCUMENTER.md`
- **Plugin:** `.cursor/Agents/THE_DOCUMENTER_AGENT/abap_function_doc_plugin.yaml`
