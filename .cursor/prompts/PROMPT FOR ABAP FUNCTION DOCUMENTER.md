# PROMPT FOR ABAP FUNCTION DOCUMENTER

**Use when:** The user wants to document an ABAP function (Exception Indicator) from five inputs and produce one output file that matches a user-provided sample format.

**Implementation folders:** **input/** (code, structure, Available fields — per-function); **reference/** (sample_format, _Parameters — shared); **output/** (generated docs). The *reference files* folder is for planning/verification/debug only, not implementation.

---

## Mandatory Pre-Reads

Before producing ABAP function (EI) documentation:

1. **.cursor/rules/abap-function-doc-format-authority.md** — Sample = format authority; parameter reference = text authority for parameters.
2. **.cursor/rules/document-authority.md** — Section "For ABAP Function / EI Documentation".
3. **.cursor/rules/anti-hallucination-rules.md** — Verify before claiming; no assumptions as facts.
4. **.cursor/Agents/THE_DOCUMENTER_AGENT/abap_function_doc_plugin.yaml** — Input files, workflow, format_authority.

---

## Reminder to Agent

When the user asks for ABAP function (EI) documentation:

- **Format:** Output structure (sections, order, tables) must match the user-provided **sample document** exactly. Parse the sample first; then fill from code, structure, and parameter list.
- **Parameters:** For each parameter from the parameter list, look up the name in the **parameter reference file**. If found, use that text **verbatim**. If not found, generate from code/domain. Never put code line numbers or custom implementation details in parameter narrative.
- **Content sources:** Code file → ABAP block(s); structure file → structure/function table(s); parameter list → Parameters Reference Table and parameter list in doc.
- **Output:** One file only. Same section count and order as the sample; same table column headers where the sample defines tables.
- **Verification:** After writing, read the generated file and confirm it matches the sample's structure and that every parameter and every structure field from the sources is present. Parameter and structure tables can be large (e.g. up to 130 items each); use a count or checklist so none are missed.

---

## Quick Checklist

- [ ] Read the sample and extract section list, heading levels, table specs.
- [ ] Used parameter reference file for verbatim text wherever parameter name matched.
- [ ] Filled all sections from code/structure/params; no invented content.
- [ ] Output has same sections and order as sample; tables match.
- [ ] Every parameter and every structure field from sources is present (parameter/structure tables can be large, e.g. up to 130 items; verify by count or checklist).
- [ ] Read output file after writing to verify.
