# ABAP Function (EI) Documentation Mode — Plan

**Version:** 1.0  
**Date:** 2026-01  
**Scope:** Agents, rules, plans, prompts, skills under `.cursor` only. No app build.

---

## Objective

Add an **ABAP function (EI) documentation from user sample** mode to ei_auto_doc:

- **Inputs:** Function code, structure, parameter list, sample doc (format authority), parameter reference file.
- **Output:** One file that matches the sample's structure exactly.
- **Automation:** AI-only (agent uses rules/skills; no new Python app or script in this phase).

---

## Authority Hierarchy (This Task Type)

1. **User-provided sample** → Format authority (sections, order, tables).
2. **Parameter reference file** → Text authority for parameter explanations.
3. **Code, structure, parameter list** → Content sources.

See: `.cursor/rules/abap-function-doc-format-authority.md`, `.cursor/rules/document-authority.md` ("For ABAP Function / EI Documentation").

---

## Deliverables (Cursor Artifacts Only)

| Item | Location | Status |
|------|----------|--------|
| Format-authority rule | `.cursor/rules/abap-function-doc-format-authority.md` | Done |
| Document-authority branch | `.cursor/rules/document-authority.md` | Done |
| Plan | `.cursor/plans/abap-function-doc-mode.md` | Done |
| Prompt | `.cursor/prompts/PROMPT FOR ABAP FUNCTION DOCUMENTER.md` | Done |
| Skill | `.cursor/skills/abap-function-doc-skill.md` | Done |
| Agent mode | THE_DOCUMENTER `agent_specification.md` | Done |
| Plugin | THE_DOCUMENTER `abap_function_doc_plugin.yaml` | Done |
| Skills (agent) | THE_DOCUMENTER `skills.yaml` | Done |

---

## Workflow (Agent Behavior)

1. **Parse sample** → Section titles, levels, order; table shapes.
2. **Load content** → Code file, structure xlsx, parameter list (e.g. "Parameters" sheet).
3. **Map to sections** → Fill each section from sample using code/structure/params; use parameter reference file for param text where name matches.
4. **Emit** one file that strictly follows the parsed format.

---

## Implementation Folders (Conventions)

- **input/** — Per-function: code, structure, Available fields. Add or replace for each next function.
- **reference/** — Shared: sample_format, _Parameters. Same for any function.
- **output/** — Generated documentation (e.g. Explanation_*.md or .docx).
- **reference files** — Planning/verification/debug only (temporary when verifying .cursor); not used in implementation.

---

## Out of Scope (This Phase)

- Building or wiring any Python/script app.
- Defining input/output folder layout or file naming outside `.cursor`.
- Changing CHUNKER or RECOMMENDER agents.
- Changing existing sap_plugin / as400_plugin for WebDynpro/AS400.
