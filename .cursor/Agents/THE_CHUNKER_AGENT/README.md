# The Chunker Agent (ei_auto_doc Adaptation)

**Version**: 1.1.0  
**Purpose**: Chunk a **single** ABAP function when it is long (e.g. 2k+ lines) so THE_DOCUMENTER can document it in the EI-from-sample workflow without holding the entire file in context.

---

## When to Use

- **One function at a time** — You have one ABAP function code file (e.g. `Code_*.txt`).
- **Function is long** — Roughly 1.5k–2k+ lines, or you want a structured pre-step for clarity.
- **EI doc workflow** — You run Chunker first; then THE_DOCUMENTER uses **DOCUMENTER_INSTRUCTIONS** + chunks instead of the raw code file.

If the function is short (<~1.5k lines), you can skip Chunker and give the full code file to THE_DOCUMENTER.

---

## Quick Start

In Cursor, point the agent at this folder and the code file:

```
@THE_CHUNKER_AGENT/agent_specification.md
@THE_CHUNKER_AGENT/chunking_strategies.yaml

Chunk this ABAP function for EI documentation:
- Code file: [path to Code_*.txt, e.g. input/Code_SW_10_02_200025.txt]
- Output: CHUNKS/ (or your folder)
- Produce DOCUMENTER_INSTRUCTIONS.md for THE_DOCUMENTER
```

Or use the skill: **/chunk:ei_function [path_to_code_file]**.

---

## What It Does

1. **Reads** the single code file.
2. **Finds** one `FUNCTION ... ENDFUNCTION` (warns if multiple or none).
3. **Splits** into EI-oriented sections:
   - **signature** — Params/interface (for Parameters table and Parameter Configuration).
   - **data_init** — Data and defaults (for Default Values section).
   - **main_logic** — SELECT/CALL/LOOP (for Overview, Problem, Resolution, table usage).
   - **helpers** — FORMs (reference only).
4. **Writes** **DOCUMENTER_INSTRUCTIONS.md** so THE_DOCUMENTER knows which chunk to use for which part of the doc.
5. **Exports** chunks (e.g. `CHUNKS/chunks.json`) and instructions to the output folder.

---

## Output

- **CHUNKS/DOCUMENTER_INSTRUCTIONS.md** — Tells THE_DOCUMENTER: “Use chunk X for Params, chunk Y for Defaults, chunk Z for main logic.”
- **CHUNKS/chunks.json** (or per-chunk files) — The sections with line ranges and summaries.

THE_DOCUMENTER still gets structure, parameter list, sample, and parameter reference from you; Chunker only structures the **code** input when the function is long.

---

## References

- **Spec**: [agent_specification.md](agent_specification.md)
- **Strategies**: [chunking_strategies.yaml](chunking_strategies.yaml) (section `abap_function_ei`)
- **Documenter**: `.cursor/Agents/THE_DOCUMENTER_AGENT/agent_specification.md` — "ABAP Function (EI) Documentation from User Sample"
- **Format authority**: `.cursor/rules/abap-function-doc-format-authority.md`
