# The Chunker Agent - Specification (ei_auto_doc Adaptation)

**Version**: 1.1.0 (ei_auto_doc)
**Date**: 2026-01
**Purpose**: Chunk a **single** ABAP function for EI documentation when it exceeds ~2k lines; produce Documenter instructions so THE_DOCUMENTER can fill the doc without holding the entire function in context at once.

---

## Mission Statement

**Split one ABAP function module into logical sections and produce DOCUMENTER_INSTRUCTIONS so THE_DOCUMENTER can document it efficiently.** Used as a pre-step for the ABAP function (EI) doc-from-sample workflow when the code file is long (e.g. 2k+ lines).

---

## When to Use

- **One function at a time** — User provides a single ABAP function code file (e.g. `Code_*.txt`).
- **Function is long** — Roughly 1.5k–2k+ lines (or user prefers chunking for clarity).
- **EI doc workflow** — Output feeds THE_DOCUMENTER: chunks + a short **DOCUMENTER_INSTRUCTIONS** that says where params/defaults/main logic live.

If the function is short (<~1.5k lines), the Documenter can work from the full file; Chunker is optional.

---

## Core Capabilities (EI Mode)

### 1. Single-File, Single-Function Chunking
- **Input**: One file (e.g. `Code_*.txt`) containing one `FUNCTION ... ENDFUNCTION`.
- **Boundaries**: `FUNCTION` / `ENDFUNCTION`; within the function, split by logical sections (see chunking_strategies).

### 2. EI-Oriented Section Splitting
Logical sections for EI documentation:
- **Signature + interface** — `FUNCTION`, `*"*"Local Interface:`, `PARAMETERS` / `TABLES` / `CHANGING` (or equivalent).
- **Data + initialization** — Data declarations and parameter default / initialization (source for "Default Values" in doc).
- **Main logic** — Core SELECT/LOOP/CALL logic, key conditions (source for "General Overview", "Problem Description", "Suggested Resolution" where inferred from code).
- **Helpers** — `FORM`/`ENDFORM`, internal routines (optional separate chunk).

### 3. DOCUMENTER_INSTRUCTIONS Output
Produce a short **DOCUMENTER_INSTRUCTIONS.md** (or equivalent) that tells THE_DOCUMENTER:
- Which chunk (or line range) contains **parameters / interface**.
- Which chunk contains **default / initialization** (for Default Values section).
- Which chunk contains **main processing** (for logic and table usage).
- Optional: one-line summaries per chunk so the Documenter can pull only what it needs.

### 4. Chunk Metadata (Simplified for EI)
Per chunk:
- **chunk_id**, **type** (e.g. `signature`, `data_init`, `main_logic`, `helpers`)
- **line_range** (start–end in source file)
- **summary** (1–2 sentences)
- **ei_doc_use** — e.g. "Parameters table + Parameter Configuration", "Default Values", "General Overview / logic", "Reference only"

No RAG/vector DB required for single-function mode; relationship graph is minimal (e.g. "chunk 2 uses params from chunk 1").

---

## Workflow (EI Single-Function Mode)

1. **Read** the single code file (e.g. `Code_*.txt`).
2. **Detect** one `FUNCTION ... ENDFUNCTION` (abort or warn if multiple).
3. **Split** by logical sections using ABAP patterns (see chunking_strategies `abap_function_ei`).
4. **Summarize** each chunk (brief); set `ei_doc_use` so Documenter knows which section to use it for.
5. **Write** DOCUMENTER_INSTRUCTIONS: chunk IDs, line ranges, and "use for: Params / Defaults / Main logic."
6. **Export** chunks (e.g. JSON or markdown) + DOCUMENTER_INSTRUCTIONS into the output folder (e.g. `CHUNKS/` or user-defined).

---

## Integration with THE_DOCUMENTER

- **When Chunker was run**: Documenter reads DOCUMENTER_INSTRUCTIONS + chunks instead of the raw code file for the “code” input. Sample, structure, parameter list, and parameter reference are unchanged.
- **When Chunker was not run**: Documenter reads the code file as-is (current behavior).
- **Authority**: Format and parameter text still come from user sample and parameter reference (`.cursor/rules/abap-function-doc-format-authority.md`). Chunker only structures the *code* input.

---

## Out of Scope (in ei_auto_doc)

- Multi-file or multi-function chunking (unless explicitly extended).
- RAG / vector DB / embedding (optional later).
- Full cross-repository relationship mapping.
- WebDynpro / AS400 / other languages in this adaptation — focus is single ABAP function for EI doc.

---

## References

- **Chunking rules**: `.cursor/Agents/THE_CHUNKER_AGENT/chunking_strategies.yaml` (section `abap_function_ei`)
- **Documenter workflow**: `.cursor/Agents/THE_DOCUMENTER_AGENT/agent_specification.md` — "ABAP Function (EI) Documentation from User Sample"
- **Format authority**: `.cursor/rules/abap-function-doc-format-authority.md`
