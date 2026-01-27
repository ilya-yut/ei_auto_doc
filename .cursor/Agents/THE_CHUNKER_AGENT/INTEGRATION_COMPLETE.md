# Chunker Agent - ei_auto_doc Integration

**Version**: 1.1.0  
**Status**: Adapted for ei_auto_doc single-function EI mode  
**Date**: 2026-01

---

## Adaptation Summary

The Chunker was adapted from the original multi-file / RAG-oriented design to support **ei_auto_doc**:

- **Scope**: One ABAP function at a time (single code file, e.g. `Code_*.txt`).
- **When**: Function is long (e.g. 2k+ lines) or user explicitly requests chunking.
- **Output**: Chunks + **DOCUMENTER_INSTRUCTIONS.md** so THE_DOCUMENTER can fill the EI doc without loading the full function in context.
- **Strategy**: `abap_function_ei` in chunking_strategies.yaml — sections are signature, data_init, main_logic, helpers, with `ei_doc_use` hints for Params / Defaults / Main logic.

---

## What Changed

| Original | ei_auto_doc Adaptation |
|----------|------------------------|
| Multi-file, multi-function, RAG | Single-file, single-function, Documenter pre-step |
| /chunk [directory] | /chunk:ei_function [path_to_code_file] |
| DOCUMENTER_INSTRUCTIONS for many chunks | DOCUMENTER_INSTRUCTIONS for one function’s sections |
| Vector DB, graph, embedding | Optional; not required for EI mode |
| WebDynpro, AS400, many languages | ABAP function module only (for this mode) |

---

## Integration with THE_DOCUMENTER

- **Documenter** reads `.cursor/Agents/THE_DOCUMENTER_AGENT/agent_specification.md` and `abap_function_doc_plugin.yaml`.
- **Chunker** writes `CHUNKS/DOCUMENTER_INSTRUCTIONS.md` + chunks. Documenter can use “code input = CHUNKS/” when the user ran Chunker first.
- **Authority** for format and parameter text remains: user sample and parameter reference (`.cursor/rules/abap-function-doc-format-authority.md`). Chunker does not change that.

---

## Files in This Folder

- **agent_specification.md** — Mission, when to use, workflow, EI mode.
- **chunking_strategies.yaml** — `abap_function_ei` section layout + ABAP fallback.
- **skills.yaml** — /chunk:ei_function, /chunk:ei_analyze, internal skills.
- **README.md** — Quick start and output description.
- **cursor_integration.md** — How to invoke in Cursor.
- **INTEGRATION_COMPLETE.md** — This file.
