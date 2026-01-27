# The Chunker Agent - Cursor Integration (ei_auto_doc)

**Version**: 1.1.0  
**Purpose**: How to invoke the Chunker in Cursor for the **single ABAP function / EI doc** workflow.

---

## When to Run Chunker

Run Chunker **before** THE_DOCUMENTER when:

- The ABAP function code file is **long** (e.g. 2k+ lines), or
- You want a clear “chunks + instructions” input for the Documenter.

Otherwise, give THE_DOCUMENTER the full code file and skip Chunker.

---

## How to Invoke in Cursor

### Option 1: Skill (recommended)

```
/chunk:ei_function input/Code_SW_10_02_200025.txt
```

Or with custom output:

```
/chunk:ei_function input/Code_*.txt --output ./CHUNKS/
```

### Option 2: @-mention + natural language

```
@THE_CHUNKER_AGENT/agent_specification.md
@THE_CHUNKER_AGENT/chunking_strategies.yaml

Chunk this single ABAP function for EI documentation:
- File: input/Code_SW_10_02_200025.txt
- Output: CHUNKS/
- Produce DOCUMENTER_INSTRUCTIONS.md for THE_DOCUMENTER. Use abap_function_ei strategy.
```

### Option 3: Analyze first (no chunking)

```
/chunk:ei_analyze input/Code_*.txt
```

Returns line count, detected sections, and a recommendation (chunk vs use full file).

---

## After Chunker Runs

1. **Check** `CHUNKS/DOCUMENTER_INSTRUCTIONS.md` — it should list chunk IDs, line ranges, and “use for: Params / Defaults / Main logic.”
2. **Run THE_DOCUMENTER** with:
   - **Code input**: either the chunks + DOCUMENTER_INSTRUCTIONS, or the original code file (if you prefer to keep one source of truth, Documenter can be told “code is in CHUNKS/”).
   - Other inputs unchanged: structure, parameter list, sample, parameter reference.

Documenter still follows `.cursor/rules/abap-function-doc-format-authority.md` — sample = format, param reference = parameter text. Chunker only structures the code.

---

## File Locations

- **Agent spec**: `.cursor/Agents/THE_CHUNKER_AGENT/agent_specification.md`
- **Strategies**: `.cursor/Agents/THE_CHUNKER_AGENT/chunking_strategies.yaml`
- **Skills**: `.cursor/Agents/THE_CHUNKER_AGENT/skills.yaml`
- **Documenter spec**: `.cursor/Agents/THE_DOCUMENTER_AGENT/agent_specification.md`
