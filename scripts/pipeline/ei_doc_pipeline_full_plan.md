---
name: EI Doc Pipeline full plan
overview: Full pipeline (Option B, no API) for EI documentation: verification of all four input files including stem match, prepare (discovery + 7 prompts), one Cursor interaction, assemble (one .md + .docx). All scripts and Cursor instruction in scripts/pipeline.
todos: []
isProject: true
---

# EI Doc Pipeline – Full Plan

## 1. Goal

- **Option B, no API:** Script does discovery, prompt preparation, assembly, and .docx conversion; Cursor generates the 7 section texts.
- **One batch file:** Run prepare → pause → user sends Cursor instruction → run assemble.
- **Verification first:** Before prepare, validate all four input files (presence, readability, stem match across all 4, structure names in code present in Structure xlsx). Fail with clear messages unless `--skip-verify`.

Paths: **input** = `input/`, **output** = `output/`, **prompts** = `prompts/`, **run** = `scripts/pipeline/run/`, **reference .docx** = `reference files/Explanation_Credit Memo Monthly volume by Payer _$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.docx`.

---

## 2. Folder and files (all under `scripts/pipeline/`)

| File | Purpose |
|------|--------|
| scripts/pipeline/pipeline.py | Main script: `verify()`, `prepare`, `assemble`. Run from repo root: `python scripts/pipeline/pipeline.py prepare [--skip-verify]` or `assemble`. |
| scripts/pipeline/run_pipeline.bat | Runs prepare → pause (with instructions) → assemble. |
| scripts/pipeline/CURSOR_INSTRUCTION.txt | Exact wording to send in Cursor after prepare (generate 7 sections, write to run/01_response.md … 07_response.md). |
| scripts/pipeline/run/ | Created by script. Holds `01_prompt.txt`…`07_prompt.txt`, `01_response.md`…`07_response.md`, `manifest.txt`. |

---

## 3. Verification (before prepare)

Verification runs at the start of `prepare()` unless `--skip-verify` is passed. It applies to **all four** required input files: **Metadata_*.xlsx**, **Available fields_*.xlsx**, **Structure_*.xlsx**, **Code_*.txt**.

### 3.1 Presence and cardinality

- Exactly **one** file matching each prefix in `input/`.
- If zero or more than one for any prefix: exit with message listing what is missing or duplicated.

### 3.2 Filename stem match (all four files)

- **Stem** = the part after the prefix (e.g. `Metadata_XYZ.xlsx` → `XYZ`, `Code_XYZ.txt` → `XYZ`).
- **Rule:** All four files must share the **same stem**.
  - Metadata: `Metadata_<stem>.xlsx`
  - Available fields: `Available fields_<stem>.xlsx`
  - Structure: `Structure_<stem>.xlsx`
  - Code: `Code_<stem>.txt`
- If any stem differs: exit with message e.g. "All four input files must have the same name after the prefix. Found: Metadata_... , Available fields_... , Structure_... , Code_... ."

### 3.3 Per-file checks

| File | Checks |
|------|--------|
| **Metadata_*.xlsx** | First sheet has row 8 col B (Exception indicator ID) and row 9 col B (Exception indicator name) readable. |
| **Available fields_*.xlsx** | Sheet "Parameters" exists; at least one data row. |
| **Structure_*.xlsx** | First sheet has header row and at least one data row; a column like "Structure Name" exists. |
| **Code_*.txt** | Non-empty, readable text. |

### 3.4 Cross-file consistency (Code and Structure)

- **Structure name in code present in Structure xlsx:** Extract structure names from ABAP (e.g. `STRUCTURE /SKN/...` or `STRUCTURE /ABC4C/...` from interface block; filter to names containing `/` to skip e.g. RSSELECT). Read "Structure Name" column from Structure xlsx (unique values). Every such structure name from the code must appear in that column. If any is missing: "Structure file does not match code: the code references structure(s) ... which are not listed in the Structure file."

### 3.5 On failure / bypass

- **On failure:** Print "Verification failed:" then each error. Print "Fix the issues above or re-run with --skip-verify to ignore." Exit 1. Do not run prepare.
- **--skip-verify:** `prepare --skip-verify` skips verification and runs prepare.

---

## 4. Section order and prompt mapping

| # | Response file | Section(s) | Prompt template (in `prompts/`) |
|---|----------------|------------|----------------------------------|
| 01 | 01_response.md | General Overview | PROMPT_General_Overview_section.md |
| 02 | 02_response.md | Problem Description + Suggested Resolution | PROMPT_Problem_Description_and_Suggested_Resolution_section.md |
| 03 | 03_response.md | Parameters Reference Table | PROMPT_Parameters_Reference_Table_section.md |
| 04 | 04_response.md | Parameter Configuration Guidelines | PROMPT_Parameter_Configuration_Guidelines_section.md |
| 05 | 05_response.md | Parameter Relationships | PROMPT_Parameter_Relationships_section.md |
| 06 | 06_response.md | Default Values + Practical Examples | PROMPT_Default_Values_and_Practical_Examples_section.md |
| 07 | 07_response.md | EI Function Structure + ABAP Code | PROMPT_EI_Function_Structure_and_ABAP_Code_section.md |

Assembly order: **Title (from Metadata)** then **01 → 02 → … → 07** concatenated.

---

## 5. Prepare mode

1. **Verify** (unless `--skip-verify`): run all checks in section 3; on failure exit 1.
2. **Clear previous run:** Before writing new files, clear `run/`: delete any existing `01_response.md` … `07_response.md` (so old section results are never reused). Overwrite `01_prompt.txt` … `07_prompt.txt` and `manifest.txt` with new content. This ensures each run starts from a clean state.
3. **Discover** paths in `input/` (Code_*.txt, Structure_*.xlsx, Available fields_*.xlsx, Metadata_*.xlsx; optional _Parameters.docx). After verification, exactly one of each is guaranteed (same stem).
4. **Read Metadata:** first sheet, row 8 col B = Exception indicator ID, row 9 col B = Exception indicator name. Output basename = stem of Metadata filename (e.g. `Metadata_My EI_123.xlsx` → `My EI_123`).
5. **Write** `run/manifest.txt`: `output_basename=<value>`, `title=Exception Indicator: <name> - <id>`.
6. **For each of 7 sections:** read prompt template from `prompts/`, replace placeholder lines (e.g. `[Provide the structure file path...]`) with absolute paths to discovered files, write `run/01_prompt.txt` … `run/07_prompt.txt`.

---

## 6. Assemble mode

1. Read `run/manifest.txt` → output_basename, title.
2. Read `run/01_response.md` … `run/07_response.md` in order. If any missing, exit with list of missing files.
3. Build one string: `# <title>\n\n` + content of 01 + … + 07.
4. Write `output/Explanation_<output_basename>.md`.
5. Convert to `output/Explanation_<output_basename>.docx` using reference .docx (e.g. pandoc `--reference-doc`). If pandoc missing, print message and skip .docx.

---

## 7. Batch file (`run_pipeline.bat`)

1. `cd` to repo root (e.g. `C:\vibe code dev\ei_auto_doc`).
2. Run `python scripts\pipeline\pipeline.py prepare`.
3. If errorlevel ne 0, exit.
4. Print: open `scripts\pipeline\CURSOR_INSTRUCTION.txt`, send the instruction in Cursor; when 7 response files are in `scripts\pipeline\run\`, press any key.
5. `pause`.
6. Run `python scripts\pipeline\pipeline.py assemble`.
7. Print "Done. Output: output\."

Optional: a second batch or argument to run `prepare --skip-verify` for force run.

---

## 8. Copy-paste instructions for Cursor (exact wording for each step)

Use these phrases in Cursor for each step. Copy-paste the block for the step you are on.

---

### Step 1 – Run prepare (verify input + write prompts with updated links)

Copy-paste this to Cursor:

```
Run the EI doc pipeline prepare step: execute the script that verifies the four input files in the input folder, then writes the seven prompt files to scripts/pipeline/run/ with the links to the input documents updated. From the project root run: python scripts/pipeline/pipeline.py prepare
```

---

### Step 2 – Generate the 7 sections (run the prompts in Cursor)

Copy-paste this to Cursor after Step 1 has finished (or open scripts/pipeline/CURSOR_INSTRUCTION.txt and send its content):

```
Generate the 7 EI documentation sections and write each to a file. Do not add a document title or preamble.

1. Read each prompt file from this project in order:
   - scripts/pipeline/run/01_prompt.txt
   - scripts/pipeline/run/02_prompt.txt
   - scripts/pipeline/run/03_prompt.txt
   - scripts/pipeline/run/04_prompt.txt
   - scripts/pipeline/run/05_prompt.txt
   - scripts/pipeline/run/06_prompt.txt
   - scripts/pipeline/run/07_prompt.txt

2. For each prompt file, follow the instructions in it. Use the file paths given inside the prompt to read the input files from the workspace (input folder or paths specified in the prompt).

3. Write the output for each prompt to the corresponding response file (only the section content requested by the prompt, no extra title or preamble):
   - 01_prompt.txt → scripts/pipeline/run/01_response.md
   - 02_prompt.txt → scripts/pipeline/run/02_response.md
   - 03_prompt.txt → scripts/pipeline/run/03_response.md
   - 04_prompt.txt → scripts/pipeline/run/04_response.md
   - 05_prompt.txt → scripts/pipeline/run/05_response.md
   - 06_prompt.txt → scripts/pipeline/run/06_response.md
   - 07_prompt.txt → scripts/pipeline/run/07_response.md

4. Produce exactly 7 response files. When all 7 are written, tell me so I can run the assemble step.
```

---

### Step 3 – Run assemble (combine results + convert to Word)

Copy-paste this to Cursor after the 7 response files exist:

```
Run the EI doc pipeline assemble step: execute the script that reads the seven response files from scripts/pipeline/run/, combines them into one document with the title from the manifest, writes output/Explanation_<name>.md, and converts it to Word using the benchmark reference. From the project root run: python scripts/pipeline/pipeline.py assemble
```

---

## 9. Cursor instruction file (`CURSOR_INSTRUCTION.txt`)

The file `scripts/pipeline/CURSOR_INSTRUCTION.txt` contains the **exact wording of Step 2** above. Open it from the project and send its content to Cursor instead of retyping.

---

## 10. Dependencies

- Python 3, openpyxl (for Metadata and Structure reading).
- Pandoc (recommended) for .md → .docx with `--reference-doc` to match benchmark formatting.

---

## 11. Error handling

- **Prepare:** Verification failures → clear messages, exit 1. Discovery failures (after verify) → clear message. Missing prompt template → exit with path.
- **Assemble:** Missing manifest or any of 01…07 response files → list missing, exit 1. Reference .docx missing or pandoc not found → still write .md, print that .docx was skipped.

---

## Summary

- **Clear run/:** Prepare deletes old `01_response.md` … `07_response.md` and overwrites prompt files and manifest so each run starts clean.
- **Stem match:** All four input files must use the same stem (same name after prefix).
- **Verification:** Presence, cardinality, stem match, per-file validity, structure names from code in Structure xlsx; bypass with `--skip-verify`.
- **Copy-paste:** Section 8 gives exact wording for Step 1 (prepare), Step 2 (generate 7 sections), Step 3 (assemble); use those phrases in Cursor for each step.
