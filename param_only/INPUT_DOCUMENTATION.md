# param_only pipeline – input documentation

This document describes the **input** expected by the **param_only** pipeline. The pipeline uses the project’s `input/` folder (no Metadata files).

---

## 1. Input location

- **Directory:** `input/` (project root).
- All required and optional files must be in this folder (not in subfolders like `input/old/` for discovery).

---

## 2. Required files

You must have **exactly one** of each of the following. The pipeline discovers them by filename prefix.

| Role        | Filename pattern(s)                    | Format | Description |
|------------|----------------------------------------|--------|-------------|
| **Code**   | `Code_*.txt` or `Code _*.txt`          | `.txt` | ABAP/source code. Used for parameters reference, configuration guidelines, relationships, and default values. |
| **Structure** | `Structure_*.xlsx` or `Structure _*.xlsx` | `.xlsx` | Structure definitions. Must have a sheet with a “Structure Name” column and at least one data row. |
| **Available fields (params)** | `Available fields_*.xlsx` or `Available fields _*.xlsx` | `.xlsx` | Parameters/fields (e.g. EI parameters). Used as the parameters table. |

- **Prefixes:**  
  - Code: `Code_` or `Code _` (with space).  
  - Structure: `Structure_` or `Structure _`.  
  - Available fields: `Available fields_` or `Available fields _`.

- If multiple files match the same role (e.g. two `Code_*.txt`), the pipeline lists them and uses the **first** one (and may prompt to proceed unless `--yes` is used).

---

## 3. Stem matching (required)

The three required files must refer to the **same logical object**. The pipeline enforces this by **stem matching**.

- **Stem** = the part of the filename after the prefix (no extension).  
  Examples:  
  - `Code_SW_ Number Range Intervals_SW_01_01_NRIV.txt` → stem `SW_ Number Range Intervals_SW_01_01_NRIV`  
  - `Structure_SKN_S_SW_01_01_NRIV.xlsx` → stem `SKN_S_SW_01_01_NRIV`  
  - `Available fields_My Report_ABC_01_02.xlsx` → stem `My Report_ABC_01_02`

- **Rule:** After normalizing (lowercase, spaces/underscores collapsed to single `_`, leading/trailing `_` removed), the stems of **Structure**, **Available fields**, and **Code** must be **equal**.  
  If they differ, `prepare` fails with an error like:  
  *"Structure, Available fields, and Code stems must match."*

- So when adding input, ensure the **suffix** (after the prefix) is aligned across the three files so that normalized stems match (e.g. same identifier or report name in all three).

---

## 4. Structure file requirements

- **Format:** Excel (`.xlsx`), single sheet used (active sheet).
- **Content:**  
  - A column whose header contains both “structure” and “name” (e.g. “Structure Name”).  
  - At least one row of data.
- **Consistency with code:** The code file may reference structures like `T_DATA STRUCTURE /NAME/`. Every such `/NAME/` must appear in the Structure file’s “Structure Name” column (case-insensitive). Otherwise `prepare` (or verify) reports that the structure file does not match the code.

---

## 5. Code file requirements

- **Format:** Plain text (`.txt`), UTF-8 (errors replaced when read).
- **Content:** Non-empty. May contain ABAP or similar; the pipeline looks for `T_DATA STRUCTURE /.../` to extract structure names and cross-check with the Structure file.

---

## 6. Optional file

| Role             | Filename        | Format  | Description |
|------------------|-----------------|---------|-------------|
| **Parameters doc** | `_Parameters.docx` | `.docx` | Optional “selected parameters” file. If present, it is passed into the prompts (e.g. section 04/05). If omitted, prompts still run without it. |

- Only one `_Parameters.docx` is used if present.

---

## 7. What is not used

- **Metadata** files (e.g. `Metadata _*.xlsx`) are **not** used by the param_only pipeline.
- **Summary** files are not used.
- Files in **subfolders** (e.g. `input/old/`) are not discovered; only files directly in `input/` are.

---

## 8. Quick checklist

- [ ] One **Code** file: `Code_*.txt` or `Code _*.txt` in `input/`.
- [ ] One **Structure** file: `Structure_*.xlsx` or `Structure _*.xlsx` in `input/`.
- [ ] One **Available fields** file: `Available fields_*.xlsx` or `Available fields _*.xlsx` in `input/`.
- [ ] Stems of the three (after prefix, normalized) **match**.
- [ ] Structure file has a “Structure Name”–style column and at least one row.
- [ ] Every `T_DATA STRUCTURE /NAME/` in the code appears in the Structure file.
- [ ] (Optional) `_Parameters.docx` in `input/` if you use selected parameters.

---

## 9. Running the pipeline

From the **repo root**:

```bash
python param_only/param_pipeline.py prepare [--skip-verify] [--yes]
```

- **prepare** discovers the three (or four, with docx) inputs from `input/`, checks stems and consistency, then writes `param_only/run/manifest.txt` and section prompts (03–07).
- If discovery or verification fails, fix the input (names, stems, structure/code alignment) and run again. Use `param_only/CURSOR_INSTRUCTION_PARAMS.txt` for the next steps (generating and verifying responses, then `verify` and `assemble`).
