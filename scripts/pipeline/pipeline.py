#!/usr/bin/env python3
"""
EI Doc Pipeline (Option B, no API).
Verify: one of each input file (Metadata, Available fields, Structure, Code); stems Metadata/Structure/Code match; Available fields may match that or output name from metadata (B8/B9). If names only differ slightly (e.g. space), prompt user to proceed. Optional --yes to assume yes.
Prepare: clear run/, discover input/, read Metadata, write 7 prompts with paths injected.
Assemble: read 7 responses, build one .md, convert to .docx.
Verify: check response files (e.g. 04: no "output only", parameter count match). Run before or after generating responses; assemble runs verify automatically.
Run from repo root: python scripts/pipeline/pipeline.py prepare [--skip-verify] [--yes] | verify | assemble
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

PIPELINE_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = PIPELINE_DIR.parent.parent
INPUT_DIR = PROJECT_ROOT / "input"
OUTPUT_DIR = PROJECT_ROOT / "output"
PROMPTS_DIR = PROJECT_ROOT / "prompts"
RUN_DIR = PIPELINE_DIR / "run"

SECTION_SPEC = [
    ("01", "PROMPT_General_Overview_section.md", ["structure", "params", "code"]),
    ("02", "PROMPT_Problem_Description_and_Suggested_Resolution_section.md", ["structure", "params", "code"]),
    ("03", "PROMPT_Parameters_Reference_Table_section.md", ["params"]),
    ("04", "PROMPT_Parameter_Configuration_Guidelines_section.md", ["structure", "params", "code", "params_docx"]),
    ("05", "PROMPT_Parameter_Relationships_section.md", ["structure", "params", "code", "params_docx"]),
    ("06", "PROMPT_Default_Values_and_Practical_Examples_section.md", ["structure", "params", "code"]),
    ("07", "PROMPT_EI_Function_Structure_and_ABAP_Code_section.md", ["structure", "code"]),
]

# Option B: accept prefix with or without space before underscore (e.g. Metadata_ vs Metadata _)
PREFIXES = {
    "metadata": ("Metadata_", "Metadata _"),
    "params": ("Available fields_", "Available fields _"),
    "structure": ("Structure_", "Structure _"),
    "code": ("Code_", "Code _"),
}


def _stem_from_path(path: Path, prefixes: tuple[str, ...]) -> str:
    """Return the stem (name after prefix, without extension). Tries each prefix (e.g. Metadata_ then Metadata _)."""
    name = path.stem
    for prefix in prefixes:
        if name.startswith(prefix):
            return name[len(prefix) :].strip()
    return name


def _normalize_stem(s: str) -> str:
    """Normalize stem for similarity: spaces/underscores interchangeable, lower, stripped."""
    return re.sub(r"[\s_]+", "_", s.strip().lower()).strip("_")


def _stems_similar(s1: str, s2: str) -> bool:
    """True if stems are effectively the same (e.g. only space vs underscore difference)."""
    return _normalize_stem(s1) == _normalize_stem(s2)


def _discover_inputs(assume_yes: bool = False) -> dict[str, Path] | None:
    """Find one of each: Code, Structure, Available fields, Metadata. If multiple similar (e.g. space in prefix), ask user to proceed. Optional _Parameters.docx."""
    code = list(dict.fromkeys(
        list(INPUT_DIR.glob("Code_*.txt")) + list(INPUT_DIR.glob("Code _*.txt"))
    ))
    struct = list(dict.fromkeys(
        list(INPUT_DIR.glob("Structure_*.xlsx")) + list(INPUT_DIR.glob("Structure _*.xlsx"))
    ))
    avail = list(dict.fromkeys(
        list(INPUT_DIR.glob("Available fields_*.xlsx")) + list(INPUT_DIR.glob("Available fields _*.xlsx"))
    ))
    meta = list(dict.fromkeys(
        list(INPUT_DIR.glob("Metadata_*.xlsx")) + list(INPUT_DIR.glob("Metadata _*.xlsx"))
    ))
    params_docx = list(INPUT_DIR.glob("_Parameters.docx"))
    if not code or not struct or not avail or not meta:
        return None

    # If multiple files for any type, show and ask to proceed with first (similar names = e.g. space in prefix only)
    for candidates, key, type_name in [
        (meta, "metadata", "Metadata"),
        (avail, "params", "Available fields"),
        (struct, "structure", "Structure"),
        (code, "code", "Code"),
    ]:
        if len(candidates) > 1:
            stems = [_stem_from_path(p, PREFIXES[key]) for p in candidates]
            similar = len(set(stems)) == 1
            print(f"Multiple {type_name} files found (names {'match' if similar else 'differ'}):")
            for i, p in enumerate(candidates, 1):
                print(f"  {i}. {p.name}")
            print(f"  → Use first: {candidates[0].name}")
            if not assume_yes:
                try:
                    reply = input("  Proceed with first? [y/N]: ").strip().lower()
                except EOFError:
                    reply = "n"
                if reply not in ("y", "yes"):
                    return None
            # use first
            if key == "metadata":
                meta = [candidates[0]]
            elif key == "params":
                avail = [candidates[0]]
            elif key == "structure":
                struct = [candidates[0]]
            else:
                code = [candidates[0]]

    return {
        "code": code[0].resolve(),
        "structure": struct[0].resolve(),
        "params": avail[0].resolve(),
        "metadata": meta[0].resolve(),
        "params_docx": params_docx[0].resolve() if params_docx else None,
    }


def _metadata_sheet(wb):
    """Use sheet with General metadata: 'General', 'Metadata general', or first sheet."""
    for name in ("General", "Metadata general"):
        if name in wb.sheetnames:
            return wb[name]
    return wb.worksheets[0] if wb.worksheets else wb.active


def _read_metadata_title(metadata_path: Path) -> tuple[str, str]:
    """Read Exception indicator ID (B8) and name (B9) from Metadata. Prefer sheet 'General' if present."""
    try:
        import openpyxl
        wb = openpyxl.load_workbook(metadata_path, read_only=True)
        ws = _metadata_sheet(wb)
        rows = list(ws.iter_rows(min_row=1, max_row=15, values_only=True))
        wb.close()
        if len(rows) >= 9 and len(rows[8]) > 1 and len(rows[7]) > 1:
            return (str(rows[7][1] or "").strip(), str(rows[8][1] or "").strip())
    except Exception:
        pass
    return ("", "")


def _verify_stem_match(
    paths: dict[str, Path],
    metadata_id: str = "",
    metadata_name: str = "",
    assume_yes: bool = False,
) -> list[str]:
    """Metadata, Structure, Code must share same stem. Available fields may match that stem or output name (from metadata contents). If names only differ slightly (e.g. space), ask user to proceed."""
    errors = []
    stems = {}
    for key in ("metadata", "params", "structure", "code"):
        p = paths.get(key)
        if p:
            stems[key] = _stem_from_path(p, PREFIXES[key])

    common_stem = stems.get("metadata") or stems.get("structure") or stems.get("code")
    meta_struct_code_ok = (
        stems.get("metadata") == stems.get("structure") == stems.get("code")
    )

    # Available fields stem can match common_stem OR output name from metadata (B8/B9): name, ID, or "name_ID" / "name - ID"
    params_stem = stems.get("params", "")
    combined_underscore = "_".join(filter(None, [metadata_name.strip(), metadata_id.strip()]))
    combined_dash = " - ".join(filter(None, [metadata_name.strip(), metadata_id.strip()]))
    params_ok = (
        params_stem == common_stem
        or params_stem == metadata_name
        or params_stem == metadata_id
        or params_stem == combined_underscore
        or params_stem == combined_dash
        or _stems_similar(params_stem, common_stem or "")
        or _stems_similar(params_stem, metadata_name)
        or _stems_similar(params_stem, metadata_id)
        or _stems_similar(params_stem, combined_underscore)
        or _stems_similar(params_stem, combined_dash)
    )

    if not meta_struct_code_ok:
        # Metadata / Structure / Code don't match; check if very similar
        if common_stem and _stems_similar(stems.get("metadata", ""), stems.get("structure", "")) and _stems_similar(stems.get("structure", ""), stems.get("code", "")):
            print("File names differ slightly (e.g. space vs underscore):")
            print(f"  Metadata: {stems.get('metadata', '?')} | Structure: {stems.get('structure', '?')} | Code: {stems.get('code', '?')}")
            if not assume_yes:
                try:
                    reply = input("  Proceed anyway? [y/N]: ").strip().lower()
                except EOFError:
                    reply = "n"
                if reply in ("y", "yes"):
                    params_ok = True if _stems_similar(params_stem, common_stem or "") or params_stem == common_stem else params_ok
                else:
                    errors.append(
                        "Metadata, Structure, and Code file names (after prefix) must match. "
                        f"Found: {stems.get('metadata', '?')} , {stems.get('structure', '?')} , {stems.get('code', '?')} ."
                    )
            else:
                params_ok = True
        else:
            errors.append(
                "Metadata, Structure, and Code file names (after prefix) must match. "
                f"Found: Metadata_{stems.get('metadata', '?')} , Structure_{stems.get('structure', '?')} , Code_{stems.get('code', '?')} . "
                "Available fields may match that name or the output name from metadata (B8/B9)."
            )
    elif not params_ok:
        # Params stem differs; check if very similar
        if _stems_similar(params_stem, common_stem or "") or _stems_similar(params_stem, metadata_name) or _stems_similar(params_stem, metadata_id):
            print("Available fields file name differs slightly from Metadata/Structure/Code (or from output name in metadata).")
            print(f"  Available fields: {params_stem} | others: {common_stem} | metadata name/ID: {metadata_name!r} / {metadata_id!r}")
            if not assume_yes:
                try:
                    reply = input("  Proceed anyway? [y/N]: ").strip().lower()
                except EOFError:
                    reply = "n"
                if reply in ("y", "yes"):
                    params_ok = True
            else:
                params_ok = True
        if not params_ok:
            errors.append(
                "Available fields file name (after prefix) should match Metadata/Structure/Code or the output name from metadata (B8/B9). "
                f"Found: Available fields_{params_stem} ; others: {common_stem} ; metadata name/ID: {metadata_name!r} / {metadata_id!r} ."
            )

    return errors


def _extract_structure_names_from_code(code_path: Path) -> list[str]:
    """Extract only the T_DATA structure name from ABAP (the main output structure). Other STRUCTURE refs (e.g. INCLUDE) are ignored."""
    text = code_path.read_text(encoding="utf-8", errors="replace")
    # T_DATA STRUCTURE /SKN/... or T_DATA STRUCTURE  /SKN/...
    pattern = re.compile(r"T_DATA\s+STRUCTURE\s+([/A-Za-z0-9_]+)", re.IGNORECASE)
    names = []
    for m in pattern.finditer(text):
        name = m.group(1).strip()
        if "/" in name:
            names.append(name)
    return list(dict.fromkeys(names))


def _structure_names_in_xlsx(struct_path: Path) -> set[str]:
    """Read 'Structure Name' column (unique values) from first sheet."""
    import openpyxl
    wb = openpyxl.load_workbook(struct_path, read_only=True)
    ws = wb.active
    rows = list(ws.iter_rows(min_row=1, max_row=500, values_only=True))
    wb.close()
    if not rows:
        return set()
    headers = [str(c).strip() if c is not None else "" for c in rows[0]]
    col_idx = None
    for i, h in enumerate(headers):
        if h and "structure" in h.lower() and "name" in h.lower():
            col_idx = i
            break
    if col_idx is None:
        col_idx = 0
    names = set()
    for row in rows[1:]:
        if row and len(row) > col_idx and row[col_idx] is not None:
            names.add(str(row[col_idx]).strip())
    return names


def verify(skip_verify: bool = False, assume_yes: bool = False) -> list[str]:
    """Run all verification checks. Return list of error strings (empty if OK)."""
    if skip_verify:
        return []
    errors = []

    # Presence and cardinality (may prompt if multiple similar files)
    paths = _discover_inputs(assume_yes=assume_yes)
    if paths is None:
        code = list(INPUT_DIR.glob("Code_*.txt")) + list(INPUT_DIR.glob("Code _*.txt"))
        struct = list(INPUT_DIR.glob("Structure_*.xlsx")) + list(INPUT_DIR.glob("Structure _*.xlsx"))
        avail = list(INPUT_DIR.glob("Available fields_*.xlsx")) + list(INPUT_DIR.glob("Available fields _*.xlsx"))
        meta = list(INPUT_DIR.glob("Metadata_*.xlsx")) + list(INPUT_DIR.glob("Metadata _*.xlsx"))
        if not meta:
            errors.append("Missing: exactly one Metadata_*.xlsx in input/")
        elif len(meta) > 1:
            errors.append("Multiple Metadata_*.xlsx found; use one function set per run.")
        if not avail:
            errors.append("Missing: exactly one Available fields_*.xlsx in input/")
        elif len(avail) > 1:
            errors.append("Multiple Available fields_*.xlsx found; use one function set per run.")
        if not struct:
            errors.append("Missing: exactly one Structure_*.xlsx in input/")
        elif len(struct) > 1:
            errors.append("Multiple Structure_*.xlsx found; use one function set per run.")
        if not code:
            errors.append("Missing: exactly one Code_*.txt in input/")
        elif len(code) > 1:
            errors.append("Multiple Code_*.txt found; use one function set per run.")
        return errors

    # Stem match: Metadata/Structure/Code must match; Available fields may match that or output name from metadata
    metadata_id, metadata_name = _read_metadata_title(paths["metadata"])
    errors.extend(
        _verify_stem_match(paths, metadata_id, metadata_name, assume_yes=assume_yes)
    )

    # Per-file checks
    try:
        import openpyxl
        wb = openpyxl.load_workbook(paths["metadata"], read_only=True)
        ws = _metadata_sheet(wb)
        rows = list(ws.iter_rows(min_row=1, max_row=15, values_only=True))
        wb.close()
        if len(rows) < 9:
            errors.append("Metadata file must have Exception indicator ID (row 8) and Exception indicator name (row 9) in the General section.")
        else:
            _ = str(rows[7][1] or "").strip()
            _ = str(rows[8][1] or "").strip()
    except Exception as e:
        errors.append(f"Metadata file unreadable or missing required cells: {e}")

    try:
        import openpyxl
        wb = openpyxl.load_workbook(paths["params"], read_only=True)
        if "Parameters" not in wb.sheetnames:
            errors.append("Available fields file must contain a 'Parameters' sheet with at least one parameter row.")
        else:
            ws = wb["Parameters"]
            rows = list(ws.iter_rows(min_row=1, max_row=10, values_only=True))
            wb.close()
            if len(rows) < 2:
                errors.append("Available fields file must contain a 'Parameters' sheet with at least one parameter row.")
    except Exception as e:
        errors.append(f"Available fields file unreadable: {e}")

    try:
        import openpyxl
        wb = openpyxl.load_workbook(paths["structure"], read_only=True)
        ws = wb.active
        rows = list(ws.iter_rows(min_row=1, max_row=5, values_only=True))
        wb.close()
        if not rows:
            errors.append("Structure file must have a sheet with Structure Name and at least one row.")
        else:
            headers = [str(c).strip() if c is not None else "" for c in rows[0]]
            if not any("structure" in h.lower() and "name" in h.lower() for h in headers) and not headers:
                errors.append("Structure file must have a sheet with Structure Name and at least one row.")
    except Exception as e:
        errors.append(f"Structure file unreadable: {e}")

    try:
        text = paths["code"].read_text(encoding="utf-8", errors="replace")
        if not text.strip():
            errors.append("Code file is empty or unreadable.")
    except Exception as e:
        errors.append(f"Code file is empty or unreadable: {e}")

    # T_DATA structure from code must be present in Structure xlsx (case-insensitive)
    code_names = _extract_structure_names_from_code(paths["code"])
    struct_names = _structure_names_in_xlsx(paths["structure"])
    struct_names_upper = {s.upper() for s in struct_names}
    for name in code_names:
        if name.upper() not in struct_names_upper:
            errors.append(
                f"Structure file does not match code: T_DATA structure '{name}' is not listed in the Structure file."
            )

    return errors


# Forbidden phrases in Parameter Configuration Guidelines (04); rule: do not classify as "output only" / "not a filter"
_04_FORBIDDEN_PHRASES = [
    "output only",
    "output field",
    "not a filter",
    "used for output display only",
]
# Date/time rule: 04 must not name internal variables (prompt: business meaning only)
_04_FORBIDDEN_INTERNAL_NAMES = [
    "R_DATUM",
    "R_UDATE",
    "SY_DATLO",
    "SY_TIMLO",
    "DATE_FROM",
    "SY-DATUM",
]
# Fixed-option parameters: when present in 03, 04 must have a "[PARAM_NAME] Options:" subsection
_04_PARAMS_REQUIRING_OPTIONS = [
    "DATE_REF_FLD",
    "TIME_REF_FLD",
    "DURATION_UNIT",
]


def _serial_series_from_03_param_names(param_names_ordered: list[str]) -> list[tuple[str, str]]:
    """Detect serial-number series (same prefix + consecutive indices). Return list of (first_name, last_name) per series."""
    def parse(name: str) -> tuple[str, int] | None:
        m = re.match(r"^(.+?)(\d+)$", name)
        if m:
            return (m.group(1), int(m.group(2)))
        return None

    series_list: list[tuple[str, str]] = []
    run: list[tuple[str, str, int]] = []  # (name, prefix, index)

    for name in param_names_ordered:
        p = parse(name)
        if p is None:
            if len(run) >= 2:
                series_list.append((run[0][0], run[-1][0]))
            run = []
            continue
        prefix, idx = p
        if run and run[-1][1] == prefix and run[-1][2] + 1 == idx:
            run.append((name, prefix, idx))
        else:
            if len(run) >= 2:
                series_list.append((run[0][0], run[-1][0]))
            run = [(name, prefix, idx)]

    if len(run) >= 2:
        series_list.append((run[0][0], run[-1][0]))
    return series_list


def verify_responses() -> list[str]:
    """Check that response files exist and 04_response.md meets Parameter Configuration Guidelines rules. Return list of error strings."""
    errors = []
    for num, _, _ in SECTION_SPEC:
        r = RUN_DIR / f"{num}_response.md"
        if not r.exists():
            errors.append(f"Missing response file: {r.name}")
    if errors:
        return errors

    # 04_response.md: forbidden phrases and parameter count match
    r04 = RUN_DIR / "04_response.md"
    r03 = RUN_DIR / "03_response.md"
    if r04.exists():
        text04 = r04.read_text(encoding="utf-8")
        lines04 = text04.splitlines()
        for phrase in _04_FORBIDDEN_PHRASES:
            for i, line in enumerate(lines04, 1):
                if phrase.lower() in line.lower():
                    errors.append(f"04_response.md line {i}: forbidden phrase '{phrase}' (do not use 'output only' / 'not a filter' wording)")
                    break  # one error per phrase type

        for name in _04_FORBIDDEN_INTERNAL_NAMES:
            for i, line in enumerate(lines04, 1):
                if name in line:
                    errors.append(
                        f"04_response.md line {i}: forbidden internal name '{name}' "
                        "(date/time params: business meaning only; no R_DATUM, SY_DATLO, DATE_FROM, etc.)"
                    )
                    break

        if r03.exists():
            text03 = r03.read_text(encoding="utf-8")
            # Count parameter rows in 03: lines like | 1 | PARAM | ...
            param_rows = [l for l in text03.splitlines() if re.match(r"^\|\s*\d+\s*\|", l)]
            expected_count = len(param_rows)
            # Extract parameter names from 03 (Parameter column = second data column, index 2 after split by |)
            params_in_03 = set()
            param_names_ordered: list[str] = []
            for row in param_rows:
                parts = [p.strip() for p in row.split("|")]
                if len(parts) >= 3:
                    pname = parts[2]
                    params_in_03.add(pname)
                    param_names_ordered.append(pname)
            # Serial-number series in 03 must be grouped in 04 (e.g. UVK01–UVK05 as one entry)
            for first, last in _serial_series_from_03_param_names(param_names_ordered):
                grouped_hyphon = f"**{first} - {last}**"
                grouped_endash = f"**{first}\u2013{last}**"
                if grouped_hyphon not in text04 and grouped_endash not in text04:
                    errors.append(
                        f"04_response.md: serial-number series {first}..{last} must be grouped into one entry "
                        f'(e.g. "{grouped_hyphon}" or "{grouped_endash}")'
                    )
            # Fixed-option params in 03 must have an Options subsection in 04
            for param in _04_PARAMS_REQUIRING_OPTIONS:
                if param in params_in_03:
                    options_marker = f"**{param} Options:**"
                    if options_marker not in text04:
                        errors.append(
                            f"04_response.md: {param} must have an Options subsection "
                            f"(fixed-option parameter; add '{options_marker}' with possible values)"
                        )
            # Check 04 IMPORTANT line contains this number
            match = re.search(r"ALL\s+(\d+)\s+parameters", text04, re.IGNORECASE)
            if match and expected_count > 0:
                n_in_04 = int(match.group(1))
                if n_in_04 != expected_count:
                    errors.append(
                        f"04_response.md IMPORTANT line says {n_in_04} parameters but 03_response.md has {expected_count} parameter rows"
                    )
            elif expected_count > 0 and not re.search(r"IMPORTANT.*\d+.*parameters", text04, re.IGNORECASE):
                errors.append("04_response.md missing IMPORTANT line with parameter count (e.g. ALL N parameters)")

    return errors


def _write_manifest_at(basename: str, title: str) -> None:
    """Write manifest.txt with output_basename and title."""
    manifest = RUN_DIR / "manifest.txt"
    manifest.write_text(f"output_basename={basename}\ntitle={title}\n", encoding="utf-8")


def _write_manifest(paths: dict) -> None:
    """Re-read Metadata (General/Metadata general sheet), compute basename and title, write manifest."""
    import openpyxl
    wb = openpyxl.load_workbook(paths["metadata"], read_only=True)
    ws = _metadata_sheet(wb)
    rows = list(ws.iter_rows(min_row=1, max_row=15, values_only=True))
    wb.close()
    id_val = str(rows[7][1] or "").strip() if len(rows) > 8 and len(rows[7]) > 1 else ""
    name_val = str(rows[8][1] or "").strip() if len(rows) >= 9 and len(rows[8]) > 1 else ""
    stem = _stem_from_path(paths["metadata"], PREFIXES["metadata"])
    basename = stem
    title = f"Exception Indicator: {name_val} - {id_val}" if (name_val or id_val) else f"Exception Indicator: {basename}"
    _write_manifest_at(basename, title)


def prepare(skip_verify: bool = False, assume_yes: bool = False, update_manifest_only: bool = False) -> None:
    """Verify (unless --skip-verify), clear run/, discover, read Metadata, write manifest and 7 prompt files. If update_manifest_only, only re-read Metadata and write manifest (do not clear response files)."""
    if update_manifest_only:
        paths = _discover_inputs(assume_yes=assume_yes)
        if paths is None:
            print("Could not discover input files.", file=sys.stderr)
            sys.exit(1)
        _write_manifest(paths)
        print("Manifest updated (title from Metadata). Run assemble to rebuild output.")
        return
    errs = verify(skip_verify=skip_verify, assume_yes=assume_yes)
    if errs:
        print("Verification failed:")
        for e in errs:
            print("  -", e)
        print("Fix the issues above or re-run with --skip-verify to ignore.")
        sys.exit(1)

    paths = _discover_inputs(assume_yes=assume_yes)
    if paths is None:
        print("Could not discover input files. Need one of each in input/: Code[_ ]*.txt, Structure[_ ]*.xlsx, Available fields[_ ]*.xlsx, Metadata[_ ]*.xlsx (or user declined to proceed).", file=sys.stderr)
        sys.exit(1)

    # Clear previous run: delete old response files (skip when only updating manifest)
    RUN_DIR.mkdir(parents=True, exist_ok=True)
    if not update_manifest_only:
        for num, _, _ in SECTION_SPEC:
            r = RUN_DIR / f"{num}_response.md"
            if r.exists():
                r.unlink()

    # Read Metadata (same sheet as verify: General / Metadata general / first sheet)
    import openpyxl
    wb = openpyxl.load_workbook(paths["metadata"], read_only=True)
    ws = _metadata_sheet(wb)
    rows = list(ws.iter_rows(min_row=1, max_row=15, values_only=True))
    wb.close()
    id_val = str(rows[7][1] or "").strip() if len(rows) > 8 and len(rows[7]) > 1 else ""
    name_val = str(rows[8][1] or "").strip() if len(rows) >= 9 and len(rows[8]) > 1 else ""
    stem = _stem_from_path(paths["metadata"], PREFIXES["metadata"])
    basename = stem
    title = f"Exception Indicator: {name_val} - {id_val}" if (name_val or id_val) else f"Exception Indicator: {basename}"

    _write_manifest_at(basename, title)

    # Replacements for placeholder lines in prompts (escape backslashes for re.sub on Windows)
    def _repl(s):
        return str(s).replace("\\", "\\\\")

    def replace_placeholders(text: str) -> str:
        text = re.sub(r"\[Provide the structure file path[^\]]*\]", _repl(paths["structure"]), text)
        text = re.sub(r"\[Provide the output structure / fields file path[^\]]*\]", _repl(paths["structure"]), text)
        text = re.sub(r"\[Provide the Parameters sheet path[^\]]*\]", _repl(paths["params"]), text)
        text = re.sub(r"\[Provide the file path or paste the parameters table content here\]", _repl(paths["params"]), text)
        text = re.sub(r"\[Provide the code file path[^\]]*\]", _repl(paths["code"]), text)
        text = re.sub(r"\[Provide the ABAP source\]", _repl(paths["code"]), text)
        if paths["params_docx"]:
            text = re.sub(
                r"Selected parameters file[^\n]*\n[^\n]*",
                "Selected parameters file: " + _repl(paths["params_docx"]) + "\n",
                text,
                count=1,
            )
        else:
            text = re.sub(
                r"Selected parameters file[^\n]*\n[^\n]*",
                "Selected parameters file: (optional – omit if not in input)\n",
                text,
                count=1,
            )
        return text

    for num, template_name, _ in SECTION_SPEC:
        template_path = PROMPTS_DIR / template_name
        if not template_path.exists():
            print(f"Missing prompt template: {template_path}", file=sys.stderr)
            sys.exit(1)
        text = template_path.read_text(encoding="utf-8")
        text = replace_placeholders(text)
        (RUN_DIR / f"{num}_prompt.txt").write_text(text, encoding="utf-8")

    print("Prepare done. Output basename:", basename)
    print("In Cursor, send the instruction from scripts/pipeline/CURSOR_INSTRUCTION.txt")
    print("When the 7 response files are in scripts/pipeline/run/, run: python scripts/pipeline/pipeline.py verify  (optional)")
    print("Then run: python scripts/pipeline/pipeline.py assemble")


def assemble() -> None:
    """Read manifest and 7 response files, build one .md, write to output/, convert to .docx."""
    if not (RUN_DIR / "manifest.txt").exists():
        print("Run prepare first. No run/manifest.txt found.", file=sys.stderr)
        sys.exit(1)
    manifest = (RUN_DIR / "manifest.txt").read_text(encoding="utf-8")
    basename = ""
    title = ""
    for line in manifest.splitlines():
        if line.startswith("output_basename="):
            basename = line.split("=", 1)[1].strip()
        elif line.startswith("title="):
            title = line.split("=", 1)[1].strip()
    if not basename:
        print("manifest.txt missing output_basename=", file=sys.stderr)
        sys.exit(1)

    # Verify response files (04 rules: no "output only", parameter count match) before assembling
    verr = verify_responses()
    if verr:
        print("Response verification failed:")
        for e in verr:
            print("  -", e)
        print("Fix the issues above (e.g. edit 04_response.md) then run assemble again.")
        sys.exit(1)

    parts = []
    for num, _, _ in SECTION_SPEC:
        r = RUN_DIR / f"{num}_response.md"
        parts.append(r.read_text(encoding="utf-8"))

    full_md = f"# {title}\n\n" + "\n\n".join(parts)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    out_md = OUTPUT_DIR / f"Explanation_{basename}.md"
    out_md.write_text(full_md, encoding="utf-8")
    print("Wrote", out_md)

    out_docx = OUTPUT_DIR / f"Explanation_{basename}.docx"
    try:
        import importlib.util
        spec = importlib.util.spec_from_file_location(
            "md_to_docx",
            PIPELINE_DIR / "md_to_docx.py",
        )
        md2docx = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(md2docx)
        md2docx.convert_md_to_docx(out_md, out_docx)
        print("Wrote", out_docx)
    except ImportError as e:
        print("MD->DOCX converter unavailable (install markdown, beautifulsoup4). Skipping .docx.", e)
    except Exception as e:
        print("MD->DOCX conversion failed. Markdown is ready.", e)


def main() -> None:
    parser = argparse.ArgumentParser(description="EI Doc Pipeline: prepare | verify | assemble")
    parser.add_argument("mode", choices=["prepare", "verify", "assemble"], help="prepare (verify + write prompts), verify (check response files), or assemble (build .md + .docx)")
    parser.add_argument("--skip-verify", action="store_true", help="skip verification when running prepare")
    parser.add_argument("--yes", "-y", action="store_true", help="assume yes for 'proceed anyway?' prompts (e.g. similar file names)")
    parser.add_argument("--update-manifest", action="store_true", help="prepare: only re-read Metadata and write manifest (do not clear response files)")
    args = parser.parse_args()
    if args.mode == "prepare":
        prepare(skip_verify=args.skip_verify, assume_yes=args.yes, update_manifest_only=args.update_manifest)
    elif args.mode == "verify":
        verr = verify_responses()
        if verr:
            print("Response verification failed:")
            for e in verr:
                print("  -", e)
            sys.exit(1)
        print("Response verification passed.")
    else:
        assemble()


if __name__ == "__main__":
    main()
