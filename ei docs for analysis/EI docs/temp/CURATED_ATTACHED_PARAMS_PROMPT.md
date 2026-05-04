# Prompt: Curated SAP Explanations (Attached Parameters Batch)

## Objective
Generate one concise, SAP-expert explanation for each attached parameter using the same flow style as `params_dictionary.xlsx` generation, but with stronger quality controls.

## Scope
Use the provided attached parameter list (deduplicated in original order).

## Evidence Sources
For each parameter, combine:
1. Existing curated overrides from `build_params_dictionary_xlsx.py` (`OVERRIDES`).
2. Parameter metadata from DOCX files in `EI docs/Part 1..4`:
   - Description
   - Data Element
   - Domain
   - Type
3. ABAP token presence in the same corpus to avoid contradictions.

## Hard Rules
1. One parameter -> one canonical sentence.
2. One sentence only, target 8-28 words.
3. Prefer business semantics over technical filler.
4. Avoid generic placeholders:
   - "technical selection parameter"
   - "used in this monitor set"
   - "align interpretation with DDIC semantics"
5. If evidence is weak, keep the statement conservative and non-contradictory.

## Quality Checklist
- [ ] Accurate SAP meaning.
- [ ] Reusable wording across files.
- [ ] No contradiction with ABAP evidence.
- [ ] No banned generic phrase.
- [ ] Word count in range 6..32.

## Output Format
XLSX with columns:
- `parameter`
- `curated_explanation`
- `confidence`
- `evidence_notes`
