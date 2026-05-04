# Prompt: Curated SAP Parameter Dictionary (Part 1 Missing Set)

## Objective
Generate **one curated, SAP-expert, concise explanation per parameter** for every parameter listed in:

`params_dictionary_part1_missing.xlsx` (sheet: `missing_dictionary`, column `parameter`)

The output is a dictionary file suitable for reuse across all Part 1 documentation updates.

## Inputs
For each parameter, use evidence from:
1. **Part 1 DOCX parameter table metadata** (Description / Data Element / Domain / Type).
2. **Part 1 prose in “Parameter Configuration Guidelines”** where available.
3. **ABAP section tokens** in the same file(s) to avoid contradiction.

## Hard Rules
1. **One parameter -> one explanation** (canonical sentence reused everywhere).
2. **No generic placeholders** (forbidden examples):
   - “technical selection parameter”
   - “used in this monitor set”
   - “align to DDIC semantics”
3. **Concise**: target 10-24 words, one sentence.
4. **SAP-specific wording**: business object semantics first (customer/vendor/material/document/status/date/job/log/etc.).
5. **ABAP-safe**:
   - if ABAP evidence is absent, avoid strong algorithm claims (joins, calculations, routing).
   - if parameter is explicitly “not used/unused”, state it as reserved/unused.

## Preferred Sentence Patterns
- `<PARAM> identifies <business object> and scopes records to <relevant subset/process>.`
- `<PARAM> filters records by <status/date/time/category> to control <monitoring scope>.`
- `<PARAM> is the <document/master key> used for <traceability/drilldown/correlation>.`

## Quality Checklist (must pass)
- [ ] SAP meaning is correct.
- [ ] No contradiction with ABAP evidence.
- [ ] One sentence only.
- [ ] No forbidden generic wording.
- [ ] Reusable across all files where parameter appears.

## Output format
XLSX with columns:
- `parameter`
- `curated_explanation`
- `confidence` (`high` / `medium` / `low`)
- `evidence_notes` (short note: main metadata signal + ABAP presence ratio)

