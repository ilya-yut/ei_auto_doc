# Prompt: Curated SAP Explanations (Selected Parameters)

## Objective
Generate one concise, SAP-expert explanation per requested parameter:

`ERDAT, CHANGENR, TAB_DESC, ACT_CHNGNO, CHANGE_IND, CHANGE_IND_DESC, CHNGIND, CHNGIND_DESC, OBJECTCLAS, OBJECT_DESC, PLANCHNGNR, UNIT_NEW, UNIT_OLD, MESSAGE, OBJECT, AUFNR`

## Evidence Sources
For each parameter, combine:
1. Parameter table metadata from DOCX files in `EI docs/Part 1..4` (Description, Data Element, Domain, Type).
2. Parameter usage in ABAP sections of the same docs (token evidence only).
3. Existing wording patterns from prior dictionary workflow.

## Hard Rules
1. One parameter -> one canonical sentence.
2. One sentence only, target 8-24 words.
3. No generic placeholders like:
   - "technical selection parameter"
   - "used in this monitor set"
   - "align interpretation with DDIC semantics"
4. No claims that contradict ABAP usage evidence.
5. Prefer business-object semantics (document, change event, object class, order, unit, message).

## Preferred Patterns
- `<PARAM> identifies ... and scopes records to ...`
- `<PARAM> marks ... for change analysis/reporting.`
- `<PARAM> provides ... so output is business-readable.`

## Quality Checklist
- [ ] SAP meaning is correct.
- [ ] Wording is reusable across files.
- [ ] No contradiction with ABAP evidence.
- [ ] Single sentence, concise, non-generic.
- [ ] Word count in range 6..28.

## Output
Create XLSX with columns:
- `parameter`
- `curated_explanation`
- `confidence`
- `evidence_notes`
