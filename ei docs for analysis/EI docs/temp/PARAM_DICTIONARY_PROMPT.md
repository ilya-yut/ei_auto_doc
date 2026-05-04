# Parameter Dictionary Generation Prompt (SAP Expert, Concise)

## Goal
Generate one concise, high-quality SAP explanation per parameter for dictionary use.

## Input
- Parameter name (`PARAM`)
- Source files where `PARAM` appears
- Parameter table metadata (Description, Data Element, Domain, Type)
- ABAP section from the respective files (for contradiction checks)

## Required Method
1. Resolve SAP meaning of `PARAM` from DDIC/business semantics first.
2. Compare with ABAP usage in the respective files.
3. Produce one concise canonical sentence that is valid across occurrences.
4. Avoid speculation; include only details supported by DDIC and/or ABAP.
5. Keep wording business-technical, short, and non-generic.

## Output Rule
- Format: `PARAM: <one concise sentence>`
- Length target: ~12-22 words.
- One parameter -> one explanation (same across all rows using that parameter).

## Quality Checklist (must pass)
- [ ] No contradiction with ABAP usage in relevant files.
- [ ] No generic filler (e.g., "technical parameter in monitor set").
- [ ] Uses SAP object semantics (customer/vendor/document/plant/etc.) where known.
- [ ] Mentions DDIC detail only if it improves precision (not noise).
- [ ] Concise and reusable across all occurrences.

## Example (KUNNR)
`KUNNR identifies the customer account and is used to scope records to specific customers across SD/FI flows.`

