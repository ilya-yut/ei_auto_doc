# Exception Indicator: Tables Content Comparison - SW_01_03_TAB_COMPARE

## General Overview

This Exception Indicator compares content between two SAP database tables (or the same table read from two RFC destinations) and surfaces rows that are missing on one side, differ on configured attributes, or sit outside configured aging and duration criteria. It supports reconciliation of master data, staging versus production copies, and interface-fed tables by pairing keys, optional descriptive fields, and attribute columns on each side.

This EI serves as an essential control for data governance and IT operations by:

- Enabling detection of one-sided records and mismatched attribute values before they drive incorrect postings, reporting, or downstream integrations
- Supporting prioritization of reconciliation work by highlighting rows whose reference dates imply staleness or unusual elapsed time since a business event
- Providing visibility into which comparison outcome types are present (records present only on the first side, only on the second side, or attribute-level differences on matched keys) for triage and ownership assignment
- Helping teams evidence periodic table reviews for audits and internal controls over configuration and master data integrity
- Reducing noise during attribute investigations by focusing presentation on attributes that actually diverged between the two sources

Organizations use this monitoring during cutover and hypercare, after transports or data loads, for periodic master-data health checks, and when validating that a central system and satellite systems stay aligned on critical tables.

The EI reads the named tables remotely where configured and relies on standard SAP table metadata services to interpret keys, fields, and dynamic selection fragments supplied for each side.


## Problem Description

Failure to monitor paired table content and attribute alignment creates multiple risks across financial reporting, operational integrity, and compliance.

**Financial and Reporting Issues**

- Unmatched or divergent master records can propagate into incorrect pricing, tax, payment, or valuation results in period-end reporting
- Silent differences between a golden record store and a reporting copy can cause reconciling items that are discovered only during audit or closing
- Interface or batch loads that partially fail may leave sibling systems with incompatible material, customer, or vendor data, distorting margin and revenue analyses
- Long-lived stale records that never appear in comparison results can mask obsolete or fraudulent master data that should have been retired or corrected

**Operational and Control Risks**

- Missing rows on one side of a pair often indicate failed replication, queue backlog, or authorization gaps that will later block transactions or batch jobs
- Attribute mismatches on matched keys can drive wrong ATP, credit, or logistics decisions when each system believes a different truth
- Undetected drift between environments (for example quality versus production) increases the likelihood of failed transports, cutover defects, and emergency fixes
- Teams spend excessive manual effort scanning tables ad hoc when exceptions are not consolidated into a repeatable comparison view

**Management Visibility and Decision-Making Risks**

- Without systematic comparison, executives lack confidence that strategic KPIs and operational dashboards draw from consistent underlying data
- Incident response is delayed when ownership of discrepancies is unclear and exception categories are not classified
- Investment in integration platforms is undermined when business users cannot see whether synchronized objects truly match in content, not only in existence
- Continuous improvement initiatives lack baselines when historical alignment between systems was never measured

## Suggested Resolution

**Immediate Response**

- Review the comparison outcome categories surfaced for the run and assign each cluster to data steward, functional, or basis owners as appropriate
- For rows that exist on only one side, validate whether the gap is an expected scope difference (test data, legal-entity filter) or a genuine synchronization defect
- For attribute-difference rows, open the relevant maintenance transactions for the object type involved and confirm which value reflects the approved business truth
- Capture screenshots or exported lists for high-impact objects to support change requests and communication with integration teams

**System Assessment**

- Confirm that the two sides compared represent the intended scope (same logical table, correct logical system or RFC destination, and comparable organizational slice)
- Re-run with a narrower business slice when volume is high, so teams can clear the highest-risk object types first
- Compare current results to a prior cycle to see whether discrepancies are new regressions or long-standing technical debt
- When aging or elapsed-time information is part of the review, validate that the business event chosen for the clock still matches how the process is managed in operations
- Engage the team responsible for the comparison engine if table metadata or authority errors prevent a complete read

**Corrective Actions**

- Correct master or configuration data in the appropriate maintenance transactions (object-specific, for example material or business partner) and re-execute the comparison to verify closure
- Fix interface, middleware, or batch jobs that omit records or map fields incorrectly, then replay or resend affected payloads where tooling allows
- Adjust replication or distribution rules when legitimate scope differences were mis-modeled
- Document root cause, remediation, and sign-off for audit trail, especially where controls require periodic evidence of table alignment
- Schedule recurring comparison runs ahead of month-end, major releases, and after bulk loads or migrations
- Escalate persistent technical blocks (RFC connectivity, authorization, table availability) to basis and security teams with concrete examples


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ATTR_COMPARE | Character Field Length = 10 | CHAR | 10 | 0 | CHAR10 | CHAR10 |
| 2 | BACKDAYS1 / BACKDAYS2 | Table1- Days Backwards / Table2- Days Backwards |  | 0 | 0 |  |  |
| 3 | COMPARE_STATE | L(Left Out) /R (Right Out) /A | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 4 | DATE_REF_FLD1 / DATE_REF_FLD2 | Table1- Date Ref Field Name / Table2- Date Ref Field Name |  | 0 | 0 |  |  |
| 5 | DEST1 / DEST2 | Table1 - RFC Destination / Table2 - RFC Destination |  | 0 | 0 |  |  |
| 6 | DURATION1 / DURATION2 | Table1-Duration(in Dur.Units) / Table2-Duration(in Dur.Units) | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 7 | DURATION_UNIT1 / DURATION_UNIT2 | Table1-Duration Unit(D/H/M/S) / Table2-Duration Unit(D/H/M/S) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 8 | PROBLEM_ATTR_ONLY | Display Problem Attri Only (X) |  | 0 | 0 |  |  |
| 9 | TAB1 | Table1 Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 10 | TAB1_ATTR1 | Table1 Attr1 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 11 | TAB1_ATTR1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 12 | TAB1_ATTR2 | Table1 Attr2 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 13 | TAB1_ATTR2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 14 | TAB1_ATTR3 | Table1 Attr3 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 15 | TAB1_ATTR3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 16 | TAB1_ATTR4 | Table1 Attr4 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 17 | TAB1_ATTR4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 18 | TAB1_ATTR5 | Table1 Attr5 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 19 | TAB1_ATTR5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 20 | TAB1_COND | Table1 Where Condition |  | 0 | 0 |  |  |
| 21 | TAB1_FLD1 | Table1 -Field Name - 1 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 22 | TAB1_FLD1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 23 | TAB1_FLD2 | Table1 - Field Name - 2 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 24 | TAB1_FLD2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 25 | TAB1_FLD3 | Table1 - Field Name - 3 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 26 | TAB1_FLD3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 27 | TAB1_FLD4 | Table1 - Field Name - 4 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 28 | TAB1_FLD4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 29 | TAB1_FLD5 | Table1 - Field Name - 5 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 30 | TAB1_FLD5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 31 | TAB1_KEY1 | Table1 Key1 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 32 | TAB1_KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 33 | TAB1_KEY2 | Table1 Key2 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 34 | TAB1_KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 35 | TAB1_KEY3 | Table1 Key3 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 36 | TAB1_KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 37 | TAB1_KEY4 | Table1 Key4 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 38 | TAB1_KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 39 | TAB1_KEY5 | Table1 Key5 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 40 | TAB1_KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 41 | TAB2 | Table2 Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 42 | TAB2_ATTR1 | Table2 Attr1 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 43 | TAB2_ATTR1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 44 | TAB2_ATTR2 | Table2 Attr2 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 45 | TAB2_ATTR2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 46 | TAB2_ATTR3 | Table2 Attr3 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 47 | TAB2_ATTR3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 48 | TAB2_ATTR4 | Table2 Attr4 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 49 | TAB2_ATTR4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 50 | TAB2_ATTR5 | Table2 Attr5 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 51 | TAB2_ATTR5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 52 | TAB2_COND | Table2 Where Condition |  | 0 | 0 |  |  |
| 53 | TAB2_FLD1 | Table2 - Field Name - 1 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 54 | TAB2_FLD1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 55 | TAB2_FLD2 | Table2 - Field Name - 2 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 56 | TAB2_FLD2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 57 | TAB2_FLD3 | Table2 - Field Name - 3 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 58 | TAB2_FLD3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 59 | TAB2_FLD4 | Table2 - Field Name - 4 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 60 | TAB2_FLD4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 61 | TAB2_FLD5 | Table2 - Field Name - 5 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 62 | TAB2_FLD5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 63 | TAB2_KEY1 | Table2 Key1 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 64 | TAB2_KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 65 | TAB2_KEY2 | Table2 Key2 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 66 | TAB2_KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 67 | TAB2_KEY3 | Table2 Key3 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 68 | TAB2_KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 69 | TAB2_KEY4 | Table2 Key4 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 70 | TAB2_KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 71 | TAB2_KEY5 | Table2 Key5 Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 72 | TAB2_KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 73 | TIME_REF_FLD1 / TIME_REF_FLD2 | Table1 - Time Ref Field Name / Table2 - Time Ref Field Name |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 79 parameters listed in the Parameters Reference Table above.

**ATTR_COMPARE** (Character Field Length = 10):

Up to ten single-character positions describe, for attribute-level comparison lines, which configured attribute slots differ between the two sides. Each position corresponds in order to the attribute slots you configured for the first table and is set by the comparison logic when values under the same key do not match. Reviewers use this profile to see at a glance which business attributes drive the exception before opening maintenance or integration follow-up.

**BACKDAYS1/BACKDAYS2** (Table1- Days Backwards / Table2- Days Backwards):

Suffix **1**: whole-day count with the first table’s date reference field defines how far back from the evaluation date the dynamic date window starts when that reference is supplied and validated. Suffix **2**: the same meaning for the second table’s extract and its own date reference field.

**BACKDAYS1/BACKDAYS2 and DATE_REF_FLD1/DATE_REF_FLD2 Connection:**

Suffix **1** parameters govern the first table’s business-date window: when a date reference field is maintained, **BACKDAYS1** and **DATE_REF_FLD1** jointly define the lower bound appended to that side’s free-text conditions; if the date reference is not maintained, the backward day count does not define the extract for that side. Suffix **2** parameters behave the same way for the second table with **BACKDAYS2** and **DATE_REF_FLD2**.

**COMPARE_STATE** (L(Left Out) /R (Right Out) /A):

Identifies the kind of comparison row you want the run to retain in the result set—key-only presence on one side versus attribute-level difference lines for matched keys.

**COMPARE_STATE Options:**

- **L**: First-side-only key rows (present on the first table side of the pair, not matched on the second)
- **R**: Second-side-only key rows (present on the second table side, not matched on the first)
- **A**: Matched keys with differing configured attributes (attribute comparison outcome)

**DATE_REF_FLD1/DATE_REF_FLD2** (Table1- Date Ref Field Name / Table2- Date Ref Field Name):

Suffix **1**: date-type column on the first table used with the backward day count to qualify rows and, when type is date, as the calendar anchor for elapsed-time on that side. Suffix **2**: the same role for the second table’s definition and aging.

**DATE_REF_FLD1 Options:**

- **ERDAT**: Common creation or entry date on many business objects
- **AEDAT**: Last change date where change tracking is maintained
- **BUDAT**: Posting date in financial documents
- **LAEDA**: Last change date on material master segments
- **CPUDT**: Computing / capture date on some batch inputs
- **Any other date-type field** defined on the first table when that field better reflects the business moment you need for the control

**DATE_REF_FLD2 Options:**

- **ERDAT**: Creation or entry date on the second table’s objects
- **AEDAT**: Last change date where maintained
- **BUDAT**: Posting date where applicable
- **LAEDA**: Last change date on material-related segments when relevant
- **CPUDT**: Capture date on inbound interface tables when relevant
- **Any other date-type field** on the second table that matches the business clock you want for that source

**DEST1/DEST2** (Table1 - RFC Destination / Table2 - RFC Destination):

Suffix **1**: logical destination when the first table must be read from a specific application server or system other than the default; when blank, the standard destination applies. Suffix **2**: the same for the second table’s read.

**DURATION1/DURATION2** (Table1-Duration(in Dur.Units) / Table2-Duration(in Dur.Units)):

Multivalued numeric bands keeping result lines whose computed elapsed value on each side falls inside the supplied intervals (very new, very old, or specific aging brackets after anchors resolve). Suffix **1** applies to the first side’s computed duration; suffix **2** to the second side’s.

**DURATION_UNIT1/DURATION_UNIT2** (Table1-Duration Unit(D/H/M/S) / Table2-Duration Unit(D/H/M/S)):

Suffix **1**: unit for elapsed-time on the first table when a valid date anchor exists—hours, minutes, days, or full-calendar-day semantics for specialized day-based logic. Suffix **2**: the same role for the second table’s elapsed-time calculation.

**DURATION_UNIT1 Options:**

- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DATE_REF_FLD1/DATE_REF_FLD2, TIME_REF_FLD1/TIME_REF_FLD2, and DURATION_UNIT1/DURATION_UNIT2 Connection:**

Suffix **1**: for each result line on the first table, the business date (when the configured field is date-type) and optional time of day refine the elapsed-time start; **DURATION_UNIT1** defines how elapsed time to the evaluation moment is expressed in that side’s duration fields. If no valid date anchor exists for a line, that side’s duration is not driven by this path. Suffix **2**: **DATE_REF_FLD2**, **TIME_REF_FLD2**, and **DURATION_UNIT2** behave the same way for the second table’s rows.

**DURATION1/DURATION2 and DURATION_UNIT1/DURATION_UNIT2 Connection:**

Suffix **1**: multivalued duration bands on the first side are evaluated against durations computed in **DURATION_UNIT1** (and the date/time anchors); configure the unit before tuning numeric intervals (e.g. whole days vs hours). Suffix **2**: the second table’s bands align with values computed in **DURATION_UNIT2**; set that unit before tuning thresholds on that side.

**DURATION_UNIT2 Options:**

- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**PROBLEM_ATTR_ONLY** (Display Problem Attri Only (X)):

Controls whether attribute values are shown for every configured attribute column on attribute lines, or only for those attribute positions flagged as differing in the comparison profile.

**PROBLEM_ATTR_ONLY Options:**

- **X**: Show values only for attribute slots that differ between the two sides on that line
- ** ** (space): Show values for all configured attribute slots on attribute lines

**TAB1** (Table1 Name):

Technical name of the first table participating in the comparison. Together with the destination, it identifies which dataset forms the left-hand side of the pairing.

**TAB1_ATTR1-5 / TAB2_ATTR1-5** (Table1 Attr1–Attr5 Field Name / Table2 Attr1–Attr5 Field Name):

Up to five attribute field names per side, compared slot-by-slot for matched keys (slot 1 to slot 1, and so on). Leave unused slots blank when fewer than five attributes participate. The second table’s list mirrors the first so like-for-like values are evaluated on the same business keys.

**TAB1_ATTR1-5_V / TAB2_ATTR1-5_V** (Short Description / Short Description):

Repository short texts for each side’s attribute field names—aligned by slot with **TAB1_ATTR1-5 / TAB2_ATTR1-5**—so output headers stay readable without looking up data-element descriptions manually.

**TAB1_COND** (Table1 Where Condition):

Additional free-text SQL fragment for the first table’s extract, combined with any generated date lower bound. Use for explicit business slices such as company code, plant, or message type that must be enforced on that side.

**TAB1_FLD1-5 / TAB2_FLD1-5** (Table1 Field Name 1–5 / Table2 Field Name 1–5):

Up to five additional non-key fields per table carried on each result line for context (status, name, quantity, and similar) beside keys and attributes in the review layout.

**TAB1_FLD1-5_V / TAB2_FLD1-5_V** (Short Description / Short Description):

Short descriptions for each side’s contextual fields, slot-aligned with **TAB1_FLD1-5 / TAB2_FLD1-5**, so column headers stay self-explanatory for business users.

**TAB1_KEY1-5 / TAB2_KEY1-5** (Table1 Key1–Key5 Field Name / Table2 Key1–Key5 Field Name):

Up to five key field names per table defining how rows match across the pair. List keys in the same business order on both sides so each line compares one logical object.

**TAB1_KEY1-5_V / TAB2_KEY1-5_V** (Short Description / Short Description):

Short descriptions for each side’s key components, shown with key values on output lines for audits and operational triage.

**TAB2** (Table2 Name):

Technical name of the second table in the pair—the right-hand dataset for the comparison.

**TAB2_COND** (Table2 Where Condition):

Supplemental extract fragment for the second table, combined with any generated date condition for that side, to scope organizational or functional subsets that must be compared.

**TIME_REF_FLD1/TIME_REF_FLD2** (Table1 - Time Ref Field Name / Table2 - Time Ref Field Name):

Suffix **1**: optional time-of-day with the date anchor to refine the elapsed-time start when sub-day precision matters. Suffix **2**: optional time field on the second table with the same refinement role.

**TIME_REF_FLD1 Options:**

- **ERZET**: Creation time paired with creation date on many documents
- **AEZET**: Time of last change when maintained
- **CPUTM**: Computing time on batch or interface headers
- **UZEIT**: Time component where stored separately from date on some objects
- **Any other time-type field** on the first table that matches the business event clock

**TIME_REF_FLD2 Options:**

- **ERZET**: Entry time for the second table’s records
- **AEZET**: Change time where available
- **CPUTM**: Capture time on inbound batches
- **UZEIT**: Separate time part when applicable
- **Any other time-type field** on the second table that fits the process being controlled


### Parameter Relationships

**RFC Destination and Table Pairing**

- **DEST1** and **TAB1** jointly identify which physical dataset is read for the first side of the comparison (system context plus table name).
- **DEST2** and **TAB2** do the same for the second side, enabling like-for-like comparison across two systems or two logical copies.

**Key Structure and Attribute Alignment**

- **TAB1_KEY1** through **TAB1_KEY5** must mirror **TAB2_KEY1** through **TAB2_KEY5** in business meaning and order so the engine pairs the same logical object on both sides.
- **TAB1_ATTR1** through **TAB1_ATTR5** and **TAB2_ATTR1** through **TAB2_ATTR5** are interpreted positionally: slot 1 compares to slot 1, and so on, for matched keys.
- **TAB1_FLD1** through **TAB1_FLD5** and **TAB2_FLD1** through **TAB2_FLD5** provide parallel contextual columns that travel with each result line for the same slot index.

**Extract Scope and Business Slice**

- **TAB1_COND** and **TAB2_COND** extend each side’s dynamic selection; they are combined with any automatically appended date-lower-bound clause driven by **BACKDAYS1**/**DATE_REF_FLD1** and **BACKDAYS2**/**DATE_REF_FLD2** respectively.

**Elapsed Time and Thresholds per Side**

- On each result line, **DATE_REF_FLD1**, optional **TIME_REF_FLD1**, and **DURATION_UNIT1** shape the first table’s elapsed value that **DURATION1** bands then evaluate; the same pattern applies to **DATE_REF_FLD2**, **TIME_REF_FLD2**, **DURATION_UNIT2**, and **DURATION2** for the second table.

**Exception Category and Attribute Profile**

- **COMPARE_STATE** and **ATTR_COMPARE** work together: the state tells whether the line is a one-sided key or an attribute mismatch, and the attribute profile indicates which attribute slots differ when the state represents attribute-level comparison.
- **PROBLEM_ATTR_ONLY** uses the attribute profile to decide whether all configured attribute values are shown or only those positions flagged as different.

**Example Configuration:**

- **TAB1** = central material table, **TAB2** = satellite material mirror, **TAB1_KEY1**/**TAB2_KEY1** = material number, **TAB1_ATTR1**/**TAB2_ATTR1** = gross weight — focuses reconciliation on identity plus one critical attribute.

**Result:**

- Exceptions list only materials present on one side or with differing gross weight, with keys and descriptions readable from the paired key and short-text parameters.


### Default Values

- **BACKDAYS1** — Default: `1` (applied when not supplied before the first-table date condition is built)
- **BACKDAYS2** — Default: `1` (same role for the second table)
- **DURATION_UNIT1** — Default: `D` (days as the unit for the first side’s elapsed-time calculation when not supplied)
- **DURATION_UNIT2** — Default: `D` (days for the second side when not supplied)

### Practical Configuration Examples

**Use Case 1: Cross-System Master Record Alignment**

```
TAB1 = KNA1
TAB2 = KNA1
DEST1 = PRD_ECC
DEST2 = CRM_PRD
TAB1_KEY1 = KUNNR
TAB2_KEY1 = KUNNR
```

**Purpose:** Compare customer master between the ERP customer table read from the core system and the same table name on a connected system so one-sided customers or key gaps surface for the integration team after organizational changes.

**Use Case 2: Recent Material Changes Only**

```
TAB1 = MARA
TAB2 = MARA
DATE_REF_FLD1 = LAEDA
BACKDAYS1 = 14
COMPARE_STATE = A
```

**Purpose:** Limit the first side to materials touched in the last two weeks (using the last-change date) while focusing the exception list on attribute-difference rows, which supports hypercare after a material migration or mass update.

**Use Case 3: Full-Day Aging Bracket with Duration Focus**

```
TAB1 = MARA
TAB2 = MARA
DATE_REF_FLD1 = LAEDA
DURATION_UNIT1 = F
DURATION1 = 30
TAB1_KEY1 = MATNR
TAB2_KEY1 = MATNR
COMPARE_STATE = A
```

**Purpose:** Use last-change date on material master with full-calendar-day duration semantics, then keep only rows whose elapsed time on the first side matches the 30-day band—suited to post-migration reviews when calendar-day aging must match cutover calendars.

**Use Case 4: Problem-Attribute Triage on Matched Keys**

```
TAB1 = LFA1
TAB2 = LFA1
TAB1_KEY1 = LIFNR
TAB2_KEY1 = LIFNR
TAB1_ATTR1 = STRAS
TAB2_ATTR1 = STRAS
PROBLEM_ATTR_ONLY = X
COMPARE_STATE = A
```

**Purpose:** For vendors with matched keys but differing street address in the first attribute slot, show only the differing attribute values to accelerate address verification without scrolling past unchanged attributes.


### EI Function Structure

## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_TAB_COMPARE | ATTR_COMPARE | Character Field Length = 10 | CHAR(10) | CHAR10 |
| /SKN/S_SW_01_03_TAB_COMPARE | COMPARE_STATE | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_01_03_TAB_COMPARE | DURATION1 | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_03_TAB_COMPARE | DURATION2 | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_03_TAB_COMPARE | DURATION_UNIT1 | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_03_TAB_COMPARE | DURATION_UNIT2 | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1 | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_ATTR5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_FLD5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB1_KEY5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2 | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_ATTR5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_FLD5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_COMPARE | TAB2_KEY5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |

### ABAP Code

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_03_TAB_COMPARE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_03_TAB_COMPARE OPTIONAL
*"----------------------------------------------------------------------
DATA: R_TAB1_COND TYPE RANGE OF SO_TEXT,
      R_TAB2_COND TYPE RANGE OF SO_TEXT,
      R_COMPARE_STATE TYPE RANGE OF CHAR1,
      R_ATTR_COMPARE  TYPE RANGE OF CHAR10.
DATA: RS_TAB1_COND LIKE LINE OF R_TAB1_COND,
      RS_TAB2_COND LIKE LINE OF R_TAB2_COND,
      RS_COMPARE_STATE LIKE LINE OF R_COMPARE_STATE,
      RS_ATTR_COMPARE LIKE LINE OF R_ATTR_COMPARE.
"--- Add Parameter for Compare State & Attr Compare String
DATA: LV_DEST1 TYPE RFCDEST,
      LV_TAB1  TYPE TABNAME ,
      LV_TAB1_KEY1 TYPE FIELDNAME,
      LV_TAB1_KEY2 TYPE FIELDNAME,
      LV_TAB1_KEY3 TYPE FIELDNAME,
      LV_TAB1_KEY4 TYPE FIELDNAME,
      LV_TAB1_KEY5 TYPE FIELDNAME,
      LV_TAB1_ATTR1 TYPE FIELDNAME,
      LV_TAB1_ATTR2 TYPE FIELDNAME,
      LV_TAB1_ATTR3 TYPE FIELDNAME,
      LV_TAB1_ATTR4 TYPE FIELDNAME,
      LV_TAB1_ATTR5 TYPE FIELDNAME.
DATA: LV_DEST2 TYPE RFCDEST,
      LV_TAB2  TYPE TABNAME ,
      LV_TAB2_KEY1 TYPE FIELDNAME,
      LV_TAB2_KEY2 TYPE FIELDNAME,
      LV_TAB2_KEY3 TYPE FIELDNAME,
      LV_TAB2_KEY4 TYPE FIELDNAME,
      LV_TAB2_KEY5 TYPE FIELDNAME,
      LV_TAB2_ATTR1 TYPE FIELDNAME,
      LV_TAB2_ATTR2 TYPE FIELDNAME,
      LV_TAB2_ATTR3 TYPE FIELDNAME,
      LV_TAB2_ATTR4 TYPE FIELDNAME,
      LV_TAB2_ATTR5 TYPE FIELDNAME.
DATA: LV_PROBLEM_ATTR_ONLY TYPE CHAR1.
"------------------------------------------
DATA: LS_KEY_FIELDS_1 TYPE RFC_DB_FLD,
      LS_KEY_FIELDS_2 TYPE RFC_DB_FLD,
      LS_COND TYPE  RFC_DB_OPT,
      LS_ATTR_FIELDS_1 TYPE RFC_DB_FLD,
      LS_ATTR_FIELDS_2 TYPE RFC_DB_FLD,
      LS_FIELDS_1 TYPE  RFC_DB_FLD ,
      LS_FIELDS_2 TYPE  RFC_DB_FLD ,
      LS_FLD_FIELDS_1 TYPE RFC_DB_FLD,
      LS_FLD_FIELDS_2 TYPE RFC_DB_FLD,
      LS_DATA_1 TYPE TAB512,
      LS_DATA_2 TYPE TAB512,
      LS_KEY_ATTR TYPE /SKN/S_SW_TAB_COMPARE_INDEX,
      LS_KEY_OUT TYPE /SKN/S_SW_TAB_COMPARE_INDEX .
DATA: LT_KEY_FIELDS_1 LIKE TABLE OF LS_KEY_FIELDS_1,
      LT_KEY_FIELDS_2 LIKE TABLE OF LS_KEY_FIELDS_2,
      LT_COND   LIKE TABLE OF LS_COND,
      LT_COND_1 LIKE TABLE OF LS_COND,
      LT_COND_2 LIKE TABLE OF LS_COND,
      LT_ATTR_FIELDS_1 LIKE TABLE OF LS_ATTR_FIELDS_1,
      LT_ATTR_FIELDS_2 LIKE TABLE OF LS_ATTR_FIELDS_2,
      LT_FLD_FIELDS_1 LIKE TABLE OF LS_FLD_FIELDS_1,
      LT_FLD_FIELDS_2 LIKE TABLE OF LS_FLD_FIELDS_2,
      LT_FIELDS_1 LIKE TABLE OF LS_FIELDS_1 ,
      LT_FIELDS_2 LIKE TABLE OF LS_FIELDS_2 ,
      LT_DATA_1 LIKE TABLE OF LS_DATA_1,
      LT_DATA_2 LIKE TABLE OF LS_DATA_2,
      LT_KEY_ATTR LIKE TABLE OF LS_KEY_ATTR,
      LT_KEY_OUT LIKE TABLE OF LS_KEY_OUT.
"------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA.
DATA: LV_TEMP_1 TYPE STRING,
      LV_TEMP_2 TYPE STRING.
DATA: LS_TAB_FIELDS TYPE  RFC_DB_FLD ,
      LT_ALL_FIELDS LIKE TABLE OF LS_TAB_FIELDS,
      LS_OPTIONS_ALL TYPE  RFC_DB_OPT,
      LT_OPTIONS_ALL LIKE TABLE OF LS_OPTIONS_ALL,
      LS_DATA_ALL TYPE  TAB512,
      LT_DATA_ALL LIKE TABLE OF LS_DATA_ALL.
DATA : REF_DATE TYPE D,
       REF_TIME TYPE T.
DATA : IS_CHECK_DATE(1) TYPE C.
DATA : TIME_DIFF TYPE  INT4 .
DATA : DATE_FROM LIKE SY-DATUM.
DATA: LV_TEMP TYPE STRING.
CONSTANTS: LC_IS_PROBLEM_ATTR(1) TYPE C VALUE 'X'.
DATA: LV_DISPLAY_ATTR(1) TYPE C,
      LC_SHIFT TYPE I.
DATA : SY_TABIX LIKE SY-TABIX .
DATA: SY_DATLO LIKE SY-DATUM,
      SY_TIMLO LIKE SY-UZEIT.
""_set_sys_date_time lv_sw_dest sy_datlo sy_timlo.
"----------------------------------------------------
*define select_single .
* clear lv_&1.
* LOOP AT T_SELECT WHERE FIELDNM = '&1'.
*   MOVE T_SELECT-low TO lv_&1.
*     exit.
*   ENDLOOP.
*end-of-definition .
*
*define select_multy .
* refresh R_&1.
* LOOP AT T_SELECT WHERE FIELDNM = '&1'.
*   MOVE-CORRESPONDING T_SELECT TO RS_&1.
*   APPEND RS_&1 to R_&1.
* ENDLOOP.
*end-of-definition .
DEFINE POPULATE_FIELD .
  " &1 - 'KEY'/'ATTR' / FLD
  " &2 - TAB Index
  " &3 - Field Index
*  clear ls_ATTR_FIELDS_1.
*  if lv_TAB1_ATTR1 is not initial.
*    ls_ATTR_FIELDS_1-FIELDNAME = lv_TAB1_ATTR1.
*    append ls_ATTR_FIELDS_1 to lt_ATTR_FIELDS_1.
*  endif.
 CLEAR LS_&1_FIELDS_&2.
 IF LV_TAB&2_&1&3 IS NOT INITIAL.
   LS_&1_FIELDS_&2-FIELDNAME = LV_TAB&2_&1&3.
   APPEND LS_&1_FIELDS_&2 TO LT_&1_FIELDS_&2.
  ENDIF.
END-OF-DEFINITION .
DEFINE POPULATE_OUTPUT_KEY .
  " &1 - TAB Index
  " &2 - Field Index
 READ TABLE LT_DATA_&1 INTO LS_DATA_&1 INDEX LS_KEY_OUT-LINE&1.
 IF SY-SUBRC IS INITIAL.
   READ TABLE LT_KEY_FIELDS_&1 INTO LS_KEY_FIELDS_&1 INDEX &2. "!!!
   IF SY-SUBRC = 0.
     READ TABLE LT_FIELDS_&1 INTO LS_FIELDS_&1
                   WITH KEY FIELDNAME = LS_KEY_FIELDS_&1-FIELDNAME.
     IF SY-SUBRC = 0.
       LV_TEMP_&1 = LS_DATA_&1-WA+LS_FIELDS_&1-OFFSET(LS_FIELDS_&1-LENGTH).
       LS_DATA-TAB&1_KEY&2_V = LV_TEMP_&1.
       LS_DATA-TAB&1_KEY&2 = LS_KEY_FIELDS_&1-FIELDNAME.
     ENDIF.
   ENDIF.
 ENDIF.
END-OF-DEFINITION .
DEFINE POPULATE_OUTPUT_KEY_SET .
  " &1 - TAB Index
 POPULATE_OUTPUT_KEY &1 1.
 POPULATE_OUTPUT_KEY &1 2.
 POPULATE_OUTPUT_KEY &1 3.
 POPULATE_OUTPUT_KEY &1 4.
 POPULATE_OUTPUT_KEY &1 5.
END-OF-DEFINITION .
DEFINE POPULATE_ATTR_KEY .
  " &1 - TAB Index
  " &2 - Field Index
 READ TABLE LT_DATA_&1 INTO LS_DATA_&1 INDEX LS_KEY_ATTR-LINE&1.
 IF SY-SUBRC IS INITIAL.
   READ TABLE LT_KEY_FIELDS_&1 INTO LS_KEY_FIELDS_&1 INDEX &2. "!!!
   IF SY-SUBRC = 0.
     READ TABLE LT_FIELDS_&1 INTO LS_FIELDS_&1
                   WITH KEY FIELDNAME = LS_KEY_FIELDS_&1-FIELDNAME.
     IF SY-SUBRC = 0.
       LV_TEMP_&1 = LS_DATA_&1-WA+LS_FIELDS_&1-OFFSET(LS_FIELDS_&1-LENGTH).
       LS_DATA-TAB&1_KEY&2_V = LV_TEMP_&1.
       LS_DATA-TAB&1_KEY&2 = LS_KEY_FIELDS_&1-FIELDNAME.
     ENDIF.
   ENDIF.
 ENDIF.
END-OF-DEFINITION .
DEFINE POPULATE_ATTR_KEY_SET .
  " &1 - TAB Index
 POPULATE_ATTR_KEY &1 1.
 POPULATE_ATTR_KEY &1 2.
 POPULATE_ATTR_KEY &1 3.
 POPULATE_ATTR_KEY &1 4.
 POPULATE_ATTR_KEY &1 5.
END-OF-DEFINITION .
DEFINE POPULATE_OUTPUT_ATTR .
  " &1 - TAB Index
  " &2 - Field Index
 LV_DISPLAY_ATTR = 'X'.
 IF LV_PROBLEM_ATTR_ONLY IS NOT INITIAL. " display Problem Attributes Only
   LC_SHIFT = &2 - 1.
   IF LS_DATA-ATTR_COMPARE+LC_SHIFT(1) <> LC_IS_PROBLEM_ATTR.
     CLEAR LV_DISPLAY_ATTR.
   ENDIF.
 ENDIF.
 READ TABLE LT_DATA_&1 INTO LS_DATA_&1 INDEX LS_KEY_ATTR-LINE&1.
 IF SY-SUBRC IS INITIAL.
   READ TABLE LT_ATTR_FIELDS_&1 INTO LS_ATTR_FIELDS_&1 INDEX &2. "!!!
   IF SY-SUBRC = 0.
     READ TABLE LT_FIELDS_&1 INTO LS_FIELDS_&1
                   WITH KEY FIELDNAME = LS_ATTR_FIELDS_&1-FIELDNAME.
     IF SY-SUBRC = 0.
       LV_TEMP_&1 = LS_DATA_&1-WA+LS_FIELDS_&1-OFFSET(LS_FIELDS_&1-LENGTH).
       IF LV_DISPLAY_ATTR IS NOT INITIAL.
         LS_DATA-TAB&1_ATTR&2_V = LV_TEMP_&1.
       ENDIF.
       LS_DATA-TAB&1_ATTR&2 = LS_ATTR_FIELDS_&1-FIELDNAME.
     ENDIF.
   ENDIF.
 ENDIF.
END-OF-DEFINITION .
DEFINE POPULATE_ATTR_SET .
  " &1 - TAB Index
 POPULATE_OUTPUT_ATTR &1 1.
 POPULATE_OUTPUT_ATTR &1 2.
 POPULATE_OUTPUT_ATTR &1 3.
 POPULATE_OUTPUT_ATTR &1 4.
 POPULATE_OUTPUT_ATTR &1 5.
END-OF-DEFINITION .
DEFINE POPULATE_OUTPUT_FLD .
  " &1 - TAB Index
  " &2 - Field Index
 READ TABLE LT_DATA_&1 INTO LS_DATA_&1 INDEX LS_KEY_ATTR-LINE&1.
 IF SY-SUBRC IS INITIAL.
   READ TABLE LT_FLD_FIELDS_&1 INTO LS_FLD_FIELDS_&1 INDEX &2. "!!!
   IF SY-SUBRC = 0.
     READ TABLE LT_FIELDS_&1 INTO LS_FIELDS_&1
                   WITH KEY FIELDNAME = LS_FLD_FIELDS_&1-FIELDNAME.
     IF SY-SUBRC = 0.
       LV_TEMP_&1 = LS_DATA_&1-WA+LS_FIELDS_&1-OFFSET(LS_FIELDS_&1-LENGTH).
       LS_DATA-TAB&1_FLD&2_V = LV_TEMP_&1.
       LS_DATA-TAB&1_FLD&2 = LS_FLD_FIELDS_&1-FIELDNAME.
     ENDIF.
   ENDIF.
 ENDIF.
END-OF-DEFINITION .
DEFINE POPULATE_FLD_SET .
  " &1 - TAB Index
 POPULATE_OUTPUT_FLD &1 1.
 POPULATE_OUTPUT_FLD &1 2.
 POPULATE_OUTPUT_FLD &1 3.
 POPULATE_OUTPUT_FLD &1 4.
 POPULATE_OUTPUT_FLD &1 5.
END-OF-DEFINITION .
DEFINE ADD_DATE_REF_CONDITION .
  " &1 - TAB Index
 REFRESH: LT_ALL_FIELDS, LT_ALL_FIELDS, LT_DATA_ALL.
 IF LV_DATE_REF_FLD&1 IS NOT INITIAL.
   CALL FUNCTION 'RFC_READ_TABLE'
     DESTINATION    LV_DEST&1
     EXPORTING
       QUERY_TABLE                = LV_TAB&1
*      DELIMITER                  = ' '
       NO_DATA                    = 'X'
*      ROWSKIPS                   = 0
*      ROWCOUNT                   = 0
     TABLES
       OPTIONS                    = LT_OPTIONS_ALL
       FIELDS                     = LT_ALL_FIELDS
       DATA                       = LT_DATA_ALL
    EXCEPTIONS
      TABLE_NOT_AVAILABLE        = 1
      TABLE_WITHOUT_DATA         = 2
      OPTION_NOT_VALID           = 3
      FIELD_NOT_VALID            = 4
      NOT_AUTHORIZED             = 5
      DATA_BUFFER_EXCEEDED       = 6
      OTHERS                     = 7.
   IF SY-SUBRC <> 0.
* Implement suitable error handling here
   ENDIF.
     READ TABLE LT_ALL_FIELDS INTO LS_TAB_FIELDS
           WITH KEY FIELDNAME = LV_DATE_REF_FLD&1.
     IF SY-SUBRC = 0.
       DATE_FROM = SY-DATUM - LV_BACKDAYS&1 .
       CONCATENATE '''' DATE_FROM '''' INTO LV_TEMP.
       CONCATENATE LV_DATE_REF_FLD&1 'GE' LV_TEMP
          INTO LS_COND-TEXT SEPARATED BY ' '.
       IF LT_COND_&1[] IS NOT INITIAL.
         CONCATENATE 'AND' LS_COND-TEXT INTO LS_COND-TEXT SEPARATED BY ' '.
       ENDIF.
       APPEND LS_COND-TEXT TO LT_COND_&1.
     ENDIF.
 ENDIF.
END-OF-DEFINITION .
DEFINE POPULATE_DATE_TIME_REF_FIELD .
  " &1 - TAB Index
  CLEAR : REF_DATE,
          REF_TIME,
          IS_CHECK_DATE,
          TIME_DIFF.
  IF SY_DATLO IS INITIAL AND SY_TIMLO IS INITIAL.
    _GET_CURRENT_DATE_TIME ' ' LV_DEST&1 SY_DATLO SY_TIMLO.
  ENDIF.
   IF LV_DATE_REF_FLD&1 IS NOT INITIAL.
     READ TABLE LT_FIELDS_&1 INTO LS_FIELDS_&1
                   WITH KEY FIELDNAME = LV_DATE_REF_FLD&1.
     IF SY-SUBRC = 0.
       IF LS_FIELDS_&1-TYPE = 'D'.
         LV_TEMP = LS_DATA_&1-WA+LS_FIELDS_&1-OFFSET(LS_FIELDS_&1-LENGTH).
         REF_DATE = LV_TEMP.
         IS_CHECK_DATE = 'X'.
       ENDIF.
     ENDIF.
     REF_TIME = SY_TIMLO.  "!!!!
   ENDIF.
   IF LV_TIME_REF_FLD&1 IS NOT INITIAL.
     READ TABLE LT_FIELDS_&1 INTO LS_FIELDS_&1
                   WITH KEY FIELDNAME = LV_TIME_REF_FLD&1.
     IF SY-SUBRC = 0.
       IF LS_FIELDS_&1-TYPE = 'T'.
         LV_TEMP = LS_DATA_&1-WA+LS_FIELDS_&1-OFFSET(LS_FIELDS_&1-LENGTH).
         REF_TIME = LV_TEMP.
       ENDIF.
     ENDIF.
   ENDIF.
   IF IS_CHECK_DATE IS NOT INITIAL.
     CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = REF_DATE
          T_FROM            = REF_TIME
          D_TO              = SY_DATLO
          T_TO              = SY_TIMLO
          TIME_UNIT         = LV_DURATION_UNIT&1 " 'D'
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
     IF SY-SUBRC = 0.
     ENDIF.
   ENDIF.
   LS_DATA-DURATION&1 = TIME_DIFF.
   LS_DATA-DURATION_UNIT&1 = LV_DURATION_UNIT&1.
END-OF-DEFINITION .
"----------------------------------------------------
"""20-11-18
DATA_SINGLE: BACKDAYS1       INT4,
             DATE_REF_FLD1   NAME_FELD,
             TIME_REF_FLD1   NAME_FELD,
             DURATION_UNIT1  /SKN/E_SW_DURATION_UNIT,
             TAB1_FLD1      FIELDNAME,
             TAB1_FLD2      FIELDNAME,
             TAB1_FLD3      FIELDNAME,
             TAB1_FLD4      FIELDNAME,
             TAB1_FLD5      FIELDNAME.
DATA_SINGLE: BACKDAYS2       INT4,
             DATE_REF_FLD2   NAME_FELD,
             TIME_REF_FLD2   NAME_FELD,
             DURATION_UNIT2  /SKN/E_SW_DURATION_UNIT,
             TAB2_FLD1      FIELDNAME,
             TAB2_FLD2      FIELDNAME,
             TAB2_FLD3      FIELDNAME,
             TAB2_FLD4      FIELDNAME,
             TAB2_FLD5      FIELDNAME.
 LV_BACKDAYS1 = 1.
 LV_DURATION_UNIT1 = 'D'.
 LV_BACKDAYS2 = 1.
 LV_DURATION_UNIT2 = 'D'.
DATA_MULTY:  DURATION1  /SKN/E_SW_DURATION,
             DURATION2  /SKN/E_SW_DURATION.
**-- Fill Selection Option Tables
SELECT_SINGLE: DEST1,
               TAB1,
               TAB1_KEY1,
               TAB1_KEY2,
               TAB1_KEY3,
               TAB1_KEY4,
               TAB1_KEY5,
               TAB1_ATTR1,
               TAB1_ATTR2,
               TAB1_ATTR3,
               TAB1_ATTR4,
               TAB1_ATTR5.
SELECT_SINGLE: DEST2,
               TAB2,
               TAB2_KEY1,
               TAB2_KEY2,
               TAB2_KEY3,
               TAB2_KEY4,
               TAB2_KEY5,
               TAB2_ATTR1,
               TAB2_ATTR2,
               TAB2_ATTR3,
               TAB2_ATTR4,
               TAB2_ATTR5.
"""20-11-18
SELECT_SINGLE:BACKDAYS1,
              DATE_REF_FLD1,
              TIME_REF_FLD1,
              DURATION_UNIT1,
              TAB1_FLD1,
              TAB1_FLD2 ,
              TAB1_FLD3,
              TAB1_FLD4,
              TAB1_FLD5.
SELECT_SINGLE:BACKDAYS2,
              DATE_REF_FLD2,
              TIME_REF_FLD2,
              DURATION_UNIT2,
              TAB2_FLD1,
              TAB2_FLD2 ,
              TAB2_FLD3,
              TAB2_FLD4,
              TAB2_FLD5.
SELECT_SINGLE: PROBLEM_ATTR_ONLY.
SELECT_MULTY: TAB1_COND,
              TAB2_COND,
              COMPARE_STATE,
              ATTR_COMPARE,
              DURATION1,
              DURATION2.
 DATA_SINGLE:   SW_DEST RFCDEST.
 SELECT_SINGLE: SW_DEST.
 IF LV_DEST1 IS INITIAL.
   LV_DEST1 = LV_SW_DEST.
 ENDIF.
 IF LV_DEST2 IS INITIAL.
   LV_DEST2 = LV_SW_DEST.
 ENDIF.
 CLEAR IS_ALERT .
 REFRESH T_DATA.
 "---- Prepare Input Parameters (tables)
 REFRESH: LT_KEY_FIELDS_1,
          LT_KEY_FIELDS_2,
          LT_ATTR_FIELDS_1,
          LT_ATTR_FIELDS_2,
          LT_FLD_FIELDS_1,
          LT_FLD_FIELDS_2,
          LT_COND_1,
          LT_COND_2.
 "--- Fill Key Fields
  REFRESH: LT_KEY_FIELDS_1,
           LT_KEY_FIELDS_2.
*  clear ls_KEY_FIELDS_1.
*  if lv_TAB1_KEY1 is not initial.
*    ls_KEY_FIELDS_1-FIELDNAME = lv_TAB1_KEY1.
*    append ls_KEY_FIELDS_1 to lt_KEY_FIELDS_1.
*  endif.
  POPULATE_FIELD: KEY 1 1,
                  KEY 1 2,
                  KEY 1 3,
                  KEY 1 4,
                  KEY 1 5.
 POPULATE_FIELD: KEY 2 1,
                 KEY 2 2,
                 KEY 2 3,
                 KEY 2 4,
                 KEY 2 5.
 "--- Fill Attr Fields
  REFRESH: LT_ATTR_FIELDS_1,
           LT_ATTR_FIELDS_2.
*  clear ls_ATTR_FIELDS_1.
*  if lv_TAB1_ATTR1 is not initial.
*    ls_ATTR_FIELDS_1-FIELDNAME = lv_TAB1_ATTR1.
*    append ls_ATTR_FIELDS_1 to lt_ATTR_FIELDS_1.
*  endif.
  POPULATE_FIELD: ATTR 1 1,
                  ATTR 1 2,
                  ATTR 1 3,
                  ATTR 1 4,
                  ATTR 1 5.
  POPULATE_FIELD: ATTR 2 1,
                  ATTR 2 2,
                  ATTR 2 3,
                  ATTR 2 4,
                  ATTR 2 5.
  REFRESH: LT_FLD_FIELDS_1,
           LT_FLD_FIELDS_2.
  POPULATE_FIELD: FLD 1 1,
                  FLD 1 2,
                  FLD 1 3,
                  FLD 1 4,
                  FLD 1 5.
  POPULATE_FIELD: FLD 2 1,
                  FLD 2 2,
                  FLD 2 3,
                  FLD 2 4,
                  FLD 2 5.
 "--- Fill Condition Criteria
 REFRESH LT_COND_1.
 CLEAR LS_COND.
 LOOP AT R_TAB1_COND INTO RS_TAB1_COND.
   LS_COND-TEXT = RS_TAB1_COND-LOW.
   APPEND LS_COND TO LT_COND_1.
 ENDLOOP.
 ADD_DATE_REF_CONDITION 1.
 REFRESH LT_COND_2.
 CLEAR LS_COND.
 LOOP AT R_TAB2_COND INTO RS_TAB2_COND.
   LS_COND-TEXT = RS_TAB2_COND-LOW.
   APPEND LS_COND TO LT_COND_2.
 ENDLOOP.
  ADD_DATE_REF_CONDITION 2.
  "--- Compare Tables Content
    CALL FUNCTION '/SKN/F_SW_TAB_COMPARE'
      EXPORTING
        DEST1                 = LV_DEST1
        TAB_NAME1             = LV_TAB1
        DEST2                 = LV_DEST2
        TAB_NAME2             = LV_TAB2
     TABLES
       T_KEY_FIELDS_1        = LT_KEY_FIELDS_1
       T_ATTR_FIELDS_1       = LT_ATTR_FIELDS_1
       T_FLD_FIELDS_1        = LT_FLD_FIELDS_1
       T_COND_1              = LT_COND_1
       T_KEY_FIELDS_2        = LT_KEY_FIELDS_2
       T_ATTR_FIELDS_2       = LT_ATTR_FIELDS_2
       T_FLD_FIELDS_2        = LT_FLD_FIELDS_2
       T_COND_2              = LT_COND_2
       T_FIELDS_1            = LT_FIELDS_1
       T_FIELDS_2            = LT_FIELDS_2
        T_DATA_1              = LT_DATA_1
        T_DATA_2              = LT_DATA_2
        T_KEY_ATTR            = LT_KEY_ATTR
        T_KEY_OUT             = LT_KEY_OUT
      EXCEPTIONS
        TABLE_1_PROBLEM       = 1
        TABLE_2_PROBLEM       = 2
        OTHERS                = 3.
    IF SY-SUBRC <> 0.
      "--- Fill T_DATA with Error Message
    ENDIF.
    "--- Fill Output Table
    REFRESH T_DATA.
    "--- Out Key Data
    LOOP AT LT_KEY_OUT INTO LS_KEY_OUT.
      MOVE-CORRESPONDING LS_KEY_OUT TO LS_KEY_ATTR.   " for  populate_fld_set
     CLEAR LS_DATA.
     LS_DATA-TAB1 = LV_TAB1.
     LS_DATA-TAB2 = LV_TAB2.
     LS_DATA-COMPARE_STATE  = LS_KEY_OUT-COMPARE_STATE.
     IF LS_DATA-COMPARE_STATE = 'L'.
       POPULATE_OUTPUT_KEY_SET 1.
       POPULATE_FLD_SET 1.
       POPULATE_DATE_TIME_REF_FIELD 1.
     ELSE.
       POPULATE_OUTPUT_KEY_SET 2.
       POPULATE_FLD_SET 2.
       POPULATE_DATE_TIME_REF_FIELD 2.
     ENDIF.
     APPEND LS_DATA TO T_DATA.
    ENDLOOP.
    "--- Out Attr Data
    LOOP AT LT_KEY_ATTR INTO LS_KEY_ATTR.
     CLEAR LS_DATA.
     LS_DATA-TAB1 = LV_TAB1.
     LS_DATA-TAB2 = LV_TAB2.
     LS_DATA-COMPARE_STATE  = LS_KEY_ATTR-COMPARE_STATE.
     LS_DATA-ATTR_COMPARE = LS_KEY_ATTR-ATTR_COMPARE.
     POPULATE_ATTR_KEY_SET 1.
     POPULATE_ATTR_KEY_SET 2.
     POPULATE_ATTR_SET 1.
     POPULATE_ATTR_SET 2.
     POPULATE_FLD_SET 1.
     POPULATE_FLD_SET 2.
     POPULATE_DATE_TIME_REF_FIELD 1.
     POPULATE_DATE_TIME_REF_FIELD 2.
     APPEND LS_DATA TO T_DATA.
    ENDLOOP.
    "--- Filter on States
    LOOP AT T_DATA INTO LS_DATA.
      SY_TABIX = SY-TABIX.
      IF LS_DATA-COMPARE_STATE IN R_COMPARE_STATE.
        IF LS_DATA-ATTR_COMPARE IN R_ATTR_COMPARE.
        ELSE.
          DELETE T_DATA INDEX SY_TABIX.
        ENDIF.
      ELSE.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
 DELETE T_DATA WHERE DURATION1 NOT IN R_DURATION1.
 DELETE T_DATA WHERE DURATION2 NOT IN R_DURATION2.
 DESCRIBE TABLE T_DATA LINES SY-TFILL .
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
