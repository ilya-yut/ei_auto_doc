# Exception Indicator: Vendor change ( SW_10_06_OBJ_VEN_CHG)

## General Overview

This Exception Indicator monitors vendor master change documents and returns vendor-related changes that match your selection, enriched with vendor master attributes and user details.

This EI serves as an essential control for procurement and vendor master governance by:
- Surfacing change-document activity on vendor objects within configurable time and scope filters
- Supporting segregation-of-duties and audit reviews of who changed which vendor fields and when
- Enabling detection of changes that exceed configured elapsed-time thresholds after the reference date
- Restricting results to vendors that still exist in general vendor master with matching account attributes
- Complementing generic change-log monitoring with vendor-specific context and descriptions

Typical use includes reviews after vendor onboarding, mass updates, or suspected unauthorized master data changes. Results are intended for exception workflows rather than full change-document extracts.

The routine calls the shared master-data change-log function, filters change lines by date and change criteria, aligns vendors with current general vendor master data, and calculates duration from the configured reference date field before alerting.


## Problem Description

Failure to monitor vendor master change documents creates multiple risks across accounts payable, compliance, and master data stewardship.

**Procurement and Payment Risks**
- Unauthorized or erroneous vendor field changes may affect payment terms, bank data, or blocking indicators before the next payment run
- High volumes of undocumented changes can hide fraud or errors in vendor master updates
- Changes outside the intended monitoring window may be missed when reviews rely on ad hoc change-document display

**Master Data and Control Risks**
- Vendor changes without periodic exception reporting weaken evidence for internal control over master data maintenance
- Inability to filter by object class, table, field, user, or change type limits focused investigation of vendor-specific activity
- Vendor changes on objects that no longer match current vendor master criteria can clutter reviews unless reconciled with live master data

**Audit and Compliance Risks**
- Auditors expect traceability of vendor master changes by user, transaction, and time; unmonitored gaps undermine that evidence
- Lack of duration-based filtering can leave stale or low-priority changes in the same queue as recent high-risk items

## Suggested Resolution

**Immediate Response**
- Review flagged change lines for vendor object, field, user, change type, and dates shown in the exception
- Confirm with master data owners whether each change was authorized and documented per policy
- Prioritize changes to payment-relevant fields, blocking indicators, and bank-related tables

**System Assessment**
- Compare current exception volume to prior runs using the same date window and object-class filters
- Look for concentrations by user, transaction code, or field name that may indicate a project or interface issue
- Validate that the reference date field and duration settings match the business definition of “how old” a change must be to alert

**Corrective Actions**
- Correct erroneous vendor master data through standard maintenance with required approvals
- Tighten monitoring scope after root cause so the queue stays actionable
- Document review outcomes for audit trail and schedule recurring runs for vendor object classes in scope
- Escalate repeat unauthorized changes to security and procurement management for access or process review


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 3 | CHANGE_IND | Appl. object change | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 4 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 5 | CHANGENR | Document number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 6 | CHNGIND | Change Indicator | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 7 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 8 | CONVERT_KEY | Convert Key | CHAR | 1 | 0 | CONVERT_KEY | XFELD |
| 9 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 10 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 11 | DATE_REF_FLD | Date Reference Field | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |
| 12 | DATUM | Reference Date | DATS | 8 | 0 | DATUM | DATUM |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 14 | DURATION_D | Duration (days) | INT4 | 10 | 0 | /SKN/E_SW_DURATION_D | /SKN/E_SW_DURATION_D |
| 15 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 16 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 17 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 18 | HEADER_ONLY | Header Only | CHAR | 1 | 0 | HEADER_ONLY | XFELD |
| 19 | KEY1 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 20 | KEY10 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 21 | KEY10_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 22 | KEY10_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 23 | KEY1_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 24 | KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 25 | KEY2 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 26 | KEY2_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 27 | KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 28 | KEY3 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 29 | KEY3_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 30 | KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 31 | KEY4 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 32 | KEY4_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 33 | KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 34 | KEY5 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 35 | KEY5_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 36 | KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 37 | KEY6 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 38 | KEY6_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 39 | KEY6_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 40 | KEY7 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 41 | KEY7_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 42 | KEY7_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 43 | KEY8 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 44 | KEY8_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 45 | KEY8_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 46 | KEY9 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 47 | KEY9_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 48 | KEY9_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 49 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 50 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 51 | LANGU | Language | CHAR | 1 | 0 | LANGU | SPRAS |
| 52 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 53 | MANAGE_IN_UTC | Manage in UTC | CHAR | 1 | 0 | MANAGE_IN_UTC | XFELD |
| 54 | NAME_FIRST | First name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 55 | NAME_LAST | Last name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 56 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 57 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 58 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 59 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 60 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 61 | RECORDS | Count (Int 4) | INT4 | 10 | 0 | /SKN/E_SW_COUNT |  |
| 62 | STKZN | Natural Person | CHAR | 1 | 0 | STKZN | STKZN |
| 63 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 64 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 65 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 66 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 67 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 68 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 69 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 70 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 71 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 72 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 73 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 74 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 75 | VBUND | Company ID | CHAR | 6 | 0 | VBUND | RCOMP |
| 76 | WAS_PLANND | gen from plan. changes | CHAR | 1 | 0 | CD_PLANNED | XFLAG |
| 77 | XCPDK | One-time account | CHAR | 1 | 0 | XCPDK | XFELD |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 77 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ACT_CHNGNO** (Document number)

Active change-document number on the business object while change recording is processed-ties rows to the current change document header key.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**CHANGE_IND** (Appl. object change)

Header-level change indicator (insert/update/delete semantics) for the changed application object in change-document processing.

**CHANGE_IND_DESC** (Domain name)

Text for the header change-indicator domain-human-readable meaning of CHANGE_IND codes in change analytics.

**CHANGENR** (Document number)

Change-document number that uniquely identifies one posted change document for an application object.

**CHNGIND** (Change Indicator)

Item-level change indicator on change-document item lines marking insert, update, delete, or key changes per field group.

**CHNGIND_DESC** (Domain name)

Text for the item change-indicator domain-readable expansion of CHNGIND values on change item rows.

**CONVERT_KEY** (Convert Key)

<mark>Technical conversion key used for formatting/normalization during output transformation.</mark>

**CUKY_NEW** (CUKY)

New currency key in change-log comparisons to detect currency master changes.

**CUKY_OLD** (CUKY)

Previous currency key in change-log comparisons for before/after analysis.

**DATE_REF_FLD** (Date Reference Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**DATUM** (Reference Date)

Pairs with duration logic: once DATUM passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_D** (Duration (days))

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in Days

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**FIELD_DESC** (Short Description)

Connects to alert semantics: rows removed for failing short description on FIELD_DESC never reach downstream filtering.

**FNAME** (Field Name)

Field name key in change documents used to filter by changed attribute.

**HEADER_ONLY** (Header Only)

Treats header only as a discriminator between similar rows that would otherwise look identical in a raw extract.

**KEY1** (Field Name)

Interprets field name as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on KEY1.

**KEY10** (Field Name)

Aligns exception volume with the chosen scope by testing field name via KEY10 before alert evaluation.

**KEY10_DS** (Short Description)

Documents expected operator behavior—short description on KEY10_DS should be set when that dimension is part of the control objective.

**KEY10_V** (Short Description)

Mirrors how administrators slice operational lists: short description (KEY10_V) is one lever that shapes which rows are comparable run over run.

**KEY1_DS** (Short Description)

Helps monitoring stay readable by requiring short description (KEY1_DS) to match organizational or technical selectors when set.

**KEY1_V** (Short Description)

Supports escalation where short description on KEY1_V signals ownership for follow-up between Basis and functional teams.

**KEY2** (Field Name)

Explains why two monitoring passes differ: only the pass with stricter field name on KEY2 surfaces the disputed rows.

**KEY2_DS** (Short Description)

Helps distinguish technical versus business attributes when short description on KEY2_DS correlates with counters or status fields.

**KEY2_V** (Short Description)

Guards against oversized extracts when short description on KEY2_V is narrowed together with client, user, or session filters.

**KEY3** (Field Name)

For distributed landscapes, field name on KEY3 often anchors which application server or destination appears in results.

**KEY3_DS** (Short Description)

For distributed landscapes, short description on KEY3_DS often anchors which application server or destination appears in results.

**KEY3_V** (Short Description)

When populated, keeps the extract focused so short description (KEY3_V) aligns with the intended triage slice.

**KEY4** (Field Name)

Reduces false positives during peak windows by tightening field name through KEY4 alongside state filters.

**KEY4_DS** (Short Description)

Combines with related filters so short description on KEY4_DS refines which records remain for duration or state checks.

**KEY4_V** (Short Description)

When left open per framework rules, KEY4_V does not restrict short description; when set, only matching rows remain.

**KEY5** (Field Name)

When populated, keeps the extract focused so field name (KEY5) aligns with the intended triage slice.

**KEY5_DS** (Short Description)

When left open per framework rules, KEY5_DS does not restrict short description; when set, only matching rows remain.

**KEY5_V** (Short Description)

Ensures reporting respects short description constraints carried by KEY5_V.

**KEY6** (Field Name)

Narrows retrieved rows where field name (KEY6) must match the configured selection for this monitor.

**KEY6_DS** (Short Description)

Treats short description as a discriminator between similar rows that would otherwise look identical in a raw extract.

**KEY6_V** (Short Description)

Combines with related filters so short description on KEY6_V refines which records remain for duration or state checks.

**KEY7** (Field Name)

Reduces false positives during peak windows by tightening field name through KEY7 alongside state filters.

**KEY7_DS** (Short Description)

Explains why two monitoring passes differ: only the pass with stricter short description on KEY7_DS surfaces the disputed rows.

**KEY7_V** (Short Description)

Treats short description as a discriminator between similar rows that would otherwise look identical in a raw extract.

**KEY8** (Field Name)

Captures edge cases where field name (KEY8) must be non-default to reproduce a customer-specific monitoring scenario.

**KEY8_DS** (Short Description)

Stabilizes week-over-week metrics by fixing short description (KEY8_DS) while allowing duration thresholds to move.

**KEY8_V** (Short Description)

Captures edge cases where short description (KEY8_V) must be non-default to reproduce a customer-specific monitoring scenario.

**KEY9** (Field Name)

Gives auditors traceable criteria because field name on KEY9 is applied consistently before any alert flag is raised.

**KEY9_DS** (Short Description)

Documents expected operator behavior—short description on KEY9_DS should be set when that dimension is part of the control objective.

**KEY9_V** (Short Description)

Captures edge cases where short description (KEY9_V) must be non-default to reproduce a customer-specific monitoring scenario.

**KTOKK** (Account group)

Account group (customer/vendor) used to segment master data governance rules.

**LAND1** (Country Key)

Country key used for legal/geographic segmentation of business partners or plants.

**LANGU** (Language)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**MANAGE_IN_UTC** (Manage in UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**NAME_FIRST** (First name)

First name.

**NAME_LAST** (Last name)

Last name.

**NAME_TEXT** (Full Name)

Full name.

**OBJECT_DESC** (Name)

Description of the referenced business/change object-readable label beside OBJECTCLAS or generic OBJECT keys.

**OBJECTCLAS** (Change doc. object)

Change-document object class naming which SAP business object type the change log belongs to.

**OBJECTID** (Object value)

Prevents accidental global scans when object value (OBJECTID) is meant to stay within a controlled application slice.

**PLANCHNGNR** (Change number)

Formal engineering/planning change number referencing a released engineering-change record tied to master updates.

**RECORDS** (Count (Int 4))

For operations, count (int 4) on RECORDS indicates whether a row belongs in the current monitoring pass versus historical noise.

**STKZN** (Natural Person)

Natural-person indicator on customer or vendor master distinguishing natural persons from legal entities.

**TAB_DESC** (Short Description)

Short description of a DDIC table so technical table-name keys are readable in output.

**TABKEY** (Table Key)

Composite table key value used in change-document record identification.

**TABNAME** (Table Name)

Database table name used to scope change/object monitoring to specific tables.

**TCODE** (Transaction Code)

SAP Transaction code

**TEXT_CASE** (Text flag)

Text case/normalization selector used for case-sensitive text filtering behavior.

**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**UNIT_NEW** (Unit)

Unit of measure after change on quantity fields-pairs with NEW_VAL in old/new quantity comparisons on change items.

**UNIT_OLD** (Unit)

Unit of measure before change on quantity fields-pairs with OLD_VAL for before/after quantity analysis.

**USERNAME** (User)

User name display field used for readable identity reporting.

**UTIME** (Time)

Update/change time used with UDATE for precise event windows.

**VALUE_NEW** (New value)

New value in change documents used for before/after comparison.

**VALUE_OLD** (Old value)

Old value in change documents used for before/after comparison.

**VBUND** (Company ID)

Trading partner/company field used for intercompany transaction analysis.

**WAS_PLANND** (gen from plan. changes)

Planned-state indicator used to distinguish planned versus actual execution records.

**XCPDK** (One-time account)

One-time account indicator used to identify one-time customer/vendor postings.


### Parameter Relationships

How parameter combinations work together

**Change-log scope:** The shared change-log parameters (**OBJECTCLAS**, **OBJECTID**, **USERNAME**, **TCODE**, **CHANGE_IND**, **TABNAME**, **FNAME**, **CHNGIND**, **UDATE**, and related filters) define which change-document lines the underlying log function returns before vendor processing begins.

**Date window:** **DATUM** and **UDATE** supply explicit calendar bounds when you populate them. When the monitor date range is empty, **BACKDAYS** builds a lower bound on the change-document date range used to filter lines by **UDATE**. **DATE_REF_FLD** names which date on each output row is used to compute **DURATION** relative to the evaluation time.

**Vendor master reconciliation:** **LIFNR**, **LAND1**, **KTOKK**, **XCPDK**, **VBUND**, and **STKZN** limit which vendors remain after change lines are matched to current general vendor master records.

**Age filter:** **DURATION** with **DURATION_UNIT** (and **DURATION_D** when used) removes rows whose elapsed time from the reference date field does not fall in the selected duration band.

**Combined effect:** Change-log selection, explicit dates (**DATUM**, **UDATE**) or the **BACKDAYS** window on **UDATE**, vendor master attributes, and the **DURATION** / **DURATION_UNIT** age test all apply together—rows must satisfy both the active date criteria and the duration band before a vendor change line appears in the final alert population.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DATE_REF_FLD** - initial - treated as ERDAT by code
- **DURATION_UNIT** - initial - treated as D by code
- **LANGU** - initial - treated as E by code
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Vendor changes in the last ten days**

**Purpose:** List vendor master change lines from the last ten days on table LFA1 with duration in days.
```
OBJECTCLAS = KRED
TABNAME = LFA1
BACKDAYS = 10
DURATION_UNIT = D
DATE_REF_FLD = ERDAT
```

**Use Case 2: Specific vendor and user**

**Purpose:** Review changes by one user on a single vendor in a fixed calendar week.
```
LIFNR = 0000100001
USERNAME = JSMITH
UDATE = 20250301
UDATE = 20250307
DURATION = 0
DURATION_UNIT = D
```

**Use Case 3: Field-level watch on account group**

**Purpose:** Monitor updates to vendor account group field for domestic vendors.
```
TABNAME = LFA1
FNAME = KTOKK
CHNGIND = U
LAND1 = US
BACKDAYS = 30
```

**Use Case 4: Changes exactly seven full days ago**

**Purpose:** Flag vendor changes whose reference date falls in the scope of exactly 7 full days ago when using full-day duration counting.
```
OBJECTCLAS = KRED
TABNAME = LFA1
DURATION = 7
DURATION_UNIT = F
DATE_REF_FLD = ERDAT
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_MD_CHNG_COUNT | ACT_CHNGNO | Document number | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGENR | Document number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGE_IND | Appl. object change | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHNGIND | Change Indicator | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CUKY_NEW | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CUKY_OLD | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | FIELD_DESC | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KTOKK | Account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_TEXT | Full Name | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECTCLAS | Change doc. object | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECT_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | PLANCHNGNR | Change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | RECORDS | Count (Int 4) | INT4(10) | /SKN/E_SW_COUNT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | STKZN | Natural Person | CHAR(1) | STKZN |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TABKEY | Table Key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TAB_DESC | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TCODE | Transaction Code | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TEXT_CASE | Text flag | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UDATE | Date | DATS(8) | CDDATUM |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UNIT_NEW | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UNIT_OLD | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | USERNAME | User | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UTIME | Time | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VALUE_NEW | New value | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VALUE_OLD | Old value | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VBUND | Company ID | CHAR(6) | VBUND |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | WAS_PLANND | gen from plan. changes | CHAR(1) | CD_PLANNED |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | XCPDK | One-time account | CHAR(1) | XCPDK |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_MD_CHNG_VENDOR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_MD_CHNG_COUNT
*"----------------------------------------------------------------------
  INCLUDE /SKN/PC_SQL_DATA.
  TYPES: BEGIN OF TY_LFA1,
           LIFNR TYPE LFA1-LIFNR,
           LAND1 TYPE LFA1-LAND1,
           KTOKK TYPE LFA1-KTOKK,
           XCPDK TYPE LFA1-XCPDK,
           VBUND TYPE LFA1-VBUND,
           STKZN TYPE LFA1-STKZN,
         END OF TY_LFA1,
         TT_LFA1 TYPE TABLE OF TY_LFA1.
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: SW_DEST             RFCDEST,
               MANAGE_IN_UTC       CHAR1 ,
               LANGU               LANGU,
               BACKDAYS            INT4,
               DURATION_D          /SKN/E_SW_DURATION_D,
               DURATION_UNIT       /SKN/E_SW_DURATION_UNIT,
               DATE_REF_FLD        NAME_FELD,
               CONVERT_KEY         CHAR1,
               HEADER_ONLY         CHAR1.
  DATA_MULTY:   LIFNR             LIFNR,
                LAND1             LAND1_GP,
                XCPDK             XCPDK,
                VBUND             RASSC,
                KTOKK             KTOKK,
                STKZN             STKZN,
                OBJECTCLAS        CDOBJECTCL,
                OBJECTID          CDOBJECTV,
                USERNAME          CDUSERNAME,
                TCODE             CDTCODE,
                CHANGE_IND        CDCHNGINDH,
                TABNAME           TABNAME,
                DURATION          /SKN/E_SW_DURATION,
                FNAME             FIELDNAME,
                CHNGIND           CDCHNGIND,
                UDATE             CDDATUM,
                DATUM             SYDATUM,
                COUNTER           I.
  CONSTANTS: C_LFA1 TYPE TABNAME VALUE 'LFA1'.
  DATA: SY_DATLO LIKE SY-DATLO ,
        SY_TIMLO LIKE SY-TIMLO .
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: SY_TABIX  LIKE SY-TABIX,
        DATE_FROM LIKE SY-DATUM .
  DATA: LV_SHIFT      TYPE DDLENG,
        LV_LENG       TYPE DDLENG,
        LV_DOMNAME    TYPE DD07V-DOMNAME,
        LV_DOMVALUE   TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT     TYPE DD07V-DDTEXT,
        LV_OBJECT     TYPE CDOBJECTV,
        LV_LIFNR      TYPE LIFNR,
        LV_STRUCTURE  TYPE DDOBJNAME,
        LV_INDEX      TYPE I,
        LV_OBJECTCLAS TYPE CDOBJECTCL,
        LV_DOC        TYPE CDCHANGENR,
        LV_COUNT_TMP  TYPE I,
        LV_LINES      TYPE I.
  DATA: LS_DATA    LIKE LINE OF T_DATA[],
        LS_ADDR    TYPE BAPIADDR3,
        LS_DATA_MD TYPE /SKN/S_SW_10_06_MD_CHNG_LOG,
        LS_LFA1    TYPE TY_LFA1.
  DATA: LT_DATA_MD  TYPE TABLE OF /SKN/S_SW_10_06_MD_CHNG_LOG,
        LT_DATA_TMP LIKE TABLE OF T_DATA,
        LT_RET     TYPE TABLE OF BAPIRET2,
        LT_LFA1    TYPE TT_LFA1.
  FIELD-SYMBOLS: <FS_DATA>    LIKE LINE OF T_DATA[],
                 <FS_DATA_MD> LIKE LINE OF LT_DATA_MD,
                          TYPE ANY.
* Set default parameter
  LV_BACKDAYS       = 10.
  LV_DURATION_UNIT  = 'D'.
  LV_DATE_REF_FLD   = 'ERDAT'.
  LV_LANGU          = 'E'.
  SELECT_MULTY:  LIFNR,
                 LAND1,
                 XCPDK,
                 VBUND,
                 KTOKK,
                 STKZN,
                 OBJECTCLAS,
                 OBJECTID,
                 USERNAME,
                 TCODE,
                 CHANGE_IND,
                 TABNAME,
                 DURATION,
                 FNAME,
                 CHNGIND,
                 UDATE,
                 COUNTER,
                 DATUM .
  SELECT_SINGLE: SW_DEST,
                 LANGU,
                 MANAGE_IN_UTC,
                 BACKDAYS,
                 DATE_REF_FLD,
                 CONVERT_KEY,
                 DATE_REF_FLD,
                 DURATION_D,
                 DURATION_UNIT,
                 HEADER_ONLY.
* Configuration Alert
  CALL FUNCTION '/SKN/F_SW_10_06_MD_CHNG_LOG'
    IMPORTING
      IS_ALERT = IS_ALERT
    TABLES
      T_SELECT = T_SELECT
      T_DATA   = LT_DATA_MD.
* Check if found some change in configuration log
  CHECK IS_ALERT EQ 'X'.
  CLEAR: IS_ALERT.
  REFRESH: T_DATA.
* Select parameters for Select from LFA1
  SELECT_MULTY:  LIFNR,
                 LAND1,
                 XCPDK,
                 VBUND,
                 KTOKK,
                 STKZN.
  CONVERT_MULTY: LIFNR ALPHA.
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  IF R_DATUM[] IS INITIAL .  " Set default value
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF .
  DELETE LT_DATA_MD WHERE UDATE NOT IN R_DATUM[].
  CHECK LT_DATA_MD IS NOT INITIAL.
  SORT LT_DATA_MD BY OBJECTCLAS OBJECTID TABNAME FNAME.
  IF LV_SW_DEST IS INITIAL.
    SELECT LIFNR LAND1 KTOKK XCPDK VBUND STKZN
      FROM LFA1
      INTO TABLE LT_LFA1
      WHERE LIFNR IN R_LIFNR
      AND   LAND1 IN R_LAND1
      AND   KTOKK IN R_KTOKK
      AND   XCPDK IN R_XCPDK
      AND   VBUND IN R_VBUND
      AND   STKZN IN R_STKZN.
  ENDIF.
  LOOP AT LT_DATA_MD INTO LS_DATA WHERE TABNAME EQ C_LFA1 .
    CLEAR: LV_LIFNR.
    LV_LIFNR = LS_DATA-OBJECTID.
    READ TABLE LT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = LV_LIFNR
                                    BINARY SEARCH.
    IF SY-SUBRC = 0.
      CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES'
        EXPORTING
          BNAME      = LS_DATA-USERNAME
        IMPORTING
          NAME_FIRST = LS_DATA-NAME_FIRST
          NAME_LAST  = LS_DATA-NAME_LAST
          NAME_TEXT  = LS_DATA-NAME_TEXT
        EXCEPTIONS
          NO_DATA    = 1
          OTHERS     = 2.
      IF LS_LFA1-LIFNR IS NOT INITIAL.
**    "--- Get  Vendor Decriptions
        CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
          EXPORTING
            LIFNR        = LS_LFA1-LIFNR
          IMPORTING
            VENDOR_DESC  = LS_DATA-OBJECT_DESC
          EXCEPTIONS
            WRONG_VENDOR = 1
            OTHERS       = 2.
*
      ENDIF.
      APPEND LS_DATA TO T_DATA.
    ENDIF.
  ENDLOOP.
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY_TIMLO
          D_TO        = SY_DATLO
          T_TO        = SY_TIMLO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          T_DATA-DURATION  = TIME_DIFF .
        ELSE.
          T_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
******************************************************************************
*    IF ls_data-matkl IS NOT INITIAL.
** Material group desc.
*      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
*      EXPORTING
*        matkl              = ls_data-matkl
*      IMPORTING
*        matkl_desc         = ls_data-wgbez
**       MATKL_DESC_L       =
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2
*        .
*    ENDIF.
**
*    IF ls_data-bsart IS NOT INITIAL AND ls_data-bstyp IS NOT INITIAL.
**    "-- BSART_DESC
*      CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
*      EXPORTING
*        bsart            = ls_data-bsart
*        langu            = lv_langu
*        bstyp            = ls_data-bstyp
*      IMPORTING
*        type_desc        = ls_data-batxt
*      EXCEPTIONS
*        wrong_code       = 1
*        OTHERS           = 2.
*    ENDIF.
*
*    IF ls_data-statu IS NOT INITIAL.
*      "-- STATU_DESC
*      lv_domname = 'ESTAK'.
*      lv_domvalue = ls_data-statu.
*
*      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
*      EXPORTING
*        i_domname        = lv_domname
*        i_domvalue       = lv_domvalue
*        langu            = lv_langu
**       SW_DEST          =
*      IMPORTING
*        e_ddtext         = lv_ddtext
*      EXCEPTIONS
*        not_exist        = 1
*        OTHERS           = 2.
*      IF sy-subrc = 0.
*        ls_data-statu_desc = lv_ddtext.
*      ENDIF.
*    ENDIF.
**
*    IF ls_data-bstyp IS NOT INITIAL.
**    "-- BSTYP_DESC
*      lv_domname = 'EBSTYP'.
*      lv_domvalue = <fs_data>-bstyp.
*
*      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
*      EXPORTING
*        i_domname        = lv_domname
*        i_domvalue       = lv_domvalue
*        langu            = lv_langu
**       SW_DEST          =
*      IMPORTING
*        e_ddtext         = lv_ddtext
*      EXCEPTIONS
*        not_exist        = 1
*        OTHERS           = 2.
*      IF sy-subrc = 0.
*        ls_data-bstyp_desc = lv_ddtext.
*      ENDIF.
*    ENDIF.
**
*    IF ls_data-lifnr IS NOT INITIAL.
**    "--- Get  Vendor Decriptions
*      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
*      EXPORTING
*        lifnr              = ls_data-lifnr
*      IMPORTING
*        vendor_desc        = ls_data-name1
*      EXCEPTIONS
*        wrong_vendor       = 1
*        OTHERS             = 2.
*
*    ENDIF.
**
*    IF ls_data-ekorg IS NOT INITIAL.
**   "-- EKORG_DESC
*      CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
*      EXPORTING
*        ekorg              = ls_data-ekorg
*      IMPORTING
*        pur_org_desc       = ls_data-ekotx
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2.
*
*    ENDIF.
**
**
*    IF ls_data-ekgrp IS NOT INITIAL.
**   "-- EKGRP_DESC
*      CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
*      EXPORTING
*        ekgrp              = ls_data-ekgrp
*      IMPORTING
*        pur_grp_desc       = ls_data-eknam
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2.
*    ENDIF.
*    APPEND ls_data TO t_data[].
*  LOOP AT t_data ASSIGNING <fs_data>.
*
*    CLEAR: lt_ret, ls_addr.
*    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*      EXPORTING
*        username = <fs_data>-username
*      IMPORTING
*        address  = ls_addr
*      TABLES
*        return   = lt_ret.
*
*    IF sy-subrc = 0.
*      <fs_data>-name_first = ls_addr-firstname.
*      <fs_data>-name_last  = ls_addr-lastname.
*    ENDIF.
*
*  ENDLOOP.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
