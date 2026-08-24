# Exception Indicator: Credit Management Approvals ( SW_10_01_CREDIT_APP)

## General Overview

This Exception Indicator monitors change-document activity related to Credit Management Approvals by showing who changed credit-relevant master data or approval-related records, when the change occurred, which transaction was used, and which fields moved from old to new values. It gives credit, sales, and compliance teams a focused review list for approval and credit-control follow-up.

This EI serves as an essential control for credit management governance by:
- Enabling detection of unauthorized or unexpected changes that affect credit approval authority or credit-control data
- Supporting accountability by identifying the responsible user, change timing, and before/after field values for credit-related updates
- Helping credit teams prioritize review of sensitive attribute changes that can alter customer exposure or release decisions
- Providing readable object and key context so reviewers can interpret which credit-related record was changed
- Supporting recurring surveillance of credit-management change activity before order release, billing, or period-close reviews

This monitoring is useful for credit limit and credit master reviews, approval-authority change surveillance, and audit sampling of high-risk credit-control updates. It is especially relevant where credit operations need evidence that approval-related changes were reviewed before downstream sales and fulfillment processes rely on them.

The EI uses SAP change-document header and item data configured for Credit Management Approvals review.


## Problem Description

Failure to monitor Credit Management Approvals–related change activity creates risks across credit control, revenue protection, and audit compliance:

**Credit and Sales Risks**

- Unauthorized or undocumented changes to credit-relevant data can alter approval authority or exposure without timely review
- Sensitive credit attributes can change without clear visibility of who made the update and what values moved
- Undetected change activity on credit-controlled customers can leave commercial exposure unmanaged before delivery or billing

**Operational Risks**

- Review windows that do not match credit approval cadence can miss recent changes or retain stale history
- Scope that is too broad floods credit reviewers with unrelated change lines; scope that is too narrow can hide critical updates
- Header-only checks speed triage but hide field-level old and new values needed for credit decision follow-up

**Control and Audit Risks**

- Weak monitoring reduces evidence that Credit Management Approvals–related changes were reviewed on a timely basis
- Missing user and object context delays escalation of high-risk or unauthorized credit-control updates
- Lack of recurring exception review limits accountability between credit operations, sales, and master-data owners

## Suggested Resolution

**Immediate Response**

- Review flagged changes for the affected credit-related object, changed fields, old and new values, user, transaction, and change date
- Confirm with credit management whether each change was authorized, documented, and aligned with approval policy
- Prioritize changes that affect credit limits, blocking, risk category, payment behavior, or approval-related attributes

**System Assessment**

- Validate object-class and table scope against the credit-management domains that must be monitored for approvals
- Tune lookback and multi-date review windows to match how quickly credit teams must detect new changes
- Decide whether header-only checks or full item-level detail, including readable key context, is required for actionable credit review

**Corrective Actions**

- Correct unauthorized or erroneous credit-related values through standard credit and master-data processes where review confirms action is required
- Adjust monitoring scope and review windows after cleanup so results stay focused on true Credit Management Approvals exceptions
- Document review outcomes and schedule recurring runs before credit review meetings, order-release peaks, or close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters for the Credit Management Approvals monitoring configuration. Users set values for these parameters to filter and control which credit-related change-document activity is selected and returned for review.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | CHANGE_IND | Appl. object change-Header Lvl | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 4 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 5 | CHANGENR | Document number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 6 | CHNGIND | Change Indicator-Row lvl | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 7 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 8 | CONVERT_KEY | 'X' - Decompose Key Field |  | 0 | 0 |  |  |
| 9 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 10 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 11 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 12 | DURATION_D | INT4 | 10 | 0 | /SKN/E_SW_DURATION_D |  |  |
| 13 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 14 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 15 | FNAME_REP | CHAR | 30 | 0 | FIELDNAME | FDNAME |  |
| 16 | HEADER_ONLY | CHAR | 1 | 0 |  | XFELD |  |
| 17 | KEY1 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 18 | KEY10 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 19 | KEY10_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 20 | KEY10_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 21 | KEY1_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 22 | KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 23 | KEY2 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 24 | KEY2_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 25 | KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 26 | KEY3 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 27 | KEY3_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 28 | KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 29 | KEY4 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 30 | KEY4_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 31 | KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 32 | KEY5 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 33 | KEY5_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 34 | KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 35 | KEY6 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 36 | KEY6_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 37 | KEY6_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 38 | KEY7 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 39 | KEY7_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 40 | KEY7_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 41 | KEY8 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 42 | KEY8_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 43 | KEY8_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 44 | KEY9 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 45 | KEY9_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 46 | KEY9_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 47 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 48 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 49 | NAME_FIRST | First name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 50 | NAME_LAST | Last name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 51 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 52 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 53 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 54 | OBJECTID | Customer / credit object | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 55 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 56 | REPET_BACKDAYS | INT4 | 10 | 0 |  |  |  |
| 57 | REPETITIVE | Repetitive Change | CHAR | 1 | 0 | /SKN/E_REPEAT | XFLAG |
| 58 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 59 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 60 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 61 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 62 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 63 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 64 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 65 | UDATE_REPET | DATS | 8 | 0 | CDDATUM | DATUM |  |
| 66 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 67 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 68 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 69 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 70 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 71 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 72 | WAS_PLANND | gen from plan. changes | CHAR | 1 | 0 | CD_PLANNED | XFLAG |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 72 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ACT_CHNGNO** (Document number)

Active change-document number on the business object while change recording is processed-ties rows to the current change document header key.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on UDATE

**CHANGE_IND** (Appl. object change-Header Lvl)

Header-level change indicator (insert/update/delete semantics) for the changed application object in change-document processing.

**CHANGE_IND_DESC** (Domain name)

Text for the header change-indicator domain-human-readable meaning of CHANGE_IND codes in change analytics.

**CHANGENR** (Document number)

Change-document number that uniquely identifies one posted change document for an application object.

**CHNGIND** (Change Indicator-Row lvl)

Item-level change indicator on change-document item lines marking insert, update, delete, or key changes per field group.

**CHNGIND_DESC** (Domain name)

Text for the item change-indicator domain-readable expansion of CHNGIND values on change item rows.

**CONVERT_KEY** ('X' - Decompose Key Field)

<mark>Flag that determines whether the change log decomposes the compressed table key (TABKEY) into readable key components and converts technical KEY change lines into field-level key updates.
CONVERT_KEY Options:
X - decompose TABKEY into KEY1-KEY10, KEY1_V - KEY10_V, and KEY1_DS - KEY10_DS; convert KEY insert/delete lines into key-field change rows and remove raw FNAME = KEY lines where the key-change case applies.
Empty or blank - do not run key conversion; keep standard change-document lines and identify the changed object primarily via TABKEY (and OBJECTID where applicable).</mark>

**CUKY_NEW** (CUKY)

New currency key in change-log comparisons to detect currency master changes.

**CUKY_OLD** (CUKY)

Previous currency key in change-log comparisons for before/after analysis.

**DATUM** (DATS)

Optional monitor date passed by the selection framework for lookback construction; in this EI the active change-date window is driven through UDATE and, in repetitive mode, UDATE_REPET.

**Not in use**
**DURATION_D** (INT4)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in Days

**FIELD_DESC** (Short Description)

<mark>Description that defines the purpose of a repository object or data element.</mark>

**FNAME** (Field Name)

Field name key in change documents used to filter by changed attribute.

**FNAME_REP** (CHAR)

Reserved field-name parameter for repetitive-change field comparisons in related master-data monitors.

**Not in use**
**HEADER_ONLY** (CHAR)

<mark>Flag that determines whether a result will display only header data or item details.
HEADER_ONLY Options:
X - return change-document headers only.
Empty or blank -include item-level changes.</mark>

**KEY1** (Field Name)

<mark>Positions 1-10 of the changed table's key: each KEYn is the technical name of the nth key field. Populated when CONVERT_KEY = X.</mark>

**KEY10** (Field Name)

When left open per framework rules, KEY10 does not restrict field name; when set, only matching rows remain.

**KEY10_DS** (Short Description)

After data is read, lines are removed unless short description on KEY10_DS still satisfies the active multivalued selection.

**KEY10_V** (Short Description)

Valuable when comparing health before and after a release—hold short description on KEY10_V constant while varying other filters.

**KEY1_DS** (Short Description)

<mark>Short text for the field named in the matching KEYn (same index n). Populated when CONVERT_KEY = X.</mark>

**KEY1_V** (Short Description)

<mark>Value of the nth key component extracted from the change line's TABKEY (same index n). Populated when CONVERT_KEY = X.</mark>

**KEY2** (Field Name)

Narrows retrieved rows where field name (KEY2) must match the configured selection for this monitor.

**KEY2_DS** (Short Description)

Guards against oversized extracts when short description on KEY2_DS is narrowed together with client, user, or session filters.

**KEY2_V** (Short Description)

Gives auditors traceable criteria because short description on KEY2_V is applied consistently before any alert flag is raised.

**KEY3** (Field Name)

Improves readability of exported lists because field name (KEY3) columns stay aligned with the configured filter intent.

**KEY3_DS** (Short Description)

Improves readability of exported lists because short description (KEY3_DS) columns stay aligned with the configured filter intent.

**KEY3_V** (Short Description)

Separates cross-client noise from in-scope work when short description on KEY3_V correlates with client or user attributes.

**KEY4** (Field Name)

Prevents accidental global scans when field name (KEY4) is meant to stay within a controlled application slice.

**KEY4_DS** (Short Description)

Ensures reporting respects short description constraints carried by KEY4_DS.

**KEY4_V** (Short Description)

Explains why two monitoring passes differ: only the pass with stricter short description on KEY4_V surfaces the disputed rows.

**KEY5** (Field Name)

Separates cross-client noise from in-scope work when field name on KEY5 correlates with client or user attributes.

**KEY5_DS** (Short Description)

Explains why two monitoring passes differ: only the pass with stricter short description on KEY5_DS surfaces the disputed rows.

**KEY5_V** (Short Description)

Works downstream of the initial read so short description on KEY5_V still participates in row-level deletion rules.

**KEY6** (Field Name)

For operations, field name on KEY6 indicates whether a row belongs in the current monitoring pass versus historical noise.

**KEY6_DS** (Short Description)

Allows phased rollout: first widen KEY6_DS for short description, then tighten thresholds once baseline noise is understood.

**KEY6_V** (Short Description)

Ensures reporting respects short description constraints carried by KEY6_V.

**KEY7** (Field Name)

Prevents accidental global scans when field name (KEY7) is meant to stay within a controlled application slice.

**KEY7_DS** (Short Description)

Narrows retrieved rows where short description (KEY7_DS) must match the configured selection for this monitor.

**KEY7_V** (Short Description)

Allows phased rollout: first widen KEY7_V for short description, then tighten thresholds once baseline noise is understood.

**KEY8** (Field Name)

Reflects real administration where field name on KEY8 is routinely restricted to a single productive client or object family.

**KEY8_DS** (Short Description)

Interprets short description as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on KEY8_DS.

**KEY8_V** (Short Description)

Reflects real administration where short description on KEY8_V is routinely restricted to a single productive client or object family.

**KEY9** (Field Name)

Connects to alert semantics: rows removed for failing field name on KEY9 never reach downstream filtering.

**KEY9_DS** (Short Description)

After data is read, lines are removed unless short description on KEY9_DS still satisfies the active multivalued selection.

**KEY9_V** (Short Description)

Reflects real administration where short description on KEY9_V is routinely restricted to a single productive client or object family.

**LANGU** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** ('X' - Manage in UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**NAME_FIRST** (First name)

<mark>First name of the user who posted the change.</mark>

**NAME_LAST** (Last name)

<mark>Last name of the user who posted the change.</mark>

**NAME_TEXT** (Full Name)

<mark>Full name of the user who posted the change.</mark>

**OBJECT_DESC** (Name)

Description of the referenced business/change object-readable label beside OBJECTCLAS or generic OBJECT keys.

**OBJECTCLAS** (Change doc. object)

Change-document object class naming which SAP business object type the change log belongs to.

**OBJECTID** (Vendor)

Change-document object identifier for the monitored business object, such as a vendor or customer number depending on the selected object class.

**PLANCHNGNR** (Change number)

Formal engineering/planning change number referencing a released engineering-change record tied to master updates.

**REPET_BACKDAYS** (INT4)

When REPETITIVE is set, day count for the wider lookback window used when an explicit repetitive date range is not supplied; pairs with BACKDAYS for the narrower change-date window on UDATE.

**REPETITIVE** (Repetitive Change)

Enables repetitive-change processing that looks for recurring updates to the same object across separate date windows and forces key decomposition for readable key comparisons.

**REPETITIVE Options:**
- **X** — Enable repetitive-change mode with separate BACKDAYS and REPET_BACKDAYS windows and forced key decomposition.
- Empty or blank — Standard change-document monitoring using a single UDATE window.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TAB_DESC** (Short Description)

Short description of a DDIC table so technical table-name keys are readable in output.

**TABKEY** (Table Key)

Composite table key value used in change-document record identification.

**TABNAME** (Table Name)

Database table name used to scope change/object monitoring to specific tables.

**TCODE** (Transaction Code)

SAP Transaction code

**TEXT_CASE** (Text flag)

<mark>Output flag from the master-data change log that marks whether the change line is a language-dependent text change (descriptions/names in text context) rather than a normal non-text field change.
TEXT_CASE Options:
X - text change line (language-dependent / text-table style change).
Empty or blank - not flagged as a text change.</mark>

**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**UDATE_REPET** (DATS)

<mark>Repeated-change date marker on user-security extracts highlighting recurring password or profile churn.</mark>

**UNIT_NEW** (Unit)

Unit of measure after change on quantity fields-pairs with NEW_VAL in old/new quantity comparisons on change items.

**UNIT_OLD** (Unit)

Unit of measure before change on quantity fields-pairs with OLD_VAL for before/after quantity analysis.

**USERNAME** (User)

<mark>User who posted the change.</mark>

**UTIME** (Time)

Update/change time used with UDATE for precise event windows.

**VALUE_NEW** (New value)

New value in change documents used for before/after comparison.

**VALUE_OLD** (Old value)

Old value in change documents used for before/after comparison.

**WAS_PLANND** (gen from plan. changes)

Planned-state indicator used to distinguish planned versus actual execution records.

### Parameter Relationships

**Object-class scope:** At least one change-document object class must be resolved before data is read. **OBJECTCLAS** selections are used directly; when **OBJECTCLAS** is empty but **TABNAME** is filled, object classes are derived from the selected tables. If no object class remains, the routine exits without data.

**Header selection:** Change headers are read with filters on **OBJECTCLAS**, **OBJECTID**, **CHANGENR**, **USERNAME**, **TCODE**, **CHANGE_IND**, and **UDATE**.

**Change-date window (standard mode):** When **REPETITIVE** is blank and no explicit date range is supplied, **BACKDAYS** builds the lookback window used for **UDATE**. Explicit **UDATE** ranges override that fallback.

**Repetitive mode:** When **REPETITIVE** is **X**, **REPET_BACKDAYS** seeds the wider lookback window and **UDATE_REPET** defaults from that window when not supplied. **BACKDAYS** then builds a separate narrower **UDATE** window, defaulting to one day when **BACKDAYS** is initial. **CONVERT_KEY** is forced on in this mode, and additional date checks apply on output rows.

**Header-only short path:** When **HEADER_ONLY** is **X**, matching headers are returned and the routine exits before item-level change lines are retrieved.

**Item detail and field scope:** When header-only mode is off, item lines are retrieved and can be narrowed by **TABNAME**, **FNAME**, and **CHNGIND** during post-processing.

**Key conversion:** When **CONVERT_KEY** is set, including when forced in repetitive mode, compressed table keys are decomposed into **KEY1**–**KEY10** columns with matching values and descriptions, and technical key change lines can be transformed into field-level key updates.

**Language:** **LANGU** defaults from the logon language when not supplied and drives short descriptions for tables, fields, and related texts.

**Duration:** **DURATION_D** is available in the parameter set, but the on-premise duration calculation and filter block is commented out in the supplied code, so age-in-days filtering should not be assumed active unless the deployed version re-enables it.

**Remote execution:** When **SW_DEST** is set, processing delegates to the remote/cloud implementation and the local retrieval path is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 0 by code
- **REPET_BACKDAYS** - initial - treated as 0 by code
- **LANGU** - initial - treated as the logon language by code
- **CONVERT_KEY** - initial - treated as blank by code
- **HEADER_ONLY** - initial - treated as blank by code
- **REPETITIVE** - initial - treated as blank by code

### Practical Example of Parameter Configuration

**Use Case 1: Recent master-data changes**

**Purpose:** Monitor change documents posted in the last thirty days for a selected object class.

```
BACKDAYS = 30
OBJECTCLAS = DEBI
TABNAME = KNA1
```

**Use Case 2: Specific table and field**

**Purpose:** Alert when selected fields on customer general data change.

```
TABNAME = KNA1
FNAME = STCD1
BACKDAYS = 14
OBJECTCLAS = DEBI
```

**Use Case 3: Header existence check**

**Purpose:** Raise an alert when any change header exists in scope without loading item lines.

```
HEADER_ONLY = X
OBJECTCLAS = KRED
BACKDAYS = 7
```

**Use Case 4: Repetitive changes on one vendor**

**Purpose:** Track recurring updates using repetitive mode and key decomposition.

```
REPETITIVE = X
OBJECTID = 0000100001
OBJECTCLAS = KRED
REPET_BACKDAYS = 90
BACKDAYS = 30
```

**Use Case 5: Readable key decomposition**

**Purpose:** Review field-level key updates with compressed table keys decomposed into readable components.

```
CONVERT_KEY = X
OBJECTCLAS = DEBI
BACKDAYS = 14
TABNAME = KNA1
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_MD_CHNG_LOG | ACT_CHNGNO | Document number | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHANGENR | Document number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHANGE_IND | Appl. object change | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHNGIND | Change Indicator | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CUKY_NEW | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CUKY_OLD | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_LOG | FIELD_DESC | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY10_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY10_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY1_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY1_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY2_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY2_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY3_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY3_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY4_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY4_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY5_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY5_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY6_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY6_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY7_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY7_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY8_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY8_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY9_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY9_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_06_MD_CHNG_LOG | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_06_MD_CHNG_LOG | NAME_TEXT | Full Name | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | OBJECTCLAS | Change doc. object | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_06_MD_CHNG_LOG | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_06_MD_CHNG_LOG | OBJECT_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_MD_CHNG_LOG | PLANCHNGNR | Change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_06_MD_CHNG_LOG | REPETITIVE | Repetitive Change | CHAR(1) | /SKN/E_REPEAT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TABKEY | Table Key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TAB_DESC | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TCODE | Transaction Code | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TEXT_CASE | Text flag | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_06_MD_CHNG_LOG | UDATE | Date | DATS(8) | CDDATUM |
| /SKN/S_SW_10_06_MD_CHNG_LOG | UNIT_NEW | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | UNIT_OLD | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | USERNAME | User | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | UTIME | Time | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | VALUE_NEW | New value | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_06_MD_CHNG_LOG | VALUE_OLD | Old value | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_06_MD_CHNG_LOG | WAS_PLANND | gen from plan. changes | CHAR(1) | CD_PLANNED |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_MD_CHNG_LOG.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_MD_CHNG_LOG
*"----------------------------------------------------------------------
*** 15.03.21++
  TYPES: BEGIN OF TY_KEY_FIELD,
           TABNAME   TYPE TABNAME,
           FIELDNAME TYPE FIELDNAME,
         END OF TY_KEY_FIELD,
         TT_KEY_FIELD TYPE STANDARD TABLE OF TY_KEY_FIELD.
*** 15.03.21++
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: MANAGE_IN_UTC       CHAR1 ,
               LANGU               LANGU,
               BACKDAYS            INT4,
               REPET_BACKDAYS      INT4,        " 31.08.21++
               CONVERT_KEY         CHAR1,
               HEADER_ONLY         CHAR1,
               REPETITIVE          CHAR1.       " 15.03.21++
  DATA_MULTY:   OBJECTCLAS        CDOBJECTCL,
                OBJECTID          CDOBJECTV,
                CHANGENR          CDCHANGENR,                  " 15.03.21++
                USERNAME          CDUSERNAME,
                TCODE             CDTCODE,
                CHANGE_IND        CDCHNGINDH,
                TABNAME           TABNAME,
                FNAME             FIELDNAME,
                CHNGIND           CDCHNGIND,
                UDATE             CDDATUM,
                UDATE_REPET       CDDATUM,                     " 31.08.21++
                DURATION_D        /SKN/E_SW_DURATION_D,
                DATUM             SYDATUM,                    " Paased by SW Online Monitor
                FNAME_REP         FIELDNAME.                   " 15.03.21++
*** 15.03.21++
  TYPES: BEGIN OF TY_LFBK_KEY,
           LIFNR TYPE LIFNR,
           BANKS TYPE BANKS,
           BANKL TYPE BANKL,
           BANKN TYPE BANKN,
         END OF TY_LFBK_KEY,
         TT_LFBK_KEY TYPE STANDARD TABLE OF TY_LFBK_KEY.
*** 15.03.21++
  DATA: LV_FIELDNAME TYPE FIELDNAME,
        LV_SHIFT     TYPE DDLENG,
        LV_LENG      TYPE DDLENG.
  DATA: LV_TABKEY_LEN TYPE I VALUE '70',    "!!!
        LV_ILEN TYPE I.
  FIELD-SYMBOLS: <FS_OLD> TYPE ANY,
                 <FS_NEW> TYPE ANY.
  DATA : FLD TYPE FIELDNAME,
         IFLD TYPE I,
         CTMP(2) TYPE C.
  DEFINE POPULATE_KEY_FIELD .
    " &1 - Field Index
    CLEAR LV_FIELDNAME.
    PERFORM GET_KEY_FIELD  USING    LS_DATA-TABNAME
                                    &1
                           CHANGING LV_FIELDNAME
                                    LV_SHIFT
                                    LV_LENG.
    IF LV_FIELDNAME IS NOT INITIAL.
      LS_DATA-KEY&1   =  LV_FIELDNAME.
      LV_ILEN = LV_SHIFT + LV_LENG.
      IF LV_ILEN <= LV_TABKEY_LEN.
        LS_DATA-KEY&1_V =  LS_DATA-TABKEY+LV_SHIFT(LV_LENG).
      ENDIF.
      PERFORM GET_FIELD_DESC USING   LS_DATA-TABNAME
                                     LV_FIELDNAME
                                     LV_LANGU
                            CHANGING LS_DATA-KEY&1_DS.
    ENDIF.
  END-OF-DEFINITION .
  DEFINE POPULATE_KEY_FIELDS .
    POPULATE_KEY_FIELD 1.
    POPULATE_KEY_FIELD 2.
    POPULATE_KEY_FIELD 3.
    POPULATE_KEY_FIELD 4.
    POPULATE_KEY_FIELD 5.
    POPULATE_KEY_FIELD 6.
    POPULATE_KEY_FIELD 7.
    POPULATE_KEY_FIELD 8.
    POPULATE_KEY_FIELD 9.
    POPULATE_KEY_FIELD 10.
  END-OF-DEFINITION .
  DEFINE CONVERT_KEY_FIELDS .
    REFRESH LT_KEY_CONV..
    CLEAR: LS_KEY_CONV,
           LS_KEY_OLD,
           LS_KEY_NEW.
    LOOP AT LT_DATA_KEY INTO LS_DATA .
      IF LS_DATA-CHNGIND = 'E' .
        LS_KEY_OLD = LS_DATA.
      ELSEIF LS_DATA-CHNGIND = 'I' .
        LS_KEY_NEW = LS_DATA.
      ENDIF.
    ENDLOOP.
    CLEAR IFLD.
    DO 10 TIMES.
      CLEAR LS_KEY_CONV.
      ADD 1 TO IFLD.
      CTMP = IFLD.
      CONCATENATE 'KEY' CTMP '_V'  INTO FLD.
      ASSIGN COMPONENT FLD OF STRUCTURE LS_KEY_OLD TO <FS_OLD>.
      ASSIGN COMPONENT FLD OF STRUCTURE LS_KEY_NEW TO <FS_NEW>.
      IF <FS_OLD> <> <FS_NEW>.
        "--- Add Data
        MOVE-CORRESPONDING LS_KEY_NEW TO LS_KEY_CONV.
        LS_KEY_CONV-CHNGIND = 'U'.
        LS_KEY_CONV-VALUE_NEW = <FS_NEW>.
        LS_KEY_CONV-VALUE_OLD = <FS_OLD>.
        CONCATENATE 'KEY' CTMP  INTO FLD.
         ASSIGN COMPONENT FLD OF STRUCTURE LS_KEY_OLD TO <FS_OLD>.
          LS_KEY_CONV-FNAME = <FS_OLD>.
        APPEND LS_KEY_CONV TO LT_KEY_CONV.
      ENDIF.
    ENDDO.
  END-OF-DEFINITION .
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_MD_CHNG_LOG'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  SELECT_MULTY:  OBJECTCLAS,
                 OBJECTID,
                 CHANGENR,                  " 15.03.21++
                 USERNAME,
                 TCODE,
                 CHANGE_IND,
                 TABNAME,
                 FNAME,
                 CHNGIND,
                 UDATE,
                 UDATE_REPET,                " 31.08.21++
                 DURATION_D,
                 DATUM.
  LV_LANGU = SY-LANGU.
  SELECT_SINGLE: LANGU,
                 MANAGE_IN_UTC,
                 BACKDAYS,
                 REPET_BACKDAYS,             " 31.08.21++
                 CONVERT_KEY,
                 HEADER_ONLY,
                 REPETITIVE.                 " 15.03.21++
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA : DATE_FROM TYPE D,
         BACKDAYS  TYPE I.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : LS_DATA LIKE LINE OF T_DATA.
  DATA : LT_DATA LIKE TABLE OF LS_DATA,
         LT_DATA_TMP LIKE TABLE OF LS_DATA.
  DATA : LT_DATA_KEY LIKE TABLE OF LS_DATA.
  DATA : LS_KEY_CONV LIKE LINE OF T_DATA,
         LS_KEY_OLD LIKE LINE OF T_DATA,
         LS_KEY_NEW LIKE LINE OF T_DATA,
         LT_KEY_CONV LIKE TABLE OF LS_KEY_CONV.
  DATA : TIME_DIFF TYPE I .
  DATA : SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
  DATA: LV_OBJECT TYPE CDOBJECTCL.
  DATA_MULTY:   OBJ_CLAS CDOBJECTCL.  "For temporary use
  DATA : DOMVALUE LIKE  DD07V-DOMVALUE_L,
         DDTEXT LIKE  DD07V-DDTEXT.
  DATA: LS_DEL TYPE CDPOS,
        LT_DEL LIKE TABLE OF LS_DEL,
        IS_DEL_ADDED(1) TYPE C.
*** Begin 15.03.21++
  DATA: LV_TABIX       TYPE I,
        LV_NUM         TYPE CHAR1,
        LV_EXIST       TYPE BOOLE_D,
        LV_APPEND      TYPE BOOLE_D,
        LV_FIELD       TYPE FIELDNAME,
        LV_LEN         TYPE I,
        LV_TABKEY      TYPE CDTABKEY,
        LV_LIFNR       TYPE LIFNR,
        LV_VENDOR_DESC TYPE NAME1_GP.
  DATA: LS_DATA_NEW   LIKE LINE OF T_DATA,
        LS_DATA_OLD   LIKE LINE OF T_DATA,
        LS_COMPONENTS TYPE ABAP_COMPDESCR,
        LS_DATA_TMP   LIKE LINE OF T_DATA,
        LS_DATA_TMP2  LIKE LINE OF T_DATA,
        LS_KEY_FIELD  TYPE TY_KEY_FIELD,
        LS_DD03L      TYPE DD03L,
        LS_INF        TYPE RPY_TABL,
        LS_TAB_FIELD  TYPE RPY_MAIN.
  DATA: LT_DATA_TMP2      LIKE TABLE OF LS_DATA,
        LT_KEY_FIELD      TYPE TT_KEY_FIELD,
        LT_DD03L          TYPE STANDARD TABLE OF DD03L,
        LT_DD03L_POS      TYPE STANDARD TABLE OF DD03L,
        LT_TAB_FIELDS     TYPE STANDARD TABLE OF RPY_MAIN,
        LT_TAB_FIELDS_TMP TYPE STANDARD TABLE OF RPY_MAIN.
  DATA: LR_STR_DESC TYPE REF TO CL_ABAP_STRUCTDESCR,
        LR_DATA     TYPE REF TO DATA.
  FIELD-SYMBOLS: <FS_FIELD_KEY> TYPE ANY,
                 <FS_VAL_KEY>   TYPE ANY,
                 <FS_VAL_KEY1>  TYPE ANY,
                 <FS_LINE>      TYPE ANY.
*** End 15.03.21++
  DATA: LV_CHANGENR TYPE CDCHANGENR.
  DATA: IS_KEY_CASE(1) TYPE C.
  "-----------------------------------------------
  " 2. Extracting & Populating Parameters        "
  "-----------------------------------------------
*** Begin 03.09.21--
*  IF r_datum[] IS INITIAL .  " Set default value
*    rs_datum-sign   = 'I' .
*    rs_datum-option = 'GE'.
****    date_from = sy-datum - lv_backdays .
*    date_from       = sy_datlo - lv_backdays.
*    rs_datum-low    = date_from .
*    APPEND rs_datum TO r_datum.
*  ENDIF .
*  r_udate[] = r_datum[].
*** End 03.09.21--
*** Begin 03.09.21++
  IF LV_REPETITIVE EQ 'X'.
    IF R_DATUM[] IS INITIAL .  " Set default value
      RS_DATUM-SIGN   = 'I' .
      RS_DATUM-OPTION = 'GE'.
      DATE_FROM       = SY_DATLO - LV_REPET_BACKDAYS.
      RS_DATUM-LOW    = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
    ENDIF .
    IF R_UDATE_REPET[] IS INITIAL.
      R_UDATE_REPET[] = R_DATUM[].
    ENDIF.
    IF LV_BACKDAYS IS INITIAL.
      LV_BACKDAYS = 1.
    ENDIF.
    IF R_UDATE[] IS INITIAL.  " Set default value
      RS_UDATE-SIGN   = 'I' .
      RS_UDATE-OPTION = 'GE'.
      DATE_FROM       = SY_DATLO - LV_BACKDAYS.
      RS_UDATE-LOW    = DATE_FROM.
      APPEND RS_UDATE TO R_UDATE.
    ENDIF.
  ELSE.
    IF R_DATUM[] IS INITIAL .  " Set default value
      RS_DATUM-SIGN   = 'I' .
      RS_DATUM-OPTION = 'GE'.
      DATE_FROM       = SY_DATLO - LV_BACKDAYS.
      RS_DATUM-LOW    = DATE_FROM.
      APPEND RS_DATUM TO R_DATUM.
    ENDIF.
    R_UDATE[] = R_DATUM[].
  ENDIF.
*** End 03.09.21++
  SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
  TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  REFRESH LT_DATA .
  IF R_OBJECTCLAS[] IS INITIAL.
    IF R_TABNAME[] IS NOT INITIAL.
      SELECT OBJECT
         FROM TCDOB
         INTO LV_OBJECT
         WHERE TABNAME IN R_TABNAME.
        RS_OBJ_CLAS-LOW    = LV_OBJECT.
        RS_OBJ_CLAS-OPTION = 'EQ'.
        RS_OBJ_CLAS-SIGN   = 'I'.
        APPEND RS_OBJ_CLAS TO R_OBJ_CLAS.
      ENDSELECT.
    ENDIF.
  ENDIF.
  LOOP AT R_OBJECTCLAS INTO RS_OBJECTCLAS.
    APPEND RS_OBJECTCLAS TO R_OBJ_CLAS.
  ENDLOOP.
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert Data           "
  "-----------------------------------------------
  "--- Check that Object Class is not empty
  IF R_OBJ_CLAS[] IS INITIAL.
    EXIT.      "!!!!!!
  ENDIF.
  SELECT *
    FROM CDHDR
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE OBJECTCLAS IN R_OBJ_CLAS
    AND   OBJECTID   IN R_OBJECTID
    AND   CHANGENR   IN R_CHANGENR           " 15.03.21++
    AND   USERNAME   IN R_USERNAME
    AND   TCODE      IN R_TCODE
    AND   CHANGE_IND IN R_CHANGE_IND
    AND   UDATE      IN R_UDATE. "
  IF LV_HEADER_ONLY EQ 'X'.
    T_DATA[] = LT_DATA[].
    READ TABLE T_DATA INTO LS_DATA INDEX 1.
    CHECK SY-TFILL IS NOT INITIAL .
    IS_ALERT = 'X' .
    EXIT.
  ENDIF.
  "-----------------------------------------------
  " 5. Post retrieving manipulations             "
  "-----------------------------------------------
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX.
    REFRESH LT_DEL.
    CLEAR IS_DEL_ADDED.
    CLEAR IS_KEY_CASE.
    REFRESH LT_DATA_KEY.
    LV_CHANGENR = LS_DATA-CHANGENR.
    REFRESH LT_DATA_TMP.
    SELECT *
     FROM CDPOS
     INTO CORRESPONDING FIELDS OF LS_DATA "TABLE T_DATA
     WHERE OBJECTCLAS EQ LS_DATA-OBJECTCLAS
     AND   OBJECTID   EQ LS_DATA-OBJECTID
     AND   CHANGENR   EQ LS_DATA-CHANGENR
     AND   TABNAME    IN R_TABNAME
"          AND FNAME IN R_FNAME
"          AND CHNGIND IN R_CHNGIND
          . "
      APPEND LS_DATA TO LT_DATA_TMP.   "  T_DATA.
      IF LS_DATA-CHNGIND = 'I'.
        IF LS_DATA-FNAME = 'KEY'.    " Insert Key
          IS_KEY_CASE = 'X'.
          POPULATE_KEY_FIELDS .
          APPEND LS_DATA TO LT_DATA_KEY.
        ENDIF.
      ENDIF.
      IF LS_DATA-CHNGIND = 'E'.
        IF IS_DEL_ADDED IS INITIAL.
          MOVE-CORRESPONDING LS_DATA TO LS_DEL.
          APPEND LS_DEL  TO LT_DEL.
          LS_DATA-FNAME = 'KEY'.
          CLEAR LS_DATA-VALUE_OLD.
          APPEND LS_DATA TO LT_DATA_TMP.  " T_DATA.
          POPULATE_KEY_FIELDS .
          APPEND LS_DATA TO LT_DATA_KEY.
          IS_DEL_ADDED = 'X'.
        ENDIF.
      ENDIF.
    ENDSELECT.
*** 15.03.21++
    IF LV_REPETITIVE EQ ABAP_TRUE.
      LV_CONVERT_KEY = 'X'.
    ENDIF.
*** 15.03.21++
    "--- Convert KEY Field (I/E)
    IF LV_CONVERT_KEY IS NOT INITIAL. " to convert KEY
*        refresh lt_DATA_KEY.
*        loop at T_DATA into LS_DATA where FNAME = 'KEY'.
*          populate_key_fields .
*          append LS_DATA to lt_DATA_KEY.
*        endloop.
      "--- Convert KEY Field (I/E)
      CONVERT_KEY_FIELDS.
      LOOP AT LT_KEY_CONV INTO LS_KEY_CONV.
        MOVE-CORRESPONDING LS_KEY_CONV TO LS_DATA.
        APPEND LS_DATA TO LT_DATA_TMP.  "T_DATA.
      ENDLOOP.
      " Delete KEY records
      IF IS_KEY_CASE IS NOT INITIAL.
        DELETE LT_DATA_TMP WHERE FNAME   = 'KEY' AND CHANGENR = LV_CHANGENR.
        DELETE LT_DATA_TMP WHERE CHNGIND = 'E'   AND CHANGENR = LV_CHANGENR.
      ENDIF.
    ENDIF.
*** Begin 15.03.21++
    IF LV_REPETITIVE EQ ABAP_TRUE.
      IF LS_DATA-UDATE IN R_UDATE.                   " 31.08.21++
        SELECT *
          FROM DD03L
          INTO TABLE LT_DD03L
          WHERE TABNAME   IN R_TABNAME
          AND   AS4LOCAL  EQ 'A'.
        SORT LT_DD03L BY TABNAME FIELDNAME.
        CALL FUNCTION 'RPY_TABLE_READ_SHORT'
          EXPORTING
            ACTIVATION_TYPE  = 'A'
            LANGUAGE         = LV_LANGU
            TABLE_NAME       = LS_DATA-TABNAME
          IMPORTING
            TABL_INF         = LS_INF
          TABLES
            TABL_FIELDS      = LT_TAB_FIELDS_TMP
          EXCEPTIONS
            CANCELLED        = 1
            NOT_FOUND        = 2
            PERMISSION_ERROR = 3
            ILLEGAL_TYPE     = 4
            OTHERS           = 5.
        IF SY-SUBRC IS INITIAL AND LT_TAB_FIELDS_TMP IS NOT INITIAL.
          DELETE LT_TAB_FIELDS_TMP WHERE KEYFLAG NE 'X'.
          LOOP AT LT_TAB_FIELDS_TMP INTO LS_TAB_FIELD.
            READ TABLE LT_TAB_FIELDS WITH KEY TABLNAME  = LS_TAB_FIELD-TABLNAME
                                              FIELDNAME = LS_TAB_FIELD-FIELDNAME
                                              TRANSPORTING NO FIELDS.
            IF SY-SUBRC IS NOT INITIAL.
              APPEND LS_TAB_FIELD TO LT_TAB_FIELDS.
            ENDIF.
          ENDLOOP.
        ELSE.
          LT_DD03L_POS = LT_DD03L.
          SORT LT_DD03L_POS BY POSITION.
          LOOP AT LT_DD03L_POS INTO LS_DD03L WHERE KEYFLAG EQ ABAP_TRUE.
            CLEAR: LS_TAB_FIELD.
            LS_TAB_FIELD-TABLNAME   = LS_DD03L-TABNAME.
            LS_TAB_FIELD-FIELDNAME  = LS_DD03L-FIELDNAME.
            LS_TAB_FIELD-DTELNAME   = LS_DD03L-ROLLNAME.
            LS_TAB_FIELD-CHECKTABLE = LS_DD03L-CHECKTABLE.
            LS_TAB_FIELD-KEYFLAG    = LS_DD03L-KEYFLAG.
            APPEND LS_TAB_FIELD TO LT_TAB_FIELDS.
          ENDLOOP.
        ENDIF.
        CLEAR: LT_TAB_FIELDS_TMP.
      ENDIF.
    ENDIF.
*** End 15.03.21++
    LOOP AT LT_DATA_TMP INTO LS_DATA.
*      APPEND ls_data TO t_data.          " 15.03.21--
*** Begin 15.03.21++
      IF LV_REPETITIVE EQ ABAP_TRUE.
        CHECK LS_DATA-UDATE IN R_UDATE.      " 31.08.21++
        CLEAR: LV_TABKEY, LV_LEN, LV_LENG.
        IF LT_DATA_TMP2 IS INITIAL.
          APPEND LS_DATA TO LT_DATA_TMP2.
        ELSE.
          LOOP AT LT_TAB_FIELDS INTO LS_TAB_FIELD WHERE TABLNAME EQ LS_DATA-TABNAME.
            LV_TABIX = SY-TABIX.
            LV_NUM   = LV_TABIX.
            CONDENSE LV_NUM.
            CONCATENATE 'KEY' LV_NUM '_V' INTO LV_FIELD.
            CREATE DATA LR_DATA TYPE (LS_TAB_FIELD-DTELNAME).
            IF LR_DATA IS BOUND.
              ASSIGN LR_DATA->* TO <FS_VAL_KEY>.
              READ TABLE LT_DD03L INTO LS_DD03L WITH KEY TABNAME   = LS_TAB_FIELD-TABLNAME
                                                         FIELDNAME = LS_TAB_FIELD-FIELDNAME
                                                         BINARY SEARCH.
            ENDIF.
            ASSIGN COMPONENT LV_FIELD OF STRUCTURE LS_DATA TO <FS_VAL_KEY>.
            IF SY-SUBRC IS INITIAL AND <FS_VAL_KEY> IS ASSIGNED.
              LV_TABKEY+LV_LEN(LS_DD03L-LENG) = <FS_VAL_KEY>+0(LS_DD03L-LENG).
              LV_LEN                          = LV_LEN + LS_DD03L-LENG.
            ENDIF.
            IF NOT LS_TAB_FIELD-FIELDNAME IN R_FNAME[].
              READ TABLE LT_KEY_FIELD WITH KEY FIELDNAME = LS_TAB_FIELD-FIELDNAME
                                      TRANSPORTING NO FIELDS.
              IF SY-SUBRC IS NOT INITIAL.
                LS_KEY_FIELD-TABNAME   = LS_TAB_FIELD-TABLNAME.
                LS_KEY_FIELD-FIELDNAME = LS_TAB_FIELD-FIELDNAME.
                APPEND LS_KEY_FIELD TO LT_KEY_FIELD.
              ENDIF.
            ENDIF.
          ENDLOOP.
          LOOP AT LT_DATA_TMP2 INTO LS_DATA_TMP.
* If tabkeys are equal,
            IF LS_DATA_TMP-VALUE_OLD EQ LS_DATA-VALUE_NEW.
              "ls_data_tmp-tabkey EQ lv_tabkey AND lv_exist IS INITIAL.
              APPEND LS_DATA TO LT_DATA_TMP2.
              LV_EXIST = 'X'.
            ELSE.
* Check if object have the same key fields, except the field that being checked
              LV_APPEND = 'X'.
              LOOP AT LT_KEY_FIELD INTO LS_KEY_FIELD.
                LV_TABIX = SY-TABIX.
                LV_NUM   = LV_TABIX.
                CONDENSE LV_NUM.
                CONCATENATE 'KEY' LV_NUM '_V' INTO LV_FIELD.
                ASSIGN COMPONENT LV_FIELD OF STRUCTURE LS_DATA     TO <FS_VAL_KEY>.
                ASSIGN COMPONENT LV_FIELD OF STRUCTURE LS_DATA_TMP TO <FS_VAL_KEY1>.
                IF <FS_VAL_KEY> IS ASSIGNED AND <FS_VAL_KEY1> IS ASSIGNED.
                  IF <FS_VAL_KEY> NE <FS_VAL_KEY1>.
                    IF LV_EXIST EQ ABAP_TRUE.
                      APPEND LINES OF LT_DATA_TMP2 TO T_DATA.
                    ENDIF.
                    CLEAR: LT_KEY_FIELD, LT_DATA_TMP2.
                    CLEAR: LV_APPEND, LV_EXIST.
* Append line of a new object
                    APPEND LS_DATA TO LT_DATA_TMP2.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDLOOP.
* Append the line with the same key field to temporary table
              IF LV_APPEND EQ ABAP_TRUE.
                READ TABLE LT_DATA_TMP2 WITH KEY UDATE  = LS_DATA-UDATE
                                                 UTIME  = LS_DATA-UTIME
                                                 TABKEY = LV_TABKEY
                  TRANSPORTING NO FIELDS.
                IF SY-SUBRC IS NOT INITIAL.
                  APPEND LS_DATA TO LT_DATA_TMP2.
                ENDIF.
                CLEAR: LV_APPEND.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.
        APPEND LS_DATA TO T_DATA.
      ENDIF.
*** End 15.03.21++
    ENDLOOP.
  ENDLOOP.
*** Begin 15.03.21++
  IF LV_EXIST EQ 'X'.
    APPEND LINES OF LT_DATA_TMP2 TO T_DATA.
  ENDIF.
*** End 15.03.21++
  "-----------------------------------------------
  " 6. Post retrieving filtering                 "
  "-----------------------------------------------
  DELETE T_DATA WHERE OBJECTCLAS NOT IN R_OBJECTCLAS.
  DELETE T_DATA WHERE CHANGE_IND NOT IN R_CHANGE_IND.
  DELETE T_DATA WHERE CHNGIND NOT IN R_CHNGIND.
  DELETE T_DATA WHERE FNAME NOT IN R_FNAME.
  LOOP AT T_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    PERFORM GET_FIELD_DESC USING    LS_DATA-TABNAME
                                    LS_DATA-FNAME
                                    LV_LANGU
                           CHANGING LS_DATA-FIELD_DESC.
    PERFORM GET_TAB_DESC USING LS_DATA-TABNAME
                               LV_LANGU
                         CHANGING LS_DATA-TAB_DESC.
**** 10/22++
    IF LS_DATA-TABNAME EQ 'LFA1' AND
       LS_DATA-OBJECTID IS NOT INITIAL.
      LV_LIFNR = LS_DATA-OBJECTID.
**    "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = LV_LIFNR
        IMPORTING
          VENDOR_DESC  = LV_VENDOR_DESC
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
      IF SY-SUBRC IS INITIAL.
        LS_DATA-OBJECT_DESC = LV_VENDOR_DESC.
        CLEAR: LV_VENDOR_DESC.
      ENDIF.
    ENDIF.
**** 10/22++
    DOMVALUE = LS_DATA-CHANGE_IND.
    CLEAR DDTEXT.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = 'CDCHNGIND'
        I_DOMVALUE = DOMVALUE
        LANGU      = LV_LANGU
      IMPORTING
        E_DDTEXT   = DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      LS_DATA-CHANGE_IND_DESC = DDTEXT.
    ENDIF.
    DOMVALUE = LS_DATA-CHNGIND.
    CLEAR DDTEXT.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = 'CDCHNGIND'
        I_DOMVALUE = DOMVALUE
        LANGU      = LV_LANGU
      IMPORTING
        E_DDTEXT   = DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      LS_DATA-CHNGIND_DESC = DDTEXT.
    ENDIF.
    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
  DESCRIBE FIELD LS_DATA-TABKEY LENGTH LV_TABKEY_LEN IN CHARACTER MODE.
  "--- Poplate Key Components
  LOOP AT T_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    POPULATE_KEY_FIELDS .
    CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES'
      EXPORTING
        BNAME      = LS_DATA-USERNAME
      IMPORTING
        NAME_FIRST = LS_DATA-NAME_FIRST
        NAME_LAST  = LS_DATA-NAME_LAST
        NAME_TEXT  = LS_DATA-NAME_TEXT
*       WA_ADRP    =
      EXCEPTIONS
        NO_DATA    = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
*  LOOP AT T_DATA INTO LS_DATA.
*    SY_TABIX = SY-TABIX .
*
*         CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*            EXPORTING
*              D_FROM          = LS_DATA-UDATE
*              T_FROM          = LS_DATA-UTIME
*              D_TO            = sy_datlo
*              T_TO            = sy_timlo
*              TIME_UNIT        = 'D'
*            IMPORTING
*              TIME_DIFF        = TIME_DIFF
*            EXCEPTIONS
*              WRONG_VALUE      = 1
*              OTHERS           = 2 .
*         IF SY-SUBRC = 0.
*           LS_DATA-DURATION_D = TIME_DIFF .
*         else.
*           LS_DATA-DURATION_D = '999999' .
*         ENDIF.
*    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
*  ENDLOOP.
*
*  DELETE T_DATA WHERE DURATION_D NOT IN R_DURATION_D.
  "-----------------------------------------------
  " 7. Finishing (Set IS_ALERT parameter)        "
  "-----------------------------------------------
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
