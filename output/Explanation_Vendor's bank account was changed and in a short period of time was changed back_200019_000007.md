# Exception Indicator: Master Data General Change Log Objects Counter - SW_10_06_OBJ_COUNT_C

## General Overview

This Exception Indicator (EI) monitors master data change documents to identify objects and fields that were changed multiple times within a configurable time window, with a focus on vendor bank account and related master data. It aggregates change-log entries by object class, object ID, table, and field, counts how many times each object/field was changed, and flags cases where the count exceeds a threshold—supporting detection of patterns such as a vendor bank account being changed and then reverted in a short period.

This EI serves as an essential control for treasury, vendor management, and compliance by:
- Enabling detection of repeated or reverted changes to vendor bank details and other master data that may indicate error correction, testing, or unauthorized adjustment
- Supporting identification of change concentration on specific objects or fields for audit and segregation-of-duties review
- Providing visibility into who changed what and when, by user and transaction, for accountability and root-cause analysis
- Supporting monitoring of high-change-frequency patterns that may require process or control improvements
- Enabling follow-up on specific object classes and tables (e.g. vendor master, bank account data) for risk-based review

The EI helps organizations spot suspicious or inconsistent change behavior—such as a bank account being updated and then quickly reverted—and supports month-end controls, vendor master reviews, and audit readiness. Data is sourced from SAP change document logic and master data change log structures.


## Problem Description

Failure to monitor repeated or reverted changes to vendor bank account and other master data creates multiple risks across financial reporting, operational control, and compliance:

**Financial and Reporting Issues**
- Unidentified reverted bank account changes may mask failed or fraudulent payment routing attempts that were corrected after the fact
- Repeated changes to vendor bank details in a short period can indicate keying errors, duplicate updates, or testing in production that distort audit trails
- Lack of visibility into change frequency on sensitive fields may delay detection of revenue or payment misdirection risks
- Concentrated changes around period-end can complicate reconciliation and financial close when discovered late

**Operational and Control Risks**
- Vendor bank account change-and-revert patterns without monitoring may indicate inadequate segregation of duties or approval controls
- Repeated changes to the same object or field may signal unstable master data or integration issues requiring process fixes
- Absence of change-count thresholds allows high-frequency or oscillating changes to go unnoticed until an incident occurs
- Inability to filter by object class, table, or user limits targeted review of high-risk areas

**Management Visibility and Decision-Making Risks**
- Lack of aggregated change counts by object and field delays management awareness of unusual change behavior
- Unidentified reverted or repeated changes reduce confidence in master data quality and control effectiveness
- Missing visibility by user and transaction code limits accountability and corrective action
- Absence of configurable time and count thresholds restricts risk-based prioritization of follow-up

## Suggested Resolution

**Immediate Response**
- Review the flagged change-count results to confirm which objects and fields (e.g. vendor bank account) show multiple changes in the window and whether any were reverted
- Verify high-count cases using change document display (e.g. transaction codes for change documents) to confirm sequence and business justification
- Check user and transaction distribution to identify single-user or single-transaction concentrations that may require escalation
- Determine business context: legitimate corrections, testing in production, or unexplained revert patterns

**System Assessment**
- Analyze the time window and date reference used for the run to ensure the lookback period and date field align with the monitoring objective
- Compare change counts and reversion patterns across object classes and tables to identify process or control gaps
- Review counter threshold settings so that only meaningful multi-change cases are flagged
- Assess whether detailed vs. aggregated output is needed for drill-down and reporting

**Corrective Actions**
- If unauthorized or erroneous changes are confirmed, follow master data correction and approval procedures (e.g. vendor bank data maintenance) and document rationale
- Escalate reverted or suspicious change patterns to treasury, vendor management, or internal audit as appropriate
- Tighten segregation of duties or approval workflows for vendor bank and other sensitive master data where repeated changes are detected
- Adjust EI parameters (time window, counter threshold, object/table/user filters) and schedule recurring runs to maintain ongoing visibility
- Document findings and remediation for audit trail and management reporting


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document Number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 4 | CHANGE_IND | Appl. object change | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 5 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 6 | CHNGIND | Change Indicator | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 7 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 8 | CONVERT_KEY | 'X' - Decompose Key Field |  | 0 | 0 |  |  |
| 9 | COUNTER | Counter |  | 0 | 0 |  |  |
| 10 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 11 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 12 | DETAILED | Detailes are presented ('X') |  | 0 | 0 |  |  |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 14 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 15 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 16 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
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
| 47 | KTOKK | Account Group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 48 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 49 | LIFNR | Supplier | CHAR | 10 | 0 | LIFNR | LIFNR |
| 50 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 51 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 52 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 53 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 54 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 55 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 56 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 57 | RECORDS | Count (Int 4) | INT4 | 10 | 0 | /SKN/E_SW_COUNT |  |
| 58 | SPRAS | Language Key |  | 0 | 0 |  |  |
| 59 | STKZN | Natural Person | CHAR | 1 | 0 | STKZN | STKZN |
| 60 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 61 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 62 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 63 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 64 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 65 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 66 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 67 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 68 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 69 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 70 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 71 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 72 | VBUND | Company ID | CHAR | 6 | 0 | VBUND | RCOMP |
| 73 | WAS_PLANND | Created from Planned | CHAR | 1 | 0 | CD_PLANNED | XFLAG |
| 74 | XCPDK | One-time account | CHAR | 1 | 0 | XCPDK | XFELD |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 74 parameters listed in the Parameters Reference Table above.

**ACT_CHNGNO** (Document Number):

Identifies the change document number created by the change in the change log. The EI uses change document data; this value links each record to the source change document.

**BACKDAYS** (Backdays):

Number of days to look back from today to form the monitoring window when no date range is supplied. The EI uses this value to compute the start of the period for reading change log entries (e.g. today minus BACKDAYS). Default in code: 10.

**CHANGENR** (Document Number):

Change document number. Used to align selection with specific change documents; the EI reads change log data keyed by change number.

**CHANGE_IND** (Appl. object change):

Application object change type (e.g. insert, update, delete) at the header level of the change document. The EI uses it to include or exclude change log entries by type of change.

**CHANGE_IND Options:**
- Values are determined by the change document object type (e.g. U, I, E, D). See change document documentation for the object class in use.

**CHANGE_IND_DESC** (Domain name):

Short description of the application object change type. Resolved from the change document object type for readability.

**CHNGIND** (Change Indicator):

Change type at field level (e.g. insert, update, delete) within the change document. The EI uses it when processing change log entries so only the desired change types are considered.

**CHNGIND Options:**
- Values are change-indicator codes (e.g. U, I, S, D). See change document domain for the object class.

**CHNGIND_DESC** (Domain name):

Short description of the change indicator. Resolved from the change document domain for readability.

**CONVERT_KEY** ('X' - Decompose Key Field):

When set to the active value, the EI decomposes or converts the table key (e.g. object ID or table key) for display or further processing. When not set, the key is used as stored.

**CONVERT_KEY Options:**
- **X**: Decompose key field (active).
- ** ** (space): Not set; key used as stored.

**COUNTER** (Counter):

Minimum number of changes on the same object/table/field that must occur within the window for the record to be flagged. The EI groups change log entries by object class, object ID, table, and field and counts changes; only groups whose count meets this threshold are output. Supplying a value (e.g. greater than 1) focuses on repeated or reverted changes (e.g. vendor bank account changed and then changed back).

**CUKY_NEW** (CUKY):

Currency code for the new value in the change document when the changed field is currency-related (e.g. bank account or payment terms).

**CUKY_OLD** (CUKY):

Currency code for the old value in the change document when the changed field is currency-related (e.g. bank account or payment terms).

**DETAILED** (Detailes are presented ('X')):

When set to the active value, the EI returns one result row per change log entry (detailed view). When not set, the EI returns one row per object/table/field group with the count of changes in the RECORDS field.

**DETAILED Options:**
- **X**: Detailed output (one row per change).
- ** ** (space): Aggregated output (one row per group with RECORDS count).

**DURATION** (Duration In Time Units):

Duration value used together with DURATION_UNIT to define a time window or threshold (e.g. “changes within N days”). Can be supplied as a single value or range; the EI uses it in combination with DURATION_UNIT for time-based logic.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is interpreted (hours, minutes, days, or full days for day-level filtering). The EI uses this with DURATION to evaluate time windows or thresholds.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** Use DURATION and DURATION_UNIT together when defining a time window or threshold (e.g. DURATION = 30 with DURATION_UNIT = D for 30 days).

**FIELD_DESC** (Short Description):

Short description of the changed field. Resolved from the repository for readability.

**FNAME** (Field Name):

Name of the changed field in the change document. The EI groups change log entries by object class, object ID, table, and this field; it appears in the result to identify which field was changed repeatedly.

**KEY1 - KEY10** (Field Name – Field Name):

Key field names (positions 1–10) that define the key structure of the change document table. The EI uses these (together with TABNAME) to interpret or decompose the object key; they determine which key components appear in the result or are used for grouping.

**KEY1_DS - KEY10_DS** (Short Description – Short Description):

Short descriptions for the key fields (positions 1–10). Resolved from the repository for key component readability.

**KEY1_V - KEY10_V** (Short Description – Short Description):

Short descriptions for the key field values (positions 1–10). Resolved from the repository for key value readability.

**KTOKK** (Account Group):

Vendor account group. Scopes change log entries by vendor account group (e.g. vendor master and bank account–related objects).

**LAND1** (Country Key):

Country key. Scopes change log entries by country (e.g. address or bank data).

**LIFNR** (Supplier):

Vendor (supplier) number. Scopes change log entries to a specific vendor; relevant when monitoring vendor master or vendor bank account changes.

**NAME_FIRST** (First Name):

First name of the user who made the change. Populated from user master when the EI enriches the result.

**NAME_LAST** (Last Name):

Last name of the user who made the change. Populated from user master when the EI enriches the result.

**NAME_TEXT** (Full Name):

Full name of the user who made the change. Populated from user master when the EI enriches the result.

**OBJECTCLAS** (Change doc. object):

Change document object class. The EI reads change log data by object class and uses it to group and filter entries (e.g. vendor master, bank account objects). Defines which business object type is monitored.

**OBJECTID** (Object value):

Object value (e.g. document number, vendor number) that uniquely identifies the changed object within the object class. The EI groups changes by this value together with table and field to count how many times the same object/field was changed.

**OBJECT_DESC** (Name):

Name or description of the object (e.g. vendor name when the object is vendor master). Resolved when the EI enriches the result.

**PLANCHNGNR** (Change number):

Planned change number when the change was created from a planned change. Scopes change log entries by planned change reference.

**RECORDS** (Count (Int 4)):

Number of changes on the same object/table/field within the window. The EI calculates this by grouping change log entries and only outputs groups whose count meets the COUNTER threshold; this field holds the count for each result row.

**SPRAS** (Language Key):

Language key for descriptions. Used when the EI resolves descriptions (e.g. for change indicators or domain texts); default in code is system language when not supplied.

**STKZN** (Natural Person):

Indicator for natural person (e.g. in vendor or partner context). Scopes change log entries by this attribute.

**STKZN Options:**
- Values are domain-specific (e.g. X: natural person; space: not). See domain STKZN for fix values.

**TABKEY** (Table Key):

Key of the changed table record. Identifies the exact record that was changed in the change document.

**TABNAME** (Table Name):

Name of the table that was changed. The EI groups change log entries by object class, object ID, this table, and field name; it appears in the result to identify which table was changed repeatedly (e.g. vendor bank table).

**TAB_DESC** (Short Description):

Short description of the table. Resolved from the repository when the EI enriches the result.

**TCODE** (Transaction Code):

Transaction code in which the change was made. The EI reads it from the change log; supports segregation-of-duties and accountability analysis.

**TEXT_CASE** (Text flag):

Indicator that the change is a text change. Scopes change log entries to include or exclude text-only changes.

**TEXT_CASE Options:**
- **X**: Text change.
- ** ** (space): Not a text change.

**UDATE** (Date):

Date of the change document. The EI uses this date to build the monitoring window (e.g. with BACKDAYS) and to filter change log entries; only entries within the date range are read and grouped.

**UNIT_NEW** (Unit):

Unit of measure for the new value when the changed field is quantity-related.

**UNIT_OLD** (Unit):

Unit of measure for the old value when the changed field is quantity-related.

**USERNAME** (User):

User who made the change. The EI reads it from the change log and uses it for grouping and scoping; supports accountability analysis.

**UTIME** (Time):

Time of the change. Read from the change document; supports time-based analysis.

**VALUE_NEW** (New value):

New contents of the changed field. Populated when detailed output is requested.

**VALUE_OLD** (Old value):

Old contents of the changed field. Populated when detailed output is requested.

**VBUND** (Company ID):

Company ID (group company). Scopes change log entries by company.

**WAS_PLANND** (Created from Planned):

Indicator that the change was created from a planned change. Scopes entries by this source.

**WAS_PLANND Options:**
- **X**: Created from planned change.
- ** ** (space): Not from planned change.

**XCPDK** (One-time account):

Indicator that the account is a one-time account. Scopes change log entries by this account type (e.g. vendor bank or partner data).

**XCPDK Options:**
- **X**: One-time account.
- ** ** (space): Not a one-time account.


### Parameter Relationships

**Time and Lookback Parameters:**

- **BACKDAYS** and **UDATE** work together to define the monitoring window: when no date range is supplied, the EI uses today minus BACKDAYS as the start date and includes change log entries whose UDATE falls in that window. Set BACKDAYS to control how far back to look (e.g. 10 for the last 10 days).
- **DURATION** and **DURATION_UNIT** work together to express a time window or threshold (e.g. “changes within 30 days”). Set DURATION to the numeric value and DURATION_UNIT to H, M, D, or F so the EI can interpret the period correctly.

**Change Count and Output Detail:**

- **COUNTER** and **RECORDS** are linked: the EI groups change log entries by object class, object ID, table, and field, counts changes per group, and only outputs groups whose count is in the COUNTER range. RECORDS holds that count in each result row. Use COUNTER (e.g. greater than 1) to focus on repeated or reverted changes.
- **DETAILED** controls whether the result is one row per change (detailed) or one row per group with RECORDS (aggregated). When DETAILED is not set, RECORDS is the main outcome; when set, each row corresponds to a single change log entry.

**Object and Key Parameters:**

- **OBJECTCLAS**, **OBJECTID**, **TABNAME**, and **FNAME** define how the EI groups change log entries: by object class, object value, table, and field. Together they determine which “same object/field” groups are counted and which appear in the result.
- **KEY1 - KEY10** (and related KEY_DS, KEY_V parameters) work with **TABNAME** and **CONVERT_KEY** to define or decompose the table key for display or filtering. When CONVERT_KEY is active, key components are resolved using the KEY and TABNAME definitions.


### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the monitoring window starts at today minus 10 days).
- **DURATION_UNIT** — Default: `D` (days).
- **DETAILED** — Default: initial (empty); aggregated output (one row per object/table/field group with RECORDS count) when not supplied.
- **CONVERT_KEY** — Default: initial (empty); key not decomposed when not supplied.
- **COUNTER** — When no range is supplied, the EI uses a default so that only groups with count greater than 1 are output (repeated or reverted changes).

**Note:** Language defaults to system language when not supplied; date reference for the monitoring window is derived from the change log date field when applicable.

### Practical Configuration Examples

**Use Case 1: Vendor bank account change-and-revert in last 10 days**
```
BACKDAYS = 10
COUNTER = 2 - 999
OBJECTCLAS = LIFNR
TABNAME = LFA1
```
**Purpose:** Focus on vendor master (object class LIFNR, table LFA1) where the same vendor/field was changed at least twice in the last 10 days, typical for detecting bank account changed and then reverted.

**Use Case 2: Repeated changes by user and transaction**
```
BACKDAYS = 30
COUNTER = 3 - 999
USERNAME = <user range>
TCODE = XK01; XK02; XK03
DETAILED = 
```
**Purpose:** Identify objects changed three or more times in 30 days by specific users and transactions (e.g. vendor create/change), with aggregated output and RECORDS count.

**Use Case 3: Full-day window for specific day filtering**
```
DURATION = 30
DURATION_UNIT = F
COUNTER = 2 - 999
OBJECTCLAS = LIFNR
UDATE = 20240101 - 20240131
```
**Purpose:** Use full days (DURATION_UNIT = F) with a single DURATION value (30) and a fixed UDATE range to evaluate changes within a specific month, flagging objects/fields changed at least twice.

**Use Case 4: Detailed drill-down for audit**
```
BACKDAYS = 14
COUNTER = 2 - 999
DETAILED = X
OBJECTCLAS = LIFNR
TABNAME = LFBK
FNAME = BANKL; BANKN; KOINH
```
**Purpose:** Retrieve detailed rows (one per change) for vendor bank table (LFBK) and specific fields in the last 14 days where the same object/field was changed at least twice, for audit trail review.

**Use Case 5: Multi-dimensional scope**
```
BACKDAYS = 7
COUNTER = 2 - 10
OBJECTCLAS = LIFNR
TABNAME = LFA1
LAND1 = DE; AT; CH
LIFNR = 1000000 - 1999999
```
**Purpose:** Narrow to vendor master in selected countries and vendor number range, flagging repeated changes (2–10) in the last week for focused review.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_MD_CHNG_COUNT | ACT_CHNGNO | Change number of the document created by this change | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGE_IND | Application object change type (U, I, E, D) | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHNGIND | Change Type (U, I, S, D) | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CUKY_NEW | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CUKY_OLD | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | FIELD_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KTOKK | Vendor account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECTCLAS | Object class | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECT_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | PLANCHNGNR | Planned change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | RECORDS | SW : Count (Int 4) | INT4(10) | /SKN/E_SW_COUNT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | STKZN | Natural Person | CHAR(1) | STKZN |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TABKEY | Changed table record key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TAB_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TCODE | Transaction in which a change was made | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TEXT_CASE | Flag: X=Text change | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UNIT_NEW | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UNIT_OLD | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VALUE_NEW | New contents of changed field | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VALUE_OLD | Old contents of changed field | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VBUND | Company ID | CHAR(6) | VBUND |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | WAS_PLANND | Flag that changes were generated from planned changes | CHAR(1) | CD_PLANNED |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | XCPDK | Indicator: Is the account a one-time account? | CHAR(1) | XCPDK |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_MD_CHNG_COUNT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_MD_CHNG_COUNT
*"----------------------------------------------------------------------
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
               HEADER_ONLY         CHAR1,
               DETAILED            BOOLE_D.
  DATA_MULTY:   OBJECTCLAS        CDOBJECTCL,
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
  DATA: LV_SHIFT      TYPE DDLENG,
        LV_LENG       TYPE DDLENG,
        LV_DOMNAME    TYPE DD07V-DOMNAME,
        LV_DOMVALUE   TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT     TYPE DD07V-DDTEXT,
        LV_OBJECT     TYPE CDOBJECTV,
        LV_TABNAME    TYPE TABNAME,
        LV_FIELD      TYPE FIELDNAME,
        LV_INDEX      TYPE I,
        LV_OBJECTCLAS TYPE CDOBJECTCL,
        LV_DOC        TYPE CDCHANGENR,
        LV_COUNT_TMP  TYPE I,
        LV_LINES      TYPE I.
  DATA: LS_DATA LIKE LINE OF T_DATA[],
        LS_ADDR TYPE BAPIADDR3.
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: SY_TABIX  LIKE SY-TABIX,
        DATE_FROM LIKE SY-DATUM .
  DATA: LT_DATA_MD  TYPE TABLE OF /SKN/S_SW_10_06_MD_CHNG_LOG,
        LT_DATA_TMP LIKE TABLE OF T_DATA,
        LT_RET     TYPE TABLE OF BAPIRET2.
  FIELD-SYMBOLS: <FS_DATA>    LIKE LINE OF T_DATA[],
                 <FS_DATA_MD> LIKE LINE OF LT_DATA_MD,
                          TYPE ANY.
* Set default parameter
  LV_BACKDAYS       = 10.
  LV_DURATION_UNIT  = 'D'.
  LV_DATE_REF_FLD   = 'UDATE'.
  LV_LANGU          = SY-LANGU.
  SELECT_MULTY:  OBJECTCLAS,
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
                 HEADER_ONLY,
                 DETAILED.
  CONVERT_MULTY: OBJECTID ALPHA.
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
  IF R_DATUM[] IS INITIAL .  " Set default value
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF .
  IF R_COUNTER IS INITIAL.
    RS_COUNTER-SIGN   = 'I'.
    RS_COUNTER-OPTION = 'GT'.
    RS_COUNTER-LOW    =  1.
    APPEND RS_COUNTER TO R_COUNTER.
  ENDIF.
  DELETE LT_DATA_MD WHERE UDATE NOT IN R_DATUM[].
  CHECK LT_DATA_MD IS NOT INITIAL.
  SORT LT_DATA_MD BY OBJECTCLAS OBJECTID TABNAME FNAME.
  DESCRIBE TABLE LT_DATA_MD LINES LV_LINES.
  LOOP AT LT_DATA_MD ASSIGNING <FS_DATA_MD>.
    LV_INDEX = SY-TABIX.
    IF LV_INDEX = 1.
      LV_OBJECTCLAS = <FS_DATA_MD>-OBJECTCLAS.
      LV_OBJECT     = <FS_DATA_MD>-OBJECTID.
      LV_TABNAME    = <FS_DATA_MD>-TABNAME.
      LV_FIELD      = <FS_DATA_MD>-FNAME.
      LV_COUNT_TMP  = 1.
      IF LV_DETAILED EQ 'X'.
        CLEAR: LS_DATA.
        MOVE-CORRESPONDING <FS_DATA_MD> TO LS_DATA.
        APPEND LS_DATA TO LT_DATA_TMP.
        CLEAR: LS_DATA.
      ENDIF.
    ELSE.
      IF LV_OBJECTCLAS = <FS_DATA_MD>-OBJECTCLAS AND
         LV_OBJECT     = <FS_DATA_MD>-OBJECTID   AND
         LV_TABNAME    = <FS_DATA_MD>-TABNAME    AND
         LV_FIELD      = <FS_DATA_MD>-FNAME.
        LV_COUNT_TMP = LV_COUNT_TMP + 1.
        IF LV_DETAILED EQ 'X'.
          CLEAR: LS_DATA.
          MOVE-CORRESPONDING <FS_DATA_MD> TO LS_DATA.
          APPEND LS_DATA TO LT_DATA_TMP.
        ELSE.
          IF LS_DATA IS INITIAL.
            MOVE-CORRESPONDING <FS_DATA_MD> TO LS_DATA.
          ENDIF.
        ENDIF.
      ELSE.
        IF LV_COUNT_TMP IN R_COUNTER[].
          IF LV_DETAILED EQ 'X'.
            LOOP AT LT_DATA_TMP ASSIGNING <FS_DATA>.
              <FS_DATA>-RECORDS = LV_COUNT_TMP.
            ENDLOOP.
            APPEND LINES OF LT_DATA_TMP TO T_DATA[].
            CLEAR: LT_DATA_TMP.
          ELSE.
            LS_DATA-RECORDS = LV_COUNT_TMP.
            IF LV_SW_DEST IS INITIAL.
              CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
                EXPORTING
                  BNAME      = LS_DATA-USERNAME
                IMPORTING
                  NAME_FIRST = LS_DATA-NAME_FIRST
                  NAME_LAST  = LS_DATA-NAME_LAST
                  NAME_TEXT  = LS_DATA-NAME_TEXT
                EXCEPTIONS
                  NO_DATA    = 1
                  OTHERS     = 2.
            ELSE.
              CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
                EXPORTING
                  BNAME      = LS_DATA-USERNAME
                  SW_DEST    = LV_SW_DEST
                IMPORTING
                  NAME_FIRST = LS_DATA-NAME_FIRST
                  NAME_LAST  = LS_DATA-NAME_LAST
                  NAME_TEXT  = LS_DATA-NAME_TEXT
                EXCEPTIONS
                  NO_DATA    = 1
                  OTHERS     = 2.
            ENDIF.
            APPEND LS_DATA TO T_DATA[].
            CLEAR: LS_DATA.
          ENDIF.
        ENDIF.
        LV_OBJECTCLAS = <FS_DATA_MD>-OBJECTCLAS.
        LV_OBJECT     = <FS_DATA_MD>-OBJECTID.
        LV_TABNAME    = <FS_DATA_MD>-TABNAME.
        LV_FIELD      = <FS_DATA_MD>-FNAME.
        LV_COUNT_TMP  = 1.
        IF LV_DETAILED EQ 'X'.
          CLEAR: LS_DATA, LT_DATA_TMP.
          MOVE-CORRESPONDING <FS_DATA_MD> TO LS_DATA.
          APPEND LS_DATA TO LT_DATA_TMP.
        ENDIF.
      ENDIF.
      IF LV_INDEX = LV_LINES.
        IF LV_COUNT_TMP IN R_COUNTER[].
          IF LV_DETAILED EQ 'X'.
            LOOP AT LT_DATA_TMP ASSIGNING <FS_DATA>.
              <FS_DATA>-RECORDS = LV_COUNT_TMP.
            ENDLOOP.
            APPEND LINES OF LT_DATA_TMP TO T_DATA[].
          ELSE.
            LS_DATA-RECORDS = LV_COUNT_TMP.
            IF LS_DATA IS INITIAL.
              MOVE-CORRESPONDING <FS_DATA_MD> TO LS_DATA.
            ENDIF.
            IF LV_SW_DEST IS INITIAL.
              CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
                EXPORTING
                  BNAME      = LS_DATA-USERNAME
                IMPORTING
                  NAME_FIRST = LS_DATA-NAME_FIRST
                  NAME_LAST  = LS_DATA-NAME_LAST
                  NAME_TEXT  = LS_DATA-NAME_TEXT
                EXCEPTIONS
                  NO_DATA    = 1
                  OTHERS     = 2.
            ELSE.
              CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
                EXPORTING
                  BNAME      = LS_DATA-USERNAME
                  SW_DEST    = LV_SW_DEST
                IMPORTING
                  NAME_FIRST = LS_DATA-NAME_FIRST
                  NAME_LAST  = LS_DATA-NAME_LAST
                  NAME_TEXT  = LS_DATA-NAME_TEXT
                EXCEPTIONS
                  NO_DATA    = 1
                  OTHERS     = 2.
            ENDIF.
            APPEND LS_DATA TO T_DATA[].
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF LV_DETAILED EQ 'X'.
    LOOP AT T_DATA ASSIGNING <FS_DATA>.
      IF LV_SW_DEST IS INITIAL.
        CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
          EXPORTING
            BNAME      = LS_DATA-USERNAME
          IMPORTING
            NAME_FIRST = LS_DATA-NAME_FIRST
            NAME_LAST  = LS_DATA-NAME_LAST
            NAME_TEXT  = LS_DATA-NAME_TEXT
          EXCEPTIONS
            NO_DATA    = 1
            OTHERS     = 2.
      ELSE.
        CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
          EXPORTING
            BNAME      = LS_DATA-USERNAME
            SW_DEST    = LV_SW_DEST
          IMPORTING
            NAME_FIRST = LS_DATA-NAME_FIRST
            NAME_LAST  = LS_DATA-NAME_LAST
            NAME_TEXT  = LS_DATA-NAME_TEXT
          EXCEPTIONS
            NO_DATA    = 1
            OTHERS     = 2.
      ENDIF.
    ENDLOOP.
  ENDIF.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
