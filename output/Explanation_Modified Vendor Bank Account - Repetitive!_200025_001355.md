# Exception Indicator: MD: Vendor Bank Details Change Log - SW_10_06_VND_BANK_CH

## General Overview

This Exception Indicator (EI) monitors change documents for vendor bank account (and related master data) in Materials Management (MM) / Financial Supply Chain Management (FSCM) to identify repetitive or one-time changes to vendor bank details across a configurable time window. It provides visibility into who changed what, when, and at which key (e.g. vendor, bank key) for audit and control.

This EI serves as an essential control for vendor master and bank data governance by:
- Enabling detection of repetitive changes to vendor bank account data that may indicate errors, fraud, or process issues
- Supporting identification of changes by user, transaction code, table, and field for prioritization and root-cause analysis
- Providing visibility into change date and optional repetitive window (REPETITIVE + REPET_BACKDAYS) for monitoring patterns over time
- Enabling analysis of change documents by object class, object ID (e.g. vendor), and key decomposition (KEY1–KEY10) for audit and compliance
- Supporting accountability by user and transaction for audit trail and data governance

This monitoring enables organizations to detect unauthorized or anomalous vendor bank changes, repetitive corrections that may indicate system or process issues, and concentration patterns that may indicate fraud. The EI is particularly valuable for vendor master data governance, period-end reconciliation, and exception management in procurement and treasury.

The EI uses change document data (CDHDR, CDPOS) and optional key decomposition (KEY1–KEY10 with TABNAME); output is filtered by the configured parameters including BACKDAYS, REPETITIVE, REPET_BACKDAYS, and selection criteria.


## Problem Description

Failure to monitor vendor bank account (and related) change documents creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unmonitored changes to vendor bank details can lead to incorrect or fraudulent payment instructions and financial loss
- Repetitive changes to the same vendor or bank key may indicate errors, testing in production, or attempted fraud
- Unreconciled change document exceptions can lead to audit findings and misstated or incomplete audit trails
- Delayed identification of anomalous changes may complicate period-end close and require corrective payments or reversals
- Concentration of changes in certain vendors or users can mask systemic process or control issues

**Operational and Control Risks**
- Vendor bank account changes without visibility by user, date, or transaction limit ability to prioritize review and remediation
- Unmonitored repetitive pattern (same object changed multiple times in a window) can delay detection of process or system issues
- Missing filtering by object class, table, or field can lead to noise or missed exceptions
- Insufficient visibility by change date or repetitive window restricts actionable reporting for auditors and operations
- Lack of key decomposition (e.g. vendor, bank key) visibility limits ability to trace changes to specific master data keys

**Management Visibility and Decision-Making Risks**
- Absence of exception monitoring delays awareness of vendor bank changes and repetitive patterns
- Unidentified concentration of changes by vendor or user can lead to missed process improvements or fraud indicators
- Lack of visibility by date or repetitive window limits ability to escalate aging or high-frequency changes for review
- Insufficient filtering by organizational and change dimensions restricts actionable reporting for auditors and operations

## Suggested Resolution

**Immediate Response**
- Review the change documents flagged by the EI to understand scope (repetitive changes, vendor or user concentration)
- Verify change document and vendor bank master (e.g. XK02, FK02) to confirm current state and legitimacy of changes
- Check user, transaction code, and change date to determine if manual correction or escalation is pending
- Identify business context for exceptions: vendor, bank key, table, field, or user responsible

**System Assessment**
- Analyze the time window and repetitive window (BACKDAYS, REPET_BACKDAYS) used for monitoring to ensure alignment with data governance and audit cycles
- Examine object class, table, and field filters to confirm the result set supports the intended analysis (e.g. vendor bank only)
- Assess vendor, user, and transaction code distribution to identify patterns or process issues
- Validate that repetitive mode (REPETITIVE = X) and REPET_BACKDAYS are interpreted correctly for pattern detection
- Review key decomposition (KEY1–KEY10) and TABNAME for change document structure and traceability

**Corrective Actions**
- Correct or reverse vendor bank data using standard transactions (e.g. FK02) where errors or illegitimate changes are confirmed
- For repetitive or erroneous changes, escalate to process owners or security; update workflows or authorizations as needed
- Update vendor master or bank details if misconfiguration or data quality issues are found
- Adjust monitoring parameters (BACKDAYS, REPETITIVE, REPET_BACKDAYS, object class, table) to align with policy and re-run the EI
- Document exceptions and resolutions for audit trail; establish recurring EI runs for continuous visibility into vendor bank change documents


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document Number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | BANKN | Bank Account compare |  | 0 | 0 |  |  |
| 4 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 5 | CHANGE_IND | Appl. object change-Header Lvl | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 6 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 7 | CHNGIND | Change Indicator-Row lvl | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 8 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 9 | CONVERT_KEY | 'X' - Decompose Key Field |  | 0 | 0 |  |  |
| 10 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 11 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 12 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 13 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 14 | KEY1 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 15 | KEY10 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 16 | KEY10_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 17 | KEY10_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 18 | KEY1_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 19 | KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 20 | KEY2 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 21 | KEY2_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 22 | KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 23 | KEY3 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 24 | KEY3_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 25 | KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 26 | KEY4 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 27 | KEY4_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 28 | KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 29 | KEY5 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 30 | KEY5_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 31 | KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 32 | KEY6 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 33 | KEY6_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 34 | KEY6_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 35 | KEY7 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 36 | KEY7_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 37 | KEY7_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 38 | KEY8 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 39 | KEY8_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 40 | KEY8_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 41 | KEY9 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 42 | KEY9_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 43 | KEY9_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 44 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 45 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 46 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 47 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 48 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 49 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 50 | OBJECTID | Vendor | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 51 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 52 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 53 | REPETITIVE | 'X' - Repetitive Change | CHAR | 1 | 0 | /SKN/E_REPEAT | XFLAG |
| 54 | REPET_BACKDAYS | Repetitive Backdays |  | 0 | 0 |  |  |
| 55 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 56 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 57 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 58 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 59 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 60 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 61 | UDATE_REPET | Repetitive Date |  | 0 | 0 |  |  |
| 62 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 63 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 64 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 65 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 66 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 67 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 68 | WAS_PLANND | Created from Planned | CHAR | 1 | 0 | CD_PLANNED | XFLAG |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 68 parameters listed in the Parameters Reference Table above.

**ACT_CHNGNO** (Document Number):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BACKDAYS** (Backdays):

Number of days to look back from today when building the default monitoring window. When no date range is supplied, the EI uses today minus this value as the start of the window for UDATE.

**BANKN** (Bank Account compare):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CHANGENR** (Document Number):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CHANGE_IND** (Appl. object change-Header Lvl):

Restricts which change documents are included or populates the result. CHAR(1) / flag where applicable.

**CHANGE_IND Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**CHANGE_IND_DESC** (Domain name):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CHNGIND** (Change Indicator-Row lvl):

Restricts which change documents are included or populates the result. CHAR(1) / flag where applicable.

**CHNGIND Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**CHNGIND_DESC** (Domain name):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CONVERT_KEY** ('X' - Decompose Key Field):

Restricts which change documents are included or populates the result. CHAR(1) / flag where applicable.

**CONVERT_KEY Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**CUKY_NEW** (CUKY):

New currency value (e.g. bank account currency after change). Business meaning: currency in which the amount or account is stated after the change.

**CUKY_OLD** (CUKY):

Previous currency value (e.g. bank account currency before change). Business meaning: currency in which the amount or account was stated before the change.

**FIELD_DESC** (Short Description):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**FNAME** (Field Name):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KEY1 - KEY10** (Field Name – Field Name):

Key field names (1–10) for the table (TABNAME) of the change document. When CONVERT_KEY is set, the table key is decomposed into these components; each KEYn holds the field name and KEYn_V holds the value. Restricts or populates the result for change document key analysis.

**LANGU** (Language for texts):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

Restricts which change documents are included or populates the result. CHAR(1) / flag where applicable.

**MANAGE_IN_UTC Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**NAME_FIRST** (First Name):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**NAME_LAST** (Last Name):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**NAME_TEXT** (Full Name):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**OBJECTCLAS** (Change doc. object):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**OBJECTID** (Vendor):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**OBJECT_DESC** (Name):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PLANCHNGNR** (Change number):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**REPETITIVE** ('X' - Repetitive Change):

Restricts which change documents are included or populates the result. CHAR(1) / flag where applicable.

**REPETITIVE Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**REPET_BACKDAYS** (Repetitive Backdays):

Number of days for the repetitive window when REPETITIVE is set. The repetitive window starts today minus REPET_BACKDAYS and is applied to UDATE_REPET for repetitive change detection.

**TABKEY** (Table Key):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**TABNAME** (Table Name):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**TAB_DESC** (Short Description):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**TCODE** (Transaction Code):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**TEXT_CASE** (Text flag):

Restricts which change documents are included or populates the result. CHAR(1) / flag where applicable.

**TEXT_CASE Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**UDATE** (Date):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UDATE_REPET** (Repetitive Date):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UNIT_NEW** (Unit):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UNIT_OLD** (Unit):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**USERNAME** (User):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UTIME** (Time):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VALUE_NEW** (New value):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VALUE_OLD** (Old value):

Restricts which change document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**WAS_PLANND** (Created from Planned):

Restricts which change documents are included or populates the result. CHAR(1) / flag where applicable.

**WAS_PLANND Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.


### Parameter Relationships

**Time and repetitive parameters:**
- BACKDAYS defines the lookback window (days from today) when no date range is supplied. The EI builds the date range from today minus BACKDAYS and applies it to UDATE (change date).
- REPETITIVE when set (e.g. X) enables repetitive change monitoring: a second window (REPET_BACKDAYS) is used for UDATE_REPET so that changes that fall within the repetitive window can be identified. When REPETITIVE is not set, standard single-window mode applies (UDATE only).
- REPET_BACKDAYS is used when REPETITIVE is set: it defines the repetitive window (days from today). UDATE_REPET is then filtered by this window. Use BACKDAYS and REPET_BACKDAYS together with REPETITIVE to focus on changes that occur repeatedly within the repetitive window.

**Change document key and table:**
- TABNAME (table name) identifies which table’s change documents are read (e.g. LFBK for vendor bank). OBJECTCLAS (object class) and OBJECTID (object ID, e.g. vendor number) work with TABNAME to scope the change documents. KEY1–KEY10 (and KEY1_DS–KEY10_DS, KEY1_V–KEY10_V) represent the key field names and values of the changed record; they are populated when CONVERT_KEY is set so that the table key is decomposed into readable key components.

**User and transaction:**
- USERNAME (user) and TCODE (transaction code) restrict which change documents are included. They work together with UDATE and CHANGE_IND (header change indicator) when analyzing who changed what and through which transaction.

**Currency (vendor bank):**
- CUKY_OLD and CUKY_NEW represent the old and new currency (e.g. bank account currency) when the changed field is currency-related. They appear in the result for before/after comparison.


### Default Values

- **BACKDAYS** — Default: `1` (lookback window when no date range is supplied; used for UDATE when REPETITIVE is set, and for the primary date range when REPETITIVE is not set).
- **REPET_BACKDAYS** — Default: initial (0); when REPETITIVE is set and REPET_BACKDAYS is not supplied, the repetitive window start is effectively today (or see code for actual default).
- **REPETITIVE** — Default: initial (empty); standard single-window mode when not supplied.
- **LANGU** — Default: system language or English; used for key field and domain descriptions when CONVERT_KEY is used.

**Note:** When REPETITIVE is set, the EI uses two date ranges: one from today minus BACKDAYS (for UDATE) and one from today minus REPET_BACKDAYS (for UDATE_REPET). When REPETITIVE is not set, only BACKDAYS applies to UDATE.

### Practical Configuration Examples

**Use Case 1: Repetitive vendor bank changes in last 7 days**
```
BACKDAYS = 7
REPETITIVE = X
REPET_BACKDAYS = 7
TABNAME = LFBK
```
**Purpose:** Identify change documents for vendor bank (LFBK) where changes occur repeatedly within the last 7 days for anomaly and process review.

**Use Case 2: One-time changes by user**
```
BACKDAYS = 30
REPETITIVE = 
USERNAME = (user range)
OBJECTCLAS = (vendor object class)
```
**Purpose:** Review vendor-related change documents in the last 30 days by specific user(s) for audit and accountability.

**Use Case 3: Key decomposition for audit**
```
BACKDAYS = 14
CONVERT_KEY = X
TABNAME = LFBK
OBJECTID = (vendor range)
```
**Purpose:** Retrieve vendor bank change documents for the last 14 days with key fields decomposed (KEY1–KEY10, KEY1_V–KEY10_V) for readable audit trail.

**Use Case 4: Change date and transaction**
```
BACKDAYS = 5
UDATE = (date range)
TCODE = FK02
```
**Purpose:** Focus on changes posted in the last 5 days via transaction FK02 (Change Vendor) for vendor master and bank detail review.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_MD_CHNG_LOG | ACT_CHNGNO | Change number of the document created by this change | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHANGE_IND | Application object change type (U, I, E, D) | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHNGIND | Change Type (U, I, S, D) | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CUKY_NEW | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_LOG | CUKY_OLD | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_LOG | FIELD_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY10_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY10_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY1_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY2_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY3_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY4_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY5_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY6_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY6_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY7_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY7_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY8_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY8_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY9_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | KEY9_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_06_MD_CHNG_LOG | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_06_MD_CHNG_LOG | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | OBJECTCLAS | Object class | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_06_MD_CHNG_LOG | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_06_MD_CHNG_LOG | OBJECT_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_MD_CHNG_LOG | PLANCHNGNR | Planned change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_06_MD_CHNG_LOG | REPETITIVE | 'X' - Repetitive change | CHAR(1) | /SKN/E_REPEAT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TABKEY | Changed table record key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TAB_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TCODE | Transaction in which a change was made | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_06_MD_CHNG_LOG | TEXT_CASE | Flag: X=Text change | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_06_MD_CHNG_LOG | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_06_MD_CHNG_LOG | UNIT_NEW | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | UNIT_OLD | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_06_MD_CHNG_LOG | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_06_MD_CHNG_LOG | VALUE_NEW | New contents of changed field | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_06_MD_CHNG_LOG | VALUE_OLD | Old contents of changed field | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_06_MD_CHNG_LOG | WAS_PLANND | Flag that changes were generated from planned changes | CHAR(1) | CD_PLANNED |

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