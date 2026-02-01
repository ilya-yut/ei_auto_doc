# Exception Indicator: Master Data General Change Log - SW_10_06_ANY_MD_CHNG

## General Overview

This Exception Indicator (EI) monitors master data change documents to identify changes to master data objects (e.g. customer address details, vendor data) recorded in SAP change document headers and positions. It provides visibility into who changed what, when, and in which transaction, supporting audit, compliance, and data governance.

This EI serves as an essential control for master data governance and compliance by:
- Enabling detection of master data changes (inserts, updates, deletions) that may affect data quality, reporting, or downstream processes
- Supporting identification of changes by object class, table, user, and transaction for accountability and audit trails
- Providing visibility into change history by time window for period-based review and exception management
- Enabling analysis of key field decompositions and before/after values for root-cause and impact analysis
- Supporting compliance and data stewardship by surfacing change documents that require review or remediation

Monitoring change documents helps organizations track master data evolution, verify that sensitive changes are authorized, and resolve data quality or consistency issues. The EI is particularly valuable for month-end close, audit preparation, and master data governance processes.

The EI uses change document header (CDHDR) and position (CDPOS) data and supports filtering by object class, object ID, change number, user, transaction code, change type, table name, field name, and date range.


## Problem Description

Failure to monitor master data change documents creates multiple risks across data quality, operational management, and compliance:

**Financial and Reporting Issues**
- Unreviewed changes to master data (e.g. payment terms, bank details, reconciliation accounts) can lead to incorrect posting or payment runs and distort financial reporting
- Changes to customer or vendor address or tax data may affect invoicing and credit management without timely visibility
- Lack of a clear change trail complicates period-end reconciliation and audit evidence collection

**Operational and Control Risks**
- Unauthorized or erroneous master data changes can propagate to transactions and cause operational failures or duplicate records
- Inability to filter change documents by table, user, or transaction limits effective segregation of duties and exception handling
- Missing visibility into repetitive or bulk changes increases the risk of undetected data quality degradation

**Management Visibility and Decision-Making Risks**
- Absence of change-document monitoring delays management awareness of sensitive or high-impact master data changes
- Lack of time-window and object-based analysis limits the ability to prioritize reviews and allocate stewardship resources
- Inadequate change history visibility hinders root-cause analysis and corrective action when data issues are reported

## Suggested Resolution

**Immediate Response**
- Review the change documents flagged by the EI to confirm the scope and nature of the changes (object class, table, user, transaction, change type)
- Verify high-impact or sensitive changes (e.g. bank details, payment terms) using the relevant transaction (e.g. VD02 for customer, XD02 for customer master) to confirm legitimacy
- Check change type (insert, update, delete) and key field or value changes to assess impact on downstream processes
- Identify business context: planned maintenance, bulk upload, or potential unauthorized change

**System Assessment**
- Analyze the time window and object/table filters used for the run to ensure the monitoring scope matches the control objective
- Compare change volumes and patterns to prior periods to detect unusual activity or concentration by user or transaction
- Review table and field filters to ensure the EI is focused on the right master data objects and attributes
- Validate that key decomposition and header-only options align with the intended use (audit list vs. detail analysis)

**Corrective Actions**
- If unauthorized or erroneous changes are found, correct master data through the appropriate transaction and document the correction
- Escalate repeated or suspicious change patterns to data owners and access management for review of authorizations
- Update monitoring parameters (e.g. lookback period, object class, table name) to align with governance and audit requirements
- Document change-review outcomes and establish recurring EI runs to maintain continuous visibility into master data changes


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document Number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 4 | CHANGE_IND | Appl. object change-Header Lvl | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 5 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 6 | CHNGIND | Change Indicator-Row lvl | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 7 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 8 | CONVERT_KEY | 'X' - Decompose Key Field |  | 0 | 0 |  |  |
| 9 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 10 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 11 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 12 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 13 | KEY1 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 14 | KEY10 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 15 | KEY10_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 16 | KEY10_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 17 | KEY1_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 18 | KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 19 | KEY2 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 20 | KEY2_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 21 | KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 22 | KEY3 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 23 | KEY3_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 24 | KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 25 | KEY4 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 26 | KEY4_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 27 | KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 28 | KEY5 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 29 | KEY5_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 30 | KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 31 | KEY6 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 32 | KEY6_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 33 | KEY6_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 34 | KEY7 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 35 | KEY7_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 36 | KEY7_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 37 | KEY8 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 38 | KEY8_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 39 | KEY8_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 40 | KEY9 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 41 | KEY9_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 42 | KEY9_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 43 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 44 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 45 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 46 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 47 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 48 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 49 | OBJECTID | Customer | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 50 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 51 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 52 | REPETITIVE | 'X' - Repetitive Change | CHAR | 1 | 0 | /SKN/E_REPEAT | XFLAG |
| 53 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 54 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 55 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 56 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 57 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 58 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 59 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 60 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 61 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 62 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 63 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 64 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 65 | WAS_PLANND | Created from Planned | CHAR | 1 | 0 | CD_PLANNED | XFLAG |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 65 parameters listed in the Parameters Reference Table above.

**ACT_CHNGNO** (Document Number):

Document number of the change document created by the change. Identifies the follow-up change document when applicable; used for traceability and linking related change records.

**BACKDAYS** (Backdays):

Number of days to look back from the current date when building the default monitoring window. When no explicit date range is supplied, the EI uses today minus this value as the start of the window for change document selection. Used in both standard and repetitive modes to define the lookback period.

**CHANGENR** (Document Number):

Change document number. Restricts which change documents are selected; used to focus on specific change events or a range of change numbers.

**CHANGE_IND** (Appl. object change-Header Lvl):

Application object change type at header level (e.g. update, insert, delete). Restricts which change document headers are included based on the type of change recorded for the object.

**CHANGE_IND Options:**
- **U**: Update.
- **I**: Insert.
- **E**: Delete (end-of-life).
- **D**: Other change types per domain CDCHNGIND.

**CHANGE_IND_DESC** (Domain name):

Short description of the change type at header level. Populated from the domain for the change indicator; used for display in the result set.

**CHNGIND** (Change Indicator-Row lvl):

Change type at position level (e.g. insert, update, delete for a single field). Restricts which change document positions are included.

**CHNGIND Options:**
- **I**: Insert.
- **U**: Update.
- **S**: (as in domain).
- **E**: Delete.
- **D**: Other per domain CDCHNGIND.

**CHNGIND_DESC** (Domain name):

Short description of the change type at position level. Populated from the domain; used for display.

**CONVERT_KEY** ('X' - Decompose Key Field):

When set, the EI decomposes the table key into individual key components (KEY1–KEY10, KEY1_V–KEY10_V, KEY1_DS–KEY10_DS) and can output before/after comparisons for key changes. When not set, key values remain in TABKEY and are not decomposed.

**CONVERT_KEY Options:**
- **X**: Decompose key field; populate key component fields and optional value/description.
- ** ** (space or initial): Do not decompose; key remains in TABKEY only.

**CUKY_NEW** (CUKY):

Currency of the new value in the change document position when the changed field is currency-related. Represents the currency associated with the new value for comparison and display.

**CUKY_OLD** (CUKY):

Currency of the old value in the change document position when the changed field is currency-related. Represents the currency associated with the previous value for comparison and display.

**FIELD_DESC** (Short Description):

Short description of the changed field. Populated from the data dictionary for the table field; used for display in the result.

**FNAME** (Field Name):

Name of the table field that was changed. Restricts which change document positions are included (e.g. only changes to specific fields); also appears in the result.

**KEY1 - KEY10** (Field Name – Field Name):

Key field names (1–10) for the table (TABNAME) of the change document. When key decomposition is active, these hold the key component field names; populated from the table key structure. Used to identify which key components changed and for display.

**KEY1_DS - KEY10_DS** (Short Description – Short Description):

Short descriptions for the key fields (1–10). Populated from the data dictionary when key decomposition is used; used for display.

**KEY1_V - KEY10_V** (Short Description – Short Description):

Key component values (1–10) when key decomposition is used. Populated from the table key; used for display and comparison of key values.

**LANGU** (Language for texts):

Language used when retrieving short descriptions for fields, tables, and domain values. Determines the language of FIELD_DESC, TAB_DESC, CHANGE_IND_DESC, CHNGIND_DESC, and key field descriptions.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set, date and time used for the monitoring window and comparisons are interpreted in UTC. When not set, system local date/time is used. Affects how the default date range and time-based filtering are applied.

**MANAGE_IN_UTC Options:**
- **X**: Use UTC for date/time.
- ** ** (space or initial): Use system local date/time.

**NAME_FIRST** (First Name):

First name of the user who performed the change. Populated from user master; used for display.

**NAME_LAST** (Last Name):

Last name of the user who performed the change. Populated from user master; used for display.

**NAME_TEXT** (Full Name):

Full name of the user who performed the change. Populated from user master; used for display.

**OBJECTCLAS** (Change doc. object):

Change document object class. Restricts which change documents are selected (e.g. by object type); mandatory for selection—if empty and no table-derived object class, the EI exits without data.

**OBJECTID** (Customer):

Object ID of the changed object (e.g. customer number, vendor number, document number). Restricts which change documents are included; used to focus on specific master data objects.

**OBJECT_DESC** (Name):

Description of the object (e.g. customer or vendor name). Populated from master data where applicable; used for display.

**PLANCHNGNR** (Change number):

Planned change number when the change document was created from a planned change. Populated from the change document; used for display and linking.

**REPETITIVE** ('X' - Repetitive Change):

When set, the EI runs in repetitive mode: it uses a separate lookback for the repetitive window and returns only changes that fall within the repetitive date range. When not set, standard single-run mode is used with one lookback period.

**REPETITIVE Options:**
- **X**: Repetitive change mode; use repetitive lookback and date filter.
- ** ** (space or initial): Standard mode; single lookback.

**TABKEY** (Table Key):

Table key of the changed record (concatenated key). Populated from the change document position; when key decomposition is not used, this is the only key representation in the result.

**TABNAME** (Table Name):

Table name of the changed object. Restricts which change document positions are included (e.g. only LFA1, KNA1); also appears in the result and drives key decomposition and field descriptions.

**TAB_DESC** (Short Description):

Short description of the table. Populated from the data dictionary; used for display.

**TCODE** (Transaction Code):

Transaction code with which the change was made. Restricts which change documents are included; used for accountability and filtering by transaction.

**TEXT_CASE** (Text flag):

Indicates whether the change document position is a text case. Used for filtering and display; domain XFELD.

**TEXT_CASE Options:**
- **X**: Text case.
- ** ** (space or initial): Not a text case.

**UDATE** (Date):

Change date of the change document. Restricts which change documents are included by date; when no explicit range is supplied, the default range is derived from the lookback period. Also appears in the result.

**UNIT_NEW** (Unit):

Unit of measure of the new value when the changed field is quantity-related. Represents the unit for the new value; used for display.

**UNIT_OLD** (Unit):

Unit of measure of the old value when the changed field is quantity-related. Represents the unit for the previous value; used for display.

**USERNAME** (User):

User who performed the change. Restricts which change documents are included; used for accountability and audit.

**UTIME** (Time):

Change time of the change document position. Appears in the result; used for ordering and display.

**VALUE_NEW** (New value):

New value of the changed field after the change. Populated from the change document position; used for display and comparison.

**VALUE_OLD** (Old value):

Old value of the changed field before the change. Populated from the change document position; used for display and comparison.

**WAS_PLANND** (Created from Planned):

Indicates whether the change document was created from a planned change. Used for filtering and display; domain XFLAG.

**WAS_PLANND Options:**
- **X**: Created from planned change.
- ** ** (space or initial): Not from planned change.


### Parameter Relationships

**Time and lookback parameters**

- **BACKDAYS** defines the default number of days to look back when no explicit date range is supplied. It is used together with the date range for change document selection (UDATE). In repetitive mode, **REPETITIVE** works with BACKDAYS to define the standard lookback window; the repetitive date filter is applied in addition when repetitive mode is active.
- **UDATE** restricts which change documents are included by change date. The default range for UDATE is derived from BACKDAYS (today minus BACKDAYS) when no range is provided. In repetitive mode, the repetitive date range is used in addition for filtering.

**Key decomposition and display**

- **CONVERT_KEY** works with **TABNAME** and the KEY1–KEY10, KEY1_DS–KEY10_DS, KEY1_V–KEY10_V output fields. When CONVERT_KEY is set, the EI decomposes the table key (TABKEY) into key component names (KEY1–KEY10), descriptions (KEY1_DS–KEY10_DS), and values (KEY1_V–KEY10_V) based on TABNAME. **LANGU** is used when populating key and field descriptions.
- **TABNAME** and **FNAME** together define which table and field changes are selected; TABNAME also drives key decomposition when CONVERT_KEY is used.

**Object and change scope**

- **OBJECTCLAS**, **OBJECTID**, **CHANGENR**, **USERNAME**, **TCODE**, **CHANGE_IND**, **TABNAME**, **FNAME**, **CHNGIND**, and **UDATE** together define the selection scope for change document headers and positions. OBJECTCLAS is mandatory for the EI to run; the others narrow the result set by object, change number, user, transaction, change type, table, field, and date.


### Default Values

- **BACKDAYS** — Default: initial (0); when no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window. In the code, when BACKDAYS is initial in repetitive mode it is set to 1.
- **REPETITIVE** — Default: initial (empty); standard single-run mode when not set. When set to 'X', repetitive mode is used with a separate lookback and date filter.
- **LANGU** — Default: system language (e.g. from SY-LANGU) when not supplied; used for field, table, and domain descriptions.
- **MANAGE_IN_UTC** — Default: initial (empty); date and time are interpreted in system local time when not set. When set to 'X', UTC is used for the monitoring window and comparisons.
- **CONVERT_KEY** — Default: initial (empty); key decomposition is not performed when not set. When set to 'X', table key is decomposed into KEY1–KEY10, KEY1_DS–KEY10_DS, KEY1_V–KEY10_V.

**Note:** In repetitive mode, when no explicit date range is supplied for the repetitive window, the start date is derived from the repetitive lookback period; the standard UDATE range is derived from BACKDAYS.

### Practical Configuration Examples

**Use Case 1: Customer address change review (recent days)**

```
OBJECTCLAS = BU_PARTNER
TABNAME = ADRC
UDATE = last 7 days (or use BACKDAYS to define window)
USERNAME = (optional, to focus on specific users)
```

**Purpose:** Focus on recent changes to customer address data (e.g. ADRC) for a given object class, to support address verification and audit.

**Use Case 2: Change documents by transaction and table**

```
OBJECTCLAS = LFA1
TABNAME = LFA1
TCODE = XK02
CHANGE_IND = U
UDATE = last 30 days
```

**Purpose:** Review vendor master (LFA1) changes made via transaction XK02 (Change Vendor) over the last 30 days, limited to update type, for vendor data governance.

**Use Case 3: Key decomposition for key-field change analysis**

```
OBJECTCLAS = (e.g. table-specific object class)
TABNAME = KNA1
CONVERT_KEY = X
LANGU = E
UDATE = last 14 days
```

**Purpose:** Retrieve change documents for customer master (KNA1) with key components decomposed into KEY1–KEY10 and descriptions, in English, for detailed key-field change analysis.

**Use Case 4: Repetitive mode with lookback**

```
REPETITIVE = X
BACKDAYS = 1
OBJECTCLAS = BU_PARTNER
TABNAME = ADRC
```

**Purpose:** Run in repetitive mode with a one-day lookback for standard UDATE range; useful for daily monitoring of address-related change documents when repetitive date range is supplied separately.


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