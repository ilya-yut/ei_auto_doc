# Exception Indicator: Master Data General Change Log - SW_10_06_ANY_MD_CHNG

## General Overview

This Exception Indicator (EI) monitors change document data in SAP to identify changes to master data (e.g. customer address, vendor, or other configurable objects) within a configurable time window. It provides visibility into who changed what, when, and which fields were affected, enabling audit, compliance, and root-cause analysis of master data changes.

This EI serves as an essential control for master data governance and change management by:
- Enabling detection of master data changes that may affect data quality, compliance, or business processes
- Supporting identification of change patterns by object, table, field, user, and transaction for audit and accountability
- Providing visibility into change history and key-field decomposition for root-cause and impact analysis
- Enabling analysis of change concentration by object class, table, and user for governance and training
- Supporting repetitive change monitoring and comparison of old vs new values for exception and reconciliation workflows

This monitoring enables organizations to meet audit and compliance requirements, investigate data quality issues, and hold users accountable for master data changes. The EI is particularly valuable for change-document analysis, master data governance, and exception management.

The EI uses change document header and position data (CDHDR, CDPOS) and related metadata (TCDOB, DD03L) for object class, table, and field descriptions.


## Problem Description

Failure to monitor master data change documents within expected timeframes creates multiple risks across compliance, data quality, and operational management:

**Compliance and Audit Risks**
- Unmonitored change documents can delay or prevent audit evidence of who changed master data and when, affecting regulatory and internal audit requirements
- Change activity in sensitive objects (e.g. customer address, vendor bank details) may indicate unauthorized or inappropriate changes that go unnoticed
- Lack of visibility into change indicators (insert, update, delete) and key-field changes limits ability to reconstruct and explain data history
- Concentrated changes in specific periods or by specific users may signal policy violations or training gaps

**Data Quality and Operational Risks**
- Unidentified mass or repetitive changes can indicate bulk loads, integrations, or errors that affect data quality
- Changes to key fields without monitoring can mask data corruption or incorrect master data updates
- Lack of filtering by object class, table, or field limits ability to focus on high-risk or business-critical changes
- Absence of user and transaction visibility delays accountability and corrective action

**Management Visibility and Decision-Making Risks**
- Lack of change-document monitoring delays executive awareness of master data risk and governance gaps
- Unidentified change patterns limit ability to target training, process improvements, or access controls
- Absence of multi-dimensional analysis (object, table, user, date) limits ability to prioritize follow-up and remediation

## Suggested Resolution

**Immediate Response**
- Review the change documents flagged by the EI to understand the nature and scope (object class, table, field, user, change indicator)
- Verify high-risk or high-volume changes using change document display (e.g. transaction code for change documents) to confirm legitimacy and context
- Check change indicator and old/new values to ensure no erroneous or unauthorized updates
- Identify business context: planned project, bulk load, single-user correction, or possible violation

**System Assessment**
- Analyze time window and object/table scope to understand which factors drive the exception pattern
- Review historical trends by comparing current change volumes to prior periods using the same criteria
- Examine user and transaction distribution to identify training or access issues
- Assess object class and table distribution to determine if exceptions are object- or table-specific
- Validate the reference date and repetitive vs one-time window to ensure the monitoring scope is appropriate

**Corrective Actions**
- If unauthorized or erroneous changes are identified, initiate correction or rollback procedures per change document and master data policy
- For legitimate changes requiring approval, escalate to data owners and governance for validation
- Update user access or authorization (SU01, role changes) if inappropriate changes are detected
- Document change patterns and business justifications for audit and governance reporting
- Establish recurring EI execution to provide continuous visibility into master data changes
- Configure alert routing to data owners and governance stakeholders based on organizational responsibility


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

Current or active change number; used for navigation or display. Filter or display by change document number.

**BACKDAYS** (Backdays):

Number of days to look back from today for the primary date window (UDATE). When no date range is supplied, the EI builds the selection using the system date minus BACKDAYS. Used to limit the change document date window.

**BACKDAYS and REPETITIVE Connection:**

When REPETITIVE is not set, BACKDAYS defines the window for UDATE and the datum range. When REPETITIVE is set, BACKDAYS defines the window for UDATE; REPET_BACKDAYS defines the window for the repetitive date (UDATE_REPET). Set both when using repetitive change monitoring.

**CHANGENR** (Document Number):

Change document number (CDHDR-CDHDR, CDPOS-CHANGENR). Filters change document headers and positions by change number. Supports range selection.

**CHANGE_IND** (Appl. object change-Header Lvl):

Change indicator at header level (CDHDR-CHANGE_IND). Filters by header change type (e.g. insert, update, delete). CHANGE_IND_DESC provides the text.

**CHANGE_IND_DESC** (Domain name):

Description for CHANGE_IND from domain CDCHNGIND. Populated by the EI for display. Filter by CHANGE_IND.

**CHNGIND** (Change Indicator-Row lvl):

Change indicator at position level (CDPOS-CHNGIND). Filters by row change type. CHNGIND_DESC provides the text.

**CHNGIND_DESC** (Domain name):

Description for CHNGIND from domain CDCHNGIND. Populated by the EI for display. Filter by CHNGIND.

**CONVERT_KEY** ('X' - Decompose Key Field):

When set (e.g. 'X'), the EI decomposes the table key into KEY1–KEY10, KEYx_V (values), and KEYx_DS (descriptions). Use with TABNAME when you need readable key components. In repetitive mode, key conversion is applied automatically.

**CONVERT_KEY and TABNAME Connection:**

TABNAME restricts change positions to the specified table(s). CONVERT_KEY controls whether key fields of that table are decomposed into KEY1–KEY10 and KEYx_V, KEYx_DS. Set TABNAME first; set CONVERT_KEY when key decomposition is required.

**CUKY_NEW** (CUKY):

New currency value (CDPOS). Output for value comparison. Use for display or currency-based analysis.

**CUKY_OLD** (CUKY):

Old currency value (CDPOS). Output for value comparison. Use for display or currency-based analysis.

**FIELD_DESC** (Short Description):

Short description for the changed field (FNAME). Populated by the EI from DD04/AS4TEXT. Use for display; filter by FNAME.

**FNAME** (Field Name):

Field name (CDPOS-FNAME). Filters change document positions by the field that was changed. Supports range or wildcard.

**KEY1 - KEY10** (Field Name 1 - Field Name 10):

Key field names (1–10) for the table (TABNAME). Populated when CONVERT_KEY is used; identifies the key components of the changed object. Use to filter or display key structure; all 10 share the same semantic (key field name for position 1–10).

**KEY10_DS** (Short Description):

Short description for KEY10. Populated when CONVERT_KEY is used. Use for display.

**KEY10_V** (Short Description):

Key value for position 10. Populated when CONVERT_KEY is used. Use for display or filtering.

**KEY1_DS - KEY10_DS** (Short Description 1 - 10):

Short descriptions for key fields 1–10. Populated when CONVERT_KEY is used. Use for display; all 10 share the same semantic.

**KEY1_V - KEY10_V** (Short Description 1 - 10):

Key values for positions 1–10. Populated when CONVERT_KEY is used. Use for display or filtering; all 10 share the same semantic.

**KEY2** (Field Name):

Key field name for position 2. Part of KEY1–KEY10 series; see KEY1 - KEY10.

**KEY2_DS** (Short Description):

Short description for KEY2. Part of KEY1_DS–KEY10_DS series.

**KEY2_V** (Short Description):

Key value for position 2. Part of KEY1_V–KEY10_V series.

**KEY3** through **KEY9** (Field Name):

Key field names for positions 3–9. Part of KEY1–KEY10 series; see KEY1 - KEY10. Same for KEY3_DS/KEY3_V through KEY9_DS/KEY9_V.

**LANGU** (Language for texts):

Language for field and table descriptions. Drives TEXT_* and *_DESC resolution.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set (e.g. 'X'), date/time handling uses UTC. Use when working across time zones.

**NAME_FIRST** (First Name):

First name of the user (USERNAME). Populated by the EI from user master. Use for display.

**NAME_LAST** (Last Name):

Last name of the user. Populated by the EI from user master. Use for display.

**NAME_TEXT** (Full Name):

Full name of the user. Populated by the EI. Use for display; filter by USERNAME.

**OBJECTCLAS** (Change doc. object):

Change document object class (CDHDR-OBJECTCLAS). Filters by object class (e.g. from TCDOB). Use to focus on specific object types (e.g. customer, vendor, material).

**OBJECTID** (Customer):

Object value (CDHDR-OBJECTID). Filters by the object instance (e.g. customer number, vendor number). OBJECT_DESC is populated for display when applicable (e.g. vendor name for LFA1).

**OBJECT_DESC** (Name):

Object description (e.g. vendor name for LFA1). Populated by the EI when OBJECTID and table/object allow. Use for display; filter by OBJECTID.

**PLANCHNGNR** (Change number):

Planned change number. Use for display or filtering when applicable.

**REPETITIVE** ('X' - Repetitive Change):

When set (e.g. 'X'), the EI uses repetitive change logic: REPET_BACKDAYS and UDATE_REPET define the repetitive window; BACKDAYS and UDATE define the primary window. Use for scenarios where change documents are evaluated in a repetitive time frame.

**REPETITIVE Options:**

- **X**: Repetitive change mode; use REPET_BACKDAYS and UDATE_REPET for the repetitive window.
- Initial (empty): One-time window; UDATE is mapped from the datum range, BACKDAYS only.

**TABKEY** (Table Key):

Table key (CDPOS-TABKEY). Output; raw key value. Use for display or technical analysis.

**TABNAME** (Table Name):

Table name (CDPOS-TABNAME). Filters change document positions by the table that was changed. Use to focus on specific master data tables (e.g. LFA1, ADRC, KNA1). TAB_DESC provides the table description.

**TAB_DESC** (Short Description):

Short description for TABNAME. Populated by the EI. Use for display; filter by TABNAME.

**TCODE** (Transaction Code):

Transaction code (CDHDR-TCODE). Filters by the transaction that performed the change. Use to focus on specific transactions (e.g. XD02, MK02).

**TEXT_CASE** (Text flag):

Text flag. Use for display or filtering when applicable.

**UDATE** (Date):

Change date (CDHDR-UDATE, CDPOS-UDATE). Filters by the date of the change. Used as the primary date when REPETITIVE is not set; when REPETITIVE is set, filtered by BACKDAYS.

**UNIT_NEW** (Unit):

New unit value (CDPOS). Output for value comparison. Use for display.

**UNIT_OLD** (Unit):

Old unit value (CDPOS). Output for value comparison. Use for display.

**USERNAME** (User):

User who made the change (CDHDR-USERNAME). Filters by user. NAME_FIRST, NAME_LAST, NAME_TEXT are populated by the EI for display.

**UTIME** (Time):

Change time (CDHDR-UTIME). Filters or displays time of change. Supports range or single value.

**VALUE_NEW** (New value):

New value (CDPOS-VALUE_NEW). Output; the new field value after the change. Use for display or comparison.

**VALUE_OLD** (Old value):

Old value (CDPOS-VALUE_OLD). Output; the old field value before the change. Use for display or comparison.

**WAS_PLANND** (Created from Planned):

Indicator for planned change. Use for display or filtering when applicable.


### Parameter Relationships

**Time and Repetitive Parameters:**
- BACKDAYS defines how many days back from today the selection window starts for the primary date (UDATE). When REPETITIVE is set, REPET_BACKDAYS defines the window for the repetitive date (UDATE_REPET), and UDATE is filtered by BACKDAYS. Set BACKDAYS and, when using repetitive mode, REPET_BACKDAYS to define the monitoring window.
- REPETITIVE and REPET_BACKDAYS work together: when REPETITIVE is set, the EI uses REPET_BACKDAYS for the datum window and maps UDATE_REPET to that window; UDATE is then filtered by BACKDAYS. Use both when monitoring repetitive change scenarios.

**Key and Table Parameters:**
- TABNAME identifies the table(s) to which change document positions are restricted. KEY1 through KEY10 (and KEY1_V through KEY10_V, KEY1_DS through KEY10_DS) hold the key field names and values for that table when CONVERT_KEY is used. Set TABNAME to focus on specific tables; use CONVERT_KEY to decompose and display key components (KEY1–KEY10, KEYx_V, KEYx_DS).
- CONVERT_KEY controls whether key fields are decomposed into KEY1–KEY10, KEYx_V, and KEYx_DS. Use with TABNAME when you need readable key components in the output.

**Object and Change Parameters:**
- OBJECTCLAS and OBJECTID work together: OBJECTCLAS is the change document object class (e.g. from TCDOB); OBJECTID is the object value (e.g. customer number, vendor number). Together they filter which change document headers are selected. OBJECT_DESC is populated for display (e.g. vendor name when table is LFA1).
- CHANGE_IND (header level) and CHNGIND (row level) filter by change type. Use both to focus on insert, update, or delete as needed.


### Default Values

- **BACKDAYS** — Default: `1` (when not supplied and REPETITIVE is not set; when REPETITIVE is set, BACKDAYS is set to 1 if initial).
- **REPET_BACKDAYS** — Default: initial (0); when REPETITIVE is set, the repetitive date window uses this value; when not supplied, the window is derived from the datum range.
- **REPETITIVE** — Default: initial (empty); standard one-time change window when not supplied. When set, repetitive mode uses REPET_BACKDAYS and UDATE_REPET for the window and BACKDAYS for UDATE.
- **LANGU** — Default: initial (system language when not supplied).
- **MANAGE_IN_UTC** — Default: initial (empty); standard time handling when not supplied.

**Note:** When REPETITIVE is set and R_DATUM or R_UDATE is initial, the EI builds the date range from REPET_BACKDAYS or BACKDAYS respectively. Parameters that are not supplied and are used when initial effectively default to initial.

### Practical Configuration Examples

**Use Case 1: Customer address changes in the last 30 days**
```
BACKDAYS = 30
TABNAME = ADRC
OBJECTCLAS = (from TCDOB for address)
FNAME = *
```
**Purpose:** Focus on address-related change documents (e.g. table ADRC) in the last 30 days. Supports "Customer's address details were changed" and similar address-change monitoring.

**Use Case 2: Header-only change document check**
```
BACKDAYS = 7
HEADER_ONLY = X
OBJECTCLAS = *
CHANGE_IND = U
```
**Purpose:** Quick check for change document headers (no position detail) in the last 7 days with change indicator Update. Use to confirm that changes exist before drilling into positions.

**Use Case 3: Repetitive change monitoring with key decomposition**
```
REPETITIVE = X
REPET_BACKDAYS = 14
BACKDAYS = 7
CONVERT_KEY = X
TABNAME = LFA1
```
**Purpose:** Monitor repetitive change scenario: repetitive window 14 days, primary UDATE window 7 days. Decompose key fields for table LFA1 (vendor) so KEY1–KEY10 and KEYx_V show key components. Supports vendor master change analysis.

**Use Case 4: Change by user and transaction**
```
BACKDAYS = 60
USERNAME = USER01
TCODE = XD02
CHNGIND = U
OBJECTCLAS = *
```
**Purpose:** Focus on changes in the last 60 days by a specific user and transaction (e.g. XD02 – Change Customer), with row-level change indicator Update. Use for user accountability and audit.


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
FUNCTION /skn/f_sw_10_06_md_chng_log.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_MD_CHNG_LOG
*"----------------------------------------------------------------------
*** 15.03.21++
  TYPES: BEGIN OF ty_key_field,
           tabname   TYPE tabname,
           fieldname TYPE fieldname,
         END OF ty_key_field,
         tt_key_field TYPE STANDARD TABLE OF ty_key_field.
*** 15.03.21++
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  data_single: manage_in_utc       char1 ,
               langu               langu,
               backdays            int4,
               repet_backdays      int4,        " 31.08.21++
               convert_key         char1,
               header_only         char1,
               repetitive          char1.       " 15.03.21++
  data_multy:   objectclas        cdobjectcl,
                objectid          cdobjectv,
                changenr          cdchangenr,                  " 15.03.21++
                username          cdusername,
                tcode             cdtcode,
                change_ind        cdchngindh,
                tabname           tabname,
                fname             fieldname,
                chngind           cdchngind,
                udate             cddatum,
                udate_repet       cddatum,                     " 31.08.21++
                duration_d        /skn/e_sw_duration_d,
                datum             sydatum,                    " Paased by SW Online Monitor
                fname_rep         fieldname.                   " 15.03.21++
*** 15.03.21++
  TYPES: BEGIN OF ty_lfbk_key,
           lifnr TYPE lifnr,
           banks TYPE banks,
           bankl TYPE bankl,
           bankn TYPE bankn,
         END OF ty_lfbk_key,
         tt_lfbk_key TYPE STANDARD TABLE OF ty_lfbk_key.
*** 15.03.21++
  DATA: lv_fieldname TYPE fieldname,
        lv_shift     TYPE ddleng,
        lv_leng      TYPE ddleng.
  DATA: lv_tabkey_len TYPE i VALUE '70',    "!!!
        lv_ilen TYPE i.
  FIELD-SYMBOLS: <fs_old> TYPE any,
                 <fs_new> TYPE any.
  DATA : fld TYPE fieldname,
         ifld TYPE i,
         ctmp(2) TYPE c.
  DEFINE populate_key_field .
    " &1 - Field Index
    clear lv_FIELDNAME.
    perform GET_KEY_FIELD  using    LS_DATA-TABNAME
                                    &1
                           changing lv_FIELDNAME
                                    lv_SHIFT
                                    lv_LENG.
    if lv_FIELDNAME is not initial.
      LS_DATA-KEY&1   =  lv_FIELDNAME.
      lv_ilen = lv_SHIFT + lv_LENG.
      if lv_ilen <= lv_TABKEY_len.
        LS_DATA-KEY&1_V =  LS_DATA-TABKEY+lv_SHIFT(lv_LENG).
      endif.
      perform get_FIELD_DESC using   LS_DATA-TABNAME
                                     lv_FIELDNAME
                                     lv_LANGU
                            changing LS_DATA-KEY&1_DS.
    endif.
  END-OF-DEFINITION .
  DEFINE populate_key_fields .
    populate_key_field 1.
    populate_key_field 2.
    populate_key_field 3.
    populate_key_field 4.
    populate_key_field 5.
    populate_key_field 6.
    populate_key_field 7.
    populate_key_field 8.
    populate_key_field 9.
    populate_key_field 10.
  END-OF-DEFINITION .
  DEFINE convert_key_fields .
    refresh lt_KEY_CONV..
    clear: ls_KEY_CONV,
           ls_KEY_old,
           ls_KEY_new.
    loop at lt_DATA_KEY into LS_DATA .
      if LS_DATA-CHNGIND = 'E' .
        ls_KEY_old = LS_DATA.
      elseif LS_DATA-CHNGIND = 'I' .
        ls_KEY_new = LS_DATA.
      endif.
    endloop.
    clear ifld.
    do 10 times.
      clear ls_KEY_CONV.
      add 1 to ifld.
      ctmp = ifld.
      concatenate 'KEY' ctmp '_V'  into fld.
      ASSIGN COMPONENT fld of STRUCTURE ls_KEY_old to <fs_old>.
      ASSIGN COMPONENT fld of STRUCTURE ls_KEY_new to <fs_new>.
      if <fs_old> <> <fs_new>.
        "--- Add Data
        move-CORRESPONDING ls_KEY_new to ls_KEY_CONV.
        ls_KEY_CONV-CHNGIND = 'U'.
        ls_KEY_CONV-VALUE_NEW = <fs_new>.
        ls_KEY_CONV-VALUE_OLD = <fs_old>.
        concatenate 'KEY' ctmp  into fld.
         ASSIGN COMPONENT fld of STRUCTURE ls_KEY_old to <fs_old>.
          ls_KEY_CONV-FNAME = <fs_old>.
        append ls_KEY_CONV to lt_KEY_CONV.
      endif.
    enddo.
  END-OF-DEFINITION .
  "--- Run Cloud Mode -----
  data_single: sw_dest rfcdest.             .
  select_single: sw_dest.
  IF lv_sw_dest IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_MD_CHNG_LOG'
      IMPORTING
        is_alert = is_alert
      TABLES
        t_select = t_select
        t_data   = t_data.
  ENDIF.
  CHECK lv_sw_dest IS INITIAL.
  "--- Run Cloud Mode -----
  select_multy:  objectclas,
                 objectid,
                 changenr,                  " 15.03.21++
                 username,
                 tcode,
                 change_ind,
                 tabname,
                 fname,
                 chngind,
                 udate,
                 udate_repet,                " 31.08.21++
                 duration_d,
                 datum.
  lv_langu = sy-langu.
  select_single: langu,
                 manage_in_utc,
                 backdays,
                 repet_backdays,             " 31.08.21++
                 convert_key,
                 header_only,
                 repetitive.                 " 15.03.21++
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA : date_from TYPE d,
         backdays  TYPE i.
  DATA : sy_tabix LIKE sy-tabix .
  DATA : ls_data LIKE LINE OF t_data.
  DATA : lt_data LIKE TABLE OF ls_data,
         lt_data_tmp LIKE TABLE OF ls_data.
  DATA : lt_data_key LIKE TABLE OF ls_data.
  DATA : ls_key_conv LIKE LINE OF t_data,
         ls_key_old LIKE LINE OF t_data,
         ls_key_new LIKE LINE OF t_data,
         lt_key_conv LIKE TABLE OF ls_key_conv.
  DATA : time_diff TYPE i .
  DATA : sy_datlo LIKE sy-datlo ,
         sy_timlo LIKE sy-timlo .
  DATA: lv_object TYPE cdobjectcl.
  data_multy:   obj_clas cdobjectcl.  "For temporary use
  DATA : domvalue LIKE  dd07v-domvalue_l,
         ddtext LIKE  dd07v-ddtext.
  DATA: ls_del TYPE cdpos,
        lt_del LIKE TABLE OF ls_del,
        is_del_added(1) TYPE c.
*** Begin 15.03.21++
  DATA: lv_tabix       TYPE i,
        lv_num         TYPE char1,
        lv_exist       TYPE boole_d,
        lv_append      TYPE boole_d,
        lv_field       TYPE fieldname,
        lv_len         TYPE i,
        lv_tabkey      TYPE cdtabkey,
        lv_lifnr       TYPE lifnr,
        lv_vendor_desc TYPE name1_gp.
  DATA: ls_data_new   LIKE LINE OF t_data,
        ls_data_old   LIKE LINE OF t_data,
        ls_components TYPE abap_compdescr,
        ls_data_tmp   LIKE LINE OF t_data,
        ls_data_tmp2  LIKE LINE OF t_data,
        ls_key_field  TYPE ty_key_field,
        ls_dd03l      TYPE dd03l,
        ls_inf        TYPE rpy_tabl,
        ls_tab_field  TYPE rpy_main.
  DATA: lt_data_tmp2      LIKE TABLE OF ls_data,
        lt_key_field      TYPE tt_key_field,
        lt_dd03l          TYPE STANDARD TABLE OF dd03l,
        lt_dd03l_pos      TYPE STANDARD TABLE OF dd03l,
        lt_tab_fields     TYPE STANDARD TABLE OF rpy_main,
        lt_tab_fields_tmp TYPE STANDARD TABLE OF rpy_main.
  DATA: lr_str_desc TYPE REF TO cl_abap_structdescr,
        lr_data     TYPE REF TO data.
  FIELD-SYMBOLS: <fs_field_key> TYPE any,
                 <fs_val_key>   TYPE any,
                 <fs_val_key1>  TYPE any,
                 <fs_line>      TYPE any.
*** End 15.03.21++
  DATA: lv_changenr TYPE cdchangenr.
  DATA: is_key_case(1) TYPE c.
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
  IF lv_repetitive EQ 'X'.
    IF r_datum[] IS INITIAL .  " Set default value
      rs_datum-sign   = 'I' .
      rs_datum-option = 'GE'.
      date_from       = sy_datlo - lv_repet_backdays.
      rs_datum-low    = date_from .
      APPEND rs_datum TO r_datum.
    ENDIF .
    IF r_udate_repet[] IS INITIAL.
      r_udate_repet[] = r_datum[].
    ENDIF.
    IF lv_backdays IS INITIAL.
      lv_backdays = 1.
    ENDIF.
    IF r_udate[] IS INITIAL.  " Set default value
      rs_udate-sign   = 'I' .
      rs_udate-option = 'GE'.
      date_from       = sy_datlo - lv_backdays.
      rs_udate-low    = date_from.
      APPEND rs_udate TO r_udate.
    ENDIF.
  ELSE.
    IF r_datum[] IS INITIAL .  " Set default value
      rs_datum-sign   = 'I' .
      rs_datum-option = 'GE'.
      date_from       = sy_datlo - lv_backdays.
      rs_datum-low    = date_from.
      APPEND rs_datum TO r_datum.
    ENDIF.
    r_udate[] = r_datum[].
  ENDIF.
*** End 03.09.21++
  set_sy_time lv_manage_in_utc sy_datlo sy_timlo .
  time_shift sy_datlo sy_timlo . " TIME_SHIFT parameter
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR is_alert .
  REFRESH t_data .
  REFRESH lt_data .
  IF r_objectclas[] IS INITIAL.
    IF r_tabname[] IS NOT INITIAL.
      SELECT object
         FROM tcdob
         INTO lv_object
         WHERE tabname IN r_tabname.
        rs_obj_clas-low    = lv_object.
        rs_obj_clas-option = 'EQ'.
        rs_obj_clas-sign   = 'I'.
        APPEND rs_obj_clas TO r_obj_clas.
      ENDSELECT.
    ENDIF.
  ENDIF.
  LOOP AT r_objectclas INTO rs_objectclas.
    APPEND rs_objectclas TO r_obj_clas.
  ENDLOOP.
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert Data           "
  "-----------------------------------------------
  "--- Check that Object Class is not empty
  IF r_obj_clas[] IS INITIAL.
    EXIT.      "!!!!!!
  ENDIF.
  SELECT *
    FROM cdhdr
    INTO CORRESPONDING FIELDS OF TABLE lt_data
    WHERE objectclas IN r_obj_clas
    AND   objectid   IN r_objectid
    AND   changenr   IN r_changenr           " 15.03.21++
    AND   username   IN r_username
    AND   tcode      IN r_tcode
    AND   change_ind IN r_change_ind
    AND   udate      IN r_udate. "
  IF lv_header_only EQ 'X'.
    t_data[] = lt_data[].
    READ TABLE t_data INTO ls_data INDEX 1.
    CHECK sy-tfill IS NOT INITIAL .
    is_alert = 'X' .
    EXIT.
  ENDIF.
  "-----------------------------------------------
  " 5. Post retrieving manipulations             "
  "-----------------------------------------------
  LOOP AT lt_data INTO ls_data.
    sy_tabix = sy-tabix.
    REFRESH lt_del.
    CLEAR is_del_added.
    CLEAR is_key_case.
    REFRESH lt_data_key.
    lv_changenr = ls_data-changenr.
    REFRESH lt_data_tmp.
    SELECT *
     FROM cdpos
     INTO CORRESPONDING FIELDS OF ls_data "TABLE T_DATA
     WHERE objectclas EQ ls_data-objectclas
     AND   objectid   EQ ls_data-objectid
     AND   changenr   EQ ls_data-changenr
     AND   tabname    IN r_tabname
"          AND FNAME IN R_FNAME
"          AND CHNGIND IN R_CHNGIND
          . "
      APPEND ls_data TO lt_data_tmp.   "  T_DATA.
      IF ls_data-chngind = 'I'.
        IF ls_data-fname = 'KEY'.    " Insert Key
          is_key_case = 'X'.
          populate_key_fields .
          APPEND ls_data TO lt_data_key.
        ENDIF.
      ENDIF.
      IF ls_data-chngind = 'E'.
        IF is_del_added IS INITIAL.
          MOVE-CORRESPONDING ls_data TO ls_del.
          APPEND ls_del  TO lt_del.
          ls_data-fname = 'KEY'.
          CLEAR ls_data-value_old.
          APPEND ls_data TO lt_data_tmp.  " T_DATA.
          populate_key_fields .
          APPEND ls_data TO lt_data_key.
          is_del_added = 'X'.
        ENDIF.
      ENDIF.
    ENDSELECT.
*** 15.03.21++
    IF lv_repetitive EQ abap_true.
      lv_convert_key = 'X'.
    ENDIF.
*** 15.03.21++
    "--- Convert KEY Field (I/E)
    IF lv_convert_key IS NOT INITIAL. " to convert KEY
*        refresh lt_DATA_KEY.
*        loop at T_DATA into LS_DATA where FNAME = 'KEY'.
*          populate_key_fields .
*          append LS_DATA to lt_DATA_KEY.
*        endloop.
      "--- Convert KEY Field (I/E)
      convert_key_fields.
      LOOP AT lt_key_conv INTO ls_key_conv.
        MOVE-CORRESPONDING ls_key_conv TO ls_data.
        APPEND ls_data TO lt_data_tmp.  "T_DATA.
      ENDLOOP.
      " Delete KEY records
      IF is_key_case IS NOT INITIAL.
        DELETE lt_data_tmp WHERE fname   = 'KEY' AND changenr = lv_changenr.
        DELETE lt_data_tmp WHERE chngind = 'E'   AND changenr = lv_changenr.
      ENDIF.
    ENDIF.
*** Begin 15.03.21++
    IF lv_repetitive EQ abap_true.
      IF ls_data-udate IN r_udate.                   " 31.08.21++
        SELECT *
          FROM dd03l
          INTO TABLE lt_dd03l
          WHERE tabname   IN r_tabname
          AND   as4local  EQ 'A'.
        SORT lt_dd03l BY tabname fieldname.
        CALL FUNCTION 'RPY_TABLE_READ_SHORT'
          EXPORTING
            activation_type  = 'A'
            language         = lv_langu
            table_name       = ls_data-tabname
          IMPORTING
            tabl_inf         = ls_inf
          TABLES
            tabl_fields      = lt_tab_fields_tmp
          EXCEPTIONS
            cancelled        = 1
            not_found        = 2
            permission_error = 3
            illegal_type     = 4
            OTHERS           = 5.
        IF sy-subrc IS INITIAL AND lt_tab_fields_tmp IS NOT INITIAL.
          DELETE lt_tab_fields_tmp WHERE keyflag NE 'X'.
          LOOP AT lt_tab_fields_tmp INTO ls_tab_field.
            READ TABLE lt_tab_fields WITH KEY tablname  = ls_tab_field-tablname
                                              fieldname = ls_tab_field-fieldname
                                              TRANSPORTING NO FIELDS.
            IF sy-subrc IS NOT INITIAL.
              APPEND ls_tab_field TO lt_tab_fields.
            ENDIF.
          ENDLOOP.
        ELSE.
          lt_dd03l_pos = lt_dd03l.
          SORT lt_dd03l_pos BY position.
          LOOP AT lt_dd03l_pos INTO ls_dd03l WHERE keyflag EQ abap_true.
            CLEAR: ls_tab_field.
            ls_tab_field-tablname   = ls_dd03l-tabname.
            ls_tab_field-fieldname  = ls_dd03l-fieldname.
            ls_tab_field-dtelname   = ls_dd03l-rollname.
            ls_tab_field-checktable = ls_dd03l-checktable.
            ls_tab_field-keyflag    = ls_dd03l-keyflag.
            APPEND ls_tab_field TO lt_tab_fields.
          ENDLOOP.
        ENDIF.
        CLEAR: lt_tab_fields_tmp.
      ENDIF.
    ENDIF.
*** End 15.03.21++
    LOOP AT lt_data_tmp INTO ls_data.
*      APPEND ls_data TO t_data.          " 15.03.21--
*** Begin 15.03.21++
      IF lv_repetitive EQ abap_true.
        CHECK ls_data-udate IN r_udate.      " 31.08.21++
        CLEAR: lv_tabkey, lv_len, lv_leng.
        IF lt_data_tmp2 IS INITIAL.
          APPEND ls_data TO lt_data_tmp2.
        ELSE.
          LOOP AT lt_tab_fields INTO ls_tab_field WHERE tablname EQ ls_data-tabname.
            lv_tabix = sy-tabix.
            lv_num   = lv_tabix.
            CONDENSE lv_num.
            CONCATENATE 'KEY' lv_num '_V' INTO lv_field.
            CREATE DATA lr_data TYPE (ls_tab_field-dtelname).
            IF lr_data IS BOUND.
              ASSIGN lr_data->* TO <fs_val_key>.
              READ TABLE lt_dd03l INTO ls_dd03l WITH KEY tabname   = ls_tab_field-tablname
                                                         fieldname = ls_tab_field-fieldname
                                                         BINARY SEARCH.
            ENDIF.
            ASSIGN COMPONENT lv_field OF STRUCTURE ls_data TO <fs_val_key>.
            IF sy-subrc IS INITIAL AND <fs_val_key> IS ASSIGNED.
              lv_tabkey+lv_len(ls_dd03l-leng) = <fs_val_key>+0(ls_dd03l-leng).
              lv_len                          = lv_len + ls_dd03l-leng.
            ENDIF.
            IF NOT ls_tab_field-fieldname IN r_fname[].
              READ TABLE lt_key_field WITH KEY fieldname = ls_tab_field-fieldname
                                      TRANSPORTING NO FIELDS.
              IF sy-subrc IS NOT INITIAL.
                ls_key_field-tabname   = ls_tab_field-tablname.
                ls_key_field-fieldname = ls_tab_field-fieldname.
                APPEND ls_key_field TO lt_key_field.
              ENDIF.
            ENDIF.
          ENDLOOP.
          LOOP AT lt_data_tmp2 INTO ls_data_tmp.
* If tabkeys are equal,
            IF ls_data_tmp-value_old EQ ls_data-value_new.
              "ls_data_tmp-tabkey EQ lv_tabkey AND lv_exist IS INITIAL.
              APPEND ls_data TO lt_data_tmp2.
              lv_exist = 'X'.
            ELSE.
* Check if object have the same key fields, except the field that being checked
              lv_append = 'X'.
              LOOP AT lt_key_field INTO ls_key_field.
                lv_tabix = sy-tabix.
                lv_num   = lv_tabix.
                CONDENSE lv_num.
                CONCATENATE 'KEY' lv_num '_V' INTO lv_field.
                ASSIGN COMPONENT lv_field OF STRUCTURE ls_data     TO <fs_val_key>.
                ASSIGN COMPONENT lv_field OF STRUCTURE ls_data_tmp TO <fs_val_key1>.
                IF <fs_val_key> IS ASSIGNED AND <fs_val_key1> IS ASSIGNED.
                  IF <fs_val_key> NE <fs_val_key1>.
                    IF lv_exist EQ abap_true.
                      APPEND LINES OF lt_data_tmp2 TO t_data.
                    ENDIF.
                    CLEAR: lt_key_field, lt_data_tmp2.
                    CLEAR: lv_append, lv_exist.
* Append line of a new object
                    APPEND ls_data TO lt_data_tmp2.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDLOOP.
* Append the line with the same key field to temporary table
              IF lv_append EQ abap_true.
                READ TABLE lt_data_tmp2 WITH KEY udate  = ls_data-udate
                                                 utime  = ls_data-utime
                                                 tabkey = lv_tabkey
                  TRANSPORTING NO FIELDS.
                IF sy-subrc IS NOT INITIAL.
                  APPEND ls_data TO lt_data_tmp2.
                ENDIF.
                CLEAR: lv_append.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.
        APPEND ls_data TO t_data.
      ENDIF.
*** End 15.03.21++
    ENDLOOP.
  ENDLOOP.
*** Begin 15.03.21++
  IF lv_exist EQ 'X'.
    APPEND LINES OF lt_data_tmp2 TO t_data.
  ENDIF.
*** End 15.03.21++
  "-----------------------------------------------
  " 6. Post retrieving filtering                 "
  "-----------------------------------------------
  DELETE t_data WHERE objectclas NOT IN r_objectclas.
  DELETE t_data WHERE change_ind NOT IN r_change_ind.
  DELETE t_data WHERE chngind NOT IN r_chngind.
  DELETE t_data WHERE fname NOT IN r_fname.
  LOOP AT t_data INTO ls_data.
    sy_tabix = sy-tabix .
    PERFORM get_field_desc USING    ls_data-tabname
                                    ls_data-fname
                                    lv_langu
                           CHANGING ls_data-field_desc.
    PERFORM get_tab_desc USING ls_data-tabname
                               lv_langu
                         CHANGING ls_data-tab_desc.
**** 10/22++
    IF ls_data-tabname EQ 'LFA1' AND
       ls_data-objectid IS NOT INITIAL.
      lv_lifnr = ls_data-objectid.
**    "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          lifnr        = lv_lifnr
        IMPORTING
          vendor_desc  = lv_vendor_desc
        EXCEPTIONS
          wrong_vendor = 1
          OTHERS       = 2.
      IF sy-subrc IS INITIAL.
        ls_data-object_desc = lv_vendor_desc.
        CLEAR: lv_vendor_desc.
      ENDIF.
    ENDIF.
**** 10/22++
    domvalue = ls_data-change_ind.
    CLEAR ddtext.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        i_domname  = 'CDCHNGIND'
        i_domvalue = domvalue
        langu      = lv_langu
      IMPORTING
        e_ddtext   = ddtext
      EXCEPTIONS
        not_exist  = 1
        OTHERS     = 2.
    IF sy-subrc = 0.
      ls_data-change_ind_desc = ddtext.
    ENDIF.
    domvalue = ls_data-chngind.
    CLEAR ddtext.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        i_domname  = 'CDCHNGIND'
        i_domvalue = domvalue
        langu      = lv_langu
      IMPORTING
        e_ddtext   = ddtext
      EXCEPTIONS
        not_exist  = 1
        OTHERS     = 2.
    IF sy-subrc = 0.
      ls_data-chngind_desc = ddtext.
    ENDIF.
    MODIFY t_data FROM ls_data INDEX sy_tabix.
  ENDLOOP.
  DESCRIBE FIELD ls_data-tabkey LENGTH lv_tabkey_len IN CHARACTER MODE.
  "--- Poplate Key Components
  LOOP AT t_data INTO ls_data.
    sy_tabix = sy-tabix .
    populate_key_fields .
    CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES'
      EXPORTING
        bname      = ls_data-username
      IMPORTING
        name_first = ls_data-name_first
        name_last  = ls_data-name_last
        name_text  = ls_data-name_text
*       WA_ADRP    =
      EXCEPTIONS
        no_data    = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    MODIFY t_data FROM ls_data INDEX sy_tabix.
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
  READ TABLE t_data INTO ls_data INDEX 1.
  CHECK sy-tfill IS NOT INITIAL .
  is_alert = 'X' .
ENDFUNCTION.
```