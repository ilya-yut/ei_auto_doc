# Exception Indicator: PT: Check Payment Terms details change log - SW_10_06_PT_CHNG_LOG

## General Overview

This Exception Indicator (EI) monitors change documents for customer payment terms to identify when payment terms (ZTERM) were changed in customer company code (KNB1) or customer sales (KNVV) master data. It provides visibility into who changed payment terms, when, and in which transaction, supporting audit, credit policy, and data governance.

This EI serves as an essential control for credit management and master data governance by:
- Enabling detection of payment term changes that may affect dunning, cash flow, or contract terms
- Supporting identification of changes by customer, company code, sales organization, distribution channel, and division for accountability and audit trails
- Providing visibility into change history by time window for period-based review and exception management
- Enabling analysis of key field decompositions and before/after values (ZTERM_OLD, ZTERM_NEW) for root-cause and impact analysis
- Supporting compliance and credit stewardship by surfacing payment term changes that require review or remediation

Monitoring payment term changes helps organizations track master data evolution, verify that sensitive changes are authorized, and resolve data quality or consistency issues. The EI is particularly valuable for month-end close, audit preparation, and credit master data governance.

The EI uses the master data change log (CDHDR, CDPOS) and filters for KNB1 and KNVV payment term changes; it enriches results with customer name, company code name, and sales organization text.


## Problem Description

Failure to monitor customer payment term changes creates multiple risks across credit management, operational control, and compliance:

**Financial and Reporting Issues**
- Unreviewed payment term changes can lead to incorrect dunning runs, disputed notices, or cash flow misalignment with contract terms
- Changes to payment terms in company code or sales area may affect collections and credit exposure without timely visibility
- Lack of a clear change trail complicates period-end reconciliation and audit evidence collection

**Operational and Control Risks**
- Unauthorized or erroneous payment term changes can propagate to dunning and payment runs and cause operational failures
- Inability to filter change documents by customer, company code, sales org, or user limits effective segregation of duties and exception handling
- Missing visibility into repetitive or bulk changes increases the risk of undetected data quality degradation

**Management Visibility and Decision-Making Risks**
- Absence of payment term change monitoring delays management awareness of sensitive or high-impact master data changes
- Lack of time-window and organizational analysis limits the ability to prioritize reviews and allocate stewardship resources
- Inadequate change history visibility hinders root-cause analysis and corrective action when dunning or payment issues are reported

## Suggested Resolution

**Immediate Response**
- Review the change documents flagged by the EI to confirm the scope and nature of the changes (customer, company code, sales org, ZTERM old/new)
- Verify high-impact or sensitive payment term changes using the relevant transaction (e.g. XD02 for customer company code, VD02 for customer sales) to confirm legitimacy
- Check change type (insert, update, delete) and before/after values to assess impact on dunning and collections
- Identify business context: planned maintenance, bulk upload, or potential unauthorized change

**System Assessment**
- Analyze the time window and object/table filters used for the run to ensure the monitoring scope matches the control objective
- Compare change volumes and patterns to prior periods to detect unusual activity or concentration by user or transaction
- Review table (KNB1, KNVV) and organizational filters to ensure the EI is focused on the right master data objects
- Validate that key decomposition and display options align with the intended use (audit list vs. detail analysis)

**Corrective Actions**
- If unauthorized or erroneous changes are found, correct payment terms through the appropriate transaction and document the correction
- Escalate repeated or suspicious change patterns to data owners and access management for review of authorizations
- Update monitoring parameters (e.g. lookback period, object class, table name) to align with governance and audit requirements
- Document change-review outcomes and establish recurring EI runs to maintain continuous visibility into payment term changes


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document Number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 3 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 4 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 5 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 6 | CHANGE_IND | Appl. object change | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 7 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 8 | CHNGIND | Change Indicator | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 9 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 10 | CONVERT_KEY | 'X' - Decompose Key Field |  | 0 | 0 |  |  |
| 11 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 12 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 13 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 14 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 15 | KEY1 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 16 | KEY10 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 17 | KEY10_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 18 | KEY10_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 19 | KEY1_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 20 | KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 21 | KEY2 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 22 | KEY2_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 23 | KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 24 | KEY3 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 25 | KEY3_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 26 | KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 27 | KEY4 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 28 | KEY4_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 29 | KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 30 | KEY5 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 31 | KEY5_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 32 | KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 33 | KEY6 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 34 | KEY6_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 35 | KEY6_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 36 | KEY7 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 37 | KEY7_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 38 | KEY7_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 39 | KEY8 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 40 | KEY8_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 41 | KEY8_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 42 | KEY9 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 43 | KEY9_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 44 | KEY9_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 45 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 46 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 47 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 48 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 49 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 50 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 51 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 52 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 53 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 54 | REPETITIVE | Repetitive Change | CHAR | 1 | 0 | /SKN/E_REPEAT | XFLAG |
| 55 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 56 | SPRAS | Language Key | LANG | 1 | 0 | LANGU | SPRAS |
| 57 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 58 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 59 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 60 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 61 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 62 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 63 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 64 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 65 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 66 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 67 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 68 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 69 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 70 | VTEXT | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 71 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 72 | WAS_PLANND | Created from Planned | CHAR | 1 | 0 | CD_PLANNED | XFLAG |
| 73 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
| 74 | ZTERM_NEW | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
| 75 | ZTERM_OLD | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 75 parameters listed in the Parameters Reference Table above.

**ACT_CHNGNO** (Document Number):

Document number of the change document created by the change. Identifies the follow-up change document when applicable; used for traceability and linking related change records.

**BACKDAYS** (Days Back):

Number of days to look back from the current date when building the default monitoring window. When no explicit date range is supplied, the EI uses today minus this value as the start of the window for change document selection.

**BUKRS** (Company Code):

Company code. Restricts which change document positions are included when the changed table is KNB1; also appears in the result and drives enrichment (company name).

**BUTXT** (Company Name):

Name of the company code or company. Populated from the company code master; used for display in the result.

**CHANGENR** (Document Number):

Change document number. Restricts which change documents are selected; used to focus on specific change events or a range of change numbers.

**CHANGE_IND** (Appl. object change):

Application object change type at header level (e.g. update, insert, delete). Restricts which change document headers are included based on the type of change recorded for the object.

**CHANGE_IND Options:**
- **U**: Update.
- **I**: Insert.
- **E**: Delete (end-of-life).
- **D**: Other change types per domain CDCHNGIND.

**CHANGE_IND_DESC** (Domain name):

Short description of the change type at header level. Populated from the domain for the change indicator; used for display in the result set.

**CHNGIND** (Change Indicator):

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

Name of the table field that was changed. Restricts which change document positions are included (e.g. only ZTERM changes); also appears in the result.

**KEY1 - KEY10** (Field Name – Field Name):

Key field names (1–10) for the table (TABNAME) of the change document. When key decomposition is active, these hold the key component field names; populated from the table key structure. Used to identify which key components changed and for display.

**KEY1_DS - KEY10_DS** (Short Description – Short Description):

Short descriptions for the key fields (1–10). Populated from the data dictionary when key decomposition is used; used for display.

**KEY1_V - KEY10_V** (Short Description – Short Description):

Key component values (1–10) when key decomposition is used. Populated from the table key; used for display and comparison of key values.

**KUNNR** (Sold-to party):

Customer number. Restricts which change documents are included (via OBJECTID when table is KNB1 or KNVV); also appears in the result and drives enrichment (customer name).

**NAME1** (Name):

Customer name. Populated from customer master; used for display.

**NAME_FIRST** (First Name):

First name of the user who performed the change. Populated from user master; used for display.

**NAME_LAST** (Last Name):

Last name of the user who performed the change. Populated from user master; used for display.

**NAME_TEXT** (Full Name):

Full name of the user who performed the change. Populated from user master; used for display.

**OBJECTCLAS** (Change doc. object):

Change document object class. Restricts which change documents are selected (e.g. by object type); used by the underlying change log EI.

**OBJECTID** (Object value):

Object ID of the changed object (e.g. customer number). Restricts which change documents are included; used to focus on specific customers. In this EI, OBJECTID is used as KUNNR in the result.

**OBJECT_DESC** (Name):

Description of the object (e.g. customer name). Populated from master data where applicable; used for display.

**PLANCHNGNR** (Change number):

Planned change number when the change document was created from a planned change. Populated from the change document; used for display and linking.

**REPETITIVE** (Repetitive Change):

When set, the EI runs in repetitive mode: it uses a separate lookback for the repetitive window and returns only changes that fall within the repetitive date range. When not set, standard single-run mode is used with one lookback period.

**REPETITIVE Options:**
- **X**: Repetitive change mode; use repetitive lookback and date filter.
- ** ** (space or initial): Standard mode; single lookback.

**SPART** (Division):

Division. Restricts which change document positions are included when the changed table is KNVV; also appears in the result.

**SPRAS** (Language Key):

Language used when retrieving short descriptions for fields, tables, and domain values, and for sales organization text. Determines the language of FIELD_DESC, TAB_DESC, CHANGE_IND_DESC, CHNGIND_DESC, key field descriptions, and VTEXT.

**TABKEY** (Table Key):

Table key of the changed record (concatenated key). Populated from the change document position; when key decomposition is not used, this is the only key representation in the result.

**TABNAME** (Table Name):

Table name of the changed object. Restricts which change document positions are included (e.g. only KNB1, KNVV); also appears in the result and drives key decomposition and field descriptions. This EI focuses on KNB1 and KNVV payment term (ZTERM) changes.

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

New value of the changed field after the change. Populated from the change document position; for ZTERM changes this is reflected in ZTERM_NEW; used for display and comparison.

**VALUE_OLD** (Old value):

Old value of the changed field before the change. Populated from the change document position; for ZTERM changes this is reflected in ZTERM_OLD; used for display and comparison.

**VKORG** (Sales Organization):

Sales organization. Restricts which change document positions are included when the changed table is KNVV; also appears in the result and drives enrichment (sales org text).

**VTEXT** (Name):

Short description of the sales organization. Populated from master data (TVKOT); used for display.

**VTWEG** (Distribution Channel):

Distribution channel. Restricts which change document positions are included when the changed table is KNVV; also appears in the result.

**WAS_PLANND** (Created from Planned):

Indicates whether the change document was created from a planned change. Used for filtering and display; domain XFLAG.

**WAS_PLANND Options:**
- **X**: Created from planned change.
- ** ** (space or initial): Not from planned change.

**ZTERM** (Terms of Payment):

Payment terms key. Restricts which change document positions are included (e.g. only changes to specific payment terms); also used as a filter on the change log result. In this EI, payment term changes from KNB1 or KNVV are surfaced.

**ZTERM_NEW** (Terms of Payment):

Payment terms key after the change. Populated from VALUE_NEW when the changed field is ZTERM; used for display and comparison.

**ZTERM_OLD** (Terms of Payment):

Payment terms key before the change. Populated from VALUE_OLD when the changed field is ZTERM; used for display and comparison.


### Parameter Relationships

**Time and lookback parameters**

- **BACKDAYS** defines the default number of days to look back when no explicit date range is supplied. It is used together with the date range for change document selection (UDATE). In repetitive mode, **REPETITIVE** works with BACKDAYS to define the standard lookback window; the repetitive date filter is applied in addition when repetitive mode is active.
- **UDATE** restricts which change documents are included by change date. The default range for UDATE is derived from BACKDAYS when no range is provided. Also appears in the result.

**Key decomposition and display**

- **CONVERT_KEY** works with **TABNAME** and the KEY1–KEY10, KEY1_DS–KEY10_DS, KEY1_V–KEY10_V output fields. When CONVERT_KEY is set, the EI decomposes the table key (TABKEY) into key component names (KEY1–KEY10), descriptions (KEY1_DS–KEY10_DS), and values (KEY1_V–KEY10_V) based on TABNAME. **SPRAS** is used when populating key and field descriptions.
- **TABNAME** and **FNAME** together define which table and field changes are selected; TABNAME also drives key decomposition when CONVERT_KEY is used. In this EI, TABNAME is KNB1 or KNVV for payment term (ZTERM) changes.

**Payment term and organizational scope**

- **ZTERM** restricts which change document positions are included (e.g. only changes to specific payment terms). **KUNNR** (OBJECTID), **BUKRS**, **VKORG**, **VTWEG**, **SPART**, and **USERNAME** together define the selection scope: customer, company code, sales organization, distribution channel, division, and user. The EI calls the master data change log and then filters the result by these parameters; only changes that match the filters (and table KNB1 or KNVV, field ZTERM) are returned. **ZTERM_OLD** and **ZTERM_NEW** in the result show the before/after payment term values.


### Default Values

- **BACKDAYS** — Default: initial (0); when no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window for change document selection. If BACKDAYS is initial, no default range is built and the caller must supply a date range.
- **SPRAS** — Default: `E` (English) in the code when not supplied; used for field, table, and sales organization description texts.

**Note:** When no explicit date range is supplied for the change log, the default range is derived from the lookback period (BACKDAYS). The underlying master data change log EI uses this range for UDATE.

### Practical Configuration Examples

**Use Case 1: Payment term changes in the last 30 days (all customers)**

```
BACKDAYS = 30
ZTERM = (optional; leave empty for all payment term changes)
```

**Purpose:** Find all customer payment term changes (KNB1 or KNVV) in the last 30 days for periodic governance review.

**Use Case 2: Payment term changes by company code and sales org**

```
BACKDAYS = 7
BUKRS = 1000, 2000
VKORG = 1000, 2000
VTWEG = 10
SPART = 00
```

**Purpose:** Focus on payment term changes in specific company codes and sales organizations over the last week, for segment-based audit or cleanup.

**Use Case 3: Specific customer and payment term**

```
OBJECTID = 0000100001
ZTERM = 0001, 0002
BACKDAYS = 90
```

**Purpose:** Review payment term changes for a single customer and specific terms over the last 90 days, for dispute or audit resolution.

**Use Case 4: Changes by user and transaction**

```
USERNAME = (user IDs to review)
BACKDAYS = 14
TABNAME = KNB1, KNVV
```

**Purpose:** Review payment term changes in KNB1 and KNVV by specific users over the last two weeks, for segregation of duties and accountability.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_PT_CHNG_LOG | .INCLUDE |  |  |  |
| /SKN/S_SW_10_06_PT_CHNG_LOG | ACT_CHNGNO | Change number of the document created by this change | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_06_PT_CHNG_LOG | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_PT_CHNG_LOG | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_06_PT_CHNG_LOG | CHANGE_IND | Application object change type (U, I, E, D) | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_06_PT_CHNG_LOG | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | CHNGIND | Change Type (U, I, S, D) | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_06_PT_CHNG_LOG | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | CUKY_NEW | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_PT_CHNG_LOG | CUKY_OLD | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_PT_CHNG_LOG | FIELD_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY10_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY10_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY1_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY2_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY3_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY4_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY5_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY6_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY6_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY7_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY7_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY8_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY8_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY9_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KEY9_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_06_PT_CHNG_LOG | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_PT_CHNG_LOG | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_06_PT_CHNG_LOG | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_06_PT_CHNG_LOG | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | OBJECTCLAS | Object class | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_06_PT_CHNG_LOG | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_06_PT_CHNG_LOG | OBJECT_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_PT_CHNG_LOG | PLANCHNGNR | Planned change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_06_PT_CHNG_LOG | REPETITIVE | 'X' - Repetitive change | CHAR(1) | /SKN/E_REPEAT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_06_PT_CHNG_LOG | SPRAS | Language Key | LANG(1) | LANGU |
| /SKN/S_SW_10_06_PT_CHNG_LOG | TABKEY | Changed table record key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_06_PT_CHNG_LOG | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | TAB_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | TCODE | Transaction in which a change was made | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_06_PT_CHNG_LOG | TEXT_CASE | Flag: X=Text change | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_06_PT_CHNG_LOG | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_06_PT_CHNG_LOG | UNIT_NEW | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | UNIT_OLD | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_06_PT_CHNG_LOG | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_06_PT_CHNG_LOG | VALUE_NEW | New contents of changed field | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_06_PT_CHNG_LOG | VALUE_OLD | Old contents of changed field | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_06_PT_CHNG_LOG | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_06_PT_CHNG_LOG | VTEXT | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_PT_CHNG_LOG | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_06_PT_CHNG_LOG | WAS_PLANND | Flag that changes were generated from planned changes | CHAR(1) | CD_PLANNED |
| /SKN/S_SW_10_06_PT_CHNG_LOG | ZTERM_NEW | Terms of Payment Key | CHAR(4) | DZTERM |
| /SKN/S_SW_10_06_PT_CHNG_LOG | ZTERM_OLD | Terms of Payment Key | CHAR(4) | DZTERM |

## ABAP Code

```abap
  FUNCTION /SKN/F_SW_10_06_PT_CHNG_LOG .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_PT_CHNG_LOG
*"----------------------------------------------------------------------
    "-----------------------------------------------
    " 1. Parameters Definition                     "
    "-----------------------------------------------
    DATA_SINGLE: BACKDAYS  INT4,
                 SW_DEST   RFCDEST,
                 SPRAS     LANGU.
    DATA_MULTY: OBJECTID    KUNNR,
                ZTERM       DZTERM,
                KUNNR       KUNNR,
                BUKRS       BUKRS,
                VKORG       VKORG,
                VTWEG       VTWEG,
                MATNR       MATNR,
                USERNAME    CDUSERNAME.
    TYPES: BEGIN OF TY_KNA1,
             KUNNR TYPE KNA1-KUNNR,
             NAME1 TYPE KNA1-NAME1,
           END OF TY_KNA1,
           TT_KNA1 TYPE STANDARD TABLE OF TY_KNA1.
    TYPES: BEGIN OF TY_T001,
             BUKRS TYPE T001-BUKRS,
             BUTXT TYPE T001-BUTXT,
           END OF TY_T001,
           TT_T001 TYPE STANDARD TABLE OF TY_T001.
    TYPES: BEGIN OF TY_TVKOT,
             SPRAS TYPE TVKOT-SPRAS,
             VKORG TYPE TVKOT-VKORG,
             VTEXT TYPE TVKOT-VTEXT,
           END OF TY_TVKOT,
           TT_TVKOT TYPE STANDARD TABLE OF TY_TVKOT.
    CONSTANTS: C_KEY TYPE STRING VALUE 'KEY',
               C_V   TYPE STRING VALUE '_V',
               C_BUKRS TYPE FIELDNAME VALUE 'BUKRS',
               C_VKORG TYPE FIELDNAME VALUE 'VKORG',
               C_VTWEG TYPE FIELDNAME VALUE 'VTWEG',
               C_SPART TYPE FIELDNAME VALUE 'SPART',
               C_KNB1  TYPE TABNAME   VALUE 'KNB1',
               C_KNVV  TYPE TABNAME   VALUE 'KNVV'.
    DATA: LV_COMP       TYPE CHAR10,
          LV_IND        TYPE CHAR2,
          LV_BREAK      TYPE FLAG,
          LV_EXIT       TYPE BOOLE_D.
    DATA: LS_DATA LIKE LINE OF T_DATA[].
    DATA: LT_DATA TYPE TABLE OF /SKN/S_SW_10_06_MD_CHNG_LOG,
          LT_KNA1  TYPE TT_KNA1,
          LT_T001  TYPE TT_T001,
          LT_TVKOT TYPE TT_TVKOT.
    DATA: LS_DATA_MD LIKE LINE OF LT_DATA,
          LS_KNA1    TYPE TY_KNA1,
          LS_T001    TYPE TY_T001,
          LS_TVKOT   TYPE TY_TVKOT.
    FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF T_DATA[],
                   <FS_VAL>  TYPE ANY.
    SELECT_SINGLE: BACKDAYS,
                   SW_DEST,
                   SPRAS.
    SELECT_MULTY: OBJECTID,
                  ZTERM,
                  KUNNR,
                  BUKRS,
                  VKORG,
                  VTWEG,
                  MATNR,
                  USERNAME.
    IF LV_SPRAS IS INITIAL.
      LV_SPRAS = 'E'.
    ENDIF.
* Configuration Alert
    CALL FUNCTION '/SKN/F_SW_10_06_MD_CHNG_LOG'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = LT_DATA.
* Check if found some change in configuration log
    CHECK IS_ALERT EQ 'X'.
    SELECT_MULTY: ZTERM,
                  KUNNR,
                  BUKRS,
                  VKORG,
                  VTWEG,
                  MATNR.
* Move data change's log to main tab.
    LOOP AT LT_DATA INTO LS_DATA_MD WHERE OBJECTID IS NOT INITIAL.
      CLEAR: LV_BREAK, LV_IND, LS_DATA, LV_EXIT.
      MOVE-CORRESPONDING LS_DATA_MD TO LS_DATA.
      LS_DATA-KUNNR = LS_DATA_MD-OBJECTID.
      ASSIGN LS_DATA TO <FS_DATA>.
      CASE LS_DATA-TABNAME.
        WHEN C_KNB1. "'KNB1'.
          WHILE LV_BREAK IS INITIAL.
            LV_IND = LV_IND + 1.
            CLEAR: LV_COMP.
            CONCATENATE C_KEY LV_IND INTO LV_COMP.
            ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_DATA_MD
              TO <FS_VAL>.
            IF SY-SUBRC = 0 AND <FS_VAL> IS NOT INITIAL.
              CASE <FS_VAL>.
                WHEN C_BUKRS. "'BUKRS'.
                  CONCATENATE C_KEY LV_IND C_V INTO LV_COMP.
                  UNASSIGN <FS_VAL>.
                  ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_DATA_MD
                    TO <FS_VAL>.
                  IF SY-SUBRC = 0 AND <FS_VAL> IS NOT INITIAL.
                    IF R_BUKRS IS NOT INITIAL.
                      IF <FS_VAL> IN R_BUKRS.
                        <FS_DATA>-BUKRS = <FS_VAL>.
                      ELSE.
                        LV_EXIT = 'X'.
                        EXIT.
                      ENDIF.
                    ELSE.
                      <FS_DATA>-BUKRS = <FS_VAL>.
                    ENDIF.
                  ENDIF.
              ENDCASE.
            ELSE.
              LV_BREAK = 'X'.
            ENDIF.
            UNASSIGN <FS_VAL>.
          ENDWHILE.
        WHEN C_KNVV. "'KNVV'.
          WHILE LV_BREAK IS INITIAL.
            LV_IND = LV_IND + 1.
            CLEAR: LV_COMP.
            CONCATENATE C_KEY LV_IND INTO LV_COMP.
            ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_DATA_MD
              TO <FS_VAL>.
            IF SY-SUBRC = 0 AND <FS_VAL> IS NOT INITIAL.
              CLEAR: LV_COMP.
              CASE <FS_VAL>.
                WHEN C_VKORG. "'VKORG'.
                  CONCATENATE C_KEY LV_IND C_V INTO LV_COMP.
                  UNASSIGN <FS_VAL>.
                  ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_DATA_MD
                    TO <FS_VAL>.
                  IF SY-SUBRC = 0 AND <FS_VAL> IS NOT INITIAL.
                    IF R_VKORG IS NOT INITIAL.
                      IF <FS_VAL> IN R_VKORG.
                        <FS_DATA>-VKORG = <FS_VAL>.
                      ELSE.
                        LV_EXIT = 'X'.
                        EXIT.
                      ENDIF.
                    ELSE.
                      <FS_DATA>-VKORG = <FS_VAL>.
                    ENDIF.
                  ENDIF.
                WHEN C_VTWEG. "'VTWEG'.
                  CONCATENATE C_KEY LV_IND C_V INTO LV_COMP.
                  UNASSIGN <FS_VAL>.
                  ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_DATA_MD
                  TO <FS_VAL>.
                  IF SY-SUBRC = 0 AND <FS_VAL> IS NOT INITIAL.
                    IF R_VTWEG IS NOT INITIAL.
                      IF <FS_VAL> IN R_VTWEG.
                        <FS_DATA>-VTWEG = <FS_VAL>.
                      ELSE.
                        LV_EXIT = 'X'.
                        EXIT.
                      ENDIF.
                    ELSE.
                      <FS_DATA>-VTWEG = <FS_VAL>.
                    ENDIF.
                  ENDIF.
                WHEN C_SPART. "'SPART'.
                  CONCATENATE C_KEY LV_IND C_V INTO LV_COMP.
                  UNASSIGN <FS_VAL>.
                  ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_DATA_MD
                  TO <FS_VAL>.
                  IF SY-SUBRC = 0 AND <FS_VAL> IS NOT INITIAL.
                    <FS_DATA>-SPART = <FS_VAL>.
                  ENDIF.
              ENDCASE.
              UNASSIGN <FS_VAL>.
            ELSE.
              LV_BREAK = 'X'.
            ENDIF.
          ENDWHILE.
      ENDCASE.
      IF LV_EXIT EQ 'X'.
        CONTINUE.
      ENDIF.
      LS_DATA-ZTERM_OLD = LS_DATA_MD-VALUE_OLD.
      LS_DATA-ZTERM_NEW = LS_DATA_MD-VALUE_NEW.
      APPEND LS_DATA TO T_DATA[].
    ENDLOOP.
* if sw_dest is empty then on premise, else on cloud
    IF LV_SW_DEST IS NOT INITIAL.
      CALL FUNCTION '/SKN/FC_SW_10_06_PT_CHNG_LOG'
        IMPORTING
          IS_ALERT = IS_ALERT
        TABLES
          T_SELECT = T_SELECT
          T_DATA   = T_DATA.
    ENDIF.
    CHECK LV_SW_DEST IS INITIAL.
    IF T_DATA[] IS NOT INITIAL.
      SELECT KUNNR NAME1
        INTO TABLE LT_KNA1
        FROM KNA1
        FOR ALL ENTRIES IN T_DATA
        WHERE KUNNR EQ T_DATA-KUNNR.
      SELECT BUKRS BUTXT
        FROM T001
        INTO TABLE LT_T001
        FOR ALL ENTRIES IN T_DATA[]
        WHERE BUKRS EQ T_DATA-BUKRS.
      SELECT SPRAS VKORG VTEXT
        FROM TVKOT
        INTO TABLE LT_TVKOT
        FOR ALL ENTRIES IN T_DATA[]
        WHERE SPRAS EQ LV_SPRAS
        AND   VKORG EQ T_DATA-VKORG.
    ENDIF.
* Customer Description Name
    LOOP AT T_DATA ASSIGNING <FS_DATA>.
      IF <FS_DATA>-KUNNR IS NOT INITIAL.
        READ TABLE LT_KNA1 INTO LS_KNA1 WITH KEY KUNNR = <FS_DATA>-KUNNR
                                                 BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FS_DATA>-NAME1 = LS_DATA-NAME1.
        ENDIF.
        IF <FS_DATA>-BUKRS IS NOT INITIAL.
          READ TABLE LT_T001 INTO LS_T001 WITH KEY BUKRS = <FS_DATA>-BUKRS.
          IF SY-SUBRC = 0.
            <FS_DATA>-BUTXT = LS_T001-BUTXT.
          ENDIF.
        ENDIF.
        IF <FS_DATA>-VKORG IS NOT INITIAL.
          READ TABLE LT_TVKOT INTO LS_TVKOT WITH KEY VKORG = <FS_DATA>-VKORG.
          IF SY-SUBRC = 0.
            <FS_DATA>-BUTXT = LS_T001-BUTXT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    READ TABLE T_DATA INTO LS_DATA INDEX 1.
    CHECK SY-TFILL IS NOT INITIAL .
    IS_ALERT = 'X' .
  ENDFUNCTION.
```