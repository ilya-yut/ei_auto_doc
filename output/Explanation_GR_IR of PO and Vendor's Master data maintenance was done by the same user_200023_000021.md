# Exception Indicator: SOD:GR/IR of PO user vs. Vendor's MD changes user - SW_10_06_MD_PO_VEN_C

## General Overview

This Exception Indicator (EI) monitors GR/IR (goods receipt/invoice receipt) postings from purchase orders and correlates them with vendor master data change documents to identify cases where the same user both posted the GR/IR and changed the vendor master. It uses EKBE (purchase order history), EKKO (purchase order header), and CDHDR/CDPOS (change documents) to find PO documents where the posting user (ERNAM) matches the change-document user (USERNAME) for the vendor (LIFNR), supporting segregation-of-duties and control reviews.

This EI serves as an essential control for procurement and vendor master governance by:
- Enabling detection of segregation-of-duties violations where one user both posts GR/IR and maintains the same vendor master
- Supporting identification of vendor master changes linked to specific GR/IR postings for audit and compliance review
- Providing visibility into which fields were changed in the vendor master and when, for root-cause analysis
- Enabling analysis by company code, purchasing organization, plant, and document type for accountability
- Supporting configurable lookback and duration filters so management can focus on recent or aged exceptions

The EI supports internal controls, SOD (segregation of duties) monitoring, and vendor master governance by surfacing PO and change-document combinations that meet configurable criteria. Data is drawn from EKBE, EKKO, CDHDR, and CDPOS.


## Problem Description

Failure to monitor GR/IR postings against vendor master data changes by the same user creates multiple risks across financial reporting, operational management, and compliance.

**Financial and Reporting Issues**
- Unreviewed cases where one user posts GR/IR and changes the same vendor master can indicate segregation-of-duties failures and increase the risk of error or fraud
- Vendor master changes (e.g. bank details, payment terms) made by the same user who posted related GR/IR may escape independent review and lead to misdirected payments or misstated liabilities
- Lack of correlation between posting users and change-document users can delay detection of inappropriate access or manipulation during period-end close
- Concentrated activity by a single user on both posting and master data may signal control bypass or unauthorized changes requiring restatement or disclosure
- Absence of structured exception reporting by vendor and company code weakens audit evidence for segregation of duties

**Operational and Control Risks**
- Without visibility into who changed the vendor master and who posted the GR/IR, accountability for vendor data quality and posting accuracy is diluted
- Same-user patterns across GR/IR and vendor changes may indicate missing or ineffective approval workflows for master data and postings
- Inability to filter by date range, duration, or change-document type can delay prioritization of high-risk exceptions
- Unmonitored correlation between EKBE postings and CDHDR/CDPOS changes increases the risk of undetected SOD violations and process abuse
- Lack of field-level change detail (what changed in the vendor master) hinders operational follow-up and corrective action

**Management Visibility and Decision-Making Risks**
- Executives and internal audit lack a single view of GR/IR vs vendor master change correlation for SOD and compliance decisions
- Unidentified same-user combinations across procurement and master data can delay remediation and increase residual risk
- Without configurable lookback and duration, management cannot efficiently target recent or aged exceptions for review
- Limited ability to drill down by purchasing organization, plant, or change type restricts root-cause analysis and policy updates
- Inadequate monitoring undermines confidence in procurement and vendor master controls and may affect external audit or regulatory outcomes

## Suggested Resolution

**Immediate Response**
- Review the GR/IR and vendor change-document combinations flagged by the EI to confirm whether same-user cases are authorized (e.g. with compensating controls) or require remediation
- Verify high-risk vendor master changes (e.g. bank details, payment terms) using transaction XD02 (Change Vendor) or change-document display to confirm business justification and approval
- Check change-document type and field-level changes to determine scope and impact of vendor master modifications
- Identify the business context: routine maintenance with proper approval, emergency corrections, or potential SOD violations requiring escalation

**System Assessment**
- Analyze the selection dimensions (company code, purchasing organization, plant, date range) to understand which factors drive the exception pattern
- Compare current exception volumes and user patterns to prior periods using the same criteria to spot trends
- Review vendor distribution and change-document object type to detect concentration or misuse
- Assess posting and change dates and duration to prioritize aged or recent same-user cases
- Validate the date fields used for the monitoring window (entry date, posting date, change document date) so that the scope matches the control objective

**Corrective Actions**
- For unauthorized or inappropriate same-user cases, reassign vendor master maintenance or GR/IR posting to different roles and document compensating controls where exceptions are justified
- Escalate unexplained SOD violations to procurement management and internal audit for validation and remediation
- Update approval workflows or access rights so that vendor master changes and GR/IR postings are performed by different users where required by policy
- Implement recurring EI runs and route results to procurement and compliance owners for continuous SOD and vendor master governance
- Document findings, risk acceptance, or remediation for audit trail and management reporting


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document Number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Backdays - GR/IR |  | 0 | 0 |  |  |
| 3 | BACKDAYS_CHANGE | Backdays - Vendor's master |  | 0 | 0 |  |  |
| 4 | BELNR | Material Document | CHAR | 10 | 0 | MBLNR | BELNR |
| 5 | BPMNG | Quantity in OPUn | QUAN | 13 | 3 | MENGE_BPR | MENG13 |
| 6 | BPRME | Order Price Unit | UNIT | 3 | 0 | BBPRM | MEINS |
| 7 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 8 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 9 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 10 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 11 | CHANGE_IND | Appl. object change | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 12 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 13 | CHNGIND | Change Indicator | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 14 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 15 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 16 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 17 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 18 | DATE_REF_FLD | MD Date reference field |  | 0 | 0 |  |  |
| 19 | DATE_REF_FLD_MD | Date ref. field of the change |  | 0 | 0 |  |  |
| 20 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 21 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 22 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 23 | EBELP | Item | NUMC | 5 | 0 | EBELP | EBELP |
| 24 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 25 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 26 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 27 | ERNAM_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 28 | ERNAM_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 29 | ERNAM_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 30 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 31 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 32 | GJAHR | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 33 | KEY1 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 34 | KEY10 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 35 | KEY10_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 36 | KEY10_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 37 | KEY1_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 38 | KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 39 | KEY2 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 40 | KEY2_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 41 | KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 42 | KEY3 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 43 | KEY3_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 44 | KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 45 | KEY4 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 46 | KEY4_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 47 | KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 48 | KEY5 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 49 | KEY5_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 50 | KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 51 | KEY6 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 52 | KEY6_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 53 | KEY6_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 54 | KEY7 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 55 | KEY7_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 56 | KEY7_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 57 | KEY8 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 58 | KEY8_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 59 | KEY8_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 60 | KEY9 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 61 | KEY9_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 62 | KEY9_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 63 | KTOKK | Account Group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 64 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 65 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 66 | LIFNR | Supplier | CHAR | 10 | 0 | LIFNR | LIFNR |
| 67 | MEINS | Order Unit | UNIT | 3 | 0 | BSTME | MEINS |
| 68 | MENGE | Quantity | QUAN | 13 | 3 | MENGE_D | MENG13 |
| 69 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 70 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 71 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 72 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 73 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 74 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 75 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 76 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 77 | RECORDS | Count (Int 4) | INT4 | 10 | 0 | /SKN/E_SW_COUNT |  |
| 78 | STKZN | Natural Person | CHAR | 1 | 0 | STKZN | STKZN |
| 79 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 80 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 81 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 82 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 83 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 84 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 85 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 86 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 87 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 88 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 89 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 90 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 91 | VBUND | Company ID | CHAR | 6 | 0 | VBUND | RCOMP |
| 92 | VGABE | Trans./event type | CHAR | 1 | 0 | VGABE | VGABE |
| 93 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 94 | WAS_PLANND | Created from Planned | CHAR | 1 | 0 | CD_PLANNED | XFLAG |
| 95 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 96 | WRBTR | Amount | CURR | 13 | 2 | WRBTR | WERT7 |
| 97 | XCPDK | One-time account | CHAR | 1 | 0 | XCPDK | XFELD |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 97 parameters listed in the Parameters Reference Table above.

**ACT_CHNGNO** (Document Number):

Change document number created by the change. Restricts or displays change document reference; also appears in the result.

**BACKDAYS** (Backdays - GR/IR):

Number of days to look back from the current date when building the default monitoring window for GR/IR documents. When no explicit date range is supplied, the EI uses today minus this value as the start of the window and applies it to the date field selected by DATE_REF_FLD.

**BACKDAYS_CHANGE** (Backdays - Vendor's master):

Number of days to look back for vendor master change documents when a separate window is used. Used for change-document date filtering when applicable.

**BELNR** (Material Document):

Material document number (goods receipt/invoice receipt). Restricts which GR/IR documents are included; also appears in the result.

**BPMNG** (Quantity in OPUn):

Quantity in order price unit. Restricts or displays; also appears in the result.

**BPRME** (Order Price Unit):

Order price unit. Restricts or displays; also appears in the result.

**BUDAT** (Posting Date):

Posting date. Restricts which documents are included; when DATE_REF_FLD = BUDAT, the default range is applied. Also appears in the result.

**BUKRS** (Company Code):

Company code. Restricts which documents are included; also appears in the result.

**BUTXT** (Company Name):

Company code name. Populated from company code master; used for display.

**CHANGENR** (Document Number):

Change document number. Restricts which change documents are included; also appears in the result.

**CHANGE_IND** (Appl. object change):

Application object change type (header level). Restricts which change documents are included; also appears in the result.

**CHANGE_IND Options:**
- **U**: Update.
- **I**: Insert.
- **E**: (domain-specific).
- **D**: Delete.

**CHANGE_IND_DESC** (Domain name):

Description of the change indicator domain. Used for display.

**CHNGIND** (Change Indicator):

Change type at field level. Restricts which change document lines are included; also appears in the result.

**CHNGIND Options:**
- **U**: Update.
- **I**: Insert.
- **S**: (domain-specific).
- **D**: Delete.

**CHNGIND_DESC** (Domain name):

Description of the change indicator domain. Used for display.

**CPUDT** (Entry Date):

Entry date of the GR/IR document. Restricts which documents are included; when DATE_REF_FLD = CPUDT (default in code), the default range is applied. Also appears in the result.

**CUKY_NEW** (CUKY):

New currency value in the change document. Business meaning: referenced currency after the change.

**CUKY_OLD** (CUKY):

Old currency value in the change document. Business meaning: referenced currency before the change.

**DATE_REF_FLD** (MD Date reference field):

Name of the date field used as the reference for the default GR/IR date range and for duration calculation. Determines which date (entry date, posting date) is used when no explicit range is supplied and for computing how long the document has been in the system.

**DATE_REF_FLD Options:**
- **CPUDT**: Entry date (default in code).
- **BUDAT**: Posting date.

**DATE_REF_FLD_MD** (Date ref. field of the change):

Name of the date field used for the change document date reference. Used when filtering or correlating by change document date.

**DATE_REF_FLD_MD Options:**
- **UDATE**: Change document date (typical for vendor change documents).

**DURATION** (Duration In Time Units):

Duration in the unit given by DURATION_UNIT between the reference date (from DATE_REF_FLD) and the run date. Restricts which documents are included when a duration filter is applied; also appears in the result.

**DURATION_UNIT** (Duration Unit):

Unit for DURATION (e.g. days, hours). Used with DURATION and DATE_REF_FLD when computing and filtering by how long the document has been in the system.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours (if supported).
- **M**: Minutes (if supported).

**EBELN** (Purchasing Document):

Purchase order number. Restricts which GR/IR documents are included; also appears in the result.

**EBELP** (Item):

Purchase order item. Restricts which line items are included; also appears in the result.

**EKORG** (Purch. Organization):

Purchasing organization. Restricts which documents are included; also appears in the result.

**EKOTX** (Description):

Purchasing organization description. Populated from master; used for display.

**ERNAM** (Created by):

User who created/posted the GR/IR document. Restricts which documents are included; also appears in the result (and is correlated with change-document user for SOD check).

**ERNAM_FIRST** (First Name):

First name of the user who created the document. Populated from user master; used for display.

**ERNAM_LAST** (Last Name):

Last name of the user who created the document. Populated from user master; used for display.

**ERNAM_TEXT** (Full Name):

Full name of the user who created the document. Populated from user master; used for display.

**FIELD_DESC** (Short Description):

Short description of the changed field. Populated from repository; used for display.

**FNAME** (Field Name):

Changed field name in the change document. Restricts which change document lines are included; also appears in the result.

**GJAHR** (Fiscal Year):

Fiscal year. Restricts which documents are included; also appears in the result.

**KEY1 - KEY10** (Field Name – Field Name):

Key field names 1 through 10 for the change document or key structure. Used for key conversion or display; each restricts or populates as defined by the function.

**KEY1_DS - KEY10_DS** (Short Description – Short Description):

Short descriptions for key fields 1 through 10 (data source context). Used for display.

**KEY1_V - KEY10_V** (Short Description – Short Description):

Short descriptions for key fields 1 through 10 (value context). Used for display.

**KTOKK** (Account Group):

Vendor account group. Restricts or displays; also appears in the result when relevant to the change document.

**LAND1** (Country Key):

Country key. Restricts or displays; also appears in the result.

**LANGU** (Language for texts):

Language key used when retrieving field and table descriptions. When not supplied, a default (e.g. system language) may be used.

**LIFNR** (Supplier):

Vendor (supplier) number. Restricts which documents and change documents are included; also appears in the result. Central to the SOD check (same user posted GR/IR and changed this vendor).

**MEINS** (Order Unit):

Order unit. Restricts or displays; also appears in the result.

**MENGE** (Quantity):

Quantity. Restricts or displays; also appears in the result.

**NAME1** (Name):

Vendor name. Populated from vendor master; used for display.

**NAME_FIRST** (First Name):

First name (change-document user). Populated from user master; used for display.

**NAME_LAST** (Last Name):

Last name (change-document user). Populated from user master; used for display.

**NAME_TEXT** (Full Name):

Full name (change-document user). Populated from user master; used for display.

**OBJECTCLAS** (Change doc. object):

Change document object class (e.g. KRED for vendor). Restricts which change documents are included; also appears in the result. Default in code is vendor object class.

**OBJECTID** (Object value):

Object value (e.g. vendor number) of the change document. Restricts which change documents are included; also appears in the result.

**OBJECT_DESC** (Name):

Object description. Populated from master; used for display.

**PLANCHNGNR** (Change number):

Planned change number. Restricts or displays when applicable.

**RECORDS** (Count (Int 4)):

Record count. Populated for aggregation or display when applicable.

**STKZN** (Natural Person):

Natural person indicator. Restricts or displays; domain XFELD-like.

**STKZN Options:**
- **X**: Set (natural person).
- ** ** (space or initial): Not set.

**TABKEY** (Table Key):

Table key of the changed record. Restricts or displays; also appears in the result.

**TABNAME** (Table Name):

Table name of the changed object. Restricts which change document lines are included (e.g. LFA1); also appears in the result.

**TAB_DESC** (Short Description):

Short description of the table. Populated from repository; used for display.

**TCODE** (Transaction Code):

Transaction code that created the change document. Restricts which change documents are included; also appears in the result.

**TEXT_CASE** (Text flag):

Text flag in change document. Restricts or displays; domain XFELD-like.

**TEXT_CASE Options:**
- **X**: Set.
- ** ** (space or initial): Not set.

**UDATE** (Date):

Change document date. Restricts which change documents are included; also appears in the result. When DATE_REF_FLD_MD = UDATE, used for change-document date range.

**UNIT_NEW** (Unit):

New unit value in the change document. Restricts or displays; also appears in the result.

**UNIT_OLD** (Unit):

Old unit value in the change document. Restricts or displays; also appears in the result.

**USERNAME** (User):

User who performed the change (change document). Restricts which change documents are included; also appears in the result. Correlated with ERNAM for same-user SOD check.

**UTIME** (Time):

Change document time. Restricts or displays; also appears in the result.

**VALUE_NEW** (New value):

New value in the change document. Restricts which change document lines are included; also appears in the result.

**VALUE_OLD** (Old value):

Old value in the change document. Restricts which change document lines are included; also appears in the result.

**VBUND** (Company ID):

Company ID. Restricts or displays when applicable.

**VGABE** (Trans./event type):

Transaction/event type (e.g. goods receipt, invoice receipt). Restricts which GR/IR documents are included; also appears in the result.

**VGABE Options:**
- **1**: Goods receipt.
- **2**: Invoice receipt.

**WAERS** (Currency):

Document currency. Business meaning: currency of the GR/IR document or amount.

**WAS_PLANND** (Created from Planned):

Created from planned indicator. Restricts or displays; domain XFLAG.

**WAS_PLANND Options:**
- **X**: Set (created from planned).
- ** ** (space or initial): Not set.

**WERKS** (Plant):

Plant. Restricts which documents are included; also appears in the result.

**WRBTR** (Amount):

Amount in document currency. Restricts or displays; also appears in the result. Business meaning: amount of the GR/IR line.

**XCPDK** (One-time account):

One-time account indicator. Restricts or displays; domain XFELD.

**XCPDK Options:**
- **X**: Set (one-time account).
- ** ** (space or initial): Not set.


### Parameter Relationships

**Time and duration parameters**

- **BACKDAYS** and **DATE_REF_FLD** work together: BACKDAYS defines how many days to look back from today when no explicit date range is supplied for GR/IR documents; DATE_REF_FLD selects which date field (CPUDT, BUDAT) is used for that range and for duration calculation. Set BACKDAYS to define the default window length and DATE_REF_FLD to align the window with entry date or posting date as needed.
- **BACKDAYS_CHANGE** and **DATE_REF_FLD_MD** (when used): BACKDAYS_CHANGE defines the lookback for vendor master change documents; DATE_REF_FLD_MD selects the change-document date field (e.g. UDATE) for that range.
- **DURATION** and **DURATION_UNIT** work with **DATE_REF_FLD**: duration is computed between the reference date (from the field named by DATE_REF_FLD) and the run date, in the unit given by DURATION_UNIT (e.g. days). Use DURATION to filter by how long the document has been in the system; DURATION_UNIT defines the unit of measure.

**GR/IR and change-document correlation**

- **ERNAM** (created by) and **USERNAME** (change document user) are the core of the SOD check: the EI finds cases where ERNAM equals USERNAME for the same vendor (LIFNR). Filter or display by ERNAM and USERNAME to focus on same-user combinations.
- **LIFNR**, **OBJECTID**, **OBJECTCLAS**: LIFNR is the vendor; OBJECTID and OBJECTCLAS identify the change-document object (e.g. vendor LIFNR, object class KRED). Use together to restrict which vendors and change-document objects are included.
- **TABNAME**, **FNAME**, **VALUE_NEW**, **VALUE_OLD**: table name, field name, and old/new values in the change document. Use together to filter by which vendor master fields were changed and to what values.

**Key and display parameters**

- **KEY1 – KEY10**, **KEY1_DS – KEY10_DS**, **KEY1_V – KEY10_V**: key field names and their short descriptions for key conversion or display. Use with CONVERT_KEY (when available) or for structured display of change-document keys.


### Default Values

- **BACKDAYS** — Default: `10` (when no explicit date range is supplied; the default monitoring window starts ten days back from today for GR/IR documents).
- **DATE_REF_FLD** — Default: `CPUDT` (entry date of the GR/IR document is used as the reference for the default date range and duration calculation).
- **DATE_REF_FLD_MD** — Default: `UDATE` (change document date is used as the reference for change-document date filtering when applicable).
- **DURATION_UNIT** — Default: `D` (duration is calculated in days).
- **LANGU** — Default: `E` (English or system default for field and table descriptions when not supplied).
- **OBJECTCLAS** — Default: `KRED` (vendor object class for change documents; only vendor change documents are considered).
- **VGABE** — Default: when no range supplied, the code sets values **1** (goods receipt) and **2** (invoice receipt) so that both transaction types are included.

### Practical Configuration Examples

**Use Case 1: Same-user GR/IR and vendor changes in the last 14 days**
```
BACKDAYS = 14
DATE_REF_FLD = CPUDT
BUKRS = 1000
LIFNR = (specific vendor or range)
```
**Purpose:** Focus on GR/IR documents entered in the last 14 days in company code 1000 where the posting user also changed the vendor master, for SOD and compliance review.

**Use Case 2: By purchasing organization and plant**
```
EKORG = 1000
WERKS = 1000
BACKDAYS = 30
DATE_REF_FLD = BUDAT
```
**Purpose:** Restrict to a specific purchasing organization and plant, using posting date and a 30-day lookback, to prioritize same-user exceptions by organizational scope.

**Use Case 3: Filter by duration in days**
```
DATE_REF_FLD = CPUDT
DURATION_UNIT = D
DURATION = 1 to 90 (e.g. range: documents 1–90 days old)
```
**Purpose:** Find GR/IR documents that have been in the system between 1 and 90 days (by entry date) and where the same user changed the vendor master, for aging and follow-up.

**Use Case 4: By change-document table and field**
```
TABNAME = LFA1
FNAME = (e.g. ZTERM, BANKL)
OBJECTCLAS = KRED
BACKDAYS = 7
```
**Purpose:** Focus on vendor master (LFA1) changes to specific fields (e.g. payment terms, bank key) that correlate with GR/IR postings by the same user in the last seven days.

**Use Case 5: By transaction type (goods receipt vs invoice receipt)**
```
VGABE = 1
BUKRS = 2000
BACKDAYS = 21
DATE_REF_FLD = BUDAT
```
**Purpose:** Restrict to goods receipt (VGABE = 1) in company code 2000 with a 21-day lookback by posting date, to review same-user cases for goods receipt only.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_03_MD_CHNG_PO | ACT_CHNGNO | Change number of the document created by this change | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_03_MD_CHNG_PO | BELNR | Number of Material Document | CHAR(10) | MBLNR |
| /SKN/S_SW_10_03_MD_CHNG_PO | BPMNG | Quantity in purchase order price unit | QUAN(13,3) | MENGE_BPR |
| /SKN/S_SW_10_03_MD_CHNG_PO | BPRME | Order Price Unit (Purchasing) | UNIT(3) | BBPRM |
| /SKN/S_SW_10_03_MD_CHNG_PO | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_03_MD_CHNG_PO | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_MD_CHNG_PO | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_03_MD_CHNG_PO | CHANGE_IND | Application object change type (U, I, E, D) | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_03_MD_CHNG_PO | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | CHNGIND | Change Type (U, I, S, D) | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_03_MD_CHNG_PO | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_03_MD_CHNG_PO | CUKY_NEW | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_03_MD_CHNG_PO | CUKY_OLD | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_03_MD_CHNG_PO | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_MD_CHNG_PO | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_MD_CHNG_PO | EBELN | Purchasing Document Number | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_MD_CHNG_PO | EBELP | Item Number of Purchasing Document | NUMC(5) | EBELP |
| /SKN/S_SW_10_03_MD_CHNG_PO | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_MD_CHNG_PO | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_MD_CHNG_PO | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_MD_CHNG_PO | ERNAM_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_03_MD_CHNG_PO | ERNAM_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_03_MD_CHNG_PO | ERNAM_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | FIELD_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY10_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY10_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY1_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY2_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY3_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY4_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY5_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY6_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY6_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY7_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY7_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY8_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY8_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY9_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KEY9_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | KTOKK | Vendor account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_03_MD_CHNG_PO | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_03_MD_CHNG_PO | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_03_MD_CHNG_PO | MEINS | Purchase Order Unit of Measure | UNIT(3) | BSTME |
| /SKN/S_SW_10_03_MD_CHNG_PO | MENGE | Quantity | QUAN(13,3) | MENGE_D |
| /SKN/S_SW_10_03_MD_CHNG_PO | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_MD_CHNG_PO | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_03_MD_CHNG_PO | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_03_MD_CHNG_PO | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | OBJECTCLAS | Object class | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_03_MD_CHNG_PO | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_03_MD_CHNG_PO | OBJECT_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_MD_CHNG_PO | PLANCHNGNR | Planned change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_03_MD_CHNG_PO | RECORDS | SW : Count (Int 4) | INT4(10) | /SKN/E_SW_COUNT |
| /SKN/S_SW_10_03_MD_CHNG_PO | STKZN | Natural Person | CHAR(1) | STKZN |
| /SKN/S_SW_10_03_MD_CHNG_PO | TABKEY | Changed table record key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_03_MD_CHNG_PO | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | TAB_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_03_MD_CHNG_PO | TCODE | Transaction in which a change was made | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_03_MD_CHNG_PO | TEXT_CASE | Flag: X=Text change | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_03_MD_CHNG_PO | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_03_MD_CHNG_PO | UNIT_NEW | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_03_MD_CHNG_PO | UNIT_OLD | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_03_MD_CHNG_PO | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_03_MD_CHNG_PO | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_03_MD_CHNG_PO | VALUE_NEW | New contents of changed field | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_03_MD_CHNG_PO | VALUE_OLD | Old contents of changed field | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_03_MD_CHNG_PO | VBUND | Company ID | CHAR(6) | VBUND |
| /SKN/S_SW_10_03_MD_CHNG_PO | VGABE | Transaction/event type, purchase order history | CHAR(1) | VGABE |
| /SKN/S_SW_10_03_MD_CHNG_PO | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_MD_CHNG_PO | WAS_PLANND | Flag that changes were generated from planned changes | CHAR(1) | CD_PLANNED |
| /SKN/S_SW_10_03_MD_CHNG_PO | WERKS | Plant | CHAR(4) | WERKS_D |
| /SKN/S_SW_10_03_MD_CHNG_PO | WRBTR | Amount in document currency | CURR(13,2) | WRBTR |
| /SKN/S_SW_10_03_MD_CHNG_PO | XCPDK | Indicator: Is the account a one-time account? | CHAR(1) | XCPDK |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_MD_PO_VEND_CHG .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_MD_CHNG_PO
*"----------------------------------------------------------------------
  INCLUDE /SKN/PC_SQL_DATA.
  TYPES: BEGIN OF TY_EKBE,
           EBELN TYPE EKBE-EBELN,
           EBELP TYPE EKBE-EBELP,
           VGABE TYPE EKBE-VGABE,
           GJAHR TYPE EKBE-GJAHR,
           BELNR TYPE EKBE-BELNR,
           BUDAT TYPE EKBE-BUDAT,
           CPUDT TYPE EKBE-CPUDT,
           MENGE TYPE EKBE-MENGE,
           MEINS TYPE EKPO-MEINS,
           BPMNG TYPE EKBE-BPMNG,
           BPRME TYPE EKPO-BPRME,
           WRBTR TYPE EKBE-WRBTR,
           WAERS TYPE EKBE-WAERS,
           WERKS TYPE EKBE-WERKS,
           ERNAM TYPE EKBE-ERNAM,
           BUKRS TYPE EKKO-BUKRS,
           BUTXT TYPE T001-BUTXT,
           LIFNR TYPE EKKO-LIFNR,
           EKORG TYPE EKKO-EKORG,
* CDHDR
           OBJECTCLAS TYPE CDHDR-OBJECTCLAS,
           OBJECTID   TYPE CDHDR-OBJECTID,
           CHANGENR   TYPE CDHDR-CHANGENR,
           USERNAME   TYPE CDHDR-USERNAME,
           UDATE      TYPE CDHDR-UDATE,
           UTIME      TYPE CDHDR-UTIME,
           TCODE      TYPE CDHDR-TCODE,
         END OF TY_EKBE,
         TT_EKBE TYPE TABLE OF TY_EKBE.
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: SW_DEST             RFCDEST,
               MANAGE_IN_UTC       CHAR1 ,
               LANGU               LANGU,
               BACKDAYS            INT4,
               BACKDAYS_CHANGE     INT4,
               DURATION_D          /SKN/E_SW_DURATION_D,
               DURATION_UNIT       /SKN/E_SW_DURATION_UNIT,
               DATE_REF_FLD        NAME_FELD,
               DATE_REF_FLD_MD     NAME_FELD,
               CONVERT_KEY         CHAR1,
               HEADER_ONLY         CHAR1.
  DATA_MULTY:   LIFNR             LIFNR,
                EBELN             EBELN,
                EBELP             EBELP,
                VGABE             VGABE,
                GJAHR             GJAHR,
                BELNR             MBLNR,
                BUDAT             BUDAT,
                CPUDT             CPUDT,
                WERKS             WERKS_D,
                ERNAM             ERNAM,
                BUKRS             BUKRS,
                EKORG             EKORG,
                OBJECTCLAS        CDOBJECTCL,
                OBJECTID          CDOBJECTV,
                CHANGENR          CDCHANGENR,
                TCODE             CDTCODE,
                CHANGE_IND        CDCHNGINDH,
                TABNAME           TABNAME,
                FNAME             FIELDNAME,
                CHNGIND           CDCHNGIND,
                VALUE_NEW         CDFLDVALN,
                VALUE_OLD         CDFLDVALO,
                USERNAME          CDUSERNAME,
                UDATE             CDDATUM,
                DATUM             SYDATUM,
                DURATION          /SKN/E_SW_DURATION.
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
        LV_OBJECTID   TYPE CDHDR-OBJECTID,
        LV_LINES      TYPE I.
  DATA: LS_DATA  LIKE LINE OF T_DATA[],
        LS_CDPOS TYPE CDPOS,
        LS_LIFNR TYPE /SKN/S_SW_10_LIFNR,
        LS_EKBE  TYPE TY_EKBE.
  DATA: LT_DATA  LIKE TABLE OF T_DATA,
        LT_LIFNR TYPE TABLE OF /SKN/S_SW_10_LIFNR,
        LT_CDPOS TYPE TABLE OF CDPOS,
        LT_EKBE  TYPE TT_EKBE.
  FIELD-SYMBOLS: <FS_DATA>    LIKE LINE OF T_DATA[],
                          TYPE ANY.
* Set default parameter
  LV_BACKDAYS        = 10.
*  lv_backdays_change = 10.
  LV_DURATION_UNIT   = 'D'.
  LV_DATE_REF_FLD    = 'CPUDT'.
  LV_DATE_REF_FLD_MD = 'UDATE'.
  LV_LANGU           = 'E'.
  LV_OBJECTCLAS      = 'KRED'.
  SELECT_MULTY: LIFNR,
                EBELN,
                EBELP,
                VGABE,
                GJAHR,
                BELNR,
                CPUDT,
                BUDAT,
                WERKS,
                ERNAM,
                BUKRS,
                EKORG,
                OBJECTCLAS,
                OBJECTID,
                CHANGENR,
                TCODE,
                CHANGE_IND,
                TABNAME,
                FNAME,
                CHNGIND,
                VALUE_NEW,
                VALUE_OLD,
                UDATE,
                USERNAME,
                DATUM,
                DURATION.
  SELECT_SINGLE: SW_DEST,
                 LANGU,
                 MANAGE_IN_UTC,
                 BACKDAYS,
                 BACKDAYS_CHANGE,
                 DATE_REF_FLD,
                 CONVERT_KEY,
                 DATE_REF_FLD,
                 DATE_REF_FLD_MD,
                 DURATION_D,
                 DURATION_UNIT,
                 HEADER_ONLY.
  "--- Run Cloud Mode -----
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_MD_PO_VEND_CH'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  IF R_DATUM[] IS INITIAL .  " Set default value
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF .
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'CPUDT'.
      IF R_CPUDT[] IS INITIAL.
        R_CPUDT[] = R_DATUM[].
      ENDIF.
    WHEN 'BUDAT'.
      R_BUDAT[] = R_DATUM[].
  ENDCASE.
* Initial change document date
*  CASE lv_date_ref_fld_md.
*    WHEN 'UDATE'.
**      IF r_udate[] IS INITIAL AND lv_backdays_change IS NOT INITIAL.
**        rs_udate-sign   = 'I' .
**        rs_udate-option = 'GE' .
**        date_from       = sy-datum - lv_backdays_change.
**        rs_udate-low    = date_from .
**        APPEND rs_udate TO r_udate.
**      ENDIF.
*  ENDCASE.
* Initial Transaction type
  IF R_VGABE[] IS INITIAL.
    REFRESH: R_VGABE[].
    CLEAR RS_VGABE.
    RS_VGABE-SIGN   = 'I'.
    RS_VGABE-OPTION = 'EQ'.
    RS_VGABE-LOW    = '1'.
    APPEND RS_VGABE TO R_VGABE[].
    RS_VGABE-LOW    = '2'.
    APPEND RS_VGABE TO R_VGABE[].
  ENDIF.
  SELECT EKBE~EBELN EKBE~EBELP EKBE~VGABE EKBE~GJAHR EKBE~BELNR EKBE~BUDAT EKBE~CPUDT
         EKBE~MENGE EKBE~BPMNG EKBE~WRBTR EKBE~WAERS EKBE~WERKS EKBE~ERNAM
         EKKO~BUKRS EKKO~LIFNR EKKO~EKORG
         CDHDR~OBJECTCLAS CDHDR~OBJECTID CDHDR~CHANGENR
         CDHDR~USERNAME CDHDR~UDATE CDHDR~UTIME CDHDR~TCODE
    FROM EKBE INNER JOIN EKKO      ON  EKBE~EBELN EQ EKKO~EBELN
              INNER JOIN CDHDR     ON  EKKO~LIFNR EQ CDHDR~OBJECTID
                                   AND EKBE~ERNAM EQ CDHDR~USERNAME
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE EKBE~EBELN       IN R_EBELN[]
    AND   EKBE~EBELP       IN R_EBELP[]
    AND   EKBE~VGABE       IN R_VGABE[]
    AND   EKBE~GJAHR       IN R_GJAHR[]
    AND   EKBE~BELNR       IN R_BELNR[]
    AND   EKBE~CPUDT       IN R_CPUDT[]
    AND   EKBE~WERKS       IN R_WERKS[]
    AND   EKBE~ERNAM       IN R_ERNAM[]
    AND   EKKO~BUKRS       IN R_BUKRS[]
    AND   EKKO~LIFNR       IN R_LIFNR[]
    AND   EKKO~EKORG       IN R_EKORG[]
    AND   CDHDR~OBJECTCLAS EQ LV_OBJECTCLAS
    AND   CDHDR~OBJECTID   IN R_OBJECTID[]
    AND   CDHDR~CHANGENR   IN R_CHANGENR[]
    AND   CDHDR~UDATE      IN R_UDATE[]
    AND   CDHDR~TCODE      IN R_TCODE[]
    AND   CDHDR~USERNAME   IN R_USERNAME[] .
  CHECK LT_DATA[] IS NOT INITIAL.
  SORT LT_DATA BY LIFNR.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING LIFNR.
  LOOP AT LT_DATA INTO LS_DATA.
    CLEAR: LS_LIFNR.
    LS_LIFNR-LIFNR = LS_DATA-LIFNR.
    APPEND LS_LIFNR TO LT_LIFNR.
  ENDLOOP.
  SORT LT_LIFNR BY LIFNR.
  DELETE ADJACENT DUPLICATES FROM LT_LIFNR COMPARING LIFNR.
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    CONCATENATE 'LS_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    CHECK  IS ASSIGNED.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
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
          LS_DATA-DURATION  = TIME_DIFF .
        ELSE.
          LS_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE LT_DATA WHERE DURATION  NOT IN R_DURATION .
******************************************************************************
* Get change document data
  IF LT_DATA[] IS NOT INITIAL.
    SORT LT_DATA BY OBJECTCLAS OBJECTID CHANGENR.
    SELECT *
      FROM CDPOS
      INTO TABLE LT_CDPOS
      FOR ALL ENTRIES IN LT_DATA
      WHERE OBJECTCLAS EQ LT_DATA-OBJECTCLAS
      AND   OBJECTID   EQ LT_DATA-OBJECTID
      AND   CHANGENR   EQ LT_DATA-CHANGENR
      AND   TABNAME    IN R_TABNAME[]
      AND   FNAME      IN R_FNAME[]
      AND   VALUE_NEW  IN R_VALUE_NEW[]
      AND   VALUE_OLD  IN R_VALUE_OLD[].
  ENDIF.
  LOOP AT LT_DATA ASSIGNING <FS_DATA>.
    CLEAR: LS_DATA.
    MOVE-CORRESPONDING <FS_DATA> TO LS_DATA.
    IF LS_DATA-USERNAME IS NOT INITIAL.
**    "-- Get User name Details
      CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES_BUF'
        EXPORTING
          BNAME      = LS_DATA-USERNAME
        IMPORTING
          NAME_FIRST = LS_DATA-NAME_FIRST
          NAME_LAST  = LS_DATA-NAME_LAST
          NAME_TEXT  = LS_DATA-NAME_TEXT
*         WA_ADRP    =
        EXCEPTIONS
          NO_DATA    = 1
          OTHERS     = 2.
    ENDIF.
    IF LS_DATA-LIFNR IS NOT INITIAL.
**    "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC_ENH'
        EXPORTING
          LIFNR        = LS_DATA-LIFNR
        IMPORTING
          VENDOR_DESC  = LS_DATA-NAME1
        TABLES
          ALL_ENTRIES  = LT_LIFNR
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
    IF LS_DATA-BUKRS IS NOT INITIAL.
*    "--- Get  BUKRS Decription
      CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
        EXPORTING
          BUKRS          = LS_DATA-BUKRS  " Company Code
        IMPORTING
          COMP_CODE_DESC = LS_DATA-BUTXT  " Name of Company Code or Company
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
    ENDIF.
    IF LS_DATA-EKORG IS NOT INITIAL.
*   "-- Purch.Org. Desc.
      CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
        EXPORTING
          EKORG        = LS_DATA-EKORG
          LANGU        = LV_LANGU
        IMPORTING
          PUR_ORG_DESC = LS_DATA-EKOTX
        EXCEPTIONS
          WRONG_CODE   = 1
          OTHERS       = 2.
    ENDIF.
    LOOP AT LT_CDPOS INTO LS_CDPOS WHERE OBJECTCLAS EQ <FS_DATA>-OBJECTCLAS
                                   AND   OBJECTID   EQ <FS_DATA>-OBJECTID
                                   AND   CHANGENR   EQ <FS_DATA>-CHANGENR.
      MOVE-CORRESPONDING LS_CDPOS TO LS_DATA.
* Get field desc.
      PERFORM GET_FIELD_DESC USING LS_DATA-TABNAME
                                   LS_DATA-FNAME
                                   LV_LANGU
                             CHANGING LS_DATA-FIELD_DESC.
* Get table desc.
      PERFORM GET_TAB_DESC USING LS_DATA-TABNAME
                                 LV_LANGU
                           CHANGING LS_DATA-TAB_DESC.
      APPEND LS_DATA TO T_DATA[].
    ENDLOOP.
  ENDLOOP.
*****************************************************************
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```