# Exception Indicator: SOD:GR/IR of PO user vs. Vendor's MD changes user - SW_10_06_MD_PO_VEN_C

## General Overview

This Exception Indicator (EI) monitors purchase order goods receipt/invoice receipt (GR/IR) postings and vendor master data changes to identify cases where the same user performed both the GR/IR posting and changes to the vendor master record. It supports segregation-of-duties (SOD) controls in procurement by correlating the user who created or last changed the GR/IR document with the user who changed the vendor master in the change document.

This EI serves as an essential control for procurement and compliance by:
- Enabling detection of potential conflict of interest when one user both posts GR/IR and maintains vendor master data
- Supporting identification of segregation-of-duties violations in the procure-to-pay process for audit and remediation
- Providing visibility into which vendor master changes coincide with GR/IR activity by the same user for risk assessment
- Enabling accountability by user and vendor for management review and access policy updates
- Supporting compliance with internal controls and audit requirements around segregation of duties in master data and posting

This monitoring helps organizations detect and remediate SOD risks where a single user can both influence vendor data and post receipts or invoices against purchase orders. The EI is particularly valuable for internal audit, access reviews, and procurement control design.

The EI uses purchase order history (EKBE), purchase order header (EKKO), and change document header (CDHDR) data, joined by vendor and user, to return matching GR/IR lines with associated change document details.


## Problem Description

Failure to monitor cases where the same user performs both GR/IR postings and vendor master data changes creates multiple risks across financial reporting, operational control, and compliance:

**Financial and Reporting Issues**
- A single user who can both change vendor data and post GR/IR may create or alter vendor bank details and then post payments, increasing fraud risk
- Undetected concentration of posting and master data maintenance in one user can undermine period-end controls and reconciliation
- Lack of visibility into user overlap delays audit evidence collection and may lead to qualified findings
- Unmonitored same-user patterns can distort segregation-of-duties testing and internal control assessments

**Procurement and Control Risks**
- Segregation-of-duties violations in procurement increase the risk of unauthorized vendor creation or modification followed by self-approved receipts or invoices
- Same user performing GR/IR and vendor master changes may bypass intended approval and review controls
- Absence of monitoring limits the organization’s ability to enforce access policies and role design in procurement
- Vendor master data quality and integrity may be compromised when the same user controls both data and postings

**Management Visibility and Decision-Making Risks**
- Management may be unaware of SOD gaps until an audit or incident occurs
- Lack of consolidated view of user activity across GR/IR and vendor master delays access redesign and role cleanup
- Unidentified same-user patterns hinder risk-based prioritization of user access reviews and training
- Insufficient monitoring limits accountability and corrective action for segregation-of-duties violations

## Suggested Resolution

**Immediate Response**
- Review the GR/IR and vendor change records flagged by the EI to confirm whether the same-user pattern is justified (e.g. small team, documented exception) or a control failure
- Verify high-value or sensitive vendor master changes (e.g. bank details, payment terms) linked to GR/IR postings by the same user
- Check whether flagged users have legitimate dual roles or require role separation and access changes
- Identify business context: one-off exception, project role, or systemic SOD gap

**System Assessment**
- Analyze the time window and organizational scope of the results to see which company codes, plants, and vendors are affected
- Compare current results to prior periods to identify recurring users or worsening concentration
- Examine change document types and fields changed (e.g. bank, payment terms) to assess risk level
- Assess whether vendor master change volume and GR/IR volume by user indicate a structural SOD issue
- Validate the date basis used for the monitoring window so that the scope aligns with audit or policy requirements

**Corrective Actions**
- Redesign roles and authorizations so that vendor master maintenance and GR/IR posting are assigned to different users where policy requires segregation
- For justified exceptions, document and approve with time limits and compensating controls
- Update access and role design in the system and communicate to process owners and internal audit
- Schedule recurring EI runs and route results to procurement and audit for ongoing monitoring
- Use EI output in access certification and SOD reviews to prioritize remediation and training


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI data selection and processing logic.

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

Identifies the change document number created by the change. In this EI it is populated from the change document header when the EI builds the result row linking GR/IR and vendor master changes.

**BACKDAYS** (Backdays - GR/IR):

Number of days to look back from today for the GR/IR posting date used when building the monitoring window. When no date range is supplied, the EI uses today minus this value as the start of the window for the GR/IR entry date. Default in code: 10.

**BACKDAYS_CHANGE** (Backdays - Vendor's master):

Reserved for future use: intended to define the lookback for the vendor master change document date. In the current code the vendor change date window is driven by the main date range and change document selection.

**BELNR** (Material Document):

Material document number of the GR/IR line. The EI uses it when selecting purchase order history and linking to change documents.

**BPMNG** (Quantity in OPUn):

Quantity in order price unit for the GR/IR line. The EI reads it from the purchase order history when building each result row.

**BPRME** (Order Price Unit):

Order price unit of measure for the GR/IR line. Populated from the purchase order item when the EI builds the result.

**BUDAT** (Posting Date):

Posting date of the GR/IR document. Used when the EI builds the date window; the code can map the lookback to this field when DATE_REF_FLD is set accordingly.

**BUKRS** (Company Code):

Company code of the purchase order. The EI joins purchase order header and GR/IR; this value identifies the company code for the order.

**BUTXT** (Company Name):

Name of the company code. Populated by the EI from master data when building the result row for the company code of the order.

**CHANGENR** (Document Number):

Change document number. The EI links GR/IR lines to change document headers by vendor and user; this value identifies the change document.

**CHANGE_IND** (Appl. object change):

Application object change type (e.g. insert, update, delete). Used when selecting change document positions.

**CHANGE_IND Options:**
- **U**: Update
- **I**: Insert
- **E**: (domain-specific)
- **D**: Delete

**CHANGE_IND_DESC** (Domain name):

Short description of the change type. Populated from the domain when the EI builds the result for the change indicator.

**CHNGIND** (Change Indicator):

Change type at field level (e.g. update, insert). Used when selecting change document position.

**CHNGIND Options:**
- **U**: Update
- **I**: Insert
- **S**: (domain-specific)
- **D**: Delete

**CHNGIND_DESC** (Domain name):

Short description of the field-level change type. Populated from the domain when the EI builds the result.

**CPUDT** (Entry Date):

Entry date of the accounting document for the GR/IR line. Used as the default date reference when building the monitoring window; the EI applies the lookback to this field when DATE_REF_FLD is CPUDT.

**CUKY_NEW** (CUKY):

Currency key for the new value in the change document. Represents document or transaction currency in the change.

**CUKY_OLD** (CUKY):

Currency key for the old value in the change document. Represents the previous currency in the change.

**DATE_REF_FLD** (MD Date reference field):

Name of the date field used for the GR/IR monitoring window. The EI uses this to decide which date (e.g. entry date, posting date) to apply the lookback to when building the selection. Default in code: CPUDT.

**DATE_REF_FLD Options:**
- **CPUDT**: Entry date of the accounting document (default)
- **BUDAT**: Posting date

**DATE_REF_FLD_MD** (Date ref. field of the change):

Name of the date field used for the change document date. Reserved for future use to align the vendor master change date window; in the current code the change document date is driven by the main date range.

**DATE_REF_FLD_MD Options:**
- **UDATE**: Change document date (typical for change doc header)

**DURATION** (Duration In Time Units):

Length of time between the reference date of the GR/IR line and the evaluation date, in the unit given by DURATION_UNIT. The EI calculates this for each line and uses it to filter and to show how long ago the posting occurred.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed. The EI uses it when computing the time difference between the GR/IR reference date and the evaluation date and when filtering by duration.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**BACKDAYS and DATE_REF_FLD Connection:** BACKDAYS defines how many days to look back; DATE_REF_FLD defines which date on the GR/IR line that lookback is applied to (e.g. entry date or posting date). Together they define the monitoring window for GR/IR.

**DURATION and DURATION_UNIT Connection:** DURATION is the numeric length of time; DURATION_UNIT is the unit (hours, minutes, days, or full days). The EI uses both to compute and filter by how long ago the event occurred.

**EBELN** (Purchasing Document):

Purchasing document number. The EI selects GR/IR lines by purchase order and links to change documents by vendor and user; this value identifies the order.

**EBELP** (Item):

Item number of the purchasing document. Used with EBELN to identify the GR/IR line.

**EKORG** (Purch. Organization):

Purchasing organization of the order. The EI uses it when selecting orders and populates it in the result from the order header.

**EKOTX** (Description):

Description of the purchasing organization. Populated by the EI from master data when building the result row.

**ERNAM** (Created by):

User who created or last changed the GR/IR document. The EI correlates this user with the user in the change document (vendor master) to find same-user SOD cases.

**ERNAM_FIRST** (First Name):

First name of the user who created the GR/IR document. Populated by the EI from user master when building the result.

**ERNAM_LAST** (Last Name):

Last name of that user. Populated by the EI from user master when building the result.

**ERNAM_TEXT** (Full Name):

Full name of that user. Populated by the EI from user master when building the result.

**FIELD_DESC** (Short Description):

Short description of the changed field in the change document. Populated by the EI when building the result for each change document position.

**FNAME** (Field Name):

Name of the changed field in the change document. Used when selecting change document positions.

**GJAHR** (Fiscal Year):

Fiscal year of the GR/IR document. Used when selecting purchase order history.

**KEY1 - KEY10** (Field Name – Field Name):

Key field names (1–10) for the change document object. Used when the EI selects or presents change document key components; each slot holds one key field name. Values are function-specific and depend on the change document object type.

**KEY1_DS - KEY10_DS** (Short Description – Short Description):

Short descriptions for the key fields (1–10). Populated from the data dictionary when the EI builds the result for the key field names.

**KEY1_V - KEY10_V** (Short Description – Short Description):

Short descriptions for the key field values (1–10). Populated when the EI builds the result for the key components of the change document.

**KTOKK** (Account Group):

Vendor account group. Can be used in selection when the EI resolves vendor master data; populated in the result when the EI enriches the output with vendor master attributes.

**LAND1** (Country Key):

Country key of the vendor. Populated in the result when the EI enriches with vendor master data; can be used in selection.

**LANGU** (Language for texts):

Language for descriptions (e.g. field and table texts). The EI uses it when resolving descriptions for the result; default in code: E.

**LIFNR** (Supplier):

Vendor number. The EI joins GR/IR and change documents by vendor; this value identifies the vendor in the join and correlation logic.

**MEINS** (Order Unit):

Order unit of measure for the GR/IR line. Populated from the purchase order item when the EI builds the result.

**MENGE** (Quantity):

Quantity of the GR/IR line. Populated from the purchase order history when the EI builds the result.

**NAME1** (Name):

Vendor name. Populated by the EI from vendor master when building the result row.

**NAME_FIRST** (First Name):

First name of the user in the change document. Populated by the EI from user master when building the result.

**NAME_LAST** (Last Name):

Last name of that user. Populated by the EI from user master when building the result.

**NAME_TEXT** (Full Name):

Full name of that user. Populated by the EI from user master when building the result.

**OBJECTCLAS** (Change doc. object):

Change document object class (e.g. KRED for vendor). The EI uses it to select change document headers; default in code: KRED.

**OBJECTID** (Object value):

Object value of the change document (e.g. vendor number). The EI links change documents to GR/IR by vendor using this value.

**OBJECT_DESC** (Name):

Description of the change document object. Populated by the EI when building the result (e.g. vendor name).

**PLANCHNGNR** (Change number):

Planned change number. Populated from the change document when the EI builds the result, when applicable.

**RECORDS** (Count (Int 4)):

Record count. Populated by the EI when the result includes aggregation or count information.

**STKZN** (Natural Person):

Indicator for natural person. Populated from vendor or user master when the EI enriches the result; used in selection when applicable.

**STKZN Options:**
- **X** or **1**: Natural person
- ** ** or **0**: Not natural person

**TABKEY** (Table Key):

Key of the changed table record. Populated from the change document when the EI builds the result.

**TABNAME** (Table Name):

Name of the changed table in the change document. Used when selecting change document positions.

**TAB_DESC** (Short Description):

Short description of the changed table. Populated by the EI from the data dictionary when building the result.

**TCODE** (Transaction Code):

Transaction code in which the change was made. Used when selecting change document headers.

**TEXT_CASE** (Text flag):

Indicator that the change is a text change. Used when selecting change document positions.

**TEXT_CASE Options:**
- **X**: Text change
- ** **: Not a text change

**UDATE** (Date):

Date of the change document. The EI uses it when selecting change document headers; when DATE_REF_FLD_MD is used, this field is the typical reference for the change document date.

**UNIT_NEW** (Unit):

Unit of measure for the new value in the change document. Populated from the change document when the EI builds the result.

**UNIT_OLD** (Unit):

Unit of measure for the old value in the change document. Populated from the change document when the EI builds the result.

**USERNAME** (User):

User who made the change in the change document. The EI correlates this with the GR/IR creator (ERNAM) to find same-user cases.

**UTIME** (Time):

Time of the change in the change document. Populated from the change document when the EI builds the result.

**VALUE_NEW** (New value):

New value of the changed field in the change document. Used when selecting change document positions.

**VALUE_OLD** (Old value):

Old value of the changed field in the change document. Used when selecting change document positions.

**VBUND** (Company ID):

Company ID. Populated from the change document or related master data when the EI builds the result.

**VGABE** (Trans./event type):

Transaction or event type of the GR/IR line (e.g. goods receipt, invoice receipt). The EI uses it when selecting purchase order history; default in code includes 1 and 2.

**VGABE Options:**
- **1**: Goods receipt
- **2**: Invoice receipt
- (other values as in domain VGABE)

**WAERS** (Currency):

Document currency of the GR/IR line. Populated from the purchase order history when the EI builds the result; represents the currency of the amount.

**WAS_PLANND** (Created from Planned):

Indicator that the change was created from planned changes. Populated from the change document when the EI builds the result.

**WAS_PLANND Options:**
- **X**: Created from planned
- ** **: Not from planned

**WERKS** (Plant):

Plant of the GR/IR line. Used when the EI selects purchase order history.

**WRBTR** (Amount):

Amount in document currency for the GR/IR line. Populated from the purchase order history when the EI builds the result.

**XCPDK** (One-time account):

Indicator for one-time vendor. Populated from vendor master when the EI enriches the result; used in selection when applicable.

**XCPDK Options:**
- **X**: One-time account
- ** **: Regular account


### Parameter Relationships

**Time and date parameters**

- **BACKDAYS** and **DATE_REF_FLD** work together: BACKDAYS defines how many days to look back from today; DATE_REF_FLD defines which date on the GR/IR line (e.g. entry date CPUDT or posting date BUDAT) that lookback is applied to. Together they define the monitoring window for GR/IR postings.
- **DURATION** and **DURATION_UNIT** work together: DURATION is the length of time between the GR/IR reference date and the evaluation date; DURATION_UNIT is the unit (hours, minutes, days, or full days). The EI uses both to compute and filter by how long ago the posting occurred.
- **DATE_REF_FLD_MD** is intended to define the date field for the change document side (e.g. UDATE); in the current code the change document date range is driven by the main date selection.

**Change document and key parameters**

- **OBJECTCLAS**, **OBJECTID**, and **CHANGENR** identify the change document; the EI uses them to join change document headers to GR/IR by vendor and user. OBJECTCLAS (e.g. KRED for vendor) is fixed by default; OBJECTID and CHANGENR are used in selection and in the join.
- **TABNAME** and **FNAME** identify the changed table and field in the change document position; they are used together when selecting and presenting change document details.
- **KEY1–KEY10** (and KEY1_DS–KEY10_DS, KEY1_V–KEY10_V) define the key structure of the change document object; they work with TABNAME when the EI presents or filters by change document key components.

**Vendor and user correlation**

- **LIFNR** (vendor) and **ERNAM** (user who created the GR/IR document) together with **USERNAME** (user in the change document) are central: the EI correlates GR/IR lines and change documents by vendor and matches ERNAM to USERNAME to find same-user SOD cases.


### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the EI uses today minus 10 days as the start of the GR/IR monitoring window).
- **DURATION_UNIT** — Default: `D` (days).
- **DATE_REF_FLD** — Default: `CPUDT` (entry date of the accounting document).
- **DATE_REF_FLD_MD** — Default: `UDATE` (change document date; used when the change-document date window is applied).
- **LANGU** — Default: `E` (English for descriptions).
- **OBJECTCLAS** — Default: `KRED` (vendor change documents).
- **VGABE** — Default when not supplied: transaction types `1` (goods receipt) and `2` (invoice receipt) are used so that both GR and IR are included.

**Note:** Parameters that are not supplied remain initial; the EI uses the defaults above for the corresponding logic (e.g. BACKDAYS for the date window, OBJECTCLAS for change document object type).

### Practical Configuration Examples

**Use Case 1: Same-user GR/IR and vendor changes – last 30 days**
```
BACKDAYS = 30
DATE_REF_FLD = CPUDT
OBJECTCLAS = KRED
```
**Purpose:** Monitor GR/IR postings from the last 30 days by entry date and link to vendor (KRED) change documents to find cases where the same user posted GR/IR and changed vendor master.

**Use Case 2: Focus on one company code and plant**
```
BUKRS = 1000
WERKS = 1010
LIFNR = 0000100000
```
**Purpose:** Restrict the analysis to a specific company code, plant, and vendor for a targeted SOD review.

**Use Case 3: Duration in full days and single vendor**
```
BACKDAYS = 14
DURATION_UNIT = F
DURATION = 7
LIFNR = 0000100000
BUKRS = 1000
```
**Purpose:** Look back 14 days, express duration in full days for specific-day filtering, and include only lines with duration 7 (e.g. 7 full days ago), for one vendor and company code.

**Use Case 4: Entry date window and change document table**
```
BACKDAYS = 10
DATE_REF_FLD = CPUDT
OBJECTCLAS = KRED
TABNAME = LFA1
FNAME = NAME1
```
**Purpose:** Use the default 10-day window on entry date, vendor change documents only, and restrict to changes on vendor name (table LFA1, field NAME1) for name-change SOD analysis.

**Use Case 5: Multi-dimensional SOD review**
```
BACKDAYS = 60
DATE_REF_FLD = BUDAT
DURATION_UNIT = D
EKORG = 1000
BUKRS = 1000
OBJECTCLAS = KRED
```
**Purpose:** Look back 60 days by posting date, use days for duration, and restrict to one purchasing organization and company code for a broader SOD review by vendor and user.


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

`bap
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
`
