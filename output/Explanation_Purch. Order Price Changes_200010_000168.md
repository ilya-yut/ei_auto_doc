# Exception Indicator: PO Change Log check - SW_10_03_PO_CHNG_LOG

## General Overview

This Exception Indicator (EI) monitors purchase order change log data to identify changes to purchase order header and item records—including price, quantity, and master data changes—across configurable time windows and organizational dimensions. It combines change document entries with purchase order and vendor master data to provide visibility into who changed what and when, supporting control over procurement and pricing integrity.

This EI serves as an essential control for procurement and financial oversight by:
- Enabling detection of purchase order and item changes (including price and condition changes) that may require approval or audit review
- Supporting identification of changes by user, date, and organizational scope for accountability and segregation of duties
- Providing visibility into the age of changes via a calculated duration from a configurable reference date for prioritization and aging analysis
- Enabling analysis of change patterns by vendor, purchasing organization, document type, and material for concentration and exception management
- Supporting month-end and audit readiness by surfacing change log exceptions that may affect valuation or compliance

Monitoring purchase order change activity helps organizations detect unauthorized or late price changes, maintain segregation between creation and amendment, and prioritize follow-up on high-value or aged changes. The EI is particularly valuable for procurement controls, internal audit, and compliance reviews.

The EI uses change document data (metadata change log) and purchase order data from EKKO, EKPO, and vendor master (LFA1), enriched with descriptions for document type, status, material group, and purchasing organization.


## Problem Description

Failure to monitor purchase order change log activity creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unmonitored price and condition changes on purchase orders can distort inventory valuation and cost accruals if discovered late during month-end close
- Changes to order value or currency after commitment may indicate unauthorized amendments requiring restatement or disclosure
- Lack of visibility into change timing can delay identification of revenue or expense recognition issues tied to PO amendments
- Concentrated changes in specific periods or company codes may signal control weaknesses affecting financial statement reliability

**Procurement and Control Risks**
- Unidentified changes by user or vendor may indicate segregation of duties violations or inappropriate override of approval workflows
- Changes to key fields (e.g. vendor, quantity, price) without monitoring increase risk of fraud or error in the procure-to-pay process
- Absence of change aging analysis can allow stale or disputed amendments to go unaddressed, affecting supplier reconciliation and dispute resolution
- High volume of changes in specific purchasing organizations or document types may reflect process or master data quality issues

**Management Visibility and Decision-Making Risks**
- Lack of consolidated change visibility delays management awareness of procurement and pricing exceptions requiring intervention
- Unmonitored change patterns by vendor or material group limit ability to enforce policies and negotiate effectively
- Missing link between change documents and PO/vendor context hinders root-cause analysis and corrective action
- Absence of duration-based prioritization (e.g. changes older than a threshold) limits efficient allocation of review resources

## Suggested Resolution

**Immediate Response**
- Review the change log records flagged by the EI to confirm the nature of the changes (price, quantity, vendor, dates) and the users and dates involved
- Verify high-impact or high-value changes using the appropriate display transaction (e.g. ME23N for purchase order) to confirm legitimacy and authorization
- Check purchase order and item status (e.g. delivery completed, deletion indicator) to assess whether corrections or reversals are still possible
- Identify business context for unusual change volume: project go-live, mass revaluation, master data cleanup, or potential error or abuse

**System Assessment**
- Analyze the monitoring window and reference date used for duration calculation to ensure the scope aligns with the control objective (e.g. last 30 days, since order date)
- Compare change volume and patterns to prior periods and to expected activity by purchasing organization, document type, and vendor
- Examine change distribution by user and organizational dimension to detect concentration or segregation issues
- Validate that filters (company code, vendor, material, status) match the intended control scope and do not hide relevant exceptions

**Corrective Actions**
- Where unauthorized or erroneous changes are confirmed, initiate correction via the appropriate transaction (e.g. ME22N for change, or follow reversal procedures) and escalate to procurement and management
- Update approval or workflow controls if segregation or authorization gaps are identified
- Adjust master data (vendor, material, purchasing info) where changes indicate data quality or process issues
- Document findings and business justifications for audit and management reporting
- Establish recurring EI runs and alert routing so that change log exceptions are reviewed continuously by responsible roles


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document Number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | AEDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 3 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 4 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 5 | BATXT | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 6 | BEDAT | Purchase Order Date | DATS | 8 | 0 | ETBDT | DATUM |
| 7 | BNFPO | Item of requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 8 | BPUMN | Quantity Conversion | DEC | 5 | 0 | BPUMN | UMBSN |
| 9 | BPUMZ | Quantity Conversion | DEC | 5 | 0 | BPUMZ | UMBSZ |
| 10 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 11 | BSTYP | Purch. doc. category | CHAR | 1 | 0 | BSTYP | BSTYP |
| 12 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 13 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 14 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 15 | BWTTY | Valuation Category | CHAR | 1 | 0 | BWTTY_D | BWTTY |
| 16 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 17 | CHANGE_IND | Appl. object change | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 18 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 19 | CHNGIND | Change Indicator | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 20 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 21 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 22 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 23 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 24 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 25 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 26 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 27 | EBELP | Item | NUMC | 5 | 0 | EBELP | EBELP |
| 28 | EINDT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 29 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 30 | EKNAM | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 31 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 32 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 33 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 34 | EREKZ | Final Invoice | CHAR | 1 | 0 | EREKZ | XFELD |
| 35 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 36 | ESTKZ | Creation indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 37 | ETENR | Schedule Line Number | NUMC | 4 | 0 | ETENR | ETENR |
| 38 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 39 | FIPOS | Commitment item | CHAR | 14 | 0 | FIPOS | FIPOS |
| 40 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 41 | GJAHR | Material Doc. Year | NUMC | 4 | 0 | MJAHR | GJAHR |
| 42 | KEY1 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 43 | KEY10 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 44 | KEY10_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 45 | KEY10_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 46 | KEY1_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 47 | KEY1_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 48 | KEY2 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 49 | KEY2_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 50 | KEY2_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 51 | KEY3 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 52 | KEY3_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 53 | KEY3_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 54 | KEY4 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 55 | KEY4_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 56 | KEY4_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 57 | KEY5 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 58 | KEY5_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 59 | KEY5_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 60 | KEY6 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 61 | KEY6_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 62 | KEY6_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 63 | KEY7 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 64 | KEY7_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 65 | KEY7_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 66 | KEY8 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 67 | KEY8_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 68 | KEY8_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 69 | KEY9 | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 70 | KEY9_DS | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 71 | KEY9_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 72 | KNTTP | Acct Assignment Cat. | CHAR | 1 | 0 | KNTTP | KNTTP |
| 73 | LIFNR | Supplier | CHAR | 10 | 0 | LIFNR | LIFNR |
| 74 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 75 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 76 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 77 | MEINS | Order Unit | UNIT | 3 | 0 | BSTME | MEINS |
| 78 | MENGE | Scheduled Quantity | QUAN | 13 | 3 | ETMEN | MENGE |
| 79 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 80 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 81 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 82 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 83 | NETPR | Net Order Price | CURR | 11 | 2 | BPREI | WERT11 |
| 84 | NETWR | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 85 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 86 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 87 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 88 | PEINH | Price unit | DEC | 5 | 0 | EPEIN | DEC5 |
| 89 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 90 | PLIFZ | Planned Deliv. Time | DEC | 3 | 0 | PLIFZ | DEC3 |
| 91 | PSTYP | Item category | CHAR | 1 | 0 | PSTYP | PSTYP |
| 92 | REPETITIVE | Repetitive Change | CHAR | 1 | 0 | /SKN/E_REPEAT | XFLAG |
| 93 | SHKZG | Debit/Credit ind | CHAR | 1 | 0 | SHKZG | SHKZG |
| 94 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 95 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 96 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 97 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 98 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 99 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 100 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 101 | TXZ01 | Short Text | CHAR | 40 | 0 | TXZ01 | TEXT40 |
| 102 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 103 | UEBTK | Unltd Overdelivery | CHAR | 1 | 0 | UEBTK | XFELD |
| 104 | UEBTO | Overdeliv. Tolerance | DEC | 3 | 1 | UEBTO | PRZ21 |
| 105 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 106 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 107 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 108 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 109 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 110 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 111 | VBUND | Trading partner | CHAR | 6 | 0 | RASSC | RCOMP |
| 112 | VGABE | Trans./event type | CHAR | 1 | 0 | VGABE | VGABE |
| 113 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 114 | WAS_PLANND | Created from Planned | CHAR | 1 | 0 | CD_PLANNED | XFLAG |
| 115 | WEMNG | Quantity Delivered | QUAN | 13 | 3 | WEEMG | MENG13 |
| 116 | WEPOS | Goods receipt | CHAR | 1 | 0 | WEPOS | XFELD |
| 117 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 118 | WGBEZ | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 118 parameters listed in the Parameters Reference Table above.

**ACT_CHNGNO** (Document Number):

Change document number identifying the change log record; used to correlate change document entries.

**AEDAT** (Created On):

Date on which the record was created. Defines the date range for change log and PO selection.

**BACKDAYS** (Days Back):

Number of days to look back from today. When no date range is supplied, the EI builds the monitoring window from today minus this value.

**BANFN** (Purchase Requisition):

Purchase requisition number.

**BATXT** (Doc. Type Descript.):

Short description of the purchasing document type; derived from document type and category via description lookup.

**BEDAT** (Purchase Order Date):

Purchase order date (schedule line order date). Defines the date range for PO selection.

**BNFPO** (Item of requisition):

Item number of the purchase requisition.

**BPUMN** (Quantity Conversion):

Denominator for conversion of order price unit into order unit. From PO item; used in quantity and price calculations.

**BPUMZ** (Quantity Conversion):

Numerator for conversion of order price unit into order unit. From PO item; used in quantity and price calculations.

**BSART** (Purchasing Doc. Type):

Purchasing document type.

**BSTYP** (Purch. doc. category):

Purchasing document category (e.g. standard order, framework order).

**BSTYP Options:**

Values are function-specific (e.g. standard order, framework order).

**BSTYP_DESC** (Short Descript.):

Short text for the purchasing document category.

**BUKRS** (Company Code):

Company code of the purchase order.

**BWTAR** (Valuation Type):

Valuation type at item level.

**BWTTY** (Valuation Category):

Valuation category at item level.

**BWTTY Options:**

Values are function-specific.

**CHANGENR** (Document Number):

Change document number; identifies the change log document.

**CHANGE_IND** (Appl. object change):

Application object change type (e.g. insert, update, delete) at header level.

**CHANGE_IND Options:**

Values indicate change type (e.g. update, insert, delete).

**CHANGE_IND_DESC** (Domain name):

Description of the change type.

**CHNGIND** (Change Indicator):

Change indicator at field level (e.g. insert, update, delete); indicates how the field value changed.

**CHNGIND Options:**

Values indicate change type (e.g. update, insert, delete).

**CHNGIND_DESC** (Domain name):

Description of the change indicator.

**CPUDT** (Entry Date):

Entry date of the change document. Defines the date range for change log selection.

**CUKY_NEW** (CUKY):

Currency key for the new value in the change document (business meaning: currency after change).

**CUKY_OLD** (CUKY):

Currency key for the old value in the change document (business meaning: currency before change).

**DURATION** (Duration In Time Units):

Duration in time units between the reference date and current date. The EI calculates this per record; use with DURATION_UNIT for time-based filtering.

**DURATION_UNIT** (Duration Unit):

Unit in which duration is expressed (e.g. days); used with DURATION for time-based filtering.

**DURATION_UNIT Options:**

- **D**: Days.

**EBELN** (Purchasing Document):

Purchase document number; key field for joining change log with PO header and item data.

**EBELP** (Item):

Item number of the purchasing document; key field with EBELN for joining change log with PO item.

**EINDT** (Delivery Date):

Item delivery date.

**EKGRP** (Purchasing Group):

Purchasing group; description is derived from master data.

**EKNAM** (Description p. group):

Description of the purchasing group (from master data).

**EKORG** (Purch. Organization):

Purchasing organization; description is derived from master data.

**EKOTX** (Description):

Description of the purchasing organization (from master data).

**ELIKZ** (Delivery Completed):

Delivery completed indicator.

**ELIKZ Options:**

- **X**: Set/active.
- ** ** (space) or blank: Not set.

**EREKZ** (Final Invoice):

Final invoice indicator.

**EREKZ Options:**

- **X**: Set/active.
- ** ** (space) or blank: Not set.

**ERNAM** (Created By):

User who created the object; supports accountability.

**ESTKZ** (Creation indicator):

Creation indicator (purchase requisition/schedule lines).

**ESTKZ Options:**

Values are function-specific (e.g. from requisition, from schedule lines).

**ETENR** (Schedule Line Number):

Schedule line number. From PO item; used in schedule line context.

**FIELD_DESC** (Short Description):

Short description of the repository object (field); from change document metadata.

**FIPOS** (Commitment item):

Commitment item.

**FNAME** (Field Name):

Field name that was changed; identifies which field had VALUE_NEW/VALUE_OLD.

**GJAHR** (Material Doc. Year):

Material document year (fiscal year context).

**KEY1 - KEY10** (Field Name – Field Name):

Key field names (1–10) for the change document table. When the key is decomposed, each KEYn holds the field name and KEYn_V holds the value.

**KEY10_DS** (Short Description):

Short description for the tenth key component in the change document key structure. Each KEYn_DS holds the description for the field named in KEYn.

**KEY10_V** (Short Description):

Short description for the tenth key component value in the change document key structure. Each KEYn_V holds the value of the key component named in KEYn.

**KEY1_DS - KEY10_DS** (Short Description – Short Description):

Short descriptions (1–10) for the change document key field names. Each KEYn_DS holds the description for the field named in KEYn; populated when the key is resolved for the change document table.

**KEY1_V - KEY10_V** (Short Description – Short Description):

Short descriptions (1–10) for the change document key field values. Each KEYn_V holds the value of the key component named in KEYn when the key is decomposed for the change document table.

**KNTTP** (Acct Assignment Cat.):

Account assignment category (e.g. cost center, order).

**KNTTP Options:**

Values are function-specific (e.g. cost center, order).

**LIFNR** (Supplier):

Vendor/supplier number; vendor name is derived from master data.

**LOEKZ** (Deletion Indicator):

Deletion indicator.

**LOEKZ Options:**

- **X**: Set/active.
- ** ** (space) or blank: Not set.

**MATKL** (Material Group):

Material group; material group description is derived from master data.

**MATNR** (Material):

Material number.

**MEINS** (Order Unit):

Order unit of measure (from PO item).

**MENGE** (Scheduled Quantity):

Scheduled quantity (from PO item).

**NAME1** (Name):

Vendor name (or object name); from vendor master.

**NAME_FIRST** (First Name):

First name of the user (from user/master data).

**NAME_LAST** (Last Name):

Last name of the user (from user/master data).

**NAME_TEXT** (Full Name):

Full name of the user (from user/master data).

**NETPR** (Net Order Price):

Net price in purchasing document (document currency) (from PO item).

**NETWR** (Net Order Value):

Net order value in PO currency (from PO item).

**OBJECTCLAS** (Change doc. object):

Change document object class. Works with OBJECTID and USERNAME to scope which change documents are read by the underlying change-log EI (e.g. purchase order object class).

**OBJECTID** (Object value):

Object value (e.g. purchase document number). Works with OBJECTCLAS to scope the change log passed to the underlying change-log EI.

**OBJECT_DESC** (Name):

Object description (e.g. vendor name); from object resolution when available.

**PEINH** (Price unit):

Price unit (from PO item); used in price calculations.

**PLANCHNGNR** (Change number):

Planned change number; indicates planned vs. actual change.

**PLIFZ** (Planned Deliv. Time):

Planned delivery time in days (from PO item).

**PSTYP** (Item category):

Item category in purchasing document (e.g. standard, subcontracting).

**PSTYP Options:**

Values are function-specific (e.g. standard, subcontracting).

**REPETITIVE** (Repetitive Change):

Indicates whether repetitive (recurring) change monitoring is used.

**REPETITIVE Options:**

- **X**: Set/active.
- ** ** (space) or blank: Not set.

**SHKZG** (Debit/Credit ind):

Debit/credit indicator; used in accounting context.

**SHKZG Options:**

Values indicate debit or credit.

**STATU** (Status):

Status of the purchasing document; status description is derived from master data.

**STATU Options:**

Values are function-specific (e.g. released, blocked).

**STATU_DESC** (Short Descript.):

Short text for the document status.

**TABKEY** (Table Key):

Changed table record key; identifies the record that was changed.

**TABNAME** (Table Name):

Table name of the changed object; used with KEY1–KEY10 to resolve the key structure.

**TAB_DESC** (Short Description):

Short description of the table (from repository).

**TCODE** (Transaction Code):

Transaction code in which the change was made (from change document).

**TEXT_CASE** (Text flag):

Indicates whether the change was a text change.

**TEXT_CASE Options:**

- **X**: Set/active.
- ** ** (space) or blank: Not set.

**TXZ01** (Short Text):

Short text (from PO item).

**UDATE** (Date):

Change document creation date. Defines the date range for change log selection.

**UEBTK** (Unltd Overdelivery):

Unlimited overdelivery allowed indicator.

**UEBTK Options:**

- **X**: Set/active.
- ** ** (space) or blank: Not set.

**UEBTO** (Overdeliv. Tolerance):

Overdelivery tolerance limit; numeric value.

**UNIT_NEW** (Unit):

Unit of measure for the new value in the change document (from change document).

**UNIT_OLD** (Unit):

Unit of measure for the old value in the change document (from change document).

**USERNAME** (User):

User who made the change. Passed to the underlying change-log EI; used to scope change documents and for accountability.

**UTIME** (Time):

Time of the change (from change document).

**VALUE_NEW** (New value):

New contents of the changed field in the change document (from change log).

**VALUE_OLD** (Old value):

Old contents of the changed field in the change document (from change log).

**VBUND** (Trading partner):

Company ID of the trading partner when joining vendor master.

**VGABE** (Trans./event type):

Transaction/event type (purchase order history).

**VGABE Options:**

Values are function-specific.

**WAERS** (Currency):

Document currency of the PO (business meaning: currency in which amounts are stated).

**WAS_PLANND** (Created from Planned):

Indicates whether the change was created from planned changes.

**WAS_PLANND Options:**

- **X**: Set/active.
- ** ** (space) or blank: Not set.

**WEMNG** (Quantity Delivered):

Quantity of goods received (from PO item).

**WEPOS** (Goods receipt):

Goods receipt indicator.

**WEPOS Options:**

- **X**: Set/active.
- ** ** (space) or blank: Not set.

**WERKS** (Plant):

Plant.

**WGBEZ** (Material Group Desc.):

Material group description (from material group).


### Parameter Relationships

**Time-Based Selection Parameters:**

- When no date range is supplied, the EI builds the monitoring window from today minus the lookback length. The number of days to look back is configured via a single numeric parameter; that value defines the start of the window used to select change log and purchase order data.

**Duration Calculation Parameters:**

- The EI computes a duration (in time units) between a reference date and the current date per record. The reference date is taken from the output record using a configurable date field name. The unit in which duration is expressed (e.g. days) is configured separately. Together, the reference date field and the duration unit determine how duration is calculated and displayed; a numeric duration filter can then be used to restrict results (e.g. changes older than N days).

**Change Document Selection Parameters:**

- Object class, object value, and user name work together to scope which change documents are read by the underlying change-log step. Object class identifies the type of object (e.g. purchase order); object value can narrow to a specific object (e.g. document number); user name restricts to changes made by specific users. Setting these in combination focuses the result set on the relevant change log subset.

**New and Old Value Filters:**

- New value and old value are multi-value parameters that restrict which change log records are included based on the after-change and before-change field contents. They are often used together to find specific price or condition changes (e.g. a given old value and new value pair).

**Key Structure Parameters (KEY1–KEY10, KEY1_DS–KEY10_DS, KEY1_V–KEY10_V, TABNAME):**

- The key field names (KEY1–KEY10) define the structure of the change document table key. When the key is decomposed, each KEYn holds the field name, KEYn_V holds the value, and KEYn_DS holds the short description for that key component. TABNAME identifies the table to which the key applies. Together they define and describe the change document key used to join with purchase order data (e.g. document number and item).


### Default Values

- **BACKDAYS** — Default: `10` (when not supplied, the EI uses a 10-day lookback from today for the monitoring window).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).
- **ELIKZ** — Default: initial (empty); when not supplied, no restriction on delivery-completed indicator.
- **LOEKZ** — Default: initial (empty); when not supplied, no restriction on deletion indicator.

**Note:** Other single-value parameters that are read from the caller and used when initial (e.g. empty or zero) effectively default to “no restriction” or “use system default” where the code allows; refer to the code for behavior of optional filters.

### Practical Configuration Examples

**Use Case 1: Last 10 days of purchase order changes (default lookback)**

```
BACKDAYS = 10
```

**Purpose:** Monitor all PO change log activity in the last 10 days with default duration unit (days). Suitable for routine weekly or biweekly review of recent changes.

**Use Case 2: Price and value changes only**

```
VALUE_NEW = <range of new values>
VALUE_OLD = <range of old values>
```

**Purpose:** Restrict results to change log records where the new or old value falls in the given ranges. Use to focus on specific price or condition changes (e.g. before/after values) for audit or exception review.

**Use Case 3: By company code and vendor**

```
BUKRS = 1000, 2000
LIFNR = 0000100001–0000100050
```

**Purpose:** Limit change log results to purchase orders in selected company codes and vendor number ranges. Supports regional or vendor-specific control and review.

**Use Case 4: Duration-based filtering (changes older than 30 days)**

```
DURATION_UNIT = D
DURATION = 30–999999
```

**Purpose:** Show changes where the calculated duration (in days) is at least 30. Helps prioritize aged changes and follow-up; duration unit and duration range work together for time-based filtering.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|----------|---------------|
| /SKN/S_SW_10_06_PO_CHNG_LOG | .INCLUDE |  |  |  |
| /SKN/S_SW_10_06_PO_CHNG_LOG | ACT_CHNGNO | Change number of the document created by this change | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_06_PO_CHNG_LOG | AEDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BANFN | Purchase Requisition Number | CHAR(10) | BANFN |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BATXT | Short Description of Purchasing Document Type | CHAR(20) | BATXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BEDAT | Order date of schedule line | DATS(8) | ETBDT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BNFPO | Item Number of Purchase Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BPUMN | Denominator for Conv. of Order Price Unit into Order Unit | DEC(5) | BPUMN |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BPUMZ | Numerator for Conversion of Order Price Unit into Order Unit | DEC(5) | BPUMZ |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BSART | Purchasing Document Type | CHAR(4) | ESART |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BSTYP | Purchasing Document Category | CHAR(1) | BSTYP |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BSTYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_06_PO_CHNG_LOG | BWTTY | Valuation Category | CHAR(1) | BWTTY_D |
| /SKN/S_SW_10_06_PO_CHNG_LOG | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_06_PO_CHNG_LOG | CHANGE_IND | Application object change type (U, I, E, D) | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_06_PO_CHNG_LOG | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | CHNGIND | Change Type (U, I, S, D) | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_06_PO_CHNG_LOG | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | CUKY_NEW | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_PO_CHNG_LOG | CUKY_OLD | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_PO_CHNG_LOG | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_06_PO_CHNG_LOG | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | EBELN | Purchasing Document Number | CHAR(10) | EBELN |
| /SKN/S_SW_10_06_PO_CHNG_LOG | EBELP | Item Number of Purchasing Document | NUMC(5) | EBELP |
| /SKN/S_SW_10_06_PO_CHNG_LOG | EINDT | Item Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_06_PO_CHNG_LOG | EKNAM | Description of purchasing group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_06_PO_CHNG_LOG | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_06_PO_CHNG_LOG | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_06_PO_CHNG_LOG | ELIKZ | "Delivery Completed" Indicator | CHAR(1) | ELIKZ |
| /SKN/S_SW_10_06_PO_CHNG_LOG | EREKZ | Final Invoice Indicator | CHAR(1) | EREKZ |
| /SKN/S_SW_10_06_PO_CHNG_LOG | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_06_PO_CHNG_LOG | ESTKZ | Creation Indicator (Purchase Requisition/Schedule Lines) | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_06_PO_CHNG_LOG | ETENR | Delivery Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_06_PO_CHNG_LOG | FIELD_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | FIPOS | Commitment Item | CHAR(14) | FIPOS |
| /SKN/S_SW_10_06_PO_CHNG_LOG | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | GJAHR | Material Document Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY10_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY10_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY1_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY1_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY2_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY2_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY3_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY3_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY4_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY4_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY5_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY5_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY6_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY6_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY7_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY7_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY8_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY8_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY9_DS | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KEY9_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | KNTTP | Account Assignment Category | CHAR(1) | KNTTP |
| /SKN/S_SW_10_06_PO_CHNG_LOG | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_PO_CHNG_LOG | LOEKZ | Deletion Indicator in Purchasing Document | CHAR(1) | ELOEK |
| /SKN/S_SW_10_06_PO_CHNG_LOG | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_06_PO_CHNG_LOG | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_06_PO_CHNG_LOG | MEINS | Purchase Order Unit of Measure | UNIT(3) | BSTME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | MENGE | Scheduled Quantity | QUAN(13,3) | ETMEN |
| /SKN/S_SW_10_06_PO_CHNG_LOG | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_PO_CHNG_LOG | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_06_PO_CHNG_LOG | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_06_PO_CHNG_LOG | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | NETPR | Net Price in Purchasing Document (in Document Currency) | CURR(11,2) | BPREI |
| /SKN/S_SW_10_06_PO_CHNG_LOG | NETWR | Net Order Value in PO Currency | CURR(13,2) | BWERT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | OBJECTCLAS | Object class | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_06_PO_CHNG_LOG | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_06_PO_CHNG_LOG | OBJECT_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_PO_CHNG_LOG | PEINH | Price Unit | DEC(5) | EPEIN |
| /SKN/S_SW_10_06_PO_CHNG_LOG | PLANCHNGNR | Planned change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_06_PO_CHNG_LOG | PLIFZ | Planned Delivery Time in Days | DEC(3) | PLIFZ |
| /SKN/S_SW_10_06_PO_CHNG_LOG | PSTYP | Item Category in Purchasing Document | CHAR(1) | PSTYP |
| /SKN/S_SW_10_06_PO_CHNG_LOG | REPETITIVE | 'X' - Repetitive change | CHAR(1) | /SKN/E_REPEAT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | SHKZG | Debit/Credit Indicator | CHAR(1) | SHKZG |
| /SKN/S_SW_10_06_PO_CHNG_LOG | STATU | Status of Purchasing Document | CHAR(1) | ESTAK |
| /SKN/S_SW_10_06_PO_CHNG_LOG | STATU_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | TABKEY | Changed table record key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_06_PO_CHNG_LOG | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | TAB_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | TCODE | Transaction in which a change was made | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_06_PO_CHNG_LOG | TEXT_CASE | Flag: X=Text change | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_06_PO_CHNG_LOG | TXZ01 | Short Text | CHAR(40) | TXZ01 |
| /SKN/S_SW_10_06_PO_CHNG_LOG | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_06_PO_CHNG_LOG | UEBTK | Indicator: Unlimited Overdelivery Allowed | CHAR(1) | UEBTK |
| /SKN/S_SW_10_06_PO_CHNG_LOG | UEBTO | Overdelivery Tolerance Limit | DEC(3,1) | UEBTO |
| /SKN/S_SW_10_06_PO_CHNG_LOG | UNIT_NEW | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | UNIT_OLD | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_06_PO_CHNG_LOG | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_06_PO_CHNG_LOG | VALUE_NEW | New contents of changed field | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_06_PO_CHNG_LOG | VALUE_OLD | Old contents of changed field | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_06_PO_CHNG_LOG | VBUND | Company ID of trading partner | CHAR(6) | RASSC |
| /SKN/S_SW_10_06_PO_CHNG_LOG | VGABE | Transaction/event type, purchase order history | CHAR(1) | VGABE |
| /SKN/S_SW_10_06_PO_CHNG_LOG | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_06_PO_CHNG_LOG | WAS_PLANND | Flag that changes were generated from planned changes | CHAR(1) | CD_PLANNED |
| /SKN/S_SW_10_06_PO_CHNG_LOG | WEMNG | Quantity of Goods Received | QUAN(13,3) | WEEMG |
| /SKN/S_SW_10_06_PO_CHNG_LOG | WEPOS | Goods Receipt Indicator | CHAR(1) | WEPOS |
| /SKN/S_SW_10_06_PO_CHNG_LOG | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_06_PO_CHNG_LOG | WGBEZ | Material Group Description | CHAR(20) | WGBEZ |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_PO_CHNG_LOG .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_PO_CHNG_LOG
*"----------------------------------------------------------------------
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: SW_DEST          RFCDEST,
               BACKDAYS         INT4,
               LANGU            LANGU,
               ELIKZ            ELIKZ,
               LOEKZ            ELOEK,
               DATE_REF_FLD     NAME_FELD,
               DURATION_UNIT    /SKN/E_SW_DURATION_UNIT.
  DATA_MULTY: OBJECTCLAS  CDOBJECTCL,
              OBJECTID    EBELN,
              EBELN       EBELN,
              BUKRS       BUKRS,
              BSART       BSART,
              BSTYP       EBSTYP,
              LOEKZ       ELOEK,
              STATU       ESTAK,
              AEDAT       ERDAT,
              ERNAM       ERNAM,
              LIFNR       ELIFN,
              EKORG       EKORG,
              EKGRP       BKGRP,
              WAERS       WAERS,
              MATNR       MATNR,
              WERKS       EWERK,
              MATKL       MATKL,
              KNTTP       KNTTP,
              BWTAR       BWTAR_D,
              BWTTY       BWTTY_D,
              ELIKZ       ELIKZ,
              EREKZ       EREKZ,
              PSTYP       PSTYP,
              FIPOS       FIPOS,
              WEPOS       WEPOS,
              BEDAT       ETBDT,
              EINDT       EINDT,
              BANFN       BANFN,
              BNFPO       BNFPO,
              ESTKZ       ESTKZ,
              VBUND       RASSC,
              UEBTO       UEBTO,
              UEBTK       UEBTK,
              USERNAME    CDUSERNAME,
              VALUE_NEW   CDFLDVALN,
              VALUE_OLD   CDFLDVALO,
              DATUM       SY-DATUM,
              DURATION    /SKN/E_SW_DURATION.
  DATA: LV_FIELDNAME TYPE FIELDNAME,
        LV_SHIFT     TYPE DDLENG,
        LV_LENG      TYPE DDLENG,
        LV_DOMNAME   TYPE DD07V-DOMNAME,
        LV_DOMVALUE  TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT    TYPE DD07V-DDTEXT,
        LV_EBELN     TYPE EBELN,
        LV_EBELP     TYPE EBELP.
  DATA: LT_DATA TYPE TABLE OF /SKN/S_SW_10_06_PO_CHNG_LOG.
  DATA: LS_DATA LIKE LINE OF T_DATA[].
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: SY_TABIX  LIKE SY-TABIX,
        DATE_FROM LIKE SY-DATUM .
  DATA: LT_DATA_MD TYPE TABLE OF /SKN/S_SW_10_06_MD_CHNG_LOG.
*  DATA: ls_data_md LIKE LINE OF lt_data_md.
  FIELD-SYMBOLS: <FS_DATA>    LIKE LINE OF T_DATA[],
                 <FS_DATA_MD> LIKE LINE OF LT_DATA_MD,
                          TYPE ANY.
  SELECT_SINGLE: SW_DEST,
                 BACKDAYS.
  SELECT_MULTY: OBJECTCLAS,
                OBJECTID,
                USERNAME.
* Configuration Alert
  CALL FUNCTION '/SKN/F_SW_10_06_MD_CHNG_LOG'
    IMPORTING
      IS_ALERT = IS_ALERT
    TABLES
      T_SELECT = T_SELECT
      T_DATA   = LT_DATA_MD.
* Check if found some change in configuration log
  CHECK IS_ALERT EQ 'X'.
  SELECT_SINGLE: LANGU,
                 LOEKZ,
                 ELIKZ,
                 LOEKZ,
                 DATE_REF_FLD,
                 DURATION_UNIT.
  IF LV_BACKDAYS IS INITIAL.
    LV_BACKDAYS = 10.
  ENDIF.
  IF LV_DURATION_UNIT IS INITIAL.
    LV_DURATION_UNIT = 'D'.
  ENDIF.
  LV_ELIKZ = SPACE.
  LV_LOEKZ = SPACE.
*  lv_date_ref_fld  = 'BEDAT'. "PO date
*  lv_wepos         = 'X'.
  SELECT_MULTY: EBELN,
                BUKRS,
                BSART,
                BSTYP,
                LOEKZ,
                STATU,
                AEDAT,
                ERNAM,
                LIFNR,
                EKORG,
                EKGRP,
                WAERS,
                MATNR,
                WERKS,
                MATKL,
                KNTTP,
                BWTAR,
                BWTTY,
                ELIKZ,
                EREKZ,
                PSTYP,
                FIPOS,
                WEPOS,
                BEDAT,
                BANFN,
                BNFPO,
                ESTKZ,
                VBUND,
                UEBTO,
                UEBTK,
                VALUE_NEW,
                VALUE_OLD,
                DATUM,
                DURATION.
  IF R_VALUE_NEW[] IS NOT INITIAL.
    DELETE LT_DATA_MD WHERE VALUE_NEW NOT IN R_VALUE_NEW[].
  ENDIF.
  IF R_VALUE_OLD[] IS NOT INITIAL.
    DELETE LT_DATA_MD WHERE VALUE_OLD NOT IN R_VALUE_OLD[].
  ENDIF.
* Move data change's log to main tab.
  LOOP AT LT_DATA_MD ASSIGNING <FS_DATA_MD>.
    CLEAR: LS_DATA.
    MOVE-CORRESPONDING <FS_DATA_MD> TO LS_DATA.
    LS_DATA-EBELN = <FS_DATA_MD>-KEY2_V.
    LS_DATA-EBELP = <FS_DATA_MD>-KEY3_V.
    APPEND LS_DATA TO T_DATA[].
  ENDLOOP.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_PO_CHNG_LOG'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    FROM EKKO AS K INNER JOIN EKPO AS P ON K~EBELN EQ P~EBELN
                   INNER JOIN LFA1 AS L ON K~LIFNR EQ L~LIFNR
    FOR ALL ENTRIES IN T_DATA
    WHERE K~EBELN EQ T_DATA-EBELN
    AND   K~BUKRS IN R_BUKRS[]
    AND   K~BSART IN R_BSART[]
    AND   K~BSTYP IN R_BSTYP[]
    AND   K~LOEKZ IN R_LOEKZ[]
    AND   K~STATU IN R_STATU[]
    AND   K~AEDAT IN R_AEDAT[]
    AND   K~ERNAM IN R_ERNAM[]
    AND   K~LIFNR IN R_LIFNR[]
    AND   K~EKORG IN R_EKORG[]
    AND   K~EKGRP IN R_EKGRP[]
    AND   P~EBELP EQ T_DATA-EBELP
    AND   P~MATNR IN R_MATNR[]
    AND   P~WERKS IN R_WERKS[]
    AND   P~MATKL IN R_MATKL[]
    AND   P~KNTTP IN R_KNTTP[]
    AND   P~BWTAR IN R_BWTAR[]
    AND   P~BWTTY IN R_BWTTY[]
    AND   P~ELIKZ IN R_ELIKZ[]
    AND   P~EREKZ IN R_EREKZ[]
    AND   P~PSTYP IN R_PSTYP[]
    AND   P~FIPOS IN R_FIPOS[]
    AND   P~WEPOS IN R_WEPOS[]
    AND   P~LOEKZ IN R_LOEKZ[]
    AND   P~UEBTO IN R_UEBTO[]
    AND   P~UEBTK IN R_UEBTK[]
    AND   L~VBUND IN R_VBUND[].
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    CONCATENATE 'LS_DATA-' LV_DATE_REF_FLD INTO FLD .
    CHECK FLD IS NOT INITIAL.
    ASSIGN (FLD) TO .
    CHECK  IS ASSIGNED.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = REF_DATE
          T_FROM            = SY-UZEIT
          D_TO              = SY-DATUM
          T_TO              = SY-UZEIT
          TIME_UNIT         = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
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
  DELETE LT_DATA WHERE DURATION  NOT IN R_DURATION.
  SORT LT_DATA_MD BY KEY2_V KEY3_V.
  SORT LT_DATA[]  BY EBELN EBELP.
* Customer Description Name
  LOOP AT LT_DATA_MD ASSIGNING <FS_DATA_MD>.
    CLEAR: LS_DATA, LV_EBELN, LV_EBELP.
    LV_EBELN = <FS_DATA_MD>-KEY2_V.
    LV_EBELP = <FS_DATA_MD>-KEY3_V.
    READ TABLE LT_DATA[] ASSIGNING <FS_DATA>
      WITH KEY EBELN = LV_EBELN
               EBELP = LV_EBELP
               BINARY SEARCH.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING <FS_DATA> TO LS_DATA.
    ENDIF.
    MOVE-CORRESPONDING <FS_DATA_MD> TO LS_DATA.
    IF LS_DATA-MATKL IS NOT INITIAL.
* Material group desc.
      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
      EXPORTING
        MATKL              = LS_DATA-MATKL
      IMPORTING
        MATKL_DESC         = LS_DATA-WGBEZ
*       MATKL_DESC_L       =
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2
        .
    ENDIF.
*
    IF LS_DATA-BSART IS NOT INITIAL AND LS_DATA-BSTYP IS NOT INITIAL.
*    "-- BSART_DESC
      CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART            = LS_DATA-BSART
        LANGU            = LV_LANGU
        BSTYP            = LS_DATA-BSTYP
      IMPORTING
        TYPE_DESC        = LS_DATA-BATXT
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    ENDIF.
    IF LS_DATA-STATU IS NOT INITIAL.
      "-- STATU_DESC
      LV_DOMNAME = 'ESTAK'.
      LV_DOMVALUE = LS_DATA-STATU.
      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME        = LV_DOMNAME
        I_DOMVALUE       = LV_DOMVALUE
        LANGU            = LV_LANGU
*       SW_DEST          =
      IMPORTING
        E_DDTEXT         = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST        = 1
        OTHERS           = 2.
      IF SY-SUBRC = 0.
        LS_DATA-STATU_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
*
    IF LS_DATA-BSTYP IS NOT INITIAL.
*    "-- BSTYP_DESC
      LV_DOMNAME = 'EBSTYP'.
      LV_DOMVALUE = <FS_DATA>-BSTYP.
      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME        = LV_DOMNAME
        I_DOMVALUE       = LV_DOMVALUE
        LANGU            = LV_LANGU
*       SW_DEST          =
      IMPORTING
        E_DDTEXT         = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST        = 1
        OTHERS           = 2.
      IF SY-SUBRC = 0.
        LS_DATA-BSTYP_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
*
    IF LS_DATA-LIFNR IS NOT INITIAL.
*    "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR              = LS_DATA-LIFNR
      IMPORTING
        VENDOR_DESC        = LS_DATA-NAME1
      EXCEPTIONS
        WRONG_VENDOR       = 1
        OTHERS             = 2.
    ENDIF.
*
    IF LS_DATA-EKORG IS NOT INITIAL.
*   "-- EKORG_DESC
      CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG              = LS_DATA-EKORG
      IMPORTING
        PUR_ORG_DESC       = LS_DATA-EKOTX
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    ENDIF.
*
*
    IF LS_DATA-EKGRP IS NOT INITIAL.
*   "-- EKGRP_DESC
      CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP              = LS_DATA-EKGRP
      IMPORTING
        PUR_GRP_DESC       = LS_DATA-EKNAM
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    ENDIF.
    APPEND LS_DATA TO T_DATA[].
  ENDLOOP.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```