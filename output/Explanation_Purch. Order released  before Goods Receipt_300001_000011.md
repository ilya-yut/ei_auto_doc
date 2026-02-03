# Exception Indicator: Released PO waiting for GR - SW_10_03_OPEN_PR_DET

## General Overview

This Exception Indicator (EI) monitors purchase order schedule lines that are released but not yet fully received, identifying open order quantities across configurable time windows and organizational dimensions. It provides visibility into procurement fulfillment gaps by linking requisition and purchase order data to highlight schedule lines with remaining quantity to be received, supporting timely goods receipt and exception management.

This EI serves as an essential control for procurement and operational oversight by:

- Enabling detection of released purchase orders with open quantities that require goods receipt or follow-up
- Supporting identification of aged or delayed schedule lines for prioritization and vendor or logistics escalation
- Providing visibility into open order quantities by company code, plant, material, and vendor for resource and delivery planning
- Enabling analysis of procurement fulfillment patterns by requisition date, delivery date, and organizational scope for performance and compliance review
- Supporting accountability by purchasing organization, purchasing group, and requisitioner for exception handling and process improvement

This monitoring helps organizations reduce delivery delays, improve goods receipt discipline, and maintain accurate inventory and commitment reporting. The EI is particularly valuable for procurement exception reviews, month-end close support, and monitoring of released-but-not-received orders.

The EI uses purchase order schedule line data (EKET), purchase requisition (EBAN), order header and item data (EKKO, EKPO), and vendor master (LFA1) to build the result set and calculate duration from a configurable reference date.


## Problem Description

Failure to monitor released purchase orders with open quantities creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**

- Unreceived released orders can distort commitment and accrual reporting and delay accurate period-end cutoffs
- Open quantities on aged schedule lines may indicate revenue or cost recognition timing issues or missed goods receipt postings
- Lack of visibility into unreceived orders can delay month-end close when discrepancies are discovered late during reconciliation
- Concentrated open quantities in specific company codes or plants can mask underlying delivery or process issues in other areas

**Procurement and Control Risks**

- Released orders without timely goods receipt may indicate supplier delivery failures, internal receiving bottlenecks, or process gaps
- Unusual patterns by vendor, material, or plant could signal quality holds, blocked receipts, or master data issues requiring intervention
- High open quantities in specific purchasing groups or requisitioners may reflect overload or inadequate follow-up
- Atypical open-order patterns could indicate data entry errors, duplicate orders, or system integration failures requiring correction

**Management Visibility and Decision-Making Risks**

- Lack of monitoring delays awareness of significant delivery backlogs and supply chain bottlenecks
- Unidentified aged open quantities can lead to missed escalation, expediting, or alternative sourcing decisions
- Absence of multi-dimensional visibility limits ability to assign accountability and optimize procurement and receiving resources

## Suggested Resolution

**Immediate Response**

- Review the released orders with open quantities flagged by the EI to understand scope, age, and organizational distribution
- Verify high-value or critical items using the appropriate display transactions to confirm legitimacy and expected receipt dates
- Check document status and processing state to ensure no blocks or pending corrections
- Identify business context: seasonal peaks, known supplier delays, or data quality issues

**System Assessment**

- Analyze the time window and reference date used for monitoring to ensure it matches business needs
- Review historical trends by comparing current open quantities to prior periods by vendor, plant, and material group
- Examine organizational distribution (company code, purchasing organization, purchasing group) to identify concentration or process gaps
- Assess requisition and delivery date patterns to determine if exceptions are date-specific or structural

**Corrective Actions**

- For delayed or blocked receipts, coordinate with logistics and vendors to complete goods receipt or adjust delivery dates
- Update master data or release strategies if recurring patterns indicate process or configuration issues
- Document exceptions and business justifications for audit and management reporting
- Establish recurring EI execution and alert routing to procurement and receiving stakeholders for continuous visibility


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | AFNAM | Requisitioner | CHAR | 12 | 0 | AFNAM | AFNAM |
| 3 | ANLN1 | Asset | CHAR | 12 | 0 | ANLN1 | ANLN1 |
| 4 | ANLN2 | Subnumber | CHAR | 4 | 0 | ANLN2 | ANLN2 |
| 5 | AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 6 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 7 | BADAT | Requisition Date | DATS | 8 | 0 | BADAT | DATUM |
| 8 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 9 | BATXT | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 10 | BEDAT | Purchase Order Date | DATS | 8 | 0 | ETBDT | DATUM |
| 11 | BNFPO | Item of requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 12 | BPUMN | Quantity Conversion | DEC | 5 | 0 | BPUMN | UMBSN |
| 13 | BPUMZ | Quantity Conversion | DEC | 5 | 0 | BPUMZ | UMBSZ |
| 14 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 15 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | BSTYP | BSTYP |
| 16 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 17 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 18 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 19 | BWTTY | Valuation Category | CHAR | 1 | 0 | BWTTY_D | BWTTY |
| 20 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 21 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 22 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 23 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 24 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 25 | EBELP | Item | NUMC | 5 | 0 | EBELP | EBELP |
| 26 | EINDT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 27 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 28 | EKNAM | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 29 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 30 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 31 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 32 | ERDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 33 | EREKZ | Final Invoice | CHAR | 1 | 0 | EREKZ | XFELD |
| 34 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 35 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 36 | ETENR | Schedule Line Number | NUMC | 4 | 0 | ETENR | ETENR |
| 37 | FIPOS | Commitment item | CHAR | 14 | 0 | FIPOS | FIPOS |
| 38 | FRGKE | Release indicator (PO) | CHAR | 1 | 0 | FRGKE | FRGKE |
| 39 | FRGKZ | Release indicator | CHAR | 1 | 0 | FRGKZ | FRGKZ |
| 40 | GJAHR | Material Doc. Year | NUMC | 4 | 0 | MJAHR | GJAHR |
| 41 | GL_ACC_TXT | G/L Acct Long Text | CHAR | 50 | 0 | TXT50_SKAT | TEXT50 |
| 42 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 43 | KNTTP | Acct Assignment Cat. | CHAR | 1 | 0 | KNTTP | KNTTP |
| 44 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 45 | KOSTL | Cost Center | CHAR | 10 | 0 | KOSTL | KOSTL |
| 46 | KOSTL_DESC | Description | CHAR | 40 | 0 | KLTXT | TEXT40 |
| 47 | LFDAT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 48 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 49 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 50 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 51 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 52 | MEINS | Order Unit | UNIT | 3 | 0 | BSTME | MEINS |
| 53 | MENGE | Scheduled Quantity | QUAN | 13 | 3 | ETMEN | MENGE |
| 54 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 55 | NETWR | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 56 | OPEN_ORDER_QUAN | Open Quantity | QUAN | 13 | 3 | OBMNG | MENG13 |
| 57 | PEINH | Price unit | DEC | 5 | 0 | EPEIN | DEC5 |
| 58 | PLIFZ | Planned Deliv. Time | DEC | 3 | 0 | PLIFZ | DEC3 |
| 59 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 60 | PRCTR_DESC | Long Text | CHAR | 40 | 0 | LTEXT | TEXT40 |
| 61 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 62 | PSTYP | Item Category | CHAR | 1 | 0 | PSTYP | PSTYP |
| 63 | PS_PSP_PNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 64 | SAKTO | G/L Account | CHAR | 10 | 0 | SAKNR | SAKNR |
| 65 | SHKZG | Debit/Credit ind | CHAR | 1 | 0 | SHKZG | SHKZG |
| 66 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 67 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 68 | TXZ01 | Short Text | CHAR | 40 | 0 | TXZ01 | TEXT40 |
| 69 | UEBTK | Unltd Overdelivery | CHAR | 1 | 0 | UEBTK | XFELD |
| 70 | UEBTO | Overdeliv. Tolerance | DEC | 3 | 1 | UEBTO | PRZ21 |
| 71 | USER_FLD | User field for Dyn Rec List |  | 0 | 0 |  |  |
| 72 | VBELN | SD Document | CHAR | 10 | 0 | VBELN_CO | VBELN |
| 73 | VBELP | Item | NUMC | 6 | 0 | POSNR_CO | POSNR |
| 74 | VBUND | Trading Partner | CHAR | 6 | 0 | RASSC | RCOMP |
| 75 | VGABE | Trans./event type | CHAR | 1 | 0 | VGABE | VGABE |
| 76 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 77 | WEMNG | Qty Delivered | QUAN | 13 | 3 | WEEMG | MENG13 |
| 78 | WEPOS | Goods receipt | CHAR | 1 | 0 | WEPOS | XFELD |
| 79 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 80 | WGBEZ | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 80 parameters listed in the Parameters Reference Table above.

**AEDAT** (Created On):

Date on which the purchasing document was created. Used when building the monitoring window and when filtering by document creation date.

**AFNAM** (Requisitioner):

Requisitioner or requester of the purchase requisition; value comes from the requisition linked to the schedule line.

**ANLN1 - ANLN2** (Asset – Subnumber):

Main asset number and subnumber from account assignment on the order item; values come from account assignment data.

**AUFNR** (Order):

Order (internal or production order) from account assignment on the order item; value comes from account assignment.

**BACKDAYS** (Days Back):

Number of days to look back from today when no date range is supplied. Defines the start of the monitoring window; the EI uses today minus this value as the lower bound for the reference date.

**BADAT** (Requisition Date):

Requisition date on the purchase requisition; value comes from the requisition linked to the schedule line. Applied when building the monitoring window or filtering by requisition date.

**BANFN** (Purchase Requisition):

Purchase requisition number; schedule lines are linked to requisition by this and item. Value comes from the schedule line or requisition.

**BATXT** (Doc. Type Descript.):

Short description of the purchasing document type; value comes from master data for the order type of the schedule line.

**BEDAT** (Purchase Order Date):

Order date of the schedule line. Used in the monitoring window when the reference date field is set to order date; also used to restrict by order date.

**BNFPO** (Item of requisition):

Item number of the purchase requisition. Used together with requisition number to restrict and to link schedule lines to requisition lines.

**BPUMN** (Quantity Conversion):

Denominator for conversion of order price unit into order unit; value comes from the order item and is used in quantity and value calculations.

**BPUMZ** (Quantity Conversion):

Numerator for conversion of order price unit into order unit; value comes from the order item and is used with denominator for conversions.

**BSART** (Purchasing Doc. Type):

Purchasing document type (e.g. standard, framework); value comes from the order header.

**BSTYP** (Purch. Doc. Category):

Purchasing document category (e.g. standard, consignment). Restricts by category; populated from the order header.

**BSTYP_DESC** (Short Descript.):

Short text for the purchasing document category; value comes from domain/master data for the order category.

**BUKRS** (Company Code):

Company code of the purchase order; value comes from the order header.

**BWTAR** (Valuation Type):

Valuation type on the order item; value comes from the order item.

**BWTTY** (Valuation Category):

Valuation category on the order item; value comes from the order item.

**CPUDT** (Entry Date):

Date the accounting document was entered; value comes from the order or related document when available.

**DATE_REF_FLD** (Date Ref Field):

Which date field is used as the reference for the monitoring window and for calculating duration (e.g. order date, delivery date, creation date). The lookback window is applied to this field; duration is computed from this date to today.

**DATE_REF_FLD Options:**

- **BEDAT**: Order date of the schedule line (purchasing document date)
- **AEDAT**: Document created / changed on
- **EINDT**: Item delivery date
- **ERDAT**: Requisition changed on
- **BADAT**: Requisition date
- **Other**: Treated as order date (BEDAT)

**DURATION** (Duration In Time Units):

Calculated duration from the reference date to today in the configured duration unit. Computed after the reference date and unit are applied; represents how long the order has been in the monitored state.

**DURATION_UNIT** (Duration Unit):

Unit for duration (day, week, month). Used when computing duration from reference date to today and when filtering by duration.

**DURATION_UNIT Options:**

- **D**: Days
- **W**: Weeks (if supported)
- **M**: Months (if supported)

**EBELN** (Purchasing Document):

Purchasing document number; primary key of the order header. Value comes from the schedule line or order.

**EBELP** (Item):

Item number of the purchasing document; used with order number to identify the schedule line. Value comes from the schedule line.

**EINDT** (Delivery Date):

Item delivery date. When chosen as reference date field, used for the monitoring window and duration; value comes from the schedule line.

**EKGRP** (Purchasing Group):

Purchasing group; value comes from the order header.

**EKNAM** (Description p. group):

Description of the purchasing group; value comes from master data.

**EKORG** (Purch. Organization):

Purchasing organization; value comes from the order header.

**EKOTX** (Description):

Description of the purchasing organization; value comes from master data.

**ELIKZ** (Delivery Completed):

Indicator for delivery completed on the order item (e.g. X = completed, space = not completed). Value comes from the order item.

**ELIKZ Options:**

- **X**: Delivery completed
- ** ** (space): Not completed

**ERDAT** (Changed On):

Requisition changed-on date. Used to restrict by requisition change date and, when chosen as reference date field, for the monitoring window.

**EREKZ** (Final Invoice):

Final invoice indicator on the order item. Restricts by this flag; value comes from the order item.

**EREKZ Options:**

- **X**: Final invoice
- ** ** (space): Not final invoice

**ERNAM** (Created by):

Requisition creator; value comes from the requisition.

**ESTKZ** (Creation Indicator):

Creation indicator (e.g. purchase requisition vs. schedule line origin); value comes from the requisition.

**ESTKZ Options:**

- Values are function-specific; see code or output structure.

**ETENR** (Schedule Line Number):

Schedule line number; identifies the schedule line within the order item. Value comes from the schedule line.

**FIPOS** (Commitment item):

Commitment item from account assignment on the order item; value comes from account assignment.

**FRGKE** (Release indicator (PO)):

Release indicator on the purchasing document; value comes from the order header.

**FRGKE Options:**

- **X**: Released
- ** ** (space): Not released (or as in domain)

**FRGKZ** (Release indicator):

Release indicator on the requisition; value comes from the requisition.

**FRGKZ Options:**

- **X**: Released
- ** ** (space): Not released (or as in domain)

**GJAHR** (Material Doc. Year):

Material document year; value comes from the order or material document when relevant.

**GL_ACC_TXT** (G/L Acct Long Text):

G/L account long text; value comes from master data when company code and G/L account are present on the record.

**GSBER** (Business Area):

Business area from account assignment; value comes from account assignment.

**KNTTP** (Acct Assignment Cat.):

Account assignment category on the order item; value comes from the order item.

**KOKRS** (Controlling Area):

Controlling area; value comes from the order or controlling data. Used when resolving cost center or profit center descriptions.

**KOSTL** (Cost Center):

Cost center from account assignment; value comes from account assignment.

**KOSTL_DESC** (Description):

Cost center description; value comes from master data when cost center and controlling area are present on the record.

**LFDAT** (Delivery Date):

Item delivery date (alternative key); same semantics as EINDT. Value comes from the schedule line.

**LIFNR** (Vendor):

Vendor (creditor) of the purchase order; value comes from the order header.

**LOEKZ** (Deletion Indicator):

Deletion indicator on the requisition (e.g. X = deleted, space = not deleted). Value comes from the requisition.

**LOEKZ Options:**

- **X**: Deletion indicator set
- ** ** (space): Not deleted

**MATKL** (Material Group):

Material group on the order item; value comes from the order item.

**MATNR** (Material):

Material number on the order item; value comes from the order item.

**MEINS** (Order Unit):

Order unit of measure; value comes from the order item.

**MENGE** (Scheduled Quantity):

Scheduled quantity on the schedule line; value comes from the schedule line and is used in open-quantity calculation.

**NAME1** (Name):

Vendor name; value comes from vendor master.

**NETWR** (Net Order Value):

Net order value in order currency; value comes from the order item.

**OPEN_ORDER_QUAN** (Open Quantity):

Remaining quantity to be received (scheduled minus received). Calculated from schedule line quantities; represents open amount to be received.

**PEINH** (Price unit):

Price unit; value comes from the order item and is used in value calculations.

**PLIFZ** (Planned Deliv. Time):

Planned delivery time in days; value comes from the order item.

**PRCTR** (Profit Center):

Profit center from account assignment; value comes from account assignment.

**PRCTR_DESC** (Long Text):

Profit center long text; value comes from master data when profit center and controlling area are present on the record.

**PROCSTAT** (Purch. doc. proc. state):

Purchasing document processing state; value comes from the order header.

**PSTYP** (Item Category):

Item category (e.g. standard, consignment); value comes from the order item.

**PS_PSP_PNR** (WBS Element):

WBS element from account assignment; value comes from account assignment.

**SAKTO** (G/L Account):

G/L account from account assignment; value comes from account assignment.

**SHKZG** (Debit/Credit ind):

Debit/credit indicator; value comes from the order or document when available.

**STATU** (Status):

Status of the purchasing document; value comes from the order header.

**STATU_DESC** (Short Descript.):

Short text for the document status; value comes from domain/master data.

**TXZ01** (Short Text):

Short text on the order item; value comes from the order item.

**UEBTK** (Unltd Overdelivery):

Indicator for unlimited overdelivery allowed; value comes from the order item.

**UEBTK Options:**

- **X**: Unlimited overdelivery allowed
- ** ** (space): Not unlimited (or as in domain)

**UEBTO** (Overdeliv. Tolerance):

Overdelivery tolerance limit; value comes from the order item.

**USER_FLD** (User field for Dyn Rec List):

User-defined field for dynamic recipient list or similar. Usage is configuration-specific; see implementation.

**VBELN** (SD Document):

SD document number from account assignment; value comes from account assignment.

**VBELP** (Item):

SD document item; value comes from account assignment.

**VBUND** (Trading Partner):

Company ID of trading partner (vendor group); value comes from the vendor master linked to the order.

**VGABE** (Trans./event type):

Transaction or event type for purchase order history; value comes from the order or history when relevant.

**WAERS** (Currency):

Order currency (document currency); value comes from the order header.

**WEMNG** (Qty Delivered):

Quantity of goods received; value comes from the schedule line and is used with scheduled quantity to derive open quantity.

**WEPOS** (Goods receipt):

Goods receipt indicator (e.g. X = GR relevant or required, space = not set). Value comes from the order item.

**WEPOS Options:**

- **X**: Goods receipt indicator set (e.g. GR required or relevant)
- ** ** (space): Not set

**WERKS** (Plant):

Plant on the order item; value comes from the order item.

**WGBEZ** (Material Group Desc.):

Material group description; value comes from master data.


### Parameter Relationships

**Time-Based Monitoring Parameters:**

- **BACKDAYS** defines how many days to look back from today when no date range is supplied; it sets the lower bound of the monitoring window.
- **DATE_REF_FLD** selects which date field (order date, delivery date, requisition date, creation date) is used for that window and for duration calculation.
- **DURATION** is computed from the reference date (from DATE_REF_FLD) to today in the unit given by **DURATION_UNIT** (e.g. days).
- Together, BACKDAYS + DATE_REF_FLD define the time window; DURATION_UNIT + the reference date determine how duration is expressed and filtered.

**Goods Receipt and Completion Flags:**

- **WEPOS** (goods receipt indicator), **ELIKZ** (delivery completed), and **LOEKZ** (deletion indicator on requisition) are used together to narrow to relevant schedule lines: WEPOS focuses on lines with GR relevance, ELIKZ on completion status, LOEKZ on requisition deletion status. Set them in combination to match the desired exception (e.g. released, not deleted, not yet fully received).

**Requisition and Order Link:**

- **BANFN** and **BNFPO** link schedule lines to requisition lines. Filtering by requisition number and item restricts the result to specific requisition-derived orders.


### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the monitoring window starts at today minus 10 days).
- **DATE_REF_FLD** — Default: `BEDAT` (order date of the schedule line is used as the reference for the monitoring window and duration).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days).
- **ELIKZ** — Default: initial (space); items with delivery-not-completed are included.
- **LOEKZ** — Default: initial (space); requisition lines without deletion indicator are included.
- **WEPOS** — Default: `X` (items with goods receipt indicator set are included).
- **LANGU** — Default: initial; when not supplied, system language (SY-LANGU) is used for descriptions.

**Note:** When R_DATUM (date range) is initial, the EI builds the window from today minus BACKDAYS.

### Practical Configuration Examples

**Use Case 1: Released orders with open quantity – last 14 days by order date**
```
BACKDAYS = 14
DATE_REF_FLD = BEDAT
DURATION_UNIT = D
WEPOS = X
```
**Purpose:** Monitor released purchase order schedule lines with open quantity whose order date falls within the last 14 days, with duration in days. Suitable for weekly exception review by order date.

**Use Case 2: Focus by company code and purchasing organization**
```
BUKRS = 1000
EKORG = 1000
BACKDAYS = 30
DATE_REF_FLD = EINDT
```
**Purpose:** Restrict to a specific company code and purchasing organization, with a 30-day window based on delivery date. Useful for organizational exception reporting.

**Use Case 3: Requisition-based view**
```
BANFN = 5000000000
BACKDAYS = 7
DATE_REF_FLD = ERDAT
ELIKZ = (space)
LOEKZ = (space)
```
**Purpose:** Review schedule lines linked to a specific requisition over the last 7 days by requisition change date, excluding completed and deleted requisition lines. Useful for requisition follow-up.

**Use Case 4: Duration filter – orders open longer than 5 days**
```
BACKDAYS = 90
DATE_REF_FLD = BEDAT
DURATION_UNIT = D
DURATION = 5 (minimum; filter applied on result)
```
**Purpose:** Wide lookback (90 days) by order date with duration in days; use post-processing or result filter to focus on lines open longer than 5 days. Supports aged-order analysis.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_03_OPEN_PR_DET | AEDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_OPEN_PR_DET | AFNAM | Name of Requisitioner/Requester | CHAR(12) | AFNAM |
| /SKN/S_SW_10_03_OPEN_PR_DET | ANLN1 | Main Asset Number | CHAR(12) | ANLN1 |
| /SKN/S_SW_10_03_OPEN_PR_DET | ANLN2 | Asset Subnumber | CHAR(4) | ANLN2 |
| /SKN/S_SW_10_03_OPEN_PR_DET | AUFNR | Order Number | CHAR(12) | AUFNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | BADAT | Requisition (Request) Date | DATS(8) | BADAT |
| /SKN/S_SW_10_03_OPEN_PR_DET | BANFN | Purchase Requisition Number | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_OPEN_PR_DET | BATXT | Short Description of Purchasing Document Type | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | BEDAT | Order date of schedule line | DATS(8) | ETBDT |
| /SKN/S_SW_10_03_OPEN_PR_DET | BNFPO | Item Number of Purchase Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_OPEN_PR_DET | BPUMN | Denominator for Conv. of Order Price Unit into Order Unit | DEC(5) | BPUMN |
| /SKN/S_SW_10_03_OPEN_PR_DET | BPUMZ | Numerator for Conversion of Order Price Unit into Order Unit | DEC(5) | BPUMZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | BSART | Purchasing Document Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_OPEN_PR_DET | BSTYP | Purchasing Document Category | CHAR(1) | BSTYP |
| /SKN/S_SW_10_03_OPEN_PR_DET | BSTYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_OPEN_PR_DET | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_03_OPEN_PR_DET | BWTTY | Valuation Category | CHAR(1) | BWTTY_D |
| /SKN/S_SW_10_03_OPEN_PR_DET | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_03_OPEN_PR_DET | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_OPEN_PR_DET | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_OPEN_PR_DET | EBELN | Purchasing Document Number | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_OPEN_PR_DET | EBELP | Item Number of Purchasing Document | NUMC(5) | EBELP |
| /SKN/S_SW_10_03_OPEN_PR_DET | EINDT | Item Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_OPEN_PR_DET | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_OPEN_PR_DET | EKNAM | Description of purchasing group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_OPEN_PR_DET | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_OPEN_PR_DET | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_OPEN_PR_DET | ELIKZ | "Delivery Completed" Indicator | CHAR(1) | ELIKZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | ERDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_03_OPEN_PR_DET | EREKZ | Final Invoice Indicator | CHAR(1) | EREKZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_OPEN_PR_DET | ESTKZ | Creation Indicator (Purchase Requisition/Schedule Lines) | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | ETENR | Delivery Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_03_OPEN_PR_DET | FIPOS | Commitment Item | CHAR(14) | FIPOS |
| /SKN/S_SW_10_03_OPEN_PR_DET | FRGKE | Release Indicator: Purchasing Document | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_OPEN_PR_DET | FRGKZ | Release Indicator | CHAR(1) | FRGKZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | GJAHR | Material Document Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_03_OPEN_PR_DET | GL_ACC_TXT | G/L Account Long Text | CHAR(50) | TXT50_SKAT |
| /SKN/S_SW_10_03_OPEN_PR_DET | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_03_OPEN_PR_DET | KNTTP | Account Assignment Category | CHAR(1) | KNTTP |
| /SKN/S_SW_10_03_OPEN_PR_DET | KOKRS | Controlling Area | CHAR(4) | CACCD |
| /SKN/S_SW_10_03_OPEN_PR_DET | KOSTL | Cost Center | CHAR(10) | KOSTL |
| /SKN/S_SW_10_03_OPEN_PR_DET | KOSTL_DESC | Description | CHAR(40) | KLTXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | LFDAT | Item Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_OPEN_PR_DET | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | LOEKZ | Deletion Indicator in Purchasing Document | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_OPEN_PR_DET | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_03_OPEN_PR_DET | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | MEINS | Purchase Order Unit of Measure | UNIT(3) | BSTME |
| /SKN/S_SW_10_03_OPEN_PR_DET | MENGE | Scheduled Quantity | QUAN(13,3) | ETMEN |
| /SKN/S_SW_10_03_OPEN_PR_DET | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_OPEN_PR_DET | NETWR | Net Order Value in PO Currency | CURR(13,2) | BWERT |
| /SKN/S_SW_10_03_OPEN_PR_DET | OPEN_ORDER_QUAN | Open Quantity | QUAN(13,3) | OBMNG |
| /SKN/S_SW_10_03_OPEN_PR_DET | PEINH | Price Unit | DEC(5) | EPEIN |
| /SKN/S_SW_10_03_OPEN_PR_DET | PLIFZ | Planned Delivery Time in Days | DEC(3) | PLIFZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | PRCTR | Profit Center | CHAR(10) | PRCTR |
| /SKN/S_SW_10_03_OPEN_PR_DET | PRCTR_DESC | Long Text | CHAR(40) | LTEXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | PROCSTAT | Purchasing document processing state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_OPEN_PR_DET | PSTYP | Item Category in Purchasing Document | CHAR(1) | PSTYP |
| /SKN/S_SW_10_03_OPEN_PR_DET | PS_PSP_PNR | Work Breakdown Structure Element (WBS Element) | NUMC(8) | PS_PSP_PNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | SAKTO | G/L Account Number | CHAR(10) | SAKNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | SHKZG | Debit/Credit Indicator | CHAR(1) | SHKZG |
| /SKN/S_SW_10_03_OPEN_PR_DET | STATU | Status of Purchasing Document | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_OPEN_PR_DET | STATU_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | TXZ01 | Short Text | CHAR(40) | TXZ01 |
| /SKN/S_SW_10_03_OPEN_PR_DET | UEBTK | Indicator: Unlimited Overdelivery Allowed | CHAR(1) | UEBTK |
| /SKN/S_SW_10_03_OPEN_PR_DET | UEBTO | Overdelivery Tolerance Limit | DEC(3,1) | UEBTO |
| /SKN/S_SW_10_03_OPEN_PR_DET | VBELN | Sales and Distribution Document Number | CHAR(10) | VBELN_CO |
| /SKN/S_SW_10_03_OPEN_PR_DET | VBELP | Sales Document Item | NUMC(6) | POSNR_CO |
| /SKN/S_SW_10_03_OPEN_PR_DET | VBUND | Company ID of trading partner | CHAR(6) | RASSC |
| /SKN/S_SW_10_03_OPEN_PR_DET | VGABE | Transaction/event type, purchase order history | CHAR(1) | VGABE |
| /SKN/S_SW_10_03_OPEN_PR_DET | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_OPEN_PR_DET | WEMNG | Quantity of Goods Received | QUAN(13,3) | WEEMG |
| /SKN/S_SW_10_03_OPEN_PR_DET | WEPOS | Goods Receipt Indicator | CHAR(1) | WEPOS |
| /SKN/S_SW_10_03_OPEN_PR_DET | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_OPEN_PR_DET | WGBEZ | Material Group Description | CHAR(20) | WGBEZ |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_OPEN_PR_DET .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_OPEN_PR_DET OPTIONAL
*"----------------------------------------------------------------------
*TYPES: BEGIN OF ty_eket,
*         ebeln TYPE eket-ebeln,
*         ebelp TYPE eket-ebelp,
*         etenr TYPE eket-etenr,
*         eindt TYPE eket-eindt,
*         menge TYPE eket-menge,
*         wemng TYPE eket-wemng,
*         banfn TYPE eket-banfn,
*         bnfpo TYPE eket-bnfpo,
*         estkz TYPE eket-estkz,
*       END OF ty_eket,
*       tt_eket TYPE STANDARD TABLE OF ty_eket.
  DATA: BEGIN OF EKKO.
          INCLUDE STRUCTURE EKKO.
  DATA:   EBELP TYPE EKPO-EBELP,
          MATNR TYPE EKPO-MATNR,
          VBUND TYPE LFA1-VBUND.
  DATA: END OF EKKO.
  DATA: LS_EKKO LIKE EKKO.
  DATA: LT_EKKO LIKE TABLE OF EKKO.
  DATA: LT_EKKN TYPE TABLE OF EKKN.
  DATA: LS_EKKN TYPE EKKN.
DATA_SINGLE: LANGU          LANGU,
             BACKDAYS       INT4,
             DATE_REF_FLD   NAME_FELD,
             ELIKZ          ELIKZ,
             LOEKZ          ELOEK,
             WEPOS          WEPOS,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
*
*
 LV_BACKDAYS      = 10.
 LV_DURATION_UNIT = 'D'.
 LV_DATE_REF_FLD  = 'BEDAT'. "PO date
 LV_ELIKZ         = SPACE.
 LV_LOEKZ         = SPACE.
 LV_WEPOS         = 'X'.
*
 SELECT_SINGLE: LANGU,
                BACKDAYS,
                DATE_REF_FLD,
                DURATION_UNIT.
*
*
DATA_MULTY: EBELN           EBELN,
            EBELP           EBELP,
            FRGKZ           FRGKZ,
            FRGKE           FRGKE,
            BUKRS           BUKRS,
            BSART           BSART,
            LOEKZ           ELOEK,
            STATU           ESTAK,
            AEDAT           ERDAT,
            ERNAM           ERNAM,
            LIFNR           ELIFN,
            EKORG           EKORG,
            EKGRP           BKGRP,
            WAERS           WAERS,
            MATNR           MATNR,
            WERKS           EWERK,
            MATKL           MATKL,
            KNTTP           KNTTP,
            BWTAR           BWTAR_D,
            BWTTY           BWTTY_D,
            ELIKZ           ELIKZ,
            EREKZ           EREKZ,
            PSTYP           PSTYP,
            FIPOS           FIPOS,
            WEPOS           WEPOS,
            BEDAT           ETBDT,
            EINDT           EINDT,
            BANFN           BANFN,
            BNFPO           BNFPO,
            ESTKZ           ESTKZ,
            VBUND           RASSC,
            UEBTO           UEBTO,
            UEBTK           UEBTK,
            SAKTO           SAKTO,
            GSBER           GSBER,
            KOSTL           KOSTL,
            VBELN           VBELN,
            VBELP           POSNR_VA,
            ANLN1           ANLN1,
            ANLN2           ANLN2,
            AUFNR           AUFNR,
            PRCTR           PRCTR,
            BADAT           BADAT,
            LFDAT           EINDT,
            ERDAT           AEDAT,
            PROCSTAT        MEPROCSTATE,
            WEMNG           WEEMG,
            OPEN_ORDER_QUAN OBMNG,
            PS_PSP_PNR      PS_PSP_PNR,
            DATUM           SY-DATUM,
            DURATION        /SKN/E_SW_DURATION.
*
SELECT_MULTY: EBELN,
              EBELP,
              FRGKZ,
              FRGKE,
              BUKRS,
              BSART,
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
              SAKTO,
              GSBER,
              KOSTL,
              VBELN,
              VBELP,
              ANLN1,
              ANLN2,
              AUFNR,
              PRCTR,
              BADAT,
              LFDAT,
              ERDAT,
              PROCSTAT,
              WEMNG,
              OPEN_ORDER_QUAN,
              PS_PSP_PNR,
              DATUM,
              DURATION.
*
CONVERT_MULTY: EBELN ALPHA,
               LIFNR ALPHA.
*
  DATA: DATE_FROM TYPE SY-DATUM.
  DATA: SY_TABIX LIKE SY-TABIX .
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: LV_WEMNG     TYPE EKET-WEMNG,
        LV_MENGE     TYPE EKET-MENGE,
        LV_DOMNAME   TYPE DD07V-DOMNAME,
        LV_DOMVALUE  TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT    TYPE DD07V-DDTEXT,
        LV_VAL_TMP1  TYPE P DECIMALS 3,
        LV_VAL_TMP2  TYPE P DECIMALS 3.
  DATA: LS_DATA LIKE LINE OF T_DATA[].
  FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF T_DATA[],
                       TYPE ANY.
*
*"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_OPEN_PR_DET'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
*
** Initial Date
   IF R_DATUM[] IS INITIAL .
     RS_DATUM-SIGN   = 'I' .
     RS_DATUM-OPTION = 'GT' .
     DATE_FROM       = SY-DATUM - LV_BACKDAYS.
     RS_DATUM-LOW    = DATE_FROM.
*     rs_datum-high   = sy-datum.
     APPEND RS_DATUM TO R_DATUM.
   ENDIF.
*
  IF R_ELIKZ[] IS INITIAL.
    REFRESH: R_ELIKZ[].
    CLEAR RS_ELIKZ.
    RS_ELIKZ-SIGN   = 'I'.
    RS_ELIKZ-OPTION = 'EQ'.
    RS_ELIKZ-LOW    = LV_ELIKZ.
    APPEND RS_ELIKZ TO R_ELIKZ[].
  ENDIF.
  IF R_LOEKZ[] IS INITIAL.
    REFRESH: R_LOEKZ[].
    CLEAR RS_LOEKZ.
    RS_LOEKZ-SIGN   = 'I'.
    RS_LOEKZ-OPTION = 'EQ'.
    RS_LOEKZ-LOW    = LV_LOEKZ.
    APPEND RS_LOEKZ TO R_LOEKZ[].
  ENDIF.
  IF R_WEPOS[] IS INITIAL.
    REFRESH: R_WEPOS[].
    CLEAR RS_WEPOS.
    RS_WEPOS-SIGN   = 'I'.
    RS_WEPOS-OPTION = 'EQ'.
    RS_WEPOS-LOW    = LV_WEPOS.
    APPEND RS_WEPOS TO R_WEPOS[].
  ENDIF.
*
  IF LV_LANGU IS INITIAL.
    LV_LANGU = SY-LANGU.
  ENDIF.
*
* "--- Set Reference Date Field
   CASE LV_DATE_REF_FLD.
     WHEN 'BEDAT'.
       R_BEDAT[] = R_DATUM[]. " Purchasing document
     WHEN 'AEDAT'.
       R_AEDAT = R_DATUM[].   " Document created
     WHEN 'EINDT'.
       R_EINDT[] = R_DATUM[]. " Item Delivery
     WHEN 'ERDAT'.
       R_ERDAT[] = R_DATUM[].
     WHEN 'BADAT'.
       R_BADAT[] = R_DATUM[].
     WHEN OTHERS.
       R_BEDAT[] = R_DATUM[]. " Purchasing document
   ENDCASE.
*
**--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
*
  SELECT *
    FROM EKET AS E LEFT JOIN EKKN AS KN ON E~EBELN EQ KN~EBELN
                                        AND E~EBELP EQ KN~EBELP
                   INNER JOIN EBAN AS A ON  E~BANFN EQ A~BANFN
                                        AND E~BNFPO EQ A~BNFPO
*                   INNER JOIN ekko AS k       ON  e~ebeln EQ k~ebeln
*                   INNER JOIN ekpo AS p       ON  e~ebeln EQ p~ebeln
*                                              AND e~ebelp EQ p~ebelp
*                   INNER JOIN lfa1 AS l ON  k~lifnr EQ l~lifnr
    INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
    WHERE E~BANFN IN R_BANFN[]
    AND   E~BNFPO IN R_BNFPO[]
    AND   E~EBELN IN R_EBELN[]
    AND   E~EBELP IN R_EBELP[]
    AND   E~EINDT IN R_EINDT[]
    AND   E~WEMNG IN R_WEMNG[]
    AND   A~FRGKZ IN R_FRGKZ[]
    AND   A~ESTKZ IN R_ESTKZ[]
    AND   A~ERDAT IN R_ERDAT[]
    AND   A~BADAT IN R_BADAT[]
    AND   A~LFDAT IN R_LFDAT[]
    AND   A~LOEKZ IN R_LOEKZ[]
    AND   A~ERNAM IN R_ERNAM[].
*    AND   k~bukrs IN r_bukrs[]
*    AND   k~bsart IN r_bsart[]
*    AND   k~loekz IN r_loekz[]
*    AND   k~statu IN r_statu[]
*    AND   k~aedat IN r_aedat[]
*    AND   k~bedat IN r_bedat[]
*    AND   k~ernam IN r_ernam[]
*    AND   k~lifnr IN r_lifnr[]
*    AND   k~ekorg IN r_ekorg[]
*    AND   k~ekgrp IN r_ekgrp[]
*    AND   k~frgke IN r_frgke[]
*    AND   k~procstat IN r_procstat[]
*    AND   p~matnr IN r_matnr[]
*    AND   p~werks IN r_werks[]
*    AND   p~matkl IN r_matkl[]
*    AND   p~knttp IN r_knttp[]
*    AND   p~bwtar IN r_bwtar[]
*    AND   p~bwtty IN r_bwtty[]
*    AND   p~elikz IN r_elikz[]
*    AND   p~erekz IN r_erekz[]
*    AND   p~pstyp IN r_pstyp[]
*    AND   p~fipos IN r_fipos[]
*    AND   p~wepos IN r_wepos[]
*    AND   p~loekz IN r_loekz[]
*    AND   p~uebto IN r_uebto[]
*    AND   p~uebtk IN r_uebtk[]
*    AND   l~vbund IN r_vbund[].
  CHECK T_DATA[] IS NOT INITIAL.
  DELETE T_DATA[] WHERE SAKTO      NOT IN R_SAKTO.
  DELETE T_DATA[] WHERE GSBER      NOT IN R_GSBER.
  DELETE T_DATA[] WHERE KOSTL      NOT IN R_KOSTL.
  DELETE T_DATA[] WHERE VBELN      NOT IN R_VBELN.
  DELETE T_DATA[] WHERE VBELP      NOT IN R_VBELP.
  DELETE T_DATA[] WHERE ANLN1      NOT IN R_ANLN1.
  DELETE T_DATA[] WHERE ANLN2      NOT IN R_ANLN2.
  DELETE T_DATA[] WHERE AUFNR      NOT IN R_AUFNR.
  DELETE T_DATA[] WHERE PRCTR      NOT IN R_PRCTR.
  DELETE T_DATA[] WHERE PS_PSP_PNR NOT IN R_PS_PSP_PNR.
  IF T_DATA[] IS NOT INITIAL.
    SORT T_DATA[] BY EBELN EBELP.
    SELECT *
      FROM EKKO AS K INNER JOIN EKPO AS P ON K~EBELN EQ P~EBELN
                     INNER JOIN LFA1 AS L ON K~LIFNR EQ L~LIFNR
      INTO CORRESPONDING FIELDS OF TABLE LT_EKKO
      FOR ALL ENTRIES IN T_DATA[]
      WHERE P~EBELN EQ T_DATA-EBELN
      AND   P~EBELP EQ T_DATA-EBELP
      AND   P~MATNR IN R_MATNR[]
      AND   P~WERKS IN R_WERKS[]
      AND   P~MATKL IN R_MATKL[]
      AND   P~KNTTP IN R_KNTTP[]
      AND   P~BWTAR IN R_BWTAR[]
      AND   P~BWTTY IN R_BWTTY[]
*      AND   p~elikz IN r_elikz[]
      AND   P~EREKZ IN R_EREKZ[]
      AND   P~PSTYP IN R_PSTYP[]
      AND   P~FIPOS IN R_FIPOS[]
      AND   P~WEPOS IN R_WEPOS[]
*      AND   p~loekz IN r_loekz[]
      AND   P~UEBTO IN R_UEBTO[]
      AND   P~UEBTK IN R_UEBTK[]
      AND   K~BUKRS IN R_BUKRS[]
      AND   K~BSART IN R_BSART[]
*      AND   k~loekz IN r_loekz[]
      AND   K~STATU IN R_STATU[]
      AND   K~AEDAT IN R_AEDAT[]
      AND   K~BEDAT IN R_BEDAT[]
*      AND   k~ernam IN r_ernam[]
      AND   K~LIFNR IN R_LIFNR[]
      AND   K~EKORG IN R_EKORG[]
      AND   K~EKGRP IN R_EKGRP[]
      AND   K~FRGKE IN R_FRGKE[]
      AND   K~PROCSTAT IN R_PROCSTAT[]
      AND   L~VBUND IN R_VBUND[].
  ENDIF.
  IF LT_EKKO IS NOT INITIAL.
    SORT LT_EKKO BY EBELN EBELP.
  ENDIF.
***********************************************************************************
*
**-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    CHECK FLD IS NOT INITIAL.
    ASSIGN (FLD) TO .
    CHECK  IS ASSIGNED.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
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
          T_DATA-DURATION  = TIME_DIFF .
        ELSE.
          T_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION.
*******************************************************************************
*  CLEAR: sy_tabix.
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    CLEAR: LS_EKKO.
*
    SY_TABIX = SY-TABIX.
*    CLEAR: lv_menge, lv_wemng, lv_val_tmp1, lv_val_tmp2.
*
*    LOOP AT lt_eket INTO ls_eket WHERE ebeln EQ <fs_data>-ebeln
*                                 AND   ebelp EQ <fs_data>-ebelp.
**                                 AND   eindt EQ <fs_data>-eindt.
*
*        lv_menge     = lv_menge + ls_eket-menge.
*        lv_wemng     = lv_wemng + ls_eket-wemng.
*
*    ENDLOOP.
    READ TABLE LT_EKKO INTO LS_EKKO WITH KEY EBELN = <FS_DATA>-EBELN
                                             EBELP = <FS_DATA>-EBELP
                                             BINARY SEARCH.
    IF SY-SUBRC = 0 .
      <FS_DATA>-LIFNR = LS_EKKO-LIFNR.
      <FS_DATA>-PROCSTAT = LS_EKKO-PROCSTAT.
      <FS_DATA>-STATU    = LS_EKKO-STATU.
      <FS_DATA>-AEDAT    = LS_EKKO-AEDAT.
      <FS_DATA>-BEDAT    = LS_EKKO-BEDAT.
      <FS_DATA>-VBUND    = LS_EKKO-VBUND.
    ELSE.
      DELETE T_DATA INDEX SY_TABIX.
      CONTINUE.
    ENDIF.
    IF <FS_DATA>-MENGE > <FS_DATA>-WEMNG.
      <FS_DATA>-OPEN_ORDER_QUAN = <FS_DATA>-MENGE - <FS_DATA>-WEMNG.
    ELSE.
      <FS_DATA>-OPEN_ORDER_QUAN = 0.
    ENDIF.
    IF R_OPEN_ORDER_QUAN[] IS NOT INITIAL.
      IF NOT <FS_DATA>-OPEN_ORDER_QUAN IN R_OPEN_ORDER_QUAN[].
        DELETE T_DATA INDEX SY_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.
*    READ TABLE lt_ekkn INTO ls_ekkn WITH KEY ebeln = <fs_data>-ebeln
*                                             ebelp = <fs_data>-ebelp
*                                             BINARY SEARCH.
*    IF sy-subrc = 0.
*      IF ls_ekkn-sakto IN r_sakto[] OR r_sakto[] IS INITIAL.
*        <fs_data>-sakto = ls_ekkn-sakto.
*      ELSE.
*        DELETE t_data WHERE sakto      NOT IN r_sakto.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-gsber IN r_gsber[] OR r_gsber[] IS INITIAL.
*        <fs_data>-gsber = ls_ekkn-gsber.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-kostl IN r_kostl[] OR r_kostl[] IS INITIAL.
*        <fs_data>-kostl = ls_ekkn-kostl.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-vbeln IN r_vbeln[] OR r_vbeln[] IS INITIAL.
*        <fs_data>-vbeln = ls_ekkn-vbeln.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-vbelp IN r_vbelp[] OR r_vbelp[] IS INITIAL.
*        <fs_data>-vbelp = ls_ekkn-vbelp.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-anln1 IN r_anln1[] OR r_anln1[] IS INITIAL.
*        <fs_data>-anln1 = ls_ekkn-anln1.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-anln2 IN r_anln2[] OR r_anln2[] IS INITIAL.
*        <fs_data>-anln2 = ls_ekkn-anln2.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-aufnr IN r_aufnr[] OR r_aufnr[] IS INITIAL.
*        <fs_data>-aufnr = ls_ekkn-aufnr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-prctr IN r_prctr[] OR r_prctr[] IS INITIAL.
*        <fs_data>-prctr = ls_ekkn-prctr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-ps_psp_pnr IN r_ps_psp_pnr[] OR r_ps_psp_pnr[] IS INITIAL.
*        <fs_data>-ps_psp_pnr = ls_ekkn-ps_psp_pnr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*    ENDIF.
*    IF lv_menge <= lv_wemng.
*      DELETE t_data[] INDEX sy_tabix.
*      CONTINUE.
*    ENDIF.
*
*    <fs_data>-waers_local     = <fs_data>-waers.
*    <fs_data>-menge           = lv_menge.
*    <fs_data>-wemng           = lv_wemng.
*    <fs_data>-open_order_quan = lv_menge - lv_wemng.
*
*    IF <fs_data>-bpumn <> 0 AND <fs_data>-peinh <> 0.
*      lv_val_tmp1 = <fs_data>-netpr * ( <fs_data>-open_order_quan ).
*      lv_val_tmp2 = ( <fs_data>-bpumz / <fs_data>-bpumn ) /
*                      <fs_data>-peinh.
*
*      <fs_data>-open_value  = lv_val_tmp1 * lv_val_tmp2.
*    ELSE.
*      <fs_data>-open_value = 0.
*    ENDIF.
    IF <FS_DATA>-SAKTO IS NOT INITIAL AND <FS_DATA>-BUKRS IS NOT INITIAL.
* G/L Account Description
    DATA: LV_ACC_DESC TYPE  TXT20_SKAT.
      CALL FUNCTION '/SKN/F_SW_10_SAKTO_DESC'
        EXPORTING
          SPRAS            = SY-LANGU
          BUKRS            = <FS_DATA>-BUKRS
*         KTOPL            =
          SAKNR            = <FS_DATA>-SAKTO
       IMPORTING
         ACC_DESC         = LV_ACC_DESC
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-GL_ACC_TXT = LV_ACC_DESC.
    ENDIF.
    IF <FS_DATA>-KOSTL IS NOT INITIAL AND <FS_DATA>-KOKRS IS NOT INITIAL.
* Cost Center Description
     DATA: LV_KTEXT TYPE  KTEXT.
      CALL FUNCTION '/SKN/F_SW_10_KOSTL_DESC'
        EXPORTING
         SPRAS           = SY-LANGU
         KOKRS           = <FS_DATA>-KOKRS
         KOSTL           = <FS_DATA>-KOSTL
       IMPORTING
         KOSTL_DESC      = LV_KTEXT
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-KOSTL_DESC = LV_KTEXT.
    ENDIF.
    IF <FS_DATA>-PRCTR IS NOT INITIAL AND <FS_DATA>-KOKRS IS NOT INITIAL.
* Profit Center Description
    DATA: LVV_KTEXT TYPE  KTEXT.
      CALL FUNCTION '/SKN/F_SW_10_PRCTR_DESC'
        EXPORTING
         SPRAS            = SY-LANGU
         PRCTR            = <FS_DATA>-PRCTR
         KOKRS            = <FS_DATA>-KOKRS
       IMPORTING
         KTEXT            = LVV_KTEXT
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-PRCTR_DESC = LVV_KTEXT.
    ENDIF.
    IF <FS_DATA>-MATKL IS NOT INITIAL.
* Material group desc.
      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
      EXPORTING
        MATKL              = <FS_DATA>-MATKL
*       LANGU              = SY-LANGU
      IMPORTING
        MATKL_DESC         = <FS_DATA>-WGBEZ
*       MATKL_DESC_L       =
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2
        .
    ENDIF.
**
    IF <FS_DATA>-BSART IS NOT INITIAL AND <FS_DATA>-BSTYP IS NOT INITIAL.
*    "-- BSART_DESC
      CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART            = <FS_DATA>-BSART
        LANGU            = LV_LANGU
        BSTYP            = <FS_DATA>-BSTYP
      IMPORTING
        TYPE_DESC        = <FS_DATA>-BATXT
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    ENDIF.
*
    IF <FS_DATA>-STATU IS NOT INITIAL.
      "-- STATU_DESC
      LV_DOMNAME = 'ESTAK'.
      LV_DOMVALUE = <FS_DATA>-STATU.
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
        <FS_DATA>-STATU_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
**
    IF <FS_DATA>-BSTYP IS NOT INITIAL.
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
        <FS_DATA>-BSTYP_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
**
    IF <FS_DATA>-LIFNR IS NOT INITIAL.
*    "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR              = <FS_DATA>-LIFNR
      IMPORTING
        VENDOR_DESC        = <FS_DATA>-NAME1
      EXCEPTIONS
        WRONG_VENDOR       = 1
        OTHERS             = 2.
    ENDIF.
**
    IF <FS_DATA>-EKORG IS NOT INITIAL.
*   "-- EKORG_DESC
      CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG              = <FS_DATA>-EKORG
        "LANGU              = lv_LANGU
      IMPORTING
        PUR_ORG_DESC       = <FS_DATA>-EKOTX
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    ENDIF.
**
**
    IF <FS_DATA>-EKGRP IS NOT INITIAL.
*   "-- EKGRP_DESC
      CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP              = <FS_DATA>-EKGRP
*       LANGU              = lv_LANGU
      IMPORTING
        PUR_GRP_DESC       = <FS_DATA>-EKNAM
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    ENDIF.
*
  ENDLOOP.
*
**
***--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
