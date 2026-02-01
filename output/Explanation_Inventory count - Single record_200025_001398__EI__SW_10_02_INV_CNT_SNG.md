# Exception Indicator: Inventory count - Single record - SW_10_02_INV_CNT_SNG

## General Overview

This Exception Indicator (EI) monitors physical inventory count data in SAP Materials Management (MM) to identify inventory count documents and differences within a configurable time window. It provides visibility into count status, posting status, value and quantity differences, and supports single-record or aggregated analysis by plant, storage location, and other dimensions.

This EI serves as an essential control for inventory accuracy and period-end close by:
- Enabling detection of physical inventory count documents and differences that require adjustment posting or management review
- Supporting identification of value and quantity variances for reconciliation and audit
- Providing visibility into count status and posting status for prioritization and root-cause analysis
- Enabling analysis by plant, storage location, and material for operational and cycle-count oversight
- Supporting accountability by count date and user for process control and exception management

The EI is valuable for period-end inventory close, cycle count monitoring, and exception management where count documents and differences must be reviewed and resolved in a timely manner.

The EI uses physical inventory and material document data from SAP MM (e.g. ISEG, MSEG, MKOL) and supports filtering by plant, storage location, count date, and status.


## Problem Description

Failure to monitor physical inventory count documents and differences creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unreconciled count differences can distort inventory valuations and cost of goods sold in financial statements
- Unposted or aged count documents may delay period-end close and inventory reconciliation
- Value and quantity variances left unaddressed can lead to material misstatement in balance sheet and P&L
- Concentrated differences in specific plants or materials may indicate systemic valuation or process issues
- Late detection of count exceptions can require restatements or audit adjustments

**Operational and Control Risks**
- Count documents stuck in status without visibility may block cycle count completion and stock accuracy reviews
- Unmonitored differences by storage location or material can mask receiving, picking, or posting errors
- Lack of visibility into count status duration may delay release of blocked items or escalation
- Inadequate tracking of who counted and when can weaken process accountability
- Value-only or special-stock counts may be overlooked without targeted monitoring

**Management Visibility and Decision-Making Risks**
- Lack of consolidated visibility into count exceptions delays management awareness of inventory risks
- Unidentified concentration of differences by plant or material can lead to missed corrective actions
- Count status and age not monitored may result in delayed prioritization and root-cause analysis
- Audit and compliance reviews may be hindered when count documentation and resolution are not traceable

## Suggested Resolution

**Immediate Response**
- Review flagged count documents and differences for high-value or high-quantity variances
- Verify status of adjustment postings and unblock or correct where appropriate
- Check count date and reference date logic to ensure the correct population is in scope
- Clarify business context for exceptional differences (e.g. write-offs, transfers, timing)

**System Assessment**
- Compare count differences and document volumes to prior periods and expected patterns
- Assess aggregation level and reference-field logic to determine if exceptions are dimension- or threshold-specific
- Review plant, storage location, and material distribution for concentration or data quality issues
- Validate that count status and posting status align with process expectations

**Corrective Actions**
- Post adjustment documents where differences are justified and approved
- Correct master data or process errors that drive recurring variances
- Adjust monitoring scope (e.g. plants, date window, status) to focus on material exceptions
- Document resolution and schedule recurring runs for ongoing control


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ABCIN | CC phys. inv. ind. | CHAR | 1 | 0 | ABCIN | ABCIN |
| 2 | ABS_DMBTR | Difference amount | CURR | 13 | 2 | DIFWR | WERT7 |
| 3 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 4 | AGG_LVL | Agg.Level | CHAR | 30 | 0 | /SKN/E_SW_AGG_LVL | /SKN/D_SW_AGG_LVL |
| 5 | ATTYP | Material Category | CHAR | 2 | 0 | ATTYP | ATTYP |
| 6 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 7 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 8 | BSTAR | Stock Type | CHAR | 1 | 0 | I_BSTAR | BSTAR |
| 9 | BUCHM | Book quantity | QUAN | 13 | 3 | BUCHM | MENGV13 |
| 10 | BUCHW | Book val. at SP | CURR | 13 | 2 | BUCHW | WERT13N |
| 11 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 12 | CHARG | Batch | CHAR | 10 | 0 | CHARG_D | CHARG |
| 13 | COMP_OPERATOR | Operator | CHAR | 2 | 0 | BUCC_OPERATOR | BUCC_OPERATOR |
| 14 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 15 | DIFF_AMOUNT | difference amount |  | 0 | 0 |  |  |
| 16 | DIWZL | Diff. value | CURR | 13 | 2 | DIWZL | WERT7 |
| 17 | DMBTR | Difference amount | CURR | 13 | 2 | DIFWR | WERT7 |
| 18 | DSTAT | Adjustment status | CHAR | 1 | 0 | DSTAT | DSTAT |
| 19 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 20 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 21 | ERFME | Unit of Entry | UNIT | 3 | 0 | ERFME | MEINS |
| 22 | ERFMG | Qty in un. of entry | QUAN | 13 | 3 | I_ERFMG | MENG13 |
| 23 | EXVKW | Sales Value | CURR | 13 | 2 | EXVKW | WERT7 |
| 24 | GIDAT | Planned count date | DATS | 8 | 0 | GIDAT | DATUM |
| 25 | GJAHR | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 26 | GRUND | Reason for inventory diff. | NUMC | 4 | 0 | GRDIF | MB_GRBEW |
| 27 | IBLNR | Phys. Inventory Doc. | CHAR | 10 | 0 | IBLNR | BELNR |
| 28 | KDAUF | Sales Order | CHAR | 10 | 0 | KDAUF | VBELN |
| 29 | KDEIN | Sales order schedule | NUMC | 4 | 0 | KDEIN | NUM04 |
| 30 | KDPOS | Sales order item | NUMC | 6 | 0 | KDPOS | NUM06 |
| 31 | KUNNR | Customer | CHAR | 10 | 0 | EKUNN | KUNNR |
| 32 | KWART | Inventory val.-only mat | CHAR | 1 | 0 | XWART | XFELD |
| 33 | LANGU | Language |  | 0 | 0 |  |  |
| 34 | LGORT | Storage Location | CHAR | 4 | 0 | LGORT_D | LGORT |
| 35 | LIFNR | Supplier | CHAR | 10 | 0 | ELIFN | LIFNR |
| 36 | LSTAT | "Delete" status | CHAR | 1 | 0 | LSTAT | DSTAT |
| 37 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 38 | MAT_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 39 | MBLNR | Material Document | CHAR | 10 | 0 | MBLNR | BELNR |
| 40 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 41 | MENGE | Quantity | QUAN | 13 | 3 | MENGE_D | MENG13 |
| 42 | MJAHR | Material Doc. Year | NUMC | 4 | 0 | MJAHR | GJAHR |
| 43 | NBLNR | Recount document | CHAR | 10 | 0 | NBLNR | BELNR |
| 44 | PLPLA | Distr. differences | CHAR | 10 | 0 | PLPLA | PLPLA |
| 45 | PRESENT_ZERO | 'X' - Present Zero |  | 0 | 0 |  |  |
| 46 | PS_PSP_PNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 47 | REF_FIELD_NAME1 | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 48 | REF_FIELD_NAME2 | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 49 | RESULT_COMP | Value to Compare | CURR | 15 | 2 |  |  |
| 50 | SOBKZ | Special Stock | CHAR | 1 | 0 | SOBKZ | SOBKZ |
| 51 | SPERR | Posting Block | CHAR | 1 | 0 | ISPER | XFELD |
| 52 | USNAD | Adj. posting made by | CHAR | 12 | 0 | USNAD | USNAM |
| 53 | USNAM | Changed by(Item lvl) | CHAR | 12 | 0 | USNAA | USNAM |
| 54 | USNAM_HD | User name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 55 | USNAZ | Counted By | CHAR | 12 | 0 | USNAZ | USNAM |
| 56 | VGART | Trans./Event Type | CHAR | 2 | 0 | VGART | VGART |
| 57 | VKMZL | Diff.SalesPrs + VAT | CURR | 13 | 2 | VKMZL | WERT7 |
| 58 | VKNZL | Diff.SalesPr w/o VAT | CURR | 13 | 2 | VKNZL | WERT7 |
| 59 | VKWRA | Sales value w/o VAT | CURR | 13 | 2 | VKWRA | WERT7 |
| 60 | VKWRT | Sales Value inc. VAT | CURR | 13 | 2 | VKWRT | WERT7 |
| 61 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 62 | WAERS_FR | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 63 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 64 | WRTBM | Value book qty | CURR | 13 | 2 | WRTBM | WERT7 |
| 65 | WRTZL | Val. cntd qty | CURR | 13 | 2 | WRTZL | WERT7 |
| 66 | XAMEI | Alternative Unit | CHAR | 1 | 0 | XAMEI | XFELD |
| 67 | XBLNI | Phys. Inventory Ref. | CHAR | 16 | 0 | XBLNI1 | XBLNR1 |
| 68 | XBUFI | Freeze book invntory | CHAR | 1 | 0 | XBUFI | XFELD |
| 69 | XDIFF | Difference posted | CHAR | 1 | 0 | XDIFF | XFELD |
| 70 | XLOEK | Item Deleted | CHAR | 1 | 0 | I_XLOEK | XFELD |
| 71 | XNULL | Zero count | CHAR | 1 | 0 | XNULL | XFELD |
| 72 | XNZAE | Recount | CHAR | 1 | 0 | XNZAE | XFELD |
| 73 | XZAEL | Item counted | CHAR | 1 | 0 | XZAEL | XFELD |
| 74 | ZEILE | Material Doc.Item | NUMC | 4 | 0 | MBLPO | MBLPO |
| 75 | ZEILI | Item | NUMC | 3 | 0 | DZEILE | ZEILE |
| 76 | ZLDAT | Count date | DATS | 8 | 0 | DZLDAT | DATUM |
| 77 | ZSTAT | Count status | CHAR | 1 | 0 | DZSTAT | ZSTAT |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 77 parameters listed in the Parameters Reference Table above.

**ABCIN** (CC phys. inv. ind.):

Parameter for cc phys. inv. ind.. Used in selection criteria or output structure as defined in the function.

**ABS_DMBTR** (Difference amount):

Filters or outputs difference amount. Used for threshold comparison or value display.

**AEDAT** (Changed On):

Parameter for changed on. Used in selection criteria or output structure as defined in the function.

**AGG_LVL** (Agg.Level):

Aggregation level for count data (e.g. single record vs aggregated). Controls how the EI groups and returns data. Use to switch between item-level and aggregated view.

**AGG_LVL Options:**

- Values are function-specific; see code or output structure. Use empty for single-record view or set aggregation key as used by the function.

**ATTYP** (Material Category):

Parameter for material category. Used in selection criteria or output structure as defined in the function.

**BACKDAYS** (Days Backward from today):

Number of days to look back from today. When no date range is supplied, the EI builds the selection using the reference date minus BACKDAYS. Used to limit the physical inventory date window.

**BACKDAYS and DATE_REF_FLD Connection:**

DATE_REF_FLD determines which date is used as the reference (e.g. BUDAT, GIDAT, ZLDAT). The selection window is built from that date minus BACKDAYS.

**BLDAT** (Document Date):

Filters or outputs by document date. Used in selection or output structure.

**BSTAR** (Stock Type):

Parameter for stock type. Used in selection criteria or output structure as defined in the function.

**BUCHM** (Book quantity):

Parameter for book quantity. Used in selection criteria or output structure as defined in the function.

**BUCHW** (Book val. at SP):

Parameter for book val. at sp. Used in selection criteria or output structure as defined in the function.

**BUDAT** (Posting Date):

Filters or outputs by posting date. Used in selection or output structure.

**CHARG** (Batch):

Parameter for batch. Used in selection criteria or output structure as defined in the function.

**COMP_OPERATOR** (Operator):

Parameter for operator. Used in selection criteria or output structure as defined in the function.

**DATE_REF_FLD** (Date Ref Field):

Name of the date field used as the reference for the time window (e.g. BUDAT = Posting Date, GIDAT = Planned count date, ZLDAT = Count date). Used with BACKDAYS or DURATION/DURATION_UNIT.

**DATE_REF_FLD Options:** (from code)

- **BUDAT**: Posting Date in the Document (default in code).
- Other date fields from the output structure (e.g. GIDAT, ZLDAT, BLDAT) may be used depending on function logic.

**DIFF_AMOUNT** (difference amount):

Filters or outputs difference amount. Used for threshold comparison or value display.

**DIWZL** (Diff. value):

Filters or outputs diff. value. Used for threshold comparison or value display.

**DMBTR** (Difference amount):

Filters or outputs difference amount. Used for threshold comparison or value display.

**DSTAT** (Adjustment status):

Filters or outputs by adjustment status. Used in selection or output.

**DURATION** (Duration In Time Units):

Duration in the selected time unit (see DURATION_UNIT) between the reference date and current date. Use with DATE_REF_FLD and DURATION_UNIT when filtering by age in status.

**DURATION and DURATION_UNIT Connection:**

DURATION gives the numeric value; DURATION_UNIT gives the unit (e.g. D = days). DATE_REF_FLD determines which date is used as the start. Set all three when filtering by how long documents have been in status.

**DURATION_UNIT** (Duration Unit):

Unit for duration (e.g. days). Used with DURATION and DATE_REF_FLD for duration-based filtering.

**DURATION_UNIT Options:** (from code)

- **D**: Days (default in code).

**ERFME** (Unit of Entry):

Parameter for unit of entry. Used in selection criteria or output structure as defined in the function.

**ERFMG** (Qty in un. of entry):

Parameter for qty in un. of entry. Used in selection criteria or output structure as defined in the function.

**EXVKW** (Sales Value):

Filters or outputs sales value. Used for threshold comparison or value display.

**GIDAT** (Planned count date):

Filters or outputs by planned count date. Used in selection or output structure.

**GJAHR** (Fiscal Year):

Parameter for fiscal year. Used in selection criteria or output structure as defined in the function.

**GRUND** (Reason for inventory diff.):

Parameter for reason for inventory diff.. Used in selection criteria or output structure as defined in the function.

**IBLNR** (Phys. Inventory Doc.):

Parameter for phys. inventory doc.. Used in selection criteria or output structure as defined in the function.

**KDAUF** (Sales Order):

Parameter for sales order. Used in selection criteria or output structure as defined in the function.

**KDEIN** (Sales order schedule):

Parameter for sales order schedule. Used in selection criteria or output structure as defined in the function.

**KDPOS** (Sales order item):

Parameter for sales order item. Used in selection criteria or output structure as defined in the function.

**KUNNR** (Customer):

Parameter for customer. Used in selection criteria or output structure as defined in the function.

**KWART** (Inventory val.-only mat):

Flag or indicator for inventory val.-only mat. Filters or populates output accordingly.

**KWART Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**LANGU** (Language):

Parameter for language. Used in selection criteria or output structure as defined in the function.

**LGORT** (Storage Location):

Parameter for storage location. Used in selection criteria or output structure as defined in the function.

**LIFNR** (Supplier):

Parameter for supplier. Used in selection criteria or output structure as defined in the function.

**LSTAT** ("Delete" status):

Filters or outputs by "delete" status. Used in selection or output.

**MATNR** (Material):

Parameter for material. Used in selection criteria or output structure as defined in the function.

**MAT_DESC** (Material Description):

Parameter for material description. Used in selection criteria or output structure as defined in the function.

**MBLNR** (Material Document):

Parameter for material document. Used in selection criteria or output structure as defined in the function.

**MEINS** (Base Unit of Measure):

Parameter for base unit of measure. Used in selection criteria or output structure as defined in the function.

**MENGE** (Quantity):

Parameter for quantity. Used in selection criteria or output structure as defined in the function.

**MJAHR** (Material Doc. Year):

Parameter for material doc. year. Used in selection criteria or output structure as defined in the function.

**NBLNR** (Recount document):

Parameter for recount document. Used in selection criteria or output structure as defined in the function.

**PLPLA** (Distr. differences):

Parameter for distr. differences. Used in selection criteria or output structure as defined in the function.

**PRESENT_ZERO** ('X' - Present Zero):

Parameter for 'x' - present zero. Used in selection criteria or output structure as defined in the function.

**PS_PSP_PNR** (WBS Element):

Parameter for wbs element. Used in selection criteria or output structure as defined in the function.

**REF_FIELD_NAME1** (Field name):

Parameter for field name. Used in selection criteria or output structure as defined in the function.

**REF_FIELD_NAME2** (Field name):

Parameter for field name. Used in selection criteria or output structure as defined in the function.

**RESULT_COMP** (Value to Compare):

Filters or outputs value to compare. Used for threshold comparison or value display.

**SOBKZ** (Special Stock):

Parameter for special stock. Used in selection criteria or output structure as defined in the function.

**SPERR** (Posting Block):

Flag or indicator for posting block. Filters or populates output accordingly.

**SPERR Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**USNAD** (Adj. posting made by):

Parameter for adj. posting made by. Used in selection criteria or output structure as defined in the function.

**USNAM** (Changed by(Item lvl)):

Parameter for changed by(item lvl). Used in selection criteria or output structure as defined in the function.

**USNAM_HD** (User name):

Parameter for user name. Used in selection criteria or output structure as defined in the function.

**USNAZ** (Counted By):

Parameter for counted by. Used in selection criteria or output structure as defined in the function.

**VGART** (Trans./Event Type):

Parameter for trans./event type. Used in selection criteria or output structure as defined in the function.

**VKMZL** (Diff.SalesPrs + VAT):

Parameter for diff.salesprs + vat. Used in selection criteria or output structure as defined in the function.

**VKNZL** (Diff.SalesPr w/o VAT):

Parameter for diff.salespr w/o vat. Used in selection criteria or output structure as defined in the function.

**VKWRA** (Sales value w/o VAT):

Filters or outputs sales value w/o vat. Used for threshold comparison or value display.

**VKWRT** (Sales Value inc. VAT):

Filters or outputs sales value inc. vat. Used for threshold comparison or value display.

**WAERS** (Currency):

Filters or outputs currency. Used for threshold comparison or value display.

**WAERS_FR** (Currency):

Filters or outputs currency. Used for threshold comparison or value display.

**WERKS** (Plant):

Parameter for plant. Used in selection criteria or output structure as defined in the function.

**WRTBM** (Value book qty):

Filters or outputs value book qty. Used for threshold comparison or value display.

**WRTZL** (Val. cntd qty):

Parameter for val. cntd qty. Used in selection criteria or output structure as defined in the function.

**XAMEI** (Alternative Unit):

Flag or indicator for alternative unit. Filters or populates output accordingly.

**XAMEI Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XBLNI** (Phys. Inventory Ref.):

Parameter for phys. inventory ref.. Used in selection criteria or output structure as defined in the function.

**XBUFI** (Freeze book invntory):

Flag or indicator for freeze book invntory. Filters or populates output accordingly.

**XBUFI Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XDIFF** (Difference posted):

Flag or indicator for difference posted. Filters or populates output accordingly.

**XDIFF Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XLOEK** (Item Deleted):

Flag or indicator for item deleted. Filters or populates output accordingly.

**XLOEK Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XNULL** (Zero count):

Flag or indicator for zero count. Filters or populates output accordingly.

**XNULL Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XNZAE** (Recount):

Flag or indicator for recount. Filters or populates output accordingly.

**XNZAE Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XZAEL** (Item counted):

Flag or indicator for item counted. Filters or populates output accordingly.

**XZAEL Options:**

- **X**: Set/active.
- ** ** (space): Not set/inactive.

**ZEILE** (Material Doc.Item):

Parameter for material doc.item. Used in selection criteria or output structure as defined in the function.

**ZEILI** (Item):

Parameter for item. Used in selection criteria or output structure as defined in the function.

**ZLDAT** (Count date):

Filters or outputs by count date. Used in selection or output structure.

**ZSTAT** (Count status):

Filters or outputs by count status. Used in selection or output.


### Parameter Relationships

**Time and Date Parameters:**
- BACKDAYS, DATE_REF_FLD, and the selected date field (e.g. BUDAT, GIDAT, ZLDAT) work together: BACKDAYS defines how many days to look back from today; DATE_REF_FLD determines which date is used as the reference (e.g. BUDAT = Posting Date). The EI builds the selection window from the reference date minus BACKDAYS.
- DURATION, DURATION_UNIT, and DATE_REF_FLD work together for duration-based filtering: DURATION gives the numeric value, DURATION_UNIT gives the unit (e.g. D = days; from code, D is the only value used), and DATE_REF_FLD determines which date is used as the start for the "age in status" calculation. Set all three when filtering by how long documents have been in a given status.

**Aggregation Parameters:**
- AGG_LVL controls the aggregation level for count data (e.g. single record vs aggregated). It affects how the EI groups and returns data and connects to the output structure and grouping logic in the code.


### Default Values

- **BACKDAYS** — Default: `1`; when not supplied, the EI uses 1 day lookback.
- **DATE_REF_FLD** — Default: `BUDAT` (Posting Date in the Document); when not supplied, posting date is used as the reference for the time window.
- **DURATION_UNIT** — Default: `D` (days); when not supplied, duration is calculated in days.
- **AGG_LVL** — Default: initial (empty); aggregation level when not supplied.
- **DIFF_AMOUNT** — Default: initial (0); difference amount threshold when not supplied.
- **PRESENT_ZERO** — Default: initial (empty); zero counts not presented when not supplied.
- **LANGU** — Default: `E` (English); when not supplied, system or English is used for language-dependent output.

### Practical Configuration Examples

**Use Case 1: Recent count documents by plant**
```
WERKS = 1000
BACKDAYS = 7
DATE_REF_FLD = BUDAT
```
**Purpose:** List physical inventory count documents for plant 1000 with posting date within the last 7 days. Use for daily or weekly count review.

**Use Case 2: Count differences above threshold**
```
DIFF_AMOUNT = 1000
WERKS = 1000
LGORT = 0001
```
**Purpose:** Identify count lines where the difference amount exceeds 1000 in local currency for a specific plant and storage location. Use for exception follow-up.

**Use Case 3: Aggregated view by count date**
```
AGG_LVL = (aggregation level value)
DATE_REF_FLD = ZLDAT
BACKDAYS = 14
WERKS = 1000
```
**Purpose:** Monitor count documents by count date (ZLDAT) in the last 14 days at an aggregated level for plant 1000. Use for period-end or cycle count overview.

**Use Case 4: Status and duration filter**
```
DURATION = 5
DURATION_UNIT = D
DATE_REF_FLD = BUDAT
ZSTAT = (count status value)
```
**Purpose:** Find count documents that have been in a given count status for at least 5 days, using posting date as reference. Use for stuck-document follow-up.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_02_INVENT_CNT | ABCIN | Physical inventory indicator for cycle counting | CHAR(1) | ABCIN |
| /SKN/S_SW_10_02_INVENT_CNT | ABS_DMBTR | Difference amount in local currency | CURR(13,2) | DIFWR |
| /SKN/S_SW_10_02_INVENT_CNT | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_02_INVENT_CNT | AGG_LVL | Agg. level | CHAR(30) | /SKN/E_SW_AGG_LVL |
| /SKN/S_SW_10_02_INVENT_CNT | ATTYP | Material Category | CHAR(2) | ATTYP |
| /SKN/S_SW_10_02_INVENT_CNT | BLDAT | Document Date in Document | DATS(8) | BLDAT |
| /SKN/S_SW_10_02_INVENT_CNT | BSTAR | Stock Type (Physical Inventory | CHAR(1) | I_BSTAR |
| /SKN/S_SW_10_02_INVENT_CNT | BUCHM | Book quantity immediately prior to count | QUAN(13,3) | BUCHM |
| /SKN/S_SW_10_02_INVENT_CNT | BUCHW | Book value based on sales prices (SP) at time of count | CURR(13,2) | BUCHW |
| /SKN/S_SW_10_02_INVENT_CNT | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_02_INVENT_CNT | CHARG | Batch Number | CHAR(10) | CHARG_D |
| /SKN/S_SW_10_02_INVENT_CNT | COMP_OPERATOR | Consistency Checks - Comparison operator | CHAR(2) | BUCC_OPERATOR |
| /SKN/S_SW_10_02_INVENT_CNT | DIWZL | Inventory difference value upon count entry | CURR(13,2) | DIWZL |
| /SKN/S_SW_10_02_INVENT_CNT | DMBTR | Difference amount in local currency | CURR(13,2) | DIFWR |
| /SKN/S_SW_10_02_INVENT_CNT | DSTAT | Status of adjustment posting (inventory differences) | CHAR(1) | DSTAT |
| /SKN/S_SW_10_02_INVENT_CNT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_02_INVENT_CNT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_02_INVENT_CNT | ERFME | Unit of Entry | UNIT(3) | ERFME |
| /SKN/S_SW_10_02_INVENT_CNT | ERFMG | Quantity in unit of entry (physical inventory) | QUAN(13,3) | I_ERFMG |
| /SKN/S_SW_10_02_INVENT_CNT | EXVKW | Externally Entered Sales Value in Local Currency | CURR(13,2) | EXVKW |
| /SKN/S_SW_10_02_INVENT_CNT | GIDAT | Planned date of inventory count | DATS(8) | GIDAT |
| /SKN/S_SW_10_02_INVENT_CNT | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /SKN/S_SW_10_02_INVENT_CNT | GRUND | Reason for inventory diff. | NUMC(4) | GRDIF |
| /SKN/S_SW_10_02_INVENT_CNT | IBLNR | Physical Inventory Document | CHAR(10) | IBLNR |
| /SKN/S_SW_10_02_INVENT_CNT | KDAUF | Sales Order Number | CHAR(10) | KDAUF |
| /SKN/S_SW_10_02_INVENT_CNT | KDEIN | Delivery Schedule for Sales Order | NUMC(4) | KDEIN |
| /SKN/S_SW_10_02_INVENT_CNT | KDPOS | Item Number in Sales Order | NUMC(6) | KDPOS |
| /SKN/S_SW_10_02_INVENT_CNT | KUNNR | Account Number of Customer | CHAR(10) | EKUNN |
| /SKN/S_SW_10_02_INVENT_CNT | KWART | Indicates that the value-only material is to be inventoried | CHAR(1) | XWART |
| /SKN/S_SW_10_02_INVENT_CNT | LGORT | Storage Location | CHAR(4) | LGORT_D |
| /SKN/S_SW_10_02_INVENT_CNT | LIFNR | Vendor Account Number | CHAR(10) | ELIFN |
| /SKN/S_SW_10_02_INVENT_CNT | LSTAT | Status of delete flag | CHAR(1) | LSTAT |
| /SKN/S_SW_10_02_INVENT_CNT | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_INVENT_CNT | MAT_DESC | Material Description (Short Text) | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_INVENT_CNT | MBLNR | Number of Material Document | CHAR(10) | MBLNR |
| /SKN/S_SW_10_02_INVENT_CNT | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_02_INVENT_CNT | MENGE | Quantity | QUAN(13,3) | MENGE_D |
| /SKN/S_SW_10_02_INVENT_CNT | MJAHR | Material Document Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_02_INVENT_CNT | NBLNR | Number of recount document | CHAR(10) | NBLNR |
| /SKN/S_SW_10_02_INVENT_CNT | PLPLA | Distribution of Differences | CHAR(10) | PLPLA |
| /SKN/S_SW_10_02_INVENT_CNT | PS_PSP_PNR | Work Breakdown Structure Element (WBS Element) | NUMC(8) | PS_PSP_PNR |
| /SKN/S_SW_10_02_INVENT_CNT | REF_FIELD_NAME1 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_02_INVENT_CNT | REF_FIELD_NAME2 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_02_INVENT_CNT | RESULT_COMP |  | CURR(15,2) |  |
| /SKN/S_SW_10_02_INVENT_CNT | SOBKZ | Special Stock Indicator | CHAR(1) | SOBKZ |
| /SKN/S_SW_10_02_INVENT_CNT | SPERR | Posting block due to physical inventory | CHAR(1) | ISPER |
| /SKN/S_SW_10_02_INVENT_CNT | USNAD | Adjustment posting made by | CHAR(12) | USNAD |
| /SKN/S_SW_10_02_INVENT_CNT | USNAM | Changed by | CHAR(12) | USNAA |
| /SKN/S_SW_10_02_INVENT_CNT | USNAM_HD | User name | CHAR(12) | USNAM |
| /SKN/S_SW_10_02_INVENT_CNT | USNAZ | Counted By | CHAR(12) | USNAZ |
| /SKN/S_SW_10_02_INVENT_CNT | VGART | Transaction/Event Type | CHAR(2) | VGART |
| /SKN/S_SW_10_02_INVENT_CNT | VKMZL | Sales Value of Inv. Differences (with VAT) on Count Entry | CURR(13,2) | VKMZL |
| /SKN/S_SW_10_02_INVENT_CNT | VKNZL | Sales Value of Inventory Difference (w/o VAT) on Count Entry | CURR(13,2) | VKNZL |
| /SKN/S_SW_10_02_INVENT_CNT | VKWRA | Value at sales prices excluding value-added tax | CURR(13,2) | VKWRA |
| /SKN/S_SW_10_02_INVENT_CNT | VKWRT | Value at Sales Prices Including Value-Added Tax | CURR(13,2) | VKWRT |
| /SKN/S_SW_10_02_INVENT_CNT | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_02_INVENT_CNT | WAERS_FR | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_02_INVENT_CNT | WERKS | Plant | CHAR(4) | WERKS_D |
| /SKN/S_SW_10_02_INVENT_CNT | WRTBM | Value of book quantity | CURR(13,2) | WRTBM |
| /SKN/S_SW_10_02_INVENT_CNT | WRTZL | Value of physical inventory count | CURR(13,2) | WRTZL |
| /SKN/S_SW_10_02_INVENT_CNT | XAMEI | Handling in Alternative Unit of Measure | CHAR(1) | XAMEI |
| /SKN/S_SW_10_02_INVENT_CNT | XBLNI | Reference Number for Physical Inventory | CHAR(16) | XBLNI1 |
| /SKN/S_SW_10_02_INVENT_CNT | XBUFI | Freeze book inventory | CHAR(1) | XBUFI |
| /SKN/S_SW_10_02_INVENT_CNT | XDIFF | Difference posted | CHAR(1) | XDIFF |
| /SKN/S_SW_10_02_INVENT_CNT | XLOEK | Item is Deleted (Physical Inventory) | CHAR(1) | I_XLOEK |
| /SKN/S_SW_10_02_INVENT_CNT | XNULL | Zero count | CHAR(1) | XNULL |
| /SKN/S_SW_10_02_INVENT_CNT | XNZAE | Item will be recounted | CHAR(1) | XNZAE |
| /SKN/S_SW_10_02_INVENT_CNT | XZAEL | Item has been counted | CHAR(1) | XZAEL |
| /SKN/S_SW_10_02_INVENT_CNT | ZEILE | Item in Material Document | NUMC(4) | MBLPO |
| /SKN/S_SW_10_02_INVENT_CNT | ZEILI | Line Number | NUMC(3) | DZEILE |
| /SKN/S_SW_10_02_INVENT_CNT | ZLDAT | Date of last count | DATS(8) | DZLDAT |
| /SKN/S_SW_10_02_INVENT_CNT | ZSTAT | Status of count | CHAR(1) | DZSTAT |

## ABAP Code

```abap
FUNCTION /skn/f_sw_10_02_invent_cnt .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_INVENT_CNT OPTIONAL
*"----------------------------------------------------------------------
*** Begin Yuri C.++ 04.12.19
  INCLUDE /skn/pc_sw_ai_top.
  TYPES: BEGIN OF ty_waers,
           waers  TYPE t001-waers,
         END OF ty_waers,
         tt_waers TYPE STANDARD TABLE OF ty_waers.
*** End Yuri C.++ 04.12.19
  data_single: manage_in_utc  char1 ,
               langu          langu,
               backdays       int4,
               date_ref_fld   name_feld,
               duration_unit  /skn/e_sw_duration_unit,
               agg_lvl        char10,
               diff_amount    int4,
               present_zero   char1,
*** Begin Yuri C.++ 04.02.19
               ref_tabname1   tabname,
               ref_tabname2   tabname,
               ref_field1     name_feld,
               ref_field2     name_feld,
               comp_operator  bucc_operator,
               waers_fr       waers.
*** End Yuri C.++ 04.12.19
  lv_backdays      = 1.
  lv_date_ref_fld  = 'BUDAT'. "Posting Date in the Document
  lv_duration_unit = 'D'.
  lv_agg_lvl       = ''.
  lv_diff_amount   = 0.
  lv_present_zero  = ''.
  lv_langu         = 'E'.
*** Begin Yuri C.++ 04.12.19
*  lv_waers_fr     = 'USD'.
*  lv_ref_tabname1 = 'ISEG'.
*  lv_ref_field1   = 'DMBTR'.
*** End Yuri C.++ 04.12.19
  select_single: manage_in_utc,
                 langu,
                 backdays,
                 date_ref_fld,
                 duration_unit,
                 agg_lvl,
                 diff_amount,
                 present_zero,
*** Begin Yuri C.++ 04.12.19
                 ref_tabname1,
                 ref_tabname2,
                 ref_field1,
                 ref_field2,
                 comp_operator,
                 waers_fr.
*** end Yuri C.++ 04.12.19
  DATA: lv_diff_amount_ TYPE int4.
  lv_diff_amount_ = lv_diff_amount * ( -1 ).
  data_multy: vgart        vgart,
              werks        werks_d,
              lgort        lgort_d,
              sobkz        sobkz,
              usnam_hd     usnam,  "User name header
              usnam        usnaa,  "Us name Item
              sperr        isper,
              zstat        dzstat,
              dstat        dstat,
              lstat        lstat,
              xbufi        xbufi,
              waers        waers,  "10/02/18
              bldat        bldat,
              gidat        gidat,
              zldat        dzldat,
              budat        budat,
              datum        sy-datum,
*** Begin Yuri C.++ 04.12.19
*** T001W/T001K/T001
              bwkey        bwkey,
              bukrs        bukrs,
              ktopl        ktopl,
              result_comp  netwr_ap,
*** End Yuri C.++ 04.12.19
              duration    /skn/e_sw_duration
                  .
  select_multy:
              vgart,
              werks,
              lgort,
              sobkz,
              usnam_hd,  "User name header
              usnam,
              sperr,
              zstat,
              dstat,
              lstat,
              xbufi,
              waers,  "10/02/18
              bldat,
              gidat,
              zldat,
              budat,
              datum,
*** Begin Yuri C.++ 04.12.19
*** T001W/T001K/T001
              bwkey,
              bukrs,
              ktopl,
              result_comp,
*** End Yuri C.++ 04.12.19
              duration
                 .
  RANGES : r_fld_name FOR dd03p-fieldname,
           r_fld_val FOR dd03p-fieldname .
  DATA: fld_name TYPE fieldname.
  DATA: i TYPE i,
        ci(1) TYPE c,
        nfields TYPE i VALUE 3.   "
  DATA : backdays  TYPE i ,
         date_from LIKE sy-datum .
  DATA : langu LIKE sy-langu .
  DATA : is_out(1) TYPE c.
  DATA : time_diff TYPE  int4 .
*** Begin Yuri C.++ 04.12.19
  DATA: lv_amount_from  TYPE dmbtr,
        lv_amount_to    TYPE dmbtr,
        lv_amount_fr    TYPE dmbtr,
        lv_query        TYPE string,
        lv_query1       TYPE string,
        lv_query2       TYPE string,
        lv_query_curr   TYPE string,
        lv_text1        TYPE string,
        lv_text2        TYPE string,
        lv_type1        TYPE datatype_d,
        lv_type2        TYPE datatype_d,
        lv_open         TYPE boole_d VALUE 'X',
        lv_open_waers   TYPE boole_d VALUE 'X',
        lv_open_having  TYPE boole_d VALUE 'X',
        lv_having       TYPE boole_d,
        lv_val          TYPE char21,
        lv_field1_exist TYPE boole_d,
        lv_field2_exist TYPE boole_d,
        lv_waers_exist  TYPE boole_d,
        lv_alias1       TYPE /skn/e_sw_alias,
        lv_alias2       TYPE /skn/e_sw_alias,
        lv_alias_curr   TYPE /skn/e_sw_alias,
        lv_return       TYPE sysubrc,
        lv_tabix        TYPE i,
        lv_waers        TYPE waers.
  DATA: ls_waers TYPE ty_waers.
  DATA: lt_waers TYPE tt_waers.
  DATA: lt_option         TYPE TABLE OF rfc_db_opt,
        lt_iblnr_option   TYPE TABLE OF rfc_db_opt,
        lt_werks_option   TYPE TABLE OF rfc_db_opt,
        lt_out_where_cond TYPE TABLE OF /skn/s_sw_where_tab,
        lt_having_options TYPE TABLE OF rfc_db_opt,
        lt_in_range	      TYPE TABLE OF /skn/s_sw_range_tab,
        lt_sel_fields     TYPE /skn/tt_sel_fields,
        lt_dd03l          TYPE TABLE OF dd03l,
        lt_data           TYPE TABLE OF /skn/s_sw_10_02_invent_cnt.
  DATA: lwa_out_where_cond LIKE LINE OF lt_out_where_cond,
        lwa_in_range       LIKE LINE OF lt_in_range,
        ls_option          LIKE LINE OF lt_option,
        ls_having_options  TYPE rfc_db_opt,
        ls_sel_fields      LIKE LINE OF lt_sel_fields,
        ls_dd03l           TYPE dd03l.
*** End Yuri C.++ 04.12.19
  DATA : w_data LIKE LINE OF t_data .
  DATA: BEGIN OF ls_werks,
         werks    TYPE werks_d,
         budat    TYPE budat,
         sum_diff TYPE f,
         waers    TYPE waers,
        END OF ls_werks.
  DATA: lt_werks LIKE TABLE OF ls_werks.
  DATA: BEGIN OF ls_iblnr,
         iblnr TYPE iblnr,
         gjahr TYPE gjahr,
        END OF ls_iblnr.
  DATA: lt_iblnr LIKE TABLE OF ls_iblnr.
  DATA: BEGIN OF ls_iblnr_sum,
         iblnr TYPE iblnr,
         gjahr TYPE gjahr,
         sum_diff TYPE f,
        END OF ls_iblnr_sum.
  DATA: lt_iblnr_sum LIKE TABLE OF ls_iblnr_sum.
  data_multy: iblnr_tot   iblnr,
              gjahr_tot   gjahr,
              dmbtr       dmbtr.
  DATA : sy_tabix LIKE sy-tabix .
  DATA : fld(60) TYPE c .
  DATA : ref_date TYPE d.
*data: lra_range type range of DD03P-FIELDNAME.
  FIELD-SYMBOLS: <fs> TYPE any ,
                 <fs_v> TYPE any .
  DATA: lv_sel_ikpf   TYPE string,
        lv_sel_iseg1  TYPE string,
        lv_sel_iseg2  TYPE string,
        lv_sel_iseg3  TYPE string,
        lv_sel_clause TYPE string.
  "--- Run Cloud Mode -----
  data_single: sw_dest rfcdest.             .
  select_single: sw_dest.
  IF lv_sw_dest IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_INVENT_CNT'
      IMPORTING
        is_alert = is_alert
      TABLES
        t_select = t_select
        t_data   = t_data.
  ENDIF.
  CHECK lv_sw_dest IS INITIAL.
  "--- Run Cloud Mode -----
  IF r_datum[] IS INITIAL .
    rs_datum-sign   = 'I' .
    rs_datum-option = 'GE' .
    date_from       = sy-datum - lv_backdays .
    rs_datum-low    = date_from .
    APPEND rs_datum TO r_datum.
  ENDIF.
  "--- Set Reference Date Field
  CASE lv_date_ref_fld.
    WHEN 'BLDAT'.
      r_bldat[] = r_datum[]. "Document Date in Document
    WHEN 'GIDAT'.
      r_gidat[] = r_datum[]. "Planned date of inventory count
    WHEN 'ZLDAT'.
      r_zldat[] = r_datum[].  "Date of last count
    WHEN 'BUDAT'.
      r_budat[] = r_datum[].  "Posting Date in the Document
    WHEN OTHERS.
      r_budat[] = r_datum[].  "Posting Date in the Document
  ENDCASE.
  REFRESH r_dmbtr.
  IF lv_present_zero IS INITIAL.
    "--- <> 0.
    rs_dmbtr-sign   = 'I'.
    rs_dmbtr-option = 'NE'.
    rs_dmbtr-low    = 0.
    APPEND rs_dmbtr TO r_dmbtr.
  ENDIF.
*--- Retrieve data
  CLEAR is_alert .
  REFRESH t_data.
*** Begin Yuri C.++ 04.12.19
  rs_result_comp-sign   = 'I'.
  rs_result_comp-option = 'GE'.
  rs_result_comp-low    = lv_diff_amount.
  APPEND rs_result_comp TO r_result_comp[].
  CLEAR: rs_result_comp.
  rs_result_comp-sign   = 'I'.
  rs_result_comp-option = 'LE'.
  rs_result_comp-low    = lv_diff_amount_.
  APPEND rs_result_comp TO r_result_comp[].
**************** Get Table field details ******************
  CLEAR: lt_option[], lt_out_where_cond[].
  CLEAR: ls_option.
  IF lv_ref_tabname1 IS NOT INITIAL.
    IF lt_option IS NOT INITIAL.
      CONCATENATE 'AND' 'TABNAME' 'EQ' INTO lv_query SEPARATED BY space.
    ELSE.
      CONCATENATE 'TABNAME' 'EQ' INTO lv_query SEPARATED BY space.
    ENDIF.
    CONCATENATE ''''lv_ref_tabname1''''  INTO lv_ref_tabname1.
    CONCATENATE lv_query lv_ref_tabname1 INTO ls_option-text SEPARATED BY space.
    APPEND ls_option TO lt_option.
  ENDIF.
  CLEAR: ls_option.
  IF lv_ref_tabname2 IS NOT INITIAL.
    IF lt_option IS NOT INITIAL.
      CONCATENATE 'AND' 'TABNAME' 'EQ' INTO lv_query SEPARATED BY space.
    ELSE.
      CONCATENATE 'TABNAME' 'EQ' INTO lv_query SEPARATED BY space.
    ENDIF.
    CONCATENATE ''''lv_ref_tabname2''''  INTO lv_ref_tabname2.
    CONCATENATE lv_query lv_ref_tabname2 INTO ls_option-text SEPARATED BY space.
    APPEND ls_option TO lt_option.
  ENDIF.
  IF lt_option IS NOT INITIAL.
* Select table fields
    SELECT *
      FROM dd03l
      INTO TABLE lt_dd03l
      WHERE (lt_option).
  ENDIF.
  CLEAR: ls_option,
         lt_option.
* Check REF field 1 in table fields
  READ TABLE lt_dd03l INTO ls_dd03l WITH KEY fieldname = lv_ref_field1.
  IF sy-subrc = 0.
    lv_field1_exist = 'X'.
    lv_type1        = ls_dd03l-datatype.
  ENDIF.
  CLEAR: ls_dd03l.
* Check REF field 2 in table fields
  READ TABLE lt_dd03l INTO ls_dd03l WITH KEY fieldname = lv_ref_field2.
  IF sy-subrc = 0.
    lv_field2_exist = 'X'.
    lv_type2        = ls_dd03l-datatype.
  ENDIF.
  REPLACE ALL OCCURRENCES OF '''' IN lv_ref_tabname1 WITH ''.
  REPLACE ALL OCCURRENCES OF '''' IN lv_ref_tabname2 WITH ''.
* Set Alias
  IF lv_field1_exist EQ 'X'.
    CASE lv_ref_tabname1.
      WHEN 'ISEG'.
        lv_alias1 = 'b'.
*      WHEN ''.
*        lv_alias1 = ''.
      WHEN OTHERS.
    ENDCASE.
    CONCATENATE lv_alias1 lv_ref_field1 INTO lv_query1
      SEPARATED BY '~'.
  ENDIF.
  IF lv_field2_exist EQ 'X'.
    IF lv_ref_tabname1 <> lv_ref_tabname2 AND lv_ref_tabname2 IS NOT INITIAL.
      CASE lv_ref_tabname2.
        WHEN 'ISEG'.
          lv_alias2 = 'b'.
*        WHEN ''.
*          lv_alias2 = ''.
        WHEN OTHERS.
      ENDCASE.
    ELSE.
      lv_alias2 = lv_alias1.
    ENDIF.
    CONCATENATE lv_alias2 lv_ref_field2 INTO lv_query2
      SEPARATED BY '~'.
  ENDIF.
**************** Get Table field details ******************
**** Get all Currencies type of documents from T001 Table
  IF lv_waers_fr IS NOT INITIAL AND r_result_comp[] IS NOT INITIAL.
    REFRESH: lt_out_where_cond.
    CLEAR: lt_option.
*** T001W *****
    _range_to_sel_table 'w~WERKS' werks.
    _range_to_sel_table 'w~BWKEY' bwkey.
**** T001W ****
**** T001K ****
    _range_to_sel_table 'k~BUKRS' bukrs.
**** T001K ****
**** T001 ****
    _range_to_sel_table 't~WAERS' waers.
    _range_to_sel_table 't~KTOPL' ktopl.
**** T001 ****
    lt_option[] = lt_out_where_cond[].
    SELECT t~waers
      FROM t001w AS w INNER JOIN t001k AS k ON w~bwkey EQ k~bwkey
                      INNER JOIN t001  AS t ON k~bukrs EQ t~bukrs
      INTO TABLE lt_waers
      WHERE (lt_option)
      GROUP BY t~waers.
    IF sy-subrc = 0.
      lv_waers_exist = 'X'.
    ENDIF.
  ENDIF.
**** Get all Currencies type of documents from T001 Table
*** End Yuri C.++ 04.12.19
*** Begin Yuri C.-- 04.12.19
*  IF lv_agg_lvl = ''. "Single
*
*    _build_sql_sel_clause 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' lv_sel_ikpf.
*    " i_tabname = &1 , i_structure = &2 , TABLE_ALIAS = &3,  TAB_DEST  = &4 , S  EL_CLAUSE   = &5.
*    _build_sql_sel_clause 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' lv_sel_iseg.
*    CONCATENATE lv_sel_ikpf lv_sel_iseg INTO lv_sel_clause SEPARATED BY ' '.
*    CONCATENATE lv_sel_clause 'a~USNAM as USNAM_HD' INTO lv_sel_clause SEPARATED BY ' '.
*    SELECT (lv_sel_clause)  "   *
*       FROM ikpf AS a
*       INNER JOIN iseg AS b ON  a~iblnr = b~iblnr
*                            AND a~gjahr = b~gjahr
*       INTO CORRESPONDING FIELDS OF TABLE t_data
*       WHERE  a~vgart IN r_vgart
*         AND  b~werks IN r_werks
*         AND  b~lgort IN r_lgort
*         AND  b~sobkz IN r_sobkz
*         AND  b~usnam IN r_usnam
*         AND  a~usnam IN r_usnam_hd
*         AND  a~sperr IN r_sperr
*         AND  a~zstat IN r_zstat
*         AND  a~dstat IN r_dstat
*         AND  a~lstat IN r_lstat
*         AND  a~xbufi IN r_xbufi
*         AND  b~waers IN r_waers   "10/02/18
*
*         AND a~bldat IN r_bldat
*         AND a~gidat IN r_gidat
*         AND b~zldat IN r_zldat  "?????
*         AND b~budat IN r_budat  "?????
*         AND b~dmbtr IN r_dmbtr  "     <> 0
*         AND ( b~dmbtr >= lv_diff_amount OR b~dmbtr =< lv_diff_amount_ )
*             .
*  ELSEIF lv_agg_lvl = 'WERKS'. "Plant(WERKS)
*
*    REFRESH lt_werks.
*
*    SELECT b~werks b~budat SUM( b~dmbtr )
*      FROM ikpf AS a
*      INNER JOIN iseg AS b
*      ON a~iblnr = b~iblnr
*      AND a~gjahr = b~gjahr
*      INTO (ls_werks-werks, ls_werks-budat, ls_werks-sum_diff)
*      WHERE  a~vgart IN r_vgart
*        AND  b~werks IN r_werks
*        AND  b~lgort IN r_lgort
*        AND  b~sobkz IN r_sobkz
*        AND  b~usnam IN r_usnam
*        AND  a~usnam IN r_usnam_hd
*        AND  a~sperr IN r_sperr
*        AND  a~zstat IN r_zstat
*        AND  a~dstat IN r_dstat
*        AND  a~lstat IN r_lstat
*        AND  a~xbufi IN r_xbufi
*        AND  b~waers IN r_waers   "10/02/18
*
*        AND a~bldat IN r_bldat
*        AND a~gidat IN r_gidat
*        AND b~zldat IN r_zldat  "?????
*        AND b~budat IN r_budat  "?????
*        AND b~dmbtr <> 0
*      GROUP BY b~werks b~budat
*      HAVING SUM( b~dmbtr )  > lv_diff_amount OR
*             SUM( b~dmbtr )  < lv_diff_amount_.
*      "and ( b~DMBTR > lv_DIFF_AMOUNT OR b~DMBTR < lv_DIFF_AMOUNT_ ).
*      APPEND ls_werks TO lt_werks.
*    ENDSELECT.
*
*    IF lt_werks[] IS NOT INITIAL.
*
*      _build_sql_sel_clause 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' lv_sel_ikpf.
*      _build_sql_sel_clause 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' lv_sel_iseg.
*
*      CONCATENATE lv_sel_ikpf lv_sel_iseg INTO lv_sel_clause SEPARATED BY ' '.
*      CONCATENATE lv_sel_clause 'a~USNAM as USNAM_HD' INTO lv_sel_clause SEPARATED BY ' '.
*
*      SELECT (lv_sel_clause)  "   *
*       FROM ikpf AS a
*       INNER JOIN iseg AS b
*       ON a~iblnr = b~iblnr
*       AND a~gjahr = b~gjahr
*       INTO CORRESPONDING FIELDS OF TABLE t_data
*       FOR ALL ENTRIES IN lt_werks
*       WHERE  a~vgart IN r_vgart
*         AND  b~werks = lt_werks-werks
*         AND  b~budat = lt_werks-budat
*         AND  b~lgort IN r_lgort
*         AND  b~sobkz IN r_sobkz
*         AND  b~usnam IN r_usnam
*         AND  a~usnam IN r_usnam_hd
*         AND  a~sperr IN r_sperr
*         AND  a~zstat IN r_zstat
*         AND  a~dstat IN r_dstat
*         AND  a~lstat IN r_lstat
*         AND  a~xbufi IN r_xbufi
*         AND  b~waers IN r_waers   "10/02/18
*
*   "      and a~BLDAT in R_BLDAT
*         AND a~gidat IN r_gidat
*         AND b~zldat IN r_zldat  "?????
*         AND b~dmbtr IN r_dmbtr.  "     <> 0
*    ENDIF.
*    "****************************************************
*  ELSEIF lv_agg_lvl = 'IBLNR'. "Document (IBLNR)
*    "--- Get IBLNR
*    REFRESH lt_iblnr.
*    SELECT DISTINCT b~iblnr b~gjahr
*      FROM ikpf AS a
*      INNER JOIN iseg AS b
*      ON a~iblnr = b~iblnr
*      AND a~gjahr = b~gjahr
*      INTO CORRESPONDING FIELDS OF TABLE lt_iblnr
*      WHERE  a~vgart IN r_vgart
*        AND  b~werks IN r_werks
*        AND  b~lgort IN r_lgort
*        AND  b~sobkz IN r_sobkz
*        AND  b~usnam IN r_usnam
*        AND  a~usnam IN r_usnam_hd
*        AND  a~sperr IN r_sperr
*        AND  a~zstat IN r_zstat
*        AND  a~dstat IN r_dstat
*        AND  a~lstat IN r_lstat
*        AND  a~xbufi IN r_xbufi
*        AND  b~waers IN r_waers   "10/02/18
*
*        AND a~bldat IN r_bldat
*        AND a~gidat IN r_gidat
*        AND b~zldat IN r_zldat  "?????
*        AND b~budat IN r_budat  "?????
*        AND b~dmbtr <> 0.
**
*    IF lt_iblnr[] IS NOT INITIAL.
*      REFRESH: r_iblnr_tot,
*               r_gjahr_tot.
*      LOOP AT lt_iblnr INTO ls_iblnr.
*        rs_iblnr_tot-sign = 'I'.
*        rs_iblnr_tot-option = 'EQ'.
*        rs_iblnr_tot-low = ls_iblnr-iblnr.
*        APPEND rs_iblnr_tot TO r_iblnr_tot.
*        rs_gjahr_tot-sign = 'I'.
*        rs_gjahr_tot-option = 'EQ'.
*        rs_gjahr_tot-low = ls_iblnr-gjahr.
*        APPEND rs_gjahr_tot TO r_gjahr_tot.
*      ENDLOOP.
*      SORT: r_iblnr_tot,
*            r_gjahr_tot.
*      DELETE ADJACENT DUPLICATES FROM: r_iblnr_tot,
*                                       r_gjahr_tot.
*      REFRESH lt_iblnr_sum.
*      SELECT b~iblnr b~gjahr SUM( b~dmbtr )
*        FROM iseg AS b
*         INTO (ls_iblnr_sum-iblnr, ls_iblnr_sum-gjahr, ls_iblnr_sum-sum_diff)
**        FOR ALL ENTRIES IN lt_IBLNR
*         WHERE b~iblnr IN r_iblnr_tot
*           AND b~gjahr IN r_gjahr_tot
*           AND b~dmbtr <> 0
*         GROUP BY b~iblnr b~gjahr
*         HAVING SUM( b~dmbtr )  > lv_diff_amount OR
*               SUM( b~dmbtr )  < lv_diff_amount_.
*        APPEND ls_iblnr_sum TO lt_iblnr_sum.
*      ENDSELECT.
*    ENDIF.
*
*    IF lt_iblnr_sum[] IS NOT INITIAL.
*
*      _build_sql_sel_clause 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' lv_sel_ikpf.
*      _build_sql_sel_clause 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' lv_sel_iseg.
*      CONCATENATE lv_sel_ikpf lv_sel_iseg INTO lv_sel_clause SEPARATED BY ' '.
*      CONCATENATE lv_sel_clause 'a~USNAM as USNAM_HD' INTO lv_sel_clause SEPARATED BY ' '.
*
*      SELECT (lv_sel_clause)  "   *
*         FROM ikpf AS a
*         INNER JOIN iseg AS b
*           ON a~iblnr = b~iblnr
*          AND a~gjahr = b~gjahr
*         INTO CORRESPONDING FIELDS OF TABLE t_data
*         FOR ALL ENTRIES IN lt_iblnr_sum
*         WHERE a~iblnr = lt_iblnr_sum-iblnr
*           AND a~gjahr = lt_iblnr_sum-gjahr
*           AND b~dmbtr IN r_dmbtr.  "     <> 0
*    ENDIF.
*
*  ELSE.
*  ENDIF.
*** End Yuri C.-- 04.12.19
*** Begin Yuri C.++ 04.12.19
  IF lt_waers IS NOT INITIAL AND ( lv_field1_exist EQ 'X' OR
       lv_field2_exist EQ 'X' ) AND lv_waers_exist EQ 'X'.
    lv_alias_curr = 'b'.
    REFRESH: lt_out_where_cond[].
* IKPF
    _range_to_sel_table 'a~vgart'  vgart.
    _range_to_sel_table 'a~usnam'  usnam_hd.
    _range_to_sel_table 'a~sperr'  sperr.
    _range_to_sel_table 'a~zstat'  zstat.
    _range_to_sel_table 'a~dstat'  dstat.
    _range_to_sel_table 'a~lstat'  lstat.
    _range_to_sel_table 'a~xbufi'  xbufi.
    _range_to_sel_table 'a~bldat'  bldat.
    _range_to_sel_table 'a~gidat'  gidat.
* ISEG
    _range_to_sel_table 'b~werks'  werks.
    _range_to_sel_table 'b~lgort'  lgort.
    _range_to_sel_table 'b~sobkz'  sobkz.
    _range_to_sel_table 'b~usnam'  usnam.
*    _range_to_sel_table 'b~waers'  waers.
    _range_to_sel_table 'b~zldat'  zldat.
    _range_to_sel_table 'b~budat'  budat.
    _range_to_sel_table 'b~dmbtr'  dmbtr.
    IF lv_agg_lvl EQ ''.
      _build_sql_sel_clause 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' lv_sel_ikpf.
      _build_sql_sel_clause 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' lv_sel_iseg1.
      CONCATENATE lv_sel_ikpf lv_sel_iseg1
        INTO lv_sel_clause SEPARATED BY ' '.
      CONCATENATE lv_sel_clause 'a~USNAM as USNAM_HD'
        INTO lv_sel_clause SEPARATED BY ' '.
    ELSEIF lv_agg_lvl EQ 'WERKS'.
      REFRESH lt_werks.
      lv_having = 'X'.
    ELSEIF lv_agg_lvl EQ 'IBLNR'.
      REFRESH lt_iblnr.
      lv_having = 'X'.
    ENDIF.
    LOOP AT lt_waers INTO ls_waers.
      CLEAR: lv_waers, lv_return, lv_tabix, ls_option, lt_option,
             lt_having_options, lt_werks_option, lt_iblnr_option.
      lv_open_having = 'X'.
      APPEND LINES OF lt_out_where_cond TO lt_option.
      APPEND LINES OF lt_out_where_cond TO lt_iblnr_option.
      APPEND LINES OF lt_out_where_cond TO lt_werks_option.
      IF lv_open_waers EQ 'X'.
        CLEAR: lv_open_waers.
        ls_option-text = '('.
        IF lt_option IS NOT INITIAL.
          CONCATENATE 'AND' ls_option-text INTO ls_option-text SEPARATED BY space.
        ENDIF.
        APPEND ls_option TO lt_option.
      ENDIF.
      lv_waers = ls_waers-waers.
      LOOP AT r_result_comp INTO rs_result_comp.
        lv_tabix = sy-tabix.
        CLEAR: lv_amount_from, lv_amount_to, lv_amount_fr,
               ls_option, lv_text1, lv_text2, lv_query_curr,
               lv_val, lv_return.
        lv_amount_from = rs_result_comp-low.
        lv_amount_to   = rs_result_comp-high.
        IF lv_amount_from IS NOT INITIAL.
* Set select condition of RS_RESULT_COMP-LOW value
          IF lv_waers_fr <> lv_waers AND lv_waers IS NOT INITIAL AND
             lv_waers_fr IS NOT INITIAL.
            CLEAR: lv_return.
* Unit conversion for LOW amount
            CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
              EXPORTING
                date             = sy-datum
                foreign_currency = lv_waers        " Document Curr
                local_amount     = lv_amount_from
                local_currency   = lv_waers_fr     " Foreign Curr
              IMPORTING
                foreign_amount   = lv_amount_fr
              EXCEPTIONS
                no_rate_found    = 1
                overflow         = 2
                no_factors_found = 3
                no_spread_found  = 4
                derived_2_times  = 5
                OTHERS           = 6.
            lv_return = sy-subrc.
          ELSE.
            lv_amount_fr = lv_amount_from.
          ENDIF.
          IF lv_return <> 0.
            lv_open_waers = 'X'.
            EXIT.
          ENDIF.
          IF lv_amount_fr IS NOT INITIAL.
            IF lv_tabix > 1.
              ls_option-text = 'OR'.
              APPEND ls_option TO lt_option.
            ENDIF.
            CLEAR: ls_option.
            IF lv_open EQ 'X'.
              CLEAR lv_open.
              ls_option-text = '('.
            ENDIF.
            lv_val = lv_amount_fr.
            SHIFT lv_val RIGHT DELETING TRAILING space.
            SHIFT lv_val LEFT DELETING LEADING space.
            CONCATENATE ''''lv_waers'''' INTO lv_query_curr.
            CONCATENATE lv_alias_curr '~' 'WAERS' INTO lv_text1.
            CONCATENATE lv_text1 'EQ' lv_query_curr INTO lv_text2
              SEPARATED BY space.
            CONCATENATE ''''lv_val'''' INTO lv_val IN CHARACTER MODE.
            CONCATENATE ls_option-text lv_text2 'AND'
                        lv_query1 rs_result_comp-option lv_val
              INTO ls_option-text SEPARATED BY space.
          ENDIF.
* Set select condition of RS_RESULT_COMP-HIGH value
          IF lv_amount_to IS NOT INITIAL.
            CLEAR: lv_amount_fr, lv_val.
* Unit conversion for HIGH amount
            CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
              EXPORTING
                date             = sy-datum
                foreign_currency = ls_waers-waers  " Document Curr
                local_amount     = lv_amount_to
                local_currency   = lv_waers_fr     " Foreign Curr
              IMPORTING
                foreign_amount   = lv_amount_fr
              EXCEPTIONS
                no_rate_found    = 1
                overflow         = 2
                no_factors_found = 3
                no_spread_found  = 4
                derived_2_times  = 5
                OTHERS           = 6.
            IF sy-subrc = 0 AND lv_amount_fr IS NOT INITIAL.
              IF ls_option IS NOT INITIAL.
                lv_val = lv_amount_fr.
                SHIFT lv_val RIGHT DELETING TRAILING space.
                SHIFT lv_val LEFT DELETING LEADING space.
                CONCATENATE ''''lv_val'''' INTO lv_val IN CHARACTER MODE.
                CONCATENATE ls_option-text 'AND' lv_val
                  INTO ls_option-text SEPARATED BY space.
              ELSE.
                lv_val = lv_amount_fr.
                SHIFT lv_val RIGHT DELETING TRAILING space.
                SHIFT lv_val LEFT DELETING LEADING space.
                CONCATENATE ''''lv_val'''' INTO lv_val IN CHARACTER MODE.
                CONCATENATE ls_option-text 'AND' lv_val
                  INTO ls_option-text SEPARATED BY space.
              ENDIF.
              CLEAR: ls_option.
              APPEND ls_option TO lt_option.
            ENDIF.
          ENDIF.
          IF lv_open IS INITIAL.
            CONCATENATE ls_option-text ')' INTO ls_option-text
              SEPARATED BY space.
            lv_open = 'X'.
          ENDIF.
          IF ls_option IS NOT INITIAL.
            APPEND ls_option TO lt_option.
          ENDIF.
          IF lv_having EQ 'X'.
            IF lv_open_having EQ 'X'.
              CONCATENATE '( SUM(' lv_query1 ')' '>' lv_val
                INTO ls_having_options-text SEPARATED BY space.
              CLEAR lv_open_having.
            ELSE.
              CONCATENATE ls_having_options-text 'OR SUM(' lv_query1 ')'
                '<' lv_val ')' INTO ls_having_options-text SEPARATED BY space.
              lv_open_having = 'X'.
            ENDIF.
            IF lt_having_options IS NOT INITIAL AND lv_open_having IS INITIAL.
              CONCATENATE 'AND' ls_having_options-text
                INTO ls_having_options-text SEPARATED BY space.
            ENDIF.
            APPEND ls_having_options TO lt_having_options.
            CLEAR: ls_having_options.
            IF lv_field2_exist EQ 'X'.
* Here we can add another having option query condition
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_return = 0.
        CLEAR: ls_option.
        IF lv_open_waers IS INITIAL.
          ls_option-text = ')'.
          APPEND ls_option TO lt_option.
          lv_open_waers = 'X'.
        ENDIF.
        IF lv_agg_lvl = ''. "Single.
          SELECT (lv_sel_clause)
             FROM ikpf AS a
             INNER JOIN iseg AS b ON  a~iblnr = b~iblnr
                                  AND a~gjahr = b~gjahr
             INTO CORRESPONDING FIELDS OF TABLE lt_data
             WHERE (lt_option)
                   .
        ELSEIF lv_agg_lvl = 'WERKS'. "Plant(WERKS)
          REFRESH lt_werks.
          CLEAR: lv_sel_clause.
          lv_sel_iseg1 = 'b~werks'.
          lv_sel_iseg2 = 'b~budat'.
          lv_sel_iseg3 = lv_query1.
          CONCATENATE lv_sel_iseg1 lv_sel_iseg2
            INTO lv_sel_clause SEPARATED BY space.
          CONCATENATE lv_sel_clause 'SUM(' lv_sel_iseg3 ')' 'AS SUM_DIFF'
             INTO lv_sel_clause SEPARATED BY space.
          CLEAR ls_option.
* lv_text2 = B~WAERS EQ current currency(to conversion on)
          ls_option-text = lv_text2.
          IF lt_werks_option IS NOT INITIAL.
            CONCATENATE 'AND' ls_option-text INTO ls_option-text
              SEPARATED BY space.
          ENDIF.
          APPEND ls_option TO lt_werks_option.
          SELECT (lv_sel_clause)  "b~werks b~budat SUM( b~dmbtr )
            FROM ikpf AS a
            INNER JOIN iseg AS b ON  a~iblnr EQ b~iblnr
                                 AND a~gjahr EQ b~gjahr
            INTO CORRESPONDING FIELDS OF ls_werks "(ls_werks-werks, ls_werks-budat, ls_werks-sum_diff)
            WHERE (lt_werks_option)
*                   a~vgart IN r_vgart
*              AND  b~werks IN r_werks
*              AND  b~lgort IN r_lgort
*              AND  b~sobkz IN r_sobkz
*              AND  b~usnam IN r_usnam
*              AND  a~usnam IN r_usnam_hd
*              AND  a~sperr IN r_sperr
*              AND  a~zstat IN r_zstat
*              AND  a~dstat IN r_dstat
*              AND  a~lstat IN r_lstat
*              AND  a~xbufi IN r_xbufi
*              AND  b~waers IN r_waers   "10/02/18
*
*              AND a~bldat IN r_bldat
*              AND a~gidat IN r_gidat
*              AND b~zldat IN r_zldat
*              AND b~budat IN r_budat
*              AND b~dmbtr <> 0
              GROUP BY b~werks b~budat
              HAVING (lt_having_options). "( SUM( b~dmbtr ) > lv_diff_amount OR
            "  SUM( b~dmbtr ) < lv_diff_amount_ ).
            APPEND ls_werks TO lt_werks.
          ENDSELECT.
          IF lt_werks[] IS NOT INITIAL.
            CLEAR: lv_sel_clause, lv_sel_ikpf, lv_sel_iseg1.
            _build_sql_sel_clause 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' lv_sel_ikpf.
            _build_sql_sel_clause 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' lv_sel_iseg1.
            CONCATENATE lv_sel_ikpf lv_sel_iseg1 INTO lv_sel_clause SEPARATED BY ' '.
            CONCATENATE lv_sel_clause 'a~USNAM as USNAM_HD' INTO lv_sel_clause SEPARATED BY ' '.
            SELECT (lv_sel_clause)
             FROM ikpf AS a
             INNER JOIN iseg AS b ON  a~iblnr EQ b~iblnr
                                  AND a~gjahr EQ b~gjahr
             INTO CORRESPONDING FIELDS OF TABLE lt_data
             FOR ALL ENTRIES IN lt_werks
             WHERE a~vgart IN r_vgart
             AND   b~werks EQ lt_werks-werks
             AND   b~budat EQ lt_werks-budat
             AND   b~lgort IN r_lgort
             AND   b~sobkz IN r_sobkz
             AND   b~usnam IN r_usnam
             AND   a~usnam IN r_usnam_hd
             AND   a~sperr IN r_sperr
             AND   a~zstat IN r_zstat
             AND   a~dstat IN r_dstat
             AND   a~lstat IN r_lstat
             AND   a~xbufi IN r_xbufi
             AND   b~waers IN r_waers
             AND   a~gidat IN r_gidat
             AND   b~zldat IN r_zldat
             AND   b~dmbtr IN r_dmbtr.
          ENDIF.
*    "****************************************************
        ELSEIF lv_agg_lvl = 'IBLNR'. "Document (IBLNR)
          "--- Get IBLNR
          REFRESH lt_iblnr.
          SELECT DISTINCT b~iblnr b~gjahr
            FROM ikpf AS a INNER JOIN iseg AS b ON  a~iblnr EQ b~iblnr
                                                AND a~gjahr EQ b~gjahr
            INTO CORRESPONDING FIELDS OF TABLE lt_iblnr
            WHERE a~vgart IN r_vgart
            AND   b~werks IN r_werks
            AND   b~lgort IN r_lgort
            AND   b~sobkz IN r_sobkz
            AND   b~usnam IN r_usnam
            AND   a~usnam IN r_usnam_hd
            AND   a~sperr IN r_sperr
            AND   a~zstat IN r_zstat
            AND   a~dstat IN r_dstat
            AND   a~lstat IN r_lstat
            AND   a~xbufi IN r_xbufi
            AND   b~waers IN r_waers
            AND   a~bldat IN r_bldat
            AND   a~gidat IN r_gidat
            AND   b~zldat IN r_zldat
            AND   b~budat IN r_budat
            AND   b~dmbtr NE 0.
          IF lt_iblnr[] IS NOT INITIAL.
            REFRESH: r_iblnr_tot,
                     r_gjahr_tot.
            LOOP AT lt_iblnr INTO ls_iblnr.
              rs_iblnr_tot-sign   = 'I'.
              rs_iblnr_tot-option = 'EQ'.
              rs_iblnr_tot-low    = ls_iblnr-iblnr.
              APPEND rs_iblnr_tot TO r_iblnr_tot.
              rs_gjahr_tot-sign   = 'I'.
              rs_gjahr_tot-option = 'EQ'.
              rs_gjahr_tot-low    = ls_iblnr-gjahr.
              APPEND rs_gjahr_tot TO r_gjahr_tot.
            ENDLOOP.
            SORT: r_iblnr_tot,
                  r_gjahr_tot.
            DELETE ADJACENT DUPLICATES FROM: r_iblnr_tot,
                                             r_gjahr_tot.
            REFRESH lt_iblnr_sum.
*** Set Selection fields
            CLEAR: lv_sel_clause.
            lv_sel_iseg1 = 'b~iblnr'.
            lv_sel_iseg2 = 'b~gjahr'.
            lv_sel_iseg3 = lv_query1.
            CONCATENATE lv_sel_iseg1 lv_sel_iseg2
              INTO lv_sel_clause SEPARATED BY space.
            CONCATENATE lv_sel_clause 'SUM(' lv_sel_iseg3 ')' 'AS SUM_DIFF'
               INTO lv_sel_clause SEPARATED BY space.
            SELECT (lv_sel_clause) "b~iblnr b~gjahr SUM( b~dmbtr )
              FROM iseg AS b
              INTO CORRESPONDING FIELDS OF ls_iblnr_sum "(ls_iblnr_sum-iblnr, ls_iblnr_sum-gjahr, ls_iblnr_sum-sum_diff)
              WHERE (lt_iblnr_option)
              AND   b~iblnr IN r_iblnr_tot
              AND   b~gjahr IN r_gjahr_tot
              AND   b~dmbtr NE 0
              GROUP BY b~iblnr b~gjahr
              HAVING (lt_having_options).   "SUM( b~dmbtr ) > lv_diff_amount
              "OR     SUM( b~dmbtr ) < lv_diff_amount_.
              APPEND ls_iblnr_sum TO lt_iblnr_sum.
            ENDSELECT.
          ENDIF.
          IF lt_iblnr_sum[] IS NOT INITIAL.
            CLEAR: lv_sel_iseg1, lv_sel_ikpf, lv_sel_clause.
            _build_sql_sel_clause 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' lv_sel_ikpf.
            _build_sql_sel_clause 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' lv_sel_iseg1.
            CONCATENATE lv_sel_ikpf lv_sel_iseg1 INTO lv_sel_clause SEPARATED BY ' '.
            CONCATENATE lv_sel_clause 'a~USNAM as USNAM_HD' INTO lv_sel_clause SEPARATED BY ' '.
            SELECT (lv_sel_clause)  "   *
               FROM ikpf AS a INNER JOIN iseg AS b ON  a~iblnr EQ b~iblnr
                                                   AND a~gjahr EQ b~gjahr
               INTO CORRESPONDING FIELDS OF TABLE lt_data
               FOR ALL ENTRIES IN lt_iblnr_sum
               WHERE a~iblnr EQ lt_iblnr_sum-iblnr
               AND   a~gjahr EQ lt_iblnr_sum-gjahr
               AND   b~dmbtr IN r_dmbtr.
          ENDIF.
        ENDIF.
        IF sy-subrc = 0 AND lt_data IS NOT INITIAL.
          APPEND LINES OF lt_data TO t_data[].
          CLEAR: lt_data.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF lv_agg_lvl = ''. "Single
      SELECT (lv_sel_clause)
         FROM ikpf AS a
         INNER JOIN iseg AS b ON  a~iblnr = b~iblnr
                              AND a~gjahr = b~gjahr
         INTO CORRESPONDING FIELDS OF TABLE t_data
         WHERE  a~vgart IN r_vgart
           AND  b~werks IN r_werks
           AND  b~lgort IN r_lgort
           AND  b~sobkz IN r_sobkz
           AND  b~usnam IN r_usnam
           AND  a~usnam IN r_usnam_hd
           AND  a~sperr IN r_sperr
           AND  a~zstat IN r_zstat
           AND  a~dstat IN r_dstat
           AND  a~lstat IN r_lstat
           AND  a~xbufi IN r_xbufi
           AND  b~waers IN r_waers   "10/02/18
           AND a~bldat IN r_bldat
           AND a~gidat IN r_gidat
           AND b~zldat IN r_zldat
           AND b~budat IN r_budat
           AND b~dmbtr IN r_dmbtr
           AND ( b~dmbtr >= lv_diff_amount OR b~dmbtr =< lv_diff_amount_ )
               .
    ELSEIF lv_agg_lvl = 'WERKS'. "Plant(WERKS)
      REFRESH lt_werks.
      SELECT b~werks b~budat SUM( b~dmbtr )
        FROM ikpf AS a
        INNER JOIN iseg AS b
        ON a~iblnr = b~iblnr
        AND a~gjahr = b~gjahr
        INTO (ls_werks-werks, ls_werks-budat, ls_werks-sum_diff)
        WHERE  a~vgart IN r_vgart
          AND  b~werks IN r_werks
          AND  b~lgort IN r_lgort
          AND  b~sobkz IN r_sobkz
          AND  b~usnam IN r_usnam
          AND  a~usnam IN r_usnam_hd
          AND  a~sperr IN r_sperr
          AND  a~zstat IN r_zstat
          AND  a~dstat IN r_dstat
          AND  a~lstat IN r_lstat
          AND  a~xbufi IN r_xbufi
          AND  b~waers IN r_waers   "10/02/18
          AND a~bldat IN r_bldat
          AND a~gidat IN r_gidat
          AND b~zldat IN r_zldat
          AND b~budat IN r_budat
          AND b~dmbtr <> 0
          GROUP BY b~werks b~budat
          HAVING SUM( b~dmbtr ) > lv_diff_amount OR
                 SUM( b~dmbtr ) < lv_diff_amount_.
        APPEND ls_werks TO lt_werks.
      ENDSELECT.
      IF lt_werks[] IS NOT INITIAL.
        CLEAR: lv_sel_ikpf, lv_sel_iseg1, lv_sel_clause.
        _build_sql_sel_clause 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' lv_sel_ikpf.
        _build_sql_sel_clause 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' lv_sel_iseg1.
        CONCATENATE lv_sel_ikpf lv_sel_iseg1 INTO lv_sel_clause SEPARATED BY ' '.
        CONCATENATE lv_sel_clause 'a~USNAM as USNAM_HD' INTO lv_sel_clause SEPARATED BY ' '.
        SELECT (lv_sel_clause)  "   *
         FROM ikpf AS a
         INNER JOIN iseg AS b
         ON a~iblnr = b~iblnr
         AND a~gjahr = b~gjahr
         INTO CORRESPONDING FIELDS OF TABLE t_data
         FOR ALL ENTRIES IN lt_werks
         WHERE  a~vgart IN r_vgart
           AND  b~werks = lt_werks-werks
           AND  b~budat = lt_werks-budat
           AND  b~lgort IN r_lgort
           AND  b~sobkz IN r_sobkz
           AND  b~usnam IN r_usnam
           AND  a~usnam IN r_usnam_hd
           AND  a~sperr IN r_sperr
           AND  a~zstat IN r_zstat
           AND  a~dstat IN r_dstat
           AND  a~lstat IN r_lstat
           AND  a~xbufi IN r_xbufi
           AND  b~waers IN r_waers   "10/02/18
     "      and a~BLDAT in R_BLDAT
           AND a~gidat IN r_gidat
           AND b~zldat IN r_zldat  "?????
           AND b~dmbtr IN r_dmbtr.  "     <> 0
      ENDIF.
*    "****************************************************
    ELSEIF lv_agg_lvl = 'IBLNR'. "Document (IBLNR)
      "--- Get IBLNR
      REFRESH lt_iblnr.
      SELECT DISTINCT b~iblnr b~gjahr
        FROM ikpf AS a
        INNER JOIN iseg AS b ON  a~iblnr EQ b~iblnr
                             AND a~gjahr EQ b~gjahr
        INTO CORRESPONDING FIELDS OF TABLE lt_iblnr
        WHERE  a~vgart IN r_vgart
        AND    b~werks IN r_werks
        AND    b~lgort IN r_lgort
        AND    b~sobkz IN r_sobkz
        AND    b~usnam IN r_usnam
        AND    a~usnam IN r_usnam_hd
        AND    a~sperr IN r_sperr
        AND    a~zstat IN r_zstat
        AND    a~dstat IN r_dstat
        AND    a~lstat IN r_lstat
        AND    a~xbufi IN r_xbufi
        AND    b~waers IN r_waers
        AND    a~bldat IN r_bldat
        AND    a~gidat IN r_gidat
        AND    b~zldat IN r_zldat
        AND    b~budat IN r_budat
        AND    b~dmbtr NE 0.
*
      IF lt_iblnr[] IS NOT INITIAL.
        REFRESH: r_iblnr_tot,
                 r_gjahr_tot.
        LOOP AT lt_iblnr INTO ls_iblnr.
          rs_iblnr_tot-sign   = 'I'.
          rs_iblnr_tot-option = 'EQ'.
          rs_iblnr_tot-low    = ls_iblnr-iblnr.
          APPEND rs_iblnr_tot TO r_iblnr_tot.
          rs_gjahr_tot-sign   = 'I'.
          rs_gjahr_tot-option = 'EQ'.
          rs_gjahr_tot-low    = ls_iblnr-gjahr.
          APPEND rs_gjahr_tot TO r_gjahr_tot.
        ENDLOOP.
        SORT: r_iblnr_tot,
              r_gjahr_tot.
        DELETE ADJACENT DUPLICATES FROM: r_iblnr_tot,
                                         r_gjahr_tot.
        REFRESH lt_iblnr_sum.
        SELECT b~iblnr b~gjahr SUM( b~dmbtr )
          FROM iseg AS b
           INTO (ls_iblnr_sum-iblnr, ls_iblnr_sum-gjahr, ls_iblnr_sum-sum_diff)
           WHERE b~iblnr IN r_iblnr_tot
           AND   b~gjahr IN r_gjahr_tot
           AND   b~dmbtr NE 0
           GROUP BY b~iblnr b~gjahr
           HAVING SUM( b~dmbtr ) > lv_diff_amount OR
                  SUM( b~dmbtr ) < lv_diff_amount_.
          APPEND ls_iblnr_sum TO lt_iblnr_sum.
        ENDSELECT.
      ENDIF.
      IF lt_iblnr_sum[] IS NOT INITIAL.
        CLEAR: lv_sel_ikpf, lv_sel_iseg1, lv_sel_clause.
        _build_sql_sel_clause 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' lv_sel_ikpf.
        _build_sql_sel_clause 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' lv_sel_iseg1.
        CONCATENATE lv_sel_ikpf lv_sel_iseg1 INTO lv_sel_clause SEPARATED BY ' '.
        CONCATENATE lv_sel_clause 'a~USNAM as USNAM_HD' INTO lv_sel_clause SEPARATED BY ' '.
        SELECT (lv_sel_clause)  "   *
           FROM ikpf AS a INNER JOIN iseg AS b ON  a~iblnr EQ b~iblnr
                                               AND a~gjahr EQ b~gjahr
           INTO CORRESPONDING FIELDS OF TABLE t_data
           FOR ALL ENTRIES IN lt_iblnr_sum
           WHERE a~iblnr EQ lt_iblnr_sum-iblnr
           AND   a~gjahr EQ lt_iblnr_sum-gjahr
           AND   b~dmbtr IN r_dmbtr.  "     <> 0
      ENDIF.
    ENDIF.
  ENDIF.
*** End Yuri C.++ 04.02.19
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT t_data .
    sy_tabix = sy-tabix .
    CONCATENATE 'T_DATA-' lv_date_ref_fld INTO fld .
    ASSIGN (fld) TO <fs>.
    ref_date = <fs> .
    IF NOT ref_date IS INITIAL.
      t_data-duration_unit = lv_duration_unit.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          d_from      = ref_date
          t_from      = sy-uzeit
          d_to        = sy-datum
          t_to        = sy-uzeit
          time_unit   = lv_duration_unit   "'D'
        IMPORTING
          time_diff   = time_diff
        EXCEPTIONS
          wrong_value = 1
          OTHERS      = 2.
      IF sy-subrc = 0.
        IF time_diff < '999999'.
          t_data-duration  = time_diff .
        ELSE.
          t_data-duration  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY t_data INDEX sy_tabix.
    ENDIF.
  ENDLOOP.
  DELETE t_data WHERE duration  NOT IN r_duration .
******************************************************************************
  LOOP AT t_data .
    sy_tabix = sy-tabix .
    ""    t_data-ABS_DMBTR = abs( t_data-DMBTR ).
    IF t_data-menge < t_data-buchm.
      t_data-abs_dmbtr = t_data-dmbtr * ( -1 ).
    ELSE.
      t_data-abs_dmbtr = t_data-dmbtr.
    ENDIF.
    MODIFY t_data INDEX sy_tabix.
  ENDLOOP.
********************************************************************************
  LOOP AT t_data.
**Material desc
    sy_tabix = sy-tabix .
    CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
      EXPORTING
        matnr         = t_data-matnr
        langu         = lv_langu
      IMPORTING
        material_desc = t_data-mat_desc
      EXCEPTIONS
        wrong_code    = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
*** Begin Yuri C.++ 11.12.19
    t_data-ref_field_name1 = lv_ref_field1.
    t_data-ref_field_name2 = lv_ref_field2.
    t_data-waers_fr        = lv_waers_fr.
*** End Yuri C.++ 11.12.19
    MODIFY t_data INDEX sy_tabix.
  ENDLOOP.
*****************************************************************
*--- Check Alert Information
  READ TABLE t_data INDEX 1.
  CHECK NOT sy-tfill  IS INITIAL .
  is_alert = 'X' .
ENDFUNCTION.
```