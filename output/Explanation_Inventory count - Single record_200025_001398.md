# Exception Indicator: Inventory count - Single record - SW_10_02_INV_CNT_SNG

## General Overview

This Exception Indicator (EI) monitors physical inventory count documents in Materials Management (MM) at single-record (item) level to identify inventory differences by plant, storage location, and document across configurable time windows. It provides visibility into count variances, difference amounts, and status duration for audit and operational control.

This EI serves as an essential control for inventory management and financial oversight by:
- Enabling detection of significant inventory count differences at item level that exceed thresholds and require investigation or adjustment
- Supporting identification of plants, documents, or items with exceptional variances for prioritization and root-cause analysis
- Providing visibility into count status and duration by configurable date reference for release and follow-up decisions
- Enabling analysis of count patterns by plant, storage location, and document type for process and control improvement
- Supporting accountability by user (header and item level) and by count status for audit and compliance

This monitoring enables organizations to detect unusual count variances, aging or blocked counts requiring action, and concentration patterns that may indicate process or control issues. The EI is particularly valuable for period-end inventory reconciliation, physical inventory audits, and exception management in materials and warehouse operations.

The EI uses physical inventory header and item data (IKPF, ISEG) and optional valuation/currency logic; output is filtered according to the configured parameters.


## Problem Description

Failure to monitor physical inventory count differences at single-record level creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected inventory count differences can distort inventory valuations and cost of goods sold in period-end closing
- Significant variances in specific plants or documents may indicate valuation errors, theft, or process failures requiring adjustment
- Unreconciled count differences can lead to misstated balance sheet and audit findings
- Delayed identification of count exceptions may complicate month-end close and require restatements
- Concentration of variances in certain organizational units can mask systemic issues in inventory controls

**Operational and Control Risks**
- Count documents with large differences or prolonged status duration may indicate blocked or incomplete counts requiring release or recount
- Lack of visibility by plant, storage location, or document type limits ability to prioritize recounts and resource allocation
- Unmonitored count status (posting block, delete flag, freeze book inventory) can delay material movements and production
- Missing item-level exception visibility prevents management from focusing on specific high-impact count lines
- Inconsistent use of date reference (document date, posting date, planned count date) can lead to wrong monitoring windows and missed exceptions

**Management Visibility and Decision-Making Risks**
- Absence of exception monitoring delays awareness of significant count variances and status backlogs
- Unidentified concentration of differences by plant or document can lead to missed process improvements or fraud indicators
- Lack of duration and status visibility limits ability to escalate aging counts or unblock inventory
- Insufficient filtering by organizational and status dimensions restricts actionable reporting for auditors and operations

## Suggested Resolution

**Immediate Response**
- Review the count differences and documents flagged by the EI to understand scope and magnitude (threshold violations, plant or document concentration)
- Verify high-variance documents using physical inventory transactions (e.g. MI04, MI09) to confirm counts and legitimacy
- Check count status (posting block, delete, freeze) and processing progress to determine if manual release or correction is pending
- Identify business context for exceptions: timing of count, material type, storage location, or user responsible

**System Assessment**
- Analyze the time window and date reference used for monitoring to ensure the lookback period and date field align with count and posting cycles
- Review aggregation level (single record vs by plant vs by document) to confirm the result set supports the intended analysis
- Examine difference amount threshold and zero-inclusion setting to ensure material variances are captured without noise
- Assess plant, storage location, and document type distribution to identify patterns or master data issues
- Validate duration unit and reference date field so that status duration is interpreted correctly for prioritization

**Corrective Actions**
- Post inventory differences or adjustments through standard MM physical inventory transactions where errors or legitimate variances are confirmed
- For blocked or incomplete counts, release posting blocks or complete recount procedures as per process
- Update count status or master data (e.g. storage location, material) if misconfiguration or data quality issues are found
- Adjust monitoring parameters (lookback, threshold, aggregation level, date reference) to align with policy and re-run the EI
- Document exceptions and resolutions for audit trail; establish recurring EI runs for continuous visibility into count variances and status


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

Aggregation level for count data (e.g. single record vs aggregated). Controls how the EI groups and returns data. The parameter controls which dimensions and level of detail the result set has: empty returns single count item records; WERKS summarizes by plant and posting date with a sum of difference amount and threshold check; IBLNR summarizes by physical inventory document and fiscal year. Choose empty for item-level audit, WERKS for plant-level exception lists, or IBLNR for document-level follow-up.

**AGG_LVL Options:**
- ** ** (empty): Single record (item level).
- **WERKS**: Aggregated by plant and posting date.
- **IBLNR**: Aggregated by physical inventory document and fiscal year.

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

When set, include count items with zero quantity or zero difference in the result; when not set, they may be excluded.

**PRESENT_ZERO Options:**
- **X**: Include zero quantities/differences.
- ** ** (space or initial): Exclude (or default behavior).

**PS_PSP_PNR** (WBS Element):

Parameter for wbs element. Used in selection criteria or output structure as defined in the function.

**REF_FIELD_NAME1 - REF_FIELD_NAME2** (Field name – Field name):

First and second reference field names for comparison or display. Used when consistency or comparison checks reference specific fields (e.g. for difference amount or value comparison).

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

Company code local currency. Business meaning: currency in which amounts are stored for the plant/company code.

**WAERS_FR** (Currency):

Foreign/reference currency used for conversion or comparison. Business meaning: currency to which amounts are converted for reporting or threshold comparison.

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

**Time and date reference parameters:**
- BACKDAYS defines the lookback window (days from today). When no date range is supplied, the EI builds the range from today minus BACKDAYS.
- DATE_REF_FLD selects which date field is used for that range: BLDAT (document date), GIDAT (planned count date), ZLDAT (date of last count), or BUDAT (posting date). The chosen field is applied to filter header/item dates and is also used later for duration calculation.
- DURATION_UNIT is used together with the date reference when calculating status duration: the EI computes the time difference from the reference date to today in the selected unit (e.g. days) and filters and outputs duration accordingly.

**Aggregation parameters:**
- AGG_LVL controls the aggregation level of the result set. Empty value returns single inventory count item records; WERKS aggregates by plant and posting date (with SUM of difference amount and HAVING threshold); IBLNR aggregates by physical inventory document and fiscal year (with SUM and HAVING). The same selection criteria apply; only the grouping and level of detail change.

**Difference amount and zero handling:**
- DIFF_AMOUNT defines the threshold; records (or aggregated groups when AGG_LVL is set) are included when the difference amount is outside the range [−DIFF_AMOUNT, +DIFF_AMOUNT].
- PRESENT_ZERO controls whether zero difference amounts are included in the selection. When initial, the EI excludes zero (difference &lt;&gt; 0); when set (e.g. X), zero differences can be presented.

**Optional comparison and currency:**
- REF_TABNAME1, REF_FIELD1, REF_TABNAME2, REF_FIELD2, COMP_OPERATOR, and RESULT_COMP support dynamic comparison logic and currency conversion (e.g. WAERS_FR). When used, they work together to filter or compare amounts from the specified tables/fields.


### Default Values

- **BACKDAYS** — Default: `1` (lookback window when no date range is supplied).
- **DATE_REF_FLD** — Default: `BUDAT` (posting date in the document).
- **DURATION_UNIT** — Default: `D` (days).
- **AGG_LVL** — Default: initial (empty); single record level when not supplied.
- **DIFF_AMOUNT** — Default: `0` (threshold; used with symmetric range for inclusion).
- **PRESENT_ZERO** — Default: initial (empty); zero differences excluded when not supplied.
- **LANGU** — Default: `E` (English).

**Note:** When PRESENT_ZERO is not set, the EI excludes records with zero difference amount from the result set.

### Practical Configuration Examples

**Use Case 1: Single-record detail for audit**
```
BACKDAYS = 30
DATE_REF_FLD = BUDAT
AGG_LVL = 
DURATION_UNIT = D
PRESENT_ZERO = X
```
**Purpose:** Full item-level detail for the last 30 days by posting date, including zero differences, with duration in days for prioritization and audit review.

**Use Case 2: Recent count differences by plant**
```
BACKDAYS = 7
DATE_REF_FLD = BUDAT
AGG_LVL = WERKS
DIFF_AMOUNT = 100
PRESENT_ZERO = 
```
**Purpose:** Focus on plants with material count variances in the last 7 days, aggregated by plant and posting date, excluding zero differences and only where absolute difference exceeds 100.

**Use Case 3: Document-level aggregation**
```
BACKDAYS = 14
DATE_REF_FLD = GIDAT
AGG_LVL = IBLNR
DIFF_AMOUNT = 50
```
**Purpose:** Identify physical inventory documents whose total difference (by document and fiscal year) exceeds the threshold over the last 14 days based on planned count date; supports document-level follow-up.

**Use Case 4: Duration and date reference**
```
BACKDAYS = 5
DATE_REF_FLD = ZLDAT
DURATION_UNIT = D
AGG_LVL = 
PRESENT_ZERO = 
```
**Purpose:** Count items with differences in the last 5 days by date of last count, with duration in days from that date for status aging and release decisions.


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
FUNCTION /SKN/F_SW_10_02_INVENT_CNT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_INVENT_CNT OPTIONAL
*"----------------------------------------------------------------------
*** Begin Yuri C.++ 04.12.19
  INCLUDE /SKN/PC_SW_AI_TOP.
  TYPES: BEGIN OF TY_WAERS,
           WAERS  TYPE T001-WAERS,
         END OF TY_WAERS,
         TT_WAERS TYPE STANDARD TABLE OF TY_WAERS.
*** End Yuri C.++ 04.12.19
  DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
               LANGU          LANGU,
               BACKDAYS       INT4,
               DATE_REF_FLD   NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               AGG_LVL        CHAR10,
               DIFF_AMOUNT    INT4,
               PRESENT_ZERO   CHAR1,
*** Begin Yuri C.++ 04.02.19
               REF_TABNAME1   TABNAME,
               REF_TABNAME2   TABNAME,
               REF_FIELD1     NAME_FELD,
               REF_FIELD2     NAME_FELD,
               COMP_OPERATOR  BUCC_OPERATOR,
               WAERS_FR       WAERS.
*** End Yuri C.++ 04.12.19
  LV_BACKDAYS      = 1.
  LV_DATE_REF_FLD  = 'BUDAT'. "Posting Date in the Document
  LV_DURATION_UNIT = 'D'.
  LV_AGG_LVL       = ''.
  LV_DIFF_AMOUNT   = 0.
  LV_PRESENT_ZERO  = ''.
  LV_LANGU         = 'E'.
*** Begin Yuri C.++ 04.12.19
*  lv_waers_fr     = 'USD'.
*  lv_ref_tabname1 = 'ISEG'.
*  lv_ref_field1   = 'DMBTR'.
*** End Yuri C.++ 04.12.19
  SELECT_SINGLE: MANAGE_IN_UTC,
                 LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT,
                 AGG_LVL,
                 DIFF_AMOUNT,
                 PRESENT_ZERO,
*** Begin Yuri C.++ 04.12.19
                 REF_TABNAME1,
                 REF_TABNAME2,
                 REF_FIELD1,
                 REF_FIELD2,
                 COMP_OPERATOR,
                 WAERS_FR.
*** end Yuri C.++ 04.12.19
  DATA: LV_DIFF_AMOUNT_ TYPE INT4.
  LV_DIFF_AMOUNT_ = LV_DIFF_AMOUNT * ( -1 ).
  DATA_MULTY: VGART        VGART,
              WERKS        WERKS_D,
              LGORT        LGORT_D,
              SOBKZ        SOBKZ,
              USNAM_HD     USNAM,  "User name header
              USNAM        USNAA,  "Us name Item
              SPERR        ISPER,
              ZSTAT        DZSTAT,
              DSTAT        DSTAT,
              LSTAT        LSTAT,
              XBUFI        XBUFI,
              WAERS        WAERS,  "10/02/18
              BLDAT        BLDAT,
              GIDAT        GIDAT,
              ZLDAT        DZLDAT,
              BUDAT        BUDAT,
              DATUM        SY-DATUM,
*** Begin Yuri C.++ 04.12.19
*** T001W/T001K/T001
              BWKEY        BWKEY,
              BUKRS        BUKRS,
              KTOPL        KTOPL,
              RESULT_COMP  NETWR_AP,
*** End Yuri C.++ 04.12.19
              DURATION    /SKN/E_SW_DURATION
                  .
  SELECT_MULTY:
              VGART,
              WERKS,
              LGORT,
              SOBKZ,
              USNAM_HD,  "User name header
              USNAM,
              SPERR,
              ZSTAT,
              DSTAT,
              LSTAT,
              XBUFI,
              WAERS,  "10/02/18
              BLDAT,
              GIDAT,
              ZLDAT,
              BUDAT,
              DATUM,
*** Begin Yuri C.++ 04.12.19
*** T001W/T001K/T001
              BWKEY,
              BUKRS,
              KTOPL,
              RESULT_COMP,
*** End Yuri C.++ 04.12.19
              DURATION
                 .
  RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
           R_FLD_VAL FOR DD03P-FIELDNAME .
  DATA: FLD_NAME TYPE FIELDNAME.
  DATA: I TYPE I,
        CI(1) TYPE C,
        NFIELDS TYPE I VALUE 3.   "
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM .
  DATA : LANGU LIKE SY-LANGU .
  DATA : IS_OUT(1) TYPE C.
  DATA : TIME_DIFF TYPE  INT4 .
*** Begin Yuri C.++ 04.12.19
  DATA: LV_AMOUNT_FROM  TYPE DMBTR,
        LV_AMOUNT_TO    TYPE DMBTR,
        LV_AMOUNT_FR    TYPE DMBTR,
        LV_QUERY        TYPE STRING,
        LV_QUERY1       TYPE STRING,
        LV_QUERY2       TYPE STRING,
        LV_QUERY_CURR   TYPE STRING,
        LV_TEXT1        TYPE STRING,
        LV_TEXT2        TYPE STRING,
        LV_TYPE1        TYPE DATATYPE_D,
        LV_TYPE2        TYPE DATATYPE_D,
        LV_OPEN         TYPE BOOLE_D VALUE 'X',
        LV_OPEN_WAERS   TYPE BOOLE_D VALUE 'X',
        LV_OPEN_HAVING  TYPE BOOLE_D VALUE 'X',
        LV_HAVING       TYPE BOOLE_D,
        LV_VAL          TYPE CHAR21,
        LV_FIELD1_EXIST TYPE BOOLE_D,
        LV_FIELD2_EXIST TYPE BOOLE_D,
        LV_WAERS_EXIST  TYPE BOOLE_D,
        LV_ALIAS1       TYPE /SKN/E_SW_ALIAS,
        LV_ALIAS2       TYPE /SKN/E_SW_ALIAS,
        LV_ALIAS_CURR   TYPE /SKN/E_SW_ALIAS,
        LV_RETURN       TYPE SYSUBRC,
        LV_TABIX        TYPE I,
        LV_WAERS        TYPE WAERS.
  DATA: LS_WAERS TYPE TY_WAERS.
  DATA: LT_WAERS TYPE TT_WAERS.
  DATA: LT_OPTION         TYPE TABLE OF RFC_DB_OPT,
        LT_IBLNR_OPTION   TYPE TABLE OF RFC_DB_OPT,
        LT_WERKS_OPTION   TYPE TABLE OF RFC_DB_OPT,
        LT_OUT_WHERE_COND TYPE TABLE OF /SKN/S_SW_WHERE_TAB,
        LT_HAVING_OPTIONS TYPE TABLE OF RFC_DB_OPT,
        LT_IN_RANGE	      TYPE TABLE OF /SKN/S_SW_RANGE_TAB,
        LT_SEL_FIELDS     TYPE /SKN/TT_SEL_FIELDS,
        LT_DD03L          TYPE TABLE OF DD03L,
        LT_DATA           TYPE TABLE OF /SKN/S_SW_10_02_INVENT_CNT.
  DATA: LWA_OUT_WHERE_COND LIKE LINE OF LT_OUT_WHERE_COND,
        LWA_IN_RANGE       LIKE LINE OF LT_IN_RANGE,
        LS_OPTION          LIKE LINE OF LT_OPTION,
        LS_HAVING_OPTIONS  TYPE RFC_DB_OPT,
        LS_SEL_FIELDS      LIKE LINE OF LT_SEL_FIELDS,
        LS_DD03L           TYPE DD03L.
*** End Yuri C.++ 04.12.19
  DATA : W_DATA LIKE LINE OF T_DATA .
  DATA: BEGIN OF LS_WERKS,
         WERKS    TYPE WERKS_D,
         BUDAT    TYPE BUDAT,
         SUM_DIFF TYPE F,
         WAERS    TYPE WAERS,
        END OF LS_WERKS.
  DATA: LT_WERKS LIKE TABLE OF LS_WERKS.
  DATA: BEGIN OF LS_IBLNR,
         IBLNR TYPE IBLNR,
         GJAHR TYPE GJAHR,
        END OF LS_IBLNR.
  DATA: LT_IBLNR LIKE TABLE OF LS_IBLNR.
  DATA: BEGIN OF LS_IBLNR_SUM,
         IBLNR TYPE IBLNR,
         GJAHR TYPE GJAHR,
         SUM_DIFF TYPE F,
        END OF LS_IBLNR_SUM.
  DATA: LT_IBLNR_SUM LIKE TABLE OF LS_IBLNR_SUM.
  DATA_MULTY: IBLNR_TOT   IBLNR,
              GJAHR_TOT   GJAHR,
              DMBTR       DMBTR.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA: LV_SEL_IKPF   TYPE STRING,
        LV_SEL_ISEG1  TYPE STRING,
        LV_SEL_ISEG2  TYPE STRING,
        LV_SEL_ISEG3  TYPE STRING,
        LV_SEL_CLAUSE TYPE STRING.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_INVENT_CNT'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'BLDAT'.
      R_BLDAT[] = R_DATUM[]. "Document Date in Document
    WHEN 'GIDAT'.
      R_GIDAT[] = R_DATUM[]. "Planned date of inventory count
    WHEN 'ZLDAT'.
      R_ZLDAT[] = R_DATUM[].  "Date of last count
    WHEN 'BUDAT'.
      R_BUDAT[] = R_DATUM[].  "Posting Date in the Document
    WHEN OTHERS.
      R_BUDAT[] = R_DATUM[].  "Posting Date in the Document
  ENDCASE.
  REFRESH R_DMBTR.
  IF LV_PRESENT_ZERO IS INITIAL.
    "--- <> 0.
    RS_DMBTR-SIGN   = 'I'.
    RS_DMBTR-OPTION = 'NE'.
    RS_DMBTR-LOW    = 0.
    APPEND RS_DMBTR TO R_DMBTR.
  ENDIF.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
*** Begin Yuri C.++ 04.12.19
  RS_RESULT_COMP-SIGN   = 'I'.
  RS_RESULT_COMP-OPTION = 'GE'.
  RS_RESULT_COMP-LOW    = LV_DIFF_AMOUNT.
  APPEND RS_RESULT_COMP TO R_RESULT_COMP[].
  CLEAR: RS_RESULT_COMP.
  RS_RESULT_COMP-SIGN   = 'I'.
  RS_RESULT_COMP-OPTION = 'LE'.
  RS_RESULT_COMP-LOW    = LV_DIFF_AMOUNT_.
  APPEND RS_RESULT_COMP TO R_RESULT_COMP[].
**************** Get Table field details ******************
  CLEAR: LT_OPTION[], LT_OUT_WHERE_COND[].
  CLEAR: LS_OPTION.
  IF LV_REF_TABNAME1 IS NOT INITIAL.
    IF LT_OPTION IS NOT INITIAL.
      CONCATENATE 'AND' 'TABNAME' 'EQ' INTO LV_QUERY SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'TABNAME' 'EQ' INTO LV_QUERY SEPARATED BY SPACE.
    ENDIF.
    CONCATENATE ''''LV_REF_TABNAME1''''  INTO LV_REF_TABNAME1.
    CONCATENATE LV_QUERY LV_REF_TABNAME1 INTO LS_OPTION-TEXT SEPARATED BY SPACE.
    APPEND LS_OPTION TO LT_OPTION.
  ENDIF.
  CLEAR: LS_OPTION.
  IF LV_REF_TABNAME2 IS NOT INITIAL.
    IF LT_OPTION IS NOT INITIAL.
      CONCATENATE 'AND' 'TABNAME' 'EQ' INTO LV_QUERY SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'TABNAME' 'EQ' INTO LV_QUERY SEPARATED BY SPACE.
    ENDIF.
    CONCATENATE ''''LV_REF_TABNAME2''''  INTO LV_REF_TABNAME2.
    CONCATENATE LV_QUERY LV_REF_TABNAME2 INTO LS_OPTION-TEXT SEPARATED BY SPACE.
    APPEND LS_OPTION TO LT_OPTION.
  ENDIF.
  IF LT_OPTION IS NOT INITIAL.
* Select table fields
    SELECT *
      FROM DD03L
      INTO TABLE LT_DD03L
      WHERE (LT_OPTION).
  ENDIF.
  CLEAR: LS_OPTION,
         LT_OPTION.
* Check REF field 1 in table fields
  READ TABLE LT_DD03L INTO LS_DD03L WITH KEY FIELDNAME = LV_REF_FIELD1.
  IF SY-SUBRC = 0.
    LV_FIELD1_EXIST = 'X'.
    LV_TYPE1        = LS_DD03L-DATATYPE.
  ENDIF.
  CLEAR: LS_DD03L.
* Check REF field 2 in table fields
  READ TABLE LT_DD03L INTO LS_DD03L WITH KEY FIELDNAME = LV_REF_FIELD2.
  IF SY-SUBRC = 0.
    LV_FIELD2_EXIST = 'X'.
    LV_TYPE2        = LS_DD03L-DATATYPE.
  ENDIF.
  REPLACE ALL OCCURRENCES OF '''' IN LV_REF_TABNAME1 WITH ''.
  REPLACE ALL OCCURRENCES OF '''' IN LV_REF_TABNAME2 WITH ''.
* Set Alias
  IF LV_FIELD1_EXIST EQ 'X'.
    CASE LV_REF_TABNAME1.
      WHEN 'ISEG'.
        LV_ALIAS1 = 'b'.
*      WHEN ''.
*        lv_alias1 = ''.
      WHEN OTHERS.
    ENDCASE.
    CONCATENATE LV_ALIAS1 LV_REF_FIELD1 INTO LV_QUERY1
      SEPARATED BY '~'.
  ENDIF.
  IF LV_FIELD2_EXIST EQ 'X'.
    IF LV_REF_TABNAME1 <> LV_REF_TABNAME2 AND LV_REF_TABNAME2 IS NOT INITIAL.
      CASE LV_REF_TABNAME2.
        WHEN 'ISEG'.
          LV_ALIAS2 = 'b'.
*        WHEN ''.
*          lv_alias2 = ''.
        WHEN OTHERS.
      ENDCASE.
    ELSE.
      LV_ALIAS2 = LV_ALIAS1.
    ENDIF.
    CONCATENATE LV_ALIAS2 LV_REF_FIELD2 INTO LV_QUERY2
      SEPARATED BY '~'.
  ENDIF.
**************** Get Table field details ******************
**** Get all Currencies type of documents from T001 Table
  IF LV_WAERS_FR IS NOT INITIAL AND R_RESULT_COMP[] IS NOT INITIAL.
    REFRESH: LT_OUT_WHERE_COND.
    CLEAR: LT_OPTION.
*** T001W *****
    _RANGE_TO_SEL_TABLE 'w~WERKS' WERKS.
    _RANGE_TO_SEL_TABLE 'w~BWKEY' BWKEY.
**** T001W ****
**** T001K ****
    _RANGE_TO_SEL_TABLE 'k~BUKRS' BUKRS.
**** T001K ****
**** T001 ****
    _RANGE_TO_SEL_TABLE 't~WAERS' WAERS.
    _RANGE_TO_SEL_TABLE 't~KTOPL' KTOPL.
**** T001 ****
    LT_OPTION[] = LT_OUT_WHERE_COND[].
    SELECT T~WAERS
      FROM T001W AS W INNER JOIN T001K AS K ON W~BWKEY EQ K~BWKEY
                      INNER JOIN T001  AS T ON K~BUKRS EQ T~BUKRS
      INTO TABLE LT_WAERS
      WHERE (LT_OPTION)
      GROUP BY T~WAERS.
    IF SY-SUBRC = 0.
      LV_WAERS_EXIST = 'X'.
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
  IF LT_WAERS IS NOT INITIAL AND ( LV_FIELD1_EXIST EQ 'X' OR
       LV_FIELD2_EXIST EQ 'X' ) AND LV_WAERS_EXIST EQ 'X'.
    LV_ALIAS_CURR = 'b'.
    REFRESH: LT_OUT_WHERE_COND[].
* IKPF
    _RANGE_TO_SEL_TABLE 'a~vgart'  VGART.
    _RANGE_TO_SEL_TABLE 'a~usnam'  USNAM_HD.
    _RANGE_TO_SEL_TABLE 'a~sperr'  SPERR.
    _RANGE_TO_SEL_TABLE 'a~zstat'  ZSTAT.
    _RANGE_TO_SEL_TABLE 'a~dstat'  DSTAT.
    _RANGE_TO_SEL_TABLE 'a~lstat'  LSTAT.
    _RANGE_TO_SEL_TABLE 'a~xbufi'  XBUFI.
    _RANGE_TO_SEL_TABLE 'a~bldat'  BLDAT.
    _RANGE_TO_SEL_TABLE 'a~gidat'  GIDAT.
* ISEG
    _RANGE_TO_SEL_TABLE 'b~werks'  WERKS.
    _RANGE_TO_SEL_TABLE 'b~lgort'  LGORT.
    _RANGE_TO_SEL_TABLE 'b~sobkz'  SOBKZ.
    _RANGE_TO_SEL_TABLE 'b~usnam'  USNAM.
*    _range_to_sel_table 'b~waers'  waers.
    _RANGE_TO_SEL_TABLE 'b~zldat'  ZLDAT.
    _RANGE_TO_SEL_TABLE 'b~budat'  BUDAT.
    _RANGE_TO_SEL_TABLE 'b~dmbtr'  DMBTR.
    IF LV_AGG_LVL EQ ''.
      _BUILD_SQL_SEL_CLAUSE 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' LV_SEL_IKPF.
      _BUILD_SQL_SEL_CLAUSE 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' LV_SEL_ISEG1.
      CONCATENATE LV_SEL_IKPF LV_SEL_ISEG1
        INTO LV_SEL_CLAUSE SEPARATED BY ' '.
      CONCATENATE LV_SEL_CLAUSE 'a~USNAM as USNAM_HD'
        INTO LV_SEL_CLAUSE SEPARATED BY ' '.
    ELSEIF LV_AGG_LVL EQ 'WERKS'.
      REFRESH LT_WERKS.
      LV_HAVING = 'X'.
    ELSEIF LV_AGG_LVL EQ 'IBLNR'.
      REFRESH LT_IBLNR.
      LV_HAVING = 'X'.
    ENDIF.
    LOOP AT LT_WAERS INTO LS_WAERS.
      CLEAR: LV_WAERS, LV_RETURN, LV_TABIX, LS_OPTION, LT_OPTION,
             LT_HAVING_OPTIONS, LT_WERKS_OPTION, LT_IBLNR_OPTION.
      LV_OPEN_HAVING = 'X'.
      APPEND LINES OF LT_OUT_WHERE_COND TO LT_OPTION.
      APPEND LINES OF LT_OUT_WHERE_COND TO LT_IBLNR_OPTION.
      APPEND LINES OF LT_OUT_WHERE_COND TO LT_WERKS_OPTION.
      IF LV_OPEN_WAERS EQ 'X'.
        CLEAR: LV_OPEN_WAERS.
        LS_OPTION-TEXT = '('.
        IF LT_OPTION IS NOT INITIAL.
          CONCATENATE 'AND' LS_OPTION-TEXT INTO LS_OPTION-TEXT SEPARATED BY SPACE.
        ENDIF.
        APPEND LS_OPTION TO LT_OPTION.
      ENDIF.
      LV_WAERS = LS_WAERS-WAERS.
      LOOP AT R_RESULT_COMP INTO RS_RESULT_COMP.
        LV_TABIX = SY-TABIX.
        CLEAR: LV_AMOUNT_FROM, LV_AMOUNT_TO, LV_AMOUNT_FR,
               LS_OPTION, LV_TEXT1, LV_TEXT2, LV_QUERY_CURR,
               LV_VAL, LV_RETURN.
        LV_AMOUNT_FROM = RS_RESULT_COMP-LOW.
        LV_AMOUNT_TO   = RS_RESULT_COMP-HIGH.
        IF LV_AMOUNT_FROM IS NOT INITIAL.
* Set select condition of RS_RESULT_COMP-LOW value
          IF LV_WAERS_FR <> LV_WAERS AND LV_WAERS IS NOT INITIAL AND
             LV_WAERS_FR IS NOT INITIAL.
            CLEAR: LV_RETURN.
* Unit conversion for LOW amount
            CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
              EXPORTING
                DATE             = SY-DATUM
                FOREIGN_CURRENCY = LV_WAERS        " Document Curr
                LOCAL_AMOUNT     = LV_AMOUNT_FROM
                LOCAL_CURRENCY   = LV_WAERS_FR     " Foreign Curr
              IMPORTING
                FOREIGN_AMOUNT   = LV_AMOUNT_FR
              EXCEPTIONS
                NO_RATE_FOUND    = 1
                OVERFLOW         = 2
                NO_FACTORS_FOUND = 3
                NO_SPREAD_FOUND  = 4
                DERIVED_2_TIMES  = 5
                OTHERS           = 6.
            LV_RETURN = SY-SUBRC.
          ELSE.
            LV_AMOUNT_FR = LV_AMOUNT_FROM.
          ENDIF.
          IF LV_RETURN <> 0.
            LV_OPEN_WAERS = 'X'.
            EXIT.
          ENDIF.
          IF LV_AMOUNT_FR IS NOT INITIAL.
            IF LV_TABIX > 1.
              LS_OPTION-TEXT = 'OR'.
              APPEND LS_OPTION TO LT_OPTION.
            ENDIF.
            CLEAR: LS_OPTION.
            IF LV_OPEN EQ 'X'.
              CLEAR LV_OPEN.
              LS_OPTION-TEXT = '('.
            ENDIF.
            LV_VAL = LV_AMOUNT_FR.
            SHIFT LV_VAL RIGHT DELETING TRAILING SPACE.
            SHIFT LV_VAL LEFT DELETING LEADING SPACE.
            CONCATENATE ''''LV_WAERS'''' INTO LV_QUERY_CURR.
            CONCATENATE LV_ALIAS_CURR '~' 'WAERS' INTO LV_TEXT1.
            CONCATENATE LV_TEXT1 'EQ' LV_QUERY_CURR INTO LV_TEXT2
              SEPARATED BY SPACE.
            CONCATENATE ''''LV_VAL'''' INTO LV_VAL IN CHARACTER MODE.
            CONCATENATE LS_OPTION-TEXT LV_TEXT2 'AND'
                        LV_QUERY1 RS_RESULT_COMP-OPTION LV_VAL
              INTO LS_OPTION-TEXT SEPARATED BY SPACE.
          ENDIF.
* Set select condition of RS_RESULT_COMP-HIGH value
          IF LV_AMOUNT_TO IS NOT INITIAL.
            CLEAR: LV_AMOUNT_FR, LV_VAL.
* Unit conversion for HIGH amount
            CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
              EXPORTING
                DATE             = SY-DATUM
                FOREIGN_CURRENCY = LS_WAERS-WAERS  " Document Curr
                LOCAL_AMOUNT     = LV_AMOUNT_TO
                LOCAL_CURRENCY   = LV_WAERS_FR     " Foreign Curr
              IMPORTING
                FOREIGN_AMOUNT   = LV_AMOUNT_FR
              EXCEPTIONS
                NO_RATE_FOUND    = 1
                OVERFLOW         = 2
                NO_FACTORS_FOUND = 3
                NO_SPREAD_FOUND  = 4
                DERIVED_2_TIMES  = 5
                OTHERS           = 6.
            IF SY-SUBRC = 0 AND LV_AMOUNT_FR IS NOT INITIAL.
              IF LS_OPTION IS NOT INITIAL.
                LV_VAL = LV_AMOUNT_FR.
                SHIFT LV_VAL RIGHT DELETING TRAILING SPACE.
                SHIFT LV_VAL LEFT DELETING LEADING SPACE.
                CONCATENATE ''''LV_VAL'''' INTO LV_VAL IN CHARACTER MODE.
                CONCATENATE LS_OPTION-TEXT 'AND' LV_VAL
                  INTO LS_OPTION-TEXT SEPARATED BY SPACE.
              ELSE.
                LV_VAL = LV_AMOUNT_FR.
                SHIFT LV_VAL RIGHT DELETING TRAILING SPACE.
                SHIFT LV_VAL LEFT DELETING LEADING SPACE.
                CONCATENATE ''''LV_VAL'''' INTO LV_VAL IN CHARACTER MODE.
                CONCATENATE LS_OPTION-TEXT 'AND' LV_VAL
                  INTO LS_OPTION-TEXT SEPARATED BY SPACE.
              ENDIF.
              CLEAR: LS_OPTION.
              APPEND LS_OPTION TO LT_OPTION.
            ENDIF.
          ENDIF.
          IF LV_OPEN IS INITIAL.
            CONCATENATE LS_OPTION-TEXT ')' INTO LS_OPTION-TEXT
              SEPARATED BY SPACE.
            LV_OPEN = 'X'.
          ENDIF.
          IF LS_OPTION IS NOT INITIAL.
            APPEND LS_OPTION TO LT_OPTION.
          ENDIF.
          IF LV_HAVING EQ 'X'.
            IF LV_OPEN_HAVING EQ 'X'.
              CONCATENATE '( SUM(' LV_QUERY1 ')' '>' LV_VAL
                INTO LS_HAVING_OPTIONS-TEXT SEPARATED BY SPACE.
              CLEAR LV_OPEN_HAVING.
            ELSE.
              CONCATENATE LS_HAVING_OPTIONS-TEXT 'OR SUM(' LV_QUERY1 ')'
                '<' LV_VAL ')' INTO LS_HAVING_OPTIONS-TEXT SEPARATED BY SPACE.
              LV_OPEN_HAVING = 'X'.
            ENDIF.
            IF LT_HAVING_OPTIONS IS NOT INITIAL AND LV_OPEN_HAVING IS INITIAL.
              CONCATENATE 'AND' LS_HAVING_OPTIONS-TEXT
                INTO LS_HAVING_OPTIONS-TEXT SEPARATED BY SPACE.
            ENDIF.
            APPEND LS_HAVING_OPTIONS TO LT_HAVING_OPTIONS.
            CLEAR: LS_HAVING_OPTIONS.
            IF LV_FIELD2_EXIST EQ 'X'.
* Here we can add another having option query condition
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF LV_RETURN = 0.
        CLEAR: LS_OPTION.
        IF LV_OPEN_WAERS IS INITIAL.
          LS_OPTION-TEXT = ')'.
          APPEND LS_OPTION TO LT_OPTION.
          LV_OPEN_WAERS = 'X'.
        ENDIF.
        IF LV_AGG_LVL = ''. "Single.
          SELECT (LV_SEL_CLAUSE)
             FROM IKPF AS A
             INNER JOIN ISEG AS B ON  A~IBLNR = B~IBLNR
                                  AND A~GJAHR = B~GJAHR
             INTO CORRESPONDING FIELDS OF TABLE LT_DATA
             WHERE (LT_OPTION)
                   .
        ELSEIF LV_AGG_LVL = 'WERKS'. "Plant(WERKS)
          REFRESH LT_WERKS.
          CLEAR: LV_SEL_CLAUSE.
          LV_SEL_ISEG1 = 'b~werks'.
          LV_SEL_ISEG2 = 'b~budat'.
          LV_SEL_ISEG3 = LV_QUERY1.
          CONCATENATE LV_SEL_ISEG1 LV_SEL_ISEG2
            INTO LV_SEL_CLAUSE SEPARATED BY SPACE.
          CONCATENATE LV_SEL_CLAUSE 'SUM(' LV_SEL_ISEG3 ')' 'AS SUM_DIFF'
             INTO LV_SEL_CLAUSE SEPARATED BY SPACE.
          CLEAR LS_OPTION.
* lv_text2 = B~WAERS EQ current currency(to conversion on)
          LS_OPTION-TEXT = LV_TEXT2.
          IF LT_WERKS_OPTION IS NOT INITIAL.
            CONCATENATE 'AND' LS_OPTION-TEXT INTO LS_OPTION-TEXT
              SEPARATED BY SPACE.
          ENDIF.
          APPEND LS_OPTION TO LT_WERKS_OPTION.
          SELECT (LV_SEL_CLAUSE)  "b~werks b~budat SUM( b~dmbtr )
            FROM IKPF AS A
            INNER JOIN ISEG AS B ON  A~IBLNR EQ B~IBLNR
                                 AND A~GJAHR EQ B~GJAHR
            INTO CORRESPONDING FIELDS OF LS_WERKS "(ls_werks-werks, ls_werks-budat, ls_werks-sum_diff)
            WHERE (LT_WERKS_OPTION)
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
              GROUP BY B~WERKS B~BUDAT
              HAVING (LT_HAVING_OPTIONS). "( SUM( b~dmbtr ) > lv_diff_amount OR
            "  SUM( b~dmbtr ) < lv_diff_amount_ ).
            APPEND LS_WERKS TO LT_WERKS.
          ENDSELECT.
          IF LT_WERKS[] IS NOT INITIAL.
            CLEAR: LV_SEL_CLAUSE, LV_SEL_IKPF, LV_SEL_ISEG1.
            _BUILD_SQL_SEL_CLAUSE 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' LV_SEL_IKPF.
            _BUILD_SQL_SEL_CLAUSE 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' LV_SEL_ISEG1.
            CONCATENATE LV_SEL_IKPF LV_SEL_ISEG1 INTO LV_SEL_CLAUSE SEPARATED BY ' '.
            CONCATENATE LV_SEL_CLAUSE 'a~USNAM as USNAM_HD' INTO LV_SEL_CLAUSE SEPARATED BY ' '.
            SELECT (LV_SEL_CLAUSE)
             FROM IKPF AS A
             INNER JOIN ISEG AS B ON  A~IBLNR EQ B~IBLNR
                                  AND A~GJAHR EQ B~GJAHR
             INTO CORRESPONDING FIELDS OF TABLE LT_DATA
             FOR ALL ENTRIES IN LT_WERKS
             WHERE A~VGART IN R_VGART
             AND   B~WERKS EQ LT_WERKS-WERKS
             AND   B~BUDAT EQ LT_WERKS-BUDAT
             AND   B~LGORT IN R_LGORT
             AND   B~SOBKZ IN R_SOBKZ
             AND   B~USNAM IN R_USNAM
             AND   A~USNAM IN R_USNAM_HD
             AND   A~SPERR IN R_SPERR
             AND   A~ZSTAT IN R_ZSTAT
             AND   A~DSTAT IN R_DSTAT
             AND   A~LSTAT IN R_LSTAT
             AND   A~XBUFI IN R_XBUFI
             AND   B~WAERS IN R_WAERS
             AND   A~GIDAT IN R_GIDAT
             AND   B~ZLDAT IN R_ZLDAT
             AND   B~DMBTR IN R_DMBTR.
          ENDIF.
*    "****************************************************
        ELSEIF LV_AGG_LVL = 'IBLNR'. "Document (IBLNR)
          "--- Get IBLNR
          REFRESH LT_IBLNR.
          SELECT DISTINCT B~IBLNR B~GJAHR
            FROM IKPF AS A INNER JOIN ISEG AS B ON  A~IBLNR EQ B~IBLNR
                                                AND A~GJAHR EQ B~GJAHR
            INTO CORRESPONDING FIELDS OF TABLE LT_IBLNR
            WHERE A~VGART IN R_VGART
            AND   B~WERKS IN R_WERKS
            AND   B~LGORT IN R_LGORT
            AND   B~SOBKZ IN R_SOBKZ
            AND   B~USNAM IN R_USNAM
            AND   A~USNAM IN R_USNAM_HD
            AND   A~SPERR IN R_SPERR
            AND   A~ZSTAT IN R_ZSTAT
            AND   A~DSTAT IN R_DSTAT
            AND   A~LSTAT IN R_LSTAT
            AND   A~XBUFI IN R_XBUFI
            AND   B~WAERS IN R_WAERS
            AND   A~BLDAT IN R_BLDAT
            AND   A~GIDAT IN R_GIDAT
            AND   B~ZLDAT IN R_ZLDAT
            AND   B~BUDAT IN R_BUDAT
            AND   B~DMBTR NE 0.
          IF LT_IBLNR[] IS NOT INITIAL.
            REFRESH: R_IBLNR_TOT,
                     R_GJAHR_TOT.
            LOOP AT LT_IBLNR INTO LS_IBLNR.
              RS_IBLNR_TOT-SIGN   = 'I'.
              RS_IBLNR_TOT-OPTION = 'EQ'.
              RS_IBLNR_TOT-LOW    = LS_IBLNR-IBLNR.
              APPEND RS_IBLNR_TOT TO R_IBLNR_TOT.
              RS_GJAHR_TOT-SIGN   = 'I'.
              RS_GJAHR_TOT-OPTION = 'EQ'.
              RS_GJAHR_TOT-LOW    = LS_IBLNR-GJAHR.
              APPEND RS_GJAHR_TOT TO R_GJAHR_TOT.
            ENDLOOP.
            SORT: R_IBLNR_TOT,
                  R_GJAHR_TOT.
            DELETE ADJACENT DUPLICATES FROM: R_IBLNR_TOT,
                                             R_GJAHR_TOT.
            REFRESH LT_IBLNR_SUM.
*** Set Selection fields
            CLEAR: LV_SEL_CLAUSE.
            LV_SEL_ISEG1 = 'b~iblnr'.
            LV_SEL_ISEG2 = 'b~gjahr'.
            LV_SEL_ISEG3 = LV_QUERY1.
            CONCATENATE LV_SEL_ISEG1 LV_SEL_ISEG2
              INTO LV_SEL_CLAUSE SEPARATED BY SPACE.
            CONCATENATE LV_SEL_CLAUSE 'SUM(' LV_SEL_ISEG3 ')' 'AS SUM_DIFF'
               INTO LV_SEL_CLAUSE SEPARATED BY SPACE.
            SELECT (LV_SEL_CLAUSE) "b~iblnr b~gjahr SUM( b~dmbtr )
              FROM ISEG AS B
              INTO CORRESPONDING FIELDS OF LS_IBLNR_SUM "(ls_iblnr_sum-iblnr, ls_iblnr_sum-gjahr, ls_iblnr_sum-sum_diff)
              WHERE (LT_IBLNR_OPTION)
              AND   B~IBLNR IN R_IBLNR_TOT
              AND   B~GJAHR IN R_GJAHR_TOT
              AND   B~DMBTR NE 0
              GROUP BY B~IBLNR B~GJAHR
              HAVING (LT_HAVING_OPTIONS).   "SUM( b~dmbtr ) > lv_diff_amount
              "OR     SUM( b~dmbtr ) < lv_diff_amount_.
              APPEND LS_IBLNR_SUM TO LT_IBLNR_SUM.
            ENDSELECT.
          ENDIF.
          IF LT_IBLNR_SUM[] IS NOT INITIAL.
            CLEAR: LV_SEL_ISEG1, LV_SEL_IKPF, LV_SEL_CLAUSE.
            _BUILD_SQL_SEL_CLAUSE 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' LV_SEL_IKPF.
            _BUILD_SQL_SEL_CLAUSE 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' LV_SEL_ISEG1.
            CONCATENATE LV_SEL_IKPF LV_SEL_ISEG1 INTO LV_SEL_CLAUSE SEPARATED BY ' '.
            CONCATENATE LV_SEL_CLAUSE 'a~USNAM as USNAM_HD' INTO LV_SEL_CLAUSE SEPARATED BY ' '.
            SELECT (LV_SEL_CLAUSE)  "   *
               FROM IKPF AS A INNER JOIN ISEG AS B ON  A~IBLNR EQ B~IBLNR
                                                   AND A~GJAHR EQ B~GJAHR
               INTO CORRESPONDING FIELDS OF TABLE LT_DATA
               FOR ALL ENTRIES IN LT_IBLNR_SUM
               WHERE A~IBLNR EQ LT_IBLNR_SUM-IBLNR
               AND   A~GJAHR EQ LT_IBLNR_SUM-GJAHR
               AND   B~DMBTR IN R_DMBTR.
          ENDIF.
        ENDIF.
        IF SY-SUBRC = 0 AND LT_DATA IS NOT INITIAL.
          APPEND LINES OF LT_DATA TO T_DATA[].
          CLEAR: LT_DATA.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF LV_AGG_LVL = ''. "Single
      SELECT (LV_SEL_CLAUSE)
         FROM IKPF AS A
         INNER JOIN ISEG AS B ON  A~IBLNR = B~IBLNR
                              AND A~GJAHR = B~GJAHR
         INTO CORRESPONDING FIELDS OF TABLE T_DATA
         WHERE  A~VGART IN R_VGART
           AND  B~WERKS IN R_WERKS
           AND  B~LGORT IN R_LGORT
           AND  B~SOBKZ IN R_SOBKZ
           AND  B~USNAM IN R_USNAM
           AND  A~USNAM IN R_USNAM_HD
           AND  A~SPERR IN R_SPERR
           AND  A~ZSTAT IN R_ZSTAT
           AND  A~DSTAT IN R_DSTAT
           AND  A~LSTAT IN R_LSTAT
           AND  A~XBUFI IN R_XBUFI
           AND  B~WAERS IN R_WAERS   "10/02/18
           AND A~BLDAT IN R_BLDAT
           AND A~GIDAT IN R_GIDAT
           AND B~ZLDAT IN R_ZLDAT
           AND B~BUDAT IN R_BUDAT
           AND B~DMBTR IN R_DMBTR
           AND ( B~DMBTR >= LV_DIFF_AMOUNT OR B~DMBTR =< LV_DIFF_AMOUNT_ )
               .
    ELSEIF LV_AGG_LVL = 'WERKS'. "Plant(WERKS)
      REFRESH LT_WERKS.
      SELECT B~WERKS B~BUDAT SUM( B~DMBTR )
        FROM IKPF AS A
        INNER JOIN ISEG AS B
        ON A~IBLNR = B~IBLNR
        AND A~GJAHR = B~GJAHR
        INTO (LS_WERKS-WERKS, LS_WERKS-BUDAT, LS_WERKS-SUM_DIFF)
        WHERE  A~VGART IN R_VGART
          AND  B~WERKS IN R_WERKS
          AND  B~LGORT IN R_LGORT
          AND  B~SOBKZ IN R_SOBKZ
          AND  B~USNAM IN R_USNAM
          AND  A~USNAM IN R_USNAM_HD
          AND  A~SPERR IN R_SPERR
          AND  A~ZSTAT IN R_ZSTAT
          AND  A~DSTAT IN R_DSTAT
          AND  A~LSTAT IN R_LSTAT
          AND  A~XBUFI IN R_XBUFI
          AND  B~WAERS IN R_WAERS   "10/02/18
          AND A~BLDAT IN R_BLDAT
          AND A~GIDAT IN R_GIDAT
          AND B~ZLDAT IN R_ZLDAT
          AND B~BUDAT IN R_BUDAT
          AND B~DMBTR <> 0
          GROUP BY B~WERKS B~BUDAT
          HAVING SUM( B~DMBTR ) > LV_DIFF_AMOUNT OR
                 SUM( B~DMBTR ) < LV_DIFF_AMOUNT_.
        APPEND LS_WERKS TO LT_WERKS.
      ENDSELECT.
      IF LT_WERKS[] IS NOT INITIAL.
        CLEAR: LV_SEL_IKPF, LV_SEL_ISEG1, LV_SEL_CLAUSE.
        _BUILD_SQL_SEL_CLAUSE 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' LV_SEL_IKPF.
        _BUILD_SQL_SEL_CLAUSE 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' LV_SEL_ISEG1.
        CONCATENATE LV_SEL_IKPF LV_SEL_ISEG1 INTO LV_SEL_CLAUSE SEPARATED BY ' '.
        CONCATENATE LV_SEL_CLAUSE 'a~USNAM as USNAM_HD' INTO LV_SEL_CLAUSE SEPARATED BY ' '.
        SELECT (LV_SEL_CLAUSE)  "   *
         FROM IKPF AS A
         INNER JOIN ISEG AS B
         ON A~IBLNR = B~IBLNR
         AND A~GJAHR = B~GJAHR
         INTO CORRESPONDING FIELDS OF TABLE T_DATA
         FOR ALL ENTRIES IN LT_WERKS
         WHERE  A~VGART IN R_VGART
           AND  B~WERKS = LT_WERKS-WERKS
           AND  B~BUDAT = LT_WERKS-BUDAT
           AND  B~LGORT IN R_LGORT
           AND  B~SOBKZ IN R_SOBKZ
           AND  B~USNAM IN R_USNAM
           AND  A~USNAM IN R_USNAM_HD
           AND  A~SPERR IN R_SPERR
           AND  A~ZSTAT IN R_ZSTAT
           AND  A~DSTAT IN R_DSTAT
           AND  A~LSTAT IN R_LSTAT
           AND  A~XBUFI IN R_XBUFI
           AND  B~WAERS IN R_WAERS   "10/02/18
     "      and a~BLDAT in R_BLDAT
           AND A~GIDAT IN R_GIDAT
           AND B~ZLDAT IN R_ZLDAT  "?????
           AND B~DMBTR IN R_DMBTR.  "     <> 0
      ENDIF.
*    "****************************************************
    ELSEIF LV_AGG_LVL = 'IBLNR'. "Document (IBLNR)
      "--- Get IBLNR
      REFRESH LT_IBLNR.
      SELECT DISTINCT B~IBLNR B~GJAHR
        FROM IKPF AS A
        INNER JOIN ISEG AS B ON  A~IBLNR EQ B~IBLNR
                             AND A~GJAHR EQ B~GJAHR
        INTO CORRESPONDING FIELDS OF TABLE LT_IBLNR
        WHERE  A~VGART IN R_VGART
        AND    B~WERKS IN R_WERKS
        AND    B~LGORT IN R_LGORT
        AND    B~SOBKZ IN R_SOBKZ
        AND    B~USNAM IN R_USNAM
        AND    A~USNAM IN R_USNAM_HD
        AND    A~SPERR IN R_SPERR
        AND    A~ZSTAT IN R_ZSTAT
        AND    A~DSTAT IN R_DSTAT
        AND    A~LSTAT IN R_LSTAT
        AND    A~XBUFI IN R_XBUFI
        AND    B~WAERS IN R_WAERS
        AND    A~BLDAT IN R_BLDAT
        AND    A~GIDAT IN R_GIDAT
        AND    B~ZLDAT IN R_ZLDAT
        AND    B~BUDAT IN R_BUDAT
        AND    B~DMBTR NE 0.
*
      IF LT_IBLNR[] IS NOT INITIAL.
        REFRESH: R_IBLNR_TOT,
                 R_GJAHR_TOT.
        LOOP AT LT_IBLNR INTO LS_IBLNR.
          RS_IBLNR_TOT-SIGN   = 'I'.
          RS_IBLNR_TOT-OPTION = 'EQ'.
          RS_IBLNR_TOT-LOW    = LS_IBLNR-IBLNR.
          APPEND RS_IBLNR_TOT TO R_IBLNR_TOT.
          RS_GJAHR_TOT-SIGN   = 'I'.
          RS_GJAHR_TOT-OPTION = 'EQ'.
          RS_GJAHR_TOT-LOW    = LS_IBLNR-GJAHR.
          APPEND RS_GJAHR_TOT TO R_GJAHR_TOT.
        ENDLOOP.
        SORT: R_IBLNR_TOT,
              R_GJAHR_TOT.
        DELETE ADJACENT DUPLICATES FROM: R_IBLNR_TOT,
                                         R_GJAHR_TOT.
        REFRESH LT_IBLNR_SUM.
        SELECT B~IBLNR B~GJAHR SUM( B~DMBTR )
          FROM ISEG AS B
           INTO (LS_IBLNR_SUM-IBLNR, LS_IBLNR_SUM-GJAHR, LS_IBLNR_SUM-SUM_DIFF)
           WHERE B~IBLNR IN R_IBLNR_TOT
           AND   B~GJAHR IN R_GJAHR_TOT
           AND   B~DMBTR NE 0
           GROUP BY B~IBLNR B~GJAHR
           HAVING SUM( B~DMBTR ) > LV_DIFF_AMOUNT OR
                  SUM( B~DMBTR ) < LV_DIFF_AMOUNT_.
          APPEND LS_IBLNR_SUM TO LT_IBLNR_SUM.
        ENDSELECT.
      ENDIF.
      IF LT_IBLNR_SUM[] IS NOT INITIAL.
        CLEAR: LV_SEL_IKPF, LV_SEL_ISEG1, LV_SEL_CLAUSE.
        _BUILD_SQL_SEL_CLAUSE 'IKPF' '/SKN/S_SW_10_02_INVENT_CNT' 'a' ' ' LV_SEL_IKPF.
        _BUILD_SQL_SEL_CLAUSE 'ISEG' '/SKN/S_SW_10_02_INVENT_CNT' 'b' ' ' LV_SEL_ISEG1.
        CONCATENATE LV_SEL_IKPF LV_SEL_ISEG1 INTO LV_SEL_CLAUSE SEPARATED BY ' '.
        CONCATENATE LV_SEL_CLAUSE 'a~USNAM as USNAM_HD' INTO LV_SEL_CLAUSE SEPARATED BY ' '.
        SELECT (LV_SEL_CLAUSE)  "   *
           FROM IKPF AS A INNER JOIN ISEG AS B ON  A~IBLNR EQ B~IBLNR
                                               AND A~GJAHR EQ B~GJAHR
           INTO CORRESPONDING FIELDS OF TABLE T_DATA
           FOR ALL ENTRIES IN LT_IBLNR_SUM
           WHERE A~IBLNR EQ LT_IBLNR_SUM-IBLNR
           AND   A~GJAHR EQ LT_IBLNR_SUM-GJAHR
           AND   B~DMBTR IN R_DMBTR.  "     <> 0
      ENDIF.
    ENDIF.
  ENDIF.
*** End Yuri C.++ 04.02.19
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
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
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
******************************************************************************
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    ""    t_data-ABS_DMBTR = abs( t_data-DMBTR ).
    IF T_DATA-MENGE < T_DATA-BUCHM.
      T_DATA-ABS_DMBTR = T_DATA-DMBTR * ( -1 ).
    ELSE.
      T_DATA-ABS_DMBTR = T_DATA-DMBTR.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
********************************************************************************
  LOOP AT T_DATA.
**Material desc
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
      EXPORTING
        MATNR         = T_DATA-MATNR
        LANGU         = LV_LANGU
      IMPORTING
        MATERIAL_DESC = T_DATA-MAT_DESC
      EXCEPTIONS
        WRONG_CODE    = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
*** Begin Yuri C.++ 11.12.19
    T_DATA-REF_FIELD_NAME1 = LV_REF_FIELD1.
    T_DATA-REF_FIELD_NAME2 = LV_REF_FIELD2.
    T_DATA-WAERS_FR        = LV_WAERS_FR.
*** End Yuri C.++ 11.12.19
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*****************************************************************
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```