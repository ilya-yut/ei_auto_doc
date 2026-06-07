# Exception Indicator: Inventory count - Inventory Document level (IBLNR) ( SW_10_02_INV_CNT_DOC)

## General Overview

This Exception Indicator monitors physical inventory count documents at inventory-document level and surfaces count lines with material differences based on configurable aggregation, amount thresholds, and date filters.

This EI serves as an essential control for inventory and warehouse operations by:

- Identifying inventory count documents (IKPF/ISEG) with non-zero or threshold-level differences in local currency
- Supporting aggregation at line, plant, or inventory-document level before detail lines are returned
- Enabling comparison of two configurable amount fields from reference tables when currency conversion is required
- Applying posting, document, planned-count, or last-count date windows aligned with how finance and warehouse review cycles
- Supporting duration-based aging so teams can target counts that are exactly N days old, not only rolling windows

Typical use includes cycle-count reviews, post-count reconciliation, and audit sampling of documents with material valuation differences. Results are intended for exception workflows rather than full inventory document extracts.

The routine builds dynamic selections on inventory header and item tables, applies difference and comparison rules, enriches output with material descriptions, and raises an alert when qualifying count lines remain.


## Problem Description

Failure to monitor inventory count differences at document level creates multiple risks across inventory accuracy, financial posting, and operational control.

**Inventory and Valuation Risks**

- Unreviewed count differences can distort stock valuations and subsequent goods movements
- Differences spread across many documents or plants are harder to detect without aggregated exception reporting
- Count lines with zero difference may clutter reviews unless explicitly included

**Operational Risks**

- Plant-level or document-level aggregation mismatches can hide material lines that need recount or adjustment
- Date windows that do not align with posting or count dates can miss recent exceptions or include obsolete documents

**Control and Audit Risks**

- Lack of repeatable monitoring weakens evidence that count differences were reviewed before period close
- Threshold and comparison-field settings that are unclear can produce false positives or missed exceptions

## Suggested Resolution

**Immediate Response**

- Review flagged inventory documents, materials, plants, difference amounts, and posting dates
- Confirm with warehouse and inventory accounting whether each difference is expected or requires recount or adjustment
- Prioritize documents with large absolute differences or sensitive materials

**System Assessment**

- Validate aggregation level (line, plant, or document) against how the business triages count exceptions
- Compare exception volume to prior runs using the same date window and difference threshold
- Revisit reference table, field, and currency settings when comparison-based results seem inconsistent

**Corrective Actions**

- Post or reverse inventory differences through standard physical inventory processes with required approvals
- Adjust monitoring parameters after root cause so the queue stays actionable
- Document review outcomes for audit trail and schedule recurring runs for relevant plants and document types


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AGG_LVL | Agg.Level | CHAR | 30 | 0 | /SKN/E_SW_AGG_LVL | /SKN/D_SW_AGG_LVL |
| 2 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 3 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 4 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 5 | BUKRS |  |  |  |  |  |  |
| 6 | BWKEY |  |  |  |  |  |  |
| 7 | COMP_OPERATOR | Operator for comparison | CHAR | 2 | 0 | BUCC_OPERATOR | BUCC_OPERATOR |
| 8 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 9 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 10 | DIFF_AMOUNT | difference amount |  | 0 | 0 |  |  |
| 11 | DSTAT | Adjustment status | CHAR | 1 | 0 | DSTAT | DSTAT |
| 12 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 13 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 14 | GIDAT | Planned count date | DATS | 8 | 0 | GIDAT | DATUM |
| 15 | KTOPL |  |  |  |  |  |  |
| 16 | LANGU | Language |  | 0 | 0 |  |  |
| 17 | LGORT | Storage Location | CHAR | 4 | 0 | LGORT_D | LGORT |
| 18 | LSTAT | "Delete" status | CHAR | 1 | 0 | LSTAT | DSTAT |
| 19 | MANAGE_IN_UTC |  | 0 | 0 |  |  |  |
| 20 | PRESENT_ZERO | 'X' - Present Zero |  | 0 | 0 |  |  |
| 21 | REF_FIELD1 | Ref field name 1 | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 22 | REF_FIELD2 | Ref field name 2 | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 23 | REF_TABNAME1 | Table name(for REF.FIELD1) |  | 0 | 0 |  |  |
| 24 | REF_TABNAME2 | Table name(for REF.FIELD2) |  | 0 | 0 |  |  |
| 25 | RESULT_COMP | Value to Compare | CURR | 15 | 2 |  |  |
| 26 | SOBKZ | Special Stock | CHAR | 1 | 0 | SOBKZ | SOBKZ |
| 27 | SPERR | Posting Block | CHAR | 1 | 0 | ISPER | XFELD |
| 28 | SW_DEST |  | 0 | 0 |  |  |  |
| 29 | USNAM | Changed by(Item lvl) | CHAR | 12 | 0 | USNAA | USNAM |
| 30 | USNAM_HD | User name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 31 | VGART | Trans./Event Type | CHAR | 2 | 0 | VGART | VGART |
| 32 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 33 | WAERS_FR | Foreign Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 34 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 35 | XBUFI | Freeze book invntory | CHAR | 1 | 0 | XBUFI | XFELD |
| 36 | ZLDAT | Count date | DATS | 8 | 0 | DZLDAT | DATUM |
| 37 | ZSTAT | Count status | CHAR | 1 | 0 | DZSTAT | ZSTAT |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 37 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**AGG_LVL** (Agg.Level)

Controls how count-line difference amounts are rolled up before detail lines are returned. Empty value evaluates each item line on its own difference amount. **WERKS** sums differences by plant and posting date, then returns all lines that belong to qualifying plant-date groups. **IBLNR** sums by inventory document and fiscal year, then returns every line for documents whose rolled-up difference crosses the threshold band.

**AGG_LVL Options:**
- Empty — line or item level using each count line’s own difference amount.
- **WERKS** — plant and posting-date rollup before detail lines.
- **IBLNR** — inventory document and fiscal-year rollup before detail lines.

**BACKDAYS** (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BLDAT** (Document Date)

Document date from the source business document, often used as legal/document reference date.

**BUDAT** (Posting Date)

Posting date used to align analysis with accounting period recognition.

**BUKRS** (BUKRS)

Company code key that scopes data to legal entity/accounting unit level.

**BWKEY** (BWKEY)

Valuation area key joining material valuation to plant/company rules for moving-average or standard price.

**COMP_OPERATOR** (Operator for comparison)

<mark>Comparison operator used to evaluate thresholds (equal, less-than, greater-than, etc.).</mark>

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- BLDAT — Document date from the source business document, often used as legal/document reference date.
- GIDAT — Planned Count Date defines the specific calendar date on which the physical inventory tracking and counting of the materials is scheduled to take place.
- ZLDAT — Date of Last Count - calendar date when the last physical inventory count took place.
- BUDAT — Posting date used to align analysis with accounting period recognition.

**DATUM** (DATS)

Reference date supplied for the run (defaults to the current date when not set). Used together with **BACKDAYS** and **DATE_REF_FLD** to anchor the monitoring window on the chosen inventory date field.

**DIFF_AMOUNT** (difference amount)

Symmetric threshold for the rolled-up or line-level difference amount. The routine builds a positive and negative bound (threshold and its mirror) so both over- and under-shoots qualify. The effective comparison level follows **AGG_LVL** (line, plant-date, or document-year).

**DSTAT** (Adjustment status)

Physical Inventory Adjustment Status, which indicates whether stock differences found during a physical count have been posted and financially adjusted.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**GIDAT** (Planned count date)

<mark>Planned Count Date defines the specific calendar date on which the physical inventory tracking and counting of the materials is scheduled to take place.</mark>

**KTOPL** (KTOPL)

Chart of accounts governing GL account numbering, groups, and financial statement versions.

**LANGU** (Language)

Language key used for language-dependent texts and user-language filtering.

**LGORT** (Storage Location)

Storage location used to segment stock/logistics movements by warehouse sub-location.

**LSTAT** ("Delete" status)

Deletion Status of a physical inventory document, indicating with an X whether the entire document header has been flagged as deleted.

**MANAGE_IN_UTC** (MANAGE_IN_UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**PRESENT_ZERO** ('X' - Present Zero)

<mark>Include-zero or zero-balance presentation flag controlling whether zero metrics appear in output.</mark>

**REF_FIELD1 - REF_FIELD2** (Ref field name 1)

Pair of amount fields on the tables named in **REF_TABNAME1** and **REF_TABNAME2**. Used in the extended comparison path when document currency and conversion settings apply; otherwise the standard item difference amount drives qualification. Leave the second pair empty when only one reference amount is needed.

**REF_TABNAME1 - REF_TABNAME2** (Table name(for REF.FIELD1))

Inventory-related tables that supply the reference amounts for extended comparison (for example count item segment for document currency logic). The second table is optional for two-sided comparison.

**RESULT_COMP** (Value to Compare)

<mark>Right-hand comparison operand (literal or bound value) evaluated against extracted metrics in alert logic.</mark>

**SOBKZ** (Special Stock)

Special stock indicator used to distinguish stock ownership categories.

**SPERR** (Posting Block)

Blocking or lock indicator marking master data or transactions as administratively blocked from use.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**USNAM** (Changed by(Item lvl))

SAP changed-by/created-by user field used for accountability filtering.

**USNAM_HD** (User name)

<mark>User name on header rows distinguishing header actor fields from item-level user attributes.</mark>

**VGART** (Trans./Event Type)

Transaction type on the material document header classifying the inventory posting category.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WAERS_FR** (Foreign Currency)

Source/from currency key used in currency-change/translation contexts.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**XBUFI** (Freeze book invntory)

<mark>Freeze Book Inventory Indicator is a flag that freezes the recorded stock balance at the time of a physical inventory count so ongoing warehouse goods movements do not alter the final difference calculation.</mark>

**ZLDAT** (Count date)

Date of Last Count - calendar date when the last physical inventory count took place.

**ZSTAT** (Count status)

Count status flag tracks whether the items on the physical inventory document have been counted.


### Parameter Relationships

How parameter combinations work together

**Aggregation path:** **AGG_LVL** selects how differences are rolled up before detail lines are returned: empty (line level), **WERKS** (plant and posting date), or **IBLNR** (inventory document and fiscal year). **DIFF_AMOUNT** and **RESULT_COMP** define the symmetric difference band applied in having/selection logic.

**Date window:** When the monitor date range is empty, **BACKDAYS** is the fallback that builds a lower bound applied to the field named in **DATE_REF_FLD**; explicit date selections override that fallback.

**Duration filter:** After date selection, **DURATION** with **DURATION_UNIT** is an additional age filter on the reference date field named in **DATE_REF_FLD**.

**Difference scope:** **PRESENT_ZERO** controls whether zero **DMBTR** differences are excluded; when empty, non-zero differences are required.

**Comparison fields:** **REF_TABNAME1**, **REF_FIELD1**, **REF_TABNAME2**, **REF_FIELD2**, and **WAERS_FR** configure optional cross-field amount comparison with currency handling via **WAERS**, **BWKEY**, **BUKRS**, and **KTOPL** when used. **COMP_OPERATOR** is selected but the routine applies the symmetric **DIFF_AMOUNT** band through **RESULT_COMP** instead of the operator value.

**Organizational filters:** **VGART**, **WERKS**, **LGORT**, **SOBKZ**, status fields, and user parameters narrow which inventory documents and items enter the selection.

**Execution and text:** **SW_DEST** delegates to the cloud function when set; **LANGU** drives material descriptions; **MANAGE_IN_UTC** applies framework UTC handling when set.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DATE_REF_FLD** - initial - treated as BUDAT by code
- **DURATION_UNIT** - initial - treated as D by code
- **DIFF_AMOUNT** - initial - treated as 0 by code
- **LANGU** - initial - treated as E by code
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Document-level differences since yesterday**

**Purpose:** List inventory documents with differences aggregated at IBLNR level for postings since yesterday.

```
AGG_LVL = IBLNR
VGART = PI
BACKDAYS = 1
DATE_REF_FLD = BUDAT
DURATION_UNIT = D
```

**Use Case 2: Plant-level rollup**

**Purpose:** Find plants and posting dates where summed count differences exceed the default threshold band.

```
AGG_LVL = WERKS
WERKS = 1000
DIFF_AMOUNT = 100
DATE_REF_FLD = BUDAT
BACKDAYS = 7
```

**Use Case 3: Line-level with storage location**

**Purpose:** Return individual count item lines for one plant and storage location.

```
WERKS = 1000
LGORT = 0001
BACKDAYS = 30
```

**Use Case 4: Planned count date window**

**Purpose:** Monitor documents whose planned count date falls in the last fourteen days.

```
DATE_REF_FLD = GIDAT
BACKDAYS = 14
VGART = PI
WERKS = 2000
```

**Use Case 5: Posting date exactly seven full days ago**

**Purpose:** Flag count lines whose reference posting date falls in the scope of exactly 7 full days ago when using full-day duration counting.

```
DATE_REF_FLD = BUDAT
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 30
AGG_LVL = IBLNR
```

**Use Case 6: Document-level USD difference on item amount**

**Purpose:** Find physical inventory documents from the last thirty days whose total item difference amount on ISEG (DMBTR) exceeds ±100 in USD, and return all count lines for those documents for review.

```
AGG_LVL = IBLNR
DIFF_AMOUNT = 100
REF_TABNAME1 = ISEG
REF_FIELD1 = DMBTR
WAERS = USD
BACKDAYS = 30
DATE_REF_FLD = BUDAT
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_02_INVENT_CNT | ABCIN | CC phys. inv. ind. | CHAR(1) | ABCIN |
| /SKN/S_SW_10_02_INVENT_CNT | ABS_DMBTR | Difference amount | CURR(13) | DIFWR |
| /SKN/S_SW_10_02_INVENT_CNT | AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_02_INVENT_CNT | AGG_LVL | Agg.Level | CHAR(30) | /SKN/E_SW_AGG_LVL |
| /SKN/S_SW_10_02_INVENT_CNT | ATTYP | Material Category | CHAR(2) | ATTYP |
| /SKN/S_SW_10_02_INVENT_CNT | BLDAT | Document Date | DATS(8) | BLDAT |
| /SKN/S_SW_10_02_INVENT_CNT | BSTAR | Stock Type | CHAR(1) | I_BSTAR |
| /SKN/S_SW_10_02_INVENT_CNT | BUCHM | Book quantity | QUAN(13) | BUCHM |
| /SKN/S_SW_10_02_INVENT_CNT | BUCHW | Book val. at SP | CURR(13) | BUCHW |
| /SKN/S_SW_10_02_INVENT_CNT | BUDAT | Posting Date | DATS(8) | BUDAT |
| /SKN/S_SW_10_02_INVENT_CNT | CHARG | Batch | CHAR(10) | CHARG_D |
| /SKN/S_SW_10_02_INVENT_CNT | COMP_OPERATOR | Operator | CHAR(2) | BUCC_OPERATOR |
| /SKN/S_SW_10_02_INVENT_CNT | DIWZL | Diff. value | CURR(13) | DIWZL |
| /SKN/S_SW_10_02_INVENT_CNT | DMBTR | Difference amount | CURR(13) | DIFWR |
| /SKN/S_SW_10_02_INVENT_CNT | DSTAT | Adjustment status | CHAR(1) | DSTAT |
| /SKN/S_SW_10_02_INVENT_CNT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_02_INVENT_CNT | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_02_INVENT_CNT | ERFME | Unit of Entry | UNIT(3) | ERFME |
| /SKN/S_SW_10_02_INVENT_CNT | ERFMG | Qty in un. of entry | QUAN(13) | I_ERFMG |
| /SKN/S_SW_10_02_INVENT_CNT | EXVKW | Sales Value | CURR(13) | EXVKW |
| /SKN/S_SW_10_02_INVENT_CNT | GIDAT | Planned count date | DATS(8) | GIDAT |
| /SKN/S_SW_10_02_INVENT_CNT | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /SKN/S_SW_10_02_INVENT_CNT | GRUND | Reason for inventory diff. | NUMC(4) | GRDIF |
| /SKN/S_SW_10_02_INVENT_CNT | IBLNR | Phys. Inventory Doc. | CHAR(10) | IBLNR |
| /SKN/S_SW_10_02_INVENT_CNT | KDAUF | Sales Order | CHAR(10) | KDAUF |
| /SKN/S_SW_10_02_INVENT_CNT | KDEIN | Sales Order Schedule | NUMC(4) | KDEIN |
| /SKN/S_SW_10_02_INVENT_CNT | KDPOS | Sales Order Item | NUMC(6) | KDPOS |
| /SKN/S_SW_10_02_INVENT_CNT | KUNNR | Customer | CHAR(10) | EKUNN |
| /SKN/S_SW_10_02_INVENT_CNT | KWART | Inventory val.-only mat | CHAR(1) | XWART |
| /SKN/S_SW_10_02_INVENT_CNT | LGORT | Storage Location | CHAR(4) | LGORT_D |
| /SKN/S_SW_10_02_INVENT_CNT | LIFNR | Vendor | CHAR(10) | ELIFN |
| /SKN/S_SW_10_02_INVENT_CNT | LSTAT | "Delete" status | CHAR(1) | LSTAT |
| /SKN/S_SW_10_02_INVENT_CNT | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_INVENT_CNT | MAT_DESC | Material Description | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_INVENT_CNT | MBLNR | Material Document | CHAR(10) | MBLNR |
| /SKN/S_SW_10_02_INVENT_CNT | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_02_INVENT_CNT | MENGE | Quantity | QUAN(13) | MENGE_D |
| /SKN/S_SW_10_02_INVENT_CNT | MJAHR | Material Doc. Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_02_INVENT_CNT | NBLNR | Recount document | CHAR(10) | NBLNR |
| /SKN/S_SW_10_02_INVENT_CNT | PLPLA | Distr. differences | CHAR(10) | PLPLA |
| /SKN/S_SW_10_02_INVENT_CNT | PS_PSP_PNR | WBS Element | NUMC(8) | PS_PSP_PNR |
| /SKN/S_SW_10_02_INVENT_CNT | REF_FIELD_NAME1 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_02_INVENT_CNT | REF_FIELD_NAME2 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_02_INVENT_CNT | RESULT_COMP | Value to Compare | CURR(15) |  |
| /SKN/S_SW_10_02_INVENT_CNT | SOBKZ | Special Stock | CHAR(1) | SOBKZ |
| /SKN/S_SW_10_02_INVENT_CNT | SPERR | Posting Block | CHAR(1) | ISPER |
| /SKN/S_SW_10_02_INVENT_CNT | USNAD | Adj. posting made by | CHAR(12) | USNAD |
| /SKN/S_SW_10_02_INVENT_CNT | USNAM | Changed by | CHAR(12) | USNAA |
| /SKN/S_SW_10_02_INVENT_CNT | USNAM_HD | User name | CHAR(12) | USNAM |
| /SKN/S_SW_10_02_INVENT_CNT | USNAZ | Counted By | CHAR(12) | USNAZ |
| /SKN/S_SW_10_02_INVENT_CNT | VGART | Trans./Event Type | CHAR(2) | VGART |
| /SKN/S_SW_10_02_INVENT_CNT | VKMZL | Diff.SalesPrs + VAT | CURR(13) | VKMZL |
| /SKN/S_SW_10_02_INVENT_CNT | VKNZL | Diff.SalesPr w/o VAT | CURR(13) | VKNZL |
| /SKN/S_SW_10_02_INVENT_CNT | VKWRA | Sales value w/o VAT | CURR(13) | VKWRA |
| /SKN/S_SW_10_02_INVENT_CNT | VKWRT | Sales Value inc. VAT | CURR(13) | VKWRT |
| /SKN/S_SW_10_02_INVENT_CNT | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_02_INVENT_CNT | WAERS_FR | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_02_INVENT_CNT | WERKS | Plant | CHAR(4) | WERKS_D |
| /SKN/S_SW_10_02_INVENT_CNT | WRTBM | Value book qty | CURR(13) | WRTBM |
| /SKN/S_SW_10_02_INVENT_CNT | WRTZL | Val. cntd qty | CURR(13) | WRTZL |
| /SKN/S_SW_10_02_INVENT_CNT | XAMEI | Alternative Unit | CHAR(1) | XAMEI |
| /SKN/S_SW_10_02_INVENT_CNT | XBLNI | Phys. Inventory Ref. | CHAR(16) | XBLNI1 |
| /SKN/S_SW_10_02_INVENT_CNT | XBUFI | Freeze book invntory | CHAR(1) | XBUFI |
| /SKN/S_SW_10_02_INVENT_CNT | XDIFF | Difference posted | CHAR(1) | XDIFF |
| /SKN/S_SW_10_02_INVENT_CNT | XLOEK | Item Deleted | CHAR(1) | I_XLOEK |
| /SKN/S_SW_10_02_INVENT_CNT | XNULL | Zero count | CHAR(1) | XNULL |
| /SKN/S_SW_10_02_INVENT_CNT | XNZAE | Recount | CHAR(1) | XNZAE |
| /SKN/S_SW_10_02_INVENT_CNT | XZAEL | Item counted | CHAR(1) | XZAEL |
| /SKN/S_SW_10_02_INVENT_CNT | ZEILE | Material Doc.Item | NUMC(4) | MBLPO |
| /SKN/S_SW_10_02_INVENT_CNT | ZEILI | Item | NUMC(3) | DZEILE |
| /SKN/S_SW_10_02_INVENT_CNT | ZLDAT | Count date | DATS(8) | DZLDAT |
| /SKN/S_SW_10_02_INVENT_CNT | ZSTAT | Count status | CHAR(1) | DZSTAT |

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
