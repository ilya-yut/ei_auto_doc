# Exception Indicator: PREQ vs PO value check ( SW_10_03_PR_PO_VAL_C)

## General Overview

This Exception Indicator identifies purchase requisition lines where the aggregated purchase order net value differs from the requisition valuation amount, surfacing cases where PO follow-up value no longer matches the approved requisition value.

This EI serves as an essential control for procurement and financial oversight by:

- Enabling detection of value mismatches between requisition valuation and linked purchase order net value
- Supporting review of requisitions subject to release before PO conversion proceeds at an inconsistent amount
- Providing visibility into total requisition value, total purchase order value, and the calculated difference on each flagged line
- Enabling age-based prioritization when value variances remain open after a chosen reference date
- Supporting audit sampling by purchasing organization, vendor, plant, and release attributes

Typical use includes buyer review before PO release, AP and procurement control samples, and periodic checks that PO follow-up respects requisition economics. Results are intended for exception workflows rather than operational MM list reporting.

The routine reads release-relevant purchase requisitions, aggregates linked purchase order net values, compares them to requisition valuation amounts, and raises an alert when the configured value difference threshold is met.


## Problem Description

Failure to monitor differences between purchase requisition valuation and linked purchase order value creates multiple risks across procurement control, budget discipline, and compliance:

**Procurement and Financial Risks**

- Purchase orders created above or below the requisition valuation can commit spend that was not approved at the requested amount
- Undetected value drift between requisition and PO can weaken three-way match and budget control before goods receipt or invoice
- Repeat variances for the same vendor or material can indicate pricing errors, master-data issues, or policy circumvention

**Operational Risks**

- Value-difference thresholds that are too loose can hide material mismatches; thresholds too tight can create reviewer fatigue
- Lookback and age settings misaligned with review cadence can exclude recent cases or retain stale rows
- Release and requisition status scope that is not tuned can mix closed or irrelevant lines into the variance queue

**Control and Audit Risks**

- Weak monitoring reduces evidence that requisition-to-PO value alignment was reviewed before commitment
- Lack of recurring exception review limits accountability for buyers and approvers when PO value changes after requisition approval
- Missing prioritization by difference magnitude or age delays escalation of high-impact variances

## Suggested Resolution

**Immediate Response**

- Review flagged requisitions for requisition value, aggregated purchase order value, and calculated difference
- Confirm with the buyer whether the PO amount change is authorized and supported by documentation
- Prioritize high-difference or long-open cases for release hold or PO correction

**System Assessment**

- Validate lookback window and value-difference threshold against how the team defines material variance
- Tune organizational, vendor, and release scope so results stay actionable
- Compare exception counts by purchasing group, document type, and vendor to find systematic pricing or conversion gaps

**Corrective Actions**

- Correct purchase order pricing or requisition valuation through standard MM processes where errors are confirmed
- Adjust monitoring thresholds after cleanup so results reflect truly material variances
- Document review outcomes and schedule recurring runs before close or high-volume PO conversion periods


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 2 | BADAT | Requisition Date | DATS | 8 | 0 | BADAT | DATUM |
| 3 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 4 | BEDAT | Purchase Order Date | DATS | 8 | 0 | BEDAT | DATUM |
| 5 | BNFPO | Item of Requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 6 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 7 | BSART | Document Type | CHAR | 4 | 0 | BBSRT | BSART |
| 8 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 9 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 10 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 11 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 12 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 13 | EBELN | Purchase Order | CHAR | 10 | 0 | BSTNR | EBELN |
| 14 | EBELP | Purchase Order Item | NUMC | 5 | 0 | BSTPO | EBELP |
| 15 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 16 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 17 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 18 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 19 | ERDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 20 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 21 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 22 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 23 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 24 | FRGKZ | Release indicator | CHAR | 1 | 0 | FRGKZ | FRGKZ |
| 25 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 26 | FRGST | Release Strategy | CHAR | 2 | 0 | FRGST | FRGST |
| 27 | FRGZU | Release status | CHAR | 8 | 0 | FRGZU | FRGZU |
| 28 | LANGU | Language |  | 0 | 0 |  |  |
| 29 | LFDAT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 30 | LIFNR | Desired Vendor | CHAR | 10 | 0 | WLIEF | LIFNR |
| 31 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 32 | MEINS | Unit of Measure | UNIT | 3 | 0 | BAMEI | MEINS |
| 33 | MENGE | Quantity Requested | QUAN | 13 | 3 | BAMNG | MENG13 |
| 34 | PEINH | Price Unit | DEC | 5 | 0 | EPEIN | DEC5 |
| 35 | PO_PREQ_DIFF | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 36 | PREIS | Valuation Price | CURR | 11 | 2 | BAPRE | WERT11 |
| 37 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 38 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 39 | STATU | Processing status | CHAR | 1 | 0 | BANST | BANST |
| 40 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 41 | TOT_PO_VAL | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 42 | TOT_PREQ_VAL | Valuation Price | CURR | 17 | 2 | /SKN/E_SW_TOT_PREQ_VAL | /SKN/D_SW_TOT_PREQ_VAL |
| 43 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 44 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 45 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 46 | WERKS_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 46 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**BACKDAYS** (Back days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERDAT

**BADAT** (Requisition Date)

Requisition date expressing when material is required-drives need-by and replenishment timing in MM.

**BANFN** (Purchase Requisition)

Purchase requisition number, the core PR document key for MM approval and lifecycle checks.

**BEDAT** (Purchase Order Date)

Purchasing document date used to filter procurement documents by document creation period.

**BNFPO** (Item of Requisition)

Purchase requisition item number used to identify PR line-level records.

**BSAKZ** (Control indicator)

Purchasing control/indicator flag used to segment procurement records by processing characteristics.

**BSART** (Document Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BSART_DESC** (Doc. Type Descript.)

Text description of purchasing document type used for readable reporting output.

**BSTYP** (Purch. Doc. Category)

Purchasing document category (PR/PO/contract etc.) used to segment MM document classes.

**BSTYP_DESC** (Short Descript.)

Description of purchasing document category for business-readable output.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**EBELN** (Purchase Order)

Purchasing document number (typically PO) used as the primary MM document key.

**EBELP** (Purchase Order Item)

Purchasing document item number used for line-level PO analytics.

**EKGRP** (Purchasing Group)

Purchasing group (buyer) used for procurement ownership and control segmentation.

**EKGRP_DESC** (Description p. group)

Description of purchasing group for readable buyer/team reporting.

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**EKORG_DESC** (Description)

Description of purchasing organization for business-readable reporting.

**ERDAT** (Changed on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ESTKZ** (Creation Indicator)

Creation indicator for PR/PO source or method, used for process-origin analysis.

**FRGC** (Release code)

Derived release code for each requisition from release group, strategy, and release status; filters rows to configured release-code values after derivation.

**FRGGR** (Release group)

Release group key controlling the purchasing release strategy framework.

**FRGKZ** (Release indicator)

Purchasing release state indicator on requisitions or orders showing whether and how release strategy applies.

**FRGRL** (Subject to release)

Release indicator/flag used in PO/PR release strategy control.

**FRGST** (Release Strategy)

Overall release status on purchasing documents summarizing approval strategy progress for PO/PR objects.

**FRGZU** (Release status)

Release strategy progression/status code used for approval lifecycle tracking.

**LANGU** (Language)

Language key used for language-dependent texts and user-language filtering.

**LFDAT** (Delivery Date)

Delivery date used for logistics due-date and fulfillment timeliness checks.

**LIFNR** (Desired Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**MEINS** (Unit of Measure)

Base unit of measure used to interpret quantity fields consistently.

**MENGE** (Quantity Requested)

Quantity field used for volumetric thresholds and variance analysis.

**PEINH** (Price Unit)

Price unit denominator used to interpret per-unit purchasing prices.

**PO_PREQ_DIFF** (Net Order Value)

Difference between aggregated purchase order net value and requisition valuation amount; rows are kept only when this calculated difference falls within the configured selection range.

**PREIS** (Valuation Price)

Valuation price on the purchase requisition line; combined with quantity and price unit to derive the total requisition valuation amount.

**RESWK** (Supplying Plant)

Supplying/Issuing Plant designates the specific internal plant from which materials are being transferred or procured during a Stock Transport Order. Used in cross-plant logistics analysis.

**RESWK_DESC** (Name 1)

Plant description text used to enrich plant-level reporting.

**STATU** (Processing status)

Status code used for state-based filtering in process monitoring.

**STATU_DESC** (Short Descript.)

Status description text used for readable status analytics.

**TOT_PO_VAL** (Net Order Value)

Aggregated net order value from purchase order lines linked to the requisition; converted using the order exchange rate when requisition and order currencies differ.

**TOT_PREQ_VAL** (Valuation Price)

Total requisition valuation amount calculated from requested quantity, valuation price, and price unit on the requisition line.

**VENDOR_DESC** (Name)

Vendor description text used for readable supplier-level reporting.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WERKS_DESC** (Name 1)

Plant name or description providing readable site context beside plant keys.


### Parameter Relationships

**Reference-date window:** When **DATUM** is empty, a lower bound of today minus **BACKDAYS** is applied with a greater-than-or-equal filter and copied to **BEDAT**, **BADAT**, or **ERDAT** per the configured date reference field (default requisition date in code). Explicit **BADAT**, **BEDAT**, or **ERDAT** selections override that fallback window.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference date to the evaluation date; rows outside the configured duration range are removed.

**Value comparison:** **TOT_PREQ_VAL** (requisition valuation from quantity and valuation price) and **TOT_PO_VAL** (aggregated linked PO net value, converted when currencies differ) combine to form **PO_PREQ_DIFF**; only rows whose difference falls within **PO_PREQ_DIFF** selection remain.

**Release scope:** **FRGRL**, **FRGGR**, non-empty **FRGST**, and derived **FRGC** narrow requisitions subject to release strategy and release-code filtering.

**Requisition scope:** **BANFN**, **BNFPO**, **BSART**, **EKORG**, **EKGRP**, **ERNAM**, **LIFNR**, **RESWK**, **WERKS**, **WAERS**, **LOEKZ**, **STATU**, **ESTKZ**, **FRGKZ**, and **LFDAT** define which requisition lines enter the comparison.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **LOEKZ** - blank

### Practical Example of Parameter Configuration

**Use Case 1: Material PO value above requisition**

**Purpose:** Flag requisitions where linked purchase order net value exceeds the requisition valuation by at least 100 in document currency.

```
PO_PREQ_DIFF = 100 - 999999999
BACKDAYS = 60
FRGRL = X
EKORG = 1000
```

**Use Case 2: Any non-zero variance in the last ninety days**

**Purpose:** Sample all requisition lines with any difference between PO and requisition value posted in the last ninety days.

```
PO_PREQ_DIFF = 0.01 - 999999999
BACKDAYS = 90
EKGRP = 001
BSTYP = B
```

**Use Case 3: Vendor-specific value review**

**Purpose:** Review value mismatches for one desired vendor before invoice matching.

```
LIFNR = 100000
PO_PREQ_DIFF = 50 - 999999999
BACKDAYS = 45
WERKS = 1000
```

**Use Case 4: Release group focus**

**Purpose:** Monitor value variances under one release group and derived release code.

```
FRGGR = 01
FRGC = 01
BACKDAYS = 30
EKORG = 1000
```

**Use Case 5: Exactly seven full days since reference date**

**Purpose:** Return rows whose reference date is exactly 7 full days ago for weekly variance escalation.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
BADAT = 20250101
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BADAT | Requisition Date | DATS(8) | BADAT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BANFN | Purchase Requisition | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BEDAT | Purchase Order Date | DATS(8) | BEDAT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BNFPO | Item of Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSAKZ | Control indicator | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSART | Document Type | CHAR(4) | BBSRT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSART_DESC | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSTYP | Purch. Doc. Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EBELN | Purchase Order | CHAR(10) | BSTNR |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EBELP | Purchase Order Item | NUMC(5) | BSTPO |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EKGRP_DESC | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EKORG_DESC | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | ERDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | ESTKZ | Creation Indicator | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGKZ | Release indicator | CHAR(1) | FRGKZ |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGRL | Subject to release | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGST | Release Strategy | CHAR(2) | FRGST |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | LFDAT | Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | LIFNR | Desired Vendor | CHAR(10) | WLIEF |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | MEINS | Unit of Measure | UNIT(3) | BAMEI |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | MENGE | Quantity Requested | QUAN(13) | BAMNG |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | PEINH | Price Unit | DEC(5) | EPEIN |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | PO_PREQ_DIFF | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | PREIS | Valuation Price | CURR(11) | BAPRE |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | RESWK | Supplying Plant | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | RESWK_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | STATU | Processing status | CHAR(1) | BANST |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | TOT_PO_VAL | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | TOT_PREQ_VAL | Valuation Price | CURR(17) | /SKN/E_SW_TOT_PREQ_VAL |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | VENDOR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | WERKS_DESC | Name 1 | CHAR(30) | NAME1 |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_10_03_PR_PO_VAL_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PR_PO_VAL_CHK OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  LV_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  LV_DATE_REF_FLD = 'BADAT'. "PO date
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT.
  DATA_MULTY: LFDAT        EINDT,
              BANFN        BANFN,
              BNFPO        BNFPO,
              BSART        BBSRT,
              BSTYP        BSTYP,
              BSAKZ        BSAKZ,
              STATU        BANST,
              ESTKZ        ESTKZ,
              FRGKZ        FRGKZ,
              EKGRP        EKGRP,
              ERNAM        ERNAM,
              FRGRL        FRGRL,
              EBELN        BSTNR,
              EKORG        EKORG,
              FRGGR        FRGGR,
              FRGST        FRGST,
              RESWK        RESWK,
              WERKS	       EWERK,
              LIFNR        ELIFN,
              ERDAT        AEDAT,
              BEDAT        BEDAT,
              BADAT        BADAT,
              WAERS        WAERS,
              DATUM        SY-DATUM,
              DURATION    /SKN/E_SW_DURATION,
              LOEKZ       ELOEK,
              FRGC        FRGCO,
              PO_PREQ_DIFF BWERT.
  SELECT_MULTY:
              LFDAT,
              BANFN,
              BNFPO,
              BSART,
              BSTYP,
              BSAKZ,
              STATU,
              ESTKZ,
              FRGKZ,
              EKGRP,
              ERNAM,
              ERDAT,"Changed on
              FRGRL,
              EBELN,
              EKORG,
              FRGGR,
              FRGST,
              RESWK,
              WERKS,
              LIFNR,
              BEDAT,
              BADAT,
              WAERS,
              DATUM,
              DURATION,
              LOEKZ,
              FRGC,
              PO_PREQ_DIFF.
  CONVERT_MULTY: EBELN ALPHA,
                 LIFNR ALPHA.
  "--- Set default for LOEKZ (not deleted only)
  READ TABLE R_LOEKZ INTO RS_LOEKZ INDEX 1.
  IF SY-TFILL = 0.
    RS_LOEKZ-SIGN = 'I'.
    RS_LOEKZ-OPTION = 'EQ'.
    RS_LOEKZ-LOW = ' '.
    APPEND RS_LOEKZ TO R_LOEKZ.
  ENDIF.
  RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
           R_FLD_VAL FOR DD03P-FIELDNAME .
  DATA : FLD_NAME TYPE FIELDNAME.
  DATA : I TYPE I,
         CI(1) TYPE C,
         NFIELDS TYPE I VALUE 3.   "
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM .
  DATA : LANGU LIKE SY-LANGU .
  DATA : IS_OUT(1) TYPE C.
  DATA : TIME_DIFF TYPE INT4 .
  DATA: LV_DOMNAME  LIKE DD07V-DOMNAME,
        LV_DOMVALUE LIKE DD07V-DOMVALUE_L,
        LV_DDTEXT   LIKE DD07V-DDTEXT.
  DATA: LV_FRGCO    TYPE FRGCO.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA : LS_VBPA TYPE VBPA,
         LT_VBPA LIKE TABLE OF LS_VBPA.
  DATA : LV_DATA_POSNR TYPE POSNR.
**********
  DATA: LV_FRGSX TYPE FRGSX.
  DATA: LV_RESWK TYPE WERKS_D.
  DATA: LV_WERKS TYPE WERKS_D.
  DATA: LV_WLIEF TYPE LIFNR.
  DATA: LV_BSART TYPE ESART.
  DATA: LS_T161 TYPE T161,
        LT_T161 LIKE TABLE OF LS_T161.
  DATA: LS_EBAN TYPE EBAN,
        LT_EBAN LIKE TABLE OF LS_EBAN.
  DATA: BEGIN OF LS_PO,
         BANFN  TYPE BANFN,
         BNFPO TYPE BNFPO,
         EBELN TYPE EBELN,
         EBELP TYPE EBELP,
         LIFNR TYPE WLIEF,
         EKORG TYPE EKORG,
         NETWR TYPE BWERT,
         WKURS TYPE WKURS,
         PO_WAERS TYPE WAERS,
         BEDAT TYPE EBDAT,
         NETWR_LOCAL_CURR TYPE BWERT,
         PR_WAERS TYPE WAERS,
        END OF LS_PO.
  DATA: LT_PO LIKE TABLE OF LS_PO.
  DATA: BEGIN OF LS_PO_PR,
         BANFN  TYPE BANFN,
         BNFPO TYPE BNFPO,
         LIFNR TYPE WLIEF,
         EKORG TYPE EKORG,
         NETWR_LOCAL_CURR TYPE BWERT,
        END OF LS_PO_PR.
  DATA: LT_PO_PR LIKE TABLE OF LS_PO_PR.
  DATA: LV_FIELD               TYPE STRING,
        LV_TABNAME             TYPE TABNAME,
        LS_DATA                TYPE TAB512,
        LT_DATA                LIKE TABLE OF LS_DATA,"   TYPE usmd_tt_tab512,
        LS_FIELDS              TYPE RFC_DB_FLD,
        LT_FIELDS              TYPE TABLE OF RFC_DB_FLD,
        LT_OPTION              TYPE TABLE OF RFC_DB_OPT,
        LS_OPTION              LIKE LINE OF LT_OPTION[],
        LT_DATA_RFC            TYPE TABLE OF /SKN/S_SW_TAB2000,
        LT_TABLES_LIST         TYPE /SKN/TT_TABLES,
        LWA_TABLES_LIST        LIKE LINE OF LT_TABLES_LIST[],
        LT_JOIN_CONDITION      TYPE /SKN/TT_TABLE_JOIN,
        LWA_JOIN_CONDITION     LIKE LINE OF LT_JOIN_CONDITION[],
        LS_SEL_FIELDS          TYPE /SKN/S_SEL_FIELDS,
        LT_SEL_FIELDS          TYPE /SKN/TT_SEL_FIELDS,
        LT_OUTPUT_FIELDS       TYPE /SKN/TT_RFC_DB_FLD_EXTEND,
        LT_DFIES               TYPE TABLE OF  DFIES,
        LT_RETURN              TYPE BAPIRET2_T,
        LS_SORT_OPTIONS        TYPE /SKN/S_SW_RFC_JOIN_DB_SORT,
        LT_SORT_OPTIONS        TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT,
        LT_IN_RANGE            TYPE TABLE OF /SKN/S_SW_RANGE_TAB,
        LT_OUT_WHERE_COND      TYPE TABLE OF /SKN/S_SW_WHERE_TAB,
        LWA_IN_RANGE           LIKE LINE OF  LT_IN_RANGE,
        LWA_OUT_WHERE_COND     LIKE LINE OF LT_OUT_WHERE_COND,
        LWA_ALL_ENTRIES_TAB    TYPE /SKN/S_SW_TAB6000,
        LT_ALL_ENTRIES_TAB     TYPE TABLE OF /SKN/S_SW_TAB6000,
        LWA_ALL_ENTRIES_COND   TYPE /SKN/S_TABLE_JOIN,
        LT_ALL_ENTRIES_COND    TYPE TABLE OF /SKN/S_TABLE_JOIN,
        LT_ALL_ENTRIES_DFIES   TYPE TABLE OF DFIES.
  FIELD-SYMBOLS:
        <FS_OUTPUT>            TYPE /SKN/RFC_DB_FLD_EXTEND.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  "--- Prepare BSART
**********************************************************************
**********************************************************************
**   select *
**     from T161
**     into CORRESPONDING FIELDS OF table lt_T161
**     where BSTYP = 'B'
**       and BSAKZ = ' '
**       and BSART in R_BSART.
  LV_TABNAME = 'T161'.
  _RANGE_TO_SEL_TABLE 'BSART'        BSART.
  LT_OPTION[] = LT_OUT_WHERE_COND[].
  IF LT_OPTION[] IS INITIAL.
    LS_OPTION-TEXT = 'BSTYP = ''B'''.
  ELSE.
    LS_OPTION-TEXT = 'AND BSTYP = ''B'''.
  ENDIF.
  APPEND LS_OPTION TO LT_OPTION.
  LS_OPTION-TEXT = 'AND BSAKZ = '' '''.
  APPEND LS_OPTION TO LT_OPTION.
  CALL FUNCTION 'RFC_READ_TABLE'
    DESTINATION LV_SW_DEST
    EXPORTING
      QUERY_TABLE          = LV_TABNAME
    TABLES
      OPTIONS              = LT_OPTION
      FIELDS               = LT_FIELDS
      DATA                 = LT_DATA
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  IF SY-SUBRC NE 0.
    CLEAR LT_DATA[].
  ENDIF.
  _RFC_TO_T_DATA_INDEX LT_DATA LT_T161 LT_FIELDS 1.
**********************************************************************
**********************************************************************
  REFRESH R_BSART.
  LOOP AT LT_T161 INTO LS_T161.
    RS_BSART-SIGN = 'I'.
    RS_BSART-OPTION = 'EQ'.
    RS_BSART-LOW = LS_T161-BSART.
    APPEND RS_BSART TO R_BSART.
  ENDLOOP.
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'BEDAT'.
      R_BEDAT[] = R_DATUM[]. "Purchase Order Date
    WHEN 'BADAT'.
      R_BADAT[] = R_DATUM[]. "Request Order Date
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[]. "Changed On
    WHEN OTHERS.
      R_BADAT[] = R_DATUM[].
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  REFRESH LT_EBAN.
**********************************************************************
**********************************************************************
**  SELECT *
**    from EBAN
**    into CORRESPONDING FIELDS OF table lt_EBAN
**
**    where FRGRL in R_FRGRL    "  EQ 'X'
**      and FRGGR in R_FRGGR
**
**      and EBELN in R_EBELN
**      and BSTYP = 'F'  " PO !!! Remove for Test
**
**      and EKORG in R_EKORG
**
**      and LIFNR in R_LIFNR
**      and RESWK in R_RESWK
**      and BEDAT in R_BEDAT
**      and BADAT in R_BADAT
**      and ERDAT in R_ERDAT
**
**      and BSART in R_BSART
**      and EKGRP in R_EKGRP
**      and ERNAM in R_ERNAM
**      and WERKS	in R_WERKS
**      and WAERS in R_WAERS
**
**      """and LOEKZ  in R_LOEKZ
**      and LOEKZ = ' '
**      and FRGKZ in R_FRGKZ
**
**      and FRGST <>  ''
**      and STATU = 'B'
**      and ESTKZ in ('D', 'F', 'R')
**      and KNTTP <> ' '
**       .
* Join table list
  REFRESH LT_TABLES_LIST[].
  _APPEND_TABLES_LIST 'EBAN'       ''  ''.
  REFRESH LT_SEL_FIELDS[].
  _ADAPT_SEL_FIELDS 'EBAN'    'EBAN'  LT_SEL_FIELDS LV_SW_DEST.
* Range condition
  REFRESH: LT_OUT_WHERE_COND[], LT_OPTION[].
  _RANGE_TO_SEL_TABLE 'FRGRL'    FRGRL.
  _RANGE_TO_SEL_TABLE 'FRGGR'    FRGGR.
  _RANGE_TO_SEL_TABLE 'EBELN'    EBELN.
  _RANGE_TO_SEL_TABLE 'EKORG'    EKORG.
  _RANGE_TO_SEL_TABLE 'LIFNR'    LIFNR.
  _RANGE_TO_SEL_TABLE 'RESWK'    RESWK.
  _RANGE_TO_SEL_TABLE 'BEDAT'    BEDAT.
  _RANGE_TO_SEL_TABLE 'BADAT'    BADAT.
  _RANGE_TO_SEL_TABLE 'ERDAT'    ERDAT.
  _RANGE_TO_SEL_TABLE 'BSART'    BSART.
  _RANGE_TO_SEL_TABLE 'EKGRP'    EKGRP.
  _RANGE_TO_SEL_TABLE 'ERNAM'    ERNAM.
  _RANGE_TO_SEL_TABLE 'WERKS'    WERKS.
  _RANGE_TO_SEL_TABLE 'WAERS'    WAERS.
  _RANGE_TO_SEL_TABLE 'FRGKZ'    FRGKZ.
  _RANGE_TO_SEL_TABLE 'LFDAT'    LFDAT.
  LT_OPTION[] = LT_OUT_WHERE_COND.
***  IF lt_option[] IS INITIAL.
***    ls_option-text = 'BSTYP = ''F'''.
***  ELSE.
***    ls_option-text = 'AND BSTYP = ''F'''.
***  ENDIF.
***  APPEND ls_option TO lt_option.
  LS_OPTION-TEXT = 'AND LOEKZ = '' '''.
  APPEND LS_OPTION TO LT_OPTION.
  LS_OPTION-TEXT = 'AND FRGST <> '' '''.
  APPEND LS_OPTION TO LT_OPTION.
  LS_OPTION-TEXT = 'AND STATU = ''B'''.
  APPEND LS_OPTION TO LT_OPTION.
  LS_OPTION-TEXT = 'AND KNTTP <> '' '''.
  APPEND LS_OPTION TO LT_OPTION.
  LS_OPTION-TEXT = 'AND ESTKZ IN ( ''D'' , ''F'' , ''R'' )'.
  APPEND LS_OPTION TO LT_OPTION.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
    TABLES
      OPTIONS              = LT_OPTION[]
      DATA                 = LT_DATA_RFC[]
      TABLES_LIST          = LT_TABLES_LIST[]
      JOIN_CONDITION       = LT_JOIN_CONDITION[]
      SEL_FIELDS           = LT_SEL_FIELDS[]
      SORT_OPTIONS         = LT_SORT_OPTIONS[]
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
      DFIES                = LT_DFIES[]
      RETURN               = LT_RETURN[]
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  IF SY-SUBRC <> 0.
    CLEAR LT_DATA_RFC[].
  ELSE.
    _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_EBAN LT_OUTPUT_FIELDS 2.
  ENDIF.
**********************************************************************
**********************************************************************
  CHECK LT_EBAN[] IS NOT INITIAL.
  SORT LT_EBAN BY BANFN BNFPO.
**********************************************************************
**********************************************************************
**   SELECT EKPO~EBELN EKPO~EBELP
**          EKPO~NETWR EKKO~WAERS as PO_WAERS
**          EKKO~BEDAT EKKO~WKURS
**          EKKO~LIFNR EKKO~EKORG
**          EKET~BANFN EKET~BNFPO
**     into CORRESPONDING FIELDS OF TABLE lt_PO
**     from EKET
**       INNER JOIN EKPO
**         ON EKPO~EBELN = EKET~EBELN AND
**            EKPO~EBELP = EKET~EBELP
**       INNER JOIN EKKO
**         ON EKKO~EBELN = EKET~EBELN
**     for all entries in lt_EBAN
**     WHERE EKET~BANFN = lt_EBAN-BANFN
**       and EKET~BNFPO = lt_EBAN-BNFPO
**     .
  CLEAR: LT_OPTION[], LT_DATA_RFC[], LT_OUT_WHERE_COND[], LT_ALL_ENTRIES_COND[], LT_ALL_ENTRIES_TAB[], LT_RETURN[], LT_OUTPUT_FIELDS[],
         LT_DFIES[],  LT_ALL_ENTRIES_DFIES[].
* Join table list
  REFRESH LT_TABLES_LIST[].
  _APPEND_TABLES_LIST 'EKET'       ''  'EKET'.
  _APPEND_TABLES_LIST 'EKPO'       ''  'EKPO'.
  _APPEND_TABLES_LIST 'EKKO'       ''  'EKKO'.
* Join condition
  REFRESH LT_JOIN_CONDITION[].
  _JOIN_CONDITION 'EKET'  'EBELN'     'EKPO'   'EBELN'.
  _JOIN_CONDITION 'EKET'  'EBELP'     'EKPO'   'EBELP'.
  _JOIN_CONDITION 'EKET'  'EBELN'     'EKKO'   'EBELN'.
  REFRESH LT_SEL_FIELDS[].
  LS_SEL_FIELDS-TABLE       = 'EKPO'.
  LS_SEL_FIELDS-FIELD       = 'EBELN'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-FIELD       = 'EBELP'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-FIELD       = 'NETWR'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE       = 'EKKO'.
  LS_SEL_FIELDS-FIELD       = 'WAERS'.
  LS_SEL_FIELDS-ALIAS       = 'PO_WAERS'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS-ALIAS.
  LS_SEL_FIELDS-FIELD       = 'BEDAT'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-FIELD       = 'WKURS'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-FIELD       = 'LIFNR'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-FIELD       = 'EKORG'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE       = 'EKET'.
  LS_SEL_FIELDS-FIELD       = 'BANFN'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-FIELD       = 'BNFPO'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
* Convert LT_EBAN to string table
  _ALL_ENTRIES_CONVERT LT_EBAN  'EBAN'  1.
* 'For All Entries' Condition
* 'For All Entries' Condition
  _ALL_ENTRIES_CONDITION 'EKET~BANFN'    'BANFN' ''.
  _ALL_ENTRIES_CONDITION 'EKET~BNFPO'    'BNFPO' ''.
* 'For All Entries' Fields
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = 'EBAN'
    TABLES
      DFIES_TAB      = LT_ALL_ENTRIES_DFIES
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    CLEAR LT_ALL_ENTRIES_DFIES[].
  ENDIF.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
    TABLES
      OPTIONS              = LT_OPTION[]
      DATA                 = LT_DATA_RFC[]
      TABLES_LIST          = LT_TABLES_LIST[]
      JOIN_CONDITION       = LT_JOIN_CONDITION[]
      SEL_FIELDS           = LT_SEL_FIELDS[]
      SORT_OPTIONS         = LT_SORT_OPTIONS[]
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
      DFIES                = LT_DFIES[]
      RETURN               = LT_RETURN[]
      ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB[]
      ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND[]
      ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES[]
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  IF SY-SUBRC <> 0.
    CLEAR LT_DATA_RFC[].
  ELSE.
    READ TABLE LT_OUTPUT_FIELDS ASSIGNING <FS_OUTPUT> WITH KEY FIELDNAME = 'WAERS'.
    IF SY-SUBRC EQ 0.
      <FS_OUTPUT>-FIELDNAME = 'PO_WAERS'.
    ENDIF.
    _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_PO LT_OUTPUT_FIELDS 3.
  ENDIF.
**********************************************************************
**********************************************************************
  SORT LT_PO BY BANFN BNFPO.
  LOOP AT LT_PO INTO LS_PO.
    SY_TABIX = SY-TABIX.
    LS_PO-NETWR_LOCAL_CURR = LS_PO-NETWR.
    READ TABLE LT_EBAN INTO LS_EBAN WITH KEY BANFN = LS_PO-BANFN
                                             BNFPO = LS_PO-BNFPO
                                    BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      LS_PO-PR_WAERS = LS_EBAN-WAERS.
      IF LS_PO-PR_WAERS <> LS_PO-PO_WAERS.
        LS_PO-NETWR_LOCAL_CURR = LS_PO-NETWR * LS_PO-WKURS.
      ENDIF.
    ENDIF.
    MODIFY LT_PO FROM LS_PO INDEX SY_TABIX.
  ENDLOOP.
  "--- Aggregate LT_PC by BANFN
  REFRESH LT_PO_PR.
  LOOP AT LT_PO INTO LS_PO.
    MOVE-CORRESPONDING LS_PO TO LS_PO_PR.
    COLLECT LS_PO_PR INTO LT_PO_PR.
  ENDLOOP.
  REFRESH T_DATA.
  LOOP AT LT_PO_PR INTO LS_PO_PR.
    MOVE-CORRESPONDING LS_PO_PR TO T_DATA.
    T_DATA-TOT_PO_VAL = LS_PO_PR-NETWR_LOCAL_CURR.
    READ TABLE LT_EBAN INTO LS_EBAN WITH KEY BANFN = LS_PO_PR-BANFN
                                             BNFPO = LS_PO_PR-BNFPO
                                    BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LS_EBAN  TO T_DATA.
      MOVE-CORRESPONDING LS_PO_PR TO T_DATA.
      IF LS_EBAN-PEINH NE 0.
        DATA(LV_RESULT)     = LS_EBAN-MENGE * LS_EBAN-PREIS.
        T_DATA-TOT_PREQ_VAL = LV_RESULT / LS_EBAN-PEINH.
      ENDIF.
      T_DATA-PO_PREQ_DIFF = T_DATA-TOT_PO_VAL - T_DATA-TOT_PREQ_VAL.
    ENDIF.
    IF T_DATA-PO_PREQ_DIFF IN R_PO_PREQ_DIFF.
      APPEND T_DATA.
    ENDIF.
  ENDLOOP.
*********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    LV_FRGSX = T_DATA-FRGST.
    CALL FUNCTION '/SKN/FC_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR             = T_DATA-FRGGR
        FRGSX             = LV_FRGSX         """t_data-FRGST        "-frgsx
        FRGZU             = T_DATA-FRGZU
        SW_DEST           = LV_SW_DEST
      IMPORTING
        FRGC              = T_DATA-FRGC
      EXCEPTIONS
        WRONG_COMBINATION = 1
        OTHERS            = 2.
    IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE FRGC  NOT IN R_FRGC .
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
********************************************************************************
  "--- Set Descriptions
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    "-- BSTYP_DESC
    LV_DOMNAME = 'BSTYP'.
    LV_DOMVALUE = T_DATA-BSTYP.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-BSTYP_DESC = LV_DDTEXT.
    ENDIF.
    "-- STATU_DESC
    LV_DOMNAME = 'BANST'.     """''ESTAK'.
    LV_DOMVALUE = T_DATA-STATU.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-STATU_DESC = LV_DDTEXT.
    ENDIF.
**    ENDIF.
    "-- BSART_DESC type ESART
    LV_BSART = T_DATA-BSART.
    CALL FUNCTION '/SKN/FC_SW_10_BSART_DESC'
      EXPORTING
        BSART      = LV_BSART """"t_data-BSART
        LANGU      = LV_LANGU
        BSTYP      = T_DATA-BSTYP
        SW_DEST    = LV_SW_DEST
      IMPORTING
        TYPE_DESC  = T_DATA-BSART_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- Get  Vendor Decriptions type LIFNR
    LV_WLIEF = T_DATA-LIFNR.
    CALL FUNCTION '/SKN/FC_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR        = LV_WLIEF """t_data-LIFNR
        SW_DEST      = LV_SW_DEST
      IMPORTING
        VENDOR_DESC  = T_DATA-VENDOR_DESC
      EXCEPTIONS
        WRONG_VENDOR = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKORG_DESC type EKORG
    CALL FUNCTION '/SKN/FC_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG        = T_DATA-EKORG
        "LANGU              = lv_LANGU
        SW_DEST      = LV_SW_DEST
      IMPORTING
        PUR_ORG_DESC = T_DATA-EKORG_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKGRP_DESC type EKGRP
    CALL FUNCTION '/SKN/FC_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP        = T_DATA-EKGRP
*       LANGU        = lv_LANGU
        SW_DEST      = LV_SW_DEST
      IMPORTING
        PUR_GRP_DESC = T_DATA-EKGRP_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "---- WERKS_DESC lv_WERKS
    LV_WERKS = T_DATA-WERKS.
    CALL FUNCTION '/SKN/FC_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = LV_WERKS    """"t_data-RESWK
*       LANGU      = lv_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        PLANT_DESC = T_DATA-WERKS_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- RESWK_DESC (WERKS) type-WERKS_D
    LV_RESWK = T_DATA-RESWK.
    CALL FUNCTION '/SKN/FC_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = LV_RESWK    """"t_data-RESWK
*       LANGU      = lv_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        PLANT_DESC = T_DATA-RESWK_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
