# Exception Indicator: PUR. One-time vendor check  (detail) ( SW_10_03_PO_1_TIME_D)

## General Overview

This Exception Indicator identifies purchase orders placed with one-time vendors, returning line-level detail on order value, release status, and organizational context for follow-up and control review.

This EI serves as an essential control for procurement and accounts payable by:

- Enabling detection of spending against one-time vendor accounts that bypass regular master-data discipline
- Supporting review of order gross and net values in document and local currency before payment processing
- Providing visibility into release strategy, processing state, and purchasing ownership on flagged orders
- Enabling segmentation by company, purchasing organization, and supply plant for targeted audit samples
- Supporting age-based prioritization when orders remain open for review after a chosen reference date

Typical use includes AP fraud prevention, vendor master governance, and periodic sampling of one-time vendor spend before invoice payment. Results are intended for exception workflows rather than operational purchasing list reporting.

The routine reads purchase order header and item data for vendors marked as one-time accounts, enriches rows with descriptions and release codes, applies optional value and age filters, and raises an alert when qualifying orders remain.


## Problem Description

Failure to monitor purchase orders with one-time vendors creates multiple risks across procurement control, accounts payable, and fraud prevention:

**Procurement and Payment Risks**

- One-time vendor orders can route spend to ad hoc payees without the same master-data and bank-detail controls as regular suppliers
- High-value orders on temporary vendor records may evade standard approval or duplicate-vendor checks
- Undetected repeat use of one-time vendors can indicate policy circumvention or split-order behavior

**Operational Risks**

- Release and processing state that is not reviewed alongside vendor type can delay escalation of blocked or incomplete orders
- Value thresholds that are misaligned can hide material one-time vendor spend in large populations
- Lookback windows that are too narrow may miss orders that still require payment review

**Control and Audit Risks**

- Weak monitoring reduces evidence that one-time vendor spend was reviewed before payment
- Lack of recurring exception review weakens segregation-of-duties over vendor creation and order placement
- Missing age-based sampling limits prioritization of long-standing one-time vendor commitments

## Suggested Resolution

**Immediate Response**

- Review flagged orders for vendor identity, order value, release status, and requester or buyer context
- Confirm each one-time vendor order has valid business justification and supporting documentation
- Prioritize high-value or repeat one-time vendor patterns for AP and procurement follow-up

**System Assessment**

- Validate monitoring window and reference-date choice against payment and review cadence
- Tune company, organization, and value scope so results stay actionable
- Compare exception counts by buyer, plant, and document type to find systematic gaps

**Corrective Actions**

- Convert recurring one-time vendors to regular vendor master records where policy allows
- Block or reverse unauthorized orders through standard MM processes
- Document review outcomes, brief buyers on policy, and schedule recurring runs for high-risk scopes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT_EKKO | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | AEDAT_EKPO | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 3 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 4 | BEDAT | Document Date | DATS | 8 | 0 | EBDAT | DATUM |
| 5 | BRTWR | Gross order value | CURR | 13 | 2 | BBWERT | WERT7 |
| 6 | BRTWR_LOCAL | LC Gross order value | CURR | 13 | 2 | BBWERT | WERT7 |
| 7 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 8 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 9 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 10 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 11 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 12 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 13 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 14 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 15 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 16 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 17 | EINDT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 18 | EKGRP | Purchasing Group | CHAR | 3 | 0 | BKGRP | EKGRP |
| 19 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 20 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 21 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 22 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 23 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 24 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 25 | FRGKE | Release indicator | CHAR | 1 | 0 | FRGKE | FRGKE |
| 26 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 27 | FRGSX | Release Strategy | CHAR | 2 | 0 | FRGSX | FRGSX |
| 28 | FRGZU | Release status | CHAR | 8 | 0 | FRGZU | FRGZU |
| 29 | KDATB | Validity Per. Start | DATS | 8 | 0 | KDATB | DATUM |
| 30 | KDATE | Validity Period End | DATS | 8 | 0 | KDATE | DATUM |
| 31 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 32 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 33 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 34 | NAME1 | Name | CHAR | 40 | 0 | AD_NAME1 | TEXT40 |
| 35 | NETWR | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 36 | NETWR_LOCAL | LC Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 37 | PRDAT | Price Date | DATS | 8 | 0 | PREDT | DATUM |
| 38 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 39 | PROCSTAT_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 40 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 41 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 42 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 43 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 44 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 45 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 46 | WAERS_LOCAL | Currency (LC) | CUKY | 5 | 0 | WAERS | WAERS |
| 47 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 48 | WKURS | Exchange Rate | DEC | 9 | 5 | WKURS | KURSP |
| 49 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 49 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT_EKKO** (Created on)

Guards against oversized extracts when created on on AEDAT_EKKO is narrowed together with client, user, or session filters.

**AEDAT_EKPO** (Created on)

Explains why two monitoring passes differ: only the pass with stricter created on on AEDAT_EKPO surfaces the disputed rows.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BEDAT** (Document Date)

Purchasing document date used to filter procurement documents by document creation period.

**BRTWR** (Gross order value)

Mirrors how administrators slice operational lists: gross order value (BRTWR) is one lever that shapes which rows are comparable run over run.

**BRTWR_LOCAL** (LC Gross order value)

Ensures reporting respects lc gross order value constraints carried by BRTWR_LOCAL.

**BSAKZ** (Control indicator)

Purchasing control/indicator flag used to segment procurement records by processing characteristics.

**BSART** (Purchasing Doc. Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BSART_DESC** (Doc. Type Descript.)

Text description of purchasing document type used for readable reporting output.

**BSTYP** (Purch. Doc. Category)

Purchasing document category (PR/PO/contract etc.) used to segment MM document classes.

**BSTYP_DESC** (Short Descript.)

Description of purchasing document category for business-readable output.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- AEDAT_EKKO — Created on.
- AEDAT_EKPO — Created on.
- BEDAT — Purchasing document date used to filter procurement documents by document creation period.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**EBELN** (Purchasing Document)

Purchasing document number (typically PO) used as the primary MM document key.

**EINDT** (Delivery Date)

Item delivery date on purchasing or delivery structures expressing vendor-promised or requested receipt date.

**EKGRP** (Purchasing Group)

Purchasing group (buyer) used for procurement ownership and control segmentation.

**EKGRP_DESC** (Description p. group)

Description of purchasing group for readable buyer/team reporting.

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**EKORG_DESC** (Description)

Description of purchasing organization for business-readable reporting.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**FRGC** (Release code)

Captures edge cases where release code (FRGC) must be non-default to reproduce a customer-specific monitoring scenario.

**FRGGR** (Release group)

Release group key controlling the purchasing release strategy framework.

**FRGKE** (Release indicator)

Release status indicator used to distinguish released vs unreleased documents.

**FRGRL** (Subject to release)

Release indicator/flag used in PO/PR release strategy control.

**FRGSX** (Release Strategy)

Extended release information or strategy outcome code complementing FRGST on MM release objects.

**FRGZU** (Release status)

Release strategy progression/status code used for approval lifecycle tracking.

**KDATB** (Validity Per. Start)

Condition record valid-from date opening the pricing or condition interval.

**KDATE** (Validity Period End)

Condition or agreement validity end date (valid-to) closing pricing master or contract validity.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**NAME1** (Name)

Supports escalation where name on NAME1 signals ownership for follow-up between Basis and functional teams.

**NETWR** (Net Order Value)

Net value amount used for commercial threshold and anomaly checks.

**NETWR_LOCAL** (LC Net Order Value)

For distributed landscapes, lc net order value on NETWR_LOCAL often anchors which application server or destination appears in results.

**PRDAT** (Price Date)

Valuable when comparing health before and after a release—hold price date on PRDAT constant while varying other filters.

**PROCSTAT** (Purch. doc. proc. state)

Purchasing document processing state describing lifecycle and processing of MM purchasing objects.

**PROCSTAT_DESC** (Short Descript.)

Readable description of purchasing processing status (PROCSTAT); text expansion for reporting output.

**RESWK** (Supplying Plant)

Supplying/Issuing Plant designates the specific internal plant from which materials are being transferred or procured during a Stock Transport Order. Used in cross-plant logistics analysis.

**RESWK_DESC** (Name 1)

Plant description text used to enrich plant-level reporting.

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**STATU_DESC** (Short Descript.)

Status description text used for readable status analytics.

**VENDOR_DESC** (Name)

Vendor description text used for readable supplier-level reporting.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WAERS_LOCAL** (Currency (LC))

Supports operational control by evaluating currency (lc) through WAERS_LOCAL for each candidate record.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WKURS** (Exchange Rate)

When combined with destination discipline, exchange rate on WKURS keeps both breadth and depth of the extract intentional.

**ZTERM** (Terms of Payment)

Terms of payment key driving baseline due dates, cash discount periods, and payment rules.


### Parameter Relationships

**Reference-date window:** When **DATUM** is empty, a lower bound of today minus **BACKDAYS** is applied with a greater-than-or-equal filter and copied to **BEDAT**, **AEDAT_EKKO**, or **AEDAT_EKPO** per **DATE_REF_FLD** (default **BEDAT**). Explicit date selections override that fallback window.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference date to the evaluation date; rows outside the configured duration range are removed.

**Release context:** **FRGGR**, **FRGSX**, and **FRGZU** on each order drive derivation of **FRGC** (release code) before results are returned.

**Value filters:** **BRTWR**, **NETWR**, **BRTWR_LOCAL**, and **NETWR_LOCAL** narrow results after local-currency amounts are derived from company currency settings.

**Organizational scope:** **BUKRS**, **EKORG**, **EKGRP**, **LIFNR**, **RESWK**, and release-related parameters (**FRGRL**, **FRGKE**, **PROCSTAT**, **BSART**, **BSTYP**) combine to define which one-time vendor orders enter the result set.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - BEDAT
- **LANGU** - initial - defaults from system logon language when not supplied

### Practical Example of Parameter Configuration

**Use Case 1: One-time vendor orders in the last ninety days**

**Purpose:** Sample one-time vendor purchase orders with document dates in the last ninety days for AP review.

```
BACKDAYS = 90
DATE_REF_FLD = BEDAT
BUKRS = 1000
```

**Use Case 2: High gross value threshold**

**Purpose:** Flag one-time vendor orders above a material gross value in document currency.

```
BRTWR = 10000 - 999999999
BACKDAYS = 60
EKORG = 1000
```

**Use Case 3: Specific vendor and purchasing group**

**Purpose:** Investigate repeat one-time vendor activity for one supplier and buyer group.

```
LIFNR = 100000
EKGRP = 001
BACKDAYS = 180
```

**Use Case 4: Created-on reference at header level**

**Purpose:** Use header changed-on date instead of document date for the lookback window.

```
DATE_REF_FLD = AEDAT_EKKO
BACKDAYS = 45
BUKRS = 2000
```

**Use Case 5: Exactly seven full days since reference date**

**Purpose:** Return rows where the reference date is exactly 7 full days ago.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
DATE_REF_FLD = BEDAT
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_03_PO_1_TIME_DET | AEDAT_EKKO | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | AEDAT_EKPO | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BEDAT | Document Date | DATS(8) | EBDAT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BRTWR | Gross order value | CURR(13) | BBWERT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BRTWR_LOCAL | Gross order value | CURR(13) | BBWERT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BSAKZ | Control indicator | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BSART_DESC | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BSTYP | Purch. Doc. Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_1_TIME_DET | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_1_TIME_DET | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_1_TIME_DET | EINDT | Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | EKGRP | Purchasing Group | CHAR(3) | BKGRP |
| /SKN/S_SW_10_03_PO_1_TIME_DET | EKGRP_DESC | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_1_TIME_DET | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_1_TIME_DET | EKORG_DESC | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_1_TIME_DET | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_1_TIME_DET | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PO_1_TIME_DET | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PO_1_TIME_DET | FRGKE | Release indicator | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_PO_1_TIME_DET | FRGRL | Subject to release | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PO_1_TIME_DET | FRGSX | Release Strategy | CHAR(2) | FRGSX |
| /SKN/S_SW_10_03_PO_1_TIME_DET | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PO_1_TIME_DET | KDATB | Validity Per. Start | DATS(8) | KDATB |
| /SKN/S_SW_10_03_PO_1_TIME_DET | KDATE | Validity Period End | DATS(8) | KDATE |
| /SKN/S_SW_10_03_PO_1_TIME_DET | LIFNR | Vendor | CHAR(10) | ELIFN |
| /SKN/S_SW_10_03_PO_1_TIME_DET | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_1_TIME_DET | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_03_PO_1_TIME_DET | NAME1 | Name | CHAR(40) | AD_NAME1 |
| /SKN/S_SW_10_03_PO_1_TIME_DET | NETWR | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | NETWR_LOCAL | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | PRDAT | Price Date | DATS(8) | PREDT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | PROCSTAT | Purch. doc. proc. state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_PO_1_TIME_DET | PROCSTAT_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | RESWK | Supplying Plant | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PO_1_TIME_DET | RESWK_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PO_1_TIME_DET | STATU | Status | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_1_TIME_DET | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_1_TIME_DET | VENDOR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_1_TIME_DET | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_1_TIME_DET | WAERS_LOCAL | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_1_TIME_DET | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_PO_1_TIME_DET | WKURS | Exchange Rate | DEC(9) | WKURS |
| /SKN/S_SW_10_03_PO_1_TIME_DET | ZTERM | Terms of Payment | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PO_1_TIME_DET .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_1_TIME_DET OPTIONAL
*"----------------------------------------------------------------------
*** Begin Yuri C.++ 29.01.19
  TYPES: BEGIN OF TY_T001,
           BUKRS TYPE T001-BUKRS,
           WAERS TYPE T001-WAERS,
         END OF TY_T001,
         TT_T001 TYPE STANDARD TABLE OF TY_T001.
*** End Yuri C.++ 29.01.19
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  LV_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  LV_DATE_REF_FLD = 'BEDAT'. "PO date
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT.
  DATA_MULTY: FRGRL        FRGRL,
              EBELN        EBELN,
              BUKRS        BUKRS,
              BSTYP        EBSTYP,
              BSART        ESART,
              EKORG        EKORG,
              EKGRP        BKGRP,
              FRGGR        FRGGR,
              FRGSX        FRGSX,
              FRGCO        FRGCO,
              FRGKE        FRGKE,
              LIFNR        ELIFN,
              RESWK        RESWK,
              ZTERM        DZTERM,
              ERNAM        ERNAM,
*            AEDAT        ERDAT,        " Yuri C.-- 20.12.17
              AEDAT_EKKO   ERDAT,       " Yuri C.++ 20.12.17
              AEDAT_EKPO   ERDAT,       " Yuri C.++ 20.12.17
              BEDAT        EBDAT,
              WAERS        WAERS,
              BRTWR        BBWERT,
              NETWR        BWERT,
              WAERS_LOCAL  WAERS,
              BRTWR_LOCAL  BBWERT,
              NETWR_LOCAL  BWERT,
              PROCSTAT    MEPROCSTATE,
              DATUM        SY-DATUM,
              DURATION    /SKN/E_SW_DURATION,
              OBJECTCLAS  CDOBJECTCL,         " Yuri C.++ 28.12.17
              OBJECTID    CDOBJECTV.          " Yuri C.++ 28.12.17
  SELECT_MULTY:
              FRGRL,
              EBELN,
              BUKRS,
              BSTYP,
              BSART,
              EKORG,
              EKGRP,
              FRGGR,
              FRGSX,
              FRGCO,
              FRGKE,
              LIFNR,
              RESWK,
              ZTERM,
              ERNAM,
*            AEDAT,        " Yuri C.-- 20.12.17
              AEDAT_EKKO,  " Yuri C.++ 20.12.17
              AEDAT_EKPO,  " Yuri C.++ 20.12.17
              BEDAT,
              BRTWR,
              NETWR,
              WAERS,
              WAERS_LOCAL,
              BRTWR_LOCAL,
              NETWR_LOCAL,
              PROCSTAT,
              DATUM,
              DURATION,
              OBJECTID,   " Yuri C.++ 28.12.17
              OBJECTCLAS. " Yuri C.++ 28.12.17
  CONVERT_MULTY: EBELN ALPHA,
                 LIFNR ALPHA.
  RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
           R_FLD_VAL FOR DD03P-FIELDNAME .
  DATA :   FLD_NAME TYPE FIELDNAME.
  DATA : I TYPE I,
         CI(1) TYPE C,
         NFIELDS TYPE I VALUE 3.   "
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM .
  DATA : LANGU LIKE SY-LANGU .
  DATA : IS_OUT(1) TYPE C.
  DATA : TIME_DIFF TYPE  INT4 .
  DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
        LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
        LV_DDTEXT LIKE  DD07V-DDTEXT.
  DATA: LV_FRGCO  TYPE FRGCO.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
*** Begin change of Yuri C.++ 28.12.17
  DATA: LT_DATA TYPE TABLE OF /SKN/S_SW_10_03_PO_1_TIME_DET.
  DATA: LS_T001 TYPE TY_T001.
  DATA: LT_T001 TYPE TT_T001.
  FIELD-SYMBOLS: <FS_DATA> TYPE /SKN/S_SW_10_03_PO_1_TIME_DET.
*** End change of Yuri C.++ 28.12.17
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA : LS_VBPA TYPE VBPA,
         LT_VBPA LIKE TABLE OF LS_VBPA.
  DATA : LV_DATA_POSNR TYPE POSNR.
**** Begin change of Yuri C.++ 28.12.17
* Configuration Alert
*  CALL FUNCTION '/SKN/F_SW_10_06_MD_CHNG_LOG'
*    IMPORTING
*      is_alert = is_alert
*    TABLES
*      t_select = t_select
*      t_data   = lt_data.
*** End change of Yuri C.++ 28.12.17
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PO_1_TIME_DET'
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
*** Begin change of Yuri C.-- 20.12.17
*     when 'AEDAT'.
*       R_AEDAT[] = R_DATUM[]. "Document created
*** End change of Yuri C.-- 20.12.17
*** Begin change of Yuri C.++ 20.12.17
    WHEN 'AEDAT_EKKO'.
      R_AEDAT_EKPO[] = R_DATUM[]. "Document created
    WHEN 'AEDAT_EKPO'.
      R_AEDAT_EKPO[] = R_DATUM[]. "Document created
*** End change of Yuri C.++ 20.12.17
    WHEN 'BEDAT'.
      R_BEDAT[] = R_DATUM[]. "PO Date
    WHEN OTHERS.
      R_BEDAT[] = R_DATUM[]. "Billing date
  ENDCASE.
  "--- Set R_PROCSTAT
***  if R_PROCSTAT[] is initial.
***    RS_PROCSTAT-sign = 'I'.
***     RS_PROCSTAT-option = 'EQ'.
***      RS_PROCSTAT-low = space.
***       APPEND RS_PROCSTAT to R_PROCSTAT.
***      RS_PROCSTAT-low = '02'.
***       APPEND RS_PROCSTAT to R_PROCSTAT.
***      RS_PROCSTAT-low = '03'.
***       APPEND RS_PROCSTAT to R_PROCSTAT.
***      RS_PROCSTAT-low = '05'.
***       APPEND RS_PROCSTAT to R_PROCSTAT.
***  endif.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM EKKO AS K INNER JOIN EKPO      AS P ON K~EBELN EQ P~EBELN
                   INNER JOIN LFA1      AS L ON K~LIFNR EQ L~LIFNR
                   LEFT OUTER JOIN ADRC AS A ON K~ADRNR EQ A~ADDRNUMBER
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE K~FRGRL    IN R_FRGRL
      AND K~FRGGR    IN R_FRGGR
      AND K~FRGSX    IN R_FRGSX
      AND K~EBELN    IN R_EBELN
      AND K~BSTYP    IN R_BSTYP
      AND K~EKORG    IN R_EKORG
      AND K~BUKRS    IN R_BUKRS
      AND K~LIFNR    IN R_LIFNR
      AND K~RESWK    IN R_RESWK
      AND K~BEDAT    IN R_BEDAT
      AND K~AEDAT    IN R_AEDAT_EKKO
      AND K~BSART    IN R_BSART
      AND K~EKGRP    IN R_EKGRP
      AND K~ERNAM    IN R_ERNAM
      AND K~ZTERM    IN R_ZTERM
      AND K~WAERS    IN R_WAERS
      AND K~LOEKZ    EQ SPACE
      AND K~MEMORY   EQ ' '               " (hold)
      AND K~PROCSTAT IN R_PROCSTAT
      AND K~FRGKE    IN R_FRGKE
      AND P~AEDAT    IN R_AEDAT_EKPO
      AND P~BRTWR    GT 0
      AND P~LOEKZ    EQ SPACE
      AND L~XCPDK    EQ 'X'.   " 1-time Vendors
*** Begin Yuri C.++ 29.01.19
  IF T_DATA[] IS NOT INITIAL.
    LT_DATA = T_DATA[].
    SORT LT_DATA BY BUKRS.
    SELECT BUKRS WAERS
      FROM T001
      INTO TABLE LT_T001
      FOR ALL ENTRIES IN LT_DATA
      WHERE BUKRS EQ LT_DATA-BUKRS
      AND   WAERS IN R_WAERS_LOCAL.
    IF SY-SUBRC = 0.
      SORT LT_T001 BY BUKRS.
    ENDIF.
  ENDIF.
*** End Yuri C.++ 29.01.19
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    SY_TABIX = SY-TABIX .
*** Begin Yuri C.-- 29.01.19
*    t_data-brtwr_local = t_data-brtwr * t_data-wkurs.
*    t_data-netwr_local = t_data-netwr * t_data-wkurs .
*    t_data-waers_local = t_data-waers .
*** End Yuri C.-- 29.01.19
***  Begin Yuri C.++ 29.01.19
    CLEAR: LS_T001.
    <FS_DATA>-BRTWR_LOCAL = <FS_DATA>-BRTWR * <FS_DATA>-WKURS.
    <FS_DATA>-NETWR_LOCAL = <FS_DATA>-NETWR * <FS_DATA>-WKURS .
    IF <FS_DATA>-BUKRS IS NOT INITIAL.
      READ TABLE LT_T001 INTO LS_T001 WITH KEY BUKRS = <FS_DATA>-BUKRS
                                      BINARY SEARCH.
      IF SY-SUBRC = 0.
        <FS_DATA>-WAERS_LOCAL = LS_T001-WAERS.
      ENDIF.
    ENDIF.
*** End Yuri C.++ 29.01.19
*    MODIFY t_data INDEX sy_tabix.     " Yuri C.-- 29.01.19
  ENDLOOP.
  DELETE T_DATA WHERE BRTWR NOT IN R_BRTWR.
  DELETE T_DATA WHERE NETWR NOT IN R_NETWR.
  DELETE T_DATA WHERE BRTWR_LOCAL NOT IN R_BRTWR_LOCAL.
  DELETE T_DATA WHERE NETWR_LOCAL NOT IN R_NETWR_LOCAL.
*********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR             = T_DATA-FRGGR
        FRGSX             = T_DATA-FRGSX
        FRGZU             = T_DATA-FRGZU
      IMPORTING
        FRGC              = T_DATA-FRGC
      EXCEPTIONS
        WRONG_COMBINATION = 1
        OTHERS            = 2.
    IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
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
    LV_DOMNAME = 'EBSTYP'.
    LV_DOMVALUE = T_DATA-BSTYP.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-BSTYP_DESC = LV_DDTEXT.
    ENDIF.
    "-- STATU_DESC
    LV_DOMNAME = 'ESTAK'.
    LV_DOMVALUE = T_DATA-STATU.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-STATU_DESC = LV_DDTEXT.
    ENDIF.
    "-- PROCSTAT_DESC
    LV_DOMNAME = 'MEPROCSTATE'.
    LV_DOMVALUE = T_DATA-PROCSTAT.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-PROCSTAT_DESC = LV_DDTEXT.
    ENDIF.
    "-- BSART_DESC
    CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART      = T_DATA-BSART
        LANGU      = LV_LANGU
        BSTYP      = T_DATA-BSTYP
      IMPORTING
        TYPE_DESC  = T_DATA-BSART_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- Get  Vendor Decriptions
    CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR        = T_DATA-LIFNR
      IMPORTING
        VENDOR_DESC  = T_DATA-VENDOR_DESC
      EXCEPTIONS
        WRONG_VENDOR = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKORG_DESC
    CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG        = T_DATA-EKORG
        "LANGU              = lv_LANGU
      IMPORTING
        PUR_ORG_DESC = T_DATA-EKORG_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKGRP_DESC
    CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP        = T_DATA-EKGRP
*       LANGU        = lv_LANGU
      IMPORTING
        PUR_GRP_DESC = T_DATA-EKGRP_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- RESWK_DESC (WERKS)
    CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = T_DATA-RESWK
*       LANGU      = lv_LANGU
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
