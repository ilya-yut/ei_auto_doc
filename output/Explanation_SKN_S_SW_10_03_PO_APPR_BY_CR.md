# Exception Indicator: Purchase order was approved by creator ( SW_10_03_PO_AP_BY_CR)

## General Overview

This Exception Indicator detects purchase orders where the document creator appears as the user on the release-related change document—highlighting potential segregation-of-duties conflicts when a buyer both creates and approves the same order.

This EI serves as an essential control for procurement compliance and fraud prevention by:

- Enabling detection of self-approval patterns on release-relevant purchase orders
- Supporting review of release workflow integrity before goods receipt or payment
- Providing visibility into change-document context alongside order header attributes
- Enabling segmentation by company, vendor, and purchasing organization for targeted audit samples
- Supporting age-based prioritization when cases remain in scope after a chosen reference date

Typical use includes SOX and internal-control testing, buyer training follow-up, and periodic sampling of release activity on high-risk document types. Results are intended for exception workflows rather than operational MM reporting extracts.

The routine joins purchase order headers with release configuration and change-document headers, reads release-status changes, flags rows where the change user matches the order creator, and raises an alert when qualifying cases remain.


## Problem Description

Failure to monitor purchase orders approved by their creator creates multiple risks across segregation of duties, procurement control, and audit compliance:

**Compliance and SoD Risks**

- Creators who also release their own orders bypass intended maker-checker separation
- Self-approval can hide unauthorized commitment, pricing, or vendor selection from independent review
- Undetected patterns across buyers or organizations weaken evidence for control attestation

**Operational Risks**

- Release configuration scope that is too broad can dilute focus on creator-release conflicts
- Lookback windows misaligned with review cadence can miss recent self-approvals or retain stale cases
- Change-document matching that is not monitored leaves workflow design gaps unnoticed

**Control and Audit Risks**

- Weak monitoring reduces defensible sampling for external audit and internal investigation
- Lack of recurring review limits escalation of repeat self-approval behavior by the same users
- Missing age-based prioritization can delay response to high-risk recent cases

## Suggested Resolution

**Immediate Response**

- Review flagged orders for creator, change user, release status, and order value context
- Confirm whether each case violates policy or has documented compensating approval
- Escalate repeat offenders or high-value self-approvals to procurement management and audit

**System Assessment**

- Validate lookback window and reference-date choice against review frequency
- Tune organizational and release scope so results highlight actionable SoD conflicts
- Compare exception counts by buyer, purchasing group, and document type to find concentration

**Corrective Actions**

- Reinstate proper approval routing or reverse non-compliant releases through standard processes
- Update release strategy or user role assignments to prevent creator self-approval
- Document review outcomes, brief buyers on policy, and schedule recurring monitoring runs


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS |  |  | 0 | 0 |  |  |
| 3 | BEDAT | Document Date | DATS | 8 | 0 | EBDAT | DATUM |
| 4 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 5 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 6 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 7 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 8 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 9 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 10 | CHANGENR | Document number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 11 | DATE_REF_FLD |  |  | 0 | 0 |  |  |
| 12 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 13 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 14 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 15 | EKGRP | Purchasing Group | CHAR | 3 | 0 | BKGRP | EKGRP |
| 16 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 17 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 18 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 19 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 20 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 21 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 22 | FRGKE | Release indicator | CHAR | 1 | 0 | FRGKE | FRGKE |
| 23 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 24 | FRGSX | Release Strategy | CHAR | 2 | 0 | FRGSX | FRGSX |
| 25 | FRGZU | Release status | CHAR | 8 | 0 | FRGZU | FRGZU |
| 26 | KDATB | Validity Per. Start | DATS | 8 | 0 | KDATB | DATUM |
| 27 | KDATE | Validity Period End | DATS | 8 | 0 | KDATE | DATUM |
| 28 | LANGU |  |  | 0 | 0 |  |  |
| 29 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 30 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 31 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 32 | PROCSTAT_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 33 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 34 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 35 | RLWRT | Total val. upon release | CURR | 15 | 2 | RLWRT | WERT15 |
| 36 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 37 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 38 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 39 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 40 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 41 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 42 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 43 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 43 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Created on)

Date on Which Record Was Created (or last changed) is used to filter documents or master records by last maintenance activity.

**BACKDAYS** (BACKDAYS)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BEDAT** (Document Date)

Purchasing document date used to filter procurement documents by document creation period.

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

**CHANGENR** (Document number)

Change-document number that uniquely identifies one posted change document for an application object.

**DATE_REF_FLD** (DATE_REF_FLD)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- AEDAT — Date on Which Record Was Created (or last changed) is used to filter documents or master records by last maintenance activity.
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

Mirrors how administrators slice operational lists: release code (FRGC) is one lever that shapes which rows are comparable run over run.

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

**LANGU** (LANGU)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**PROCSTAT** (Purch. doc. proc. state)

Purchasing document processing state describing lifecycle and processing of MM purchasing objects.

**PROCSTAT_DESC** (Short Descript.)

Readable description of purchasing processing status (PROCSTAT); text expansion for reporting output.

**RESWK** (Supplying Plant)

Supplying/Issuing Plant designates the specific internal plant from which materials are being transferred or procured during a Stock Transport Order. Used in cross-plant logistics analysis.

**RESWK_DESC** (Name 1)

Plant description text used to enrich plant-level reporting.

**RLWRT** (Total val. upon release)

Cleared amount in local currency on FI clearing items showing how much open item balance was netted.

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**STATU_DESC** (Short Descript.)

Status description text used for readable status analytics.

**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**USERNAME** (User)

<mark>User who posted the change.</mark>

**UTIME** (Time)

Update/change time used with UDATE for precise event windows.

**VENDOR_DESC** (Name)

Vendor description text used for readable supplier-level reporting.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**ZTERM** (Terms of Payment)

Terms of payment key driving baseline due dates, cash discount periods, and payment rules.


### Parameter Relationships

**Reference-date window:** When **DATUM** is empty, a lower bound of today minus **BACKDAYS** is applied with a greater-than-or-equal filter and copied to **BEDAT** or **AEDAT** per **DATE_REF_FLD** (default **BEDAT**). Explicit date selections override that fallback window.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference date to the evaluation date; rows outside the configured duration range are removed.

**Creator self-approval check:** The EI compares the change-document **USERNAME** with the order **ERNAM**; only rows where both match are retained as creator-approved cases.

**Release context:** Orders must be release-relevant with a non-empty release strategy; release status is updated from change-document entries on the purchase order header before the current release code is derived.

**Header scope:** **FRGRL**, **FRGGR**, **FRGSX**, **FRGKE**, **EBELN**, **BUKRS**, **BSTYP**, **BSART**, **EKORG**, **EKGRP**, **LIFNR**, **RESWK**, **ZTERM**, **WAERS**, **PROCSTAT**, **KDATB**, and **KDATE** combine to define which purchase orders enter the analysis.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - BEDAT
- **LANGU** - initial - defaults from system logon language when not supplied

### Practical Example of Parameter Configuration

**Use Case 1: Creator self-approval in the last ninety days**

**Purpose:** Sample purchase orders where the creator may have approved their own release in the last ninety days.

```
BACKDAYS = 90
DATE_REF_FLD = BEDAT
BUKRS = 1000
FRGRL = X
```

**Use Case 2: One purchasing organization**

**Purpose:** Focus SoD review on a single purchasing organization.

```
EKORG = 1000
BACKDAYS = 60
FRGKE = X
```

**Use Case 3: Specific release group**

**Purpose:** Monitor creator self-approval under one release group.

```
FRGGR = 01
FRGSX = 01
BACKDAYS = 45
BUKRS = 1000
```

**Use Case 4: Validity period window**

**Purpose:** Limit orders to a contract validity interval while reviewing creator approvals.

```
KDATB = 20250101 - 20251231
BACKDAYS = 180
EKGRP = 001
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
| /SKN/S_SW_10_03_PO_APPR_BY_CR | AEDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BEDAT | Document Date | DATS(8) | EBDAT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSAKZ | Control indicator | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSART_DESC | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSTYP | Purch. Doc. Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | CHANGENR | Document number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKGRP | Purchasing Group | CHAR(3) | BKGRP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKGRP_DESC | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKORG_DESC | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGKE | Release indicator | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGRL | Subject to release | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGSX | Release Strategy | CHAR(2) | FRGSX |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | KDATB | Validity Per. Start | DATS(8) | KDATB |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | KDATE | Validity Period End | DATS(8) | KDATE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | LIFNR | Vendor | CHAR(10) | ELIFN |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | PROCSTAT | Purch. doc. proc. state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | PROCSTAT_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RESWK | Supplying Plant | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RESWK_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RLWRT | Total val. upon release | CURR(15) | RLWRT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | STATU | Status | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | UDATE | Date | DATS(8) | CDDATUM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | USERNAME | User | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | UTIME | Time | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | VENDOR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | ZTERM | Terms of Payment | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_10_03_PO_APPR_BY_CR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_APPR_BY_CR OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               LAST_ONLY CHAR1,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  LV_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  LV_DATE_REF_FLD = 'BEDAT'. "PO date
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 LAST_ONLY,
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
              AEDAT        ERDAT,
              BEDAT        EBDAT,
              KDATB        KDATB,
              KDATE        KDATE,
              WAERS        WAERS,
              PROCSTAT    MEPROCSTATE,
              DATUM        SY-DATUM,
              DURATION    /SKN/E_SW_DURATION.
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
              AEDAT,
              BEDAT,
              KDATB,
              KDATE,
              WAERS,
              PROCSTAT,
              DATUM,
              DURATION.
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
  DATA: LS_CDPOS TYPE CDPOS,
        LT_CDPOS LIKE TABLE OF LS_CDPOS.
  DATA: BEGIN OF LS_WRK.
          INCLUDE STRUCTURE /SKN/S_SW_10_03_PO_APPR_BY_CR.
  DATA: WRK_OBJECTID  TYPE CDOBJECTV.
  DATA: WRK_TABKEY    TYPE CDPOS-TABKEY.
  DATA: END OF LS_WRK.
  DATA: LT_WRK LIKE TABLE OF LS_WRK.
  DATA: LV_OBJECTCLAS TYPE CDOBJECTCL VALUE 'EINKBELEG'.
  DATA: WRK_OBJECTID  TYPE CDOBJECTV,
        WRK_TABKEY    TYPE CDPOS-TABKEY.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA: LV_FIELD               TYPE STRING,
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
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "Document created
    WHEN 'BEDAT'.
      R_BEDAT[] = R_DATUM[]. "PO Date
    WHEN OTHERS.
      R_BEDAT[] = R_DATUM[]. "Billing date
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
**********************************************************************
**********************************************************************
**  SELECT *
**    FROM ekko AS e
**      INNER JOIN t16fb AS t  ON e~frgke = t~frgke
**      INNER JOIN cdhdr AS ch ON ch~objectid = e~ebeln
**      INTO CORRESPONDING FIELDS OF TABLE lt_wrk
**      WHERE   t~kzfre = 'X'
**        AND   frgrl IN r_frgrl    "  EQ 'X'
**        AND frggr IN r_frggr
**        AND frgsx IN r_frgsx
**        AND ebeln IN r_ebeln
**        AND bstyp IN r_bstyp
**        AND ekorg IN r_ekorg
**        AND bukrs IN r_bukrs
**        AND lifnr IN r_lifnr
**        AND reswk IN r_reswk
**        AND bedat IN r_bedat
**        AND aedat IN r_aedat
**        AND bsart IN r_bsart
**        AND ekgrp IN r_ekgrp
**        AND ernam IN r_ernam
**        AND zterm IN r_zterm
**        AND waers IN r_waers
**        AND loekz EQ space
**        AND procstat IN r_procstat
**     "----
**      AND   ch~objectclas = lv_objectclas  " 'EINKBELEG'
***      AND   e~ernam = ch~username
**      AND frgsx > ''
**      AND e~frgke IN r_frgke.
* Join table list
  REFRESH LT_TABLES_LIST[].
  _APPEND_TABLES_LIST 'EKKO'       ''  'E'.
  _APPEND_TABLES_LIST 'T16FB'      ''  'T'.
  _APPEND_TABLES_LIST 'CDHDR'      ''  'CH'.
* Join condition
  REFRESH LT_JOIN_CONDITION[].
  _JOIN_CONDITION 'E'  'FRGKE'     'T'   'FRGKE'.
  _JOIN_CONDITION 'E'  'EBELN'     'CH'  'OBJECTID'.
*  _join_condition 'E'  'ERNAM'     'CH'  'USERNAME'.
  REFRESH LT_SEL_FIELDS[].
  _ADAPT_SEL_FIELDS 'CDHDR'   '/SKN/S_SW_10_03_PO_APPR_BY_CR'  LT_SEL_FIELDS LV_SW_DEST.
  _ADAPT_SEL_FIELDS 'T16FB'   '/SKN/S_SW_10_03_PO_APPR_BY_CR'  LT_SEL_FIELDS LV_SW_DEST.
  _ADAPT_SEL_FIELDS 'EKKO'    '/SKN/S_SW_10_03_PO_APPR_BY_CR'  LT_SEL_FIELDS LV_SW_DEST.
* Range condition
  REFRESH LT_OUT_WHERE_COND[].
  _RANGE_TO_SEL_TABLE 'E~FRGRL'    FRGRL.
  _RANGE_TO_SEL_TABLE 'E~FRGGR'    FRGGR.
  _RANGE_TO_SEL_TABLE 'E~FRGSX'    FRGSX.
  _RANGE_TO_SEL_TABLE 'E~EBELN'    EBELN.
  _RANGE_TO_SEL_TABLE 'E~BSTYP'    BSTYP.
  _RANGE_TO_SEL_TABLE 'E~EKORG'    EKORG.
  _RANGE_TO_SEL_TABLE 'E~BUKRS'    BUKRS.
  _RANGE_TO_SEL_TABLE 'E~LIFNR'    LIFNR.
  _RANGE_TO_SEL_TABLE 'E~RESWK'    RESWK.
  _RANGE_TO_SEL_TABLE 'E~BEDAT'    BEDAT.
  _RANGE_TO_SEL_TABLE 'E~AEDAT'    AEDAT.
  _RANGE_TO_SEL_TABLE 'E~KDATB'    KDATB.
  _RANGE_TO_SEL_TABLE 'E~KDATE'    KDATE.
  _RANGE_TO_SEL_TABLE 'E~BSART'    BSART.
  _RANGE_TO_SEL_TABLE 'E~EKGRP'    EKGRP.
  _RANGE_TO_SEL_TABLE 'E~ERNAM'    ERNAM.
  _RANGE_TO_SEL_TABLE 'E~ZTERM'    ZTERM.
  _RANGE_TO_SEL_TABLE 'E~WAERS'    WAERS.
  _RANGE_TO_SEL_TABLE 'E~PROCSTAT' PROCSTAT.
  _RANGE_TO_SEL_TABLE 'E~FRGKE'    FRGKE.
  LT_OPTION[] = LT_OUT_WHERE_COND.
  IF LT_OPTION[] IS INITIAL.
    LS_OPTION-TEXT = 'T~KZFRE = ''X'''.
  ELSE.
    LS_OPTION-TEXT = 'AND T~KZFRE = ''X'''.
  ENDIF.
  APPEND LS_OPTION TO LT_OPTION.
  LS_OPTION-TEXT = 'AND E~LOEKZ = '' '''.
  APPEND LS_OPTION TO LT_OPTION.
  LS_OPTION-TEXT = 'AND E~FRGSX > '' '''.
  APPEND LS_OPTION TO LT_OPTION.
  CONCATENATE '''' LV_OBJECTCLAS '''' INTO LV_FIELD.
  CONCATENATE 'AND CH~OBJECTCLAS EQ' LV_FIELD INTO LS_OPTION-TEXT SEPARATED BY SPACE.
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
    _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_WRK LT_OUTPUT_FIELDS 1.
  ENDIF.
**********************************************************************
**********************************************************************
  SORT LT_WRK.
  DELETE ADJACENT DUPLICATES FROM LT_WRK.
  LOOP AT LT_WRK INTO LS_WRK.
    SY_TABIX = SY-TABIX.
    LS_WRK-WRK_OBJECTID  = LS_WRK-EBELN.
    MODIFY LT_WRK FROM LS_WRK INDEX SY_TABIX.
  ENDLOOP.
  IF LT_WRK[] IS NOT INITIAL.
**********************************************************************
**********************************************************************
**    SELECT * FROM cdpos
**      INTO CORRESPONDING FIELDS OF TABLE lt_cdpos
**      FOR ALL ENTRIES IN lt_wrk
**      WHERE objectclas = lv_objectclas  "'EINKBELEG'
**        AND objectid = lt_wrk-wrk_objectid "  EBELN
**        AND tabname = 'EKKO'
**        AND fname = 'FRGZU'.
    CLEAR: LT_OPTION[], LT_DATA_RFC[], LT_OUT_WHERE_COND[], LT_ALL_ENTRIES_COND[], LT_ALL_ENTRIES_TAB[], LT_RETURN[], LT_OUTPUT_FIELDS[],
           LT_DFIES[],  LT_ALL_ENTRIES_DFIES[].
* Convert LT_WRK to string table
    _ALL_ENTRIES_CONVERT LT_WRK  '/SKN/S_SW_10_03_PO_APPR_BY_CR1'  1.
* Table List
    REFRESH LT_TABLES_LIST[].
    _APPEND_TABLES_LIST 'CDPOS' '' ''.
* Table Fields
    REFRESH LT_SEL_FIELDS[].
    _ADAPT_SEL_FIELDS   'CDPOS' 'CDPOS'  LT_SEL_FIELDS LV_SW_DEST.
* 'For All Entries' Condition
    _ALL_ENTRIES_CONDITION 'OBJECTID' 'WRK_OBJECTID' ''.
*   'For All Entries' Fields
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        TABNAME        = '/SKN/S_SW_10_03_PO_APPR_BY_CR1'
      TABLES
        DFIES_TAB      = LT_ALL_ENTRIES_DFIES
      EXCEPTIONS
        NOT_FOUND      = 1
        INTERNAL_ERROR = 2
        OTHERS         = 3.
    IF SY-SUBRC NE 0.
      CLEAR LT_ALL_ENTRIES_DFIES[].
    ENDIF.
    CONCATENATE '''' LV_OBJECTCLAS '''' INTO LV_FIELD.
    CONCATENATE 'OBJECTCLAS EQ' LV_FIELD INTO LS_OPTION-TEXT SEPARATED BY SPACE.
    APPEND LS_OPTION TO LT_OPTION.
    LS_OPTION-TEXT = 'AND TABNAME EQ ''EKKO'''.
    APPEND LS_OPTION TO LT_OPTION.
    LS_OPTION-TEXT = 'AND FNAME EQ ''FRGZU'''.
    APPEND LS_OPTION TO LT_OPTION.
    CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
      DESTINATION LV_SW_DEST
      TABLES
        OPTIONS              = LT_OPTION[]
        DATA                 = LT_DATA_RFC[]
        TABLES_LIST          = LT_TABLES_LIST[]
        SEL_FIELDS           = LT_SEL_FIELDS[]
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
    IF SY-SUBRC NE 0.
      CLEAR LT_DATA_RFC[].
    ELSE.
      _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_CDPOS LT_OUTPUT_FIELDS 3.
    ENDIF.
*** YC++ 18.05.21
    IF LT_CDPOS IS NOT INITIAL.
      SORT LT_CDPOS BY VALUE_NEW.
      DELETE LT_CDPOS WHERE VALUE_NEW IS INITIAL.
    ENDIF.
*** YC++ 18.05.21
*    SORT lt_cdpos BY objectclas objectid.
    SORT LT_CDPOS BY OBJECTCLAS OBJECTID CHANGENR  TABKEY.
*    IF lv_last_only IS NOT INITIAL. "Only last release is relevant - the rest is deleted.
*      CLEAR: wrk_objectid, wrk_tabkey.
*      LOOP AT lt_cdpos INTO ls_cdpos.
*        sy_tabix = sy-tabix.
*        IF wrk_objectid EQ ls_cdpos-objectid AND
*           wrk_tabkey   EQ ls_cdpos-tabkey.
*          DELETE lt_cdpos INDEX sy_tabix.
*          CONTINUE.
*        ELSE.
*          wrk_objectid = ls_cdpos-objectid.
*          wrk_tabkey = ls_cdpos-tabkey.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
**********************************************************************
**********************************************************************
  ENDIF.
  REFRESH T_DATA.
  DATA: LT_WRK_TABIX TYPE INT4.
  LOOP AT LT_WRK INTO LS_WRK.
    LT_WRK_TABIX = SY-TABIX.
    READ TABLE LT_CDPOS INTO LS_CDPOS   " TRANSPORTING NO FIELDS
                        WITH KEY OBJECTCLAS = LV_OBJECTCLAS
                                 OBJECTID   = LS_WRK-WRK_OBJECTID
                                 CHANGENR   = LS_WRK-CHANGENR
                         BINARY SEARCH.
    IF NOT SY-SUBRC IS INITIAL.
      DELETE LT_WRK INDEX LT_WRK_TABIX.
      CONTINUE.
    ELSE.
      LS_WRK-FRGZU = LS_CDPOS-VALUE_NEW.
      MODIFY LT_WRK FROM LS_WRK INDEX LT_WRK_TABIX.
    ENDIF.
  ENDLOOP.
  SORT LT_WRK BY WRK_OBJECTID UDATE DESCENDING UTIME DESCENDING.
  IF LV_LAST_ONLY IS NOT INITIAL. "Only last release is relevant - the rest is deleted.
    CLEAR: WRK_OBJECTID, WRK_TABKEY.
    LOOP AT LT_WRK INTO LS_WRK.
      IF WRK_OBJECTID EQ LS_WRK-WRK_OBJECTID
*          AND wrk_tabkey   EQ ls_cdpos-tabkey
        .
        CONTINUE.
      ELSE.
        WRK_OBJECTID = LS_WRK-WRK_OBJECTID.
        IF LS_WRK-USERNAME EQ LS_WRK-ERNAM.
          MOVE-CORRESPONDING LS_WRK TO T_DATA.
          APPEND T_DATA.
        ENDIF.
*          wrk_tabkey = ls_cdpos-tabkey.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT LT_WRK INTO LS_WRK.
      IF LS_WRK-USERNAME EQ LS_WRK-ERNAM.
        MOVE-CORRESPONDING LS_WRK TO T_DATA.
        APPEND T_DATA.
      ENDIF.
    ENDLOOP.
  ENDIF.
*********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/FC_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR             = T_DATA-FRGGR
        FRGSX             = T_DATA-FRGSX
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
  DELETE T_DATA WHERE FRGC NOT IN R_FRGCO.
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
    LV_DOMNAME = 'ESTAK'.
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
    "-- PROCSTAT_DESC
    LV_DOMNAME = 'MEPROCSTATE'.
    LV_DOMVALUE = T_DATA-PROCSTAT.
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
      T_DATA-PROCSTAT_DESC = LV_DDTEXT.
    ENDIF.
    "-- BSART_DESC
    CALL FUNCTION '/SKN/FC_SW_10_BSART_DESC'
      EXPORTING
        BSART      = T_DATA-BSART
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
    "--- Get  Vendor Decriptions
    CALL FUNCTION '/SKN/FC_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR        = T_DATA-LIFNR
        SW_DEST      = LV_SW_DEST
      IMPORTING
        VENDOR_DESC  = T_DATA-VENDOR_DESC
      EXCEPTIONS
        WRONG_VENDOR = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKORG_DESC
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
    "-- EKGRP_DESC
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
    "--- RESWK_DESC (WERKS)
    CALL FUNCTION '/SKN/FC_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = T_DATA-RESWK
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
