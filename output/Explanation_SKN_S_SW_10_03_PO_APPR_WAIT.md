# Exception Indicator: PUR PO Waiting for Approval ( SW_10_03_PO_APPR_WAT)

## General Overview

This Exception Indicator identifies purchase orders that are subject to release, have an active release strategy, and match configured release-indicator and processing-state criteria—surfacing orders that are still waiting for approval action.

This EI serves as an essential control for procurement release governance by:

- Enabling detection of purchase orders blocked in the release workflow before goods receipt or invoice processing
- Supporting follow-up on orders with assigned release strategies that have not completed approval
- Providing visibility into release group, strategy, derived release code, and processing state on each flagged header
- Enabling age-based prioritization when orders remain in the approval queue after a chosen reference date
- Supporting audit sampling of pending release backlog by company, vendor, and purchasing organization

Typical use includes buyer and approver escalation, release workflow health checks, and periodic control samples before close. Results are intended for exception workflows rather than operational MM list reporting.

The routine reads purchase order header data for release-relevant documents with a non-empty release strategy, derives the current release code, applies optional age filtering, and raises an alert when qualifying orders remain.


## Problem Description

Failure to monitor purchase orders waiting for release approval creates multiple risks across procurement control, operational throughput, and compliance:

**Procurement and Approval Risks**

- Orders stuck in the release workflow can delay sourcing, receipt, and payment when approvers are not alerted
- Release strategies assigned but not completed may leave commitment on the books without authorized approval
- Undetected backlog across vendors or purchasing groups can concentrate risk on critical suppliers

**Operational Risks**

- Release-indicator scope that is too loose or too tight can hide actionable pending orders or create reviewer fatigue
- Lookback and age settings misaligned with approval cadence can exclude recent queue items or retain stale cases
- Processing-state filters that are not tuned can mix closed orders into the pending queue

**Control and Audit Risks**

- Weak monitoring reduces evidence that release backlog was reviewed before period close
- Lack of recurring exception review weakens segregation-of-duties over who may approve high-value orders
- Missing age-based prioritization limits escalation of long-pending approval cases

## Suggested Resolution

**Immediate Response**

- Review flagged purchase orders for vendor, release group, strategy, release code, and processing state
- Contact the responsible buyer or approver to confirm whether release action is pending or overdue
- Prioritize high-value or long-waiting orders for approval or rejection through standard workflows

**System Assessment**

- Validate lookback window and reference-date choice against approval review cadence
- Tune release-indicator, processing-state, and organizational scope so results stay actionable
- Compare exception counts by purchasing group, release group, and document type to find systematic gaps

**Corrective Actions**

- Complete pending releases or reject orders through standard MM approval processes
- Correct release strategy or master-data issues identified during review
- Document outcomes, brief stakeholders on recurring patterns, and schedule recurring runs before close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS | Days Bacward |  | 0 | 0 |  |  |
| 3 | BEDAT | Document Date | DATS | 8 | 0 | EBDAT | DATUM |
| 4 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 5 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 6 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 7 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 8 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 9 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 10 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 11 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 12 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 13 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 14 | EKGRP | Purchasing Group | CHAR | 3 | 0 | BKGRP | EKGRP |
| 15 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 16 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 17 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 18 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 19 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 20 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 21 | FRGKE | Release indicator | CHAR | 1 | 0 | FRGKE | FRGKE |
| 22 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 23 | FRGSX | Release Strategy | CHAR | 2 | 0 | FRGSX | FRGSX |
| 24 | FRGZU | Release status | CHAR | 8 | 0 | FRGZU | FRGZU |
| 25 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 26 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 27 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 28 | PROCSTAT_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 29 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 30 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 31 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 32 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 33 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 34 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 35 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 35 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Created on)

Date on Which Record Was Created (or last changed) is used to filter documents or master records by last maintenance activity.

**BACKDAYS** (Days Bacward)

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

**DATE_REF_FLD** (Date Ref Field)

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

Helps monitoring stay readable by requiring release code (FRGC) to match organizational or technical selectors when set.

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

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**STATU_DESC** (Short Descript.)

Status description text used for readable status analytics.

**VENDOR_DESC** (Name)

Vendor description text used for readable supplier-level reporting.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**ZTERM** (Terms of Payment)

Terms of payment key driving baseline due dates, cash discount periods, and payment rules.


### Parameter Relationships

**Reference-date window:** When **DATUM** is empty, a lower bound of today minus **BACKDAYS** is applied with a greater-than-or-equal filter and copied to **BEDAT** or **AEDAT** per **DATE_REF_FLD** (default **BEDAT**). Explicit date selections override that fallback window.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference date to the evaluation date; rows outside the configured duration range are removed.

**Release waiting scope:** The header selection requires a non-empty **FRGSX** (release strategy) and matching **FRGKE** (release indicator) values; **FRGRL** and other header filters further narrow release-relevant purchase orders.

**Release code derivation:** **FRGGR**, **FRGSX**, and **FRGZU** on each order drive derivation of **FRGC** (release code) before results are returned.

**Header scope:** **EBELN**, **BUKRS**, **BSTYP**, **BSART**, **EKORG**, **EKGRP**, **LIFNR**, **RESWK**, **ERNAM**, **ZTERM**, **WAERS**, and **PROCSTAT** combine to define which waiting-for-approval purchase order headers enter the result set.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - BEDAT
- **LANGU** - initial - defaults from system logon language when not supplied

### Practical Example of Parameter Configuration

**Use Case 1: Release backlog in the last sixty days**

**Purpose:** Review purchase orders waiting for approval with document dates in the last sixty days.

```
BACKDAYS = 60
DATE_REF_FLD = BEDAT
BUKRS = 1000
FRGRL = X
```

**Use Case 2: Blocked release indicator**

**Purpose:** Focus on orders with release indicator set to blocked pending approval action.

```
FRGKE = B
BACKDAYS = 90
EKORG = 1000
```

**Use Case 3: Specific release group and strategy**

**Purpose:** Monitor waiting orders under one release group and strategy combination.

```
FRGGR = 01
FRGSX = 01
BACKDAYS = 45
BUKRS = 1000
```

**Use Case 4: Created-on reference window**

**Purpose:** Use header changed-on date instead of document date for the lookback window.

```
DATE_REF_FLD = AEDAT
BACKDAYS = 30
EKGRP = 001
LIFNR = 100000
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
| /SKN/S_SW_10_03_PO_APPR_WAIT | AEDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_APPR_WAIT | BEDAT | Document Date | DATS(8) | EBDAT |
| /SKN/S_SW_10_03_PO_APPR_WAIT | BSAKZ | Control indicator | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PO_APPR_WAIT | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_APPR_WAIT | BSART_DESC | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_APPR_WAIT | BSTYP | Purch. Doc. Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PO_APPR_WAIT | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_WAIT | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_APPR_WAIT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_APPR_WAIT | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_APPR_WAIT | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_APPR_WAIT | EKGRP | Purchasing Group | CHAR(3) | BKGRP |
| /SKN/S_SW_10_03_PO_APPR_WAIT | EKGRP_DESC | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_APPR_WAIT | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_APPR_WAIT | EKORG_DESC | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_APPR_WAIT | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_APPR_WAIT | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PO_APPR_WAIT | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PO_APPR_WAIT | FRGKE | Release indicator | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_PO_APPR_WAIT | FRGRL | Subject to release | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PO_APPR_WAIT | FRGSX | Release Strategy | CHAR(2) | FRGSX |
| /SKN/S_SW_10_03_PO_APPR_WAIT | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PO_APPR_WAIT | LIFNR | Vendor | CHAR(10) | ELIFN |
| /SKN/S_SW_10_03_PO_APPR_WAIT | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_APPR_WAIT | PROCSTAT | Purch. doc. proc. state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_PO_APPR_WAIT | PROCSTAT_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_WAIT | RESWK | Supplying Plant | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PO_APPR_WAIT | RESWK_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PO_APPR_WAIT | STATU | Status | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_APPR_WAIT | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_WAIT | VENDOR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_APPR_WAIT | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_APPR_WAIT | ZTERM | Terms of Payment | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PO_APPR_WAIT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_APPR_WAIT OPTIONAL
*"----------------------------------------------------------------------
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
            AEDAT        ERDAT,
            BEDAT        EBDAT,
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
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PO_APPR_WAIT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
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
    FROM EKKO
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
                     "FOR ALL ENTRIES IN zus
    WHERE FRGRL IN R_FRGRL    "  EQ 'X'
      AND FRGGR IN R_FRGGR
      AND FRGSX IN R_FRGSX
      AND EBELN IN R_EBELN
      AND BSTYP IN R_BSTYP
      AND EKORG IN R_EKORG
      AND BUKRS IN R_BUKRS
      AND LIFNR IN R_LIFNR
      AND RESWK IN R_RESWK
      AND BEDAT IN R_BEDAT
      AND AEDAT IN R_AEDAT
      AND BSART IN R_BSART
      AND EKGRP IN R_EKGRP
      AND ERNAM IN R_ERNAM
      AND ZTERM IN R_ZTERM
      AND WAERS IN R_WAERS
      AND LOEKZ EQ SPACE
      AND PROCSTAT IN R_PROCSTAT
     "----
      AND FRGSX > ''
      AND FRGKE IN R_FRGKE.
*********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR                   = T_DATA-FRGGR
        FRGSX                   = T_DATA-FRGSX
        FRGZU                   = T_DATA-FRGZU
      IMPORTING
        FRGC                    = T_DATA-FRGC
      EXCEPTIONS
        WRONG_COMBINATION       = 1
        OTHERS                  = 2.
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
      T_DATA-BSTYP_DESC = LV_DDTEXT.
    ENDIF.
    "-- STATU_DESC
    LV_DOMNAME = 'ESTAK'.
    LV_DOMVALUE = T_DATA-STATU.
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
      T_DATA-STATU_DESC = LV_DDTEXT.
    ENDIF.
    "-- PROCSTAT_DESC
    LV_DOMNAME = 'MEPROCSTATE'.
    LV_DOMVALUE = T_DATA-PROCSTAT.
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
      T_DATA-PROCSTAT_DESC = LV_DDTEXT.
    ENDIF.
    "-- BSART_DESC
    CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART            = T_DATA-BSART
        LANGU            = LV_LANGU
        BSTYP            = T_DATA-BSTYP
      IMPORTING
        TYPE_DESC        = T_DATA-BSART_DESC
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- Get  Vendor Decriptions
     CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
       EXPORTING
         LIFNR              = T_DATA-LIFNR
       IMPORTING
         VENDOR_DESC        = T_DATA-VENDOR_DESC
       EXCEPTIONS
         WRONG_VENDOR       = 1
         OTHERS             = 2.
     IF SY-SUBRC <> 0.
     ENDIF.
   "-- EKORG_DESC
    CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG              = T_DATA-EKORG
        "LANGU              = lv_LANGU
      IMPORTING
        PUR_ORG_DESC       = T_DATA-EKORG_DESC
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
   "-- EKGRP_DESC
    CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP              = T_DATA-EKGRP
*       LANGU              = lv_LANGU
      IMPORTING
        PUR_GRP_DESC       = T_DATA-EKGRP_DESC
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- RESWK_DESC (WERKS)
     CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
       EXPORTING
         WERKS            = T_DATA-RESWK
*        LANGU            = lv_LANGU
       IMPORTING
         PLANT_DESC       = T_DATA-RESWK_DESC
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2.
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
