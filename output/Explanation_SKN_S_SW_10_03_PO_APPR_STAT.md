# Exception Indicator: PUR PO Approval State ( SW_10_03_PO_APPR_STA)

## General Overview

This Exception Indicator monitors purchase orders that are subject to release and sit in selected approval or processing states, helping teams follow up on orders that are not fully released or completed.

This EI serves as an essential control for procurement approval governance by:

- Enabling detection of purchase orders awaiting or stuck in release-related processing states
- Supporting review of release group and strategy combinations before orders proceed to goods receipt or invoice
- Providing visibility into vendor, company, and purchasing organization context on flagged header records
- Enabling age-based prioritization when orders remain in scope for review after a chosen reference date
- Supporting audit sampling of release backlog before period close or delegation limit reviews

Typical use includes buyer follow-up on pending approvals, release-strategy validation, and periodic control samples of orders not yet in a final processing state. Results are intended for exception workflows rather than operational MM list reporting.

The routine reads purchase order header data for release-relevant documents, validates release group and strategy combinations, applies optional age filtering, and raises an alert when qualifying orders remain.


## Problem Description

Failure to monitor purchase order approval and processing state creates multiple risks across procurement control, compliance, and operational throughput:

**Procurement and Approval Risks**

- Orders awaiting release can delay sourcing, receipt, and payment when buyers do not see the backlog
- Invalid release group and strategy combinations may pass unnoticed until downstream processing fails
- Undetected orders in intermediate processing states can accumulate financial commitment without timely approval

**Operational Risks**

- Release scope that is too broad or too narrow can hide critical pending orders or flood reviewers with noise
- Lookback and age settings misaligned with review cadence can exclude recent backlog or retain stale rows
- Default processing-state scope that is not tuned can mix closed or irrelevant states into the queue

**Control and Audit Risks**

- Weak monitoring reduces evidence that release backlog was reviewed before close or accrual
- Lack of recurring exception review weakens segregation-of-duties over who may release high-value orders
- Missing age-based prioritization limits escalation of long-pending approval cases

## Suggested Resolution

**Immediate Response**

- Review flagged purchase orders for vendor, value context, release group, strategy, and current processing state
- Confirm whether each order should still be pending or requires buyer or approver action
- Prioritize high-value or long-waiting orders for release or rejection through standard workflows

**System Assessment**

- Validate lookback window and reference-date choice against how the team defines pending approval
- Review default processing-state scope and release filters so the queue reflects actionable backlog
- Compare exception counts by purchasing group, organization, and document type to find systematic gaps

**Corrective Actions**

- Complete or reject pending orders through standard MM release and approval processes
- Correct release strategy or master-data issues identified during review
- Document outcomes, brief buyers on recurring patterns, and schedule recurring runs before close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS | Days Backward |  | 0 | 0 |  |  |
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
| 19 | FRGCO | Release code |  | 0 | 0 |  |  |
| 20 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 21 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 22 | FRGSX | Release Strategy | CHAR | 2 | 0 | FRGSX | FRGSX |
| 23 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 24 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 25 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 26 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 27 | PROCSTAT_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 28 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 29 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 30 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 31 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 32 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 33 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 34 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 34 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Created on)

Date on Which Record Was Created (or last changed) is used to filter documents or master records by last maintenance activity.

**BACKDAYS** (Days Backward)

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

**FRGCO** (Release code)

Release Code is a standard two-character identifier that represents a specific approval authority or responsibility level within a purchasing document's release strategy.

**Not in use**
**FRGGR** (Release group)

Release group key controlling the purchasing release strategy framework.

**FRGRL** (Subject to release)

Release indicator/flag used in PO/PR release strategy control.

**FRGSX** (Release Strategy)

Extended release information or strategy outcome code complementing FRGST on MM release objects.

**LANGU** (Language for texts)

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

**Processing state defaults:** When **PROCSTAT** is not supplied, the selection includes blank, **02**, **03**, and **05** processing states (or null processing state).

**Release validation:** **FRGGR** and **FRGSX** on each order are checked against the configured release-code selection; rows that fail the release group and strategy combination check are removed.

**Header scope:** **FRGRL**, **EBELN**, **BUKRS**, **BSTYP**, **BSART**, **EKORG**, **EKGRP**, **LIFNR**, **RESWK**, **ERNAM**, **ZTERM**, and **WAERS** combine to define which release-relevant purchase order headers enter the result set.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - BEDAT
- **LANGU** - initial - defaults from system logon language when not supplied
- **PROCSTAT** - initial - treated as blank, 02, 03, and 05 by code

### Practical Example of Parameter Configuration

**Use Case 1: Pending approval backlog in the last sixty days**

**Purpose:** Review release-relevant purchase orders with document dates in the last sixty days.

```
BACKDAYS = 60
DATE_REF_FLD = BEDAT
BUKRS = 1000
```

**Use Case 2: One purchasing organization**

**Purpose:** Monitor approval-state backlog for a single purchasing organization.

```
EKORG = 1000
BACKDAYS = 90
FRGRL = X
```

**Use Case 3: Specific release group and strategy**

**Purpose:** Focus on orders under one release group and strategy combination.

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
| /SKN/S_SW_10_03_PO_APPR_STAT | AEDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_APPR_STAT | BEDAT | Document Date | DATS(8) | EBDAT |
| /SKN/S_SW_10_03_PO_APPR_STAT | BSAKZ | Control indicator | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PO_APPR_STAT | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_APPR_STAT | BSART_DESC | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_APPR_STAT | BSTYP | Purch. Doc. Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PO_APPR_STAT | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_STAT | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_APPR_STAT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_APPR_STAT | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_APPR_STAT | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_APPR_STAT | EKGRP | Purchasing Group | CHAR(3) | BKGRP |
| /SKN/S_SW_10_03_PO_APPR_STAT | EKGRP_DESC | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_APPR_STAT | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_APPR_STAT | EKORG_DESC | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_APPR_STAT | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_APPR_STAT | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PO_APPR_STAT | FRGRL | Subject to release | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PO_APPR_STAT | FRGSX | Release Strategy | CHAR(2) | FRGSX |
| /SKN/S_SW_10_03_PO_APPR_STAT | LIFNR | Vendor | CHAR(10) | ELIFN |
| /SKN/S_SW_10_03_PO_APPR_STAT | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_APPR_STAT | PROCSTAT | Purch. doc. proc. state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_PO_APPR_STAT | PROCSTAT_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_STAT | RESWK | Supplying Plant | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PO_APPR_STAT | RESWK_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PO_APPR_STAT | STATU | Status | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_APPR_STAT | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_STAT | VENDOR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_APPR_STAT | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_APPR_STAT | ZTERM | Terms of Payment | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PO_APPR_STAT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_APPR_STAT OPTIONAL
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
    CALL FUNCTION '/SKN/FC_SW_10_03_PO_APPR_STAT'
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
  IF R_PROCSTAT[] IS INITIAL.
    RS_PROCSTAT-SIGN = 'I'.
     RS_PROCSTAT-OPTION = 'EQ'.
      RS_PROCSTAT-LOW = SPACE.
       APPEND RS_PROCSTAT TO R_PROCSTAT.
      RS_PROCSTAT-LOW = '02'.
       APPEND RS_PROCSTAT TO R_PROCSTAT.
      RS_PROCSTAT-LOW = '03'.
       APPEND RS_PROCSTAT TO R_PROCSTAT.
      RS_PROCSTAT-LOW = '05'.
       APPEND RS_PROCSTAT TO R_PROCSTAT.
  ENDIF.
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
      AND ( PROCSTAT IN R_PROCSTAT
            OR PROCSTAT IS NULL ).
*********************************************************************************
  "--- Check Release group - Release strategy combination
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_REL_GRP_STRT_CHK'
      EXPORTING
*       FRGOT                   = '2'
*       FRGCO                   =
        FRGGR                   = T_DATA-FRGGR
        FRGSX                   = T_DATA-FRGSX
      IMPORTING
        FRGCO                   = LV_FRGCO
*       WA                      =
      TABLES
        T_FRGCO                 = R_FRGCO
      EXCEPTIONS
        WRONG_COMBINATION       = 1
        OTHERS                  = 2.
    IF SY-SUBRC <> 0.
      DELETE T_DATA INDEX SY_TABIX.
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
