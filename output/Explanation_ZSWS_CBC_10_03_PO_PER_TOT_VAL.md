# Exception Indicator: PO - Total Group Value Control ( CBC_PO_TOT_VAL)

## General Overview

This Exception Indicator monitors purchase order activity to identify vendor and buyer combinations whose combined order value exceeds a configured group total, helping organizations detect concentrated spend that may warrant additional review.

This EI serves as an essential control for procurement governance by:

- Highlighting cumulative purchase value grouped by vendor and the user who created the orders
- Supporting spend-concentration and delegation-of-authority reviews without exporting the full purchasing population
- Enabling scoped monitoring by company, purchasing organization, document type, and release attributes
- Applying optional age filtering relative to a chosen reference date on each returned document
- Enriching results with document-type, vendor, and organizational descriptions for faster triage

Typical use includes vendor–buyer spend reviews, threshold-based sampling, and periodic controls before period close. Results are intended for exception workflows rather than operational purchasing extracts.

The routine reads purchase order headers, calculates net order values, rolls them up by vendor and creator, compares the cumulative total to the configured threshold, and returns the underlying orders that belong to qualifying groups.


## Problem Description

Failure to monitor cumulative purchase order value by vendor and creator creates multiple risks across procurement control, financial exposure, and compliance:

**Procurement and Financial Risks**

- High combined order value for one vendor and buyer pair can indicate insufficient splitting controls or unauthorized aggregation of spend
- Release and processing-state scope that is too broad can dilute the exception queue with irrelevant documents
- Currency and company-code scope that is misaligned can distort perceived concentration of spend

**Operational Risks**

- Monitoring windows that do not match business intent can exclude recent orders or retain obsolete history
- Group-total thresholds set too low flood reviewers; thresholds set too high miss meaningful concentration
- Age filters applied after load can retain rows outside the intended recency band when not tuned with the date window

**Control and Audit Risks**

- Weak monitoring reduces evidence that high cumulative totals were reviewed against delegation-of-authority rules
- Release-code validation present in older design notes is not active in the supplied on-premise code path

## Suggested Resolution

**Immediate Response**

- Review flagged purchase orders, vendor, creator, individual net values, and the cumulative group total
- Confirm with procurement management whether the combined value is authorized for that vendor–buyer relationship
- Prioritize open or recently changed documents with the highest individual net values

**System Assessment**

- Validate the group-total threshold against organizational limits for cumulative purchase value per vendor and user
- Align the monitoring window and reference date choice with how the business defines recent spend
- Review release group and strategy filters against the purchase order population you intend to monitor

**Corrective Actions**

- Correct or split purchase documents through standard MM processes where limits were exceeded
- Adjust monitoring scope and thresholds after root-cause review to keep queues actionable
- Document review outcomes for audit and schedule recurring runs for critical vendors or purchasing groups


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS | Days Backward |  | 0 | 0 |  |  |
| 3 | BEDAT | Document Date | DATS | 8 | 0 | EBDAT | DATUM |
| 4 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 5 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 6 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 7 | DATE_REF_FLD | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |  |
| 8 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 9 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 11 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 12 | EKGRP | Purchasing Group | CHAR | 3 | 0 | BKGRP | EKGRP |
| 13 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 14 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 15 | FRGCO | CHAR | 2 | 0 | FRGCO | FRGCO |  |
| 16 | FRGGR | CHAR | 2 | 0 | FRGGR | FRGGR |  |
| 17 | FRGRL | CHAR | 1 | 0 | FRGRL | FRGRL |  |
| 18 | FRGSX | CHAR | 2 | 0 | FRGSX | FRGSX |  |
| 19 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 20 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 21 | PO_GRP_AMOUNT | Group Total Amount | CURR | 13 | 2 | BWERT | WERT7 |
| 22 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 23 | RESWK | CHAR | 4 | 0 | RESWK | WERKS |  |
| 24 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 25 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 25 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**AEDAT** (Created on)

Changed-on date used to filter documents or master records by last maintenance activity.

**BACKDAYS** (Days Backward)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BEDAT** (Document Date)

Purchasing document date used to filter procurement documents by document creation period.

**BSART** (Purchasing Doc. Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BSTYP** (Purch. Doc. Category)

Purchasing document category (PR/PO/contract etc.) used to segment MM document classes.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATE_REF_FLD** (CHAR)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- AEDAT — Changed-on date used to filter documents or master records by last maintenance activity.
- BEDAT — Purchasing document date used to filter procurement documents by document creation period.

**DATUM** (DATS)

Optional reference date from the selection framework; when empty, the monitoring range is built from **BACKDAYS** and applied to **AEDAT** or **BEDAT** per **DATE_REF_FLD**.

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

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**FRGCO** (CHAR)

Release code used in purchasing release strategy; read from selection but not applied in the active **EKKO** selection in the supplied ABAP.

**FRGGR** (CHAR)

Release group key controlling the purchasing release strategy framework.

**FRGRL** (CHAR)

Release indicator/flag used in PO/PR release strategy control.

**FRGSX** (CHAR)

Extended release information or strategy outcome code complementing FRGST on MM release objects.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**PO_GRP_AMOUNT** (Group Total Amount)

Minimum cumulative net order value threshold for a vendor and created-by user pair; groups below the selected value are excluded before purchase orders are returned.

**PROCSTAT** (Purch. doc. proc. state)

Purchasing document processing state describing lifecycle and processing of MM purchasing objects.

**RESWK** (CHAR)

Supplying/receiving plant key used in cross-plant logistics analysis.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**ZTERM** (Terms of Payment)

Terms of payment key driving baseline due dates, cash discount periods, and payment rules.


### Parameter Relationships

How parameter combinations work together

**Header selection:** Purchase order headers are read from **EKKO** with filters on **FRGRL**, **FRGGR**, **FRGSX**, **EBELN**, **BSTYP**, **EKORG**, **BUKRS**, **LIFNR**, **RESWK**, **BEDAT**, **AEDAT**, **BSART**, **EKGRP**, **ERNAM**, **ZTERM**, **WAERS**, and **PROCSTAT**. Deleted headers (**LOEKZ**) are excluded in code.

**Date window:** When **DATUM** is empty, a from-date of today minus **BACKDAYS** is built. **DATE_REF_FLD** routes that range to **AEDAT** when **AEDAT**, to **BEDAT** when **BEDAT**, or to **BEDAT** for any other value. Explicit **AEDAT** or **BEDAT** selections override the copied range.

**Value rollup:** Net order value is calculated per **EBELN**, summed by **LIFNR** and **ERNAM** into **PO_GRP_AMOUNT**, and groups that do not match **PO_GRP_AMOUNT** selection are dropped before output rows are built.

**Age filter:** **DURATION** is computed on each output row from the field named in **DATE_REF_FLD** using **DURATION_UNIT**; rows are removed when **DURATION** is not in the selected **DURATION** range.

**Release code:** **FRGCO** is read from selection but the release-group/strategy validation call is commented out in the supplied ABAP.

**Descriptions:** **LANGU** defaults from the system logon language when not supplied and drives domain and master-data short texts on output.


### Default Values

- **BACKDAYS** - initial - treated as 3 by code
- **DURATION** - initial - treated as 0 by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - initial - treated as AEDAT by code
- **LANGU** - initial - defaults from system logon language when not supplied

### Practical Example of Parameter Configuration

**Use Case 1: High group total in last ninety days**

**Purpose:** Flag vendor–creator pairs whose combined net PO value exceeds a threshold for orders changed in the last ninety days.

```
BACKDAYS = 90
DATE_REF_FLD = AEDAT
PO_GRP_AMOUNT = 500000
```

**Use Case 2: Released orders for one vendor**

**Purpose:** Monitor cumulative value for a single vendor with release indicator set.

```
LIFNR = 100000
FRGRL = X
BACKDAYS = 30
PO_GRP_AMOUNT = 100000
```

**Use Case 3: Document-date window**

**Purpose:** Use purchase order date instead of changed-on date for the monitoring window.

```
DATE_REF_FLD = BEDAT
BACKDAYS = 60
PO_GRP_AMOUNT = 250000
```

**Use Case 4: Age filter on reference date**

**Purpose:** Keep only rows whose reference date is at least thirty days old.

```
BACKDAYS = 180
DURATION = 30
DURATION_UNIT = D
PO_GRP_AMOUNT = 75000
```

**Use Case 5: Specific purchasing document**

**Purpose:** Review one PO and its contribution to the vendor–creator group total.

```
EBELN = 4500012345
PO_GRP_AMOUNT = 1
```

**Use Case 6: Exactly seven full days since reference date**

**Purpose:** Return groups where the reference date is exactly 7 full days ago.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 90
PO_GRP_AMOUNT = 50000
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | AEDAT | Created on | DATS(8) | ERDAT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | BEDAT | Document Date | DATS(8) | EBDAT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | BSAKZ | Control indicator | CHAR(1) | BSAKZ |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | BSART_DESC | Doc. Type Descript. | CHAR(20) | BATXT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | BSTYP | Purch. Doc. Category | CHAR(1) | EBSTYP |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | BUKRS | Company Code | CHAR(4) | BUKRS |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | EBELN | Purchasing Document | CHAR(10) | EBELN |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | EKGRP | Purchasing Group | CHAR(3) | BKGRP |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | EKGRP_DESC | Description p. group | CHAR(18) | EKNAM |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | EKORG | Purch. Organization | CHAR(4) | EKORG |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | EKORG_DESC | Description | CHAR(20) | EKOTX |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | ERNAM | Created by | CHAR(12) | ERNAM |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | LIFNR | Vendor | CHAR(10) | ELIFN |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | PO_GRP_AMOUNT | Net Order Value | CURR(13) | BWERT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | PO_VALUE | Net Order Value | CURR(13) | BWERT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | PO_VALUE_ILS | Net Order Value | CURR(13) | BWERT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | PROCSTAT | Purch. doc. proc. state | CHAR(2) | MEPROCSTATE |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | PROCSTAT_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | STATU | Status | CHAR(1) | ESTAK |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | VENDOR_DESC | Name | CHAR(35) | NAME1_GP |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | WAERS | Currency | CUKY(5) | WAERS |
| ZSWS_CBC_10_03_PO_PER_TOT_VAL | ZTERM | Terms of Payment | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION ZSWF_CBC_10_03_PO_PER_TOT_VAL .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  ZSWS_CBC_10_03_PO_PER_TOT_VAL OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: LANGU  LANGU,
             BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
 LV_BACKDAYS = 3.
 LV_DURATION_UNIT = 'D'.
 LV_LANGU = SY-LANGU.
 LV_DATE_REF_FLD = 'AEDAT'. "PO Creation
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
            DURATION    /SKN/E_SW_DURATION,
            PO_GRP_AMOUNT BPREI.
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
            DURATION,
            PO_GRP_AMOUNT.
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
DATA: BEGIN OF LS_EKKO.
 INCLUDE STRUCTURE EKKO.
DATA: PO_VALUE_ILS TYPE BPREI,      "
      PO_VALUE     TYPE BPREI.      "
DATA: END OF LS_EKKO.
DATA: LT_EKKO LIKE TABLE OF LS_EKKO.
DATA: BEGIN OF LS_EKKO_TOT.
DATA: LIFNR TYPE ELIFN,
      ERNAM TYPE ERNAM.
DATA: PO_GRP_AMOUNT TYPE BPREI.      "
DATA: END OF LS_EKKO_TOT.
DATA: LT_EKKO_TOT LIKE TABLE OF LS_EKKO_TOT.
DATA: LS_EKPO TYPE EKPO,
      LT_EKPO LIKE TABLE OF LS_EKPO.
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
***  "--- Set R_PROCSTAT
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
  REFRESH LT_EKKO.
  SELECT *
    FROM EKKO
    INTO CORRESPONDING FIELDS OF TABLE LT_EKKO
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
      AND PROCSTAT IN R_PROCSTAT.
    "--- Calculate PO tot value
    DATA: LT_PO_TOTAL TYPE  ZMM_PO_TOTAL_T,
          LS_PO_TOTAL LIKE LINE OF LT_PO_TOTAL.
    LOOP AT LT_EKKO INTO LS_EKKO.
      MOVE-CORRESPONDING LS_EKKO TO LS_PO_TOTAL.
      APPEND LS_PO_TOTAL TO LT_PO_TOTAL.
    ENDLOOP.
    CALL FUNCTION 'ZMM_CALC_PO_TOTAL'
      TABLES
        T_EKPO            = LT_EKPO
      CHANGING
        CT_PO_TOTAL       = LT_PO_TOTAL.
     SORT LT_PO_TOTAL BY EBELN.
    LOOP AT LT_EKKO INTO LS_EKKO.
      SY_TABIX = SY-TABIX.
      READ TABLE LT_PO_TOTAL INTO LS_PO_TOTAL
                             WITH KEY EBELN = LS_EKKO-EBELN
                             BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        MOVE-CORRESPONDING LS_PO_TOTAL TO LS_EKKO.
        MODIFY LT_EKKO FROM LS_EKKO INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
    "--- Calculate cummulative value
    SORT LT_EKKO BY LIFNR ERNAM.
    LOOP AT LT_EKKO INTO LS_EKKO.
      MOVE-CORRESPONDING LS_EKKO TO LS_EKKO_TOT.
      LS_EKKO_TOT-PO_GRP_AMOUNT = LS_EKKO-PO_VALUE_ILS.
      COLLECT LS_EKKO_TOT INTO LT_EKKO_TOT.
    ENDLOOP.
    DELETE LT_EKKO_TOT WHERE PO_GRP_AMOUNT NOT IN R_PO_GRP_AMOUNT.
    SORT LT_EKKO_TOT BY LIFNR ERNAM.
    "---- Fill result table
    LOOP AT LT_EKKO INTO LS_EKKO.
      MOVE-CORRESPONDING LS_EKKO TO T_DATA.
      READ TABLE LT_EKKO_TOT INTO LS_EKKO_TOT
                             WITH KEY LIFNR = LS_EKKO-LIFNR
                                      ERNAM = LS_EKKO-ERNAM
                             BINARY SEARCH .
      IF SY-SUBRC IS INITIAL.
        MOVE-CORRESPONDING LS_EKKO_TOT TO T_DATA.
        APPEND T_DATA.
      ENDIF.
    ENDLOOP.
*********************************************************************************
***  "--- Check Release group - Release strategy combination
***  loop at t_data .
***    sy_tabix = sy-tabix .
***    CALL FUNCTION '/SKN/F_SW_10_REL_GRP_STRT_CHK'
***      EXPORTING
****       FRGOT                   = '2'
****       FRGCO                   =
***        frggr                   = t_data-FRGGR
***        FRGSX                   = t_data-FRGSX
***      IMPORTING
***        FRGCO                   = lv_FRGCO
****       WA                      =
***      TABLES
***        T_FRGCO                 = R_FRGCO
***      EXCEPTIONS
***        WRONG_COMBINATION       = 1
***        OTHERS                  = 2.
***    IF sy-subrc <> 0.
***      delete t_data index sy_tabix.
***    ENDIF.
***  endloop.
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
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
