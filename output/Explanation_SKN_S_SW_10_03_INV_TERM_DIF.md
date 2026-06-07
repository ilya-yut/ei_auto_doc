# Exception Indicator: Vendor payment terms differs invoice payment terms ( SW_10_03_INV_TERM_DI)

## General Overview

This Exception Indicator finds posted vendor invoice documents where the payment terms on the invoice differ from the vendor’s payment terms on the linked purchasing organization, then compares calculated net due dates to highlight whether cash timing is earlier or later than vendor master terms would imply.

This EI serves as an essential control for accounts payable and procurement by:

- Detecting invoice payment terms that do not match vendor purchasing-organization terms on linked purchase orders
- Limiting review to posted, uncanceled logistics invoice documents in scope
- Calculating net due dates for both the invoice terms and the vendor terms and the day difference between them
- Supporting optional filtering for earlier versus later net due timing relative to vendor terms
- Providing vendor, company, document, and purchasing organization context for remediation

Typical use includes invoice validation after MIRO posting, vendor master alignment reviews, and sampling for payment-timing risk. Results are intended for exception workflows rather than full accounts payable extracts.

The routine reads invoice header data joined to purchase order and vendor purchasing data, keeps rows where invoice and vendor payment terms differ, enriches due-date calculations, and raises an alert when qualifying mismatches remain after filtering.


## Problem Description

Failure to monitor invoice payment terms against vendor purchasing-organization terms creates multiple risks across accounts payable accuracy, cash management, and vendor master quality:

**Financial and Reporting Risks**

- Invoices posted with payment terms that differ from vendor master terms can drive wrong due dates, discounts, and cash forecasts
- Net due dates derived from invoice terms versus vendor terms can diverge without visible comparison in standard document display
- High invoice volumes make manual comparison of terms and due dates impractical across vendors and company codes

**Operational Risks**

- Monitoring windows misaligned with entry or posting dates can miss recent mismatches or retain closed-period noise
- Day-difference thresholds set too tight or too loose can flood reviewers or hide material timing gaps
- Age filters applied after load can retain rows outside the intended recency band when not tuned with the date window

**Control and Audit Risks**

- Weak monitoring reduces evidence that payment-term mismatches were reviewed before payment runs
- Release-related selection parameters present in the parameter set are not applied on the active invoice read in the supplied on-premise code

## Suggested Resolution

**Immediate Response**

- Review flagged invoices, vendor, company code, invoice and vendor payment terms, and both calculated net due dates
- Confirm with accounts payable whether the invoice terms are correct or should follow vendor master terms
- Prioritize high-value or near-due invoices where the day difference indicates materially earlier or later payment timing

**System Assessment**

- Validate monitoring window and reference date choice against how the team defines recent invoice activity
- Tune day-difference and earlier-versus-later filters so the queue reflects policy on acceptable timing gaps
- Review company code, vendor, and document-type scope against the invoice population you intend to monitor

**Corrective Actions**

- Correct vendor purchasing-organization payment terms or reprocess invoices through standard FI/MM processes where terms are wrong
- Adjust monitoring scope and thresholds after root-cause review to keep queues actionable
- Document review outcomes for audit and schedule recurring runs for critical vendors or company codes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ADV_VS_DELAYED | Adv.vs Delayed (A-adv,D-del) | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 2 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 3 | BELNR | Invoice Document No. | CHAR | 10 | 0 | RE_BELNR | BELNR |
| 4 | BLART | Document Type | CHAR | 2 | 0 | BLART | BLART |
| 5 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 6 | BSART | CHAR | 4 | 0 | ESART | BSART |  |
| 7 | BSTYP | CHAR | 1 | 0 | EBSTYP | EBSTYP |  |
| 8 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 9 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 10 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 11 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 12 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 14 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 15 | EBELN | CHAR | 10 | 0 | EBELN | EBELN |  |
| 16 | EKGRP | CHAR | 3 | 0 | BKGRP | EKGRP |  |
| 17 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 18 | FRGCO | CHAR | 2 | 0 | FRGCO | FRGCO |  |
| 19 | FRGGR | CHAR | 2 | 0 | FRGGR | FRGGR |  |
| 20 | FRGKE | CHAR | 1 | 0 | FRGKE | FRGKE |  |
| 21 | FRGRL | CHAR | 1 | 0 | FRGRL | FRGRL |  |
| 22 | FRGSX | CHAR | 2 | 0 | FRGSX | FRGSX |  |
| 23 | LANGU | Language |  | 0 | 0 |  |  |
| 24 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 25 | NETDT_DIFF | Days difference | INT4 | 10 | 0 | /SKN/DAYS_DIF | INT4 |
| 26 | PROCSTAT | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |  |
| 27 | RESWK | CHAR | 4 | 0 | RESWK | WERKS |  |
| 28 | SW_DEST |  | 0 | 0 |  |  |  |
| 29 | USNAM | CHAR | 12 | 0 | USNAM | XUBNAME |  |
| 30 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 31 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 31 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ADV_VS_DELAYED** (Adv.vs Delayed (A-adv,D-del))

Filters output rows by whether invoice net due is earlier or later than vendor-terms net due after the day difference is calculated.

**ADV_VS_DELAYED Options:**
- **A** — keep rows where invoice net due is earlier than vendor-terms net due (positive day difference).
- **D** — keep rows where invoice net due is later than vendor-terms net due (negative day difference).
- Empty or blank — do not apply the advance/delayed filter.

**BACKDAYS** (Back days)

<mark>BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.</mark>

Backdays is based on DATE_REF_FLD field.

**BELNR** (Invoice Document No.)

Accounting document number, the primary FI document key for journal-level traceability.

**BLART** (Document Type)

FI document type classifying accounting documents such as invoices, payments, or general postings.

**BLDAT** (Document Date)

Document date from the source business document, often used as legal/document reference date.

**BSART** (CHAR)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BSTYP** (CHAR)

Purchasing document category (PR/PO/contract etc.) used to segment MM document classes.

**BUDAT** (Posting Date)

Posting date used to align analysis with accounting period recognition.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**CPUDT** (Entry Date)

Entry/creation date used for technical posting timestamp filtering.

**DATE_REF_FLD** (Date reference field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- CPUDT — Entry/creation date used for technical posting timestamp filtering.
- BLDAT — Document date from the source business document, often used as legal/document reference date.
- BUDAT — Posting date used to align analysis with accounting period recognition.

**DATUM** (DATS)

Optional reference date from the selection framework; when empty, the monitoring range is built from **BACKDAYS** and applied to **CPUDT**, **BLDAT**, or **BUDAT** per **DATE_REF_FLD**.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**EBELN** (CHAR)

Purchasing document number (typically PO) used as the primary MM document key.

**EKGRP** (CHAR)

Purchasing group (buyer) used for procurement ownership and control segmentation.

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**FRGCO** (CHAR)

Release code used in purchasing release strategy; read from selection but not applied in the active invoice selection in the supplied ABAP.

**FRGGR** (CHAR)

Release group key controlling the purchasing release strategy framework.

**FRGKE** (CHAR)

Release status indicator used to distinguish released vs unreleased documents.

**FRGRL** (CHAR)

Release indicator/flag used in PO/PR release strategy control.

**FRGSX** (CHAR)

Extended release information or strategy outcome code complementing FRGST on MM release objects.

**LANGU** (Language)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**NETDT_DIFF** (Days difference)

Day difference between invoice net due date and vendor-terms net due date; rows must match the selected range after calculation.

**PROCSTAT** (CHAR)

Purchasing document processing state describing lifecycle and processing of MM purchasing objects.

**RESWK** (CHAR)

Supplying/receiving plant key used in cross-plant logistics analysis.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**USNAM** (CHAR)

SAP changed-by/created-by user field used for accountability filtering.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**ZTERM** (Terms of Payment)

Terms of payment key driving baseline due dates, cash discount periods, and payment rules.


### Parameter Relationships

How parameter combinations work together

**Cloud execution:** When **SW_DEST** is set, processing delegates to `/SKN/FC_SW_10_03_INV_TERM_DIF` and the on-premise path below that call is skipped.

**Date window:** When **DATUM** is empty, a from-date of today minus **BACKDAYS** is built. **DATE_REF_FLD** routes that range to **CPUDT**, **BLDAT**, or **BUDAT** (default **CPUDT** when initial). Explicit **CPUDT**, **BLDAT**, or **BUDAT** selections override the copied range.

**Document type:** When **BLART** is empty, document types linked to transaction **MIRO** in **T169F** populate the invoice document-type filter before the main read.

**Invoice selection:** Posted, uncanceled invoice headers are read with filters on **BLART**, **BUKRS**, **CPUDT**, **LIFNR**, **BELNR**, **BLDAT**, **BUDAT**, and **USNAM**. Rows are kept only when invoice payment terms differ from vendor purchasing-organization terms on the linked purchase order (**ZTERM** on invoice versus **LFM1** terms).

**Due-date comparison:** For each row, net due date is calculated from invoice baseline and cash-discount days, then again using vendor master payment terms; **NETDT_DIFF** stores the day difference. Output **ADV_VS_DELAYED** is set to **A** when the difference is positive and **D** when negative.

**Post filters:** After the date window selects invoice rows, **DURATION** with **DURATION_UNIT** applies a second age filter on the reference date field. Rows are removed when **DURATION** is not in the selected range. **NETDT_DIFF** selection narrows by day difference. When **ADV_VS_DELAYED** is **A**, rows with non-positive differences are dropped; when **D**, rows with non-negative differences are dropped.

**Release parameters:** **FRGRL**, **FRGGR**, **FRGSX**, **FRGCO**, and **FRGKE** are read from selection but are not used in the active invoice **SELECT** in the supplied ABAP.

**Descriptions:** **LANGU** defaults from the system logon language when not supplied and drives short texts on output.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as 0 by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - initial - treated as CPUDT by code
- **LANGU** - initial - defaults from system logon language when not supplied

### Practical Example of Parameter Configuration

**Use Case 1: Recent invoice term mismatches**

**Purpose:** Find posted invoices in the last thirty days where invoice payment terms differ from vendor terms.

```
BACKDAYS = 30
DATE_REF_FLD = CPUDT
```

**Use Case 2: Earlier net due than vendor terms**

**Purpose:** Keep only rows where invoice net due is earlier than vendor-terms net due.

```
BACKDAYS = 60
ADV_VS_DELAYED = A
NETDT_DIFF = 1
```

**Use Case 3: Later net due than vendor terms**

**Purpose:** Keep only rows where invoice net due is later than vendor-terms net due.

```
BACKDAYS = 90
ADV_VS_DELAYED = D
NETDT_DIFF = 1
```

**Use Case 4: One vendor and company**

**Purpose:** Review payment-term mismatches for a single vendor in one company code.

```
LIFNR = 100000
BUKRS = 1000
BACKDAYS = 45
```

**Use Case 5: Specific invoice document**

**Purpose:** Analyze one posted invoice against vendor terms.

```
BELNR = 5100000123
BUKRS = 1000
```

**Use Case 6: Exactly seven full days since reference date**

**Purpose:** Return rows where the reference date is exactly 7 full days ago.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 90
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_03_INV_TERM_DIF | ADV_VS_DELAYED | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_03_INV_TERM_DIF | BELNR | Invoice Document No. | CHAR(10) | RE_BELNR |
| /SKN/S_SW_10_03_INV_TERM_DIF | BLART | Document Type | CHAR(2) | BLART |
| /SKN/S_SW_10_03_INV_TERM_DIF | BLART_DESC | Description | CHAR(20) | LTEXT_003T |
| /SKN/S_SW_10_03_INV_TERM_DIF | BLDAT | Document Date | DATS(8) | BLDAT |
| /SKN/S_SW_10_03_INV_TERM_DIF | BUDAT | Posting Date | DATS(8) | BUDAT |
| /SKN/S_SW_10_03_INV_TERM_DIF | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_INV_TERM_DIF | CPUDT | Entry Date | DATS(8) | CPUDT |
| /SKN/S_SW_10_03_INV_TERM_DIF | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_INV_TERM_DIF | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_INV_TERM_DIF | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_INV_TERM_DIF | EKORG_DESC | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_INV_TERM_DIF | ERNAME | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_INV_TERM_DIF | LFB1_ZTERM | Terms of Payment | CHAR(4) | DZTERM |
| /SKN/S_SW_10_03_INV_TERM_DIF | LFM1_ZTERM | Terms of Payment | CHAR(4) | DZTERM |
| /SKN/S_SW_10_03_INV_TERM_DIF | LIFNR | Vendor | CHAR(10) | ELIFN |
| /SKN/S_SW_10_03_INV_TERM_DIF | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_INV_TERM_DIF | NETDT | Net due date | DATS(8) | NETDT |
| /SKN/S_SW_10_03_INV_TERM_DIF | NETDT_DIFF | Days difference | INT4(10) | /SKN/DAYS_DIF |
| /SKN/S_SW_10_03_INV_TERM_DIF | NETDT_VEND | Net due date | DATS(8) | NETDT |
| /SKN/S_SW_10_03_INV_TERM_DIF | RMWWR | Gross invoice amount | CURR(13) | RMWWR |
| /SKN/S_SW_10_03_INV_TERM_DIF | VENDOR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_INV_TERM_DIF | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_INV_TERM_DIF | ZBD1T | Days 1 | DEC(3) | DZBD1T |
| /SKN/S_SW_10_03_INV_TERM_DIF | ZBD1T_VEND | Days 1 | DEC(3) | DZBD1T |
| /SKN/S_SW_10_03_INV_TERM_DIF | ZFBDT | Baseline Payment Dte | DATS(8) | DZFBDT |
| /SKN/S_SW_10_03_INV_TERM_DIF | ZFBDT_VEND | Baseline Payment Dte | DATS(8) | DZFBDT |
| /SKN/S_SW_10_03_INV_TERM_DIF | ZTERM | Terms of Payment | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_INV_TERM_DIF .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_INV_TERM_DIF OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               ADV_VS_DELAYED CHAR1. ""A-adv-before, D-delayed - after
  LV_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  LV_DATE_REF_FLD = 'CPUDT'. "Entered on
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT,
                 ADV_VS_DELAYED.
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
              USNAM        USNAM,
              BLDAT        BLDAT,
              BUDAT        BUDAT,
              WAERS        WAERS,
              PROCSTAT    MEPROCSTATE,
              DATUM        SY-DATUM,
              CPUDT        CPUDT,
              BLART        BLART,
              BELNR     RE_BELNR,
              NETDT_DIFF   INT4,
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
              USNAM,
              BLDAT,
              BUDAT,
              WAERS,
              PROCSTAT,
              DATUM,
              CPUDT,
              BLART,
              BELNR,
              NETDT_DIFF,
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
  DATA: LS_T169F TYPE T169F,
        LT_T169F LIKE TABLE OF LS_T169F.
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
  DATA : E_FAEDE TYPE FAEDE,
         I_FAEDE TYPE FAEDE.
  DATA:  LV_ZFBDT TYPE DZFBDT,
         LV_ZBD1T TYPE DZBD1T.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_INV_TERM_DIF'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
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
    WHEN 'CPUDT'.
      R_CPUDT[] = R_DATUM[]. "Entered on
    WHEN 'BLDAT'.
      R_BLDAT[] = R_DATUM[]. "PO Date
    WHEN 'BUDAT'.
      R_BUDAT[] = R_DATUM[]. "PO Date
    WHEN OTHERS.
      R_CPUDT[] = R_DATUM[]. "Billing date
  ENDCASE.
  "--- Prepare BLART
  SELECT *
    FROM T169F
    INTO CORRESPONDING FIELDS OF TABLE LT_T169F
    WHERE TCODE EQ 'MIRO'
    AND   BLART IN R_BLART.
  REFRESH R_BLART.
  LOOP AT LT_T169F INTO LS_T169F.
    RS_BLART-SIGN = 'I'.
    RS_BLART-OPTION = 'EQ'.
    RS_BLART-LOW = LS_T169F-BLART.
    APPEND RS_BLART TO R_BLART.
  ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT R~LIFNR R~ZBD3T R~ZFBDT R~BLDAT R~BUDAT R~CPUDT R~USNAM R~BUKRS R~RMWWR R~WAERS R~ZTERM
         R~USNAM R~BELNR R~BLART R~ZFBDT R~ZBD1T
*         l~zterm AS lfb1_zterm           " 20.06.21--
         L~EKORG                          " 20.06.21++
         L~ZTERM AS LFM1_ZTERM            " 20.06.21++
    FROM RBKP AS R
*    INNER JOIN lfb1 AS l    " 20.06.21--
*** 20.06.21++
    INNER JOIN RSEG AS RS ON  R~BELNR  EQ RS~BELNR
                          AND R~GJAHR  EQ RS~GJAHR
    INNER JOIN EKKO AS E  ON  RS~EBELN EQ E~EBELN
    INNER JOIN LFM1 AS L  ON  R~LIFNR  EQ L~LIFNR
                          AND E~EKORG  EQ L~EKORG
*** 20.06.21++
*    AND r~bukrs EQ l~bukrs  " 20.06.21--
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE R~BLART  IN R_BLART
      AND R~BUKRS  IN R_BUKRS
      AND R~CPUDT  IN R_CPUDT
      AND R~LIFNR  IN R_LIFNR
      AND R~BELNR  IN R_BELNR
      AND R~BLDAT  IN R_BLDAT
      AND R~BUDAT  IN R_BUDAT
      AND R~USNAM  IN R_USNAM
      AND R~RBSTAT IN ( '5',' ' ) "  (posted invoice)
      AND R~STBLG  EQ ' '         "  (uncanceled invoice)
      AND R~ZTERM  NE L~ZTERM.
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
********************************************************************************
  "--- Set Descriptions
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
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
    CLEAR I_FAEDE.
    I_FAEDE-SHKZG = 'H'.
    I_FAEDE-KOART = 'K'.
    I_FAEDE-ZFBDT = T_DATA-ZFBDT.
    I_FAEDE-ZBD1T = T_DATA-ZBD1T.
    CALL FUNCTION 'DETERMINE_DUE_DATE'
      EXPORTING
        I_FAEDE                    = I_FAEDE
*       I_GL_FAEDE                 =
      IMPORTING
        E_FAEDE                    = E_FAEDE
      EXCEPTIONS
        ACCOUNT_TYPE_NOT_SUPPORTED = 1
        OTHERS                     = 2.
    IF SY-SUBRC = 0.
      T_DATA-NETDT = E_FAEDE-NETDT.
    ENDIF.
    "====
    CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
      EXPORTING
        I_BLDAT         = T_DATA-BLDAT
        I_BUDAT         = T_DATA-BUDAT
        I_CPUDT         = T_DATA-CPUDT
*       I_ZFBDT         =
*       i_zterm         = t_data-lfb1_zterm     " 20.06.21--
        I_ZTERM         = T_DATA-LFM1_ZTERM     " 20.06.21++
*       I_REINDAT       =
*       I_LIFNR         =
*       I_BUKRS         =
      IMPORTING
        E_ZBD1T         = LV_ZBD1T
*       E_ZBD1P         =
*       E_ZBD2T         =
*       E_ZBD2P         =
*       E_ZBD3T         =
        E_ZFBDT         = LV_ZFBDT
*       E_SPLIT         =
*       E_ZSCHF         =
*       E_ZLSCH         =
*       E_T052          =
      EXCEPTIONS
        TERMS_NOT_FOUND = 1
        OTHERS          = 2.
    IF SY-SUBRC <> 0.
      CLEAR : LV_ZBD1T,
              LV_ZFBDT.
    ENDIF.
    "===
    CLEAR I_FAEDE.
    I_FAEDE-SHKZG = 'H'.
    I_FAEDE-KOART = 'K'.
    I_FAEDE-ZFBDT = LV_ZFBDT.
    I_FAEDE-ZBD1T = LV_ZBD1T.
    CALL FUNCTION 'DETERMINE_DUE_DATE'
      EXPORTING
        I_FAEDE                    = I_FAEDE
*       I_GL_FAEDE                 =
      IMPORTING
        E_FAEDE                    = E_FAEDE
      EXCEPTIONS
        ACCOUNT_TYPE_NOT_SUPPORTED = 1
        OTHERS                     = 2.
    IF SY-SUBRC = 0.
      T_DATA-NETDT_VEND = E_FAEDE-NETDT.
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = T_DATA-NETDT
        T_FROM      = SY-UZEIT
        D_TO        = T_DATA-NETDT_VEND
        T_TO        = SY-UZEIT
        TIME_UNIT   = 'D'
      IMPORTING
        TIME_DIFF   = TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      T_DATA-NETDT_DIFF = TIME_DIFF.
      IF TIME_DIFF > 0.
        T_DATA-ADV_VS_DELAYED = 'A'.
      ELSEIF TIME_DIFF < 0.
        T_DATA-ADV_VS_DELAYED = 'D'.
      ELSE.
        T_DATA-ADV_VS_DELAYED = ''.
      ENDIF.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
  DELETE T_DATA WHERE NETDT_DIFF NOT IN R_NETDT_DIFF.
  """28.1.19
  IF LV_ADV_VS_DELAYED = 'D'.
    DELETE T_DATA WHERE NETDT_DIFF >= 0.
  ELSEIF LV_ADV_VS_DELAYED = 'A'.
    DELETE T_DATA WHERE NETDT_DIFF =< 0.
  ELSE.
  ENDIF.
  """"""""
  "--- Set Descriptions
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
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
*** 20.06.21++
    CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG        = T_DATA-EKORG
*        langu        = sy-langu
      IMPORTING
        PUR_ORG_DESC = T_DATA-EKORG_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
*** 20.06.21++
    CALL FUNCTION '/SKN/F_SW_10_BLART_DESC'
      EXPORTING
        BLART      = T_DATA-BLART
*       LANGU      = SY-LANGU
      IMPORTING
        TYPE_DESC  = T_DATA-BLART_DESC
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
