# Exception Indicator: PO based on agreement with long validity ( SW_10_02_PO_AGR_VAL)

## General Overview

This Exception Indicator monitors purchase orders whose last change date falls more than a configured number of days after the purchase order date, highlighting agreements that may have been maintained with unusually long validity relative to their document date.

This EI serves as an essential control for procurement and compliance by:

- Detecting purchase orders where maintenance activity occurred long after the original document date
- Supporting review of outline agreements and long-running purchase documents that may need re-approval
- Enabling focused lists by purchasing document, document date window, and day-gap threshold
- Providing visibility for audit and contract-governance teams without exporting the full purchase order population
- Helping organizations catch data-entry or process patterns where header dates and change dates are far apart

Typical use includes agreement reviews, periodic procurement controls, and sampling before contract renewals. Results are intended for exception workflows rather than operational purchasing extracts.

The routine reads purchase order headers, applies document-date selection, compares change date to order date against the configured day threshold, and raises an alert when qualifying documents remain.


## Problem Description

Purchase orders whose change date is far later than the purchase order date can indicate outdated agreements, delayed maintenance, or controls that did not catch long validity periods at the time of creation.

**Procurement and Contract Risks**

- Agreements may remain effective on paper while header dates no longer reflect when commercial terms were set
- Large gaps between order date and last change can obscure when terms were actually updated
- High-volume environments can miss individual documents without automated comparison of the two dates

**Operational Risks**

- Buyers and approvers may rely on document date alone and overlook that the record was changed much later
- Overly wide document-date windows can flood reviewers; overly narrow windows can miss older long-validity cases

**Control and Audit Risks**

- Weak monitoring reduces evidence that long validity periods were reviewed against policy
- Thresholds that are not documented or tested can produce false positives or missed exceptions

## Suggested Resolution

**Immediate Response**

- Review flagged purchasing documents, order dates, change dates, and the calculated day gap
- Confirm with procurement and contract owners whether the validity period is intended or requires re-creation or amendment
- Prioritize high-value agreements and vendors with recurring exceptions

**System Assessment**

- Compare exception counts to prior runs using the same day-gap threshold and document-date window
- Validate whether explicit purchase order date selections should replace the default backward window
- Tune the day-gap parameter so the queue matches organizational policy on acceptable order-date versus change-date separation

**Corrective Actions**

- Correct or re-release purchase documents through standard MM processes where terms are wrong
- Adjust monitoring parameters after root-cause review
- Document outcomes for audit and schedule recurring monitoring for relevant document types


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 2 | BEDAT | Purchase Order Date | DATS | 8 | 0 | BEDAT | DATUM |
| 3 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 4 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 5 | PERIOD | INT4 | 10 | 0 |  |  |  |
| 6 | SW_DEST |  | 0 | 0 |  |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 6 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

**BEDAT** (Purchase Order Date)

Purchasing document date used to filter procurement documents by document creation period.

**DATUM** (DATS)

Optional reference date for the run; when document-date selections are built from the monitoring window, this value participates in the same date-range logic applied before purchase orders are read.

**EBELN** (Purchasing Document)

Purchasing document number (typically PO) used as the primary MM document key.

**PERIOD** (INT4)

<mark>Generic period bucket key (month/year) on aggregates when fiscal period fields are rolled up for charts.</mark>

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.


### Parameter Relationships

How parameter combinations work together

**Document scope:** **EBELN** limits which purchasing documents are read from the purchase order header table.

**Order-date window:** When no explicit **BEDAT** selection is supplied, **BACKDAYS** builds a lower bound on purchase order date from today backward and applies it to **BEDAT**. Explicit **BEDAT** selections override that fallback.

**Validity gap:** In code, the difference between change date and purchase order date is compared to **PERIOD**; only documents where that gap exceeds the configured value are returned in **AEDAT**, **BEDAT**, and **EBELN** output fields. (Dictionary label for **PERIOD** is generic; operational meaning for this EI is the day-gap threshold.)

**Reference date:** **DATUM** supports date-range handling together with the monitoring window when selections are derived from run parameters rather than fixed from/to dates.

**Execution path:** **SW_DEST** delegates to the cloud function when set; otherwise the on-premise comparison logic runs locally.


### Default Values

- **BACKDAYS** - initial - treated as 0 by code

### Practical Example of Parameter Configuration

**Use Case 1: Recent orders with thirty-day gap**

**Purpose:** Flag purchase orders created in the last ninety days where the change date is more than thirty days after the order date.

```
BACKDAYS = 90
PERIOD = 30
```

**Use Case 2: Specific purchasing document**

**Purpose:** Review one agreement’s order date and change date against the configured gap rule.

```
EBELN = 4500012345
PERIOD = 60
```

**Use Case 3: Fixed order-date range**

**Purpose:** Analyze documents with order dates in a defined interval without using the backward-day fallback.

```
BEDAT = 20250101
PERIOD = 45
BACKDAYS = 30
```

**Use Case 4: Tighter monitoring window**

**Purpose:** Focus on orders from the last two weeks with a fifteen-day change-versus-order-date threshold.

```
BACKDAYS = 14
PERIOD = 15
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_02_PO_AGREE_VALID | AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_02_PO_AGREE_VALID | BEDAT | Purchase Order Date | DATS(8) | BEDAT |
| /SKN/S_SW_10_02_PO_AGREE_VALID | EBELN | Purchasing Document | CHAR(10) | EBELN |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_PO_AGREE_VALID.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_PO_AGREE_VALID OPTIONAL
*"----------------------------------------------------------------------
  CONSTANTS: C_BSART TYPE BSART VALUE ''.
  TYPES: BEGIN OF TY_EKKO,
           EBELN TYPE EKKO-EBELN,
           AEDAT TYPE EKKO-AEDAT,
           BEDAT TYPE EKKO-BEDAT,
         END OF TY_EKKO,
         TT_EKKO TYPE STANDARD TABLE OF TY_EKKO.
  DATA_SINGLE: SW_DEST    RFCDEST,
               BACKDAYS   INT4,
               PERIOD     INT4    .
  DATA_MULTY: EBELN    EBELN,
              BEDAT    EBDAT,
              DATUM    SY-DATUM.
  SELECT_MULTY: EBELN,
                BEDAT.
  SELECT_SINGLE: SW_DEST,
                 BACKDAYS.
  CONVERT_MULTY: EBELN ALPHA.
  DATA: SY_TABIX LIKE SY-TABIX,
        SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  DATA: LT_EKKO TYPE TT_EKKO,
        LS_EKKO TYPE TY_EKKO,
        LS_DATA LIKE LINE OF T_DATA[].
  DATA: BACKDAYS  TYPE I,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM,
        REF_DATE  TYPE D.
  DATA: TIME_DIFF TYPE  INT4 .
  DATA: FLD(60) TYPE C.
  FIELD-SYMBOLS:  TYPE ANY.
  REFRESH T_DATA[].
*  IF NOT lv_forwdays  IS INITIAL.
*    lv_backdays = lv_forwdays * ( -1 ).
*  ENDIF.
  IF R_BEDAT[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
    R_BEDAT = R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  DATE_FROM = SY-DATUM.
  READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_DATUM-LOW.
    DATE_TO   = RS_DATUM-HIGH.
    IF DATE_TO < DATE_FROM.
      DATE_TO = DATE_FROM.
    ENDIF.
  ENDIF.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_PO_AGRE_VALID'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR: IS_ALERT, LT_EKKO.
  REFRESH T_DATA.
  SELECT EBELN BSART BEDAT KDATB KDATE
    FROM EKKO
    INTO TABLE LT_EKKO
    WHERE EBELN IN R_EBELN
    AND   BEDAT IN R_BEDAT.
  LOOP AT LT_EKKO INTO LS_EKKO.
    CLEAR LS_DATA.
    IF ( LS_EKKO-AEDAT - LS_EKKO-BEDAT ) > LV_PERIOD.
      LS_DATA-EBELN = LS_EKKO-EBELN.
      LS_DATA-AEDAT = LS_EKKO-AEDAT.
      LS_DATA-BEDAT = LS_EKKO-BEDAT.
      APPEND LS_DATA TO T_DATA[].
    ENDIF.
  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
