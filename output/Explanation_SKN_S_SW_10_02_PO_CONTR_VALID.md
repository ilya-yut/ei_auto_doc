# Exception Indicator: PO based on contract with long validity ( SW_10_02_PO_CONT_VAL)

## General Overview

This Exception Indicator monitors contract-related purchasing documents where the agreement validity period (valid-to minus valid-from) exceeds a configured day threshold, focusing on outline-agreement-linked procurement data.

This EI serves as an essential control for procurement and contract governance by:

- Identifying contracts whose validity interval is longer than policy allows
- Linking contract item records to purchasing document headers for review of dates and document types
- Supporting filters by company code, agreement number, item, and purchase order date
- Enriching results with purchasing document type and company code descriptions for business readers
- Helping teams catch long-running agreements before renewal or compliance reviews

Typical use includes contract portfolio reviews, audit sampling, and periodic checks on agreement validity windows. Results are intended for exception workflows rather than full purchasing extracts.

The routine reads contract items with assigned agreement numbers, resolves related purchasing headers, compares validity end and start dates against the threshold, and raises an alert when qualifying records remain.


## Problem Description

Contracts whose validity period spans more days than policy allows can remain in the system without timely review, creating commercial and compliance exposure.

**Procurement and Contract Risks**

- Overlong validity intervals may not match approved contract terms or renewal cycles
- Agreement-linked purchase data can be hard to spot without comparing validity start and end on the header
- Mixed document categories or types can clutter results if filters are not aligned with contract monitoring

**Operational Risks**

- Buyers may focus on document date alone and miss that validity start and end define the true agreement window
- Derived validity ranges from the monitoring window may be built in code but not applied if selections are left empty

**Control and Audit Risks**

- Without repeatable monitoring, evidence weakens that long validity periods were reviewed against policy
- Threshold and reference-date settings that are unclear can produce false positives or missed exceptions

## Suggested Resolution

**Immediate Response**

- Review flagged agreement numbers, validity start and end dates, company codes, and document types
- Confirm with procurement and legal whether the validity span is intended or requires amendment
- Prioritize high-value agreements and vendors with recurring exceptions

**System Assessment**

- Compare exception volume to prior runs using the same day threshold and backward window
- Validate document category and type filters match the contract population under review
- Align **DATE_REF_FLD** and **BACKDAYS** with how the business defines the monitoring window on purchase order date

**Corrective Actions**

- Correct contract or purchasing master data through standard MM processes where validity dates are wrong
- Adjust **PERIOD** and selection filters after root-cause review
- Document outcomes for audit and schedule recurring monitoring for relevant company codes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 2 | BEDAT |  |  |  |  |  |  |
| 3 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 4 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | BSTYP | BSTYP |
| 5 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 6 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 7 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 8 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 9 | EBELP |  |  |  |  |  |  |
| 10 | KDATB | Validity Per. Start | DATS | 8 | 0 | KDATB | DATUM |
| 11 | KDATE | Validity Period End | DATS | 8 | 0 | KDATE | DATUM |
| 12 | KONNR | Outline Agreement | CHAR | 10 | 0 | KONNR | EBELN |
| 13 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 14 | PERIOD | Period validity check |  | 0 | 0 |  |  |
| 15 | SW_DEST |  | 0 | 0 |  |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 15 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BEDAT** (BEDAT)

Purchasing document date used to filter procurement documents by document creation period.

**BSART** (Purchasing Doc. Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BSTYP** (Purch. Doc. Category)

Purchasing document category (PR/PO/contract etc.) used to segment MM document classes.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATE_REF_FLD** (Date reference field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- BEDAT — Purchasing document date used to filter procurement documents by document creation period.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**EBELN** (Purchasing Document)

Purchasing document number (typically PO) used as the primary MM document key.

**EBELP** (EBELP)

Purchasing document item number used for line-level PO analytics.

**KDATB** (Validity Per. Start)

Condition record valid-from date opening the pricing or condition interval.

**KDATE** (Validity Period End)

Condition or agreement validity end date (valid-to) closing pricing master or contract validity.

**KONNR** (Outline Agreement)

Outline agreement number on the contract item; only items with a non-initial agreement number and contract document category are read before headers are matched.

**LANGU** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**PERIOD** (Period validity check)

<mark>Generic period bucket key (month/year) on aggregates when fiscal period fields are rolled up for charts.</mark>

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.


### Parameter Relationships

How parameter combinations work together

**Contract items:** **BSTYP** defaults to contract category in code; **KONNR**, **EBELP**, and **EBELN** narrow contract lines with an assigned agreement number before headers are read.

**Header match:** Purchasing headers are matched where header document number equals the item agreement number, with **BUKRS**, **BSART**, and **BEDAT** filters applied.

**Monitoring window:** When the internal date range is empty, **BACKDAYS** fills it from today backward. **DATE_REF_FLD** controls which field receives that range—in code, only **BEDAT** is implemented, copying the range to purchase order date when **BEDAT** is not explicitly selected.

**Validity span:** **PERIOD** is compared to header validity end minus validity start; items whose header exceeds the threshold are output with **KDATB**, **KDATE**, **BSART**, and descriptions. (Dictionary label for **PERIOD** is generic; for this EI it is the day-count threshold for that span.)

**Derived ranges:** When **KDATE** or **KDATB** selections are empty, the code can build ranges from the monitoring window; validity filters on those fields are commented out in the on-premise header read, so they do not restrict the database selection in the current logic.

**Text language:** **LANGU** is selected for description functions on document type and company code when those values are present.

**Execution path:** **SW_DEST** delegates to the cloud function when set; otherwise the on-premise logic above runs locally.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DATE_REF_FLD** - initial - treated as BEDAT by code
- **LANGU** - initial - treated as E by code
- **BSTYP** - initial - treated as K by code
- **BSART** - initial - treated as blank by code
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Contract validity longer than one year**

**Purpose:** Flag contract-linked headers from the last ninety days on purchase order date where validity end minus start exceeds three hundred sixty-five days.

```
BACKDAYS = 90
DATE_REF_FLD = BEDAT
PERIOD = 365
BSTYP = K
```

**Use Case 2: Company-specific review**

**Purpose:** Limit monitoring to one company code and a defined agreement number.

```
BUKRS = 1000
KONNR = 4600001234
PERIOD = 180
BACKDAYS = 30
```

**Use Case 3: Document type focus**

**Purpose:** Review contract category documents for a specific purchasing document type while keeping the default purchase-order-date reference.

```
BSART = MK
BSTYP = K
PERIOD = 90
DATE_REF_FLD = BEDAT
BACKDAYS = 60
```

**Use Case 4: Line-level agreement item**

**Purpose:** Examine one agreement line together with header validity dates.

```
EBELN = 4500012345
EBELP = 10
PERIOD = 120
BACKDAYS = 14
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_02_PO_CONTR_VALID | AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_02_PO_CONTR_VALID | BATXT | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_02_PO_CONTR_VALID | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_02_PO_CONTR_VALID | BSTYP | Purch. Doc. Category | CHAR(1) | BSTYP |
| /SKN/S_SW_10_02_PO_CONTR_VALID | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_02_PO_CONTR_VALID | BUTXT | Company Name | CHAR(25) | BUTXT |
| /SKN/S_SW_10_02_PO_CONTR_VALID | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_02_PO_CONTR_VALID | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_02_PO_CONTR_VALID | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_02_PO_CONTR_VALID | KDATB | Validity Per. Start | DATS(8) | KDATB |
| /SKN/S_SW_10_02_PO_CONTR_VALID | KDATE | Validity Period End | DATS(8) | KDATE |
| /SKN/S_SW_10_02_PO_CONTR_VALID | KONNR | Outline Agreement | CHAR(10) | KONNR |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_PO_CONTR_VALID.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_PO_CONTR_VALID OPTIONAL
*"----------------------------------------------------------------------
  CONSTANTS: C_BSART TYPE BSART VALUE '',
             C_BSTYP TYPE BSTYP VALUE 'K'.
  TYPES: BEGIN OF TY_EKPO,
             EBELN TYPE EKPO-EBELN,
             EBELP TYPE EKPO-EBELP,
*             bukrs TYPE ekpo-bukrs,
*             werks TYPE ekpo-werks,
             KONNR TYPE EKPO-KONNR,
             BSTYP TYPE EKPO-BSTYP,
           END OF TY_EKPO,
           TT_EKPO TYPE STANDARD TABLE OF TY_EKPO.
  TYPES: BEGIN OF TY_EKKO,
             EBELN TYPE EKKO-EBELN,
             BUKRS TYPE EKKO-BUKRS,
             BSTYP TYPE EKKO-BSTYP,
             BSART TYPE EKKO-BSART,
             AEDAT TYPE EKKO-AEDAT,
             BEDAT TYPE EKKO-BEDAT,
             KDATB TYPE EKKO-KDATB,
             KDATE TYPE EKKO-KDATE,
         END OF TY_EKKO,
         TT_EKKO TYPE STANDARD TABLE OF TY_EKKO.
  DATA_SINGLE: SW_DEST        RFCDEST,
               BACKDAYS       INT4,
               BSART          BSART,
               BSTYP          BSTYP,
               PERIOD         INT4,
               LANGU          LANGU,
               DATE_REF_FLD   NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  DATA_MULTY: EBELN     EBELN,
              EBELP     EBELP,
              BUKRS     BUKRS,
              KONNR     KONNR,
              BEDAT     EBDAT,
              KDATE     KDATE,
              KDATB     KDATB,
              DATUM     SY-DATUM,
              DURATION  /SKN/E_SW_DURATION.
  SELECT_MULTY: EBELN,
                EBELP,
                BUKRS,
                KONNR,
                BEDAT,
                KDATE,
                KDATB,
                DURATION.
* Set default parameter
  LV_BACKDAYS     = 10.
  LV_DATE_REF_FLD = 'BEDAT'.
  LV_LANGU        = 'E'.
  LV_BSTYP        = C_BSTYP.
  LV_BSART        = SPACE.
  SELECT_SINGLE: SW_DEST,
                 BACKDAYS,
                 PERIOD,
                 BSTYP,
                 BSART,
                 LANGU,
                 DATE_REF_FLD.
  CONVERT_MULTY: EBELN ALPHA.
  CONVERT_MULTY: KONNR ALPHA.
  DATA: SY_TABIX LIKE SY-TABIX,
        SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  DATA: LT_EKPO TYPE TT_EKPO,
        LS_EKPO TYPE TY_EKPO,
        LT_EKKO TYPE TT_EKKO,
        LS_EKKO TYPE TY_EKKO,
        LS_DATA LIKE LINE OF T_DATA[].
  DATA: BACKDAYS  TYPE I,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM,
        REF_DATE  TYPE D.
  DATA: TIME_DIFF TYPE INT4 .
  DATA: FLD(60) TYPE C.
  FIELD-SYMBOLS:  TYPE ANY.
  REFRESH T_DATA[].
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_PO_CONT_VALID'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  CASE LV_DATE_REF_FLD.
    WHEN 'BEDAT'.
      IF R_BEDAT[] IS INITIAL AND R_DATUM[] IS NOT INITIAL.
        R_BEDAT[] = R_DATUM[].
      ENDIF.
  ENDCASE.
  IF R_KDATE IS INITIAL AND R_DATUM IS NOT INITIAL.
    LOOP AT R_DATUM INTO RS_DATUM.
      RS_KDATE-SIGN   = 'I' .
      RS_KDATE-OPTION = 'LE' .
      IF RS_DATUM-HIGH IS NOT INITIAL.
        RS_KDATE-LOW    = RS_DATUM-HIGH.
      ELSE.
        RS_KDATE-LOW    = SY-DATUM.
      ENDIF.
      APPEND RS_KDATE TO R_KDATE.
    ENDLOOP.
  ENDIF.
  IF R_KDATB IS INITIAL AND R_DATUM IS NOT INITIAL.
    LOOP AT R_DATUM INTO RS_DATUM.
      RS_KDATB-SIGN   = 'I' .
      RS_KDATB-OPTION = 'GE'.
      RS_KDATB-LOW    = RS_DATUM-LOW.
      APPEND RS_KDATB TO R_KDATB.
    ENDLOOP.
  ENDIF.
  "--- Set Reference Date Field
*  date_from = sy-datum.
*  READ TABLE r_datum INTO rs_datum INDEX 1.
*  IF sy-subrc IS INITIAL.
*    date_from = rs_datum-low.
*    date_to   = rs_datum-high.
*    IF date_to < date_from.
*      date_to = date_from.
*    ENDIF.
*  ENDIF.
*"--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR: IS_ALERT, LT_EKKO, LT_EKPO.
  REFRESH T_DATA.
  IF LV_BSTYP IS NOT INITIAL.
    SELECT EBELN EBELP KONNR BSTYP
      FROM EKPO
      INTO TABLE LT_EKPO
      WHERE KONNR IN R_KONNR[]
      AND   KONNR IS NOT NULL
      AND   BSTYP EQ LV_BSTYP.
  ENDIF.
  CHECK LT_EKPO IS NOT INITIAL.
  SORT LT_EKPO BY KONNR.
  SELECT EBELN BUKRS BSTYP BSART AEDAT BEDAT KDATB KDATE
    FROM EKKO
    INTO TABLE LT_EKKO
    FOR ALL ENTRIES IN LT_EKPO
    WHERE EBELN EQ LT_EKPO-KONNR
    AND   BUKRS IN R_BUKRS
    AND   BSART EQ LV_BSART
    AND   BEDAT IN R_BEDAT[].
*    AND   kdate IN r_kdate[]
*    AND   kdatb IN r_kdatb[].
  SORT LT_EKKO BY EBELN.
  LOOP AT LT_EKPO INTO LS_EKPO.
    CLEAR: LS_DATA, LS_EKKO.
    READ TABLE LT_EKKO INTO LS_EKKO WITH KEY EBELN = LS_EKPO-KONNR
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF ( LS_EKKO-KDATE - LS_EKKO-KDATB ) > LV_PERIOD.
        LS_DATA-BSART = LS_EKKO-BSART.
        LS_DATA-EBELN = LS_EKKO-EBELN.
        LS_DATA-KDATB = LS_EKKO-KDATB.
        LS_DATA-KDATE = LS_EKKO-KDATE.
        LS_DATA-BSTYP = LS_EKPO-BSTYP.
        IF LS_DATA-BSART IS NOT INITIAL.
          CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
            EXPORTING
              BSART      = LS_DATA-BSART
*             LANGU      = SY-LANGU
              BSTYP      = LS_DATA-BSTYP
            IMPORTING
              TYPE_DESC  = LS_DATA-BATXT
            EXCEPTIONS
              WRONG_CODE = 1
              OTHERS     = 2.
        ENDIF.
        IF LS_DATA-BUKRS IS NOT INITIAL.
          CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
            EXPORTING
              BUKRS          = LS_DATA-BUKRS
            IMPORTING
              COMP_CODE_DESC = LS_DATA-BUTXT
            EXCEPTIONS
              WRONG_CODE     = 1
              OTHERS         = 2.
        ENDIF.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
        ENDIF.
        APPEND LS_DATA TO T_DATA[].
      ENDIF.
    ENDIF.
  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
