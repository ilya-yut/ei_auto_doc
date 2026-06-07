# Exception Indicator: Alternative payee ( SW_10_06_ALTR_PAYEE)

## General Overview

This Exception Indicator identifies vendors in a company code that have an alternative payee assigned at the company-code level, at the general vendor level, or both.

This EI serves as an essential control for accounts payable and vendor master governance by:
- Surfacing vendor–company code combinations where payment may be directed to a party other than the vendor on the invoice
- Highlighting central and company-specific alternative payee numbers together with descriptive names for review
- Supporting segregation-of-duties and payment-fraud prevention through visibility into payee routing on vendor master data
- Enabling filtered reviews by vendor, company code, and creation date of the company-code record
- Complementing periodic vendor master audits with a repeatable exception list for alternative payee maintenance

Typical use includes reviews after vendor onboarding, mergers, or mass updates to payment recipients, and before payment runs in selected company codes. Results are intended for exception workflows rather than full vendor master extracts.

The routine reads vendor company-code records joined to general vendor and company names, retains rows where either alternative payee field is populated, and enriches output with names for the alternative payee vendors.


## Problem Description

Failure to monitor alternative payee assignments on vendor master data creates multiple risks across accounts payable, treasury, and compliance.

**Payment and Fraud Risks**
- Invoices may be paid to an unintended recipient when alternative payee numbers are set without timely review
- New or changed alternative payees can bypass standard vendor validation if not visible to AP and master data teams
- Concentrations of alternative payees in a company code may indicate systemic setup errors or policy gaps

**Master Data and Control Risks**
- Company-code-level and general-level alternative payees can diverge, causing confusion about the effective payee on payment
- Vendor records created or changed within a lookback window may reach production with payee fields populated before approval
- Lack of periodic exception reporting weakens evidence for internal control over vendor payment routing

**Audit and Compliance Risks**
- Auditors expect traceability of who may receive payment versus who is invoiced; unreviewed alternative payees undermine that evidence
- Segregation-of-duties reviews are harder when payee changes are not surfaced in a dedicated monitoring population

## Suggested Resolution

**Immediate Response**
- Review each flagged vendor, company code, alternative payee numbers, and displayed payee names
- Confirm with procurement or AP whether the alternative payee is valid, documented, and approved for the company code
- Prioritize high-value vendors and company codes with active payment volume

**System Assessment**
- Compare current exception volume to prior runs using the same company code and date filters
- Look for clusters by company code or vendor ranges that may trace to a single migration or project
- Verify whether company-code alternative payee, general alternative payee, or both drive most alerts

**Corrective Actions**
- Correct invalid alternative payee assignments through standard vendor maintenance with required approvals
- Update monitoring scope after root cause so the queue stays actionable for master data and AP teams
- Document review outcomes for audit trail and schedule recurring runs for company codes in scope
- Route repeat setup defects from interfaces or conversions into change management when payee fields are systematically wrong


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | DATUM | Reference Date | DATS | 8 | 0 | DATUM | DATUM |
| 4 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 5 | FORWDAYS | Forward Days | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |
| 6 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 6 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATUM** (Reference Date)

When populated, keeps the extract focused so reference date (DATUM) aligns with the intended triage slice.

**ERDAT** (Created On)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**FORWDAYS** (Forward Days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.


### Parameter Relationships

How parameter combinations work together

**Company and vendor scope:** **BUKRS** limits results to selected company codes; **LIFNR** narrows to specific vendors when populated. Both apply to the company-code vendor records read by the routine.

**Creation date filtering:** **ERDAT** restricts vendor company-code records by creation date. When **ERDAT** is left empty and **BACKDAYS** is not zero, the routine builds a creation-date lower bound from the current date minus **BACKDAYS**.

**Forward window:** When **FORWDAYS** is set, the routine derives an effective backward window from **FORWDAYS** (converted to a negative backdays value) before applying the automatic creation-date filter described above.

**Reference date:** **DATUM** is supplied by the online monitor as the run reference date and participates in selection together with the other filters.

**Combined effect:** Company code, vendor, creation date (explicit or derived from **BACKDAYS** / **FORWDAYS**), and reference date work together; a row appears when a matching vendor company-code record has a non-empty company-code or general alternative payee.


### Default Values

- **BACKDAYS** - initial - treated as 0 by code

### Practical Example of Parameter Configuration

**Use Case 1: New vendors with alternative payee in one company code**

**Purpose:** List vendors created in the last thirty days in company code 1000 that have any alternative payee assigned.
```
BUKRS = 1000
BACKDAYS = 30
```

**Use Case 2: Specific vendor review**

**Purpose:** Check whether a known vendor has alternative payee numbers in two company codes.
```
LIFNR = 0000100001
BUKRS = 1000
BUKRS = 2000
```

**Use Case 3: Forward-dated creation window**

**Purpose:** Review vendors whose company-code records were created in a short forward-looking window relative to the run date when **FORWDAYS** is used.
```
BUKRS = US01
FORWDAYS = 7
LIFNR = 0000200000 - 0000299999
```

**Use Case 4: Explicit creation date range**

**Purpose:** Target vendors created on or after a fixed date without using the backdays-derived window.
```
BUKRS = 2000
ERDAT = 20250101
LIFNR = V100
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_ALTR_PAYEE | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_ALTR_PAYEE | BUTXT | Company Name | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_ALTR_PAYEE | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_ALTR_PAYEE | LNRZA | Alternative payee | CHAR(10) | LNRZA |
| /SKN/S_SW_10_06_ALTR_PAYEE | LNRZB | Alternative payee | CHAR(10) | LNRZB |
| /SKN/S_SW_10_06_ALTR_PAYEE | NAME1 | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_06_ALTR_PAYEE | NAME_LNRZA | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_ALTR_PAYEE | NAME_LNRZB | Name | CHAR(35) | NAME1_GP |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_ALTR_PAYEE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_ALTR_PAYEE OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_DATA,
             LIFNR TYPE LFB1-LIFNR,
             BUKRS TYPE LFB1-BUKRS,
             NAME1 TYPE LFA1-NAME1,
             ERDAT TYPE LFB1-ERDAT,
             LNRZB TYPE LFB1-LNRZB,
             LNRZA TYPE LFA1-LNRZA,
             BUTXT TYPE T001-BUTXT,
           END OF TY_DATA,
           TT_DATA TYPE STANDARD TABLE OF TY_DATA.
  TYPES: BEGIN OF TY_LFA1,
           LIFNR TYPE LFA1-LIFNR,
           NAME1 TYPE LFA1-NAME1,
         END OF TY_LFA1,
         TT_LFA1 TYPE STANDARD TABLE OF TY_LFA1.
  DATA_SINGLE: SW_DEST       RFCDEST,
               BACKDAYS      INT4,
               FORWDAYS      INT4.
  DATA_MULTY: LIFNR LIFNR,
              BUKRS BUKRS,
              ERDAT ERDAT_RF,
              DATUM SY-DATUM.
  SELECT_MULTY: LIFNR,
                BUKRS,
                ERDAT,
                DATUM.
  SELECT_SINGLE: SW_DEST.
  CONVERT_MULTY: LIFNR ALPHA,
                 BUKRS ALPHA.
  DATA: LT_DATA  TYPE TT_DATA,
        LT_LFA1  TYPE TT_LFA1,
        LS_LFA1  TYPE TY_LFA1,
        LS_DATA  TYPE TY_DATA,
        LS_DATA2 LIKE LINE OF T_DATA[].
  DATA: BACKDAYS  TYPE I,
        FORWDAYS  TYPE I,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM,
        REF_DATE  TYPE D.
  DATA: TIME_DIFF TYPE  INT4 .
  DATA: FLD(60) TYPE C.
  FIELD-SYMBOLS:       TYPE ANY,
                 <FS_DATA> LIKE LINE OF T_DATA[].
  IF NOT LV_FORWDAYS  IS INITIAL.
    LV_BACKDAYS = LV_FORWDAYS * ( -1 ).
  ENDIF.
  IF R_ERDAT[] IS INITIAL AND LV_BACKDAYS <> 0.
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
    R_ERDAT[] = R_DATUM[].
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
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_ALTR_PAYEE'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
  " time filling
*  set_sy_time lv_manage_in_utc sy_datlo sy_timlo.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT LFB1~LIFNR LFB1~BUKRS LFB1~LNRZB
         LFA1~NAME1 LFA1~LNRZA
         T001~BUTXT
    FROM LFB1 INNER JOIN LFA1 ON LFB1~LIFNR EQ LFA1~LIFNR
              INNER JOIN T001 ON LFB1~BUKRS EQ T001~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE LFB1~LIFNR IN R_LIFNR
    AND   LFB1~BUKRS IN R_BUKRS
    AND   LFB1~ERDAT IN R_ERDAT
    AND ( LFB1~LNRZB <> SPACE OR LFA1~LNRZA <> SPACE ).
*  IF t_data IS NOT INITIAL.
*    SELECT lifnr name1
*      FROM lfa1
*      INTO TABLE lt_lfa1
*      FOR ALL ENTRIES IN t_data
*      WHERE lifnr EQ t_data-lnrza
*      OR    lifnr EQ t_data-lnrzb.
*  ENDIF.
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
*    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = <fs_data>-lnrza.
*    IF sy-subrc = 0.
*    <fs_data>-name_lnrza = ls_lfa1-name1.
*    ENDIF.
*
*    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = <fs_data>-lnrzb.
*    IF sy-subrc = 0.
*      <fs_data>-name_lnrzb = ls_lfa1-name1.
*    ENDIF.
    IF <FS_DATA>-LNRZA IS NOT INITIAL.
* Get Vendor 1 description
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = <FS_DATA>-LNRZA
        IMPORTING
          VENDOR_DESC  = <FS_DATA>-NAME_LNRZA
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
* Get Vendor 2 description
    IF <FS_DATA>-LNRZB IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = <FS_DATA>-LNRZB
        IMPORTING
          VENDOR_DESC  = <FS_DATA>-NAME_LNRZB
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
