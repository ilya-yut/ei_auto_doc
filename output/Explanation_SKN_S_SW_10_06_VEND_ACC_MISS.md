# Exception Indicator: No bank account record ( SW_10_06_VEN_ACC_MIS)

## General Overview

This Exception Indicator identifies vendor company-code records that have no bank account maintained or whose bank validity interval does not cover the evaluation date.

This EI serves as an essential control for accounts payable, treasury, and vendor master governance by:

- Surfacing vendors in selected company codes that lack any bank detail in vendor bank master
- Flagging vendors whose bank records exist but are outside the current validity window
- Supporting payment-readiness reviews before runs that depend on complete vendor bank data
- Enabling scoped monitoring by vendor, company code, bank country, and vendor company-code creation date
- Supporting both on-premise and cloud execution through optional destination routing

Typical use includes reviews before payment proposals, after vendor onboarding, or when validating bank master completeness by company code. Results are intended for exception workflows rather than full vendor bank extracts.

The routine reads vendor company-code data joined to vendor general data and company names, compares against vendor bank records, and returns vendors missing banks or with bank validity that does not include the current date.


## Problem Description

Failure to monitor vendors without usable bank accounts creates multiple risks across payment execution, compliance, and master data quality.

**Payment and Treasury Risks**

- Payment programs may fail or stall when required vendor bank details are missing for a company code
- Expired or not-yet-valid bank records can cause rejections even when historical bank lines still exist in master data
- New vendor company-code records may reach production before bank data is complete

**Control and Compliance Risks**

- Lack of periodic exception reporting weakens evidence that payee bank data was reviewed before disbursement
- Concentrations of missing banks by company code or vendor range are harder to detect without automated comparison to bank master

**Master Data and Operations Risks**

- Incomplete vendor bank maintenance is often discovered only at payment time rather than during master data governance
- Cloud and on-premise landscapes need the same visibility when remote execution is used

## Suggested Resolution

**Immediate Response**

- Review each flagged vendor, company code, bank country (when present), validity dates, and vendor name shown in the exception
- Confirm with AP or master data whether a bank record must be created, reactivated, or corrected
- Prioritize vendors with upcoming payment activity in the affected company code

**System Assessment**

- Compare current exception volume to prior runs using the same company code and creation-date filters
- Look for clusters by vendor account group or onboarding source that may indicate a process gap
- Validate that creation-date and bank country filters match the intended monitoring population

**Corrective Actions**

- Maintain missing bank details through standard vendor bank maintenance with required approvals
- Correct validity dates on existing bank records when the account should be active
- Document review outcomes for audit trail and schedule recurring runs for company codes in scope
- Route repeat interface or conversion defects into change management when bank gaps are systematic


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 2 | BANKS | Bank Country | CHAR | 3 | 0 | BANKS | LAND1 |
| 3 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 4 | DATUM | Reference Date | DATS | 8 | 0 | DATUM | DATUM |
| 5 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 6 | FORWDAYS | Forward Days | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |
| 7 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 8 | SW_DEST | RFC Destination | CHAR | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 8 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERDAT

**BANKS** (Bank Country)

Bank country key governing bank-key validation rules and payment formats for the account.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATUM** (Reference Date)

Reference date supplied by the online monitor; used with explicit date selection when deriving the effective date range for vendor company-code filtering.

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**FORWDAYS** (Forward Days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**SW_DEST** (RFC Destination)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.


### Parameter Relationships

How parameter combinations work together

**Vendor and company scope:** **LIFNR** and **BUKRS** limit which vendor company-code records are read from vendor company-code master. **BANKS** restricts which bank countries are considered when bank lines are loaded.

**Creation-date window:** **ERDAT** supplies explicit creation-date bounds on vendor company-code records. When **ERDAT** is empty and **BACKDAYS** is not zero, the routine builds a lower bound from the current date minus **BACKDAYS** and applies it to **ERDAT** selection.

**Reference date:** **DATUM** is supplied by the online monitor as the run reference date and is used when deriving the effective date range from explicit date selection.

**Execution path:** **SW_DEST** selects the cloud destination; when populated, processing is delegated to the cloud function module with the same parameter set. When empty, the on-premise selection and bank comparison logic runs locally.

**Bank validity evaluation:** After vendor company-code records are selected, the routine loads matching bank records and flags vendors with no bank line or with bank validity (**KOVON** / **KOBIS** on output) that does not include the evaluation date.


### Default Values

- **BACKDAYS** - initial - treated as unconstrained by code
- **FORWDAYS** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Recently created vendors missing banks**

**Purpose:** Find vendor company-code records created in the last fourteen days in company code 1000 that have no active bank account.

```
BUKRS = 1000
BACKDAYS = 14
```

**Use Case 2: Specific vendor review**

**Purpose:** Check one vendor across selected company codes for missing or invalid bank data.

```
LIFNR = 0000100001
BUKRS = 1000
BUKRS = 2000
```

**Use Case 3: Bank country scope**

**Purpose:** Monitor vendors in company code US01 where bank master is expected for US bank country only.

```
BUKRS = US01
BANKS = US
BACKDAYS = 30
```

**Use Case 4: Explicit creation-date window**

**Purpose:** Review vendors whose company-code record was created on or after a fixed date.

```
BUKRS = DE01
ERDAT = 20250101
BACKDAYS = 0
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_VEND_ACC_MISS | BANKS | Bank Country | CHAR(3) | BANKS |
| /SKN/S_SW_10_06_VEND_ACC_MISS | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_VEND_ACC_MISS | BUTXT | Company Name | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_VEND_ACC_MISS | ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_06_VEND_ACC_MISS | KOBIS | Eff.to | DATS(8) | KOBIS |
| /SKN/S_SW_10_06_VEND_ACC_MISS | KOVON | Valid from | DATS(8) | KOVON |
| /SKN/S_SW_10_06_VEND_ACC_MISS | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_VEND_ACC_MISS | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_VEND_ACC_MISS | NAME1 | Name 1 | CHAR(30) | NAME1 |

## ABAP Code

```abap
  FUNCTION /SKN/F_SW_10_06_VEND_ACC_MISS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_VEND_ACC_MISS OPTIONAL
*"----------------------------------------------------------------------
    TYPES: BEGIN OF TY_LFB1,
      LIFNR TYPE LFB1-LIFNR,
      BUKRS TYPE LFB1-BUKRS,
      NAME1 TYPE LFA1-NAME1,
      ERDAT TYPE LFB1-ERDAT,
      BUTXT TYPE T001-BUTXT,
    END OF TY_LFB1,
    TT_LFB1 TYPE STANDARD TABLE OF TY_LFB1.
    TYPES: BEGIN OF TY_LFBK,
      LIFNR TYPE LFBK-LIFNR,
      BANKS TYPE LFBK-BANKS,
      KOVON TYPE LFBK-KOVON,
      KOBIS TYPE LFBK-KOBIS,
    END OF TY_LFBK,
    TT_LFBK TYPE STANDARD TABLE OF TY_LFBK.
    DATA_SINGLE: SW_DEST   RFCDEST,
                 BACKDAYS  INT4,
                 FORWDAYS  INT4.
    DATA_MULTY: LIFNR LIFNR,
                BUKRS BUKRS,
                ERDAT ERDAT_RF,
                BANKS BANKS,
    DATUM SY-DATUM.
    SELECT_MULTY: LIFNR,
                  BUKRS,
                  ERDAT,
                  BANKS,
    DATUM.
    SELECT_SINGLE: SW_DEST.
    CONVERT_MULTY: LIFNR ALPHA,
    BUKRS ALPHA.
    DATA: LT_LFB1  TYPE TT_LFB1,
          LT_LFBK  TYPE TT_LFBK,
          LS_LFBK  TYPE TY_LFBK,
          LS_LFB1  TYPE TY_LFB1,
          LS_DATA  LIKE LINE OF T_DATA[].
    DATA: BACKDAYS  TYPE I,
          FORWDAYS  TYPE I,
          DATE_FROM LIKE SY-DATUM,
          DATE_TO   LIKE SY-DATUM,
          REF_DATE  TYPE D.
    DATA: TIME_DIFF TYPE  INT4 .
    DATA: FLD(60) TYPE C.
    FIELD-SYMBOLS:  TYPE ANY.
    IF R_ERDAT[] IS INITIAL AND LV_BACKDAYS <> 0.
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
    R_ERDAT[] = R_DATUM[].
    ENDIF.
*  --- set reference date field
    DATE_FROM = SY-DATUM.
    READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
    IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_DATUM-LOW.
    DATE_TO   = RS_DATUM-HIGH.
    IF DATE_TO < DATE_FROM.
    DATE_TO = DATE_FROM.
    ENDIF.
    ENDIF.
    IF LV_SW_DEST IS NOT INITIAL.
      CALL FUNCTION '/SKN/FC_SW_10_06_VEND_ACC_MISS'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
    ENDIF.
    CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
*--- Retrieve data
    CLEAR IS_ALERT .
    REFRESH T_DATA.
    SELECT LFB1~LIFNR LFB1~BUKRS LFB1~LNRZB LFB1~ERDAT
           LFA1~NAME1
           T001~BUTXT T001~LAND1
      FROM LFB1 INNER JOIN LFA1 ON LFB1~LIFNR EQ LFA1~LIFNR
      INNER JOIN T001 ON LFB1~BUKRS EQ T001~BUKRS
      INTO CORRESPONDING FIELDS OF TABLE LT_LFB1
      WHERE LFB1~LIFNR IN R_LIFNR
      AND   LFB1~BUKRS IN R_BUKRS
      AND   LFB1~ERDAT IN R_ERDAT.
    IF LT_LFB1 IS NOT INITIAL.
    SELECT LIFNR BANKS KOVON KOBIS
      FROM LFBK
      INTO TABLE LT_LFBK
      FOR ALL ENTRIES IN LT_LFB1
      WHERE LIFNR EQ LT_LFB1-LIFNR
      AND   BANKS IN R_BANKS.
    ENDIF.
    LOOP AT LT_LFB1 INTO LS_LFB1.
      CLEAR: LS_LFBK, LS_DATA.
      READ TABLE LT_LFBK INTO LS_LFBK WITH KEY LIFNR = LS_LFB1-LIFNR
      BINARY SEARCH.
      IF SY-SUBRC <> 0.
      MOVE-CORRESPONDING LS_LFB1 TO LS_DATA.
      APPEND LS_DATA TO T_DATA.
*   Check validity date
      ELSE.
        IF NOT ( LS_LFBK-KOVON <= SY-DATUM AND LS_LFBK-KOBIS >= SY-DATUM ).
          MOVE-CORRESPONDING LS_LFB1 TO LS_DATA.
          MOVE-CORRESPONDING LS_LFBK TO LS_DATA.
          APPEND LS_DATA TO T_DATA.
        ENDIF.
      ENDIF.
    ENDLOOP.
    READ TABLE T_DATA INDEX 1.
    CHECK NOT SY-TFILL  IS INITIAL .
    IS_ALERT = 'X' .
ENDFUNCTION.
```
