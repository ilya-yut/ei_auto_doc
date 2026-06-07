# Exception Indicator: Company code country differs bank account country ( SW_10_06_BANK_COMPAR)

## General Overview

This Exception Indicator identifies vendor bank accounts whose bank country differs from the country of the company code in which the vendor is maintained.

This EI serves as an essential control for accounts payable, treasury, and vendor master governance by:
- Surfacing bank records where the bank country key does not match the company code country
- Supporting payment and compliance reviews before funds are sent to accounts in an unexpected jurisdiction
- Enabling scoped monitoring by vendor, company code, bank country, and creation date of the vendor company-code record
- Applying bank-detail validity dates so reviews can align with active or selected validity windows
- Complementing vendor master audits with a repeatable exception list for cross-border or mis-keyed bank countries

Typical use includes reviews before payment runs, after vendor bank uploads, or when onboarding vendors in multinational company codes. Results are intended for exception workflows rather than full bank master extracts.

The routine reads vendor company-code and bank data joined to company master country, retains rows where bank country and company code country differ, and returns vendor, bank, and country attributes for each exception.


## Problem Description

Failure to monitor vendor bank accounts whose country differs from the company code country creates multiple risks across payment execution, compliance, and master data quality.

**Payment and Treasury Risks**
- Payments may be routed to a bank in a country that does not align with the legal entity’s country, causing rejections, delays, or manual repair
- Cross-border bank setups without review can conflict with treasury policy or banking mandates for the company code
- New or changed bank records may reach production before AP validates country alignment

**Compliance and Control Risks**
- Country mismatches can complicate sanctions screening, tax reporting, and audit evidence for payee location
- Unfiltered validity windows may include obsolete bank lines or miss the intended active population
- Lack of periodic exception reporting weakens internal control over vendor bank master changes

**Master Data and Operations Risks**
- Data entry errors on bank country keys are harder to detect without a company-code-scoped comparison
- Concentrations by vendor or company code are less visible when reviews rely on ad hoc table browsing

## Suggested Resolution

**Immediate Response**
- Review each flagged vendor, company code, bank country, company country, and validity dates shown in the exception
- Confirm with treasury or AP whether the bank country is intentional for that company code or requires correction
- Prioritize vendors with upcoming payment runs or high spend in the affected company code

**System Assessment**
- Compare current exception volume to prior runs using the same company code and date filters
- Look for clusters by bank country or vendor range that may trace to a migration or mass upload
- Revisit **KOVON** and **KOBIS** settings when results include banks outside the intended validity window

**Corrective Actions**
- Correct erroneous bank country or company assignments through standard vendor bank maintenance with required approvals
- Update monitoring scope after root cause so the queue stays actionable for master data and AP teams
- Document review outcomes for audit trail and schedule recurring runs for company codes in scope
- Route repeat interface or conversion defects into change management when bank countries are systematically wrong


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 2 | BANKS | Bank Country | CHAR | 3 | 0 | BANKS | LAND1 |
| 3 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 4 | DATUM | Reference Date | DATS | 8 | 0 | DATUM | DATUM |
| 5 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 6 | FORWDAYS | Forward Days | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |
| 7 | KOBIS | Valid To | DATS | 8 | 0 | KOBIS | DATUM |
| 8 | KOVON | Valid From | DATS | 8 | 0 | KOVON | DATUM |
| 9 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 10 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 10 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**BANKS** (Bank Country)

Bank country key governing bank-key validation rules and payment formats for the account.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATUM** (Reference Date)

When left open per framework rules, DATUM does not restrict reference date; when set, only matching rows remain.

**ERDAT** (Created On)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**FORWDAYS** (Forward Days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

**KOBIS** (Valid To)

Backorder or delivery-relevance indicator on schedule lines showing procurement or delivery tie status.

**KOVON** (Valid From)

Valid-from date on condition or agreement records opening the interval where pricing or terms apply.

**LAND1** (Country Key)

Country key used for legal/geographic segmentation of business partners or plants.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.


### Parameter Relationships

How parameter combinations work together

**Company and vendor scope:** **BUKRS** limits vendor company-code records; **LIFNR** narrows to specific vendors when populated. **LAND1** can further restrict the company code country context used in selection.

**Bank and creation filters:** **BANKS** limits which bank countries are in scope; **ERDAT** filters vendor company-code records by creation date. When **ERDAT** is empty and **BACKDAYS** is not zero, the routine derives a creation-date lower bound from the current date minus **BACKDAYS**.

**Forward window:** When **FORWDAYS** is set, the routine converts it to an effective backward window before applying the automatic creation-date filter described above.

**Bank validity window:** **KOVON** and **KOBIS** bound bank-detail validity. When left initial, the routine sets them from the low and high bounds of the reference date range read from the date selection.

**Reference date:** **DATUM** is supplied by the online monitor as the run reference date and works with the date and validity parameters above.

**Combined effect:** Company code, vendor, bank country, creation date (explicit or from **BACKDAYS** / **FORWDAYS**), validity dates, and reference date apply together; a row appears when an in-scope bank record’s country differs from the company code country.


### Default Values

- **BACKDAYS** - initial - treated as 0 by code
- **KOVON** - initial - treated as the reference date range low bound by code
- **KOBIS** - initial - treated as the reference date range high bound by code

### Practical Example of Parameter Configuration

**Use Case 1: Recent vendor bank mismatches in one company code**

**Purpose:** Find vendors created in the last fourteen days in company code 1000 whose bank country differs from that company code’s country.
```
BUKRS = 1000
BACKDAYS = 14
```

**Use Case 2: Specific bank country review**

**Purpose:** Monitor US bank countries for selected vendors when the company code country is not US.
```
BUKRS = 2000
BANKS = US
LIFNR = 0000100001
LIFNR = 0000100002
```

**Use Case 3: Explicit validity window**

**Purpose:** Review mismatches for banks valid within a fixed calendar period.
```
BUKRS = US01
KOVON = 20250101
KOBIS = 20251231
BACKDAYS = 30
```

**Use Case 4: Company country filter with creation date**

**Purpose:** Limit to German company codes and vendors created on or after a fixed date.
```
LAND1 = DE
BUKRS = DE01
ERDAT = 20240601
BANKS = CH
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_BANK_CODE_COMP | BANKL | Bank Key | CHAR(15) | BANKK |
| /SKN/S_SW_10_06_BANK_CODE_COMP | BANKN | Bank Account | CHAR(18) | BANKN |
| /SKN/S_SW_10_06_BANK_CODE_COMP | BANKS | Bank Country | CHAR(3) | BANKS |
| /SKN/S_SW_10_06_BANK_CODE_COMP | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_BANK_CODE_COMP | ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_06_BANK_CODE_COMP | KOBIS | Eff.to | DATS(8) | KOBIS |
| /SKN/S_SW_10_06_BANK_CODE_COMP | KOVON | Valid from | DATS(8) | KOVON |
| /SKN/S_SW_10_06_BANK_CODE_COMP | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_BANK_CODE_COMP | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_BANK_CODE_COMP | NAME1 | Name 1 | CHAR(30) | NAME1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_LAND_BANK_COMP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_BANK_CODE_COMP OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_DATA,
             LIFNR TYPE LFB1-LIFNR,
             BUKRS TYPE LFB1-BUKRS,
             BANKS TYPE LFBK-BANKS,
             KOVON TYPE LFBK-KOVON,
             KOBIS TYPE LFBK-KOBIS,
             LAND1 TYPE T001-LAND1,
           END OF TY_DATA,
           TT_DATA TYPE STANDARD TABLE OF TY_DATA.
  DATA_SINGLE: SW_DEST       RFCDEST,
               BACKDAYS      INT4,
               FORWDAYS      INT4,
               LAND1         LAND1,
               KOVON         KOVON,
               KOBIS         KOBIS.
  DATA_MULTY: LIFNR    LIFNR,
              BUKRS    BUKRS,
              ERDAT    ERDAT_RF,
              BANKS    BANKS,
              DATUM    SY-DATUM.
*  lv_duration_unit = 'H'.
  SELECT_MULTY: LIFNR,
                BUKRS,
                ERDAT,
                BANKS,
                DATUM.
  SELECT_SINGLE: SW_DEST,
                 LAND1,
                 BACKDAYS.
*  convert_multy: lifnr alpha,
*                 bukrs alpha.
  DATA: SY_TABIX LIKE SY-TABIX,
        SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  DATA: LT_DATA  TYPE TT_DATA,
        LS_DATA  TYPE TY_DATA,
        LS_DATA2 LIKE LINE OF T_DATA[].
  DATA: BACKDAYS  TYPE I,
        FORWDAYS  TYPE I,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM,
        REF_DATE  TYPE D.
  DATA: TIME_DIFF TYPE  INT4 .
  DATA: FLD(60) TYPE C.
  FIELD-SYMBOLS:  TYPE ANY.
  IF NOT LV_FORWDAYS  IS INITIAL.
    LV_BACKDAYS = LV_FORWDAYS * ( -1 ).
  ENDIF.
  IF R_ERDAT[] IS INITIAL AND LV_BACKDAYS <> 0.
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
    R_ERDAT = R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
*  date_from = sy-datum.
  READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_DATUM-LOW.
    DATE_TO   = RS_DATUM-HIGH.
    IF DATE_TO < DATE_FROM.
      DATE_TO = DATE_FROM.
    ENDIF.
  ENDIF.
  IF LV_KOVON IS INITIAL.
    LV_KOVON = DATE_FROM.
  ENDIF.
  IF LV_KOBIS IS INITIAL.
    LV_KOBIS = DATE_TO.
  ENDIF.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_LAND_BANK_COM'
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
  SELECT LFB1~LIFNR LFB1~BUKRS
         LFA1~NAME1
         LFBK~BANKS LFBK~KOVON LFBK~KOBIS
         T001~LAND1
    FROM LFB1 INNER JOIN LFA1 ON  LFB1~LIFNR EQ LFA1~LIFNR
              INNER JOIN LFBK ON  LFB1~LIFNR EQ LFBK~LIFNR
              INNER JOIN T001 ON  LFB1~BUKRS EQ T001~BUKRS
                              AND LFBK~BANKS NE T001~LAND1
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE LFB1~LIFNR IN R_LIFNR
    AND   LFB1~BUKRS IN R_BUKRS
    AND   LFB1~ERDAT IN R_ERDAT
    AND   LFBK~BANKS IN R_BANKS
    AND   LFBK~KOVON >= SY-DATUM
    AND   LFBK~KOBIS <= SY-DATUM.
*  LOOP AT lt_data INTO ls_data.
*
*    CLEAR: ls_data2.
*    IF ls_data-banks <> ls_data-land1.
*      MOVE-CORRESPONDING ls_data TO ls_data2.
*
*      APPEND ls_data2 TO t_data.
*    ENDIF.
*  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
