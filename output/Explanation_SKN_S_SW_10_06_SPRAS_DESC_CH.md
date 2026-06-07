# Exception Indicator: Cost center missing description alert in output language ( SW_10_06_CC_DSC_MISS)

## General Overview

This Exception Indicator finds cost centers that lack a description in the configured output language, using the language reference you select (cost center, company code, or profit center language).

This EI serves as an essential control for controlling master data and reporting quality by:
- Surfacing cost centers with missing text in the language used for monitoring output
- Supporting CO master data governance before allocations, planning, and reports rely on readable cost center names
- Enabling reviews by controlling area, cost center, validity period, and creation date
- Allowing comparison against cost center language, company code language, or profit center language depending on organizational rules
- Complementing manual checks in cost center maintenance with a repeatable exception list

Typical use includes reviews after mass uploads, language harmonization projects, or periodic hygiene on active cost centers. Results are intended for exception workflows rather than full cost center master extracts.

The routine reads active cost center master data for the selected scope, applies validity and creation-date filters, and removes records that already have a description in the relevant language before raising an alert.


## Problem Description

Failure to monitor cost centers without descriptions in the output language creates multiple risks across controlling, financial reporting, and master data quality.

**Reporting and Controlling Risks**
- Reports and allocations may show blank or technical identifiers instead of meaningful cost center names
- Planners and reviewers cannot quickly validate assignments when descriptions are missing in the monitoring language
- Language harmonization gaps between company code, profit center, and cost center master data go undetected

**Master Data and Operations Risks**
- New or changed cost centers can reach production without text in the language required for local reporting
- Bulk loads and interfaces may omit descriptions while still creating valid master records
- Without scoped validity and creation filters, reviews may miss recently created or currently active cost centers

**Audit and Compliance Risks**
- Evidence of periodic master data text review is weaker when checks rely on ad hoc display transactions
- Cross-entity comparisons are harder when company-code or profit-center language rules are not applied consistently

## Suggested Resolution

**Immediate Response**
- Review each flagged cost center, controlling area, validity dates, and language keys shown in the exception
- Confirm with CO master data owners whether text should be created in the output language or whether the language reference setting should change
- Prioritize cost centers used in current posting, planning, or allocation cycles

**System Assessment**
- Compare exception volume to prior runs using the same controlling area and **LANG_REF_FLD** setting
- Look for clusters by cost center category or company code that may trace to a single upload or project
- Validate that **SPRAS** values match the languages your organization expects on reports

**Corrective Actions**
- Maintain or correct cost center descriptions through standard CO master data processes with required approvals
- Adjust **LANG_REF_FLD** only when business rules define a different reference language for the control
- Update monitoring scope after root cause so the queue stays actionable for master data teams
- Document review outcomes for audit trail and schedule recurring runs for controlling areas in scope


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Back | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | DATAB | Valid From | DATS | 8 | 0 | DATAB | DATUM |
| 4 | DATBI | Valid To | DATS | 8 | 0 | DATBI | DATUM |
| 5 | ERSDA | Created On | DATS | 8 | 0 | ERSDA | DATUM |
| 6 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 7 | KOSTL | Cost Center | CHAR | 10 | 0 | KOSTL | KOSTL |
| 8 | LANG_REF_FLD | Language Ref. Field | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |
| 9 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 10 | SPRAS | Language Key | CHAR | 1 | 0 | SPRAS | SPRAS |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 10 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level. Used when **LANG_REF_FLD** is set to compare descriptions against the company code language.

**DATAB** (Valid From)

Valid-from date on the cost center master record. When left open, the routine includes cost centers whose valid-from is on or before the run date.

**DATBI** (Valid To)

Valid-to date on the cost center master record. When left open, the routine includes cost centers whose valid-to is on or after the run date.

**ERSDA** (Created On)

Created-on date on the cost center master record. When left open and **BACKDAYS** is not initial, the routine builds a created-on range from the run date minus **BACKDAYS** through the run date.

**KOKRS** (Controlling Area)

Controlling area key used for CO-level organizational scoping.

**KOSTL** (Cost Center)

Cost center used as primary CO account assignment for postings and budgets. Values are normalized with leading zeros before selection.

**LANG_REF_FLD** (Language Ref. Field)

Selects which language source drives the missing-description check: cost center language, company code language, or profit center language.

**LANG_REF_FLD Options:**

- KOSTL — Compare the cost center’s own language key to text maintained for that cost center; rows with a non-empty description in that language are removed.
- BUKRS — Use the company code language from financial master data as the reference language for the check.
- PRCTR — Use the profit center language from profit center master data as the reference language for the check.

**PRCTR** (Profit Center)

Profit center used for management accounting segmentation and profitability reporting. Used when **LANG_REF_FLD** is set to the profit-center language mode.

**SPRAS** (Language Key)

Language key used for language-dependent text retrieval and filtering. The routine requires at least one language value; without it, no data is processed.


### Parameter Relationships

How parameter combinations work together

**Controlling scope:** **KOKRS** defines the controlling area; **KOSTL**, **BUKRS**, and **PRCTR** narrow which cost center records are read when populated.

**Language selection:** **SPRAS** must be populated—the routine does not run without a language selection. **LANG_REF_FLD** determines whether missing descriptions are judged against cost center language, company code language, or profit center language.

**Validity window:** **DATAB** and **DATBI** filter cost centers by valid-from and valid-to. When either is left open, the routine applies a default bound using the run date so currently valid cost centers are in scope.

**Creation date:** **ERSDA** filters by cost center created-on date. When **ERSDA** is empty and **BACKDAYS** is not initial, the routine sets a created-on range from the run date minus **BACKDAYS** through the run date.

**Combined effect:** Controlling area, cost center, company code, profit center, validity dates, creation date, output language, and language reference mode apply together; a row remains when the cost center is in scope and lacks description text in the language determined by **LANG_REF_FLD** and **SPRAS**.


### Default Values

- **BACKDAYS** - initial - treated as 0 by code
- **LANG_REF_FLD** - initial - treated as KOSTL by code

### Practical Example of Parameter Configuration

**Use Case 1: Missing text in English for one controlling area**

**Purpose:** Find active cost centers in controlling area 1000 with no English description when the cost center language is the reference.
```
KOKRS = 1000
SPRAS = E
LANG_REF_FLD = KOSTL
BACKDAYS = 30
```

**Use Case 2: Company code language check**

**Purpose:** Flag cost centers in selected company codes where description text is missing for the company code language.
```
KOKRS = 1000
BUKRS = 1000
BUKRS = 2000
SPRAS = D
LANG_REF_FLD = BUKRS
```

**Use Case 3: Profit center language mode**

**Purpose:** Review cost centers tied to profit centers when text is missing for the profit center language.
```
KOKRS = COEU
PRCTR = EU10000001
PRCTR = EU10000002
SPRAS = E
LANG_REF_FLD = PRCTR
```

**Use Case 4: Specific cost center with validity filter**

**Purpose:** Check one cost center for missing description with an explicit valid-to lower bound.
```
KOKRS = 1000
KOSTL = 0000041000
SPRAS = E
DATBI = 99991231
ERSDA = 20250101
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_SPRAS_DESC_CH | BKZER | Actual revenues | CHAR(1) | BKZER |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | BKZKP | Actual primary costs | CHAR(1) | BKZKP |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | BKZKS | Actual secondary costs | CHAR(1) | BKZKS |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | BKZOB | Commitment update | CHAR(1) | BKZOB |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | DATAB | Valid From | DATS(8) | DATAB |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | DATBI | Valid To | DATS(8) | DATBI |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | ERSDA | Created On | DATS(8) | ERSDA |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | FUNC_AREA | Functional Area | CHAR(16) | FKBER |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | KHINR | Hierarchy Area | CHAR(12) | PHINR |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | KOKRS | Controlling Area | CHAR(4) | KOKRS |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | KOSAR | Cost Center Category | CHAR(1) | KOSAR |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | KOSTL | Cost Center | CHAR(10) | KOSTL |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | KTEXT | Name | CHAR(20) | KTEXT |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | LOCK_IND | Lock indicator | CHAR(1) | LOCK_IND |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | LTEXT | Description | CHAR(40) | KLTXT |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | MCTXT | Cost ctr short text | CHAR(20) | MCDS3 |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | MGEFL | Record Quantity | CHAR(1) | MGEFL |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | PKZER | Plan revenues | CHAR(1) | PKZER |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | PKZKP | Plan primary costs | CHAR(1) | PKZKP |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | PKZKS | Plan secondary costs | CHAR(1) | PKZKS |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | PRCTR | Profit Center | CHAR(10) | PRCTR |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | SPRAS | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | SPRAS_CEPC | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | SPRAS_T001 | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | VERAK | Person Responsible | CHAR(20) | VERAK |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | VERAK_USER | User Responsible | CHAR(12) | VERAK_USER |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_06_SPRAS_DESC_CH | WERKS | Plant | CHAR(4) | WERKS_D |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_SPRAS_DESC_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_SPRAS_DESC_CH
*"----------------------------------------------------------------------
  DATA_SINGLE: BACKDAYS         INT4,
               LANG_REF_FLD     NAME_FELD,
               SW_DEST          RFCDEST
               .
  LV_LANG_REF_FLD = 'KOSTL'.
  DATA_MULTY: KOKRS KOKRS,
              KOSTL KOSTL,
              DATBI DATBI,
              SPRAS SPRAS,
              DATAB DATAB,
              ERSDA ERFDT,
              BUKRS BUKRS,
              PRCTR PRCTR.
  DATA: DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM.
  DATA: LV_TABIX TYPE I.
  DATA: LS_DATA LIKE LINE OF T_DATA[],
        LS_CSKT TYPE CSKT.
  DATA: LT_DATA LIKE TABLE OF T_DATA,
        LT_CSKT TYPE TABLE OF CSKT.
  DATA: SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  SELECT_SINGLE: BACKDAYS,
                 LANG_REF_FLD,
                 SW_DEST.
  SELECT_MULTY: KOKRS,
                KOSTL,
                DATBI,
                SPRAS,
                DATAB,
                ERSDA,
                BUKRS,
                PRCTR.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_SPRAS_DSC_CH'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  CHECK R_SPRAS IS NOT INITIAL.
" Valid from
  IF R_DATAB[] IS INITIAL.
    RS_DATAB-SIGN   = 'I' .
    RS_DATAB-OPTION = 'LE' .
    DATE_FROM       = SY-DATUM.
    RS_DATAB-LOW    = DATE_FROM.
    APPEND RS_DATAB TO R_DATAB.
  ENDIF.
* Valid to
  IF R_DATBI[] IS INITIAL .
    RS_DATBI-SIGN   = 'I' .
    RS_DATBI-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM.
    RS_DATBI-LOW    = DATE_FROM.
    APPEND RS_DATBI TO R_DATBI.
  ENDIF.
* Set 'Created on' date
  IF R_ERSDA[] IS INITIAL AND LV_BACKDAYS IS NOT INITIAL.
    RS_ERSDA-SIGN   = 'I' .
    RS_ERSDA-OPTION = 'BT' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    DATE_TO         = SY-DATUM.
    RS_ERSDA-LOW    = DATE_FROM.
    RS_ERSDA-HIGH   = DATE_TO.
    APPEND RS_ERSDA TO R_ERSDA.
  ENDIF.
  IF R_KOSTL IS NOT INITIAL.
    LOOP AT R_KOSTL[] INTO RS_KOSTL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT         = RS_KOSTL-LOW
        IMPORTING
          OUTPUT        = RS_KOSTL-LOW
          .
      IF RS_KOSTL-HIGH IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT         = RS_KOSTL-HIGH
        IMPORTING
          OUTPUT        = RS_KOSTL-HIGH
          .
      ENDIF.
      MODIFY R_KOSTL[] FROM RS_KOSTL.
    ENDLOOP.
  ENDIF.
  CASE LV_LANG_REF_FLD.
* Description missing check according to cost center language
    WHEN 'KOSTL'.
      SELECT *
        FROM CSKS LEFT OUTER JOIN CSKT ON  CSKS~SPRAS EQ CSKT~SPRAS
                                       AND CSKS~KOKRS EQ CSKT~KOKRS
                                       AND CSKS~KOSTL EQ CSKT~KOSTL
                                       AND CSKS~DATBI EQ CSKT~DATBI
        INTO CORRESPONDING FIELDS OF TABLE T_DATA
        WHERE CSKS~KOKRS IN R_KOKRS
        AND   CSKS~KOSTL IN R_KOSTL
        AND   CSKS~DATBI IN R_DATBI
        AND   CSKS~DATAB IN R_DATAB
        AND   CSKS~ERSDA IN R_ERSDA
        AND   CSKS~SPRAS IN R_SPRAS
        AND   CSKS~SPRAS NE SPACE.
* Description missing check according to company code language
    WHEN 'BUKRS'.
      SELECT CSKS~KOKRS CSKS~KOSTL CSKS~DATBI CSKS~BUKRS CSKS~GSBER
             CSKS~KOSAR CSKS~VERAK CSKS~VERAK_USER CSKS~WAERS
             CSKS~PRCTR CSKS~WERKS CSKS~BKZKP CSKS~PKZKP CSKS~BKZKS
             CSKS~BKZER CSKS~BKZOB CSKS~PKZKS CSKS~PKZER CSKS~MGEFL
             CSKS~MGEFL CSKS~FUNC_AREA T001~SPRAS AS SPRAS_ADD
        FROM CSKS INNER JOIN T001 ON CSKS~BUKRS EQ T001~BUKRS
        INTO CORRESPONDING FIELDS OF TABLE T_DATA
        WHERE CSKS~KOKRS IN R_KOKRS
        AND   CSKS~KOSTL IN R_KOSTL
        AND   CSKS~DATBI IN R_DATBI
        AND   CSKS~DATAB IN R_DATAB
        AND   CSKS~ERSDA IN R_ERSDA
        AND   CSKS~BUKRS IN R_BUKRS
        AND   CSKS~SPRAS IN R_SPRAS
        AND   T001~SPRAS NE SPACE.
*   Description missing check according to profit center language
    WHEN 'PRCTR'.
      SELECT CSKS~KOKRS CSKS~KOSTL CSKS~DATBI CSKS~BUKRS CSKS~GSBER
             CSKS~KOSAR CSKS~VERAK CSKS~VERAK_USER CSKS~WAERS
             CSKS~PRCTR CSKS~WERKS CSKS~BKZKP CSKS~PKZKP CSKS~BKZKS
             CSKS~BKZER CSKS~BKZOB CSKS~PKZKS CSKS~PKZER CSKS~MGEFL
             CSKS~MGEFL CSKS~FUNC_AREA
             CEPC~KOKRS CEPC~PRCTR CEPC~DATBI CEPC~DATAB CEPC~BUKRS
             CEPC~VERAK CEPC~VERAK_USER CEPC~WAERS CEPC~KHINR
             CEPC~LOCK_IND CEPC~SPRAS AS SPRAS_ADD
        FROM CSKS INNER JOIN CEPC ON  CSKS~PRCTR EQ CEPC~PRCTR
        INTO CORRESPONDING FIELDS OF TABLE T_DATA
        WHERE CSKS~KOKRS IN R_KOKRS
        AND   CSKS~KOSTL IN R_KOSTL
        AND   CSKS~DATBI IN R_DATBI
        AND   CSKS~DATAB IN R_DATAB
        AND   CSKS~ERSDA IN R_ERSDA
        AND   CSKS~PRCTR IN R_PRCTR
        AND   CSKS~SPRAS IN R_SPRAS
        AND   CSKS~SPRAS NE SPACE.
  WHEN OTHERS.
  ENDCASE.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  IF T_DATA[] IS NOT INITIAL AND LV_LANG_REF_FLD EQ 'KOSTL'.
    SORT T_DATA[] BY LTEXT.
    DELETE T_DATA[] WHERE LTEXT IS NOT INITIAL.
  ELSE.
    IF T_DATA[] IS NOT INITIAL.
      SELECT *
      FROM CSKT
      INTO TABLE LT_CSKT
      FOR ALL ENTRIES IN T_DATA[]
      WHERE SPRAS EQ T_DATA-SPRAS
      AND   KOKRS EQ T_DATA-KOKRS
      AND   KOSTL EQ T_DATA-KOSTL
      AND   DATBI EQ T_DATA-DATBI
      AND   SPRAS NE SPACE
      AND   LTEXT EQ SPACE.
    ENDIF.
    IF LT_CSKT IS NOT INITIAL.
      SORT T_DATA[] BY KOKRS KOSTL DATBI SPRAS.
      SORT LT_CSKT  BY KOKRS KOSTL DATBI SPRAS.
    ENDIF.
    LOOP AT LT_CSKT INTO LS_CSKT WHERE LTEXT IS NOT INITIAL.
      CLEAR LS_DATA.
*        IF ls_cskt-ltext IS NOT INITIAL.
      READ TABLE T_DATA[] INTO LS_DATA WITH KEY KOKRS = LS_CSKT-KOKRS
      KOSTL = LS_CSKT-KOSTL
      DATBI = LS_CSKT-DATBI
      SPRAS = LS_CSKT-SPRAS
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        DELETE T_DATA[] FROM LS_DATA.
      ENDIF.
*        ENDIF.
    ENDLOOP.
  ENDIF.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
