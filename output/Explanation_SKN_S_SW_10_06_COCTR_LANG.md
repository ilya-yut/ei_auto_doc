# Exception Indicator: Cost Center desc. is missing for specified lang. ( SW_10_06_COCTR_LANG)

## General Overview

This Exception Indicator finds cost centers that have no description—or no text row at all—in the language you specify.

This EI serves as an essential control for controlling master data and reporting quality by:
- Surfacing cost centers missing long text in a given language
- Flagging cost centers with no text maintained at all for that language when no text record exists
- Supporting CO master data governance before allocations, planning, and reports rely on readable cost center names
- Enabling reviews by controlling area, cost center, company code, profit center, and validity or creation dates
- Complementing manual checks in cost center maintenance with a repeatable exception list

Typical use includes reviews after mass uploads, language rollout, or periodic hygiene on active cost centers in a controlling area. Results are intended for exception workflows rather than full cost center master extracts.

The routine reads cost center master records for the selected scope, loads text for the configured language, and raises an alert when text is missing entirely or the long description is blank.


## Problem Description

Failure to monitor cost centers without descriptions in a specified language creates multiple risks across controlling, financial reporting, and master data quality.

**Reporting and Controlling Risks**
- Reports and allocations may show blank or technical identifiers instead of meaningful cost center names in the required language
- Planners and reviewers cannot validate assignments when long text is missing for the language used on output
- Cost centers without any text row for a language may be overlooked until posting or reporting fails a readability check

**Master Data and Operations Risks**
- New cost centers can reach production without text in the language required for local or group reporting
- Bulk loads and interfaces may create master records without corresponding text in every needed language
- Unscoped validity or creation filters can produce incomplete or overly broad exception populations

**Audit and Compliance Risks**
- Evidence of periodic text review is weaker when checks rely on ad hoc display transactions
- Multilingual master data policies are harder to enforce without language-specific exception monitoring

## Suggested Resolution

**Immediate Response**
- Review each flagged cost center, controlling area, validity dates, and language key in the exception
- Confirm with CO master data owners whether long text should be created in the specified language
- Prioritize cost centers used in current posting, planning, or allocation cycles

**System Assessment**
- Compare exception volume to prior runs using the same controlling area and **SPRAS** value
- Determine whether alerts are driven by completely missing text records or by empty long descriptions only
- Look for clusters by company code or cost center range that may trace to a single upload or project

**Corrective Actions**
- Maintain or correct cost center descriptions through standard CO text maintenance with required approvals
- Update monitoring scope after root cause so the queue stays actionable for master data teams
- Document review outcomes for audit trail and schedule recurring runs for controlling areas in scope
- Route repeat interface defects into change management when text records are systematically omitted


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
| 8 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 9 | SPRAS | Language Key | CHAR | 1 | 0 | SPRAS | SPRAS |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 9 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATAB** (Valid From)

Valid-from date on the cost center master record used to limit which cost centers are read.

**DATBI** (Valid To)

Valid-to date on the cost center master record used together with valid-from to define the active cost center interval in scope.

**ERSDA** (Created On)

Created-on date on the cost center master record used to restrict results to cost centers created in the selected period.

**KOKRS** (Controlling Area)

Controlling area key used for CO-level organizational scoping.

**KOSTL** (Cost Center)

Cost center used as primary CO account assignment for postings and budgets. Values are normalized with leading zeros before selection.

**PRCTR** (Profit Center)

Profit center used for management accounting segmentation and profitability reporting.

**SPRAS** (Language Key)

Language key used for language-dependent text retrieval and filtering. The routine reads cost center text for this language only and flags cost centers with no text record or with an empty long description in that language.


### Parameter Relationships

How parameter combinations work together

**Controlling scope:** **KOKRS** defines the controlling area; **KOSTL**, **BUKRS**, and **PRCTR** narrow which cost center master records are read when populated.

**Language:** **SPRAS** sets the single language used to read cost center text. All missing-description checks apply only to that language.

**Validity and creation:** **DATAB**, **DATBI**, and **ERSDA** filter cost center master records by valid-from, valid-to, and created-on dates as supplied in selection.

**Backdays:** **BACKDAYS** is available in the parameter set for monitor configuration; record selection for this function uses the explicit date and master-data filters above.

**Combined effect:** Controlling area, cost center, company code, profit center, validity and creation dates, and **SPRAS** apply together. A row appears when the cost center is in scope and has no text row for **SPRAS**, or has a text row with an empty long description in that language.


### Default Values

- **BACKDAYS** - initial - treated as 0 by code

### Practical Example of Parameter Configuration

**Use Case 1: Missing English descriptions in one controlling area**

**Purpose:** Find cost centers in controlling area 1000 with no English long text or no English text record at all.
```
KOKRS = 1000
SPRAS = E
```

**Use Case 2: Recent cost centers in a company code**

**Purpose:** Review cost centers created since the start of the year in selected company codes for missing German text.
```
KOKRS = COEU
BUKRS = DE01
BUKRS = DE02
SPRAS = D
ERSDA = 20250101
```

**Use Case 3: Specific cost center with validity filter**

**Purpose:** Check one cost center for missing description in French for records valid through year-end.
```
KOKRS = 1000
KOSTL = 0000041000
SPRAS = F
DATBI = 99991231
```

**Use Case 4: Profit center slice with valid-from bound**

**Purpose:** Monitor cost centers assigned to a profit center when text is missing in Spanish from a given valid-from date.
```
KOKRS = 2000
PRCTR = PC1000001000
SPRAS = S
DATAB = 20240101
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
FUNCTION /SKN/F_SW_10_06_COCTR_TEXT_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_SPRAS_DESC_CH
*"----------------------------------------------------------------------
  DATA_SINGLE: BACKDAYS INT4,
               SPRAS    SPRAS,
               SW_DEST  RFCDEST
               .
  DATA_MULTY: KOKRS KOKRS,
              KOSTL KOSTL,
              DATBI DATBI,
              DATAB DATAB,
              ERSDA ERSDA,
              BUKRS BUKRS,
              PRCTR PRCTR.
  DATA: LV_TABIX TYPE I.
  DATA: DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM.
  DATA: LS_DATA LIKE LINE OF T_DATA[],
        LS_CSKT TYPE CSKT,
        LS_CSKS TYPE CSKS.
  DATA: LT_DATA LIKE TABLE OF T_DATA,
        LT_CSKT TYPE TABLE OF CSKT,
        LT_CSKS TYPE TABLE OF CSKS.
  DATA: SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF T_DATA[].
  SELECT_SINGLE: BACKDAYS,
                 SPRAS,
                 SW_DEST.
  SELECT_MULTY: KOKRS,
                KOSTL,
                DATBI,
                DATAB,
                ERSDA,
                BUKRS,
                PRCTR.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_COCTR_TEXT_CH'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
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
    SELECT *
      FROM CSKS
      INTO TABLE LT_CSKS
      WHERE KOKRS IN R_KOKRS
      AND   KOSTL IN R_KOSTL
      AND   DATBI IN R_DATBI
      AND   DATAB IN R_DATAB
      AND   ERSDA IN R_ERSDA
      AND   BUKRS IN R_BUKRS.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
    EXIT.
  ENDIF.
  IF SY-SUBRC = 0.
    SELECT *
      FROM CSKT
      INTO TABLE LT_CSKT
      FOR ALL ENTRIES IN LT_CSKS
      WHERE SPRAS EQ LV_SPRAS
      AND   KOKRS EQ LT_CSKS-KOKRS
      AND   KOSTL EQ LT_CSKS-KOSTL
      AND   DATBI EQ LT_CSKS-DATBI
      AND   SPRAS NE SPACE.
  ENDIF.
  IF LT_CSKT IS INITIAL.
    LOOP AT LT_CSKS INTO LS_CSKS.
      CLEAR LS_DATA.
      MOVE-CORRESPONDING LS_CSKS TO LS_DATA.
      APPEND LS_DATA TO T_DATA[].
    ENDLOOP.
  ELSE.
    SORT LT_CSKT BY KOKRS KOSTL DATBI.
    SORT LT_CSKS BY KOKRS KOSTL DATBI.
    LOOP AT LT_CSKT INTO LS_CSKT WHERE LTEXT IS INITIAL.
      CLEAR: LS_CSKS, LS_DATA.
      READ TABLE LT_CSKS INTO LS_CSKS WITH KEY KOKRS = LS_CSKT-KOKRS
                                               KOSTL = LS_CSKT-KOSTL
                                               DATBI = LS_CSKT-DATBI
                                               BINARY SEARCH.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING LS_CSKS TO LS_DATA.
        LS_DATA-SPRAS = LS_CSKT-SPRAS.
        LS_DATA-KTEXT = LS_CSKT-KTEXT.
        APPEND LS_DATA TO T_DATA[].
      ENDIF.
    ENDLOOP.
  ENDIF.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
