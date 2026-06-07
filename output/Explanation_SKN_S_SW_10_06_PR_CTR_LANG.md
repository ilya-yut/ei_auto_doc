# Exception Indicator: Prof. Center desc. is miss. for specified by lang. ( SW_10_06_PR_CTR_LANG)

## General Overview

This Exception Indicator monitors profit center master data for missing long descriptions in a specified language, using profit center records from the controlling master and their language-dependent texts.

This EI serves as an essential control for CO master data quality by:

- Surfacing profit centers where long description text is empty in the configured language
- Supporting governance before planning, allocation, and reporting rely on readable profit center names
- Enabling scoped review by controlling area, profit center, validity period, company code, and created-on date
- Complementing manual profit center maintenance with a repeatable exception list
- Helping language harmonization efforts catch gaps after uploads or organizational changes

Typical use includes periodic hygiene on active profit centers, post-migration checks, and targeted reviews after bulk master data loads. The routine reads profit center headers for the selected scope, loads texts for the output language, and flags records where long description remains empty.


## Problem Description

Profit centers without long descriptions in the required language reduce readability of CO reports, allocations, and audit samples.

**Reporting and Controlling Risks**

- Reports and dashboards may show technical profit center keys without meaningful descriptions
- Reviewers spend extra time validating assignments when text is missing in the monitoring language
- Language gaps after reorganizations or uploads may stay hidden until downstream processes fail

**Master Data and Operations Risks**

- New profit centers can be created with valid organizational data but without text in the language used for monitoring
- Validity and company-code scoping errors are harder to spot when descriptions are blank

**Audit and Compliance Risks**

- Evidence of periodic text review is weaker when checks rely on ad hoc display transactions
- Cross-entity comparisons are harder when language selection is not applied consistently

## Suggested Resolution

**Immediate Response**

- Review each flagged controlling area, profit center, validity dates, and language key
- Create or correct long descriptions in the output language through standard CO master data maintenance
- Prioritize profit centers active in current posting or planning cycles

**System Assessment**

- Confirm **SPRAS** matches the language your reports and local teams expect
- Validate **DATAB**, **DATBI**, and **ERSDA** scope against the profit center population you intend to govern
- Compare exception volume by **KOKRS** and **BUKRS** to find clusters from a single project or upload

**Corrective Actions**

- Standardize description maintenance in profit center creation and change procedures
- Update monitoring scope after root cause so the queue stays actionable
- Schedule recurring runs for controlling areas in scope and document review outcomes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | DATAB | Valid From | DATS | 8 | 0 | DATAB | DATUM |
| 4 | DATBI | Valid To | DATS | 8 | 0 | DATBI | DATUM |
| 5 | ERSDA | Created On | DATS | 8 | 0 | ERSDA | DATUM |
| 6 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 7 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 8 | SPRAS | Language Key | LANG | 1 | 0 | SPRAS | SPRAS |
| 9 | SW_DEST | RFC Destination |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 9 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERSDA

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATAB** (Valid From)

Valid-from date on the profit center master record; limits which profit center validity intervals are included when populated.

**DATBI** (Valid To)

Valid-to date on the profit center master record; limits which profit center validity intervals are included when populated.

**ERSDA** (Created On)

Created-on date on the profit center master record used in the selection when populated.

**KOKRS** (Controlling Area)

Controlling area key used for CO-level organizational scoping.

**PRCTR** (Profit Center)

Profit center used for management accounting segmentation and profitability reporting.

**SPRAS** (Language Key)

Language key used for language-dependent text retrieval and filtering.

**SW_DEST** (RFC Destination)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.


### Parameter Relationships

**Controlling scope:** **KOKRS** defines the controlling area; **PRCTR** and **BUKRS** narrow which profit center records are read from the master when populated.

**Validity window:** **DATAB** and **DATBI** limit profit centers to the intended valid-from and valid-to range on the master record.

**Creation date:** **ERSDA** filters profit center created-on date on the master. When used with **BACKDAYS**, the monitoring window applies to that created-on selection.

**Language for text check:** **SPRAS** sets the language used when reading profit center texts; missing long description is evaluated for that language only.

**Execution path:** **SW_DEST** selects cloud versus on-premise execution; all other parameters apply to the on-premise path before any cloud handoff.


### Default Values

- **SPRAS** - initial - treated as unconstrained by code
- **BACKDAYS** - initial - treated as unconstrained by code
- **SW_DEST** - initial - treated as on-premise execution by code

### Practical Example of Parameter Configuration

**Use Case 1: Missing long text in English**

**Purpose:** Find profit centers in controlling area 1000 with no long description in English.

```
KOKRS = 1000
SPRAS = E
BACKDAYS = 30
```

**Use Case 2: Company code scope**

**Purpose:** Limit the review to profit centers linked to selected company codes.

```
KOKRS = 1000
BUKRS = 1000
BUKRS = 2000
SPRAS = D
PRCTR = 1000001000
```

**Use Case 3: Active validity window**

**Purpose:** Review profit centers valid on a key date with explicit validity bounds.

```
KOKRS = COEU
DATAB = 20240101
DATBI = 99991231
SPRAS = E
ERSDA = 20240101
```

**Use Case 4: Named profit centers**

**Purpose:** Audit specific profit centers for missing description text.

```
KOKRS = 1000
PRCTR = 1000001000
PRCTR = 1000002000
SPRAS = E
BACKDAYS = 3650
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
FUNCTION /SKN/F_SW_10_06_PRCTR_TEXT_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_SPRAS_DESC_CH
*"----------------------------------------------------------------------
  DATA_SINGLE: BACKDAYS  INT4,
               SPRAS     SPRAS,
               SW_DEST   RFCDEST.
  DATA_MULTY: KOKRS KOKRS,
              PRCTR PRCTR,
              DATBI DATBI,
              DATAB DATAB,
              ERSDA ERFDT,
              BUKRS BUKRS.
  DATA: DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM.
  DATA: LS_DATA  LIKE LINE OF T_DATA[],
        LS_CEPCT TYPE CEPCT,
        LS_CEPC  TYPE CEPC.
  DATA: LT_CEPCT TYPE TABLE OF CEPCT,
        LT_CEPC  TYPE TABLE OF CEPC,
        LT_DATA  LIKE TABLE OF T_DATA.
  DATA: SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  SELECT_SINGLE: BACKDAYS,
                 SPRAS,
                 SW_DEST.
  SELECT_MULTY: KOKRS,
                PRCTR,
                DATBI,
                DATAB,
                ERSDA,
                BUKRS.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_PRCTR_TEXT_CH'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
    LOOP AT R_PRCTR[] INTO RS_PRCTR.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT         = RS_PRCTR-LOW
        IMPORTING
          OUTPUT        = RS_PRCTR-LOW
          .
      IF RS_PRCTR-HIGH IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT         = RS_PRCTR-HIGH
        IMPORTING
          OUTPUT        = RS_PRCTR-HIGH
          .
      ENDIF.
      MODIFY R_PRCTR[] FROM RS_PRCTR.
    ENDLOOP.
    SELECT *
      FROM CEPC
      INTO TABLE LT_CEPC
      WHERE KOKRS IN R_KOKRS
      AND   PRCTR IN R_PRCTR
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
      FROM CEPCT
      INTO TABLE LT_CEPCT
      FOR ALL ENTRIES IN LT_CEPC
      WHERE SPRAS EQ LV_SPRAS
      AND   KOKRS EQ LT_CEPC-KOKRS
      AND   PRCTR EQ LT_CEPC-PRCTR
      AND   DATBI EQ LT_CEPC-DATBI
      AND   SPRAS NE SPACE.
  ENDIF.
  IF LT_CEPCT IS INITIAL.
    LOOP AT LT_CEPC INTO LS_CEPC.
      CLEAR LS_DATA.
      MOVE-CORRESPONDING LS_CEPC TO LS_DATA.
      APPEND LS_DATA TO T_DATA[].
    ENDLOOP.
  ELSE.
    SORT LT_CEPC  BY KOKRS PRCTR DATBI.
    SORT LT_CEPCT BY KOKRS PRCTR DATBI.
    LOOP AT LT_CEPCT INTO LS_CEPCT WHERE LTEXT IS INITIAL.
      CLEAR: LS_CEPC, LS_DATA.
      READ TABLE LT_CEPC INTO LS_CEPC WITH KEY KOKRS = LS_CEPCT-KOKRS
                                               PRCTR = LS_CEPCT-PRCTR
                                               DATBI = LS_CEPCT-DATBI
                                               BINARY SEARCH.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING LS_CEPC TO LS_DATA.
        LS_DATA-SPRAS = LS_CEPCT-SPRAS.
        LS_DATA-KTEXT = LS_CEPCT-KTEXT.
        APPEND LS_DATA TO T_DATA[].
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
