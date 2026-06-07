# Exception Indicator: ProfCen. - Missing desc. alert in output lang. ( SW_10_06_PC_DSC_MISS)

## General Overview

This Exception Indicator finds profit centers that lack a long description (**LTEXT**) in the language used for the check, based on either the profit center language or the company code language.

This EI serves as an essential control for controlling master data and reporting quality by:
- Surfacing profit centers with missing text in the configured output language
- Supporting CO master data governance before allocations, planning, and reports rely on readable profit center names
- Enabling reviews by controlling area, profit center, validity period, and creation date
- Allowing comparison against profit center language or company code language depending on organizational rules
- Complementing manual checks in profit center maintenance with a repeatable exception list

Typical use includes reviews after mass uploads, language harmonization projects, or periodic hygiene on active profit centers. Results are intended for exception workflows rather than full profit center master extracts.

The routine reads profit center master data (**CEPC**) for the selected scope, applies validity and creation-date filters, compares text from **CEPCT**, and keeps only records where **LTEXT** is still empty in the relevant language before raising an alert.


## Problem Description

Failure to monitor profit centers without descriptions in the output language creates multiple risks across controlling, financial reporting, and master data quality.

**Reporting and Controlling Risks**
- Reports and allocations may show blank or technical identifiers instead of meaningful profit center names
- Planners and reviewers cannot quickly validate assignments when descriptions are missing in the monitoring language
- Language harmonization gaps between company code and profit center master data go undetected

**Master Data and Operations Risks**
- New or changed profit centers can reach production without text in the language required for local reporting
- Bulk loads and interfaces may omit descriptions while still creating valid master records
- Without scoped validity and creation filters, reviews may miss recently created or currently active profit centers

**Audit and Compliance Risks**
- Evidence of periodic master data text review is weaker when checks rely on ad hoc display transactions
- Cross-entity comparisons are harder when company-code or profit-center language rules are not applied consistently

## Suggested Resolution

**Immediate Response**
- Review each flagged profit center, controlling area, validity dates, and language keys shown in the exception
- Confirm with CO master data owners whether text should be created in the output language or whether **LANG_REF_FLD** should change
- Prioritize profit centers used in current posting, planning, or allocation cycles

**System Assessment**
- Compare exception volume to prior runs using the same controlling area and **LANG_REF_FLD** setting
- Look for clusters by company code or hierarchy that may trace to a single upload or project
- Validate that **SPRAS** and reference languages on **CEPC** / **T001** match what reports expect

**Corrective Actions**
- Maintain or correct profit center descriptions through standard CO master data processes with required approvals
- Adjust **LANG_REF_FLD** only when business rules define a different reference language for the control
- Update monitoring scope after root cause so the queue stays actionable for master data teams
- Document review outcomes for audit trail and schedule recurring runs for controlling areas in scope


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Back | INT4 | 10 | 0 | BACKDAYS |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS |
| 3 | DATAB | Valid From | DATS | 8 | 0 | DATAB |
| 4 | DATBI | Valid To | DATS | 8 | 0 | DATBI |
| 5 | ERSDA | Created On | DATS | 8 | 0 | ERSDA |
| 6 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS |
| 7 | LANG_REF_FLD | Language Ref. Field | CHAR | 30 | 0 | NAME_FELD |
| 8 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR |
| 9 | SPRAS | Language Key | CHAR | 1 | 0 | SPRAS |
| 10 | SW_DEST | RFC Destination | CHAR | 32 | 0 | RFCDEST |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 10 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, and surfaced for alerting.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

When **ERSDA** is not supplied and **BACKDAYS** is not initial, the routine builds a created-on range from the run date minus **BACKDAYS** through the run date.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level. In **BUKRS** language mode, **T001-SPRAS** drives the description check for linked profit centers.

**DATAB** (Valid From)

Valid-from date on the profit center (**CEPC-DATAB**). When left open, the routine includes profit centers whose valid-from is on or before the run date.

**DATBI** (Valid To)

Valid-to date on the profit center (**CEPC-DATBI**). When left open, the routine includes profit centers whose valid-to is on or after the run date.

**ERSDA** (Created On)

Created-on date on the profit center (**CEPC-ERSDA**). When left open and **BACKDAYS** is not initial, the routine sets a created-on range from the run date minus **BACKDAYS** through the run date.

**KOKRS** (Controlling Area)

Controlling area key used for CO-level organizational scoping.

**LANG_REF_FLD** (Language Ref. Field)

Selects which language source drives the missing-description check for profit centers.

**LANG_REF_FLD Options:**

- **BUKRS** — Use the company code language from **T001** (including profit centers linked through **CEPC_BUKRS**); read **CEPCT** text for that language and flag rows where **LTEXT** is still empty.
- **PRCTR** — Use the profit center language on **CEPC**; join **CEPCT** on **CEPC-SPRAS** and flag rows where **LTEXT** is still empty.

**PRCTR** (Profit Center)

Profit center used for management accounting segmentation and profitability reporting. Values are normalized with leading zeros before selection.

**SPRAS** (Language Key)

Language key used for language-dependent text retrieval and filtering. In **PRCTR** mode filters **CEPC-SPRAS**; in **BUKRS** mode can restrict **T001-SPRAS**.

**SW_DEST** (RFC Destination)

When populated, the routine runs the cloud function `/SKN/FC_SW_10_06_PC_SPR_DSC_CH` instead of on-premise logic.


### Parameter Relationships

**Controlling scope:** **KOKRS** defines the controlling area; **PRCTR** and **BUKRS** narrow which profit center records are read when populated.

**Language selection:** **LANG_REF_FLD** determines whether missing descriptions are judged against profit center language (**PRCTR**) or company code language (**BUKRS**). **SPRAS** filters the language key in the corresponding mode.

**Validity window:** **DATAB** and **DATBI** filter profit centers by valid-from and valid-to. When either is left open, the routine applies a default bound using the run date so currently valid profit centers are in scope.

**Creation date:** **ERSDA** filters by profit center created-on date. When **ERSDA** is empty and **BACKDAYS** is not initial, the routine sets a created-on range from the run date minus **BACKDAYS** through the run date.

**Execution path:** **SW_DEST** selects cloud versus on-premise execution; all other parameters apply to the on-premise path before any cloud handoff.

**Combined effect:** Controlling area, profit center, company code, validity dates, creation date (or the **BACKDAYS**-derived created-on window), output language, and **LANG_REF_FLD** apply together; a row remains when the profit center is in scope and **LTEXT** is still empty in the language determined by the check mode.


### Default Values

- **BACKDAYS** - initial - treated as unconstrained by code
- **LANG_REF_FLD** - initial - treated as PRCTR by code

### Practical Example of Parameter Configuration

**Use Case 1: Missing long text in profit center language**

**Purpose:** Find profit centers in controlling area 1000 with no long description when the profit center language is the reference.

```
KOKRS = 1000
SPRAS = E
LANG_REF_FLD = PRCTR
BACKDAYS = 30
```

**Use Case 2: Company code language check**

**Purpose:** Flag profit centers in selected company codes where **LTEXT** is missing for the company code language from **T001**.

```
KOKRS = 1000
BUKRS = 1000
SPRAS = D
LANG_REF_FLD = BUKRS
```

**Use Case 3: Specific profit centers**

**Purpose:** Review named profit centers for missing description with explicit validity bounds.

```
KOKRS = COEU
PRCTR = EU10000001
SPRAS = E
LANG_REF_FLD = PRCTR
DATBI = 99991231
```

**Use Case 4: Recently created profit centers**

**Purpose:** Limit the review to profit centers created in the last 14 days when **ERSDA** is left open.

```
KOKRS = 1000
SPRAS = E
LANG_REF_FLD = PRCTR
BACKDAYS = 14
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
FUNCTION /SKN/F_SW_10_06_PC_SPR_DSC_CHK .
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
               SW_DEST          RFCDEST.
  LV_LANG_REF_FLD = 'PRCTR'.
  DATA_MULTY: KOKRS KOKRS,
              PRCTR PRCTR,
              DATBI DATBI,
              DATAB DATAB,
              ERSDA ERFDT,
              BUKRS BUKRS,
              SPRAS SPRAS.
  DATA: DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM.
  DATA: LS_DATA  LIKE LINE OF T_DATA[],
        LS_CEPCT TYPE CEPCT.
  DATA: LT_CEPCT TYPE TABLE OF CEPCT,
        LT_DATA  LIKE TABLE OF T_DATA.
  DATA: SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  SELECT_SINGLE: BACKDAYS,
                 LANG_REF_FLD,
                 SW_DEST.
  SELECT_MULTY: KOKRS,
                PRCTR,
                DATBI,
                DATAB,
                ERSDA,
                BUKRS,
                SPRAS.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_PC_SPR_DSC_CH'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  " Valid from
  IF R_DATAB[] IS INITIAL.
    RS_DATAB-SIGN   = 'I' .
    RS_DATAB-OPTION = 'LE' .
    DATE_FROM       = SY_DATLO.
    RS_DATAB-LOW    = DATE_FROM.
    APPEND RS_DATAB TO R_DATAB.
  ENDIF.
* Valid to
  IF R_DATBI[] IS INITIAL .
    RS_DATBI-SIGN   = 'I' .
    RS_DATBI-OPTION = 'GE' .
    DATE_FROM       = SY_DATLO.
    RS_DATBI-LOW    = DATE_FROM.
    APPEND RS_DATBI TO R_DATBI.
  ENDIF.
* Set 'Created on' date
  IF R_ERSDA[] IS INITIAL AND LV_BACKDAYS IS NOT INITIAL.
    RS_ERSDA-SIGN   = 'I' .
    RS_ERSDA-OPTION = 'BT' .
    DATE_FROM       = SY_DATLO - LV_BACKDAYS.
    DATE_TO         = SY_DATLO.
    RS_ERSDA-LOW    = DATE_FROM.
    RS_ERSDA-HIGH   = DATE_TO.
    APPEND RS_ERSDA TO R_ERSDA.
  ENDIF.
  IF R_PRCTR IS NOT INITIAL.
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
  ENDIF.
  CASE LV_LANG_REF_FLD.
*   Language check according to company code language
    WHEN 'BUKRS'.
      SELECT CEPC~KOKRS CEPC~PRCTR CEPC~DATBI CEPC~DATAB CEPC~BUKRS
             CEPC~VERAK CEPC~VERAK_USER CEPC~WAERS CEPC~KHINR
             CEPC~LOCK_IND
             T001~SPRAS AS SPRAS_T001
        FROM CEPC LEFT JOIN T001 ON CEPC~BUKRS EQ T001~BUKRS
        INTO CORRESPONDING FIELDS OF TABLE LT_DATA
        WHERE CEPC~PRCTR IN R_PRCTR[]
        AND   CEPC~KOKRS IN R_KOKRS[]
        AND   CEPC~DATBI IN R_DATBI[]
        AND   CEPC~DATAB IN R_DATAB[]
        AND   CEPC~ERSDA IN R_ERSDA[]
        AND   CEPC~BUKRS IN R_BUKRS[].
    IF LT_DATA IS NOT INITIAL.
      SORT LT_DATA BY BUKRS.
      DELETE LT_DATA WHERE BUKRS EQ SPACE.
    ENDIF.
      SELECT CEPC~KOKRS CEPC~PRCTR CEPC~DATBI CEPC~DATAB
             CEPC~VERAK CEPC~VERAK_USER CEPC~WAERS CEPC~KHINR
             CEPC~LOCK_IND
             CEPC_BUKRS~BUKRS
             T001~SPRAS AS SPRAS_T001
        FROM CEPC LEFT JOIN CEPC_BUKRS ON  CEPC~PRCTR       EQ CEPC_BUKRS~PRCTR
                                       AND CEPC~KOKRS       EQ CEPC_BUKRS~KOKRS
                  INNER JOIN T001      ON  CEPC_BUKRS~BUKRS EQ T001~BUKRS
        APPENDING CORRESPONDING FIELDS OF TABLE LT_DATA
        WHERE CEPC~PRCTR IN R_PRCTR[]
        AND   CEPC~KOKRS IN R_KOKRS[]
        AND   CEPC~DATBI IN R_DATBI[]
        AND   CEPC~DATAB IN R_DATAB[]
        AND   CEPC~ERSDA IN R_ERSDA[]
        AND   CEPC~BUKRS IN R_BUKRS[]
        AND   T001~BUKRS IN R_BUKRS[]
        AND   T001~SPRAS IN R_SPRAS[]
        AND   T001~SPRAS NE SPACE.
    IF LT_DATA[] IS NOT INITIAL.
      SORT LT_DATA BY BUKRS.
      SELECT *
        FROM CEPCT
        INTO TABLE LT_CEPCT
        FOR ALL ENTRIES IN LT_DATA
        WHERE ( SPRAS EQ LT_DATA-SPRAS_T001 OR SPRAS EQ LT_DATA-SPRAS )
        AND     PRCTR EQ LT_DATA-PRCTR
        AND     DATBI EQ LT_DATA-DATBI
        AND     KOKRS EQ LT_DATA-KOKRS.
    ENDIF.
    SORT LT_CEPCT BY SPRAS PRCTR DATBI KOKRS.
    SORT LT_DATA  BY SPRAS PRCTR DATBI KOKRS.
    LOOP AT LT_DATA INTO LS_DATA.
      CLEAR LS_CEPCT.
      IF LS_DATA-SPRAS IS INITIAL.
        LS_DATA-SPRAS = LS_DATA-SPRAS_T001.
      ENDIF.
      IF LS_DATA-SPRAS IS NOT INITIAL.
        READ TABLE LT_CEPCT INTO LS_CEPCT
          WITH KEY SPRAS = LS_DATA-SPRAS
                   PRCTR = LS_DATA-PRCTR
                   DATBI = LS_DATA-DATBI
                   KOKRS = LS_DATA-KOKRS
                   BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_DATA-KTEXT = LS_CEPCT-KTEXT.
          LS_DATA-LTEXT = LS_CEPCT-LTEXT.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.
      IF LS_DATA-LTEXT IS INITIAL.
        APPEND LS_DATA TO T_DATA[].
      ENDIF.
    ENDLOOP.
*   Language check according to profit center language
    WHEN 'PRCTR'.
      SELECT *
        FROM CEPC LEFT OUTER JOIN CEPCT ON  CEPC~SPRAS EQ CEPCT~SPRAS
                                        AND CEPC~PRCTR EQ CEPCT~PRCTR
                                        AND CEPC~DATBI EQ CEPCT~DATBI
                                        AND CEPC~KOKRS EQ CEPCT~KOKRS
        INTO CORRESPONDING FIELDS OF TABLE T_DATA
        WHERE CEPC~PRCTR IN R_PRCTR[]
        AND   CEPC~KOKRS IN R_KOKRS[]
        AND   CEPC~DATBI IN R_DATBI[]
        AND   CEPC~DATAB IN R_DATAB[]
        AND   CEPC~ERSDA IN R_ERSDA[]
        AND   CEPC~SPRAS IN R_SPRAS[]
        AND   CEPC~SPRAS NE SPACE.
    WHEN OTHERS.
  ENDCASE.
  IF T_DATA[] IS NOT INITIAL.
    SORT T_DATA[] BY LTEXT.
    DELETE T_DATA[] WHERE LTEXT IS NOT INITIAL.
  ENDIF.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
