# Exception Indicator: Check Vendor's Partners - SW_10_06_PF_VENDOR

## General Overview

This Exception Indicator (EI) detects vendor–purchasing relationships where expected SD partner functions are not maintained on the purchasing-side link, based on LFM1/LFM2 and WYT3 partner assignments.

This EI helps by:
- Highlighting vendors that lack configured partner roles on purchasing org (and plant/subrange) records when those roles are required by policy
- Combining master-data filters (vendor, org, country, account group, creation window) with a configurable partner-role checklist
- Returning one output row per required partner role still missing after evaluating purchasing data and partner text

The function loads candidate vendors from purchasing tables, expands the configured partner-role list per language, and raises an alert when any required role is absent for a vendor org (or plant/subrange) slice.


## Problem Description

Incomplete partner functions on vendor purchasing data weaken order-to-party traceability and can break downstream logistics, invoice, or compliance checks when the business assumes standard SD partner roles exist.

**Operational and Process Risks**
- Orders or deliveries may reference vendors without the expected ordering or goods-supplier partner roles
- Plant-level purchasing variants can hide missing partners when only central vendor data is reviewed
- Partner gaps may stay undetected until a document fails in SD or logistics

**Control and Compliance Risks**
- Segregation or audit evidence can weaken when partner roles do not match real trading relationships
- Cross-border or regulated scenarios may require demonstrable partner completeness on purchasing masters

**Management Visibility Risks**
- Portfolio-level hygiene is harder without automated comparison of required versus maintained partner roles

### Suggested Resolution

**Immediate Response**
- Review alerted vendor and purchasing organization (and plant/subrange) combinations first
- Validate which partner roles are mandatory for the account group and country in scope
- Correct WYT3 partner assignments or master data maintenance gaps

**System Assessment**
- Confirm PARVW selection matches corporate partner-role policy
- Validate BACKDAYS, DURATION, and DURATION_UNIT behavior against the intended vendor creation-date window
- Reconcile LFM1 versus LFM2 paths if plant-level partners are in scope

**Corrective Actions**
- Standardize partner-role templates per purchasing organization and plant
- Add governance for vendor master changes that affect partner functions
- Track recurring alerts by KTOKK, LAND1, and EKORG to prioritize master-data quality work


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 4 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 5 | DURATION_D | Duration In Days |  | 0 | 0 |  |  |
| 6 | DURATION_UNIT | Duration Unit |  | 0 | 0 |  |  |
| 7 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 8 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 9 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 10 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 11 | LANGU | Language |  | 0 | 0 |  |  |
| 12 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 13 | LTSNR | Vendor Subrange | CHAR | 6 | 0 | LTSNR | LTSNR |
| 14 | NAME1 | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 15 | PARVW | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 16 | VTEXT | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 17 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 17 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Backdays):

BACKDAYS defines the older boundary of the vendor master creation-date window when no explicit ERDAT range is supplied, so selection still has a finite historical span.

**BUKRS** (Company Code):

BUKRS restricts purchasing-organization rows to those whose controlling company code matches the configured corporate-entity filter.

**BUTXT** (Company Name):

BUTXT carries the company-code short text from T001 so result rows remain readable when multiple company codes appear in one alert population.

**DURATION** (Duration In Time Units):

DURATION works with BACKDAYS to bound the vendor creation-date interval from the recent side, shrinking the evaluated master-creation window when no explicit date range is passed.

**DURATION_D** (Duration In Days):

DURATION_D supplies a day-based duration input where the UI or integration maps calendar days before the duration logic is combined with BACKDAYS for the creation-date window.

**DURATION_UNIT** (Duration Unit):

DURATION_UNIT states the calendar unit assumed for DURATION/DURATION_D so the same numeric value is not misread as hours versus days in partner-gap evaluation.

**EKORG** (Purch. Organization):

EKORG limits LFM1/LFM2 sourcing links to the purchasing organizations you scope for partner-role completeness checks.

**EKOTX** (Description):

EKOTX surfaces the purchasing-organization description from T024E for analysts who work from names rather than four-character org codes.

**KTOKK** (Account group):

KTOKK filters vendors by account-group policy so partner checks focus on externally relevant supplier categories.

**LAND1** (Country Key):

LAND1 narrows vendors by country of origin from LFA1 to align alerts with regional sourcing or sanctions review scope.

**LANGU** (Language):

LANGU selects the text language used when resolving partner-function descriptions from TPART/TPAUM so VTEXT aligns with the logon or reporting language.

**LIFNR** (Vendor):

LIFNR targets specific vendor accounts when the control is run as a focused audit instead of a broad portfolio sweep.

**LTSNR** (Vendor Subrange):

LTSNR scopes vendor subranges on LFM2 rows so plant-/subrange-level partner assignments are included or excluded consistently with the organizational slice.

**NAME1** (Name 1):

NAME1 is the vendor master name carried on the output row for identification; it is not a substitute filter for LIFNR but helps human review of expanded partner lines.

**PARVW** (Partner Function):

PARVW lists which SD partner roles (for example ordering address or goods supplier) must be present on the purchasing relationship for the vendor to be considered complete.

**VTEXT** (Name):

VTEXT stores the resolved partner-function description text after language-specific lookup, separate from NAME1 which is the vendor master name.

**WERKS** (Plant):

WERKS ties evaluation to plant-specific purchasing data on LFM2 so missing partner roles are detected per plant where plant-level sourcing applies.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering


### Parameter Relationship

How parameter combinations work together

Multi-value parameters (PARVW, LIFNR, EKORG, LAND1, KTOKK, BUKRS) narrow which purchasing rows enter evaluation; they combine with logical AND against the LFM1/LFM2 selection so each additional filter shrinks the population.

BACKDAYS and DURATION (with DURATION_UNIT or DURATION_D as mapped by the integration) define how the vendor master creation-date range is built when no explicit ERDAT range is supplied: the interval is bounded from recent history using those values together.

LTSNR and WERKS align the evaluation path with subrange and plant-level LFM2 data; when they are filled, plant-specific partner gaps are visible instead of only org-level LFM1 links.

LANGU controls how partner role text (VTEXT) is resolved for the expanded PARVW list so descriptions match the same language used for TPART/TPAUM reads.


### Default Values

- **LANGU** - falls back to the current system logon language when left initial
- **BACKDAYS** - used only when no explicit vendor creation-date range is provided
- **DURATION** - used with BACKDAYS to cap the recent edge of that creation-date window when no explicit range is provided
- **DURATION_UNIT** - H

### Practical Example of Parameter Configuration

**Use Case 1: Org-level partner completeness for EU vendors**

**Purpose:** Detect EU vendors in selected purchasing orgs that are missing ordering-party or goods-supplier partner roles on the purchasing record.

```
PARVW = AG / LF
EKORG = 1000 / 2000
LAND1 = DE / FR
BACKDAYS = 3650
DURATION = 0
DURATION_UNIT = D
LANGU = EN
```

**Use Case 2: Focused vendor with plant-level scope**

**Purpose:** Check one vendor across a plant where LFM2 applies and subrange keys may be populated.

```
LIFNR = 0000123456
EKORG = 1000
WERKS = 0001
PARVW = WE
KTOKK = ZVEN
BUKRS = 1000
BACKDAYS = 1825
DURATION = 30
DURATION_UNIT = D
```


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_PF_VENDOR | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_PF_VENDOR | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_PF_VENDOR | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_06_PF_VENDOR | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_06_PF_VENDOR | KTOKK | Vendor account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_06_PF_VENDOR | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_PF_VENDOR | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_PF_VENDOR | LTSNR | Vendor Subrange | CHAR(6) | LTSNR |
| /SKN/S_SW_10_06_PF_VENDOR | NAME1 | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_06_PF_VENDOR | PARVW | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_06_PF_VENDOR | VTEXT | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_PF_VENDOR | WERKS | Plant | CHAR(4) | WERKS_D |

The ABAP source for this Exception Indicator is shown below.

```abap
FUNCTION /SKN/F_SW_10_06_PF_VENDOR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_PF_VENDOR
*"----------------------------------------------------------------------
  DATA_SINGLE: SW_DEST   RFCDEST,
               LANGU  LANGU,      """langu     char2, Tanya 21/1/19
               BACKDAYS  INT4,
               DURATION  INT4.
  DATA_MULTY: PARVW PARVW,
              LIFNR LIFNR,
              EKORG EKORG,
              LAND1 LAND1,
              KTOKK KTOKK,
              BUKRS BUKRS,
              ERDAT ERDAT.
  SELECT_MULTY: PARVW,
                LIFNR,
                EKORG,
                LAND1,
                KTOKK,
                BUKRS,
                ERDAT.
  SELECT_SINGLE: SW_DEST, LANGU, BACKDAYS, DURATION.
  CONVERT_MULTY: """parvw alpha,
                 LIFNR ALPHA.
 CONVERT_MULTY:  PARVW PARVW. """Tanya 14/11/18
  DATA: LS_ERDAT LIKE LINE OF R_ERDAT.
  IF R_ERDAT[] IS INITIAL AND LV_BACKDAYS IS NOT INITIAL.
    LS_ERDAT-SIGN  = 'I'.
    LS_ERDAT-OPTION = 'BT'.
    LS_ERDAT-LOW = SY-DATUM - LV_BACKDAYS.
    LS_ERDAT-HIGH = SY-DATUM - LV_DURATION.
    APPEND LS_ERDAT TO R_ERDAT.
  ENDIF.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_PF_VENDOR'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
***  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
***    EXPORTING
***      input            = lv_langu
***    IMPORTING
***      output           = lv_langu_INT
***    EXCEPTIONS
***      unknown_language = 1
***      OTHERS           = 2.
  IF LV_LANGU IS INITIAL.
    LV_LANGU = SY-LANGU.
  ENDIF.
  CHECK R_PARVW[] IS NOT INITIAL.
  TYPES: BEGIN OF TY_PARTNER,
      PARVW TYPE TPAUM-PARVW,
      PABEZ TYPE TPAUM-PABEZ,
      VTEXT TYPE TPART-VTEXT,
    END OF TY_PARTNER,
    TT_PARTNER TYPE STANDARD TABLE OF TY_PARTNER.
  DATA: LT_PARTNER   TYPE TT_PARTNER,
        LS_PARTNER   TYPE TY_PARTNER,
        LT_PARTNER_T TYPE TT_PARTNER,
        LT_TPART     TYPE STANDARD TABLE OF TPART,
        LS_TPART     TYPE TPART,
        LT_DATA      TYPE STANDARD TABLE OF /SKN/S_SW_10_06_PF_VENDOR,
        LT_DATA2     TYPE STANDARD TABLE OF /SKN/S_SW_10_06_PF_VENDOR,
        LS_DATA      TYPE /SKN/S_SW_10_06_PF_VENDOR,
        LS_PREV      TYPE /SKN/S_SW_10_06_PF_VENDOR,
        LS_PARVW     LIKE LINE OF R_PARVW.
  FIELD-SYMBOLS: <LS_PARTNER> TYPE TY_PARTNER.
  SELECT TPAUM~PARVW TPAUM~PABEZ
    FROM TPAUM
    INTO TABLE LT_PARTNER
    WHERE TPAUM~SPRAS EQ LV_LANGU
    AND   TPAUM~PABEZ IN R_PARVW.
  LOOP AT R_PARVW INTO LS_PARVW.
    READ TABLE LT_PARTNER WITH KEY PABEZ = LS_PARVW-LOW
    TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      LS_PARTNER-PABEZ = LS_PARVW-LOW.
      LS_PARTNER-PARVW = LS_PARVW-LOW.
      APPEND LS_PARTNER TO LT_PARTNER.
    ENDIF.
  ENDLOOP.
  IF LT_PARTNER[] IS NOT INITIAL.
    SELECT PARVW VTEXT FROM TPART
      INTO CORRESPONDING FIELDS OF TABLE LT_TPART
      FOR ALL ENTRIES IN LT_PARTNER
      WHERE SPRAS EQ LV_LANGU
      AND   PARVW EQ LT_PARTNER-PARVW.
    SORT LT_TPART BY PARVW.
    LOOP AT LT_PARTNER ASSIGNING <LS_PARTNER>.
      READ TABLE LT_TPART INTO LS_TPART
      WITH KEY PARVW = <LS_PARTNER>-PARVW BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <LS_PARTNER>-VTEXT = LS_TPART-VTEXT.
      ENDIF.
    ENDLOOP.
    SORT LT_PARTNER BY PARVW.
  ENDIF.
  CHECK LT_PARTNER[] IS NOT INITIAL.
  SELECT LFM1~LIFNR LFM1~EKORG WYT3~LTSNR WYT3~WERKS
    WYT3~PARVW
    LFA1~NAME1 LFA1~LAND1 LFA1~KTOKK
    T024E~EKOTX T024E~BUKRS T001~BUTXT
    FROM LFM1
    LEFT OUTER JOIN WYT3 ON WYT3~LIFNR  EQ LFM1~LIFNR
                        AND WYT3~EKORG  EQ LFM1~EKORG
*                        AND wyt3~ltsnr  EQ space
*                        AND wyt3~werks  EQ space
    INNER JOIN LFA1      ON LFA1~LIFNR  EQ LFM1~LIFNR
    INNER JOIN T024E     ON T024E~EKORG EQ LFM1~EKORG
    LEFT OUTER JOIN T001 ON T001~BUKRS  EQ T024E~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE LFM1~LIFNR IN R_LIFNR
    AND   LFM1~EKORG IN R_EKORG
    AND   LFA1~LAND1 IN R_LAND1
    AND   LFA1~KTOKK IN R_KTOKK
    AND   LFA1~ERDAT IN R_ERDAT
    AND   T024E~BUKRS IN R_BUKRS.
*    ORDER BY lfm1~lifnr lfm1~ekorg wyt3~parvw.
  DELETE LT_DATA WHERE LTSNR IS NOT INITIAL OR WERKS IS NOT INITIAL.
  SELECT LFM2~LIFNR LFM2~EKORG LFM2~LTSNR LFM2~WERKS
    WYT3~PARVW
    LFA1~NAME1 LFA1~LAND1 LFA1~KTOKK
    T024E~EKOTX T024E~BUKRS T001~BUTXT
    FROM LFM2
    LEFT OUTER JOIN WYT3 ON WYT3~LIFNR  EQ LFM2~LIFNR
                        AND WYT3~EKORG  EQ LFM2~EKORG
                        AND WYT3~LTSNR  EQ LFM2~LTSNR
                        AND WYT3~WERKS  EQ LFM2~WERKS
    INNER JOIN LFA1      ON LFA1~LIFNR  EQ LFM2~LIFNR
    INNER JOIN T024E     ON T024E~EKORG EQ LFM2~EKORG
    LEFT OUTER JOIN T001 ON T001~BUKRS  EQ T024E~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA2
    WHERE LFM2~LIFNR IN R_LIFNR
    AND   LFM2~EKORG IN R_EKORG
    AND   LFA1~LAND1 IN R_LAND1
    AND   LFA1~KTOKK IN R_KTOKK
    AND   LFA1~ERDAT IN R_ERDAT
    AND   T024E~BUKRS IN R_BUKRS.
*    ORDER BY lfm2~lifnr lfm2~ekorg lfm2~ltsnr lfm2~werks wyt3~parvw.
  APPEND LINES OF LT_DATA2 TO LT_DATA.
  SORT LT_DATA BY LIFNR EKORG LTSNR WERKS PARVW.
  CHECK LT_DATA IS NOT INITIAL.
  READ TABLE LT_DATA INTO LS_PREV INDEX 1.
  LT_PARTNER_T[] = LT_PARTNER[].
  LOOP AT LT_DATA INTO LS_DATA.
    IF LS_PREV-LIFNR NE LS_DATA-LIFNR
    OR LS_PREV-EKORG NE LS_DATA-EKORG
    OR LS_PREV-LTSNR NE LS_DATA-LTSNR
    OR LS_PREV-WERKS NE LS_DATA-WERKS.
      LOOP AT LT_PARTNER_T INTO LS_PARTNER.
        LS_PREV-PARVW = LS_PARTNER-PARVW.
        LS_PREV-VTEXT = LS_PARTNER-VTEXT.
        APPEND LS_PREV TO T_DATA.
      ENDLOOP.
      LT_PARTNER_T[] = LT_PARTNER[].
    ENDIF.
    DELETE LT_PARTNER_T WHERE PARVW EQ LS_DATA-PARVW.
    LS_PREV = LS_DATA.
  ENDLOOP.
  LOOP AT LT_PARTNER_T INTO LS_PARTNER.
    LS_PREV-PARVW = LS_PARTNER-PARVW.
    LS_PREV-VTEXT = LS_PARTNER-VTEXT.
    APPEND LS_PREV TO T_DATA.
  ENDLOOP.
  IF T_DATA[] IS NOT INITIAL.
    IS_ALERT = ABAP_TRUE.
  ENDIF.
ENDFUNCTION.
```
