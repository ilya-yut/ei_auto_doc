# Exception Indicator: Check Vendor's Partners ( SW_10_06_PF_VENDOR)

## General Overview

This Exception Indicator monitors vendor purchasing relationships to detect when expected partner functions are not maintained on vendor master partner links, based on purchasing organization scope and a configurable partner-role checklist.

This EI serves as an essential control for procurement and vendor master governance by:

- Highlighting vendors where required SD/MM partner roles are absent on the purchasing view or partner assignment
- Supporting accountability for partner completeness before PO processing, invoicing, or logistics handoffs
- Enabling targeted review of recently created vendors within a controlled creation-date window
- Improving audit readiness when partner roles must match organizational purchasing policies
- Reducing operational risk from incomplete vendor partner data across purchasing organizations and countries

The indicator compares configured partner roles against partner records on vendor purchasing data and raises alerts when roles remain missing for a vendor organizational slice.

Partner role descriptions are resolved in the configured language, and results include vendor, purchasing organization, and company context for remediation.


## Problem Description

Incomplete partner functions on vendor purchasing data weaken procurement traceability and can disrupt PO, logistics, or payment flows when standard partner roles are assumed to exist.

**Operational and Process Risks**

- Purchase orders or inbound logistics may reference vendors without expected ordering, invoicing, or goods-supplier partner roles
- Purchasing-organization variants can hide missing partners when only general vendor data is reviewed
- Partner gaps may surface only when a document fails validation in MM or SD integration

**Control and Compliance Risks**

- Audit evidence weakens when partner roles do not reflect actual trading relationships
- Cross-border procurement may require demonstrable partner completeness on vendor masters

**Management Visibility Risks**

- Portfolio-level vendor hygiene is harder without automated comparison of required versus maintained partner roles

## Suggested Resolution

**Immediate Response**

- Review alerted vendor and purchasing organization combinations first
- Validate which partner roles are mandatory for the account group and country in scope
- Correct partner assignments on vendor master partner data where gaps are confirmed

**System Assessment**

- Confirm the partner-role checklist matches corporate purchasing policy
- Validate the creation-date window behavior against the intended vendor creation-date selection
- Reconcile language settings so partner descriptions match the reporting language

**Corrective Actions**

- Standardize partner-role templates per purchasing organization
- Add governance for vendor master changes that affect partner functions
- Track recurring alerts by account group, country, and purchasing organization to prioritize master-data quality work


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 4 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 5 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 6 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 7 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 8 | LANGU | Language |  | 0 | 0 |  |  |
| 9 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 10 | PARVW | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 11 | SW_DEST | RFC Destination |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 11 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERDAT

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**ERDAT** (Created On)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**KTOKK** (Account group)

Account group (customer/vendor) used to segment master data governance rules.

**LAND1** (Country Key)

Country key used for legal/geographic segmentation of business partners or plants.

**LANGU** (Language)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**PARVW** (Partner Function)

SD partner function such as sold-to, ship-to, or payer defining partner roles on documents.

**SW_DEST** (RFC Destination)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.


### Parameter Relationships

**Partner role checklist:** **PARVW** lists which partner roles must be present. The routine expands each configured role with text from partner-function master data in **LANGU** and emits one output line per missing role per vendor purchasing slice.

**Vendor scope:** **LIFNR**, **EKORG**, **LAND1**, **KTOKK**, and **BUKRS** narrow which vendor purchasing records are evaluated. **BUKRS** filters through purchasing-organization company code assignment.

**Creation-date window:** When no explicit creation-date range is supplied, **BACKDAYS** and **DURATION** together bound vendor creation date from `today − BACKDAYS` through `today − DURATION`.

**Language and descriptions:** **LANGU** controls partner-function description text on missing-role output lines.

**Execution path:** **SW_DEST** selects cloud versus on-premise execution; all other parameters apply to the on-premise path before any cloud handoff.


### Default Values

- **LANGU** - initial - treated as SY-LANGU by code
- **BACKDAYS** - initial - treated as unconstrained by code
- **DURATION** - initial - treated as 0 by code

### Practical Example of Parameter Configuration

**Use Case 1: Purchasing-organization partner completeness**

**Purpose:** Detect vendors in purchasing org 1000 that are missing ordering or invoicing partner roles.

```
PARVW = LF
PARVW = RS
EKORG = 1000
BACKDAYS = 3650
DURATION = 0
LANGU = E
```

**Use Case 2: Focused vendor audit**

**Purpose:** Check one vendor across a purchasing organization.

```
LIFNR = 0000100001
EKORG = 1000
PARVW = LF
KTOKK = KRED
BACKDAYS = 3650
DURATION = 0
```

**Use Case 3: Company code and country scope**

**Purpose:** Limit review to domestic vendors in selected company codes.

```
BUKRS = 1000
LAND1 = DE
KTOKK = KRED
PARVW = LF
BACKDAYS = 90
DURATION = 0
```

**Use Case 4: Recently created vendors**

**Purpose:** Review vendors created in the last 30 days for missing partner roles.

```
EKORG = 2000
PARVW = RS
BACKDAYS = 30
DURATION = 0
LANGU = E
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

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

## ABAP Code

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
