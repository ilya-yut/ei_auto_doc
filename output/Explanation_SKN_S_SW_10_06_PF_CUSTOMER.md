# Exception Indicator: Check Customer's Partners ( SW_10_06_PF_CUSTOMER)

## General Overview

This Exception Indicator detects customer sales-area relationships where expected SD partner functions are not maintained on the customer master link, based on KNVV/KNVP partner assignments.

This EI helps by:
- Highlighting customers that lack configured partner roles on sales organization, distribution channel, and division when those roles are required by policy
- Combining master-data filters (customer, sales org, account group, classification, company code, creation window) with a configurable partner-role checklist
- Returning one output row per required partner role still missing after evaluating sales data and partner text

The function loads candidate customers from **KNVV** with **KNVP** partners, expands the configured partner-role list per language from **TPAUM**/**TPART**, and raises an alert when any required role is absent for a customer sales-area slice.


## Problem Description

Incomplete partner functions on customer sales data weaken order-to-party traceability and can break downstream logistics, billing, or compliance checks when the business assumes standard SD partner roles exist.

**Operational and Process Risks**
- Sales orders or deliveries may reference customers without the expected sold-to, ship-to, or payer partner roles
- Sales-area variants can hide missing partners when only general customer data is reviewed
- Partner gaps may stay undetected until a document fails in SD or logistics

**Control and Compliance Risks**
- Segregation or audit evidence can weaken when partner roles do not match real trading relationships
- Cross-border or regulated scenarios may require demonstrable partner completeness on customer masters

**Management Visibility Risks**
- Portfolio-level hygiene is harder without automated comparison of required versus maintained partner roles

## Suggested Resolution

**Immediate Response**
- Review alerted customer and sales organization, distribution channel, and division combinations first
- Validate which partner roles are mandatory for the account group and classification in scope
- Correct **KNVP** partner assignments or master data maintenance gaps

**System Assessment**
- Confirm **PARVW** selection matches corporate partner-role policy
- Validate **BACKDAYS** and **DURATION** behavior against the intended customer creation-date window on **KNVV-ERDAT**
- Reconcile language settings via **LANGU** so **VTEXT** descriptions match reporting language

**Corrective Actions**
- Standardize partner-role templates per sales organization and channel
- Add governance for customer master changes that affect partner functions
- Track recurring alerts by **KTOKD**, **KUKLA**, and **VKORG** to prioritize master-data quality work


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS |
| 3 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |
| 4 | KTOKD | Customer Account Group | CHAR | 4 | 0 | KTOKD |
| 5 | KUKLA | Customer Classification | CHAR | 2 | 0 | KUKLA |
| 6 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR |
| 7 | LANGU | Language | CHAR | 1 | 0 | SPRAS |
| 8 | PARVW | Partner Function | CHAR | 2 | 0 | PARVW |
| 9 | SPART | Division | CHAR | 2 | 0 | SPART |
| 10 | SW_DEST | RFC Destination | CHAR | 32 | 0 | RFCDEST |
| 11 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG |
| 12 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 12 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERDAT

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**KTOKD** (Customer Account Group)

Customer Account Group determines the screen layout, number range, and field rules when creating a customer master record.

**KUKLA** (Customer Classification)

Customer Classification categorizes a customer into specific groups for marketing, sales analysis, or statistical reporting.

**KUNNR** (Customer)

Customer account and is used to scope records to specific customers across SD/FI flows.

**LANGU** (Language)

Language key used for language-dependent texts and user-language filtering.

**PARVW** (Partner Function)

SD partner function such as sold-to, ship-to, or payer defining partner roles on documents.

**SPART** (Division)

Division key used for SD product-line segmentation.

**SW_DEST** (RFC Destination)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.


### Parameter Relationships

**Partner role checklist:** **PARVW** lists which SD partner roles must be present. The routine expands each configured role with text from **TPAUM**/**TPART** in **LANGU** and emits one output line per missing role per customer sales-area slice.

**Sales-area scope:** **KUNNR**, **VKORG**, **VTWEG**, and **SPART** narrow which **KNVV** rows are evaluated. **KTOKD** and **KUKLA** filter via **KNA1**. **BUKRS** limits rows through **TVKO** / **T001**.

**Creation-date window:** When no explicit creation-date range is supplied, **BACKDAYS** and **DURATION** together bound **KNVV-ERDAT** from `today − BACKDAYS` through `today − DURATION`.

**Language and descriptions:** **LANGU** controls partner-function text (**VTEXT**) on missing-role output lines.

**Execution path:** **SW_DEST** selects cloud versus on-premise execution; all other parameters apply to the on-premise path before any cloud handoff.


### Default Values

- **LANGU** - initial - treated as SY-LANGU by code
- **BACKDAYS** - initial - treated as unconstrained by code
- **DURATION** - initial - treated as 0 by code

### Practical Example of Parameter Configuration

**Use Case 1: Sales-area partner completeness**

**Purpose:** Detect customers in sales org 1000 that are missing sold-to or ship-to partner roles on the sales-area record.

```
PARVW = AG
PARVW = WE
VKORG = 1000
BACKDAYS = 3650
DURATION = 0
LANGU = E
```

**Use Case 2: Focused customer audit**

**Purpose:** Check one customer across a sales organization and channel.

```
KUNNR = 0000100001
VKORG = 1000
VTWEG = 10
SPART = 00
PARVW = RG
KTOKD = DEBI
```

**Use Case 3: Company code and classification scope**

**Purpose:** Limit review to domestic account groups in selected company codes.

```
BUKRS = 1000
KTOKD = DEBI
KUKLA = 01
PARVW = AG
BACKDAYS = 90
DURATION = 0
```

**Use Case 4: Recently created customers**

**Purpose:** Review customers created in the last 30 days for missing partner roles.

```
VKORG = 2000
PARVW = WE
BACKDAYS = 30
DURATION = 0
LANGU = E
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_PF_CUSTOMER | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_PF_CUSTOMER | BUTXT | Company Name | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_PF_CUSTOMER | KTOKD | Account group | CHAR(4) | KTOKD |
| /SKN/S_SW_10_06_PF_CUSTOMER | KUKLA | Customer classific. | CHAR(2) | KUKLA |
| /SKN/S_SW_10_06_PF_CUSTOMER | KUNNR | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_06_PF_CUSTOMER | NAME1 | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_06_PF_CUSTOMER | PARVW | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_06_PF_CUSTOMER | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_06_PF_CUSTOMER | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_06_PF_CUSTOMER | VTEXS | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_PF_CUSTOMER | VTEXT | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_PF_CUSTOMER | VTEXV | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_PF_CUSTOMER | VTEXW | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_PF_CUSTOMER | VTWEG | Distribution Channel | CHAR(2) | VTWEG |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_PF_CUSTOMER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_PF_CUSTOMER
*"----------------------------------------------------------------------
  DATA_SINGLE: SW_DEST   RFCDEST,
               LANGU  LANGU,      """langu     char2, Tanya 21/1/19
               BACKDAYS  INT4,
               DURATION  INT4.
  DATA_MULTY: PARVW PARVW,
              KUNNR KUNNR,
              VKORG VKORG,
              VTWEG VTWEG,
              SPART SPART,
              KTOKD KTOKD,
              KUKLA KUKLA,
              BUKRS BUKRS,
              ERDAT ERDAT.
  SELECT_MULTY: PARVW,
                KUNNR,
                VKORG,
                VTWEG,
                SPART,
                KTOKD,
                KUKLA,
                BUKRS.
  SELECT_SINGLE: SW_DEST, LANGU, BACKDAYS, DURATION.
  CONVERT_MULTY: PARVW ALPHA,
                 KUNNR ALPHA,
                 VKORG ALPHA,
                 VTWEG ALPHA,
                 SPART ALPHA,
                 KTOKD ALPHA,
                 KUKLA ALPHA,
                 BUKRS ALPHA.
  CONVERT_MULTY:  PARVW PARVW . """Tanya 14/11/18
  DATA: LS_ERDAT LIKE LINE OF R_ERDAT.
  IF R_ERDAT[] IS INITIAL AND LV_BACKDAYS IS NOT INITIAL.
    LS_ERDAT-SIGN  = 'I'.
    LS_ERDAT-OPTION = 'BT'.
    LS_ERDAT-LOW = SY-DATUM - LV_BACKDAYS.
    LS_ERDAT-HIGH = SY-DATUM - LV_DURATION.
    APPEND LS_ERDAT TO R_ERDAT.
  ENDIF.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_PF_CUSTOMER'
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
        LT_DATA      TYPE STANDARD TABLE OF /SKN/S_SW_10_06_PF_CUSTOMER,
        LS_DATA      TYPE /SKN/S_SW_10_06_PF_CUSTOMER,
        LS_PREV      TYPE /SKN/S_SW_10_06_PF_CUSTOMER,
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
  SELECT KNVV~KUNNR KNVV~VKORG KNVV~VTWEG KNVV~SPART
    KNVP~PARVW
    KNA1~NAME1 KNA1~KTOKD KNA1~KUKLA
    TVKOT~VTEXT AS VTEXV TVTWT~VTEXT AS VTEXW
    TSPAT~VTEXT AS VTEXS
    T001~BUKRS T001~BUTXT
    FROM KNVV
    LEFT OUTER JOIN KNVP ON KNVP~KUNNR EQ KNVV~KUNNR
                        AND KNVP~VKORG EQ KNVV~VKORG
                        AND KNVP~VTWEG EQ KNVV~VTWEG
                        AND KNVP~SPART EQ KNVV~SPART
    INNER JOIN KNA1 ON KNA1~KUNNR EQ KNVV~KUNNR
    LEFT OUTER JOIN TVKOT ON TVKOT~SPRAS EQ LV_LANGU
                         AND TVKOT~VKORG EQ KNVV~VKORG
    LEFT OUTER JOIN TVTWT ON TVTWT~SPRAS EQ LV_LANGU
                         AND TVTWT~VTWEG EQ KNVV~VTWEG
    LEFT OUTER JOIN TSPAT ON TSPAT~SPRAS EQ LV_LANGU
                         AND TSPAT~SPART EQ KNVV~SPART
    INNER JOIN TVKO ON TVKO~VKORG EQ KNVV~VKORG
    LEFT OUTER JOIN T001 ON T001~BUKRS EQ TVKO~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE KNVV~KUNNR IN R_KUNNR
    AND   KNVV~VKORG IN R_VKORG
    AND   KNVV~VTWEG IN R_VTWEG
    AND   KNVV~SPART IN R_SPART
    AND   KNVV~ERDAT IN R_ERDAT
    AND   KNA1~KTOKD IN R_KTOKD
    AND   KNA1~KUKLA IN R_KUKLA
    AND   TVKO~BUKRS IN R_BUKRS
    ORDER BY KNVV~KUNNR KNVV~VKORG KNVV~VTWEG KNVV~SPART KNVP~PARVW.
  CHECK LT_DATA IS NOT INITIAL.
  READ TABLE LT_DATA INTO LS_PREV INDEX 1.
  LT_PARTNER_T[] = LT_PARTNER[].
  LOOP AT LT_DATA INTO LS_DATA.
    IF LS_PREV-KUNNR NE LS_DATA-KUNNR
    OR LS_PREV-VKORG NE LS_DATA-VKORG
    OR LS_PREV-VTWEG NE LS_DATA-VTWEG
    OR LS_PREV-SPART NE LS_DATA-SPART.
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
