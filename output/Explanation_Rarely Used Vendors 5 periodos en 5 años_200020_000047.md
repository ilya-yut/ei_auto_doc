# Exception Indicator: Rarely used vendors - SW_10_07_ACT_VEND_CO

## General Overview

This Exception Indicator (EI) monitors vendors with very low operational use across a long horizon and highlights dormant-vendor exposure with financial relevance.

This EI helps by:
- Detecting vendors active in only a small number of periods
- Segmenting low-use patterns by company code and purchasing organization
- Combining activity-count logic with amount and balance thresholds
- Supporting vendor lifecycle and dormant-master governance

The function derives period activity from accounting/vendor tables, computes period counters, and returns vendors that match rare-usage criteria.


## Problem Description

Vendors that remain rarely used over years can create hidden control, fraud, and data-quality risk if not reviewed with objective activity criteria.

**Operational and Process Risks**
- Dormant vendors can be reused unexpectedly without strong business need
- Low-frequency transactions may avoid day-to-day monitoring attention
- Legacy vendor records can remain active longer than justified

**Control and Compliance Risks**
- Dormant-vendor governance can weaken if periodic reviews are not enforced
- Low-usage vendors with balances may create residual financial exposure
- Inconsistent inactivity thresholds can reduce audit defensibility

**Management Visibility Risks**
- Vendor hygiene deterioration may be discovered late
- Prioritization of cleanup/remediation is harder without count-based evidence

### Suggested Resolution

**Immediate Response**
- Review low-counter vendors with meaningful balances first
- Validate current business justification for rarely used vendor masters
- Escalate high-risk candidates for restriction, cleanup, or remediation

**System Assessment**
- Validate period horizon and counter thresholds against policy
- Compare patterns by company code and purchasing organization
- Reconcile block/deletion flags with observed activity behavior

**Corrective Actions**
- Standardize dormant-vendor review cadence and ownership
- Tighten lifecycle controls (block/archive/delete)
- Track trend metrics for recurring low-activity vendor profiles


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AMOUNT | Credit | CURR | 15 | 2 | UMXXH | WRTV8 |
| 2 | AUSBK | Source company code | CHAR | 4 | 0 | AUSBK | BUKRS |
| 3 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 4 | BALANCE_NORMAL | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 5 | BALANCE_NORMAL_FR | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 6 | BALANCE_SPEC | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 7 | BALANCE_SPEC_FR | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 8 | BALANCE_TOTAL | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 9 | BALANCE_TOTAL_FR | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 10 | BELNR | Document Number | CHAR | 10 | 0 | BELNR_D | BELNR |
| 11 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 12 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 13 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 14 | COUNTER | Counter for active periods | INT4 | 10 | 0 | INT4 | INT4 |
| 15 | DMBTR | Amt.in loc.cur. | CURR | 13 | 2 | DMBTR | WERT7 |
| 16 | DMBTR_FR | Amt.in loc.cur. | CURR | 13 | 2 | DMBTR | WERT7 |
| 17 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 18 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 19 | ERDAT | Created on | DATS | 8 | 0 | ERDAT_RF | DATUM |
| 20 | FABKL | Factory calendar | CHAR | 2 | 0 | FABKL | WFCID |
| 21 | FACDATE | Factory date | DEC | 5 | 0 | FACDATE | HFDATE |
| 22 | FDGRV | Planning group | CHAR | 10 | 0 | FDGRV | FDGRP |
| 23 | GJAHR | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 24 | KONZS | Group key | CHAR | 10 | 0 | KONZS | KONZS |
| 25 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 26 | LFA1_LOEVM | Central deletion flag | CHAR | 1 | 0 | LOEVM_X | XFELD |
| 27 | LFA1_SPERM | Central purchasing block | CHAR | 1 | 0 | SPERM_X | XFELD |
| 28 | LFA1_SPERR | Central posting block | CHAR | 1 | 0 | SPERB_X | XFELD |
| 29 | LFB1_LOEVM | Delete flag for purchasing organization | CHAR | 1 | 0 | LOEVM_M | XFELD |
| 30 | LFB1_SPERR | Posting block for company code | CHAR | 1 | 0 | SPERB_B | XFELD |
| 31 | LFM1_LOEVM | Delete flag for purchasing organization | CHAR | 1 | 0 | LOEVM_M | XFELD |
| 32 | LFM1_SPERM | Purch. block for purchasing organization | CHAR | 1 | 0 | SPERM_M | XFELD |
| 33 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 34 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 35 | PARKED_IVNOICE | Parked Invoice Ind. | CHAR | 1 | 0 | /SKN/E_SW_PARK_INV |  |
| 36 | PERIOD | Active Period | CHAR | 7 | 0 | /SKN/E_SW_ACT_PER |  |
| 37 | POSSIBLE_APP | App Description | CHAR | 200 | 0 | /SKN/E_SW_APP_DESC | /SKN/D_SW_APP_DESC |
| 38 | UMSAV | Balance Carryforward | CURR | 15 | 2 | UMSAV | WRTV8 |
| 39 | UMSAV_FR | Balance Carryforward | CURR | 15 | 2 | UMSAV | WRTV8 |
| 40 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 41 | WAERS_FR | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 42 | WORKING_DAYS | Flag | CHAR | 1 | 0 | CIND | CHAR1 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 42 parameters listed in the Parameters Reference Table above.

**AMOUNT** (Credit):

AMOUNT reflects transaction amount context used to evaluate whether low activity still carries material value impact.

**AUSBK** (Source company code):

AUSBK captures relevant account/bank assignment context that helps classify financial exposure characteristics for the vendor.

**BACKDAYS** (Backdays):

BACKDAYS sets lookback length and therefore controls how far the period-activity model reaches into history.

**BALANCE_NORMAL** (Balance Amount):

BALANCE_NORMAL represents normal-balance component used in total exposure calculation for low-usage vendors.

**BALANCE_NORMAL_FR** (Balance Amount):

BALANCE_NORMAL_FR stores converted normal-balance values in the target currency for comparable thresholding.

**BALANCE_SPEC** (Balance Amount):

BALANCE_SPEC represents special G/L balance component used to assess non-standard exposure.

**BALANCE_SPEC_FR** (Balance Amount):

BALANCE_SPEC_FR stores converted special-balance values for consistent risk filtering.

**BALANCE_TOTAL** (Balance Amount):

BALANCE_TOTAL combines normal and special balance components to express total open exposure.

**BALANCE_TOTAL_FR** (Balance Amount):

BALANCE_TOTAL_FR is the converted total balance used directly by balance-threshold filtering logic.

**BELNR** (Document Number):

BELNR links evaluation to accounting document identity for drill-down and evidence tracing.

**BUDAT** (Posting Date):

BUDAT provides posting-date context for period assignment and transaction chronology checks.

**BUKRS** (Company Code):

BUKRS scopes results to selected company codes so dormant-vendor risk can be compared per entity.

**BUTXT** (Company Name):

BUTXT adds company-code descriptive context to improve readability of entity-level findings.

**COUNTER** (Counter for active periods):

COUNTER is the primary rarity threshold: it defines the allowed number of active periods in the observation window.

**DMBTR** (Amt.in loc.cur.):

DMBTR captures local-currency amount basis used in amount and exposure derivations.

**DMBTR_FR** (Amt.in loc.cur.):

DMBTR_FR carries converted amount values so cross-company comparisons use a consistent currency basis.

**EKORG** (Purch. Organization):

EKORG narrows scope by purchasing organization to isolate procurement-driven vendor activity behavior.

**EKOTX** (Description):

EKOTX adds purchasing-organization descriptive context for clearer operational interpretation.

**ERDAT** (Created on):

ERDAT constrains vendor-creation date and supports lifecycle-stage filtering in inactivity analysis.

**FABKL** (Factory calendar):

FABKL provides factory-calendar related context that can affect working-day interpretation in periodic logic.

**FACDATE** (Factory date):

FACDATE represents factory-calendar date context used when working-day based interpretations are applied.

**FDGRV** (Planning group):

FDGRV filters planning-group assignment to align results with treasury/payment planning segmentation.

**GJAHR** (Fiscal Year):

GJAHR scopes fiscal-year context used by period aggregation and counter computation.

**KONZS** (Group key):

KONZS filters assignment groups, enabling governance segmentation across vendor portfolios.

**KTOKK** (Account group):

KTOKK filters account groups to focus on relevant vendor categories for dormant-risk review.

**LFA1_LOEVM** (Central deletion flag):

LFA1_LOEVM applies central deletion-flag filtering for master-level lifecycle governance.

**LFA1_SPERM** (Central purchasing block):

LFA1_SPERM applies central purchasing-block status filtering.

**LFA1_SPERR** (Central posting block):

LFA1_SPERR applies central posting-block status filtering.

**LFB1_LOEVM** (Delete flag for company code view):

LFB1_LOEVM applies company-code deletion status filtering for finance-view inactive vendors.

**LFB1_SPERR** (Posting block for company code):

LFB1_SPERR applies company-code posting-block filtering to isolate restricted FI vendors.

**LFM1_LOEVM** (Delete flag for purchasing organization view):

LFM1_LOEVM applies purchasing-organization deletion filtering to identify procurement-side inactive vendors.

**LFM1_SPERM** (Purch. block for purchasing organization):

LFM1_SPERM applies purchasing-organization block filtering to isolate sourcing-restricted vendors.

**LIFNR** (Vendor):

LIFNR targets specific vendor accounts for focused investigation and remediation workflows.

**NAME1** (Name):

NAME1 provides vendor-name readability for business review and action ownership.

**PARKED_IVNOICE** (Parked Invoice Ind.):

PARKED_IVNOICE indicates parked-invoice context relevant to low-usage anomaly interpretation.

**PERIOD** (Active Period):

PERIOD stores derived period labels used in activity-count evidence and reporting.

**POSSIBLE_APP** (App Description):

POSSIBLE_APP indicates possible application/usage pattern classification output of the model.

**UMSAV** (Balance Carryforward):

UMSAV represents carry-forward/turnover balance component used in annual amount aggregation.

**UMSAV_FR** (Balance Carryforward):

UMSAV_FR stores converted UMSAV values for consistent cross-currency comparison.

**WAERS** (Currency):

WAERS is source currency context for local balances and amounts.

**WAERS_FR** (Currency):

WAERS_FR is target conversion currency for financial-threshold comparison (code default: USD).

**WORKING_DAYS** (Flag):

WORKING_DAYS captures business-day activity context to distinguish occasional use from sustained use patterns.


### Parameter Relationship

How parameter combinations work together

**Scope Definition Layer:**

- **LIFNR**, **BUKRS**, and **EKORG** define the vendor population and organization scope.
- **KTOKK**, **KONZS**, and **FDGRV** refine category/planning segmentation inside that scope.

**Rare-Usage Core Logic:**

- **BACKDAYS** and date/fiscal fields (for example **DATUM**, **BUDAT**, **GJAHR**) establish the analysis horizon.
- Period activity is derived and aggregated, then compared against **COUNTER**.
- Vendors are retained only when activity-period counts satisfy configured rarity thresholds.

**Financial Materiality Layer:**

- Amount and balance families (**AMOUNT**, **DMBTR***, **BALANCE_***, **UMSAV*** ) determine whether low usage is financially meaningful.
- **WAERS** with **WAERS_FR** controls currency normalization for consistent threshold checks.

**Governance Status Layer:**

- **LFA1_***, **LFB1_***, and **LFM1_*** flags combine central, company-code, and purchasing restrictions/deletion indicators.


### Default Values
- **BACKDAYS** - 1 (today and yesterday)
- **WAERS_FR** - USD
- **COUNTER** - 0

### Practical Example of Parameter Configuration
**Use Case 1: Dormant vendors with meaningful exposure**

```plaintext
BACKDAYS = 1825
COUNTER = 0 - 2
BALANCE_TOTAL_FR = 1000 - 999999999
WAERS_FR = USD
```

**Purpose:** Detect vendors with very low usage over five years but still carrying material financial exposure.

**Use Case 2: Entity and procurement focused hygiene check**

```plaintext
BUKRS = 1000
EKORG = P100
COUNTER = 0 - 1
LFA1_LOEVM = 
LFM1_LOEVM = 
```

**Purpose:** Review vendors that remain almost unused in one organization scope while validating lifecycle flags.

**Use Case 3: Newer vendors with weak utilization**

```plaintext
ERDAT = 20240101-20260331
COUNTER = 0 - 1
AMOUNT = 0 - 5000
KTOKK = Z001
```

**Purpose:** Identify recently created vendors with minimal operational usage for governance and cleanup decisions.


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_INACT_VEND | AMOUNT | Total of the Credit Postings for the Month | CURR(15,2) | UMXXH |
| /SKN/S_SW_10_06_INACT_VEND | AUSBK | Source Company Code | CHAR(4) | AUSBK |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_NORMAL | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_NORMAL_FR | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_SPEC | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_SPEC_FR | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_TOTAL | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_TOTAL_FR | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BELNR | Accounting Document Number | CHAR(10) | BELNR_D |
| /SKN/S_SW_10_06_INACT_VEND | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_06_INACT_VEND | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_INACT_VEND | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_INACT_VEND | COUNTER | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_06_INACT_VEND | DMBTR | Amount in Local Currency | CURR(13,2) | DMBTR |
| /SKN/S_SW_10_06_INACT_VEND | DMBTR_FR | Amount in Local Currency | CURR(13,2) | DMBTR |
| /SKN/S_SW_10_06_INACT_VEND | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_06_INACT_VEND | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_06_INACT_VEND | ERDAT | Date on which the Record Was Created | DATS(8) | ERDAT_RF |
| /SKN/S_SW_10_06_INACT_VEND | FABKL | Factory calendar key | CHAR(2) | FABKL |
| /SKN/S_SW_10_06_INACT_VEND | FACDATE | Factory calendar: Factory date | DEC(5) | FACDATE |
| /SKN/S_SW_10_06_INACT_VEND | FDGRV | Planning group | CHAR(10) | FDGRV |
| /SKN/S_SW_10_06_INACT_VEND | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /SKN/S_SW_10_06_INACT_VEND | KONZS | Group key | CHAR(10) | KONZS |
| /SKN/S_SW_10_06_INACT_VEND | KTOKK | Vendor account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_06_INACT_VEND | LFA1_LOEVM | Central Deletion Flag for Master Record | CHAR(1) | LOEVM_X |
| /SKN/S_SW_10_06_INACT_VEND | LFA1_SPERM | Centrally imposed purchasing block | CHAR(1) | SPERM_X |
| /SKN/S_SW_10_06_INACT_VEND | LFA1_SPERR | Central posting block | CHAR(1) | SPERB_X |
| /SKN/S_SW_10_06_INACT_VEND | LFB1_LOEVM | Delete flag for vendor at purchasing level | CHAR(1) | LOEVM_M |
| /SKN/S_SW_10_06_INACT_VEND | LFB1_SPERR | Posting block for company code | CHAR(1) | SPERB_B |
| /SKN/S_SW_10_06_INACT_VEND | LFM1_LOEVM | Delete flag for vendor at purchasing level | CHAR(1) | LOEVM_M |
| /SKN/S_SW_10_06_INACT_VEND | LFM1_SPERM | Purchasing block at purchasing organization level | CHAR(1) | SPERM_M |
| /SKN/S_SW_10_06_INACT_VEND | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_INACT_VEND | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_INACT_VEND | PARKED_IVNOICE | Parking Invoice Ind. | CHAR(1) | /SKN/E_SW_PARK_INV |
| /SKN/S_SW_10_06_INACT_VEND | PERIOD | Active Period | CHAR(7) | /SKN/E_SW_ACT_PER |
| /SKN/S_SW_10_06_INACT_VEND | POSSIBLE_APP | SW: App Description | CHAR(200) | /SKN/E_SW_APP_DESC |
| /SKN/S_SW_10_06_INACT_VEND | UMSAV | Balance Carried Forward in Local Currency | CURR(15,2) | UMSAV |
| /SKN/S_SW_10_06_INACT_VEND | UMSAV_FR | Balance Carried Forward in Local Currency | CURR(15,2) | UMSAV |
| /SKN/S_SW_10_06_INACT_VEND | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_06_INACT_VEND | WAERS_FR | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_06_INACT_VEND | WORKING_DAYS | Fatory calendar flag | CHAR(1) | CIND |

## ABAP Code

```abap
  FUNCTION /SKN/F_SW_10_07_ACT_VEND_COUNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_INACT_VEND OPTIONAL
*"----------------------------------------------------------------------
    DATA_SINGLE: SW_DEST       RFCDEST,
                 BACKDAYS      INT4,
                 LFA1_SPERR    SPERB_X,
                 LFA1_SPERM    SPERM_X,
                 LFA1_LOEVM    LOEVM_X,
                 LFB1_SPERR    SPERB_B,
                 LFB1_LOEVM    LOEVM_M,
                 LFM1_SPERM    SPERM_M,
                 LFM1_LOEVM    LOEVM_M,
                 WAERS_FR      WAERS.
    DATA_MULTY: LIFNR      LIFNR,
                BUKRS      BUKRS,
                EKORG      EKORG,
                KTOKK      KTOKK,
                KONZS      KONZS,
                FDGRV      FDGRV,
                ERDAT      ERDAT_RF,
                COUNTER    I,
                AMOUNT_MAX DMBTR,
                BALANCE_FR DMBTR,
                DATUM      SY-DATUM,
                GJAHR      GJAHR.
* Set default param.
    LV_BACKDAYS = 1.
    LV_WAERS_FR = 'USD'.
    SELECT_MULTY: LIFNR,
                  BUKRS,
                  EKORG,
                  KTOKK,
                  KONZS,
                  FDGRV,
                  ERDAT,
                  COUNTER,
                  AMOUNT_MAX,
                  BALANCE_FR,
                  DATUM.
    SELECT_SINGLE: SW_DEST,
                   BACKDAYS,
                   LFA1_SPERM,
                   LFA1_LOEVM,
                   LFB1_SPERR,
                   LFB1_LOEVM,
                   LFM1_SPERM,
                   LFM1_LOEVM,
                   WAERS_FR.
    CONVERT_MULTY: BUKRS ALPHA,
                   LIFNR ALPHA.
    TYPES: BEGIN OF TY_T001,
             LIFNR TYPE LFA1-LIFNR,
             BUKRS TYPE T001-BUKRS,
           END OF TY_T001,
           TT_T001 TYPE TABLE OF TY_T001.
    TYPES: BEGIN OF TY_LFC1,
      LIFNR TYPE LFB1-LIFNR,
      BUKRS TYPE LFB1-BUKRS,
      GJAHR TYPE LFC1-GJAHR,
      UMSAV TYPE LFC1-UMSAV,
      UM01S TYPE LFC1-UM01S,
      UM01H TYPE LFC1-UM01H,
      UM02S TYPE LFC1-UM02S,
      UM02H TYPE LFC1-UM02H,
      UM03S TYPE LFC1-UM03S,
      UM03H TYPE LFC1-UM03H,
      UM04S TYPE LFC1-UM04S,
      UM04H TYPE LFC1-UM04H,
      UM05S TYPE LFC1-UM05S,
      UM05H TYPE LFC1-UM05H,
      UM06S TYPE LFC1-UM06S,
      UM06H TYPE LFC1-UM06H,
      UM07S TYPE LFC1-UM07S,
      UM07H TYPE LFC1-UM07H,
      UM08S TYPE LFC1-UM08S,
      UM08H TYPE LFC1-UM08H,
      UM09S TYPE LFC1-UM09S,
      UM09H TYPE LFC1-UM09H,
      UM10S TYPE LFC1-UM10S,
      UM10H TYPE LFC1-UM10H,
      UM11S TYPE LFC1-UM11S,
      UM11H TYPE LFC1-UM11H,
      UM12S TYPE LFC1-UM12S,
      UM12H TYPE LFC1-UM12H,
      WAERS TYPE T001-WAERS,
    END OF TY_LFC1,
    TT_LFC1 TYPE STANDARD TABLE OF TY_LFC1.
    TYPES: BEGIN OF TY_LFC3,
      BUKRS TYPE LFC3-BUKRS,
      LIFNR TYPE LFC3-LIFNR,
      GJAHR TYPE LFC3-GJAHR,
      SHBKZ TYPE LFC3-SHBKZ,
      SALDV TYPE LFC3-SALDV,
      SOLLL TYPE LFC3-SOLLL,
      HABNL TYPE LFC3-HABNL,
      WAERS TYPE T001-WAERS,
    END OF TY_LFC3,
    TT_LFC3 TYPE STANDARD TABLE OF TY_LFC3.
    DATA: LV_START_DATE    TYPE ERDAT,
          LV_START_YEAR    TYPE GJAHR,
          LV_FIRST_YEAR    TYPE GJAHR,
          LV_START_MONTH   TYPE MONTH,
          LV_CURR_YEAR     TYPE GJAHR,
          LV_CURR_MONTH    TYPE MONTH,
          LV_COMP          TYPE CHAR10,
          LV_ACT           TYPE FLAG,
          LV_TABIX         TYPE I,
          LV_WHILE         TYPE STRING,
          LV_QUERY         TYPE STRING,
          LV_VAL           TYPE STRING,
          LV_WHERE         TYPE RFC_DB_OPT,
          LV_CHAR          TYPE CHAR20,
          LV_VALUE_HIGH    TYPE STRING VALUE '''0.00''',
          LV_VALUE         TYPE STRING VALUE '''0.00''',
          LV_COUNTER_TMP   TYPE I,
          LV_COUNTER_TOTAL TYPE I,
          LV_LINES         TYPE I,
          LV_LIFNR         TYPE LFC1-LIFNR,
          LV_BUKRS         TYPE LFC1-BUKRS,
          LV_COUNTER       TYPE I,
          LV_TOTAL_NORMAL  TYPE ZWRBTR,
          LV_TOTAL_SPEC    TYPE ZWRBTR,
          LV_TOTAL         TYPE ZWRBTR,
          LV_TOTAL_FR      TYPE ZWRBTR,
          LV_DEBIT         TYPE SOLLL,
          LV_PERIOD        TYPE NUMC5,
          LV_ZERO          TYPE I VALUE 0,
          LV_CREDIT        TYPE HABNL.
    DATA: LS_DATA     LIKE LINE OF T_DATA[],
          LS_DATA_TMP TYPE /SKN/S_SW_10_06_INACT_VEND,
          LS_LFC1     TYPE TY_LFC1,
          LS_LFC3     TYPE TY_LFC3,
          LS_T001     TYPE TY_T001.
    DATA: LT_DATA      LIKE TABLE OF T_DATA,
          LT_DATA_TMP  TYPE STANDARD TABLE OF /SKN/S_SW_10_06_INACT_VEND,
          LT_DATA_TMP2 TYPE STANDARD TABLE OF /SKN/S_SW_10_06_INACT_VEND,
          LT_LFC1      TYPE TT_LFC1,
          LT_LFC1_TMP  TYPE TT_LFC1,
          LT_QUERY     TYPE TABLE OF RFC_DB_OPT,
          LT_LFC3      TYPE TT_LFC3,
          LT_T001      TYPE TT_T001.
    DATA: BACKDAYS  TYPE I,
          DATE_FROM LIKE SY-DATUM,
          DATE_TO   LIKE SY-DATUM,
          REF_DATE  TYPE D.
    DATA: TIME_DIFF TYPE  INT4 .
    DATA: FLD(60) TYPE C.
    FIELD-SYMBOLS: <FS_VAL>  TYPE ANY,
                   <FS_DATA> LIKE LINE OF T_DATA[].
* if sw_dest is empty then on premise, else on cloud
    IF LV_SW_DEST IS NOT INITIAL.
      CALL FUNCTION '/SKN/FC_SW_10_07_ACT_VEND_COUN'
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
    CONVERT_MULTY: LIFNR ALPHA.
    IF R_BUKRS IS INITIAL.
      SELECT LFA1~LIFNR T001~BUKRS LFA1~ERDAT
        FROM LFA1 INNER JOIN LFB1 ON LFA1~LIFNR EQ LFB1~LIFNR
                  INNER JOIN T001 ON LFB1~BUKRS EQ T001~BUKRS
        INTO  TABLE LT_T001
        WHERE LFA1~LIFNR IN R_LIFNR[].
      IF LT_T001 IS NOT INITIAL.
        SORT LT_T001 BY BUKRS.
        DELETE ADJACENT DUPLICATES FROM LT_T001 COMPARING BUKRS.
        LOOP AT LT_T001 INTO LS_T001.
          RS_BUKRS-SIGN   = 'I'.
          RS_BUKRS-OPTION = 'EQ'.
          RS_BUKRS-LOW    = LS_T001-BUKRS.
          APPEND RS_BUKRS TO R_BUKRS.
        ENDLOOP.
        SORT R_BUKRS BY LOW.
      ENDIF.
    ENDIF.
    LV_START_DATE  = SY-DATUM - LV_BACKDAYS.
    LV_START_YEAR  = LV_START_DATE(4).
    LV_START_MONTH = LV_START_DATE+4(2).
    LV_CURR_YEAR  = SY-DATUM(4).
    LV_CURR_MONTH = SY-DATUM+4(2).
    LV_START_DATE  = SY-DATUM - LV_BACKDAYS.
    LV_START_YEAR  = LV_START_DATE(4).
    LV_FIRST_YEAR  = LV_START_YEAR.
    LV_START_MONTH = LV_START_DATE+4(2).
* Set Fiscal Year range for main condition
    REFRESH R_GJAHR.
    CLEAR RS_GJAHR.
    RS_GJAHR-SIGN   = 'I'.
    RS_GJAHR-OPTION = 'BT'.
    RS_GJAHR-LOW    = LV_START_YEAR.
    RS_GJAHR-HIGH   = LV_CURR_YEAR.
    APPEND RS_GJAHR TO R_GJAHR[].
*** 07.06.21++
* Set default counter
    IF R_COUNTER IS INITIAL.
      RS_COUNTER-SIGN   = 'I'.
      RS_COUNTER-OPTION = 'EQ'.
      RS_COUNTER-LOW    =  0.
      APPEND RS_COUNTER TO R_COUNTER.
    ENDIF.
*** 07.06.21++
* Set Vendor Creation date
    REFRESH R_ERDAT.
    CLEAR RS_ERDAT.
    RS_ERDAT-SIGN   = 'I'.
*** 28.06.22--
*** 07.06.21++
*    IF lv_zero IN r_counter[].
*      rs_erdat-option = 'LE'.
*    ELSE.
**** 07.06.21++
*      rs_erdat-option = 'GE'.
*    ENDIF.
*** 28.06.22--
    RS_ERDAT-OPTION = 'LE'.   " 28.06.22++
    RS_ERDAT-LOW    = LV_START_DATE.
    APPEND RS_ERDAT TO R_ERDAT[].
    SELECT LFB1~LIFNR LFB1~BUKRS LFB1~FDGRV LFB1~SPERR AS LFB1_SPERR LFB1~LOEVM AS LFB1_LOEVM
           LFM1~EKORG LFM1~SPERM AS LFM1_SPERM LFM1~LOEVM AS LFM1_LOEVM
           LFA1~NAME1 LFA1~KONZS LFA1~KTOKK LFA1~SPERR AS LFA1_SPERR LFA1~SPERM AS LFA1_SPERM
           LFA1~LOEVM AS LFA1_LOEVM LFA1~KTOKK LFA1~KONZS
           T001~BUTXT
      FROM LFB1 INNER JOIN LFA1      ON LFB1~LIFNR EQ LFA1~LIFNR
                INNER JOIN T001      ON LFB1~BUKRS EQ T001~BUKRS
                LEFT OUTER JOIN LFM1 ON LFA1~LIFNR EQ LFM1~LIFNR
      INTO CORRESPONDING FIELDS OF TABLE LT_DATA
      WHERE LFB1~LIFNR  IN R_LIFNR[]
      AND   LFB1~BUKRS  IN R_BUKRS[]
      AND   LFB1~SPERR  EQ LV_LFB1_SPERR
      AND   LFB1~LOEVM  EQ LV_LFB1_LOEVM
      AND   LFA1~ERDAT  IN R_ERDAT[]
      AND   LFA1~SPERR  EQ LV_LFA1_SPERR
      AND   LFA1~SPERM  EQ LV_LFA1_SPERM
      AND   LFA1~LOEVM  EQ LV_LFA1_LOEVM.
    IF LT_DATA IS NOT INITIAL.
      SORT LT_DATA[] BY LIFNR BUKRS.
      DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING LIFNR BUKRS.
    ENDIF.
    CHECK LT_DATA IS NOT INITIAL.
    READ TABLE R_COUNTER INTO RS_COUNTER INDEX 1.
    IF LT_DATA IS INITIAL.
      EXIT.
    ENDIF.
*
    IF LT_DATA IS NOT INITIAL.
      SELECT LFC1~LIFNR LFC1~BUKRS LFC1~GJAHR
             LFC1~UMSAV
             LFC1~UM01S LFC1~UM01H LFC1~UM02S LFC1~UM02H LFC1~UM03S LFC1~UM03H
             LFC1~UM04S LFC1~UM04H LFC1~UM05S LFC1~UM05H LFC1~UM06S LFC1~UM06H
             LFC1~UM07S LFC1~UM07H LFC1~UM08S LFC1~UM08H LFC1~UM09S LFC1~UM09H
             LFC1~UM10S LFC1~UM10H LFC1~UM11S LFC1~UM11H LFC1~UM12S LFC1~UM12H
        FROM LFC1 LEFT OUTER JOIN T001 ON LFC1~BUKRS EQ T001~BUKRS
        INTO CORRESPONDING FIELDS OF TABLE LT_LFC1
        FOR ALL ENTRIES IN LT_DATA[]
        WHERE LFC1~LIFNR EQ LT_DATA-LIFNR
        AND   LFC1~BUKRS EQ LT_DATA-BUKRS
        AND   LFC1~GJAHR IN R_GJAHR[].
    ENDIF.
    CHECK LT_LFC1 IS NOT INITIAL.
*** Yuri C.++ 06.01.19
    SORT LT_LFC1 BY LIFNR BUKRS.
    IF RS_COUNTER-LOW > 0 OR RS_COUNTER-HIGH > 0.
      SORT LT_LFC1 BY LIFNR BUKRS GJAHR.
      SORT LT_DATA BY LIFNR BUKRS.
      LOOP AT LT_LFC1 INTO LS_LFC1.
        LV_TABIX = SY-TABIX.
        CLEAR: LV_COUNTER_TMP, LT_DATA_TMP.
        READ TABLE LT_DATA ASSIGNING <FS_DATA> WITH KEY LIFNR = LS_LFC1-LIFNR
                                                        BUKRS = LS_LFC1-BUKRS
                                                        BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          IF LV_START_YEAR EQ LS_LFC1-GJAHR.
            LV_WHILE      = LV_START_MONTH.
            LV_CURR_MONTH = LV_START_MONTH.
          ELSE.
            LV_CURR_MONTH = 1.
            LV_WHILE      = 1.
          ENDIF.
          WHILE LV_WHILE <= 12.
            IF LV_LIFNR <> LS_LFC1-LIFNR OR LV_BUKRS <> LS_LFC1-BUKRS.
              CLEAR: LV_COUNTER.
              DESCRIBE TABLE LT_DATA_TMP LINES LV_COUNTER.
              IF LV_COUNTER IN R_COUNTER[] AND LT_DATA_TMP IS NOT INITIAL.
                LOOP AT LT_DATA_TMP ASSIGNING <FS_DATA>.
                  <FS_DATA>-COUNTER = LV_COUNTER.
                ENDLOOP.
                APPEND LINES OF LT_DATA_TMP TO LT_DATA_TMP2.
                CLEAR: LT_DATA_TMP.
              ENDIF.
              CLEAR: LV_COUNTER_TOTAL.
              LV_LIFNR = LS_LFC1-LIFNR.
              LV_BUKRS = LS_LFC1-BUKRS.
            ENDIF.
            CLEAR: LV_COMP.
            CONCATENATE 'UM' LV_CURR_MONTH 'H' INTO LV_COMP.
            ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_LFC1 TO <FS_VAL>.
            IF <FS_VAL> NE 0.
              CLEAR: LS_DATA_TMP-AMOUNT, LS_DATA_TMP-GJAHR, LS_DATA_TMP-WAERS,
                     LS_DATA_TMP-PERIOD.
              LV_COUNTER_TMP = LV_COUNTER_TMP + 1.
              LS_DATA_TMP-GJAHR  = LV_START_YEAR.
**** 28.06.22--
*              CONCATENATE lv_start_month ls_lfc1-gjahr INTO ls_data_tmp-period
*                SEPARATED BY '/'.
**** 28.06.22--
**** 28.06.22++
              CONCATENATE LV_CURR_MONTH LS_LFC1-GJAHR INTO LS_DATA_TMP-PERIOD
                SEPARATED BY '/'.
**** 28.06.22++
              LS_DATA_TMP-AMOUNT = <FS_VAL>.
              LS_DATA_TMP-WAERS  = LS_LFC1-WAERS.
              APPEND LS_DATA_TMP TO LT_DATA_TMP.
            ENDIF.
            LV_WHILE      = LV_WHILE + 1.
            LV_CURR_MONTH = LV_CURR_MONTH + 1.
          ENDWHILE.
          LV_COUNTER_TOTAL = LV_COUNTER_TOTAL + LV_COUNTER_TMP.
          IF LV_COUNTER_TOTAL IN R_COUNTER[] OR LV_COUNTER_TOTAL LT RS_COUNTER-LOW.
            SORT LT_DATA_TMP BY LIFNR BUKRS GJAHR.
            IF LV_LINES EQ LV_TABIX.    " Last Row at LFC1
              DESCRIBE TABLE LT_DATA_TMP LINES LV_COUNTER.
              IF LV_COUNTER IN R_COUNTER[] AND LT_DATA_TMP IS NOT INITIAL.
                LOOP AT LT_DATA_TMP ASSIGNING <FS_DATA>.
                  <FS_DATA>-COUNTER = LV_COUNTER.
                ENDLOOP.
                APPEND LINES OF LT_DATA_TMP TO LT_DATA_TMP2.
                CLEAR: LT_DATA_TMP.
              ENDIF.
            ENDIF.
          ELSE.
            CLEAR: LT_DATA_TMP.
            SORT LT_DATA BY LIFNR BUKRS.
            DELETE LT_DATA WHERE LIFNR EQ LS_LFC1-LIFNR
                           AND   BUKRS EQ LS_LFC1-BUKRS.
          ENDIF.
        ELSE.
          CLEAR: LT_DATA_TMP.
          SORT LT_DATA     BY LIFNR BUKRS.
          DELETE LT_DATA WHERE LIFNR EQ LS_LFC1-LIFNR
                         AND   BUKRS EQ LS_LFC1-BUKRS.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CHECK LT_DATA IS NOT INITIAL.
    IF NOT LV_ZERO IN R_COUNTER.
      DELETE LT_DATA_TMP2 WHERE COUNTER NOT IN R_COUNTER[].
      LT_DATA = LT_DATA_TMP2.
    ENDIF.
    IF LT_DATA[] IS NOT INITIAL.
      SORT LT_DATA BY LIFNR BUKRS.
* Select all vendors related to T_DATA and for current year
      CLEAR: LT_LFC1.
      LV_CURR_YEAR = SY-DATUM(4).
      SELECT LFC1~LIFNR LFC1~BUKRS LFC1~GJAHR LFC1~UMSAV
             LFC1~UM01S LFC1~UM01H LFC1~UM02S LFC1~UM02H LFC1~UM03S LFC1~UM03H
             LFC1~UM04S LFC1~UM04H LFC1~UM05S LFC1~UM05H LFC1~UM06S LFC1~UM06H
             LFC1~UM07S LFC1~UM07H LFC1~UM08S LFC1~UM08H LFC1~UM09S LFC1~UM09H
             LFC1~UM10S LFC1~UM10H LFC1~UM11S LFC1~UM11H LFC1~UM12S LFC1~UM12H
             T001~WAERS
        FROM LFC1 LEFT OUTER JOIN T001 ON LFC1~BUKRS EQ T001~BUKRS
        INTO CORRESPONDING FIELDS OF TABLE LT_LFC1
        FOR ALL ENTRIES IN LT_DATA
        WHERE LFC1~LIFNR EQ LT_DATA-LIFNR
        AND   LFC1~BUKRS EQ LT_DATA-BUKRS
        AND   LFC1~GJAHR IN R_GJAHR[].        "EQ lv_curr_year.
      SELECT LFC3~LIFNR LFC3~BUKRS LFC3~GJAHR LFC3~SHBKZ LFC3~SALDV LFC3~SOLLL LFC3~HABNL
             T001~WAERS
        FROM LFC3 LEFT OUTER JOIN T001 ON LFC3~BUKRS EQ T001~BUKRS
        INTO CORRESPONDING FIELDS OF TABLE LT_LFC3
        FOR ALL ENTRIES IN LT_DATA
        WHERE LFC3~LIFNR EQ LT_DATA-LIFNR
        AND   LFC3~BUKRS EQ LT_DATA-BUKRS
        AND   LFC3~GJAHR IN R_GJAHR[].       "EQ lv_curr_year.
      IF SY-SUBRC = 0.
        SORT LT_LFC3 BY LIFNR BUKRS GJAHR.
      ENDIF.
    ENDIF.
    LV_CURR_YEAR = SY-DATUM(4).
    SORT LT_LFC1 BY LIFNR BUKRS GJAHR.
* Calculate debit, credit and balance of current year
    LOOP AT LT_DATA ASSIGNING <FS_DATA>.
****** Description *****************************
****** Description *****************************
      CLEAR: LV_TOTAL_NORMAL, LV_TOTAL_SPEC, LS_LFC1, LS_LFC3, LV_TOTAL.
* LFC3
* Calculate balance of spec. G/L
      LOOP AT LT_LFC3 INTO LS_LFC3 WHERE LIFNR EQ <FS_DATA>-LIFNR
                                   AND   BUKRS EQ <FS_DATA>-BUKRS
                                   AND   GJAHR EQ LV_CURR_YEAR.
        IF <FS_DATA>-WAERS IS INITIAL.
          <FS_DATA>-WAERS = LS_LFC3-WAERS.
        ENDIF.
        LV_TOTAL_SPEC = LS_LFC3-SALDV + LS_LFC3-SOLLL - LS_LFC3-HABNL.
      ENDLOOP.
      IF LV_WAERS_FR IS NOT INITIAL AND LS_LFC3 IS NOT INITIAL AND
         LV_WAERS_FR NE LS_LFC3-WAERS AND LV_TOTAL_SPEC IS NOT INITIAL.
        CLEAR: LV_TOTAL_FR.
        <FS_DATA>-WAERS_FR = LV_WAERS_FR.
        CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
          EXPORTING
            DATE             = SY-DATUM
            FOREIGN_CURRENCY = LV_WAERS_FR   " 'USD'
            LOCAL_AMOUNT     = LV_TOTAL_SPEC
            LOCAL_CURRENCY   = LS_LFC3-WAERS
          IMPORTING
            FOREIGN_AMOUNT   = LV_TOTAL_FR
          EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND  = 4
            DERIVED_2_TIMES  = 5
            OTHERS           = 6.
      ELSE.
        LV_TOTAL_FR = LV_TOTAL_SPEC.
      ENDIF.
      <FS_DATA>-BALANCE_SPEC    = <FS_DATA>-BALANCE_SPEC    + LV_TOTAL_SPEC.
      <FS_DATA>-BALANCE_SPEC_FR = <FS_DATA>-BALANCE_SPEC_FR + LV_TOTAL_FR.
* LFC1
      LOOP AT LT_LFC1 INTO LS_LFC1 WHERE LIFNR EQ <FS_DATA>-LIFNR
                                   AND   BUKRS EQ <FS_DATA>-BUKRS
                                   AND   GJAHR EQ LV_CURR_YEAR.
        CLEAR: LV_CREDIT, LV_DEBIT.
        IF <FS_DATA>-WAERS IS INITIAL.
          <FS_DATA>-WAERS = LS_LFC1-WAERS.
        ENDIF.
*** Begin 29.03.22--
*        lv_curr_month = 1.
*        lv_while      = 1.
*** End 29.03.22--
*** Begin 29.03.22++
        IF LV_START_YEAR IS NOT INITIAL.
          IF LV_START_YEAR LT SY-DATUM(4).
            LV_CURR_MONTH = 1.
            LV_WHILE      = 1.
          ELSE.
            LV_CURR_MONTH = LV_START_MONTH.
            LV_WHILE      = LV_START_MONTH.
          ENDIF.
        ELSE.
          LV_CURR_MONTH = 1.
          LV_WHILE      = 1.
        ENDIF.
*** End 29.03.22++
        WHILE LV_WHILE GE 12 .
          CONCATENATE 'UM' LV_CURR_MONTH 'H' INTO LV_COMP.
          ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_LFC1 TO <FS_VAL>.
          IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
            LV_CREDIT = LV_CREDIT + <FS_VAL>.
            UNASSIGN <FS_VAL>.
          ENDIF.
          CLEAR: LV_COMP.
          CONCATENATE 'UM' LV_CURR_MONTH 'S' INTO LV_COMP.
          ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_LFC1 TO <FS_VAL>.
          IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
            LV_DEBIT = LV_DEBIT + <FS_VAL>.
            UNASSIGN <FS_VAL>.
          ENDIF.
          ADD 1 TO LV_CURR_MONTH.
          ADD 1 TO LV_WHILE.
        ENDWHILE.
*        lv_total_normal = lv_total_normal + ( ls_lfc1-umsav + lv_credit + lv_debit ).   " 28.06.22--
        LV_TOTAL_NORMAL = LV_TOTAL_NORMAL + ( LS_LFC1-UMSAV + LV_DEBIT - LV_CREDIT ).    " 28.06.22++
      ENDLOOP.
******** 22.02.19
      IF LV_WAERS_FR IS NOT INITIAL AND LS_LFC1 IS NOT INITIAL AND
         LV_WAERS_FR NE LS_LFC1-WAERS AND LV_TOTAL_NORMAL IS NOT INITIAL.
        CLEAR: LV_TOTAL_FR.
        <FS_DATA>-WAERS_FR = LV_WAERS_FR.
        CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
          EXPORTING
            DATE             = SY-DATUM
            FOREIGN_CURRENCY = LV_WAERS_FR   " 'USD'
            LOCAL_AMOUNT     = LV_TOTAL_NORMAL
            LOCAL_CURRENCY   = LS_LFC1-WAERS
          IMPORTING
            FOREIGN_AMOUNT   = LV_TOTAL_FR
          EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND  = 4
            DERIVED_2_TIMES  = 5
            OTHERS           = 6.
      ELSE.
        LV_TOTAL_FR = LV_TOTAL_NORMAL.
      ENDIF.
      <FS_DATA>-BALANCE_NORMAL    = <FS_DATA>-BALANCE_NORMAL    + LV_TOTAL_NORMAL. " Balance total normal   of LFC1
      <FS_DATA>-BALANCE_NORMAL_FR = <FS_DATA>-BALANCE_NORMAL_FR + LV_TOTAL_FR.
* Total balance
      LV_TOTAL = <FS_DATA>-BALANCE_NORMAL + <FS_DATA>-BALANCE_SPEC.
      IF LV_WAERS_FR IS NOT INITIAL AND <FS_DATA>-WAERS IS NOT INITIAL AND
         LV_WAERS_FR NE LS_LFC1-WAERS AND LV_TOTAL IS NOT INITIAL.
        CLEAR: LV_TOTAL_FR.
        <FS_DATA>-WAERS_FR = LV_WAERS_FR.
        CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
          EXPORTING
            DATE             = SY-DATUM
            FOREIGN_CURRENCY = LV_WAERS_FR   " 'USD'
            LOCAL_AMOUNT     = LV_TOTAL
            LOCAL_CURRENCY   = <FS_DATA>-WAERS
          IMPORTING
            FOREIGN_AMOUNT   = LV_TOTAL_FR
          EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND  = 4
            DERIVED_2_TIMES  = 5
            OTHERS           = 6.
      ELSE.
        IF <FS_DATA>-WAERS_FR IS INITIAL.
          <FS_DATA>-WAERS_FR = LV_WAERS_FR.
        ENDIF.
        LV_TOTAL_FR = LV_TOTAL.
      ENDIF.
******** 22.02.19
      <FS_DATA>-BALANCE_TOTAL = LV_TOTAL.
      IF LV_TOTAL_FR IN R_BALANCE_FR[].
        <FS_DATA>-BALANCE_TOTAL_FR = LV_TOTAL_FR.
        APPEND <FS_DATA> TO T_DATA.
      ENDIF.
    ENDLOOP.
    READ TABLE T_DATA INDEX 1.
    CHECK NOT SY-TFILL  IS INITIAL .
    IS_ALERT = 'X' .
  ENDFUNCTION.
```
