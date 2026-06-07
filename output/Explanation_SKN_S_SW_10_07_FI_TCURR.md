# Exception Indicator: FI exchange rates update status ( SW_10_07_FI_TCURR)

## General Overview

This Exception Indicator watches table TCURR for exchange-rate rows whose last-change timing or inverted validity dates fall outside the monitoring window you configure, so treasury and accounting teams can spot stale or unusually updated rates before they affect pricing and valuation.

This EI serves as an essential control for exchange-rate governance by:
- Surfacing rate rows that remain outside agreed freshness or validity rules for key rate types and currency pairs
- Giving operations a filtered view by rate type, from-currency, to-currency, and quoted rate without manual table browsing
- Supporting cut-over and hypercare periods when rate loads must be verified quickly across systems
- Complementing standard rate maintenance with automated checks tied to calendar windows and optional UTC-normalized clocks
- Providing evidence for audits that critical conversion factors were supervised on a schedule

Typical use includes month-end valuation preparation, post-migration rate validation, and investigations after market events. Results are intended for exception queues rather than bulk operational reporting.

The routine reads standard exchange-rate table data and enriches rows with descriptive rate-type text and a day-based age measure before alerting when any rows remain in scope.


## Problem Description

Failure to monitor exchange-rate master updates against agreed freshness and validity rules creates multiple risks across financial valuation, operational control, and regulatory readiness.

**Financial and Reporting Risks**
- Outdated or missing rates can silently distort margin, inventory valuation, and statutory reporting until discovered late in close
- Incorrect from-to currency pairs may propagate through dependent pricing and tax logic without timely detection
- Sudden market moves make stale rates financially material even when technical posting still succeeds

**Operational and Master Data Risks**
- Manual spot checks of TCURR do not scale when many rate types and currency pairs are in scope across entities
- Inconsistent interpretation of “as of” dates versus inverted validity dates can hide the same issue in different reports
- Distributed teams may update rates on different clocks, making UTC versus local evaluation a recurring blind spot

**Management Visibility Risks**
- Treasury and finance leadership lack a compact signal when rate maintenance lags policy expectations
- Shared service centers cannot prioritize remediation without a repeatable exception list tied to rate type and currency dimensions

## Suggested Resolution

**Immediate Response**
- Review each flagged row for rate type, currency pair, quoted rate, and validity or inverted date context before changing production rates
- Confirm whether the row reflects an authorized manual override, a batch load defect, or an expected market lag per policy
- Coordinate with treasury when valuation-sensitive currencies appear in the exception set

**System Assessment**
- Compare this cycle to the prior one after transports, batch jobs, or interface changes that feed rate maintenance
- Examine concentrations by rate type or currency pair to see whether one integration path drives most findings
- Validate whether UTC versus local evaluation still matches how your landscape schedules rate updates

**Corrective Actions**
- Correct erroneous rates through your standard exchange-rate maintenance process with required approvals
- Adjust monitoring windows or duration thresholds after root cause so the queue stays actionable
- Document outcomes for audit trails and route systemic interface issues into defect or change management when needed


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 2 | DATUM | Datum | CHAR | 50 | 0 | DATUM | DATUM |
| 3 | DURATION_D | Duration D | INT4 | 10 | 0 | DURATION_D | DURATION_D |
| 4 | FCURR | Fcurr | CHAR | 50 | 0 | FCURR | FCURR |
| 5 | GDATU | Gdatu | CHAR | 50 | 0 | GDATU | GDATU |
| 6 | KURST | Kurst | CHAR | 50 | 0 | KURST | KURST |
| 7 | LANGU | Langu | CHAR | 1 | 0 | LANGU | LANGU |
| 8 | MANAGE_IN_UTC | Manage In Utc | CHAR | 1 | 0 | MANAGE_IN_UTC | MANAGE_IN_UTC |
| 9 | SW_DEST | Sw Dest | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 10 | TCURR | Tcurr | CHAR | 50 | 0 | TCURR | TCURR |
| 11 | UKURS | Ukurs | CHAR | 50 | 0 | UKURS | UKURS |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 11 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**DATUM** (Datum)

Explains why two monitoring passes differ: only the pass with stricter datum on DATUM surfaces the disputed rows.

**DURATION_D** (Duration D)

<mark>Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in Days</mark>

**FCURR** (Fcurr)

Aligns exception volume with the chosen scope by testing fcurr via FCURR before alert evaluation.

**GDATU** (Gdatu)

Improves readability of exported lists because gdatu (GDATU) columns stay aligned with the configured filter intent.

**KURST** (Kurst)

Reflects real administration where kurst on KURST is routinely restricted to a single productive client or object family.

**LANGU** (Langu)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** (Manage In Utc)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**SW_DEST** (Sw Dest)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TCURR** (Tcurr)

When tightened, tcurr (TCURR) removes rows that would otherwise dilute attention from failing or stuck cases.

**UKURS** (Ukurs)

Pairs with duration logic: once UKURS passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.


### Parameter Relationships

How parameter combinations work together

**Explicit calendar window versus relative lookback:** **DATUM** supplies explicit validity-date bounds when populated. When **DATUM** is empty, **BACKDAYS** drives how far back the monitor builds the default date range before TCURR rows are selected.

**Inverted validity dates:** **GDATU** works with the date-inversion helper in the routine so the inverted validity date used in selection stays aligned with the calendar window you expressed through **DATUM** or **BACKDAYS**.

**Age filter after selection:** **DURATION_D** is an additional day-based age filter applied after rows are read: each rate line must still fit the configured day-count band relative to the evaluation clock before it remains in the alert population.

**UTC versus local evaluation:** **MANAGE_IN_UTC** shifts whether the evaluation clock used with **DATUM** and duration math follows UTC semantics versus local application-server time, keeping calendar and duration results consistent with how your landscape runs the monitor.

**Remote execution path:** When **SW_DEST** is supplied, the monitor can execute the check in the connected system context; rate-type, currency, and amount filters only narrow results once that path is active.

**Final selection:** Both the date window logic (explicit **DATUM** or **BACKDAYS**) and the **DURATION_D** age test must be satisfied together with currency and rate-type filters before a row is treated as part of the final exception set.


### Default Values

- **BACKDAYS** - initial - treated as 0 for same-day lower bound by code

### Practical Example of Parameter Configuration

**Use Case 1: Core rate types for month-end**

**Purpose:** Watch the main corporate rate type and key hard-currency pairs during close week.
```
KURST = M
FCURR = USD
TCURR = EUR
BACKDAYS = 10
DATUM = 20250401 - 20250430
MANAGE_IN_UTC = X
```

**Use Case 2: Quoted-rate band with duration cap**

**Purpose:** Flag rows where the quoted rate is extreme and the line is older than a few days in day units.
```
UKURS = 0.5 - 2.0
DURATION_D = 0 - 5
BACKDAYS = 14
LANGU = E
```

**Use Case 3: Validity window and remote check**

**Purpose:** Anchor to a maintenance weekend and run the check on the productive finance destination.
```
GDATU = 20250328 - 20250330
KURST = EURX
FCURR = EUR
TCURR = GBP
SW_DEST = PROD_FIN
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_07_FI_TCURR | BACKDAYS | BACKDAYS | INT4(10) | BACKDAYS |
| /SKN/S_SW_10_07_FI_TCURR | DATUM | DATUM | CHAR(50) | DATUM |
| /SKN/S_SW_10_07_FI_TCURR | DURATION_D | DURATION_D | INT4(10) | DURATION_D |
| /SKN/S_SW_10_07_FI_TCURR | FCURR | FCURR | CHAR(50) | FCURR |
| /SKN/S_SW_10_07_FI_TCURR | GDATU | GDATU | CHAR(50) | GDATU |
| /SKN/S_SW_10_07_FI_TCURR | KURST | KURST | CHAR(50) | KURST |
| /SKN/S_SW_10_07_FI_TCURR | LANGU | LANGU | CHAR(1) | LANGU |
| /SKN/S_SW_10_07_FI_TCURR | MANAGE_IN_UTC | MANAGE_IN_UTC | CHAR(1) | MANAGE_IN_UTC |
| /SKN/S_SW_10_07_FI_TCURR | TCURR | TCURR | CHAR(50) | TCURR |
| /SKN/S_SW_10_07_FI_TCURR | UKURS | UKURS | CHAR(50) | UKURS |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_07_FI_TCURR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_07_FI_TCURR
*"----------------------------------------------------------------------
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
DATA_SINGLE: MANAGE_IN_UTC       CHAR1 ,
             LANGU               LANGU,
             BACKDAYS            INT4.
DATA_MULTY:   KURST             KURST_CURR,
              FCURR             FCURR_CURR,
              TCURR             TCURR_CURR,
              UKURS             UKURS_CURR,
              DURATION_D       /SKN/E_SW_DURATION_D,
              DATUM            SYDATUM . " Paased by SW Online Monitor
  SELECT_MULTY: KURST,
                FCURR,
                TCURR,
                UKURS,
                DURATION_D,
                DATUM.
   LV_LANGU = SY-LANGU.
   SELECT_SINGLE: MANAGE_IN_UTC,
                  LANGU,
                  BACKDAYS.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_07_FI_TCURR'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA : DATE_FROM TYPE D,
         BACKDAYS  TYPE I.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : LS_DATA LIKE LINE OF T_DATA.
  DATA : LT_DATA LIKE TABLE OF LS_DATA.
  DATA : TIME_DIFF TYPE I .
  DATA : SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
  DATA : DOMVALUE LIKE  DD07V-DOMVALUE_L,
         DDTEXT LIKE  DD07V-DDTEXT.
DATA_MULTY: GDATU  GDATU_INV.
  "-----------------------------------------------
  " 2. Extracting & Populating Parameters        "
  "-----------------------------------------------
    IF R_DATUM[] IS INITIAL .  " Set default value
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      DATE_FROM = SY-DATUM - LV_BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
    ENDIF .
    "--- Fill GDATU
    DATE_2_INVERTED_RANGE DATUM GDATU.
*    refresh R_GDATU.
*    loop at R_DATUM into RS_DATUM.
*      move-corresponding RS_DATUM to RS_GDATU.
*      date_2_inverted RS_DATUM-LOW RS_GDATU-LOW.
*      if RS_DATUM-HIGH is not initial.
*        date_2_inverted RS_DATUM-HIGH RS_GDATU-HIGH.
*      endif.
*      append RS_GDATU to R_GDATU.
*    endloop.
   SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
   TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  REFRESH LT_DATA .
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert Data           "
  "-----------------------------------------------
  "--- Check that Object Class is not empty
  SELECT *
     FROM TCURR
     INTO CORRESPONDING FIELDS OF TABLE T_DATA
     WHERE KURST IN R_KURST
       AND FCURR IN   R_FCURR
       AND TCURR IN   R_TCURR
       AND GDATU IN   R_GDATU
       AND UKURS IN   R_UKURS.
  "-----------------------------------------------
  " 5. Post retrieving manipulations             "
  "-----------------------------------------------
  "-----------------------------------------------
  " 6. Post retrieving filtering                 "
  "-----------------------------------------------
  LOOP AT T_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    DATE_2_INVERTED  LS_DATA-GDATU LS_DATA-ACT_DATE.
         CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
            EXPORTING
              D_FROM          = LS_DATA-ACT_DATE
              T_FROM          = SY-UZEIT
              D_TO            = SY_DATLO
              T_TO            = SY_TIMLO
              TIME_UNIT        = 'D'
            IMPORTING
              TIME_DIFF        = TIME_DIFF
            EXCEPTIONS
              WRONG_VALUE      = 1
              OTHERS           = 2 .
         IF SY-SUBRC = 0.
           LS_DATA-DURATION_D = TIME_DIFF .
         ELSE.
           LS_DATA-DURATION_D = '999999' .
         ENDIF.
    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
*
 DELETE T_DATA WHERE DURATION_D NOT IN R_DURATION_D.
 "
 LOOP AT T_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
  CALL FUNCTION '/SKN/F_SW_10_EXCHRATE_TYP_DESC'
    EXPORTING
      KURST                   = LS_DATA-KURST
      LANGU                   = SY-LANGU
  IMPORTING
      EXCHRATE_TYP_DESC       = LS_DATA-EXCHRATE_TYP_DESC
  EXCEPTIONS
      WRONG_CODE              = 1
  OTHERS                  = 2
          .
 IF SY-SUBRC <> 0.
* Implement suitable error handling here
 ENDIF.
    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
 ENDLOOP.
  "-----------------------------------------------
  " 7. Finishing (Set IS_ALERT parameter)        "
  "-----------------------------------------------
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
