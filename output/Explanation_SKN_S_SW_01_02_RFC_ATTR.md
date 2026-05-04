# Exception Indicator: RFC destination attributes - SW_01_02_RFC_ATTR

## General Overview

This Exception Indicator reads RFC destination master data together with attribute timestamps from the join of destination and attribute tables, then enriches each row with a computed duration from last change to the evaluation clock so operations can spot destinations that have been stable or unusually idle for too long.

This EI helps by:
- Giving Basis and integration teams a filtered list of destinations with creator, changer, and change-date context without exporting full tables manually
- Supporting reviews after security or landscape changes when attribute change patterns should be reconciled quickly
- Highlighting destinations whose elapsed time since last modification falls outside configured duration bands after date-oriented selection has already applied
- Providing repeatable monitoring evidence when interface governance expects periodic confirmation of destination metadata freshness
- Complementing standard destination administration by packaging selection, time handling, and duration filtering in one pass

Typical use includes post-migration validation, periodic integration hygiene, and troubleshooting when remote systems were re-pointed or renamed. Teams still use standard RFC destination maintenance when a single destination needs immediate correction.

The routine selects from the destination and attribute join, computes duration using the configured unit relative to the evaluation date and time, and removes rows that do not satisfy the duration interval filter.


## Problem Description

Failure to monitor RFC destination attribute freshness and change patterns creates multiple risks across integration stability, security posture, and control evidence.

**Operational and Integration Risks**
- Stale or orphaned destination metadata can persist until remote calls fail or behave inconsistently in production batches
- Broad extracts hide which destinations actually changed during critical windows such as cutover or certificate rotation
- Without duration-style aging, teams cannot quickly separate recently touched destinations from long-unchanged ones

**Control and Compliance Risks**
- Weak recurring checks weaken evidence that destination metadata was supervised when policy expects periodic attestation
- Delayed detection of unusual inactivity or change bursts increases remediation time when external partners already see symptoms
- Inconsistent review makes it harder to document which landscape segments were validated and when

**Management Visibility Risks**
- Leadership lacks a compact signal of where destination maintenance risk concentrates across systems and naming patterns
- Capacity and prioritization decisions suffer when hotspots are not visible in a single monitoring pass
- Cross-team triage slows when Basis and application support cannot share the same filtered view of problematic rows

## Suggested Resolution

**Immediate Response**
- Review each flagged line for destination identity, short description, and last-change context before changing infrastructure or partner settings
- Open the standard RFC destination and attribute maintenance paths that fit your landscape to validate current definitions and ownership
- Capture when the finding ties to regulated or financially material interfaces so follow-up is prioritized appropriately
- Segment results by naming pattern and change user attributes before drilling into calendar or duration explanations
- Confirm whether the situation is transient noise versus persistent configuration drift

**System Assessment**
- Compare current results to prior monitoring cycles after transports, partner maintenance, or batch jobs that touch destination metadata
- Look for concentration by creator or last changer to see whether the issue is localized to one team or systemic
- Validate that explicit monitoring dates and relative lookback behavior still match the operational calendar you intended
- Examine whether duration thresholds still reflect agreed service windows for “too old” or “too young” destination rows
- Check whether UTC versus local evaluation changes the interpretation of borderline duration results

**Corrective Actions**
- Correct destination definitions or attribute inconsistencies according to SAP guidance, then re-run monitoring to confirm the population returned within tolerance
- Refine monitoring parameters after root cause so benign destinations are excluded without hiding genuine risk
- Document remediation and escalation outcomes when metadata issues affected materially sensitive processes
- Schedule recurring runs during critical business windows and retain exports when audit evidence is required
- Route repeat systemic findings into defect or change management when landscape-wide corrections are required


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 2 | CUDATE | Created On | DATS | 8 | 0 | RDIR_CDATE | RDIR_CDATE |
| 3 | CUNAME | Created By User Name | CHAR | 12 | 0 | CREUSRNAME | CREUSRNAME |
| 4 | DATUM | Date |  | 0 | 0 |  |  |
| 5 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 6 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 7 | LANGU | Language | LANG | 1 | 0 | LANGU | LANGU |
| 8 | MANAGE_IN_UTC | Manage In UTC | CHAR | 1 | 0 |  |  |
| 9 | MUDATE | Last Changed On | DATS | 8 | 0 | RDIR_UDATE | RDIR_UDATE |
| 10 | MUNAME | Changed By User Name | CHAR | 12 | 0 | MODUSRNAME | MODUSRNAME |
| 11 | RFCDEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 11 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Back Days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**CUDATE** (Created On)

Separates cross-client noise from in-scope work when created on on CUDATE correlates with client or user attributes.

**CUNAME** (Created By User Name)

Reflects real administration where created by user name on CUNAME is routinely restricted to a single productive client or object family.

**DATUM** (Date)

When left open per framework rules, DATUM does not restrict date; when set, only matching rows remain.

**DURATION** (Duration In Time Units)

Interprets duration in time units as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on DURATION.

**DURATION_UNIT** (Duration Unit(D/H/M))

Unit for elapsed time between each session's creation date and time and the evaluation clock.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**LANGU** (Language)

Stabilizes week-over-week metrics by fixing language (LANGU) while allowing duration thresholds to move.

**MANAGE_IN_UTC** (Manage In UTC)

Controls whether reference timestamps for filtering and duration checks are interpreted in UTC or local time.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MUDATE** (Last Changed On)

Treats last changed on as a discriminator between similar rows that would otherwise look identical in a raw extract.

**MUNAME** (Changed By User Name)

Stabilizes week-over-week metrics by fixing changed by user name (MUNAME) while allowing duration thresholds to move.

**RFCDEST** (RFC Destination)

For operations, rfc destination on RFCDEST indicates whether a row belongs in the current monitoring pass versus historical noise.


### Parameter Relationships

How parameter combinations work together

**DATUM** supplies an explicit monitoring-date range when you populate it, so the evaluation clock for duration calculations and any monitor-supplied date context is anchored to calendar bounds you choose instead of relying only on relative lookback.

When **DATUM** is not provided, **BACKDAYS** is the fallback that builds the lower monitoring date from the evaluation day backward for the date axis the online monitor uses before attribute rows are aged.

**DURATION** and **DURATION_UNIT** act as an additional filter after date-oriented selection: only destinations whose computed elapsed interval from last change timestamp to the evaluation moment still fit the configured duration band remain in the extract.

Both the date criteria (explicit **DATUM** or **BACKDAYS**-driven window) and the **DURATION** / **DURATION_UNIT** age test are applied together—rows must satisfy the date side and the duration side before the result set is considered final for alerting.

**MANAGE_IN_UTC** shifts whether the evaluation clock used with **DATUM** and duration math follows UTC semantics versus local application-server time, so calendar and duration results stay consistent with how your landscape runs the monitor.

**RFCDEST** ranges define which logical destinations enter the join; **CUNAME**, **CUDATE**, **MUNAME**, and **MUDATE** filters refine which attribute history rows are considered part of the same evaluation pass.

**LANGU** aligns description lookups and language-sensitive presentation with the monitor session when populated.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code (SY-DATUM minus one day as the lower bound for the monitoring date range when neither explicit monitoring dates nor a populated back-day interval fill the range tables).
- **DURATION** - initial - treated as unconstrained by code (empty multivalued interval keeps every computed duration value until explicit bounds are supplied on the selection interface).
- **DURATION_UNIT** - initial - treated as D by code (day-based duration math runs before the single-value unit read completes unless the caller overrides the unit afterward).

### Practical Example of Parameter Configuration

**Use Case 1: Explicit calendar window with day-based aging**

**Purpose:** Anchor evaluation to a fiscal-year window and flag destinations unchanged for more than thirty full days.

```
DATUM = 20250101 - 20251231
DURATION = 30
DURATION_UNIT = F
RFCDEST = PRD_*
```

**Use Case 2: Relative lookback with UTC evaluation**

**Purpose:** Use default lookback while forcing UTC-aligned clocks for a productive naming slice.

```
BACKDAYS = 7
MANAGE_IN_UTC = X
RFCDEST = CENTRAL*
```

**Use Case 3: Changer-based slice with minute cap**

**Purpose:** Review destinations last touched by a service account with a short inactivity cap in minutes.

```
MUNAME = SVC_RFC*
DURATION = 360
DURATION_UNIT = M
RFCDEST = EXT_*
```

**Use Case 4: Full bundle for integration review**

**Purpose:** Combine destination, language, duration band, and explicit monitoring dates in one pass.

```
RFCDEST = PARTNER_A
LANGU = E
DATUM = 20250401 - 20250430
DURATION = 14
DURATION_UNIT = D
CUNAME = ADMIN01
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_02_RFC_ATTR | CUCLIENT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_02_RFC_ATTR | CUDATE | Created on | DATS(8) | RDIR_CDATE |
| /SKN/S_SW_01_02_RFC_ATTR | CUNAME | Created by | CHAR(12) | CREUSRNAME |
| /SKN/S_SW_01_02_RFC_ATTR | CUTIME | System Time | TIMS(6) | SYUZEIT |
| /SKN/S_SW_01_02_RFC_ATTR | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_02_RFC_ATTR | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_02_RFC_ATTR | ERR_MSG | SW: Run Error | CHAR(255) | /SKN/E_SW_ERROR |
| /SKN/S_SW_01_02_RFC_ATTR | MUCLIENT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_02_RFC_ATTR | MUDATE | Changed On | DATS(8) | RDIR_UDATE |
| /SKN/S_SW_01_02_RFC_ATTR | MUNAME | Last Changed By | CHAR(12) | MODUSRNAME |
| /SKN/S_SW_01_02_RFC_ATTR | MUTIME | System Time | TIMS(6) | SYUZEIT |
| /SKN/S_SW_01_02_RFC_ATTR | RFCDEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_02_RFC_ATTR | RFCDEST_DESC |  | CHAR(100) |  |
| /SKN/S_SW_01_02_RFC_ATTR | RFCVERSION | Version ID for table entry | CHAR(32) | RFCVERSION |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_02_RFC_ATTR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_RFC_ATTR OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: MANAGE_IN_UTC       CHAR1 ,
             LANGU               LANGU,
             BACKDAYS            INT4,
             DURATION_UNIT /SKN/E_SW_DURATION_UNIT.
DATA_MULTY: RFCDEST           RFCDEST,
            CUNAME            CREUSRNAME,
            CUDATE            RDIR_CDATE,
            MUNAME            MODUSRNAME,
            MUDATE            RDIR_UDATE,
            DURATION         /SKN/E_SW_DURATION,
            DATUM             SYDATUM . " Paased by SW Online Monitor
DATA : WA TYPE /SKN/T_SW_RFC.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : RFCDEST TYPE RFCDEST.
DATA : RFC_MESS(255) TYPE C .
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
*-- Fill Selection Option Tables
   SELECT_MULTY:
            RFCDEST,
            CUNAME,
            CUDATE,
            MUNAME,
            MUDATE,
            DURATION,
            DATUM.
   LV_LANGU = SY-LANGU.
   LV_DURATION_UNIT = 'D'.
   SELECT_SINGLE: BACKDAYS,
                  DURATION_UNIT,
                  MANAGE_IN_UTC,
                  LANGU.
   "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_02_RFC_ATTR'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
 SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
 TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
    SELECT *
      FROM RFCDES AS R
        INNER JOIN RFCATTRIB AS A
          ON R~RFCDEST = A~RFCDEST
      INTO CORRESPONDING FIELDS OF TABLE T_DATA
      WHERE  R~RFCDEST IN R_RFCDEST
      AND A~CUNAME IN R_CUNAME
      AND A~MUNAME IN R_MUNAME
      .
*  loop at t_data.
*    sy_tabix = sy-tabix .
*    " Test RFC Destinations
*    RFCDEST = t_data-RFCDEST.
*    if not RFCDEST is initial .
*      CALL FUNCTION 'RFC_PING'
*        destination  RFCDEST
*        EXCEPTIONS SYSTEM_FAILURE = 1
*                   MESSAGE RFC_MESS
*                   COMMUNICATION_FAILURE = 2
*                   MESSAGE RFC_MESS
*                   OTHERS            = 9 .
*
*      if sy-subrc <> 0 and sy-subrc < 9.
*        t_data-ERR_MSG = RFC_MESS.
*        t_data-UDATE = sy-datum.
*        t_data-UTIME = sy-uzeit.
*        t_data-STATE_COLOR = 'R'.
*        CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
*          EXPORTING
*            STATE_COLOR       = t_data-STATE_COLOR
*          IMPORTING
*            STATE_ICON        = t_data-STATE_ICON         .
*         modify t_data index sy_tabix.
*      else.
*        t_data-ERR_MSG = 'OK'.
*        t_data-UDATE = sy-datum.
*        t_data-UTIME = sy-uzeit.
*        t_data-STATE_COLOR = 'G'.
*        CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
*          EXPORTING
*            STATE_COLOR       = t_data-STATE_COLOR
*          IMPORTING
*            STATE_ICON        = t_data-STATE_ICON         .
*         modify t_data index sy_tabix.
*      endif .
*     endif .
*  endloop.
   LOOP AT T_DATA.
     SY_TABIX = SY-TABIX.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-MUDATE
          T_FROM            = T_DATA-MUTIME
          D_TO              = SY_DATLO
          T_TO              = SY_TIMLO
          TIME_UNIT         = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
      IF SY-SUBRC = 0.
        T_DATA-DURATION = TIME_DIFF .
      ELSE.
        T_DATA-DURATION = '999999'.
      ENDIF.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF T_DATA-RFCDEST_DESC IS INITIAL.
      SELECT SINGLE RFCDOC1
        INTO T_DATA-RFCDEST_DESC
        FROM RFCDOC
        WHERE RFCDEST = T_DATA-RFCDEST
          AND RFCLANG = SY-LANGU .
      IF SY-SUBRC IS INITIAL.
        MODIFY T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDIF.
  ENDLOOP.
*  loop at t_data .
*    sy_tabix = sy-tabix .
*    if not t_data-STATE_COLOR in R_STATE_COLOR.
*      delete t_data index sy_tabix.
*    endif.
*  endloop.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
