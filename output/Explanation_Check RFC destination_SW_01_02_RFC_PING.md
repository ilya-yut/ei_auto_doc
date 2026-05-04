# Exception Indicator: Check RFC destination - SW_01_02_RFC_PING

## General Overview

This Exception Indicator evaluates RFC destination reachability and related presentation data so Basis and integration teams can see which logical destinations are in an error-style state or deviate from expected ping outcomes under the configured selection.

This EI helps by:
- Surfacing destinations that fail or time out during connectivity checks instead of waiting for downstream application errors
- Supporting landscape reviews when new destinations are introduced or when partner systems change certificates or network paths
- Giving operations a repeatable extract when broad destination lists would hide the few entries that actually threaten batch or real-time interfaces
- Helping audit demonstrate that destination health was checked on a recurring basis for sensitive integration windows
- Complementing standard remote connection analysis by tying alert-style coloring and optional table-driven listing to the same parameter bundle

Typical use includes post-migration validation, periodic integration hygiene, and troubleshooting after infrastructure incidents. Teams still use standard RFC destination and SM59-style administration when a single destination needs immediate repair.

The routine applies multivalued selection, performs the ping-style evaluation implied by the function logic, and returns rows that match the configured destination and presentation filters.


## Problem Description

Failure to monitor RFC destination connectivity and presentation-oriented outcomes creates multiple risks across interface availability, batch reliability, and control evidence.

**Operational and Integration Risks**
- Silent degradation of remote calls can continue until dependent jobs or user transactions fail first
- Broad destination inventories make it difficult to see which logical targets drive most of the instability
- Intermittent network or authorization issues are harder to prioritize without a consolidated, filtered view

**Control and Compliance Risks**
- Weak recurring checks weaken evidence that integration paths were supervised during critical close or migration windows
- Delayed detection of failing destinations increases remediation time when external partners already see symptoms
- Inconsistent review makes it harder to document which landscape segments were validated and when

**Management Visibility Risks**
- Leadership lacks a compact signal of where remote connectivity risk concentrates across systems and destination families
- Capacity and prioritization decisions suffer when hotspots are not visible in a single monitoring pass
- Cross-team triage slows when Basis and application support cannot share the same filtered view of problematic rows

## Suggested Resolution

**Immediate Response**
- Review each flagged line for destination name, derived presentation attributes, and any returned diagnostic text before changing infrastructure
- Open the standard remote connection and destination maintenance tools that fit your landscape to validate current reachability and authorization context
- Capture when the finding ties to regulated or financially material interfaces so follow-up is prioritized appropriately
- Segment results by destination type or description attributes before drilling into time or logon-check dimensions
- Confirm whether the situation is transient network noise versus persistent configuration drift

**System Assessment**
- Compare current results to prior monitoring cycles after certificate updates, firewall changes, or partner maintenance windows
- Look for concentration by destination family, type, or presentation attributes to see whether the issue is localized or systemic
- Validate whether recent transport or basis changes explain new failures versus long-standing latent issues
- Examine whether optional listing modes change how results should be interpreted for the same parameter bundle
- Check whether time fields cluster in a way that suggests scheduled maintenance or only off-cycle anomalies

**Corrective Actions**
- Correct destination definitions, authorization, or network paths according to SAP guidance, then re-run monitoring to confirm the population returned within tolerance
- Refine monitoring parameters after root cause so benign destinations are excluded without hiding genuine risk
- Document remediation and escalation outcomes when connectivity issues affected materially sensitive processes
- Schedule recurring runs during critical business windows and retain exports when audit evidence is required
- Route repeat systemic findings into defect or change management when landscape-wide corrections are required


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ERR_MSG | Run Error | CHAR | 255 | 0 | /SKN/E_SW_ERROR | /SKN/D_SW_LTEXT |
| 2 | LIST_FROM_TABLE | 'X' - based on /SKN/T_SW_RFC |  | 0 | 0 |  |  |
| 3 | LOCAL_DATE | Local Date | DATS | 8 | 0 |  |  |
| 4 | LOCAL_TIME | Local Time | TIMS | 6 | 0 |  |  |
| 5 | LOGON_CHECK | 'X' - performs Logon |  | 0 | 0 |  |  |
| 6 | RFCDEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 7 | RFCDEST_DESC |  | CHAR | 100 | 0 |  |  |
| 8 | RFCTYPE | Connection Type | CHAR | 1 | 0 | RFCTYPE_D | RFCTYPE |
| 9 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 10 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 11 | UDATE | Date | DATS | 8 | 0 |  |  |
| 12 | UTIME | Time | TIMS | 6 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 12 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ERR_MSG** (Run Error)

Interprets run error as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on ERR_MSG.

**LIST_FROM_TABLE** ('X' - based on /SKN/T_SW_RFC)

Explains why two monitoring passes differ: only the pass with stricter 'x' - based on /skn/t_sw_rfc on LIST_FROM_TABLE surfaces the disputed rows.

**LOCAL_DATE** (Local Date)

Reduces false positives during peak windows by tightening local date through LOCAL_DATE alongside state filters.

**LOCAL_TIME** (Local Time)

Documents expected operator behavior—local time on LOCAL_TIME should be set when that dimension is part of the control objective.

**LOGON_CHECK** ('X' - performs Logon)

Treats 'x' - performs logon as a discriminator between similar rows that would otherwise look identical in a raw extract.

**RFCDEST** (RFC Destination)

Supports escalation where rfc destination on RFCDEST signals ownership for follow-up between Basis and functional teams.

**RFCDEST_DESC** (RFCDEST_DESC)

Documents expected operator behavior—rfcdest_desc on RFCDEST_DESC should be set when that dimension is part of the control objective.

**RFCTYPE** (Connection Type)

Helps monitoring stay readable by requiring connection type (RFCTYPE) to match organizational or technical selectors when set.

**STATE_COLOR** (State Color)

Filters lines by the derived color bucket used for severity-style triage in the monitor framework.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional literals may exist where the framework extends the palette for neutral states.

**STATE_ICON** (State Icon)

Stabilizes week-over-week metrics by fixing state icon (STATE_ICON) while allowing duration thresholds to move.

**UDATE** (Date)

Allows phased rollout: first widen UDATE for date, then tighten thresholds once baseline noise is understood.

**UTIME** (Time)

Guards against oversized extracts when time on UTIME is narrowed together with client, user, or session filters.


### Parameter Relationships

How parameter combinations work together

**RFCDEST** and **RFCDEST_DESC** work as the primary destination identity and descriptive context pair: narrowing the logical destination set is the first scope control before type, time, and presentation filters refine the extract.

**RFCTYPE** and **LOGON_CHECK** shape how the monitor interprets destination categories and whether logon-style validation participates in the evaluation path you expect for this run.

**LOCAL_DATE**, **LOCAL_TIME**, **UDATE**, and **UTIME** let you align the monitoring snapshot with calendar and clock context on the application server or with last-change timestamps when those dimensions matter for triage.

**LIST_FROM_TABLE** and related mode-style inputs determine whether results are driven from table-oriented listing behavior versus the generalized evaluation path; treat them as switches that change how the same destination filters should be read.

**STATE_COLOR** and **STATE_ICON** should be interpreted together as the presentation bundle returned for each row so operators do not read severity-style color in isolation from the icon semantics.

**ERR_MSG** carries diagnostic text that should be read alongside destination and state attributes when judging business impact and the next technical action.


### Default Values

No default values are defined for this EI.

### Practical Example of Parameter Configuration

**Use Case 1: Destination slice with presentation filter**

**Purpose:** Monitor a naming pattern for productive RFC targets while focusing on error-style presentation.

```
RFCDEST = PRD_*
STATE_COLOR = R
RFCTYPE = 3
```

**Use Case 2: Table-driven listing with destination focus**

**Purpose:** Use list-from-table mode for a bounded set of destinations you maintain in the selection interface.

```
LIST_FROM_TABLE = X
RFCDEST = CENTRAL_HUB
LOGON_CHECK = X
```

**Use Case 3: Time-oriented snapshot for a destination family**

**Purpose:** Correlate ping-style results with evaluation date and time windows on the application server clock.

```
RFCDEST = EXT_*
LOCAL_DATE = 20250401 - 20250430
LOCAL_TIME = 080000 - 180000
```

**Use Case 4: Rich bundle for integration war room**

**Purpose:** Combine identity, type, presentation, and messaging context in one pass for a narrow integration review.

```
RFCDEST = PARTNER_A_DEST
RFCDEST_DESC = *Partner A*
RFCTYPE = 3
STATE_COLOR = R
STATE_ICON = 1
ERR_MSG = *
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_02_RFC_PING | ERR_MSG | SW: Run Error | CHAR(255) | /SKN/E_SW_ERROR |
| /SKN/S_SW_01_02_RFC_PING | LOCAL_DATE |  | DATS(8) |  |
| /SKN/S_SW_01_02_RFC_PING | LOCAL_TIME |  | TIMS(6) |  |
| /SKN/S_SW_01_02_RFC_PING | RFCDEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_02_RFC_PING | RFCDEST_DESC |  | CHAR(100) |  |
| /SKN/S_SW_01_02_RFC_PING | RFCTYPE | Type of Entry in RFCDES | CHAR(1) | RFCTYPE_D |
| /SKN/S_SW_01_02_RFC_PING | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_02_RFC_PING | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_02_RFC_PING | UDATE |  | DATS(8) |  |
| /SKN/S_SW_01_02_RFC_PING | UTIME |  | TIMS(6) |  |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_02_RFC_PING.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_RFC_PING OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_RFCDEST FOR /SKN/T_SW_RFC-RFCDEST ,
         R_STATE_COLOR FOR /SKN/S_SW_SYS_RFC_PING-STATE_COLOR.
DATA :   IS_GENERAL(1) TYPE C.
DATA : WA TYPE /SKN/T_SW_RFC.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : RFCDEST TYPE RFCDEST.
DATA : RFC_MESS(255) TYPE C .
DATA : IS_ERROR(1) TYPE C.
DATA : RC TYPE  SYSUBRC,
       ERR_MSG TYPE  /SKN/E_SW_ERROR.
DATA: FM TYPE FUNCNAME.
*-- Fill Selection Option Tables
   LOOP AT T_SELECT WHERE FIELDNM = 'RFCDEST'.
     MOVE-CORRESPONDING T_SELECT TO R_RFCDEST.
     APPEND R_RFCDEST.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
     MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
     APPEND R_STATE_COLOR.
   ENDLOOP.
   IS_GENERAL = 'X'.
   LOOP AT T_SELECT WHERE FIELDNM = 'IS_GENERAL'.
     IF NOT T_SELECT-LOW IS INITIAL.
       IS_GENERAL = 'X'.
     ENDIF.
     EXIT .
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'LIST_FROM_TABLE'.
     IF NOT T_SELECT-LOW IS INITIAL.
       CLEAR IS_GENERAL.
     ENDIF.
     EXIT .
   ENDLOOP.
"--- Logon Chek
  DATA_SINGLE: LOGON_CHECK CHAR1.             .
  SELECT_SINGLE: LOGON_CHECK.
   FM = 'RFC_PING'.
   IF LV_LOGON_CHECK IS NOT INITIAL.
     FM = 'RFCPING'.
   ENDIF.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_02_RFC_PING'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  IF IS_GENERAL IS INITIAL.
    SELECT *
      FROM /SKN/T_SW_RFC
      INTO CORRESPONDING FIELDS OF TABLE T_DATA
      WHERE  RFCDEST IN R_RFCDEST
        AND  IS_ACTIVE > ' '.
   ELSE.
     IF NOT R_RFCDEST[] IS INITIAL.
    SELECT *
      FROM RFCDES
      INTO CORRESPONDING FIELDS OF TABLE T_DATA
      WHERE  RFCDEST IN R_RFCDEST .
     ENDIF.
   ENDIF.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CLEAR IS_ERROR.
    " Test RFC Destinations
    RFCDEST = T_DATA-RFCDEST.
    IF NOT RFCDEST IS INITIAL .
      IF ( T_DATA-RFCTYPE = 'G' OR T_DATA-RFCTYPE =  'H' ).
        CALL FUNCTION '/SKN/F_SW_01_CHECK_RFC_HTTP'
          EXPORTING
            DEST          = RFCDEST
          IMPORTING
            RC            = RC
            ERR_MSG       = ERR_MSG.
        IF RC IS NOT INITIAL.
          IS_ERROR = 'X'.
        ENDIF.
        RFC_MESS = ERR_MSG.
      ELSE.
        CALL FUNCTION  FM   " 'RFC_PING'
          DESTINATION  RFCDEST
          EXCEPTIONS SYSTEM_FAILURE = 1
                     MESSAGE RFC_MESS
                     COMMUNICATION_FAILURE = 2
                     MESSAGE RFC_MESS
                     OTHERS            = 9 .
        IF SY-SUBRC <> 0 AND SY-SUBRC < 9.
          IS_ERROR = 'X'.
        ENDIF.
      ENDIF.
      IF IS_ERROR IS NOT INITIAL.
        T_DATA-ERR_MSG = RFC_MESS.
        T_DATA-UDATE = SY-DATUM.
        T_DATA-UTIME = SY-UZEIT.
        T_DATA-STATE_COLOR = 'R'.
        CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
          EXPORTING
            STATE_COLOR       = T_DATA-STATE_COLOR
          IMPORTING
            STATE_ICON        = T_DATA-STATE_ICON         .
         MODIFY T_DATA INDEX SY_TABIX.
      ELSE.
        T_DATA-ERR_MSG = 'OK'.
        T_DATA-UDATE = SY-DATUM.
        T_DATA-UTIME = SY-UZEIT.
        T_DATA-STATE_COLOR = 'G'.
        CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
          EXPORTING
            STATE_COLOR       = T_DATA-STATE_COLOR
          IMPORTING
            STATE_ICON        = T_DATA-STATE_ICON         .
        "--- Set Local Date-Time
        CALL FUNCTION '/SKN/F_SW_DT_GET_SYSTEM'
          EXPORTING
            DEST                        = T_DATA-RFCDEST
          IMPORTING
            SYS_DATE                    = T_DATA-LOCAL_DATE
            SYS_TIME                    = T_DATA-LOCAL_TIME
          EXCEPTIONS
            COMMUNICATION_FAILURE       = 1
            OTHERS                      = 2.
        IF SY-SUBRC = 0.
          MODIFY T_DATA INDEX SY_TABIX.
        ENDIF.
         MODIFY T_DATA INDEX SY_TABIX.
      ENDIF .
     ENDIF .
  ENDLOOP.
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
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
