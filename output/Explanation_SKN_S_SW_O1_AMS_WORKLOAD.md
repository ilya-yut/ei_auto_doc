# Exception Indicator: AMS - Workload - SW_01_AMS_WORKLOAD

## General Overview

This Exception Indicator summarizes SAP workload monitoring statistics for dialog-style processing so operations teams can see task types, timing breakdowns, and volume counters in a single structured extract aligned with cloud AMS retrieval.

The EI supports performance and stability reviews by:
- Surfacing average response, processing, CPU, database, wait, lock, and GUI-related timings in comparable units
- Making it easier to relate roll-in, roll-wait, queue, and load-and-generation time dimensions when triaging slow dialog behavior
- Supporting follow-up when byte and round-trip style counters suggest heavier frontend or payload patterns than baseline
- Giving Basis and application operations a repeatable snapshot for audits, release validation, and incident correlation

Organizations use workload-style monitoring to detect regressions after configuration changes, to compare peak windows to steady state, and to reduce guesswork when users report intermittent slowness. It is most useful when you need an exception-oriented summary rather than ad hoc drill-down in multiple tools.

The function reads workload rows for the evaluated date window from the cloud integration path, enriches duration in the configured unit, applies the declared selection ranges, and returns rows ready for alerting and reporting.


## Problem Description

Failure to monitor aggregated dialog workload behavior creates multiple risks across user experience, operational control, and capacity planning:

**Service Experience and Productivity Risks**
- Slow dialog response can appear as “random” slowness when the underlying pattern is workload-specific rather than infrastructure-wide
- Concentrated waits in queue, roll, or lock time can be missed when teams only watch average response time
- Frontend and network-time symptoms can be mistaken for application code defects when the workload mix shifted quietly
- Short spikes during peak logon or campaign windows may never be correlated with step count or round-trip patterns
- Cross-component symptoms can send teams down the wrong path when database time is fine but wait or roll time is not

**Operational Control and Platform Risks**
- Release and change teams lack a simple before-and-after signal when dialog-related parameters or notes are applied
- Capacity conversations may rely on anecdotal evidence instead of comparable workload metrics week over week
- Seasonal peaks can stress specific task types in predictable ways that still surprise teams if thresholds are not trended
- Batch and online contention can interact in ways that only become visible through multi-metric workload slices
- Temporary traffic shifts can change workload profiles without being documented as baseline changes

**Management Visibility and Decision-Making Risks**
- Leadership may approve additional online volume without visibility into whether dialog headroom supports it
- Finance and operations planning can misalign when technical signals are informal rather than comparable
- Audit and governance stakeholders lack concise evidence that workload health was monitored during sensitive periods
- Problem management slows when teams cannot separate “one noisy task pattern” from “broad dialog pressure”
- Post-incident reviews lack a compact before-and-after picture tying remediation actions to measurable timing movement

## Suggested Resolution

**Immediate Response**
- Review the flagged workload snapshot to see which timing dimensions drive the exception (response, processing, database, wait, roll, lock, GUI)
- Validate whether the signal aligns with a known change window (release, maintenance, campaign) versus unexpected load
- Check whether the pattern is isolated to specific task types or appears broadly across the extract
- Coordinate with the application owner if user-facing symptoms correlate with the same monitoring window
- Capture business context (close activities, campaigns, interface bursts) so later analysis separates normal peaks from defects

**System Assessment**
- Compare current readings to prior periods using the same monitoring intent and threshold philosophy
- Examine whether degradation is gradual (trend) versus step-change (configuration or workload shift)
- Review related platform signals when symptoms persist—database health, application server load, and background schedules
- Assess whether exceptions cluster in time-of-day or calendar patterns that match known operational cycles
- Validate that operational expectations for dialog performance still match the current release and sizing assumptions

**Corrective Actions**
- Tune monitoring thresholds or workload-related parameters when evidence shows sustained misalignment with service targets, following your change process
- Escalate to the responsible application or platform team when a specific task type or timing dimension remains out of band after validation
- Document threshold adjustments and rationale so future reviewers understand intent
- Schedule recurring checks during high-risk periods until stability is re-established
- Fold user communication and knowledge-base updates into ownership and runbook improvements under **Corrective Actions**—not as a separate training-only subsection


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backward |  | 0 | 0 |  |  |
| 2 | BYTES_AVG | Requested Data (KB) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_BYTES_AVG |  |
| 3 | COUNT | Counter | DEC | 24 | 0 | SWNCCNTAGG | SWNCDOMDEC24 |
| 4 | CPICTI_AVG | Average RFC Interf Time (ms) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_CPICTI_AVG |  |
| 5 | CPUTI_AVG | Average CPU Time (ms) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_CPUTI_AVG |  |
| 6 | DATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 7 | DBTI_AVG | Average DB Time (ms) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_DBTI_AVG |  |
| 8 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 9 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 10 | GUICNT | Number of Roundtrips | DEC | 24 | 0 | /SKN/E_SWNCTIMMS_GUICNT | SWNCDOMDEC24 |
| 11 | GUINETTIME_AVG | Average Frontend Network Time | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_GUINETTIM_AVG |  |
| 12 | GUITIME_AVG | Average GUI Time per Dialog St | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_GUITIME_AVG |  |
| 13 | LOADGENTI_AVG | Average Load and Gen Time (ms) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_LOADGENTI_AVG |  |
| 14 | LOCKTI_AVG | Average Lock Time (ms) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_LOCKTI_AVG |  |
| 15 | PROCTI_AVG | Avg. Processing Time | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_PROCTI_AVG |  |
| 16 | QUEUETI_AVG | Average Wait Time (ms) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_QUEUETI_AVG |  |
| 17 | RESPTI_AVG | Average response Time (mS) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_RESPTI_AVG |  |
| 18 | ROLLINTI_AVG | Average Roll In Time (ms) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_ROLLINTI_AVG |  |
| 19 | ROLLSTEP | Number of Dialog Steps | DEC | 24 | 0 | /SKN/E_SWNCCNTAGG_ROLLSTEP | SWNCDOMDEC24 |
| 20 | ROLLWAITTI_AVG | Average Roll Wait Time (ms) | DEC | 24 | 1 | /SKN/E_SWNCTIMMS_ROLLWAITT_AVG |  |
| 21 | TASKTYPE | Task Type | RAW | 1 | 0 | SWNCTASKTYPERAW | SWNCDOMRAW1 |
| 22 | TASKTYPE_TXT | Task Type | CHAR | 16 | 0 | SWNCTASKTYPE | SWNCDOMTXT16 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 22 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Days Backward)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**BYTES_AVG** (Requested Data (KB))

Separates cross-client noise from in-scope work when requested data (kb) on BYTES_AVG correlates with client or user attributes.

**COUNT** (Counter)

Improves readability of exported lists because counter (COUNT) columns stay aligned with the configured filter intent.

**CPICTI_AVG** (Average RFC Interf Time (ms))

Guards against oversized extracts when average rfc interf time (ms) on CPICTI_AVG is narrowed together with client, user, or session filters.

**CPUTI_AVG** (Average CPU Time (ms))

Combines with related filters so average cpu time (ms) on CPUTI_AVG refines which records remain for duration or state checks.

**DATE** (Date)

When populated, keeps the extract focused so date (DATE) aligns with the intended triage slice.

**DBTI_AVG** (Average DB Time (ms))

For distributed landscapes, average db time (ms) on DBTI_AVG often anchors which application server or destination appears in results.

**DURATION** (Duration In Time Units)

When tightened, duration in time units (DURATION) removes rows that would otherwise dilute attention from failing or stuck cases.

**DURATION_UNIT** (Duration Unit)

Unit for elapsed time between each session's creation date and time and the evaluation clock.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**GUICNT** (Number of Roundtrips)

Valuable when comparing health before and after a release—hold number of roundtrips on GUICNT constant while varying other filters.

**GUINETTIME_AVG** (Average Frontend Network Time)

For distributed landscapes, average frontend network time on GUINETTIME_AVG often anchors which application server or destination appears in results.

**GUITIME_AVG** (Average GUI Time per Dialog St)

Improves readability of exported lists because average gui time per dialog st (GUITIME_AVG) columns stay aligned with the configured filter intent.

**LOADGENTI_AVG** (Average Load and Gen Time (ms))

Helps distinguish technical versus business attributes when average load and gen time (ms) on LOADGENTI_AVG correlates with counters or status fields.

**LOCKTI_AVG** (Average Lock Time (ms))

Ensures reporting respects average lock time (ms) constraints carried by LOCKTI_AVG.

**PROCTI_AVG** (Avg. Processing Time)

When tightened, avg. processing time (PROCTI_AVG) removes rows that would otherwise dilute attention from failing or stuck cases.

**QUEUETI_AVG** (Average Wait Time (ms))

Stabilizes week-over-week metrics by fixing average wait time (ms) (QUEUETI_AVG) while allowing duration thresholds to move.

**RESPTI_AVG** (Average response Time (mS))

Documents expected operator behavior—average response time (ms) on RESPTI_AVG should be set when that dimension is part of the control objective.

**ROLLINTI_AVG** (Average Roll In Time (ms))

For operations, average roll in time (ms) on ROLLINTI_AVG indicates whether a row belongs in the current monitoring pass versus historical noise.

**ROLLSTEP** (Number of Dialog Steps)

Works downstream of the initial read so number of dialog steps on ROLLSTEP still participates in row-level deletion rules.

**ROLLWAITTI_AVG** (Average Roll Wait Time (ms))

Reflects real administration where average roll wait time (ms) on ROLLWAITTI_AVG is routinely restricted to a single productive client or object family.

**TASKTYPE** (Task Type)

Separates cross-client noise from in-scope work when task type on TASKTYPE correlates with client or user attributes.

**TASKTYPE_TXT** (Task Type)

Treats task type as a discriminator between similar rows that would otherwise look identical in a raw extract.


### Parameter Relationships

How parameter combinations work together

**Time window vs. duration filtering**

- **BACKDAYS** shapes how far back the cloud retrieval looks when explicit date selection does not already define the window. **DATE** narrows which calendar rows are in scope for the workload statistics pulled into the extract.
- **DURATION** and **DURATION_UNIT** work together as an additional age-style filter on the computed elapsed interval for each row relative to the evaluation clock, so date window and duration conditions both apply when configuring tight exception logic.

**Metric families**

- Response and processing timings (**RESPTI_AVG**, **PROCTI_AVG**) describe end-to-end and application-side delay; CPU, database, queue, lock, roll, load-and-generation, and GUI-related averages split where time accumulates across the dialog path.
- Volume-style counters such as **COUNT**, **ROLLSTEP**, and **GUICNT** complement timing averages when you need both “how slow” and “how much work” in the same snapshot.
- **BYTES_AVG** adds a payload-oriented signal that can explain higher network or transfer cost even when pure timings look acceptable.

**Selection contract**

- The multivalued selection inputs define the ranges the monitor is allowed to surface. Threshold changes on any dimension directly change which workload rows remain after filtering, so tightening one metric without revisiting related metrics can shift which patterns appear in alerting.


### Default Values

- **BACKDAYS** - initial - treated as 1 day backward window by code
- **DURATION** - initial - treated as no extra duration filter by code
- **DURATION_UNIT** - initial - treated as D day units by code

### Practical Example of Parameter Configuration

**Use Case 1: High average response with elevated database time**

**Purpose:** Catch dialog workload rows where overall responsiveness and database time both indicate pressure.

```
DATE = 20260101 - 20260131
BACKDAYS = 1
DURATION = 0 - 999999999
DURATION_UNIT = D
RESPTI_AVG = 2000 - 999999999
DBTI_AVG = 500 - 999999999
```

**Use Case 2: Queue and roll wait hotspot**

**Purpose:** Focus on wait and roll dimensions when users report stuck or uneven dialog steps.

```
TASKTYPE_TXT = DIALOG
QUEUETI_AVG = 500 - 999999999
ROLLWAITTI_AVG = 500 - 999999999
ROLLSTEP = 10 - 999999999
```

**Use Case 3: Frontend-heavy pattern**

**Purpose:** Highlight rows where GUI and network-time averages dominate the profile.

```
GUITIME_AVG = 500 - 999999999
GUINETTIME_AVG = 300 - 999999999
GUICNT = 20 - 999999999
BYTES_AVG = 500 - 999999999
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_O1_AMS_WORKLOAD | BYTES_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_BYTES_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | COUNT | SAP Workload NW Collector: Counter in Aggregates | DEC(24) | SWNCCNTAGG |
| /SKN/S_SW_O1_AMS_WORKLOAD | CPICTI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_CPICTI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | CPUTI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_CPUTI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | DATE | Date | DATS(8) | DATUM |
| /SKN/S_SW_O1_AMS_WORKLOAD | DBTI_AVG | Average DB Time (ms) | DEC(24,1) | /SKN/E_SWNCTIMMS_DBTI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_O1_AMS_WORKLOAD | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_O1_AMS_WORKLOAD | GUICNT | Number of Roundtrips | DEC(24) | /SKN/E_SWNCTIMMS_GUICNT |
| /SKN/S_SW_O1_AMS_WORKLOAD | GUINETTIME_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_GUINETTIM_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | GUITIME_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_GUITIME_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | LOADGENTI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_LOADGENTI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | LOCKTI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_LOCKTI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | PROCTI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_PROCTI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | QUEUETI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_QUEUETI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | RESPTI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_RESPTI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | ROLLINTI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_ROLLINTI_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | ROLLSTEP |  | DEC(24) | /SKN/E_SWNCCNTAGG_ROLLSTEP |
| /SKN/S_SW_O1_AMS_WORKLOAD | ROLLWAITTI_AVG |  | DEC(24,1) | /SKN/E_SWNCTIMMS_ROLLWAITT_AVG |
| /SKN/S_SW_O1_AMS_WORKLOAD | TASKTYPE | SAP Workload NW Collector: Task Type (Raw Format) | RAW(1) | SWNCTASKTYPERAW |
| /SKN/S_SW_O1_AMS_WORKLOAD | TASKTYPE_TXT | SAP Workload NW Collector: Task Type | CHAR(16) | SWNCTASKTYPE |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_O1_AMS_WORKLOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_O1_AMS_WORKLOAD OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
DATA_MULTY: DURATION   /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
            DATUM     SY-DATUM.
DATA_MULTY: TASKTYPE  SWNCTASKTYPERAW,
            TASKTYPE_TXT SWNCTASKTYPE,
            COUNT SWNCCNTAGG,
            ROLLSTEP SWNCCNTAGG,
            RESPTI_AVG     /SKN/E_SWNCTIMMSAVG,
            PROCTI_AVG     /SKN/E_SWNCTIMMSAVG,
            CPUTI_AVG      /SKN/E_SWNCTIMMSAVG,
            DBTI_AVG       /SKN/E_SWNCTIMMSAVG,
            QUEUETI_AVG    /SKN/E_SWNCTIMMSAVG,
            ROLLINTI_AVG   /SKN/E_SWNCTIMMSAVG,
            ROLLWAITTI_AVG /SKN/E_SWNCTIMMSAVG,
            LOADGENTI_AVG  /SKN/E_SWNCTIMMSAVG,
            LOCKTI_AVG     /SKN/E_SWNCTIMMSAVG,
            CPICTI_AVG     /SKN/E_SWNCTIMMSAVG,
            GUINETTIME_AVG /SKN/E_SWNCTIMMSAVG,
            GUITIME_AVG    /SKN/E_SWNCTIMMSAVG,
            GUICNT         SWNCCNTAGG,
            BYTES_AVG      /SKN/E_SWNCTIMMSAVG.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             BACKDAYS INT4,
             LANGU LANGU.
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LV_D_FROM TYPE DATUM,
      LV_D_TO TYPE DATUM.
DATA: LS_WORLOAD TYPE /SKN/S_SW_AMS_WORLOAD,
      LT_WORLOAD LIKE TABLE OF LS_WORLOAD.
DATA: SY_DATLO LIKE SY-DATLO ,
      SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
*-- Fill Selection Option Tables
   SELECT_MULTY: DURATION,
                 DATUM.
   SELECT_MULTY: TASKTYPE,
                 TASKTYPE_TXT,
                 COUNT,
                 ROLLSTEP,
                 RESPTI_AVG,
                 PROCTI_AVG,
                 CPUTI_AVG,
                 DBTI_AVG,
                 QUEUETI_AVG,
                 ROLLINTI_AVG,
                 ROLLWAITTI_AVG,
                 LOADGENTI_AVG,
                 LOCKTI_AVG,
                 CPICTI_AVG,
                 GUINETTIME_AVG,
                 GUITIME_AVG,
                 GUICNT,
                 BYTES_AVG.
   LV_DURATION_UNIT = 'D'.
   LV_BACKDAYS = 1.
   SELECT_SINGLE: DURATION_UNIT,
                  BACKDAYS,
                  LANGU.
***  set_sy_time manage_in_utc sy_datlo sy_timlo .
***  time_shift sy_datlo sy_timlo . " TIME_SHIFT parameter
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
  IF R_DATUM[] IS INITIAL .  " Set default value
    RS_DATUM-SIGN   = 'I'.
    RS_DATUM-OPTION = 'GE'.
    LV_D_FROM  = SY_DATLO - LV_BACKDAYS .
    RS_DATUM-LOW = LV_D_FROM .
     APPEND RS_DATUM TO R_DATUM.
  ENDIF .
  LV_D_FROM = SY-DATUM - 1.
  LOOP AT R_DATUM INTO RS_DATUM.
    IF RS_DATUM-LOW < LV_D_FROM.
      LV_D_FROM = RS_DATUM-LOW.
    ENDIF.
  ENDLOOP.
  LV_D_TO = SY-DATUM - 1.
  LOOP AT R_DATUM INTO RS_DATUM.
    IF RS_DATUM-LOW > LV_D_TO.
      LV_D_TO = RS_DATUM-LOW.
    ENDIF.
    IF RS_DATUM-HIGH > LV_D_TO.
      LV_D_TO = RS_DATUM-HIGH.
    ENDIF.
  ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  CALL FUNCTION '/SKN/F_SW_O1_AMS_WORKLOAD_D'
    EXPORTING
      DEST            = LV_SW_DEST
      D_FROM          = LV_D_FROM
      D_TO            = LV_D_TO
    TABLES
      T_WORLOAD       = LT_WORLOAD.
   LOOP AT LT_WORLOAD INTO LS_WORLOAD.
     MOVE-CORRESPONDING LS_WORLOAD TO LS_DATA.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = LS_DATA-DATE
          T_FROM      = SY_TIMLO
          D_TO        = SY_DATLO
          T_TO        = SY_TIMLO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        LS_DATA-DURATION = TIME_DIFF .
        LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      ENDIF.
     APPEND LS_DATA TO LT_DATA.
   ENDLOOP.
  DELETE LT_DATA WHERE DURATION NOT IN R_DURATION.
  DELETE LT_DATA WHERE TASKTYPE NOT IN R_TASKTYPE.
  DELETE LT_DATA WHERE TASKTYPE_TXT NOT IN R_TASKTYPE_TXT.
  DELETE LT_DATA WHERE COUNT NOT IN R_COUNT.
  DELETE LT_DATA WHERE ROLLSTEP NOT IN R_ROLLSTEP.
  DELETE LT_DATA WHERE RESPTI_AVG NOT IN R_RESPTI_AVG.
  DELETE LT_DATA WHERE PROCTI_AVG NOT IN R_PROCTI_AVG.
  DELETE LT_DATA WHERE CPUTI_AVG NOT IN R_CPUTI_AVG.
  DELETE LT_DATA WHERE DBTI_AVG NOT IN R_DBTI_AVG.
  DELETE LT_DATA WHERE QUEUETI_AVG NOT IN R_QUEUETI_AVG.
  DELETE LT_DATA WHERE ROLLINTI_AVG NOT IN R_ROLLINTI_AVG.
  DELETE LT_DATA WHERE ROLLWAITTI_AVG NOT IN R_ROLLWAITTI_AVG.
  DELETE LT_DATA WHERE LOADGENTI_AVG NOT IN R_LOADGENTI_AVG.
  DELETE LT_DATA WHERE LOCKTI_AVG NOT IN R_LOCKTI_AVG.
  DELETE LT_DATA WHERE CPICTI_AVG NOT IN R_CPICTI_AVG.
  DELETE LT_DATA WHERE GUINETTIME_AVG NOT IN R_GUINETTIME_AVG.
  DELETE LT_DATA WHERE GUITIME_AVG NOT IN R_GUITIME_AVG.
  DELETE LT_DATA WHERE GUICNT NOT IN R_GUICNT.
  DELETE LT_DATA WHERE BYTES_AVG NOT IN R_BYTES_AVG.
 T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
