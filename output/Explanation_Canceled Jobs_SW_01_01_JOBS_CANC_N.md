# Exception Indicator: Canceled Jobs - SW_01_01_JOBS_CANC_N

## General Overview

This Exception Indicator (EI) monitors canceled background jobs and related unstable job outcomes, helping identify execution failures by status, schedule context, and runtime duration.

This EI helps by:
- Detecting canceled/failed job executions before SLA impact expands
- Segmenting issues by scheduler, program, job class, and variant
- Prioritizing unresolved or risky jobs with duration and state logic
- Supporting operational control over recurring background processing failures

The function retrieves background job records, derives status color/icon semantics, computes runtime and age durations, and returns exception-focused job entries.


## Problem Description

When canceled and unstable jobs are not monitored systematically, recurring failures can accumulate and disrupt scheduled processing chains.

**Operational and Process Risks**
- Canceled jobs can delay downstream data and business processes
- Runtime exceptions may repeat without pattern-based remediation
- Long-running or stuck jobs can consume resources and block windows

**Control and Compliance Risks**
- Missing canceled-job oversight weakens operations-control evidence
- Inconsistent review periods reduce comparability across cycles
- Weak ownership traceability slows accountable remediation

**Management Visibility Risks**
- Job instability trends can remain hidden until major incidents occur
- Capacity and support planning are harder without failure segmentation

### Suggested Resolution

**Immediate Response**
- Prioritize canceled and error-like job states
- Escalate recurring failing programs/variants and job owners
- Isolate high-duration jobs for rapid stabilization actions

**System Assessment**
- Validate date-reference and duration settings used in monitoring
- Review status/state-color distributions by scheduler and program
- Compare periodic-job behavior against expected baselines

**Corrective Actions**
- Standardize canceled-job monitoring cadence and thresholds
- Improve restart/repair playbooks by failure pattern
- Feed recurring causes into scheduling and code-quality improvements


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BTCSYSTEM | Targ. Sys of a Backg. Job | CHAR | 32 | 0 | BTCTGTSYS | TEXT32 |
| 3 | DATE_REF_FLD | Date Ref. Field |  | 0 | 0 |  |  |
| 4 | DURATION | Duration from Start in d.unit | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 5 | DURATION_H | Execution Time(Hours) | NUMC | 6 | 0 | /SKN/E_SW_DURATION_H |  |
| 6 | DURATION_M | Execution Time(Minutes) | NUMC | 6 | 0 | /SKN/E_SW_DURATION_M |  |
| 7 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 8 | ENDDATE | Execution Date | DATS | 8 | 0 | BTCXDATE | SYDATS |
| 9 | ENDTIME | Start Time | TIMS | 6 | 0 | BTCXTIME | SYTIME |
| 10 | JOBCLASS | Job class | CHAR | 1 | 0 | BTCJOBCLAS | CHAR1 |
| 11 | JOBCOUNT | Job No. | CHAR | 8 | 0 | BTCJOBCNT | CHAR8 |
| 12 | JOBNAME | Job name | CHAR | 32 | 0 | BTCJOB | CHAR32 |
| 13 | LASTCHDATE | Last job change | DATS | 8 | 0 | BTCJCHDATE | SYDATS |
| 14 | LASTCHNAME | Last job change by | CHAR | 12 | 0 | BTCJCHNM | CHAR12 |
| 15 | LASTCHTIME | Last job change/made by | TIMS | 6 | 0 | BTCJCHTIME | SYTIME |
| 16 | NO_DATE_RESTRICTION | No date restriction |  | 0 | 0 |  |  |
| 17 | PERIODIC | Periodic job | CHAR | 1 | 0 | BTCPFLAG | CHAR1 |
| 18 | PRDHOURS | Periodic job | NUMC | 2 | 0 | BTCPHOUR | NUM02 |
| 19 | PRDMINS | Periodic job | NUMC | 2 | 0 | BTCPMIN | NUM02 |
| 20 | PROGNAME | Rept Name | CHAR | 40 | 0 | BTCPROG | PROGNAME |
| 21 | RELDATE | Scheduled release | DATS | 8 | 0 | BTCRELDT | SYDATS |
| 22 | RELTIME | Scheduled release | TIMS | 6 | 0 | BTCRELTM | SYTIME |
| 23 | RELUNAME | User that released scheduled batch job | CHAR | 12 | 0 | BTCRELNM | CHAR12 |
| 24 | SDLDATE | Schedule date | DATS | 8 | 0 | BTCSDLDATE | SYDATS |
| 25 | SDLSTRTDT | Start date | DATS | 8 | 0 | BTCSDATE | SYDATS |
| 26 | SDLSTRTTM | Start time | TIMS | 6 | 0 | BTCSTIME | SYTIME |
| 27 | SDLTIME | Schedule date | TIMS | 6 | 0 | BTCSDLTIME | SYTIME |
| 28 | SDLUNAME | Job scheduler | CHAR | 12 | 0 | BTCSDLNM | CHAR12 |
| 29 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 30 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 31 | STATUS | Job status | CHAR | 1 | 0 | BTCSTATUS | CHAR1 |
| 32 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 33 | STEPCOUNT | Step no. | INT4 | 10 | 0 | BTCSTEPCNT |  |
| 34 | STRTDATE | Execution Date | DATS | 8 | 0 | BTCXDATE | SYDATS |
| 35 | STRTTIME | Start Time | TIMS | 6 | 0 | BTCXTIME | SYTIME |
| 36 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 37 | VARIANT | Variant | CHAR | 14 | 0 | BTCVARIANT | CHAR14 |
| 38 | W_VARIANT | x - Include Program Variant |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 38 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Backdays):

BACKDAYS defines fallback lookback days when explicit date ranges are not provided.

**BTCSYSTEM** (Targ. Sys of a Backg. Job):

BTCSYSTEM limits monitoring to jobs targeted to a specific background processing system, useful in multi-system landscapes.

**DATE_REF_FLD** (Date Ref. Field):

DATE_REF_FLD selects which job date field is used as reference for date-oriented filtering and duration logic.

**DATE_REF_FLD Options:**
- **STRTDATE**: Job start date reference (default fallback).
- **ENDDATE**: Job end date reference.
- **SDLDATE**: Schedule date reference.
- **SDLSTRTDT**: Planned start date reference.
- **LASTCHDATE**: Last change date reference.
- **RELDATE**: Release date reference.

**DURATION** (Duration from Start in d.unit):

DURATION defines elapsed-time threshold in selected duration unit for prioritizing delayed jobs.

**DURATION_H** (Execution Time(Hours)):

DURATION_H applies hour-based runtime filtering from start-to-end execution span.

**DURATION_M** (Execution Time(Minutes)):

DURATION_M applies minute-based runtime filtering from start-to-end execution span.

**DURATION_UNIT** (Duration Unit(D/H/M)):

DURATION_UNIT defines unit used for DURATION calculation from reference time to now (or end time fallback).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**ENDDATE** (Execution Date):

ENDDATE bounds analysis by actual completion date and is especially useful for identifying jobs that ended unsuccessfully in the review period.

**ENDTIME** (Start Time):

ENDTIME narrows completion-time granularity so analysts can isolate cancellation clusters within specific intraday windows.

**JOBCLASS** (Job class):

JOBCLASS segments jobs by priority class so you can distinguish failures in critical versus low-priority processing.

**JOBCOUNT** (Job No.):

JOBCOUNT identifies a concrete job execution instance and is useful when multiple runs share the same job name.

**JOBNAME** (Job name):

JOBNAME scopes monitoring to specific background job definitions for targeted failure analysis.

**LASTCHDATE** (Last job change):

LASTCHDATE focuses on when job definition or control attributes were last modified, helping correlate failures with recent changes.

**LASTCHNAME** (Last job change by):

LASTCHNAME identifies who last changed the job, supporting ownership tracing when failures start after administrative updates.

**LASTCHTIME** (Last job change/made by):

LASTCHTIME adds the time-of-change dimension to LASTCHDATE so change-related incidents can be sequenced more precisely.

**NO_DATE_RESTRICTION** (No date restriction):

NO_DATE_RESTRICTION disables date-window restriction when set, broadening result scope.

**PERIODIC** (Periodic job):

PERIODIC distinguishes recurring jobs from one-time jobs, helping isolate scheduler-driven repeat failure patterns.

**PRDHOURS** (Periodic job):

PRDHOURS represents hourly periodicity settings and helps analyze whether short repetition intervals correlate with cancellations.

**PRDMINS** (Periodic job):

PRDMINS represents minute-based periodicity settings and is useful for detecting failures in high-frequency job cycles.

**PROGNAME** (Rept Name):

PROGNAME focuses analysis on the executed ABAP report/program to localize technical root causes by code path.

**RELDATE** (Scheduled release):

RELDATE filters by release date for scheduled jobs, useful when validating whether cancellations spike after release events.

**RELTIME** (Scheduled release):

RELTIME filters by release-time point, enabling analysis of failures around narrow release-time intervals.

**RELUNAME** (User that released scheduled batch job):

RELUNAME identifies the user who released the job schedule, which helps connect release actions to subsequent canceled executions.

**SDLDATE** (Schedule date):

SDLDATE scopes records by scheduling date, separating planning-time issues from execution-time issues.

**SDLSTRTDT** (Start date):

SDLSTRTDT targets planned start date context to detect jobs that miss or fail near expected start-day commitments.

**SDLSTRTTM** (Start time):

SDLSTRTTM targets planned start time, helping detect timing-related instability in tightly scheduled job windows.

**SDLTIME** (Schedule date):

SDLTIME captures scheduling-time detail and supports intraday analysis of scheduler-driven failure patterns.

**SDLUNAME** (Job scheduler):

SDLUNAME identifies the scheduling user, which is useful for tracing recurring issues tied to specific scheduling ownership.

**STATE_COLOR** (State Color):

STATE_COLOR is the visual severity/status classifier used for quick job-triage grouping.

**STATE_COLOR Options:**
- **R**: Red (failed/error jobs).
- **G**: Green (successful/completed jobs).
- **Y**: Yellow (warning/processing jobs).

**STATE_ICON** (State Icon):

STATE_ICON is the visual marker derived from state color that supports quick scan-based triage in dashboards and reports.

**STATUS** (Job status):

STATUS is the direct job-state selector that controls lifecycle-state inclusion.

**STATUS Options:**
- Use SAP background-job status values configured in the system.
- State-color logic can be used as complementary status segmentation.

**STATUS_DESC** (SW Message):

STATUS_DESC provides the human-readable explanation of STATUS, making operational interpretation and escalation faster.

**STEPCOUNT** (Step no.):

STEPCOUNT isolates a specific step within a multi-step job, which helps pinpoint where failure occurred in the chain.

**STRTDATE** (Execution Date):

STRTDATE defines execution start-date scope and is commonly used as the primary reference date for canceled-job monitoring.

**STRTTIME** (Start Time):

STRTTIME defines execution start-time boundaries for intraday failure analysis and delay diagnostics.

**USER_FLD** (Dynamic Recipient User Field):

USER_FLD is a dynamic user-context selector; fixed values are implementation-dependent unless explicitly defined in code.

**USER_FLD Options:**
- No fixed USER_FLD value list is defined in the available code for this EI.

**VARIANT** (Variant):

VARIANT identifies the program variant used by the job, enabling comparison of failures across different runtime parameterizations.

**W_VARIANT** (x - Include Program Variant):

W_VARIANT controls whether program variant enrichment is included for result records.


### Parameter Relationship

How parameter combinations work together

**Date and Time Controls:**

- **BACKDAYS** is fallback when explicit date ranges are not provided.
- **DATE_REF_FLD** decides which date field receives the derived date window.
- **NO_DATE_RESTRICTION** can override date-window filtering and broaden scope.

**Duration Logic:**

- **DURATION_H** and **DURATION_M** evaluate run duration from start to end time.
- **DURATION** + **DURATION_UNIT** are an additional (second) filter after date selection.
- Final result depends on both date conditions and duration conditions.

**State and Status Interpretation:**

- **STATUS** maps to **STATUS_DESC** and **STATE_COLOR/STATE_ICON** for readable triage.
- **STATE_COLOR** and **STATUS** together focus remediation on high-risk job outcomes.


### Default Values
- **DURATION_UNIT** - M
- **DATE_REF_FLD** - STRTDATE
- **BACKDAYS** - 1 (today and yesterday)

### Practical Example of Parameter Configuration
**Use Case 1: Recent canceled jobs triage**

```plaintext
BACKDAYS = 1
STATUS = CANCELED
DURATION = 30
DURATION_UNIT = M
STATE_COLOR = R
```

**Purpose:** Detect recent canceled/error jobs and prioritize those with meaningful delay profile.

**Use Case 2: Scheduler-specific unstable jobs**

```plaintext
SDLUNAME = BATCH_ADMIN
PROGNAME = ZFI_CLOSE_JOB
DATE_REF_FLD = LASTCHDATE
BACKDAYS = 3
```

**Purpose:** Analyze instability around a scheduler/program pair using last-change date context.

**Use Case 3: Full-day long-running periodic jobs**

```plaintext
PERIODIC = X
DURATION = 1
DURATION_UNIT = F
NO_DATE_RESTRICTION = X
```

**Purpose:** Identify periodic jobs with full-day delay behavior without strict date-window exclusion.


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_JOBS | BTCSYSTEM | Target System to Run Background Job | CHAR(32) | BTCTGTSYS |
| /SKN/S_SW_01_01_JOBS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_JOBS | DURATION_H | SW: Duration In Hours | NUMC(6) | /SKN/E_SW_DURATION_H |
| /SKN/S_SW_01_01_JOBS | DURATION_M | SW: Duration In Minutes | NUMC(6) | /SKN/E_SW_DURATION_M |
| /SKN/S_SW_01_01_JOBS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_JOBS | ENDDATE | Job start date | DATS(8) | BTCXDATE |
| /SKN/S_SW_01_01_JOBS | ENDTIME | Batch job start time | TIMS(6) | BTCXTIME |
| /SKN/S_SW_01_01_JOBS | JOBCLASS | Job classification | CHAR(1) | BTCJOBCLAS |
| /SKN/S_SW_01_01_JOBS | JOBCOUNT | Job ID | CHAR(8) | BTCJOBCNT |
| /SKN/S_SW_01_01_JOBS | JOBNAME | Background job name | CHAR(32) | BTCJOB |
| /SKN/S_SW_01_01_JOBS | LASTCHDATE | Date of last job change | DATS(8) | BTCJCHDATE |
| /SKN/S_SW_01_01_JOBS | LASTCHNAME | Last job change made by | CHAR(12) | BTCJCHNM |
| /SKN/S_SW_01_01_JOBS | LASTCHTIME | Time of last job change | TIMS(6) | BTCJCHTIME |
| /SKN/S_SW_01_01_JOBS | PERIODIC | Periodic Jobs Indicator | CHAR(1) | BTCPFLAG |
| /SKN/S_SW_01_01_JOBS | PRDHOURS | Duration period (in hours) for a batch job | NUMC(2) | BTCPHOUR |
| /SKN/S_SW_01_01_JOBS | PRDMINS | Duration period (in minutes) for a batch job | NUMC(2) | BTCPMIN |
| /SKN/S_SW_01_01_JOBS | PROGNAME | Program name within a step (e.g. report) | CHAR(40) | BTCPROG |
| /SKN/S_SW_01_01_JOBS | RELDATE | Release Date for Background Scheduling | DATS(8) | BTCRELDT |
| /SKN/S_SW_01_01_JOBS | RELTIME | Release time of scheduled background job | TIMS(6) | BTCRELTM |
| /SKN/S_SW_01_01_JOBS | RELUNAME | User that released scheduled batch job | CHAR(12) | BTCRELNM |
| /SKN/S_SW_01_01_JOBS | SDLDATE | Date of job/step scheduling | DATS(8) | BTCSDLDATE |
| /SKN/S_SW_01_01_JOBS | SDLSTRTDT | Planned Start Date for Background Job | DATS(8) | BTCSDATE |
| /SKN/S_SW_01_01_JOBS | SDLSTRTTM | Planned start time for background Job | TIMS(6) | BTCSTIME |
| /SKN/S_SW_01_01_JOBS | SDLTIME | Time of a scheduled job/step | TIMS(6) | BTCSDLTIME |
| /SKN/S_SW_01_01_JOBS | SDLUNAME | Initiator of job/step scheduling | CHAR(12) | BTCSDLNM |
| /SKN/S_SW_01_01_JOBS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_JOBS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_JOBS | STATUS | State of Background Job | CHAR(1) | BTCSTATUS |
| /SKN/S_SW_01_01_JOBS | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_01_JOBS | STEPCOUNT | Job step ID number. | INT4(10) | BTCSTEPCNT |
| /SKN/S_SW_01_01_JOBS | STRTDATE | Job start date | DATS(8) | BTCXDATE |
| /SKN/S_SW_01_01_JOBS | STRTTIME | Batch job start time | TIMS(6) | BTCXTIME |
| /SKN/S_SW_01_01_JOBS | VARIANT | Name of variant within a step | CHAR(14) | BTCVARIANT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_JOBS_N.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_JOBS OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: MANAGE_IN_UTC         CHAR1 ,
               LANGU                 LANGU,
               NO_DATE_RESTRICTION   CHAR1,
               DATE_REF_FLD          NAME_FELD,
               W_VARIANT             CHAR1 ,
               DURATION_UNIT         /SKN/E_SW_DURATION_UNIT.
  DATA_MULTY: DURATION_M    /SKN/E_SW_DURATION_M, "Job Running Duration -From start to End
              DURATION_H    /SKN/E_SW_DURATION_H, "Job Running Duration
              DURATION      /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
              JOBNAME       BTCJOB,
              STATE_COLOR   /SKN/E_SW_STATE_COLOR,
              STATUS        BTCSTATUS,
              SDLUNAME      BTCSDLNM,
              LASTCHNAME    BTCJCHNM,
              PROGNAME      BTCPROG,
              PERIODIC      BTCPFLAG,
              STRTDATE      BTCXDATE,   " Job start date
              STRTTIME      BTCXTIME,   " Job start time
              ENDDATE       BTCXDATE,   " Job end date
              ENDTIME       BTCXTIME,   " Job end time         " 11/24++
              SDLDATE       BTCSDLDATE, " Date of job/step scheduling
              SDLSTRTDT     BTCSDATE,   " Planned Start Date for Background Job
              LASTCHDATE    BTCJCHDATE, " Date of last job change
              RELDATE       BTCRELDT,   " Release Date for Background Scheduling
              DATUM         SY-DATUM .
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: LANGU,
                 MANAGE_IN_UTC,
                 NO_DATE_RESTRICTION,
                 DATE_REF_FLD,
                 W_VARIANT,
                 DURATION_UNIT.
  SELECT_MULTY: DURATION_M ,
                DURATION_H ,
                DURATION,
                JOBNAME ,
                STATE_COLOR,
                STATUS,
                SDLUNAME,
                LASTCHNAME,
                PROGNAME,
                PERIODIC,
                STRTDATE,
                STRTTIME,
                ENDDATE,
                ENDTIME,   " 11/24++
                SDLDATE,
                SDLSTRTDT,
                LASTCHDATE ,
                RELDATE,
                DATUM.
  DATA :   IS_GENERAL(1) TYPE C.
  DATA : DATE_FROM LIKE SY-DATUM ,
         BACKDAYS  TYPE I .
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA :  ENDDATE LIKE SY-DATUM,
          ENDTIME LIKE SY-UZEIT.
  DATA : TIME_DIFF TYPE  INT4 .
  DATA : IS_OUT(1) TYPE C.
  DATA :   SY_DATLO LIKE SY-DATLO ,
           SY_TIMLO LIKE SY-TIMLO .
****26-6-16*** Time Diff correction - for Date Ref. Field
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D,
         REF_TIME TYPE T.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA: LV_D_FROM LIKE  SY-DATUM,
        LV_T_FROM LIKE  SY-UZEIT,
        LV_D_TO LIKE  SY-DATUM,
        LV_T_TO LIKE  SY-UZEIT.
  DATA: LV_DATE_REF_FLD_ORG TYPE NAME_FELD.
  LV_DATE_REF_FLD_ORG = LV_DATE_REF_FLD.
  IF LV_DATE_REF_FLD IS INITIAL.
    LV_DATE_REF_FLD = 'STRTDATE'.
  ENDIF.
*****************************************************26-6-16
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_JOBS_N'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
***  endif.
***  check lv_sw_dest is initial.
    "--- Run Cloud Mode -----
  ELSE.
    SY_DATLO = SY-DATUM .        " Appl Server's Date
    SY_TIMLO = SY-UZEIT.
***********************************************
    LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
      RS_DATUM-SIGN   = 'I' .
      RS_DATUM-OPTION = 'GE'.
      BACKDAYS     = T_SELECT-LOW .
      DATE_FROM    = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
      EXIT.
    ENDLOOP.
    IF R_DATUM[] IS INITIAL .  " Set default value
      RS_DATUM-SIGN   = 'I'.
      RS_DATUM-OPTION = 'GE'.
      BACKDAYS     = '1' .  "--- Default
      DATE_FROM    = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
    ENDIF .
**********************************************
*  IF r_strtdate[] IS INITIAL.
*    r_strtdate[] = r_datum[] .
*  ENDIF.
**************** SET DATE REFERENCE FIELD ************************
    IF R_STRTDATE[]   IS INITIAL AND
       R_ENDDATE[]    IS INITIAL AND
       R_SDLDATE[]    IS INITIAL AND
       R_SDLSTRTDT[]  IS INITIAL AND
       R_LASTCHDATE[] IS INITIAL AND
       R_RELDATE[]    IS INITIAL.
      R_STRTDATE[] = R_DATUM[] .
    ENDIF.
    IF LV_NO_DATE_RESTRICTION IS NOT INITIAL.
      REFRESH R_DATUM.
    ENDIF.
    CASE LV_DATE_REF_FLD.
      WHEN 'STRTDATE'.
        R_STRTDATE[] = R_DATUM[].   " Job start date
      WHEN 'ENDDATE'.
        R_ENDDATE[] = R_DATUM[].    " Job end date
      WHEN 'SDLDATE'.
        R_SDLDATE[] = R_DATUM[].    " Date of job/step scheduling
      WHEN 'SDLSTRTDT'.
        R_SDLSTRTDT[] = R_DATUM[].  " Planned Start Date for Background Job
      WHEN 'LASTCHDATE'.
        R_LASTCHDATE[] = R_DATUM[]. " Last change Date for Background Job
      WHEN 'RELDATE'.
        R_RELDATE[] = R_DATUM[].    " Release Date for Background Scheduling
      WHEN OTHERS.
        R_STRTDATE[] = R_DATUM[].   " Job start date
    ENDCASE.
*************************************************************************
*--- Retrieve data
    CLEAR IS_ALERT .
    REFRESH T_DATA .
    SELECT *
       FROM V_OP
       INTO CORRESPONDING FIELDS OF TABLE T_DATA
       WHERE  JOBNAME   IN R_JOBNAME
         AND  STATUS    IN R_STATUS
         AND SDLUNAME   IN R_SDLUNAME
         AND LASTCHNAME IN R_LASTCHNAME
         AND PROGNAME   IN R_PROGNAME
         AND PERIODIC   IN R_PERIODIC
         AND STRTTIME   IN R_STRTTIME
         AND STRTDATE   IN R_STRTDATE
         AND ENDDATE    IN R_ENDDATE
         AND ENDTIME    IN R_ENDTIME
         AND SDLDATE    IN R_SDLDATE
         AND SDLSTRTDT  IN R_SDLSTRTDT
         AND LASTCHDATE IN R_LASTCHDATE
         AND RELDATE    IN R_RELDATE
             .
    SORT T_DATA BY JOBNAME JOBCOUNT STRTDATE STRTTIME.
    DELETE ADJACENT DUPLICATES FROM T_DATA.
    LOOP AT T_DATA.
      SY_TABIX = SY-TABIX .
      CALL FUNCTION '/SKN/F_SW_01_01_JOB_STATUS'
        EXPORTING
          STATUS      = T_DATA-STATUS
        IMPORTING
          STATUS_DESC = T_DATA-STATUS_DESC
          STATE_COLOR = T_DATA-STATE_COLOR.
      CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
        EXPORTING
          STATE_COLOR = T_DATA-STATE_COLOR
        IMPORTING
          STATE_ICON  = T_DATA-STATE_ICON.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDLOOP.
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX .
      IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
*Job Running DURATION - From START DATE(TIME) to END DATE(TIME)******************
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX .
      ENDDATE  = T_DATA-ENDDATE.
      ENDTIME  = T_DATA-ENDTIME.
      IF ENDDATE <= '00000000'.
        ENDDATE = SY-DATUM.
        ENDTIME = SY-UZEIT.
      ENDIF.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = T_DATA-STRTDATE
          T_FROM      = T_DATA-STRTTIME
          D_TO        = ENDDATE
          T_TO        = ENDTIME
          TIME_UNIT   = 'M'
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        T_DATA-DURATION_M = TIME_DIFF .
        T_DATA-DURATION_H = T_DATA-DURATION_M / 60.
        MODIFY T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
    DELETE T_DATA WHERE DURATION_H NOT IN R_DURATION_H.
    DELETE T_DATA WHERE DURATION_M NOT IN R_DURATION_M.
**********************************************************
*********** DURATION in Time Units : From NOW to JOB Start TIME POINT***************
*-- Fill Duration Value
*-- Calculate  Duration (associating to Reference Field (DATE_REF_FLD)
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX.
**********26-6-16 ********************
      CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
      ASSIGN (FLD) TO .
      IF  IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      REF_DATE =  .
      REF_TIME = T_DATA-STRTTIME.
      CASE LV_DATE_REF_FLD.
        WHEN 'STRTDATE'.                  " Job start date
          REF_TIME = T_DATA-STRTTIME .    " Batch job start time
        WHEN 'ENDDATE'.                   " End date
          REF_TIME = T_DATA-ENDTIME .     " End Time
        WHEN 'SDLDATE'.
          REF_TIME = T_DATA-SDLTIME .     " Time of a scheduled job/step
        WHEN 'SDLSTRTDT'.
          REF_TIME = T_DATA-SDLSTRTTM .   " Planned start time for background Job
        WHEN 'LASTCHDATE'.
          REF_TIME = T_DATA-LASTCHTIME .  " Time of last job change
        WHEN 'RELDATE'.
          REF_TIME = T_DATA-RELTIME .     " Release time of scheduled background job
        WHEN OTHERS.
          REF_TIME = T_DATA-STRTTIME.     " Batch job start time
      ENDCASE.
*****************************************26-6-16
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      "--- Calculate Job's Duration if DATE_REF_FLD is not defined
      LV_D_FROM  = REF_DATE.
      LV_T_FROM  = REF_TIME.
      LV_D_TO    = SY_DATLO.
      LV_T_TO    = SY_TIMLO.
      IF LV_DATE_REF_FLD_ORG IS INITIAL.
        LV_D_FROM  = T_DATA-STRTDATE.
        LV_T_FROM  = T_DATA-STRTTIME.
        LV_D_TO    = T_DATA-ENDDATE.
        LV_T_TO    = T_DATA-ENDTIME.
        IF LV_D_TO <= '00000000'.
          LV_D_TO    = SY_DATLO.
          LV_T_TO    = SY_TIMLO.
        ENDIF.
      ENDIF.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = LV_D_FROM
          T_FROM      = LV_T_FROM
          D_TO        = LV_D_TO
          T_TO        = LV_T_TO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        T_DATA-DURATION = TIME_DIFF .
      ELSE.
        T_DATA-DURATION = '999999'.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX .
    ENDLOOP .
    DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  ENDIF.
******************************************************************
  "--  Add Variant
  IF LV_W_VARIANT IS NOT INITIAL.
    DATA: LS_JOB_STEP TYPE /SKN/S_SW_01_JOB_VARIANT,
          LT_JOB_STEP LIKE TABLE OF LS_JOB_STEP,
          LS_JOB_VARIANT TYPE /SKN/S_SW_01_JOB_VARIANT,
          LT_JOB_VARIANT LIKE TABLE OF LS_JOB_VARIANT.
    LOOP AT T_DATA.
      MOVE-CORRESPONDING T_DATA TO LS_JOB_STEP.
      APPEND LS_JOB_STEP TO LT_JOB_STEP.
    ENDLOOP.
    CALL FUNCTION '/SKN/F_SW_01_GET_JOB_VARIANT'
      EXPORTING
        SW_DEST        = LV_SW_DEST
      TABLES
        IT_JOB_STEP    = LT_JOB_STEP
        ET_JOB_VARIANT = LT_JOB_VARIANT
      EXCEPTIONS
        NO_DATA        = 1
        OTHERS         = 2.
    IF SY-SUBRC = 0.
      SORT LT_JOB_VARIANT BY JOBNAME JOBCOUNT STEPCOUNT.
      LOOP AT T_DATA.
        SY_TABIX = SY-TABIX.
        READ TABLE LT_JOB_VARIANT INTO LS_JOB_VARIANT
                                  WITH KEY JOBNAME   = T_DATA-JOBNAME
                                           JOBCOUNT  = T_DATA-JOBCOUNT
                                           STEPCOUNT = T_DATA-STEPCOUNT
                                  BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          MOVE-CORRESPONDING LS_JOB_VARIANT TO T_DATA.
          MODIFY T_DATA INDEX SY_TABIX .
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
