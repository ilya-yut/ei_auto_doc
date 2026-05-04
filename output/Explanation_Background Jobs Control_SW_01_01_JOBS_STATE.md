# Exception Indicator: Background Jobs Control - SW_01_01_JOBS_STATE

## General Overview

This Exception Indicator monitors background job executions as administrators see them in job overview and scheduling contexts, combining calendar windows, runtime measures, status interpretation, and optional variant detail.

This EI serves as an essential control for Basis and application operations by:
- Surfacing jobs that exceed expected runtime or sit in non-success status bands long enough to threaten batch chains
- Highlighting concentration of work by scheduler, program, target system, or periodicity when queues grow unevenly
- Supporting release and close activities when job traffic must be validated after transports, data loads, or infrastructure changes
- Giving teams a single exception-oriented slice instead of manually reconciling lists from multiple monitoring tools
- Helping internal audit demonstrate that critical batch windows stayed within agreed operational boundaries

Typical use includes daily operations checks, incident triage after failed overnight processing, and capacity discussions before peak periods. Teams act on results in standard job monitoring transactions, then adjust schedules, variants, or system resources as appropriate.

The routine reads consolidated job header data from the job overview selection used in the ABAP, then enriches rows with status descriptions, color buckets, and optional step variant information.


## Problem Description

Failure to monitor background job execution health creates multiple risks across service levels, financial timing, and operational control:

**Service Level and Processing Risks**
- Long-running or stuck jobs can delay dependent steps without a shared prioritized view for on-call staff
- Silent growth of failed or canceled jobs erodes confidence in overnight settlement, replication, or interface chains
- Periodic and ad-hoc workloads become hard to compare when no consistent exception list exists across systems

**Financial and Closing Risks**
- Accounting or logistics close steps that rely on batch posting may miss deadlines when job backlog issues are discovered late
- Retroactive corrections become harder when schedulers cannot prove which jobs were active during a sensitive window

**Operational Visibility and Accountability Risks**
- Application owners cannot see whether their programs dominate failure or duration statistics relative to peers
- Management lacks objective evidence for staffing or infrastructure decisions tied to observed job volume and severity
- Cross-team hand-offs weaken when schedulers, Basis, and functional owners each inspect different ad-hoc extracts

## Suggested Resolution

**Immediate Response**
- Review each flagged job for program, scheduler, status, and runtime context to judge business severity and next owner
- Open standard job monitoring for the same selection to validate current state, logs, and any dependent steps
- Confirm whether failures are transient infrastructure issues or repeatable application defects requiring code or data fixes
- Capture accountable roles and target resolution times when jobs touch financial posting, inventory, or regulatory extracts

**System Assessment**
- Segment results by status, color bucket, program name, scheduler, and periodicity to see where volume concentrates
- Compare current failure and duration profiles to prior monitoring cycles after releases, data migrations, or hardware changes
- Examine optional variant enrichment results when enabled to see whether misconfigured variants drive concentrated failures
- Validate that calendar and reference-date choices match the intended business window so exceptions are not dominated by benign history

**Corrective Actions**
- Reschedule, split, or optimize jobs; fix variants and selection criteria; or adjust job classes when systemic overload appears
- Coordinate with development when specific programs or interfaces generate recurring errors or excessive runtimes
- Document remediation for audit when jobs affected regulated reporting or statutory batch deadlines
- Tune monitoring parameters after root-cause review so known benign patterns are excluded without hiding genuine risk
- Schedule recurring monitoring after major landscape changes so new scheduling patterns are validated early


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back days |  | 0 | 0 |  |  |
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
| 12 | JOBNAME | JOB NAME | CHAR | 32 | 0 | BTCJOB | CHAR32 |
| 13 | LASTCHDATE | Last job change | DATS | 8 | 0 | BTCJCHDATE | SYDATS |
| 14 | LASTCHNAME | Last job change/made by | CHAR | 12 | 0 | BTCJCHNM | CHAR12 |
| 15 | LASTCHTIME | Last job change | TIMS | 6 | 0 | BTCJCHTIME | SYTIME |
| 16 | NO_DATE_RESTRICTION | No date restriction |  | 0 | 0 |  |  |
| 17 | PERIODIC | Periodic job | CHAR | 1 | 0 | BTCPFLAG | CHAR1 |
| 18 | PRDHOURS | Periodic job | NUMC | 2 | 0 | BTCPHOUR | NUM02 |
| 19 | PRDMINS | Periodic job | NUMC | 2 | 0 | BTCPMIN | NUM02 |
| 20 | PROGNAME | Rept Name | CHAR | 40 | 0 | BTCPROG | PROGNAME |
| 21 | RELDATE | Scheduled release | DATS | 8 | 0 | BTCRELDT | SYDATS |
| 22 | RELTIME | Scheduled release | TIMS | 6 | 0 | BTCRELTM | SYTIME |
| 23 | RELUNAME | User that released scheduled batch job | CHAR | 12 | 0 | BTCRELNM | CHAR12 |
| 24 | SDLDATE | Schedule date | DATS | 8 | 0 | BTCSDLDATE | SYDATS |
| 25 | SDLSTRTDT | START DATE | DATS | 8 | 0 | BTCSDATE | SYDATS |
| 26 | SDLSTRTTM | START TIME | TIMS | 6 | 0 | BTCSTIME | SYTIME |
| 27 | SDLTIME | Schedule date | TIMS | 6 | 0 | BTCSDLTIME | SYTIME |
| 28 | SDLUNAME | Job Scheduler | CHAR | 12 | 0 | BTCSDLNM | CHAR12 |
| 29 | STATE_COLOR | STATE COLOR | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 30 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 31 | STATUS | JOB STATUS | CHAR | 1 | 0 | BTCSTATUS | CHAR1 |
| 32 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 33 | STEPCOUNT | Step no. | INT4 | 10 | 0 | BTCSTEPCNT |  |
| 34 | STRTDATE | Execution Date | DATS | 8 | 0 | BTCXDATE | SYDATS |
| 35 | STRTTIME | Start Time | TIMS | 6 | 0 | BTCXTIME | SYTIME |
| 36 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 37 | VARIANT | Variant | CHAR | 14 | 0 | BTCVARIANT | CHAR14 |
| 38 | W_VARIANT | X - Include Program Variant |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 38 parameters listed in the Parameters Reference Table when tuning this EI; each influences which background jobs are retrieved, enriched, and evaluated for alerting.

**BACKDAYS** (Back days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BTCSYSTEM** (Targ. Sys of a Backg. Job)

For distributed landscapes, targ. sys of a backg. job on BTCSYSTEM often anchors which target system or scheduler dimension appears in results.

**DATE_REF_FLD** (Date Ref. Field)

Selects which job timestamp column receives the default lookback window from BACKDAYS before rows are read from the job overview source.

**DATE_REF_FLD Options:**
- **STRTDATE** — Actual job start date (default when the parameter is left initial in code).
- **ENDDATE** — Job end date.
- **SDLDATE** — Schedule date for the job or step.
- **SDLSTRTDT** — Planned start date for the background job.
- **LASTCHDATE** — Date of the last job change.
- **RELDATE** — Scheduled release date.
- **OTHERS** — Treated like STRTDATE in the selection branch when an unexpected value is supplied.

**DURATION** (Duration from Start in d.unit)

Helps monitoring stay readable by requiring duration from start in d.unit (DURATION) to match organizational or technical selectors when set.

**DURATION_H** (Execution Time(Hours))

Stabilizes week-over-week metrics by fixing execution time(hours) (DURATION_H) while allowing runtime or age thresholds to move.

**DURATION_M** (Execution Time(Minutes))

Supports escalation where execution time(minutes) on DURATION_M signals ownership for follow-up between Basis and application teams.

**DURATION_UNIT** (Duration Unit(D/H/M))

Unit for the relative-age measure from the reference timestamp chosen by DATE_REF_FLD to the evaluation moment.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**ENDDATE** (Execution Date)

Supports batch operations review by evaluating execution date through ENDDATE for each job header candidate.

**ENDTIME** (Start Time)

When tightened, start time (ENDTIME) removes jobs that would otherwise dilute attention from long-running or failed work.

**JOBCLASS** (Job class)

Uses job class from the job overview context so only records with JOBCLASS inside declared intervals contribute to alerts.

**JOBCOUNT** (Job No.)

Interprets job no. as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on JOBCOUNT.

**JOBNAME** (JOB NAME)

Ensures reporting on programs and schedulers respects job name constraints carried by JOBNAME.

**LASTCHDATE** (Last job change)

For distributed landscapes, last job change on LASTCHDATE often anchors which target system or scheduler dimension appears in results.

**LASTCHNAME** (Last job change/made by)

Mirrors how administrators slice job lists: last job change/made by (LASTCHNAME) is one lever that shapes which rows are comparable run over run.

**LASTCHTIME** (Last job change)

Ensures reporting on programs and schedulers respects last job change constraints carried by LASTCHTIME.

**NO_DATE_RESTRICTION** (No date restriction)

When set active, clears the default calendar window so date-driven selection does not constrain the job list beyond other filters.

**NO_DATE_RESTRICTION Options:**
- **X** — Suppress the default lookback window application for date selection.
- **Empty or blank** — Apply the standard lookback and date-reference mapping from BACKDAYS and DATE_REF_FLD.

**PERIODIC** (Periodic job)

Reflects real job-monitoring usage where periodic job on PERIODIC is routinely restricted to a single productive window.

**PRDHOURS** (Periodic job)

Helps monitoring stay readable by requiring periodic job (PRDHOURS) to match organizational or technical selectors when set.

**PRDMINS** (Periodic job)

Pairs with duration logic: once PRDMINS passes list selection, elapsed or runtime-based measures still must fit configured bands.

**PROGNAME** (Rept Name)

Ensures reporting on programs and schedulers respects rept name constraints carried by PROGNAME.

**RELDATE** (Scheduled release)

For operations, scheduled release on RELDATE indicates whether a line belongs in the current monitoring pass versus historical noise.

**RELTIME** (Scheduled release)

Pairs with duration logic: once RELTIME passes list selection, elapsed or runtime-based measures still must fit configured bands.

**RELUNAME** (User that released scheduled batch job)

Stabilizes week-over-week metrics by fixing user that released scheduled batch job (RELUNAME) while allowing runtime or age thresholds to move.

**SDLDATE** (Schedule date)

After the job list is built, rows are removed unless schedule date on SDLDATE still satisfies the active filter criteria.

**SDLSTRTDT** (START DATE)

Documents expected operator behavior—start date on SDLSTRTDT should be set when that dimension is part of the control objective.

**SDLSTRTTM** (START TIME)

Allows phased rollout: first widen SDLSTRTTM for start time, then tighten once baseline noise is understood.

**SDLTIME** (Schedule date)

Stabilizes week-over-week metrics by fixing schedule date (SDLTIME) while allowing runtime or age thresholds to move.

**SDLUNAME** (Job Scheduler)

Works downstream of the initial selection so job scheduler on SDLUNAME still participates in row-level deletion rules.

**STATE_COLOR** (STATE COLOR)

Filters jobs after status enrichment so only rows whose color bucket matches the configured severity set remain.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional domain literals may appear per system configuration for neutral or inactive states.

**STATE_ICON** (State Icon)

Ensures reporting on programs and schedulers respects state icon constraints carried by STATE_ICON.

**STATUS** (JOB STATUS)

Restricts rows to the batch job lifecycle codes present in the technical job status field before color and duration filters run.

**STATUS Options:**
- **P** — Scheduled (waiting in the scheduler queue).
- **S** — Released for execution.
- **Y** — Active / running.
- **F** — Finished successfully.
- **A** — Aborted or canceled with error.
- Additional single-character codes may appear per your SAP batch status domain; align selections with transaction SM37 display values.

**STATUS_DESC** (SW Message)

Allows phased rollout: first widen STATUS_DESC for sw message, then tighten once baseline noise is understood.

**STEPCOUNT** (Step no.)

Supports escalation where step no. on STEPCOUNT signals ownership for follow-up between Basis and application teams.

**STRTDATE** (Execution Date)

Narrows background job rows where execution date (STRTDATE) must match the configured selection for this monitor.

**STRTTIME** (Start Time)

Captures edge cases where start time (STRTTIME) must be non-default to reproduce a customer-specific monitoring scenario.

**USER_FLD** (Dynamic Recipient User Field)

Optional dynamic recipient or routing field used by the monitor framework when populated for distribution or extensions.

**USER_FLD Options:**
No fixed USER_FLD value list is defined in the available code for this EI.

**VARIANT** (Variant)

Supports batch operations review by evaluating variant through VARIANT for each job header candidate.

**W_VARIANT** (X - Include Program Variant)

When set active, triggers enrichment that reads step-level variant information into the result structure for jobs that pass earlier filters.

**W_VARIANT Options:**
- **X** — Include program variant details per step where the enrichment call returns data.
- **Empty or blank** — Skip variant enrichment for lighter extracts.


### Parameter Relationships

How parameter combinations work together

**BACKDAYS** and **DATE_REF_FLD** work together to build the default calendar window applied to the chosen job timestamp column before the main read, matching the fixed wording used under BACKDAYS in the Parameter Configuration Guidelines. **NO_DATE_RESTRICTION**, when active, suppresses that default window so other filters alone shape the extract—use it only when the business case truly requires an unrestricted calendar slice.

**DURATION_H** and **DURATION_M** filter on measured wall-clock runtime from each job’s start to end timestamps after the list is retrieved; they are independent of the relative-age **DURATION** field, which instead measures elapsed time from the reference timestamp implied by **DATE_REF_FLD** to the evaluation clock using **DURATION_UNIT**.

**W_VARIANT** and **VARIANT** interact: the flag enables enrichment that fills variant-related columns for qualifying steps, while **VARIANT** (when restricted) still limits which jobs remain in scope before that enrichment runs.

**STATE_COLOR** and **STATUS** are applied after status-to-description mapping: status narrows the raw job lifecycle set, while color keeps only the severity buckets you want in the final alert list.


### Default Values

- **BACKDAYS** - 1 from the default applied when no overriding selection supplies a different lookback before the main read.
- **DURATION_UNIT** - M from the preset before duration calculations when the caller does not override it.
- **DURATION** - initial — when the duration selection range is left empty, the routine does not filter rows out by the relative-age measure until a range is supplied.

### Practical Example of Parameter Configuration

**Use Case 1: Recent failures on actual start date**

**Purpose:** List jobs started in the last week that are still in a failed status and exceed a short relative-age threshold in hours.

```
BACKDAYS = 7
DATE_REF_FLD = STRTDATE
STATUS = F
STATE_COLOR = R
DURATION_UNIT = H
DURATION = 4
```

**Use Case 2: Scheduler accountability slice**

**Purpose:** Review jobs scheduled by a specific batch user with default start-date reference and variant detail enabled.

```
SDLUNAME = BATCHADMIN
BACKDAYS = 3
DATE_REF_FLD = STRTDATE
W_VARIANT = X
```

**Use Case 3: Program family with runtime guard**

**Purpose:** Monitor a reporting program prefix for long wall-clock runs while keeping a modest default lookback.

```
PROGNAME = ZFI_MONTHEND*
DURATION_H = 2
BACKDAYS = 14
JOBCLASS = A
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
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
