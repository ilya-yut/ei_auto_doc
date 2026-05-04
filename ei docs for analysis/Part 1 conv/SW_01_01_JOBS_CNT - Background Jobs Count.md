Exception Indicator: Background Jobs Count (SW_01_01_JOBS_CNT )


# General Overview

This Exception Indicator (EI) provides quantitative information (count) about SAP jobs that meet specific criteria within a specified time period and filtering conditions. Jobs in SAP systems may run for long periods and it's important to monitor them to avoid excessive system resource consumption and ensure proper business process execution.

This EI acts as a comprehensive job monitoring indicator that provides count-based analysis for:

Job volume monitoring indicating system workload and capacity planning needs

Unusual job count patterns that may signal underlying technical problems, scheduling issues, or business process changes

Threshold-based alerting for validation that job counts are within expected operational ranges

Job count trending where administrators need aggregate data for capacity and performance analysis

As part of daily monitoring, it is the responsibility of the basis administrator to analyze job patterns and take necessary actions to optimize system performance. A few examples of job monitoring scenarios are long-running jobs, failed job counts, periodic job scheduling issues etc. This EI provides count-based job analysis rather than individual job details, focusing on aggregate job metrics for system health monitoring. The alert output contains only the job count number without any detailed job information.


# Problem Description

Abnormal job counts indicate system issues causing:

System Performance Issues

Resource exhaustion from excessive concurrent jobs

Poor job scheduling causing system bottlenecks

Infrastructure problems affecting job execution capacity

Process Disruptions

Business process failures due to missing or failed jobs

Scheduling conflicts preventing critical job execution

Performance degradation during peak job processing periods

Business Continuity

Data processing delays affecting business operations

SLA violations due to job processing delays

Operational efficiency issues from suboptimal job scheduling


# Suggested Resolution

Immediate Response

Investigate job patterns using SM37 transaction

Check system resource levels and job queue capacity

Review job scheduling and dependencies for conflicts

System Assessment

Monitor job execution times and resource consumption

Check background processing work processes availability

Analyze job frequency patterns and peak usage times

Corrective Actions

Optimize job scheduling and reduce concurrent job loads

Adjust system parameters for background processing

Establish proactive job monitoring and capacity planning


# Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 2 | DATE_REF_FLD | Date Ref. Field |  | 0 | 0 |  |  |
| 3 | DURATION | Duration in time units |  | 0 | 0 |  |  |
| 4 | DURATION_H | Execution Time(Hours) |  | 0 | 0 |  |  |
| 5 | DURATION_M | Execution Time(Minutes) |  | 0 | 0 |  |  |
| 6 | DURATION_UNIT | Duration Unit(D/H/M) |  | 0 | 0 |  |  |
| 7 | ENDDATE | Job end date |  | 0 | 0 |  |  |
| 8 | ENDTIME | Batch job end time |  | 0 | 0 |  |  |
| 9 | JOBNAME | JOB NAME |  | 0 | 0 |  |  |
| 10 | JOBS_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 11 | LASTCHNAME | Last job change/made by |  | 0 | 0 |  |  |
| 12 | NO_DATE_RESTRICTION | No date restriction |  | 0 | 0 |  |  |
| 13 | PERIODIC | Periodic job | CHAR | 1 | 0 | BTCPFLAG | CHAR1 |
| 14 | PROGNAME | Rept Name | CHAR | 40 | 0 | BTCPROG | PROGNAME |
| 15 | SDLSTRTDT | START DATE |  | 0 | 0 |  |  |
| 16 | SDLSTRTTM | START TIME |  | 0 | 0 |  |  |
| 17 | SDLUNAME | JOB SCHEDULER |  | 0 | 0 |  |  |
| 18 | STATE_COLOR | STATE COLOR |  | 0 | 0 |  |  |
| 19 | STATUS | JOB STATUS |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 19 parameters listed in the Parameters Reference Table when tuning this EI; each influences how many background jobs match the delegated selection and whether the returned count falls inside the JOBS_CNT interval.

BACKDAYS (Back days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

Backdays is based on the DATE_REF_FLD field.

DATE_REF_FLD (Date Ref. Field)

Selects which job timestamp column receives the default lookback window from BACKDAYS inside the delegated background-job monitor.

DATE_REF_FLD Options:

·        STRTDATE — Actual job start date (default when the parameter is left initial in the delegated logic).

·        ENDDATE — Job end date.

·        SDLDATE — Schedule date for the job or step.

·        SDLSTRTDT — Planned start date for the background job.

·        LASTCHDATE — Date of the last job change.

·        RELDATE — Scheduled release date.

·        OTHERS — Treated like STRTDATE in the selection branch when an unexpected value is supplied.

DURATION (Duration in time units)

Valuable when comparing volume before and after a release—hold duration in time units on DURATION constant while varying JOBS_CNT.

DURATION_H (Execution Time(Hours))

Captures edge cases where execution time(hours) (DURATION_H) must be non-default to reproduce a customer-specific counting scenario.

DURATION_M (Execution Time(Minutes))

Interprets execution time(minutes) as part of the selection contract passed through to the job monitor: open ranges follow its defaults; restricted ranges apply strict matching on DURATION_M.

DURATION_UNIT (Duration Unit(D/H/M))

Unit for the relative-age measure computed inside the delegated job monitor from the reference timestamp to the evaluation moment.

DURATION_UNIT Options:

·        H — Hours.

·        M — Minutes (preset in the delegated routine when not overridden).

·        D — Days.

·        F — Full-day style counting where applicable to the duration helper.

ENDDATE (Job end date)

Reflects real batch-monitoring usage where job end date on ENDDATE is routinely restricted to a single productive window.

ENDTIME (Batch job end time)

Interprets batch job end time as part of the selection contract passed through to the job monitor: open ranges follow its defaults; restricted ranges apply strict matching on ENDTIME.

JOBNAME (JOB NAME)

Treats job name as a discriminator between similar jobs that would otherwise change the counted volume in unexpected ways.

JOBS_CNT (Count)

Defines the inclusive numeric interval for acceptable job counts: the EI raises an alert only when the delegated monitor returns a line count that falls inside this range and the delegated alert flag is set.

LASTCHNAME (Last job change/made by)

Allows phased rollout: first widen LASTCHNAME for last job change/made by, then tighten JOBS_CNT once baseline volumes are understood.

NO_DATE_RESTRICTION (No date restriction)

When set active, suppresses the default calendar window inside the delegated monitor so counting reflects only the remaining filters.

NO_DATE_RESTRICTION Options:

·        X — Suppress default lookback application for the date-driven portion of the delegated selection.

·        Empty or blank — Apply the standard lookback and date-reference mapping from BACKDAYS and DATE_REF_FLD.

PERIODIC (Periodic job)

Allows phased rollout: first widen PERIODIC for periodic job, then tighten JOBS_CNT once baseline volumes are understood.

PROGNAME (Rept Name)

For operations, rept name on PROGNAME indicates whether a job line belongs in the population whose size is compared to JOBS_CNT.

SDLSTRTDT (START DATE)

Connects to alert semantics: the alert flag from the delegated run is preserved only when the resulting population size lies inside JOBS_CNT.

SDLSTRTTM (START TIME)

Connects to alert semantics: the alert flag from the delegated run is preserved only when the resulting population size lies inside JOBS_CNT.

SDLUNAME (JOB SCHEDULER)

Separates test noise from productive work when job scheduler on SDLUNAME correlates with scheduler or program attributes.

STATE_COLOR (STATE COLOR)

Filters the delegated job list by color bucket before counting so only severity-aligned rows contribute to the final population size.

STATE_COLOR Options:

·        R — Red (error or failed-style outcomes).

·        G — Green (successful outcomes).

·        Y — Yellow (warning or in-process outcomes).

·        Additional domain literals may appear per system configuration for neutral or inactive states.

STATUS (JOB STATUS)

Restricts the delegated population to the configured batch job lifecycle codes before counting and threshold evaluation.

STATUS Options:

·        P — Scheduled (waiting in the scheduler queue).

·        S — Released for execution.

·        Y — Active / running.

·        F — Finished successfully.

·        A — Aborted or canceled with error.

·        Additional single-character codes may appear per your SAP batch status domain.


### Parameter Relationships

How parameter combinations work together

BACKDAYS, DATE_REF_FLD, and NO_DATE_RESTRICTION are interpreted first inside the delegated background-job monitor: together they define whether and how a default calendar window is applied to the chosen reference timestamp column. The BACKDAYS wording in section 04 matches that delegated behavior.

DURATION, DURATION_UNIT, and the separate runtime measures DURATION_H / DURATION_M all pass through to the same delegated routine, so age and runtime filters shrink or grow the population before the wrapper counts lines.

STATUS, STATE_COLOR, and the various job key fields (for example JOBNAME, PROGNAME, SDLUNAME) narrow the same working set the detailed monitor would use; only jobs that survive those filters contribute to the line count compared against JOBS_CNT.

JOBS_CNT is evaluated after delegation: the wrapper keeps the delegated alert only when the resulting table size lies inside the configured count interval and outputs a single row carrying that count.


### Default Values

·        BACKDAYS - 1 from the delegated monitor’s default when the caller does not override the lookback before the internal job read.

·        DURATION_UNIT - M from the delegated preset before duration calculations when the caller does not override it.

·        DURATION - initial — when the duration selection range is left empty, the delegated routine does not filter rows out by the relative-age measure until a range is supplied.


### Practical Example of Parameter Configuration

Use Case 1: Failed-job volume band

Purpose: Alert only when between 5 and 50 failed jobs match a one-week window on actual start date.

STATUS = F
 BACKDAYS = 7
 DATE_REF_FLD = STRTDATE
 JOBS_CNT = 10
 STATE_COLOR = R



Use Case 2: Program family watch

Purpose: Track volume for a month-end program prefix with hourly runtime bounds passed through to the delegated monitor.

PROGNAME = ZFI_CLOSE*
 DURATION_H = 1
 DURATION_M = 30
 PERIODIC = X



Use Case 3: Scheduler-centric slice

Purpose: Compare counts for one batch scheduler with planned start date as the reference and a modest lookback.

SDLUNAME = BATCHADMIN
 DATE_REF_FLD = SDLSTRTDT
 BACKDAYS = 3
 JOBS_CNT = 500
 JOBNAME = Z*



 EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_01_JOBS_N_CNT | JOBS_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_01_JOBS_N_CNT.
 *"----------------------------------------------------------------------
 *"*"Local Interface:
 *"  EXPORTING
 *" 	VALUE(IS_ALERT) TYPE  CHAR1
 *"  TABLES
 *"  	T_SELECT STRUCTURE  RSSELECT OPTIONAL
 *"  	T_DATA STRUCTURE  /SKN/S_SW_01_01_JOBS_N_CNT OPTIONAL
 *"----------------------------------------------------------------------
 DATA : LV_ALERT TYPE  CHAR1.
 DATA : LS_DATA TYPE /SKN/S_SW_01_01_JOBS,
    	LT_DATA LIKE TABLE OF LS_DATA.
 DATA : LV_CNT TYPE I.
 DATA_MULTY: JOBS_CNT /SKN/E_SW_CNT.
 SELECT_MULTY: JOBS_CNT.
    REFRESH T_DATA.
    CALL FUNCTION '/SKN/F_SW_01_01_JOBS_N'
 	IMPORTING
    	IS_ALERT   	= LV_ALERT
  	TABLES
        T_SELECT   	= T_SELECT
    	T_DATA     	= LT_DATA.
 	IS_ALERT = LV_ALERT.
 	DESCRIBE TABLE LT_DATA LINES LV_CNT.
 	IF LV_CNT IN R_JOBS_CNT.
   	T_DATA-JOBS_CNT = LV_CNT.
   	APPEND T_DATA.
   	IS_ALERT = LV_ALERT.
 	ELSE.
   	CLEAR IS_ALERT.
 	ENDIF.
 ENDFUNCTION.
```
