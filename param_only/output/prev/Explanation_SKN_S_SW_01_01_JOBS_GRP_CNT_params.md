# Parameters: SKN_S_SW_01_01_JOBS_GRP_CNT

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 2 | DATE_REF_FLD | Date Ref. Field |  | 0 | 0 |  |  |
| 3 | DURATION | Duration in time units |  | 0 | 0 |  |  |
| 4 | DURATION_H | Execution Time(Hours) |  | 0 | 0 |  |  |
| 5 | DURATION_M | Execution Time(Minutes) |  | 0 | 0 |  |  |
| 6 | DURATION_UNIT | Duration Unit(D/H/M) |  | 0 | 0 |  |  |
| 7 | JOBNAME | JOB NAME | CHAR | 32 | 0 | BTCJOB | CHAR32 |
| 8 | JOBS_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 9 | LASTCHNAME | Last job change/made by |  | 0 | 0 |  |  |
| 10 | NO_DATE_RESTRICTION | No date restriction |  | 0 | 0 |  |  |
| 11 | PERIODIC | Periodic job | CHAR | 1 | 0 | BTCPFLAG | CHAR1 |
| 12 | PROGNAME | Rept Name | CHAR | 40 | 0 | BTCPROG | PROGNAME |
| 13 | SDLSTRTDT | START DATE |  | 0 | 0 |  |  |
| 14 | SDLSTRTTM | START TIME |  | 0 | 0 |  |  |
| 15 | SDLUNAME | JOB SCHEDULER |  | 0 | 0 |  |  |
| 16 | STATE_COLOR | STATE COLOR |  | 0 | 0 |  |  |
| 17 | STATUS | JOB STATUS |  | 0 | 0 |  |  |
| 18 | VARIANT | Variant | CHAR | 14 | 0 | BTCVARIANT | CHAR14 |
| 19 | W_VARIANT | X - Include Program Variant |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 19 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Back days):

Number of days to look back from today when no explicit date range is supplied. The EI uses this to define the start of the monitoring window for background job selection.

**DATE_REF_FLD** (Date Ref. Field):

Date field used as the reference for the monitoring window (e.g. job start date). The EI uses this when filtering jobs by date range.

**DATE_REF_FLD Options:**

- **SDLSTRTDT** (start date), **STRTDATE** (job start date), or other date fields available in the job data—use the field that matches the EI's job structure.

**DURATION** (Duration in time units):

Duration value used to filter jobs by execution time (e.g. minimum or maximum duration). The EI compares job duration to this value in the unit given by DURATION_UNIT.

**DURATION_H** (Execution Time(Hours)):

Execution time in hours. Used to filter or display job run duration in hours.

**DURATION_M** (Execution Time(Minutes)):

Execution time in minutes. Used to filter or display job run duration in minutes.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit in which duration is interpreted (hours, minutes, or days). The EI uses this when comparing or filtering by DURATION.

**DURATION_UNIT Options:**

- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**JOBNAME** (JOB NAME):

Background job name. The EI aggregates job counts by job name (and variant) and uses this to filter which jobs are included in the result set.

**JOBS_CNT** (Count):

Count of jobs per group (job name and variant). Populated in the output after aggregation; used to filter by minimum or maximum count.

**LASTCHNAME** (Last job change/made by):

User who last changed the job. The EI uses this to filter jobs by the person who last modified them.

**NO_DATE_RESTRICTION** (No date restriction):

When set, the EI does not apply a date restriction to job selection; all jobs matching other criteria are considered.

**PERIODIC** (Periodic job):

Indicates whether the job is periodic. The EI uses this to filter by periodic vs one-time jobs.

**PERIODIC Options:**

- **X**: Periodic job
- ** ** (space): One-time or not periodic

**PROGNAME** (Rept Name):

Report or program name. The EI uses this to filter jobs by the program executed.

**SDLSTRTDT** (START DATE):

Scheduled start date of the job. The EI uses this to restrict the job selection to the given date range.

**SDLSTRTTM** (START TIME):

Scheduled start time of the job. The EI uses this (together with SDLSTRTDT) to restrict the job selection to the given time range.

**SDLUNAME** (JOB SCHEDULER):

User name of the job scheduler. The EI uses this to filter jobs by the user who scheduled them.

**STATE_COLOR** (STATE COLOR):

Status color of the job. Used to filter or display job status in the result set.

**STATUS** (JOB STATUS):

Job status (e.g. released, finished, cancelled). The EI uses this to filter which job statuses are included in the result set.

**VARIANT** (Variant):

Variant name within a job step. The EI aggregates by job name and variant and uses this to filter which variants are included.

**W_VARIANT** (X - Include Program Variant):

When set, the EI includes program variant in the grouping or filtering logic.

**W_VARIANT Options:**

- **X**: Include program variant
- ** ** (space): Do not include


### Parameter Relationships

**Time and duration parameters:**

- **BACKDAYS**, **SDLSTRTDT**, and **SDLSTRTTM** define the monitoring window: when no explicit date/time range is supplied, the EI uses BACKDAYS to compute the start; when supplied, SDLSTRTDT and SDLSTRTTM restrict which background jobs are included by scheduled start date and time.
- **DURATION**, **DURATION_UNIT**, **DURATION_H**, and **DURATION_M** work together: DURATION is a numeric value in the unit given by DURATION_UNIT; DURATION_H and DURATION_M can be used to filter or display execution time in hours and minutes. The EI uses these when filtering jobs by execution duration.

**Job and variant parameters:**

- **JOBNAME** and **VARIANT** work together: the EI aggregates job counts by job name and variant; both are used to filter and group the result set.
- **JOBS_CNT** filters the aggregated count (e.g. minimum or maximum number of job runs per JOBNAME/VARIANT). It works with **JOBNAME** and **VARIANT** to narrow the result.

**Date reference:**

- **DATE_REF_FLD** specifies which date field is used as the reference for the monitoring window (e.g. job start date). It works with **BACKDAYS** or explicit date range parameters when the EI evaluates the job selection period.


### Default Values

- **LANGU** — Default: system language (when used for texts). The called function may use SY-LANGU when LANGU is initial.

**Note:** This EI delegates to another function for job selection; default behavior for date range and duration depends on that function when BACKDAYS or date parameters are not supplied.

### Practical Configuration Examples

**Use Case 1: Last 10 days, filter by job count**
```
BACKDAYS = 10
JOBS_CNT = 1 - 999999
DURATION_UNIT = D
```
**Purpose:** Monitor background jobs that ran in the last 10 days, with duration in days, and filter by job count range. Useful for volume and frequency checks.

**Use Case 2: Specific job name and variant**
```
JOBNAME = Z_MY_JOB
VARIANT = *
SDLSTRTDT = 20240101 - 20240131
STATUS = F
```
**Purpose:** Count runs of a specific job (Z_MY_JOB) for any variant in January 2024 with status Finished. Supports audit and job monitoring.

**Use Case 3: Periodic jobs and scheduler**
```
PERIODIC = X
SDLUNAME = JOBUSER
BACKDAYS = 14
PROGNAME = RS*
DURATION_UNIT = H
```
**Purpose:** Monitor periodic jobs scheduled by JOBUSER in the last 14 days for programs starting with RS, with duration in hours. Used for periodic job oversight.

**Use Case 4: Multiple filters**
```
BACKDAYS = 7
JOBNAME = Z*
VARIANT = PROD*
JOBS_CNT = 2 - 100
DURATION_UNIT = D
DATE_REF_FLD = SDLSTRTDT
```
**Purpose:** Combined monitoring over the last 7 days for custom jobs (Z*) with variants starting with PROD, job count between 2 and 100, using start date as reference. Suitable for focused job analysis.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_JOBS_GRP_CNT | JOBNAME | Background job name | CHAR(32) | BTCJOB |
| /SKN/S_SW_01_01_JOBS_GRP_CNT | JOBS_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |
| /SKN/S_SW_01_01_JOBS_GRP_CNT | VARIANT | Name of variant within a step | CHAR(14) | BTCVARIANT |
