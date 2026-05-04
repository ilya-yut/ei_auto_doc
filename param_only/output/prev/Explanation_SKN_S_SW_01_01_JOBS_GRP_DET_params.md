# Parameters: SKN_S_SW_01_01_JOBS_GRP_DET

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 2 | BTCSYSTEM | Targ. Sys of a Backg. Job | CHAR | 32 | 0 | BTCTGTSYS | TEXT32 |
| 3 | DATE_REF_FLD | Date Ref. Field |  | 0 | 0 |  |  |
| 4 | DURATION | Duration from Start in d.unit | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 5 | DURATION_H | Execution Time(Hours) | NUMC | 6 | 0 | /SKN/E_SW_DURATION_H |  |
| 6 | DURATION_M | Execution Time(Minutes) | NUMC | 6 | 0 | /SKN/E_SW_DURATION_M |  |
| 7 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 8 | ENDDATE | Start date | DATS | 8 | 0 | BTCXDATE | SYDATS |
| 9 | ENDTIME | Start time | TIMS | 6 | 0 | BTCXTIME | SYTIME |
| 10 | JOBCLASS | Job class | CHAR | 1 | 0 | BTCJOBCLAS | CHAR1 |
| 11 | JOBCOUNT | Job no. | CHAR | 8 | 0 | BTCJOBCNT | CHAR8 |
| 12 | JOBNAME | JOB NAME | CHAR | 32 | 0 | BTCJOB | CHAR32 |
| 13 | JOBS_CNT | Jobs Count |  | 0 | 0 |  |  |
| 14 | LASTCHDATE | Last job change | DATS | 8 | 0 | BTCJCHDATE | SYDATS |
| 15 | LASTCHNAME | Last job change/made by | CHAR | 12 | 0 | BTCJCHNM | CHAR12 |
| 16 | LASTCHTIME | Last job change | TIMS | 6 | 0 | BTCJCHTIME | SYTIME |
| 17 | NO_DATE_RESTRICTION | No date restriction |  | 0 | 0 |  |  |
| 18 | PERIODIC | Periodic job | CHAR | 1 | 0 | BTCPFLAG | CHAR1 |
| 19 | PRDHOURS | Periodic job | NUMC | 2 | 0 | BTCPHOUR | NUM02 |
| 20 | PRDMINS | Periodic job | NUMC | 2 | 0 | BTCPMIN | NUM02 |
| 21 | PROGNAME | Rept Name | CHAR | 40 | 0 | BTCPROG | PROGNAME |
| 22 | RELDATE | Scheduled release | DATS | 8 | 0 | BTCRELDT | SYDATS |
| 23 | RELTIME | Scheduled release | TIMS | 6 | 0 | BTCRELTM | SYTIME |
| 24 | RELUNAME | User that released scheduled batch job | CHAR | 12 | 0 | BTCRELNM | CHAR12 |
| 25 | SDLDATE | Schedule date | DATS | 8 | 0 | BTCSDLDATE | SYDATS |
| 26 | SDLSTRTDT | START DATE | DATS | 8 | 0 | BTCSDATE | SYDATS |
| 27 | SDLSTRTTM | START TIME | TIMS | 6 | 0 | BTCSTIME | SYTIME |
| 28 | SDLTIME | Schedule date | TIMS | 6 | 0 | BTCSDLTIME | SYTIME |
| 29 | SDLUNAME | Job Scheduler | CHAR | 12 | 0 | BTCSDLNM | CHAR12 |
| 30 | STATE_COLOR | STATE COLOR | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 31 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 32 | STATUS | JOB STATUS | CHAR | 1 | 0 | BTCSTATUS | CHAR1 |
| 33 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 34 | STEPCOUNT | Step no. | INT4 | 10 | 0 | BTCSTEPCNT |  |
| 35 | STRTDATE | Start date | DATS | 8 | 0 | BTCXDATE | SYDATS |
| 36 | STRTTIME | Start time | TIMS | 6 | 0 | BTCXTIME | SYTIME |
| 37 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 38 | VARIANT | Variant | CHAR | 14 | 0 | BTCVARIANT | CHAR14 |
| 39 | W_VARIANT | X - Include Program Variant |  | 0 | 0 |  |  |




### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 39 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Back days): Number of days to look back from today when no explicit date range is supplied. The EI uses this to define the start of the monitoring window for background job selection.

**BTCSYSTEM** (Targ. Sys of a Backg. Job): Target system where the background job runs. The EI uses this to filter jobs by the system they execute on.

**DATE_REF_FLD** (Date Ref. Field): Date field used as the reference for the monitoring window (e.g. job start date, schedule date). The EI uses this when filtering jobs by date range.

**DATE_REF_FLD Options:**
- **SDLSTRTDT**: Planned start date
- **STRTDATE**: Actual start date
- **RELDATE**: Release date
- **SDLDATE**: Schedule date
- **LASTCHDATE**: Last change date
Use the field that matches the EI's job structure.

**DURATION** (Duration from Start in d.unit): Duration value in the unit given by DURATION_UNIT. The EI uses this to filter jobs by execution duration.

**DURATION_H** (Execution Time(Hours)): Execution time in hours. Used to filter or display job run duration in hours.

**DURATION_M** (Execution Time(Minutes)): Execution time in minutes. Used to filter or display job run duration in minutes.

**DURATION_UNIT** (Duration Unit(D/H/M)): Unit in which DURATION is interpreted (hours, minutes, or days). The EI uses this when comparing or filtering by duration.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**ENDDATE** (Start date): End date of the job or selection range. The EI uses this to restrict the job selection to the given date range.

**ENDTIME** (Start time): End time. The EI uses this (together with ENDDATE) to restrict the job selection to the given time range.

**JOBCLASS** (Job class): Job classification. The EI uses this to filter jobs by class (e.g. A, B, C).

**JOBCOUNT** (Job no.): Job number (run ID). The EI uses this to filter or identify specific job runs.

**JOBNAME** (JOB NAME): Background job name. The EI uses this to filter which jobs are included in the result set.

**JOBS_CNT** (Jobs Count): Count of job runs per group. Used to filter by minimum or maximum count after aggregation.

**LASTCHDATE** (Last job change): Date of last change to the job. The EI uses this to filter jobs by when they were last modified.

**LASTCHNAME** (Last job change/made by): User who last changed the job. The EI uses this to filter jobs by the person who last modified them.

**LASTCHTIME** (Last job change): Time of last change. The EI uses this (together with LASTCHDATE) to restrict by last change time.

**NO_DATE_RESTRICTION** (No date restriction): When set, the EI does not apply a date restriction; all jobs matching other criteria are considered.

**PERIODIC** (Periodic job): Indicates whether the job is periodic. The EI uses this to filter by periodic vs one-time jobs.
**PERIODIC Options:**
- **X**: Periodic
- (space): One-time

**PRDHOURS** (Periodic job): Duration period in hours for a periodic job. The EI uses this for periodic job filtering or display.

**PRDMINS** (Periodic job): Duration period in minutes for a periodic job. The EI uses this for periodic job filtering or display.

**PROGNAME** (Rept Name): Report or program name. The EI uses this to filter jobs by the program executed.

**RELDATE** (Scheduled release): Scheduled release date. The EI uses this to restrict the job selection to the given release date range.

**RELTIME** (Scheduled release): Scheduled release time. The EI uses this (together with RELDATE) to restrict by release time.

**RELUNAME** (User that released scheduled batch job): User who released the job. The EI uses this to filter jobs by the releaser.

**SDLDATE** (Schedule date): Date of job/step scheduling. The EI uses this to filter by when the job was scheduled.

**SDLSTRTDT** (START DATE): Planned start date. The EI uses this to restrict the job selection to the given planned start date range.

**SDLSTRTTM** (START TIME): Planned start time. The EI uses this (together with SDLSTRTDT) to restrict by planned start time.

**SDLTIME** (Schedule date): Time of job/step scheduling. The EI uses this (together with SDLDATE) to restrict by schedule time.

**SDLUNAME** (Job Scheduler): User who scheduled the job. The EI uses this to filter jobs by the scheduler.

**STATE_COLOR** (STATE COLOR): Status color of the job. Used to filter or display job status color in the result set.

**STATE_ICON** (State Icon): State icon. Used to filter or display job state icon in the result set.

**STATUS** (JOB STATUS): Job status (e.g. released, finished, cancelled). The EI uses this to filter which statuses are included.
**STATUS Options:** Value keys as in domain BTCSTATUS; examples:
- **R**: Released
- **F**: Finished

**STATUS_DESC** (SW Message): Status description text. Populated in the output for display.

**STEPCOUNT** (Step no.): Job step number. The EI uses this to filter or identify specific steps within a job.

**STRTDATE** (Start date): Actual job start date. The EI uses this to restrict the job selection to the given start date range.

**STRTTIME** (Start time): Actual job start time. The EI uses this (together with STRTDATE) to restrict by start time.

**USER_FLD** (Dynamic Recipient User Field): Dynamic user field for recipient or similar. The EI uses this when user-based filtering or routing is applied.

**VARIANT** (Variant): Variant name within a job step. The EI uses this to filter which variants are included.

**W_VARIANT** (X - Include Program Variant): When set, the EI includes program variant in the grouping or filtering.
**W_VARIANT Options:**
- **X**: Include
- (space): Do not include




### Parameter Relationships

**Time and duration parameters:** **BACKDAYS**, **SDLSTRTDT**, **SDLSTRTTM**, **STRTDATE**, **STRTTIME**, **RELDATE**, **RELTIME**, **SDLDATE**, **SDLTIME**, **ENDDATE**, **ENDTIME**, and **LASTCHDATE**/**LASTCHTIME** define the monitoring window and date/time ranges. When no explicit range is supplied, the EI uses BACKDAYS. **DURATION**, **DURATION_UNIT**, **DURATION_H**, and **DURATION_M** work together to filter or display execution duration; the EI uses DURATION_UNIT when comparing DURATION. **DATE_REF_FLD** specifies which date field is used as the reference for the window.

**Job identity and grouping:** **JOBNAME**, **VARIANT**, **JOBCOUNT**, and **STEPCOUNT** identify the job and step. **JOBS_CNT** filters the aggregated count per job/variant. **PROGNAME** and **W_VARIANT** work with **JOBNAME** and **VARIANT** to narrow by program and variant inclusion.

**Periodic and schedule:** **PERIODIC**, **PRDHOURS**, and **PRDMINS** relate to periodic jobs. **SDLUNAME**, **RELUNAME**, and **LASTCHNAME** identify who scheduled, released, or last changed the job; they can be used together for audit-style filtering.

**Status and display:** **STATUS**, **STATE_COLOR**, and **STATE_ICON** (and **STATUS_DESC**) work together to filter and display job state.




### Default Values

- **LANGU** — Default: system language (when used for texts). The called function may use SY-LANGU when LANGU is initial.

**Note:** This EI delegates to another function for job selection; default behavior for date range and duration depends on that function when BACKDAYS or date parameters are not supplied.



### Practical Configuration Examples

**Use Case 1: Last 10 days, filter by job count**
```
BACKDAYS = 10
JOBS_CNT = 1 - 999999
DURATION_UNIT = D
JOBNAME = *
```
**Purpose:** Monitor background jobs that ran in the last 10 days, with duration in days, and filter by job count range. Useful for volume checks.

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
DATE_REF_FLD = SDLSTRTDT
```
**Purpose:** Monitor periodic jobs scheduled by JOBUSER in the last 14 days for programs starting with RS, with duration in hours and start date as reference. Used for periodic job oversight.

**Use Case 4: Multiple filters**
```
BACKDAYS = 7
JOBNAME = Z*
VARIANT = PROD*
JOBS_CNT = 2 - 100
DURATION_UNIT = D
SDLSTRTDT = 20240301 - 20240331
```
**Purpose:** Combined monitoring over the last 7 days for custom jobs (Z*) with variants starting with PROD, job count between 2 and 100, with planned start in March 2024. Suitable for focused job analysis.




### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_JOBS_GRP_DET | BTCSYSTEM | Target System to Run Background Job | CHAR(32) | BTCTGTSYS |
| /SKN/S_SW_01_01_JOBS_GRP_DET | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_JOBS_GRP_DET | DURATION_H | SW: Duration In Hours | NUMC(6) | /SKN/E_SW_DURATION_H |
| /SKN/S_SW_01_01_JOBS_GRP_DET | DURATION_M | SW: Duration In Minutes | NUMC(6) | /SKN/E_SW_DURATION_M |
| /SKN/S_SW_01_01_JOBS_GRP_DET | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_JOBS_GRP_DET | ENDDATE | Job start date | DATS(8) | BTCXDATE |
| /SKN/S_SW_01_01_JOBS_GRP_DET | ENDTIME | Batch job start time | TIMS(6) | BTCXTIME |
| /SKN/S_SW_01_01_JOBS_GRP_DET | JOBCLASS | Job classification | CHAR(1) | BTCJOBCLAS |
| /SKN/S_SW_01_01_JOBS_GRP_DET | JOBCOUNT | Job ID | CHAR(8) | BTCJOBCNT |
| /SKN/S_SW_01_01_JOBS_GRP_DET | JOBNAME | Background job name | CHAR(32) | BTCJOB |
| /SKN/S_SW_01_01_JOBS_GRP_DET | LASTCHDATE | Date of last job change | DATS(8) | BTCJCHDATE |
| /SKN/S_SW_01_01_JOBS_GRP_DET | LASTCHNAME | Last job change made by | CHAR(12) | BTCJCHNM |
| /SKN/S_SW_01_01_JOBS_GRP_DET | LASTCHTIME | Time of last job change | TIMS(6) | BTCJCHTIME |
| /SKN/S_SW_01_01_JOBS_GRP_DET | PERIODIC | Periodic Jobs Indicator | CHAR(1) | BTCPFLAG |
| /SKN/S_SW_01_01_JOBS_GRP_DET | PRDHOURS | Duration period (in hours) for a batch job | NUMC(2) | BTCPHOUR |
| /SKN/S_SW_01_01_JOBS_GRP_DET | PRDMINS | Duration period (in minutes) for a batch job | NUMC(2) | BTCPMIN |
| /SKN/S_SW_01_01_JOBS_GRP_DET | PROGNAME | Program name within a step (e.g. report) | CHAR(40) | BTCPROG |
| /SKN/S_SW_01_01_JOBS_GRP_DET | RELDATE | Release Date for Background Scheduling | DATS(8) | BTCRELDT |
| /SKN/S_SW_01_01_JOBS_GRP_DET | RELTIME | Release time of scheduled background job | TIMS(6) | BTCRELTM |
| /SKN/S_SW_01_01_JOBS_GRP_DET | RELUNAME | User that released scheduled batch job | CHAR(12) | BTCRELNM |
| /SKN/S_SW_01_01_JOBS_GRP_DET | SDLDATE | Date of job/step scheduling | DATS(8) | BTCSDLDATE |
| /SKN/S_SW_01_01_JOBS_GRP_DET | SDLSTRTDT | Planned Start Date for Background Job | DATS(8) | BTCSDATE |
| /SKN/S_SW_01_01_JOBS_GRP_DET | SDLSTRTTM | Planned start time for background Job | TIMS(6) | BTCSTIME |
| /SKN/S_SW_01_01_JOBS_GRP_DET | SDLTIME | Time of a scheduled job/step | TIMS(6) | BTCSDLTIME |
| /SKN/S_SW_01_01_JOBS_GRP_DET | SDLUNAME | Initiator of job/step scheduling | CHAR(12) | BTCSDLNM |
| /SKN/S_SW_01_01_JOBS_GRP_DET | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_JOBS_GRP_DET | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_JOBS_GRP_DET | STATUS | State of Background Job | CHAR(1) | BTCSTATUS |
| /SKN/S_SW_01_01_JOBS_GRP_DET | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_01_JOBS_GRP_DET | STEPCOUNT | Job step ID number. | INT4(10) | BTCSTEPCNT |
| /SKN/S_SW_01_01_JOBS_GRP_DET | STRTDATE | Job start date | DATS(8) | BTCXDATE |
| /SKN/S_SW_01_01_JOBS_GRP_DET | STRTTIME | Batch job start time | TIMS(6) | BTCXTIME |
| /SKN/S_SW_01_01_JOBS_GRP_DET | VARIANT | Name of variant within a step | CHAR(14) | BTCVARIANT |
