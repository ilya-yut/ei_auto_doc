# Exception Indicator: System Files Control - SW_01_04_FILES

## General Overview

This Exception Indicator monitors **application-server file system objects** under configurable directory and mask rules, combining **size**, **last-update date and time**, and **duration-based freshness** so operations and security teams can spot missing, stale, or abnormally large files without manual directory walks.

This EI serves as an essential control for platform and application operations by:
- Enabling detection of files that breach expected size bands or have not been refreshed within the configured lookback window
- Supporting identification of directory or naming patterns that no longer match production standards after releases or batch changes
- Providing visibility into how severity styling on the result lines highlights exceptions for dashboard consumers
- Enabling comparison of file timestamps against the monitoring run when evaluation uses a consistent UTC-based clock context
- Supporting accountability when file feeds, logs, or interfaces must land in specific folders by a defined schedule

The EI is valuable during release verification, batch close, and security reviews where silent file drift causes downstream process failure. It relies on server-local path semantics; monitoring teams align monitored paths and name patterns with the technical layout of each controlled directory.


## Problem Description

Failure to monitor critical server files and their freshness creates multiple risks across operational stability, security investigations, and management reporting:

**Financial and Reporting Issues**

- Batch extracts or interface files that arrive late or never appear can distort period-close reconciliations when downstream booking still assumes the file exists.
- Oversized log or data files can fill disks and delay financial posting jobs without an early warning tied to size thresholds.
- Inconsistent dating of file metadata versus business calendars makes it hard to prove that controls ran on the intended monitoring day.

**Operational / Control Risks**

- Wrong directory or mask configuration silently scans an empty or non-production path, producing a false “all clear” while the real folder is unmanaged.
- Mixed local-time versus UTC evaluation for the same path can shift which files appear “current,” breaking cross-system comparisons during incidents.
- Stale files that should have been replaced continue to satisfy superficial checks if update-date filters are never applied with the right window.

**Management Visibility and Decision-Making Risks**

- Leadership lacks a single, repeatable picture of which paths are healthy, which breaches size or age policy, and how severity styling surfaces priority items.
- Without aligned duration and unit semantics, teams cannot compare monitoring definitions between regions or between development and production landscapes.

## Suggested Resolution

**Immediate Response**

- Review any flagged file lines together with the configured directory, mask, and update window to confirm whether the exception reflects a real gap or a narrow test path.
- Validate that the monitored path still matches the application’s current deployment layout after transports or infrastructure moves.
- Confirm whether evaluation should use standard application-server local time or the UTC-aligned mode when global teams interpret the same results.

**System Assessment**

- Compare current file counts and sizes to a known baseline taken after a successful batch or release cycle.
- Walk through a sample of files that barely pass or fail the update-date filter to see whether business processes still write to those names on schedule.
- Revisit duration-based windows alongside business calendars (e.g. month-end freeze) to ensure the monitoring window matches operational reality.

**Corrective Actions**

- Correct directory, mask, or threshold parameters where infrastructure or naming conventions changed; document approved combinations per environment.
- Escalate repeated misses on mandatory interface files to application owners and schedule a remediation task for the feeding job or share.
- Tighten or relax size and date bands after root-cause review, and record the rationale for audit.
- Add or update operational runbooks that explain how severity coloring maps to incident priority for this EI’s consumers.


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | DATE_SEL | Take Date from Monitor (X') |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 3 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 4 | FILE_DATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 5 | FILE_DIR | Net Directory | CHAR | 128 | 0 | LOCALFILE | LOCALFILE |
| 6 | FILE_MASK | File Mask (*.*) |  | 0 | 0 |  |  |
| 7 | FILE_NAME | File name | CHAR | 128 | 0 | LOCALFILE | LOCALFILE |
| 8 | FILE_SIZE | File Size (Bait) | INT4 | 10 | 0 | INT4 | INT4 |
| 9 | FILE_SIZE_KB | File Size (KB) | INT4 | 10 | 0 | INT4 | INT4 |
| 10 | FILE_SIZE_MB | File Size (MB) | INT4 | 10 | 0 | INT4 | INT4 |
| 11 | FILE_TIME | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 12 | MANAGE_IN_UTC | 'X'- in UTC |  | 0 | 0 |  |  |
| 13 | STATE_COLOR | Set State Color (R/Y/G) | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 14 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 15 | UPD_DATE | Upd Date |  | 0 | 0 |  |  |
| 16 | UPD_TIME | Upd Time |  | 0 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 16 parameters listed in the Parameters Reference Table above.

**DATE_SEL** (Take Date from Monitor (X')):

Chooses whether the monitoring run should anchor its date logic on values carried from the central monitor context instead of only the values supplied for this execution. Use it when the same EI definition must line up with a shared monitoring date that other jobs already established.

**DATE_SEL Options:**
- **X** — Use the monitor-supplied date context when building the evaluation window for this run.
- ** ** (space) — Do not take the date from the monitor; rely on the date window implied by the other inputs for this call.

**DURATION** (Duration In Time Units):

Length of the relative lookback or freshness window expressed as a whole number in the unit given by **DURATION_UNIT**. It works with that unit to bound how far back file update timestamps are considered “in scope” for the current scenario.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Selects whether **DURATION** counts hours, minutes, days, or full-calendar-day slices. The unit must match how operations describe their SLA (for example hourly batch cycles versus nightly file drops).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:**

**DURATION** supplies the numeric span; **DURATION_UNIT** defines what each increment means. Set both together so reviewers can read the configuration as plain language (for example “90 minutes” or “2 full days”) without mixing incompatible units.

**FILE_DATE** (Date):

Filters or interprets the business calendar date associated with the file metadata that this EI surfaces on the result line. Use it when the monitoring question is tied to a particular posting or processing day rather than only “last modified in the last N minutes.”

**FILE_DIR** (Net Directory):

Root directory path the scan evaluates on the application server. It scopes which subtree is eligible; combined with **FILE_MASK**, it defines the exact population of files considered for size and freshness checks.

**FILE_MASK** (File Mask (*.*)):

Pattern for file names inside **FILE_DIR** (for example all log files of a given extension). Tight masks reduce noise; broader masks are for discovery-style sweeps across many file types.

**FILE_DIR and FILE_MASK Connection:**

**FILE_DIR** selects the folder; **FILE_MASK** selects which names inside that folder participate. A path change without an updated mask can accidentally include or exclude entire groups of feeds—keep both aligned whenever application packaging moves interface files.

**FILE_NAME** (File name):

Identifies a specific file name when the scenario targets one known artifact (for example a single control file) rather than a wildcard population under **FILE_MASK**.

**FILE_SIZE** (File Size (Bait)):

Lower and upper bounds for the raw byte length of matching files. Use it to catch empty placeholders, runaway logs, or unexpectedly small extracts.

**FILE_SIZE_KB** (File Size (KB)):

Same intent as **FILE_SIZE** but expressed in kilobytes for thresholds operators reason about in KB (for example archive growth in hundreds of KB).

**FILE_SIZE_MB** (File Size (MB)):

Bounds in megabytes for large dumps or database extracts where MB is the natural unit for capacity planning.

**FILE_SIZE, FILE_SIZE_KB, and FILE_SIZE_MB Connection:**

Each expresses size in a different scale for the same physical files. Configure the band that your runbook uses; avoid overlapping contradictory ranges across two scales in one scenario unless you intentionally layer a coarse MB gate with a fine-byte check.

**FILE_TIME** (Time):

Time-of-day component paired with date-oriented parameters when the monitoring story depends on intraday cutoffs (for example files that must land before a nightly batch time).

**MANAGE_IN_UTC** ('X'- in UTC):

When set, shifts evaluation of clock fields to a UTC-consistent basis so distributed teams see the same “age” of a file regardless of local application-server offset.

**MANAGE_IN_UTC Options:**
- **X** — Evaluate relevant clock context in UTC for this run.
- ** ** (space) — Use the default application-server local clock context for the same fields.

**STATE_COLOR** (Set State Color (R/Y/G)):

Severity band written onto the result row for front-end coloring. It drives how prominently the line appears in monitoring consoles.

**STATE_COLOR Options:**
- **R** — Red band: treat the line as a high-priority exception for operations triage.
- **Y** — Yellow band: warning that needs review but may not stop processing.
- **G** — Green band: within expected bounds for the configured checks.

**STATE_ICON** (State Icon):

Icon token shown with the row after the icon helper resolves the visual for the active **STATE_COLOR** band. Configure it when dashboards require an icon column distinct from color alone.

**STATE_ICON Options:**
- **R** — Icon slot populated for the red severity band after resolution.
- **Y** — Icon slot for the yellow severity band.
- **G** — Icon slot for the green severity band.

**STATE_COLOR and STATE_ICON Connection:**

**STATE_COLOR** sets the semantic severity; **STATE_ICON** carries the display token that maps to that severity in your system’s icon catalog. Keep them consistent so operators do not see a green icon on a red severity band.

**UPD_DATE** (Upd Date):

Filter on the last-change date of files in scope. Primary lever for “stale file” detection when feeds must refresh every business day.

**UPD_TIME** (Upd Time):

Filter on the last-change time, used together with **UPD_DATE** when the SLA is tighter than one day (for example hourly landing files).

**UPD_DATE and UPD_TIME Connection:**

**UPD_DATE** bounds the calendar day; **UPD_TIME** refines within that day. Use both when the monitoring policy names an intraday cutoff (for example “before 18:00”).


### Parameter Relationships

- **FILE_DIR** and **FILE_MASK** together define which server files enter the scan; changing only one without the other often produces empty or overly broad results.
- **DURATION** and **DURATION_UNIT** must be read as a pair: the number is meaningless without the unit that scales the monitoring window.
- **UPD_DATE** and **UPD_TIME** jointly express freshness when business owners care about both calendar day and time-of-day cutoffs.
- **STATE_COLOR** and **STATE_ICON** align semantic severity with the visual the dashboard shows; mismatched pairs confuse triage.
- **MANAGE_IN_UTC** changes how clock-derived comparisons behave relative to **FILE_DATE**, **FILE_TIME**, and update stamps—keep it consistent across environments that share one global dashboard.
- **DATE_SEL** shifts whether the run inherits a monitor-provided date anchor; when set, other date-related inputs should be interpreted in that shared context.


### Default Values

- **DURATION_UNIT** — Default: `M` (minutes) when the unit is resolved before read and the caller has not supplied another value yet for this run.

### Practical Configuration Examples

**Use Case 1: Nightly log directory sweep**

```
FILE_DIR = /usr/sap/logs/interfaces
FILE_MASK = *.log
DURATION = 36
DURATION_UNIT = H
UPD_DATE = today - 1
```

**Purpose:** Highlight interface logs under the nightly path that were not touched within the last day and a half, using an hour-based window for operations close.

**Use Case 2: Single control file size gate**

```
FILE_DIR = /interface/outbound
FILE_NAME = CONTROL.ok
FILE_SIZE = 1 - 999999
STATE_COLOR = Y
MANAGE_IN_UTC = X
```

**Purpose:** Ensure a specific control marker file exists with non-trivial size while evaluating timestamps in UTC for a global control tower.

**Use Case 3: Large extract MB band with full-day unit**

```
FILE_DIR = /data/extracts
FILE_MASK = *.csv
FILE_SIZE_MB = 10 - 5000
DURATION = 1
DURATION_UNIT = F
DATE_SEL = X
```

**Purpose:** Catch extracts that grew beyond expected megabyte bounds while using full-day semantics for the freshness window and inheriting the monitor date context.

**Use Case 4: Time-of-day freshness with mask**

```
FILE_DIR = /batch/archive
FILE_MASK = PAYROLL_*
UPD_DATE = current period start
UPD_TIME = 000000 - 235959
STATE_ICON = G
FILE_SIZE_KB = 100 - 999999
```

**Purpose:** Verify payroll-related archives received a same-day update across the full clock range, with a kilobyte floor to ignore empty stubs.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_04_FILES | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_04_FILES | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_04_FILES | FILE_DATE | Date | DATS(8) | DATUM |
| /SKN/S_SW_01_04_FILES | FILE_DIR | Local file for upload/download | CHAR(128) | LOCALFILE |
| /SKN/S_SW_01_04_FILES | FILE_NAME | Local file for upload/download | CHAR(128) | LOCALFILE |
| /SKN/S_SW_01_04_FILES | FILE_SIZE | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_04_FILES | FILE_SIZE_KB | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_04_FILES | FILE_SIZE_MB | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_04_FILES | FILE_TIME | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_01_04_FILES | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_04_FILES | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_04_FILES .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_04_FILES OPTIONAL
*"----------------------------------------------------------------------
DATA: MANAGE_IN_UTC TYPE  CHAR1 .
DATA : R_FILE_DIR TYPE RANGE OF LOCALFILE,
       R_FILE_MASK TYPE RANGE OF LOCALFILE,
       R_FILE_SIZE  TYPE RANGE OF INT4,
       R_FILE_SIZE_KB TYPE RANGE OF INT4,
       R_FILE_SIZE_MB TYPE RANGE OF INT4,
       R_UPD_DATE     TYPE RANGE OF SYDATUM,
       R_UPD_TIME     TYPE RANGE OF SYUZEIT,
       R_STATE_COLOR  TYPE RANGE OF /SKN/E_SW_STATE_COLOR.
DATA : RS_FILE_DIR LIKE LINE OF  R_FILE_DIR,
       RS_FILE_MASK LIKE LINE OF R_FILE_MASK,
       RS_FILE_SIZE  LIKE LINE OF R_FILE_SIZE,
       RS_FILE_SIZE_KB LIKE LINE OF R_FILE_SIZE_KB,
       RS_FILE_SIZE_MB LIKE LINE OF R_FILE_SIZE_MB,
       RS_UPD_DATE     LIKE LINE OF R_UPD_DATE,
       RS_UPD_TIME     LIKE LINE OF R_UPD_TIME,
       RS_STATE_COLOR  LIKE LINE OF R_STATE_COLOR.
DATA: R_DATUM TYPE RANGE OF SYDATUM.
DATA: RS_DATUM LIKE LINE OF R_DATUM.
DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
DATA : DATE_FROM LIKE SY-DATUM ,
       BACKDAYS  TYPE I .
DATA : W_DATA LIKE LINE OF T_DATA .
DATA : TIME_DIFF TYPE I .
DATA : SY_DATLO LIKE SY-DATLO,
       SY_TIMLO LIKE SY-TIMLO.
  DATA : FILE_DIR TYPE LOCALFILE ,
         FILE_MASK TYPE LOCALFILE .
  DATA : BEGIN OF T_FILE_MASK OCCURS 0,
           FILE_MASK TYPE LOCALFILE ,
         END OF T_FILE_MASK .
  DATA : BEGIN OF FILE_LIST OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_FILE_LIST .
  DATA : END OF FILE_LIST .
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : IS_OUT(1) TYPE C.
  DATA : LV_SEL_DAT(1) TYPE C.
  DATA : STATE_COLOR TYPE /SKN/E_SW_STATE_COLOR.
*-- Fill Selection Option Tables
 SELECT_MULTY: DURATION.
 LV_DURATION_UNIT = 'M'.
 SELECT_SINGLE: DURATION_UNIT.
 SY_DATLO = SY-DATUM .        " Appl Server's Date
 SY_TIMLO = SY-UZEIT.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_04_FILES'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
    LOOP AT T_SELECT WHERE FIELDNM = 'MANAGE_IN_UTC'.
     MANAGE_IN_UTC = T_SELECT-LOW.
     EXIT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
     MOVE-CORRESPONDING T_SELECT TO RS_DATUM.
     APPEND RS_DATUM TO R_DATUM.
   ENDLOOP.
   SET_SY_TIME MANAGE_IN_UTC SY_DATLO SY_TIMLO .
   TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
   LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
     RS_DATUM-SIGN = 'I'.
       RS_DATUM-OPTION = 'GE' .
       BACKDAYS = T_SELECT-LOW .
       DATE_FROM = SY_DATLO - BACKDAYS .
       RS_DATUM-LOW = DATE_FROM .
     APPEND RS_DATUM TO R_DATUM.
     EXIT.
   ENDLOOP.
   IF R_DATUM[] IS INITIAL .
     RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
       BACKDAYS = 1 .
       DATE_FROM = SY_DATLO - BACKDAYS .
       RS_DATUM-LOW = DATE_FROM .
     APPEND RS_DATUM TO R_DATUM.
   ENDIF .
  LOOP AT T_SELECT WHERE FIELDNM = 'UPD_DATE'.
    MOVE-CORRESPONDING T_SELECT TO RS_UPD_DATE.
    APPEND RS_UPD_DATE TO R_UPD_DATE.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'FILE_DIR'.
    MOVE-CORRESPONDING T_SELECT TO RS_FILE_DIR.
    APPEND RS_FILE_DIR TO R_FILE_DIR.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'FILE_MASK'.
    MOVE-CORRESPONDING T_SELECT TO RS_FILE_MASK.
    APPEND RS_FILE_MASK TO R_FILE_MASK.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'UPD_TIME'.
    MOVE-CORRESPONDING T_SELECT TO RS_UPD_TIME.
    APPEND RS_UPD_TIME TO R_UPD_TIME.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'DATE_SEL'.
    LV_SEL_DAT = T_SELECT-LOW.
    EXIT.
  ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FILE_SIZE'.
     MOVE-CORRESPONDING T_SELECT TO RS_FILE_SIZE.
     APPEND RS_FILE_SIZE TO R_FILE_SIZE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FILE_SIZE_KB'.
     MOVE-CORRESPONDING T_SELECT TO RS_FILE_SIZE_KB.
     APPEND RS_FILE_SIZE_KB TO R_FILE_SIZE_KB.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FILE_SIZE_MB'.
     MOVE-CORRESPONDING T_SELECT TO RS_FILE_SIZE_MB.
     APPEND RS_FILE_SIZE_MB TO R_FILE_SIZE_MB.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
     MOVE-CORRESPONDING T_SELECT TO RS_STATE_COLOR.
     APPEND RS_STATE_COLOR TO R_STATE_COLOR.
     STATE_COLOR = RS_STATE_COLOR-LOW.
   ENDLOOP.
*-- Fill Selection Option Tables
  IF R_DATUM[] IS INITIAL .
    LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      BACKDAYS = T_SELECT-LOW .
      DATE_FROM = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
      EXIT.
    ENDLOOP.
    IF R_DATUM[] IS INITIAL .
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      BACKDAYS = 1 .
      DATE_FROM = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
    ENDIF .
  ENDIF.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
*  READ TABLE r_file_dir into rs_file_dir INDEX 1.
*  IF sy-subrc IS INITIAL.
*    file_dir = rs_file_dir-low.
*  ENDIF.
  CLEAR FILE_DIR.
  LOOP AT R_FILE_DIR INTO RS_FILE_DIR.
    CONCATENATE FILE_DIR RS_FILE_DIR-LOW INTO FILE_DIR.
  ENDLOOP.
  REFRESH T_FILE_MASK.
  LOOP AT R_FILE_MASK INTO RS_FILE_MASK .
    MOVE RS_FILE_MASK-LOW TO T_FILE_MASK-FILE_MASK .
    APPEND T_FILE_MASK .
  ENDLOOP.
  IF T_FILE_MASK[] IS INITIAL.
    T_FILE_MASK-FILE_MASK = '*.*'.
    APPEND T_FILE_MASK .
  ENDIF.
**--- Prepare FTM_MASK collection
*
*
  LOOP AT T_FILE_MASK .
    CLEAR T_DATA.
    FILE_MASK = T_FILE_MASK-FILE_MASK.
    REFRESH FILE_LIST.
     CALL FUNCTION '/SKN/F_SW_01_04_FILE_LIST'
       EXPORTING
         LOCAL_DIR            = FILE_DIR
         LOCAL_MASK           = FILE_MASK
         FRONTEND             = ' '
         UTC                  = MANAGE_IN_UTC
       TABLES
         FILE_LIST            = FILE_LIST
       EXCEPTIONS
         NO_FILES_FOUND       = 1
         READ_ERROR           = 2
         OTHERS               = 3.
     IF SY-SUBRC > 1.
       T_DATA-FILE_NAME = 'File read Problen' .
       T_DATA-STATE_COLOR = 'R'.
       APPEND T_DATA.
     ENDIF.
    IF NOT ( LV_SEL_DAT IS INITIAL ).
      DELETE FILE_LIST WHERE NOT ( WRITEDATE IN R_DATUM ).
    ENDIF.
*
    LOOP AT FILE_LIST .
      MOVE-CORRESPONDING FILE_LIST TO T_DATA .
      T_DATA-FILE_DIR     = FILE_DIR.
      T_DATA-FILE_NAME    = FILE_LIST-FILENAME.
      T_DATA-FILE_DATE    = FILE_LIST-WRITEDATE.
      T_DATA-FILE_TIME    = FILE_LIST-WRITETIME.
      T_DATA-FILE_SIZE    = FILE_LIST-FILELENGTH.
      T_DATA-FILE_SIZE_KB = T_DATA-FILE_SIZE / 1024.
      T_DATA-FILE_SIZE_MB = T_DATA-FILE_SIZE_KB / 1024.
      T_DATA-STATE_COLOR  = STATE_COLOR.
      APPEND T_DATA.
    ENDLOOP.
  ENDLOOP.
*
*    LOOP AT t_data .
*    sy_tabix = sy-tabix .
*    IF NOT t_data-file_date IS INITIAL.
*      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*        EXPORTING
*          d_from      = t_data-file_date
*          t_from      = t_data-file_time
*          d_to        = sy-datum
*          t_to        = sy-uzeit
*          time_unit   = 'M'
*        IMPORTING
*          time_diff   = time_diff
*        EXCEPTIONS
*          wrong_value = 1
*          OTHERS      = 2.
*      IF sy-subrc = 0.
*        t_data-duration_m = time_diff .
*        t_data-duration_h = t_data-duration_m / 60.
*        "t_data-state_color = 'G'.
*        MODIFY t_data INDEX sy_tabix.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-FILE_DATE
          T_FROM            = T_DATA-FILE_TIME
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
   MODIFY T_DATA INDEX SY_TABIX .
 ENDLOOP .
 DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CLEAR IS_OUT.
*    IF NOT t_data-duration_h IN r_duration_h .
*      is_out = 'X'.
*    ENDIF.
*    IF NOT t_data-duration_m IN r_duration_m .
*      is_out = 'X'.
*    ENDIF.
    IF NOT T_DATA-FILE_SIZE_KB IN R_FILE_SIZE_KB .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT T_DATA-FILE_SIZE_MB IN R_FILE_SIZE_MB .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT T_DATA-FILE_SIZE IN R_FILE_SIZE .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT IS_OUT IS INITIAL.
      DELETE T_DATA INDEX SY_TABIX .
    ELSE.
      IF NOT ( R_DURATION[] IS INITIAL ).
        "t_data-state_color = 'Y'.
        MODIFY T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CLEAR IS_OUT.
    IF NOT T_DATA-FILE_DATE IN R_UPD_DATE .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT T_DATA-FILE_TIME IN R_UPD_TIME .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT IS_OUT IS INITIAL.
      DELETE T_DATA INDEX SY_TABIX .
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
      EXPORTING
        STATE_COLOR = T_DATA-STATE_COLOR
      IMPORTING
        STATE_ICON  = T_DATA-STATE_ICON.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
  SORT T_DATA.
  DELETE ADJACENT DUPLICATES FROM T_DATA .
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
