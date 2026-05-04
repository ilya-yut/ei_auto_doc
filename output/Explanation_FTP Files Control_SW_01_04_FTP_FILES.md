# Exception Indicator: FTP Files Control - SW_01_04_FTP_FILES

## General Overview

This Exception Indicator monitors **files reached through FTP-style remote access**—using host, credentials, remote directory, and name patterns—while applying **size** and **freshness** rules so operations teams can confirm that expected extracts, logs, or hand-offs exist on the remote side before downstream jobs run.

This EI serves as an essential control for integration and platform operations by:
- Surfacing remote files that violate agreed size bands or have not been refreshed inside the business window
- Making mismatches between connection targets (wrong host or path) visible before batch chains consume stale or empty inputs
- Giving monitoring consoles a severity signal on each result line so triage can prioritize broken feeds
- Aligning clock interpretation for global landscapes where the same job must judge file age in a single reference timezone
- Supporting governance over recurring transfers that must land in named folders on schedule for finance or logistics close

The EI is most valuable when interface agreements name concrete remote paths, masks, and file naming conventions; keeping those aligned with this function’s configuration prevents silent gaps between what partners publish and what internal processes expect.


## Problem Description

Failure to monitor partner- or server-published files over remote file access creates multiple risks across process reliability, security, and executive visibility:

**Financial and Reporting Issues**

- Late or missing remote extracts can delay accrual postings and reconciliations while schedulers still assume the file landed.
- Oversized or zero-byte remote artifacts can break parsing jobs and distort period reporting if no one sees the anomaly before import.
- Inconsistent dating of remote file metadata versus corporate calendars undermines evidence that controls ran on the intended close day.

**Operational / Control Risks**

- A wrong host, directory, or mask can make the check succeed against an empty sandbox path while production feeds are elsewhere.
- Credential or path drift after password rotations or infrastructure moves produces false comfort until a business team notices missing data.
- Mixed local versus GMT evaluation for the same remote path splits incident analysis when regions compare “how old” a file is.

**Management Visibility and Decision-Making Risks**

- Leadership lacks one consolidated view of which remote feeds are healthy, which breach policy, and which severity styling marks as urgent.
- Without a shared duration semantics, teams cannot compare monitoring definitions across regions or between test and production.

## Suggested Resolution

**Immediate Response**

- Inspect flagged lines together with the configured remote path, mask, and connection target to confirm the exception reflects the real production feed.
- Re-validate host identity and path after infrastructure or vendor changes before treating results as regression-free.
- Decide whether GMT-aligned evaluation should stay on for global dashboards or revert to local clock context for a single-region pilot.

**System Assessment**

- Compare current remote file sizes and timestamps to a baseline captured after a known-good transfer cycle.
- Sample files that barely satisfy freshness rules to see whether upstream publishing still meets the contracted cadence.
- Review credential rotation logs and partner change tickets whenever authentication-related noise appears in monitoring.

**Corrective Actions**

- Update host, path, mask, or threshold parameters after approved architecture changes; record the approved combination per environment.
- Escalate chronic misses to the owning application or vendor team and track remediation with a dated action item.
- Refresh operational playbooks that map severity coloring to incident priority for this EI’s audience.
- Schedule a recurring review of remote path catalog entries so decommissioned folders are removed from monitoring definitions.


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | DATE_SEL | Take Date from Monitor |  | 0 | 0 |  |  |
| 2 | DATUM | Date |  | 0 | 0 |  |  |
| 3 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 4 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 5 | FILE_SIZE | File Size (B) | INT4 | 10 | 0 | INT4 | INT4 |
| 6 | FILE_SIZE_KB | File Size (KB) | INT4 | 10 | 0 | INT4 | INT4 |
| 7 | FILE_SIZE_MB | File Size (MB) | INT4 | 10 | 0 | INT4 | INT4 |
| 8 | FTP_DATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 9 | FTP_DIR | FTP Directory |  | 0 | 0 |  |  |
| 10 | FTP_FILE | File name | CHAR | 128 | 0 | LOCALFILE | LOCALFILE |
| 11 | FTP_MASK | File Mask (*.*) |  | 0 | 0 |  |  |
| 12 | FTP_TIME | TIME | TIMS | 6 | 0 | UZEIT | UZEIT |
| 13 | HOST | Host |  | 0 | 0 |  |  |
| 14 | MANAGE_IN_GMT | 'X'- in GMT |  | 0 | 0 |  |  |
| 15 | PASSWORD | Password |  | 0 | 0 |  |  |
| 16 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 17 | STATE_ICON | STATE ICON | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 18 | UPD_DATE | Upd Date |  | 0 | 0 |  |  |
| 19 | UPD_TIME | Upd Time |  | 0 | 0 |  |  |
| 20 | USER | FTP User Name |  | 0 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 20 parameters listed in the Parameters Reference Table above.

**DATE_SEL** (Take Date from Monitor):

Controls whether this run inherits a **monitor-provided calendar anchor** so the same EI definition stays synchronized with a shared monitoring calendar used by related jobs.

**DATE_SEL Options:**
- **X** — Apply the monitor-supplied date context when evaluating this run.
- ** ** (space) — Do not inherit the monitor date; rely on the other date-related inputs supplied for this execution.

**DATUM** (Date):

Defines the calendar-day slice that bounds which remote file timestamps count as “in scope” for the current monitoring story. Pair it with **DATE_SEL** when the business question is anchored to a specific posting or processing day.

**DURATION** (Duration In Time Units):

Numeric span for a relative freshness window, interpreted in the unit from **DURATION_UNIT**. Use it when the SLA is expressed as “files must have changed within the last N units.”

**DURATION_UNIT** (Duration Unit(D/H/M)):

Selects whether **DURATION** counts hours, minutes, days, or full-calendar-day slices for the same window.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:**

**DURATION** is only meaningful together with **DURATION_UNIT**. Configure both so operators read a single plain-language window (for example “45 minutes” or “3 full days”).

**FILE_SIZE** (File Size (B)):

Byte-level lower and upper bounds for remote files that match the path and mask. Useful for catching empty placeholders or runaway logs before they enter parsing.

**FILE_SIZE_KB** (File Size (KB)):

Kilobyte-scale bounds for the same physical files when runbooks express limits in KB.

**FILE_SIZE_MB** (File Size (MB)):

Megabyte-scale bounds for large archives or database dumps where capacity planning uses MB.

**FILE_SIZE, FILE_SIZE_KB, and FILE_SIZE_MB Connection:**

Each parameter expresses size at a different scale for the same remote objects. Choose the scale your policy names; avoid contradictory overlapping bands across two scales unless you intentionally layer a coarse gate with a fine one.

**FTP_DATE** (Date):

Business calendar date associated with the remote file metadata surfaced on the result line—distinct from the monitoring-window anchors used with **DATUM** and duration logic.

**FTP_DIR** (FTP Directory):

Remote folder the scan targets on the host. Together with **FTP_MASK** and optional **FTP_FILE**, it defines which remote names are eligible.

**FTP_FILE** (File name):

A specific remote file name when the scenario watches one control artifact instead of a wildcard population.

**FTP_MASK** (File Mask (*.*)):

Pattern (for example `*.csv`) limiting which names under **FTP_DIR** participate. Tight masks reduce noise from unrelated files in the same folder.

**FTP_DIR, FTP_MASK, and FTP_FILE Connection:**

**FTP_DIR** chooses the folder; **FTP_MASK** filters many names; **FTP_FILE** pins one name. After path or naming convention changes on the partner side, revisit all three so the EI still watches the intended artifact.

**FTP_TIME** (TIME):

Time-of-day facet paired with **FTP_DATE** when the monitoring policy names an intraday cutoff for the remote file stamp.

**FTP_DATE and FTP_TIME Connection:**

Together they express a precise remote timestamp story for the same row the EI returns—use both when the SLA is tighter than one calendar day.

**HOST** (Host):

Network identity of the FTP endpoint. Must match the environment (production versus test) that owns the feed contract.

**MANAGE_IN_GMT** ('X'- in GMT):

When set, evaluates clock-driven comparisons in GMT so distributed teams agree on file age regardless of local server offsets.

**MANAGE_IN_GMT Options:**
- **X** — Use GMT as the reference clock for applicable comparisons in this run.
- ** ** (space) — Use the default local clock context for the same comparisons.

**PASSWORD** (Password):

Secret paired with **USER** for authenticating to **HOST**. Store and rotate according to your credential policy; the EI consumes the value the monitoring layer supplies for this definition.

**STATE_COLOR** (State Color):

Severity band on the output row for UI coloring.

**STATE_COLOR Options:**
- **R** — Red band: high-priority exception for operations triage.
- **Y** — Yellow band: warning requiring review.
- **G** — Green band: within expected bounds for the configured checks.

**STATE_ICON** (STATE ICON):

Icon token paired with the row after severity resolution for dashboards that render both color and glyph.

**STATE_ICON Options:**
- **R** — Icon slot aligned with the red severity band after resolution.
- **Y** — Icon slot aligned with the yellow band.
- **G** — Icon slot aligned with the green band.

**STATE_COLOR and STATE_ICON Connection:**

**STATE_COLOR** carries semantic severity; **STATE_ICON** carries the display token. Keep them coherent so operators never see conflicting visual cues.

**UPD_DATE** (Upd Date):

Bounds the last-change **date** of remote files in scope—primary control for stale-file detection on a daily cadence.

**UPD_TIME** (Upd Time):

Bounds the last-change **time** within **UPD_DATE** when the SLA names an intraday landing deadline.

**UPD_DATE and UPD_TIME Connection:**

**UPD_DATE** selects the day; **UPD_TIME** refines the clock inside that day for the same freshness check.

**USER** (FTP User Name):

Account name presented to **HOST** together with **PASSWORD** to establish the remote session before listing or measuring files.

**HOST, USER, and PASSWORD Connection:**

All three are required to reach the remote server identity and open a session; if any one drifts after a rotation or DNS change, the EI may authenticate against the wrong target or fail silently until paths are corrected.


### Parameter Relationships

- **HOST**, **USER**, and **PASSWORD** jointly establish access to the remote endpoint; a change to any one breaks the session context for **FTP_DIR** / **FTP_MASK** scans.
- **FTP_DIR**, **FTP_MASK**, and **FTP_FILE** together define which remote paths and names are evaluated; they must stay aligned after partner or folder restructuring.
- **DURATION** and **DURATION_UNIT** always define the relative freshness window as a pair.
- **UPD_DATE** and **UPD_TIME** combine for intraday freshness policies on remote file stamps.
- **FTP_DATE** and **FTP_TIME** describe the timestamp attributes carried on the result line for the same remote object story.
- **DATE_SEL** and **DATUM** interact when the run should share a monitor-wide calendar anchor versus a locally supplied day boundary.
- **MANAGE_IN_GMT** changes how clock-driven comparisons relate to **FTP_DATE**, **FTP_TIME**, and update stamps—keep it consistent across environments that share one global dashboard.
- **STATE_COLOR** and **STATE_ICON** must stay aligned so severity semantics match the rendered icon.


### Default Values

- **DURATION_UNIT** — Default: `M` (minutes) when the unit is resolved before read and the caller has not yet supplied another value for this run.

### Practical Configuration Examples

**Use Case 1: Hourly freshness on a partner outbound folder**

```
HOST = ftp.partner.example.com
USER = mon_svc
PASSWORD = ***
FTP_DIR = /outbound/invoices
FTP_MASK = *.xml
DURATION = 2
DURATION_UNIT = H
```

**Purpose:** Confirm invoice XML files on the partner server were touched within the last two hours before the local payment run starts.

**Use Case 2: Single control file with GMT and size gate**

```
HOST = ftp.internal.corp
FTP_DIR = /controls
FTP_FILE = READY.flag
MANAGE_IN_GMT = X
FILE_SIZE = 10 - 999999
STATE_COLOR = Y
```

**Purpose:** Verify a named readiness flag exists with non-trivial size while evaluating timestamps in GMT for a global control tower.

**Use Case 3: Full-day duration unit with mask**

```
FTP_DIR = /data/dumps
FTP_MASK = DUMP_*.zip
DURATION = 1
DURATION_UNIT = F
FILE_SIZE_MB = 50 - 8000
DATE_SEL = X
```

**Purpose:** Catch oversized or missing daily dump archives while inheriting the monitor date context and using full-day duration semantics.

**Use Case 4: Intraday update window**

```
FTP_DIR = /batch/current
FTP_MASK = PAY_*
UPD_DATE = current business date
UPD_TIME = 060000 - 220000
STATE_ICON = G
FILE_SIZE_KB = 500 - 500000
```

**Purpose:** Ensure payroll-related remote files received a same-day update inside business hours with a kilobyte floor to skip empty stubs.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_04_FTP_FILES | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_04_FTP_FILES | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_04_FTP_FILES | FILE_SIZE | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_04_FTP_FILES | FILE_SIZE_KB | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_04_FTP_FILES | FILE_SIZE_MB | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_04_FTP_FILES | FTP_DATE | Date | DATS(8) | DATUM |
| /SKN/S_SW_01_04_FTP_FILES | FTP_FILE | Local file for upload/download | CHAR(128) | LOCALFILE |
| /SKN/S_SW_01_04_FTP_FILES | FTP_TIME | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_01_04_FTP_FILES | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_04_FTP_FILES | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_04_FTP_FILES .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_04_FTP_FILES OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_FTP_DIR FOR /SKN/S_SW_01_04_FTP_FILES-FTP_FILE ,
         R_FTP_FILE FOR /SKN/S_SW_01_04_FTP_FILES-FTP_FILE ,
         R_FTP_DATE FOR /SKN/S_SW_01_04_FTP_FILES-FTP_DATE ,
         R_FTP_TIME FOR /SKN/S_SW_01_04_FTP_FILES-FTP_TIME ,
         R_FILE_SIZE  FOR /SKN/S_SW_01_04_FTP_FILES-FILE_SIZE,
         R_FILE_SIZE_KB FOR /SKN/S_SW_01_04_FTP_FILES-FILE_SIZE_KB,
         R_FILE_SIZE_MB FOR /SKN/S_SW_01_04_FTP_FILES-FILE_SIZE_MB,
         R_DATUM   FOR SY-DATUM .
RANGES : R_HOST FOR /SKN/S_SW_01_04_FTP_FILES-FTP_FILE ,
         R_USER FOR /SKN/S_SW_01_04_FTP_FILES-FTP_FILE ,
         R_PASSWORD FOR /SKN/S_SW_01_04_FTP_FILES-FTP_FILE ,
         R_FTP_MASK FOR /SKN/S_SW_01_04_FTP_FILES-FTP_FILE .
DATA :   MANAGE_IN_UTC TYPE  CHAR1 .
DATA : HOST TYPE LOCALFILE ,
       USER TYPE LOCALFILE ,
       PASSWORD TYPE LOCALFILE ,
       FTP_DIR TYPE LOCALFILE ,
       FTP_MASK TYPE LOCALFILE .
DATA : BEGIN OF T_FTP_MASK OCCURS 0,
         FTP_MASK TYPE LOCALFILE ,
       END OF T_FTP_MASK .
DATA : BEGIN OF FILE_LIST OCCURS 0.
 INCLUDE STRUCTURE /SKN/S_SW_FTP_FILE_LIST .
DATA : END OF FILE_LIST .
DATA : DATE_FROM LIKE SY-DATUM ,
       BACKDAYS  TYPE I .
DATA : SY_TABIX LIKE SY-TABIX .
DATA : IS_OUT(1) TYPE C.
DATA : TIME_DIFF TYPE  INT4 .
DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
*-- Fill Selection Option Tables
  SELECT_MULTY: DURATION.
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
   LOOP AT T_SELECT WHERE FIELDNM = 'FTP_DIR'.
     MOVE-CORRESPONDING T_SELECT TO R_FTP_DIR.
     APPEND R_FTP_DIR.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FTP_FILE'.
     MOVE-CORRESPONDING T_SELECT TO R_FTP_FILE.
     APPEND R_FTP_FILE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FTP_DATE'.
     MOVE-CORRESPONDING T_SELECT TO R_FTP_DATE.
     APPEND R_FTP_DATE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FTP_TIME'.
     MOVE-CORRESPONDING T_SELECT TO R_FTP_TIME.
     APPEND R_FTP_TIME.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'HOST'.
     MOVE-CORRESPONDING T_SELECT TO R_HOST.
     APPEND R_HOST.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'USER'.
     MOVE-CORRESPONDING T_SELECT TO R_USER.
     APPEND R_USER.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'PASSWORD'.
     MOVE-CORRESPONDING T_SELECT TO R_PASSWORD.
     APPEND R_PASSWORD.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FTP_MASK'.
     MOVE-CORRESPONDING T_SELECT TO R_FTP_MASK.
     APPEND R_FTP_MASK.
   ENDLOOP.
*
   LOOP AT T_SELECT WHERE FIELDNM = 'FILE_SIZE'.
     MOVE-CORRESPONDING T_SELECT TO R_FILE_SIZE.
     APPEND R_FILE_SIZE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FILE_SIZE_KB'.
     MOVE-CORRESPONDING T_SELECT TO R_FILE_SIZE_KB.
     APPEND R_FILE_SIZE_KB.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FILE_SIZE_MB'.
     MOVE-CORRESPONDING T_SELECT TO R_FILE_SIZE_MB.
     APPEND R_FILE_SIZE_MB.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'MANAGE_IN_GMT'.
     MANAGE_IN_UTC = T_SELECT-LOW.
     EXIT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
     MOVE-CORRESPONDING T_SELECT TO R_DATUM.
     APPEND R_DATUM.
   ENDLOOP.
   IF R_DATUM[] IS INITIAL .
     LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
       R_DATUM-SIGN = 'I' .
        R_DATUM-OPTION = 'GE' .
         BACKDAYS = T_SELECT-LOW .
         DATE_FROM = SY-DATUM - BACKDAYS .
         R_DATUM-LOW = DATE_FROM .
         APPEND R_DATUM.
         EXIT.
     ENDLOOP.
     IF R_DATUM[] IS INITIAL .
       R_DATUM-SIGN = 'I' .
        R_DATUM-OPTION = 'GE' .
         BACKDAYS = 1 .
         DATE_FROM = SY-DATUM - BACKDAYS .
         R_DATUM-LOW = DATE_FROM .
       APPEND R_DATUM.
     ENDIF .
   ENDIF.
*  if R_FTP_DATE[] is initial.
*    R_FTP_DATE[] = R_DATUM[] .
*  endif.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
 READ TABLE R_HOST INDEX 1.
 IF SY-SUBRC IS INITIAL.
   HOST = R_HOST-LOW.
 ENDIF.
 READ TABLE R_USER INDEX 1.
 IF SY-SUBRC IS INITIAL.
   USER = R_USER-LOW.
 ENDIF.
 READ TABLE R_PASSWORD INDEX 1.
 IF SY-SUBRC IS INITIAL.
   PASSWORD = R_PASSWORD-LOW.
 ENDIF.
* read table R_FTP_DIR index 1.
* if sy-subrc is initial.
*   FTP_DIR = R_FTP_DIR-LOW.
* endif.
  CLEAR FTP_DIR.
  LOOP AT R_FTP_DIR .
    CONCATENATE FTP_DIR R_FTP_DIR-LOW INTO FTP_DIR.
  ENDLOOP.
*--- Prepare FTM_MASK collection
* read table R_FTP_MASK index 1.
* if sy-subrc is initial.
*   FTP_MASK = R_FTP_MASK-LOW.
* endif.
* if FTP_MASK is initial.
*   FTP_MASK = '*.*'.
* endif.
 REFRESH T_FTP_MASK.
 LOOP AT R_FTP_MASK .
   MOVE R_FTP_MASK-LOW TO T_FTP_MASK-FTP_MASK .
   APPEND T_FTP_MASK .
 ENDLOOP.
 IF T_FTP_MASK[] IS INITIAL.
   T_FTP_MASK-FTP_MASK = '*.*'.
   APPEND T_FTP_MASK .
 ENDIF.
*--- Prepare FTM_MASK collection
 LOOP AT T_FTP_MASK .
   CLEAR T_DATA.
   FTP_MASK = T_FTP_MASK-FTP_MASK.
   REFRESH FILE_LIST.
  CALL FUNCTION '/SKN/F_SW_01_04_FTP_FILE_LIST'
    EXPORTING
      HOST                          = HOST
      USER                          = USER
      PASSWORD                      = PASSWORD
*     RFC_DESTINATION               = 'SAPFTPA'
      FTP_DIR                       = FTP_DIR
      FTP_MASK                      = FTP_MASK
    TABLES
      FILE_LIST                     = FILE_LIST
    EXCEPTIONS
      NO_FTP_COMMANDS_TO_EXECUTE    = 1
      FTP_CONNECTION_FAILED         = 2
      FTP_COMMAND_FAILURE           = 3
      OTHERS                        = 4        .
  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN 1.
        T_DATA-FTP_FILE = 'NO_FTP_COMMANDS_TO_EXECUTE' .
      WHEN 2.
        T_DATA-FTP_FILE = 'FTP_CONNECTION_FAILED' .
      WHEN 3.
        T_DATA-FTP_FILE = 'FTP_COMMAND_FAILURE' .
      WHEN OTHERS.
        T_DATA-FTP_FILE = 'FTP Problen' .
    ENDCASE.
      T_DATA-STATE_COLOR = 'R'.
      APPEND T_DATA.
  ENDIF.
  LOOP AT FILE_LIST .
    MOVE-CORRESPONDING FILE_LIST TO T_DATA .
    T_DATA-FILE_SIZE = FILE_LIST-FTP_SIZE.
    T_DATA-FILE_SIZE_KB = T_DATA-FILE_SIZE / 1024.
    T_DATA-FILE_SIZE_MB = T_DATA-FILE_SIZE_KB / 1024.
    APPEND T_DATA.
  ENDLOOP.
ENDLOOP.
*-- Convert to Local Time
IF MANAGE_IN_UTC IS INITIAL.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_UTC_TO_LOCAL_DT'
      EXPORTING
        UTC_DATE         = T_DATA-FTP_DATE
        UTC_TIME         = T_DATA-FTP_TIME
      IMPORTING
        LOCAL_DATE       = T_DATA-FTP_DATE
        LOCAL_TIME       = T_DATA-FTP_TIME.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
ENDIF.
*  loop at t_data .
*    sy_tabix = sy-tabix .
*    if not t_data-FTP_DATE is initial.
*      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*        EXPORTING
*          D_FROM            = t_data-FTP_DATE
*          T_FROM            = t_data-FTP_TIME
*          D_TO              = sy-datum
*          T_TO              = sy-uzeit
*          TIME_UNIT         = 'M'
*        IMPORTING
*          TIME_DIFF         = TIME_DIFF
*        EXCEPTIONS
*          WRONG_VALUE       = 1
*          OTHERS            = 2    .
*      IF SY-SUBRC = 0.
*        t_data-DURATION_M = TIME_DIFF .
*        t_data-DURATION_H = t_data-DURATION_M / 60.
*        t_data-STATE_COLOR = 'G'.
*        modify t_data index sy_tabix.
*      ENDIF.
*    endif.
*  endloop.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-FTP_DATE
          T_FROM            = T_DATA-FTP_TIME
          D_TO              = SY-DATUM
          T_TO              = SY-UZEIT
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
    IF NOT IS_OUT IS INITIAL.
      DELETE T_DATA INDEX SY_TABIX .
    ELSE.
      IF NOT ( R_DURATION[] IS INITIAL ).
        T_DATA-STATE_COLOR = 'Y'.
        MODIFY T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDIF.
   ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CLEAR IS_OUT.
    IF NOT T_DATA-FTP_DATE IN R_FTP_DATE .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT T_DATA-FTP_TIME IN R_FTP_TIME .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT T_DATA-FILE_SIZE IN R_FILE_SIZE .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT T_DATA-FILE_SIZE_KB IN R_FILE_SIZE_KB .
      IS_OUT = 'X'.
    ENDIF.
    IF NOT T_DATA-FILE_SIZE_MB IN R_FILE_SIZE_MB .
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
         STATE_COLOR       = T_DATA-STATE_COLOR
       IMPORTING
         STATE_ICON        = T_DATA-STATE_ICON         .
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
