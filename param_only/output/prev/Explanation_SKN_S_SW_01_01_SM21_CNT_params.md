# Parameters: SKN_S_SW_01_01_SM21_CNT

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AREA | SysLog msg. group | CHAR | 2 | 0 | RSLGAREA | RSLGAREA |
| 2 | BACKDAYS | Days backward |  | 0 | 0 |  |  |
| 3 | DEVCLASS | Package | CHAR | 30 | 0 | DEVCLASS | DEVCLASS |
| 4 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 5 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 6 | INSTANCENAME | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 7 | LANGU | Text laguage |  | 0 | 0 |  |  |
| 8 | REQ_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 9 | SLGDATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 10 | SLGDATTIM | System log time stamp | CHAR | 16 | 0 | RSLGTIME | RSLGTIME |
| 11 | SLGLTRM | Terminal | CHAR | 8 | 0 | RSLGTERM | TEXT8 |
| 12 | SLGMAND | Client | CLNT | 3 | 0 | MANDT | MANDT |
| 13 | SLGMODE | External mode of an SAP dialog | CHAR | 1 | 0 | SAPMODE | CHAR1 |
| 14 | SLGPROC | Process | CHAR | 12 | 0 | RSLGPID_D | RSLGPID |
| 15 | SLGREPNA | Program | CHAR | 40 | 0 | PROGRAM_ID | PROGNAME |
| 16 | SLGTC | T-Code | CHAR | 20 | 0 | TCODE | TCODE |
| 17 | SLGTIME | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 18 | SLGTYPE | Identification | CHAR | 4 | 0 | RSLGETYP_D | RSLGETYP |
| 19 | SLGUSER | User | CHAR | 12 | 0 | RSLGUSER | CHAR12 |
| 20 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 21 | SUBID | Sub-name | CHAR | 1 | 0 | RSLGSUBID | RSLGSUBID |
| 22 | TEXT | Error Message | CHAR | 255 | 0 | /SKN/E_SW_ERROR | /SKN/D_SW_LTEXT |
| 23 | WP_TYPE | WP Type | CHAR | 4 | 0 | CHAR4 | CHAR4 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 23 parameters listed in the Parameters Reference Table above.

**AREA** (SysLog msg. group):

System log message group that categorizes the log entry type. Area (with SUBID) is derived from the entry type and used to retrieve the monitoring cache entry for state color and message text.

**BACKDAYS** (Days backward):

Number of days to look back from the current date when no date range is supplied. The called function sets the start of the monitoring window as current date minus BACKDAYS when the date range is initial.

**DEVCLASS** (Package):

Development class (package) of the program that wrote the system log entry.

**DURATION** (Duration In Time Units):

Time difference between the log entry timestamp and the current system date/time, in the unit given by DURATION_UNIT. The called function computes this for each entry.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed and in which the per-entry time difference is computed (hours, minutes, days, or full days).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION_UNIT and DURATION Connection:** DURATION_UNIT defines the unit for both the DURATION selection range and the per-entry duration calculation; DURATION supplies the allowed range in that unit.

**INSTANCENAME** (Server Name):

Application server from which the system log is read. The called function reads the log from this instance.

**LANGU** (Text laguage):

Language in which system log message text is resolved when retrieving cache entries and error text for log entries.

**REQ_CNT** (Count):

Number of system log entries per aggregated key (e.g. per application server). The main function aggregates detail rows by key; the parameter supplies the allowed count range for the key.

**SLGDATE** (Date):

Date of the system log entry. The called function builds the datetime window for reading the system log from the date selection.

**SLGDATTIM** (System log time stamp):

Full timestamp (date and time) of the system log entry. The EI derives entry date and time from this field and uses it for duration calculation and date/time scope.

**SLGLTRM** (Terminal):

Terminal of the session that generated the log entry.

**SLGMAND** (Client):

Client (mandant) of the log entry.

**SLGMODE** (External mode of an SAP dialog):

Dialog mode of the session (dialog, background, update, etc.) that generated the entry. The called function uses it when selecting which log entries to include.

**SLGMODE Options:**
- **A**: Dialog (external mode)
- **B**: Background
- **E**: Update
- Other mode values as defined by the system (SAPMODE domain)

**SLGPROC** (Process):

Process identifier of the log entry. The called function uses it to resolve work process type (WP_TYPE) and message text.

**SLGREPNA** (Program):

Program that generated the system log entry.

**SLGTC** (T-Code):

Transaction code of the session that generated the log entry.

**SLGTIME** (Time):

Time of the system log entry. Together with SLGDATE, defines the entry timestamp; the called function sets the system log read window from the date selection.

**SLGTYPE** (Identification):

Log entry type identifier (4-character structure RSLGETYP). The called function derives area (positions 1–2, RSLGAREA) and sub-id (position 4, RSLGSUBID) from it for cache lookup and message resolution.

**SLGTYPE Options:**
- **01** — System/database message group (area 01)
- **02** — Authorization-related message group (area 02)
- **03** — RFC/communication message group (area 03)
- **04** — General error or application message group (area 04)
- Combined 4-char values (area + subid) as in RSLGETYP; sub-id varies by system log catalog.

**SLGUSER** (User):

User that generated the system log entry.

**STATE_COLOR** (State Color):

State or severity color of the log entry from the monitoring cache (MONBEW). The called function assigns it from the cache entry and uses it when selecting which log entries to include.

**STATE_COLOR Options:**
- **R**: Red indicating critical events requiring immediate attention
- **Y**: Yellow indicating warning-level events
- **G**: Green indicating informational or low-severity events
- **B**: Blue or other state as defined by the monitoring framework

**SUBID** (Sub-name):

Sub-identifier of the system log message type. Used with AREA for cache lookup and message resolution.

**TEXT** (Error Message):

Resolved error or message text of the system log entry.

**WP_TYPE** (WP Type):

Work process type (dialog, background, update, etc.) of the process that generated the log entry. The called function derives it from the process identifier via SM21_SYSLOG_TYPE.

**WP_TYPE Options:**
- **DIA**: Dialog work processes
- **BTC**: Background work processes
- **UPD**: Update work processes
- **ENQ**: Enqueue work processes
- **SPO**: Spool work processes


### Parameter Relationships

**Time and duration parameters:**

- **BACKDAYS** is used when no date range is supplied: the start of the monitoring window is set to current date minus BACKDAYS; the end is the current date. Together with the date range (e.g. SLGDATE or the range passed via the selection table), BACKDAYS defines the lookback window.
- **DURATION** and **DURATION_UNIT** work together: DURATION_UNIT defines the unit (hours, minutes, days, or full days) in which both the DURATION selection range and the per-entry duration are expressed. The called function computes the time difference between each log entry's timestamp and the current system date/time in the chosen unit and keeps only entries whose duration lies within the DURATION range.
- **SLGDATE** and **SLGTIME** (and **SLGDATTIM**) refer to the same log entry timestamp; filtering by date and time is applied after reading the system log and building the filter datetime range from the date selection.

**Count and aggregation:**

- **REQ_CNT** is applied in the main function after the called function returns detailed log data: rows are aggregated by key (COLLECT), so REQ_CNT is the number of log entries per key. The REQ_CNT parameter filters which aggregated keys appear in the result (e.g. only keys with at least N entries).


### Default Values

- **DURATION_UNIT** — Default: `M` (minutes; set in the called function before reading single-value parameters).
- **BACKDAYS** — Default: `0` (no lookback days when not supplied; set in the called function before reading single-value parameters).

**Note:** These defaults are defined in the called function that performs the system log read and filtering. When not supplied by the caller, the monitoring window starts at the current date (BACKDAYS = 0) and duration is expressed in minutes (DURATION_UNIT = 'M').

### Practical Configuration Examples

**Use Case 1: Recent system log entries by count**
```
REQ_CNT = 5 - 999999
BACKDAYS = 7
DURATION_UNIT = M
```
**Purpose:** Focus on application servers that had at least 5 system log entries in the last 7 days, with duration expressed in minutes.

**Use Case 2: High-severity entries on a specific server**
```
INSTANCENAME = ASPROD_DVEBMGS00
STATE_COLOR = R
SLGDATE = 20250101 - 20250131
DURATION = 0 - 60
DURATION_UNIT = D
```
**Purpose:** Identify critical (red) system log entries on a given application server in January 2025, limited to entries within 60 days of the reference time.

**Use Case 3: Full-day filtering for a specific lookback**
```
DURATION = 30
DURATION_UNIT = F
BACKDAYS = 30
SLGUSER = DDIC
REQ_CNT = 1 - 999999
```
**Purpose:** Monitor entries for user DDIC over the last 30 days using full-day duration filtering, and include all keys with at least one entry.

**Use Case 4: Filter by transaction, program, and work process type**
```
SLGTC = SE38
SLGREPNA = SAPMS38E
WP_TYPE = DIA
REQ_CNT = 2 - 100
STATE_COLOR = Y
```
**Purpose:** Find application servers where transaction SE38 (program SAPMS38E) ran in dialog mode with warning-level entries and produced between 2 and 100 log entries per key.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_SM21_CNT | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_01_SM21_CNT | REQ_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |
