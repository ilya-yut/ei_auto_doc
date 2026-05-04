# Parameters: SKN_S_SW_01_01_SM21_GRP_CNT

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AGGR_PERIOD | Aggregation Period (D/H/M) |  | 0 | 0 |  |  |
| 2 | AREA | SysLog msg. group | CHAR | 2 | 0 | RSLGAREA | RSLGAREA |
| 3 | BACKDAYS | Days backward |  | 0 | 0 |  |  |
| 4 | DEVCLASS | Package | CHAR | 30 | 0 | DEVCLASS | DEVCLASS |
| 5 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 6 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 7 | ERRNO | Op. sys. error no. | CHAR | 6 | 0 | SERRNO | SERRNO |
| 8 | INSTANCENAME | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 9 | LANGU | Text laguage |  | 0 | 0 |  |  |
| 10 | REQ_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 11 | SLGDATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 12 | SLGDATTIM | System log time stamp | CHAR | 16 | 0 | RSLGTIME | RSLGTIME |
| 13 | SLGLTRM | Terminal | CHAR | 8 | 0 | RSLGTERM | TEXT8 |
| 14 | SLGMAND | Client | CLNT | 3 | 0 | MANDT | MANDT |
| 15 | SLGMODE | External mode of an SAP dialog | CHAR | 1 | 0 | SAPMODE | CHAR1 |
| 16 | SLGPROC | Process | CHAR | 12 | 0 | RSLGPID_D | RSLGPID |
| 17 | SLGREPNA | Program | CHAR | 40 | 0 | PROGRAM_ID | PROGNAME |
| 18 | SLGTC | T-Code | CHAR | 20 | 0 | TCODE | TCODE |
| 19 | SLGTIME | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 20 | SLGTYPE | Identification | CHAR | 4 | 0 | RSLGETYP_D | RSLGETYP |
| 21 | SLGUSER | User | CHAR | 12 | 0 | RSLGUSER | CHAR12 |
| 22 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 23 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 24 | SUBID | Sub-name | CHAR | 1 | 0 | RSLGSUBID | RSLGSUBID |
| 25 | TABE | Message ID | CHAR | 3 | 0 | RSLGNO | RSLGNO |
| 26 | TEXT | Error Message | CHAR | 255 | 0 | /SKN/E_SW_ERROR | /SKN/D_SW_LTEXT |
| 27 | WITH_MESSAGE | 'X' - Include Error Mess. text |  | 0 | 0 |  |  |
| 28 | WP_TYPE | WP Type | CHAR | 4 | 0 | CHAR4 | CHAR4 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 28 parameters listed in the Parameters Reference Table above.

**AGGR_PERIOD** (Aggregation Period (D/H/M)):

Controls how system log entries are grouped for counting. The EI calls the underlying system log function, then aggregates detail rows by date and time according to this value. The output count (REQ_CNT) is per group.

**AGGR_PERIOD Options:**
- **D**: Day — group by date only; time portion cleared so all entries on the same day aggregate together.
- **H**: Hour — group by date and hour; minutes and seconds cleared (e.g. 10:00:00).
- **M**: Minute — group by date and minute; seconds cleared.
- **Others**: When not D, H, or M, date and time are cleared and aggregation is by other grouping keys only.

**AREA** (SysLog msg. group):

Message group (RSLGAREA) from the system log entry. Used to restrict which log entries are included in the result. Values come from the system log message catalog (e.g. two-character area codes).

**BACKDAYS** (Days backward):

Number of days to look back from the reference date when building the time window for system log selection. Used in the called function when no explicit date range is supplied: the start date is reference date minus BACKDAYS.

**DEVCLASS** (Package):

SAP development package (DEVCLASS). Filters system log entries by the package associated with the program that generated the log entry.

**DURATION** (Duration In Time Units):

Duration value expressing age or span in the unit given by DURATION_UNIT. In the called function, each log entry gets a computed duration (e.g. time from log entry to "now"); this parameter filters entries by that computed duration (e.g. only entries within the last N minutes or hours).

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is interpreted and in which the called function computes duration for each log entry.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**ERRNO** (Op. sys. error no.):

Operating system or application error number from the system log entry. Filters entries by this identifier.

**INSTANCENAME** (Server Name):

Application server instance name. Restricts analysis to log entries from the specified server(s). The called function reads the system log per server and filters by this parameter.

**LANGU** (Text laguage):

Language for text resolution (e.g. error messages). When error or classification texts are read, this language is used.

**REQ_CNT** (Count):

Minimum count threshold per aggregation group. After grouping by AGGR_PERIOD (and other keys), only groups whose aggregated count lies in the REQ_CNT range are returned. Used to focus on groups that meet the configured count criteria.

**REQ_CNT** is used together with AGGR_PERIOD and WITH_MESSAGE: aggregation defines the groups, and REQ_CNT filters which groups appear in the output.

**SLGDATE** (Date):

Log entry date. Filters system log entries by the date on which the entry was written.

**SLGDATTIM** (System log time stamp):

Full timestamp (date + time) of the system log entry. Used to filter entries by this combined value.

**SLGLTRM** (Terminal):

Terminal identifier of the session that generated the log entry. Filters by terminal.

**SLGMAND** (Client):

Client (MANDT). Restricts log entries to the specified client(s).

**SLGMODE** (External mode of an SAP dialog):

Dialog mode (e.g. A=background, B=debugging). Filters entries by this mode.

**SLGMODE Options:**
- **A** — Dialog (interactive).
- **B** — Background (batch).
- **C** — RFC or communication.
- **D** — Update.
- **E** — Enqueue.
- **F** — Spool.
- **S** — System.

**SLGPROC** (Process):

Process identifier (e.g. work process ID) that wrote the log entry. Filters by process.

**SLGREPNA** (Program):

Program name that generated the log entry. Filters by program (PROGRAM_ID).

**SLGTC** (T-Code):

Transaction code. Restricts entries to the specified transaction(s).

**SLGTIME** (Time):

Log entry time. Filters by time of day independent of date.

**SLGTYPE** (Identification):

System log entry type (RSLGETYP_D). Identifies the kind of log record (area/subid or type code). Filters by this classification.

**SLGUSER** (User):

User ID (RSLGUSER) that generated the log entry. Restricts analysis to entries from the specified user(s).

**STATE_COLOR** (State Color):

Severity or state indicator (e.g. from monitoring evaluation) used to filter which log entries are included. The called function derives state color from the log cache (e.g. MONBEW) and filters by this parameter.

**STATE_COLOR Options:**
- **R** — Red indicating critical or high-priority entries requiring immediate attention.
- **Y** — Yellow indicating warning or medium-priority entries.
- **G** — Green indicating informational or low-priority entries.

**STATE_ICON** (State Icon):

Icon code that corresponds to the entry’s state or severity. Typically derived from STATE_COLOR via the state-icon function. Used for display and can be used to filter when the front end or post-processing aligns icon to color.

**STATE_ICON Options:**
- **Icon codes** — Values from the ICON domain that represent the state (e.g. critical, warning, success). Specific codes map to the same semantics as STATE_COLOR (red/yellow/green).

**SUBID** (Sub-name):

Sub-identifier (RSLGSUBID) of the system log message type. Used together with area for fine-grained filtering of log message categories.

**TABE** (Message ID):

Message ID (RSLGNO, three-character). Filters by the specific message identifier in the log.

**TEXT** (Error Message):

Error or log message text. Filters entries by the resolved message text (e.g. long text from the message catalog).

**WITH_MESSAGE** ('X' - Include Error Mess. text):

Controls whether the aggregated output includes the error message text. When set, the text from the called function is kept in the result; when not set, the text field is cleared so only grouping keys and count are shown.

**WITH_MESSAGE Options:**
- **X**: Include error message text in the result rows.
- ** ** (space): Do not include error message text; text field is cleared in the output.

**WP_TYPE** (WP Type):

Work process type (e.g. DIA, BTC, UPD, ENQ, SPO). Filters system log entries by the type of work process that wrote the entry.

**WP_TYPE Options:**
- **DIA** — Dialog work processes.
- **BTC** — Background work processes.
- **UPD** — Update work processes.
- **ENQ** — Enqueue work processes.
- **SPO** — Spool work processes.


### Parameter Relationships

**Aggregation parameters:**

- **AGGR_PERIOD**, **REQ_CNT**, and **WITH_MESSAGE** work together. AGGR_PERIOD defines how detail rows are grouped (by day, hour, or minute). After grouping, REQ_CNT filters which groups are returned based on the aggregated count. WITH_MESSAGE controls whether the error message text is kept in the aggregated output or cleared.

**Time parameters:**

- **BACKDAYS**, **DURATION**, **DURATION_UNIT**, **SLGDATE**, and **SLGTIME** define the time window and age of log entries. In the called function, when no explicit date range is supplied, the start date is derived from the reference date minus BACKDAYS. DURATION and DURATION_UNIT define the unit and value used to filter entries by their computed age (e.g. last N minutes). SLGDATE and SLGTIME filter entries by log date and time directly.

**Filtering parameters:**

- **INSTANCENAME**, **SLGUSER**, **SLGTC**, and **STATE_COLOR** restrict which log entries are included. The called function reads the system log per instance and filters by INSTANCENAME; it then filters in memory by SLGUSER, SLGTC, and STATE_COLOR so that only entries matching these criteria appear before aggregation.


### Default Values

- **DURATION_UNIT** — Default: `M` (Minutes when not supplied).
- **BACKDAYS** — Default: `0` (when not supplied).

**Note:** These defaults are set in the called function (F_SW_01_01_SM21) before single-value parameters are read; the main EI (SM21_GRP_CNT) does not set defaults for these parameters.

### Practical Configuration Examples

**Use Case 1: Hourly system log counts with message text (last 2 days)**

```
AGGR_PERIOD = H
BACKDAYS = 2
DURATION_UNIT = M
WITH_MESSAGE = X
REQ_CNT = 5 - 999999
```

**Purpose:** Aggregate system log entries by hour over the last two days, include error message text in the result, and return only groups with at least 5 entries. Suitable for spotting repeated issues within specific hours.

**Use Case 2: Critical entries on one server, full-day filtering**

```
INSTANCENAME = ASCS01
STATE_COLOR = R
DURATION_UNIT = F
DURATION = 30
SLGDATE = 20240301 - 20240331
```

**Purpose:** Focus on critical (red) log entries from server ASCS01 for a single full-day window (day 30 in the range) and restrict by a date range in March 2024. DURATION_UNIT = F with DURATION = 30 is used for specific-day filtering.

**Use Case 3: Per-minute aggregation for a specific user and transaction**

```
AGGR_PERIOD = M
SLGUSER = DEVELOPER01
SLGTC = SE38
REQ_CNT = 1 - 999999
BACKDAYS = 1
```

**Purpose:** See minute-level counts of system log entries for user DEVELOPER01 and transaction SE38 over the last day. All groups are returned (REQ_CNT 1–999999); useful for activity analysis by minute.

**Use Case 4: Daily summary with count threshold and duration filter**

```
AGGR_PERIOD = D
DURATION_UNIT = H
DURATION = 0 - 24
REQ_CNT = 10 - 999999
WITH_MESSAGE = 
BACKDAYS = 7
```

**Purpose:** Daily aggregation over the last seven days, including only entries whose computed age is between 0 and 24 hours, and returning only groups with at least 10 entries. Message text is not included so the result stays compact for daily summaries.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_SM21_GRP_CNT | ERRNO | Operating system or C library errno | CHAR(6) | SERRNO |
| /SKN/S_SW_01_01_SM21_GRP_CNT | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_01_SM21_GRP_CNT | REQ_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |
| /SKN/S_SW_01_01_SM21_GRP_CNT | SLGDATE | Date | DATS(8) | DATUM |
| /SKN/S_SW_01_01_SM21_GRP_CNT | SLGTIME | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_01_01_SM21_GRP_CNT | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_SM21_GRP_CNT | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_SM21_GRP_CNT | TABE | System log message Identifier | CHAR(3) | RSLGNO |
| /SKN/S_SW_01_01_SM21_GRP_CNT | TEXT | SW: Run Error | CHAR(255) | /SKN/E_SW_ERROR |
| /SKN/S_SW_01_01_SM21_GRP_CNT | WP_TYPE | Not More Closely Defined Area, Possibly Used for Patchlevels | CHAR(4) | CHAR4 |
