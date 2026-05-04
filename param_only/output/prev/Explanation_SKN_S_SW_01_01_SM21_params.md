# Parameters: SKN_S_SW_01_01_SM21

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AREA | SysLog msg. group | CHAR | 2 | 0 | RSLGAREA | RSLGAREA |
| 2 | BACKDAYS | Days backward |  | 0 | 0 |  |  |
| 3 | CLASID | Problem class | CHAR | 1 | 0 | RSLGCLASID | RSLGCLASID |
| 4 | COUNTER | System log entries read | NUMC | 10 | 0 | RSLGENTREA | RSLGENTCNT |
| 5 | DEVCLASS | Package | CHAR | 30 | 0 | DEVCLASS | DEVCLASS |
| 6 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 7 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 8 | ERRNO | Op. sys. error no. | CHAR | 6 | 0 | SERRNO | SERRNO |
| 9 | ERRORNAME | errno name | CHAR | 16 | 0 | ERRORNAME | TEXT16 |
| 10 | INSTANCENAME | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 11 | LANGU | Text laguage |  | 0 | 0 |  |  |
| 12 | MESSAGE | Message text | CHAR | 220 | 0 | BAPI_MSG | TEXT220 |
| 13 | MONBEW | Syslog monitoring category | CHAR | 2 | 0 | RSLGNKKD | RSLGNKKD |
| 14 | MONKAT | Syslog monitoring category | CHAR | 2 | 0 | RSLGNKKD | RSLGNKKD |
| 15 | PROCESSID | Process No. | CHAR | 3 | 0 | RSLGSELNUM |  |
| 16 | SLGCONNECTION | Connection ID | CHAR | 32 | 0 | RSLG_CONNECTION | RSLG_CONNECTION_32 |
| 17 | SLGCONNECTIONCOUNTER | Call Counter | NUMC | 10 | 0 | RSLG_CONNECTION_COUNTER | RSLG_ROOT_COUNT |
| 18 | SLGDATA | Variable Message Data | CHAR | 64 | 0 | RSLG_DATA | RSLG_DATA |
| 19 | SLGDATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 20 | SLGDATTIM | System log time stamp | CHAR | 16 | 0 | RSLGTIME | RSLGTIME |
| 21 | SLGDUMMY2 | Dummy | CHAR | 22 | 0 | RSLG_DUMMY_2 | RSLG_DUMMY2 |
| 22 | SLGLTRM | Terminal | CHAR | 8 | 0 | RSLGTERM | TEXT8 |
| 23 | SLGMAND | Client | CLNT | 3 | 0 | MANDT | MANDT |
| 24 | SLGMODE | External mode of an SAP dialog | CHAR | 1 | 0 | SAPMODE | CHAR1 |
| 25 | SLGPASSPORT | Transaction ID | CHAR | 32 | 0 | RSLG_GUID32 | RSLG_GUID_32 |
| 26 | SLGPROC | Process | CHAR | 12 | 0 | RSLGPID_D | RSLGPID |
| 27 | SLGREPNA | Program | CHAR | 40 | 0 | PROGRAM_ID | PROGNAME |
| 28 | SLGROOTCONTEXT | Overall Context ID | CHAR | 32 | 0 | RSLG_ROOT_CONTEXT_ID | RSLG_ROOT_CONTEXT |
| 29 | SLGTC | T-Code | CHAR | 20 | 0 | TCODE | TCODE |
| 30 | SLGTERMIDNEW | Terminal | CHAR | 12 | 0 | RSLG_TERM_ID_NEW | RSLG_TERM_ID_NEW_32 |
| 31 | SLGTIME | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 32 | SLGTYPE | Identification | CHAR | 4 | 0 | RSLGETYP_D | RSLGETYP |
| 33 | SLGUSER | User | CHAR | 12 | 0 | RSLGUSER | CHAR12 |
| 34 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 35 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 36 | SUBID | Sub-name | CHAR | 1 | 0 | RSLGSUBID | RSLGSUBID |
| 37 | TABE | Message ID | CHAR | 3 | 0 | RSLGNO | RSLGNO |
| 38 | TEXT | Error Message | CHAR | 255 | 0 | /SKN/E_SW_ERROR | /SKN/D_SW_LTEXT |
| 39 | TXT | Message text | CHAR | 78 | 0 | RSLGMSGTXT | TEXT78 |
| 40 | USES_32_DO | Selection | CHAR | 1 | 0 | RSLGS_ | RSLGS_ |
| 41 | USTYP | User Type | CHAR | 1 | 0 | XUUSTYP | XUUSTYP |
| 42 | USTYP_DESC | User Type Desc. | CHAR | 20 | 0 | /SKN/E_SW_USTYP_DESC |  |
| 43 | WP_TYPE | WP Type | CHAR | 4 | 0 | CHAR4 | CHAR4 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 43 parameters listed in the Parameters Reference Table above.

**AREA** (SysLog msg. group):

Part of the system log message type identification (with SUBID). The EI uses AREA to determine which message group the log entry belongs to when reading and categorizing entries.

**BACKDAYS** (Days backward):

Number of days to look back from the reference date when no explicit date range is supplied. The monitoring window start is set to reference date minus this value.

**CLASID** (Problem class):

Classification ID for system log messages, used to target specific entry types and severity levels.

**CLASID Options:**
- **DB** — Database-related errors and performance issues.
- **AUTH** — Authorization and security-related messages.
- **RFC** — Remote Function Call communication issues.
- **ERROR** — General system error classifications.

**COUNTER** (System log entries read):

Number of system log entries read; the EI can use it when reporting or capping how many entries are considered.

**DEVCLASS** (Package):

SAP development package (devclass) that produced the log entry; the EI uses it when scoping by originating package.

**DURATION** (Duration In Time Units):

Duration value in the unit given by DURATION_UNIT, representing the time span between the log entry timestamp and the reference date/time. The EI computes this for each entry and compares it to the supplied range.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed and in which the EI computes the time difference from log entry to reference date/time.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** DURATION is interpreted in the unit specified by DURATION_UNIT. The EI calculates the time difference per entry in that unit and compares it to the DURATION range.

**ERRNO** (Op. sys. error no.):

Operating system or C library error number associated with the log entry.

**ERRORNAME** (errno name):

C constant name for the errno value in the log entry.

**INSTANCENAME** (Server Name):

Application server name that wrote the system log entry; the EI uses it when scoping by host or instance.

**LANGU** (Text laguage):

Language used for text resolution (e.g. message or error text).

**MESSAGE** (Message text):

Full message text of the system log entry.

**MONBEW** (Syslog monitoring category):

Priority or evaluation code for monitoring categorization of the log entry. Works together with MONKAT for evaluation and categorization.

**MONBEW Options:**
- **1** — High priority monitoring evaluation.
- **2** — Medium priority monitoring evaluation.
- **3** — Low priority monitoring evaluation.

**MONKAT** (Syslog monitoring category):

Category code for monitoring classification of the log entry. Works together with MONBEW for evaluation and categorization.

**MONKAT Options:**
- **PERF** — Performance-related monitoring category.
- **SEC** — Security-related monitoring category.
- **COMM** — Communication-related monitoring category.
- **SYS** — System-related monitoring category.

**MONBEW and MONKAT Connection:** They work together to provide monitoring evaluation and categorization of system log entries. Use MONBEW for priority and MONKAT for category when targeting specific monitoring types.

**PROCESSID** (Process No.):

Process number of the work process that wrote the log entry; derived from the system log process information.

**SLGCONNECTION** (Connection ID):

EPP connection ID associated with the log entry.

**SLGCONNECTIONCOUNTER** (Call Counter):

EPP call counter for the connection.

**SLGDATA** (Variable Message Data):

Variable message data of the system log entry.

**SLGDATE** (Date):

Date when the log entry was written; the EI uses it when building the selection by log date.

**SLGDATTIM** (System log time stamp):

Combined date-time stamp of the log entry.

**SLGDUMMY2** (Dummy):

Reserved/dummy field in the system log structure.

**SLGLTRM** (Terminal):

Terminal name associated with the dialog or session that produced the log entry.

**SLGMAND** (Client):

Client (mandant) in which the log entry was written; the EI uses it when scoping by client.

**SLGMODE** (External mode of an SAP dialog):

Dialog mode (e.g. dialog, batch, update) of the session that produced the entry.

**SLGMODE Options:**
- **X** — Set/active for the mode.
- ** ** (space) — Not set/inactive.

**SLGPASSPORT** (Transaction ID):

Transaction (passport) ID for tracing the log entry.

**SLGPROC** (Process):

Process identifier (e.g. work process) that wrote the log entry.

**SLGREPNA** (Program):

Program name that produced the log entry; the EI uses it when scoping by program.

**SLGROOTCONTEXT** (Overall Context ID):

EPP overall context ID for the log entry.

**SLGTC** (T-Code):

Transaction code executed when the log entry was written; the EI uses it when scoping by transaction.

**SLGTERMIDNEW** (Terminal):

Extended terminal ID for the session.

**SLGTIME** (Time):

Time when the log entry was written; the EI uses it together with SLGDATE when scoping by log timestamp.

**SLGTYPE** (Identification):

System log message type identifier (area + subid); identifies the message class.

**SLGUSER** (User):

SAP user name of the session that produced the log entry; the EI uses it when scoping by user.

**STATE_COLOR** (State Color):

Single-character state or severity color for the log entry. The EI resolves it from the system log cache (e.g. first character of the monitoring evaluation field) and compares it to the supplied values.

**STATE_COLOR Options:**
- **R** — Red indicating critical events requiring immediate attention.
- **Y** — Yellow indicating warnings or attention needed.
- **G** — Green indicating normal or successful state.
- **B** — Blue or informational state.

**STATE_ICON** (State Icon):

Icon code corresponding to the state/severity; the EI maps STATE_COLOR to this icon for display.

**STATE_ICON Options:** Values are function-specific; the EI maps STATE_COLOR to an icon via the function module. Use the same semantic levels as STATE_COLOR (critical, warning, normal, info).

**SUBID** (Sub-name):

Third character of the system log message type name; part of the message identification (with AREA).

**TABE** (Message ID):

System log message identifier (short form).

**TEXT** (Error Message):

Error or run message text resolved for the log entry; the EI uses it when scoping or displaying by message content.

**TXT** (Message text):

Short message text with placeholders; part of the system log message template.

**USES_32_DO** (Selection):

Selection switch for system log processing (unspecific).

**USES_32_DO Options:**
- **X** — Selection active.
- ** ** (space) — Selection inactive.

**USTYP** (User Type):

Type of user or process that generated the log entry (dialog, background, system, communication).

**USTYP Options:**
- **D** — Dialog users (interactive sessions and transaction processing).
- **B** — Background users (batch job execution and automated processes).
- **S** — System users (internal system processes and communication users).
- **C** — Communication users (RFC and interface processing).

**USTYP_DESC** (User Type Desc.):

Short description of the user type (e.g. Dialog, Background); the EI uses it when scoping or displaying by user type description.

**USTYP_DESC Sample values:** Dialog, Background, System, Communication (aligned with USTYP keys).

**WP_TYPE** (WP Type):

Work process type (dialog, batch, update, enqueue, spool) that wrote the log entry. The EI derives it from the system log process information and uses it when scoping by work process type.

**WP_TYPE Options:**
- **DIA** — Dialog work processes (user transaction processing and interactive sessions).
- **BTC** — Background work processes (batch job execution and automated processing).
- **UPD** — Update work processes (database update processing and transaction completion).
- **ENQ** — Enqueue work processes (lock management and resource synchronization).
- **SPO** — Spool work processes (print and output processing).


### Parameter Relationships

**Time and Lookback Parameters:**

- **BACKDAYS** defines how many days to look back from the reference date when no explicit date range is supplied. **SLGDATE** and **SLGTIME** filter the result set by the log entry date and time. Use BACKDAYS to set the default monitoring window; use SLGDATE and SLGTIME to narrow by when the entry was written.

**Duration Parameters:**

- **DURATION** and **DURATION_UNIT** work together: the EI computes the time difference between each log entry's timestamp and the reference date/time in the unit given by DURATION_UNIT and filters by the DURATION range. Set DURATION_UNIT (e.g. M for minutes, D for days) first, then set the DURATION range (e.g. 0–60 for "last 60 minutes").

**Filtering Parameters:**

- **INSTANCENAME**, **SLGUSER**, and **SLGTC** are passed to the system log retrieval as server, user, and transaction code filters. Use them together to scope by application server, user, or transaction (e.g. SM21).

**Monitoring Evaluation and Category Parameters:**

- **MONBEW** and **MONKAT** work together for monitoring evaluation and categorization: MONBEW carries the priority (e.g. 1= high, 2= medium, 3= low) and MONKAT the category (e.g. PERF, SEC, COMM, SYS). Use both when targeting specific monitoring types or severity levels.

**State and Severity:**

- **STATE_COLOR** is derived from the system log cache (e.g. from MONBEW) and is used to filter by severity/state. **STATE_ICON** is the display icon for that state; filtering by STATE_COLOR effectively restricts by the same semantic level as STATE_ICON.


### Default Values

- **DURATION_UNIT** — Default: `M` (Minutes when not supplied).
- **BACKDAYS** — Default: `0` (when not supplied).

### Practical Configuration Examples

**Use Case 1: Last 60 minutes of system log (single server)**

```
DURATION_UNIT = M
DURATION = 0 - 60
INSTANCENAME = hostname_01
```

**Purpose:** Focus on the most recent hour of system log entries from one application server. Useful for real-time or near-real-time monitoring.

**Use Case 2: Critical state entries in a 7-day window**

```
BACKDAYS = 7
STATE_COLOR = R
SLGTC = SM21
```

**Purpose:** Find critical (red) system log entries from the last seven days for transaction SM21, for security or stability review.

**Use Case 3: Full-day filtering for a specific day (DURATION_UNIT = F)**

```
DURATION_UNIT = F
DURATION = 30
SLGDATE = 20250101 - 20250131
STATE_COLOR = R
WP_TYPE = DIA
```

**Purpose:** Analyze critical dialog work process entries for a specific 30-day period using full-day filtering. DURATION_UNIT = F with single-value DURATION (30) supports full-day filtering.

**Use Case 4: User and program focus with severity and time**

```
BACKDAYS = 1
SLGUSER = DEVELOPER
SLGREPNA = SAPM*
MONBEW = 1
MONKAT = SEC
TEXT = *
```

**Purpose:** Review high-priority security-related log entries from the last day for a specific user and programs whose names start with SAPM, for development or audit checks.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_SM21 | AREA | System Log: Group of 36 System Log Messages | CHAR(2) | RSLGAREA |
| /SKN/S_SW_01_01_SM21 | CLASID | System Log: Classification ID for Messages | CHAR(1) | RSLGCLASID |
| /SKN/S_SW_01_01_SM21 | COUNTER | System log: No. system log messages read | NUMC(10) | RSLGENTREA |
| /SKN/S_SW_01_01_SM21 | DEVCLASS | Package | CHAR(30) | DEVCLASS |
| /SKN/S_SW_01_01_SM21 | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_SM21 | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_SM21 | ERRNO | Operating system or C library errno | CHAR(6) | SERRNO |
| /SKN/S_SW_01_01_SM21 | ERRORNAME | C constant name for values in 'errno' | CHAR(16) | ERRORNAME |
| /SKN/S_SW_01_01_SM21 | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_01_SM21 | MESSAGE | Message Text | CHAR(220) | BAPI_MSG |
| /SKN/S_SW_01_01_SM21 | MONBEW | Key for syslog monitoring category | CHAR(2) | RSLGNKKD |
| /SKN/S_SW_01_01_SM21 | MONKAT | Key for syslog monitoring category | CHAR(2) | RSLGNKKD |
| /SKN/S_SW_01_01_SM21 | PROCESSID | Process Number | CHAR(3) | RSLGSELNUM |
| /SKN/S_SW_01_01_SM21 | SLGCONNECTION | EPP Connection ID | CHAR(32) | RSLG_CONNECTION |
| /SKN/S_SW_01_01_SM21 | SLGCONNECTIONCOUNTER | EPP Call Counter | NUMC(10) | RSLG_CONNECTION_COUNTER |
| /SKN/S_SW_01_01_SM21 | SLGDATA | SysLog: variable message data | CHAR(64) | RSLG_DATA |
| /SKN/S_SW_01_01_SM21 | SLGDATE | Date | DATS(8) | DATUM |
| /SKN/S_SW_01_01_SM21 | SLGDATTIM | System log time stamp | CHAR(16) | RSLGTIME |
| /SKN/S_SW_01_01_SM21 | SLGDUMMY2 | Dummy 22 Characters | CHAR(22) | RSLG_DUMMY_2 |
| /SKN/S_SW_01_01_SM21 | SLGLTRM | Terminal name | CHAR(8) | RSLGTERM |
| /SKN/S_SW_01_01_SM21 | SLGMAND | Client | CLNT(3) | MANDT |
| /SKN/S_SW_01_01_SM21 | SLGMODE | External mode of an SAP dialog | CHAR(1) | SAPMODE |
| /SKN/S_SW_01_01_SM21 | SLGPASSPORT | Transaction ID | CHAR(32) | RSLG_GUID32 |
| /SKN/S_SW_01_01_SM21 | SLGPROC | SysLog: LIKE RSLGPID structure | CHAR(12) | RSLGPID_D |
| /SKN/S_SW_01_01_SM21 | SLGREPNA | Program Name | CHAR(40) | PROGRAM_ID |
| /SKN/S_SW_01_01_SM21 | SLGROOTCONTEXT | EPP Overall Context ID | CHAR(32) | RSLG_ROOT_CONTEXT_ID |
| /SKN/S_SW_01_01_SM21 | SLGTC | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_01_SM21 | SLGTERMIDNEW | Terminal ID Extension, 12 Characters | CHAR(12) | RSLG_TERM_ID_NEW |
| /SKN/S_SW_01_01_SM21 | SLGTIME | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_01_01_SM21 | SLGTYPE | SysLog: LIKE structure RSLGETYP | CHAR(4) | RSLGETYP_D |
| /SKN/S_SW_01_01_SM21 | SLGUSER | System log: SAP user name | CHAR(12) | RSLGUSER |
| /SKN/S_SW_01_01_SM21 | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_SM21 | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_SM21 | SUBID | System log: Third character of message name | CHAR(1) | RSLGSUBID |
| /SKN/S_SW_01_01_SM21 | TABE | System log message Identifier | CHAR(3) | RSLGNO |
| /SKN/S_SW_01_01_SM21 | TEXT | SW: Run Error | CHAR(255) | /SKN/E_SW_ERROR |
| /SKN/S_SW_01_01_SM21 | TXT | Text part of system log message with placeholders (& or $) | CHAR(78) | RSLGMSGTXT |
| /SKN/S_SW_01_01_SM21 | USES_32_DO | SysLog: Selection switch (unspecific) | CHAR(1) | RSLGS_ |
| /SKN/S_SW_01_01_SM21 | USTYP | User Type | CHAR(1) | XUUSTYP |
| /SKN/S_SW_01_01_SM21 | USTYP_DESC | SW: User Type Description | CHAR(20) | /SKN/E_SW_USTYP_DESC |
| /SKN/S_SW_01_01_SM21 | WP_TYPE | Not More Closely Defined Area, Possibly Used for Patchlevels | CHAR(4) | CHAR4 |
