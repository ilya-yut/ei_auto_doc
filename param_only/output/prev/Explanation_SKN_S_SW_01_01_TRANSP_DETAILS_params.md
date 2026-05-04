# Parameters: SKN_S_SW_01_01_TRANSP_DETAILS

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACTFLG | tp Active Flag | CHAR | 1 | 0 | TRTPACTFLG |  |
| 2 | ACTIVITY | IMG Activity | CHAR | 20 | 0 | TRACTIVITY | CUS_IMG_AC |
| 3 | AS4DATE | Date | DATS | 8 | 0 | AS4DATE | AS4DATE |
| 4 | AS4POS | Dictionary: Line item | NUMC | 6 | 0 | DDPOSITION | DDPOSITION |
| 5 | AS4TIME | Time | TIMS | 6 | 0 | AS4TIME | AS4TIME |
| 6 | AS4USER | Request/Task Owner | CHAR | 12 | 0 | TR_AS4USER | AS4USER |
| 7 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 8 | BUFLVL | Counter | NUMC | 1 | 0 | COUNTER | COUNTER |
| 9 | BUFPOS | Dictionary: Line item | NUMC | 6 | 0 | DDPOSITION | DDPOSITION |
| 10 | COMSYS | System Name(from) | CHAR | 10 | 0 | TMSSYSNAM | SYSNAME |
| 11 | CVERSFLG | Result of Component Check | CHAR | 3 | 0 | CCRESLT | CCRESULT |
| 12 | DOMNAM | Transport Domain | CHAR | 10 | 0 | TMSDOMNAM | TMSDOMNAM |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 14 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 15 | GENNUM | Information Key | CHAR | 3 | 0 | TRGENNUM | TRGENNUM |
| 16 | IMPFLG | tp Import Flag | CHAR | 1 | 0 | TRTPIMPFLG | TRTPIMPFLG |
| 17 | IMPSING | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 18 | INT_MAXRC | Return code (0/4/8/12) |  | 0 | 0 |  |  |
| 19 | JOBID | TMS CI Background Job Number | NUMC | 10 | 0 | CIBJOBNUM | CIBJOBNUM |
| 20 | LANG | Language Key | LANG | 1 | 0 | SPRAS | SPRAS |
| 21 | LOCKFLAG | Lock/Import Status | CHAR | 1 | 0 | LOCKFLAG | TR_IMPORT_STATUS |
| 22 | MAXRC | Return code | CHAR | 4 | 0 | TRRETCODE | TRRETCODE |
| 23 | NODATAFLG | tp NoData Flag | CHAR | 1 | 0 | TRTPNODFLG |  |
| 24 | OBJECT | Object Type | CHAR | 4 | 0 | TROBJTYPE | OBJECT |
| 25 | OBJFUNC | Function | CHAR | 1 | 0 | OBJFUNC | OBJFUNC |
| 26 | OBJ_NAME | Obj. Name | CHAR | 120 | 0 | TROBJ_NAME | TROBJ_NAME |
| 27 | OWNER | Owner | CHAR | 12 | 0 | TMSOWNER | AS4USER |
| 28 | PGMID | Program ID | CHAR | 4 | 0 | PGMID | PGMID |
| 29 | PREFLG | tp Predecessor Flag | CHAR | 1 | 0 | TRTPPREFLG |  |
| 30 | PROJECT | CTS Project | CHAR | 20 | 0 | TRKORR_P | TRKORR |
| 31 | SRCCLI | Source client | CHAR | 3 | 0 | TRCLIENT | CHAR3 |
| 32 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 33 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 34 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 35 | STRKORR | Higher-Level Request | CHAR | 20 | 0 | STRKORR | TRKORR |
| 36 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 37 | SYSNAM | System Name(For) | CHAR | 10 | 0 | TMSSYSNAM | SYSNAME |
| 38 | TARCLI | Target Client | CHAR | 3 | 0 | TRTARCLI | CHAR3 |
| 39 | TARSYSTEM | Transport Target | CHAR | 10 | 0 | TR_TARGET | TR_TARGET |
| 40 | TEXT | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 41 | TPSTATID | Time Stamp | CHAR | 20 | 0 | TRTPTSTAMP | CHAR20 |
| 42 | TRFUNCTION | Type of request/task | CHAR | 1 | 0 | TRFUNCTION | TRFUNCTION |
| 43 | TRKORR | Request/Task | CHAR | 20 | 0 | TRKORR | TRKORR |
| 44 | TRSTATUS | Status | CHAR | 1 | 0 | TRSTATUS | TRSTATUS |
| 45 | UMODES | Unconditional modes | CHAR | 20 | 0 | TRTPUMODES | CHAR20 |
| 46 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 46 parameters listed in the Parameters Reference Table above.

**ACTFLG** (tp Active Flag):

Identifies whether the transport or task is active in the transport buffer. Used to filter transport details by active-flag status so that only entries matching the configured value appear in the result.

**ACTIVITY** (IMG Activity):

IMG activity that wrote the entry to the object list. Used to restrict results to transports or tasks associated with a specific Customizing activity.

**AS4DATE** (Date):

Date of last change of the request or task. Used to restrict transport details to a given date or date range.

**AS4POS** (Dictionary: Line item):

Dictionary line item (position) within the transport. Used to target specific object positions when needed.

**AS4TIME** (Time):

Time of last change. Used together with date parameters to narrow the monitoring window by time.

**AS4USER** (Request/Task Owner):

Owner of the request or task. Used to filter transport details by the user who owns the transport or task.

**BACKDAYS** (Days Backward from today):

Number of days to look back from today when no date range is supplied. The EI uses this to build the lower bound of the monitoring window (today minus BACKDAYS).

**Work together / Connection:** BACKDAYS is used together with the date logic: when no date range is provided, the EI sets the start of the selection to today minus BACKDAYS. It works with DURATION and DURATION_UNIT for time-based filtering of results.

**BUFLVL** (Counter):

Counter level in the transport buffer. Used to distinguish buffer levels when analyzing queue data.

**BUFPOS** (Dictionary: Line item):

Buffer position (line item) in the transport buffer. Used to target specific buffer entries when needed.

**COMSYS** (System Name(from)):

System name of the source system (from) in the transport route. Used to filter transport details by the system where the transport originated.

**Work together / Connection:** COMSYS works with DOMNAM and SYSNAM to scope transport details by transport domain and system (from/to). Set these together when filtering by landscape and system.

**CVERSFLG** (Result of Component Check):

Result of the component check for the transport. Used to filter by component-verification outcome when relevant.

**DOMNAM** (Transport Domain):

Transport domain in the Transport Management System. Used to restrict results to transports belonging to a specific domain.

**Work together / Connection:** DOMNAM works with SYSNAM and COMSYS to define the transport landscape scope. Set DOMNAM, SYSNAM, and COMSYS together when filtering by domain and systems.

**DURATION** (Duration In Time Units):

Duration in the unit defined by DURATION_UNIT, representing the age or elapsed time of the transport (e.g. hours since last change). The EI calculates duration per record and filters out records that do not fall within the configured range.

**Work together / Connection:** DURATION is used together with DURATION_UNIT and BACKDAYS. DURATION_UNIT defines the unit (hours, minutes, days, or full days); BACKDAYS defines the lookback when no date range is supplied. Filter by DURATION after the EI has computed it for each transport record.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit in which DURATION is expressed and in which the EI computes elapsed time (from last change to application server date/time).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**GENNUM** (Information Key):

Language or generation information key for the object entry in the Transport Organizer. Used when filtering by this attribute.

**IMPFLG** (tp Import Flag):

tp import flag indicating import status. Used to filter by whether the transport has been imported (or similar tp state).

**IMPSING** (Indicator):

General flag (e.g. single import indicator). Used to filter by this indicator when relevant.

**INT_MAXRC** (Return code (0/4/8/12)):

Numeric return code (0, 4, 8, 12) of the transport. Used to filter by numeric return code in addition to the character MAXRC field.

**JOBID** (TMS CI Background Job Number):

TMS Configuration Import background job number. Used to filter transport details related to a specific CI job when applicable.

**LANG** (Language Key):

Language key. Used to restrict results to a specific language when the EI uses language in selection or output.

**LOCKFLAG** (Lock/Import Status):

Lock or import status of an object entry. Used to filter by lock or import state.

**MAXRC** (Return code):

Return code of the transport (e.g. 0000, 0004, 0008, 0012). The EI derives STATE_COLOR from MAXRC (e.g. 0000 → green, 0004 → yellow, 0008/0012 → red). Used to filter which return codes are included in the result.

**NODATAFLG** (tp NoData Flag):

tp no-data flag. Used to filter by this flag when analyzing buffer or tp state.

**OBJECT** (Object Type):

Object type (e.g. PROG, TABL, CLAS). Used to restrict transport details to specific repository object types.

**OBJFUNC** (Function):

Object function (e.g. add, change, delete). Used to filter by the function of the object in the transport.

**OBJ_NAME** (Obj. Name):

Object name in the object list. Used to restrict results to specific repository objects (e.g. a program or table name).

**OWNER** (Owner):

Owner of the transport in TMS. Used to filter by the configured owner.

**PGMID** (Program ID):

Program ID (e.g. R3TR, LIMU) in requests and tasks. Used to filter by the type of development object (program ID).

**PREFLG** (tp Predecessor Flag):

tp predecessor flag. Used to filter by predecessor relationship in the buffer when relevant.

**PROJECT** (CTS Project):

CTS project (project ID) in the Change and Transport System. Used to restrict results to a specific project.

**SRCCLI** (Source client):

Source client of the request. Used to filter transport details by the client from which the transport was created.

**Work together / Connection:** SRCCLI and TARCLI together define the client pair (source and target). Set both when filtering by client scope.

**STATE_COLOR** (State Color):

State color derived from the transport return code (MAXRC): red for critical, yellow for warning, green for success. Used to filter which severity levels appear in the result.

**STATE_COLOR Options:**
- **R** — Red: critical return codes (e.g. 0008, 0012) requiring immediate attention
- **Y** — Yellow: warning return code (e.g. 0004)
- **G** — Green: success return code (0000)

**STATE_ICON** (State Icon):

Icon representing the state (e.g. traffic-light style). The EI sets it from the return code (MAXRC): red/green/yellow for return codes 0008/0012, 0000, 0004; alert or inactive for other cases. Used for display and can be used to filter when the EI exposes it as a selection criterion.

**STATE_ICON Options:**
- **Red icon** — Critical return codes (0008, 0012)
- **Green icon** — Success (0000)
- **Yellow icon** — Warning (0004)
- **Alert icon** — Alert state (0012)
- **Inactive/LED** — No return code or inactive

**STATUS_DESC** (SW Message):

Short message or status description for the state. Used for display and context when filtering by status text.

**STRKORR** (Higher-Level Request):

Higher-level request (parent request) of a task. Used to filter by the main transport to which tasks belong.

**Work together / Connection:** TRKORR and STRKORR work together: TRKORR is the request/task ID; STRKORR is the parent request. Use both when focusing on a specific transport and its tasks or when filtering by main request.

**SW_DEST** (Cloud Destination):

Cloud or RFC destination used when the EI runs in cloud mode. When set, the EI delegates to the configured destination; when initial, it runs locally.

**SYSNAM** (System Name(For)):

System name (for) in the transport route—the target system. Used to filter transport details by the system for which the transport is intended.

**Work together / Connection:** SYSNAM works with DOMNAM and COMSYS to scope by transport domain and systems (from/to). Set these together when filtering by landscape.

**TARCLI** (Target Client):

Target client for the request. Used to filter by the client to which the transport is directed.

**Work together / Connection:** TARCLI and SRCCLI define the client pair. Set both when filtering by source and target client.

**TARSYSTEM** (Transport Target):

Transport target system. Used to filter by the target system of the transport.

**TEXT** (Short Description):

Short description of the request or task. Used to search or filter by description text when the EI supports it.

**TPSTATID** (Time Stamp):

tp time stamp. Used to filter or order by tp timestamp when relevant.

**TRFUNCTION** (Type of request/task):

Type of request or task (e.g. Workbench vs Customizing). Used in the selection to restrict results to development (workbench) or Customizing transports.

**TRFUNCTION Options:**
- **K** — Workbench (development) request/task
- **W** — Customizing request/task

**TRKORR** (Request/Task):

Request or task ID. Used to restrict transport details to specific transport IDs or a range of IDs.

**Work together / Connection:** TRKORR and STRKORR work together: TRKORR identifies the request/task; STRKORR identifies the higher-level request. Use both when focusing on a main request and its tasks.

**TRSTATUS** (Status):

Status of the request or task (e.g. modifiable, released). Used to filter by lifecycle status.

**TRSTATUS Options:**
- **D** — Modifiable (development in progress)
- **R** — Released
- **O** — Other statuses as defined in the domain

**UMODES** (Unconditional modes):

tp unconditional modes. Used to filter by tp mode when relevant for buffer analysis.

**USER_FLD** (Dynamic Recipient User Field):

User-defined or dynamic recipient user field. Used when the EI supports filtering or routing by this field; values are context-specific (e.g. user group or role-related codes).

**USER_FLD Options:**
- **USR01** — User group or field value 1 (context-specific)
- **USR02** — User group or field value 2 (context-specific)
- Values are function- or configuration-specific; see code or output structure for available values.


### Parameter Relationships

**Time window and duration parameters**

- **BACKDAYS** defines how many days to look back from today when no date range is supplied; the EI uses it to set the start of the monitoring window.
- **DURATION_UNIT** defines the unit (hours, minutes, days, or full days) in which elapsed time is calculated.
- **DURATION** is the range of allowed elapsed time (in that unit) per record; the EI computes duration from last change (AS4DATE/AS4TIME) to application server date/time and filters by this range.
- Use BACKDAYS to set the initial date window; use DURATION_UNIT and DURATION to filter results by how “old” or “recent” the transport is in the chosen unit.

**Request and higher-level request**

- **TRKORR** identifies the request or task.
- **STRKORR** identifies the higher-level (parent) request.
- Together they allow filtering by a specific transport ID or by the main request that groups several tasks. The EI uses both in selection and in buffer lookups.

**Transport domain and system scope**

- **DOMNAM** is the transport domain.
- **SYSNAM** is the system name “for” (target system in the route).
- **COMSYS** is the system name “from” (source system in the route).
- Set these together to restrict transport details to a given domain and system pair (from/to) in the transport landscape.

**Status and state color**

- **TRSTATUS** filters by request/task lifecycle status (e.g. modifiable, released).
- **STATE_COLOR** filters by the derived state (red, yellow, green) that the EI sets from the return code (MAXRC).
- TRSTATUS is used in the initial selection; STATE_COLOR is applied after the EI has derived it from MAXRC for each record.

**Source and target client**

- **SRCCLI** is the source client of the request.
- **TARCLI** is the target client.
- Use both together to scope transport details by the client pair (source and target).


### Default Values

- **DURATION_UNIT** — Default: `H` (Hours when not supplied).
- **BACKDAYS** — Default: `1` (when not supplied).

**Note:** When no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window. DURATION is then calculated in the unit given by DURATION_UNIT (default hours).

### Practical Configuration Examples

**Use Case 1: Last 24 hours, critical and warning only**

```
BACKDAYS = 1
DURATION_UNIT = H
DURATION = 0 - 24
STATE_COLOR = R, Y
```

**Purpose:** Focus on transports changed in the last day, in hours, and show only critical (red) and warning (yellow) states for quick follow-up.

**Use Case 2: Released transports in last 7 days, full-day filtering**

```
BACKDAYS = 7
DURATION_UNIT = F
DURATION = 7
TRSTATUS = R
STATE_COLOR = G, Y, R
```

**Purpose:** Monitor released transports over the last week using full-day (F) unit with a single DURATION value, and include all state colors for a full picture.

**Use Case 3: Specific domain, system, and client pair**

```
DOMNAM = DOMAIN01
SYSNAM = PRD_SYS
COMSYS = DEV_SYS
SRCCLI = 100
TARCLI = 200
TRSTATUS = R
```

**Purpose:** Restrict to transports in a given transport domain, from a specific development system to a production system, for client pair 100 → 200, released only.

**Use Case 4: Single request and its tasks, last 3 days**

```
TRKORR = N5K900123
STRKORR = N5K900123
BACKDAYS = 3
DURATION_UNIT = D
DURATION = 0 - 3
TRFUNCTION = K
```

**Purpose:** Analyze a specific request/task and its higher-level request over the last three days, in days, for Workbench (development) transports only.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_TRANSP_DETAILS | ACTFLG | tp Active Flag | CHAR(1) | TRTPACTFLG |
| /SKN/S_SW_01_01_TRANSP_DETAILS | ACTIVITY | Activity that wrote the entry to the object list | CHAR(20) | TRACTIVITY |
| /SKN/S_SW_01_01_TRANSP_DETAILS | AS4DATE | Date of Last Change | DATS(8) | AS4DATE |
| /SKN/S_SW_01_01_TRANSP_DETAILS | AS4POS | Dictionary: Line item | NUMC(6) | DDPOSITION |
| /SKN/S_SW_01_01_TRANSP_DETAILS | AS4TIME | Last changed at | TIMS(6) | AS4TIME |
| /SKN/S_SW_01_01_TRANSP_DETAILS | AS4USER | Owner of a Request or Task | CHAR(12) | TR_AS4USER |
| /SKN/S_SW_01_01_TRANSP_DETAILS | BUFLVL | Counter | NUMC(1) | COUNTER |
| /SKN/S_SW_01_01_TRANSP_DETAILS | BUFPOS | Dictionary: Line item | NUMC(6) | DDPOSITION |
| /SKN/S_SW_01_01_TRANSP_DETAILS | COMSYS | TMS: System Name | CHAR(10) | TMSSYSNAM |
| /SKN/S_SW_01_01_TRANSP_DETAILS | CVERSFLG | Result of Component Check | CHAR(3) | CCRESLT |
| /SKN/S_SW_01_01_TRANSP_DETAILS | DOMNAM | TMS: Transport Domain | CHAR(10) | TMSDOMNAM |
| /SKN/S_SW_01_01_TRANSP_DETAILS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_TRANSP_DETAILS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_TRANSP_DETAILS | GENNUM | Language information for object entry in Transport Organizer | CHAR(3) | TRGENNUM |
| /SKN/S_SW_01_01_TRANSP_DETAILS | IMPFLG | tp Import Flag | CHAR(1) | TRTPIMPFLG |
| /SKN/S_SW_01_01_TRANSP_DETAILS | IMPSING | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_TRANSP_DETAILS | JOBID | TMS CI Background Job Number | NUMC(10) | CIBJOBNUM |
| /SKN/S_SW_01_01_TRANSP_DETAILS | LANG | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_01_01_TRANSP_DETAILS | LOCKFLAG | Lock status or import status of an object entry | CHAR(1) | LOCKFLAG |
| /SKN/S_SW_01_01_TRANSP_DETAILS | MAXRC | Return code | CHAR(4) | TRRETCODE |
| /SKN/S_SW_01_01_TRANSP_DETAILS | NODATAFLG | tp No Data Flag | CHAR(1) | TRTPNODFLG |
| /SKN/S_SW_01_01_TRANSP_DETAILS | OBJECT | Object Type | CHAR(4) | TROBJTYPE |
| /SKN/S_SW_01_01_TRANSP_DETAILS | OBJFUNC | Object function | CHAR(1) | OBJFUNC |
| /SKN/S_SW_01_01_TRANSP_DETAILS | OBJ_NAME | Object Name in Object List | CHAR(120) | TROBJ_NAME |
| /SKN/S_SW_01_01_TRANSP_DETAILS | OWNER | TMS: Owner | CHAR(12) | TMSOWNER |
| /SKN/S_SW_01_01_TRANSP_DETAILS | PGMID | Program ID in Requests and Tasks | CHAR(4) | PGMID |
| /SKN/S_SW_01_01_TRANSP_DETAILS | PREFLG | tp Predecessor Flag | CHAR(1) | TRTPPREFLG |
| /SKN/S_SW_01_01_TRANSP_DETAILS | PROJECT | Project in Change and Transport System | CHAR(20) | TRKORR_P |
| /SKN/S_SW_01_01_TRANSP_DETAILS | SRCCLI | Source client of request | CHAR(3) | TRCLIENT |
| /SKN/S_SW_01_01_TRANSP_DETAILS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_TRANSP_DETAILS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_TRANSP_DETAILS | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_01_TRANSP_DETAILS | STRKORR | Higher-Level Request | CHAR(20) | STRKORR |
| /SKN/S_SW_01_01_TRANSP_DETAILS | SYSNAM | TMS: System Name | CHAR(10) | TMSSYSNAM |
| /SKN/S_SW_01_01_TRANSP_DETAILS | TARCLI | Target client for the request | CHAR(3) | TRTARCLI |
| /SKN/S_SW_01_01_TRANSP_DETAILS | TARSYSTEM | Transport Target of Request | CHAR(10) | TR_TARGET |
| /SKN/S_SW_01_01_TRANSP_DETAILS | TEXT | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_01_TRANSP_DETAILS | TPSTATID | tp Time Stamp | CHAR(20) | TRTPTSTAMP |
| /SKN/S_SW_01_01_TRANSP_DETAILS | TRFUNCTION | Type of request/task | CHAR(1) | TRFUNCTION |
| /SKN/S_SW_01_01_TRANSP_DETAILS | TRKORR | Request/Task | CHAR(20) | TRKORR |
| /SKN/S_SW_01_01_TRANSP_DETAILS | TRSTATUS | Status of request/task | CHAR(1) | TRSTATUS |
| /SKN/S_SW_01_01_TRANSP_DETAILS | UMODES | tp Unconditional Modes | CHAR(20) | TRTPUMODES |