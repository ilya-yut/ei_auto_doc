# Parameters: SKN_S_SW_01_01_TRANSP_STAT

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACTFLG | tp Active Flag | CHAR | 1 | 0 | TRTPACTFLG |  |
| 2 | AS4DATE | Date | DATS | 8 | 0 | AS4DATE | AS4DATE |
| 3 | AS4TIME | Time | TIMS | 6 | 0 | AS4TIME | AS4TIME |
| 4 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 5 | BUFLVL | Counter | NUMC | 1 | 0 | COUNTER | COUNTER |
| 6 | BUFPOS | Dictionary: Line item | NUMC | 6 | 0 | DDPOSITION | DDPOSITION |
| 7 | COMSYS | System Name | CHAR | 10 | 0 | TMSSYSNAM | SYSNAME |
| 8 | CVERSFLG | Result of Component Check | CHAR | 3 | 0 | CCRESLT | CCRESULT |
| 9 | DOMNAM | Transport Domain | CHAR | 10 | 0 | TMSDOMNAM | TMSDOMNAM |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | IMPFLG | tp Import Flag | CHAR | 1 | 0 | TRTPIMPFLG | TRTPIMPFLG |
| 13 | IMPSING | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 14 | INT_MAXRC | Return code (0/4/8/12) |  | 0 | 0 |  |  |
| 15 | JOBID | TMS CI Background Job Number | NUMC | 10 | 0 | CIBJOBNUM | CIBJOBNUM |
| 16 | MAXRC | Return code | CHAR | 4 | 0 | TRRETCODE | TRRETCODE |
| 17 | NODATAFLG | tp NoData Flag | CHAR | 1 | 0 | TRTPNODFLG |  |
| 18 | OWNER | Owner | CHAR | 12 | 0 | TMSOWNER | AS4USER |
| 19 | PREFLG | tp Predecessor Flag | CHAR | 1 | 0 | TRTPPREFLG |  |
| 20 | PROJECT | CTS Project | CHAR | 20 | 0 | TRKORR_P | TRKORR |
| 21 | SRCCLI | Source client | CHAR | 3 | 0 | TRCLIENT | CHAR3 |
| 22 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 23 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 24 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 25 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 26 | SYSNAM | System Name(For) | CHAR | 10 | 0 | TMSSYSNAM | SYSNAME |
| 27 | TARCLI | Target client | CHAR | 3 | 0 | TRTARCLI | CHAR3 |
| 28 | TEXT | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 29 | TPSTATID | Time Stamp | CHAR | 20 | 0 | TRTPTSTAMP | CHAR20 |
| 30 | TRFUNC | Type of request/task | CHAR | 1 | 0 | TRFUNCTION | TRFUNCTION |
| 31 | TRKORR | Request/Task | CHAR | 20 | 0 | TRKORR | TRKORR |
| 32 | TRSTATUS | Status | CHAR | 1 | 0 | TRSTATUS | TRSTATUS |
| 33 | UMODES | Unconditional modes | CHAR | 20 | 0 | TRTPUMODES | CHAR20 |
| 34 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 34 parameters listed in the Parameters Reference Table above.

**ACTFLG** (tp Active Flag):

Indicates whether the transport request or task is active in the tp buffer. Used to restrict which buffer entries are considered.

**ACTFLG Options:**
- **X**: Active
- ** ** (space): Inactive or not set

**AS4DATE** (Date):

Date used for the change or last-change of the transport request. Used together with the time window (e.g. BACKDAYS) to restrict which transports are included.

**AS4TIME** (Time):

Time of last change of the transport request. Used with date and time-window parameters to scope the result set.

**BACKDAYS** (Days Backward from today):

Number of days to look back from today. When no explicit date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window for transport last-change date.

**BUFLVL** (Counter):

Buffer level counter in the TMS buffer. Used to identify or filter by buffer hierarchy level.

**BUFPOS** (Dictionary: Line item):

Line position in the buffer (dictionary line item). Used to target specific buffer entries.

**COMSYS** (System Name):

TMS system name (communication system). Used to restrict results to transports related to a specific system.

**CVERSFLG** (Result of Component Check):

Result of the component version check. Used to filter by component check outcome.

**DOMNAM** (Transport Domain):

Transport domain in TMS. Used to restrict results to a specific domain.

**DURATION** (Duration In Time Units):

Duration value in the unit given by DURATION_UNIT. The EI computes the time difference between each transport’s last-change date/time and the reference date/time, then filters by this range (e.g. transports changed within the last N hours or days).

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit in which DURATION is interpreted. The EI uses this when computing time difference and filtering by duration.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** DURATION is interpreted in the unit specified by DURATION_UNIT. Set DURATION_UNIT first, then the DURATION range (e.g. 0–24 for last 24 hours when DURATION_UNIT = H).

**IMPFLG** (tp Import Flag):

tp import flag. Used to filter by import status in the buffer.

**IMPFLG Options:**
- **X**: Set
- ** ** (space): Not set

**IMPSING** (Indicator):

Single-import indicator. Used to filter or flag single-import scenarios.

**IMPSING Options:**
- **X**: Set
- ** ** (space): Not set

**INT_MAXRC** (Return code (0/4/8/12)):

Return code as integer (0, 4, 8, 12). Used to filter transports by maximum return code; the EI maps this to the character MAXRC for the database filter.

**JOBID** (TMS CI Background Job Number):

TMS CI background job number. Used to filter by the job that processed the transport.

**MAXRC** (Return code):

Maximum return code of the transport (e.g. 0000, 0004, 0008, 0012). Used to filter by execution outcome; the EI also derives STATE_COLOR and STATE_ICON from this value.

**NODATAFLG** (tp NoData Flag):

tp no-data flag. Used to filter by presence or absence of data in the buffer.

**NODATAFLG Options:**
- **X**: Set
- ** ** (space): Not set

**OWNER** (Owner):

Owner of the transport request or task. Used to restrict results to a specific user.

**PREFLG** (tp Predecessor Flag):

tp predecessor flag. Used to filter by predecessor relationship in the buffer.

**PREFLG Options:**
- **X**: Set
- ** ** (space): Not set

**PROJECT** (CTS Project):

CTS project (transport request prefix/project). Used to restrict to a specific project or request namespace.

**SRCCLI** (Source client):

Source client of the request. Used to filter by client where the transport was created.

**STATE_COLOR** (State Color):

Status color derived from the transport return code (e.g. red for errors, yellow for warnings, green for success). The EI sets this from MAXRC and then filters the result set by this value.

**STATE_COLOR Options:**
- **R**: Red — critical (return code 0008 or 0012); requires immediate attention.
- **Y**: Yellow — warning (return code 0004).
- **G**: Green — success (return code 0000).
- ** ** (space or inactive): No color / inactive; used when MAXRC is initial.

**STATE_ICON** (State Icon):

Icon code representing the transport state. The EI derives this from MAXRC (and thus aligns with STATE_COLOR) for display; filtering by STATE_COLOR effectively restricts by the same semantic level as STATE_ICON.

**STATE_ICON Options:**
- **Icon codes** — Values from the ICON domain that represent the state (e.g. critical, warning, success). Specific codes map to the same semantics as STATE_COLOR (red/yellow/green/inactive).

**STATUS_DESC** (SW Message):

Short message or description for the status. Used for display and can be used in post-processing or filtering when the front end aligns by message.

**SW_DEST** (Cloud Destination):

Cloud destination for running the EI in cloud mode. When set, the EI delegates to the specified destination.

**SYSNAM** (System Name(For)):

TMS system name (target or context system). Used to restrict results to a specific system.

**TARCLI** (Target client):

Target client of the request. Used to filter by client where the transport is intended to be imported.

**TEXT** (Short Description):

Short description of the transport. Used to filter or display by description text.

**TPSTATID** (Time Stamp):

tp status time stamp. Used to filter or identify buffer entries by time stamp.

**TRFUNC** (Type of request/task):

Type of request or task (transport request vs. task). Used to restrict to requests or tasks only.

**TRFUNC Options:**
- **T**: Transport request
- **K**: Task

**TRKORR** (Request/Task):

Transport request or task ID. Used to restrict results to specific request(s) or task(s).

**TRSTATUS** (Status):

Status of the transport request or task (e.g. modifiable, released, locked). Used to filter by lifecycle status.

**TRSTATUS Options:**
- **D**: Modifiable
- **L**: Locked
- **O**: Released
- **N**: Modifiable (protected)

**UMODES** (Unconditional modes):

tp unconditional modes. Used to filter or control tp behavior.

**USER_FLD** (Dynamic Recipient User Field):

User-defined recipient or user field. Used for routing or filtering by dynamic user attribute.

**USER_FLD Options:** Values are function- or configuration-specific; derive from the EI configuration or user field catalog.


### Parameter Relationships

**Time and duration parameters:**

- **BACKDAYS**, **DURATION_UNIT**, and **DURATION** define the monitoring window and duration filter. When no explicit date range is supplied, the EI uses today minus BACKDAYS as the start date for transport last-change. DURATION is interpreted in the unit given by DURATION_UNIT; the EI computes the time difference per transport and filters by the DURATION range.

**Transport identifier:**

- **TRKORR** identifies the request or task. Use it to restrict results to specific transport(s).

**System and domain:**

- **DOMNAM**, **SYSNAM**, and **COMSYS** work together to restrict by transport domain and system (target system and communication system). Set these when focusing on a specific domain or system.

**Status and state:**

- **TRSTATUS** (request/task status) and **STATE_COLOR** (and **STATE_ICON**) work together: TRSTATUS filters by lifecycle (e.g. released), and STATE_COLOR filters by outcome severity (red/yellow/green) derived from return code. Filtering by STATE_COLOR aligns with the same semantic level as STATE_ICON.

**Client:**

- **SRCCLI** and **TARCLI** define source and target client. Use both when restricting by client pair (where the transport was created and where it is intended to be imported).

**Owner and type:**

- **OWNER** and **TRFUNC** restrict by owner and by type of request/task (request vs. task). Use together to narrow by who created the transport and whether it is a request or a task.


### Default Values

- **DURATION_UNIT** — Default: `H` (Hours when not supplied).
- **BACKDAYS** — Default: `1` (when not supplied).

### Practical Configuration Examples

**Use Case 1: Released transports in the last day with critical status**
```
BACKDAYS = 7
STATE_COLOR = R
TRSTATUS = O
```
**Purpose:** Find released transports from the last seven days that have a critical (red) return code, for quick identification of failed or warning imports.

**Use Case 2: Transports by owner and client pair**
```
OWNER = DEVUSER
SRCCLI = 100
TARCLI = 200
TRFUNC = T
```
**Purpose:** Restrict to transport requests (not tasks) created by a specific owner from source client 100 and target client 200.

**Use Case 3: Full-day filtering for a specific duration (DURATION_UNIT = F)**
```
DURATION_UNIT = F
DURATION = 30
STATE_COLOR = Y
DOMNAM = DOMAIN_A
SYSNAM = PRD_SYS
```
**Purpose:** Focus on transports with warning (yellow) status in a 30 full-day window for a specific domain and system. DURATION_UNIT = F with DURATION = 30 supports specific-day filtering.

**Use Case 4: Time window and return-code range**
```
DURATION_UNIT = H
DURATION = 0 - 24
TRSTATUS = O
STATE_COLOR = G
COMSYS = PRD_SYS
```
**Purpose:** Monitor released, successful (green) transports from the last 24 hours for a given communication system, for health checks.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_TRANSP_STAT | ACTFLG | tp Active Flag | CHAR(1) | TRTPACTFLG |
| /SKN/S_SW_01_01_TRANSP_STAT | AS4DATE | Date of Last Change | DATS(8) | AS4DATE |
| /SKN/S_SW_01_01_TRANSP_STAT | AS4TIME | Last changed at | TIMS(6) | AS4TIME |
| /SKN/S_SW_01_01_TRANSP_STAT | BUFLVL | Counter | NUMC(1) | COUNTER |
| /SKN/S_SW_01_01_TRANSP_STAT | BUFPOS | Dictionary: Line item | NUMC(6) | DDPOSITION |
| /SKN/S_SW_01_01_TRANSP_STAT | COMSYS | TMS: System Name | CHAR(10) | TMSSYSNAM |
| /SKN/S_SW_01_01_TRANSP_STAT | CVERSFLG | Result of Component Check | CHAR(3) | CCRESLT |
| /SKN/S_SW_01_01_TRANSP_STAT | DOMNAM | TMS: Transport Domain | CHAR(10) | TMSDOMNAM |
| /SKN/S_SW_01_01_TRANSP_STAT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_TRANSP_STAT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_TRANSP_STAT | IMPFLG | tp Import Flag | CHAR(1) | TRTPIMPFLG |
| /SKN/S_SW_01_01_TRANSP_STAT | IMPSING | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_TRANSP_STAT | JOBID | TMS CI Background Job Number | NUMC(10) | CIBJOBNUM |
| /SKN/S_SW_01_01_TRANSP_STAT | MAXRC | Return code | CHAR(4) | TRRETCODE |
| /SKN/S_SW_01_01_TRANSP_STAT | NODATAFLG | tp No Data Flag | CHAR(1) | TRTPNODFLG |
| /SKN/S_SW_01_01_TRANSP_STAT | OWNER | TMS: Owner | CHAR(12) | TMSOWNER |
| /SKN/S_SW_01_01_TRANSP_STAT | PREFLG | tp Predecessor Flag | CHAR(1) | TRTPPREFLG |
| /SKN/S_SW_01_01_TRANSP_STAT | PROJECT | Project in Change and Transport System | CHAR(20) | TRKORR_P |
| /SKN/S_SW_01_01_TRANSP_STAT | SRCCLI | Source client of request | CHAR(3) | TRCLIENT |
| /SKN/S_SW_01_01_TRANSP_STAT | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_TRANSP_STAT | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_TRANSP_STAT | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_01_TRANSP_STAT | SYSNAM | TMS: System Name | CHAR(10) | TMSSYSNAM |
| /SKN/S_SW_01_01_TRANSP_STAT | TARCLI | Target client for the request | CHAR(3) | TRTARCLI |
| /SKN/S_SW_01_01_TRANSP_STAT | TEXT | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_01_TRANSP_STAT | TPSTATID | tp Time Stamp | CHAR(20) | TRTPTSTAMP |
| /SKN/S_SW_01_01_TRANSP_STAT | TRFUNC | Type of request/task | CHAR(1) | TRFUNCTION |
| /SKN/S_SW_01_01_TRANSP_STAT | TRKORR | Request/Task | CHAR(20) | TRKORR |
| /SKN/S_SW_01_01_TRANSP_STAT | TRSTATUS | Status of request/task | CHAR(1) | TRSTATUS |
| /SKN/S_SW_01_01_TRANSP_STAT | UMODES | tp Unconditional Modes | CHAR(20) | TRTPUMODES |