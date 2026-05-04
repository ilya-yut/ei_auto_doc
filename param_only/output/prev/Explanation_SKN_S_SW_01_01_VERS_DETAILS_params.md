# Parameters: SKN_S_SW_01_01_VERS_DETAILS

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACTIVITY | IMG Activity | CHAR | 20 | 0 | TRACTIVITY | CUS_IMG_AC |
| 2 | ARCHIVED | Archiving status | CHAR | 1 | 0 | VRS_ARCHVD | VRS_ARCHVD |
| 3 | AS4DATE | Date | DATS | 8 | 0 | AS4DATE | AS4DATE |
| 4 | AS4POS | Dictionary: Line item | NUMC | 6 | 0 | DDPOSITION | DDPOSITION |
| 5 | AS4TEXT | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 6 | AS4TIME | Time | TIMS | 6 | 0 | AS4TIME | AS4TIME |
| 7 | AS4USER | Owner | CHAR | 12 | 0 | TR_AS4USER | AS4USER |
| 8 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 9 | DATUM | Date | DATS | 8 | 0 | VERSDATE | AS4DATE |
| 10 | DEFVERSNO | Version of object definition | NUMC | 5 | 0 | DEFVERSNO | VERSNO |
| 11 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 12 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 13 | FIRSTVERSN | Version number | NUMC | 5 | 0 | VERSFIRST | VERSNO |
| 14 | KEYLEN | Length of key | INT2 | 5 | 0 | VERSKEYLEN | VERSLEN |
| 15 | KORRDEV | Category | CHAR | 4 | 0 | TRCATEG | TRCATEG |
| 16 | KORRNUM | Request number | CHAR | 20 | 0 | VERSKORRNO | TRKORR |
| 17 | LANGU | Lang. | LANG | 1 | 0 | DDLANGUAGE | SPRAS |
| 18 | LASTVERSNO | Version number | NUMC | 5 | 0 | VERSLAST | VERSNO |
| 19 | LOCKFLAG | Lock/Import Status | CHAR | 1 | 0 | LOCKFLAG | TR_IMPORT_STATUS |
| 20 | LOEKZ | Special flag | CHAR | 1 | 0 | VERSLOEKZ | VERSLOEKZ |
| 21 | OBJECT | Object Type | CHAR | 4 | 0 | TROBJTYPE | OBJECT |
| 22 | OBJFUNC | Function | CHAR | 1 | 0 | OBJFUNC | OBJFUNC |
| 23 | OBJ_NAME | Obj. Name | CHAR | 120 | 0 | TROBJ_NAME | TROBJ_NAME |
| 24 | ORIGIN | Origin flag | CHAR | 1 | 0 | VERSORIGIN | VERSORIGIN |
| 25 | RELS | Release | CHAR | 4 | 0 | VERSRELS | SYCHAR04 |
| 26 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 27 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 28 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 29 | STEXT | Short text | CHAR | 74 | 0 | RS38L_FTXT | FUNCTEXT |
| 30 | STRKORR | Higher-Level Request | CHAR | 20 | 0 | STRKORR | TRKORR |
| 31 | TABLEN | Table width | INT2 | 5 | 0 | VERSTABLEN | VERSLEN |
| 32 | TARSYSTEM | Transport Target | CHAR | 10 | 0 | TR_TARGET | TR_TARGET |
| 33 | TRFUNCTION | Type of request/task | CHAR | 1 | 0 | TRFUNCTION | TRFUNCTION |
| 34 | TRKORR | Request/Task | CHAR | 20 | 0 | TRKORR | TRKORR |
| 35 | TRSTATUS | Status | CHAR | 1 | 0 | TRSTATUS | TRSTATUS |
| 36 | VERSMODE | Versioning Mode | CHAR | 1 | 0 | VERSMODE | VERSMODE |
| 37 | VERSNO | Version Number | NUMC | 5 | 0 | VERSNO | VERSNO |
| 38 | ZEIT | Time | TIMS | 6 | 0 | VERSTIME | AS4TIME |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 38 parameters listed in the Parameters Reference Table above.

**ACTIVITY** (IMG Activity):

IMG activity that wrote the entry to the object list. Used to restrict version details to a specific implementation activity.

**ARCHIVED** (Archiving status):

Archiving status of the version. Used to include or exclude archived version data.

**AS4DATE** (Date):

Date of last change of the request or task. Used to filter version details by change date.

**AS4POS** (Dictionary: Line item):

Line item position in the object list. Used to identify or filter by a specific line in the version data.

**AS4TEXT** (Short Description):

Short description of the repository object. Used to filter or search by object description text.

**AS4TIME** (Time):

Time of last change. Used together with AS4DATE to filter by change timestamp.

**AS4USER** (Owner):

Owner of the request or task. Used to restrict version details to a specific user or owner.

**BACKDAYS** (Backdays):

Number of days to look back from the reference date. Defines the time window for version data when no explicit date range is supplied.

**DATUM** (Date):

Version creation date. Used to filter version details by the date the version was created.

**DEFVERSNO** (Version of object definition):

Version number of the object definition. Used to filter by definition version.

**DURATION** (Duration In Time Units):

Length of the time window in the unit given by DURATION_UNIT. Defines the span for version analysis when used with DURATION_UNIT.

**DURATION_UNIT** (Duration Unit):

Time unit for DURATION. Must be used together with DURATION to define the analysis window.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**FIRSTVERSN** (Version number):

Lowest (first) version number in the version range. Used to filter by version range start.

**KEYLEN** (Length of key):

Key length of the table in the version database. Used when filtering or interpreting version key structure.

**KORRDEV** (Category):

Request or task category (e.g. client-specific vs cross-client). Used to restrict by transport category.

**KORRNUM** (Request number):

Transport request number associated with the version. Used to filter version details by transport.

**LANGU** (Lang.):

Language for descriptions and text. Used when the EI returns or displays language-dependent content; when not supplied, English is used.

**LASTVERSNO** (Version number):

Highest (last) version number in the version range. Used to filter by version range end.

**LOCKFLAG** (Lock/Import Status):

Lock or import status of the object entry. Used to filter by lock or import state.

**LOEKZ** (Special flag):

Special flag in version management. Used to filter by deletion or special version flag.

**OBJECT** (Object Type):

Repository object type. Used to restrict version details to a specific object type (e.g. PROG, TABL).

**OBJFUNC** (Function):

Object function. Used to filter by the function of the object in the list.

**OBJ_NAME** (Obj. Name):

Object name in the object list. Used to restrict version details to a specific object name or pattern.

**ORIGIN** (Origin flag):

Origin identifier of the version. Used to filter by version origin.

**RELS** (Release):

Release associated with the version. Used to filter by SAP release.

**STATE_COLOR** (State Color):

State color of the version or alert (e.g. green, yellow, red). Used to filter or display by state color when the parameter is active in the selection.

**STATE_COLOR Options:**
- **R** — Red indicating critical items requiring immediate attention.
- **Y** — Yellow indicating warning-level items for standard monitoring.
- **G** — Green indicating informational or success state.
- **B** — Blue indicating system-level or technical state.

**STATE_ICON** (State Icon):

Icon code representing the state. Used for display or filtering by state when the parameter is active; aligns with STATE_COLOR semantics.

**STATE_ICON Options:**
- Icon codes from the ICON domain that represent the state (e.g. critical, warning, success). Values map to the same semantics as STATE_COLOR (red/yellow/green/inactive).

**STATUS_DESC** (SW Message):

Message or status description. Used to filter or display by status text.

**STEXT** (Short text):

Short text for the function or object. Used to filter or display by short text.

**STRKORR** (Higher-Level Request):

Higher-level (parent) request. Used to restrict version details to tasks under a specific request.

**TABLEN** (Table width):

Width of the table in the version database. Used when interpreting version table structure.

**TARSYSTEM** (Transport Target):

Target system of the transport. Used to filter by transport target system.

**TRFUNCTION** (Type of request/task):

Type of request or task (e.g. Workbench vs Customizing). Used to restrict results to development or Customizing transports.

**TRFUNCTION Options:**
- **K** — Workbench (development) request/task
- **W** — Customizing request/task
- **T** — Transport of copies
- **R** — Repair
- **S** — Development (transport)

**TRKORR** (Request/Task):

Request or task ID. Used to restrict version details to specific transport IDs or a range.

**TRSTATUS** (Status):

Status of the request or task (e.g. modifiable, released). Used to filter by lifecycle status.

**TRSTATUS Options:**
- **D** — Modifiable (development in progress)
- **R** — Released
- **O** — Other statuses as defined in the domain

**VERSMODE** (Versioning Mode):

Version creation type. Used to filter by how the version was created.

**VERSNO** (Version Number):

Version number. Used to filter version details by a specific version or version range.

**ZEIT** (Time):

Version creation time. Used to filter by the time the version was created.


### Parameter Relationships

**Time window**

- **DURATION**, **DURATION_UNIT**, and **BACKDAYS** define the time span for version analysis. BACKDAYS is the lookback in days; DURATION and DURATION_UNIT define an alternative window (e.g. last 24 hours, last 7 days). Use one approach per scenario so the EI uses the correct time range.

**Version and transport**

- **VERSNO**, **KORRNUM**, and **AS4USER** relate to version number, associated transport request, and owner. The EI uses them to filter version data by version range, transport, and author.

**Request and task**

- **TRKORR** and **STRKORR** work together: TRKORR identifies the request or task; STRKORR identifies the higher-level request. Use both when focusing on a main request and its tasks.

**Request status and type**

- **TRSTATUS** and **TRFUNCTION** describe lifecycle status and type of the request/task. The EI uses them in selection; use together to filter by status (e.g. released) and type (e.g. Workbench or Customizing).

**Object identification**

- **OBJECT** and **OBJ_NAME** identify the repository object type and name. Use together to restrict version details to a specific object (e.g. program, table).

**Version range and metadata**

- **FIRSTVERSN**, **LASTVERSNO**, **DEFVERSNO**, **DATUM**, and **ZEIT** describe version range and creation metadata. Use to filter by version interval or creation date/time.

**State and display**

- **STATE_COLOR** and **STATE_ICON** (when active) work together to filter and display version or alert state; STATE_ICON aligns with STATE_COLOR semantics.


### Default Values

**DURATION_UNIT** — Default: `H` (Hours). Used when not supplied by the user.

**BACKDAYS** — Default: `1`. Number of days to look back when no explicit value is supplied.

**LANGU** — Default: `E` (English when not supplied). Language for descriptions and text.

### Practical Configuration Examples

**Use Case 1: Last 24 hours by transport status**
```
DURATION = 24
DURATION_UNIT = H
TRSTATUS = R
TRFUNCTION = K
```
**Purpose:** Released Workbench transports in the last 24 hours. Use for recent development transport monitoring.

**Use Case 2: Version range and owner**
```
VERSNO = 1 - 99999
AS4USER = DEVELOPER01
BACKDAYS = 7
LANGU = E
```
**Purpose:** All versions in the last 7 days for a specific owner, English text. Use for author-based version analysis.

**Use Case 3: Full days and single duration**
```
DURATION = 1
DURATION_UNIT = F
BACKDAYS = 1
TRKORR = *
STRKORR = *
```
**Purpose:** Full-day filtering with duration 1 and one day lookback; any request/task. Use for day-level version coverage.

**Use Case 4: Object type, state, and target system**
```
OBJECT = PROG
OBJ_NAME = *
STATE_COLOR = R
TARSYSTEM = PRD
TRSTATUS = R
```
**Purpose:** Released programs with red state on production target. Use for critical-object monitoring across systems.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_VERS_DETAILS | ACTIVITY | Activity that wrote the entry to the object list | CHAR(20) | TRACTIVITY |
| /SKN/S_SW_01_01_VERS_DETAILS | ARCHIVED | Archiving flag for version management | CHAR(1) | VRS_ARCHVD |
| /SKN/S_SW_01_01_VERS_DETAILS | AS4DATE | Date of Last Change | DATS(8) | AS4DATE |
| /SKN/S_SW_01_01_VERS_DETAILS | AS4POS | Dictionary: Line item | NUMC(6) | DDPOSITION |
| /SKN/S_SW_01_01_VERS_DETAILS | AS4TEXT | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_01_VERS_DETAILS | AS4TIME | Last changed at | TIMS(6) | AS4TIME |
| /SKN/S_SW_01_01_VERS_DETAILS | AS4USER | Owner of a Request or Task | CHAR(12) | TR_AS4USER |
| /SKN/S_SW_01_01_VERS_DETAILS | DATUM | Version management: Version creation date | DATS(8) | VERSDATE |
| /SKN/S_SW_01_01_VERS_DETAILS | DEFVERSNO | Version number of the object definition | NUMC(5) | DEFVERSNO |
| /SKN/S_SW_01_01_VERS_DETAILS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_VERS_DETAILS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_VERS_DETAILS | FIRSTVERSN | Version control: lowest version number (intern) | NUMC(5) | VERSFIRST |
| /SKN/S_SW_01_01_VERS_DETAILS | KEYLEN | Key length of table in version data base | INT2(5) | VERSKEYLEN |
| /SKN/S_SW_01_01_VERS_DETAILS | KORRDEV | Request or task category | CHAR(4) | TRCATEG |
| /SKN/S_SW_01_01_VERS_DETAILS | KORRNUM | Version management: Version request number | CHAR(20) | VERSKORRNO |
| /SKN/S_SW_01_01_VERS_DETAILS | LANGU | Language Key | LANG(1) | DDLANGUAGE |
| /SKN/S_SW_01_01_VERS_DETAILS | LASTVERSNO | Version control: last version number (intern) | NUMC(5) | VERSLAST |
| /SKN/S_SW_01_01_VERS_DETAILS | LOCKFLAG | Lock status or import status of an object entry | CHAR(1) | LOCKFLAG |
| /SKN/S_SW_01_01_VERS_DETAILS | LOEKZ | Version management: Special flag | CHAR(1) | VERSLOEKZ |
| /SKN/S_SW_01_01_VERS_DETAILS | OBJECT | Object Type | CHAR(4) | TROBJTYPE |
| /SKN/S_SW_01_01_VERS_DETAILS | OBJFUNC | Object function | CHAR(1) | OBJFUNC |
| /SKN/S_SW_01_01_VERS_DETAILS | OBJ_NAME | Object Name in Object List | CHAR(120) | TROBJ_NAME |
| /SKN/S_SW_01_01_VERS_DETAILS | ORIGIN | Version management: Origin ID | CHAR(1) | VERSORIGIN |
| /SKN/S_SW_01_01_VERS_DETAILS | RELS | Version management: Release | CHAR(4) | VERSRELS |
| /SKN/S_SW_01_01_VERS_DETAILS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_VERS_DETAILS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_VERS_DETAILS | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_01_VERS_DETAILS | STEXT | Short text for function module | CHAR(74) | RS38L_FTXT |
| /SKN/S_SW_01_01_VERS_DETAILS | STRKORR | Higher-Level Request | CHAR(20) | STRKORR |
| /SKN/S_SW_01_01_VERS_DETAILS | TABLEN | Width of table in the version data base | INT2(5) | VERSTABLEN |
| /SKN/S_SW_01_01_VERS_DETAILS | TARSYSTEM | Transport Target of Request | CHAR(10) | TR_TARGET |
| /SKN/S_SW_01_01_VERS_DETAILS | TRFUNCTION | Type of request/task | CHAR(1) | TRFUNCTION |
| /SKN/S_SW_01_01_VERS_DETAILS | TRKORR | Request/Task | CHAR(20) | TRKORR |
| /SKN/S_SW_01_01_VERS_DETAILS | TRSTATUS | Status of request/task | CHAR(1) | TRSTATUS |
| /SKN/S_SW_01_01_VERS_DETAILS | VERSMODE | Version management: Version creation type | CHAR(1) | VERSMODE |
| /SKN/S_SW_01_01_VERS_DETAILS | VERSNO | Version Management: Version Number | NUMC(5) | VERSNO |
| /SKN/S_SW_01_01_VERS_DETAILS | ZEIT | Version management: Version creation time | TIMS(6) | VERSTIME |