# Parameters: SKN_S_SW_01_01_SP01

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACCESS | Action | CHAR | 4 | 0 | RSPOACTION | RSPOACTION |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 4 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 5 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 6 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 7 | NONEX_DEV | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 8 | PAGES | Pages |  | 0 | 0 |  |  |
| 9 | PROTO | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 10 | RFCDEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 11 | RFCSYSTEM | SAP System ID | CHAR | 8 | 0 | SYSYSID | SYCHAR08 |
| 12 | RQ0NAME | Spool request name | CHAR | 6 | 0 | RSPO0NAME | CHAR6 |
| 13 | RQ1DISPO | Print immediately or later | CHAR | 1 | 0 | RSPO1DISPO | CHAR1 |
| 14 | RQ1NAME | Suffix 1 | CHAR | 4 | 0 | RSPO1NAME | CHAR4 |
| 15 | RQ2DISPO | Delete spool request automatically | CHAR | 1 | 0 | RSPO2DISPO | CHAR1 |
| 16 | RQ2NAME | Suffix 2 | CHAR | 12 | 0 | RSPO2NAME | CHAR12 |
| 17 | RQ3DISPO | Spool option 3: Automatic re-routing | CHAR | 1 | 0 | RSPO3DISPO | CHAR1 |
| 18 | RQADESL | Output Device | CHAR | 30 | 0 | RSPOLNAME | RSPOLNAME |
| 19 | RQAPPRULE | Add rule no. | INT2 | 5 | 0 | RSTSAPPRUL | SAPUSHORT |
| 20 | RQARCHDEST | Archiving device | CHAR | 4 | 0 | RSPOARCHDE | RSPOPNAME |
| 21 | RQARCHSTAT | Archive status | CHAR | 1 | 0 | RSPOARCHST | RSPOARCHST |
| 22 | RQARCHTYPE | Archive | CHAR | 1 | 0 | RSPOARCHTY | RSPOARCHTY |
| 23 | RQAUTH | Authorization | CHAR | 12 | 0 | RSPOAUTH | RSPOAUTH |
| 24 | RQCLIENT | Client | CLNT | 3 | 0 | RSTSCLIENT | MANDT |
| 25 | RQCMODE | External mode of an SAP dialog | CHAR | 1 | 0 | SAPMODE | CHAR1 |
| 26 | RQCOPIES | Number of copies | INT1 | 3 | 0 | RSPOCOPIES | RSPOCNT1 |
| 27 | RQCRED | Creation Time | DATS | 8 | 0 | RSPOCREDAT | RSTSDATUMD |
| 28 | RQCRET | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 29 | RQCRETIME | Creation time | CHAR | 16 | 0 | RSPOCRTIME | RSLGTIME |
| 30 | RQDELD | Delete date | DATS | 8 | 0 | RSPODELDAT | RSTSDATUMD |
| 31 | RQDELETED | Deleted | CHAR | 1 | 0 | RSPODELFLG | CHAR1 |
| 32 | RQDELRULE | Deletion rule number | INT2 | 5 | 0 | RSTSDELRUL | SAPUSHORT |
| 33 | RQDELTIME | Deleted At | CHAR | 16 | 0 | RSPODLTIME | RSLGTIME |
| 34 | RQDEST | Output Device | CHAR | 4 | 0 | RSPOPNAME | RSPOPNAME |
| 35 | RQDESTL | Output Device | CHAR | 30 | 0 | RSPOLNAME | RSPOLNAME |
| 36 | RQDIVISION | Department | CHAR | 12 | 0 | RSPODIVISI | TEXT12 |
| 37 | RQDOCTYPE | Document type | CHAR | 6 | 0 | RSPODOCTYP | CHAR6 |
| 38 | RQERR | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 39 | RQERROR | Spool: Error status | CHAR | 1 | 0 | RSPOERROR | CHAR1 |
| 40 | RQFINAL | Spool request completed | CHAR | 1 | 0 | RSPOFINAL | CHAR1 |
| 41 | RQIDENT | Spool request number | INT4 | 10 | 0 | RSPOID | RSPOID |
| 42 | RQISPROTOK | Log flag | CHAR | 1 | 0 | RSPOISPROT | CHAR1 |
| 43 | RQMODRULE | Change rule no. | INT2 | 5 | 0 | RSTSMODRUL | SAPUSHORT |
| 44 | RQMODTIME | Last changed at | CHAR | 16 | 0 | RSPOMODTIM | RSLGTIME |
| 45 | RQNONE | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 46 | RQO1CLIE | Client | CLNT | 3 | 0 | RSTSCLIENT | MANDT |
| 47 | RQO1NAME | TemSe object name | CHAR | 20 | 0 | RSTSONAME | CHAR20 |
| 48 | RQO1PART | Part of object | INT2 | 5 | 0 | RSTSPART | SAPUSHORT |
| 49 | RQOWNER | User name | CHAR | 12 | 0 | RSPOUSER | USERNAME |
| 50 | RQPAGE | Pages | INT4 | 10 | 0 | RSPOPAGES |  |
| 51 | RQPAPER | Format | CHAR | 16 | 0 | RSPOPAPFAM | RSPOPAPER |
| 52 | RQPJDONE | OutputReq. processed | INT2 | 5 | 0 | RSPOPJDONE | RSPOPJCNT |
| 53 | RQPJHERR | No. output req.w/err | INT2 | 5 | 0 | RSPOPJHERR | RSPOPJCNT |
| 54 | RQPJREQ | Spool request total | INT2 | 5 | 0 | RSPOPJTOTL | RSPOPJCNT |
| 55 | RQPJSERR | Output req. w/ probl | INT2 | 5 | 0 | RSPOPJSERR | RSPOPJCNT |
| 56 | RQPOSNAME | Host printer | CHAR | 50 | 0 | RSPOPRNAME | TEXT50 |
| 57 | RQPRIO | Priority | INT1 | 3 | 0 | RSPOPRIO | RSPOPRIO |
| 58 | RQPROC | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 59 | RQREARULE | Read rule no. | INT2 | 5 | 0 | RSTSREARUL | SAPUSHORT |
| 60 | RQRECEIVER | Recipient | CHAR | 12 | 0 | RSPORECEIV | TEXT12 |
| 61 | RQSAPTITLE | SAP cover page | CHAR | 1 | 0 | RSPOSAPTTL | RSPOSAPTTL |
| 62 | RQSTATUS_V | Output status | CHAR | 7 | 0 | RQSTATUS | CHAR7 |
| 63 | RQSUCC | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 64 | RQTELELAN | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 65 | RQTELENUM | Recipient number | CHAR | 30 | 0 | SKTELNR | TDTELENUM |
| 66 | RQTELENUME | Recipient number | CHAR | 30 | 0 | SKTELNR | TDTELENUM |
| 67 | RQTITLE | Title or name of spool request | CHAR | 68 | 0 | RSPOTITLE | TEXT68 |
| 68 | RQUNXTITLE | OS Cover Sheet | CHAR | 1 | 0 | RSPOUNXTTL | RSPOUNXTTL |
| 69 | RQWRITER | TemSe: General counter | INT2 | 5 | 0 | RSTSCNT | SAPUSHORT |
| 70 | RQ_ACCESS | Spool request number | INT4 | 10 | 0 | RSPOID | RSPOID |
| 71 | RQ_NO_ACCESS | Spool request number | INT4 | 10 | 0 | RSPOID | RSPOID |
| 72 | SYS | SAP System ID | CHAR | 8 | 0 | RSPO_SYS | SYCHAR08 |
| 73 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 73 parameters listed in the Parameters Reference Table above.

**ACCESS** (Action):

Identifies the type of action applicable to spool requests in the monitoring context. Used when the EI evaluates or filters spool-related actions.

**BACKDAYS** (Backdays):

Number of days to look back from the current date to form the start of the monitoring window for spool request creation. When no date range is supplied, the EI uses the current date minus BACKDAYS as the start date.

**DURATION** (Duration In Time Units):

Duration of the spool request in the unit defined by DURATION_UNIT (hours, minutes, days, or full days). The EI computes duration from creation date/time to the reference date/time and filters results by this range.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed and in which the EI computes time difference for spool requests.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**LANGU** (Language for texts):

Language key used for text resolution when the EI returns language-dependent descriptions. When not supplied, the function uses the system logon language.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set, date and time evaluation for the monitoring window and duration uses UTC; otherwise the system uses the local time of the application server.

**MANAGE_IN_UTC Options:**
- **X**: Manage in UTC
- ** ** (space): Use local time

**NONEX_DEV** (Indicator):

Flag controlling behavior related to non-existent or unavailable output devices in spool selection.

**NONEX_DEV Options:**
- **X**: Set/active
- ** ** (space): Not set/inactive

**PAGES** (Pages):

Page count or page range used to restrict which spool requests are included (e.g. by number of pages). The EI passes this to the spool selection and filters the result set accordingly.

**PROTO** (Indicator):

Indicator that influences whether protocol or log-related spool requests are included in the selection.

**PROTO Options:**
- **X**: Set/active
- ** ** (space): Not set/inactive

**RFCDEST** (RFC Destination):

Logical RFC destination used when the EI runs in distributed or cloud mode. When set, the actual data retrieval is delegated to the specified system.

**RFCSYSTEM** (SAP System ID):

SAP system identifier used to restrict spool requests to a specific application server or system. The EI filters results by this system ID when supplied.

**RQ0NAME** (Spool request name):

Name component (first part) of the spool request. Used to filter spool requests by the primary name component.

**RQ1DISPO** (Print immediately or later):

Disposition flag for print timing (immediate or delayed). Used in spool request attributes and filtering.

**RQ1NAME** (Suffix 1):

First suffix of the spool request name. Used together with RQ0NAME and RQ2NAME to target specific spool request naming patterns.

**RQ2DISPO** (Delete spool request automatically):

Flag controlling automatic deletion of the spool request after output. Used in spool request selection and behavior.

**RQ2NAME** (Suffix 2):

Second suffix of the spool request name. Used with RQ0NAME and RQ1NAME to filter by full spool request name.

**RQ3DISPO** (Spool option 3: Automatic re-routing):

Option for automatic re-routing of spool output. Used in spool request attributes.

**RQADESL** (Output Device):

Long name of the output (printer) device. The EI filters spool requests by this device name when the parameter is supplied.

**RQAPPRULE** (Add rule no.):

TemSe add-protection rule number associated with the spool request. Used for filtering or attributing spool requests by protection rules.

**RQARCHDEST** (Archiving device):

Archiving device to which spool requests may be or have been sent. Used to filter by archive destination.

**RQARCHSTAT** (Archive status):

Status of the spool request with respect to archiving (e.g. ready for archive, released, finished, cancelled, planned). The EI can filter by this status when supplied.

**RQARCHSTAT Options:**
- **A**: Ready for archive
- **R**: Released
- **F**: Finished
- **X**: Cancelled
- **P**: Planned

**RQARCHTYPE** (Archive):

Archive type (internal classification). Used to filter or classify spool requests by archive type.

**RQAUTH** (Authorization):

Authorization value used for spool request checks. The EI restricts results to spool requests matching this authorization when supplied.

**RQCLIENT** (Client):

Client (mandant) in which the spool request was created. The EI filters spool requests by client when this parameter is supplied.

**RQCMODE** (External mode of an SAP dialog):

Session or process mode (dialog, background, RFC, update, enqueue, spool, system) that generated or is associated with the spool request. Used to filter by execution context.

**RQCMODE Options:**
- **A**: Dialog (interactive user session)
- **B**: Background (batch job)
- **C**: RFC or communication (external call)
- **D**: Update
- **E**: Enqueue
- **F**: Spool
- **S**: System

**RQCOPIES** (Number of copies):

Number of copies requested for the spool output. Used to filter or display spool request attributes.

**RQCRED** (Creation Time):

Creation date of the spool request. The EI uses this (together with BACKDAYS when no range is given) to build the selection window and filters results by creation date.

**RQCRET** (Time):

Creation time of the spool request. Used with RQCRED for precise creation date/time filtering.

**RQCRETIME** (Creation time):

Combined creation date and time of the spool request (character format). Used for selection and display.

**RQDELD** (Delete date):

Planned or actual delete date of the spool request. Used to filter by deletion date.

**RQDELETED** (Deleted):

Flag indicating whether the spool request has been marked as deleted.

**RQDELETED Options:**
- **X**: Deleted
- ** ** (space): Not deleted

**RQDELRULE** (Deletion rule number):

TemSe deletion protection rule number. Used for filtering or attributing spool requests.

**RQDELTIME** (Deleted At):

Timestamp when the spool request was deleted. Used for filtering or display.

**RQDEST** (Output Device):

Short (4-character) output device code. The EI converts this to the long device name for display and filters by output device when supplied.

**RQDESTL** (Output Device):

Long name of the output device. The EI filters spool requests by this device name; results are restricted to the given device(s).

**RQDIVISION** (Department):

Department or division associated with the spool request. The EI filters results by this value when supplied.

**RQDOCTYPE** (Document type):

Document type of the spool request. Used to filter or classify by document type.

**RQERR** (Indicator):

Flag indicating error status of the spool request. Used in selection (e.g. include requests with errors).

**RQERR Options:**
- **X**: Set/active (include error requests)
- ** ** (space): Not set/inactive

**RQERROR** (Spool: Error status):

Detailed error status of the spool request. Used to filter by error state.

**RQFINAL** (Spool request completed):

Flag indicating whether the spool request has been fully processed (completed).

**RQFINAL Options:**
- **X**: Completed
- ** ** (space): Not completed

**RQIDENT** (Spool request number):

Unique spool request identifier. The EI filters results by this number (or range) when supplied; used to target specific spool requests.

**RQISPROTOK** (Log flag):

Flag indicating whether the spool request is a protocol or log for another request. Used for filtering.

**RQISPROTOK Options:**
- **X**: Is protocol/log
- ** ** (space): Not a protocol

**RQMODRULE** (Change rule no.):

TemSe change protection rule number. Used for filtering or attributing spool requests.

**RQMODTIME** (Last changed at):

Timestamp of the last change to the spool request. Used for filtering or display.

**RQNONE** (Indicator):

Flag used in spool selection (e.g. include requests with no specific status). Passed to the underlying spool selection function.

**RQNONE Options:**
- **X**: Set/active
- ** ** (space): Not set/inactive

**RQO1CLIE** (Client):

Client for the TemSe object linked to the spool request. Used for filtering by object client.

**RQO1NAME** (TemSe object name):

Name of the TemSe object. Used to filter spool requests by associated TemSe object.

**RQO1PART** (Part of object):

Part number of the TemSe object. Used for filtering when multiple parts exist.

**RQOWNER** (User name):

User who created or owns the spool request. The EI filters spool requests by this user when supplied.

**RQPAGE** (Pages):

Number of pages of the spool request. The EI filters results by page count (or range) when this parameter is supplied.

**RQPAPER** (Format):

Paper or format type of the spool request. Used to filter or display format.

**RQPJDONE** (OutputReq. processed):

Number of output requests already processed. Used for filtering or display of processing status.

**RQPJHERR** (No. output req.w/err):

Number of output requests with errors. Used to filter or display error counts.

**RQPJREQ** (Spool request total):

Total number of output requests for the spool request. Used for filtering or display.

**RQPJSERR** (Output req. w/ probl):

Number of output requests with problems. Used to filter or display.

**RQPOSNAME** (Host printer):

Host printer name. Used to filter or display the physical printer.

**RQPRIO** (Priority):

Priority of the spool or print request. Used to filter or display priority.

**RQPROC** (Indicator):

Flag used in spool selection (e.g. include requests in processing). Passed to the underlying spool selection function.

**RQPROC Options:**
- **X**: Set/active
- ** ** (space): Not set/inactive

**RQREARULE** (Read rule no.):

TemSe read protection rule number. Used for filtering or attributing spool requests.

**RQRECEIVER** (Recipient):

Recipient of the spool request. The EI filters results by recipient when supplied.

**RQSAPTITLE** (SAP cover page):

Indicator for use of the SAP cover page. Used in spool request attributes.

**RQSAPTITLE Options:**
- **X**: Use SAP cover page
- ** ** (space): Do not use

**RQSTATUS_V** (Output status):

Output status of the spool request (e.g. completed, error, processing). The EI retrieves status text and filters results by this status when supplied.

**RQSUCC** (Indicator):

Flag used in spool selection (e.g. include successfully completed requests). Passed to the underlying spool selection function.

**RQSUCC Options:**
- **X**: Set/active
- ** ** (space): Not set/inactive

**RQTELELAN** (Country Key):

Country key for telecommunications or recipient. Used for filtering or display.

**RQTELENUM** (Recipient number):

Recipient telephone number. Used for filtering or display.

**RQTELENUME** (Recipient number):

Alternate recipient number field. Used for filtering or display.

**RQTITLE** (Title or name of spool request):

Title or descriptive name of the spool request. The EI filters results by title when supplied; the output concatenates name components into this field.

**RQUNXTITLE** (OS Cover Sheet):

Indicator for use of the operating system cover sheet. Used in spool request attributes.

**RQUNXTITLE Options:**
- **X**: Use OS cover sheet
- ** ** (space): Do not use

**RQWRITER** (TemSe: General counter):

TemSe writer or general counter. Used for filtering or display of internal attributes.

**RQ_ACCESS** (Spool request number):

Spool request number returned by the underlying API for requests the user can access. Used for access control or filtering.

**RQ_NO_ACCESS** (Spool request number):

Spool request number returned by the underlying API for requests the user cannot access. Used for access control or exclusion.

**SYS** (SAP System ID):

SAP system ID where the spool request was created or is stored. The EI filters results by system when supplied.

**USER_FLD** (Dynamic Recipient User Field):

User-defined field code used for dynamic recipient or user classification (e.g. user group or role). Used to filter or attribute spool requests by custom user field.

**USER_FLD Options:**
- **USR01**: User group 1 (or user-defined field 1)
- **USR02**: User group 2 (or user-defined field 2)


### Parameter Relationships

**Time-based parameters:**

- **BACKDAYS**, **RQCRED**, **DURATION**, and **DURATION_UNIT** work together to define the monitoring window and duration filter. When no date range is supplied, the EI uses the current date minus BACKDAYS as the start date and builds the selection range for RQCRED (creation time). DURATION is computed from creation date/time to the reference date/time using DURATION_UNIT (hours, minutes, days, or full days); results are then filtered by the DURATION range.
- Use BACKDAYS to set how far back the window starts; use RQCRED (or the implied range) to restrict by creation date; use DURATION and DURATION_UNIT to restrict by how long ago the spool request was created.

**Spool identity and ownership:**

- **RQIDENT**, **RQOWNER**, **RQCLIENT**, **RQAUTH**, **RQDESTL**, **RQTITLE**, **RQRECEIVER**, **RQDIVISION**, and **SYS** are used together to identify and filter spool requests. The EI passes client, owner, creation date, and device criteria to the spool selection API and then applies additional filters (e.g. RQDESTL, RQDIVISION, SYS, RQIDENT, RQAUTH, RQTITLE, RQRECEIVER) on the result set.
- Set RQOWNER to restrict by user; RQCLIENT by client; RQDESTL by output device; RQTITLE and RQRECEIVER by title and recipient; RQDIVISION by department; SYS by system ID; and RQIDENT to target specific spool request numbers.

**Output device and page scope:**

- **RQDESTL** and **PAGES** (with **RQPAGE** in the output) define which output devices and page counts are included. The EI filters the result set by RQDESTL and by page criteria (PAGES selection) so that only spool requests matching the chosen devices and page range are returned.
- Use RQDESTL to limit to specific printers or devices; use PAGES to limit by number of pages (e.g. only large jobs).

**Status and duration filter:**

- **RQSTATUS_V** and **DURATION** (with **DURATION_UNIT**) are applied after data retrieval: the EI computes duration per spool request and filters by the DURATION range, and filters by output status (RQSTATUS_V). Used together, they narrow results to spool requests with the desired status and within the desired age in the chosen time unit.
- Set RQSTATUS_V to filter by output status (e.g. completed, error); set DURATION and DURATION_UNIT to keep only requests within a certain age.


### Default Values

- **LANGU** — Default: system language (when not supplied).
- **DURATION_UNIT** — Default: `H` (Hours when not supplied).
- **BACKDAYS** — Default: `1` (when not supplied).

### Practical Configuration Examples

**Use Case 1: Last 7 days, duration in hours**
```
BACKDAYS = 7
DURATION_UNIT = H
DURATION = 0 - 168
```
**Purpose:** Monitor spool requests created in the last 7 days and restrict to those with a computed duration of 0–168 hours (up to one week old). Suitable for short-term spool monitoring.

**Use Case 2: Full-day filtering for specific day range**
```
DURATION_UNIT = F
DURATION = 30
RQOWNER = specific_user
RQDESTL = LP01
```
**Purpose:** Use full days (F) for specific day filtering: include spool requests up to 30 full days old, restricted by owner and output device. Useful for monthly reporting by user and printer.

**Use Case 3: High-volume and error focus**
```
BACKDAYS = 14
RQSTATUS_V = ERROR
PAGES = 100 - 99999
RQDIVISION = FINANCE
MANAGE_IN_UTC = X
```
**Purpose:** Focus on high-page-count spool requests in the Finance department over the last 14 days, with error status, using UTC for consistent time handling across systems.

**Use Case 4: Targeted spool request and device**
```
RQIDENT = 12345678 - 12345999
RQDESTL = SAPPDF
RQCRED = 20250101 - 20250302
LANGU = E
```
**Purpose:** Retrieve specific spool request numbers in a range, for a given output device and creation date range, with English texts. Useful for auditing or troubleshooting a known set of jobs.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_SP01 | ACCESS | Authorization field for spool actions | CHAR(4) | RSPOACTION |
| /SKN/S_SW_01_01_SP01 | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_SP01 | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_SP01 | NONEX_DEV | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_SP01 | PROTO | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_SP01 | RFCDEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_01_SP01 | RFCSYSTEM | Name of SAP System | CHAR(8) | SYSYSID |
| /SKN/S_SW_01_01_SP01 | RQ0NAME | Spool request: Name | CHAR(6) | RSPO0NAME |
| /SKN/S_SW_01_01_SP01 | RQ1DISPO | Print immediately or later | CHAR(1) | RSPO1DISPO |
| /SKN/S_SW_01_01_SP01 | RQ1NAME | Spool request: Suffix 1 | CHAR(4) | RSPO1NAME |
| /SKN/S_SW_01_01_SP01 | RQ2DISPO | Delete spool request automatically | CHAR(1) | RSPO2DISPO |
| /SKN/S_SW_01_01_SP01 | RQ2NAME | Spool request: Suffix 2 | CHAR(12) | RSPO2NAME |
| /SKN/S_SW_01_01_SP01 | RQ3DISPO | Spool option 3: Automatic re-routing | CHAR(1) | RSPO3DISPO |
| /SKN/S_SW_01_01_SP01 | RQADESL | Spool: Long device names | CHAR(30) | RSPOLNAME |
| /SKN/S_SW_01_01_SP01 | RQAPPRULE | TemSe: Number of add protection rule | INT2(5) | RSTSAPPRUL |
| /SKN/S_SW_01_01_SP01 | RQARCHDEST | Archiving device | CHAR(4) | RSPOARCHDE |
| /SKN/S_SW_01_01_SP01 | RQARCHSTAT | Spool: Archive status of spool request | CHAR(1) | RSPOARCHST |
| /SKN/S_SW_01_01_SP01 | RQARCHTYPE | Archive type (internal, not on screen) | CHAR(1) | RSPOARCHTY |
| /SKN/S_SW_01_01_SP01 | RQAUTH | Value for authorization check | CHAR(12) | RSPOAUTH |
| /SKN/S_SW_01_01_SP01 | RQCLIENT | Client for which object was generated | CLNT(3) | RSTSCLIENT |
| /SKN/S_SW_01_01_SP01 | RQCMODE | External mode of an SAP dialog | CHAR(1) | SAPMODE |
| /SKN/S_SW_01_01_SP01 | RQCOPIES | Number of copies | INT1(3) | RSPOCOPIES |
| /SKN/S_SW_01_01_SP01 | RQCRED | Date spool request was created | DATS(8) | RSPOCREDAT |
| /SKN/S_SW_01_01_SP01 | RQCRET | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_01_01_SP01 | RQCRETIME | Time a spool request was created | CHAR(16) | RSPOCRTIME |
| /SKN/S_SW_01_01_SP01 | RQDELD | Spool request delete date | DATS(8) | RSPODELDAT |
| /SKN/S_SW_01_01_SP01 | RQDELETED | Delete flag for spool requests | CHAR(1) | RSPODELFLG |
| /SKN/S_SW_01_01_SP01 | RQDELRULE | TemSe: Number of delete protection rule | INT2(5) | RSTSDELRUL |
| /SKN/S_SW_01_01_SP01 | RQDELTIME | Spool Request Deleted At | CHAR(16) | RSPODLTIME |
| /SKN/S_SW_01_01_SP01 | RQDEST | Spool: Output device | CHAR(4) | RSPOPNAME |
| /SKN/S_SW_01_01_SP01 | RQDESTL | Spool: Long device names | CHAR(30) | RSPOLNAME |
| /SKN/S_SW_01_01_SP01 | RQDIVISION | Department | CHAR(12) | RSPODIVISI |
| /SKN/S_SW_01_01_SP01 | RQDOCTYPE | Spool: document type | CHAR(6) | RSPODOCTYP |
| /SKN/S_SW_01_01_SP01 | RQERR | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_SP01 | RQERROR | Spool: Error status | CHAR(1) | RSPOERROR |
| /SKN/S_SW_01_01_SP01 | RQFINAL | Spool request completed | CHAR(1) | RSPOFINAL |
| /SKN/S_SW_01_01_SP01 | RQIDENT | Spool request number | INT4(10) | RSPOID |
| /SKN/S_SW_01_01_SP01 | RQISPROTOK | Flag: Whether this request is a log for another request | CHAR(1) | RSPOISPROT |
| /SKN/S_SW_01_01_SP01 | RQMODRULE | TemSe: Number of change protection rule | INT2(5) | RSTSMODRUL |
| /SKN/S_SW_01_01_SP01 | RQMODTIME | Last time a spool request was changed | CHAR(16) | RSPOMODTIM |
| /SKN/S_SW_01_01_SP01 | RQNONE | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_SP01 | RQO1CLIE | Client for which object was generated | CLNT(3) | RSTSCLIENT |
| /SKN/S_SW_01_01_SP01 | RQO1NAME | TemSe object name | CHAR(20) | RSTSONAME |
| /SKN/S_SW_01_01_SP01 | RQO1PART | TemSe: Number of the part of a TemSe object | INT2(5) | RSTSPART |
| /SKN/S_SW_01_01_SP01 | RQOWNER | User name | CHAR(12) | RSPOUSER |
| /SKN/S_SW_01_01_SP01 | RQPAGE | Number of pages in a spool request | INT4(10) | RSPOPAGES |
| /SKN/S_SW_01_01_SP01 | RQPAPER | Spool: Format type | CHAR(16) | RSPOPAPFAM |
| /SKN/S_SW_01_01_SP01 | RQPJDONE | Number of output requests processed | INT2(5) | RSPOPJDONE |
| /SKN/S_SW_01_01_SP01 | RQPJHERR | Spool: Number of output requests with errors (no printout) | INT2(5) | RSPOPJHERR |
| /SKN/S_SW_01_01_SP01 | RQPJREQ | Spool: No. output requests for spool request total | INT2(5) | RSPOPJTOTL |
| /SKN/S_SW_01_01_SP01 | RQPJSERR | Spool: Number of output requests with problems | INT2(5) | RSPOPJSERR |
| /SKN/S_SW_01_01_SP01 | RQPOSNAME | Spool: Long name of printers for host spooler | CHAR(50) | RSPOPRNAME |
| /SKN/S_SW_01_01_SP01 | RQPRIO | Spool: Spool or print request priority | INT1(3) | RSPOPRIO |
| /SKN/S_SW_01_01_SP01 | RQPROC | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_SP01 | RQREARULE | TemSe: Read protection rule number | INT2(5) | RSTSREARUL |
| /SKN/S_SW_01_01_SP01 | RQRECEIVER | Spool: Recipient of spool request | CHAR(12) | RSPORECEIV |
| /SKN/S_SW_01_01_SP01 | RQSAPTITLE | Print SAP cover page | CHAR(1) | RSPOSAPTTL |
| /SKN/S_SW_01_01_SP01 | RQSTATUS_V | Output status | CHAR(7) | RQSTATUS |
| /SKN/S_SW_01_01_SP01 | RQSUCC | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_SP01 | RQTELELAN | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_01_01_SP01 | RQTELENUM | Telecommunications partner | CHAR(30) | SKTELNR |
| /SKN/S_SW_01_01_SP01 | RQTELENUME | Telecommunications partner | CHAR(30) | SKTELNR |
| /SKN/S_SW_01_01_SP01 | RQTITLE | Title of a spool request | CHAR(68) | RSPOTITLE |
| /SKN/S_SW_01_01_SP01 | RQUNXTITLE | Print operating system cover page | CHAR(1) | RSPOUNXTTL |
| /SKN/S_SW_01_01_SP01 | RQWRITER | TemSe: General counter | INT2(5) | RSTSCNT |
| /SKN/S_SW_01_01_SP01 | RQ_ACCESS | Spool request number | INT4(10) | RSPOID |
| /SKN/S_SW_01_01_SP01 | RQ_NO_ACCESS | Spool request number | INT4(10) | RSPOID |
| /SKN/S_SW_01_01_SP01 | SYS | SAP system identification | CHAR(8) | RSPO_SYS |