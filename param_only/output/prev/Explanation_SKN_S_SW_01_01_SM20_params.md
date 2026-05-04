# Parameters: SKN_S_SW_01_01_SM20

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACTUALVAL | Alert value | INT4 | 10 | 0 | ALVALUE |  |
| 2 | ALIDXINTRE | Index of MT in Tree | INT4 | 10 | 0 | ALIDXINTRE |  |
| 3 | ALLEVINTRE | Level of MTE in Tree | INT4 | 10 | 0 | ALLEVINTRE |  |
| 4 | ARGTYPE1 | Argument type | CHAR | 1 | 0 | XMIARGTYP | XMIARGTYPE |
| 5 | ARGTYPE2 | Argument type | CHAR | 1 | 0 | XMIARGTYP | XMIARGTYPE |
| 6 | ARGTYPE3 | Argument type | CHAR | 1 | 0 | XMIARGTYP | XMIARGTYPE |
| 7 | ARGTYPE4 | Argument type | CHAR | 1 | 0 | XMIARGTYP | XMIARGTYPE |
| 8 | BACKDAYS | Days backward |  | 0 | 0 |  |  |
| 9 | CUSGRPNAME | Attribute Group | CHAR | 40 | 0 | ALCUSGROUP | TEXT40 |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | INSTANCENAME | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 13 | LANGU | Text laguage |  | 0 | 0 |  |  |
| 14 | MANDT | Client ID | CLNT | 3 | 0 | SYMANDT | MANDT |
| 15 | MSCDATE | Date | DATS | 8 | 0 | ALDATE | DATUM |
| 16 | MSCGLLID | MsgCont. line ID | CHAR | 50 | 0 | ALMSCGLLID | CHAR50 |
| 17 | MSCTIME | Time | TIMS | 6 | 0 | ALTIME | TIME |
| 18 | MSG | Message | CHAR | 255 | 0 | XMIEXTMSG | CHAR255 |
| 19 | MSGARG1 | XMI log argument | CHAR | 128 | 0 | XMIARGTXT | CHAR128 |
| 20 | MSGARG2 | XMI log argument | CHAR | 128 | 0 | XMIARGTXT | CHAR128 |
| 21 | MSGARG3 | XMI log argument | CHAR | 128 | 0 | XMIARGTXT | CHAR128 |
| 22 | MSGARG4 | XMI log argument | CHAR | 128 | 0 | XMIARGTXT | CHAR128 |
| 23 | MSGCLASS | Company | CHAR | 16 | 0 | XMILOGCOMP | TEXT16 |
| 24 | MSGID | Message ID | CHAR | 30 | 0 | XMILOGMID | XMSGID |
| 25 | MSGTEXT | Message text | CHAR | 128 | 0 | XMIMSGTXT | CHAR128 |
| 26 | MTNAMESHRT | Short Name of Monitoring Type | CHAR | 40 | 0 | ALMTNAMESH | CHAR40 |
| 27 | SEVERFLTRD | severity | INT4 | 10 | 0 | ALSEVERITY |  |
| 28 | SEVERORIG | severity | INT4 | 10 | 0 | ALSEVERITY |  |
| 29 | SEVE_LEVEL | Severity Level (L/M/H) |  | 0 | 0 |  |  |
| 30 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 31 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 32 | USERID | User Name | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |
| 33 | VALUEFLTRD | Alert value | INT4 | 10 | 0 | ALVALUE |  |
| 34 | VALUEORIG | Alert value | INT4 | 10 | 0 | ALVALUE |  |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 34 parameters listed in the Parameters Reference Table above.

**ACTUALVAL** (Alert value):

Numeric alert value used by the Security Audit Log to match message line entries; the EI compares this value to the message container when evaluating which entries to process. It corresponds to the internal severity scale (e.g. 1 = green, 2 = yellow, 3 = red) used in the monitoring tree.

**ALIDXINTRE** (Index of MT in Tree):

Position index of the monitoring type element within the Security Audit Log tree. Used together with MTNAMESHRT and ALLEVINTRE to target which tree nodes (alert types) are read from the XMI interface when retrieving security audit messages.

**ALLEVINTRE** (Level of MTE in Tree):

Hierarchy level of the monitoring tree element. The EI skips the first level (header) and processes nodes at level greater than 1; this parameter identifies which tree levels are considered when iterating over MTEs for security audit data.

**ARGTYPE1 - ARGTYPE4** (Argument type – Argument type):

Argument type classifiers for the first four placeholders of an XMI log message. Each specifies how the corresponding message argument (MSGARG1–MSGARG4) is interpreted by the logging interface.

**ARGTYPE1 - ARGTYPE4 Options:**

- Values are function-specific; see code or output structure.

**BACKDAYS** (Days backward):

Number of calendar days to look back from the current date when no explicit date range is supplied. The EI uses this to compute the start of the monitoring window (e.g. today minus BACKDAYS to today) for reading Security Audit Log entries from the XMI session.

**CUSGRPNAME** (Attribute Group):

Name of the customization or attribute group associated with monitoring types. Used in the Security Audit Log context to scope or label customization for alert evaluation.

**DURATION** (Duration In Time Units):

Length of the time span expressed in the unit given by DURATION_UNIT. The EI computes the time difference between each log entry’s timestamp (MSCDATE/MSCTIME) and the current system date/time, then compares that difference to this parameter to determine whether the entry falls within the chosen duration window.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION and the computed time difference are interpreted (hours, minutes, days, or full days for day-level filtering).

**DURATION_UNIT Options:**

- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**INSTANCENAME** (Server Name):

Application server name (host) from which Security Audit Log entries are read. The EI retrieves the server list and uses this parameter to determine which instance(s) to read when calling the XMI interface.

**LANGU** (Text laguage):

Language used for text elements (e.g. message texts) when the EI or the XMI interface returns language-dependent content.

**MANDT** (Client ID):

SAP client identifier. The EI uses this when reading and evaluating Security Audit Log entries so that processing is client-specific.

**MSCDATE** (Date):

Date of the security audit log entry (message container date). The EI uses this when building the monitoring window and when evaluating which log entries fall within the requested period.

**MSCGLLID** (MsgCont. line ID):

Global line identifier for a message container entry. Used to uniquely identify and optionally filter individual log lines in the Security Audit Log output.

**MSCTIME** (Time):

Time of the security audit log entry. Used together with MSCDATE to form the timestamp of the event when building the duration comparison and when evaluating the log entry.

**MSG** (Message):

Expanded or translated message text for the log entry as provided to external tools. The EI uses this when evaluating which log entries match the supplied message value(s).

**MSGARG1 - MSGARG4** (XMI log argument – XMI log argument):

First through fourth argument strings for an XMI log entry. Each holds a placeholder value that, together with ARGTYPE1–ARGTYPE4, completes the message text and identifies specific log events.

**MSGCLASS** (Company):

Company name of the external management tool in the XMI logging context. Used to categorize log entries by the originating company/product context.

**MSGID** (Message ID):

Message ID of the log entry. Identifies the message template used for the security audit event.

**MSGTEXT** (Message text):

Actual text of the message. Used when evaluating Security Audit Log entries by the displayed message content.

**MTNAMESHRT** (Short Name of Monitoring Type):

Short name of the monitoring type in the Security Audit Log tree. The EI uses this when iterating over the MTE tree to determine which monitoring types (e.g. security-related nodes) are read from the XMI interface.

**SEVERFLTRD** (severity):

Severity of the log entry after applying monitoring-type customization or filtering. Used to align displayed or filtered severity with the customized alert evaluation.

**SEVERORIG** (severity):

Original severity of the log entry from the source. The EI maps this to STATE_COLOR (R/Y/G) for display and uses it together with SEVE_LEVEL when evaluating message line values against the severity range.

**SEVE_LEVEL** (Severity Level (L/M/H)):

Severity level for security audit events. When not supplied, the EI defaults to high severity. The code maps L/M/H to an internal numeric scale (1/2/3) and uses it when evaluating which message line values are read from the XMI interface.

**SEVE_LEVEL Options:**

- **L** — Low severity; routine or informational security events (value 1).
- **M** — Medium severity; events requiring review (value 2).
- **H** — High severity security events requiring immediate attention (value 3).

**STATE_COLOR** (State Color):

Display color representing the state of the security audit entry. The EI derives it from the original severity (SEVERORIG) and uses it when evaluating entries and when resolving the state icon for presentation.

**STATE_COLOR Options:**

- **R** — Red; critical security events requiring immediate attention.
- **Y** — Yellow; medium severity events requiring review.
- **G** — Green; low severity or informational events.

**STATE_ICON** (State Icon):

Icon code corresponding to the state (and STATE_COLOR) of the log entry. The EI resolves it from STATE_COLOR via the state icon function for display in the result set.

**USERID** (User Name):

User name associated with the security audit log entry. The EI uses this when evaluating which log entries correspond to the specified user(s).

**VALUEFLTRD** (Alert value):

Alert value after applying filtering or customization. Used to align the result set with the customized alert evaluation.

**VALUEORIG** (Alert value):

Original alert value from the message line. The EI uses it to match entries against the SEVE_LEVEL-derived range (R_ACTUALVAL) when deciding which entries are processed and returned.


### Parameter Relationships

**Time and Lookback Parameters:**

- **BACKDAYS** defines how many days to look back from the current date when no explicit date range is supplied. The EI builds the monitoring window (from date to current date) using this value so that only Security Audit Log entries within that window are read from the XMI interface.

**Duration Parameters:**

- **DURATION** and **DURATION_UNIT** work together. DURATION is the length of the time span; DURATION_UNIT specifies whether that length is in hours (H), minutes (M), days (D), or full days (F). The EI computes the time difference between each log entry’s timestamp and the current system date/time in the chosen unit, then compares it to DURATION to decide which entries are included in the result.

**Monitoring Tree and Navigation Parameters:**

- **MTNAMESHRT**, **ALIDXINTRE**, and **ALLEVINTRE** work together to target which nodes of the Security Audit Log tree are processed. MTNAMESHRT restricts by short name of the monitoring type; ALIDXINTRE and ALLEVINTRE restrict by index and level in the tree. The EI iterates over tree nodes (skipping level 1) and uses these parameters to filter which MTEs are used when calling the XMI interface to retrieve message history.


### Default Values

- **DURATION_UNIT** — Default: `M` (minutes; used for duration and time-difference calculations when not supplied).
- **BACKDAYS** — Default: `0` (when no date range is supplied, the monitoring window ends at the current date with zero lookback days).
- **AUDIT_LEVEL** — Default: `1` (standard audit level for the XMI session when not supplied).
- **SEVE_LEVEL** — Default: high (when not supplied, the EI appends high severity to the selection so only high-severity security events are considered).

### Practical Configuration Examples

**Use Case 1: High-severity security events on a specific server (last 7 days)**
```
BACKDAYS = 7
SEVE_LEVEL = H
INSTANCENAME = hostname_AS00
```
**Purpose:** Focus on high-severity Security Audit Log entries from one application server over the past week for immediate security review.

**Use Case 2: Duration-based filtering in minutes with state color**
```
DURATION = 60
DURATION_UNIT = M
STATE_COLOR = R
USERID = ADMIN
```
**Purpose:** View critical (red) security events for a specific user where the event occurred within the last 60 minutes, for quick incident triage.

**Use Case 3: Full-day window and severity level**
```
DURATION = 30
DURATION_UNIT = F
SEVE_LEVEL = M
MANDT = 100
INSTANCENAME = prod_AS01
```
**Purpose:** Monitor medium-severity security events in client 100 on a given server over a 30 full-day window for periodic compliance review.

**Use Case 4: Monitoring tree and date range**
```
MTNAMESHRT = Security
ALIDXINTRE = 1 - 20
MSCDATE = 20250101 - 20250131
STATE_COLOR = R
STATE_COLOR = Y
```
**Purpose:** Review security-related monitoring types (by tree index) for red and yellow events in a specific calendar month.

**Use Case 5: Multi-parameter audit review**
```
BACKDAYS = 14
DURATION_UNIT = D
DURATION = 7
SEVE_LEVEL = H
SEVE_LEVEL = M
INSTANCENAME = hostname_AS00
MANDT = 200
USERID = AUDIT_USER
```
**Purpose:** Audit configuration for high and medium severity events from a specific client, server, and user over a two-week lookback with duration expressed in days.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_SM20 | ACTUALVAL | Alert: alert value (1 = green, 2 = yellow, ....) | INT4(10) | ALVALUE |
| /SKN/S_SW_01_01_SM20 | ALIDXINTRE | Alert: Tree Info: Index of MT in Tree | INT4(10) | ALIDXINTRE |
| /SKN/S_SW_01_01_SM20 | ALLEVINTRE | Alert: Tree Info: Level of MTE in Tree | INT4(10) | ALLEVINTRE |
| /SKN/S_SW_01_01_SM20 | ARGTYPE1 | Argument type of an XMI log entry | CHAR(1) | XMIARGTYP |
| /SKN/S_SW_01_01_SM20 | ARGTYPE2 | Argument type of an XMI log entry | CHAR(1) | XMIARGTYP |
| /SKN/S_SW_01_01_SM20 | ARGTYPE3 | Argument type of an XMI log entry | CHAR(1) | XMIARGTYP |
| /SKN/S_SW_01_01_SM20 | ARGTYPE4 | Argument type of an XMI log entry | CHAR(1) | XMIARGTYP |
| /SKN/S_SW_01_01_SM20 | CUSGRPNAME | Alert: Customization: Name of Customization Group | CHAR(40) | ALCUSGROUP |
| /SKN/S_SW_01_01_SM20 | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_SM20 | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_SM20 | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_01_SM20 | MANDT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_01_SM20 | MSCDATE | Alert: date | DATS(8) | ALDATE |
| /SKN/S_SW_01_01_SM20 | MSCGLLID | Alert: Message container: global external line ID | CHAR(50) | ALMSCGLLID |
| /SKN/S_SW_01_01_SM20 | MSCTIME | Alert: Time value in timeformat | TIMS(6) | ALTIME |
| /SKN/S_SW_01_01_SM20 | MSG | The expanded (translated) message for ext. tools | CHAR(255) | XMIEXTMSG |
| /SKN/S_SW_01_01_SM20 | MSGARG1 | Argument string for an XMI log | CHAR(128) | XMIARGTXT |
| /SKN/S_SW_01_01_SM20 | MSGARG2 | Argument string for an XMI log | CHAR(128) | XMIARGTXT |
| /SKN/S_SW_01_01_SM20 | MSGARG3 | Argument string for an XMI log | CHAR(128) | XMIARGTXT |
| /SKN/S_SW_01_01_SM20 | MSGARG4 | Argument string for an XMI log | CHAR(128) | XMIARGTXT |
| /SKN/S_SW_01_01_SM20 | MSGCLASS | XMI logging: company name of external management tool | CHAR(16) | XMILOGCOMP |
| /SKN/S_SW_01_01_SM20 | MSGID | Message ID for an XMI log entry | CHAR(30) | XMILOGMID |
| /SKN/S_SW_01_01_SM20 | MSGTEXT | Actual text of message | CHAR(128) | XMIMSGTXT |
| /SKN/S_SW_01_01_SM20 | MTNAMESHRT | Alert: Short Name of Monitoring Type | CHAR(40) | ALMTNAMESH |
| /SKN/S_SW_01_01_SM20 | SEVERFLTRD | Alert: severity (alerts, monitoring type custom..) | INT4(10) | ALSEVERITY |
| /SKN/S_SW_01_01_SM20 | SEVERORIG | Alert: severity (alerts, monitoring type custom..) | INT4(10) | ALSEVERITY |
| /SKN/S_SW_01_01_SM20 | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_SM20 | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_SM20 | USERID | User Name | CHAR(12) | SYUNAME |
| /SKN/S_SW_01_01_SM20 | VALUEFLTRD | Alert: alert value (1 = green, 2 = yellow, ....) | INT4(10) | ALVALUE |
| /SKN/S_SW_01_01_SM20 | VALUEORIG | Alert: alert value (1 = green, 2 = yellow, ....) | INT4(10) | ALVALUE |
