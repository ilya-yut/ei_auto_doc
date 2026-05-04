# Parameters: SKN_S_SW_01_01_SM20_CNT

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days backward |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 3 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 4 | INSTANCENAME | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 5 | LANGU | Text laguage |  | 0 | 0 |  |  |
| 6 | MANDT | Client ID | CLNT | 3 | 0 | SYMANDT | MANDT |
| 7 | MSCDATE | Date | DATS | 8 | 0 | ALDATE | DATUM |
| 8 | MSCTIME | Time | TIMS | 6 | 0 | ALTIME | TIME |
| 9 | MSG | Message | CHAR | 255 | 0 | XMIEXTMSG | CHAR255 |
| 10 | REQ_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 11 | SEVERFLTRD | severity | INT4 | 10 | 0 | ALSEVERITY |  |
| 12 | SEVE_LEVEL | Severity Level (L/M/H) |  | 0 | 0 |  |  |
| 13 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 14 | USERID | User Name | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |
| 15 | VALUEORIG | Single-Character Indicator | CHAR | 1 | 0 | CHAR1 | CHAR1 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 15 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Days backward):

Number of days to look back from the reference date. Defines the time window for reading the security audit log when the EI calls the underlying SM20 function.

**DURATION** (Duration In Time Units):

Length of the time window in the unit given by DURATION_UNIT. Defines the span for security audit log analysis passed to the underlying function.

**DURATION_UNIT** (Duration Unit):

Time unit for DURATION. Must be used together with DURATION to define the analysis window.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**INSTANCENAME** (Server Name):

Application server name. Restricts the result to log entries from a specific instance; the EI aggregates by instance and returns count per instance.

**LANGU** (Text laguage):

Language for descriptions and message text. Used when the underlying logic returns or displays language-dependent content.

**MANDT** (Client ID):

SAP client. Scopes the security audit log analysis to a specific client.

**MSCDATE** (Date):

Date of the log entry. Used to filter or aggregate entries by date.

**MSCTIME** (Time):

Time of the log entry. Used together with MSCDATE to filter or order by timestamp.

**MSG** (Message):

Expanded (translated) message text for the log entry. The EI uses it to filter or aggregate by message content when calling the underlying function.

**REQ_CNT** (Count):

Count of matching log entries per aggregation key (e.g. per instance). The EI compares this value to the selection range so that only results within the configured count range are returned and the alert is set accordingly.

**SEVERFLTRD** (severity):

Severity of the entry after customization or filtering. The EI uses it to compare against severity criteria when aggregating and alerting.

**SEVE_LEVEL** (Severity Level (L/M/H)):

Severity level for security audit log entries. The EI uses it to scope the analysis to a severity band when calling the underlying function.

**SEVE_LEVEL Options:**
- **H** — High severity security events requiring immediate attention (value 3).
- **M** — Medium severity security events for standard monitoring (value 2).
- **L** — Low severity security events for comprehensive logging (value 1).

**STATE_COLOR** (State Color):

State color of the alert (e.g. green, yellow, red). The EI uses it to filter or aggregate by alert state.

**STATE_COLOR Options:**
- **R** — Red indicating critical security events requiring immediate attention.
- **Y** — Yellow indicating warning-level security events for standard monitoring.
- **G** — Green indicating informational security events for audit trails.
- **B** — Blue indicating system-level security events for technical analysis.

**USERID** (User Name):

User name associated with the log entry. Restricts the result to a specific user or user pattern when calling the underlying function.

**VALUEORIG** (Single-Character Indicator):

Single-character indicator from the underlying log entry. The EI uses it to filter or aggregate; exact values are function-specific (e.g. state or type codes).


### Parameter Relationships

**Time window:**

**BACKDAYS**, **DURATION**, and **DURATION_UNIT** define the time span for security audit log analysis. BACKDAYS is a lookback in days; DURATION and DURATION_UNIT define an alternative window (e.g. last 24 hours, last 7 days). Use one approach per scenario so the underlying EI reads the correct time range.

**Severity and state:**

**SEVE_LEVEL**, **SEVERFLTRD**, **STATE_COLOR**, and **VALUEORIG** describe severity and alert state. The EI uses them to filter and aggregate when calling the underlying function. Use severity and state parameters together for consistent count-based alerting.

**Context and count:**

**INSTANCENAME**, **MANDT**, and **USERID** scope the analysis to server, client, and user. **REQ_CNT** is the count range that determines which aggregated results are returned and whether the alert is set. Configure REQ_CNT together with selection parameters (e.g. SEVE_LEVEL, INSTANCENAME) to monitor counts per instance or per severity band.


### Default Values

No default values are defined for this EI; all parameters are used as supplied or as initial when not supplied.

### Practical Configuration Examples

**Use Case 1: Count by instance and client**
```
MANDT = 100
INSTANCENAME = *
REQ_CNT = 1 - 999999
```
**Purpose:** Count security audit log entries per application server in client 100. Use to see volume per instance.

**Use Case 2: High severity over last 7 days**
```
BACKDAYS = 7
SEVE_LEVEL = H
REQ_CNT = 1 - 999999
```
**Purpose:** Count high-severity entries over the last seven days. Use for high-severity volume monitoring.

**Use Case 3: Time window and count threshold**
```
DURATION = 24
DURATION_UNIT = H
REQ_CNT = 10 - 999999
USERID = *
MANDT = 200
```
**Purpose:** Last 24 hours, client 200, any user; only return aggregates with count at least 10. Use for threshold-based alerting.

**Use Case 4: State and severity**
```
STATE_COLOR = R
SEVE_LEVEL = H
REQ_CNT = 1 - 100
INSTANCENAME = appserver01
```
**Purpose:** Red-state, high-severity entries on a specific server with count between 1 and 100. Use for critical-state count monitoring.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_SM20_CNT | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_01_SM20_CNT | REQ_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |
| /SKN/S_SW_01_01_SM20_CNT | VALUEORIG | Single-Character Indicator | CHAR(1) | CHAR1 |
