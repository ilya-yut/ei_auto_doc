# Parameters: SKN_S_SW_01_01_SM12_CNT

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | DEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 2 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 3 | DURATION_UNIT | Duration Unit(D/H/M) |  | 0 | 0 |  |  |
| 4 | GARG | Lock Argument | CHAR | 150 | 0 | EQEGRAARG | EQDARG |
| 5 | GCLIENT | Client | CHAR | 3 | 0 | EQECLIENT | CHAR3 |
| 6 | GMODE | Lock mode | CHAR | 1 | 0 | EQEGRAMODE | CHAR1 |
| 7 | GNAME | Table name | CHAR | 30 | 0 | EQEGRANAME | CHAR30 |
| 8 | GTARG | Lock argument | CHAR | 50 | 0 | EQEGTARG | TEXT50 |
| 9 | GTCODE | Transaction Code | CHAR | 20 | 0 | EQETCODE | TCODE |
| 10 | GTHOST | Host name | CHAR | 32 | 0 | EQEHOST | TEXT32 |
| 11 | GUNAME | User name | CHAR | 12 | 0 | EQEUNAME | CHAR12 |
| 12 | GUSR | Lock Owner | CHAR | 58 | 0 | EQEUSR | EQDUSR |
| 13 | LANGU | Description Lanfuage |  | 0 | 0 |  |  |
| 14 | LOCKS_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 15 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 16 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 16 parameters listed in the Parameters Reference Table above.

**DEST** (RFC Destination):

Logical destination for the RFC call. When the EI runs on a remote system or cloud, this identifies the target system. Used by the wrapper to route the call to the correct application server or cloud destination.

**DURATION** (Duration In Time Units):

Length of the time window for lock analysis, expressed in the unit given by DURATION_UNIT. Defines how far back (or the span) the EI considers application locks.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Time unit for DURATION (e.g. days, hours, minutes). Must be used together with DURATION to define the analysis window.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**GARG** (Lock Argument):

Lock argument value for the enqueue. The EI uses this to restrict which locks are considered (e.g. by argument pattern).

**GCLIENT** (Client):

SAP client. Scopes lock analysis to a specific client so that cross-client locks are excluded when not relevant.

**GMODE** (Lock mode):

Enqueue lock mode (e.g. shared, exclusive). The EI uses this to filter locks by the mode with which they were set.

**GNAME** (Table name):

Table name associated with the lock. Restricts the result set to locks on a specific table or table pattern.

**GTARG** (Lock argument):

Additional lock argument. Used together with other lock parameters to narrow the selection to specific lock entries.

**GTCODE** (Transaction Code):

Transaction code that holds the lock. Restricts the result to locks held by a given transaction or transaction pattern.

**GTHOST** (Host name):

Application server host name. Restricts the result to locks on a specific host.

**GUNAME** (User name):

User who owns or holds the lock. Restricts the result to locks for a specific user or user pattern.

**GUSR** (Lock Owner):

Lock owner identifier. The EI uses this to filter by the owner of the enqueue lock.

**LANGU** (Description Lanfuage):

Language for descriptions. Used when the underlying logic returns or displays language-dependent text.

**LOCKS_CNT** (Count):

Number of locks (count) returned by the underlying logic. The EI compares this value to the selection range so that only results within the configured count range are returned and the alert flag is set accordingly.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set, time-related evaluation is done in UTC. Affects how the EI interprets time windows for lock analysis.

**SW_DEST** (Cloud Destination):

Cloud or remote destination. When set, the EI may route execution to a cloud or remote system; when initial, execution is on-premise.


### Parameter Relationships

**Time window:**

**DURATION** and **DURATION_UNIT** work together to define the time span for lock analysis. DURATION gives the numeric value; DURATION_UNIT specifies whether it is in days, hours, or minutes. Set both when you need a time-bounded analysis.

**Lock identification:**

**GCLIENT**, **GNAME**, **GUNAME**, **GMODE**, **GARG**, **GTARG**, **GTCODE**, **GTHOST**, and **GUSR** together describe the enqueue lock attributes. They are passed as selection criteria to the underlying function. Use a subset that matches your monitoring scenario (e.g. user and table, or transaction and host).

**Destination:**

**DEST** and **SW_DEST** relate to where the logic runs: DEST is the RFC destination; SW_DEST indicates cloud or remote execution. When SW_DEST is set, the wrapper may call the remote function; when initial, it runs on-premise.


### Default Values

No default values are defined for this EI; all parameters are used as supplied or as initial when not supplied.

### Practical Configuration Examples

**Use Case 1: Lock count by user and client**
```
GCLIENT = 100
GUNAME = *
LOCKS_CNT = 1 - 999999
```
**Purpose:** Monitor lock count per user in client 100. Use to see how many locks each user holds and to spot high lock counts.

**Use Case 2: Transaction and table scope**
```
GTCODE = SM30
GNAME = *
LOCKS_CNT = 10 - 999999
DEST =
```
**Purpose:** Find locks held by transaction SM30 on any table, with count at least 10. Useful for table maintenance lock analysis.

**Use Case 3: Host and lock mode**
```
GTHOST = appserver01
GMODE = E
LOCKS_CNT = 1 - 100
```
**Purpose:** Restrict to exclusive locks on a specific host with count between 1 and 100. Use for host-level lock monitoring.

**Use Case 4: Time window and user**
```
GUNAME = SAP*
GCLIENT = 200
DURATION = 24
DURATION_UNIT = H
LOCKS_CNT = 0 - 50
```
**Purpose:** Analyze locks for users starting with SAP in client 200 over the last 24 hours, with count up to 50. Combines time window and count for operational review.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_SM12_CNT | DEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_01_SM12_CNT | LOCKS_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |
