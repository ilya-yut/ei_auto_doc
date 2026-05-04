# Parameters: SKN_S_SW_01_03_ORA_TBL_SPACES

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | DEST | RFC Destination |  | 0 | 0 |  |  |
| 2 | INIT_EXT | InitialExtent | INT4 | 10 | 0 | INIT_EXT |  |
| 3 | MAX_EXT | MaxExtent: maximum number of extents | INT4 | 10 | 0 | MAX_EXT |  |
| 4 | MIN_EXT | MinExtent | INT4 | 10 | 0 | MIN_EXT |  |
| 5 | NEXT_EXT | NextExtent | INT4 | 10 | 0 | NEXT_EXT |  |
| 6 | PCT_INCR | percentage increase | INT4 | 10 | 0 | PCT_INCR |  |
| 7 | STATUS | Status of tablespace: ONLINE / OFFLINE | CHAR | 9 | 0 | STATUS_ | CHAR9 |
| 8 | TEXTENTS | Total number of extents in a tablespace | INT4 | 10 | 0 | TEXTENTS |  |
| 9 | TFREE | Freespace | DEC | 15 | 0 | TFREE1 | DEC15 |
| 10 | TPCTUSED | Used percentage of tablespace memory | INT4 | 10 | 0 | TPCTUSED |  |
| 11 | TS | Name of tablespace | CHAR | 30 | 0 | TS | CHAR30 |
| 12 | TSAU | Total maximum autoextensible size (Kbyte) | CHAR | 14 | 0 | DB_TSAU | CHAR14 |
| 13 | TSAUPCT | % of maximum autoextensible size used | INT4 | 10 | 0 | DB_TSAUPCT |  |
| 14 | TSEGMENTS | Number of segments | INT4 | 10 | 0 | TSEGMENTS |  |
| 15 | TSIZE | Size of Tablespace | DEC | 15 | 0 | TSIZE1 | DEC15 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 15 parameters listed in the Parameters Reference Table above.

**DEST** (RFC Destination):

RFC destination used for the Oracle connection. When set, the EI reads tablespace data from the specified system.

**INIT_EXT** (InitialExtent):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**MAX_EXT** (MaxExtent: maximum number of extents):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**MIN_EXT** (MinExtent):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**NEXT_EXT** (NextExtent):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**PCT_INCR** (percentage increase):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**STATUS** (Status of tablespace: ONLINE / OFFLINE):

Filters by Oracle tablespace status. Used to include only online, offline, or other statuses.

**STATUS Options:**
- **ONLINE** — Tablespace is online and available for use.
- **OFFLINE** — Tablespace is offline and not available.

**TEXTENTS** (Total number of extents in a tablespace):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**TFREE** (Freespace):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**TPCTUSED** (Used percentage of tablespace memory):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**TS** (Name of tablespace):

Identifies the Oracle tablespace by name. Used to restrict results to specific tablespaces.

**TS Options:**
- **SYSTEM** — System tablespace (core dictionary and PL/SQL).
- **SYSAUX** — Auxiliary system tablespace (e.g. OEM, stats).
- **USERS** — Default user data tablespace.
- **TEMP** — Temporary tablespace for sorts and temporary segments.

**TSAU** (Total maximum autoextensible size (Kbyte)):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**TSAUPCT** (% of maximum autoextensible size used):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**TSEGMENTS** (Number of segments):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.

**TSIZE** (Size of Tablespace):

Numeric or range filter on this tablespace metric. Restricts which tablespaces are returned based on this value.



### Parameter Relationships

**Filter parameters (TS, STATUS, TSIZE, TFREE, TPCTUSED, TEXTENTS, TSEGMENTS, TSAU, TSAUPCT):**

- These parameters are applied together after tablespace data is retrieved. Each acts as a selection criterion: only rows that fall within the specified ranges or values for each parameter are returned.
- **TS** and **STATUS** narrow by tablespace name and online/offline state; **TSIZE**, **TFREE**, **TPCTUSED**, **TEXTENTS**, **TSEGMENTS**, **TSAU**, and **TSAUPCT** narrow by size and usage metrics.
- **DEST** is used earlier in the flow to select the target system (RFC destination) from which tablespace data is read; it does not combine with the filter parameters in the same way.


### Default Values

No default values are defined for this EI.

### Practical Configuration Examples

**Use Case 1: Monitor system and auxiliary tablespaces only**
```
TS = SYSTEM
TS = SYSAUX
STATUS = ONLINE
```
**Purpose:** Focus on core system tablespaces that are online to check size and usage without including user or temporary tablespaces.

**Use Case 2: High usage and low free space**
```
TPCTUSED = 80 - 100
TFREE = 0 - 1000
STATUS = ONLINE
```
**Purpose:** Identify online tablespaces that are heavily used and have little free space (e.g. under 1000 MB free) for capacity planning.

**Use Case 3: Tablespace size and extent configuration**
```
TS = USERS
TSIZE = 1000 - 999999
TEXTENTS = 1 - 100
TSEGMENTS = 1 - 500
```
**Purpose:** Review user tablespaces within a size range and by extent and segment counts for tuning and growth analysis.

**Use Case 4: Autoextensible usage**
```
TSAU = 1000 - 9999999999
TSAUPCT = 70 - 100
DEST = ORA_SID
STATUS = ONLINE
```
**Purpose:** Find online tablespaces where autoextensible space is configured and largely used, with a specific RFC destination.

**Use Case 5: Full filter set for a single tablespace**
```
TS = SYSAUX
STATUS = ONLINE
TSIZE = 500 - 50000
TFREE = 0 - 5000
TPCTUSED = 50 - 100
TEXTENTS = 1 - 1000
TSEGMENTS = 1 - 200
TSAU = 0 - 999999999
TSAUPCT = 0 - 100
DEST = ORA_SID
```
**Purpose:** Detailed monitoring of SYSAUX on a target system with constraints on size, free space, usage percentage, extents, segments, and autoextend usage.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_ORA_TBL_SPACES | INIT_EXT | InitialExtent: Initial NextExtent size | INT4(10) | INIT_EXT |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | MAX_EXT | MaxExtent: maximum number of extents | INT4(10) | MAX_EXT |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | MIN_EXT | MinExtent: Minimum of NextExtent size | INT4(10) | MIN_EXT |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | NEXT_EXT | NextExtent: Size of next extent | INT4(10) | NEXT_EXT |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | PCT_INCR | percentage increase | INT4(10) | PCT_INCR |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | STATUS | Status of tablespace: ONLINE / OFFLINE | CHAR(9) | STATUS_ |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | TEXTENTS | Total number of extents in a tablespace | INT4(10) | TEXTENTS |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | TFREE | Freespace | DEC(15) | TFREE1 |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | TPCTUSED | Used percentage of tablespace memory | INT4(10) | TPCTUSED |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | TS | Name of tablespace | CHAR(30) | TS |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | TSAU | Total maximum autoextensible size (Kbyte) | CHAR(14) | DB_TSAU |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | TSAUPCT | % of maximum autoextensible size used | INT4(10) | DB_TSAUPCT |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | TSEGMENTS | Number of segments | INT4(10) | TSEGMENTS |
| /SKN/S_SW_01_03_ORA_TBL_SPACES | TSIZE | Size of Tablespace | DEC(15) | TSIZE1 |
