# Parameters: SKN_S_SW_01_03_MSS_DB_SIZE

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | CON_NAME | Logical name of a Database Con |  | 0 | 0 |  |  |
| 2 | DBFREE | DB Freespace (MB) | INT4 | 10 | 0 | MSS_DBFREESIZE |  |
| 3 | DBFREE_PER | DB Free in Per. | INT1 | 3 | 0 | /SKN/E_SW_DBFREE_PER |  |
| 4 | DB_ALLOC | Allocated DB  space (MB) | DEC | 15 | 0 | MSS_DBALLOC |  |
| 5 | DB_NAME | Database name |  | 0 | 0 |  |  |
| 6 | DB_SIZE | Database size (MB) | DEC | 15 | 0 | MSS_DBSIZE |  |
| 7 | DEST | RFC Destination |  | 0 | 0 |  |  |
| 8 | LOGFREE | Free log space in MB | INT4 | 10 | 0 | MSS_LOGFREESIZE |  |
| 9 | LOG_ALLOC | Allocated log  space (MB) | DEC | 15 | 0 | MSS_LOGALLOC |  |
| 10 | LOG_SIZE | Log size (MB) | DEC | 15 | 0 | MSS_LOGSIZE |  |
| 11 | SCHEMA | Target Schema |  | 0 | 0 |  |  |
| 12 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 12 parameters listed in the Parameters Reference Table above.

**CON_NAME** (Logical name of a Database Con):

Logical name of the database connection used when calling the remote MSS_GET_DB_SIZE_INFO function. The EI passes this to the RFC call to identify which database connection to use. When not supplied, the code uses the default value 'DEFAULT'.

**DBFREE** (DB Freespace (MB)):

Free space in the database in MB. Used as a multiselect filter on the result set: only rows where DBFREE matches the configured range or values are returned. The EI deletes from the result table rows where DBFREE is not in the selection.

**DBFREE_PER** (DB Free in Per.):

Free database space as a percentage. Used as a multiselect filter on the result set: only rows where DBFREE_PER matches the configured range or values are returned. The EI deletes from the result table rows where DBFREE_PER is not in the selection.

**DB_ALLOC** (Allocated DB space (MB)):

Allocated database space in MB. Used as a multiselect filter on the result set: only rows where DB_ALLOC matches the configured range or values are returned. The EI deletes from the result table rows where DB_ALLOC is not in the selection.

**DB_NAME** (Database name):

Database name passed to the remote MSS_GET_DB_SIZE_INFO call. Scopes the EI to the specified database when the RFC destination returns size info for multiple databases.

**DB_SIZE** (Database size (MB)):

Database size in MB. Used as a multiselect filter on the result set: only rows where DB_SIZE matches the configured range or values are returned. The EI deletes from the result table rows where DB_SIZE is not in the selection.

**DEST** (RFC Destination):

RFC destination used to call the remote system where MSS_GET_DB_SIZE_INFO runs. When SW_DEST (cloud destination) is supplied, it overrides DEST for the actual call.

**LOGFREE** (Free log space in MB):

Free log space in MB. Used as a multiselect filter on the result set: only rows where LOGFREE matches the configured range or values are returned. The EI deletes from the result table rows where LOGFREE is not in the selection.

**LOG_ALLOC** (Allocated log space (MB)):

Allocated log space in MB. Used as a multiselect filter on the result set: only rows where LOG_ALLOC matches the configured range or values are returned. The EI deletes from the result table rows where LOG_ALLOC is not in the selection.

**LOG_SIZE** (Log size (MB)):

Log size in MB. Used as a multiselect filter on the result set: only rows where LOG_SIZE matches the configured range or values are returned. The EI deletes from the result table rows where LOG_SIZE is not in the selection.

**SCHEMA** (Target Schema):

Target schema passed to the remote MSS_GET_DB_SIZE_INFO call. Scopes the EI to the specified schema when retrieving database size information.

**SW_DEST** (Cloud Destination):

Cloud RFC destination. When supplied, the EI uses this instead of DEST for the call to MSS_GET_DB_SIZE_INFO. Use this to target a cloud or alternate system for size retrieval.


### Parameter Relationships

**Connection/RFC:**

- **DEST**, **CON_NAME**, **SCHEMA**, and **DB_NAME** define how the EI connects and what target is queried. **DEST** (or **SW_DEST** when set) is the RFC destination for the call to MSS_GET_DB_SIZE_INFO. **CON_NAME** defaults to 'DEFAULT' when not supplied and is the logical database connection name. **SCHEMA** and **DB_NAME** scope the call to a specific schema and database. Set these together to target the correct MSSQL instance and object.

**Result filters (multiselect):**

- **DB_SIZE**, **DB_ALLOC**, **LOG_SIZE**, **LOG_ALLOC**, **DBFREE**, **LOGFREE**, and **DBFREE_PER** are applied as multiselect filters on the result set after the remote function returns. The EI deletes result rows where each of these fields is not in the corresponding selection. Use them to restrict output to databases or logs meeting size or free-space criteria.


### Default Values

- **CON_NAME** — Default: `DEFAULT` when not supplied.
- **LANGU** — System language (SY-LANGU) when not supplied.

### Practical Configuration Examples

**Use Case 1: Default connection, filter by database size and free space**
```
DEST = MY_MSSQL_SERVER
CON_NAME = DEFAULT
DB_SIZE = 1000 - 50000
DBFREE = 100 - 9999
```
**Purpose:** Retrieve size info from the default connection on the given RFC destination and keep only rows where database size is between 1 GB and 50 GB and free space is between 100 MB and about 10 GB.

**Use Case 2: Specific schema and database, log and DB free filters**
```
DEST = PROD_MSSQL
SCHEMA = SAPABAP1
DB_NAME = SAPDB
LOG_SIZE = 500 - 2000
LOG_ALLOC = 100 - 1500
DBFREE_PER = 10 - 90
```
**Purpose:** Target a specific schema and database on production and restrict results to log size/alloc ranges and DB free percentage.

**Use Case 3: Cloud destination with multiple result filters**
```
SW_DEST = CLOUD_RFC_DEST
CON_NAME = DEFAULT
DB_NAME = MYDB
DB_SIZE = 5000 - 100000
DB_ALLOC = 1000 - 80000
DBFREE = 500 - 50000
LOGFREE = 100 - 5000
DBFREE_PER = 5 - 95
```
**Purpose:** Use cloud destination for size retrieval, scope to one database, and apply size and free-space filters (DB and log) to focus on relevant instances.

**Use Case 4: Minimal connection and one filter**
```
DEST = DEV_MSSQL
DBFREE_PER = 0 - 10
```
**Purpose:** Connect to dev and show only databases where free space is 10% or less (low free space).


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_MSS_DB_SIZE | DBFREE | Free space in database in MB | INT4(10) | MSS_DBFREESIZE |
| /SKN/S_SW_01_03_MSS_DB_SIZE | DBFREE_PER | SW: Free DB space in MB (in percentage) | INT1(3) | /SKN/E_SW_DBFREE_PER |
| /SKN/S_SW_01_03_MSS_DB_SIZE | DB_ALLOC | Allocated space in database in MB | DEC(15) | MSS_DBALLOC |
| /SKN/S_SW_01_03_MSS_DB_SIZE | DB_SIZE | Database size in MB | DEC(15) | MSS_DBSIZE |
| /SKN/S_SW_01_03_MSS_DB_SIZE | LOGFREE | Free log space in MB | INT4(10) | MSS_LOGFREESIZE |
| /SKN/S_SW_01_03_MSS_DB_SIZE | LOG_ALLOC | Allocated space in log in MB | DEC(15) | MSS_LOGALLOC |
| /SKN/S_SW_01_03_MSS_DB_SIZE | LOG_SIZE | Log size in MB | DEC(15) | MSS_LOGSIZE |