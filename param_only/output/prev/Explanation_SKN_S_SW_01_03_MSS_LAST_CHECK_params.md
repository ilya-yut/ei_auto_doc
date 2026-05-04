# Parameters: SKN_S_SW_01_03_MSS_LAST_CHECK

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ABAPSCHEMA | Schema name | CHAR | 128 | 0 | MSSSCHEMA | MSSSYSNAME |
| 2 | CON_NAME | DB Connection Name | CHAR | 30 | 0 | DBCON_NAME | DBCON_NAME |
| 3 | DBNAME | Database name | CHAR | 128 | 0 | MSSDB | MSSSYSNAME |
| 4 | DBRELEASE | Database release | CHAR | 3 | 0 | MSSDBRELE | MSSDBRELE |
| 5 | DBSCHEMA | Object source schema | CHAR | 128 | 0 | MSSOBJSRC | MSSSYSNAME |
| 6 | DBUSER | Database user name | CHAR | 128 | 0 | MSSDBUSER | MSSSYSNAME |
| 7 | DB_VERS | Version of SAP TSQL monitor layer | CHAR | 16 | 0 | MSSTSQLVER |  |
| 8 | DT | Date of last known good dbcc | DATS | 8 | 0 | MSSLKGDBCCDT | SYDATS |
| 9 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 11 | EXTRELEASE | SAP SupportPackLevel | CHAR | 10 | 0 | SAPPATCHLV | CHAR10 |
| 12 | HOSTNAME | SQL Server name | CHAR | 128 | 0 | MSSINSTANC | MSSSYSNAME |
| 13 | JAVASCHEMA | Schema name | CHAR | 128 | 0 | MSSSCHEMA | MSSSYSNAME |
| 14 | MESSAGE | Message text | CHAR | 220 | 0 | BAPI_MSG | TEXT220 |
| 15 | PROCSVERS | Version of SAP TSQL monitor layer | CHAR | 16 | 0 | MSSTSQLVER |  |
| 16 | RFCDEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 17 | SAPRELEASE | SAP Release | CHAR | 10 | 0 | SAPRELEASE | CHAR10 |
| 18 | SQLRELEASE | SQL Server release | CHAR | 16 | 0 | MSSRELEASE |  |
| 19 | TM | Time of last known good dbcc | TIMS | 6 | 0 | MSSLKGDBCCTM | SYTIME |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 19 parameters listed in the Parameters Reference Table above.

**ABAPSCHEMA** (Schema name):

Schema name used in the MSS context. Set to the ABAP schema identifier when the EI targets a specific schema for last DBCC check reporting.

**CON_NAME** (DB Connection Name):

Logical database connection name. Set to the DBCON entry that points to the target SQL Server instance and database for DBCC CHECKDB result retrieval.

**DBNAME** (Database name):

Database name. Set to the MS SQL Server database name to scope the last known good DBCC check date/time to that database.

**DBRELEASE** (Database release):

Database release (e.g. version string). Used to align or filter by the MS SQL Server database release when evaluating DBCC metadata.

**DBSCHEMA** (Object source schema):

Object source schema. Set when the EI must consider object source schema for MSS multiconnect context in last DBCC reporting.

**DBUSER** (Database user name):

Database user name. Set to narrow results to the DB user context relevant to the MSS connection used for DBCC checks.

**DB_VERS** (Version of SAP TSQL monitor layer):

Version of the SAP TSQL monitor layer. Set when filtering or reporting by this layer version for consistency with the monitored environment.

**DT** (Date of last known good dbcc):

Date of last known good DBCC. Set to filter or compare against the date when a successful DBCC CHECKDB was last run (DATS format).

**DURATION** (Duration In Time Units):

Duration in time units. Set together with DURATION_UNIT to define the time window (e.g. how far back to consider last DBCC results). Values not in the selection table are excluded (DELETE filter: DURATION NOT IN R_DURATION).

**DURATION and DURATION_UNIT Connection:** Use DURATION with DURATION_UNIT to express the window (e.g. 24 with unit H for 24 hours).

**DURATION_UNIT** (Duration Unit):

Unit for DURATION. Set to the same unit used for interpreting DURATION (hours, minutes, days, or full days).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**EXTRELEASE** (SAP SupportPackLevel):

SAP Support Package Level. Set when the EI output or filtering must align with a specific support pack level for the MSS stack.

**HOSTNAME** (SQL Server name):

SQL Server name (instance). Set to the host or instance name to restrict last DBCC check results to that server.

**JAVASCHEMA** (Schema name):

Schema name (Java/MSS context). Set when the EI uses Java schema for MSS multiconnect in last DBCC reporting.

**MESSAGE** (Message text):

Message text. Set or used for passing or displaying BAPI-style messages (e.g. errors or status) related to the last DBCC check.

**PROCSVERS** (Version of SAP TSQL monitor layer):

Version of the SAP TSQL monitor layer (procedure version). Set when reporting or filtering by this procedure layer version.

**RFCDEST** (RFC Destination):

RFC destination. Set to the logical RFC destination that defines the connection to the system where MSS last DBCC data is read (e.g. the application server or gateway for the MSS connection).

**SAPRELEASE** (SAP Release):

SAP Release. Set when the output or logic must be scoped to a specific SAP release for the MSS last DBCC context.

**SQLRELEASE** (SQL Server release):

SQL Server release string. Set to filter or report by the SQL Server version (e.g. XX.YYY.ZZZZ) for the last DBCC check metadata.

**TM** (Time of last known good dbcc):

Time of last known good DBCC. Set to filter or compare against the time of the last successful DBCC CHECKDB (TIMS format). Use together with DT for full date/time scope.


### Parameter Relationships

- **RFC/connection**
  - **RFCDEST** — Logical RFC destination used to connect to the system that provides MSS last DBCC data; defines the connection used for the EI call.

- **Duration**
  - **DURATION_UNIT** — Unit for the duration window (H, M, D, or F).
  - **DURATION** — Numeric duration in that unit; together they define the time window for evaluating last DBCC checks (e.g. last 24 hours, last 7 days).


### Default Values

- **DURATION_UNIT** — Default: `H` (Hours) when not supplied.

### Practical Configuration Examples

**Use case 1: Single DB, default duration unit (hours)**  
```  
RFCDEST = 'MSS_SYSTEM_A'
DBNAME = 'SAPDB'
DURATION = 24
DURATION_UNIT = 'H'
```
**Purpose:** Check last known good DBCC for database SAPDB via MSS_SYSTEM_A over the last 24 hours (hours unit). Typical daily health check for one database.

**Use case 2: Full days window and specific host**  
```  
RFCDEST = 'MSS_PROD'
HOSTNAME = 'SQLPROD01'
DURATION = 7
DURATION_UNIT = 'F'
```
**Purpose:** Restrict to SQL Server SQLPROD01 and evaluate last DBCC over the last 7 full days. Supports weekly compliance reporting for a specific host.

**Use case 3: Connection, database, and date/time scope**  
```  
CON_NAME = 'MSS_CON_01'
DBNAME = 'SAPDB'
RFCDEST = 'MSS_RFC'
DT = '20250301'
TM = '120000'
```
**Purpose:** Use a specific DB connection (MSS_CON_01) and database (SAPDB), and scope results to last known good DBCC at or before the given date/time (1 Mar 2025, 12:00). Useful for point-in-time or audit checks.

**Use case 4: Multi-parameter monitoring setup**  
```  
RFCDEST = 'MSS_BACKEND'
CON_NAME = 'DBCON_MSS'
DBNAME = 'SAPDB'
HOSTNAME = 'SQLHOST01'
DURATION = 48
DURATION_UNIT = 'H'
DT = '20250228'
```
**Purpose:** Full monitoring setup: backend RFC, named DB connection, database, and host, with a 48-hour window and optional reference date. Covers comprehensive last-DBCC monitoring for a single target.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_MSS_LAST_CHECK | ABAPSCHEMA | Schema name | CHAR(128) | MSSSCHEMA |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | CON_NAME | Logical name for a database connection | CHAR(30) | DBCON_NAME |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | DBNAME | Database name | CHAR(128) | MSSDB |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | DBRELEASE | MS SQL Server : Database Release X.YY.ZZZ | CHAR(3) | MSSDBRELE |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | DBSCHEMA | MSSQL Multiconnect : Object source schema | CHAR(128) | MSSOBJSRC |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | DBUSER | MSSQL Multiconnect : DB user | CHAR(128) | MSSDBUSER |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | DB_VERS | MSSQL Multiconnect : Version of SAP TSQL layer | CHAR(16) | MSSTSQLVER |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | DT | Date of last known good dbcc checkdb | DATS(8) | MSSLKGDBCCDT |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | EXTRELEASE | Support Package Level of a Software Component | CHAR(10) | SAPPATCHLV |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | HOSTNAME | MSSQL Multiconnect : SQL Server name | CHAR(128) | MSSINSTANC |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | JAVASCHEMA | Schema name | CHAR(128) | MSSSCHEMA |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | MESSAGE | Message Text | CHAR(220) | BAPI_MSG |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | PROCSVERS | MSSQL Multiconnect : Version of SAP TSQL layer | CHAR(16) | MSSTSQLVER |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | RFCDEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | SAPRELEASE | SAP Release | CHAR(10) | SAPRELEASE |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | SQLRELEASE | MSSQL : SQL Server Release string  >> XX.YYY.ZZZZ << | CHAR(16) | MSSRELEASE |
| /SKN/S_SW_01_03_MSS_LAST_CHECK | TM | Time of last known good dbcc checkdb | TIMS(6) | MSSLKGDBCCTM |