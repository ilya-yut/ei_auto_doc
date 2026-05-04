# Parameters: SKN_S_SW_01_03_MSS_BKP_LIST

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | 1FAMILYNUM | First family media | INT2 | 5 | 0 | MSSFSTFANU |  |
| 2 | BACKSETID | Backup id | INT4 | 10 | 0 | MSQBAKUPID | INT4 |
| 3 | BACKUPPOS | Backupset position | INT4 | 10 | 0 | MSSBKSETPO |  |
| 4 | BACKUPTYPE | Backup type description | CHAR | 30 | 0 | MSSBKTYPES |  |
| 5 | BACKUP_DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 6 | DBNAME | Database name | CHAR | 128 | 0 | MSSDB | MSSSYSNAME |
| 7 | DESC1 | Backup mediaset description | CHAR | 128 | 0 | MSSMDSETDS | MSSSYSNAME |
| 8 | DESC2 | Backup set description | CHAR | 255 | 0 | MSSBKDESC |  |
| 9 | DEST | RFC Destination |  | 0 | 0 |  |  |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | END_DATE | Backup finish date | CHAR | 20 | 0 | MSQBKFINDT |  |
| 13 | END_DT | Date | DATS | 8 | 0 | DATUM | DATUM |
| 14 | END_TM | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 15 | EXP_DATE | Backup expiration date | CHAR | 20 | 0 | MSQEXPDATE | CHAR20 |
| 16 | JOB_ID | Job id | RAW | 16 | 0 | MSQJOBID | MSQHEX16 |
| 17 | MACHINENAM | Host server name | CHAR | 30 | 0 | MSQHOSTNA | CHAR30 |
| 18 | MEIDASETID | Backup mediaset id | INT4 | 10 | 0 | MSSBKMDSID |  |
| 19 | NAME1 | Mediaset name | CHAR | 128 | 0 | MSQBKMEDNA |  |
| 20 | NAME2 | Backup set | CHAR | 128 | 0 | MSSBKNAME | MSSSYSNAME |
| 21 | SERVERNAM | SQL task server | CHAR | 30 | 0 | MSSSERVER1 | TEXT30 |
| 22 | SFTNAME | Backup software name | CHAR | 128 | 0 | MSSBKSFTNA |  |
| 23 | START_DATE | Backup start time | CHAR | 20 | 0 | MSSBKSTART | CHAR20 |
| 24 | START_DT | Date | DATS | 8 | 0 | DATUM | DATUM |
| 25 | START_TM | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 26 | TYPE | Backup type (D /I / L- log) | CHAR | 1 | 0 | MSQBKTYPE | CHAR1 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 26 parameters listed in the Parameters Reference Table above.

**1FAMILYNUM** (First family media):

Identifies the first family media in the backup context. Used by the EI to associate backup records with the correct media family for reporting and grouping.

**BACKSETID** (Backup id):

MS SQL Server backup task identifier. The EI uses this to uniquely identify a backup operation for tracking and display.

**BACKUPPOS** (Backupset position):

Position of the backup set within the backup media. Enables ordering and identification of backup components in multi-part backup scenarios.

**BACKUPTYPE** (Backup type description):

Text description of the backup type (e.g. full, differential, log). Used to label and filter backups by their operational type.

**BACKUP_DURATION** (Duration In Time Units):

Length of the backup operation expressed in the unit given by DURATION_UNIT. The EI computes or uses this value for reporting how long the backup took.

**DBNAME** (Database name):

Name of the MS SQL Server database. Restricts the backup list to a specific database.

**DESC1 - DESC2** (Backup mediaset description – Backup set description):

DESC1 is the description of the backup media set; DESC2 is the description of the backup set. Both provide human-readable labels for media set and backup set used in the backup.

**DEST** (RFC Destination):

RFC destination used to call the MS SQL Server backend. Determines which system and connection the EI uses to retrieve the backup list.

**DURATION** (Duration In Time Units):

Duration from backup start to a reference time (e.g. current time), in the unit given by DURATION_UNIT. Used for reporting and filtering by elapsed time.

**DURATION_UNIT** (Duration Unit):

Unit in which BACKUP_DURATION and DURATION are expressed. Must match the unit expected by the EI for duration calculations and display.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**END_DATE** (Backup finish date):

Character representation of the backup completion date/time. Used for display and for deriving END_DT and END_TM.

**END_DT** (Date):

Date component of the backup finish. Used for date-based ordering and duration calculations.

**END_TM** (Time):

Time component of the backup finish. Used together with END_DT for duration calculations.

**EXP_DATE** (Backup expiration date):

Date when the backup expires or is no longer retained. Used for retention reporting and filtering.

**JOB_ID** (Job id):

MS SQL Server job identifier that ran or triggered the backup. Used to correlate backups with job execution.

**MACHINENAM** (Host server name):

Name of the host server where the backup ran. Restricts the list to backups on a specific machine.

**MEIDASETID** (Backup mediaset id):

Identifier of the backup media set. Used to group and identify backup media.

**NAME1 - NAME2** (Mediaset name – Backup set):

NAME1 is the name of the media set; NAME2 is the name of the backup set. Both provide short labels for media set and backup set.

**SERVERNAM** (SQL task server):

Name of the SQL Server instance or task server used for the backup. Restricts the list to a specific server.

**SFTNAME** (Backup software name):

Name of the backup software that performed the backup. Used to filter or label by backup product.

**START_DATE** (Backup start time):

Character representation of the backup start date/time. Used for display and for deriving START_DT and START_TM.

**START_DT** (Date):

Date component of the backup start. Used for date-based filtering and duration calculations.

**START_TM** (Time):

Time component of the backup start. Used together with START_DT for duration calculations.

**TYPE** (Backup type (D /I / L- log)):

Single-character backup type: D (full/database), I (differential), or L (log). Restricts the list by backup type.


### Parameter Relationships

**RFC/connection**
- **DEST** — RFC destination used to call the MS SQL Server; determines the system and connection for retrieving the backup list.

**Duration**
- **DURATION_UNIT** — Unit (H/M/D/F) for duration values.
- **BACKUP_DURATION** — Duration of the backup in DURATION_UNIT.
- **DURATION** — Elapsed time from backup start to reference time in DURATION_UNIT.

**Filters (multiselect)**
- **DBNAME** — Filter by database name.
- **SERVERNAM** — Filter by SQL task server.
- **MACHINENAM** — Filter by host server name.
- **TYPE** — Filter by backup type (D/I/L).
- **BACKUPTYPE** — Filter by backup type description.


### Default Values

- **DURATION_UNIT** — Default: `M` (Minutes). Used for BACKUP_DURATION and DURATION when not supplied.
- **LANGU** — System language (SY-LANGU) when not supplied. (Not in the Parameters Reference Table; used internally for language-dependent data.)
- **MANAGE_IN_UTC** and **DEST** — If the code or connection settings provide defaults (e.g. from SW_DEST or profile), they are used when not supplied; otherwise values are used as entered.

### Practical Configuration Examples

**Use Case 1: Single database, duration in minutes**

```
DEST = 'MSS_PROD'
DBNAME = 'SAP_PRD'
DURATION_UNIT = 'M'
BACKUP_DURATION = 120
```

**Purpose:** List backups for database SAP_PRD via RFC destination MSS_PROD, with duration reported in minutes and filtering for backups that took at least 120 minutes.

**Use Case 2: Multiple servers and backup types**

```
DEST = 'MSS_DR'
SERVERNAM = 'SQL01'
MACHINENAM = 'HOST-A'
TYPE = 'D'
BACKUPTYPE = 'Full'
DURATION_UNIT = 'H'
```

**Purpose:** Focus on full backups (type D) on server SQL01 / host HOST-A via MSS_DR, with duration in hours.

**Use Case 3: Full-day filtering and duration**

```
DEST = 'MSS_BKP'
DBNAME = 'SAP_ERP'
DURATION_UNIT = 'F'
DURATION = 1
BACKUP_DURATION = 240
```

**Purpose:** Use full-day unit (F) for DURATION; restrict to database SAP_ERP and backups with duration 240 (in the chosen unit). DURATION = 1 with unit F supports day-level filtering.

**Use Case 4: Log backups on specific host**

```
DEST = 'MSS_LOG'
MACHINENAM = 'HOST-B'
TYPE = 'L'
DURATION_UNIT = 'M'
DURATION = 60
```

**Purpose:** List log backups (type L) on HOST-B via MSS_LOG, with duration in minutes and DURATION set to 60 for elapsed-time criteria.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_MSS_BKP_LIST | 1FAMILYNUM | MS SQL Server - Family num of media where the backup starts | INT2(5) | MSSFSTFANU |
| /SKN/S_SW_01_03_MSS_BKP_LIST | BACKSETID | MS SQL Server backup ID for the backup task | INT4(10) | MSQBAKUPID |
| /SKN/S_SW_01_03_MSS_BKP_LIST | BACKUPPOS | MS SQL Server - Backup set position | INT4(10) | MSSBKSETPO |
| /SKN/S_SW_01_03_MSS_BKP_LIST | BACKUPTYPE | MS SQL server - Backup Type description | CHAR(30) | MSSBKTYPES |
| /SKN/S_SW_01_03_MSS_BKP_LIST | BACKUP_DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_03_MSS_BKP_LIST | DBNAME | Database name | CHAR(128) | MSSDB |
| /SKN/S_SW_01_03_MSS_BKP_LIST | DESC1 | MS Sql Server -  Backup media set description | CHAR(128) | MSSMDSETDS |
| /SKN/S_SW_01_03_MSS_BKP_LIST | DESC2 | Description of the backup set | CHAR(255) | MSSBKDESC |
| /SKN/S_SW_01_03_MSS_BKP_LIST | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_03_MSS_BKP_LIST | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_03_MSS_BKP_LIST | END_DATE | MS SQL server backup finish date | CHAR(20) | MSQBKFINDT |
| /SKN/S_SW_01_03_MSS_BKP_LIST | END_DT | Date | DATS(8) | DATUM |
| /SKN/S_SW_01_03_MSS_BKP_LIST | END_TM | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_01_03_MSS_BKP_LIST | EXP_DATE | MS SQL server - backup expiration date | CHAR(20) | MSQEXPDATE |
| /SKN/S_SW_01_03_MSS_BKP_LIST | JOB_ID | MS SQL Server job ID | RAW(16) | MSQJOBID |
| /SKN/S_SW_01_03_MSS_BKP_LIST | MACHINENAM | MS SQL server - server name | CHAR(30) | MSQHOSTNA |
| /SKN/S_SW_01_03_MSS_BKP_LIST | MEIDASETID | Backup media set id | INT4(10) | MSSBKMDSID |
| /SKN/S_SW_01_03_MSS_BKP_LIST | NAME1 | MS SQL Server - Desciption of the media name | CHAR(128) | MSQBKMEDNA |
| /SKN/S_SW_01_03_MSS_BKP_LIST | NAME2 | Name of the backup set | CHAR(128) | MSSBKNAME |
| /SKN/S_SW_01_03_MSS_BKP_LIST | SERVERNAM | MS SQL Server server to be used for a SQL task execution | CHAR(30) | MSSSERVER1 |
| /SKN/S_SW_01_03_MSS_BKP_LIST | SFTNAME | MS SQL Server - Backup software Name | CHAR(128) | MSSBKSFTNA |
| /SKN/S_SW_01_03_MSS_BKP_LIST | START_DATE | MS SQL Server: Database starting time | CHAR(20) | MSSBKSTART |
| /SKN/S_SW_01_03_MSS_BKP_LIST | START_DT | Date | DATS(8) | DATUM |
| /SKN/S_SW_01_03_MSS_BKP_LIST | START_TM | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_01_03_MSS_BKP_LIST | TYPE | MS SQL server - backup type ( full, differential, log) | CHAR(1) | MSQBKTYPE |