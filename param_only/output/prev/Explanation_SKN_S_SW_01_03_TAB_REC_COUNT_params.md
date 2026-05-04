# Parameters: SKN_S_SW_01_03_TAB_REC_COUNT

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | DEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 2 | LANGU | Language for Texts |  | 0 | 0 |  |  |
| 3 | RECORDS | Count (Int 4) | INT4 | 10 | 0 | /SKN/E_SW_COUNT |  |
| 4 | RFC | RFC Destination |  | 0 | 0 |  |  |
| 5 | TAB | Table Names | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 6 | TAB_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 6 parameters listed in the Parameters Reference Table above.

**DEST** (RFC Destination):

Logical RFC destination used when the EI calls the remote function to retrieve table record counts. When supplied, the EI runs in cloud mode and delegates the count to the specified system.

**LANGU** (Language for Texts):

Language key for short text (e.g. table description). When initial, the EI uses the system language. Used when resolving TAB_DESC or other language-dependent texts.

**RECORDS** (Count (Int 4)):

Record count returned for each table. Populated in the output by the EI after counting rows in the specified tables; used for filtering or display in the result set.

**RFC** (RFC Destination):

RFC destination (alternative or same as DEST). The EI uses this when a separate RFC destination is required for the table count call.

**TAB** (Table Names):

Table names to be checked. The EI counts the number of records in each table listed here (excluding INTTAB and APPEND table classes) and returns one row per table with TAB, RECORDS, and optionally TAB_DESC.

**TAB_DESC** (Short text):

Explanatory short text for the table. Populated in the output from the data dictionary (DD02T or similar) in the language given by LANGU when available.


### Parameter Relationships

**RFC and table parameters:**

- **DEST** (or **RFC**) and **TAB** work together: the EI uses the RFC destination to call the system where the tables are to be counted, and **TAB** supplies the list of table names. When DEST is initial, the EI runs locally and counts tables from the current system.

**Language and description:**

- **LANGU** and **TAB_DESC** work together: LANGU specifies the language for descriptive texts; TAB_DESC in the output is filled from the data dictionary in that language for each table in TAB.

**Table and output:**

- **TAB** (input table names) and **RECORDS** (output count per table) relate to the same result row: for each table in TAB, the EI returns TAB, RECORDS, and optionally TAB_DESC and DEST.


### Default Values

- **LANGU** — Default: system language (when initial, the EI uses SY-LANGU for language-dependent texts).

**Note:** When DEST is initial, the EI runs in local (non–cloud) mode and counts tables on the current system. When DEST is supplied, the EI runs in cloud mode and delegates to the specified RFC destination.

### Practical Configuration Examples

**Use Case 1: Local table count for two tables**
```
TAB = MARA
TAB = MARC
LANGU = E
```
**Purpose:** Count records in tables MARA and MARC on the current system and return short text in English. Useful for quick data volume checks.

**Use Case 2: RFC destination and multiple tables**
```
DEST = PRD_APP
TAB = VBAK
TAB = VBAP
TAB = LIKP
LANGU = E
```
**Purpose:** Count records in sales and delivery tables (VBAK, VBAP, LIKP) on the system specified by RFC destination PRD_APP, with English descriptions. Used for cross-system or cloud mode checks.

**Use Case 3: Single table and language**
```
TAB = DD02L
LANGU = D
```
**Purpose:** Count records in data dictionary table DD02L in the current system with German short text. Suitable for DDIC checks.

**Use Case 4: Multiple tables for audit**
```
TAB = USR02
TAB = AGR_USERS
TAB = AGR_DEFINE
DEST = PRD_APP
LANGU = E
RFC = PRD_APP
```
**Purpose:** Count records in user and role-related tables on the target system via DEST/RFC, with English descriptions. Supports audit and role/user object volume checks.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_TAB_REC_COUNT | DEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_03_TAB_REC_COUNT | RECORDS | SW : Count (Int 4) | INT4(10) | /SKN/E_SW_COUNT |
| /SKN/S_SW_01_03_TAB_REC_COUNT | TAB | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_01_03_TAB_REC_COUNT | TAB_DESC | Explanatory short text | CHAR(60) | DDTEXT |
