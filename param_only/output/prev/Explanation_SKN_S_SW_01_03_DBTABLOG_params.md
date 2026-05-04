# Parameters: SKN_S_SW_01_03_DBTABLOG

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | DATA_CNT | Number of records | INT4 | 10 | 0 | INT4 | INT4 |
| 2 | DATA_VOL | Table Volume | FLTP | 16 | 16 |  |  |
| 3 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL **3** parameters listed in the Parameters Reference Table above.

**DATA_CNT** (Number of records):

Used as a post-query filter on the result set from DBTABLOG. The EI restricts returned rows to those where the record count matches the supplied range or value. Typically used to focus on tables with a specific number of records (e.g. empty tables, or tables above a threshold).

**DATA_VOL** (Table Volume):

Used as a post-query filter on the result set from DBTABLOG. The EI restricts returned rows to those where the table volume matches the supplied range or value. Enables focusing on tables by size (e.g. large tables for capacity or cleanup analysis).

**TABNAME** (Table Name):

Drives the primary selection from DBTABLOG: only rows for the specified table name(s) are read. Supply one or more table names (or a range) to limit the EI to those tables; when not supplied, the selection is unrestricted by table name.


### Parameter Relationships

**Table selection**

- **TABNAME** — Primary filter: determines which DBTABLOG rows are selected (WHERE TABNAME IN R_TABNAME).

**Result filtering**

- **DATA_CNT** — Secondary filter applied to the result set (R_DATA_CNT).
- **DATA_VOL** — Secondary filter applied to the result set (R_DATA_VOL).


### Default Values

The code does not set explicit default values for any of the parameters; all three (DATA_CNT, DATA_VOL, TABNAME) are optional. When not supplied, the corresponding selection or filter is not applied (T_SELECT is optional).

### Practical Configuration Examples

**Use Case 1: Specific tables by record count**

```
TABNAME = MARA
DATA_CNT = 1000 - 999999
```

**Purpose:** Return DBTABLOG entries for table MARA where the number of records is between 1,000 and 999,999, e.g. for monitoring or cleanup of medium-to-large tables.

**Use Case 2: Specific tables by volume**

```
TABNAME = BKPF
DATA_VOL = 1000000 -
```

**Purpose:** Return DBTABLOG entries for table BKPF with table volume of at least 1,000,000, e.g. to identify large document tables for archiving or performance review.

**Use Case 3: Table selection with record count and volume**

```
TABNAME = VBAK
DATA_CNT = 1 - 100
DATA_VOL = 0 - 50000
```

**Purpose:** Return DBTABLOG entries for sales order header table VBAK restricted to small record counts and low volume, e.g. for pilot or test-system analysis.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_DBTABLOG | DATA_CNT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_03_DBTABLOG | DATA_VOL |  | FLTP(16,16) |  |
| /SKN/S_SW_01_03_DBTABLOG | TABNAME | Table Name | CHAR(30) | TABNAME |