# Parameters: SKN_S_SW_O1_CRE_DEL_ROLE

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACTION | Action | CHAR | 30 | 0 | /SKN/E_ACTION | CHAR30 |
| 2 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 3 | CHANGENR | Document number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 4 | DEPARTMENT | Department | CHAR | 40 | 0 | AD_DPRTMNT | TEXT40 |
| 5 | NAME_FIRST | First name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 6 | NAME_LAST | Last name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 7 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 8 | OBJECTID | Role | CHAR | 30 | 0 | AGR_NAME | AGR_NAME |
| 9 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 10 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 11 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 12 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 12 parameters listed in the Parameters Reference Table above.

**ACTION** (Action):

Type of change recorded in the change document (e.g. create or delete role). The EI uses this to filter which change types are included in the result set.

**BACKDAYS** (Back Days):

Number of days to look back from today when no explicit date range is supplied. The EI uses this to define the start of the monitoring window for role create/delete change documents (e.g. last 10 days).

**CHANGENR** (Document number):

Change document number. Used to uniquely identify or filter specific change document entries for role changes.

**DEPARTMENT** (Department):

Department from user address data. Populated in the output for the user who created or deleted the role.

**NAME_FIRST** (First name):

First name of the user who created or deleted the role. Populated in the output from user master for the target user.

**NAME_LAST** (Last name):

Last name of the user who created or deleted the role. Populated in the output from user master for the target user.

**NAME_TEXT** (Full Name):

Full name of the user who created or deleted the role. Populated in the output from user master for the target user.

**OBJECTID** (Role):

Role name (e.g. AGR_DEFINE object). The EI reads change documents for role definitions and uses this to filter or display which role was created or deleted.

**TCODE** (Transaction Code):

Transaction code in which the change was made. Populated in the output from the change document for each role create/delete record.

**UDATE** (Date):

Creation date of the change document. The EI uses this to restrict the change document selection to the given date range.

**USERNAME** (User):

User name of the person responsible in the change document (who created or deleted the role). The EI restricts the result to changes by users in the given range.

**UTIME** (Time):

Time of the change. The EI uses this (together with UDATE) to restrict the change document selection to the given time range.


### Parameter Relationships

**Time and date parameters:**

- **BACKDAYS** and **UDATE** define the monitoring window: when no explicit date range is supplied (e.g. when UDATE is not passed as a filter), the EI uses BACKDAYS to compute the start date (today minus BACKDAYS) and today as the end date. When a date range is supplied via UDATE or equivalent selection, that range restricts which role create/delete change documents are read.
- **UDATE** and **UTIME** work together to restrict change document selection to a specific date and time range; the EI passes these to the change document read.

**User and role filters:**

- **USERNAME** (user responsible in the change document) can be used together with **OBJECTID** (role name) to analyze who created or deleted which roles; the EI uses both when reading and filtering change documents.


### Default Values

- **BACKDAYS** — Default: `10` (number of days to look back when no explicit date range is supplied).
- **LANGU** — Default: `EN` (language for descriptions when not supplied).

**Note:** When no date range is supplied, the EI uses BACKDAYS to define the start of the monitoring window (today minus BACKDAYS through today) for reading role create/delete change documents.

### Practical Configuration Examples

**Use Case 1: Last 10 days – all role create/delete changes**
```
BACKDAYS = 10
USERNAME = *
```
**Purpose:** Monitor all role create and delete changes in the last 10 days for any user. Useful for compliance and change tracking.

**Use Case 2: Specific role and user**
```
OBJECTID = Z_MY_ROLE
USERNAME = ADMIN01
UDATE = 20240101 - 20240131
```
**Purpose:** Trace create/delete changes for a specific role (Z_MY_ROLE) by a specific user (ADMIN01) in January 2024. Supports audit and troubleshooting.

**Use Case 3: Date range and transaction**
```
UDATE = 20240301 - 20240331
UTIME = 000000 - 235959
TCODE = PFCG
OBJECTID = Z*
```
**Purpose:** Review role changes (roles starting with Z) in March 2024 made via transaction PFCG. Used for periodic access and role management reviews.

**Use Case 4: Multiple filters**
```
BACKDAYS = 14
USERNAME = ADMIN*
OBJECTID = Z_CUST*
TCODE = PFCG
CHANGENR = *
```
**Purpose:** Combined monitoring over the last 14 days for admin users and custom roles (Z_CUST*), with transaction and change number scope. Suitable for focused role change analysis.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_O1_CRE_DEL_ROLE | ACTION | Action | CHAR(30) | /SKN/E_ACTION |
| /SKN/S_SW_O1_CRE_DEL_ROLE | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_O1_CRE_DEL_ROLE | DEPARTMENT | Department | CHAR(40) | AD_DPRTMNT |
| /SKN/S_SW_O1_CRE_DEL_ROLE | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_O1_CRE_DEL_ROLE | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_O1_CRE_DEL_ROLE | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_O1_CRE_DEL_ROLE | OBJECTID | Role Name | CHAR(30) | AGR_NAME |
| /SKN/S_SW_O1_CRE_DEL_ROLE | TCODE | Transaction in which a change was made | CHAR(20) | CDTCODE |
| /SKN/S_SW_O1_CRE_DEL_ROLE | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_O1_CRE_DEL_ROLE | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_O1_CRE_DEL_ROLE | UTIME | Time changed | TIMS(6) | CDUZEIT |
