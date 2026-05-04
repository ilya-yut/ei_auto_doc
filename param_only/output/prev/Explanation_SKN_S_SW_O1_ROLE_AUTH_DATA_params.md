# Parameters: SKN_S_SW_O1_ROLE_AUTH_DATA

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACTION | Action | CHAR | 30 | 0 | /SKN/E_ACTION | CHAR30 |
| 2 | AUTH | Authorization | CHAR | 12 | 0 | AGAUTH | XUAUTH |
| 3 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 4 | CHANGENR | Document number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 5 | COUNTER | ID | NUMC | 6 | 0 | MENU_NUM_6 | NUM06 |
| 6 | DEPARTMENT | Department | CHAR | 40 | 0 | AD_DPRTMNT | TEXT40 |
| 7 | FIELD | Field name | CHAR | 10 | 0 | AGRFIELD | XUFIELD |
| 8 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 9 | F_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 10 | F_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 11 | NAME_FIRST | First name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 12 | NAME_LAST | Last name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 13 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 14 | OBJECT | Authorization object | CHAR | 10 | 0 | AGOBJECT | XUOBJECT |
| 15 | OBJECTID | Role | CHAR | 30 | 0 | AGR_NAME | AGR_NAME |
| 16 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 17 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 18 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 19 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 19 parameters listed in the Parameters Reference Table above.

**ACTION** (Action):

Type of change recorded in the change document for role authorization data. The EI uses this to filter which change types are included in the result set.

**AUTH** (Authorization):

Authorization name in user master maintenance. The EI uses this to filter or display which authorization was changed within the role.

**BACKDAYS** (Back Days):

Number of days to look back from today when no explicit date range is supplied. The EI uses this to define the start of the monitoring window for role authorization change documents (e.g. last 10 days).

**CHANGENR** (Document number):

Change document number. Used to uniquely identify or filter specific change document entries for role authorization changes.

**COUNTER** (ID):

Menu ID for BIW. Used in the output to identify or order authorization change entries.

**DEPARTMENT** (Department):

Department from user address data. Populated in the output for the user who changed the role authorization.

**FIELD** (Field name):

Field name of an authorization. The EI uses this to filter or display which authorization field was changed.

**FNAME** (Field Name):

Field name (technical or descriptive). Populated in the output for the changed authorization field.

**F_NEW** (New value):

New value of the changed field in the change document. Appears in the output for each role authorization change record.

**F_OLD** (Old value):

Previous value of the changed field before the change. Appears in the output for each role authorization change record.

**NAME_FIRST** (First name):

First name of the user who changed the role authorization. Populated in the output from user master for the target user.

**NAME_LAST** (Last name):

Last name of the user who changed the role authorization. Populated in the output from user master for the target user.

**NAME_TEXT** (Full Name):

Full name of the user who changed the role authorization. Populated in the output from user master for the target user.

**OBJECT** (Authorization object):

Authorization object in user master maintenance. The EI uses this to filter or display which authorization object was changed within the role.

**OBJECTID** (Role):

Role name. The EI reads change documents for role authorization data and uses this to filter or display which role was changed.

**TCODE** (Transaction Code):

Transaction code in which the change was made. Populated in the output from the change document for each role authorization change record.

**UDATE** (Date):

Creation date of the change document. The EI uses this to restrict the change document selection to the given date range.

**USERNAME** (User):

User name of the person responsible in the change document (who changed the role authorization). The EI restricts the result to changes by users in the given range.

**UTIME** (Time):

Time of the change. The EI uses this (together with UDATE) to restrict the change document selection to the given time range.


### Parameter Relationships

**Time and date parameters:**

- **BACKDAYS** and **UDATE** define the monitoring window: when no explicit date range is supplied, the EI uses BACKDAYS to compute the start date (today minus BACKDAYS) and today as the end date. When a date range is supplied via UDATE or equivalent selection, that range restricts which role authorization change documents are read.
- **UDATE** and **UTIME** work together to restrict change document selection to a specific date and time range; the EI passes these to the change document read.

**Role and authorization parameters:**

- **OBJECTID** (role name), **OBJECT** (authorization object), **AUTH** (authorization), and **FIELD** relate to the same change record: which role, which authorization object, which authorization, and which field were changed. The EI uses them together when reading and filtering role authorization change documents.

**User filters:**

- **USERNAME** (user responsible in the change document) can be used together with **OBJECTID** (role) to analyze who changed which role's authorizations; the EI uses both when reading and filtering change documents.


### Default Values

- **BACKDAYS** — Default: `10` (number of days to look back when no explicit date range is supplied).
- **LANGU** — Default: `EN` (language for descriptions when not supplied).

**Note:** When no date range is supplied, the EI uses BACKDAYS to define the start of the monitoring window (today minus BACKDAYS through today) for reading role authorization change documents.

### Practical Configuration Examples

**Use Case 1: Last 10 days – all role authorization changes**
```
BACKDAYS = 10
USERNAME = *
```
**Purpose:** Monitor all role authorization changes in the last 10 days for any user. Useful for compliance and change tracking.

**Use Case 2: Specific role and authorization object**
```
OBJECTID = Z_MY_ROLE
OBJECT = S_TCODE
USERNAME = ADMIN01
UDATE = 20240101 - 20240131
```
**Purpose:** Trace authorization changes for a specific role (Z_MY_ROLE) and authorization object (S_TCODE) by a specific user (ADMIN01) in January 2024. Supports audit and troubleshooting.

**Use Case 3: Date range and transaction**
```
UDATE = 20240301 - 20240331
UTIME = 000000 - 235959
TCODE = PFCG
OBJECTID = Z*
```
**Purpose:** Review role authorization changes (roles starting with Z) in March 2024 made via transaction PFCG. Used for periodic access and role management reviews.

**Use Case 4: Multiple filters**
```
BACKDAYS = 14
USERNAME = ADMIN*
OBJECTID = Z_CUST*
OBJECT = S_TCODE
AUTH = *
```
**Purpose:** Combined monitoring over the last 14 days for admin users and custom roles (Z_CUST*), filtered by authorization object. Suitable for focused role authorization change analysis.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_O1_ROLE_AUTH_DATA | ACTION | Action | CHAR(30) | /SKN/E_ACTION |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | AUTH | Authorization name in user master maintenance | CHAR(12) | AGAUTH |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | COUNTER | Menu ID for BIW | NUMC(6) | MENU_NUM_6 |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | DEPARTMENT | Department | CHAR(40) | AD_DPRTMNT |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | FIELD | Field name of an authorization | CHAR(10) | AGRFIELD |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | F_NEW | New contents of changed field | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | F_OLD | Old contents of changed field | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | OBJECT | Auth. Object in User Master Maintenance | CHAR(10) | AGOBJECT |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | OBJECTID | Role Name | CHAR(30) | AGR_NAME |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | TCODE | Transaction in which a change was made | CHAR(20) | CDTCODE |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_O1_ROLE_AUTH_DATA | UTIME | Time changed | TIMS(6) | CDUZEIT |
