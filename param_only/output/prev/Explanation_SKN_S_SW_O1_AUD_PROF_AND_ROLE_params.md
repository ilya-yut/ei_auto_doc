# Parameters: SKN_S_SW_O1_AUD_PROF_AND_ROLE

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACTION | Type of the Change Doc. | CHAR | 1 | 0 | /SKN/E_SW_USER_ACT | /SKN/D_SW_USER_ACT |
| 2 | ACTION_DESC | Type of the Change Doc. | CHAR | 30 | 0 | /SKN/E_SW_USER_ACT_DESC |  |
| 3 | AGR_FDATE | Change Start Date | DATS | 8 | 0 | SUID_CHANGE_FROM_DAT | DATS |
| 4 | AGR_TDATE | Change End Date | DATS | 8 | 0 | SUID_CHANGE_TO_DAT | DATS |
| 5 | ATTRBT | Attribute Name of the Changed Field | CHAR | 20 | 0 | XUATTR_CD | USATTRID |
| 6 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 7 | BNAME | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 8 | COUNTER | Counter for Change Documents | CHAR | 4 | 0 | XUCOUNT_CD |  |
| 9 | DEPARTMENT | Department | CHAR | 40 | 0 | AD_DPRTMNT | TEXT40 |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | MODBE | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 13 | MODBE_NAME_FIRST | Changer First Name | CHAR | 40 | 0 | /SKN/E_CHANGER_FIRST_NAME | TEXT40 |
| 14 | MODBE_NAME_LAST | Changer Last Name | CHAR | 40 | 0 | /SKN/E_CHANGER_LAST_NAME | TEXT40 |
| 15 | MODDA | Modification date | DATS | 8 | 0 | XUMODDATE | DATUM |
| 16 | MODTI | Modification time | TIMS | 6 | 0 | XUMODTIME | UZEIT |
| 17 | NAME_FIRST | First name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 18 | NAME_LAST | Last name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 19 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 20 | NEW_VAL | New Contents of Changed Field | CHAR | 100 | 0 | XUAV_CDNEW |  |
| 21 | NRPRO | Number of profiles or authorizations | INT2 | 5 | 0 | XUNUMBER | XUNUMBER |
| 22 | OLD_VAL | Old Contents of Changed Field | CHAR | 100 | 0 | XUAV_CDOLD |  |
| 23 | PROF_ASS | X IF Tracking Adding Profiles |  | 0 | 0 |  |  |
| 24 | PROF_ASS_T | Profile Adding Slection |  | 0 | 0 |  |  |
| 25 | PROF_DEL | X IF Tracking Delete Profiles |  | 0 | 0 |  |  |
| 26 | PROF_DEL_T | Profile Deletion Slection |  | 0 | 0 |  |  |
| 27 | ROLE | X IF Tracking Roles |  | 0 | 0 |  |  |
| 28 | ROLE_T | ROLE Selection |  | 0 | 0 |  |  |
| 29 | SUBSYSTEM | Receiving system | CHAR | 10 | 0 | RFCRCVSYS | LOGSYS |
| 30 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |



### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 30 parameters listed in the Parameters Reference Table above.

**ACTION** (Type of the Change Doc.):

Identifies the kind of user change recorded in the change document (e.g. profile assignment, profile deletion, role assignment). The EI uses this to filter which change types are included in the result set.

**ACTION_DESC** (Type of the Change Doc.):

Text description of the change type. Populated in the output from the change document system for display.

**AGR_FDATE** (Change Start Date):

Start of the date range for change document selection. The EI includes only changes from the receiving system that fall on or after this date.

**AGR_TDATE** (Change End Date):

End of the date range for change document selection. The EI includes only changes that fall on or before this date.

**ATTRBT** (Attribute Name of the Changed Field):

Name of the attribute (field) that was changed in the user master or authorization object. Used to restrict which changed attributes appear in the result.

**BACKDAYS** (Days Backward from today):

Number of days to look back from today when no explicit date range is supplied. The EI uses this to define the start of the monitoring window for change documents (e.g. last 10 days).

**BNAME** (User):

User name (logon) of the user whose change documents are read. The EI restricts the result to changes for users in the given range.

**COUNTER** (Counter for Change Documents):

Counter value for the change document record. Used to uniquely identify or filter specific change document entries.

**DEPARTMENT** (Department):

Department from user address data. Populated in the output for the user who received the profile or role change.

**DURATION** (Duration In Time Units):

Length of the monitoring window expressed in the unit given by DURATION_UNIT. The EI computes duration from the selected date/time range and filters output by this value.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is interpreted (hours, minutes, days, or full days for day-level filtering). The EI uses this when computing the duration value from the selected date/time range.

**DURATION_UNIT Options:**

- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**MODBE** (Changed By):

User who performed the change (modifier). The EI restricts the result to changes made by users in the given range.

**MODBE_NAME_FIRST** (Changer First Name):

First name of the user who performed the change. Filled in the output from user master when MODBE is set.

**MODBE_NAME_LAST** (Changer Last Name):

Last name of the user who performed the change. Filled in the output from user master when MODBE is set.

**MODDA** (Modification date):

Date of the change. The EI uses this (together with MODTI) to restrict the change document selection to the given date range.

**MODTI** (Modification time):

Time of the change. The EI uses this (together with MODDA) to restrict the change document selection to the given time range.

**NAME_FIRST** (First name):

First name of the user who received the profile or role change. Populated in the output from user master for the target user.

**NAME_LAST** (Last name):

Last name of the user who received the change. Populated in the output from user master for the target user.

**NAME_TEXT** (Full Name):

Full name of the user who received the change. Populated in the output from user master for the target user.

**NEW_VAL** (New Contents of Changed Field):

New value of the changed field in the change document. Appears in the output for each change record.

**NRPRO** (Number of profiles or authorizations):

Count of profiles or authorizations associated with the user or change. Used in the output to show how many profiles/authorizations are affected.

**OLD_VAL** (Old Contents of Changed Field):

Previous value of the changed field before the change. Appears in the output for each change record.

**PROF_ASS** (X IF Tracking Adding Profiles):

Controls whether the EI includes change documents for profile assignments. When set, profile assignment changes are read and included in the result.

**PROF_ASS Options:**

- **X**: Include profile assignment changes.
- ** ** (space): Do not include profile assignment changes.

**PROF_ASS_T** (Profile Adding Slection):

List of profile names or ranges to restrict which profile assignment changes are included. Used together with PROF_ASS when PROF_ASS is set.

**PROF_DEL** (X IF Tracking Delete Profiles):

Controls whether the EI includes change documents for profile deletions. When set, profile deletion changes are read and included in the result.

**PROF_DEL Options:**

- **X**: Include profile deletion changes.
- ** ** (space): Do not include profile deletion changes.

**PROF_DEL_T** (Profile Deletion Slection):

List of profile names or ranges to restrict which profile deletion changes are included. Used together with PROF_DEL when PROF_DEL is set.

**ROLE** (X IF Tracking Roles):

Controls whether the EI includes change documents for role assignments. When set, role assignment changes are read and included in the result.

**ROLE Options:**

- **X**: Include role assignment changes.
- ** ** (space): Do not include role assignment changes.

**ROLE_T** (ROLE Selection):

List of role names or ranges to restrict which role assignment changes are included. Used together with ROLE when ROLE is set.

**SUBSYSTEM** (Receiving system):

Logical system (receiving system) from which change documents are read. The EI uses this to call the correct system for user change document data.

**TCODE** (Transaction Code):

Transaction code associated with the change. Populated in the output when available from the change document context.



### Parameter Relationships

**Time and duration parameters:**

- **BACKDAYS**, **MODDA**, and **MODTI** define the monitoring window: when no explicit date range is supplied (MODDA empty), the EI uses BACKDAYS to compute the start date (today minus BACKDAYS) and today as the end date. When MODDA is supplied, the low/high values define the date range; MODTI optionally narrows the time range. Together they control which change documents are read from the receiving system.
- **DURATION** and **DURATION_UNIT** work together: DURATION is a numeric value (e.g. number of days or hours), and DURATION_UNIT specifies whether that value is in hours (H), minutes (M), days (D), or full days (F). The EI computes the duration from the selected date/time range and filters output records by the resulting duration value.

**Profile and role tracking parameters:**

- **PROF_ASS** and **PROF_ASS_T** work together: set PROF_ASS to include profile assignment changes, and use PROF_ASS_T to restrict to specific profile names or ranges.
- **PROF_DEL** and **PROF_DEL_T** work together: set PROF_DEL to include profile deletion changes, and use PROF_DEL_T to restrict to specific profile names or ranges.
- **ROLE** and **ROLE_T** work together: set ROLE to include role assignment changes, and use ROLE_T to restrict to specific role names or ranges.

**User and changer filters:**

- **BNAME** (user whose changes are read) and **MODBE** (user who performed the change) can be used together to analyze both the target user and the modifier; the EI passes both ranges to the change document read.



### Default Values

- **BACKDAYS** — Default: `10` (number of days to look back when no explicit date range is supplied).
- **LANGU** — Default: `EN` (language for descriptions when not supplied).
- **PROF_ASS** — Default: initial (empty); profile assignment changes are not included when not set.
- **PROF_DEL** — Default: initial (empty); profile deletion changes are not included when not set.
- **ROLE** — Default: initial (empty); role assignment changes are not included when not set.
- **DURATION_UNIT** — Default: initial (empty); duration is computed from the selected date/time range using the unit supplied or a single default interpretation when not set.

**Note:** When MODDA (modification date) is not supplied, the EI uses BACKDAYS to define the start of the monitoring window (today minus BACKDAYS through today).


### Practical Configuration Examples

**Use Case 1: Last 10 days – profile assignment changes only**
```
BACKDAYS = 10
PROF_ASS = X
PROF_ASS_T = SAP_ALL*
```
**Purpose:** Monitor which users received profile assignments in the last 10 days, restricted to profiles whose names start with SAP_ALL. Useful for compliance and access reviews.

**Use Case 2: Role assignment changes in a fixed date range**
```
AGR_FDATE = 20240101
AGR_TDATE = 20240131
ROLE = X
ROLE_T = SAP_*
SUBSYSTEM = PRD
```
**Purpose:** Review role assignment changes in a specific month (January 2024) for roles starting with SAP_ in the production system. Supports periodic access certification.

**Use Case 3: Profile and role changes with duration filter (full days)**
```
BACKDAYS = 14
PROF_ASS = X
PROF_DEL = X
ROLE = X
DURATION_UNIT = D
DURATION = 1 - 999999
```
**Purpose:** Combined monitoring of profile additions, profile deletions, and role assignments over the last 14 days, with duration in days so that only changes within a defined duration range are included. Suitable for cross-cutting user change analysis.

**Use Case 4: Single user and changer – profile changes**
```
BNAME = USER01
MODBE = ADMIN02
MODDA = 20240301 - 20240331
PROF_ASS = X
PROF_ASS_T = Z*
```
**Purpose:** Trace profile assignment changes for a specific user (USER01) performed by a specific changer (ADMIN02) in March 2024, limited to custom profiles (Z*). Used for audit and troubleshooting.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | ACTION | SW: Type of the Change Document | CHAR(1) | /SKN/E_SW_USER_ACT |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | ACTION_DESC | SW: Type of Change Document Desc. | CHAR(30) | /SKN/E_SW_USER_ACT_DESC |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | AGR_FDATE | Start of the Change Date of the Validity | DATS(8) | SUID_CHANGE_FROM_DAT |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | AGR_TDATE | End of the Change Date of the Validity | DATS(8) | SUID_CHANGE_TO_DAT |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | ATTRBT | Attribute Name of the Changed Field | CHAR(20) | XUATTR_CD |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | BNAME | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | COUNTER | Counter for Change Documents | CHAR(4) | XUCOUNT_CD |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | DEPARTMENT | Department | CHAR(40) | AD_DPRTMNT |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | MODBE | Last Changed By | CHAR(12) | XUMODIFIER |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | MODBE_NAME_FIRST | Changer First Name | CHAR(40) | /SKN/E_CHANGER_FIRST_NAME |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | MODBE_NAME_LAST | Changer Last Name | CHAR(40) | /SKN/E_CHANGER_LAST_NAME |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | MODDA | Modification date | DATS(8) | XUMODDATE |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | MODTI | Modification time | TIMS(6) | XUMODTIME |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | NEW_VAL | New Contents of Changed Field | CHAR(100) | XUAV_CDNEW |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | NRPRO | Number of profiles or authorizations | INT2(5) | XUNUMBER |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | OLD_VAL | Old Contents of Changed Field | CHAR(100) | XUAV_CDOLD |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | SUBSYSTEM | Receiving system for central user administration | CHAR(10) | RFCRCVSYS |
| /SKN/S_SW_O1_AUD_PROF_AND_ROLE | TCODE | Transaction Code | CHAR(20) | TCODE |
