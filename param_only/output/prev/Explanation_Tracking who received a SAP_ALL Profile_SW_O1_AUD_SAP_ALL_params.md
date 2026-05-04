# Parameters: Tracking who received a SAP_ALL Profile_SW_O1_AUD_SAP_ALL

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
| 23 | SUBSYSTEM | Receiving system | CHAR | 10 | 0 | RFCRCVSYS | LOGSYS |
| 24 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 24 parameters listed in the Parameters Reference Table above.

**ACTION** (Type of the Change Doc.):

Identifies the kind of user change recorded in the change document. The EI uses this to filter which change types are included in the result set (e.g. SAP_ALL profile assignment).

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

Department from user address data. Populated in the output for the user who received the profile change.

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

First name of the user who received the profile change. Populated in the output from user master for the target user.

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

**SUBSYSTEM** (Receiving system):

Logical system (receiving system) from which change documents are read. The EI uses this to call the correct system for user change document data.

**TCODE** (Transaction Code):

Transaction code associated with the change. Populated in the output when available from the change document context.


### Parameter Relationships

**Time and duration parameters:**

- **BACKDAYS**, **MODDA**, and **MODTI** define the monitoring window: when no explicit date range is supplied (MODDA empty), the EI uses BACKDAYS to compute the start date (today minus BACKDAYS) and today as the end date. When MODDA is supplied, the low/high values define the date range; MODTI optionally narrows the time range. Together they control which change documents are read from the receiving system.
- **DURATION** and **DURATION_UNIT** work together: DURATION is a numeric value (e.g. number of days or hours), and DURATION_UNIT specifies whether that value is in hours (H), minutes (M), days (D), or full days (F). The EI computes the duration from the selected date/time range and filters output records by the resulting duration value.

**User and changer filters:**

- **BNAME** (user whose changes are read) and **MODBE** (user who performed the change) can be used together to analyze both the target user and the modifier; the EI passes both ranges to the change document read.


### Default Values

- **BACKDAYS** — Default: `10` (number of days to look back when no explicit date range is supplied).
- **LANGU** — Default: `EN` (language for descriptions when not supplied).
- **DURATION_UNIT** — Default: initial (empty); duration is computed from the selected date/time range using the unit supplied or a single default interpretation when not set.

**Note:** When MODDA (modification date) is not supplied, the EI uses BACKDAYS to define the start of the monitoring window (today minus BACKDAYS through today).

### Practical Configuration Examples

**Use Case 1: Last 10 days – SAP_ALL profile changes**
```
BACKDAYS = 10
SUBSYSTEM = PRD
```
**Purpose:** Monitor which users received SAP_ALL profile changes in the last 10 days in the production system. Useful for compliance and access reviews.

**Use Case 2: Fixed date range and user filter**
```
AGR_FDATE = 20240101
AGR_TDATE = 20240131
BNAME = USER01
MODBE = ADMIN02
SUBSYSTEM = PRD
```
**Purpose:** Review SAP_ALL profile changes in January 2024 for a specific user (USER01) performed by a specific changer (ADMIN02) in production. Supports audit and access certification.

**Use Case 3: Duration filter in days**
```
BACKDAYS = 14
DURATION_UNIT = D
DURATION = 1 - 999999
SUBSYSTEM = PRD
```
**Purpose:** Combined monitoring over the last 14 days with duration in days so that only changes within a defined duration range are included. Suitable for cross-cutting change analysis.

**Use Case 4: Date and time range**
```
MODDA = 20240301 - 20240331
MODTI = 000000 - 235959
BNAME = USER*
SUBSYSTEM = PRD
TCODE = SU01
```
**Purpose:** Trace SAP_ALL profile changes in March 2024 for all users (USER*) in production, with optional transaction code filter. Used for periodic reviews.


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
