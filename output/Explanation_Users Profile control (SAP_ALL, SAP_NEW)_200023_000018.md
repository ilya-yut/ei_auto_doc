# Exception Indicator: User Profile - SW_01_20_USR_PROFILE

## General Overview

This Exception Indicator (EI) monitors user master and user profile assignments to identify users who have been assigned high-risk or sensitive profiles such as SAP_ALL or SAP_NEW, or who exhibit lock status, last-logon, or validity patterns that require management or security review. It correlates user (BNAME), profile (PROFILE), user type (USTYP), user group (CLASS), and lock status (UFLAG) from user master and profile tables, and returns users whose profile or status matches the configured criteria.

This EI serves as an essential control for user access and security by:
- Enabling detection of users with sensitive or powerful profiles (e.g. SAP_ALL, SAP_NEW) that may violate least-privilege or segregation-of-duties policies
- Supporting identification of locked users or users with failed-logon patterns for security and access review
- Providing visibility into last-logon date and duration since last logon for dormant-account and cleanup decisions
- Enabling analysis of user validity period (valid-from/valid-to) for access certification
- Supporting accountability for user profile and lock status across the user base

This monitoring helps organizations detect excessive privileges, locked or dormant users, and validity exceptions. The EI is particularly valuable for access reviews, security audits, and user administration.

The EI uses user profile (UST04), user master (USR02), and optional user data (USR04) to return users and profiles that meet the configured selection and duration criteria.


## Problem Description

Failure to monitor users with sensitive profiles (e.g. SAP_ALL, SAP_NEW) or lock/validity status creates multiple risks across security, compliance, and operations:

**Security and Access Risks**
- Users with SAP_ALL or SAP_NEW may bypass intended segregation of duties and least-privilege controls
- Unmonitored lock status and failed-logon patterns can delay detection of compromised or misused accounts
- Lack of visibility into last-logon and duration since last logon hinders dormant-account cleanup and access certification
- Unset or incorrect validity periods (valid-from/valid-to) can leave access active when it should be restricted

**Compliance and Audit Risks**
- Excessive or sensitive profile assignments without monitoring may lead to audit findings and control deficiencies
- Unmonitored user profile and user type distribution limits evidence for access reviews and certification
- Absence of monitoring delays remediation of segregation-of-duties and sensitive-profile violations

**Operational and Management Risks**
- Management may be unaware of sensitive-profile or lock-status concentrations until an audit or incident
- Unidentified dormant or locked users delay cleanup and support decisions
- Insufficient visibility into user validity and profile assignment undermines accountability for user access

## Suggested Resolution

**Immediate Response**
- Review the users and profiles flagged by the EI to confirm whether sensitive profiles (e.g. SAP_ALL, SAP_NEW) are justified (e.g. basis, admin) or require restriction
- Verify locked users (UFLAG) and failed-logon counts for security or support follow-up
- Check validity period (valid-from/valid-to) for flagged users to ensure alignment with policy
- Identify business context: approved exception, role change, or control gap

**System Assessment**
- Analyze the time window (e.g. last-logon date, default lookback) and scope (profiles, user groups, user types) of the results
- Compare current results to prior periods to identify new sensitive-profile assignments or lock patterns
- Examine distribution by profile, user type, and user group to find concentrations
- Validate the lookback and duration criteria used for the monitoring window

**Corrective Actions**
- Restrict or remove SAP_ALL/SAP_NEW where policy requires least privilege; assign role-based profiles instead
- Unlock or reset users where lock status is erroneous; escalate where lock indicates security concern
- Update validity period (SU01) or user master where valid-from/valid-to is incorrect
- Schedule recurring EI runs and route results to security and access owners for ongoing monitoring
- Use EI output in access certification and profile reviews to prioritize remediation and training


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ANAME | Creator of User Master Record | CHAR | 12 | 0 | XUANAME | BNAME |
| 2 | BCDA1 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 3 | BNAME | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 4 | CLASS | User group | CHAR | 12 | 0 | XUCLASS | XUCLASS |
| 5 | CODV1 | Password Code Vers. | CHAR | 1 | 0 | XUCODEVERS | XUCODEVERS |
| 6 | CODVN | Password Code Vers. | CHAR | 1 | 0 | XUCODEVER2 | XUCODEVER2 |
| 7 | DURATION | Duration(from Last Logon) | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 8 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 9 | ERDAT | Creation Date of User Master | DATS | 8 | 0 | XUERDAT | DATUM |
| 10 | GLTGB | Valid to | DATS | 8 | 0 | XUGLTGB | DATUM |
| 11 | GLTGV | Valid from | DATS | 8 | 0 | XUGLTGV | DATUM |
| 12 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 13 | LOCK_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 14 | LOCNT | Number of failed logon attempts | INT1 | 3 | 0 | XULOCNT | XULOCNT |
| 15 | LTIME | Last Logon Time | TIMS | 6 | 0 | XULTIME | UZEIT |
| 16 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 17 | MODBE | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 18 | MODDA | Modification date | DATS | 8 | 0 | XUMODDATE | DATUM |
| 19 | MODTI | Modification time | TIMS | 6 | 0 | XUMODTIME | UZEIT |
| 20 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 21 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 22 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 23 | NRPRO | Number of profiles or authorizations | INT2 | 5 | 0 | XUNUMBER | XUNUMBER |
| 24 | PROFILE | Profile | CHAR | 12 | 0 | XUPROFILE | XUPROFILE |
| 25 | PWDLGNDATE | Date of Last Password Logon | DATS | 8 | 0 | XULPDAT | DATUM |
| 26 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 27 | STATE_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 28 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 29 | TRDAT | Last Logon Date | DATS | 8 | 0 | XULDATE | DATUM |
| 30 | TZONE | Time Zone | CHAR | 6 | 0 | TZNZONE | TZNZONE |
| 31 | UFLAG | User Lock Status | INT1 | 3 | 0 | XUUFLAG | XUUFLAG |
| 32 | USTYP | User Type | CHAR | 1 | 0 | XUUSTYP | XUUSTYP |
| 33 | VALID_USERS_ONLY | 'X' - Display only valid users |  | 0 | 0 |  |  |
| 34 | VERSN | User master record version | CHAR | 3 | 0 | XUVERSION | XUVERSION |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 34 parameters listed in the Parameters Reference Table above.

**ANAME** (Creator of User Master Record):

Creator of the user master record. The EI uses it when selecting user data.

**BCDA1** (Date of Last Password Change):

Date of last password change. The EI uses it when selecting user data.

**BNAME** (User):

User name. The EI uses it when selecting users from user profile and user master; it identifies each result row.

**CLASS** (User group):

User group. The EI uses it when selecting user master .

**CODV1** (Password Code Vers.):

Password code version. The EI uses it when selecting user data.

**CODVN** (Password Code Vers.):

Password code version (second field). The EI uses it when selecting user data.

**DURATION** (Duration(from Last Logon)):

Length of time between last logon date/time and the evaluation date, in the unit given by DURATION_UNIT. The EI calculates it for each user and uses it to filter and to show how long since last logon.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit in which DURATION is expressed. The EI uses it when computing the time difference between last logon and the evaluation date and when filtering by duration.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** DURATION is the numeric length of time since last logon; DURATION_UNIT is the unit (hours, minutes, days, or full days). The EI uses both to compute and filter by how long since the user last logged on.

**ERDAT** (Creation Date of User Master):

Creation date of the user master record. The EI uses it when selecting user data.

**GLTGB** (Valid to):

User validity end date. The EI uses it when filtering by VALID_USERS_ONLY (only users valid on the evaluation date).

**GLTGV** (Valid from):

User validity start date. The EI uses it when filtering by VALID_USERS_ONLY.

**LANGU** (Language for texts):

Language for descriptions (e.g. lock status text). The EI uses it when resolving texts; default in code: system language.

**LOCK_ICON** (State Icon):

Icon for lock or state. Populated by the EI when building the result row for user lock status.

**LOCNT** (Number of failed logon attempts):

Number of failed logon attempts. The EI uses it when selecting user data.

**LTIME** (Last Logon Time):

Last logon time. The EI uses it when computing duration since last logon and when building the result.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

Indicator to manage dates/times in UTC. The EI uses it when building the evaluation date and time for duration calculation.

**MANAGE_IN_UTC Options:**
- **X**: Manage in UTC
- ** **: Use local time

**MODBE** (Changed By):

User who last changed the user master record. The EI uses it when selecting user data.

**MODDA** (Modification date):

Modification date of the user master record. The EI uses it when selecting user data.

**MODTI** (Modification time):

Modification time of the user master record. The EI uses it when selecting user data.

**NAME_FIRST** (First Name):

First name of the user. Populated by the EI from user master when building the result row.

**NAME_LAST** (Last Name):

Last name of the user. Populated by the EI from user master when building the result row.

**NAME_TEXT** (Full Name):

Full name of the user. Populated by the EI from user master when building the result row.

**NRPRO** (Number of profiles or authorizations):

Number of profiles or authorizations. The EI uses it when selecting user data.

**PROFILE** (Profile):

User profile (e.g. SAP_ALL, SAP_NEW). The EI uses it when selecting users from user profile (UST04); it identifies which profiles are assigned to users.

**PWDLGNDATE** (Date of Last Password Logon):

Date of last password logon. The EI uses it when selecting user data.

**STATE_COLOR** (State Color):

State color (e.g. green/yellow/red for lock status). The EI assigns it from user lock status (UFLAG) and uses it to filter and to show in the result.

**STATE_COLOR Options:**
- **G**: Green (user not locked)
- **Y**: Yellow (domain-specific)
- **R**: Red (user locked)

**STATE_DESC** (SW Message):

Short description of the lock or state. Populated by the EI when building the result row for user lock status.

**STATE_ICON** (State Icon):

Icon for the lock or state. Populated by the EI when building the result row.

**TRDAT** (Last Logon Date):

Last logon date. The EI uses it as the reference date when computing duration since last logon and when building the result; when no date range is supplied the EI uses the lookback window (e.g. BACKDAYS) to populate the date range.

**TZONE** (Time Zone):

Time zone. The EI uses it when selecting user data.

**UFLAG** (User Lock Status):

User lock status (e.g. 0/32/64/128). The EI uses it when selecting user master and when assigning STATE_COLOR (e.g. locked = red, not locked = green).

**UFLAG Options:**
- **0**: Not locked
- **32**, **64**, **128**: Lock indicators (domain-specific)

**USTYP** (User Type):

User type. The EI uses it when selecting user master .

**USTYP Options:**
- **A**: Dialog user
- **B**: System user
- **C**: Communication user
- **S**: Service user
- (other domain values as in USTYP)

**VALID_USERS_ONLY** ('X' - Display only valid users):

Indicator to return only users whose validity period (valid-from to valid-to) includes the evaluation date. The EI uses it to filter out users not valid on the evaluation date.

**VALID_USERS_ONLY Options:**
- **X**: Display only valid users (valid on evaluation date)
- ** **: Display all users (no validity filter)

**VERSN** (User master record version):

User master record version. The EI uses it when selecting user data.


### Parameter Relationships

**Time and duration parameters**

- **TRDAT** (last logon date) and **DURATION** / **DURATION_UNIT** work together: TRDAT is the reference date for each user; the EI computes DURATION as the time between TRDAT (and last logon time) and the evaluation date in the unit given by DURATION_UNIT. The EI then filters by the configured DURATION range.
- When no date range is supplied for TRDAT, the EI uses a default lookback (e.g. BACKDAYS from the selection table) to build the date window; TRDAT is then used to filter users whose last logon falls within that window.

**Profile and user identification**

- **BNAME** (user) and **PROFILE** (profile, e.g. SAP_ALL, SAP_NEW) are central: the EI selects from user profile (UST04) joined to user master (USR02) by BNAME; PROFILE identifies which profile assignment is returned. At least one of PROFILE or BNAME must be supplied for the EI to continue.

**Lock status and state**

- **UFLAG** (user lock status) and **STATE_COLOR** work together: the EI derives STATE_COLOR (e.g. green/red) from UFLAG and uses STATE_COLOR to filter and to show in the result. UFLAG is used when selecting user master; STATE_COLOR is assigned and then filtered by the configured range.

**Validity**

- **VALID_USERS_ONLY** filters by **GLTGV** (valid-from) and **GLTGB** (valid-to): when VALID_USERS_ONLY is set, the EI returns only users whose validity period includes the evaluation date.

**User group and type**

- **CLASS** (user group) and **USTYP** (user type) are used together when selecting user master; the EI uses them to narrow the user set and populates them in the result.


### Default Values

- **LANGU** — Default: system language (SY-LANGU).
- **DURATION_UNIT** — Default: `D` (days).
- When no date range is supplied for TRDAT (last logon date), the EI uses a default lookback (e.g. BACKDAYS = 3000 from the selection table) to build the date window.

**Note:** Parameters that are not supplied remain initial; the EI uses the defaults above for the corresponding logic.

### Practical Configuration Examples

**Use Case 1: Users with SAP_ALL or SAP_NEW – last 90 days**
```
PROFILE = SAP_ALL
PROFILE = SAP_NEW
STATE_COLOR = G R
DURATION_UNIT = D
DURATION = 0 - 90
```
**Purpose:** Find users who have SAP_ALL or SAP_NEW assigned and whose last logon was within the last 90 days (duration 0–90 days), including both active (green) and locked (red) users.

**Use Case 2: Locked users only**
```
STATE_COLOR = R
BNAME = USER01 USER02 USER03
```
**Purpose:** Review lock status for specific users (USER01, USER02, USER03) where state color is red (locked).

**Use Case 3: Sensitive profiles and validity**
```
PROFILE = SAP_ALL
VALID_USERS_ONLY = X
DURATION_UNIT = D
DURATION = 0 - 365
CLASS = 001
```
**Purpose:** List users with SAP_ALL who are valid on the evaluation date and whose last logon was within the last 365 days, restricted to user group 001.

**Use Case 4: Multi-profile and user type**
```
PROFILE = SAP_NEW
USTYP = A
STATE_COLOR = G
DURATION = 0 - 30
TRDAT = (use default lookback)
```
**Purpose:** Dialog users (USTYP = A) with SAP_NEW who are not locked (green) and who logged on within the last 30 days.

**Use Case 5: Dormant users by profile**
```
PROFILE = SAP_ALL
DURATION_UNIT = D
DURATION = 365 - 999999
CLASS = 002
USTYP = A
```
**Purpose:** Identify dialog users in user group 002 with SAP_ALL who have not logged on for at least 365 days (dormant-account review).


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_20_USER_PROFILE | ANAME | Creator of the User Master Record | CHAR(12) | XUANAME |
| /SKN/S_SW_01_20_USER_PROFILE | BCDA1 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_20_USER_PROFILE | BNAME | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_20_USER_PROFILE | CLASS | User group in user master maintenance | CHAR(12) | XUCLASS |
| /SKN/S_SW_01_20_USER_PROFILE | CODV1 | Code Version of Password Hash Algorithm (Old Systems) | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_01_20_USER_PROFILE | CODVN | Code Version of Password Hash Algorithm (New Systems) | CHAR(1) | XUCODEVER2 |
| /SKN/S_SW_01_20_USER_PROFILE | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_20_USER_PROFILE | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_20_USER_PROFILE | ERDAT | Creation Date of the User Master Record | DATS(8) | XUERDAT |
| /SKN/S_SW_01_20_USER_PROFILE | GLTGB | User valid to | DATS(8) | XUGLTGB |
| /SKN/S_SW_01_20_USER_PROFILE | GLTGV | User valid from | DATS(8) | XUGLTGV |
| /SKN/S_SW_01_20_USER_PROFILE | LOCK_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_20_USER_PROFILE | LOCNT | Number of failed logon attempts | INT1(3) | XULOCNT |
| /SKN/S_SW_01_20_USER_PROFILE | LTIME | Last Logon Time | TIMS(6) | XULTIME |
| /SKN/S_SW_01_20_USER_PROFILE | MODBE | Last Changed By | CHAR(12) | XUMODIFIER |
| /SKN/S_SW_01_20_USER_PROFILE | MODDA | Modification date | DATS(8) | XUMODDATE |
| /SKN/S_SW_01_20_USER_PROFILE | MODTI | Modification time | TIMS(6) | XUMODTIME |
| /SKN/S_SW_01_20_USER_PROFILE | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_01_20_USER_PROFILE | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_01_20_USER_PROFILE | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_01_20_USER_PROFILE | NRPRO | Number of profiles or authorizations | INT2(5) | XUNUMBER |
| /SKN/S_SW_01_20_USER_PROFILE | PROFILE | Profile name | CHAR(12) | XUPROFILE |
| /SKN/S_SW_01_20_USER_PROFILE | PWDLGNDATE | Date of Last Password Logon | DATS(8) | XULPDAT |
| /SKN/S_SW_01_20_USER_PROFILE | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_20_USER_PROFILE | STATE_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_20_USER_PROFILE | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_20_USER_PROFILE | TRDAT | Last Logon Date | DATS(8) | XULDATE |
| /SKN/S_SW_01_20_USER_PROFILE | TZONE | Time Zone | CHAR(6) | TZNZONE |
| /SKN/S_SW_01_20_USER_PROFILE | UFLAG | User Lock Status | INT1(3) | XUUFLAG |
| /SKN/S_SW_01_20_USER_PROFILE | USTYP | User Type | CHAR(1) | XUUSTYP |
| /SKN/S_SW_01_20_USER_PROFILE | VERSN | User master record version | CHAR(3) | XUVERSION |

## ABAP Code

`bap
FUNCTION /SKN/F_SW_01_20_USER_PROFILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_01_20_USER_PROFILE
*"----------------------------------------------------------------------
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: MANAGE_IN_UTC       CHAR1 ,
               LANGU               LANGU,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               NO_DATE_RESTRICTION CHAR1,
               VALID_USERS_ONLY    CHAR1. "Display only valid users
  DATA_MULTY: BNAME            XUBNAME,
              PROFILE          XUPROFILE,
                CLASS            XUCLASS,
                USTYP            XUUSTYP,
                UFLAG            XUUFLAG,  " Int 0/32/64/128
                TRDAT            XULDATE,  " Last Logon
                STATE_COLOR      /SKN/E_SW_STATE_COLOR,  " G/Y/R
                DURATION   /SKN/E_SW_DURATION,
                DATUM            SYDATUM . " Paased by SW Online Monitor
  SELECT_MULTY:  BNAME,
                 PROFILE,
                 CLASS,
                 USTYP,
                 UFLAG ,
                 TRDAT ,
                 STATE_COLOR,
                 DURATION,
                 DATUM .
  LV_LANGU = SY-LANGU.
  LV_DURATION_UNIT = 'D'.
  SELECT_SINGLE: LANGU,
                 MANAGE_IN_UTC,
                 DURATION_UNIT,
                 NO_DATE_RESTRICTION,
                 VALID_USERS_ONLY.
  "--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_20_USER_PROFILE'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Clood Mode -----
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA : DATE_FROM TYPE D,
         BACKDAYS  TYPE I.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : LS_DATA LIKE LINE OF T_DATA.
  DATA : TIME_DIFF TYPE I .
  DATA : SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
  "-----------------------------------------------
  " 2. Extracting & Populating Parameters        "
  "-----------------------------------------------
  LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    BACKDAYS = T_SELECT-LOW .
    DATE_FROM = SY-DATUM - BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
    EXIT.
  ENDLOOP.
  IF R_DATUM[] IS INITIAL .  " Set default value
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    BACKDAYS = '3000' .  "--- Default
    DATE_FROM = SY-DATUM - BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF .
*  ENDIF.
  IF R_TRDAT[] IS INITIAL.
    R_TRDAT[] = R_DATUM[] .
  ENDIF.
  IF LV_NO_DATE_RESTRICTION IS NOT INITIAL.
    REFRESH R_TRDAT.
  ENDIF.
  SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
  TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert Data           "
  "-----------------------------------------------
  IF R_PROFILE[] IS INITIAL AND R_BNAME[] IS INITIAL.
    EXIT.  " No continue
  ENDIF.
  SELECT *
     FROM UST04 AS P
      INNER JOIN USR02 AS U
        ON P~BNAME = U~BNAME
      LEFT OUTER JOIN USR04 AS S
        ON P~BNAME = S~BNAME
     INTO CORRESPONDING FIELDS OF TABLE T_DATA
     WHERE P~BNAME IN R_BNAME
       AND P~PROFILE IN R_PROFILE
       AND U~CLASS IN R_CLASS  " User Group
       AND U~USTYP IN R_USTYP  " Uer Type
       AND U~UFLAG IN R_UFLAG.  " Int 0/32/64/128
  "      AND u~TRDAT IN R_TRDAT.  " Last Logon
  SORT T_DATA[] BY BNAME ASCENDING.
  DELETE ADJACENT DUPLICATES FROM T_DATA[] COMPARING BNAME.
  "-----------------------------------------------
  " 5. Post retrieving manipulations             "
  "-----------------------------------------------
  IF LV_VALID_USERS_ONLY IS NOT INITIAL.
    LOOP AT T_DATA INTO LS_DATA.
      SY_TABIX = SY-TABIX .
      IF LS_DATA-GLTGV <= SY-DATUM AND LS_DATA-GLTGB >= SY-DATUM.
      ELSE.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT T_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    CLEAR: LS_DATA-STATE_COLOR,
           LS_DATA-STATE_ICON,
           LS_DATA-STATE_DESC,
           LS_DATA-LOCK_ICON.
    IF LS_DATA-UFLAG IS NOT INITIAL.
      LS_DATA-STATE_COLOR = 'R'.
      LS_DATA-STATE_ICON = STATE_ICON_RED.
      CALL FUNCTION '/SKN/F_SW_01_GET_LOCK_DESC'
        EXPORTING
          UFLAG      = LS_DATA-UFLAG
          LANGU      = LV_LANGU
        IMPORTING
          STATE_DESC = LS_DATA-STATE_DESC.
      LS_DATA-LOCK_ICON = ICON_LOCKED.
    ELSE.
      LS_DATA-STATE_COLOR = 'G'.
      LS_DATA-STATE_ICON = STATE_ICON_GREEN.
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES'
      EXPORTING
        BNAME      = LS_DATA-BNAME
      IMPORTING
        NAME_FIRST = LS_DATA-NAME_FIRST
        NAME_LAST  = LS_DATA-NAME_LAST
        NAME_TEXT  = LS_DATA-NAME_TEXT
      EXCEPTIONS
        NO_DATA    = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
      CLEAR: LS_DATA-NAME_FIRST,
             LS_DATA-NAME_LAST,
             LS_DATA-NAME_TEXT.
    ENDIF.
    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
  "-----------------------------------------------
  " 6. Post retrieving filtering                 "
  "-----------------------------------------------
  DELETE T_DATA WHERE STATE_COLOR NOT IN R_STATE_COLOR.
*-- Fill Duration Value
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX.
    T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = T_DATA-TRDAT
        T_FROM      = T_DATA-LTIME
        D_TO        = SY_DATLO
        T_TO        = SY_TIMLO
        TIME_UNIT   = LV_DURATION_UNIT
      IMPORTING
        TIME_DIFF   = TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      T_DATA-DURATION = TIME_DIFF .
    ELSE.
      T_DATA-DURATION = '999999'.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX .
  ENDLOOP .
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  "-----------------------------------------------
  " 7. Finishing (Set IS_ALERT parameter)        "
  "-----------------------------------------------
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
`
