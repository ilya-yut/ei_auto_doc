# Exception Indicator: List of Inactive users - SW_01_20_NOTACT_USER

## General Overview

This Exception Indicator (EI) monitors SAP user master records to identify inactive users based on last-logon and related time references, while also showing lock status, validity boundaries, and account context. It combines inactivity timing with user-state enrichment so security and operations teams can detect dormant identities that may create unnecessary risk.

This EI serves as an essential control for identity governance and operational security by:

- Enabling early detection of dormant user accounts that should be reviewed, locked, or removed before misuse occurs
- Supporting segmentation of inactive users by type, group, and lock conditions for targeted remediation
- Providing visibility into stale credentials and long-unused access that can violate least-privilege expectations
- Helping teams correlate inactivity findings with provisioning and master-data change context during reviews
- Giving auditors a repeatable, monitor-aligned extract for periodic access recertification and dormant-account control testing

Typical uses include quarterly access hygiene reviews, post-incident account sweeps, pre-go-live cleanup cycles, and recurring deprovisioning assurance checks. Results support prioritization of remediation tickets and evidence retention for compliance activities.

The function evaluates user master data with relative-date logic, enriches rows with status and identity details, computes elapsed inactivity spans, and applies post-processing filters for severity and validity.


## Problem Description

Failure to monitor inactive SAP users and dormant access patterns creates multiple risks across security operations, compliance, and administrative control:

**Security and Access Control Risks**

- Dormant accounts can remain technically usable long after business need has ended
- Stale credentials and inactive identities increase exposure to credential theft and unauthorized reuse
- Inactive technical users may retain privileged capabilities without active ownership
- Lock and validity inconsistencies can mask accounts that should be retired or restricted
- Delayed discovery of long-unused users expands the attack surface in production systems

**Compliance and Audit Risks**

- Regulators and internal audit expect evidence of periodic dormant-account monitoring and cleanup
- Inactive-user populations without structured review weaken access recertification and SoD assurance
- Manual account sweeps are error-prone and difficult to defend under audit scrutiny
- Missing visibility into inactivity timelines reduces confidence in account lifecycle governance
- Late remediation of dormant users can create repeat audit findings and control exceptions

**Management Visibility and Decision-Making Risks**

- Leadership lacks a clear risk picture of inactive-user volume and severity across populations
- Operations teams cannot prioritize remediation effectively without consistent inactivity indicators
- Governance decisions are delayed when stale-account evidence is fragmented across ad hoc reports
- Resource planning for cleanup and deprovisioning becomes reactive rather than proactive
- Cross-team handoffs between security, basis, and process owners slow down without one shared inactive-user view

## Suggested Resolution

**Immediate Response**

- Review flagged inactive users and validate current business need with account owners
- Isolate high-risk dormant accounts and apply temporary restrictions where policy requires
- Confirm lock and validity status for suspicious or long-unused identities
- Open remediation tasks with clear ownership and due dates for each inactive-user cluster
- Record evidence from the monitoring output for incident and governance tracking

**System Assessment**

- Compare inactivity patterns by user type, group, and lock condition against expected baselines
- Analyze recurring stale-account themes tied to onboarding/offboarding or role-management processes
- Correlate high inactivity pockets with recent organizational or system changes
- Validate whether inactivity thresholds and review cadence match current policy and risk appetite
- Document systemic lifecycle gaps that create repeated dormant-account accumulation

**Corrective Actions**

- Deactivate or remove inactive users that no longer have justified business purpose
- Correct validity dates, lock settings, and account classifications for retained users
- Tune monitoring thresholds and schedule with security stakeholders for ongoing dormant-account control
- Train reviewers on consistent interpretation of inactivity, state, and validity indicators
- Integrate recurring findings into formal access-governance and remediation workflows


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACCNT | Account number | CHAR | 12 | 0 | XUACCNT | XUACCNT |
| 2 | ANAME | Creator of User Master Record | CHAR | 12 | 0 | XUANAME | BNAME |
| 3 | BACKDAYS | Days backwards from today |  | 0 | 0 |  |  |
| 4 | BCDA1 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 5 | BNAME | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 6 | CLASS | User group | CHAR | 12 | 0 | XUCLASS | XUCLASS |
| 7 | CODV1 | Password Code Vers. | CHAR | 1 | 0 | XUCODEVERS | XUCODEVERS |
| 8 | CODVN | Password Code Vers. | CHAR | 1 | 0 | XUCODEVER2 | XUCODEVER2 |
| 9 | DURATION | Duration (from last Logon) | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 11 | ERDAT | Creation Date of User Master | DATS | 8 | 0 | XUERDAT | DATUM |
| 12 | GLTGB | Valid to | DATS | 8 | 0 | XUGLTGB | DATUM |
| 13 | GLTGV | Valid from | DATS | 8 | 0 | XUGLTGV | DATUM |
| 14 | INIT_PWD_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 15 | LANGU | Description Language |  | 0 | 0 |  |  |
| 16 | LOCK_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 17 | LOCNT | Number of failed logon attempts | INT1 | 3 | 0 | XULOCNT | XULOCNT |
| 18 | LTIME | Last Logon Time | TIMS | 6 | 0 | XULTIME | UZEIT |
| 19 | MODBE | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 20 | MODDATE | Modification date | DATS | 8 | 0 | XUMODDATE | DATUM |
| 21 | MODIFIER | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 22 | MODTIME | Modification time | TIMS | 6 | 0 | XUMODTIME | UZEIT |
| 23 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 24 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 25 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 26 | NO_DATE_RESTRICTION | 'X' - No restriction |  | 0 | 0 |  |  |
| 27 | PWDLGNDATE | Date of Last Password Logon | DATS | 8 | 0 | XULPDAT | DATUM |
| 28 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 29 | STATE_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 30 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 31 | TRDAT | Last Logon Date | DATS | 8 | 0 | XULDATE | DATUM |
| 32 | TZONE | Time Zone | CHAR | 6 | 0 | TZNZONE | TZNZONE |
| 33 | UFLAG | User Lock Status | INT1 | 3 | 0 | XUUFLAG | XUUFLAG |
| 34 | USTYP | User Type | CHAR | 1 | 0 | XUUSTYP | XUUSTYP |
| 35 | VALID_USERS_ONLY | 'X' - Display only valid users |  | 0 | 0 |  |  |
| 36 | VERSN | User master record version | CHAR | 3 | 0 | XUVERSION | XUVERSION |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 36 parameters listed in the Parameters Reference Table above.

**ACCNT** (Account number):

Account identifier on user master rows; supports tying inactive-user findings to account context.

**ANAME** (Creator of User Master Record):

Creator of the user master record; helps determine who provisioned users that are now inactive or stale.

**BACKDAYS** (Days backwards from today):

Defines how far back from the evaluation day the default relative window starts when no explicit date range is supplied.

**BCDA1** (Date of Last Password Change):

Date of last password change; useful for identifying inactive users with stale credentials.

**BNAME** (User):

Technical user name; key filter for targeting known inactive-account candidates.

**CLASS** (User group):

User group used to segment inactive-user reviews by organizational or technical grouping.

**CODV1** (Password Code Vers.):

Legacy password-hash version indicator for compatibility-era systems.

**CODVN** (Password Code Vers.):

Current password-hash version indicator for modern systems.

**DURATION** (Duration (from last Logon)):

Numeric threshold applied after elapsed-time calculation to retain users whose inactivity span matches policy.

**DURATION and DURATION_UNIT Connection:**

**DURATION** selects rows by computed inactivity span; **DURATION_UNIT** defines whether that span is measured in hours, minutes, days, or full calendar-day slices.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit used for elapsed inactivity calculations (hours, minutes, days, or full-day slices).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**ERDAT** (Creation Date of User Master):

User master creation date; helps identify long-lived inactive accounts.

**GLTGB** (Valid to):

Valid-to date; surfaces users that may be expired yet still present in inactive-user populations.

**GLTGV** (Valid from):

Valid-from date; helps detect users with unusual validity timelines.

**INIT_PWD_ICON** (State Icon):

Presentation icon for initial-password state on output rows.

**INIT_PWD_ICON Options:**
- **R** — Icon for red-severity mapping.
- **Y** — Icon for yellow-severity mapping.
- **G** — Icon for green-severity mapping.

**LANGU** (Description Language):

Language key for resolved text descriptions in output.

**LOCK_ICON** (State Icon):

Presentation icon indicating lock-related state.

**LOCK_ICON Options:**
- **R** — Icon for red-severity mapping.
- **Y** — Icon for yellow-severity mapping.
- **G** — Icon for green-severity mapping.

**LOCNT** (Number of failed logon attempts):

Failed logon counter used to identify inactive users with repeated login issues.

**LTIME** (Last Logon Time):

Last logon time, paired with logon date for inactivity computation.

**MODBE** (Changed By):

Last changer on user master; useful for auditing updates to inactive users.

**MODDATE** (Modification date):

Last modification date from detail enrichment for stale-account tracking.

**MODIFIER** (Changed By):

Last modifier user from detail enrichment.

**MODTIME** (Modification time):

Last modification time from detail enrichment.

**NAME_FIRST** (First Name):

Resolved first name for readable inactive-user investigations.

**NAME_LAST** (Last Name):

Resolved last name, paired with first name for human-readable review.

**NAME_FIRST and NAME_LAST Connection:**

Together they provide readable identity context while **BNAME** remains the technical key.

**NAME_TEXT** (Full Name):

Resolved full name used in dashboards and reports.

**NO_DATE_RESTRICTION** ('X' - No restriction):

When set, clears the built date window so inactive-user review is not restricted by relative-date logic.

**NO_DATE_RESTRICTION Options:**
- **X** — Apply the flag as described in the parameter name.
- ** ** (space) — Leave the flag unset for this run.

**PWDLGNDATE** (Date of Last Password Logon):

Date of last password logon, distinct from password-change date.

**STATE_COLOR** (State Color):

Severity band (red/yellow/green) for inactive-user status presentation.

**STATE_COLOR Options:**
- **R** — Red band: critical finding.
- **Y** — Yellow band: warning state.
- **G** — Green band: expected state.

**STATE_DESC** (SW Message):

Resolved state description text for each row.

**STATE_ICON** (State Icon):

Resolved primary status icon for each row.

**STATE_ICON Options:**
- **R** — Icon for red-severity mapping.
- **Y** — Icon for yellow-severity mapping.
- **G** — Icon for green-severity mapping.

**TRDAT** (Last Logon Date):

Last logon date, central to inactive-user determination and elapsed-time calculation.

**TZONE** (Time Zone):

User time zone to interpret timestamp fields consistently.

**UFLAG** (User Lock Status):

User lock-status bitmap used to classify lock conditions.

**UFLAG Options:**
- **0** — No lock flags asserted.
- **32** — Password lock state.
- **64** — Administrative lock state.
- **128** — Additional lock level (where applicable).

**USTYP** (User Type):

User type (dialog/system/communication/etc.) for segmented inactive-user analysis.

**VALID_USERS_ONLY** ('X' - Display only valid users):

When set, keeps only currently valid users by validity dates.

**VALID_USERS_ONLY Options:**
- **X** — Apply the flag as described in the parameter name.
- ** ** (space) — Leave the flag unset for this run.

**VERSN** (User master record version):

User master version counter to detect unusual churn among inactive users.


### Parameter Relationships

**Relative date window and inactivity scope**

- **BACKDAYS** defines the default backward window when explicit date ranges are not supplied.
- **NO_DATE_RESTRICTION** clears that constructed date window when unrestricted listing is intentionally required.

**Elapsed inactivity filtering**

- **TRDAT** and **LTIME** provide key reference values for inactivity computation on each user row.
- **DURATION** and **DURATION_UNIT** must be configured together: unit defines elapsed-time semantics and duration selection keeps matching rows.

**Population and state segmentation**

- **BNAME**, **CLASS**, **USTYP**, and **ACCNT** shape which user population is reviewed.
- **UFLAG** and **STATE_COLOR** focus monitoring on lock/severity subsets.
- **VALID_USERS_ONLY** applies validity-window filtering so only currently valid users remain.

**Credential and recency context**

- **PWDLGNDATE** and **BCDA1** complement last-logon fields by showing password-use and password-change recency.

**Readability and accountability**

- **NAME_FIRST**, **NAME_LAST**, and **NAME_TEXT** provide human-readable identity context for **BNAME**.
- **ANAME** and **MODBE** provide creator/changer accountability context alongside inactivity findings.


### Default Values

- **LANGU** — Default: session logon language (`SY-LANGU`) before caller selections are read.
- **DURATION_UNIT** — Default: `D` (days), assigned in code before caller selections are read.
- **BACKDAYS** — Default: `3000` when no date-range selection is supplied and date restriction remains active.

**Note:** When **NO_DATE_RESTRICTION** is set, the constructed default date range is cleared.

### Practical Configuration Examples

**Use Case 1: Quarterly dormant-account review**

```
BACKDAYS = 90
DURATION = 60
DURATION_UNIT = D
```

**Purpose:** Focuses on users inactive for at least two months within a quarter-scale review window.

**Use Case 2: Full-day inactivity and valid-user focus**

```
DURATION = 30
DURATION_UNIT = F
VALID_USERS_ONLY = X
```

**Purpose:** Targets currently valid users with prolonged inactivity measured in full-day units for cleanup decisions.

**Use Case 3: Locked inactive technical users**

```
USTYP = B
UFLAG = 64
STATE_COLOR = R
```

**Purpose:** Highlights technical users in administrative-lock states with critical severity for high-priority remediation.

**Use Case 4: Creator-scoped inactivity check**

```
ANAME = BATCH_CREATE
BACKDAYS = 180
CLASS = *
```

**Purpose:** Reviews inactive users originally created by a specific provisioning identity over a six-month horizon.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_20_USER_STATE | ACCNT | Account ID | CHAR(12) | XUACCNT |
| /SKN/S_SW_01_20_USER_STATE | ANAME | Creator of the User Master Record | CHAR(12) | XUANAME |
| /SKN/S_SW_01_20_USER_STATE | BCDA1 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_20_USER_STATE | BNAME | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_20_USER_STATE | CLASS | User group in user master maintenance | CHAR(12) | XUCLASS |
| /SKN/S_SW_01_20_USER_STATE | CODV1 | Code Version of Password Hash Algorithm (Old Systems) | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_01_20_USER_STATE | CODVN | Code Version of Password Hash Algorithm (New Systems) | CHAR(1) | XUCODEVER2 |
| /SKN/S_SW_01_20_USER_STATE | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_20_USER_STATE | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_20_USER_STATE | ERDAT | Creation Date of the User Master Record | DATS(8) | XUERDAT |
| /SKN/S_SW_01_20_USER_STATE | GLTGB | User valid to | DATS(8) | XUGLTGB |
| /SKN/S_SW_01_20_USER_STATE | GLTGV | User valid from | DATS(8) | XUGLTGV |
| /SKN/S_SW_01_20_USER_STATE | INIT_PWD_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_20_USER_STATE | LOCK_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_20_USER_STATE | LOCNT | Number of failed logon attempts | INT1(3) | XULOCNT |
| /SKN/S_SW_01_20_USER_STATE | LTIME | Last Logon Time | TIMS(6) | XULTIME |
| /SKN/S_SW_01_20_USER_STATE | MODBE | Last Changed By | CHAR(12) | XUMODIFIER |
| /SKN/S_SW_01_20_USER_STATE | MODDATE | Modification date | DATS(8) | XUMODDATE |
| /SKN/S_SW_01_20_USER_STATE | MODIFIER | Last Changed By | CHAR(12) | XUMODIFIER |
| /SKN/S_SW_01_20_USER_STATE | MODTIME | Modification time | TIMS(6) | XUMODTIME |
| /SKN/S_SW_01_20_USER_STATE | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_01_20_USER_STATE | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_01_20_USER_STATE | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_01_20_USER_STATE | PWDLGNDATE | Date of Last Password Logon | DATS(8) | XULPDAT |
| /SKN/S_SW_01_20_USER_STATE | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_20_USER_STATE | STATE_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_20_USER_STATE | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_20_USER_STATE | TRDAT | Last Logon Date | DATS(8) | XULDATE |
| /SKN/S_SW_01_20_USER_STATE | TZONE | Time Zone | CHAR(6) | TZNZONE |
| /SKN/S_SW_01_20_USER_STATE | UFLAG | User Lock Status | INT1(3) | XUUFLAG |
| /SKN/S_SW_01_20_USER_STATE | USTYP | User Type | CHAR(1) | XUUSTYP |
| /SKN/S_SW_01_20_USER_STATE | VERSN | User master record version | CHAR(3) | XUVERSION |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_20_USER_STATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_01_20_USER_STATE
*"----------------------------------------------------------------------
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: MANAGE_IN_UTC       CHAR1 ,
               LANGU               LANGU,
               DURATION_UNIT       /SKN/E_SW_DURATION_UNIT,
               NO_DATE_RESTRICTION CHAR1,
               VALID_USERS_ONLY    CHAR1, "Display only valid users
               DATE_REF_FLD        NAME_FELD.
  DATA_MULTY: BNAME            XUBNAME,
                CLASS            XUCLASS,
                USTYP            XUUSTYP,
                UFLAG            XUUFLAG,  " Int 0/32/64/128
                TRDAT            XULDATE,  " Last Logon
                ERDAT            XUERDAT,   "Creation Date of the User Master Record
                ANAME            XUANAME,   "Creator of the User Master Record
                PWDLGNDATE       XUERDAT, "Date of Last Password Logon
                PWDSETDATE       XUERDAT, "Date: Password Reset by Administrator
                PWDLOCKDATE      XUERDAT, "Date: Setting of Password Lock
                STATE_COLOR      /SKN/E_SW_STATE_COLOR,  " G/Y/R
                DURATION         /SKN/E_SW_DURATION,
                DATUM            SYDATUM , " Paased by SW Online Monitor
                LOCNT             XULOCNT, ""9-8-16
                MODBE	            XUMODIFIER, "Changed By 12-9-16
                ACCNT	            XUACCNT. "Account ID
  SELECT_MULTY:  BNAME,
                 CLASS,
                 USTYP,
                 UFLAG ,
                 TRDAT ,
                 ERDAT ,   "Creation Date of the User Master Record
                 PWDLGNDATE, "Date of Last Password Logon
                 PWDSETDATE, "Date: Password Reset by Administrator
                 PWDLOCKDATE, "Date: Setting of Password Lock
                 ANAME,
                 STATE_COLOR,
                 DURATION,
                 DATUM ,
                 LOCNT, "9-8-16
                 MODBE,
                 ACCNT.
  LV_LANGU = SY-LANGU.
  LV_DURATION_UNIT = 'D'.
  SELECT_SINGLE: LANGU,
                 MANAGE_IN_UTC,
                 DURATION_UNIT,
                 NO_DATE_RESTRICTION,
                 VALID_USERS_ONLY,
                 DATE_REF_FLD.
  "--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_20_USER_STATE'
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
  DATA : REF_DATE LIKE SY-DATLO ,
         REF_TIME LIKE SY-TIMLO .
  DATA: C_FUNCTION    TYPE           RS38L_FNAM VALUE 'BAPI_USER_GET_DETAIL',
        FUNC_EXISTS   TYPE           FLAG,
        LV_GROUP      TYPE           RS38L_AREA,
        LV_INCLUDE    TYPE           PROGNAME,
        LV_NAMESPACE  TYPE           NAMESPACE,
        LV_STR_AREA   TYPE           AREA,
        LT_RETURN     TYPE TABLE OF  BAPIRET2,
        LOGONDATA     LIKE           BAPILOGOND,
        DEFAULTS      LIKE           BAPIDEFAUL,
        ADDRESS       LIKE           BAPIADDR3,
        COMPANY       LIKE           BAPIUSCOMP,
        UCLASS        TYPE           BAPIUCLASS,
        LASTMODIFIED  TYPE           BAPIMODDAT,
        ISLOCKED      TYPE           BAPISLOCKD,
        IDENTITY      TYPE           BAPIIDENTITY,
        ADMINDATA     TYPE           BAPIUSERADMIN
.
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
  "-- 15/02/2015
  IF LV_NO_DATE_RESTRICTION IS NOT INITIAL.
    REFRESH R_DATUM.
  ENDIF.
**************** SET DATE REFERENCE FIELD ************************
  "-- 15/02/2015
  IF LV_DATE_REF_FLD IS NOT INITIAL.
    CASE LV_DATE_REF_FLD.
      WHEN 'ERDAT'.
        R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
      WHEN 'TRDAT'.
        R_TRDAT[] = R_DATUM[]. "" Last Logon
      WHEN 'PWDLGNDATE'.
        R_PWDLGNDATE[] = R_DATUM[]. "Date of Last Password Logon
      WHEN 'PWDSETDATE'.
        R_PWDSETDATE[] = R_DATUM[]. "Date: Password Reset by Administrator
      WHEN 'PWDLOCKDATE'.
        R_PWDLOCKDATE[] = R_DATUM[]. "Date: Setting of Password Lock
      WHEN OTHERS.
        R_TRDAT[] = R_DATUM[]. "Last Logon
    ENDCASE.
  ENDIF.
  IF R_TRDAT[] IS INITIAL AND R_ERDAT[] IS INITIAL.
    R_TRDAT[] = R_DATUM[] .
  ENDIF.
*************************************************************************
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
*  SELECT *
*     FROM USR02
*     INTO CORRESPONDING FIELDS OF TABLE T_DATA
*     WHERE BNAME IN R_BNAME
*       AND CLASS IN R_CLASS  " User Group
*       AND USTYP IN R_USTYP  " User Type
*       AND UFLAG IN R_UFLAG  " Int 0/32/64/128
*       AND TRDAT IN R_TRDAT  " Last Logon
*       AND ERDAT IN R_ERDAT  "Creation date
*       AND ANAME IN R_ANAME  " Creator
*       AND LOCNT IN R_LOCNT. "" Number of failed logon attempts "9-8-16
  "****8-9-16************************
  SELECT *
   FROM USR02 AS A
   LEFT  OUTER JOIN USR04 AS B
    ON A~BNAME = B~BNAME
   INTO CORRESPONDING FIELDS OF TABLE T_DATA
   WHERE A~BNAME IN R_BNAME
     AND A~CLASS IN R_CLASS  " User Group
     AND A~USTYP IN R_USTYP  " User Type
     AND A~UFLAG IN R_UFLAG  " Int 0/32/64/128
     AND A~TRDAT IN R_TRDAT  " Last Logon
     AND A~PWDLGNDATE IN R_PWDLGNDATE "Date of Last Password Logon
     AND A~PWDSETDATE IN R_PWDSETDATE "Date: Password Reset by Administrator
     AND A~PWDLOCKDATE IN R_PWDLOCKDATE "Date: Setting of Password Lock
     AND A~ERDAT IN R_ERDAT  "Creation date
     AND A~ANAME IN R_ANAME  " Creator
     AND A~LOCNT IN R_LOCNT  "" Number of failed logon attempts "9-8-16
     AND A~ACCNT IN R_ACCNT. """Account ID 05-06-17
  DELETE T_DATA WHERE MODBE NOT IN R_MODBE. "  12-9-16
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      FUNCNAME           = C_FUNCTION     " Name of Function Module
    IMPORTING
      GROUP              = LV_GROUP        " Name of function group
      INCLUDE            = LV_INCLUDE      " Name of include
      NAMESPACE          = LV_NAMESPACE    " Namespace
      STR_AREA           = LV_STR_AREA     " Name of function group without namespace
    EXCEPTIONS
      FUNCTION_NOT_EXIST = 1
      OTHERS             = 2.
  IF SY-SUBRC EQ 0.
    FUNC_EXISTS = 'X'.
  ENDIF.
  "-----------------------------------------------
  " 5. Post retrieving manipulations             "
  "-----------------------------------------------
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
    LS_DATA-INIT_PWD_ICON = ICON_INIT_PWD.
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
    IF FUNC_EXISTS IS NOT INITIAL.
      CLEAR: LOGONDATA, DEFAULTS, ADDRESS, COMPANY
             , UCLASS, LASTMODIFIED, ISLOCKED
             , IDENTITY, ADMINDATA, LT_RETURN.
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          USERNAME     = LS_DATA-BNAME    " User Name
*         cache_results  = 'X'    " Temporarily buffer results in work process
        IMPORTING
          LOGONDATA    = LOGONDATA
          DEFAULTS     = DEFAULTS
          ADDRESS      = ADDRESS
          COMPANY      = COMPANY
          UCLASS       = UCLASS
          LASTMODIFIED = LASTMODIFIED
          ISLOCKED     = ISLOCKED
          IDENTITY     = IDENTITY
          ADMINDATA    = ADMINDATA
        TABLES
          RETURN       = LT_RETURN.
      IF LT_RETURN IS INITIAL.
        LS_DATA-MODDATE = LASTMODIFIED-MODDATE.
        LS_DATA-MODTIME = LASTMODIFIED-MODTIME.
        LS_DATA-MODIFIER = LASTMODIFIED-MODIFIER.
      ENDIF.
    ENDIF.
    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
  "-----------------------------------------------
  " 6. Post retrieving filtering                 "
  "-----------------------------------------------
  DELETE T_DATA WHERE STATE_COLOR NOT IN R_STATE_COLOR.
  LOOP AT T_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    REF_DATE = LS_DATA-TRDAT.
    REF_TIME = LS_DATA-LTIME.
    "-- 14/05/2015
    IF LV_DATE_REF_FLD = 'ERDAT' OR LS_DATA-TRDAT IS INITIAL. " For New user with empty Log on date
      REF_DATE = LS_DATA-ERDAT. "Date on Which Record Was Created
      REF_TIME = SY_TIMLO.
    ELSE.
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = REF_DATE
        T_FROM      = REF_TIME
        D_TO        = SY_DATLO
        T_TO        = SY_TIMLO
        TIME_UNIT   = LV_DURATION_UNIT
      IMPORTING
        TIME_DIFF   = TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      LS_DATA-DURATION = TIME_DIFF .
    ELSE.
      LS_DATA-DURATION = '999999' .
    ENDIF.
    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  IF LV_VALID_USERS_ONLY IS NOT INITIAL.
    LOOP AT T_DATA INTO LS_DATA.
      SY_TABIX = SY-TABIX .
      IF LS_DATA-GLTGV <= SY-DATUM AND LS_DATA-GLTGB >= SY-DATUM.
      ELSE.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.
  "-----------------------------------------------
  " 7. Finishing (Set IS_ALERT parameter)        "
  "-----------------------------------------------
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
