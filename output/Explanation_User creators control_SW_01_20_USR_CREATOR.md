# Exception Indicator: User creators control - SW_01_20_USR_CREATOR

## General Overview

This Exception Indicator (EI) monitors SAP user master records with emphasis on **who originally created each account** (the creator on the user master), alongside lock state, activity, and validity. It surfaces users provisioned by specific technical identities so security and governance teams can enforce creator accountability, detect unusual onboarding patterns, and review dormant or risky accounts in the context of their source.

This EI serves as an essential control for identity governance and SAP security operations by:

- Enabling detection of users introduced by non-standard or shared provisioning accounts before those assignments create long-term risk
- Supporting segregation-of-duties and least-privilege programs when creator IDs must be justified and periodically recertified
- Providing visibility into inactive, locked, or validity-challenged users grouped by their originating creator for prioritized cleanup
- Helping teams correlate provisioning behavior with password, logon, and modification signals without manually joining multiple tools
- Giving auditors a repeatable, monitor-aligned extract that matches the same user-master logic used in online exception handling

Typical uses include creator attestation after reorganizations, investigations when a provisioning account is suspected of misuse, periodic reviews of service or batch users by source, and operational checks before role redesign or decommissioning. Results support ticketing, access revocation, and master-data correction.

The evaluation reads the central user security repository together with related assignment data where applicable, enriches rows with names and state cues, and applies post-processing time-span screening after retrieval consistent with the monitoring configuration.


## Problem Description

Failure to monitor SAP user masters with clear accountability for **who created each account** creates multiple risks across security, compliance, and operations:

**Security and Access Control Risks**

- Provisioning identities that create many users without oversight can become single points of abuse or compromise
- Dormant or misconfigured users from the same source are hard to prioritize when creator context is missing from monitoring
- Lock and validity anomalies spread across the population cannot be tied back to onboarding behavior without structured extracts
- Shared or emergency accounts used as creators may leave no durable ownership trail for later review
- Technical and dialog populations mixed in one view obscure whether creator patterns differ by user type

**Compliance and Audit Risks**

- Access and identity audits increasingly expect evidence of **provisioning accountability**, not only current role assignments
- Recertification without creator lineage weakens arguments that onboarding was controlled and traceable
- Manual spreadsheets to reconstruct creators are error-prone and rarely repeatable on a schedule regulators expect
- Inability to segment exceptions by source slows responses to findings on specific provisioning teams or tools
- Late discovery of toxic combinations (for example many sensitive users from one unexpected creator) undermines timeliness of remediation evidence

**Management Visibility and Decision-Making Risks**

- Leadership lacks a clear picture of which provisioning paths or accounts feed the largest share of production users
- Operations cannot easily rank “which creator-owned backlog to clear first” during cleanup programs
- Project teams underestimate migration effort when creator-based debt is invisible until cutover
- Cross-functional disputes between security, basis, and process owners drag on without a shared, monitor-backed user list
- Month-end or quarter-end governance cycles extend when creator-related exceptions surface only through ad hoc queries

## Suggested Resolution

**Immediate Response**

- Review flagged users grouped by provisioning source and validate each batch against approved onboarding procedures and ownership
- For unexpected or high-risk sources, confirm whether the creator ID is still legitimate, dedicated, and appropriately locked down
- Escalate suspected misuse of provisioning rights according to your privileged-access and incident playbooks
- Open structured remediation tasks with clear owners when creator accounts or their products must be corrected
- Where authorized, adjust user master data, validity, or locks using standard administrative transactions after approval

**System Assessment**

- Compare current and prior monitoring cycles to see whether a creator’s footprint is growing faster than headcount or projects justify
- Segment by user group and user type to see whether creator patterns differ between dialog and technical populations
- Correlate spikes with change windows, transports, or integrations that might explain bulk creation events
- Validate organizational standards for creator IDs (naming, exclusivity, lifecycle) still match how monitoring is configured
- Document systemic provisioning defects rather than treating each user line as an unrelated one-off

**Corrective Actions**

- Rotate or retire shared creators; enforce dedicated provisioning identities with logging and periodic review
- Deactivate or correct users that should not have been created; align validity and group assignments with policy
- Tune monitoring scope and schedules with security stakeholders so creator-centric reviews run on an agreed cadence
- Brief reviewers on how to interpret state, severity, and name columns together with technical user keys
- Retain evidence of review and tie results into GRC or ticketing so creator drift does not recur unnoticed


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
| 9 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
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

Account identifier on the user master row—supports tying technical users to financial or account context in creator reviews.

**ANAME** (Creator of User Master Record):

Creator of the user master record—the primary lever for this EI: restrict to users originally provisioned by specific technical identities to enforce creator accountability and detect rogue onboarding.

**BACKDAYS** (Days backwards from today):

How many calendar days back from the evaluation anchor form the default lower bound for the relative date window when no explicit range is supplied via selection.

**BCDA1** (Date of Last Password Change):

Date of the last password change—supports credential-hygiene checks on users tied to a given creator.

**BNAME** (User):

Technical user name—targets or excludes individual accounts while creator-centric filters remain in scope.

**CLASS** (User group):

User group—segments batch, dialog, and other populations so creator reviews stay comparable within each segment.

**CODV1** (Password Code Vers.):

Legacy password hash algorithm version—relevant when cryptographic posture must be compared on older stacks.

**CODVN** (Password Code Vers.):

Current password hash algorithm version—pairs with **CODV1** on mixed-release landscapes.

**DURATION** (Duration In Time Units):

After retrieval, keeps rows whose computed inactivity span (from the active reference timestamp on the user row through **DURATION_UNIT**) matches your monitoring thresholds.

**DURATION and DURATION_UNIT Connection:**

**DURATION** selects rows by computed span; **DURATION_UNIT** defines whether that span is hours, minutes, days, or full calendar days. Configure both so reviewers interpret one consistent window.

**DURATION_UNIT** (Duration Unit):

Unit for measuring elapsed time for the post-processing span (hours, minutes, days, or full-day slices).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**ERDAT** (Creation Date of User Master):

User master creation date—supports onboarding-age and “who created when” analysis alongside **ANAME**.

**GLTGB** (Valid to):

Valid-to date—surfaces users who should already be expired but still appear in creator-based extracts.

**GLTGV** (Valid from):

Valid-from date—supports detection of back-dated or prematurely active accounts from a given creator.

**INIT_PWD_ICON** (State Icon):

Icon token for initial-password presentation on the row after the standard icon resolution step.

**INIT_PWD_ICON Options:**
- **R** — Icon slot for red-severity resolution.
- **Y** — Icon slot for yellow-severity resolution.
- **G** — Icon slot for green-severity resolution.

**LANGU** (Description Language):

Language key for lock descriptions and other resolved texts on each output line.

**LOCK_ICON** (State Icon):

Icon token for lock-related state for quick visual triage in monitoring consoles.

**LOCK_ICON Options:**
- **R** — Icon slot for red-severity resolution.
- **Y** — Icon slot for yellow-severity resolution.
- **G** — Icon slot for green-severity resolution.

**LOCNT** (Number of failed logon attempts):

Failed logon attempt counter—supports brute-force or mis-configuration detection on users from targeted creators.

**LTIME** (Last Logon Time):

Last logon time—pairs with **TRDAT** for sub-day inactivity rules.

**MODBE** (Changed By):

User who last changed the master record—after retrieval, rows can be dropped when the changer is outside the supplied range (per the function logic).

**MODDATE** (Modification date):

Last modification date from user detail when available—supports “dormant since” narratives.

**MODIFIER** (Changed By):

Last-changed-by user from detail resolution—complements **MODBE** filtering context.

**MODTIME** (Modification time):

Time of last modification—refines **MODDATE** for same-day edit storms.

**NAME_FIRST** (First Name):

Given name resolved for the user—helps business readers map creator-scoped technical IDs to people.

**NAME_LAST** (Last Name):

Family name resolved for the user—pairs with **NAME_FIRST** for directory-style reading.

**NAME_FIRST and NAME_LAST Connection:**

Together they identify the person behind **BNAME** while creator filters on **ANAME** stay in force.

**NAME_TEXT** (Full Name):

Single formatted full-name column for dashboards.

**NO_DATE_RESTRICTION** ('X' - No restriction):

When set, clears the built relative date range so selection is not limited by that window—use only when policy allows.

**NO_DATE_RESTRICTION Options:**
- **X** — Apply the flag as described in the parameter name.
- ** ** (space) — Leave the flag unset for this run.

**PWDLGNDATE** (Date of Last Password Logon):

Date of last password logon—distinct from password change date for dormant-credential policies.

**STATE_COLOR** (State Color):

Severity color band (red/yellow/green) after lock and state resolution.

**STATE_COLOR Options:**
- **R** — Red band: critical user-state finding.
- **Y** — Yellow band: warning requiring review.
- **G** — Green band: within expected policy envelope.

**STATE_DESC** (SW Message):

Short text describing the derived state for the row.

**STATE_ICON** (State Icon):

Primary state icon token after severity resolution.

**STATE_ICON Options:**
- **R** — Icon slot for red-severity resolution.
- **Y** — Icon slot for yellow-severity resolution.
- **G** — Icon slot for green-severity resolution.

**TRDAT** (Last Logon Date):

Last logon date—core for inactivity analysis; when empty, the duration step can fall back to creation-style references in the processing path.

**TZONE** (Time Zone):

User time zone—aligns logon and validity interpretation globally.

**UFLAG** (User Lock Status):

Lock status bitmap—administrative versus password lock semantics per SAP user master evaluation.

**UFLAG Options:**
- **0** — No administrative lock flags asserted in the evaluated band.
- **32** — Password-related lock state per user master semantics.
- **64** — Administrative lock state per user master semantics.
- **128** — Additional lock level when present in your system’s user master evaluation.

**USTYP** (User Type):

User type (dialog, system, communication, etc.)—separates human from technical consumers in creator-based lists.

**VALID_USERS_ONLY** ('X' - Display only valid users):

When set, retains only users whose validity window covers the evaluation day.

**VALID_USERS_ONLY Options:**
- **X** — Apply the flag as described in the parameter name.
- ** ** (space) — Leave the flag unset for this run.

**VERSN** (User master record version):

User master version counter—highlights unexpected churn among users from monitored creators.


### Parameter Relationships

**Creator-centric scope**

- **ANAME** is the defining dimension for this EI: it restricts the extract to users whose user master **creator** field matches the supplied range—use it to audit or attest specific provisioning identities.
- **BNAME**, **CLASS**, **USTYP**, **UFLAG**, **ACCNT**, and related multi-filters combine conjunctively on the user master read so creator reviews can still target populations, user types, and lock posture.

**Relative window and unrestricted mode**

- **BACKDAYS** establishes how far back the default lower bound reaches for the relative date window when callers supply it through the standard selection mechanism.
- **NO_DATE_RESTRICTION** clears that built date range when policy allows listing users without the relative window filter.

**Activity and post-processing span**

- **DURATION** and **DURATION_UNIT** apply after rows are retrieved: the unit governs how elapsed time is measured from the reference timestamps on each user row, and the selection retains rows whose computed value fits your thresholds.

**Validity and presentation**

- **VALID_USERS_ONLY** removes users outside their validity window after the main select.
- **LANGU** drives language-dependent lock descriptions; **STATE_COLOR**, **STATE_ICON**, **STATE_DESC**, **LOCK_ICON**, and **INIT_PWD_ICON** should be read together when interpreting severity and lock context on a line.

**Changer versus creator**

- **MODBE** filters which rows remain after retrieval based on the last-changed-by user on the master (per the function’s delete step), distinct from **ANAME**, which reflects the original creator.


### Default Values

- **LANGU** — Default: session logon language (assigned in code before reading caller selections when not overridden).
- **DURATION_UNIT** — Default: `D` (days) when not supplied via selection.
- **BACKDAYS** — Default: `3000` when no date range is supplied through the selection table and date restriction is still in effect, establishing a wide backward window for the relative filter.

**Note:** When **NO_DATE_RESTRICTION** is set, the built date range is cleared, so the default **BACKDAYS** behavior does not apply in that mode.

### Practical Configuration Examples

**Use Case 1: Users created by one provisioning account**

```
ANAME = SVC_USER_ADMIN
BACKDAYS = 90
```

**Purpose:** Lists users whose master record shows that provisioning account as creator within roughly the last quarter—typical for attesting a known service identity.

**Use Case 2: Dialog users from a creator with validity filter**

```
ANAME = BATCH_ONBOARD
USTYP = A
VALID_USERS_ONLY = X
```

**Purpose:** Focuses on currently valid dialog users attributed to a batch-style creator—useful when separating human from technical onboarding sources.

**Use Case 3: Inactivity window with full-day unit**

```
ANAME = *
DURATION = 30
DURATION_UNIT = F
```

**Purpose:** Reviews users from any creator whose post-processed inactivity span matches thirty full-day units—helpful for dormant-user programs tied to creator attestation.

**Use Case 4: Creator plus user group slice**

```
ANAME = EMERGENCY_ADMIN
CLASS = PROJECT_X
STATE_COLOR = R
```

**Purpose:** Surfaces users from an emergency provisioning ID in a specific group that already show a critical severity band—good for incident follow-up.


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
