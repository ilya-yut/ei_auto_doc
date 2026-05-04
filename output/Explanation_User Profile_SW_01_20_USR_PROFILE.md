# Exception Indicator: User Profile - SW_01_20_USR_PROFILE

## General Overview

This Exception Indicator (EI) monitors assignments of SAP authorization profiles to users and enriches each hit with user master context, lock state, and human-readable identity fields. It combines profile-to-user relationships with user security data so governance teams can see who holds a given profile, under what lock and validity conditions, and how long it has been since they last authenticated.

This EI serves as an essential control for access governance and SAP security operations by:

- Enabling detection of sensitive profiles attached to unexpected user types, groups, or lock states before those assignments are exploited or audited late
- Supporting segregation-of-duties and least-privilege reviews when profiles must be justified per user population (for example dialog versus batch)
- Providing visibility into inactive or locked accounts that still carry powerful profiles, so remediation can prioritize high-risk rows
- Helping teams correlate profile exposure with password logon patterns, failed logon counters, and master-data change metadata where those attributes are relevant to the review
- Giving auditors a repeatable, monitor-aligned view of profile holders that matches the same logic used in online exception handling

Typical uses include periodic access reviews, preparation for role/profile redesign projects, investigations after security incidents, and operational checks before go-live or cleanup transports. Results support ticketing, deprovisioning, and master-data correction workflows.

The evaluation reads profile-to-user assignment data joined with user master security information, then resolves descriptive and state information through standard user interfaces used elsewhere in the monitoring suite.


## Problem Description

Failure to monitor which users hold specific authorization profiles—and under which lock, validity, and activity conditions—creates multiple risks across security, compliance, and operational management:

**Security and Access Control Risks**

- Powerful profiles may remain attached to dormant, shared, or service accounts without periodic confirmation of business need
- Lock and validity issues on user masters can go unnoticed while risky profile assignments stay technically in force
- Mixing user types in one review makes it easy to apply the wrong policy to technical versus human accounts
- Concentration of sensitive profiles on a small set of users may indicate over-provisioning or inappropriate bundling of duties
- Delayed detection of mismatches between profile intent and actual user population widens the window for misuse or fraud

**Compliance and Audit Risks**

- Access certification and recertification exercises require accurate lists of profile holders with enough context to approve or revoke
- Regulators and internal audit expect evidence that privileged or sensitive access is reviewed on a defined cadence
- Inconsistent or manual extracts increase the risk of omissions, harming the defensibility of access reports
- Lack of standardized state labeling (severity, lock context) slows evidence gathering for GRC tools and workpapers
- Unexplained churn in user master versions or modification metadata can undermine confidence in access records if not reviewed with profile data

**Management Visibility and Decision-Making Risks**

- Security and application owners cannot easily rank which profile assignments need immediate attention versus periodic review
- Project teams lack a single comparable picture of profile exposure across populations when planning role conversions or decommissioning
- Help desk and operations cannot quickly separate one-off data issues from systemic provisioning errors
- Leadership receives late or fragmented signals when exceptions are discovered only through ad hoc queries or spreadsheets
- Cross-team handoffs between security, basis, and functional teams suffer when everyone uses different definitions of “who has the profile”

## Suggested Resolution

**Immediate Response**

- Review flagged profile holders against current org charts, contractor schedules, and system account inventories to confirm business need
- For rows showing restrictive states, validate whether the user should remain valid, be locked, or be scheduled for deprovisioning
- Escalate high-risk combinations (sensitive profile on unexpected user type or group) according to your access-governance playbook
- Open structured tasks in identity or service management with clear owners, due dates, and links to evidence from the monitoring run
- Where approved, use standard user and profile maintenance transactions to adjust assignments or master data after proper authorization

**System Assessment**

- Segment results by user group and user type to see whether issues cluster in batch, dialog, or interface populations
- Compare current results to prior monitoring cycles to distinguish new assignments from long-standing technical debt
- Correlate spikes with change windows, transports, or mass uploads that might explain temporary anomalies
- Validate naming and grouping conventions still match how monitoring is configured for the organization
- Document patterns that suggest template or provisioning defects rather than isolated user errors

**Corrective Actions**

- Remove or replace profile assignments that are no longer justified; update roles and profiles in line with least privilege
- Correct user master validity, lock status, and grouping where policy violations are confirmed
- Adjust monitoring thresholds or schedules with security stakeholders so future runs reflect agreed risk appetite
- Brief reviewers and help desk staff on how to interpret state and severity cues in the output for consistent first-line response
- Retain evidence of review and remediation for audit trails, and schedule recurring runs so profile drift does not accumulate unnoticed


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
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

Creator of the user master record on the assignment row—supports traceability from profile reviews back to whoever originally provisioned the user.

**BCDA1** (Date of Last Password Change):

Date of the last password change on the user master—relevant when judging credential hygiene for accounts that hold the monitored profile.

**BNAME** (User):

Technical user name carrying the profile assignment; the function requires at least one of **BNAME** or **PROFILE** before it retrieves data.

**CLASS** (User group):

User group on the user master—segments dialog, batch, and other populations so profile risk reviews stay comparable within each segment.

**CODV1** (Password Code Vers.):

Legacy password hash algorithm version marker—useful when cryptographic posture must be compared across older stacks.

**CODVN** (Password Code Vers.):

Current password hash algorithm version marker—pairs with **CODV1** when both legacy and modern algorithms coexist.

**DURATION** (Duration(from Last Logon)):

After retrieval, the function compares last logon date and time to the evaluation moment and stores the elapsed amount in the unit given by **DURATION_UNIT**; supply a selection range to keep only rows whose computed span fits your threshold.

**DURATION and DURATION_UNIT Connection:**

The function computes elapsed time from last logon to the evaluation anchor using **DURATION_UNIT**; the numeric **DURATION** selection then retains only rows whose computed span matches your thresholds.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Defines whether the computed inactivity span is expressed in hours, minutes, days, or full calendar-day slices.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**ERDAT** (Creation Date of User Master):

Creation date of the user master—supports onboarding-age or long-lived technical ID questions tied to the profile.

**GLTGB** (Valid to):

Valid-to date on the user master—highlights users who should already be expired while the profile row still exists.

**GLTGV** (Valid from):

Valid-from date—supports checks on prematurely active or incorrectly back-dated accounts tied to the profile.

**LANGU** (Language for texts):

Language key for lock descriptions and other language-dependent texts resolved during the enrichment pass.

**LOCK_ICON** (State Icon):

Icon token applied when the user is in a locked state so dashboards can show lock context at a glance.

**LOCK_ICON Options:**
- **R** — Icon aligned to red-severity resolution.
- **Y** — Icon aligned to yellow-severity resolution.
- **G** — Icon aligned to green-severity resolution.

**LOCNT** (Number of failed logon attempts):

Failed logon attempt counter from user security data—relevant when reviewing brute-force or mis-configuration risk on profile-bearing users.

**LTIME** (Last Logon Time):

Time-of-day component of last logon—pairs with **TRDAT** for sub-day inactivity analysis after data is retrieved.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set, time shifting for the evaluation anchor follows UTC-oriented handling in the surrounding monitor integration.

**MANAGE_IN_UTC Options:**
- **X** — Enable the behavior described in the parameter text.
- ** ** (space) — Leave unset for this monitoring run.

**MODBE** (Changed By):

User who last changed the user master—supports segregation-of-duties checks when profile assignments coincide with suspicious master edits.

**MODDA** (Modification date):

Calendar date of the last user master modification.

**MODTI** (Modification time):

Clock time of the last user master modification—pairs with **MODDA** for same-day change investigations.

**MODDA and MODTI Connection:**

Combine calendar date and clock time when investigations focus on bursts of master-data edits on a single day.

**NAME_FIRST** (First Name):

Given name resolved for the user on the result line so reviewers recognize human accounts next to **BNAME** and **PROFILE**.

**NAME_LAST** (Last Name):

Family name resolved for the user—pairs with **NAME_FIRST** for readable identity context in the output.

**NAME_FIRST and NAME_LAST Connection:**

Use together when you need human-readable name targeting; **BNAME** remains the technical key for the assignment row.

**NAME_TEXT** (Full Name):

Formatted full name for dashboards that display one label per technical user.

**NRPRO** (Number of profiles or authorizations):

Indicator of how many profiles or authorizations are in play for the user—helps spot over-provisioned identities.

**PROFILE** (Profile):

Authorization profile under review; at least one of **PROFILE** or **BNAME** must be supplied for the function to read assignment data.

**PWDLGNDATE** (Date of Last Password Logon):

Date of last password logon—distinct from password change date when judging dormant credentials.

**STATE_COLOR** (State Color):

Severity color band after lock and state resolution; restrict selections to **R**, **Y**, or **G** when the monitor should show only matching bands.

**STATE_COLOR Options:**
- **R** — Red band: critical finding on the row.
- **Y** — Yellow band: warning state.
- **G** — Green band: within normal envelope.

**STATE_DESC** (SW Message):

Short text describing the derived user state after lock-description lookup.

**STATE_ICON** (State Icon):

Primary icon token for the row’s severity or lock outcome after processing.

**STATE_ICON Options:**
- **R** — Icon aligned to red-severity resolution.
- **Y** — Icon aligned to yellow-severity resolution.
- **G** — Icon aligned to green-severity resolution.

**TRDAT** (Last Logon Date):

Last logon date on the user master—feeds the duration calculation performed after the main select.

**TZONE** (Time Zone):

User time zone from master data—aligns interpretation of logon and change timestamps for global populations.

**UFLAG** (User Lock Status):

User master lock bitmap—distinguishes unlocked, password-locked, administratively locked, and other coded states per SAP semantics.

**UFLAG Options:**
- **0** — No lock flags in the evaluated band.
- **32** — Password-related lock per user master semantics.
- **64** — Administrative lock per user master semantics.
- **128** — Additional lock level when used in your system.

**USTYP** (User Type):

User type (dialog, system, communication, etc.) so technical and interactive accounts are not mixed in the same profile review.

**VALID_USERS_ONLY** ('X' - Display only valid users):

When set, removes users whose validity window does not cover the evaluation day after the assignment list is built.

**VALID_USERS_ONLY Options:**
- **X** — Enable the behavior described in the parameter text.
- ** ** (space) — Leave unset for this monitoring run.

**VERSN** (User master record version):

Version counter on the user master—unexpected increments can signal churn worth correlating with profile changes.


### Parameter Relationships

**Profile scope gate**

- The function retrieves data only when at least one value is supplied for **PROFILE** or **BNAME**; leaving both empty ends processing before the main select.
- **PROFILE** identifies which authorization profile (or profiles) is under review; **BNAME** narrows to specific technical users when you already know the account set.

**User master dimensions on the extract**

- **CLASS**, **USTYP**, and **UFLAG** work together as conjunctive filters on the joined user master data, shaping which users remain in the assignment list.
- **VALID_USERS_ONLY** applies after the main read: users outside their validity window are removed so only currently valid identities remain in the result.

**Activity and duration**

- **TRDAT** and **LTIME** provide the last-logon date and time used when the function computes an inactivity span after retrieval.
- **DURATION** and **DURATION_UNIT** must be read as a pair: the unit defines how elapsed time is measured, and the selection on **DURATION** keeps rows whose computed span matches your monitoring thresholds.

**Presentation and severity**

- **STATE_COLOR** filtering is applied after state and lock information is derived, so it reflects the post-processed severity band rather than the raw database row alone.
- **LANGU** aligns language-dependent lock descriptions with the reviewer’s expected logon language.
- **STATE_ICON**, **LOCK_ICON**, and **STATE_DESC** describe the same logical state from icon and text perspectives and are most useful when read together on each line.


### Default Values

- **LANGU** — Default: session logon language (assigned in code before reading caller selections, so resolved texts follow the user’s language when not overridden).
- **DURATION_UNIT** — Default: `D` (days) when not supplied via selection.

### Practical Configuration Examples

**Use Case 1: Single user holding a critical profile**

```
BNAME = ADMIN01
PROFILE = Z_CRITICAL_POSTING
```

**Purpose:** Confirms whether a known technical account still carries a named sensitive profile—typical during incident response or focused access reviews.

**Use Case 2: Profile population by user type and validity**

```
PROFILE = Z_AUDIT_ALL
CLASS = *
USTYP = A
VALID_USERS_ONLY = X
```

**Purpose:** Lists currently valid dialog users in any user group who hold the profile—useful for quarterly certification of a powerful assignment.

**Use Case 3: Long inactivity with full-day duration unit**

```
PROFILE = Z_REMOTE_ACCESS
DURATION = 60
DURATION_UNIT = F
STATE_COLOR = R
```

**Purpose:** Surfaces holders of a connectivity-related profile whose computed inactivity exceeds sixty full-day units and already show a critical severity band—good for dormant-access cleanup programs.

**Use Case 4: Batch population with lock focus**

```
PROFILE = Z_BATCH_POST
CLASS = BATCH
UFLAG = 64
```

**Purpose:** Targets batch users in a defined group carrying the profile while administratively locked—helps separate intentional suspension from accidental lock on active jobs.


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

```abap
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
```
