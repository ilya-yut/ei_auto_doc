# Exception Indicator: List of Users (General) - SW_01_20_USER_STATE

## General Overview

This Exception Indicator (EI) monitors SAP user master data to surface accounts whose logon, password, validity, or lock-related attributes fall outside expected patterns within a configurable time window. It reads the central user table together with related user data and enriches each line with human-readable names and visual state cues so security and operations teams can review risky or stale identities in one place.

This EI serves as an essential control for identity governance and IT operations by:

- Enabling detection of users who have not authenticated or changed credentials within policy-defined horizons, before those accounts are abused or violate audit expectations
- Supporting review of administratively locked or otherwise restricted accounts alongside plain-language state descriptions for faster triage
- Providing visibility into user type and grouping dimensions so dialog, batch, and technical populations can be monitored with different thresholds
- Helping teams spot unexpected concentration of failed logon indicators or validity anomalies that warrant password resets, deprovisioning, or master-data correction
- Giving auditors and access reviewers a repeatable view of user state aligned to the same selection logic used in online monitoring

Organizations use this EI during periodic access reviews, after security incidents, before go-live cutovers, and in recurring operational checks where dormant or misconfigured users must be identified early. Results support prioritization of remediation work and evidence for compliance with internal and external identity controls.

The primary data source is the SAP user master repository (for example the user table read in the function logic), supplemented where applicable by related user records and standard user-detail interfaces used to resolve descriptive fields.


## Problem Description

Failure to monitor SAP user master state and activity against defined time horizons creates multiple risks across security operations, compliance, and day-to-day administration:

**Security and Access Control Risks**

- Dormant interactive accounts that remain technically valid can be reused for unauthorized access if compromise goes unnoticed
- Password and lock conditions that drift from policy may leave weak or indefinitely locked identities unmanaged
- Service and batch identities without timely review can accumulate excessive privileges relative to actual need
- Failed logon patterns may indicate brute-force attempts or misconfigured interfaces long before help desk tickets appear
- Mixing user types without segmentation makes it hard to apply appropriate controls to human versus technical accounts

**Compliance and Audit Risks**

- Access recertification and segregation-of-duties programs depend on accurate, time-bounded views of who can still log on
- Regulators and internal audit often expect evidence that inactive or terminated users are identified and addressed on a schedule
- Inconsistent interpretation of “last activity” versus “record creation” can undermine the defensibility of access reports
- Lack of standardized state labeling increases manual effort and error when exporting findings to GRC or ticketing tools
- Delayed detection of validity-window violations weakens the story that only entitled users are active in production

**Management Visibility and Decision-Making Risks**

- Leadership lacks a single, comparable picture of user risk posture across systems when monitoring is ad hoc or spreadsheet-based
- Operations teams cannot easily rank which accounts need password resets, validity extensions, or deactivation first
- Project and support teams struggle to distinguish one-off anomalies from systemic configuration or provisioning issues
- Month-end or quarter-end reviews take longer when user exceptions are discovered late in the cycle
- Cross-functional handoffs between security, basis, and application teams suffer when everyone uses different definitions of “inactive”

## Suggested Resolution

**Immediate Response**

- Review flagged users in the monitoring output and validate each case against the business context (still employed, still needed, contractor end date, etc.)
- For high-risk rows, confirm whether the account should remain valid, be locked, or be scheduled for deprovisioning
- Compare findings to recent organizational changes (onboarding, transfers, terminations) to separate data lag from true exceptions
- Open follow-up tasks in the identity or service management process with clear ownership and due dates
- Where appropriate, use standard user maintenance transactions to inspect or adjust the underlying master record after approval

**System Assessment**

- Re-run or adjust monitoring using different time anchors (for example creation versus last logon versus password-related dates) to see whether issues persist or were artifacts of one definition of activity
- Segment results by user group and user type to see whether problems concentrate in batch, dialog, or interface populations
- Correlate spikes with change windows, transports, or mass uploads that might explain temporary anomalies
- Validate that organizational naming and grouping conventions still match how monitoring is configured
- Document any patterns that suggest systematic provisioning or template errors rather than isolated mistakes

**Corrective Actions**

- Correct master data (validity dates, user type, group assignments) and password or lock status in line with policy after proper authorization
- Deactivate or archive accounts that are no longer required, and update role assignments where least privilege demands it
- Adjust monitoring thresholds or schedules with security and operations stakeholders so future runs align with agreed risk appetite
- Brief process owners and help desk staff on how to interpret state and severity cues in the output so first-line response stays consistent
- Retain evidence of review and remediation for audit trails, and schedule recurring runs so user drift does not accumulate unnoticed


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
| 9 | DATE_REF_FLD | Date Ref. Field |  | 0 | 0 |  |  |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | ERDAT | Creation Date of User Master | DATS | 8 | 0 | XUERDAT | DATUM |
| 13 | GLTGB | Valid to | DATS | 8 | 0 | XUGLTGB | DATUM |
| 14 | GLTGV | Valid from | DATS | 8 | 0 | XUGLTGV | DATUM |
| 15 | INIT_PWD_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 16 | LANGU | Description Language |  | 0 | 0 |  |  |
| 17 | LOCK_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 18 | LOCNT | Number of failed logon attempts | INT1 | 3 | 0 | XULOCNT | XULOCNT |
| 19 | LTIME | Last Logon Time | TIMS | 6 | 0 | XULTIME | UZEIT |
| 20 | MODBE | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 21 | MODDATE | Modification date | DATS | 8 | 0 | XUMODDATE | DATUM |
| 22 | MODIFIER | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 23 | MODTIME | Modification time | TIMS | 6 | 0 | XUMODTIME | UZEIT |
| 24 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 25 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 26 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 27 | NO_DATE_RESTRICTION | 'X' - No restriction |  | 0 | 0 |  |  |
| 28 | PWDLGNDATE | Date of Last Password Logon | DATS | 8 | 0 | XULPDAT | DATUM |
| 29 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 30 | STATE_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 31 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 32 | TRDAT | Last Logon Date | DATS | 8 | 0 | XULDATE | DATUM |
| 33 | TZONE | Time Zone | CHAR | 6 | 0 | TZNZONE | TZNZONE |
| 34 | UFLAG | User Lock Status | INT1 | 3 | 0 | XUUFLAG | XUUFLAG |
| 35 | USTYP | User Type | CHAR | 1 | 0 | XUUSTYP | XUUSTYP |
| 36 | VALID_USERS_ONLY | 'X' - Display only valid users |  | 0 | 0 |  |  |
| 37 | VERSN | User master record version | CHAR | 3 | 0 | XUVERSION | XUVERSION |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 37 parameters listed in the Parameters Reference Table above.

**ACCNT** (Account number):

Account identifier carried on the user master context this EI returns; use it when monitoring ties a technical user to a specific account record.

**ANAME** (Creator of User Master Record):

Creator of the user master row—helps audits trace who originally provisioned an account that now violates policy.

**BACKDAYS** (Days backwards from today):

How many calendar days back from the evaluation anchor day form the default lower bound for date-driven user selection when no explicit range is supplied.

**BCDA1** (Date of Last Password Change):

Date of the last password change on the user master—central for detecting stale credentials or policy breaches.

**BNAME** (User):

Technical user name in the user master—primary key for targeting or excluding individual accounts in the result set.

**CLASS** (User group):

User group from user master maintenance—segments populations (for example batch versus dialog service accounts) for scoped monitoring.

**CODV1** (Password Code Vers.):

Legacy password hash algorithm version marker for older systems—relevant when comparing cryptographic posture across heterogeneous landscapes.

**CODVN** (Password Code Vers.):

Current password hash algorithm version marker—pair with policy rules that require modern algorithms only.

**DATE_REF_FLD** (Date Ref. Field):

Chooses which user-master date field receives the relative date window from **BACKDAYS** / **DURATION** (creation, last logon, password logon, password reset, or password lock date per the function’s mapping). Align this with how your policy defines “inactive” or “stale.”

**DATE_REF_FLD Options:**
- **ERDAT** — Applies the monitoring window to user master creation date (matches the function’s date-range mapping).
- **TRDAT** — Applies the window to last logon date; also used when the field is blank or does not match a handled value.
- **PWDLGNDATE** — Applies the window to date of last password logon.
- **PWDSETDATE** — Applies the window to date of password reset by an administrator.
- **PWDLOCKDATE** — Applies the window to date when the password lock was set.
- **Other values** — Fall through to last-logon-style handling per the function’s default branch.

**DURATION** (Duration In Time Units):

Numeric span paired with **DURATION_UNIT** to express how far back from the reference date the EI should evaluate activity or validity.

**DURATION and DURATION_UNIT Connection:**

**DURATION** supplies the numeric span; **DURATION_UNIT** defines whether that span is hours, minutes, days, or full calendar days. Configure both together so operators read one clear window (for example “30 days” or “12 hours”).

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit for **DURATION** (hours, minutes, days, or full-day slices).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**ERDAT** (Creation Date of User Master):

Creation date of the user master—useful for onboarding-age rules and detecting long-lived technical accounts.

**GLTGB** (Valid to):

Valid-to date—flags users that should already be expired but still appear active.

**GLTGV** (Valid from):

Valid-from date—supports checks on prematurely active or back-dated accounts.

**INIT_PWD_ICON** (State Icon):

Icon token for initial-password state on the output row after the icon resolution step.

**INIT_PWD_ICON Options:**
- **R** — Icon slot for red-severity resolution.
- **Y** — Icon slot for yellow-severity resolution.
- **G** — Icon slot for green-severity resolution.

**LANGU** (Description Language):

Language key for descriptive texts on the result line so reviewers see names and messages in the expected logon language.

**LOCK_ICON** (State Icon):

Icon token representing lock-related state on the output row for quick visual triage.

**LOCK_ICON Options:**
- **R** — Icon slot for red-severity resolution.
- **Y** — Icon slot for yellow-severity resolution.
- **G** — Icon slot for green-severity resolution.

**LOCNT** (Number of failed logon attempts):

Count of failed logon attempts—supports brute-force or mis-configuration detection on sensitive IDs.

**LTIME** (Last Logon Time):

Last logon time-of-day—pairs with last logon date for intraday inactivity policies.

**MODBE** (Changed By):

User who last changed the master record—supports segregation-of-duties reviews when unauthorized editors appear.

**MODDATE** (Modification date):

Date of the last change to the user master—feeds “dormant since” style analytics.

**MODIFIER** (Changed By):

Alternate display of the last-changed-by user where the monitoring layout expects a separate modifier column.

**MODTIME** (Modification time):

Time of the last master-data change—refines **MODDATE** for same-day change storms.

**NAME_FIRST** (First Name):

Given name from address data—helps business teams recognize human accounts in a technical list.

**NAME_LAST** (Last Name):

Family name from address data—pairs with **NAME_FIRST** for directory-style review.

**NAME_FIRST and NAME_LAST Connection:**

Together they reconstruct the person’s name for business readers while **BNAME** remains the technical key.

**NAME_TEXT** (Full Name):

Formatted full name for dashboards that show one readable label per technical user.

**NO_DATE_RESTRICTION** ('X' - No restriction):

When set, disables the usual date window so the EI can list users without applying the relative date filter—use only when policy explicitly allows unbounded reads.

**NO_DATE_RESTRICTION Options:**
- **X** — Apply the flag as described in the parameter name.
- ** ** (space) — Leave the flag unset for this run.

**PWDLGNDATE** (Date of Last Password Logon):

Date of the last password logon—distinct from password change date; supports dormant-password policies.

**STATE_COLOR** (State Color):

Severity color band on each output row for monitoring consoles (red/yellow/green semantics).

**STATE_COLOR Options:**
- **R** — Red band: critical user-state finding.
- **Y** — Yellow band: warning requiring review.
- **G** — Green band: within expected policy envelope.

**STATE_DESC** (SW Message):

Short message text describing the derived state for the row—human-readable complement to color and icons.

**STATE_ICON** (State Icon):

Primary state icon token on the row after resolution from severity and lock context.

**STATE_ICON Options:**
- **R** — Icon slot for red-severity resolution.
- **Y** — Icon slot for yellow-severity resolution.
- **G** — Icon slot for green-severity resolution.

**TRDAT** (Last Logon Date):

Last logon date—core field for identifying users who have not authenticated within the expected period.

**TZONE** (Time Zone):

Time zone of the user master—aligns logon and validity interpretation for global user populations.

**UFLAG** (User Lock Status):

Lock status bitmap from the user master—indicates administrative lock, password lock, or unrestricted states depending on the coded value.

**UFLAG Options:**
- **0** — No administrative lock flags asserted in the evaluated band.
- **32** — Password-related lock state per user master semantics.
- **64** — Administrative lock state per user master semantics.
- **128** — Additional lock level when present in your system’s user master evaluation.

**USTYP** (User Type):

User type (dialog, system, communication, etc.)—separates interactive humans from technical consumers in the same report.

**VALID_USERS_ONLY** ('X' - Display only valid users):

When set, restricts the population to currently valid users per master validity—excludes expired or not-yet-valid rows from the narrative the EI produces.

**VALID_USERS_ONLY Options:**
- **X** — Apply the flag as described in the parameter name.
- ** ** (space) — Leave the flag unset for this run.

**VERSN** (User master record version):

Version counter of the user master record—surfaces unexpected churn when frequent unexplained increments appear.


### Parameter Relationships

**Relative window and date reference**

- **BACKDAYS** defines how far back the default lower bound of the evaluation window reaches when callers supply it through the standard selection mechanism; it works together with whichever date field **DATE_REF_FLD** maps to the same window (creation, last logon, or password-related dates as implemented in the function).
- **DURATION** and **DURATION_UNIT** supply an alternative way to express a span from the reference moment; they should be configured as a pair so the monitored period is unambiguous (for example a number of days versus hours).
- **NO_DATE_RESTRICTION** overrides the usual application of that window: when active, the logic clears the built date range so selection is not limited by the relative period—use only when policy allows unconstrained reads.

**Population scope**

- **VALID_USERS_ONLY** further narrows the result after retrieval by keeping only users whose validity window covers the evaluation day, which interacts with any date-window filters already applied.
- **BNAME**, **CLASS**, **USTYP**, **UFLAG**, **ANAME**, **ACCNT**, **LOCNT**, and related multi-value selections combine as conjunctive filters on the user master extract; tightening one dimension reduces the set passed to downstream enrichment and duration checks.

**Presentation and derived state**

- **LANGU** affects language-dependent descriptions resolved for each user row; it does not change who is selected but aligns text with the reviewer’s logon language.
- **STATE_COLOR**, **STATE_ICON**, **STATE_DESC**, **LOCK_ICON**, and **INIT_PWD_ICON** reflect outcomes of the post-processing pass (for example lock and password-init semantics) and should be read together when interpreting a single line in a monitoring dashboard.


### Default Values

- **LANGU** — Default: session logon language (assigned in code before reading caller selections, so descriptive texts follow the user’s language when not overridden).
- **DURATION_UNIT** — Default: `D` (days) when not supplied via selection.
- **BACKDAYS** — Default: `3000` when no date range is supplied through the selection table and date restriction is still in effect, establishing a wide backward window for the relative filter.

**Note:** When **NO_DATE_RESTRICTION** is set, the built date range is cleared, so the default **BACKDAYS** behavior does not apply in that mode.

### Practical Configuration Examples

**Use Case 1: Dormant dialog users (last logon)**

```
BACKDAYS = 90
DATE_REF_FLD = TRDAT
```

**Purpose:** Highlights interactive users who have not logged on within roughly three months when the monitoring window is anchored on last logon—typical for quarterly access hygiene.

**Use Case 2: Password-logon review with valid-only population**

```
BACKDAYS = 180
DATE_REF_FLD = PWDLGNDATE
VALID_USERS_ONLY = X
USTYP = A
```

**Purpose:** Focuses on currently valid dialog-type users whose last password logon falls outside a six-month horizon, a common pattern for credential-staleness reviews.

**Use Case 3: New-account lookback with full-day unit**

```
DURATION = 30
DURATION_UNIT = F
DATE_REF_FLD = ERDAT
```

**Purpose:** Evaluates user master records created in the last thirty full days using the duration unit intended for day-based slices, useful after mass onboarding or migrations.

**Use Case 4: Targeted service accounts without date window**

```
NO_DATE_RESTRICTION = X
BNAME = SVC*
CLASS = BATCH
```

**Purpose:** Lists batch-style service users matching a naming pattern regardless of relative date filters, while still restricting by user group—helpful when validating a known population outside a rolling window.


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
