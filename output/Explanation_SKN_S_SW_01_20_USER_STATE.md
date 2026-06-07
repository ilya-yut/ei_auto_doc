# Exception Indicator: User actions control ( SW_01_20_USR_MULTI)

## General Overview

This Exception Indicator reads user master and security-relevant attributes from the user repository together with join data, then applies date-window, duration, validity, and presentation filters so security and user administration teams can spot accounts that violate policy or show risky states.

This EI serves as an essential control for identity and access governance by:
- Surfacing users whose last logon, password, lock, or creation dates fall outside the monitoring window you configure
- Giving administrators grouped visibility by user name, type, lock flags, failed logon counters, and modifier context
- Supporting reviews of password lifecycle signals alongside administrator resets and lock timestamps
- Enabling color-coded state signals and optional UTC-aligned evaluation when landscapes span regions
- Complementing standard user maintenance with repeatable, parameterized extracts for audits and operational hygiene

Typical use includes periodic access reviews, cleanup after reorganizations, and investigations following authentication incidents. Results are intended for exception workflows rather than full user directory exports.

The routine selects from the user tables with an outer join where configured, enriches rows with descriptive and lock context, and filters by configured duration and validity rules before raising an alert when rows remain in scope.


## Problem Description

Failure to monitor user master records for stale logons, inconsistent validity windows, and risky lock states creates multiple risks across security, compliance, and operational support.

**Security and Access Risks**
- Dormant or orphaned accounts may retain productive access until discovered during an incident
- Password and lock anomalies can indicate compromise or misconfiguration without proactive surfacing
- Failed logon counters and administrative resets may signal brute-force or help-desk misuse patterns

**Compliance and Audit Risks**
- Access certification and segregation-of-duties programs weaken when reviewers lack a repeatable exception population
- Evidence of supervisory review is harder to produce when user state checks are manual and ad hoc

**Operational and Support Risks**
- Help desks react late when account validity dates expire unexpectedly across large user populations
- Distributed teams cannot prioritize remediation without a common filtered view of high-risk user rows

## Suggested Resolution

**Immediate Response**
- Review each flagged user for identifier, validity interval, lock flags, and the dates shown in the exception list
- Validate the business context with the account owner or security team before changing passwords, locks, or validity
- Capture rationale when the finding is a known exception approved by policy

**System Assessment**
- Compare this cycle to the prior one after security projects, mass uploads, or identity interface changes
- Examine concentrations by user group, type, or modifier to see whether one integration drives most items
- Revisit UTC versus local evaluation when borderline timing appears around midnight boundaries

**Corrective Actions**
- Correct master data through your standard user administration process with required approvals
- Adjust monitoring parameters after root cause so the queue remains actionable for operations
- Route repeat systemic issues into defect or change management when directory feeds or jobs require fixes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACCNT | Accnt | CHAR | 50 | 0 | ACCNT | ACCNT |
| 2 | ANAME | Aname | CHAR | 50 | 0 | ANAME | ANAME |
| 3 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 4 | BNAME | Bname | CHAR | 50 | 0 | BNAME | BNAME |
| 5 | CLASS | Class | CHAR | 50 | 0 | CLASS | CLASS |
| 6 | DATE_REF_FLD | Date Ref Fld | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |
| 7 | DATUM | Datum | CHAR | 50 | 0 | DATUM | DATUM |
| 8 | DURATION | Duration | INT4 | 10 | 0 | DURATION | DURATION |
| 9 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | DURATION_UNIT | DURATION_UNIT |
| 10 | ERDAT | Erdat | CHAR | 50 | 0 | ERDAT | ERDAT |
| 11 | LANGU | Langu | CHAR | 1 | 0 | LANGU | LANGU |
| 12 | LOCNT | Locnt | INT4 | 10 | 0 | LOCNT | LOCNT |
| 13 | MANAGE_IN_UTC | Manage In Utc | CHAR | 1 | 0 | MANAGE_IN_UTC | MANAGE_IN_UTC |
| 14 | MODBE | Modbe | CHAR | 50 | 0 | MODBE | MODBE |
| 15 | NO_DATE_RESTRICTION | No Date Restriction | CHAR | 1 | 0 | NO_DATE_RESTRICTION | NO_DATE_RESTRICTION |
| 16 | PWDLGNDATE | Pwdlgndate | CHAR | 50 | 0 | PWDLGNDATE | PWDLGNDATE |
| 17 | PWDLOCKDATE | Pwdlockdate | CHAR | 50 | 0 | PWDLOCKDATE | PWDLOCKDATE |
| 18 | PWDSETDATE | Pwdsetdate | CHAR | 50 | 0 | PWDSETDATE | PWDSETDATE |
| 19 | STATE_COLOR | State Color | CHAR | 50 | 0 | STATE_COLOR | STATE_COLOR |
| 20 | SW_DEST | Sw Dest | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 21 | TRDAT | Trdat | CHAR | 50 | 0 | TRDAT | TRDAT |
| 22 | UFLAG | Uflag | CHAR | 50 | 0 | UFLAG | UFLAG |
| 23 | USTYP | Ustyp | CHAR | 50 | 0 | USTYP | USTYP |
| 24 | VALID_USERS_ONLY | Valid Users Only | CHAR | 1 | 0 | VALID_USERS_ONLY | VALID_USERS_ONLY |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 24 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ACCNT** (Accnt)

Account number key used to scope user/account records in the monitored dataset.

**ANAME** (Aname)

User who created the master/user record; used for creator-based accountability filtering.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BNAME** (Bname)

SAP user name used to restrict output to specific users or user populations.

**CLASS** (Class)

User group/class used to filter users by administrative classification.

**DATE_REF_FLD** (Date Ref Fld)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- TRDAT — Last logon date (or technical date marker) used for user activity recency checks.
- PWDLGNDATE — Date of last password logon usage used for credential-age/security checks.
- PWDSETDATE — Date when the current password was set distinguishing resets from routine rotation events.
- PWDLOCKDATE — Date when the password was administratively locked marking start of lock-driven login denial.

**DATUM** (Datum)

Gives auditors traceable criteria because datum on DATUM is applied consistently before any alert flag is raised.

**DURATION** (Duration)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**ERDAT** (Erdat)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**LANGU** (Langu)

Language key used for language-dependent texts and user-language filtering.

**LOCNT** (Locnt)

<mark>Local count/occurrence metric used for threshold-based exception logic.</mark>

**MANAGE_IN_UTC** (Manage In Utc)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MODBE** (Modbe)

<mark>Modification area/module indicator used in user-change auditing.</mark>

**NO_DATE_RESTRICTION** (No Date Restriction)

Flag that disables default date-window filtering when set.

**PWDLGNDATE** (Pwdlgndate)

Date of last password logon usage used for credential-age/security checks.

**PWDLOCKDATE** (Pwdlockdate)

Date when the password was administratively locked marking start of lock-driven login denial.

**PWDSETDATE** (Pwdsetdate)

<mark>Date when the current password was set distinguishing resets from routine rotation events.</mark>

**STATE_COLOR** (State Color)

State selector used for quick triage via color-coded processing outcomes.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional literals may exist where the framework extends the palette for neutral states.

**SW_DEST** (Sw Dest)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TRDAT** (Trdat)

Last logon date (or technical date marker) used for user activity recency checks.

**UFLAG** (Uflag)

User lock/status flag used to identify locked/disabled user states.

**USTYP** (Ustyp)

User type category used to segment dialog/system/service users.

**VALID_USERS_ONLY** (Valid Users Only)

Boolean flag to restrict results to users validated as active/allowed.


### Parameter Relationships

How parameter combinations work together

**Explicit calendar window versus default lookback:** **DATUM** supplies explicit calendar bounds when populated. When explicit dates are not provided and **NO_DATE_RESTRICTION** is not active, **BACKDAYS** is the fallback that drives how far back the default range reaches (including values passed through the selection interface) before users are read.

**Reference date axis:** **DATE_REF_FLD** routes that calendar window onto creation, last logon, or password-related date fields so the same backward span follows the business date you intend for the review.

**Age filter after selection:** **DURATION** with **DURATION_UNIT** is an additional filter after rows are read: each user must still fit the configured elapsed-time band relative to the evaluation clock and the reference date logic in the routine.

**UTC versus local evaluation:** **MANAGE_IN_UTC** shifts whether the evaluation clock used with **DATUM** and duration math follows UTC semantics versus local application-server time.

**Validity narrowing:** When **VALID_USERS_ONLY** is active, the result set is further reduced to users whose validity window still covers the evaluation day after other filters apply.

**No date restriction:** **NO_DATE_RESTRICTION** clears the built date-range tables so monitoring does not apply the usual calendar window before other filters run.

**Remote execution path:** **SW_DEST** enables the cloud-delegation path that runs the paired function in the connected system when populated.

**Final selection:** Both the date side (explicit **DATUM** or **BACKDAYS** fallback when applicable, unless **NO_DATE_RESTRICTION** clears it) and the **DURATION**/**DURATION_UNIT** age filter apply together with state-color and optional validity rules—rows must satisfy the active combination of date and duration conditions before they appear in the final alert population.


### Default Values

- **BACKDAYS** - 3000
- **DURATION_UNIT** - D
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Stale logon review for dialog users**

**Purpose:** Focus on dialog-type users with a last-logon reference and day-based aging in UTC mode.
```
USTYP = A
DATE_REF_FLD = TRDAT
BACKDAYS = 90
DURATION = 30 - 999999
DURATION_UNIT = D
MANAGE_IN_UTC = X
```

**Use Case 2: Full-day inactivity band**

**Purpose:** Flag accounts whose reference date is at least fourteen full days behind the evaluation moment.
```
CLASS = SAP*
DURATION = 14
DURATION_UNIT = F
STATE_COLOR = R
LANGU = E
```

**Use Case 3: Password-event window**

**Purpose:** Review password logon and reset dates inside an explicit maintenance weekend with validity filtering.
```
DATE_REF_FLD = PWDLGNDATE
DATUM = 20250501 - 20250503
PWDSETDATE = 20250401 - 20250515
VALID_USERS_ONLY = X
```

**Use Case 4: Failed logon concentration**

**Purpose:** Narrow to rows with elevated failed logon counts for a service account pattern.
```
LOCNT = 5 - 99
BNAME = SVC*
UFLAG = 64
SW_DEST = PROD_SEC
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_20_USER_STATE | ACCNT | ACCNT | CHAR(50) | ACCNT |
| /SKN/S_SW_01_20_USER_STATE | ANAME | ANAME | CHAR(50) | ANAME |
| /SKN/S_SW_01_20_USER_STATE | BACKDAYS | BACKDAYS | INT4(10) | BACKDAYS |
| /SKN/S_SW_01_20_USER_STATE | BNAME | BNAME | CHAR(50) | BNAME |
| /SKN/S_SW_01_20_USER_STATE | CLASS | CLASS | CHAR(50) | CLASS |
| /SKN/S_SW_01_20_USER_STATE | DATE_REF_FLD | DATE_REF_FLD | CHAR(30) | NAME_FELD |
| /SKN/S_SW_01_20_USER_STATE | DATUM | DATUM | CHAR(50) | DATUM |
| /SKN/S_SW_01_20_USER_STATE | DURATION | DURATION | INT4(10) | DURATION |
| /SKN/S_SW_01_20_USER_STATE | DURATION_UNIT | DURATION_UNIT | CHAR(1) | DURATION_UNIT |
| /SKN/S_SW_01_20_USER_STATE | ERDAT | ERDAT | CHAR(50) | ERDAT |
| /SKN/S_SW_01_20_USER_STATE | LANGU | LANGU | CHAR(1) | LANGU |
| /SKN/S_SW_01_20_USER_STATE | LOCNT | LOCNT | INT4(10) | LOCNT |
| /SKN/S_SW_01_20_USER_STATE | MANAGE_IN_UTC | MANAGE_IN_UTC | CHAR(1) | MANAGE_IN_UTC |
| /SKN/S_SW_01_20_USER_STATE | MODBE | MODBE | CHAR(50) | MODBE |
| /SKN/S_SW_01_20_USER_STATE | NO_DATE_RESTRICTION | NO_DATE_RESTRICTION | CHAR(1) | NO_DATE_RESTRICTION |
| /SKN/S_SW_01_20_USER_STATE | PWDLGNDATE | PWDLGNDATE | CHAR(50) | PWDLGNDATE |
| /SKN/S_SW_01_20_USER_STATE | PWDLOCKDATE | PWDLOCKDATE | CHAR(50) | PWDLOCKDATE |
| /SKN/S_SW_01_20_USER_STATE | PWDSETDATE | PWDSETDATE | CHAR(50) | PWDSETDATE |
| /SKN/S_SW_01_20_USER_STATE | STATE_COLOR | STATE_COLOR | CHAR(50) | STATE_COLOR |
| /SKN/S_SW_01_20_USER_STATE | TRDAT | TRDAT | CHAR(50) | TRDAT |
| /SKN/S_SW_01_20_USER_STATE | UFLAG | UFLAG | CHAR(50) | UFLAG |
| /SKN/S_SW_01_20_USER_STATE | USTYP | USTYP | CHAR(50) | USTYP |
| /SKN/S_SW_01_20_USER_STATE | VALID_USERS_ONLY | VALID_USERS_ONLY | CHAR(1) | VALID_USERS_ONLY |

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
