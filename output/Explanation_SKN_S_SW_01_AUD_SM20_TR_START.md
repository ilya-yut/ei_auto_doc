# Exception Indicator: Transactions Start (SM20 based) - SW_AUD_SM20_TR_START

## General Overview

This Exception Indicator reports security audit log–derived transaction start activity so governance and operations teams can see which users executed which transactions, with timing context and descriptive text, in a structured extract suitable for access reviews and unauthorized-activity investigations.

The EI supports audit and security operations by:
- Surfacing user and transaction combinations observed in the monitoring window so reviewers can compare actual usage to role design
- Making it easier to relate transaction start timestamps to duration-based filters when triaging suspicious sessions
- Supporting follow-up when packaged date retrieval spans multiple intervals in large landscapes
- Giving security and Basis teams a repeatable snapshot for periodic reviews, incident correlation, and evidence collection

Organizations use this style of monitoring to detect anomalies in transaction usage patterns, to validate remediation after role changes, and to document oversight during sensitive periods. It is most useful when you need an exception-oriented summary aligned with SM20-based retrieval rather than ad hoc log browsing alone.

The function reads transaction start records for the evaluated date range from the cloud integration path, enriches duration in the configured unit, applies the declared selection ranges, and returns rows ready for alerting and reporting.


## Problem Description

Failure to monitor transaction start activity against intended access creates multiple risks across security governance, operational control, and compliance:

**Security and Access Governance Risks**
- Unauthorized or unexpected transaction usage can continue without timely detection when reviews rely on manual sampling alone
- Segregation-of-duties gaps become harder to prove or disprove when usage evidence is fragmented across tools and teams
- Privileged or sensitive transactions may be executed outside expected windows without a compact monitoring signal
- Investigation cycles lengthen when auditors cannot quickly separate normal business traffic from out-of-pattern execution
- Repeatable evidence trails for sensitive periods may be incomplete when monitoring is informal or inconsistent

**Operational Control and Investigation Risks**
- Incident response teams lack a simple before-and-after view when roles, profiles, or firefighter access change
- Application and security operations may mis-prioritize work when transaction usage signals are anecdotal rather than comparable week over week
- Seasonal peaks can change usage mixes in predictable ways that still surprise teams if thresholds are not trended
- Cross-system symptoms can send investigators down the wrong path when user and transaction context is not visible in one extract
- Temporary project access can shift usage profiles without being documented as baseline changes

**Management Visibility and Compliance Risks**
- Leadership may approve expanded access without visibility into whether observed usage matches policy intent over time
- Audit and compliance stakeholders lack concise evidence that transaction usage was monitored during required periods
- Problem management slows when teams cannot quickly isolate “one noisy user pattern” from broader misuse signals
- Post-incident reviews lack a compact narrative tying remediation actions to measurable usage movement
- External assessments become harder when monitoring discipline cannot be demonstrated with consistent artifacts

## Suggested Resolution

**Immediate Response**
- Review the flagged transaction start extract to understand which users and transactions drive the exception relative to the monitoring intent
- Validate whether the signal aligns with a known change window (role change, go-live, maintenance) versus unexpected usage
- Check whether the pattern is isolated to specific users or transactions versus broad usage drift
- Coordinate with the security owner if the timing correlates with an access review, incident, or campaign window
- Capture business context (project, close activities, break-glass events) so later analysis separates normal peaks from misuse

**System Assessment**
- Compare current readings to prior periods using the same monitoring intent and threshold philosophy
- Examine whether the pattern is gradual (trend) versus step-change (access or process shift)
- Review related identity and authorization signals when symptoms persist—roles, profiles, and emergency access records
- Assess whether exceptions cluster in time-of-day or calendar patterns that match known operational cycles
- Validate that monitoring expectations still match the current release, integration points, and audit configuration assumptions

**Corrective Actions**
- Adjust monitoring thresholds or scope when evidence shows sustained misalignment with policy targets, following your change process
- Escalate to security and application owners when specific transactions or users remain out of band after validation
- Document remediation and rationale so future reviewers understand intent and scope
- Schedule recurring checks during high-risk periods until stability is re-established
- Fold communication and knowledge-base updates into ownership and runbook improvements under **Corrective Actions**—not as a separate training-only subsection


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 3 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 4 | INSTANCENAME | AS Instance | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 5 | LANGU | Description Language |  | 0 | 0 |  |  |
| 6 | MANDT | Client ID | CLNT | 3 | 0 | SYMANDT | MANDT |
| 7 | MSCDATE | Transaction Start Date | DATS | 8 | 0 | ALDATE | DATUM |
| 8 | MSCTIME | Time | TIMS | 6 | 0 | ALTIME | TIME |
| 9 | PACKAGE_INTERVAL | Package size (days) |  | 0 | 0 |  |  |
| 10 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 11 | TRN_BY | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 12 | TRN_EX | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 13 | TTEXT | Transaction text | CHAR | 36 | 0 | TTEXT_STCT | TEXT36 |
| 14 | USER | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 15 | USERID | User | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 15 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**INSTANCENAME** (AS Instance)

Application/HANA instance identifier used for server-level monitoring scope.

**LANGU** (Description Language)

Language key used for language-dependent texts and user-language filtering.

**MANDT** (Client ID)

SAP client (MANDT) mandatory organizational key on all client-dependent tables.

**MSCDATE** (Transaction Start Date)

Message/status creation date used for log/event period filtering.

**MSCTIME** (Time)

Message/status creation time used for intra-day event analysis.

**PACKAGE_INTERVAL** (Package size (days))

Explains why two monitoring passes differ: only the pass with stricter package size (days) on PACKAGE_INTERVAL surfaces the disputed rows.

**TCODE** (Transaction Code)

SAP Transaction code

**TRN_BY** (Transaction Code)

Reduces false positives during peak windows by tightening transaction code through TRN_BY alongside state filters.

**TRN_EX** (Transaction Code)

Reduces false positives during peak windows by tightening transaction code through TRN_EX alongside state filters.

**TTEXT** (Transaction text)

When combined with destination discipline, transaction text on TTEXT keeps both breadth and depth of the extract intentional.

**USER** (User)

User identifier field used for actor-based filtering.

**USERID** (User)

User ID key used for authentication/user master level scoping.


### Parameter Relationships

How parameter combinations work together

**Time window and packaging**

- **BACKDAYS** shapes the default evaluation window when the monitoring date range is not already fully specified through the start-date style inputs. **PACKAGE_INTERVAL** controls how the retrieval chunks long ranges into smaller packages so large windows can be processed without exceeding practical limits.
- **MSCDATE** and **MSCTIME** anchor the business timing of each recorded transaction start event, while **DURATION** and **DURATION_UNIT** work together as an additional filter on the computed elapsed interval relative to the evaluation clock, so date context and duration filtering can both apply when the monitor is tuned tightly.

**Identity and transaction dimensions**

- **USER** and **USERID** represent user-related identity keys for filtering and interpreting rows; **TCODE**, **TRN_BY**, and **TRN_EX** distinguish transaction identifiers and related transaction attributes used in the extract.
- **TTEXT** carries descriptive text aligned with the transaction code when available, helping reviewers interpret raw codes without separate lookups.

**Technical context**

- **INSTANCENAME** and **MANDT** help place each row in application server and client context when those dimensions are part of the monitoring contract for your landscape.

**Selection contract**

- The multivalued selection inputs define the ranges the monitor is allowed to surface. Any combination of thresholds applies together as filters on the returned rows, so tightening one dimension without revisiting related dimensions can change which usage patterns appear in alerting.


### Default Values

- **BACKDAYS** - initial - treated as 1 day backward window by code
- **DURATION** - initial - treated as no extra duration filter by code
- **DURATION_UNIT** - initial - treated as D day units by code
- **PACKAGE_INTERVAL** - 10

### Practical Example of Parameter Configuration

**Use Case 1: Focused user and transaction watch**

**Purpose:** Monitor a specific user executing sensitive transaction codes across a defined calendar slice.

```
MSCDATE = 20260101 - 20260131
USERID = ADMIN01
TCODE = SE16 - SE16
BACKDAYS = 1
DURATION = 0 - 999999999
DURATION_UNIT = D
PACKAGE_INTERVAL = 10
```

**Use Case 2: Broad transaction pattern**

**Purpose:** Catch elevated activity for a transaction family while keeping duration filtering tight.

```
TRN_EX = SM20 - SM20
DURATION = 100 - 999999999
DURATION_UNIT = D
MSCTIME = 000000 - 235959
```

**Use Case 3: Client and instance scoped review**

**Purpose:** Narrow the extract to one client and application server instance for investigation.

```
MANDT = 100 - 100
INSTANCENAME = PRD - PRD
USER = BATCHUSER01 - BATCHUSER01
TTEXT = * - *
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_AUD_SM20_TR_START | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_AUD_SM20_TR_START | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_AUD_SM20_TR_START | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_AUD_SM20_TR_START | MANDT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_AUD_SM20_TR_START | MSCDATE | Alert: date | DATS(8) | ALDATE |
| /SKN/S_SW_01_AUD_SM20_TR_START | MSCTIME | Alert: Time value in timeformat | TIMS(6) | ALTIME |
| /SKN/S_SW_01_AUD_SM20_TR_START | SLGLTRM2 | SecAudit: Terminal name | CHAR(20) | RSAUTERM |
| /SKN/S_SW_01_AUD_SM20_TR_START | SLGREPNA | Program Name | CHAR(40) | PROGRAM_ID |
| /SKN/S_SW_01_AUD_SM20_TR_START | TCODE | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_SM20_TR_START | TRN_BY | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_SM20_TR_START | TRN_EX | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_SM20_TR_START | TTEXT | Transaction Text | CHAR(36) | TTEXT_STCT |
| /SKN/S_SW_01_AUD_SM20_TR_START | USER | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_AUD_SM20_TR_START | USERID | User Name | CHAR(12) | SYUNAME |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_AUD_SM20_TR_START.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_AUD_SM20_TR_START OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA_MULTY: DURATION   /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
            DATUM     SY-DATUM,
            MSCDATE   ALDATE,
            USERID      XUBNAME,
            TCODE     TCODE.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             BACKDAYS INT4,
             LANGU LANGU.
DATA_SINGLE: PACKAGE_INTERVAL  INT2.   " Dates Interval for packages
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LS_TR_FACT_AGGR TYPE /SKN/S_SW_USER_TR_FACT_AGGR,
      LT_TR_FACT_AGGR LIKE TABLE OF LS_TR_FACT_AGGR.
DATA: LS_USER_TR_LOG TYPE /SKN/S_SW_O1_AUD_TR_SM20,
      LT_USER_TR_LOG LIKE TABLE OF LS_USER_TR_LOG.
DATA: LS_USERS TYPE /SKN/S_SW_AUD_USERS,
      LT_USERS LIKE TABLE OF LS_USERS.
DATA: BEGIN OF LS_USER_LIST,
        USER TYPE XUBNAME,
      END OF LS_USER_LIST,
      LT_USER_LIST LIKE TABLE OF LS_USER_LIST.
DATA: LS_TR_LIST TYPE /SKN/S_SW_TR_LIST,
      LT_TR_LIST LIKE TABLE OF LS_TR_LIST,
      LT_ACT_TR_LIST LIKE TABLE OF LS_TR_LIST,
      LS_ACT_TR_LIST LIKE LINE OF LT_ACT_TR_LIST.
DATA: LS_TCODE TYPE /SKN/S_SW_TCODE,
      LT_TCODE LIKE TABLE OF LS_TCODE.
DATA: LS_USER_RNG TYPE /SKN/S_SW_USER_RNG,
      LT_SAP_ALL_RNG LIKE TABLE OF LS_USER_RNG.
DATA: DATE_FROM LIKE SY-DATUM,
      DATE_TO LIKE SY-DATUM.
DATA: LV_DATE_FROM LIKE SY-DATUM,
      LV_DATE_TO LIKE SY-DATUM,
      LV_DATE_INTERVAL TYPE I,
      LV_PAGES TYPE I.
DATA : TIME_DIFF TYPE  INT4 .
DATA : SY_DATLO LIKE SY-DATLO ,
       SY_TIMLO LIKE SY-TIMLO.
DATA : MANAGE_IN_UTC TYPE  CHAR1.
DATA: LV_STAT_AUTH_OK TYPE CHAR1.
*-- Fill Selection Option Tables
   SELECT_MULTY: DURATION,
                 DATUM,
                 MSCDATE,
                 USERID,
                 TCODE.
   LV_DURATION_UNIT = 'D'.
   LV_BACKDAYS = 1.
   SELECT_SINGLE: DURATION_UNIT,
                  BACKDAYS,
                  LANGU.
   LV_PACKAGE_INTERVAL = 10.  " 10 days
   SELECT_SINGLE: PACKAGE_INTERVAL.
  READ TABLE R_MSCDATE INTO RS_MSCDATE INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_MSCDATE-LOW.
    DATE_TO = RS_MSCDATE-HIGH.
    IF DATE_TO IS INITIAL.
      DATE_TO = DATE_FROM.
    ENDIF.
  ELSE.
    DATE_FROM = SY-DATUM - LV_BACKDAYS.
    DATE_TO = SY-DATUM.
  ENDIF.
  LV_DATE_INTERVAL = DATE_FROM - DATE_TO + 1.
  SET_SY_TIME MANAGE_IN_UTC SY_DATLO SY_TIMLO .
  TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "-- Add TRs to be excluded
   RS_TCODE-SIGN = 'E'.
    RS_TCODE-OPTION = 'EQ'.
     RS_TCODE-LOW = 'SESSION_MANAGER'.
      APPEND RS_TCODE TO R_TCODE.
     RS_TCODE-LOW = 'SMEN'.
      APPEND RS_TCODE TO R_TCODE.
     RS_TCODE-LOW = 'BI_CLIENT_RUNTIME'.
      APPEND RS_TCODE TO R_TCODE.
  LV_STAT_AUTH_OK = 'X'.
  LV_PAGES = 1.
  IF LV_PACKAGE_INTERVAL IS NOT INITIAL.
    LV_DATE_INTERVAL = DATE_TO - DATE_FROM + 1.
    LV_PAGES = ROUND( VAL = ( LV_DATE_INTERVAL / LV_PACKAGE_INTERVAL ) DEC  = 0 MODE = CL_ABAP_MATH=>ROUND_UP ).
  ENDIF.
  LV_DATE_FROM = DATE_FROM.
  LV_DATE_TO = LV_DATE_FROM + LV_PACKAGE_INTERVAL.
  DO LV_PAGES TIMES.
    REFRESH LT_USER_TR_LOG.
    CALL FUNCTION '/SKN/F_SW_O1_AUD_GET_TR_SM20'
      EXPORTING
        DEST                        = LV_SW_DEST
        D_FROM                      = LV_DATE_FROM
        D_TO                        = LV_DATE_TO
      TABLES
*       T_USERS                     =
        T_USER_TR_LOG               = LT_USER_TR_LOG
      EXCEPTIONS
        COMMUNICATION_FAILURE       = 1
        OTHERS                      = 2.
    IF SY-SUBRC <> 0.
      CLEAR: LV_STAT_AUTH_OK.
    ENDIF.
    IF LV_STAT_AUTH_OK IS NOT INITIAL.
      DELETE LT_USER_TR_LOG WHERE TRN_EX NOT IN R_TCODE.
      DELETE LT_USER_TR_LOG WHERE USER NOT IN R_USERID.
      "sort lt_USER_TR_LOG by USER TRN_EX.
      LOOP AT LT_USER_TR_LOG INTO LS_USER_TR_LOG.
        MOVE-CORRESPONDING LS_USER_TR_LOG TO LS_DATA.
        APPEND LS_DATA TO LT_DATA.
        "---
        MOVE-CORRESPONDING LS_USER_TR_LOG TO LS_TCODE.
        APPEND LS_TCODE TO LT_TCODE.
        "---
        MOVE-CORRESPONDING  LS_USER_TR_LOG TO LS_USER_LIST.
        APPEND LS_USER_LIST TO LT_USER_LIST.
      ENDLOOP.
      "---
      SORT LT_TCODE BY TCODE.
      DELETE ADJACENT DUPLICATES FROM LT_TCODE COMPARING TCODE.
      "---
      SORT LT_USER_LIST BY USER.
      DELETE ADJACENT DUPLICATES FROM LT_USER_LIST COMPARING USER.
    ENDIF.
    LV_DATE_FROM = LV_DATE_TO + 1.
    IF LV_DATE_FROM > DATE_TO.
      EXIT.
    ENDIF.
    LV_DATE_TO = LV_DATE_FROM + LV_PACKAGE_INTERVAL.
    IF LV_DATE_TO > DATE_TO.
      LV_DATE_TO = DATE_TO.
    ENDIF.
  ENDDO.
      "-- Get Users List
      CALL FUNCTION '/SKN/F_SW_O1_AUD_GET_TR_LIST'
        EXPORTING
          DEST            = LV_SW_DEST
          LANGU           = LV_LANGU
        TABLES
          T_TR_COND       = LT_TCODE
          T_TR_LIST       = LT_ACT_TR_LIST
*         T_TR_RNG        =
                .
      SORT LT_ACT_TR_LIST BY TCODE.
   LOOP AT LT_DATA INTO LS_DATA.
     SY_TABIX = SY-TABIX.
     READ TABLE LT_ACT_TR_LIST INTO LS_ACT_TR_LIST
                               WITH KEY TCODE = LS_DATA-TCODE
                               BINARY SEARCH.
     IF SY-SUBRC IS INITIAL.
       LS_DATA-TTEXT = LS_ACT_TR_LIST-TTEXT.
     ENDIF.
     "---
     LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
          EXPORTING
            D_FROM            = LS_DATA-MSCDATE
            T_FROM            = LS_DATA-MSCTIME
            D_TO              = SY_DATLO
            T_TO              = SY_TIMLO
            TIME_UNIT         = LV_DURATION_UNIT
          IMPORTING
            TIME_DIFF         = TIME_DIFF
          EXCEPTIONS
            WRONG_VALUE       = 1
            OTHERS            = 2    .
        IF SY-SUBRC = 0.
          LS_DATA-DURATION = TIME_DIFF .
        ELSE.
          LS_DATA-DURATION = '999999'.
        ENDIF.
     MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
   ENDLOOP.
  DELETE LT_DATA WHERE DURATION NOT IN R_DURATION.
 T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
