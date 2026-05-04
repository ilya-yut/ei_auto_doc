# Exception Indicator: Transactions Start (SM20 based) - SW_AUD_SM20_TR_START

## General Overview

This Exception Indicator (EI) monitors transaction starts recorded in SM20 security logs and identifies execution patterns that require security attention. It analyzes user and transaction activity within a configurable time window, enriches events with transaction descriptions, and calculates elapsed duration from transaction start to current run time.

This EI serves as an essential control for security monitoring and access governance by:
- Enabling detection of suspicious or high-risk transaction starts across users and time windows
- Supporting rapid triage of transaction execution activity with user, client, and timestamp context
- Providing visibility into elapsed time since transaction-start events for aging-based investigation prioritization
- Reducing monitoring noise by excluding generic launcher/session transactions from analysis
- Supporting periodic compliance review with reproducible event-level security evidence

This monitoring helps organizations improve incident detection, strengthen access control oversight, and prioritize remediation of risky user activity. It is particularly useful for continuous SOC review, internal audit support, and post-incident forensic analysis.

The EI retrieves SM20-based transaction-start records, enriches transaction text by language, and applies duration-based filtering for focused exception output.


## Problem Description

Failure to monitor SM20 transaction-start activity creates multiple risks across security operations, compliance, and management oversight.

**Security and Access-Control Risks**
- Suspicious transaction starts may go undetected, increasing exposure to unauthorized or abusive activity
- Delayed detection of risky user-transaction combinations can extend incident dwell time
- High-volume or repeated transaction starts by privileged users can remain unnoticed without focused monitoring
- Missing duration-based prioritization can slow response to potentially active threats

**Compliance and Audit Risks**
- Lack of consistent event-level monitoring weakens evidence for access-governance controls
- Incomplete visibility into who started which transactions and when can create audit traceability gaps
- Inconsistent exclusion handling for non-business launcher codes can distort control effectiveness reporting
- Weak trend visibility over monitoring windows reduces confidence in periodic control attestations

**Management Visibility and Decision-Making Risks**
- Without structured SM20 transaction-start monitoring, management lacks clear risk prioritization signals
- Untracked execution trends reduce effectiveness of preventive role and policy improvements
- Delayed escalation of significant transaction-start anomalies can impact operational resilience

### Suggested Resolution

**Immediate Response**
- Review flagged user-transaction start events and validate business legitimacy
- Prioritize events involving sensitive transactions, repeated execution patterns, or privileged users
- Confirm whether activity is expected operational behavior or requires escalation
- Triage by elapsed duration to focus on active or unresolved risk scenarios

**System Assessment**
- Analyze event distribution by user, transaction code, and date/time to identify recurring risk patterns
- Validate configuration of lookback window, package interval, and duration unit for coverage quality
- Verify language/text enrichment so investigators can interpret transaction context quickly
- Confirm exclusion list behavior for non-business launcher/session transactions

**Corrective Actions**
- Remediate unauthorized or unjustified transaction access through role/authorization adjustments
- Strengthen monitoring thresholds and review cadence for high-risk user populations
- Document investigations and closures for compliance traceability
- Schedule recurring EI execution and governance review cycles for continuous transaction-start oversight
- Feed recurring findings into control-hardening and preventive IAM improvements


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 3 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 4 | INSTANCENAME | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 5 | LANGU | Description Language |  | 0 | 0 |  |  |
| 6 | MANDT | Client ID | CLNT | 3 | 0 | SYMANDT | MANDT |
| 7 | MSCDATE | Transaction Start Date | DATS | 8 | 0 | ALDATE | DATUM |
| 8 | MSCTIME | Time | TIMS | 6 | 0 | ALTIME | TIME |
| 9 | PACKAGE_INTERVAL | Package size (days) |  | 0 | 0 |  |  |
| 10 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 11 | TRN_BY | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 12 | TRN_EX | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 13 | TTEXT | Transaction Text | CHAR | 36 | 0 | TTEXT_STCT | TEXT36 |
| 14 | USER | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 15 | USERID | User | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 15 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Days Backward from today):

Defines fallback lookback days when no explicit event-date range is provided.

**DURATION** (Duration In Time Units):

Elapsed-time threshold applied after runtime duration calculation.

**DURATION and DURATION_UNIT Connection:**

**DURATION** defines the elapsed threshold, while **DURATION_UNIT** determines whether the threshold is interpreted in minutes, hours, days, or full-day logic.

**DURATION_UNIT** (Duration Unit):

Unit used to calculate and interpret elapsed duration values.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for exact day semantics

**INSTANCENAME** (Server Name):

Transaction-start monitoring attribute **INSTANCENAME** used to constrain or enrich SM20-based results.

**LANGU** (Description Language):

Language key used while resolving transaction descriptions.

**MANDT** (Client ID):

Transaction-start monitoring attribute **MANDT** used to constrain or enrich SM20-based results.

**MSCDATE** (Transaction Start Date):

Event date filter for transaction-start records.

**MSCTIME** (Time):

Event time information paired with event dates for elapsed calculations.

**PACKAGE_INTERVAL** (Package size (days)):

Controls date-range chunk size for package retrieval from SM20 source data.

**TCODE** (Transaction Code):

Transaction code selector with built-in exclusions for framework/system entries.

**TRN_BY** (Transaction Code):

Transaction-start monitoring attribute **TRN_BY** used to constrain or enrich SM20-based results.

**TRN_EX** (Transaction Code):

Executed transaction identifier captured in SM20 log rows.

**TTEXT** (Transaction Text):

Resolved transaction short text for analyst-readable context.

**USER** (User):

Result-side user field used for visibility and downstream filtering.

**USERID** (User):

User selector applied to transaction-start log extraction.


### Parameter Relationship

How parameter combinations work together

**Date Window and Packaging:**

- **MSCDATE** defines the primary transaction-start date range.
- When **MSCDATE** is not provided, **BACKDAYS** is used to build a default window from today minus `BACKDAYS` to today.
- **PACKAGE_INTERVAL** splits the selected date range into smaller chunks for retrieval and processing.

**Time and Duration Parameters:**

- **MSCTIME** stores event time for each transaction-start record.
- **DURATION_UNIT** controls the unit used for elapsed-time calculation.
- **DURATION** acts as a threshold filter after duration is calculated.
- Duration is calculated from **MSCDATE + MSCTIME** to current run timestamp.

**User and Transaction Filtering:**

- **USERID** is applied as input filter on extracted SM20 records.
- **TCODE** is applied as transaction filter, with additional built-in exclusions for launcher/system transactions.
- **USER**, **TRN_EX**, and **TTEXT** are output fields supporting analyst review and downstream filtering.

**Language and Description Enrichment:**

- **LANGU** controls the language used when resolving transaction text descriptions.
- **TTEXT** enrichment depends on successful transaction list resolution and language selection.

**Contextual Output Dimensions:**

- **MANDT** and **INSTANCENAME** provide client/server context for each event.
- These dimensions support forensic traceability when correlating events across systems.


### Default Values
- **BACKDAYS** — Default: `1` (used when no explicit **MSCDATE** range is provided).
- **DURATION_UNIT** — Default: `D` (duration is calculated in days unless specified otherwise).
- **PACKAGE_INTERVAL** — Default: `10` (date retrieval is processed in 10-day chunks when not supplied).

### Practical Example of Parameter Configuration
**Use Case 1: Daily monitoring of recent transaction starts for selected users**

```plaintext
BACKDAYS = 3
USERID = USER_A
TCODE = SE38
DURATION = 1
DURATION_UNIT = D
LANGU = E
```

**Purpose:** Monitor recent transaction starts for selected users and quickly surface activity that exceeds the configured elapsed-time threshold.

**Use Case 2: Date-bound review with package chunking for higher data volumes**

```plaintext
MSCDATE = 20260301-20260315
PACKAGE_INTERVAL = 5
TCODE = SU01
USERID = BASIS01
DURATION = 0
DURATION_UNIT = D
```

**Purpose:** Analyze a fixed review period in smaller retrieval chunks to support stable processing for higher-volume SM20 datasets.

**Use Case 3: Broad monitoring focused on elapsed-time outliers**

```plaintext
MSCDATE = 20260310-20260316
DURATION = 2
DURATION_UNIT = D
LANGU = E
```

**Purpose:** Identify older unresolved transaction-start events within a recent date window and prioritize investigation based on elapsed duration.


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_AUD_SM20_TR_START | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_AUD_SM20_TR_START | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_AUD_SM20_TR_START | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_AUD_SM20_TR_START | MANDT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_AUD_SM20_TR_START | MSCDATE | Alert: date | DATS(8) | ALDATE |
| /SKN/S_SW_01_AUD_SM20_TR_START | MSCTIME | Alert: Time value in timeformat | TIMS(6) | ALTIME |
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
