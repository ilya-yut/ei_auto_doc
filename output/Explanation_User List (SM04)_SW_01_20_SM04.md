# Exception Indicator: User List (SM04) - SW_01_20_SM04

## General Overview

This Exception Indicator (EI) monitors active SAP user sessions from the SM04/AL08 runtime view across local and remote application servers, including user identity, transaction context, terminal metadata, and session resource footprint. It helps operations and security teams identify unusual session behavior, unstable client patterns, and high-memory usage while preserving a user-centric view for investigation.

This EI serves as an essential control for SAP operations and security monitoring by:

- Enabling early detection of abnormal active-session patterns across users, terminals, and transaction activity before performance or security impact escalates
- Supporting analysis of interactive, RFC, and plugin session channels so teams can separate business workload from technical integration noise
- Providing visibility into memory-intensive sessions that may signal runaway activity, misconfigured clients, or resource contention on app servers
- Helping teams correlate runtime session status with user identity and terminal attributes during incident response and root-cause analysis
- Giving administrators a repeatable operational extract aligned with real-time session lists used in day-to-day Basis monitoring

Typical uses include peak-hour stability checks, post-incident forensic review, session hygiene audits for shared environments, and recurring runtime health reviews by Basis and security teams. Results support prioritization of corrective actions and evidence for operational governance.

The function reads server/session user lists through standard runtime interfaces, enriches lines with user-detail names and optional host-to-IP conversion, and then applies post-processing time-span screening and session filters.


## Problem Description

Failure to monitor active SAP user sessions and runtime behavior creates multiple risks across system stability, security, and operational decision-making:

**System Stability and Performance Risks**

- High-memory or excessive mode activity can accumulate silently until response times degrade for critical business users
- Session surges on specific servers may indicate imbalance or technical bottlenecks that are missed without structured monitoring
- Unnoticed client/version inconsistencies can increase support incidents and intermittent GUI instability
- Resource-heavy sessions discovered too late can trigger avoidable outages or emergency restarts
- Inability to compare terminal and protocol patterns across active users delays technical root-cause analysis

**Security and Access Monitoring Risks**

- Unexpected session channels can mask unauthorized access paths when runtime context is not reviewed regularly
- Shared or generic accounts may run sensitive transactions without timely visibility into where and how they connect
- Lack of consolidated session identity details slows investigations into suspicious runtime behavior
- Missing visibility into terminal and network origin hinders triage of potentially compromised sessions
- Operational teams may overlook long-lived suspicious sessions when no threshold-based screening is applied

**Management Visibility and Decision-Making Risks**

- Leadership lacks a reliable view of live user activity concentration by client, transaction, and channel
- Basis and security teams struggle to prioritize remediation when runtime anomalies are spread across ad hoc reports
- Incident timelines become harder to reconstruct when session data is gathered manually and inconsistently
- Capacity planning suffers when active-session pressure trends are not captured in repeatable monitoring cycles
- Cross-team escalation slows when evidence does not combine user identity, session state, and technical footprint in one output

## Suggested Resolution

**Immediate Response**

- Review flagged active sessions and validate business legitimacy with process owners and support teams
- Isolate suspicious or resource-heavy sessions and confirm whether emergency termination or containment is required
- Verify unusual connection channels and terminal patterns against approved integration and access models
- Capture evidence from monitoring output for incident records and post-mortem analysis
- Use standard operational transactions to inspect impacted users, sessions, and server context before taking corrective action

**System Assessment**

- Compare current session distributions by server, channel, and transaction to baseline operating windows
- Analyze concentration of heavy sessions to identify recurring capacity hotspots or misconfigured clients
- Correlate runtime anomalies with recent deployments, transports, or infrastructure changes
- Validate session governance standards for shared IDs, technical users, and remote destinations
- Document systemic runtime patterns that require architecture or operations changes instead of one-off fixes

**Corrective Actions**

- Tune server/session management practices and user behavior controls to reduce recurring heavy-session outliers
- Correct integration and client configurations that drive unstable or unauthorized session patterns
- Adjust monitoring scope and scheduling with Basis/security stakeholders to match peak-risk windows
- Train first-line responders to interpret session identity, channel, and memory indicators consistently
- Retain remediation evidence and integrate repeated findings into operational governance and problem-management workflows


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BNAME | User | CHAR | 12 | 0 | UBNAME | UBNAME |
| 2 | DATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 3 | DEST | RFC Destination |  | 0 | 0 |  |  |
| 4 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 5 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 6 | EXTMODI | Modes | INT4 | 10 | 0 | UMODE |  |
| 7 | GUIVERSION | GUIVERSION | CHAR | 10 | 0 | CHAR10 | CHAR10 |
| 8 | HOSTADR | IP address | RAW | 4 | 0 | MSHOSTADR | RAW4 |
| 9 | INTMODI | Modes | INT4 | 10 | 0 | UMODE |  |
| 10 | IP_ADDRESS | Terminal | CHAR | 36 | 0 | XUTERMINAL | XUTERMINAL |
| 11 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 12 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 13 | MANDT | Client | CLNT | 3 | 0 | MANDT | MANDT |
| 14 | MASTER | Master | CHAR | 12 | 0 | UMASTER | UMASTER |
| 15 | MEMSUM | 64 Bit Integer with Prefix | DEC | 20 | 0 | ABAP_MSIZE | ABAP_MSIZE |
| 16 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 17 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 18 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 19 | PAGE | 64 Bit Integer with Prefix | DEC | 20 | 0 | ABAP_MSIZE | ABAP_MSIZE |
| 20 | PRIVSUM | 64 Bit Integer with Prefix | DEC | 20 | 0 | ABAP_MSIZE | ABAP_MSIZE |
| 21 | PROTOCOL | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 22 | RFCDEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 23 | RFC_TYPE | RFC | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 24 | ROLL | 64 Bit Integer with Prefix | DEC | 20 | 0 | ABAP_MSIZE | ABAP_MSIZE |
| 25 | STAT | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 26 | TCODE | Transaction | CHAR | 20 | 0 | UTCODE | TCODE |
| 27 | TERM | Terminal | CHAR | 20 | 0 | UTERM | CHAR20 |
| 28 | TID | Terminal ID | INT4 | 10 | 0 | UTID | UTID |
| 29 | TOTAL_MEM_MB | Total Memory (MB) | INT4 | 10 | 0 | INT4 | INT4 |
| 30 | TRACE | User trace | INT1 | 3 | 0 | USER_TRACE | USER_TRACE |
| 31 | TYPE | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 32 | US_GUI | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 33 | US_PLUGIN | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 34 | US_RFC | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 35 | ZEIT | Time | TIMS | 6 | 0 | SYUZEIT | SYTIME |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 35 parameters listed in the Parameters Reference Table above.

**BNAME** (User):

User name returned by the active-session reader; use it to focus monitoring on named accounts.

**DATE** (Date):

Calculated logon date used in the post-processing span calculation before duration filtering.

**DEST** (RFC Destination):

RFC destination used when session data is read remotely (especially cloud/targeted destination mode).

**DURATION** (Duration In Time Units):

Numeric threshold applied after elapsed-time calculation for each session row.

**DURATION and DURATION_UNIT Connection:**

The function computes elapsed session age in **DURATION_UNIT**, then keeps rows whose computed value fits the **DURATION** selection range.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit for elapsed-time calculation and filtering (hours, minutes, days, or full-day slices).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**EXTMODI** (Modes):

External mode count or marker on the active session line.

**GUIVERSION** (GUIVERSION):

GUI version reported for the user session; supports client footprint checks.

**HOSTADR** (IP address):

Host address returned for the active user session.

**INTMODI** (Modes):

Internal mode count or marker on the active session line.

**IP_ADDRESS** (Terminal):

IP address resolved from host information through the conversion helper when available.

**HOSTADR and IP_ADDRESS Connection:**

`HOSTADR` comes from session data; `IP_ADDRESS` is derived from host conversion logic when host data is present.

**LANGU** (Language for texts):

Language key for text resolution in user detail enrichment.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

Controls whether current date/time anchor handling is managed in UTC mode.

**MANAGE_IN_UTC Options:**
- **X** — Enable the behavior described by the flag.
- ** ** (space) — Leave unset for this run.

**MANDT** (Client):

Client value attached to the active session row.

**MASTER** (Master):

Master terminal/server marker from session information.

**MEMSUM** (64 Bit Integer with Prefix):

Memory metric from the session/memory collection block.

**NAME_FIRST** (First Name):

Resolved first name for the logged-on user.

**NAME_LAST** (Last Name):

Resolved last name for the logged-on user.

**NAME_FIRST and NAME_LAST Connection:**

Together they provide a readable user identity while **BNAME** remains the technical key.

**NAME_TEXT** (Full Name):

Resolved full name for readable dashboards.

**PAGE** (64 Bit Integer with Prefix):

Memory page metric from the enriched session output.

**PRIVSUM** (64 Bit Integer with Prefix):

Private-memory metric from the session output.

**PROTOCOL** (Natural Number):

Session protocol code for connection/channel analysis.

**RFCDEST** (RFC Destination):

Destination name returned on each line; pairs with **DEST** input when routing collection.

**RFC_TYPE** (RFC):

RFC channel/type indicator on the active session line.

**ROLL** (64 Bit Integer with Prefix):

Roll-memory metric from session details.

**STAT** (Natural Number):

Session status code as delivered by the user list function.

**TCODE** (Transaction):

Current transaction code in the user session.

**TERM** (Terminal):

Terminal identifier shown in active session data.

**TID** (Terminal ID):

Terminal/session ID key for correlating auxiliary metrics (e.g., memory list lookup).

**TOTAL_MEM_MB** (Total Memory (MB)):

Total memory metric in MB captured for the session.

**TRACE** (User trace):

User trace marker from active-session context.

**TYPE** (Natural Number):

Session type code used to derive GUI/RFC/plugin flags.

**US_GUI** (Single-Character Flag):

Derived flag indicating GUI session type.

**US_GUI Options:**
- **X** — Enable the behavior described by the flag.
- ** ** (space) — Leave unset for this run.

**US_PLUGIN** (Single-Character Flag):

Derived flag indicating plugin-style session type.

**US_PLUGIN Options:**
- **X** — Enable the behavior described by the flag.
- ** ** (space) — Leave unset for this run.

**US_RFC** (Single-Character Flag):

Derived flag indicating RFC session type.

**US_RFC Options:**
- **X** — Enable the behavior described by the flag.
- ** ** (space) — Leave unset for this run.

**ZEIT** (Time):

Session time used as part of elapsed-time calculation.


### Parameter Relationships

**Session identity and location**

- **BNAME**, **MANDT**, **TCODE**, **TERM**, and **TID** together define who is active, in which client, what transaction context is open, and which terminal/session identity is involved.
- **HOSTADR** and **IP_ADDRESS** are related: host data is collected from session runtime info, and IP may be derived from host conversion logic when host data is available.

**Session channel classification**

- **TYPE** drives derived flags such as **US_GUI**, **US_RFC**, and **US_PLUGIN** in the processing logic.
- **PROTOCOL** and **RFC_TYPE** complement those flags by describing connection/channel characteristics for runtime triage.

**Elapsed session-age filtering**

- **ZEIT** and calculated **DATE** feed the elapsed-time computation block.
- **DURATION** and **DURATION_UNIT** work as a pair: the unit defines how elapsed time is measured, and the duration selection keeps rows whose computed value matches thresholds.

**Operational routing and destination context**

- **DEST** influences which server destination is queried in runtime calls; **RFCDEST** is returned as line-level context in the output.
- **MANAGE_IN_UTC** affects how the current time anchor is interpreted before elapsed-time calculations.

**User readability**

- **NAME_FIRST**, **NAME_LAST**, and **NAME_TEXT** are enriched from user-detail lookup and should be read together with **BNAME** for human-friendly investigations.


### Default Values

- **DURATION_UNIT** — Default: `M` (minutes), assigned in code before caller selections are read.

### Practical Configuration Examples

**Use Case 1: Long-lived RFC/plugin sessions**

```
DURATION = 120
DURATION_UNIT = M
US_RFC = X
US_PLUGIN = X
```

**Purpose:** Highlights technical channel sessions that remain active longer than expected and may require integration-side cleanup.

**Use Case 2: Full-day stale sessions by transaction focus**

```
DURATION = 1
DURATION_UNIT = F
TCODE = SM*
```

**Purpose:** Flags sessions that effectively persist across day boundaries in technical/admin transaction families for closer review.

**Use Case 3: Memory-heavy active users on specific terminal patterns**

```
TOTAL_MEM_MB = 1024 - 999999
TERM = APP*
BNAME = *
```

**Purpose:** Surfaces high-memory sessions linked to a terminal prefix pattern, useful for runtime capacity investigations.

**Use Case 4: Destination-scoped monitoring window**

```
DEST = PROD_RFC
DURATION = 30
DURATION_UNIT = M
```

**Purpose:** Limits monitoring to one destination and focuses on sessions older than 30 minutes, supporting targeted server checks.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_AL08 | BNAME | User Name | CHAR(12) | UBNAME |
| /SKN/S_SW_01_01_AL08 | DATE | Date | DATS(8) | DATUM |
| /SKN/S_SW_01_01_AL08 | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_AL08 | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_AL08 | EXTMODI | Task Handler: Number of External or Internal Modes | INT4(10) | UMODE |
| /SKN/S_SW_01_01_AL08 | GUIVERSION | Character Field Length = 10 | CHAR(10) | CHAR10 |
| /SKN/S_SW_01_01_AL08 | HOSTADR | Host IP address | RAW(4) | MSHOSTADR |
| /SKN/S_SW_01_01_AL08 | INTMODI | Task Handler: Number of External or Internal Modes | INT4(10) | UMODE |
| /SKN/S_SW_01_01_AL08 | IP_ADDRESS | Terminal | CHAR(36) | XUTERMINAL |
| /SKN/S_SW_01_01_AL08 | MANDT | Client | CLNT(3) | MANDT |
| /SKN/S_SW_01_01_AL08 | MASTER | Master | CHAR(12) | UMASTER |
| /SKN/S_SW_01_01_AL08 | MEMSUM | DEC type that is suitable for 64 bit integer | DEC(20) | ABAP_MSIZE |
| /SKN/S_SW_01_01_AL08 | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_01_01_AL08 | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_01_01_AL08 | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_01_01_AL08 | PAGE | DEC type that is suitable for 64 bit integer | DEC(20) | ABAP_MSIZE |
| /SKN/S_SW_01_01_AL08 | PRIVSUM | DEC type that is suitable for 64 bit integer | DEC(20) | ABAP_MSIZE |
| /SKN/S_SW_01_01_AL08 | PROTOCOL | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_01_AL08 | RFCDEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_01_AL08 | RFC_TYPE | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_01_01_AL08 | ROLL | DEC type that is suitable for 64 bit integer | DEC(20) | ABAP_MSIZE |
| /SKN/S_SW_01_01_AL08 | STAT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_01_AL08 | TCODE | TCODE | CHAR(20) | UTCODE |
| /SKN/S_SW_01_01_AL08 | TERM | Terminal ID | CHAR(20) | UTERM |
| /SKN/S_SW_01_01_AL08 | TID | Terminal ID | INT4(10) | UTID |
| /SKN/S_SW_01_01_AL08 | TOTAL_MEM_MB | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_01_AL08 | TRACE | User trace | INT1(3) | USER_TRACE |
| /SKN/S_SW_01_01_AL08 | TYPE | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_01_AL08 | US_GUI | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_01_01_AL08 | US_PLUGIN | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_01_01_AL08 | US_RFC | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_01_01_AL08 | ZEIT | System Time | TIMS(6) | SYUZEIT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_AL08.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_AL08 OPTIONAL
*"----------------------------------------------------------------------
  DATA_MULTY: DURATION     /SKN/E_SW_DURATION,
              TID          UTID,
              MANDT        MANDT,
              BNAME        UBNAME,
              TCODE        UTCODE,
              TERM         UTERM,
              ZEIT         UDTIME,
              MASTER       UMASTER,
              HOSTADR      MSHOSTADR,
              IP_ADDRESS   XUTERMINAL,   " 09/23++
              TYPE         INT4,
              STAT         INT4,
              PROTOCOL     INT4,
              GUIVERSION   CHAR10,
              RFC_TYPE     CHAR1,
              TOTAL_MEM_MB INT4.
  DATA_SINGLE: LANGU          LANGU,
               NUMBER         SY-TABIX,
               SUBRC          SY-SUBRC,
               MANAGE_IN_UTC  CHAR1,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               DEST           RFCDEST.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
  DATA : TIME_DIFF TYPE  INT4 .
  DATA : IS_OUT(1) TYPE C.
  DATA: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
**** Begin 09/23++
  DATA: LV_USERNAME TYPE XUBNAME.
**** End 09/23++
  DATA: LS_DESTI TYPE RFCHOSTS,
        LT_DESTI LIKE TABLE OF LS_DESTI.
  DATA: LS_SERVER_LIST TYPE MSXXLIST,
        LT_SERVER_LIST LIKE TABLE OF LS_SERVER_LIST.
  DATA: LS_USR_LISTE TYPE UINFO,
        LT_USR_LISTE LIKE TABLE OF LS_USR_LISTE.
  DATA: LV_DATUM TYPE D,
        LV_ZEIT TYPE T.
  DATA: LS_LIST_MEMO TYPE /SKN/S_SW_01_MEMO_INFO,
        LT_LIST_MEMO LIKE TABLE OF LS_LIST_MEMO.
  DATA: LV_NO_SERVER_LIST(1) TYPE C.
*-- Fill Selection Option Tables
  SELECT_MULTY: DURATION,
                TID,
                MANDT,
                BNAME,
                TCODE,
                TERM,
                ZEIT,
                MASTER,
                HOSTADR,
                IP_ADDRESS,    " 09/23++
                TYPE,
                STAT,
                PROTOCOL,
                GUIVERSION,
                RFC_TYPE,
                TOTAL_MEM_MB.
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT,
                 MANAGE_IN_UTC,
                 LANGU,
                 DEST.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_AL08'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
  TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  REFRESH LT_DESTI.
  CALL FUNCTION 'TH_SERVER_LIST'
    DESTINATION LV_DEST
    TABLES
      LIST           = LT_SERVER_LIST
    EXCEPTIONS
      NO_SERVER_LIST = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN 1.  " NO_SERVER_LIST
        LV_NO_SERVER_LIST = 'X'.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDIF.
  IF LV_NO_SERVER_LIST IS NOT INITIAL. " Add fictive Item
    LS_SERVER_LIST-NAME = LV_DEST.
    APPEND LS_SERVER_LIST TO LT_SERVER_LIST.
  ENDIF.
  LOOP AT LT_SERVER_LIST INTO LS_SERVER_LIST.
    LS_DESTI-RFCDEST = LS_SERVER_LIST-NAME.
    IF LV_DEST IS NOT INITIAL.  "--- Cloud Mode
      LS_DESTI-RFCDEST = LV_DEST.
    ENDIF.
    APPEND LS_DESTI TO LT_DESTI.
  ENDLOOP.
  LOOP AT LT_DESTI INTO LS_DESTI.
    REFRESH LT_USR_LISTE.
    CLEAR LS_USR_LISTE.
    CALL FUNCTION 'THUSRINFO'
      DESTINATION LS_DESTI
      TABLES
        USR_TABL              = LT_USR_LISTE
      EXCEPTIONS
        COMMUNICATION_FAILURE = 17
        SYSTEM_FAILURE        = 17.
    IF SY-SUBRC = '17'.
      "ERROR = 'X'.
    ENDIF.
    LOOP AT LT_USR_LISTE INTO LS_USR_LISTE.
      CLEAR T_DATA.
      MOVE-CORRESPONDING LS_DESTI TO T_DATA.
      MOVE-CORRESPONDING LS_USR_LISTE TO T_DATA.
      CASE LS_USR_LISTE-TYPE.                "EA 14.09.99
        WHEN 4.
          T_DATA-US_GUI = 'X'.
        WHEN 32.
          T_DATA-US_RFC = 'X'.
        WHEN 202.
          T_DATA-US_PLUGIN = 'X'.
        WHEN OTHERS.
      ENDCASE.
**** Begin 09/23++
      IF LS_USR_LISTE-HOSTADR IS NOT INITIAL AND
         LS_USR_LISTE-HOSTADR NE '00000000'.
        LV_USERNAME = LS_USR_LISTE-BNAME.
* Convert User's host address to IP address
        CALL FUNCTION '/SKN/F_SW_CONVERT_HOST_2_IP'
          EXPORTING
            IF_USERNAME   = LV_USERNAME
            IF_HOSTADR    = LS_USR_LISTE-HOSTADR
            DEST          = LV_DEST
          IMPORTING
            EF_IP_ADDRESS = T_DATA-IP_ADDRESS
          EXCEPTIONS
            OTHERS        = 1.
      ENDIF.
**** End 09/23++
      APPEND T_DATA.
    ENDLOOP.
    "----Build Memory List
    IF LV_DEST IS INITIAL.  "--- Non Cloud Mode
      CALL FUNCTION '/SKN/F_SW_01_MEMO_INFO'
        DESTINATION LS_DESTI
        TABLES
          LIST_MEMO = LT_LIST_MEMO.
      LOOP AT T_DATA.
        SY_TABIX = SY-TABIX.
        READ TABLE LT_LIST_MEMO INTO LS_LIST_MEMO
                                WITH KEY TID = T_DATA-TID.
        IF SY-SUBRC IS INITIAL.
          MOVE-CORRESPONDING LS_LIST_MEMO TO T_DATA.
          MODIFY T_DATA INDEX SY_TABIX.
        ENDIF.
      ENDLOOP.
    ENDIF.
    "----Build Memory List
  ENDLOOP.
*-- Fill Duration Value
  SY_DATLO = SY-DATUM.   " System Date/Time
  SY_TIMLO = SY-UZEIT.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX.
    LV_DATUM = SY_DATLO.
    LV_ZEIT = T_DATA-ZEIT.
    IF LV_ZEIT > SY_TIMLO.
      LV_DATUM = LV_DATUM - 1.
    ENDIF.
    T_DATA-DATE = LV_DATUM.
    T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = LV_DATUM
        T_FROM      = LV_ZEIT
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
  DELETE T_DATA WHERE TID          NOT IN R_TID.
  DELETE T_DATA WHERE MANDT        NOT IN R_MANDT.
  DELETE T_DATA WHERE BNAME        NOT IN R_BNAME.
  DELETE T_DATA WHERE TCODE        NOT IN R_TCODE.
  DELETE T_DATA WHERE TERM         NOT IN R_TERM.
  DELETE T_DATA WHERE ZEIT         NOT IN R_ZEIT.
  DELETE T_DATA WHERE MASTER       NOT IN R_MASTER.
  DELETE T_DATA WHERE HOSTADR      NOT IN R_HOSTADR.
  DELETE T_DATA WHERE IP_ADDRESS   NOT IN R_IP_ADDRESS.     " 09/23++
  DELETE T_DATA WHERE TYPE         NOT IN R_TYPE.
  DELETE T_DATA WHERE STAT         NOT IN R_STAT.
  DELETE T_DATA WHERE PROTOCOL     NOT IN R_PROTOCOL.
  DELETE T_DATA WHERE GUIVERSION   NOT IN R_GUIVERSION.
  DELETE T_DATA WHERE RFC_TYPE     NOT IN R_RFC_TYPE.
  DELETE T_DATA WHERE TOTAL_MEM_MB NOT IN R_TOTAL_MEM_MB.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX.
    CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES'
      EXPORTING
        BNAME      = T_DATA-BNAME
      IMPORTING
        NAME_FIRST = T_DATA-NAME_FIRST
        NAME_LAST  = T_DATA-NAME_LAST
        NAME_TEXT  = T_DATA-NAME_TEXT
      EXCEPTIONS
        NO_DATA    = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
