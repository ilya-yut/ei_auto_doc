# Exception Indicator: List of Logged On Users - SW_01_20_AL08

## General Overview

This Exception Indicator (EI) monitors currently logged-on SAP users across application servers using the AL08 runtime session view. It consolidates user identity, transaction context, connection channel details, terminal/network metadata, and memory-related session indicators so Basis and security teams can detect abnormal live activity quickly.

This EI serves as an essential control for SAP operations and runtime security by:

- Enabling early detection of unusual active-session patterns across users, terminals, and transaction activity before service quality degrades
- Supporting separation of GUI, RFC, and plugin-style sessions so operational teams can distinguish business usage from technical integration traffic
- Providing visibility into high-memory or long-lived sessions that may indicate runaway jobs, unstable clients, or infrastructure contention
- Helping incident responders correlate live session status with user identity and host/network context during triage
- Giving administrators a repeatable, monitor-aligned extract for daily operational oversight and post-incident review

Typical uses include peak-time stability checks, suspicious-session investigations, connection-channel hygiene reviews, and recurring runtime governance cycles by Basis and security operations. Results support prioritization of remediation work and evidence-based escalation.

The function retrieves active-session data from standard runtime server/user interfaces, enriches rows with name details and host-to-IP conversion where possible, then applies post-processing elapsed-time and session filters.


## Problem Description

Failure to monitor live logged-on users and active session behavior creates multiple risks across system stability, security operations, and management visibility:

**System Stability and Performance Risks**

- Resource-heavy sessions can remain unnoticed until response times degrade for critical business activities
- Session surges on specific servers may hide balancing or infrastructure issues when not reviewed in one consolidated view
- Unstable client or terminal patterns can recur without root-cause action when runtime evidence is fragmented
- Long-lived sessions may consume memory and work process capacity beyond expected operational windows
- Runtime bottlenecks discovered late increase emergency interventions and service disruption risk

**Security and Access Monitoring Risks**

- Unexpected connection channels can mask unauthorized activity when live session context is not reviewed regularly
- Shared or technical users may execute sensitive transactions without timely operational scrutiny
- Missing host and terminal correlation slows triage of suspicious logged-on behavior
- Lack of consolidated active-session evidence weakens early detection in incident response workflows
- Teams may overlook abnormal persistence of sessions that should have ended after standard task completion

**Management Visibility and Decision-Making Risks**

- Leadership lacks a reliable snapshot of real-time user activity distribution by user, transaction, and channel
- Basis and security teams cannot prioritize remediation effectively when evidence is split across ad hoc checks
- Incident timelines become harder to reconstruct when session details are collected manually and inconsistently
- Capacity planning is less accurate when sustained active-session pressure is not measured in repeatable cycles
- Cross-team escalation slows when one report does not combine identity, technical context, and runtime footprint

## Suggested Resolution

**Immediate Response**

- Review flagged live sessions and confirm business legitimacy with process owners and support teams
- Isolate suspicious or resource-intensive sessions and decide quickly whether containment actions are required
- Validate unexpected channel usage against approved integration and access models
- Capture output evidence for incident records and operational follow-up
- Use standard SAP operational transactions to inspect impacted users and runtime context before corrective action

**System Assessment**

- Compare current active-session distribution against normal operating baselines by server and channel
- Analyze memory-intensive patterns to identify recurring technical causes
- Correlate anomalies with recent deployments, transport windows, or infrastructure changes
- Validate governance rules for technical users, shared IDs, and remote connections
- Document recurring runtime patterns that indicate systemic rather than one-off issues

**Corrective Actions**

- Tune runtime/session management controls to reduce repeated high-load outliers
- Correct client, integration, or infrastructure configurations driving unstable session behavior
- Adjust monitoring scope and cadence with Basis/security stakeholders for high-risk periods
- Train first-line responders to interpret identity, channel, and memory indicators consistently
- Integrate recurring findings into formal operational governance and problem-management workflows


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BNAME | User | CHAR | 12 | 0 | UBNAME | UBNAME |
| 2 | DATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 3 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 4 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 5 | EXTMODI | Modes | INT4 | 10 | 0 | UMODE |  |
| 6 | GUIVERSION | GUIVERSION | CHAR | 10 | 0 | CHAR10 | CHAR10 |
| 7 | HOSTADR | IP address | RAW | 4 | 0 | MSHOSTADR | RAW4 |
| 8 | INTMODI | Modes | INT4 | 10 | 0 | UMODE |  |
| 9 | IP_ADDRESS | Terminal | CHAR | 36 | 0 | XUTERMINAL | XUTERMINAL |
| 10 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 11 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 12 | MANDT | Client | CLNT | 3 | 0 | MANDT | MANDT |
| 13 | MASTER | Master | CHAR | 12 | 0 | UMASTER | UMASTER |
| 14 | MEMSUM | 64 Bit Integer with Prefix | DEC | 20 | 0 | ABAP_MSIZE | ABAP_MSIZE |
| 15 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 16 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 17 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 18 | PAGE | 64 Bit Integer with Prefix | DEC | 20 | 0 | ABAP_MSIZE | ABAP_MSIZE |
| 19 | PRIVSUM | 64 Bit Integer with Prefix | DEC | 20 | 0 | ABAP_MSIZE | ABAP_MSIZE |
| 20 | PROTOCOL | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 21 | RFCDEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 22 | RFC_TYPE | RFC | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 23 | ROLL | 64 Bit Integer with Prefix | DEC | 20 | 0 | ABAP_MSIZE | ABAP_MSIZE |
| 24 | STAT | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 25 | TCODE | Transaction | CHAR | 20 | 0 | UTCODE | TCODE |
| 26 | TERM | Terminal | CHAR | 20 | 0 | UTERM | CHAR20 |
| 27 | TID | Terminal ID | INT4 | 10 | 0 | UTID | UTID |
| 28 | TOTAL_MEM_MB | Natural number | INT4 | 10 | 0 | INT4 | INT4 |
| 29 | TRACE | User trace | INT1 | 3 | 0 | USER_TRACE | USER_TRACE |
| 30 | TYPE | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 31 | US_GUI | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 32 | US_PLUGIN | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 33 | US_RFC | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 34 | ZEIT | Time | TIMS | 6 | 0 | SYUZEIT | SYTIME |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 34 parameters listed in the Parameters Reference Table above.

**BNAME** (User):

User name returned by the active-session reader; use it to focus monitoring on named accounts.

**DATE** (Date):

Calculated logon date used in the post-processing span calculation before elapsed-time filtering.

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

Destination name returned on each line for routed server collection context.

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

Terminal/session ID key for correlating auxiliary metrics (for example memory list lookup).

**TOTAL_MEM_MB** (Natural number):

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

**Session identity and context**

- **BNAME**, **MANDT**, **TCODE**, **TERM**, and **TID** jointly describe who is logged on, in which client, under which transaction and terminal/session context.
- **NAME_FIRST**, **NAME_LAST**, and **NAME_TEXT** enrich **BNAME** for readable operational investigations.

**Connection and channel classification**

- **TYPE** is used to derive **US_GUI**, **US_RFC**, and **US_PLUGIN** flags in processing logic.
- **PROTOCOL** and **RFC_TYPE** complement channel interpretation for live-session triage.

**Host and network traceability**

- **HOSTADR** is taken from session data; **IP_ADDRESS** is derived through host-to-IP conversion when host information is present.

**Elapsed-time filtering**

- **ZEIT** and calculated **DATE** feed elapsed-time computation for each row.
- **DURATION** and **DURATION_UNIT** work together: the unit defines measurement semantics and duration selection keeps matching rows.

**Runtime footprint**

- **TOTAL_MEM_MB**, **MEMSUM**, **PRIVSUM**, **ROLL**, and **PAGE** together describe memory pressure and can be interpreted jointly for capacity-related alerts.


### Default Values

- **DURATION_UNIT** — Default: `M` (minutes), assigned in code before caller selections are read.

### Practical Configuration Examples

**Use Case 1: Long-running RFC or plugin sessions**

```
DURATION = 120
DURATION_UNIT = M
US_RFC = X
US_PLUGIN = X
```

**Purpose:** Highlights technical channel sessions that remain active longer than expected and may require integration-side investigation.

**Use Case 2: Full-day stale sessions in admin transactions**

```
DURATION = 1
DURATION_UNIT = F
TCODE = SM*
```

**Purpose:** Flags sessions persisting across day boundaries in administrative transaction families for closer review.

**Use Case 3: Memory-intensive active sessions**

```
TOTAL_MEM_MB = 1024 - 999999
TERM = APP*
BNAME = *
```

**Purpose:** Surfaces high-memory sessions associated with specific terminal patterns for runtime capacity troubleshooting.

**Use Case 4: User-and-channel scoped monitoring**

```
BNAME = SAP*
DURATION = 30
DURATION_UNIT = M
US_GUI = X
```

**Purpose:** Focuses on GUI sessions for a user naming pattern that exceed a 30-minute threshold, useful for targeted session hygiene checks.


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
