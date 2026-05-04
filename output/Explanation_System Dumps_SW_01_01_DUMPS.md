# Exception Indicator: System Dumps - SW_01_01_DUMPS

## General Overview

This Exception Indicator (EI) monitors ABAP runtime dumps (ST22) and helps identify recent dump events by time window, host, user, program, include, and dump ID.

This EI helps by:
- Highlighting dump-heavy periods before operational impact expands
- Segmenting dump events by technical ownership (user/program/include)
- Prioritizing recent dumps using duration-based aging
- Supporting control evidence for stability monitoring

The function retrieves dump events per day, applies technical filters, calculates elapsed duration from dump timestamp, and returns exception-focused records.


## Problem Description

Without structured dump monitoring, recurring runtime errors can accumulate and reduce system stability and business-process continuity.

**Operational and Process Risks**
- Repeated short dumps can disrupt background and dialog processing
- Root-cause analysis slows down when dump context is not segmented
- Critical dump bursts can remain unnoticed during high-load periods

**Control and Compliance Risks**
- Incomplete dump surveillance weakens control evidence quality
- Inconsistent review windows reduce comparability over time
- Delayed remediation can increase repeated-failure exposure

**Management Visibility Risks**
- Stability degradation may be detected too late
- Teams may miss host- or user-specific concentration patterns

### Suggested Resolution

**Immediate Response**
- Prioritize newest and highest-impact dump clusters
- Escalate recurring dump IDs and affected programs
- Assign ownership by user/host/program context

**System Assessment**
- Validate date/time and duration settings used in monitoring
- Analyze trend concentration by host, user, and program/include
- Review remediation outcomes for recurring runtime errors

**Corrective Actions**
- Standardize periodic dump monitoring cadence
- Tune alert thresholds for dump aging and recurrence
- Feed remediation findings into development and operations backlog


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 2 | DATUM | Date |  | 0 | 0 |  |  |
| 3 | DUMPID | Runtime Error | CHAR | 30 | 0 | DUMPID | CHAR30 |
| 4 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 5 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 6 | INCLUDENAME | Include Name | CHAR | 40 | 0 | PROGNAME | PROGNAME |
| 7 | LINENUMBER | Source Code Line of ABAP Program | CHAR | 5 | 0 | ABP_SLINE | SYCHAR05 |
| 8 | PROGRAMNAME | Program Name | CHAR | 40 | 0 | PROGNAME | PROGNAME |
| 9 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 10 | SYDATE | Date | DATS | 8 | 0 | SYDATUM | SYDATS |
| 11 | SYHOST | Host | CHAR | 32 | 0 | SYHOST | HOST_ID |
| 12 | SYTIME | Time | TIMS | 6 | 0 | SYUZEIT | SYTIME |
| 13 | SYUSER | User | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |
| 14 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 14 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Back Days):

BACKDAYS sets fallback lookback days when no explicit date range is provided.

**DATUM** (Date):

DATUM is the explicit date selector for dump retrieval window.

**DUMPID** (Runtime Error):

DUMPID restricts analysis to specific runtime error identifiers.

**DURATION** (Duration In Time Units):

DURATION sets elapsed-time threshold used after dump timestamp comparison with current time.

**DURATION_UNIT** (Duration Unit(D/H/M)):

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**INCLUDENAME** (Include Name):

INCLUDENAME narrows monitoring to includes related to problematic call stacks.

**LINENUMBER** (Source Code Line of ABAP Program):

LINENUMBER pinpoints source-code location context for technical root-cause correlation.

**PROGRAMNAME** (Program Name):

PROGRAMNAME targets ABAP program context to isolate unstable components.

**SW_DEST** (Cloud Destination):

SW_DEST controls optional cloud-destination execution path for this monitoring logic.

**SYDATE** (Date):

SYDATE explicitly selects dump event dates used for retrieval.

**SYHOST** (Host):

SYHOST segments dump events by application host to detect node-specific instability.

**SYTIME** (Time):

SYTIME refines intraday time slicing inside selected dump dates.

**SYUSER** (User):

SYUSER narrows events by user context to support ownership-based investigation.

**USER_FLD** (Dynamic Recipient User Field):

USER_FLD is a dynamic recipient/user context selector; fixed values are implementation-dependent.

**USER_FLD Options:**
- No fixed USER_FLD value list is defined in the available code for this EI.


### Parameter Relationship

How parameter combinations work together

**Date and Time Controls:**

- **DATUM** and **SYDATE** define explicit dump-date scope when provided.
- **BACKDAYS** is fallback logic when explicit date ranges are not supplied.
- **SYTIME** refines time boundaries inside the selected date window.

**Duration Prioritization:**

- **DURATION** + **DURATION_UNIT** are an additional (second) filter after date selection.
- Simple flow: first retrieve by date window, then keep dumps matching duration threshold.
- Final result depends on both date conditions and duration conditions.

**Technical Ownership Segmentation:**

- **SYHOST**, **SYUSER**, **PROGRAMNAME**, **INCLUDENAME**, and **DUMPID** work together to isolate recurring technical root-cause patterns.


### Default Values
- **DURATION_UNIT** - H
- **BACKDAYS** - 1 (today and yesterday)

### Practical Example of Parameter Configuration
**Use Case 1: Recent critical dump triage**

```plaintext
BACKDAYS = 1
DURATION = 1
DURATION_UNIT = H
DUMPID = TSV_TNEW_PAGE_ALLOC_FAILED
```

**Purpose:** Focus on dumps from the last day that remain recent/high-priority in memory-allocation failure scenarios.

**Use Case 2: Host-specific stability investigation**

```plaintext
SYDATE = 20260330-20260331
SYHOST = APP01
PROGRAMNAME = Z_BATCH_IMPORT
DURATION = 30
DURATION_UNIT = M
```

**Purpose:** Identify dump concentration on one host and one program during the selected period.

**Use Case 3: User and include deep-dive**

```plaintext
SYUSER = BATCH_USER_02
INCLUDENAME = ZFI_POSTING_INCLUDE
DATUM = 20260331-20260331
SYTIME = 080000-180000
```

**Purpose:** Investigate user-owned daytime dump activity linked to a specific include source.


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_DUMPS | DUMPID | Name of Runtime Error | CHAR(30) | DUMPID |
| /SKN/S_SW_01_01_DUMPS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_DUMPS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_DUMPS | INCLUDENAME | ABAP Program Name | CHAR(40) | PROGNAME |
| /SKN/S_SW_01_01_DUMPS | LINENUMBER | Source code line of an ABAP/4 program | CHAR(5) | ABP_SLINE |
| /SKN/S_SW_01_01_DUMPS | PROGRAMNAME | ABAP Program Name | CHAR(40) | PROGNAME |
| /SKN/S_SW_01_01_DUMPS | SYDATE | System Date | DATS(8) | SYDATUM |
| /SKN/S_SW_01_01_DUMPS | SYHOST | Application Server | CHAR(32) | SYHOST |
| /SKN/S_SW_01_01_DUMPS | SYTIME | System Time | TIMS(6) | SYUZEIT |
| /SKN/S_SW_01_01_DUMPS | SYUSER | User Name | CHAR(12) | SYUNAME |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_DUMPS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_DUMPS OPTIONAL
*"----------------------------------------------------------------------
* Changed by Zoe on 29.12.10 to allow selection by msg parameters (msgv1 & msgv2)
* Zoe on 3.1.10 for Ortal - complete matnr & plant numbers for CIF messages
*"----------------------------------------------------------------------
  RANGES : R_SYDATE FOR RSDUMPINFO-SYDATE ,
           R_SYTIME FOR RSDUMPINFO-SYTIME,
           R_SYHOST FOR RSDUMPINFO-SYHOST,
           R_SYUSER FOR RSDUMPINFO-SYUSER,
           R_DUMPID FOR RSDUMPINFO-DUMPID,
           R_PROGRAMNAME FOR RSDUMPINFO-PROGRAMNAME,
           R_INCLUDENAME FOR RSDUMPINFO-INCLUDENAME,
           R_DATUM   FOR SY-DATUM .
  DATA : DATE_FROM LIKE SY-DATUM ,
         DATE_TO LIKE SY-DATUM ,
         BACKDAYS  TYPE I .
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : DDATE LIKE SY-DATUM.
  DATA : T_DUMP TYPE RSDUMPTAB,
         L_DUMP LIKE LINE OF T_DUMP.
  DATA :   SY_DATLO LIKE SY-DATLO ,
           SY_TIMLO LIKE SY-TIMLO .
  DATA : TIME_DIFF TYPE  INT4 .
  DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  DATA_MULTY:  DURATION       /SKN/E_SW_DURATION.   "From NOW to Job Start Time point -in duration units
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
*-- Fill Selection Option Tables
  LV_DURATION_UNIT = 'H'.
  SELECT_SINGLE: DURATION_UNIT.
   SELECT_MULTY: DURATION.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_DUMPS'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
  LOOP AT T_SELECT WHERE FIELDNM = 'SYDATE'.
    MOVE-CORRESPONDING T_SELECT TO R_SYDATE.
    APPEND R_SYDATE.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'SYTIME'.
    MOVE-CORRESPONDING T_SELECT TO R_SYTIME.
    APPEND R_SYTIME.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'SYHOST'.
    MOVE-CORRESPONDING T_SELECT TO R_SYHOST.
    APPEND R_SYHOST.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'SYUSER'.
    MOVE-CORRESPONDING T_SELECT TO R_SYUSER.
    APPEND R_SYUSER.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'DUMPID'.
    MOVE-CORRESPONDING T_SELECT TO R_DUMPID.
    APPEND R_DUMPID.
  ENDLOOP.
*
  LOOP AT T_SELECT WHERE FIELDNM = 'PROGRAMNAME'.
    MOVE-CORRESPONDING T_SELECT TO R_PROGRAMNAME.
    APPEND R_PROGRAMNAME.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'INCLUDENAME'.
    MOVE-CORRESPONDING T_SELECT TO R_INCLUDENAME.
    APPEND R_INCLUDENAME.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
    MOVE-CORRESPONDING T_SELECT TO R_DATUM.
    APPEND R_DATUM.
  ENDLOOP.
  IF R_DATUM[] IS INITIAL .
    R_DATUM[] = R_SYDATE[].
  ENDIF.
  IF R_DATUM[] IS INITIAL .
    LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
      R_DATUM-SIGN = 'I' .
      R_DATUM-OPTION = 'GE' .
      BACKDAYS = T_SELECT-LOW .
      DATE_FROM = SY-DATUM - BACKDAYS .
      DATE_TO = SY-DATUM.
      R_DATUM-LOW = DATE_FROM .
      APPEND R_DATUM.
      EXIT.
    ENDLOOP.
    IF R_DATUM[] IS INITIAL .
      R_DATUM-SIGN = 'I' .
      R_DATUM-OPTION = 'GE' .
      BACKDAYS = 1 .
      DATE_FROM = SY-DATUM - BACKDAYS .
      DATE_TO = SY-DATUM.
      R_DATUM-LOW = DATE_FROM .
      APPEND R_DATUM.
    ENDIF .
  ENDIF.
  "--------------
  READ TABLE R_DATUM INDEX 1.
  DATE_FROM = R_DATUM-LOW.
  IF R_DATUM-HIGH IS NOT INITIAL.
    DATE_TO = R_DATUM-HIGH.
  ENDIF.
  LOOP AT R_DATUM .
    IF DATE_FROM > R_DATUM-LOW.
      DATE_FROM = R_DATUM-LOW.
    ENDIF.
    IF DATE_TO < R_DATUM-HIGH.
      DATE_TO = R_DATUM-HIGH.
    ENDIF.
  ENDLOOP.
  IF DATE_TO IS INITIAL.
    DATE_TO = DATE_FROM.
  ENDIF.
  "--------------
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  DDATE = DATE_FROM.
  WHILE DDATE <= DATE_TO.
    REFRESH: T_DUMP.
    CALL FUNCTION 'RS_ST22_GET_DUMPS'
      EXPORTING
        P_DAY       = DDATE
      IMPORTING
        P_INFOTAB   = T_DUMP .
    LOOP AT T_DUMP INTO L_DUMP.
      MOVE-CORRESPONDING L_DUMP TO T_DATA.
      APPEND T_DATA.
    ENDLOOP.
     ADD 1 TO DDATE.
  ENDWHILE.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-SYDATE IN R_SYDATE.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-SYTIME IN R_SYTIME.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-SYHOST IN R_SYHOST.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-SYUSER IN R_SYUSER.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-DUMPID IN R_DUMPID.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-PROGRAMNAME IN R_PROGRAMNAME.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-INCLUDENAME IN R_INCLUDENAME.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-SYDATE
          T_FROM            = T_DATA-SYTIME
          D_TO              = SY_DATLO
          T_TO              = SY_TIMLO
          TIME_UNIT         = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
      IF SY-SUBRC = 0.
        T_DATA-DURATION = TIME_DIFF .
      ELSE.
        T_DATA-DURATION = '999999'.
      ENDIF.
   MODIFY T_DATA INDEX SY_TABIX .
 ENDLOOP .
 DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
