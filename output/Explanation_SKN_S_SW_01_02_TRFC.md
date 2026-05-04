# Exception Indicator: TRFC Monitoring - SW_01_02_TRFC

## General Overview

This Exception Indicator monitors transactional RFC (tRFC) queue entries and their processing state across destinations, function modules, and time windows. It gives operations teams visibility into stuck, failing, or long-running asynchronous RFC work so that interface health can be judged before end users see timeouts or missing follow-on processing.

This EI serves as an essential control for integration and basis operations by:

- Enabling detection of destinations or function modules where transactional RFC traffic accumulates in non-success states.
- Supporting prioritization when host, user, or transaction context shows concentration of errors versus normal backlog.
- Providing a basis for comparing current queue posture to prior periods during releases, migrations, or partner changes.
- Helping leadership and auditors demonstrate that asynchronous interface activity is reviewed with the same discipline as synchronous checks.
- Reducing mean time to understand by surfacing state and age signals together with technical identifiers operators already use in SM58-style workflows.

Typical use cases include daily interface health reviews, post-cutover stabilization windows, and escalation paths when batch-driven RFC traffic deviates from an agreed baseline.

The function works against standard transactional RFC state data and emphasizes destination, function module, timestamps, and duration-based aging consistent with how administrators triage SM58-related issues.


## Problem Description

Failure to monitor transactional RFC queue health creates multiple risks across integrated business processes, operational recovery time, and confidence in automated posting chains.

**Integration and Business Process Risks**

- Stuck or failing asynchronous calls can delay downstream financial postings, inventory updates, or confirmations without an immediate dialog error for end users.
- Recurring concentration on specific destinations may hide capacity or authorization problems until a major closing window.
- Retries and confirmations that never complete can leave business documents in inconsistent intermediate states.

**Operational and System Stability Risks**

- Technical teams may only react after batch aborts or user complaints, missing earlier warning signs visible in queue state distribution.
- Host or process-level clustering of errors can be overlooked when traces are reviewed one object at a time.
- Long-running entries consume resources and can cascade into broader gateway or application server stress.

**Management Visibility and Decision-Making Risks**

- Leadership lacks a compact narrative on whether interface automation is under control during high-change periods.
- Service levels between business and IT become harder to defend without repeatable monitoring of asynchronous traffic.
- Post-incident reviews struggle to reconstruct whether monitoring scope matched the real landscape of destinations and function modules.

## Suggested Resolution

**Immediate Response**

- When attention is raised, review the transactional RFC monitoring context in the standard administration path your organization uses for SM58-style analysis, scoped to the same destinations and time slice implied by the observation.
- Identify whether affected entries cluster on one destination, one function module, or one application user context.
- Determine whether failures coincide with a known transport, certificate rotation, or partner maintenance window.
- Capture representative examples for the interface owner if functional correction is required.

**System Assessment**

- Compare current distribution of states and colors to the prior week for the same technical slice.
- Validate whether duration-based aging aligns with internal expectations for how long work may legitimately wait.
- Check for correlation with batch schedules that enqueue large volumes of transactional RFC calls.
- Review whether dynamic recipient configuration is influencing which rows appear in the working set.

**Corrective Actions**

- Correct destination definitions, authorizations, or partner parameters when misconfiguration is confirmed.
- Coordinate with basis on gateway or RFC destination capacity when technical limits are suspected.
- Adjust monitoring scope or thresholds after baseline behavior is documented, and record the decision for audit trail.
- Fold recurring enablement guidance into existing operational runbooks so first-line staff recognize the pattern without adding a separate training subsection.


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ARFCDATUM | Date | DATS | 8 | 0 | SYDATUM | SYDATS |
| 2 | ARFCDEST | Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 3 | ARFCFNAM | RFC FM | CHAR | 30 | 0 | RS38L_FNAM | FUNCNAME |
| 4 | ARFCIPID | Host ID | CHAR | 8 | 0 | ARFCIPID | ARFCIPID |
| 5 | ARFCLUWCNT | Counter | CHAR | 8 | 0 | ARFCLUWCNT | ARFCLUWCNT |
| 6 | ARFCMSG | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 7 | ARFCPID | Process ID | CHAR | 4 | 0 | ARFCPID | ARFCPID |
| 8 | ARFCRESERV | Horizontal Line | CHAR | 255 | 0 | SYULINE | SYCHAR255 |
| 9 | ARFCRETRYS | No. of attempts | NUMC | 4 | 0 | RETRY | NUM04 |
| 10 | ARFCRETURN | Reply | CHAR | 1 | 0 | ARFCRETURN | SYST_FLAG |
| 11 | ARFCRHOST | char8 | CHAR | 8 | 0 | RFCCHAR8 | RFCCHAR8 |
| 12 | ARFCSTATE | Status | CHAR | 8 | 0 | ARFCSTATE | ARFCSTATE |
| 13 | ARFCTCODE | Transaction code | CHAR | 20 | 0 | SYTCODE | TCODE |
| 14 | ARFCTIDCNT | Counter | CHAR | 4 | 0 | ARFCTIDCNT | ARFCTIDCNT |
| 15 | ARFCTIME | Time Stamp | CHAR | 8 | 0 | ARFCTIME | ARFCTIME |
| 16 | ARFCUSER | User | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |
| 17 | ARFCUZEIT | Time | TIMS | 6 | 0 | SYUZEIT | SYTIME |
| 18 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 19 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 20 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 21 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 22 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 23 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 23 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ARFCDATUM** (Date)

Stabilizes week-over-week metrics by fixing date (ARFCDATUM) while allowing duration thresholds to move.

**ARFCDEST** (Destination)

Connects to alert semantics: rows removed for failing destination on ARFCDEST never reach downstream filtering.

**ARFCFNAM** (RFC FM)

Reflects real administration where rfc fm on ARFCFNAM is routinely restricted to a single productive client or object family.

**ARFCIPID** (Host ID)

Documents expected operator behavior—host id on ARFCIPID should be set when that dimension is part of the control objective.

**ARFCLUWCNT** (Counter)

For operations, counter on ARFCLUWCNT indicates whether a row belongs in the current monitoring pass versus historical noise.

**ARFCMSG** (Message Variable)

Reduces false positives during peak windows by tightening message variable through ARFCMSG alongside state filters.

**ARFCPID** (Process ID)

Interprets process id as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on ARFCPID.

**ARFCRESERV** (Horizontal Line)

When combined with destination discipline, horizontal line on ARFCRESERV keeps both breadth and depth of the extract intentional.

**ARFCRETRYS** (No. of attempts)

Stabilizes week-over-week metrics by fixing no. of attempts (ARFCRETRYS) while allowing duration thresholds to move.

**ARFCRETURN** (Reply)

Prevents accidental global scans when reply (ARFCRETURN) is meant to stay within a controlled application slice.

**ARFCRHOST** (char8)

Supports escalation where char8 on ARFCRHOST signals ownership for follow-up between Basis and functional teams.

**ARFCSTATE** (Status)

When left open per framework rules, ARFCSTATE does not restrict status; when set, only matching rows remain.

**ARFCTCODE** (Transaction code)

Narrows retrieved rows where transaction code (ARFCTCODE) must match the configured selection for this monitor.

**ARFCTIDCNT** (Counter)

Helps distinguish technical versus business attributes when counter on ARFCTIDCNT correlates with counters or status fields.

**ARFCTIME** (Time Stamp)

Gives auditors traceable criteria because time stamp on ARFCTIME is applied consistently before any alert flag is raised.

**ARFCUSER** (User)

Helps distinguish technical versus business attributes when user on ARFCUSER correlates with counters or status fields.

**ARFCUZEIT** (Time)

For operations, time on ARFCUZEIT indicates whether a row belongs in the current monitoring pass versus historical noise.

**DURATION** (Duration In Time Units)

Reflects real administration where duration in time units on DURATION is routinely restricted to a single productive client or object family.

**DURATION_UNIT** (Duration Unit(D/H/M))

Unit for elapsed time between each session's creation date and time and the evaluation clock.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**STATE_COLOR** (State Color)

Filters lines by the derived color bucket used for severity-style triage in the monitor framework.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional literals may exist where the framework extends the palette for neutral states.

**STATE_ICON** (State Icon)

Reflects real administration where state icon on STATE_ICON is routinely restricted to a single productive client or object family.

**SW_DEST** (Cloud Destination)

Pairs with duration logic: once SW_DEST passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**USER_FLD** (Dynamic Recipient User Field)

Optional dynamic recipient or routing field used by the monitor framework when populated for distribution or extensions.

**USER_FLD Options:**
No fixed USER_FLD value list is defined in the available code for this EI.


### Parameter Relationships

How parameter combinations work together

Destination, function module, user, host, and transaction-oriented selectors define which transactional RFC rows enter the extract before state, color, and icon filters narrow the list. Date and time ranges on the monitoring selection anchor the historical slice, while duration and unit values describe how long entries have aged relative to the evaluation clock once rows are read. The optional cloud destination and dynamic recipient field align the run with landscape-specific routing when your configuration uses those dimensions.

After the initial read, duration-based deletion rules interact with the computed age on each row, so duration parameters should be interpreted together with the unit that the code applies for elapsed-time calculation. State color and icon selections should stay consistent with the underlying status values you intend to highlight so operators do not receive contradictory signals between dimensions.


### Default Values

- **DURATION** - initial - treated as unset by code (the duration interval filter does not remove rows until a populated duration range is supplied on selection).
- **DURATION_UNIT** - initial - treated as M by code (minutes preset on the duration unit variable before selection values are read).

**Note:** When no explicit monitoring date range is supplied on selection, the routine derives a lower bound from the evaluation date and the configured lookback from the selection table before applying date filters on the transactional RFC date field.

### Practical Example of Parameter Configuration

**Use Case 1: Destination under stress**

**Purpose:** Watch one RFC destination with a short age window and visible error coloring.

```
ARFCDEST = PROD_BW
DURATION = 60
DURATION_UNIT = M
STATE_COLOR = R
ARFCSTATE = SYSFAIL
SW_DEST = CLOUD_CONN
```

**Use Case 2: Function module focus**

**Purpose:** Track a specific remote-enabled module across users with a broader date span.

```
ARFCFNAM = ZMM_IF_SEND
ARFCDATUM = 20260101-20260131
ARFCUSER = BATCHRFC
ARFCTCODE = SM58
STATE_ICON = ICON_MESSAGE_ERROR
USER_FLD = EMAIL_ADDR
```

**Use Case 3: Wide discovery with technical slice**

**Purpose:** Sample many dimensions while still bounding time and duration for a nightly review.

```
ARFCDEST = %PRD%
ARFCIPID = APPHOST1
ARFCRETRYS = 0003-0009
DURATION = 240
DURATION_UNIT = M
STATE_COLOR = Y
ARFCMSG = TIMEOUT
ARFCLUWCNT = 00000001
ARFCPID = 1234
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_02_TRFC | ARFCDATUM | System Date | DATS(8) | SYDATUM |
| /SKN/S_SW_01_02_TRFC | ARFCDEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_02_TRFC | ARFCFNAM | Name of Function Module | CHAR(30) | RS38L_FNAM |
| /SKN/S_SW_01_02_TRFC | ARFCIPID | Host ID (IP ID | CHAR(8) | ARFCIPID |
| /SKN/S_SW_01_02_TRFC | ARFCLUWCNT | Counter within a transaction (LUW) | CHAR(8) | ARFCLUWCNT |
| /SKN/S_SW_01_02_TRFC | ARFCMSG | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_02_TRFC | ARFCPID | Process ID | CHAR(4) | ARFCPID |
| /SKN/S_SW_01_02_TRFC | ARFCRESERV | Horizontal Line | CHAR(255) | SYULINE |
| /SKN/S_SW_01_02_TRFC | ARFCRETRYS | No. of attempts | NUMC(4) | RETRY |
| /SKN/S_SW_01_02_TRFC | ARFCRETURN | Reply expected | CHAR(1) | ARFCRETURN |
| /SKN/S_SW_01_02_TRFC | ARFCRHOST | RFCCHAR8 | CHAR(8) | RFCCHAR8 |
| /SKN/S_SW_01_02_TRFC | ARFCSTATE | Status of an ARFC call (RECORDED,CPICERR,MAILED,READ..) | CHAR(8) | ARFCSTATE |
| /SKN/S_SW_01_02_TRFC | ARFCTCODE | Transaction Code | CHAR(20) | SYTCODE |
| /SKN/S_SW_01_02_TRFC | ARFCTIDCNT | Transaction ID (LUW -> COMMIT WORK) | CHAR(4) | ARFCTIDCNT |
| /SKN/S_SW_01_02_TRFC | ARFCTIME | Time Stamp | CHAR(8) | ARFCTIME |
| /SKN/S_SW_01_02_TRFC | ARFCUSER | User Name | CHAR(12) | SYUNAME |
| /SKN/S_SW_01_02_TRFC | ARFCUZEIT | System Time | TIMS(6) | SYUZEIT |
| /SKN/S_SW_01_02_TRFC | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_02_TRFC | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_02_TRFC | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_02_TRFC | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_02_TRFC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_TRFC OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_ARFCSTATE FOR ARFCSSTATE-ARFCSTATE,
         R_ARFCUSER  FOR ARFCSSTATE-ARFCUSER,
         R_ARFCDEST  FOR ARFCSSTATE-ARFCDEST,
         R_ARFCFNAM  FOR ARFCSSTATE-ARFCFNAM,
         R_ARFCDATUM FOR ARFCSSTATE-ARFCDATUM,
         R_ARFCUZEIT FOR ARFCSSTATE-ARFCUZEIT.
RANGES : R_STATE_COLOR FOR /SKN/S_SW_SYS_RFC_PING-STATE_COLOR,
         R_DATUM   FOR SY-DATUM .
DATA : DATE_FROM LIKE SY-DATUM ,
       BACKDAYS  TYPE I .
DATA : LANGU LIKE SY-LANGU .
DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
*
* define all known statuses
*
DATA :
       RECORDED LIKE ARFCSSTATE-ARFCSTATE VALUE 'RECORDED',
       CPICERR  LIKE ARFCSSTATE-ARFCSTATE VALUE 'CPICERR',
       SYSFAIL  LIKE ARFCSSTATE-ARFCSTATE VALUE 'SYSFAIL',
       CONFAIL  LIKE ARFCSSTATE-ARFCSTATE VALUE 'NO_CONF',
       EXECUTED LIKE ARFCSSTATE-ARFCSTATE VALUE 'EXECUTED',
       MAILED   LIKE ARFCSSTATE-ARFCSTATE VALUE 'MAILED',
       READ     LIKE ARFCSSTATE-ARFCSTATE VALUE 'READ',
       VBRECORD LIKE ARFCSSTATE-ARFCSTATE VALUE 'VBRECORD',
       SENDED   LIKE ARFCSSTATE-ARFCSTATE VALUE 'SENDED',
       DEBUG    LIKE ARFCSSTATE-ARFCSTATE VALUE 'DEBUG',
       VXRECORD LIKE ARFCSSTATE-ARFCSTATE VALUE 'VXRECORD',
       CONFAIL1 LIKE ARFCSSTATE-ARFCSTATE VALUE 'CONFAIL'.
DATA: BEGIN OF %_RESERV,               "Rel. Adr.
        CPROG        LIKE SY-CPROG,                          "40       0
        MANDT        LIKE SY-MANDT,                          "3       40
        BATCHPLA     TYPE C,                                 "1       43
        QRFCFLAG     TYPE C,                                 "1       44
        QNAME        LIKE TRFCQIN-QNAME,                     "24      45
        QCOUNT       LIKE TRFCQIN-QCOUNT,                    "24      69
        QRCVTID      LIKE ARFCTID,                           "24      93
        ARFCMSG2(23) TYPE C,                                 "23     117
        CHNGTRFC     TYPE C,                                 "1      140
        ORGHOST(20)  TYPE C,                                 "20     141
        ASGROUP(20)  TYPE C,                                 "20     161
        QTRACE       TYPE C,                                 "1      181
        QACTION      TYPE C,                                 "1      182
        QDATE        LIKE SY-DATUM,                          "8      183
        QTIME        LIKE SY-UZEIT,                          "6      191
        QLOG         TYPE C,                                 "1      197
        PACTION      TYPE C,                                 "1      198
        QNOEXEC      TYPE C,                                 "1      199
        USEQLIST     TYPE C,                                 "1      200
        QRCVNEW      TYPE C,                                 "1      201
        QNOSEND      TYPE C,                                 "1      202
        NRDATA(8)    TYPE N,                                 "8      203
        VBERRKEY(32) TYPE C,                                 "32     211
        LANGU        LIKE SY-LANGU.                          "1      243
                                                             "       244
                                     " hier kצnnnen bis max. 255 insges.
DATA: END OF %_RESERV.               " weitere Eintrהge abgesp. werden
DATA: STATUS_DESC TYPE  EDI_TEXT60.
DATA : WA TYPE ARFCSSTATE.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : TIME_DIFF TYPE  INT4 .
DATA : IS_OUT(1) TYPE C.
*-- Fill Selection Option Tables
  SELECT_MULTY: DURATION.
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_02_TRFC'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCDEST'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCDEST.
     APPEND R_ARFCDEST.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCSTATE'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCSTATE.
     APPEND R_ARFCSTATE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCUSER'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCUSER.
     APPEND R_ARFCUSER.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCFNAM'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCFNAM.
     APPEND R_ARFCFNAM.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCDATUM'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCDATUM.
     APPEND R_ARFCDATUM.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCUZEIT'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCUZEIT.
     APPEND R_ARFCUZEIT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
     MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
     APPEND R_STATE_COLOR.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
     MOVE-CORRESPONDING T_SELECT TO R_DATUM.
     APPEND R_DATUM.
   ENDLOOP.
   IF R_DATUM[] IS INITIAL .
     LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
       R_DATUM-SIGN = 'I' .
        R_DATUM-OPTION = 'GE' .
         BACKDAYS = T_SELECT-LOW .
         DATE_FROM = SY-DATUM - BACKDAYS .
         R_DATUM-LOW = DATE_FROM .
         APPEND R_DATUM.
         EXIT.
     ENDLOOP.
     IF R_DATUM[] IS INITIAL .
       R_DATUM-SIGN = 'I' .
        R_DATUM-OPTION = 'GE' .
         BACKDAYS = 1 .
         DATE_FROM = SY-DATUM - BACKDAYS .
         R_DATUM-LOW = DATE_FROM .
       APPEND R_DATUM.
     ENDIF .
   ENDIF.
   IF R_ARFCDATUM[] IS INITIAL.
     R_ARFCDATUM[] = R_DATUM[].
   ENDIF.
  "-----
  LANGU = SY-LANGU.
  LOOP AT T_SELECT WHERE FIELDNM = 'LANGU'.
    LANGU = T_SELECT-LOW.
    EXIT.
  ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
SELECT * FROM ARFCSSTATE
   INTO WA
   WHERE ARFCSTATE  IN R_ARFCSTATE
     "and  ARFCSTATE IN (CONFAIL1, SYSFAIL, CPICERR , RECORDED)
     AND ARFCUSER   IN R_ARFCUSER
     AND ARFCDEST   IN R_ARFCDEST
     AND ARFCFNAM   IN R_ARFCFNAM
     AND ARFCDATUM  IN R_ARFCDATUM
     AND ARFCUZEIT  IN R_ARFCUZEIT
     AND ARFCRETURN  = SPACE.
  %_RESERV = WA-ARFCRESERV.
  IF %_RESERV-QRFCFLAG <> SPACE.
    "qRFC-LUWs exclude
  ELSE.
    MOVE-CORRESPONDING WA TO T_DATA.
    APPEND T_DATA.
  ENDIF.
ENDSELECT.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
     CALL FUNCTION '/SKN/F_SW_01_02_TRFC_STATUS'
       EXPORTING
         STATUS            = T_DATA-ARFCSTATE
         LANGU             = LANGU
       IMPORTING
         STATUS_DESC       = STATUS_DESC
         STATE_COLOR       = T_DATA-STATE_COLOR.
     CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
       EXPORTING
         STATE_COLOR       = T_DATA-STATE_COLOR
       IMPORTING
         STATE_ICON        = T_DATA-STATE_ICON         .
     IF T_DATA-ARFCMSG IS INITIAL.
       T_DATA-ARFCMSG = STATUS_DESC.
     ENDIF.
     MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*  loop at t_data .
*    sy_tabix = sy-tabix .
*    clear is_out.
*    if not t_data-ARFCDATUM is initial.
*      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*        EXPORTING
*          D_FROM            = t_data-ARFCDATUM
*          T_FROM            = t_data-ARFCUZEIT
*          D_TO              = sy-datum
*          T_TO              = sy-uzeit
*          TIME_UNIT         = 'M'
*        IMPORTING
*          TIME_DIFF         = TIME_DIFF
*        EXCEPTIONS
*          WRONG_VALUE       = 1
*          OTHERS            = 2    .
*      IF SY-SUBRC = 0.
*        t_data-DURATION_M = TIME_DIFF .
*        t_data-DURATION_H = t_data-DURATION_M / 60.
*        if not t_data-DURATION_H in R_DURATION_H .
*          is_out = 'X'.
*        endif.
*        if not t_data-DURATION_M in R_DURATION_M .
*          is_out = 'X'.
*        endif.
*       if not is_out is initial.
*         delete t_data index sy_tabix .
*       else.
*         modify t_data index sy_tabix.
*       endif.
*      ENDIF.
*    endif.
*  endloop.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-ARFCDATUM
          T_FROM            = T_DATA-ARFCUZEIT
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
