# Exception Indicator: Update Requests (SM13) - SW_01_01_SM13

## General Overview

This Exception Indicator monitors SAP update requests as shown in transaction SM13, focusing on update header records that match organizational and technical filters together with age and state-style triage.

This EI serves as an essential control for application operations and Basis support by:
- Surfacing update requests that remain in states or return-code bands that warrant investigation or repeat processing
- Highlighting concentration of work by scheduler, program, target system, or context when queues grow unevenly
- Supporting incident review when specific users, transactions, or application servers drive problematic update activity
- Giving teams a bounded extract when large update queues would otherwise hide material exceptions
- Helping internal audit demonstrate that sensitive update traffic stayed within agreed operational boundaries

Typical use includes daily operations checks, incident triage after failed processing, and capacity discussions before peak periods. Teams act on results in standard update administration, then adjust parameters or clear backlog as appropriate.

The routine reads update header data from the VBHDR family of data used for asynchronous update administration.


## Problem Description

Failure to monitor update request traffic and backlog patterns creates multiple risks across operational stability, data consistency, and compliance:

**Operational Stability Issues**
- Stuck or failing updates can accumulate without a prioritized queue, delaying posting of dependent business documents
- Peak load periods may overwhelm administrators when no exception view highlights the worst-aging or highest-risk lines
- Cross-client or cross-server noise can hide the few updates that actually block closing activities or batch chains
- Silent growth of problematic return codes erodes trust in processing until users report downstream symptoms

**Data Consistency and Processing Risks**
- Business tables may remain temporarily inconsistent when update execution lags behind user-facing confirmations
- Retries and partial processing become harder to diagnose when no consolidated monitor ties user, transaction, and scheduling context
- Long-running updates increase collision risk with later changes to the same objects if exceptions are not reviewed promptly

**Management Visibility and Decision-Making Risks**
- Executives and application owners lack a simple trend signal for update health by system, user, or functional area
- Strategic reviews of change activity miss objective evidence of where update exceptions concentrate after releases

## Suggested Resolution

**Immediate Response**
- Review each flagged line for user, transaction, program, server, and scheduling context to judge business impact and ownership
- Open SM13 or equivalent update administration for the same selection slice to validate current status and related diagnostics
- Confirm whether the update should still be active or represents a stale entry that requires repeat or cleanup processing
- Capture accountable roles and target dates when lines tie to financial posting, inventory, or customer-visible processes

**System Assessment**
- Segment results by client, user, transaction code, return code, and state-oriented attributes to see where volume concentrates
- Compare current counts to prior monitoring cycles after transports, batch campaigns, or infrastructure changes
- Examine how long each case has been outstanding relative to recent changes on the update master for prioritization
- Validate that server and language filters match the productive landscape so exceptions are not dominated by test traffic

**Corrective Actions**
- Resolve underlying application errors, repeat failed updates, or clear obsolete entries according to SAP update administration best practices
- Adjust monitoring parameters after root-cause review so recurring benign patterns are excluded without hiding genuine risk
- Coordinate with functional owners when mass master-data or custom-code defects drive concentrated update failures
- Document remediation for audit when updates touched regulated processes or period-critical postings
- Schedule recurring monitoring after template releases so new transaction or server patterns are validated early


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 3 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 4 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 5 | MAX_RECORDS | Maximum no. records |  | 0 | 0 |  |  |
| 6 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 7 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 8 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 9 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 10 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 11 | VBACCNT | Update account | CHAR | 12 | 0 | VBACCNT | VBACCNT |
| 12 | VBCLIINFO | Bytes | RAW | 1 | 0 | THRAW1 | THRAW1 |
| 13 | VBCLINAME | Update Server Name | CHAR | 64 | 0 | VBNAME | VBNAME |
| 14 | VBCONTEXT | Update context | CHAR | 20 | 0 | VBCONTEXT | VBCONTEXT |
| 15 | VBDATE | Update date and time | CHAR | 14 | 0 | VBDATE | CHAR14 |
| 16 | VBDATFM | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 17 | VBDCPFM | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 18 | VBENQKEY | Enqueue key | CHAR | 58 | 0 | ENQKEY | ENQKEY |
| 19 | VBETRANSID | Tran.ID | CHAR | 32 | 0 | PFTRANSID | CHAR32 |
| 20 | VBETRANSLN | Length of external transaction ID | INT4 | 10 | 0 | TXETRANSLN | TXETRANSLN |
| 21 | VBKEY | Update key | CHAR | 32 | 0 | VBKEY_D | VBKEY |
| 22 | VBLANG | Logon Language | LANG | 1 | 0 | XULANGU | LANG |
| 23 | VBMANDT | Client | CLNT | 3 | 0 | VBMANDT | MANDT |
| 24 | VBNAME | Update Server Name | CHAR | 64 | 0 | VBNAME | VBNAME |
| 25 | VBRC | Update return code | INT4 | 10 | 0 | VBRC | VBRC |
| 26 | VBREPORT | Generating program | CHAR | 40 | 0 | VBREPORT | WPREPORT |
| 27 | VBSTATE | Status | INT1 | 3 | 0 | VBSTATE | INT1 |
| 28 | VBTCODE | TCODE | CHAR | 20 | 0 | VBTCODE | VBTCODE |
| 29 | VBTIMOFF | Time Offset | INT4 | 10 | 0 | VBTIMOFF | VBTIMOFF |
| 30 | VBTRANSID | Tran.ID | CHAR | 32 | 0 | PFTRANSID | CHAR32 |
| 31 | VBUSR | User | CHAR | 12 | 0 | VBBNAME | UBNAME |
| 32 | VBZONLO | Local time zone | CHAR | 6 | 0 | VBZONLO | VBZONLO |
| 33 | VDATE | Current Date | DATS | 8 | 0 | SYDATUM | SYDATS |
| 34 | VTIME | Time | TIMS | 6 | 0 | SYUZEIT | SYTIME |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 34 parameters listed in the Parameters Reference Table when tuning this EI; each influences which SM13 update requests are retrieved, aged, and surfaced for alerting.

**BACKDAYS** (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

Backdays is based on VBDATE

**DURATION** (Duration In Time Units)

Uses duration in time units from the update header context so only records with DURATION inside declared values proceed.

**DURATION_UNIT** (Duration Unit(D/H/M))

Unit for elapsed time between each line’s scheduling timestamp and the evaluation run.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**MANAGE_IN_UTC** ('X' - Manage in UTC)

Controls whether the monitor normalizes evaluation timestamps to UTC or keeps local application-server time context.

**MANAGE_IN_UTC Options:**
- **X** — Manage timing comparisons in UTC for consistent cross-time-zone batch results.
- **Empty or blank** — Use local-time handling consistent with the application server clock context.

**MAX_RECORDS** (Maximum no. records)

When tightened, maximum no. records (MAX_RECORDS) removes updates that would otherwise dilute attention from failing or long-running work.

**STATE_COLOR** (State Color)

Filters update-request rows by the derived color bucket used for severity-style triage in the monitor framework.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional domain literals may appear per system configuration for neutral or inactive states.

**STATE_ICON** (State Icon)

Improves readability of exported lists because state icon (STATE_ICON) columns stay aligned with the configured filter intent.

**STATUS_DESC** (SW Message)

For distributed landscapes, sw message on STATUS_DESC often anchors which application server or time-zone dimension appears in results.

**SW_DEST** (Cloud Destination)

Captures edge cases where cloud destination (SW_DEST) must be non-default to reproduce a customer-specific monitoring scenario.

**USER_FLD** (Dynamic Recipient User Field)

Optional dynamic recipient or routing field used by the monitor framework when populated for distribution or extensions.

**USER_FLD Options:**
No fixed USER_FLD value list is defined in the available code for this EI.

**VBACCNT** (Update account)

When tightened, update account (VBACCNT) removes updates that would otherwise dilute attention from failing or long-running work.

**VBCLIINFO** (Bytes)

When tightened, bytes (VBCLIINFO) removes updates that would otherwise dilute attention from failing or long-running work.

**VBCLINAME** (Update Server Name)

Combines with other VBHDR-oriented filters so update server name on VBCLINAME refines which update requests remain for duration checks.

**VBCONTEXT** (Update context)

Connects to alert semantics: lines removed for failing update context on VBCONTEXT never reach duration or state-color filtering.

**VBDATE** (Update date and time)

Pairs with duration logic: once VBDATE passes list selection, elapsed time since scheduling still must fit configured duration windows.

**VBDATFM** (Single-Character Flag)

Separates cross-client noise from in-scope work when single-character flag on VBDATFM correlates with client or user attributes.

**VBDCPFM** (Single-Character Flag)

Pairs with duration logic: once VBDCPFM passes list selection, elapsed time since scheduling still must fit configured duration windows.

**VBENQKEY** (Enqueue key)

Interprets enqueue key as part of the SM13 selection contract: open ranges follow framework defaults; non-empty ranges apply strict matching on VBENQKEY.

**VBETRANSID** (Tran.ID)

Prevents accidental global scans when tran.id (VBETRANSID) is meant to stay within a controlled maintenance window.

**VBETRANSLN** (Length of external transaction ID)

Captures edge cases where length of external transaction id (VBETRANSLN) must be non-default to reproduce a customer-specific monitoring scenario.

**VBKEY** (Update key)

Supports operations review by evaluating update key through VBKEY for each update request candidate.

**VBLANG** (Logon Language)

For operations, logon language on VBLANG indicates whether a line belongs in the current monitoring pass versus historical noise.

**VBMANDT** (Client)

Ensures reporting on programs and transactions respects client constraints carried by VBMANDT.

**VBNAME** (Update Server Name)

Explains why two monitoring passes differ: only the pass with stricter update server name on VBNAME surfaces the disputed lines.

**VBRC** (Update return code)

Documents expected operator behavior—update return code on VBRC should be set when that dimension is part of the control objective.

**VBREPORT** (Generating program)

When populated, keeps the extract focused so generating program (VBREPORT) aligns with the intended SM13 triage slice.

**VBSTATE** (Status)

When tightened, status (VBSTATE) removes updates that would otherwise dilute attention from failing or long-running work.

**VBTCODE** (TCODE)

When populated, keeps the extract focused so tcode (VBTCODE) aligns with the intended SM13 triage slice.

**VBTIMOFF** (Time Offset)

Valuable when comparing quality before and after a release—hold time offset on VBTIMOFF constant while varying other filters.

**VBTRANSID** (Tran.ID)

Supports operations review by evaluating tran.id through VBTRANSID for each update request candidate.

**VBUSR** (User)

Gives auditors traceable criteria because user on VBUSR is applied consistently before any alert flag is raised.

**VBZONLO** (Local time zone)

Improves readability of exported lists because local time zone (VBZONLO) columns stay aligned with the configured filter intent.

**VDATE** (Current Date)

Stabilizes week-over-week metrics by fixing current date (VDATE) while allowing duration thresholds to move.

**VTIME** (Time)

When populated, keeps the extract focused so time (VTIME) aligns with the intended SM13 triage slice.


### Parameter Relationships

How parameter combinations work together

**BACKDAYS** defines the default calendar span applied to each line’s scheduling timestamp when the caller does not supply an explicit monitoring date interval through the standard selection mechanism. That default window still respects the evaluation clock context implied by **MANAGE_IN_UTC**, so the same numeric lookback can align differently across time zones.

**DURATION** and **DURATION_UNIT** operate after rows are retrieved. They measure elapsed time from the scheduling timestamp on each result line through the evaluation run, using the unit you configure, and only rows whose computed age fits the duration selection remain.

**MAX_RECORDS** caps how many VBHDR rows the routine reads before downstream attribute filters and duration logic run, so it should be sized together with tighter user, transaction, or server filters when queues are very large.

**STATE_COLOR** limits the final list once duration filtering has completed, keeping only lines whose color bucket matches the configured severity mix.

**MANAGE_IN_UTC** should be chosen consistently with how operations teams interpret scheduling timestamps for **BACKDAYS** and **DURATION**, because both the window boundary and the age calculation use the same clock interpretation.


### Default Values

- **BACKDAYS** - 1 from the preset before the selection read when the caller does not override it.
- **DURATION_UNIT** - M from the preset before the selection read when the caller does not override it.
- **DURATION** - initial — when the duration selection range is left empty, the routine does not filter rows out by computed age until a range is supplied.

### Practical Example of Parameter Configuration

**Use Case 1: Operations tower — last day of updates with age filter**

**Purpose:** Review recent update headers for one client with minute-based aging and a modest record cap.

```
VBMANDT = 100
BACKDAYS = 7
DURATION_UNIT = M
DURATION = 120
MAX_RECORDS = 5000
MANAGE_IN_UTC = 
```

**Use Case 2: Power user with red-state focus**

**Purpose:** Surface error-style color buckets for a specific batch user together with a restrictive transaction filter.

```
VBUSR = BATCHUSER01
STATE_COLOR = R
VBTCODE = MIGO
```

**Use Case 3: Server-scoped health check**

**Purpose:** Compare update load on a named application server with default lookback and UTC-normalized clocks.

```
VBNAME = PROD_APP01
MANAGE_IN_UTC = X
BACKDAYS = 3
VBLANG = E
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_SM13 | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_SM13 | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_SM13 | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_SM13 | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_SM13 | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_01_SM13 | VBACCNT | Update accounting | CHAR(12) | VBACCNT |
| /SKN/S_SW_01_01_SM13 | VBCLIINFO | Byte Value | RAW(1) | THRAW1 |
| /SKN/S_SW_01_01_SM13 | VBCLINAME | Name of the Executing Update Server | CHAR(64) | VBNAME |
| /SKN/S_SW_01_01_SM13 | VBCONTEXT | Update context (Code-page, DB node, ..) | CHAR(20) | VBCONTEXT |
| /SKN/S_SW_01_01_SM13 | VBDATE | Date and time of update record (sort) | CHAR(14) | VBDATE |
| /SKN/S_SW_01_01_SM13 | VBDATFM | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_01_01_SM13 | VBDCPFM | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_01_01_SM13 | VBENQKEY | Enqueue key | CHAR(58) | ENQKEY |
| /SKN/S_SW_01_01_SM13 | VBETRANSID | SAP Statistics: Transaction ID | CHAR(32) | PFTRANSID |
| /SKN/S_SW_01_01_SM13 | VBETRANSLN | Length of External Transaction ID | INT4(10) | TXETRANSLN |
| /SKN/S_SW_01_01_SM13 | VBKEY | Key for Update and Enqueue/Dequeue | CHAR(32) | VBKEY_D |
| /SKN/S_SW_01_01_SM13 | VBLANG | Logon Language | LANG(1) | XULANGU |
| /SKN/S_SW_01_01_SM13 | VBMANDT | Client for update request | CLNT(3) | VBMANDT |
| /SKN/S_SW_01_01_SM13 | VBNAME | Name of the Executing Update Server | CHAR(64) | VBNAME |
| /SKN/S_SW_01_01_SM13 | VBRC | Update return code | INT4(10) | VBRC |
| /SKN/S_SW_01_01_SM13 | VBREPORT | Program that generated the update request | CHAR(40) | VBREPORT |
| /SKN/S_SW_01_01_SM13 | VBSTATE | Global status of an update request | INT1(3) | VBSTATE |
| /SKN/S_SW_01_01_SM13 | VBTCODE | Transaction which has created the update request | CHAR(20) | VBTCODE |
| /SKN/S_SW_01_01_SM13 | VBTIMOFF | Offset Local to System Time | INT4(10) | VBTIMOFF |
| /SKN/S_SW_01_01_SM13 | VBTRANSID | SAP Statistics: Transaction ID | CHAR(32) | PFTRANSID |
| /SKN/S_SW_01_01_SM13 | VBUSR | User name in update request | CHAR(12) | VBBNAME |
| /SKN/S_SW_01_01_SM13 | VBZONLO | Name of Local Time Zone | CHAR(6) | VBZONLO |
| /SKN/S_SW_01_01_SM13 | VDATE | System Date | DATS(8) | SYDATUM |
| /SKN/S_SW_01_01_SM13 | VTIME | System Time | TIMS(6) | SYUZEIT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_SM13.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SM13 OPTIONAL
*"----------------------------------------------------------------------
DATA_MULTY: DURATION   /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
            STATE_COLOR /SKN/E_SW_STATE_COLOR,
            DATUM     SY-DATUM,
            VBMANDT   VBMANDT,
            VBUSR     VBBNAME,
            VBREPORT  VBREPORT,
            VBTCODE   VBTCODE,
            VBRC      VBRC,
            VBNAME    VBNAME,
            VBCONTEXT VBCONTEXT,
            VBSTATE   VBSTATE,
            VBCLIINFO THRAW1,
            VBLANG    XULANGU,
            VBCLINAME VBNAME,
            VBZONLO   VBZONLO.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             BACKDAYS INT4,
             LANGU LANGU,
             NUMBER SY-TABIX,
             SUBRC SY-SUBRC,
             MANAGE_IN_UTC CHAR1,
             MAX_RECORDS INT4.
DATA : SY_TABIX LIKE SY-TABIX .
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA: DATE_FROM LIKE SY-DATUM,
      DATE_TO LIKE SY-DATUM.
DATA : TIME_DIFF TYPE  INT4 .
*data : is_out(1) type C.
*data: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
DATA: VBHDR_NAME(20) VALUE 'VBHDR',
      MAX_RECORDS  TYPE I VALUE '99999',
      VBHDR_LINE TYPE VBHDR,
      VBDATE_FROM LIKE VBHDR-VBDATE,
      VBDATE_TO LIKE VBHDR-VBDATE.
*-- Fill Selection Option Tables
   SELECT_MULTY: DURATION,
                 STATE_COLOR,
                 DATUM,
                 VBMANDT,
                 VBUSR,
                 VBREPORT,
                 VBTCODE,
                 VBRC,
                 VBNAME,
                 VBCONTEXT,
                 VBSTATE,
                 VBCLIINFO,
                 VBLANG,
                 VBCLINAME,
                 VBZONLO.
   LV_DURATION_UNIT = 'M'.
   SELECT_SINGLE: DURATION_UNIT,
                  BACKDAYS,
                  MANAGE_IN_UTC,
                  LANGU,
                  MAX_RECORDS.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_SM13'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
 SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
 TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
  IF LV_MAX_RECORDS IS NOT INITIAL.
    MAX_RECORDS = LV_MAX_RECORDS.
  ENDIF.
* PROCESS_ICON = ICON_DELETE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
      CLEAR VBDATE_FROM.
      VBDATE_FROM+0(8) = SY_DATLO.
      VBDATE_TO   = '99991231235959'.
      IF LV_BACKDAYS IS INITIAL.
        LV_BACKDAYS = 1.
      ENDIF.
      IF R_DATUM[] IS NOT INITIAL .
        READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
        IF SY-SUBRC IS INITIAL.
          DATE_FROM = RS_DATUM-LOW.
          IF DATE_FROM IS INITIAL.
            DATE_FROM = SY_DATLO.
          ENDIF.
          DATE_TO = RS_DATUM-HIGH.
          IF DATE_TO IS INITIAL.
            DATE_TO = '99991231'.
          ENDIF.
        ENDIF.
      ELSE.
        DATE_FROM = SY_DATLO - LV_BACKDAYS .
        DATE_TO = '99991231'.
      ENDIF.
      VBDATE_FROM+0(8) = DATE_FROM.
      VBDATE_TO+0(8) = DATE_TO.
      SELECT * UP TO MAX_RECORDS ROWS FROM (VBHDR_NAME)
            INTO CORRESPONDING FIELDS OF VBHDR_LINE
            WHERE VBMANDT IN R_VBMANDT
                  AND VBUSR IN R_VBUSR
                  AND VBSTATE IN R_VBSTATE "     BETWEEN vbstate_from AND vbstate_to
                  AND VBRC  IN R_VBRC "  BETWEEN vbrc_from AND vbrc_to
                  AND VBDATE BETWEEN VBDATE_FROM AND VBDATE_TO
                  AND VBNAME  IN R_VBNAME "  LIKE vbserver
            ORDER BY VBDATE DESCENDING.
          MOVE-CORRESPONDING VBHDR_LINE TO T_DATA.
          T_DATA-VDATE = T_DATA-VBDATE+0(8).
          T_DATA-VTIME = T_DATA-VBDATE+8(6).
          APPEND T_DATA.
        ENDSELECT.
  DELETE T_DATA WHERE: VBREPORT NOT IN R_VBREPORT,
                       VBTCODE NOT IN R_VBTCODE,
                       VBCONTEXT NOT IN R_VBCONTEXT,
                       VBCLIINFO NOT IN R_VBCLIINFO,
                       VBLANG NOT IN R_VBLANG,
                       VBCLINAME NOT IN R_VBCLINAME,
                       VBZONLO NOT IN R_VBZONLO.
*   loop at t_data.
*     sy_tabix = sy-tabix.
*      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*        EXPORTING
*          D_FROM            = t_data-VDATE
*          T_FROM            = t_data-VTIME
*          D_TO              = sy_datlo
*          T_TO              = sy_timlo
*          TIME_UNIT         = 'M'
*        IMPORTING
*          TIME_DIFF         = TIME_DIFF
*        EXCEPTIONS
*          WRONG_VALUE       = 1
*          OTHERS            = 2    .
*      IF SY-SUBRC = 0.
*        if TIME_DIFF < '999999'.
*          t_data-DURATION_M = TIME_DIFF .
*        else.
*          t_data-DURATION_M = '999999'.
*        endif.
*        t_data-DURATION_H = t_data-DURATION_M / 60.
*        CALL FUNCTION '/SKN/F_SW_01_01_SM13_STATUS'
*          EXPORTING
*            status            = t_data-vbrc
*          IMPORTING
*            STATUS_DESC       = t_data-STATUS_DESC
*            STATE_COLOR       = t_data-STATE_COLOR.
*
*         CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
*          EXPORTING
*            state_color = t_data-state_color
*          IMPORTING
*            state_icon  = t_data-state_icon.
*
*        modify t_data index sy_tabix.
*      ENDIF.
*    endloop.
*
*  delete t_data where DURATION_M not in R_DURATION_M.
*  delete t_data where DURATION_H not in R_DURATION_H.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-VDATE
          T_FROM            = T_DATA-VTIME
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
  DELETE T_DATA WHERE STATE_COLOR NOT IN R_STATE_COLOR.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
