# Exception Indicator: Application Locks (SM12) - SW_01_01_SM12

## General Overview

This Exception Indicator monitors SAP application locks as administrators review them in transaction SM12, focusing on who holds which generic lock objects, through which transactions, and for how long relative to the monitoring run.

This EI serves as an essential control for Basis and application operations by:
- Surfacing long-held or unusual locks that can block posting, updates, or critical batch jobs
- Highlighting concentration of lock activity by client, user, transaction, or application server for faster triage
- Supporting incident response when upgrades, transports, or custom code changes coincide with elevated lock volume
- Giving teams a bounded extract when global lock lists would hide the few entries that actually threaten throughput
- Helping audits demonstrate that lock hygiene was checked on a recurring basis for sensitive processes

Typical use includes daily operations checks, troubleshooting deadlocks and long transactions, and validating cleanup after maintenance windows. Teams pair results with SM12 and related tools when they need to contact lock owners or terminate obsolete sessions.

The routine reads the current lock table through the standard enqueue read interface, then enriches rows with duration in the configured time unit.


## Problem Description

Failure to monitor application lock patterns creates multiple risks across throughput, data consistency, and operational control:

**Throughput and User Experience Risks**
- Long-lived locks can serialize work queues, delay interactive users, and extend batch runtimes without a clear owner list
- Spikes in lock volume may go unnoticed until transactions time out or background jobs fail with enqueue errors
- Cross-client or cross-user noise in raw lock lists hides the few locks that actually threaten closing activities

**Data Consistency and Integrity Risks**
- Stale locks after cancelled dialogs or crashed sessions prolong false contention and encourage risky manual termination
- Misaligned filters on transaction or generic object attributes can leave blind spots where duplicate business updates are still possible

**Governance and Accountability Risks**
- Management lacks objective evidence of which programs or users drove lock pressure during sensitive periods
- Post-incident reviews miss structured history when no monitor captured who held which generic lock and for how long

## Suggested Resolution

**Immediate Response**
- Review each flagged lock for user, transaction, generic object, and host context to judge business severity and next owner
- Open SM12 with the same selection to validate whether the lock is still active, whether the session is healthy, and whether termination is appropriate
- Confirm whether the lock ties to a known maintenance window, a stuck batch job, or an interactive session that needs coaching rather than forceful cleanup

**System Assessment**
- Segment results by client, user, transaction code, lock mode, and host to see where volume or duration concentrates
- Compare current counts and age profiles to prior monitoring cycles after releases, data migrations, or infrastructure changes
- When RFC destination filters are used, validate that the monitored application server list still matches the productive landscape

**Corrective Actions**
- End obsolete sessions, guide users to complete or cancel work, or adjust programs that hold locks longer than business rules allow
- Tune monitoring parameters after root-cause review so benign patterns are excluded without hiding genuine risk
- Coordinate with development when specific transactions or custom code paths generate recurring exclusive locks
- Document remediation for audit when locks affected financially material posting or regulated processes
- Schedule recurring monitoring after major changes so new transaction or server patterns are validated early


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | DEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 2 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 3 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 4 | GARG | Lock Argument | CHAR | 150 | 0 | EQEGRAARG | EQDARG |
| 5 | GBCKTYPE | Backup flag | CHAR | 1 | 0 | EQEBCKTYPE | CHAR1 |
| 6 | GCLIENT | Client | CHAR | 3 | 0 | EQECLIENT | CHAR3 |
| 7 | GMODE | Lock mode | CHAR | 1 | 0 | EQEGRAMODE | CHAR1 |
| 8 | GNAME | Table name | CHAR | 30 | 0 | EQEGRANAME | CHAR30 |
| 9 | GOBJ | Lock Object Name | CHAR | 16 | 0 | EQEOBJ | CHAR16 |
| 10 | GTARG | Lock argument | CHAR | 50 | 0 | EQEGTARG | TEXT50 |
| 11 | GTCODE | Transaction Code | CHAR | 20 | 0 | EQETCODE | TCODE |
| 12 | GTDATE | Time Stamp | DATS | 8 | 0 | EQEDATE | DATUM |
| 13 | GTHOST | Host name | CHAR | 32 | 0 | EQEHOST | TEXT32 |
| 14 | GTMARK | Selected | CHAR | 1 | 0 | EQEMARK | CHAR1 |
| 15 | GTSYSNR | SAP System Number | NUMC | 2 | 0 | EQESYSNR | NUM02 |
| 16 | GTTIME | Time | TIMS | 6 | 0 | EQETIME | UZEIT |
| 17 | GTUSEC | Microseconds | NUMC | 6 | 0 | EQEUSEC | NUM06 |
| 18 | GTWP | Work Process Number | NUMC | 2 | 0 | EQEWP | NUM02 |
| 19 | GUNAME | User name | CHAR | 12 | 0 | EQEUNAME | CHAR12 |
| 20 | GUSE | Cumulative Counter 1 | INT4 | 10 | 0 | EQEUSE |  |
| 21 | GUSETXT | Cumulative Counter | NUMC | 10 | 0 | EQEUSETXT | NUM10 |
| 22 | GUSEVB | Cumulative Counter 2 | INT4 | 10 | 0 | EQEUSEVB |  |
| 23 | GUSEVBT | Cumulative Counter 2 | NUMC | 10 | 0 | EQEUSEVBT | NUM10 |
| 24 | GUSR | Lock Owner | CHAR | 58 | 0 | EQEUSR | EQDUSR |
| 25 | GUSRVB | Lock Owner 2 | CHAR | 58 | 0 | EQEUSRVB | EQDUSR |
| 26 | LANGU | Description Lanfuage |  | 0 | 0 |  |  |
| 27 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 28 | PROCESS_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 29 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 29 parameters listed in the Parameters Reference Table when tuning this EI; each influences which application locks are retrieved, aged, and surfaced for alerting.

**DEST** (RFC Destination)

Combines with user, transaction, and generic object filters so rfc destination on DEST refines which locks reach duration checks.

**DURATION** (Duration In Time Units)

Uses duration in time units from the lock context so only records with DURATION inside declared values proceed.

**DURATION_UNIT** (Duration Unit(D/H/M))

Unit for elapsed time between each lock’s enqueue date and time and the evaluation clock.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**GARG** (Lock Argument)

Aligns exception volume with the chosen scope by testing lock argument via GARG before alert evaluation.

**GBCKTYPE** (Backup flag)

Ensures reporting on transactions and lock modes respects backup flag constraints carried by GBCKTYPE.

**GCLIENT** (Client)

Supports Basis and development control by evaluating client through GCLIENT for each lock candidate.

**GMODE** (Lock mode)

Helps monitoring stay readable by requiring lock mode (GMODE) to match organizational or technical selectors when set.

**GNAME** (Table name)

Allows phased rollout: first widen GNAME for table name, then tighten duration thresholds once baseline noise is understood.

**GOBJ** (Lock Object Name)

Improves readability of exported lists because lock object name (GOBJ) columns stay aligned with the configured filter intent.

**GTARG** (Lock argument)

Separates cross-client noise from in-scope work when lock argument on GTARG correlates with client or user attributes.

**GTCODE** (Transaction Code)

Treats transaction code as a discriminator between similar lock rows that would otherwise look identical in a raw extract.

**GTDATE** (Time Stamp)

Supports escalation where time stamp on GTDATE signals ownership for follow-up between Basis and application teams.

**GTHOST** (Host name)

Combines with user, transaction, and generic object filters so host name on GTHOST refines which locks reach duration checks.

**GTMARK** (Selected)

Supports escalation where selected on GTMARK signals ownership for follow-up between Basis and application teams.

**GTSYSNR** (SAP System Number)

Allows phased rollout: first widen GTSYSNR for sap system number, then tighten duration thresholds once baseline noise is understood.

**GTTIME** (Time)

Separates cross-client noise from in-scope work when time on GTTIME correlates with client or user attributes.

**GTUSEC** (Microseconds)

Narrows enqueue lock rows where microseconds (GTUSEC) must match the configured selection for this monitor.

**GTWP** (Work Process Number)

Valuable when comparing lock health before and after a release—hold work process number on GTWP constant while varying other filters.

**GUNAME** (User name)

Separates cross-client noise from in-scope work when user name on GUNAME correlates with client or user attributes.

**GUSE** (Cumulative Counter 1)

Mirrors how administrators slice SM12: cumulative counter 1 (GUSE) is one lever that shapes which locks are comparable run over run.

**GUSETXT** (Cumulative Counter)

Guards against oversized extracts when cumulative counter on GUSETXT is narrowed together with client, user, or host filters.

**GUSEVB** (Cumulative Counter 2)

Stabilizes week-over-week metrics by fixing cumulative counter 2 (GUSEVB) while allowing duration thresholds to move.

**GUSEVBT** (Cumulative Counter 2)

Supports escalation where cumulative counter 2 on GUSEVBT signals ownership for follow-up between Basis and application teams.

**GUSR** (Lock Owner)

After ENQUEUE_READ results are mapped, rows are removed unless lock owner on GUSR still satisfies the active filter criteria.

**GUSRVB** (Lock Owner 2)

When populated, keeps the extract focused so lock owner 2 (GUSRVB) aligns with the intended SM12-style triage slice.

**LANGU** (Description Lanfuage)

Connects to alert semantics: lines removed for failing description lanfuage on LANGU never reach duration filtering.

**MANAGE_IN_UTC** ('X' - Manage in UTC)

Controls whether the monitor normalizes evaluation timestamps to UTC or keeps local application-server time context.

**MANAGE_IN_UTC Options:**
- **X** — Manage timing comparisons in UTC for consistent cross-time-zone batch results.
- **Empty or blank** — Use local-time handling consistent with the application server clock context.

**PROCESS_ICON** (State Icon)

Reflects real SM12 usage where state icon on PROCESS_ICON is routinely restricted to a single productive client or user.

**USER_FLD** (Dynamic Recipient User Field)

Optional dynamic recipient or routing field used by the monitor framework when populated for distribution or extensions.

**USER_FLD Options:**
No fixed USER_FLD value list is defined in the available code for this EI.


### Parameter Relationships

How parameter combinations work together

**MANAGE_IN_UTC** should be chosen consistently with how operations teams interpret lock timestamps, because both the duration calculation and any cross-system comparisons assume the same clock normalization.

**DURATION** and **DURATION_UNIT** work together as an age filter measured from each row’s enqueue date and time to the evaluation moment; only locks whose elapsed time fits the configured duration selection remain after the loop.

**DEST** (when restricted) steers the enqueue read to the intended RFC destination so client, user, and transaction filters are evaluated in the correct server context before duration logic runs.

**GCLIENT**, **GUNAME**, **GTCODE**, **GMODE**, **GTARG**, **GNAME**, **GARG**, **GUSR**, and **GTHOST** narrow the same logical population the security and Basis teams would slice manually in SM12; combining several of them is how you keep the extract relevant when global lock volume is high.

**USER_FLD** remains available for framework-style routing or extensions when your monitoring template expects a free-form recipient or tag alongside the technical lock attributes.


### Default Values

- **DURATION_UNIT** - M from the preset before the selection read when the caller does not override it.
- **DURATION** - initial — when the duration selection range is left empty, the routine does not filter rows out by computed age until a range is supplied.

### Practical Example of Parameter Configuration

**Use Case 1: Client-scoped exclusive locks**

**Purpose:** Review exclusive locks in one productive client with minute-based aging and local server time.

```
GCLIENT = 100
GMODE = E
DURATION_UNIT = M
DURATION = 120
MANAGE_IN_UTC = 
```

**Use Case 2: Program and user slice**

**Purpose:** Highlight locks owned by a batch user for a specific main program name.

```
GUNAME = BATCHUSER01
GTCODE = VA01
```

**Use Case 3: UTC-normalized host sweep**

**Purpose:** Compare lock age across hosts using coordinated clocks and a modest duration ceiling.

```
MANAGE_IN_UTC = X
DURATION = 60
DURATION_UNIT = M
GTHOST = *
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_SM12 | DEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_01_SM12 | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_SM12 | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_SM12 | GARG | Argument String (=Key Fields) of Lock Entry | CHAR(150) | EQEGRAARG |
| /SKN/S_SW_01_01_SM12 | GBCKTYPE | Backup flag for lock entry | CHAR(1) | EQEBCKTYPE |
| /SKN/S_SW_01_01_SM12 | GCLIENT | Client in the lock entry | CHAR(3) | EQECLIENT |
| /SKN/S_SW_01_01_SM12 | GMODE | Lock Mode (Shared/Exclusive) of a Lock Entry | CHAR(1) | EQEGRAMODE |
| /SKN/S_SW_01_01_SM12 | GNAME | Elementary Lock of Lock Entry (Table Name) | CHAR(30) | EQEGRANAME |
| /SKN/S_SW_01_01_SM12 | GOBJ | Name of Lock Object in the Lock Entry | CHAR(16) | EQEOBJ |
| /SKN/S_SW_01_01_SM12 | GTARG | Argument String of Lock Entry (Table Key Fields) | CHAR(50) | EQEGTARG |
| /SKN/S_SW_01_01_SM12 | GTCODE | Transaction Code in the Lock Entry | CHAR(20) | EQETCODE |
| /SKN/S_SW_01_01_SM12 | GTDATE | Date within lock owner ID | DATS(8) | EQEDATE |
| /SKN/S_SW_01_01_SM12 | GTHOST | Host Name in the Lock Owner ID | CHAR(32) | EQEHOST |
| /SKN/S_SW_01_01_SM12 | GTMARK | Selection Indicator of Lock Entry | CHAR(1) | EQEMARK |
| /SKN/S_SW_01_01_SM12 | GTSYSNR | SAP System Number in Lock Owner ID | NUMC(2) | EQESYSNR |
| /SKN/S_SW_01_01_SM12 | GTTIME | Time in Lock Owner ID | TIMS(6) | EQETIME |
| /SKN/S_SW_01_01_SM12 | GTUSEC | Time/Microseconds Share in Lock Owner ID | NUMC(6) | EQEUSEC |
| /SKN/S_SW_01_01_SM12 | GTWP | Work Process Number in Lock Owner ID | NUMC(2) | EQEWP |
| /SKN/S_SW_01_01_SM12 | GUNAME | User name in lock entry | CHAR(12) | EQEUNAME |
| /SKN/S_SW_01_01_SM12 | GUSE | Cumulative Counter for Lock Entry /Dialog | INT4(10) | EQEUSE |
| /SKN/S_SW_01_01_SM12 | GUSETXT | Cumulative Counter for Lock Entry /Dialog | NUMC(10) | EQEUSETXT |
| /SKN/S_SW_01_01_SM12 | GUSEVB | Cumulative Counter for Lock Entry / Update Task | INT4(10) | EQEUSEVB |
| /SKN/S_SW_01_01_SM12 | GUSEVBT | Cumulative Counter for Lock Entry / Update Task | NUMC(10) | EQEUSEVBT |
| /SKN/S_SW_01_01_SM12 | GUSR | Lock Owner, ID of Logical Unit of Work (LUW) | CHAR(58) | EQEUSR |
| /SKN/S_SW_01_01_SM12 | GUSRVB | Lock Owner, ID of Logical Unit of Work (LUW) /Update Task | CHAR(58) | EQEUSRVB |
| /SKN/S_SW_01_01_SM12 | PROCESS_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_SM12.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SM12 OPTIONAL
*"----------------------------------------------------------------------
DATA_MULTY: DURATION   /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
            GCLIENT EQECLIENT,
            GUNAME  EQEUNAME,
            GMODE   EQEGRAMODE,
            GTARG   EQEGTARG,
            GTCODE  EQETCODE,
            GUSR    EQEUSR,
            GNAME   EQEGRANAME,
            GARG    EQEGRAARG,
            GTHOST  EQEHOST.
DATA_SINGLE: LANGU LANGU,
             NUMBER SY-TABIX,
             SUBRC SY-SUBRC,
             MANAGE_IN_UTC CHAR1,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             DEST RFCDEST.
DATA : LS_ENQ TYPE SEQG3,
       LT_ENQ LIKE TABLE OF LS_ENQ.
DATA : SY_TABIX LIKE SY-TABIX .
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
DATA : IS_OUT(1) TYPE C.
DATA: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
*-- Fill Selection Option Tables
   SELECT_MULTY: DURATION,
                 GCLIENT,
                 GUNAME,
                 GMODE,
                 GTARG,
                 GTCODE,
                 GUSR,
                 GNAME,
                 GARG ,
                 GTHOST.
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT,
                 MANAGE_IN_UTC,
                 DEST.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_SM12'
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
 PROCESS_ICON = ICON_DELETE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  REFRESH LT_ENQ.
  CALL FUNCTION 'ENQUEUE_READ'
    DESTINATION        LV_DEST
    EXPORTING
      GCLIENT                     = ' '  " SY-MANDT
*     GNAME                       = ' '
*     GARG                        = ' '
      GUNAME                      = ' '  " SY-UNAME
*     LOCAL                       = ' '
*     FAST                        = ' '
*     GARGNOWC                    = ' '
    IMPORTING
      NUMBER                      = LV_NUMBER
      SUBRC                       = LV_SUBRC
    TABLES
      ENQ                         = LT_ENQ
   EXCEPTIONS
     COMMUNICATION_FAILURE       = 1
     SYSTEM_FAILURE              = 2
     OTHERS                      = 3.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  DELETE LT_ENQ WHERE GCLIENT NOT IN R_GCLIENT.
  DELETE LT_ENQ WHERE GUNAME NOT IN R_GUNAME.
  DELETE LT_ENQ WHERE GMODE NOT IN R_GMODE.
  DELETE LT_ENQ WHERE GTARG NOT IN R_GTARG.
  DELETE LT_ENQ WHERE GTCODE NOT IN R_GTCODE.
  DELETE LT_ENQ WHERE GTHOST NOT IN R_GTHOST.
  DELETE LT_ENQ WHERE GUSR NOT IN R_GUSR.
  DELETE LT_ENQ WHERE GNAME NOT IN R_GNAME.
  DELETE LT_ENQ WHERE GARG NOT IN R_GARG.
  LOOP AT LT_ENQ INTO LS_ENQ.
    CLEAR T_DATA.
    MOVE-CORRESPONDING LS_ENQ TO T_DATA.
    T_DATA-DEST = LV_DEST.
    APPEND T_DATA.
  ENDLOOP.
*   loop at t_data.
*     sy_tabix = sy-tabix.
*      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*        EXPORTING
*          D_FROM            = t_data-GTDATE
*          T_FROM            = t_data-GTTIME
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
*        t_data-PROCESS_ICON = PROCESS_ICON.
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
          D_FROM            = T_DATA-GTDATE
          T_FROM            = T_DATA-GTTIME
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
