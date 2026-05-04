# Exception Indicator: Batch Input Monitoring - SW_01_01_SYS_BDC

## General Overview

This Exception Indicator monitors SAP batch input sessions and their queue-style attributes as operations teams would review them for failed, stuck, or unusually long-running work behind transaction SM35-style administration.

This EI serves as an essential control for Basis and application support by:
- Surfacing batch sessions that remain in states or color bands that warrant investigation or repeat processing
- Highlighting concentration of risk by creator, session name, data type, or queue state when volumes grow unevenly
- Supporting incident review when drivers, programs, or message counters move outside normal bands after releases or data loads
- Giving teams a bounded extract when global session lists would hide the few entries that actually threaten close or cutover activities
- Helping internal audit demonstrate that batch input health was checked on a recurring basis for sensitive processing windows

Typical use includes daily operations checks, troubleshooting stuck sessions, and validating cleanup after maintenance. Teams still use standard batch input monitoring when a session needs immediate retry or deletion.

The routine reads batch input header data for the configured selection, enriches state presentation, then evaluates elapsed time from each line’s creation timestamp against the configured duration window.


## Problem Description

Weak oversight of batch input sessions creates operational and data-quality exposure:

**Processing and Throughput Risks**
- Stuck or failing sessions can accumulate without a prioritized view of which creators, programs, or queue states drive the backlog
- Silent growth of error or restart-prone states delays posting until users or downstream jobs fail first
- Cross-client or cross-session-type noise in raw lists hides the few sessions that actually threaten throughput

**Data Consistency and Control Risks**
- Long-running sessions increase the window where dependent steps see partially applied batch work
- Without a consolidated monitor, teams lack a single place to tie session state to owner and program context

**Governance and Evidence Risks**
- Management lacks objective evidence of how batch input queues behaved during critical close or migration windows

## Suggested Resolution

**Immediate Response**
- Review each flagged line for session name, creator, queue state, color bucket, and program context to judge business impact and owner
- Open standard batch input administration for the same selection to validate current status and appropriate next action
- Capture run time, client, and dominant attributes when the finding ties to regulated or financially material processing

**System Assessment**
- Segment results by client, creator, data type, queue state, and derived color bucket to see where volume or age concentrates
- Compare current extracts to prior monitoring cycles after transports, data loads, or infrastructure changes
- Validate cloud or remote destination settings when the monitor is meant to evaluate a central hub

**Corrective Actions**
- Retry, correct, or delete sessions according to SAP batch input administration guidance, then re-run the monitor to confirm the population returned within tolerance
- Adjust parameters after root-cause review so recurring benign patterns are excluded without hiding genuine risk
- Document remediation for audit when delayed batch input affected materially sensitive processes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 2 | CREATOR | Created By | CHAR | 12 | 0 | APQ_MAPN | CHAR12 |
| 3 | CREDATE | CREATED ON | DATS | 8 | 0 | APQ_CRDA | DATUM |
| 4 | CRETIME | TIME CREATED | TIMS | 6 | 0 | APQ_CRTI | UZEIT |
| 5 | DATATYP | DATA TYPE | CHAR | 4 | 0 | APQ_DTYP | CHAR4 |
| 6 | DATUM | Date |  | 0 | 0 |  |  |
| 7 | DESTAPP | Target application | CHAR | 8 | 0 | APQ_APPL | CHAR8 |
| 8 | DESTSYS | Target System | CHAR | 32 | 0 | APQ_DEST | HOST_ID |
| 9 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 11 | FORMID | Modules | CHAR | 30 | 0 | APQ_MODP | CHAR30 |
| 12 | GETACTIVE | Driver status GET | CHAR | 1 | 0 | APQ_INAC | CHAR1 |
| 13 | GETBLOCK | Queue sum log.blocks | INT4 | 10 | 0 | APQ_INBL | CDINT4 |
| 14 | GETDATE | GETQ last GETQ/error | DATS | 8 | 0 | APQ_INDA | DATUM |
| 15 | GETPID | GETQ process ID | INT4 | 10 | 0 | APQ_INID | CDINT4 |
| 16 | GETTIME | GETQ last time/error | TIMS | 6 | 0 | APQ_INTI | UZEIT |
| 17 | GETTRANS | GETQ trans.count | INT4 | 10 | 0 | APQ_INTR | CDINT4 |
| 18 | GROUPID | Session Name | CHAR | 12 | 0 | APQ_GRPN | CHAR12 |
| 19 | MANDANT | Client | CLNT | 3 | 0 | APQ_MANDT | MANDT |
| 20 | MSGCNT | Message counter | INT4 | 10 | 0 | APQ_RECO | CDINT4 |
| 21 | MSGCNTD | Message counter | INT4 | 10 | 0 | APQ_RECO | CDINT4 |
| 22 | MSGCNTE | Message counter | INT4 | 10 | 0 | APQ_RECO | CDINT4 |
| 23 | MSGCNTF | Message counter | INT4 | 10 | 0 | APQ_RECO | CDINT4 |
| 24 | PASSWD | Password | CHAR | 40 | 0 | APQ_PASS | XUBCODE |
| 25 | PROGID | Program | CHAR | 40 | 0 | APQ_PROG | PROGNAME |
| 26 | PUTACTIVE | PUTQ active flag | CHAR | 1 | 0 | APQ_OUAC | CHAR1 |
| 27 | PUTBLOCK | Queue sum log.blocks | INT4 | 10 | 0 | APQ_OUBL | CDINT4 |
| 28 | PUTDATE | PUTQ last PUTQ/error | DATS | 8 | 0 | APQ_OUDA | DATUM |
| 29 | PUTPID | PUTQ process ID | INT4 | 10 | 0 | APQ_OUID | CDINT4 |
| 30 | PUTTIME | PUTQ last time/error | TIMS | 6 | 0 | APQ_OUTI | UZEIT |
| 31 | PUTTRANS | PUTQ trans.count | INT4 | 10 | 0 | APQ_OUTR | CDINT4 |
| 32 | QATTRIB | Attribute | CHAR | 1 | 0 | APQ_QATT | CHAR1 |
| 33 | QERASE | Deletion Indicator | CHAR | 1 | 0 | APQ_QDEL | CHAR1 |
| 34 | QID | Queue name | CHAR | 20 | 0 | APQ_QUID | CHAR20 |
| 35 | QSTATE | STATUS | CHAR | 1 | 0 | APQ_STAT | HSTAT |
| 36 | STARTDATE | Start date | DATS | 8 | 0 | APQ_STDA | DATUM |
| 37 | STARTMODE | Start mode | CHAR | 1 | 0 | APQ_STRT | CHAR1 |
| 38 | STARTPGID | Driver program | CHAR | 40 | 0 | APQ_TREI | CHAR40 |
| 39 | STARTTIME | Start time | TIMS | 6 | 0 | APQ_STTI | UZEIT |
| 40 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 41 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 42 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 43 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 44 | TRANSCNT | Transaction counter | INT4 | 10 | 0 | APQ_TRAN | CDINT4 |
| 45 | TRANSCNTD | Transaction counter | INT4 | 10 | 0 | APQ_TRAN | CDINT4 |
| 46 | TRANSCNTE | Transaction counter | INT4 | 10 | 0 | APQ_TRAN | CDINT4 |
| 47 | TRANSCNTF | Transaction counter | INT4 | 10 | 0 | APQ_TRAN | CDINT4 |
| 48 | USERID | User ID | CHAR | 12 | 0 | APQ_MAPN | CHAR12 |
| 49 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 49 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Back Days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on CREDATE

**CREATOR** (Created By)

Helps monitoring stay readable by requiring created by (CREATOR) to match organizational or technical selectors when set.

**CREDATE** (CREATED ON)

Mirrors how administrators slice operational lists: created on (CREDATE) is one lever that shapes which rows are comparable run over run.

**CRETIME** (TIME CREATED)

Allows phased rollout: first widen CRETIME for time created, then tighten thresholds once baseline noise is understood.

**DATATYP** (DATA TYPE)

Gives auditors traceable criteria because data type on DATATYP is applied consistently before any alert flag is raised.

**DATUM** (Date)

Aligns exception volume with the chosen scope by testing date via DATUM before alert evaluation.

**DESTAPP** (Target application)

Supports operational control by evaluating target application through DESTAPP for each candidate record.

**DESTSYS** (Target System)

When left open per framework rules, DESTSYS does not restrict target system; when set, only matching rows remain.

**DURATION** (Duration In Time Units)

Works downstream of the initial read so duration in time units on DURATION still participates in row-level deletion rules.

**DURATION_UNIT** (Duration Unit(D/H/M))

Unit for elapsed time between each session's creation date and time and the evaluation clock.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**FORMID** (Modules)

Pairs with duration logic: once FORMID passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**GETACTIVE** (Driver status GET)

Valuable when comparing health before and after a release—hold driver status get on GETACTIVE constant while varying other filters.

**GETBLOCK** (Queue sum log.blocks)

Helps monitoring stay readable by requiring queue sum log.blocks (GETBLOCK) to match organizational or technical selectors when set.

**GETDATE** (GETQ last GETQ/error)

Prevents accidental global scans when getq last getq/error (GETDATE) is meant to stay within a controlled application slice.

**GETPID** (GETQ process ID)

Guards against oversized extracts when getq process id on GETPID is narrowed together with client, user, or session filters.

**GETTIME** (GETQ last time/error)

Helps monitoring stay readable by requiring getq last time/error (GETTIME) to match organizational or technical selectors when set.

**GETTRANS** (GETQ trans.count)

Works downstream of the initial read so getq trans.count on GETTRANS still participates in row-level deletion rules.

**GROUPID** (Session Name)

When populated, keeps the extract focused so session name (GROUPID) aligns with the intended triage slice.

**MANDANT** (Client)

Documents expected operator behavior—client on MANDANT should be set when that dimension is part of the control objective.

**MSGCNT** (Message counter)

For operations, message counter on MSGCNT indicates whether a row belongs in the current monitoring pass versus historical noise.

**MSGCNTD** (Message counter)

Supports operational control by evaluating message counter through MSGCNTD for each candidate record.

**MSGCNTE** (Message counter)

Reduces false positives during peak windows by tightening message counter through MSGCNTE alongside state filters.

**MSGCNTF** (Message counter)

Helps monitoring stay readable by requiring message counter (MSGCNTF) to match organizational or technical selectors when set.

**PASSWD** (Password)

After data is read, lines are removed unless password on PASSWD still satisfies the active multivalued selection.

**PROGID** (Program)

Combines with related filters so program on PROGID refines which records remain for duration or state checks.

**PUTACTIVE** (PUTQ active flag)

Guards against oversized extracts when putq active flag on PUTACTIVE is narrowed together with client, user, or session filters.

**PUTBLOCK** (Queue sum log.blocks)

Treats queue sum log.blocks as a discriminator between similar rows that would otherwise look identical in a raw extract.

**PUTDATE** (PUTQ last PUTQ/error)

Separates cross-client noise from in-scope work when putq last putq/error on PUTDATE correlates with client or user attributes.

**PUTPID** (PUTQ process ID)

When populated, keeps the extract focused so putq process id (PUTPID) aligns with the intended triage slice.

**PUTTIME** (PUTQ last time/error)

Aligns exception volume with the chosen scope by testing putq last time/error via PUTTIME before alert evaluation.

**PUTTRANS** (PUTQ trans.count)

Captures edge cases where putq trans.count (PUTTRANS) must be non-default to reproduce a customer-specific monitoring scenario.

**QATTRIB** (Attribute)

Separates cross-client noise from in-scope work when attribute on QATTRIB correlates with client or user attributes.

**QERASE** (Deletion Indicator)

Guards against oversized extracts when deletion indicator on QERASE is narrowed together with client, user, or session filters.

**QID** (Queue name)

Aligns exception volume with the chosen scope by testing queue name via QID before alert evaluation.

**QSTATE** (STATUS)

Limits rows to the queue or processing state values you declare, so monitoring can target only selected outcome bands.

**QSTATE Options:**
- Use standard SAP status values configured for the monitored object type.
- Code in this EI applies QSTATE as a selector but does not enumerate fixed literals inline.

**STARTDATE** (Start date)

Prevents accidental global scans when start date (STARTDATE) is meant to stay within a controlled application slice.

**STARTMODE** (Start mode)

Helps distinguish technical versus business attributes when start mode on STARTMODE correlates with counters or status fields.

**STARTPGID** (Driver program)

Narrows retrieved rows where driver program (STARTPGID) must match the configured selection for this monitor.

**STARTTIME** (Start time)

When tightened, start time (STARTTIME) removes rows that would otherwise dilute attention from failing or stuck cases.

**STATE_COLOR** (State Color)

Filters lines by the derived color bucket used for severity-style triage in the monitor framework.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional literals may exist where the framework extends the palette for neutral states.

**STATE_ICON** (State Icon)

Uses state icon from the source context so only records with STATE_ICON inside declared values proceed.

**STATUS_DESC** (SW Message)

Improves readability of exported lists because sw message (STATUS_DESC) columns stay aligned with the configured filter intent.

**SW_DEST** (Cloud Destination)

Reflects real administration where cloud destination on SW_DEST is routinely restricted to a single productive client or object family.

**TRANSCNT** (Transaction counter)

Separates cross-client noise from in-scope work when transaction counter on TRANSCNT correlates with client or user attributes.

**TRANSCNTD** (Transaction counter)

Improves readability of exported lists because transaction counter (TRANSCNTD) columns stay aligned with the configured filter intent.

**TRANSCNTE** (Transaction counter)

Gives auditors traceable criteria because transaction counter on TRANSCNTE is applied consistently before any alert flag is raised.

**TRANSCNTF** (Transaction counter)

Helps monitoring stay readable by requiring transaction counter (TRANSCNTF) to match organizational or technical selectors when set.

**USERID** (User ID)

When left open per framework rules, USERID does not restrict user id; when set, only matching rows remain.

**USER_FLD** (Dynamic Recipient User Field)

Optional dynamic recipient or routing field used by the monitor framework when populated for distribution or extensions.

**USER_FLD Options:**
No fixed USER_FLD value list is defined in the available code for this EI.


### Parameter Relationships

How parameter combinations work together

**DATUM** supplies an explicit monitoring-date range when you populate it, so the session population is anchored to calendar bounds you choose instead of a relative lookback.

When **DATUM** is not provided, **BACKDAYS** is the fallback that builds the lower monitoring date from the evaluation day backward, and that same window is then applied to creation-date selection unless you override **CREDATE** separately.

**DURATION** and **DURATION_UNIT** act as an additional filter after date-oriented selection: only sessions whose elapsed time from creation to the evaluation moment still fits the configured duration band remain in the extract.

Both the date criteria (explicit **DATUM** or **BACKDAYS**-driven window, together with **CREDATE** when set) and the **DURATION** / **DURATION_UNIT** age test are applied together—lines must satisfy the date side and the duration side before alerting logic runs.

**STATE_COLOR** and **QSTATE** should be configured together when you want the color bucket to reflect only the queue-state slice you consider material for escalation.

**SW_DEST**, when used, shifts execution to the alternate path so the same parameter bundle is evaluated in the remote destination’s context before duration and state-color trimming.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code (SY-DATUM minus 1 day lower bound when DATUM and BACKDAYS selections do not fill the range).
- **DURATION** - initial - treated as empty by code (no duration range; age filter off).
- **DURATION_UNIT** - initial - treated as M by code (minutes).

### Practical Example of Parameter Configuration

**Use Case 1: Error-colored sessions only**

**Purpose:** Focus on sessions the framework classifies in the error color bucket for one productive client.

```
MANDANT = 100
STATE_COLOR = R
QSTATE = E
CREATOR = BATCH01
```

**Use Case 2: Explicit date plus duration cap**

**Purpose:** Use a fixed calendar window for creation dates and cap how old sessions can be in minutes.

```
DATUM = 20250101 - 20251231
DURATION = 120
DURATION_UNIT = M
DATATYP = BATCH
```

**Use Case 3: Full-day aging on a program**

**Purpose:** Flag sessions older than thirty full days for a known driver program.

```
PROGID = Z_MASS_POST
DURATION = 30
DURATION_UNIT = F
GROUPID = CLOSE*
```

**Use Case 4: Session name slice**

**Purpose:** Watch a naming pattern and require multiple hits before paging.

```
GROUPID = MONTH_END*
MSGCNT = 5 - 999999
QSTATE = E
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_SYS_BDC | CREATOR | Queue user ID / for historical reasons | CHAR(12) | APQ_MAPN |
| /SKN/S_SW_01_01_SYS_BDC | CREDATE | Queue creation date | DATS(8) | APQ_CRDA |
| /SKN/S_SW_01_01_SYS_BDC | CRETIME | Queue creation time | TIMS(6) | APQ_CRTI |
| /SKN/S_SW_01_01_SYS_BDC | DATATYP | Queue data type | CHAR(4) | APQ_DTYP |
| /SKN/S_SW_01_01_SYS_BDC | DESTAPP | Queue target application | CHAR(8) | APQ_APPL |
| /SKN/S_SW_01_01_SYS_BDC | DESTSYS | Queue target system | CHAR(32) | APQ_DEST |
| /SKN/S_SW_01_01_SYS_BDC | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_SYS_BDC | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_SYS_BDC | FORMID | Queue module/unique | CHAR(30) | APQ_MODP |
| /SKN/S_SW_01_01_SYS_BDC | GETACTIVE | Queue GETQ active flag | CHAR(1) | APQ_INAC |
| /SKN/S_SW_01_01_SYS_BDC | GETBLOCK | Queue GETQ sum of logical blocks | INT4(10) | APQ_INBL |
| /SKN/S_SW_01_01_SYS_BDC | GETDATE | Queue GETQ date last GETQ or error | DATS(8) | APQ_INDA |
| /SKN/S_SW_01_01_SYS_BDC | GETPID | Queue PUTQ Process ID | INT4(10) | APQ_INID |
| /SKN/S_SW_01_01_SYS_BDC | GETTIME | Queue GETQ time last GETQ or error | TIMS(6) | APQ_INTI |
| /SKN/S_SW_01_01_SYS_BDC | GETTRANS | Queue GETQ transaction counter | INT4(10) | APQ_INTR |
| /SKN/S_SW_01_01_SYS_BDC | GROUPID | Group name: Batch input session name | CHAR(12) | APQ_GRPN |
| /SKN/S_SW_01_01_SYS_BDC | MANDANT | Queue client | CLNT(3) | APQ_MANDT |
| /SKN/S_SW_01_01_SYS_BDC | MSGCNT | Message counter: Batch input, statistics | INT4(10) | APQ_RECO |
| /SKN/S_SW_01_01_SYS_BDC | MSGCNTD | Message counter: Batch input, statistics | INT4(10) | APQ_RECO |
| /SKN/S_SW_01_01_SYS_BDC | MSGCNTE | Message counter: Batch input, statistics | INT4(10) | APQ_RECO |
| /SKN/S_SW_01_01_SYS_BDC | MSGCNTF | Message counter: Batch input, statistics | INT4(10) | APQ_RECO |
| /SKN/S_SW_01_01_SYS_BDC | PASSWD | Queue password | CHAR(40) | APQ_PASS |
| /SKN/S_SW_01_01_SYS_BDC | PROGID | Queue program | CHAR(40) | APQ_PROG |
| /SKN/S_SW_01_01_SYS_BDC | PUTACTIVE | Queue PUTQ active flag | CHAR(1) | APQ_OUAC |
| /SKN/S_SW_01_01_SYS_BDC | PUTBLOCK | Queue PUTQ sum of logical blocks | INT4(10) | APQ_OUBL |
| /SKN/S_SW_01_01_SYS_BDC | PUTDATE | Queue PUTQ date of last PUTQ or error | DATS(8) | APQ_OUDA |
| /SKN/S_SW_01_01_SYS_BDC | PUTPID | Queue PUTQ Process ID | INT4(10) | APQ_OUID |
| /SKN/S_SW_01_01_SYS_BDC | PUTTIME | Queue PUTQ time last PUTQ or error | TIMS(6) | APQ_OUTI |
| /SKN/S_SW_01_01_SYS_BDC | PUTTRANS | Queue PUTQ transaction counter | INT4(10) | APQ_OUTR |
| /SKN/S_SW_01_01_SYS_BDC | QATTRIB | Queue attribute (unique or append) | CHAR(1) | APQ_QATT |
| /SKN/S_SW_01_01_SYS_BDC | QERASE | Queue deletion indicator for processed sessions | CHAR(1) | APQ_QDEL |
| /SKN/S_SW_01_01_SYS_BDC | QID | Queue identification (unique key) | CHAR(20) | APQ_QUID |
| /SKN/S_SW_01_01_SYS_BDC | QSTATE | Queue status | CHAR(1) | APQ_STAT |
| /SKN/S_SW_01_01_SYS_BDC | STARTDATE | Queue start date | DATS(8) | APQ_STDA |
| /SKN/S_SW_01_01_SYS_BDC | STARTMODE | Queue start mode | CHAR(1) | APQ_STRT |
| /SKN/S_SW_01_01_SYS_BDC | STARTPGID | Queue driver program | CHAR(40) | APQ_TREI |
| /SKN/S_SW_01_01_SYS_BDC | STARTTIME | Queue start time | TIMS(6) | APQ_STTI |
| /SKN/S_SW_01_01_SYS_BDC | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_01_SYS_BDC | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_01_SYS_BDC | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_01_SYS_BDC | TRANSCNT | Transaction counter: Batch input, statistics | INT4(10) | APQ_TRAN |
| /SKN/S_SW_01_01_SYS_BDC | TRANSCNTD | Transaction counter: Batch input, statistics | INT4(10) | APQ_TRAN |
| /SKN/S_SW_01_01_SYS_BDC | TRANSCNTE | Transaction counter: Batch input, statistics | INT4(10) | APQ_TRAN |
| /SKN/S_SW_01_01_SYS_BDC | TRANSCNTF | Transaction counter: Batch input, statistics | INT4(10) | APQ_TRAN |
| /SKN/S_SW_01_01_SYS_BDC | USERID | Queue user ID / for historical reasons | CHAR(12) | APQ_MAPN |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_SYS_BDC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SYS_BDC OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_GROUPID FOR APQI-GROUPID ,
         R_STATE_COLOR FOR /SKN/S_SW_SYS_BDC-STATE_COLOR,
         R_DATATYP FOR APQI-DATATYP,
         R_CREATOR FOR APQI-CREATOR,
         R_CREDATE FOR APQI-CREDATE,
         R_CRETIME FOR APQI-CRETIME,
         R_QSTATE   FOR APQI-QSTATE,
         R_DATUM   FOR SY-DATUM .
DATA : DATE_FROM LIKE SY-DATUM ,
       BACKDAYS  TYPE I .
DATA_MULTY: DURATION   /SKN/E_SW_DURATION.   "From NOW to Job Start Time point -in duration units
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
DATA : SY_TABIX LIKE SY-TABIX .
DATA :  ENDDATE LIKE SY-DATUM,
        ENDTIME LIKE SY-UZEIT.
DATA : IS_OUT(1) TYPE C.
*-- Fill Selection Option Tables
  SELECT_MULTY: DURATION.
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_SYS_BDC'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
   LOOP AT T_SELECT WHERE FIELDNM = 'GROUPID'.
     MOVE-CORRESPONDING T_SELECT TO R_GROUPID.
     APPEND R_GROUPID.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'DATATYP'.
     MOVE-CORRESPONDING T_SELECT TO R_DATATYP.
     APPEND R_DATATYP.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'CREATOR'.
     MOVE-CORRESPONDING T_SELECT TO R_CREATOR.
     APPEND R_CREATOR.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'CREDATE'.
     MOVE-CORRESPONDING T_SELECT TO R_CREDATE.
     APPEND R_CREDATE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'CRETIME'.
     MOVE-CORRESPONDING T_SELECT TO R_CRETIME.
     APPEND R_CRETIME.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
     MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
     APPEND R_STATE_COLOR.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'QSTATE'.
     MOVE-CORRESPONDING T_SELECT TO R_QSTATE.
     APPEND R_QSTATE.
   ENDLOOP.
*
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
  IF R_CREDATE[] IS INITIAL.
    R_CREDATE[] = R_DATUM[] .
  ENDIF.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
   SELECT *
      FROM APQI
      INTO CORRESPONDING FIELDS OF TABLE T_DATA
      WHERE DESTSYS  = SPACE       "In Batch-Input always SPACE
        AND DESTAPP  = SPACE       "In Batch-Input always SPACE
        AND FORMID  = SPACE        "In Batch-Input always SPACE
        AND QATTRIB  = SPACE       "In Batch-Input always SPACE
        AND GROUPID IN R_GROUPID
        AND DATATYP IN  R_DATATYP
        AND CREATOR IN R_CREATOR
        AND CREDATE IN R_CREDATE
        AND CRETIME IN R_CRETIME
        AND QSTATE IN R_QSTATE.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
     CALL FUNCTION '/SKN/F_SW_01_01_BDC_STATUS'
       EXPORTING
         QSTATE            = T_DATA-QSTATE
      IMPORTING
        STATUS_DESC       = T_DATA-STATUS_DESC
        STATE_COLOR       = T_DATA-STATE_COLOR
        STATE_ICON        = T_DATA-STATE_ICON               .
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-CREDATE
          T_FROM            = T_DATA-CRETIME
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
