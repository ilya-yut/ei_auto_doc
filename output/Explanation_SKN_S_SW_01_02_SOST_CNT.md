# Exception Indicator: SapConnect_ Send Requests (SOST) Count - SW_01_02_SOST_CNT

## General Overview

This Exception Indicator focuses on outbound SapConnect send requests and reports when the number of qualifying records crosses a configured count threshold. It helps operations and messaging teams notice unusually large batches of stuck, failed, or slow communications before queues or downstream consumers are overloaded.

This EI serves as an essential control for application messaging and interface stability by:

- Enabling early detection of abnormal volumes of send objects that may signal batch issues, connectivity problems, or misrouted traffic.
- Supporting prioritization of investigation when many messages sit in non-final states during critical business windows.
- Providing a simple count-based signal that complements detailed line lists from the related detail monitor.
- Helping month-end and peak-period reviews by surfacing spikes that warrant a second look at communication configuration and workload.
- Supporting accountability between functional owners and technical teams when send traffic deviates from expected norms.

Typical use cases include operational health checks after releases, recurring reviews during high message volume, and exception workflows where a numeric threshold triggers follow-up before users report delays.

The logic relies on standard send-request processing concepts and reuses the underlying selection model of the companion detail function so that the same business filters apply before the count is evaluated.


## Problem Description

Failure to monitor outbound send-request traffic and its concentration in problem states creates multiple risks across operational stability, customer-facing communications, and internal control over automated messaging.

**Messaging Operations and Delivery Risks**

- Large backlogs of unsent or waiting communications can delay invoices, order confirmations, and workflow steps without an obvious system error banner.
- Spikes in error-type outcomes may point to misconfiguration or partner issues that remain hidden when only ad hoc inbox checks are used.
- Teams may treat slow delivery as “normal noise” until customers or auditors surface missed legal or contractual notifications.

**System Performance and Integration Risks**

- Sudden increases in send objects can stress spool, gateway, or remote destinations before capacity reviews are triggered.
- Recurring error bands can mask a single root cause when volume is never compared to a baseline expectation.
- Cross-system interfaces may appear healthy at the technical ping level while business messages pile up in intermediate states.

**Management Visibility and Decision-Making Risks**

- Leadership lacks a compact indicator when only raw transaction lists or technical traces are consulted sporadically.
- Prioritization between “open a ticket” and “watch and wait” becomes subjective without a repeatable volume-based trigger.
- Post-incident reviews struggle to show that monitoring design matched the real communication pattern.

## Suggested Resolution

**Immediate Response**

- When the monitor raises attention, review the current send-request landscape in the standard communication monitoring transaction for the same business scope.
- Validate whether the spike aligns with a known campaign, batch job, or migration window.
- Confirm that critical message types (billing, shipping, alerts) are not disproportionately represented in non-final states.
- Capture screenshots or export samples for the service desk if escalation is required.

**System Assessment**

- Compare the current observation to the prior week and month for the same organizational slice to judge persistence versus one-off load.
- Check whether partner or destination changes coincided with the increase.
- Review recent transport or configuration activity that affects message creation or routing.
- Correlate with application job schedules that mass-create communications.

**Corrective Actions**

- Correct master data or output determination settings when erroneous message generation is confirmed.
- Coordinate with basis or middleware teams on destination or gateway capacity if technical limits are suspected.
- Adjust monitoring thresholds or scope after baseline noise is understood, and document the rationale for audit trail.
- Schedule recurring operational reviews and, where helpful, add short enablement notes into existing runbooks so first-line staff recognize the pattern without a separate training module.


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 3 | DURATION_UNIT | Duration Unit(D/H/M) |  | 0 | 0 |  |  |
| 4 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 5 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 6 | SNDART | AddrType | CHAR | 3 | 0 | SX_ADDRTYP | SX_ADDRTYP |
| 7 | SNDNO | Sender no. | CHAR | 12 | 0 | SO_SND_NO | SO_OBJ_NO |
| 8 | SNDTP | Sender type | CHAR | 3 | 0 | SO_SND_TP | SO_OBJ_TP |
| 9 | SOST_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 10 | STAT_ERROR | 'X' - Errors |  | 0 | 0 |  |  |
| 11 | STAT_OK | 'X' - Sent |  | 0 | 0 |  |  |
| 12 | STAT_TRANSIT | 'X' - Transmitted |  | 0 | 0 |  |  |
| 13 | STAT_WAIT | 'X' - Waiting |  | 0 | 0 |  |  |
| 14 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 15 | STATUS | status of the sent object |  | 0 | 0 |  |  |
| 16 | USERNAM | Sender |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 16 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**DURATION** (Duration In Time Units)

Uses duration in time units from the source context so only records with DURATION inside declared values proceed.

**DURATION_UNIT** (Duration Unit(D/H/M))

Unit for elapsed time between each session's creation date and time and the evaluation clock.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**LANGU** (Language for texts)

Improves readability of exported lists because language for texts (LANGU) columns stay aligned with the configured filter intent.

**MANAGE_IN_UTC** ('X' - Manage in UTC)

Controls whether reference timestamps for filtering and duration checks are interpreted in UTC or local time.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**SNDART** (AddrType)

Helps distinguish technical versus business attributes when addrtype on SNDART correlates with counters or status fields.

**SNDNO** (Sender no.)

Prevents accidental global scans when sender no. (SNDNO) is meant to stay within a controlled application slice.

**SNDTP** (Sender type)

Improves readability of exported lists because sender type (SNDTP) columns stay aligned with the configured filter intent.

**SOST_CNT** (Count)

Allows phased rollout: first widen SOST_CNT for count, then tighten thresholds once baseline noise is understood.

**STAT_ERROR** ('X' - Errors)

Optional send-state selector: when set, the monitor applies this outcome flag together with the other active status dimensions.

**STAT_ERROR Options:**
- **X** — Restrict the extract to rows where this send or processing state is active for the object.
- Empty or initial — Do not use this flag as a filter dimension.

**STAT_OK** ('X' - Sent)

Optional send-state selector: when set, the monitor applies this outcome flag together with the other active status dimensions.

**STAT_OK Options:**
- **X** — Restrict the extract to rows where this send or processing state is active for the object.
- Empty or initial — Do not use this flag as a filter dimension.

**STAT_TRANSIT** ('X' - Transmitted)

Optional send-state selector: when set, the monitor applies this outcome flag together with the other active status dimensions.

**STAT_TRANSIT Options:**
- **X** — Restrict the extract to rows where this send or processing state is active for the object.
- Empty or initial — Do not use this flag as a filter dimension.

**STAT_WAIT** ('X' - Waiting)

Optional send-state selector: when set, the monitor applies this outcome flag together with the other active status dimensions.

**STAT_WAIT Options:**
- **X** — Restrict the extract to rows where this send or processing state is active for the object.
- Empty or initial — Do not use this flag as a filter dimension.

**STATE_COLOR** (State Color)

Filters lines by the derived color bucket used for severity-style triage in the monitor framework.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional literals may exist where the framework extends the palette for neutral states.

**STATUS** (status of the sent object)

Restricts the extract to the operational status values you configure for this EI's object type.

**STATUS Options:**
- Use status domain values defined for the underlying SAP object (see data element or domain in the system).
- Code applies STATUS as a filter; literals are environment-specific.

**USERNAM** (Sender)

After data is read, lines are removed unless sender on USERNAM still satisfies the active multivalued selection.


### Parameter Relationships

How parameter combinations work together

Sender identity, address type, and language selections narrow which send objects enter the working set before state, color, and status filters are applied. Duration-related inputs shape how long objects have remained in their current state relative to the evaluation moment, while lookback shapes how far back the initial read reaches when no explicit monitoring dates are supplied. The count threshold then decides whether the summarized volume is high enough to surface an alert after the detail function has produced its candidate list.

State flags are evaluated together with the generic status and color selectors so that operations can target, for example, waiting or error bands without contradicting the broader status list. Time-zone handling applies consistently to timestamp comparisons used with duration logic. When all dimensions are left wide, the monitor still applies framework defaults for lookback and unit so that the run remains bounded and comparable from one execution to the next.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code (one day of lookback when building the default monitoring date window from the evaluation clock).
- **DURATION** - initial - treated as unset by code (the duration interval filter does not remove rows until a populated duration range is supplied).
- **DURATION_UNIT** - initial - treated as M by code (minutes as the unit for elapsed-time calculation when the parameter remains blank at read time).

**Note:** Default handling for lookback and duration is implemented in the called detail routine; this count wrapper forwards selection tables and reads the alert flag from that routine before comparing the resulting table size to the configured count limit.

### Practical Example of Parameter Configuration

**Use Case 1: Tight operational slice for a busy hour**

**Purpose:** Focus on one sender during a short window while still requiring a meaningful volume before alerting.

```
BACKDAYS = 1
DURATION = 120
DURATION_UNIT = M
USERNAM = BATCH_USER
SOST_CNT = 500
```

**Use Case 2: Error-heavy corridor with color emphasis**

**Purpose:** Highlight when many red-state communications accumulate for monitored address types.

```
BACKDAYS = 3
STATE_COLOR = R
STAT_ERROR = X
SNDART = INT
STATUS = ERR
SOST_CNT = 50
MANAGE_IN_UTC = X
```

**Use Case 3: Broad discovery with higher threshold**

**Purpose:** Catch only very large backlogs during month-end when traffic is expected to rise.

```
BACKDAYS = 7
DURATION = 2880
DURATION_UNIT = M
STAT_WAIT = X
STAT_TRANSIT = X
SNDTP = BUP
SNDNO = 10000001
LANGU = E
SOST_CNT = 2000
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_02_SOST_CNT | SOST_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_02_SOST_CNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_SOST_CNT OPTIONAL
*"----------------------------------------------------------------------
DATA : LV_ALERT TYPE  CHAR1.
DATA : LS_DATA TYPE /SKN/S_SW_01_02_SOST,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA : LV_CNT TYPE I.
DATA_MULTY: SOST_CNT /SKN/E_SW_CNT.
SELECT_MULTY: SOST_CNT.
   REFRESH T_DATA.
   CALL FUNCTION '/SKN/F_SW_01_02_SOST'
    IMPORTING
       IS_ALERT       = LV_ALERT
     TABLES
       T_SELECT       = T_SELECT
       T_DATA         = LT_DATA.
    IS_ALERT = LV_ALERT.
    DESCRIBE TABLE LT_DATA LINES LV_CNT.
    IF LV_CNT IN R_SOST_CNT.
      T_DATA-SOST_CNT = LV_CNT.
      APPEND T_DATA.
      IS_ALERT = LV_ALERT.
    ELSE.
      CLEAR IS_ALERT.
    ENDIF.
ENDFUNCTION.
```
