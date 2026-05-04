# Exception Indicator: SAPconnect Send Requests Count Monitoring (SW_01_02_SOST_CNT)


## General Overview

This Exception Indicator (EI) provides comprehensive count-based monitoring and analysis of SAP SAPconnect send requests to ensure communication volume thresholds are maintained and identify potential processing bottlenecks or abnormal communication patterns. This count monitoring focuses on volume analysis rather than detailed request inspection, making it essential for capacity management and early detection of communication system overload scenarios.

SAPconnect Count Monitoring extends the standard SAPconnect monitoring framework by providing volume-based alerting capabilities with count threshold management. The technology enables proactive monitoring of communication processing volumes and provides early warning when send request counts exceed defined operational thresholds, crucial for maintaining system performance and preventing communication backlogs.

This Exception Indicator provides detailed SAPconnect count monitoring capabilities that enable:

Volume threshold monitoring to track send request counts against predefined operational limits

Communication load analysis for understanding processing volume patterns and identifying peak usage periods

Capacity management to monitor system communication processing capacity and utilization

Trend analysis for identifying gradual increases in communication volume that may impact performance

Threshold-based alerting for proactive notification when communication volumes exceed acceptable limits

The monitoring solution analyzes SAPconnect volume statistics by leveraging the main SOST monitoring function and applying count-based filtering to focus on volume thresholds rather than individual request details. This enables targeted analysis of communication volume characteristics and identification of capacity-related issues before they impact business operations.

This Exception Indicator checks whether SAP SAPconnect processing volumes are within acceptable operational thresholds and identifies potential capacity issues that may impact automated communication delivery performance and system stability.


## Problem Description

Excessive SAPconnect processing volumes and abnormal count patterns indicate communication system capacity issues causing:

Volume and Capacity Problems

High communication volumes exceeding system processing capacity leading to backlogs and delivery delays

Abnormal send request accumulation indicating processing bottlenecks or system performance degradation

Communication queue growth beyond operational thresholds affecting system resource utilization

Volume spikes disrupting normal communication processing workflows and system stability

System Performance Issues

Resource exhaustion from excessive communication processing workload affecting overall system performance

Memory consumption issues from accumulated send requests impacting available system resources

Processing capacity limitations causing communication delivery delays and system responsiveness issues

Database performance impact from high-volume communication transaction processing affecting system operations

Business Impact

Communication delivery delays due to volume-related processing bottlenecks affecting business operations

SLA violations from capacity-related performance degradation impacting service level commitments

Business process interruptions from communication system overload affecting operational continuity

Potential communication failures from resource exhaustion compromising business communication reliability


## Suggested Resolution

Immediate Response

Investigate high communication volumes using SOST transaction for detailed volume analysis and trend identification

Check system resource utilization and processing capacity during high-volume periods for capacity assessment

Review communication processing queues and system performance metrics for bottleneck identification

Analyze volume patterns and identify communication sources contributing to high processing loads

System Assessment

Monitor communication volume trends and processing capacity utilization for optimization opportunities

Evaluate system sizing and resource allocation for communication processing workload management

Check communication channel configuration and processing parameters for performance optimization

Analyze volume patterns by communication type, time period, and system for capacity planning

Corrective Actions

Optimize communication processing parameters and system configurations for improved volume handling capacity

Implement volume management and throttling mechanisms for enhanced capacity control and performance

Establish proactive volume monitoring and capacity planning procedures for early issue detection and prevention

Plan system capacity upgrades based on communication volume analysis and growth projections


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
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

BACKDAYS (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

DURATION (Duration In Time Units)

Uses duration in time units from the source context so only records with DURATION inside declared values proceed.

DURATION_UNIT (Duration Unit(D/H/M))

Unit for elapsed time between each session's creation date and time and the evaluation clock.

DURATION_UNIT Options:

·        H — Hours.

·        M — Minutes (preset in code before the selection read when not overridden).

·        D — Days.

·        F — Full-day style counting where applicable to the duration helper.

LANGU (Language for texts)

Improves readability of exported lists because language for texts (LANGU) columns stay aligned with the configured filter intent.

MANAGE_IN_UTC ('X' - Manage in UTC)

Controls whether reference timestamps for filtering and duration checks are interpreted in UTC or local time.

MANAGE_IN_UTC Options:

·        X — UTC mode for the relevant timestamp comparisons.

·        Empty or blank — local time / framework default for the application server clock context.

SNDART (AddrType)

Helps distinguish technical versus business attributes when addrtype on SNDART correlates with counters or status fields.

SNDNO (Sender no.)

Prevents accidental global scans when sender no. (SNDNO) is meant to stay within a controlled application slice.

SNDTP (Sender type)

Improves readability of exported lists because sender type (SNDTP) columns stay aligned with the configured filter intent.

SOST_CNT (Count)

Allows phased rollout: first widen SOST_CNT for count, then tighten thresholds once baseline noise is understood.

STAT_ERROR ('X' - Errors)

Optional send-state selector: when set, the monitor applies this outcome flag together with the other active status dimensions.

STAT_ERROR Options:

·        X — Restrict the extract to rows where this send or processing state is active for the object.

·        Empty or initial — Do not use this flag as a filter dimension.

STAT_OK ('X' - Sent)

Optional send-state selector: when set, the monitor applies this outcome flag together with the other active status dimensions.

STAT_OK Options:

·        X — Restrict the extract to rows where this send or processing state is active for the object.

·        Empty or initial — Do not use this flag as a filter dimension.

STAT_TRANSIT ('X' - Transmitted)

Optional send-state selector: when set, the monitor applies this outcome flag together with the other active status dimensions.

STAT_TRANSIT Options:

·        X — Restrict the extract to rows where this send or processing state is active for the object.

·        Empty or initial — Do not use this flag as a filter dimension.

STAT_WAIT ('X' - Waiting)

Optional send-state selector: when set, the monitor applies this outcome flag together with the other active status dimensions.

STAT_WAIT Options:

·        X — Restrict the extract to rows where this send or processing state is active for the object.

·        Empty or initial — Do not use this flag as a filter dimension.

STATE_COLOR (State Color)

Filters lines by the derived color bucket used for severity-style triage in the monitor framework.

STATE_COLOR Options:

·        R — Red (error or failed-style outcomes).

·        G — Green (successful outcomes).

·        Y — Yellow (warning or in-process outcomes).

·        Additional literals may exist where the framework extends the palette for neutral states.

STATUS (status of the sent object)

Restricts the extract to the operational status values you configure for this EI's object type.

STATUS Options:

·        Use status domain values defined for the underlying SAP object (see data element or domain in the system).

·        Code applies STATUS as a filter; literals are environment-specific.

USERNAM (Sender)

After data is read, lines are removed unless sender on USERNAM still satisfies the active multivalued selection.


### Parameter Relationships

How parameter combinations work together

Sender identity, address type, and language selections narrow which send objects enter the working set before state, color, and status filters are applied. Duration-related inputs shape how long objects have remained in their current state relative to the evaluation moment, while lookback shapes how far back the initial read reaches when no explicit monitoring dates are supplied. The count threshold then decides whether the summarized volume is high enough to surface an alert after the detail function has produced its candidate list.

State flags are evaluated together with the generic status and color selectors so that operations can target, for example, waiting or error bands without contradicting the broader status list. Time-zone handling applies consistently to timestamp comparisons used with duration logic. When all dimensions are left wide, the monitor still applies framework defaults for lookback and unit so that the run remains bounded and comparable from one execution to the next.


### Default Values

·        BACKDAYS - initial - treated as 1 by code (one day of lookback when building the default monitoring date window from the evaluation clock).

·        DURATION - initial - treated as unset by code (the duration interval filter does not remove rows until a populated duration range is supplied).

·        DURATION_UNIT - initial - treated as M by code (minutes as the unit for elapsed-time calculation when the parameter remains blank at read time).

Note: Default handling for lookback and duration is implemented in the called detail routine; this count wrapper forwards selection tables and reads the alert flag from that routine before comparing the resulting table size to the configured count limit.


### Practical Example of Parameter Configuration

Use Case 1: Tight operational slice for a busy hour

Purpose: Focus on one sender during a short window while still requiring a meaningful volume before alerting.

BACKDAYS = 1
 DURATION = 120
 DURATION_UNIT = M
 USERNAM = BATCH_USER
 SOST_CNT = 500



Use Case 2: Error-heavy corridor with color emphasis

Purpose: Highlight when many red-state communications accumulate for monitored address types.

BACKDAYS = 3
 STATE_COLOR = R
 STAT_ERROR = X
 SNDART = INT
 STATUS = ERR
 SOST_CNT = 50
 MANAGE_IN_UTC = X



Use Case 3: Broad discovery with higher threshold

Purpose: Catch only very large backlogs during month-end when traffic is expected to rise.

BACKDAYS = 7
 DURATION = 2880
 DURATION_UNIT = M
 STAT_WAIT = X
 STAT_TRANSIT = X
 SNDTP = BUP
 SNDNO = 10000001
 LANGU = E
 SOST_CNT = 2000




## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
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
