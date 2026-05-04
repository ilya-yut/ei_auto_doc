# Exception Indicator: Work Process Monitoring (SW_01_01_SM50)


## General Overview

This Exception Indicator (EI) provides comprehensive monitoring and analysis of SAP Work Processes to ensure optimal system performance, resource utilization, and application server stability. Work Process monitoring is essential for maintaining system health as work processes are the fundamental execution units that handle user requests, background jobs, and system operations in SAP environments.

Work Processes are the core execution threads within SAP application servers that process user dialogues, execute background jobs, handle RFC calls, and manage system operations. Each work process has specific characteristics including type (Dialog, Background, Update, etc.), status, and resource consumption patterns. Effective monitoring of work processes enables identification of performance bottlenecks, resource contention, and system stability issues before they impact business operations.

This Exception Indicator provides detailed work process monitoring capabilities that enable:

Work Process type analysis to monitor different process categories (Dialog, Background, Update, Enqueue, Message, Gateway, Spool) and their resource utilization patterns


Process status tracking to identify stuck, waiting, or failed work processes that may impact system performance and user experience


Duration-based monitoring for detecting long-running processes that may indicate performance problems or resource contention issues


Action and operation analysis to understand current work process activities and identify patterns in system workload distribution


Dump analysis for tracking work process failures and system errors that require immediate attention and resolution


Resource consumption assessment for identifying work processes consuming excessive system resources and impacting overall performance


The monitoring solution analyzes work process statistics similar to data available through the SM50 (Work Process Overview) and SM66 (System-wide Work Process Overview) transactions, providing enhanced filtering capabilities to focus on specific process types, states, users, and time periods. This enables targeted analysis of work process performance characteristics and identification of system-related issues.

This Exception Indicator checks whether SAP work processes are functioning efficiently and identifies potential issues that may impact system performance, user experience, and business process execution.


## Problem Description

Poor work process performance and processing failures indicate system resource and performance issues causing:

System Performance and Resource Problems

Stuck or long-running work processes consuming system resources and preventing efficient request processing


Work process dumps indicating system errors, memory issues, or application failures affecting system stability


Resource contention from excessive work process workload causing system performance degradation and response time issues


Work process type imbalances leading to inefficient resource allocation and processing bottlenecks in specific operation categories


User Experience and Application Issues

Dialog work process problems causing user session delays, timeouts, and poor response times affecting productivity


Background job processing failures from work process issues disrupting automated business operations and scheduled tasks


Update work process problems causing database update failures and data consistency issues in business transactions


RFC processing delays from communication work process issues affecting system integration and external connectivity


System Stability and Availability

Frequent work process restarts indicating underlying system instability and configuration problems


Memory-related work process failures compromising system reliability and requiring immediate intervention


Work process queue saturation leading to system overload conditions and potential service interruptions


Process execution failures causing transaction rollbacks and business process disruptions affecting operational continuity


Business Impact

SLA violations from work process performance issues impacting service level commitments and user satisfaction


Business process interruptions from unreliable work process execution affecting operational efficiency and productivity


Data processing delays causing bottlenecks in time-critical business operations and reporting activities


System unavailability from work process failures compromising business continuity and operational resilience



## Suggested Resolution

Immediate Response

Investigate stuck and long-running work processes using SM50/SM66 transactions for detailed analysis and resolution


Check system resource utilization (CPU, memory) during work process activity periods for capacity assessment


Review work process dump logs and system messages for root cause identification and error pattern analysis


Analyze work process distribution and workload patterns across different process types for optimization opportunities


System Assessment

Monitor work process performance trends and processing duration patterns for proactive capacity planning


Evaluate system sizing and work process configuration parameters for optimal resource allocation


Check application server configuration and work process instance settings for performance tuning


Analyze work process activity patterns by user, transaction, and time periods for workload optimization


Corrective Actions

Optimize work process parameters and system configurations for improved performance and resource efficiency


Implement work process monitoring and alerting procedures for early issue detection and prevention


Establish proactive capacity management based on work process utilization analysis and business growth projections


Plan system capacity upgrades based on work process volume analysis and performance requirements



## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | BTCJOBCNT | Job no. | CHAR | 8 | 0 | BTCJOBCNT | CHAR8 |
| 2 | BTCJOBNAME | Job name | CHAR | 32 | 0 | BTCJOB | CHAR32 |
| 3 | CNT | Number of Result Records |  | 0 | 0 |  |  |
| 4 | CUAPROGRAM | CUA program name | CHAR | 40 | 0 | WPTOTINFCP | PROGNAME |
| 5 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 6 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 7 | DYNPRONR | Screen number or transaction | CHAR | 4 | 0 | STA_DYNPNO | STAT_DYNPR |
| 8 | FCODE | FCode | CHAR | 4 | 0 | WPTOTINFFC | SYCHAR04 |
| 9 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 10 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 11 | MEMSUM | Extended memory | DEC | 20 | 0 | MEMSUM | ABAP_MSIZE |
| 12 | PRIVSUM | Heap (private) Memory | DEC | 20 | 0 | PRIVSUM | ABAP_MSIZE |
| 13 | STARTDATE | Start date | DATS | 8 | 0 | WPTOTINFSD | SYDATS |
| 14 | STARTTIME | Start time | TIMS | 6 | 0 | WPTOTINFST | SYTIME |
| 15 | TCODE | Transaction code | CHAR | 20 | 0 | STA_TCODE4 | TCODE |
| 16 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 17 | WP_ACTION | Action | CHAR | 25 | 0 | WPACTION | WPACTION |
| 18 | WP_BNAME | User Names | CHAR | 12 | 0 | WPBNAME | UBNAME |
| 19 | WP_CPU | CPU | CHAR | 8 | 0 | WPCPU | WPCPU |
| 20 | WP_DUMPS | Dumps | CHAR | 2 | 0 | WPDUMPS | WPDUMPS |
| 21 | WP_ELTIME | Runtime | CHAR | 6 | 0 | WPELZEIT | WPELZEIT |
| 22 | WP_IACTION | Action | INT1 | 3 | 0 | WPIACTION | INT1 |
| 23 | WP_INDEX | WP index | INT4 | 10 | 0 | WPINDEX | WPINDEX |
| 24 | WP_IRESTRT | Restart | INT1 | 3 | 0 | WPIRESTART | WPIRESTART |
| 25 | WP_ISTATUS | Status | INT1 | 3 | 0 | WPISTATUS | WPISTATUS |
| 26 | WP_ITYPE | Process Category | INT1 | 3 | 0 | WPITYPE | WPITYPE |
| 27 | WP_IWAIT | Reason for wait | INT1 | 3 | 0 | WPIWAITING | WPIWAITING |
| 28 | WP_MANDT | Client | CLNT | 3 | 0 | MANDT | MANDT |
| 29 | WP_NO | Work Process Number | CHAR | 2 | 0 | WPNO | WPNO |
| 30 | WP_PID | Work process PID | CHAR | 8 | 0 | WPPID | WPPID |
| 31 | WP_REPORT | Report | CHAR | 40 | 0 | WPREPORT | WPREPORT |
| 32 | WP_RESTART | Restart | CHAR | 4 | 0 | WPRESTART | WPRESTART |
| 33 | WP_SEM | Semaphore no. | CHAR | 2 | 0 | WPSEM | WPSEM |
| 34 | WP_SEMSTAT | Process Category | INT1 | 3 | 0 | WPITYPE | WPITYPE |
| 35 | WP_SERVER | Server Name | CHAR | 20 | 0 | MSNAME | MSNAME |
| 36 | WP_STATUS | Status | CHAR | 7 | 0 | WPSTATUS | WPSTATUS |
| 37 | WP_TABLE | Table | CHAR | 30 | 0 | WPTABLE | CHAR30 |
| 38 | WP_TYP | Process type | CHAR | 3 | 0 | WPTYP | WPTYP |
| 39 | WP_WAITINF | Wait info | CHAR | 40 | 0 | WPWAITINF | WPWAITINF |
| 40 | WP_WAITING | waiting for | CHAR | 5 | 0 | WPWAITING | WPWAITING |
| 41 | WP_WAITTIM | waiting since | CHAR | 8 | 0 | WPWAITTIME | UDTIME |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 41 parameters listed in the Parameters Reference Table when tuning this EI; each influences which work process activity lines are retrieved from the total-activity snapshot, aged, and checked against the optional result-count band.

BTCJOBCNT (Job no.)

When combined with cloud-destination discipline, job no. on BTCJOBCNT keeps both breadth and depth of the extract intentional.

BTCJOBNAME (Job name)

Mirrors how administrators slice work-process lists: job name (BTCJOBNAME) is one lever that shapes which rows are comparable run over run.

CNT (Number of Result Records)

Prevents accidental global scans when number of result records (CNT) is meant to stay within a controlled application slice.

CUAPROGRAM (CUA program name)

Captures edge cases where cua program name (CUAPROGRAM) must be non-default to reproduce a customer-specific monitoring scenario.

DURATION (Duration In Time Units)

Interprets duration in time units as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on DURATION.

DURATION_UNIT (Duration Unit)

Unit for elapsed time between each line’s start date and time and the evaluation clock used after the snapshot is built.

DURATION_UNIT Options:

·        H — Hours.

·        M — Minutes (preset in code before the selection read when not overridden).

·        D — Days.

·        F — Full-day style counting where applicable to the duration helper.

DYNPRONR (Screen number or transaction)

Improves readability of exported lists because screen number or transaction (DYNPRONR) columns stay aligned with the configured filter intent.

FCODE (FCode)

Documents expected operator behavior—fcode on FCODE should be set when that dimension is part of the control objective.

LANGU (Language for texts)

Works downstream of the initial snapshot read so language for texts on LANGU still participates in row-level deletion rules.

MANAGE_IN_UTC ('X' - Manage in UTC)

Controls whether the monitor normalizes evaluation timestamps to UTC or keeps local application-server time context.

MANAGE_IN_UTC Options:

·        X — Manage timing comparisons in UTC for consistent cross-time-zone batch results.

·        Empty or blank — Use local-time handling consistent with the application server clock context.

MEMSUM (Extended memory)

Helps monitoring stay readable by requiring extended memory (MEMSUM) to match organizational or technical selectors when set.

PRIVSUM (Heap (private) Memory)

Separates cross-server noise from in-scope work when heap (private) memory on PRIVSUM correlates with host or client attributes.

STARTDATE (Start date)

Supports escalation where start date on STARTDATE signals ownership for follow-up between Basis and application teams.

STARTTIME (Start time)

For operations, start time on STARTTIME indicates whether a line belongs in the current monitoring pass versus historical noise.

TCODE (Transaction code)

Improves readability of exported lists because transaction code (TCODE) columns stay aligned with the configured filter intent.

USER_FLD (Dynamic Recipient User Field)

The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.

How DRL Works:

When USER_FLD is specified, the system extracts values from that field in the monitoring result set

These extracted values are then used as recipient addresses for alert notifications

This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored

The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users

WP_ACTION (Action)

Uses action from the work process context so only records with WP_ACTION inside declared values proceed to elapsed-time checks.

WP_BNAME (User Names)

Explains why two monitoring passes differ: only the pass with stricter user names on WP_BNAME surfaces the disputed lines.

WP_CPU (CPU)

When left open per framework rules, WP_CPU does not restrict cpu; when set, only matching rows remain.

WP_DUMPS (Dumps)

Helps distinguish dialog versus background work when dumps on WP_DUMPS correlates with process-type attributes.

WP_ELTIME (Runtime)

Separates cross-server noise from in-scope work when runtime on WP_ELTIME correlates with host or client attributes.

WP_IACTION (Action)

Helps distinguish dialog versus background work when action on WP_IACTION correlates with process-type attributes.

WP_INDEX (WP index)

After the activity snapshot is read, rows are dropped unless wp index on WP_INDEX still satisfies the active multivalued selection.

WP_IRESTRT (Restart)

Interprets restart as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on WP_IRESTRT.

WP_ISTATUS (Status)

Improves readability of exported lists because status (WP_ISTATUS) columns stay aligned with the configured filter intent.

WP_ITYPE (Process Category)

Keeps the extract aligned with process category when WP_ITYPE is restricted for this work-process overview pass.

WP_IWAIT (Reason for wait)

Improves readability of exported lists because reason for wait (WP_IWAIT) columns stay aligned with the configured filter intent.

WP_MANDT (Client)

Helps distinguish dialog versus background work when client on WP_MANDT correlates with process-type attributes.

WP_NO (Work Process Number)

Gives auditors traceable criteria because work process number on WP_NO is applied consistently before any alert flag is raised.

WP_PID (Work process PID)

Captures edge cases where work process pid (WP_PID) must be non-default to reproduce a customer-specific monitoring scenario.

WP_REPORT (Report)

Guards against oversized extracts when report on WP_REPORT is narrowed together with client, user, or server filters.

WP_RESTART (Restart)

Interprets restart as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on WP_RESTART.

WP_SEM (Semaphore no.)

Allows phased rollout: first widen WP_SEM for semaphore no., then tighten duration thresholds once baseline noise is understood.

WP_SEMSTAT (Process Category)

Aligns exception volume with the chosen scope by testing process category via WP_SEMSTAT before alert evaluation.

WP_SERVER (Server Name)

Works downstream of the initial snapshot read so server name on WP_SERVER still participates in row-level deletion rules.

WP_STATUS (Status)

Guards against oversized extracts when status on WP_STATUS is narrowed together with client, user, or server filters.

WP_TABLE (Table)

Combines with process-type and status filters so table on WP_TABLE refines which application-server work processes remain visible.

WP_TYP (Process type)

Captures edge cases where process type (WP_TYP) must be non-default to reproduce a customer-specific monitoring scenario.

WP_WAITINF (Wait info)

Treats wait info as a discriminator between similar work-process rows that would otherwise look identical in a raw extract.

WP_WAITING (waiting for)

Explains why two monitoring passes differ: only the pass with stricter waiting for on WP_WAITING surfaces the disputed lines.

WP_WAITTIM (waiting since)

Gives auditors traceable criteria because waiting since on WP_WAITTIM is applied consistently before any alert flag is raised.


### Parameter Relationships

How parameter combinations work together

MANAGE_IN_UTC should stay consistent with how operators read start timestamps on the snapshot so elapsed comparisons and cross-system reviews use the same clock interpretation.

DURATION and DURATION_UNIT apply only after each retained line has a non-initial start date; they define the age window used with the configured unit before lines are dropped for being outside the duration multivalued selection.

CNT works as a final gate on how many lines survive all prior filters: when populated, the monitor clears the result set unless the row count falls inside the declared count band, so it should be tuned together with the dimensional filters that define what “too many processes” means in your organization.

SW_DEST, when set, routes execution through the alternate path so client, program, and status filters are evaluated in that destination’s context before duration and count logic run.

WP_* and related selectors (client, user, program, transaction, process type, status, server, batch job fields, and similar) should be composed as one logical slice so the headline duration and count tests describe the same population your administrators would defend in an incident.


### Default Values

·        DURATION - initial - treated as empty by code (no duration range; age filter off).

·        DURATION_UNIT - initial - treated as M by code (minutes).


### Practical Example of Parameter Configuration

Use Case 1: Dialog-heavy client slice

Purpose: Watch one productive client and cap how many matching dialog-style processes appear in one pass.

WP_MANDT = 100
 WP_TYP = DIA
 CNT = 5 - 999999
 MANAGE_IN_UTC = X



Use Case 2: Full-day age window

Purpose: Flag processes whose step start is older than thirty full days using the full-day unit style.

DURATION = 30
 DURATION_UNIT = F
 WP_SERVER = APPHOST01



Use Case 3: Program and transaction triage

Purpose: Narrow to a known report driver and transaction while requiring at least two lines before alerting.

WP_REPORT = Z_BATCH_DRIVER
 TCODE = SM37
 CNT = 2 - 999999



Use Case 4: Background job context

Purpose: Combine batch job name with restart and dump indicators for overnight operations.

BTCJOBNAME = FIN_CLOSE_JOB
 WP_DUMPS = 01
 WP_RESTART = YES
 WP_MANDT = 200




## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_01_SM66 | BTCJOBCNT | Job ID | CHAR(8) | BTCJOBCNT |
| /SKN/S_SW_01_01_SM66 | BTCJOBNAME | Background job name | CHAR(32) | BTCJOB |
| /SKN/S_SW_01_01_SM66 | CUAPROGRAM | CUA program name | CHAR(40) | WPTOTINFCP |
| /SKN/S_SW_01_01_SM66 | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_SM66 | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_SM66 | DYNPRONR | Screen number or transaction name | CHAR(4) | STA_DYNPNO |
| /SKN/S_SW_01_01_SM66 | FCODE | Function code | CHAR(4) | WPTOTINFFC |
| /SKN/S_SW_01_01_SM66 | MEMSUM | Extended memory | DEC(20) | MEMSUM |
| /SKN/S_SW_01_01_SM66 | PRIVSUM | Heap (private) Memory | DEC(20) | PRIVSUM |
| /SKN/S_SW_01_01_SM66 | STARTDATE | Start date for dialog step | DATS(8) | WPTOTINFSD |
| /SKN/S_SW_01_01_SM66 | STARTTIME | Start time for dialog step | TIMS(6) | WPTOTINFST |
| /SKN/S_SW_01_01_SM66 | TCODE | Transaction code | CHAR(20) | STA_TCODE4 |
| /SKN/S_SW_01_01_SM66 | WP_ACTION | Current Activity of the Work Process | CHAR(25) | WPACTION |
| /SKN/S_SW_01_01_SM66 | WP_BNAME | User Currently Served | CHAR(12) | WPBNAME |
| /SKN/S_SW_01_01_SM66 | WP_CPU | CPU | CHAR(8) | WPCPU |
| /SKN/S_SW_01_01_SM66 | WP_DUMPS | Number of Dumps of the Work Process | CHAR(2) | WPDUMPS |
| /SKN/S_SW_01_01_SM66 | WP_ELTIME | Previous Runtime of Request (elapsed) | CHAR(6) | WPELZEIT |
| /SKN/S_SW_01_01_SM66 | WP_IACTION | Current activity of the work process | INT1(3) | WPIACTION |
| /SKN/S_SW_01_01_SM66 | WP_INDEX | Work Process Number | INT4(10) | WPINDEX |
| /SKN/S_SW_01_01_SM66 | WP_IRESTRT | Restart After Error | INT1(3) | WPIRESTART |
| /SKN/S_SW_01_01_SM66 | WP_ISTATUS | Work process status | INT1(3) | WPISTATUS |
| /SKN/S_SW_01_01_SM66 | WP_ITYPE | Internal work process type | INT1(3) | WPITYPE |
| /SKN/S_SW_01_01_SM66 | WP_IWAIT | Reason for wait | INT1(3) | WPIWAITING |
| /SKN/S_SW_01_01_SM66 | WP_MANDT | Client | CLNT(3) | MANDT |
| /SKN/S_SW_01_01_SM66 | WP_NO | (Deprecated) Use WP_UNDEX | CHAR(2) | WPNO |
| /SKN/S_SW_01_01_SM66 | WP_PID | Process Identification | CHAR(8) | WPPID |
| /SKN/S_SW_01_01_SM66 | WP_REPORT | Report now running | CHAR(40) | WPREPORT |
| /SKN/S_SW_01_01_SM66 | WP_RESTART | Restart work process after dump? | CHAR(4) | WPRESTART |
| /SKN/S_SW_01_01_SM66 | WP_SEM | Semaphore that the work process is waiting for | CHAR(2) | WPSEM |
| /SKN/S_SW_01_01_SM66 | WP_SEMSTAT | Internal work process type | INT1(3) | WPITYPE |
| /SKN/S_SW_01_01_SM66 | WP_SERVER | Server Name | CHAR(20) | MSNAME |
| /SKN/S_SW_01_01_SM66 | WP_STATUS | Work process status | CHAR(7) | WPSTATUS |
| /SKN/S_SW_01_01_SM66 | WP_TABLE | DB table last accessed by the work process | CHAR(30) | WPTABLE |
| /SKN/S_SW_01_01_SM66 | WP_TYP | Type | CHAR(3) | WPTYP |
| /SKN/S_SW_01_01_SM66 | WP_WAITINF | Additional information about the reason for waiting | CHAR(40) | WPWAITINF |
| /SKN/S_SW_01_01_SM66 | WP_WAITING | Why Work Process is Waiting | CHAR(5) | WPWAITING |
| /SKN/S_SW_01_01_SM66 | WP_WAITTIM | Time when waiting started | CHAR(8) | WPWAITTIME |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_01_SM66.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SM66 OPTIONAL
*"----------------------------------------------------------------------
DATA_MULTY: DURATION   /SKN/E_SW_DURATION,
            WP_TYP     WPTYP,
            WP_ISTATUS  WPISTATUS,
            WP_WAITING WPWAITING,
            WP_IRESTRT WPIRESTART,
            WP_RESTART WPRESTART,
            WP_DUMPS   WPDUMPS,
            WP_CPU     WPCPU,
            WP_ELTIME  WPELZEIT,
            WP_MANDT   MANDT,
            WP_REPORT  WPREPORT,
            WP_ACTION  WPACTION,
            WP_TABLE   WPTABLE,
            WP_SERVER  MSNAME,
            WP_WAITINF WPWAITINF,
            WP_INDEX   WPINDEX,
            STARTDATE  WPTOTINFSD,
            STARTTIME  WPTOTINFST,
            DYNPRONR   STA_DYNPNO,
            TCODE      STA_TCODE4,
            CUAPROGRAM WPTOTINFCP,
            FCODE      WPTOTINFFC,
            BTCJOBNAME BTCJOB,
            BTCJOBCNT  BTCJOBCNT.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             LANGU LANGU,
             NUMBER SY-TABIX,
             SUBRC SY-SUBRC,
             MANAGE_IN_UTC CHAR1.
DATA : LV_CNT TYPE I.
DATA_MULTY: CNT /SKN/E_SW_CNT.
SELECT_MULTY: CNT.
DATA: LS_WP_TOTAL_INFO TYPE WPTOTLINFO,
      LT_WP_TOTAL_INFO LIKE TABLE OF LS_WP_TOTAL_INFO,
      LS_COMMUNICATION_ERRORS TYPE WPTOTLICER,
      LT_COMMUNICATION_ERRORS LIKE TABLE OF LS_COMMUNICATION_ERRORS,
      IS_USED_WP TYPE WPTOTLIUWP,
      IT_USED_WP LIKE TABLE OF IS_USED_WP.
DATA : SY_TABIX LIKE SY-TABIX .
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
DATA : IS_OUT(1) TYPE C.
DATA: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
*-- Fill Selection Option Tables
SELECT_MULTY: DURATION,
            WP_TYP,
            WP_ISTATUS,
            WP_WAITING,
            WP_IRESTRT,
            WP_RESTART,
            WP_DUMPS,
            WP_CPU,
            WP_ELTIME,
            WP_MANDT,
            WP_REPORT,
            WP_ACTION,
            WP_TABLE,
            WP_SERVER,
            WP_WAITINF,
            WP_INDEX,
            STARTDATE,
            STARTTIME,
            DYNPRONR,
            TCODE,
            CUAPROGRAM,
            FCODE,
            BTCJOBNAME,
            BTCJOBCNT.
 LV_DURATION_UNIT = 'M'.
 SELECT_SINGLE: DURATION_UNIT,
                MANAGE_IN_UTC,
                LANGU.
 SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
 TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
 PROCESS_ICON = ICON_DELETE.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_SM66'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  CALL FUNCTION 'STUM_WP_TOTAL_ACTIVITY'
    EXPORTING
*     GET_ONLY_RESTRICTED_DATA       = ' '
      WITH_CPU                       = '1'
*     SHOW_STATUS_ON_GUI             = ' '
      GET_NO_DB_LOCK_INFO            = 'X'
    TABLES
      WP_TOTAL_INFO                  = LT_WP_TOTAL_INFO
      COMMUNICATION_ERRORS           = LT_COMMUNICATION_ERRORS
      USED_WP                        = IT_USED_WP.
  DELETE LT_WP_TOTAL_INFO WHERE WP_TYP NOT IN R_WP_TYP.
  DELETE LT_WP_TOTAL_INFO WHERE WP_ISTATUS NOT IN R_WP_ISTATUS.
  DELETE LT_WP_TOTAL_INFO WHERE WP_WAITING NOT IN R_WP_WAITING.
  DELETE LT_WP_TOTAL_INFO WHERE WP_IRESTRT NOT IN R_WP_IRESTRT.
  DELETE LT_WP_TOTAL_INFO WHERE WP_RESTART NOT IN R_WP_RESTART.
  DELETE LT_WP_TOTAL_INFO WHERE WP_DUMPS NOT IN R_WP_DUMPS.
  DELETE LT_WP_TOTAL_INFO WHERE WP_CPU NOT IN R_WP_CPU.
  DELETE LT_WP_TOTAL_INFO WHERE WP_ELTIME NOT IN R_WP_ELTIME.
  DELETE LT_WP_TOTAL_INFO WHERE WP_MANDT NOT IN R_WP_MANDT.
  DELETE LT_WP_TOTAL_INFO WHERE WP_REPORT NOT IN R_WP_REPORT.
  DELETE LT_WP_TOTAL_INFO WHERE WP_ACTION NOT IN R_WP_ACTION.
  DELETE LT_WP_TOTAL_INFO WHERE WP_TABLE NOT IN R_WP_TABLE.
  DELETE LT_WP_TOTAL_INFO WHERE WP_SERVER NOT IN R_WP_SERVER.
  DELETE LT_WP_TOTAL_INFO WHERE WP_WAITINF NOT IN R_WP_WAITINF.
  DELETE LT_WP_TOTAL_INFO WHERE WP_INDEX NOT IN R_WP_INDEX.
  DELETE LT_WP_TOTAL_INFO WHERE STARTDATE NOT IN R_STARTDATE.
  DELETE LT_WP_TOTAL_INFO WHERE STARTTIME NOT IN R_STARTTIME.
  DELETE LT_WP_TOTAL_INFO WHERE DYNPRONR NOT IN R_DYNPRONR.
  DELETE LT_WP_TOTAL_INFO WHERE TCODE NOT IN R_TCODE.
  DELETE LT_WP_TOTAL_INFO WHERE CUAPROGRAM NOT IN R_CUAPROGRAM.
  DELETE LT_WP_TOTAL_INFO WHERE FCODE NOT IN R_FCODE.
  DELETE LT_WP_TOTAL_INFO WHERE BTCJOBNAME NOT IN R_BTCJOBNAME.
  DELETE LT_WP_TOTAL_INFO WHERE BTCJOBCNT NOT IN R_BTCJOBCNT.
  LOOP AT LT_WP_TOTAL_INFO INTO LS_WP_TOTAL_INFO.
    CLEAR T_DATA.
    MOVE-CORRESPONDING LS_WP_TOTAL_INFO TO T_DATA.
    APPEND T_DATA.
  ENDLOOP.
*   loop at t_data.
*     sy_tabix = sy-tabix.
*      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*        EXPORTING
*          D_FROM            = t_data-STARTDATE
*          T_FROM            = t_data-STARTTIME
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
*        "t_data-PROCESS_ICON = PROCESS_ICON.
*        modify t_data index sy_tabix.
*      ENDIF.
*    endloop.
*
*  delete t_data where DURATION_M not in R_DURATION_M.
*  delete t_data where DURATION_H not in R_DURATION_H.
*-- Fill Duration Value
 SY_DATLO = SY-DATUM.   "--- System Date/Time
 SY_TIMLO = SY-UZEIT.
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
   IF T_DATA-STARTDATE IS INITIAL.
     CONTINUE.
   ENDIF.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-STARTDATE
          T_FROM            = T_DATA-STARTTIME
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
 IF R_CNT[] IS NOT INITIAL.
   READ TABLE T_DATA INDEX 1.
   LV_CNT = SY-TFILL.
   IF LV_CNT NOT IN R_CNT.
     REFRESH T_DATA.
   ENDIF.
 ENDIF.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
