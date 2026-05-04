# Exception Indicator: Server CPU Performance Monitoring (SW_01_20_SRV_CPU)


## General Overview

This Exception Indicator (EI) provides comprehensive CPU performance monitoring and analysis across SAP system landscapes to ensure optimal system performance and prevent CPU bottlenecks that could cause critical performance degradation. CPU monitoring is fundamental for maintaining SAP system responsiveness, throughput, and user experience.

The Server CPU Performance Monitoring solution monitors CPU utilization and load metrics across all application servers in a distributed SAP landscape, providing real-time visibility into CPU consumption patterns and system load characteristics. When CPU utilization exceeds optimal thresholds, it can lead to response time degradation, transaction timeouts, user session failures, and overall system performance issues.

This Exception Indicator provides advanced CPU monitoring capabilities that enable:

Multi-server monitoring across distributed SAP landscapes with centralized visibility

Comprehensive CPU metrics including usage percentages, load averages, and system/user time distribution

Real-time performance analysis with current utilization and load trend calculations

Cross-platform compatibility supporting various operating systems and hardware configurations

Enhanced error handling for network connectivity issues and collector service failures

Historical tracking with timestamp recording for performance trend analysis and capacity planning

The monitoring solution leverages SAP's built-in CPU monitoring functions and provides enhanced analytical capabilities including CPU usage calculations, load average processing, and comprehensive server list management for enterprise-scale CPU performance monitoring.

This Exception Indicator ensures that SAP systems maintain optimal CPU performance across all critical application servers and identifies potential performance bottlenecks before they impact business operations.


## Problem Description

High CPU utilization and inadequate performance monitoring indicate critical system resource issues causing:

System Performance Risks

Severe response time degradation when CPU utilization consistently exceeds 80-90% capacity

Transaction timeouts and user session failures due to insufficient CPU resources for processing

Batch job delays and failures affecting critical business processes and reporting cycles

System instability and potential crashes under sustained high CPU load conditions

User Experience Impact

Slow application response times affecting user productivity and business process efficiency

Login failures and session timeouts during peak CPU utilization periods

Dialog step timeouts causing transaction interruptions and data loss scenarios

Poor system responsiveness impacting user satisfaction and operational efficiency

Business Process Disruption

Interface processing delays affecting real-time data exchange with external systems

Background job queue congestion from CPU resource constraints impacting automation

Report generation failures and delays affecting business decision-making processes

Integration failures with external systems due to processing timeouts and resource exhaustion

Capacity Planning Issues

Lack of visibility into CPU consumption patterns preventing proactive capacity management

Unpredictable performance degradation scenarios causing emergency system interventions

Inadequate load balancing across application servers leading to resource inefficiency

Poor utilization tracking preventing optimization of system configurations and workload distribution


## Suggested Resolution

Immediate Response

Investigate servers showing consistently high CPU usage (>80%) for immediate load balancing opportunities

Check CPU load averages (1, 5, 15 minute intervals) for sustained high load patterns requiring attention

Review active work processes and identify resource-intensive operations for optimization or rescheduling

Analyze user and system CPU time distribution to identify optimization opportunities

Performance Assessment

Monitor CPU utilization trends across all application servers for workload optimization

Evaluate current load distribution against server capacity and performance baselines

Check system and user CPU time ratios for process efficiency analysis and tuning opportunities

Analyze interrupt and context switch rates for system-level performance optimization

Corrective Actions

Implement workload balancing across application servers to distribute CPU load effectively

Optimize resource-intensive batch jobs and background processes for improved CPU efficiency

Configure automatic load balancing and failover mechanisms for peak load management

Schedule CPU-intensive operations during off-peak hours to maintain optimal system performance

Preventive Measures

Establish regular CPU utilization reviews and performance baseline monitoring procedures

Implement automated monitoring for all critical servers with appropriate performance thresholds

Create documentation and procedures for performance tuning and emergency load management

Plan system capacity upgrades based on utilization analysis and business growth projections


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | CPU_USAGE | CPU Utilization | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 2 | CS_SEC | Context switches (sec) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 3 | HOST | Host Name | CHAR | 32 | 0 | MSHOST2 | MSHOST2 |
| 4 | IDLE_TOTAL | Idle (%) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 5 | IDLE_TRUE | Idle True (%) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 6 | INT_SEC | Interrupts (sec) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 7 | LOAD15_AVG | Avg processes waiting (15 min) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 8 | LOAD1_AVG | Avg processes waiting (1 min) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 9 | LOAD5_AVG | Avg processes waiting (5 min) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 10 | NBR_CPU | Number of CPUs | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 11 | SERIALNR | Serial number | INT2 | 5 | 0 | COLLSERNR | INT2 |
| 12 | SERVER | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 13 | SUBTYPE | saposcol subtype | INT2 | 5 | 0 | COLLSUBTYP | INT2 |
| 14 | SYS_TOTAL | System Utilization (%) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 15 | SYSC_SEC | System Calls (sec) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 16 | TYPE | saposcoltype | INT2 | 5 | 0 | COLLTYPE | INT2 |
| 17 | USR_TOTAL | User Utilization ( %) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 18 | WAIT_TRUE | Wait True (%) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 18 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

CPU_USAGE (CPU Utilization)

Valuable when comparing health before and after a release—hold cpu utilization on CPU_USAGE constant while varying other filters.

CS_SEC (Context switches (sec))

For distributed landscapes, context switches (sec) on CS_SEC often anchors which application server or destination appears in results.

HOST (Host Name)

Helps distinguish technical versus business attributes when host name on HOST correlates with counters or status fields.

IDLE_TOTAL (Idle (%))

When tightened, idle (%) (IDLE_TOTAL) removes rows that would otherwise dilute attention from failing or stuck cases.

IDLE_TRUE (Idle True (%))

Improves readability of exported lists because idle true (%) (IDLE_TRUE) columns stay aligned with the configured filter intent.

INT_SEC (Interrupts (sec))

Gives auditors traceable criteria because interrupts (sec) on INT_SEC is applied consistently before any alert flag is raised.

LOAD15_AVG (Avg processes waiting (15 min))

Explains why two monitoring passes differ: only the pass with stricter avg processes waiting (15 min) on LOAD15_AVG surfaces the disputed rows.

LOAD1_AVG (Avg processes waiting (1 min))

Separates cross-client noise from in-scope work when avg processes waiting (1 min) on LOAD1_AVG correlates with client or user attributes.

LOAD5_AVG (Avg processes waiting (5 min))

When tightened, avg processes waiting (5 min) (LOAD5_AVG) removes rows that would otherwise dilute attention from failing or stuck cases.

NBR_CPU (Number of CPUs)

Explains why two monitoring passes differ: only the pass with stricter number of cpus on NBR_CPU surfaces the disputed rows.

SERIALNR (Serial number)

When tightened, serial number (SERIALNR) removes rows that would otherwise dilute attention from failing or stuck cases.

SERVER (Server Name)

Gives auditors traceable criteria because server name on SERVER is applied consistently before any alert flag is raised.

SUBTYPE (saposcol subtype)

Documents expected operator behavior—saposcol subtype on SUBTYPE should be set when that dimension is part of the control objective.

SYS_TOTAL (System Utilization (%))

Supports escalation where system utilization (%) on SYS_TOTAL signals ownership for follow-up between Basis and functional teams.

SYSC_SEC (System Calls (sec))

After data is read, lines are removed unless system calls (sec) on SYSC_SEC still satisfies the active multivalued selection.

TYPE (saposcoltype)

Prevents accidental global scans when saposcoltype (TYPE) is meant to stay within a controlled application slice.

USR_TOTAL (User Utilization ( %))

Supports escalation where user utilization ( %) on USR_TOTAL signals ownership for follow-up between Basis and functional teams.

WAIT_TRUE (Wait True (%))

Supports operational control by evaluating wait true (%) through WAIT_TRUE for each candidate record.


### Parameter Relationships

How parameter combinations work together

The CPU monitoring function provides both percentage-based and absolute

value parameters to accommodate

different monitoring scenarios and business requirements:

Percentage vs. Absolute Value Parameters

Percentage Parameters:

·       CPU_USAGE - Current CPU utilization as percentage of total available capacity

·       USR_TOTAL - User CPU time as percentage of total CPU time

·       SYS_TOTAL - System CPU time as percentage of total CPU time

·       IDLE_TOTAL - Idle CPU time as percentage of total CPU time

Absolute Value Parameters:

·       LOAD1_AVG - 1-minute load average for recent system load assessment

·       LOAD5_AVG - 5-minute load average for medium-term system load assessment

·       LOAD15_AVG - 15-minute load average for long-term system load assessment

·       NBR_CPU - Number of CPU cores available on the server

Note: Load average is unitless, representing the average number of processes running or waiting for resources. Load average should be interpreted relative to CPU count (e.g., load of 4.0 on a 4-core system = 100% utilization).

Why Both Options Exist:

The dual parameter approach provides monitoring flexibility for different business contexts. Percentage-based thresholds work well for systems of varying CPU capacities (e.g., alert when any server exceeds 85% CPU usage), while absolute value thresholds are essential for load-sensitive systems where

specific load average limits must be maintained (e.g., always keep load average below 4.0 regardless of total CPU count).


### Default Values

No default values are defined for this EI.


### Practical Example of Parameter Configuration

Use Case 1: Single production instance

Purpose: Watch one application server with tight CPU utilization and load-average ceilings.

 SERVER = PRD_ASCS01 
 HOST = prd-phx-01 
 CPU_USAGE = 85-100
 LOAD1_AVG = 8-999999
 NBR_CPU = 8



Use Case 2: User versus system imbalance

Purpose: Highlight hosts where user time dominates while idle time collapses.

 SERVER = %DEV%
 USR_TOTAL = 70-100
 IDLE_TOTAL = 0-15
 WAIT_TRUE = 5-100
 SYS_TOTAL = 10-40



Use Case 3: Kernel churn review

Purpose: Sample several OS-level counters for a weekly stability report.

 HOST = %QAS%
 INT_SEC = 5000-999999999
 SYSC_SEC = 8000-999999999
 CS_SEC = 12000-999999999
 LOAD15_AVG = 4-999999
 TYPE = 1
 SUBTYPE = 2
 SERIALNR = 0-32767



Use Case 5: Percentage based monitoring

Purpose: Alert when CPU utilization percentage exceeds 85% for performance management

 CPU_USAGE = >85

Use Case 2: Absolute value based monitoring  

Purpose: Enterprise-scale CPU monitoring using absolute load thresholds for capacity planning

 LOAD5_AVG = >2


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_01_SRV_CPU | CPU_USAGE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | CS_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | HOST | Name of Application Server | CHAR(32) | MSHOST2 |
| /SKN/S_SW_01_01_SRV_CPU | IDLE_TOTAL | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | IDLE_TRUE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | INT_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | LOAD15_AVG | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | LOAD1_AVG | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | LOAD5_AVG | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | NBR_CPU | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | SERIALNR | Serial number in saposcol result structure | INT2(5) | COLLSERNR |
| /SKN/S_SW_01_01_SRV_CPU | SERVER | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_01_SRV_CPU | SUBTYPE | Subtype in saposcol result structure | INT2(5) | COLLSUBTYP |
| /SKN/S_SW_01_01_SRV_CPU | SYSC_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | SYS_TOTAL | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | TYPE | Type in saposcol result structure | INT2(5) | COLLTYPE |
| /SKN/S_SW_01_01_SRV_CPU | USR_TOTAL | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | WAIT_TRUE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |


## ABAP Code


```abap
  FUNCTION /SKN/F_SW_01_01_SRV_CPU.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SRV_CPU OPTIONAL
*"----------------------------------------------------------------------
DATA_MULTY: DURATION /SKN/E_SW_DURATION,
            SERVER   MSNAME2,
            HOST     MSHOST2,
            NBR_CPU  INT4_DATA,
            LOAD1_AVG  INT4_DATA,
            LOAD5_AVG  INT4_DATA,
            LOAD15_AVG  INT4_DATA,
            INT_SEC  INT4_DATA,
            SYSC_SEC  INT4_DATA,
            CS_SEC  INT4_DATA,
            USR_TOTAL  INT4_DATA,
            SYS_TOTAL  INT4_DATA,
            IDLE_TOTAL  INT4_DATA,
            IDLE_TRUE  INT4_DATA,
            WAIT_TRUE  INT4_DATA,
            CPU_USAGE  INT4_DATA.
            .
DATA_SINGLE: AGGR_LEVEL CHAR1.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             LANGU LANGU,
             NUMBER SY-TABIX,
             SUBRC SY-SUBRC,
             MANAGE_IN_UTC CHAR1.
DATA: LS_SRV_LIST TYPE  MSXXLIST,
      LT_SRV_LIST LIKE TABLE OF LS_SRV_LIST.
DATA: LV_LOGICAL_DESTINATION TYPE RFCDEST.
DATA: F_CPU_ALL_READ LIKE  DEF_PAR_FU-NO_YES,
      ACTIVEFLAG LIKE  DEF_PAR_FU-ACTIVEFLAG,
      INTERVAL LIKE  DEF_PAR_FU-INTERVAL,
      OP_SYSTEM LIKE  DEF_PAR_FU-OPSYSTEM,
      DETAILSCOLL LIKE  DEF_PAR_FU-DETAILSCOL,
      DETAILSREQI LIKE  DEF_PAR_FU-DETAILSREQ,
      DETAILSMODE LIKE  DEF_PAR_FU-DETAILSMOD,
      LASTCOLLWRT LIKE  DEF_PAR_FU-LASTCOLLWR,
      LASTCOLLINT LIKE  DEF_PAR_FU-LASTCOLLIN,
      NORMCOLLINT LIKE  DEF_PAR_FU-NORMCOLLIN.
DATA: LS_CPU_ALL TYPE CPU_ALL,
      LT_CPU_ALL LIKE TABLE OF LS_CPU_ALL.
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
DATA : SY_TABIX LIKE SY-TABIX .
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
DATA: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
DATA: LV_NO_SERVER_LIST(1) TYPE C.
*-- Fill Selection Option Tables
SELECT_MULTY: DURATION,
            SERVER,
            HOST,
            NBR_CPU,
            LOAD1_AVG,
            LOAD5_AVG,
            LOAD15_AVG,
            INT_SEC,
            SYSC_SEC,
            CS_SEC,
            USR_TOTAL,
            SYS_TOTAL,
            IDLE_TOTAL,
            IDLE_TRUE,
            WAIT_TRUE,
            CPU_USAGE.
 LV_DURATION_UNIT = 'M'.
 SELECT_SINGLE: DURATION_UNIT,
                MANAGE_IN_UTC,
                LANGU.
 SELECT_SINGLE: AGGR_LEVEL.
 SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
 TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
 PROCESS_ICON = ICON_DELETE.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  REFRESH: LT_DATA.
  CLEAR LV_NO_SERVER_LIST.
* get a list of all running instances & name of own instance
  CALL FUNCTION 'TH_SERVER_LIST'
     DESTINATION LV_SW_DEST
"      exporting
"           SERVICES = 255
       TABLES
            LIST     = LT_SRV_LIST
      EXCEPTIONS
        NO_SERVER_LIST        = 1
        SYSTEM_FAILURE        = 2
        COMMUNICATION_FAILURE = 3
        OTHERS                = 9.
    IF SY-SUBRC IS NOT INITIAL .
      CASE SY-SUBRC.
        WHEN 1.  " NO_SERVER_LIST
          LV_NO_SERVER_LIST = 'X'.
        WHEN OTHERS.
         " exit.
      ENDCASE.
    ENDIF.
    IF LV_NO_SERVER_LIST IS NOT INITIAL. " Add fictive Item
      CLEAR LS_SRV_LIST.
      "ls_SRV_LIST-NAME = lv_SW_DEST.
      APPEND LS_SRV_LIST TO LT_SRV_LIST.
  ENDIF.
    LOOP AT LT_SRV_LIST INTO LS_SRV_LIST.
      LV_LOGICAL_DESTINATION = LS_SRV_LIST-NAME.
      REFRESH LT_CPU_ALL.
      CALL FUNCTION 'GET_CPU_ALL'
        DESTINATION LV_SW_DEST
        EXPORTING
          LOCAL_REMOTE                         = 'INTERN'
          LOGICAL_DESTINATION                  = LV_LOGICAL_DESTINATION
        IMPORTING
          F_CPU_ALL_READ                       = F_CPU_ALL_READ
          ACTIVEFLAG                           = ACTIVEFLAG
          INTERVAL                             = INTERVAL
          OP_SYSTEM                            = OP_SYSTEM
          DETAILSCOLL                          = DETAILSCOLL
          DETAILSREQI                          = DETAILSREQI
          DETAILSMODE                          = DETAILSMODE
          LASTCOLLWRT                          = LASTCOLLWRT
          LASTCOLLINT                          = LASTCOLLINT
          NORMCOLLINT                          = NORMCOLLINT
        TABLES
          TF_CPU_ALL                           = LT_CPU_ALL
        EXCEPTIONS
          INTERNAL_ERROR_ADRESS_FAILED         = 1
          INTERNAL_ERROR_DIFFERENT_FIELD       = 2
          INTERNAL_ERROR_NO_NEW_LINE           = 3
          COLLECTOR_NOT_RUNNING                = 4
          SHARED_MEMORY_NOT_AVAILABLE          = 5
          COLLECTOR_BUSY                       = 6
          VERSION_CONFLICT                     = 7
          NO_NETWORK_COLLECTOR_RUNNING         = 8
          SYSTEM_FAILURE                       = 9
          COMMUNICATION_FAILURE                = 10
          OTHERS                               = 11.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      LOOP AT LT_CPU_ALL INTO LS_CPU_ALL.
        CLEAR: LS_DATA.
        MOVE-CORRESPONDING LS_CPU_ALL TO LS_DATA.
        LS_DATA-SERVER = LS_SRV_LIST-NAME.
        LS_DATA-HOST   = LS_SRV_LIST-HOST.
        LS_DATA-CPU_USAGE = 100 - LS_DATA-IDLE_TOTAL. "Calcing the current cpu % usage.
        APPEND LS_DATA TO LT_DATA.
      ENDLOOP.
    ENDLOOP.
***  delete lt_wp_total_info where WP_TYP not in R_WP_TYP.
***  delete lt_wp_total_info where WP_ISTATUS not in R_WP_ISTATUS.
 T_DATA[] = LT_DATA[].
****-- Fill Duration Value
*** sy_datlo = sy-datum.   "--- System Date/Time
*** sy_timlo = sy-uzeit.
*** loop at T_DATA .
***   sy_tabix = sy-tabix.
***   T_DATA-DURATION_UNIT = lv_DURATION_UNIT.
***   if t_data-STARTDATE is initial.
***     continue.
***   endif.
***    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
***        EXPORTING
***          D_FROM            = t_data-STARTDATE
***          T_FROM            = t_data-STARTTIME
***          D_TO              = sy_datlo
***          T_TO              = sy_timlo
***          TIME_UNIT         = lv_DURATION_UNIT
***        IMPORTING
***          TIME_DIFF         = TIME_DIFF
***        EXCEPTIONS
***          WRONG_VALUE       = 1
***          OTHERS            = 2    .
***      IF SY-SUBRC = 0.
***        T_DATA-DURATION = TIME_DIFF .
***      else.
***        T_DATA-DURATION = '999999'.
***      endif.
***   modify T_DATA index sy_tabix .
*** endloop .
 DELETE T_DATA WHERE SERVER NOT IN R_SERVER.
 DELETE T_DATA WHERE HOST NOT IN R_HOST.
 DELETE T_DATA WHERE NBR_CPU NOT IN R_NBR_CPU.
 DELETE T_DATA WHERE NBR_CPU NOT IN R_NBR_CPU.
 DELETE T_DATA WHERE LOAD1_AVG NOT IN R_LOAD1_AVG.
 DELETE T_DATA WHERE LOAD5_AVG NOT IN R_LOAD5_AVG.
 DELETE T_DATA WHERE LOAD15_AVG NOT IN R_LOAD15_AVG.
 DELETE T_DATA WHERE INT_SEC NOT IN R_INT_SEC.
 DELETE T_DATA WHERE SYSC_SEC NOT IN R_SYSC_SEC.
 DELETE T_DATA WHERE CS_SEC NOT IN R_CS_SEC.
 DELETE T_DATA WHERE USR_TOTAL NOT IN R_USR_TOTAL.
 DELETE T_DATA WHERE SYS_TOTAL NOT IN R_SYS_TOTAL.
 DELETE T_DATA WHERE IDLE_TOTAL NOT IN R_IDLE_TOTAL.
 DELETE T_DATA WHERE IDLE_TRUE NOT IN R_IDLE_TRUE.
 DELETE T_DATA WHERE WAIT_TRUE NOT IN R_WAIT_TRUE.
 DELETE T_DATA WHERE CPU_USAGE NOT IN R_CPU_USAGE.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
