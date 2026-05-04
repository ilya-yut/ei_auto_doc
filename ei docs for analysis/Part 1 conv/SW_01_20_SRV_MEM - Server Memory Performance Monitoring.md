# Exception Indicator: Server Memory Performance Monitoring (SW_01_20_SRV_MEM)


## General Overview

This Exception Indicator (EI) provides comprehensive memory performance monitoring and analysis across SAP system landscapes to ensure optimal memory utilization and prevent memory exhaustion scenarios that could cause critical system failures. Memory monitoring is fundamental for maintaining SAP system stability, performance, and preventing out-of-memory conditions.

The Server Memory Performance Monitoring solution monitors physical memory, swap space utilization, and memory paging activity across all application servers in a distributed SAP landscape, providing real-time visibility into memory consumption patterns and availability. When memory utilization exceeds critical thresholds, it can lead to excessive swapping, performance degradation, system crashes, and complete service interruption.

This Exception Indicator provides advanced memory monitoring capabilities that enable:

Multi-server monitoring across distributed SAP landscapes with centralized visibility

Comprehensive memory metrics including physical memory, swap space, and paging activity analysis

Real-time utilization analysis with current consumption and available capacity calculations

Cross-platform compatibility supporting various operating systems and memory configurations

Enhanced error handling for network connectivity issues and collector service failures

Historical tracking with timestamp recording for memory trend analysis and capacity planning

The monitoring solution leverages SAP's built-in memory monitoring functions and provides enhanced analytical capabilities including memory usage calculations, swap utilization processing, and comprehensive server list management for enterprise-scale memory performance monitoring.

This Exception Indicator ensures that SAP systems maintain optimal memory performance across all critical application servers and identifies potential memory bottlenecks before they impact business operations.


## Problem Description

High memory utilization and inadequate memory monitoring indicate critical system resource issues causing:

System Stability Risks

System crashes and out-of-memory errors when physical memory reaches critical depletion levels

Excessive swap space utilization causing severe performance degradation and system instability

Memory allocation failures preventing new processes and user sessions from starting successfully

Database buffer cache inefficiency due to insufficient physical memory for optimal data caching

Performance Degradation Issues

Severe response time slowdowns when systems rely heavily on swap space for memory operations

Increased disk I/O from memory paging activities affecting overall system throughput

Work process failures and timeouts due to insufficient memory for transaction processing

Application server performance issues from memory pressure and resource contention

Business Process Impact

Transaction failures and user session terminations due to memory allocation errors

Batch job failures affecting critical business processes when memory resources become exhausted

Interface processing delays from memory-constrained operations affecting real-time data exchange

Report generation failures and incomplete processing due to insufficient memory allocation

Capacity Planning Issues

Lack of visibility into memory consumption patterns preventing proactive capacity management

Unpredictable memory exhaustion scenarios causing emergency system maintenance and downtime

Inadequate swap space configuration leading to system failures during peak memory demand

Poor memory utilization tracking preventing optimization of system configurations and workload distribution


## Suggested Resolution

Immediate Response

Investigate servers showing high physical memory usage (>85%) for immediate memory recovery actions

Check swap space utilization levels and identify excessive paging activity requiring attention

Review memory-intensive processes and optimize or reschedule resource-heavy operations

Analyze memory paging rates (pages in/out per second) for system performance optimization

Capacity Assessment

Monitor memory utilization trends across all application servers for capacity planning

Evaluate current memory allocation against business volume projections and growth patterns

Check swap space configuration and utilization patterns for optimization opportunities

Analyze memory consumption by work processes and applications for efficiency improvements

Corrective Actions

Implement memory optimization for SAP instances including buffer tuning and parameter adjustments

Extend physical memory capacity or optimize swap space configuration for critical systems

Configure automated memory monitoring with appropriate alert thresholds and escalation procedures

Schedule memory-intensive operations during off-peak hours to maintain optimal system performance

Preventive Measures

Establish regular memory utilization reviews and performance baseline monitoring procedures

Implement automated monitoring for all critical servers with multiple memory threshold levels

Create documentation and procedures for memory tuning and emergency memory management

Plan system capacity upgrades based on memory utilization analysis and business growth projections


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | FREE_MEM | Free Memory (KB) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 2 | FREE_MEM_PRC | Free Memory (%) | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 3 | HOST | Host Name | CHAR | 32 | 0 | MSHOST2 | MSHOST2 |
| 4 | IN_KB_SEC | Page In (KB/s) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 5 | IN_PG_SEC | Pages In (Pg/S) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 6 | OUT_KB_SEC | Page Out (KB/s) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 7 | OUT_PG_SEC | Pages Out (Pg/S) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 8 | PHYS_MEM | Physical memory (KB) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 9 | PHYS_USAGE | Physical memory Usage (KB) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 10 | SERIALNR | Serial number | INT2 | 5 | 0 | COLLSERNR | INT2 |
| 11 | SERVER | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 12 | SUBTYPE | saposcol subtype | INT2 | 5 | 0 | COLLSUBTYP | INT2 |
| 13 | SWAP_CONF | Config primary swap fixed spac | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 14 | SWAP_FREE | Free space - total swap (KB) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 15 | SWAP_FREE_PRC | Free swap size (%) | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 16 | SWAP_MAX | Maximum swap size by filesyste | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 17 | SWAP_SIZE | Actual size of total swap (KB) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 18 | SWAP_USAGE | SWAP memory Usage (KB) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 19 | TYPE | saposcoltype | INT2 | 5 | 0 | COLLTYPE | INT2 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 19 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

FREE_MEM (Free Memory (KB))

Guards against oversized extracts when free memory (kb) on FREE_MEM is narrowed together with client, user, or session filters.

FREE_MEM_PRC (Free Memory (%))

Separates cross-client noise from in-scope work when free memory (%) on FREE_MEM_PRC correlates with client or user attributes.

HOST (Host Name)

Helps distinguish technical versus business attributes when host name on HOST correlates with counters or status fields.

IN_KB_SEC (Page In (KB/s))

Helps monitoring stay readable by requiring page in (kb/s) (IN_KB_SEC) to match organizational or technical selectors when set.

IN_PG_SEC (Pages In (Pg/S))

Explains why two monitoring passes differ: only the pass with stricter pages in (pg/s) on IN_PG_SEC surfaces the disputed rows.

OUT_KB_SEC (Page Out (KB/s))

When populated, keeps the extract focused so page out (kb/s) (OUT_KB_SEC) aligns with the intended triage slice.

OUT_PG_SEC (Pages Out (Pg/S))

Mirrors how administrators slice operational lists: pages out (pg/s) (OUT_PG_SEC) is one lever that shapes which rows are comparable run over run.

PHYS_MEM (Physical memory (KB))

Separates cross-client noise from in-scope work when physical memory (kb) on PHYS_MEM correlates with client or user attributes.

PHYS_USAGE (Physical memory Usage (KB))

Captures edge cases where physical memory usage (kb) (PHYS_USAGE) must be non-default to reproduce a customer-specific monitoring scenario.

SERIALNR (Serial number)

Stabilizes week-over-week metrics by fixing serial number (SERIALNR) while allowing duration thresholds to move.

SERVER (Server Name)

Aligns exception volume with the chosen scope by testing server name via SERVER before alert evaluation.

SUBTYPE (saposcol subtype)

Ensures reporting respects saposcol subtype constraints carried by SUBTYPE.

SWAP_CONF (Config primary swap fixed spac)

Reduces false positives during peak windows by tightening config primary swap fixed spac through SWAP_CONF alongside state filters.

SWAP_FREE (Free space - total swap (KB))

When harmonized with related filters, free space - total swap (kb) on SWAP_FREE isolates the highest-risk record families.

SWAP_FREE_PRC (Free swap size (%))

Separates cross-client noise from in-scope work when free swap size (%) on SWAP_FREE_PRC correlates with client or user attributes.

SWAP_MAX (Maximum swap size by filesyste)

Separates cross-client noise from in-scope work when maximum swap size by filesyste on SWAP_MAX correlates with client or user attributes.

SWAP_SIZE (Actual size of total swap (KB))

When combined with destination discipline, actual size of total swap (kb) on SWAP_SIZE keeps both breadth and depth of the extract intentional.

SWAP_USAGE (SWAP memory Usage (KB))

Interprets swap memory usage (kb) as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on SWAP_USAGE.

TYPE (saposcoltype)

Improves readability of exported lists because saposcoltype (TYPE) columns stay aligned with the configured filter intent.


### Parameter Relationships

How parameter combinations work together

The memory monitoring function provides both percentage-based and absolute value parameters to accommodate different monitoring scenarios and business requirements:


### Percentage vs. Absolute Value Parameters

Percentage Parameters (_PRC):

FREE_MEM_PRC - Remaining physical memory as percentage of total physical memory

SWAP_FREE_PRC - Remaining swap space as percentage of total swap space

Absolute Value Parameters:

FREE_MEM - Absolute amount of free physical memory in KB

PHYS_MEM - Total physical memory available in KB

SWAP_FREE - Absolute amount of free swap space in KB

SWAP_SIZE - Total swap space configured in KB

PHYS_USAGE - Physical memory usage in MB

SWAP_USAGE - Swap space usage in MB

Why Both Options Exist: The dual parameter approach provides monitoring flexibility for different business contexts. Percentage-based thresholds work well for systems of varying memory sizes (e.g., alert when any server drops below 15% free memory), while absolute value thresholds are essential for memory-critical systems where specific minimum memory quantities must be maintained (e.g., always keep at least 2GB physical memory available regardless of total system memory).


### Default Values

No default values are defined for this EI.


### Practical Example of Parameter Configuration

Use Case 1: Monitoring based on percentage values

Purpose: Alert when free memory percentage drops below 15% for capacity management

FREE_MEM_PRC = <15

Use Case 2: Monitoring based on absolute values

Purpose: Enterprise-scale memory monitoring using absolute thresholds for critical systems

FREE_MEM = <2000000

Use Case 3: Tight free-memory guard

Purpose: Alert when a named instance shows critically low free memory in kilobytes and percent.

SERVER = PRD_APP01
HOST = prd-mem-01
FREE_MEM_PRC = 0-8
PHYS_MEM = 67108864-999999999999



Use Case 4: Swap pressure

Purpose: Track swap consumption and free percentage on a host group.

HOST = %QAS%
SWAP_FREE_PRC = 0-15
SWAP_USAGE = 1073741824-9999999999999999
SWAP_SIZE = 17179869184-9999999999999999
SWAP_CONF = 17179869184-9999999999999999



Use Case 5: Paging spike review

Purpose: Combine paging rates with physical usage for a weekly stability report.

SERVER = %DEV%
IN_PG_SEC = 500-999999999
OUT_PG_SEC = 500-999999999
IN_KB_SEC = 1024-999999999
OUT_KB_SEC = 1024-999999999
PHYS_USAGE = 50000000-9999999999999999
TYPE = 1
SUBTYPE = 2
SERIALNR = 0-32767



EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_01_SRV_MEM | FREE_MEM | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | FREE_MEM_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_SRV_MEM | HOST | Name of Application Server | CHAR(32) | MSHOST2 |
| /SKN/S_SW_01_01_SRV_MEM | IN_KB_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | IN_PG_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | OUT_KB_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | OUT_PG_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | PHYS_MEM | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | PHYS_USAGE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | SERIALNR | Serial number in saposcol result structure | INT2(5) | COLLSERNR |
| /SKN/S_SW_01_01_SRV_MEM | SERVER | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_01_SRV_MEM | SUBTYPE | Subtype in saposcol result structure | INT2(5) | COLLSUBTYP |
| /SKN/S_SW_01_01_SRV_MEM | SWAP_CONF | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | SWAP_FREE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | SWAP_FREE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_SRV_MEM | SWAP_MAX | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | SWAP_SIZE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | SWAP_USAGE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_MEM | TYPE | Type in saposcol result structure | INT2(5) | COLLTYPE |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_01_SRV_MEM.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SRV_MEM OPTIONAL
*"----------------------------------------------------------------------
DATA_MULTY: DURATION /SKN/E_SW_DURATION,
            SERVER   MSNAME2,
            HOST     MSHOST2,
            IN_PG_SEC  INT4_DATA,
            OUT_PG_SEC  INT4_DATA,
            IN_KB_SEC  INT4_DATA,
            OUT_KB_SEC  INT4_DATA,
            FREE_MEM  INT4_DATA,
            PHYS_MEM  INT4_DATA,
            SWAP_CONF  INT4_DATA,
            SWAP_FREE  INT4_DATA,
            SWAP_SIZE  INT4_DATA,
            SWAP_MAX  INT4_DATA,
            PHYS_USAGE  INT4_DATA,
            SWAP_USAGE  INT4_DATA,
            FREE_MEM_PRC /SKN/E_SW_PRC,
            SWAP_FREE_PRC /SKN/E_SW_PRC.
DATA_SINGLE: AGGR_LEVEL CHAR1.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             LANGU LANGU,
             NUMBER SY-TABIX,
             SUBRC SY-SUBRC,
             MANAGE_IN_UTC CHAR1.
DATA: LS_WP_TOTAL_INFO TYPE WPTOTLINFO,
      LT_WP_TOTAL_INFO LIKE TABLE OF LS_WP_TOTAL_INFO,
      LS_COMMUNICATION_ERRORS TYPE WPTOTLICER,
      LT_COMMUNICATION_ERRORS LIKE TABLE OF LS_COMMUNICATION_ERRORS,
      IS_USED_WP TYPE WPTOTLIUWP,
      IT_USED_WP LIKE TABLE OF IS_USED_WP.
DATA: LS_SRV_LIST TYPE  MSXXLIST,
      LT_SRV_LIST LIKE TABLE OF LS_SRV_LIST.
DATA: LV_LOGICAL_DESTINATION TYPE RFCDEST.
DATA: F_MEM_ALL_READ LIKE  DEF_PAR_FU-NO_YES,
      ACTIVEFLAG LIKE  DEF_PAR_FU-ACTIVEFLAG,
      INTERVAL LIKE  DEF_PAR_FU-INTERVAL,
      DETAILSCOLL LIKE  DEF_PAR_FU-DETAILSCOL,
      DETAILSREQI LIKE  DEF_PAR_FU-DETAILSREQ,
      DETAILSMODE LIKE  DEF_PAR_FU-DETAILSMOD,
      LASTCOLLWRT LIKE  DEF_PAR_FU-LASTCOLLWR,
      LASTCOLLINT LIKE  DEF_PAR_FU-LASTCOLLIN,
      NORMCOLLINT LIKE  DEF_PAR_FU-NORMCOLLIN.
DATA: LS_MEM_ALL TYPE MEM_ALL,
      LT_MEM_ALL LIKE TABLE OF LS_MEM_ALL.
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
DATA : SY_TABIX LIKE SY-TABIX .
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
DATA : IS_OUT(1) TYPE C.
DATA: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
DATA: LV_NO_SERVER_LIST(1) TYPE C.
*-- Fill Selection Option Tables
SELECT_MULTY: DURATION,
            SERVER,
            HOST,
            IN_PG_SEC,
            OUT_PG_SEC,
            IN_KB_SEC,
            OUT_KB_SEC,
            FREE_MEM,
            PHYS_MEM,
            SWAP_CONF,
            SWAP_FREE,
            SWAP_SIZE,
            SWAP_MAX,
            PHYS_USAGE,
            SWAP_USAGE,
            FREE_MEM_PRC,
            SWAP_FREE_PRC.
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
      REFRESH LT_MEM_ALL.
      CALL FUNCTION 'GET_MEM_ALL'
        DESTINATION LV_SW_DEST
       EXPORTING
         LOCAL_REMOTE                         = 'INTERN'
         LOGICAL_DESTINATION                  = LV_LOGICAL_DESTINATION
        IMPORTING
          F_MEM_ALL_READ                       = F_MEM_ALL_READ
          ACTIVEFLAG                           = ACTIVEFLAG
          INTERVAL                             = INTERVAL
          DETAILSCOLL                          = DETAILSCOLL
          DETAILSREQI                          = DETAILSREQI
          DETAILSMODE                          = DETAILSMODE
          LASTCOLLWRT                          = LASTCOLLWRT
          LASTCOLLINT                          = LASTCOLLINT
          NORMCOLLINT                          = NORMCOLLINT
        TABLES
          TF_MEM_ALL                           = LT_MEM_ALL
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
      LOOP AT LT_MEM_ALL INTO LS_MEM_ALL.
        CLEAR: LS_DATA.
        MOVE-CORRESPONDING LS_MEM_ALL TO LS_DATA.
        LS_DATA-SERVER = LS_SRV_LIST-NAME.
        LS_DATA-HOST   = LS_SRV_LIST-HOST.
        LS_DATA-PHYS_USAGE = TRUNC( ( LS_DATA-PHYS_MEM - LS_DATA-FREE_MEM ) / 1000 ).
        LS_DATA-SWAP_USAGE = TRUNC( ( LS_DATA-SWAP_SIZE - LS_DATA-SWAP_FREE ) / 1000 ).
        IF LS_DATA-PHYS_MEM IS NOT INITIAL.
          LS_DATA-FREE_MEM_PRC = LS_DATA-FREE_MEM / LS_DATA-PHYS_MEM * 100.
        ENDIF.
        IF LS_DATA-SWAP_MAX IS NOT INITIAL.
          LS_DATA-SWAP_FREE_PRC = LS_DATA-SWAP_FREE / LS_DATA-SWAP_MAX * 100.
        ENDIF.
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
*** delete t_data where DURATION not in R_DURATION.
 DELETE T_DATA WHERE SERVER NOT IN R_SERVER.
 DELETE T_DATA WHERE HOST NOT IN R_HOST.
 DELETE T_DATA WHERE IN_PG_SEC NOT IN R_IN_PG_SEC.
 DELETE T_DATA WHERE OUT_PG_SEC NOT IN R_OUT_PG_SEC.
 DELETE T_DATA WHERE IN_KB_SEC NOT IN R_IN_KB_SEC.
 DELETE T_DATA WHERE OUT_KB_SEC NOT IN R_OUT_KB_SEC.
 DELETE T_DATA WHERE FREE_MEM NOT IN R_FREE_MEM.
 DELETE T_DATA WHERE PHYS_MEM NOT IN R_PHYS_MEM.
 DELETE T_DATA WHERE SWAP_CONF NOT IN R_SWAP_CONF.
 DELETE T_DATA WHERE SWAP_FREE NOT IN R_SWAP_FREE.
 DELETE T_DATA WHERE SWAP_SIZE NOT IN R_SWAP_SIZE.
 DELETE T_DATA WHERE SWAP_MAX NOT IN R_SWAP_MAX.
 DELETE T_DATA WHERE PHYS_USAGE NOT IN R_PHYS_USAGE.
 DELETE T_DATA WHERE SWAP_USAGE NOT IN R_SWAP_USAGE.
 DELETE T_DATA WHERE FREE_MEM_PRC NOT IN R_FREE_MEM_PRC.
 DELETE T_DATA WHERE SWAP_FREE_PRC NOT IN R_SWAP_FREE_PRC.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
