# Exception Indicator: System Work Processes Count Monitoring (SW_01_20_WP_TOT)


## General Overview

This Exception Indicator (EI) provides comprehensive work process monitoring and analysis across SAP system landscapes to ensure optimal work process utilization and prevent resource bottlenecks that could cause critical performance issues. Work process monitoring is fundamental for maintaining SAP system responsiveness, user session management, and background job processing efficiency.

The System Work Processes Count Monitoring solution monitors work process allocation, utilization, and availability across all application servers in a distributed SAP landscape, providing real-time visibility into work process consumption patterns and resource availability. When work processes become fully utilized or unavailable, it can lead to user login failures, transaction queuing, batch job delays, and complete system unresponsiveness.

This Exception Indicator provides advanced work process monitoring capabilities that enable:

Multi-server monitoring across distributed SAP landscapes with centralized visibility

Comprehensive work process metrics including dialog, batch, update, and spool process tracking

Real-time utilization analysis with current usage and availability calculations

Process type differentiation supporting DIA, BTC, UPD, UP2, and SPO work process monitoring

Enhanced duration tracking for long-running processes and performance optimization

Historical tracking with timestamp recording for work process trend analysis and capacity planning

The monitoring solution leverages SAP's built-in work process monitoring functions and provides enhanced analytical capabilities including work process usage calculations, duration tracking, and comprehensive server list management for enterprise-scale work process performance monitoring.

This Exception Indicator ensures that SAP systems maintain optimal work process availability across all critical application servers and identifies potential resource bottlenecks before they impact business operations.


## Problem Description

Work process exhaustion and inadequate monitoring indicate critical system resource issues causing:

System Availability Risks

Complete user login failures when dialog work processes become fully utilized

Batch job queue stagnation when background work processes are exhausted

Update process bottlenecks causing transaction commit delays and data consistency issues

Spool process unavailability preventing report generation and printing operations

Performance Degradation Issues

Severe response time increases when work processes reach capacity limits

Transaction queuing and user wait times during peak work process utilization

Background job processing delays affecting critical business processes and reporting cycles

System lock-ups from work process resource contention and deadlock scenarios

Business Process Impact

User productivity loss from session timeouts and login failures during peak usage

Batch processing delays affecting payroll, billing, and financial closing procedures

Report generation failures impacting business decision-making and compliance requirements

Interface processing delays affecting real-time data exchange with external systems

Capacity Planning Issues

Lack of visibility into work process consumption patterns preventing proactive capacity management

Unpredictable resource exhaustion scenarios causing emergency system interventions

Inadequate work process allocation leading to frequent resource shortages and user complaints

Poor utilization tracking preventing optimization of work process configurations and load distribution


## Suggested Resolution

Immediate Response

Investigate servers showing high work process utilization (>90%) for immediate load balancing opportunities

Check dialog work process availability and redistribute user load across available application servers

Review background work process queues and reschedule non-critical batch jobs to off-peak hours

Analyze long-running processes and identify optimization opportunities or termination candidates

Capacity Assessment

Monitor work process utilization trends across all application servers for capacity planning

Evaluate current work process allocation against user load and business volume projections

Check work process duration patterns for performance optimization and configuration tuning

Analyze work process distribution by type and server for load balancing optimization

Corrective Actions

Increase work process allocation for dialog and background processing based on utilization analysis

Configure dynamic work process management to automatically adjust allocation during peak periods

Implement work process monitoring with appropriate alert thresholds and escalation procedures

Optimize long-running processes and implement background job scheduling improvements

Preventive Measures

Establish regular work process utilization reviews and capacity planning procedures

Implement automated monitoring for all critical servers with multiple work process threshold levels

Create documentation and procedures for work process tuning and emergency resource management

Plan system capacity upgrades based on work process utilization analysis and business growth projections


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | AGGR_LEVEL | Aggr. Level (T-total/S-server) |  | 0 | 0 |  |  |
| 2 | BTC_CNT | Total Background WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 3 | BTC_DURATION | Backgroung WP Duration In Min. | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 4 | BTC_FREE_CNT | Free Background WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 5 | BTC_FREE_PRC | % of Free Background WPs | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 6 | BTC_INUSE_CNT | Used Background WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 7 | BTC_INUSE_PRC | % of In Use Background WPs | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 8 | DIA_CNT | Total Dialog WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 9 | DIA_DURATION | Dialog WP Duration In Min. | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DIA_FREE_CNT | Free Dialog WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 11 | DIA_FREE_PRC | % of Free Dialog WPs | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 12 | DIA_INUSE_CNT | Used Dialog WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 13 | DIA_INUSE_PRC | % of In Use Dialog WPs | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 14 | SPO_CNT | Total Spool WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 15 | SPO_FREE_CNT | Free Spool WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 16 | SPO_INUSE_CNT | Used Spool WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 17 | UP2_CNT | Total Update-2 WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 18 | UP2_FREE_CNT | Free Update-2 WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 19 | UP2_INUSE_CNT | Used Update-2 WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 20 | UPD_CNT | Total Update WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 21 | UPD_FREE_CNT | Free Update WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 22 | UPD_INUSE_CNT | Used Update WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 23 | WP_SERVER | Server Name | CHAR | 20 | 0 | MSNAME | MSNAME |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 23 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

AGGR_LEVEL (Aggr. Level (T-total/S-server))

Controls aggregation granularity for work process totals versus server-specific breakdown.

AGGR_LEVEL Options:

·        T — Total aggregation across all servers.

·        S — Server-level aggregation.

BTC_CNT (Total Background WPs)

Stabilizes week-over-week metrics by fixing total background wps (BTC_CNT) while allowing duration thresholds to move.

BTC_DURATION (Backgroung WP Duration In Min.)

When populated, keeps the extract focused so backgroung wp duration in min. (BTC_DURATION) aligns with the intended triage slice.

BTC_FREE_CNT (Free Background WPs)

When tightened, free background wps (BTC_FREE_CNT) removes rows that would otherwise dilute attention from failing or stuck cases.

BTC_FREE_PRC (% of Free Background WPs)

For distributed landscapes, % of free background wps on BTC_FREE_PRC often anchors which application server or destination appears in results.

BTC_INUSE_CNT (Used Background WPs)

Captures edge cases where used background wps (BTC_INUSE_CNT) must be non-default to reproduce a customer-specific monitoring scenario.

BTC_INUSE_PRC (% of In Use Background WPs)

Ensures reporting respects % of in use background wps constraints carried by BTC_INUSE_PRC.

DIA_CNT (Total Dialog WPs)

Reflects real administration where total dialog wps on DIA_CNT is routinely restricted to a single productive client or object family.

DIA_DURATION (Dialog WP Duration In Min.)

Uses dialog wp duration in min. from the source context so only records with DIA_DURATION inside declared values proceed.

DIA_FREE_CNT (Free Dialog WPs)

Stabilizes week-over-week metrics by fixing free dialog wps (DIA_FREE_CNT) while allowing duration thresholds to move.

DIA_FREE_PRC (% of Free Dialog WPs)

Helps monitoring stay readable by requiring % of free dialog wps (DIA_FREE_PRC) to match organizational or technical selectors when set.

DIA_INUSE_CNT (Used Dialog WPs)

Helps distinguish technical versus business attributes when used dialog wps on DIA_INUSE_CNT correlates with counters or status fields.

DIA_INUSE_PRC (% of In Use Dialog WPs)

Treats % of in use dialog wps as a discriminator between similar rows that would otherwise look identical in a raw extract.

SPO_CNT (Total Spool WPs)

Narrows retrieved rows where total spool wps (SPO_CNT) must match the configured selection for this monitor.

SPO_FREE_CNT (Free Spool WPs)

Separates cross-client noise from in-scope work when free spool wps on SPO_FREE_CNT correlates with client or user attributes.

SPO_INUSE_CNT (Used Spool WPs)

Improves readability of exported lists because used spool wps (SPO_INUSE_CNT) columns stay aligned with the configured filter intent.

UP2_CNT (Total Update-2 WPs)

Prevents accidental global scans when total update-2 wps (UP2_CNT) is meant to stay within a controlled application slice.

UP2_FREE_CNT (Free Update-2 WPs)

Allows phased rollout: first widen UP2_FREE_CNT for free update-2 wps, then tighten thresholds once baseline noise is understood.

UP2_INUSE_CNT (Used Update-2 WPs)

Mirrors how administrators slice operational lists: used update-2 wps (UP2_INUSE_CNT) is one lever that shapes which rows are comparable run over run.

UPD_CNT (Total Update WPs)

Separates cross-client noise from in-scope work when total update wps on UPD_CNT correlates with client or user attributes.

UPD_FREE_CNT (Free Update WPs)

Narrows retrieved rows where free update wps (UPD_FREE_CNT) must match the configured selection for this monitor.

UPD_INUSE_CNT (Used Update WPs)

Improves readability of exported lists because used update wps (UPD_INUSE_CNT) columns stay aligned with the configured filter intent.

WP_SERVER (Server Name)

Pairs with duration logic: once WP_SERVER passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.


### Parameter Relationships

The work process monitoring function provides both percentage-based and absolute value parameters to accommodate different monitoring scenarios and business requirements:


### Percentage vs. Absolute Value Parameters

Percentage Parameters (_PRC):

DIA_FREE_PRC - Available dialog work processes as percentage of total dialog processes

BTC_FREE_PRC - Available batch work processes as percentage of total batch processes

DIA_INUSE_PRC - Utilized dialog work processes as percentage of total dialog processes

BTC_INUSE_PRC - Utilized batch work processes as percentage of total batch processes

Absolute Value Parameters (_CNT):

DIA_CNT - Total count of dialog work processes configured

BTC_CNT - Total count of batch work processes configured

DIA_FREE_CNT - Absolute count of available dialog work processes

BTC_FREE_CNT - Absolute count of available batch work processes

DIA_INUSE_CNT - Absolute count of utilized dialog work processes

BTC_INUSE_CNT - Absolute count of utilized batch work processes

Why Both Options Exist: The dual parameter approach provides monitoring flexibility for different business contexts. Percentage-based thresholds work well for systems of varying work process configurations (e.g., alert when any server drops below 20% free dialog processes), while absolute value thresholds are essential for business-critical systems where specific minimum work process quantities must be maintained (e.g., always keep at least 5 dialog work processes available regardless of total configuration).


### Default Values

·        AGGR_LEVEL - T

Use Case 1: Alert when dialog work process utilization exceeds 85% for user session management

DIA_INUSE_PRC = >85

Use Case 2: Enterprise-scale batch monitoring using absolute thresholds for critical processing

BTC_FREE_CNT = <3

Use Case 3: Combined dialog and batch monitoring for comprehensive work process capacity management

DIA_FREE_PRC = <15

BTC_INUSE_PRC = >80


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_01_WP_TOT | BTC_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | BTC_DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_WP_TOT | BTC_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | BTC_FREE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_WP_TOT | BTC_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | BTC_INUSE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_WP_TOT | DIA_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | DIA_DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_WP_TOT | DIA_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | DIA_FREE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_WP_TOT | DIA_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | DIA_INUSE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_WP_TOT | SPO_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | SPO_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | SPO_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UP2_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UP2_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UP2_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UPD_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UPD_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UPD_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | WP_SERVER | Server Name | CHAR(20) | MSNAME |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_01_WP_TOT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_WP_TOT OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_WP TYPE /SKN/S_SW_01_01_SM66,
      LT_WP LIKE TABLE OF LS_WP.
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
DATA: BEGIN OF LS_DURATION,
        WP_SERVER TYPE MSNAME,
        WP_TYP TYPE WPTYP,
        DURATION TYPE /SKN/E_SW_DURATION,
***        DIA_DURATION type /SKN/E_SW_DURATION,
***        BTC_DURATION type /SKN/E_SW_DURATION,
      END OF LS_DURATION,
      LT_DURATION LIKE TABLE OF LS_DURATION.
DATA: FLD(30) TYPE C.
FIELD-SYMBOLS: <FS_CNT> TYPE INT2,
               <FS_FREE> TYPE INT2,
               <FS_INUSE> TYPE INT2.
DATA: LV_WP_TYP(3) TYPE C.
DATA: SY_TABIX LIKE SY-TABIX.
DATA_MULTY: WP_SERVER  MSNAME.
DATA_MULTY: DIA_CNT INT2,
            BTC_CNT INT2,
            UPD_CNT INT2,
            UP2_CNT INT2,
            SPO_CNT INT2,
            DIA_FREE_CNT INT2,
            BTC_FREE_CNT INT2,
            UPD_FREE_CNT INT2,
            UP2_FREE_CNT INT2,
            SPO_FREE_CNT INT2,
            DIA_INUSE_CNT INT2,
            BTC_INUSE_CNT INT2,
            UPD_INUSE_CNT INT2,
            UP2_INUSE_CNT INT2,
            SPO_INUSE_CNT INT2,
            DIA_FREE_PRC /SKN/E_SW_PRC,
            BTC_FREE_PRC /SKN/E_SW_PRC,
            DIA_INUSE_PRC /SKN/E_SW_PRC,
            BTC_INUSE_PRC /SKN/E_SW_PRC,
            DIA_DURATION   /SKN/E_SW_DURATION,
            BTC_DURATION   /SKN/E_SW_DURATION.
.
DATA_SINGLE: AGGR_LEVEL CHAR1.
*-- Fill Selection Option Tables
SELECT_MULTY: WP_SERVER.
SELECT_MULTY: DIA_CNT,
              BTC_CNT,
              UPD_CNT,
              UP2_CNT,
              SPO_CNT,
              DIA_FREE_CNT,
              BTC_FREE_CNT,
              UPD_FREE_CNT,
              UP2_FREE_CNT,
              SPO_FREE_CNT,
              DIA_INUSE_CNT,
              BTC_INUSE_CNT,
              UPD_INUSE_CNT,
              UP2_INUSE_CNT,
              SPO_INUSE_CNT,
              DIA_FREE_PRC,
              BTC_FREE_PRC,
              DIA_INUSE_PRC,
              BTC_INUSE_PRC,
              DIA_DURATION,
              BTC_DURATION.
 LV_AGGR_LEVEL = 'T'.
 SELECT_SINGLE: AGGR_LEVEL.
    CALL FUNCTION '/SKN/F_SW_01_01_SM66'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = LT_WP.
     SORT LT_WP BY WP_SERVER WP_TYP.
     LOOP AT LT_WP INTO LS_WP.
       CLEAR LS_DATA.
       IF LV_AGGR_LEVEL <> 'S'.
         CLEAR LS_DATA-WP_SERVER.
       ELSE.
         LS_DATA-WP_SERVER = LS_WP-WP_SERVER.
       ENDIF.
       LV_WP_TYP = LS_WP-WP_TYP.
       IF LV_WP_TYP = 'BGD'.
         LV_WP_TYP = 'BTC'.
       ENDIF.
       CONCATENATE LV_WP_TYP '_CNT' INTO FLD.
        ASSIGN COMPONENT FLD OF STRUCTURE LS_DATA TO <FS_CNT>.
        IF <FS_CNT> IS ASSIGNED.
          ADD 1 TO <FS_CNT>.
        ENDIF.
       CONCATENATE LV_WP_TYP '_FREE_CNT' INTO FLD.
        ASSIGN COMPONENT FLD OF STRUCTURE LS_DATA TO <FS_FREE>.
        IF <FS_FREE> IS ASSIGNED.
          IF LS_WP-WP_ISTATUS = '2'.  " Waiting
            ADD 1 TO <FS_FREE>.
          ENDIF.
        ENDIF.
       CONCATENATE LV_WP_TYP '_INUSE_CNT' INTO FLD.
        ASSIGN COMPONENT FLD OF STRUCTURE LS_DATA TO <FS_INUSE>.
        IF <FS_INUSE> IS ASSIGNED.
          IF LS_WP-WP_ISTATUS <> '2'.
            ADD 1 TO <FS_INUSE>.
          ENDIF.
        ENDIF.
        COLLECT LS_DATA INTO LT_DATA.
        MOVE-CORRESPONDING LS_WP TO LS_DURATION.
        LS_DURATION-WP_SERVER = LS_DATA-WP_SERVER.
        IF LS_WP-DURATION > 0.
          APPEND LS_DURATION TO LT_DURATION.
        ENDIF.
     ENDLOOP.
     SORT LT_DURATION BY WP_SERVER WP_TYP DURATION.
     LOOP AT LT_DATA INTO LS_DATA.
       SY_TABIX = SY-TABIX.
       IF LS_DATA-DIA_CNT <> 0.
         LS_DATA-DIA_FREE_PRC = LS_DATA-DIA_FREE_CNT / LS_DATA-DIA_CNT * 100.
         LS_DATA-DIA_INUSE_PRC = LS_DATA-DIA_INUSE_CNT / LS_DATA-DIA_CNT * 100.
       ENDIF.
       IF LS_DATA-BTC_CNT IS NOT INITIAL.
         LS_DATA-BTC_FREE_PRC = LS_DATA-BTC_FREE_CNT / LS_DATA-BTC_CNT * 100.
         LS_DATA-BTC_INUSE_PRC = LS_DATA-BTC_INUSE_CNT / LS_DATA-BTC_CNT * 100.
       ENDIF.
       READ TABLE LT_DURATION INTO LS_DURATION
                              WITH KEY WP_SERVER = LS_DATA-WP_SERVER
                                       WP_TYP = 'BTC'.
       IF SY-SUBRC IS INITIAL.
         LS_DATA-BTC_DURATION = LS_DURATION-DURATION.
       ENDIF.
       "---
       READ TABLE LT_DURATION INTO LS_DURATION
                              WITH KEY WP_SERVER = LS_DATA-WP_SERVER
                                       WP_TYP = 'DIA'.
       IF SY-SUBRC IS INITIAL.
         LS_DATA-DIA_DURATION = LS_DURATION-DURATION.
       ENDIF.
       MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
     ENDLOOP.
  DELETE LT_DATA WHERE: DIA_CNT NOT IN R_DIA_CNT,
                        BTC_CNT NOT IN R_BTC_CNT,
                        UPD_CNT NOT IN R_UPD_CNT,
                        UP2_CNT NOT IN R_UP2_CNT,
                        SPO_CNT NOT IN R_SPO_CNT,
                        DIA_FREE_CNT NOT IN R_DIA_FREE_CNT,
                        BTC_FREE_CNT NOT IN R_BTC_FREE_CNT,
                        UPD_FREE_CNT NOT IN R_UPD_FREE_CNT,
                        UP2_FREE_CNT NOT IN R_UP2_FREE_CNT,
                        SPO_FREE_CNT NOT IN R_SPO_FREE_CNT,
                        DIA_INUSE_CNT NOT IN R_DIA_INUSE_CNT,
                        BTC_INUSE_CNT NOT IN R_BTC_INUSE_CNT,
                        UPD_INUSE_CNT NOT IN R_UPD_INUSE_CNT,
                        UP2_INUSE_CNT NOT IN R_UP2_INUSE_CNT,
                        SPO_INUSE_CNT NOT IN R_SPO_INUSE_CNT,
                        DIA_FREE_PRC NOT IN R_DIA_FREE_PRC,
                        BTC_FREE_PRC NOT IN R_BTC_FREE_PRC,
                        DIA_INUSE_PRC NOT IN R_DIA_INUSE_PRC,
                        BTC_INUSE_PRC NOT IN R_BTC_INUSE_PRC,
                        DIA_DURATION  NOT IN R_DIA_DURATION,
                        BTC_DURATION  NOT IN R_BTC_DURATION.
  T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
