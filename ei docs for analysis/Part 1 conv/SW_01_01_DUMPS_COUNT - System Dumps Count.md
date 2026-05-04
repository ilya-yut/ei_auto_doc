# Exception Indicator: System Dumps Count (SW_01_01_DUMPS_COUNT)


## General Overview

This Exception Indicator (EI) monitors and counts the total number of system dumps (ABAP runtime errors) that occur within a specified time period and filtering criteria. Short dumps occur when the SAP system encounters an error during runtime, resulting in the termination of the affected process. As part of daily monitoring, it is the responsibility of the basis administrator to analyze the dumps and take necessary actions to avoid future issues.

 The EI acts as a high-level system health indicator that catches:

Unusual spikes in error rates that may signal underlying technical problems, code defects, or infrastructure issues

Zero-dump periods for validation that systems are running cleanly (when monitoring for minimum thresholds)

Threshold breaches where the number of dumps exceeds acceptable operational limits

A few examples of ABAP dumps are timeout problems, database space issues, spool overflow issues etc. This EI provides count-based monitoring rather than detailed dump analysis, using the same data sources as ST22 (ABAP Dump Analysis) but focusing on aggregate metrics.


## Problem Description

High dump counts indicate system instability causing:

System Performance Issues

Resource exhaustion and memory shortages

Code defects causing runtime failures

Infrastructure problems affecting servers

Process Disruptions

Batch job failures preventing automated processes

Transaction failures blocking critical operations

Performance degradation during peak usage

Business Continuity

Data inconsistency during update processes

SLA violations due to system unavailability

Customer-facing errors damaging reputation


## Suggested Resolution

Immediate Response

Investigate dump root causes using ST22 transaction

Check system resource levels and restart servers if needed

Review system logs for infrastructure errors

System Assessment

Monitor CPU, memory, and database performance

Check system processes and communication channels

Analyze resource consumption patterns

Corrective Actions

Apply code fixes and system parameter adjustments

Optimize resource-intensive operations

Establish proactive monitoring and quality controls


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | DUMPS_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 3 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 4 | DURATION_UNIT | Duration Unit(D/H/M) |  | 0 | 0 |  |  |
| 5 | INCLUDENAME | Include Name |  | 0 | 0 |  |  |
| 6 | PROGRAMNAME | Program Name |  | 0 | 0 |  |  |
| 7 | SYHOST | Host |  | 0 | 0 |  |  |
| 8 | SYUSER | User |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: This section provides configuration guidance for ALL 8 parameters listed in the Parameters Reference Table above.

BACKDAYS (Days Backward from today):

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

DUMPS_CNT (Count):

DUMPS_CNT is the final threshold/range check against the counted number of dump records returned from the base function.

DURATION (Duration In Time Units):

DURATION is the aging threshold applied in the base function before records are counted.

DURATION_UNIT (Duration Unit(D/H/M)):

DURATION_UNIT defines time unit for DURATION evaluation in underlying dump processing.

DURATION_UNIT Options:

·        H: Hours

·        M: Minutes

·        D: Days

·        F: Full days for specific day filtering

INCLUDENAME (Include Name):

INCLUDENAME narrows the base dataset to include-level code context before final count is calculated.

PROGRAMNAME (Program Name):

PROGRAMNAME narrows base dump events by ABAP program ownership before count aggregation.

SYHOST (Host):

SYHOST scopes base dump selection by host to identify node-specific instability contributions to total count.

SYUSER (User):

SYUSER scopes base dump selection by user context for ownership-based surge analysis.


### Parameter Relationship

How parameter combinations work together

Selection Scope and Count Outcome:

BACKDAYS provides fallback time scope when explicit date filters are not supplied to the base logic.

Duration and Threshold Sequence:

·        DURATION + DURATION_UNIT are an additional (second) filter after date selection inside the base function.

·        The wrapper then counts remaining records and checks the result against DUMPS_CNT.

·        Final alert behavior depends on both base filtering and count-threshold match.


### Default Values

·        DURATION_UNIT - H

·        BACKDAYS - 1 (today and yesterday)


### Practical Example of Parameter Configuration

Use Case 1: Short-window dump surge alert

BACKDAYS = 1
 DURATION = 1
 DURATION_UNIT = H
 DUMPS_CNT = 20 - 999999



Purpose: Alert when at least 20 dumps occurred in the recent short monitoring window.

Use Case 2: Program-specific count spike

PROGRAMNAME = SAPLZ_CUSTOM_POSTING
 SYHOST = APP01
 DURATION = 30
 DURATION_UNIT = M
 DUMPS_CNT = 10 - 999999



Purpose: Detect concentrated dump volume related to one program on one host.

Use Case 3: User-context stability control

SYUSER = BATCH_USER_01
 INCLUDENAME = ZMM_IMPORT_INCLUDE
 BACKDAYS = 2
 DUMPS_CNT = 5 - 999999



Purpose: Monitor recurring dump accumulation linked to a user/include execution context.


## EI Function Structure

This table lists all output fields returned by the EI.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_01_DUMPS_CNT | DUMPS_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_01_DUMPS_CNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_DUMPS_CNT OPTIONAL
*"----------------------------------------------------------------------
DATA : LV_ALERT TYPE  CHAR1.
DATA : LS_DATA TYPE /SKN/S_SW_01_01_DUMPS,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA : LV_CNT TYPE I.
DATA_MULTY: DUMPS_CNT /SKN/E_SW_CNT.
SELECT_MULTY: DUMPS_CNT.
   REFRESH T_DATA.
   CALL FUNCTION '/SKN/F_SW_01_01_DUMPS'
    IMPORTING
       IS_ALERT       = LV_ALERT
     TABLES
       T_SELECT       = T_SELECT
       T_DATA         = LT_DATA.
    IS_ALERT = LV_ALERT.
    DESCRIBE TABLE LT_DATA LINES LV_CNT.
    IF LV_CNT IN R_DUMPS_CNT.
      T_DATA-DUMPS_CNT = LV_CNT.
      APPEND T_DATA.
      IS_ALERT = LV_ALERT.
    ELSE.
      CLEAR IS_ALERT.
    ENDIF.
ENDFUNCTION.
```
