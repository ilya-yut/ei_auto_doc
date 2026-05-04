# Exception Indicator: Canceled Background Jobs Monitoring (SW_01_01_JOBS_CANC_N)


## General Overview

This Exception Indicator (EI) provides specialized monitoring and analysis of canceled, aborted, and failed SAP background jobs that meet specific criteria within a specified time period and filtering conditions. Canceled jobs represent critical system events that require immediate attention as they indicate process failures, system issues, or operational problems that prevent successful job completion.

This EI acts as a specialized job failure monitoring system that provides comprehensive information for:

Canceled job identification for tracking jobs that were terminated abnormally or failed to complete successfully

Failure pattern analysis that may signal underlying technical problems, system instability, or process issues

User-specific failure tracking for monitoring job cancellations by specific users or user groups

Enhanced status analysis with human-readable descriptions for better understanding of failure reasons

Detailed failure analysis where administrators need comprehensive data for troubleshooting and failure prevention

As part of critical monitoring, it is the responsibility of the basis administrator to analyze canceled jobs immediately and take necessary corrective actions to prevent business process disruptions. This EI provides detailed analysis of job failures with runtime calculations, user attribution, and comprehensive filtering capabilities, focusing on identifying and resolving job cancellation issues.


## Problem Description

Canceled background jobs indicate critical system issues causing:

System Stability Issues

Job cancellations due to system resource exhaustion, memory shortages, or infrastructure failures

Database connectivity problems or lock conflicts causing job terminations

System crashes or server failures interrupting job execution and causing abnormal terminations

Resource contention issues preventing jobs from completing successfully

Process Disruptions

Critical business processes failing due to job cancellations affecting operational continuity

Data processing failures causing incomplete transactions and data inconsistency

Automated workflows breaking due to job failures affecting downstream business operations

Scheduled processes not completing causing delays in reporting and business activities

Business Continuity

SLA violations due to failed automated processes exceeding agreed service levels

Data integrity issues from incomplete job executions affecting business operations and reporting

Customer-facing impacts from failed automated processes causing service disruptions

Financial losses due to incomplete data processing and failed critical business operations


## Suggested Resolution

Immediate Response

Investigate canceled jobs immediately using SM37 transaction for detailed failure analysis

Check system logs and job logs for specific error messages and failure root causes

Review system resource levels during job execution periods to identify capacity issues

System Assessment

Monitor job cancellation patterns and frequency to identify systemic issues

Check system stability indicators including memory, CPU, and database performance

Analyze job failure trends by user, program, and time patterns for prevention strategies

Corrective Actions

Address underlying system issues causing job cancellations through infrastructure improvements

Implement job monitoring and automatic restart procedures for critical processes

Establish proactive failure prevention measures including resource monitoring and capacity planning

Improve job design and error handling to reduce cancellation frequency


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

·        SYHOST, SYUSER, PROGRAMNAME, and INCLUDENAME narrow the dump event population in the base function.

·        BACKDAYS provides fallback time scope when explicit date filters are not supplied to the base logic.

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
FUNCTION /SKN/F_SW_01_01_JOBS_N.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_JOBS OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: MANAGE_IN_UTC         CHAR1 ,
               LANGU                 LANGU,
               NO_DATE_RESTRICTION   CHAR1,
               DATE_REF_FLD          NAME_FELD,
               W_VARIANT             CHAR1 ,
               DURATION_UNIT         /SKN/E_SW_DURATION_UNIT.
  DATA_MULTY: DURATION_M    /SKN/E_SW_DURATION_M, "Job Running Duration -From start to End
              DURATION_H    /SKN/E_SW_DURATION_H, "Job Running Duration
              DURATION      /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
              JOBNAME       BTCJOB,
              STATE_COLOR   /SKN/E_SW_STATE_COLOR,
              STATUS        BTCSTATUS,
              SDLUNAME      BTCSDLNM,
              LASTCHNAME    BTCJCHNM,
              PROGNAME      BTCPROG,
              PERIODIC      BTCPFLAG,
              STRTDATE      BTCXDATE,   " Job start date
              STRTTIME      BTCXTIME,   " Job start time
              ENDDATE       BTCXDATE,   " Job end date
              ENDTIME       BTCXTIME,   " Job end time         " 11/24++
              SDLDATE       BTCSDLDATE, " Date of job/step scheduling
              SDLSTRTDT     BTCSDATE,   " Planned Start Date for Background Job
              LASTCHDATE    BTCJCHDATE, " Date of last job change
              RELDATE       BTCRELDT,   " Release Date for Background Scheduling
              DATUM         SY-DATUM .
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: LANGU,
                 MANAGE_IN_UTC,
                 NO_DATE_RESTRICTION,
                 DATE_REF_FLD,
                 W_VARIANT,
                 DURATION_UNIT.
  SELECT_MULTY: DURATION_M ,
                DURATION_H ,
                DURATION,
                JOBNAME ,
                STATE_COLOR,
                STATUS,
                SDLUNAME,
                LASTCHNAME,
                PROGNAME,
                PERIODIC,
                STRTDATE,
                STRTTIME,
                ENDDATE,
                ENDTIME,   " 11/24++
                SDLDATE,
                SDLSTRTDT,
                LASTCHDATE ,
                RELDATE,
                DATUM.
  DATA :   IS_GENERAL(1) TYPE C.
  DATA : DATE_FROM LIKE SY-DATUM ,
         BACKDAYS  TYPE I .
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA :  ENDDATE LIKE SY-DATUM,
          ENDTIME LIKE SY-UZEIT.
  DATA : TIME_DIFF TYPE  INT4 .
  DATA : IS_OUT(1) TYPE C.
  DATA :   SY_DATLO LIKE SY-DATLO ,
           SY_TIMLO LIKE SY-TIMLO .
****26-6-16*** Time Diff correction - for Date Ref. Field
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D,
         REF_TIME TYPE T.
  FIELD-SYMBOLS: <FS> TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA: LV_D_FROM LIKE  SY-DATUM,
        LV_T_FROM LIKE  SY-UZEIT,
        LV_D_TO LIKE  SY-DATUM,
        LV_T_TO LIKE  SY-UZEIT.
  DATA: LV_DATE_REF_FLD_ORG TYPE NAME_FELD.
  LV_DATE_REF_FLD_ORG = LV_DATE_REF_FLD.
  IF LV_DATE_REF_FLD IS INITIAL.
    LV_DATE_REF_FLD = 'STRTDATE'.
  ENDIF.
*****************************************************26-6-16
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_JOBS_N'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
***  endif.
***  check lv_sw_dest is initial.
    "--- Run Cloud Mode -----
  ELSE.
    SY_DATLO = SY-DATUM .        " Appl Server's Date
    SY_TIMLO = SY-UZEIT.
***********************************************
    LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
      RS_DATUM-SIGN   = 'I' .
      RS_DATUM-OPTION = 'GE'.
      BACKDAYS     = T_SELECT-LOW .
      DATE_FROM    = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
      EXIT.
    ENDLOOP.
    IF R_DATUM[] IS INITIAL .  " Set default value
      RS_DATUM-SIGN   = 'I'.
      RS_DATUM-OPTION = 'GE'.
      BACKDAYS     = '1' .  "--- Default
      DATE_FROM    = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
    ENDIF .
**********************************************
*  IF r_strtdate[] IS INITIAL.
*    r_strtdate[] = r_datum[] .
*  ENDIF.
**************** SET DATE REFERENCE FIELD ************************
    IF R_STRTDATE[]   IS INITIAL AND
       R_ENDDATE[]    IS INITIAL AND
       R_SDLDATE[]    IS INITIAL AND
       R_SDLSTRTDT[]  IS INITIAL AND
       R_LASTCHDATE[] IS INITIAL AND
       R_RELDATE[]    IS INITIAL.
      R_STRTDATE[] = R_DATUM[] .
    ENDIF.
    IF LV_NO_DATE_RESTRICTION IS NOT INITIAL.
      REFRESH R_DATUM.
    ENDIF.
    CASE LV_DATE_REF_FLD.
      WHEN 'STRTDATE'.
        R_STRTDATE[] = R_DATUM[].   " Job start date
      WHEN 'ENDDATE'.
        R_ENDDATE[] = R_DATUM[].    " Job end date
      WHEN 'SDLDATE'.
        R_SDLDATE[] = R_DATUM[].    " Date of job/step scheduling
      WHEN 'SDLSTRTDT'.
        R_SDLSTRTDT[] = R_DATUM[].  " Planned Start Date for Background Job
      WHEN 'LASTCHDATE'.
        R_LASTCHDATE[] = R_DATUM[]. " Last change Date for Background Job
      WHEN 'RELDATE'.
        R_RELDATE[] = R_DATUM[].    " Release Date for Background Scheduling
      WHEN OTHERS.
        R_STRTDATE[] = R_DATUM[].   " Job start date
    ENDCASE.
*************************************************************************
*--- Retrieve data
    CLEAR IS_ALERT .
    REFRESH T_DATA .
    SELECT *
       FROM V_OP
       INTO CORRESPONDING FIELDS OF TABLE T_DATA
       WHERE  JOBNAME   IN R_JOBNAME
         AND  STATUS    IN R_STATUS
         AND SDLUNAME   IN R_SDLUNAME
         AND LASTCHNAME IN R_LASTCHNAME
         AND PROGNAME   IN R_PROGNAME
         AND PERIODIC   IN R_PERIODIC
         AND STRTTIME   IN R_STRTTIME
         AND STRTDATE   IN R_STRTDATE
         AND ENDDATE    IN R_ENDDATE
         AND ENDTIME    IN R_ENDTIME
         AND SDLDATE    IN R_SDLDATE
         AND SDLSTRTDT  IN R_SDLSTRTDT
         AND LASTCHDATE IN R_LASTCHDATE
         AND RELDATE    IN R_RELDATE
             .
    SORT T_DATA BY JOBNAME JOBCOUNT STRTDATE STRTTIME.
    DELETE ADJACENT DUPLICATES FROM T_DATA.
    LOOP AT T_DATA.
      SY_TABIX = SY-TABIX .
      CALL FUNCTION '/SKN/F_SW_01_01_JOB_STATUS'
        EXPORTING
          STATUS      = T_DATA-STATUS
        IMPORTING
          STATUS_DESC = T_DATA-STATUS_DESC
          STATE_COLOR = T_DATA-STATE_COLOR.
      CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
        EXPORTING
          STATE_COLOR = T_DATA-STATE_COLOR
        IMPORTING
          STATE_ICON  = T_DATA-STATE_ICON.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDLOOP.
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX .
      IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
*Job Running DURATION - From START DATE(TIME) to END DATE(TIME)******************
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX .
      ENDDATE  = T_DATA-ENDDATE.
      ENDTIME  = T_DATA-ENDTIME.
      IF ENDDATE <= '00000000'.
        ENDDATE = SY-DATUM.
        ENDTIME = SY-UZEIT.
      ENDIF.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = T_DATA-STRTDATE
          T_FROM      = T_DATA-STRTTIME
          D_TO        = ENDDATE
          T_TO        = ENDTIME
          TIME_UNIT   = 'M'
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        T_DATA-DURATION_M = TIME_DIFF .
        T_DATA-DURATION_H = T_DATA-DURATION_M / 60.
        MODIFY T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
    DELETE T_DATA WHERE DURATION_H NOT IN R_DURATION_H.
    DELETE T_DATA WHERE DURATION_M NOT IN R_DURATION_M.
**********************************************************
*********** DURATION in Time Units : From NOW to JOB Start TIME POINT***************
*-- Fill Duration Value
*-- Calculate  Duration (associating to Reference Field (DATE_REF_FLD)
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX.
**********26-6-16 ********************
      CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
      ASSIGN (FLD) TO <FS>.
      IF <FS> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      REF_DATE = <FS> .
      REF_TIME = T_DATA-STRTTIME.
      CASE LV_DATE_REF_FLD.
        WHEN 'STRTDATE'.                  " Job start date
          REF_TIME = T_DATA-STRTTIME .    " Batch job start time
        WHEN 'ENDDATE'.                   " End date
          REF_TIME = T_DATA-ENDTIME .     " End Time
        WHEN 'SDLDATE'.
          REF_TIME = T_DATA-SDLTIME .     " Time of a scheduled job/step
        WHEN 'SDLSTRTDT'.
          REF_TIME = T_DATA-SDLSTRTTM .   " Planned start time for background Job
        WHEN 'LASTCHDATE'.
          REF_TIME = T_DATA-LASTCHTIME .  " Time of last job change
        WHEN 'RELDATE'.
          REF_TIME = T_DATA-RELTIME .     " Release time of scheduled background job
        WHEN OTHERS.
          REF_TIME = T_DATA-STRTTIME.     " Batch job start time
      ENDCASE.
*****************************************26-6-16
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      "--- Calculate Job's Duration if DATE_REF_FLD is not defined
      LV_D_FROM  = REF_DATE.
      LV_T_FROM  = REF_TIME.
      LV_D_TO    = SY_DATLO.
      LV_T_TO    = SY_TIMLO.
      IF LV_DATE_REF_FLD_ORG IS INITIAL.
        LV_D_FROM  = T_DATA-STRTDATE.
        LV_T_FROM  = T_DATA-STRTTIME.
        LV_D_TO    = T_DATA-ENDDATE.
        LV_T_TO    = T_DATA-ENDTIME.
        IF LV_D_TO <= '00000000'.
          LV_D_TO    = SY_DATLO.
          LV_T_TO    = SY_TIMLO.
        ENDIF.
      ENDIF.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = LV_D_FROM
          T_FROM      = LV_T_FROM
          D_TO        = LV_D_TO
          T_TO        = LV_T_TO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        T_DATA-DURATION = TIME_DIFF .
      ELSE.
        T_DATA-DURATION = '999999'.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX .
    ENDLOOP .
    DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  ENDIF.
******************************************************************
  "--  Add Variant
  IF LV_W_VARIANT IS NOT INITIAL.
    DATA: LS_JOB_STEP TYPE /SKN/S_SW_01_JOB_VARIANT,
          LT_JOB_STEP LIKE TABLE OF LS_JOB_STEP,
          LS_JOB_VARIANT TYPE /SKN/S_SW_01_JOB_VARIANT,
          LT_JOB_VARIANT LIKE TABLE OF LS_JOB_VARIANT.
    LOOP AT T_DATA.
      MOVE-CORRESPONDING T_DATA TO LS_JOB_STEP.
      APPEND LS_JOB_STEP TO LT_JOB_STEP.
    ENDLOOP.
    CALL FUNCTION '/SKN/F_SW_01_GET_JOB_VARIANT'
      EXPORTING
        SW_DEST        = LV_SW_DEST
      TABLES
        IT_JOB_STEP    = LT_JOB_STEP
        ET_JOB_VARIANT = LT_JOB_VARIANT
      EXCEPTIONS
        NO_DATA        = 1
        OTHERS         = 2.
    IF SY-SUBRC = 0.
      SORT LT_JOB_VARIANT BY JOBNAME JOBCOUNT STEPCOUNT.
      LOOP AT T_DATA.
        SY_TABIX = SY-TABIX.
        READ TABLE LT_JOB_VARIANT INTO LS_JOB_VARIANT
                                  WITH KEY JOBNAME   = T_DATA-JOBNAME
                                           JOBCOUNT  = T_DATA-JOBCOUNT
                                           STEPCOUNT = T_DATA-STEPCOUNT
                                  BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          MOVE-CORRESPONDING LS_JOB_VARIANT TO T_DATA.
          MODIFY T_DATA INDEX SY_TABIX .
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
