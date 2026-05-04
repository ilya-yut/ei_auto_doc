# Exception Indicator: tRFC Transactional RFC Monitoring (SW_01_02_TRFC)


## General Overview

This Exception Indicator (EI) provides comprehensive monitoring and analysis of SAP tRFC (transactional Remote Function Call) processing to ensure reliable system integration and data transfer between distributed SAP systems and external applications. tRFC technology enables guaranteed asynchronous communication where function calls are executed exactly once in the target system, making it essential for business-critical integration scenarios where data integrity and processing reliability must be ensured.

tRFC (transactional RFC) extends the standard RFC mechanism by providing transaction-safe communication with guaranteed delivery and execution. Unlike standard RFC calls, tRFC ensures that function modules are executed exactly once and in the correct sequence, even in cases of system failures or network interruptions. This is crucial for business-critical processes where data consistency and processing reliability are paramount.

This Exception Indicator provides detailed tRFC monitoring capabilities that enable:

Transactional call tracking to monitor the status and performance of guaranteed RFC processing

LUW (Logical Unit of Work) analysis for understanding transaction processing patterns and identifying stuck operations


```abap
Function module monitoring to track specific RFC function calls and their execution status across systems
Transaction ID analysis for detailed tracing and troubleshooting of individual tRFC transactions
Performance assessment for identifying processing delays and system bottlenecks in transactional operations
The monitoring solution analyzes tRFC statistics from the ARFCSSTATE table (outbound tRFC calls), similar to data available through the SM58 transaction (Transactional RFC Error Log), and provides enhanced filtering capabilities to focus on specific function modules, states, users, and time periods. This enables targeted analysis of tRFC performance characteristics and identification of integration-related issues.
This Exception Indicator checks whether SAP tRFC processing is functioning efficiently and identifies potential issues that may impact distributed system communication, data integrity, and business process execution.
Problem Description
Poor tRFC performance and processing failures indicate system integration and communication issues causing:
Integration and Communication Problems
Failed transactional function calls preventing critical data transfer and business process execution
Stuck LUWs (Logical Units of Work) causing data processing delays and system performance degradation
Communication errors between distributed systems leading to transaction failures and data inconsistency
Function module execution failures disrupting automated business workflows and integration scenarios
System Performance Issues
Resource contention from excessive tRFC processing workload affecting overall system performance
Memory consumption issues from accumulated failed transactions impacting available system resources
Processing delays causing bottlenecks in time-critical business operations and automated processes
Database locking issues from long-running tRFC transactions affecting system responsiveness
Business Impact
Data consistency problems due to failed or incomplete tRFC processing affecting business operations
SLA violations from processing delays impacting service level commitments and operational efficiency
Business process interruptions from unreliable system-to-system communication affecting operational continuity
Potential data loss or corruption from tRFC processing failures compromising business data integrity
Suggested Resolution
Immediate Response
Investigate failed and stuck tRFC entries using SM58 transaction for detailed error analysis and resolution
Check system resource utilization and memory consumption during tRFC processing periods for capacity assessment
Review tRFC error logs and system messages for root cause identification and pattern analysis
Analyze function module execution patterns and identify frequently failing operations for targeted remediation
System Assessment
Monitor tRFC processing performance trends and LUW completion patterns for optimization opportunities
Evaluate system sizing and resource allocation for tRFC processing workload management
Check network connectivity and RFC destination configuration for reliability optimization
Analyze tRFC processing patterns by function module, user, and system for performance tuning
Corrective Actions
Optimize tRFC processing parameters and system configurations for improved reliability and performance
Implement tRFC error handling and retry mechanisms for enhanced fault tolerance and recovery
Establish proactive tRFC monitoring and alerting procedures for early issue detection and prevention
Plan system capacity upgrades based on tRFC volume analysis and processing requirements
Parameters
Parameters Reference Table
This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.
#
Field
Description
Type
Length
Decimal
Data Element
Domain
1
ARFCDATUM
Date
DATS
8
0
SYDATUM
SYDATS
2
ARFCDEST
Destination
CHAR
32
0
RFCDEST
RFCDEST
3
ARFCFNAM
RFC FM
CHAR
30
0
RS38L_FNAM
FUNCNAME
4
ARFCIPID
Host ID
CHAR
8
0
ARFCIPID
ARFCIPID
5
ARFCLUWCNT
Counter
CHAR
8
0
ARFCLUWCNT
ARFCLUWCNT
6
ARFCMSG
Message Variable
CHAR
50
0
SYMSGV
SYCHAR50
7
ARFCPID
Process ID
CHAR
4
0
ARFCPID
ARFCPID
8
ARFCRESERV
Horizontal Line
CHAR
255
0
SYULINE
SYCHAR255
9
ARFCRETRYS
No. of attempts
NUMC
4
0
RETRY
NUM04
10
ARFCRETURN
Reply
CHAR
1
0
ARFCRETURN
SYST_FLAG
11
ARFCRHOST
char8
CHAR
8
0
RFCCHAR8
RFCCHAR8
12
ARFCSTATE
Status
CHAR
8
0
ARFCSTATE
ARFCSTATE
13
ARFCTCODE
Transaction code
CHAR
20
0
SYTCODE
TCODE
14
ARFCTIDCNT
Counter
CHAR
4
0
ARFCTIDCNT
ARFCTIDCNT
15
ARFCTIME
Time Stamp
CHAR
8
0
ARFCTIME
ARFCTIME
16
ARFCUSER
User
CHAR
12
0
SYUNAME
SYCHAR12
17
ARFCUZEIT
Time
TIMS
6
0
SYUZEIT
SYTIME
18
DURATION
Duration In Time Units
INT4
10
0
/SKN/E_SW_DURATION
19
DURATION_UNIT
Duration Unit(D/H/M)
CHAR
1
0
/SKN/E_SW_DURATION_UNIT
/SKN/D_SW_DURATION_UNIT
20
STATE_COLOR
State Color
CHAR
1
0
/SKN/E_SW_STATE_COLOR
/SKN/D_SW_STATE_COLOR
21
STATE_ICON
State Icon
CHAR
4
0
/SKN/E_SW_STATE_ICON
ICON
22
SW_DEST
Cloud Destination
0
0
23
USER_FLD
Dynamic Recipient User Field
0
0
Parameter Configuration Guidelines
IMPORTANT: Configure ALL 23 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.
ARFCDATUM (Date)
Stabilizes week-over-week metrics by fixing date (ARFCDATUM) while allowing duration thresholds to move.
ARFCDEST (Destination)
Connects to alert semantics: rows removed for failing destination on ARFCDEST never reach downstream filtering.
ARFCFNAM (RFC FM)
Reflects real administration where rfc fm on ARFCFNAM is routinely restricted to a single productive client or object family.
ARFCIPID (Host ID)
Documents expected operator behavior—host id on ARFCIPID should be set when that dimension is part of the control objective.
ARFCLUWCNT (Counter)
For operations, counter on ARFCLUWCNT indicates whether a row belongs in the current monitoring pass versus historical noise.
ARFCMSG (Message Variable)
Reduces false positives during peak windows by tightening message variable through ARFCMSG alongside state filters.
ARFCPID (Process ID)
Interprets process id as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on ARFCPID.
ARFCRESERV (Horizontal Line)
When combined with destination discipline, horizontal line on ARFCRESERV keeps both breadth and depth of the extract intentional.
ARFCRETRYS (No. of attempts)
Stabilizes week-over-week metrics by fixing no. of attempts (ARFCRETRYS) while allowing duration thresholds to move.
ARFCRETURN (Reply)
Prevents accidental global scans when reply (ARFCRETURN) is meant to stay within a controlled application slice.
ARFCRHOST (char8)
Supports escalation where char8 on ARFCRHOST signals ownership for follow-up between Basis and functional teams.
ARFCSTATE (Status)
When left open per framework rules, ARFCSTATE does not restrict status; when set, only matching rows remain.
ARFCTCODE (Transaction code)
Narrows retrieved rows where transaction code (ARFCTCODE) must match the configured selection for this monitor.
ARFCTIDCNT (Counter)
Helps distinguish technical versus business attributes when counter on ARFCTIDCNT correlates with counters or status fields.
ARFCTIME (Time Stamp)
Gives auditors traceable criteria because time stamp on ARFCTIME is applied consistently before any alert flag is raised.
ARFCUSER (User)
Helps distinguish technical versus business attributes when user on ARFCUSER correlates with counters or status fields.
ARFCUZEIT (Time)
For operations, time on ARFCUZEIT indicates whether a row belongs in the current monitoring pass versus historical noise.
DURATION (Duration In Time Units)
Reflects real administration where duration in time units on DURATION is routinely restricted to a single productive client or object family.
DURATION_UNIT (Duration Unit(D/H/M))
Unit for elapsed time between each session's creation date and time and the evaluation clock.
DURATION_UNIT Options:
·        H — Hours.
·        M — Minutes (preset in code before the selection read when not overridden).
·        D — Days.
·        F — Full-day style counting where applicable to the duration helper.
STATE_COLOR (State Color)
Filters lines by the derived color bucket used for severity-style triage in the monitor framework.
STATE_COLOR Options:
·        R — Red (error or failed-style outcomes).
·        G — Green (successful outcomes).
·        Y — Yellow (warning or in-process outcomes).
·        Additional literals may exist where the framework extends the palette for neutral states.
STATE_ICON (State Icon)
Reflects real administration where state icon on STATE_ICON is routinely restricted to a single productive client or object family.
SW_DEST (Cloud Destination)
Pairs with duration logic: once SW_DEST passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.
USER_FLD (Dynamic Recipient User Field)
The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.
How DRL Works:
When USER_FLD is specified, the system extracts values from that field in the monitoring result set
These extracted values are then used as recipient addresses for alert notifications
This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored
The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users
Default Values
·        DURATION - initial - treated as unset by code (the duration interval filter does not remove rows until a populated duration range is supplied on selection).
·        DURATION_UNIT - initial - treated as M by code (minutes preset on the duration unit variable before selection values are read).
Practical Example of Parameter Configuration
Use Case 1: Destination under stress
Purpose: Watch one RFC destination with a short age window and visible error coloring.
ARFCDEST = PROD_BW
 DURATION = 60
 DURATION_UNIT = M
 STATE_COLOR = R
 ARFCSTATE = SYSFAIL
 SW_DEST = CLOUD_CONN


Use Case 2: Function module focus
Purpose: Track a specific remote-enabled module across users with a broader date span.
ARFCFNAM = ZMM_IF_SEND
 ARFCDATUM = 20260101-20260131
 ARFCUSER = BATCHRFC
 ARFCTCODE = SM58
 STATE_ICON = ICON_MESSAGE_ERROR
 USER_FLD = EMAIL_ADDR


Use Case 3: Wide discovery with technical slice
Purpose: Sample many dimensions while still bounding time and duration for a nightly review.
ARFCDEST = %PRD%
 ARFCIPID = APPHOST1
 ARFCRETRYS = 0003-0009
 DURATION = 240
 DURATION_UNIT = M
 STATE_COLOR = Y
 ARFCMSG = TIMEOUT
 ARFCLUWCNT = 00000001
 ARFCPID = 1234


EI Function Structure
This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.
Structure Name
Field Name
Description
Data Type
Component Type
/SKN/S_SW_01_02_TRFC
ARFCDATUM
System Date
DATS(8)
SYDATUM
/SKN/S_SW_01_02_TRFC
ARFCDEST
Logical Destination (Specified in Function Call)
CHAR(32)
RFCDEST
/SKN/S_SW_01_02_TRFC
ARFCFNAM
Name of Function Module
CHAR(30)
RS38L_FNAM
/SKN/S_SW_01_02_TRFC
ARFCIPID
Host ID (IP ID
CHAR(8)
ARFCIPID
/SKN/S_SW_01_02_TRFC
ARFCLUWCNT
Counter within a transaction (LUW)
CHAR(8)
ARFCLUWCNT
/SKN/S_SW_01_02_TRFC
ARFCMSG
Message Variable
CHAR(50)
SYMSGV
/SKN/S_SW_01_02_TRFC
ARFCPID
Process ID
CHAR(4)
ARFCPID
/SKN/S_SW_01_02_TRFC
ARFCRESERV
Horizontal Line
CHAR(255)
SYULINE
/SKN/S_SW_01_02_TRFC
ARFCRETRYS
No. of attempts
NUMC(4)
RETRY
/SKN/S_SW_01_02_TRFC
ARFCRETURN
Reply expected
CHAR(1)
ARFCRETURN
/SKN/S_SW_01_02_TRFC
ARFCRHOST
RFCCHAR8
CHAR(8)
RFCCHAR8
/SKN/S_SW_01_02_TRFC
ARFCSTATE
Status of an ARFC call (RECORDED,CPICERR,MAILED,READ..)
CHAR(8)
ARFCSTATE
/SKN/S_SW_01_02_TRFC
ARFCTCODE
Transaction Code
CHAR(20)
SYTCODE
/SKN/S_SW_01_02_TRFC
ARFCTIDCNT
Transaction ID (LUW -> COMMIT WORK)
CHAR(4)
ARFCTIDCNT
/SKN/S_SW_01_02_TRFC
ARFCTIME
Time Stamp
CHAR(8)
ARFCTIME
/SKN/S_SW_01_02_TRFC
ARFCUSER
User Name
CHAR(12)
SYUNAME
/SKN/S_SW_01_02_TRFC
ARFCUZEIT
System Time
TIMS(6)
SYUZEIT
/SKN/S_SW_01_02_TRFC
DURATION
SW: Duration In Time Units (defined separatly)
INT4(10)
/SKN/E_SW_DURATION
/SKN/S_SW_01_02_TRFC
DURATION_UNIT
SW: Duration Unit
CHAR(1)
/SKN/E_SW_DURATION_UNIT
/SKN/S_SW_01_02_TRFC
STATE_COLOR
SW: State Color
CHAR(1)
/SKN/E_SW_STATE_COLOR
/SKN/S_SW_01_02_TRFC
STATE_ICON
SW: State Icon
CHAR(4)
/SKN/E_SW_STATE_ICON
ABAP Code
FUNCTION /SKN/F_SW_01_02_TRFC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_TRFC OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_ARFCSTATE FOR ARFCSSTATE-ARFCSTATE,
         R_ARFCUSER  FOR ARFCSSTATE-ARFCUSER,
         R_ARFCDEST  FOR ARFCSSTATE-ARFCDEST,
         R_ARFCFNAM  FOR ARFCSSTATE-ARFCFNAM,
         R_ARFCDATUM FOR ARFCSSTATE-ARFCDATUM,
         R_ARFCUZEIT FOR ARFCSSTATE-ARFCUZEIT.
RANGES : R_STATE_COLOR FOR /SKN/S_SW_SYS_RFC_PING-STATE_COLOR,
         R_DATUM   FOR SY-DATUM .
DATA : DATE_FROM LIKE SY-DATUM ,
       BACKDAYS  TYPE I .
DATA : LANGU LIKE SY-LANGU .
DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
*
* define all known statuses
*
DATA :
       RECORDED LIKE ARFCSSTATE-ARFCSTATE VALUE 'RECORDED',
       CPICERR  LIKE ARFCSSTATE-ARFCSTATE VALUE 'CPICERR',
       SYSFAIL  LIKE ARFCSSTATE-ARFCSTATE VALUE 'SYSFAIL',
       CONFAIL  LIKE ARFCSSTATE-ARFCSTATE VALUE 'NO_CONF',
       EXECUTED LIKE ARFCSSTATE-ARFCSTATE VALUE 'EXECUTED',
       MAILED   LIKE ARFCSSTATE-ARFCSTATE VALUE 'MAILED',
       READ     LIKE ARFCSSTATE-ARFCSTATE VALUE 'READ',
       VBRECORD LIKE ARFCSSTATE-ARFCSTATE VALUE 'VBRECORD',
       SENDED   LIKE ARFCSSTATE-ARFCSTATE VALUE 'SENDED',
       DEBUG    LIKE ARFCSSTATE-ARFCSTATE VALUE 'DEBUG',
       VXRECORD LIKE ARFCSSTATE-ARFCSTATE VALUE 'VXRECORD',
       CONFAIL1 LIKE ARFCSSTATE-ARFCSTATE VALUE 'CONFAIL'.
DATA: BEGIN OF %_RESERV,               "Rel. Adr.
        CPROG        LIKE SY-CPROG,                          "40       0
        MANDT        LIKE SY-MANDT,                          "3       40
        BATCHPLA     TYPE C,                                 "1       43
        QRFCFLAG     TYPE C,                                 "1       44
        QNAME        LIKE TRFCQIN-QNAME,                     "24      45
        QCOUNT       LIKE TRFCQIN-QCOUNT,                    "24      69
        QRCVTID      LIKE ARFCTID,                           "24      93
        ARFCMSG2(23) TYPE C,                                 "23     117
        CHNGTRFC     TYPE C,                                 "1      140
        ORGHOST(20)  TYPE C,                                 "20     141
        ASGROUP(20)  TYPE C,                                 "20     161
        QTRACE       TYPE C,                                 "1      181
        QACTION      TYPE C,                                 "1      182
        QDATE        LIKE SY-DATUM,                          "8      183
        QTIME        LIKE SY-UZEIT,                          "6      191
        QLOG         TYPE C,                                 "1      197
        PACTION      TYPE C,                                 "1      198
        QNOEXEC      TYPE C,                                 "1      199
        USEQLIST     TYPE C,                                 "1      200
        QRCVNEW      TYPE C,                                 "1      201
        QNOSEND      TYPE C,                                 "1      202
        NRDATA(8)    TYPE N,                                 "8      203
        VBERRKEY(32) TYPE C,                                 "32     211
        LANGU        LIKE SY-LANGU.                          "1      243
                                                             "       244
                                     " hier kצnnnen bis max. 255 insges.
DATA: END OF %_RESERV.               " weitere Eintrהge abgesp. werden
DATA: STATUS_DESC TYPE  EDI_TEXT60.
DATA : WA TYPE ARFCSSTATE.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : TIME_DIFF TYPE  INT4 .
DATA : IS_OUT(1) TYPE C.
*-- Fill Selection Option Tables
  SELECT_MULTY: DURATION.
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_02_TRFC'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCDEST'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCDEST.
     APPEND R_ARFCDEST.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCSTATE'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCSTATE.
     APPEND R_ARFCSTATE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCUSER'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCUSER.
     APPEND R_ARFCUSER.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCFNAM'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCFNAM.
     APPEND R_ARFCFNAM.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCDATUM'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCDATUM.
     APPEND R_ARFCDATUM.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ARFCUZEIT'.
     MOVE-CORRESPONDING T_SELECT TO R_ARFCUZEIT.
     APPEND R_ARFCUZEIT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
     MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
     APPEND R_STATE_COLOR.
   ENDLOOP.
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
   IF R_ARFCDATUM[] IS INITIAL.
     R_ARFCDATUM[] = R_DATUM[].
   ENDIF.
  "-----
  LANGU = SY-LANGU.
  LOOP AT T_SELECT WHERE FIELDNM = 'LANGU'.
    LANGU = T_SELECT-LOW.
    EXIT.
  ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
SELECT * FROM ARFCSSTATE
   INTO WA
   WHERE ARFCSTATE  IN R_ARFCSTATE
     "and  ARFCSTATE IN (CONFAIL1, SYSFAIL, CPICERR , RECORDED)
     AND ARFCUSER   IN R_ARFCUSER
     AND ARFCDEST   IN R_ARFCDEST
     AND ARFCFNAM   IN R_ARFCFNAM
     AND ARFCDATUM  IN R_ARFCDATUM
     AND ARFCUZEIT  IN R_ARFCUZEIT
     AND ARFCRETURN  = SPACE.
  %_RESERV = WA-ARFCRESERV.
  IF %_RESERV-QRFCFLAG <> SPACE.
    "qRFC-LUWs exclude
  ELSE.
    MOVE-CORRESPONDING WA TO T_DATA.
    APPEND T_DATA.
  ENDIF.
ENDSELECT.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
     CALL FUNCTION '/SKN/F_SW_01_02_TRFC_STATUS'
       EXPORTING
         STATUS            = T_DATA-ARFCSTATE
         LANGU             = LANGU
       IMPORTING
         STATUS_DESC       = STATUS_DESC
         STATE_COLOR       = T_DATA-STATE_COLOR.
     CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
       EXPORTING
         STATE_COLOR       = T_DATA-STATE_COLOR
       IMPORTING
         STATE_ICON        = T_DATA-STATE_ICON         .
     IF T_DATA-ARFCMSG IS INITIAL.
       T_DATA-ARFCMSG = STATUS_DESC.
     ENDIF.
     MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*  loop at t_data .
*    sy_tabix = sy-tabix .
*    clear is_out.
*    if not t_data-ARFCDATUM is initial.
*      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*        EXPORTING
*          D_FROM            = t_data-ARFCDATUM
*          T_FROM            = t_data-ARFCUZEIT
*          D_TO              = sy-datum
*          T_TO              = sy-uzeit
*          TIME_UNIT         = 'M'
*        IMPORTING
*          TIME_DIFF         = TIME_DIFF
*        EXCEPTIONS
*          WRONG_VALUE       = 1
*          OTHERS            = 2    .
*      IF SY-SUBRC = 0.
*        t_data-DURATION_M = TIME_DIFF .
*        t_data-DURATION_H = t_data-DURATION_M / 60.
*        if not t_data-DURATION_H in R_DURATION_H .
*          is_out = 'X'.
*        endif.
*        if not t_data-DURATION_M in R_DURATION_M .
*          is_out = 'X'.
*        endif.
*       if not is_out is initial.
*         delete t_data index sy_tabix .
*       else.
*         modify t_data index sy_tabix.
*       endif.
*      ENDIF.
*    endif.
*  endloop.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-ARFCDATUM
          T_FROM            = T_DATA-ARFCUZEIT
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
