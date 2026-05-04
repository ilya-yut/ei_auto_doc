# Exception Indicator: IDOCs Count (SW_01_02_IDOCS_CNT)


## General Overview

IDOC (Intermediate Document) is SAP's standard document format for business transaction data transfers. The IDOC interface serves as a translator for business data from external systems, enabling seamless data exchange between different platforms and applications.

IDOCs function as a data formatting tool that enables easy sharing of data between databases and applications within a company (ALE - Application Link Enabling) as well as serving as an efficient data carrier between different companies (EDI - Electronic Data Interchange).

IDOC Structure Components

An IDOC consists of three main components:

1. Control Data Record - Contains the IDOC number, direction ('2' indicates inbound/'1' indicates outbound), type of IDOC, sender/receiver data, and partner port information.

2. Data Records - Contains segment names and actual application data.

3. Status Records - Attached to IDOCs at processing milestones or errors, containing status messages like 'IDOC created', 'successfully passed to port', error descriptions, etc.

This Exception Indicator (EI) provides count-based monitoring of Intermediate Document (IDOC) processing within SAP systems. Unlike detailed IDOC monitoring that shows individual IDOC records, this monitoring solution focuses on volume analysis and threshold-based alerting by counting IDOCs that match specified criteria.

This EI acts as a volume-based integration health indicator that provides IDOC count information for:

Volume threshold monitoring to detect unusual spikes or drops in IDOC processing

High-level integration health assessment without performance overhead of detailed analysis

Capacity planning by tracking IDOC processing volumes over time

SLA compliance monitoring based on processing volume thresholds

The system counts IDOCs matching the specified criteria and triggers alerts when counts fall within defined ranges, enabling proactive monitoring of integration volumes without the overhead of processing detailed IDOC data.


## Problem Description

Abnormal IDOC volumes indicate integration problems causing:

Volume-Based Issues

Unexpected spikes in error volumes indicating system problems

Unusually low processing volumes suggesting interface failures

Processing backlogs exceeding capacity thresholds

Business Process Disruptions

High error counts affecting business transaction processing

Low processing volumes indicating integration downtime

Volume patterns inconsistent with business expectations

System Performance Issues

Processing volume exceeding system capacity

Queue buildups from insufficient processing throughput

Resource constraints limiting IDOC processing rates

Compliance and SLA Concerns

Processing volumes not meeting SLA requirements

Error rates exceeding acceptable thresholds

Integration capacity insufficient for business demands


## Suggested Resolution

Immediate Response

Investigate high error counts using detailed IDOC monitoring (SW_01_02_IDOCS)

Check system capacity when processing volumes are unusually high

Verify interface connectivity when volumes are unexpectedly low

System Assessment

Analyze volume trends to identify patterns or anomalies

Compare current volumes against historical baselines

Assess system capacity against current processing demands

Corrective Actions

Adjust processing parameters to handle volume spikes

Scale system resources to meet capacity requirements

Implement volume-based alerting for proactive monitoring


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 2 | DATE_REF_FLD | Date Ref. Field |  | 0 | 0 |  |  |
| 3 | DATUM | Date |  | 0 | 0 |  |  |
| 4 | DIRECT | Direction |  | 0 | 0 |  |  |
| 5 | DOCNUM | IDOC NUMBER |  | 0 | 0 |  |  |
| 6 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 7 | DURATION_UNIT | Duration Unit(D/H/M) |  | 0 | 0 |  |  |
| 8 | IDOCTP | BASIC TYPE |  | 0 | 0 |  |  |
| 9 | IDOC_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 10 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 11 | MESTYP | MESSAGE TYPE |  | 0 | 0 |  |  |
| 12 | RCVPOR | RECEIVER PORT |  | 0 | 0 |  |  |
| 13 | RCVPRN | RECEIVER PARTNER NO. |  | 0 | 0 |  |  |
| 14 | RCVPRT | RECEIVER PARTNR TYPE |  | 0 | 0 |  |  |
| 15 | SNDPOR | SENDER PORT |  | 0 | 0 |  |  |
| 16 | SNDPRN | SENDER PARTNER NO. |  | 0 | 0 |  |  |
| 17 | SNDPRT | SENDER PARTNER TYPE |  | 0 | 0 |  |  |
| 18 | STATE_COLOR | STATE COLOR |  | 0 | 0 |  |  |
| 19 | STATUS | IDOC STATUS |  | 0 | 0 |  |  |
| 20 | UNAME | User |  | 0 | 0 |  |  |
| 21 | UPDDAT | CHANGED ON |  | 0 | 0 |  |  |
| 22 | UPDTIM | TIME CHANGED |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: This section provides configuration guidance for ALL 22 parameters listed in the Parameters Reference Table above.

BACKDAYS (Back Days):

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

Backdays is based on the DATE_REF_FLD field.

DATE_REF_FLD (Date Ref. Field):

DATE_REF_FLD chooses date context used for date-oriented selection logic.

DATE_REF_FLD Options:

·        DATUM: Use generic date window context.

·        UPDDAT: Use last-change date context.

DATUM (Date):

DATUM provides explicit date-window selection for the counting run.

DIRECT (Direction):

DIRECT limits document/message classification scope that feeds the counted result set.

DOCNUM (IDOC NUMBER):

DOCNUM limits document/message classification scope that feeds the counted result set.

DURATION (Duration In Time Units):

DURATION controls aging threshold used by the base function before counting.

DURATION_UNIT (Duration Unit(D/H/M)):

DURATION_UNIT defines time unit for DURATION evaluation in the underlying IDoc-state logic.

DURATION_UNIT Options:

·        H: Hours

·        M: Minutes

·        D: Days

·        F: Full days for specific day filtering

IDOCTP (BASIC TYPE):

IDOCTP limits document/message classification scope that feeds the counted result set.

IDOC_CNT (Count):

IDOC_CNT is the final count threshold/range selector applied after the base IDoc-state result set is counted.

MANAGE_IN_UTC ('X' - Manage in UTC):

MANAGE_IN_UTC controls UTC vs local-time interpretation for current-time comparisons.

MANAGE_IN_UTC Options:

·        X: UTC mode for distributed landscapes.

·        (blank): Local/system time mode.

MESTYP (MESSAGE TYPE):

MESTYP limits document/message classification scope that feeds the counted result set.

RCVPOR (RECEIVER PORT):

RCVPOR narrows sender/receiver partner-routing scope before final count aggregation.

RCVPRN (RECEIVER PARTNER NO.):

RCVPRN narrows sender/receiver partner-routing scope before final count aggregation.

RCVPRT (RECEIVER PARTNR TYPE):

RCVPRT narrows sender/receiver partner-routing scope before final count aggregation.

SNDPOR (SENDER PORT):

SNDPOR narrows sender/receiver partner-routing scope before final count aggregation.

SNDPRN (SENDER PARTNER NO.):

SNDPRN narrows sender/receiver partner-routing scope before final count aggregation.

SNDPRT (SENDER PARTNER TYPE):

SNDPRT narrows sender/receiver partner-routing scope before final count aggregation.

STATE_COLOR (STATE COLOR):

STATE_COLOR selects severity/state groups and can drive derived status selection.

STATE_COLOR Options:

·        R: Red severity state.

·        G: Green severity state.

·        Y: Yellow severity state.

·        Standard SAP mapping applies when literals are not explicitly listed inline.

STATUS (IDOC STATUS):

STATUS explicitly selects IDoc lifecycle states to include before count aggregation.

UNAME (User):

UNAME narrows counting scope to user-context ownership segments.

UPDDAT (CHANGED ON):

UPDDAT constrains selection by last-change date in the base result set.

UPDTIM (TIME CHANGED):

UPDTIM refines the last-change time window used before counting.


### Parameter Relationship

How parameter combinations work together

Date and Time Controls:

·        BACKDAYS is fallback when explicit date input is missing.

·        DATUM and UPDDAT/UPDTIM provide explicit temporal scope before counting.

Duration and Count Logic:

·        DURATION + DURATION_UNIT filter aged IDoc records in the base function.

·        DURATION + DURATION_UNIT are an additional (second) filter applied after date selection.

·        The wrapper then counts remaining rows and compares the result to IDOC_CNT.

·        Final alert logic depends on both base filters and count threshold match.

State and Status Selection:

·        STATE_COLOR and STATUS work together to define process-state inclusion.

·        Message/document filters (for example MESTYP, IDOCTP, DIRECT) shape what is counted.


### Default Values

·        DURATION_UNIT - M

·        ONLY_HEADER - X


### Practical Example of Parameter Configuration

Use Case 1: Failed IDoc surge in short interval

STATUS = 51
 BACKDAYS = 1
 DURATION = 10
 DURATION_UNIT = M
 IDOC_CNT = 20 - 999999



Purpose: Trigger when at least 20 failed IDocs accumulate in the recent short monitoring window.

Use Case 2: Outbound message-type count breach

DIRECT = 2
 MESTYP = ORDERS
 UPDDAT = 20260331-20260331
 IDOC_CNT = 50 - 999999



Purpose: Detect high-volume same-day outbound IDoc issues for one message type.

Use Case 3: State-based threshold monitoring with UTC

STATE_COLOR = R
 MANAGE_IN_UTC = X
 DURATION = 1
 DURATION_UNIT = H
 IDOC_CNT = 10 - 999999



Purpose: Count red-state IDocs in a UTC-consistent window and escalate when threshold is exceeded.


## EI Function Structure

This table lists all output fields returned by the EI.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_02_IDOCS | ARCKEY | EDI archive key | CHAR(70) | IDOCCARKEY |
| /SKN/S_SW_01_02_IDOCS | CIMTYP | Extension | CHAR(30) | EDI_CIMTYP |
| /SKN/S_SW_01_02_IDOCS | COUNTR | IDoc status counter | NUMC(16) | EDI_COUNTR |
| /SKN/S_SW_01_02_IDOCS | CREDAT | IDoc Created On | DATS(8) | EDI_CCRDAT |
| /SKN/S_SW_01_02_IDOCS | CRETIM | IDoc Created at | TIMS(6) | EDI_CCRTIM |
| /SKN/S_SW_01_02_IDOCS | DIRECT | Direction for IDoc | CHAR(1) | EDI_DIRECT |
| /SKN/S_SW_01_02_IDOCS | DOCNUM | IDoc number | NUMC(16) | EDI_DOCNUM |
| /SKN/S_SW_01_02_IDOCS | DOCREL | SAP Release for IDoc | CHAR(4) | EDI_DOCREL |
| /SKN/S_SW_01_02_IDOCS | DOCTYP | IDoc Type | CHAR(8) | EDI_DOCTYP |
| /SKN/S_SW_01_02_IDOCS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_02_IDOCS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_02_IDOCS | EXPRSS | Overriding in inbound processing | CHAR(1) | EDI_EXPRSS |
| /SKN/S_SW_01_02_IDOCS | IDOCTP | Basic type | CHAR(30) | EDI_IDOCTP |
| /SKN/S_SW_01_02_IDOCS | LOGDAT | Date of status information | DATS(8) | EDI_LOGDAT |
| /SKN/S_SW_01_02_IDOCS | LOGTIM | Time of status information | TIMS(6) | EDI_LOGTIM |
| /SKN/S_SW_01_02_IDOCS | MAXSEGNUM | Number of data records | NUMC(6) | ANZEDIDD |
| /SKN/S_SW_01_02_IDOCS | MESCOD | Logical Message Variant | CHAR(3) | EDI_MESCOD |
| /SKN/S_SW_01_02_IDOCS | MESFCT | Logical message function | CHAR(3) | EDI_MESFCT |
| /SKN/S_SW_01_02_IDOCS | MESSAGE | Message Text | CHAR(220) | BAPI_MSG |
| /SKN/S_SW_01_02_IDOCS | MESTYP | Message Type | CHAR(30) | EDI_MESTYP |
| /SKN/S_SW_01_02_IDOCS | OUTMOD | Output Mode | CHAR(1) | EDI_OUTMOD |
| /SKN/S_SW_01_02_IDOCS | PROCESS_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_02_IDOCS | RCVLAD | Logical address of recipient | CHAR(70) | EDI_RCVLAD |
| /SKN/S_SW_01_02_IDOCS | RCVPFC | Partner Function of Receiver | CHAR(2) | EDI_RCVPFC |
| /SKN/S_SW_01_02_IDOCS | RCVPOR | Receiver port (SAP System, EDI subsystem) | CHAR(10) | EDI_RCVPOR |
| /SKN/S_SW_01_02_IDOCS | RCVPRN | Partner Number of Receiver | CHAR(10) | EDI_RCVPRN |
| /SKN/S_SW_01_02_IDOCS | RCVPRT | Partner Type of Receiver | CHAR(2) | EDI_RCVPRT |
| /SKN/S_SW_01_02_IDOCS | RCVSAD | Recipient address (SADR) | CHAR(10) | EDI_RCVSAD |
| /SKN/S_SW_01_02_IDOCS | RCVSCA | Communication type (SADR) of recipient | CHAR(3) | EDI_RCVSCA |
| /SKN/S_SW_01_02_IDOCS | RCVSDF | SADR default flag for recipient address | CHAR(1) | EDI_RCVSDF |
| /SKN/S_SW_01_02_IDOCS | RCVSLF | Sequential Number of Recipient Address (SADR) | CHAR(3) | EDI_RCVSLF |
| /SKN/S_SW_01_02_IDOCS | RCVSMN | SADR client (recipient) | CLNT(3) | EDI_RCVSMN |
| /SKN/S_SW_01_02_IDOCS | RCVSNA | SADR flag for international recipient address | CHAR(1) | EDI_RCVSNA |
| /SKN/S_SW_01_02_IDOCS | REFGRP | Reference to message group | CHAR(14) | IDOCCRFGRP |
| /SKN/S_SW_01_02_IDOCS | REFINT | Reference to interchange file | CHAR(14) | IDOCCRFINT |
| /SKN/S_SW_01_02_IDOCS | REFMES | Reference to message | CHAR(14) | IDOCCRFMES |
| /SKN/S_SW_01_02_IDOCS | REPID | Program Name | CHAR(30) | EDI_REPID |
| /SKN/S_SW_01_02_IDOCS | ROUTID | Name of subroutine (routine, function module) | CHAR(30) | EDI_ROUTID |
| /SKN/S_SW_01_02_IDOCS | SEGFLD | Field Name in SAP Segment | CHAR(30) | EDI_SEGFLD |
| /SKN/S_SW_01_02_IDOCS | SEGNUM | Number of SAP segment | NUMC(6) | IDOCSSGNUM |
| /SKN/S_SW_01_02_IDOCS | SERIAL | Serialization field | CHAR(20) | EDI_SERIAL |
| /SKN/S_SW_01_02_IDOCS | SNDLAD | Logical address of sender | CHAR(70) | EDI_SNDLAD |
| /SKN/S_SW_01_02_IDOCS | SNDPFC | Partner Function of Sender | CHAR(2) | EDI_SNDPFC |
| /SKN/S_SW_01_02_IDOCS | SNDPOR | Sender port (SAP System, EDI subsystem) | CHAR(10) | EDI_SNDPOR |
| /SKN/S_SW_01_02_IDOCS | SNDPRN | Partner Number of Sender | CHAR(10) | EDI_SNDPRN |
| /SKN/S_SW_01_02_IDOCS | SNDPRT | Partner type of sender | CHAR(2) | EDI_SNDPRT |
| /SKN/S_SW_01_02_IDOCS | SNDSAD | Sender address (SADR) | CHAR(10) | EDI_SNDSAD |
| /SKN/S_SW_01_02_IDOCS | SNDSCA | Communication type (SADR) of sender | CHAR(3) | EDI_SNDSCA |
| /SKN/S_SW_01_02_IDOCS | SNDSDF | SADR default flag for sender address | CHAR(1) | EDI_SNDSDF |
| /SKN/S_SW_01_02_IDOCS | SNDSLF | Sequential Number of the Sender Address (SADR) | CHAR(3) | EDI_SNDSLF |
| /SKN/S_SW_01_02_IDOCS | SNDSMN | SADR client (sender) | CLNT(3) | EDI_SNDSMN |
| /SKN/S_SW_01_02_IDOCS | SNDSNA | SADR flag for international sender address | CHAR(1) | EDI_SNDSNA |
| /SKN/S_SW_01_02_IDOCS | STACOD | Status code | CHAR(8) | EDI_STACOD |
| /SKN/S_SW_01_02_IDOCS | STAMID | Status message ID | CHAR(20) | EDI_STAMID |
| /SKN/S_SW_01_02_IDOCS | STAMNO | Status message number | NUMC(3) | EDI_STAMNO |
| /SKN/S_SW_01_02_IDOCS | STAMQU | Status message qualifier | CHAR(3) | EDI_STAMQU |
| /SKN/S_SW_01_02_IDOCS | STAPA1 | Parameter 1 | CHAR(50) | EDI_STAPA1 |
| /SKN/S_SW_01_02_IDOCS | STAPA2 | Parameter 2 | CHAR(50) | EDI_STAPA2 |
| /SKN/S_SW_01_02_IDOCS | STAPA3 | Parameter 3 | CHAR(50) | EDI_STAPA3 |
| /SKN/S_SW_01_02_IDOCS | STAPA4 | Parameter 4 | CHAR(50) | EDI_STAPA4 |
| /SKN/S_SW_01_02_IDOCS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_02_IDOCS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_02_IDOCS | STATUS | Status of IDoc | CHAR(2) | EDI_STATUS |
| /SKN/S_SW_01_02_IDOCS | STATUS_DESC | Short description of object | CHAR(60) | EDI_TEXT60 |
| /SKN/S_SW_01_02_IDOCS | STATXT | Text for status code | CHAR(70) | EDI_STATX_ |
| /SKN/S_SW_01_02_IDOCS | STATYP | Type of system error message (A, W, E, S, I) | CHAR(1) | EDI_SYMSTY |
| /SKN/S_SW_01_02_IDOCS | STD | EDI Standard | CHAR(1) | EDI_STD |
| /SKN/S_SW_01_02_IDOCS | STDMES | EDI message type | CHAR(6) | EDI_STDMES |
| /SKN/S_SW_01_02_IDOCS | STDVRS | Version of EDI standard | CHAR(6) | EDI_STDVRS |
| /SKN/S_SW_01_02_IDOCS | TEST | Test Flag | CHAR(1) | EDI_TEST |
| /SKN/S_SW_01_02_IDOCS | TID | Transaction ID | CHAR(24) | EDI_TID |
| /SKN/S_SW_01_02_IDOCS | UNAME | User name | CHAR(12) | EDI_UNAME |
| /SKN/S_SW_01_02_IDOCS | UPDDAT | Date on which control record was last changed | DATS(8) | EDI_UPDDAT |
| /SKN/S_SW_01_02_IDOCS | UPDTIM | Time at which control record was last changed | TIMS(6) | EDI_UPDTIM |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_02_IDOCS_CNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_IDOCS_CNT OPTIONAL
*"----------------------------------------------------------------------
DATA : LV_ALERT TYPE  CHAR1.
DATA : LS_DATA TYPE /SKN/S_SW_01_02_IDOCS,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA : LV_CNT TYPE I.
DATA_MULTY: IDOC_CNT /SKN/E_SW_CNT.
SELECT_MULTY: IDOC_CNT.
"=== Add <ONLY_HEADER> parameter.
DATA: LS_SELECT LIKE LINE OF T_SELECT.
      LS_SELECT-FIELDNM = 'ONLY_HEADER'.
       LS_SELECT-SIGN = 'I'.
        LS_SELECT-OPTION = 'EQ'.
         LS_SELECT-LOW = 'X'.
      APPEND LS_SELECT TO T_SELECT.
"=== Add "ONLY_HEADER " parameter.
   REFRESH T_DATA.
   CALL FUNCTION '/SKN/F_SW_01_02_IDOCS'
    IMPORTING
       IS_ALERT       = LV_ALERT
     TABLES
       T_SELECT       = T_SELECT
       T_DATA         = LT_DATA.
    IS_ALERT = LV_ALERT.
    DESCRIBE TABLE LT_DATA LINES LV_CNT.
    IF LV_CNT IN R_IDOC_CNT.
      T_DATA-IDOC_CNT = LV_CNT.
      APPEND T_DATA.
      IS_ALERT = LV_ALERT.
    ELSE.
      CLEAR IS_ALERT.
    ENDIF.
ENDFUNCTION.
```
