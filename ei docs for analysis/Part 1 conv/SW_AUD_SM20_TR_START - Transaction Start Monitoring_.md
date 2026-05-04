# Exception Indicator: Transaction Start Monitoring (SW_AUD_SM20_TR_START)


## General Overview

Transaction Start Monitoring is a critical security and auditing system indicator that provides comprehensive analysis of user transaction initiation patterns and security events within the SAP environment. This monitoring solution is designed to track and analyze transaction start activities from the Security Audit Log (SM20), enabling administrators to monitor user behavior, detect security anomalies, and ensure compliance with security policies.

The transaction start monitoring functionality serves as an essential tool for security administrators and auditors to identify unauthorized access attempts, unusual user activity patterns, and potential security breaches. By analyzing historical transaction start data, administrators can proactively address security risks before they impact business operations and data integrity.

This Exception Indicator provides detailed transaction start analysis capabilities that enable:

Security event monitoring to identify unauthorized transaction access and usage patterns

User activity analysis for compliance auditing and behavioral pattern detection

Transaction usage evaluation to pinpoint specific security areas requiring attention

Historical audit pattern analysis for understanding user behavior and access trends

The monitoring solution analyzes transaction start statistics from security audit log tables, similar to data available through the SM20 transaction (Security Audit Log), and provides enhanced filtering capabilities to focus on specific time periods, transaction types, users, and security thresholds. This enables targeted analysis of security characteristics and identification of security anomalies.

This Exception Indicator checks whether SAP system transaction start activities are within acceptable security parameters and identifies potential security issues that may impact system integrity and compliance requirements.


## Problem Description

Unusual transaction start patterns and suspicious user activities indicate potential security vulnerabilities causing:

Security Issues

Unauthorized transaction access affecting system security and data protection

Suspicious user activity patterns causing security compliance violations

Transaction abuse scenarios impacting system integrity and audit trails

Unusual access times and patterns indicating potential security breaches

Compliance Violations

Audit trail gaps leading to compliance failures and regulatory issues

User access violations causing policy breaches and security incidents

Transaction monitoring failures impacting security oversight and control

Documentation deficiencies affecting audit requirements and compliance reporting

Business Impact

Security incident escalation due to delayed detection of unauthorized activities

Compliance violations due to inadequate transaction monitoring and reporting

Data protection risks from unmonitored user activities and transaction patterns

Potential system compromise from undetected security anomalies and breaches


## Suggested Resolution

Immediate Response

Investigate suspicious transaction start patterns and user activities

Check user access permissions and transaction authorization settings

Review security audit log for anomalous activities and access patterns

Analyze current user sessions and transaction usage patterns

Security Assessment

Monitor user behavior trends and transaction access patterns

Evaluate security policies and access control requirements

Check for unauthorized transaction usage and access violations

Analyze security audit patterns and compliance requirements

Corrective Actions

Optimize security monitoring parameters and access control configurations

Implement enhanced transaction monitoring and security alerting systems

Establish proactive security monitoring procedures and automated compliance reporting

Plan security improvements based on transaction start analysis results


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 3 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 4 | INSTANCENAME | AS Instance | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 5 | LANGU | Description Language |  | 0 | 0 |  |  |
| 6 | MANDT | Client ID | CLNT | 3 | 0 | SYMANDT | MANDT |
| 7 | MSCDATE | Transaction Start Date | DATS | 8 | 0 | ALDATE | DATUM |
| 8 | MSCTIME | Time | TIMS | 6 | 0 | ALTIME | TIME |
| 9 | PACKAGE_INTERVAL | Package size (days) |  | 0 | 0 |  |  |
| 10 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 11 | TRN_BY | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 12 | TRN_EX | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 13 | TTEXT | Transaction text | CHAR | 36 | 0 | TTEXT_STCT | TEXT36 |
| 14 | USER | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 15 | USERID | User | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 15 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

BACKDAYS (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

DURATION (Duration In Time Units)

Uses duration in time units from the source context so only records with DURATION inside declared values proceed.

DURATION_UNIT (Duration Unit)

Unit for elapsed time between each session's creation date and time and the evaluation clock.

DURATION_UNIT Options:

·        H — Hours.

·        M — Minutes (preset in code before the selection read when not overridden).

·        D — Days.

·        F — Full-day style counting where applicable to the duration helper.

INSTANCENAME (AS Instance)

Aligns exception volume with the chosen scope by testing as instance via INSTANCENAME before alert evaluation.

LANGU (Description Language)

Interprets description language as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on LANGU.

MANDT (Client ID)

When populated, keeps the extract focused so client id (MANDT) aligns with the intended triage slice.

MSCDATE (Transaction Start Date)

Combines with related filters so transaction start date on MSCDATE refines which records remain for duration or state checks.

MSCTIME (Time)

Valuable when comparing health before and after a release—hold time on MSCTIME constant while varying other filters.

PACKAGE_INTERVAL (Package size (days))

Explains why two monitoring passes differ: only the pass with stricter package size (days) on PACKAGE_INTERVAL surfaces the disputed rows.

TCODE (Transaction Code)

Supports operational control by evaluating transaction code through TCODE for each candidate record.

TRN_BY (Transaction Code)

Reduces false positives during peak windows by tightening transaction code through TRN_BY alongside state filters.

TRN_EX (Transaction Code)

Reduces false positives during peak windows by tightening transaction code through TRN_EX alongside state filters.

TTEXT (Transaction text)

When combined with destination discipline, transaction text on TTEXT keeps both breadth and depth of the extract intentional.

USER (User)

Helps monitoring stay readable by requiring user (USER) to match organizational or technical selectors when set.

USERID (User)

Gives auditors traceable criteria because user on USERID is applied consistently before any alert flag is raised.


### Parameter Relationships

How parameter combinations work together

·        MSCDATE and MSCTIME anchor the business timing of each recorded transaction start event, while DURATION and DURATION_UNIT work together as alternative dynamic filter.

DURATION vs BACKDAYS:

DURATION  & DURATION_UNIT flexibly specify a time period length for filtering (e.g., last 4 hours or exactly 5 days ago)

BACKDAYS strictly defines the starting point for data retrieval (e.g., 3 days ago until today)

TCODE vs TRN_EX vs TRN_BY vs TTEXT:

TCODE is the standard SAP transaction code used for filtering and analysis (e.g., SU01)

TRN_EX represents the transaction code captured in security audit log execution records (e.g., SU01)

TRN_BY represents the transaction code identifier used by system processes and background activities (e.g., SU01)

TTEXT provides human-readable transaction descriptions for enhanced reporting and analysis (e.g., "User Maintenance")

USER vs USERID:

USER is the standard SAP user name used for filtering and reporting

USERID is the user identifier captured in security audit log records, which may include system-generated identifiers


### Default Values

·        BACKDAYS - initial - treated as 1 day backward window by code

·        DURATION - initial - treated as no extra duration filter by code

·        DURATION_UNIT - initial - treated as D day units by code

·        PACKAGE_INTERVAL - 10


### Practical Example of Parameter Configuration

Use Case 1: Focused user and transaction watch

Purpose: Monitor a specific user executing sensitive transaction codes across a defined calendar slice.

MSCDATE = 20260101 - 20260131
 USERID = ADMIN01
 TCODE = SE16 - SE16
 BACKDAYS = 1
 DURATION = 0 - 999999999
 DURATION_UNIT = D
 PACKAGE_INTERVAL = 10



Use Case 2: Broad transaction pattern

Purpose: Catch elevated activity for a transaction family while keeping duration filtering tight.

TRN_EX = SM20 - SM20
 DURATION = 100 - 999999999
 DURATION_UNIT = D
 MSCTIME = 000000 - 235959



Use Case 3: Client and instance scoped review

Purpose: Narrow the extract to one client and application server instance for investigation.

MANDT = 100 - 100
 INSTANCENAME = PRD - PRD
 USER = BATCHUSER01 - BATCHUSER01
 TTEXT = * - *




## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_AUD_SM20_TR_START | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_AUD_SM20_TR_START | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_AUD_SM20_TR_START | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_AUD_SM20_TR_START | MANDT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_AUD_SM20_TR_START | MSCDATE | Alert: date | DATS(8) | ALDATE |
| /SKN/S_SW_01_AUD_SM20_TR_START | MSCTIME | Alert: Time value in timeformat | TIMS(6) | ALTIME |
| /SKN/S_SW_01_AUD_SM20_TR_START | SLGLTRM2 | SecAudit: Terminal name | CHAR(20) | RSAUTERM |
| /SKN/S_SW_01_AUD_SM20_TR_START | SLGREPNA | Program Name | CHAR(40) | PROGRAM_ID |
| /SKN/S_SW_01_AUD_SM20_TR_START | TCODE | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_SM20_TR_START | TRN_BY | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_SM20_TR_START | TRN_EX | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_SM20_TR_START | TTEXT | Transaction Text | CHAR(36) | TTEXT_STCT |
| /SKN/S_SW_01_AUD_SM20_TR_START | USER | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_AUD_SM20_TR_START | USERID | User Name | CHAR(12) | SYUNAME |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_AUD_SM20_TR_START.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_AUD_SM20_TR_START OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA_MULTY: DURATION   /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
            DATUM     SY-DATUM,
            MSCDATE   ALDATE,
            USER      XUBNAME,
            TCODE     TCODE.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             BACKDAYS INT4,
             LANGU LANGU.
DATA_SINGLE: PACKAGE_INTERVAL  INT2.   " Dates Interval for packages
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LS_TR_FACT_AGGR TYPE /SKN/S_SW_USER_TR_FACT_AGGR,
      LT_TR_FACT_AGGR LIKE TABLE OF LS_TR_FACT_AGGR.
DATA: LS_USER_TR_LOG TYPE /SKN/S_SW_O1_AUD_TR_SM20,
      LT_USER_TR_LOG LIKE TABLE OF LS_USER_TR_LOG.
DATA: LS_USERS TYPE /SKN/S_SW_AUD_USERS,
      LT_USERS LIKE TABLE OF LS_USERS.
DATA: BEGIN OF LS_USER_LIST,
        USER TYPE XUBNAME,
      END OF LS_USER_LIST,
      LT_USER_LIST LIKE TABLE OF LS_USER_LIST.
DATA: LS_TR_LIST TYPE /SKN/S_SW_TR_LIST,
      LT_TR_LIST LIKE TABLE OF LS_TR_LIST,
      LT_ACT_TR_LIST LIKE TABLE OF LS_TR_LIST,
      LS_ACT_TR_LIST LIKE LINE OF LT_ACT_TR_LIST.
DATA: LS_TCODE TYPE /SKN/S_SW_TCODE,
      LT_TCODE LIKE TABLE OF LS_TCODE.
DATA: LS_USER_RNG TYPE /SKN/S_SW_USER_RNG,
      LT_SAP_ALL_RNG LIKE TABLE OF LS_USER_RNG.
DATA: DATE_FROM LIKE SY-DATUM,
      DATE_TO LIKE SY-DATUM.
DATA: LV_DATE_FROM LIKE SY-DATUM,
      LV_DATE_TO LIKE SY-DATUM,
      LV_DATE_INTERVAL TYPE I,
      LV_PAGES TYPE I.
DATA : TIME_DIFF TYPE  INT4 .
DATA : SY_DATLO LIKE SY-DATLO ,
       SY_TIMLO LIKE SY-TIMLO.
DATA : MANAGE_IN_UTC TYPE  CHAR1.
DATA: LV_STAT_AUTH_OK TYPE CHAR1.
*-- Fill Selection Option Tables
   SELECT_MULTY: DURATION,
                 DATUM,
                 MSCDATE,
                 USER,
                 TCODE.
   LV_DURATION_UNIT = 'D'.
   LV_BACKDAYS = 1.
   SELECT_SINGLE: DURATION_UNIT,
                  BACKDAYS,
                  LANGU.
   LV_PACKAGE_INTERVAL = 10.  " 10 days
   SELECT_SINGLE: PACKAGE_INTERVAL.
  READ TABLE R_MSCDATE INTO RS_MSCDATE INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_MSCDATE-LOW.
    DATE_TO = RS_MSCDATE-HIGH.
    IF DATE_TO IS INITIAL.
      DATE_TO = DATE_FROM.
    ENDIF.
  ELSE.
    DATE_FROM = SY-DATUM - LV_BACKDAYS.
    DATE_TO = SY-DATUM.
  ENDIF.
  LV_DATE_INTERVAL = DATE_FROM - DATE_TO + 1.
  SET_SY_TIME MANAGE_IN_UTC SY_DATLO SY_TIMLO .
  TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "-- Add TRs to be excluded
   RS_TCODE-SIGN = 'E'.
    RS_TCODE-OPTION = 'EQ'.
     RS_TCODE-LOW = 'SESSION_MANAGER'.
      APPEND RS_TCODE TO R_TCODE.
     RS_TCODE-LOW = 'SMEN'.
      APPEND RS_TCODE TO R_TCODE.
     RS_TCODE-LOW = 'BI_CLIENT_RUNTIME'.
      APPEND RS_TCODE TO R_TCODE.
  LV_STAT_AUTH_OK = 'X'.
  LV_PAGES = 1.
  IF LV_PACKAGE_INTERVAL IS NOT INITIAL.
    LV_DATE_INTERVAL = DATE_TO - DATE_FROM + 1.
    LV_PAGES = ROUND( VAL = ( LV_DATE_INTERVAL / LV_PACKAGE_INTERVAL ) DEC  = 0 MODE = CL_ABAP_MATH=>ROUND_UP ).
  ENDIF.
  LV_DATE_FROM = DATE_FROM.
  LV_DATE_TO = LV_DATE_FROM + LV_PACKAGE_INTERVAL.
  DO LV_PAGES TIMES.
    REFRESH LT_USER_TR_LOG.
    CALL FUNCTION '/SKN/F_SW_O1_AUD_GET_TR_SM20'
      EXPORTING
        DEST                        = LV_SW_DEST
        D_FROM                      = LV_DATE_FROM
        D_TO                        = LV_DATE_TO
      TABLES
*       T_USERS                     =
        T_USER_TR_LOG               = LT_USER_TR_LOG
      EXCEPTIONS
        COMMUNICATION_FAILURE       = 1
        OTHERS                      = 2.
    IF SY-SUBRC <> 0.
      CLEAR: LV_STAT_AUTH_OK.
    ENDIF.
    IF LV_STAT_AUTH_OK IS NOT INITIAL.
      DELETE LT_USER_TR_LOG WHERE TRN_EX NOT IN R_TCODE.
      DELETE LT_USER_TR_LOG WHERE USER NOT IN R_USER.
      "sort lt_USER_TR_LOG by USER TRN_EX.
      LOOP AT LT_USER_TR_LOG INTO LS_USER_TR_LOG.
        MOVE-CORRESPONDING LS_USER_TR_LOG TO LS_DATA.
        APPEND LS_DATA TO LT_DATA.
        "---
        MOVE-CORRESPONDING LS_USER_TR_LOG TO LS_TCODE.
        APPEND LS_TCODE TO LT_TCODE.
        "---
        MOVE-CORRESPONDING  LS_USER_TR_LOG TO LS_USER_LIST.
        APPEND LS_USER_LIST TO LT_USER_LIST.
      ENDLOOP.
      "---
      SORT LT_TCODE BY TCODE.
      DELETE ADJACENT DUPLICATES FROM LT_TCODE COMPARING TCODE.
      "---
      SORT LT_USER_LIST BY USER.
      DELETE ADJACENT DUPLICATES FROM LT_USER_LIST COMPARING USER.
    ENDIF.
    LV_DATE_FROM = LV_DATE_TO + 1.
    IF LV_DATE_FROM > DATE_TO.
      EXIT.
    ENDIF.
    LV_DATE_TO = LV_DATE_FROM + LV_PACKAGE_INTERVAL.
    IF LV_DATE_TO > DATE_TO.
      LV_DATE_TO = DATE_TO.
    ENDIF.
  ENDDO.
      "-- Get Users List
      CALL FUNCTION '/SKN/F_SW_O1_AUD_GET_TR_LIST'
        EXPORTING
          DEST            = LV_SW_DEST
          LANGU           = LV_LANGU
        TABLES
          T_TR_COND       = LT_TCODE
          T_TR_LIST       = LT_ACT_TR_LIST
*         T_TR_RNG        =
                .
      SORT LT_ACT_TR_LIST BY TCODE.
   LOOP AT LT_DATA INTO LS_DATA.
     SY_TABIX = SY-TABIX.
     READ TABLE LT_ACT_TR_LIST INTO LS_ACT_TR_LIST
                               WITH KEY TCODE = LS_DATA-TCODE
                               BINARY SEARCH.
     IF SY-SUBRC IS INITIAL.
       LS_DATA-TTEXT = LS_ACT_TR_LIST-TTEXT.
     ENDIF.
     "---
     LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
          EXPORTING
            D_FROM            = LS_DATA-MSCDATE
            T_FROM            = LS_DATA-MSCTIME
            D_TO              = SY_DATLO
            T_TO              = SY_TIMLO
            TIME_UNIT         = LV_DURATION_UNIT
          IMPORTING
            TIME_DIFF         = TIME_DIFF
          EXCEPTIONS
            WRONG_VALUE       = 1
            OTHERS            = 2    .
        IF SY-SUBRC = 0.
          LS_DATA-DURATION = TIME_DIFF .
        ELSE.
          LS_DATA-DURATION = '999999'.
        ENDIF.
     MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
   ENDLOOP.
  DELETE LT_DATA WHERE DURATION NOT IN R_DURATION.
 T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
