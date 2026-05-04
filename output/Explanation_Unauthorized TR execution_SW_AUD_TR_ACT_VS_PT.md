# Exception Indicator: Unauthorized TR execution - SW_AUD_TR_ACT_VS_PT

## General Overview

This Exception Indicator (EI) monitors unauthorized transaction execution by comparing users' actual transaction activity with their permitted transaction list and highlighting mismatches. It analyzes transaction statistics in a configurable period and reports user/transaction combinations that appear executed but not allowed in the user profile scope.

This EI serves as an essential control for security monitoring and access governance by:
- Enabling detection of transaction usage outside approved authorization scope
- Supporting rapid identification of risky user behavior patterns requiring investigation
- Providing visibility into who executed which transactions and when, for accountability and escalation
- Helping compliance teams validate access controls with repeatable exception evidence
- Supporting continuous review of unauthorized activity trends across configured monitoring windows

This monitoring helps organizations reduce unauthorized-access risk, prioritize remediation, and strengthen role governance. It is especially useful for periodic user-access reviews, internal audit procedures, and incident-response workflows.

The EI uses transaction activity statistics and authorization-related transaction catalogs retrieved via SAP security-related function modules.


## Problem Description

Failure to monitor unauthorized transaction execution creates multiple risks across compliance, operational control, and security governance.

**Compliance and Audit Risks**
- Unauthorized transaction usage can remain undetected, increasing audit findings and control deficiencies
- Weak visibility into executed-versus-permitted transactions delays remediation of access-control gaps
- Repeated policy violations may continue without traceable exception reporting
- Missing evidence trails can reduce confidence in access-governance attestations

**Operational and Security Risks**
- Users may execute sensitive transactions outside approved scope, increasing fraud and data-integrity exposure
- Hidden unauthorized activity can lead to unauthorized changes and business disruptions
- Delayed detection of suspicious patterns increases incident impact and recovery effort
- Concentrated risky activity by specific users or transaction codes may remain unnoticed

**Management Visibility and Decision-Making Risks**
- Limited exception visibility weakens prioritization of role and authorization improvements
- Untracked unauthorized trends reduce effectiveness of preventive control decisions
- Lack of clear user-level and transaction-level transparency delays ownership and escalation actions

## Suggested Resolution

**Immediate Response**
- Review flagged user/transaction exceptions and validate whether execution falls outside approved scope
- Classify exceptions by risk level and business impact with security and process owners
- Distinguish true unauthorized activity from temporary role-maintenance or emergency-access scenarios
- Escalate repeated or high-impact exceptions for immediate investigation

**System Assessment**
- Analyze exception patterns by user and transaction to identify recurring control weaknesses
- Compare activity across the monitored period to separate one-off events from systematic issues
- Validate source and period settings so monitoring coverage aligns with policy expectations
- Confirm transaction text/language setup supports clear investigation output

**Corrective Actions**
- Update roles and authorization assignments where unauthorized usage is confirmed
- Revoke excessive access and enforce governed approval for sensitive transaction privileges
- Document findings, owners, and closure evidence for audit traceability
- Schedule recurring EI execution and structured review cycles for continuous oversight
- Feed repeated exception patterns into role redesign and preventive control hardening


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | COUNT | Counter | DEC | 24 | 0 | SWNCCNTAGG | SWNCDOMDEC24 |
| 3 | COUNT_DAYS | Natural number | INT4 | 10 | 0 | INT4 | INT4 |
| 4 | DATE_MAX | Date | DATS | 8 | 0 | DATUM | DATUM |
| 5 | DATE_MIN | Date | DATS | 8 | 0 | DATUM | DATUM |
| 6 | LANGU | Description Language |  | 0 | 0 |  |  |
| 7 | SOURCE | Statistics Source (D / W / M) |  | 0 | 0 |  |  |
| 8 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 9 | TTEXT | Transaction text | CHAR | 36 | 0 | TTEXT_STCT | TEXT36 |
| 10 | USER | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 10 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Days Backward from today):

Defines how many days before today the monitoring interval starts. The EI builds its activity window from today minus BACKDAYS through today before evaluating unauthorized transaction usage.

**COUNT** (Counter):

Represents transaction activity count values from statistics data. Use this parameter when focusing on high-volume or low-volume unauthorized activity patterns.

**COUNT_DAYS** (Natural number):

Represents day-oriented count metrics used in activity aggregation context. It helps isolate users or transactions with persistent activity across days.

**DATE_MAX** (Date):

Upper date boundary for activity review. Use to end monitoring at a specific date during audit, investigation, or controlled review cycles.

**DATE_MIN** (Date):

Lower date boundary for activity review. Use to start monitoring from a specific date that aligns with control or investigation scope.

**LANGU** (Description Language):

Defines the language used for descriptive transaction metadata in output. Set this to match reviewer language requirements for clearer triage and reporting.

**SOURCE** (Statistics Source (D / W / M)):

Specifies the statistics source granularity used to read transaction activity facts. This setting determines whether activity is evaluated from daily, weekly, or monthly statistics layers.

**SOURCE Options:**
- **D**: Daily statistics source
- **W**: Weekly statistics source
- **M**: Monthly statistics source

**TCODE** (Transaction Code):

Defines transaction-code scope for unauthorized execution analysis. The EI also internally excludes launcher/menu transaction codes from exception evaluation.

**TTEXT** (Transaction text):

Holds descriptive transaction text for output readability. This supports faster investigation by providing business-readable labels next to technical transaction codes.

**USER** (User):

Defines user scope for monitoring. The EI evaluates each user's executed transactions against permitted transaction lists and reports mismatches as unauthorized exceptions.


### Parameter Relationship

**Time Window Parameters:**

- **BACKDAYS**, **DATE_MIN**, and **DATE_MAX** shape the monitored activity period used for exception detection.
- **SOURCE** applies within that period and determines whether statistics are read at daily, weekly, or monthly granularity.

**User and Transaction Scope Parameters:**

- **USER** and **TCODE** define who and which transactions are in analysis scope before unauthorized checks are applied.
- **LANGU** supports readable output for scoped transactions by controlling transaction text language.

**Activity Volume Parameters:**

- **COUNT** and **COUNT_DAYS** support volume-focused review of suspicious behavior and repeated transaction activity.
- **TTEXT** complements **TCODE** for faster interpretation during triage and investigation.


### Default Values

- **DURATION_UNIT** — Default: `D` (days).
- **BACKDAYS** — Default: `10`.
- **SOURCE** — Default: `D` (daily statistics source).

### Practical Example of Parameter Configuration

**Use Case 1: Daily unauthorized-activity monitoring for selected users**
```
SOURCE = D
BACKDAYS = 10
USER = TEST_USER_01
TCODE = SE38 - SM49
```
**Purpose:** Detects recent daily unauthorized transaction execution for a targeted user and transaction range.

**Use Case 2: Weekly trend review of suspicious transaction usage**
```
SOURCE = W
BACKDAYS = 30
COUNT = 1 - 999999
TCODE = SU01 - PFCG
```
**Purpose:** Reviews weekly unauthorized-usage trends for sensitive admin transactions over the past month.

**Use Case 3: Date-bounded audit cycle execution**
```
SOURCE = M
DATE_MIN = 20250101
DATE_MAX = 20250331
LANGU = E
```
**Purpose:** Produces monthly unauthorized-activity analysis for a quarterly audit window with English transaction descriptions.

**Use Case 4: High-repeat activity triage**
```
SOURCE = D
BACKDAYS = 7
COUNT_DAYS = 3 - 999
USER = BASIS_USER
```
**Purpose:** Highlights users with repeated daily activity patterns that require security triage and access validation.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | MANDT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | MSCDATE | Alert: date | DATS(8) | ALDATE |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | MSCTIME | Alert: Time value in timeformat | TIMS(6) | ALTIME |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | TCODE | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | TRN_BY | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | TRN_EX | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | TTEXT | Transaction Text | CHAR(36) | TTEXT_STCT |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | USER | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_AUD_TR_ACT_VS_PT | USERID | User Name | CHAR(12) | SYUNAME |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_AUD_TR_ACT_VS_PT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_AUD_TR_ACT_VS_PT OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA_MULTY: DURATION   /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
            DATUM     SY-DATUM,
            USER      XUBNAME,
            TCODE     TCODE.
DATA_SINGLE: SOURCE  CHAR1,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             BACKDAYS INT4,
             LANGU LANGU.
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LS_TR_FACT_AGGR TYPE /SKN/S_SW_USER_TR_FACT_AGGR,
      LT_TR_FACT_AGGR LIKE TABLE OF LS_TR_FACT_AGGR.
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
DATA : TIME_DIFF TYPE  INT4 .
DATA: LV_STAT_AUTH_OK TYPE CHAR1.
*-- Fill Selection Option Tables
   SELECT_MULTY: DURATION,
                 DATUM,
                 USER,
                 TCODE.
   LV_DURATION_UNIT = 'D'.
   LV_BACKDAYS = 10.
   LV_SOURCE = 'D'.
   SELECT_SINGLE: SOURCE,
                  DURATION_UNIT,
                  BACKDAYS,
                  LANGU.
  DATE_FROM = SY-DATUM - LV_BACKDAYS.
  DATE_TO = SY-DATUM.
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
  CLEAR: LV_STAT_AUTH_OK.
  CALL FUNCTION '/SKN/F_SW_O1_AUD_GET_TR_ACT_AG'
    EXPORTING
      DEST                 = LV_SW_DEST
      D_FROM               = DATE_FROM
      D_TO                 = DATE_TO
      SOURCE               = LV_SOURCE
    TABLES
*     T_USERS              =
*     T_TR                 =
      T_TR_FACT_AGGR       = LT_TR_FACT_AGGR
    EXCEPTIONS
      NO_USER_STAT_AUTHORITY       = 1
      OTHERS                       = 2.
  IF SY-SUBRC <> 0.
    LS_DATA-TTEXT = 'No Authorization for User Statistics'.
    APPEND LS_DATA TO LT_DATA.
  ELSE.
    LV_STAT_AUTH_OK = 'X'.
  ENDIF.
  IF LV_STAT_AUTH_OK IS NOT INITIAL.
      DELETE LT_TR_FACT_AGGR WHERE TCODE NOT IN R_TCODE.
      DELETE LT_TR_FACT_AGGR WHERE USER NOT IN R_USER.
      SORT LT_TR_FACT_AGGR BY USER TCODE.
      "-- Get Users List
      REFRESH LT_USER_LIST.
      LOOP AT LT_TR_FACT_AGGR INTO LS_TR_FACT_AGGR.
        MOVE-CORRESPONDING  LS_TR_FACT_AGGR TO LS_USER_LIST.
         APPEND LS_USER_LIST TO LT_USER_LIST.
        MOVE-CORRESPONDING LS_TR_FACT_AGGR TO LS_TCODE.
         APPEND LS_TCODE TO LT_TCODE.
      ENDLOOP.
      SORT LT_USER_LIST BY USER.
       DELETE ADJACENT DUPLICATES FROM LT_USER_LIST COMPARING USER.
      SORT LT_TCODE BY TCODE.
       DELETE ADJACENT DUPLICATES FROM LT_TCODE COMPARING TCODE.
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
      "--- Exclude SAP ALL Users
      CALL FUNCTION '/SKN/F_SW_01_AUD_UR_SAP_ALL'
        EXPORTING
          DEST             = LV_SW_DEST
        TABLES
          T_USER_RNG       = LT_SAP_ALL_RNG
*         T_USERS          =
                .
      IF LT_SAP_ALL_RNG[] IS NOT INITIAL.
        DELETE LT_USER_LIST WHERE USER IN LT_SAP_ALL_RNG.
      ENDIF.
      LOOP AT LT_USER_LIST INTO LS_USER_LIST.
        CALL FUNCTION '/SKN/F_SW_O1_AUD_GET_TR_PT'
          EXPORTING
            DEST                      = LV_SW_DEST
            USER                      = LS_USER_LIST-USER
            LANGU                     = LV_LANGU
          TABLES
            T_TR_LIST                 = LT_TR_LIST
          EXCEPTIONS
            USER_NAME_NOT_EXIST       = 1
            NOT_AUTHORIZED            = 2
            INTERNAL_ERROR            = 3
            NO_DATA                   = 4
            OTHERS                    = 5.
        IF SY-SUBRC <> 0.
          CONTINUE.
        ENDIF.
        SORT LT_TR_LIST BY TCODE.
        LOOP AT LT_TR_FACT_AGGR INTO LS_TR_FACT_AGGR WHERE USER = LS_USER_LIST-USER.
          READ TABLE LT_TR_LIST INTO LS_TR_LIST WITH KEY TCODE = LS_TR_FACT_AGGR-TCODE.
          IF SY-SUBRC IS NOT INITIAL. " The trn was not allowed.
            MOVE-CORRESPONDING LS_TR_FACT_AGGR TO LS_DATA.
            READ TABLE LT_ACT_TR_LIST INTO LS_ACT_TR_LIST
                                      WITH KEY TCODE = LS_TR_FACT_AGGR-TCODE
                                      BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              MOVE-CORRESPONDING LS_ACT_TR_LIST TO LS_DATA.
            ENDIF.
            APPEND LS_DATA TO LT_DATA.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
  ENDIF.
***  delete t_data where: VBREPORT not in R_VBREPORT,
****-- Fill Duration Value
*** loop at T_DATA .
***   sy_tabix = sy-tabix.
***   T_DATA-DURATION_UNIT = lv_DURATION_UNIT.
***    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
***        EXPORTING
***          D_FROM            = t_data-VDATE
***          T_FROM            = t_data-VTIME
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
 T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
