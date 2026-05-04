# Exception Indicator: Unauthorized TR execution based on SM20 log - SW_AUD_TR_SM20_VS_PT

## General Overview

This Exception Indicator (EI) monitors unauthorized transaction execution based on SM20 security log events by comparing executed transactions with users' permitted transaction profiles. It identifies user/transaction events where an executed transaction is not found in the user's authorized transaction list and reports those exceptions for security follow-up.

This EI serves as an essential control for security monitoring and access governance by:
- Enabling detection of transaction executions outside approved user authorization scope
- Supporting investigation of suspicious transaction activity captured in security logs
- Providing visibility into user, transaction, client, instance, and timestamp context for each unauthorized execution event
- Reducing blind spots caused by generic launcher transactions through explicit exclusion of non-business menu/session codes
- Supporting periodic compliance review with reproducible evidence of unauthorized transaction use

This monitoring helps security and control teams prioritize high-risk access violations, improve role governance, and strengthen incident response. It is especially useful for continuous access-control monitoring, internal audit testing, and post-incident forensic review.

The EI retrieves SM20 transaction-start activity and authorized transaction catalogs, then reports only mismatches between executed and permitted transactions.


## Problem Description

Failure to monitor unauthorized transaction execution from SM20 logs creates multiple risks across compliance, operations, and security governance.

**Compliance and Audit Risks**
- Unauthorized transaction usage can persist without evidence, increasing audit findings and control deficiencies
- Missing traceability between executed and permitted transactions weakens user-access compliance attestations
- Delayed detection of role violations can leave non-compliant access unresolved for extended periods
- Inconsistent review of security-log transaction activity undermines periodic control effectiveness

**Operational and Security Risks**
- Users may execute sensitive transactions not permitted by their profiles, increasing fraud and data-integrity exposure
- Repeated unauthorized transaction events can indicate compromised accounts or privilege misuse
- Without continuous monitoring, suspicious execution patterns by user or transaction can remain undetected
- High-risk activity in productive clients may escalate before security teams are alerted

**Management Visibility and Decision-Making Risks**
- Lack of exception-level transparency limits timely escalation and ownership assignment
- Untracked unauthorized-usage trends reduce effectiveness of role redesign decisions
- Inadequate visibility into where and when violations occur weakens risk prioritization and remediation planning

## Suggested Resolution

**Immediate Response**
- Review flagged unauthorized events by user and transaction and validate business legitimacy
- Prioritize exceptions involving sensitive transactions, privileged users, or repeated violations
- Confirm whether events represent true unauthorized usage, emergency access, or pending role-maintenance changes
- Escalate high-impact violations to security governance and process owners for immediate action

**System Assessment**
- Analyze exception patterns by user, transaction, date, and time to identify recurring control gaps
- Cross-check authorized transaction catalogs used in comparison to validate role-governance accuracy
- Evaluate client and application-server context to detect concentration of violations in specific environments
- Validate language/text resolution for transaction descriptions to improve investigation quality

**Corrective Actions**
- Update roles and authorization assignments where unauthorized execution is confirmed
- Revoke excessive access and enforce approval-based role remediation for critical transactions
- Document findings, ownership, and closure evidence for audit traceability
- Schedule recurring EI runs and structured review cycles to maintain continuous unauthorized-usage monitoring
- Feed recurring violation patterns into preventive control improvements and role design hardening


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | INSTANCENAME | AS Instance | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 3 | LANGU | Description Language |  | 0 | 0 |  |  |
| 4 | MANDT | Client ID | CLNT | 3 | 0 | SYMANDT | MANDT |
| 5 | MSCDATE | Date | DATS | 8 | 0 | ALDATE | DATUM |
| 6 | MSCTIME | Time | TIMS | 6 | 0 | ALTIME | TIME |
| 7 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 8 | TRN_BY | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 9 | TRN_EX | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 10 | TTEXT | Transaction text | CHAR | 36 | 0 | TTEXT_STCT | TEXT36 |
| 11 | USER | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 12 | USERID | User Name | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 12 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Days Backward from today):

Defines the default lookback period in days used to derive the monitoring window start date. The EI retrieves SM20 transaction activity from today minus BACKDAYS through today when explicit date boundaries are not supplied.

**INSTANCENAME** (AS Instance):

Application-server instance context for logged transaction events. Use this parameter to focus monitoring on specific SAP instances when analyzing unauthorized execution patterns.

**LANGU** (Description Language):

Language key for resolving transaction descriptions. Set this to match reviewer language requirements so transaction text in output is readable for investigation teams.

**MANDT** (Client ID):

SAP client identifier used to scope logged events. Use this to isolate unauthorized transaction execution in specific clients.

**MSCDATE** (Date):

Date component of the SM20 transaction-start event. Used for temporal filtering and sequence analysis of unauthorized execution activity.

**MSCTIME** (Time):

Time component of the SM20 transaction-start event. Used with MSCDATE for precise event chronology and investigation timelines.

**TCODE** (Transaction Code):

Transaction-code scope parameter for monitoring. It narrows which transactions are evaluated and compared against profile-authorized transaction lists.

**TRN_BY** (Transaction Code):

Transaction context attribute retained in output for traceability. It helps analysts interpret event context during unauthorized-use investigations.

**TRN_EX** (Transaction Code):

Executed transaction code captured in SM20 log events. This is the transaction actually compared against the user's permitted transaction list.

**TTEXT** (Transaction text):

Human-readable transaction description used for investigator-friendly output and reporting.

**USER** (User):

User scope parameter for event retrieval and comparison. It controls which users are included in unauthorized transaction analysis.

**USERID** (User Name):

User identity field in the output event record. It supports per-user investigation, escalation, and audit evidence mapping.


### Parameter Relationship

**Time Window and Event Timestamp Parameters:**

- **BACKDAYS** defines the default monitoring interval; **MSCDATE** and **MSCTIME** represent the event timestamp used for temporal analysis of unauthorized activity.
- **MSCDATE** and **MSCTIME** should be used together when reconstructing execution sequence and incident timelines.

**User and Transaction Scope Parameters:**

- **USER** and **USERID** represent user scope and user identity in exception output.
- **TCODE** narrows transaction scope before comparison, while **TRN_EX** represents the executed transaction evaluated against permissions.
- **TRN_BY** adds transaction context, and **TTEXT** provides readable transaction labels for triage.

**Environment Context Parameters:**

- **MANDT** and **INSTANCENAME** provide client and instance context for each exception record, supporting environment-specific root-cause analysis.
- **LANGU** affects transaction description rendering and should align with reviewer language for effective investigation.


### Default Values

- **DURATION_UNIT** — Default: `D` (days).
- **BACKDAYS** — Default: `1`.

### Practical Example of Parameter Configuration

**Use Case 1: Daily unauthorized transaction monitoring for critical users**
```
BACKDAYS = 1
USER = BASIS_ADMIN
TCODE = SU01 - PFCG
LANGU = E
```
**Purpose:** Detects recent unauthorized admin-transaction execution for a critical user scope.

**Use Case 2: Client-specific investigation run**
```
BACKDAYS = 7
MANDT = 100
USER = SAPUSER01
TCODE = SE38 - SM49
```
**Purpose:** Focuses unauthorized transaction analysis on one client over the past week for a targeted user and transaction band.

**Use Case 3: Instance-level triage with event timestamp focus**
```
BACKDAYS = 3
INSTANCENAME = PRD_AS01
MSCDATE = 20260301 - 20260303
MSCTIME = 000000 - 235959
```
**Purpose:** Isolates unauthorized events from a specific application server instance within a bounded incident window.

**Use Case 4: Broad user review with transaction context enrichment**
```
BACKDAYS = 14
TCODE = SE16 - SM20
TRN_EX = SE16 - SM20
USERID = USER_A - USER_Z
```
**Purpose:** Performs a two-week unauthorized-execution sweep with explicit executed-transaction and user-identity context for compliance review.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | INSTANCENAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | MANDT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | MSCDATE | Alert: date | DATS(8) | ALDATE |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | MSCTIME | Alert: Time value in timeformat | TIMS(6) | ALTIME |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | TCODE | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | TRN_BY | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | TRN_EX | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | TTEXT | Transaction Text | CHAR(36) | TTEXT_STCT |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | USER | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_AUD_TR_SM20_VS_PT | USERID | User Name | CHAR(12) | SYUNAME |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_AUD_TR_SM20_VS_PT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_AUD_TR_SM20_VS_PT OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA_MULTY: DURATION   /SKN/E_SW_DURATION,   "From NOW to Job Start Time point -in duration units
            DATUM     SY-DATUM,
            USER      XUBNAME,
            TCODE     TCODE.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             BACKDAYS INT4,
             LANGU LANGU.
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
DATA : TIME_DIFF TYPE  INT4 .
DATA: LV_STAT_AUTH_OK TYPE CHAR1.
*-- Fill Selection Option Tables
   SELECT_MULTY: DURATION,
                 DATUM,
                 USER,
                 TCODE.
   LV_DURATION_UNIT = 'D'.
   LV_BACKDAYS = 1.
   SELECT_SINGLE: DURATION_UNIT,
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
  LV_STAT_AUTH_OK = 'X'.
  CALL FUNCTION '/SKN/F_SW_O1_AUD_GET_TR_SM20'
    EXPORTING
      DEST                        = LV_SW_DEST
      D_FROM                      = DATE_FROM
      D_TO                        = DATE_TO
    TABLES
*     T_USERS                     =
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
      SORT LT_USER_TR_LOG BY USER TRN_EX.
      "-- Get Users List
      REFRESH LT_USER_LIST.
      LOOP AT LT_USER_TR_LOG INTO LS_USER_TR_LOG.
        MOVE-CORRESPONDING  LS_USER_TR_LOG TO LS_USER_LIST.
         APPEND LS_USER_LIST TO LT_USER_LIST.
        MOVE-CORRESPONDING LS_USER_TR_LOG TO LS_TCODE.
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
        LOOP AT LT_USER_TR_LOG INTO LS_USER_TR_LOG WHERE USER = LS_USER_LIST-USER.
          READ TABLE LT_TR_LIST INTO LS_TR_LIST WITH KEY TCODE = LS_USER_TR_LOG-TRN_EX.
          IF SY-SUBRC IS NOT INITIAL. " The trn was not allowed.
            MOVE-CORRESPONDING LS_USER_TR_LOG TO LS_DATA.
            READ TABLE LT_ACT_TR_LIST INTO LS_ACT_TR_LIST
                                      WITH KEY TCODE = LS_USER_TR_LOG-TRN_EX
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
