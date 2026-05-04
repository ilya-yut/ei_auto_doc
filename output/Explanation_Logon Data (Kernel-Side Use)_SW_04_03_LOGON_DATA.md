# Exception Indicator: Logon Data (Kernel-Side Use) - SW_04_03_LOGON_DATA

## General Overview

This Exception Indicator (EI) monitors SAP user logon and account-status data using kernel-side user master information and highlights records that meet configurable control criteria (validity windows, password/change states, lock status, user type, and logon behavior). It supports security and identity-governance review by bringing user lifecycle, credential state, and access hygiene signals into one analyzable output.

This EI serves as an essential control for user access governance and security operations by:
- Enabling detection of risky user-account states such as lock anomalies, password state exceptions, and unusual failed-logon patterns
- Supporting periodic review of user validity windows, creator attributes, and user type segmentation for access governance
- Providing visibility into last logon activity and password lifecycle events for dormant-account and credential-risk analysis
- Helping prioritize remediation of user records that violate policy windows or control thresholds
- Strengthening audit readiness with consistent, repeatable user-security evidence from SAP kernel-side data

This monitoring helps organizations improve identity hygiene, reduce unauthorized-access exposure, and accelerate security review cycles. It is particularly useful for IAM governance, periodic user recertification, and operational security control testing.

The EI reads SAP user master/logon-related attributes and enriches output with user descriptions and classification context.


## Problem Description

Failure to monitor kernel-side user logon and account-state data creates multiple risks across security, compliance, and operational governance.

**Security and Access-Control Risks**
- Inactive, expired, or improperly locked accounts may remain undetected, increasing unauthorized-access exposure
- Weak visibility into password change/lock events can delay detection of credential-management violations
- Repeated failed-logon patterns may be missed without systematic threshold-based review
- User-type and account-group inconsistencies can allow excessive or misaligned privileges to persist

**Compliance and Audit Risks**
- Missing evidence on user validity, password lifecycle, and lock status weakens control attestations
- Inconsistent review of account-state data increases likelihood of audit findings in user-access governance
- Lack of structured monitoring can delay remediation of policy violations tied to user master controls
- Incomplete traceability of user creation and activity attributes reduces confidence in audit narratives

**Operational and Management Risks**
- Dormant or stale accounts may not be prioritized for cleanup, increasing operational and security debt
- Management lacks clear trend visibility for account-state exceptions across time windows
- Delayed escalation of high-risk account conditions can extend incident response timelines

## Suggested Resolution

**Immediate Response**
- Review flagged user records for lock state, validity period, password status, and failed-logon indicators
- Prioritize high-risk combinations (expired yet active usage, lock inconsistencies, repeated failed logons)
- Validate business ownership for exception users and confirm whether access remains justified
- Escalate critical findings to IAM/security owners for immediate containment

**System Assessment**
- Analyze user-account patterns by type, group, creator, and validity timeline to find recurring control gaps
- Compare current exception distribution with previous periods to identify worsening trends
- Validate date/time reference configuration so duration and monitoring windows align with policy intent
- Review language/description settings to ensure reviewer-ready output for global control teams

**Corrective Actions**
- Disable, lock, or remediate unjustified accounts and apply password/control policy corrections
- Update role/user governance rules where recurring exception patterns indicate systemic issues
- Document findings, ownership, and closure evidence for compliance traceability
- Schedule recurring EI execution and review cadence for continuous user-account hygiene monitoring
- Feed recurring violations into preventive IAM control improvements and policy hardening


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACCNT | Account number | CHAR | 12 | 0 | XUACCNT | XUACCNT |
| 2 | ANAME | Creator of User Master Record | CHAR | 12 | 0 | XUANAME | BNAME |
| 3 | BACKDAYS | Back Days | INT4 | 10 | 0 | /SKN/E_MN_AN_BACKDAYS | /SKN/D_MN_AN_BACKDAYS |
| 4 | BCDA1 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 5 | BCDA2 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 6 | BCDA3 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 7 | BCDA4 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 8 | BCDA5 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 9 | BNAME | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 10 | BNAME_DESC | Complete name | CHAR | 80 | 0 | NAME_TEXT | TEXT80 |
| 11 | CLASS | User group | CHAR | 12 | 0 | XUCLASS | XUCLASS |
| 12 | DATE_REF_FLD | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_MN_AN_DURATION | /SKN/D_MN_AN_DURATION |
| 14 | ERDAT | Creation Date of User Master | DATS | 8 | 0 | XUERDAT | DATUM |
| 15 | EXC_RATE_TYPE | Exchange Rate Type | CHAR | 4 | 0 | KURST | KURST |
| 16 | FORWDAYS | Forth Days | INT4 | 10 | 0 | /SKN/E_MN_AN_FORWDAYS | /SKN/D_MN_AN_FORWDAYS |
| 17 | GLTGB | Valid through | DATS | 8 | 0 | XUGLTGB | DATUM |
| 18 | GLTGV | Valid from | DATS | 8 | 0 | XUGLTGV | DATUM |
| 19 | KTOPL | Chart of Accounts | CHAR | 4 | 0 | KTOPL | KTOPL |
| 20 | LANGU | Language Key | LANG | 1 | 0 | LANGU | SPRAS |
| 21 | LOCNT | Number of failed logon attempts | INT1 | 3 | 0 | XULOCNT | XULOCNT |
| 22 | LTIME | Last Logon Time | TIMS | 6 | 0 | XULTIME | UZEIT |
| 23 | PWDCHGDATE | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 24 | PWDLGNDATE | Date of Last Password Logon | DATS | 8 | 0 | XULPDAT | DATUM |
| 25 | PWDLOCKDATE | Date: Password Lock | DATS | 8 | 0 | XUPLDAT | DATUM |
| 26 | PWDSETDATE | Date: Password Reset by Administrator | DATS | 8 | 0 | XUSPDAT | DATUM |
| 27 | PWDSTATE | Password Change Status | INT1 | 3 | 0 | PWDCHGSTATE | PWDCHGSTATE |
| 28 | TIME_REF_FLD | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 29 | TRDAT | Last Logon Date | DATS | 8 | 0 | XULDATE | DATUM |
| 30 | UFLAG | User Lock Status | INT1 | 3 | 0 | XUUFLAG | XUUFLAG |
| 31 | USTYP | User Type | CHAR | 1 | 0 | XUUSTYP | XUUSTYP |
| 32 | USTYP_DESC | User Type Desc. | CHAR | 20 | 0 | /SKN/E_SW_USTYP_DESC |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 32 parameters listed in the Parameters Reference Table above.

**ACCNT** (Account number):

Identifies account number context used in user master controls. Use it to focus monitoring on specific account identifiers where relevant.

**ANAME** (Creator of User Master Record):

Represents the user who created the account. Useful for ownership and provisioning-governance analysis.

**BACKDAYS** (Back Days):

Defines the default historical lookback period when explicit date boundaries are not supplied.

**BCDA1 - BCDA5** (Date of Last Password Change):

These parallel slots represent password-change date checks used in multi-condition control scenarios. Configure ranges consistently when applying multiple password-date criteria.

**BNAME** (User):

Primary SAP user identifier used for filtering and exception review by account.

**BNAME_DESC** (Complete name):

Human-readable user name used for analyst-friendly output and review documentation.

**CLASS** (User group):

User-group classification used to segment governance checks by organizational grouping.

**DATE_REF_FLD** (Field name):

Defines which date field is used as the reference for monitoring-window and duration logic.

**DATE_REF_FLD Options:**
- **ERDAT**: Creation-date reference
- **TRDAT**: Last-logon-date reference
- **PWDCHGDATE**: Password-change-date reference
- **GLTGV / GLTGB**: Valid-from / valid-to reference

**DURATION** (Duration In Time Units):

Duration threshold/range used with DURATION_UNIT to evaluate elapsed time against the selected date/time reference.

**DURATION_UNIT** (Duration In Time Units):

Unit used for duration calculations and duration filtering.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**ERDAT** (Creation Date of User Master):

Account creation date used to filter user population and evaluate account age.

**EXC_RATE_TYPE** (Exchange Rate Type):

Exchange-rate type parameter retained for consistency across shared calculation frameworks; use when currency-conversion context is required by downstream logic.

**FORWDAYS** (Forth Days):

Defines forward-looking day window used in date-window controls where future horizon checks are needed.

**GLTGB** (Valid through):

Account valid-to date used to detect expired or near-expiry users.

**GLTGV** (Valid from):

Account valid-from date used to detect not-yet-valid users or provisioning timing issues.

**KTOPL** (Chart of Accounts):

Chart-of-accounts context parameter for scenarios where organizational accounting scope is part of filtering logic.

**LANGU** (Language Key):

Language used for description/text resolution in output.

**LOCNT** (Number of failed logon attempts):

Failed-logon-attempt count used to identify brute-force or repeated-authentication-failure patterns.

**LTIME** (Last Logon Time):

Time of last user logon used for activity chronology and dormant-account analysis.

**PWDCHGDATE** (Date of Last Password Change):

Date of latest password change used for credential-age controls.

**PWDLGNDATE** (Date of Last Password Logon):

Date of last password-based logon used for password-usage activity checks.

**PWDLOCKDATE** (Date: Password Lock):

Date when password lock was set; supports lock-event governance review.

**PWDSETDATE** (Date: Password Reset by Administrator):

Administrator password-reset date used for reset-governance and post-reset monitoring.

**PWDSTATE** (Password Change Status):

Password-state indicator for required/allowed/not-possible change conditions.

**TIME_REF_FLD** (Field name):

Defines which time field is used as reference in duration/time-window logic.

**TIME_REF_FLD Options:**
- **LTIME**: Last-logon-time reference
- **MSCTIME**: Event-time reference (where applicable)

**TRDAT** (Last Logon Date):

Last logon date used to detect dormant or stale user accounts.

**UFLAG** (User Lock Status):

User lock-status indicator used for lock-state controls.

**UFLAG Options:**
- **0**: Not locked
- **64**: Locally locked
- **128**: Globally/administratively locked

**USTYP** (User Type):

User type used to segment controls by technical/dialog/service/system account category.

**USTYP_DESC** (User Type Desc.):

Descriptive user-type label used for analyst-readable output and governance reporting.


### Parameter Relationship

**Monitoring Window and Duration Parameters:**

- **BACKDAYS** and **FORWDAYS** define default backward/forward monitoring horizon when explicit ranges are not supplied.
- **DATE_REF_FLD** and **TIME_REF_FLD** select the reference date/time basis used for elapsed-time evaluation.
- **DURATION** and **DURATION_UNIT** work together to apply duration-based filtering against the selected reference fields.

**Validity and Lifecycle Parameters:**

- **GLTGV** and **GLTGB** define account validity period boundaries and should be interpreted together.
- **ERDAT**, **TRDAT**, **LTIME**, and **PWDCHGDATE** provide lifecycle/activity timeline context for dormant or stale-account detection.

**Credential and Lock-State Parameters:**

- **PWDSTATE**, **PWDLOCKDATE**, **PWDLGNDATE**, and **PWDSETDATE** together describe password lifecycle and lock/reset state.
- **LOCNT** and **UFLAG** complement credential-state review by indicating failed-logon behavior and lock condition.

**Identity and Classification Parameters:**

- **BNAME**, **BNAME_DESC**, **USER group/class**, **USTYP**, and **USTYP_DESC** support user identity segmentation and reporting clarity.
- **ANAME** and **ACCNT** add provisioning/account-reference context for ownership tracing.

**Parallel Password-Date Slots:**

- **BCDA1 - BCDA5** are parallel password-change-date slots intended for multi-condition rule patterns and should be configured consistently across all active slots.


### Default Values

- **BACKDAYS** — Default: `10`.
- **DURATION_UNIT** — Default: `D` (days).
- **EXC_RATE_TYPE** — Default: `M`.
- **LANGU** — Default: `E`.

### Practical Example of Parameter Configuration

**Use Case 1: Dormant account review in the last 90 days**
```
BACKDAYS = 90
DATE_REF_FLD = TRDAT
DURATION_UNIT = D
DURATION = 30 - 9999
```
**Purpose:** Finds accounts with aged last-logon activity to support dormant-user cleanup.

**Use Case 2: Failed-logon and lock-state control**
```
BACKDAYS = 30
LOCNT = 3 - 255
UFLAG = 64 - 128
USTYP = A - S
```
**Purpose:** Highlights users with repeated failed logons and lock-state concerns across key user types.

**Use Case 3: Password lifecycle compliance check**
```
BACKDAYS = 180
PWDSTATE = 0 - 9
PWDCHGDATE = 20250101 - 20251231
PWDLOCKDATE = 20250101 - 20251231
```
**Purpose:** Reviews password-change and lock-date behavior for policy compliance across the period.

**Use Case 4: Full-day precision for validity-window exceptions**
```
DATE_REF_FLD = GLTGB
DURATION_UNIT = F
DURATION = 7
GLTGB = 20260101 - 20261231
```
**Purpose:** Detects accounts approaching valid-through boundaries with exact full-day filtering.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_AG24 | ACCNT | Account ID | CHAR(12) | XUACCNT |
| /SKN/S_SW_01_01_AG24 | ANAME | Creator of the User Master Record | CHAR(12) | XUANAME |
| /SKN/S_SW_01_01_AG24 | BACKDAYS | Backdays | INT4(10) | /SKN/E_MN_AN_BACKDAYS |
| /SKN/S_SW_01_01_AG24 | BCDA1 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_01_AG24 | BCDA2 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_01_AG24 | BCDA3 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_01_AG24 | BCDA4 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_01_AG24 | BCDA5 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_01_AG24 | BNAME | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_01_AG24 | BNAME_DESC | Full Name of Person | CHAR(80) | NAME_TEXT |
| /SKN/S_SW_01_01_AG24 | CLASS | User group in user master maintenance | CHAR(12) | XUCLASS |
| /SKN/S_SW_01_01_AG24 | DATE_REF_FLD | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_01_01_AG24 | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_MN_AN_DURATION |
| /SKN/S_SW_01_01_AG24 | ERDAT | Creation Date of the User Master Record | DATS(8) | XUERDAT |
| /SKN/S_SW_01_01_AG24 | EXC_RATE_TYPE | Exchange Rate Type | CHAR(4) | KURST |
| /SKN/S_SW_01_01_AG24 | FORWDAYS | Forwdays | INT4(10) | /SKN/E_MN_AN_FORWDAYS |
| /SKN/S_SW_01_01_AG24 | GLTGB | User valid to | DATS(8) | XUGLTGB |
| /SKN/S_SW_01_01_AG24 | GLTGV | User valid from | DATS(8) | XUGLTGV |
| /SKN/S_SW_01_01_AG24 | KTOPL | Chart of Accounts | CHAR(4) | KTOPL |
| /SKN/S_SW_01_01_AG24 | LANGU | Language Key | LANG(1) | LANGU |
| /SKN/S_SW_01_01_AG24 | LOCNT | Number of failed logon attempts | INT1(3) | XULOCNT |
| /SKN/S_SW_01_01_AG24 | LTIME | Last Logon Time | TIMS(6) | XULTIME |
| /SKN/S_SW_01_01_AG24 | PWDCHGDATE | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_01_AG24 | PWDLGNDATE | Date of Last Password Logon | DATS(8) | XULPDAT |
| /SKN/S_SW_01_01_AG24 | PWDLOCKDATE | Date: Setting of Password Lock | DATS(8) | XUPLDAT |
| /SKN/S_SW_01_01_AG24 | PWDSETDATE | Date: Password Reset by Administrator | DATS(8) | XUSPDAT |
| /SKN/S_SW_01_01_AG24 | PWDSTATE | Password Change: Required / Allowed / Not Possible | INT1(3) | PWDCHGSTATE |
| /SKN/S_SW_01_01_AG24 | TIME_REF_FLD | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_01_01_AG24 | TRDAT | Last Logon Date | DATS(8) | XULDATE |
| /SKN/S_SW_01_01_AG24 | UFLAG | User Lock Status | INT1(3) | XUUFLAG |
| /SKN/S_SW_01_01_AG24 | USTYP | User Type | CHAR(1) | XUUSTYP |
| /SKN/S_SW_01_01_AG24 | USTYP_DESC | SW: User Type Description | CHAR(20) | /SKN/E_SW_USTYP_DESC |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_AG24.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_AG24 OPTIONAL
*"----------------------------------------------------------------------
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: DATUM DATUM.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BNAME XUBNAME.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: GLTGV XUGLTGV.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: GLTGB XUGLTGB.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: USTYP XUUSTYP.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: CLASS XUCLASS.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: LOCNT XULOCNT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: UFLAG XUUFLAG.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: ACCNT XUACCNT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: ANAME XUANAME.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: ERDAT XUERDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: TRDAT XULDATE.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: LTIME XULTIME.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BCDA1 XUBCDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BCDA2 XUBCDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BCDA3 XUBCDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BCDA4 XUBCDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BCDA5 XUBCDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: PWDCHGDATE XUBCDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: PWDSTATE PWDCHGSTATE.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: PWDLGNDATE XULPDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: PWDSETDATE XUSPDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: PWDLOCKDATE XUPLDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BNAME_DESC NAME_TEXT.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: BACKDAYS /SKN/E_MN_AN_BACKDAYS.
  LV_BACKDAYS = '10'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: DATE_REF_FLD NAME_FELD.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: CURRENCY_CONV_DATE /SKN/E_MN_AN_CUR_CONV_DATE_FLD.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: DURATION /SKN/E_SW_DURATION.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: DURATION_UNIT /SKN/E_SW_DURATION_UNIT.
  LV_DURATION_UNIT = 'D'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: EXC_RATE_TYPE KURST.
  LV_EXC_RATE_TYPE = 'M'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: FORWDAYS /SKN/E_MN_AN_FORWDAYS.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: KTOPL KTOPL.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: LANGU LANGU.
  LV_LANGU = 'E'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: TARGET_CUKY /SKN/E_MN_AN_TARGET_CURR.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: TIME_REF_FLD NAME_FELD.
  ##NEEDED
  DATA SY_DATLO LIKE SY-DATLO.
  ##NEEDED
  DATA SY_TIMLO LIKE SY-TIMLO.
  ##NEEDED
  DATA DATE_FROM LIKE SY-DATUM.
  ##NEEDED
  DATA DATE_TO LIKE SY-DATUM.
  ##NEEDED
  DATA LV_TAB TYPE DDOBJNAME.
  ##NEEDED
  DATA LV_STRUC TYPE DDOBJNAME.
  ##NEEDED
  DATA LS_LIST TYPE /SKN/S_TABLES.
  ##NEEDED
  DATA LT_DATA_TMP LIKE T_DATA[].
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BNAME.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: GLTGV.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: GLTGB.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: USTYP.
  CONVERT_MULTY: USTYP USTYP.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: CLASS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: LOCNT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: UFLAG.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: ACCNT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: ANAME.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: ERDAT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: TRDAT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: LTIME.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BCDA1.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BCDA2.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BCDA3.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BCDA4.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BCDA5.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: PWDCHGDATE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: PWDSTATE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: PWDLGNDATE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: PWDSETDATE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: PWDLOCKDATE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BNAME_DESC.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: BACKDAYS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: DATE_REF_FLD.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: CURRENCY_CONV_DATE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: DURATION.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: DURATION_UNIT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: EXC_RATE_TYPE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: FORWDAYS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: KTOPL.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: LANGU.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: TARGET_CUKY.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: TIME_REF_FLD.
  ##NEEDED
  DATA LV_SW_DEST TYPE RFCDEST.
  ##NEEDED
  DATA LV_DELIMITER TYPE SONV-FLAG.
  ##NEEDED
  DATA LV_NO_DATA TYPE SONV-FLAG.
  ##NEEDED
  DATA LV_ROWSKIPS TYPE SOID-ACCNT.
  ##NEEDED
  DATA LV_ROWCOUNT TYPE SOID-ACCNT.
  ##NEEDED
  DATA LV_REC_CNT_ONLY TYPE FLAG.
  ##NEEDED
  DATA LV_ROWCOUNT2 TYPE SOID-ACCNT.
  ##NEEDED
  DATA LT_OPTIONS TYPE TABLE OF RFC_DB_OPT.
  ##NEEDED
  DATA LT_DATA TYPE TABLE OF /SKN/S_SW_TAB2000.
  ##NEEDED
  DATA LT_TABLES_LIST TYPE /SKN/TT_TABLES.
  ##NEEDED
  DATA LT_JOIN_CONDITION TYPE /SKN/TT_TABLE_JOIN.
  ##NEEDED
  DATA LT_SEL_FIELDS TYPE /SKN/TT_SEL_FIELDS.
  ##NEEDED
  DATA LT_SORT_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT.
  ##NEEDED
  DATA LT_GROUP_BY_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT.
  ##NEEDED
  DATA LT_HAVING_OPTIONS TYPE TABLE OF RFC_DB_OPT.
  ##NEEDED
  DATA LT_OUTPUT_FIELDS TYPE /SKN/TT_RFC_DB_FLD_EXTEND.
  ##NEEDED
  DATA LT_DFIES TYPE TABLE OF DFIES.
  ##NEEDED
  DATA LT_RETURN TYPE BAPIRET2_T.
  ##NEEDED
  DATA LT_ALL_ENTRIES_TAB TYPE TABLE OF /SKN/S_SW_TAB6000.
  ##NEEDED
  DATA LT_ALL_ENTRIES_COND TYPE TABLE OF /SKN/S_TABLE_JOIN.
  ##NEEDED
  DATA LT_ALL_ENTRIES_DFIES TYPE TABLE OF DFIES.
  ##NEEDED
  DATA LV_D_FROM TYPE SY-DATUM.
  ##NEEDED
  DATA LV_T_FROM TYPE SY-UZEIT.
  ##NEEDED
  DATA LV_D_TO TYPE SY-DATUM.
  ##NEEDED
  DATA LV_T_TO TYPE SY-UZEIT.
  ##NEEDED
  DATA LV_TIME_UNIT TYPE /SKN/E_SW_SCHEDL_UNIT.
  ##NEEDED
  DATA LV_TIME_DIFF TYPE INT4.
  ##NEEDED
  DATA LV_BNAME TYPE XUBNAME.
  ##NEEDED
  DATA LV_NAME_FIRST TYPE AD_NAMEFIR.
  ##NEEDED
  DATA LV_NAME_LAST TYPE AD_NAMELAS.
  ##NEEDED
  DATA LV_NAME_TEXT TYPE AD_NAMTEXT.
  ##NEEDED
  DATA LS_WA_ADRP TYPE ADRP.
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  REFRESH LT_OPTIONS.
  REFRESH LT_OUT_WHERE_COND.
  REFRESH LT_TABLES_LIST.
  CLEAR: LV_LINES, LS_OPTION,
         LT_OPTIONS_CURR, LT_COND_CURR, LT_OPTIONS_MAIN.
  ##NO_HANDLER
  SELECT_SINGLE: SW_DEST.
  ##NO_HANDLER
  _GET_CURRENT_DATE_TIME LV_MANAGE_IN_UTC LV_SW_DEST SY_DATLO SY_TIMLO.
  IF R_DATUM[] IS INITIAL.
    RS_DATUM-SIGN   = 'I'.
    IF LV_FORWDAYS IS INITIAL.
      DATE_FROM = SY_DATLO - LV_BACKDAYS.
      DATE_TO   = SY_DATLO.
      RS_DATUM-OPTION = 'BT'.
    ELSE.
      IF LV_BACKDAYS IS NOT INITIAL.
        DATE_FROM = SY_DATLO - LV_BACKDAYS.
        DATE_TO   = SY_DATLO + LV_FORWDAYS.
        RS_DATUM-OPTION = 'BT'.
      ELSE.
        DATE_FROM = SY_DATLO + LV_FORWDAYS.
        RS_DATUM-OPTION = 'GE'.
      ENDIF.
    ENDIF.
    RS_DATUM-LOW  = DATE_FROM.
    RS_DATUM-HIGH = DATE_TO.
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  CASE LV_DATE_REF_FLD.
    WHEN 'GLTGV'.
      IF R_GLTGV[] IS INITIAL.
        R_GLTGV[] = R_DATUM[].
      ENDIF.
    WHEN 'GLTGB'.
      IF R_GLTGB[] IS INITIAL.
        R_GLTGB[] = R_DATUM[].
      ENDIF.
    WHEN 'ERDAT'.
      IF R_ERDAT[] IS INITIAL.
        R_ERDAT[] = R_DATUM[].
      ENDIF.
    WHEN 'TRDAT'.
      IF R_TRDAT[] IS INITIAL.
        R_TRDAT[] = R_DATUM[].
      ENDIF.
    WHEN 'BCDA1'.
      IF R_BCDA1[] IS INITIAL.
        R_BCDA1[] = R_DATUM[].
      ENDIF.
    WHEN 'BCDA2'.
      IF R_BCDA2[] IS INITIAL.
        R_BCDA2[] = R_DATUM[].
      ENDIF.
    WHEN 'BCDA3'.
      IF R_BCDA3[] IS INITIAL.
        R_BCDA3[] = R_DATUM[].
      ENDIF.
    WHEN 'BCDA4'.
      IF R_BCDA4[] IS INITIAL.
        R_BCDA4[] = R_DATUM[].
      ENDIF.
    WHEN 'BCDA5'.
      IF R_BCDA5[] IS INITIAL.
        R_BCDA5[] = R_DATUM[].
      ENDIF.
    WHEN 'PWDCHGDATE'.
      IF R_PWDCHGDATE[] IS INITIAL.
        R_PWDCHGDATE[] = R_DATUM[].
      ENDIF.
    WHEN 'PWDLGNDATE'.
      IF R_PWDLGNDATE[] IS INITIAL.
        R_PWDLGNDATE[] = R_DATUM[].
      ENDIF.
    WHEN 'PWDSETDATE'.
      IF R_PWDSETDATE[] IS INITIAL.
        R_PWDSETDATE[] = R_DATUM[].
      ENDIF.
    WHEN 'PWDLOCKDATE'.
      IF R_PWDLOCKDATE[] IS INITIAL.
        R_PWDLOCKDATE[] = R_DATUM[].
      ENDIF.
  ENDCASE.
  REFRESH R_DATUM.
  ##NO_HANDLER
  _APPEND_TABLES_LIST 'USR02' '' 'A'.
  LV_RANGE = 'ACCNT'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE ACCNT.
  LV_RANGE = 'ANAME'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE ANAME.
  LV_RANGE = 'BCDA1'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE BCDA1.
  LV_RANGE = 'BCDA2'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE BCDA2.
  LV_RANGE = 'BCDA3'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE BCDA3.
  LV_RANGE = 'BCDA4'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE BCDA4.
  LV_RANGE = 'BCDA5'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE BCDA5.
  LV_RANGE = 'BNAME'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE BNAME.
  LV_RANGE = 'CLASS'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE CLASS.
  LV_RANGE = 'ERDAT'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE ERDAT.
  LV_RANGE = 'GLTGB'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE GLTGB.
  LV_RANGE = 'GLTGV'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE GLTGV.
  LV_RANGE = 'LOCNT'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE LOCNT.
  LV_RANGE = 'LTIME'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE LTIME.
  LV_RANGE = 'PWDCHGDATE'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE PWDCHGDATE.
  LV_RANGE = 'PWDLGNDATE'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE PWDLGNDATE.
  LV_RANGE = 'PWDLOCKDATE'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE PWDLOCKDATE.
  LV_RANGE = 'PWDSETDATE'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE PWDSETDATE.
  LV_RANGE = 'PWDSTATE'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE PWDSTATE.
  LV_RANGE = 'TRDAT'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE TRDAT.
  LV_RANGE = 'UFLAG'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE UFLAG.
  LV_RANGE = 'USTYP'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE USTYP.
  LT_OPTIONS[] = LT_OUT_WHERE_COND[].
  LT_OPTIONS_MAIN[] = LT_OPTIONS[].
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  REFRESH LT_SORT_OPTIONS.
  REFRESH LT_OUT_WHERE_COND.
  REFRESH LT_GROUP_BY_OPTIONS.
  CLEAR LT_DATA.
  CLEAR LT_DATA_RFC.
  CLEAR LT_SEL_FIELDS.
  CLEAR LT_RETURN.
  IF LT_OPTIONS_MAIN IS NOT INITIAL.
    CLEAR LT_OPTIONS.
    LT_OPTIONS = LT_OPTIONS_MAIN.
  ENDIF.
  _ADAPT_SEL_FIELDS 'USR02' 'ZSWS_ARG_200004_000024' LT_SEL_FIELDS
  LV_SW_DEST.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: SW_DEST.
  CLEAR: LS_SEL_FIELDS, LT_DATA_TMP.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
*   EXPORTING
*     delimiter            = ' '
*     NO_DATA              = ' '
*     ROWSKIPS             = 0
*     rowcount             = im_number
*     REC_CNT_ONLY         =
    IMPORTING
      ##NEEDED
      ROWCOUNT             = LV_ROWCOUNT
    TABLES
      OPTIONS              = LT_OPTIONS
      DATA                 = LT_DATA
      TABLES_LIST          = LT_TABLES_LIST ##ENH_OK
      JOIN_CONDITION       = LT_JOIN_CONDITION ##ENH_OK
      SEL_FIELDS           = LT_SEL_FIELDS ##ENH_OK
      SORT_OPTIONS         = LT_SORT_OPTIONS ##ENH_OK
      GROUP_BY_OPTIONS     = LT_GROUP_BY_OPTIONS ##ENH_OK
      HAVING_OPTIONS       = LT_HAVING_OPTIONS ##ENH_OK
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS ##ENH_OK
      DFIES                = LT_DFIES ##ENH_OK
      RETURN               = LT_RETURN ##ENH_OK
      ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB ##ENH_OK
      ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND ##ENH_OK
      ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES ##ENH_OK
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  CLEAR: LV_ROWCOUNT, LT_JOIN_CONDITION, LT_SEL_FIELDS,
         LT_SORT_OPTIONS, LT_GROUP_BY_OPTIONS, LT_TABLES_LIST.
  IF SY-SUBRC IS NOT INITIAL OR LT_RETURN IS NOT INITIAL.
    CLEAR LT_DATA_RFC.
  ELSE.
    _RFC_TO_T_DATA_INDEX LT_DATA LT_DATA_TMP LT_OUTPUT_FIELDS 1.
    IF LT_DATA_TMP[] IS NOT INITIAL.
      APPEND LINES OF LT_DATA_TMP[] TO T_DATA[].
    ENDIF.
  ENDIF.
  CHECK T_DATA[] IS NOT INITIAL.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: D_FROM.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: T_FROM.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: D_TO.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: T_TO.
* The parameter field 'lv_date_ref_fld'
* and 'lv_time_ref_fld' is declared
* at '/SKN/T_AR_FIELDS' custom. table
* and is initialized on the user screen
  ##NEEDED
  DATA: SY_TABIX LIKE SY-TABIX .
  ##NEEDED
  FIELD-SYMBOLS:  TYPE ANY,
  ##NEEDED
                 <FS_DURATION> TYPE ANY,
  ##NEEDED
                 <FS_DU>       TYPE ANY.
  CLEAR: LV_FLD, SY_TABIX.
  LV_T_FROM = SY_TIMLO.
  LV_D_TO   = SY_DATLO.
  LV_T_TO   = SY_TIMLO.
*-- Calculate Status Duration
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX.
* Set field 'date_from' by date reference field
* which is determined on the user screen
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO LV_FLD.
    ASSIGN (LV_FLD) TO .
    IF  IS NOT ASSIGNED.
      CONTINUE.
    ELSE.
      LV_D_FROM = .
      UNASSIGN .
    ENDIF.
    CLEAR: LV_FLD.
* Set field 'time_from' by time reference field
* which is determined on the user screen
    IF LV_TIME_REF_FLD IS NOT INITIAL.
      CONCATENATE 'T_DATA-' LV_TIME_REF_FLD INTO LV_FLD.
      ASSIGN (LV_FLD) TO .
      IF  IS ASSIGNED.
        LV_T_FROM = .
      ENDIF.
    ENDIF.
    IF NOT LV_D_FROM IS INITIAL.
      ASSIGN COMPONENT 'DURATION_UNIT' OF STRUCTURE T_DATA TO <FS_DU>.
      IF SY-SUBRC EQ 0 AND <FS_DU> IS ASSIGNED.
*      t_data-duration_unit = lv_duration_unit.
        <FS_DU> = LV_DURATION_UNIT.
      ENDIF.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = LV_D_FROM
          T_FROM      = LV_T_FROM
          D_TO        = LV_D_TO
          T_TO        = LV_T_TO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = LV_TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        ASSIGN COMPONENT 'DURATION' OF STRUCTURE T_DATA TO <FS_DURATION>.
        IF SY-SUBRC EQ 0 AND <FS_DURATION> IS ASSIGNED.
          <FS_DURATION> = LV_TIME_DIFF.
        ENDIF.
      ELSE.
        ASSIGN COMPONENT 'DURATION' OF STRUCTURE T_DATA TO <FS_DURATION>.
        IF SY-SUBRC EQ 0 AND <FS_DURATION> IS ASSIGNED.
          <FS_DURATION> = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  LOOP AT T_DATA.
    CLEAR LV_FIELDTAB.
    CLEAR LV_FIELDTAB2.
    LV_FIELDTAB = 'BNAME'.
    LV_LANGU = 'E'.
    LV_DESC_FIELD_PR = 'BNAME'.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
    CLEAR: LV_FLD, LV_BNAME, LV_NAME_TEXT,
           LV_NAME_FIRST, LV_NAME_LAST.
    IF LV_FIELDTAB IS NOT INITIAL.
      CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
      ASSIGN (LV_FLD) TO <FS_VAL>.
      IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
        LV_BNAME = <FS_VAL>.
        UNASSIGN <FS_VAL>.
        CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES'
          EXPORTING
            BNAME      = LV_BNAME
            SW_DEST    = LV_SW_DEST
          IMPORTING
            NAME_FIRST = LV_NAME_FIRST
            NAME_LAST  = LV_NAME_LAST
            NAME_TEXT  = LV_NAME_TEXT
          EXCEPTIONS
            NO_DATA    = 1
            OTHERS     = 2.
        IF SY-SUBRC EQ 0.
          CLEAR: LV_FLD.
          CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_NAME_TEXT.
            MODIFY T_DATA.
            UNASSIGN <FS_VAL>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_USTYP_OUTPUT'
      EXPORTING
        INPUT           = T_DATA-USTYP
      IMPORTING
        OUTPUT          = T_DATA-USTYP_DESC
      EXCEPTIONS
        INPUT_NOT_VALID = 1
        OTHERS          = 2.
    MODIFY T_DATA[] FROM  T_DATA.
  ENDLOOP.
  DELETE T_DATA[] WHERE BNAME_DESC NOT IN  R_BNAME_DESC[].
  CHECK T_DATA[] IS NOT INITIAL.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
