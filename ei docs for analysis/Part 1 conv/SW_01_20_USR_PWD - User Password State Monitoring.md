# Exception Indicator: User Password State Monitoring (SW_01_20_USER_PWD)


## General Overview

This Exception Indicator (EI) provides comprehensive monitoring and analysis of SAP user password states to ensure user security compliance, password policy enforcement, and identification of potential security risks related to user authentication. Password state monitoring is essential for maintaining system security integrity and ensuring compliance with organizational security policies and regulatory requirements.

User Password State Monitoring extends the standard SAP user administration framework by providing detailed password lifecycle tracking and security state analysis. The technology enables proactive monitoring of password-related security events including expired passwords, initial passwords, password change patterns, and user locking scenarios, crucial for maintaining robust security posture and preventing unauthorized access.

This Exception Indicator provides detailed user password monitoring capabilities that enable:

Password lifecycle tracking to monitor password change patterns, expiration dates, and security compliance

Security state analysis for understanding user authentication security posture and identifying compliance gaps

Initial password detection to identify users with administrator-set passwords requiring immediate change

User locking monitoring to track locked user accounts and security-related access restrictions

Password policy compliance for ensuring adherence to organizational password policies and security standards

The monitoring solution analyzes user password data from the USR02 (User Master Record) and USR04 (User Master Authorization) tables, similar to data available through SU01 transaction (User Maintenance), and provides enhanced filtering capabilities to focus on specific user types, password states, dates, and security conditions. This enables targeted analysis of password security characteristics and identification of authentication-related security risks.

This Exception Indicator checks whether SAP user password management is functioning securely and identifies potential security issues that may impact user authentication, access control, and system security compliance.


## Problem Description

Poor user password management and security compliance issues indicate authentication system vulnerabilities causing:

Security and Compliance Problems

Users with initial passwords set by administrators creating security vulnerabilities and access risks

Expired or unchanged passwords violating organizational security policies and compliance requirements

Locked user accounts indicating potential security incidents or authentication system issues

Password policy violations compromising system security integrity and regulatory compliance

Access Control Issues

Inadequate password change monitoring leading to security policy violations and compliance gaps

User account management failures affecting access control effectiveness and security oversight

Authentication system inconsistencies creating security vulnerabilities and access control weaknesses

Password lifecycle management problems impacting user security posture and system protection

Business Impact

Security compliance violations due to inadequate password management affecting regulatory adherence

Unauthorized access risks from poor password security practices impacting data protection and system integrity

Business process interruptions from locked or inaccessible user accounts affecting operational continuity

Audit findings and compliance failures from insufficient password monitoring compromising organizational security posture


## Suggested Resolution

Immediate Response

Investigate users with initial passwords using SU01 transaction for immediate password security assessment

Check locked user accounts and security incidents for potential security breach identification

Review password change patterns and compliance violations for security policy enforcement

Analyze user authentication events and identify security-related access issues for immediate remediation

System Assessment

Monitor password lifecycle trends and compliance patterns for security policy optimization

Evaluate user authentication security posture and password policy effectiveness for compliance management

Check user account management processes and security controls for access control optimization

Analyze password security patterns by user type, role, and system for security enhancement

Corrective Actions

Implement enhanced password policies and security controls for improved authentication protection

Establish automated password monitoring and compliance enforcement procedures for security management

Develop proactive user security monitoring and alerting capabilities for early security issue detection

Plan security awareness training and password management improvements based on monitoring analysis


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | ANAME | Creator of User Master Record | CHAR | 12 | 0 | XUANAME | BNAME |
| 2 | BACKDAYS | Days backwards from today |  | 0 | 0 |  |  |
| 3 | BCDA1 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 4 | BCDA2 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 5 | BCDA3 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 6 | BCDA4 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 7 | BCDA5 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 8 | BNAME | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 9 | CLASS | User group | CHAR | 12 | 0 | XUCLASS | XUCLASS |
| 10 | CODV1 | Password Code Vers. | CHAR | 1 | 0 | XUCODEVERS | XUCODEVERS |
| 11 | CODV2 | Password Code Vers. | CHAR | 1 | 0 | XUCODEVERS | XUCODEVERS |
| 12 | CODV3 | Password Code Vers. | CHAR | 1 | 0 | XUCODEVERS | XUCODEVERS |
| 13 | CODV4 | Password Code Vers. | CHAR | 1 | 0 | XUCODEVERS | XUCODEVERS |
| 14 | CODV5 | Password Code Vers. | CHAR | 1 | 0 | XUCODEVERS | XUCODEVERS |
| 15 | CODVN | Password Code Vers. | CHAR | 1 | 0 | XUCODEVER2 | XUCODEVER2 |
| 16 | DATE_REF_FLD | Date Ref. Field |  | 0 | 0 |  |  |
| 17 | DURATION | Duration(from Last Logon) | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 18 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 19 | ERDAT | Creation Date of User Master | DATS | 8 | 0 | XUERDAT | DATUM |
| 20 | GLTGB | Valid to | DATS | 8 | 0 | XUGLTGB | DATUM |
| 21 | GLTGV | Valid from | DATS | 8 | 0 | XUGLTGV | DATUM |
| 22 | INIT_PWD_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 23 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 24 | LOCK_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 25 | LOCNT | Number of failed logon attempts | INT1 | 3 | 0 | XULOCNT | XULOCNT |
| 26 | LTIME | Last Logon Time | TIMS | 6 | 0 | XULTIME | UZEIT |
| 27 | MODBE | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 28 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 29 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 30 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 31 | OCOD1 | Initial Password | RAW | 8 | 0 | XUCODE | XUCODE |
| 32 | OCOD2 | Initial Password | RAW | 8 | 0 | XUCODE | XUCODE |
| 33 | OCOD3 | Initial Password | RAW | 8 | 0 | XUCODE | XUCODE |
| 34 | OCOD4 | Initial Password | RAW | 8 | 0 | XUCODE | XUCODE |
| 35 | OCOD5 | Initial Password | RAW | 8 | 0 | XUCODE | XUCODE |
| 36 | PASSCODE | Password Hash Val.(SAH1, 160 Bit) | RAW | 20 | 0 | PWD_SHA1 | SHA1_HASH |
| 37 | PWDCHGDATE | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 38 | PWDHISTORY | External Password History(USRPWDHISTORY) | INT1 | 3 | 0 | XUPWDHIST | IBOOLEAN |
| 39 | PWDINITIAL | Indicator: Password Is Initial | INT1 | 3 | 0 | XUPWDINIT | IBOOLEAN3 |
| 40 | PWDLGNDATE | Date of Last Password Logon | DATS | 8 | 0 | XULPDAT | DATUM |
| 41 | PWDLOCKDATE | Date: Password Lock | DATS | 8 | 0 | XUPLDAT | DATUM |
| 42 | PWDSETDATE | Date: Password Reset by Administrator | DATS | 8 | 0 | XUSPDAT | DATUM |
| 43 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 44 | STATE_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 45 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 46 | TRDAT | Last Logon Date | DATS | 8 | 0 | XULDATE | DATUM |
| 47 | TZONE | Time Zone | CHAR | 6 | 0 | TZNZONE | TZNZONE |
| 48 | UFLAG | User Lock Status | INT1 | 3 | 0 | XUUFLAG | XUUFLAG |
| 49 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 50 | USTYP | User Type | CHAR | 1 | 0 | XUUSTYP | XUUSTYP |
| 51 | VALID_USERS_ONLY | 'X' - Display only valid users |  | 0 | 0 |  |  |
| 52 | VERSN | User master record version | CHAR | 3 | 0 | XUVERSION | XUVERSION |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 52 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

ANAME (Creator of User Master Record)

Ensures reporting respects creator of user master record constraints carried by ANAME.

BACKDAYS (Days backwards from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

BCDA1 - BCDA5 (Date of Last Password Change)

Gives auditors traceable criteria because date of last password change on BCDA1 is applied consistently before any alert flag is raised.

BNAME (User)

Mirrors how administrators slice operational lists: user (BNAME) is one lever that shapes which rows are comparable run over run.

CLASS (User group)

Improves readability of exported lists because user group (CLASS) columns stay aligned with the configured filter intent.

CODV1 - CODV5 (Password Code Vers.)

Prevents accidental global scans when password code vers. (CODV1) is meant to stay within a controlled application slice.

CODVN (Password Code Vers.)

Ensures reporting respects password code vers. constraints carried by CODVN.

DATE_REF_FLD (Date Ref. Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

DATE_REF_FLD Options:

·        PWDCHGDATE — Date of Last Password Change.

·        PWDLGNDATE — Date of Last Password Logon.

·        PWDSETDATE — Date: Password Reset by Administrator.

·        PWDLOCKDATE — Date: Password Lock.

DURATION (Duration(from Last Logon))

Mirrors how administrators slice operational lists: duration(from last logon) (DURATION) is one lever that shapes which rows are comparable run over run.

DURATION_UNIT (Duration Unit(D/H/M))

Unit for elapsed time between each session's creation date and time and the evaluation clock.

DURATION_UNIT Options:

·        H — Hours.

·        M — Minutes (preset in code before the selection read when not overridden).

·        D — Days.

·        F — Full-day style counting where applicable to the duration helper.

ERDAT (Creation Date of User Master)

Reflects real administration where creation date of user master on ERDAT is routinely restricted to a single productive client or object family.

GLTGB (Valid to)

Treats valid to as a discriminator between similar rows that would otherwise look identical in a raw extract.

GLTGV (Valid from)

Allows phased rollout: first widen GLTGV for valid from, then tighten thresholds once baseline noise is understood.

INIT_PWD_ICON (State Icon)

Helps distinguish technical versus business attributes when state icon on INIT_PWD_ICON correlates with counters or status fields.

LANGU (Language for texts)

Explains why two monitoring passes differ: only the pass with stricter language for texts on LANGU surfaces the disputed rows.

LOCK_ICON (State Icon)

When harmonized with related filters, state icon on LOCK_ICON isolates the highest-risk record families.

LOCNT (Number of failed logon attempts)

Supports escalation where number of failed logon attempts on LOCNT signals ownership for follow-up between Basis and functional teams.

LTIME (Last Logon Time)

When combined with destination discipline, last logon time on LTIME keeps both breadth and depth of the extract intentional.

MODBE (Changed By)

Reduces false positives during peak windows by tightening changed by through MODBE alongside state filters.

NAME_FIRST (First Name)

Improves readability of exported lists because first name (NAME_FIRST) columns stay aligned with the configured filter intent.

NAME_LAST (Last Name)

Stabilizes week-over-week metrics by fixing last name (NAME_LAST) while allowing duration thresholds to move.

NAME_TEXT (Full Name)

When left open per framework rules, NAME_TEXT does not restrict full name; when set, only matching rows remain.

OCOD1 - OCOD5 (Initial Password)

When populated, keeps the extract focused so initial password (OCOD1) aligns with the intended triage slice.

PASSCODE (Password Hash Val.(SAH1, 160 Bit))

Pairs with duration logic: once PASSCODE passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

PWDCHGDATE (Date of Last Password Change)

Combines with related filters so date of last password change on PWDCHGDATE refines which records remain for duration or state checks.

PWDHISTORY (External Password History(USRPWDHISTORY))

Improves readability of exported lists because external password history(usrpwdhistory) (PWDHISTORY) columns stay aligned with the configured filter intent.

PWDINITIAL (Indicator: Password Is Initial)

Improves readability of exported lists because indicator: password is initial (PWDINITIAL) columns stay aligned with the configured filter intent.

PWDLGNDATE (Date of Last Password Logon)

After data is read, lines are removed unless date of last password logon on PWDLGNDATE still satisfies the active multivalued selection.

PWDLOCKDATE (Date: Password Lock)

Narrows retrieved rows where date: password lock (PWDLOCKDATE) must match the configured selection for this monitor.

PWDSETDATE (Date: Password Reset by Administrator)

When combined with destination discipline, date: password reset by administrator on PWDSETDATE keeps both breadth and depth of the extract intentional.

STATE_COLOR (State Color)

Filters lines by the derived color bucket used for severity-style triage in the monitor framework.

STATE_COLOR Options:

·        R — Red (error or failed-style outcomes).

·        G — Green (successful outcomes).

·        Y — Yellow (warning or in-process outcomes).

·        Additional literals may exist where the framework extends the palette for neutral states.

STATE_DESC (SW Message)

Separates cross-client noise from in-scope work when sw message on STATE_DESC correlates with client or user attributes.

STATE_ICON (State Icon)

For distributed landscapes, state icon on STATE_ICON often anchors which application server or destination appears in results.

TRDAT (Last Logon Date)

When left open per framework rules, TRDAT does not restrict last logon date; when set, only matching rows remain.

TZONE (Time Zone)

When left open per framework rules, TZONE does not restrict time zone; when set, only matching rows remain.

UFLAG (User Lock Status)

Treats user lock status as a discriminator between similar rows that would otherwise look identical in a raw extract.

USER_FLD (Dynamic Recipient User Field)

The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.
 How DRL Works:
 When USER_FLD is specified, the system extracts values from that field in the monitoring result set
 These extracted values are then used as recipient addresses for alert notifications
 This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored
 The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users

USTYP (User Type)

For distributed landscapes, user type on USTYP often anchors which application server or destination appears in results.

VALID_USERS_ONLY ('X' - Display only valid users)

Allows phased rollout: first widen VALID_USERS_ONLY for 'x' - display only valid users, then tighten thresholds once baseline noise is understood.

VERSN (User master record version)

Combines with related filters so user master record version on VERSN refines which records remain for duration or state checks.


### Parameter Relationships

How parameter combinations work together

DATUM supplies an explicit monitoring-date range when you populate it, so the evaluation clock for duration calculations and any monitor-supplied date context is anchored to calendar bounds you choose instead of relying only on relative lookback.

When DATUM is not provided, BACKDAYS is the fallback that builds the lower monitoring date from the evaluation day backward for the date axis the online monitor uses before attribute rows are aged.

DURATION and DURATION_UNIT act as an additional filter after date-oriented selection: only destinations whose computed elapsed interval from last change timestamp to the evaluation moment still fit the configured duration band remain in the extract.

Both the date criteria (explicit DATUM or BACKDAYS-driven window) and the DURATION / DURATION_UNIT age test are applied together—rows must satisfy the date side and the duration side before the result set is considered final for alerting.

MANAGE_IN_UTC shifts whether the evaluation clock used with DATUM and duration math follows UTC semantics versus local application-server time, so calendar and duration results stay consistent with how your landscape runs the monitor.


### Default Values

·        BACKDAYS - initial - treated as 3000 by code

·        DURATION - initial - treated as unconstrained by code

·        DURATION_UNIT - initial - treated as D by code


### Practical Configuration Examples

Use Case 1: Monitor password changes older than 90 days

BACKDAYS = 180

DATE_REF_FLD = PWDCHGDATE

DURATION > 90

DURATION_UNIT = D

STATE_COLOR = Y

Use Case 2: Track administrative password resets within last 30 days

BACKDAYS = 30

DATE_REF_FLD = PWDSETDATE

DURATION = 30

DURATION_UNIT = D

STATUS = RESET

Use Case 3: Monitor user lockout events with immediate notification

BACKDAYS = 7

DATE_REF_FLD = PWDLOCKDATE

DURATION = 24

DURATION_UNIT = H

STATE_COLOR = R

USER_FLD = BNAME

Use Case 4: Alert on security risk users requiring attention

BACKDAYS = 0

STATUS = LOCKED

STATE_COLOR = R

USER_FLD = CLASS

Use Case 5: Monitor authentication activity patterns for security analysis

BACKDAYS = 14

DATE_REF_FLD = PWDLGNDATE

DURATION = 7

DURATION_UNIT = D

USER_FLD = USTYP


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_20_USER_PWD_STATE | ANAME | Creator of the User Master Record | CHAR(12) | XUANAME |
| /SKN/S_SW_01_20_USER_PWD_STATE | BCDA1 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | BCDA2 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | BCDA3 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | BCDA4 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | BCDA5 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | BNAME | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_20_USER_PWD_STATE | CLASS | User group in user master maintenance | CHAR(12) | XUCLASS |
| /SKN/S_SW_01_20_USER_PWD_STATE | CODV1 | Code Version of Password Hash Algorithm (Old Systems) | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_01_20_USER_PWD_STATE | CODV2 | Code Version of Password Hash Algorithm (Old Systems) | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_01_20_USER_PWD_STATE | CODV3 | Code Version of Password Hash Algorithm (Old Systems) | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_01_20_USER_PWD_STATE | CODV4 | Code Version of Password Hash Algorithm (Old Systems) | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_01_20_USER_PWD_STATE | CODV5 | Code Version of Password Hash Algorithm (Old Systems) | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_01_20_USER_PWD_STATE | CODVN | Code Version of Password Hash Algorithm (New Systems) | CHAR(1) | XUCODEVER2 |
| /SKN/S_SW_01_20_USER_PWD_STATE | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_20_USER_PWD_STATE | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_20_USER_PWD_STATE | ERDAT | Creation Date of the User Master Record | DATS(8) | XUERDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | GLTGB | User valid to | DATS(8) | XUGLTGB |
| /SKN/S_SW_01_20_USER_PWD_STATE | GLTGV | User valid from | DATS(8) | XUGLTGV |
| /SKN/S_SW_01_20_USER_PWD_STATE | INIT_PWD_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_20_USER_PWD_STATE | LOCK_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_20_USER_PWD_STATE | LOCNT | Number of failed logon attempts | INT1(3) | XULOCNT |
| /SKN/S_SW_01_20_USER_PWD_STATE | LTIME | Last Logon Time | TIMS(6) | XULTIME |
| /SKN/S_SW_01_20_USER_PWD_STATE | MODBE | Last Changed By | CHAR(12) | XUMODIFIER |
| /SKN/S_SW_01_20_USER_PWD_STATE | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_01_20_USER_PWD_STATE | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_01_20_USER_PWD_STATE | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_01_20_USER_PWD_STATE | OCOD1 | Password Hash Key | RAW(8) | XUCODE |
| /SKN/S_SW_01_20_USER_PWD_STATE | OCOD2 | Password Hash Key | RAW(8) | XUCODE |
| /SKN/S_SW_01_20_USER_PWD_STATE | OCOD3 | Password Hash Key | RAW(8) | XUCODE |
| /SKN/S_SW_01_20_USER_PWD_STATE | OCOD4 | Password Hash Key | RAW(8) | XUCODE |
| /SKN/S_SW_01_20_USER_PWD_STATE | OCOD5 | Password Hash Key | RAW(8) | XUCODE |
| /SKN/S_SW_01_20_USER_PWD_STATE | PASSCODE | Password Hash Value (SHA1, 160 Bit) | RAW(20) | PWD_SHA1 |
| /SKN/S_SW_01_20_USER_PWD_STATE | PWDCHGDATE | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | PWDHISTORY | Indicator: Password History Stored in Table USRPWDHISTORY | INT1(3) | XUPWDHIST |
| /SKN/S_SW_01_20_USER_PWD_STATE | PWDINITIAL | Indicator: Password Is Initial (= Set by Administrator) | INT1(3) | XUPWDINIT |
| /SKN/S_SW_01_20_USER_PWD_STATE | PWDLGNDATE | Date of Last Password Logon | DATS(8) | XULPDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | PWDLOCKDATE | Date: Setting of Password Lock | DATS(8) | XUPLDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | PWDSETDATE | Date: Password Reset by Administrator | DATS(8) | XUSPDAT |
| /SKN/S_SW_01_20_USER_PWD_STATE | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_20_USER_PWD_STATE | STATE_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_20_USER_PWD_STATE | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_20_USER_PWD_STATE | TRDAT | Last Logon Date | DATS(8) | XULDATE |
| /SKN/S_SW_01_20_USER_PWD_STATE | TZONE | Time Zone | CHAR(6) | TZNZONE |
| /SKN/S_SW_01_20_USER_PWD_STATE | UFLAG | User Lock Status | INT1(3) | XUUFLAG |
| /SKN/S_SW_01_20_USER_PWD_STATE | USTYP | User Type | CHAR(1) | XUUSTYP |
| /SKN/S_SW_01_20_USER_PWD_STATE | VERSN | User master record version | CHAR(3) | XUVERSION |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_20_USER_PWD_STATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_01_20_USER_PWD_STATE
*"----------------------------------------------------------------------
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
DATA_SINGLE: MANAGE_IN_UTC       CHAR1 ,
             LANGU               LANGU,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             "NO_DATE_RESTRICTION CHAR1,
             VALID_USERS_ONLY    CHAR1, "Display only valid users
             DATE_REF_FLD        NAME_FELD.
DATA_MULTY: BNAME            XUBNAME,
              CLASS            XUCLASS,
              USTYP            XUUSTYP,
              UFLAG            XUUFLAG,  " Int 0/32/64/128
              "TRDAT            XULDATE,  " Last Logon
              "ERDAT            XUERDAT,   "Creation Date of the User Master Record
              ANAME            XUANAME,   "Creator of the User Master Record
              PWDCHGDATE       XUBCDAT,   "Date of Last Password Change
              PWDLGNDATE       XULPDAT,   "Date of Last Password Logon
              PWDSETDATE       XUSPDAT,   "Date: Password Reset by Administrator
              PWDLOCKDATE      XUPLDAT,   "Date: Setting of Password Lock
              "PWDSTATE         PWDCHGSTATE, "Password Change: Required / Allowed / Not Possible
              PWDINITIAL       XUPWDINIT, "Indicator: Password Is Initial (= Set by Administrator)
              STATE_COLOR      /SKN/E_SW_STATE_COLOR,  " G/Y/R
              DURATION         /SKN/E_SW_DURATION,
              DATUM            SYDATUM , " Paased by SW Online Monitor
              MODBE	            XUMODIFIER. "Changed By 13-9-16
  SELECT_MULTY:  BNAME,
                 CLASS,
                 USTYP,
                 UFLAG ,
                 "TRDAT ,
                 "ERDAT ,   "Creation Date of the User Master Record
                 ANAME,
                 STATE_COLOR,
                 DURATION,
                 DATUM ,
                 PWDCHGDATE,
                 PWDLGNDATE,
                 PWDSETDATE,
                 PWDLOCKDATE,
                 "PWDSTATE,
                 PWDINITIAL,
                 MODBE. "Changed By 13-9-16
   LV_LANGU = SY-LANGU.
   LV_DURATION_UNIT = 'D'.
   SELECT_SINGLE: LANGU,
                  MANAGE_IN_UTC,
                  DURATION_UNIT,
                  "NO_DATE_RESTRICTION,
                  VALID_USERS_ONLY,
                  DATE_REF_FLD.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_20_USER_PWD_STAT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA : DATE_FROM TYPE D,
         BACKDAYS  TYPE I.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : LS_DATA LIKE LINE OF T_DATA.
  DATA : TIME_DIFF TYPE I .
  DATA : SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
  DATA : REF_DATE LIKE SY-DATLO ,
         REF_TIME LIKE SY-TIMLO .
  "-----------------------------------------------
  " 2. Extracting & Populating Parameters        "
  "-----------------------------------------------
    LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      BACKDAYS = T_SELECT-LOW .
      DATE_FROM = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
      EXIT.
    ENDLOOP.
    IF R_DATUM[] IS INITIAL .  " Set default value
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      BACKDAYS = '3000' .  "--- Default
      DATE_FROM = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
    ENDIF .
*  ENDIF.
**************** SET DATE REFERENCE FIELD ************************
 " IF R_TRDAT[] IS INITIAL AND R_ERDAT[] IS INITIAL.
 "   R_TRDAT[] = R_DATUM[] .
 " ENDIF.
 IF R_PWDCHGDATE[] IS INITIAL AND R_PWDLGNDATE IS INITIAL AND R_PWDSETDATE IS INITIAL AND R_PWDLOCKDATE IS INITIAL.
    R_PWDCHGDATE[] = R_DATUM[] .
  ENDIF.
  CASE LV_DATE_REF_FLD.
     WHEN 'PWDCHGDATE'.
       R_PWDCHGDATE[] = R_DATUM[]. "Date of Last Password Change
     WHEN 'PWDLGNDATE'.
      R_PWDLGNDATE[] = R_DATUM[]. "" Date of Last Password Logon
     WHEN 'PWDSETDATE' .
       R_PWDSETDATE[] = R_DATUM[]. "Date: Password Reset by Administrator
     WHEN 'PWDLOCKDATE'.
       R_PWDLOCKDATE[] = R_DATUM[].  "Date: Setting of Password Lock
     WHEN OTHERS.
       R_PWDCHGDATE[] = R_DATUM[]. "Date of Last Password Change
*
   ENDCASE.
*************************************************************************
*  if lv_NO_DATE_RESTRICTION is not initial.
*    refresh R_TRDAT.
*  endif.
   SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
   TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert Data           "
  "-----------------------------------------------
***  SELECT *
***     FROM USR02
***     INTO CORRESPONDING FIELDS OF TABLE T_DATA
***     WHERE BNAME IN R_BNAME
***       AND CLASS IN R_CLASS  " User Group
***       AND USTYP IN R_USTYP  " User Type
***       AND UFLAG IN R_UFLAG  " Int 0/32/64/128
***       "AND TRDAT IN R_TRDAT  " Last Logon
***       "AND ERDAT IN R_ERDAT. "Creation date
***       AND PWDCHGDATE IN R_PWDCHGDATE
***       AND PWDLGNDATE IN R_PWDLGNDATE
***       AND PWDSETDATE IN R_PWDSETDATE
***       AND PWDLOCKDATE IN R_PWDLOCKDATE
***       "AND PWDSTATE   IN R_PWDSTATE
***       AND PWDINITIAL IN R_PWDINITIAL.
"*******13-9-16 ********************************************8
  SELECT *
     FROM USR02 AS A
     LEFT  OUTER JOIN USR04 AS B
      ON A~BNAME = B~BNAME
     INTO CORRESPONDING FIELDS OF TABLE T_DATA
     WHERE A~BNAME IN R_BNAME
       AND A~CLASS IN R_CLASS  " User Group
       AND A~USTYP IN R_USTYP  " User Type
       AND A~UFLAG IN R_UFLAG  " Int 0/32/64/128
       "AND TRDAT IN R_TRDAT  " Last Logon
       "AND ERDAT IN R_ERDAT. "Creation date
       AND A~PWDCHGDATE IN R_PWDCHGDATE
       AND A~PWDLGNDATE IN R_PWDLGNDATE
       AND A~PWDSETDATE IN R_PWDSETDATE
       AND A~PWDLOCKDATE IN R_PWDLOCKDATE
       "AND PWDSTATE   IN R_PWDSTATE
       AND A~PWDINITIAL IN R_PWDINITIAL.
      DELETE T_DATA WHERE MODBE NOT IN R_MODBE. "  13-9-16
  "-----------------------------------------------
  " 5. Post retrieving manipulations             "
  "-----------------------------------------------
  LOOP AT T_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    CLEAR: LS_DATA-STATE_COLOR,
           LS_DATA-STATE_ICON,
           LS_DATA-STATE_DESC,
           LS_DATA-LOCK_ICON.
    IF LS_DATA-UFLAG IS NOT INITIAL.
      LS_DATA-STATE_COLOR = 'R'.
      LS_DATA-STATE_ICON = STATE_ICON_RED.
      CALL FUNCTION '/SKN/F_SW_01_GET_LOCK_DESC'
        EXPORTING
          UFLAG            = LS_DATA-UFLAG
          LANGU            = LV_LANGU
       IMPORTING
         STATE_DESC       = LS_DATA-STATE_DESC.
      LS_DATA-LOCK_ICON = ICON_LOCKED.
    ELSE.
      LS_DATA-STATE_COLOR = 'G'.
      LS_DATA-STATE_ICON = STATE_ICON_GREEN.
    ENDIF.
    LS_DATA-INIT_PWD_ICON = ICON_INIT_PWD.
    CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES'
      EXPORTING
        BNAME      = LS_DATA-BNAME
      IMPORTING
        NAME_FIRST = LS_DATA-NAME_FIRST
        NAME_LAST  = LS_DATA-NAME_LAST
        NAME_TEXT  = LS_DATA-NAME_TEXT
      EXCEPTIONS
        NO_DATA    = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
      CLEAR: LS_DATA-NAME_FIRST,
             LS_DATA-NAME_LAST,
             LS_DATA-NAME_TEXT.
    ENDIF.
    MODIFY T_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
  "-----------------------------------------------
  " 6. Post retrieving filtering                 "
  "-----------------------------------------------
  DELETE T_DATA WHERE STATE_COLOR NOT IN R_STATE_COLOR.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
   REF_DATE = T_DATA-PWDCHGDATE.
   REF_TIME = SY_TIMLO.
   CASE LV_DATE_REF_FLD.
     WHEN 'PWDCHGDATE'.
       REF_DATE = T_DATA-PWDCHGDATE. "Date of Last Password Change
     WHEN 'PWDLGNDATE'.
       REF_DATE = T_DATA-PWDLGNDATE. "" Date of Last Password Logon
     WHEN 'PWDSETDATE' .
       REF_DATE = T_DATA-PWDSETDATE. "Date: Password Reset by Administrator
     WHEN 'PWDLOCKDATE'.
       REF_DATE = T_DATA-PWDLOCKDATE.  "Date: Setting of Password Lock
     ENDCASE.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = REF_DATE
          T_FROM            = REF_TIME
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
  IF LV_VALID_USERS_ONLY IS NOT INITIAL.
    LOOP AT T_DATA INTO LS_DATA.
      SY_TABIX = SY-TABIX .
      IF LS_DATA-GLTGV <= SY-DATUM AND LS_DATA-GLTGB >= SY-DATUM.
      ELSE.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.
  "-----------------------------------------------
  " 7. Finishing (Set IS_ALERT parameter)        "
  "-----------------------------------------------
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
