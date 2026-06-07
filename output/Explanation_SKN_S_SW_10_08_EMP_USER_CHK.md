# Exception Indicator: HR Employee - SAP User Association Check ( SW_10_08_EMP_USR_CHK)

## General Overview

This Exception Indicator cross-checks employees who have an SAP user ID assigned in HR communications data against their user master records, applies HR organizational and user-master filters, and highlights lock status and naming context for access-governance reviews.

This EI serves as an essential control for HR and security operations by:
- Surfacing active employees whose HR user assignment can be compared to the corresponding SAP user account in the same review cycle
- Enabling detection of locked or administratively flagged user accounts tied to personnel still in scope for the population you monitor
- Supporting identity and access teams with user type, user group, creator, and last-change metadata alongside HR assignment details
- Helping compliance programs demonstrate that HR-to-user linkage and user-master attributes were screened on a repeatable schedule
- Complementing manual SU01 or HR infotype checks with a consolidated exception list for follow-up

Typical use includes periodic access reviews after hires or transfers, checks before deprovisioning campaigns, and investigations when HR master data and security user records must align. Results are intended for exception workflows rather than full user or personnel directory exports.

The routine reads HR communication and master records valid on the evaluation date, resolves linked SAP user accounts, enriches rows with user-master and display attributes, and raises an alert when qualifying associations remain after duration and user-master filtering.


## Problem Description

Failure to monitor alignment between HR employee user assignments and SAP user master data creates multiple risks across access governance, HR operations, and audit readiness.

**Identity and Access Risks**
- Employees may retain SAP users that are locked, outdated, or inconsistent with HR assignment while still appearing active in HR extracts
- HR user identifiers without a matching or expected user-master profile are harder to detect without an automated cross-check
- User lock and user-type signals may be missed when HR and security teams work from separate lists

**HR Operations Risks**
- Transfers and reorganizations can leave communication infotype user assignments out of step with personnel area or status filters you intend to monitor
- Birthday or date-window logic combined with user linkage increases the chance of false positives unless populations are scoped deliberately

**Compliance and Audit Risks**
- Evidence of periodic HR-to-user reconciliation is weaker when reviews rely on manual SU01 and PA20 comparisons
- Creator and last-changed metadata on user masters is not assembled with HR context in one exception population

## Suggested Resolution

**Immediate Response**
- Review each flagged row for personnel number, HR user assignment, SAP user name, lock description, and employment status shown in the exception
- Confirm with HR and security whether the user should remain, be unlocked, or be deprovisioned according to policy
- Validate name text on the user master against HR name fields when mismatches drive the alert

**System Assessment**
- Compare this cycle to prior runs after mass hires, interface loads, or role-deployment projects
- Look for concentrations by user type, user group, personnel area, or lock state to see whether one batch job or team drives most items
- Revisit duration and user-master filters when the queue is dominated by expected locked service accounts

**Corrective Actions**
- Correct HR communication or user master data through standard personnel administration and user management with required approvals
- Adjust monitoring scope after root cause so the queue stays actionable for operations
- Route repeat linkage failures into identity governance or interface remediation when HR-to-user feeds require fixes
- Document approved exceptions when locked users must remain for technical or break-glass reasons


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACCNT | Account number | CHAR | 12 | 0 | XUACCNT | XUACCNT |
| 2 | AEDTM | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 3 | ANAME | Creator of User Master Record | CHAR | 12 | 0 | XUANAME | BNAME |
| 4 | BCDA1 | Date of Last Password Change | DATS | 8 | 0 | XUBCDAT | DATUM |
| 5 | BEGDA | Start Date | DATS | 8 | 0 | BEGDA | DATUM |
| 6 | BNAME | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 7 | BTRTL | Personnel subarea | CHAR | 4 | 0 | BTRTL | BTRTL |
| 8 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 9 | CLASS | User group | CHAR | 12 | 0 | XUCLASS | XUCLASS |
| 10 | CODV1 | Password Code Vers. | CHAR | 1 | 0 | XUCODEVERS | XUCODEVERS |
| 11 | CODVN | Password Code Vers. | CHAR | 1 | 0 | XUCODEVER2 | XUCODEVER2 |
| 12 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 13 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 14 | ENDDA | End Date | DATS | 8 | 0 | ENDDA | DATUM |
| 15 | ERDAT | Creation Date of User Master | DATS | 8 | 0 | XUERDAT | DATUM |
| 16 | GBDAT | Date of birth | DATS | 8 | 0 | GBDAT | GBDAT |
| 17 | GBJHR | Year of birth | NUMC | 4 | 0 | GBJHR | GJAHR |
| 18 | GBMON | Month of Birth | NUMC | 2 | 0 | GBMON | NUM2 |
| 19 | GBTAG | Birth Date (to Month/Year) | NUMC | 2 | 0 | GBTAG | NUM2 |
| 20 | GLTGB | Valid through | DATS | 8 | 0 | XUGLTGB | DATUM |
| 21 | GLTGV | Valid from | DATS | 8 | 0 | XUGLTGV | DATUM |
| 22 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 23 | LOCK_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 24 | LOCNT | Number of failed logon attempts | INT1 | 3 | 0 | XULOCNT | XULOCNT |
| 25 | LTIME | Last Logon Time | TIMS | 6 | 0 | XULTIME | UZEIT |
| 26 | MODBE | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 27 | NACHN | Last name | CHAR | 40 | 0 | PAD_NACHN | PAD_NACHN |
| 28 | NAME_FIRST | First name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 29 | NAME_LAST | Last name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 30 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 31 | OBJPS | Object ID | CHAR | 2 | 0 | OBJPS | OBJPS |
| 32 | PERNR | Personnel number | NUMC | 8 | 0 | PERSNO | PERSNO |
| 33 | PERSG | Employee group | CHAR | 1 | 0 | PERSG | PERSG |
| 34 | PERSK | Employee subgroup | CHAR | 2 | 0 | PERSK | PERSK |
| 35 | PLANS | Position | NUMC | 8 | 0 | PLANS | PLANS |
| 36 | PWDLGNDATE | Date of Last Password Logon | DATS | 8 | 0 | XULPDAT | DATUM |
| 37 | SEQNR | Infotype record no. | NUMC | 3 | 0 | SEQNR | NUM03 |
| 38 | SPRPS | Lock indicator | CHAR | 1 | 0 | SPRPS | SPRPS |
| 39 | STAT1 | Cust.-specific stat. | CHAR | 1 | 0 | STAT1 | STATA |
| 40 | STAT2 | Employment status | CHAR | 1 | 0 | STAT2 | STATA |
| 41 | STAT3 | Spec.payment status | CHAR | 1 | 0 | STAT3 | STATA |
| 42 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 43 | STATE_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 44 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 45 | SUBTY | Subtype | CHAR | 4 | 0 | SUBTY | SUBTY |
| 46 | TRDAT | Last Logon Date | DATS | 8 | 0 | XULDATE | DATUM |
| 47 | TZONE | Time Zone | CHAR | 6 | 0 | TZNZONE | TZNZONE |
| 48 | UFLAG | User Lock Status | INT1 | 3 | 0 | XUUFLAG | XUUFLAG |
| 49 | UNAME | Changed by | CHAR | 12 | 0 | AENAM | USNAM |
| 50 | USER | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 51 | USRID | HR - User | CHAR | 30 | 0 | SYSID | CHAR30 |
| 52 | USRTY | Communication type | CHAR | 4 | 0 | USRTY | SUBTY_591A |
| 53 | USTYP | User Type | CHAR | 1 | 0 | XUUSTYP | XUUSTYP |
| 54 | VDSK1 | Organizational key | CHAR | 14 | 0 | VDSK1 | VDSK1 |
| 55 | VERSN | User master record version | CHAR | 3 | 0 | XUVERSION | XUVERSION |
| 56 | VORNA | First name | CHAR | 40 | 0 | PAD_VORNA | PAD_VORNM |
| 57 | WERKS | Personnel area | CHAR | 4 | 0 | PERSA | PERSA |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 57 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ACCNT** (Account number)

Account number key used to scope user/account records in the monitored dataset.

**AEDTM** (Changed on)

Calendar date when a data record was last changed or modified.

**ANAME** (Creator of User Master Record)

User who created the master/user record; used for creator-based accountability filtering.

**BCDA1** (Date of Last Password Change)

Date of last password change field used in user-security aging and credential hygiene checks.

**BEGDA** (Start Date)

Guards against oversized extracts when start date on BEGDA is narrowed together with client, user, or session filters.

**BNAME** (User)

SAP user name used to restrict output to specific users or user populations.

**BTRTL** (Personnel subarea)

Technical code for Personnel Subarea, a small piece of a company used to set specific work hours, pay rules, and holidays for a group of workers.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**CLASS** (User group)

User group/class used to filter users by administrative classification.

**CODV1** (Password Code Vers.)

Password hash/version component used in user credential state analysis.

**CODVN** (Password Code Vers.)

Password hash version indicator used to detect outdated credential hash schemes.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**ENDDA** (End Date)

Mirrors how administrators slice operational lists: end date (ENDDA) is one lever that shapes which rows are comparable run over run.

**ERDAT** (Creation Date of User Master)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**GBDAT** (Date of birth)

Date of birth.

**GBJHR** (Year of birth)

Year of birth.

**GBMON** (Month of Birth)

Month of birth.

**GBTAG** (Birth Date (to Month/Year))

Day portion of the date of birth (just the day number, like 15, not 15.01.2000).

**GLTGB** (Valid through)

Valid-to date used to check whether authorization/master data is still active.

**GLTGV** (Valid from)

Valid-from date used to ensure records are active in the analyzed period.

**GSBER** (Business Area)

Business area key used for FI organizational reporting segmentation.

**LOCK_ICON** (State Icon)

Visual status icon indicating locked-state conditions in monitoring output.

**LOCNT** (Number of failed logon attempts)

<mark>Number of failed logon attempts.</mark>

**LTIME** (Last Logon Time)

<mark>Last logon time.</mark>

**MODBE** (Changed By)

<mark>User ID of the person who made the last change.</mark>

**NACHN** (Last name)

Last name.

**NAME_FIRST** (First name)

First name.

**NAME_LAST** (Last name)

Last name.

**NAME_TEXT** (Full Name)

Full name.

**OBJPS** (Object ID)

Stabilizes week-over-week metrics by fixing object id (OBJPS) while allowing duration thresholds to move.

**PERNR** (Personnel number)

Personnel Number, a unique eight-digit number assigned to every employee to track all their HR data and history.

**PERSG** (Employee group)

Employee Group is a main category used to classify workers into broad groups like active employees, retirees, or external staff.

**PERSK** (Employee subgroup)

Employee Subgroup is a division of the Employee Group used to set specific payroll rules, work schedules, and hourly or salaried pay status.

**PLANS** (Position)

PLANS is the technical field name for Position, a specific seven-digit code that represents a job slot filled by an employee within the company structure.

**PWDLGNDATE** (Date of Last Password Logon)

Date of last password logon usage used for credential-age/security checks.

**SEQNR** (Infotype record no.)

Pairs with duration logic: once SEQNR passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**SPRPS** (Lock indicator)

Lock Indicator is used to freeze a data record so it cannot be used in payroll or reporting until it is reviewed and approved.

**STAT1 - STAT3** (Cust.-specific stat.)

Valuable when comparing health before and after a release—hold cust.-specific stat. on STAT1 constant while varying other filters.

**STATE_COLOR** (State Color)

State selector used for quick triage via color-coded processing outcomes.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional literals may exist where the framework extends the palette for neutral states.

**STATE_DESC** (SW Message)

Human-readable state description used for alert/report interpretation.

**STATE_ICON** (State Icon)

Icon column paired with STATE_COLOR for UI/ALM rendering of status.

**SUBTY** (Subtype)

Valuable when comparing health before and after a release—hold subtype on SUBTY constant while varying other filters.

**TRDAT** (Last Logon Date)

Last logon date (or technical date marker) used for user activity recency checks.

**TZONE** (Time Zone)

Time zone key used to interpret and normalize timestamp fields.

**UFLAG** (User Lock Status)

User lock/status flag used to identify locked/disabled user states.

**UNAME** (Changed by)

SAP user name on business records

**USER** (User)

User identifier field used for actor-based filtering.

**USRID** (HR - User)

Explains why two monitoring passes differ: only the pass with stricter hr - user on USRID surfaces the disputed rows.

**USRTY** (Communication type)

Communication category, like email or user ID, for an employee record.

**USTYP** (User Type)

User type category used to segment dialog/system/service users.

**VDSK1** (Organizational key)

Organizational Key is a customizable 14-character code used to combine employee assignment details like company code, personnel area, or cost center into a single field for managing user security and authorizations.

**VERSN** (User master record version)

Version field used to separate records by versioned configuration/data state.

**VORNA** (First name)

First name

**WERKS** (Personnel area)

Plant key used to scope logistics/procurement records by site.


### Parameter Relationships

How parameter combinations work together

**HR-to-user linkage:** **USRID** (and the derived user name on each row) connects the HR communications assignment to the SAP user master record that is read and filtered in the second phase of processing.

**Employee population scope:** **PERNR**, **BUKRS**, **WERKS**, **PERSG**, **PERSK**, **BTRTL**, **PLANS**, and employment status fields (**STAT1**, **STAT2**, **STAT3**) narrow which employees enter the check before user master data is applied.

**User master filters:** **BNAME**, **CLASS**, **USTYP**, **ACCNT**, **UFLAG**, **TRDAT**, **ERDAT**, **ANAME**, and **MODBE** restrict which SAP user records remain after HR rows are built; lock status (**UFLAG**) also drives alert coloring and lock description text.

**Age filter after assembly:** **DURATION** with **DURATION_UNIT** is an additional filter applied after rows are assembled: each line must fit the configured elapsed-time band relative to the evaluation moment.

**Final selection:** Employee and status scope, HR user linkage, user-master attribute filters, and the duration band apply together—rows must satisfy the active combination before they appear in the final alert population.


### Default Values

- **DURATION_UNIT** - D
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Locked users for active employees in one personnel area**

**Purpose:** Highlight SAP users with a non-zero lock flag where HR shows an active employment status in a selected personnel area.
```
WERKS = 1000
STAT2 = 3
UFLAG = 32 - 128
DURATION = 30
DURATION_UNIT = D
```

**Use Case 2: Dialog users with recent logon window**

**Purpose:** Review dialog-type users linked to HR assignments where last logon falls in a recent interval.
```
USTYP = A
TRDAT = 20250101 - 20251231
USRID = *
PERNR = *
```

**Use Case 3: Exactly seven full days since assignment start**

**Purpose:** Flags HR-to-user rows whose assignment start date is exactly 7 full days ago, using full-day duration counting.
```
BNAME = SMITH*
MODBE = ADMIN*
DURATION = 7
DURATION_UNIT = F
CLASS = SUPER
```

**Use Case 4: Company and employee subgroup scope**

**Purpose:** Limit the population to selected company codes and employee subgroups before applying user group filters.
```
BUKRS = 1000 - 2000
PERSG = 1
PERSK = 01 - 99
ACCNT = *
ANAME = HR_BATCH
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_08_EMP_USER_CHK | ACCNT | Account number | CHAR(12) | XUACCNT |
| /SKN/S_SW_10_08_EMP_USER_CHK | AEDTM | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_08_EMP_USER_CHK | ANAME | Creator of User Master Record | CHAR(12) | XUANAME |
| /SKN/S_SW_10_08_EMP_USER_CHK | BCDA1 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_10_08_EMP_USER_CHK | BEGDA | Start Date | DATS(8) | BEGDA |
| /SKN/S_SW_10_08_EMP_USER_CHK | BNAME | User | CHAR(12) | XUBNAME |
| /SKN/S_SW_10_08_EMP_USER_CHK | BTRTL | Personnel subarea | CHAR(4) | BTRTL |
| /SKN/S_SW_10_08_EMP_USER_CHK | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_08_EMP_USER_CHK | CLASS | User group | CHAR(12) | XUCLASS |
| /SKN/S_SW_10_08_EMP_USER_CHK | CODV1 | Password Code Vers. | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_10_08_EMP_USER_CHK | CODVN | Password Code Vers. | CHAR(1) | XUCODEVER2 |
| /SKN/S_SW_10_08_EMP_USER_CHK | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_08_EMP_USER_CHK | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_08_EMP_USER_CHK | ENDDA | End Date | DATS(8) | ENDDA |
| /SKN/S_SW_10_08_EMP_USER_CHK | ERDAT | Creation Date of User Master | DATS(8) | XUERDAT |
| /SKN/S_SW_10_08_EMP_USER_CHK | GBDAT | Date of birth | DATS(8) | GBDAT |
| /SKN/S_SW_10_08_EMP_USER_CHK | GBJHR | Year of birth | NUMC(4) | GBJHR |
| /SKN/S_SW_10_08_EMP_USER_CHK | GBMON | Month of Birth | NUMC(2) | GBMON |
| /SKN/S_SW_10_08_EMP_USER_CHK | GBTAG | Birth Date (to Month/Year) | NUMC(2) | GBTAG |
| /SKN/S_SW_10_08_EMP_USER_CHK | GLTGB | Valid through | DATS(8) | XUGLTGB |
| /SKN/S_SW_10_08_EMP_USER_CHK | GLTGV | Valid from | DATS(8) | XUGLTGV |
| /SKN/S_SW_10_08_EMP_USER_CHK | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_08_EMP_USER_CHK | LOCK_ICON | State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_10_08_EMP_USER_CHK | LOCNT | Number of failed logon attempts | INT1(3) | XULOCNT |
| /SKN/S_SW_10_08_EMP_USER_CHK | LTIME | Last Logon Time | TIMS(6) | XULTIME |
| /SKN/S_SW_10_08_EMP_USER_CHK | MODBE | Changed By | CHAR(12) | XUMODIFIER |
| /SKN/S_SW_10_08_EMP_USER_CHK | NACHN | Last name | CHAR(40) | PAD_NACHN |
| /SKN/S_SW_10_08_EMP_USER_CHK | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_08_EMP_USER_CHK | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_08_EMP_USER_CHK | NAME_TEXT | Full Name | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_08_EMP_USER_CHK | OBJPS | Object ID | CHAR(2) | OBJPS |
| /SKN/S_SW_10_08_EMP_USER_CHK | PERNR | Personnel number | NUMC(8) | PERSNO |
| /SKN/S_SW_10_08_EMP_USER_CHK | PERSG | Employee group | CHAR(1) | PERSG |
| /SKN/S_SW_10_08_EMP_USER_CHK | PERSK | Employee subgroup | CHAR(2) | PERSK |
| /SKN/S_SW_10_08_EMP_USER_CHK | PLANS | Position | NUMC(8) | PLANS |
| /SKN/S_SW_10_08_EMP_USER_CHK | PWDLGNDATE | Date of Last Password Logon | DATS(8) | XULPDAT |
| /SKN/S_SW_10_08_EMP_USER_CHK | SEQNR | Infotype record no. | NUMC(3) | SEQNR |
| /SKN/S_SW_10_08_EMP_USER_CHK | SPRPS | Lock indicator | CHAR(1) | SPRPS |
| /SKN/S_SW_10_08_EMP_USER_CHK | STAT1 | Cust.-specific stat. | CHAR(1) | STAT1 |
| /SKN/S_SW_10_08_EMP_USER_CHK | STAT2 | Employment status | CHAR(1) | STAT2 |
| /SKN/S_SW_10_08_EMP_USER_CHK | STAT3 | Spec.payment status | CHAR(1) | STAT3 |
| /SKN/S_SW_10_08_EMP_USER_CHK | STATE_COLOR | State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_10_08_EMP_USER_CHK | STATE_DESC | SW Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_10_08_EMP_USER_CHK | STATE_ICON | State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_10_08_EMP_USER_CHK | SUBTY | Subtype | CHAR(4) | SUBTY |
| /SKN/S_SW_10_08_EMP_USER_CHK | TRDAT | Last Logon Date | DATS(8) | XULDATE |
| /SKN/S_SW_10_08_EMP_USER_CHK | TZONE | Time Zone | CHAR(6) | TZNZONE |
| /SKN/S_SW_10_08_EMP_USER_CHK | UFLAG | User Lock Status | INT1(3) | XUUFLAG |
| /SKN/S_SW_10_08_EMP_USER_CHK | UNAME | Changed by | CHAR(12) | AENAM |
| /SKN/S_SW_10_08_EMP_USER_CHK | USER | User | CHAR(12) | XUBNAME |
| /SKN/S_SW_10_08_EMP_USER_CHK | USRID | HR - User | CHAR(30) | SYSID |
| /SKN/S_SW_10_08_EMP_USER_CHK | USRTY | Communication type | CHAR(4) | USRTY |
| /SKN/S_SW_10_08_EMP_USER_CHK | USTYP | User Type | CHAR(1) | XUUSTYP |
| /SKN/S_SW_10_08_EMP_USER_CHK | VDSK1 | Organizational key | CHAR(14) | VDSK1 |
| /SKN/S_SW_10_08_EMP_USER_CHK | VERSN | User master record version | CHAR(3) | XUVERSION |
| /SKN/S_SW_10_08_EMP_USER_CHK | VORNA | First name | CHAR(40) | PAD_VORNA |
| /SKN/S_SW_10_08_EMP_USER_CHK | WERKS | Personnel area | CHAR(4) | PERSA |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_08_EMP_USER_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_08_EMP_USER_CHK OPTIONAL
*"----------------------------------------------------------------------
TYPES : T_MONTDAY(4) TYPE N.
DATA : DATE_FROM LIKE SY-DATUM,
       DATE_TO LIKE SY-DATUM .
DATA : MONTH_FROM TYPE GBMON,
       MONTH_TO TYPE GBMON.
DATA : MONTDAY_FROM TYPE T_MONTDAY,
       MONTDAY_TO TYPE T_MONTDAY.
DATA : LV_MONTDAY TYPE T_MONTDAY,
       LV_DAY TYPE GBTAG,
       LV_MONTH TYPE GBMON,
       LV_DATE LIKE SY-DATUM,
       LV_YEAR TYPE GBJHR.
DATA_MULTY : MONTDAY T_MONTDAY,
              MONTH GBMON,
              DAY GBTAG.
DATA: LV_OBJID TYPE  REALO,
      LV_LEADER_ID TYPE  REALO.
DATA : TIME_DIFF TYPE  INT4 .
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LS_DATA LIKE LINE OF T_DATA.
DATA: LS_PA0000 TYPE PA0000,
      LT_PA0000 LIKE TABLE OF LS_PA0000 WITH HEADER LINE.
DATA: WA_PA0002  TYPE  PA0002.
DATA: LS_PA0002 TYPE PA0002,
      LT_PA0002 LIKE TABLE OF LS_PA0002.
DATA: WA_PA0001  TYPE  PA0001.
DATA: LS_PA0001 TYPE PA0001,
      LT_PA0001 LIKE TABLE OF LS_PA0001.
DATA: LS_PA0105 TYPE PA0105,
      LT_PA0105 LIKE TABLE OF LS_PA0105 WITH HEADER LINE.
DATA: LS_USR02 TYPE USR02,
      LT_USR02 LIKE TABLE OF LS_USR02 WITH HEADER LINE.
DATA_SINGLE: BACKDAYS        INT4,
             FORWDAYS        INT4,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             PLVAR          PLVAR,
             OTYPE          OTYPE,
             LANGU          LANGU.
*
 LV_BACKDAYS = 0.
 LV_FORWDAYS = 1.
 LV_DURATION_UNIT = 'D'.
 LV_PLVAR = '01'.
 LV_OTYPE = 'S'.
 LV_LANGU = SY-LANGU.
*
 SELECT_SINGLE: BACKDAYS,
                FORWDAYS,
                DURATION_UNIT,
                PLVAR,
                OTYPE,
                LANGU.
*
*
DATA_MULTY: PERNR       PERSNO,
            BUKRS       BUKRS,
            WERKS       PERSA,
            PERSG       PERSG,
            PERSK       PERSK,
            BTRTL       BTRTL,
            PLANS       PLANS,
            USRID       SYSID,
            DURATION   /SKN/E_SW_DURATION,
            DATUM        SY-DATUM,
            STAT1       STAT1,
            STAT2       STAT2,
            STAT3       STAT3.
SELECT_MULTY:
            PERNR,
            BUKRS,
            WERKS,
            PERSG,
            PERSK,
            BTRTL,
            PLANS,
            USRID,
            DURATION,
            DATUM ,
            STAT1,
            STAT2,
            STAT3.
SELECT_MULTY : MONTH,
               DAY.
*
DATA_MULTY:   BNAME            XUBNAME,
              CLASS            XUCLASS,
              USTYP            XUUSTYP,
              ACCNT            XUACCNT,
              UFLAG            XUUFLAG,  " Int 0/32/64/128
              TRDAT            XULDATE,  " Last Logon
              ERDAT            XUERDAT,   "Creation Date of the User Master Record
              ANAME            XUANAME,   "Creator of the User Master Record
              MODBE	            XUMODIFIER. "Changed By 12-9-16
SELECT_MULTY: BNAME,
              CLASS,
              USTYP,
              ACCNT,
              UFLAG ,
              TRDAT ,
              ERDAT ,   "Creation Date of the User Master Record
              ANAME,
              MODBE.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_08_EMP_USER_CHK'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
    SY_DATLO = SY-DATUM .        " Appl Server's Date
    SY_TIMLO = SY-UZEIT.
********************************************************
    DATE_FROM = SY-DATUM.
    DATE_TO = SY-DATUM.
   IF R_DATUM[] IS INITIAL .
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'BT' .
      DATE_FROM = SY-DATUM - LV_BACKDAYS .
      DATE_TO = SY-DATUM + LV_FORWDAYS.
      RS_DATUM-LOW = DATE_FROM .
      RS_DATUM-HIGH = DATE_TO .
      APPEND RS_DATUM TO R_DATUM.
   ELSE.
     READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
     IF SY-SUBRC IS INITIAL.
       DATE_FROM = RS_DATUM-LOW.
       DATE_TO = RS_DATUM-HIGH.
       IF DATE_TO < DATE_FROM.
         DATE_TO = DATE_FROM.
       ENDIF.
     ENDIF.
   ENDIF.
   MONTH_FROM = DATE_FROM+4(2).
   MONTH_TO = DATE_TO+4(2).
   MONTDAY_FROM = DATE_FROM+4(4).
   MONTDAY_TO = DATE_TO+4(4).
   REFRESH R_MONTDAY.
   LV_MONTDAY = MONTDAY_FROM.
   LV_DATE = SY-DATUM.
   IF MONTH_FROM > MONTH_TO.
     LV_DATE+4(4) = '0101' . LV_DATE = LV_DATE - 1. "Prev Year
     LV_DATE+4(4) = LV_MONTDAY.
   ELSE.
      LV_DATE+4(4) = LV_MONTDAY.
   ENDIF.
    DO.
      RS_MONTDAY-SIGN = 'I'.
       RS_MONTDAY-OPTION = 'EQ'.
        RS_MONTDAY-LOW = LV_MONTDAY.
      APPEND RS_MONTDAY TO R_MONTDAY.
      "--- Increment
      ADD 1 TO LV_DATE.
      IF LV_DATE > DATE_TO.
        EXIT.
      ENDIF.
      LV_MONTDAY = LV_DATE+4(4).
    ENDDO.
***    refresh: R_MONTH,
***             R_DAY.
***    loop at R_MONTDAY into RS_MONTDAY.
***      MOVE-CORRESPONDING RS_MONTDAY to RS_MONTH.
***       RS_MONTH-low = RS_MONTDAY-low+0(2).
***      append RS_MONTH to R_MONTH.
***      MOVE-CORRESPONDING RS_MONTDAY to RS_DAY.
***       RS_DAY-low = RS_MONTDAY-low+2(2).
***      append RS_DAY to R_DAY.
***    endloop.
***
***    sort R_MONTH.
***    delete ADJACENT DUPLICATES FROM R_MONTH.
***    sort R_DAY.
***    delete ADJACENT DUPLICATES FROM  R_DAY.
*********************************************************
**--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  "--- Get Employes with USER
  SELECT *
     FROM PA0105
     INTO CORRESPONDING FIELDS OF TABLE LT_PA0105
     WHERE PERNR IN R_PERNR
       AND USRTY = '0001'
       AND BEGDA = SY-DATUM
       AND USRID > ' '
       AND USRID IN R_USRID.
  CHECK LT_PA0105[] IS NOT INITIAL.
  SELECT *
     FROM PA0002
     INTO CORRESPONDING FIELDS OF TABLE LT_PA0002
     FOR ALL ENTRIES IN LT_PA0105
       WHERE  PERNR = LT_PA0105-PERNR
         AND BEGDA = SY-DATUM
         AND GBMON IN R_MONTH
         AND GBTAG IN R_DAY.
     SELECT *
       FROM PA0001
       INTO CORRESPONDING FIELDS OF TABLE LT_PA0001
       FOR ALL ENTRIES IN LT_PA0105
       WHERE  PERNR = LT_PA0105-PERNR
          AND BUKRS IN R_BUKRS
          AND WERKS IN R_WERKS
          AND PERSG IN R_PERSG
          AND PERSK IN R_PERSK
          AND BTRTL IN R_BTRTL
          AND PLANS IN R_PLANS
          AND BEGDA   = SY-DATUM.
     SELECT *
       FROM PA0000
       INTO CORRESPONDING FIELDS OF TABLE LT_PA0000
       FOR ALL ENTRIES IN LT_PA0105
       WHERE  PERNR = LT_PA0105-PERNR
          AND STAT1 IN R_STAT1
          AND STAT2 IN R_STAT2
          AND STAT3 IN R_STAT3
          AND BEGDA   = SY-DATUM.
  REFRESH T_DATA.
  LOOP AT LT_PA0105 INTO LS_PA0105.
    LOOP AT LT_PA0002 INTO LS_PA0002 WHERE PERNR = LS_PA0105-PERNR
                                       AND BEGDA = SY-DATUM.      .
      LOOP AT LT_PA0001 INTO LS_PA0001 WHERE PERNR = LS_PA0105-PERNR
                                         AND BEGDA = SY-DATUM.
        LOOP AT LT_PA0000 INTO LS_PA0000 WHERE PERNR = LS_PA0105-PERNR
                                           AND BEGDA = SY-DATUM.
          MOVE-CORRESPONDING LS_PA0000 TO LS_DATA.
          MOVE-CORRESPONDING LS_PA0001 TO LS_DATA.
          MOVE-CORRESPONDING LS_PA0002 TO LS_DATA.
          MOVE-CORRESPONDING LS_PA0105 TO LS_DATA.
          LS_DATA-USER = LS_DATA-USRID.
          APPEND LS_DATA TO T_DATA.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
*********************
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
   LV_DATE = T_DATA-BEGDA.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = LV_DATE
          T_FROM            = SY_TIMLO
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
*************
 "--- Get User's Data
  SELECT *
     FROM USR02
     INTO CORRESPONDING FIELDS OF TABLE LT_USR02
     FOR ALL ENTRIES IN T_DATA
       WHERE  BNAME = T_DATA-USER.
  SORT LT_USR02 BY BNAME.
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   READ TABLE LT_USR02 INTO LS_USR02
                       WITH KEY BNAME = T_DATA-USER
                       BINARY SEARCH.
   IF SY-SUBRC IS INITIAL.
     MOVE-CORRESPONDING LS_USR02 TO T_DATA.
     MODIFY T_DATA INDEX SY_TABIX .
   ENDIF.
 ENDLOOP .
 DELETE T_DATA WHERE BNAME NOT IN R_BNAME.
 DELETE T_DATA WHERE CLASS NOT IN R_CLASS.
 DELETE T_DATA WHERE USTYP NOT IN R_USTYP.
 DELETE T_DATA WHERE ACCNT NOT IN R_ACCNT.
 DELETE T_DATA WHERE UFLAG NOT IN R_UFLAG.
 DELETE T_DATA WHERE TRDAT NOT IN R_TRDAT.
 DELETE T_DATA WHERE ERDAT NOT IN R_ERDAT.
 DELETE T_DATA WHERE ANAME NOT IN R_ANAME.
 DELETE T_DATA WHERE MODBE NOT IN R_MODBE.
 LOOP AT T_DATA INTO LS_DATA.
   SY_TABIX = SY-TABIX .
    CLEAR: LS_DATA-STATE_DESC,
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
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
