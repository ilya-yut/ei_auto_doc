# Exception Indicator: HR SAP User - Employee Association Check ( SW_10_08_USR_EMP_CHK)

## General Overview

This Exception Indicator reviews SAP user master records, resolves the linked employee number from a configurable user-master reference field, and checks whether that employee aligns with HR master and communications data for the monitoring window—including optional birthday filtering and an employee-match consistency flag.

This EI serves as an essential control for identity and access governance by:
- Surfacing SAP users whose assigned employee reference does not match the personnel number on HR infotype data
- Enabling detection of locked or invalid users while still tied to HR records that fail association checks
- Supporting access reviews with user type, account, and validity filters alongside HR organizational context when a match exists
- Helping security and HR teams prioritize mismatches between user master and employee master before access certification cycles
- Complementing manual SU01 and HR comparisons with a repeatable exception population driven from the user side inward

Typical use includes periodic user-to-employee reconciliation, reviews after account provisioning changes, and checks that dialog users reference the correct personnel number. Results are intended for exception workflows rather than full user or HR directory exports.

The routine reads user master candidates, derives the employee number from the selected reference field, enriches rows with HR data when available, evaluates the employee-match indicator, and raises an alert when qualifying inconsistencies remain after duration filtering.


## Problem Description

Failure to monitor whether SAP user accounts point to the correct employee master record creates multiple risks across access governance, HR data quality, and audit readiness.

**Identity and Access Risks**
- Users may retain productive access while referencing a wrong or empty employee number in the user master
- Provisioning errors that set account number instead of personnel number are hard to detect without a user-first cross-check
- Locked or expired users tied to mismatched HR links may be overlooked in user-only review lists

**HR and Master Data Risks**
- HR reorganizations and re-hires can leave user masters pointing to obsolete personnel numbers
- Birthday-window monitoring combined with user filters may surface large populations unless association rules are applied consistently

**Compliance and Operational Risks**
- Evidence of user-to-employee reconciliation is weaker when reviews rely on manual SU01 and PA20 comparisons
- Service and dialog accounts without a valid employee link may violate policy when still active in user master

## Suggested Resolution

**Immediate Response**
- Review each flagged user for account name, employee reference, match indicator, lock status, and HR name or assignment fields shown in the exception
- Confirm with HR and security whether the user should be corrected, deactivated, or formally accepted as an exception
- Prioritize rows where the consistency indicator shows the employee number does not equal the HR personnel number on the assignment

**System Assessment**
- Compare this cycle to prior runs after mass user imports, role deployments, or HR interface updates
- Look for concentrations by user type, user group, or creator to see whether one batch process drives most mismatches
- Revisit validity-only and employee-match filters when the queue mixes expected technical accounts with true data errors

**Corrective Actions**
- Correct user master or HR reference data through standard user administration and personnel maintenance with required approvals
- Adjust monitoring scope after root cause so the queue stays actionable for operations
- Route repeat interface or provisioning defects into identity governance when employee numbers are systematically wrong on new users
- Document approved exceptions for break-glass or shared accounts that intentionally lack a one-to-one employee link


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
| 14 | EMPLOYEE | Personnel number | NUMC | 8 | 0 | PERSNO | PERSNO |
| 15 | EMPLOYEE_OK | Consistency Indicator | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 16 | ENDDA | End Date | DATS | 8 | 0 | ENDDA | DATUM |
| 17 | ERDAT | Creation Date of User Master | DATS | 8 | 0 | XUERDAT | DATUM |
| 18 | GBDAT | Date of birth | DATS | 8 | 0 | GBDAT | GBDAT |
| 19 | GBJHR | Year of birth | NUMC | 4 | 0 | GBJHR | GJAHR |
| 20 | GBMON | Month of Birth | NUMC | 2 | 0 | GBMON | NUM2 |
| 21 | GBTAG | Birth Date (to Month/Year) | NUMC | 2 | 0 | GBTAG | NUM2 |
| 22 | GLTGB | Valid through | DATS | 8 | 0 | XUGLTGB | DATUM |
| 23 | GLTGV | Valid from | DATS | 8 | 0 | XUGLTGV | DATUM |
| 24 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 25 | LANGU | Descriptions languege |  | 0 | 0 |  |  |
| 26 | LOCK_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 27 | LOCNT | Number of failed logon attempts | INT1 | 3 | 0 | XULOCNT | XULOCNT |
| 28 | LTIME | Last Logon Time | TIMS | 6 | 0 | XULTIME | UZEIT |
| 29 | MODBE | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 30 | NACHN | Last name | CHAR | 40 | 0 | PAD_NACHN | PAD_NACHN |
| 31 | NAME_FIRST | First name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 32 | NAME_LAST | Last name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 33 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 34 | OBJPS | Object ID | CHAR | 2 | 0 | OBJPS | OBJPS |
| 35 | PERNR | Personnel number | NUMC | 8 | 0 | PERSNO | PERSNO |
| 36 | PERSG | Employee group | CHAR | 1 | 0 | PERSG | PERSG |
| 37 | PERSK | Employee subgroup | CHAR | 2 | 0 | PERSK | PERSK |
| 38 | PLANS | Position | NUMC | 8 | 0 | PLANS | PLANS |
| 39 | PWDLGNDATE | Date of Last Password Logon | DATS | 8 | 0 | XULPDAT | DATUM |
| 40 | SEQNR | Infotype record no. | NUMC | 3 | 0 | SEQNR | NUM03 |
| 41 | SPRPS | Lock indicator | CHAR | 1 | 0 | SPRPS | SPRPS |
| 42 | STAT1 | Cust.-specific stat. | CHAR | 1 | 0 | STAT1 | STATA |
| 43 | STAT2 | Employment status | CHAR | 1 | 0 | STAT2 | STATA |
| 44 | STAT3 | Spec.payment status | CHAR | 1 | 0 | STAT3 | STATA |
| 45 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 46 | STATE_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 47 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 48 | SUBTY | Subtype | CHAR | 4 | 0 | SUBTY | SUBTY |
| 49 | TRDAT | Last Logon Date | DATS | 8 | 0 | XULDATE | DATUM |
| 50 | TZONE | Time Zone | CHAR | 6 | 0 | TZNZONE | TZNZONE |
| 51 | UFLAG | User Lock Status | INT1 | 3 | 0 | XUUFLAG | XUUFLAG |
| 52 | UNAME | Changed by | CHAR | 12 | 0 | AENAM | USNAM |
| 53 | USER | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 54 | USRID | System ID | CHAR | 30 | 0 | SYSID | CHAR30 |
| 55 | USRTY | Communication type | CHAR | 4 | 0 | USRTY | SUBTY_591A |
| 56 | USTYP | User Type | CHAR | 1 | 0 | XUUSTYP | XUUSTYP |
| 57 | VALID_USERS_ONLY | 'X' - Display only valid users |  | 0 | 0 |  |  |
| 58 | VDSK1 | Organizational key | CHAR | 14 | 0 | VDSK1 | VDSK1 |
| 59 | VERSN | User master record version | CHAR | 3 | 0 | XUVERSION | XUVERSION |
| 60 | VORNA | First name | CHAR | 40 | 0 | PAD_VORNA | PAD_VORNM |
| 61 | WERKS | Personnel area | CHAR | 4 | 0 | PERSA | PERSA |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 61 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

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

**EMPLOYEE** (Personnel number)

Helps distinguish technical versus business attributes when personnel number on EMPLOYEE correlates with counters or status fields.

**EMPLOYEE_OK** (Consistency Indicator)

When harmonized with related filters, consistency indicator on EMPLOYEE_OK isolates the highest-risk record families.

**ENDDA** (End Date)

For operations, end date on ENDDA indicates whether a row belongs in the current monitoring pass versus historical noise.

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

**LANGU** (Descriptions languege)

Language key used for language-dependent texts and user-language filtering.

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

After data is read, lines are removed unless object id on OBJPS still satisfies the active multivalued selection.

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

Treats infotype record no. as a discriminator between similar rows that would otherwise look identical in a raw extract.

**SPRPS** (Lock indicator)

Lock Indicator is used to freeze a data record so it cannot be used in payroll or reporting until it is reviewed and approved.

**STAT1 - STAT3** (Cust.-specific stat.)

Reflects real administration where cust.-specific stat. on STAT1 is routinely restricted to a single productive client or object family.

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

Reflects real administration where subtype on SUBTY is routinely restricted to a single productive client or object family.

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

**USRID** (System ID)

Connects to alert semantics: rows removed for failing system id on USRID never reach downstream filtering.

**USRTY** (Communication type)

Communication category, like email or user ID, for an employee record.

**USTYP** (User Type)

User type category used to segment dialog/system/service users.

**VALID_USERS_ONLY** ('X' - Display only valid users)

Boolean flag to restrict results to users validated as active/allowed.

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

**User-side population:** **BNAME**, **CLASS**, **USTYP**, **ACCNT**, **UFLAG**, **TRDAT**, **ERDAT**, and **ANAME** narrow which SAP user master records enter the check before any HR data is joined.

**Employee reference resolution:** **EMP_REF_FLD** (when present in configuration) determines which user-master field supplies the employee number compared against HR (**EMPLOYEE** versus **PERNR**).

**Match quality:** **EMPLOYEE_OK** filters rows where the resolved employee number equals the HR personnel number on the assignment, helping separate true mismatches from aligned records.

**Validity narrowing:** **VALID_USERS_ONLY** restricts the user population to accounts valid on the evaluation date when active.

**HR context filters:** **PERNR**, **BUKRS**, **WERKS**, **PERSG**, **PERSK**, **BTRTL**, **PLANS**, and employment status fields (**STAT1**, **STAT2**, **STAT3**) apply after HR infotype data is joined to each user row.

**Age filter after assembly:** **DURATION** with **DURATION_UNIT** is an additional filter based on elapsed time from the user’s last logon date to the evaluation moment.

**Final selection:** User-master scope, employee reference and match rules, optional validity filtering, HR organizational filters, and the duration band apply together before rows appear in the final alert population.


### Default Values

- **DURATION_UNIT** - D
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Dialog users with employee mismatch**

**Purpose:** Flag dialog-type users where the consistency indicator shows the HR personnel number does not match the employee derived from the user master.
```
USTYP = A
BNAME = *
VALID_USERS_ONLY = X
CLASS = SUPER
PERNR = *
```

**Use Case 2: Locked users still tied to an HR row**

**Purpose:** Review locked accounts that remain valid on the evaluation date and show an employee reference for follow-up with security operations.
```
UFLAG = 32
VALID_USERS_ONLY = X
BTRTL = 1000
STAT2 = 3
```

**Use Case 3: Exactly fourteen full days since last logon**

**Purpose:** Flags user-to-employee rows whose last logon date is exactly 14 full days ago, using full-day duration counting.
```
DURATION = 14
DURATION_UNIT = F
USTYP = A
ACCNT = *
ANAME = HR_BATCH
```

**Use Case 4: Service accounts with recent creation date**

**Purpose:** Narrow to recently created service-type users before checking employee association on a selected company code pattern.
```
USTYP = S
ERDAT = 20250101 - 20250331
BUKRS = 1000
PERSG = 1
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_08_USER_EMP_CHK | ACCNT | Account number | CHAR(12) | XUACCNT |
| /SKN/S_SW_10_08_USER_EMP_CHK | AEDTM | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_08_USER_EMP_CHK | ANAME | Creator of User Master Record | CHAR(12) | XUANAME |
| /SKN/S_SW_10_08_USER_EMP_CHK | BCDA1 | Date of Last Password Change | DATS(8) | XUBCDAT |
| /SKN/S_SW_10_08_USER_EMP_CHK | BEGDA | Start Date | DATS(8) | BEGDA |
| /SKN/S_SW_10_08_USER_EMP_CHK | BNAME | User | CHAR(12) | XUBNAME |
| /SKN/S_SW_10_08_USER_EMP_CHK | BTRTL | Personnel subarea | CHAR(4) | BTRTL |
| /SKN/S_SW_10_08_USER_EMP_CHK | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_08_USER_EMP_CHK | CLASS | User group | CHAR(12) | XUCLASS |
| /SKN/S_SW_10_08_USER_EMP_CHK | CODV1 | Password Code Vers. | CHAR(1) | XUCODEVERS |
| /SKN/S_SW_10_08_USER_EMP_CHK | CODVN | Password Code Vers. | CHAR(1) | XUCODEVER2 |
| /SKN/S_SW_10_08_USER_EMP_CHK | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_08_USER_EMP_CHK | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_08_USER_EMP_CHK | EMPLOYEE | Personnel number | NUMC(8) | PERSNO |
| /SKN/S_SW_10_08_USER_EMP_CHK | EMPLOYEE_OK | Consistency Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_08_USER_EMP_CHK | ENDDA | End Date | DATS(8) | ENDDA |
| /SKN/S_SW_10_08_USER_EMP_CHK | ERDAT | Creation Date of User Master | DATS(8) | XUERDAT |
| /SKN/S_SW_10_08_USER_EMP_CHK | GBDAT | Date of birth | DATS(8) | GBDAT |
| /SKN/S_SW_10_08_USER_EMP_CHK | GBJHR | Year of birth | NUMC(4) | GBJHR |
| /SKN/S_SW_10_08_USER_EMP_CHK | GBMON | Month of Birth | NUMC(2) | GBMON |
| /SKN/S_SW_10_08_USER_EMP_CHK | GBTAG | Birth Date (to Month/Year) | NUMC(2) | GBTAG |
| /SKN/S_SW_10_08_USER_EMP_CHK | GLTGB | Valid through | DATS(8) | XUGLTGB |
| /SKN/S_SW_10_08_USER_EMP_CHK | GLTGV | Valid from | DATS(8) | XUGLTGV |
| /SKN/S_SW_10_08_USER_EMP_CHK | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_08_USER_EMP_CHK | LANGU | Descriptions languege | CHAR(0) | LANGU |
| /SKN/S_SW_10_08_USER_EMP_CHK | LOCK_ICON | State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_10_08_USER_EMP_CHK | LOCNT | Number of failed logon attempts | INT1(3) | XULOCNT |
| /SKN/S_SW_10_08_USER_EMP_CHK | LTIME | Last Logon Time | TIMS(6) | XULTIME |
| /SKN/S_SW_10_08_USER_EMP_CHK | MODBE | Changed By | CHAR(12) | XUMODIFIER |
| /SKN/S_SW_10_08_USER_EMP_CHK | NACHN | Last name | CHAR(40) | PAD_NACHN |
| /SKN/S_SW_10_08_USER_EMP_CHK | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_08_USER_EMP_CHK | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_08_USER_EMP_CHK | NAME_TEXT | Full Name | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_08_USER_EMP_CHK | OBJPS | Object ID | CHAR(2) | OBJPS |
| /SKN/S_SW_10_08_USER_EMP_CHK | PERNR | Personnel number | NUMC(8) | PERSNO |
| /SKN/S_SW_10_08_USER_EMP_CHK | PERSG | Employee group | CHAR(1) | PERSG |
| /SKN/S_SW_10_08_USER_EMP_CHK | PERSK | Employee subgroup | CHAR(2) | PERSK |
| /SKN/S_SW_10_08_USER_EMP_CHK | PLANS | Position | NUMC(8) | PLANS |
| /SKN/S_SW_10_08_USER_EMP_CHK | PWDLGNDATE | Date of Last Password Logon | DATS(8) | XULPDAT |
| /SKN/S_SW_10_08_USER_EMP_CHK | SEQNR | Infotype record no. | NUMC(3) | SEQNR |
| /SKN/S_SW_10_08_USER_EMP_CHK | SPRPS | Lock indicator | CHAR(1) | SPRPS |
| /SKN/S_SW_10_08_USER_EMP_CHK | STAT1 | Cust.-specific stat. | CHAR(1) | STAT1 |
| /SKN/S_SW_10_08_USER_EMP_CHK | STAT2 | Employment status | CHAR(1) | STAT2 |
| /SKN/S_SW_10_08_USER_EMP_CHK | STAT3 | Spec.payment status | CHAR(1) | STAT3 |
| /SKN/S_SW_10_08_USER_EMP_CHK | STATE_COLOR | State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_10_08_USER_EMP_CHK | STATE_DESC | SW Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_10_08_USER_EMP_CHK | STATE_ICON | State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_10_08_USER_EMP_CHK | SUBTY | Subtype | CHAR(4) | SUBTY |
| /SKN/S_SW_10_08_USER_EMP_CHK | TRDAT | Last Logon Date | DATS(8) | XULDATE |
| /SKN/S_SW_10_08_USER_EMP_CHK | TZONE | Time Zone | CHAR(6) | TZNZONE |
| /SKN/S_SW_10_08_USER_EMP_CHK | UFLAG | User Lock Status | INT1(3) | XUUFLAG |
| /SKN/S_SW_10_08_USER_EMP_CHK | UNAME | Changed by | CHAR(12) | AENAM |
| /SKN/S_SW_10_08_USER_EMP_CHK | USER | User | CHAR(12) | XUBNAME |
| /SKN/S_SW_10_08_USER_EMP_CHK | USRID | System ID | CHAR(30) | SYSID |
| /SKN/S_SW_10_08_USER_EMP_CHK | USRTY | Communication type | CHAR(4) | USRTY |
| /SKN/S_SW_10_08_USER_EMP_CHK | USTYP | User Type | CHAR(1) | XUUSTYP |
| /SKN/S_SW_10_08_USER_EMP_CHK | VALID_USERS_ONLY | 'X' - Display only valid users | CHAR(0) | VALID_USERS_ONLY |
| /SKN/S_SW_10_08_USER_EMP_CHK | VDSK1 | Organizational key | CHAR(14) | VDSK1 |
| /SKN/S_SW_10_08_USER_EMP_CHK | VERSN | User master record version | CHAR(3) | XUVERSION |
| /SKN/S_SW_10_08_USER_EMP_CHK | VORNA | First name | CHAR(40) | PAD_VORNA |
| /SKN/S_SW_10_08_USER_EMP_CHK | WERKS | Personnel area | CHAR(4) | PERSA |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_08_USER_EMP_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_08_USER_EMP_CHK OPTIONAL
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
DATA: LV_EMPLOYEE TYPE PERSNO.
DATA : FLD(60) TYPE C .
FIELD-SYMBOLS:  TYPE ANY ,
               <FS_V> TYPE ANY .
DATA_SINGLE: BACKDAYS        INT4,
             FORWDAYS        INT4,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             NO_DATE_RESTRICTION CHAR1,
             VALID_USERS_ONLY    CHAR1, "Display only valid users
             PLVAR          PLVAR,
             OTYPE          OTYPE,
             LANGU          LANGU.
*
DATA_SINGLE: EMP_REF_FLD NAME_FELD.
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
                NO_DATE_RESTRICTION,
                VALID_USERS_ONLY, "Display only valid users
                PLVAR,
                OTYPE,
                LANGU.
 LV_EMP_REF_FLD = 'ACCNT'.
 SELECT_SINGLE: EMP_REF_FLD.
*
*
DATA_MULTY: PERNR       PERSNO,
            BUKRS       BUKRS,
            WERKS       PERSA,
            PERSG       PERSG,
            PERSK       PERSK,
            BTRTL       BTRTL,
            PLANS       PLANS,
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
            DURATION,
            DATUM ,
            STAT1,
            STAT2,
            STAT3.
SELECT_MULTY :
              MONTH,
              DAY.
*
DATA_MULTY:   BNAME            XUBNAME,
              CLASS            XUCLASS,
              USTYP            XUUSTYP,
              ACCNT            XUACCNT,
              UFLAG            XUUFLAG,  " Int 0/32/64/128
              TRDAT            XULDATE,  " Last Logon
              ERDAT            XUERDAT,   "Creation Date of the User Master Record
              ANAME            XUANAME.   "Creator of the User Master Record
SELECT_MULTY: BNAME,
              CLASS,
              USTYP,
              ACCNT,
              UFLAG ,
              TRDAT ,
              ERDAT ,   "Creation Date of the User Master Record
              ANAME.
DATA_MULTY:   EMPLOYEE_OK CHAR1.
SELECT_MULTY: EMPLOYEE_OK.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_08_USER_EMP_CHK'
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
  IF LV_NO_DATE_RESTRICTION IS NOT INITIAL.
    REFRESH R_DATUM.
  ENDIF.
 "--- Get User's Data
  SELECT *
     FROM USR02
     INTO CORRESPONDING FIELDS OF TABLE LT_USR02
       WHERE BNAME IN R_BNAME
         AND CLASS IN R_CLASS
         AND USTYP IN R_USTYP
         AND ACCNT IN R_ACCNT
         AND UFLAG IN R_UFLAG
         AND TRDAT IN R_TRDAT
         AND ERDAT IN R_ERDAT
         AND ANAME IN R_ANAME.
  IF LV_VALID_USERS_ONLY IS NOT INITIAL.
    LOOP AT LT_USR02 INTO LS_USR02.
      SY_TABIX = SY-TABIX .
      IF LS_USR02-GLTGV <= SY-DATUM AND LS_USR02-GLTGB >= SY-DATUM.
      ELSE.
        DELETE LT_USR02 INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT LT_USR02 INTO LS_USR02.
    MOVE-CORRESPONDING LS_USR02 TO T_DATA.
    CONCATENATE 'T_DATA-' LV_EMP_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    IF  IS ASSIGNED.
      T_DATA-EMPLOYEE = .
    ELSE.
      CLEAR T_DATA-EMPLOYEE.
    ENDIF.
    IF T_DATA-EMPLOYEE IS NOT INITIAL.
      APPEND T_DATA.
    ENDIF.
  ENDLOOP.
  CHECK T_DATA[] IS NOT INITIAL.
   SELECT *
     FROM PA0000
     INTO CORRESPONDING FIELDS OF TABLE LT_PA0000
     FOR ALL ENTRIES IN T_DATA
       WHERE  PERNR = T_DATA-EMPLOYEE
***         and STAT1 in R_STAT1
***         and STAT2 in R_STAT2
***         and STAT3 in R_STAT3
         AND BEGDA   <= SY-DATUM
         AND BEGDA   = SY-DATUM.
IF LT_PA0000[] IS NOT INITIAL.
   "--- Get Employes with USER
   SELECT *
     FROM PA0105
     INTO CORRESPONDING FIELDS OF TABLE LT_PA0105
     FOR ALL ENTRIES IN LT_PA0000
     WHERE PERNR = LT_PA0000-PERNR
       AND USRTY = '0001'
       AND BEGDA = SY-DATUM.
  SELECT *
     FROM PA0002
     INTO CORRESPONDING FIELDS OF TABLE LT_PA0002
     FOR ALL ENTRIES IN LT_PA0000
       WHERE PERNR = LT_PA0000-PERNR
         AND BEGDA = SY-DATUM
         AND GBMON IN R_MONTH
         AND GBTAG IN R_DAY.
     SELECT *
       FROM PA0001
       INTO CORRESPONDING FIELDS OF TABLE LT_PA0001
       FOR ALL ENTRIES IN LT_PA0000
       WHERE  PERNR = LT_PA0000-PERNR
          AND BUKRS IN R_BUKRS
          AND WERKS IN R_WERKS
          AND PERSG IN R_PERSG
          AND PERSK IN R_PERSK
          AND BTRTL IN R_BTRTL
          AND PLANS IN R_PLANS
          AND BEGDA   = SY-DATUM.
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   READ TABLE LT_PA0000 INTO LS_PA0000  WITH KEY PERNR = T_DATA-EMPLOYEE.
   IF SY-SUBRC IS INITIAL.
     MOVE-CORRESPONDING LS_PA0000 TO T_DATA.
   ENDIF.
   READ TABLE LT_PA0001 INTO LS_PA0001  WITH KEY PERNR = T_DATA-EMPLOYEE.
   IF SY-SUBRC IS INITIAL.
     MOVE-CORRESPONDING LS_PA0001 TO T_DATA.
   ENDIF.
   READ TABLE LT_PA0002 INTO LS_PA0002  WITH KEY PERNR = T_DATA-EMPLOYEE.
   IF SY-SUBRC IS INITIAL.
     MOVE-CORRESPONDING LS_PA0002 TO T_DATA.
   ENDIF.
   READ TABLE LT_PA0105 INTO LS_PA0105  WITH KEY PERNR = T_DATA-EMPLOYEE.
   IF SY-SUBRC IS INITIAL.
     MOVE-CORRESPONDING LS_PA0105 TO T_DATA.
   ENDIF.
   CLEAR T_DATA-EMPLOYEE_OK.
   IF T_DATA-EMPLOYEE = T_DATA-PERNR.
     T_DATA-EMPLOYEE_OK = 'X'.
   ENDIF.
   MODIFY T_DATA INDEX SY_TABIX .
 ENDLOOP .
 DELETE T_DATA WHERE EMPLOYEE_OK NOT IN R_EMPLOYEE_OK.
 DELETE T_DATA WHERE STAT1 NOT IN R_STAT1.
 DELETE T_DATA WHERE STAT2 NOT IN R_STAT2.
 DELETE T_DATA WHERE STAT3 NOT IN R_STAT3.
*********************
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
   LV_DATE = T_DATA-TRDAT.
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
ENDIF.
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
**--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
