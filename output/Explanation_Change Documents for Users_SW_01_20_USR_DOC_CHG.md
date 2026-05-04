# Exception Indicator: Change Documents for Users - SW_01_20_USR_DOC_CHG

## General Overview

This Exception Indicator (EI) reads SAP user master change documents and presents them in a monitoring-friendly layout with resolved person names for both the affected user and the changer. It supports investigations into account creation and deletion, password and validity changes, profile assignment and removal, lock transitions, and related security-relevant events within a configurable time window.

This EI serves as an essential control for identity governance and SAP security operations by:

- Enabling auditors and security analysts to reconstruct who changed what on user masters, and when, without manual reconstruction from multiple transactions
- Supporting segregation-of-duties reviews when privileged actors modify users, profiles, or lock states outside normal change windows
- Providing visibility into profile assignment and removal patterns that may indicate over-provisioning, emergency access abuse, or incomplete deprovisioning
- Helping operations teams correlate bursts of changes with incidents, transports, or mass uploads through modification date and time slicing
- Giving compliance functions repeatable evidence of monitoring over user lifecycle events tied to standard change-document services

Typical uses include forensic review after suspicious admin activity, periodic checks on sensitive profile changes, onboarding and offboarding control testing, and integration with broader access-governance programs. Results can feed ticketing, escalation, and remediation workflows.

The evaluation delegates user change-document retrieval to the standard user change-document interface used in the monitoring landscape, then enriches rows with readable names and applies post-processing time-span screening consistent with the configured evaluation window.


## Problem Description

Failure to monitor and review SAP user master change documents in a structured way creates multiple risks across security operations, compliance, and operational integrity:

**Security and Forensic Risks**

- Unauthorized or emergency user master edits may go unnoticed until damage is done or logs age out
- Profile assignments and removals performed outside approved change processes can expand privileged access without oversight
- Password, lock, and validity manipulations by powerful technical accounts may lack independent corroboration
- Coordinated bursts of changes across many users are hard to spot without time-bounded, aggregated monitoring
- Investigations after incidents stall when teams cannot quickly list all relevant change lines with actor and subject context

**Compliance and Audit Risks**

- Regulators and internal audit expect demonstrable monitoring over administrative changes to identities and access-related attributes
- Change evidence scattered across tools weakens the defensibility of access and SoD programs
- Inability to filter by change type (for example creation versus deletion versus profile events) forces manual sampling that misses critical patterns
- Retention and archive handling that is not reflected in monitoring can create gaps in historical proof
- Missing linkage between technical user IDs and human-readable names increases review errors in certification and investigation workpapers

**Management Visibility and Decision-Making Risks**

- Security leadership lacks a single comparable picture of change volume and severity by period, actor, or user population
- Application and process owners cannot easily see whether operational changes align with approved windows or emergency procedures
- Delayed discovery of misconfiguration or mass provisioning errors prolongs exposure and rework
- Cross-team handoffs between security, basis, and identity operations suffer when each team uses different extracts and definitions
- Prioritization of remediation suffers when high-risk change patterns are not distinguished from routine maintenance noise

## Suggested Resolution

**Immediate Response**

- Review flagged change lines and validate each against approved change tickets, emergency procedures, or known maintenance windows
- For high-risk patterns, confirm actor identity and business justification before accepting the change as legitimate
- Escalate suspected misuse of administrative rights according to your security incident playbook
- Capture monitoring output and links to underlying change metadata for evidence retention
- Where appropriate, use standard user and authorization maintenance transactions to correct erroneous master data after proper authorization

**System Assessment**

- Slice results by time window and actor to see whether issues cluster around specific administrators or service accounts
- Compare current and prior monitoring cycles to distinguish one-off spikes from emerging trends
- Segment by change category (for example profile versus password versus lock) to see which control themes need policy updates
- Validate integration settings (for example archive inclusion, view binding) still match your retention and audit requirements
- Document systemic issues such as recurring provisioning defects rather than treating each line as an isolated exception

**Corrective Actions**

- Revoke or adjust inappropriate profile assignments and locks; reset credentials where policy demands after investigation
- Reinforce change management and privileged-access procedures with teams responsible for user administration
- Tune monitoring scope and schedules with security stakeholders so future runs reflect current risk appetite and regulatory expectations
- Train reviewers on how to read action codes, field-level old and new values, and post-processing time-span criteria in the output
- Schedule recurring runs and tie results into GRC or ticketing workflows so drift and abuse are caught early


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACCNT | Billing number flag |  | 0 | 0 |  |  |
| 2 | ACTION | Type of the Change Doc. | CHAR | 1 | 0 | /SKN/E_SW_USER_ACT | /SKN/D_SW_USER_ACT |
| 3 | ACTION_DESC | Type of the Change Doc. | CHAR | 30 | 0 | /SKN/E_SW_USER_ACT_DESC |  |
| 4 | AGR_FDATE | Change Start Date | DATS | 8 | 0 | SUID_CHANGE_FROM_DAT | DATS |
| 5 | AGR_TDATE | Change End Date | DATS | 8 | 0 | SUID_CHANGE_TO_DAT | DATS |
| 6 | ALOCK_D | Admin lock deleted flag |  | 0 | 0 |  |  |
| 7 | ALOCK_S | Admin lock set flag |  | 0 | 0 |  |  |
| 8 | ARCHIVE | Archive data |  | 0 | 0 |  |  |
| 9 | ATTRBT | Attribute Name of the Changed Field | CHAR | 20 | 0 | XUATTR_CD | USATTRID |
| 10 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 11 | BNAME | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 12 | COUNTER | Counter for Change Documents | CHAR | 4 | 0 | XUCOUNT_CD |  |
| 13 | DEPARTMENT | Department | CHAR | 40 | 0 | AD_DPRTMNT | TEXT40 |
| 14 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 15 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 16 | FLOCK_D | False Registr. lock deleted |  | 0 | 0 |  |  |
| 17 | FLOCK_S | False Registr. lock set |  | 0 | 0 |  |  |
| 18 | GROUP | User group flag |  | 0 | 0 |  |  |
| 19 | MODBE | Changed By | CHAR | 12 | 0 | XUMODIFIER | BNAME |
| 20 | MODBE_NAME_FIRST | Changer First Name | CHAR | 40 | 0 | /SKN/E_CHANGER_FIRST_NAME | TEXT40 |
| 21 | MODBE_NAME_LAST | Changer Last Name | CHAR | 40 | 0 | /SKN/E_CHANGER_LAST_NAME | TEXT40 |
| 22 | MODDA | Modification date | DATS | 8 | 0 | XUMODDATE | DATUM |
| 23 | MODTI | Modification time | TIMS | 6 | 0 | XUMODTIME | UZEIT |
| 24 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 25 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 26 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 27 | NEW_VAL | New Contents of Changed Field | CHAR | 100 | 0 | XUAV_CDNEW |  |
| 28 | NRPRO | Number of profiles or authorizations | INT2 | 5 | 0 | XUNUMBER | XUNUMBER |
| 29 | OLD_VAL | Old Contents of Changed Field | CHAR | 100 | 0 | XUAV_CDOLD |  |
| 30 | PASS | Password changed flag |  | 0 | 0 |  |  |
| 31 | PROF_ASS | Profile assignment flag |  | 0 | 0 |  |  |
| 32 | PROF_ASS_T | Select Options for Profile Ass |  | 0 | 0 |  |  |
| 33 | PROF_DEL | Profile deletion flag |  | 0 | 0 |  |  |
| 34 | PROF_DEL_T | Select Options for Profile Del |  | 0 | 0 |  |  |
| 35 | SECU | Security Policy |  | 0 | 0 |  |  |
| 36 | SUBSYSTEM | Receiving system | CHAR | 10 | 0 | RFCRCVSYS | LOGSYS |
| 37 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 38 | TVAL | Validity Period flag |  | 0 | 0 |  |  |
| 39 | TYPE | User type changed flag |  | 0 | 0 |  |  |
| 40 | USER_CRT | User Created flag |  | 0 | 0 |  |  |
| 41 | USER_DEL | User Deleted flag |  | 0 | 0 |  |  |
| 42 | VIEW | View of change history |  | 0 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 42 parameters listed in the Parameters Reference Table above.

**ACCNT** (Billing number flag):

Narrows user change lines tied to billing-number–related master updates when your governance scope includes that dimension.

**ACCNT Options:**
- **X** — Apply this slice as an inclusive filter for matching change lines.
- ** ** (space) — Leave unset so this slice does not restrict the extract.

**ACTION** (Type of the Change Doc.):

Selects change-document action codes so reviews focus on creates, deletes, profile changes, or other coded events.

**ACTION_DESC** (Type of the Change Doc.):

Carries the textual action label on each returned line—pairs logically with **ACTION** for reading dashboards while **ACTION** drives range selection.

**AGR_FDATE** (Change Start Date):

Lower bound of the assignment-related change interval when monitoring profile or authorization lifecycle windows.

**AGR_TDATE** (Change End Date):

Upper bound of the same assignment-related interval—use with **AGR_FDATE** to bracket the period under review.

**AGR_FDATE and AGR_TDATE Connection:**

Define both ends of the assignment-related horizon so profile or authorization lifecycle reviews stay within an auditable window.

**ALOCK_D** (Admin lock deleted flag):

Signals inclusion of lines where an administrative lock was removed from the user master.

**ALOCK_D Options:**
- **X** — Require this condition in the change-document selection handed to the reader.
- ** ** (space) — Do not require this condition.

**ALOCK_S** (Admin lock set flag):

Signals inclusion of lines where an administrative lock was set on the user master.

**ALOCK_S Options:**
- **X** — Apply this slice as an inclusive filter for matching change lines.
- ** ** (space) — Leave unset so this slice does not restrict the extract.

**ARCHIVE** (Archive data):

Controls whether archived change-document sources participate in the evaluation for long-retention audits.

**ARCHIVE Options:**
- **X** — Require this condition in the change-document selection handed to the reader.
- ** ** (space) — Do not require this condition.

**ATTRBT** (Attribute Name of the Changed Field):

Restricts to changes against a named user master attribute—useful when hunting specific field tampering.

**BACKDAYS** (Backdays):

When no explicit modification-date range is supplied, defines how far back from the evaluation day the default window begins.

**BNAME** (User):

Limits the extract to named technical users whose change documents you are investigating.

**COUNTER** (Counter for Change Documents):

Relates to the change-document counter when duplicate or sequence checks matter in the monitoring layout.

**DEPARTMENT** (Department):

Supports organizational scoping when department attributes are carried on the change line for reporting.

**DURATION** (Duration In Time Units):

After the run builds rows, keeps only those whose computed span in **DURATION_UNIT** matches your supplied thresholds.

**DURATION and DURATION_UNIT Connection:**

The function derives a numeric span using **DURATION_UNIT**, then your selection on **DURATION** retains only rows whose computed value falls in range.

**DURATION_UNIT** (Duration Unit):

Defines whether elapsed spans are expressed in hours, minutes, days, or full calendar-day units for the duration step.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**FLOCK_D** (False Registr. lock deleted):

Signals inclusion of lines where a false-registration lock was cleared.

**FLOCK_D Options:**
- **X** — Apply this slice as an inclusive filter for matching change lines.
- ** ** (space) — Leave unset so this slice does not restrict the extract.

**FLOCK_S** (False Registr. lock set):

Signals inclusion of lines where a false-registration lock was applied.

**FLOCK_S Options:**
- **X** — Require this condition in the change-document selection handed to the reader.
- ** ** (space) — Do not require this condition.

**GROUP** (User group flag):

Restricts to changes involving user group–related semantics when the flag is asserted in the integration path.

**GROUP Options:**
- **X** — Apply this slice as an inclusive filter for matching change lines.
- ** ** (space) — Leave unset so this slice does not restrict the extract.

**MODBE** (Changed By):

Filters by the user ID that performed the change—central for segregation-of-duties and privileged-actor reviews.

**MODBE_NAME_FIRST** (Changer First Name):

Given name resolved for **MODBE** so reviewers recognize the changer without looking up master data separately.

**MODBE_NAME_LAST** (Changer Last Name):

Family name resolved for **MODBE**—read together with **MODBE_NAME_FIRST** for full changer identity context.

**MODDA** (Modification date):

Explicit calendar range for modification date; when supplied, it overrides the default back-day window for the read.

**MODTI** (Modification time):

Time-of-day slice for modifications—combine with **MODDA** to isolate bursts on a single calendar day.

**MODDA and MODTI Connection:**

Together they bound both calendar day and clock time for modification-driven investigations; omitting **MODTI** leaves end-of-day defaults in typical setups.

**NAME_FIRST** (First Name):

Given name of the user subject of the change document, enriched for readable monitoring output.

**NAME_LAST** (Last Name):

Family name of that user—pairs with **NAME_FIRST** for directory-style review alongside **BNAME**.

**NAME_FIRST and NAME_LAST Connection:**

Jointly identify the affected person on the change line while **BNAME** remains the technical key.

**NAME_TEXT** (Full Name):

Single formatted full-name column for dashboards that show one label per technical user.

**NEW_VAL** (New Contents of Changed Field):

Filters or displays the new field contents after the change—supports field-level forensic review.

**NRPRO** (Number of profiles or authorizations):

Relates to counts of profiles or authorizations referenced on the change line for concentration analysis.

**OLD_VAL** (Old Contents of Changed Field):

Filters or displays the prior field contents before the change—pairs with **NEW_VAL** for before/after storytelling.

**PASS** (Password changed flag):

Restricts to password-related user master change events when credential governance is the focus.

**PASS Options:**
- **X** — Require this condition in the change-document selection handed to the reader.
- ** ** (space) — Do not require this condition.

**PROF_ASS** (Profile assignment flag):

When set, includes the slice of changes where profile assignment activity is relevant to the underlying service call.

**PROF_ASS Options:**
- **X** — Apply this slice as an inclusive filter for matching change lines.
- ** ** (space) — Leave unset so this slice does not restrict the extract.

**PROF_ASS_T** (Select Options for Profile Ass):

Supplies select-options for specific profiles on assignment—pair with **PROF_ASS** when you need named profiles, not only the generic assignment slice.

**PROF_ASS and PROF_ASS_T Connection:**

The flag enables the assignment slice; the select-options name the specific profiles whose assignment changes you need to prove.

**PROF_DEL** (Profile deletion flag):

When set, includes profile-deletion–related change activity in the scope sent to the change-document reader.

**PROF_DEL Options:**
- **X** — Require this condition in the change-document selection handed to the reader.
- ** ** (space) — Do not require this condition.

**PROF_DEL_T** (Select Options for Profile Del):

Select-options for profiles removed—use with **PROF_DEL** to target high-risk profile removals.

**PROF_DEL and PROF_DEL_T Connection:**

The flag enables deletion-related events; the select-options list the profiles whose removals must appear in the result.

**SECU** (Security Policy):

Relates to security-policy–linked user changes when your landscape evaluates policy-driven events.

**SECU Options:**
- **X** — Apply this slice as an inclusive filter for matching change lines.
- ** ** (space) — Leave unset so this slice does not restrict the extract.

**SUBSYSTEM** (Receiving system):

Scopes changes by receiving or logical subsystem when distributed or ALE-style contexts apply.

**TCODE** (Transaction Code):

Restricts to changes recorded under named transactions—helps tie findings to SM50/SM20-style operational reviews.

**TVAL** (Validity Period flag):

Signals inclusion of validity-period adjustments on the user master within the monitored slice.

**TVAL Options:**
- **X** — Require this condition in the change-document selection handed to the reader.
- ** ** (space) — Do not require this condition.

**TYPE** (User type changed flag):

Restricts to user-type change events (for example shifts between dialog and non-dialog categories).

**TYPE Options:**
- **X** — Apply this slice as an inclusive filter for matching change lines.
- ** ** (space) — Leave unset so this slice does not restrict the extract.

**USER_CRT** (User Created flag):

Restricts to user-creation change documents for onboarding and provisioning control testing.

**USER_CRT Options:**
- **X** — Require this condition in the change-document selection handed to the reader.
- ** ** (space) — Do not require this condition.

**USER_DEL** (User Deleted flag):

Restricts to user-deletion change documents for deprovisioning and orphan-account investigations.

**USER_DEL Options:**
- **X** — Apply this slice as an inclusive filter for matching change lines.
- ** ** (space) — Leave unset so this slice does not restrict the extract.

**VIEW** (View of change history):

Chooses the change-history view semantics the monitor passes into the user change-document interface for this run.


### Parameter Relationships

**Time window**

- When no explicit modification-date range is supplied, the evaluation uses a backward window anchored on the monitoring run’s current date, controlled by **BACKDAYS**.
- Supplying **MODDA** (and optionally **MODTI**) overrides that default with an explicit calendar (and time) slice for investigations that must align to a known incident window.

**Profile-related changes**

- **PROF_ASS** and **PROF_ASS_T** work together: the former enables assignment-related semantics in the reader, while the latter names the specific profiles whose assignment changes must appear.
- **PROF_DEL** and **PROF_DEL_T** mirror that pattern for profile removals—use both when deprovisioning and toxic-combination reviews are in scope.

**Event-type flags**

- **USER_CRT**, **USER_DEL**, **PASS**, **TYPE**, **TVAL**, **ALOCK_S**, **ALOCK_D**, **FLOCK_S**, **FLOCK_D**, **ACCNT**, and **GROUP** act as toggles that narrow which classes of user master change lines the downstream change-document reader returns; combine only those slices your policy requires to avoid over-filtering legitimate maintenance.

**Actors and subjects**

- **BNAME** scopes the user whose change documents are retrieved, while **MODBE** scopes the administrator or technical identity that performed the change—pair them in investigations that must prove both victim and actor.

**After retrieval**

- **DURATION** and **DURATION_UNIT** apply after rows are built: the unit defines how elapsed time is measured for the post-processing step, and the selection retains rows whose computed value matches your monitoring thresholds.

**Presentation fields**

- **NAME_FIRST**, **NAME_LAST**, and **NAME_TEXT** describe the subject user; **MODBE_NAME_FIRST** and **MODBE_NAME_LAST** describe the changer—read these together with the technical IDs on the same line.


### Default Values

- **BACKDAYS** — Default: `10` (applied in code before reading caller selections when no explicit modification-date range is supplied, establishing a ten-day backward window from the evaluation date).
- **LANGU** — Default: `EN` (assigned before reading caller selections when no language is supplied).

### Practical Configuration Examples

**Use Case 1: Single user under investigation**

```
BNAME = ADMIN01
MODBE = *
```

**Purpose:** Lists change documents for one technical user while allowing any changer—typical first step when a specific account is suspected.

**Use Case 2: New accounts and password events**

```
USER_CRT = X
PASS = X
BACKDAYS = 14
```

**Purpose:** Surfaces user creations and password-related changes inside a two-week window—useful after a suspected credential incident or mass onboarding.

**Use Case 3: Windowed review with full-day duration filter**

```
BACKDAYS = 30
DURATION = 5
DURATION_UNIT = F
```

**Purpose:** Reviews roughly the last month of default-window activity but keeps only rows whose post-processed span matches a five full-day unit threshold—helpful when tuning dormant-change detection.

**Use Case 4: Profile assignment and removal focus**

```
PROF_ASS = X
PROF_DEL = X
PROF_ASS_T = Z_FIN_POSTING
PROF_DEL_T = Z_FIN_POSTING
```

**Purpose:** Targets assignment and deletion events for a named sensitive profile during a profile redesign or cleanup program.

**Use Case 5: Field-level tampering on an attribute**

```
ATTRBT = CLASS
OLD_VAL = *
NEW_VAL = *
```

**Purpose:** Highlights changes to the user group field with any before/after values—supports investigations into unauthorized group moves.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_20_USER_CHG_DOC | ACTION | SW: Type of the Change Document | CHAR(1) | /SKN/E_SW_USER_ACT |
| /SKN/S_SW_01_20_USER_CHG_DOC | ACTION_DESC | SW: Type of Change Document Desc. | CHAR(30) | /SKN/E_SW_USER_ACT_DESC |
| /SKN/S_SW_01_20_USER_CHG_DOC | AGR_FDATE | Start of the Change Date of the Validity | DATS(8) | SUID_CHANGE_FROM_DAT |
| /SKN/S_SW_01_20_USER_CHG_DOC | AGR_TDATE | End of the Change Date of the Validity | DATS(8) | SUID_CHANGE_TO_DAT |
| /SKN/S_SW_01_20_USER_CHG_DOC | ATTRBT | Attribute Name of the Changed Field | CHAR(20) | XUATTR_CD |
| /SKN/S_SW_01_20_USER_CHG_DOC | BNAME | User Name in User Master Record | CHAR(12) | XUBNAME |
| /SKN/S_SW_01_20_USER_CHG_DOC | COUNTER | Counter for Change Documents | CHAR(4) | XUCOUNT_CD |
| /SKN/S_SW_01_20_USER_CHG_DOC | DEPARTMENT | Department | CHAR(40) | AD_DPRTMNT |
| /SKN/S_SW_01_20_USER_CHG_DOC | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_20_USER_CHG_DOC | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_20_USER_CHG_DOC | MODBE | Last Changed By | CHAR(12) | XUMODIFIER |
| /SKN/S_SW_01_20_USER_CHG_DOC | MODBE_NAME_FIRST | Changer First Name | CHAR(40) | /SKN/E_CHANGER_FIRST_NAME |
| /SKN/S_SW_01_20_USER_CHG_DOC | MODBE_NAME_LAST | Changer Last Name | CHAR(40) | /SKN/E_CHANGER_LAST_NAME |
| /SKN/S_SW_01_20_USER_CHG_DOC | MODDA | Modification date | DATS(8) | XUMODDATE |
| /SKN/S_SW_01_20_USER_CHG_DOC | MODTI | Modification time | TIMS(6) | XUMODTIME |
| /SKN/S_SW_01_20_USER_CHG_DOC | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_01_20_USER_CHG_DOC | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_01_20_USER_CHG_DOC | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_01_20_USER_CHG_DOC | NEW_VAL | New Contents of Changed Field | CHAR(100) | XUAV_CDNEW |
| /SKN/S_SW_01_20_USER_CHG_DOC | NRPRO | Number of profiles or authorizations | INT2(5) | XUNUMBER |
| /SKN/S_SW_01_20_USER_CHG_DOC | OLD_VAL | Old Contents of Changed Field | CHAR(100) | XUAV_CDOLD |
| /SKN/S_SW_01_20_USER_CHG_DOC | SUBSYSTEM | Receiving system for central user administration | CHAR(10) | RFCRCVSYS |
| /SKN/S_SW_01_20_USER_CHG_DOC | TCODE | Transaction Code | CHAR(20) | TCODE |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_01_20_USER_CHG_DOC .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_20_USER_CHG_DOC OPTIONAL
*"----------------------------------------------------------------------
  DATA: LV_PROFS     TYPE XUPROFS,
        LV_FORWDAYS  TYPE I,
        LV_ROWCOUNT  TYPE I,
        LV_REF_DATE  TYPE D,
        LV_TIME_DIFF TYPE INT4,
        LV_FLD       TYPE FIELDNAME,
        LV_TABIX     TYPE I.
  DATA: LS_USER_RANGE TYPE CDUSERNAME_RANGE,
        LS_PROF_RANGE TYPE USSELPROF,
        LS_OUT        TYPE USRCD,
        LS_DATA       TYPE /SKN/S_SW_01_20_USER_CHG_DOC.
  DATA: LT_USER_RANGE     TYPE CDUSERNAME_RANGE_TAB,
        LT_CHG_USER_RANGE TYPE CDUSERNAME_RANGE_TAB,
        LT_PROF_ASS_RANGE TYPE SUSR_T_SEL_OPT_PROF,
        LT_PROF_DEL_RANGE TYPE SUSR_T_SEL_OPT_PROF,
        LT_OUT            TYPE SUSR_T_USRCD.
  FIELD-SYMBOLS: <FS_FLD>  TYPE ANY,
                 <FS_DATA> LIKE LINE OF T_DATA[].
  DATA_SINGLE: DATLO         SY-DATLO.
  DATA_SINGLE: TIMLO         SY-TIMLO.
  DATA_SINGLE: DATE_FROM     CDDATUM.
  DATA_SINGLE: DATE_TO       CDDATUM.
  DATA_SINGLE: TIME_FROM     CDUZEIT.
  DATA_SINGLE: TIME_TO       CDUZEIT.
  DATA_SINGLE: USER_CRT      C.
  DATA_SINGLE: USER_DEL      C.
  DATA_SINGLE: PASS          C.
  DATA_SINGLE: TYPE          C.
  DATA_SINGLE: ALOCK_S       C.
  DATA_SINGLE: ALOCK_D       C.
  DATA_SINGLE: FLOCK_S       C.
  DATA_SINGLE: FLOCK_D       C.
  DATA_SINGLE: TVAL          C.
  DATA_SINGLE: ACCNT         C.
  DATA_SINGLE: GROUP         C.
  DATA_SINGLE: PROF_ASS      C.
  DATA_SINGLE: PROF_DEL      C.
  DATA_SINGLE: VIEW          US_CD_VIEW.
  DATA_SINGLE: ARCHIVE       C.
  DATA_SINGLE: SECU          C.
  DATA_SINGLE: DURATION_UNIT /SKN/E_SW_DURATION_UNIT.
  DATA_SINGLE: BACKDAYS /SKN/E_MN_AN_BACKDAYS.
  LV_BACKDAYS = '10'.
*  data_single: date_ref_fld name_feld.
  DATA_SINGLE: LANGU LANGU.
  LV_LANGU = 'EN'.
  DATA_SINGLE: SW_DEST RFCDEST.
  DATA_MULTY: DATUM      DATUM.
  DATA_MULTY: BNAME      XUBNAME.     " User
  DATA_MULTY: MODBE      XUMODIFIER.  " Changed by(User)
  DATA_MULTY: MODDA      XUMODDATE.   " Modification Date
  DATA_MULTY: MODTI      XUMODTIME.   " Modification Time
  DATA_MULTY: ACTION     XUACT_CD.    " Type of the Change Doc.
  DATA_MULTY: PROF_ASS_T XUPROFILE.   " Profile assignment
  DATA_MULTY: PROF_DEL_T XUPROFILE.   " Profile delete
  DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
  SELECT_SINGLE: SW_DEST.
  SELECT_SINGLE: BACKDAYS.
  SELECT_SINGLE: USER_CRT.
  SELECT_SINGLE: USER_DEL.
  SELECT_SINGLE: PASS.
  SELECT_SINGLE: TYPE.
  SELECT_SINGLE: ALOCK_S.
  SELECT_SINGLE: ALOCK_D.
  SELECT_SINGLE: FLOCK_S.
  SELECT_SINGLE: FLOCK_D.
  SELECT_SINGLE: TVAL.
  SELECT_SINGLE: ACCNT.
  SELECT_SINGLE: GROUP.
  SELECT_SINGLE: PROF_ASS.
  SELECT_SINGLE: PROF_DEL.
  SELECT_SINGLE: VIEW.
  SELECT_SINGLE: ARCHIVE.
  SELECT_SINGLE: SECU.
  SELECT_SINGLE: LANGU.
  SELECT_SINGLE: DURATION_UNIT.
  SELECT_MULTY: BNAME.
  SELECT_MULTY: MODBE.
  SELECT_MULTY: MODDA.
  SELECT_MULTY: MODTI.
  SELECT_MULTY: ACTION.
  SELECT_MULTY: PROF_ASS_T.
  SELECT_MULTY: PROF_DEL_T.
  CONVERT_SINGLE: LANGU ISOLA.
  _GET_CURRENT_DATE_TIME ' ' LV_SW_DEST LV_DATLO LV_TIMLO.
  IF R_MODDA[] IS INITIAL.
    IF LV_FORWDAYS IS INITIAL.
      LV_DATE_FROM = LV_DATLO - LV_BACKDAYS.
      LV_DATE_TO   = LV_DATLO.
    ELSE.
      IF LV_BACKDAYS IS NOT INITIAL.
        LV_DATE_FROM = LV_DATLO - LV_BACKDAYS.
        LV_DATE_TO   = LV_DATLO + LV_FORWDAYS.
      ELSE.
        LV_DATE_FROM = LV_DATLO + LV_FORWDAYS.
      ENDIF.
    ENDIF.
  ELSE.
    READ TABLE R_MODDA INTO RS_MODDA INDEX 1.
    IF SY-SUBRC IS INITIAL.
      LV_DATE_FROM = RS_MODDA-LOW.
      IF RS_MODDA-HIGH IS NOT INITIAL.
        LV_DATE_TO   = RS_MODDA-HIGH.
      ELSE.
        LV_DATE_TO = LV_DATE_FROM.
      ENDIF.
    ENDIF.
  ENDIF.
  IF R_MODTI[] IS NOT INITIAL.
    READ TABLE R_MODTI INTO RS_MODTI INDEX 1.
    IF SY-SUBRC IS INITIAL.
      LV_TIME_FROM = RS_MODTI-LOW.
      IF RS_MODTI-HIGH IS NOT INITIAL.
        LV_TIME_TO = RS_MODTI-HIGH.
      ELSE.
        LV_TIME_TO = '235959'.
      ENDIF.
    ENDIF.
  ENDIF.
* Set "User" Range Values
  LOOP AT R_BNAME INTO RS_BNAME.
    IF RS_BNAME-SIGN IS NOT INITIAL.
      LS_USER_RANGE-SIGN   = RS_BNAME-SIGN.
    ELSE.
      LS_USER_RANGE-SIGN   = 'I'.
    ENDIF.
    IF RS_BNAME-OPTION IS NOT INITIAL.
      LS_USER_RANGE-OPTION = RS_BNAME-OPTION.
    ELSE.
      LS_USER_RANGE-OPTION = 'EQ'.
    ENDIF.
    LS_USER_RANGE-LOW = RS_BNAME-LOW.
    IF LS_USER_RANGE-OPTION EQ 'BT'.
      LS_USER_RANGE-HIGH = RS_BNAME-HIGH.
    ENDIF.
    APPEND LS_USER_RANGE TO LT_USER_RANGE.
  ENDLOOP.
* Set "Changed by" Range Values
  LOOP AT R_MODBE INTO RS_MODBE.
    IF RS_MODBE-SIGN IS NOT INITIAL.
      LS_USER_RANGE-SIGN = RS_MODBE-SIGN.
    ELSE.
      LS_USER_RANGE-SIGN = 'I'.
    ENDIF.
    IF RS_MODBE-OPTION IS NOT INITIAL.
      LS_USER_RANGE-OPTION = RS_MODBE-OPTION.
    ELSE.
      LS_USER_RANGE-OPTION = 'EQ'.
    ENDIF.
    LS_USER_RANGE-LOW = RS_MODBE-LOW.
    IF LS_USER_RANGE-OPTION EQ 'BT'.
      LS_USER_RANGE-HIGH = RS_MODBE-HIGH.
    ENDIF.
    APPEND LS_USER_RANGE TO LT_CHG_USER_RANGE.
  ENDLOOP.
* Set "Profile Assignment" Range Values
  LOOP AT R_PROF_ASS_T INTO RS_PROF_ASS_T.
    IF RS_PROF_ASS_T-SIGN IS NOT INITIAL.
      LS_PROF_RANGE-SIGN = RS_PROF_ASS_T-SIGN.
    ELSE.
      LS_PROF_RANGE-SIGN = 'I'.
    ENDIF.
    IF RS_PROF_ASS_T-OPTION IS NOT INITIAL.
      LS_PROF_RANGE-OPTION = RS_PROF_ASS_T-OPTION.
    ELSE.
      LS_PROF_RANGE-OPTION = 'EQ'.
    ENDIF.
    LS_PROF_RANGE-LOW = RS_PROF_ASS_T-LOW.
    IF LS_PROF_RANGE-OPTION EQ 'BT'.
      LS_PROF_RANGE-HIGH = RS_PROF_ASS_T-HIGH.
    ENDIF.
    APPEND LS_PROF_RANGE TO LT_PROF_ASS_RANGE.
  ENDLOOP.
* Set "Profile Delete" Range Values
  LOOP AT R_PROF_DEL_T INTO RS_PROF_DEL_T.
    IF RS_PROF_DEL_T-SIGN IS NOT INITIAL.
      LS_PROF_RANGE-SIGN = RS_PROF_DEL_T-SIGN.
    ELSE.
      LS_PROF_RANGE-SIGN = 'I'.
    ENDIF.
    IF RS_PROF_DEL_T-OPTION IS NOT INITIAL.
      LS_PROF_RANGE-OPTION = RS_PROF_DEL_T-OPTION.
    ELSE.
      LS_PROF_RANGE-OPTION = 'EQ'.
    ENDIF.
    LS_PROF_RANGE-LOW = RS_PROF_DEL_T-LOW.
    IF LS_PROF_RANGE-OPTION EQ 'BT'.
      LS_PROF_RANGE-HIGH = RS_PROF_DEL_T-HIGH.
    ENDIF.
    APPEND LS_PROF_RANGE TO LT_PROF_DEL_RANGE.
  ENDLOOP.
* Get User Change Doc.
  CALL FUNCTION '/SKN/FC_USER_CHANGE_DOC'
    EXPORTING
      SW_DEST         = LV_SW_DEST
      IT_USERNAME     = LT_USER_RANGE[]
      IT_CDUSERNAME   = LT_CHG_USER_RANGE[]
      IV_FDATE        = LV_DATE_FROM
      IV_TDATE        = LV_DATE_TO
      IV_FTIME        = LV_TIME_FROM
      IV_TTIME        = LV_TIME_TO
      IV_USER_CRT     = LV_USER_CRT
      IV_USER_DEL     = LV_USER_DEL
      IV_PASS         = LV_PASS
      IV_TYPE         = LV_TYPE
      IV_ALOCK_S      = LV_ALOCK_S
      IV_ALOCK_D      = LV_ALOCK_D
      IV_FLOCK_S      = LV_FLOCK_S
      IV_FLOCK_D      = LV_FLOCK_D
      IV_TVAL         = LV_TVAL
      IV_ACCNT        = LV_ACCNT
      IV_GROUP        = LV_GROUP
      IV_PROF_ASS     = LV_PROF_ASS
      IT_PROF_ASS     = LT_PROF_ASS_RANGE[]
      IV_PROF_DEL     = LV_PROF_DEL
      IT_PROF_DEL     = LT_PROF_DEL_RANGE[]
      IV_VIEW         = 'C'
      IV_ARCHIVE      = LV_ARCHIVE
      IV_SECU         = LV_SECU
    IMPORTING
      ET_CDRED_OUTPUT = LT_OUT[].
  LOOP AT LT_OUT INTO LS_OUT.
    MOVE-CORRESPONDING LS_OUT TO LS_DATA.
    LS_DATA-ACTION_DESC    =  LS_OUT-ACTION.
    IF LS_DATA-BNAME IS NOT INITIAL AND
       ( LS_DATA-NAME_FIRST IS INITIAL OR LS_DATA-NAME_LAST IS INITIAL ).
      CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
        EXPORTING
          BNAME      = LS_DATA-BNAME
          SW_DEST    = LV_SW_DEST
        IMPORTING
          NAME_FIRST = LS_DATA-NAME_FIRST
          NAME_LAST  = LS_DATA-NAME_LAST
          NAME_TEXT  = LS_DATA-NAME_TEXT
        EXCEPTIONS
          NO_DATA    = 1
          OTHERS     = 2.
      IF SY-SUBRC IS NOT INITIAL.
        CLEAR: LS_DATA-NAME_FIRST, LS_DATA-NAME_LAST, LS_DATA-NAME_TEXT.
      ENDIF.
    ENDIF.
    IF LS_DATA-MODBE IS NOT INITIAL.
      CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
        EXPORTING
          BNAME      = LS_DATA-MODBE
          SW_DEST    = LV_SW_DEST
        IMPORTING
          NAME_FIRST = LS_DATA-MODBE_NAME_FIRST
          NAME_LAST  = LS_DATA-MODBE_NAME_LAST
*          name_text  = ls_data-name_text
        EXCEPTIONS
          NO_DATA    = 1
          OTHERS     = 2.
      IF SY-SUBRC IS NOT INITIAL.
        CLEAR: LS_DATA-NAME_FIRST, LS_DATA-NAME_LAST, LS_DATA-NAME_TEXT.
      ENDIF.
    ENDIF.
    APPEND LS_DATA TO T_DATA.
    CLEAR LS_DATA.
  ENDLOOP.
* Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    LV_TABIX = SY-TABIX .
*    CONCATENATE 'T_DATA-' lv_date_ref_fld INTO lv_fld .
*    ASSIGN (lv_fld) TO <fs_fld>.
*    lv_ref_date = <fs_fld> .
*
*    IF NOT lv_ref_date IS INITIAL.
    T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = LV_DATE_FROM
        T_FROM      = LV_TIME_FROM
        D_TO        = LV_DATE_TO
        T_TO        = LV_TIME_TO
        TIME_UNIT   = LV_DURATION_UNIT   "'D'
      IMPORTING
        TIME_DIFF   = LV_TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC IS INITIAL.
      IF LV_TIME_DIFF < '999999'.
        T_DATA-DURATION = LV_TIME_DIFF .
      ELSE.
        T_DATA-DURATION = '999999'.
      ENDIF.
    ENDIF.
    MODIFY T_DATA INDEX LV_TABIX.
*    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
* Finishing (Set IS_ALERT parameter)
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
