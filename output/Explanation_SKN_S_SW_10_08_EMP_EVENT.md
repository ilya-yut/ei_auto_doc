# Exception Indicator: HR Employee events ( SW_10_08_EMP_EVENT)

## General Overview

This Exception Indicator monitors personnel action events from HR master data, optionally enriches each current event with prior events for the same employee, and applies organizational and duration filters so HR teams can review recent hiring, transfer, or status changes with full context.

This EI serves as an essential control for human resources operations and workforce administration by:
- Surfacing employees whose personnel actions fall within the configured monitoring window and action-type scope
- Enabling comparison of a current event with earlier events of the same or any prior action type for trend and repeat-change detection
- Supporting HR business partners with descriptive texts for action types, reasons, organization units, and assignments in the selected language
- Helping compliance and audit teams demonstrate that critical workforce changes were reviewed with assignment and identity context attached
- Complementing ad hoc PA30-style lookups with repeatable, parameterized exception lists for operational follow-up

Typical use includes post-run reviews after payroll or organizational updates, monitoring repeated hire or transfer patterns, and scheduled checks on action types in selected company or personnel areas. Results are intended for exception workflows rather than full HR event history exports.

The routine reads personnel action records for the date window, optionally attaches prior events per employee according to previous-event rules, joins organizational and personal data valid on each event date, and raises an alert when qualifying rows remain after duration filtering.


## Problem Description

Failure to monitor personnel action events and their relationship to prior actions creates multiple risks across workforce administration, compliance, and management oversight.

**HR Operations and Workforce Risks**
- Recent hires, transfers, or status changes may go unnoticed until payroll or access issues appear
- Repeated action sequences (for example back-to-back transfers) are hard to spot without automated prior-event comparison
- HR teams lack a consolidated view tying action type, reason, organization unit, and employee identity for the same review cycle

**Compliance and Audit Risks**
- Evidence of supervisory review after organizational changes is weaker when event populations are pulled manually
- Action types outside agreed scope may persist in the landscape without a recurring exception check

**Management Visibility Risks**
- Leaders cannot prioritize follow-up when descriptive texts and user identifiers are not assembled in one exception list
- Cross-company or cross-personnel-area concentrations of the same action type remain hidden until month-end reporting

## Suggested Resolution

**Immediate Response**
- Review each flagged event together with employee name, action type, reason, organizational assignment, and any prior event rows shown in the exception
- Confirm business justification with HR operations or the line manager before reversing or re-posting actions
- Exclude rows that reflect approved mass-update projects once the project ID or change ticket is validated

**System Assessment**
- Compare this cycle to prior runs after reorganizations, acquisitions, or interface loads that increase action volume
- Look for concentrations by action type, company, or personnel area to see whether one integration or batch job drives most items
- Revisit previous-event settings when duplicate or unrelated prior rows appear in the result

**Corrective Actions**
- Correct master data or actions through standard personnel administration with required approvals
- Adjust monitoring parameters after root cause so the queue remains actionable for HR operations
- Update written procedures when specific action types require mandatory prior-event review
- Route repeat data-quality or interface issues into HR master-data governance when event feeds require fixes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ANY_PREV_EVENT | 'X'- Show Any Prev. Event |  | 0 | 0 |  |  |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | BEGDA | Start Date | DATS | 8 | 0 | BEGDA | DATUM |
| 4 | BTRTL | Personnel subarea | CHAR | 4 | 0 | BTRTL | BTRTL |
| 5 | BTRTX | Pers. subarea text | CHAR | 15 | 0 | BTRTX | TEXT15 |
| 6 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 7 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 8 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 9 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 10 | ENDDA | End Date | DATS | 8 | 0 | ENDDA | DATUM |
| 11 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 12 | LANGU | Language for Texts |  | 0 | 0 |  |  |
| 13 | MASSG | Reason for Action | CHAR | 2 | 0 | MASSG | MASSG |
| 14 | MASSN | Action Type | CHAR | 2 | 0 | MASSN | MASSN |
| 15 | MGTXT | Reason f.action text | CHAR | 30 | 0 | MGTXT | TEXT30 |
| 16 | MNTXT | Name of action type | CHAR | 30 | 0 | MNTXT | TEXT30 |
| 17 | NACHN | Last name | CHAR | 40 | 0 | PAD_NACHN | PAD_NACHN |
| 18 | OBJPS | Object ID | CHAR | 2 | 0 | OBJPS | OBJPS |
| 19 | ORGEH | Organizational unit | NUMC | 8 | 0 | ORGEH | ORGEH |
| 20 | ORGTX | Org.Unit Short Text | CHAR | 25 | 0 | ORGTX | TEXT25 |
| 21 | PBTXT | Personnel Area Text | CHAR | 30 | 0 | PBTXT | TEXT30 |
| 22 | PERNR | Personnel number | NUMC | 8 | 0 | PERSNO | PERSNO |
| 23 | PERSG | Employee group | CHAR | 1 | 0 | PERSG | PERSG |
| 24 | PERSK | Employee subgroup | CHAR | 2 | 0 | PERSK | PERSK |
| 25 | PGTXT | Name of employee grp | CHAR | 20 | 0 | PGTXT | TEXT20 |
| 26 | PKTXT | Name of EE subgroup | CHAR | 20 | 0 | PKTXT | TEXT20 |
| 27 | PLANS | Position | NUMC | 8 | 0 | PLANS | PLANS |
| 28 | PLSTX | Position Short Text | CHAR | 25 | 0 | PLSTX | TEXT25 |
| 29 | PREV_EVENT_NUM | Number of prev. events to show |  | 0 | 0 |  |  |
| 30 | SAME_PREV_EVENT | 'X'- Show the Same Prev. Event |  | 0 | 0 |  |  |
| 31 | SEQNR | Infotype record no. | NUMC | 3 | 0 | SEQNR | NUM03 |
| 32 | SPRPS | Lock indicator | CHAR | 1 | 0 | SPRPS | SPRPS |
| 33 | STAT1 | Cust.-specific stat. | CHAR | 1 | 0 | STAT1 | STATA |
| 34 | STAT2 | Employment status | CHAR | 1 | 0 | STAT2 | STATA |
| 35 | STAT3 | Spec.payment status | CHAR | 1 | 0 | STAT3 | STATA |
| 36 | STELL | Job key | NUMC | 8 | 0 | STELL | STELL |
| 37 | STLTX | Job Title | CHAR | 25 | 0 | STLTX | TEXT25 |
| 38 | SUBTY | Subtype | CHAR | 4 | 0 | SUBTY | SUBTY |
| 39 | UNAME | Created By | CHAR | 12 | 0 | CRNAM | USNAM |
| 40 | USRID | System ID | CHAR | 30 | 0 | SYSID | CHAR30 |
| 41 | VDSK1 | Organizational key | CHAR | 14 | 0 | VDSK1 | VDSK1 |
| 42 | VORNA | First name | CHAR | 40 | 0 | PAD_VORNA | PAD_VORNM |
| 43 | WERKS | Personnel area | CHAR | 4 | 0 | PERSA | PERSA |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 43 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ANY_PREV_EVENT** ('X'- Show Any Prev. Event)

Pairs with duration logic: once ANY_PREV_EVENT passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on BEGDA

**BEGDA** (Start Date)

Gives auditors traceable criteria because start date on BEGDA is applied consistently before any alert flag is raised.

**BTRTL** (Personnel subarea)

Technical code for Personnel Subarea, a small piece of a company used to set specific work hours, pay rules, and holidays for a group of workers.

**BTRTX** (Pers. subarea text)

Documents expected operator behavior—pers. subarea text on BTRTX should be set when that dimension is part of the control objective.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**BUTXT** (Company Name)

Company code name/description used to present legal entity context in output.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit(D/H/M))

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**ENDDA** (End Date)

Combines with related filters so end date on ENDDA refines which records remain for duration or state checks.

**GSBER** (Business Area)

Business area key used for FI organizational reporting segmentation.

**LANGU** (Language for Texts)

Language key used for language-dependent texts and user-language filtering.

**MASSG** (Reason for Action)

Supports escalation where reason for action on MASSG signals ownership for follow-up between Basis and functional teams.

**MASSN** (Action Type)

Combines with related filters so action type on MASSN refines which records remain for duration or state checks.

**MGTXT** (Reason f.action text)

Gives auditors traceable criteria because reason f.action text on MGTXT is applied consistently before any alert flag is raised.

**MNTXT** (Name of action type)

For operations, name of action type on MNTXT indicates whether a row belongs in the current monitoring pass versus historical noise.

**NACHN** (Last name)

When tightened, last name (NACHN) removes rows that would otherwise dilute attention from failing or stuck cases.

**OBJPS** (Object ID)

Guards against oversized extracts when object id on OBJPS is narrowed together with client, user, or session filters.

**ORGEH** (Organizational unit)

For operations, organizational unit on ORGEH indicates whether a row belongs in the current monitoring pass versus historical noise.

**ORGTX** (Org.Unit Short Text)

Gives auditors traceable criteria because org.unit short text on ORGTX is applied consistently before any alert flag is raised.

**PBTXT** (Personnel Area Text)

Mirrors how administrators slice operational lists: personnel area text (PBTXT) is one lever that shapes which rows are comparable run over run.

**PERNR** (Personnel number)

Personnel Number, a unique eight-digit number assigned to every employee to track all their HR data and history.

**PERSG** (Employee group)

Employee Group is a main category used to classify workers into broad groups like active employees, retirees, or external staff.

**PERSK** (Employee subgroup)

Employee Subgroup is a division of the Employee Group used to set specific payroll rules, work schedules, and hourly or salaried pay status.

**PGTXT** (Name of employee grp)

Interprets name of employee grp as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on PGTXT.

**PKTXT** (Name of EE subgroup)

Supports escalation where name of ee subgroup on PKTXT signals ownership for follow-up between Basis and functional teams.

**PLANS** (Position)

PLANS is the technical field name for Position, a specific seven-digit code that represents a job slot filled by an employee within the company structure.

**PLSTX** (Position Short Text)

Gives auditors traceable criteria because position short text on PLSTX is applied consistently before any alert flag is raised.

**PREV_EVENT_NUM** (Number of prev. events to show)

Valuable when comparing health before and after a release—hold number of prev. events to show on PREV_EVENT_NUM constant while varying other filters.

**SAME_PREV_EVENT** ('X'- Show the Same Prev. Event)

Documents expected operator behavior—'x'- show the same prev. event on SAME_PREV_EVENT should be set when that dimension is part of the control objective.

**SEQNR** (Infotype record no.)

Aligns exception volume with the chosen scope by testing infotype record no. via SEQNR before alert evaluation.

**SPRPS** (Lock indicator)

Lock Indicator is used to freeze a data record so it cannot be used in payroll or reporting until it is reviewed and approved.

**STAT1 - STAT3** (Cust.-specific stat.)

Works downstream of the initial read so cust.-specific stat. on STAT1 still participates in row-level deletion rules.

**STELL** (Job key)

For distributed landscapes, job key on STELL often anchors which application server or destination appears in results.

**STLTX** (Job Title)

Helps monitoring stay readable by requiring job title (STLTX) to match organizational or technical selectors when set.

**SUBTY** (Subtype)

When tightened, subtype (SUBTY) removes rows that would otherwise dilute attention from failing or stuck cases.

**UNAME** (Created By)

SAP user name on business records

**USRID** (System ID)

Documents expected operator behavior—system id on USRID should be set when that dimension is part of the control objective.

**VDSK1** (Organizational key)

Organizational Key is a customizable 14-character code used to combine employee assignment details like company code, personnel area, or cost center into a single field for managing user security and authorizations. [1, 2]

**VORNA** (First name)

Stabilizes week-over-week metrics by fixing first name (VORNA) while allowing duration thresholds to move.

**WERKS** (Personnel area)

Plant key used to scope logistics/procurement records by site.


### Parameter Relationships

How parameter combinations work together

**Monitoring window:** **BACKDAYS** defines how many days backward from the evaluation date are included when building the action start-date range used to read personnel events.

**Action and population scope:** **MASSN** together with **PERNR**, **BUKRS**, **WERKS**, **PERSG**, **PERSK**, **BTRTL**, **PLANS**, **ORGEH**, **STELL**, and employment status fields (**STAT1**, **STAT2**, **STAT3**) narrow which current events enter the initial selection.

**Prior-event enrichment:** **SAME_PREV_EVENT**, **ANY_PREV_EVENT**, and **PREV_EVENT_NUM** control whether earlier personnel actions for the same employee are attached, whether only the same action type or any prior type is considered, and how many prior records are kept per person.

**Age filter after assembly:** **DURATION** with **DURATION_UNIT** is an additional filter applied after rows are built: each line must still fit the configured elapsed-time band measured from the event start date to the evaluation moment.

**Language for descriptions:** **LANGU** aligns action-type, reason, organization, and assignment texts loaded into the result.

**Final selection:** The backward day window, action and organizational filters, prior-event rules, and duration band apply together—rows must satisfy the active combination of date scope, event logic, and duration conditions before they appear in the final alert population.


### Default Values

- **BACKDAYS** - 1
- **DURATION_UNIT** - D
- **LANGU** - E
- **PREV_EVENT_NUM** - 1
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Yesterday's hires in one personnel area**

**Purpose:** Flag new hire actions recorded since the prior day for a single personnel area with day-based duration filtering.
```
BACKDAYS = 1
MASSN = 01
WERKS = 1000
DURATION = 0 - 2
DURATION_UNIT = D
```

**Use Case 2: Same prior action type for repeat transfers**

**Purpose:** Review transfer actions and include the immediately preceding transfer for the same employee to detect back-to-back moves.
```
BACKDAYS = 7
MASSN = 02
SAME_PREV_EVENT = X
PREV_EVENT_NUM = 1
BUKRS = 1000
```

**Use Case 3: Any prior event with full-day duration band**

**Purpose:** Monitor a broad action population while requiring at least three full days since the event start date.
```
BACKDAYS = 14
ANY_PREV_EVENT = X
PREV_EVENT_NUM = 2
DURATION = 3
DURATION_UNIT = F
STAT2 = 3
```

**Use Case 4: Organizational unit and job focus**

**Purpose:** Limit exceptions to selected org units and jobs with English descriptions for HR review.
```
BACKDAYS = 3
ORGEH = 10000001 - 10009999
STELL = 50000001
LANGU = E
PERSG = 1
PERSK = 01
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_08_EMP_EVENT | ANY_PREV_EVENT | 'X'- Show Any Prev. Event | CHAR(0) | ANY_PREV_EVENT |
| /SKN/S_SW_10_08_EMP_EVENT | BACKDAYS | Backdays | CHAR(0) | BACKDAYS |
| /SKN/S_SW_10_08_EMP_EVENT | BEGDA | Start Date | DATS(8) | BEGDA |
| /SKN/S_SW_10_08_EMP_EVENT | BTRTL | Personnel subarea | CHAR(4) | BTRTL |
| /SKN/S_SW_10_08_EMP_EVENT | BTRTX | Pers. subarea text | CHAR(15) | BTRTX |
| /SKN/S_SW_10_08_EMP_EVENT | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_08_EMP_EVENT | BUTXT | Company Name | CHAR(25) | BUTXT |
| /SKN/S_SW_10_08_EMP_EVENT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_08_EMP_EVENT | DURATION_UNIT | Duration Unit(D/H/M) | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_08_EMP_EVENT | ENDDA | End Date | DATS(8) | ENDDA |
| /SKN/S_SW_10_08_EMP_EVENT | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_08_EMP_EVENT | LANGU | Language for Texts | CHAR(0) | LANGU |
| /SKN/S_SW_10_08_EMP_EVENT | MASSG | Reason for Action | CHAR(2) | MASSG |
| /SKN/S_SW_10_08_EMP_EVENT | MASSN | Action Type | CHAR(2) | MASSN |
| /SKN/S_SW_10_08_EMP_EVENT | MGTXT | Reason f.action text | CHAR(30) | MGTXT |
| /SKN/S_SW_10_08_EMP_EVENT | MNTXT | Name of action type | CHAR(30) | MNTXT |
| /SKN/S_SW_10_08_EMP_EVENT | NACHN | Last name | CHAR(40) | PAD_NACHN |
| /SKN/S_SW_10_08_EMP_EVENT | OBJPS | Object ID | CHAR(2) | OBJPS |
| /SKN/S_SW_10_08_EMP_EVENT | ORGEH | Organizational unit | NUMC(8) | ORGEH |
| /SKN/S_SW_10_08_EMP_EVENT | ORGTX | Org.Unit Short Text | CHAR(25) | ORGTX |
| /SKN/S_SW_10_08_EMP_EVENT | PBTXT | Personnel Area Text | CHAR(30) | PBTXT |
| /SKN/S_SW_10_08_EMP_EVENT | PERNR | Personnel number | NUMC(8) | PERSNO |
| /SKN/S_SW_10_08_EMP_EVENT | PERSG | Employee group | CHAR(1) | PERSG |
| /SKN/S_SW_10_08_EMP_EVENT | PERSK | Employee subgroup | CHAR(2) | PERSK |
| /SKN/S_SW_10_08_EMP_EVENT | PGTXT | Name of employee grp | CHAR(20) | PGTXT |
| /SKN/S_SW_10_08_EMP_EVENT | PKTXT | Name of EE subgroup | CHAR(20) | PKTXT |
| /SKN/S_SW_10_08_EMP_EVENT | PLANS | Position | NUMC(8) | PLANS |
| /SKN/S_SW_10_08_EMP_EVENT | PLSTX | Position Short Text | CHAR(25) | PLSTX |
| /SKN/S_SW_10_08_EMP_EVENT | PREV_EVENT_NUM | Number of prev. events to show | CHAR(0) | PREV_EVENT_NUM |
| /SKN/S_SW_10_08_EMP_EVENT | SAME_PREV_EVENT | 'X'- Show the Same Prev. Event | CHAR(0) | SAME_PREV_EVENT |
| /SKN/S_SW_10_08_EMP_EVENT | SEQNR | Infotype record no. | NUMC(3) | SEQNR |
| /SKN/S_SW_10_08_EMP_EVENT | SPRPS | Lock indicator | CHAR(1) | SPRPS |
| /SKN/S_SW_10_08_EMP_EVENT | STAT1 | Cust.-specific stat. | CHAR(1) | STAT1 |
| /SKN/S_SW_10_08_EMP_EVENT | STAT2 | Employment status | CHAR(1) | STAT2 |
| /SKN/S_SW_10_08_EMP_EVENT | STAT3 | Spec.payment status | CHAR(1) | STAT3 |
| /SKN/S_SW_10_08_EMP_EVENT | STELL | Job key | NUMC(8) | STELL |
| /SKN/S_SW_10_08_EMP_EVENT | STLTX | Job Title | CHAR(25) | STLTX |
| /SKN/S_SW_10_08_EMP_EVENT | SUBTY | Subtype | CHAR(4) | SUBTY |
| /SKN/S_SW_10_08_EMP_EVENT | UNAME | Created By | CHAR(12) | CRNAM |
| /SKN/S_SW_10_08_EMP_EVENT | USRID | System ID | CHAR(30) | SYSID |
| /SKN/S_SW_10_08_EMP_EVENT | VDSK1 | Organizational key | CHAR(14) | VDSK1 |
| /SKN/S_SW_10_08_EMP_EVENT | VORNA | First name | CHAR(40) | PAD_VORNA |
| /SKN/S_SW_10_08_EMP_EVENT | WERKS | Personnel area | CHAR(4) | PERSA |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_08_EMP_EVENT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_08_EMP_EVENT OPTIONAL
*"----------------------------------------------------------------------
DATA : DATE_FROM LIKE SY-DATUM,
       DATE_TO LIKE SY-DATUM .
DATA : TIME_DIFF TYPE  INT4 .
DATA : SPRAS_T TYPE SPRAS .
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LS_DATA LIKE LINE OF T_DATA.
DATA: LS_PA0000 TYPE PA0000,
      LT_PA0000 LIKE TABLE OF LS_PA0000 WITH HEADER LINE,
      LT_PA0000_PREV LIKE TABLE OF LS_PA0000 WITH HEADER LINE,
      LS_PA0000_PREV TYPE PA0000,
      LT_PA0000_ADD LIKE TABLE OF LS_PA0000 WITH HEADER LINE.      .
DATA: LS_PA0002 TYPE PA0002,
      LT_PA0002 LIKE TABLE OF LS_PA0002.
DATA: LS_PA0001 TYPE PA0001,
      LT_PA0001 LIKE TABLE OF LS_PA0001.
DATA_SINGLE: BACKDAYS INT4,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             LANGU  LANGU,
             SAME_PREV_EVENT CHAR1, "The same type of event
             ANY_PREV_EVENT CHAR1,
             PREV_EVENT_NUM INT1.
DATA: LV_PERNR_CURR TYPE PERSNO,
      LV_BEGDA TYPE BEGDA,
      LV_PERNR_CNT TYPE I.
*
*
*
 LV_BACKDAYS = 1.
 LV_LANGU = 'E'.
 LV_DURATION_UNIT = 'D'.
 LV_PREV_EVENT_NUM = 1.
*
 SELECT_SINGLE: BACKDAYS,
                DURATION_UNIT,
                LANGU,
                SAME_PREV_EVENT,
                ANY_PREV_EVENT,
                PREV_EVENT_NUM.
*
*
DATA_MULTY: PERNR       PERSNO,
            MASSN       MASSN,
            BUKRS       BUKRS,
            WERKS       PERSA,
            PERSG       PERSG,
            PERSK       PERSK,
            BTRTL       BTRTL,
            PLANS       PLANS,
            STAT1       STAT1,
            STAT2       STAT2,
            STAT3       STAT3,
            ORGEH       ORGEH,
            STELL       STELL,
            DURATION   /SKN/E_SW_DURATION,
            DATUM        SY-DATUM.
SELECT_MULTY:
            PERNR,
            MASSN,
            BUKRS,
            WERKS,
            PERSG,
            PERSK,
            BTRTL,
            PLANS,
            STAT1,
            STAT2,
            STAT3,
            ORGEH,
            STELL,
            DURATION,
            DATUM .
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_08_EMP_EVENT'
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
*"convert and check langu
CONVERT_SINGLE: LANGU ISOLA.
SELECT SINGLE SPRAS INTO SPRAS_T
  FROM T002
  WHERE SPRAS = LV_LANGU.
  IF SY-SUBRC <> 0.
    LV_LANGU = 'E'.
  ENDIF.
************************************
    SY_DATLO = SY-DATUM .        " Appl Server's Date
    SY_TIMLO = SY-UZEIT.
*
********************************************************
    DATE_FROM = SY-DATUM.
    DATE_TO = SY-DATUM.
   IF R_DATUM[] IS INITIAL .
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      DATE_FROM = SY-DATUM - LV_BACKDAYS .
      DATE_TO = SY-DATUM.
      RS_DATUM-LOW = DATE_FROM .
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
*********************************************************
**  if lv_NO_DATE_RESTRICTION is not initial.
**    refresh R_DATUM.
**  endif.
*
**--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
*
   SELECT *
     FROM PA0000
     INTO CORRESPONDING FIELDS OF TABLE LT_PA0000
     WHERE   PERNR IN R_PERNR
       AND    MASSN IN R_MASSN
       AND STAT1 IN R_STAT1
       AND STAT2 IN R_STAT2
       AND STAT3 IN R_STAT3
       AND BEGDA IN R_DATUM.
   "--- Get the  Prev Event ----
      IF LV_ANY_PREV_EVENT IS NOT INITIAL.
        CLEAR LV_SAME_PREV_EVENT.
      ENDIF.
   "--- Get the SAME Prev Event ----
   REFRESH LT_PA0000_PREV.
   IF LV_SAME_PREV_EVENT IS NOT INITIAL.
     IF LT_PA0000[] IS NOT INITIAL.
       SELECT *
       FROM PA0000
       INTO CORRESPONDING FIELDS OF TABLE LT_PA0000_PREV
       FOR ALL ENTRIES IN LT_PA0000
       WHERE  PERNR = LT_PA0000-PERNR
         AND  SUBTY = LT_PA0000-SUBTY
         AND  OBJPS = LT_PA0000-OBJPS
         AND  SPRPS = LT_PA0000-SPRPS
         AND  MASSN = LT_PA0000-MASSN
         AND  ENDDA < LT_PA0000-BEGDA.
     ENDIF.
   ENDIF.
   IF LV_ANY_PREV_EVENT IS NOT INITIAL.
     IF LT_PA0000[] IS NOT INITIAL.
       SELECT *
       FROM PA0000
       INTO CORRESPONDING FIELDS OF TABLE LT_PA0000_PREV
       FOR ALL ENTRIES IN LT_PA0000
       WHERE  PERNR = LT_PA0000-PERNR
         AND  SUBTY = LT_PA0000-SUBTY
         AND  OBJPS = LT_PA0000-OBJPS
         AND  SPRPS = LT_PA0000-SPRPS
"         and  MASSN = lt_PA0000-MASSN
         AND  ENDDA < LT_PA0000-BEGDA.
     ENDIF.
   ENDIF.
   SORT LT_PA0000_PREV BY PERNR BEGDA DESCENDING.
   CLEAR: LV_PERNR_CURR.
   LV_PERNR_CNT = 1.
   REFRESH LT_PA0000_ADD.
   LOOP AT LT_PA0000 INTO LS_PA0000.
     CLEAR LV_PERNR_CNT.
     LOOP AT LT_PA0000_PREV INTO LS_PA0000_PREV WHERE PERNR = LS_PA0000-PERNR.
       IF LS_PA0000_PREV-ENDDA < LS_PA0000-BEGDA.
         ADD 1 TO LV_PERNR_CNT.
         IF LV_PERNR_CNT <= LV_PREV_EVENT_NUM.
           APPEND LS_PA0000_PREV TO LT_PA0000_ADD.
         ENDIF.
       ENDIF.
     ENDLOOP.
   ENDLOOP.
   LOOP AT LT_PA0000_ADD INTO LS_PA0000.
     APPEND LS_PA0000 TO LT_PA0000.
   ENDLOOP.
*   loop at lt_PA0000_PREV into ls_PA0000.
*     if lv_PERNR_curr = ls_PA0000-PERNR.
*       add 1 to lv_PERNR_cnt.
*     else.
*       lv_PERNR_cnt = 1.
*       lv_PERNR_curr = ls_PA0000-PERNR.
*     endif.
*     if lv_PERNR_cnt <= lv_PREV_EVENT_NUM.
*       append ls_PA0000 to lt_PA0000.
*     endif.
*   endloop.
   SORT LT_PA0000 BY PERNR BEGDA.
   "--- Get the  Prev Event ----
   IF LT_PA0000[] IS NOT INITIAL.
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
          AND ORGEH IN R_ORGEH
          AND STELL IN R_STELL
          "and ENDDA   = '99991231'.
          AND BEGDA   <= LT_PA0000-BEGDA
          AND ENDDA   >= LT_PA0000-BEGDA.
     SELECT *
       FROM PA0002
       INTO CORRESPONDING FIELDS OF TABLE LT_PA0002
       FOR ALL ENTRIES IN LT_PA0000
       WHERE  PERNR = LT_PA0000-PERNR
          "and ENDDA   = '99991231'.
          AND BEGDA   <= LT_PA0000-BEGDA
          AND ENDDA   >= LT_PA0000-BEGDA.
   ENDIF.
  REFRESH T_DATA.
  LOOP AT LT_PA0000 INTO LS_PA0000.
    LOOP AT LT_PA0001 INTO LS_PA0001 WHERE PERNR = LS_PA0000-PERNR
                                       AND BEGDA <= LS_PA0000-BEGDA
                                       AND ENDDA >= LS_PA0000-BEGDA.
      LOOP AT LT_PA0002 INTO LS_PA0002 WHERE PERNR = LS_PA0000-PERNR
                                       AND BEGDA <= LS_PA0000-BEGDA
                                       AND ENDDA >= LS_PA0000-BEGDA.
        MOVE-CORRESPONDING LS_PA0001 TO LS_DATA.
        MOVE-CORRESPONDING LS_PA0002 TO LS_DATA.
        MOVE-CORRESPONDING LS_PA0000 TO LS_DATA.
        APPEND LS_DATA TO T_DATA.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
*********************
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-BEGDA
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
 LOOP AT T_DATA.
   SY_TABIX = SY-TABIX.
   CALL FUNCTION '/SKN/F_SW_10_USRID_BY_PERNR'
     EXPORTING
       PERNR         = T_DATA-PERNR
    IMPORTING
      USRID         = T_DATA-USRID
    EXCEPTIONS
      WRONG_CODE       = 1
*      OTHERS           = 2
             .
*****Act. type-**********
   CALL FUNCTION '/SKN/F_SW_10_ACTION_TYP_DESC'
     EXPORTING
       MASSN                 = T_DATA-MASSN
      LANGU                 = LV_LANGU
    IMPORTING
     ACTION_TYP_DESC       = T_DATA-MNTXT
    EXCEPTIONS
      WRONG_CODE            = 1
*      OTHERS                = 2
             .
   IF SY-SUBRC <> 0.
* Implement suitable error handling here
   ENDIF.
*****-REASON-********
   CALL FUNCTION '/SKN/F_SW_10_ACT_REASON_DESC'
     EXPORTING
       MASSN                    = T_DATA-MASSN
       MASSG                    = T_DATA-MASSG
      LANGU                    = LV_LANGU
    IMPORTING
      ACTION_REASON_DESC       = T_DATA-MGTXT
    EXCEPTIONS
      WRONG_CODE               = 1
*      OTHERS                   = 2
             .
   IF SY-SUBRC <> 0.
   ENDIF.
*****-Employee Group-***********
   CALL FUNCTION '/SKN/F_SW_10_EMP_GROUP_DESC'
     EXPORTING
       PERSG                = T_DATA-PERSG
      LANGU                = LV_LANGU
    IMPORTING
      EMP_GROUP_DESC       = T_DATA-PGTXT
    EXCEPTIONS
      WRONG_CODE           = 1
*      OTHERS               = 2
             .
   IF SY-SUBRC <> 0.
   ENDIF.
*****-Emp. Subgroup -******************
  CALL FUNCTION '/SKN/F_SW_10_EMP_SUBGROUP_DESC'
    EXPORTING
      PERSK                   = T_DATA-PERSK
     LANGU                   = LV_LANGU
   IMPORTING
     EMP_SUBGROUP_DESC       = T_DATA-PKTXT
   EXCEPTIONS
     WRONG_CODE              = 1
*     OTHERS                  = 2
            .
  IF SY-SUBRC <> 0.
  ENDIF.
             .
******- Job  -**************
  CALL FUNCTION '/SKN/F_SW_10_EMP_JOB_DESC'
    EXPORTING
     STELL              = T_DATA-STELL
     LANGU              = LV_LANGU
   IMPORTING
     EMP_JOB_DESC       = T_DATA-STLTX
   EXCEPTIONS
     WRONG_CODE         = 1
*   OTHERS             = 2
          .
  IF SY-SUBRC <> 0.
  ENDIF.
*****-ORG UNIT  -**************
  CALL FUNCTION '/SKN/F_SW_10_ORG_UNIT_DESC'
    EXPORTING
      ORGEH               = T_DATA-ORGEH
      LANGU                = LV_LANGU
   IMPORTING
      ORG_UNIT_DESC       = T_DATA-ORGTX
   EXCEPTIONS
     WRONG_CODE          = 1
*   OTHERS              = 2
          .
  IF SY-SUBRC <> 0.
  ENDIF.
********Pers. Area-*************
  CALL FUNCTION '/SKN/F_SW_10_PERS_AREA_DESC'
    EXPORTING
      PERSA                = T_DATA-WERKS
   IMPORTING
     PERS_AREA_DESC       = T_DATA-PBTXT
   EXCEPTIONS
     WRONG_CODE           = 1
*    OTHERS               = 2
          .
  IF SY-SUBRC <> 0.
  ENDIF.
******-Pers. SUBarea-**********
  CALL FUNCTION '/SKN/F_SW_10_PERS_SUBAREA_DESC'
    EXPORTING
      PERSA                   = T_DATA-WERKS
      BTRTL                   = T_DATA-BTRTL
   IMPORTING
     PERS_SUBAREA_DESC       = T_DATA-BTRTX
   EXCEPTIONS
     WRONG_CODE              = 1
*     OTHERS                  = 2
            .
  IF SY-SUBRC <> 0.
  ENDIF.
*****-Position-*********
  CALL FUNCTION '/SKN/F_SW_10_EMP_POSITION_DESC'
    EXPORTING
      PLANS               = T_DATA-PLANS
      OTYPE               = 'S'
     LANGU               = LV_LANGU
   IMPORTING
     POSITION_DESC       = T_DATA-PLSTX
   EXCEPTIONS
     WRONG_CODE          = 1
*     OTHERS              = 2
            .
  IF SY-SUBRC <> 0.
  ENDIF.
*****- Comp. Code-*********
  CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
    EXPORTING
      BUKRS                = T_DATA-BUKRS
   IMPORTING
     COMP_CODE_DESC       = T_DATA-BUTXT
   EXCEPTIONS
     WRONG_CODE           = 1
*     OTHERS               = 2
            .
  IF SY-SUBRC <> 0.
  ENDIF.
********************************
   MODIFY T_DATA INDEX SY_TABIX .
 ENDLOOP.
**--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
