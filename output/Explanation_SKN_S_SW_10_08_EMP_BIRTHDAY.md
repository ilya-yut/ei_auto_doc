# Exception Indicator: HR Employee Birthday ( SW_10_08_EMP_BIRTHD)

## General Overview

This Exception Indicator identifies employees whose birthdays fall within a configurable calendar window around the evaluation date, enriches each match with current organizational assignment and employment status, and resolves the responsible leader for follow-up.

This EI serves as an essential control for human resources operations and people management by:
- Surfacing personnel whose birth month and day align with the monitoring window so teams can run timely recognition or compliance activities
- Enabling HR administrators to limit results by company, personnel area, employee group, position, and employment status
- Supporting managers with leader identification tied to each employee's position for routed notifications or approvals
- Helping workforce planners see name and organizational context together with computed time-since-birthday metrics for prioritization
- Complementing manual birthday lists with repeatable, parameterized extracts tied to live HR master data

Typical use includes weekly birthday announcements, anniversary-style outreach for executives, and periodic checks that active employees in selected populations have upcoming birthdays. Results are intended for exception workflows rather than full HR reporting extracts.

The routine reads personal and organizational infotype records valid on the evaluation date, matches month-and-day patterns across the configured date span, applies duration filtering, and raises an alert when qualifying employees remain in scope.


## Problem Description

Failure to monitor employee birthdays within defined organizational populations creates multiple risks across people operations, employee engagement, and management accountability.

**HR Operations and Employee Experience Risks**
- Birthday recognition programs miss eligible employees when dates are tracked manually across spreadsheets
- Last-minute outreach fails when personnel area or status filters are not applied consistently before the event date
- Contract or international assignment populations are overlooked when company and subgroup scoping is ad hoc

**Management and Accountability Risks**
- Position-based leader resolution is skipped, so notifications reach the wrong manager or no manager at all
- Teams cannot prioritize upcoming birthdays when duration-based aging is not calculated uniformly across the result set

**Compliance and Data Quality Risks**
- Reviews rely on outdated extracts instead of infotype records valid on the evaluation date
- Inactive or non-productive employment statuses remain in birthday lists without status filtering

## Suggested Resolution

**Immediate Response**
- Review each flagged employee together with name, birth date, organizational assignment, and employment status shown in the exception
- Confirm the leader identifier with the responsible manager before sending customer-facing or internal birthday communications
- Exclude known exceptions (leave, termination in progress) when status fields indicate the person should not receive outreach

**System Assessment**
- Compare this cycle to prior runs after changes to personnel area structure, employee group definitions, or holiday calendars
- Look for concentrations by company, personnel area, or position to see whether one unit drives most upcoming birthdays
- Validate that backward and forward day settings still match how far ahead HR wants to prepare recognition activities

**Corrective Actions**
- Correct master data through standard personnel administration when birth date or assignment fields are wrong
- Adjust monitoring scope after root cause so the queue stays actionable for HR operations
- Document recurring false positives when organizational keys or status combinations need template updates
- Route repeat data-quality issues into HR master-data governance when infotype maintenance or interfaces require fixes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BEGDA | Start Date | DATS | 8 | 0 | BEGDA | DATUM |
| 3 | BTRTL | Personnel subarea | CHAR | 4 | 0 | BTRTL | BTRTL |
| 4 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 5 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 6 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 7 | ENDDA | End Date | DATS | 8 | 0 | ENDDA | DATUM |
| 8 | EXEC_YEAR | Year of birth | NUMC | 4 | 0 | GBJHR | GJAHR |
| 9 | FORWDAYS | Days Forward |  | 0 | 0 |  |  |
| 10 | GBDAT | Date of birth | DATS | 8 | 0 | GBDAT | GBDAT |
| 11 | GBJHR | Year of birth | NUMC | 4 | 0 | GBJHR | GJAHR |
| 12 | GBMON | Month of Birth | NUMC | 2 | 0 | GBMON | NUM2 |
| 13 | GBTAG | Birth Date (to Month/Year) | NUMC | 2 | 0 | GBTAG | NUM2 |
| 14 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 15 | LEADER_ID | Personnel number | NUMC | 8 | 0 | PERSNO | PERSNO |
| 16 | NACHN | Last name | CHAR | 40 | 0 | PAD_NACHN | PAD_NACHN |
| 17 | OBJPS | Object ID | CHAR | 2 | 0 | OBJPS | OBJPS |
| 18 | OTYPE | Object Type |  | 0 | 0 |  |  |
| 19 | PERNR | Personnel number | NUMC | 8 | 0 | PERSNO | PERSNO |
| 20 | PERSG | Employee group | CHAR | 1 | 0 | PERSG | PERSG |
| 21 | PERSK | Employee subgroup | CHAR | 2 | 0 | PERSK | PERSK |
| 22 | PLANS | Position | NUMC | 8 | 0 | PLANS | PLANS |
| 23 | PLVAR | Plan Version |  | 0 | 0 |  |  |
| 24 | SEQNR | Infotype record no. | NUMC | 3 | 0 | SEQNR | NUM03 |
| 25 | SPRPS | Lock indicator | CHAR | 1 | 0 | SPRPS | SPRPS |
| 26 | STAT1 | Cust.-specific stat. | CHAR | 1 | 0 | STAT1 | STATA |
| 27 | STAT2 | Employment status | CHAR | 1 | 0 | STAT2 | STATA |
| 28 | STAT3 | Spec.payment status | CHAR | 1 | 0 | STAT3 | STATA |
| 29 | SUBTY | Subtype | CHAR | 4 | 0 | SUBTY | SUBTY |
| 30 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 31 | VDSK1 | Organizational key | CHAR | 14 | 0 | VDSK1 | VDSK1 |
| 32 | VORNA | First name | CHAR | 40 | 0 | PAD_VORNA | PAD_VORNM |
| 33 | WERKS | Personnel area | CHAR | 4 | 0 | PERSA | PERSA |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 33 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**BEGDA** (Start Date)

Aligns exception volume with the chosen scope by testing start date via BEGDA before alert evaluation.

**BTRTL** (Personnel subarea)

Documents expected operator behavior—personnel subarea on BTRTL should be set when that dimension is part of the control objective.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

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

After data is read, lines are removed unless end date on ENDDA still satisfies the active multivalued selection.

**EXEC_YEAR** (Year of birth)

Captures edge cases where year of birth (EXEC_YEAR) must be non-default to reproduce a customer-specific monitoring scenario.

**FORWDAYS** (Days Forward)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

**GBDAT** (Date of birth)

Separates cross-client noise from in-scope work when date of birth on GBDAT correlates with client or user attributes.

**GBJHR** (Year of birth)

Explains why two monitoring passes differ: only the pass with stricter year of birth on GBJHR surfaces the disputed rows.

**GBMON** (Month of Birth)

Improves readability of exported lists because month of birth (GBMON) columns stay aligned with the configured filter intent.

**GBTAG** (Birth Date (to Month/Year))

For distributed landscapes, birth date (to month/year) on GBTAG often anchors which application server or destination appears in results.

**GSBER** (Business Area)

Business area key used for FI organizational reporting segmentation.

**LEADER_ID** (Personnel number)

Supports operational control by evaluating personnel number through LEADER_ID for each candidate record.

**NACHN** (Last name)

Stabilizes week-over-week metrics by fixing last name (NACHN) while allowing duration thresholds to move.

**OBJPS** (Object ID)

When combined with destination discipline, object id on OBJPS keeps both breadth and depth of the extract intentional.

**OTYPE** (Object Type)

Treats object type as a discriminator between similar rows that would otherwise look identical in a raw extract.

**PERNR** (Personnel number)

Combines with related filters so personnel number on PERNR refines which records remain for duration or state checks.

**PERSG** (Employee group)

Reduces false positives during peak windows by tightening employee group through PERSG alongside state filters.

**PERSK** (Employee subgroup)

Reflects real administration where employee subgroup on PERSK is routinely restricted to a single productive client or object family.

**PLANS** (Position)

Narrows retrieved rows where position (PLANS) must match the configured selection for this monitor.

**PLVAR** (Plan Version)

Works downstream of the initial read so plan version on PLVAR still participates in row-level deletion rules.

**SEQNR** (Infotype record no.)

Captures edge cases where infotype record no. (SEQNR) must be non-default to reproduce a customer-specific monitoring scenario.

**SPRPS** (Lock indicator)

Helps distinguish technical versus business attributes when lock indicator on SPRPS correlates with counters or status fields.

**STAT1 - STAT3** (Cust.-specific stat.)

Uses cust.-specific stat. from the source context so only records with STAT1 inside declared values proceed.

**SUBTY** (Subtype)

Supports operational control by evaluating subtype through SUBTY for each candidate record.

**USER_FLD** (Dynamic Recipient User Field)

The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.
How DRL Works:
When USER_FLD is specified, the system extracts values from that field in the monitoring result set
These extracted values are then used as recipient addresses for alert notifications
This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored
The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users

**VDSK1** (Organizational key)

Connects to alert semantics: rows removed for failing organizational key on VDSK1 never reach downstream filtering.

**VORNA** (First name)

Pairs with duration logic: once VORNA passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**WERKS** (Personnel area)

Plant key used to scope logistics/procurement records by site.


### Parameter Relationships

How parameter combinations work together

**Monitoring window around today:** **BACKDAYS** and **FORWDAYS** define how many days before and after the evaluation date are included when building the birthday month-and-day range. Together they set the calendar span used to derive which birth dates qualify before employee master data is read.

**Employee population scope:** **PERNR**, **BUKRS**, **WERKS**, **PERSG**, **PERSK**, **BTRTL**, **PLANS**, and employment status fields (**STAT1**, **STAT2**, **STAT3**) narrow which personnel records are considered once the birthday window is established.

**Age filter after selection:** **DURATION** with **DURATION_UNIT** is an additional filter applied after rows are assembled: each employee must still fit the configured elapsed-time band measured from the evaluation moment to the birthday date used in the duration calculation.

**Organizational leader resolution:** **PLVAR** and **OTYPE** work with each row's position (**PLANS**) so the responsible leader identifier can be resolved for routing or review.

**Final selection:** The backward/forward day window, employee and status filters, duration band, and leader resolution apply together—rows must satisfy the active combination of window, scope, and duration conditions before they appear in the final alert population.


### Default Values

- **BACKDAYS** - 0
- **FORWDAYS** - 1
- **DURATION_UNIT** - D
- **PLVAR** - 01
- **OTYPE** - S
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Today and tomorrow birthdays**

**Purpose:** List active employees in a personnel area whose birthdays fall today or the next calendar day for short-notice recognition.
```
BACKDAYS = 0
FORWDAYS = 1
WERKS = 1000
STAT2 = 3
PERSG = 1
```

**Use Case 2: Seven-day lookahead with day-based duration cap**

**Purpose:** Prepare HR communications for birthdays in the coming week while excluding rows whose computed duration falls outside an acceptable day band.
```
BACKDAYS = 0
FORWDAYS = 7
DURATION = 0 - 7
DURATION_UNIT = D
BUKRS = 1000
```

**Use Case 3: Full-day duration band for executive population**

**Purpose:** Focus on a leadership employee subgroup where birthdays must be at least fourteen full days before the evaluation date.
```
BACKDAYS = 0
FORWDAYS = 30
PERSK = E1
DURATION = 14
DURATION_UNIT = F
STAT1 = 1
```

**Use Case 4: Company-wide scan with position filter**

**Purpose:** Review upcoming birthdays for employees assigned to selected positions across two company codes.
```
BACKDAYS = 3
FORWDAYS = 3
BUKRS = 1000 - 2000
PLANS = 50000001 - 50009999
PLVAR = 01
OTYPE = S
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_08_EMP_BIRTHDAY | BACKDAYS | Backdays | CHAR(0) | BACKDAYS |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | BEGDA | Start Date | DATS(8) | BEGDA |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | BTRTL | Personnel subarea | CHAR(4) | BTRTL |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | DURATION_UNIT | Duration Unit(D/H/M) | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | ENDDA | End Date | DATS(8) | ENDDA |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | EXEC_YEAR | Year of birth | NUMC(4) | GBJHR |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | FORWDAYS | Days Forward | CHAR(0) | FORWDAYS |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | GBDAT | Date of birth | DATS(8) | GBDAT |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | GBJHR | Year of birth | NUMC(4) | GBJHR |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | GBMON | Month of Birth | NUMC(2) | GBMON |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | GBTAG | Birth Date (to Month/Year) | NUMC(2) | GBTAG |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | LEADER_ID | Personnel number | NUMC(8) | PERSNO |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | NACHN | Last name | CHAR(40) | PAD_NACHN |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | OBJPS | Object ID | CHAR(2) | OBJPS |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | OTYPE | Object Type | CHAR(0) | OTYPE |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | PERNR | Personnel number | NUMC(8) | PERSNO |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | PERSG | Employee group | CHAR(1) | PERSG |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | PERSK | Employee subgroup | CHAR(2) | PERSK |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | PLANS | Position | NUMC(8) | PLANS |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | PLVAR | Plan Version | CHAR(0) | PLVAR |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | SEQNR | Infotype record no. | NUMC(3) | SEQNR |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | SPRPS | Lock indicator | CHAR(1) | SPRPS |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | STAT1 | Cust.-specific stat. | CHAR(1) | STAT1 |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | STAT2 | Employment status | CHAR(1) | STAT2 |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | STAT3 | Spec.payment status | CHAR(1) | STAT3 |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | SUBTY | Subtype | CHAR(4) | SUBTY |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | USER_FLD | Dynamic Recipient User Field | CHAR(0) | USER_FLD |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | VDSK1 | Organizational key | CHAR(14) | VDSK1 |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | VORNA | First name | CHAR(40) | PAD_VORNA |
| /SKN/S_SW_10_08_EMP_BIRTHDAY | WERKS | Personnel area | CHAR(4) | PERSA |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_08_EMP_BIRTHDAY .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_08_EMP_BIRTHDAY OPTIONAL
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
DATA_SINGLE: BACKDAYS        INT4,
             FORWDAYS        INT4,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             PLVAR          PLVAR,
             OTYPE          OTYPE.
*
 LV_BACKDAYS = 0.
 LV_FORWDAYS = 1.
 LV_DURATION_UNIT = 'D'.
 LV_PLVAR = '01'.
 LV_OTYPE = 'S'.
*
 SELECT_SINGLE: BACKDAYS,
                FORWDAYS,
                DURATION_UNIT,
                PLVAR,
                OTYPE.
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
*
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_08_EMP_BIRTHDAY'
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
***   lv_date = sy-datum.
***   if MONTH_FROM > MONTH_TO.
***     lv_date+4(4) = '0101' . lv_date = lv_date - 1. "Prev Year
***     lv_date+4(4) = lv_MONTDAY.
***   else.
***      lv_date+4(4) = lv_MONTDAY.
***   endif.
  LV_DATE = DATE_FROM.
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
    REFRESH: R_MONTH,
             R_DAY.
    LOOP AT R_MONTDAY INTO RS_MONTDAY.
      MOVE-CORRESPONDING RS_MONTDAY TO RS_MONTH.
       RS_MONTH-LOW = RS_MONTDAY-LOW+0(2).
      APPEND RS_MONTH TO R_MONTH.
      MOVE-CORRESPONDING RS_MONTDAY TO RS_DAY.
       RS_DAY-LOW = RS_MONTDAY-LOW+2(2).
      APPEND RS_DAY TO R_DAY.
    ENDLOOP.
    SORT R_MONTH.
    DELETE ADJACENT DUPLICATES FROM R_MONTH.
    SORT R_DAY.
    DELETE ADJACENT DUPLICATES FROM  R_DAY.
*********************************************************
**--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
*
  SELECT *
     FROM PA0002
     INTO CORRESPONDING FIELDS OF TABLE LT_PA0002
     WHERE PERNR IN R_PERNR
       AND BEGDA = SY-DATUM
       AND GBMON IN R_MONTH
       AND GBTAG IN R_DAY.
   "--- Restrict for the required Birth Days
    LOOP AT LT_PA0002 INTO LS_PA0002.
      SY_TABIX = SY-TABIX .
     LV_MONTDAY = LS_PA0002-GBDAT+4(4).
     IF LV_MONTDAY NOT IN R_MONTDAY.
       DELETE LT_PA0002 INDEX SY_TABIX.
     ENDIF.
    ENDLOOP.
   IF LT_PA0002[] IS NOT INITIAL.
     SELECT *
       FROM PA0001
       INTO CORRESPONDING FIELDS OF TABLE LT_PA0001
       FOR ALL ENTRIES IN LT_PA0002
       WHERE  PERNR = LT_PA0002-PERNR
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
       FOR ALL ENTRIES IN LT_PA0002
       WHERE  PERNR = LT_PA0002-PERNR
          AND STAT1 IN R_STAT1
          AND STAT2 IN R_STAT2
          AND STAT3 IN R_STAT3
          AND BEGDA   = SY-DATUM.
   ENDIF.
  REFRESH T_DATA.
  LOOP AT LT_PA0002 INTO LS_PA0002.
    LOOP AT LT_PA0001 INTO LS_PA0001 WHERE PERNR = LS_PA0002-PERNR
                                       AND BEGDA = SY-DATUM.
      LOOP AT LT_PA0000 INTO LS_PA0000 WHERE PERNR = LS_PA0002-PERNR
                                       AND BEGDA = SY-DATUM.
        MOVE-CORRESPONDING LS_PA0000 TO LS_DATA.
        MOVE-CORRESPONDING LS_PA0001 TO LS_DATA.
        MOVE-CORRESPONDING LS_PA0002 TO LS_DATA.
        APPEND LS_DATA TO T_DATA.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
*********************
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
   LV_DATE = SY-DATUM.
   LV_DATE+4(4) = T_DATA-GBDAT+4(4).
   IF LV_DATE > DATE_TO.
     LV_YEAR = LV_DATE+0(4).  LV_YEAR = LV_YEAR - 1. LV_DATE+0(4) = LV_YEAR.
   ENDIF.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = SY_DATLO
          T_FROM            = SY_TIMLO
          D_TO              = LV_DATE
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
LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   LV_OBJID = T_DATA-PLANS.
   CLEAR LV_LEADER_ID.
   CALL FUNCTION 'RH_GET_LEADER'
     EXPORTING
       PLVAR                           = LV_PLVAR
       KEYDATE                         = SY-DATUM
       OTYPE                           = LV_OTYPE
       OBJID                           = LV_OBJID "T_DATA-PLANS
      GET_LEADER_TAB                  = 'X'
*      CONSIDER_VAC_POS                = ' '
    IMPORTING
*      LEADER_TYPE                     =
      LEADER_ID                       = LV_LEADER_ID
*      MULTIPLE                        =
*    TABLES
*      LEADER_TAB                      =
    EXCEPTIONS
      NO_LEADER_FOUND                 = 1
      NO_LEADING_POSITION_FOUND       = 2
      OTHERS                          = 3
             .
   IF SY-SUBRC <> 0.
* Implement suitable error handling here
   ENDIF.
   T_DATA-LEADER_ID = LV_LEADER_ID .
   T_DATA-EXEC_YEAR = SY-DATUM+0(4).
   MODIFY T_DATA INDEX SY_TABIX .
ENDLOOP.
**--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
