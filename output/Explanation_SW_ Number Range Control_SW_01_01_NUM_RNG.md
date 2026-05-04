# Exception Indicator: SW Number Range Control - SW_01_01_NUM_RNG

## General Overview

This Exception Indicator monitors SAP number range intervals as stored for posting and sequencing, focusing on remaining capacity, utilization percentages, and optional runway projections based on recent consumption statistics.

This EI serves as an essential control for application operations and audit readiness by:
- Surfacing intervals where free numbers are exhausted or critically low before document creation fails in production
- Highlighting skew between allocated, used, and remaining counts when ranges are close to rollover or year boundaries
- Supporting capacity reviews when specific business objects concentrate consumption risk across clients or fiscal years
- Giving teams a consolidated view enriched with short and long descriptions for communication with non-technical stakeholders
- Helping compliance and IT governance demonstrate that number range health was reviewed on a recurring basis

Typical use includes go-live readiness checks, year-end or fiscal-year rollover preparation, and investigations after posting errors tied to numbering. Teams pair results with transaction SNRO and object-specific maintenance when changes are required.

The routine reads interval rows from the central number range interval table and may call supporting routines for descriptions and statistical projections.


## Problem Description

Failure to monitor number range utilization and runway creates multiple risks across business continuity, data integrity, and compliance:

**Operational and Posting Continuity Risks**
- Documents and batch jobs can fail abruptly when the next number cannot be issued, halting sales, logistics, or financial close activities
- Low-visibility intervals may exhaust quietly until the first user-facing error appears in peak processing windows
- Year-boundary or object-family concentration is missed when teams only react to individual error messages

**Data Integrity and Master-Data Risks**
- Manual emergency fixes to numbering can introduce gaps, overlaps, or inconsistent year handling if intervals were not monitored proactively
- Misaligned subobjects or external numbering flags prolong incorrect utilization reporting until someone reconciles SNRO with runtime behavior

**Governance and Audit Risks**
- Regulators and internal auditors expect evidence that critical numbering objects stayed within agreed utilization bands
- Post-incident reviews lack objective history when no monitor captured percentage used, numbers left, or projected runway

## Suggested Resolution

**Immediate Response**
- Review each flagged interval for object, subobject, fiscal year, and utilization context to judge business severity and next owner
- Open transaction SNRO (or equivalent) for the same object and subobject to validate current numbers, buffering, and year settings
- Confirm whether low remaining counts are expected seasonality or indicate misconfiguration, excessive consumption, or missing extension of the interval

**System Assessment**
- Segment results by object family, fiscal year, and utilization percentages to see where risk concentrates
- Compare current utilization to prior monitoring cycles after major campaigns, migrations, or go-lives that increase posting volume
- When statistical estimation is enabled, examine projected runway fields alongside raw remaining counts to validate plausibility

**Corrective Actions**
- Extend intervals, add buffering, or roll fiscal-year definitions according to SAP best practices for the affected object
- Correct master data or customizing when subobject or external-indicator mismatches drive false risk signals
- Document remediation for audit when numbering touched regulated document chains or statutory reporting
- Tune monitoring selections after root-cause review so benign objects do not hide material exceptions
- Schedule recurring monitoring after template releases so new objects inherit healthy numbering behavior early


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AVAILABLE_DEC | Available Numbers (Dec Value) | DEC | 25 | 0 | DEC_25 | DEC_25 |
| 2 | AVG_DAILY | P value | DEC | 25 | 0 | DEC_25 | DEC_25 |
| 3 | AVG_DAILY_NZ | P value | DEC | 25 | 0 | DEC_25 | DEC_25 |
| 4 | EXTERNIND | External ('X') | CHAR | 1 | 0 | NRIND | XFELD |
| 5 | FROMNUMBER | From number | CHAR | 20 | 0 | NRFROM | CHAR20 |
| 6 | FROMNUMBER_DEC | From Number (Dec Value) | DEC | 25 | 0 | DEC_25 | DEC_25 |
| 7 | LANGU | Languege for description |  | 0 | 0 |  |  |
| 8 | LEFT_DAYS | Days left (generally) | INT4 | 10 | 0 | INT4 | INT4 |
| 9 | LEFT_DAYS_ESTIMATION | X - Estimate the left days |  | 0 | 0 |  |  |
| 10 | LEFT_DAYS_NZ | Days left (with activities) | INT4 | 10 | 0 | INT4 | INT4 |
| 11 | LEFT_DEC | Left Numbers | DEC | 25 | 0 | DEC_25 | DEC_25 |
| 12 | LEFT_PER | Left Numbers (%) | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 13 | NRLEVEL | Current Number | NUMC | 20 | 0 | NRLEVEL | NUMC20 |
| 14 | NRLEVEL_DEC | Current Number (Dec Value) | DEC | 25 | 0 | DEC_25 | DEC_25 |
| 15 | NRRANGENR | Number range number | CHAR | 2 | 0 | NRNR | CHAR2 |
| 16 | OBJECT | Object name | CHAR | 10 | 0 | NROBJ | NROBJ |
| 17 | STATISTICS_ALERT | Alert Instance (Code) for stat |  | 0 | 0 |  |  |
| 18 | STATISTICS_DAYS | Days for statistics |  | 0 | 0 |  |  |
| 19 | SUBOBJECT | Subobject value | CHAR | 6 | 0 | NRSOBJ | CHAR6 |
| 20 | TONUMBER | To number | CHAR | 20 | 0 | NRTO | CHAR20 |
| 21 | TONUMBER_DEC | To Number (Dec Value) | DEC | 25 | 0 | DEC_25 | DEC_25 |
| 22 | TOYEAR | To year | NUMC | 4 | 0 | NRYEAR | GJAHR |
| 23 | TXT | Long text | CHAR | 60 | 0 | NROBJTXT | TEXT60 |
| 24 | TXTSHORT | Short text | CHAR | 20 | 0 | NROBJSTXT | TEXT20 |
| 25 | USED_DEC | Used Numbers | DEC | 25 | 0 | DEC_25 | DEC_25 |
| 26 | USED_PER | Used Numbers (%) | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 26 parameters listed in the Parameters Reference Table when tuning this EI; each influences which number range intervals are retrieved, enriched with utilization, and optionally projected with runway days.

**AVAILABLE_DEC** (Available Numbers (Dec Value))

Mirrors how teams reason about SNRO traffic: available numbers (dec value) (AVAILABLE_DEC) is one lever that shapes which intervals are comparable run over run.

**AVG_DAILY** (P value)

Allows phased rollout: first widen AVG_DAILY for p value, then tighten percentage bands once baseline noise is understood.

**AVG_DAILY_NZ** (P value)

After reading number range intervals, rows are removed unless p value on AVG_DAILY_NZ still satisfies the active filter criteria.

**EXTERNIND** (External ('X'))

Connects to alert semantics: lines removed for failing external ('x') on EXTERNIND never reach optional day-estimation enrichment.

**FROMNUMBER** (From number)

Uses from number from the interval context so only records with FROMNUMBER inside declared values proceed.

**FROMNUMBER_DEC** (From Number (Dec Value))

Documents expected operator behavior—from number (dec value) on FROMNUMBER_DEC should be set when that dimension is part of the control objective.

**LANGU** (Languege for description)

When harmonized with remaining-number filters, languege for description on LANGU isolates the highest-risk interval families.

**LEFT_DAYS** (Days left (generally))

Supports escalation where days left (generally) on LEFT_DAYS signals ownership for follow-up between Basis and functional teams.

**LEFT_DAYS_ESTIMATION** (X - Estimate the left days)

Enables optional statistical projection that estimates how many days of runway remain based on recent consumption velocity.

**LEFT_DAYS_ESTIMATION Options:**
- **X** — Run historical consumption analysis and populate average daily usage and projected left-day fields when prerequisites are met.
- **Empty or blank** — Skip statistical estimation so only direct NRIV utilization fields drive filtering and alerts.

**LEFT_DAYS_NZ** (Days left (with activities))

Treats days left (with activities) as a discriminator between similar intervals that would otherwise look identical in a raw extract.

**LEFT_DEC** (Left Numbers)

Helps distinguish fiscal variants when left numbers on LEFT_DEC correlates with year or numbering level attributes.

**LEFT_PER** (Left Numbers (%))

Captures edge cases where left numbers (%) (LEFT_PER) must be non-default to reproduce a customer-specific monitoring scenario.

**NRLEVEL** (Current Number)

Uses current number from the interval context so only records with NRLEVEL inside declared values proceed.

**NRLEVEL_DEC** (Current Number (Dec Value))

Uses current number (dec value) from the interval context so only records with NRLEVEL_DEC inside declared values proceed.

**NRRANGENR** (Number range number)

Ensures reporting on ranges respects number range number constraints carried by NRRANGENR before any alert fires.

**OBJECT** (Object name)

Prevents accidental global scans when object name (OBJECT) is meant to stay within a controlled object catalog slice.

**STATISTICS_ALERT** (Alert Instance (Code) for stat)

Identifies which packaged monitoring statistic to read when estimating consumption; when left initial, the routine may default to the current monitor’s own analysis code if available.

**STATISTICS_DAYS** (Days for statistics)

Defines how many recent calendar days feed the consumption statistics when left-day estimation is active.

**SUBOBJECT** (Subobject value)

When harmonized with remaining-number filters, subobject value on SUBOBJECT isolates the highest-risk interval families.

**TONUMBER** (To number)

Supports escalation where to number on TONUMBER signals ownership for follow-up between Basis and functional teams.

**TONUMBER_DEC** (To Number (Dec Value))

Guards against oversized extracts when to number (dec value) on TONUMBER_DEC is narrowed together with fiscal year or external-indicators filters.

**TOYEAR** (To year)

After reading number range intervals, rows are removed unless to year on TOYEAR still satisfies the active filter criteria.

**TXT** (Long text)

Connects to alert semantics: lines removed for failing long text on TXT never reach optional day-estimation enrichment.

**TXTSHORT** (Short text)

Helps distinguish fiscal variants when short text on TXTSHORT correlates with year or numbering level attributes.

**USED_DEC** (Used Numbers)

Interprets used numbers as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on USED_DEC.

**USED_PER** (Used Numbers (%))

Combines with object and subobject filters so used numbers (%) on USED_PER refines which intervals reach utilization and percentage checks.


### Parameter Relationships

How parameter combinations work together

**OBJECT**, **SUBOBJECT**, **NRRANGENR**, and **TOYEAR** form the natural key for each interval row read from the number range repository; filters on those fields should be set together when the intent is to monitor one logical numbering stream rather than a broad catalog slice.

**LEFT_DAYS_ESTIMATION**, **STATISTICS_DAYS**, and **STATISTICS_ALERT** work as a chain: estimation runs only when the flag is active, the day window defines how much history is read for consumption averages, and the alert code selects which packaged statistic supplies the velocity inputs. The resulting **AVG_DAILY**, **AVG_DAILY_NZ**, **LEFT_DAYS**, and **LEFT_DAYS_NZ** fields are then subject to their own selection intervals.

**FROMNUMBER**, **TONUMBER**, **NRLEVEL**, and the derived decimal fields (**AVAILABLE_DEC**, **USED_DEC**, **LEFT_DEC**, **LEFT_PER**, **USED_PER**) are evaluated after the routine normalizes internal versus external numbering; percentage and absolute remaining filters should be interpreted together when judging severity.

**LANGU** aligns the descriptive text returned for each interval with the language users expect in monitoring output, which matters when the same technical key is shared across regions.


### Default Values

- **STATISTICS_DAYS** - 30 from the preset before the statistics read when the caller does not override it.
- **LEFT_DAYS_ESTIMATION** - initial — when unset, statistical day estimation and its dependent filters are skipped.
- **STATISTICS_ALERT** - initial — when unset, the routine may substitute the monitor’s own analysis code from system context when estimation is active.

### Practical Example of Parameter Configuration

**Use Case 1: Material numbering with runway projection**

**Purpose:** Monitor a specific material number range with two-week consumption statistics and estimation enabled.

```
OBJECT = MATERIAL
SUBOBJECT = *
NRRANGENR = 01
TOYEAR = 2025
LEFT_DAYS_ESTIMATION = X
STATISTICS_DAYS = 14
```

**Use Case 2: Utilization percentage alert band**

**Purpose:** Flag intervals that are at least three-quarters consumed while a meaningful absolute remainder still exists.

```
USED_PER = 75
LEFT_DEC = 500
AVAILABLE_DEC = 2000
```

**Use Case 3: Statistics source and language**

**Purpose:** Keep a thirty-day statistics window with an explicit analysis code and English descriptions.

```
LEFT_DAYS_ESTIMATION = X
STATISTICS_ALERT = NUMRNG01
STATISTICS_DAYS = 30
LANGU = E
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_NUM_RNG | AVAILABLE_DEC | P field, 25 character | DEC(25) | DEC_25 |
| /SKN/S_SW_01_01_NUM_RNG | AVG_DAILY | P field, 25 character | DEC(25) | DEC_25 |
| /SKN/S_SW_01_01_NUM_RNG | AVG_DAILY_NZ | P field, 25 character | DEC(25) | DEC_25 |
| /SKN/S_SW_01_01_NUM_RNG | EXTERNIND | Internal (' ') or external ('X') number range flag | CHAR(1) | NRIND |
| /SKN/S_SW_01_01_NUM_RNG | FROMNUMBER | From number | CHAR(20) | NRFROM |
| /SKN/S_SW_01_01_NUM_RNG | FROMNUMBER_DEC | P field, 25 character | DEC(25) | DEC_25 |
| /SKN/S_SW_01_01_NUM_RNG | LEFT_DAYS | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_01_NUM_RNG | LEFT_DAYS_NZ | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_01_NUM_RNG | LEFT_DEC | P field, 25 character | DEC(25) | DEC_25 |
| /SKN/S_SW_01_01_NUM_RNG | LEFT_PER | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_NUM_RNG | NRLEVEL | Number range status | NUMC(20) | NRLEVEL |
| /SKN/S_SW_01_01_NUM_RNG | NRLEVEL_DEC | P field, 25 character | DEC(25) | DEC_25 |
| /SKN/S_SW_01_01_NUM_RNG | NRRANGENR | Number range number | CHAR(2) | NRNR |
| /SKN/S_SW_01_01_NUM_RNG | OBJECT | Name of number range object | CHAR(10) | NROBJ |
| /SKN/S_SW_01_01_NUM_RNG | SUBOBJECT | Number range object subobject value | CHAR(6) | NRSOBJ |
| /SKN/S_SW_01_01_NUM_RNG | TONUMBER | To number | CHAR(20) | NRTO |
| /SKN/S_SW_01_01_NUM_RNG | TONUMBER_DEC | P field, 25 character | DEC(25) | DEC_25 |
| /SKN/S_SW_01_01_NUM_RNG | TOYEAR | To fiscal year | NUMC(4) | NRYEAR |
| /SKN/S_SW_01_01_NUM_RNG | TXT | Number range object text | CHAR(60) | NROBJTXT |
| /SKN/S_SW_01_01_NUM_RNG | TXTSHORT | Number range object text | CHAR(20) | NROBJSTXT |
| /SKN/S_SW_01_01_NUM_RNG | USED_DEC | P field, 25 character | DEC(25) | DEC_25 |
| /SKN/S_SW_01_01_NUM_RNG | USED_PER | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_NUM_RNG.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_NUM_RNG OPTIONAL
*"----------------------------------------------------------------------
  " variables definition
  DATA_MULTY:   OBJECT         NROBJ,
                SUBOBJECT      NRSOBJ,
                NRRANGENR      NRNR,
                NRLEVEL        NRLEVEL,
                TOYEAR         NRYEAR,
                BNAME         XUBNAME,
                CLASS         XUCLASS,
                TONUMBER      NRTO,
                FROMNUMBER    NRLEVEL,
                NRLEVEL_DEC    DEC_25,
                FROMNUMBER_DEC DEC_25,
                TONUMBER_DEC   DEC_25,
                AVAILABLE_DEC  DEC_25,
                LEFT_DEC       DEC_25,
                USED_DEC       DEC_25,
                LEFT_PER       /SKN/E_SW_PRC,
                USED_PER       /SKN/E_SW_PRC,
                EXTERNIND     NRIND.
  DATA_SINGLE:  LANGU         LANGU,
                MANAGE_IN_UTC CHAR1,
                SW_DEST       RFCDEST.
  DATA :        SY_TABIX      LIKE SY-TABIX,
                SY_DATLO      LIKE SY-DATLO,
                SY_TIMLO      LIKE SY-TIMLO,
                DOMVALUE      LIKE DD07V-DOMVALUE_L,
                DDTEXT        LIKE DD07V-DDTEXT.
  FIELD-SYMBOLS: <T_DATA_WA_FS> LIKE LINE OF T_DATA.
***data: lv_freenumabs(20) type N,
***      lv_THRESHOLDABS(20) type N,
***      lv_normalizednrlev(20) type N,
***      lv_availnumrange(20) type N,
***      lv_THRESHOLDPERC type I.
DATA: LV_F TYPE F.
  " variables population
  SELECT_MULTY: OBJECT,
                SUBOBJECT,
                NRRANGENR,
                NRLEVEL,
                TOYEAR,
                BNAME,
                CLASS,
                TONUMBER,
                FROMNUMBER,
                NRLEVEL_DEC,
                FROMNUMBER_DEC,
                TONUMBER_DEC,
                AVAILABLE_DEC,
                LEFT_DEC,
                USED_DEC,
                LEFT_PER,
                USED_PER,
                EXTERNIND.
*** convert_multy: nrlevel     ALPHA,
***                freenumabs   ALPHA,
***                thresholdabs ALPHA,
***                tonumber     ALPHA,
***                fromnumber   ALPHA.
  LV_LANGU = SY-LANGU.
  SELECT_SINGLE: MANAGE_IN_UTC,
                 LANGU,
                 SW_DEST.
  "--- Days estimation
  DATA_SINGLE: LEFT_DAYS_ESTIMATION CHAR1,
               STATISTICS_DAYS      INT2,
               STATISTICS_ALERT     /SKN/E_SW_AN_CODE.
  LV_STATISTICS_DAYS = 30.
  SELECT_SINGLE: LEFT_DAYS_ESTIMATION,
                 STATISTICS_DAYS,
                 STATISTICS_ALERT.
  DATA_MULTY:   AVG_DAILY    DEC_25,
                AVG_DAILY_NZ DEC_25,
                LEFT_DAYS    INT4,
                LEFT_DAYS_NZ INT4.
  SELECT_MULTY: AVG_DAILY,
                AVG_DAILY_NZ,
                LEFT_DAYS,
                LEFT_DAYS_NZ.
 DATA: LV_STAT_DATE_FROM TYPE  /SKN/E_SW_AN_LG_DATE,
       LV_STAT_DATE_TO TYPE  /SKN/E_SW_AN_LG_DATE.
 DATA: LS_DAILY_STAT TYPE /SKN/S_SW_01_NUM_RNG_STAT,
       LT_DAILY_STAT LIKE TABLE OF LS_DAILY_STAT.
 DATA: LS_SW_SYST TYPE  /SKN/S_SW_SYST.
"--- Days estimation
  " if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_NUM_RNG'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  " time filling and shifting
  SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO.
  TIME_SHIFT SY_DATLO SY_TIMLO.
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  " retrieve the data
  SELECT *
    FROM NRIV
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE OBJECT        IN R_OBJECT
      AND SUBOBJECT     IN R_SUBOBJECT
      AND NRRANGENR     IN R_NRRANGENR
      AND TOYEAR        IN R_TOYEAR
      AND TONUMBER      IN R_TONUMBER
      AND NRLEVEL       IN R_NRLEVEL
      AND FROMNUMBER    IN R_FROMNUMBER
      AND EXTERNIND     IN R_EXTERNIND.
  DATA: TONUMBER_STR      TYPE STRING,
        FROMNUMBER_STR    TYPE STRING,
        NRLEVEL_STR       TYPE STRING,
        NORMALIZEDNRLEV   TYPE NRTO.
  DATA: LV_N(20) TYPE N.
  " data post processing
  LOOP AT T_DATA ASSIGNING <T_DATA_WA_FS>.
    IF <T_DATA_WA_FS>-EXTERNIND IS INITIAL.
      <T_DATA_WA_FS>-NRLEVEL_DEC    = <T_DATA_WA_FS>-NRLEVEL.
      LV_N = <T_DATA_WA_FS>-FROMNUMBER.
       <T_DATA_WA_FS>-FROMNUMBER_DEC   = LV_N.
      LV_N = <T_DATA_WA_FS>-TONUMBER.
      <T_DATA_WA_FS>-TONUMBER_DEC   = LV_N.
      <T_DATA_WA_FS>-AVAILABLE_DEC = <T_DATA_WA_FS>-TONUMBER_DEC - <T_DATA_WA_FS>-FROMNUMBER_DEC + 1.
      IF <T_DATA_WA_FS>-NRLEVEL_DEC > 0.
        <T_DATA_WA_FS>-USED_DEC = <T_DATA_WA_FS>-NRLEVEL_DEC - <T_DATA_WA_FS>-FROMNUMBER_DEC + 1.
        <T_DATA_WA_FS>-LEFT_DEC = <T_DATA_WA_FS>-TONUMBER_DEC - <T_DATA_WA_FS>-NRLEVEL_DEC.
      ELSE.
        CLEAR <T_DATA_WA_FS>-USED_DEC.
        <T_DATA_WA_FS>-LEFT_DEC = <T_DATA_WA_FS>-AVAILABLE_DEC.
      ENDIF.
      IF <T_DATA_WA_FS>-AVAILABLE_DEC > 0.
        LV_F = <T_DATA_WA_FS>-LEFT_DEC / <T_DATA_WA_FS>-AVAILABLE_DEC * 100.
        <T_DATA_WA_FS>-LEFT_PER = LV_F.
        LV_F = <T_DATA_WA_FS>-USED_DEC / <T_DATA_WA_FS>-AVAILABLE_DEC * 100.
        <T_DATA_WA_FS>-USED_PER = LV_F.
      ENDIF.
    ENDIF.
  ENDLOOP.
  " clean-up the data
  DELETE T_DATA WHERE NRLEVEL_DEC    NOT IN R_NRLEVEL_DEC.
  DELETE T_DATA WHERE FROMNUMBER_DEC   NOT IN R_FROMNUMBER_DEC.
  DELETE T_DATA WHERE TONUMBER_DEC  NOT IN R_TONUMBER_DEC.
  DELETE T_DATA WHERE AVAILABLE_DEC NOT IN R_AVAILABLE_DEC.
  DELETE T_DATA WHERE LEFT_DEC NOT IN R_LEFT_DEC.
  DELETE T_DATA WHERE USED_DEC NOT IN R_USED_DEC.
  DELETE T_DATA WHERE LEFT_PER NOT IN R_LEFT_PER.
  DELETE T_DATA WHERE USED_PER NOT IN R_USED_PER.
  LOOP AT T_DATA ASSIGNING <T_DATA_WA_FS>.
    CALL FUNCTION '/SKN/F_SW_01_GET_RANGE_DESC'
      EXPORTING
        OBJECT           = <T_DATA_WA_FS>-OBJECT
        LANGU            = LV_LANGU
      IMPORTING
        TXT              = <T_DATA_WA_FS>-TXT
        TXTSHORT         = <T_DATA_WA_FS>-TXTSHORT
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
  ENDLOOP.
  "--- Left Days Estimation
  IF LV_LEFT_DAYS_ESTIMATION IS NOT INITIAL.
     CALL FUNCTION '/SKN/F_SW_GET_SW_SYST'
       IMPORTING
         SW_SYST         = LS_SW_SYST
       TABLES
         T_SW_SYST       = T_SELECT.
     IF LV_STATISTICS_ALERT IS INITIAL.
       "--- Get myself
       LV_STATISTICS_ALERT = LS_SW_SYST-AN_CODE.
     ENDIF.
     IF LV_STATISTICS_ALERT IS NOT INITIAL.
       LV_STAT_DATE_TO = SY-DATUM.
       LV_STAT_DATE_FROM = LV_STAT_DATE_TO - LV_STATISTICS_DAYS.
       CALL FUNCTION '/SKN/F_SW_01_NUM_RNG_STAT'
         EXPORTING
           AN_CODE                = LV_STATISTICS_ALERT
           SW_CLIENT              = LS_SW_SYST-SW_CLIENT
           DATE_FROM              = LV_STAT_DATE_FROM
           DATE_TO                = LV_STAT_DATE_TO
         TABLES
           T_DAILY_STAT           = LT_DAILY_STAT
         EXCEPTIONS
           EXTRUCTION_ERROR       = 1
           OTHERS                 = 2.
       IF SY-SUBRC = 0.
         SORT LT_DAILY_STAT BY OBJECT SUBOBJECT NRRANGENR TOYEAR.
         LOOP AT T_DATA ASSIGNING <T_DATA_WA_FS>.
           READ TABLE LT_DAILY_STAT INTO LS_DAILY_STAT
                                    WITH KEY OBJECT    = <T_DATA_WA_FS>-OBJECT
                                             SUBOBJECT = <T_DATA_WA_FS>-SUBOBJECT
                                             NRRANGENR = <T_DATA_WA_FS>-NRRANGENR
                                             TOYEAR    = <T_DATA_WA_FS>-TOYEAR
                                    BINARY SEARCH.
           IF SY-SUBRC IS INITIAL.
             <T_DATA_WA_FS>-AVG_DAILY = LS_DAILY_STAT-AVG_DAILY.
             <T_DATA_WA_FS>-AVG_DAILY_NZ = LS_DAILY_STAT-AVG_DAILY_NZ.
             IF LS_DAILY_STAT-AVG_DAILY > 0.
                LV_F = <T_DATA_WA_FS>-LEFT_DEC / LS_DAILY_STAT-AVG_DAILY .
*                CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
*                  <t_data_wa_fs>-LEFT_DAYS = lv_f.
*                ENDCATCH.
*                if sy-subrc <> 0.
*                  <t_data_wa_fs>-LEFT_DAYS = 999999.
*                endif.
                TRY.
                 <T_DATA_WA_FS>-LEFT_DAYS = LV_F.
                CATCH CX_SY_CONVERSION_ERROR.
                 <T_DATA_WA_FS>-LEFT_DAYS = 999999.
                ENDTRY.
             ENDIF.
             IF LS_DAILY_STAT-AVG_DAILY_NZ > 0.
*               CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
*               lv_f = <t_data_wa_fs>-LEFT_DEC / ls_DAILY_STAT-AVG_DAILY_NZ .
*              <t_data_wa_fs>-LEFT_DAYS_NZ = lv_f.
*                ENDCATCH.
*                if sy-subrc <> 0.
*                  <t_data_wa_fs>-LEFT_DAYS_NZ = 999999.
*                endif.
               TRY.
                LV_F = <T_DATA_WA_FS>-LEFT_DEC / LS_DAILY_STAT-AVG_DAILY_NZ .
                <T_DATA_WA_FS>-LEFT_DAYS_NZ = LV_F.
               CATCH CX_SY_CONVERSION_ERROR.
                <T_DATA_WA_FS>-LEFT_DAYS_NZ = 999999.
               ENDTRY.
             ENDIF.
           ENDIF.
         ENDLOOP.
       ENDIF.
       "--- Filtering Stat parameters
       DELETE T_DATA WHERE AVG_DAILY NOT IN R_AVG_DAILY.
       DELETE T_DATA WHERE AVG_DAILY_NZ NOT IN R_AVG_DAILY_NZ.
       DELETE T_DATA WHERE LEFT_DAYS NOT IN R_LEFT_DAYS.
       DELETE T_DATA WHERE LEFT_DAYS_NZ NOT IN R_LEFT_DAYS_NZ.
     ENDIF.
  ENDIF.
  "--- Left Days Estimation
  " check alert information
  READ TABLE T_DATA   INDEX 1.
  CHECK NOT  SY-TFILL IS INITIAL.
  IS_ALERT = 'X'.
ENDFUNCTION.
```
