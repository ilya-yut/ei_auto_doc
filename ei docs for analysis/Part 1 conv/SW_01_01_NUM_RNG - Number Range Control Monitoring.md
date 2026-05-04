# Exception Indicator: Number Range Control Monitoring (SW_01_01_NUM_RNG)


## General Overview

This Exception Indicator (EI) provides comprehensive monitoring and analysis of SAP Number Range objects to ensure business continuity and prevent number range exhaustion scenarios that could disrupt critical business processes. Number ranges are fundamental SAP components that provide unique sequential numbering for business documents, master data records, and transactional objects across all SAP modules.

Number Range objects in SAP control the automatic assignment of unique identifiers for various business entities including document numbers, material numbers, customer numbers, vendor numbers, and other critical business objects. When number ranges approach exhaustion, it can lead to system errors, process interruptions, and potential business disruptions.

This Exception Indicator provides detailed number range monitoring capabilities that enable:

Range utilization analysis to monitor current usage levels and identify ranges approaching exhaustion

Percentage-based monitoring for proactive alerting based on configurable thresholds

Daily consumption tracking to analyze usage patterns and predict future exhaustion dates

Statistical analysis for capacity planning and optimization of number range configurations

Multi-client support for enterprise-wide number range monitoring across distributed SAP landscapes

The monitoring solution analyzes number range data from the NRIV table (Number Range Intervals) and provides enhanced analytical capabilities including usage percentages, remaining capacity calculations, daily consumption statistics, and predictive analytics for proactive number range management.

This Exception Indicator checks whether SAP Number Range objects are functioning efficiently and identifies potential exhaustion scenarios before they impact business operations.


## Problem Description

Number range exhaustion and poor utilization indicate critical system configuration issues causing:

Business Continuity Risks

Number range exhaustion preventing creation of new business documents and master data records

Document posting failures due to unavailable number assignments disrupting business processes

System errors and transaction failures when number ranges reach maximum values

Process interruptions affecting time-critical business operations and compliance requirements

System Performance Issues

Inefficient number range configurations causing performance degradation during number assignment

Fragmented number ranges leading to suboptimal database performance and index efficiency

Memory consumption issues from excessive number range processing workload

Database locking issues during high-volume number assignment operations affecting system responsiveness

Operational Impact

SLA violations from number range-related processing delays impacting service level commitments

Business process interruptions from unreliable number assignment affecting operational continuity

Data consistency problems due to number range conflicts across distributed systems

Compliance issues from inability to create required business documents within regulatory timeframes

Planning and Capacity Issues

Lack of visibility into number range consumption patterns preventing proactive capacity management

Unpredictable exhaustion scenarios causing emergency system maintenance and business disruptions

Inadequate number range sizing leading to frequent range extensions and system interventions

Poor utilization tracking preventing optimization of number range configurations


## Suggested Resolution

Immediate Response

Investigate number ranges with high utilization percentages (>80%) for immediate capacity assessment

Check ranges approaching exhaustion using SNRO transaction for detailed analysis and extension planning

Review daily consumption patterns to identify unusual spikes or trends requiring immediate attention

Analyze external number ranges and custom objects for potential configuration issues

Capacity Assessment

Monitor number range utilization trends and consumption patterns for optimization opportunities

Evaluate current range sizing against business volume projections for adequate capacity planning

Check number range buffering settings and optimization parameters for performance improvement

Analyze number range assignment patterns by object type and business process for efficiency optimization

Corrective Actions

Extend number ranges approaching exhaustion with adequate buffer capacity for continued operations

Optimize number range configurations and buffering settings for improved performance and reliability

Implement proactive number range monitoring and alerting procedures for early issue detection

Plan number range capacity upgrades based on consumption analysis and business growth projections

Preventive Measures

Establish regular number range utilization reviews and capacity planning procedures

Implement automated monitoring for critical number ranges with appropriate alert thresholds

Create documentation and procedures for number range management and emergency extensions

Plan system capacity upgrades based on number range volume analysis and processing requirements


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
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

AVAILABLE_DEC (Available Numbers (Dec Value))

Mirrors how teams reason about SNRO traffic: available numbers (dec value) (AVAILABLE_DEC) is one lever that shapes which intervals are comparable run over run.

AVG_DAILY (P value)

Allows phased rollout: first widen AVG_DAILY for p value, then tighten percentage bands once baseline noise is understood.

AVG_DAILY_NZ (P value)

After reading number range intervals, rows are removed unless p value on AVG_DAILY_NZ still satisfies the active filter criteria.

EXTERNIND (External ('X'))

Connects to alert semantics: lines removed for failing external ('x') on EXTERNIND never reach optional day-estimation enrichment.

FROMNUMBER (From number)

Uses from number from the interval context so only records with FROMNUMBER inside declared values proceed.

FROMNUMBER_DEC (From Number (Dec Value))

Documents expected operator behavior—from number (dec value) on FROMNUMBER_DEC should be set when that dimension is part of the control objective.

LANGU (Languege for description)

When harmonized with remaining-number filters, languege for description on LANGU isolates the highest-risk interval families.

LEFT_DAYS (Days left (generally))

Supports escalation where days left (generally) on LEFT_DAYS signals ownership for follow-up between Basis and functional teams.

LEFT_DAYS_ESTIMATION (X - Estimate the left days)

Enables optional statistical projection that estimates how many days of runway remain based on recent consumption velocity.

LEFT_DAYS_ESTIMATION Options:

·        X — Run historical consumption analysis and populate average daily usage and projected left-day fields when prerequisites are met.

·        Empty or blank — Skip statistical estimation so only direct NRIV utilization fields drive filtering and alerts.

LEFT_DAYS_NZ (Days left (with activities))

Treats days left (with activities) as a discriminator between similar intervals that would otherwise look identical in a raw extract.

LEFT_DEC (Left Numbers)

Helps distinguish fiscal variants when left numbers on LEFT_DEC correlates with year or numbering level attributes.

LEFT_PER (Left Numbers (%))

Captures edge cases where left numbers (%) (LEFT_PER) must be non-default to reproduce a customer-specific monitoring scenario.

NRLEVEL (Current Number)

Uses current number from the interval context so only records with NRLEVEL inside declared values proceed.

NRLEVEL_DEC (Current Number (Dec Value))

Uses current number (dec value) from the interval context so only records with NRLEVEL_DEC inside declared values proceed.

NRRANGENR (Number range number)

Ensures reporting on ranges respects number range number constraints carried by NRRANGENR before any alert fires.

OBJECT (Object name)

Prevents accidental global scans when object name (OBJECT) is meant to stay within a controlled object catalog slice.

STATISTICS_ALERT (Alert Instance (Code) for stat)

Identifies which packaged monitoring statistic to read when estimating consumption; when left initial, the routine may default to the current monitor’s own analysis code if available.

STATISTICS_DAYS (Days for statistics)

Defines how many recent calendar days feed the consumption statistics when left-day estimation is active.

SUBOBJECT (Subobject value)

When harmonized with remaining-number filters, subobject value on SUBOBJECT isolates the highest-risk interval families.

TONUMBER (To number)

Supports escalation where to number on TONUMBER signals ownership for follow-up between Basis and functional teams.

TONUMBER_DEC (To Number (Dec Value))

Guards against oversized extracts when to number (dec value) on TONUMBER_DEC is narrowed together with fiscal year or external-indicators filters.

TOYEAR (To year)

After reading number range intervals, rows are removed unless to year on TOYEAR still satisfies the active filter criteria.

TXT (Long text)

Connects to alert semantics: lines removed for failing long text on TXT never reach optional day-estimation enrichment.

TXTSHORT (Short text)

Helps distinguish fiscal variants when short text on TXTSHORT correlates with year or numbering level attributes.

USED_DEC (Used Numbers)

Interprets used numbers as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on USED_DEC.

USED_PER (Used Numbers (%))

Combines with object and subobject filters so used numbers (%) on USED_PER refines which intervals reach utilization and percentage checks.


### Parameter Relationships

The number range monitoring function provides both percentage-based and absolute

value parameters to accommodate different monitoring scenarios and business requirements:

Percentage vs. Absolute Value Parameters

Percentage Parameters (_PER):

·       LEFT_PER - Remaining capacity as percentage of total range

·       USED_PER - Utilized capacity as percentage of total range

Absolute Value

Parameters (_DEC):

·      AVAILABLE_DEC - Total numbers available in the range

·      LEFT_DEC - Absolute count of remaining numbers

·      USED_DEC - Absolute count of consumed numbers

·      NRLEVEL_DEC - Current number level in decimal format

Why Both Options Exist:

The dual parameter approach provides monitoring flexibility for different business contexts. Percentage-based thresholds work well for ranges of varying sizes (e.g., alert when any range drops below 20% capacity), while absolute value thresholds are essential for business-critical ranges where specific minimum quantities must be maintained (e.g., always keep at least 1,000 invoice numbers available regardless of total range size).

Statistical Analysis and Forecasting Parameters

Statistics Configuration:

·       STATISTICS_DAYS - Defines the historical period (in days) used to calculate average daily consumption patterns

·       LEFT_DAYS_ESTIMATION - Boolean flag enabling/disabling predictive

analytics based on consumption trends

Calculated Forecasting  Fields:

·   LEFT_DAYS - Estimated days until range exhaustion based on historical average daily usage

·   LEFT_DAYS_NZ - Estimated days until exhaustion using non-zero daily averages only

(excludes days with no consumption)

·      AVG_DAILY - Average daily consumption including zero-usage days

·       AVG_DAILY_NZ - Average daily consumption excluding zero-usage days

This statistical approach enables proactive capacity management by combining current utilization levels with historical consumption patterns to predict when intervention will be required.


### Default Values

·        STATISTICS_DAYS - 30 from the preset before the statistics read when the caller does not override it.


### Practical Example of Parameter Configuration

Use Case 1: Material numbering with runway projection

Purpose: Monitor a specific material number range with two-week consumption statistics and estimation enabled.

OBJECT = MATERIAL
 SUBOBJECT = *
 NRRANGENR = 01
 TOYEAR = 2025
 LEFT_DAYS_ESTIMATION = X
 STATISTICS_DAYS = 14



Use Case 2: Utilization percentage alert band

Purpose: Flag intervals that are at least three-quarters consumed while a meaningful absolute remainder still exists.

USED_PER = 75
 LEFT_DEC = 500
 AVAILABLE_DEC = 2000



Use Case 3: Statistics source and language

Purpose: Keep a thirty-day statistics window with an explicit analysis code and English descriptions.

LEFT_DAYS_ESTIMATION = X
 STATISTICS_ALERT = NUMRNG01
 STATISTICS_DAYS = 30
 LANGU = E




## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
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
