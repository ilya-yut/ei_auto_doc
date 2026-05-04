# Exception Indicator: Vendor Payment Terms comparison - SW_10_06_VND_P_TRM_C

## General Overview

This Exception Indicator (EI) monitors vendor master data to identify vendors whose payment terms at the purchasing organization level differ from the payment terms at the company code level. It compares payment terms configured in the vendor–purchasing organization view with those in the vendor–company code view, flagging inconsistencies that may cause payment timing mismatches, reconciliation issues, or audit findings.

This EI serves as an essential control for procurement, treasury, and accounts payable by:
- Enabling detection of payment term inconsistencies between purchasing organization and company code that can lead to incorrect due dates or cash flow misalignment
- Supporting identification of vendors requiring payment term harmonization for process standardization and audit readiness
- Providing visibility into which vendors have divergent terms for prioritization and corrective action
- Supporting reconciliation between procurement agreements and payment execution parameters
- Enabling follow-up by company code, purchasing organization, or vendor for targeted remediation

The EI helps organizations maintain consistent payment terms across vendor master views and supports month-end controls, vendor master reviews, and audit compliance. Data is sourced from vendor master tables (purchasing organization and company code views) and purchasing organization–company code assignments.


## Problem Description

Failure to monitor vendor payment term consistency between purchasing organization and company code creates multiple risks across financial reporting, operational control, and compliance:

**Financial and Reporting Issues**
- Unidentified payment term differences can cause incorrect due date calculations and cash flow forecasting errors
- Inconsistent terms between purchasing organization and company code may lead to early or late payments, affecting liquidity and vendor relationships
- Lack of visibility into term mismatches can delay month-end close when discovered during reconciliation
- Divergent terms may distort aging reports and payment prioritization

**Operational and Control Risks**
- Vendors with different terms at purchasing org vs. company code may indicate incomplete or erroneous master data maintenance
- Absence of monitoring allows payment term inconsistencies to persist and multiply across the vendor base
- Inability to filter by company code, purchasing organization, or vendor limits targeted review of high-risk areas
- Missing time-window controls restricts analysis to recently created or recently changed vendor records

**Management Visibility and Decision-Making Risks**
- Lack of consolidated visibility delays management awareness of payment term inconsistencies
- Unidentified mismatches reduce confidence in vendor master data quality and payment process controls
- Missing visibility by organizational dimension limits accountability and corrective action
- Absence of configurable lookback and duration thresholds restricts risk-based prioritization

## Suggested Resolution

**Immediate Response**
- Review the flagged vendors to confirm the payment term difference between purchasing organization and company code and assess business impact
- Verify the intended terms using vendor master display (e.g. XK03 for company code, MK03 for purchasing organization) to determine which view is correct
- Check whether the difference is intentional (e.g. special terms for a company code) or an error requiring correction
- Identify the business context: new vendor setup, recent changes, or long-standing inconsistency

**System Assessment**
- Analyze the time window and date reference used for the run to ensure the lookback period aligns with the monitoring objective
- Compare term differences across company codes and purchasing organizations to identify process or control gaps
- Review duration threshold settings so that only meaningful cases (e.g. recent creations or changes) are flagged
- Assess vendor distribution by organizational dimension for prioritization

**Corrective Actions**
- If erroneous terms are confirmed, update vendor master data (XK02 for company code, MK02 for purchasing organization) to align payment terms
- Escalate intentional differences for documentation and approval where policy requires alignment
- Establish recurring EI execution to maintain ongoing visibility into payment term consistency
- Adjust parameters (lookback, duration, company code, purchasing organization, vendor) and schedule runs for continuous monitoring
- Document findings and remediation for audit trail and management reporting


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 4 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 5 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 6 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 7 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 8 | ERDAT | Created on | DATS | 8 | 0 | ERDAT_RF | DATUM |
| 9 | LANGU | Language |  | 0 | 0 |  |  |
| 10 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 11 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 12 | VTEXT_BKRS | Description | CHAR | 30 | 0 | DZTERM_BEZ | TEXT30 |
| 13 | VTEXT_EKRG | Description | CHAR | 30 | 0 | DZTERM_BEZ | TEXT30 |
| 14 | ZTERM_BKRS | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
| 15 | ZTERM_EKRG | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 15 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Back days):

Number of days to look back from today to form the monitoring window when no date range is supplied. The EI uses this value to compute the start of the period for vendor records (e.g. today minus BACKDAYS) based on the creation date. When BACKDAYS is not supplied and no date range is given, no date filter is applied.

**BUKRS** (Company Code):

Company code. Scopes the EI to vendors with purchasing organization–company code assignments in the specified company codes. The EI compares payment terms at purchasing organization level with those at company code level; this parameter restricts which company codes are included.

**BUTXT** (Company Name):

Company code name. Resolved from the company code master when the EI builds the result; provides the company name for each result row.

**DURATION** (Duration In Time Units):

Duration value used together with DURATION_UNIT to express the time difference between the vendor record creation date and today. The EI calculates this difference and only outputs records whose duration falls within the supplied range. Use to focus on recently created vendors (e.g. within 30 days) or exclude very old records.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is interpreted (hours, minutes, days, or full days for day-level filtering). The EI uses this with DURATION when calculating and evaluating the time difference between creation date and today.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** Use DURATION and DURATION_UNIT together when defining the time window (e.g. DURATION = 0 - 30 with DURATION_UNIT = D for vendors created within the last 30 days).

**EKORG** (Purch. Organization):

Purchasing organization. Scopes the EI to vendors assigned to the specified purchasing organizations. The EI compares payment terms at this level with those at company code level.

**EKOTX** (Description):

Description of the purchasing organization. Resolved from the purchasing organization master for each result row.

**ERDAT** (Created on):

Vendor record creation date. When supplied, the EI restricts results to vendors whose creation date falls within the given range. When no range is supplied and BACKDAYS is set, the EI uses today minus BACKDAYS as the start date.

**LANGU** (Language):

Language key for descriptions. Used when the EI resolves payment term and organizational descriptions; default in code is system language when not supplied.

**LIFNR** (Vendor):

Vendor number. Scopes the EI to the specified vendors. The EI compares payment terms for each vendor between purchasing organization and company code views.

**NAME1** (Name):

Vendor name. Resolved from the vendor master when the EI builds the result.

**VTEXT_BKRS** (Description):

Description of the payment terms at company code level. Resolved from the terms of payment master.

**VTEXT_EKRG** (Description):

Description of the payment terms at purchasing organization level. Resolved from the terms of payment master.

**ZTERM_BKRS** (Terms of Payment):

Payment terms key at company code level. Populated from the vendor–company code view in the EI result; one of the two terms compared by the EI.

**ZTERM_EKRG** (Terms of Payment):

Payment terms key at purchasing organization level. Populated from the vendor–purchasing organization view in the EI result; one of the two terms compared by the EI. The EI flags records where ZTERM_EKRG differs from ZTERM_BKRS.


### Parameter Relationships

**Time and Lookback Parameters:**

- **BACKDAYS** and **ERDAT** work together to define the monitoring window: when no date range is supplied for ERDAT, the EI uses today minus BACKDAYS as the start date and includes vendor records whose creation date (ERDAT) falls in that window. Set BACKDAYS to control how far back to look (e.g. 30 for the last 30 days).
- **DURATION** and **DURATION_UNIT** work together to filter by the calculated time difference between the vendor creation date and today. The EI computes this difference using DURATION_UNIT (e.g. days) and only outputs records whose duration is within the DURATION range. Use DURATION = 0 - 30 with DURATION_UNIT = D to focus on vendors created within the last 30 days.

**Organizational Scope Parameters:**

- **BUKRS**, **EKORG**, and **LIFNR** define the organizational and vendor scope. Together they restrict which company codes, purchasing organizations, and vendors are included in the comparison. The EI joins vendor master (purchasing org and company code views) with purchasing organization–company code assignments; these parameters narrow that scope.


### Default Values

- **DURATION_UNIT** — Default: `D` (days).
- **LANGU** — Default: system language when not supplied.

**Note:** When no date range is supplied for ERDAT and BACKDAYS is not set, no date filter is applied. When BACKDAYS is set, the EI uses today minus BACKDAYS as the start date for the creation date window.

### Practical Configuration Examples

**Use Case 1: Payment term mismatches in last 30 days**
```
BACKDAYS = 30
DURATION = 0 - 90
DURATION_UNIT = D
```
**Purpose:** Identify vendors created or changed in the last 30 days whose payment terms differ between purchasing organization and company code, with duration (days since creation) between 0 and 90 for focus on recent records.

**Use Case 2: Specific company code and purchasing organization**
```
BUKRS = 1000
EKORG = 1000
LIFNR = 1000000 - 1999999
```
**Purpose:** Narrow to a specific company code and purchasing organization, with a vendor number range, for targeted review of payment term consistency.

**Use Case 3: Full-day window for specific day filtering**
```
DURATION = 30
DURATION_UNIT = F
ERDAT = 20240101 - 20240131
BUKRS = 1000; 2000
```
**Purpose:** Use full days (DURATION_UNIT = F) with a single DURATION value (30) and a fixed ERDAT range to evaluate vendors created in a specific month, scoped to selected company codes.

**Use Case 4: Multi-dimensional scope**
```
BACKDAYS = 60
DURATION = 0 - 60
DURATION_UNIT = D
EKORG = 1000; 2000
BUKRS = 1000 - 2999
```
**Purpose:** Look back 60 days, restrict duration to 0–60 days since creation, and scope by purchasing organizations and company code range for a broad payment term consistency review.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | ERDAT | Date on which the Record Was Created | DATS(8) | ERDAT_RF |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | VTEXT_BKRS | Description of terms of payment | CHAR(30) | DZTERM_BEZ |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | VTEXT_EKRG | Description of terms of payment | CHAR(30) | DZTERM_BEZ |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | ZTERM_BKRS | Terms of Payment Key | CHAR(4) | DZTERM |
| /SKN/S_SW_10_06_VEND_ZTERM_CHK | ZTERM_EKRG | Terms of Payment Key | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_VEND_TERM_CHK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_VEND_ZTERM_CHK
*"----------------------------------------------------------------------
  INCLUDE /SKN/PC_SQL_DATA.
  DATA_SINGLE: SW_DEST   RFCDEST,
               LANGU     LANGU,
               BACKDAYS  INT4,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  DATA_MULTY: LIFNR LIFNR,
              EKORG EKORG,
              LAND1 LAND1,
              BUKRS BUKRS,
              ERDAT ERDAT,
              DURATION    /SKN/E_SW_DURATION.
  SELECT_MULTY: LIFNR,
                EKORG,
                LAND1,
                BUKRS,
                ERDAT,
                DURATION.
  "lv_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  SELECT_SINGLE: SW_DEST, LANGU, BACKDAYS, DURATION_UNIT.
  CONVERT_MULTY: LIFNR ALPHA.
  DATA : LS_DATA LIKE LINE OF T_DATA,
         LT_DATA LIKE TABLE OF LS_DATA.
  DATA : LV_TIME_DIFF TYPE  INT4 .
  DATA : SY_TABIX LIKE SY-TABIX.
  DATA : DATE_FROM LIKE SY-DATUM .
  DATA : SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
  FIELD-SYMBOLS: <FS_DATA> TYPE /SKN/S_SW_10_06_VEND_ZTERM_CHK.
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_VEND_TERM_CHK'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
  IF R_ERDAT[] IS INITIAL .
    IF LV_BACKDAYS IS NOT INITIAL.
      RS_ERDAT-SIGN   = 'I' .
      RS_ERDAT-OPTION = 'GE' .
      DATE_FROM       = SY-DATUM - LV_BACKDAYS .
      RS_ERDAT-LOW    = DATE_FROM .
      APPEND RS_ERDAT TO R_ERDAT.
    ENDIF.
  ENDIF.
  SELECT LFM1~LIFNR LFM1~EKORG LFM1~ZTERM AS ZTERM_EKORG
         LFA1~NAME1 LFA1~LAND1 LFA1~KTOKK LFA1~ERDAT
         LFB1~ZTERM AS ZTERM_BUKRS
         T024E~EKOTX T024E~BUKRS
         T001~BUTXT
    FROM LFM1 INNER JOIN LFA1  ON LFA1~LIFNR  EQ LFM1~LIFNR
              INNER JOIN LFB1  ON LFB1~LIFNR  EQ LFM1~LIFNR
              INNER JOIN T024E ON T024E~EKORG EQ LFM1~EKORG
              INNER JOIN T001  ON T001~BUKRS  EQ T024E~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE LFM1~LIFNR  IN R_LIFNR
    AND   LFM1~EKORG  IN R_EKORG
    AND   LFA1~LAND1  IN R_LAND1
    AND   LFA1~ERDAT  IN R_ERDAT
    AND   T024E~BUKRS IN R_BUKRS
    AND   T024E~BUKRS EQ LFB1~BUKRS
    AND   LFM1~ZTERM  NE LFB1~ZTERM.
  CHECK T_DATA IS NOT INITIAL.
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    <FS_DATA>-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = <FS_DATA>-ERDAT
        T_FROM      = SY-TIMLO
        D_TO        = SY-DATLO
        T_TO        = SY-TIMLO
        TIME_UNIT   = LV_DURATION_UNIT   "'D'
      IMPORTING
        TIME_DIFF   = LV_TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      <FS_DATA>-DURATION  = LV_TIME_DIFF .
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    IF <FS_DATA>-ZTERM_EKRG IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_ZTERM_DESC'
        EXPORTING
          ZTERM      = <FS_DATA>-ZTERM_EKRG
          LANGU      = LV_LANGU
        IMPORTING
          ZTERM_DESC = <FS_DATA>-VTEXT_EKRG
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF <FS_DATA>-ZTERM_BKRS IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_ZTERM_DESC'
        EXPORTING
          ZTERM      = <FS_DATA>-ZTERM_BKRS
          LANGU      = LV_LANGU
        IMPORTING
          ZTERM_DESC = <FS_DATA>-VTEXT_BKRS
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
