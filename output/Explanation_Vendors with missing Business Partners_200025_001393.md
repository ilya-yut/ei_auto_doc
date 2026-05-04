# Exception Indicator: Check Vendor's Partners - SW_10_06_PF_VENDOR

## General Overview

This Exception Indicator (EI) monitors vendor master data to identify vendors that are missing required business partner assignments for specified partner functions. It compares vendor–purchasing organization (and plant-level) records against a configurable list of partner functions and flags vendors that do not have a partner assigned for one or more of those functions.

This EI serves as an essential control for procurement, vendor management, and master data quality by:
- Enabling detection of vendors without required partner assignments (e.g. ordering party, invoicing party) that can block or delay procurement and payment processes
- Supporting identification of incomplete vendor master data for remediation and process standardization
- Providing visibility into which partner functions are missing by vendor, purchasing organization, and company code for targeted follow-up
- Supporting audit readiness and compliance with partner function requirements
- Enabling prioritization by organizational dimension (company code, purchasing organization, country, account group) for risk-based remediation

The EI helps organizations maintain complete vendor partner assignments and supports vendor master reviews, procurement process controls, and audit compliance. Data is sourced from vendor master, purchasing organization assignments, and partner function configuration tables.


## Problem Description

Failure to monitor vendors for missing business partner assignments creates multiple risks across procurement, financial reporting, and compliance:

**Financial and Reporting Issues**
- Vendors without required partner assignments (e.g. invoicing party) may cause payment delays, incorrect payment routing, or failed automatic processing
- Incomplete partner data can distort vendor analytics and concentration reporting
- Lack of visibility into missing partners may delay month-end close when discovered during reconciliation
- Missing partner functions can block invoice verification or three-way match processes

**Operational and Control Risks**
- Vendors with missing partner assignments may indicate incomplete or erroneous master data maintenance during vendor creation or extension
- Absence of monitoring allows incomplete partner data to persist and multiply across the vendor base
- Inability to filter by partner function, company code, purchasing organization, or vendor limits targeted review of high-risk areas
- Missing time-window controls restricts analysis to recently created vendor records

**Management Visibility and Decision-Making Risks**
- Lack of consolidated visibility delays management awareness of incomplete vendor partner data
- Unidentified missing partners reduce confidence in vendor master data quality and procurement process controls
- Missing visibility by organizational dimension limits accountability and corrective action
- Absence of configurable lookback and duration thresholds restricts risk-based prioritization

## Suggested Resolution

**Immediate Response**
- Review the flagged vendors to confirm which partner functions are missing and assess process impact
- Verify the intended partner assignments using vendor master display (e.g. XK03, MK03) to determine whether partners should be maintained
- Check whether the missing partner is intentional (e.g. not applicable for the vendor type) or an error requiring correction
- Identify the business context: new vendor setup, recent extension to new purchasing organization, or long-standing gap

**System Assessment**
- Analyze the time window and date reference used for the run to ensure the lookback period aligns with the monitoring objective
- Compare missing-partner patterns across company codes, purchasing organizations, and partner functions to identify process or control gaps
- Review which partner functions are configured for the check and whether the list is complete for your process requirements
- Assess vendor distribution by organizational dimension for prioritization

**Corrective Actions**
- If missing partners are confirmed as errors, update vendor master data (XK02, MK02) to assign the required partners
- Document intentional exceptions where partner functions are not applicable
- Establish recurring EI execution to maintain ongoing visibility into vendor partner completeness
- Adjust parameters (lookback, partner function list, company code, purchasing organization, vendor) and schedule runs for continuous monitoring
- Document findings and remediation for audit trail and management reporting


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 4 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 5 | DURATION_D | Duration In Days |  | 0 | 0 |  |  |
| 6 | DURATION_UNIT | Duration Unit |  | 0 | 0 |  |  |
| 7 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 8 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 9 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 10 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 11 | LANGU | Language |  | 0 | 0 |  |  |
| 12 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 13 | LTSNR | Vendor Subrange | CHAR | 6 | 0 | LTSNR | LTSNR |
| 14 | NAME1 | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 15 | PARVW | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 16 | VTEXT | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 17 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 17 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Backdays):

Number of days to look back from today to form the start of the creation date window when no date range is supplied. The EI uses this value together with DURATION to compute the vendor creation date range (e.g. from today minus BACKDAYS to today minus DURATION). When not supplied and no date range is given, no date filter is applied.

**BUKRS** (Company Code):

Company code. Scopes the EI to vendors with purchasing organization–company code assignments in the specified company codes. The EI checks partner assignments for vendors in these company codes.

**BUTXT** (Company Name):

Company code name. Resolved from the company code master for each result row.

**DURATION** (Duration In Time Units):

Number of days from today that defines the end of the creation date window when no date range is supplied. The EI uses BACKDAYS and DURATION together to form the range (from today minus BACKDAYS to today minus DURATION) for filtering vendors by creation date.

**DURATION_D** (Duration In Days):

Duration expressed in days. Alternative to DURATION for day-based date window or time calculations when the EI supports it.

**DURATION_UNIT** (Duration Unit):

Unit in which duration values are interpreted (hours, minutes, days, or full days for day-level filtering). Applied when the EI evaluates time-based logic.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**BACKDAYS and DURATION Connection:** Use BACKDAYS and DURATION together to define the creation date window (e.g. BACKDAYS = 365, DURATION = 0 for vendors created in the last year).

**EKORG** (Purch. Organization):

Purchasing organization. Scopes the EI to vendors assigned to the specified purchasing organizations. The EI checks partner assignments at purchasing organization (and plant) level.

**EKOTX** (Description):

Description of the purchasing organization. Resolved from the purchasing organization master for each result row.

**KTOKK** (Account group):

Vendor account group. Scopes the EI to vendors in the specified account groups. Supports analysis by vendor type.

**LAND1** (Country Key):

Country key. Scopes the EI to vendors with the specified country in the address data.

**LANGU** (Language):

Language key for descriptions. Used when the EI resolves partner function and organizational descriptions; default in code is system language when not supplied.

**LIFNR** (Vendor):

Vendor number. Scopes the EI to the specified vendors. The EI checks whether each vendor has the required partner assignments for the partner functions in PARVW.

**LTSNR** (Vendor Subrange):

Vendor subrange (subrange number). Scopes the EI to specific vendor subranges when plant-level or subrange-level partner assignments are checked.

**NAME1** (Name 1):

Vendor name. Resolved from the vendor master for each result row.

**PARVW** (Partner Function):

Partner function code(s) to check. The EI compares each vendor’s assigned partners against this list and outputs vendors that are missing one or more of these partner functions. At least one partner function must be supplied for the EI to run.

**PARVW Options:**
- Values are partner function codes (e.g. SP for ordering party, RE for invoicing party). See partner function configuration for the full list applicable to vendors.

**VTEXT** (Name):

Partner function description. Resolved from the partner function master for each result row; identifies which partner function is missing for the vendor.

**WERKS** (Plant):

Plant. Scopes the EI to specific plants when plant-level partner assignments are checked. The EI evaluates both vendor–purchasing organization level and vendor–plant level records.


### Parameter Relationships

**Time and Creation Date Parameters:**

- **BACKDAYS** and **DURATION** work together to define the creation date window: when no date range is supplied, the EI uses today minus BACKDAYS as the start date and today minus DURATION as the end date. Vendors whose creation date falls in this range are included. Set both to control the lookback (e.g. BACKDAYS = 365, DURATION = 0 for the last year).

**Partner Function and Output Parameters:**

- **PARVW** defines which partner functions the EI checks. The EI compares each vendor’s assigned partners against the PARVW list; vendors missing one or more of these partner functions are output. **VTEXT** holds the description of the missing partner function for each result row. At least one partner function must be supplied in PARVW for the EI to run.

**Organizational Scope Parameters:**

- **BUKRS**, **EKORG**, **LIFNR**, **LAND1**, and **KTOKK** define the organizational and vendor scope. Together they restrict which company codes, purchasing organizations, vendors, countries, and account groups are included in the partner completeness check. The EI joins vendor master with partner assignment data; these parameters narrow that scope.


### Default Values

- **LANGU** — Default: system language when not supplied.

**Note:** When no date range is supplied and BACKDAYS is not set, no date filter is applied. When both BACKDAYS and DURATION are set, the EI uses today minus BACKDAYS to today minus DURATION as the creation date window.

### Practical Configuration Examples

**Use Case 1: Missing ordering and invoicing partners in last year**
```
BACKDAYS = 365
DURATION = 0
PARVW = SP; RE
EKORG = 1000
BUKRS = 1000
```
**Purpose:** Identify vendors created in the last year that are missing ordering party (SP) or invoicing party (RE) partners, scoped to a specific purchasing organization and company code.

**Use Case 2: Multi-dimensional scope**
```
BACKDAYS = 180
DURATION = 0
PARVW = SP; RE; ZP
EKORG = 1000; 2000
BUKRS = 1000 - 2999
LAND1 = DE; AT; CH
KTOKK = LIEF; KRED
```
**Purpose:** Check ordering, invoicing, and a custom partner function across multiple purchasing organizations, company codes, and countries for vendors in specified account groups.

**Use Case 3: Full-day window for specific period**
```
DURATION_UNIT = F
DURATION = 90
PARVW = SP
BUKRS = 1000
LIFNR = 1000000 - 1999999
```
**Purpose:** Use full days (DURATION_UNIT = F) with a single DURATION value to focus on vendors in a specific creation window, checking only ordering party (SP) for a vendor number range.

**Use Case 4: Targeted vendor review**
```
BACKDAYS = 30
DURATION = 0
PARVW = RE
LIFNR = 5000000; 5000001; 5000002
EKORG = 1000
```
**Purpose:** Check specific vendors for missing invoicing party (RE) in the last 30 days, for focused remediation.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_PF_VENDOR | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_PF_VENDOR | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_PF_VENDOR | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_06_PF_VENDOR | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_06_PF_VENDOR | KTOKK | Vendor account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_06_PF_VENDOR | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_PF_VENDOR | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_PF_VENDOR | LTSNR | Vendor Subrange | CHAR(6) | LTSNR |
| /SKN/S_SW_10_06_PF_VENDOR | NAME1 | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_06_PF_VENDOR | PARVW | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_06_PF_VENDOR | VTEXT | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_PF_VENDOR | WERKS | Plant | CHAR(4) | WERKS_D |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_PF_VENDOR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_PF_VENDOR
*"----------------------------------------------------------------------
  DATA_SINGLE: SW_DEST   RFCDEST,
               LANGU  LANGU,
               BACKDAYS  INT4,
               DURATION  INT4.
  DATA_MULTY: PARVW PARVW,
              LIFNR LIFNR,
              EKORG EKORG,
              LAND1 LAND1,
              KTOKK KTOKK,
              BUKRS BUKRS,
              ERDAT ERDAT.
  SELECT_MULTY: PARVW,
                LIFNR,
                EKORG,
                LAND1,
                KTOKK,
                BUKRS,
                ERDAT.
  SELECT_SINGLE: SW_DEST, LANGU, BACKDAYS, DURATION.
  CONVERT_MULTY: LIFNR ALPHA.
  CONVERT_MULTY:  PARVW PARVW.
  DATA: LS_ERDAT LIKE LINE OF R_ERDAT.
  IF R_ERDAT[] IS INITIAL AND LV_BACKDAYS IS NOT INITIAL.
    LS_ERDAT-SIGN  = 'I'.
    LS_ERDAT-OPTION = 'BT'.
    LS_ERDAT-LOW = SY-DATUM - LV_BACKDAYS.
    LS_ERDAT-HIGH = SY-DATUM - LV_DURATION.
    APPEND LS_ERDAT TO R_ERDAT.
  ENDIF.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_PF_VENDOR'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  IF LV_LANGU IS INITIAL.
    LV_LANGU = SY-LANGU.
  ENDIF.
  CHECK R_PARVW[] IS NOT INITIAL.
  TYPES: BEGIN OF TY_PARTNER,
      PARVW TYPE TPAUM-PARVW,
      PABEZ TYPE TPAUM-PABEZ,
      VTEXT TYPE TPART-VTEXT,
    END OF TY_PARTNER,
    TT_PARTNER TYPE STANDARD TABLE OF TY_PARTNER.
  DATA: LT_PARTNER   TYPE TT_PARTNER,
        LS_PARTNER   TYPE TY_PARTNER,
        LT_PARTNER_T TYPE TT_PARTNER,
        LT_TPART     TYPE STANDARD TABLE OF TPART,
        LS_TPART     TYPE TPART,
        LT_DATA      TYPE STANDARD TABLE OF /SKN/S_SW_10_06_PF_VENDOR,
        LT_DATA2     TYPE STANDARD TABLE OF /SKN/S_SW_10_06_PF_VENDOR,
        LS_DATA      TYPE /SKN/S_SW_10_06_PF_VENDOR,
        LS_PREV      TYPE /SKN/S_SW_10_06_PF_VENDOR,
        LS_PARVW     LIKE LINE OF R_PARVW.
  FIELD-SYMBOLS: <LS_PARTNER> TYPE TY_PARTNER.
  SELECT TPAUM~PARVW TPAUM~PABEZ
    FROM TPAUM
    INTO TABLE LT_PARTNER
    WHERE TPAUM~SPRAS EQ LV_LANGU
    AND   TPAUM~PABEZ IN R_PARVW.
  LOOP AT R_PARVW INTO LS_PARVW.
    READ TABLE LT_PARTNER WITH KEY PABEZ = LS_PARVW-LOW
    TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      LS_PARTNER-PABEZ = LS_PARVW-LOW.
      LS_PARTNER-PARVW = LS_PARVW-LOW.
      APPEND LS_PARTNER TO LT_PARTNER.
    ENDIF.
  ENDLOOP.
  IF LT_PARTNER[] IS NOT INITIAL.
    SELECT PARVW VTEXT FROM TPART
      INTO CORRESPONDING FIELDS OF TABLE LT_TPART
      FOR ALL ENTRIES IN LT_PARTNER
      WHERE SPRAS EQ LV_LANGU
      AND   PARVW EQ LT_PARTNER-PARVW.
    SORT LT_TPART BY PARVW.
    LOOP AT LT_PARTNER ASSIGNING <LS_PARTNER>.
      READ TABLE LT_TPART INTO LS_TPART
      WITH KEY PARVW = <LS_PARTNER>-PARVW BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <LS_PARTNER>-VTEXT = LS_TPART-VTEXT.
      ENDIF.
    ENDLOOP.
    SORT LT_PARTNER BY PARVW.
  ENDIF.
  CHECK LT_PARTNER[] IS NOT INITIAL.
  SELECT LFM1~LIFNR LFM1~EKORG WYT3~LTSNR WYT3~WERKS
    WYT3~PARVW
    LFA1~NAME1 LFA1~LAND1 LFA1~KTOKK
    T024E~EKOTX T024E~BUKRS T001~BUTXT
    FROM LFM1
    LEFT OUTER JOIN WYT3 ON WYT3~LIFNR  EQ LFM1~LIFNR
                        AND WYT3~EKORG  EQ LFM1~EKORG
    INNER JOIN LFA1      ON LFA1~LIFNR  EQ LFM1~LIFNR
    INNER JOIN T024E     ON T024E~EKORG EQ LFM1~EKORG
    LEFT OUTER JOIN T001 ON T001~BUKRS  EQ T024E~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE LFM1~LIFNR IN R_LIFNR
    AND   LFM1~EKORG IN R_EKORG
    AND   LFA1~LAND1 IN R_LAND1
    AND   LFA1~KTOKK IN R_KTOKK
    AND   LFA1~ERDAT IN R_ERDAT
    AND   T024E~BUKRS IN R_BUKRS.
  DELETE LT_DATA WHERE LTSNR IS NOT INITIAL OR WERKS IS NOT INITIAL.
  SELECT LFM2~LIFNR LFM2~EKORG LFM2~LTSNR LFM2~WERKS
    WYT3~PARVW
    LFA1~NAME1 LFA1~LAND1 LFA1~KTOKK
    T024E~EKOTX T024E~BUKRS T001~BUTXT
    FROM LFM2
    LEFT OUTER JOIN WYT3 ON WYT3~LIFNR  EQ LFM2~LIFNR
                        AND WYT3~EKORG  EQ LFM2~EKORG
                        AND WYT3~LTSNR  EQ LFM2~LTSNR
                        AND WYT3~WERKS  EQ LFM2~WERKS
    INNER JOIN LFA1      ON LFA1~LIFNR  EQ LFM2~LIFNR
    INNER JOIN T024E     ON T024E~EKORG EQ LFM2~EKORG
    LEFT OUTER JOIN T001 ON T001~BUKRS  EQ T024E~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA2
    WHERE LFM2~LIFNR IN R_LIFNR
    AND   LFM2~EKORG IN R_EKORG
    AND   LFA1~LAND1 IN R_LAND1
    AND   LFA1~KTOKK IN R_KTOKK
    AND   LFA1~ERDAT IN R_ERDAT
    AND   T024E~BUKRS IN R_BUKRS.
  APPEND LINES OF LT_DATA2 TO LT_DATA.
  SORT LT_DATA BY LIFNR EKORG LTSNR WERKS PARVW.
  CHECK LT_DATA IS NOT INITIAL.
  READ TABLE LT_DATA INTO LS_PREV INDEX 1.
  LT_PARTNER_T[] = LT_PARTNER[].
  LOOP AT LT_DATA INTO LS_DATA.
    IF LS_PREV-LIFNR NE LS_DATA-LIFNR
    OR LS_PREV-EKORG NE LS_DATA-EKORG
    OR LS_PREV-LTSNR NE LS_DATA-LTSNR
    OR LS_PREV-WERKS NE LS_DATA-WERKS.
      LOOP AT LT_PARTNER_T INTO LS_PARTNER.
        LS_PREV-PARVW = LS_PARTNER-PARVW.
        LS_PREV-VTEXT = LS_PARTNER-VTEXT.
        APPEND LS_PREV TO T_DATA.
      ENDLOOP.
      LT_PARTNER_T[] = LT_PARTNER[].
    ENDIF.
    DELETE LT_PARTNER_T WHERE PARVW EQ LS_DATA-PARVW.
    LS_PREV = LS_DATA.
  ENDLOOP.
  LOOP AT LT_PARTNER_T INTO LS_PARTNER.
    LS_PREV-PARVW = LS_PARTNER-PARVW.
    LS_PREV-VTEXT = LS_PARTNER-VTEXT.
    APPEND LS_PREV TO T_DATA.
  ENDLOOP.
  IF T_DATA[] IS NOT INITIAL.
    IS_ALERT = ABAP_TRUE.
  ENDIF.
ENDFUNCTION.
```
