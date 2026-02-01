# Exception Indicator: Bank account country differs from Vendor's country - SW_10_06_BNK_VND_COM

## General Overview

This Exception Indicator (EI) monitors vendor bank account data in SAP to identify cases where the bank account country differs from the vendor's country. It compares bank country (LFBK-BANKS) with vendor country (LFA1-LAND1) and returns vendor bank records where they do not match, enabling detection of potential payment or compliance issues.

This EI serves as an essential control for procurement, treasury, and compliance by:
- Enabling detection of vendor bank accounts whose country does not match the vendor's country for payment and anti–money laundering review
- Supporting identification of records that may require correction or approval before payment
- Providing visibility into vendor bank master data (LFBK) and vendor master (LFA1) for audit and reconciliation
- Enabling filtering by vendor, bank country, vendor country, and date range so monitoring can be focused on relevant periods and entities
- Supporting compliance and treasury oversight by flagging country mismatches that could affect payment execution or reporting

This monitoring enables organizations to detect and correct vendor bank country mismatches, support payment controls, and maintain alignment between vendor and bank master data. The EI is particularly valuable for payment readiness, vendor master governance, and compliance reviews.

The EI uses vendor bank data (LFBK) and vendor master (LFA1) and filters by the join condition that bank country differs from vendor country.


## Problem Description

Failure to monitor vendor bank accounts where the bank country differs from the vendor's country creates multiple risks across payment execution, compliance, and master data governance:

**Payment and Treasury Issues**
- Unmonitored country mismatches can lead to payment rejections, delays, or incorrect routing when bank country and vendor country are inconsistent
- Late discovery of mismatched bank data may require corrective payments or vendor master updates during period-end or audit
- Lack of visibility into vendor–bank country alignment can undermine payment controls and treasury reconciliation
- Records outside the monitored date range or selection criteria may be missed, leaving exceptions unreviewed

**Compliance and Control Risks**
- Unreviewed vendor bank records with country mismatches can mask anti–money laundering or sanctions screening gaps
- Missing or inconsistent application of date and vendor filters can produce incomplete or irrelevant result sets
- Lack of filtering by validity dates (valid from / valid to) can include obsolete or not-yet-valid bank data
- Unmonitored vendor bank data can delay identification of master data errors that affect financial reporting

**Management Visibility and Decision-Making Risks**
- Lack of monitoring delays awareness of vendor bank records that require business review or correction
- Unidentified patterns (e.g. by vendor, country, or period) can lead to missed corrective actions and policy enforcement
- Absence of date-range and entity filtering limits the ability to focus on high-risk or recent changes

## Suggested Resolution

**Immediate Response**
- Review the vendor bank records flagged by the EI to understand scope (vendor, bank country, vendor country, and count)
- Verify high-value or sensitive vendors to confirm whether the country mismatch is intentional (e.g. cross-border banking) or an error
- Check that the date range (e.g. creation date, validity) and selection criteria (vendor, bank country, vendor country) match the intended monitoring scope
- Identify business context for result volume: expected vs unexpected counts, and whether corrections are needed

**System Assessment**
- Analyze the date parameters (days back, creation date range, validity from/to) to ensure the monitoring window covers the required period
- Compare current result volumes to prior runs using the same criteria to spot trends or anomalies
- Examine vendor and country selection to ensure critical vendors and regions are included
- Validate validity dates (valid from, valid to) so that only relevant bank account records are in scope
- Assess whether RFC (cloud) mode or on-premise mode is in use and that the correct data source is monitored

**Corrective Actions**
- If erroneous bank country or vendor country data is identified, correct vendor bank master (e.g. XK02 or bank detail maintenance) or vendor master as appropriate
- Escalate persistent mismatches to procurement and treasury for policy clarification (e.g. when cross-border bank accounts are allowed)
- Update selection criteria or date range to align monitoring with current business rules
- Document review and resolution for audit trail and schedule recurring EI runs for continuous visibility
- Configure alert routing to procurement and treasury for timely review of vendor bank country exceptions


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 2 | BANKL | Bank Key | CHAR | 15 | 0 | BANKK | BANKK |
| 3 | BANKN | Bank Account | CHAR | 18 | 0 | BANKN | BANKN |
| 4 | BANKS | Bank Country | CHAR | 3 | 0 | BANKS | LAND1 |
| 5 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 6 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 7 | KOBIS | Eff.to | DATS | 8 | 0 | KOBIS | DATUM |
| 8 | KOVON | Valid from | DATS | 8 | 0 | KOVON | DATUM |
| 9 | LAND1 | Vendor Country | CHAR | 3 | 0 | LAND1 | LAND1 |
| 10 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 11 | NAME1 | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 11 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Days Back):

How many days to look back from today. When no creation date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window for vendor and bank data. Used to limit the creation date window (ERDAT) for vendor master records.

**BANKL** (Bank Key):

Bank key (e.g. bank identifier). Filters vendor bank data (LFBK). Restricts which bank records are returned; used together with bank account and bank country for identification.

**BANKN** (Bank Account):

Bank account number. Filters vendor bank data (LFBK). Restricts which bank accounts are returned; used with bank key and bank country.

**BANKS** (Bank Country):

Country of the bank. Filters vendor bank data (LFBK). The EI returns only records where this country differs from the vendor's country (LAND1). Determines which bank countries are in scope.

**BUKRS** (Company Code):

Company code. May be used for filtering or context; check function usage for exact role in selection or output.

**ERDAT** (Created on):

Creation date of the vendor master record. Filters vendor data (LFA1) by creation date. When no date range is supplied, the EI builds a default range from today minus BACKDAYS to today. Restricts which vendors (and thus which bank records) are in scope by creation date.

**KOBIS** (Eff.to):

Valid-to date of the bank account. Filters vendor bank data (LFBK); only records valid up to this date (or overlapping with the monitoring period) are considered. When not supplied, the EI may use the end of the date range (e.g. today). Used with KOVON for validity window.

**KOVON** (Valid from):

Valid-from date of the bank account. Filters vendor bank data (LFBK); only records valid from this date (or overlapping with the monitoring period) are considered. When not supplied, the EI may use the start of the date range. Used with KOBIS for validity window.

**LAND1** (Vendor Country):

Country of the vendor. Filters vendor data (LFA1). The EI returns only records where the bank country (BANKS) differs from this vendor country. Determines which vendor countries are in scope.

**LIFNR** (Vendor):

Vendor number. Filters vendor and vendor bank data (LFA1, LFBK). Restricts which vendors (and their bank accounts) are returned.

**NAME1** (Name 1):

Vendor name. May be used for display or filtering depending on function usage; check output structure and code for exact role.


### Parameter Relationships

**Date and validity parameters**
- **BACKDAYS** and **ERDAT** work together to define the creation date window for vendor master records. When no ERDAT range is supplied, the EI builds a default range from today minus BACKDAYS to today; only vendors created in that window are considered.
- **KOVON** and **KOBIS** work together to define the validity window for vendor bank accounts. Only bank account records valid in the overlap with the monitoring period (e.g. KOVON ≤ today, KOBIS ≥ start of window) are returned. When not supplied, the EI uses default start/end dates from the date range.

**Vendor and bank identification**
- **LIFNR**, **BANKS**, **BANKL**, and **BANKN**: LIFNR filters by vendor; BANKS, BANKL, BANKN filter by bank country, bank key, and bank account. Together they define which vendor bank records are read; the EI then returns only those where BANKS differs from the vendor's country (LAND1).
- **LAND1** (Vendor Country): Filters vendors by country. The EI returns only records where the bank country (BANKS) does not equal LAND1, so LAND1 and BANKS together define the mismatch condition.

**Selection scope**
- **ERDAT**, **LAND1**, **LIFNR**, **BANKS**: ERDAT limits vendors by creation date; LAND1 and LIFNR limit which vendors are in scope; BANKS limits which bank countries are considered. Together they define the set of vendor bank records evaluated for country mismatch.


### Default Values

- **BACKDAYS** — Used to derive the default creation date range when no ERDAT range is supplied. The EI uses today minus BACKDAYS as the start and today as the end of the monitoring window for vendor creation date.
- **ERDAT** — When not supplied, the EI builds a default range from today minus BACKDAYS to today for filtering vendor master by creation date.
- **KOVON** — When not supplied, the EI may use the start of the date range (e.g. DATE_FROM) as the default valid-from for bank account validity.
- **KOBIS** — When not supplied, the EI may use the end of the date range (e.g. today) as the default valid-to for bank account validity.

**Note:** The EI returns only vendor bank records where the bank country (BANKS) differs from the vendor's country (LAND1). Date and validity parameters define the time window and validity overlap for selection.

### Practical Configuration Examples

**Use Case 1: Recent vendor creations – last 30 days**
```
BACKDAYS = 30
ERDAT = (default from BACKDAYS)
LIFNR = (optional range)
LAND1 = (optional country)
BANKS = (optional bank country)
```
**Purpose:** Monitor vendor bank country vs vendor country for vendors created in the last 30 days; optional filters for vendor and countries.

**Use Case 2: Specific vendor and validity window**
```
LIFNR = 0000100001 (or range)
KOVON = 20240101
KOBIS = 20251231
ERDAT = (optional)
```
**Purpose:** Check bank country mismatch for a specific vendor (or range) with bank accounts valid in the given validity window.

**Use Case 3: Country-focused review**
```
LAND1 = DE
BANKS = (leave open or specify non-DE countries)
BACKDAYS = 90
```
**Purpose:** Identify German vendors (LAND1 = DE) whose bank country (BANKS) is not DE, over the last 90 days of creation date, for payment and compliance review.


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_BANK_CODE_COMP | BANKL | Bank Keys | CHAR(15) | BANKK |
| /SKN/S_SW_10_06_BANK_CODE_COMP | BANKN | Bank account number | CHAR(18) | BANKN |
| /SKN/S_SW_10_06_BANK_CODE_COMP | BANKS | Bank country key | CHAR(3) | BANKS |
| /SKN/S_SW_10_06_BANK_CODE_COMP | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_BANK_CODE_COMP | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_06_BANK_CODE_COMP | KOBIS | Bank details valid to | DATS(8) | KOBIS |
| /SKN/S_SW_10_06_BANK_CODE_COMP | KOVON | Bank Details Valid From | DATS(8) | KOVON |
| /SKN/S_SW_10_06_BANK_CODE_COMP | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_BANK_CODE_COMP | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_BANK_CODE_COMP | NAME1 | Name | CHAR(30) | NAME1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_THIRD_BANK_ACC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_BANK_CODE_COMP OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_DATA,
             LIFNR TYPE LFBK-LIFNR,
             BANKS TYPE LFBK-BANKS,
             BANKL TYPE LFBK-BANKL,
             BANKN TYPE LFBK-BANKN,
             KOVON TYPE LFBK-KOVON,
             KOBIS TYPE LFBK-KOBIS,
             LAND1 TYPE LFA1-LAND1,
             NAME1 TYPE LFA1-NAME1,
           END OF TY_DATA,
           TT_DATA TYPE STANDARD TABLE OF TY_DATA.
  DATA_SINGLE: SW_DEST       RFCDEST,
               BACKDAYS      INT4,
               FORWDAYS      INT4,
               KOVON         KOVON,
               KOBIS         KOBIS.
  DATA_MULTY: LIFNR    LIFNR,
              LAND1    LAND1,
              ERDAT    ERDAT_RF,
              BANKS    BANKS,
              DATUM    SY-DATUM.
  SELECT_MULTY: LIFNR,
                LAND1,
                ERDAT,
                BANKS,
                DATUM.
  SELECT_SINGLE: SW_DEST,
                 BACKDAYS.
  CONVERT_MULTY: LIFNR ALPHA.
  DATA: SY_TABIX LIKE SY-TABIX,
        SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
*  DATA: lt_data  TYPE tt_data,
*        ls_data  TYPE ty_data,
*        ls_data2 LIKE LINE OF t_data[].
  DATA: BACKDAYS  TYPE I,
        FORWDAYS  TYPE I,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM,
        REF_DATE  TYPE D.
  DATA: TIME_DIFF TYPE  INT4 .
  DATA: FLD(60) TYPE C.
  FIELD-SYMBOLS:  TYPE ANY.
  IF NOT LV_FORWDAYS  IS INITIAL.
    LV_BACKDAYS = LV_FORWDAYS * ( -1 ).
  ENDIF.
  IF R_ERDAT[] IS INITIAL AND LV_BACKDAYS <> 0.
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM .
    RS_DATUM-HIGH   = SY-DATUM.
    APPEND RS_DATUM TO R_DATUM.
    R_ERDAT = R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  DATE_FROM = SY-DATUM.
  READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_DATUM-LOW.
    DATE_TO   = RS_DATUM-HIGH.
    IF DATE_TO < DATE_FROM.
      DATE_TO = DATE_FROM.
    ENDIF.
  ENDIF.
  IF LV_KOVON IS INITIAL.
    LV_KOVON = DATE_FROM.
  ENDIF.
  IF LV_KOBIS IS INITIAL.
    LV_KOBIS = DATE_TO.
  ENDIF.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_THIRD_BANK_AC'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR: IS_ALERT. "lt_data.
  REFRESH T_DATA.
  SELECT LFBK~LIFNR LFBK~BANKS LFBK~BANKL LFBK~BANKN
         LFBK~KOVON LFBK~KOBIS
         LFA1~LAND1 LFA1~NAME1
    FROM LFBK INNER JOIN LFA1 ON  LFBK~LIFNR EQ LFA1~LIFNR
                              AND LFBK~BANKS <> LFA1~LAND1
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE LFBK~LIFNR IN R_LIFNR
    AND   LFBK~BANKS IN R_BANKS
    AND   LFA1~ERDAT IN R_ERDAT
    AND   LFA1~LAND1 IN R_LAND1
    AND   LFBK~KOVON >= SY-DATUM
    AND   LFBK~KOBIS <= SY-DATUM.
.
*  LOOP AT lt_data INTO ls_data.
*
*    CLEAR: ls_data2.
*    IF ls_data-banks <> ls_data-land1.
*      MOVE-CORRESPONDING ls_data TO ls_data2.
*      APPEND ls_data2 TO t_data.
*    ENDIF.
*  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```