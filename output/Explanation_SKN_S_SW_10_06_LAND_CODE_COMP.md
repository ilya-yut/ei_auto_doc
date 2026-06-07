# Exception Indicator: Company code country differs vendor's country ( SW_10_06_LAND_COMPAR)

## General Overview

This Exception Indicator finds vendor company-code records where the vendor’s country on the business partner master differs from the country of the assigned company code.

This EI serves as an essential control for master data and accounts payable quality by:

- Detecting cross-border vendor–company code combinations that may affect tax, payment, or compliance rules
- Supporting review of newly created or recently changed vendor company-code records
- Enabling filters by vendor, company code, and vendor country before exceptions are raised
- Providing vendor name and both country keys for straightforward remediation
- Reducing payment and reporting risk from mismatched geographic master data

Typical use includes vendor master reviews, post-migration checks, and periodic compliance sampling. Results are intended for exception workflows rather than full vendor extracts.

The routine reads vendor company-code data joined to vendor general data and company code country, applies selection filters, and raises an alert when at least one country mismatch remains.


## Problem Description

When a vendor’s country does not match the country of the company code they are extended to, tax, banking, and regulatory treatment can be wrong while the master record still appears complete.

**Master Data and Compliance Risks**

- Cross-border vendor–company code pairs may use incorrect tax or payment rules
- New or changed vendor company-code records can go live before country alignment is validated
- High vendor volumes make manual comparison of general versus company code country impractical

**Operational Risks**

- Forward-day and backward-day window settings that are misaligned can miss recent creations or include obsolete records
- Empty creation-date selection relies on the monitoring window; explicit date ranges override that behavior

**Control and Audit Risks**

- Weak monitoring reduces evidence that country mismatches were reviewed before payments or reporting
- Unclear forward-day interaction with backward days can confuse teams tuning the time window

## Suggested Resolution

**Immediate Response**

- Review flagged vendors, company codes, vendor country, company country, and creation dates
- Confirm with master-data owners whether the country pairing is valid for the business relationship
- Prioritize vendors with open items or recent payment activity

**System Assessment**

- Compare exception counts by company code and vendor country to find systematic data gaps
- Validate creation-date windows and forward-day settings against how the team defines “new” vendors
- Check whether vendor or company code filters should narrow the queue

**Corrective Actions**

- Correct vendor or company code country attributes through standard master-data processes
- Adjust monitoring parameters after cleanup to keep results actionable
- Document review outcomes and schedule recurring runs for critical company codes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |  |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 4 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 5 | FORWDAYS | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |  |
| 6 | LFA1_LAND1 | Vendor's Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 7 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 8 | SW_DEST |  | 0 | 0 |  |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 8 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (INT4)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERDAT

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**DATUM** (DATS)

Optional reference date parameter from the selection framework; when creation date is not supplied and **BACKDAYS** is non-zero, the built monitoring range is applied to **ERDAT** instead.

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**FORWDAYS** (INT4)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

Forwdays is based on ERDAT

**LFA1_LAND1** (Vendor's Country Key)

Vendor country on the business partner master; the routine returns rows where this value differs from the company code country on the linked company record.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.


### Parameter Relationships

How parameter combinations work together

**Mismatch logic:** The database selection joins vendor company code, vendor general data, and company code master data and keeps rows where vendor country and company code country are not equal.

**Scope filters:** **LIFNR**, **BUKRS**, and **LFA1_LAND1** narrow which vendor company-code records enter the join.

**Creation-date window:** When **ERDAT** is not selected and **BACKDAYS** is not zero, a from–to range from today minus **BACKDAYS** through today is built and applied to **ERDAT**. Explicit **ERDAT** selections override that fallback.

**Forward days:** When **FORWDAYS** is set, it replaces **BACKDAYS** with the negated forward-day value before the creation-date window is built, shifting the backward window accordingly.

**Reference date:** **DATUM** is available for selection but the on-premise path applies the monitoring range to **ERDAT** via the internal date range copy described above.

**Execution path:** **SW_DEST** delegates to the cloud function when set; otherwise the on-premise join and filter logic runs locally.


### Default Values

- **BACKDAYS** - initial - treated as 0 by code
- **FORWDAYS** - initial - treated as 0 by code

### Practical Example of Parameter Configuration

**Use Case 1: Recent vendor company-code creations**

**Purpose:** Find country mismatches for vendor company-code records created in the last thirty days.

```
BACKDAYS = 30
BUKRS = 1000
```

**Use Case 2: Single vendor review**

**Purpose:** Check one vendor across all selected company codes for country alignment.

```
LIFNR = 100000
BACKDAYS = 90
```

**Use Case 3: Vendor country filter**

**Purpose:** Monitor vendors with a specific vendor country key that still differ from company code country.

```
LFA1_LAND1 = US
BUKRS = 2000
BACKDAYS = 14
```

**Use Case 4: Forward-day window adjustment**

**Purpose:** Shift the creation-date window using forward days before applying the backward range.

```
FORWDAYS = 7
BACKDAYS = 30
LIFNR = 200000
BUKRS = 3000
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_LAND_CODE_COMP | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_LAND_CODE_COMP | ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_06_LAND_CODE_COMP | LFA1_LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_LAND_CODE_COMP | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_LAND_CODE_COMP | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_LAND_CODE_COMP | T001_LAND1 | Country Key | CHAR(3) | LAND1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_LAND_VEND_COMP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_LAND_CODE_COMP OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_DATA,
           LIFNR       TYPE LFB1-LIFNR,
           BUKRS       TYPE LFB1-BUKRS,
           LFA1_LAND1  TYPE LFA1-LAND1,
           NAME1       TYPE LFA1-NAME1,
           ERDAT       TYPE LFB1-ERDAT,
           T001_LAND1  TYPE T001-LAND1,
         END OF TY_DATA,
         TT_DATA TYPE STANDARD TABLE OF TY_DATA.
  DATA_SINGLE: SW_DEST       RFCDEST,
               BACKDAYS      INT4,
               FORWDAYS      INT4.
  DATA_MULTY: DURATION /SKN/E_SW_DURATION,
              LIFNR    LIFNR,
              BUKRS    BUKRS,
              LFA1_LAND1    LAND1,
              ERDAT    ERDAT_RF,
              DATUM    SY-DATUM.
  SELECT_MULTY: LIFNR,
                BUKRS,
                LFA1_LAND1,
                ERDAT,
                DATUM.
  SELECT_SINGLE: SW_DEST,
                 BACKDAYS
                 .
*  convert_multy: lifnr alpha,
*                 bukrs alpha.
  DATA: SY_TABIX LIKE SY-TABIX,
        SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  DATA: LT_DATA  TYPE TT_DATA,
        LS_DATA  TYPE TY_DATA,
        LS_DATA2 LIKE LINE OF T_DATA[].
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
    RS_DATUM-OPTION = 'BT' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM .
    RS_DATUM-HIGH   = SY-DATUM.
    APPEND RS_DATUM TO R_DATUM.
    R_ERDAT[] = R_DATUM[].
  ENDIF.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_LAND_VEND_COM'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT LFB1~LIFNR LFB1~BUKRS
         LFA1~LAND1 AS LFA1_LAND1 LFA1~NAME1
         T001~LAND1 AS T001_LAND1
    FROM LFB1 INNER JOIN LFA1 ON  LFB1~LIFNR EQ LFA1~LIFNR
              INNER JOIN T001 ON  LFB1~BUKRS EQ T001~BUKRS
                              AND LFA1~LAND1 <> T001~LAND1
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE LFB1~LIFNR IN R_LIFNR
    AND   LFB1~BUKRS IN R_BUKRS
    AND   LFB1~ERDAT IN R_ERDAT
    AND   LFA1~LAND1 IN R_LFA1_LAND1.
*  LOOP AT lt_data INTO ls_data.
*    IF ls_data-lfa1_land1 <> ls_data-t001_land1.
*      MOVE-CORRESPONDING ls_data TO ls_data2.
*      APPEND ls_data2 TO t_data.
*    ENDIF.
*  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
