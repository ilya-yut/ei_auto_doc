# Exception Indicator: Bank in payment advise missing in Vendor's master ( SW_10_07_BANK_VEND_D)

## General Overview

This Exception Indicator monitors payment run header data and flags cases where the bank details used in the payment advise are missing from the vendor master or are outside the bank validity period on vendor bank records.

This EI serves as an essential control for accounts payable, treasury, and vendor master governance by:

- Surfacing payment runs where REGUH bank data does not match an active vendor bank record in LFBK
- Detecting bank lines on the payment advise whose validity interval does not include the payment run date
- Supporting payment-readiness and fraud reviews before disbursement
- Enabling scoped monitoring by company code, vendor, payee, run date, and payment bank details
- Supporting both on-premise and cloud execution through optional destination routing

Typical use includes reviews before payment proposals, after payment run completion, or when validating that advise bank data exists on vendor master. Results are intended for exception workflows rather than full payment or bank master extracts.

The routine reads payment run headers joined to vendor and payee master, applies date and duration filters, compares advise bank keys to vendor bank master, and returns lines where the bank is missing or not valid on the run date.


## Problem Description

Failure to monitor payment advise bank details against vendor master creates multiple risks across payment execution, compliance, and master data quality.

**Payment and Treasury Risks**

- Payments may proceed with bank data on the advise that does not exist on vendor master, increasing rejection and repair effort
- Bank records present on master but outside their validity window may still appear on payment advises without review
- Concentrations by company code or run date are harder to detect without automated comparison to LFBK

**Control and Compliance Risks**

- Lack of periodic exception reporting weakens evidence that payee bank data was validated before disbursement
- Misalignment between REGUH bank fields and LFBK can complicate audit trails for payee location and account

**Master Data and Operations Risks**

- Payment and vendor bank maintenance gaps are often discovered only at execution time
- Cloud and on-premise landscapes need consistent monitoring when remote execution is used

## Suggested Resolution

**Immediate Response**

- Review each flagged payment run line for vendor, payee, company code, advise bank country, key, account, and run date
- Confirm with AP or master data whether vendor bank master must be created, updated, or validity dates corrected
- Prioritize vendors with upcoming or completed payment runs in the affected company code

**System Assessment**

- Compare exception volume to prior runs using the same run-date window and company code filters
- Look for clusters by bank country or vendor range that may trace to migration or mass upload
- Validate that run-date, backdays, forward days, and duration settings match the intended monitoring period

**Corrective Actions**

- Maintain or correct vendor bank details in LFBK with required approvals
- Adjust bank validity dates when the account should be active on the payment run date
- Document review outcomes for audit trail and schedule recurring runs for company codes in scope
- Route repeat interface or conversion defects into change management when advise versus master gaps are systematic


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BANKL | Bank Key | CHAR | 15 | 0 | BANKK | BANKK |
| 3 | BANKN | Bank Account | CHAR | 18 | 0 | BANKN | BANKN |
| 4 | BANKS | Bank Country | CHAR | 3 | 0 | BANKS | LAND1 |
| 5 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 6 | CONVERT_KEY |  | 0 | 0 |  |  |  |
| 7 | COUNTER |  |  |  |  |  |  |
| 8 | DATE_REF_FLD | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |  |
| 9 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | EMPFG | Payment recipient | CHAR | 16 | 0 | EMPFG | EMPFG |
| 13 | FORWDAYS | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |  |
| 14 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 15 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 16 | LANGU |  | 0 | 0 |  |  |  |
| 17 | LAUFD | Run Date | DATS | 8 | 0 | LAUFD | DATUM |
| 18 | LAUFI | Identification | CHAR | 6 | 0 | LAUFI | LAUFI |
| 19 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 20 | LNRZA | Alternative payee | CHAR | 10 | 0 | LNRZA | LIFNR |
| 21 | LOEVM | Central deletion flag | CHAR | 1 | 0 | LOEVM_X | XFELD |
| 22 | STKZN |  |  |  |  |  |  |
| 23 | SW_DEST |  | 0 | 0 |  |  |  |
| 24 | VALID_FROM | DATS | 8 | 0 | KOVON | DATUM |  |
| 25 | VALID_TO | DATS | 8 | 0 | KOBIS | DATUM |  |
| 26 | VBUND |  |  |  |  |  |  |
| 27 | XCPDK | One-time account | CHAR | 1 | 0 | XCPDK | XFELD |
| 28 | XVORL | Indicator: Only Proposal Run? | CHAR | 1 | 0 | XVORL | XFELD |
| 29 | ZBNKL | Bank number | CHAR | 15 | 0 | DZBNKL | BANKL |
| 30 | ZBNKN | Payee's bank acct number | CHAR | 18 | 0 | DZBNKN | BANKN |
| 31 | ZBNKS | Country Key | CHAR | 3 | 0 | DZBNKS | LAND1 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 31 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BANKL** (Bank Key)

Bank number key linking payment master to a specific bank account at the house bank in cash management.

**BANKN** (Bank Account)

House bank account id linking company code banking data to a specific account at the house bank (payment run context).

**BANKS** (Bank Country)

Bank country key governing bank-key validation rules and payment formats for the account.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**CONVERT_KEY** (CONVERT_KEY)

<mark>Flag that determines whether the change log decomposes the compressed table key (TABKEY) into readable key components and converts technical KEY change lines into field-level key updates.
CONVERT_KEY Options:
X - decompose TABKEY into KEY1-KEY10, KEY1_V - KEY10_V, and KEY1_DS - KEY10_DS; convert KEY insert/delete lines into key-field change rows and remove raw FNAME = KEY lines where the key-change case applies.
Empty or blank - do not run key conversion; keep standard change-document lines and identify the changed object primarily via TABKEY (and OBJECTID where applicable).</mark>

**COUNTER** (Counter)

Runtime counter passed by the online monitor to identify the evaluation pass when multiple runs are coordinated.

**DATE_REF_FLD** (Date Reference Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- LAUFD — Run Date.

**DATUM** (Reference Date)

Reference date supplied by the online monitor; used with explicit date selection when deriving the effective run-date window.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**EMPFG** (Payment recipient)

Helps monitoring stay readable by requiring payment recipient (EMPFG) to match organizational or technical selectors when set.

**FORWDAYS** (Forward Days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

**KTOKK** (Account group)

Account group (customer/vendor) used to segment master data governance rules.

**LAND1** (Country Key)

Country key used for legal/geographic segmentation of business partners or plants.

**LANGU** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**LAUFD** (Run Date)

Payment program run date on the REGUH header; primary date for run selection and default reference for backdays and duration logic.

**LAUFI** (Identification)

Explains why two monitoring passes differ: only the pass with stricter identification on LAUFI surfaces the disputed rows.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LNRZA** (Alternative payee)

Valuable when comparing health before and after a release—hold alternative payee on LNRZA constant while varying other filters.

**LOEVM** (Central deletion flag)

Documents expected operator behavior—central deletion flag on LOEVM should be set when that dimension is part of the control objective.

**STKZN** (Natural Person)

Natural-person indicator on customer or vendor master distinguishing natural persons from legal entities.

**SW_DEST** (RFC Destination)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**VALID_FROM** (DATS)

For operations, dats on VALID_FROM indicates whether a row belongs in the current monitoring pass versus historical noise.

**VALID_TO** (DATS)

Separates cross-client noise from in-scope work when dats on VALID_TO correlates with client or user attributes.

**VBUND** (Company ID)

Trading partner/company field used for intercompany transaction analysis.

**XCPDK** (One-time account)

One-time account indicator used to identify one-time customer/vendor postings.

**XVORL** (Indicator: Only Proposal Run?)

For distributed landscapes, indicator: only proposal run? on XVORL often anchors which application server or destination appears in results.

**ZBNKL** (Bank number)

Stabilizes week-over-week metrics by fixing bank number (ZBNKL) while allowing duration thresholds to move.

**ZBNKN** (Payee's bank acct number)

After data is read, lines are removed unless payee's bank acct number on ZBNKN still satisfies the active multivalued selection.

**ZBNKS** (Country Key)

When populated, keeps the extract focused so country key (ZBNKS) aligns with the intended triage slice.


### Parameter Relationships

How parameter combinations work together

**Payment run scope:** **LAUFD**, **LAUFI**, **XVORL**, **BUKRS**, **LIFNR**, **EMPFG**, **ZBNKS**, **ZBNKN**, and **ZBNKL** define which payment run header lines are read from REGUH. **KTOKK**, **LAND1**, **LNRZA**, **VBUND**, **STKZN**, **XCPDK**, and **LOEVM** narrow vendor and payee master attributes (the routine excludes one-time vendors per the **XCPDK** setting).

**Date window:** When the monitor date range is empty, **BACKDAYS** and **FORWDAYS** build a from–to window on the reference date range, which is applied to **LAUFD** when **DATE_REF_FLD** is LAUFD. Explicit **LAUFD** or **DATUM** selections override that fallback when populated.

**Duration filter:** After the date window is applied, **DURATION** with **DURATION_UNIT** is an additional age filter: rows whose elapsed time from the field named in **DATE_REF_FLD** does not fall in the selected duration band are removed.

**Bank master comparison:** **BANKS**, **BANKL**, and **BANKN** can further scope vendor bank master reads; **VALID_FROM** and **VALID_TO** bound bank validity when supplied. A row is flagged when no matching LFBK line exists for the advise bank or when the run date is outside the bank validity interval.

**Execution path:** **SW_DEST** delegates processing to the cloud function module when populated; **LANGU** drives country text retrieval on the on-premise path.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DATE_REF_FLD** - initial - treated as LAUFD by code
- **DURATION_UNIT** - initial - treated as D by code
- **FORWDAYS** - initial - treated as 10 by code
- **LANGU** - initial - treated as EN by code
- **XCPDK** - initial - treated as X by code
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Payment runs in the last ten days**

**Purpose:** Find payment advise lines from the last ten days whose bank data is missing or invalid on vendor master.

```
BUKRS = 1000
BACKDAYS = 10
FORWDAYS = 10
DATE_REF_FLD = LAUFD
DURATION_UNIT = D
```

**Use Case 2: Specific vendor and run identification**

**Purpose:** Review one vendor's payment runs for a known run identification.

```
LIFNR = 0000100001
BUKRS = 1000
LAUFI = 000001
BACKDAYS = 30
```

**Use Case 3: Advise bank country focus**

**Purpose:** Monitor US advise bank countries for selected company codes.

```
BUKRS = US01
ZBNKS = US
BACKDAYS = 14
FORWDAYS = 0
```

**Use Case 4: Duration filter on run date**

**Purpose:** Flag payment lines whose run date is at least seven days before evaluation when using day-based duration.

```
BUKRS = DE01
DURATION = 7
DURATION_UNIT = D
DATE_REF_FLD = LAUFD
BACKDAYS = 30
```

**Use Case 5: Run date exactly seven full days ago**

**Purpose:** Flag payment lines whose run date falls in the scope of exactly 7 full days ago when using full-day duration counting.

```
BUKRS = 1000
DURATION = 7
DURATION_UNIT = F
DATE_REF_FLD = LAUFD
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_07_ONE_TIME_VEND | BANKL | Bank Key | CHAR(15) | BANKK |
| /SKN/S_SW_10_07_ONE_TIME_VEND | BANKN | Bank Account | CHAR(18) | BANKN |
| /SKN/S_SW_10_07_ONE_TIME_VEND | BANKS | Bank Country | CHAR(3) | BANKS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | BUTXT | Company Name | CHAR(25) | BUTXT |
| /SKN/S_SW_10_07_ONE_TIME_VEND | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_07_ONE_TIME_VEND | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_07_ONE_TIME_VEND | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_07_ONE_TIME_VEND | EKOTX | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_07_ONE_TIME_VEND | EMPFG | Payment recipient | CHAR(16) | EMPFG |
| /SKN/S_SW_10_07_ONE_TIME_VEND | KONZS | Group key | CHAR(10) | KONZS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | KTOKK | Account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LANDX | Name | CHAR(15) | LANDX |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAUFD | Run Date | DATS(8) | LAUFD |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAUFI | Identification | CHAR(6) | LAUFI |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LNRZA | Alternative payee | CHAR(10) | LNRZA |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LOEVM | Central deletion flag | CHAR(1) | LOEVM_X |
| /SKN/S_SW_10_07_ONE_TIME_VEND | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_07_ONE_TIME_VEND | NAME1_PAYEE | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_07_ONE_TIME_VEND | PAYEE | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | SPERM | Central purchasing block | CHAR(1) | SPERM_X |
| /SKN/S_SW_10_07_ONE_TIME_VEND | SPERR | Central posting block | CHAR(1) | SPERB_X |
| /SKN/S_SW_10_07_ONE_TIME_VEND | VBLNR | Payment document no. | CHAR(10) | VBLNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | XCPDK | One-time account | CHAR(1) | XCPDK |
| /SKN/S_SW_10_07_ONE_TIME_VEND | XVORL | Indicator: Only Proposal Run? | CHAR(1) | XVORL |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZBNKL | Bank number | CHAR(15) | DZBNKL |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZBNKN | Payee's bank acct number | CHAR(18) | DZBNKN |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZBNKS | Country Key | CHAR(3) | DZBNKS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZNME1 | Payee name | CHAR(35) | DZNME1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_07_BANK_VEND_DEF .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_07_ONE_TIME_VEND
*"----------------------------------------------------------------------
  INCLUDE /SKN/PC_SQL_DATA.
  TYPES: BEGIN OF TY_DATA,
           LAUFD         TYPE REGUH-LAUFD,
           LAUFI         TYPE REGUH-LAUFI,
           XVORL         TYPE REGUH-XVORL,
           ZBUKR         TYPE REGUH-ZBUKR,
           LIFNR         TYPE REGUH-LIFNR,
           KUNNR         TYPE REGUH-KUNNR,
           EMPFG         TYPE REGUH-EMPFG,
           VBLNR         TYPE REGUH-VBLNR,
           ZBNKS         TYPE REGUH-ZBNKS,
           ZBNKN         TYPE REGUH-ZBNKN,
           ZBNKL         TYPE REGUH-ZBNKL,
           PAYEE         TYPE LFA1-LIFNR,
           LAND1         TYPE LFA1-LAND1,
           LANDX         TYPE T005T-LANDX,
           ERDAT         TYPE LFA1-ERDAT,
           LNRZA         TYPE LFA1-LNRZA,
           LOEVM         TYPE LFA1-LOEVM,
           KTOKK         TYPE LFA1-KTOKK,
           XCPDK         TYPE LFA1-XCPDK,    " One time vendor
           VBUND         TYPE LFA1-VBUND,
           STKZN         TYPE LFA1-STKZN,
           SPRAS         TYPE T005T-SPRAS,
           DURATION      TYPE /SKN/E_SW_DURATION,
           DURATION_UNIT TYPE /SKN/E_SW_DURATION_UNIT,
         END OF TY_DATA,
         TT_DATA TYPE TABLE OF TY_DATA.
  TYPES: BEGIN OF TY_LFA1,
           LIFNR TYPE LFA1-LIFNR,
           LAND1 TYPE LFA1-LAND1,
           ERDAT TYPE LFA1-ERDAT,
           LNRZA TYPE LFA1-LNRZA,
           LOEVM TYPE LFA1-LOEVM,
           KTOKK TYPE LFA1-KTOKK,
           XCPDK TYPE LFA1-XCPDK,    " One time vendor
           VBUND TYPE LFA1-VBUND,
           STKZN TYPE LFA1-STKZN,
           LANDX TYPE T005T-LANDX,
           SPRAS TYPE T005T-SPRAS,
         END OF TY_LFA1,
         TT_LFA1 TYPE TABLE OF TY_LFA1.
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: SW_DEST             RFCDEST,
               LANGU               LANGU,
               BACKDAYS            INT4,
               FORWDAYS            INT4,
               XCPDK               XCPDK,
               LOEVM               LOEVM_X,
               DURATION_UNIT       /SKN/E_SW_DURATION_UNIT,
               DATE_REF_FLD        NAME_FELD,
               CONVERT_KEY         CHAR1.
  DATA_MULTY:   LIFNR             LIFNR,
                BUKRS             DZBUKR,
                LAND1             LAND1_GP,
                LAUFD             LAUFD,
                LAUFI             LAUFI,
                XVORL             XVORL,
                VBUND             RASSC,
                KTOKK             KTOKK,
                STKZN             STKZN,
                LNRZA             LNRZA,
                EMPFG             EMPFG,
                ZBNKS             DZBNKS,
                ZBNKN             DZBNKN,
                ZBNKL             DZBNKL,
                BANKS             BANKS,
                BANKL             BANKK,
                BANKN             BANKN,
                VALID_FROM        KOVON,
                VALID_TO          KOBIS,
                DURATION          /SKN/E_SW_DURATION,
                DATUM             SYDATUM,
                COUNTER           I.
  DATA: SY_DATLO LIKE SY-DATLO ,
        SY_TIMLO LIKE SY-TIMLO .
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: SY_TABIX  LIKE SY-TABIX,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM.
  DATA: LV_SHIFT      TYPE DDLENG,
        LV_LENG       TYPE DDLENG,
        LV_DOMNAME    TYPE DD07V-DOMNAME,
        LV_DOMVALUE   TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT     TYPE DD07V-DDTEXT,
        LV_OBJECT     TYPE CDOBJECTV,
        LV_LIFNR      TYPE LIFNR,
        LV_STRUCTURE  TYPE DDOBJNAME,
        LV_INDEX      TYPE I,
        LV_OBJECTCLAS TYPE CDOBJECTCL,
        LV_DOC        TYPE CDCHANGENR,
        LV_COUNT_TMP  TYPE I,
        LV_LINES      TYPE I.
  DATA: LS_DATA TYPE TY_DATA,
        LS_LIFNR TYPE /SKN/S_SW_10_LIFNR,
        LS_LFBK TYPE LFBK,
        LS_LFA1 TYPE TY_LFA1.
  DATA: LT_DATA  TYPE TT_DATA,
        LT_LFA1  TYPE TT_LFA1,
        LT_LIFNR TYPE TABLE OF /SKN/S_SW_10_LIFNR,
        LT_LFBK  TYPE TABLE OF LFBK.
  FIELD-SYMBOLS: <FS_DATA>    LIKE LINE OF T_DATA[],
                          TYPE ANY.
* Set default parameter
  LV_BACKDAYS       = 10.
  LV_FORWDAYS       = 10.
  LV_DURATION_UNIT  = 'D'.
  LV_DATE_REF_FLD   = 'LAUFD'.
  LV_LANGU          = 'EN'.
  LV_XCPDK          = 'X'.     " One time vendor
*  lv_loevm          = ''.        " Deletion flag
  SELECT_MULTY:  LIFNR,
                 BUKRS,
                 LAND1,
                 LAUFD,
                 VBUND,
                 KTOKK,
                 STKZN,
                 LNRZA,
                 EMPFG,
                 ZBNKS,
                 ZBNKN,
                 ZBNKL,
                 BANKS,
                 BANKL,
                 BANKN,
                 VALID_FROM,
                 VALID_TO,
                 COUNTER,
                 DATUM .
  SELECT_SINGLE: SW_DEST,
                 LANGU,
                 BACKDAYS,
                 FORWDAYS,
                 XCPDK,
                 LOEVM,
                 DATE_REF_FLD,
                 CONVERT_KEY,
                 DATE_REF_FLD,
                 DURATION_UNIT.
  "--- Run Cloud Mode -----
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_07_BANK_VEND_DEF'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  CONVERT_MULTY: LIFNR ALPHA.
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  " Set default value
  IF R_DATUM[] IS INITIAL .
* Backdays
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'BT' .
    DATE_FROM       = SY_DATLO - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM .
    DATE_TO         = SY_DATLO + LV_FORWDAYS.
    RS_DATUM-HIGH   = DATE_TO .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF .
  CASE LV_DATE_REF_FLD.
    WHEN 'LAUFD'.
      IF R_LAUFD[] IS INITIAL AND R_DATUM[] IS NOT INITIAL.
        R_LAUFD[] = R_DATUM[].
      ENDIF.
*    WHEN 'ERDAT'.
*      IF r_erdat[] IS INITIAL AND r_datum[] IS NOT INITIAL.
*        r_erdat[] = r_datum[].
*      ENDIF.
  ENDCASE.
*  SELECT lfa1~lifnr lfa1~land1 lfa1~erdat lfa1~lnrza lfa1~loevm lfa1~ktokk
*         lfa1~xcpdk lfa1~vbund lfa1~stkzn
*         t005t~landx
*    INTO CORRESPONDING FIELDS OF TABLE lt_lfa1
*    FROM lfa1 LEFT OUTER JOIN t005t ON  t005t~land1 EQ lfa1~land1
*                                    AND t005t~spras EQ lv_langu
*    WHERE lfa1~lifnr  IN r_lifnr[]
*    AND   lfa1~land1  IN r_land1[]
*    AND   lfa1~ktokk  IN r_ktokk[]
*    AND   lfa1~lnrza  IN r_lnrza[]
*    AND   lfa1~xcpdk  EQ lv_xcpdk
**    AND   loevm EQ lv_loevm
*    AND   lfa1~vbund  IN r_vbund[]
*    AND   lfa1~stkzn  IN r_stkzn[].
*
*  IF lt_lfa1 IS NOT INITIAL.
*    SORT lt_lfa1 BY lifnr.
*  ENDIF.
*
*  IF lt_lfa1 IS NOT INITIAL.
*    SELECT *
*      FROM reguh
*      INTO CORRESPONDING FIELDS OF TABLE t_data
*      FOR ALL ENTRIES IN lt_lfa1
*      WHERE lifnr EQ lt_lfa1-lifnr
*      AND   zbukr IN r_bukrs[]
*      AND   empfg IN r_empfg[]
*      AND   zbnks IN r_zbnks[]
*      AND   zbnkn IN r_zbnkn[]
*      AND   zbnkl IN r_zbnkl[].
*  ENDIF.
* Select From LFA1 and REGUH by LIFNR
*  SELECT r~laufd r~laufi r~xvorl r~zbukr r~lifnr
*         r~kunnr r~empfg r~vblnr r~zbnks r~zbnkn r~zbnkl
*         l~land1 l~erdat l~lnrza l~loevm l~ktokk
*         l~xcpdk l~vbund l~stkzn
*         t~spras t~landx
*    INTO CORRESPONDING FIELDS OF TABLE lt_data
*    FROM reguh AS r INNER JOIN lfa1       AS l ON  r~lifnr EQ l~lifnr
*                    LEFT OUTER JOIN t005t AS t ON  l~land1 EQ t~land1
*                                               AND t~spras EQ lv_langu
*    WHERE r~laufd IN r_laufd[]
*    AND   r~laufi IN r_laufi[]
*    AND   r~xvorl IN r_xvorl[]
*    AND   r~lifnr IN r_lifnr[]
*    AND   r~zbukr IN r_bukrs[]
*    AND   r~empfg EQ space
*    AND   r~zbnks IN r_zbnks[]
*    AND   r~zbnkn IN r_zbnkn[]
*    AND   r~zbnkl IN r_zbnkl[]
*    AND   l~ktokk IN r_ktokk[]
*    AND   l~xcpdk EQ lv_xcpdk
*    AND   l~vbund IN r_vbund[]
*    AND   l~stkzn IN r_stkzn[].
  SELECT R~LAUFD R~LAUFI R~XVORL R~ZBUKR R~LIFNR
         R~KUNNR R~EMPFG R~VBLNR R~ZBNKS R~ZBNKN R~ZBNKL
         L2~LIFNR AS PAYEE L2~LAND1 L2~ERDAT L2~LNRZA L2~LOEVM L2~KTOKK
         L2~XCPDK L2~VBUND L2~STKZN
*         t~spras t~landx
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    FROM REGUH AS R INNER JOIN LFA1       AS L  ON  R~LIFNR EQ L~LIFNR
                    INNER JOIN LFA1       AS L2 ON  R~EMPFG EQ L2~LIFNR
*                    LEFT OUTER JOIN t005t AS t  ON  l~land1  EQ t~land1
*                                                AND t~spras  EQ lv_langu
    WHERE R~LAUFD  IN R_LAUFD[]
    AND   R~LAUFI  IN R_LAUFI[]
    AND   R~XVORL  IN R_XVORL[]
    AND   R~LIFNR  IN R_LIFNR[]
    AND   R~ZBUKR  IN R_BUKRS[]
*    AND   r~empfg  EQ space
    AND   R~ZBNKS  IN R_ZBNKS[]
    AND   R~ZBNKN  IN R_ZBNKN[]
    AND   R~ZBNKL  IN R_ZBNKL[]
    AND   L~KTOKK  IN R_KTOKK[]
    AND   L~XCPDK  NE LV_XCPDK
    AND   L~VBUND  IN R_VBUND[]
    AND   L~STKZN  IN R_STKZN[]
    AND   L2~XCPDK NE LV_XCPDK.
* Select From LFA1 and REGUH by EMPFG
*  SELECT r~laufd r~laufi r~xvorl r~zbukr r~lifnr
*         r~kunnr r~empfg r~vblnr r~zbnks r~zbnkn r~zbnkl
*         l~lifnr AS payee l~land1 l~erdat l~lnrza l~loevm l~ktokk
*         l~xcpdk l~vbund l~stkzn
*         t~spras t~landx
*    APPENDING CORRESPONDING FIELDS OF TABLE lt_data
*    FROM reguh AS r INNER JOIN lfa1       AS l ON  r~empfg EQ l~lifnr
*                    LEFT OUTER JOIN t005t AS t ON  l~land1 EQ t~land1
*                                               AND t~spras EQ lv_langu
*    WHERE r~laufd IN r_laufd[]
*    AND   r~laufi IN r_laufi[]
*    AND   r~xvorl IN r_xvorl[]
*    AND   r~lifnr IN r_lifnr[]
*    AND   r~zbukr IN r_bukrs[]
*    AND   r~empfg NE space
*    AND   r~zbnks IN r_zbnks[]
*    AND   r~zbnkn IN r_zbnkn[]
*    AND   r~zbnkl IN r_zbnkl[]
*    AND   l~ktokk IN r_ktokk[]
*    AND   l~xcpdk EQ lv_xcpdk
*    AND   l~vbund IN r_vbund[]
*    AND   l~stkzn IN r_stkzn[].
**** Select LFA1 ******
**** Select LFA1 ******
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    CONCATENATE 'LS_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    CHECK  IS ASSIGNED.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-TIMLO
          D_TO        = SY-DATLO
          T_TO        = SY-TIMLO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          LS_DATA-DURATION  = TIME_DIFF .
        ELSE.
          LS_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE LT_DATA WHERE DURATION  NOT IN R_DURATION .
*********************************************************************************
  CHECK LT_DATA IS NOT INITIAL.
  SORT LT_DATA BY LIFNR ZBNKS ZBNKN ZBNKL.
* Get MD data of Vendor
  SELECT *
    FROM LFBK
    INTO TABLE LT_LFBK
    FOR ALL ENTRIES IN LT_DATA
    WHERE LIFNR EQ LT_DATA-LIFNR
    AND   BANKS EQ LT_DATA-ZBNKS
    AND   BANKL EQ LT_DATA-ZBNKL
    AND   BANKN EQ LT_DATA-ZBNKN.
*      AND   banks IN r_banks
*      AND   bankl IN r_bankl
*      AND   bankn IN r_bankn.
*      AND   kovon IN r_valid_from[]
*      AND   kobis IN r_valid_to[].
  IF LT_LFBK IS NOT INITIAL.
    SORT LT_LFBK BY LIFNR BANKS BANKL BANKN.
  ENDIF.
  LOOP AT LT_DATA INTO LS_DATA.
    CLEAR: LS_LIFNR.
    LS_LIFNR-LIFNR = LS_DATA-PAYEE.
    APPEND LS_LIFNR TO LT_LIFNR.
  ENDLOOP.
  IF LT_LIFNR IS NOT INITIAL.
    SORT LT_LIFNR BY LIFNR.
    DELETE ADJACENT DUPLICATES FROM LT_LIFNR COMPARING LIFNR.
  ENDIF.
*********************************************************************************
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX.
    CLEAR: T_DATA, LS_LFBK.
    READ TABLE LT_LFBK INTO LS_LFBK WITH KEY LIFNR = LS_DATA-LIFNR
                                             BANKS = LS_DATA-ZBNKS
                                             BANKL = LS_DATA-ZBNKL
                                             BANKN = LS_DATA-ZBNKN
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
* If bank account(for payment) is defined in MD, but have not validation
      IF LS_LFBK-KOVON IS NOT INITIAL AND LS_LFBK-KOBIS IS NOT INITIAL
           AND NOT ( LS_DATA-LAUFD BETWEEN LS_LFBK-KOVON AND LS_LFBK-KOBIS ).
        MOVE-CORRESPONDING LS_DATA TO T_DATA.
        MOVE-CORRESPONDING LS_LFBK TO T_DATA.
        T_DATA-BUKRS = LS_DATA-ZBUKR.
      ELSE.
        CONTINUE.
      ENDIF.
* If bank account(for payment) is not defined in MD
    ELSE.
      MOVE-CORRESPONDING LS_DATA TO T_DATA.
      T_DATA-BUKRS = LS_DATA-ZBUKR.
    ENDIF.
    IF T_DATA-LIFNR IS NOT INITIAL.
**    "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC_ENH'
        EXPORTING
          LIFNR        = T_DATA-LIFNR
        IMPORTING
          VENDOR_DESC  = T_DATA-NAME1
        TABLES
          ALL_ENTRIES  = LT_LIFNR
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
    IF T_DATA-PAYEE IS NOT INITIAL.
*    "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = T_DATA-PAYEE
        IMPORTING
          VENDOR_DESC  = T_DATA-NAME1_PAYEE
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
    IF T_DATA-BUKRS IS NOT INITIAL.
*    "--- Get  BUKRS Decription
      CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
        EXPORTING
          BUKRS          = T_DATA-BUKRS  " Company Code
        IMPORTING
          COMP_CODE_DESC = T_DATA-BUTXT  " Name of Company Code or Company
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
    ENDIF.
    IF T_DATA-LAND1 IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_COUNTRY_DESC'
        EXPORTING
          LAND1      = T_DATA-LAND1
          LANGU      = LV_LANGU
        IMPORTING
          LANDX      = T_DATA-LANDX
*         NATIO      =
*         LANDX50    =
*         NATIO50    =
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
*      IF <fs_data>-ekorg IS NOT INITIAL.
**   "-- EKORG_DESC
*        CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
*          EXPORTING
*            ekorg        = <fs_data>-ekorg
*            "LANGU              = lv_LANGU
*          IMPORTING
*            pur_org_desc = <fs_data>-ekotx
*          EXCEPTIONS
*            wrong_code   = 1
*            OTHERS       = 2.
*
*      ENDIF.
    APPEND T_DATA.
  ENDLOOP.
*    IF ls_data-matkl IS NOT INITIAL.
** Material group desc.
*      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
*      EXPORTING
*        matkl              = ls_data-matkl
*      IMPORTING
*        matkl_desc         = ls_data-wgbez
**       MATKL_DESC_L       =
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2
*        .
*    ENDIF.
**
*    IF ls_data-bsart IS NOT INITIAL AND ls_data-bstyp IS NOT INITIAL.
**    "-- BSART_DESC
*      CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
*      EXPORTING
*        bsart            = ls_data-bsart
*        langu            = lv_langu
*        bstyp            = ls_data-bstyp
*      IMPORTING
*        type_desc        = ls_data-batxt
*      EXCEPTIONS
*        wrong_code       = 1
*        OTHERS           = 2.
*    ENDIF.
*
*    IF ls_data-statu IS NOT INITIAL.
*      "-- STATU_DESC
*      lv_domname = 'ESTAK'.
*      lv_domvalue = ls_data-statu.
*
*      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
*      EXPORTING
*        i_domname        = lv_domname
*        i_domvalue       = lv_domvalue
*        langu            = lv_langu
**       SW_DEST          =
*      IMPORTING
*        e_ddtext         = lv_ddtext
*      EXCEPTIONS
*        not_exist        = 1
*        OTHERS           = 2.
*      IF sy-subrc = 0.
*        ls_data-statu_desc = lv_ddtext.
*      ENDIF.
*    ENDIF.
**
*    IF ls_data-bstyp IS NOT INITIAL.
**    "-- BSTYP_DESC
*      lv_domname = 'EBSTYP'.
*      lv_domvalue = <fs_data>-bstyp.
*
*      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
*      EXPORTING
*        i_domname        = lv_domname
*        i_domvalue       = lv_domvalue
*        langu            = lv_langu
**       SW_DEST          =
*      IMPORTING
*        e_ddtext         = lv_ddtext
*      EXCEPTIONS
*        not_exist        = 1
*        OTHERS           = 2.
*      IF sy-subrc = 0.
*        ls_data-bstyp_desc = lv_ddtext.
*      ENDIF.
*    ENDIF.
**
*    IF ls_data-lifnr IS NOT INITIAL.
**    "--- Get  Vendor Decriptions
*      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
*      EXPORTING
*        lifnr              = ls_data-lifnr
*      IMPORTING
*        vendor_desc        = ls_data-name1
*      EXCEPTIONS
*        wrong_vendor       = 1
*        OTHERS             = 2.
*
*    ENDIF.
**
*    IF ls_data-ekorg IS NOT INITIAL.
**   "-- EKORG_DESC
*      CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
*      EXPORTING
*        ekorg              = ls_data-ekorg
*      IMPORTING
*        pur_org_desc       = ls_data-ekotx
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2.
*
*    ENDIF.
**
**
*    IF ls_data-ekgrp IS NOT INITIAL.
**   "-- EKGRP_DESC
*      CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
*      EXPORTING
*        ekgrp              = ls_data-ekgrp
*      IMPORTING
*        pur_grp_desc       = ls_data-eknam
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2.
*    ENDIF.
*    APPEND ls_data TO t_data[].
*  LOOP AT t_data ASSIGNING <fs_data>.
*      CLEAR: lt_ret, ls_addr.
*      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*        EXPORTING
*          username = <fs_data>-username
*        IMPORTING
*          address  = ls_addr
*        TABLES
*          return   = lt_ret.
*
*      IF sy-subrc = 0.
*        <fs_data>-name_first = ls_addr-firstname.
*        <fs_data>-name_last  = ls_addr-lastname.
*      ENDIF.
*  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
