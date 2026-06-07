# Exception Indicator: MD: Vendors Master General Change Log ( SW_10_06_VEND_CHANGE)

## General Overview

This Exception Indicator monitors vendor master general-data change documents and returns change lines that match your selection, enriched with vendor descriptions and user details.

This EI serves as an essential control for procurement and vendor master governance by:

- Surfacing change-document activity on vendor objects within configurable time and scope filters
- Supporting segregation-of-duties and audit reviews of who changed which vendor fields and when
- Enabling detection of changes that exceed configured elapsed-time thresholds after the reference date field you configure
- Restricting results to vendors that still exist in general vendor master with matching account attributes
- Supporting both on-premise and cloud execution paths through optional destination routing

Typical use includes reviews after vendor onboarding, mass updates, or suspected unauthorized master data changes. Results are intended for exception workflows rather than full change-document extracts.

The routine calls the shared master-data change-log function, filters change lines by change-document date and change criteria, reconciles vendors with current general vendor master data, and calculates duration from the configured reference date field before alerting.


## Problem Description

Failure to monitor vendor master general-data change documents creates multiple risks across accounts payable, compliance, and master data stewardship.

**Procurement and Payment Risks**

- Unauthorized or erroneous vendor field changes may affect payment terms, bank data, or blocking indicators before the next payment run
- High volumes of undocumented changes can hide fraud or errors in vendor master updates
- Changes outside the intended monitoring window may be missed when reviews rely on ad hoc change-document display

**Master Data and Control Risks**

- Vendor changes without periodic exception reporting weaken evidence for internal control over master data maintenance
- Inability to filter by object class, table, field, user, or change type limits focused investigation of vendor-specific activity
- Vendor changes on objects that no longer match current vendor master criteria can clutter reviews unless reconciled with live master data

**Audit and Compliance Risks**

- Auditors expect traceability of vendor master changes by user, transaction, and time; unmonitored gaps undermine that evidence
- Lack of duration-based filtering can leave stale or low-priority changes in the same queue as recent high-risk items

## Suggested Resolution

**Immediate Response**

- Review flagged change lines for vendor object, field, user, change type, and dates shown in the exception
- Confirm with master data owners whether each change was authorized and documented per policy
- Prioritize changes to payment-relevant fields, blocking indicators, and bank-related tables

**System Assessment**

- Compare current exception volume to prior runs using the same date window and object-class filters
- Look for concentrations by user, transaction code, or field name that may indicate a project or interface issue
- Validate that the reference date field and duration settings match the business definition of how old a change must be to alert

**Corrective Actions**

- Correct erroneous vendor master data through standard maintenance with required approvals
- Tighten monitoring scope after root cause so the queue stays actionable
- Document review outcomes for audit trail and schedule recurring runs for vendor object classes in scope
- Escalate repeat unauthorized changes to security and procurement management for access or process review


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | CHANGE_IND | Appl. object change | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 3 | CHNGIND | Change Indicator | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 4 | CONVERT_KEY | 'X' - Decompose Key Field |  | 0 | 0 |  |  |
| 5 | COUNTER |  |  |  |  |  |  |
| 6 | DATE_REF_FLD |  |  |  |  |  |  |
| 7 | DATUM |  |  |  |  |  |  |
| 8 | DURATION |  |  |  |  |  |  |
| 9 | DURATION_D |  |  |  |  |  |  |
| 10 | DURATION_UNIT |  |  |  |  |  |  |
| 11 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 12 | HEADER_ONLY |  |  |  |  |  |  |
| 13 | KTOKK |  |  |  |  |  |  |
| 14 | LAND1 |  |  |  |  |  |  |
| 15 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 16 | LIFNR |  |  |  |  |  |  |
| 17 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 18 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 19 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 20 | STKZN |  |  |  |  |  |  |
| 21 | SW_DEST |  |  |  |  |  |  |
| 22 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 23 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 24 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 25 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 26 | VBUND |  |  |  |  |  |  |
| 27 | XCPDK |  |  |  |  |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 27 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**CHANGE_IND** (Appl. object change)

Header-level change indicator (insert/update/delete semantics) for the changed application object in change-document processing.

**CHNGIND** (Change Indicator)

Item-level change indicator on change-document item lines marking insert, update, delete, or key changes per field group.

**CONVERT_KEY** ('X' - Decompose Key Field)

<mark>Flag that determines whether the change log decomposes the compressed table key (TABKEY) into readable key components and converts technical KEY change lines into field-level key updates.
CONVERT_KEY Options:
X - decompose TABKEY into KEY1-KEY10, KEY1_V - KEY10_V, and KEY1_DS - KEY10_DS; convert KEY insert/delete lines into key-field change rows and remove raw FNAME = KEY lines where the key-change case applies.
Empty or blank - do not run key conversion; keep standard change-document lines and identify the changed object primarily via TABKEY (and OBJECTID where applicable).</mark>

**COUNTER** (COUNTER)

Runtime counter passed by the online monitor to identify the evaluation pass when multiple runs are coordinated.

**DATE_REF_FLD** (DATE_REF_FLD)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**DATUM** (DATUM)

Reference date range for the monitor; when populated, defines the calendar window applied to change-document **UDATE** filtering. When empty, **BACKDAYS** supplies the lower bound.

**DURATION** (DURATION)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_D** (DURATION_D)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in Days

**DURATION_UNIT** (DURATION_UNIT)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**FNAME** (Field Name)

Field name key in change documents used to filter by changed attribute.

**HEADER_ONLY** (HEADER_ONLY)

<mark>Flag that determines whether a result will display only header data or item details.
HEADER_ONLY Options:
X - return change-document headers only.
Empty or blank -include item-level changes.</mark>

**KTOKK** (KTOKK)

Account group (customer/vendor) used to segment master data governance rules.

**LAND1** (LAND1)

Country key used for legal/geographic segmentation of business partners or plants.

**LANGU** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (LIFNR)

Vendor account number used to scope records to supplier-specific flows.

**MANAGE_IN_UTC** ('X' - Manage in UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**OBJECTCLAS** (Change doc. object)

Change-document object class naming which SAP business object type the change log belongs to.

**OBJECTID** (Object value)

Change-document object value identifying the changed vendor (typically the vendor number for **LFA1** changes).

**STKZN** (STKZN)

Natural-person indicator on customer or vendor master distinguishing natural persons from legal entities.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TABNAME** (Table Name)

Database table name used to scope change/object monitoring to specific tables.

**TCODE** (Transaction Code)

SAP Transaction code

**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**USERNAME** (User)

<mark>User who posted the change.</mark>

**VBUND** (VBUND)

Trading partner/company field used for intercompany transaction analysis.

**XCPDK** (XCPDK)

One-time account indicator used to identify one-time customer/vendor postings.


### Parameter Relationships

How parameter combinations work together

**Change-log scope:** The shared change-log parameters (**OBJECTCLAS**, **OBJECTID**, **USERNAME**, **TCODE**, **CHANGE_IND**, **TABNAME**, **FNAME**, **CHNGIND**, **UDATE**, **CONVERT_KEY**, **HEADER_ONLY**, and related filters) define which change-document lines the underlying log function returns before vendor processing begins.

**Date window:** **DATUM** supplies explicit calendar bounds for the monitor date range when you populate it. When that range is empty, **BACKDAYS** builds a lower bound on **UDATE** so only change lines within the lookback window remain. **DATE_REF_FLD** names which date on each output row is used to compute **DURATION** relative to evaluation time; it does not replace the **UDATE** window built from **BACKDAYS** or **DATUM**.

**Vendor master reconciliation:** **LIFNR**, **LAND1**, **KTOKK**, **XCPDK**, **VBUND**, and **STKZN** limit which vendors remain after change lines on table **LFA1** are matched to current general vendor master records.

**Age filter:** **DURATION** with **DURATION_UNIT** (and **DURATION_D** when used) removes rows whose elapsed time from the reference date field does not fall in the selected duration band.

**Cloud routing:** **SW_DEST** selects the cloud destination path for remote user and vendor description lookups and for RFC vendor master reads when populated; when empty, on-premise function modules are used.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DATE_REF_FLD** - initial - treated as ERDAT by code
- **DURATION_UNIT** - initial - treated as D by code
- **LANGU** - initial - treated as E by code
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Vendor LFA1 changes in the last ten days**

**Purpose:** List vendor master change lines from the last ten days on table LFA1 with duration measured in days from the created-on reference date.

```
OBJECTCLAS = KRED
TABNAME = LFA1
BACKDAYS = 10
DURATION_UNIT = D
DATE_REF_FLD = ERDAT
```

**Use Case 2: Specific vendor and user in a fixed week**

**Purpose:** Review changes by one user on a single vendor across an explicit calendar window.

```
LIFNR = 0000100001
USERNAME = JSMITH
UDATE = 20250301
UDATE = 20250307
DURATION = 0
DURATION_UNIT = D
```

**Use Case 3: Field-level watch on account group**

**Purpose:** Monitor updates to vendor account group for domestic vendors over thirty days.

```
TABNAME = LFA1
FNAME = KTOKK
CHNGIND = U
LAND1 = US
BACKDAYS = 30
```

**Use Case 4: Changes exactly seven full days ago**

**Purpose:** Flag vendor changes whose reference date falls in the scope of exactly 7 full days ago when using full-day duration counting.

```
OBJECTCLAS = KRED
TABNAME = LFA1
DURATION = 7
DURATION_UNIT = F
DATE_REF_FLD = ERDAT
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_MD_CHNG_COUNT | ACT_CHNGNO | Document number | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGENR | Document number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGE_IND | Appl. object change | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHNGIND | Change Indicator | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CUKY_NEW | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | CUKY_OLD | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | FIELD_DESC | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY10_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY1_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY2_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY3_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY4_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY5_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY6_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY7_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY8_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KEY9_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | KTOKK | Account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | NAME_TEXT | Full Name | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECTCLAS | Change doc. object | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | OBJECT_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | PLANCHNGNR | Change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | RECORDS | Count (Int 4) | INT4(10) | /SKN/E_SW_COUNT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | STKZN | Natural Person | CHAR(1) | STKZN |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TABKEY | Table Key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TAB_DESC | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TCODE | Transaction Code | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | TEXT_CASE | Text flag | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UDATE | Date | DATS(8) | CDDATUM |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UNIT_NEW | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UNIT_OLD | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | USERNAME | User | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | UTIME | Time | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VALUE_NEW | New value | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VALUE_OLD | Old value | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | VBUND | Company ID | CHAR(6) | VBUND |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | WAS_PLANND | gen from plan. changes | CHAR(1) | CD_PLANNED |
| /SKN/S_SW_10_06_MD_CHNG_COUNT | XCPDK | One-time account | CHAR(1) | XCPDK |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_10_06_MD_CHG_VENDOR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_MD_CHNG_COUNT
*"----------------------------------------------------------------------
  INCLUDE /SKN/PC_SQL_DATA.
  TYPES: BEGIN OF TY_LFA1,
           LIFNR TYPE LFA1-LIFNR,
           LAND1 TYPE LFA1-LAND1,
           KTOKK TYPE LFA1-KTOKK,
           XCPDK TYPE LFA1-XCPDK,
           VBUND TYPE LFA1-VBUND,
           STKZN TYPE LFA1-STKZN,
         END OF TY_LFA1,
         TT_LFA1 TYPE TABLE OF TY_LFA1.
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: SW_DEST             RFCDEST,
               MANAGE_IN_UTC       CHAR1 ,
               LANGU               LANGU,
               BACKDAYS            INT4,
               DURATION_D          /SKN/E_SW_DURATION_D,
               DURATION_UNIT       /SKN/E_SW_DURATION_UNIT,
               DATE_REF_FLD        NAME_FELD,
               CONVERT_KEY         CHAR1,
               HEADER_ONLY         CHAR1.
  DATA_MULTY:   LIFNR             LIFNR,
                LAND1             LAND1_GP,
                XCPDK             XCPDK,
                VBUND             RASSC,
                KTOKK             KTOKK,
                STKZN             STKZN,
                OBJECTCLAS        CDOBJECTCL,
                OBJECTID          CDOBJECTV,
                USERNAME          CDUSERNAME,
                TCODE             CDTCODE,
                CHANGE_IND        CDCHNGINDH,
                TABNAME           TABNAME,
                DURATION          /SKN/E_SW_DURATION,
                FNAME             FIELDNAME,
                CHNGIND           CDCHNGIND,
                UDATE             CDDATUM,
                DATUM             SYDATUM,
                COUNTER           I.
  CONSTANTS: C_LFA1 TYPE TABNAME VALUE 'LFA1'.
  DATA: SY_DATLO LIKE SY-DATLO ,
        SY_TIMLO LIKE SY-TIMLO .
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: SY_TABIX  LIKE SY-TABIX,
        DATE_FROM LIKE SY-DATUM .
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
  DATA: LS_SEL_FIELDS TYPE /SKN/S_SEL_FIELDS.
  DATA: LS_DATA    LIKE LINE OF T_DATA[],
        LS_ADDR    TYPE BAPIADDR3,
        LS_DATA_MD TYPE /SKN/S_SW_10_06_MD_CHNG_LOG,
        LS_LFA1    TYPE TY_LFA1.
  DATA: LT_DATA_MD  TYPE TABLE OF /SKN/S_SW_10_06_MD_CHNG_LOG,
        LT_DATA_TMP LIKE TABLE OF T_DATA,
        LT_RET     TYPE TABLE OF BAPIRET2,
        LT_LFA1    TYPE TT_LFA1.
  FIELD-SYMBOLS: <FS_DATA>    LIKE LINE OF T_DATA[],
                 <FS_DATA_MD> LIKE LINE OF LT_DATA_MD,
                          TYPE ANY.
* Set default parameter
  LV_BACKDAYS       = 10.
  LV_DURATION_UNIT  = 'D'.
  LV_DATE_REF_FLD   = 'ERDAT'.
  LV_LANGU          = 'E'.
  SELECT_MULTY:  LIFNR,
                 LAND1,
                 XCPDK,
                 VBUND,
                 KTOKK,
                 STKZN,
                 OBJECTCLAS,
                 OBJECTID,
                 USERNAME,
                 TCODE,
                 CHANGE_IND,
                 TABNAME,
                 DURATION,
                 FNAME,
                 CHNGIND,
                 UDATE,
                 COUNTER,
                 DATUM .
  SELECT_SINGLE: SW_DEST,
                 LANGU,
                 MANAGE_IN_UTC,
                 BACKDAYS,
                 DATE_REF_FLD,
                 CONVERT_KEY,
                 DATE_REF_FLD,
                 DURATION_D,
                 DURATION_UNIT,
                 HEADER_ONLY.
* Configuration Alert
  CALL FUNCTION '/SKN/F_SW_10_06_MD_CHNG_LOG'
    IMPORTING
      IS_ALERT = IS_ALERT
    TABLES
      T_SELECT = T_SELECT
      T_DATA   = LT_DATA_MD.
* Check if found some change in configuration log
  CHECK IS_ALERT EQ 'X'.
  CLEAR: IS_ALERT.
  REFRESH: T_DATA.
* Select parameters for Select from LFA1
  SELECT_MULTY:  LIFNR,
                 LAND1,
                 XCPDK,
                 VBUND,
                 KTOKK,
                 STKZN.
  CONVERT_MULTY: LIFNR ALPHA.
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  IF R_DATUM[] IS INITIAL .  " Set default value
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF .
  DELETE LT_DATA_MD WHERE UDATE NOT IN R_DATUM[].
  CHECK LT_DATA_MD IS NOT INITIAL.
  SORT LT_DATA_MD BY OBJECTCLAS OBJECTID TABNAME FNAME.
  IF LV_SW_DEST IS NOT INITIAL.
    LOOP AT T_DATA INTO LS_DATA.
      RS_OBJECTID-SIGN   = 'I'.
      RS_OBJECTID-OPTION = 'EQ'.
      RS_OBJECTID-LOW    = LS_DATA-OBJECTID.
      APPEND RS_OBJECTID TO R_OBJECTID.
    ENDLOOP.
* Table List
* Select's Condition
    _RANGE_TO_SEL_TABLE 'LIFNR' LIFNR.
    _RANGE_TO_SEL_TABLE 'LAND1' LAND1.
    _RANGE_TO_SEL_TABLE 'KTOKK' KTOKK.
    _RANGE_TO_SEL_TABLE 'XCPDK' XCPDK.
    _RANGE_TO_SEL_TABLE 'VBUND' VBUND.
    _RANGE_TO_SEL_TABLE 'STKZN' STKZN.
    LT_OPTIONS[] = LT_OUT_WHERE_COND[].
* Selection Fields
*    lv_tabname   = 'LFA1'.
*    lv_structure = '/SKN/S_SW_10_06_MD_CHNG_COUNT'.
*
*    CALL FUNCTION '/SKN/F_SW_STRUCT_2_FIELDS'
*      EXPORTING
*        i_tabname   = lv_tabname
*        i_structure = lv_structure
*        sw_dest     = lv_sw_dest
*      TABLES
*        t_fields    = lt_fields[].
    LS_SEL_FIELDS-TABLE       = 'LFA1'.
* LIFNR
    LS_SEL_FIELDS-FIELD       = 'LIFNR'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
* LAND1
    LS_SEL_FIELDS-FIELD       = 'LAND1'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
* KTOKK
    LS_SEL_FIELDS-FIELD       = 'KTOKK'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
* XCPDK
    LS_SEL_FIELDS-FIELD       = 'XCPDK'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
* VBUND
    LS_SEL_FIELDS-FIELD       = 'VBUND'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
* STKZN
    LS_SEL_FIELDS-FIELD       = 'STKZN'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION LV_SW_DEST
      EXPORTING
        QUERY_TABLE          = LV_TABNAME
      TABLES
        OPTIONS              = LT_OPTIONS
        FIELDS               = LT_FIELDS
        DATA                 = LT_DATA_RFC
      EXCEPTIONS
        TABLE_NOT_AVAILABLE  = 1
        TABLE_WITHOUT_DATA   = 2
        OPTION_NOT_VALID     = 3
        FIELD_NOT_VALID      = 4
        NOT_AUTHORIZED       = 5
        DATA_BUFFER_EXCEEDED = 6
        OTHERS               = 7.
    IF SY-SUBRC <> 0.
      CLEAR LT_DATA_RFC[].
    ELSE.
      _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_LFA1 LT_FIELDS 1.
    ENDIF.
  ENDIF.
  IF LV_SW_DEST IS INITIAL.
    SELECT LIFNR LAND1 KTOKK XCPDK VBUND STKZN
      FROM LFA1
      INTO TABLE LT_LFA1
      WHERE LIFNR IN R_LIFNR
      AND   LAND1 IN R_LAND1
      AND   KTOKK IN R_KTOKK
      AND   XCPDK IN R_XCPDK
      AND   VBUND IN R_VBUND
      AND   STKZN IN R_STKZN.
  ENDIF.
  LOOP AT LT_DATA_MD INTO LS_DATA WHERE TABNAME EQ C_LFA1 .
    CLEAR: LV_LIFNR.
    LV_LIFNR = LS_DATA-OBJECTID.
    READ TABLE LT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = LV_LIFNR
                                    BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF LV_SW_DEST IS INITIAL.
        IF LS_DATA-USERNAME IS NOT INITIAL.
          CALL FUNCTION '/SKN/F_SW_01_GET_DETAILES'
            EXPORTING
              BNAME      = LS_DATA-USERNAME
            IMPORTING
              NAME_FIRST = LS_DATA-NAME_FIRST
              NAME_LAST  = LS_DATA-NAME_LAST
              NAME_TEXT  = LS_DATA-NAME_TEXT
            EXCEPTIONS
              NO_DATA    = 1
              OTHERS     = 2.
        ENDIF.
        IF LS_LFA1-LIFNR IS NOT INITIAL.
**    "--- Get  Vendor Decriptions
          CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
            EXPORTING
              LIFNR        = LS_LFA1-LIFNR
            IMPORTING
              VENDOR_DESC  = LS_DATA-OBJECT_DESC
            EXCEPTIONS
              WRONG_VENDOR = 1
              OTHERS       = 2.
*
        ENDIF.
      ELSE.
        IF LS_DATA-USERNAME IS NOT INITIAL.
          CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES'
            EXPORTING
              BNAME      = LS_DATA-USERNAME
              SW_DEST    = LV_SW_DEST
            IMPORTING
              NAME_FIRST = LS_DATA-NAME_FIRST
              NAME_LAST  = LS_DATA-NAME_LAST
              NAME_TEXT  = LS_DATA-NAME_TEXT
            EXCEPTIONS
              NO_DATA    = 1
              OTHERS     = 2.
        ENDIF.
        IF LS_DATA-LIFNR IS NOT INITIAL.
*    "--- Get  Vendor Decriptions
          CALL FUNCTION '/SKN/FC_SW_10_VENDOR_DESC'
            EXPORTING
              LIFNR        = LS_DATA-LIFNR
              SW_DEST      = LV_SW_DEST
            IMPORTING
              VENDOR_DESC  = LS_DATA-OBJECT_DESC
            EXCEPTIONS
              WRONG_VENDOR = 1
              OTHERS       = 2.
        ENDIF.
      ENDIF.
      APPEND LS_DATA TO T_DATA.
    ENDIF.
  ENDLOOP.
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY_TIMLO
          D_TO        = SY_DATLO
          T_TO        = SY_TIMLO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          T_DATA-DURATION  = TIME_DIFF .
        ELSE.
          T_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
******************************************************************************
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
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    CLEAR: LT_RET, LS_ADDR.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        USERNAME = <FS_DATA>-USERNAME
      IMPORTING
        ADDRESS  = LS_ADDR
      TABLES
        RETURN   = LT_RET.
    IF LS_ADDR IS NOT INITIAL.
      <FS_DATA>-NAME_FIRST = LS_ADDR-FIRSTNAME.
      <FS_DATA>-NAME_LAST  = LS_ADDR-LASTNAME.
    ENDIF.
  ENDLOOP.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
