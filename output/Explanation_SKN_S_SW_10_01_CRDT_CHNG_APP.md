# Exception Indicator: Credit management approvers ( SW_10_01_CRDT_MNG_AP)

## General Overview

This Exception Indicator detects credit-management-related changes on sales documents and deliveries when the number of recorded changes within a monitoring window meets a configured minimum threshold, returning change-log detail enriched with document value and payer information.

This EI serves as an essential control for credit management governance by:

- Enabling detection of repeated or clustered changes on sales orders and deliveries tied to credit management approver updates
- Supporting review of who changed credit-related data, when changes occurred, and which fields were affected
- Providing document net value and payer context so reviewers can prioritize high-exposure cases
- Enabling filtering by change object type, user, transaction, table, and field for targeted follow-up
- Supporting recurring monitoring of change activity before period close or credit review cycles

Typical use includes credit approver change surveillance, segregation-of-duties review, and sampling of SD documents with elevated change frequency. Results are intended for exception workflows rather than operational change-document list reporting.

The routine reads change-document entries through the master-data change log framework, aggregates change counts per business object, enriches qualifying rows with sales order or delivery value and payer partner data, and raises an alert when at least one row remains after filtering.


## Problem Description

Failure to monitor credit management approver changes on sales documents creates multiple risks across credit control, revenue protection, and audit compliance:

**Credit and Sales Risks**

- Repeated or unauthorized changes to credit approver data can alter release authority without timely review
- High-value sales orders or deliveries with clustered changes may indicate manual override of credit decisions
- Undetected change activity on payer-related documents can leave exposure unmanaged before billing or delivery

**Operational Risks**

- Monitoring windows that are too narrow or too wide can miss recent changes or flood reviewers with noise
- Minimum change-count thresholds set too low can create alert fatigue; thresholds set too high can hide meaningful patterns
- Scope that is not aligned to relevant object types, users, or changed fields can mix irrelevant change lines into the review queue

**Control and Audit Risks**

- Weak change surveillance reduces evidence that credit approver updates were reviewed on a timely basis
- Lack of recurring exception review limits accountability for credit operations follow-up on suspicious change patterns
- Missing payer and document value context delays escalation of changes on commercially significant documents

## Suggested Resolution

**Immediate Response**

- Review flagged change lines for object type, document number, user, date, and changed field values
- Confirm with credit management whether repeated changes on the same document are authorized or require reversal
- Prioritize high net-value documents and payer accounts for immediate follow-up

**System Assessment**

- Validate monitoring window and minimum change-count settings against credit review cadence
- Tune object class, user, transaction, table, and field scope so results stay actionable
- Compare exception counts by user, object type, and time period to identify systematic gaps

**Corrective Actions**

- Correct unauthorized or erroneous changes through standard SD and credit management processes where review confirms action is required
- Adjust monitoring scope after cleanup so results reflect truly exceptional change patterns
- Document review outcomes and schedule recurring runs before credit review meetings or close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 3 | CHANGE_IND | Appl. object change | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 4 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 5 | CHANGECNT | minimal changes counter |  | 0 | 0 |  |  |
| 6 | CHANGENR | Document number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 7 | CHNG_COUNT | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 8 | CHNGIND | Change Indicator | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 9 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 10 | CONVERT_KEY | CHAR | 1 | 0 |  | XFELD |  |
| 11 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 12 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 13 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 14 | DURATION_D | NUMC | 6 | 0 | /SKN/E_SW_DURATION_D |  |  |
| 15 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 16 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 17 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 18 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 19 | MANAGE_IN_UTC | CHAR | 1 | 0 |  | XFELD |  |
| 20 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 21 | NAME2 | Name 2 | CHAR | 35 | 0 | NAME2_GP | NAME |
| 22 | NAME_FIRST | First name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 23 | NAME_LAST | Last name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 24 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 25 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 26 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 27 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 28 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 29 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 30 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 31 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 32 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 33 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 34 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 35 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 36 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 37 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 38 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 39 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 40 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 41 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 42 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 43 | VBELN | CHAR | 10 | 0 | VBELN | VBELN |  |
| 44 | VBELN2 | CHAR | 10 | 0 | VBELN | VBELN |  |
| 45 | VBELN3 | CHAR | 10 | 0 | VBELN | VBELN |  |
| 46 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 47 | WAS_PLANND | gen from plan. changes | CHAR | 1 | 0 | CD_PLANNED | XFLAG |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 47 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ACT_CHNGNO** (Document number)

Active change-document number on the business object while change recording is processed-ties rows to the current change document header key.

**BACKDAYS** (Back days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**CHANGE_IND** (Appl. object change)

Header-level change indicator (insert/update/delete semantics) for the changed application object in change-document processing.

**CHANGE_IND_DESC** (Domain name)

Text for the header change-indicator domain-human-readable meaning of CHANGE_IND codes in change analytics.

**CHANGECNT** (minimal changes counter)

Minimum number of change lines recorded for the same business object within the run; rows with a computed change count below this value are excluded before alerting.

**CHANGENR** (Document number)

Change-document number that uniquely identifies one posted change document for an application object.

**CHNG_COUNT** (Natural Number)

Computed count of change log lines aggregated for one business object during the run; compared against **CHANGECNT** to decide whether the object remains in the result set.

**CHNGIND** (Change Indicator)

Item-level change indicator on change-document item lines marking insert, update, delete, or key changes per field group.

**CHNGIND_DESC** (Domain name)

Text for the item change-indicator domain-readable expansion of CHNGIND values on change item rows.

**CONVERT_KEY** (CHAR)

<mark>Flag that determines whether the change log decomposes the compressed table key (TABKEY) into readable key components and converts technical KEY change lines into field-level key updates.
CONVERT_KEY Options:
X - decompose TABKEY into KEY1-KEY10, KEY1_V - KEY10_V, and KEY1_DS - KEY10_DS; convert KEY insert/delete lines into key-field change rows and remove raw FNAME = KEY lines where the key-change case applies.
Empty or blank - do not run key conversion; keep standard change-document lines and identify the changed object primarily via TABKEY (and OBJECTID where applicable).</mark>

**CUKY_NEW** (CUKY)

New currency key in change-log comparisons to detect currency master changes.

**CUKY_OLD** (CUKY)

Previous currency key in change-log comparisons for before/after analysis.

**DATUM** (DATS)

Explicit monitoring date range supplied by the online monitor; when empty, the evaluation window is built from **BACKDAYS** relative to the current day.

**Not in use**
**DURATION_D** (NUMC)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in Days

**FIELD_DESC** (Short Description)

<mark>Description that defines the purpose of a repository object or data element.</mark>

**FNAME** (Field Name)

Field name key in change documents used to filter by changed attribute.

**KUNNR** (Customer)

Customer account is used to scope records to specific customers across SD/FI flows.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** (CHAR)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**NAME1 - NAME2** (Name)

First and second name lines of the bill-to partner returned from payer partner lookup on the related sales document.

**NAME_FIRST** (First name)

<mark>First name of the user who posted the change.</mark>

**NAME_LAST** (Last name)

<mark>Last name of the user who posted the change.</mark>

**NAME_TEXT** (Full Name)

<mark>Full name of the user who posted the change.</mark>

**NETWR** (Net value)

Net value amount used for commercial threshold and anomaly checks.

**OBJECT_DESC** (Name)

Description of the referenced business/change object-readable label beside OBJECTCLAS or generic OBJECT keys.

**OBJECTCLAS** (Change doc. object)

Change-document object class naming which SAP business object type the change log belongs to.

**OBJECTID** (Object value)

Business object key from the change log—typically the sales document or delivery number whose credit-related changes are being monitored.

**PLANCHNGNR** (Change number)

Formal engineering/planning change number referencing a released engineering-change record tied to master updates.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TAB_DESC** (Short Description)

Short description of a DDIC table so technical table-name keys are readable in output.

**TABKEY** (Table Key)

Composite table key value used in change-document record identification.

**TABNAME** (Table Name)

Database table name used to scope change/object monitoring to specific tables.

**TCODE** (Transaction Code)

SAP Transaction code

**TEXT_CASE** (Text flag)

<mark>Output flag from the master-data change log that marks whether the change line is a language-dependent text change (descriptions/names in text context) rather than a normal non-text field change.
TEXT_CASE Options:
X - text change line (language-dependent / text-table style change).
Empty or blank - not flagged as a text change.</mark>

**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**UNIT_NEW** (Unit)

Unit of measure after change on quantity fields-pairs with NEW_VAL in old/new quantity comparisons on change items.

**UNIT_OLD** (Unit)

Unit of measure before change on quantity fields-pairs with OLD_VAL for before/after quantity analysis.

**USERNAME** (User)

<mark>User who posted the change.</mark>

**UTIME** (Time)

Update/change time used with UDATE for precise event windows.

**VALUE_NEW** (New value)

New value in change documents used for before/after comparison.

**VALUE_OLD** (Old value)

Old value in change documents used for before/after comparison.

**VBELN** (CHAR)

SD document number used as primary key for sales/billing/delivery documents.

**VBELN2 - VBELN3** (CHAR)

Internal document number ranges populated from change-log object identifiers to drive sales order and delivery lookups during result enrichment.

**WAERK** (Document Currency)

Document currency key used for value analysis in transaction currency.

**WAS_PLANND** (gen from plan. changes)

Planned-state indicator used to distinguish planned versus actual execution records.

### Parameter Relationships

**Monitoring window:** When no explicit calendar range is supplied, **BACKDAYS** builds the evaluation window relative to the current day before change log entries are read. **UDATE** filters change-document creation dates within that window.

**Change log scope:** **OBJECTCLAS**, **OBJECTID**, **USERNAME**, **TCODE**, **CHANGE_IND**, **TABNAME**, **FNAME**, and **CHNGIND** narrow which change-document lines are retrieved from the master-data change log.

**Minimum change threshold:** The routine groups change lines by **OBJECTID** and counts consecutive entries per object into **CHNG_COUNT**. Rows where **CHNG_COUNT** is below **CHANGECNT** are removed before alerting.

**Document enrichment:** For sales documents (**OBJECTCLAS** = VERKBELEG), **NETWR** and **WAERK** come from the sales order header. For deliveries (**OBJECTCLAS** = LIEFERUNG), net value is derived from delivery item quantities and related order line values.

**Payer context:** **KUNNR**, **NAME1**, and **NAME2** are filled from the bill-to partner on the related sales document when partner data is available.

**Key conversion:** When **CONVERT_KEY** is set, compressed table keys in change lines can be expanded into readable key components before results are returned.

**Age filter:** **DURATION_D** can further restrict rows based on elapsed days from the change date when configured.


### Default Values

- **BACKDAYS** - initial - treated as 7 by code
- **CHANGECNT** - initial - treated as 0 by code

### Practical Example of Parameter Configuration

**Use Case 1: Weekly credit approver change review**

**Purpose:** Review credit-related change activity on sales documents over the past seven days.

```
BACKDAYS = 7
OBJECTCLAS = VERKBELEG
UDATE = 20250101 - 20251231
```

**Use Case 2: Multiple changes on one delivery**

**Purpose:** Flag delivery documents where at least two change lines were recorded for the same object.

```
CHANGECNT = 2
OBJECTCLAS = LIEFERUNG
BACKDAYS = 14
UDATE = 20250101 - 20251231
```

**Use Case 3: Changes by a specific user**

**Purpose:** Monitor change activity posted by one user on sales documents within the default lookback window.

```
USERNAME = JSMITH
OBJECTCLAS = VERKBELEG
BACKDAYS = 7
TCODE = VA02
```

**Use Case 4: Field-level credit approver changes**

**Purpose:** Focus on changes to a specific field on a named table for credit management review.

```
FNAME = CMGRA
TABNAME = VBAK
OBJECTCLAS = VERKBELEG
BACKDAYS = 30
```

**Use Case 5: Exactly seven full days since change date**

**Purpose:** Return rows whose change date is exactly 7 full days ago for weekly follow-up on clustered changes.

```
DURATION_D = 7
CHANGECNT = 1
OBJECTCLAS = VERKBELEG
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_CRDT_CHNG_APP | ACT_CHNGNO | Change number of the document created by this change | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | CHANGE_IND | Application object change type (U, I, E, D) | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | CHNGIND | Change Type (U, I, S, D) | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | CHNG_COUNT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | CUKY_NEW | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | CUKY_OLD | Change documents, referenced currency | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | FIELD_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | KUNNR | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | NAME2 | Name 2 | CHAR(35) | NAME2_GP |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | NETWR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | OBJECTCLAS | Object class | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | OBJECT_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | PLANCHNGNR | Planned change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | TABKEY | Changed table record key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | TAB_DESC | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | TCODE | Transaction in which a change was made | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | TEXT_CASE | Flag: X=Text change | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | UNIT_NEW | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | UNIT_OLD | Change documents, unit referenced | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | VALUE_NEW | New contents of changed field | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | VALUE_OLD | Old contents of changed field | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_CRDT_CHNG_APP | WAS_PLANND | Flag that changes were generated from planned changes | CHAR(1) | CD_PLANNED |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_CRDT_CHNG_APP .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_CRDT_CHNG_APP OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: MANAGE_IN_UTC       CHAR1 ,
               LANGU               LANGU,
               BACKDAYS            INT4,
               CHANGECNT           INT4,
               CONVERT_KEY         CHAR1 .
  SELECT_SINGLE: LANGU,
                BACKDAYS,
                CHANGECNT.
  DATA_MULTY:     OBJECTCLAS        CDOBJECTCL,
                  OBJECTID          CDOBJECTV,
                  USERNAME          CDUSERNAME,
                  TCODE             CDTCODE,
                  CHANGE_IND        CDCHNGINDH,
                  TABNAME           TABNAME,
                  FNAME             FIELDNAME,
                  CHNGIND           CDCHNGIND,
                  UDATE             CDDATUM,
                  DURATION_D       /SKN/E_SW_DURATION_D,
                  DATUM            SYDATUM , " Paased by SW Online Monitor
                  VBELN            VBELN,
                  VBELN2           VBELN,"""9-7-19
                  VBELN3           VBELN . """9-7-19         vblen.
  SELECT_MULTY:  OBJECTCLAS,
                 OBJECTID,
                 USERNAME,
                 TCODE,
                 CHANGE_IND,
                 TABNAME,
                 FNAME,
                 CHNGIND,
                 UDATE,
                 DURATION_D,
                 DATUM .
  LV_LANGU = SY-LANGU.
  LV_BACKDAYS = 7.
  LV_CHANGECNT = 0.
  SELECT_SINGLE: LANGU,
                 MANAGE_IN_UTC,
                 BACKDAYS,
                 CONVERT_KEY.
  DATA: T_DATA_CHLOG TYPE TABLE OF /SKN/S_SW_10_06_MD_CHNG_LOG,
        LS_DATA_CHLOG LIKE LINE OF T_DATA_CHLOG,
        LS_PREV_REC LIKE LINE OF T_DATA,
        LV_CHNG_COUNT TYPE I,
        LS_DATA LIKE LINE OF T_DATA,
        SY_TABIX LIKE SY-TABIX,
        SY_TABIX2 LIKE SY-TABIX,
        REL_VAL TYPE F,
        TMP_CUR TYPE WAERK,
        DATE_FROM LIKE SY-DATUM,""""9-7-19
        DATE_TO LIKE SY-DATUM.",
  DATA: LV_KEY TYPE VBELN.
  TYPES : BEGIN OF  DELIVERY_DATA_TYPE,
            VBELN TYPE VBELN_VL,
            POSNR	TYPE POSNR_VL,
            VGBEL	TYPE VGBEL,
            VGPOS	TYPE VGPOS,
            LFIMG TYPE LFIMG,
            KWMENG TYPE KWMENG,
            NETWR	TYPE NETWR_AP,
            WAERK	TYPE WAERK,
          END OF DELIVERY_DATA_TYPE,
          BEGIN OF  VBPA_TYPE,
            VBELN TYPE VBELN_VL,
            POSNR	TYPE POSNR_VL,
            KUNNR TYPE KUNNR,
            NAME1	TYPE NAME1_GP,
            NAME2	TYPE NAME2_GP,
          END OF VBPA_TYPE.
  DATA:T_VBPA TYPE TABLE OF VBPA_TYPE,
       LS_VBPA LIKE LINE OF T_VBPA,
       T_DELIVERY TYPE TABLE OF DELIVERY_DATA_TYPE,
       LS_DELIVERY LIKE LINE OF T_DELIVERY,
       T_VBAK TYPE TABLE OF VBAK,
       LS_VBAK LIKE LINE OF T_VBAK.
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
FIELD-SYMBOLS: <RES_FIELDS> TYPE /SKN/S_SW_10_01_CRDT_CHNG_APP ,
               <FS_V> TYPE ANY .
"--- Run Cloud Mode -----
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_CRDT_CHNG_APP'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
   IF R_DATUM[] IS INITIAL .
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'BT' .
      IF LV_BACKDAYS > 0 .
        DATE_FROM = SY-DATUM - LV_BACKDAYS .
        DATE_TO = SY-DATUM .
      ELSE.
        DATE_FROM = SY-DATUM .
        DATE_TO = SY-DATUM - LV_BACKDAYS.
      ENDIF.
      RS_DATUM-LOW = DATE_FROM .
      RS_DATUM-HIGH = DATE_TO .
      APPEND RS_DATUM TO R_DATUM.
   ENDIF.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
   CALL FUNCTION '/SKN/F_SW_10_06_MD_CHNG_LOG'
*    IMPORTING
*      IS_ALERT       =
     TABLES
       T_SELECT       = T_SELECT
       T_DATA         = T_DATA_CHLOG
             .
*********************************************************************************
*-- Calculate the value of change counter
  CHECK NOT T_DATA_CHLOG[] IS INITIAL.
*  if t_data_chlog[] is not initial.
    REFRESH R_VBELN.
    CLEAR RS_VBELN.
    RS_VBELN-SIGN = 'I'.
    RS_VBELN-OPTION = 'EQ'.
    """"Comented 9-7-19
**    MOVE-CORRESPONDING rs_vbeln to rs_vbeln2.
**    MOVE-CORRESPONDING rs_vbeln to rs_vbeln3
**    .
    """""""
    SORT T_DATA_CHLOG BY OBJECTCLAS OBJECTID.
    CLEAR LS_PREV_REC.
    READ TABLE T_DATA_CHLOG INDEX 1 INTO LS_DATA_CHLOG.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING LS_DATA_CHLOG TO LS_PREV_REC.
      LS_PREV_REC-CHNG_COUNT = 1.
      LOOP AT T_DATA_CHLOG INTO LS_DATA_CHLOG.
        """Comented 9-7-19
**        clear rs_vbeln2-low.
**        clear rs_vbeln3-low.
        """""
        SY_TABIX = SY-TABIX .
          " filling the range for relevant document numbers
          RS_VBELN-LOW = LS_DATA_CHLOG-OBJECTID.
          IF LS_DATA_CHLOG-OBJECTCLAS = 'VERKBELEG'.
            APPEND RS_VBELN TO R_VBELN. "orders
          ELSE .
            """09-7-19 comented and copied from FC
**            rs_vbeln2-low = ls_data_chlog-objectid.
              MOVE-CORRESPONDING RS_VBELN TO RS_VBELN2.
              APPEND RS_VBELN2 TO R_VBELN2. " deliveries
          ENDIF.
          """"Comented 9-7-19, copy from FC
**          rs_vbeln3-low = ls_data_chlog-objectid.
           MOVE-CORRESPONDING RS_VBELN TO RS_VBELN3.
          "!!!!!!!!!!! somehow check overflow conversion  DUMP
          APPEND RS_VBELN3 TO R_VBELN3. "orders
        IF LS_DATA_CHLOG-OBJECTID = LS_PREV_REC-OBJECTID.
          LV_CHNG_COUNT = LV_CHNG_COUNT  + 1.
        ELSE.
          MOVE-CORRESPONDING LS_PREV_REC TO LS_DATA.
          LS_DATA-CHNG_COUNT = LV_CHNG_COUNT.
          APPEND LS_DATA TO T_DATA.
          MOVE-CORRESPONDING  LS_DATA_CHLOG TO LS_PREV_REC .
          LV_CHNG_COUNT = 1.
          CLEAR LS_DATA.
        ENDIF.
      ENDLOOP.
      SORT R_VBELN BY LOW.
      SORT R_VBELN2 BY LOW.
      SORT R_VBELN3 BY LOW.
      DELETE ADJACENT DUPLICATES FROM R_VBELN.
      DELETE ADJACENT DUPLICATES FROM R_VBELN2.
      DELETE ADJACENT DUPLICATES FROM R_VBELN3.
      MOVE-CORRESPONDING LS_PREV_REC TO LS_DATA.
      LS_DATA-CHNG_COUNT = LV_CHNG_COUNT.
      APPEND LS_DATA TO T_DATA.
      DELETE T_DATA WHERE CHNG_COUNT LT LV_CHANGECNT.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""
    REFRESH T_VBPA.
    IF R_VBELN3[] IS NOT INITIAL.
*      "retrieving KUNNRS from partners tab
      SELECT A~VBELN A~POSNR A~KUNNR B~NAME1 B~NAME2 INTO TABLE T_VBPA
         FROM VBPA AS A INNER JOIN KNA1 AS B ON
          A~KUNNR = B~KUNNR
        WHERE A~VBELN IN  R_VBELN3 AND
              PARVW = 'RG' AND
              POSNR = '000000'
        ORDER BY VBELN .
    ENDIF.
""""""""""""""""""""""""""""""""""""""""""""""""""""""
*      select * from vbak into table t_vbak
*      where vbeln in r_vbeln
*      order by vbeln  .
*    " retrieve deliveries
      " retrieving vbak relevant data
    REFRESH T_VBAK.
    IF R_VBELN[] IS NOT INITIAL.
      SELECT * FROM VBAK INTO TABLE T_VBAK
      WHERE VBELN IN R_VBELN
      ORDER BY VBELN  .
    ENDIF.
    REFRESH T_DELIVERY.
      " retrieving lips relevant data
    IF R_VBELN2[] IS NOT INITIAL.
      SELECT A~VBELN A~POSNR A~VGBEL A~VGPOS A~LFIMG B~KWMENG B~NETWR B~WAERK
        INTO CORRESPONDING FIELDS OF TABLE T_DELIVERY
         FROM LIPS AS A INNER JOIN VBAP AS B
          ON A~VGBEL = B~VBELN AND A~VGPOS = B~POSNR
        WHERE A~VBELN IN R_VBELN2 AND
              A~VGTYP = 'C'
        ORDER BY A~VBELN.
   ENDIF.
*  *****************************************************************************
    LOOP AT T_DATA ASSIGNING <RES_FIELDS>.
      "sy_tabix = sy-tabix .
      CASE <RES_FIELDS>-OBJECTCLAS.
        WHEN 'VERKBELEG'.
        LV_KEY = <RES_FIELDS>-OBJECTID.
        " reading the release value
        READ TABLE T_VBAK INTO LS_VBAK BINARY SEARCH WITH KEY
          VBELN = <RES_FIELDS>-OBJECTID.
        IF SY-SUBRC = 0.
          "case of sales document
          <RES_FIELDS>-NETWR = LS_VBAK-NETWR.
          <RES_FIELDS>-WAERK = LS_VBAK-WAERK.
        ENDIF.
        WHEN 'LIEFERUNG'.
          LV_KEY = <RES_FIELDS>-OBJECTID.
          READ TABLE T_DELIVERY INTO LS_DELIVERY BINARY SEARCH WITH KEY
            VBELN = <RES_FIELDS>-OBJECTID.
          IF SY-SUBRC = 0.
            " running trhrough delivery lines
            TMP_CUR = LS_DELIVERY-WAERK.
            SY_TABIX = SY-TABIX.
            CLEAR REL_VAL.
            LOOP AT T_DELIVERY INTO LS_DELIVERY FROM SY_TABIX.
              IF LS_DELIVERY-VBELN = <RES_FIELDS>-OBJECTID.
                 REL_VAL = REL_VAL + LS_DELIVERY-NETWR * LS_DELIVERY-LFIMG / LS_DELIVERY-KWMENG.
                  IF TMP_CUR <> LS_DELIVERY-WAERK .
                    TMP_CUR = '*'.
                  ELSE .
                    TMP_CUR = LS_DELIVERY-WAERK.
                  ENDIF.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
            <RES_FIELDS>-WAERK = TMP_CUR.
            <RES_FIELDS>-NETWR = REL_VAL.
          ENDIF.
        WHEN OTHERS .
          CONTINUE.
      ENDCASE.
      READ TABLE T_VBPA INTO LS_VBPA BINARY SEARCH WITH KEY VBELN = LV_KEY.
      IF SY-SUBRC = 0 .
        <RES_FIELDS>-KUNNR = LS_VBPA-KUNNR.
        <RES_FIELDS>-NAME1 = LS_VBPA-NAME1.
        <RES_FIELDS>-NAME2 = LS_VBPA-NAME2.
      ENDIF.
    ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
