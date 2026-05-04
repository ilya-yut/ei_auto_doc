# Exception Indicator: Tables Content Check - SW_01_03_TAB_CONTENT

### General Overview

## General Overview

This Exception Indicator reads a single SAP table **remotely** via RFC, applies optional Open SQL–style conditions and a date lower bound, and returns a result set with up to ten configurable columns plus duration context. It supports operational monitoring where teams need to confirm that specific rows in a central or satellite table still fall within a **lookback window**, match **elapsed-time** bands, and expose the **field values** and **short descriptions** needed for triage without opening each system manually.

The EI resolves **destination** (explicit RFC destination or default from the execution context), **table name**, **field list**, **row window** (skip and max rows), and optional **date** and **time** reference fields. When a date reference is maintained and typed as a date in the table metadata, the run can append a lower bound from **days backwards** and evaluate **time difference** against the evaluation clock using the configured **time-difference unit**. Output lines populate **TAB**, **DEST**, **DURATION**, **DURATION_UNIT**, and each requested **FLDnn** / **FLDnn_V** pair when the field exists in the remote read layout.

Typical use: compare a reference table’s recent rows against policy (e.g. records older than a threshold, or within a narrow freshness band), feed dashboards or alert workflows, and attach human-readable column headers from the repository short texts alongside raw field names.


### Problem Description

## Problem Description

Without this EI, teams must log on to each target system, run table displays or custom queries, and manually reconcile which rows violate freshness or duration rules—slow, error-prone, and hard to evidence in audits. Remote table content checks that combine **dynamic field selection**, **optional free-text conditions**, and **date/time-aware filtering** are easy to get wrong when rebuilt ad hoc (wrong destination, missing lower bound, inconsistent duration unit).

**Financial and Reporting Issues**

- Stale or out-of-window rows in reference tables can distort reconciliations and period-close checks when nobody systematically verifies remote content against the same clock and rules.

**Operational / Control Risks**

- Inconsistent application of lookback days, duration bands, and row limits allows exceptions to hide in plain sight; wrong RFC destination can silently read the wrong system copy.

**Management Visibility and Decision-Making Risks**

- Leadership lacks a standardized, repeatable extract that pairs **technical field names** with **short descriptions** and **duration context**, delaying decisions on data quality and remediation ownership.

### Suggested Resolution

## Suggested Resolution

**Immediate Actions**

- Configure **TAB**, **DEST**, and the **FLD01–FLD10** slots (plus **COND** when needed) so each run targets the correct logical system and column layout. Set **BACKDAYS** and **DATE_REF_FLD** when the control depends on a business-date column; add **TIME_REF_FLD**, **TIME_DIFF**, and **TIME_DIFF_UNIT** (or **DURATION** / **DURATION_UNIT** per your parameter mapping) when elapsed-time from “now” must fall in defined bands.

**System Assessment**

- Validate a sample run against a known table: confirm **SKIPROWS** / **MAXROWS** bound volume, and that output **DURATION** values align with the intended unit (days vs hours vs minutes).

**Process Improvements**

- Document approved combinations of destination, table, and field slots per control, and keep **COND** fragments reviewed so they remain aligned with table keys and indexes on the target system.

**Training**

- Train monitors on how **FLDnn_V** short texts map to **FLDnn** technical names so review queues interpret results without ABAP Dictionary lookups.


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backwards |  | 0 | 0 |  |  |
| 2 | COND | Where condition (Open SQL) |  | 0 | 0 |  |  |
| 3 | DATE_REF_FLD | Date Ref Field Name |  | 0 | 0 |  |  |
| 4 | DEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 5 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 6 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 7 | FLD01 | Field Name - 1 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 8 | FLD01_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 9 | FLD02 | Field Name - 2 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 10 | FLD02_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 11 | FLD03 | Field Name - 3 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 12 | FLD03_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 13 | FLD04 | Field Name - 4 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 14 | FLD04_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 15 | FLD05 | Field Name - 5 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 16 | FLD05_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 17 | FLD06 | Field Name - 6 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 18 | FLD06_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 19 | FLD07 | Field Name - 7 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 20 | FLD07_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 21 | FLD08 | Field Name - 8 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 22 | FLD08_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 23 | FLD09 | Field Name - 9 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 24 | FLD09_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 25 | FLD10 | Field Name - 10 | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 26 | FLD10_V | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 27 | MAXROWS | Max rows to select (0 - All) |  | 0 | 0 |  |  |
| 28 | SKIPROWS | Rows to skip from beginning |  | 0 | 0 |  |  |
| 29 | TAB | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 30 | TIME_DIFF | Date/Time Difference(from now) |  | 0 | 0 |  |  |
| 31 | TIME_DIFF_UNIT | Time Diff Unit (D/H/M/S) |  | 0 | 0 |  |  |
| 32 | TIME_REF_FLD | Time Ref Field Name |  | 0 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 32 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Days Backwards):

Whole-day count used with the configured date reference field to build the lower bound of the business-date window when no explicit date range is supplied for the monitoring extract.

**COND** (Where condition (Open SQL)):

Free-text Open SQL fragment appended to the remote read options for the table, after the optional automatic date clause. Use for organizational or functional slices (company code, plant, status) that must always apply.

**DATE_REF_FLD** (Date Ref Field Name):

Name of a date-type column in the target table whose presence triggers generation of a **GE** lower bound from **BACKDAYS** before the remote read, and—when the field is type **D**—serves as the calendar anchor for elapsed-time evaluation on each result row.

**DATE_REF_FLD Options:**

- **ERDAT**: Creation or entry date on many business objects
- **AEDAT**: Last change date where maintained
- **BUDAT**: Posting date in financial documents
- **LAEDA**: Last change on material segments when relevant
- **CPUDT**: Capture date on interface or batch headers when relevant
- **Any other date-type field** on the table that reflects the business moment for the control

**DATE_REF_FLD and BACKDAYS Connection:**

When **DATE_REF_FLD** is supplied and the field is known as a date on the table layout, **BACKDAYS** defines how far back from the evaluation date the dynamic lower bound starts. If the date reference is not used, that automatic lower bound is not built from this path.

**DEST** (RFC Destination):

Logical RFC destination for the remote read. When left blank at runtime, the function assumes the standard destination from the execution context so the table is still read from the intended application server.

**DURATION** (Duration In Time Units):

Multivalued numeric bands applied to the computed elapsed value returned for each row (after date and time anchors resolve), keeping only lines whose duration falls inside the configured intervals.

**DURATION_UNIT** (Duration Unit):

Unit in which elapsed time from the reference timestamp to the evaluation moment is expressed when duration filtering is active.

**DURATION_UNIT Options:**

- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:**

The multivalued **DURATION** criteria are evaluated against the duration computed in **DURATION_UNIT**; configure the unit before tuning numeric thresholds so intervals are interpreted in the intended business sense.

**FLD01** (Field Name - 1):

First output column: technical field name from the table whose value and description are carried on each result line when the name is maintained and found in the remote field catalog.

**FLD01_V** (Short Description):

Repository short text for **FLD01**, shown beside the technical name on the output line for readability.

**FLD02** (Field Name - 2):

Second configurable output column; same role as **FLD01** for the next slot.

**FLD02_V** (Short Description):

Short text for **FLD02**, aligned by slot.

**FLD03** (Field Name - 3):

Third configurable output column.

**FLD03_V** (Short Description):

Short text for **FLD03**.

**FLD04** (Field Name - 4):

Fourth configurable output column.

**FLD04_V** (Short Description):

Short text for **FLD04**.

**FLD05** (Field Name - 5):

Fifth configurable output column.

**FLD05_V** (Short Description):

Short text for **FLD05**.

**FLD06** (Field Name - 6):

Sixth configurable output column.

**FLD06_V** (Short Description):

Short text for **FLD06**.

**FLD07** (Field Name - 7):

Seventh configurable output column.

**FLD07_V** (Short Description):

Short text for **FLD07**.

**FLD08** (Field Name - 8):

Eighth configurable output column.

**FLD08_V** (Short Description):

Short text for **FLD08**.

**FLD09** (Field Name - 9):

Ninth configurable output column.

**FLD09_V** (Short Description):

Short text for **FLD09**.

**FLD10** (Field Name - 10):

Tenth configurable output column.

**FLD10_V** (Short Description):

Short text for **FLD10**.

**MAXROWS** (Max rows to select (0 - All)):

Upper cap on rows returned from the remote read; zero means no artificial cap beyond what the remote API allows.

**SKIPROWS** (Rows to skip from beginning):

Number of initial rows to skip after the remote read, useful for paging or ignoring header-like rows when the source layout requires it.

**TAB** (Table Name):

Technical table name read on the target system together with **DEST**; defines which dataset is scanned.

**TIME_DIFF** (Date/Time Difference(from now)):

Multivalued bands for the computed elapsed value from the row’s reference date/time to the evaluation clock; rows outside the bands are dropped when date checking is active.

**TIME_DIFF_UNIT** (Time Diff Unit (D/H/M/S)):

Unit passed into elapsed-time calculation for the row when a valid date anchor exists, matching the semantics used for duration filtering on the result path.

**TIME_DIFF_UNIT Options:**

- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**TIME_REF_FLD** (Time Ref Field Name):

Optional time-of-day field used with the date anchor to refine the starting timestamp for elapsed-time calculation when sub-day precision matters.

**TIME_REF_FLD Options:**

- **ERZET**: Creation time paired with creation date
- **AEZET**: Time of last change when maintained
- **CPUTM**: Computing time on batch or interface headers
- **UZEIT**: Time part stored separately from date when applicable
- **Any other time-type field** on the table that matches the business event clock

**DATE_REF_FLD, TIME_REF_FLD, TIME_DIFF, TIME_DIFF_UNIT, and DURATION Connection:**

When **DATE_REF_FLD** resolves to a date-type column, the row’s business date and optional **TIME_REF_FLD** define the start of the elapsed interval; **TIME_DIFF_UNIT** (and aligned **DURATION_UNIT** when used) defines how the difference to the evaluation moment is measured. **TIME_DIFF** (and **DURATION**) supply the multivalued bands that keep or discard the row. If no valid date anchor exists for a line, that duration path does not drive inclusion.


### Parameter Relationships

## Parameter Relationships

- **TAB** and **DEST** jointly identify **which** remote dataset is read; **FLD01**–**FLD10** (with **FLD01_V**–**FLD10_V**) define **which** columns appear on each output line when maintained.
- **COND** augments the remote options together with any **DATE_REF_FLD** / **BACKDAYS**-driven lower bound appended for the extract.
- **SKIPROWS** and **MAXROWS** bound **how many** physical rows are processed from the remote result set after the read returns.
- **DATE_REF_FLD**, optional **TIME_REF_FLD**, **TIME_DIFF_UNIT**, and multivalued **TIME_DIFF** work together to compute and filter elapsed time per row; **DURATION** and **DURATION_UNIT** align with the same duration semantics on the parameter side when both naming styles are exposed in your configuration layer.
- **Example:** **TAB** = MARA, **DEST** = production logical system, **FLD01**/**FLD01_V** = material number and short text, **DATE_REF_FLD** = LAEDA, **BACKDAYS** = 30, **TIME_DIFF** band = 0–24 hours in **TIME_DIFF_UNIT** **H** — focuses the control on recently changed materials within a one-day elapsed window.


### Default Values

## Default Values

- **BACKDAYS** — Default: `1` (applied when not supplied before the date lower bound is built).
- **TIME_DIFF_UNIT** — Default: `D` (days) when not supplied before single-value parameters are read.

### Practical Configuration Examples

## Practical Configuration Examples

**Use case — Recent material changes with key attributes**

**Purpose:** Flag materials touched in the last 14 days and show number and description for quick review.

```
TAB = MARA
DEST =
FLD01 = MATNR
FLD02 = MTART
DATE_REF_FLD = LAEDA
BACKDAYS = 14
MAXROWS = 500
SKIPROWS = 0
```

**Use case — Time-based slice on document headers**

**Purpose:** Keep only rows whose elapsed time from the business timestamp falls within a narrow hour band.

```
TAB = VBAK
COND = VKORG = '1000'
DATE_REF_FLD = ERDAT
TIME_REF_FLD = ERZET
TIME_DIFF_UNIT = H
BACKDAYS = 7
DURATION_UNIT = H
```

**Use case — Limited window after skipping noise rows**

**Purpose:** Read a large extract but evaluate only rows after an initial offset, with an explicit RFC target.

```
TAB = BSEG
DEST = MYRFC
SKIPROWS = 1000
MAXROWS = 2000
FLD01 = BUKRS
FLD02 = BELNR
FLD03 = GJAHR
COND = HKONT = '0000400000'
```


### EI Function Structure

## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_TAB_CONTENT | DEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_03_TAB_CONTENT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_03_TAB_CONTENT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD01 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD01_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD02 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD02_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD03 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD03_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD04 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD04_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD05 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD05_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD06 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD06_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD07 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD07_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD08 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD08_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD09 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD09_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_01_03_TAB_CONTENT | FLD10_V | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_03_TAB_CONTENT | TAB | Table Name | CHAR(30) | TABNAME |

### ABAP Code

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_03_TAB_CONTENT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_03_TAB_CONTENT OPTIONAL
*"----------------------------------------------------------------------
DATA: R_COND TYPE RANGE OF SO_TEXT.
DATA: RS_COND LIKE LINE OF R_COND.
" ---data_multy: COND SO_TEXT.
"--- Add Parameter for Compare State & Attr Compare String
"--- data_single: DEST RFCDEST.
DATA: LV_DEST TYPE RFCDEST,
      LV_TAB  TYPE TABNAME ,
      LV_FLD01 TYPE FIELDNAME,
      LV_FLD02 TYPE FIELDNAME,
      LV_FLD03 TYPE FIELDNAME,
      LV_FLD04 TYPE FIELDNAME,
      LV_FLD05 TYPE FIELDNAME,
      LV_FLD06 TYPE FIELDNAME,
      LV_FLD07 TYPE FIELDNAME,
      LV_FLD08 TYPE FIELDNAME,
      LV_FLD09 TYPE FIELDNAME,
      LV_FLD10 TYPE FIELDNAME.
DATA: LV_SKIPROWS TYPE SYTABIX,
      LV_MAXROWS TYPE SYTABIX.
"------------------------------------------
DATA: LS_FIELDS TYPE RFC_DB_FLD,
      LS_COND TYPE  RFC_DB_OPT,
      LS_TAB_FIELDS TYPE  RFC_DB_FLD ,
      LS_TAB_DATA TYPE TAB512.
DATA: LT_FIELDS LIKE TABLE OF LS_FIELDS,
      LT_COND LIKE TABLE OF LS_COND,
      LT_TAB_FIELDS LIKE TABLE OF LS_TAB_FIELDS ,
      LT_TAB_DATA LIKE TABLE OF LS_TAB_DATA.
DATA: LT_ALL_FIELDS LIKE TABLE OF LS_TAB_FIELDS,
      LS_OPTIONS_ALL TYPE  RFC_DB_OPT,
      LT_OPTIONS_ALL LIKE TABLE OF LS_OPTIONS_ALL,
      LS_DATA_ALL TYPE  TAB512,
      LT_DATA_ALL LIKE TABLE OF LS_DATA_ALL.
"------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA.
DATA: LV_TEMP TYPE STRING.
DATA: IND TYPE I.
DATA: LV_KEY_LEN TYPE I.
CONSTANTS: LC_IS_PROBLEM_ATTR(1) TYPE C VALUE 'X'.
DATA: LV_DISPLAY_ATTR(1) TYPE C,
      LC_SHIFT TYPE I.
*--- Date Fiels Accosoating ----
DATA : FLD(60) TYPE C .
DATA : REF_DATE TYPE D,
       REF_TIME TYPE T.
DATA : IS_CHECK_DATE(1) TYPE C.
DATA : IS_OUT(1) TYPE C.
DATA : TIME_DIFF TYPE  INT4 .
DATA : DATE_FROM LIKE SY-DATUM.
*FIELD-SYMBOLS:  TYPE ANY ,
*               <fs_v> TYPE ANY .
DATA_SINGLE: BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD,
             TIME_REF_FLD NAME_FELD,
             TIME_DIFF_UNIT CHAR1.
DATA_MULTY:  TIME_DIFF  INT4,
             DATUM     SY-DATUM.
 LV_BACKDAYS = 1.
 LV_TIME_DIFF_UNIT = 'D'.
 SELECT_SINGLE: BACKDAYS,
                DATE_REF_FLD,
                TIME_REF_FLD,
                TIME_DIFF_UNIT.
SELECT_MULTY: DATUM,
              TIME_DIFF.
*data : sy_datlo like sy-datlo ,
*       sy_timlo like SY-timlo .
*
*_set_sys_date_time lv_sw_dest sy_datlo sy_timlo.
   IF R_DATUM[] IS INITIAL .
     RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      DATE_FROM = SY-DATUM - LV_BACKDAYS .
***       DATE_FROM = sy_datlo - lv_BACKDAYS .
       RS_DATUM-LOW = DATE_FROM .
        APPEND RS_DATUM TO R_DATUM.
   ENDIF.
*--- Date Fiels Accosoating ----
DATA : SY_TABIX LIKE SY-TABIX .
DEFINE POPULATE_FIELD .
  " &1 - Field Index  (XX)
 CLEAR LS_FIELDS.
 IF LV_FLD&1 IS NOT INITIAL.
   LS_FIELDS-FIELDNAME = LV_FLD&1.
   APPEND LS_FIELDS TO LT_FIELDS.
  ENDIF.
END-OF-DEFINITION .
DEFINE POPULATE_OUTPUT_FIELD .
  " &1 - Field Index
* read table lt_TAB_DATA into ls_TAB_DATA index ls_KEY_OUT-LINE&1.
* if sy-subrc is initial.
   IND = &1.
   "read table lt_FIELDS into ls_FIELDS index ind. " &1.
   "if sy-subrc = 0.
   IF LV_FLD&1 IS NOT INITIAL.
     READ TABLE LT_TAB_FIELDS INTO LS_TAB_FIELDS
                   WITH KEY FIELDNAME = LV_FLD&1. " ls_FIELDS-FIELDNAME.
     IF SY-SUBRC = 0.
       LV_TEMP = LS_TAB_DATA-WA+LS_TAB_FIELDS-OFFSET(LS_TAB_FIELDS-LENGTH).
       LS_DATA-FLD&1_V = LV_TEMP.
       LS_DATA-FLD&1 = LS_FIELDS-FIELDNAME.
     ENDIF.
   ENDIF.
* endif.
END-OF-DEFINITION .
DEFINE POPULATE_OUTPUT_SET .
 POPULATE_OUTPUT_FIELD 01.
 POPULATE_OUTPUT_FIELD 02.
 POPULATE_OUTPUT_FIELD 03.
 POPULATE_OUTPUT_FIELD 04.
 POPULATE_OUTPUT_FIELD 05.
 POPULATE_OUTPUT_FIELD 06.
 POPULATE_OUTPUT_FIELD 07.
 POPULATE_OUTPUT_FIELD 08.
 POPULATE_OUTPUT_FIELD 09.
 POPULATE_OUTPUT_FIELD 10.
END-OF-DEFINITION .
DEFINE POPULATE_DATE_TIME_REF_FIELD .
  CLEAR : REF_DATE,
          REF_TIME,
          IS_CHECK_DATE.
   IF LV_DATE_REF_FLD IS NOT INITIAL.
     READ TABLE LT_TAB_FIELDS INTO LS_TAB_FIELDS
                   WITH KEY FIELDNAME = LV_DATE_REF_FLD.
     IF SY-SUBRC = 0.
       IF LS_TAB_FIELDS-TYPE = 'D'.
         LV_TEMP = LS_TAB_DATA-WA+LS_TAB_FIELDS-OFFSET(LS_TAB_FIELDS-LENGTH).
         REF_DATE = LV_TEMP.
         IS_CHECK_DATE = 'X'.
       ENDIF.
     ENDIF.
     REF_TIME = SY_TIMLO.  "!!!!
   ENDIF.
   IF LV_TIME_REF_FLD IS NOT INITIAL.
     READ TABLE LT_TAB_FIELDS INTO LS_TAB_FIELDS
                   WITH KEY FIELDNAME = LV_TIME_REF_FLD.
     IF SY-SUBRC = 0.
       IF LS_TAB_FIELDS-TYPE = 'T'.
         LV_TEMP = LS_TAB_DATA-WA+LS_TAB_FIELDS-OFFSET(LS_TAB_FIELDS-LENGTH).
         REF_TIME = LV_TEMP.
       ENDIF.
     ENDIF.
   ENDIF.
END-OF-DEFINITION .
"----------------------------------------------------
**-- Fill Selection Option Tables
SELECT_SINGLE: DEST,
               TAB,
               FLD01,
               FLD02,
               FLD03,
               FLD04,
               FLD05,
               FLD06,
               FLD07,
               FLD08,
               FLD09,
               FLD10,
               SKIPROWS,
               MAXROWS.
SELECT_MULTY: COND.
 DATA_SINGLE:   SW_DEST RFCDEST.
 SELECT_SINGLE: SW_DEST.
 IF LV_DEST IS INITIAL.
   LV_DEST = LV_SW_DEST.
 ENDIF.
DATA: SY_DATLO LIKE SY-DATUM,
      SY_TIMLO LIKE SY-UZEIT.
""_set_sys_date_time lv_sw_dest sy_datlo sy_timlo.
_GET_CURRENT_DATE_TIME ' ' LV_SW_DEST SY_DATLO SY_TIMLO.
 CLEAR IS_ALERT .
 REFRESH T_DATA.
 "---- Prepare Input Parameters (tables)
 REFRESH: LT_FIELDS,
          LT_COND.
  POPULATE_FIELD: 01,
                  02,
                  03,
                  04,
                  05,
                  06,
                  07,
                  08,
                  09,
                  10.
 "--- Fill Condition Criteria
 REFRESH LT_COND.
 CLEAR LS_COND.
 LOOP AT R_COND INTO RS_COND.
   LS_COND-TEXT = RS_COND-LOW.
   APPEND LS_COND TO LT_COND.
 ENDLOOP.
 "--- Date Condition
 IF LV_DATE_REF_FLD IS NOT INITIAL.
   CALL FUNCTION 'RFC_READ_TABLE'
     DESTINATION    LV_DEST
     EXPORTING
       QUERY_TABLE                = LV_TAB
*      DELIMITER                  = ' '
       NO_DATA                    = 'X'
*      ROWSKIPS                   = 0
*      ROWCOUNT                   = 0
     TABLES
       OPTIONS                    = LT_OPTIONS_ALL
       FIELDS                     = LT_ALL_FIELDS
       DATA                       = LT_DATA_ALL
    EXCEPTIONS
      TABLE_NOT_AVAILABLE        = 1
      TABLE_WITHOUT_DATA         = 2
      OPTION_NOT_VALID           = 3
      FIELD_NOT_VALID            = 4
      NOT_AUTHORIZED             = 5
      DATA_BUFFER_EXCEEDED       = 6
      OTHERS                     = 7.
   IF SY-SUBRC <> 0.
* Implement suitable error handling here
   ENDIF.
     READ TABLE LT_ALL_FIELDS INTO LS_TAB_FIELDS
           WITH KEY FIELDNAME = LV_DATE_REF_FLD.
     IF SY-SUBRC = 0.
       DATE_FROM = SY-DATUM - LV_BACKDAYS .
       CONCATENATE '''' DATE_FROM '''' INTO LV_TEMP.
       CONCATENATE LV_DATE_REF_FLD 'GE' LV_TEMP
          INTO LS_COND-TEXT SEPARATED BY ' '.
       IF LT_COND[] IS NOT INITIAL.
         CONCATENATE 'AND' LS_COND-TEXT INTO LS_COND-TEXT SEPARATED BY ' '.
       ENDIF.
       APPEND LS_COND-TEXT TO LT_COND.
     ENDIF.
 ENDIF.
  "--- Retrieve Table data
    CALL FUNCTION '/SKN/F_SW_READ_TAB_REMOTE'
      EXPORTING
        DEST                       = LV_DEST
        TAB_NAME                   = LV_TAB
        SKIPROWS                   = LV_SKIPROWS
        MAXROWS                    = LV_MAXROWS
      IMPORTING
        KEY_LEN                    = LV_KEY_LEN
      TABLES
        T_KEY_FIELDS               = LT_FIELDS
*       T_ATTR_FIELDS              =
        T_COND                     = LT_COND
        T_FIELDS                   = LT_TAB_FIELDS
        T_DATA                     = LT_TAB_DATA
      EXCEPTIONS
        TABLE_NOT_AVAILABLE        = 1
        TABLE_WITHOUT_DATA         = 2
        OPTION_NOT_VALID           = 3
        FIELD_NOT_VALID            = 4
        NOT_AUTHORIZED             = 5
        DATA_BUFFER_EXCEEDED       = 6
        UNDETECTED_PROBLEM         = 7
        OTHERS                     = 8.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    "--- Fill Output Table
    REFRESH T_DATA.
    LOOP AT LT_TAB_DATA INTO LS_TAB_DATA.
      CLEAR IS_OUT.
      CLEAR LS_DATA.
      LS_DATA-TAB = LV_TAB.
      POPULATE_OUTPUT_SET .
      POPULATE_DATE_TIME_REF_FIELD .
      IF IS_CHECK_DATE IS NOT INITIAL.
        IF REF_DATE NOT IN R_DATUM.
          CONTINUE.
        ENDIF.
        CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
          EXPORTING
            D_FROM            = REF_DATE
            T_FROM            = REF_TIME
            D_TO              = SY_DATLO
            T_TO              = SY_TIMLO
            TIME_UNIT         = LV_TIME_DIFF_UNIT " 'D'
          IMPORTING
            TIME_DIFF         = TIME_DIFF
          EXCEPTIONS
            WRONG_VALUE       = 1
            OTHERS            = 2    .
          IF SY-SUBRC = 0.
            IF TIME_DIFF NOT IN R_TIME_DIFF.
              CONTINUE.
            ENDIF.
          ENDIF.
      ENDIF.
      LS_DATA-DURATION = TIME_DIFF.
      LS_DATA-DURATION_UNIT = LV_TIME_DIFF_UNIT.
      LS_DATA-DEST = LV_DEST.
      APPEND LS_DATA TO T_DATA.
    ENDLOOP.
 DESCRIBE TABLE T_DATA LINES SY-TFILL .
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
