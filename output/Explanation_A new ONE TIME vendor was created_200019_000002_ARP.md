# Exception Indicator: Tables Content Check - SW_01_03_TAB_CONTENT

## General Overview

This Exception Indicator (EI) monitors table content in SAP systems via RFC to identify rows that meet configurable selection criteria, optional date-based filtering, and time-difference thresholds. It provides visibility into remote table data for content checks, enabling detection of records that require review—such as recently created or changed master data—within a configurable time window and field set.

This EI serves as an essential control for data quality and operational oversight by:
- Enabling detection of table rows that meet business-defined criteria (e.g. creation or change within a lookback period) for audit and compliance review
- Supporting identification of records by configurable field selection and optional conditions for focused exception monitoring
- Providing visibility into table content across RFC destinations for cross-system or central monitoring
- Enabling time-window and duration-based filtering so only relevant records are returned for management attention
- Supporting content checks (e.g. one-time vendor creation, master data changes) with configurable table, fields, and filters

This monitoring enables organizations to detect table content that requires review, support data quality and master data governance, and maintain visibility into configurable table-based exceptions. The EI is particularly valuable for content checks, RFC-based monitoring, and exception management.

The EI reads table data via RFC (RFC_READ_TABLE and remote table read) and applies optional date and time-difference logic to filter rows.


## Problem Description

Failure to monitor table content via configurable criteria creates multiple risks across data quality, operational management, and compliance:

**Data Quality and Reporting Issues**
- Unmonitored table content can lead to undetected master data or transactional records that require review or correction
- Late discovery of records that meet exception criteria (e.g. one-time vendor creation) may delay period-end or audit procedures
- Lack of visibility into table content across systems or tables can undermine data governance and reconciliation
- Missing or inconsistent application of date and time-window filters can produce irrelevant or incomplete result sets

**Operational and Control Risks**
- Table content checks without clear selection criteria may return large volumes of data without actionable focus
- Unreviewed records that meet time or condition criteria can mask data quality issues or unauthorized changes
- Missing RFC destination or table configuration can lead to wrong system or wrong table being monitored
- Inappropriate SKIPROWS or MAXROWS settings can omit critical rows or overload processing

**Management Visibility and Decision-Making Risks**
- Lack of table-content monitoring delays awareness of records that require business review or approval
- Unidentified patterns (e.g. by table, field, or time window) can lead to missed corrective actions and policy enforcement
- Absence of configurable field and condition logic limits the ability to tailor checks to specific business rules

## Suggested Resolution

**Immediate Response**
- Review the table rows flagged by the EI to understand scope (table, fields returned, and number of rows)
- Verify high-impact or sensitive records (e.g. vendor master, payment-related tables) to confirm legitimacy and authorization
- Check that the RFC destination, table name, and field list match the intended monitoring scope
- Identify business context for result volume: expected vs unexpected counts, date range, and condition logic

**System Assessment**
- Analyze the date and time parameters (lookback days, date reference field, time-difference unit) to ensure the monitoring window and duration filter match requirements
- Compare current result volumes to prior runs using the same table and criteria to spot trends or anomalies
- Examine field selection (FLD01–FLD10) and condition (COND) to ensure the right data is being read and filtered
- Validate SKIPROWS and MAXROWS so that critical rows are not skipped and result set size is appropriate
- Assess RFC destination and table availability to ensure connectivity and authorization

**Corrective Actions**
- If erroneous or unauthorized records are identified, correct source data via the appropriate transaction or master data maintenance
- Escalate access or authorization issues to basis and security for RFC and table permissions
- Update table name, field list, or conditions to align monitoring with current business rules
- Document content-check results and resolution for audit trail and schedule recurring EI runs for continuous visibility
- Configure alert routing to data owners or process owners for timely review of table content exceptions


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
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

How many days to look back from today. When a date reference field is used, the EI builds the default date filter so that only rows with a date in that field within the lookback window are returned. When no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window.

**COND** (Where condition (Open SQL)):

Optional Open SQL–style condition to filter table rows. Applied together with the date and time-difference filters. Restricts which rows are returned from the table.

**DATE_REF_FLD** (Date Ref Field Name):

Name of the date field in the table used for the date filter. When set, the EI filters rows so that this field falls within the lookback window (today minus BACKDAYS when no explicit date range is supplied). When not set, no date-based filtering is applied.

**DATE_REF_FLD Options:**
- Any date field (type D) in the table (TAB); values are table-specific. Common examples:
- **ERDAT**: Creation date.
- **BUDAT**: Posting date.
- **AEDAT**: Changed on.
- **GIDAT**: Planned date (e.g. inventory).
- **BLDAT**: Document date (or other table-specific date fields).

**DATE_REF_FLD and BACKDAYS Connection:**
DATE_REF_FLD determines which date column in the table is used for the time window. The selection window is built from today minus BACKDAYS when no explicit date range is supplied.

**DEST** (RFC Destination):

RFC destination used to read the table. When not set, the EI uses a default destination. Determines which system and connection are used for the table read.

**DURATION** (Duration In Time Units):

Time difference (in the unit given by TIME_DIFF_UNIT) used to filter rows when a time reference field is used. Rows are included only if the time difference between the reference date/time and the current time falls within the supplied range. Used together with TIME_REF_FLD and TIME_DIFF_UNIT.

**DURATION_UNIT** (Duration Unit):

Unit for duration and time-difference calculations (e.g. days, hours). Used with DATE_REF_FLD and TIME_REF_FLD for time-window and duration filtering.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours.
- **M**: Minutes.
- **S**: Seconds (or other unit as in code).

**FLD01 - FLD10** (Field Name - 1 – Field Name - 10):

Names of up to 10 fields to read from the table. Determines which columns are retrieved and populated in FLD01_V–FLD10_V in the output. Leave unused positions initial.

**FLD01_V - FLD10_V** (Short Description – Short Description):

Output values for the fields specified in FLD01–FLD10. Populated by the EI with the corresponding table column values for each returned row.

**MAXROWS** (Max rows to select (0 - All)):

Maximum number of rows to read from the table. When 0, all rows that match the filters are read (subject to system limits). Used to limit result set size.

**SKIPROWS** (Rows to skip from beginning):

Number of rows to skip at the start of the table before reading. Used for pagination or to skip a header or leading set of rows.

**TAB** (Table Name):

Name of the table to read via RFC. Determines the data source for the content check. Must be a table accessible from the RFC destination.

**TIME_DIFF** (Date/Time Difference(from now)):

Range of time difference (in TIME_DIFF_UNIT) from the current time used to filter rows when a time reference field is set. Only rows whose reference date/time falls within this range relative to the current time are returned. Works with TIME_REF_FLD and TIME_DIFF_UNIT.

**TIME_DIFF_UNIT** (Time Diff Unit (D/H/M/S)):

Unit for TIME_DIFF and duration (e.g. D = days, H = hours). Determines how the time-difference filter is interpreted.

**TIME_DIFF_UNIT Options:**
- **D**: Days.
- **H**: Hours.
- **M**: Minutes.
- **S**: Seconds (or as in code).

**TIME_REF_FLD** (Time Ref Field Name):

Name of the time field in the table used for time-difference filtering. When set together with DATE_REF_FLD and TIME_DIFF, only rows whose reference date/time is within the specified time difference from the current time are returned. When not set, time-difference filtering is not applied.

**TIME_REF_FLD Options:**
- Any time field (type T) in the table (TAB); values are table-specific. Common examples:
- **ERZET**: Creation time.
- **AEZET**: Change time.
- **UZEIT**: Time (or other table-specific time fields).


### Parameter Relationships

**Time and date parameters**
- **BACKDAYS**, **DATE_REF_FLD**, **TIME_REF_FLD**, **TIME_DIFF**, and **TIME_DIFF_UNIT** work together to define the time window for table content. When DATE_REF_FLD is set, the EI filters rows so that the date in that field falls within today minus BACKDAYS when no explicit date range is supplied. When TIME_REF_FLD is set, TIME_DIFF and TIME_DIFF_UNIT define the allowed time difference from the current time; only rows within that window are returned.
- **DURATION** and **DURATION_UNIT**: DURATION (in TIME_DIFF_UNIT) is used with the time reference field to filter rows by duration; DURATION_UNIT determines the unit for duration and time-difference calculations.

**Table and field selection**
- **TAB**, **FLD01 – FLD10**, and **COND**: TAB identifies the table to read; FLD01–FLD10 specify which columns to return (and populate FLD01_V–FLD10_V); COND adds optional Open SQL–style filtering. Together they define which table is read and which rows and columns are returned.
- **SKIPROWS** and **MAXROWS**: SKIPROWS skips leading rows; MAXROWS limits how many rows are read (0 = all). Together they control pagination and result set size.

**RFC and destination**
- **DEST**: Determines the RFC destination used for the table read; when not set, a default destination is used.


### Default Values

- **BACKDAYS** — Default: 1 (when not supplied); used to derive the default date filter when DATE_REF_FLD is set. When no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window.
- **DURATION_UNIT** — Default: D (days); used for duration and time-difference calculations when TIME_REF_FLD or DATE_REF_FLD is used.
- **SKIPROWS** — Default: initial (0); no rows are skipped at the start.
- **MAXROWS** — Default: initial (0); all rows matching the filters are read (subject to system limits).
- **DEST** — Default: when not supplied, the EI uses a default RFC destination for the table read.

**Note:** When DATE_REF_FLD is set and no explicit date range is supplied, the EI builds the default date filter from today and BACKDAYS. When TIME_REF_FLD is set, TIME_DIFF and TIME_DIFF_UNIT define the time-difference window.

### Practical Configuration Examples

**Use Case 1: Table content check – last 7 days by creation date**
```
TAB = (table name, e.g. LFA1)
DATE_REF_FLD = ERDAT
BACKDAYS = 7
FLD01 = LIFNR
FLD02 = NAME1
MAXROWS = 1000
```
**Purpose:** Monitor vendor master (LFA1) for records created in the last 7 days; return vendor number and name, limited to 1000 rows.

**Use Case 2: One-time vendor creation check**
```
TAB = LFA1
DATE_REF_FLD = ERDAT
BACKDAYS = 30
FLD01 = LIFNR
FLD02 = NAME1
FLD03 = ERSDA
COND = (optional condition)
DEST = (RFC destination)
```
**Purpose:** Identify vendors created in the last 30 days for one-time vendor review and audit.

**Use Case 3: Time-difference filter**
```
TAB = (table name)
DATE_REF_FLD = (date field)
TIME_REF_FLD = (time field)
TIME_DIFF = 0 to 24 (hours)
TIME_DIFF_UNIT = H
BACKDAYS = 1
```
**Purpose:** Return rows whose reference date/time is within the last 24 hours; suitable for near-real-time content checks.


## EI Function Structure

This table lists all output fields returned by the EI.

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

## ABAP Code

`bap
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
`