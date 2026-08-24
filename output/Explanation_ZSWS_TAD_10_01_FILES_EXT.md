# Exception Indicator: Files analizing with extention (SD) ( TD_10_01_FILES_EXT)

## General Overview

This Exception Indicator monitors files in a configured local directory using standard file-system rules, then interprets each file name as a billing document number by taking the text before the file extension and enriches matching rows with billing header customer and price group data.

This EI serves as an essential control for SD billing file operations by:

- Scanning a local directory with configurable path, mask, size, and freshness rules through the shared system files monitoring function
- Deriving the billing document number from each file name prefix before the extension
- Validating derived billing documents against billing header data and clearing invalid references
- Enriching results with payer, sold-to party, and price group descriptions for reviewer context
- Supporting severity styling on file rows through state color and icon attributes from the underlying file scan

Typical use includes monitoring outbound billing extract folders, verifying that file names map to valid billing documents, and sampling files that fail size or age thresholds before downstream import or archive jobs run. Results are intended for exception workflows rather than full directory listings.

The routine delegates file retrieval to the system files function, parses billing document numbers from file names, validates against billing headers, enriches descriptive attributes, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor billing-related files on application servers creates multiple risks across SD billing, customer master data, and integration hand-offs:

**Billing and Customer Risks**

- Files whose names do not correspond to valid billing documents can indicate misnamed extracts or failed posting references
- Missing or stale files in agreed export folders can delay customer invoicing or downstream archive processes without structured review
- Incorrect payer or sold-to context on flagged files can hide commercial ownership gaps until manual investigation

**Operational Risks**

- Directory or mask settings misaligned with the actual export path can exclude recent files or scan the wrong folder
- Size and freshness thresholds that are too wide or too narrow can hide actionable anomalies or create reviewer fatigue
- Invalid billing document numbers embedded in file names can propagate into manual rework if not caught early

**Control and Audit Risks**

- Weak file monitoring reduces evidence that billing extracts were reviewed before customer distribution
- Lack of recurring exception review limits accountability for operations follow-up on missing or misnamed files
- Missing customer and price group context delays escalation of commercially significant cases

## Suggested Resolution

**Immediate Response**

- Review flagged files for directory path, file name, size, update timestamp, and derived billing document reference
- Confirm with billing operations whether the file name convention and billing document mapping are correct
- Prioritize missing, oversized, or stale files in production export paths for immediate follow-up

**System Assessment**

- Validate directory, mask, size, and age threshold settings against the agreed billing extract cadence
- Compare exception counts by folder, file pattern, and severity styling to identify systematic gaps
- Sample file names that fail billing header validation to confirm naming convention compliance

**Corrective Actions**

- Correct export paths, masks, or naming conventions through standard processes where review confirms action is required
- Adjust monitoring scope after cleanup so results reflect truly exceptional file cases
- Document review outcomes and schedule recurring runs before billing distribution or archive milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | DATE_SEL | Take Date from Monitor (X') |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 3 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 4 | FILE_DATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 5 | FILE_DIR | Net Directory | CHAR | 128 | 0 | LOCALFILE | LOCALFILE |
| 6 | FILE_MASK | File Mask (*.*) |  | 0 | 0 |  |  |
| 7 | FILE_NAME | File name | CHAR | 128 | 0 | LOCALFILE | LOCALFILE |
| 8 | FILE_SIZE | File Size (Bait) | INT4 | 10 | 0 | INT4 | INT4 |
| 9 | FILE_SIZE_KB | File Size (KB) | INT4 | 10 | 0 | INT4 | INT4 |
| 10 | FILE_SIZE_MB | File Size (MB) | INT4 | 10 | 0 | INT4 | INT4 |
| 11 | FILE_TIME | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 12 | KONDA | Price group | CHAR | 2 | 0 | KONDA | KONDA |
| 13 | KONDA_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 14 | KUNAG | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 15 | KUNAG_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 16 | KUNRG | Payer | CHAR | 10 | 0 | KUNRG | KUNNR |
| 17 | KUNRG_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 18 | LANGU | Description Language |  | 0 | 0 |  |  |
| 19 | MANAGE_IN_UTC | 'X'- in UTC |  | 0 | 0 |  |  |
| 20 | STATE_COLOR | Set State Color (R/Y/G) | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 21 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 22 | UPD_DATE | Upd Date |  | 0 | 0 |  |  |
| 23 | UPD_TIME | Upd Time |  | 0 | 0 |  |  |
| 24 | VBELN | Billing Document | CHAR | 10 | 0 | VBELN_VF | VBELN |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 24 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**DATE_SEL** (Take Date from Monitor (X'))

**Not in use**
**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit(D/H/M))

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**FILE_DATE** (Date)

For operations, date on FILE_DATE indicates whether a row belongs in the current monitoring pass versus historical noise.

**FILE_DIR** (Net Directory)

When harmonized with related filters, net directory on FILE_DIR isolates the highest-risk record families.

**FILE_MASK** (File Mask (*.*))

When tightened, file mask (*.*) (FILE_MASK) removes rows that would otherwise dilute attention from failing or stuck cases.

**FILE_NAME** (File name)

After data is read, lines are removed unless file name on FILE_NAME still satisfies the active multivalued selection.

**FILE_SIZE** (File Size (Bait))

<mark>File Size</mark>

**FILE_SIZE_KB** (File Size (KB))

<mark>File size expressed in kilobytes for readability; technical metric derived from byte counts.</mark>

**FILE_SIZE_MB** (File Size (MB))

<mark>File size expressed in megabytes for readability; technical metric derived from byte counts.</mark>

**FILE_TIME** (Time)

Separates cross-client noise from in-scope work when time on FILE_TIME correlates with client or user attributes.

**KONDA** (Price group)

Aligns exception volume with the chosen scope by testing price group via KONDA before alert evaluation.

**KONDA_DESC** (Description)

Uses description from the source context so only records with KONDA_DESC inside declared values proceed.

**KUNAG** (Sold-to party)

Sold-to party/customer field used for SD partner-role based filtering.

**KUNAG_DESC** (Name)

Gives auditors traceable criteria because name on KUNAG_DESC is applied consistently before any alert flag is raised.

**KUNRG** (Payer)

Payer/customer field used to analyze SD/FI records by billing responsibility.

**KUNRG_DESC** (Name)

Supports escalation where name on KUNRG_DESC signals ownership for follow-up between Basis and functional teams.

**LANGU** (Description Language)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** ('X'- in UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**STATE_COLOR** (Set State Color (R/Y/G))

State selector used for quick triage via color-coded processing outcomes.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional literals may exist where the framework extends the palette for neutral states.

**STATE_ICON** (State Icon)

Icon column paired with STATE_COLOR for UI/ALM rendering of status.

**UPD_DATE** (Upd Date)

Update date

**UPD_TIME** (Upd Time)

Update Time

**VBELN** (Billing Document)

SD document number used as primary key for sales/billing/delivery documents.

### Parameter Relationships

**File scan delegation:** **FILE_DIR**, **FILE_MASK**, **FILE_SIZE**, **FILE_SIZE_KB**, **FILE_SIZE_MB**, **UPD_DATE**, **UPD_TIME**, **DURATION**, and **DURATION_UNIT** are passed to the underlying system files function, which scans the local directory and returns matching file rows with size, timestamp, and severity attributes. **DURATION** with **DURATION_UNIT** is an additional filter applied after the initial file list is built, limiting rows by file age relative to the evaluation time.

**Billing document derivation:** **FILE_NAME** supplies the source name; the text before the first extension separator is interpreted as the billing document number and written to **VBELN** after numeric formatting.

**Billing validation:** Each derived billing document is checked against billing header data; **KONDA**, **KUNRG**, and **KUNAG** are populated when a matching header exists, and the billing document reference is cleared when no header is found.

**Description language:** **LANGU** controls the language used when loading **KONDA_DESC** from price group texts.

**Output severity:** **STATE_COLOR** and **STATE_ICON** reflect severity styling assigned during the underlying file scan for triage in monitoring consoles.


### Default Values

- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as M by code

### Practical Example of Parameter Configuration

**Use Case 1: Billing extract folder with default mask**

**Purpose:** Monitor all files in the billing export directory for size and freshness exceptions.

```
FILE_DIR = /interface/billing/out
FILE_MASK = *.*
DURATION = 60
DURATION_UNIT = M
```

**Use Case 2: Large file threshold**

**Purpose:** Flag files exceeding a configured size limit in megabytes.

```
FILE_DIR = /interface/billing/out
FILE_SIZE_MB = GT 50
FILE_MASK = *.pdf
```

**Use Case 3: PDF billing files only**

**Purpose:** Review PDF files whose names should map to billing documents.

```
FILE_DIR = /interface/billing/out
FILE_MASK = *.pdf
STATE_COLOR = R
```

**Use Case 4: Stale files by update date**

**Purpose:** Sample files not updated within the configured age window.

```
FILE_DIR = /interface/billing/out
DURATION = 24
DURATION_UNIT = H
FILE_MASK = *.*
```

**Use Case 5: Full-day age filter**

**Purpose:** Flag files whose scope is exactly 3 full days ago when DURATION_UNIT = F and DURATION = 3.

```
FILE_DIR = /interface/billing/out
DURATION = 3
DURATION_UNIT = F
FILE_MASK = *.*
```

**Use Case 6: Price group description in English**

**Purpose:** Return price group descriptions in English for reviewed billing file rows.

```
FILE_DIR = /interface/billing/out
LANGU = E
FILE_MASK = *.*
DURATION = 120
DURATION_UNIT = M
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| ZSWS_TAD_10_01_FILES_EXT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| ZSWS_TAD_10_01_FILES_EXT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| ZSWS_TAD_10_01_FILES_EXT | FILE_DATE | Date | DATS(8) | DATUM |
| ZSWS_TAD_10_01_FILES_EXT | FILE_DIR | Local file for upload/download | CHAR(128) | LOCALFILE |
| ZSWS_TAD_10_01_FILES_EXT | FILE_NAME | Local file for upload/download | CHAR(128) | LOCALFILE |
| ZSWS_TAD_10_01_FILES_EXT | FILE_SIZE | Natural Number | INT4(10) | INT4 |
| ZSWS_TAD_10_01_FILES_EXT | FILE_SIZE_KB | Natural Number | INT4(10) | INT4 |
| ZSWS_TAD_10_01_FILES_EXT | FILE_SIZE_MB | Natural Number | INT4(10) | INT4 |
| ZSWS_TAD_10_01_FILES_EXT | FILE_TIME | Time | TIMS(6) | UZEIT |
| ZSWS_TAD_10_01_FILES_EXT | KONDA | Price group (customer) | CHAR(2) | KONDA |
| ZSWS_TAD_10_01_FILES_EXT | KONDA_DESC | Description | CHAR(20) | BEZEI20 |
| ZSWS_TAD_10_01_FILES_EXT | KUNAG | Sold-to party | CHAR(10) | KUNAG |
| ZSWS_TAD_10_01_FILES_EXT | KUNAG_DESC | Name 1 | CHAR(35) | NAME1_GP |
| ZSWS_TAD_10_01_FILES_EXT | KUNRG | Payer | CHAR(10) | KUNRG |
| ZSWS_TAD_10_01_FILES_EXT | KUNRG_DESC | Name 1 | CHAR(35) | NAME1_GP |
| ZSWS_TAD_10_01_FILES_EXT | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| ZSWS_TAD_10_01_FILES_EXT | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| ZSWS_TAD_10_01_FILES_EXT | VBELN | Billing Document | CHAR(10) | VBELN_VF |

## ABAP Code

```abap
FUNCTION ZSWF_TAD_10_01_FILES_EXT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  ZSWS_TAD_10_01_FILES_EXT OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: LANGU  LANGU.
***             BACKDAYS INT4,
***             DATE_REF_FLD NAME_FELD,
***             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
***
***
*** lv_BACKDAYS = 3.
*** lv_DURATION_UNIT = 'D'.
 LV_LANGU = SY-LANGU.
***
*** lv_DATE_REF_FLD = 'AEDAT'. "PO Creation
 SELECT_SINGLE: LANGU.
***                BACKDAYS,
***                DATE_REF_FLD,
***                DURATION_UNIT.
***
***
***data_multy: FRGRL        FRGRL,
***            EBELN        EBELN,
***            BUKRS        BUKRS,
***            BSTYP        EBSTYP,
***            BSART        ESART,
***            EKORG        EKORG,
***            EKGRP        BKGRP,
***            FRGGR        FRGGR,
***            FRGSX        FRGSX,
***            FRGCO        FRGCO,
***
***            LIFNR        ELIFN,
***            RESWK        RESWK,
***            ZTERM        DZTERM,
***            ERNAM        ERNAM,
***
***            AEDAT        ERDAT,
***            BEDAT        EBDAT,
***
***            WAERS        WAERS,
***
***            PROCSTAT    MEPROCSTATE,
***
***            DATUM        sy-datum,
***
***            DURATION    /SKN/E_SW_DURATION,
***            PO_GRP_AMOUNT BPREI.
***
***select_multy:
***            FRGRL,
***            EBELN,
***            BUKRS,
***            BSTYP,
***            BSART,
***            EKORG,
***            EKGRP,
***            FRGGR,
***            FRGSX,
***            FRGCO,
***
***            LIFNR,
***            RESWK,
***            ZTERM,
***            ERNAM,
***
***            AEDAT,
***            BEDAT,
***
***            WAERS,
***
***            PROCSTAT,
***            DATUM,
***            DURATION,
***            PO_GRP_AMOUNT.
***
***convert_multy: EBELN ALPHA,
***               LIFNR ALPHA.
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LS_FILES TYPE /SKN/S_SW_01_04_FILES,
      LT_FILES LIKE TABLE OF LS_FILES.
DATA: LS_DATA LIKE LINE OF T_DATA.
DATA: LV_VBELN TYPE VBELN_VF.
DATA: LV_STR1 TYPE STRING,
      LV_STR2 TYPE STRING.
DATA: LV_FILE_NAME TYPE LOCALFILE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  REFRESH LT_FILES.
  CALL FUNCTION '/SKN/F_SW_01_04_FILES'
    IMPORTING
      IS_ALERT       = IS_ALERT
   TABLES
     T_SELECT       = T_SELECT
     T_DATA         = LT_FILES.
  LOOP AT LT_FILES INTO LS_FILES.
    MOVE-CORRESPONDING LS_FILES TO LS_DATA.
    LV_FILE_NAME = LS_FILES-FILE_NAME.
    SPLIT LV_FILE_NAME AT '.' INTO LV_STR1 LV_STR2.
   " find FIRST OCCURRENCE OF '.' in lv_FILE_NAME.
    LV_VBELN = LV_STR1.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT         = LV_VBELN
      IMPORTING
        OUTPUT        = LV_VBELN .
   "-- Check VBRLN
    LS_DATA-VBELN = LV_VBELN.
    APPEND LS_DATA TO T_DATA.
  ENDLOOP.
*********************************************************************************
***  "--- Check Release group - Release strategy combination
***  loop at t_data .
***    sy_tabix = sy-tabix .
***    CALL FUNCTION '/SKN/F_SW_10_REL_GRP_STRT_CHK'
***      EXPORTING
****       FRGOT                   = '2'
****       FRGCO                   =
***        frggr                   = t_data-FRGGR
***        FRGSX                   = t_data-FRGSX
***      IMPORTING
***        FRGCO                   = lv_FRGCO
****       WA                      =
***      TABLES
***        T_FRGCO                 = R_FRGCO
***      EXCEPTIONS
***        WRONG_COMBINATION       = 1
***        OTHERS                  = 2.
***    IF sy-subrc <> 0.
***      delete t_data index sy_tabix.
***    ENDIF.
***  endloop.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    SELECT SINGLE KONDA KUNRG KUNAG
    FROM VBRK
    INTO CORRESPONDING FIELDS OF  T_DATA
    WHERE VBELN = T_DATA-VBELN.
    IF SY-SUBRC <> 0.
      CLEAR T_DATA-VBELN.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*---  GET Descriptions
LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF T_DATA-KUNRG IS NOT INITIAL.
         SELECT SINGLE NAME1
         FROM KNA1
         INTO T_DATA-KUNRG_DESC
         WHERE KUNNR = T_DATA-KUNRG.
    ENDIF.
    IF T_DATA-KUNAG IS NOT INITIAL.
         SELECT SINGLE NAME1
         FROM KNA1
         INTO T_DATA-KUNAG_DESC
         WHERE KUNNR = T_DATA-KUNAG.
    ENDIF.
    IF T_DATA-KONDA IS NOT INITIAL.
       SELECT SINGLE VTEXT
         FROM T188T
         INTO T_DATA-KONDA_DESC
         WHERE KONDA = T_DATA-KONDA
         AND SPRAS =  LV_LANGU.
*
    ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
ENDLOOP.
*---
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
