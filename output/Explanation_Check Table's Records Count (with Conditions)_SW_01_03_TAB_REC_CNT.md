# Exception Indicator: Check Table's Records Count (with Conditions) - SW_01_03_TAB_REC_CNT

## General Overview

This Exception Indicator returns **how many rows** match optional Open SQL–style conditions on a named SAP table, using either a **local** RFC read or a **remote** destination when supplied. Each run produces one output line with the **table name**, the **record count**, the **logical destination** used for the description lookup, and a **short table description** in the requested **language**—giving monitoring teams a lightweight way to track table volume under consistent filters without pulling full table content.

The function builds **OPTIONS** from **COND**, calls a central RFC helper to obtain **ROWCOUNT**, then enriches the line with **TAB_DESC** via table-description function modules (remote-capable when a switch destination is configured and the callable wrapper exists).


## Problem Description

Volume and data-quality controls often need **record counts** under specific slices (company code, document type, status flags). Doing this manually per system is slow and inconsistent; ad-hoc counts rarely reuse the same condition text or destination, so **alerts** and **dashboards** lack a repeatable definition of “how many rows qualify.”

**Financial and Reporting Issues**

- Reconciliation and close processes that depend on “no more than N open items” or similar thresholds need a **stable, automatable** count—not a one-off SE16 browse.

**Operational / Control Risks**

- Wrong **RFC destination** or **table name** silently counts the wrong system or object; ambiguous **language** for descriptions makes triage harder for global teams.

**Management Visibility and Decision-Making Risks**

- Without a single EI output that pairs **TAB**, **RECORDS**, and **TAB_DESC**, leadership sees numbers without context on **which** technical table was measured.

## Suggested Resolution

**Immediate Actions**

- Configure **TAB** and **COND** for each control scenario; set **DEST** (or rely on the execution default) so counts always hit the intended application server. Maintain **LANGU** when descriptions must appear in a specific logon language.

**System Assessment**

- Validate counts against a known test table with a fixed **COND** fragment and compare to a manual RFC read count before promoting to production monitoring.

**Process Improvements**

- Catalog approved **COND** patterns per table and link them to monitoring policies so changes are reviewed like transportable code.

**Training**

- Teach operators how **RECORDS** relates to the **OPTIONS** built from **COND**, and when **RFC** / **DEST** must be aligned with your landscape naming.


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | COND | Where condition (Open SQL) |  | 0 | 0 |  |  |
| 2 | DEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 3 | LANGU | Language for Texts |  | 0 | 0 |  |  |
| 4 | RECORDS | Count (Int 4) | INT4 | 10 | 0 | /SKN/E_SW_COUNT |  |
| 5 | RFC | RFC Destination |  | 0 | 0 |  |  |
| 6 | TAB | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 7 | TAB_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 7 parameters listed in the Parameters Reference Table above.

**COND** (Where condition (Open SQL)):

Free-text fragments that become **OPTIONS** entries for the RFC table record-count call. Together they express the business slice (for example organizational unit, status, or document category) whose matching row count you want to evaluate.

**DEST** (RFC Destination):

Primary logical destination used when the run should execute the count (and optional remote description call) against a specific application server or system other than the local default.

**DEST and RFC Connection:**

**DEST** is read as the main destination for preparing the call context; **RFC** is maintained when your configuration layer distinguishes the **switch** destination used for callable remote wrappers from the primary **DEST** naming—align both with the same logical target when only one destination applies.

**LANGU** (Language for Texts):

Language key for resolving **TAB_DESC**. When not supplied, the run assumes the current **SY-LANGU** so descriptions match the session language.

**RECORDS** (Count (Int 4)):

The integer count returned for the table and **COND** combination on each run—the value written to the result line that monitoring consumes for thresholds and trending.

**RFC** (RFC Destination):

Secondary destination slot used in landscapes where remote-capable function wrappers are invoked with an explicit **SW_DEST** separate from the primary **DEST** label; keep it consistent with the logical system you intend for remote description and count helpers.

**TAB** (Table Name):

Technical name of the table whose rows are counted under the supplied **OPTIONS**.

**TAB_DESC** (Short text):

Repository short description of **TAB** in **LANGU**, populated after the count succeeds so reviewers see a human-readable object name next to the numeric **RECORDS** value.


### Parameter Relationships

- **TAB** identifies **which** object is counted; **COND** defines **which** rows qualify by building the **OPTIONS** passed into the RFC count.
- **DEST** and **RFC** jointly align with how your system resolves **local** versus **remote** calls to the count and table-description helpers; when only one destination is meaningful, keep both consistent with that target.
- **LANGU** controls **TAB_DESC** only; it does not change the count logic but must match the language reviewers expect next to **RECORDS**.
- **RECORDS** on the output line is the direct result of the count for the **TAB** + **COND** combination under the resolved destination context.
- **Example:** **TAB** = BKPF, **COND** restricts company code and posting status, **DEST** points to the central finance instance, **LANGU** = **E** — returns one line with **RECORDS** and English **TAB_DESC** for audit of open-item volume.


### Default Values

- **LANGU** — Default: session language (when not supplied before read, the function uses **SY-LANGU** for description lookup).

### Practical Configuration Examples

**Use case — Document header volume by company code**

**Purpose:** Monitor posted FI documents for a company code slice.

```
TAB = BKPF
COND = BUKRS = '1000' AND BSTAT = ' '
DEST =
LANGU = E
```

**Use case — Remote count on a satellite system**

**Purpose:** Compare table volume on a named RFC destination.

```
TAB = MARA
DEST = QASCLNT100
RFC = QASCLNT100
COND = MTART = 'FERT'
LANGU = D
```

**Use case — Minimal condition, language-specific description**

**Purpose:** Full-table count with French table text for a French review queue.

```
TAB = T001
COND =
LANGU = F
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_03_TAB_REC_COUNT | DEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_03_TAB_REC_COUNT | RECORDS | SW : Count (Int 4) | INT4(10) | /SKN/E_SW_COUNT |
| /SKN/S_SW_01_03_TAB_REC_COUNT | TAB | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_01_03_TAB_REC_COUNT | TAB_DESC | Explanatory short text | CHAR(60) | DDTEXT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_03_TAB_REC_COUNT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_03_TAB_REC_COUNT OPTIONAL
*"----------------------------------------------------------------------
DATA: R_COND TYPE RANGE OF SO_TEXT.
DATA: RS_COND LIKE LINE OF R_COND.
"--- Add Parameter for Compare State & Attr Compare String
DATA: LV_DEST TYPE RFCDEST,
      LV_TAB  TYPE TABNAME,
      LV_LANGU TYPE LANGU.
"------------------------------------------
DATA: LS_COND TYPE  RFC_DB_OPT.
DATA: LT_COND LIKE TABLE OF LS_COND.
"------------------------------------------
DATA : ROWCOUNT TYPE INT4.
DATA : SY_TABIX LIKE SY-TABIX .
DATA: QUERY_TABLE TYPE TABNAME,
      LS_OPTIONS  TYPE  RFC_DB_OPT,
      LT_OPTIONS LIKE TABLE OF LS_OPTIONS. " ,
DATA: LS_DATA LIKE LINE OF T_DATA.
**-- Fill Selection Option Tables
SELECT_SINGLE: DEST,
               TAB,
               LANGU.
IF LV_LANGU IS INITIAL.
  LV_LANGU = SY-LANGU.
ENDIF.
SELECT_MULTY: COND.
 DATA_SINGLE:   SW_DEST RFCDEST.
 SELECT_SINGLE: SW_DEST.
 DATA: LV_FM TYPE FUNCNAME.
 CLEAR IS_ALERT .
 REFRESH T_DATA.
 "---- Prepare Input Parameters (tables)
 REFRESH: LT_COND.
 "--- Fill Condition Criteria
 REFRESH LT_COND.
 CLEAR LS_COND.
 LOOP AT R_COND INTO RS_COND.
   LS_COND-TEXT = RS_COND-LOW.
   APPEND LS_COND TO LT_COND.
 ENDLOOP.
*  "--- Retrieve Table data Count
     QUERY_TABLE = LV_TAB.
   "--- Where Condition Prepare
   REFRESH LT_OPTIONS.
   LOOP AT LT_COND INTO LS_COND.
     MOVE-CORRESPONDING LS_COND TO LS_OPTIONS.
     APPEND LS_OPTIONS TO LT_OPTIONS.
   ENDLOOP.
     IF LV_SW_DEST IS NOT INITIAL.
       LV_FM = '/SKN/FC_SW_RFC_TABLE_REC_CNT'.
       CALL FUNCTION 'FUNCTION_EXISTS'
         EXPORTING
           FUNCNAME                 = LV_FM
         EXCEPTIONS
           FUNCTION_NOT_EXIST       = 1
           OTHERS                   = 2.
       IF SY-SUBRC = 0.
         CALL FUNCTION LV_FM " '/SKN/F_SW_RFC_TABLE_REC_CNT'
           EXPORTING
             QUERY_TABLE       = QUERY_TABLE
             SW_DEST           = LV_SW_DEST
           IMPORTING
             ROWCOUNT          = ROWCOUNT
           TABLES
             OPTIONS           = LT_OPTIONS
          EXCEPTIONS
            TABLE_NOT_AVAILABLE       = 1
            TABLE_WITHOUT_DATA        = 2
            OPTION_NOT_VALID          = 3
            OTHERS                    = 4.
       ENDIF.
     ELSE.
       LV_FM = '/SKN/F_SW_RFC_TABLE_REC_CNT'.
       CALL FUNCTION LV_FM " '/SKN/F_SW_RFC_TABLE_REC_CNT'
         EXPORTING
           QUERY_TABLE       = QUERY_TABLE
         IMPORTING
           ROWCOUNT          = ROWCOUNT
         TABLES
           OPTIONS           = LT_OPTIONS
        EXCEPTIONS
          TABLE_NOT_AVAILABLE       = 1
          TABLE_WITHOUT_DATA        = 2
          OPTION_NOT_VALID          = 3
          OTHERS                    = 4.
       ENDIF.
     IF SY-SUBRC <> 0.
* Implement suitable error handling here
     ENDIF.
    "--- Fill Output Table
    REFRESH T_DATA.
    CLEAR LS_DATA.
    LS_DATA-TAB = LV_TAB.
    LS_DATA-RECORDS = ROWCOUNT.
    LS_DATA-DEST = LV_SW_DEST.
    CLEAR LS_DATA-TAB_DESC.
    IF LV_SW_DEST IS NOT INITIAL.
      LV_FM = '/SKN/FC_SW_TABLE_DESC'.
      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          FUNCNAME                 = LV_FM
        EXCEPTIONS
          FUNCTION_NOT_EXIST       = 1
          OTHERS                   = 2.
      IF SY-SUBRC = 0.
        CALL FUNCTION LV_FM  "'/SKN/F_SW_TABLE_DESC'
          EXPORTING
            TABNAME        = LS_DATA-TAB
            LANGU          = LV_LANGU
            SW_DEST           = LV_SW_DEST
          IMPORTING
            TAB_DESC       = LS_DATA-TAB_DESC
          EXCEPTIONS
            WRONG_TABLE                       = 1
            NO_DESCRIPTION_FOR_LANGUAGE       = 2
            OTHERS                            = 3.
      ENDIF.
    ELSE.
      LV_FM = '/SKN/F_SW_TABLE_DESC'.
    CALL FUNCTION LV_FM  "'/SKN/F_SW_TABLE_DESC'
      EXPORTING
        TABNAME        = LS_DATA-TAB
        LANGU          = LV_LANGU
      IMPORTING
        TAB_DESC       = LS_DATA-TAB_DESC
      EXCEPTIONS
        WRONG_TABLE                       = 1
       NO_DESCRIPTION_FOR_LANGUAGE       = 2
       OTHERS                            = 3.
    ENDIF.
    IF SY-SUBRC = 0.
      APPEND LS_DATA TO T_DATA.
    ENDIF.
 DESCRIBE TABLE T_DATA LINES SY-TFILL .
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
