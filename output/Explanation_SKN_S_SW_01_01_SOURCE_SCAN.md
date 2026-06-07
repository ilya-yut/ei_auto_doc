# Exception Indicator: Scan ABAP Report Sources ( SW_01_01_SOURCE_SCAN)

## General Overview

This Exception Indicator reviews ABAP repository objects that appear on active transport tasks, loads their published source, and searches that source for configured text patterns so development governance teams can spot risky coding constructs before release.

This EI serves as an essential control for change management and secure development by:
- Surfacing repository programs, includes, function modules, and class methods tied to transport workflows when their source matches sensitive search terms
- Enabling detection of data-changing statements and similar constructs that may violate development standards or segregation-of-duties expectations
- Supporting transport coordinators and security reviewers with line-level context, object metadata, and request status for prioritized follow-up
- Helping audit teams demonstrate that technical changes were screened against agreed search policies before import to production
- Complementing manual code walkthroughs with repeatable, parameterized scans across object types and transport populations

Typical use includes pre-import checks for emergency transports, periodic reviews of workbench changes, and investigations after policy updates to forbidden statement lists. Results are intended for exception workflows rather than full repository exports.

The routine first resolves transport-linked technical objects, retrieves their source through standard repository services, applies pattern matching with optional table-name validation for database-changing statements, and raises an alert when qualifying lines remain after age filtering.


## Problem Description

Failure to monitor ABAP source on in-flight or recent transport objects for disallowed or high-risk coding patterns creates multiple risks across secure development, change governance, and compliance.

**Secure Development and Code Quality Risks**
- Unauthorized database-changing logic may reach production without independent review of the actual source lines
- Emergency or fast-track transports can bypass informal peer checks when automated source screening is absent
- Repeated use of sensitive statement types may go unnoticed until an incident or audit finding

**Change Management and Transport Risks**
- Coordinators lack a consolidated view tying transport requests, object types, and matching source lines before release
- Objects modified outside agreed packages or by unexpected owners are harder to detect without object and author scoping
- Released versus modifiable transport populations cannot be compared systematically when status filters are not applied consistently

**Audit and Accountability Risks**
- Evidence of pre-import technical review is weaker when search policies are not executed and retained on a schedule
- Investigations after a security event require manual SE80-style browsing instead of a repeatable exception list with line numbers

## Suggested Resolution

**Immediate Response**
- Review each flagged transport object together with the matching source line, request status, and last-changed metadata shown in the exception
- Confirm with the developer or transport owner whether the statement is approved, documented, or requires rework before import
- Hold or return transports that contain non-approved patterns until remediation or formal risk acceptance is recorded

**System Assessment**
- Compare this cycle to prior runs after search-policy changes, mass transport activity, or major release windows
- Look for concentrations by object type, package, author, or transport status to see whether one project or team drives most hits
- Validate that the search terms and age window still match the current development standard and release cadence

**Corrective Actions**
- Remove or refactor disallowed source through your standard development and transport process with required approvals
- Tighten monitoring scope after root cause so the queue stays actionable for transport operations
- Update written secure-coding guidance and developer training when the same pattern recurs across teams
- Route repeat systemic issues into defect or change management when repository scanning rules or transport paths require fixes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AS4USER | As4User | CHAR | 50 | 0 | AS4USER | AS4USER |
| 2 | AUTHOR | Author | CHAR | 50 | 0 | AUTHOR | AUTHOR |
| 3 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 4 | CDAT | Cdat | DATS | 8 | 0 | CDAT | CDAT |
| 5 | CNAM | Cnam | CHAR | 50 | 0 | CNAM | CNAM |
| 6 | CREATEDON | Createdon | DATS | 8 | 0 | CREATEDON | CREATEDON |
| 7 | DATUM | Datum | DATS | 8 | 0 | DATUM | DATUM |
| 8 | DEVCLASS | Devclass | CHAR | 50 | 0 | DEVCLASS | DEVCLASS |
| 9 | DURATION | Duration | INT4 | 10 | 0 | DURATION | DURATION |
| 10 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | DURATION_UNIT | DURATION_UNIT |
| 11 | INCLUDE | Include | CHAR | 40 | 0 | INCLUDE | INCLUDE |
| 12 | LANGU | Langu | CHAR | 1 | 0 | LANGU | LANGU |
| 13 | OBJNAME | Objname | CHAR | 40 | 0 | OBJNAME | OBJNAME |
| 14 | OBJTYPE | Objtype | CHAR | 4 | 0 | TROBJTYPE | TROBJTYPE |
| 15 | PGMID | Pgmid | CHAR | 4 | 0 | PGMID | PGMID |
| 16 | SRCSYSTEM | Srcsystem | CHAR | 50 | 0 | SRCSYSTEM | SRCSYSTEM |
| 17 | STATE | State | CHAR | 50 | 0 | STATE | STATE |
| 18 | STRING_SEARCH | String Search | CHAR | 255 | 0 | /SKN/E_SW_SOURCE_SCAN_STRING | /SKN/E_SW_SOURCE_SCAN_STRING |
| 19 | SUBC | Subc | CHAR | 50 | 0 | SUBC | SUBC |
| 20 | SW_DEST | Sw Dest | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 21 | TRFUNCTION | Trfunction | CHAR | 1 | 0 | TRFUNCTION | TRFUNCTION |
| 22 | TRKORR | Trkorr | CHAR | 20 | 0 | TRKORR | TRKORR |
| 23 | TRSTATUS | Trstatus | CHAR | 1 | 0 | TRSTATUS | TRSTATUS |
| 24 | UDAT | Udat | DATS | 8 | 0 | UDAT | UDAT |
| 25 | UNAM | Unam | CHAR | 50 | 0 | UNAM | UNAM |
| 26 | VERN | Vern | CHAR | 50 | 0 | VERN | VERN |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 26 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**AS4USER** (As4User)

<mark>User who last changed a repository object in CTS/SE11-style metadata used for ownership of technical changes.</mark>

**AUTHOR** (Author)

Combines with related filters so author on AUTHOR refines which records remain for duration or state checks.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**CDAT** (Cdat)

When harmonized with related filters, cdat on CDAT isolates the highest-risk record families.

**CNAM** (Cnam)

For operations, cnam on CNAM indicates whether a row belongs in the current monitoring pass versus historical noise.

**CREATEDON** (Createdon)

Allows phased rollout: first widen CREATEDON for createdon, then tighten thresholds once baseline noise is understood.

**DATUM** (Datum)

Gives auditors traceable criteria because datum on DATUM is applied consistently before any alert flag is raised.

**DEVCLASS** (Devclass)

ABAP package/development class used to scope technical object ownership.

**DURATION** (Duration)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**INCLUDE** (Include)

When harmonized with related filters, include on INCLUDE isolates the highest-risk record families.

**LANGU** (Langu)

Language key used for language-dependent texts and user-language filtering.

**OBJNAME** (Objname)

Supports operational control by evaluating objname through OBJNAME for each candidate record.

**OBJTYPE** (Objtype)

Reflects real administration where objtype on OBJTYPE is routinely restricted to a single productive client or object family.

**PGMID** (Pgmid)

Supports escalation where pgmid on PGMID signals ownership for follow-up between Basis and functional teams.

**SRCSYSTEM** (Srcsystem)

Allows phased rollout: first widen SRCSYSTEM for srcsystem, then tighten thresholds once baseline noise is understood.

**STATE** (State)

When harmonized with related filters, state on STATE isolates the highest-risk record families.

**STRING_SEARCH** (String Search)

Aligns exception volume with the chosen scope by testing string search via STRING_SEARCH before alert evaluation.

**SUBC** (Subc)

Separates cross-client noise from in-scope work when subc on SUBC correlates with client or user attributes.

**SW_DEST** (Sw Dest)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TRFUNCTION** (Trfunction)

Transport function code on CTS requests describing import, export, or repair actions on repository.

**TRKORR** (Trkorr)

Transport request or task id in CTS identifying a repository change package.

**TRSTATUS** (Trstatus)

Transport request status such as modifiable or released governing CTS workflow.

**UDAT** (Udat)

Prevents accidental global scans when udat (UDAT) is meant to stay within a controlled application slice.

**UNAM** (Unam)

Reduces false positives during peak windows by tightening unam through UNAM alongside state filters.

**VERN** (Vern)

For distributed landscapes, vern on VERN often anchors which application server or destination appears in results.


### Parameter Relationships

How parameter combinations work together

**Explicit calendar window versus default lookback:** **DATUM** supplies explicit calendar bounds on transport-related change dates when populated. When explicit dates are not provided, **BACKDAYS** is the fallback that builds the default backward window from the evaluation date before transport objects are collected.

**Age filter after selection:** **DURATION** with **DURATION_UNIT** is an additional filter applied after source lines are found: each hit must still fit the configured elapsed-time band measured from the transport change timestamp to the evaluation clock.

**Source pattern scope:** **STRING_SEARCH** works with object-type and transport filters (**OBJTYPE**, **OBJNAME**, **TRKORR**, **TRSTATUS**, **TRFUNCTION**, **AUTHOR**, **AS4USER**, and related repository attributes) so only relevant transports and technical objects are scanned for the configured text.

**Remote execution path:** **SW_DEST** enables evaluation against a connected system when populated, including domain text retrieval for transport status and function codes.

**Language context:** **LANGU** aligns descriptive texts loaded for transport domains with the language used during review.

**Final selection:** Both the date side (explicit **DATUM** or **BACKDAYS** fallback when applicable) and the **DURATION**/**DURATION_UNIT** age filter apply together with **STRING_SEARCH** and transport or object filters—rows must satisfy the active combination of date, duration, pattern, and scope conditions before they appear in the final alert population.


### Default Values

- **BACKDAYS** - 1
- **DURATION_UNIT** - H
- **LANGU** - E
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Daily scan for direct database changes**

**Purpose:** Catch MODIFY, UPDATE, INSERT, or DELETE patterns in recent workbench transports with a one-day lookback and hour-based aging.
```
BACKDAYS = 1
STRING_SEARCH = MODIFY
OBJTYPE = PROG
TRSTATUS = D
DURATION = 24
DURATION_UNIT = H
```

**Use Case 2: Full-day age band on released tasks**

**Purpose:** Review source hits on released transports where the last change is at least seven full days old, useful for post-release hygiene checks.
```
BACKDAYS = 14
TRSTATUS = R
DURATION = 7
DURATION_UNIT = F
AUTHOR = DEV*
```

**Use Case 3: Explicit change-date window**

**Purpose:** Limit the transport population to a known release weekend using explicit dates instead of the default lookback.
```
DATUM = 20250328 - 20250330
STRING_SEARCH = DELETE
TRKORR = DEVK*
```

**Use Case 4: Class method review in a package**

**Purpose:** Focus on class methods in selected development packages with English texts and a remote system destination.
```
OBJTYPE = METH
DEVCLASS = ZCUSTOM*
LANGU = E
SW_DEST = S4H_DEV
STRING_SEARCH = UPDATE
BACKDAYS = 3
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_SOURCE_SCAN | AS4USER | AS4USER | CHAR(50) | AS4USER |
| /SKN/S_SW_01_01_SOURCE_SCAN | AUTHOR | AUTHOR | CHAR(50) | AUTHOR |
| /SKN/S_SW_01_01_SOURCE_SCAN | BACKDAYS | BACKDAYS | INT4(10) | BACKDAYS |
| /SKN/S_SW_01_01_SOURCE_SCAN | CDAT | CDAT | DATS(8) | CDAT |
| /SKN/S_SW_01_01_SOURCE_SCAN | CNAM | CNAM | CHAR(50) | CNAM |
| /SKN/S_SW_01_01_SOURCE_SCAN | CREATEDON | CREATEDON | DATS(8) | CREATEDON |
| /SKN/S_SW_01_01_SOURCE_SCAN | DATUM | DATUM | DATS(8) | DATUM |
| /SKN/S_SW_01_01_SOURCE_SCAN | DEVCLASS | DEVCLASS | CHAR(50) | DEVCLASS |
| /SKN/S_SW_01_01_SOURCE_SCAN | DURATION | DURATION | INT4(10) | DURATION |
| /SKN/S_SW_01_01_SOURCE_SCAN | DURATION_UNIT | DURATION_UNIT | CHAR(1) | DURATION_UNIT |
| /SKN/S_SW_01_01_SOURCE_SCAN | INCLUDE | INCLUDE | CHAR(40) | INCLUDE |
| /SKN/S_SW_01_01_SOURCE_SCAN | LANGU | LANGU | CHAR(1) | LANGU |
| /SKN/S_SW_01_01_SOURCE_SCAN | OBJNAME | OBJNAME | CHAR(40) | OBJNAME |
| /SKN/S_SW_01_01_SOURCE_SCAN | OBJTYPE | OBJTYPE | CHAR(4) | TROBJTYPE |
| /SKN/S_SW_01_01_SOURCE_SCAN | PGMID | PGMID | CHAR(4) | PGMID |
| /SKN/S_SW_01_01_SOURCE_SCAN | SRCSYSTEM | SRCSYSTEM | CHAR(50) | SRCSYSTEM |
| /SKN/S_SW_01_01_SOURCE_SCAN | STATE | STATE | CHAR(50) | STATE |
| /SKN/S_SW_01_01_SOURCE_SCAN | STRING_SEARCH | STRING_SEARCH | CHAR(255) | /SKN/E_SW_SOURCE_SCAN_STRING |
| /SKN/S_SW_01_01_SOURCE_SCAN | SUBC | SUBC | CHAR(50) | SUBC |
| /SKN/S_SW_01_01_SOURCE_SCAN | TRFUNCTION | TRFUNCTION | CHAR(1) | TRFUNCTION |
| /SKN/S_SW_01_01_SOURCE_SCAN | TRKORR | TRKORR | CHAR(20) | TRKORR |
| /SKN/S_SW_01_01_SOURCE_SCAN | TRSTATUS | TRSTATUS | CHAR(1) | TRSTATUS |
| /SKN/S_SW_01_01_SOURCE_SCAN | UDAT | UDAT | DATS(8) | UDAT |
| /SKN/S_SW_01_01_SOURCE_SCAN | UNAM | UNAM | CHAR(50) | UNAM |
| /SKN/S_SW_01_01_SOURCE_SCAN | VERN | VERN | CHAR(50) | VERN |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_01_01_SOURCE_SCAN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SOURCE_SCAN OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_OBJECT,
* TADIR
           PGMID      TYPE PGMID,
           OBJECT     TYPE TROBJTYPE,
           OBJ_NAME   TYPE SOBJ_NAME,
           SRCSYSTEM  TYPE SRCSYSTEM,
           AUTHOR     TYPE RESPONSIBL,
           DEVCLASS   TYPE DEVCLASS,
           CREATED_ON TYPE CREATIONDT,
* PROGDIR
           STATE      TYPE STATE,
           SUBC       TYPE SUBC,
           APPL       TYPE RDIR_APPL,
           CNAM       TYPE CNAM,
           CDAT       TYPE RDIR_CDATE,
           UNAM       TYPE UNAM,
           UDAT       TYPE RDIR_UDATE,
           VERN       TYPE VERN,
* D010INC
           INCLUDE    TYPE INCLUDE,
         END OF TY_OBJECT,
         TT_OBJECT TYPE STANDARD TABLE OF TY_OBJECT.
  DATA_SINGLE: DURATION_UNIT   /SKN/E_SW_DURATION_UNIT ,
               BACKDAYS        INT4,
               LANGU           LANGU,
               SW_DEST         RFCDEST.
  DATA_MULTY:
* TADIR
              PGMID        PGMID,              " Program Id
              OBJTYPE      TROBJTYPE,          " Object Type - FUGR/CLAS/PROG
              OBJNAME      SOBJ_NAME,          " Object Name
              SRCSYSTEM    SRCSYSTEM,          " Original System
              AUTHOR       RESPONSIBL,         " Person Responsible
              DEVCLASS     DEVCLASS,           " Package
              CREATEDON    CREATIONDT,         " Creation Date
*              domnam       tmsdomnam,          "
*              sysnam       tmssysnam,          " System Name(For)
* PROGDIR
              STATE        R3STATE,            " Object Status
              SUBC         SUBC,               " Program type
              CNAM         CNAM,               " Created by
              CDAT         RDIR_CDATE,         " Created on
              UNAM         UNAM,               " Last changed by
              UDAT         RDIR_UDATE,         " Last changed on
              VERN         VERN,               " Version Number
* D010INC/E071
              INCLUDE      INCLUDE,            " Include
              TRKORR       TRKORR,             " Request/task
*              strkorr      strkorr,            " High level request
*              maxrc        trretcode,
*              int_maxrc    int1,
*              tarcli       trtarcli,           " Targ Clnt
*              comsys       tmssysnam,          " System Name(from)
*              srccli       trclient,           " Source client
              STRING_SEARCH /SKN/E_SW_SOURCE_SCAN_STRING,  " String Serach
              TRSTATUS      TRSTATUS,                      " Status of request/task
              TRFUNCTION    TRFUNCTION,                    " Type of request/task
              AS4USER       TR_AS4USER,
*              object        trobjtype,
*              obj_name      trobj_name,
*              state_color   /skn/e_sw_state_color,
              DATUM         SY-DATUM,
              DURATION     /SKN/E_SW_DURATION.
  LV_DURATION_UNIT = 'H'.
  LV_LANGU         = 'E'.
  LV_BACKDAYS      = 1.
  SELECT_SINGLE: BACKDAYS,
                 DURATION_UNIT,
                 LANGU,
                 SW_DEST.
  SELECT_MULTY:
* TADIR
                PGMID,     " Program id
                OBJTYPE,   " Object type
                OBJNAME,   " Object name
                SRCSYSTEM, " Original system
                AUTHOR,    " Person responsible
                CREATEDON, " Creation Date
*                domnam,    "
*                sysnam,    " System Name(For)
* PROGDIR
                STATE,     " Object status
                SUBC,      " Program type
                CNAM,      " Created by
                CDAT,      " Created on
                UNAM,      " Last changed by
                UDAT,      " Last changed on
                VERN,      " Version number
* D010INC/E071
                INCLUDE,       " Include
                TRKORR,        " Request/task
*                strkorr,
*                maxrc,
*                int_maxrc,
*                tarcli,
*                comsys,  " System Name(from)
*                srccli,
*                trstatus,
*                trfunction,
*                as4user,
*                pgmid,
*                object,
*                obj_name,
*                state_color,
                STRING_SEARCH,   " String Search
                DATUM,
                DURATION.
  DATA: R_TAB_COND TYPE RANGE OF SO_TEXT.
*        rs_tab_cond LIKE LINE OF r_tab_cond.
  DATA: SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  DATA: IS_GENERAL(1) TYPE C,
        DATE_FROM     LIKE SY-DATUM,
        BACKDAYS      TYPE I,
        SY_TABIX      LIKE SY-TABIX,
        FROM_TABIX    LIKE SY-TABIX,
        ENDDATE       LIKE SY-DATUM,
        ENDTIME       LIKE SY-UZEIT,
        TIME_DIFF     TYPE INT4,
        IS_OUT(1)     TYPE C,
        MAX_RC        TYPE TRRETCODE,
        MAX_RC_N(4)   TYPE N.
  DATA: LV_TRKORR   TYPE TRKORR,
        LV_OBJ      TYPE TADIR-OBJ_NAME,
        LV_TABIX    TYPE I,
        LV_OBJ_NAME TYPE VERSOBJNAM,
        LV_LINE_NUM TYPE I,
        LV_WHILE    TYPE I,
        LV_STRING   TYPE STRING,
        LV_TABNAME  TYPE DDOBJNAME.
  DATA: LS_DATA     LIKE LINE OF T_DATA,
        LS_QUEUE    TYPE TMSBUFFER,
        LS_OBJ      TYPE TY_OBJECT,
        LS_DD07V    TYPE DD07V,
        LS_MAIN_REQ TYPE /SKN/S_SW_TRKOR.
  DATA: LS_SEL_FIELDS TYPE /SKN/S_SEL_FIELDS,
        LS_ABAP       TYPE ABAPTXT255,
        LS_TRDIR      TYPE TRDIR,
        LS_RESULT     TYPE MATCH_RESULT,
        LS_TRANSP_DET TYPE /SKN/S_SW_01_01_TRANSP_DETAILS.
  DATA: LT_MAIN_REQ   LIKE TABLE OF LS_MAIN_REQ,
        LT_STR_SPLIT  TYPE TABLE OF STRING,
        LT_DATA       LIKE TABLE OF LS_DATA,
        LT_QUEUE      LIKE TABLE OF LS_QUEUE,
        LT_OBJECT     TYPE TT_OBJECT,
        LT_ABAP       TYPE ABAPTXT255_TAB,
        LT_TRDIR      TYPE STANDARD TABLE OF TRDIR,
        LT_RESULT     TYPE MATCH_RESULT_TAB,
        LT_DD07V_STAT TYPE STANDARD TABLE OF DD07V,
        LT_DD07V_FUNC TYPE STANDARD TABLE OF DD07V,
        LT_DD07V_N    TYPE STANDARD TABLE OF DD07V,
        LT_TRANSP_DET TYPE STANDARD TABLE OF /SKN/S_SW_01_01_TRANSP_DETAILS.
** Begin OF RFC CALL
  DATA: LT_OPTION TYPE TABLE OF RFC_DB_OPT,
        LT_DATA_RFC TYPE TABLE OF /SKN/S_SW_TAB2000,
        LT_TABLES_LIST  TYPE  /SKN/TT_TABLES,
        LT_JOIN_CONDITION TYPE  /SKN/TT_TABLE_JOIN,
        LWA_JOIN_CONDITION LIKE LINE OF LT_JOIN_CONDITION[],
        LT_SEL_FIELDS TYPE  /SKN/TT_SEL_FIELDS,
        LT_SORT_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT,
        LT_OUTPUT_FIELDS  TYPE  /SKN/TT_RFC_DB_FLD_EXTEND,
        LT_DFIES  TYPE TABLE OF  DFIES,
        LT_RETURN TYPE  BAPIRET2_T,
        LV_ROWCOUNT TYPE SOID-ACCNT,
        LWA_TABLES_LIST LIKE LINE OF LT_TABLES_LIST[],
        LT_IN_RANGE	TYPE TABLE OF	/SKN/S_SW_RANGE_TAB,
        LT_OUT_WHERE_COND	TYPE TABLE OF	/SKN/S_SW_WHERE_TAB,
        LWA_IN_RANGE LIKE LINE OF LT_IN_RANGE,
        LWA_OUT_WHERE_COND LIKE LINE OF LT_OUT_WHERE_COND.
  FIELD-SYMBOLS: <FS_OBJ> TYPE TY_OBJECT.
  _GET_CURRENT_DATE_TIME ' ' LV_SW_DEST SY_DATLO SY_TIMLO.
  IF R_DATUM[] IS INITIAL.
    RS_DATUM-SIGN   = 'I'.
    RS_DATUM-OPTION = 'GE'.
    DATE_FROM       = SY_DATLO - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM.
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
*--- Retrieve data
  CLEAR: IS_ALERT.
  REFRESH: T_DATA, LT_DATA.
******************************************** Get Transport Details **********************************************************************
  CALL FUNCTION '/SKN/FC_SW_01_01_TRANSP_DETAIL'
* IMPORTING
*   IS_ALERT       =
    TABLES
      T_SELECT = T_SELECT[]
      T_DATA   = LT_TRANSP_DET.
******************************************** Get Transport Details **********************************************************************
******************************************** Check and Get Program's Includes ***********************************************************
*  LOOP AT lt_transp_det INTO ls_transp_det.
*
*    CHECK ls_transp_det-object EQ gc_object_reps OR   " Report
*          ls_transp_det-object EQ gc_object_prog OR   " Program/Include
*          ls_transp_det-object EQ gc_object_func OR   " FM
*          ls_transp_det-object EQ gc_object_meth.     " Class Method
*
*    rs_objname-sign   = 'I'.
*    rs_objname-option = 'EQ'.
*    rs_objname-low    = ls_transp_det-obj_name.
*
*    APPEND rs_objname TO r_objname.
*    CLEAR: rs_objname.
*
*  ENDLOOP.
*
*  CHECK r_objname[] IS NOT INITIAL.
*
*  SORT r_objname BY low.
*  DELETE ADJACENT DUPLICATES FROM r_objname COMPARING low.
*
*  REFRESH lt_tables_list[].
*  _append_tables_list 'TADIR'   '' 'T'.    " Repository Table
**  _append_tables_list 'D010INC' '' 'D'.    " Includes Table
*  _append_tables_list 'PROGDIR' '' 'P'.    " General Table
*
** TADIR
*  _range_to_sel_table 'T~PGMID'      pgmid.       " Program Id
*  _range_to_sel_table 'T~OBJECT'     objtype.     " Object Type
*  _range_to_sel_table 'T~OBJ_NAME'   objname.     " Object Name
*  _range_to_sel_table 'T~SRCSYSTEM'  srcsystem.   " Original System
*  _range_to_sel_table 'T~AUTHOR'     author.      " Person Responsible
*  _range_to_sel_table 'T~DEVCLASS'   devclass.    " Package
*  _range_to_sel_table 'T~CREATED_ON' createdon.   " Creation Date
*
** PROGDIR
*  _range_to_sel_table 'P~STATE'      state.      " Object Status
*  _range_to_sel_table 'P~SUBC'       subc.       " Program type
*  _range_to_sel_table 'P~CNAM'       cnam.       " Created by
*  _range_to_sel_table 'P~CDAT'       cdat.       " Created on
*  _range_to_sel_table 'P~UNAM'       unam.       " Last changed by
*  _range_to_sel_table 'P~UDAT'       udat.       " Last changed on
*  _range_to_sel_table 'P~VERN'       vern.       " Version Number
*
** D010INC
**  _range_to_sel_table 'D~INCLUDE'    include.    " Include
*
*  lt_option[] = lt_out_where_cond[].
*
*  REFRESH lt_sel_fields[].
*
** TADIR
*  ls_sel_fields-table = 'TADIR'.
*
*  ls_sel_fields-field = 'OBJECT'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'OBJ_NAME'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'SRCSYSTEM'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'AUTHOR'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'DEVCLASS'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'CREATED_ON'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
** PROGDIR
*  ls_sel_fields-table = 'PROGDIR'.
*
*  ls_sel_fields-field = 'NAME'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'STATE'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'SUBC'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'CNAM'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'CDAT'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'UNAM'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'UDAT'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*** D010INC
**  ls_sel_fields-table = 'D010INC'.
**
**  ls_sel_fields-field = 'INCLUDE'.
**  APPEND ls_sel_fields TO lt_sel_fields.
*
*  _join_condition 'P' 'NAME'     'T' 'OBJ_NAME'.
**  _join_condition 'T' 'OBJ_NAME' 'D' 'MASTER'.
*
*  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
*    DESTINATION lv_sw_dest
*    IMPORTING
*      rowcount             = lv_rowcount
*    TABLES
*      options              = lt_option[]
*      data                 = lt_data_rfc[]
*      tables_list          = lt_tables_list[]
*      join_condition       = lt_join_condition[]
*      sel_fields           = lt_sel_fields[]
*      sort_options         = lt_sort_options[]
*      output_fields        = lt_output_fields[]
*      dfies                = lt_dfies[]
*      return               = lt_return[]
*    EXCEPTIONS
*      table_not_available  = 1
*      table_without_data   = 2
*      option_not_valid     = 3
*      field_not_valid      = 4
*      not_authorized       = 5
*      data_buffer_exceeded = 6
*      OTHERS               = 7.
*  IF sy-subrc IS NOT INITIAL OR lt_return IS NOT INITIAL.
*
*  ELSE.
*    _rfc_to_t_data lt_data_rfc lt_object lt_output_fields.
*  ENDIF.
******************************************** Check and Get Program's Includes ***********************************************************
*  CHECK lt_object IS NOT INITIAL.
  CHECK LT_TRANSP_DET IS NOT INITIAL.
* Get domain text values
  CALL FUNCTION 'DD_DOMA_GET'
    DESTINATION LV_SW_DEST
    EXPORTING
      DOMAIN_NAME = 'TRSTATUS'
*     GET_STATE   = 'M  '
*     LANGU       = SY-LANGU
    TABLES
      DD07V_TAB_A = LT_DD07V_STAT
      DD07V_TAB_N = LT_DD07V_N.
* Get domain text values
  CALL FUNCTION 'DD_DOMA_GET'
    DESTINATION LV_SW_DEST
    EXPORTING
      DOMAIN_NAME = 'TRFUNCTION'
*     GET_STATE   = 'M  '
*     LANGU       = SY-LANGU
    TABLES
      DD07V_TAB_A = LT_DD07V_FUNC
      DD07V_TAB_N = LT_DD07V_N.
  LOOP AT LT_TRANSP_DET INTO LS_TRANSP_DET.
    CLEAR: LT_RESULT, LT_ABAP, LT_TRDIR.
    LV_OBJ_NAME = LS_TRANSP_DET-OBJ_NAME.
    CHECK LS_TRANSP_DET-OBJECT EQ GC_OBJECT_REPS OR   " Report
          LS_TRANSP_DET-OBJECT EQ GC_OBJECT_REPO OR   " Report
          LS_TRANSP_DET-OBJECT EQ GC_OBJECT_PROG OR   " Program/Include
          LS_TRANSP_DET-OBJECT EQ GC_OBJECT_FUNC OR   " FM
          LS_TRANSP_DET-OBJECT EQ GC_OBJECT_METH.     " Class Method
* due to object type 'PROG' not processed in next FM
    IF LS_TRANSP_DET-OBJECT EQ GC_OBJECT_PROG.
      LS_TRANSP_DET-OBJECT = GC_OBJECT_REPS.
    ENDIF.
* Read object code and additional technical details
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      DESTINATION LV_SW_DEST
      EXPORTING
        OBJECT_NAME = LV_OBJ_NAME
        OBJECT_TYPE = LS_TRANSP_DET-OBJECT
*       versno      =
*       destination =
*       IV_NO_RELEASE_TRANSFORMATION       = ' '
      TABLES
        REPOS_TAB   = LT_ABAP
        TRDIR_TAB   = LT_TRDIR
      EXCEPTIONS
        NO_VERSION  = 1
        OTHERS      = 2.
    IF SY-SUBRC IS INITIAL.
      LOOP AT R_STRING_SEARCH INTO RS_STRING_SEARCH.
        CLEAR: LV_STRING,
               LS_ABAP.
        FIND ALL OCCURRENCES OF REGEX RS_STRING_SEARCH-LOW IN TABLE LT_ABAP
          IN CHARACTER MODE
          IGNORING CASE
          RESULTS LT_RESULT.
        CHECK LT_RESULT IS NOT INITIAL.
        SORT LT_RESULT BY LINE.
        DELETE ADJACENT DUPLICATES FROM LT_RESULT COMPARING LINE.
        IF RS_STRING_SEARCH-LOW(6) EQ 'MODIFY' OR
           RS_STRING_SEARCH-LOW(6) EQ 'UPDATE' OR
           RS_STRING_SEARCH-LOW(6) EQ 'INSERT' OR
           RS_STRING_SEARCH-LOW(6) EQ 'DELETE'.
          LOOP AT LT_RESULT INTO LS_RESULT.
            LV_TABIX = SY-TABIX.
            READ TABLE LT_ABAP INTO LS_ABAP INDEX LS_RESULT-LINE.
            IF SY-SUBRC IS INITIAL.
              SHIFT LS_ABAP-LINE LEFT DELETING LEADING SPACE.
              SPLIT LS_ABAP-LINE AT SPACE INTO TABLE LT_STR_SPLIT.
              READ TABLE LT_STR_SPLIT INTO LV_STRING INDEX 2.
            ENDIF.
* Check if the commands related to change
            IF LV_STRING IS NOT INITIAL.
              LV_TABNAME = LV_STRING.
              CALL FUNCTION 'CHECK_TAB_NAME'
                DESTINATION LV_SW_DEST
                EXPORTING
                  I_TABNAME     = LV_TABNAME
                EXCEPTIONS
                  TAB_NOT_FOUND = 1
                  OTHERS        = 2.
              IF SY-SUBRC IS NOT INITIAL.
                READ TABLE LT_STR_SPLIT INTO LV_STRING INDEX 3.
                IF LV_STRING IS NOT INITIAL.
                  LV_TABNAME = LV_STRING.
                  CALL FUNCTION 'CHECK_TAB_NAME'
                    DESTINATION LV_SW_DEST
                    EXPORTING
                      I_TABNAME     = LV_TABNAME
                    EXCEPTIONS
                      TAB_NOT_FOUND = 1
                      OTHERS        = 2.
                ENDIF.
                IF SY-SUBRC IS NOT INITIAL.
                  DELETE LT_RESULT INDEX LV_TABIX.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        CHECK LT_RESULT IS NOT INITIAL.
        MOVE-CORRESPONDING LS_TRANSP_DET TO LS_DATA.
        READ TABLE LT_TRDIR INTO LS_TRDIR INDEX 1.
        IF SY-SUBRC IS INITIAL.
          MOVE-CORRESPONDING LS_TRDIR TO LS_DATA.
        ENDIF.
        LS_DATA-STRING_SEARCH = RS_STRING_SEARCH-LOW.
* Set TRSTATUS value text
        IF LS_DATA-TRSTATUS IS NOT INITIAL.
          READ TABLE LT_DD07V_STAT INTO LS_DD07V WITH KEY DOMVALUE_L = LS_DATA-TRSTATUS.
          IF SY-SUBRC IS INITIAL.
            LS_DATA-TRSTATUS_TEXT = LS_DD07V-DDTEXT.
          ENDIF.
        ENDIF.
        CLEAR: LS_DD07V.
* Set TRFUNCTION text value
        IF LS_DATA-TRFUNCTION IS NOT INITIAL.
          READ TABLE LT_DD07V_STAT INTO LS_DD07V WITH KEY DOMVALUE_L = LS_DATA-TRFUNCTION.
          IF SY-SUBRC IS INITIAL.
            LS_DATA-TRFUNCTION_TEXT = LS_DD07V-DDTEXT.
          ENDIF.
        ENDIF.
        CLEAR: LS_DD07V.
        LOOP AT LT_RESULT INTO LS_RESULT.
          READ TABLE T_DATA WITH KEY LINE_SCAN = LS_RESULT-LINE
            TRANSPORTING NO FIELDS.
          CHECK SY-SUBRC IS NOT INITIAL.
          LS_DATA-LINE_NO = LS_RESULT-LINE.
          READ TABLE LT_ABAP INTO LS_ABAP INDEX LS_RESULT-LINE.
          IF SY-SUBRC IS INITIAL.
            LS_DATA-LINE_SCAN = LS_ABAP-LINE.
          ENDIF.
          APPEND LS_DATA TO T_DATA.
          CLEAR: LS_DATA-STRING_SEARCH.
        ENDLOOP.
*          ELSEIF rs_string_search-low(6) EQ 'DELETE'.
*
** Check if result is relevant
*            LOOP AT lt_result INTO ls_result.
*
*              lv_tabix = sy-tabix.
*              READ TABLE lt_abap INTO ls_abap INDEX ls_result-line.
*              IF sy-subrc IS INITIAL.
*                SHIFT ls_abap-line LEFT DELETING LEADING space.
*                SPLIT ls_abap-line AT space INTO TABLE lt_str_split.
*                READ TABLE lt_str_split INTO lv_string INDEX 2.
*              ENDIF.
*
** Check if the commands related to DB table change
*              IF lv_string IS NOT INITIAL.
*
*                lv_tabname = lv_string.
*                CALL FUNCTION 'CHECK_TAB_NAME'
*                  DESTINATION lv_sw_dest
*                  EXPORTING
*                    i_tabname   = lv_tabname
*                  EXCEPTIONS
*                    not_found = 1
*                    OTHERS    = 2.
*                IF sy-subrc IS NOT INITIAL.
*                  READ TABLE lt_str_split INTO lv_string INDEX 3.
*                  IF lv_string IS NOT INITIAL.
*                    lv_tabname = lv_string.
*                    CALL FUNCTION 'CHECK_TAB_NAME'
*                      DESTINATION lv_sw_dest
*                      EXPORTING
*                        i_tabname     = lv_tabname
*                      EXCEPTIONS
*                        tab_not_found = 1
*                        OTHERS        = 2.
*                  ENDIF.
*                  IF sy-subrc IS NOT INITIAL.
*                    DELETE lt_result INDEX lv_tabix.
*                  ENDIF.
*                ENDIF.
*        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
*  ENDIF.
*  ENDLOOP.
*  LOOP AT lt_object ASSIGNING <fs_obj>.
*
*    lv_tabix = sy-tabix.
*
*    IF <fs_obj>+30(2) EQ 'CP'. " Class Pool
*      DELETE lt_object INDEX lv_tabix.
*
*    ELSEIF <fs_obj>+30(2) = 'IP'. " Interface Pool
**      DELETE lt_object INDEX lv_tabix.
*
*      <fs_obj>+31(1) = 'U'.
*
*    ENDIF.
*
*    IF <fs_obj>-subc EQ 'I'.
*      EXIT.
*    ENDIF.
*
*  ENDLOOP.
******************************************** Get Source Code **********************************************************
*  _range_to_sel_table 'E~AS4DATE'    datum.       " Date of Last Change
*  _range_to_sel_table 'E~TRKORR'     trkorr.      " Request/Task
*  _range_to_sel_table 'E~TRSTATUS'   trstatus.    " Status:Modifiable, Released
*  _range_to_sel_table 'E~TRFUNCTION' trfunction.  " Type of request/task:Workbench, Customizing
******************************************** Get Source Code **********************************************************
*  REFRESH lt_main_req.
******************************************** Get Transport Details ****************************************************
*-- Fill Duration Value
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX.
    T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = T_DATA-AS4DATE
        T_FROM      = T_DATA-AS4TIME
        D_TO        = SY_DATLO
        T_TO        = SY_TIMLO
        TIME_UNIT   = LV_DURATION_UNIT
      IMPORTING
        TIME_DIFF   = TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      T_DATA-DURATION = TIME_DIFF .
    ELSE.
      T_DATA-DURATION = '999999'.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX .
  ENDLOOP .
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL.
  IS_ALERT = 'X'.
ENDFUNCTION.
```
