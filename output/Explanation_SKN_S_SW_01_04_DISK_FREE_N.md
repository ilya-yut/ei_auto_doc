# Exception Indicator: Free Disk Space Enhanced - SW_01_04_DISK_FREE_E

## General Overview

This Exception Indicator reports server file-system free-space and usage figures gathered through the standard host and file-system interfaces, so operations teams can see drives or mount points that are critically full or trending toward risk thresholds. It is aimed at proactive capacity management for application servers and related landscapes where disk exhaustion would otherwise surface only as sudden job failures or database errors.

This EI serves as an essential control for infrastructure and basis operations by:

- Enabling early detection of volumes whose free percentage or absolute free space no longer meets internal runway targets.
- Supporting prioritization when specific hosts, instances, or services concentrate the worst utilization signals.
- Providing a repeatable snapshot that aligns with how administrators review OS-level space alongside SAP work processes.
- Helping release and migration windows by highlighting filesystem pressure before new components are deployed.
- Giving management a concise indicator that storage discipline is monitored with the same rigor as batch and dialog health.

Typical use cases include daily capacity reviews, escalation after backup or log growth events, and evidence for audits that show monitoring of critical server resources.

The function combines remote host queries with derived utilization metrics so that both absolute free space and percentage-based risk can be judged in one pass.


## Problem Description

Failure to monitor server disk utilization in a structured way creates multiple risks across application availability, database integrity, and the ability to recover quickly when growth or misconfiguration consumes space unexpectedly.

**Availability and Performance Risks**

- Sudden full filesystem conditions can halt batch chains, trace writing, or temporary sort areas without a clear upstream warning to business users.
- Uneven utilization across instances can overload one node while siblings remain idle, masking hotspots until interactive work degrades.
- Log or spool directories that grow without bounds can turn a minor incident into a wide outage.

**Operational and Recovery Risks**

- Backup and restore procedures may fail silently or partially when targets cannot write completion metadata.
- Patching or kernel activities that require transient space can stall mid-run, extending maintenance windows.
- Teams may discover the problem only from generic short dumps or database alerts rather than from a filesystem-first signal.

**Management Visibility and Decision-Making Risks**

- Leadership lacks a compact narrative on whether capacity keeps pace with application growth between formal infrastructure reviews.
- Finance and procurement cannot align disk expansion decisions with observed utilization trends when evidence is anecdotal.
- Post-incident reviews struggle to show that monitoring scope covered all hosts and services in scope for the service catalog.

## Suggested Resolution

**Immediate Response**

- When the monitor highlights a host or filesystem row, confirm the same condition in your standard OS or storage tooling for that server and mount point before opening a major incident.
- Identify whether the pressure is on data volumes, log areas, or transient directories so remediation matches the real growth driver.
- Check whether the spike aligns with batch archives, trace levels, or recent transport activity.
- Capture the utilization snapshot for the infrastructure owner if change tickets are required.

**System Assessment**

- Compare current free percentage and absolute free values to the prior week for the same host and service slice.
- Validate whether growth is steady or stepwise, which informs whether a quick cleanup versus a capacity purchase is appropriate.
- Review scheduled jobs that write large extracts or traces to the affected paths.
- Correlate with database or application alerts that may have fired slightly earlier on the same node.

**Corrective Actions**

- Purge or relocate obsolete logs, traces, and dumps when safe retention policies allow.
- Expand volumes or rebalance data paths when sustained utilization exceeds agreed guardrails.
- Adjust monitoring thresholds after baseline noise is understood, and document the rationale for audit trail.
- Fold short guidance into existing runbooks so on-call staff recognize the pattern without adding a separate training subsection.


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | CAPACITY | Total Space (MB) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 2 | DRIVE | Drive Letter | CHAR | 1 | 0 | /SKN/E_SW_SYS_DRIVE |  |
| 3 | FREE | Data for tables to call "SAPOSCOL" | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 4 | FREE_GB | Free Space (GB) | INT4 | 10 | 0 | /SKN/E_SW_SYS_DRIVE_FREE_G |  |
| 5 | FREE_KB | Free Space (KB) | INT4 | 10 | 0 | /SKN/E_SW_SYS_DRIVE_FREE_K |  |
| 6 | FREE_MB | Free Space (MB) | INT4 | 10 | 0 | /SKN/E_SW_SYS_DRIVE_FREE_M |  |
| 7 | FREE_PRC | Free Space (%) | INT1 | 3 | 0 | INT1 | INT1 |
| 8 | FSYSNAME | File System Name | CHAR | 36 | 0 | FSYSNAME | TEXT36 |
| 9 | HOST | Host Name | CHAR | 32 | 0 | MSHOST2 | MSHOST2 |
| 10 | NAME | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 11 | SERV | Service | CHAR | 20 | 0 | MSSERV | MSSERV |
| 12 | UDATE |  | DATS | 8 | 0 |  |  |
| 13 | USED_PRC | Used Space (%) | INT1 | 3 | 0 | INT1 | INT1 |
| 14 | UTIME |  | TIMS | 6 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 14 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**CAPACITY** (Total Space (MB))

When left open per framework rules, CAPACITY does not restrict total space (mb); when set, only matching rows remain.

**DRIVE** (Drive Letter)

Separates cross-client noise from in-scope work when drive letter on DRIVE correlates with client or user attributes.

**FREE** (Data for tables to call "SAPOSCOL")

After data is read, lines are removed unless data for tables to call "saposcol" on FREE still satisfies the active multivalued selection.

**FREE_GB** (Free Space (GB))

Prevents accidental global scans when free space (gb) (FREE_GB) is meant to stay within a controlled application slice.

**FREE_KB** (Free Space (KB))

Reflects real administration where free space (kb) on FREE_KB is routinely restricted to a single productive client or object family.

**FREE_MB** (Free Space (MB))

When harmonized with related filters, free space (mb) on FREE_MB isolates the highest-risk record families.

**FREE_PRC** (Free Space (%))

When left open per framework rules, FREE_PRC does not restrict free space (%); when set, only matching rows remain.

**FSYSNAME** (File System Name)

When combined with destination discipline, file system name on FSYSNAME keeps both breadth and depth of the extract intentional.

**HOST** (Host Name)

Uses host name from the source context so only records with HOST inside declared values proceed.

**NAME** (Server Name)

Documents expected operator behavior—server name on NAME should be set when that dimension is part of the control objective.

**SERV** (Service)

Supports escalation where service on SERV signals ownership for follow-up between Basis and functional teams.

**UDATE** (UDATE)

Supports operational control by evaluating udate through UDATE for each candidate record.

**USED_PRC** (Used Space (%))

For distributed landscapes, used space (%) on USED_PRC often anchors which application server or destination appears in results.

**UTIME** (UTIME)

Helps distinguish technical versus business attributes when utime on UTIME correlates with counters or status fields.


### Parameter Relationships

How parameter combinations work together

Host, service, and server name selectors narrow which instances contribute rows before drive letter and free-space range filters decide which filesystem lines remain. Percentage-based free and used thresholds work together with absolute free-space ranges in kilobytes, megabytes, or gigabytes so operators can express policy either as a fraction of capacity or as a concrete remaining size. File-system name and capacity fields help distinguish mount points that share a drive letter or naming pattern in large landscapes.

Timestamp fields on the result line describe when the observation was collected; they should be read together with utilization metrics when comparing runs from different times of day or after maintenance. Cloud destination selection, when used, aligns remote collection with the same logical routing your landscape uses for other remote monitors so results stay comparable across tools.


### Default Values

No default values are defined for this EI.

### Practical Example of Parameter Configuration

**Use Case 1: Critical low free percentage**

**Purpose:** Alert when any monitored drive on a named host drops below ten percent free while still bounding absolute space.

```
HOST = SAPPRD01
FREE_PRC = 0-10
FREE_GB = 0-5
DRIVE = C
SERV = disp+work
```

**Use Case 2: Database server slice**

**Purpose:** Track large data volumes on a specific instance with both MB and percent windows.

```
NAME = prddb00
HOST = prddb00
FREE_MB = 50000-999999999
USED_PRC = 85-100
FSYSNAME = /db/data01
CAPACITY = 1048576
```

**Use Case 3: Multi-drive review**

**Purpose:** Sample several dimensions for a weekly capacity report.

```
HOST = %PRD%
DRIVE = D
FREE_KB = 104857600-999999999999999
UDATE = 20260101-20260131
UTIME = 000000-235959
FREE = 1
SW_DEST = NONE
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_04_DISK_FREE_N | CAPACITY | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_04_DISK_FREE_N | DRIVE | SW:  Disk Drive Letter | CHAR(1) | /SKN/E_SW_SYS_DRIVE |
| /SKN/S_SW_01_04_DISK_FREE_N | FREE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_04_DISK_FREE_N | FREE_GB |  | INT4(10) | /SKN/E_SW_SYS_DRIVE_FREE_G |
| /SKN/S_SW_01_04_DISK_FREE_N | FREE_KB |  | INT4(10) | /SKN/E_SW_SYS_DRIVE_FREE_K |
| /SKN/S_SW_01_04_DISK_FREE_N | FREE_MB |  | INT4(10) | /SKN/E_SW_SYS_DRIVE_FREE_M |
| /SKN/S_SW_01_04_DISK_FREE_N | FREE_PRC | Dummy for B20 int1 (Local Everywhere) | INT1(3) | INT1 |
| /SKN/S_SW_01_04_DISK_FREE_N | FSYSNAME | Name of a File System (for "SAPOSCOL") | CHAR(36) | FSYSNAME |
| /SKN/S_SW_01_04_DISK_FREE_N | HOST | Name of Application Server | CHAR(32) | MSHOST2 |
| /SKN/S_SW_01_04_DISK_FREE_N | NAME | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_04_DISK_FREE_N | SERV | Service | CHAR(20) | MSSERV |
| /SKN/S_SW_01_04_DISK_FREE_N | UDATE |  | DATS(8) |  |
| /SKN/S_SW_01_04_DISK_FREE_N | USED_PRC | Dummy for B20 int1 (Local Everywhere) | INT1(3) | INT1 |
| /SKN/S_SW_01_04_DISK_FREE_N | UTIME |  | TIMS(6) |  |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_04_DISK_FREE_N.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_04_DISK_FREE_N OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_DRIVE   FOR /SKN/S_SW_SYS_DISK_SPACE-DRIVE ,
         R_FREE_KB   FOR /SKN/S_SW_SYS_DISK_SPACE-FREE_KB,
         R_FREE_MB   FOR /SKN/S_SW_SYS_DISK_SPACE-FREE_MB ,
         R_FREE_GB   FOR /SKN/S_SW_SYS_DISK_SPACE-FREE_GB .
DATA : DRIVE(1) TYPE C ,
       FREE_KB LIKE /SKN/S_SW_SYS_DISK_SPACE-FREE_KB ,
       FREE_MB LIKE /SKN/S_SW_SYS_DISK_SPACE-FREE_MB ,
       FREE_GB LIKE /SKN/S_SW_SYS_DISK_SPACE-FREE_GB .
DATA : DIRECTORY LIKE SDBAIDIR-DIRECTORY ,
       AVAILABLE LIKE SDBAIDIR-AVAILABLE ,
       PERCENT   LIKE SDBAIDIR-PERCENT ,
       TOTAL     LIKE SDBAIDIR-TOTAL ,
       USED      LIKE SDBAIDIR-USED .
DATA : IS_SPACE(1) TYPE C .
DATA : IFREE TYPE I .
DATA : LV_FREE_PRC TYPE F.
DATA: LS_DESTI TYPE RFCHOSTS,
      LT_DESTI LIKE TABLE OF LS_DESTI.
DATA: LS_SERVER_LIST TYPE MSXXLIST,
      LT_SERVER_LIST LIKE TABLE OF LS_SERVER_LIST.
DATA: LS_FSYS_SINGLE TYPE FSY_SINGLE,
      LT_FSYS_SINGLE LIKE TABLE OF LS_FSYS_SINGLE.
DATA: LV_LOG_DEST TYPE RFCDEST.
DATA : SY_DATLO LIKE SY-DATLO ,
       SY_TIMLO LIKE SY-TIMLO .
DATA: LV_NO_SERVER_LIST(1) TYPE C.
DATA_SINGLE: DEST RFCDEST.             .
SELECT_SINGLE: DEST.
DATA_SINGLE:   SW_DEST RFCDEST.
SELECT_SINGLE: SW_DEST.
IF LV_SW_DEST IS NOT INITIAL.
  LV_DEST = LV_SW_DEST.
ENDIF.
DATA_MULTY: FREE_PRC INT1,
            USED_PRC INT1,
            NAME MSNAME2,
            HOST MSHOST2,
            SERV MSSERV.
SELECT_MULTY: FREE_PRC,
              USED_PRC,
              NAME,
              HOST,
              SERV.
""_set_sys_date_time lv_sw_dest sy_datlo sy_timlo
_GET_CURRENT_DATE_TIME ' ' LV_SW_DEST SY_DATLO SY_TIMLO.  .
   LOOP AT T_SELECT WHERE FIELDNM = 'DRIVE'.
     MOVE-CORRESPONDING T_SELECT TO R_DRIVE.
     APPEND R_DRIVE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FREE_KB'.
     MOVE-CORRESPONDING T_SELECT TO R_FREE_KB.
     APPEND R_FREE_KB.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FREE_MB'.
     MOVE-CORRESPONDING T_SELECT TO R_FREE_MB.
     APPEND R_FREE_MB.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'FREE_GB'.
     MOVE-CORRESPONDING T_SELECT TO R_FREE_GB.
     APPEND R_FREE_GB.
   ENDLOOP.
  IF R_FREE_KB[] IS INITIAL.
   LOOP AT R_FREE_MB.
     IFREE = R_FREE_MB-LOW .
     R_FREE_KB-LOW = IFREE * 1024 .
      IFREE = R_FREE_MB-HIGH .
      R_FREE_KB-HIGH = IFREE * 1024 .
       R_FREE_KB-SIGN = R_FREE_MB-SIGN .
         R_FREE_KB-OPTION = R_FREE_MB-OPTION .
          APPEND R_FREE_KB .
   ENDLOOP .
  ENDIF.
  IF R_FREE_KB[] IS INITIAL.
   LOOP AT R_FREE_GB.
     IFREE = R_FREE_GB-LOW .
     R_FREE_KB-LOW = IFREE * 1024 * 1024 .
      IFREE = R_FREE_GB-HIGH .
      R_FREE_KB-HIGH = IFREE * 1024 * 1024.
       R_FREE_KB-SIGN = R_FREE_GB-SIGN .
         R_FREE_KB-OPTION = R_FREE_GB-OPTION .
          APPEND R_FREE_KB .
   ENDLOOP .
  ENDIF.
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  CLEAR LV_NO_SERVER_LIST.
  REFRESH LT_DESTI.
  CALL FUNCTION 'TH_SERVER_LIST'
    DESTINATION        LV_DEST
    TABLES
      LIST = LT_SERVER_LIST
    EXCEPTIONS
      NO_SERVER_LIST        = 1
      SYSTEM_FAILURE        = 2
      COMMUNICATION_FAILURE = 3
      OTHERS                = 9.
  IF SY-SUBRC IS NOT INITIAL .
    CASE SY-SUBRC.
      WHEN 1.  " NO_SERVER_LIST
        LV_NO_SERVER_LIST = 'X'.
      WHEN OTHERS.
       " exit.
    ENDCASE.
  ENDIF.
  IF LV_NO_SERVER_LIST IS NOT INITIAL. " Add fictive Item
    CLEAR LS_SERVER_LIST.
    "ls_SERVER_LIST-NAME = lv_SW_DEST.
    APPEND LS_SERVER_LIST TO LT_SERVER_LIST.
  ENDIF.
***  LOOP AT lt_SERVER_LIST into ls_SERVER_LIST.
***    ls_DESTI-RFCDEST = ls_SERVER_LIST-NAME.
***
***    if lv_DEST is not initial.  "--- Cloud Mode
***      ls_DESTI-RFCDEST = lv_DEST.
***    endif.
***
***    APPEND ls_DESTI to lt_DESTI.
***  ENDLOOP.
  LOOP AT LT_SERVER_LIST INTO LS_SERVER_LIST.
    IF LV_NO_SERVER_LIST IS INITIAL.
      LV_LOG_DEST = LS_SERVER_LIST-NAME.
    ENDIF.
    CALL FUNCTION 'GET_FSYS_SINGLE'
      DESTINATION        LV_DEST
      EXPORTING
        LOCAL_REMOTE                         = 'INTERN'
        LOGICAL_DESTINATION                  = LV_LOG_DEST
*     IMPORTING
*       F_FSYS_SINGLE_READ                   =
*       ACTIVEFLAG                           =
*       INTERVAL                             =
*       DETAILSCOLL                          =
*       DETAILSREQI                          =
*       DETAILSMODE                          =
*       LASTCOLLWRT                          =
*       LASTCOLLINT                          =
*       NORMCOLLINT                          =
      TABLES
        TF_FSYS_SINGLE                       = LT_FSYS_SINGLE
      EXCEPTIONS
        INTERNAL_ERROR_ADRESS_FAILED         = 1
        INTERNAL_ERROR_DIFFERENT_FIELD       = 2
        INTERNAL_ERROR_NO_NEW_LINE           = 3
        COLLECTOR_NOT_RUNNING                = 4
        SHARED_MEMORY_NOT_AVAILABLE          = 5
        COLLECTOR_BUSY                       = 6
        VERSION_CONFLICT                     = 7
        NO_NETWORK_COLLECTOR_RUNNING         = 8
        SYSTEM_FAILURE                       = 9
        COMMUNICATION_FAILURE                = 10
        OTHERS                               = 11.
    IF SY-SUBRC = 0.
      LOOP AT LT_FSYS_SINGLE INTO LS_FSYS_SINGLE.
        CLEAR T_DATA.
        MOVE-CORRESPONDING LS_SERVER_LIST TO T_DATA.
        MOVE-CORRESPONDING LS_FSYS_SINGLE TO T_DATA.
"        if strlen( ls_fsys_single-FSYSNAME ) <= 2.
          T_DATA-FREE_MB = T_DATA-FREE.
          T_DATA-FREE_GB = T_DATA-FREE_MB / 1024.
          TRY.
            T_DATA-FREE_KB = T_DATA-FREE * 1024.
            CATCH CX_SY_ARITHMETIC_OVERFLOW. " INTO myref.
            CLEAR T_DATA-FREE_KB.
          ENDTRY.
          IF T_DATA-CAPACITY <> 0.
            LV_FREE_PRC = T_DATA-FREE / T_DATA-CAPACITY * 100.
            IF LV_FREE_PRC >= 0 AND LV_FREE_PRC <= 100.
              T_DATA-FREE_PRC = LV_FREE_PRC.
            ENDIF.
            T_DATA-USED_PRC = 100 - T_DATA-FREE_PRC.
          ENDIF.
          T_DATA-DRIVE = LS_FSYS_SINGLE-FSYSNAME+0(1).
          T_DATA-UDATE = SY_DATLO.
          T_DATA-UTIME = SY_TIMLO.
          APPEND T_DATA.
 "       endif.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DRIVE NOT IN R_DRIVE.
  DELETE T_DATA WHERE FREE_PRC NOT IN R_FREE_PRC.
  DELETE T_DATA WHERE USED_PRC NOT IN R_USED_PRC.
  DELETE T_DATA WHERE NAME NOT IN R_NAME.
  DELETE T_DATA WHERE HOST NOT IN R_HOST.
  DELETE T_DATA WHERE SERV NOT IN R_SERV.
  DELETE T_DATA WHERE FREE_KB NOT IN R_FREE_KB.
  DELETE T_DATA WHERE FREE_MB NOT IN R_FREE_MB.
  DELETE T_DATA WHERE FREE_GB NOT IN R_FREE_GB.
*** loop at R_DRIVE .
***  DRIVE = R_DRIVE-LOW .
***  if not DRIVE is initial .
***    concatenate DRIVE ':' into DIRECTORY .
***    clear is_space .
***    CALL FUNCTION 'SHOW_DIRECTORY_FILL_STATUS'
***      DESTINATION        lv_DEST
***      EXPORTING
***        directory   = DIRECTORY
***      IMPORTING
***       AVAILABLE    = AVAILABLE
***       PERCENT      = PERCENT
***       TOTAL        = TOTAL
***       USED         = USED
***     EXCEPTIONS
***       PROBLEMS        = 1
***       OTHERS          = 2 .
***    IF sy-subrc = 0.
***      is_space = 'X' .
***      if AVAILABLE <= '9999999999'.
***        FREE_KB = AVAILABLE .
***        ifree = FREE_KB .
***         ifree = ifree / 1024 .
***         FREE_MB = ifree .
***          ifree = ifree / 1024 .
***          FREE_GB = ifree .
***       else.
***         FREE_KB = 999999999.
***          ifree = AVAILABLE / 1024 .
***          FREE_MB = ifree .
***           ifree = ifree / 1024 .
***           FREE_GB = ifree .
***       endif.
***    ENDIF.
***    if not is_space is initial .
***      if  FREE_KB in R_FREE_KB .
***        IS_ALERT = 'X' .
***         T_DATA-DRIVE = DRIVE .
***          T_DATA-FREE_KB = FREE_KB .
***           T_DATA-FREE_MB = FREE_MB .
***            T_DATA-FREE_GB = FREE_GB .
***             T_DATA-UDATE = sy-datum.
***              T_DATA-UTIME = sy-uzeit.
***         append T_DATA .
***      endif .
***    endif .
***  endif .
*** endloop .
ENDFUNCTION.
```
