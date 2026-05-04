# Exception Indicator: Servers CPU controlliing - SW_01_20_SRV_CPU

## General Overview

This Exception Indicator monitors application server CPU utilization and related operating-system counters collected through the standard application-server list and CPU measurement interfaces. It gives basis and operations teams a consolidated view of load averages, interrupt and context-switch rates, and user versus system versus idle time so that capacity stress is visible before interactive response or batch throughput degrades.

This EI serves as an essential control for infrastructure and performance management by:

- Enabling detection of hosts or instances where sustained load, wait, or utilization patterns diverge from agreed norms.
- Supporting prioritization when specific servers, CPU counts, or utilization bands concentrate the risk signal.
- Providing a repeatable snapshot aligned with how administrators review SM50-class workload together with OS-level CPU posture.
- Helping change windows and month-end peaks by surfacing spikes that correlate with job bursts or new deployments.
- Giving management confidence that processor capacity is observed with the same discipline as dialog and batch health checks.

Typical use cases include daily capacity triage, escalation after user-reported slowness, and evidence for reviews of hardware or virtualization sizing.

The function relies on standard SAP server list and CPU-all retrieval services, then applies the configured multivalued filters to the returned metrics before alert evaluation.


## Problem Description

Failure to monitor CPU utilization and related server-side counters in a structured way creates multiple risks across interactive performance, batch stability, and the ability to prove that processor capacity keeps pace with business growth.

**Performance and Availability Risks**

- Sustained high utilization or rising wait indicators can slow dialog and background work long before a formal capacity project is approved.
- Uneven load across instances can hide hotspots when averages look acceptable at the landscape level.
- Interrupt or context-switch pressure can signal driver, firmware, or virtualization issues that simple CPU percentage alone would miss.

**Operational and Recovery Risks**

- Teams may react only after widespread user complaints or job cancellations, missing earlier concentration on a subset of hosts.
- Without comparable metrics run over run, administrators cannot judge whether a spike is seasonal noise or a new baseline.
- Emergency mitigation becomes guesswork when no agreed monitoring lens exists for load averages versus utilization percentages.

**Management Visibility and Decision-Making Risks**

- Leadership lacks a compact narrative on whether processor headroom is healthy during high-change periods.
- Hardware or cloud sizing reviews lack objective series data tied to the same server naming the business already uses in incident tickets.
- Post-incident analysis struggles to show that monitoring scope matched the real server population in support contracts.

## Suggested Resolution

**Immediate Response**

- When the monitor highlights a server or host row, confirm the same condition in your standard OS or virtualization console for that instance before opening a major incident.
- Identify whether the signal is driven by load averages, raw utilization splits, or wait-style percentages so remediation targets the right dimension.
- Check whether the observation aligns with a known batch window, month-end close, or recent transport or kernel activity.
- Capture the metric snapshot for the infrastructure owner if change or capacity tickets are required.

**System Assessment**

- Compare current utilization and load patterns to the prior week for the same host and application server slice.
- Validate whether CPU count and subtype filters still match the intended landscape after server renames or migrations.
- Correlate spikes with scheduled jobs that are known to be CPU-intensive on specific nodes.
- Review whether time-zone handling for the evaluation clock could explain apparent drift around boundary times.

**Corrective Actions**

- Rebalance workload, reschedule heavy jobs, or add instances when sustained pressure exceeds internal guardrails.
- Coordinate with hardware or cloud providers when virtualization limits or host contention are suspected.
- Adjust monitoring thresholds after baseline behavior is documented, and record the rationale for audit trail.
- Fold short guidance into existing runbooks so on-call staff recognize the pattern without adding a separate training subsection.


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | CPU_USAGE | CPU Utilization | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 2 | CS_SEC | Context switches (sec) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 3 | HOST | Host Name | CHAR | 32 | 0 | MSHOST2 | MSHOST2 |
| 4 | IDLE_TOTAL | Idle (%) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 5 | IDLE_TRUE | Idle True (%) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 6 | INT_SEC | Interrupts (sec) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 7 | LOAD15_AVG | Avg processes waiting (15 min) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 8 | LOAD1_AVG | Avg processes waiting (1 min) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 9 | LOAD5_AVG | Avg processes waiting (5 min) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 10 | NBR_CPU | Number of CPUs | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 11 | SERIALNR | Serial number | INT2 | 5 | 0 | COLLSERNR | INT2 |
| 12 | SERVER | Server Name | CHAR | 40 | 0 | MSNAME2 | MSNAME2 |
| 13 | SUBTYPE | saposcol subtype | INT2 | 5 | 0 | COLLSUBTYP | INT2 |
| 14 | SYS_TOTAL | System Utilization (%) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 15 | SYSC_SEC | System Calls (sec) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 16 | TYPE | saposcoltype | INT2 | 5 | 0 | COLLTYPE | INT2 |
| 17 | USR_TOTAL | User Utilization ( %) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |
| 18 | WAIT_TRUE | Wait True (%) | INT4 | 10 | 0 | INT4_DATA | SYST_LONG |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 18 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**CPU_USAGE** (CPU Utilization)

Valuable when comparing health before and after a release—hold cpu utilization on CPU_USAGE constant while varying other filters.

**CS_SEC** (Context switches (sec))

For distributed landscapes, context switches (sec) on CS_SEC often anchors which application server or destination appears in results.

**HOST** (Host Name)

Helps distinguish technical versus business attributes when host name on HOST correlates with counters or status fields.

**IDLE_TOTAL** (Idle (%))

When tightened, idle (%) (IDLE_TOTAL) removes rows that would otherwise dilute attention from failing or stuck cases.

**IDLE_TRUE** (Idle True (%))

Improves readability of exported lists because idle true (%) (IDLE_TRUE) columns stay aligned with the configured filter intent.

**INT_SEC** (Interrupts (sec))

Gives auditors traceable criteria because interrupts (sec) on INT_SEC is applied consistently before any alert flag is raised.

**LOAD15_AVG** (Avg processes waiting (15 min))

Explains why two monitoring passes differ: only the pass with stricter avg processes waiting (15 min) on LOAD15_AVG surfaces the disputed rows.

**LOAD1_AVG** (Avg processes waiting (1 min))

Separates cross-client noise from in-scope work when avg processes waiting (1 min) on LOAD1_AVG correlates with client or user attributes.

**LOAD5_AVG** (Avg processes waiting (5 min))

When tightened, avg processes waiting (5 min) (LOAD5_AVG) removes rows that would otherwise dilute attention from failing or stuck cases.

**NBR_CPU** (Number of CPUs)

Explains why two monitoring passes differ: only the pass with stricter number of cpus on NBR_CPU surfaces the disputed rows.

**SERIALNR** (Serial number)

When tightened, serial number (SERIALNR) removes rows that would otherwise dilute attention from failing or stuck cases.

**SERVER** (Server Name)

Gives auditors traceable criteria because server name on SERVER is applied consistently before any alert flag is raised.

**SUBTYPE** (saposcol subtype)

Documents expected operator behavior—saposcol subtype on SUBTYPE should be set when that dimension is part of the control objective.

**SYS_TOTAL** (System Utilization (%))

Supports escalation where system utilization (%) on SYS_TOTAL signals ownership for follow-up between Basis and functional teams.

**SYSC_SEC** (System Calls (sec))

After data is read, lines are removed unless system calls (sec) on SYSC_SEC still satisfies the active multivalued selection.

**TYPE** (saposcoltype)

Prevents accidental global scans when saposcoltype (TYPE) is meant to stay within a controlled application slice.

**USR_TOTAL** (User Utilization ( %))

Supports escalation where user utilization ( %) on USR_TOTAL signals ownership for follow-up between Basis and functional teams.

**WAIT_TRUE** (Wait True (%))

Supports operational control by evaluating wait true (%) through WAIT_TRUE for each candidate record.


### Parameter Relationships

How parameter combinations work together

Server and host selectors define which application server instances contribute rows before utilization and load-average filters narrow the list to the CPU posture you intend to review. Interrupt, system-call, and context-switch counters work alongside user, system, idle, and wait percentages so operators can distinguish raw utilization pressure from kernel-style churn. CPU count and saposcol type or subtype fields help isolate comparable hardware families when mixed generations exist in one landscape.

When several numeric thresholds are active together, each dimension still applies to the same retrieved interval: a row must satisfy the server scope and every populated utilization or rate filter before it influences alerting. Cloud destination selection, when configured, keeps remote collection aligned with the same logical routing other remote monitors use so results stay comparable across tools.


### Default Values

No default values are defined for this EI.

### Practical Example of Parameter Configuration

**Use Case 1: Single production instance**

**Purpose:** Watch one application server with tight CPU utilization and load-average ceilings.

```
SERVER = PRD_ASCS01
HOST = prd-phx-01
CPU_USAGE = 85-100
LOAD1_AVG = 8-999999
NBR_CPU = 8
```

**Use Case 2: User versus system imbalance**

**Purpose:** Highlight hosts where user time dominates while idle time collapses.

```
SERVER = %DEV%
USR_TOTAL = 70-100
IDLE_TOTAL = 0-15
WAIT_TRUE = 5-100
SYS_TOTAL = 10-40
```

**Use Case 3: Kernel churn review**

**Purpose:** Sample several OS-level counters for a weekly stability report.

```
HOST = %QAS%
INT_SEC = 5000-999999999
SYSC_SEC = 8000-999999999
CS_SEC = 12000-999999999
LOAD15_AVG = 4-999999
TYPE = 1
SUBTYPE = 2
SERIALNR = 0-32767
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_SRV_CPU | CPU_USAGE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | CS_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | HOST | Name of Application Server | CHAR(32) | MSHOST2 |
| /SKN/S_SW_01_01_SRV_CPU | IDLE_TOTAL | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | IDLE_TRUE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | INT_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | LOAD15_AVG | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | LOAD1_AVG | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | LOAD5_AVG | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | NBR_CPU | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | SERIALNR | Serial number in saposcol result structure | INT2(5) | COLLSERNR |
| /SKN/S_SW_01_01_SRV_CPU | SERVER | Application Server Name | CHAR(40) | MSNAME2 |
| /SKN/S_SW_01_01_SRV_CPU | SUBTYPE | Subtype in saposcol result structure | INT2(5) | COLLSUBTYP |
| /SKN/S_SW_01_01_SRV_CPU | SYSC_SEC | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | SYS_TOTAL | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | TYPE | Type in saposcol result structure | INT2(5) | COLLTYPE |
| /SKN/S_SW_01_01_SRV_CPU | USR_TOTAL | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |
| /SKN/S_SW_01_01_SRV_CPU | WAIT_TRUE | Data for tables to call "SAPOSCOL" | INT4(10) | INT4_DATA |

## ABAP Code

```abap
  FUNCTION /SKN/F_SW_01_01_SRV_CPU.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SRV_CPU OPTIONAL
*"----------------------------------------------------------------------
DATA_MULTY: DURATION /SKN/E_SW_DURATION,
            SERVER   MSNAME2,
            HOST     MSHOST2,
            NBR_CPU  INT4_DATA,
            LOAD1_AVG  INT4_DATA,
            LOAD5_AVG  INT4_DATA,
            LOAD15_AVG  INT4_DATA,
            INT_SEC  INT4_DATA,
            SYSC_SEC  INT4_DATA,
            CS_SEC  INT4_DATA,
            USR_TOTAL  INT4_DATA,
            SYS_TOTAL  INT4_DATA,
            IDLE_TOTAL  INT4_DATA,
            IDLE_TRUE  INT4_DATA,
            WAIT_TRUE  INT4_DATA,
            CPU_USAGE  INT4_DATA.
            .
DATA_SINGLE: AGGR_LEVEL CHAR1.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             LANGU LANGU,
             NUMBER SY-TABIX,
             SUBRC SY-SUBRC,
             MANAGE_IN_UTC CHAR1.
DATA: LS_SRV_LIST TYPE  MSXXLIST,
      LT_SRV_LIST LIKE TABLE OF LS_SRV_LIST.
DATA: LV_LOGICAL_DESTINATION TYPE RFCDEST.
DATA: F_CPU_ALL_READ LIKE  DEF_PAR_FU-NO_YES,
      ACTIVEFLAG LIKE  DEF_PAR_FU-ACTIVEFLAG,
      INTERVAL LIKE  DEF_PAR_FU-INTERVAL,
      OP_SYSTEM LIKE  DEF_PAR_FU-OPSYSTEM,
      DETAILSCOLL LIKE  DEF_PAR_FU-DETAILSCOL,
      DETAILSREQI LIKE  DEF_PAR_FU-DETAILSREQ,
      DETAILSMODE LIKE  DEF_PAR_FU-DETAILSMOD,
      LASTCOLLWRT LIKE  DEF_PAR_FU-LASTCOLLWR,
      LASTCOLLINT LIKE  DEF_PAR_FU-LASTCOLLIN,
      NORMCOLLINT LIKE  DEF_PAR_FU-NORMCOLLIN.
DATA: LS_CPU_ALL TYPE CPU_ALL,
      LT_CPU_ALL LIKE TABLE OF LS_CPU_ALL.
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
DATA : SY_TABIX LIKE SY-TABIX .
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
DATA: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
DATA: LV_NO_SERVER_LIST(1) TYPE C.
*-- Fill Selection Option Tables
SELECT_MULTY: DURATION,
            SERVER,
            HOST,
            NBR_CPU,
            LOAD1_AVG,
            LOAD5_AVG,
            LOAD15_AVG,
            INT_SEC,
            SYSC_SEC,
            CS_SEC,
            USR_TOTAL,
            SYS_TOTAL,
            IDLE_TOTAL,
            IDLE_TRUE,
            WAIT_TRUE,
            CPU_USAGE.
 LV_DURATION_UNIT = 'M'.
 SELECT_SINGLE: DURATION_UNIT,
                MANAGE_IN_UTC,
                LANGU.
 SELECT_SINGLE: AGGR_LEVEL.
 SET_SY_TIME LV_MANAGE_IN_UTC SY_DATLO SY_TIMLO .
 TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
 PROCESS_ICON = ICON_DELETE.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  REFRESH: LT_DATA.
  CLEAR LV_NO_SERVER_LIST.
* get a list of all running instances & name of own instance
  CALL FUNCTION 'TH_SERVER_LIST'
     DESTINATION LV_SW_DEST
"      exporting
"           SERVICES = 255
       TABLES
            LIST     = LT_SRV_LIST
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
      CLEAR LS_SRV_LIST.
      "ls_SRV_LIST-NAME = lv_SW_DEST.
      APPEND LS_SRV_LIST TO LT_SRV_LIST.
  ENDIF.
    LOOP AT LT_SRV_LIST INTO LS_SRV_LIST.
      LV_LOGICAL_DESTINATION = LS_SRV_LIST-NAME.
      REFRESH LT_CPU_ALL.
      CALL FUNCTION 'GET_CPU_ALL'
        DESTINATION LV_SW_DEST
        EXPORTING
          LOCAL_REMOTE                         = 'INTERN'
          LOGICAL_DESTINATION                  = LV_LOGICAL_DESTINATION
        IMPORTING
          F_CPU_ALL_READ                       = F_CPU_ALL_READ
          ACTIVEFLAG                           = ACTIVEFLAG
          INTERVAL                             = INTERVAL
          OP_SYSTEM                            = OP_SYSTEM
          DETAILSCOLL                          = DETAILSCOLL
          DETAILSREQI                          = DETAILSREQI
          DETAILSMODE                          = DETAILSMODE
          LASTCOLLWRT                          = LASTCOLLWRT
          LASTCOLLINT                          = LASTCOLLINT
          NORMCOLLINT                          = NORMCOLLINT
        TABLES
          TF_CPU_ALL                           = LT_CPU_ALL
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
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      LOOP AT LT_CPU_ALL INTO LS_CPU_ALL.
        CLEAR: LS_DATA.
        MOVE-CORRESPONDING LS_CPU_ALL TO LS_DATA.
        LS_DATA-SERVER = LS_SRV_LIST-NAME.
        LS_DATA-HOST   = LS_SRV_LIST-HOST.
        LS_DATA-CPU_USAGE = 100 - LS_DATA-IDLE_TOTAL. "Calcing the current cpu % usage.
        APPEND LS_DATA TO LT_DATA.
      ENDLOOP.
    ENDLOOP.
***  delete lt_wp_total_info where WP_TYP not in R_WP_TYP.
***  delete lt_wp_total_info where WP_ISTATUS not in R_WP_ISTATUS.
 T_DATA[] = LT_DATA[].
****-- Fill Duration Value
*** sy_datlo = sy-datum.   "--- System Date/Time
*** sy_timlo = sy-uzeit.
*** loop at T_DATA .
***   sy_tabix = sy-tabix.
***   T_DATA-DURATION_UNIT = lv_DURATION_UNIT.
***   if t_data-STARTDATE is initial.
***     continue.
***   endif.
***    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
***        EXPORTING
***          D_FROM            = t_data-STARTDATE
***          T_FROM            = t_data-STARTTIME
***          D_TO              = sy_datlo
***          T_TO              = sy_timlo
***          TIME_UNIT         = lv_DURATION_UNIT
***        IMPORTING
***          TIME_DIFF         = TIME_DIFF
***        EXCEPTIONS
***          WRONG_VALUE       = 1
***          OTHERS            = 2    .
***      IF SY-SUBRC = 0.
***        T_DATA-DURATION = TIME_DIFF .
***      else.
***        T_DATA-DURATION = '999999'.
***      endif.
***   modify T_DATA index sy_tabix .
*** endloop .
 DELETE T_DATA WHERE SERVER NOT IN R_SERVER.
 DELETE T_DATA WHERE HOST NOT IN R_HOST.
 DELETE T_DATA WHERE NBR_CPU NOT IN R_NBR_CPU.
 DELETE T_DATA WHERE NBR_CPU NOT IN R_NBR_CPU.
 DELETE T_DATA WHERE LOAD1_AVG NOT IN R_LOAD1_AVG.
 DELETE T_DATA WHERE LOAD5_AVG NOT IN R_LOAD5_AVG.
 DELETE T_DATA WHERE LOAD15_AVG NOT IN R_LOAD15_AVG.
 DELETE T_DATA WHERE INT_SEC NOT IN R_INT_SEC.
 DELETE T_DATA WHERE SYSC_SEC NOT IN R_SYSC_SEC.
 DELETE T_DATA WHERE CS_SEC NOT IN R_CS_SEC.
 DELETE T_DATA WHERE USR_TOTAL NOT IN R_USR_TOTAL.
 DELETE T_DATA WHERE SYS_TOTAL NOT IN R_SYS_TOTAL.
 DELETE T_DATA WHERE IDLE_TOTAL NOT IN R_IDLE_TOTAL.
 DELETE T_DATA WHERE IDLE_TRUE NOT IN R_IDLE_TRUE.
 DELETE T_DATA WHERE WAIT_TRUE NOT IN R_WAIT_TRUE.
 DELETE T_DATA WHERE CPU_USAGE NOT IN R_CPU_USAGE.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
