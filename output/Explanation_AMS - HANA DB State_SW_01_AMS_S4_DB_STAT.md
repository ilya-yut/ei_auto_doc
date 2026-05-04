# Exception Indicator: AMS - HANA DB State - SW_01_AMS_S4_DB_STAT

## General Overview

This Exception Indicator (EI) monitors key SAP HANA database state metrics for AMS operations, including storage utilization, memory consumption, and CPU/thread indicators returned by the remote DB-state collector. It provides a compact operational view of infrastructure pressure points so teams can detect capacity and performance risks early.

This EI serves as an essential control for database health and service continuity by:

- Enabling early detection of storage, memory, and compute saturation trends before service degradation
- Supporting targeted capacity diagnostics by separating data, log, and trace storage dimensions
- Providing visibility into tenant and overall memory pressure for proactive tuning decisions
- Helping operations teams correlate CPU/thread stress with database resource consumption patterns
- Giving AMS stakeholders a repeatable, monitor-aligned snapshot for daily operational risk review

Typical uses include daily HANA health checks, pre-incident capacity surveillance, post-incident resource diagnostics, and recurring infrastructure governance reviews. Results support prioritized remediation and evidence-based escalation.

The function calls a dedicated DB-state calculation module on the target destination, returns core resource metrics, and applies configurable threshold filtering to produce alert-ready output.


## Problem Description

Failure to monitor HANA database state metrics continuously creates multiple risks across platform stability, service quality, and operational governance:

**Platform Stability and Capacity Risks**

- Data, log, or trace storage growth can approach exhaustion without timely intervention
- Memory saturation at total or tenant scope may degrade performance and increase failure probability
- CPU and thread pressure can rise unnoticed until workload responsiveness declines
- Delayed identification of infrastructure bottlenecks increases risk of service disruption
- Resource contention trends may persist across cycles when monitoring is not systematic

**Operational and Compliance Risks**

- AMS control frameworks require repeatable evidence of database health monitoring and response
- Inconsistent monitoring of core resource metrics weakens operational assurance and audit defensibility
- Manual point-in-time checks are difficult to compare and trend reliably
- Repeated unresolved capacity warnings can trigger recurring governance findings
- Lack of threshold-focused review reduces confidence in escalation timeliness

**Management Visibility and Decision-Making Risks**

- Leadership lacks a clear trend view of resource pressure across storage, memory, and compute domains
- Operations teams cannot prioritize remediation effectively when metrics are fragmented
- Capacity planning decisions are weakened without consistent utilization evidence
- Cross-team coordination slows when DB-state context is not unified in a single report
- Preventive maintenance becomes reactive when leading indicators are not reviewed consistently

## Suggested Resolution

**Immediate Response**

- Review flagged resource metrics and validate operational impact with AMS/Basis stakeholders
- Prioritize critical storage or memory pressure conditions for immediate containment actions
- Confirm whether anomalies are transient load spikes or sustained capacity risk patterns
- Open remediation tasks with explicit ownership and deadlines for affected domains
- Preserve output evidence for governance and incident follow-up

**System Assessment**

- Analyze storage, memory, and CPU/thread metrics together to identify dominant bottlenecks
- Compare current values with prior monitoring cycles to detect trend acceleration
- Correlate resource anomalies with workload changes, batch windows, or infrastructure events
- Validate threshold ranges and monitoring cadence against current operational risk appetite
- Document recurring root causes driving repeated DB-state exceptions

**Corrective Actions**

- Implement storage cleanup, archiving, or resizing actions where capacity risk is persistent
- Tune memory and workload settings to reduce sustained utilization pressure
- Optimize compute/thread behavior through workload management and scheduling adjustments
- Update monitoring thresholds and schedules with AMS stakeholders for sustained oversight
- Integrate recurring findings into formal capacity planning and problem-management workflows


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | CPUS | CPU State | CHAR | 255 | 0 | /SKN/E_SW_S4_CPUS | /SKN/D_SW_LTEXT |
| 2 | CPU_PROCESS | CPU_PROCESS | DEC | 20 | 2 | /SKN/E_SW_CPU_PROCESS | DEC20_2 |
| 3 | CPU_TOTAL | CPU_TOTAL | DEC | 20 | 2 | /SKN/E_SW_CPU_TOTAL | DEC20_2 |
| 4 | DB_MEMORY_TEN_TOTAL | MEMORY_TEN_TOTAL | DEC | 20 | 2 | /SKN/E_SW_DB_MEMORY_TEN_TOTAL | DEC20_2 |
| 5 | DB_MEMORY_TEN_USED | MEMORY_TEN_USED | DEC | 20 | 2 | /SKN/E_SW_DB_MEMORY_TEN_USED | DEC20_2 |
| 6 | DB_MEMORY_TOTAL | MEMORY_TOTAL | DEC | 20 | 2 | /SKN/E_SW_DB_MEMORY_TOTAL | DEC20_2 |
| 7 | DB_MEMORY_USED | MEMORY_USED | DEC | 20 | 2 | /SKN/E_SW_DB_MEMORY_USED | DEC20_2 |
| 8 | DB_STORAGE_DATA_TOTAL | DB_STORAGE_DATA_TOTAL | DEC | 20 | 2 | /SKN/E_SW_DB_STORAGE_DATA_TOT | DEC20_2 |
| 9 | DB_STORAGE_DATA_USED | DB_STORAGE_DATA_Used | DEC | 20 | 2 | /SKN/E_SW_DB_STORAGE_DATA_USED | DEC20_2 |
| 10 | DB_STORAGE_LOG_TOTAL | DB_STORAGE_LOG_TOTAL | DEC | 20 | 2 | /SKN/E_SW_DB_STORAGE_LOG_TOT | DEC20_2 |
| 11 | DB_STORAGE_LOG_USED | DB_STORAGE_LOG_USED | DEC | 20 | 2 | /SKN/E_SW_DB_STORAGE_LOG_USED | DEC20_2 |
| 12 | DB_STORAGE_TRACE_TOTAL | DB_STORAGE_TRACE_TOTAL | DEC | 20 | 2 | /SKN/E_SW_DB_STORAGE_TRACE_TOT | DEC20_2 |
| 13 | DB_STORAGE_TRACE_USED | DB_STORAGE_TRACE_USED | DEC | 20 | 2 | /SKN/E_SW_DB_STORAGE_TRACE_USE | DEC20_2 |
| 14 | THREADS | THREADS State | CHAR | 255 | 0 | /SKN/E_SW_S4_THREADS | /SKN/D_SW_LTEXT |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 14 parameters listed in the Parameters Reference Table above.

**CPUS** (CPU State):

CPU topology/context indicator returned by the remote DB-state collector; use for host-capacity context in AMS reviews.

**CPU_PROCESS** (CPU_PROCESS):

CPU consumption metric attributed to active processes; useful for identifying processing pressure.

**CPU_TOTAL** (CPU_TOTAL):

Total CPU utilization/capacity metric used to gauge overall compute load.

**DB_MEMORY_TEN_TOTAL** (MEMORY_TEN_TOTAL):

Total memory allocation for tenant scope; compare with used values for saturation analysis.

**DB_MEMORY_TEN_USED** (MEMORY_TEN_USED):

Used memory in tenant scope; key indicator for tenant-level pressure.

**DB_MEMORY_TOTAL** (MEMORY_TOTAL):

Total DB memory capacity metric across the monitored scope.

**DB_MEMORY_USED** (MEMORY_USED):

Used DB memory metric; central for memory saturation monitoring.

**DB_MEMORY_USED and DB_MEMORY_TOTAL Connection:**

Use together to evaluate memory saturation ratio and distinguish normal load from capacity-risk conditions.

**DB_STORAGE_DATA_TOTAL** (DB_STORAGE_DATA_TOTAL):

Total data storage capacity for the monitored database environment.

**DB_STORAGE_DATA_USED** (DB_STORAGE_DATA_Used):

Used data storage amount; primary indicator of data-volume saturation risk.

**DB_STORAGE_DATA_USED and DB_STORAGE_DATA_TOTAL Connection:**

Use together to assess data-storage headroom and growth pressure.

**DB_STORAGE_LOG_TOTAL** (DB_STORAGE_LOG_TOTAL):

Total log storage capacity.

**DB_STORAGE_LOG_USED** (DB_STORAGE_LOG_USED):

Used log storage amount; critical for log growth and retention risk.

**DB_STORAGE_LOG_USED and DB_STORAGE_LOG_TOTAL Connection:**

Use together to monitor log-volume headroom and prevent log-space exhaustion.

**DB_STORAGE_TRACE_TOTAL** (DB_STORAGE_TRACE_TOTAL):

Total trace storage capacity.

**DB_STORAGE_TRACE_USED** (DB_STORAGE_TRACE_USED):

Used trace storage amount; useful for diagnosing diagnostic/logging overhead.

**THREADS** (THREADS State):

Thread availability/usage context returned by the DB-state collector.


### Parameter Relationships

**Storage capacity pairs**

- **DB_STORAGE_DATA_USED** with **DB_STORAGE_DATA_TOTAL** indicates data-area saturation headroom.
- **DB_STORAGE_LOG_USED** with **DB_STORAGE_LOG_TOTAL** indicates log-area saturation headroom.
- **DB_STORAGE_TRACE_USED** with **DB_STORAGE_TRACE_TOTAL** indicates trace-area saturation headroom.

**Memory capacity pairs**

- **DB_MEMORY_USED** with **DB_MEMORY_TOTAL** indicates overall memory pressure.
- **DB_MEMORY_TEN_USED** with **DB_MEMORY_TEN_TOTAL** indicates tenant-scoped memory pressure.

**Compute context**

- **CPU_PROCESS** and **CPU_TOTAL** should be interpreted together to distinguish process-specific pressure from total CPU load.
- **CPUS** and **THREADS** provide topology/context dimensions for interpreting compute stress behavior.


### Default Values

No explicit default values are defined in the EI code for these business parameters; values are evaluated as supplied in the selection ranges.

### Practical Configuration Examples

**Use Case 1: Critical data-storage pressure**

```
DB_STORAGE_DATA_USED = 900000 - 999999999
DB_STORAGE_DATA_TOTAL = 1000000 - 999999999
```

**Purpose:** Isolates scenarios where data usage is near known high-capacity ranges for urgent storage review.

**Use Case 2: Log-area risk monitoring**

```
DB_STORAGE_LOG_USED = 700000 - 999999999
DB_STORAGE_LOG_TOTAL = 800000 - 999999999
```

**Purpose:** Focuses on high log consumption patterns that may threaten log-space headroom.

**Use Case 3: Tenant memory stress**

```
DB_MEMORY_TEN_USED = 500000 - 999999999
DB_MEMORY_TEN_TOTAL = 600000 - 999999999
CPU_PROCESS = 70 - 100
```

**Purpose:** Detects tenant memory pressure combined with elevated process CPU utilization.

**Use Case 4: Compute-heavy DB state snapshot**

```
CPU_TOTAL = 80 - 100
THREADS = *
CPUS = *
```

**Purpose:** Highlights high overall CPU conditions while retaining compute topology context.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_O1_AMS_S4_DB_STAT | CPUS | SW : S4 CPU State | CHAR(255) | /SKN/E_SW_S4_CPUS |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | CPU_PROCESS | SW - S4 CPU Process | DEC(20,2) | /SKN/E_SW_CPU_PROCESS |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | CPU_TOTAL | SW - S4 CPU Total | DEC(20,2) | /SKN/E_SW_CPU_TOTAL |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_MEMORY_TEN_TOTAL |  | DEC(20,2) | /SKN/E_SW_DB_MEMORY_TEN_TOTAL |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_MEMORY_TEN_USED |  | DEC(20,2) | /SKN/E_SW_DB_MEMORY_TEN_USED |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_MEMORY_TOTAL | SW - S4 DB Memory Total | DEC(20,2) | /SKN/E_SW_DB_MEMORY_TOTAL |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_MEMORY_USED | SW - S4 DB_Memory Used | DEC(20,2) | /SKN/E_SW_DB_MEMORY_USED |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_STORAGE_DATA_TOTAL |  | DEC(20,2) | /SKN/E_SW_DB_STORAGE_DATA_TOT |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_STORAGE_DATA_USED |  | DEC(20,2) | /SKN/E_SW_DB_STORAGE_DATA_USED |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_STORAGE_LOG_TOTAL |  | DEC(20,2) | /SKN/E_SW_DB_STORAGE_LOG_TOT |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_STORAGE_LOG_USED |  | DEC(20,2) | /SKN/E_SW_DB_STORAGE_LOG_USED |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_STORAGE_TRACE_TOTAL |  | DEC(20,2) | /SKN/E_SW_DB_STORAGE_TRACE_TOT |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | DB_STORAGE_TRACE_USED |  | DEC(20,2) | /SKN/E_SW_DB_STORAGE_TRACE_USE |
| /SKN/S_SW_O1_AMS_S4_DB_STAT | THREADS | SW : S4 Threads State | CHAR(255) | /SKN/E_SW_S4_THREADS |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_O1_AMS_S4_DB_STAT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_O1_AMS_S4_DB_STAT OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
DATA_MULTY: DB_STORAGE_DATA_TOTAL  DEC20_2,
            DB_STORAGE_DATA_USED   DEC20_2,
            DB_STORAGE_LOG_TOTAL   DEC20_2,
            DB_STORAGE_LOG_USED    DEC20_2,
            DB_STORAGE_TRACE_TOTAL DEC20_2,
            DB_STORAGE_TRACE_USED  DEC20_2,
            DB_MEMORY_USED         DEC20_2,
            DB_MEMORY_TOTAL        DEC20_2,
            DB_MEMORY_TEN_USED     DEC20_2,
            DB_MEMORY_TEN_TOTAL    DEC20_2,
            CPUS                   STRING,
            THREADS                STRING,
            CPU_TOTAL              DEC20_2,
            CPU_PROCESS            DEC20_2.
DATA_SINGLE: LANGU LANGU.
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LS_BUFFER_STATISTIC TYPE /SKN/S_SW_O1_AMS_TUNEHDWQ,
      LT_BUFFER_STATISTIC LIKE TABLE OF LS_BUFFER_STATISTIC.
DATA: LV_DB_STORAGE_DATA_TOTAL TYPE  DEC20_2,
      LV_DB_STORAGE_DATA_USED TYPE  DEC20_2,
      LV_DB_STORAGE_LOG_TOTAL TYPE  DEC20_2,
      LV_DB_STORAGE_LOG_USED TYPE  DEC20_2,
      LV_DB_STORAGE_TRACE_TOTAL TYPE  DEC20_2,
      LV_DB_STORAGE_TRACE_USED TYPE  DEC20_2,
      LV_DB_MEMORY_USED TYPE  DEC20_2,
      LV_DB_MEMORY_TOTAL TYPE  DEC20_2,
      LV_DB_MEMORY_TEN_USED TYPE  DEC20_2,
      LV_DB_MEMORY_TEN_TOTAL TYPE  DEC20_2,
      LV_CPUS TYPE  STRING,
      LV_THREADS TYPE  STRING,
      LV_CPU_TOTAL TYPE  DEC20_2,
      LV_CPU_PROCESS TYPE  DEC20_2.
DATA: SY_DATLO LIKE SY-DATLO ,
      SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
*-- Fill Selection Option Tables
  SELECT_MULTY: DB_STORAGE_DATA_TOTAL,
                DB_STORAGE_DATA_USED,
                DB_STORAGE_LOG_TOTAL,
                DB_STORAGE_LOG_USED,
                DB_STORAGE_TRACE_TOTAL,
                DB_STORAGE_TRACE_USED,
                DB_MEMORY_USED,
                DB_MEMORY_TOTAL,
                DB_MEMORY_TEN_USED,
                DB_MEMORY_TEN_TOTAL,
                CPUS,
                THREADS,
                CPU_TOTAL,
                CPU_PROCESS.
   SELECT_SINGLE: LANGU.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "--- Check Exitens Add-On FM
  CALL FUNCTION 'FUNCTION_EXISTS'
    DESTINATION  LV_SW_DEST
    EXPORTING
      FUNCNAME           = '/SKN/F_SW_CLC_S4_DB_STAT'
    EXCEPTIONS
      FUNCTION_NOT_EXIST = 1
      COMMUNICATION_FAILURE = 11
      SYSTEM_FAILURE        = 12
      OTHERS             =   9.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  CALL FUNCTION '/SKN/F_SW_CLC_S4_DB_STAT'
    DESTINATION LV_SW_DEST
    IMPORTING
      DB_STORAGE_DATA_TOTAL        = LV_DB_STORAGE_DATA_TOTAL
      DB_STORAGE_DATA_USED         = LV_DB_STORAGE_DATA_USED
      DB_STORAGE_LOG_TOTAL         = LV_DB_STORAGE_LOG_TOTAL
      DB_STORAGE_LOG_USED          = LV_DB_STORAGE_LOG_USED
      DB_STORAGE_TRACE_TOTAL       = LV_DB_STORAGE_TRACE_TOTAL
      DB_STORAGE_TRACE_USED        = LV_DB_STORAGE_TRACE_USED
      DB_MEMORY_USED               = LV_DB_MEMORY_USED
      DB_MEMORY_TOTAL              = LV_DB_MEMORY_TOTAL
      DB_MEMORY_TEN_USED           = LV_DB_MEMORY_TEN_USED
      DB_MEMORY_TEN_TOTAL          = LV_DB_MEMORY_TEN_TOTAL
      CPUS                         = LV_CPUS
      THREADS                      = LV_THREADS
      CPU_TOTAL                    = LV_CPU_TOTAL
      CPU_PROCESS                  = LV_CPU_PROCESS
    EXCEPTIONS
      SYSTEM_FAILURE        = 1
      COMMUNICATION_FAILURE = 2
      OTHERS                = 9.
    IF SY-SUBRC IS NOT INITIAL.
      EXIT.
    ENDIF.
    LS_DATA-DB_STORAGE_DATA_TOTAL        = LV_DB_STORAGE_DATA_TOTAL.
    LS_DATA-DB_STORAGE_DATA_USED         = LV_DB_STORAGE_DATA_USED.
    LS_DATA-DB_STORAGE_LOG_TOTAL         = LV_DB_STORAGE_LOG_TOTAL.
    LS_DATA-DB_STORAGE_LOG_USED          = LV_DB_STORAGE_LOG_USED.
    LS_DATA-DB_STORAGE_TRACE_TOTAL       = LV_DB_STORAGE_TRACE_TOTAL.
    LS_DATA-DB_STORAGE_TRACE_USED        = LV_DB_STORAGE_TRACE_USED.
    LS_DATA-DB_MEMORY_USED               = LV_DB_MEMORY_USED.
    LS_DATA-DB_MEMORY_TOTAL              = LV_DB_MEMORY_TOTAL.
    LS_DATA-DB_MEMORY_TEN_USED           = LV_DB_MEMORY_TEN_USED.
    LS_DATA-DB_MEMORY_TEN_TOTAL          = LV_DB_MEMORY_TEN_TOTAL.
    LS_DATA-CPUS                         = LV_CPUS.
    LS_DATA-THREADS                      = LV_THREADS.
    LS_DATA-CPU_TOTAL                    = LV_CPU_TOTAL.
    LS_DATA-CPU_PROCESS                  = LV_CPU_PROCESS.
 APPEND LS_DATA TO LT_DATA.
  DELETE LT_DATA WHERE DB_STORAGE_DATA_TOTAL NOT IN R_DB_STORAGE_DATA_TOTAL.
  DELETE LT_DATA WHERE DB_STORAGE_DATA_TOTAL NOT IN R_DB_STORAGE_DATA_TOTAL.
  DELETE LT_DATA WHERE DB_STORAGE_DATA_USED NOT IN R_DB_STORAGE_DATA_USED.
  DELETE LT_DATA WHERE DB_STORAGE_LOG_TOTAL NOT IN R_DB_STORAGE_LOG_TOTAL.
  DELETE LT_DATA WHERE DB_STORAGE_LOG_USED NOT IN R_DB_STORAGE_LOG_USED.
  DELETE LT_DATA WHERE DB_STORAGE_TRACE_TOTAL NOT IN R_DB_STORAGE_TRACE_TOTAL.
  DELETE LT_DATA WHERE DB_STORAGE_TRACE_USED NOT IN R_DB_STORAGE_TRACE_USED.
  DELETE LT_DATA WHERE DB_MEMORY_USED NOT IN R_DB_MEMORY_USED.
  DELETE LT_DATA WHERE DB_MEMORY_TOTAL NOT IN R_DB_MEMORY_TOTAL.
  DELETE LT_DATA WHERE DB_MEMORY_TEN_USED NOT IN R_DB_MEMORY_TEN_USED.
  DELETE LT_DATA WHERE DB_MEMORY_TEN_TOTAL NOT IN R_DB_MEMORY_TEN_TOTAL.
  DELETE LT_DATA WHERE CPUS NOT IN R_CPUS.
  DELETE LT_DATA WHERE THREADS NOT IN R_THREADS.
  DELETE LT_DATA WHERE CPU_TOTAL NOT IN R_CPU_TOTAL.
  DELETE LT_DATA WHERE CPU_PROCESS NOT IN R_CPU_PROCESS.
 T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
