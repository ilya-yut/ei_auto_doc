# Exception Indicator: AMS - Buffers Statistics - SW_01_AMS_BUFF_STAT

## General Overview

This Exception Indicator reports SAP application buffer statistics from the managed cloud monitoring path so operations teams can see allocation, utilization, and quality signals—such as hit ratio, free storage and object headroom, and database access counts—in one structured extract per buffer.

The EI supports routine health reviews of memory-related tuning objects by:
- Highlighting buffers whose measured hit quality or free space percentage falls outside expected operating bands
- Making it easier to compare object counts against configured maxima when investigating capacity or fragmentation symptoms
- Supporting follow-up when database access counters suggest unexpected persistence-layer pressure tied to specific buffers
- Giving Basis and platform owners a repeatable snapshot for audits, change windows, and post-maintenance validation

Organizations use buffer-level monitoring to catch regressions after upgrades or parameter changes, to justify tuning work with evidence, and to reduce time spent correlating scattered technical views during incidents. It is most valuable when you want a compact exception-oriented view rather than manual navigation through multiple monitoring transactions.

The function retrieves buffer statistics from the cloud integration path used by your AMS monitoring stack, applies the configured selection ranges, and returns rows aligned with the EI output structure for alerting and reporting.


## Problem Description

Failure to monitor application buffer statistics in a managed landscape creates multiple risks across system stability, operational control, and service quality:

**System Stability and Performance Risks**
- Memory-related hotspots can grow until dialog and batch response times degrade without a clear single transaction to blame
- Degraded buffer hit quality can remain invisible until CPU or database symptoms appear in unrelated monitoring views
- Pressure on buffer object pools can surface as intermittent failures during peak posting or batch peaks
- Fragmentation or sizing drift can accumulate across release cycles until a minor change triggers outsized impact
- Cross-component noise makes it harder to separate genuine buffer regression from normal business load variation

**Operational Control and Platform Risks**
- Change and release teams lack a simple before-and-after signal when memory-related parameters are adjusted
- Capacity reviews may rely on anecdotal evidence instead of comparable buffer metrics week over week
- Seasonal peaks can stress buffers in predictable ways that still surprise teams if thresholds are not trended
- Vendor or custom jobs can shift workload mixes in ways that alter buffer behavior without being documented as baseline changes
- Temporary project traffic can change utilization profiles while operations still assumes prior steady-state assumptions

**Management Visibility and Decision-Making Risks**
- Leadership may approve additional business volume without visibility into whether buffer headroom supports it
- Finance and operations planning can misalign when technical footprint signals are informal rather than comparable
- Audit and governance stakeholders lack a concise evidence trail that buffer health was monitored during sensitive periods
- Problem management slows when teams cannot quickly separate “one noisy buffer” from “landscape-wide memory pressure”
- Post-incident reviews lack a compact before-and-after picture tying remediation actions to measurable buffer movement

## Suggested Resolution

**Immediate Response**
- Review the flagged buffer statistics to see which utilization or quality dimensions drive the exception (hit ratio, free storage percentage, object headroom, database access)
- Validate whether the signal aligns with a known change window (release, maintenance, data migration) versus unexpected load
- Check whether the pattern is isolated to named buffers or appears broadly across the extract
- Coordinate with the application owner if user-facing symptoms correlate with the same monitoring window
- Capture business context (close activities, campaigns, interface bursts) so later analysis separates normal peaks from defects

**System Assessment**
- Compare current readings to prior periods using the same monitoring intent (apples-to-apples thresholds and scope)
- Examine whether degradation is gradual (trend) versus step-change (configuration or workload shift)
- Review related platform signals outside this EI when symptoms persist—database, application server load, and batch schedules
- Assess whether exceptions cluster in time-of-day or calendar patterns that match known operational cycles
- Validate that operational expectations for buffer behavior still match the current release and sizing assumptions

**Corrective Actions**
- Tune buffer or memory-related parameters when evidence shows sustained misalignment with service targets, following your change process
- Escalate to the platform or database team when database access counters suggest persistence-layer issues beyond pure buffer sizing
- Document monitoring threshold adjustments and the business rationale so future reviewers understand intent
- Schedule recurring checks during high-risk periods until stability is re-established
- Add targeted communication or knowledge-base notes for teams who operate buffers in your landscape, folded into ownership and runbook updates—without relying on a separate training subsection


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_OBJCTS | No.of active objects | INT4 | 10 | 0 | ACTOBJECTS | INT4 |
| 2 | ALLOC_SIZE | Alloc.adr.space | INT4 | 10 | 0 | MEMALLOCSZ | INT4 |
| 3 | AVAIL_SIZE | Storage available | INT4 | 10 | 0 | MEMAVAILSZ | INT4 |
| 4 | BUF_DESC | Buffer Description | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 5 | BUF_NAME | Buffer Name | CHAR | 5 | 0 | BUFNAME | BUFNAME |
| 6 | DB_ACCESS | Database accesses | INT4 | 10 | 0 | NODBACCESS | INT4 |
| 7 | FRAME_SIZE | Buffer frame length | INT4 | 10 | 0 | BFRAMESIZE | INT4 |
| 8 | FREE_OBJCTS | No.of free objects | INT4 | 10 | 0 | /SKN/E_FREE_OBJECTS | INT4 |
| 9 | FREE_OBJCTS_PRC | Free  Objects % | DEC | 5 | 2 | /SKN/E_FREE_OBJCTS_PRC |  |
| 10 | FREE_SIZE | Storage free | INT4 | 10 | 0 | /SKN/E_FREE_MEMSZ | INT4 |
| 11 | FREE_SIZE_PRC | Storage free % | DEC | 5 | 2 | /SKN/E_FREE_STORAGE_PRC |  |
| 12 | HITRATIO | Hit rate | DEC | 7 | 4 | BUFQUAL_HR | BUFQUAL |
| 13 | MAX_OBJCTS | Max. no.obj. | INT4 | 10 | 0 | MAXOBJECTS | INT4 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 13 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ACT_OBJCTS** (No.of active objects)

Combines with related filters so no.of active objects on ACT_OBJCTS refines which records remain for duration or state checks.

**ALLOC_SIZE** (Alloc.adr.space)

Interprets alloc.adr.space as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on ALLOC_SIZE.

**AVAIL_SIZE** (Storage available)

Supports escalation where storage available on AVAIL_SIZE signals ownership for follow-up between Basis and functional teams.

**BUF_DESC** (Buffer Description)

Mirrors how administrators slice operational lists: buffer description (BUF_DESC) is one lever that shapes which rows are comparable run over run.

**BUF_NAME** (Buffer Name)

When harmonized with related filters, buffer name on BUF_NAME isolates the highest-risk record families.

**DB_ACCESS** (Database accesses)

Separates cross-client noise from in-scope work when database accesses on DB_ACCESS correlates with client or user attributes.

**FRAME_SIZE** (Buffer frame length)

When populated, keeps the extract focused so buffer frame length (FRAME_SIZE) aligns with the intended triage slice.

**FREE_OBJCTS** (No.of free objects)

Ensures reporting respects no.of free objects constraints carried by FREE_OBJCTS.

**FREE_OBJCTS_PRC** (Free  Objects %)

Allows phased rollout: first widen FREE_OBJCTS_PRC for free  objects %, then tighten thresholds once baseline noise is understood.

**FREE_SIZE** (Storage free)

For distributed landscapes, storage free on FREE_SIZE often anchors which application server or destination appears in results.

**FREE_SIZE_PRC** (Storage free %)

Combines with related filters so storage free % on FREE_SIZE_PRC refines which records remain for duration or state checks.

**HITRATIO** (Hit rate)

Captures edge cases where hit rate (HITRATIO) must be non-default to reproduce a customer-specific monitoring scenario.

**MAX_OBJCTS** (Max. no.obj.)

Stabilizes week-over-week metrics by fixing max. no.obj. (MAX_OBJCTS) while allowing duration thresholds to move.


### Parameter Relationships

How parameter combinations work together

**Identification vs. measurement**

- **BUF_NAME** and **BUF_DESC** identify which buffer rows are in scope for the extract. They work together with the numeric measurements so teams can separate “which buffer” from “how it is behaving” in the same snapshot.

**Utilization and headroom**

- **ALLOC_SIZE**, **AVAIL_SIZE**, and **FREE_SIZE** describe sizing and remaining storage in consistent units; **FREE_SIZE_PRC** expresses the same headroom story as a percentage when allocation is non-zero. **FRAME_SIZE** complements the sizing picture by describing the buffer frame length in the same technical context.
- **MAX_OBJCTS**, **ACT_OBJCTS**, and **FREE_OBJCTS** describe object pool usage; **FREE_OBJCTS_PRC** summarizes free object headroom as a percentage when a maximum is present.

**Quality and activity signals**

- **HITRATIO** captures buffer quality from a hit-rate perspective and is typically read together with sizing and free-percentage signals when deciding whether a buffer is healthy or trending poorly.
- **DB_ACCESS** adds an activity-oriented counter that helps interpret whether observed buffer behavior coincides with higher database interaction for the same rows.

**Selection contract**

- The multivalued selection inputs define the ranges the monitor is allowed to surface. Any combination of thresholds applies together as filters on the returned rows, so tightening one dimension without revisiting related dimensions can change which buffers remain visible to alerting.


### Default Values

No default values in the monitoring profile.

### Practical Example of Parameter Configuration

**Use Case 1: Weak hit ratio with low free storage**

**Purpose:** Catch buffers that show both poor hit quality and critically low free storage percentage.

```
BUF_NAME = FULL
HITRATIO = 0 - 7000
FREE_SIZE_PRC = 0 - 1500
FREE_OBJCTS_PRC = 0 - 1000
```

**Use Case 2: Database access spike on a named buffer**

**Purpose:** Focus on a specific buffer when database access counters suggest unexpected persistence pressure.

```
BUF_NAME = TABL
DB_ACCESS = 5000 - 999999999
HITRATIO = 6000 - 10000
```

**Use Case 3: Object pool exhaustion risk**

**Purpose:** Highlight buffers where free objects are nearly exhausted relative to configured maxima.

```
MAX_OBJCTS = 100 - 999999999
FREE_OBJCTS = 0 - 5
FREE_OBJCTS_PRC = 0 - 500
ACT_OBJCTS = 90 - 999999999
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_O1_AMS_BUFF_STAT | ACT_OBJCTS | No. of active objects | INT4(10) | ACTOBJECTS |
| /SKN/S_SW_O1_AMS_BUFF_STAT | ALLOC_SIZE | Size of allocated address space | INT4(10) | MEMALLOCSZ |
| /SKN/S_SW_O1_AMS_BUFF_STAT | AVAIL_SIZE | Storage space available | INT4(10) | MEMAVAILSZ |
| /SKN/S_SW_O1_AMS_BUFF_STAT | BUF_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_O1_AMS_BUFF_STAT | BUF_NAME | Area name | CHAR(5) | BUFNAME |
| /SKN/S_SW_O1_AMS_BUFF_STAT | DB_ACCESS | No. of database accesses | INT4(10) | NODBACCESS |
| /SKN/S_SW_O1_AMS_BUFF_STAT | FRAME_SIZE | Length of the buffer frames | INT4(10) | BFRAMESIZE |
| /SKN/S_SW_O1_AMS_BUFF_STAT | FREE_OBJCTS | No. of free objects | INT4(10) | /SKN/E_FREE_OBJECTS |
| /SKN/S_SW_O1_AMS_BUFF_STAT | FREE_OBJCTS_PRC | Free objects % | DEC(5,2) | /SKN/E_FREE_OBJCTS_PRC |
| /SKN/S_SW_O1_AMS_BUFF_STAT | FREE_SIZE | Storage space free | INT4(10) | /SKN/E_FREE_MEMSZ |
| /SKN/S_SW_O1_AMS_BUFF_STAT | FREE_SIZE_PRC | Storage space free % | DEC(5,2) | /SKN/E_FREE_STORAGE_PRC |
| /SKN/S_SW_O1_AMS_BUFF_STAT | HITRATIO | Hit rate SAP buffer | DEC(7,4) | BUFQUAL_HR |
| /SKN/S_SW_O1_AMS_BUFF_STAT | MAX_OBJCTS | Maximum no. of objects | INT4(10) | MAXOBJECTS |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_O1_AMS_BUFF_STAT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_O1_AMS_BUFF_STAT OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
DATA_MULTY: BUF_NAME  BUFNAME,
            BUF_DESC  VAL_TEXT,
            HITRATIO  BUFQUAL_HR,
            ALLOC_SIZE MEMALLOCSZ,
            AVAIL_SIZE MEMAVAILSZ,
            FREE_SIZE  /SKN/E_FREE_MEMSZ,
            FREE_SIZE_PRC /SKN/E_FREE_STORAGE_PRC,
            FRAME_SIZE  BFRAMESIZE,
            MAX_OBJCTS  MAXOBJECTS,
            ACT_OBJCTS  ACTOBJECTS,
            FREE_OBJCTS /SKN/E_FREE_OBJECTS,
            FREE_OBJCTS_PRC /SKN/E_FREE_OBJCTS_PRC,
            DB_ACCESS   NODBACCESS.
DATA_SINGLE: LANGU LANGU.
DATA : SY_TABIX LIKE SY-TABIX .
DATA: LS_BUFFER_STATISTIC TYPE /SKN/S_SW_O1_AMS_TUNEHDWQ,
      LT_BUFFER_STATISTIC LIKE TABLE OF LS_BUFFER_STATISTIC.
DATA: SY_DATLO LIKE SY-DATLO ,
      SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
*-- Fill Selection Option Tables
   SELECT_MULTY: BUF_NAME,
                 BUF_DESC,
                 HITRATIO,
                 ALLOC_SIZE,
                 AVAIL_SIZE,
                 FREE_SIZE,
                 FREE_SIZE_PRC,
                 FRAME_SIZE,
                 MAX_OBJCTS,
                 ACT_OBJCTS,
                 FREE_OBJCTS,
                 FREE_OBJCTS_PRC,
                 DB_ACCESS.
   SELECT_SINGLE: LANGU.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  CALL FUNCTION '/SKN/F_SW_O1_AMS_BUFFERS_STAT'
    EXPORTING
      DEST                     = LV_SW_DEST
    TABLES
      T_BUFFER_STATISTIC       = LT_BUFFER_STATISTIC.
   LOOP AT LT_BUFFER_STATISTIC INTO LS_BUFFER_STATISTIC.
     CLEAR LS_DATA.
     MOVE-CORRESPONDING LS_BUFFER_STATISTIC TO LS_DATA.
     LS_DATA-BUF_NAME = LS_BUFFER_STATISTIC-NAME.
     LS_DATA-FREE_SIZE = LS_DATA-ALLOC_SIZE - LS_DATA-AVAIL_SIZE.
     IF LS_DATA-ALLOC_SIZE > 0.
       LS_DATA-FREE_SIZE_PRC = LS_DATA-FREE_SIZE / LS_DATA-ALLOC_SIZE * 100.
     ENDIF.
     LS_DATA-FREE_OBJCTS = LS_DATA-MAX_OBJCTS - LS_DATA-ACT_OBJCTS.
     IF LS_DATA-MAX_OBJCTS > 0.
       LS_DATA-FREE_OBJCTS_PRC = LS_DATA-FREE_OBJCTS / LS_DATA-MAX_OBJCTS * 100.
     ENDIF.
     APPEND LS_DATA TO LT_DATA.
   ENDLOOP.
   DELETE LT_DATA WHERE BUF_NAME NOT IN R_BUF_NAME.
   DELETE LT_DATA WHERE BUF_DESC NOT IN R_BUF_DESC.
   DELETE LT_DATA WHERE HITRATIO NOT IN R_HITRATIO.
   DELETE LT_DATA WHERE ALLOC_SIZE NOT IN R_ALLOC_SIZE.
   DELETE LT_DATA WHERE AVAIL_SIZE NOT IN R_AVAIL_SIZE.
   DELETE LT_DATA WHERE FREE_SIZE_PRC NOT IN R_FREE_SIZE_PRC.
   DELETE LT_DATA WHERE FRAME_SIZE NOT IN R_FRAME_SIZE.
   DELETE LT_DATA WHERE MAX_OBJCTS NOT IN R_MAX_OBJCTS.
   DELETE LT_DATA WHERE ACT_OBJCTS NOT IN R_ACT_OBJCTS.
   DELETE LT_DATA WHERE FREE_OBJCTS NOT IN R_FREE_OBJCTS.
   DELETE LT_DATA WHERE FREE_OBJCTS_PRC NOT IN R_FREE_OBJCTS_PRC.
   DELETE LT_DATA WHERE DB_ACCESS NOT IN R_DB_ACCESS.
 T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
