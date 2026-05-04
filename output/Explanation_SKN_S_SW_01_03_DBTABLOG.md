# Exception Indicator: Control DBTABLOG - SW_01_03_DBTABLOG

## General Overview

This Exception Indicator summarizes database change logging activity from the `DBTABLOG` domain so operations and security teams can see which tables drive the largest change-record counts and data volumes in a compact extract.

The EI supports technical governance and stability reviews by:
- Highlighting tables whose logged change volume or row counts exceed expected operating bands
- Making it easier to compare growth or churn patterns table by table when investigating storage or performance symptoms
- Supporting follow-up when change logging volume spikes after releases, transports, or bulk maintenance
- Giving Basis and database owners a repeatable snapshot for audits, capacity reviews, and incident triage

Organizations use change-log style monitoring to catch unexpected table-level activity early, to validate remediation after configuration changes, and to document operational posture during sensitive periods. It is most useful when you need an exception-oriented signal rather than manual navigation through raw logging views alone.

The function retrieves aggregated counts and volumes per table name from the configured integration path, applies the declared selection ranges, and returns rows ready for alerting and reporting.


## Problem Description

Failure to monitor concentrated change logging volume at the table level creates multiple risks across system stability, operational control, and compliance:

**System Stability and Performance Risks**
- Rapid growth in logged changes can stress storage and I/O long before end users report generic slowness
- A small number of tables can dominate change volume and hide which objects actually drive pressure
- Short spikes during batch or data migration windows may never be correlated with the underlying table mix
- Fragmented visibility makes it harder to separate normal housekeeping from abnormal logging churn
- Cross-component symptoms can send teams on the wrong investigation path when the footprint is actually table-centric

**Operational Control and Platform Risks**
- Change and release teams lack a simple before-and-after signal when large data maintenance or custom jobs run
- Capacity reviews may rely on anecdotal evidence instead of comparable table-level metrics week over week
- Seasonal peaks can stress logging-related storage in predictable ways that still surprise teams if not trended
- Temporary project traffic can shift logging profiles without being documented as baseline changes
- Ownership gaps widen when no compact metric ties technical growth to a business or maintenance trigger

**Management Visibility and Compliance Risks**
- Leadership may approve additional online or batch volume without visibility into whether logging headroom supports it
- Audit and governance stakeholders lack concise evidence that logging posture was monitored during sensitive periods
- Problem management slows when teams cannot quickly isolate “one noisy table” from broader landscape pressure
- Post-incident reviews lack a compact before-and-after picture tying remediation actions to measurable logging movement
- External assessments become harder when monitoring discipline cannot be demonstrated with consistent artifacts

## Suggested Resolution

**Immediate Response**
- Review the flagged table-level snapshot to see which volume and count dimensions drive the exception relative to the monitoring intent
- Validate whether the signal aligns with a known change window (release, maintenance, data migration) versus unexpected activity
- Check whether the pattern is isolated to a small set of tables or appears broadly across the extract
- Coordinate with the application owner if business symptoms correlate with the same monitoring window
- Capture business context (close activities, campaigns, bulk corrections) so later analysis separates normal peaks from defects

**System Assessment**
- Compare current readings to prior periods using the same monitoring intent and threshold philosophy
- Examine whether degradation is gradual (trend) versus step-change (process or workload shift)
- Review related database and batch signals when symptoms persist—jobs, interfaces, and housekeeping schedules
- Assess whether exceptions cluster in time-of-day or calendar patterns that match known operational cycles
- Validate that operational expectations for logging growth still match the current release and landscape assumptions

**Corrective Actions**
- Tune housekeeping, archiving, or retention posture when evidence shows sustained misalignment with service targets, following your change process
- Escalate to the database or application owner when specific tables remain out of band after validation
- Document threshold adjustments and rationale so future reviewers understand intent
- Schedule recurring checks during high-risk periods until stability is re-established
- Fold communication and knowledge-base updates into ownership and runbook improvements under **Corrective Actions**—not as a separate training-only subsection


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | DATA_CNT | Number of records | INT4 | 10 | 0 | INT4 | INT4 |
| 2 | DATA_VOL | Table Volume | FLTP | 16 | 16 |  |  |
| 3 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 3 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**DATA_CNT** (Number of records)

When populated, keeps the extract focused so number of records (DATA_CNT) aligns with the intended triage slice.

**DATA_VOL** (Table Volume)

Documents expected operator behavior—table volume on DATA_VOL should be set when that dimension is part of the control objective.

**TABNAME** (Table Name)

Database table name used to scope change/object monitoring to specific tables.


### Parameter Relationships

How parameter combinations work together

**Table identity versus size signals**

- **TABNAME** identifies which database tables are in scope for the aggregated logging view. It works together with the numeric measures so teams can separate “which table” from “how large the logged footprint is” in the same snapshot.

**Volume and count interplay**

- **DATA_CNT** and **DATA_VOL** describe different dimensions of the same underlying logging aggregation: one emphasizes how many logged rows contribute, the other emphasizes the summed data length footprint. They are typically read together when deciding whether growth is row-dominant versus payload-dominant for the same table.

**Selection contract**

- The multivalued selection inputs define the ranges the monitor is allowed to surface. Any combination of thresholds applies together as filters on the returned rows, so tightening one dimension without revisiting related dimensions can change which tables remain visible to alerting.


### Default Values

No default values in the monitoring profile.

### Practical Example of Parameter Configuration

**Use Case 1: High row count and volume on critical tables**

**Purpose:** Catch tables where both logged row counts and summed data volume indicate sustained pressure.

```
TABNAME = MARA - MARA
DATA_CNT = 100000 - 999999999
DATA_VOL = 1E9 - 1E20
```

**Use Case 2: Watchlist table names**

**Purpose:** Focus monitoring on a small set of tables that matter for a release or audit window.

```
TABNAME = BKPF - BKPF
DATA_CNT = 1 - 999999999
```

**Use Case 3: Volume-only threshold**

**Purpose:** Flag extreme data volume even when row counts look moderate for the same table family.

```
TABNAME = * - *
DATA_VOL = 5E10 - 1E25
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_03_DBTABLOG | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_01_03_DBTABLOG | DATA_CNT | Number of records | INT4(10) | INT4 |
| /SKN/S_SW_01_03_DBTABLOG | DATA_VOL | Table Volume | FLTP(16) | FLTP |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_03_DBTABLOG.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_03_DBTABLOG OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_TABNAME FOR DBTABLOG-TABNAME ,
         R_DATA_CNT FOR /SKN/S_SW_SYS_DBTABLOG-DATA_CNT,
         R_DATA_VOL FOR /SKN/S_SW_SYS_DBTABLOG-DATA_VOL.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : IS_OUT(1) TYPE C.
*-- Fill Selection Option Tables
   LOOP AT T_SELECT WHERE FIELDNM = 'TABNAME'.
     MOVE-CORRESPONDING T_SELECT TO R_TABNAME.
     APPEND R_TABNAME.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'DATA_CNT'.
     MOVE-CORRESPONDING T_SELECT TO R_DATA_CNT.
     APPEND R_DATA_CNT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'DATA_VOL'.
     MOVE-CORRESPONDING T_SELECT TO R_DATA_VOL.
     APPEND R_DATA_VOL.
   ENDLOOP.
 "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_03_DBTABLOG'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
   SELECT  TABNAME                 "TCODE PROGNAME OPTYPE
           COUNT( * )
           SUM( DATALN )
      FROM DBTABLOG
      INTO (T_DATA-TABNAME , T_DATA-DATA_CNT,
            T_DATA-DATA_VOL)
      WHERE TABNAME IN R_TABNAME
      GROUP BY TABNAME.
     APPEND T_DATA.
    ENDSELECT .
  IF R_DATA_CNT[] IS NOT INITIAL.
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX .
      IF NOT T_DATA-DATA_CNT IN R_DATA_CNT.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF R_DATA_VOL[] IS NOT INITIAL.
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX .
      IF NOT T_DATA-DATA_VOL IN R_DATA_VOL.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
