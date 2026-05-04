# Exception Indicator: SD Billing Document – Count - SW_10_01_INV_CNT

## General Overview

This Exception Indicator (EI) monitors the count of SD billing documents over time by aggregating billing-document detail records into configurable time buckets (by day, hour, or minute). It relies on the same underlying billing-document selection as the SD Billing Document – Details EI, then groups results by billing date and creation time to produce document counts per bucket for trend analysis and volume monitoring.

This EI serves as an essential control for billing volume and trend oversight by:
- Enabling detection of unusual billing document volumes in specific time buckets that may indicate peaks, gaps, or anomalies requiring review
- Supporting identification of billing activity patterns by day, hour, or minute for capacity and process planning
- Providing visibility into billing document counts over time for month-end close and period comparisons
- Enabling analysis of billing volume trends without inspecting individual document records
- Supporting threshold-based monitoring when document counts per bucket exceed or fall below configured levels

This count-level monitoring helps organizations track billing throughput, spot unusual volume patterns, and align monitoring with the same selection criteria as the detail EI. The EI is particularly valuable for trend dashboards, volume reporting, and exception management based on document counts rather than detail records.

The EI uses the same billing document data as the SD Billing Document – Details EI (via a call to that function) and aggregates the result set by billing date and time to produce counts per bucket.


## Problem Description

Failure to monitor billing document counts by time bucket creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unmonitored billing volume trends may delay awareness of period-over-period changes in billing activity that affect revenue recognition and closing
- Unusual counts in specific time buckets (e.g. spikes or gaps) may indicate batch runs, errors, or duplicate processing that require investigation
- Lack of aggregated count visibility can make it harder to reconcile billing throughput with expectations and to prioritize follow-up

**Sales Operations and Control Risks**
- Billing document count anomalies by day or time window without monitoring may indicate process or system issues that go unnoticed
- Missing trend visibility by time bucket can obscure capacity or performance problems in billing execution
- Count-based thresholds (e.g. minimum or maximum documents per bucket) that are not monitored may be breached without alerting

**Management Visibility and Decision-Making Risks**
- Lack of count-level monitoring delays awareness of volume trends and exceptions that require management attention
- Unidentified count patterns can lead to missed opportunities for process improvement or resource allocation
- Count exceptions that require audit or compliance review may go unnoticed without targeted monitoring by time bucket

## Suggested Resolution

**Immediate Response**
- Review the billing document counts flagged by the EI to understand which time buckets and selection criteria drove the result set
- Verify whether unusual counts reflect legitimate volume (e.g. month-end runs) or possible errors or duplicates
- Check that the underlying detail selection (same as the SD Billing Document – Details EI) aligns with the intended scope
- Identify business context: planned billing waves, system changes, or data quality issues

**System Assessment**
- Analyze the aggregation period (day, hour, or minute) and time buckets to ensure they match the intended trend granularity
- Compare current counts by bucket to prior periods using the same criteria to spot trends or one-off spikes
- Review the relationship between count results and the detail EI output to ensure consistency and correct interpretation

**Corrective Actions**
- Adjust aggregation period or count thresholds so that future runs focus on the intended exception set
- Document findings and volume patterns for audit and management reporting
- Establish recurring EI execution to provide continuous visibility into billing document counts by time bucket
- Use count results together with the detail EI where drill-down to individual documents is required


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AGGR_PERIOD | Aggregation Period (D/H/M) |  | 0 | 0 |  |  |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | DOCS_CNT | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 4 | DURATION | Duration |  | 0 | 0 |  |  |
| 5 | DURATION_UNIT | Duration_Unit |  | 0 | 0 |  |  |
| 6 | ERZET | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 7 | FKDAT | Billing Date | DATS | 8 | 0 | FKDAT | DATUM |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 7 parameters listed in the Parameters Reference Table above.

**AGGR_PERIOD** (Aggregation Period (D/H/M)):

Defines the time granularity for grouping billing documents into buckets. The EI calls the SD Billing Document – Details function to retrieve detail records, then aggregates them by billing date and creation time. AGGR_PERIOD controls how creation time is truncated: by day (time cleared), by hour (minutes and seconds zeroed), or by minute (seconds zeroed). This determines the level of detail in the count output (one row per day, per hour, or per minute within the selection).

**AGGR_PERIOD Options:**
- **D**: Day — one count row per billing date (time portion cleared).
- **H**: Hour — one count row per billing date and hour (minutes and seconds zeroed).
- **M**: Minute — one count row per billing date and minute (seconds zeroed).

**BACKDAYS** (Backdays):

Number of days to look back from today when building the default date range for the underlying detail selection. Passed through to the SD Billing Document – Details function when no explicit date range is supplied. The same semantics as in the Details EI apply.

**DOCS_CNT** (Natural Number):

Count of billing documents per aggregation bucket in the EI output. Used to filter the result set so that only buckets whose document count falls within the supplied range are returned. Enables threshold-based monitoring (e.g. buckets with count greater than a minimum or within a range).

**DURATION** (Duration):

Duration value used by the underlying detail selection (passed through to the SD Billing Document – Details function). Used together with DURATION_UNIT to express age or time-window criteria for the billing documents that are then counted.

**DURATION_UNIT** (Duration_Unit):

Unit for DURATION, passed through to the underlying detail function. Determines how duration is interpreted (e.g. days, hours, minutes) when filtering the detail set before aggregation.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**ERZET** (Time):

Creation time of the billing document. Passed through to the underlying detail selection to restrict which billing documents are included by creation time. The same time field is used for aggregation when AGGR_PERIOD is H or M.

**FKDAT** (Billing Date):

Billing date of the document. Passed through to the underlying detail selection to restrict which billing documents are included by billing date. The EI groups output by this date (and by ERZET according to AGGR_PERIOD).


### Parameter Relationships

**Aggregation and Time Bucket Parameters:**

- **AGGR_PERIOD** defines how billing documents are grouped into count buckets: by day (D), hour (H), or minute (M). It works with **FKDAT** and **ERZET** in the output: FKDAT and ERZET (truncated per AGGR_PERIOD) form the aggregation key, and DOCS_CNT is the count per bucket.
- **FKDAT** and **ERZET** are passed through to the underlying detail selection and also define the aggregation dimensions for the count result. Set AGGR_PERIOD to choose the time granularity of the count output.

**Detail Selection Parameters (passed through to underlying EI):**

- **BACKDAYS**, **DURATION**, **DURATION_UNIT**, **FKDAT**, and **ERZET** are forwarded to the SD Billing Document – Details function. They define which billing documents are included before aggregation. Use them consistently with the Details EI when you want count and detail to align.

**Count Filter:**

- **DOCS_CNT** filters the count result set after aggregation. Use it to retain only buckets whose document count falls within the supplied range (e.g. buckets with count above a threshold).


### Default Values

- **AGGR_PERIOD** — Default: `D` (aggregation by day when not supplied; one count row per billing date).

### Practical Configuration Examples

**Use Case 1: Daily billing document counts**
```
AGGR_PERIOD = D
BACKDAYS = 30
```
**Purpose:** Obtain one count row per billing date for the last 30 days, for daily volume trend review and month-end comparison.

**Use Case 2: Hourly counts with minimum volume filter**
```
AGGR_PERIOD = H
BACKDAYS = 7
DOCS_CNT = 10 - 999999
```
**Purpose:** Count billing documents by billing date and hour for the last week, retaining only buckets with at least 10 documents for peak-hour and capacity analysis.

**Use Case 3: Minute-level aggregation for a short window**
```
AGGR_PERIOD = M
FKDAT = 20250101 - 20250131
DOCS_CNT = 1 - 100
```
**Purpose:** Count billing documents by date and minute for January 2025, retaining only buckets with 1–100 documents to focus on lower-activity periods.

**Use Case 4: Daily counts with duration filter**
```
AGGR_PERIOD = D
DURATION_UNIT = D
DURATION = 0 - 30
BACKDAYS = 60
DOCS_CNT = 5 - 500
```
**Purpose:** Count billing documents aggregated by day, for documents up to 30 days old, over a 60-day lookback, retaining buckets with 5–500 documents for trend and threshold monitoring.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_INV_CNT | FKDAT | Billing date for billing index and printout | DATS(8) | FKDAT |
| /SKN/S_SW_10_01_INV_CNT | ERZET | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_10_01_INV_CNT | DOCS_CNT | Count of billing documents | INT4(10) | /SKN/E_SW_CNT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_INV_CNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_INV_CNT OPTIONAL
*"----------------------------------------------------------------------
DATA: LV_ALERT   TYPE CHAR1,
      LS_DET     TYPE /SKN/S_SW_10_01_INV_DET,
      LT_DET     LIKE TABLE OF LS_DET,
      LS_CNT     LIKE LINE OF T_DATA,
      LT_CNT     LIKE TABLE OF LS_CNT.
DATA_SINGLE: AGGR_PERIOD CHAR1.
LV_AGGR_PERIOD = 'D'.
SELECT_SINGLE: AGGR_PERIOD.
DATA_MULTY: DOCS_CNT /SKN/E_SW_CNT.
SELECT_MULTY: DOCS_CNT.
REFRESH T_DATA.
CALL FUNCTION '/SKN/F_SW_10_01_INV_DET'
  IMPORTING
    IS_ALERT = LV_ALERT
  TABLES
    T_SELECT = T_SELECT
    T_DATA   = LT_DET.
IS_ALERT = LV_ALERT.
REFRESH LT_CNT.
LOOP AT LT_DET INTO LS_DET.
  LS_CNT-FKDAT    = LS_DET-FKDAT.
  LS_CNT-ERZET    = LS_DET-ERZET.
  CASE LV_AGGR_PERIOD.
    WHEN 'D'.
      CLEAR LS_CNT-ERZET.
    WHEN 'H'.
      LS_CNT-ERZET+2(4) = '0000'.
    WHEN 'M'.
      LS_CNT-ERZET+4(2) = '00'.
    WHEN OTHERS.
      CLEAR LS_CNT-ERZET.
  ENDCASE.
  LS_CNT-DOCS_CNT = 1.
  COLLECT LS_CNT INTO LT_CNT.
ENDLOOP.
IF R_DOCS_CNT[] IS NOT INITIAL.
  DELETE LT_CNT WHERE DOCS_CNT NOT IN R_DOCS_CNT.
ENDIF.
T_DATA[] = LT_CNT[].
READ TABLE T_DATA INDEX 1.
CHECK NOT SY-TFILL IS INITIAL.
IS_ALERT = 'X'.
ENDFUNCTION.
```
