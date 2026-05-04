# Exception Indicator: Global Work Process Overview - SW_01_20_SM66

## General Overview

This Exception Indicator consolidates application server work process utilization into a single result row per aggregation mode, so operations teams can see whether dialog, background, update, update-2, and spool capacity is balanced or trending toward saturation. It is intended for routine health checks during peak business windows, release weekends, and incident triage when users report slow response times.

This EI serves as an essential control for SAP Basis and application operations by:
- Surfacing concentrated utilization in specific work process types before end users experience widespread slowdowns
- Making it easier to compare “whole system” pressure versus pressure isolated to individual application servers
- Supporting escalation decisions when free capacity disappears while in-use counts climb
- Giving leadership a repeatable snapshot metric for operational reviews and capacity planning conversations
- Reducing guesswork during investigations by summarizing utilization dimensions that are normally spread across monitoring views

Organizations use this style of monitoring to catch emerging capacity risk early, validate remediation after configuration or hardware changes, and document operational posture during audits or service reviews. It is especially useful when you need a compact exception signal rather than a full interactive drill-down every time.

The EI reads work process snapshot data from the SAP application server layer (the same operational domain as transaction SM50) and returns a compact structure of totals, free and in-use counts, utilization percentages, and representative duration figures.


## Problem Description

Failure to monitor consolidated work process utilization creates multiple risks across service continuity, user productivity, and operational governance:

**Service Continuity and User Experience Risks**
- Dialog or background saturation can appear as “random” slowness long before formal incident records exist
- Spool and update work process shortages can delay printing and asynchronous processing without an obvious business trigger
- Capacity pressure concentrated on one application server can be missed when teams only review global dashboards
- Short spikes during batch peaks may never be correlated with the underlying work process mix that caused them
- Cross-component symptoms (updates backing up while dialog looks healthy) can send teams on the wrong investigation path

**Operational Control and Capacity Risks**
- Misallocated work process profiles can silently drift until a single peak exhausts a pool that was sized for a different workload
- Over-reliance on reactive paging hides recurring patterns that should drive permanent sizing or landscape changes
- Seasonal or month-end batch windows can create predictable stress that still surprises teams if not trended over time
- Vendor or custom jobs that consume background capacity may crowd out business-critical batch chains
- Temporary project traffic can shift utilization away from steady-state assumptions without being documented as a baseline change

**Management Visibility and Decision-Making Risks**
- Executives may approve new business volumes without visibility into whether the technical footprint can absorb the load
- Finance and operations planning can misalign when utilization signals are anecdotal instead of comparable week over week
- Audit and governance stakeholders lack a simple evidence trail that capacity monitoring was performed during sensitive periods
- Problem management becomes slower when teams cannot quickly separate “one noisy host” from “whole landscape pressure”
- Post-incident reviews lack a compact before/after picture that ties remediation actions to measurable utilization movement

## Suggested Resolution

**Immediate Response**
- Review the flagged utilization snapshot and identify which work process families drive the exception (dialog, background, update, spool)
- Validate whether the signal reflects a known change window (release, batch peak, maintenance) versus unexpected load
- Check whether the issue is localized to one application server or appears in total aggregation mode as landscape-wide pressure
- Coordinate with the application owner if user-facing slowdowns correlate with the same time window as the alert
- Capture the business context (campaign, close activities, interface bursts) so later analysis separates normal peaks from defects

**System Assessment**
- Compare current totals and percentages to the prior monitoring cycle using the same aggregation mode for apples-to-apples interpretation
- If server-level mode is used, rank hosts by the highest in-use counts and lowest free counts to find outliers
- Review recent transport, profile, or instance configuration changes that could change work process counts or distribution
- Examine batch job schedules and application jobs that overlap the alert window for competing background demand
- Validate whether update or spool queues show correlated symptoms in standard SAP monitoring views when deeper drill-down is needed

**Corrective Actions**
- Rebalance workload or reschedule non-critical jobs when recurring peaks exhaust predictable capacity windows
- Adjust instance/work process sizing or profile parameters when sustained utilization indicates a structural shortage rather than a one-off spike
- Escalate to hardware or virtualization teams when host-level saturation persists despite software-side tuning
- Document accepted risk thresholds and update monitoring thresholds when business volumes permanently step up
- Establish recurring review cadences (weekly operations, monthly capacity governance) using the same compact signal for trend tracking
- Route alerts to the correct on-call rotation (Basis, application operations, batch operations) based on the dominant work process type in the signal


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AGGR_LEVEL | Aggr. Level (T-total/S-server) |  | 0 | 0 |  |  |
| 2 | BTC_CNT | Total Background WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 3 | BTC_DURATION | Backgroung WP Duration In Min. | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 4 | BTC_FREE_CNT | Free Background WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 5 | BTC_FREE_PRC | % of Free Background WPs | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 6 | BTC_INUSE_CNT | Used Background WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 7 | BTC_INUSE_PRC | % of In Use Background WPs | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 8 | DIA_CNT | Total Dialog WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 9 | DIA_DURATION | Dialog WP Duration In Min. | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DIA_FREE_CNT | Free Dialog WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 11 | DIA_FREE_PRC | % of Free Dialog WPs | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 12 | DIA_INUSE_CNT | Used Dialog WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 13 | DIA_INUSE_PRC | % of In Use Dialog WPs | DEC | 5 | 2 | /SKN/E_SW_PRC | /SKN/D_SW_PRC |
| 14 | SPO_CNT | Total Spool WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 15 | SPO_FREE_CNT | Free Spool WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 16 | SPO_INUSE_CNT | Used Spool WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 17 | UP2_CNT | Total Update-2 WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 18 | UP2_FREE_CNT | Free Update-2 WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 19 | UP2_INUSE_CNT | Used Update-2 WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 20 | UPD_CNT | Total Update WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 21 | UPD_FREE_CNT | Free Update WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 22 | UPD_INUSE_CNT | Used Update WPs | INT2 | 5 | 0 | INT2 | INT2 |
| 23 | WP_SERVER | Server Name | CHAR | 20 | 0 | MSNAME | MSNAME |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 23 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**AGGR_LEVEL** (Aggr. Level (T-total/S-server))

Controls aggregation granularity for work process totals versus server-specific breakdown.

**AGGR_LEVEL Options:**
- T — Total aggregation across all servers.
- S — Server-level aggregation.

**BTC_CNT** (Total Background WPs)

Stabilizes week-over-week metrics by fixing total background wps (BTC_CNT) while allowing duration thresholds to move.

**BTC_DURATION** (Backgroung WP Duration In Min.)

When populated, keeps the extract focused so backgroung wp duration in min. (BTC_DURATION) aligns with the intended triage slice.

**BTC_FREE_CNT** (Free Background WPs)

When tightened, free background wps (BTC_FREE_CNT) removes rows that would otherwise dilute attention from failing or stuck cases.

**BTC_FREE_PRC** (% of Free Background WPs)

For distributed landscapes, % of free background wps on BTC_FREE_PRC often anchors which application server or destination appears in results.

**BTC_INUSE_CNT** (Used Background WPs)

Captures edge cases where used background wps (BTC_INUSE_CNT) must be non-default to reproduce a customer-specific monitoring scenario.

**BTC_INUSE_PRC** (% of In Use Background WPs)

Ensures reporting respects % of in use background wps constraints carried by BTC_INUSE_PRC.

**DIA_CNT** (Total Dialog WPs)

Reflects real administration where total dialog wps on DIA_CNT is routinely restricted to a single productive client or object family.

**DIA_DURATION** (Dialog WP Duration In Min.)

Uses dialog wp duration in min. from the source context so only records with DIA_DURATION inside declared values proceed.

**DIA_FREE_CNT** (Free Dialog WPs)

Stabilizes week-over-week metrics by fixing free dialog wps (DIA_FREE_CNT) while allowing duration thresholds to move.

**DIA_FREE_PRC** (% of Free Dialog WPs)

Helps monitoring stay readable by requiring % of free dialog wps (DIA_FREE_PRC) to match organizational or technical selectors when set.

**DIA_INUSE_CNT** (Used Dialog WPs)

Helps distinguish technical versus business attributes when used dialog wps on DIA_INUSE_CNT correlates with counters or status fields.

**DIA_INUSE_PRC** (% of In Use Dialog WPs)

Treats % of in use dialog wps as a discriminator between similar rows that would otherwise look identical in a raw extract.

**SPO_CNT** (Total Spool WPs)

Narrows retrieved rows where total spool wps (SPO_CNT) must match the configured selection for this monitor.

**SPO_FREE_CNT** (Free Spool WPs)

Separates cross-client noise from in-scope work when free spool wps on SPO_FREE_CNT correlates with client or user attributes.

**SPO_INUSE_CNT** (Used Spool WPs)

Improves readability of exported lists because used spool wps (SPO_INUSE_CNT) columns stay aligned with the configured filter intent.

**UP2_CNT** (Total Update-2 WPs)

Prevents accidental global scans when total update-2 wps (UP2_CNT) is meant to stay within a controlled application slice.

**UP2_FREE_CNT** (Free Update-2 WPs)

Allows phased rollout: first widen UP2_FREE_CNT for free update-2 wps, then tighten thresholds once baseline noise is understood.

**UP2_INUSE_CNT** (Used Update-2 WPs)

Mirrors how administrators slice operational lists: used update-2 wps (UP2_INUSE_CNT) is one lever that shapes which rows are comparable run over run.

**UPD_CNT** (Total Update WPs)

Separates cross-client noise from in-scope work when total update wps on UPD_CNT correlates with client or user attributes.

**UPD_FREE_CNT** (Free Update WPs)

Narrows retrieved rows where free update wps (UPD_FREE_CNT) must match the configured selection for this monitor.

**UPD_INUSE_CNT** (Used Update WPs)

Improves readability of exported lists because used update wps (UPD_INUSE_CNT) columns stay aligned with the configured filter intent.

**WP_SERVER** (Server Name)

Pairs with duration logic: once WP_SERVER passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.


### Parameter Relationships

How parameter combinations work together

**Aggregation mode and server identity**

- **AGGR_LEVEL** controls whether the returned row represents **all application servers combined** or **one server at a time**. When the mode keeps server identity, **WP_SERVER** is the key that ties each output row to a specific application server name; when the mode aggregates totals, server identity is intentionally cleared so counts represent the whole monitored population in one line.

**Counts, percentages, and duration fields**

- The **`*_CNT`** parameters (totals, free, in-use) are the building blocks the EI uses to compute the **`*_PRC`** percentage fields. Practically, when total dialog capacity is configured and non-zero, free and in-use dialog counts determine the free and in-use percentages for dialog work processes; the same pattern applies across background, update, update-2, and spool families.
- **DIA_DURATION** and **BTC_DURATION** are populated from measured work process duration samples gathered while classifying processes by type. They are intended to be read together with the utilization counts for the same work process family when you want both “how busy” and “how long running tasks are” in the same snapshot.

**Selection ranges vs. derived output fields**

- The multivalued selection inputs (for example server name and the numeric range selectors for counts, percentages, and durations) define what the monitor is allowed to surface. The output columns with the same business names carry the computed totals and derived percentages after aggregation and filtering, so configuration changes on the input side directly change which aggregated rows remain in the final extract.


### Default Values

- **AGGR_LEVEL** - T

### Practical Example of Parameter Configuration

**Use Case 1: Landscape-wide dialog pressure**

**Purpose:** Detect when dialog work processes are heavily utilized across the whole system, without splitting by host.

```
AGGR_LEVEL = T
DIA_CNT = 200 - 9999
DIA_INUSE_PRC = 85 - 100
DIA_FREE_CNT = 0 - 5
```

**Use Case 2: One server with constrained dialog free pool**

**Purpose:** Focus on a single application server when operations suspects a noisy instance.

```
AGGR_LEVEL = S
WP_SERVER = PRDAPP01
DIA_FREE_CNT = 0 - 2
```

**Use Case 3: Background saturation watch**

**Purpose:** Track background work process load when batch peaks are expected.

```
AGGR_LEVEL = T
BTC_CNT = 50 - 9999
BTC_INUSE_CNT = 40 - 9999
BTC_FREE_PRC = 0 - 15
BTC_DURATION = 30 - 999999
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_WP_TOT | BTC_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | BTC_DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_WP_TOT | BTC_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | BTC_FREE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_WP_TOT | BTC_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | BTC_INUSE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_WP_TOT | DIA_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | DIA_DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_WP_TOT | DIA_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | DIA_FREE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_WP_TOT | DIA_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | DIA_INUSE_PRC | SW : Percent | DEC(5,2) | /SKN/E_SW_PRC |
| /SKN/S_SW_01_01_WP_TOT | SPO_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | SPO_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | SPO_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UP2_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UP2_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UP2_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UPD_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UPD_FREE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | UPD_INUSE_CNT | 2 byte integer (signed) | INT2(5) | INT2 |
| /SKN/S_SW_01_01_WP_TOT | WP_SERVER | Server Name | CHAR(20) | MSNAME |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_WP_TOT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_WP_TOT OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_WP TYPE /SKN/S_SW_01_01_SM66,
      LT_WP LIKE TABLE OF LS_WP.
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
DATA: BEGIN OF LS_DURATION,
        WP_SERVER TYPE MSNAME,
        WP_TYP TYPE WPTYP,
        DURATION TYPE /SKN/E_SW_DURATION,
***        DIA_DURATION type /SKN/E_SW_DURATION,
***        BTC_DURATION type /SKN/E_SW_DURATION,
      END OF LS_DURATION,
      LT_DURATION LIKE TABLE OF LS_DURATION.
DATA: FLD(30) TYPE C.
FIELD-SYMBOLS: <FS_CNT> TYPE INT2,
               <FS_FREE> TYPE INT2,
               <FS_INUSE> TYPE INT2.
DATA: LV_WP_TYP(3) TYPE C.
DATA: SY_TABIX LIKE SY-TABIX.
DATA_MULTY: WP_SERVER  MSNAME.
DATA_MULTY: DIA_CNT INT2,
            BTC_CNT INT2,
            UPD_CNT INT2,
            UP2_CNT INT2,
            SPO_CNT INT2,
            DIA_FREE_CNT INT2,
            BTC_FREE_CNT INT2,
            UPD_FREE_CNT INT2,
            UP2_FREE_CNT INT2,
            SPO_FREE_CNT INT2,
            DIA_INUSE_CNT INT2,
            BTC_INUSE_CNT INT2,
            UPD_INUSE_CNT INT2,
            UP2_INUSE_CNT INT2,
            SPO_INUSE_CNT INT2,
            DIA_FREE_PRC /SKN/E_SW_PRC,
            BTC_FREE_PRC /SKN/E_SW_PRC,
            DIA_INUSE_PRC /SKN/E_SW_PRC,
            BTC_INUSE_PRC /SKN/E_SW_PRC,
            DIA_DURATION   /SKN/E_SW_DURATION,
            BTC_DURATION   /SKN/E_SW_DURATION.
.
DATA_SINGLE: AGGR_LEVEL CHAR1.
*-- Fill Selection Option Tables
SELECT_MULTY: WP_SERVER.
SELECT_MULTY: DIA_CNT,
              BTC_CNT,
              UPD_CNT,
              UP2_CNT,
              SPO_CNT,
              DIA_FREE_CNT,
              BTC_FREE_CNT,
              UPD_FREE_CNT,
              UP2_FREE_CNT,
              SPO_FREE_CNT,
              DIA_INUSE_CNT,
              BTC_INUSE_CNT,
              UPD_INUSE_CNT,
              UP2_INUSE_CNT,
              SPO_INUSE_CNT,
              DIA_FREE_PRC,
              BTC_FREE_PRC,
              DIA_INUSE_PRC,
              BTC_INUSE_PRC,
              DIA_DURATION,
              BTC_DURATION.
 LV_AGGR_LEVEL = 'T'.
 SELECT_SINGLE: AGGR_LEVEL.
    CALL FUNCTION '/SKN/F_SW_01_01_SM66'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = LT_WP.
     SORT LT_WP BY WP_SERVER WP_TYP.
     LOOP AT LT_WP INTO LS_WP.
       CLEAR LS_DATA.
       IF LV_AGGR_LEVEL <> 'S'.
         CLEAR LS_DATA-WP_SERVER.
       ELSE.
         LS_DATA-WP_SERVER = LS_WP-WP_SERVER.
       ENDIF.
       LV_WP_TYP = LS_WP-WP_TYP.
       IF LV_WP_TYP = 'BGD'.
         LV_WP_TYP = 'BTC'.
       ENDIF.
       CONCATENATE LV_WP_TYP '_CNT' INTO FLD.
        ASSIGN COMPONENT FLD OF STRUCTURE LS_DATA TO <FS_CNT>.
        IF <FS_CNT> IS ASSIGNED.
          ADD 1 TO <FS_CNT>.
        ENDIF.
       CONCATENATE LV_WP_TYP '_FREE_CNT' INTO FLD.
        ASSIGN COMPONENT FLD OF STRUCTURE LS_DATA TO <FS_FREE>.
        IF <FS_FREE> IS ASSIGNED.
          IF LS_WP-WP_ISTATUS = '2'.  " Waiting
            ADD 1 TO <FS_FREE>.
          ENDIF.
        ENDIF.
       CONCATENATE LV_WP_TYP '_INUSE_CNT' INTO FLD.
        ASSIGN COMPONENT FLD OF STRUCTURE LS_DATA TO <FS_INUSE>.
        IF <FS_INUSE> IS ASSIGNED.
          IF LS_WP-WP_ISTATUS <> '2'.
            ADD 1 TO <FS_INUSE>.
          ENDIF.
        ENDIF.
        COLLECT LS_DATA INTO LT_DATA.
        MOVE-CORRESPONDING LS_WP TO LS_DURATION.
        LS_DURATION-WP_SERVER = LS_DATA-WP_SERVER.
        IF LS_WP-DURATION > 0.
          APPEND LS_DURATION TO LT_DURATION.
        ENDIF.
     ENDLOOP.
     SORT LT_DURATION BY WP_SERVER WP_TYP DURATION.
     LOOP AT LT_DATA INTO LS_DATA.
       SY_TABIX = SY-TABIX.
       IF LS_DATA-DIA_CNT <> 0.
         LS_DATA-DIA_FREE_PRC = LS_DATA-DIA_FREE_CNT / LS_DATA-DIA_CNT * 100.
         LS_DATA-DIA_INUSE_PRC = LS_DATA-DIA_INUSE_CNT / LS_DATA-DIA_CNT * 100.
       ENDIF.
       IF LS_DATA-BTC_CNT IS NOT INITIAL.
         LS_DATA-BTC_FREE_PRC = LS_DATA-BTC_FREE_CNT / LS_DATA-BTC_CNT * 100.
         LS_DATA-BTC_INUSE_PRC = LS_DATA-BTC_INUSE_CNT / LS_DATA-BTC_CNT * 100.
       ENDIF.
       READ TABLE LT_DURATION INTO LS_DURATION
                              WITH KEY WP_SERVER = LS_DATA-WP_SERVER
                                       WP_TYP = 'BTC'.
       IF SY-SUBRC IS INITIAL.
         LS_DATA-BTC_DURATION = LS_DURATION-DURATION.
       ENDIF.
       "---
       READ TABLE LT_DURATION INTO LS_DURATION
                              WITH KEY WP_SERVER = LS_DATA-WP_SERVER
                                       WP_TYP = 'DIA'.
       IF SY-SUBRC IS INITIAL.
         LS_DATA-DIA_DURATION = LS_DURATION-DURATION.
       ENDIF.
       MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
     ENDLOOP.
  DELETE LT_DATA WHERE: DIA_CNT NOT IN R_DIA_CNT,
                        BTC_CNT NOT IN R_BTC_CNT,
                        UPD_CNT NOT IN R_UPD_CNT,
                        UP2_CNT NOT IN R_UP2_CNT,
                        SPO_CNT NOT IN R_SPO_CNT,
                        DIA_FREE_CNT NOT IN R_DIA_FREE_CNT,
                        BTC_FREE_CNT NOT IN R_BTC_FREE_CNT,
                        UPD_FREE_CNT NOT IN R_UPD_FREE_CNT,
                        UP2_FREE_CNT NOT IN R_UP2_FREE_CNT,
                        SPO_FREE_CNT NOT IN R_SPO_FREE_CNT,
                        DIA_INUSE_CNT NOT IN R_DIA_INUSE_CNT,
                        BTC_INUSE_CNT NOT IN R_BTC_INUSE_CNT,
                        UPD_INUSE_CNT NOT IN R_UPD_INUSE_CNT,
                        UP2_INUSE_CNT NOT IN R_UP2_INUSE_CNT,
                        SPO_INUSE_CNT NOT IN R_SPO_INUSE_CNT,
                        DIA_FREE_PRC NOT IN R_DIA_FREE_PRC,
                        BTC_FREE_PRC NOT IN R_BTC_FREE_PRC,
                        DIA_INUSE_PRC NOT IN R_DIA_INUSE_PRC,
                        BTC_INUSE_PRC NOT IN R_BTC_INUSE_PRC,
                        DIA_DURATION  NOT IN R_DIA_DURATION,
                        BTC_DURATION  NOT IN R_BTC_DURATION.
  T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
