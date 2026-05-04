# Exception Indicator: System Dumps Count - SW_01_01_DUMPS_CNT

## General Overview

This Exception Indicator (EI) provides count-based monitoring for ABAP runtime dumps by wrapping detailed dump retrieval logic and returning one aggregated count result.

This EI helps by:
- Converting detailed dump events into a threshold-friendly count control
- Enabling quick alerting when dump volume exceeds expected limits
- Reusing proven dump-selection logic from the base System Dumps function
- Supporting operational stability governance with simple count output

The function executes the base dump-monitoring FM, counts returned dump records, and outputs the count when it matches configured DUMPS_CNT criteria.


## Problem Description

Without a dump-count control, teams may review individual dumps but miss aggregate surge conditions that indicate broader stability incidents.

**Operational and Process Risks**
- Dump spikes can go undetected until user/business impact grows
- Manual counting delays response and escalation
- Recurrent instability windows may remain hidden without thresholds

**Control and Compliance Risks**
- Missing count-level controls weakens stability-monitoring evidence
- Inconsistent threshold handling reduces comparability between periods
- Escalation criteria can become subjective without explicit count ranges

**Management Visibility Risks**
- Management may lack clear, fast indicators of current dump pressure
- Capacity and remediation planning are harder without simple surge metrics

### Suggested Resolution

**Immediate Response**
- Monitor dump count thresholds on agreed cadence
- Escalate when DUMPS_CNT breaches operational baseline
- Trigger detailed drill-down using the base System Dumps EI

**System Assessment**
- Validate threshold values against historical baseline
- Confirm supporting filters (host/user/program/include/date) are aligned to monitoring intent
- Review false-positive versus missed-event balance

**Corrective Actions**
- Standardize dump surge thresholds by landscape criticality
- Integrate count alerts with incident ownership workflow
- Periodically tune thresholds based on trend behavior


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | DUMPS_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 3 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 4 | DURATION_UNIT | Duration Unit(D/H/M) |  | 0 | 0 |  |  |
| 5 | INCLUDENAME | Include Name |  | 0 | 0 |  |  |
| 6 | PROGRAMNAME | Program Name |  | 0 | 0 |  |  |
| 7 | SYHOST | Host |  | 0 | 0 |  |  |
| 8 | SYUSER | User |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 8 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Days Backward from today):

BACKDAYS sets the fallback lookback window (days) used by the underlying dump retrieval logic when explicit date scope is absent.

**DUMPS_CNT** (Count):

DUMPS_CNT is the final threshold/range check against the counted number of dump records returned from the base function.

**DURATION** (Duration In Time Units):

DURATION is the aging threshold applied in the base function before records are counted.

**DURATION_UNIT** (Duration Unit(D/H/M)):

DURATION_UNIT defines time unit for DURATION evaluation in underlying dump processing.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**INCLUDENAME** (Include Name):

INCLUDENAME narrows the base dataset to include-level code context before final count is calculated.

**PROGRAMNAME** (Program Name):

PROGRAMNAME narrows base dump events by ABAP program ownership before count aggregation.

**SYHOST** (Host):

SYHOST scopes base dump selection by host to identify node-specific instability contributions to total count.

**SYUSER** (User):

SYUSER scopes base dump selection by user context for ownership-based surge analysis.


### Parameter Relationship

How parameter combinations work together

**Selection Scope and Count Outcome:**

- **SYHOST**, **SYUSER**, **PROGRAMNAME**, and **INCLUDENAME** narrow the dump event population in the base function.
- **BACKDAYS** provides fallback time scope when explicit date filters are not supplied to the base logic.

**Duration and Threshold Sequence:**

- **DURATION** + **DURATION_UNIT** are an additional (second) filter after date selection inside the base function.
- The wrapper then counts remaining records and checks the result against **DUMPS_CNT**.
- Final alert behavior depends on both base filtering and count-threshold match.


### Default Values
- **DURATION_UNIT** - H
- **BACKDAYS** - 1 (today and yesterday)

### Practical Example of Parameter Configuration
**Use Case 1: Short-window dump surge alert**

```plaintext
BACKDAYS = 1
DURATION = 1
DURATION_UNIT = H
DUMPS_CNT = 20 - 999999
```

**Purpose:** Alert when at least 20 dumps occurred in the recent short monitoring window.

**Use Case 2: Program-specific count spike**

```plaintext
PROGRAMNAME = SAPLZ_CUSTOM_POSTING
SYHOST = APP01
DURATION = 30
DURATION_UNIT = M
DUMPS_CNT = 10 - 999999
```

**Purpose:** Detect concentrated dump volume related to one program on one host.

**Use Case 3: User-context stability control**

```plaintext
SYUSER = BATCH_USER_01
INCLUDENAME = ZMM_IMPORT_INCLUDE
BACKDAYS = 2
DUMPS_CNT = 5 - 999999
```

**Purpose:** Monitor recurring dump accumulation linked to a user/include execution context.


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_DUMPS_CNT | DUMPS_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_DUMPS_CNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_DUMPS_CNT OPTIONAL
*"----------------------------------------------------------------------
DATA : LV_ALERT TYPE  CHAR1.
DATA : LS_DATA TYPE /SKN/S_SW_01_01_DUMPS,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA : LV_CNT TYPE I.
DATA_MULTY: DUMPS_CNT /SKN/E_SW_CNT.
SELECT_MULTY: DUMPS_CNT.
   REFRESH T_DATA.
   CALL FUNCTION '/SKN/F_SW_01_01_DUMPS'
    IMPORTING
       IS_ALERT       = LV_ALERT
     TABLES
       T_SELECT       = T_SELECT
       T_DATA         = LT_DATA.
    IS_ALERT = LV_ALERT.
    DESCRIBE TABLE LT_DATA LINES LV_CNT.
    IF LV_CNT IN R_DUMPS_CNT.
      T_DATA-DUMPS_CNT = LV_CNT.
      APPEND T_DATA.
      IS_ALERT = LV_ALERT.
    ELSE.
      CLEAR IS_ALERT.
    ENDIF.
ENDFUNCTION.
```
