# Exception Indicator: qRFC - Outbound - SW_01_02_QRFC_OUT_CNT

## General Overview

This Exception Indicator (EI) monitors outbound qRFC queues and identifies queue destinations or queue names with abnormal item volume.

This EI helps by:
- Highlighting outbound queues with elevated transactional load
- Narrowing review by queue, destination, queue state, user, and function module
- Supporting operational checks for stuck or overloaded qRFC processing
- Providing count-based evidence for queue monitoring and escalation

The function aggregates outbound qRFC records from table TRFCQOUT, applies selection filters, and returns grouped queue counts.


## Problem Description

When outbound qRFC queues are not monitored by count and state, backlog growth or processing instability can remain unnoticed until integration failures appear.

**Operational and Process Risks**
- Queue build-up can delay outbound processing to dependent systems
- High-volume queues may hide repeated failures or throughput bottlenecks
- Queue-state patterns can indicate persistent processing interruptions

**Control and Compliance Risks**
- Lack of queue-volume monitoring weakens control evidence for interface operations
- Delayed detection of queue anomalies increases remediation lead time
- Inconsistent queue review makes trend comparison difficult

**Management Visibility Risks**
- Queue pressure hotspots may remain hidden without grouped counts
- Planning and prioritization are harder without destination and queue segmentation

## Suggested Resolution

**Immediate Response**
- Review queue groups with highest item counts
- Prioritize problematic queue states for technical follow-up
- Escalate critical destination-related queue accumulation

**System Assessment**
- Analyze queue trends by destination, queue name, and state
- Validate filter settings for queue user and function module
- Check whether count thresholds align with operational capacity

**Corrective Actions**
- Introduce periodic monitoring of queue count outliers
- Refine queue-level alert thresholds and escalation paths
- Document queue remediation outcomes for recurring issues


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | CNT | Items Count | INT4 | 10 | 0 | INT4 | INT4 |
| 2 | DEST | RFC Destination | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 3 | MANDT | Client | CLNT | 3 | 0 | SYMANDT | MANDT |
| 4 | QNAME | Queue | CHAR | 24 | 0 | TRFCQNAM | TRFCQNAM |
| 5 | QRFCFNAM | RFC FM |  | 0 | 0 |  |  |
| 6 | QRFCUSER | RFC User |  | 0 | 0 |  |  |
| 7 | QSTATE | Transactional tRFC queue statu |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 7 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**CNT** (Items Count)

Supports operational control by evaluating items count through CNT for each candidate record.

**DEST** (RFC Destination)

Supports escalation where rfc destination on DEST signals ownership for follow-up between Basis and functional teams.

**MANDT** (Client)

Narrows retrieved rows where client (MANDT) must match the configured selection for this monitor.

**QNAME** (Queue)

Connects to alert semantics: rows removed for failing queue on QNAME never reach downstream filtering.

**QRFCFNAM** (RFC FM)

Reflects real administration where rfc fm on QRFCFNAM is routinely restricted to a single productive client or object family.

**QRFCUSER** (RFC User)

When left open per framework rules, QRFCUSER does not restrict rfc user; when set, only matching rows remain.

**QSTATE** (Transactional tRFC queue statu)

Limits rows to the queue or processing state values you declare, so monitoring can target only selected outcome bands.

**QSTATE Options:**
- Use standard SAP status values configured for the monitored object type.
- Code in this EI applies QSTATE as a selector but does not enumerate fixed literals inline.


### Parameter Relationship

How parameter combinations work together

**Queue Scope Controls:**

- **MANDT**, **QNAME**, and **DEST** define which outbound queue groups are included in the base dataset.
- **QSTATE**, **QRFCUSER**, and **QRFCFNAM** add processing-state and ownership/function context to that dataset.

**Count Filtering Logic:**

- The function first groups data by queue client, name, and destination and calculates item counts.
- **CNT** is applied after grouping as a final threshold/range filter.
- Final result keeps queue groups that satisfy both scope filters and count conditions.

**Volume and Prioritization:**

- Combining **QSTATE** with **CNT** helps prioritize states with high queue volume for remediation.
- Combining **DEST** with **CNT** helps identify destination-specific queue pressure.


### Default Values
No default values are defined for this EI.

### Practical Example of Parameter Configuration
**Use Case 1: High-volume outbound queue review by destination**

```plaintext
DEST = RFC_PRD_01
QSTATE = RUNNING
CNT = 100 - 999999
```

**Purpose:** Focus monitoring on queue groups with high item counts for a specific destination and active processing state.

**Use Case 2: User and function-specific queue anomaly check**

```plaintext
QRFCUSER = BATCH_USER_01
QRFCFNAM = Z_IF_OUTBOUND_SEND
QNAME = OUTBOUND_QUEUE_A
CNT = 50 - 999999
```

**Purpose:** Identify whether a specific user/function integration flow is generating unusually large queue backlog.

**Use Case 3: Client-level queue pressure snapshot**

```plaintext
MANDT = 100
QSTATE = SYSFAIL
DEST = RFC_EXT_02
CNT = 10 - 999999
```

**Purpose:** Detect failed outbound queues with non-trivial volume in one client and destination context for faster escalation.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_02_QRFC_OUT_CNT | CNT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_02_QRFC_OUT_CNT | DEST | Logical Destination (Specified in Function Call) | CHAR(32) | RFCDEST |
| /SKN/S_SW_01_02_QRFC_OUT_CNT | MANDT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_02_QRFC_OUT_CNT | QNAME | Name of tRFC Queue | CHAR(24) | TRFCQNAM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_02_QRFC_OUT_CNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_QRFC_OUT_CNT OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_MANDT FOR TRFCQIN-MANDT,
         R_QNAME FOR TRFCQIN-QNAME,
         R_DEST FOR TRFCQIN-DEST,
         R_CNT FOR /SKN/S_SW_SYS_QRFC_OUT_CNT-CNT,
         R_QSTATE FOR TRFCQIN-QSTATE,
         R_QRFCUSER FOR TRFCQIN-QRFCUSER,
         R_QRFCFNAM FOR TRFCQIN-QRFCFNAM,
         R_QRFCDATUM FOR TRFCQIN-QRFCDATUM.
DATA : WA TYPE /SKN/S_SW_SYS_QRFC_OUT_CNT.
DATA : SY_TABIX LIKE SY-TABIX .
*data : TIME_DIFF TYPE  INT4 .
DATA : IS_OUT(1) TYPE C.
*
*-- Fill Selection Option Tables
   LOOP AT T_SELECT WHERE FIELDNM = 'MANDT'.
     MOVE-CORRESPONDING T_SELECT TO R_MANDT.
     APPEND R_MANDT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'QNAME'.
     MOVE-CORRESPONDING T_SELECT TO R_QNAME.
     APPEND R_QNAME.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'DEST'.
     MOVE-CORRESPONDING T_SELECT TO R_DEST.
     APPEND R_DEST.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'CNT'.
     MOVE-CORRESPONDING T_SELECT TO R_CNT.
     APPEND R_CNT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'QSTATE'.
     MOVE-CORRESPONDING T_SELECT TO R_QSTATE.
     APPEND R_QSTATE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'QRFCUSER'.
     MOVE-CORRESPONDING T_SELECT TO R_QRFCUSER.
     APPEND R_QRFCUSER.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'QRFCFNAM'.
     MOVE-CORRESPONDING T_SELECT TO R_QRFCFNAM.
     APPEND R_QRFCFNAM.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'QRFCDATUM'.
     MOVE-CORRESPONDING T_SELECT TO R_QRFCDATUM.
     APPEND R_QRFCDATUM.
   ENDLOOP.
   "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_02_QRFC_OUT_CNT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
*
*   LOOP AT T_SELECT WHERE FIELDNM = 'DURATION_M'.
*     MOVE-CORRESPONDING T_SELECT TO R_DURATION_M.
*     APPEND R_DURATION_M.
*   ENDLOOP.
*   LOOP AT T_SELECT WHERE FIELDNM = 'DURATION_H'.
*     MOVE-CORRESPONDING T_SELECT TO R_DURATION_H.
*     APPEND R_DURATION_H.
*   ENDLOOP.
*
*
*   LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
*     MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
*     APPEND R_STATE_COLOR.
*   ENDLOOP.
*   LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
*     MOVE-CORRESPONDING T_SELECT TO R_DATUM.
*     APPEND R_DATUM.
*   ENDLOOP.
*   if R_DATUM[] is initial .
*     LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
*       R_DATUM-SIGN = 'I' .
*        R_DATUM-OPTION = 'GE' .
*         BACKDAYS = T_SELECT-LOW .
*         DATE_FROM = sy-datum - BACKDAYS .
*         R_DATUM-LOW = DATE_FROM .
*         APPEND R_DATUM.
*         exit.
*     ENDLOOP.
*     if R_DATUM[] is initial .
*       R_DATUM-SIGN = 'I' .
*        R_DATUM-OPTION = 'GE' .
*         BACKDAYS = 1 .
*         DATE_FROM = sy-datum - BACKDAYS .
*         R_DATUM-LOW = DATE_FROM .
*       APPEND R_DATUM.
*     endif .
*   endif.
*
*   if R_ARFCDATUM[] is initial.
*     R_ARFCDATUM[] = R_DATUM[].
*   endif.
*
*  "-----
*
*
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
*
  SELECT MANDT QNAME DEST COUNT( * )
     FROM TRFCQOUT
     INTO (WA-MANDT, WA-QNAME, WA-DEST, WA-CNT)
     WHERE QNAME IN R_QNAME
       "and mandt in R_MANDT
       AND QSTATE IN R_QSTATE
       AND QRFCUSER IN R_QRFCUSER
       AND QRFCFNAM IN R_QRFCFNAM
       AND QRFCDATUM IN R_QRFCDATUM
       AND DEST     IN R_DEST
     GROUP BY MANDT QNAME DEST.
     MOVE-CORRESPONDING WA TO T_DATA.
     APPEND T_DATA.
  ENDSELECT.
*
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-CNT IN R_CNT.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
