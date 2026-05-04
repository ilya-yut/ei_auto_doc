# Exception Indicator: qRFC - Inbound - SW_01_02_QRFC_IN_CNT

## General Overview

This Exception Indicator monitors inbound qRFC queue traffic and surfaces queue names or processing contexts where item counts exceed the thresholds you configure, so integration teams can react before dependent processes stall.

This EI helps by:
- Highlighting inbound queues with elevated transactional load against grouped counts
- Narrowing review by client, queue name, queue state, user, and function module associated with the queue
- Supporting recurring operational checks for backlog growth or unstable queue states after releases or partner changes
- Giving auditors a repeatable, parameter-driven slice of queue metrics instead of ad hoc transaction reviews
- Helping Basis and application support prioritize the few queue keys that drive most of the volume in a monitoring pass

Typical use includes daily integration health checks, troubleshooting after transport or configuration waves, and validating cleanup when batch interfaces were restarted. Teams still use standard qRFC administration when a single queue needs immediate technical action.

The routine reads inbound queue data, applies multivalued selection from the parameter sheet, aggregates counts, and returns rows that satisfy the configured count filter for alerting.


## Problem Description

Failure to monitor inbound qRFC queue volume and state patterns creates multiple risks across interface stability, dependent process execution, and control evidence.

**Operational and Integration Risks**
- Queue build-up can delay inbound processing before dependent applications show hard errors
- High-volume queues may hide recurring retries or throughput limits that only surface under peak load
- Without grouped counts, teams cannot quickly see which queue keys warrant immediate attention

**Control and Evidence Risks**
- Lack of structured queue monitoring weakens evidence that interface operations were supervised during critical windows
- Delayed detection of abnormal queue states increases remediation time when downstream partners already see symptoms
- Inconsistent review makes trend comparison and ownership hand-offs harder to document

**Management Visibility Risks**
- Leadership lacks a compact view of where inbound pressure concentrates across clients and queue families
- Capacity and prioritization decisions suffer when queue hotspots are not visible in a single monitoring pass
- Cross-team alignment slows when operations and integration support do not share the same filtered view of risk

## Suggested Resolution

**Immediate Response**
- Review the highest-count queue groups first and validate whether the business context is still within agreed service expectations
- Open the standard inbound qRFC monitoring path that fits your landscape to confirm current queue status and ownership
- Capture time window and dominant queue attributes when the finding ties to regulated or financially material interfaces
- Segment results by client and queue name before drilling into user or function-module context
- Confirm whether the spike aligns with a known batch window or indicates an unexpected integration change

**System Assessment**
- Compare the current extract to prior cycles after transports, partner certificate changes, or batch schedule updates
- Analyze whether problematic queue states cluster on specific destinations, users, or function modules
- Validate that count thresholds still reflect operational capacity and partner agreements
- Check whether recent data volume shifts explain growth without implying a defect
- Review whether cloud or remote execution paths should be evaluated with the same parameter bundle

**Corrective Actions**
- Tune processing, throttling, or partner-side behavior according to SAP and integration standards, then re-run monitoring to confirm counts normalized
- Refine monitoring parameters after root cause so benign patterns are excluded without hiding genuine backlog risk
- Document remediation and escalation outcomes when queue delays affected materially sensitive processes
- Schedule recurring monitoring during critical business windows and retain exports when audit evidence is required
- Route repeat systemic findings into defect or change management when configuration or custom logic must be updated centrally


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | CNT | Items Count | INT4 | 10 | 0 | INT4 | INT4 |
| 2 | MANDT | Client | CLNT | 3 | 0 | SYMANDT | MANDT |
| 3 | QNAME | Queue | CHAR | 24 | 0 | TRFCQNAM | TRFCQNAM |
| 4 | QRFCFNAM | RFC FM |  | 0 | 0 |  |  |
| 5 | QRFCUSER | RFC User |  | 0 | 0 |  |  |
| 6 | QSTATE | Transactional tRFC queue statu |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 6 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**CNT** (Items Count)

Supports operational control by evaluating items count through CNT for each candidate record.

**MANDT** (Client)

Captures edge cases where client (MANDT) must be non-default to reproduce a customer-specific monitoring scenario.

**QNAME** (Queue)

When left open per framework rules, QNAME does not restrict queue; when set, only matching rows remain.

**QRFCFNAM** (RFC FM)

Mirrors how administrators slice operational lists: rfc fm (QRFCFNAM) is one lever that shapes which rows are comparable run over run.

**QRFCUSER** (RFC User)

When populated, keeps the extract focused so rfc user (QRFCUSER) aligns with the intended triage slice.

**QSTATE** (Transactional tRFC queue statu)

Limits rows to the queue or processing state values you declare, so monitoring can target only selected outcome bands.

**QSTATE Options:**
- Use standard SAP status values configured for the monitored object type.
- Code in this EI applies QSTATE as a selector but does not enumerate fixed literals inline.


### Parameter Relationships

How parameter combinations work together

**MANDT**, **QNAME**, and the user and function-module selectors define which inbound queue population enters the aggregation step before counts are computed.

**QSTATE** narrows the dataset to the processing states you consider material for the review cycle, so volume signals are not diluted by states you intentionally ignore.

**CNT** is applied after grouping as a threshold or interval filter on the computed queue item counts, so only queue groups that exceed or sit inside your configured volume band remain in the result set.

Combining **QSTATE** with **CNT** helps prioritize states that show both elevated volume and operational concern; combining **QRFCUSER** or **QRFCFNAM** with **CNT** helps isolate integration paths that generate unusually large backlog for the same queue family.


### Default Values

No default values are defined for this EI.

### Practical Example of Parameter Configuration

**Use Case 1: High-count inbound queues on one client**

**Purpose:** Surface inbound queue groups whose item count exceeds a floor while limiting to one productive client.

```
MANDT = 100
CNT = 500 - 999999
QNAME = INBOUND*
QSTATE = RUNNING
```

**Use Case 2: User and function module slice**

**Purpose:** Check whether a specific technical caller drives unusual queue volume.

```
QRFCUSER = BATCH_IFUSER
QRFCFNAM = Z_RFC_INBOUND_RECV
CNT = 50 - 999999
```

**Use Case 3: Broad queue scan with state focus**

**Purpose:** Highlight queues in a failure-style state above a modest count threshold.

```
QSTATE = SYSFAIL
CNT = 10 - 999999
MANDT = 200
```

**Use Case 4: Full parameter bundle for a narrow integration review**

**Purpose:** Reproduce a monitoring pass that ties client, queue pattern, state, caller, function module, and count band into one extract.

```
MANDT = 100
QNAME = ERP_IN*
QSTATE = RUNNING
QRFCUSER = PI_USER
QRFCFNAM = Z_IDOC_IN_PROCESS
CNT = 100 - 999999
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_02_QRFC_IN_CNT | CNT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_01_02_QRFC_IN_CNT | MANDT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_02_QRFC_IN_CNT | QNAME | Name of tRFC Queue | CHAR(24) | TRFCQNAM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_02_QRFC_IN_CNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_QRFC_IN_CNT OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_MANDT FOR TRFCQIN-MANDT,
         R_QNAME FOR TRFCQIN-QNAME,
         R_CNT FOR /SKN/S_SW_SYS_QRFC_IN_CNT-CNT,
         R_QSTATE FOR TRFCQIN-QSTATE,
         R_QRFCUSER FOR TRFCQIN-QRFCUSER,
         R_QRFCFNAM FOR TRFCQIN-QRFCFNAM,
         R_QRFCDATUM FOR TRFCQIN-QRFCDATUM.
DATA : WA TYPE /SKN/S_SW_SYS_QRFC_IN_CNT.
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
    CALL FUNCTION '/SKN/FC_SW_01_02_QRFC_IN_CNT'
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
  SELECT MANDT QNAME COUNT( * )
     FROM TRFCQIN
     INTO (WA-MANDT, WA-QNAME, WA-CNT)
     WHERE QNAME IN R_QNAME
       "and mandt in R_MANDT
       AND QSTATE IN R_QSTATE
       AND QRFCUSER IN R_QRFCUSER
       AND QRFCFNAM IN R_QRFCFNAM
       AND QRFCDATUM IN R_QRFCDATUM
     GROUP BY MANDT QNAME.
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
