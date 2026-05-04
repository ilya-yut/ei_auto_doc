# Exception Indicator: Application Log Analysis - SW_01_10_APPL_LOG

## General Overview

This Exception Indicator (EI) monitors SAP Application Log records to detect recurring technical and business-process issues across configured objects, subobjects, and message signatures. It consolidates header-level context (program, user, transaction, severity) and message-level details to provide a focused view of operational anomalies.

This EI serves as an essential control for application reliability and operational risk management by:

- Enabling early detection of recurring application-log errors and warnings before they impact core business processing
- Supporting root-cause analysis through correlated program, transaction, user, and message signature context
- Providing visibility into severity concentration across objects and subobjects for targeted remediation
- Helping teams distinguish isolated events from systemic issues using message pattern and duration-based filtering
- Giving control owners a repeatable, monitor-aligned extract for reliability governance and incident trend reviews

Typical uses include daily operations monitoring, post-incident diagnostics, release validation checks, and recurring quality-control reviews for business-critical processes. Results support prioritized fix execution and evidence-backed escalation.

The function retrieves application-log headers and messages, resolves multilingual message text details, derives severity state indicators, computes elapsed log age, and applies post-processing filters for analysis-ready output.


## Problem Description

Failure to monitor application logs in a structured and repeatable way creates multiple risks across service quality, compliance, and operational decision-making:

**Service Quality and Stability Risks**

- Recurring application errors may remain unresolved until they trigger broader process disruptions
- Warning patterns can be overlooked, reducing opportunity for early preventive action
- Program or transaction hotspots with repeated failures may persist without clear ownership
- Delayed detection of high-severity log clusters increases incident response effort and downtime risk
- Inconsistent log review practices can allow latent defects to accumulate across release cycles

**Compliance and Control Risks**

- Operational control frameworks often require evidence of systematic log monitoring and remediation
- Missing traceability from log signatures to corrective action weakens audit defensibility
- Manual ad hoc log checks are difficult to reproduce and validate consistently
- Repeated unresolved log anomalies can generate recurring control findings
- Lack of severity-focused review reduces confidence in issue prioritization decisions

**Management Visibility and Decision-Making Risks**

- Leadership lacks a clear trend view of error concentration by object, subobject, and message signature
- Technical teams cannot prioritize remediation effectively when root-cause dimensions are fragmented
- Release-governance decisions are weakened without comparable before/after log behavior evidence
- Cross-team escalation slows when context (program, user, transaction, message) is not unified
- Capacity planning for support and maintenance is less accurate when log trends are not measured consistently

## Suggested Resolution

**Immediate Response**

- Review flagged high-severity log clusters and validate business impact with process owners
- Prioritize recurring error signatures for rapid triage and containment
- Confirm whether anomalies are transient operational noise or persistent defects requiring escalation
- Open remediation tasks with explicit ownership for program, process, and support teams
- Preserve extracted evidence for incident records and governance follow-up

**System Assessment**

- Analyze log distribution by object, subobject, and message signature to isolate systemic themes
- Compare current and prior monitoring cycles to detect regressions and recurring patterns
- Correlate findings with releases, transports, and configuration changes
- Validate that review thresholds and cadence align with operational risk appetite
- Document recurring root-cause categories driving repeated log anomalies

**Corrective Actions**

- Fix recurring application defects identified through message and source-context analysis
- Improve process controls and technical validations where warning/error patterns repeatedly occur
- Tune monitoring scope and schedules with operations stakeholders for sustained oversight
- Train responders to classify severity and ownership consistently for faster closure
- Integrate recurring findings into formal problem-management and release-governance workflows


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ALCHDATE | Changed on | DATS | 8 | 0 | BALCHDATE | DATUM |
| 2 | ALCHTIME | Changed at | TIMS | 6 | 0 | BALCHTIME | UZEIT |
| 3 | ALCHUSER | Changed by | CHAR | 12 | 0 | BALCHUSER | XUBNAME |
| 4 | ALDATE | DATE | DATS | 8 | 0 | BALDATE | DATUM |
| 5 | ALMODE | Operating mode | CHAR | 1 | 0 | BALMODE | BALMODE |
| 6 | ALPROG | Program | CHAR | 40 | 0 | BALPROG | PROGNAME |
| 7 | ALSTATE | Processing status | CHAR | 1 | 0 | ALSTATE | ALSTATE |
| 8 | ALTCODE | Transaction code | CHAR | 20 | 0 | BALTCODE | TCODE |
| 9 | ALTEXT | Standard text | CHAR | 28 | 0 | BALTEXT | CHAR28 |
| 10 | ALTIME | TIME | TIMS | 6 | 0 | BALTIME | UZEIT |
| 11 | ALUSER | USER | CHAR | 12 | 0 | BALUSER | XUBNAME |
| 12 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 13 | DB_VERSION | DB format | CHAR | 4 | 0 | BALDBVER | BALDBVER |
| 14 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 15 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 16 | EXTNUMBER | External ID | CHAR | 100 | 0 | BALNREXT | BALNREXT |
| 17 | LANGU | Language Key |  | 0 | 0 |  |  |
| 18 | LAST_MSGNR | Message serial no. | NUMC | 6 | 0 | BAL_LASTNR | BALMNR |
| 19 | LOGNUMBER | Log Number | CHAR | 20 | 0 | BALOGNR | BALOGNR |
| 20 | MESSAGE | MESSAGE TEXT | CHAR | 220 | 0 | BAPI_MSG | TEXT220 |
| 21 | MSGID | MESSAGE ID | CHAR | 20 | 0 | SYMSGID | ARBGB |
| 22 | MSGNO | MESSAGE NUMBER | NUMC | 3 | 0 | SYMSGNO | SYMSGNO |
| 23 | MSGNUMBER | Message serial no. | NUMC | 6 | 0 | BALMNR | BALMNR |
| 24 | MSGTY | MESSAGE TYPE (E,I,W,...) | CHAR | 1 | 0 | SYMSGTY | SYCHAR01 |
| 25 | MSGV1 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 26 | MSGV2 | MESSAGE VARIABLE | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 27 | MSGV3 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 28 | MSGV4 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 29 | MSG_CNT_A | Termination Messages | NUMC | 6 | 0 | BALDBCNTA |  |
| 30 | MSG_CNT_AL | Total Messages | NUMC | 6 | 0 | BALDBCNTAL |  |
| 31 | MSG_CNT_E | No. of Error Msgs | NUMC | 6 | 0 | BALDBCNTE |  |
| 32 | MSG_CNT_I | I Messages | NUMC | 6 | 0 | BALDBCNTI |  |
| 33 | MSG_CNT_P1 | very high | NUMC | 6 | 0 | BALDBCNTP1 |  |
| 34 | MSG_CNT_P2 | High | NUMC | 6 | 0 | BALDBCNTP2 |  |
| 35 | MSG_CNT_P3 | Medium | NUMC | 6 | 0 | BALDBCNTP3 |  |
| 36 | MSG_CNT_P4 | low | NUMC | 6 | 0 | BALDBCNTP4 |  |
| 37 | MSG_CNT_S | Success Messages | NUMC | 6 | 0 | BALDBCNTS |  |
| 38 | MSG_CNT_W | Number of Warnings | NUMC | 6 | 0 | BALDBCNTW |  |
| 39 | OBJECT | OBJECT | CHAR | 20 | 0 | BALOBJ_D | BALOBJ |
| 40 | PROBCLASS | MAX. PROBLEM CLASS | CHAR | 1 | 0 | BALPROBCLH | BALPROBCL |
| 41 | STATE_COLOR | STATE COLOR | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 42 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 43 | SUBOBJECT | SUBOBJECT | CHAR | 20 | 0 | BALSUBOBJ | BALSUBOBJ |
| 44 | TIM_STMP | Short Time Stamp | DEC | 15 | 0 | BALCONTSMP | TZNTSTMPS |
| 45 | UDATE | Date | DATS | 8 | 0 |  |  |
| 46 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 47 | UTIME | Time | TIMS | 6 | 0 |  |  |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 47 parameters listed in the Parameters Reference Table above.

**ALCHDATE** (Changed on):

Application-log monitoring attribute **ALCHDATE** used to narrow records to the context required for analysis and remediation.

**ALCHTIME** (Changed at):

Application-log monitoring attribute **ALCHTIME** used to narrow records to the context required for analysis and remediation.

**ALCHUSER** (Changed by):

Application-log monitoring attribute **ALCHUSER** used to narrow records to the context required for analysis and remediation.

**ALDATE** (DATE):

Application-log monitoring attribute **ALDATE** used to narrow records to the context required for analysis and remediation.

**ALMODE** (Operating mode):

Application-log monitoring attribute **ALMODE** used to narrow records to the context required for analysis and remediation.

**ALPROG** (Program):

Program-origin selector from application log header.

**ALSTATE** (Processing status):

Application-log monitoring attribute **ALSTATE** used to narrow records to the context required for analysis and remediation.

**ALTCODE** (Transaction code):

Transaction-code selector associated with logged events.

**ALTEXT** (Standard text):

Application-log monitoring attribute **ALTEXT** used to narrow records to the context required for analysis and remediation.

**ALTIME** (TIME):

Application-log monitoring attribute **ALTIME** used to narrow records to the context required for analysis and remediation.

**ALUSER** (USER):

Application log user selector from header context.

**BACKDAYS** (Back Days):

Sets default lookback days when explicit log date range is not supplied.

**DB_VERSION** (DB format):

Application-log monitoring attribute **DB_VERSION** used to narrow records to the context required for analysis and remediation.

**DURATION** (Duration In Time Units):

Elapsed-time threshold based on log timestamp versus current system time.

**DURATION and DURATION_UNIT Connection:**

**DURATION** filters by computed elapsed span from log date/time; **DURATION_UNIT** defines the measurement semantics.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit for elapsed-time computation (hours, minutes, days, full-day slices).

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**EXTNUMBER** (External ID):

Application-log monitoring attribute **EXTNUMBER** used to narrow records to the context required for analysis and remediation.

**LANGU** (Language Key):

Language used for resolving message text details.

**LAST_MSGNR** (Message serial no.):

Application-log monitoring attribute **LAST_MSGNR** used to narrow records to the context required for analysis and remediation.

**LOGNUMBER** (Log Number):

Application-log monitoring attribute **LOGNUMBER** used to narrow records to the context required for analysis and remediation.

**MESSAGE** (MESSAGE TEXT):

Resolved textual message filter after message-detail enrichment.

**MSGID** (MESSAGE ID):

Message class selector for signature-based filtering.

**MSGNO** (MESSAGE NUMBER):

Message number selector, usually paired with MSGID.

**MSGNUMBER** (Message serial no.):

Application-log monitoring attribute **MSGNUMBER** used to narrow records to the context required for analysis and remediation.

**MSGTY** (MESSAGE TYPE (E,I,W,...)):

Message type selector (error/warning/info/success).

**MSGV1 - MSGV4** (Message Variables):

Message-variable selectors used with **MSGID** and **MSGNO** to isolate precise message-signature variants in application logs.

**MSG_CNT_A** (Termination Messages):

Application-log monitoring attribute **MSG_CNT_A** used to narrow records to the context required for analysis and remediation.

**MSG_CNT_AL** (Total Messages):

Application-log monitoring attribute **MSG_CNT_AL** used to narrow records to the context required for analysis and remediation.

**MSG_CNT_E** (No. of Error Msgs):

Application-log monitoring attribute **MSG_CNT_E** used to narrow records to the context required for analysis and remediation.

**MSG_CNT_I** (I Messages):

Application-log monitoring attribute **MSG_CNT_I** used to narrow records to the context required for analysis and remediation.

**MSG_CNT_P1 - MSG_CNT_P4** (Message Count Buckets):

Grouped message-count bucket selectors used to analyze recurring application-log concentration levels across P-series counters.

**MSG_CNT_S** (Success Messages):

Application-log monitoring attribute **MSG_CNT_S** used to narrow records to the context required for analysis and remediation.

**MSG_CNT_W** (Number of Warnings):

Application-log monitoring attribute **MSG_CNT_W** used to narrow records to the context required for analysis and remediation.

**OBJECT** (OBJECT):

Application log object selector (high-level business/technical domain).

**PROBCLASS** (MAX. PROBLEM CLASS):

Problem class severity selector from application log header.

**STATE_COLOR** (STATE COLOR):

Derived severity color selector after message-type evaluation.

**STATE_COLOR Options:**
- **R** — Red: critical/error-focused outcomes.
- **Y** — Yellow: warning-focused outcomes.
- **G** — Green: info/success-focused outcomes.

**STATE_ICON** (State Icon):

Application-log monitoring attribute **STATE_ICON** used to narrow records to the context required for analysis and remediation.

**SUBOBJECT** (SUBOBJECT):

Subobject selector for narrower domain scoping.

**TIM_STMP** (Short Time Stamp):

Application-log monitoring attribute **TIM_STMP** used to narrow records to the context required for analysis and remediation.

**UDATE** (Date):

Application-log monitoring attribute **UDATE** used to narrow records to the context required for analysis and remediation.

**USER_FLD** (Dynamic Recipient User Field):

Application-log monitoring attribute **USER_FLD** used to narrow records to the context required for analysis and remediation.

**UTIME** (Time):

Application-log monitoring attribute **UTIME** used to narrow records to the context required for analysis and remediation.


### Parameter Relationships

**Log-domain scoping**

- **OBJECT** and **SUBOBJECT** define the primary functional scope of retrieved log records.
- **PROBCLASS**, **ALPROG**, **ALTCODE**, and **ALUSER** add severity, source, transaction, and actor context from log headers.

**Message-signature filtering**

- **MSGTY**, **MSGID**, and **MSGNO** provide core message signature dimensions.
- **MSGV1 - MSGV4** refine that signature to specific variable-instantiated message variants.
- **MESSAGE** is resolved text and can be used as a final semantic filter after message enrichment.

**Time-window and recency control**

- **BACKDAYS** supplies default lookback behavior when explicit date range values are absent.
- **ALDATE** and **ALTIME** represent log event timing used in elapsed-age computation.
- **DURATION** and **DURATION_UNIT** work together to retain records matching elapsed-time criteria.

**Severity-state derivation**

- **MSGTY** drives derived **STATE_COLOR**, which in turn determines **STATE_ICON** through state-icon mapping logic.

**Aggregation and count context**

- **MSG_CNT_A**, **MSG_CNT_E**, **MSG_CNT_W**, **MSG_CNT_I**, and related counters support concentration analysis by message-type families.


### Default Values

- **DURATION_UNIT** — Default: `M` (minutes), assigned before caller selection extraction.
- **BACKDAYS** — Default: `1` day when no explicit log-date range is supplied.
- **LANGU** — Default: system logon language (`SY-LANGU`) unless overridden by selection.

### Practical Configuration Examples

**Use Case 1: Recent critical error concentration**

```
OBJECT = FI
MSGTY = E
BACKDAYS = 1
```

**Purpose:** Focuses on recent financial-domain error logs for rapid operational triage.

**Use Case 2: Signature-specific recurring defect tracking**

```
MSGID = ZF
MSGNO = 123
MSGV1 = *
```

**Purpose:** Isolates a known message signature to monitor recurrence and remediation effectiveness.

**Use Case 3: Full-day persistence of warning/error logs**

```
DURATION = 2
DURATION_UNIT = F
STATE_COLOR = Y
```

**Purpose:** Tracks warning-state logs persisting beyond two full-day elapsed units for preventive action.

**Use Case 4: Program-and-transaction ownership analysis**

```
ALPROG = Z_BATCH_LOAD
ALTCODE = ZTRN01
ALUSER = BATCHUSR
```

**Purpose:** Narrows logs to a specific execution path and user ownership for root-cause assignment.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_10_APPL_LOG | ALCHDATE | Application Log: Date of last change | DATS(8) | BALCHDATE |
| /SKN/S_SW_01_10_APPL_LOG | ALCHTIME | Application Log: Time of last change | TIMS(6) | BALCHTIME |
| /SKN/S_SW_01_10_APPL_LOG | ALCHUSER | Application Log: User that changed the log | CHAR(12) | BALCHUSER |
| /SKN/S_SW_01_10_APPL_LOG | ALDATE | Application log: date | DATS(8) | BALDATE |
| /SKN/S_SW_01_10_APPL_LOG | ALMODE | Application Log: Operating mode (Batch,Batch Input,Dialog) | CHAR(1) | BALMODE |
| /SKN/S_SW_01_10_APPL_LOG | ALPROG | Application log: Program name | CHAR(40) | BALPROG |
| /SKN/S_SW_01_10_APPL_LOG | ALSTATE | Application log: processing status | CHAR(1) | ALSTATE |
| /SKN/S_SW_01_10_APPL_LOG | ALTCODE | Application Log: Transaction code | CHAR(20) | BALTCODE |
| /SKN/S_SW_01_10_APPL_LOG | ALTEXT | Application log: Standard text | CHAR(28) | BALTEXT |
| /SKN/S_SW_01_10_APPL_LOG | ALTIME | Application log: time | TIMS(6) | BALTIME |
| /SKN/S_SW_01_10_APPL_LOG | ALUSER | Application log: user name | CHAR(12) | BALUSER |
| /SKN/S_SW_01_10_APPL_LOG | DB_VERSION | Application Log: Database repository format | CHAR(4) | BALDBVER |
| /SKN/S_SW_01_10_APPL_LOG | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_10_APPL_LOG | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_10_APPL_LOG | EXTNUMBER | Application Log: External ID | CHAR(100) | BALNREXT |
| /SKN/S_SW_01_10_APPL_LOG | LAST_MSGNR | ApplicationLog: Last message serial number assigned | NUMC(6) | BAL_LASTNR |
| /SKN/S_SW_01_10_APPL_LOG | LOGNUMBER | Application log: log number | CHAR(20) | BALOGNR |
| /SKN/S_SW_01_10_APPL_LOG | MESSAGE | Message Text | CHAR(220) | BAPI_MSG |
| /SKN/S_SW_01_10_APPL_LOG | MSGID | Message Class | CHAR(20) | SYMSGID |
| /SKN/S_SW_01_10_APPL_LOG | MSGNO | Message Number | NUMC(3) | SYMSGNO |
| /SKN/S_SW_01_10_APPL_LOG | MSGNUMBER | Application log: Internal message serial number | NUMC(6) | BALMNR |
| /SKN/S_SW_01_10_APPL_LOG | MSGTY | Message Type | CHAR(1) | SYMSGTY |
| /SKN/S_SW_01_10_APPL_LOG | MSGV1 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_10_APPL_LOG | MSGV2 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_10_APPL_LOG | MSGV3 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_10_APPL_LOG | MSGV4 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_A | Application Log: Number of A Messages | NUMC(6) | BALDBCNTA |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_AL | Application Log: Total Number of Messages | NUMC(6) | BALDBCNTAL |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_E | Application Log: Number of E Messages | NUMC(6) | BALDBCNTE |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_I | Application Log: Number of I Messages | NUMC(6) | BALDBCNTI |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_P1 | Application Log: Number of messages with problem class 1 | NUMC(6) | BALDBCNTP1 |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_P2 | Application Log: Number of messages with problem class 2 | NUMC(6) | BALDBCNTP2 |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_P3 | Application Log: Number of messages with problem class 3 | NUMC(6) | BALDBCNTP3 |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_P4 | Application Log: Number of messages with problem class 4 | NUMC(6) | BALDBCNTP4 |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_S | Application Log: Number of S Messages | NUMC(6) | BALDBCNTS |
| /SKN/S_SW_01_10_APPL_LOG | MSG_CNT_W | Application Log: Number of W Messages | NUMC(6) | BALDBCNTW |
| /SKN/S_SW_01_10_APPL_LOG | OBJECT | Application Log: Object Name (Application Code) | CHAR(20) | BALOBJ_D |
| /SKN/S_SW_01_10_APPL_LOG | PROBCLASS | Application Log: Log problem class | CHAR(1) | BALPROBCLH |
| /SKN/S_SW_01_10_APPL_LOG | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_10_APPL_LOG | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_10_APPL_LOG | SUBOBJECT | Application Log: Subobject | CHAR(20) | BALSUBOBJ |
| /SKN/S_SW_01_10_APPL_LOG | TIM_STMP | Application log: Time basis for context nametab | DEC(15) | BALCONTSMP |
| /SKN/S_SW_01_10_APPL_LOG | UDATE |  | DATS(8) |  |
| /SKN/S_SW_01_10_APPL_LOG | UTIME |  | TIMS(6) |  |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_10_APPL_LOG.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_10_APPL_LOG OPTIONAL
*"----------------------------------------------------------------------
* Changed by Zoe on 29.12.10 to allow selection by msg parameters (msgv1 & msgv2)
* Zoe on 3.1.10 for Ortal - complete matnr & plant numbers for CIF messages
*"----------------------------------------------------------------------
  RANGES : R_OBJECT FOR BALHDR-OBJECT ,
           R_SUBOBJECT FOR BALHDR-SUBOBJECT,
           R_STATE_COLOR FOR /SKN/S_SW_SYS_JOB-STATE_COLOR,
           R_PROBCLASS FOR BALHDR-PROBCLASS,
           R_ALPROG FOR BALHDR-ALPROG,
           R_ALTCODE FOR BALHDR-ALTCODE,
           R_ALUSER FOR BALHDR-ALUSER,
           R_DATUM   FOR SY-DATUM ,
           R_UZEIT   FOR SY-UZEIT .
  RANGES : R_MSGTY   FOR BALM-MSGTY,
           R_MSGID   FOR BALM-MSGID,
           R_MSGNO   FOR BALM-MSGNO.
* Zoe on 29.12.10
  RANGES : R_MSGV1   FOR BALM-MSGV1,
           R_MSGV2   FOR BALM-MSGV2.
* end 29.12.10
  RANGES : R_MESSAGE FOR 	/SKN/S_SW_SYS_APPL_LOG-MESSAGE.
  DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
  DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  DATA : SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
  DATA : TIME_DIFF TYPE  INT4 .
  DATA :   IS_GENERAL(1) TYPE C.
  DATA : DATE_FROM LIKE SY-DATUM ,
         DATE_TO LIKE SY-DATUM ,
         BACKDAYS  TYPE I .
  DATA : LANGU LIKE SY-LANGU .
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : BEGIN OF T_HDR OCCURS 0,
          OBJECT TYPE BALOBJ_D,
          SUBOBJECT TYPE BALSUBOBJ,
          EXTNUMBER TYPE BALNREXT,
          ALDATE_F TYPE BALDATE ,
          ALDATE_T TYPE BALDATE ,
          ALTIME_F TYPE ALTIME ,
          ALTIME_T TYPE ALTIME ,
          ALUSER TYPE BALUSER,
          PROBCLASS TYPE BALPROBCLH,
         END OF T_HDR.
  DATA : NUMBER_OF_LOGS TYPE I.
  DATA : BEGIN OF HEADER_DATA OCCURS 0.
          INCLUDE STRUCTURE BALHDR .
  DATA : END OF HEADER_DATA.
  DATA : BEGIN OF MESSAGES OCCURS 0.
          INCLUDE STRUCTURE BALM .
  DATA : END OF MESSAGES.
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_10_APPL_LOG'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*-- Fill Selection Option Tables
  SELECT_MULTY: DURATION.
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
  LOOP AT T_SELECT WHERE FIELDNM = 'OBJECT'.
    MOVE-CORRESPONDING T_SELECT TO R_OBJECT.
    APPEND R_OBJECT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
    MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
    APPEND R_STATE_COLOR.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'SUBOBJECT'.
    MOVE-CORRESPONDING T_SELECT TO R_SUBOBJECT.
    APPEND R_SUBOBJECT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'PROBCLASS'.
    MOVE-CORRESPONDING T_SELECT TO R_PROBCLASS.
    APPEND R_PROBCLASS.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'ALPROG'.
    MOVE-CORRESPONDING T_SELECT TO R_ALPROG.
    APPEND R_ALPROG.
  ENDLOOP.
*
  LOOP AT T_SELECT WHERE FIELDNM = 'ALTCODE'.
    MOVE-CORRESPONDING T_SELECT TO R_ALTCODE.
    APPEND R_ALTCODE.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'ALUSER'.
    MOVE-CORRESPONDING T_SELECT TO R_ALUSER.
    APPEND R_ALUSER.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'MSGTY'.
    MOVE-CORRESPONDING T_SELECT TO R_MSGTY.
    APPEND R_MSGTY.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'MSGID'.
    MOVE-CORRESPONDING T_SELECT TO R_MSGID.
    APPEND R_MSGID.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'MSGNO'.
    MOVE-CORRESPONDING T_SELECT TO R_MSGNO.
    APPEND R_MSGNO.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'MESSAGE'.
    MOVE-CORRESPONDING T_SELECT TO R_MESSAGE.
    APPEND R_MESSAGE.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
    MOVE-CORRESPONDING T_SELECT TO R_DATUM.
    APPEND R_DATUM.
  ENDLOOP.
* Zoe on 29.12.10
  LOOP AT T_SELECT WHERE FIELDNM = 'MSGV1'.
    MOVE-CORRESPONDING T_SELECT TO R_MSGV1.
    APPEND R_MSGV1.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'MSGV2'.
    MOVE-CORRESPONDING T_SELECT TO R_MSGV2.
    APPEND R_MSGV2.
  ENDLOOP.
* end 29.12.10
  IF R_DATUM[] IS INITIAL .
    LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
      R_DATUM-SIGN = 'I' .
      R_DATUM-OPTION = 'GE' .
      BACKDAYS = T_SELECT-LOW .
      DATE_FROM = SY-DATUM - BACKDAYS .
      R_DATUM-LOW = DATE_FROM .
      APPEND R_DATUM.
      EXIT.
    ENDLOOP.
    IF R_DATUM[] IS INITIAL .
      R_DATUM-SIGN = 'I' .
      R_DATUM-OPTION = 'GE' .
      BACKDAYS = 1 .
      DATE_FROM = SY-DATUM - BACKDAYS .
      R_DATUM-LOW = DATE_FROM .
      APPEND R_DATUM.
    ENDIF .
  ENDIF.
  "--------------
  READ TABLE R_DATUM INDEX 1.
  DATE_FROM = R_DATUM-LOW.
  DATE_TO = R_DATUM-HIGH.
  LOOP AT R_DATUM .
    IF DATE_FROM > R_DATUM-LOW.
      DATE_FROM = R_DATUM-LOW.
    ENDIF.
    IF DATE_TO < R_DATUM-HIGH.
      DATE_TO = R_DATUM-HIGH.
    ENDIF.
  ENDLOOP.
  IF DATE_TO IS INITIAL.
    DATE_TO = DATE_FROM.
  ENDIF.
  "--------------
  LANGU = SY-LANGU.
  LOOP AT T_SELECT WHERE FIELDNM = 'LANGU'.
    LANGU = T_SELECT-LOW.
    EXIT.
  ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  REFRESH T_HDR .
  SELECT OBJECT SUBOBJECT EXTNUMBER
         PROBCLASS ALUSER
         MIN( ALDATE ) MAX( ALDATE ) MIN( ALTIME ) MAX( ALTIME )
    FROM BALHDR
    INTO (T_HDR-OBJECT, T_HDR-SUBOBJECT, T_HDR-EXTNUMBER,
          T_HDR-PROBCLASS, T_HDR-ALUSER,
          T_HDR-ALDATE_F, T_HDR-ALDATE_T, T_HDR-ALTIME_F, T_HDR-ALTIME_T)
     WHERE OBJECT IN R_OBJECT
       AND SUBOBJECT IN R_SUBOBJECT
       AND PROBCLASS IN R_PROBCLASS
       AND ALPROG IN R_ALPROG
       AND ALTCODE IN R_ALTCODE
       AND ALUSER IN R_ALUSER
       AND ALDATE IN R_DATUM
       AND ALTIME IN R_UZEIT
     GROUP BY OBJECT SUBOBJECT EXTNUMBER PROBCLASS ALUSER.
    APPEND T_HDR.
  ENDSELECT.
  LOOP AT T_HDR.
    CLEAR HEADER_DATA[].
    CLEAR MESSAGES[].
    CALL FUNCTION 'APPL_LOG_READ_DB'
      EXPORTING
        OBJECT                   = T_HDR-OBJECT
        SUBOBJECT                = T_HDR-SUBOBJECT
        EXTERNAL_NUMBER          = T_HDR-EXTNUMBER
        DATE_FROM                = T_HDR-ALDATE_F
        DATE_TO                  = T_HDR-ALDATE_T
        TIME_FROM                = T_HDR-ALTIME_F
        TIME_TO                  = T_HDR-ALTIME_T
        LOG_CLASS                = T_HDR-PROBCLASS
*        PROGRAM_NAME             = '*'
*        TRANSACTION_CODE         = '*'
        USER_ID                  = T_HDR-ALUSER
*        MODE                     = '+'
*        PUT_INTO_MEMORY          = ' '
      IMPORTING
        NUMBER_OF_LOGS           = NUMBER_OF_LOGS
      TABLES
        HEADER_DATA              = HEADER_DATA
*        HEADER_PARAMETERS        =
        MESSAGES                 = MESSAGES
*        MESSAGE_PARAMETERS       =
*        CONTEXTS                 =
*        T_EXCEPTIONS             =
              .
    DATA LWA_MSG LIKE LINE OF MESSAGES.
    LOOP AT MESSAGES .
      READ TABLE HEADER_DATA WITH KEY LOGNUMBER = MESSAGES-LOGNUMBER.
      IF SY-SUBRC IS INITIAL.
        MOVE-CORRESPONDING MESSAGES TO T_DATA.
        MOVE-CORRESPONDING HEADER_DATA TO T_DATA.
        APPEND T_DATA.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
*   select *
*      from BALHDR
*      into CORRESPONDING FIELDS OF TABLE t_data
*      where OBJECT in R_OBJECT
*        and SUBOBJECT in R_SUBOBJECT
*        and PROBCLASS in R_PROBCLASS
*        and ALPROG in R_ALPROG
*        and ALTCODE in R_ALTCODE
*        and ALUSER in R_ALUSER
*        and ALDATE in R_DATUM
*        and ALTIME in R_UZEIT.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    IF T_DATA-MSGTY IN R_MSGTY AND
       T_DATA-MSGID IN R_MSGID AND
       T_DATA-MSGNO IN R_MSGNO AND
       T_DATA-MSGV1 IN R_MSGV1 AND
       T_DATA-MSGV2 IN R_MSGV2.
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          ID                = T_DATA-MSGID
          NUMBER            = T_DATA-MSGNO
          LANGUAGE          = LANGU
          TEXTFORMAT        = 'ASC'
*         LINKPATTERN       =
          MESSAGE_V1        = T_DATA-MSGV1
          MESSAGE_V2        = T_DATA-MSGV2
          MESSAGE_V3        = T_DATA-MSGV3
          MESSAGE_V4        = T_DATA-MSGV4
        IMPORTING
          MESSAGE           = T_DATA-MESSAGE
*         RETURN            =
*       TABLES
*         TEXT              =
                .
      MODIFY T_DATA INDEX SY_TABIX.
    ELSE.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  IF NOT R_MESSAGE[] IS INITIAL.
    LOOP AT T_DATA .
      SY_TABIX = SY-TABIX .
      IF NOT T_DATA-MESSAGE IN R_MESSAGE.
        DELETE T_DATA INDEX SY_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    " State ICON
    CASE T_DATA-MSGTY.
      WHEN 'E' .
        T_DATA-STATE_COLOR = 'R'.
      WHEN 'W' .
        T_DATA-STATE_COLOR = 'Y'.
      WHEN 'S' .
        T_DATA-STATE_COLOR = 'G'.
      WHEN 'I' .
        T_DATA-STATE_COLOR = 'G'.
      WHEN OTHERS.
    ENDCASE.
    CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
      EXPORTING
        STATE_COLOR = T_DATA-STATE_COLOR
      IMPORTING
        STATE_ICON  = T_DATA-STATE_ICON.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-ALDATE
          T_FROM            = T_DATA-ALTIME
          D_TO              = SY_DATLO
          T_TO              = SY_TIMLO
          TIME_UNIT         = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
      IF SY-SUBRC = 0.
        T_DATA-DURATION = TIME_DIFF .
      ELSE.
        T_DATA-DURATION = '999999'.
      ENDIF.
   MODIFY T_DATA INDEX SY_TABIX .
 ENDLOOP .
 DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
