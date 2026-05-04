# Exception Indicator: ODATA Gateway Error Monitoring - SW_ODATA_GW_ERR_M1

## General Overview

This Exception Indicator (EI) monitors SAP OData Gateway runtime errors by collecting structured error-log entries and exposing technical context such as service, request, message, component, and source-code location. It gives operations and development teams a focused view of recurring gateway failures so they can detect unstable integrations and service defects early.

This EI serves as an essential control for API reliability and operational stability by:

- Enabling early detection of recurring Gateway errors before they propagate into user-facing disruptions
- Supporting targeted troubleshooting by correlating error text, message identifiers, request metadata, and source-code references
- Providing visibility into service, endpoint, and client-origin hotspots that drive repeated failures
- Helping teams separate one-off technical noise from systematic service or package-level defects
- Giving control owners a repeatable extract for ongoing monitoring of integration quality and incident trends

Typical uses include daily interface-health monitoring, post-incident root-cause analysis, release validation for OData services, and recurring reliability reviews across API landscapes. Results support prioritization of fixes and evidence-based escalation.

The function reads Gateway error-log records, applies configurable technical filters, enriches timestamp-derived date/time fields, and returns a structured error dataset for analysis.


## Problem Description

Failure to monitor OData Gateway errors in a structured and repeatable way creates multiple risks across service reliability, security, and operational governance:

**Service Reliability and Performance Risks**

- Recurrent Gateway failures may remain unresolved until they cause broad process interruptions
- Unidentified endpoint hotspots can degrade API response quality and user experience over time
- Error bursts tied to specific services or versions may be missed without consolidated technical filtering
- Untracked payload/context growth can hide memory or callstack pressure that worsens stability
- Delayed detection of persistent failures increases backlog and incident recovery time

**Security and Compliance Risks**

- Inadequate visibility into remote-address and request-context patterns can obscure malicious or abusive traffic behavior
- Missing correlation of technical IDs and message signatures weakens forensic traceability during incidents
- Auditability of API error handling declines when evidence is fragmented across ad hoc checks
- Repeated unresolved error conditions can violate internal control expectations for interface operations
- Lack of severity-focused monitoring reduces confidence in incident classification and escalation decisions

**Management Visibility and Decision-Making Risks**

- Leadership lacks a clear trend view of Gateway error concentration by service, component, and origin
- Technical teams cannot prioritize remediation effectively when critical dimensions are not unified in one report
- Root-cause ownership becomes unclear when source and package context are not consistently monitored
- Release-risk decisions are weakened without comparative error behavior across service versions
- Cross-team coordination between Basis, integration, and development slows when evidence is inconsistent

## Suggested Resolution

**Immediate Response**

- Review flagged error clusters and confirm affected services, endpoints, and business impact
- Prioritize high-frequency or high-severity error patterns for rapid containment and triage
- Validate whether failures are transient operational anomalies or code/service defects requiring escalation
- Open remediation tasks with explicit ownership for service, package, and component stakeholders
- Preserve monitoring output for incident timelines and governance follow-up

**System Assessment**

- Analyze error distribution by service, message signature, and source location to identify recurring defect themes
- Compare current behavior with previous monitoring cycles to detect regression trends
- Correlate request and remote-origin patterns to isolate client-side or integration-channel contributors
- Validate filter thresholds and review cadence against operational risk appetite
- Document systemic reliability gaps that cause repeat Gateway failures

**Corrective Actions**

- Fix recurring service and code defects identified through message/signature and source-context analysis
- Improve endpoint and payload handling where request-size and context indicators point to instability
- Tune monitoring scope and schedule with operations stakeholders for sustained API reliability oversight
- Train responders to interpret error dimensions consistently for faster root-cause ownership assignment
- Integrate recurring findings into formal problem-management and release-governance workflows


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BALOGNO | Log number | CHAR | 20 | 0 | BALOGNR | BALOGNR |
| 3 | CALLSTACK_SIZE | XML Size of Call Stack | INT4 | 10 | 0 |  |  |
| 4 | CHANNEL | Channel | CHAR | 1 | 0 |  |  |
| 5 | CONNECTION_CNT | EPP Connection Count | INT4 | 10 | 0 |  |  |
| 6 | CONNECTION_ID | EPP Connection ID | CHAR | 32 | 0 |  |  |
| 7 | CONTEXT_SIZE | XML Size of Error Context | INT4 | 10 | 0 |  |  |
| 8 | DATE | Date in Format YYYYMMSS in 8 Characters | DATS | 8 | 0 | DATS | DATS |
| 9 | DESTINATION | Destination | CHAR | 32 | 0 |  |  |
| 10 | ERROR_COMPONENT | ABAP Component | CHAR | 24 | 0 |  |  |
| 11 | ERROR_COUNT | Error Count | INT4 | 10 | 0 |  |  |
| 12 | ERROR_PACKAGE | ABAP Package | CHAR | 30 | 0 |  |  |
| 13 | ERROR_TEXT | Error Text | CHAR | 128 | 0 |  |  |
| 14 | EXPIRY_DATE | Expiry Date | DATS | 8 | 0 |  |  |
| 15 | FIRST_TSTMP | Time Stamp | DEC | 21 | 7 | TIMESTAMPL | TZNTSTMPL |
| 16 | HTTP_STATUS | HTTP Status Code | NUMC | 3 | 0 |  |  |
| 17 | ICF_NODE | ICF Node | CHAR | 8 | 0 |  |  |
| 18 | LOCATION | Location | CHAR | 1 | 0 | /IWFND/SUTIL_LOCATION | /IWFND/SUTIL_LOCATION |
| 19 | LOG_LEVEL | SUTIL Error Log Level | CHAR | 1 | 0 | /IWFND/SUTIL_LOG_LEVEL | /IWFND/SUTIL_LOG_LEVEL |
| 20 | NAMESPACE | Service Namespace | CHAR | 10 | 0 |  |  |
| 21 | OPID | Operation ID | CHAR | 32 | 0 |  |  |
| 22 | REMOTE_ADDRESS | Remote Address | CHAR | 45 | 0 |  |  |
| 23 | REQUEST_ID | Request ID | CHAR | 100 | 0 |  |  |
| 24 | REQUEST_SIZE | XML Size of Payload Request | INT4 | 10 | 0 |  |  |
| 25 | REQUEST_URI | Request URI | CHAR | 255 | 0 |  |  |
| 26 | RESPONSE_SIZE | XML Size of Payload Response | INT4 | 10 | 0 |  |  |
| 27 | ROOT_CONTEXT_ID | EPP Root Context ID | CHAR | 32 | 0 |  |  |
| 28 | SAPCLIENT | Client ID | CLNT | 3 | 0 | SYMANDT | MANDT |
| 29 | SERVICE_NAME | Service Name | CHAR | 40 | 0 |  |  |
| 30 | SERVICE_REPO | OData V4 Service Repository | CHAR | 10 | 0 |  |  |
| 31 | SERVICE_VERSION | Service Version | NUMC | 4 | 0 |  |  |
| 32 | SOURCE_INCLUDE | Source Include | CHAR | 40 | 0 |  |  |
| 33 | SOURCE_LINE | Source Line | INT4 | 10 | 0 |  |  |
| 34 | SOURCE_PROGRAM | Source Program | CHAR | 40 | 0 |  |  |
| 35 | SUBNO | Entry number within an operation | INT4 | 10 | 0 |  |  |
| 36 | T100_MSGID | Message Class | CHAR | 20 | 0 | SYMSGID | ARBGB |
| 37 | T100_MSGNO | Message number | NUMC | 3 | 0 | SYMSGNO | SYMSGNO |
| 38 | T100_MSGV1 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 39 | T100_MSGV2 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 40 | T100_MSGV3 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 41 | T100_MSGV4 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 42 | TERMINAL_ID | Terminal ID | CHAR | 32 | 0 |  |  |
| 43 | TIME | Field of type TIMS | TIMS | 6 | 0 | TIMS | TIME |
| 44 | TIMESTAMP | Time Stamp | DEC | 21 | 7 | TIMESTAMPL | TZNTSTMPL |
| 45 | TRANSACTION_ID | EPP Transaction ID | CHAR | 32 | 0 |  |  |
| 46 | USERNAME | User Name | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 46 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Backdays):

Controls the default lookback window when timestamp range is not supplied explicitly; the function builds a lower timestamp bound from current date minus this value.

**BALOGNO** (Log number):

Gateway error-monitoring attribute **BALOGNO** used to narrow technical error records to the context relevant for investigation and remediation.

**CALLSTACK_SIZE** (XML Size of Call Stack):

Gateway error-monitoring attribute **CALLSTACK_SIZE** used to narrow technical error records to the context relevant for investigation and remediation.

**CHANNEL** (Channel):

Gateway error-monitoring attribute **CHANNEL** used to narrow technical error records to the context relevant for investigation and remediation.

**CONNECTION_CNT** (EPP Connection Count):

Connection-count thresholding dimension for concurrency-related errors.

**CONNECTION_ID** (EPP Connection ID):

Connection identifier for channel/session scoping in gateway runtime analysis.

**CONTEXT_SIZE** (XML Size of Error Context):

Gateway error-monitoring attribute **CONTEXT_SIZE** used to narrow technical error records to the context relevant for investigation and remediation.

**DATE** (Date in Format YYYYMMSS in 8 Characters):

Gateway error-monitoring attribute **DATE** used to narrow technical error records to the context relevant for investigation and remediation.

**DESTINATION** (Destination):

Gateway error-monitoring attribute **DESTINATION** used to narrow technical error records to the context relevant for investigation and remediation.

**ERROR_COMPONENT** (ABAP Component):

Gateway error-monitoring attribute **ERROR_COMPONENT** used to narrow technical error records to the context relevant for investigation and remediation.

**ERROR_COUNT** (Error Count):

Counts repeated occurrences of gateway errors and supports concentration analysis by severity and source context.

**ERROR_PACKAGE** (ABAP Package):

Gateway error-monitoring attribute **ERROR_PACKAGE** used to narrow technical error records to the context relevant for investigation and remediation.

**ERROR_TEXT** (Error Text):

Filter on full gateway error text to isolate known failure patterns.

**EXPIRY_DATE** (Expiry Date):

Gateway error-monitoring attribute **EXPIRY_DATE** used to narrow technical error records to the context relevant for investigation and remediation.

**FIRST_TSTMP** (Time Stamp):

Gateway error-monitoring attribute **FIRST_TSTMP** used to narrow technical error records to the context relevant for investigation and remediation.

**HTTP_STATUS** (HTTP Status Code):

HTTP status selector when focusing on protocol-level failures.

**ICF_NODE** (ICF Node):

Gateway error-monitoring attribute **ICF_NODE** used to narrow technical error records to the context relevant for investigation and remediation.

**LOCATION** (Location):

Gateway error-monitoring attribute **LOCATION** used to narrow technical error records to the context relevant for investigation and remediation.

**LOG_LEVEL** (SUTIL Error Log Level):

Log-severity level selector for focusing on critical versus informational records.

**NAMESPACE** (Service Namespace):

Gateway error-monitoring attribute **NAMESPACE** used to narrow technical error records to the context relevant for investigation and remediation.

**OPID** (Operation ID):

Gateway error-monitoring attribute **OPID** used to narrow technical error records to the context relevant for investigation and remediation.

**REMOTE_ADDRESS** (Remote Address):

Client/remote IP endpoint filter for identifying origin clusters.

**REQUEST_ID** (Request ID):

Technical request identifier for one-to-one traceability with logs.

**REQUEST_SIZE** (XML Size of Payload Request):

Gateway error-monitoring attribute **REQUEST_SIZE** used to narrow technical error records to the context relevant for investigation and remediation.

**REQUEST_URI** (Request URI):

Filter by failing OData request endpoint path for service-level troubleshooting.

**RESPONSE_SIZE** (XML Size of Payload Response):

Gateway error-monitoring attribute **RESPONSE_SIZE** used to narrow technical error records to the context relevant for investigation and remediation.

**ROOT_CONTEXT_ID** (EPP Root Context ID):

Context-correlation key to connect nested calls and trace trees.

**SAPCLIENT** (Client ID):

Gateway error-monitoring attribute **SAPCLIENT** used to narrow technical error records to the context relevant for investigation and remediation.

**SERVICE_NAME** (Service Name):

Scope monitoring to a specific OData service experiencing errors.

**SERVICE_REPO** (OData V4 Service Repository):

Gateway error-monitoring attribute **SERVICE_REPO** used to narrow technical error records to the context relevant for investigation and remediation.

**SERVICE_VERSION** (Service Version):

Service-version selector to isolate regressions introduced by specific service deployments.

**SOURCE_INCLUDE** (Source Include):

Include name complementing SOURCE_PROGRAM for precise code-location tracing.

**SOURCE_LINE** (Source Line):

Source line indicator used for pinpointing recurring coding hotspots.

**SOURCE_PROGRAM** (Source Program):

Program name where the error originated, useful for technical root-cause assignment.

**SUBNO** (Entry number within an operation):

Gateway error-monitoring attribute **SUBNO** used to narrow technical error records to the context relevant for investigation and remediation.

**T100_MSGID** (Message Class):

Message class selector for ABAP message-origin analysis.

**T100_MSGNO** (Message number):

Message number selector, usually paired with T100_MSGID.

**T100_MSGV1 - T100_MSGV4** (Message Variables):

Message-variable selectors used with **T100_MSGID** and **T100_MSGNO** to isolate precise message-signature variants in Gateway runtime failures.

**TERMINAL_ID** (Terminal ID):

Gateway error-monitoring attribute **TERMINAL_ID** used to narrow technical error records to the context relevant for investigation and remediation.

**TIME** (Field of type TIMS):

Gateway error-monitoring attribute **TIME** used to narrow technical error records to the context relevant for investigation and remediation.

**TIMESTAMP** (Time Stamp):

Primary time-range selector for OData gateway errors; when omitted, the function auto-builds a default range using BACKDAYS.

**TRANSACTION_ID** (EPP Transaction ID):

Cross-system transaction identifier for end-to-end diagnostics.

**USERNAME** (User Name):

Gateway error-monitoring attribute **USERNAME** used to narrow technical error records to the context relevant for investigation and remediation.


### Parameter Relationships

**Time-window control**

- **TIMESTAMP** is the primary range selector for error-log retrieval.
- **BACKDAYS** supplies default lookback behavior when **TIMESTAMP** is not provided.
- **DATE** and **TIME** are derived from timestamp conversion in post-retrieval processing for readable analysis.

**Message and error signature correlation**

- **T100_MSGID**, **T100_MSGNO**, and **T100_MSGV1-V4** form a message-signature set used to isolate specific technical failure patterns.
- **ERROR_TEXT**, **ERROR_COMPONENT**, and **ERROR_PACKAGE** complement signature filters with runtime and ownership context.

**Service and request tracing**

- **SERVICE_NAME**, **SERVICE_VERSION**, **REQUEST_URI**, and **REQUEST_ID** jointly identify where and under which request context failures occur.
- **TRANSACTION_ID**, **ROOT_CONTEXT_ID**, **CONNECTION_ID**, and **CONNECTION_CNT** provide cross-call and session correlation dimensions.

**Source-code localization**

- **SOURCE_PROGRAM**, **SOURCE_INCLUDE**, and **SOURCE_LINE** should be used together for precise defect localization.

**Origin and transport context**

- **SAPCLIENT**, **USERNAME**, **TERMINAL_ID**, **REMOTE_ADDRESS**, **DESTINATION**, and **CHANNEL** provide actor and origin dimensions for pattern analysis.


### Default Values

- **LANGU** — Default: `E` (English), assigned before parameter extraction.
- **BACKDAYS** — Default: `1` day, assigned before parameter extraction.

**Note:** When **TIMESTAMP** is not supplied, the function constructs a default timestamp-from bound using current date minus BACKDAYS.

### Practical Configuration Examples

**Use Case 1: High-volume Gateway failures by service**

```
SERVICE_NAME = ZAPI_ORDER_SRV
ERROR_COUNT = 10 - 999999
BACKDAYS = 1
```

**Purpose:** Detects high-frequency errors for a specific OData service within the default one-day operational window.

**Use Case 2: Message-signature based defect isolation**

```
T100_MSGID = /IWBEP/CM_MGW_RT
T100_MSGNO = 020
ERROR_COMPONENT = /IWBEP/
```

**Purpose:** Isolates a known runtime message signature and component for focused defect triage.

**Use Case 3: Endpoint and origin cluster analysis**

```
REQUEST_URI = */sap/opu/odata/*
REMOTE_ADDRESS = 10.*
ERROR_COUNT = 3 - 999999
```

**Purpose:** Identifies recurring endpoint failures from a specific network origin range.

**Use Case 4: Source-code localization for recurring failures**

```
SOURCE_PROGRAM = CL_ZGW_ORDER_DPC_EXT
SOURCE_INCLUDE = LZGW_ORDERDPCU01
SOURCE_LINE = 100 - 500
```

**Purpose:** Narrows recurring error records to a specific program/include/line span for developer assignment.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_ODATA_GW_ERR_MNTR | BALOGNO | Application log: log number | CHAR(20) | BALOGNR |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | CALLSTACK_SIZE |  | INT4(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | CHANNEL |  | CHAR(1) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | CONNECTION_CNT |  | INT4(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | CONNECTION_ID |  | CHAR(32) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | CONTEXT_SIZE |  | INT4(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | DATE | Field of type DATS | DATS(8) | DATS |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | DESTINATION |  | CHAR(32) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | ERROR_COMPONENT |  | CHAR(24) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | ERROR_COUNT |  | INT4(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | ERROR_PACKAGE |  | CHAR(30) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | ERROR_TEXT |  | CHAR(128) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | EXPIRY_DATE |  | DATS(8) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | FIRST_TSTMP | UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun) | DEC(21,7) | TIMESTAMPL |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | HTTP_STATUS |  | NUMC(3) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | ICF_NODE |  | CHAR(8) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | LOCATION | Location | CHAR(1) | /IWFND/SUTIL_LOCATION |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | LOG_LEVEL | Error Log Level | CHAR(1) | /IWFND/SUTIL_LOG_LEVEL |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | NAMESPACE |  | CHAR(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | OPID |  | CHAR(32) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | REMOTE_ADDRESS |  | CHAR(45) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | REQUEST_ID |  | CHAR(100) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | REQUEST_SIZE |  | INT4(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | REQUEST_URI |  | CHAR(255) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | RESPONSE_SIZE |  | INT4(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | ROOT_CONTEXT_ID |  | CHAR(32) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | SAPCLIENT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | SERVICE_NAME |  | CHAR(40) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | SERVICE_REPO |  | CHAR(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | SERVICE_VERSION |  | NUMC(4) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | SOURCE_INCLUDE |  | CHAR(40) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | SOURCE_LINE |  | INT4(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | SOURCE_PROGRAM |  | CHAR(40) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | SUBNO |  | INT4(10) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | T100_MSGID | Message Class | CHAR(20) | SYMSGID |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | T100_MSGNO | Message Number | NUMC(3) | SYMSGNO |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | T100_MSGV1 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | T100_MSGV2 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | T100_MSGV3 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | T100_MSGV4 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | TERMINAL_ID |  | CHAR(32) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | TIME | Field of type TIMS | TIMS(6) | TIMS |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | TIMESTAMP | UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun) | DEC(21,7) | TIMESTAMPL |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | TRANSACTION_ID |  | CHAR(32) |  |
| /SKN/S_SW_ODATA_GW_ERR_MNTR | USERNAME | User Name | CHAR(12) | SYUNAME |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_ODATA_GW_ERR_MNTR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_ODATA_GW_ERR_MNTR OPTIONAL
*"----------------------------------------------------------------------
* ----------------------------------------------------------------------
* Local Type definition
* ----------------------------------------------------------------------
* ----------------------------------------------------------------------
* Local Data definition
* ----------------------------------------------------------------------
* - single DATA
  DATA_SINGLE:
    LANGU                LANGU,                    " Language (not in use)
    DATUM                SY-DATUM                  " System Date
    .
*
* - range DATA
  DATA_MULTY:
    TIMESTAMP            TIMESTAMP                 " Timestamp
    .
*
* ----------------------------------------------------------------------
* Parameters Definition
* ----------------------------------------------------------------------
* Define Special Parameters
* - single DATA
  DATA_SINGLE:
    "sw_dest              rfcdest,                  " RFC destination
    BACKDAYS             INT4                     " BACKDAYS
    .
* - range DATA
  DATA_MULTY:
      SAPCLIENT         CHAR3,
      USERNAME          CHAR12,
      ERROR_COUNT       INT4,
      T100_MSGID        SYMSGID,
      T100_MSGNO        SYMSGNO,
      T100_MSGV1        SYMSGV,
      T100_MSGV2        SYMSGV,
      T100_MSGV3        SYMSGV,
      T100_MSGV4        SYMSGV,
      LOCATION          CHAR1,
      CHANNEL           CHAR1,
      ERROR_TEXT        CHAR128,
      ERROR_COMPONENT   CHAR24,
      ERROR_PACKAGE     CHAR30,
      SOURCE_PROGRAM    CHAR40,
      SOURCE_INCLUDE    CHAR40,
      SOURCE_LINE       INT4,
      NAMESPACE         NAMESPACE,
      SERVICE_NAME      CHAR40,
      TRANSACTION_ID    CHAR32,
      ROOT_CONTEXT_ID   CHAR32,
      CONNECTION_ID     CHAR32,
      CONNECTION_CNT    INT4,
      TERMINAL_ID       CHAR32,
      REMOTE_ADDRESS    CHAR45,
      DESTINATION       CHAR32,
      REQUEST_URI       CHAR255,
      BALOGNO           CHAR20,
      CONTEXT_SIZE      INT4,
      CALLSTACK_SIZE    INT4,
      REQUEST_SIZE      INT4,
      RESPONSE_SIZE     INT4,
      HTML_PAGE         CHAR1024,
      ICF_NODE          CHAR8,
      REQUEST_ID        CHAR100,       "   request_id,
      SERVICE_VERSION   NUMC4,
      LOG_LEVEL         CHAR1
 "     service_repo      char10,
 "     http_status       numc3
    .
  DATA: DATE_FROM      LIKE SY-DATUM,
        TIMESTAMP_FROM TYPE TIMESTAMPL,
        LT_DATA2       TYPE TABLE OF /SKN/S_SW_ODATA_GW_ERR_MNTR,
        LS_DATA2       TYPE /SKN/S_SW_ODATA_GW_ERR_MNTR,
        LV_TZONE       TYPE SY-ZONLO
        .
* ----------------------------------------------------------------------
* Extracting parameters’ value and populating variables
* ----------------------------------------------------------------------
* Set initial value
  LV_LANGU               = 'E'.     " English
  LV_BACKDAYS            =  1.
*
* Extract Special Parameters
* - single value
  SELECT_SINGLE:
    "sw_dest,                        " RFC destination
    BACKDAYS                       " BACKDAYS
    .
* - range value
  SELECT_MULTY:
      SAPCLIENT,
      USERNAME,
      ERROR_COUNT,
      T100_MSGID,
      T100_MSGNO,
      T100_MSGV1,
      T100_MSGV2,
      T100_MSGV3,
      T100_MSGV4,
      LOCATION,
      CHANNEL,
      ERROR_TEXT,
      ERROR_COMPONENT,
      ERROR_PACKAGE,
      SOURCE_PROGRAM,
      SOURCE_INCLUDE,
      SOURCE_LINE,
      NAMESPACE,
      SERVICE_NAME,
      TRANSACTION_ID,
      ROOT_CONTEXT_ID,
      CONNECTION_ID,
      CONNECTION_CNT,
      TERMINAL_ID,
      REMOTE_ADDRESS,
      DESTINATION,
      REQUEST_URI,
      BALOGNO,
      CONTEXT_SIZE,
      CALLSTACK_SIZE,
      REQUEST_SIZE,
      RESPONSE_SIZE,
      HTML_PAGE,
      ICF_NODE,
      REQUEST_ID,
      SERVICE_VERSION,
      LOG_LEVEL
      "service_repo,
      "http_status
    .
*
* ----------------------------------------------------------------------
* Initiating
* ----------------------------------------------------------------------
  CLEAR:
    IS_ALERT
    .
  REFRESH
    T_DATA
    .
*
* ----------------------------------------------------------------------
* Retrieving alert data
* ----------------------------------------------------------------------
  IF R_TIMESTAMP[] IS INITIAL .
   RS_TIMESTAMP-SIGN   = 'I' .
   RS_TIMESTAMP-OPTION = 'GE' .
   DATE_FROM           = SY-DATUM - LV_BACKDAYS .
   CONVERT DATE DATE_FROM INTO TIME STAMP TIMESTAMP_FROM TIME ZONE SY-ZONLO.
   RS_TIMESTAMP-LOW    = TIMESTAMP_FROM .
   APPEND RS_TIMESTAMP TO R_TIMESTAMP.
  ENDIF.
*  --- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_ODATA_GW_ERR_MNTR'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*  --- Run Cloud Mode -----
* Extract data to t_data table.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA2
    FROM /IWFND/SU_ERRLOG AS A
     WHERE A~SAPCLIENT       IN R_SAPCLIENT
     AND   A~USERNAME        IN R_USERNAME
     AND   A~ERROR_COUNT     IN R_ERROR_COUNT
     AND   A~T100_MSGID      IN R_T100_MSGID
     AND   A~T100_MSGNO      IN R_T100_MSGNO
     AND   A~T100_MSGV1      IN R_T100_MSGV1
     AND   A~T100_MSGV2      IN R_T100_MSGV2
     AND   A~T100_MSGV3      IN R_T100_MSGV3
     AND   A~T100_MSGV4      IN R_T100_MSGV4
     AND   A~LOCATION        IN R_LOCATION
     AND   A~CHANNEL         IN R_CHANNEL
     AND   A~ERROR_TEXT      IN R_ERROR_TEXT
     AND   A~ERROR_COMPONENT IN R_ERROR_COMPONENT
     AND   A~ERROR_PACKAGE   IN R_ERROR_PACKAGE
     AND   A~SOURCE_PROGRAM  IN R_SOURCE_PROGRAM
     AND   A~SOURCE_INCLUDE  IN R_SOURCE_INCLUDE
     AND   A~SOURCE_LINE     IN R_SOURCE_LINE
     AND   A~NAMESPACE       IN R_NAMESPACE
     AND   A~SERVICE_NAME    IN R_SERVICE_NAME
     AND   A~TRANSACTION_ID  IN R_TRANSACTION_ID
     AND   A~ROOT_CONTEXT_ID IN R_ROOT_CONTEXT_ID
     AND   A~CONNECTION_ID   IN R_CONNECTION_CNT
     AND   A~TERMINAL_ID     IN R_TERMINAL_ID
     AND   A~REMOTE_ADDRESS  IN R_REMOTE_ADDRESS
     AND   A~DESTINATION     IN R_DESTINATION
     AND   A~REQUEST_URI     IN R_REQUEST_URI
     AND   A~BALOGNO         IN R_BALOGNO
     AND   A~CONTEXT_SIZE    IN R_CONTEXT_SIZE
     AND   A~CALLSTACK_SIZE  IN R_CALLSTACK_SIZE
     AND   A~REQUEST_SIZE    IN R_REQUEST_SIZE
     AND   A~RESPONSE_SIZE   IN R_RESPONSE_SIZE
     "AND   a~html_page       IN r_html_page
     AND   A~ICF_NODE        IN R_ICF_NODE
     AND   A~REQUEST_ID      IN R_REQUEST_ID
     AND   A~SERVICE_VERSION IN R_SERVICE_VERSION
     AND   A~LOG_LEVEL       IN R_LOG_LEVEL
     "AND   a~service_repo    IN r_service_repo
     "AND   a~http_status     IN r_http_status
     AND   A~TIMESTAMP       IN R_TIMESTAMP
     .
*
*
* ----------------------------------------------------------------------
* Post retrieving manipulations
* ----------------------------------------------------------------------
* Fill Date & Time fields based on Timestamp
   CHECK NOT LT_DATA2[]  IS INITIAL .
   LOOP AT LT_DATA2 INTO LS_DATA2.
     CONVERT TIME STAMP LS_DATA2-TIMESTAMP TIME ZONE LV_TZONE
        INTO DATE LS_DATA2-DATE TIME LS_DATA2-TIME.
     APPEND LS_DATA2 TO T_DATA.
   ENDLOOP.
* ----------------------------------------------------------------------
* Post retrieving filtering
* ----------------------------------------------------------------------
* no action
*
* ----------------------------------------------------------------------
* Finishing
* ----------------------------------------------------------------------
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
