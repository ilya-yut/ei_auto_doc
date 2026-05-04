# Exception Indicator: IDOCs State - SW_01_02_IDOCS

## General Overview

This Exception Indicator (EI) monitors IDoc processing state and highlights delayed, failed, and inconsistent IDoc outcomes in outbound/inbound interfaces.

This EI helps by:
- Detecting IDocs with problematic status patterns before business impact escalates
- Segmenting analysis by message type, basic type, direction, and partner fields
- Prioritizing unresolved items using duration and state indicators
- Supporting audit-ready monitoring of interface status handling

The function reads IDoc control/status data, enriches it with status text and color/icon mapping, calculates elapsed duration, and returns exception-focused records.


## Problem Description

When IDoc state monitoring is inconsistent, failed or delayed interface processing can remain unresolved and disrupt dependent business flows.

**Operational and Process Risks**
- Error-state IDocs may remain open without timely remediation
- Processing delay can increase when duration-based prioritization is missing
- Partner/direction-specific interface issues can recur unnoticed

**Control and Compliance Risks**
- Weak IDoc exception traceability reduces control evidence quality
- Inconsistent status review weakens governance and audit readiness
- Missing standard filter logic makes period-over-period comparison unreliable

**Management Visibility Risks**
- Persistent interface instability may surface too late
- Resource planning is harder without clear status-aging insights
- Root-cause analysis slows down without structured segmentation

### Suggested Resolution

**Immediate Response**
- Triage high-risk status and color groups first
- Prioritize oldest unresolved IDocs by duration and update date/time
- Escalate repeated failing message types and partner combinations

**System Assessment**
- Review trends by status, message type, basic type, and direction
- Validate date and duration filter configuration used in monitoring
- Verify UTC/local-time handling consistency where landscapes are distributed

**Corrective Actions**
- Standardize IDoc monitoring cadence and ownership
- Improve remediation playbooks for common status failures
- Feed recurring failure patterns back into configuration and alerting


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ARCKEY | EDI Archive Key | CHAR | 70 | 0 | IDOCCARKEY | EDI_ARCKEY |
| 2 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 3 | CIMTYP | Extension | CHAR | 30 | 0 | EDI_CIMTYP | EDI_CIMTYP |
| 4 | COUNTR | Status counter | NUMC | 16 | 0 | EDI_COUNTR | EDI_COUNTR |
| 5 | CREDAT | Created On | DATS | 8 | 0 | EDI_CCRDAT | DATUM |
| 6 | CRETIM | Created at | TIMS | 6 | 0 | EDI_CCRTIM | UZEIT |
| 7 | DATE_REF_FLD | Date Ref. Field |  | 0 | 0 |  |  |
| 8 | DATUM | Date |  | 0 | 0 |  |  |
| 9 | DIRECT | Direction | CHAR | 1 | 0 | EDI_DIRECT | EDI_DIRECT |
| 10 | DOCNUM | IDOC NUMBER | NUMC | 16 | 0 | EDI_DOCNUM | EDI_DOCNUM |
| 11 | DOCREL | IDoc: SAP Release | CHAR | 4 | 0 | EDI_DOCREL | SAPRL |
| 12 | DOCTYP | IDoc Type | CHAR | 8 | 0 | EDI_DOCTYP | EDI_DOCTYP |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 14 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 15 | EXPRSS | Express | CHAR | 1 | 0 | EDI_EXPRSS | EDI_EXPRSS |
| 16 | IDOCTP | BASIC TYPE | CHAR | 30 | 0 | EDI_IDOCTP | EDI_IDOCTP |
| 17 | LOGDAT | Date status error | DATS | 8 | 0 | EDI_LOGDAT | DATUM |
| 18 | LOGTIM | Time status error | TIMS | 6 | 0 | EDI_LOGTIM | UZEIT |
| 19 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 20 | MAXSEGNUM | Number of data records | NUMC | 6 | 0 | ANZEDIDD | EDI_NUMBER |
| 21 | MESCOD | Message Variant | CHAR | 3 | 0 | EDI_MESCOD | EDI_MESCOD |
| 22 | MESFCT | Message function | CHAR | 3 | 0 | EDI_MESFCT | EDI_MESFCT |
| 23 | MESSAGE | Message text | CHAR | 220 | 0 | BAPI_MSG | TEXT220 |
| 24 | MESTYP | MESSAGE TYPE | CHAR | 30 | 0 | EDI_MESTYP | EDI_MESTYP |
| 25 | OUTMOD | Output Mode | CHAR | 1 | 0 | EDI_OUTMOD | EDI_OUTMOD |
| 26 | PROCESS_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 27 | RCVLAD | Logical address of recipient | CHAR | 70 | 0 | EDI_RCVLAD | EDI_LOGADR |
| 28 | RCVPFC | Receiver Part.Functn | CHAR | 2 | 0 | EDI_RCVPFC | EDI_PARVW |
| 29 | RCVPOR | RECEIVER PORT | CHAR | 10 | 0 | EDI_RCVPOR | EDI_PORT |
| 30 | RCVPRN | RECEIVER PARTNER NO. | CHAR | 10 | 0 | EDI_RCVPRN | EDI_PARNUM |
| 31 | RCVPRT | RECEIVER PARTNR TYPE | CHAR | 2 | 0 | EDI_RCVPRT | EDI_PARTYP |
| 32 | RCVSAD | SADR recipient addr. | CHAR | 10 | 0 | EDI_RCVSAD | EDI_SADNR |
| 33 | RCVSCA | Communication Type | CHAR | 3 | 0 | EDI_RCVSCA | EDI_SADCA |
| 34 | RCVSDF | Default Setting | CHAR | 1 | 0 | EDI_RCVSDF | EDI_SADDF |
| 35 | RCVSLF | Sequential number | CHAR | 3 | 0 | EDI_RCVSLF | EDI_SADLF |
| 36 | RCVSMN | SADR client(rec.) | CLNT | 3 | 0 | EDI_RCVSMN | EDI_SADMDT |
| 37 | RCVSNA | Internat.addr. | CHAR | 1 | 0 | EDI_RCVSNA | EDI_SADNA |
| 38 | REFGRP | Reference to Message Group | CHAR | 14 | 0 | IDOCCRFGRP | EDI_REFNUM |
| 39 | REFINT | Interchange File Reference | CHAR | 14 | 0 | IDOCCRFINT | EDI_REFNUM |
| 40 | REFMES | Message reference | CHAR | 14 | 0 | IDOCCRFMES | EDI_REFNUM |
| 41 | REPID | Program Name | CHAR | 30 | 0 | EDI_REPID | EDI_REPID |
| 42 | ROUTID | Function module | CHAR | 30 | 0 | EDI_ROUTID | EDI_ROUTID |
| 43 | SEGFLD | Field name in SAP segment | CHAR | 30 | 0 | EDI_SEGFLD | EDI_SEGFLD |
| 44 | SEGNUM | Number of SAP segment | NUMC | 6 | 0 | IDOCSSGNUM | EDI_NUMBER |
| 45 | SERIAL | Serialization | CHAR | 20 | 0 | EDI_SERIAL | CHAR20 |
| 46 | SNDLAD | Logical address of sender | CHAR | 70 | 0 | EDI_SNDLAD | EDI_LOGADR |
| 47 | SNDPFC | Sender partn.funct. | CHAR | 2 | 0 | EDI_SNDPFC | EDI_PARVW |
| 48 | SNDPOR | SENDER PORT | CHAR | 10 | 0 | EDI_SNDPOR | EDI_PORT |
| 49 | SNDPRN | SENDER PARTNER NO. | CHAR | 10 | 0 | EDI_SNDPRN | EDI_PARNUM |
| 50 | SNDPRT | SENDER PARTNER TYPE | CHAR | 2 | 0 | EDI_SNDPRT | EDI_PARTYP |
| 51 | SNDSAD | SADR sender addr. | CHAR | 10 | 0 | EDI_SNDSAD | EDI_SADNR |
| 52 | SNDSCA | Communication Type | CHAR | 3 | 0 | EDI_SNDSCA | EDI_SADCA |
| 53 | SNDSDF | Default Setting | CHAR | 1 | 0 | EDI_SNDSDF | EDI_SADDF |
| 54 | SNDSLF | Sequential number | CHAR | 3 | 0 | EDI_SNDSLF | EDI_SADLF |
| 55 | SNDSMN | SADR sender client | CLNT | 3 | 0 | EDI_SNDSMN | EDI_SADMDT |
| 56 | SNDSNA | Internat.addr. | CHAR | 1 | 0 | EDI_SNDSNA | EDI_SADNA |
| 57 | STACOD | Status code | CHAR | 8 | 0 | EDI_STACOD | EDI_STACOD |
| 58 | STAMID | Status message ID | CHAR | 20 | 0 | EDI_STAMID | EDI_STAMID |
| 59 | STAMNO | Status message number | NUMC | 3 | 0 | EDI_STAMNO | EDI_STAMNO |
| 60 | STAMQU | Status message qualifier | CHAR | 3 | 0 | EDI_STAMQU | EDI_STAMQU |
| 61 | STAPA1 | Parameter 1 | CHAR | 50 | 0 | EDI_STAPA1 | EDI_PARMTR |
| 62 | STAPA2 | Parameter 2 | CHAR | 50 | 0 | EDI_STAPA2 | EDI_PARMTR |
| 63 | STAPA3 | Parameter 3 | CHAR | 50 | 0 | EDI_STAPA3 | EDI_PARMTR |
| 64 | STAPA4 | Parameter 4 | CHAR | 50 | 0 | EDI_STAPA4 | EDI_PARMTR |
| 65 | STATE_COLOR | STATE COLOR | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 66 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 67 | STATUS | IDOC STATUS | CHAR | 2 | 0 | EDI_STATUS | EDI_STATUS |
| 68 | STATUS_DESC | Short text | CHAR | 60 | 0 | EDI_TEXT60 | TEXT60 |
| 69 | STATXT | Text for status code | CHAR | 70 | 0 | EDI_STATX_ | EDI_STATXT |
| 70 | STATYP | Msg.type (E,I,W,A,S) | CHAR | 1 | 0 | EDI_SYMSTY | EDI_SYMSTY |
| 71 | STD | EDI Standard | CHAR | 1 | 0 | EDI_STD | EDI_STD |
| 72 | STDMES | EDI message type | CHAR | 6 | 0 | EDI_STDMES | EDI_STDMES |
| 73 | STDVRS | EDI std version | CHAR | 6 | 0 | EDI_STDVRS | EDI_STDVRS |
| 74 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 75 | TEST | Test Flag | CHAR | 1 | 0 | EDI_TEST | EDI_TEST |
| 76 | TID | Transaction ID | CHAR | 24 | 0 | EDI_TID | EDI_TID |
| 77 | UNAME | User | CHAR | 12 | 0 | EDI_UNAME | UNAME |
| 78 | UPDDAT | CHANGED ON | DATS | 8 | 0 | EDI_UPDDAT | DATUM |
| 79 | UPDTIM | TIME CHANGED | TIMS | 6 | 0 | EDI_UPDTIM | UZEIT |
| 80 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 80 parameters listed in the Parameters Reference Table above.

**ARCKEY** (EDI Archive Key):

ARCKEY directs attention to archived IDoc linkage key so monitoring can target interface records tied to a specific archive chain.

**BACKDAYS** (Back Days):

BACKDAYS sets fallback lookback days when no explicit date range is provided for IDoc selection.

**CIMTYP** (Extension):

CIMTYP restricts analysis to extension type variants, useful when custom extension processing differs from baseline IDoc flow.

**COUNTR** (Status counter):

COUNTR narrows by status-counter sequence to isolate repeated status transitions for the same IDoc lifecycle.

**CREDAT** (Created On):

CREDAT applies selection on IDoc creation date and is commonly used for period-oriented operational review.

**CRETIM** (Created at):

CRETIM refines creation-time boundaries inside the selected creation-date window.

**DATE_REF_FLD** (Date Ref. Field):

DATE_REF_FLD chooses which date context should drive date-oriented logic when multiple date fields are available.

**DATE_REF_FLD Options:**
- **CREDAT**: Use creation-date context.
- **UPDDAT**: Use last-change-date context.
- Use code-defined date-reference mapping when explicitly provided.

**DATUM** (Date):

DATUM is the explicit monitoring date selector used as the primary date window when supplied.

**DIRECT** (Direction):

DIRECT applies selection on IDoc direction (inbound/outbound) so monitoring focuses on the relevant integration flow.

**DOCNUM** (IDOC NUMBER):

DOCNUM selects specific IDoc numbers for record-level investigation and incident follow-up.

**DOCREL** (IDoc: SAP Release):

DOCREL constrains records by SAP release context, useful when behavior differences are release-specific.

**DOCTYP** (IDoc Type):

DOCTYP applies selection on IDoc type category for document-family-specific monitoring.

**DURATION** (Duration In Time Units):

DURATION defines the elapsed-time threshold used to prioritize aged or delayed IDoc processing.

**DURATION_UNIT** (Duration Unit(D/H/M)):

DURATION_UNIT defines the unit used for DURATION calculations, changing whether thresholds are interpreted in minutes/hours/days.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**EXPRSS** (Express):

EXPRSS segments express-processing flag context for urgency-sensitive interface monitoring.

**IDOCTP** (BASIC TYPE):

IDOCTP restricts to basic type, which is key when troubleshooting a specific message structure.

**LOGDAT** (Date status error):

LOGDAT applies selection on status-log date so analysts can focus on when status errors were recorded.

**LOGTIM** (Time status error):

LOGTIM refines status-log time slicing within the selected log date.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

MANAGE_IN_UTC controls whether current-time comparison uses UTC normalization or local-time interpretation.

**MANAGE_IN_UTC Options:**
- **X**: UTC mode for distributed or multi-time-zone landscapes.
- **(blank)**: Local/system time mode for single-time-zone operation.

**MAXSEGNUM** (Number of data records):

MAXSEGNUM segments segment-count size, helping identify unusually large or complex IDoc payloads.

**MESCOD** (Message Variant):

MESCOD applies selection on message variant and helps separate processing scenarios within one message type.

**MESFCT** (Message function):

MESFCT narrows review to message function (business operation intent) to isolate relevant interface actions.

**MESSAGE** (Message text):

MESSAGE holds resolved status text and supports direct, human-readable triage of IDoc issues.

**MESTYP** (MESSAGE TYPE):

MESTYP is the primary message-type filter used to segment monitoring by business integration process.

**OUTMOD** (Output Mode):

OUTMOD focuses analysis on output mode so monitoring can isolate processing behavior by transmission method.

**PROCESS_ICON** (State Icon):

PROCESS_ICON provides quick visual emphasis for non-green/problematic IDoc rows during triage.

**RCVLAD** (Logical address of recipient):

RCVLAD constrains receiver logical address, enabling target-system-specific routing analysis.

**RCVPFC** (Receiver Part.Functn):

RCVPFC constrains receiver partner function to focus on role-specific integration relationships.

**RCVPOR** (RECEIVER PORT):

RCVPOR constrains receiver port for endpoint-specific troubleshooting.

**RCVPRN** (RECEIVER PARTNER NO.):

RCVPRN applies selection on receiver partner number for partner-specific incident analysis.

**RCVPRT** (RECEIVER PARTNR TYPE):

RCVPRT targets receiver partner type to separate business-partner categories in monitoring.

**RCVSAD** (SADR recipient addr.):

RCVSAD applies selection on receiver SADR address references for address-level diagnostics.

**RCVSCA** (Communication Type):

RCVSCA narrows review to receiver communication type, useful when channels have different failure patterns.

**RCVSDF** (Default Setting):

RCVSDF focuses analysis on receiver default-setting indicators used in routing/address behavior.

**RCVSLF** (Sequential number):

RCVSLF focuses analysis on receiver-side sequential identifiers to isolate record-order issues.

**RCVSMN** (SADR client(rec.)):

RCVSMN targets receiver SADR client context in cross-client/interface routing checks.

**RCVSNA** (Internat.addr.):

RCVSNA segments receiver international address context for cross-border addressing analysis.

**REFGRP** (Reference to Message Group):

REFGRP pins down message-group references to analyze grouped transmission behavior.

**REFINT** (Interchange File Reference):

REFINT focuses analysis on interchange-file reference values used to trace IDocs that belong to the same interchange batch.

**REFMES** (Message reference):

REFMES narrows review to message-reference links to follow related IDoc message chains end-to-end.

**REPID** (Program Name):

REPID constrains program name that touched/generated the record, supporting source-program root-cause analysis.

**ROUTID** (Function module):

ROUTID directs attention to function module route identifier to isolate issues tied to specific processing modules.

**SEGFLD** (Field name in SAP segment):

SEGFLD segments segment field names for payload-content-oriented diagnostics.

**SEGNUM** (Number of SAP segment):

SEGNUM directs attention to segment numbers to focus on specific segment positions in IDoc payload.

**SERIAL** (Serialization):

SERIAL focuses analysis on serialization context for sequence-dependent processing checks.

**SNDLAD** (Logical address of sender):

SNDLAD pins down sender logical address for source-system routing analysis.

**SNDPFC** (Sender partn.funct.):

SNDPFC pins down sender partner function for role-specific source analysis.

**SNDPOR** (SENDER PORT):

SNDPOR pins down sender port to isolate source endpoint behavior.

**SNDPRN** (SENDER PARTNER NO.):

SNDPRN segments sender partner number for sender-specific monitoring.

**SNDPRT** (SENDER PARTNER TYPE):

SNDPRT applies selection on sender partner type to separate sender categories in analysis.

**SNDSAD** (SADR sender addr.):

SNDSAD segments sender SADR address references for address-level source checks.

**SNDSCA** (Communication Type):

SNDSCA constrains sender communication type to compare channel-specific outcomes.

**SNDSDF** (Default Setting):

SNDSDF narrows review to sender default-setting indicators relevant to addressing/routing.

**SNDSLF** (Sequential number):

SNDSLF narrows review to sender-side sequential identifiers for sequence troubleshooting.

**SNDSMN** (SADR sender client):

SNDSMN applies selection on sender SADR client context in multi-client landscapes.

**SNDSNA** (Internat.addr.):

SNDSNA directs attention to sender international address context for international routing diagnostics.

**STACOD** (Status code):

STACOD pins down status code values used for process-state segmentation.

**STAMID** (Status message ID):

STAMID identifies message class namespace used to resolve IDoc status-message semantics.

**STAMNO** (Status message number):

STAMNO is the message number inside STAMID and pinpoints the exact message variant.

**STAMQU** (Status message qualifier):

STAMQU applies selection on status-message qualifier used to interpret message context.

**STAPA1 - STAPA4** (Status Message Parameters):

STAPA1 through STAPA4 form one ordered placeholder set used to render complete status text for IDoc diagnostics.

**STAPA1 - STAPA4 Connection:**

Interpret these fields together because final message meaning depends on combined placeholder substitution.

**STATE_COLOR** (STATE COLOR):

STATE_COLOR is the severity/state selector used for quick triage via color-coded processing outcomes.

**STATE_COLOR Options:**
- **R**: Red (error/failed outcomes).
- **G**: Green (successful outcomes).
- **Y**: Yellow (warning/in-process outcomes).
- Standard SAP mapping is used when literals are not explicitly enumerated in code.

**STATE_ICON** (State Icon):

STATE_ICON is the visual state marker derived from state color for dashboard-style scanning.

**STATUS** (IDOC STATUS):

STATUS is the IDoc status filter that controls which lifecycle states are included in the result set.

**STATUS Options:**
- Use SAP IDoc status domain values configured for this system.
- Code may derive status range from state color when status is not explicitly supplied.

**STATUS_DESC** (Short text):

STATUS_DESC is the resolved short description for STATUS and supports business-readable interpretation.

**STATXT** (Text for status code):

STATXT is used to filter and interpret IDoc monitoring records according to its business meaning in this EI.

**STATYP** (Msg.type (E,I,W,A,S)):

STATYP segments message type/severity (E/I/W/A/S) for risk-focused status review.

**STD** (EDI Standard):

STD applies selection on EDI standard context to segment multi-standard integrations.

**STDMES** (EDI message type):

STDMES focuses analysis on EDI message type within the selected EDI standard.

**STDVRS** (EDI std version):

STDVRS pins down EDI standard version for version-specific behavior analysis.

**SW_DEST** (Cloud Destination):

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TEST** (Test Flag):

TEST focuses analysis on test-flagged IDocs to separate non-production test traffic from operational traffic.

**TID** (Transaction ID):

TID targets transaction identifiers for transaction-level traceability.

**UNAME** (User):

UNAME pins down user context for ownership and change-trace analysis.

**UPDDAT** (CHANGED ON):

UPDDAT narrows review to last change date and is used for recency-focused exception monitoring.

**UPDTIM** (TIME CHANGED):

UPDTIM refines last change time boundaries in the selected update-date window.

**USER_FLD** (Dynamic Recipient User Field):

USER_FLD is a dynamic user-context selector; fixed value lists are implementation-dependent unless code explicitly defines them.

**USER_FLD Options:**
- No fixed USER_FLD value list is defined in the available code for this EI.


### Parameter Relationship

How parameter combinations work together

**Date and Time Controls:**

- **BACKDAYS** is fallback logic: when explicit date filters are not provided, BACKDAYS creates the initial lookback window.
- **DATUM**, **CREDAT**, and **UPDDAT** are explicit date filters; when provided, they define the date scope directly.
- **CRETIM** and **UPDTIM** refine time granularity within the selected date scope.

**Duration Prioritization:**

- **DURATION** + **DURATION_UNIT** form an additional age filter after date selection.
- Simple flow: first filter by date window, then filter by elapsed duration.
- Final result keeps records that satisfy both date conditions and duration conditions.

**Status Derivation and State Mapping:**

- **STATUS** and **STATE_COLOR** are linked: if STATUS is not provided and one STATE_COLOR value is selected, status range can be derived from color mapping.
- **STATUS_DESC**, **STATE_COLOR**, and **STATE_ICON** are populated together for readable triage output.

**Message Diagnostics Context:**

- **STAMID** + **STAMNO** identify the message template.
- **STAPA1 - STAPA4** provide placeholder values to render the final message text.


### Default Values
- **LANGU** - system language (SY-LANGU)
- **DURATION_UNIT** - M
- **BACKDAYS** - 1 (today and yesterday)

### Practical Example of Parameter Configuration
**Use Case 1: Recent failed outbound IDocs by type**

```plaintext
DIRECT = 2
MESTYP = ORDERS
STATUS = 51
BACKDAYS = 2
DURATION = 30
DURATION_UNIT = M
```

**Purpose:** Identify recently failing outbound IDocs for a specific message type and prioritize those with meaningful delay.

**Use Case 2: Update-date focused remediation queue**

```plaintext
UPDDAT = 20260325-20260331
UPDTIM = 080000-235959
STATE_COLOR = R
DURATION = 1
DURATION_UNIT = H
```

**Purpose:** Focus remediation on red-state IDocs updated in the current review period and aged beyond one hour.

**Use Case 3: Full-day delay monitoring**

```plaintext
DATUM = 20260320-20260331
STATUS = 64
DURATION = 1
DURATION_UNIT = F
MANAGE_IN_UTC = X
```

**Purpose:** Monitor IDocs stuck in transfer-related status for at least one full day using UTC-consistent timing.


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_02_IDOCS | ARCKEY | EDI archive key | CHAR(70) | IDOCCARKEY |
| /SKN/S_SW_01_02_IDOCS | CIMTYP | Extension | CHAR(30) | EDI_CIMTYP |
| /SKN/S_SW_01_02_IDOCS | COUNTR | IDoc status counter | NUMC(16) | EDI_COUNTR |
| /SKN/S_SW_01_02_IDOCS | CREDAT | IDoc Created On | DATS(8) | EDI_CCRDAT |
| /SKN/S_SW_01_02_IDOCS | CRETIM | IDoc Created at | TIMS(6) | EDI_CCRTIM |
| /SKN/S_SW_01_02_IDOCS | DIRECT | Direction for IDoc | CHAR(1) | EDI_DIRECT |
| /SKN/S_SW_01_02_IDOCS | DOCNUM | IDoc number | NUMC(16) | EDI_DOCNUM |
| /SKN/S_SW_01_02_IDOCS | DOCREL | SAP Release for IDoc | CHAR(4) | EDI_DOCREL |
| /SKN/S_SW_01_02_IDOCS | DOCTYP | IDoc Type | CHAR(8) | EDI_DOCTYP |
| /SKN/S_SW_01_02_IDOCS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_02_IDOCS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_02_IDOCS | EXPRSS | Overriding in inbound processing | CHAR(1) | EDI_EXPRSS |
| /SKN/S_SW_01_02_IDOCS | IDOCTP | Basic type | CHAR(30) | EDI_IDOCTP |
| /SKN/S_SW_01_02_IDOCS | LOGDAT | Date of status information | DATS(8) | EDI_LOGDAT |
| /SKN/S_SW_01_02_IDOCS | LOGTIM | Time of status information | TIMS(6) | EDI_LOGTIM |
| /SKN/S_SW_01_02_IDOCS | MAXSEGNUM | Number of data records | NUMC(6) | ANZEDIDD |
| /SKN/S_SW_01_02_IDOCS | MESCOD | Logical Message Variant | CHAR(3) | EDI_MESCOD |
| /SKN/S_SW_01_02_IDOCS | MESFCT | Logical message function | CHAR(3) | EDI_MESFCT |
| /SKN/S_SW_01_02_IDOCS | MESSAGE | Message Text | CHAR(220) | BAPI_MSG |
| /SKN/S_SW_01_02_IDOCS | MESTYP | Message Type | CHAR(30) | EDI_MESTYP |
| /SKN/S_SW_01_02_IDOCS | OUTMOD | Output Mode | CHAR(1) | EDI_OUTMOD |
| /SKN/S_SW_01_02_IDOCS | PROCESS_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_02_IDOCS | RCVLAD | Logical address of recipient | CHAR(70) | EDI_RCVLAD |
| /SKN/S_SW_01_02_IDOCS | RCVPFC | Partner Function of Receiver | CHAR(2) | EDI_RCVPFC |
| /SKN/S_SW_01_02_IDOCS | RCVPOR | Receiver port (SAP System, EDI subsystem) | CHAR(10) | EDI_RCVPOR |
| /SKN/S_SW_01_02_IDOCS | RCVPRN | Partner Number of Receiver | CHAR(10) | EDI_RCVPRN |
| /SKN/S_SW_01_02_IDOCS | RCVPRT | Partner Type of Receiver | CHAR(2) | EDI_RCVPRT |
| /SKN/S_SW_01_02_IDOCS | RCVSAD | Recipient address (SADR) | CHAR(10) | EDI_RCVSAD |
| /SKN/S_SW_01_02_IDOCS | RCVSCA | Communication type (SADR) of recipient | CHAR(3) | EDI_RCVSCA |
| /SKN/S_SW_01_02_IDOCS | RCVSDF | SADR default flag for recipient address | CHAR(1) | EDI_RCVSDF |
| /SKN/S_SW_01_02_IDOCS | RCVSLF | Sequential Number of Recipient Address (SADR) | CHAR(3) | EDI_RCVSLF |
| /SKN/S_SW_01_02_IDOCS | RCVSMN | SADR client (recipient) | CLNT(3) | EDI_RCVSMN |
| /SKN/S_SW_01_02_IDOCS | RCVSNA | SADR flag for international recipient address | CHAR(1) | EDI_RCVSNA |
| /SKN/S_SW_01_02_IDOCS | REFGRP | Reference to message group | CHAR(14) | IDOCCRFGRP |
| /SKN/S_SW_01_02_IDOCS | REFINT | Reference to interchange file | CHAR(14) | IDOCCRFINT |
| /SKN/S_SW_01_02_IDOCS | REFMES | Reference to message | CHAR(14) | IDOCCRFMES |
| /SKN/S_SW_01_02_IDOCS | REPID | Program Name | CHAR(30) | EDI_REPID |
| /SKN/S_SW_01_02_IDOCS | ROUTID | Name of subroutine (routine, function module) | CHAR(30) | EDI_ROUTID |
| /SKN/S_SW_01_02_IDOCS | SEGFLD | Field Name in SAP Segment | CHAR(30) | EDI_SEGFLD |
| /SKN/S_SW_01_02_IDOCS | SEGNUM | Number of SAP segment | NUMC(6) | IDOCSSGNUM |
| /SKN/S_SW_01_02_IDOCS | SERIAL | Serialization field | CHAR(20) | EDI_SERIAL |
| /SKN/S_SW_01_02_IDOCS | SNDLAD | Logical address of sender | CHAR(70) | EDI_SNDLAD |
| /SKN/S_SW_01_02_IDOCS | SNDPFC | Partner Function of Sender | CHAR(2) | EDI_SNDPFC |
| /SKN/S_SW_01_02_IDOCS | SNDPOR | Sender port (SAP System, EDI subsystem) | CHAR(10) | EDI_SNDPOR |
| /SKN/S_SW_01_02_IDOCS | SNDPRN | Partner Number of Sender | CHAR(10) | EDI_SNDPRN |
| /SKN/S_SW_01_02_IDOCS | SNDPRT | Partner type of sender | CHAR(2) | EDI_SNDPRT |
| /SKN/S_SW_01_02_IDOCS | SNDSAD | Sender address (SADR) | CHAR(10) | EDI_SNDSAD |
| /SKN/S_SW_01_02_IDOCS | SNDSCA | Communication type (SADR) of sender | CHAR(3) | EDI_SNDSCA |
| /SKN/S_SW_01_02_IDOCS | SNDSDF | SADR default flag for sender address | CHAR(1) | EDI_SNDSDF |
| /SKN/S_SW_01_02_IDOCS | SNDSLF | Sequential Number of the Sender Address (SADR) | CHAR(3) | EDI_SNDSLF |
| /SKN/S_SW_01_02_IDOCS | SNDSMN | SADR client (sender) | CLNT(3) | EDI_SNDSMN |
| /SKN/S_SW_01_02_IDOCS | SNDSNA | SADR flag for international sender address | CHAR(1) | EDI_SNDSNA |
| /SKN/S_SW_01_02_IDOCS | STACOD | Status code | CHAR(8) | EDI_STACOD |
| /SKN/S_SW_01_02_IDOCS | STAMID | Status message ID | CHAR(20) | EDI_STAMID |
| /SKN/S_SW_01_02_IDOCS | STAMNO | Status message number | NUMC(3) | EDI_STAMNO |
| /SKN/S_SW_01_02_IDOCS | STAMQU | Status message qualifier | CHAR(3) | EDI_STAMQU |
| /SKN/S_SW_01_02_IDOCS | STAPA1 | Parameter 1 | CHAR(50) | EDI_STAPA1 |
| /SKN/S_SW_01_02_IDOCS | STAPA2 | Parameter 2 | CHAR(50) | EDI_STAPA2 |
| /SKN/S_SW_01_02_IDOCS | STAPA3 | Parameter 3 | CHAR(50) | EDI_STAPA3 |
| /SKN/S_SW_01_02_IDOCS | STAPA4 | Parameter 4 | CHAR(50) | EDI_STAPA4 |
| /SKN/S_SW_01_02_IDOCS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_02_IDOCS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_02_IDOCS | STATUS | Status of IDoc | CHAR(2) | EDI_STATUS |
| /SKN/S_SW_01_02_IDOCS | STATUS_DESC | Short description of object | CHAR(60) | EDI_TEXT60 |
| /SKN/S_SW_01_02_IDOCS | STATXT | Text for status code | CHAR(70) | EDI_STATX_ |
| /SKN/S_SW_01_02_IDOCS | STATYP | Type of system error message (A, W, E, S, I) | CHAR(1) | EDI_SYMSTY |
| /SKN/S_SW_01_02_IDOCS | STD | EDI Standard | CHAR(1) | EDI_STD |
| /SKN/S_SW_01_02_IDOCS | STDMES | EDI message type | CHAR(6) | EDI_STDMES |
| /SKN/S_SW_01_02_IDOCS | STDVRS | Version of EDI standard | CHAR(6) | EDI_STDVRS |
| /SKN/S_SW_01_02_IDOCS | TEST | Test Flag | CHAR(1) | EDI_TEST |
| /SKN/S_SW_01_02_IDOCS | TID | Transaction ID | CHAR(24) | EDI_TID |
| /SKN/S_SW_01_02_IDOCS | UNAME | User name | CHAR(12) | EDI_UNAME |
| /SKN/S_SW_01_02_IDOCS | UPDDAT | Date on which control record was last changed | DATS(8) | EDI_UPDDAT |
| /SKN/S_SW_01_02_IDOCS | UPDTIM | Time at which control record was last changed | TIMS(6) | EDI_UPDTIM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_02_IDOCS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_IDOCS OPTIONAL
*"----------------------------------------------------------------------
  DATA :   MANAGE_IN_UTC TYPE  CHAR1 ..
  RANGES : R_MESTYP FOR EDIDC-MESTYP ,
           R_IDOCTP FOR EDIDC-IDOCTP,
           R_DIRECT FOR EDIDC-DIRECT,
           R_RCVPOR FOR EDIDC-RCVPOR,
           R_RCVPRT FOR EDIDC-RCVPRT,
           R_RCVPRN FOR EDIDC-RCVPRN,
           R_SNDPOR FOR EDIDC-SNDPOR,
           R_SNDPRT FOR EDIDC-SNDPRT,
           R_SNDPRN FOR EDIDC-SNDPRN,
           R_CREDAT FOR EDIDC-CREDAT,
           R_CRETIM FOR EDIDC-CRETIM,
           R_UPDDAT FOR EDIDC-UPDDAT,
           R_UPDTIM FOR EDIDC-UPDTIM,
           R_STATUS   FOR EDIDC-STATUS,
           R_STATE_COLOR FOR /SKN/S_SW_SYS_IDOCS-STATE_COLOR,
           R_DATUM   FOR SY-DATUM .
  DATA_SINGLE: DURATION_UNIT         /SKN/E_SW_DURATION_UNIT,
               STATE_COLOR           /SKN/E_SW_STATE_COLOR,
               ONLY_HEADER           CHAR1  .
  DATA_MULTY: DURATION      /SKN/E_SW_DURATION.   "From NOW to  Start Time point -in duration units
  DATA :   IS_GENERAL(1) TYPE C.
  DATA : DATE_FROM LIKE SY-DATUM ,
         BACKDAYS  TYPE I .
  DATA : LANGU LIKE SY-LANGU .
  DATA : PARAM_VAL TYPE /SKN/E_SW_PARAM .
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : WA_EDIDS TYPE EDIDS .
  DATA: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
  DATA :   SY_DATLO LIKE SY-DATLO ,
           SY_TIMLO LIKE SY-TIMLO .
  DATA : TIME_DIFF TYPE I .
  DATA: LS_EDIDC TYPE EDIDC,
        LT_EDIDC LIKE TABLE OF LS_EDIDC.
  DATA: LS_EDIDS TYPE EDIDS,
        LT_EDIDS LIKE TABLE OF LS_EDIDS.
*-- Fill Selection Option Tables
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT,
                 ONLY_HEADER.
  SELECT_MULTY: DURATION.
  "--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_02_IDOCS'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Clood Mode -----
  LOOP AT T_SELECT WHERE FIELDNM = 'MANAGE_IN_UTC'.
    MANAGE_IN_UTC = T_SELECT-LOW.
    EXIT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'MESTYP'.
    MOVE-CORRESPONDING T_SELECT TO R_MESTYP.
    APPEND R_MESTYP.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
    MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
    APPEND R_STATE_COLOR.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'STATUS'.
    MOVE-CORRESPONDING T_SELECT TO R_STATUS.
    APPEND R_STATUS.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'IDOCTP'.
    MOVE-CORRESPONDING T_SELECT TO R_IDOCTP.
    APPEND R_IDOCTP.
  ENDLOOP.
*
  LOOP AT T_SELECT WHERE FIELDNM = 'DIRECT'.
    MOVE-CORRESPONDING T_SELECT TO R_DIRECT.
    APPEND R_DIRECT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'RCVPOR'.
    MOVE-CORRESPONDING T_SELECT TO R_RCVPOR.
    APPEND R_RCVPOR.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'RCVPRT'.
    MOVE-CORRESPONDING T_SELECT TO R_RCVPRT.
    APPEND R_RCVPRT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'RCVPRT'.
    MOVE-CORRESPONDING T_SELECT TO R_RCVPRT.
    APPEND R_RCVPRT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'RCVPRN'.
    MOVE-CORRESPONDING T_SELECT TO R_RCVPRN.
    APPEND R_RCVPRN.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'SNDPOR'.
    MOVE-CORRESPONDING T_SELECT TO R_SNDPOR.
    APPEND R_SNDPOR.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'SNDPRT'.
    MOVE-CORRESPONDING T_SELECT TO R_SNDPRT.
    APPEND R_SNDPRT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'SNDPRN'.
    MOVE-CORRESPONDING T_SELECT TO R_SNDPRN.
    APPEND R_SNDPRN.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'CREDAT'.
    MOVE-CORRESPONDING T_SELECT TO R_CREDAT.
    APPEND R_CREDAT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'CRETIM'.
    MOVE-CORRESPONDING T_SELECT TO R_CRETIM.
    APPEND R_CRETIM.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'UPDDAT'.
    MOVE-CORRESPONDING T_SELECT TO R_UPDDAT.
    APPEND R_UPDDAT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'UPDTIM'.
    MOVE-CORRESPONDING T_SELECT TO R_UPDTIM.
    APPEND R_UPDTIM.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
    MOVE-CORRESPONDING T_SELECT TO R_DATUM.
    APPEND R_DATUM.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'IS_GENERAL'.
    IF NOT T_SELECT-LOW IS INITIAL.
      IS_GENERAL = 'X'.
    ENDIF.
    EXIT .
  ENDLOOP.
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
  "----
  IF R_CREDAT[] IS INITIAL AND
     R_UPDDAT[] IS INITIAL.
    R_CREDAT[] = R_DATUM[].
  ENDIF.
  "-----
  LANGU = SY-LANGU.
  LOOP AT T_SELECT WHERE FIELDNM = 'LANGU'.
    LANGU = T_SELECT-LOW.
    EXIT.
  ENDLOOP.
  SET_SY_TIME MANAGE_IN_UTC SY_DATLO SY_TIMLO .
  TIME_SHIFT SY_DATLO SY_TIMLO . " TIME_SHIFT parameter
  "--- Complete Status
  IF R_STATUS[] IS INITIAL.
    IF R_STATE_COLOR[] IS NOT INITIAL.
      READ TABLE R_STATE_COLOR INDEX 1.
      IF SY-TFILL = 1.
        LV_STATE_COLOR = R_STATE_COLOR-LOW.
        CALL FUNCTION '/SKN/F_SW_01_02_IDOC_STAT_RNR'
          EXPORTING
            STATE_COLOR = LV_STATE_COLOR
          TABLES
            R_STATUS    = R_STATUS.
      ENDIF.
    ENDIF.
  ENDIF.
  "--- Complete Status
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  SELECT *
     FROM EDIDC
     INTO CORRESPONDING FIELDS OF TABLE LT_EDIDC  " t_data
     WHERE MESTYP IN R_MESTYP
       AND IDOCTP IN R_IDOCTP
       AND DIRECT IN R_DIRECT
       AND RCVPOR IN R_RCVPOR
       AND RCVPRT IN R_RCVPRT
       AND RCVPRN IN R_RCVPRN
       AND SNDPOR IN R_SNDPOR
       AND SNDPRT IN R_SNDPRT
       AND SNDPRN IN R_SNDPRN
       AND CREDAT IN R_CREDAT
       AND CRETIM IN R_CRETIM
       AND UPDDAT IN R_UPDDAT
       AND UPDTIM IN R_UPDTIM
       AND STATUS IN R_STATUS .
  SORT LT_EDIDC BY DOCNUM.
  IF LV_ONLY_HEADER IS INITIAL.
    IF LT_EDIDC[] IS NOT INITIAL.
      SELECT *
         FROM EDIDS
         INTO CORRESPONDING FIELDS OF TABLE LT_EDIDS
         FOR ALL ENTRIES IN LT_EDIDC
         WHERE DOCNUM = LT_EDIDC-DOCNUM
           AND STATUS = LT_EDIDC-STATUS.
      SORT LT_EDIDS BY DOCNUM STATUS LOGDAT DESCENDING LOGTIM DESCENDING.
    ENDIF.
  ENDIF.
  LOOP AT LT_EDIDC INTO LS_EDIDC.
    MOVE-CORRESPONDING LS_EDIDC TO T_DATA.
    READ TABLE LT_EDIDS INTO LS_EDIDS WITH KEY DOCNUM = LS_EDIDC-DOCNUM
                                               STATUS = LS_EDIDC-STATUS.
    APPEND T_DATA.
  ENDLOOP.
*-- Fill Duration Value
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX.
    T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = T_DATA-UPDDAT
        T_FROM      = T_DATA-UPDTIM
        D_TO        = SY_DATLO
        T_TO        = SY_TIMLO
        TIME_UNIT   = LV_DURATION_UNIT
      IMPORTING
        TIME_DIFF   = TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      T_DATA-DURATION = TIME_DIFF .
    ELSE.
      T_DATA-DURATION = '999999'.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX .
  ENDLOOP .
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_01_02_IDOC_STATUS'
      EXPORTING
        STATUS      = T_DATA-STATUS
        LANGU       = LANGU
      IMPORTING
        STATUS_DESC = T_DATA-STATUS_DESC
        STATE_COLOR = T_DATA-STATE_COLOR.
    CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
      EXPORTING
        STATE_COLOR = T_DATA-STATE_COLOR
      IMPORTING
        STATE_ICON  = T_DATA-STATE_ICON.
    "--- Set Process Icon for Red Yeloow
    IF T_DATA-STATE_COLOR <> 'G' .
      PROCESS_ICON = '@15@'.
      GET_PARAMETER 'PROCESS_ICON' PARAM_VAL.
      PROCESS_ICON = PARAM_VAL.
      T_DATA-PROCESS_ICON = PROCESS_ICON.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-STAMID IS INITIAL.
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          ID         = T_DATA-STAMID
          NUMBER     = T_DATA-STAMNO
          LANGUAGE   = LANGU
          TEXTFORMAT = 'ASC'
*         LINKPATTERN       =
          MESSAGE_V1 = T_DATA-STAPA1
          MESSAGE_V2 = T_DATA-STAPA2
          MESSAGE_V3 = T_DATA-STAPA3
          MESSAGE_V4 = T_DATA-STAPA4
        IMPORTING
          MESSAGE    = T_DATA-MESSAGE
*         RETURN     =
*       TABLES
*         TEXT       =
        .
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
