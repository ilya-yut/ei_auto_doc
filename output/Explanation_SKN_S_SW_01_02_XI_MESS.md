# Exception Indicator: XI Monitor Messages - SW_01_02_XI_MESS

## General Overview

This Exception Indicator monitors Exchange Infrastructure / Process Integration style message traffic selected through the standard message browser API, with emphasis on execution timestamps, message state, error classification, and sender or receiver identifiers. It supports operations and integration teams who must detect backlog, failure, or latency patterns before business processes that depend on asynchronous messaging stall.

This EI serves as an essential control for interface and application management by:

- Enabling detection of unusual concentrations of errors, system failures, or adapter states that warrant immediate technical review.
- Supporting prioritization when specific sender or receiver interfaces, parties, or logical systems dominate the exception set.
- Providing a repeatable view of message age and processing status aligned with how administrators triage SXMB_MONI-style scenarios.
- Helping month-end and release windows by surfacing spikes that correlate with transport activity or partner onboarding.
- Giving management confidence that asynchronous traffic is observed with the same discipline as synchronous RFC or IDoc checks.

Typical use cases include daily integration health reviews, escalation after batch-driven message bursts, and evidence gathering for audits of cross-system communication reliability.

The function relies on SAP standard message selection services and post-filters the returned rows by category, type, state, and visual severity indicators consistent with operational monitoring practice.


## Problem Description

Failure to monitor integration message queues and their processing outcomes creates multiple risks across business process continuity, customer-facing communications, and the ability to prove control over automated cross-system flows.

**Integration and Business Continuity Risks**

- Stuck or failing messages can delay financial postings, confirmations, or logistics updates without a blocking dialog error for end users.
- Error categories that cluster on one interface or logical system may hide a single misconfiguration until volumes overwhelm support capacity.
- Retries and acknowledgments that never complete can leave business documents in ambiguous states across systems.

**Operational and Technical Risks**

- Teams may react only after downstream batch aborts or manual user complaints, missing earlier concentration of adapter or pipeline issues.
- Without a bounded time view, administrators cannot distinguish normal overnight backlog from a genuine processing incident.
- Security or routing changes can shift traffic patterns in ways that remain invisible when only single-message lookups are used.

**Management Visibility and Decision-Making Risks**

- Leadership lacks a concise signal on whether integration automation is healthy during high-change periods.
- Service reviews between business and IT become subjective when no agreed monitoring lens exists for message state and age.
- Post-incident analysis struggles to show that monitoring scope matched the real landscape of senders, receivers, and error families.

## Suggested Resolution

**Immediate Response**

- When the monitor draws attention, review the same slice of traffic in the standard SAP message monitoring path your organization uses for cross-system troubleshooting, aligned to the sender, receiver, and error context implied by the results.
- Identify whether failures cluster on one logical system, interface name, or error category before opening individual technical traces.
- Check whether the observation aligns with a known release, certificate change, or partner maintenance window.
- Capture representative examples for the integration owner if functional correction is required.

**System Assessment**

- Compare current message states and severity indicators to the prior week for the same organizational or technical slice.
- Validate whether aging behavior matches expectations for how long work may legitimately wait in each state band.
- Correlate spikes with batch schedules or mass-restart jobs that enqueue large volumes of asynchronous messages.
- Review whether UTC versus local-time interpretation could explain apparent drift around midnight boundaries.

**Corrective Actions**

- Correct interface metadata, party definitions, or adapter configuration when misalignment is confirmed.
- Coordinate with basis and middleware teams on pipeline or gateway capacity when infrastructure limits are suspected.
- Adjust monitoring thresholds or scope after baseline behavior is documented, and record the rationale for audit trail.
- Fold recurring guidance into existing operational runbooks so first-line staff recognize the pattern without adding a separate training subsection.


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACK_STATE | Acknowledgment Status | NUMC | 3 | 0 | SXMSPACKSTAT | SXMSPACKSTAT |
| 2 | ADAPT_STAT | Outbound Status | NUMC | 3 | 0 | SXMSPASTAT | SXMSPASTAT |
| 3 | ADAPT_TP_I | Type | CHAR | 10 | 0 | SXMSPSTYPE | SXMSPSTYPE |
| 4 | ADAPT_TYP | Type | CHAR | 10 | 0 | SXMSPSTYPE | SXMSPSTYPE |
| 5 | ADMINUSER | User Name | CHAR | 12 | 0 | SYUNAME | SYCHAR12 |
| 6 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 7 | CHILDCOUNT | Dummy | NUMC | 3 | 0 | NUMC3 | NUMC3 |
| 8 | CLIENT | Client ID | CLNT | 3 | 0 | SYMANDT | MANDT |
| 9 | COMMITACT | Flag | CHAR | 1 | 0 | SXMSFLAG | SXMSFLAG |
| 10 | DATUM | Date |  | 0 | 0 |  |  |
| 11 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 12 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 13 | EO_REFID | Reference Type | CHAR | 8 | 0 | SXMSREFID | SXMSREFID |
| 14 | EO_REFID_I | Reference Type | CHAR | 8 | 0 | SXMSREFID | SXMSREFID |
| 15 | EO_REFID_O | Reference Type | CHAR | 8 | 0 | SXMSREFID | SXMSREFID |
| 16 | EO_REFVAL | Reference Value | CHAR | 32 | 0 | SXMSREFVAL |  |
| 17 | EO_REFVL_I | Reference Value | CHAR | 32 | 0 | SXMSREFVAL |  |
| 18 | EO_REFVL_O | Reference Value | CHAR | 32 | 0 | SXMSREFVAL |  |
| 19 | ERRCAT | Error Category | CHAR | 20 | 0 | SXMSERRCAT | TEXT20 |
| 20 | ERRCODE | Error ID | CHAR | 30 | 0 | SXMSERRID |  |
| 21 | EXE_DATE | Current Date | DATS | 8 | 0 | SYDATUM | SYDATS |
| 22 | EXE_TIME | Time | TIMS | 6 | 0 | SYUZEIT | SYTIME |
| 23 | EXEDATE | Date |  | 0 | 0 |  |  |
| 24 | EXETIME | Time |  | 0 | 0 |  |  |
| 25 | EXETIMEST | Time Stamp | DEC | 21 | 7 | TIMESTAMPL | TZNTSTMPL |
| 26 | GEN_ENTRY | Flag | CHAR | 1 | 0 | SXMSFLAG | SXMSFLAG |
| 27 | IB_NAME | Receivr Interf. Name | CHAR | 120 | 0 | RM_IIFNAME | AIT_INTF |
| 28 | IB_NS | Recr If Namespace | CHAR | 255 | 0 | RM_IIFNS | SAI_NSPCE |
| 29 | IB_PARTY | Communication Party | CHAR | 60 | 0 | SXI_PARTY | SXI_PARTY |
| 30 | IB_PARTY_AGENCY | Agency | CHAR | 120 | 0 | SXI_PARTY_AGENCY | SXI_PARTY_AGENCY |
| 31 | IB_PARTY_TYPE | Identification Schema | CHAR | 120 | 0 | SXI_PARTY_TYPE | SXI_PARTY_TYPE |
| 32 | IB_SYSTEM | Receiver | CHAR | 60 | 0 | AIT_RCVR | AIT_SYSTEM |
| 33 | INIT_DATE | Current Date | DATS | 8 | 0 | SYDATUM | SYDATS |
| 34 | INIT_TIME | Time | TIMS | 6 | 0 | SYUZEIT | SYTIME |
| 35 | INITTIMEST | Time Stamp | DEC | 21 | 7 | TIMESTAMPL | TZNTSTMPL |
| 36 | ITFACTION | Reorganization Action | CHAR | 4 | 0 | SXMSITFACT | SXMSITFACT |
| 37 | JOB_ID | Job ID | CHAR | 32 | 0 | SXMSJOB |  |
| 38 | MANAGE_IN_UTC | 'X' - UTC / ' ' - Local Time |  | 0 | 0 |  |  |
| 39 | MANUALSTAT | Flag | CHAR | 1 | 0 | SXMSFLAG | SXMSFLAG |
| 40 | MSGGUID | Message ID | RAW | 16 | 0 | SXMSMGUID | SYSUUID |
| 41 | MSGSTATE | MESSAGE STATUS | NUMC | 3 | 0 | SXMSPMSTAT | SXMSPMSTAT |
| 42 | MSGTXT | Description | CHAR | 60 | 0 | SXMSPMSTATTX | TEXT60 |
| 43 | MSGTYPE | MESSAGE TYPE | CHAR | 1 | 0 | SXMSPMTYPE | SXMSPMTYPE |
| 44 | OB_NAME | SENDERINTERFACE NAME | CHAR | 120 | 0 | RM_OIFNAME | AIT_INTF |
| 45 | OB_NS | SENDER IF NAMESPACE | CHAR | 255 | 0 | RM_OIFNS | SAI_NSPCE |
| 46 | OB_PARTY | Communication Party | CHAR | 60 | 0 | SXI_PARTY | SXI_PARTY |
| 47 | OB_PARTY_AGENCY | Agency | CHAR | 120 | 0 | SXI_PARTY_AGENCY | SXI_PARTY_AGENCY |
| 48 | OB_PARTY_TYPE | Identification Schema | CHAR | 120 | 0 | SXI_PARTY_TYPE | SXI_PARTY_TYPE |
| 49 | OB_SYSTEM | Sender | CHAR | 60 | 0 | AIT_SNDR | AIT_SYSTEM |
| 50 | PARENTMSG | Message ID | RAW | 16 | 0 | SXMSMGUID | SYSUUID |
| 51 | PID | Pipeline ID | CHAR | 40 | 0 | SXMSPID | SXMSPID |
| 52 | QUEUEGUID | Message Queue | CHAR | 32 | 0 | SXMSQID | SXMSQID |
| 53 | QUEUEINT | Queue Name | CHAR | 24 | 0 | TRFCQNAM | TRFCQNAM |
| 54 | REF_TO_MSG | Message ID | RAW | 16 | 0 | SXMSMGUID | SYSUUID |
| 55 | REF_TO_REC_MSG | Message ID | RAW | 16 | 0 | SXMSMGUID | SYSUUID |
| 56 | REORG | Reorganization State | CHAR | 3 | 0 | SXMSREORG | SXMSREORG |
| 57 | REST_VERS | Message Version | NUMC | 3 | 0 | SXMSLSQNBR |  |
| 58 | RETRYCOUNT | Dummy | NUMC | 3 | 0 | NUMC3 | NUMC3 |
| 59 | RETRYCOUNT_MAN | Man. Retry Counter | INT4 | 10 | 0 | SXMS_MAN_RETRYCO | SXMS_MAN_RETRYCO |
| 60 | RND_NUMBER | Random Number | NUMC | 2 | 0 | SXMSRNDNUM | SXMSRNDNUM |
| 61 | SECURITY | Security Flag | NUMC | 1 | 0 | SXMSSECURITY | SXMSSECURITY |
| 62 | SEND_DATE | Current Date | DATS | 8 | 0 | SYDATUM | SYDATS |
| 63 | SEND_TIME | Time | TIMS | 6 | 0 | SYUZEIT | SYTIME |
| 64 | SENDTIMEST | Time Stamp | DEC | 21 | 7 | TIMESTAMPL | TZNTSTMPL |
| 65 | SNDRGUID | Sender ID | RAW | 16 | 0 | SXMSSNDRID | SYSUUID |
| 66 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 67 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 68 | STATUS | Job Processing Status | CHAR | 1 | 0 | SXMSMSGREFSTATUS | SXMSMSGREFSTATUS |
| 69 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 70 | VERS | Message Version | NUMC | 3 | 0 | SXMSLSQNBR |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 70 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ACK_STATE** (Acknowledgment Status)

Works downstream of the initial read so acknowledgment status on ACK_STATE still participates in row-level deletion rules.

**ADAPT_STAT** (Outbound Status)

For operations, outbound status on ADAPT_STAT indicates whether a row belongs in the current monitoring pass versus historical noise.

**ADAPT_TP_I** (Type)

Helps monitoring stay readable by requiring type (ADAPT_TP_I) to match organizational or technical selectors when set.

**ADAPT_TYP** (Type)

Improves readability of exported lists because type (ADAPT_TYP) columns stay aligned with the configured filter intent.

**ADMINUSER** (User Name)

Guards against oversized extracts when user name on ADMINUSER is narrowed together with client, user, or session filters.

**BACKDAYS** (Back Days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**CHILDCOUNT** (Dummy)

Helps monitoring stay readable by requiring dummy (CHILDCOUNT) to match organizational or technical selectors when set.

**CLIENT** (Client ID)

Mirrors how administrators slice operational lists: client id (CLIENT) is one lever that shapes which rows are comparable run over run.

**COMMITACT** (Flag)

Narrows retrieved rows where flag (COMMITACT) must match the configured selection for this monitor.

**DATUM** (Date)

When harmonized with related filters, date on DATUM isolates the highest-risk record families.

**DURATION** (Duration In Time Units)

Ensures reporting respects duration in time units constraints carried by DURATION.

**DURATION_UNIT** (Duration Unit(D/H/M))

Unit for elapsed time between each session's creation date and time and the evaluation clock.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**EO_REFID** (Reference Type)

When combined with destination discipline, reference type on EO_REFID keeps both breadth and depth of the extract intentional.

**EO_REFID_I** (Reference Type)

Stabilizes week-over-week metrics by fixing reference type (EO_REFID_I) while allowing duration thresholds to move.

**EO_REFID_O** (Reference Type)

Allows phased rollout: first widen EO_REFID_O for reference type, then tighten thresholds once baseline noise is understood.

**EO_REFVAL** (Reference Value)

For distributed landscapes, reference value on EO_REFVAL often anchors which application server or destination appears in results.

**EO_REFVL_I** (Reference Value)

Helps distinguish technical versus business attributes when reference value on EO_REFVL_I correlates with counters or status fields.

**EO_REFVL_O** (Reference Value)

Valuable when comparing health before and after a release—hold reference value on EO_REFVL_O constant while varying other filters.

**ERRCAT** (Error Category)

Narrows retrieved rows where error category (ERRCAT) must match the configured selection for this monitor.

**ERRCODE** (Error ID)

Uses error id from the source context so only records with ERRCODE inside declared values proceed.

**EXE_DATE** (Current Date)

Helps monitoring stay readable by requiring current date (EXE_DATE) to match organizational or technical selectors when set.

**EXE_TIME** (Time)

Reflects real administration where time on EXE_TIME is routinely restricted to a single productive client or object family.

**EXEDATE** (Date)

Reduces false positives during peak windows by tightening date through EXEDATE alongside state filters.

**EXETIME** (Time)

Uses time from the source context so only records with EXETIME inside declared values proceed.

**EXETIMEST** (Time Stamp)

Connects to alert semantics: rows removed for failing time stamp on EXETIMEST never reach downstream filtering.

**GEN_ENTRY** (Flag)

When combined with destination discipline, flag on GEN_ENTRY keeps both breadth and depth of the extract intentional.

**IB_NAME** (Receivr Interf. Name)

Guards against oversized extracts when receivr interf. name on IB_NAME is narrowed together with client, user, or session filters.

**IB_NS** (Recr If Namespace)

Allows phased rollout: first widen IB_NS for recr if namespace, then tighten thresholds once baseline noise is understood.

**IB_PARTY** (Communication Party)

Ensures reporting respects communication party constraints carried by IB_PARTY.

**IB_PARTY_AGENCY** (Agency)

When left open per framework rules, IB_PARTY_AGENCY does not restrict agency; when set, only matching rows remain.

**IB_PARTY_TYPE** (Identification Schema)

When combined with destination discipline, identification schema on IB_PARTY_TYPE keeps both breadth and depth of the extract intentional.

**IB_SYSTEM** (Receiver)

When tightened, receiver (IB_SYSTEM) removes rows that would otherwise dilute attention from failing or stuck cases.

**INIT_DATE** (Current Date)

Documents expected operator behavior—current date on INIT_DATE should be set when that dimension is part of the control objective.

**INIT_TIME** (Time)

Works downstream of the initial read so time on INIT_TIME still participates in row-level deletion rules.

**INITTIMEST** (Time Stamp)

After data is read, lines are removed unless time stamp on INITTIMEST still satisfies the active multivalued selection.

**ITFACTION** (Reorganization Action)

Allows phased rollout: first widen ITFACTION for reorganization action, then tighten thresholds once baseline noise is understood.

**JOB_ID** (Job ID)

Combines with related filters so job id on JOB_ID refines which records remain for duration or state checks.

**MANAGE_IN_UTC** ('X' - UTC / ' ' - Local Time)

Controls whether reference timestamps for filtering and duration checks are interpreted in UTC or local time.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MANUALSTAT** (Flag)

Treats flag as a discriminator between similar rows that would otherwise look identical in a raw extract.

**MSGGUID** (Message ID)

Connects to alert semantics: rows removed for failing message id on MSGGUID never reach downstream filtering.

**MSGSTATE** (MESSAGE STATUS)

Separates cross-client noise from in-scope work when message status on MSGSTATE correlates with client or user attributes.

**MSGTXT** (Description)

Connects to alert semantics: rows removed for failing description on MSGTXT never reach downstream filtering.

**MSGTYPE** (MESSAGE TYPE)

Narrows retrieved rows where message type (MSGTYPE) must match the configured selection for this monitor.

**OB_NAME** (SENDERINTERFACE NAME)

Improves readability of exported lists because senderinterface name (OB_NAME) columns stay aligned with the configured filter intent.

**OB_NS** (SENDER IF NAMESPACE)

Uses sender if namespace from the source context so only records with OB_NS inside declared values proceed.

**OB_PARTY** (Communication Party)

Helps monitoring stay readable by requiring communication party (OB_PARTY) to match organizational or technical selectors when set.

**OB_PARTY_AGENCY** (Agency)

Pairs with duration logic: once OB_PARTY_AGENCY passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**OB_PARTY_TYPE** (Identification Schema)

Supports escalation where identification schema on OB_PARTY_TYPE signals ownership for follow-up between Basis and functional teams.

**OB_SYSTEM** (Sender)

When populated, keeps the extract focused so sender (OB_SYSTEM) aligns with the intended triage slice.

**PARENTMSG** (Message ID)

Reduces false positives during peak windows by tightening message id through PARENTMSG alongside state filters.

**PID** (Pipeline ID)

When combined with destination discipline, pipeline id on PID keeps both breadth and depth of the extract intentional.

**QUEUEGUID** (Message Queue)

Captures edge cases where message queue (QUEUEGUID) must be non-default to reproduce a customer-specific monitoring scenario.

**QUEUEINT** (Queue Name)

Works downstream of the initial read so queue name on QUEUEINT still participates in row-level deletion rules.

**REF_TO_MSG** (Message ID)

Supports escalation where message id on REF_TO_MSG signals ownership for follow-up between Basis and functional teams.

**REF_TO_REC_MSG** (Message ID)

For operations, message id on REF_TO_REC_MSG indicates whether a row belongs in the current monitoring pass versus historical noise.

**REORG** (Reorganization State)

Guards against oversized extracts when reorganization state on REORG is narrowed together with client, user, or session filters.

**REST_VERS** (Message Version)

Treats message version as a discriminator between similar rows that would otherwise look identical in a raw extract.

**RETRYCOUNT** (Dummy)

When harmonized with related filters, dummy on RETRYCOUNT isolates the highest-risk record families.

**RETRYCOUNT_MAN** (Man. Retry Counter)

Interprets man. retry counter as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on RETRYCOUNT_MAN.

**RND_NUMBER** (Random Number)

Captures edge cases where random number (RND_NUMBER) must be non-default to reproduce a customer-specific monitoring scenario.

**SECURITY** (Security Flag)

Combines with related filters so security flag on SECURITY refines which records remain for duration or state checks.

**SEND_DATE** (Current Date)

Reduces false positives during peak windows by tightening current date through SEND_DATE alongside state filters.

**SEND_TIME** (Time)

When harmonized with related filters, time on SEND_TIME isolates the highest-risk record families.

**SENDTIMEST** (Time Stamp)

When tightened, time stamp (SENDTIMEST) removes rows that would otherwise dilute attention from failing or stuck cases.

**SNDRGUID** (Sender ID)

Works downstream of the initial read so sender id on SNDRGUID still participates in row-level deletion rules.

**STATE_COLOR** (State Color)

Filters lines by the derived color bucket used for severity-style triage in the monitor framework.

**STATE_COLOR Options:**
- **R** — Red (error or failed-style outcomes).
- **G** — Green (successful outcomes).
- **Y** — Yellow (warning or in-process outcomes).
- Additional literals may exist where the framework extends the palette for neutral states.

**STATE_ICON** (State Icon)

Gives auditors traceable criteria because state icon on STATE_ICON is applied consistently before any alert flag is raised.

**STATUS** (Job Processing Status)

Restricts the extract to the operational status values you configure for this EI's object type.

**STATUS Options:**
- Use status domain values defined for the underlying SAP object (see data element or domain in the system).
- Code applies STATUS as a filter; literals are environment-specific.

**SW_DEST** (Cloud Destination)

When tightened, cloud destination (SW_DEST) removes rows that would otherwise dilute attention from failing or stuck cases.

**VERS** (Message Version)

Narrows retrieved rows where message version (VERS) must match the configured selection for this monitor.


### Parameter Relationships

How parameter combinations work together

Explicit date parameters such as **DATUM** and execution-related dates (including **EXEDATE** when you use it on selection) define the primary calendar window the message selection service receives. When those explicit date inputs are **not provided** or left open in the way the framework treats as empty, **BACKDAYS** acts as the **fallback** that still builds a lower bound from the evaluation date so the read does not scan unbounded history. After rows return, **DURATION** together with **DURATION_UNIT** forms an **additional filter** (an age-style test on how long messages have been in their current processing context relative to the evaluation clock). Operators should assume **both** the **date** side (explicit window or back-day–driven default) **and** the **duration** side must be satisfied before rows contribute to alerting, alongside message state, type, error family, and sender or receiver selectors that further narrow the set.

Party, namespace, interface, and logical system parameters work in parallel: tightening one dimension without adjusting related dimensions can unintentionally hide legitimate failures or amplify noise. State color and icon selections should stay consistent with the underlying message state values you intend to highlight so triage teams do not see contradictory signals between dimensions.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code (one calendar day of lookback applied when building the default lower date bound for message selection).
- **DURATION** - initial - treated as unset by code (the duration interval filter does not remove rows until a populated duration range is supplied on selection).
- **DURATION_UNIT** - initial - treated as M by code (minutes preset on the duration unit variable before selection values are read).

**Note:** The code seeds default lookback and unit values before reading the selection table so that duration math and date lower bounds remain defined even when operators leave some inputs blank.

### Practical Example of Parameter Configuration

**Use Case 1: Interface owner daily slice**

**Purpose:** Watch one outbound interface namespace with a short lookback and visible severity.

```
BACKDAYS = 2
OB_NS = urn:sap-com:document
OB_NAME = CustomerNotify_Out
STATE_COLOR = R
MSGTYPE = E
MANAGE_IN_UTC = X
```

**Use Case 2: Receiver-side backlog**

**Purpose:** Focus on a receiver logical system with execution window and duration guard.

```
IB_SYSTEM = PRD_ERP
EXEDATE = 20260101-20260115
DURATION = 180
DURATION_UNIT = M
MSGSTATE = 029
ERRCAT = MAPPING
CLIENT = 100
```

**Use Case 3: Cross-dimension health check**

**Purpose:** Broader discovery across parties and queues while still bounding time and age.

```
OB_PARTY = CUSTOMER_A
IB_PARTY = INTERNAL_BILL
QUEUEINT = XBTPR*
DURATION = 360
DURATION_UNIT = M
BACKDAYS = 5
STATUS = R
SW_DEST = CLOUD_MON
MSGTXT = timeout
ERRCODE = MAPPING_ERROR
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_02_XI_MESS | ACK_STATE | XI: Acknowledgment Status | NUMC(3) | SXMSPACKSTAT |
| /SKN/S_SW_01_02_XI_MESS | ADAPT_STAT | Integration Engine: Adapter Status | NUMC(3) | SXMSPASTAT |
| /SKN/S_SW_01_02_XI_MESS | ADAPT_TP_I | Pipeline Service Type | CHAR(10) | SXMSPSTYPE |
| /SKN/S_SW_01_02_XI_MESS | ADAPT_TYP | Pipeline Service Type | CHAR(10) | SXMSPSTYPE |
| /SKN/S_SW_01_02_XI_MESS | ADMINUSER | User Name | CHAR(12) | SYUNAME |
| /SKN/S_SW_01_02_XI_MESS | CHILDCOUNT | Numc3, internal use | NUMC(3) | NUMC3 |
| /SKN/S_SW_01_02_XI_MESS | CLIENT | Client ID | CLNT(3) | SYMANDT |
| /SKN/S_SW_01_02_XI_MESS | COMMITACT | XMS: Flag (true/false) | CHAR(1) | SXMSFLAG |
| /SKN/S_SW_01_02_XI_MESS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_02_XI_MESS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_02_XI_MESS | EO_REFID | Integration Engine: Reference Type | CHAR(8) | SXMSREFID |
| /SKN/S_SW_01_02_XI_MESS | EO_REFID_I | Integration Engine: Reference Type | CHAR(8) | SXMSREFID |
| /SKN/S_SW_01_02_XI_MESS | EO_REFID_O | Integration Engine: Reference Type | CHAR(8) | SXMSREFID |
| /SKN/S_SW_01_02_XI_MESS | EO_REFVAL | Integration Engine: Reference Value | CHAR(32) | SXMSREFVAL |
| /SKN/S_SW_01_02_XI_MESS | EO_REFVL_I | Integration Engine: Reference Value | CHAR(32) | SXMSREFVAL |
| /SKN/S_SW_01_02_XI_MESS | EO_REFVL_O | Integration Engine: Reference Value | CHAR(32) | SXMSREFVAL |
| /SKN/S_SW_01_02_XI_MESS | ERRCAT | XI: Error Category | CHAR(20) | SXMSERRCAT |
| /SKN/S_SW_01_02_XI_MESS | ERRCODE | XI: Error ID | CHAR(30) | SXMSERRID |
| /SKN/S_SW_01_02_XI_MESS | EXETIMEST | UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun) | DEC(21,7) | TIMESTAMPL |
| /SKN/S_SW_01_02_XI_MESS | EXE_DATE | System Date | DATS(8) | SYDATUM |
| /SKN/S_SW_01_02_XI_MESS | EXE_TIME | System Time | TIMS(6) | SYUZEIT |
| /SKN/S_SW_01_02_XI_MESS | GEN_ENTRY | XMS: Flag (true/false) | CHAR(1) | SXMSFLAG |
| /SKN/S_SW_01_02_XI_MESS | IB_NAME | Inbound Interface Name | CHAR(120) | RM_IIFNAME |
| /SKN/S_SW_01_02_XI_MESS | IB_NS | Inbound Interface Namespace | CHAR(255) | RM_IIFNS |
| /SKN/S_SW_01_02_XI_MESS | IB_PARTY | XI: Communication Party | CHAR(60) | SXI_PARTY |
| /SKN/S_SW_01_02_XI_MESS | IB_PARTY_AGENCY | XI Partner: Agency | CHAR(120) | SXI_PARTY_AGENCY |
| /SKN/S_SW_01_02_XI_MESS | IB_PARTY_TYPE | XI Partner: Identification Schema | CHAR(120) | SXI_PARTY_TYPE |
| /SKN/S_SW_01_02_XI_MESS | IB_SYSTEM | Receiving System | CHAR(60) | AIT_RCVR |
| /SKN/S_SW_01_02_XI_MESS | INITTIMEST | UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun) | DEC(21,7) | TIMESTAMPL |
| /SKN/S_SW_01_02_XI_MESS | INIT_DATE | System Date | DATS(8) | SYDATUM |
| /SKN/S_SW_01_02_XI_MESS | INIT_TIME | System Time | TIMS(6) | SYUZEIT |
| /SKN/S_SW_01_02_XI_MESS | ITFACTION | Integration Engine: Action for Interface | CHAR(4) | SXMSITFACT |
| /SKN/S_SW_01_02_XI_MESS | JOB_ID | Job ID | CHAR(32) | SXMSJOB |
| /SKN/S_SW_01_02_XI_MESS | MANUALSTAT | XMS: Flag (true/false) | CHAR(1) | SXMSFLAG |
| /SKN/S_SW_01_02_XI_MESS | MSGGUID | XI: Message ID | RAW(16) | SXMSMGUID |
| /SKN/S_SW_01_02_XI_MESS | MSGSTATE | Integration Engine: Message Status | NUMC(3) | SXMSPMSTAT |
| /SKN/S_SW_01_02_XI_MESS | MSGTXT | Exchange Infrastructure: Message Status Description | CHAR(60) | SXMSPMSTATTX |
| /SKN/S_SW_01_02_XI_MESS | MSGTYPE | Integration Engine: Message Type | CHAR(1) | SXMSPMTYPE |
| /SKN/S_SW_01_02_XI_MESS | OB_NAME | Outbound Interface Name | CHAR(120) | RM_OIFNAME |
| /SKN/S_SW_01_02_XI_MESS | OB_NS | Outbound Interface Namespace | CHAR(255) | RM_OIFNS |
| /SKN/S_SW_01_02_XI_MESS | OB_PARTY | XI: Communication Party | CHAR(60) | SXI_PARTY |
| /SKN/S_SW_01_02_XI_MESS | OB_PARTY_AGENCY | XI Partner: Agency | CHAR(120) | SXI_PARTY_AGENCY |
| /SKN/S_SW_01_02_XI_MESS | OB_PARTY_TYPE | XI Partner: Identification Schema | CHAR(120) | SXI_PARTY_TYPE |
| /SKN/S_SW_01_02_XI_MESS | OB_SYSTEM | Sending System | CHAR(60) | AIT_SNDR |
| /SKN/S_SW_01_02_XI_MESS | PARENTMSG | XI: Message ID | RAW(16) | SXMSMGUID |
| /SKN/S_SW_01_02_XI_MESS | PID | Integration Engine: Pipeline ID | CHAR(40) | SXMSPID |
| /SKN/S_SW_01_02_XI_MESS | QUEUEGUID | ID for Message Queue | CHAR(32) | SXMSQID |
| /SKN/S_SW_01_02_XI_MESS | QUEUEINT | Name of tRFC Queue | CHAR(24) | TRFCQNAM |
| /SKN/S_SW_01_02_XI_MESS | REF_TO_MSG | XI: Message ID | RAW(16) | SXMSMGUID |
| /SKN/S_SW_01_02_XI_MESS | REF_TO_REC_MSG | XI: Message ID | RAW(16) | SXMSMGUID |
| /SKN/S_SW_01_02_XI_MESS | REORG | Reorganization Status | CHAR(3) | SXMSREORG |
| /SKN/S_SW_01_02_XI_MESS | REST_VERS | Sequence Number for a Message Log ID | NUMC(3) | SXMSLSQNBR |
| /SKN/S_SW_01_02_XI_MESS | RETRYCOUNT | Numc3, internal use | NUMC(3) | NUMC3 |
| /SKN/S_SW_01_02_XI_MESS | RETRYCOUNT_MAN | Manual Retry Counter | INT4(10) | SXMS_MAN_RETRYCO |
| /SKN/S_SW_01_02_XI_MESS | RND_NUMBER | Random Number for Integration Engine Messages | NUMC(2) | SXMSRNDNUM |
| /SKN/S_SW_01_02_XI_MESS | SECURITY | Security Flag | NUMC(1) | SXMSSECURITY |
| /SKN/S_SW_01_02_XI_MESS | SENDTIMEST | UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun) | DEC(21,7) | TIMESTAMPL |
| /SKN/S_SW_01_02_XI_MESS | SEND_DATE | System Date | DATS(8) | SYDATUM |
| /SKN/S_SW_01_02_XI_MESS | SEND_TIME | System Time | TIMS(6) | SYUZEIT |
| /SKN/S_SW_01_02_XI_MESS | SNDRGUID | Sender ID | RAW(16) | SXMSSNDRID |
| /SKN/S_SW_01_02_XI_MESS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_02_XI_MESS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_02_XI_MESS | STATUS | Job Processing Status | CHAR(1) | SXMSMSGREFSTATUS |
| /SKN/S_SW_01_02_XI_MESS | VERS | Sequence Number for a Message Log ID | NUMC(3) | SXMSLSQNBR |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_02_XI_MESS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_XI_MESS OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_EXEDATE FOR SY-DATUM ,
         R_EXETIME FOR SY-UZEIT,
         R_ERRCAT  FOR /SKN/S_SW_SYS_XI_MESS-ERRCAT,
         R_ERRCODE FOR /SKN/S_SW_SYS_XI_MESS-ERRCODE,
         R_MSGTYPE FOR /SKN/S_SW_SYS_XI_MESS-MSGTYPE,
         R_MSGSTATE FOR /SKN/S_SW_SYS_XI_MESS-MSGSTATE ,
         R_OB_SYSTEM FOR /SKN/S_SW_SYS_XI_MESS-OB_SYSTEM,
         R_OB_NS     FOR /SKN/S_SW_SYS_XI_MESS-OB_NS,
         R_OB_NAME   FOR /SKN/S_SW_SYS_XI_MESS-OB_NAME,
         R_STATE_COLOR FOR /SKN/S_SW_SYS_IDOCS,
         R_DATUM   FOR SY-DATUM .
DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             BACKDAYS INT4,
             MANAGE_IN_UTC  CHAR1.
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
DATA :   IS_GENERAL(1) TYPE C.
DATA : DATE_FROM LIKE SY-DATUM ,
       DATE_TO LIKE SY-DATUM ,
       TIME_FROM LIKE SY-UZEIT ,
       TIME_TO LIKE SY-UZEIT ,
       BACKDAYS  TYPE I .
DATA : LANGU LIKE SY-LANGU .
DATA : SY_TABIX LIKE SY-TABIX .
DATA : EX_MSGTAB  TYPE SXMSMSGTAB.
DATA : L_EX_MSGTAB  LIKE LINE OF EX_MSGTAB.
DATA : IS_OUT(1) TYPE C.
DATA : LT_MSGSTATE_TAB TYPE  SXMSPMSTAT_TAB,
        LS_MSGSTATE_TAB LIKE LINE OF LT_MSGSTATE_TAB.
DATA : LV_MSGSTATE TYPE SXMSPMSTAT.
DATA : LS_MSGSTATE TYPE SXMSMSTAT,
        LT_MSGSTATE LIKE TABLE OF LS_MSGSTATE.
*-- Fill Selection Option Tables
  SELECT_MULTY: DURATION.
  LV_DURATION_UNIT = 'M'.
  LV_BACKDAYS = 1.
  SELECT_SINGLE: DURATION_UNIT,
                 BACKDAYS,
                 MANAGE_IN_UTC.
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
   LOOP AT T_SELECT WHERE FIELDNM = 'EXEDATE'.
     MOVE-CORRESPONDING T_SELECT TO R_EXEDATE.
     APPEND R_EXEDATE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'EXETIME'.
     MOVE-CORRESPONDING T_SELECT TO R_EXETIME.
     APPEND R_EXETIME.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ERRCAT'.
     MOVE-CORRESPONDING T_SELECT TO R_ERRCAT.
     APPEND R_ERRCAT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ERRCODE'.
     MOVE-CORRESPONDING T_SELECT TO R_ERRCODE.
     APPEND R_ERRCODE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'MSGTYPE'.
     MOVE-CORRESPONDING T_SELECT TO R_MSGTYPE.
     APPEND R_MSGTYPE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'MSGSTATE'.
     MOVE-CORRESPONDING T_SELECT TO R_MSGSTATE.
     APPEND R_MSGSTATE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'OB_SYSTEM'.
     MOVE-CORRESPONDING T_SELECT TO R_OB_SYSTEM.
     APPEND R_OB_SYSTEM.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'OB_NS'.
     MOVE-CORRESPONDING T_SELECT TO R_OB_NS.
     APPEND R_OB_NS.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'OB_NAME'.
     MOVE-CORRESPONDING T_SELECT TO R_OB_NAME.
     APPEND R_OB_NAME.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
     MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
     APPEND R_STATE_COLOR.
   ENDLOOP.
*
   LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
     MOVE-CORRESPONDING T_SELECT TO R_DATUM.
     APPEND R_DATUM.
   ENDLOOP.
   DATE_TO = SY-DATUM.
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
   IF R_EXEDATE[] IS INITIAL .
     R_EXEDATE[] = R_DATUM[].
   ENDIF.
"------------------------
  READ TABLE R_EXEDATE INDEX 1.
  DATE_FROM = R_EXEDATE-LOW.
  LOOP AT R_EXEDATE .
    IF DATE_FROM > R_EXEDATE-LOW.
      DATE_FROM = R_EXEDATE-LOW.
    ENDIF.
    IF DATE_TO < R_EXEDATE-HIGH.
      DATE_TO = R_EXEDATE-HIGH.
    ENDIF.
  ENDLOOP.
  IF DATE_TO IS INITIAL.
    DATE_TO = DATE_FROM.
  ENDIF.
"------------------------
  "-----
  LANGU = SY-LANGU.
  LOOP AT T_SELECT WHERE FIELDNM = 'LANGU'.
    LANGU = T_SELECT-LOW.
    EXIT.
  ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  TIME_FROM = '000000'.
  TIME_TO = '235959'.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_02_XI_MESS'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
*--- Message State Prepare
   REFRESH LT_MSGSTATE_TAB.
   IF R_MSGSTATE[] IS NOT INITIAL.
    "--- Fill Possible States
     SELECT * FROM SXMSMSTAT
       INTO CORRESPONDING FIELDS OF TABLE LT_MSGSTATE
       WHERE MSGSTATE IN R_MSGSTATE.
     LOOP AT LT_MSGSTATE INTO LS_MSGSTATE.
       LS_MSGSTATE_TAB = LS_MSGSTATE-MSGSTATE.
       APPEND LS_MSGSTATE_TAB TO LT_MSGSTATE_TAB.
     ENDLOOP.
   ENDIF.
  CALL FUNCTION 'SXMB_SELECT_MESSAGES'
    EXPORTING
*     IM_SENDDATE                =
*     IM_SENDTIME                =
*     IM_SND2DATE                =
*     IM_SND2TIME                =
      IM_EXEDATE                 = DATE_FROM
      IM_EXETIME                 = TIME_FROM
      IM_EXE2DATE                = DATE_TO
      IM_EXE2TIME                = TIME_TO
*     IM_PID                     =
*     IM_PIDS                    =
*     IM_CLIENT                  =
*     IM_USER                    =
*     IM_ADAPTER_TYPE_IN         =
*     IM_ADAPTER_TYPE            =
*     IM_ADAPTER_STATE           =
*     IM_S_SENDER_RECEIVER       =
*     IM_ERRCAT                  =
*     IM_ERRCODE                 =
*     IM_MSGGUID_TAB             =
      IM_MSGSTATE_TAB            = LT_MSGSTATE_TAB
*     IM_QUEUEID                 =
*     IM_MSGTYPE                 =
      IM_NUMBER                  = 9999
*     IM_ADAPTER_OR              = '0'
*     IM_PROCESS_MODE            = '0'
   IMPORTING
      EX_MSGTAB                  = EX_MSGTAB
*     EX_RESULT                  =
*     EX_FIRST_TS                =
    EXCEPTIONS
      PERSIST_ERROR              = 1
      MISSING_PARAMETER          = 2
      NEGATIVE_TIME_RANGE        = 3
      TOO_MANY_PARAMETERS        = 4
      OTHERS                     = 5         .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  REFRESH T_DATA.
  LOOP AT EX_MSGTAB INTO L_EX_MSGTAB.
    MOVE-CORRESPONDING L_EX_MSGTAB TO T_DATA .
    APPEND T_DATA.
  ENDLOOP .
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CLEAR IS_OUT .
    IF NOT T_DATA-ERRCAT IN R_ERRCAT . IS_OUT = 'X'. ENDIF.
    IF NOT T_DATA-ERRCODE IN R_ERRCODE . IS_OUT = 'X'. ENDIF.
    IF NOT T_DATA-MSGTYPE IN R_MSGTYPE . IS_OUT = 'X'. ENDIF.
    IF NOT T_DATA-MSGSTATE IN R_MSGSTATE . IS_OUT = 'X'. ENDIF.
    IF NOT T_DATA-OB_SYSTEM IN R_OB_SYSTEM . IS_OUT = 'X'. ENDIF.
    IF NOT T_DATA-OB_NS IN R_OB_NS . IS_OUT = 'X'. ENDIF.
    IF NOT T_DATA-OB_NAME IN R_OB_NAME . IS_OUT = 'X'. ENDIF.
    IF NOT IS_OUT IS INITIAL.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
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
    TIMESTAMP_TO_DATE_TIME_ADJUST T_DATA-EXETIMEST
                                  T_DATA-EXE_DATE T_DATA-EXE_TIME
                                  LV_MANAGE_IN_UTC.
    TIMESTAMP_TO_DATE_TIME_ADJUST T_DATA-INITTIMEST
                                  T_DATA-INIT_DATE T_DATA-INIT_TIME
                                  LV_MANAGE_IN_UTC.
    TIMESTAMP_TO_DATE_TIME_ADJUST T_DATA-SENDTIMEST
                                  T_DATA-SEND_DATE T_DATA-SEND_TIME
                                  LV_MANAGE_IN_UTC.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-EXE_DATE
          T_FROM            = T_DATA-EXE_TIME
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
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_01_02_XI_MESS_STAUS'
      EXPORTING
        MSGSTATE            = T_DATA-MSGSTATE
        LANGU               = LANGU
      IMPORTING
        MSGTXT              = T_DATA-MSGTXT
        ICON_ID             = T_DATA-STATE_ICON
      EXCEPTIONS
        WRONG_MESSAGE       = 1
        OTHERS              = 2    .
    IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
