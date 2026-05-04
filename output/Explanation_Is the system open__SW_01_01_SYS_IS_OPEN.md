# Exception Indicator: Is the system open? - SW_01_01_SYS_IS_OPEN

## General Overview

This Exception Indicator reviews SAP client definitions to highlight situations where changeability settings indicate that the system is still open to repository or cross-client customizing activity when policy expects a locked-down landscape.

This EI serves as an essential control for Basis governance and audit readiness by:
- Surfacing clients whose cross-client maintenance indicators show fully open change paths when leadership expects hardened controls
- Giving security and change-management forums objective evidence of which clients remain permissive for customizing or repository edits
- Supporting release and hypercare reviews when transports should be frozen but client tables still allow broad maintenance
- Helping auditors tie exception alerts to the same client attributes they validate in standard client administration
- Enabling operations teams to prioritize follow-up when monitoring shows concentration of permissive clients in productive numbers

Typical use includes monthly control self-assessments, pre-freeze checks before financial close, and post-migration validation when client copies might reset indicators. Teams act on results in standard client maintenance, then document remediation or parameter tuning.

The routine reads the central client directory table that stores each client’s role and changeability flags.


## Problem Description

Failure to monitor how open each SAP client is to repository and cross-client customizing changes creates multiple risks across operational stability, security posture, and compliance evidence:

**Operational Stability and Change-Control Risks**
- Transport freezes and release milestones lose meaning when permissive clients remain editable without anyone noticing until downstream posting errors appear
- Parallel development streams collide more often when multiple teams can still change cross-client objects during the same window
- Emergency fixes take longer to scope when nobody can quickly see which clients still allow unrestricted customizing paths
- Client-copy or restore activities can silently revert hardened settings, leaving production-looking numbers with test-style openness

**Security and Integrity Risks**
- Attackers or insiders with broad authorizations gain more leverage when repository and customizing paths stay open longer than policy allows
- Segregation-of-duty reviews miss a material signal when permissive client indicators are not trended alongside user activity
- Evidence of “who changed what” becomes harder to interpret when the underlying client still permits classes of change leadership thought were blocked

**Management Visibility and Decision-Making Risks**
- Executives lack a concise exception list tying technical openness to business-critical clients during audits or M&A diligence
- Program and portfolio managers cannot prove that landscape hardening initiatives actually landed in each client’s control data
- Regional rollouts proceed unevenly when some company codes’ clients remain permissive without a single monitoring view

## Suggested Resolution

**Immediate Response**
- Treat each alert line as a client-level exception: confirm the numeric client, its name, and the narrative severity shown for the openness pattern
- Validate whether the client is meant to be a sandbox, quality, or training system before assuming a production defect
- Compare the reported pattern to the change calendar to see if a maintenance window explains temporary openness
- Capture accountable owners for follow-up when the client is business-critical or handles regulated data

**System Assessment**
- Review adjacent attributes such as role category, transport behavior, and cross-client maintenance flags together to understand the full posture of that client row
- Compare current results to the prior monitoring cycle after transports, upgrades, or client copies that might reset indicators
- Segment exceptions by company code name, city, or currency when many clients exist so leadership sees where concentration occurs
- Confirm that monitoring scope still matches the productive landscape so test clients do not drown out material production findings

**Corrective Actions**
- Use standard SAP client administration transactions to tighten changeability according to policy once business sign-off is obtained
- Re-run monitoring after corrections to prove indicators moved to the expected hardened state and alerts cleared
- Update operational runbooks so help desks know how to interpret each openness narrative and which functional area to engage
- Document remediation packages for auditors, including before/after screenshots or extracts from client maintenance
- Schedule recurring checks after template releases or landscape refreshes so new permissive patterns are caught within the first business day


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ADRNR | Character Field Length = 10 | CHAR | 10 | 0 | CHAR10 |  |
| 2 | CCCATEGORY | Client role | CHAR | 1 | 0 | CCCATEGORY |  |
| 3 | CCCOPYLOCK | Protection reg. client copy/comparison | CHAR | 1 | 0 | CCCOPYLOCK |  |
| 4 | CCCORACTIV | Changes and transports | CHAR | 1 | 0 | CCCORACTIV |  |
| 5 | CCIMAILDIS | CATT Authorization | CHAR | 1 | 0 | CCIMAILDIS |  |
| 6 | CCNOCASCAD | Protection against SAP upgrade | CHAR | 1 | 0 | CCNOCASCAD |  |
| 7 | CCNOCLIIND | No cross-client maintenance | CHAR | 1 | 0 | CCNOCLIIND |  |
| 8 | CCORIGCONT | Create Switch BC Sets | CHAR | 1 | 0 | CCORIGCONT |  |
| 9 | CCSOFTLOCK | Lock Method | CHAR | 1 | 0 | CCSOFTLOCK |  |
| 10 | CCTEMPLOCK | Locked due to client copy | CHAR | 1 | 0 | CCTEMPLOCK |  |
| 11 | CHANGEDATE | Date | DATS | 8 | 0 | AS4DATE |  |
| 12 | CHANGEUSER | Last Changed By | CHAR | 12 | 0 | AS4USER |  |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 14 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT |  |
| 15 | LOGSYS | Logical System | CHAR | 10 | 0 | LOGSYS |  |
| 16 | MANDT | Client | CLNT | 3 | 0 | MANDT |  |
| 17 | MTEXT | Name | CHAR | 25 | 0 | MTEXT_D |  |
| 18 | MWAER | Standard currency | CUKY | 5 | 0 | MWAER |  |
| 19 | ORT01 | City | CHAR | 25 | 0 | ORT01 |  |
| 20 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR |  |
| 21 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON |  |
| 22 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT |  |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 22 parameters listed in the Parameters Reference Table when tuning this EI; each influences which client rows are retrieved, aged, and surfaced for alerting.

**ADRNR** (Character Field Length = 10)

ADRNR carries the address number reference stored on the client row so distributed teams can tie an exception back to the same address record they maintain in standard business partner workflows.

**CCCATEGORY** (Client role)

CCCATEGORY distinguishes production, test, demo, and training-style clients so monitoring can emphasize business-critical numbers while still listing exceptions elsewhere for completeness.

**CCCOPYLOCK** (Protection reg. client copy/comparison)

CCCOPYLOCK signals whether client copy utilities should treat the client as protected, helping change managers see when copy tooling could still run despite governance expectations.

**CCCORACTIV** (Changes and transports)

CCCORACTIV captures whether client-specific transports and changes are allowed, which is a primary lever auditors review alongside repository openness.

**CCIMAILDIS** (CATT Authorization)

CCIMAILDIS records how computer-aided test tools may run in the client, which matters when automated scripts could still execute while customizing paths look hardened.

**CCNOCASCAD** (Protection against SAP upgrade)

CCNOCASCAD shows upgrade-related cascade protections so upgrade managers can confirm the client matches their freeze strategy before go-live.

**CCNOCLIIND** (No cross-client maintenance)

CCNOCLIIND is the strongest single indicator of how far cross-client repository work may proceed; the monitoring logic maps its values to the plain-language severity narrative and traffic-light styling that operators see first.

**CCNOCLIIND Options:**
- Space – repository and cross-client customizing remain fully changeable, which triggers the most severe narrative path
- 1 – cross-client customizing objects are blocked while repository work may continue
- 2 – repository objects are blocked while some customizing paths may still differ
- 3 – both repository and cross-client customizing objects are blocked, representing the hardened posture leadership usually expects in production

**CCORIGCONT** (Create Switch BC Sets)

CCORIGCONT highlights which client records BC Set switches when solution documentation teams need to know where switch content is authored.

**CCSOFTLOCK** (Lock Method)

CCSOFTLOCK documents soft-lock expectations that Basis teams reconcile with enqueue behavior during maintenance windows.

**CCTEMPLOCK** (Locked due to client copy)

CCTEMPLOCK tells operators when a client is temporarily locked because a copy is running, preventing false escalations during long-running infrastructure jobs.

**CHANGEDATE** (Date)

CHANGEDATE stores the last time client control data changed, and the aging routine compares that calendar stamp to the evaluation moment so freshness rules can drop stale exceptions.

**CHANGEUSER** (Last Changed By)

CHANGEUSER identifies the account that last touched the client row, giving security teams a direct pointer for follow-up interviews after an alert fires.

**DURATION** (Duration In Time Units)

DURATION receives the computed age returned by the shared time-difference helper after each client row is evaluated, and only rows whose age still fits the declared interval remain in the alert set.

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT tells the aging helper whether to measure elapsed time in minutes, hours, days, or full-day buckets before comparing the result with the configured interval.

**DURATION_UNIT Options:**
- D – day-based increments between the last change date and the evaluation timestamp
- H – hour-level granularity for short lookback windows
- M – minute-level granularity when near-real-time freshness is required
- F – full-day treatment when business rules expect calendar-day floors instead of fractional days

**LOGSYS** (Logical System)

LOGSYS ties each client to its logical-system name so central monitoring can align exceptions with the same identifier used in ALE distribution lists.

**MANDT** (Client)

MANDT is the primary key for selection; supply the productive or non-productive numbers the runbook names so the extract matches the landscape segment under review.

**MTEXT** (Name)

MTEXT repeats the familiar client description text so dashboards remain human-readable without opening additional transactions.

**MWAER** (Standard currency)

MWAER shows the client currency so finance-facing reviewers can interpret openness exceptions alongside monetary reporting expectations.

**ORT01** (City)

ORT01 provides a geographic hint for distributed governance teams that organize follow-up by region.

**STATE_COLOR** (State Color)

STATE_COLOR is filled by the routine after interpreting cross-client maintenance indicators, giving alert consumers an at-a-glance severity token that downstream icon logic can reuse.

**STATE_COLOR Options:**
- R – indicates the narrative path where repository and cross-client customizing remain fully open
- Y – indicates partially restricted combinations where one class of change is blocked but others may remain
- G – indicates the hardened path where both repository and cross-client customizing objects are blocked
- Any other value leaves color styling untouched when the indicator does not map to a known posture

**STATE_ICON** (State Icon)

STATE_ICON stores the icon token returned by the shared state-icon routine so alert layouts can show the same glyph users already recognize from other monitors.

**STATUS_DESC** (SW Message)

STATUS_DESC carries the full sentence that explains the openness posture in business language, which is what email or ticketing integrations should surface to non-Basis readers.


### Parameter Relationships

How parameter combinations work together

Client-identifying attributes such as **MANDT**, **MTEXT**, **ORT01**, **MWAER**, and **LOGSYS** travel together as descriptive context so each openness exception is immediately recognizable to regional and finance stakeholders without opening another transaction.

The governance indicators **CCCATEGORY**, **CCCORACTIV**, **CCNOCLIIND**, **CCCOPYLOCK**, **CCNOCASCAD**, **CCORIGCONT**, **CCSOFTLOCK**, **CCTEMPLOCK**, and **CCIMAILDIS** describe different facets of the same client-control record; narrowing any of them in selection refines which clients enter the evaluation loop where the narrative severity is derived.

**CHANGEDATE** and **CHANGEUSER** describe when and who last edited that control record, while **DURATION** and **DURATION_UNIT** jointly express how long the row may remain in the alert set after its last change: the unit tells the aging helper how to measure elapsed time, and the numeric duration window decides whether the aged row survives the final retention test.

**STATE_COLOR**, **STATE_ICON**, and **STATUS_DESC** form a single presentation bundle produced after the openness logic runs, so operators should interpret them together rather than treating the color token in isolation from the message text.

**ADRNR** links the client row to address maintenance, which is useful when the openness alert triggers a master-data review that must include physical location attributes.


### Default Values

- **DURATION** - initial - treated as unconstrained multi-value selection by code (empty interval table keeps every computed age value until explicit bounds are supplied).
- **DURATION_UNIT** - D set in code before the single-value read completes by code (day-based aging math runs unless the caller overrides the unit afterward).

### Practical Example of Parameter Configuration

**Use Case 1: Production landscape hardening sweep**

**Purpose:** Focus on productive-role clients that still allow repository work, while requiring freshness within a single calendar-day window expressed in full-day units.

```
MANDT = 100–399
CCCATEGORY = P
CCNOCLIIND = ' '
DURATION_UNIT = F
DURATION = 1
```

**Use Case 2: Transport-free weekend watch**

**Purpose:** Highlight any client whose customizing path remains fully open immediately after a freeze announcement, using a short lookback without narrowing by duration window.

```
CCCORACTIV = '1'
CCNOCLIIND = ' '
```

**Use Case 3: Post-copy validation**

**Purpose:** Catch clients still flagged as temporarily locked or soft-locked after infrastructure work, while scoping to a named company cluster.

```
CCTEMPLOCK = X
CCSOFTLOCK = X
MTEXT = PRD*
```

**Use Case 4: Currency-specific governance sample**

**Purpose:** Give treasury-facing reviewers a slice of Euro-denominated productive clients whose cross-client indicator still shows partial restriction rather than full lock-down.

```
MWAER = EUR
CCNOCLIIND = 1
CCCATEGORY = P
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/CLIENT_STATUS | ADRNR | Character Field Length = 10 | CHAR(10) | CHAR10 |
| /SKN/CLIENT_STATUS | CCCATEGORY | Client control: Role of client (production, test,...) | CHAR(1) | CCCATEGORY |
| /SKN/CLIENT_STATUS | CCCOPYLOCK | Protection reg. client copy program and comparison tools | CHAR(1) | CCCOPYLOCK |
| /SKN/CLIENT_STATUS | CCCORACTIV | Changes and transports for client-specific objects | CHAR(1) | CCCORACTIV |
| /SKN/CLIENT_STATUS | CCIMAILDIS | Client Control: CATT und eCATT Authorization | CHAR(1) | CCIMAILDIS |
| /SKN/CLIENT_STATUS | CCNOCASCAD | Client control: No client cascade for upgrade import | CHAR(1) | CCNOCASCAD |
| /SKN/CLIENT_STATUS | CCNOCLIIND | Maintenance authorization for objects in all clients | CHAR(1) | CCNOCLIIND |
| /SKN/CLIENT_STATUS | CCORIGCONT | Recording Client for Switch BC Sets | CHAR(1) | CCORIGCONT |
| /SKN/CLIENT_STATUS | CCSOFTLOCK | Client control: Soft Lock Required (Planned for 4.0) | CHAR(1) | CCSOFTLOCK |
| /SKN/CLIENT_STATUS | CCTEMPLOCK | Client control: Indicator that client is temporarily locked | CHAR(1) | CCTEMPLOCK |
| /SKN/CLIENT_STATUS | CHANGEDATE | Date of Last Change | DATS(8) | AS4DATE |
| /SKN/CLIENT_STATUS | CHANGEUSER | Last Changed by | CHAR(12) | AS4USER |
| /SKN/CLIENT_STATUS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/CLIENT_STATUS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/CLIENT_STATUS | LOGSYS | Logical system | CHAR(10) | LOGSYS |
| /SKN/CLIENT_STATUS | MANDT | Client | CLNT(3) | MANDT |
| /SKN/CLIENT_STATUS | MTEXT | Client name | CHAR(25) | MTEXT_D |
| /SKN/CLIENT_STATUS | MWAER | Standard currency throughout client | CUKY(5) | MWAER |
| /SKN/CLIENT_STATUS | ORT01 | City | CHAR(25) | ORT01 |
| /SKN/CLIENT_STATUS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/CLIENT_STATUS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/CLIENT_STATUS | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_SYS_IS_OPENED.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/CLIENT_STATUS OPTIONAL
*"----------------------------------------------------------------------
  RANGES : R_MANDT    FOR /SKN/CLIENT_STATUS-MANDT,
           R_CCCATEGORY FOR /SKN/CLIENT_STATUS-CCCATEGORY,
           R_CCCORACTIV FOR /SKN/CLIENT_STATUS-CCCORACTIV,
           R_CCORIGCONT FOR /SKN/CLIENT_STATUS-CCORIGCONT.
DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
DATA: SY_TABIX LIKE SY-TABIX.
*-- Fill Selection Option Tables
 SELECT_MULTY: DURATION.
 LV_DURATION_UNIT = 'D'.
 SELECT_SINGLE: DURATION_UNIT.
 SY_DATLO = SY-DATUM .        " Appl Server's Date
 SY_TIMLO = SY-UZEIT.
"--- Run Clood Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_01_SYS_IS_OPENED'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Clood Mode -----
  LOOP AT T_SELECT WHERE FIELDNM = 'MANDT'.
    MOVE-CORRESPONDING T_SELECT TO R_MANDT.
    APPEND R_MANDT.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'CCCATEGORY'.
    MOVE-CORRESPONDING T_SELECT TO R_CCCATEGORY.
    APPEND R_CCCATEGORY.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'CCCORACTIV'.
    MOVE-CORRESPONDING T_SELECT TO R_CCCORACTIV.
    APPEND R_CCCORACTIV.
  ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'CCORIGCONT'.
    MOVE-CORRESPONDING T_SELECT TO R_CCORIGCONT.
    APPEND R_CCORIGCONT.
  ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  SELECT *
     FROM T000
     INTO CORRESPONDING FIELDS OF TABLE T_DATA
     WHERE MANDT IN  R_MANDT
       AND CCCATEGORY IN R_CCCATEGORY
       AND CCCORACTIV IN R_CCCORACTIV
       AND CCORIGCONT IN R_CCORIGCONT.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CASE T_DATA-CCNOCLIIND.
      WHEN ' '.
        T_DATA-STATUS_DESC = 'Changes to Repository and cross-client Customizing allowed'.
        T_DATA-STATE_COLOR = 'R'.
      WHEN '1'.
        T_DATA-STATUS_DESC = 'No changes to cross-client Customizing objects'.
        T_DATA-STATE_COLOR = 'Y'.
      WHEN '2'.
        T_DATA-STATUS_DESC = 'No changes to Repository objects'.
        T_DATA-STATE_COLOR = 'Y'.
      WHEN '3'.
        T_DATA-STATUS_DESC = 'No changes to Repository and cross-client Customizing objs'.
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
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-CHANGEDATE
          T_FROM            = SY_TIMLO
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
