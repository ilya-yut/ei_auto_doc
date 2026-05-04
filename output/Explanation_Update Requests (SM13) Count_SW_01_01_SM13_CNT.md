# Exception Indicator: Update Requests (SM13) Count - SW_01_01_SM13_CNT

## General Overview

This Exception Indicator summarizes how many SAP update requests currently match your SM13-style selection after the standard update administration population runs, and it signals when that quantity sits inside a band you treat as exceptional.

This EI serves as an essential control for application operations and Basis governance by:
- Giving leadership a single numeric signal when update queues swell beyond agreed tolerance instead of scrolling long line lists
- Supporting escalation during closing peaks or after transports when sudden growth in pending work threatens downstream posting
- Enabling trend comparisons of “how many problematic lines exist right now” between systems, clients, or functional slices you already trust
- Helping audit teams evidence that queue health was checked on a recurring basis without exporting entire SM13 extracts
- Reducing false alarms by pairing the headline count with the same filters your administrators already use for triage

Typical use includes hourly operations dashboards, release validation windows, and executive summaries that need one number rather than raw tables. Teams still open standard update administration when the count warrants drill-down.

The EI reuses the same update header population used for detailed SM13 monitoring, then evaluates how many lines matched before deciding whether to flag the run.


## Problem Description

Relying only on detailed SM13-style line lists makes it easy to miss systemic queue pressure until transactions begin to fail or batch chains stall:

**Operational Stability Issues**
- Administrators spend cycles scanning hundreds of benign lines while a modest count spike that actually threatens throughput goes unnoticed
- Executive stakeholders lack a compact indicator they can track day over day during volatile maintenance periods
- Cross-client noise in raw extracts obscures whether a single productive client suddenly carries most of the pending work

**Data Consistency and Processing Risks**
- Downstream objects may already reflect user confirmations even while update execution lags, increasing reconciliation effort when nobody noticed the growing backlog early
- Without a thresholded view, teams cannot distinguish “busy but normal” from “busy and breaching policy” using the same filters they trust for triage

**Governance and Visibility Risks**
- Internal control narratives struggle to cite objective evidence when only anecdotal SM13 screenshots exist
- Post-incident reviews miss quantitative baselines that would show when the population first crossed an agreed danger band

## Suggested Resolution

**Immediate Response**
- When the indicator fires, open update administration with the same selection to confirm whether the elevated count still exists and which users, programs, or return codes dominate the population
- Capture the measured count, time of run, and owning application area so remediation can be prioritized against business deadlines
- If the spike ties to a known deployment or batch campaign, coordinate with the responsible team before forcing retries or cancellations

**System Assessment**
- Compare the current count with prior runs that used identical filters to see whether growth is gradual or step-changed after a specific change window
- Segment the underlying detail view by client, transaction, and return code to learn whether one driver explains the headline number
- Validate that cloud or remote destination selections still point at the systems you intend to include in the tally

**Corrective Actions**
- Resolve application errors, repeat failed updates, or clear obsolete entries according to SAP update administration guidance, then re-run the monitor to confirm the count returned inside the safe band
- Tighten or widen the count threshold only after reviewing false-positive and false-negative history with both Basis and functional owners
- Fold training for key users into existing operational playbooks so they know when a numeric alert requires immediate SM13 review versus informational tracking
- Document outcomes for audit when regulated or financially material processes were affected by delayed updates


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 2 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 3 | DURATION_UNIT | Duration Unit(D/H/M) |  | 0 | 0 |  |  |
| 4 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 5 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 6 | SW_DEST | Cloud Destination |  | 0 | 0 |  |  |
| 7 | UPD_REQ_CNT | Count | INT4 | 10 | 0 | /SKN/E_SW_CNT |  |
| 8 | VBCLIINFO | Bytes | RAW | 1 | 0 | THRAW1 | THRAW1 |
| 9 | VBCLINAME | Update Server Name | CHAR | 64 | 0 | VBNAME | VBNAME |
| 10 | VBCONTEXT | Update context | CHAR | 20 | 0 | VBCONTEXT | VBCONTEXT |
| 11 | VBLANG | Logon Language | LANG | 1 | 0 | XULANGU | LANG |
| 12 | VBMANDT | Client | CLNT | 3 | 0 | VBMANDT | MANDT |
| 13 | VBNAME | Update Server Name | CHAR | 64 | 0 | VBNAME | VBNAME |
| 14 | VBRC | Update return code | INT4 | 10 | 0 | VBRC | VBRC |
| 15 | VBREPORT | Generating program | CHAR | 40 | 0 | VBREPORT | WPREPORT |
| 16 | VBSTATE | Status | INT1 | 3 | 0 | VBSTATE | INT1 |
| 17 | VBTCODE | TCODE | CHAR | 20 | 0 | VBTCODE | VBTCODE |
| 18 | VBUSR | User | CHAR | 12 | 0 | VBBNAME | UBNAME |
| 19 | VBZONLO | Local time zone | CHAR | 6 | 0 | VBZONLO | VBZONLO |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 19 parameters listed in the Parameters Reference Table when tuning this EI; each shapes which update requests participate in the population pass before the final row tally is compared to your count band.

**BACKDAYS** (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on VBDATE

**DURATION** (Duration In Time Units)

After the date window from BACKDAYS is applied, DURATION supplies the numeric width for the secondary age check measured in the unit given by DURATION_UNIT, so only update lines whose scheduling age still fits the declared span remain eligible for counting.

**DURATION_UNIT** (Duration Unit(D/H/M))

Selects the calendar meaning applied to DURATION when the monitor evaluates how long each update request has been pending relative to the run timestamp.

**DURATION_UNIT Options:**
- **H** — Hours between the update scheduling timestamp and the evaluation moment.
- **M** — Minutes for short-interval operations reviews.
- **D** — Whole-day style spacing when you reason in business days rather than clock minutes.
- **F** — Full-day counting style used by the duration helper when the template expects day-bucket alignment instead of raw clock math.

**MANAGE_IN_UTC** ('X' - Manage in UTC)

Chooses whether duration math and cross-system comparisons normalize timestamps to UTC or stay on the application server’s local clock context.

**MANAGE_IN_UTC Options:**
- **X** — Compare pending times in UTC so distributed landscapes share one clock reference.
- **Empty or blank** — Keep local application-server time for operators who work entirely in one time zone.

**STATE_COLOR** (State Color)

Maps each surviving update line into the monitor’s red / yellow / green style buckets so you can require only the severity band that should influence the count going into the threshold test.

**STATE_COLOR Options:**
- **R** — Error-style or failed processing bucket.
- **G** — Successful or cleared bucket.
- **Y** — Warning or in-flight bucket.
- Additional literals may exist where the framework extends the palette for neutral states.

**SW_DEST** (Cloud Destination)

Routes the underlying update read through the declared cloud or RFC-capable destination when your template evaluates a central hub instead of the local instance only.

**UPD_REQ_CNT** (Count)

Declares the inclusive count window for the number of update-request lines returned after the population pass; the monitor raises its alert only when that line count lies inside UPD_REQ_CNT exactly as you configured the multivalued selection.

**UPD_REQ_CNT Options:**
- **Single value** — Fire only when the population contains exactly that many lines.
- **Low - high** — Fire when the population size falls between the bounds, inclusive.
- **Open upper bound** — Use the framework’s open-ended pattern when any count at or above a floor should qualify.

**VBCLIINFO** (Bytes)

Holds optional raw client logon metadata bytes; when restricted, it keeps the population aligned with diagnostics that already identified a specific client fingerprint.

**VBCLINAME** (Update Server Name)

Filters on the update-server name resolved from the client logon metadata pair, which is useful when you split traffic across named update gateways and need the count scoped to one gateway’s view.

**VBCONTEXT** (Update context)

Isolates update work tagged with a particular VBCONTEXT value so batch versus dialog versus inbound-idoc contexts do not inflate each other’s counts.

**VBLANG** (Logon Language)

Restricts the population to update records created under a specific logon language, which matters when language-specific posting paths generate different queue volumes.

**VBMANDT** (Client)

Pins the population to one or more productive or regression clients before the tally runs, preventing cross-client noise from masking a spike in a single client.

**VBNAME** (Update Server Name)

Targets the update-server name stored on the update header itself (distinct from the client-resolved name), which is the field operators usually recognize in SM13-style listings tied to the VBHDR scheduler.

**VBRC** (Update return code)

Limits counting to lines whose SAP update return code still matches your declared band, so benign informational codes do not dilute alerts aimed at hard failures.

**VBREPORT** (Generating program)

Narrows the population to updates spawned by specific generating programs, which is how functional teams watch a single custom include or report driver.

**VBSTATE** (Status)

Keeps only update lines whose numeric processing state matches the configured selection, letting you alert on paused versus active versus finished buckets independently.

**VBTCODE** (TCODE)

Scopes the tally to updates that originated from named transactions, which is ideal when month-end closing transactions are the only ones you want reflected in the count.

**VBUSR** (User)

Restricts the population to updates owned by particular batch or dialog users before the framework compares the resulting quantity to UPD_REQ_CNT.

**VBZONLO** (Local time zone)

Aligns interpretation of local timestamps with the update owner’s recorded time-zone token so duration and BACKDAYS comparisons stay consistent for globally distributed user populations.


### Parameter Relationships

How parameter combinations work together

**BACKDAYS**, **DURATION**, and **DURATION_UNIT** work as successive narrowing steps on the same update population: the historical window comes first, then the age-of-line test removes work that is either too fresh or too stale for the scenario you are counting.

**MANAGE_IN_UTC** should stay aligned with how your team reads timestamps in SM13; mixing UTC-normalized duration math with locally interpreted BACKDAYS expectations can make the same physical queue look larger or smaller than operators expect.

**STATE_COLOR** and the detailed VB* selectors (client, user, transaction, return code, program, status, server names, context, language, time zone) should be configured together so the headline **UPD_REQ_CNT** test measures the same logical slice your administrators would defend in an incident review.

**SW_DEST** must be populated when the count should reflect a remote hub; otherwise the tally may describe only the local instance while management believes it covers the full landscape.

**UPD_REQ_CNT** does not replace those filters—it only decides whether the quantity of rows that survived them is itself exceptional, which is why tightening VB filters without revisiting the count band often creates unwanted churn or silence.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code (1 calendar day back from evaluation date; upper bound open).
- **DURATION** - initial - treated as empty by code (no duration range; age filter off).
- **DURATION_UNIT** - initial - treated as M by code (minutes).

### Practical Example of Parameter Configuration

**Use Case 1: Production client spike watch**

**Purpose:** Catch when more than a handful of update lines simultaneously match a tight productive-client slice, using full-day duration alignment for the age leg.

```
VBMANDT = 100
BACKDAYS = 2
DURATION = 1
DURATION_UNIT = F
UPD_REQ_CNT = 10 - 999999
```

**Use Case 2: Batch user backlog**

**Purpose:** Monitor a known integration user and require a minimum queue size before paging overnight operations.

```
VBUSR = RFC_BATCH_01
VBTCODE = ME21N
UPD_REQ_CNT = 25 - 999999
MANAGE_IN_UTC = X
```

**Use Case 3: Error-colored short window**

**Purpose:** Highlight bursts of error-bucket traffic within a short clock window for war-room visibility.

```
STATE_COLOR = R
DURATION = 45
DURATION_UNIT = M
VBRC = 1 - 9999
```

**Use Case 4: Program-specific cloud hub**

**Purpose:** Count updates spawned by a custom driver through a named cloud destination while keeping language-specific noise out.

```
SW_DEST = MY_CLOUD_CONN
VBREPORT = ZUPDATE_DRIVER
VBLANG = E
UPD_REQ_CNT = 1 - 5
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_SM13_CNT | UPD_REQ_CNT | SW: Count | INT4(10) | /SKN/E_SW_CNT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_01_01_SM13_CNT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SM13_CNT OPTIONAL
*"----------------------------------------------------------------------
DATA : LV_ALERT TYPE  CHAR1.
DATA : LS_DATA TYPE /SKN/S_SW_01_01_SM13,
       LT_DATA LIKE TABLE OF LS_DATA.
DATA : LV_CNT TYPE I.
DATA_MULTY: UPD_REQ_CNT /SKN/E_SW_CNT.
SELECT_MULTY: UPD_REQ_CNT.
   REFRESH T_DATA.
   CALL FUNCTION '/SKN/F_SW_01_01_SM13'
    IMPORTING
       IS_ALERT       = LV_ALERT
     TABLES
       T_SELECT       = T_SELECT
       T_DATA         = LT_DATA.
    IS_ALERT = LV_ALERT.
    DESCRIBE TABLE LT_DATA LINES LV_CNT.
    IF LV_CNT IN R_UPD_REQ_CNT.
      T_DATA-UPD_REQ_CNT = LV_CNT.
      APPEND T_DATA.
      IS_ALERT = LV_ALERT.
    ELSE.
      CLEAR IS_ALERT.
    ENDIF.
ENDFUNCTION.
```
