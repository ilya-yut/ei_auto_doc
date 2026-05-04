# Exception Indicator: PUR: PUR PR  Approved by Creator - SW_10_03_PR_APPR_BY

## General Overview

This Exception Indicator (EI) monitors purchase requisitions that are subject to release and flags those where the same user who created the requisition also performed the release (approval). It combines requisition header data with change document data to compare creator and approver, supporting segregation of duties and release-control oversight in procurement.

This EI serves as an essential control for procurement and financial oversight by:
- Enabling detection of purchase requisitions approved by their creator, which may indicate segregation-of-duties violations or missing approval workflows
- Supporting identification of release strategy and release-status patterns by company code, purchasing organization, and vendor for control design review
- Providing visibility into the timing of creation and approval via configurable date reference and duration for prioritization and audit
- Enabling analysis of release groups, release codes, and processing status for exception management and policy enforcement
- Supporting month-end and audit readiness by surfacing creator-approver same-user exceptions that may require remediation or disclosure

Monitoring creator-approver separation helps organizations enforce segregation of duties in purchasing requisitions, reduce risk of unauthorized commitments, and prioritize follow-up on high-value or aged exceptions. The EI is particularly valuable for procurement controls, internal audit, and compliance reviews.

The EI uses purchase requisition data (EBAN), change document header and item data (CDHDR, CDPOS) for release status (FRGZU), and release and description lookups to determine release status and compare creator with approver.


## Problem Description

Failure to monitor purchase requisitions approved by their creator creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unmonitored creator-approver same-user cases can indicate weak release controls affecting commitment accuracy and period-end accruals
- Purchase requisitions released by the creator without independent approval may lead to unauthorized commitments or duplicate orders if discovered late
- Lack of visibility into release timing and value at release can delay identification of control gaps affecting financial statement assertions
- Concentrated exceptions in specific company codes or purchasing organizations may signal systemic release-control weaknesses

**Procurement and Control Risks**
- Same user creating and approving requisitions violates segregation of duties and may enable fraud or error in the procure-to-pay process
- Unidentified release strategy or release-code patterns can mask missing or misconfigured approval workflows
- Absence of monitoring by vendor, plant, or document type limits ability to enforce approval policies and delegation rules
- High volume of creator-approver exceptions in specific organizational units may reflect process or training issues

**Management Visibility and Decision-Making Risks**
- Lack of consolidated visibility delays management awareness of segregation-of-duties exceptions requiring intervention
- Unmonitored release patterns by organizational dimension limit ability to assign accountability and optimize controls
- Missing link between change document approver and requisition creator hinders root-cause analysis and corrective action
- Absence of duration-based prioritization (e.g. time since creation or release) limits efficient allocation of review resources

## Suggested Resolution

**Immediate Response**
- Review the purchase requisitions flagged by the EI to confirm creator and approver user IDs and the release status and value at release
- Verify high-value or high-risk requisitions using the appropriate display transaction (e.g. ME53N for requisition) to confirm legitimacy and whether approval was appropriate
- Check release strategy and release code configuration for the affected document types and organizational units
- Identify business context: delegated approval, emergency procedures, or missing workflow configuration

**System Assessment**
- Analyze the monitoring window and date reference used for duration calculation to ensure the scope aligns with the control objective
- Compare exception volume and patterns to prior periods and to expected activity by purchasing organization, vendor, and document type
- Examine release group, release strategy, and processing status distribution to detect misconfiguration or policy gaps
- Validate that filters (requisition number, vendor, document type, release group) match the intended control scope

**Corrective Actions**
- Where segregation-of-duties violations are confirmed, escalate to procurement and management; reinforce approval workflows or delegation rules
- Update release strategy or release configuration (e.g. approval limits, release codes) where policy or design gaps are identified
- Adjust master data or organizational assignment where exceptions indicate training or process issues
- Document findings and business justifications for audit and management reporting
- Establish recurring EI runs and alert routing so that creator-approver exceptions are reviewed continuously by responsible roles


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BADAT | Requisition Date | DATS | 8 | 0 | BADAT | DATUM |
| 3 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 4 | BEDAT | Purchase Order Date | DATS | 8 | 0 | BEDAT | DATUM |
| 5 | BNFPO | Item of Requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 6 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 7 | BSART | Document Type | CHAR | 4 | 0 | BBSRT | BSART |
| 8 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 9 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 10 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 11 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 12 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 13 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 14 | EBELN | Purchase Order | CHAR | 10 | 0 | BSTNR | EBELN |
| 15 | EBELP | Purchase Order Item | NUMC | 5 | 0 | BSTPO | EBELP |
| 16 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 17 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 18 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 19 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 20 | ERDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 21 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 22 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 23 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 24 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 25 | FRGKZ | Release indicator | CHAR | 1 | 0 | FRGKZ | FRGKZ |
| 26 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 27 | FRGST | Release strategy | CHAR | 2 | 0 | FRGST | FRGST |
| 28 | FRGZU | Release status | CHAR | 8 | 0 | FRGZU | FRGZU |
| 29 | GSFRG | Overall release of requisitions | CHAR | 1 | 0 | GSFRG | XFELD |
| 30 | KDATB | Validity Per. Start | DATS | 8 | 0 | KDATB | DATUM |
| 31 | KDATE | Validity Period End | DATS | 8 | 0 | KDATE | DATUM |
| 32 | LAST_ONLY | Only last approver is checked |  | 0 | 0 |  |  |
| 33 | LIFNR | Desired Vendor | CHAR | 10 | 0 | WLIEF | LIFNR |
| 34 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 35 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 36 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 37 | RLWRT | Total val. upon release | CURR | 15 | 2 | RLWRT | WERT15 |
| 38 | STATU | Processing status | CHAR | 1 | 0 | BANST | BANST |
| 39 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 40 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 41 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 42 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 43 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 44 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 45 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 46 | WERKS_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 46 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Backdays):

Number of days used to build the monitoring window for purchase requisitions. When no date range is supplied, the EI uses today minus this value as the start date. The chosen reference date field (e.g. requisition date or changed-on date) is then restricted to that window when reading requisition headers.

**BADAT** (Requisition Date):

Date of the purchase requisition. The EI uses this date (or BEDAT / ERDAT, depending on configuration) to decide whether a requisition falls within the monitoring window. It is the main date used when the reference date field is set to requisition date.

**BANFN** (Purchase Requisition):

Purchase requisition number. The EI reads requisition headers and change documents keyed by this identifier. Values narrow which requisitions are evaluated for the exception (creator same as last approver).

**BEDAT** (Purchase Order Date):

Purchase order date on the requisition. Can be used as the reference date for the monitoring window when the configuration points the lookback to order date instead of requisition or changed-on date.

**BNFPO** (Item of Requisition):

Item number of the purchase requisition. The EI works at header and item level for release; this identifies the requisition line.

**BSAKZ** (Control indicator):

Control indicator for the purchasing document type. The EI includes it in selection and in the result so configurations can focus on specific document-type behaviors.

**BSAKZ Options:**
- Values are defined by the document type configuration; typical values indicate control flags (e.g. **X**: set, ** ** (space): not set). Use transaction or master data to see allowed values for the relevant document type.

**BSART** (Document Type):

Purchase requisition document type (e.g. standard, framework). The EI restricts which requisition types are read and reported; each type can have different release and approval semantics.

**BSART_DESC** (Doc. Type Descript.):

Short description of the purchase requisition document type, derived from master data for display in the result.

**BSTYP** (Purch. Doc. Category):

Purchasing document category (e.g. requisition vs order). The EI fixes this to requisition (e.g. 'B') when reading requisition data; the parameter allows narrowing or displaying by category.

**BSTYP Options:**
- **B**: Purchase requisition (typical for this EI).
- **F**: Order (not used for requisition-based selection in this function).
- Other domain values as in standard SAP; use only values relevant to requisitions for this EI.

**BSTYP_DESC** (Short Descript.):

Short text for the purchasing document category, from domain or master data, for use in the result.

**CHANGENR** (Document Number):

Change document number for the release step. The EI uses change documents to determine who performed the last release; this field identifies the change document and appears in the result.

**DURATION** (Duration In Time Units):

Elapsed time between the reference date (requisition date, order date, or changed-on date, as configured) and the change document date of the release, in the unit given by DURATION_UNIT. The EI calculates it for each requisition release and uses it to apply duration-based filtering.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed and evaluated (hours, minutes, days, or full days for specific-day logic). The EI uses this when computing and comparing duration for the release step.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** DURATION holds the numeric value; DURATION_UNIT defines its meaning. Set both when using duration-based filtering (e.g. “released within N days of requisition date”).

**EBELN** (Purchase Order):

Purchase order number created from the requisition (if any). The EI can restrict or display by linked order; the relationship is from requisition to order.

**EBELP** (Purchase Order Item):

Purchase order item number. Links the requisition item to the order item in the result.

**EKGRP** (Purchasing Group):

Purchasing group responsible for the requisition. The EI selects and reports by purchasing group so monitoring can be scoped by responsibility.

**EKGRP_DESC** (Description p. group):

Description of the purchasing group from master data, for display in the result.

**EKORG** (Purch. Organization):

Purchasing organization of the requisition. The EI restricts and reports by purchasing organization for organizational scope.

**EKORG_DESC** (Description):

Description of the purchasing organization from master data, for display.

**ERDAT** (Changed On):

Date when the requisition (or relevant object) was last changed. Can be used as the reference date for the monitoring window when the configuration points the lookback to “changed on” instead of requisition or order date.

**ERNAM** (Created By):

User who created the purchase requisition. The EI compares this with the user who performed the last release (from the change document) to detect the exception “requisition approved by creator.”

**ESTKZ** (Creation Indicator):

Creation indicator for the requisition (e.g. manual vs automatic). The EI includes it in selection and result for differentiation by origin.

**ESTKZ Options:**
- Values are domain-specific (e.g. **1**: created by, **2**: created without, or similar); use domain fix values or documentation for the exact list.

**FRGC** (Release code):

Release code that represents the approval or release status. The EI resolves the release strategy and status to this code and uses it to filter which release steps are included (e.g. only fully released).

**FRGGR** (Release group):

Release group that defines the release strategy for the requisition. The EI uses it to select requisitions and to resolve the correct release strategy and codes.

**FRGKZ** (Release indicator):

Release indicator that categorizes the release step (e.g. per T161S). The EI uses it together with the release strategy to determine which requisitions and release steps are in scope.

**FRGKZ Options:**
- Values come from customizing (e.g. T161S); each value corresponds to a release indicator. Configure according to your release strategy.

**FRGRL** (Subject to release):

Indicates whether the requisition is subject to release. The EI typically restricts to requisitions that are subject to release (e.g. FRGRL = 'X') when evaluating “approved by creator.”

**FRGRL Options:**
- **X**: Subject to release (requisition requires release).
- ** ** (space): Not subject to release.

**FRGST** (Release strategy):

Release strategy code that defines the approval steps. The EI uses it to select requisitions and to interpret release status and release code.

**FRGZU** (Release status):

Release status code for the current step. The EI reads it from change documents (e.g. FRGZU) to determine who performed the last release and to compute duration.

**GSFRG** (Overall release of requisitions):

Indicates whether the requisition uses overall release (header-level) or item-level release. The EI uses it to decide whether to read the change document at header or item level for the last approver.

**GSFRG Options:**
- **X**: Overall release (header-level); one change document per requisition.
- ** ** (space): Item-level release; change document keyed by requisition item.

**KDATB** (Validity Per. Start):

Start of validity period for the requisition or related object. The EI can use it for time-based scope when exposed as a parameter.

**KDATE** (Validity Period End):

End of validity period. Used together with KDATB for validity-window filtering when relevant.

**LAST_ONLY** (Only last approver is checked):

When set, the EI considers only the last release step (last approver) per requisition or item; earlier steps are ignored. When not set, every release step is considered and the exception flags cases where the creator appears as any approver.

**LAST_ONLY Options:**
- **X**: Only the last approver is checked; earlier approvers are ignored.
- ** ** (space): All approvers are considered (creator must not be any approver).

**LIFNR** (Desired Vendor):

Desired or proposed vendor on the requisition. The EI selects and reports by vendor for monitoring by supplier.

**LOEKZ** (Deletion Indicator):

Deletion indicator on the requisition. The EI excludes deleted requisitions (e.g. LOEKZ = space) so only active requisitions are evaluated.

**LOEKZ Options:**
- ** ** (space): Not deleted (active).
- **L**: Deletion flag (or similar per domain); such records are typically excluded from the EI result.

**RESWK** (Supplying Plant):

Supplying or issuing plant. The EI selects and reports by plant for plant-level monitoring.

**RESWK_DESC** (Name 1):

Name of the supplying plant from master data, for display.

**RLWRT** (Total val. upon release):

Total value at time of release (document currency). The EI can use it for value-based filtering or display; describe as document currency value at release.

**STATU** (Processing status):

Processing status of the purchase requisition (e.g. approval status). The EI includes it in selection and result to scope or display by status.

**STATU Options:**
- Values are from domain BANST (e.g. pending, released, etc.). Use customizing or domain values for the exact list.

**STATU_DESC** (Short Descript.):

Short text for the processing status from domain or master data, for display.

**UDATE** (Date):

Date of the change document (release step). The EI uses it to compute duration from the reference date to the release and to sort or filter by when the approval occurred.

**USERNAME** (User):

User who performed the change documented in the change document (e.g. the last release). The EI compares this with ERNAM (creator) to detect “approved by creator.”

**UTIME** (Time):

Time of the change document. Used together with UDATE for precise ordering of release steps and duration calculation.

**VENDOR_DESC** (Name):

Name of the desired vendor from master data, for display.

**WAERS** (Currency):

Document currency of the purchase requisition. The EI uses it for currency-specific selection or display; it is the currency in which amounts (e.g. RLWRT) are expressed.

**WERKS** (Plant):

Plant (requisitioning plant or similar) on the requisition. The EI selects and reports by plant for organizational scope.

**WERKS_DESC** (Name 1):

Name of the plant from master data, for display.


### Parameter Relationships

**Time-Based Selection Parameters:**

- When no date range is supplied, the EI builds the monitoring window from today minus the lookback length. The number of days to look back is configured via a single numeric parameter; that value defines the start of the window. The EI then maps this window to a configurable date field (e.g. requisition date, order date, or changed-on date) so that requisitions are selected by the chosen date basis.

**Duration Calculation Parameters:**

- The EI computes a duration (in time units) between a reference date taken from each record and the current date. The reference date is taken from the output record using a configurable date field name. The unit in which duration is expressed (e.g. days) is configured separately. Together, the reference date field and the duration unit determine how duration is calculated; a numeric duration filter can then be used to restrict results (e.g. requisitions with duration within a range).

**Release Strategy and Release Code Parameters:**

- Release group, release strategy (FRGST), release indicator (FRGKZ), and release code (FRGC) work together to scope which requisitions are subject to release and which release states are included. The EI reads release-related data from the requisition and from change documents (FRGZU); it resolves release status to a release code and then filters by release code.

**Creator vs. Approver (LAST_ONLY):**

- When "only last approver" is set, the EI keeps at most one record per requisition (the most recent release by date/time) and then checks whether the user who performed that release is the same as the requisition creator. When not set, every release step where the approver equals the creator is included. This parameter therefore controls whether the result set is limited to the latest release per requisition or includes all creator-approver same-user release steps.


### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the EI uses a 10-day lookback from today for the monitoring window).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).
- **LAST_ONLY** — Default: initial (empty); when not supplied, the EI includes all release steps where the approver equals the creator (not only the last release per requisition).
- **LOEKZ** — Default: initial (empty); when not supplied, the EI uses only requisitions that are not marked for deletion (effectively "not deleted" only).

**Note:** The reference date field used for the monitoring window and for duration calculation is set in the code to a default (e.g. requisition date) when not supplied by the caller; other single-value parameters that are used when initial effectively default to "no restriction" where the code allows.

### Practical Configuration Examples

**Use Case 1: Last 10 days, creator = approver (default lookback)**

```
BACKDAYS = 10
LAST_ONLY = 
```

**Purpose:** Monitor purchase requisitions where the creator is also the approver, for the last 10 days. Suitable for routine weekly or biweekly segregation-of-duties review.

**Use Case 2: By purchasing organization and vendor**

```
EKORG = 1000, 2000
LIFNR = 0000100001–0000100050
```

**Purpose:** Limit results to specific purchasing organizations and vendor number ranges. Supports regional or vendor-specific control and delegation review.

**Use Case 3: Only last release per requisition**

```
LAST_ONLY = X
FRGST = 01, 02
```

**Purpose:** Keep only the most recent release per requisition and flag it only if that last approver is the requisition creator; restrict to specific release strategies. Reduces duplicate rows and focuses on the current release state.

**Use Case 4: Duration in full days (single value)**

```
DURATION_UNIT = F
DURATION = 7
```

**Purpose:** Express duration in full days and restrict to requisitions with duration equal to 7 full days since the reference date. Useful for age-based prioritization (e.g. requisitions exactly one week old). DURATION is a single value, not a range, when using DURATION_UNIT = F.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BADAT | Requisition (Request) Date | DATS(8) | BADAT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BANFN | Purchase Requisition Number | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BEDAT | Purchase Order Date | DATS(8) | BEDAT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BNFPO | Item Number of Purchase Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BSAKZ | Control indicator for purchasing document type | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BSART | Purchase Requisition Document Type | CHAR(4) | BBSRT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BSART_DESC | Short Description of Purchasing Document Type | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BSTYP | Purchasing Document Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | BSTYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | EBELN | Purchase Order Number | CHAR(10) | BSTNR |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | EBELP | Purchase Order Item Number | NUMC(5) | BSTPO |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | EKGRP_DESC | Description of purchasing group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | EKORG_DESC | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | ERDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | ESTKZ | Creation Indicator (Purchase Requisition/Schedule Lines) | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | FRGKZ | Release Indicator | CHAR(1) | FRGKZ |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | FRGRL | Release Not Yet Completely Effected | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | FRGST | Release Strategy in Purchase Requisition | CHAR(2) | FRGST |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | GSFRG | Overall release of purchase requisitions | CHAR(1) | GSFRG |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | KDATB | Start of Validity Period | DATS(8) | KDATB |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | KDATE | End of Validity Period | DATS(8) | KDATE |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | LIFNR | Desired Vendor | CHAR(10) | WLIEF |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | LOEKZ | Deletion Indicator in Purchasing Document | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | RESWK | Supplying (Issuing) Plant in Stock Transport Order | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | RESWK_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | RLWRT | Total value at time of release | CURR(15,2) | RLWRT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | STATU | Processing status of purchase requisition | CHAR(1) | BANST |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | STATU_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | VENDOR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_PR_APPR_BY_CR | WERKS_DESC | Name | CHAR(30) | NAME1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PR_APPR_BY_CR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PR_APPR_BY_CR OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               LAST_ONLY CHAR1,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  LV_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  LV_DATE_REF_FLD = 'BADAT'. "PO date
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 LAST_ONLY,
                 DURATION_UNIT.
  DATA_MULTY: BANFN        BANFN,
              BNFPO        BNFPO,
              BSART        BBSRT,
              BSTYP        BSTYP,
              BSAKZ        BSAKZ,
              STATU        BANST,
              ESTKZ        ESTKZ,
              FRGKZ        FRGKZ,
              EKGRP        EKGRP,
              ERNAM        ERNAM,
              FRGRL        FRGRL,
              EBELN        BSTNR,
              EKORG        EKORG,
              FRGGR        FRGGR,
              FRGST        FRGST,
              RESWK        RESWK,
              WERKS	       EWERK,
              LIFNR        ELIFN,
              ERDAT        AEDAT,
              BEDAT        BEDAT,
              BADAT        BADAT,
              WAERS        WAERS,
              DATUM        SY-DATUM,
              DURATION    /SKN/E_SW_DURATION,
              LOEKZ       ELOEK,
              FRGC        FRGCO.
  SELECT_MULTY:
              BANFN,
              BNFPO,
              BSART,
              BSTYP,
              BSAKZ,
              STATU,
              ESTKZ,
              FRGKZ,
              EKGRP,
              ERNAM,
              ERDAT,"Changed on
              FRGRL,
              EBELN,
              EKORG,
              FRGGR,
              FRGST,
              RESWK,
              WERKS,
              LIFNR,
              BEDAT,
              BADAT,
              WAERS,
              DATUM,
              DURATION,
              LOEKZ,
              FRGC.
  CONVERT_MULTY: EBELN ALPHA,
                 LIFNR ALPHA.
  "--- Set default for LOEKZ (not deleted only)
  READ TABLE R_LOEKZ INTO RS_LOEKZ INDEX 1.
  IF SY-TFILL = 0.
    RS_LOEKZ-SIGN = 'I'.
    RS_LOEKZ-OPTION = 'EQ'.
    RS_LOEKZ-LOW = ' '.
    APPEND RS_LOEKZ TO R_LOEKZ.
  ENDIF.
  RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
           R_FLD_VAL FOR DD03P-FIELDNAME .
  DATA :   FLD_NAME TYPE FIELDNAME.
  DATA : I TYPE I,
         CI(1) TYPE C,
         NFIELDS TYPE I VALUE 3.   "
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM .
  DATA : LANGU LIKE SY-LANGU .
  DATA : IS_OUT(1) TYPE C.
  DATA : TIME_DIFF TYPE  INT4 .
  DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
        LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
        LV_DDTEXT LIKE  DD07V-DDTEXT.
  DATA: LV_FRGCO  TYPE FRGCO.
  DATA: LS_CDPOS TYPE CDPOS,
        LT_CDPOS LIKE TABLE OF LS_CDPOS.
  DATA: BEGIN OF LS_WRK.
          INCLUDE STRUCTURE /SKN/S_SW_10_03_PR_APPR_BY_CR.
  DATA: WRK_OBJECTID  TYPE CDOBJECTV.
  DATA: WRK_TABKEY    TYPE CDPOS-TABKEY.
  DATA: END OF LS_WRK.
  DATA: LT_WRK LIKE TABLE OF LS_WRK.
  DATA: LV_OBJECTCLAS TYPE CDOBJECTCL VALUE 'BANF'.
  DATA: LV_TABKEY TYPE CDTABKEY.
  DATA: WRK_OBJECTID  TYPE CDOBJECTV,
        WRK_TABKEY    TYPE CDPOS-TABKEY.
  DATA: LS_T161S TYPE T161S,
        LT_T161S LIKE TABLE OF LS_T161S.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA : LS_VBPA TYPE VBPA,
         LT_VBPA LIKE TABLE OF LS_VBPA.
  DATA : LV_DATA_POSNR TYPE POSNR.
**********
  DATA: LV_FRGSX TYPE FRGSX.
  DATA: LV_RESWK TYPE WERKS_D.
  DATA: LV_WERKS TYPE WERKS_D.
  DATA: LV_WLIEF TYPE LIFNR.
  DATA: LV_BSART TYPE ESART.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PR_APPR_BY_CR'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'BEDAT'.
      R_BEDAT[] = R_DATUM[]. "Purchase Order Date
    WHEN 'BADAT'.
      R_BADAT[] = R_DATUM[]. "Request Order Date
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[]. "Changed On
    WHEN OTHERS.
      R_BADAT[] = R_DATUM[].
  ENDCASE.
  "--- Prepare FRGKZ
  SELECT *
    FROM T161S
    INTO CORRESPONDING FIELDS OF TABLE LT_T161S
    WHERE FRANG = 'X'          " Indicator: Released for RFQ/quotation processing
      AND FRGKZ IN R_FRGKZ.
  REFRESH R_FRGKZ.
  LOOP AT LT_T161S INTO LS_T161S.
    RS_FRGKZ-SIGN = 'I'.
    RS_FRGKZ-OPTION = 'EQ'.
    RS_FRGKZ-LOW = LS_T161S-FRGKZ.
    APPEND RS_FRGKZ TO R_FRGKZ.
  ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM EBAN AS E
      INNER JOIN CDHDR AS CH ON CH~OBJECTID = E~BANFN
      INTO CORRESPONDING FIELDS OF TABLE LT_WRK
    WHERE FRGRL IN R_FRGRL    "  EQ 'X'
      AND FRGGR IN R_FRGGR
      AND BANFN IN R_BANFN
      AND EBELN IN R_EBELN
      AND BSTYP = 'B'    " Reqisition in R_BSTYP
      AND EKORG IN R_EKORG
      AND LIFNR IN R_LIFNR
      AND RESWK IN R_RESWK
      AND BEDAT IN R_BEDAT
      AND BADAT IN R_BADAT
      AND ERDAT IN R_ERDAT
      AND BSART IN R_BSART
      AND EKGRP IN R_EKGRP
      AND ERNAM IN R_ERNAM
      AND WERKS	IN R_WERKS
      AND WAERS IN R_WAERS
      AND LOEKZ EQ SPACE      ""????????
      AND MEMORY EQ SPACE
      AND FRGKZ IN R_FRGKZ
      AND FRGST <>  ''
      AND   CH~OBJECTCLAS = LV_OBJECTCLAS  " 'EINKBELEG'
*      AND   e~ernam = ch~username
      .
  SORT LT_WRK.
  DELETE ADJACENT DUPLICATES FROM LT_WRK.
  LOOP AT LT_WRK INTO LS_WRK.
    SY_TABIX = SY-TABIX.
    LS_WRK-WRK_OBJECTID  = LS_WRK-BANFN.
    MODIFY LT_WRK FROM LS_WRK INDEX SY_TABIX.
  ENDLOOP.
  IF LT_WRK[] IS NOT INITIAL.
    SELECT * FROM CDPOS
      INTO CORRESPONDING FIELDS OF TABLE LT_CDPOS
      FOR ALL ENTRIES IN LT_WRK
      WHERE OBJECTCLAS = LV_OBJECTCLAS
        AND OBJECTID = LT_WRK-WRK_OBJECTID "  BANFN
        AND TABNAME = 'EBAN'
        AND FNAME = 'FRGZU'.
    SORT LT_CDPOS BY OBJECTCLAS OBJECTID CHANGENR DESCENDING TABKEY.
    IF LV_LAST_ONLY IS NOT INITIAL. "Only last release is relevant - the rest is deleted.
      CLEAR: WRK_OBJECTID, WRK_TABKEY.
      LOOP AT LT_CDPOS INTO LS_CDPOS.
        SY_TABIX = SY-TABIX.
        IF WRK_OBJECTID EQ LS_CDPOS-OBJECTID AND
           WRK_TABKEY   EQ LS_CDPOS-TABKEY.
          DELETE LT_CDPOS INDEX SY_TABIX.
          CONTINUE.
        ELSE.
          WRK_OBJECTID = LS_CDPOS-OBJECTID.
          WRK_TABKEY = LS_CDPOS-TABKEY.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  REFRESH T_DATA.
  DATA: LT_WRK_TABIX TYPE INT4.
  LOOP AT LT_WRK INTO LS_WRK.
*    MOVE-CORRESPONDING ls_wrk TO t_data.
    LT_WRK_TABIX = SY-TABIX.
    IF LS_WRK-GSFRG = 'X'.  " overall release
      READ TABLE LT_CDPOS INTO LS_CDPOS   " TRANSPORTING NO FIELDS
                          WITH KEY OBJECTCLAS = LV_OBJECTCLAS
                                   OBJECTID   = LS_WRK-WRK_OBJECTID
                                   CHANGENR   = LS_WRK-CHANGENR
                           BINARY SEARCH.
      IF NOT SY-SUBRC IS INITIAL.
        DELETE LT_WRK INDEX LT_WRK_TABIX.
        CONTINUE.
      ELSE.
        LS_WRK-FRGZU = LS_CDPOS-VALUE_NEW.
        MODIFY LT_WRK FROM LS_WRK INDEX LT_WRK_TABIX.
      ENDIF.
    ELSE.
      CONCATENATE SY-MANDT  LS_WRK-BANFN LS_WRK-BNFPO INTO LV_TABKEY.
      LS_WRK-WRK_TABKEY = LV_TABKEY.
      READ TABLE LT_CDPOS INTO LS_CDPOS   " TRANSPORTING NO FIELDS
                          WITH KEY OBJECTCLAS = LV_OBJECTCLAS
                                   OBJECTID   = LS_WRK-WRK_OBJECTID
                                   CHANGENR   = LS_WRK-CHANGENR
                                   TABKEY     = LV_TABKEY
                           BINARY SEARCH.
      IF NOT SY-SUBRC IS INITIAL.
        DELETE LT_WRK INDEX LT_WRK_TABIX.
        CONTINUE.
      ELSE.
        MODIFY LT_WRK FROM LS_WRK INDEX LT_WRK_TABIX.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SORT LT_WRK BY WRK_OBJECTID WRK_TABKEY UDATE DESCENDING UTIME DESCENDING.
  IF LV_LAST_ONLY IS NOT INITIAL. "Only last release is relevant - the rest is deleted.
    CLEAR: WRK_OBJECTID, WRK_TABKEY.
    LOOP AT LT_WRK INTO LS_WRK.
      IF WRK_OBJECTID EQ LS_WRK-WRK_OBJECTID
          AND WRK_TABKEY   EQ LS_CDPOS-TABKEY
        .
        CONTINUE.
      ELSE.
        WRK_OBJECTID = LS_WRK-WRK_OBJECTID.
        WRK_TABKEY   = LS_WRK-WRK_TABKEY.
        IF LS_WRK-USERNAME EQ LS_WRK-ERNAM.
          MOVE-CORRESPONDING LS_WRK TO T_DATA.
          APPEND T_DATA.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT LT_WRK INTO LS_WRK.
      IF LS_WRK-USERNAME EQ LS_WRK-ERNAM.
        MOVE-CORRESPONDING LS_WRK TO T_DATA.
        APPEND T_DATA.
      ENDIF.
    ENDLOOP.
  ENDIF.
********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    LV_FRGSX = T_DATA-FRGST.
    CALL FUNCTION '/SKN/F_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR             = T_DATA-FRGGR
        FRGSX             = LV_FRGSX         """t_data-FRGST        "-frgsx
        FRGZU             = T_DATA-FRGZU
      IMPORTING
        FRGC              = T_DATA-FRGC
      EXCEPTIONS
        WRONG_COMBINATION = 1
        OTHERS            = 2.
    IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE FRGC  NOT IN R_FRGC .
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          T_DATA-DURATION  = TIME_DIFF .
        ELSE.
          T_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
******************************************************************************
********************************************************************************
  "--- Set Descriptions
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    "-- BSTYP_DESC
    LV_DOMNAME = 'BSTYP'.
    LV_DOMVALUE = T_DATA-BSTYP.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-BSTYP_DESC = LV_DDTEXT.
    ENDIF.
    "-- STATU_DESC
    LV_DOMNAME = 'BANST'.     """''ESTAK'.
    LV_DOMVALUE = T_DATA-STATU.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-STATU_DESC = LV_DDTEXT.
    ENDIF.
    "-- BSART_DESC type ESART
    LV_BSART = T_DATA-BSART.
    CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART      = LV_BSART """"t_data-BSART
        LANGU      = LV_LANGU
        BSTYP      = T_DATA-BSTYP
      IMPORTING
        TYPE_DESC  = T_DATA-BSART_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- Get  Vendor Decriptions type LIFNR
    LV_WLIEF = T_DATA-LIFNR.
    CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR        = LV_WLIEF """t_data-LIFNR
      IMPORTING
        VENDOR_DESC  = T_DATA-VENDOR_DESC
      EXCEPTIONS
        WRONG_VENDOR = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKORG_DESC type EKORG
    CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG        = T_DATA-EKORG
        "LANGU              = lv_LANGU
      IMPORTING
        PUR_ORG_DESC = T_DATA-EKORG_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKGRP_DESC type EKGRP
    CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP        = T_DATA-EKGRP
*       LANGU        = lv_LANGU
      IMPORTING
        PUR_GRP_DESC = T_DATA-EKGRP_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "---- WERKS_DESC lv_WERKS
    LV_WERKS = T_DATA-WERKS.
    CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = LV_WERKS    """"t_data-RESWK
*       LANGU      = lv_LANGU
      IMPORTING
        PLANT_DESC = T_DATA-WERKS_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- RESWK_DESC (WERKS) type-WERKS_D
    LV_RESWK = T_DATA-RESWK.
    CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = LV_RESWK    """"t_data-RESWK
*       LANGU      = lv_LANGU
      IMPORTING
        PLANT_DESC = T_DATA-RESWK_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
