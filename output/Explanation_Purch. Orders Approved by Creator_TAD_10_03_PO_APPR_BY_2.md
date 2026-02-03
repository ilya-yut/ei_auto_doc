# Exception Indicator: PUR: PUR PO Approved by Creator - SW_10_03_PO_APPR_BY

## General Overview

This Exception Indicator (EI) monitors purchase orders that are subject to release and flags those where the same user who created the order also performed the release (approval). It combines purchase order header data with change document data to compare creator and approver, supporting segregation of duties and release-control oversight in procurement.

This EI serves as an essential control for procurement and financial oversight by:
- Enabling detection of purchase orders approved by their creator, which may indicate segregation-of-duties violations or missing approval workflows
- Supporting identification of release strategy and release-status patterns by company code, purchasing organization, and vendor for control design review
- Providing visibility into the timing of creation and approval via configurable date reference and duration for prioritization and audit
- Enabling analysis of release groups, release codes, and processing status for exception management and policy enforcement
- Supporting month-end and audit readiness by surfacing creator-approver same-user exceptions that may require remediation or disclosure

Monitoring creator-approver separation helps organizations enforce segregation of duties in purchasing, reduce risk of unauthorized commitments, and prioritize follow-up on high-value or aged exceptions. The EI is particularly valuable for procurement controls, internal audit, and compliance reviews.

The EI uses purchase order header data (EKKO), release configuration (T16FB), and change document header and item data (CDHDR, CDPOS) to determine release status and compare creator with approver.


## Problem Description

Failure to monitor purchase orders approved by their creator creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unmonitored creator-approver same-user cases can indicate weak release controls affecting commitment accuracy and period-end accruals
- Purchase orders released by the creator without independent approval may lead to unauthorized commitments or duplicate payments if discovered late
- Lack of visibility into release timing and value at release can delay identification of control gaps affecting financial statement assertions
- Concentrated exceptions in specific company codes or purchasing organizations may signal systemic release-control weaknesses

**Procurement and Control Risks**
- Same user creating and approving orders violates segregation of duties and may enable fraud or error in the procure-to-pay process
- Unidentified release strategy or release-code patterns can mask missing or misconfigured approval workflows
- Absence of monitoring by vendor, plant, or document type limits ability to enforce approval policies and delegation rules
- High volume of creator-approver exceptions in specific organizational units may reflect process or training issues

**Management Visibility and Decision-Making Risks**
- Lack of consolidated visibility delays management awareness of segregation-of-duties exceptions requiring intervention
- Unmonitored release patterns by organizational dimension limit ability to assign accountability and optimize controls
- Missing link between change document approver and order creator hinders root-cause analysis and corrective action
- Absence of duration-based prioritization (e.g. time since creation or release) limits efficient allocation of review resources

## Suggested Resolution

**Immediate Response**
- Review the purchase orders flagged by the EI to confirm creator and approver user IDs and the release status and value at release
- Verify high-value or high-risk orders using the appropriate display transaction (e.g. ME23N) to confirm legitimacy and whether approval was appropriate
- Check release strategy and release code configuration for the affected document types and organizational units
- Identify business context: delegated approval, emergency procedures, or missing workflow configuration

**System Assessment**
- Analyze the monitoring window and date reference used for duration calculation to ensure the scope aligns with the control objective
- Compare exception volume and patterns to prior periods and to expected activity by company code, purchasing organization, and vendor
- Examine release group, release strategy, and processing status distribution to detect misconfiguration or policy gaps
- Validate that filters (company code, vendor, document type, release group) match the intended control scope

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
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | BEDAT | Document Date | DATS | 8 | 0 | EBDAT | DATUM |
| 4 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 5 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 6 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 7 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 8 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 9 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 10 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 11 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 12 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 13 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 14 | EKGRP | Purchasing Group | CHAR | 3 | 0 | BKGRP | EKGRP |
| 15 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 16 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 17 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 18 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 19 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 20 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 21 | FRGKE | Release indicator | CHAR | 1 | 0 | FRGKE | FRGKE |
| 22 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 23 | FRGSX | Release Strategy | CHAR | 2 | 0 | FRGSX | FRGSX |
| 24 | FRGZU | Release State | CHAR | 8 | 0 | FRGZU | FRGZU |
| 25 | KDATB | Validity Per. Start | DATS | 8 | 0 | KDATB | DATUM |
| 26 | KDATE | Validity Period End | DATS | 8 | 0 | KDATE | DATUM |
| 27 | LAST_ONLY | Only last approver is checked |  | 0 | 0 |  |  |
| 28 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 29 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 30 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 31 | PROCSTAT_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 32 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 33 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 34 | RLWRT | Total val. upon release | CURR | 15 | 2 | RLWRT | WERT15 |
| 35 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 36 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 37 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 38 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 39 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 40 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 41 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 42 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 42 parameters listed in the Parameters Reference Table above.

**AEDAT** (Created on):

Date on which the purchase order was created. The EI uses this date (or the document date, depending on configuration) when building the monitoring window for order selection.

**BACKDAYS** (Backdays):

Number of days to look back from today. When no date range is supplied, the EI builds the monitoring window from today minus this value and applies it to the configured date field (e.g. creation date or PO date).

**BEDAT** (Document Date):

Purchase order document date (schedule line order date). The EI can use this date as the basis for the monitoring window and for duration calculation when so configured.

**BSAKZ** (Control indicator):

Control indicator for the purchasing document type. Values are function-specific.

**BSAKZ Options:**

Values are function-specific; see output structure or document type configuration.

**BSART** (Purchasing Doc. Type):

Purchasing document type (e.g. standard order, framework order). Used when the EI reads order header data to scope which document types are considered for the creator-approver check.

**BSART_DESC** (Doc. Type Descript.):

Short description of the purchasing document type; derived from document type and category via description lookup in the EI.

**BSTYP** (Purch. Doc. Category):

Purchasing document category (e.g. standard order, framework order). Used when the EI reads order header data to scope which document categories are considered.

**BSTYP Options:**

Values are function-specific (e.g. standard order, framework order).

**BSTYP_DESC** (Short Descript.):

Short text for the purchasing document category; derived from master data in the EI.

**BUKRS** (Company Code):

Company code of the purchase order. Used when the EI reads order header data to scope which company codes are considered for the creator-approver check.

**CHANGENR** (Document Number):

Change document number identifying the change log record for the release step; used to correlate the approver (USERNAME) with the release status (FRGZU) and to join order data with change document data.

**DURATION** (Duration In Time Units):

Duration in time units between the reference date (e.g. creation or PO date) and current date. The EI calculates this per record; the unit is configured via DURATION_UNIT. Used for age-based prioritization or filtering.

**DURATION_UNIT** (Duration Unit):

Unit in which duration is expressed (e.g. days). Used with DURATION for the duration calculation in the EI and for time-based filtering.

**DURATION_UNIT Options:**

- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**EBELN** (Purchasing Document):

Purchasing document number. Used when the EI reads order header data and change documents to scope which orders are considered (by document number).

**EKGRP** (Purchasing Group):

Purchasing group of the order. Description is derived from master data in the EI. Used when the EI reads order header data to scope by purchasing group.

**EKGRP_DESC** (Description p. group):

Description of the purchasing group; from master data.

**EKORG** (Purch. Organization):

Purchasing organization of the order. Used when the EI reads order header data to scope which purchasing organizations are considered. Description is derived from master data in the EI.

**EKORG_DESC** (Description):

Description of the purchasing organization; from master data.

**ERNAM** (Created by):

User who created the purchase order. The EI compares this with the user who performed the release (USERNAME) to flag creator-approver same-user cases.

**FRGC** (Release code):

Release code resolved from release group, release strategy, and release status in the EI. Used after the EI reads release status from the change document to scope which release codes are included in the result.

**FRGC Options:**

Values are function-specific (release code configuration).

**FRGGR** (Release group):

Release group of the order. Used when the EI reads order header data to scope which orders are subject to release and have the specified release group(s).

**FRGKE** (Release indicator):

Release indicator that determines which release configuration applies to the order. Used when the EI reads order header data together with release configuration to scope which orders are subject to release.

**FRGKE Options:**

Values are function-specific (release configuration).

**FRGRL** (Subject to release):

Indicates whether the order is subject to release. Used when the EI reads order header data to scope which orders are subject to release and have a release strategy (creator-approver check applies only to such orders).

**FRGRL Options:**

- **X**: Set/active (subject to release).
- ** ** (space) or blank: Not set.

**FRGSX** (Release Strategy):

Release strategy code of the order. Used when the EI reads order header data to scope which orders have the specified release strategy and are subject to release.

**FRGZU** (Release State):

Release status (e.g. released, partially released). The EI reads this from the change document for the release field and resolves it to a release code (FRGC); the release code is then used to scope which orders are included in the result.

**KDATB** (Validity Per. Start):

Start of validity period for the release strategy or related configuration.

**KDATE** (Validity Period End):

End of validity period for the release strategy or related configuration.

**LAST_ONLY** (Only last approver is checked):

When set, the EI keeps only the most recent release per order and flags it only if that last approver is the order creator. When not set, every release step where the approver equals the creator is included.

**LAST_ONLY Options:**

- **X**: Set/active (only last release per order).
- ** ** (space) or blank: Not set (all creator-approver same-user steps).

**LIFNR** (Vendor):

Vendor (supplier) number of the order. Used when the EI reads order header data to scope which vendors are considered for the creator-approver check.

**LOEKZ** (Deletion Indicator):

Deletion indicator on the order. Used when the EI reads order header data; orders marked for deletion are excluded when this is not set, or scoped by the supplied value(s).

**LOEKZ Options:**

- **X**: Set/active (deletion indicator set).
- ** ** (space) or blank: Not set.

**PROCSTAT** (Purch. doc. proc. state):

Purchasing document processing state. Used when the EI reads order header data to scope which processing states are considered.

**PROCSTAT Options:**

Values are function-specific (e.g. draft, released, partially processed).

**PROCSTAT_DESC** (Short Descript.):

Short text for the processing state; derived from master data in the EI.

**RESWK** (Supplying Plant):

Supplying (issuing) plant of the order. Used when the EI reads order header data to scope which plants are considered.

**RESWK_DESC** (Name 1):

Name or description of the supplying plant; from master data.

**RLWRT** (Total val. upon release):

Total value at time of release. From the order; used for value-based analysis when exposed in the result.

**STATU** (Status):

Status of the purchasing document. Used when the EI reads order header data to scope which statuses are considered.

**STATU Options:**

Values are function-specific (e.g. released, blocked).

**STATU_DESC** (Short Descript.):

Short text for the document status; derived from master data in the EI.

**UDATE** (Date):

Creation date of the change document (release step). Identifies when the release was performed; used together with UTIME to order release steps (e.g. last release per order when LAST_ONLY is set).

**USERNAME** (User):

User who performed the release (approver). The EI compares this with the order creator (ERNAM) to flag creator-approver same-user cases.

**UTIME** (Time):

Time of the change document (release step). Used together with UDATE to order release steps (e.g. last release per order when LAST_ONLY is set).

**VENDOR_DESC** (Name):

Vendor name; derived from vendor master in the EI.

**WAERS** (Currency):

Document currency of the purchase order. Business meaning: currency in which the order is valued.

**ZTERM** (Terms of Payment):

Terms of payment key of the order. Used when the EI reads order header data to scope which terms of payment are considered.


### Parameter Relationships

**Time-Based Selection Parameters:**

- When no date range is supplied, the EI builds the monitoring window from today minus the lookback length. The number of days to look back is configured via a single numeric parameter; that value defines the start of the window. The EI then maps this window to a configurable date field (e.g. document created date or PO date) so that purchase orders are selected by the chosen date basis.

**Duration Calculation Parameters:**

- The EI computes a duration (in time units) between a reference date taken from each record and the current date. The reference date is taken from the output record using a configurable date field name. The unit in which duration is expressed (e.g. days) is configured separately. Together, the reference date field and the duration unit determine how duration is calculated; a numeric duration filter can then be used to restrict results (e.g. orders with duration within a range).

**Release Strategy and Release Code Parameters:**

- Release group, release strategy, release indicator, and release code work together to scope which purchase orders are subject to release and which release states are included. The EI reads release status from the change document (field FRGZU) and resolves it to a release code; the release code filter then determines which orders appear in the result. Setting release group, release strategy, and release code in combination focuses the result set on the relevant release configuration and status.

**Creator vs. Approver (LAST_ONLY):**

- When "only last approver" is set, the EI keeps at most one record per order (the most recent change by release date/time) and then checks whether the user who performed that release is the same as the order creator. When not set, the EI includes every release step where the approver equals the creator. This parameter therefore controls whether the result set is limited to the latest release per order or includes all creator-approver same-user release steps.


### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the EI uses a 10-day lookback from today for the monitoring window).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).
- **LAST_ONLY** — Default: initial (empty); when not supplied, the EI includes all release steps where the approver equals the creator (not only the last release per order).

**Note:** The reference date field used for the monitoring window and for duration calculation is set in the code to a default (e.g. PO date) when not supplied by the caller; other single-value parameters that are used when initial effectively default to "no restriction" where the code allows.

### Practical Configuration Examples

**Use Case 1: Last 10 days, creator = approver (default lookback)**

```
BACKDAYS = 10
```

**Purpose:** Monitor purchase orders where the creator is also the approver, for the last 10 days. Suitable for routine weekly or biweekly segregation-of-duties review.

**Use Case 2: By company code and purchasing organization**

```
BUKRS = 1000, 2000
EKORG = 1000, 2000
```

**Purpose:** Limit results to specific company codes and purchasing organizations. Supports regional or organizational control and delegation review.

**Use Case 3: Only last release per order**

```
LAST_ONLY = X
```

**Purpose:** Keep only the most recent release per order and flag it only if that last approver is the order creator. Reduces duplicate rows and focuses on the current release state.

**Use Case 4: Duration in full days (specific day filtering)**

```
DURATION_UNIT = F
DURATION = 0–30
```

**Purpose:** Express duration in full days and restrict to orders with duration between 0 and 30 full days since the reference date. Useful for age-based prioritization (e.g. recent approvals only).


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_03_PO_APPR_BY_CR | AEDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BEDAT | Purchasing Document Date | DATS(8) | EBDAT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSAKZ | Control indicator for purchasing document type | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSART | Purchasing Document Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSART_DESC | Short Description of Purchasing Document Type | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSTYP | Purchasing Document Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSTYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EBELN | Purchasing Document Number | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKGRP | Purchasing Group | CHAR(3) | BKGRP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKGRP_DESC | Description of purchasing group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKORG_DESC | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGKE | Release Indicator: Purchasing Document | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGRL | Release Not Yet Completely Effected | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGSX | Release Strategy | CHAR(2) | FRGSX |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | KDATB | Start of Validity Period | DATS(8) | KDATB |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | KDATE | End of Validity Period | DATS(8) | KDATE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | LIFNR | Vendor Account Number | CHAR(10) | ELIFN |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | LOEKZ | Deletion Indicator in Purchasing Document | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | PROCSTAT | Purchasing document processing state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | PROCSTAT_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RESWK | Supplying (Issuing) Plant in Stock Transport Order | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RESWK_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RLWRT | Total value at time of release | CURR(15,2) | RLWRT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | STATU | Status of Purchasing Document | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | STATU_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | VENDOR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | ZTERM | Terms of Payment Key | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PO_APPR_BY_CR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_APPR_BY_CR OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               LAST_ONLY CHAR1,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  LV_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  LV_DATE_REF_FLD = 'BEDAT'. "PO date
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 LAST_ONLY,
                 DURATION_UNIT.
  DATA_MULTY: FRGRL        FRGRL,
              EBELN        EBELN,
              BUKRS        BUKRS,
              BSTYP        EBSTYP,
              BSART        ESART,
              EKORG        EKORG,
              EKGRP        BKGRP,
              FRGGR        FRGGR,
              FRGSX        FRGSX,
              FRGCO        FRGCO,
              FRGKE        FRGKE,
              LIFNR        ELIFN,
              RESWK        RESWK,
              ZTERM        DZTERM,
              ERNAM        ERNAM,
              AEDAT        ERDAT,
              BEDAT        EBDAT,
              WAERS        WAERS,
              PROCSTAT    MEPROCSTATE,
              DATUM        SY-DATUM,
              DURATION    /SKN/E_SW_DURATION.
  SELECT_MULTY:
              FRGRL,
              EBELN,
              BUKRS,
              BSTYP,
              BSART,
              EKORG,
              EKGRP,
              FRGGR,
              FRGSX,
              FRGCO,
              FRGKE,
              LIFNR,
              RESWK,
              ZTERM,
              ERNAM,
              AEDAT,
              BEDAT,
              WAERS,
              PROCSTAT,
              DATUM,
              DURATION.
  CONVERT_MULTY: EBELN ALPHA,
                 LIFNR ALPHA.
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
          INCLUDE STRUCTURE /SKN/S_SW_10_03_PO_APPR_BY_CR.
  DATA: WRK_OBJECTID  TYPE CDOBJECTV.
  DATA: WRK_TABKEY    TYPE CDPOS-TABKEY.
  DATA: END OF LS_WRK.
  DATA: LT_WRK LIKE TABLE OF LS_WRK.
  DATA: LV_OBJECTCLAS TYPE CDOBJECTCL VALUE 'EINKBELEG'.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
  DATA: WRK_OBJECTID  TYPE CDOBJECTV,
        WRK_TABKEY    TYPE CDPOS-TABKEY.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA : LS_VBPA TYPE VBPA,
         LT_VBPA LIKE TABLE OF LS_VBPA.
  DATA : LV_DATA_POSNR TYPE POSNR.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PO_APPR_BY_CR'
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
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "Document created
    WHEN 'BEDAT'.
      R_BEDAT[] = R_DATUM[]. "PO Date
    WHEN OTHERS.
      R_BEDAT[] = R_DATUM[]. "Billing date
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM EKKO AS E
      INNER JOIN T16FB AS T  ON E~FRGKE = T~FRGKE
      INNER JOIN CDHDR AS CH ON CH~OBJECTID = E~EBELN
      INTO CORRESPONDING FIELDS OF TABLE LT_WRK
      WHERE   T~KZFRE = 'X'
        AND   FRGRL IN R_FRGRL    "  EQ 'X'
        AND FRGGR IN R_FRGGR
        AND FRGSX IN R_FRGSX
        AND EBELN IN R_EBELN
        AND BSTYP IN R_BSTYP
        AND EKORG IN R_EKORG
        AND BUKRS IN R_BUKRS
        AND LIFNR IN R_LIFNR
        AND RESWK IN R_RESWK
        AND BEDAT IN R_BEDAT
        AND AEDAT IN R_AEDAT
        AND BSART IN R_BSART
        AND EKGRP IN R_EKGRP
        AND ERNAM IN R_ERNAM
        AND ZTERM IN R_ZTERM
        AND WAERS IN R_WAERS
        AND LOEKZ EQ SPACE
        AND PROCSTAT IN R_PROCSTAT
     "----
      AND   CH~OBJECTCLAS = LV_OBJECTCLAS  " 'EINKBELEG'
*      AND   e~ernam = ch~username
      AND FRGSX > ''
      AND E~FRGKE IN R_FRGKE.
  SORT LT_WRK.
  DELETE ADJACENT DUPLICATES FROM LT_WRK.
  LOOP AT LT_WRK INTO LS_WRK.
    SY_TABIX = SY-TABIX.
    LS_WRK-WRK_OBJECTID  = LS_WRK-EBELN.
    MODIFY LT_WRK FROM LS_WRK INDEX SY_TABIX.
  ENDLOOP.
  IF LT_WRK[] IS NOT INITIAL.
    SELECT * FROM CDPOS
      INTO CORRESPONDING FIELDS OF TABLE LT_CDPOS
      FOR ALL ENTRIES IN LT_WRK
      WHERE OBJECTCLAS = LV_OBJECTCLAS  "'EINKBELEG'
        AND OBJECTID = LT_WRK-WRK_OBJECTID "  EBELN
        AND TABNAME = 'EKKO'
        AND FNAME = 'FRGZU'.
*** YC++ 18.05.21
    IF LT_CDPOS IS NOT INITIAL.
      SORT LT_CDPOS BY VALUE_NEW.
      DELETE LT_CDPOS WHERE VALUE_NEW IS INITIAL.
    ENDIF.
*** YC++ 18.05.21
*      sort lt_CDPOS by OBJECTCLAS OBJECTID.
    SORT LT_CDPOS BY OBJECTCLAS OBJECTID CHANGENR DESCENDING TABKEY.
*    IF lv_last_only IS NOT INITIAL. "Only last release is relevant - the
*      CLEAR: wrk_objectid, wrk_tabkey.
*      LOOP AT lt_cdpos INTO ls_cdpos.
*        sy_tabix = sy-tabix.
*        IF wrk_objectid EQ ls_cdpos-objectid AND
*           wrk_tabkey   EQ ls_cdpos-tabkey.
*          DELETE lt_cdpos INDEX sy_tabix.
*          CONTINUE.
*        ELSE.
*          wrk_objectid = ls_cdpos-objectid.
*          wrk_tabkey = ls_cdpos-tabkey.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
  ENDIF.
  REFRESH T_DATA.
  DATA: LT_WRK_TABIX TYPE INT4.
  LOOP AT LT_WRK INTO LS_WRK.
    LT_WRK_TABIX = SY-TABIX.
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
  ENDLOOP.
  SORT LT_WRK BY WRK_OBJECTID UDATE DESCENDING UTIME DESCENDING.
  IF LV_LAST_ONLY IS NOT INITIAL. "Only last release is relevant - the rest is deleted.
    CLEAR: WRK_OBJECTID, WRK_TABKEY.
    LOOP AT LT_WRK INTO LS_WRK.
      IF WRK_OBJECTID EQ LS_WRK-WRK_OBJECTID
*          AND wrk_tabkey   EQ ls_cdpos-tabkey
        .
        CONTINUE.
      ELSE.
        WRK_OBJECTID = LS_WRK-WRK_OBJECTID.
        IF LS_WRK-USERNAME EQ LS_WRK-ERNAM.
          MOVE-CORRESPONDING LS_WRK TO T_DATA.
          APPEND T_DATA.
        ENDIF.
*          wrk_tabkey = ls_cdpos-tabkey.
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
*********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR             = T_DATA-FRGGR
        FRGSX             = T_DATA-FRGSX
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
  DELETE T_DATA WHERE FRGC NOT IN R_FRGCO.
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
    LV_DOMNAME = 'EBSTYP'.
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
    LV_DOMNAME = 'ESTAK'.
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
    "-- PROCSTAT_DESC
    LV_DOMNAME = 'MEPROCSTATE'.
    LV_DOMVALUE = T_DATA-PROCSTAT.
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
      T_DATA-PROCSTAT_DESC = LV_DDTEXT.
    ENDIF.
    "-- BSART_DESC
    CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART      = T_DATA-BSART
        LANGU      = LV_LANGU
        BSTYP      = T_DATA-BSTYP
      IMPORTING
        TYPE_DESC  = T_DATA-BSART_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- Get  Vendor Decriptions
    CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR        = T_DATA-LIFNR
      IMPORTING
        VENDOR_DESC  = T_DATA-VENDOR_DESC
      EXCEPTIONS
        WRONG_VENDOR = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKORG_DESC
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
    "-- EKGRP_DESC
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
    "--- RESWK_DESC (WERKS)
    CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = T_DATA-RESWK
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
