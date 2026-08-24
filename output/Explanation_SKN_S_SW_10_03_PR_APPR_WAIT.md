# Exception Indicator: PR Waiting for Approval ( SW_10_03_PR_APPR_WAT)

## General Overview

This Exception Indicator identifies purchase requisitions that are subject to release, have an assigned release strategy, and match configured release-indicator and release-code criteria—surfacing requisitions that are still waiting for approval action.

This EI serves as an essential control for procurement release governance by:

- Enabling detection of purchase requisitions blocked in the release workflow before they convert to purchase orders
- Supporting follow-up on requisitions with assigned release strategies that have not completed approval
- Providing visibility into release group, strategy, derived release code, and requisition status on each flagged line
- Enabling age-based prioritization when requisitions remain in the approval queue after a chosen reference date
- Supporting audit sampling of pending release backlog by purchasing organization, plant, vendor, and account assignment

Typical use includes buyer and approver escalation, release workflow health checks, and periodic control samples before close. Results are intended for exception workflows rather than operational MM list reporting.

The routine reads purchase requisition data with optional account-assignment context, derives the current release code, applies optional age filtering, and raises an alert when qualifying requisitions remain.


## Problem Description

Failure to monitor purchase requisitions waiting for release approval creates multiple risks across procurement control, operational throughput, and compliance:

**Procurement and Approval Risks**

- Requisitions stuck in the release workflow can delay sourcing and purchase order creation when approvers are not alerted
- Release strategies assigned but not completed may leave demand visible without authorized approval to proceed
- Undetected backlog across vendors, plants, or purchasing groups can concentrate risk on critical materials or services

**Operational Risks**

- Release-indicator scope that is too loose or too tight can hide actionable pending requisitions or create reviewer fatigue
- Lookback and age settings misaligned with approval cadence can exclude recent queue items or retain stale cases
- Account-assignment and organizational filters that are not tuned can mix irrelevant requisitions into the pending queue

**Control and Audit Risks**

- Weak monitoring reduces evidence that requisition release backlog was reviewed before period close
- Lack of recurring exception review weakens segregation-of-duties over who may approve high-value requisitions
- Missing age-based prioritization limits escalation of long-pending approval cases

## Suggested Resolution

**Immediate Response**

- Review flagged purchase requisitions for vendor, plant, release group, strategy, release code, and status
- Contact the responsible requisitioner, buyer, or approver to confirm whether release action is pending or overdue
- Prioritize high-value or long-waiting requisitions for approval or rejection through standard workflows

**System Assessment**

- Validate lookback window and reference-date choice against approval review cadence
- Tune release-indicator, release-code, and organizational scope so results stay actionable
- Compare exception counts by purchasing group, release group, and document type to find systematic gaps

**Corrective Actions**

- Complete pending releases or reject requisitions through standard MM approval processes
- Correct release strategy or master-data issues identified during review
- Document outcomes, brief stakeholders on recurring patterns, and schedule recurring runs before close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ANLN1 | Asset | CHAR | 12 | 0 | ANLN1 | ANLN1 |
| 2 | ANLN2 | Asset Subnumber | CHAR | 4 | 0 | ANLN2 | ANLN2 |
| 3 | AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 4 | BACKDAYS | Days Backward |  | 0 | 0 |  |  |
| 5 | BADAT | Requisition Date | DATS | 8 | 0 | BADAT | DATUM |
| 6 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 7 | BEDAT | Purchase Order Date | DATS | 8 | 0 | BEDAT | DATUM |
| 8 | BNFPO | Item of Requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 9 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 10 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | BBSRT | BSART |
| 11 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 12 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 13 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 14 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 15 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 16 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 17 | EBELN | Purchasing Document | CHAR | 10 | 0 | BSTNR | EBELN |
| 18 | EBELP | Purchase Order Item | NUMC | 5 | 0 | BSTPO | EBELP |
| 19 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 20 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 21 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 22 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 23 | ERDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 24 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 25 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 26 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 27 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 28 | FRGKZ | Release indicator | CHAR | 1 | 0 | FRGKZ | FRGKZ |
| 29 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 30 | FRGST | Release Strategy | CHAR | 2 | 0 | FRGST | FRGST |
| 31 | FRGZU | Release status | CHAR | 8 | 0 | FRGZU | FRGZU |
| 32 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 33 | KOSTL | Cost Center | CHAR | 10 | 0 | KOSTL | KOSTL |
| 34 | LANG | Language for texts |  | 0 | 0 |  |  |
| 35 | LIFNR | Vendor | CHAR | 10 | 0 | WLIEF | LIFNR |
| 36 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 37 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 38 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 39 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 40 | SAKTO | G/L Account | CHAR | 10 | 0 | SAKNR | SAKNR |
| 41 | STATU | Status | CHAR | 1 | 0 | BANST | BANST |
| 42 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 43 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 44 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN | VBELN |
| 45 | VBELP | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 46 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 47 | VETEN | Schedule Line Number | NUMC | 4 | 0 | ETENR | ETENR |
| 48 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 49 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 50 | WERKS_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 50 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ANLN1 - ANLN2** (Asset)

Main Asset Number uniquely identifies the core capital asset (e.g., a delivery truck or a building).

**AUFNR** (Order)

Order number key for internal orders or manufacturing orders-primary CO/PP order identifier in many extracts.

**BACKDAYS** (Days Backward)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BADAT** (Requisition Date)

Requisition date expressing when material is required-drives need-by and replenishment timing in MM.

**BANFN** (Purchase Requisition)

Purchase requisition number, the core PR document key for MM approval and lifecycle checks.

**BEDAT** (Purchase Order Date)

Purchasing document date used to filter procurement documents by document creation period.

**BNFPO** (Item of Requisition)

Purchase requisition item number used to identify PR line-level records.

**BSAKZ** (Control indicator)

Purchasing control/indicator flag used to segment procurement records by processing characteristics.

**BSART** (Purchasing Doc. Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BSART_DESC** (Doc. Type Descript.)

Text description of purchasing document type used for readable reporting output.

**BSTYP** (Purch. Doc. Category)

Purchasing document category (PR/PO/contract etc.) used to segment MM document classes.

**BSTYP_DESC** (Short Descript.)

Description of purchasing document category for business-readable output.

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- BEDAT — Purchasing document date used to filter procurement documents by document creation period.
- BADAT — Requisition date expressing when material is required-drives need-by and replenishment timing in MM.
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**EBELN** (Purchasing Document)

Purchasing document number (typically PO) used as the primary MM document key.

**EBELP** (Purchase Order Item)

Purchasing document item number used for line-level PO analytics.

**EKGRP** (Purchasing Group)

Purchasing group (buyer) used for procurement ownership and control segmentation.

**EKGRP_DESC** (Description p. group)

Description of purchasing group for readable buyer/team reporting.

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**EKORG_DESC** (Description)

Description of purchasing organization for business-readable reporting.

**ERDAT** (Changed on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ESTKZ** (Creation Indicator)

Creation indicator for PR/PO source or method, used for process-origin analysis.

**FRGC** (Release code)

Derived release code for each requisition from release group, strategy, and release status; filters rows to the configured release-code values after derivation.

**FRGGR** (Release group)

Release group key controlling the purchasing release strategy framework.

**FRGKZ** (Release indicator)

Purchasing release state indicator on requisitions or orders showing whether and how release strategy applies.

**FRGRL** (Subject to release)

Release indicator/flag used in PO/PR release strategy control.

**FRGRL Options:**
- X: Subject to release
- (blank): Not subject to release

**FRGST** (Release Strategy)

Overall release status on purchasing documents summarizing approval strategy progress for PO/PR objects.

**FRGZU** (Release status)

Release strategy progression/status code used for approval lifecycle tracking.

**GSBER** (Business Area)

Business area key used for FI organizational reporting segmentation.

**KOSTL** (Cost Center)

Cost center used as primary CO account assignment for postings and budgets.

**LANG** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**Not in use**
**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**PRCTR** (Profit Center)

Profit center used for management accounting segmentation and profitability reporting.

**RESWK** (Supplying Plant)

Supplying/Issuing Plant designates the specific internal plant from which materials are being transferred or procured during a Stock Transport Order. Used in cross-plant logistics analysis.

**RESWK_DESC** (Name 1)

Plant description text used to enrich plant-level reporting.

**SAKTO** (G/L Account)

Cost element used in CO postings for primary or secondary cost capture and account assignment.

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**STATU_DESC** (Short Descript.)

Status description text used for readable status analytics.

**USER_FLD** (Dynamic Recipient User Field)

The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.
How DRL Works:
When USER_FLD is specified, the system extracts values from that field in the monitoring result set
These extracted values are then used as recipient addresses for alert notifications
This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored
The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users

**Not in use**
**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBELP** (Sales Document Item)

Sales document item number alias on extension extracts; typically mirrors POSNR line indexing.

**VENDOR_DESC** (Name)

Vendor description text used for readable supplier-level reporting.

**VETEN** (Schedule Line Number)

Schedule line number on the account assignment used to filter requisition lines linked to a specific schedule row.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WERKS_DESC** (Name 1)

Plant name or description providing readable site context beside plant keys.

### Parameter Relationships

**Reference-date window:** When **DATUM** is empty, a lower bound of today minus **BACKDAYS** is applied with a greater-than-or-equal filter and copied to **BEDAT**, **BADAT**, or **ERDAT** per **DATE_REF_FLD** (default **ERDAT**). Explicit date selections override that fallback window.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference date to the evaluation date; rows outside the configured duration range are removed.

**Release waiting scope:** The requisition selection requires matching **FRGRL** (subject to release), **FRGGR**, and a non-empty **FRGST** (release strategy); **FRGKZ** further narrows release-relevant requisitions.

**Release code derivation:** **FRGGR**, **FRGST**, and **FRGZU** on each requisition drive derivation of **FRGC** (release code) before results are returned; **FRGC** filters the final set.

**Requisition scope:** **BANFN**, **BNFPO**, **BSTYP**, **BSART**, **EKORG**, **EKGRP**, **ERNAM**, **LIFNR**, **RESWK**, **WERKS**, **WAERS**, **LOEKZ**, **STATU**, and **ESTKZ** combine to define which waiting-for-approval requisitions enter the result set.

**Account assignment:** **SAKTO**, **GSBER**, **KOSTL**, **VBELN**, **VBELP**, **VETEN**, **ANLN1**, **ANLN2**, **AUFNR**, and **PRCTR** filter rows after the requisition is read, using linked account-assignment data.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - ERDAT
- **LOEKZ** - blank

### Practical Example of Parameter Configuration

**Use Case 1: Requisition release backlog in the last sixty days**

**Purpose:** Review purchase requisitions waiting for approval with changed-on dates in the last sixty days.

```
BACKDAYS = 60
DATE_REF_FLD = ERDAT
FRGRL = X
EKORG = 1000
```

**Use Case 2: Specific release group and strategy**

**Purpose:** Monitor waiting requisitions under one release group and strategy combination.

```
FRGGR = 01
FRGST = 01
BACKDAYS = 45
EKGRP = 001
```

**Use Case 3: Pending release indicator**

**Purpose:** Focus on requisitions with release indicator values that indicate approval is still outstanding.

```
FRGKZ = 0
BACKDAYS = 90
BSTYP = B
WERKS = 1000
```

**Use Case 4: Cost center account assignment**

**Purpose:** Sample pending release requisitions charged to one cost center for controller review.

```
KOSTL = 1000
BACKDAYS = 30
FRGRL = X
EKORG = 1000
```

**Use Case 5: Exactly seven full days since reference date**

**Purpose:** Return rows whose reference date is exactly 7 full days ago for weekly approval escalation.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
DATE_REF_FLD = ERDAT
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_03_PR_APPR_WAIT | ANLN1 | Asset | CHAR(12) | ANLN1 |
| /SKN/S_SW_10_03_PR_APPR_WAIT | ANLN2 | Subnumber | CHAR(4) | ANLN2 |
| /SKN/S_SW_10_03_PR_APPR_WAIT | AUFNR | Order | CHAR(12) | AUFNR |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BADAT | Requisition Date | DATS(8) | BADAT |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BANFN | Purchase Requisition | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BEDAT | Purchase Order Date | DATS(8) | BEDAT |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BNFPO | Item of Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BSAKZ | Control indicator | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BSART | Document Type | CHAR(4) | BBSRT |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BSART_DESC | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BSTYP | Purch. Doc. Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PR_APPR_WAIT | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PR_APPR_WAIT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PR_APPR_WAIT | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PR_APPR_WAIT | EBELN | Purchase Order | CHAR(10) | BSTNR |
| /SKN/S_SW_10_03_PR_APPR_WAIT | EBELP | Purchase Order Item | NUMC(5) | BSTPO |
| /SKN/S_SW_10_03_PR_APPR_WAIT | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_PR_APPR_WAIT | EKGRP_DESC | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PR_APPR_WAIT | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PR_APPR_WAIT | EKORG_DESC | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PR_APPR_WAIT | ERDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_03_PR_APPR_WAIT | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PR_APPR_WAIT | ESTKZ | Creation Indicator | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_PR_APPR_WAIT | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PR_APPR_WAIT | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PR_APPR_WAIT | FRGKZ | Release indicator | CHAR(1) | FRGKZ |
| /SKN/S_SW_10_03_PR_APPR_WAIT | FRGRL | Subject to release | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PR_APPR_WAIT | FRGST | Release Strategy | CHAR(2) | FRGST |
| /SKN/S_SW_10_03_PR_APPR_WAIT | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PR_APPR_WAIT | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_03_PR_APPR_WAIT | KOSTL | Cost Center | CHAR(10) | KOSTL |
| /SKN/S_SW_10_03_PR_APPR_WAIT | LIFNR | Desired Vendor | CHAR(10) | WLIEF |
| /SKN/S_SW_10_03_PR_APPR_WAIT | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PR_APPR_WAIT | PRCTR | Profit Center | CHAR(10) | PRCTR |
| /SKN/S_SW_10_03_PR_APPR_WAIT | RESWK | Supplying Plant | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PR_APPR_WAIT | RESWK_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PR_APPR_WAIT | SAKTO | G/L Account | CHAR(10) | SAKNR |
| /SKN/S_SW_10_03_PR_APPR_WAIT | STATU | Processing status | CHAR(1) | BANST |
| /SKN/S_SW_10_03_PR_APPR_WAIT | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PR_APPR_WAIT | VBELN | Sales Document | CHAR(10) | VBELN |
| /SKN/S_SW_10_03_PR_APPR_WAIT | VBELP | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_03_PR_APPR_WAIT | VENDOR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PR_APPR_WAIT | VETEN | Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_03_PR_APPR_WAIT | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PR_APPR_WAIT | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_PR_APPR_WAIT | WERKS_DESC | Name 1 | CHAR(30) | NAME1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PR_APPR_WAIT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PR_APPR_WAIT OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: LANGU  LANGU,
             BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
 LV_BACKDAYS = 10.
 LV_DURATION_UNIT = 'D'.
 LV_LANGU = SY-LANGU.
 LV_DATE_REF_FLD = 'ERDAT'. "PO date
 SELECT_SINGLE: LANGU,
                BACKDAYS,
                DATE_REF_FLD,
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
            DURATION     /SKN/E_SW_DURATION,
            LOEKZ        ELOEK,
            FRGC         FRGCO,
            SAKTO        SAKTO,
            GSBER        GSBER,
            KOSTL        KOSTL,
            VBELN        VBELN,
            VBELP        POSNR_VA,
            VETEN        ETENR,
            ANLN1        ANLN1,
            ANLN2        ANLN2,
            AUFNR        AUFNR,
            PRCTR        PRCTR.
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
            FRGC,
            SAKTO,
            GSBER,
            KOSTL,
            VBELN,
            VBELP,
            VETEN,
            ANLN1,
            ANLN2,
            AUFNR,
            PRCTR.
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
    CALL FUNCTION '/SKN/FC_SW_10_03_PR_APPR_WAIT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
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
       R_ERDAT[] = R_DATUM[].
   ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM EBAN AS A LEFT OUTER JOIN EBKN AS K ON  A~BANFN EQ K~BANFN
                                             AND A~BNFPO EQ K~BNFPO
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE A~FRGRL IN R_FRGRL    "  eq 'x'
      AND A~FRGGR IN R_FRGGR
      AND A~EBELN IN R_EBELN
      AND A~BSTYP IN R_BSTYP
      AND A~EKORG IN R_EKORG
      AND A~LIFNR IN R_LIFNR
      AND A~RESWK IN R_RESWK
      AND A~BEDAT IN R_BEDAT
      AND A~BADAT IN R_BADAT
      AND A~ERDAT IN R_ERDAT
      AND A~BSART IN R_BSART
      AND A~EKGRP IN R_EKGRP
      AND A~ERNAM IN R_ERNAM
      AND A~WERKS	IN R_WERKS
      AND A~WAERS IN R_WAERS
***      AND LOEKZ eq SPACE      ""????????
      AND A~LOEKZ	IN R_LOEKZ
      AND A~FRGKZ IN R_FRGKZ
     "----
***      AND frgsx > ''
      AND A~FRGST NE  ''
*** Begin change Yuri C.++ 12.04.18
*      AND k~sakto IN r_sakto
*      AND k~gsber IN r_gsber
*      AND k~kostl IN r_kostl
*      AND k~vbeln IN r_vbeln
*      AND k~vbelp IN r_vbelp
*      AND k~veten IN r_veten
*      AND k~anln1 IN r_anln1
*      AND k~anln2 IN r_anln2
*      AND k~prctr IN r_prctr
*** End Yuri C.++ 12.04.18
***      and FRGKZ = '0'
       .
*********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    LV_FRGSX = T_DATA-FRGST.
    CALL FUNCTION '/SKN/F_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR                   = T_DATA-FRGGR
        FRGSX                   = LV_FRGSX         """t_data-FRGST        "-frgsx
        FRGZU                   = T_DATA-FRGZU
      IMPORTING
        FRGC                    = T_DATA-FRGC
      EXCEPTIONS
        WRONG_COMBINATION       = 1
        OTHERS                  = 2.
    IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE FRGC  NOT IN R_FRGC .
*** Begin Yuri C.++ 15.04.18
    DELETE T_DATA WHERE SAKTO NOT IN R_SAKTO.
    DELETE T_DATA WHERE GSBER NOT IN R_GSBER.
    DELETE T_DATA WHERE KOSTL NOT IN R_KOSTL.
    DELETE T_DATA WHERE VBELN NOT IN R_VBELN.
    DELETE T_DATA WHERE VBELP NOT IN R_VBELP.
    DELETE T_DATA WHERE VETEN NOT IN R_VETEN.
    DELETE T_DATA WHERE ANLN1 NOT IN R_ANLN1.
    DELETE T_DATA WHERE ANLN2 NOT IN R_ANLN2.
    DELETE T_DATA WHERE AUFNR NOT IN R_AUFNR.
    DELETE T_DATA WHERE PRCTR NOT IN R_PRCTR.
*** End Yuri C.++ 15.04.18
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
          D_FROM            = REF_DATE
          T_FROM            = SY-UZEIT
          D_TO              = SY-DATUM
          T_TO              = SY-UZEIT
          TIME_UNIT         = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
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
        I_DOMNAME        = LV_DOMNAME
        I_DOMVALUE       = LV_DOMVALUE
        LANGU            = LV_LANGU
*       SW_DEST          =
      IMPORTING
        E_DDTEXT         = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST        = 1
        OTHERS           = 2.
    IF SY-SUBRC = 0.
      T_DATA-BSTYP_DESC = LV_DDTEXT.
    ENDIF.
    "-- STATU_DESC
    LV_DOMNAME = 'BANST'.     """''ESTAK'.
    LV_DOMVALUE = T_DATA-STATU.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME        = LV_DOMNAME
        I_DOMVALUE       = LV_DOMVALUE
        LANGU            = LV_LANGU
*       SW_DEST          =
      IMPORTING
        E_DDTEXT         = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST        = 1
        OTHERS           = 2.
    IF SY-SUBRC = 0.
      T_DATA-STATU_DESC = LV_DDTEXT.
    ENDIF.
**    "-- PROCSTAT_DESC
**    lv_DOMNAME = 'MEPROCSTATE'.
**    lv_DOMVALUE = t_data-PROCSTAT.
**
**    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
**      EXPORTING
**        i_domname        = lv_DOMNAME
**        i_domvalue       = lv_DOMVALUE
**        LANGU            = lv_LANGU
***       SW_DEST          =
**      IMPORTING
**        E_DDTEXT         = lv_DDTEXT
**      EXCEPTIONS
**        NOT_EXIST        = 1
**        OTHERS           = 2.
**    IF sy-subrc = 0.
**      t_data-PROCSTAT_DESC = lv_DDTEXT.
**    ENDIF.
    "-- BSART_DESC type ESART
    LV_BSART = T_DATA-BSART.
    CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART            = LV_BSART """"t_data-BSART
        LANGU            = LV_LANGU
        BSTYP            = T_DATA-BSTYP
      IMPORTING
        TYPE_DESC        = T_DATA-BSART_DESC
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- Get  Vendor Decriptions type LIFNR
     LV_WLIEF = T_DATA-LIFNR.
     CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
       EXPORTING
         LIFNR              = LV_WLIEF """t_data-LIFNR
       IMPORTING
         VENDOR_DESC        = T_DATA-VENDOR_DESC
       EXCEPTIONS
         WRONG_VENDOR       = 1
         OTHERS             = 2.
     IF SY-SUBRC <> 0.
     ENDIF.
   "-- EKORG_DESC type EKORG
    CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG              = T_DATA-EKORG
        "LANGU              = lv_LANGU
      IMPORTING
        PUR_ORG_DESC       = T_DATA-EKORG_DESC
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
   "-- EKGRP_DESC type EKGRP
    CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP              = T_DATA-EKGRP
*       LANGU              = lv_LANGU
      IMPORTING
        PUR_GRP_DESC       = T_DATA-EKGRP_DESC
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
"---- WERKS_DESC lv_WERKS
     LV_WERKS = T_DATA-WERKS.
     CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
       EXPORTING
         WERKS            = LV_WERKS    """"t_data-RESWK
*        LANGU            = lv_LANGU
       IMPORTING
         PLANT_DESC       = T_DATA-WERKS_DESC
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2.
     IF SY-SUBRC <> 0.
     ENDIF.
    "--- RESWK_DESC (WERKS) type-WERKS_D
    LV_RESWK = T_DATA-RESWK.
     CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
       EXPORTING
         WERKS            = LV_RESWK    """"t_data-RESWK
*        LANGU            = lv_LANGU
       IMPORTING
         PLANT_DESC       = T_DATA-RESWK_DESC
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2.
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
