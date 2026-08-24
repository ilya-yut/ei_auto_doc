# Exception Indicator: Released PO waiting for GR ( SW_10_03_OPEN_PR_DET)

## General Overview

This Exception Indicator identifies released purchase order schedule lines that still await goods receipt, combining requisition release status with schedule-line quantities to surface procurement items where ordered quantity exceeds quantity already received.

This EI serves as an essential control for procurement and inventory management by:

- Enabling detection of released orders with outstanding receipt quantity that can delay production, projects, or replenishment
- Supporting follow-up on vendor deliveries before open commitments accumulate unnoticed
- Providing visibility into release and processing state alongside open quantity for prioritization
- Enabling segmentation by vendor, plant, material, and organizational purchasing scope for targeted buyer action
- Supporting age-based review of how long lines have remained open since a chosen reference date

Typical use includes buyer escalation on overdue receipts, pre-close reviews of open procurement, and sampling of released lines before accrual or vendor performance discussions. Results are intended for exception workflows rather than operational MM list reporting.

The routine reads purchase requisition, schedule-line, and purchase order data joined to vendor master information, retains lines with remaining open quantity after enrichment, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor released purchase orders that still await goods receipt creates multiple risks across procurement operations, supply continuity, and financial control:

**Procurement and Supply Risks**

- Released lines with unreceived quantity can stall manufacturing, maintenance, or project timelines when deliveries are late or partially posted
- Buyers may lack a consolidated view of open receipt quantity across schedule lines tied to the same order item
- Vendor or plant scope that is too broad can hide concentrated overdue lines on critical materials or services

**Operational Risks**

- Release and processing state that is not reviewed alongside open quantity can mix lines that are not yet actionable into follow-up queues
- Lookback and age settings that are misaligned can exclude recently released lines or retain rows outside the intended monitoring window
- Goods-receipt and completion indicators that are too loose can surface lines that are no longer receipt-relevant

**Control and Audit Risks**

- Weak monitoring reduces evidence that open released commitments were reviewed before period close or supplier escalation
- Unaddressed open receipt quantity can inflate commitment exposure and weaken three-way match discipline
- Lack of recurring exception review limits accountability for buyer follow-up on vendor performance

## Suggested Resolution

**Immediate Response**

- Review flagged purchase orders, items, vendors, delivery dates, and open receipt quantity
- Confirm release and processing status with the responsible buyer or requisitioner
- Prioritize high-value, critical-material, or long-waiting lines for vendor contact or internal receipt posting

**System Assessment**

- Validate lookback window and reference-date choice against how the team defines overdue or pending receipts
- Tune deletion, delivery-completed, and goods-receipt scope so the queue reflects truly open, receipt-relevant lines
- Compare exception counts by vendor, plant, purchasing group, and document type to find systematic follow-up gaps

**Corrective Actions**

- Post goods receipts or correct schedule lines through standard MM processes where deliveries were recorded late
- Adjust monitoring scope after cleanup so results stay actionable for buyers
- Document review outcomes, brief buyers on recurring patterns, and schedule recurring runs for critical vendors or materials


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | AFNAM | Requisitioner | CHAR | 12 | 0 | AFNAM | AFNAM |
| 3 | ANLN1 | Asset | CHAR | 12 | 0 | ANLN1 | ANLN1 |
| 4 | ANLN2 | Subnumber | CHAR | 4 | 0 | ANLN2 | ANLN2 |
| 5 | AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 6 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 7 | BADAT | Requisition Date | DATS | 8 | 0 | BADAT | DATUM |
| 8 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 9 | BATXT | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 10 | BEDAT | Purchase Order Date | DATS | 8 | 0 | ETBDT | DATUM |
| 11 | BNFPO | Item of Requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 12 | BPUMN | Quantity Conversion | DEC | 5 | 0 | BPUMN | UMBSN |
| 13 | BPUMZ | Quantity Conversion | DEC | 5 | 0 | BPUMZ | UMBSZ |
| 14 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 15 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | BSTYP | BSTYP |
| 16 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 17 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 18 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 19 | BWTTY | Valuation Category | CHAR | 1 | 0 | BWTTY_D | BWTTY |
| 20 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 21 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 22 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 23 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 24 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 25 | EBELP | Item | NUMC | 5 | 0 | EBELP | EBELP |
| 26 | EINDT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 27 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 28 | EKNAM | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 29 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 30 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 31 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 32 | ERDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 33 | EREKZ | Final Invoice | CHAR | 1 | 0 | EREKZ | XFELD |
| 34 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 35 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 36 | ETENR | Schedule Line Number | NUMC | 4 | 0 | ETENR | ETENR |
| 37 | FIPOS | Commitment Item | CHAR | 14 | 0 | FIPOS | FIPOS |
| 38 | FRGKE | Release indicator (PO) | CHAR | 1 | 0 | FRGKE | FRGKE |
| 39 | FRGKZ | Release indicator | CHAR | 1 | 0 | FRGKZ | FRGKZ |
| 40 | GJAHR | Material Doc. Year | NUMC | 4 | 0 | MJAHR | GJAHR |
| 41 | GL_ACC_TXT | G/L Acct Long Text | CHAR | 50 | 0 | TXT50_SKAT | TEXT50 |
| 42 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 43 | KNTTP | Acct Assignment Cat. | CHAR | 1 | 0 | KNTTP | KNTTP |
| 44 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 45 | KOSTL | Cost Center | CHAR | 10 | 0 | KOSTL | KOSTL |
| 46 | KOSTL_DESC | Description | CHAR | 40 | 0 | KLTXT | TEXT40 |
| 47 | LFDAT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 48 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 49 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 50 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 51 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 52 | MEINS | Order Unit | UNIT | 3 | 0 | BSTME | MEINS |
| 53 | MENGE | Scheduled Quantity | QUAN | 13 | 3 | ETMEN | MENGE |
| 54 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 55 | NETWR | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 56 | OPEN_ORDER_QUAN | Open Quantity | QUAN | 13 | 3 | OBMNG | MENG13 |
| 57 | PEINH | Price Unit | DEC | 5 | 0 | EPEIN | DEC5 |
| 58 | PLIFZ | Planned Deliv. Time | DEC | 3 | 0 | PLIFZ | DEC3 |
| 59 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 60 | PRCTR_DESC | Long Text | CHAR | 40 | 0 | LTEXT | TEXT40 |
| 61 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 62 | PS_PSP_PNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 63 | PSTYP | Item Category | CHAR | 1 | 0 | PSTYP | PSTYP |
| 64 | SAKTO | G/L Account | CHAR | 10 | 0 | SAKNR | SAKNR |
| 65 | SHKZG | Debit/Credit Ind. | CHAR | 1 | 0 | SHKZG | SHKZG |
| 66 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 67 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 68 | TXZ01 | Short Text | CHAR | 40 | 0 | TXZ01 | TEXT40 |
| 69 | UEBTK | Unltd Overdelivery | CHAR | 1 | 0 | UEBTK | XFELD |
| 70 | UEBTO | Overdeliv. Tolerance | DEC | 3 | 1 | UEBTO | PRZ21 |
| 71 | USER_FLD | User field for Dyn Rec List |  | 0 | 0 |  |  |
| 72 | VBELN | SD Document | CHAR | 10 | 0 | VBELN_CO | VBELN |
| 73 | VBELP | Item | NUMC | 6 | 0 | POSNR_CO | POSNR |
| 74 | VBUND | Trading Partner | CHAR | 6 | 0 | RASSC | RCOMP |
| 75 | VGABE | Trans./event type | CHAR | 1 | 0 | VGABE | VGABE |
| 76 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 77 | WEMNG | Qty Delivered | QUAN | 13 | 3 | WEEMG | MENG13 |
| 78 | WEPOS | Goods Receipt | CHAR | 1 | 0 | WEPOS | XFELD |
| 79 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 80 | WGBEZ | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 80 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Created on)

Changed-on date used to filter documents or master records by last maintenance activity.

**AFNAM** (Requisitioner)

After data is read, lines are removed unless requisitioner on AFNAM still satisfies the active multivalued selection.

**ANLN1 - ANLN2** (Asset)

For distributed landscapes, asset on ANLN1 often anchors which application server or destination appears in results.

**AUFNR** (Order)

Order number key for internal orders or manufacturing orders-primary CO/PP order identifier in many extracts.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BADAT** (Requisition Date)

Requisition date expressing when material is required-drives need-by and replenishment timing in MM.

**BANFN** (Purchase Requisition)

Purchase requisition number, the core PR document key for MM approval and lifecycle checks.

**BATXT** (Doc. Type Descript.)

<mark>Session header text on batch-input sessions describing purpose or source of the BDC run.</mark>

**BEDAT** (Purchase Order Date)

Purchasing document date used to filter procurement documents by document creation period.

**BNFPO** (Item of Requisition)

Purchase requisition item number used to identify PR line-level records.

**BPUMN** (Quantity Conversion)

Denominator for price-unit conversion on purchasing conditions translating condition amounts to order quantities.

**BPUMZ** (Quantity Conversion)

Numerator for price-unit conversion paired with BPUMN to express per-unit purchasing prices correctly.

**BSART** (Purchasing Doc. Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BSTYP** (Purch. Doc. Category)

Purchasing document category (PR/PO/contract etc.) used to segment MM document classes.

**BSTYP_DESC** (Short Descript.)

Description of purchasing document category for business-readable output.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**BWTAR** (Valuation Type)

Valuation type key used in split valuation scenarios (batch/material valuation layers).

**BWTTY** (Valuation Category)

Valuation category distinguishing split valuation types such as sales-order stock versus own stock.

**CPUDT** (Entry Date)

Entry/creation date used for technical posting timestamp filtering.

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- BEDAT — Purchasing document date used to filter procurement documents by document creation period.
- AEDAT — Changed-on date used to filter documents or master records by last maintenance activity.
- EINDT — Item delivery date on purchasing or delivery structures expressing vendor-promised or requested receipt date.
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- BADAT — Requisition date expressing when material is required-drives need-by and replenishment timing in MM.

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

**EBELP** (Item)

Purchasing document item number used for line-level PO analytics.

**EINDT** (Delivery Date)

Item delivery date on purchasing or delivery structures expressing vendor-promised or requested receipt date.

**EKGRP** (Purchasing Group)

Purchasing group (buyer) used for procurement ownership and control segmentation.

**EKNAM** (Description p. group)

Purchasing group description or buyer name text paired with EKGRP for readable procurement ownership lists.

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**EKOTX** (Description)

Purchasing org/group related text field used for descriptive output enrichment.

**ELIKZ** (Delivery Completed)

Delivery completed indicator used to identify open versus completed procurement items.

**ERDAT** (Changed on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**EREKZ** (Final Invoice)

Final invoice indicator on the PO item signaling that invoice completion is expected or locked for the line.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ESTKZ** (Creation Indicator)

Creation indicator for PR/PO source or method, used for process-origin analysis.

**ETENR** (Schedule Line Number)

Schedule line number splitting a sales item into multiple delivery or availability schedule rows.

**FIPOS** (Commitment Item)

Commitment Item, which is an alphanumeric key used in Funds Management (FI-FM) to mirror the budget structure for specific revenues and expenditures.

**FRGKE** (Release indicator (PO))

Release status indicator used to distinguish released vs unreleased documents.

**FRGKZ** (Release indicator)

Purchasing release state indicator on requisitions or orders showing whether and how release strategy applies.

**GJAHR** (Material Doc. Year)

Supports operational control by evaluating material doc. year through GJAHR for each candidate record.

**GL_ACC_TXT** (G/L Acct Long Text)

When combined with destination discipline, g/l acct long text on GL_ACC_TXT keeps both breadth and depth of the extract intentional.

**GSBER** (Business Area)

Business area key used for FI organizational reporting segmentation.

**KNTTP** (Acct Assignment Cat.)

Account assignment category on purchasing items telling whether stock is project, asset, cost-center, or sales-order.

**KOKRS** (Controlling Area)

Controlling area key used for CO-level organizational scoping.

**KOSTL** (Cost Center)

Cost center used as primary CO account assignment for postings and budgets.

**KOSTL_DESC** (Description)

Allows phased rollout: first widen KOSTL_DESC for description, then tighten thresholds once baseline noise is understood.

**LFDAT** (Delivery Date)

Delivery date used for logistics due-date and fulfillment timeliness checks.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MEINS** (Order Unit)

Base unit of measure used to interpret quantity fields consistently.

**MENGE** (Scheduled Quantity)

Quantity field used for volumetric thresholds and variance analysis.

**NAME1** (Name)

Allows phased rollout: first widen NAME1 for name, then tighten thresholds once baseline noise is understood.

**NETWR** (Net Order Value)

Net value amount used for commercial threshold and anomaly checks.

**OPEN_ORDER_QUAN** (Open Quantity)

Combines with related filters so open quantity on OPEN_ORDER_QUAN refines which records remain for duration or state checks.

**PEINH** (Price Unit)

Price unit denominator used to interpret per-unit purchasing prices.

**PLIFZ** (Planned Deliv. Time)

Planned delivery time in days from purchasing info records or schedule lines for lead-time analytics.

**PRCTR** (Profit Center)

Profit center used for management accounting segmentation and profitability reporting.

**PRCTR_DESC** (Long Text)

Ensures reporting respects long text constraints carried by PRCTR_DESC.

**PROCSTAT** (Purch. doc. proc. state)

Purchasing document processing state describing lifecycle and processing of MM purchasing objects.

**PS_PSP_PNR** (WBS Element)

WBS element key used for project-system linked cost/procurement monitoring.

**PSTYP** (Item Category)

Purchasing document item category controlling item behavior, account assignment, and goods-receipt rules.

**SAKTO** (G/L Account)

Cost element used in CO postings for primary or secondary cost capture and account assignment.

**SHKZG** (Debit/Credit Ind.)

Debit/Credit indicator used to separate accounting posting direction.

**SHKZG Options:**
- S — Debit posting
- H — Credit posting

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**STATU_DESC** (Short Descript.)

Status description text used for readable status analytics.

**TXZ01** (Short Text)

Short item text on purchasing or SD lines used for free-text filters and readable line descriptions.

**UEBTK** (Unltd Overdelivery)

Unlimited overdelivery allowed indicator on SD or MM quantity contracts controlling tolerance behavior.

**UEBTO** (Overdeliv. Tolerance)

Overdelivery tolerance percent defining how much quantity overrun is accepted versus the order quantity.

**USER_FLD** (User field for Dyn Rec List)

The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.
How DRL Works:
When USER_FLD is specified, the system extracts values from that field in the monitoring result set
These extracted values are then used as recipient addresses for alert notifications
This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored
The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users

**Not in use**
**VBELN** (SD Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBELP** (Item)

Sales document item number alias on extension extracts; typically mirrors POSNR line indexing.

**VBUND** (Trading Partner)

Trading partner/company field used for intercompany transaction analysis.

**VGABE** (Trans./event type)

Transaction/event type in purchasing history used to classify movement category.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WEMNG** (Qty Delivered)

<mark>Goods-receipt quantity on purchasing history or order-related rows for GR-versus-PO variance checks.</mark>

**WEPOS** (Goods Receipt)

Goods-receipt indicator on purchasing history rows marking lines created by goods receipt postings.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WGBEZ** (Material Group Desc.)

Material group description used for readable category reporting.

### Parameter Relationships

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper function and the on-premise selection path below that call is skipped.

**Reference-date window:** When **DATUM** is empty, a lower bound of today minus **BACKDAYS** is built with a greater-than filter and copied to **BEDAT**, **AEDAT**, **EINDT**, **ERDAT**, or **BADAT** per **DATE_REF_FLD** (default **BEDAT**). Explicit date selections override that fallback window.

**Age filter after date selection:** **DURATION** with **DURATION_UNIT** applies a second filter on elapsed time from each row's reference date to the evaluation date. Both the date window and the duration filter must be satisfied when duration is supplied.

**Open-line defaults:** When **ELIKZ**, **LOEKZ**, or **WEPOS** ranges are empty, defaults apply: delivery-completed blank, deletion indicator blank, goods-receipt indicator set to receipt-relevant.

**Requisition release scope:** **FRGKZ** and **FRGKE** on the linked requisition restrict which released requisition lines enter the initial schedule-line selection.

**Open quantity:** **OPEN_ORDER_QUAN** is derived as scheduled quantity minus quantity delivered; optional filtering on **OPEN_ORDER_QUAN** keeps only lines with remaining receipt quantity above configured thresholds.

**Header and item enrichment:** After the initial schedule-line and requisition read, purchase order header, item, and vendor attributes (**BUKRS**, **LIFNR**, **MATNR**, **WERKS**, **PROCSTAT**, and related scope fields) are applied in a second pass to remove rows outside organizational and material filters.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - BEDAT
- **ELIKZ** - blank
- **LOEKZ** - blank
- **WEPOS** - X
- **LANGU** - initial - defaults from system logon language when not supplied

### Practical Example of Parameter Configuration

**Use Case 1: Released lines with open receipt in the last sixty days**

**Purpose:** Monitor released requisition-linked schedule lines with order dates in the last sixty days and receipt-relevant items only.

```
BACKDAYS = 60
DATE_REF_FLD = BEDAT
WEPOS = X
ELIKZ =
```

**Use Case 2: One vendor and plant**

**Purpose:** Follow up open receipt quantity for a single vendor at one plant.

```
LIFNR = 100000
WERKS = 1000
BACKDAYS = 90
FRGKZ = 2
```

**Use Case 3: Purchasing organization and material group**

**Purpose:** Review open released lines for a purchasing organization and product category.

```
EKORG = 1000
MATKL = 001
BACKDAYS = 30
BUKRS = 1000
```

**Use Case 4: Minimum open quantity threshold**

**Purpose:** Flag only lines with meaningful remaining receipt quantity.

```
OPEN_ORDER_QUAN = 1 - 999999
BACKDAYS = 45
DATE_REF_FLD = BEDAT
```

**Use Case 5: Exactly seven full days since reference date**

**Purpose:** Return rows where the reference date is exactly 7 full days ago.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
DATE_REF_FLD = BEDAT
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_03_OPEN_PR_DET | AEDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_OPEN_PR_DET | AFNAM | Requisitioner | CHAR(12) | AFNAM |
| /SKN/S_SW_10_03_OPEN_PR_DET | ANLN1 | Asset | CHAR(12) | ANLN1 |
| /SKN/S_SW_10_03_OPEN_PR_DET | ANLN2 | Subnumber | CHAR(4) | ANLN2 |
| /SKN/S_SW_10_03_OPEN_PR_DET | AUFNR | Order | CHAR(12) | AUFNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | BADAT | Requisition Date | DATS(8) | BADAT |
| /SKN/S_SW_10_03_OPEN_PR_DET | BANFN | Purchase Requisition | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_OPEN_PR_DET | BATXT | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | BEDAT | Purchase Order Date | DATS(8) | ETBDT |
| /SKN/S_SW_10_03_OPEN_PR_DET | BNFPO | Item of Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_OPEN_PR_DET | BPUMN | Quantity Conversion | DEC(5) | BPUMN |
| /SKN/S_SW_10_03_OPEN_PR_DET | BPUMZ | Quantity Conversion | DEC(5) | BPUMZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_OPEN_PR_DET | BSTYP | Purch. Doc. Category | CHAR(1) | BSTYP |
| /SKN/S_SW_10_03_OPEN_PR_DET | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_OPEN_PR_DET | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_03_OPEN_PR_DET | BWTTY | Valuation Category | CHAR(1) | BWTTY_D |
| /SKN/S_SW_10_03_OPEN_PR_DET | CPUDT | Entry Date | DATS(8) | CPUDT |
| /SKN/S_SW_10_03_OPEN_PR_DET | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_OPEN_PR_DET | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_OPEN_PR_DET | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_OPEN_PR_DET | EBELP | Item | NUMC(5) | EBELP |
| /SKN/S_SW_10_03_OPEN_PR_DET | EINDT | Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_OPEN_PR_DET | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_OPEN_PR_DET | EKNAM | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_OPEN_PR_DET | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_OPEN_PR_DET | EKOTX | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_OPEN_PR_DET | ELIKZ | Delivery Completed | CHAR(1) | ELIKZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | ERDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_03_OPEN_PR_DET | EREKZ | Final Invoice | CHAR(1) | EREKZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_OPEN_PR_DET | ESTKZ | Creation Indicator | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | ETENR | Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_03_OPEN_PR_DET | FIPOS | Commitment Item | CHAR(14) | FIPOS |
| /SKN/S_SW_10_03_OPEN_PR_DET | FRGKE | Release indicator | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_OPEN_PR_DET | FRGKZ | Release indicator | CHAR(1) | FRGKZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | GJAHR | Material Doc. Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_03_OPEN_PR_DET | GL_ACC_TXT | G/L Acct Long Text | CHAR(50) | TXT50_SKAT |
| /SKN/S_SW_10_03_OPEN_PR_DET | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_03_OPEN_PR_DET | KNTTP | Acct Assignment Cat. | CHAR(1) | KNTTP |
| /SKN/S_SW_10_03_OPEN_PR_DET | KOKRS | Controlling Area | CHAR(4) | KOKRS |
| /SKN/S_SW_10_03_OPEN_PR_DET | KOSTL | Cost Center | CHAR(10) | KOSTL |
| /SKN/S_SW_10_03_OPEN_PR_DET | KOSTL_DESC | Description | CHAR(40) | KLTXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | LFDAT | Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_OPEN_PR_DET | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_OPEN_PR_DET | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_03_OPEN_PR_DET | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | MEINS | Order Unit | UNIT(3) | BSTME |
| /SKN/S_SW_10_03_OPEN_PR_DET | MENGE | Scheduled Quantity | QUAN(13) | ETMEN |
| /SKN/S_SW_10_03_OPEN_PR_DET | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_OPEN_PR_DET | NETWR | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_OPEN_PR_DET | OPEN_ORDER_QUAN | Open Quantity | QUAN(13) | OBMNG |
| /SKN/S_SW_10_03_OPEN_PR_DET | PEINH | Price Unit | DEC(5) | EPEIN |
| /SKN/S_SW_10_03_OPEN_PR_DET | PLIFZ | Planned Deliv. Time | DEC(3) | PLIFZ |
| /SKN/S_SW_10_03_OPEN_PR_DET | PRCTR | Profit Center | CHAR(10) | PRCTR |
| /SKN/S_SW_10_03_OPEN_PR_DET | PRCTR_DESC | Long Text | CHAR(40) | LTEXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | PROCSTAT | Purch. doc. proc. state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_OPEN_PR_DET | PSTYP | Item Category | CHAR(1) | PSTYP |
| /SKN/S_SW_10_03_OPEN_PR_DET | PS_PSP_PNR | WBS Element | NUMC(8) | PS_PSP_PNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | SAKTO | G/L Account | CHAR(10) | SAKNR |
| /SKN/S_SW_10_03_OPEN_PR_DET | SHKZG | Debit/Credit Ind. | CHAR(1) | SHKZG |
| /SKN/S_SW_10_03_OPEN_PR_DET | STATU | Status | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_OPEN_PR_DET | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_OPEN_PR_DET | TXZ01 | Short Text | CHAR(40) | TXZ01 |
| /SKN/S_SW_10_03_OPEN_PR_DET | UEBTK | Unltd Overdelivery | CHAR(1) | UEBTK |
| /SKN/S_SW_10_03_OPEN_PR_DET | UEBTO | Overdeliv. Tolerance | DEC(3) | UEBTO |
| /SKN/S_SW_10_03_OPEN_PR_DET | VBELN | SD Document | CHAR(10) | VBELN_CO |
| /SKN/S_SW_10_03_OPEN_PR_DET | VBELP | Item | NUMC(6) | POSNR_CO |
| /SKN/S_SW_10_03_OPEN_PR_DET | VBUND | Trading Partner | CHAR(6) | RASSC |
| /SKN/S_SW_10_03_OPEN_PR_DET | VGABE | Trans./event type | CHAR(1) | VGABE |
| /SKN/S_SW_10_03_OPEN_PR_DET | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_OPEN_PR_DET | WEMNG | Qty Delivered | QUAN(13) | WEEMG |
| /SKN/S_SW_10_03_OPEN_PR_DET | WEPOS | Goods Receipt | CHAR(1) | WEPOS |
| /SKN/S_SW_10_03_OPEN_PR_DET | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_OPEN_PR_DET | WGBEZ | Material Group Desc. | CHAR(20) | WGBEZ |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_OPEN_PR_DET .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_OPEN_PR_DET OPTIONAL
*"----------------------------------------------------------------------
*TYPES: BEGIN OF ty_eket,
*         ebeln TYPE eket-ebeln,
*         ebelp TYPE eket-ebelp,
*         etenr TYPE eket-etenr,
*         eindt TYPE eket-eindt,
*         menge TYPE eket-menge,
*         wemng TYPE eket-wemng,
*         banfn TYPE eket-banfn,
*         bnfpo TYPE eket-bnfpo,
*         estkz TYPE eket-estkz,
*       END OF ty_eket,
*       tt_eket TYPE STANDARD TABLE OF ty_eket.
  DATA: BEGIN OF EKKO.
          INCLUDE STRUCTURE EKKO.
  DATA:   EBELP TYPE EKPO-EBELP,
          MATNR TYPE EKPO-MATNR,
          VBUND TYPE LFA1-VBUND.
  DATA: END OF EKKO.
  DATA: LS_EKKO LIKE EKKO.
  DATA: LT_EKKO LIKE TABLE OF EKKO.
  DATA: LT_EKKN TYPE TABLE OF EKKN.
  DATA: LS_EKKN TYPE EKKN.
DATA_SINGLE: LANGU          LANGU,
             BACKDAYS       INT4,
             DATE_REF_FLD   NAME_FELD,
             ELIKZ          ELIKZ,
             LOEKZ          ELOEK,
             WEPOS          WEPOS,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
*
*
 LV_BACKDAYS      = 10.
 LV_DURATION_UNIT = 'D'.
 LV_DATE_REF_FLD  = 'BEDAT'. "PO date
 LV_ELIKZ         = SPACE.
 LV_LOEKZ         = SPACE.
 LV_WEPOS         = 'X'.
*
 SELECT_SINGLE: LANGU,
                BACKDAYS,
                DATE_REF_FLD,
                DURATION_UNIT.
*
*
DATA_MULTY: EBELN           EBELN,
            EBELP           EBELP,
            FRGKZ           FRGKZ,
            FRGKE           FRGKE,
            BUKRS           BUKRS,
            BSART           BSART,
            LOEKZ           ELOEK,
            STATU           ESTAK,
            AEDAT           ERDAT,
            ERNAM           ERNAM,
            LIFNR           ELIFN,
            EKORG           EKORG,
            EKGRP           BKGRP,
            WAERS           WAERS,
            MATNR           MATNR,
            WERKS           EWERK,
            MATKL           MATKL,
            KNTTP           KNTTP,
            BWTAR           BWTAR_D,
            BWTTY           BWTTY_D,
            ELIKZ           ELIKZ,
            EREKZ           EREKZ,
            PSTYP           PSTYP,
            FIPOS           FIPOS,
            WEPOS           WEPOS,
            BEDAT           ETBDT,
            EINDT           EINDT,
            BANFN           BANFN,
            BNFPO           BNFPO,
            ESTKZ           ESTKZ,
            VBUND           RASSC,
            UEBTO           UEBTO,
            UEBTK           UEBTK,
            SAKTO           SAKTO,
            GSBER           GSBER,
            KOSTL           KOSTL,
            VBELN           VBELN,
            VBELP           POSNR_VA,
            ANLN1           ANLN1,
            ANLN2           ANLN2,
            AUFNR           AUFNR,
            PRCTR           PRCTR,
            BADAT           BADAT,
            LFDAT           EINDT,
            ERDAT           AEDAT,
            PROCSTAT        MEPROCSTATE,
            WEMNG           WEEMG,
            OPEN_ORDER_QUAN OBMNG,
            PS_PSP_PNR      PS_PSP_PNR,
            DATUM           SY-DATUM,
            DURATION        /SKN/E_SW_DURATION.
*
SELECT_MULTY: EBELN,
              EBELP,
              FRGKZ,
              FRGKE,
              BUKRS,
              BSART,
              LOEKZ,
              STATU,
              AEDAT,
              ERNAM,
              LIFNR,
              EKORG,
              EKGRP,
              WAERS,
              MATNR,
              WERKS,
              MATKL,
              KNTTP,
              BWTAR,
              BWTTY,
              ELIKZ,
              EREKZ,
              PSTYP,
              FIPOS,
              WEPOS,
              BEDAT,
              BANFN,
              BNFPO,
              ESTKZ,
              VBUND,
              UEBTO,
              UEBTK,
              SAKTO,
              GSBER,
              KOSTL,
              VBELN,
              VBELP,
              ANLN1,
              ANLN2,
              AUFNR,
              PRCTR,
              BADAT,
              LFDAT,
              ERDAT,
              PROCSTAT,
              WEMNG,
              OPEN_ORDER_QUAN,
              PS_PSP_PNR,
              DATUM,
              DURATION.
*
CONVERT_MULTY: EBELN ALPHA,
               LIFNR ALPHA.
*
  DATA: DATE_FROM TYPE SY-DATUM.
  DATA: SY_TABIX LIKE SY-TABIX .
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: LV_WEMNG     TYPE EKET-WEMNG,
        LV_MENGE     TYPE EKET-MENGE,
        LV_DOMNAME   TYPE DD07V-DOMNAME,
        LV_DOMVALUE  TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT    TYPE DD07V-DDTEXT,
        LV_VAL_TMP1  TYPE P DECIMALS 3,
        LV_VAL_TMP2  TYPE P DECIMALS 3.
  DATA: LS_DATA LIKE LINE OF T_DATA[].
  FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF T_DATA[],
                       TYPE ANY.
*
*"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_OPEN_PR_DET'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
*
** Initial Date
   IF R_DATUM[] IS INITIAL .
     RS_DATUM-SIGN   = 'I' .
     RS_DATUM-OPTION = 'GT' .
     DATE_FROM       = SY-DATUM - LV_BACKDAYS.
     RS_DATUM-LOW    = DATE_FROM.
*     rs_datum-high   = sy-datum.
     APPEND RS_DATUM TO R_DATUM.
   ENDIF.
*
  IF R_ELIKZ[] IS INITIAL.
    REFRESH: R_ELIKZ[].
    CLEAR RS_ELIKZ.
    RS_ELIKZ-SIGN   = 'I'.
    RS_ELIKZ-OPTION = 'EQ'.
    RS_ELIKZ-LOW    = LV_ELIKZ.
    APPEND RS_ELIKZ TO R_ELIKZ[].
  ENDIF.
  IF R_LOEKZ[] IS INITIAL.
    REFRESH: R_LOEKZ[].
    CLEAR RS_LOEKZ.
    RS_LOEKZ-SIGN   = 'I'.
    RS_LOEKZ-OPTION = 'EQ'.
    RS_LOEKZ-LOW    = LV_LOEKZ.
    APPEND RS_LOEKZ TO R_LOEKZ[].
  ENDIF.
  IF R_WEPOS[] IS INITIAL.
    REFRESH: R_WEPOS[].
    CLEAR RS_WEPOS.
    RS_WEPOS-SIGN   = 'I'.
    RS_WEPOS-OPTION = 'EQ'.
    RS_WEPOS-LOW    = LV_WEPOS.
    APPEND RS_WEPOS TO R_WEPOS[].
  ENDIF.
*
  IF LV_LANGU IS INITIAL.
    LV_LANGU = SY-LANGU.
  ENDIF.
*
* "--- Set Reference Date Field
   CASE LV_DATE_REF_FLD.
     WHEN 'BEDAT'.
       R_BEDAT[] = R_DATUM[]. " Purchasing document
     WHEN 'AEDAT'.
       R_AEDAT = R_DATUM[].   " Document created
     WHEN 'EINDT'.
       R_EINDT[] = R_DATUM[]. " Item Delivery
     WHEN 'ERDAT'.
       R_ERDAT[] = R_DATUM[].
     WHEN 'BADAT'.
       R_BADAT[] = R_DATUM[].
     WHEN OTHERS.
       R_BEDAT[] = R_DATUM[]. " Purchasing document
   ENDCASE.
*
**--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
*
  SELECT *
    FROM EKET AS E LEFT JOIN EKKN AS KN ON E~EBELN EQ KN~EBELN
                                        AND E~EBELP EQ KN~EBELP
                   INNER JOIN EBAN AS A ON  E~BANFN EQ A~BANFN
                                        AND E~BNFPO EQ A~BNFPO
*                   INNER JOIN ekko AS k       ON  e~ebeln EQ k~ebeln
*                   INNER JOIN ekpo AS p       ON  e~ebeln EQ p~ebeln
*                                              AND e~ebelp EQ p~ebelp
*                   INNER JOIN lfa1 AS l ON  k~lifnr EQ l~lifnr
    INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
    WHERE E~BANFN IN R_BANFN[]
    AND   E~BNFPO IN R_BNFPO[]
    AND   E~EBELN IN R_EBELN[]
    AND   E~EBELP IN R_EBELP[]
    AND   E~EINDT IN R_EINDT[]
    AND   E~WEMNG IN R_WEMNG[]
    AND   A~FRGKZ IN R_FRGKZ[]
    AND   A~ESTKZ IN R_ESTKZ[]
    AND   A~ERDAT IN R_ERDAT[]
    AND   A~BADAT IN R_BADAT[]
    AND   A~LFDAT IN R_LFDAT[]
    AND   A~LOEKZ IN R_LOEKZ[]
    AND   A~ERNAM IN R_ERNAM[].
*    AND   k~bukrs IN r_bukrs[]
*    AND   k~bsart IN r_bsart[]
*    AND   k~loekz IN r_loekz[]
*    AND   k~statu IN r_statu[]
*    AND   k~aedat IN r_aedat[]
*    AND   k~bedat IN r_bedat[]
*    AND   k~ernam IN r_ernam[]
*    AND   k~lifnr IN r_lifnr[]
*    AND   k~ekorg IN r_ekorg[]
*    AND   k~ekgrp IN r_ekgrp[]
*    AND   k~frgke IN r_frgke[]
*    AND   k~procstat IN r_procstat[]
*    AND   p~matnr IN r_matnr[]
*    AND   p~werks IN r_werks[]
*    AND   p~matkl IN r_matkl[]
*    AND   p~knttp IN r_knttp[]
*    AND   p~bwtar IN r_bwtar[]
*    AND   p~bwtty IN r_bwtty[]
*    AND   p~elikz IN r_elikz[]
*    AND   p~erekz IN r_erekz[]
*    AND   p~pstyp IN r_pstyp[]
*    AND   p~fipos IN r_fipos[]
*    AND   p~wepos IN r_wepos[]
*    AND   p~loekz IN r_loekz[]
*    AND   p~uebto IN r_uebto[]
*    AND   p~uebtk IN r_uebtk[]
*    AND   l~vbund IN r_vbund[].
  CHECK T_DATA[] IS NOT INITIAL.
  DELETE T_DATA[] WHERE SAKTO      NOT IN R_SAKTO.
  DELETE T_DATA[] WHERE GSBER      NOT IN R_GSBER.
  DELETE T_DATA[] WHERE KOSTL      NOT IN R_KOSTL.
  DELETE T_DATA[] WHERE VBELN      NOT IN R_VBELN.
  DELETE T_DATA[] WHERE VBELP      NOT IN R_VBELP.
  DELETE T_DATA[] WHERE ANLN1      NOT IN R_ANLN1.
  DELETE T_DATA[] WHERE ANLN2      NOT IN R_ANLN2.
  DELETE T_DATA[] WHERE AUFNR      NOT IN R_AUFNR.
  DELETE T_DATA[] WHERE PRCTR      NOT IN R_PRCTR.
  DELETE T_DATA[] WHERE PS_PSP_PNR NOT IN R_PS_PSP_PNR.
  IF T_DATA[] IS NOT INITIAL.
    SORT T_DATA[] BY EBELN EBELP.
    SELECT *
      FROM EKKO AS K INNER JOIN EKPO AS P ON K~EBELN EQ P~EBELN
                     INNER JOIN LFA1 AS L ON K~LIFNR EQ L~LIFNR
      INTO CORRESPONDING FIELDS OF TABLE LT_EKKO
      FOR ALL ENTRIES IN T_DATA[]
      WHERE P~EBELN EQ T_DATA-EBELN
      AND   P~EBELP EQ T_DATA-EBELP
      AND   P~MATNR IN R_MATNR[]
      AND   P~WERKS IN R_WERKS[]
      AND   P~MATKL IN R_MATKL[]
      AND   P~KNTTP IN R_KNTTP[]
      AND   P~BWTAR IN R_BWTAR[]
      AND   P~BWTTY IN R_BWTTY[]
*      AND   p~elikz IN r_elikz[]
      AND   P~EREKZ IN R_EREKZ[]
      AND   P~PSTYP IN R_PSTYP[]
      AND   P~FIPOS IN R_FIPOS[]
      AND   P~WEPOS IN R_WEPOS[]
*      AND   p~loekz IN r_loekz[]
      AND   P~UEBTO IN R_UEBTO[]
      AND   P~UEBTK IN R_UEBTK[]
      AND   K~BUKRS IN R_BUKRS[]
      AND   K~BSART IN R_BSART[]
*      AND   k~loekz IN r_loekz[]
      AND   K~STATU IN R_STATU[]
      AND   K~AEDAT IN R_AEDAT[]
      AND   K~BEDAT IN R_BEDAT[]
*      AND   k~ernam IN r_ernam[]
      AND   K~LIFNR IN R_LIFNR[]
      AND   K~EKORG IN R_EKORG[]
      AND   K~EKGRP IN R_EKGRP[]
      AND   K~FRGKE IN R_FRGKE[]
      AND   K~PROCSTAT IN R_PROCSTAT[]
      AND   L~VBUND IN R_VBUND[].
  ENDIF.
  IF LT_EKKO IS NOT INITIAL.
    SORT LT_EKKO BY EBELN EBELP.
  ENDIF.
***********************************************************************************
*
**-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    CHECK FLD IS NOT INITIAL.
    ASSIGN (FLD) TO .
    CHECK  IS ASSIGNED.
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
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION.
*******************************************************************************
*  CLEAR: sy_tabix.
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    CLEAR: LS_EKKO.
*
    SY_TABIX = SY-TABIX.
*    CLEAR: lv_menge, lv_wemng, lv_val_tmp1, lv_val_tmp2.
*
*    LOOP AT lt_eket INTO ls_eket WHERE ebeln EQ <fs_data>-ebeln
*                                 AND   ebelp EQ <fs_data>-ebelp.
**                                 AND   eindt EQ <fs_data>-eindt.
*
*        lv_menge     = lv_menge + ls_eket-menge.
*        lv_wemng     = lv_wemng + ls_eket-wemng.
*
*    ENDLOOP.
    READ TABLE LT_EKKO INTO LS_EKKO WITH KEY EBELN = <FS_DATA>-EBELN
                                             EBELP = <FS_DATA>-EBELP
                                             BINARY SEARCH.
    IF SY-SUBRC = 0 .
      <FS_DATA>-LIFNR = LS_EKKO-LIFNR.
      <FS_DATA>-PROCSTAT = LS_EKKO-PROCSTAT.
      <FS_DATA>-STATU    = LS_EKKO-STATU.
      <FS_DATA>-AEDAT    = LS_EKKO-AEDAT.
      <FS_DATA>-BEDAT    = LS_EKKO-BEDAT.
      <FS_DATA>-VBUND    = LS_EKKO-VBUND.
    ELSE.
      DELETE T_DATA INDEX SY_TABIX.
      CONTINUE.
    ENDIF.
    IF <FS_DATA>-MENGE > <FS_DATA>-WEMNG.
      <FS_DATA>-OPEN_ORDER_QUAN = <FS_DATA>-MENGE - <FS_DATA>-WEMNG.
    ELSE.
      <FS_DATA>-OPEN_ORDER_QUAN = 0.
    ENDIF.
    IF R_OPEN_ORDER_QUAN[] IS NOT INITIAL.
      IF NOT <FS_DATA>-OPEN_ORDER_QUAN IN R_OPEN_ORDER_QUAN[].
        DELETE T_DATA INDEX SY_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.
*    READ TABLE lt_ekkn INTO ls_ekkn WITH KEY ebeln = <fs_data>-ebeln
*                                             ebelp = <fs_data>-ebelp
*                                             BINARY SEARCH.
*    IF sy-subrc = 0.
*      IF ls_ekkn-sakto IN r_sakto[] OR r_sakto[] IS INITIAL.
*        <fs_data>-sakto = ls_ekkn-sakto.
*      ELSE.
*        DELETE t_data WHERE sakto      NOT IN r_sakto.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-gsber IN r_gsber[] OR r_gsber[] IS INITIAL.
*        <fs_data>-gsber = ls_ekkn-gsber.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-kostl IN r_kostl[] OR r_kostl[] IS INITIAL.
*        <fs_data>-kostl = ls_ekkn-kostl.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-vbeln IN r_vbeln[] OR r_vbeln[] IS INITIAL.
*        <fs_data>-vbeln = ls_ekkn-vbeln.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-vbelp IN r_vbelp[] OR r_vbelp[] IS INITIAL.
*        <fs_data>-vbelp = ls_ekkn-vbelp.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-anln1 IN r_anln1[] OR r_anln1[] IS INITIAL.
*        <fs_data>-anln1 = ls_ekkn-anln1.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-anln2 IN r_anln2[] OR r_anln2[] IS INITIAL.
*        <fs_data>-anln2 = ls_ekkn-anln2.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-aufnr IN r_aufnr[] OR r_aufnr[] IS INITIAL.
*        <fs_data>-aufnr = ls_ekkn-aufnr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-prctr IN r_prctr[] OR r_prctr[] IS INITIAL.
*        <fs_data>-prctr = ls_ekkn-prctr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-ps_psp_pnr IN r_ps_psp_pnr[] OR r_ps_psp_pnr[] IS INITIAL.
*        <fs_data>-ps_psp_pnr = ls_ekkn-ps_psp_pnr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*    ENDIF.
*    IF lv_menge <= lv_wemng.
*      DELETE t_data[] INDEX sy_tabix.
*      CONTINUE.
*    ENDIF.
*
*    <fs_data>-waers_local     = <fs_data>-waers.
*    <fs_data>-menge           = lv_menge.
*    <fs_data>-wemng           = lv_wemng.
*    <fs_data>-open_order_quan = lv_menge - lv_wemng.
*
*    IF <fs_data>-bpumn <> 0 AND <fs_data>-peinh <> 0.
*      lv_val_tmp1 = <fs_data>-netpr * ( <fs_data>-open_order_quan ).
*      lv_val_tmp2 = ( <fs_data>-bpumz / <fs_data>-bpumn ) /
*                      <fs_data>-peinh.
*
*      <fs_data>-open_value  = lv_val_tmp1 * lv_val_tmp2.
*    ELSE.
*      <fs_data>-open_value = 0.
*    ENDIF.
    IF <FS_DATA>-SAKTO IS NOT INITIAL AND <FS_DATA>-BUKRS IS NOT INITIAL.
* G/L Account Description
    DATA: LV_ACC_DESC TYPE  TXT20_SKAT.
      CALL FUNCTION '/SKN/F_SW_10_SAKTO_DESC'
        EXPORTING
          SPRAS            = SY-LANGU
          BUKRS            = <FS_DATA>-BUKRS
*         KTOPL            =
          SAKNR            = <FS_DATA>-SAKTO
       IMPORTING
         ACC_DESC         = LV_ACC_DESC
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-GL_ACC_TXT = LV_ACC_DESC.
    ENDIF.
    IF <FS_DATA>-KOSTL IS NOT INITIAL AND <FS_DATA>-KOKRS IS NOT INITIAL.
* Cost Center Description
     DATA: LV_KTEXT TYPE  KTEXT.
      CALL FUNCTION '/SKN/F_SW_10_KOSTL_DESC'
        EXPORTING
         SPRAS           = SY-LANGU
         KOKRS           = <FS_DATA>-KOKRS
         KOSTL           = <FS_DATA>-KOSTL
       IMPORTING
         KOSTL_DESC      = LV_KTEXT
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-KOSTL_DESC = LV_KTEXT.
    ENDIF.
    IF <FS_DATA>-PRCTR IS NOT INITIAL AND <FS_DATA>-KOKRS IS NOT INITIAL.
* Profit Center Description
    DATA: LVV_KTEXT TYPE  KTEXT.
      CALL FUNCTION '/SKN/F_SW_10_PRCTR_DESC'
        EXPORTING
         SPRAS            = SY-LANGU
         PRCTR            = <FS_DATA>-PRCTR
         KOKRS            = <FS_DATA>-KOKRS
       IMPORTING
         KTEXT            = LVV_KTEXT
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-PRCTR_DESC = LVV_KTEXT.
    ENDIF.
    IF <FS_DATA>-MATKL IS NOT INITIAL.
* Material group desc.
      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
      EXPORTING
        MATKL              = <FS_DATA>-MATKL
*       LANGU              = SY-LANGU
      IMPORTING
        MATKL_DESC         = <FS_DATA>-WGBEZ
*       MATKL_DESC_L       =
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2
        .
    ENDIF.
**
    IF <FS_DATA>-BSART IS NOT INITIAL AND <FS_DATA>-BSTYP IS NOT INITIAL.
*    "-- BSART_DESC
      CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART            = <FS_DATA>-BSART
        LANGU            = LV_LANGU
        BSTYP            = <FS_DATA>-BSTYP
      IMPORTING
        TYPE_DESC        = <FS_DATA>-BATXT
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    ENDIF.
*
    IF <FS_DATA>-STATU IS NOT INITIAL.
      "-- STATU_DESC
      LV_DOMNAME = 'ESTAK'.
      LV_DOMVALUE = <FS_DATA>-STATU.
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
        <FS_DATA>-STATU_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
**
    IF <FS_DATA>-BSTYP IS NOT INITIAL.
*    "-- BSTYP_DESC
      LV_DOMNAME = 'EBSTYP'.
      LV_DOMVALUE = <FS_DATA>-BSTYP.
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
        <FS_DATA>-BSTYP_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
**
    IF <FS_DATA>-LIFNR IS NOT INITIAL.
*    "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR              = <FS_DATA>-LIFNR
      IMPORTING
        VENDOR_DESC        = <FS_DATA>-NAME1
      EXCEPTIONS
        WRONG_VENDOR       = 1
        OTHERS             = 2.
    ENDIF.
**
    IF <FS_DATA>-EKORG IS NOT INITIAL.
*   "-- EKORG_DESC
      CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG              = <FS_DATA>-EKORG
        "LANGU              = lv_LANGU
      IMPORTING
        PUR_ORG_DESC       = <FS_DATA>-EKOTX
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    ENDIF.
**
**
    IF <FS_DATA>-EKGRP IS NOT INITIAL.
*   "-- EKGRP_DESC
      CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP              = <FS_DATA>-EKGRP
*       LANGU              = lv_LANGU
      IMPORTING
        PUR_GRP_DESC       = <FS_DATA>-EKNAM
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    ENDIF.
*
  ENDLOOP.
*
**
***--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
