# Exception Indicator: Purchase order line lead time ( SW_10_03_PO_LEADTIME)

## General Overview

This Exception Indicator identifies purchase order lines where actual lead time—the elapsed days between line creation and goods receipt—deviates from the planned delivery time maintained in material master data, using purchasing history and schedule-line context to surface late or unusually fast receipts.

This EI serves as an essential control for procurement and supply performance by:

- Enabling detection of goods receipts that took longer than the master-planned delivery time for the material and plant
- Supporting identification of receipts that completed faster than planned for vendor performance or master-data review
- Providing visibility into open quantity and delivery value alongside lead-time results for prioritization
- Enabling age-based review of how long ago goods were posted relative to a chosen reference date
- Supporting segmentation by vendor, plant, material, and purchasing organization for targeted buyer and planner action

Typical use includes vendor performance reviews, master-data validation of planned delivery times, and periodic sampling of receipt timeliness before close. Results are intended for exception workflows rather than operational MM list reporting.

The routine reads purchasing history joined to order header, item, schedule line, material-plant, and vendor data, calculates actual lead time per line, compares it to planned delivery time when configured, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor purchase order line lead time against planned delivery expectations creates multiple risks across procurement performance, inventory planning, and master-data quality:

**Procurement and Supply Risks**

- Receipts that exceed planned delivery time can delay production, projects, or replenishment when buyers are not alerted to late vendor performance
- Lines where actual lead time is much shorter than planned can mask outdated planned delivery times in material master data
- Undetected lead-time variance across vendors or plants can concentrate supply risk on critical materials without structured follow-up

**Operational Risks**

- Goods-receipt scope that is too broad can mix irrelevant movement types into lead-time analysis
- Lookback windows misaligned with review cadence can exclude recent receipts or retain rows outside the intended monitoring period
- Lead-time and comparison-mode settings that are not tuned can produce noise or miss actionable late-delivery cases

**Control and Audit Risks**

- Weak monitoring reduces evidence that vendor delivery performance was reviewed against master planning data
- Outdated planned delivery times weaken MRP and buyer commitment timing when not corrected after repeated exceptions
- Lack of recurring exception review limits accountability for master-data maintenance and vendor escalation

## Suggested Resolution

**Immediate Response**

- Review flagged purchase orders for vendor, material, plant, planned delivery time, and calculated actual lead time
- Confirm goods receipt dates and order line creation context with the responsible buyer or receiving team
- Prioritize high-value, critical-material, or severely late lines for vendor contact or internal receipt investigation

**System Assessment**

- Validate lookback window and comparison mode against how the team defines late versus early receipt performance
- Tune organizational, vendor, and material scope so results stay actionable for buyers and planners
- Compare exception counts by vendor, plant, and material group to find systematic lead-time or master-data gaps

**Corrective Actions**

- Update planned delivery times in material master where repeated exceptions show master data is outdated
- Escalate recurring late deliveries with vendors through standard procurement processes
- Document review outcomes and schedule recurring runs for critical vendors, plants, or material groups


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT_EKKO | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | AEDAT_EKPO | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 3 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 4 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 5 | BATXT | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 6 | BEDAT | Purchase Order Date | DATS | 8 | 0 | ETBDT | DATUM |
| 7 | BNFPO | Item of Requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 8 | BPUMN | Quantity Conversion | DEC | 5 | 0 | BPUMN | UMBSN |
| 9 | BPUMZ | Quantity Conversion | DEC | 5 | 0 | BPUMZ | UMBSZ |
| 10 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 11 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | BSTYP | BSTYP |
| 12 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 13 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 14 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 15 | BWTTY | Valuation Category | CHAR | 1 | 0 | BWTTY_D | BWTTY |
| 16 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 17 | DELIVERY_VALUE | Delivery Value | CURR | 11 | 2 |  |  |
| 18 | DURATION | Duration In Days | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 19 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 20 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 21 | EBELP | Item | NUMC | 5 | 0 | EBELP | EBELP |
| 22 | EINDT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 23 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 24 | EKNAM | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 25 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 26 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 27 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 28 | EREKZ | Final Invoice | CHAR | 1 | 0 | EREKZ | XFELD |
| 29 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 30 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 31 | ETENR | Schedule Line Number | NUMC | 4 | 0 | ETENR | ETENR |
| 32 | FIPOS | Commitment Item | CHAR | 14 | 0 | FIPOS | FIPOS |
| 33 | GJAHR | Material Doc. Year | NUMC | 4 | 0 | MJAHR | GJAHR |
| 34 | KNTTP | Acct Assignment Cat. | CHAR | 1 | 0 | KNTTP | KNTTP |
| 35 | LANG_REF_FLD | Language Ref. Field |  | 0 | 0 |  |  |
| 36 | LEADTIME | Lead Time | INT4 | 10 | 0 | INT4 | INT4 |
| 37 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 38 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 39 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 40 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 41 | MD_CHECK | Actual LT vs MD del. time(G/L) |  | 0 | 0 |  |  |
| 42 | MEINS | Order Unit | UNIT | 3 | 0 | BSTME | MEINS |
| 43 | MENGE | Scheduled Quantity | QUAN | 13 | 3 | ETMEN | MENGE |
| 44 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 45 | NETPR | Net Order Price | CURR | 11 | 2 | BPREI | WERT11 |
| 46 | NETWR | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 47 | OPEN_ORDER_QUAN | Open Quantity | QUAN | 13 | 3 | OBMNG | MENG13 |
| 48 | OPEN_VALUE | Open Order Value | CURR | 11 | 2 |  |  |
| 49 | PEINH | Price Unit | DEC | 5 | 0 | EPEIN | DEC5 |
| 50 | PLIFZ | Planned Deliv. Time | DEC | 3 | 0 | PLIFZ | DEC3 |
| 51 | PSTYP | Item Category | CHAR | 1 | 0 | PSTYP | PSTYP |
| 52 | SHKZG | Debit/Credit Ind. | CHAR | 1 | 0 | SHKZG | SHKZG |
| 53 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 54 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 55 | TXZ01 | Short Text | CHAR | 40 | 0 | TXZ01 | TEXT40 |
| 56 | VBUND | Trading Partner | CHAR | 6 | 0 | RASSC | RCOMP |
| 57 | VGABE | Trans./event type | CHAR | 1 | 0 | VGABE | VGABE |
| 58 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 59 | WAERS_LOCAL | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 60 | WEMNG | Qty Delivered | QUAN | 13 | 3 | WEEMG | MENG13 |
| 61 | WEPOS | Goods Receipt | CHAR | 1 | 0 | WEPOS | XFELD |
| 62 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 63 | WGBEZ | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 63 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT_EKKO** (Created on)

Guards against oversized extracts when created on on AEDAT_EKKO is narrowed together with client, user, or session filters.

**AEDAT_EKPO** (Created on)

Explains why two monitoring passes differ: only the pass with stricter created on on AEDAT_EKPO surfaces the disputed rows.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on BEDAT

**BANFN** (Purchase Requisition)

Purchase requisition number, the core PR document key for MM approval and lifecycle checks.

**BATXT** (Doc. Type Descript.)

Description of Purchasing Document Type provides the short text description for custom or standard purchasing codes (such as "Standard PO" for the NB document type).

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

**DELIVERY_VALUE** (Delivery Value)

When left open per framework rules, DELIVERY_VALUE does not restrict delivery value; when set, only matching rows remain.

**DURATION** (Duration In Days)

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

Description of Purchasing Organisation provides the short text description for custom or standard purchasing organizational units (such as "North America Procurement" for the US01 purchasing organization).

**ELIKZ** (Delivery Completed)

Delivery completed indicator used to identify open versus completed procurement items.

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

**GJAHR** (Material Doc. Year)

<mark>Calendar year.</mark>

**KNTTP** (Acct Assignment Cat.)

Account assignment category on purchasing items telling whether stock is project, asset, cost-center, or sales-order.

**LANG_REF_FLD** (Language Ref. Field)

Pairs with duration logic: once LANG_REF_FLD passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**Not in use**
**LEADTIME** (Lead Time)

Calculated day count between purchase order line creation and goods-receipt entry date; filters lines by actual lead-time thresholds.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MD_CHECK** (Actual LT vs MD del. time(G/L))

Selects how calculated actual lead time is compared to the material master planned delivery time: greater-than mode flags receipts slower than planned; less-than mode flags receipts faster than planned.

**MD_CHECK Options:**
- G: Actual lead time greater than planned delivery time (late receipt)
- L: Actual lead time less than planned delivery time (faster than planned)

**MEINS** (Order Unit)

Base unit of measure used to interpret quantity fields consistently.

**MENGE** (Scheduled Quantity)

Quantity field used for volumetric thresholds and variance analysis.

**NAME1** (Name)

When populated, keeps the extract focused so name (NAME1) aligns with the intended triage slice.

**NETPR** (Net Order Price)

Mirrors how administrators slice operational lists: net order price (NETPR) is one lever that shapes which rows are comparable run over run.

**NETWR** (Net Order Value)

Net value amount used for commercial threshold and anomaly checks.

**OPEN_ORDER_QUAN** (Open Quantity)

<mark>Remaining receipt quantity on each schedule line: scheduled quantity minus quantity already delivered, or zero when nothing is left open. It is written to the output and can also be used as a filter when a selection range is supplied.</mark>

**OPEN_VALUE** (Open Order Value)

When left open per framework rules, OPEN_VALUE does not restrict open order value; when set, only matching rows remain.

**PEINH** (Price Unit)

Price unit denominator used to interpret per-unit purchasing prices.

**PLIFZ** (Planned Deliv. Time)

Planned delivery time in days from purchasing info records or schedule lines for lead-time analytics.

**PSTYP** (Item Category)

Purchasing document item category controlling item behavior, account assignment, and goods-receipt rules.

**SHKZG** (Debit/Credit Ind.)

Debit/Credit indicator used to separate accounting posting direction.

**SHKZG Options:**
- S: Debit posting
- H: Credit posting

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**STATU_DESC** (Short Descript.)

Status description text used for readable status analytics.

**TXZ01** (Short Text)

Description of Short Text provides the readable name or detailed line-item description for a material, service, or component within a purchasing, sales, or production document (such as "Standard 10mm Steel Bolt" for an inventory item).

**VBUND** (Trading Partner)

Trading partner/company field used for intercompany transaction analysis.

**VGABE** (Trans./event type)

Transaction/event type in purchasing history used to classify movement category.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WAERS_LOCAL** (Currency)

Documents expected operator behavior—currency on WAERS_LOCAL should be set when that dimension is part of the control objective.

**WEMNG** (Qty Delivered)

Goods-receipt quantity on purchasing history or order-related rows for GR-versus-PO variance checks.

**WEPOS** (Goods Receipt)

Goods-receipt indicator on purchasing history rows marking lines created by goods receipt postings.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WGBEZ** (Material Group Desc.)

Material group description used for readable category reporting.

### Parameter Relationships

**Goods-receipt date window:** When no explicit goods-receipt date range is supplied, a lower bound of today minus **BACKDAYS** is applied with a greater-than-or-equal filter on **CPUDT** (entry date on the purchasing history row). Explicit **CPUDT** selections override that fallback window.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference timestamp to the evaluation date; rows outside the configured duration range are removed.

**Purchasing history scope:** **SHKZG** and **VGABE** restrict which purchasing history movement types are read; defaults target debit goods-receipt postings.

**Actual lead time:** For each purchase order line, actual lead time is the day count between line creation date and goods-receipt entry date; **LEADTIME** filters that calculated value.

**Master comparison:** When **MD_CHECK** is set, **LEADTIME** results are compared to **PLIFZ** (planned delivery time from material master): **G** retains lines where actual lead time exceeds planned delivery time; **L** retains lines where actual lead time is less than planned delivery time.

**Header and item scope:** **EBELN**, **EBELP**, **BUKRS**, **BSART**, **BSTYP**, **LOEKZ**, **LIFNR**, **EKORG**, **EKGRP**, **MATNR**, **WERKS**, **MATKL**, **KNTTP**, **PSTYP**, and **VBUND** combine to define which purchase order lines and history rows enter the result set.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **SHKZG** - S
- **VGABE** - 1

### Practical Example of Parameter Configuration

**Use Case 1: Late receipts in the last thirty days**

**Purpose:** Flag purchase order lines where actual lead time exceeds the material master planned delivery time for goods receipts posted in the last thirty days.

```
BACKDAYS = 30
MD_CHECK = G
BUKRS = 1000
EKORG = 1000
```

**Use Case 2: Vendor lead-time performance**

**Purpose:** Review one vendor's receipt lead times against master planned delivery time for targeted supplier discussion.

```
LIFNR = 100000
MD_CHECK = G
BACKDAYS = 60
PLIFZ = 5
```

**Use Case 3: Plant and material group focus**

**Purpose:** Monitor late lead times for one plant and material group combination.

```
WERKS = 1000
MATKL = 001
MD_CHECK = G
BACKDAYS = 45
BUKRS = 1000
```

**Use Case 4: Faster-than-planned receipts**

**Purpose:** Identify lines where goods were received faster than the master planned delivery time for master-data review.

```
MD_CHECK = L
BACKDAYS = 90
BSTYP = F
EKGRP = 001
```

**Use Case 5: Exactly seven full days since reference date**

**Purpose:** Return rows whose reference date is exactly 7 full days ago for weekly lead-time escalation.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
BUKRS = 1000
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | AEDAT_EKKO | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | AEDAT_EKPO | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BANFN | Purchase Requisition | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BATXT | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BEDAT | Purchase Order Date | DATS(8) | ETBDT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BNFPO | Item of Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BPUMN | Quantity Conversion | DEC(5) | BPUMN |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BPUMZ | Quantity Conversion | DEC(5) | BPUMZ |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BSTYP | Purch. Doc. Category | CHAR(1) | BSTYP |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | BWTTY | Valuation Category | CHAR(1) | BWTTY_D |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | CPUDT | Entry Date | DATS(8) | CPUDT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | DELIVERY_VALUE | Delivery Value | CURR(11) |  |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | EBELP | Item | NUMC(5) | EBELP |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | EINDT | Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | EKNAM | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | EKOTX | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | ELIKZ | Delivery Completed | CHAR(1) | ELIKZ |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | EREKZ | Final Invoice | CHAR(1) | EREKZ |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | ESTKZ | Creation Indicator | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | ETENR | Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | FIPOS | Commitment Item | CHAR(14) | FIPOS |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | GJAHR | Material Doc. Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | KNTTP | Acct Assignment Cat. | CHAR(1) | KNTTP |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | LEADTIME | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | MEINS | Order Unit | UNIT(3) | BSTME |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | MENGE | Scheduled Quantity | QUAN(13) | ETMEN |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | NETPR | Net Order Price | CURR(11) | BPREI |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | NETWR | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | OPEN_ORDER_QUAN | Open Quantity | QUAN(13) | OBMNG |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | OPEN_VALUE | Open Order Value | CURR(11) |  |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | PEINH | Price Unit | DEC(5) | EPEIN |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | PLIFZ | Planned Deliv. Time | DEC(3) | PLIFZ |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | PSTYP | Item Category | CHAR(1) | PSTYP |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | SHKZG | Debit/Credit Ind. | CHAR(1) | SHKZG |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | STATU | Status | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | TXZ01 | Short Text | CHAR(40) | TXZ01 |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | VBUND | Trading Partner | CHAR(6) | RASSC |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | VGABE | Trans./event type | CHAR(1) | VGABE |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | WAERS_LOCAL | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | WEMNG | Qty Delivered | QUAN(13) | WEEMG |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | WEPOS | Goods Receipt | CHAR(1) | WEPOS |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_PO_SH_LEAD_TIM | WGBEZ | Material Group Desc. | CHAR(20) | WGBEZ |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PO_SH_LEAD_TIM .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_SH_LEAD_TIM OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: LANGU          LANGU,
             BACKDAYS       INT4,
             LEADTIME       INT4,
             DATE_REF_FLD   NAME_FELD,
             SHKZG          SHKZG,
             VGABE          VGABE,
             MD_CHECK       CHAR1,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
* Default values
 LV_BACKDAYS      = 10.       " back days for GR date
 LV_SHKZG         = 'S'.      " debit/credit indic.
 LV_VGABE         = '1'.
 LV_DURATION_UNIT = 'D'. " duration unit
 LV_LANGU         = SY-LANGU.
 LV_DATE_REF_FLD  = 'CPUDT'. " GR date ref. field
 SELECT_SINGLE: LANGU,
                BACKDAYS,
                DATE_REF_FLD,
                MD_CHECK,
                DURATION_UNIT.
DATA_MULTY: EBELN         EBELN,
            EBELP         EBELP,
            BUKRS         BUKRS,
            BSART         BSART,
            LOEKZ         ELOEK,
            AEDAT_EKKO    ERDAT,
            AEDAT_EKPO    ERDAT,
            LIFNR         ELIFN,
            EKORG         EKORG,
            EKGRP         BKGRP,
            MATNR         MATNR,
            WERKS         EWERK,
            MATKL         MATKL,
            KNTTP         KNTTP,
            BWTAR         BWTAR_D,
            PSTYP         PSTYP,
            BEDAT         ETBDT,
            CPUDT         CPUDT,
            GJAHR         MJAHR,
            ZEKKN         DZEKKN,
            VGABE         VGABE,
            SHKZG         SHKZG,
            BANFN         BANFN,
            BNFPO         BNFPO,
            ESTKZ         ESTKZ,
            PLIFZ         PLIFZ,
            VBUND         RASSC,
            BSTYP         EBSTYP,
            DATUM         SY-DATUM,
            DATUM2        SY-DATUM,
            LEADTIME      INT4,
            DURATION      /SKN/E_SW_DURATION.
SELECT_MULTY: EBELN,
              EBELP,
              BUKRS,
              BSART,
              LOEKZ,
              AEDAT_EKKO,
              AEDAT_EKPO,
              LIFNR,
              EKORG,
              EKGRP,
              MATNR,
              WERKS,
              MATKL,
              KNTTP,
              BWTAR,
              PSTYP,
              BEDAT,
              CPUDT,
              GJAHR,
              ZEKKN,
              VGABE,
              SHKZG,
              BANFN,
              BNFPO,
              ESTKZ,
              PLIFZ,
              VBUND,
              BSTYP,
              DATUM,
              DATUM2,
              LEADTIME,
              DURATION.
CONVERT_MULTY: EBELN ALPHA,
               LIFNR ALPHA.
DATA: TIME_DIFF TYPE INT4.
DATA: FLD(60) TYPE C.
DATA: REF_DATE TYPE D.
DATA: DATE_FROM TYPE SY-DATUM.
DATA: SY_TABIX LIKE SY-TABIX .
DATA: LV_DAYS       TYPE I,
      LV_PLIFZ      TYPE MARC-PLIFZ,
      LV_TABIX      TYPE I,
      LV_CREAT_DATE LIKE EKKO-AEDAT,
      LV_MENGE      TYPE ETMEN,
      LV_WEMNG      TYPE WEEMG,
      LV_VAL_TMP1   TYPE P DECIMALS 3,
      LV_VAL_TMP2   TYPE P DECIMALS 3.
DATA: LS_DATA TYPE /SKN/S_SW_10_03_PO_SH_LEAD_TIM.
DATA: LT_DATA TYPE TABLE OF /SKN/S_SW_10_03_PO_SH_LEAD_TIM.
 FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF T_DATA[].
 FIELD-SYMBOLS:    TYPE ANY ,
                <FS_V> TYPE ANY .
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PO_SH_LEAD_TI'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
* Calc. GR date
   IF R_CPUDT[] IS INITIAL .
     RS_DATUM-SIGN   = 'I' .
     RS_DATUM-OPTION = 'GT' .
     DATE_FROM       = SY-DATUM - LV_BACKDAYS.
     RS_DATUM-LOW    = DATE_FROM.
*     rs_datum-high   = sy-datum.
     APPEND RS_DATUM TO R_DATUM.
      R_CPUDT[] = R_DATUM[].           "GR date
   ENDIF.
* Initial debit/credit indic.
  IF R_SHKZG IS INITIAL.
    RS_SHKZG-SIGN   = 'I'.
    RS_SHKZG-OPTION = 'EQ'.
    RS_SHKZG-LOW    = LV_SHKZG.
    APPEND RS_SHKZG TO R_SHKZG[].
  ENDIF.
* Initial Transaction type
  IF R_VGABE[] IS INITIAL.
    RS_VGABE-SIGN   = 'I'.
    RS_VGABE-OPTION = 'EQ'.
    RS_VGABE-LOW    = LV_VGABE.
    APPEND RS_VGABE TO R_VGABE[].
  ENDIF.
* Initial Lead Time
  IF LV_MD_CHECK IS NOT INITIAL.
    IF R_LEADTIME[] IS INITIAL AND LV_LEADTIME NE 0.
      RS_LEADTIME-SIGN = 'I'.
      IF LV_MD_CHECK EQ 'L'.
        RS_LEADTIME-OPTION = 'LT'.
      ELSEIF LV_MD_CHECK EQ 'G'.
        RS_LEADTIME-OPTION = 'GT'.
      ENDIF.
      RS_LEADTIME-LOW = LV_LEADTIME.
      APPEND RS_LEADTIME TO R_LEADTIME[].
    ENDIF.
  ENDIF.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT EKBE~EBELN EKBE~EBELP EKBE~ZEKKN EKBE~GJAHR
         EKBE~VGABE EKBE~SHKZG EKBE~CPUDT EKBE~MATNR EKBE~WERKS
         EKKO~BUKRS EKKO~BSTYP EKKO~LOEKZ EKKO~BSART
         EKKO~AEDAT AS AEDAT_EKKO EKKO~LIFNR EKKO~EKORG EKKO~EKGRP
         EKKO~WAERS EKKO~ERNAM
         EKPO~AEDAT AS AEDAT_EKPO
         EKPO~MATKL EKPO~KNTTP EKPO~BWTAR EKPO~PSTYP
         EKPO~BPUMZ EKPO~BPUMN EKPO~PEINH EKPO~NETWR EKPO~NETPR
         EKPO~TXZ01 EKPO~MEINS
         EKET~ETENR EKET~EINDT EKET~BEDAT EKET~MENGE EKET~WEMNG
         EKET~BANFN EKET~BNFPO EKET~ESTKZ
         MARC~PLIFZ
         LFA1~VBUND
        FROM EKBE INNER JOIN EKKO ON  EKBE~EBELN EQ EKKO~EBELN
                  INNER JOIN EKPO ON  EKBE~EBELN EQ EKPO~EBELN
                                  AND EKBE~EBELP EQ EKPO~EBELP
                  INNER JOIN EKET ON  EKBE~EBELN EQ EKET~EBELN
                                  AND EKBE~EBELP EQ EKET~EBELP
                  INNER JOIN MARC ON  EKBE~MATNR EQ MARC~MATNR
                                  AND EKBE~WERKS EQ MARC~WERKS
                  INNER JOIN LFA1 ON  EKKO~LIFNR EQ LFA1~LIFNR
        INTO CORRESPONDING FIELDS OF TABLE LT_DATA[]
        WHERE EKBE~EBELN IN R_EBELN[]
        AND   EKBE~VGABE IN R_VGABE[]
        AND   EKBE~ZEKKN IN R_ZEKKN[]
        AND   EKBE~GJAHR IN R_GJAHR[]
        AND   EKBE~WERKS IN R_WERKS[]
        AND   EKBE~BWTAR IN R_BWTAR[]
        AND   EKBE~SHKZG IN R_SHKZG[]
        AND   EKBE~CPUDT IN R_CPUDT[]
        AND   EKBE~MATNR IN R_MATNR[]
        AND   EKBE~WERKS IN R_WERKS[]
        AND   EKKO~EBELN IN R_EBELN[]
        AND   EKKO~BUKRS IN R_BUKRS[]
        AND   EKKO~BSTYP IN R_BSTYP[]
        AND   EKKO~LOEKZ IN R_LOEKZ[]
        AND   EKKO~BSART IN R_BSART[]
        AND   EKKO~AEDAT IN R_AEDAT_EKKO[]
        AND   EKKO~LIFNR IN R_LIFNR[]
        AND   EKKO~EKORG IN R_EKORG[]
        AND   EKKO~EKGRP IN R_EKGRP[]
        AND   EKPO~AEDAT IN R_AEDAT_EKPO[]
        AND   EKPO~MATKL IN R_MATKL[]
        AND   EKPO~KNTTP IN R_KNTTP[]
        AND   EKPO~BWTAR IN R_BWTAR[]
        AND   EKPO~PSTYP IN R_PSTYP[]
        AND   EKET~BEDAT IN R_BEDAT[]
        AND   EKET~BEDAT NE '00000000'
        AND   MARC~PLIFZ IN R_PLIFZ
        AND   LFA1~VBUND IN R_VBUND.
  CHECK LT_DATA[] IS NOT INITIAL.
  SORT LT_DATA[] BY EBELN EBELP ETENR.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING EBELN EBELP ETENR.
  SORT LT_DATA BY EBELN EBELP ETENR BEDAT.
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    CONCATENATE 'LS_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
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
          LS_DATA-DURATION  = TIME_DIFF .
        ELSE.
          LS_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY LT_DATA FROM LS_DATA. "INDEX sy_tabix.
    ENDIF.
  ENDLOOP.
  DELETE LT_DATA WHERE DURATION  NOT IN R_DURATION .
*  LOOP AT t_data ASSIGNING <fs_data>.
  LOOP AT LT_DATA ASSIGNING <FS_DATA>.
    LV_TABIX = SY-TABIX.
     AT NEW EBELN.
       CLEAR: LV_DAYS, LV_CREAT_DATE, LS_DATA, LV_MENGE, LV_WEMNG.
     ENDAT.
     AT NEW EBELP.
       CLEAR: LV_DAYS, LV_CREAT_DATE, LS_DATA, LV_MENGE, LV_WEMNG.
       MOVE-CORRESPONDING <FS_DATA> TO LS_DATA.
       IF <FS_DATA>-AEDAT_EKKO <> <FS_DATA>-AEDAT_EKPO.
         LV_CREAT_DATE = <FS_DATA>-BEDAT.
       ELSE.
         LV_CREAT_DATE = <FS_DATA>-AEDAT_EKKO. "ls_ekko-aedat_ekko.
       ENDIF.
       IF <FS_DATA>-CPUDT IS NOT INITIAL AND LV_CREAT_DATE IS NOT INITIAL.
         CALL FUNCTION 'C14B_DIFF_BT_2_DATES'
         EXPORTING
           I_DATE_FROM                     = LV_CREAT_DATE
           I_DATE_TO                       = <FS_DATA>-CPUDT
         IMPORTING
           E_DAYS                          = LV_DAYS
         EXCEPTIONS
           PLAUSIBILITY_CHECK_FAILED       = 1
           OTHERS                          = 2
            .
         IF SY-SUBRC <> 0.
           CONTINUE.
         ENDIF.
        LV_PLIFZ         = <FS_DATA>-PLIFZ.
        LS_DATA-LEADTIME = LV_DAYS.
*   Compare delivery time in MD to actual LT:
*   There are 2 options of the parameter G for greater then and L for less then.
*   Additional restriction could be done by LT.
*   LT is defined as number of days between PO line creation date and actual GR.
*         ls_data-leadtime = lv_days.
*         IF lv_days NOT IN r_leadtime[].
**           DELETE t_data[] INDEX lv_tabix.
*           CONTINUE.
*         ENDIF.
*
*         IF lv_md_check IS NOT INITIAL.
*           IF lv_md_check EQ 'G'.
*             IF <fs_data>-plifz >= lv_days.
*               CONTINUE.
*             ENDIF.
*           ELSEIF lv_md_check EQ 'L'.
*             IF <fs_data>-plifz <= lv_days.
*               CONTINUE.
*             ENDIF.
*           ENDIF.
*         ENDIF.
      ENDIF.
    ENDAT.
********************************************
*   Compare delivery time in MD to actual LT:
*   There are 2 options of the parameter G for greater then and L for less then.
*   Additional restriction could be done by LT.
*   LT is defined as number of days between PO line creation date and actual GR.
    IF LV_DAYS NOT IN R_LEADTIME[].
      CONTINUE.
    ENDIF.
    IF LV_MD_CHECK IS NOT INITIAL.
      IF LV_MD_CHECK EQ 'G'.
        IF LV_PLIFZ >= LV_DAYS.
          CONTINUE.
        ENDIF.
      ELSEIF LV_MD_CHECK EQ 'L'.
        IF LV_PLIFZ <= LV_DAYS.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
    LV_MENGE = LV_MENGE + <FS_DATA>-MENGE.
    LV_WEMNG = LV_WEMNG + <FS_DATA>-WEMNG.
    AT END OF EBELP.
      CLEAR: LV_VAL_TMP1, LV_VAL_TMP2.
      LS_DATA-WAERS_LOCAL     = <FS_DATA>-WAERS.
      LS_DATA-MENGE           = LV_MENGE.
      LS_DATA-WEMNG           = LV_WEMNG.
      LS_DATA-OPEN_ORDER_QUAN = LV_MENGE - LV_WEMNG.
* Open value calc.
      IF <FS_DATA>-BPUMN <> 0 AND <FS_DATA>-PEINH <> 0.
        LV_VAL_TMP1 = <FS_DATA>-NETPR * ( LS_DATA-OPEN_ORDER_QUAN ).
        LV_VAL_TMP2 = ( <FS_DATA>-BPUMZ / <FS_DATA>-BPUMN ) /
        <FS_DATA>-PEINH.
        LS_DATA-OPEN_VALUE  = LV_VAL_TMP1 * LV_VAL_TMP2.
      ELSE.
        LS_DATA-OPEN_VALUE = 0.
      ENDIF.
* Delivery value calc.
      IF <FS_DATA>-BPUMN <> 0 AND <FS_DATA>-PEINH <> 0.
        LV_VAL_TMP1 = <FS_DATA>-NETPR * LV_WEMNG.
        LV_VAL_TMP2 = ( <FS_DATA>-BPUMZ / <FS_DATA>-BPUMN ) /
        <FS_DATA>-PEINH.
        LS_DATA-DELIVERY_VALUE  = LV_VAL_TMP1 * LV_VAL_TMP2.
      ELSE.
        LS_DATA-DELIVERY_VALUE = 0.
      ENDIF.
      IF <FS_DATA>-MATKL IS NOT INITIAL.
* Material group desc.
        CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
        EXPORTING
          MATKL              = <FS_DATA>-MATKL
          LANGU              = LV_LANGU
        IMPORTING
          MATKL_DESC         = LS_DATA-WGBEZ
*       MATKL_DESC_L       =
        EXCEPTIONS
          WRONG_CODE         = 1
          OTHERS             = 2
          .
        IF SY-SUBRC <> 0.
*     Implement suitable error handling here
        ENDIF.
      ENDIF.
      IF <FS_DATA>-LIFNR IS NOT INITIAL.
        "--- Get  Vendor Decriptions
        CALL FUNCTION '/SKN/FC_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = <FS_DATA>-LIFNR
          SW_DEST      = LV_SW_DEST
        IMPORTING
          VENDOR_DESC  = LS_DATA-NAME1
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
      ENDIF.
      IF <FS_DATA>-BSART IS NOT INITIAL AND <FS_DATA>-BSTYP IS NOT INITIAL.
        "-- BSART_DESC
        CALL FUNCTION '/SKN/FC_SW_10_BSART_DESC'
        EXPORTING
          BSART      = <FS_DATA>-BSART
          LANGU      = LV_LANGU
          BSTYP      = <FS_DATA>-BSTYP
          SW_DEST    = LV_SW_DEST
        IMPORTING
          TYPE_DESC  = LS_DATA-BATXT
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
      ENDIF.
      IF <FS_DATA>-EKORG IS NOT INITIAL.
*    "-- EKORG_DESC
        CALL FUNCTION '/SKN/FC_SW_10_PUR_ORG_DESC'
        EXPORTING
          EKORG        = <FS_DATA>-EKORG
          "LANGU              = lv_LANGU
          SW_DEST      = LV_SW_DEST
        IMPORTING
          PUR_ORG_DESC = LS_DATA-EKOTX
        EXCEPTIONS
          WRONG_CODE   = 1
          OTHERS       = 2.
      ENDIF.
      IF <FS_DATA>-EKGRP IS NOT INITIAL .
*    "-- EKGRP_DESC
        CALL FUNCTION '/SKN/FC_SW_10_PUR_GRP_DESC'
        EXPORTING
          EKGRP        = <FS_DATA>-EKGRP
*       LANGU        = lv_LANGU
          SW_DEST      = LV_SW_DEST
        IMPORTING
          PUR_GRP_DESC = LS_DATA-EKNAM
        EXCEPTIONS
          WRONG_CODE   = 1
          OTHERS       = 2.
      ENDIF.
      APPEND LS_DATA TO T_DATA[].
    ENDAT.
  ENDLOOP.
*
**--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
