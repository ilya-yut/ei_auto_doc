# Exception Indicator: Determine Open Purchase orders ( SW_10_03_OPEN_PO_DET)

## General Overview

This Exception Indicator identifies purchase order items that still have open quantity on schedule lines, focusing on lines with overdue or upcoming delivery dates relative to configurable grace and lookback settings.

This EI serves as an essential control for procurement and materials management by:

- Finding order items where scheduled quantity exceeds goods-receipt quantity
- Supporting follow-up on late or pending deliveries without exporting the full open-PO population
- Enabling filters by vendor, plant, material, company code, and purchasing organization
- Calculating open quantity and open value per item after schedule-line aggregation
- Applying optional age filtering on a configurable reference date for each returned row

Typical use includes buyer follow-up on overdue deliveries, open-commitment reviews, and periodic sampling before period close. Results are intended for exception workflows rather than operational MM reporting extracts.

The routine reads purchase order header, item, and schedule-line data joined to vendor master data, keeps rows with remaining open quantity, and raises an alert when qualifying open lines remain after filtering.


## Problem Description

Failure to monitor open purchase order lines with outstanding delivery quantity creates multiple risks across supply continuity, financial commitment, and procurement control:

**Procurement and Supply Risks**

- Open schedule lines can block production or project timelines when deliveries are late or partially received
- Buyers may not see consolidated open quantity and value across multiple schedule lines on the same item
- Vendor or plant scope that is too broad can hide concentrated overdue lines on critical materials

**Operational Risks**

- Lookback and grace-day settings that are misaligned can exclude recently due lines or retain fully received items
- Deletion, delivery-completed, and goods-receipt indicators that are too loose can mix closed lines into the open queue
- Age filters applied after load can retain rows outside the intended recency band when not tuned with the date window

**Control and Audit Risks**

- Weak monitoring reduces evidence that open commitments were reviewed before accruals or supplier escalation
- Thresholds on open value that are not aligned with delegation rules can miss material exposure on single items

## Suggested Resolution

**Immediate Response**

- Review flagged purchase orders, items, vendors, delivery dates, open quantity, and open value
- Contact the buyer or vendor owner to confirm expected receipt dates and goods-receipt status
- Prioritize high-value or critical-material lines with the largest open quantity

**System Assessment**

- Validate lookback, grace-day, and reference-date settings against how the team defines overdue or pending deliveries
- Tune deletion, delivery-completed, and goods-receipt scope so the queue reflects truly open lines
- Compare exception counts by vendor, plant, and purchasing group to find systematic follow-up gaps

**Corrective Actions**

- Post goods receipts or adjust schedule lines through standard MM processes where deliveries were recorded late
- Adjust monitoring parameters after cleanup to keep results actionable
- Document review outcomes and schedule recurring runs for critical vendors or materials


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 3 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 4 | BEDAT | Purchase Order Date | DATS | 8 | 0 | ETBDT | DATUM |
| 5 | BNFPO | Item of Requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 6 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 7 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 8 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 9 | BWTTY | Valuation Category | CHAR | 1 | 0 | BWTTY_D | BWTTY |
| 10 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 11 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 12 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 13 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 14 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 15 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 16 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 17 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 18 | EREKZ | Final Invoice | CHAR | 1 | 0 | EREKZ | XFELD |
| 19 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 20 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 21 | FIPOS | Commitment Item | CHAR | 14 | 0 | FIPOS | FIPOS |
| 22 | GRACEDAYS | Days Grace |  | 0 | 0 |  |  |
| 23 | KNTTP | Acct Assignment Cat. | CHAR | 1 | 0 | KNTTP | KNTTP |
| 24 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 25 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 26 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 27 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 28 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 29 | PSTYP | Item Category | CHAR | 1 | 0 | PSTYP | PSTYP |
| 30 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 31 | SW_DEST |  | 0 | 0 |  |  |  |
| 32 | UEBTK | Unltd Overdelivery | CHAR | 1 | 0 | UEBTK | XFELD |
| 33 | UEBTO | Overdeliv. Tolerance | DEC | 3 | 1 | UEBTO | PRZ21 |
| 34 | VBUND | Trading Partner | CHAR | 6 | 0 | RASSC | RCOMP |
| 35 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 36 | WEPOS | Goods Receipt | CHAR | 1 | 0 | WEPOS | XFELD |
| 37 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 37 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**AEDAT** (Created on)

Changed-on date used to filter documents or master records by last maintenance activity.

**BACKDAYS** (Days Back)

<mark>BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.</mark>

Backdays is based on DATE_REF_FLD field.

**BANFN** (Purchase Requisition)

Purchase requisition number, the core PR document key for MM approval and lifecycle checks.

**BEDAT** (Purchase Order Date)

Purchasing document date used to filter procurement documents by document creation period.

**BNFPO** (Item of Requisition)

Purchase requisition item number used to identify PR line-level records.

**BSART** (Purchasing Doc. Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**BWTAR** (Valuation Type)

<mark>Valuation type key used in split valuation scenarios (batch/material valuation layers).</mark>

**BWTTY** (Valuation Category)

Valuation category distinguishing split valuation types such as sales-order stock versus own stock.

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- BEDAT — Purchasing document date used to filter procurement documents by document creation period.
- AEDAT — Changed-on date used to filter documents or master records by last maintenance activity.
- EINDT — Item delivery date on purchasing or delivery structures expressing vendor-promised or requested receipt date.

**DATUM** (DATS)

Optional reference date from the selection framework; when empty, the monitoring lower bound is built from **BACKDAYS** and applied per **DATE_REF_FLD**.

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

**EKGRP** (Purchasing Group)

Purchasing group (buyer) used for procurement ownership and control segmentation.

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**ELIKZ** (Delivery Completed)

Delivery completed indicator used to identify open versus completed procurement items.

**EREKZ** (Final Invoice)

Final invoice indicator on the PO item signaling that invoice completion is expected or locked for the line.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ESTKZ** (Creation Indicator)

Creation indicator for PR/PO source or method, used for process-origin analysis.

**FIPOS** (Commitment Item)

<mark>Financial budget period or commitment item line on FM/CO budget addresses when funds management is active.</mark>

**GRACEDAYS** (Days Grace)

Day count subtracted from today to form the upper bound on schedule-line delivery date when delivery date is not supplied (less-than filter on overdue or due lines).

**KNTTP** (Acct Assignment Cat.)

Account assignment category on purchasing items telling whether stock is project, asset, cost-center, or sales-order.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**PSTYP** (Item Category)

Purchasing document item category controlling item behavior, account assignment, and goods-receipt rules.

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**UEBTK** (Unltd Overdelivery)

Unlimited overdelivery allowed indicator on SD or MM quantity contracts controlling tolerance behavior.

**UEBTO** (Overdeliv. Tolerance)

Overdelivery tolerance percent defining how much quantity overrun is accepted versus the order quantity.

**VBUND** (Trading Partner)

Trading partner/company field used for intercompany transaction analysis.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WEPOS** (Goods Receipt)

Goods-receipt indicator on purchasing history rows marking lines created by goods receipt postings.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.


### Parameter Relationships

How parameter combinations work together

**Cloud execution:** When **SW_DEST** is set, processing delegates to `/SKN/FC_SW_10_03_OPEN_PO_DET` and the on-premise path below that call is skipped.

**Reference-date window:** When **DATUM** is empty, a lower bound of today minus **BACKDAYS** is built with a greater-than filter and copied to **BEDAT**, **AEDAT**, or schedule-line **EINDT** per **DATE_REF_FLD** (default **BEDAT**). Explicit date selections override those ranges.

**Delivery-date grace:** When schedule-line delivery date is not supplied, an upper bound of today minus **GRACEDAYS** is applied with a less-than filter on **EINDT** (overdue-or-due schedule lines). When **DATE_REF_FLD** is **EINDT**, the reference-date copy replaces that default **EINDT** range.

**Open-line defaults:** When **ELIKZ**, **LOEKZ**, or **WEPOS** ranges are empty, defaults apply: delivery-completed blank, deletion indicator blank, goods-receipt indicator **X**.

**Main selection:** Purchase order header, item, and schedule lines are read with standard scope filters; schedule lines must have quantity greater than goods-receipt quantity. Adjacent duplicates per order and item are collapsed before schedule lines are re-read for aggregation.

**Open quantity and value:** Open quantity is total scheduled minus total received across schedule lines; rows with no remaining quantity are removed. Open value is derived from net price, open quantity, and unit conversions when conversion factors are present.

**Age filter:** After the date window selects rows, **DURATION** with **DURATION_UNIT** applies a second age filter on the reference date field.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as 0 by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - initial - treated as BEDAT by code
- **GRACEDAYS** - initial - treated as 0 by code
- **ELIKZ** - initial - treated as blank by code
- **LOEKZ** - initial - treated as blank by code
- **WEPOS** - initial - treated as X by code
- **LANGU** - initial - defaults from system logon language when not supplied

### Practical Example of Parameter Configuration

**Use Case 1: Overdue open lines in last ninety days**

**Purpose:** Find open PO items with order dates in scope and delivery dates before today minus seven grace days.

```
BACKDAYS = 90
GRACEDAYS = 7
DATE_REF_FLD = BEDAT
```

**Use Case 2: One vendor and plant**

**Purpose:** Follow up open quantity for a single vendor at one plant.

```
LIFNR = 100000
WERKS = 1000
BACKDAYS = 60
GRACEDAYS = 0
```

**Use Case 3: Material group focus**

**Purpose:** Monitor open lines for a product category.

```
MATKL = 001
EKORG = 1000
BACKDAYS = 30
GRACEDAYS = 14
```

**Use Case 4: Specific purchasing document**

**Purpose:** Review all open schedule lines on one purchase order.

```
EBELN = 4500012345
BACKDAYS = 365
GRACEDAYS = 0
```

**Use Case 5: Created-on reference window**

**Purpose:** Use changed-on header date instead of order date for the lookback window.

```
DATE_REF_FLD = AEDAT
BACKDAYS = 45
GRACEDAYS = 7
```

**Use Case 6: Exactly seven full days since reference date**

**Purpose:** Return rows where the reference date is exactly 7 full days ago.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
GRACEDAYS = 0
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_03_OPEN_PO_DET | AEDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_OPEN_PO_DET | AEDAT_EKKO | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_OPEN_PO_DET | AEDAT_EKPO | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_OPEN_PO_DET | BANFN | Purchase Requisition | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_OPEN_PO_DET | BATXT | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_OPEN_PO_DET | BEDAT | Purchase Order Date | DATS(8) | ETBDT |
| /SKN/S_SW_10_03_OPEN_PO_DET | BNFPO | Item of Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_OPEN_PO_DET | BPUMN | Quantity Conversion | DEC(5) | BPUMN |
| /SKN/S_SW_10_03_OPEN_PO_DET | BPUMZ | Quantity Conversion | DEC(5) | BPUMZ |
| /SKN/S_SW_10_03_OPEN_PO_DET | BSART | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_OPEN_PO_DET | BSTYP | Purch. Doc. Category | CHAR(1) | BSTYP |
| /SKN/S_SW_10_03_OPEN_PO_DET | BSTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_OPEN_PO_DET | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_OPEN_PO_DET | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_03_OPEN_PO_DET | BWTTY | Valuation Category | CHAR(1) | BWTTY_D |
| /SKN/S_SW_10_03_OPEN_PO_DET | CPUDT | Entry Date | DATS(8) | CPUDT |
| /SKN/S_SW_10_03_OPEN_PO_DET | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_OPEN_PO_DET | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_OPEN_PO_DET | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_OPEN_PO_DET | EBELP | Item | NUMC(5) | EBELP |
| /SKN/S_SW_10_03_OPEN_PO_DET | EINDT | Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_OPEN_PO_DET | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_OPEN_PO_DET | EKNAM | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_OPEN_PO_DET | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_OPEN_PO_DET | EKOTX | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_OPEN_PO_DET | ELIKZ | Delivery Completed | CHAR(1) | ELIKZ |
| /SKN/S_SW_10_03_OPEN_PO_DET | EREKZ | Final Invoice | CHAR(1) | EREKZ |
| /SKN/S_SW_10_03_OPEN_PO_DET | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_OPEN_PO_DET | ESTKZ | Creation Indicator | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_OPEN_PO_DET | ETENR | Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_03_OPEN_PO_DET | FIPOS | Commitment Item | CHAR(14) | FIPOS |
| /SKN/S_SW_10_03_OPEN_PO_DET | GJAHR | Material Doc. Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_03_OPEN_PO_DET | KNTTP | Acct Assignment Cat. | CHAR(1) | KNTTP |
| /SKN/S_SW_10_03_OPEN_PO_DET | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_03_OPEN_PO_DET | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_OPEN_PO_DET | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_03_OPEN_PO_DET | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_03_OPEN_PO_DET | MEINS | Order Unit | UNIT(3) | BSTME |
| /SKN/S_SW_10_03_OPEN_PO_DET | MENGE | Scheduled Quantity | QUAN(13) | ETMEN |
| /SKN/S_SW_10_03_OPEN_PO_DET | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_OPEN_PO_DET | NETPR | Net Order Price | CURR(11) | BPREI |
| /SKN/S_SW_10_03_OPEN_PO_DET | NETWR | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_OPEN_PO_DET | OPEN_ORDER_QUAN | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_OPEN_PO_DET | OPEN_VALUE | Net Price in Purchasing Document (in Document Currency) | CURR(13) |  |
| /SKN/S_SW_10_03_OPEN_PO_DET | PEINH | Price Unit | DEC(5) | EPEIN |
| /SKN/S_SW_10_03_OPEN_PO_DET | PLIFZ | Planned Deliv. Time | DEC(3) | PLIFZ |
| /SKN/S_SW_10_03_OPEN_PO_DET | PSTYP | Item Category | CHAR(1) | PSTYP |
| /SKN/S_SW_10_03_OPEN_PO_DET | SHKZG | Debit/Credit Ind. | CHAR(1) | SHKZG |
| /SKN/S_SW_10_03_OPEN_PO_DET | STATU | Status | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_OPEN_PO_DET | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_OPEN_PO_DET | TXZ01 | Short Text | CHAR(40) | TXZ01 |
| /SKN/S_SW_10_03_OPEN_PO_DET | UEBTK | Unltd Overdelivery | CHAR(1) | UEBTK |
| /SKN/S_SW_10_03_OPEN_PO_DET | UEBTO | Overdeliv. Tolerance | DEC(3) | UEBTO |
| /SKN/S_SW_10_03_OPEN_PO_DET | VBUND | Trading Partner | CHAR(6) | RASSC |
| /SKN/S_SW_10_03_OPEN_PO_DET | VGABE | Trans./event type | CHAR(1) | VGABE |
| /SKN/S_SW_10_03_OPEN_PO_DET | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_OPEN_PO_DET | WAERS_LOCAL | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_OPEN_PO_DET | WEMNG | Qty Delivered | QUAN(13) | WEEMG |
| /SKN/S_SW_10_03_OPEN_PO_DET | WEPOS | Goods Receipt | CHAR(1) | WEPOS |
| /SKN/S_SW_10_03_OPEN_PO_DET | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_OPEN_PO_DET | WGBEZ | Material Group Desc. | CHAR(20) | WGBEZ |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_OPEN_PO_DET .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_OPEN_PO_DET OPTIONAL
*"----------------------------------------------------------------------
TYPES: BEGIN OF TY_EKET,
         EBELN TYPE EKET-EBELN,
         EBELP TYPE EKET-EBELP,
         ETENR TYPE EKET-ETENR,
         EINDT TYPE EKET-EINDT,
         MENGE TYPE EKET-MENGE,
         WEMNG TYPE EKET-WEMNG,
         BANFN TYPE EKET-BANFN,
         BNFPO TYPE EKET-BNFPO,
         ESTKZ TYPE EKET-ESTKZ,
       END OF TY_EKET,
       TT_EKET TYPE STANDARD TABLE OF TY_EKET.
DATA_SINGLE: LANGU          LANGU,
             BACKDAYS       INT4,
             GRACEDAYS      INT4,
             DATE_REF_FLD   NAME_FELD,
             ELIKZ          ELIKZ,
             LOEKZ          ELOEK,
             WEPOS          WEPOS,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
 LV_BACKDAYS      = 10.
 LV_DURATION_UNIT = 'D'.
 LV_DATE_REF_FLD  = 'BEDAT'. "PO date
 LV_ELIKZ         = SPACE.
 LV_LOEKZ         = SPACE.
 LV_WEPOS         = 'X'.
 SELECT_SINGLE: LANGU,
                BACKDAYS,
                GRACEDAYS,
                DATE_REF_FLD,
                DURATION_UNIT.
DATA_MULTY: EBELN    EBELN,
            BUKRS    BUKRS,
            BSART    BSART,
            LOEKZ    ELOEK,
            STATU    ESTAK,
            AEDAT    ERDAT,
            ERNAM    ERNAM,
            LIFNR    ELIFN,
            EKORG    EKORG,
            EKGRP    BKGRP,
            WAERS    WAERS,
            MATNR    MATNR,
            WERKS    EWERK,
            MATKL    MATKL,
            KNTTP    KNTTP,
            BWTAR    BWTAR_D,
            BWTTY    BWTTY_D,
            ELIKZ    ELIKZ,
            EREKZ    EREKZ,
            PSTYP    PSTYP,
            FIPOS    FIPOS,
            WEPOS    WEPOS,
            BEDAT    ETBDT,
            EINDT    EINDT,
            BANFN    BANFN,
            BNFPO    BNFPO,
            ESTKZ    ESTKZ,
            VBUND    RASSC,
            UEBTO    UEBTO,
            UEBTK    UEBTK,
            DATUM    SY-DATUM,
            DURATION /SKN/E_SW_DURATION.
SELECT_MULTY: EBELN,
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
              DATUM,
              DURATION.
CONVERT_MULTY: EBELN ALPHA,
               LIFNR ALPHA.
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
  DATA: LS_DATA LIKE LINE OF T_DATA[],
        LS_EKET TYPE TY_EKET.
  DATA: LT_EKET TYPE TT_EKET.
  FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF T_DATA[],
                       TYPE ANY.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_OPEN_PO_DET'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
* Initial Date
   IF R_DATUM[] IS INITIAL .
     RS_DATUM-SIGN   = 'I' .
     RS_DATUM-OPTION = 'GT' .
     DATE_FROM       = SY-DATUM - LV_BACKDAYS.
     RS_DATUM-LOW    = DATE_FROM.
*     rs_datum-high   = sy-datum.
     APPEND RS_DATUM TO R_DATUM.
   ENDIF.
* Initial Delivery date
  IF R_EINDT[] IS INITIAL.
    REFRESH: R_EINDT[].
    CLEAR RS_EINDT.
    RS_EINDT-SIGN   = 'I'.
    RS_EINDT-OPTION = 'LT'.
    RS_EINDT-LOW    = SY-DATUM - LV_GRACEDAYS.
    APPEND RS_EINDT TO R_EINDT[].
  ENDIF.
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
  IF LV_LANGU IS INITIAL.
    LV_LANGU = SY-LANGU.
  ENDIF.
 "--- Set Reference Date Field
   CASE LV_DATE_REF_FLD.
     WHEN 'BEDAT'.
       R_BEDAT[] = R_DATUM[]. " Purchasing document
     WHEN 'AEDAT'.
       R_AEDAT = R_DATUM[].   " Document created
     WHEN 'EINDT'.
       R_EINDT[] = R_DATUM[]. " Item Delivery
     WHEN OTHERS.
       R_BEDAT[] = R_DATUM[]. " Purchasing document
   ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM EKKO AS K INNER JOIN EKPO AS P ON  K~EBELN EQ P~EBELN
                   INNER JOIN EKET AS E ON  P~EBELN EQ E~EBELN
                                        AND P~EBELP EQ E~EBELP
                   INNER JOIN LFA1 AS L ON  K~LIFNR EQ L~LIFNR
    INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
    WHERE K~EBELN IN R_EBELN[]
    AND   K~BUKRS IN R_BUKRS[]
    AND   K~BSART IN R_BSART[]
    AND   K~LOEKZ IN R_LOEKZ[]
    AND   K~STATU IN R_STATU[]
    AND   K~AEDAT IN R_AEDAT[]
    AND   K~ERNAM IN R_ERNAM[]
    AND   K~LIFNR IN R_LIFNR[]
    AND   K~EKORG IN R_EKORG[]
    AND   K~EKGRP IN R_EKGRP[]
    AND   P~MATNR IN R_MATNR[]
    AND   P~WERKS IN R_WERKS[]
    AND   P~MATKL IN R_MATKL[]
    AND   P~KNTTP IN R_KNTTP[]
    AND   P~BWTAR IN R_BWTAR[]
    AND   P~BWTTY IN R_BWTTY[]
    AND   P~ELIKZ IN R_ELIKZ[]
    AND   P~EREKZ IN R_EREKZ[]
    AND   P~PSTYP IN R_PSTYP[]
    AND   P~FIPOS IN R_FIPOS[]
    AND   P~WEPOS IN R_WEPOS[]
    AND   P~LOEKZ IN R_LOEKZ[]
    AND   P~UEBTO IN R_UEBTO[]
    AND   P~UEBTK IN R_UEBTK[]
    AND   E~BEDAT IN R_BEDAT[]
    AND   E~EINDT IN R_EINDT[]
    AND   E~MENGE GT E~WEMNG
    AND   L~VBUND IN R_VBUND[].
  IF T_DATA[] IS NOT INITIAL.
    SORT T_DATA[] BY EBELN EBELP ASCENDING EINDT DESCENDING.
    DELETE ADJACENT DUPLICATES FROM T_DATA[] COMPARING EBELN EBELP.
    SELECT EBELN EBELP ETENR EINDT MENGE WEMNG BANFN BNFPO ESTKZ
      FROM EKET AS E
      INTO TABLE LT_EKET
      FOR ALL ENTRIES IN T_DATA
      WHERE EBELN EQ T_DATA-EBELN
      AND   EBELP EQ T_DATA-EBELP
      AND   BANFN IN R_BANFN[]
      AND   BNFPO IN R_BNFPO[]
      AND   ESTKZ IN R_ESTKZ[]
      AND   MENGE GT E~WEMNG.
  ENDIF.
  IF LT_EKET IS NOT INITIAL.
    SORT LT_EKET BY EBELN EBELP ASCENDING EINDT DESCENDING.
  ENDIF.
**********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
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
******************************************************************************
  CLEAR: SY_TABIX.
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    SY_TABIX = SY-TABIX.
    CLEAR: LV_MENGE, LV_WEMNG, LV_VAL_TMP1, LV_VAL_TMP2.
    LOOP AT LT_EKET INTO LS_EKET WHERE EBELN EQ <FS_DATA>-EBELN
                                 AND   EBELP EQ <FS_DATA>-EBELP.
*                                 AND   eindt EQ <fs_data>-eindt.
        LV_MENGE     = LV_MENGE + LS_EKET-MENGE.
        LV_WEMNG     = LV_WEMNG + LS_EKET-WEMNG.
    ENDLOOP.
    IF LV_MENGE <= LV_WEMNG.
      DELETE T_DATA[] INDEX SY_TABIX.
      CONTINUE.
    ENDIF.
    <FS_DATA>-WAERS_LOCAL     = <FS_DATA>-WAERS.
    <FS_DATA>-MENGE           = LV_MENGE.
    <FS_DATA>-WEMNG           = LV_WEMNG.
    <FS_DATA>-OPEN_ORDER_QUAN = LV_MENGE - LV_WEMNG.
    IF <FS_DATA>-BPUMN <> 0 AND <FS_DATA>-PEINH <> 0.
      LV_VAL_TMP1 = <FS_DATA>-NETPR * ( <FS_DATA>-OPEN_ORDER_QUAN ).
      LV_VAL_TMP2 = ( <FS_DATA>-BPUMZ / <FS_DATA>-BPUMN ) /
                      <FS_DATA>-PEINH.
      <FS_DATA>-OPEN_VALUE  = LV_VAL_TMP1 * LV_VAL_TMP2.
    ELSE.
      <FS_DATA>-OPEN_VALUE = 0.
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
*
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
*
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
*
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
*
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
*
*
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
  ENDLOOP.
*
**--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
