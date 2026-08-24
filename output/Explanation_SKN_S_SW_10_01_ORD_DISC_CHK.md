# Exception Indicator: SD Order- Pricing Conditions (Discount)Check ( SW_10_01_ORD_DISC_CH)

## General Overview

This Exception Indicator identifies sales order items whose aggregated pricing condition reductions match configured percentage, value, or multiplicity thresholds, returning order and condition detail enriched with sold-to and payer customer descriptions.

This EI serves as an essential control for pricing and commercial governance by:

- Enabling detection of order lines with percentage or value reductions outside expected ranges
- Supporting review of multiple pricing conditions applied to the same order item
- Providing visibility into condition type, material group, and organizational context on flagged lines
- Enabling age-based prioritization when order lines remain in scope after a chosen reference date
- Supporting optional customer-specific filtering through a configured additional filter routine

Typical use includes pricing compliance monitoring, duplicate-condition review, and periodic sampling of pricing anomalies before billing. Results are intended for exception workflows rather than operational order list reporting.

The routine reads sales order headers and items, aggregates active pricing conditions from the condition table, applies percentage, value, and multiplicity filters, optionally applies customer filtering, enriches customer descriptions, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor pricing condition reductions on sales order lines creates multiple risks across commercial compliance, margin protection, and billing accuracy:

**Commercial and Pricing Risks**

- Order lines with excessive or unexpected price reductions can erode margin without timely review
- Aggregated reduction values outside policy thresholds can pass to billing undetected
- Multiple pricing conditions on one item can indicate duplicate or conflicting price adjustment application

**Operational Risks**

- Monitoring windows misaligned with order entry cadence can exclude recent pricing exceptions or retain resolved cases
- Condition-type scope that is too broad or too narrow can hide actionable lines or create reviewer fatigue
- Material group or sales organization scope that is not tuned can mix irrelevant items into the review queue

**Control and Audit Risks**

- Weak pricing-condition monitoring reduces evidence that price adjustments were reviewed before release or billing
- Lack of recurring exception review limits accountability for pricing operations follow-up on anomalous reductions
- Missing sold-to and payer context delays escalation of high-value pricing exceptions

## Suggested Resolution

**Immediate Response**

- Review flagged order lines for condition type, aggregated percentage and value, and customer
- Confirm with pricing or sales operations whether the price adjustment is authorized or requires correction
- Prioritize high-value lines and items with multiple condition records for immediate follow-up

**System Assessment**

- Validate lookback window, reference-date field, and age threshold settings against pricing review cadence
- Tune condition type, percentage, value, and multiplicity scope so results stay actionable
- Compare exception counts by condition type, sales organization, and material group to identify systematic gaps

**Corrective Actions**

- Correct pricing conditions through standard SD order maintenance where review confirms action is required
- Adjust monitoring scope after cleanup so results reflect truly exceptional pricing cases
- Document review outcomes and schedule recurring runs before billing or close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 3 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 4 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 5 | BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 6 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 7 | CUST_FILTER_FM | FM - Customer's Add. Filtering |  | 0 | 0 |  |  |
| 8 | CUST_P_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 9 | DATE_REF_FLD | Date Reference Field |  | 0 | 0 |  |  |
| 10 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 11 | DISCOUNT | Discount % | CURR | 11 | 2 | /SKN/E_DISCOUNT | /SKN/D_DISCOUNT |
| 12 | DISCOUNT_VAL | Discount Value | CURR | 13 | 2 | /SKN/E_DISCOUNT_VAL | WERTV7 |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 14 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 15 | EDATU | Delivery Date | DATS | 8 | 0 | EDATU | DATUM |
| 16 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 17 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 18 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 19 | FORWDAYS | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |  |
| 20 | KAWRT | Condition base value | CURR | 15 | 2 | KAWRT | WERTV8 |
| 21 | KBETR | Condition rate | CURR | 11 | 2 | KBETR | WERTV6 |
| 22 | KMPRS | Changed manually | CHAR | 1 | 0 | KMPRS | XFELD |
| 23 | KNUMV | Doc. condition no. | CHAR | 10 | 0 | KNUMV | KNUMV |
| 24 | KSCHL | Condition type | CHAR | 4 | 0 | KSCHA | KSCHL |
| 25 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 26 | KUNNR_P | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 27 | KWERT | Condition value | CURR | 13 | 2 | KWERT | WERTV7 |
| 28 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 29 | MANAGE_IN_UTC | CHAR | 1 | 0 |  | XFELD |  |
| 30 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 31 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 32 | MPROK_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 33 | MULTIPLE_FACTOR | Multiple Factor | INT4 | 10 | 0 | /SKN/E_SW_MULT_FACTOR | /SKN/D_SW_MULT_FACTOR |
| 34 | MVGR1 | Material group 1 | CHAR | 3 | 0 | MVGR1 | MVGR1 |
| 35 | MVGR2 | Material group 2 | CHAR | 3 | 0 | MVGR2 | MVGR2 |
| 36 | MVGR3 | Material group 3 | CHAR | 3 | 0 | MVGR3 | MVGR3 |
| 37 | MVGR4 | Material group 4 | CHAR | 3 | 0 | MVGR4 | MVGR4 |
| 38 | MVGR5 | Material group 5 | CHAR | 3 | 0 | MVGR5 | MVGR5 |
| 39 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 40 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 41 | POSNR_MULT_FACTOR | Multiple Factor | INT4 | 10 | 0 | /SKN/E_SW_MULT_FACTOR | /SKN/D_SW_MULT_FACTOR |
| 42 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 43 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 44 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 45 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 46 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 47 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 48 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 49 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 50 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 51 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 52 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 53 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 53 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**AUDAT** (Document Date)

Sales document date (order date) used for period-based SD selection.

**BACKDAYS** (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BSTNK** (Purchase order no.)

Customer or external PO reference number used for cross-system document matching.

**CUST_DESC** (Name)

Customer description/name text used for readable customer-level reporting.

**CUST_FILTER_FM** (FM - Customer's Add. Filtering)

Optional function module name; when set, each result row is passed to that routine for additional customer-specific acceptance filtering before alerting.

**CUST_P_DESC** (Name)

Aligns exception volume with the chosen scope by testing name via CUST_P_DESC before alert evaluation.

**DATE_REF_FLD** (Date Reference Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- AEDAT — Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.
- AUDAT — Sales document date (order date) used for period-based SD selection.
- VDATU — Requested/validity date used for schedule and due-date based filtering.
- GUEBG — Valid-from date used when the document category is contract and the reference field is auto-set from **VBTYP**.
- ANGDT — Quotation valid-from date used when the document category is quotation and the reference field is auto-set from **VBTYP**.

**DATUM** (DATS)

Explicit monitoring date range supplied by the online monitor; when empty, the lookback window is built from **BACKDAYS** relative to the current day.

**DISCOUNT** (Discount %)

Aggregated discount percentage from active pricing conditions on the order item for the selected condition type group.

**DISCOUNT_VAL** (Discount Value)

Aggregated discount value in document currency from active pricing conditions on the order item.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit(D/H/M))

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**EDATU** (Delivery Date)

Schedule line date in SD deliveries and sales schedules for availability and transportation planning windows.

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ERZET** (Time)

Entry time used to refine timestamp windows within a selected day.

**FORWDAYS** (INT4)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

Forwdays is based on DATE_REF_FLD field.

**Not in use**
**KAWRT** (Condition base value)

Separates cross-client noise from in-scope work when condition base value on KAWRT correlates with client or user attributes.

**KBETR** (Condition rate)

Valuable when comparing health before and after a release—hold condition rate on KBETR constant while varying other filters.

**KMPRS** (Changed manually)

When combined with destination discipline, changed manually on KMPRS keeps both breadth and depth of the extract intentional.

**KNUMV** (Doc. condition no.)

Document conditions number tying SD/MM pricing procedure results to header-level condition records.

**KSCHL** (Condition type)

Condition type such as base price, discount, or freight controlling SD and MM pricing procedures.

**KUNNR** (Sold-to party)

Customer account is used to scope records to specific customers across SD/FI flows.

**KUNNR_P** (Customer)

Treats customer as a discriminator between similar rows that would otherwise look identical in a raw extract.

**KWERT** (Condition value)

Condition value amount in document currency on pricing result lines for threshold and variance checks.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** (CHAR)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MPROK** (Manual price)

Material/procurement status key used to identify control-relevant status states.

**MPROK_DESC** (Short text)

Description of material/procurement status for readable reporting.

**MULTIPLE_FACTOR** (Multiple Factor)

Number of pricing condition records aggregated within one condition-type group on an order item; used to detect multiple conditions of the same type.

**MVGR1 - MVGR5** (Material group 1)

When populated, keeps the extract focused so material group 1 (MVGR1) aligns with the intended triage slice.

**NETWR** (Net value)

Net value amount used for commercial threshold and anomaly checks.

**POSNR** (Sales Document Item)

Document item number used for line-level drilldown and joins.

**POSNR_MULT_FACTOR** (Multiple Factor)

Total count of active pricing condition records on the order item regardless of condition type grouping.

**SPART** (Division)

Division key used for SD product-line segmentation.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

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

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VDATU** (Requested deliv.date)

Requested/validity date used for schedule and due-date based filtering.

**VKBUR** (Sales Office)

Sales office key used for organizational SD segmentation.

**VKGRP** (Sales Group)

Sales group key used for team-level SD analytics.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.

**WAERK** (Document Currency)

Document currency key used for value analysis in transaction currency.

**WAVWR** (Cost)

Statistical value amount field used for value-based exception thresholds.

### Parameter Relationships

**Explicit calendar window versus lookback:** **DATUM** supplies explicit from-and-to calendar bounds for the monitoring pass. When **DATUM** is empty, **BACKDAYS** builds the calendar window relative to the evaluation day before orders are read.

**Reference date mapping:** **DATE_REF_FLD** directs the lookback window to the created-on, changed-on, document, or requested delivery date on each order line.

**Order and item selection:** **VBELN**, **KUNNR**, **VKORG**, **VTWEG**, **SPART**, **AUART**, **VBTYP**, **BSTNK**, **MVGR1**–**MVGR5**, **MPROK**, and **WAVWR** narrow which sales order items enter the initial read.

**Pricing condition aggregation:** Active pricing conditions for each item are read by **KSCHL** and **KMPRS**. **DISCOUNT** and **DISCOUNT_VAL** hold the aggregated percentage and value; **MULTIPLE_FACTOR** counts condition records within each condition-type group, and **POSNR_MULT_FACTOR** holds the total condition count per item.

**Post-aggregation filters:** Rows are removed unless **DISCOUNT**, **DISCOUNT_VAL**, **MULTIPLE_FACTOR**, and **POSNR_MULT_FACTOR** remain within their configured selections.

**Age filter:** Elapsed time from each row's reference date to the evaluation time is calculated using **DURATION_UNIT** and stored in **DURATION**; rows outside the configured range are removed.

**Customer filtering:** When **CUST_FILTER_FM** is set, an optional function module can remove rows that fail customer-specific acceptance rules.

**Descriptions:** **CUST_DESC** and **CUST_P_DESC** are filled for the sold-to party and payer after filtering completes.

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper and the on-premise path below that call is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DATE_REF_FLD** - initial - treated as VDATU by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code

### Practical Example of Parameter Configuration

**Use Case 1: High discount percentage**

**Purpose:** Flag order items whose aggregated discount percentage exceeds a threshold for one condition type.

```
KSCHL = K007
DISCOUNT = 15 - 100
VKORG = 1000
VTWEG = 10
```

**Use Case 2: Discount value threshold**

**Purpose:** Review items with aggregated discount value above a monetary limit in one sales organization.

```
DISCOUNT_VAL = 1000 - 999999999
VKORG = 1000
BACKDAYS = 7
KSCHL = K007
```

**Use Case 3: Multiple conditions on one item**

**Purpose:** Detect items where more than one pricing condition record was aggregated for the same condition type.

```
MULTIPLE_FACTOR = 2 - 99
KSCHL = K007
VKORG = 1000
BACKDAYS = 14
```

**Use Case 4: Material group and condition type**

**Purpose:** Monitor discount exceptions for one material group and sales document type.

```
MVGR1 = 01
AUART = TA
KSCHL = K007
DISCOUNT = 10 - 100
```

**Use Case 5: Exactly seven full days since requested delivery date**

**Purpose:** Return rows whose requested delivery date is exactly 7 full days ago for weekly pricing follow-up.

```
DURATION = 7
DURATION_UNIT = F
DATE_REF_FLD = VDATU
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_ORD_DISC_CHK | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_ORD_DISC_CHK | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_DISC_CHK | AUDAT | Document Date (Date Received/Sent) | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_ORD_DISC_CHK | BSTNK | Customer purchase order number | CHAR(20) | BSTNK |
| /SKN/S_SW_10_01_ORD_DISC_CHK | CUST_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_DISC_CHK | CUST_P_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_DISC_CHK | DISCOUNT | Discount (%) | CURR(11,2) | /SKN/E_DISCOUNT |
| /SKN/S_SW_10_01_ORD_DISC_CHK | DISCOUNT_VAL | Discount Value | CURR(13,2) | /SKN/E_DISCOUNT_VAL |
| /SKN/S_SW_10_01_ORD_DISC_CHK | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ORD_DISC_CHK | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ORD_DISC_CHK | EDATU | Schedule line date | DATS(8) | EDATU |
| /SKN/S_SW_10_01_ORD_DISC_CHK | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_ORD_DISC_CHK | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_ORD_DISC_CHK | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_ORD_DISC_CHK | KAWRT | Condition base value | CURR(15,2) | KAWRT |
| /SKN/S_SW_10_01_ORD_DISC_CHK | KBETR | Rate (condition amount or percentage) | CURR(11,2) | KBETR |
| /SKN/S_SW_10_01_ORD_DISC_CHK | KMPRS | Condition changed manually | CHAR(1) | KMPRS |
| /SKN/S_SW_10_01_ORD_DISC_CHK | KNUMV | Number of the document condition | CHAR(10) | KNUMV |
| /SKN/S_SW_10_01_ORD_DISC_CHK | KSCHL | Condition type | CHAR(4) | KSCHA |
| /SKN/S_SW_10_01_ORD_DISC_CHK | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_ORD_DISC_CHK | KUNNR_P | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_DISC_CHK | KWERT | Condition value | CURR(13,2) | KWERT |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MPROK | Status manual price change | CHAR(1) | MPROK |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MPROK_DESC | Explanatory short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MULTIPLE_FACTOR | Multiple Factor | INT4(10) | /SKN/E_SW_MULT_FACTOR |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MVGR1 | Material group 1 | CHAR(3) | MVGR1 |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MVGR2 | Material group 2 | CHAR(3) | MVGR2 |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MVGR3 | Material group 3 | CHAR(3) | MVGR3 |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MVGR4 | Material group 4 | CHAR(3) | MVGR4 |
| /SKN/S_SW_10_01_ORD_DISC_CHK | MVGR5 | Material group 5 | CHAR(3) | MVGR5 |
| /SKN/S_SW_10_01_ORD_DISC_CHK | NETWR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_DISC_CHK | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_ORD_DISC_CHK | POSNR_MULT_FACTOR | Multiple Factor | INT4(10) | /SKN/E_SW_MULT_FACTOR |
| /SKN/S_SW_10_01_ORD_DISC_CHK | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_DISC_CHK | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_DISC_CHK | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_DISC_CHK | VDATU | Requested delivery date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_ORD_DISC_CHK | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_ORD_DISC_CHK | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_ORD_DISC_CHK | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_DISC_CHK | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_DISC_CHK | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_DISC_CHK | WAVWR | Cost in document currency | CURR(13,2) | WAVWR |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ORD_DISC_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_DISC_CHK OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
               LANGU  LANGU,
               BACKDAYS INT4,
               FORWDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               CUST_FILTER_FM /SKN/E_SW_RL_FM.  " Customer's Additional Filtering FM
               "kmprs          kmprs.
** Default values
  LV_BACKDAYS = 1.
  LV_DATE_REF_FLD = 'VDATU'.  "'ERDAT'.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU. """02-6-19
  SELECT_SINGLE: MANAGE_IN_UTC,
                 LANGU,
                 BACKDAYS,
                 FORWDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT,
                 CUST_FILTER_FM.
                 "kmprs.
  DATA_MULTY: KUNNR        VBAK-KUNNR,
              VBELN        VBAK-VBELN,
              VKORG        VBAK-VKORG,
              VTWEG        VBAK-VTWEG,
              SPART        VBAK-SPART,
              BSTNK        VBAK-BSTNK,
              VBTYP        VBAK-VBTYP,
*            NETWR_USD    VBAK-NETWR,
              AUART        VBAK-AUART,
              ERDAT        VBAK-ERDAT,
              AEDAT        VBAK-AEDAT,
              AUDAT        VBAK-AUDAT,
              VDATU        VBAK-VDATU,
              DATUM        SY-DATUM,
              KSCHL        KONV-KSCHL,
              MVGR1        VBAP-MVGR1,
              MVGR2        VBAP-MVGR2,
              MVGR3        VBAP-MVGR3,
              MVGR4        VBAP-MVGR4,
              MVGR5        VBAP-MVGR5,
*              edatu        vbep-edatu,
              DISCOUNT     /SKN/S_SW_10_01_ORD_DISC_CHK-DISCOUNT,
              DISCOUNT_VAL /SKN/E_DISCOUNT_VAL,
              MULTIPLE_FACTOR   /SKN/E_SW_MULT_FACTOR,
              POSNR_MULT_FACTOR /SKN/E_SW_MULT_FACTOR,
              DURATION   /SKN/E_SW_DURATION,
              MPROK       MPROK,  """"02-6-19
              WAVWR       WAVWR,
              KMPRS          KMPRS.
               .
  SELECT_MULTY: KUNNR,
              VBELN,
              VKORG ,
              VTWEG ,
              SPART,
              BSTNK,
              VBTYP,
*              netwr_usd,
              AUART,
              ERDAT,
              AEDAT,
              AUDAT,
              VDATU,
              DATUM,
              KSCHL,
              MVGR1,
              MVGR2,
              MVGR3,
              MVGR4,
              MVGR5,
*              edatu,
              DISCOUNT,
              DISCOUNT_VAL,
              MULTIPLE_FACTOR,
              POSNR_MULT_FACTOR,
              DURATION,
              MPROK,  """ 02-6-19
              WAVWR,
              KMPRS.
  CONVERT_MULTY: KUNNR ALPHA,
                 VBELN ALPHA.
  CONVERT_MULTY:  AUART AUART . """Tanya 14/11/18
  RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
           R_FLD_VAL FOR DD03P-FIELDNAME .
  DATA :   FLD_NAME TYPE FIELDNAME.
  DATA : I TYPE I,
         CI(1) TYPE C,
         NFIELDS TYPE I VALUE 3.   "
  DATA : BACKDAYS  TYPE I ,
         FORWDAYS TYPE I,
         DATE_FROM LIKE SY-DATUM,
         DATE_TO LIKE SY-DATUM .
  DATA : LANGU LIKE SY-LANGU .
  DATA : IS_OUT(1) TYPE C.
  DATA : TIME_DIFF TYPE  INT4 .
  DATA : W_DATA LIKE LINE OF T_DATA .
*  DATA : wa_vbpa TYPE vbpa.
  DATA : LV_VBELN TYPE VBELN,
         LV_POSNR TYPE POSNR,
         LV_PARVW TYPE PARVW,
         LV_KUNNR TYPE  KUNNR,
         LV_KUNNR_NAME TYPE  NAME1_GP,
         LV_KUNNR_P TYPE  KUNNR,
         LV_CUST_P_DESC TYPE  NAME1_GP.
*         lv_lifnr TYPE  lifnr,
*         lv_lifnr_name TYPE  name1_gp,
*         lv_pernr TYPE  pernr_d,
*         lv_pernr_name TYPE  name1_gp,
*         lv_nrart TYPE nrart.
  DATA: LV_VBTYP TYPE VBTYP.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA: LT_DATA LIKE T_DATA[],
        LWA_DATA LIKE LINE OF T_DATA,
        LWA_DATA_ORG LIKE LINE OF T_DATA.
  """ 02-6-19
DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
      LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
      LV_DDTEXT LIKE  DD07V-DDTEXT.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    DATA: LV_IS_HANA(1) TYPE C.
    CALL FUNCTION '/SKN/F_SW_IS_RFCDEST_HANA'
      EXPORTING
        DEST          = LV_SW_DEST
      IMPORTING
        IS_HANA       =  LV_IS_HANA.
              .
   IF LV_IS_HANA IS NOT INITIAL.
    CALL FUNCTION '/SKN/FH_SW_10_01_ORD_DISC_CHK'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   ELSE.
    CALL FUNCTION '/SKN/FC_SW_10_01_ORD_DISC_CHK'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   ENDIF.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
  IF NOT LV_FORWDAYS  IS INITIAL.
    LV_BACKDAYS = LV_FORWDAYS * ( -1 ).
  ENDIF.
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  DATE_FROM = SY-DATUM.
  READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_DATUM-LOW.
    DATE_TO = RS_DATUM-HIGH.
    IF DATE_TO < DATE_FROM.
      DATE_TO = DATE_FROM.
    ENDIF.
  ENDIF.
  "--- Check Quatetion or Contracts types
  IF LV_DATE_REF_FLD IS INITIAL.
    READ TABLE R_VBTYP INTO RS_VBTYP INDEX 1.
    IF SY-TFILL = 1. " the single record only
      IF RS_VBTYP-OPTION = 'EQ'.
        LV_VBTYP = RS_VBTYP-LOW.
      ENDIF.
    ENDIF.
    IF LV_VBTYP = 'G'.
      LV_DATE_REF_FLD = 'GUEBG'.
    ELSEIF LV_VBTYP = 'B'.
      LV_DATE_REF_FLD = 'ANGDT'.
    ENDIF.
    "    endif.
  ENDIF.
  "---
  CASE LV_DATE_REF_FLD.
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "Changed On
    WHEN 'AUDAT'.
      R_AUDAT[] = R_DATUM[]. "Document Date (Date Received/Sent)
*    WHEN 'EDATU'.
*      r_edatu = r_datum[].
    WHEN 'VDATU'.
      R_VDATU[] = R_DATUM[].
    WHEN OTHERS.
      R_ERDAT[] = R_DATUM[]. "Document created
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH: LT_DATA.
  SELECT *
**   Temporary will be removed later
**    vbak~vbeln vbap~posnr vbak~erdat vbak~erzet vbak~ernam vbak~audat vbak~vbtyp vbak~auart vbak~aedat vbak~netwr
**    vbak~waerk vbak~kunnr vbak~vkorg vbak~vtweg vbak~spart vbak~vkgrp vbak~vkbur vbak~bstnk
**    vbak~knumv vbap~mvgr1 vbap~mvgr2 vbap~mvgr3 vbap~mvgr4 vbap~mvgr5 vbep~edatu
    FROM VBAK
    INNER JOIN VBAP ON
    VBAK~VBELN = VBAP~VBELN
** Deactive: Ready for future use schedule date
*    INNER JOIN vbep ON
*    vbap~vbeln = vbep~vbeln AND
*    vbap~posnr = vbep~posnr
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE VBAK~VBELN IN R_VBELN
      AND KUNNR IN R_KUNNR
      AND VKORG IN R_VKORG
      AND VTWEG IN R_VTWEG
      AND VBAK~SPART IN R_SPART
      AND BSTNK IN R_BSTNK
      AND AUART IN R_AUART
      AND VBAK~ERDAT IN R_ERDAT
      AND VBAK~AUDAT IN R_AUDAT
      AND VBAK~AEDAT IN R_AEDAT
      AND VBAK~VDATU IN R_VDATU
      AND VBTYP IN R_VBTYP
      AND MVGR1 IN R_MVGR1
      AND MVGR2 IN R_MVGR2
      AND MVGR3 IN R_MVGR3
      AND MVGR4 IN R_MVGR4
      AND MVGR5 IN R_MVGR5
      AND MPROK IN R_MPROK  """ 02-6-19
      AND WAVWR IN R_WAVWR.
      .
*      AND edatu IN r_edatu. " Deactive: Ready for future use schedule date
  CHECK LT_DATA[] IS NOT INITIAL.
  DATA: LT_KONV TYPE TABLE OF KONV,
        LWA_KONV LIKE LINE OF LT_KONV.
  SELECT * INTO TABLE LT_KONV
    FROM KONV
    FOR ALL ENTRIES IN LT_DATA
    WHERE KNUMV = LT_DATA-KNUMV
      AND KPOSN = LT_DATA-POSNR
      AND KSCHL IN R_KSCHL
      AND KINAK = ''
      AND KMPRS IN R_KMPRS.
  DATA: BEGIN OF LS_KONV_TOT,
          KNUMV TYPE KNUMV,
          KPOSN TYPE KPOSN,
          CNT   TYPE I,
        END OF LS_KONV_TOT.
  DATA: LT_KONV_TOT LIKE TABLE OF LS_KONV_TOT.
  DATA: LV_KNUMV TYPE KNUMV,
        LV_KPOSN TYPE KPOSN,
        LV_CNT   TYPE I.
  DATA:
        LV_TABIX LIKE SY-TABIX,
        LV_KSCHL LIKE KONV-KSCHL.
  SORT LT_KONV BY KNUMV KPOSN KSCHL.
  "--- Calculate Total Value for condition
  REFRESH LT_KONV_TOT.
  CLEAR: LV_KNUMV, LV_KPOSN, LV_CNT.
  LOOP AT LT_KONV INTO LWA_KONV.
    IF LV_KNUMV = LWA_KONV-KNUMV AND LV_KPOSN = LWA_KONV-KPOSN.
      ADD 1 TO LV_CNT.
    ELSE.
      IF LV_CNT > 0.
        LS_KONV_TOT-KNUMV = LV_KNUMV.
        LS_KONV_TOT-KPOSN = LV_KPOSN.
        LS_KONV_TOT-CNT = LV_CNT.
        APPEND LS_KONV_TOT TO LT_KONV_TOT.
      ENDIF.
      LV_KNUMV = LWA_KONV-KNUMV.
      LV_KPOSN = LWA_KONV-KPOSN.
      LV_CNT = 1.
    ENDIF.
  ENDLOOP.
      IF LV_CNT > 0.
        LS_KONV_TOT-KNUMV = LV_KNUMV.
        LS_KONV_TOT-KPOSN = LV_KPOSN.
        LS_KONV_TOT-CNT = LV_CNT.
        APPEND LS_KONV_TOT TO LT_KONV_TOT.
      ENDIF.
  "--- Calculate Total Value for condition
  CLEAR LWA_DATA_ORG.
** Stay with record with the last schedule date
** Deactive: Ready for future use schedule date
*  SORT lt_data BY vbeln posnr edatu DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING vbeln posnr.
  LOOP AT LT_DATA INTO LWA_DATA.
    CLEAR LV_KSCHL.
    LWA_DATA_ORG = LWA_DATA.
    CLEAR: LWA_KONV, LV_TABIX.
    READ TABLE LT_KONV INTO LWA_KONV
                       WITH KEY KNUMV = LWA_DATA-KNUMV
                                KPOSN = LWA_DATA-POSNR
                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_TABIX = SY-TABIX.
      LV_KSCHL = LWA_KONV-KSCHL.
      WHILE SY-SUBRC = 0 AND LWA_KONV-KNUMV = LWA_DATA-KNUMV AND
            LWA_KONV-KPOSN = LWA_DATA-POSNR.
        IF LV_KSCHL <> LWA_KONV-KSCHL.
          IF LWA_DATA-DISCOUNT IN R_DISCOUNT AND LWA_DATA-MULTIPLE_FACTOR IN R_MULTIPLE_FACTOR.
            APPEND LWA_DATA TO T_DATA.
          ENDIF.
          CLEAR LWA_DATA.
          LWA_DATA = LWA_DATA_ORG.
        ENDIF.
        LWA_DATA-DISCOUNT = LWA_DATA-DISCOUNT + ( LWA_KONV-KBETR / 10 ).
        LWA_DATA-DISCOUNT_VAL = LWA_DATA-DISCOUNT_VAL + LWA_KONV-KWERT.
        LWA_DATA-KBETR = LWA_DATA-KBETR + LWA_KONV-KBETR.
        LWA_DATA-KAWRT = LWA_DATA-KAWRT + LWA_KONV-KAWRT.
        LWA_DATA-KWERT = LWA_DATA-KWERT + LWA_KONV-KWERT.
        LWA_DATA-KSCHL =  LWA_KONV-KSCHL.
        LWA_DATA-MULTIPLE_FACTOR = LWA_DATA-MULTIPLE_FACTOR + 1.
        LV_TABIX = LV_TABIX + 1.
        LV_KSCHL = LWA_KONV-KSCHL.
        CLEAR LWA_KONV.
        READ TABLE LT_KONV INTO LWA_KONV INDEX LV_TABIX.
      ENDWHILE.
      IF LWA_DATA-DISCOUNT IN R_DISCOUNT AND LWA_DATA-MULTIPLE_FACTOR IN R_MULTIPLE_FACTOR.
        APPEND LWA_DATA TO T_DATA.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DISCOUNT_VAL NOT IN R_DISCOUNT_VAL.
  "--- Set POSNR_MULT_FACTOR
  SORT LT_KONV_TOT BY KNUMV KPOSN.
  LOOP AT T_DATA INTO LWA_DATA.
    LV_TABIX = SY-TABIX.
    READ TABLE LT_KONV_TOT INTO LS_KONV_TOT
                           WITH KEY  KNUMV = LWA_DATA-KNUMV
                                     KPOSN = LWA_DATA-POSNR
                           BINARY SEARCH .
    IF SY-SUBRC IS INITIAL.
      LWA_DATA-POSNR_MULT_FACTOR = LS_KONV_TOT-CNT.
    ENDIF.
    MODIFY T_DATA FROM LWA_DATA INDEX LV_TABIX.
  ENDLOOP.
  DELETE T_DATA WHERE POSNR_MULT_FACTOR NOT IN R_POSNR_MULT_FACTOR.
**********************************************************************
**********************************************************************
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    IF  IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          T_DATA-DURATION = TIME_DIFF .
        ELSE.
          T_DATA-DURATION = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
"--- Fill Payer
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    SELECT SINGLE KUNNR
      FROM VBPA
     INTO T_DATA-KUNNR_P
     WHERE VBELN = T_DATA-VBELN
       AND POSNR = '000000'
       AND PARVW = 'RG'.
     IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
**********************************************************************
 "--- Additional Customer's filtering
 IF LV_CUST_FILTER_FM IS NOT INITIAL.
  DATA : LV_CUST_FILTER_DEST TYPE RFCDEST VALUE 'NONE'.  "!!!
  DATA : LS_STRUCTURE TYPE /SKN/S_SW_S_FCAT,
         LT_STRUCTURE LIKE TABLE OF LS_STRUCTURE.
  DATA : LS_DD_SET TYPE /SKN/SW_SELECT,
         LT_DD_SET LIKE TABLE OF LS_DD_SET.
  DATA :    "fld_Name(30)  type C,
         "fld(100)  type C,
         FLD_VALUE(45) TYPE C.
  "FIELD-SYMBOLS:  type any.
  DATA : LV_CUST_FILTER_OK(1) TYPE C.
    CALL FUNCTION '/SKN/F_SW_GET_STUCTURE'
      EXPORTING
        AI_STRUCTURE_NAME       = '/SKN/S_SW_10_01_ORD_DISC_CHK'
*       LANGU                   = SY-LANGU
      TABLES
        AI_STRUCTURE            = LT_STRUCTURE
*       AI_MN_STRUCTURE         =
      EXCEPTIONS
        WRONG_STRUCTURE         = 1
        OTHERS                  = 2.
    IF SY-SUBRC <> 0.
      CLEAR LV_CUST_FILTER_FM.
    ENDIF.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    REFRESH LT_DD_SET .
    LOOP AT LT_STRUCTURE INTO LS_STRUCTURE .
      FLD_NAME = LS_STRUCTURE-FIELDNAME .
      CONCATENATE 'T_DATA-' FLD_NAME INTO FLD .
      ASSIGN (FLD) TO .
      FLD_VALUE =  .
      LS_DD_SET-FIELDNM = FLD_NAME .
       LS_DD_SET-SIGN = 'I'.
        LS_DD_SET-OPTION = 'EQ' .
         LS_DD_SET-LOW = FLD_VALUE .
          APPEND LS_DD_SET TO LT_DD_SET .
    ENDLOOP .
    LV_CUST_FILTER_OK = 'X'.
    CALL FUNCTION LV_CUST_FILTER_FM
       DESTINATION  LV_CUST_FILTER_DEST
*       EXPORTING
*        AN_CODE        = AN_CODE
      IMPORTING
        IS_ACCEPTED     = LV_CUST_FILTER_OK
      TABLES
        T_DD_SET       = LT_DD_SET
        T_SELECT       = T_SELECT
      EXCEPTIONS
        EXCEPTIONS SYSTEM_FAILURE = 1
        COMMUNICATION_FAILURE = 2
        OTHERS              = 9 .
    IF NOT SY-SUBRC IS INITIAL .
      EXIT.
      " Wrong FM Call
      CLEAR LV_CUST_FILTER_FM.
    ENDIF .
    IF LV_CUST_FILTER_OK IS INITIAL.  " Filtered Out
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
 ENDIF."
**********************************************************************
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR          = T_DATA-KUNNR
      IMPORTING
        CUST_DESC      = T_DATA-CUST_DESC
      EXCEPTIONS
        WRONG_CUSTOMER = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR          = T_DATA-KUNNR_P
      IMPORTING
        CUST_DESC      = T_DATA-CUST_P_DESC
      EXCEPTIONS
        WRONG_CUSTOMER = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
 """ 02-6-19
    LV_DOMNAME = 'MPROK'.
    LV_DOMVALUE = T_DATA-MPROK.
      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
        EXPORTING
          I_DOMNAME        = LV_DOMNAME
          I_DOMVALUE       = LV_DOMVALUE
          LANGU            = LV_LANGU
*         SW_DEST          =
       IMPORTING
         E_DDTEXT          = LV_DDTEXT
       EXCEPTIONS
         NOT_EXIST        = 1
         OTHERS           = 2
                .
     IF SY-SUBRC = 0.
      T_DATA-MPROK_DESC = LV_DDTEXT.
     ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
**********************************************************************
*********************************************************************
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
