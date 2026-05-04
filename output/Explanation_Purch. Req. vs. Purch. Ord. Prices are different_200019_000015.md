# Exception Indicator: MM: PREQ & PO value check - SW_10_03_PR_PO_VAL

## General Overview

This Exception Indicator (EI) monitors purchase requisitions that have been converted to purchase orders and flags cases where the total value on the purchase order differs from the total value derived from the requisition (quantity × price / price unit). It compares requisition and order data by requisition number and item, supports configurable value-difference ranges, and provides visibility into pricing and conversion integrity in procurement.

This EI serves as an essential control for procurement and financial oversight by:
- Enabling detection of value differences between requisition and order that may indicate pricing errors, currency conversion issues, or unauthorized changes at order creation
- Supporting identification of exceptions by purchasing organization, vendor, plant, and release strategy for accountability and process review
- Providing visibility into the age of requisitions and orders via a configurable date reference and duration for prioritization and audit
- Enabling analysis of value-difference patterns by document type, status, and organizational dimension for exception management
- Supporting month-end and audit readiness by surfacing requisition–order value mismatches that may affect commitment or accrual accuracy

Monitoring requisition versus order value helps organizations detect pricing and conversion errors early, enforce approval and conversion controls, and prioritize follow-up on high-value or aged exceptions. The EI is particularly valuable for procurement controls, internal audit, and compliance reviews.

The EI uses purchase requisition data (EBAN), purchase order and schedule line data (EKKO, EKPO, EKET), and release and description lookups to compute and compare requisition value and order value by requisition number and item.


## Problem Description

Failure to monitor requisition versus purchase order value differences creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unmonitored value differences between requisition and order can distort commitment and accrual accuracy if discovered late during period close
- Systematic over- or under-ordering relative to requisition value may indicate pricing, currency, or quantity errors affecting financial statements
- Lack of visibility into value-difference timing can delay identification of conversion or approval issues tied to requisition-to-order process
- Concentrated differences in specific periods or organizational units may signal control weaknesses affecting commitment reliability

**Procurement and Control Risks**
- Unidentified value differences by vendor, plant, or document type may indicate unauthorized pricing or quantity changes at order creation
- Requisition–order value mismatches without monitoring increase risk of fraud or error in the procure-to-pay process
- Absence of monitoring by release strategy or status limits ability to enforce approval and conversion policies
- High volume of exceptions in specific purchasing organizations or requisition types may reflect process or master data quality issues

**Management Visibility and Decision-Making Risks**
- Lack of consolidated visibility delays management awareness of requisition–order value exceptions requiring intervention
- Unmonitored value-difference patterns by organizational dimension limit ability to assign accountability and optimize controls
- Missing link between requisition value and order value hinders root-cause analysis and corrective action
- Absence of duration-based prioritization (e.g. age of requisition or order) limits efficient allocation of review resources

## Suggested Resolution

**Immediate Response**
- Review the requisition–order pairs flagged by the EI to confirm the nature of the value difference (pricing, quantity, currency, or rounding) and the underlying documents
- Verify high-value or high-difference cases using the appropriate display transactions (e.g. ME53N for requisition, ME23N for order) to confirm legitimacy and authorization
- Check requisition and order status, release, and deletion indicator to assess whether corrections or reversals are still possible
- Identify business context: currency conversion, mass conversion, master data change, or potential error or abuse

**System Assessment**
- Analyze the monitoring window and date reference used for duration calculation to ensure the scope aligns with the control objective
- Compare exception volume and value-difference patterns to prior periods and to expected activity by purchasing organization, vendor, and document type
- Examine distribution by plant, release strategy, and processing status to detect misconfiguration or policy gaps
- Validate that filters (requisition/order number, vendor, document type, value-difference range) match the intended control scope

**Corrective Actions**
- Where pricing or quantity errors are confirmed, initiate correction via the appropriate transaction (e.g. ME22N for order, or follow reversal procedures) and escalate to procurement and management
- Update approval or conversion controls if policy or design gaps are identified
- Adjust master data (vendor, material, pricing) where exceptions indicate data quality or process issues
- Document findings and business justifications for audit and management reporting
- Establish recurring EI runs and alert routing so that requisition–order value exceptions are reviewed continuously by responsible roles


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
| 11 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 12 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 13 | EBELN | Purchase Order | CHAR | 10 | 0 | BSTNR | EBELN |
| 14 | EBELP | Purchase Order Item | NUMC | 5 | 0 | BSTPO | EBELP |
| 15 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 16 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 17 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 18 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 19 | ERDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 20 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 21 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 22 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 23 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 24 | FRGKZ | Release indicator | CHAR | 1 | 0 | FRGKZ | FRGKZ |
| 25 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 26 | FRGST | Release strategy | CHAR | 2 | 0 | FRGST | FRGST |
| 27 | FRGZU | Release status | CHAR | 8 | 0 | FRGZU | FRGZU |
| 28 | LFDAT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 29 | LIFNR | Desired Vendor | CHAR | 10 | 0 | WLIEF | LIFNR |
| 30 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 31 | MEINS | Unit of Measure | UNIT | 3 | 0 | BAMEI | MEINS |
| 32 | MENGE | Quantity requested | QUAN | 13 | 3 | BAMNG | MENG13 |
| 33 | PEINH | Price unit | DEC | 5 | 0 | EPEIN | DEC5 |
| 34 | PO_PREQ_DIFF | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 35 | PREIS | Valuation Price | CURR | 11 | 2 | BAPRE | WERT11 |
| 36 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 37 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 38 | STATU | Processing status | CHAR | 1 | 0 | BANST | BANST |
| 39 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 40 | TOT_PO_VAL | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 41 | TOT_PREQ_VAL | Valuation Price | CURR | 17 | 2 | /SKN/E_SW_TOT_PREQ_VAL | /SKN/D_SW_TOT_PREQ_VAL |
| 42 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 43 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 44 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 45 | WERKS_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 45 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Backdays):

Number of days to look back from today. When no date range is supplied, the EI builds the monitoring window from today minus this value and applies it to the configured date field (e.g. requisition date, order date, or changed-on date).

**BADAT** (Requisition Date):

Requisition (request) date. The EI can use this date as the basis for the monitoring window and for duration calculation when so configured.

**BANFN** (Purchase Requisition):

Purchase requisition number. Used when the EI reads requisition data to scope which requisitions are considered for the value comparison with orders.

**BEDAT** (Purchase Order Date):

Purchase order document date. The EI can use this date as the basis for the monitoring window and for duration calculation when so configured.

**BNFPO** (Item of Requisition):

Item number of the purchase requisition. Used when the EI matches requisition items to order schedule lines for value comparison.

**BSAKZ** (Control indicator):

Control indicator for the purchasing document type. Values are function-specific.

**BSAKZ Options:**

Values are function-specific; see output structure or document type configuration.

**BSART** (Document Type):

Requisition document type (e.g. standard, framework). Used when the EI reads requisition data to scope which document types are considered; the code resolves to allowed document types from configuration.

**BSART_DESC** (Doc. Type Descript.):

Short description of the requisition document type; derived from document type and category via description lookup in the EI.

**BSTYP** (Purch. Doc. Category):

Purchasing document category (e.g. standard order, framework order). Used when the EI reads requisition data to scope which document categories are considered.

**BSTYP Options:**

Values are function-specific (e.g. standard order, framework order).

**BSTYP_DESC** (Short Descript.):

Short text for the purchasing document category; derived from master data in the EI.

**DURATION** (Duration In Time Units):

Duration in time units between the reference date (e.g. requisition or order date) and current date. The EI calculates this per record; the unit is configured via DURATION_UNIT. Used for age-based prioritization or filtering.

**DURATION_UNIT** (Duration Unit):

Unit in which duration is expressed (e.g. days). Used with DURATION for the duration calculation in the EI and for time-based filtering.

**DURATION_UNIT Options:**

- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**EBELN** (Purchase Order):

Purchase order number. Used when the EI reads requisition data and matches to order schedule lines to scope which orders are considered for the value comparison.

**EBELP** (Purchase Order Item):

Purchase order item number. From the order schedule line; used when matching requisition items to order items for value comparison.

**EKGRP** (Purchasing Group):

Purchasing group of the requisition. Used when the EI reads requisition data to scope by purchasing group. Description is derived from master data in the EI.

**EKGRP_DESC** (Description p. group):

Description of the purchasing group; from master data.

**EKORG** (Purch. Organization):

Purchasing organization of the requisition. Used when the EI reads requisition data to scope which purchasing organizations are considered. Description is derived from master data in the EI.

**EKORG_DESC** (Description):

Description of the purchasing organization; from master data.

**ERDAT** (Changed On):

Date on which the requisition was created or changed. The EI can use this date as the basis for the monitoring window and for duration calculation when so configured.

**ERNAM** (Created By):

User who created the requisition. Used when the EI reads requisition data to scope by creator for accountability.

**ESTKZ** (Creation Indicator):

Creation indicator (e.g. from requisition, from schedule lines). Used when the EI reads requisition data; the code restricts to specific values (e.g. D, F, R) for the value-comparison logic.

**ESTKZ Options:**

Values are function-specific (e.g. from requisition, from schedule lines); see code for allowed values.

**FRGC** (Release code):

Release code resolved from release group, release strategy, and release status in the EI. Used after the EI resolves release data to scope which release codes are included in the result.

**FRGC Options:**

Values are function-specific (release code configuration).

**FRGGR** (Release group):

Release group of the requisition. Used when the EI reads requisition data to scope which requisitions are subject to release and have the specified release group(s).

**FRGKZ** (Release indicator):

Release indicator that determines which release configuration applies to the requisition. Used when the EI reads requisition data to scope which requisitions are subject to release.

**FRGKZ Options:**

Values are function-specific (release configuration).

**FRGRL** (Subject to release):

Indicates whether the requisition is subject to release. Used when the EI reads requisition data to scope which requisitions are subject to release (value comparison applies only to such requisitions).

**FRGRL Options:**

- **X**: Set/active (subject to release).
- ** ** (space) or blank: Not set.

**FRGST** (Release strategy):

Release strategy code of the requisition. Used when the EI reads requisition data to scope which requisitions have the specified release strategy and are subject to release.

**FRGZU** (Release status):

Release status (e.g. released, partially released). From the requisition; the EI resolves it to a release code (FRGC) and then scopes by release code.

**LFDAT** (Delivery Date):

Item delivery date. From the requisition or order; used in scheduling context.

**LIFNR** (Desired Vendor):

Vendor (desired supplier) number of the requisition. Used when the EI reads requisition data to scope which vendors are considered.

**LOEKZ** (Deletion Indicator):

Deletion indicator on the requisition. Used when the EI reads requisition data; when not set, the code excludes requisitions marked for deletion (effectively “not deleted” only).

**LOEKZ Options:**

- **X**: Set/active (deletion indicator set).
- ** ** (space) or blank: Not set.

**MEINS** (Unit of Measure):

Unit of measure of the requisition. From the requisition; used in quantity and value calculations.

**MENGE** (Quantity requested):

Quantity requested in the requisition. From the requisition; used together with price and price unit to compute requisition value (MENGE × PREIS / PEINH).

**PEINH** (Price unit):

Price unit of the requisition. From the requisition; used together with quantity and price to compute requisition value.

**PO_PREQ_DIFF** (Net Order Value):

Difference between total order value and total requisition value (order value minus requisition value). The EI calculates this per requisition (aggregated by requisition number and item); used to scope which value-difference range is included in the result (e.g. over-order, under-order, or band around zero).

**PREIS** (Valuation Price):

Price in the purchase requisition. From the requisition; used together with quantity and price unit to compute requisition value.

**RESWK** (Supplying Plant):

Supplying (issuing) plant of the requisition. Used when the EI reads requisition data to scope which plants are considered.

**RESWK_DESC** (Name 1):

Name or description of the supplying plant; from master data.

**STATU** (Processing status):

Processing status of the purchase requisition. Used when the EI reads requisition data; the code restricts to a specific status (e.g. released) for the value-comparison logic.

**STATU Options:**

Values are function-specific (e.g. released, blocked); see code for allowed values.

**STATU_DESC** (Short Descript.):

Short text for the processing status; derived from master data in the EI.

**TOT_PO_VAL** (Net Order Value):

Total order value (in order currency) for the requisition–order pair. From the EI calculation; used for value comparison and reporting.

**TOT_PREQ_VAL** (Valuation Price):

Total requisition value (quantity × price / price unit) for the requisition. From the EI calculation; used for value comparison and reporting.

**VENDOR_DESC** (Name):

Vendor name; derived from vendor master in the EI.

**WAERS** (Currency):

Document currency of the requisition or order. Business meaning: currency in which the requisition or order is valued.

**WERKS** (Plant):

Plant of the requisition. Used when the EI reads requisition data to scope which plants are considered.

**WERKS_DESC** (Name 1):

Name or description of the plant; from master data.


### Parameter Relationships

**Time-Based Selection Parameters:**

- When no date range is supplied, the EI builds the monitoring window from today minus the lookback length. The number of days to look back is configured via a single numeric parameter; that value defines the start of the window. The EI then maps this window to a configurable date field (e.g. requisition date, order date, or changed-on date) so that requisitions and orders are selected by the chosen date basis.

**Duration Calculation Parameters:**

- The EI computes a duration (in time units) between a reference date taken from each record and the current date. The reference date is taken from the output record using a configurable date field name. The unit in which duration is expressed (e.g. days) is configured separately. Together, the reference date field and the duration unit determine how duration is calculated; a numeric duration filter can then be used to restrict results (e.g. requisitions with duration within a range).

**Value-Difference Filter (PO_PREQ_DIFF):**

- The EI calculates the difference between total order value and total requisition value (by requisition number and item, aggregated as per the logic). The PO_PREQ_DIFF parameter defines which value-difference range is included in the result (e.g. only positive differences, only above a threshold, or a band around zero). Used to focus on material discrepancies (over- or under-order relative to requisition).

**Release Strategy and Release Code Parameters:**

- Release group, release strategy (FRGST), release indicator (FRGKZ), and release code (FRGC) work together to scope which requisitions are subject to release and which release states are included. The EI reads release-related data from the requisition and resolves release status to a release code; the release code filter then determines which requisition–order pairs appear in the result.


### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the EI uses a 10-day lookback from today for the monitoring window).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).
- **LOEKZ** — Default: initial (empty); when not supplied, the EI uses only requisitions that are not marked for deletion (effectively “not deleted” only).

**Note:** The reference date field used for the monitoring window and for duration calculation is set in the code to a default (e.g. requisition date) when not supplied by the caller; other single-value parameters that are used when initial effectively default to “no restriction” where the code allows.

### Practical Configuration Examples

**Use Case 1: Last 10 days, value difference above threshold**

```
BACKDAYS = 10
PO_PREQ_DIFF = 100–999999999
```

**Purpose:** Monitor requisition–order pairs in the last 10 days where the order value exceeds the requisition value by at least 100 (in document currency). Suitable for detecting over-ordering or pricing increases.

**Use Case 2: By purchasing organization and vendor**

```
EKORG = 1000, 2000
LIFNR = 0000100001–0000100050
```

**Purpose:** Limit results to specific purchasing organizations and vendor number ranges. Supports regional or vendor-specific control and review of value differences.

**Use Case 3: Duration in full days, single value (e.g. exactly 7 days old)**

```
DURATION_UNIT = F
DURATION = 7
```

**Purpose:** Express duration in full days and restrict to requisition–order pairs where the calculated duration equals 7 full days since the reference date. Useful for age-based prioritization (e.g. items exactly one week old). DURATION is a single value, not a range, when using DURATION_UNIT = F.

**Use Case 4: Release strategy and value-difference band**

```
FRGST = 01, 02
PO_PREQ_DIFF = -500–500
```

**Purpose:** Restrict to specific release strategies and to value differences within ±500 of zero. Helps focus on small discrepancies that may be rounding or currency effects rather than large over- or under-orders.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BADAT | Requisition (Request) Date | DATS(8) | BADAT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BANFN | Purchase Requisition Number | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BEDAT | Purchase Order Date | DATS(8) | BEDAT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BNFPO | Item Number of Purchase Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSAKZ | Control indicator for purchasing document type | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSART | Purchase Requisition Document Type | CHAR(4) | BBSRT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSART_DESC | Short Description of Purchasing Document Type | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSTYP | Purchasing Document Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | BSTYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EBELN | Purchase Order Number | CHAR(10) | BSTNR |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EBELP | Purchase Order Item Number | NUMC(5) | BSTPO |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EKGRP_DESC | Description of purchasing group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | EKORG_DESC | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | ERDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | ESTKZ | Creation Indicator (Purchase Requisition/Schedule Lines) | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGKZ | Release Indicator | CHAR(1) | FRGKZ |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGRL | Release Not Yet Completely Effected | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGST | Release Strategy in Purchase Requisition | CHAR(2) | FRGST |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | LFDAT | Item Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | LIFNR | Desired Vendor | CHAR(10) | WLIEF |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | LOEKZ | Deletion Indicator in Purchasing Document | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | MEINS | Purchase Requisition Unit of Measure | UNIT(3) | BAMEI |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | MENGE | Purchase Requisition Quantity | QUAN(13,3) | BAMNG |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | PEINH | Price Unit | DEC(5) | EPEIN |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | PO_PREQ_DIFF | Net Order Value in PO Currency | CURR(13,2) | BWERT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | PREIS | Price in Purchase Requisition | CURR(11,2) | BAPRE |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | RESWK | Supplying (Issuing) Plant in Stock Transport Order | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | RESWK_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | STATU | Processing status of purchase requisition | CHAR(1) | BANST |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | STATU_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | TOT_PO_VAL | Net Order Value in PO Currency | CURR(13,2) | BWERT |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | TOT_PREQ_VAL | Total Price in Purchase Requisition | CURR(17,2) | /SKN/E_SW_TOT_PREQ_VAL |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | VENDOR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_PR_PO_VAL_CHK | WERKS_DESC | Name | CHAR(30) | NAME1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PR_PO_VAL_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PR_PO_VAL_CHK OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: LANGU  LANGU,
             BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
 LV_BACKDAYS = 10.
 LV_DURATION_UNIT = 'D'.
 LV_LANGU = SY-LANGU.
 LV_DATE_REF_FLD = 'BADAT'. "PO date
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
            DURATION    /SKN/E_SW_DURATION,
            LOEKZ       ELOEK,
            FRGC        FRGCO,
            PO_PREQ_DIFF BWERT.
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
            PO_PREQ_DIFF.
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
DATA: LS_T161 TYPE T161,
      LT_T161 LIKE TABLE OF LS_T161.
DATA: LS_EBAN TYPE EBAN,
      LT_EBAN LIKE TABLE OF LS_EBAN.
DATA: BEGIN OF LS_PO,
       BANFN  TYPE BANFN,
       BNFPO TYPE BNFPO,
       EBELN TYPE EBELN,
       EBELP TYPE EBELP,
       LIFNR TYPE WLIEF,
       EKORG TYPE EKORG,
       NETWR TYPE BWERT,
       WKURS TYPE WKURS,
       PO_WAERS TYPE WAERS,
       BEDAT TYPE EBDAT,
       NETWR_LOCAL_CURR TYPE BWERT,
       PR_WAERS TYPE WAERS,
      END OF LS_PO.
DATA: LT_PO LIKE TABLE OF LS_PO.
DATA: BEGIN OF LS_PO_PR,
       BANFN  TYPE BANFN,
       BNFPO TYPE BNFPO,
       LIFNR TYPE WLIEF,
       EKORG TYPE EKORG,
       NETWR_LOCAL_CURR TYPE BWERT,
      END OF LS_PO_PR.
DATA: LT_PO_PR LIKE TABLE OF LS_PO_PR.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PR_PO_VAL_CHK'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
 "--- Prepare BSART
   SELECT *
     FROM T161
     INTO CORRESPONDING FIELDS OF TABLE LT_T161
     WHERE BSTYP = 'B'
       AND BSAKZ = ' '
       AND BSART IN R_BSART.
     REFRESH R_BSART.
     LOOP AT LT_T161 INTO LS_T161.
       RS_BSART-SIGN = 'I'.
       RS_BSART-OPTION = 'EQ'.
       RS_BSART-LOW = LS_T161-BSART.
       APPEND RS_BSART TO R_BSART.
     ENDLOOP.
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
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  REFRESH LT_EBAN.
  SELECT *
    FROM EBAN
    INTO CORRESPONDING FIELDS OF TABLE LT_EBAN
    WHERE FRGRL IN R_FRGRL    "  EQ 'X'
      AND FRGGR IN R_FRGGR
      AND EBELN IN R_EBELN
***      and BSTYP = 'F'  " PO !!! Remove for Test
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
      """and LOEKZ  in R_LOEKZ
      AND LOEKZ = ' '
      AND FRGKZ IN R_FRGKZ
      AND FRGST <>  ''
      AND STATU = 'B'
      AND ESTKZ IN ('D', 'F', 'R')
      AND KNTTP <> ' '
       .
   CHECK LT_EBAN[] IS NOT INITIAL.
   SORT LT_EBAN BY BANFN BNFPO.
   SELECT EKPO~EBELN EKPO~EBELP
          EKPO~NETWR EKKO~WAERS AS PO_WAERS
          EKKO~BEDAT EKKO~WKURS
          EKKO~LIFNR EKKO~EKORG
          EKET~BANFN EKET~BNFPO
     INTO CORRESPONDING FIELDS OF TABLE LT_PO
     FROM EKET
       INNER JOIN EKPO
         ON EKPO~EBELN = EKET~EBELN AND
            EKPO~EBELP = EKET~EBELP
       INNER JOIN EKKO
         ON EKKO~EBELN = EKET~EBELN
     FOR ALL ENTRIES IN LT_EBAN
     WHERE EKET~BANFN = LT_EBAN-BANFN
       AND EKET~BNFPO = LT_EBAN-BNFPO
     .
    SORT LT_PO BY BANFN BNFPO.
    LOOP AT LT_PO INTO LS_PO.
      SY_TABIX = SY-TABIX.
      LS_PO-NETWR_LOCAL_CURR = LS_PO-NETWR.
      READ TABLE LT_EBAN INTO LS_EBAN WITH KEY BANFN = LS_PO-BANFN
                                               BNFPO = LS_PO-BNFPO
                                      BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LS_PO-PR_WAERS = LS_EBAN-WAERS.
        IF LS_PO-PR_WAERS <> LS_PO-PO_WAERS.
          LS_PO-NETWR_LOCAL_CURR = LS_PO-NETWR * LS_PO-WKURS.
        ENDIF.
      ENDIF.
      MODIFY LT_PO FROM LS_PO INDEX SY_TABIX.
    ENDLOOP.
    "--- Aggregate LT_PC by BANFN
    REFRESH LT_PO_PR.
    LOOP AT LT_PO INTO LS_PO.
      MOVE-CORRESPONDING LS_PO TO LS_PO_PR.
      COLLECT LS_PO_PR INTO LT_PO_PR.
    ENDLOOP.
    REFRESH T_DATA.
    LOOP AT LT_PO_PR INTO LS_PO_PR.
      MOVE-CORRESPONDING LS_PO_PR TO T_DATA.
      T_DATA-TOT_PO_VAL = LS_PO_PR-NETWR_LOCAL_CURR.
      READ TABLE LT_EBAN INTO LS_EBAN WITH KEY BANFN = LS_PO_PR-BANFN
                                               BNFPO = LS_PO_PR-BNFPO
                                      BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        MOVE-CORRESPONDING LS_EBAN TO T_DATA.
        MOVE-CORRESPONDING LS_PO_PR TO T_DATA.
        T_DATA-TOT_PREQ_VAL = ( LS_EBAN-MENGE * LS_EBAN-PREIS ) / LS_EBAN-PEINH.
        T_DATA-PO_PREQ_DIFF = T_DATA-TOT_PO_VAL - T_DATA-TOT_PREQ_VAL.
      ENDIF.
      IF T_DATA-PO_PREQ_DIFF IN R_PO_PREQ_DIFF.
        APPEND T_DATA.
      ENDIF.
    ENDLOOP.
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
