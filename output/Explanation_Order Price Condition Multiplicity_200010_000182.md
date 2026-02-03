# Exception Indicator: SD Order -Detection of multiple pricing conditions - SW_10_01_ORD_PRICE_M

## General Overview

This Exception Indicator (EI) monitors sales order pricing in Sales and Distribution (SD) to identify cases where multiple pricing conditions of the same type apply to an order or item, supporting detection of pricing-condition multiplicity and discount anomalies. It delegates data selection and logic to the Order Discount Check function and returns the same result structure, providing visibility into sales documents with multiple conditions of the same condition type within configurable time and organizational dimensions.

This EI serves as an essential control for pricing and revenue assurance by:
- Enabling detection of sales order lines or documents where the same condition type appears multiple times, which may indicate data entry errors, duplicate discounts, or configuration issues
- Supporting identification of pricing-condition multiplicity for audit review and pricing policy compliance
- Providing visibility into condition types, discount values, and manual-price indicators across sales organization, distribution channel, and customer
- Enabling analysis of condition base value, rate, and value patterns for margin and revenue recognition checks
- Supporting accountability by sold-to party, sales office, and sales group for pricing and discount governance

Monitoring order price condition multiplicity helps organizations catch duplicate or inconsistent pricing conditions, enforce pricing controls, and prioritize corrections during month-end close and sales reviews. The EI is valuable for pricing exception management and audit of sales document conditions.

The EI uses the Order Discount Check function, which reads sales order and condition data (e.g. VBAK, VBAP, KONV) within a configurable date window.


## Problem Description

Failure to monitor sales orders for multiple pricing conditions of the same type creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected duplicate or multiple conditions of the same type can distort net value, discount, and margin reporting
- Multiple condition entries may indicate double discounts or incorrect condition types affecting revenue recognition
- Concentrations of condition multiplicity in specific periods or organizational units can signal systematic pricing or configuration errors
- Unidentified multiplicity may delay month-end close when discovered late during pricing or revenue review
- Condition base value, rate, and value inconsistencies can affect financial statement assertions and audit evidence

**Sales Operations and Control Risks**
- Sales orders with multiple conditions of the same type without visibility may reflect data entry errors, copy-paste mistakes, or process gaps
- Condition multiplicity by customer, material, or sales channel could indicate unauthorized discounting or pricing policy violations
- Manual price and manually changed condition indicators may go unreviewed when multiplicity is not monitored
- Lack of visibility by document type, item category, and condition type limits operational correction and pricing governance
- Duplicate or redundant conditions can lead to incorrect order values and dispute or refund exposure

**Management Visibility and Decision-Making Risks**
- Absence of condition-multiplicity monitoring delays awareness of pricing data quality and discount accuracy
- Unidentified multiple conditions limit ability to enforce pricing and condition-type policies
- Exceptions may require audit or compliance review but go unnoticed without targeted monitoring
- Inability to attribute multiplicity to organization, customer, or product constrains accountability and corrective action

## Suggested Resolution

**Immediate Response**
- Review the sales order lines or documents flagged by the EI to confirm multiple conditions of the same type and the business context
- Verify condition types, rates, and values in the system (e.g. VA03, VK13) to confirm legitimacy and authorization
- Check manual price and manually changed condition indicators to determine whether multiplicity is intentional or erroneous
- Identify responsible sales organization, distribution channel, and customer for escalation or correction

**System Assessment**
- Analyze the date window and reference date used for selection to ensure the monitoring period is appropriate
- Compare condition-multiplicity volume and count to prior periods and to total order volume to assess materiality and trend
- Examine distribution by sales org, channel, material group, and condition type to find concentration or systematic issues
- Assess manual price and condition-type patterns to distinguish policy-compliant vs. erroneous multiplicity
- Validate condition and pricing data sources to ensure correct multiplicity logic and threshold

**Corrective Actions**
- Correct erroneous or duplicate conditions (e.g. remove duplicate condition records, adjust condition type or value) using VA02 (Change Sales Order) or condition maintenance where applicable
- For legitimate multiple conditions, document business justification and ensure proper approval and audit trail
- Enforce pricing and condition-type policies: restrict duplicate condition types where inappropriate and update training or configuration
- Update monitoring parameters (e.g. lookback, organizational scope, multiplicity factor) to align with pricing and close calendar
- Establish recurring EI runs and route results to sales and finance for continuous pricing oversight and exception resolution


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 3 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 4 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 5 | BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 6 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 7 | CUST_P_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 8 | DATE_REF_FLD | Date Reference Field |  | 0 | 0 |  |  |
| 9 | DISCOUNT | Discount % | CURR | 11 | 2 | /SKN/E_DISCOUNT | /SKN/D_DISCOUNT |
| 10 | DISCOUNT_VAL | Discount Value | CURR | 13 | 2 | /SKN/E_DISCOUNT_VAL | WERTV7 |
| 11 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 12 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 13 | EDATU | Delivery Date | DATS | 8 | 0 | EDATU | DATUM |
| 14 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 15 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 16 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 17 | KAWRT | Condition base value | CURR | 15 | 2 | KAWRT | WERTV8 |
| 18 | KBETR | Condition rate | CURR | 11 | 2 | KBETR | WERTV6 |
| 19 | KMPRS | Changed manually | CHAR | 1 | 0 | KMPRS | XFELD |
| 20 | KNUMV | Doc. condition no. | CHAR | 10 | 0 | KNUMV | KNUMV |
| 21 | KSCHL | Condition type | CHAR | 4 | 0 | KSCHA | KSCHL |
| 22 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 23 | KUNNR_P | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 24 | KWERT | Condition value | CURR | 13 | 2 | KWERT | WERTV7 |
| 25 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 26 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 27 | MPROK_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 28 | MULTIPLE_FACTOR | Same Cond Type Multiple Factor | INT4 | 10 | 0 | /SKN/E_SW_MULT_FACTOR | /SKN/D_SW_MULT_FACTOR |
| 29 | MVGR1 | Material group 1 | CHAR | 3 | 0 | MVGR1 | MVGR1 |
| 30 | MVGR2 | Material group 2 | CHAR | 3 | 0 | MVGR2 | MVGR2 |
| 31 | MVGR3 | Material group 3 | CHAR | 3 | 0 | MVGR3 | MVGR3 |
| 32 | MVGR4 | Material group 4 | CHAR | 3 | 0 | MVGR4 | MVGR4 |
| 33 | MVGR5 | Material group 5 | CHAR | 3 | 0 | MVGR5 | MVGR5 |
| 34 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 35 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 36 | POSNR_MULT_FACTOR | Total Item Conditions | INT4 | 10 | 0 | /SKN/E_SW_MULT_FACTOR | /SKN/D_SW_MULT_FACTOR |
| 37 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 38 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 39 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 40 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 41 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 42 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 43 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 44 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 45 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 46 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 47 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 47 parameters listed in the Parameters Reference Table above.

**AEDAT** (Changed On):

Date or range used to restrict items by change date. The EI (via the called function) compares document dates to the monitoring window; this parameter can narrow selection to a specific changed-on range. Used together with the lookback window when no explicit range is given.

**AUART** (Sales Document Type):

Restricts selection to specific sales document types (e.g. order, quotation, returns). Only documents whose type is in the supplied list are included. Use standard SD document type codes.

**AUDAT** (Document Date):

Document date (date received/sent). When used as a criterion, restricts which documents are included by document date. Used together with the date window when applicable.

**BACKDAYS** (Days Backward from today):

Controls how many days to look back from today when no date range is supplied. The EI builds the monitoring window from today minus this number of days. Larger values widen the period for detecting condition multiplicity; smaller values focus on recent orders.

**BSTNK** (Purchase order no.):

Customer purchase order number. When supplied, restricts or labels results by customer PO for display or filtering in the result set.

**CUST_DESC** (Name):

Customer name (sold-to party description). When used as a criterion, it restricts or labels results by customer name. Typically derived from the sold-to party for display or filtering in the result set.

**CUST_P_DESC** (Name):

Customer name (partner/customer). When used as a criterion, restricts or labels results by partner customer name for display or filtering in the result set.

**DATE_REF_FLD** (Date Reference Field):

Specifies which date field is used as the reference for the monitoring window and for duration calculation. When no date range is supplied, the EI uses this field together with BACKDAYS to build the selection range. Use a date field that exists on the sales order header or item (e.g. ERDAT, AEDAT, AUDAT).

**DATE_REF_FLD Options:**
- **ERDAT**: Created on (document creation date).
- **AEDAT**: Changed on (last change date).
- **AUDAT**: Document date (date received/sent).
- Other date fields (type D) on the order header or item may be supported by the called function; see code or structure for the full list.

**DISCOUNT** (Discount %):

Discount percentage. When used as a selection range, restricts which lines are included by discount percentage. Describes the discount in percent applied to the condition base.

**DISCOUNT_VAL** (Discount Value):

Discount value in document currency. When used as a selection range, restricts which lines are included by discount amount. Describes the discount amount in the document currency.

**DURATION** (Duration In Time Units):

Numeric duration used to filter or display how long ago the document date falls relative to a reference date. The EI calculates duration from the reference date to the run date and can filter by this value. Use together with DURATION_UNIT.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit in which duration is expressed (e.g. days, hours, months). Used for duration calculation and for filtering by DURATION so that age of the document is interpreted consistently.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours.
- **M**: Months (or minutes depending on implementation).

**EDATU** (Delivery Date):

Schedule line / delivery date. When used as a criterion, restricts which items are included by delivery date. Used together with the date window when applicable.

**ERDAT** (Created On):

Date or range for document creation date. Restricts which items are included by creation date. The EI uses creation and change dates to build the selection window and to compute duration.

**ERNAM** (Created By):

Name of person who created the object. When used as a criterion, restricts or labels results by creator for display or filtering in the result set.

**ERZET** (Time):

Entry time. When used as a criterion, restricts or labels results by creation time for display or filtering in the result set.

**KAWRT** (Condition base value):

Condition base value in document currency. When used as a selection range, restricts which lines are included by condition base value. Describes the base value on which the condition rate or value is applied.

**KBETR** (Condition rate):

Condition rate (amount or percentage). When used as a selection range, restricts which lines are included by condition rate. Describes the rate applied to the condition base.

**KMPRS** (Changed manually):

Indicates whether the condition was changed manually. When supplied, restricts results to conditions with the given manual-change status. Used to distinguish manually adjusted conditions in multiplicity analysis.

**KMPRS Options:**
- **X**: Condition changed manually.
- ** ** (space) or blank: Not changed manually (system/default).

**KNUMV** (Doc. condition no.):

Document condition number. When used as a criterion, restricts or labels results by condition document number for display or filtering in the result set.

**KSCHL** (Condition type):

Restricts selection to specific condition types. Only conditions whose type is in the supplied list are included. Use standard SD condition type codes (e.g. price, discount, surcharge).

**KUNNR** (Sold-to party):

Restricts selection to specific sold-to customers. Only sales documents and items for the given customer numbers are included. Use standard customer number format.

**KUNNR_P** (Customer):

Restricts selection by partner customer. Only documents whose partner customer matches the supplied values are included when applicable.

**KWERT** (Condition value):

Condition value in document currency. When used as a selection range, restricts which lines are included by condition value. Describes the condition value in the document currency.

**MATNR** (Material):

Restricts selection to specific materials. Only items with the given material numbers are included. Use standard material number format.

**MPROK** (Manual price):

Indicates whether the item has manual price change. When supplied, restricts results to items with the given manual-price status. Used to distinguish manually adjusted prices in condition multiplicity analysis.

**MPROK Options:**
- **X**: Manual price change (or equivalent active status).
- ** ** (space) or blank: No manual price change / standard.

**MPROK_DESC** (Short text):

Short text for the manual price status. Populated from the MPROK domain for display in the result set.

**MULTIPLE_FACTOR** (Same Cond Type Multiple Factor):

Threshold for same condition type multiplicity (e.g. minimum number of times the same condition type appears). When supplied, the EI flags documents or items where the same condition type appears at least this many times. Use together with condition-type selection for pricing control.

**MVGR1 - MVGR5** (Material group 1 – Material group 5):

Material grouping hierarchy (levels 1–5). When supplied, restrict or label results by material group at the given levels. Use together to scope by product hierarchy for condition multiplicity analysis. Each level (1–5) corresponds to a material group field on the order item or material master.

**NETWR** (Net value):

Net value of the order item in document currency. When used as a selection range, restricts which items are included by net value. Describes the net value in the document currency.

**POSNR** (Sales Document Item):

Item number of the sales document. When supplied, restricts which items are included by position. Used for detailed or single-item analysis.

**POSNR_MULT_FACTOR** (Total Item Conditions):

Multiple factor at item level (e.g. total number of conditions per item or same-type count). When supplied, restricts or labels results by item-level multiplicity for display or filtering in the result set.

**SPART** (Division):

Restricts selection to specific divisions. Only items whose division is in the supplied list are included. Supports division-level pricing and condition multiplicity analysis.

**USER_FLD** (Dynamic Recipient User Field):

Dynamic recipient user field. When used as a criterion, restricts or labels results by user-defined recipient field for display or filtering in the result set.

**VBELN** (Sales Document):

Restricts selection to specific sales documents. Only the given document numbers (and their items) are included. Use for focused review of known orders.

**VBTYP** (SD document categ.):

SD document category (e.g. order, delivery, billing). Restricts which document types are included at the category level. Use together with AUART for precise document-type control.

**VBTYP Options:**
- **A**: Order (e.g. sales order).
- **C**: Billing document.
- **J**: Delivery.
- Other domain values as in VBTYP; exact list is system-dependent.

**VDATU** (Requested deliv.date):

Requested delivery date. When used as a criterion, restricts which items are included by requested delivery date. Used together with the date window when applicable.

**VKBUR** (Sales Office):

Restricts selection to specific sales offices. Only documents and items assigned to the given sales offices are included. Supports accountability by sales office.

**VKGRP** (Sales Group):

Restricts selection to specific sales groups. Only documents and items assigned to the given sales groups are included. Supports accountability by sales group.

**VKORG** (Sales Organization):

Restricts selection to specific sales organizations. Only documents and items belonging to the given sales organizations are included. Use for organizational scoping of condition multiplicity monitoring.

**VTWEG** (Distribution Channel):

Restricts selection to specific distribution channels. Only documents and items with the given distribution channels are included. Use for channel-level pricing analysis.

**WAERK** (Document Currency):

Document currency of the sales document/item. When supplied, restricts results to items in the given currencies. Describes the currency in which net value and condition values are expressed in the document.

**WAVWR** (Cost):

Cost of the item in document currency. When used as a selection range, can restrict which items are included by cost. Describes the cost in the document currency.


### Parameter Relationships

**Time and Duration Parameters:**
- **BACKDAYS** defines the lookback window when no date range is supplied; the EI (via the called function) uses this to build the selection range for document dates (e.g. ERDAT, AEDAT, AUDAT).
- **DATE_REF_FLD** specifies which date field is used as the reference for the monitoring window and for duration calculation; it works with BACKDAYS to define the effective date range.
- **DURATION** and **DURATION_UNIT** work together: DURATION holds the numeric value and DURATION_UNIT the unit (e.g. days, hours, months). The EI uses them to compute and filter by age of the document relative to the reference date and to populate the output duration field.

**Organizational and Selection Parameters:**
- **VKORG**, **VTWEG**, **SPART**, **VKGRP**, **VKBUR** define sales organization, distribution channel, division, sales group, and sales office; they restrict which sales documents and items are selected and appear together in the result.
- **AUART**, **VBTYP** define document type and SD document category; they narrow the type of sales documents included.
- **KUNNR**, **KUNNR_P**, **MATNR** restrict sold-to party, customer, and material and are used together to scope the result set.

**Pricing and Condition Parameters:**
- **KSCHL**, **KBETR**, **KWERT**, **KAWRT**, **DISCOUNT**, **DISCOUNT_VAL** relate to condition type, rate, value, base value, and discount; they restrict or label which conditions and discount ranges are considered and appear in the result.
- **MULTIPLE_FACTOR** and **POSNR_MULT_FACTOR** define thresholds for condition multiplicity (same condition type multiple times); they work with condition selection to determine which lines or documents are flagged.

**Material Group Series:**
- **MVGR1 - MVGR5** (material groups 1–5) work together to restrict or label results by material grouping hierarchy; they can be used for product-line or category-level analysis of condition multiplicity.

**Language and Descriptions:**
- **LANGU** (when applicable in the called function) drives the language for description texts; it affects only output text, not which rows are selected.


### Default Values

Default values are set in the called function (Order Discount Check). When no date range is supplied, the EI typically uses a lookback from today; **BACKDAYS** and **DATE_REF_FLD** define the window. **DURATION_UNIT** is commonly defaulted to days (e.g. `D`). Parameters that are not supplied and have no explicit default are used when initial (empty or 0).

**Note:** For exact default values (e.g. BACKDAYS, DATE_REF_FLD, DURATION_UNIT), refer to the implementation of the called function; this EI passes selection and data through without changing defaults.

### Practical Configuration Examples

**Use Case 1: Multiple pricing conditions in the last 30 days**
```
BACKDAYS = 30
DATE_REF_FLD = ERDAT
DURATION_UNIT = D
```
**Purpose:** Identify sales order lines with multiple conditions of the same type in the last 30 days by creation date for pricing review.

**Use Case 2: Condition multiplicity by sales organization and channel**
```
VKORG = 1000, 2000
VTWEG = 10, 20
BACKDAYS = 14
```
**Purpose:** Focus on condition multiplicity in selected sales organizations and distribution channels over the past two weeks for organizational accountability.

**Use Case 3: By condition type and multiplicity factor**
```
KSCHL = K004, K007
MULTIPLE_FACTOR = 2
BACKDAYS = 7
```
**Purpose:** Flag orders where the same condition type appears at least twice (or per factor) for specific condition types, for discount and pricing control.

**Use Case 4: By material group and document type**
```
AUART = ZOR, ZOR2
MVGR1 = 001, 002
BACKDAYS = 1
```
**Purpose:** Monitor condition multiplicity for specific order types and material groups for process and product-level pricing checks.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_ORD_PRICE_MLT | .INCLUDE |  |  |  |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | AUDAT | Document Date (Date Received/Sent) | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | BSTNK | Customer purchase order number | CHAR(20) | BSTNK |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | CUST_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | CUST_P_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | DISCOUNT | Discount (%) | CURR(11,2) | /SKN/E_DISCOUNT |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | DISCOUNT_VAL | Discount Value | CURR(13,2) | /SKN/E_DISCOUNT_VAL |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | EDATU | Schedule line date | DATS(8) | EDATU |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | KAWRT | Condition base value | CURR(15,2) | KAWRT |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | KBETR | Rate (condition amount or percentage) | CURR(11,2) | KBETR |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | KMPRS | Condition changed manually | CHAR(1) | KMPRS |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | KNUMV | Number of the document condition | CHAR(10) | KNUMV |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | KSCHL | Condition type | CHAR(4) | KSCHA |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | KUNNR_P | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | KWERT | Condition value | CURR(13,2) | KWERT |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MPROK | Status manual price change | CHAR(1) | MPROK |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MPROK_DESC | Explanatory short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MULTIPLE_FACTOR | Multiple Factor | INT4(10) | /SKN/E_SW_MULT_FACTOR |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MVGR1 | Material group 1 | CHAR(3) | MVGR1 |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MVGR2 | Material group 2 | CHAR(3) | MVGR2 |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MVGR3 | Material group 3 | CHAR(3) | MVGR3 |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MVGR4 | Material group 4 | CHAR(3) | MVGR4 |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | MVGR5 | Material group 5 | CHAR(3) | MVGR5 |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | NETWR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | POSNR_MULT_FACTOR | Multiple Factor | INT4(10) | /SKN/E_SW_MULT_FACTOR |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | VDATU | Requested delivery date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_PRICE_MLT | WAVWR | Cost in document currency | CURR(13,2) | WAVWR |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ORD_PRICE_MLT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_PRICE_MLT OPTIONAL
*"----------------------------------------------------------------------
  DATA: LT_DATA TYPE TABLE OF /SKN/S_SW_10_01_ORD_DISC_CHK,
        LWA_DATA LIKE LINE OF LT_DATA,
        LS_DATA LIKE T_DATA.
  CALL FUNCTION '/SKN/F_SW_10_01_ORD_DISC_CHK'
*   IMPORTING
*     IS_ALERT       =
   TABLES
     T_SELECT       = T_SELECT
     T_DATA         = LT_DATA.
  LOOP AT LT_DATA INTO LWA_DATA.
      MOVE-CORRESPONDING LWA_DATA TO LS_DATA.
      APPEND LS_DATA TO T_DATA.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```