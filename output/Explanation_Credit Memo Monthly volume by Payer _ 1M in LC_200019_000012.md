# Exception Indicator: Exceptional sales documents values – Aggregated - SW_10_01_ORD_VAL_TOT

## General Overview

This Exception Indicator (EI) monitors aggregated sales order values in SAP SD to identify exceptional patterns in sales document values across configurable time periods and organizational dimensions. It aggregates net value and count by period (month, week, quarter, or year) and by configurable aggregation fields, providing consolidated visibility into sales volume trends and enabling detection of unusual value concentrations, high-value transaction clusters, and sales pattern anomalies that require management attention.

This EI serves as an essential control for sales management and financial oversight by:
- Enabling detection of exceptional sales volumes and value concentrations that exceed thresholds and require management attention
- Supporting identification of high-value transactions and unusual sales patterns for revenue recognition and audit review
- Providing visibility into sales trends by time period and organizational dimension for performance and resource allocation decisions
- Enabling analysis of sales concentration and customer patterns for strategic pricing and engagement
- Supporting accountability and relationship visibility by sold-to, ship-to, and other partner roles for dispute and fulfillment oversight

This aggregated monitoring enables organizations to detect high-value transaction clusters, unusual sales concentration patterns, potential revenue recognition issues, and sales trends requiring executive visibility. The EI is particularly valuable for month-end close processes, sales performance reviews, and financial exception management.

The EI uses sales order data from SAP SD (VBAK, VBPA, KNA1) and aggregates by time period and organizational dimensions.


## Problem Description

Failure to monitor aggregated sales order values creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected high-value order concentrations can distort period-over-period sales trend analysis and forecasting accuracy
- Exceptional sales volumes in specific periods may indicate revenue recognition timing issues or premature booking
- Unusual value patterns in foreign currency transactions can signal currency risk exposure requiring hedging actions
- Aggregated sales anomalies may delay month-end close processes when discovered late during financial review
- Concentrated high-value orders in specific sales organizations or channels can mask underlying performance issues in other business units

**Sales Operations and Control Risks**
- Large transaction clusters without proper visibility may indicate unauthorized discounting or pricing violations
- Exceptional values in specific customer segments could signal credit risk concentration requiring management intervention
- Unusual sales patterns by partner functions (sold-to, ship-to, bill-to) may indicate customer master data quality issues
- High-volume activity in specific divisions or distribution channels may reflect operational bottlenecks or resource constraints
- Atypical aggregated values could indicate data entry errors or system integration failures requiring immediate correction

**Management Visibility and Decision-Making Risks**
- Lack of aggregated value monitoring delays executive awareness of significant business trends and market shifts
- Unidentified sales concentration patterns can lead to missed opportunities for strategic pricing or customer engagement
- Exceptional transaction volumes may require additional audit scrutiny or compliance review but go unnoticed
- Absence of multi-dimensional sales analysis limits ability to optimize sales territory assignments and resource allocation

## Suggested Resolution

**Immediate Response**
- Review the aggregated sales values flagged by the EI to understand the nature and scope of the exceptional pattern (threshold violations, value concentration, period-specific spikes)
- Verify the authenticity of high-value orders using transaction VA03 (Display Sales Order) to confirm legitimacy and proper authorization
- Check sales document status and processing progress to ensure no manual intervention or corrections are pending
- Identify the business context for exceptional volumes: promotional campaigns, large customer orders, seasonal patterns, or data quality issues

**System Assessment**
- Analyze the aggregation dimensions (time period, organizational scope) to understand which factors drive the exceptional pattern
- Review historical trends by comparing current aggregated values to prior periods using the same criteria
- Examine currency-specific patterns (document currency and foreign currency) to identify exposure or booking issues
- Assess partner and customer distribution to identify relationship patterns or master data inconsistencies
- Investigate document type and category to determine if exceptions are type-specific
- Validate the timing basis for aggregation (e.g. requested delivery date, document date, creation date) to ensure the monitoring window is appropriate

**Corrective Actions**
- If unauthorized or erroneous orders are identified, initiate sales document correction procedures using VA02 (Change Sales Order)
- For legitimate high-value orders requiring special approval, escalate to sales management and finance for validation
- Update customer master data (VD02) if partner or credit issues are detected
- Adjust pricing or discounting arrangements (VK11) if pricing violations are confirmed
- Implement additional monitoring controls by tightening threshold criteria for future executions
- Document exceptional patterns and business justifications for audit trail and management reporting purposes
- Establish recurring EI execution schedules to provide continuous visibility into sales value trends and concentration risks
- Configure alert routing to appropriate sales managers and finance stakeholders based on organizational responsibility


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AGGR_FIELDS | Aggregation Fields |  | 0 | 0 |  |  |
| 3 | AGGR_PERIOD | Aggregation Period(Y,Q,M,W) |  | 0 | 0 |  |  |
| 4 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 5 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 6 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 7 | BP1_CODE | Customer Code 1 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 8 | BP1_FUNCT | Partner Function 1 | CHAR | 2 | 0 | PARVW | PARVW |
| 9 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 10 | BP2_CODE | Customer code 2 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 11 | BP2_FUNCT | Partner Function 2 | CHAR | 2 | 0 | PARVW | PARVW |
| 12 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 13 | BP3_CODE | Customer code 3 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 14 | BP3_FUNCT | Partner Function 3 | CHAR | 2 | 0 | PARVW | PARVW |
| 15 | BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 16 | BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 17 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 18 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 19 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 20 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 21 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 22 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 23 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 24 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 25 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 26 | NETWR_FR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 27 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 28 | STAT | Status | CHAR | 5 | 0 | J_STATUS | J_STATUS |
| 29 | TOT_CNT | Natural number | INT4 | 10 | 0 | INT4 | INT4 |
| 30 | TOT_NETWR | Total Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 31 | TOT_NETWR_FR | Tot. Foreign Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 32 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 33 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 34 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 35 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 36 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 37 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 38 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 39 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 40 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 41 | WAERK_FR | Foreign Currency | CUKY | 5 | 0 | WAERK | WAERS |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 41 parameters listed in the Parameters Reference Table above.

**AEDAT** (Changed On):

Change date of the sales document. Filters and appears in the result; used for selection and display.

**AGGR_FIELDS** (Aggregation Fields):

Names of output structure fields used as aggregation keys. Together with AGGR_PERIOD and DATE_REF_FLD, determines how sales data is grouped (e.g. by sold-to, sales org, period). Values are field names from the output structure; see structure and code for allowed fields.

**AGGR_PERIOD** (Aggregation Period(Y,Q,M,W)):

Time bucket for aggregation: month, week, quarter, or year. Used with DATE_REF_FLD to build aggregation periods from the reference date. Determines how sales values and counts are grouped by time.

**AGGR_PERIOD Options:**
- **M**: Month.
- **W**: Week.
- **Q**: Quarter.
- **Y**: Year.

**AUART** (Sales Document Type):

Sales document type. Filters sales orders by type (e.g. OR, RE); restricts which documents are aggregated.

**AUDAT** (Document Date):

Document date of the sales order. Filters and appears in the result; used for selection and display.

**BACKDAYS** (Days Backward from today):

How many days to look back from today. When no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window for sales orders. Used to derive the default date range when DATUM is initial.

**BP1_CODE** (Customer Code 1):

Customer number for first partner role. Filters by partner; used with BP1_FUNCT and BP1_NAME for partner 1.

**BP1_FUNCT** (Partner Function 1):

Partner function for first partner (e.g. sold-to, ship-to). Filters and appears in the result; used with BP1_CODE.

**BP1_NAME** (Name):

Name for first partner. Populated from customer master; used for display with BP1_CODE.

**BP2_CODE** (Customer code 2):

Customer number for second partner role. Filters by partner; used with BP2_FUNCT and BP2_NAME.

**BP2_FUNCT** (Partner Function 2):

Partner function for second partner. Filters and appears in the result; used with BP2_CODE.

**BP2_NAME** (Name):

Name for second partner. Populated from customer master; used for display with BP2_CODE.

**BP3_CODE** (Customer code 3):

Customer number for third partner role. Filters by partner; used with BP3_FUNCT and BP3_NAME.

**BP3_FUNCT** (Partner Function 3):

Partner function for third partner. Filters and appears in the result; used with BP3_CODE.

**BP3_NAME** (Name):

Name for third partner. Populated from customer master; used for display with BP3_CODE.

**BSTNK** (Purchase order no.):

Customer purchase order number. Filters and appears in the result.

**CUST_DESC** (Name):

Customer name. Populated from customer master; used for display.

**DATE_REF_FLD** (Date reference field):

Name of the date field used as the reference for aggregation periods. Determines which date (e.g. requested delivery date, document date, creation date) is used to build the time bucket (AGGR_PERIOD). Used with BACKDAYS and AGGR_PERIOD.

**DATE_REF_FLD Options:**
- **VDATU**: Requested delivery date (default in code).
- **AEDAT**: Changed on.
- **AUDAT**: Document date.
- **ERDAT**: Created on.
- Other date fields from the output structure may be used depending on function logic.

**DATE_REF_FLD and BACKDAYS Connection:**
DATE_REF_FLD determines which date is used for the time window and aggregation period. The selection window is built from today minus BACKDAYS when no explicit date range is supplied.

**DURATION** (Duration In Time Units):

Duration in the unit given by DURATION_UNIT (e.g. days). Used with DATE_REF_FLD and DURATION_UNIT when filtering by age or time span. Determines the length of the monitoring window in the chosen unit.

**DURATION_UNIT** (Duration Unit(D/H/M)):

Unit for DURATION (e.g. days, hours). Used with DURATION and DATE_REF_FLD for duration-based filtering.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours.
- **M**: Minutes.

**ERDAT** (Created On):

Creation date of the sales document. Filters and appears in the result; used for selection and display.

**ERNAM** (Created By):

User who created the sales document. Filters and appears in the result.

**ERZET** (Time):

Creation time. Filters and appears in the result; used for selection and display.

**KUNNR** (Sold-to party):

Sold-to party (customer). Filters sales orders by sold-to; restricts which documents are aggregated and returned.

**NETWR** (Net value):

Net value in document currency. Filters and appears in the result; used for threshold or range filtering.

**NETWR_FR** (Net value):

Net value in foreign currency. Filters and appears in the result; used for threshold or range filtering.

**SPART** (Division):

Division. Filters sales orders by division; restricts which documents are aggregated.

**STAT** (Status):

Sales document status. Filters and appears in the result; restricts which documents are included.

**TOT_CNT** (Natural number):

Aggregated count of documents per period and aggregation key. Filters and appears in the result; used for threshold filtering (e.g. minimum count).

**TOT_NETWR** (Total Net value):

Aggregated net value in document currency per period and aggregation key. Filters and appears in the result; used for threshold filtering.

**TOT_NETWR_FR** (Tot. Foreign Net value):

Aggregated net value in foreign currency per period and aggregation key. Filters and appears in the result; used for threshold filtering.

**USER_FLD** (Dynamic Recipient User Field):

Dynamic user field for recipient or alert routing. Used for workflow or notification; see function usage for allowed values.

**VBELN** (Sales Document):

Sales document number. Filters and appears in the result; identifies the document.

**VBTYP** (SD document categ.):

SD document category. Filters and appears in the result; restricts document type (e.g. order, delivery).

**VDATU** (Requested deliv.date):

Requested delivery date. Filters and appears in the result; used for selection and as a common date reference for aggregation.

**VKBUR** (Sales Office):

Sales office. Filters sales orders by sales office; restricts which documents are aggregated.

**VKGRP** (Sales Group):

Sales group. Filters sales orders by sales group; restricts which documents are aggregated.

**VKORG** (Sales Organization):

Sales organization. Filters sales orders by sales organization; restricts which documents are aggregated.

**VTWEG** (Distribution Channel):

Distribution channel. Filters sales orders by distribution channel; restricts which documents are aggregated.

**WAERK** (Document Currency):

Document currency of the sales order. Represents the currency in which the document is posted. Used for display and for filtering or aggregating by currency; filters and appears in the result.

**WAERK_FR** (Foreign Currency):

Foreign currency (e.g. second currency) for the sales order. Represents the foreign currency amount for reporting or comparison. Used for display and for filtering or aggregating by foreign currency; filters and appears in the result.


### Parameter Relationships

**Time and aggregation parameters**
- **BACKDAYS**, **DATE_REF_FLD**, **AGGR_PERIOD**, and **DATUM** work together to define the time window and aggregation buckets. When no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window. DATE_REF_FLD determines which date field is used to build aggregation periods (AGGR_PERIOD: month, week, quarter, year). DATUM filters the underlying sales documents by date.
- **DURATION** and **DURATION_UNIT**: DURATION (in DURATION_UNIT) defines the length of the monitoring window or time span when used with DATE_REF_FLD; DURATION_UNIT gives the unit (e.g. D = days, H = hours).

**Aggregation parameters**
- **AGGR_FIELDS**, **AGGR_PERIOD**, and **DATE_REF_FLD**: AGGR_FIELDS lists the output structure fields used as aggregation keys (e.g. KUNNR, VKORG). AGGR_PERIOD defines the time bucket (M/W/Q/Y). DATE_REF_FLD determines which date is used to compute the period. Together they define how sales values and counts are grouped (by period and by aggregation key).
- **TOT_CNT**, **TOT_NETWR**, **TOT_NETWR_FR**: Aggregated count and net values (document and foreign currency) per period and aggregation key. Used together for threshold filtering (e.g. minimum count or minimum value).

**Partner parameters**
- **BP1_CODE**, **BP1_FUNCT**, **BP1_NAME** (and BP2, BP3): Partner code filters by customer number; partner function identifies the role (sold-to, ship-to, etc.); name is populated for display. Use together to filter and display by partner role.

**Organizational parameters**
- **VKORG**, **VTWEG**, **VKBUR**, **VKGRP**, **SPART**: Sales organization, distribution channel, sales office, sales group, division. Filter sales orders by organizational dimension; together they restrict which documents are aggregated and returned.

**Currency parameters**
- **WAERK**, **WAERK_FR**: Document currency and foreign currency. Used for display and for filtering or aggregating by currency; NETWR and NETWR_FR (and TOT_NETWR, TOT_NETWR_FR) are in these currencies.


### Default Values

- **BACKDAYS** — When not supplied, default is 0; when no date range is supplied, the EI may build a default range. Used to derive the default date window (today minus BACKDAYS to today) when DATUM is initial.
- **DATE_REF_FLD** — Default in code: VDATU (requested delivery date); used as the reference for aggregation periods when AGGR_PERIOD is set.
- **AGGR_PERIOD** — Default in code: M (month); used to build time buckets for aggregation when aggregation is performed.
- **DATUM** — When not supplied, the EI may build a default range from today minus BACKDAYS to today for filtering sales documents.

**Note:** When date range parameters are initial, the EI builds default ranges from today and BACKDAYS. DATE_REF_FLD determines which date field is used for aggregation periods; AGGR_PERIOD determines the time bucket (month, week, quarter, year).

### Practical Configuration Examples

**Use Case 1: Credit memo monthly volume by payer – last 90 days**
```
BACKDAYS = 90
AGGR_PERIOD = M
DATE_REF_FLD = VDATU
AGGR_FIELDS = KUNNR (or sold-to field)
TOT_NETWR = (e.g. GE 1000000 for $1M threshold)
```
**Purpose:** Aggregate sales order net value by month and sold-to party for the last 90 days; flag periods where aggregated value exceeds a threshold (e.g. $1M in LC).

**Use Case 2: Weekly aggregation by sales organization**
```
BACKDAYS = 30
AGGR_PERIOD = W
DATE_REF_FLD = ERDAT
AGGR_FIELDS = VKORG
VKORG = (optional range)
```
**Purpose:** Aggregate by week (creation date) and sales organization for the last 30 days; suitable for weekly sales performance exception monitoring.

**Use Case 3: High-value concentration by customer and document currency**
```
BACKDAYS = 60
AGGR_PERIOD = M
AGGR_FIELDS = KUNNR WAERK
TOT_NETWR = GE 1000000
TOT_CNT = GE 1
```
**Purpose:** Identify customers and document currencies with monthly aggregated net value of at least $1M and at least one document; suitable for credit memo or sales value concentration review.


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_ORD_VAL_TOT | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_ORD_VAL_TOT | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_VAL_TOT | AUDAT | Document Date (Date Received/Sent) | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP1_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP1_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP1_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP2_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP2_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP2_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP3_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP3_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BP3_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAL_TOT | BSTNK | Customer purchase order number | CHAR(20) | BSTNK |
| /SKN/S_SW_10_01_ORD_VAL_TOT | CUST_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAL_TOT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ORD_VAL_TOT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ORD_VAL_TOT | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_ORD_VAL_TOT | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_ORD_VAL_TOT | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_ORD_VAL_TOT | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_ORD_VAL_TOT | NETWR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_VAL_TOT | NETWR_FR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_VAL_TOT | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_VAL_TOT | TOT_CNT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_01_ORD_VAL_TOT | TOT_NETWR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_VAL_TOT | TOT_NETWR_FR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_VAL_TOT | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_VAL_TOT | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_VAL_TOT | VDATU | Requested delivery date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_ORD_VAL_TOT | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_ORD_VAL_TOT | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_ORD_VAL_TOT | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_VAL_TOT | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_VAL_TOT | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_VAL_TOT | WAERK_FR | SD Document Currency | CUKY(5) | WAERK |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ORD_VAL_TOT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_VAL_TOT OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DATA_DET TYPE /SKN/S_SW_10_01_ORD_VAL_NEW,
      LT_DATA_DET LIKE TABLE OF LS_DATA_DET.
*FIELD-SYMBOLS <fs_DATA_DET> type /SKN/S_SW_10_01_ORD_VAL_NEW.
DATA: LS_DATA LIKE LINE OF T_DATA.
TYPES: TY_AGGR_PERIOD(8) TYPE N,
       TY_AGGR_ARG TYPE STRING.
DATA: LV_AGGR_PERIOD_VAL TYPE TY_AGGR_PERIOD,
      LV_AGGR_ARG_VAL TYPE TY_AGGR_ARG.
DATA: BEGIN OF LS_DET_AGGR.
  INCLUDE STRUCTURE /SKN/S_SW_10_01_ORD_VAL_NEW.
DATA: AGGR_PERIOD TYPE TY_AGGR_PERIOD,
      AGGR_ARG TYPE TY_AGGR_ARG.
DATA: END OF LS_DET_AGGR.
DATA: LT_DET_AGGR LIKE TABLE OF LS_DET_AGGR.
FIELD-SYMBOLS <FS_DET_AGGR> LIKE LS_DET_AGGR.
DATA: BEGIN OF LS_AGGR,
        AGGR_PERIOD  TYPE TY_AGGR_PERIOD,
        AGGR_ARG     TYPE TY_AGGR_ARG,
        TOT_NETWR	   TYPE NETWR_AK,
        TOT_NETWR_FR TYPE NETWR_AK,
        TOT_CNT      TYPE INT4,
      END OF LS_AGGR.
*data: lt_AGGR like  TABLE OF ls_AGGR.
DATA: LT_AGGR LIKE HASHED TABLE OF LS_AGGR
               WITH UNIQUE KEY AGGR_ARG AGGR_PERIOD.
*FIELD-SYMBOLS <fs_AGGR> like ls_AGGR.
*data: begin of ls_AGGR_key,
*        aggr_period  type ty_aggr_period,
*        aggr_arg     type ty_aggr_arg,
*      end of ls_AGGR_key.
DATA : FLD(60) TYPE C .
DATA : REF_DATE TYPE D.
FIELD-SYMBOLS:  TYPE ANY ,
               <FS_V> TYPE ANY .
DATA : REF_STRING(60) TYPE C.
DATA: BEGIN OF LS_AGGR_FIELDS,
       FIELDNAME TYPE FIELDNAME,
      END OF LS_AGGR_FIELDS.
DATA: LT_AGGR_FIELDS LIKE TABLE OF LS_AGGR_FIELDS.
DATA:  DATE_FROM LIKE SY-DATUM,
       DATE_TO LIKE SY-DATUM,
       DATE_FROM_START_PERIOD LIKE SY-DATUM.
DATA_MULTY: AGGR_FIELDS  NAME_KOMP,
            TOT_NETWR	   NETWR_AK,
            TOT_NETWR_FR NETWR_AK,
            TOT_CNT      INT4,
            DATUM        SY-DATUM,
            STAT         J_STATUS.
DATA_SINGLE: AGGR_PERIOD  CHAR1,  " M - Month/W - Week/Q - Qurter/Y - Year
             DATE_REF_FLD NAME_FELD,
             BACKDAYS     INT4,
             FORWDAYS     INT4.
SELECT_MULTY: DATUM,
              AGGR_FIELDS,
              TOT_NETWR,
              TOT_NETWR_FR,
              TOT_CNT,
              STAT.
 LV_DATE_REF_FLD = 'VDATU'."Delivery Date)
 LV_AGGR_PERIOD = 'M'.
 LV_BACKDAYS = 0.
 SELECT_SINGLE: AGGR_PERIOD,
                DATE_REF_FLD,
                BACKDAYS,
                FORWDAYS.
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
 REFRESH LT_AGGR_FIELDS.
 IF R_AGGR_FIELDS[] IS NOT INITIAL.
   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE LT_AGGR_FIELDS
     FROM DD03L
     WHERE TABNAME = '/SKN/S_SW_10_01_ORD_VAL_TOT'
       AND FIELDNAME IN R_AGGR_FIELDS.
 ENDIF.
 "--- Prepare Dates for Aggregation (Month/...)
  "-- Save Orogina Dates Selection
   DATA_MULTY: DATUM_SRC    SY-DATUM. " To save original Selection
   R_DATUM_SRC[] = R_DATUM[].
  "-- Set full Date Period Interval
    PERFORM CALC_AGGR_PERIOD_START_DATE USING DATE_FROM LV_AGGR_PERIOD
                                        CHANGING DATE_FROM_START_PERIOD.
    READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
    IF SY-SUBRC IS INITIAL.
      RS_DATUM-LOW = DATE_FROM_START_PERIOD.
      MODIFY R_DATUM FROM RS_DATUM INDEX SY-TABIX.
    ENDIF.
    "--- Substitute DATUM condition in T_SELECT
    DELETE T_SELECT WHERE FIELDNM = 'DATUM'.
    LOOP AT R_DATUM INTO RS_DATUM.
      MOVE-CORRESPONDING RS_DATUM TO T_SELECT.
      T_SELECT-FIELDNM = 'DATUM'.
      APPEND T_SELECT.
    ENDLOOP.
 CALL FUNCTION '/SKN/F_SW_10_01_ORD_VAL_NEW'
   IMPORTING
     IS_ALERT       = IS_ALERT
   TABLES
     T_SELECT       = T_SELECT
     T_DATA         = LT_DATA_DET.
  "--- Prepare Aggregation Arguments
  LOOP AT LT_DATA_DET INTO LS_DATA_DET.
    MOVE-CORRESPONDING LS_DATA_DET TO LS_DET_AGGR.
    "--- Fill Aggregation Arguments
     "-- Aggr Period
      CONCATENATE 'ls_DATA_DET-' LV_DATE_REF_FLD INTO FLD .
      ASSIGN (FLD) TO .
      IF  IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      REF_DATE =  .
      PERFORM CALCULATE_AGGR_PERIOD_VAL USING REF_DATE LV_AGGR_PERIOD
                                        CHANGING LV_AGGR_PERIOD_VAL.
      LS_DET_AGGR-AGGR_PERIOD = LV_AGGR_PERIOD_VAL.
     "-- Aggr Key
      CLEAR LV_AGGR_ARG_VAL.
      LOOP AT LT_AGGR_FIELDS INTO LS_AGGR_FIELDS.
        CONCATENATE 'ls_DATA_DET-' LS_AGGR_FIELDS-FIELDNAME INTO FLD .
        ASSIGN (FLD) TO .
        IF  IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.
        WRITE  TO REF_STRING.
        CONCATENATE LV_AGGR_ARG_VAL REF_STRING INTO LV_AGGR_ARG_VAL.
      ENDLOOP.
      LS_DET_AGGR-AGGR_ARG = LV_AGGR_ARG_VAL.
    APPEND LS_DET_AGGR TO LT_DET_AGGR.
    MOVE-CORRESPONDING LS_DET_AGGR TO LS_AGGR.
    LS_AGGR-TOT_CNT = 1.
    LS_AGGR-TOT_NETWR = LS_DET_AGGR-NETWR.
    LS_AGGR-TOT_NETWR_FR = LS_DET_AGGR-NETWR_FR.
    COLLECT LS_AGGR INTO LT_AGGR.
  ENDLOOP.
  "--- Total Filtering
  DELETE LT_AGGR WHERE TOT_CNT NOT IN R_TOT_CNT.
  DELETE LT_AGGR WHERE TOT_NETWR NOT IN R_TOT_NETWR.
  DELETE LT_AGGR WHERE TOT_NETWR_FR NOT IN R_TOT_NETWR_FR.
  "--- Fill result Table
  LOOP AT LT_DET_AGGR INTO LS_DET_AGGR.
    "--- Filter for Original Date Selection
     "-- Get Date ref Field
      CONCATENATE 'ls_DET_AGGR-' LV_DATE_REF_FLD INTO FLD .
      ASSIGN (FLD) TO .
      IF  IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      REF_DATE =  .
      "-- Filter
      IF REF_DATE NOT IN R_DATUM_SRC.
        CONTINUE.
      ENDIF.
    READ TABLE LT_AGGR INTO LS_AGGR
                       WITH KEY AGGR_PERIOD = LS_DET_AGGR-AGGR_PERIOD
                                AGGR_ARG    = LS_DET_AGGR-AGGR_ARG.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LS_DET_AGGR TO LS_DATA.
      MOVE-CORRESPONDING LS_AGGR TO LS_DATA.
      APPEND LS_DATA TO T_DATA.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```