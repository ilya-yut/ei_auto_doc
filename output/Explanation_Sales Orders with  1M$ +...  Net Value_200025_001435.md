# Exception Indicator: Exceptional sales documents values – Doc. level - SW_10_01_ORD_VAL_NEW

## General Overview

This Exception Indicator (EI) monitors sales orders with exceptional net values at the document level, identifying high-value orders that exceed configurable thresholds. It provides visibility into sales order values across configurable time periods and organizational dimensions, supporting detection of unusual value concentrations, high-value transactions, and sales pattern anomalies that require management attention.

This EI serves as an essential control for sales management and financial oversight by:
- Enabling detection of exceptional sales order values and value concentrations that exceed predefined thresholds and require management attention
- Supporting identification of high-value transactions and unusual sales patterns for revenue recognition and audit review
- Providing visibility into sales values by time period and organizational dimension for performance and resource allocation decisions
- Enabling analysis by sold-to party, partner roles (e.g. bill-to, ship-to), and document type for accountability and relationship visibility
- Supporting accountability and relationship visibility by business partner roles for dispute and fulfillment oversight

The EI is valuable for month-end close processes, sales performance reviews, and financial exception management. It helps organizations detect high-value transaction clusters, unusual sales concentration patterns, and potential revenue recognition issues requiring executive visibility.


## Problem Description

Failure to monitor exceptional sales order values creates multiple risks across financial reporting, operational management, and compliance:

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
- Lack of value monitoring delays executive awareness of significant business trends and market shifts
- Unidentified sales concentration patterns can lead to missed opportunities for strategic pricing or customer engagement
- Exceptional transaction volumes may require additional audit scrutiny or compliance review but go unnoticed
- Absence of multi-dimensional sales analysis limits ability to optimize sales territory assignments and resource allocation

## Suggested Resolution

**Immediate Response**
- Review the flagged sales orders to understand the nature and scope of the exceptional pattern (threshold violations, value concentration, period-specific spikes)
- Verify the authenticity of high-value orders using transaction VA03 (Display Sales Order) to confirm legitimacy and proper authorization
- Check sales document status and processing progress to ensure no manual intervention or corrections are pending
- Identify the business context for exceptional volumes: promotional campaigns, large customer orders, seasonal patterns, or data quality issues

**System Assessment**
- Analyze the aggregation dimensions (time period, organizational scope) to understand which factors drive the exceptional pattern
- Review historical trends by comparing current values to prior periods using the same criteria
- Examine currency-specific patterns (document currency and foreign currency) to identify exposure or booking issues
- Assess partner and customer distribution to identify relationship patterns or master data inconsistencies
- Investigate document type and category to determine if exceptions are type-specific
- Validate the timing basis (e.g. requested delivery date, document date, creation date) to ensure the monitoring window is appropriate

**Corrective Actions**
- If unauthorized or erroneous orders are identified, initiate sales document correction procedures using VA02 (Change Sales Order)
- For legitimate high-value orders requiring special approval, escalate to sales management and finance for validation
- Update customer master data (VD02) if partner or credit issues are detected
- Adjust pricing or discounting arrangements using VK11 if pricing violations are confirmed
- Implement additional monitoring controls by tightening threshold criteria for future executions
- Document exceptional patterns and business justifications for audit trail and management reporting purposes
- Establish recurring EI execution schedules to provide continuous visibility into sales value trends and concentration risks


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 3 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 4 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 5 | BP1_CODE | Customer code 1 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 6 | BP1_FUNCT | Partner Function 1 | CHAR | 2 | 0 | PARVW | PARVW |
| 7 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 8 | BP2_CODE | Customer code 2 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 9 | BP2_FUNCT | Partner Function 2 | CHAR | 2 | 0 | PARVW | PARVW |
| 10 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 11 | BP3_CODE | Customer code 3 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 12 | BP3_FUNCT | Partner Function 3 | CHAR | 2 | 0 | PARVW | PARVW |
| 13 | BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 14 | BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 15 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 16 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 17 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 18 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 19 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 20 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 21 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 22 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 23 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 24 | NETWR_FR | Net value in stat Currency | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 25 | OBJNR | Object no.header | CHAR | 22 | 0 | OBJKO | J_OBJNR |
| 26 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 27 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 28 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 29 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 30 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 31 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 32 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 33 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 34 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 35 | WAERK_FR | Statistics Currency (USD) | CUKY | 5 | 0 | /SKN/WAERK_FR | WAERS |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 35 parameters listed in the Parameters Reference Table above.

**AEDAT** (Changed On):

Date when the sales document was last changed. The EI uses it as the reference date for the monitoring window when DATE_REF_FLD is set to AEDAT; the chosen date field is restricted to the lookback window when reading sales order data.

**AUART** (Sales Document Type):

Sales document type (e.g. OR, RE). The EI restricts which order types are read; each type can have different value and business semantics.

**AUDAT** (Document Date):

Document date (date received/sent) on the sales order. Can be used as the reference date for the monitoring window when DATE_REF_FLD is set to AUDAT; the EI then restricts this field to the lookback window.

**BACKDAYS** (Days Backward from today):

Number of days used to build the monitoring window. When no date range is supplied, the EI uses today minus this value as the start date; the chosen reference date field (ERDAT, AEDAT, AUDAT, or VDATU) is restricted to that window when reading sales order data.

**BP1_CODE - BP3_CODE** (Customer code 1 – Customer code 3):

Customer or business partner codes for up to three partner roles (e.g. sold-to, bill-to, ship-to). Each pair BPn_FUNCT + BPn_CODE defines a partner role and the customer code to filter by; the EI reads partner data from VBPA and filters by these codes.

**BP1_FUNCT - BP3_FUNCT** (Partner Function 1 – Partner Function 3):

Partner function codes (e.g. RG, RE, WE) for up to three partner roles. Each pair BPn_FUNCT + BPn_CODE defines which partner role and which customer code to filter by; the EI uses these to scope which business partners are evaluated.

**BP1_FUNCT - BP3_FUNCT Options:**
- **RG**: Sold-to party; **RE**: Bill-to party; **WE**: Ship-to party; **AG**: Payer. Other partner function values as in standard SAP (PARVW domain).

**BP1_FUNCT and BP1_CODE Connection:** BP1_FUNCT defines the partner role; BP1_CODE holds the customer code for that role. Set both when filtering by a specific partner (e.g. bill-to = customer X). Same for BP2 and BP3.

**BP1_NAME - BP3_NAME** (Name – Name):

Names of the business partners for roles 1–3, resolved from master data for display.

**BSTNK** (Purchase order no.):

Customer purchase order number on the sales document. The EI includes it in selection so configurations can focus on specific customer PO references.

**CUST_DESC** (Name):

Customer name from master data (KNA1) for the sold-to party.

**DATE_REF_FLD** (Date reference field):

Selects which date field on the sales order is used for the monitoring window and for duration calculation: creation date (ERDAT), changed-on date (AEDAT), document date (AUDAT), or requested delivery date (VDATU). The EI applies the lookback window to the chosen field when reading data.

**DATE_REF_FLD Options:**
- **ERDAT**: Date on which record was created.
- **AEDAT**: Changed on.
- **AUDAT**: Document date (date received/sent).
- **VDATU**: Requested delivery date; default in the code.

**BACKDAYS and DATE_REF_FLD Connection:** BACKDAYS defines the lookback length; DATE_REF_FLD defines which date field is restricted to that window. Set both when configuring the monitoring window.

**DURATION** (Duration In Time Units):

Elapsed time between the reference date (ERDAT, AEDAT, AUDAT, or VDATU, as configured) and the evaluation date, in the unit given by DURATION_UNIT. The EI calculates this per order and uses it for duration-based filtering.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed and evaluated (hours, minutes, days, or full days for specific-day logic). The EI uses this when computing and comparing duration for each order.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** DURATION holds the numeric value; DURATION_UNIT defines its meaning. Set both when using duration-based filtering (e.g. orders with duration equal to 30 full days).

**ERDAT** (Created On):

Date when the sales document was created. Can be used as the reference date for the monitoring window when DATE_REF_FLD is set to ERDAT; the EI then restricts creation date to the lookback window.

**ERNAM** (Created By):

User who created the sales document. The EI includes it in selection and result for creator-based scope or accountability.

**ERZET** (Time):

Entry time when the sales document was created. The EI includes it in the result for timestamp context.

**KUNNR** (Sold-to party):

Sold-to party customer number on the sales order. The EI restricts and reports by sold-to for customer-level monitoring.

**NETWR** (Net value):

Net value of the sales order in document currency. The EI filters by this (R_NETWR) so only orders meeting the value threshold or range are included; it is the core field for high-value order detection.

**NETWR_FR** (Net value in stat Currency):

Net value of the sales order in the statistics (foreign) currency (WAERK_FR). The EI converts document currency to WAERK_FR when they differ and filters by this for value-based scope in a common currency.

**OBJNR** (Object no.header):

Object number at header level for status determination. The EI uses it to call STATUS_READ when STAT filtering is applied; it links the order to the status object.

**SPART** (Division):

Division. The EI restricts and reports by division for organizational scope.

**VBELN** (Sales Document):

Sales document number. The EI reads order header data keyed by this identifier; values scope which orders are evaluated for exceptional value.

**VBTYP** (SD document categ.):

SD document category (e.g. order, contract). The EI includes it in selection and result for scope by category.

**VBTYP Options:**
- **C**: Order; **A**: Contract; **B**: Scheduling agreement. Other domain values as in standard SAP.

**VDATU** (Requested deliv.date):

Requested delivery date. The EI uses it as the default reference date for the monitoring window when DATE_REF_FLD is not supplied; when set to VDATU, the EI restricts this field to the lookback window.

**VKBUR** (Sales Office):

Sales office. The EI restricts and reports by sales office for organizational scope.

**VKGRP** (Sales Group):

Sales group. The EI restricts and reports by sales group for responsibility-based scope.

**VKORG** (Sales Organization):

Sales organization. The EI restricts and reports by sales organization for organizational scope.

**VTWEG** (Distribution Channel):

Distribution channel. The EI restricts and reports by distribution channel for organizational scope.

**WAERK** (Document Currency):

Document currency of the sales order; amounts such as NETWR are expressed in this currency.

**WAERK_FR** (Statistics Currency (USD)):

Foreign or statistics currency (e.g. USD) used for value conversion. When set, the EI converts NETWR from document currency to this currency and populates NETWR_FR; filtering by NETWR_FR then applies in the common currency.


### Parameter Relationships

**Time-Based and Duration Parameters:**
- **BACKDAYS** defines how many days to look back from today when no date range is supplied; the EI builds the monitoring window from today minus this value.
- **DATE_REF_FLD** selects which date field on the sales order is used for that window: ERDAT (creation), AEDAT (changed on), AUDAT (document date), or VDATU (requested delivery date). The chosen field is restricted to the window when the EI reads order data.
- **DURATION** and **DURATION_UNIT** work together: DURATION holds the elapsed time (in the unit given by DURATION_UNIT) between the reference date and the evaluation date; the EI calculates this per order and uses it for duration-based filtering. Set both when filtering by how long ago the order was created or by requested delivery.

**Business Partner Analysis Parameters:**
- **BP1_FUNCT** and **BP1_CODE** work together: BP1_FUNCT defines the partner role (e.g. bill-to); BP1_CODE holds the customer code for that role. The EI uses them to filter by specific partners. Same for **BP2_FUNCT** + **BP2_CODE** and **BP3_FUNCT** + **BP3_CODE**.
- Use these pairs when focusing on specific sold-to, bill-to, ship-to, or other partner roles and their customer codes.

**Value and Currency Parameters:**
- **NETWR** filters by net value in document currency; **NETWR_FR** filters by net value in the statistics currency. **WAERK_FR** defines the target currency for conversion; when set, the EI converts NETWR to NETWR_FR and filtering by NETWR_FR applies in that currency. Use WAERK_FR together with NETWR_FR when monitoring high-value orders in a common currency (e.g. USD).


### Default Values

- **BACKDAYS** — Default: `1` (when no date range is supplied, the EI uses a 1-day lookback from today for the monitoring window).
- **DATE_REF_FLD** — Default: `VDATU` (requested delivery date is used as the reference date for the monitoring window and duration calculation when not supplied).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).

**Note:** When no date range is supplied, the EI builds the monitoring window from today minus BACKDAYS and applies it to the date field selected by DATE_REF_FLD (ERDAT, AEDAT, AUDAT, or VDATU).

### Practical Configuration Examples

**Use Case 1: Last 7 days by requested delivery date**

```
BACKDAYS = 7
DATE_REF_FLD = VDATU
```

**Purpose:** Monitor sales orders with requested delivery date in the last 7 days for exceptional value. Suitable for routine weekly high-value order review.

**Use Case 2: High-value orders by sales organization and document type**

```
VKORG = 1000, 2000
AUART = OR, RE
NETWR = 1000000–999999999
```

**Purpose:** Limit results to specific sales organizations and document types (e.g. orders, credit memos) with net value between 1M and 999M in document currency. Supports regional or type-specific high-value monitoring.

**Use Case 3: Duration in full days, reference date, and value threshold**

```
DATE_REF_FLD = VDATU
DURATION_UNIT = F
DURATION = 30
NETWR = 500000–999999999
BACKDAYS = 90
```

**Purpose:** Flag orders with requested delivery date exactly 30 full days ago, within a 90-day lookback, and net value between 500K and 999M. Useful for age-based prioritization of high-value orders. DURATION is a single value when using DURATION_UNIT = F.

**Use Case 4: Bill-to party, statistics currency, and organizational scope**

```
BP1_FUNCT = RE
BP1_CODE = 0000100001–0000100100
WAERK_FR = USD
NETWR_FR = 1000000–999999999
VKORG = 1000
VTWEG = 10
```

**Purpose:** Focus on bill-to customers in a range, with net value in USD between 1M and 999M, for a specific sales organization and distribution channel. Supports multi-currency high-value monitoring by partner role.


### EI Function Structure

## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_ORD_VAL_NEW | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_ORD_VAL_NEW | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_VAL_NEW | AUDAT | Document Date (Date Received/Sent) | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP1_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP1_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP1_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP2_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP2_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP2_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP3_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP3_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BP3_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAL_NEW | BSTNK | Customer purchase order number | CHAR(20) | BSTNK |
| /SKN/S_SW_10_01_ORD_VAL_NEW | CUST_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAL_NEW | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ORD_VAL_NEW | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ORD_VAL_NEW | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_ORD_VAL_NEW | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_ORD_VAL_NEW | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_ORD_VAL_NEW | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_ORD_VAL_NEW | NETWR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_VAL_NEW | NETWR_FR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_VAL_NEW | OBJNR | Object number at header level | CHAR(22) | OBJKO |
| /SKN/S_SW_10_01_ORD_VAL_NEW | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_VAL_NEW | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_VAL_NEW | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_VAL_NEW | VDATU | Requested delivery date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_ORD_VAL_NEW | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_ORD_VAL_NEW | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_ORD_VAL_NEW | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_VAL_NEW | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_VAL_NEW | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_VAL_NEW | WAERK_FR | Foreign Currency | CUKY(5) | /SKN/WAERK_FR |

### ABAP Code

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ORD_VAL_NEW .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_VAL_NEW OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
             LANGU          LANGU,
             BACKDAYS       INT4,
             BP1_FUNCT      PARVW,
             BP2_FUNCT      PARVW,
             BP3_FUNCT      PARVW,
             DATE_REF_FLD   NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             WAERK_FR       WAERK.
 LV_BACKDAYS = 1.
 LV_DATE_REF_FLD = 'VDATU'.
 LV_DURATION_UNIT = 'D'.
 SELECT_SINGLE: MANAGE_IN_UTC,
                LANGU,
                BACKDAYS,
                BP1_FUNCT,
                BP2_FUNCT,
                BP3_FUNCT,
                DATE_REF_FLD,
                DURATION_UNIT,
                WAERK_FR.
DATA_MULTY: KUNNR        VBAK-KUNNR,
            VBELN        VBAK-VBELN,
            VKORG        VBAK-VKORG,
            VTWEG        VBAK-VTWEG,
            BSTNK        VBAK-BSTNK,
            NETWR        VBAK-NETWR,
            NETWR_FR     VBAK-NETWR,
            VBTYP        VBAK-VBTYP,
            AUART        VBAK-AUART,
            SPART        VBAK-SPART,
            VKGRP        VBAK-VKGRP,
            VKBUR        VBAK-VKBUR,
            AEDAT        VBAK-AEDAT,
            AUDAT        VBAK-AUDAT,
            ERDAT        VBAK-ERDAT,
            VDATU        VBAK-VDATU,
            DATUM        SY-DATUM,
            DURATION     /SKN/E_SW_DURATION,
            STAT         J_STATUS,
            BP1_CODE     KUNNR,
            BP2_CODE     KUNNR,
            BP3_CODE     KUNNR,
            BP_FUNCT     PARVW.
SELECT_MULTY: KUNNR,
            VBELN,
            VKORG ,
            VTWEG ,
            BSTNK,
            NETWR,
            NETWR_FR,
            AUART,
            VBTYP,
            SPART,
            VKGRP,
            VKBUR,
            AUDAT,
            AEDAT,
            ERDAT,
            DATUM,
            VDATU,
            DURATION,
            STAT,
            BP1_CODE,
            BP2_CODE,
            BP3_CODE.
CONVERT_MULTY: KUNNR ALPHA,
               VBELN ALPHA,
               BP1_CODE ALPHA,
               BP2_CODE ALPHA,
               BP3_CODE ALPHA.
CONVERT_SINGLE: BP1_FUNCT PARVW ,
                BP2_FUNCT PARVW ,
                BP3_FUNCT PARVW .
CONVERT_MULTY: AUART AUART .
RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
         R_FLD_VAL FOR DD03P-FIELDNAME .
DATA : FLD_NAME TYPE FIELDNAME.
DATA : I TYPE I,
       CI(1) TYPE C,
       NFIELDS TYPE I VALUE 3.
DATA : BACKDAYS  TYPE I ,
       FORWDAYS TYPE I,
       DATE_FROM LIKE SY-DATUM,
       DATE_TO   LIKE SY-DATUM .
DATA : LANGU LIKE SY-LANGU .
DATA : IS_OUT(1) TYPE C.
DATA : TIME_DIFF TYPE  INT4 .
DATA : W_DATA LIKE LINE OF T_DATA .
DATA : WA_VBPA TYPE VBPA.
DATA : LV_VBELN TYPE VBELN,
       LV_POSNR TYPE POSNR,
       LV_PARVW TYPE PARVW,
       LV_KUNNR TYPE KUNNR,
       LV_KUNNR_NAME TYPE NAME1_GP,
       LV_LIFNR TYPE LIFNR,
       LV_LIFNR_NAME TYPE NAME1_GP,
       LV_PERNR TYPE PERNR_D,
       LV_PERNR_NAME TYPE NAME1_GP,
       LV_NRART TYPE NRART.
DATA: LV_VBTYP TYPE VBTYP.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : FLD(60) TYPE C .
DATA : REF_DATE TYPE D.
FIELD-SYMBOLS:  TYPE ANY ,
               <FS_V> TYPE ANY .
DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
  INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
DATA : END OF SW_STRUCTURE .
DATA : LS_VBPA TYPE VBPA,
       LT_VBPA LIKE TABLE OF LS_VBPA.
DATA : LV_DATA_POSNR TYPE POSNR.
DATA: LS_STATUS TYPE JSTAT,
      LT_STATUS LIKE TABLE OF LS_STATUS,
      LV_STATUS TYPE J_STATUS,
      LV_STATUS_OK(1) TYPE C.
   IF R_DATUM[] IS INITIAL .
     RS_DATUM-SIGN = 'I' .
     RS_DATUM-OPTION = 'GE' .
     DATE_FROM = SY-DATUM - LV_BACKDAYS .
     RS_DATUM-LOW = DATE_FROM .
     APPEND RS_DATUM TO R_DATUM.
   ENDIF.
 "--- Set Reference Date Field
   CASE LV_DATE_REF_FLD.
     WHEN 'ERDAT'.
       R_ERDAT[] = R_DATUM[].
     WHEN 'AUDAT'.
       R_AUDAT[] = R_DATUM[].
     WHEN 'VDATU'.
       R_VDATU[] = R_DATUM[].
     WHEN 'AEDAT'.
       R_AEDAT[] = R_DATUM[].
     WHEN OTHERS.
       R_ERDAT[] = R_DATUM[].
   ENDCASE.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_ORD_VAL_NEW'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM VBAK
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE VBELN IN R_VBELN
      AND KUNNR IN R_KUNNR
      AND VKORG IN R_VKORG
      AND VTWEG IN R_VTWEG
      AND BSTNK IN R_BSTNK
      AND AUART IN R_AUART
      AND VBTYP IN R_VBTYP
      AND SPART IN R_SPART
      AND VKGRP IN R_VKGRP
      AND VKBUR IN R_VKBUR
      AND ERDAT IN R_ERDAT
      AND VDATU IN R_VDATU
      AND AEDAT IN R_AEDAT
      AND AUDAT IN R_AUDAT.
  DELETE T_DATA WHERE NETWR NOT IN R_NETWR.
 "--- Check STAT Parameter
 IF R_STAT[] IS NOT INITIAL.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CLEAR LV_STATUS_OK.
    IF T_DATA-OBJNR IS NOT INITIAL.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          OBJNR                  = T_DATA-OBJNR
          ONLY_ACTIVE            = 'X'
        TABLES
          STATUS                 = LT_STATUS
        EXCEPTIONS
          OBJECT_NOT_FOUND       = 1
          OTHERS                 = 2.
      IF SY-SUBRC = 0.
        LOOP AT LT_STATUS INTO LS_STATUS.
          IF LS_STATUS-STAT IN R_STAT.
            LV_STATUS_OK = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    IF LV_STATUS_OK IS INITIAL.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
 ENDIF.
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
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
          D_FROM            = REF_DATE
          T_FROM            = SY-UZEIT
          D_TO              = SY-DATUM
          T_TO              = SY-UZEIT
          TIME_UNIT         = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
      IF SY-SUBRC = 0.
        T_DATA-DURATION = TIME_DIFF .
      ELSE.
         T_DATA-DURATION = '999999'.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  "--- Get BPs
    REFRESH R_BP_FUNCT.
    SET_BP_RANGE 1.
    SET_BP_RANGE 2.
    SET_BP_RANGE 3.
  IF R_BP_FUNCT[] IS NOT INITIAL.
    SELECT * FROM VBPA
      INTO CORRESPONDING FIELDS OF TABLE LT_VBPA
      FOR ALL ENTRIES IN T_DATA
      WHERE VBELN = T_DATA-VBELN
        AND PARVW IN R_BP_FUNCT.
    SORT LT_VBPA BY VBELN POSNR PARVW.
    LOOP AT T_DATA.
      SY_TABIX = SY-TABIX .
      GET_BP_ATTR 1.
      GET_BP_ATTR 2.
      GET_BP_ATTR 3.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDLOOP.
    DELETE T_DATA WHERE BP1_CODE NOT IN R_BP1_CODE.
    DELETE T_DATA WHERE BP2_CODE NOT IN R_BP2_CODE.
    DELETE T_DATA WHERE BP3_CODE NOT IN R_BP3_CODE.
  ENDIF.
   LOOP AT T_DATA .
     SY_TABIX = SY-TABIX .
     T_DATA-WAERK_FR = LV_WAERK_FR.
     IF  T_DATA-WAERK = LV_WAERK_FR.
         T_DATA-NETWR_FR = T_DATA-NETWR.
     ELSE.
       IF LV_WAERK_FR IS NOT INITIAL.
         CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
           EXPORTING
             DATE                    = SY-DATUM
             FOREIGN_CURRENCY        = LV_WAERK_FR
             LOCAL_AMOUNT            = T_DATA-NETWR
             LOCAL_CURRENCY          = T_DATA-WAERK
          IMPORTING
            FOREIGN_AMOUNT          = T_DATA-NETWR_FR
          EXCEPTIONS
            NO_RATE_FOUND           = 1
            OVERFLOW                = 2
            NO_FACTORS_FOUND        = 3
            NO_SPREAD_FOUND         = 4
            DERIVED_2_TIMES         = 5
            OTHERS                  = 6.
         IF SY-SUBRC <> 0.
         ENDIF.
       ENDIF.
     ENDIF.
     MODIFY T_DATA INDEX SY_TABIX.
   ENDLOOP.
   DELETE T_DATA WHERE NETWR_FR NOT IN R_NETWR_FR.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR                = T_DATA-KUNNR
      IMPORTING
        CUST_DESC            = T_DATA-CUST_DESC
      EXCEPTIONS
        WRONG_CUSTOMER       = 1
        OTHERS               = 2              .
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
