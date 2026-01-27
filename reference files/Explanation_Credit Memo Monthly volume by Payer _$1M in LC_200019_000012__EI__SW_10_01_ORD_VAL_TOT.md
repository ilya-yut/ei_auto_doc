# Exception Indicator: Exceptional Sales Documents Values – Aggregated (SW_10_01_ORD_VAL_TOT)

## General Overview

This Exception Indicator (EI) monitors aggregated sales order values in Sales and Distribution (SD) to identify exceptional patterns in sales document values across configurable time periods and organizational dimensions. It provides consolidated visibility into sales volume trends, enabling detection of unusual value concentrations, high-value transactions, and sales pattern anomalies that require management attention.

This EI serves as an essential control for sales management and financial oversight by:
- Aggregating sales order net values by customizable time periods (Month/Week/Quarter/Year) and organizational dimensions
- Identifying exceptional sales volumes that exceed predefined thresholds for transaction counts or value totals
- Monitoring sales patterns across multiple currencies (document currency and foreign currency)
- Providing flexible filtering by sales organization, distribution channel, division, customer, and document characteristics
- Supporting multi-partner analysis through three configurable business partner fields for comprehensive sales relationship visibility

This aggregated monitoring enables organizations to detect high-value transaction clusters, unusual sales concentration patterns, potential revenue recognition issues, and sales trends requiring executive visibility. The EI is particularly valuable for month-end close processes, sales performance reviews, and financial exception management.

The EI retrieves detailed sales order data from SAP SD tables (VBAK - Sales Document Header, VBPA - Sales Document Partner, KNA1 - Customer Master), then aggregates the results by the selected time period and organizational dimensions. It calculates total counts (TOT_CNT), total document currency values (TOT_NETWR), and total foreign currency values (TOT_NETWR_FR) for each aggregation bucket, then filters based on user-specified threshold ranges.

---

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

---

## Suggested Resolution

**Immediate Response**
- Review the aggregated sales values flagged by the EI to understand the nature and scope of the exceptional pattern (threshold violations, value concentration, period-specific spikes)
- Verify the authenticity of high-value orders using transaction VA03 (Display Sales Order) to confirm legitimacy and proper authorization
- Check sales document status and processing progress to ensure no manual intervention or corrections are pending
- Identify the business context for exceptional volumes: promotional campaigns, large customer orders, seasonal patterns, or data quality issues

**System Assessment**
- Analyze the aggregation dimensions (time period, organizational fields) to understand which factors drive the exceptional pattern
- Review historical trends by comparing current aggregated values to prior periods using the same aggregation criteria
- Examine currency-specific patterns by comparing TOT_NETWR (document currency) and TOT_NETWR_FR (foreign currency) totals
- Assess partner function distribution (BP1, BP2, BP3) to identify customer relationship patterns or master data inconsistencies
- Investigate sales document characteristics (AUART - document type, VBTYP - document category) to determine if exceptions are type-specific
- Validate date reference field usage to ensure appropriate timing basis for aggregation (VDATU - requested delivery date, AUDAT - document date, ERDAT - creation date)

**Corrective Actions**
- If unauthorized or erroneous orders are identified, initiate sales document correction procedures using VA02 (Change Sales Order)
- For legitimate high-value orders requiring special approval, escalate to sales management and finance for validation
- Update customer master data (VD02 - Change Customer Sales Data) if partner function issues or credit limit violations are detected
- Adjust pricing or discounting arrangements using VK11 (Create Condition Record) if pricing violations are confirmed
- Implement additional monitoring controls by adjusting EI parameters to tighten threshold criteria for future executions
- Document exceptional patterns and business justifications for audit trail and management reporting purposes
- Establish recurring EI execution schedules to provide continuous visibility into sales value trends and concentration risks
- Configure Dynamic Recipient List (USER_FLD parameter) to automatically route alerts to appropriate sales managers and finance stakeholders based on organizational responsibility

---

## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|-----------|-------------|------|--------|---------|--------------|--------|
| AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| AGGR_FIELDS | Aggregation Fields |  | 0 | 0 |  |  |
| AGGR_PERIOD | Aggregation Period(Y,Q,M,W) |  | 0 | 0 |  |  |
| AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| BP1_CODE | Customer Code 1 | CHAR | 10 | 0 | KUNNR | KUNNR |
| BP1_FUNCT | Partner Function 1 | CHAR | 2 | 0 | PARVW | PARVW |
| BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| BP2_CODE | Customer code 2 | CHAR | 10 | 0 | KUNNR | KUNNR |
| BP2_FUNCT | Partner Function 2 | CHAR | 2 | 0 | PARVW | PARVW |
| BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| BP3_CODE | Customer code 3 | CHAR | 10 | 0 | KUNNR | KUNNR |
| BP3_FUNCT | Partner Function 3 | CHAR | 2 | 0 | PARVW | PARVW |
| BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| NETWR_FR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| STAT | Status | CHAR | 5 | 0 | J_STATUS | J_STATUS |
| TOT_CNT | Natural number | INT4 | 10 | 0 | INT4 | INT4 |
| TOT_NETWR | Total Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| TOT_NETWR_FR | Tot. Foreign Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| WAERK_FR | Foreign Currency | CUKY | 5 | 0 | WAERK | WAERS |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 41 parameters listed in the Parameters Reference Table above.

**AEDAT** (Changed On):

Date filter for sales document last change date. Filters sales orders based on when they were last modified in the system (field VBAK-AEDAT). Use to identify recently updated orders or track document change patterns over time. Works together with DATE_REF_FLD when AEDAT is selected as the date reference for BACKDAYS calculations.

---

**AGGR_FIELDS** (Aggregation Fields):

Specifies which output fields to use as aggregation keys for grouping sales documents. Accepts a range of field names from the output structure (e.g., VKORG, SPART, VTWEG, KUNNR, AUART). The EI queries the data dictionary table DD03L to validate specified field names against the output structure definition. All sales documents sharing the same values for the specified AGGR_FIELDS and the same aggregation period are grouped together, with their values summed into TOT_CNT, TOT_NETWR, and TOT_NETWR_FR. Leave empty to aggregate only by time period without additional dimensional grouping.

---

**AGGR_PERIOD** (Aggregation Period(Y,Q,M,W)):

Defines the time period granularity for sales order value aggregation. Controls how the selected date reference field (DATE_REF_FLD) is divided into reporting buckets.

**AGGR_PERIOD Options:**
- **M**: Month (default) - Groups sales orders by calendar month for monthly trend analysis and financial reporting
- **W**: Week - Groups by calendar week for short-term operational monitoring and weekly sales reviews
- **Q**: Quarter - Groups by fiscal quarter for quarterly business reviews and strategic planning
- **Y**: Year - Groups by fiscal year for annual reporting and year-over-year comparisons

**Configuration Impact:**
- AGGR_PERIOD = M with DATE_REF_FLD = VDATU: Monthly aggregation by requested delivery date (forward-looking demand analysis)
- AGGR_PERIOD = W with DATE_REF_FLD = AUDAT: Weekly aggregation by document creation date (order entry velocity tracking)
- AGGR_PERIOD = Q with DATE_REF_FLD = ERDAT: Quarterly aggregation by creation date (long-term trend analysis)

**Important:** The aggregation period determines the starting date for BACKDAYS calculations. For example, if AGGR_PERIOD = M and the selected date falls on January 15, the EI retrieves data from January 1 (month start) to ensure complete monthly buckets, even if BACKDAYS = 30.

---

**AUART** (Sales Document Type):

Sales document type filter from VBAK-AUART (e.g., TA = Standard Order, KB = Cash Sales, KR = Credit Memo Request). Use to focus monitoring on specific order transaction types or exclude certain document categories from aggregated analysis. Multiple values supported via range selection.

---

**AUDAT** (Document Date):

Document date filter from VBAK-AUDAT. Represents the date the sales document was received or sent. Use to filter sales orders by business date rather than system date, enabling retrospective analysis and historical pattern detection.

---

**BACKDAYS** (Days Backward from today):

Defines the data retrieval window by specifying how many days backward from today to filter records by the selected date reference field (DATE_REF_FLD). Controls which records are included in the aggregation analysis based on their reference date.

**BACKDAYS and DATE_REF_FLD Connection:**
When BACKDAYS is specified, the EI sets the selected date field (determined by DATE_REF_FLD) >= (SY-DATUM - BACKDAYS), retrieving only records within the lookback window. If BACKDAYS is not specified, the date field uses user-provided range or retrieves all records regardless of date.

**How They Work Together:**
- BACKDAYS specifies how many days back to retrieve records (based on DATE_REF_FLD date)
- DATE_REF_FLD determines which date field the range applies to (VDATU, AUDAT, ERDAT, or AEDAT)
- AGGR_PERIOD determines the time bucket granularity for grouping records
- Together they define: retrieve records where [DATE_REF_FLD] >= (Current Date - BACKDAYS), then aggregate by [AGGR_PERIOD]

**BACKDAYS Best Practices for Sales Aggregation:**
- BACKDAYS = 0 or 1: Real-time monitoring (today or today + yesterday) - default for operational monitoring
- BACKDAYS = 7: Weekly analysis - identify current week sales patterns and exceptions
- BACKDAYS = 30: Monthly analysis - month-to-date tracking and month-end closing support
- BACKDAYS = 90: Quarterly review - trend analysis and quarterly business review preparation
- BACKDAYS = 365: Annual analysis - year-over-year comparisons and long-term pattern detection
- BACKDAYS = (not set): Historical analysis - comprehensive review across all periods

**Important:** BACKDAYS defines the data retrieval scope (which records to analyze based on DATE_REF_FLD), while AGGR_PERIOD controls how those records are grouped into time buckets. For example, BACKDAYS=90 with AGGR_PERIOD=M retrieves 90 days of data and presents it in monthly aggregation buckets. The EI automatically adjusts the start date to align with period boundaries (e.g., if AGGR_PERIOD=M and BACKDAYS=30, it retrieves from the start of the month containing the calculated start date).

---

**BP1_CODE** (Customer Code 1):

Customer number filter for the first configurable business partner field. Filters by customer number from the partner function specified in BP1_FUNCT (e.g., sold-to party, ship-to party, bill-to party). Use to analyze sales patterns for specific customers or customer groups in the first partner dimension. Supports range selection for multi-customer analysis.

---

**BP1_FUNCT** (Partner Function 1):

Partner function code for the first business partner selection (e.g., AG = Sold-To Party, WE = Ship-To Party, RE = Bill-To Party, RG = Payer). Determines which partner function from VBPA (Sales Document Partner) is used to populate BP1_CODE and BP1_NAME. Enables flexible partner-based filtering and aggregation. If specified, BP1 fields are populated in the output structure.

---

**BP1_NAME** (Name):

Display field showing the customer name (from KNA1-NAME1) for the partner identified by BP1_FUNCT and BP1_CODE. Populated automatically by the EI for reporting and analysis. Not a filter parameter - used for output display only.

---

**BP2_CODE** (Customer code 2):

Customer number filter for the second configurable business partner field. Similar to BP1_CODE but allows simultaneous filtering or aggregation by a different partner function. For example, filter by sold-to party (BP1) and ship-to party (BP2) to analyze shipping relationships and delivery patterns.

---

**BP2_FUNCT** (Partner Function 2):

Partner function code for the second business partner selection. Similar to BP1_FUNCT but enables multi-dimensional partner analysis. Use to include a second partner perspective in the aggregation and filtering logic.

---

**BP2_NAME** (Name):

Display field showing the customer name for the partner identified by BP2_FUNCT and BP2_CODE. Populated automatically for reporting purposes.

---

**BP3_CODE** (Customer code 3):

Customer number filter for the third configurable business partner field. Enables three-way partner analysis (e.g., sold-to, ship-to, and payer relationships) in a single EI execution. Useful for complex sales scenarios with multiple involved parties.

---

**BP3_FUNCT** (Partner Function 3):

Partner function code for the third business partner selection. Provides maximum flexibility for partner-based filtering and aggregation in complex sales organizational structures.

---

**BP3_NAME** (Name):

Display field showing the customer name for the partner identified by BP3_FUNCT and BP3_CODE. Populated automatically for reporting purposes.

---

**BSTNK** (Purchase order no.):

Customer purchase order number filter from VBAK-BSTNK. Filters sales orders by the customer's external PO number. Use for targeted investigation of specific customer purchase orders or to identify duplicate PO number issues across multiple sales documents.

---

**CUST_DESC** (Name):

Display field showing the sold-to party customer name (KUNNR) from KNA1-NAME1. Populated automatically by the EI for reporting and analysis. Not a filter parameter - used for output display only.

---

**DATE_REF_FLD** (Date reference field):

Specifies which date field to use as the reference point for BACKDAYS data retrieval and AGGR_PERIOD time bucket calculations. Critical parameter that determines the time dimension basis for aggregation analysis.

**DATE_REF_FLD and BACKDAYS Connection:**
BACKDAYS determines how many days backward from today to retrieve data, while DATE_REF_FLD specifies which date field this range is applied to. Together: retrieve sales orders where [DATE_REF_FLD] is between (Today - BACKDAYS) and Today, then aggregate by AGGR_PERIOD.

**DATE_REF_FLD Options for Sales Documents:**
- **VDATU**: Requested delivery date (default value) - uses customer-requested delivery date for demand-based aggregation
- **AUDAT**: Document date - uses the business date when the order was received/sent
- **ERDAT**: Creation date - uses system creation date for order entry velocity analysis
- **AEDAT**: Change date - uses last modification date for tracking document update patterns

**Configuration Impact:**
- DATE_REF_FLD = VDATU (default): Aggregation based on when customer REQUESTED delivery (forward-looking demand analysis)
- DATE_REF_FLD = AUDAT: Aggregation based on document business date (aligns with fiscal period reporting)
- DATE_REF_FLD = ERDAT: Aggregation based on order entry timing (order processing velocity and workload analysis)
- DATE_REF_FLD = AEDAT: Aggregation based on document changes (change management and correction pattern tracking)

**Combined Configuration Examples:**
- BACKDAYS=30, DATE_REF_FLD=VDATU, AGGR_PERIOD=M: Retrieve orders with requested delivery in last 30 days, aggregate by month
- BACKDAYS=90, DATE_REF_FLD=AUDAT, AGGR_PERIOD=W: Retrieve orders by document date from last 90 days, aggregate by week
- BACKDAYS=7, DATE_REF_FLD=ERDAT, AGGR_PERIOD=M: Retrieve orders created in last 7 days, aggregate by creation month

**Important:** DATE_REF_FLD should be selected based on analysis objective. Use VDATU for demand planning and delivery scheduling analysis, AUDAT for financial period alignment, ERDAT for order entry workload analysis, or AEDAT for document change pattern monitoring. The selected field determines both the BACKDAYS filter range and the AGGR_PERIOD time bucket assignment.

---

**DURATION** (Duration In Time Units):

Defines the aging threshold by calculating time elapsed since the selected date reference field (DATE_REF_FLD). Filters records based on how long they have been in their current state. Calculated using SAP standard date difference logic. Must be used together with DURATION_UNIT.

**DURATION and DURATION_UNIT Connection:**
DURATION and DURATION_UNIT work together to define the aging filter. The EI calculates time elapsed from the reference date (determined by DATE_REF_FLD) to current date, then filters by DURATION range using the specified DURATION_UNIT.

**How They Work Together:**
- DURATION provides the numeric threshold value (e.g., 2, 7, 30)
- DURATION_UNIT defines the time unit (D=days, H=hours, M=minutes)
- DATE_REF_FLD determines which date field is the starting point for age calculation
- Together they filter: include records where (Current Date - [DATE_REF_FLD]) falls within DURATION range

**Combined Configuration Examples for Sales Orders:**
- BACKDAYS=30, DATE_REF_FLD=VDATU, DURATION=7-999, DURATION_UNIT=D: Orders with requested delivery in last 30 days, aged more than 7 days (overdue deliveries)
- BACKDAYS=90, DATE_REF_FLD=ERDAT, DURATION=14-999, DURATION_UNIT=D: Orders created in last 90 days, still open after 14 days (stale orders)
- BACKDAYS=7, DATE_REF_FLD=AUDAT, DURATION=0-2, DURATION_UNIT=D: Recent orders (last 7 days) less than 2 days old (new order monitoring)

**DURATION Best Practices for Sales Orders:**
- Use DURATION = 1-7 days to identify short-term aged orders requiring immediate action
- Use DURATION = 7-30 days to flag medium-term aging issues requiring escalation
- Use DURATION = 30+ days to detect long-term stale orders requiring review or cancellation

**Important:** DURATION filters based on record age (time since DATE_REF_FLD reference date), while BACKDAYS controls which records are retrieved for analysis. For comprehensive monitoring: set BACKDAYS to retrieve a time window, then use DURATION to identify those aging beyond acceptable thresholds. In aggregated analysis, DURATION filtering happens before aggregation, so only aged records contribute to totals.

---

**DURATION_UNIT** (Duration Unit(D/H/M)):

Specifies the time unit for DURATION aging threshold calculation. Must always be used together with DURATION to define how record age is measured. Default value: 'D' (days).

**DURATION_UNIT and DURATION Connection:**
DURATION and DURATION_UNIT always work together to define the time window. DURATION provides the numeric value, while DURATION_UNIT specifies whether that value represents days, hours, or minutes.

**DURATION_UNIT Options:**
- **D**: Days - for day-based aging analysis (default, recommended for sales order monitoring)
- **H**: Hours - for hour-based filtering (intraday operations, urgent order processing)
- **M**: Minutes - for minute-based filtering (real-time monitoring, not typically used for sales aggregation)

**Combined Configuration Examples:**
- DURATION = 7, DURATION_UNIT = D: Orders aged 7 days since reference date (weekly threshold)
- DURATION = 30, DURATION_UNIT = D: Orders aged 30 days (monthly aging threshold)
- DURATION = 24, DURATION_UNIT = H: Orders aged 24 hours (same-day processing SLA)
- DURATION = 48, DURATION_UNIT = H: Orders aged 48 hours (2-day processing window)

**Important:** For sales order aggregation analysis, always use DURATION_UNIT = D (days). The aging analysis is based on the selected date reference field (DATE_REF_FLD), making days the appropriate time unit for business process monitoring. Hours and minutes are more suitable for intraday operational monitoring scenarios.

---

**ERDAT** (Created On):

Date filter for sales document creation date from VBAK-ERDAT. Filters orders based on when they were originally created in the system. Use to analyze order entry patterns, identify creation date clusters, or track order volume trends by entry date. Works together with DATE_REF_FLD when ERDAT is selected as the date reference for BACKDAYS and AGGR_PERIOD calculations.

---

**ERNAM** (Created By):

User ID filter for the person who created the sales document (VBAK-ERNAM). Filters orders by creator to analyze order entry patterns by user, identify user-specific issues, or track individual performance metrics. Use for user activity analysis and training needs assessment.

---

**ERZET** (Time):

Creation time filter from VBAK-ERZET. Filters sales orders by system time when they were created. Use for intraday analysis to identify order entry timing patterns, peak workload periods, or time-specific processing issues. Typically used in combination with ERDAT for precise timestamp filtering.

---

**KUNNR** (Sold-to party):

Sold-to party customer number filter from VBAK-KUNNR. Filters sales orders by the primary customer (sold-to party). Use to analyze sales patterns for specific customers, identify high-value customer orders, or monitor customer-specific exceptions. Supports range selection for multi-customer analysis.

---

**NETWR** (Net value):

Net value filter for individual sales order line items in document currency (VBAK-NETWR). Filters orders based on the net order value before aggregation. Use to focus on high-value individual orders or exclude small-value transactions from aggregation analysis. Note: This filters individual orders, while TOT_NETWR filters aggregated totals.

---

**NETWR_FR** (Net value):

Net value filter for individual sales order line items in foreign currency. Similar to NETWR but for foreign currency transactions. Use when monitoring currency-specific order patterns or multi-currency sales analysis.

---

**SPART** (Division):

Division filter from VBAK-SPART. Filters sales orders by division (product line or business unit). Use to focus aggregation analysis on specific divisions or compare divisional sales patterns. Supports range selection for multi-division monitoring.

---

**STAT** (Status):

Status filter for sales document overall status. Filters orders by processing status indicators. Use to focus aggregation on specific status categories (e.g., open orders, blocked orders, completed orders). Status values are system-dependent and should be validated against your SAP configuration.

---

**TOT_CNT** (Natural number):

Aggregated count threshold filter. Filters aggregation results based on the number of sales orders grouped into each aggregation bucket. Use to identify exceptional transaction volumes - for example, TOT_CNT > 100 flags aggregation buckets with more than 100 orders.

**Configuration Examples:**
- TOT_CNT = 50-999999: Flag buckets with 50 or more orders (high-volume detection)
- TOT_CNT = 1-10: Flag buckets with very few orders (low-volume monitoring)
- TOT_CNT = 100-999999: Exception-only monitoring (extremely high volumes)

**Important:** TOT_CNT filters the aggregated results AFTER aggregation is complete. Individual order-level filtering happens first, then aggregation calculates TOT_CNT per bucket, then TOT_CNT filter is applied. This is a threshold parameter for identifying exceptional aggregation patterns, not an individual order filter.

---

**TOT_NETWR** (Total Net value):

Aggregated net value threshold filter in document currency. Filters aggregation results based on the sum of NETWR values grouped into each aggregation bucket. Use to identify exceptional total values - for example, TOT_NETWR > 1000000 flags aggregation buckets exceeding one million in total sales value.

**Configuration Examples:**
- TOT_NETWR = 500000-999999999: Flag buckets with total value exceeding 500,000
- TOT_NETWR = 1000000-999999999: Exception-only monitoring (very high total values)
- TOT_NETWR = 0-100000: Low-value bucket monitoring

**Important:** TOT_NETWR filters aggregated totals, not individual orders. It identifies aggregation buckets (by AGGR_PERIOD and AGGR_FIELDS) whose summed net values exceed threshold criteria. Use this to detect concentration risk, unusual sales patterns, or high-value periods requiring management attention.

---

**TOT_NETWR_FR** (Tot. Foreign Net value):

Aggregated net value threshold filter in foreign currency. Similar to TOT_NETWR but for foreign currency totals. Use to monitor currency-specific aggregated value patterns or identify exceptional foreign currency transaction volumes.

---

**USER_FLD** (Dynamic Recipient User Field):

Specifies which output field to use for Dynamic Recipient List (DRL) routing. The DRL mechanism enables flexible, role-based notification routing by automatically determining alert recipients based on the monitoring results rather than pre-assigned static user lists.

**Dynamic Recipient List (DRL) Mechanism:**
Instead of notifying a fixed set of users, the USER_FLD parameter allows the EI to extract user IDs or organizational values from the monitoring results and route alerts dynamically. For example, setting USER_FLD = ERNAM sends alerts to the user who created each flagged sales order, or setting USER_FLD = VKORG sends alerts to users responsible for each flagged sales organization.

**Common USER_FLD Values for Sales Orders:**
- ERNAM: Route alerts to the user who created the sales order
- VKORG: Route alerts based on sales organization responsibility
- VKBUR: Route alerts based on sales office assignment
- VKGRP: Route alerts based on sales group ownership

**Advanced Filtering Combinations:**
USER_FLD serves dual purposes: (1) filtering results to specific user-responsible data and (2) enabling dynamic notification routing. For example, ERNAM in USER_FLD with a specific user range filters to that user's orders AND routes alerts to those users.

---

**VBELN** (Sales Document):

Sales document number filter from VBAK-VBELN. Filters sales orders by specific document numbers. Use for targeted investigation of individual sales documents or verification of specific transaction processing. Supports range selection for multi-document analysis.

---

**VBTYP** (SD document categ.):

SD document category filter from VBAK-VBTYP (e.g., C = Order, G = Contract, I = Inquiry, L = Scheduling Agreement). Filters sales documents by category to focus analysis on specific document types. Use to exclude non-order documents or monitor specific transaction categories.

---

**VDATU** (Requested deliv.date):

Requested delivery date filter from VBAK-VDATU. Represents the customer-requested delivery date. Default date reference field (DATE_REF_FLD) for this EI. Use to filter sales orders by delivery commitment date or analyze delivery scheduling patterns. Critical for demand planning and logistics analysis.

---

**VKBUR** (Sales Office):

Sales office filter from VBAK-VKBUR. Filters sales orders by sales office organizational unit. Use to focus aggregation analysis on specific sales offices or compare office-level sales performance. Supports range selection for multi-office monitoring.

---

**VKGRP** (Sales Group):

Sales group filter from VBAK-VKGRP. Filters sales orders by sales group (team-level organizational unit). Use to analyze team-specific sales patterns or compare sales group performance. Supports range selection for multi-group analysis.

---

**VKORG** (Sales Organization):

Sales organization filter from VBAK-VKORG. Filters sales orders by sales organization (company code level sales unit). Use to focus aggregation analysis on specific sales organizations or enable multi-organization comparison. Supports range selection for multi-org monitoring.

---

**VTWEG** (Distribution Channel):

Distribution channel filter from VBAK-VTWEG. Filters sales orders by distribution channel (e.g., direct sales, wholesale, retail). Use to analyze channel-specific sales patterns or compare channel performance. Supports range selection for multi-channel monitoring.

---

**WAERK** (Document Currency):

Document currency filter from VBAK-WAERK. Filters sales orders by document currency code (e.g., USD, EUR, GBP). Use to focus aggregation on specific currencies or analyze currency-specific sales patterns. Supports multi-currency filtering via range selection.

---

**WAERK_FR** (Foreign Currency):

Foreign currency filter for multi-currency analysis. Filters orders with foreign currency values. Use when monitoring cross-border transactions or analyzing foreign exchange exposure in sales operations.

---


### Parameter Relationships

**Time-Based Aggregation Parameters:**

The following parameters work together to control the time dimension of aggregation analysis:

- **DATE_REF_FLD** determines which date field is used as the time reference (VDATU - requested delivery date, AUDAT - document date, ERDAT - creation date, AEDAT - change date)
- **BACKDAYS** specifies the lookback window from today (how many days of historical data to retrieve)
- **AGGR_PERIOD** defines the time bucket granularity (M=Month, W=Week, Q=Quarter, Y=Year)
- **DURATION and DURATION_UNIT** filter by aging within the retrieved dataset (how long records have existed since the reference date)

**Example Configuration:**
- DATE_REF_FLD = VDATU (requested delivery date)
- BACKDAYS = 90 (retrieve last 90 days)
- AGGR_PERIOD = M (group by month)
- DURATION = 7-999, DURATION_UNIT = D (include only records aged more than 7 days)

**Result:** Retrieves sales orders with requested delivery dates in the last 90 days, filters to those more than 7 days old, aggregates by month, calculates monthly totals.

**Multi-Dimensional Aggregation Parameters:**

These parameters control how records are grouped beyond time period:

- **AGGR_FIELDS** specifies which output fields to use as aggregation keys (e.g., VKORG, SPART, KUNNR)
- **AGGR_PERIOD** combined with AGGR_FIELDS creates multi-dimensional aggregation buckets

**Example Configuration:**
- AGGR_PERIOD = M (monthly buckets)
- AGGR_FIELDS = VKORG, SPART (aggregate by sales org and division)

**Result:** Creates separate aggregation buckets for each unique combination of (Month, Sales Org, Division), calculating totals for each bucket.

**Business Partner Analysis Parameters:**

These parameters enable multi-partner dimensional analysis:

- **BP1_FUNCT + BP1_CODE** for first partner dimension
- **BP2_FUNCT + BP2_CODE** for second partner dimension  
- **BP3_FUNCT + BP3_CODE** for third partner dimension

**Example Configuration:**
- BP1_FUNCT = AG (Sold-To Party)
- BP2_FUNCT = WE (Ship-To Party)
- BP3_FUNCT = RG (Payer)

**Result:** Each aggregation bucket includes sold-to, ship-to, and payer information, enabling multi-party relationship analysis.

**Threshold Filtering Parameters:**

These parameters filter aggregated results based on calculated totals:

- **TOT_CNT** filters by count of orders in each aggregation bucket
- **TOT_NETWR** filters by total document currency value in each bucket
- **TOT_NETWR_FR** filters by total foreign currency value in each bucket

**Example Configuration:**
- TOT_CNT = 50-999999 (include buckets with 50+ orders)
- TOT_NETWR = 1000000-999999999 (include buckets with total value > 1M)

**Result:** Only aggregation buckets meeting BOTH criteria are included in final results (high volume AND high value).

### Default Values and Parameter Options Explicitly Stated in EI Code

- **DATE_REF_FLD** - Default: 'VDATU' (Requested delivery date) - line 502 of ABAP code
- **AGGR_PERIOD** - Default: 'M' (Monthly aggregation) - line 503 of ABAP code
- **BACKDAYS** - Default: 0 (today only) - line 504 of ABAP code

**Note:** If BACKDAYS is not specified and the date range is not provided via selection, all records are retrieved regardless of date. For aggregation period alignment, the EI automatically adjusts the retrieval start date to the beginning of the period (e.g., for AGGR_PERIOD='M', retrieves from the first day of the calculated start month).

### Practical Configuration Examples

**Use Case 1: Monthly High-Value Sales Monitoring**
```
AGGR_PERIOD = M
DATE_REF_FLD = VDATU
BACKDAYS = 90
AGGR_FIELDS = VKORG, SPART
TOT_NETWR = 5000000-999999999
TOT_CNT = 100-999999
```
**Purpose:** Retrieve sales orders with requested delivery dates in the last 90 days, aggregate by month, sales organization, and division. Flag aggregation buckets with total value exceeding 5 million AND transaction count exceeding 100. Ideal for identifying high-volume, high-value sales concentrations requiring management review.

**Use Case 2: Weekly Customer Sales Pattern Analysis**
```
AGGR_PERIOD = W
DATE_REF_FLD = AUDAT
BACKDAYS = 30
AGGR_FIELDS = KUNNR, VKORG
TOT_CNT = 20-999999
KUNNR = (specific customer range)
```
**Purpose:** Retrieve sales orders by document date from the last 30 days, aggregate weekly by customer and sales organization. Flag weeks with 20+ orders per customer. Ideal for monitoring high-frequency customer ordering patterns and identifying unusual customer activity spikes.

**Use Case 3: Aged Order Value Concentration Monitoring**
```
AGGR_PERIOD = M
DATE_REF_FLD = ERDAT
BACKDAYS = 180
DURATION = 30-999
DURATION_UNIT = D
AGGR_FIELDS = VKBUR, AUART
TOT_NETWR = 1000000-999999999
```
**Purpose:** Retrieve sales orders created in the last 180 days, filter to those more than 30 days old (aged orders), aggregate monthly by sales office and document type. Flag aggregation buckets with total value exceeding 1 million. Ideal for identifying stale high-value orders requiring expedited processing or review.

**Use Case 4: Multi-Partner Sales Relationship Analysis**
```
AGGR_PERIOD = Q
DATE_REF_FLD = VDATU
BACKDAYS = 365
BP1_FUNCT = AG
BP2_FUNCT = WE
BP3_FUNCT = RG
AGGR_FIELDS = BP1_CODE, BP2_CODE, BP3_CODE
TOT_CNT = 50-999999
```
**Purpose:** Retrieve sales orders from the last year by requested delivery date, aggregate quarterly by sold-to, ship-to, and payer combinations. Flag relationship patterns with 50+ orders. Ideal for analyzing complex customer relationships, identifying multi-party transaction patterns, and detecting customer master data issues.

**Use Case 5: Distribution Channel Performance Comparison**
```
AGGR_PERIOD = M
DATE_REF_FLD = VDATU
BACKDAYS = 90
AGGR_FIELDS = VTWEG, VKORG
TOT_NETWR = 0-999999999
TOT_CNT = 1-999999
```
**Purpose:** Retrieve all sales orders with requested delivery dates in the last 90 days, aggregate monthly by distribution channel and sales organization. Include all aggregation buckets (no threshold filtering) for comprehensive comparison. Ideal for channel performance analysis, sales mix evaluation, and strategic planning support.

---

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

---

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
data: ls_DATA_DET type /SKN/S_SW_10_01_ORD_VAL_NEW,
      lt_DATA_DET like TABLE OF ls_DATA_DET.
*FIELD-SYMBOLS <fs_DATA_DET> type /SKN/S_SW_10_01_ORD_VAL_NEW.
data: ls_DATA like LINE OF T_DATA.
types: ty_aggr_period(8) type N,
       ty_aggr_arg type string.
data: lv_aggr_period_val type ty_aggr_period,
      lv_aggr_arg_val type ty_aggr_arg.
data: begin of ls_DET_AGGR.
  include structure /SKN/S_SW_10_01_ORD_VAL_NEW.
data: aggr_period type ty_aggr_period,
      aggr_arg type ty_aggr_arg.
data: end of ls_DET_AGGR.
data: lt_DET_AGGR like TABLE OF ls_DET_AGGR.
FIELD-SYMBOLS <fs_DET_AGGR> like ls_DET_AGGR.
data: begin of ls_AGGR,
        aggr_period  type ty_aggr_period,
        aggr_arg     type ty_aggr_arg,
        TOT_NETWR	   type NETWR_AK,
        TOT_NETWR_FR type NETWR_AK,
        TOT_CNT      type INT4,
      end of ls_AGGR.
*data: lt_AGGR like  TABLE OF ls_AGGR.
data: lt_AGGR like HASHED TABLE OF ls_AGGR
               WITH UNIQUE KEY aggr_arg aggr_period.
*FIELD-SYMBOLS <fs_AGGR> like ls_AGGR.
*data: begin of ls_AGGR_key,
*        aggr_period  type ty_aggr_period,
*        aggr_arg     type ty_aggr_arg,
*      end of ls_AGGR_key.
data : fld(60) type c .
data : ref_date type D.
FIELD-SYMBOLS: <fs> TYPE ANY ,
               <fs_v> TYPE ANY .
data : ref_string(60) type C.
data: begin of ls_AGGR_FIELDS,
       FIELDNAME type FIELDNAME,
      end of ls_AGGR_FIELDS.
data: lt_AGGR_FIELDS like TABLE OF ls_AGGR_FIELDS.
data:  date_from LIKE sy-datum,
       date_to LIKE sy-datum,
       date_from_start_period LIKE sy-datum.
data_multy: AGGR_FIELDS  NAME_KOMP,
            TOT_NETWR	   NETWR_AK,
            TOT_NETWR_FR NETWR_AK,
            TOT_CNT      INT4,
            DATUM        sy-datum,
            STAT         J_STATUS.
data_single: AGGR_PERIOD  CHAR1,  " M - Month/W - Week/Q - Qurter/Y - Year
             DATE_REF_FLD NAME_FELD,
             BACKDAYS     INT4,
             FORWDAYS     INT4.
select_multy: DATUM,
              AGGR_FIELDS,
              TOT_NETWR,
              TOT_NETWR_FR,
              TOT_CNT,
              STAT.
 lv_DATE_REF_FLD = 'VDATU'."Delivery Date)
 lv_AGGR_PERIOD = 'M'.
 lv_backdays = 0.
 select_single: AGGR_PERIOD,
                DATE_REF_FLD,
                BACKDAYS,
                FORWDAYS.
 IF NOT lv_forwdays  IS INITIAL.
   lv_backdays = lv_forwdays * ( -1 ).
 ENDIF.
 IF r_datum[] IS INITIAL .
   rs_datum-sign = 'I' .
   rs_datum-option = 'GE' .
   date_from = sy-datum - lv_backdays .
   rs_datum-low = date_from .
   APPEND rs_datum TO r_datum.
 ENDIF.
  "--- Set Reference Date Field
  date_from = sy-datum.
  READ TABLE r_datum INTO rs_datum INDEX 1.
  IF sy-subrc IS INITIAL.
    date_from = rs_datum-low.
    date_to = rs_datum-high.
    IF date_to < date_from.
      date_to = date_from.
    ENDIF.
  ENDIF.
 refresh lt_AGGR_FIELDS.
 if R_AGGR_FIELDS[] is not initial.
   select *
     into CORRESPONDING FIELDS OF TABLE lt_AGGR_FIELDS
     from DD03L
     where TABNAME = '/SKN/S_SW_10_01_ORD_VAL_TOT'
       and FIELDNAME in R_AGGR_FIELDS.
 endif.
 "--- Prepare Dates for Aggregation (Month/...)
  "-- Save Orogina Dates Selection
   data_multy: DATUM_SRC    sy-datum. " To save original Selection
   R_DATUM_SRC[] = R_DATUM[].
  "-- Set full Date Period Interval
    perform calc_aggr_period_start_date using date_from lv_AGGR_PERIOD
                                        changing date_from_start_period.
    READ TABLE r_datum INTO rs_datum INDEX 1.
    IF sy-subrc IS INITIAL.
      rs_datum-low = date_from_start_period.
      modify r_datum from rs_datum index sy-tabix.
    endif.
    "--- Substitute DATUM condition in T_SELECT
    delete T_SELECT where FIELDNM = 'DATUM'.
    loop at r_datum INTO rs_datum.
      MOVE-CORRESPONDING rs_datum to T_SELECT.
      T_SELECT-FIELDNM = 'DATUM'.
      append T_SELECT.
    endloop.
 CALL FUNCTION '/SKN/F_SW_10_01_ORD_VAL_NEW'
   IMPORTING
     IS_ALERT       = IS_ALERT
   TABLES
     T_SELECT       = T_SELECT
     T_DATA         = lt_DATA_DET.
  "--- Prepare Aggregation Arguments
  loop at lt_DATA_DET into ls_DATA_DET.
    MOVE-CORRESPONDING ls_DATA_DET to ls_DET_AGGR.
    "--- Fill Aggregation Arguments
     "-- Aggr Period
      concatenate 'ls_DATA_DET-' lv_DATE_REF_FLD into fld .
      ASSIGN (fld) TO <fs>.
      if <fs> is not ASSIGNED.
        continue.
      endif.
      ref_date = <fs> .
      perform calculate_aggr_period_val using ref_date lv_AGGR_PERIOD
                                        changing lv_aggr_period_val.
      ls_DET_AGGR-aggr_period = lv_aggr_period_val.
     "-- Aggr Key
      clear lv_aggr_arg_val.
      loop at lt_AGGR_FIELDS into ls_AGGR_FIELDS.
        concatenate 'ls_DATA_DET-' ls_AGGR_FIELDS-FIELDNAME into fld .
        ASSIGN (fld) TO <fs>.
        if <fs> is not ASSIGNED.
          continue.
        endif.
        write <fs> to ref_string.
        concatenate lv_aggr_arg_val ref_string into lv_aggr_arg_val.
      endloop.
      ls_DET_AGGR-aggr_arg = lv_aggr_arg_val.
    append ls_DET_AGGR to lt_DET_AGGR.
    MOVE-CORRESPONDING ls_DET_AGGR to ls_AGGR.
    ls_AGGR-TOT_CNT = 1.
    ls_AGGR-TOT_NETWR = ls_DET_AGGR-NETWR.
    ls_AGGR-TOT_NETWR_FR = ls_DET_AGGR-NETWR_FR.
    COLLECT ls_AGGR into lt_AGGR.
  endloop.
  "--- Total Filtering
  delete lt_AGGR where TOT_CNT not in R_TOT_CNT.
  delete lt_AGGR where TOT_NETWR not in R_TOT_NETWR.
  delete lt_AGGR where TOT_NETWR_FR not in R_TOT_NETWR_FR.
  "--- Fill result Table
  loop at lt_DET_AGGR into ls_DET_AGGR.
    "--- Filter for Original Date Selection
     "-- Get Date ref Field
      concatenate 'ls_DET_AGGR-' lv_DATE_REF_FLD into fld .
      ASSIGN (fld) TO <fs>.
      if <fs> is not ASSIGNED.
        continue.
      endif.
      ref_date = <fs> .
      "-- Filter
      if ref_date not in R_DATUM_SRC.
        continue.
      endif.
    READ TABLE lt_AGGR into ls_AGGR
                       WITH KEY aggr_period = ls_DET_AGGR-aggr_period
                                aggr_arg    = ls_DET_AGGR-aggr_arg.
    if sy-subrc is initial.
      MOVE-CORRESPONDING ls_DET_AGGR to ls_DATA.
      MOVE-CORRESPONDING ls_AGGR to ls_DATA.
      append ls_DATA to t_data.
    endif.
  endloop.
*--- Check Alert Information
 read table t_data index 1.
 check not sy-tfill  is initial .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
