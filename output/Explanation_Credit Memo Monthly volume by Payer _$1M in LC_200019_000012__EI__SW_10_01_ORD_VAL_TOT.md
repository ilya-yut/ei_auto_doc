# Exception Indicator: Exceptional Sales Documents Values – Aggregated (SW_10_01_ORD_VAL_TOT)

## General Overview

This Exception Indicator (EI) monitors aggregated credit memo and billing-correction values in Sales and Distribution (SD), with emphasis on monthly aggregation and the payer dimension. It highlights payers or periods where credit memo volumes exceed configurable value thresholds (e.g. high totals in local currency) so that refunds, disputes, and revenue adjustments can be reviewed in a timely way.

This EI serves as an essential control for credit memo and revenue oversight by:
- Aggregating credit memo net values by month (or other configurable period) and by payer/customer using configurable aggregation fields
- Identifying payers or periods whose credit memo totals exceed predefined thresholds for transaction count or value (including in local/document currency)
- Monitoring credit memo patterns in both document and foreign currency to support reconciliation and exposure analysis
- Enabling filtering by sales organization, distribution channel, division, document type (e.g. credit memo types), and partner roles (sold-to, ship-to, bill-to)
- Supporting payer-level analysis through configurable business partner fields (BP1, BP2, BP3) for concentration and dispute visibility

This monitoring helps detect high credit-memo concentration by payer, unusual refund or dispute patterns, and revenue-adjustment risks. The EI is particularly useful for month-end close, rebate and returns review, and management control of credit memo volumes by payer.

The EI reads sales document data from SAP SD tables (VBAK - Sales Document Header, VBPA - Sales Document Partner, KNA1 - Customer Master), aggregates by the chosen period and dimensions (e.g. month and payer), and computes total counts (TOT_CNT), total document-currency value (TOT_NETWR), and total foreign-currency value (TOT_NETWR_FR) per bucket. Results are filtered using user-defined thresholds (e.g. TOT_NETWR for local-currency limits).

---

## Problem Description

Failure to monitor credit memo volumes by payer and by period creates multiple risks across financial reporting, operations, and compliance:

**Financial and Reporting Issues**
- Undetected high credit-memo totals by payer can distort revenue and period-over-period analysis, especially when thresholds (e.g. in local currency) are exceeded without review
- Concentrated credit memo volumes in specific months may indicate revenue recognition issues, refund timing, or disputed billing requiring adjustment
- Large credit memo values in foreign currency can create reconciliation and exposure issues if not tracked by payer and period
- Late discovery of material credit-memo concentrations may delay month-end close and reserve or adjustment postings
- Credit memo concentration in certain sales organizations or channels can hide underlying dispute or process problems in other areas

**Sales Operations and Control Risks**
- High credit-memo volumes by payer without visibility may point to unresolved disputes, pricing errors, or unauthorized credits and refunds
- Payer-level concentration in credit memos can signal customer dispute patterns, master data issues, or need for contract or process review
- Unusual credit-memo patterns by partner roles (sold-to, ship-to, bill-to) may indicate incorrect payer assignment or duplicate processing
- Spikes in credit memo activity by division or channel may reflect returns, rebates, or operational issues that need immediate follow-up
- Atypical aggregated credit-memo values may indicate data or integration errors (e.g. duplicate postings, wrong document types) requiring correction

**Management Visibility and Decision-Making Risks**
- Lack of credit-memo-by-payer monitoring delays awareness of refund trends, dispute patterns, and concentration risk
- Unidentified payer-level credit-memo concentrations can lead to late escalation of disputes or missed process improvements
- Material credit-memo volumes that exceed thresholds may require audit or compliance review but remain invisible without targeted monitoring
- Absence of monthly, payer-based credit-memo analysis limits the ability to prioritize collections, dispute resolution, and revenue-assurance actions

---

## Suggested Resolution

**Immediate Response**
- Review the credit-memo aggregates flagged by the EI (by payer and period) to see whether thresholds in local currency or count were breached and over which payers or months
- Verify high-value credit memos using VA03 (Display Sales Document) or the appropriate credit-memo display to confirm legitimacy, correct payer, and proper approval
- Check document status and processing (e.g. billing status, reversals) to ensure no stuck or duplicate credit memos
- Clarify business context: planned rebates/returns, dispute settlements, pricing corrections, or possible data/process errors

**System Assessment**
- Analyze aggregation choices (AGGR_PERIOD, AGGR_FIELDS, DATE_REF_FLD) to see how monthly and payer dimensions drive the exceptions
- Compare current credit-memo totals by payer and month to prior periods using the same criteria to spot trends or one-off spikes
- Compare TOT_NETWR (document/local currency) and TOT_NETWR_FR (foreign currency) by payer to support reconciliation and exposure review
- Review partner roles (BP1, BP2, BP3) and payer assignment to find misallocated credits or master data issues
- Filter by AUART (document type) and VBTYP (document category) to confirm credit-memo and reversal types and avoid mixing with order types
- Confirm DATE_REF_FLD (VDATU, AUDAT, or ERDAT) matches the intended basis for “monthly” and for BACKDAYS

**Corrective Actions**
- Correct or reverse erroneous credit memos via the appropriate sales and billing transactions (e.g. VA02 for order-related documents, billing reversal where applicable)
- Escalate large but legitimate credit-memo volumes to sales and finance for approval and reserve/recognition treatment
- Update customer master (VD02) and partner/procedures if payer or partner assignment errors drive repeated exceptions
- Adjust pricing or conditions (VK11) only where credit memos are linked to incorrect pricing; document and approve refunds and rebates
- Tighten or broaden EI parameters (TOT_NETWR, TOT_CNT, BACKDAYS, AGGR_FIELDS) so thresholds and payer/month cuts align with policy (e.g. “$1M in LC by payer per month”)
- Document findings and approvals for exceptions to support audit and management reporting
- Run the EI on a recurring schedule (e.g. monthly) to monitor credit-memo volume by payer and period continuously
- Use USER_FLD to route alerts to the responsible sales, collections, or revenue-assurance teams by payer or organizational unit

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

**IMPORTANT:** This section provides configuration guidance for ALL parameters listed in the Parameters Reference Table above.

**AEDAT** (Changed On):

DATE_REF_FIELD determines which date field this range is applied to (e.g. AEDAT, BUDAT etc.)



**AGGR_FIELDS** (Aggregation Fields):

Configuration for AGGR_FIELDS. See parameter reference or code analysis.



**AGGR_PERIOD** (Aggregation Period(Y,Q,M,W)):

Configuration for AGGR_PERIOD. See parameter reference or code analysis.



**AUART** (Sales Document Type):

Configuration for AUART. See parameter reference or code analysis.



**AUDAT** (Document Date):

Configuration for AUDAT. See parameter reference or code analysis.



**BACKDAYS** (Days Backward from today):

BACKDAYS and DURATION Connection: BACKDAYS defines the overall time window by bringing data from a specific number of days in the past up until today, while DURATION filters within this time frame  BACKDAYS specifies how many days back to data BACKDAYS Best Practices: BACKDAYS = 0 - Real-time monitoring (today only) BACKDAYS = 1 - Recent trend analysis (yesterday + today) BACKDAYS = 7 - Weekly pattern analysis Important: DURATION and DURATION_UNIT parameters must always be used together. For most monitoring cases, it is recommended to use both DURATION + DURATION_UNIT together with BACKDAYS for comprehensive time-based filtering. BACKDAYS and DATE_REF_FIELD Connection: BACKDAYS and DATE_REF_FIELD work together to define the time window. BACKDAYS determines how many days backward from today to look, while DATE_REF_FIELD specifies which date field is used as the reference point for this calculation. BACKDAYS creates a date range: (Today - BACKDAYS) to Today Important: BACKDAYS defines the overall data retrieval window based on the DATE_REF_FIELD, while DURATION filters based on time since last event. Use both parameters together for comprehensive time-based filtering. BACKDAYS = 0, STATUS = ERROR, DURATION > 1, DURATION_UNIT = H - Failed events older than 1 hour from today BACKDAYS = 1, STAT_WAIT = X, DURATION > 4, DURATION_UNIT = H - Stuck requests waiting for processing longer than 4 hours within last 2 days BACKDAYS = 0, STATE_COLOR = Y, DURATION > 30, DURATION_UNIT = M - Warning status running longer than 30 minutes today BACKDAYS - Default: 1 (yesterday + today)



**BP1_CODE** (Customer Code 1):

Configuration for BP1_CODE. See parameter reference or code analysis.



**BP1_FUNCT** (Partner Function 1):

Configuration for BP1_FUNCT. See parameter reference or code analysis.



**BP1_NAME** (Name):

Configuration for BP1_NAME. See parameter reference or code analysis.



**BP2_CODE** (Customer code 2):

Configuration for BP2_CODE. See parameter reference or code analysis.



**BP2_FUNCT** (Partner Function 2):

Configuration for BP2_FUNCT. See parameter reference or code analysis.



**BP2_NAME** (Name):

Configuration for BP2_NAME. See parameter reference or code analysis.



**BP3_CODE** (Customer code 3):

Configuration for BP3_CODE. See parameter reference or code analysis.



**BP3_FUNCT** (Partner Function 3):

Configuration for BP3_FUNCT. See parameter reference or code analysis.



**BP3_NAME** (Name):

Configuration for BP3_NAME. See parameter reference or code analysis.



**BSTNK** (Purchase order no.):

Configuration for BSTNK. See parameter reference or code analysis.



**CUST_DESC** (Name):

Configuration for CUST_DESC. See parameter reference or code analysis.



**DATE_REF_FLD** (Date reference field):

Configuration for DATE_REF_FLD. See parameter reference or code analysis.



**DURATION** (Duration In Time Units):

BACKDAYS and DURATION Connection: BACKDAYS defines the overall time window by bringing data from a specific number of days in the past up until today, while DURATION filters within this time frame  DURATION parameters filters based on the age events DURATION Types: DURATION - Time from reference date/time point to current time (age-based filtering) DURATION_H – Duration time in hours (from start to end) DURATION_M – Duration time in minutes (from start to end) DURATION and DURATION_UNIT Connection: DURATION and DURATION_UNIT always work together to define the time window. DURATION = 1, DURATION_UNIT = H – Events  within the last 1 hour DURATION = 30, DURATION_UNIT = M – Events within the last 30 minutes DURATION = 1, DURATION_UNIT = D - Events within the last 1 day Important: DURATION and DURATION_UNIT parameters must always be used together. For most monitoring cases, it is recommended to use both DURATION + DURATION_UNIT together with BACKDAYS for comprehensive time-based filtering. Important: BACKDAYS defines the overall data retrieval window based on the DATE_REF_FIELD, while DURATION filters based on time since last event. Use both parameters together for comprehensive time-based filtering. Status and DURATION Analysis: Status parameters and DURATION work together to identify problematic entries based on both processing state and age, enabling precise monitoring of stuck or failed communication operations. BACKDAYS = 0, STATUS = ERROR, DURATION > 1, DURATION_UNIT = H - Failed events older than 1 hour from today BACKDAYS = 1, STAT_WAIT = X, DURATION > 4, DURATION_UNIT = H - Stuck requests waiting for processing longer than 4 hours within last 2 days BACKDAYS = 0, STATE_COLOR = Y, DURATION > 30, DURATION_UNIT = M - Warning status running longer than 30 minutes today Note: in most cases it is recommended to focus on STATUS = ERROR or STAT_ERROR = X to specifically target failures, and STATUS = WAIT or STAT_WAIT = X with high DURATION values to identify stuck processes or jobs.



**DURATION_UNIT** (Duration Unit(D/H/M)):

DURATION and DURATION_UNIT Connection: DURATION and DURATION_UNIT always work together to define the time window. DURATION = 1, DURATION_UNIT = H – Events  within the last 1 hour DURATION = 30, DURATION_UNIT = M – Events within the last 30 minutes DURATION = 1, DURATION_UNIT = D - Events within the last 1 day DURATION_UNIT Options: Important: DURATION and DURATION_UNIT parameters must always be used together. For most monitoring cases, it is recommended to use both DURATION + DURATION_UNIT together with BACKDAYS for comprehensive time-based filtering. BACKDAYS = 0, STATUS = ERROR, DURATION > 1, DURATION_UNIT = H - Failed events older than 1 hour from today BACKDAYS = 1, STAT_WAIT = X, DURATION > 4, DURATION_UNIT = H - Stuck requests waiting for processing longer than 4 hours within last 2 days BACKDAYS = 0, STATE_COLOR = Y, DURATION > 30, DURATION_UNIT = M - Warning status running longer than 30 minutes today DURATION_UNIT - Default: M (Minutes)



**ERDAT** (Created On):

Configuration for ERDAT. See parameter reference or code analysis.



**ERNAM** (Created By):

Configuration for ERNAM. See parameter reference or code analysis.



**ERZET** (Time):

Configuration for ERZET. See parameter reference or code analysis.



**KUNNR** (Sold-to party):

Configuration for KUNNR. See parameter reference or code analysis.



**NETWR** (Net value):

Configuration for NETWR. See parameter reference or code analysis.



**NETWR_FR** (Net value):

Configuration for NETWR_FR. See parameter reference or code analysis.



**SPART** (Division):

Configuration for SPART. See parameter reference or code analysis.



**STAT** (Status):

Configuration for STAT. See parameter reference or code analysis.



**TOT_CNT** (Natural number):

Configuration for TOT_CNT. See parameter reference or code analysis.



**TOT_NETWR** (Total Net value):

Configuration for TOT_NETWR. See parameter reference or code analysis.



**TOT_NETWR_FR** (Tot. Foreign Net value):

Configuration for TOT_NETWR_FR. See parameter reference or code analysis.



**USER_FLD** (Dynamic Recipient User Field):

User Field Filtering (USER_FLD): Dynamic Recipient List (DRL) Mechanism: The USER_FLD parameter serves a dual purpose: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.



**VBELN** (Sales Document):

Configuration for VBELN. See parameter reference or code analysis.



**VBTYP** (SD document categ.):

Configuration for VBTYP. See parameter reference or code analysis.



**VDATU** (Requested deliv.date):

Configuration for VDATU. See parameter reference or code analysis.



**VKBUR** (Sales Office):

Configuration for VKBUR. See parameter reference or code analysis.



**VKGRP** (Sales Group):

Configuration for VKGRP. See parameter reference or code analysis.



**VKORG** (Sales Organization):

Configuration for VKORG. See parameter reference or code analysis.



**VTWEG** (Distribution Channel):

Configuration for VTWEG. See parameter reference or code analysis.



**WAERK** (Document Currency):

Configuration for WAERK. See parameter reference or code analysis.



**WAERK_FR** (Foreign Currency):

Configuration for WAERK_FR. See parameter reference or code analysis.


### Parameter Relationships

Parameters work together to define filters and aggregation. BACKDAYS and date reference define the time window; aggregation and field parameters control grouping.

### Default Values and Parameter Options Explicitly Stated in EI Code

- **DATE_REF_FLD** — Default: `VDATU`
- **AGGR_PERIOD** — Default: `M`
- **BACKDAYS** — Default: `0`

### Practical Configuration Examples

```
BACKDAYS=7, DATE_REF_FLD=VDATU, AGGR_PERIOD=M — Last 7 days, monthly by requested delivery date
```

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