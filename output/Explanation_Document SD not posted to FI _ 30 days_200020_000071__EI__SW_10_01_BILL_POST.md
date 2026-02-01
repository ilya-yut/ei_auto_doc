# Exception Indicator: SD Billing Doc. -post. stat.(Excl. invoice list) - SW_10_01_BILL_POST

## General Overview

This Exception Indicator (EI) monitors billing documents in Sales and Distribution (SD) that have not been posted to Financial Accounting (FI) within a configurable period. It provides visibility into billing document posting status and age, enabling identification of documents that remain in a given status beyond expected timeframes and that may require release, correction, or escalation.

This EI serves as an essential control for revenue recognition and billing operations by:
- Enabling detection of billing documents that have not been posted to FI within expected timeframes and that may delay revenue recognition or period close
- Supporting identification of posting blocks and status anomalies that require release or correction
- Providing visibility into billing document age and status for prioritization and root-cause analysis
- Enabling analysis of billing concentration by sales organization, distribution channel, payer, and partner role for operational and financial oversight
- Supporting accountability by sold-to, payer, and other partner roles for dispute resolution and collection follow-up

This monitoring enables organizations to reduce revenue leakage, accelerate period close, and address posting blocks and status exceptions in a timely manner. The EI is particularly valuable for month-end close, revenue recognition reviews, and billing exception management.

The EI uses billing document data from SAP SD (VBRK, VBUK) and partner data (VBPA) with customer master (KNA1) for descriptions.


## Problem Description

Failure to monitor billing documents that have not been posted to FI within expected timeframes creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unposted billing documents can delay revenue recognition and distort period-over-period revenue and receivables reporting
- Billing documents that remain in a non-posted status beyond expected timeframes may indicate revenue recognition timing issues or blocked postings requiring correction
- Unidentified posting blocks can delay month-end close and lead to late adjustments during financial review
- Concentration of unposted documents in specific sales organizations or periods can mask underlying process or system issues

**Billing Operations and Control Risks**
- Posting blocks and status anomalies without visibility may indicate authorization, credit, or master data issues requiring management intervention
- Unusual patterns by payer or sold-to party could signal customer master or payment-term issues affecting cash collection
- Billing type and category concentrations may reflect process bottlenecks or misconfiguration
- Lack of monitoring of status duration can allow aged exceptions to persist without escalation

**Management Visibility and Decision-Making Risks**
- Lack of visibility into unposted billing documents delays executive awareness of revenue and collection risks
- Unidentified posting blocks and status exceptions limit ability to prioritize follow-up and resource allocation
- Absence of multi-dimensional analysis (organization, channel, payer, partner) limits ability to target process improvements and dispute resolution

## Suggested Resolution

**Immediate Response**
- Review the billing documents flagged by the EI to understand the nature and scope of the exception (posting status, age, organizational concentration)
- Verify high-value or critical billing documents using transaction VF03 (Display Billing Document) to confirm legitimacy and posting block reason
- Check billing document status and posting status to ensure no manual intervention or corrections are pending
- Identify business context: temporary blocks, credit holds, master data errors, or process delays

**System Assessment**
- Analyze time period and organizational scope to understand which factors drive the exception pattern
- Review historical trends by comparing current unposted volumes to prior periods using the same criteria
- Examine payer and sold-to distribution to identify relationship or master data patterns
- Assess billing type and category to determine if exceptions are type-specific
- Validate the reference date used for age calculation to ensure the monitoring window is appropriate

**Corrective Actions**
- If posting blocks are identified, resolve block reasons (e.g. credit, authorization) and re-run posting where appropriate (VF02 – Change Billing Document; FB01/FB08 – Post/Post with clearing)
- For legitimate blocks requiring approval, escalate to billing and finance for release
- Update customer master (VD02 – Change Customer) if payer or sold-to issues are detected
- Document exception patterns and business justifications for audit and management reporting
- Establish recurring EI execution to provide continuous visibility into unposted billing documents and posting status
- Configure alert routing to billing and finance stakeholders based on organizational responsibility


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | BLOCK | Indicator: Document preselected for archiving | CHAR | 1 | 0 | BLOCK_VB | BLOCK_VB |
| 4 | BP1_CODE | Partner1 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 5 | BP1_FUNCT | Partner1 -  Function | CHAR | 2 | 0 | PARVW | PARVW |
| 6 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 7 | BP2_CODE | Partner2 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 8 | BP2_FUNCT | Partner2 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 9 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 10 | BP3_CODE | Partner3 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 11 | BP3_FUNCT | Partner3 -  Function | CHAR | 2 | 0 | PARVW | PARVW |
| 12 | BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 13 | BUCHK | Posting Status | CHAR | 1 | 0 | BUCHK | STATV |
| 14 | BZIRK | Sales district | CHAR | 6 | 0 | BZIRK | BZIRK |
| 15 | COSTA | Confirmation status | CHAR | 1 | 0 | COSTA_D | COSTA |
| 16 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 17 | DUMMY | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 18 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 19 | DURATION_D | Duration In Days | NUMC | 6 | 0 | /SKN/E_SW_DURATION_D |  |
| 20 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 21 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 22 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 23 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 24 | FKART | Billing Type | CHAR | 4 | 0 | FKART | FKART |
| 25 | FKDAT | Billing Date | DATS | 8 | 0 | FKDAT | DATUM |
| 26 | FKSTO | Cancelled | CHAR | 1 | 0 | FKSTO | XFELD |
| 27 | FKTYP | Billing category | CHAR | 1 | 0 | FKTYP | FKTYP |
| 28 | FMSTK | Status Funds Management | CHAR | 1 | 0 | FMSTK | STATV |
| 29 | KDGRP | Customer group | CHAR | 2 | 0 | KDGRP | KDGRP |
| 30 | KUNAG | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 31 | KUNRG | Payer | CHAR | 10 | 0 | KUNRG | KUNNR |
| 32 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 33 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 34 | NETWR | Net Value | CURR | 15 | 2 | NETWR | WERTV8 |
| 35 | PAYER_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 36 | RELIK | Inv.list status | CHAR | 1 | 0 | RELIK | STATV |
| 37 | RFBSK | Posting Status | CHAR | 1 | 0 | RFBSK | RFBSK |
| 38 | RRSTA | Rev. determ. status | CHAR | 1 | 0 | RR_STATUS | STATV |
| 39 | SOLDTO_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 40 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 41 | UVK01 | Header reserves 1 | CHAR | 1 | 0 | UVK01 | STATV |
| 42 | UVK02 | Header reserves 2 | CHAR | 1 | 0 | UVK02 | STATV |
| 43 | UVK03 | Header reserves 3 | CHAR | 1 | 0 | UVK03 | STATV |
| 44 | UVK04 | Header reserves 4 | CHAR | 1 | 0 | UVK04 | STATV |
| 45 | UVK05 | Header reserves 5 | CHAR | 1 | 0 | UVK05 | STATV |
| 46 | UVS01 | Total reserves 1 | CHAR | 1 | 0 | UVS01 | STATV |
| 47 | UVS02 | Total reserves 2 | CHAR | 1 | 0 | UVS02 | STATV |
| 48 | UVS03 | Total reserves 3 | CHAR | 1 | 0 | UVS03 | STATV |
| 49 | UVS04 | Total reserves 4 | CHAR | 1 | 0 | UVS04 | STATV |
| 50 | UVS05 | Total reserves 5 | CHAR | 1 | 0 | UVS05 | STATV |
| 51 | VBELN | Billing Document | CHAR | 10 | 0 | VBELN_VF | VBELN |
| 52 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 53 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 54 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 55 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 55 parameters listed in the Parameters Reference Table above.


**AEDAT** (Changed On):

Filters billing document data by the "changed on" date (VBRK). The EI applies this to the date range derived from BACKDAYS and DATE_REF_FLD when DATE_REF_FLD is set to AEDAT. Use to focus on recently changed billing documents.

**BACKDAYS** (Backdays):

Number of days to look back from today. When no date range is supplied, the EI builds the selection using SY-DATUM minus BACKDAYS. Used to limit the billing document date window.

**BACKDAYS and DATE_REF_FLD Connection:**

BACKDAYS defines how many days back from today the window starts. DATE_REF_FLD selects which date field (ERDAT, AEDAT, or FKDAT) is used for that window. Together they determine which billing documents fall into the selection. Set BACKDAYS first, then choose DATE_REF_FLD to match the business date you care about (e.g. billing date vs creation date).

**BLOCK** (Indicator: Document preselected for archiving):

Indicator from VBUK-BLOCK. Filters billing documents by archiving preselection. Use to include or exclude documents marked for archiving.

**BP1_CODE** (Partner1 - Code):

Partner number for the first partner role. Filters billing documents by this partner (from VBPA). BP1_NAME is populated from customer master (KNA1). Use with BP1_FUNCT to define the role.

**BP1_FUNCT** (Partner1 -  Function):

Partner function for the first business partner (PARVW). Used with BP1_CODE to filter and enrich partner data from VBPA.

**BP1_FUNCT Options:**

- **AG**: Sold-to party  
- **WE**: Ship-to party  
- **RE**: Bill-to party  
(Other PARVW values apply; use the value that matches the role you want to filter by.)

**BP1_NAME** (Name):

Name of the first partner (from KNA1-NAME1). Populated by the EI from customer master based on BP1_CODE. Use for display; filter by BP1_CODE.

**BP2_CODE** (Partner2 - Code):

Partner code for the second partner role. Filters and enriches output; works with BP2_FUNCT and BP2_NAME.

**BP2_FUNCT** (Partner2 - Function):

Partner function for the second business partner. Works with BP2_CODE and BP2_NAME.

**BP2_NAME** (Name):

Name of the second partner. Populated from customer master. Filter by BP2_CODE.

**BP3_CODE** (Partner3 - Code):

Partner code for the third partner role. Filters and enriches output.

**BP3_FUNCT** (Partner3 -  Function):

Partner function for the third business partner. Works with BP3_CODE and BP3_NAME.

**BP3_NAME** (Name):

Name of the third partner. Populated from customer master. Filter by BP3_CODE.

**BUCHK** (Posting Status):

Posting status (VBUK-BUCHK). Filters billing documents by posting status. Use to focus on posted vs unposted documents.

**BZIRK** (Sales district):

Sales district (VBRK-BZIRK). Filters by sales district. Supports range selection.

**COSTA** (Confirmation status):

Confirmation status. Filters by confirmation status. Supports range or single value.

**DATE_REF_FLD** (Date Ref Field):

Field name used as the reference date for the selection window and for duration calculation. The EI maps this to the corresponding billing document date and uses it with BACKDAYS to build the date range and with DURATION/DURATION_UNIT to compute how long the document has been in the current status.

**DATE_REF_FLD Options:**

- **ERDAT**: Created on  
- **AEDAT**: Changed on  
- **FKDAT**: Billing date (default in code)  

**DUMMY** (Single-Character Flag):

Single-character flag; technical or placeholder. Use as required by the front end or selection interface.

**DURATION** (Duration In Time Units):

Duration in the selected time unit (see DURATION_UNIT) between the reference date (DATE_REF_FLD) and current date. The EI calculates this per billing document and filters by the supplied range to focus on documents that have been in the current status for a given length of time.

**DURATION and DURATION_UNIT Connection:**

DURATION gives the numeric value (e.g. 7); DURATION_UNIT gives the unit (e.g. D = days). Together they define the "age in status" filter. DATE_REF_FLD determines which date is used as the start for the calculation. Set all three when filtering by how long billing documents have been in status.

**DURATION_D** (Duration In Days):

Duration in days (calculated/output). Use to filter or display age in days when the unit is days.

**DURATION_UNIT** (Duration Unit):

Unit for duration calculation. Used with DATE_REF_FLD and DURATION to filter billing documents by how long they have been in the current status.

**DURATION_UNIT Options:**

- **D**: Days (default in code)

**ERDAT** (Created On):

Creation date of the billing document (VBRK-ERDAT). Filters by creation date. Used in the date range when DATE_REF_FLD = ERDAT.

**ERNAM** (Created by):

User who created the billing document (VBRK-ERNAM). Filters by creator. Supports range or single value.

**ERZET** (Time):

Creation time. Filters by time when time-based selection is used.

**FKART** (Billing Type):

Billing type (VBRK-FKART). Filters by billing type (e.g. invoice, credit memo). Supports range selection.

**FKDAT** (Billing Date):

Billing date (VBRK-FKDAT). Filters by billing date. Default reference date in code when DATE_REF_FLD = FKDAT.

**FKSTO** (Cancelled):

Cancellation indicator (VBRK-FKSTO). Filters by cancelled vs active billing documents.

**FKTYP** (Billing category):

Billing category (VBRK-FKTYP). Filters by billing category. Supports range or single value.

**FMSTK** (Status Funds Management):

Funds management status. Filters by funds management status.

**KDGRP** (Customer group):

Customer group (VBRK-KDGRP). Filters by customer group. Supports range selection.

**KUNAG** (Sold-to party):

Sold-to party (VBRK-KUNAG). Filters billing documents by sold-to customer. SOLDTO_DESC is populated from KNA1.

**KUNRG** (Payer):

Payer (VBRK-KUNRG). Filters by payer. PAYER_DESC is populated from KNA1. Use for collection and dispute analysis.

**LANGU** (Language for texts):

Language for descriptions. Drives text resolution for partner and payer names.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set (e.g. 'X'), date/time handling uses UTC. Use when working across time zones.

**NETWR** (Net Value):

Net value of the billing document. Output for value-based analysis. Use to filter or display by value.

**PAYER_DESC** (Name):

Payer name (KNA1-NAME1 for KUNRG). Populated by the EI from customer master. Use for display; filter by KUNRG.

**RELIK** (Inv.list status):

Invoice list status (VBUK-RELIK). Filters by invoice list status. Use to focus on posting or list status.

**RFBSK** (Posting Status):

Posting block (VBRK-RFBSK). Filters by posting block. Use to identify blocked postings.

**RRSTA** (Rev. determ. status):

Revenue determination status (VBUK-RRSTA). Filters by revenue determination status.

**SOLDTO_DESC** (Name):

Sold-to party name (KNA1-NAME1 for KUNAG). Populated by the EI from customer master. Use for display; filter by KUNAG.

**SPART** (Division):

Division (VBRK-SPART). Filters by division. Supports range selection.

**UVK01 - UVK05** (Header reserves 1 - Header reserves 5):

Reserve/status indicators at header level (VBUK). Used to filter billing documents by the corresponding status dimension. All five share the same type and domain; use the index that matches the dimension you need.

**UVS01 - UVS05** (Total reserves 1 - Total reserves 5):

Reserve/status indicators for totals. Used to filter billing documents by the corresponding status dimension. All five share the same type and domain.

**VBELN** (Billing Document):

Billing document number (VBRK-VBELN). Filters by billing document. Supports range selection.

**VBTYP** (SD document categ.):

SD document category (VBRK-VBTYP). Filters by document category.

**VKORG** (Sales Organization):

Sales organization (VBRK-VKORG). Filters by sales organization. Supports range selection.

**VTWEG** (Distribution Channel):

Distribution channel (VBRK-VTWEG). Filters by distribution channel. Supports range selection.

**WAERK** (Document Currency):

Document currency of the billing document (VBRK-WAERK). Represents the document/transaction currency. Use for display or currency-based filtering; describe only business meaning (document/transaction currency).


### Parameter Relationships

**Time and Reference Date Parameters:**
- BACKDAYS defines how many days back from today the selection window starts. DATE_REF_FLD selects which date field (ERDAT, AEDAT, or FKDAT) is used for that window. Together they determine which billing documents fall into the selection.
- DURATION gives the numeric value (e.g. 7); DURATION_UNIT gives the unit (e.g. D = days). DATE_REF_FLD determines which date is used as the start for the "age in status" calculation. Set all three when filtering by how long documents have been in the current status.

**Business Partner Parameters:**
- BP1_FUNCT and BP1_CODE work together: BP1_FUNCT defines the partner role (e.g. sold-to, bill-to); BP1_CODE filters by that partner number. BP1_NAME is populated from customer master. The same applies to BP2_FUNCT/BP2_CODE/BP2_NAME and BP3_FUNCT/BP3_CODE/BP3_NAME.


### Default Values

- **BACKDAYS** — Default: `1` (when not supplied, the EI uses one day back from today for the date window).
- **DATE_REF_FLD** — Default: `FKDAT` (billing date); when not supplied, the EI uses billing date as the reference for the selection window and duration calculation.
- **DURATION_UNIT** — Default: `D` (days); when not supplied, duration is calculated in days.
- **LANGU** — Default: initial (system language when not supplied).
- **MANAGE_IN_UTC** — Default: initial (empty); standard time handling when not supplied.

**Note:** Parameters that are not supplied and are used when initial (e.g. empty or 0) effectively default to initial; the EI applies the date window and filters accordingly.

### Practical Configuration Examples

**Use Case 1: Billing documents not posted within 30 days (billing date reference)**
```
BACKDAYS = 30
DATE_REF_FLD = FKDAT
DURATION_UNIT = D
VKORG = 1000
```
**Purpose:** Focus on billing documents whose billing date falls within the last 30 days and that have not been posted, by sales organization 1000. Supports revenue recognition and period-close monitoring.

**Use Case 2: Aged unposted documents by creation date**
```
BACKDAYS = 14
DATE_REF_FLD = ERDAT
DURATION = 7
DURATION_UNIT = D
BUCHK = 
```
**Purpose:** Identify billing documents created in the last 14 days that have been in the current posting status for more than 7 days. Use to prioritize follow-up on aged unposted documents.

**Use Case 3: Payer and partner analysis**
```
BACKDAYS = 60
DATE_REF_FLD = FKDAT
BP1_FUNCT = AG
BP1_CODE = 0000100001
KUNAG = 0000100002
```
**Purpose:** Monitor unposted billing documents for a specific sold-to (BP1) and sold-to party (KUNAG) over a 60-day window. Supports dispute and collection follow-up by customer.

**Use Case 4: Posting and invoice list status focus**
```
BACKDAYS = 7
BUCHK = A
RELIK = 
RRSTA = 
FKART = F2
```
**Purpose:** Focus on recent billing documents (last 7 days) by posting status, invoice list status, revenue determination status, and billing type. Use for status-based exception review.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_BILL_STAT | .INCLU--AP |  |  |  |
| /SKN/S_SW_10_01_BILL_STAT | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_BILL_STAT | BLOCK | Indicator: Document preselected for archiving | CHAR(1) | BLOCK_VB |
| /SKN/S_SW_10_01_BILL_STAT | BP1_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_BILL_STAT | BP1_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_BILL_STAT | BP1_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_BILL_STAT | BP2_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_BILL_STAT | BP2_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_BILL_STAT | BP2_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_BILL_STAT | BP3_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_BILL_STAT | BP3_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_BILL_STAT | BP3_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_BILL_STAT | BUCHK | Posting Status of Billing Document | CHAR(1) | BUCHK |
| /SKN/S_SW_10_01_BILL_STAT | BZIRK | Sales district | CHAR(6) | BZIRK |
| /SKN/S_SW_10_01_BILL_STAT | COSTA | Confirmation status for ALE | CHAR(1) | COSTA_D |
| /SKN/S_SW_10_01_BILL_STAT | DUMMY | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_01_BILL_STAT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_BILL_STAT | DURATION_D | SW: Duration In Days | NUMC(6) | /SKN/E_SW_DURATION_D |
| /SKN/S_SW_10_01_BILL_STAT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_BILL_STAT | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_BILL_STAT | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_BILL_STAT | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_BILL_STAT | FKART | Billing Type | CHAR(4) | FKART |
| /SKN/S_SW_10_01_BILL_STAT | FKDAT | Billing date for billing index and printout | DATS(8) | FKDAT |
| /SKN/S_SW_10_01_BILL_STAT | FKSTO | Billing document is cancelled | CHAR(1) | FKSTO |
| /SKN/S_SW_10_01_BILL_STAT | FKTYP | Billing category | CHAR(1) | FKTYP |
| /SKN/S_SW_10_01_BILL_STAT | FMSTK | Status Funds Management | CHAR(1) | FMSTK |
| /SKN/S_SW_10_01_BILL_STAT | KDGRP | Customer group | CHAR(2) | KDGRP |
| /SKN/S_SW_10_01_BILL_STAT | KUNAG | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_BILL_STAT | KUNRG | Payer | CHAR(10) | KUNRG |
| /SKN/S_SW_10_01_BILL_STAT | NETWR | Net Value in Document Currency | CURR(15,2) | NETWR |
| /SKN/S_SW_10_01_BILL_STAT | PAYER_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_BILL_STAT | RELIK | Invoice list status of billing document | CHAR(1) | RELIK |
| /SKN/S_SW_10_01_BILL_STAT | RFBSK | Status for transfer to accounting | CHAR(1) | RFBSK |
| /SKN/S_SW_10_01_BILL_STAT | RRSTA | Revenue determination status | CHAR(1) | RR_STATUS |
| /SKN/S_SW_10_01_BILL_STAT | SOLDTO_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_BILL_STAT | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_BILL_STAT | UVK01 | Customer reserves 1: Header status | CHAR(1) | UVK01 |
| /SKN/S_SW_10_01_BILL_STAT | UVK02 | Customer reserves 2: Header status | CHAR(1) | UVK02 |
| /SKN/S_SW_10_01_BILL_STAT | UVK03 | Customer reserves 3: Header status | CHAR(1) | UVK03 |
| /SKN/S_SW_10_01_BILL_STAT | UVK04 | Custmer reserves 4: Header status | CHAR(1) | UVK04 |
| /SKN/S_SW_10_01_BILL_STAT | UVK05 | Customer reserves 5: Header status | CHAR(1) | UVK05 |
| /SKN/S_SW_10_01_BILL_STAT | UVS01 | Customer reserves 1: Sum of all items | CHAR(1) | UVS01 |
| /SKN/S_SW_10_01_BILL_STAT | UVS02 | Customer reserves 2: Sum of all items | CHAR(1) | UVS02 |
| /SKN/S_SW_10_01_BILL_STAT | UVS03 | Customer reserves 3: Sum of all items | CHAR(1) | UVS03 |
| /SKN/S_SW_10_01_BILL_STAT | UVS04 | Customer reserves 4: Sum of all items | CHAR(1) | UVS04 |
| /SKN/S_SW_10_01_BILL_STAT | UVS05 | Customer reserves 5: Sum of all items | CHAR(1) | UVS05 |
| /SKN/S_SW_10_01_BILL_STAT | VBELN | Billing Document | CHAR(10) | VBELN_VF |
| /SKN/S_SW_10_01_BILL_STAT | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_BILL_STAT | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_BILL_STAT | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_BILL_STAT | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_S_FCAT | DECIMALS | Number of Decimal Places | NUMC(6) | DECIMALS |
| /SKN/S_SW_S_FCAT | FIELDNAME | ALV control: Field name of internal table field | CHAR(30) | LVC_FNAME |
| /SKN/S_SW_S_FCAT | INTLEN | Internal Length in Bytes | NUMC(6) | INTLEN |
| /SKN/S_SW_S_FCAT | INTTYPE | ABAP data type (C,D,N,...) | CHAR(1) | INTTYPE |
| /SKN/S_SW_S_FCAT | REF_FIELD | ALV control: Reference field name for internal table field | CHAR(30) | LVC_RFNAME |
| /SKN/S_SW_S_FCAT | REF_TABLE | ALV control: Reference table name for internal table field | CHAR(30) | LVC_RTNAME |
| /SKN/S_SW_S_FCAT | ROLLNAME | Data element (semantic domain) | CHAR(30) | ROLLNAME |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_BILL_STAT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_BILL_STAT OPTIONAL
*"----------------------------------------------------------------------
data_single: MANAGE_IN_UTC  CHAR1 ,
             LANGU  LANGU,
             BACKDAYS INT4,
             BP1_FUNCT   PARVW,
             BP2_FUNCT   PARVW,
             BP3_FUNCT   PARVW,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
 lv_BACKDAYS = 1.
 lv_DATE_REF_FLD = 'FKDAT'. "Billing date
 lv_DURATION_UNIT = 'D'.
 select_single: MANAGE_IN_UTC,
                LANGU,
                BACKDAYS,
                BP1_FUNCT,
                BP2_FUNCT,
                BP3_FUNCT,
                DATE_REF_FLD,
                DURATION_UNIT.
data_multy: VBELN        VBELN_VF,
            FKART        FKART,
            FKTYP        FKTYP,
            VBTYP        VBTYP,
            VKORG        VKORG,
            VTWEG        VTWEG,
            KDGRP        KDGRP,
            BZIRK        BZIRK,
            FKDAT        FKDAT,
            ERDAT        ERDAT,
            ERNAM        ERNAM,
            AEDAT        AEDAT,
            DATUM        sy-datum,
"            DURATION_M   /SKN/E_SW_DURATION_M,
"            DURATION_H   /SKN/E_SW_DURATION_H,
"            DURATION_D   /SKN/E_SW_DURATION_D,
            DURATION    /SKN/E_SW_DURATION,
            KUNRG       KUNRG,
            KUNAG       KUNAG,
            SPART       SPART,
            BUCHK       BUCHK,
            RELIK       RELIK,
            RRSTA       RR_STATUS,
            BLOCK       BLOCK_VB,
            RFBSK       RFBSK,
            BP1_CODE    KUNNR,
            BP2_CODE    KUNNR,
            BP3_CODE    KUNNR,
            BP_FUNCT    PARVW,
            FKSTO        FKSTO.
select_multy:
            VBELN,
            FKART ,
            FKTYP,
            VBTYP ,
            VKORG ,
            VTWEG,
            KDGRP,
            BZIRK,
            FKDAT,
            ERDAT,
            ERNAM,
            AEDAT,
            DATUM,
"            DURATION_M,
"            DURATION_H ,
"            DURATION_D,
            DURATION,
            KUNRG ,
            KUNAG,
            SPART,
            BUCHK,
            RELIK,
            RRSTA,
            BLOCK,
            RFBSK,
            BP1_CODE,
            BP2_CODE,
            BP3_CODE,
            FKSTO.
convert_multy: KUNRG ALPHA,
               KUNAG ALPHA,
               VBELN ALPHA,
               BP1_CODE ALPHA,
               BP2_CODE ALPHA,
               BP3_CODE ALPHA.
  ""Tanya 14/11/18 :
  convert_single:  bp1_funct PARVW ,
                   bp2_funct PARVW ,
                   bp3_funct PARVW .
ranges : R_FLD_NAME for DD03P-FIELDNAME,
         R_FLD_VAL for DD03P-FIELDNAME .
data :   FLD_NAME type FIELDNAME.
data : i type I,
       ci(1) type c,
       nfields type I value 3.   "
data : BACKDAYS  type I ,
       DATE_FROM like sy-datum .
data : LANGU like SY-LANGU .
data : is_out(1) type C.
data : TIME_DIFF TYPE  INT4 .
data : W_DATA like line of T_DATA .
data : wa_VBPA type VBPA.
data : lv_VBELN type VBELN,
       lv_POSNR type POSNR,
       lv_PARVW type PARVW,
       lv_KUNNR TYPE  KUNNR,
       lv_KUNNR_NAME TYPE  NAME1_GP,
       lv_LIFNR TYPE  LIFNR,
       lv_LIFNR_NAME TYPE  NAME1_GP,
       lv_PERNR TYPE  PERNR_D,
       lv_PERNR_NAME TYPE  NAME1_GP,
       lv_NRART type NRART.
data : sy_tabix like sy-tabix .
data : fld(60) type c .
data : ref_date type D.
*data: lra_range type range of DD03P-FIELDNAME.
FIELD-SYMBOLS: <fs> TYPE ANY ,
               <fs_v> TYPE ANY .
data : begin of SW_STRUCTURE occurs 0.
  include structure /SKN/S_SW_S_FCAT .
data : end of SW_STRUCTURE .
data : ls_VBPA type VBPA,
       lt_VBPA like TABLE OF ls_VBPA.
data : lv_data_POSNR type POSNR.
"--- Run Cloud Mode -----
  data_single: sw_dest rfcdest.             .
  select_single: sw_dest.
  if lv_sw_dest is not initial.
    data: lv_IS_HANA(1) type C.
    CALL FUNCTION '/SKN/F_SW_IS_RFCDEST_HANA'
      EXPORTING
        dest          = lv_sw_dest
      IMPORTING
        IS_HANA       =  lv_IS_HANA.
   if lv_IS_HANA is not initial.
    CALL FUNCTION '/SKN/FH_SW_10_01_BILL_STAT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   else.
    CALL FUNCTION '/SKN/FC_SW_10_01_BILL_STAT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   endif.
  endif.
  check lv_sw_dest is initial.
"--- Run Cloud Mode -----
   if R_DATUM[] is initial .
     RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
       DATE_FROM = sy-datum - lv_BACKDAYS .
       RS_DATUM-LOW = DATE_FROM .
        APPEND RS_DATUM to R_DATUM.
   endif.
 "--- Set Reference Date Field
   CASE lv_DATE_REF_FLD.
     when 'ERDAT'.
       R_ERDAT[] = R_DATUM[]. "Document created
     when 'AEDAT'.
       R_AEDAT[] = R_DATUM[]. "changed on
     WHEN OTHERS.
       R_FKDAT[] = R_DATUM[]. "Billing date
   ENDCASE.
*--- Retrieve data
  clear IS_ALERT .
  refresh t_data.
  select *
    from VBRK as a
    inner join VBUK as k
    on a~VBELN = k~VBELN
    into CORRESPONDING FIELDS OF TABLE T_DATA
    where a~VBELN in R_VBELN
      and  a~FKART in R_FKART
      and a~FKTYP in R_FKTYP
      and a~VBTYP in R_VBTYP
      and a~VKORG in R_VKORG
      and a~VTWEG in R_VTWEG
      and a~KDGRP in R_KDGRP
      and a~BZIRK in R_BZIRK
      and a~FKDAT in R_FKDAT
      and a~ERDAT in R_ERDAT
      and a~ERNAM in R_ERNAM
      and a~AEDAT in R_AEDAT
      and a~KUNRG in R_KUNRG
      and a~KUNAG in R_KUNAG
      and a~SPART in R_SPART
      and k~BUCHK in R_BUCHK
      and k~RELIK in R_RELIK
      and k~RRSTA in R_RRSTA
      and k~BLOCK in R_BLOCK
      and a~RFBSK in R_RFBSK
      and a~FKSTO in R_FKSTO.
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  loop at t_data .
    sy_tabix = sy-tabix .
    concatenate 'T_DATA-' lv_DATE_REF_FLD into fld .
    ASSIGN (fld) TO <fs>.
    ref_date = <fs> .
    if not ref_date is initial.
      t_data-duration_unit = lv_duration_unit.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = ref_date
          T_FROM            = sy-uzeit
          D_TO              = sy-datum
          T_TO              = sy-uzeit
          TIME_UNIT         = lv_duration_unit   "'D'
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
      IF SY-SUBRC = 0.
        if TIME_DIFF < '999999'.
          t_data-DURATION  = TIME_DIFF .
        else.
          t_data-DURATION  = '999999'.
        endif.
      endif.
      modify t_data index sy_tabix.
    endif.
  endloop.
  delete t_data where DURATION  not in R_DURATION .
******************************************************************************
********************************************************************************
  "--- Get BPs
  if t_data[] is not initial.
    "--- Fill R_BP_FUNCT ----
    refresh R_BP_FUNCT.
    set_BP_range 1.
    set_BP_range 2.
    set_BP_range 3.
   if R_BP_FUNCT[] is not initial.
     select * from VBPA
        into CORRESPONDING FIELDS OF TABLE lt_VBPA
        FOR ALL ENTRIES IN t_data
        where VBELN = t_data-VBELN
          and PARVW in R_BP_FUNCT.
     sort lt_VBPA by VBELN POSNR PARVW.
     loop at t_data.
       sy_tabix = sy-tabix .
       get_BP_attr 1.
       get_BP_attr 2.
       get_BP_attr 3.
       modify t_data index sy_tabix.
     endloop.
    "--- Get BPs
*  "--- Get BPs
*  loop at t_data.
*    sy_tabix = sy-tabix .
*    get_BP_attr 1.
*    get_BP_attr 2.
*    get_BP_attr 3.
*    modify t_data index sy_tabix.
*  endloop.
   "--- Get BPs
    delete t_data where BP1_CODE not in R_BP1_CODE.
    delete t_data where BP2_CODE not in R_BP2_CODE.
    delete t_data where BP3_CODE not in R_BP3_CODE.
   endif.
  endif.
  loop at t_data .
    sy_tabix = sy-tabix .
*Payer desc
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR                = t_data-KUNRG
      IMPORTING
        CUST_DESC            = t_data-PAYER_DESC
      EXCEPTIONS
        WRONG_CUSTOMER       = 1
        OTHERS               = 2              .
    IF SY-SUBRC <> 0.
    ENDIF.
*Sold-to party desc
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR                = t_data-KUNAG
      IMPORTING
        CUST_DESC            = t_data-SOLDTO_DESC
      EXCEPTIONS
        WRONG_CUSTOMER       = 1
        OTHERS               = 2              .
    IF SY-SUBRC <> 0.
    ENDIF.
    modify t_data index sy_tabix.
  endloop.
*--- Check Alert Information
 read table t_data index 1.
 check not sy-tfill  is initial .
 IS_ALERT = 'X' .
ENDFUNCTION.
```