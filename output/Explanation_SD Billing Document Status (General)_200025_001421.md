# Exception Indicator: SD Billing Document Status (General) - SW_10_01_BILL_STAT

## General Overview

This Exception Indicator (EI) monitors SD billing document status at the document level, identifying billing documents that meet configurable criteria for status, date, organizational dimension, and business partner. It provides visibility into billing document status across configurable time periods and organizational dimensions, supporting detection of billing documents in specific statuses (e.g. posting, revenue determination, transfer to accounting) that require management attention or follow-up.

This EI serves as an essential control for billing and revenue oversight by:
- Enabling detection of billing documents in specific statuses (posting, invoice list, revenue determination, transfer to accounting) that may require follow-up or correction
- Supporting identification of billing documents by billing date, creation date, or changed-on date for period-based and age-based monitoring
- Providing visibility into billing status duration for prioritization and root-cause analysis
- Enabling analysis by sales organization, distribution channel, division, customer group, and business partner roles for accountability and delegation review
- Supporting audit readiness by surfacing billing documents that require business justification or status resolution

The EI is valuable for month-end close processes, billing operations control, and revenue recognition monitoring. It helps ensure that billing document status is monitored across organizational dimensions and time windows.


## Problem Description

Failure to monitor SD billing document status creates multiple risks across financial reporting, operational control, and compliance:

**Financial and Reporting Issues**
- Undetected billing documents in specific statuses (e.g. not yet posted, not yet transferred to accounting) can distort period-end close and revenue recognition timing
- Billing documents stuck in intermediate statuses may delay month-end close when discovered late during financial review
- Unreported status patterns by sales organization or customer group can signal systemic process or system issues requiring management intervention
- Revenue determination and transfer-to-accounting status exceptions may require additional audit scrutiny but go unnoticed without monitoring

**Billing Operations and Control Risks**
- Billing documents without status visibility may indicate process bottlenecks, errors, or inadequate follow-up
- Lack of monitoring by billing type, billing category, or organizational dimension can mask repeated status issues by specific document types or areas
- Exceptions by business partner role (sold-to, payer) may reveal delegation or master data quality issues
- Unchecked status patterns can undermine billing policies and create audit findings
- High volumes of exceptions could indicate integration or workflow failures requiring immediate correction

**Management Visibility and Decision-Making Risks**
- Absence of monitoring delays executive awareness of billing and revenue control weaknesses
- Unidentified status patterns can lead to missed opportunities for process improvement or system correction
- Billing status exceptions may require additional compliance review but go unnoticed without the EI
- Lack of visibility by organizational dimension limits ability to assign accountability and remediate root causes

## Suggested Resolution

**Immediate Response**
- Review the flagged billing documents to confirm status (posting, invoice list, revenue determination, transfer to accounting) and understand the business context
- Verify high-value or high-volume exceptions using transaction VF03 (Display Billing Document) to confirm status and legitimacy
- Check whether the status indicates a legitimate delay, data entry error, or system issue
- Identify whether exceptions correlate with specific billing types, sales organizations, or customer segments

**System Assessment**
- Analyze the reference date used (e.g. billing date, creation date, changed-on date) and the lookback window to ensure the monitoring scope is appropriate
- Compare current exception counts and patterns to prior periods to identify trends or one-time spikes
- Examine distribution by sales organization, distribution channel, division, and customer group to pinpoint concentration or process issues
- Assess business partner (sold-to, payer) distribution to determine if exceptions correlate with specific roles or configurations
- Validate that the date range and organizational filters align with the intended control objective

**Corrective Actions**
- Where status indicates errors or blocks, initiate billing document correction or release procedures (e.g. VF02, VFX3)
- For legitimate delays, document business justification and consider process or workflow updates to reduce future exceptions
- Update master data or customizing if exceptions point to configuration or delegation issues
- Adjust monitoring parameters (e.g. lookback days, reference date field, duration, organizational scope) to focus on material exceptions and reduce noise
- Establish recurring EI execution and alert routing to billing and finance stakeholders for continuous control monitoring


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | BLOCK | Indicator: Document preselected for archiving | CHAR | 1 | 0 | BLOCK_VB | BLOCK_VB |
| 4 | BP1_CODE | Partner1 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 5 | BP1_FUNCT | Partner1 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 6 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 7 | BP2_CODE | Partner2 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 8 | BP2_FUNCT | Partner2 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 9 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 10 | BP3_CODE | Partner3 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 11 | BP3_FUNCT | Partner3 - Function | CHAR | 2 | 0 | PARVW | PARVW |
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
| 32 | LANG | Language for texts |  | 0 | 0 |  |  |
| 33 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 34 | NETWR | Net Value | CURR | 15 | 2 | NETWR | WERTV8 |
| 35 | PAYER_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 36 | RELIK | Inv.list status | CHAR | 1 | 0 | RELIK | STATV |
| 37 | RFBSK | Status for transfer to account | CHAR | 1 | 0 | RFBSK | RFBSK |
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

Date when the billing document was last changed. The EI uses it as the reference date for the monitoring window when DATE_REF_FLD is set to AEDAT; the chosen date field is restricted to the lookback window when reading billing data.

**BACKDAYS** (Backdays):

Number of days used to build the monitoring window. When no date range is supplied, the EI uses today minus this value as the start date; the chosen reference date field (ERDAT, AEDAT, or FKDAT) is restricted to that window when reading billing document data.

**BLOCK** (Indicator: Document preselected for archiving):

Indicates whether the billing document is preselected for archiving. The EI restricts by this status so configurations can focus on documents not yet archived or on archiving candidates.

**BLOCK Options:**
- **X**: Preselected for archiving; ** ** (space): not preselected. Values are domain-specific (BLOCK_VB).

**BP1_CODE - BP3_CODE** (Partner1 - Code – Partner3 - Code):

Customer or business partner codes for up to three partner roles. Each pair BPn_FUNCT + BPn_CODE defines a partner role and the customer code to filter by; the EI reads partner data from VBPA and filters by these codes.

**BP1_FUNCT - BP3_FUNCT** (Partner1 - Function – Partner3 - Function):

Partner function codes (e.g. RG, RE, WE) for up to three partner roles. Each pair BPn_FUNCT + BPn_CODE defines which partner role and which customer code to filter by.

**BP1_FUNCT - BP3_FUNCT Options:**
- **RG**: Sold-to party; **RE**: Bill-to party; **WE**: Ship-to party; **AG**: Payer. Other partner function values as in standard SAP (PARVW domain).

**BP1_FUNCT and BP1_CODE Connection:** BP1_FUNCT defines the partner role; BP1_CODE holds the customer code for that role. Set both when filtering by a specific partner. Same for BP2 and BP3.

**BP1_NAME - BP3_NAME** (Name – Name):

Names of the business partners for roles 1–3, resolved from master data.

**BUCHK** (Posting Status):

Posting status of the billing document. The EI restricts by this status so only documents in the selected posting states are included.

**BUCHK Options:**
- Values are from domain STATV (e.g. posted, not posted). Use customizing or domain values for the exact list.

**BZIRK** (Sales district):

Sales district. The EI restricts and reports by sales district for organizational scope.

**COSTA** (Confirmation status):

Confirmation status for ALE. The EI can include it in selection or result for confirmation-based scope when available.

**COSTA Options:**
- Values are domain-specific (COSTA). Use customizing for the exact list.

**DATE_REF_FLD** (Date Ref Field):

Selects which date field on the billing document is used for the monitoring window and for duration calculation: creation date (ERDAT), changed-on date (AEDAT), or billing date (FKDAT). The EI applies the lookback window to the chosen field when reading data.

**DATE_REF_FLD Options:**
- **ERDAT**: Date on which record was created.
- **AEDAT**: Changed on.
- **FKDAT**: Billing date; default in the code.

**BACKDAYS and DATE_REF_FLD Connection:** BACKDAYS defines the lookback length; DATE_REF_FLD defines which date field is restricted to that window. Set both when configuring the monitoring window.

**DUMMY** (Single-Character Flag):

Single-character indicator; used internally when needed. Values are typically **X** or ** ** (space).

**DUMMY Options:**
- **X**: Set; ** ** (space): not set.

**DURATION** (Duration In Time Units):

Elapsed time between the reference date (ERDAT, AEDAT, or FKDAT, as configured) and the evaluation date, in the unit given by DURATION_UNIT. The EI calculates this per billing document and uses it for duration-based filtering.

**DURATION_D** (Duration In Days):

Duration in days; derived or populated for display when duration is expressed in days.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed and evaluated (hours, minutes, days, or full days for specific-day logic). The EI uses this when computing and comparing duration for each billing document.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** DURATION holds the numeric value; DURATION_UNIT defines its meaning. Set both when using duration-based filtering.

**ERDAT** (Created On):

Date when the billing document was created. Can be used as the reference date for the monitoring window when DATE_REF_FLD is set to ERDAT; the EI then restricts creation date to the lookback window.

**ERNAM** (Created by):

User who created the billing document. The EI includes it in selection and result for creator-based scope or accountability.

**ERZET** (Time):

Entry time when the billing document was created. The EI includes it in the result for timestamp context.

**FKART** (Billing Type):

Billing type (e.g. F2, F8). The EI restricts which billing types are read; each type can have different status and business semantics.

**FKDAT** (Billing Date):

Billing date on the billing document. The EI uses it as the default reference date for the monitoring window when DATE_REF_FLD is not supplied; when set to FKDAT, the EI restricts this field to the lookback window.

**FKSTO** (Cancelled):

Indicates whether the billing document is cancelled. The EI typically excludes cancelled documents so only active billing documents are evaluated.

**FKSTO Options:**
- **X**: Cancelled; ** ** (space): not cancelled (active).

**FKTYP** (Billing category):

Billing category (e.g. order-related, delivery-related). The EI includes it in selection and result for scope by category.

**FKTYP Options:**
- Values are from domain FKTYP (e.g. **C**: order-related; **L**: delivery-related). Use customizing or domain values for the exact list.

**FMSTK** (Status Funds Management):

Status for funds management. The EI can include it in selection or result for funds-management status when available.

**FMSTK Options:**
- Values are from domain STATV. Use customizing for the exact list.

**KDGRP** (Customer group):

Customer group. The EI restricts and reports by customer group for organizational scope.

**KUNAG** (Sold-to party):

Sold-to party customer number on the billing document. The EI restricts and reports by sold-to for customer-level monitoring.

**KUNRG** (Payer):

Payer customer number on the billing document. The EI restricts and reports by payer for customer-level monitoring.

**LANG** (Language for texts):

Language key for description texts. The EI uses it when resolving domain or master data descriptions.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set to **X**, dates/times are managed in UTC. When not set, system local time is used. The EI uses it for date/time handling when applicable.

**MANAGE_IN_UTC Options:**
- **X**: Manage in UTC; ** ** (space): use system local time.

**NETWR** (Net Value):

Net value of the billing document in document currency. The EI can use it for value-based filtering or display when exposed.

**PAYER_DESC** (Name):

Name of the payer from master data (KNA1).

**RELIK** (Inv.list status):

Invoice list status of the billing document. The EI restricts by this status so only documents in the selected invoice-list states are included.

**RELIK Options:**
- Values are from domain STATV (e.g. not yet in list, in list). Use customizing for the exact list.

**RFBSK** (Status for transfer to account):

Status for transfer to accounting. The EI restricts by this status so only documents in the selected transfer states are included.

**RFBSK Options:**
- Values are from domain RFBSK (e.g. not yet transferred, transferred). Use customizing for the exact list.

**RRSTA** (Rev. determ. status):

Revenue determination status. The EI restricts by this status so only documents in the selected revenue-determination states are included.

**RRSTA Options:**
- Values are from domain RR_STATUS/STATV. Use customizing for the exact list.

**SOLDTO_DESC** (Name):

Name of the sold-to party from master data (KNA1).

**SPART** (Division):

Division. The EI restricts and reports by division for organizational scope.

**UVK01 - UVK05** (Header reserves 1 – Header reserves 5):

Customer reserve statuses 1–5 at header level. Each restricts which billing documents are included based on the corresponding reserve status; the EI uses them for status-based filtering.

**UVK01 - UVK05 Options:**
- Values are from domain STATV (e.g. not set, set). Use customizing for the exact list per reserve.

**UVS01 - UVS05** (Total reserves 1 – Total reserves 5):

Customer reserve statuses 1–5 at total level (sum of all items). Each restricts which billing documents are included based on the corresponding total reserve status.

**UVS01 - UVS05 Options:**
- Values are from domain STATV. Use customizing for the exact list per reserve.

**VBELN** (Billing Document):

Billing document number. The EI reads billing header data (VBRK) and status (VBUK) keyed by this identifier; values scope which billing documents are evaluated.

**VBTYP** (SD document categ.):

SD document category (e.g. billing document type). The EI includes it in selection and result for scope by category.

**VBTYP Options:**
- **M**: Billing document; **C**: Order; **A**: Contract. Other domain values as in standard SAP.

**VKORG** (Sales Organization):

Sales organization. The EI restricts and reports by sales organization for organizational scope.

**VTWEG** (Distribution Channel):

Distribution channel. The EI restricts and reports by distribution channel for organizational scope.

**WAERK** (Document Currency):

Document currency of the billing document; amounts such as NETWR are expressed in this currency.


### Parameter Relationships

**Time-Based and Duration Parameters:**
- **BACKDAYS** defines how many days to look back from today when no date range is supplied; the EI builds the monitoring window from today minus this value.
- **DATE_REF_FLD** selects which date field on the billing document is used for that window: ERDAT (creation), AEDAT (changed on), or FKDAT (billing date). The chosen field is restricted to the window when the EI reads billing data.
- **DURATION** and **DURATION_UNIT** work together: DURATION holds the elapsed time (in the unit given by DURATION_UNIT) between the reference date and the evaluation date; the EI calculates this per billing document and uses it for duration-based filtering. Set both when filtering by how long ago the billing document was created or billed.

**Business Partner Analysis Parameters:**
- **BP1_FUNCT** and **BP1_CODE** work together: BP1_FUNCT defines the partner role; BP1_CODE holds the customer code for that role. The EI uses them to filter by specific partners. Same for **BP2_FUNCT** + **BP2_CODE** and **BP3_FUNCT** + **BP3_CODE**.
- Use these pairs when focusing on specific sold-to, bill-to, ship-to, or payer and their customer codes.

**Billing Status Parameters:**
- **BUCHK** (posting status), **RELIK** (invoice list status), **RRSTA** (revenue determination status), and **RFBSK** (transfer to accounting status) are used together to scope billing documents by their workflow status. Use them when monitoring documents in specific statuses (e.g. not yet posted, not yet transferred to accounting).
- **UVK01–UVK05** (header reserves) and **UVS01–UVS05** (total reserves) are status series; use them when filtering by customer reserve status at header or total level.


### Default Values

- **BACKDAYS** — Default: `1` (when no date range is supplied, the EI uses a 1-day lookback from today for the monitoring window).
- **DATE_REF_FLD** — Default: `FKDAT` (billing date is used as the reference date for the monitoring window and duration calculation when not supplied).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).

**Note:** When no date range is supplied, the EI builds the monitoring window from today minus BACKDAYS and applies it to the date field selected by DATE_REF_FLD (ERDAT, AEDAT, or FKDAT).

### Practical Configuration Examples

**Use Case 1: Last 7 days by billing date**

```
BACKDAYS = 7
DATE_REF_FLD = FKDAT
```

**Purpose:** Monitor billing documents with billing date in the last 7 days. Suitable for routine weekly billing status review.

**Use Case 2: By billing type and sales organization**

```
FKART = F2, F8
VKORG = 1000, 2000
VTWEG = 10
```

**Purpose:** Limit results to specific billing types (e.g. invoice, credit memo) and sales organization and distribution channel. Supports regional or type-specific billing status monitoring.

**Use Case 3: Duration in full days, reference date, and status**

```
DATE_REF_FLD = FKDAT
DURATION_UNIT = F
DURATION = 14
BUCHK = A, B
BACKDAYS = 30
```

**Purpose:** Flag billing documents with billing date exactly 14 full days ago, within a 30-day lookback, and with specific posting status values. Useful for age-based prioritization of billing documents in certain statuses. DURATION is a single value when using DURATION_UNIT = F.

**Use Case 4: Payer, sold-to, and organizational scope**

```
KUNRG = 0000100001–0000100200
KUNAG = 0000100001–0000100500
VKORG = 1000
SPART = 00, 01
KDGRP = 01, 02
```

**Purpose:** Focus on specific payer and sold-to customer ranges for selected sales organization, division, and customer group. Supports customer-level billing status monitoring.


### EI Function Structure

## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
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

### ABAP Code

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
DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
             LANGU  LANGU,
             BACKDAYS INT4,
             BP1_FUNCT   PARVW,
             BP2_FUNCT   PARVW,
             BP3_FUNCT   PARVW,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
 LV_BACKDAYS = 1.
 LV_DATE_REF_FLD = 'FKDAT'.
 LV_DURATION_UNIT = 'D'.
 SELECT_SINGLE: MANAGE_IN_UTC,
                LANGU,
                BACKDAYS,
                BP1_FUNCT,
                BP2_FUNCT,
                BP3_FUNCT,
                DATE_REF_FLD,
                DURATION_UNIT.
DATA_MULTY: VBELN        VBELN_VF,
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
            DATUM        SY-DATUM,
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
SELECT_MULTY:
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
CONVERT_MULTY: KUNRG ALPHA,
               KUNAG ALPHA,
               VBELN ALPHA,
               BP1_CODE ALPHA,
               BP2_CODE ALPHA,
               BP3_CODE ALPHA.
CONVERT_SINGLE: BP1_FUNCT PARVW ,
                BP2_FUNCT PARVW ,
                BP3_FUNCT PARVW .
RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
         R_FLD_VAL FOR DD03P-FIELDNAME .
DATA : FLD_NAME TYPE FIELDNAME.
DATA : I TYPE I,
       CI(1) TYPE C,
       NFIELDS TYPE I VALUE 3.
DATA : BACKDAYS  TYPE I ,
       DATE_FROM LIKE SY-DATUM .
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
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    DATA: LV_IS_HANA(1) TYPE C.
    CALL FUNCTION '/SKN/F_SW_IS_RFCDEST_HANA'
      EXPORTING
        DEST          = LV_SW_DEST
      IMPORTING
        IS_HANA       =  LV_IS_HANA.
   IF LV_IS_HANA IS NOT INITIAL.
    CALL FUNCTION '/SKN/FH_SW_10_01_BILL_STAT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   ELSE.
    CALL FUNCTION '/SKN/FC_SW_10_01_BILL_STAT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   ENDIF.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
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
     WHEN 'AEDAT'.
       R_AEDAT[] = R_DATUM[].
     WHEN OTHERS.
       R_FKDAT[] = R_DATUM[].
   ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM VBRK AS A
    INNER JOIN VBUK AS K
    ON A~VBELN = K~VBELN
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE A~VBELN IN R_VBELN
      AND A~FKART IN R_FKART
      AND A~FKTYP IN R_FKTYP
      AND A~VBTYP IN R_VBTYP
      AND A~VKORG IN R_VKORG
      AND A~VTWEG IN R_VTWEG
      AND A~KDGRP IN R_KDGRP
      AND A~BZIRK IN R_BZIRK
      AND A~FKDAT IN R_FKDAT
      AND A~ERDAT IN R_ERDAT
      AND A~ERNAM IN R_ERNAM
      AND A~AEDAT IN R_AEDAT
      AND A~KUNRG IN R_KUNRG
      AND A~KUNAG IN R_KUNAG
      AND A~SPART IN R_SPART
      AND K~BUCHK IN R_BUCHK
      AND K~RELIK IN R_RELIK
      AND K~RRSTA IN R_RRSTA
      AND K~BLOCK IN R_BLOCK
      AND A~RFBSK IN R_RFBSK
      AND A~FKSTO IN R_FKSTO.
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
          TIME_UNIT         = LV_DURATION_UNIT
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
  "--- Get BPs
  IF T_DATA[] IS NOT INITIAL.
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
  ENDIF.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR                = T_DATA-KUNRG
      IMPORTING
        CUST_DESC            = T_DATA-PAYER_DESC
      EXCEPTIONS
        WRONG_CUSTOMER       = 1
        OTHERS               = 2              .
    IF SY-SUBRC <> 0.
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR                = T_DATA-KUNAG
      IMPORTING
        CUST_DESC            = T_DATA-SOLDTO_DESC
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
