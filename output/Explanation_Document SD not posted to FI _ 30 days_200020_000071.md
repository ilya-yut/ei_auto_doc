# Exception Indicator: SD Billing Doc. -post. stat.(Excl. invoice list) - SW_10_01_BILL_POST

## General Overview

This Exception Indicator (EI) monitors SD billing documents to identify billing documents that have not been posted to FI within a configurable period (e.g. 30 days). It provides visibility into billing documents by billing date, posting status, organizational dimension, and duration since billing, supporting revenue recognition, month-end close, and exception management.

This EI serves as an essential control for order-to-cash and financial reporting by:
- Enabling detection of billing documents not yet posted to FI that may delay revenue recognition or period-end close
- Supporting identification of posting and invoice-list status exceptions by sales organization, distribution channel, and customer for accountability and prioritization
- Providing visibility into how long billing documents have remained unposted (duration since billing or creation date) for aging and root-cause analysis
- Enabling analysis by payer, sold-to party, and partner role for credit and collections coordination
- Supporting month-end close and audit by surfacing billing documents that require posting or resolution

Monitoring unposted billing documents helps organizations prioritize FI posting, align with revenue recognition policies, and reduce revenue leakage. The EI is particularly valuable for month-end close, revenue recognition, and exception handling in sales and distribution.

The EI uses billing document header (VBRK) and status (VBUK) data and supports filtering by billing type, sales org, distribution channel, billing date, posting status, and revenue determination status.


## Problem Description

Failure to monitor SD billing documents not posted to FI creates multiple risks across revenue recognition, financial reporting, and compliance:

**Financial and Reporting Issues**
- Unposted billing documents can delay revenue recognition and distort period-end reporting
- Billing documents that remain unposted beyond a threshold (e.g. 30 days) may indicate process failures or blocking issues that require escalation
- Lack of visibility into posting and invoice-list status complicates month-end close and audit evidence

**Operational and Control Risks**
- Billing documents blocked from posting (e.g. posting status, revenue determination status) may go unreviewed when not monitored by organization and customer
- Inability to filter by billing date, posting status, or duration limits effective prioritization and segregation of duties
- Missing visibility into how long documents have been unposted hinders root-cause analysis and posting coordination

**Management Visibility and Decision-Making Risks**
- Absence of unposted-billing monitoring delays management awareness of posting exceptions that affect revenue and close
- Lack of organizational and duration-based analysis limits the ability to allocate finance and SD resources
- Inadequate visibility hinders posting workflows and corrective action when revenue or audit issues are reported

## Suggested Resolution

**Immediate Response**
- Review the billing documents flagged by the EI to confirm posting status, invoice-list status, and revenue determination status
- Verify high-value or aged unposted documents using the relevant transaction (e.g. VF03 for billing document) to confirm blocking reasons
- Check billing date and duration to assess how long the document has been unposted and prioritize by impact
- Identify business context: posting block, revenue determination issue, or process exception requiring resolution

**System Assessment**
- Analyze the time window and reference date field used for the run to ensure the monitoring scope matches the control objective
- Compare unposted document counts and value to prior periods to detect deterioration or improvement after posting actions
- Review sales org, distribution channel, division, and customer filters to focus on the right segments
- Validate that duration and reference date settings align with the intended use (e.g. focus on documents unposted more than 30 days)

**Corrective Actions**
- If blocks are legitimate, coordinate with finance and SD to resolve posting blocks (e.g. correct master data, release blocks) and document the action
- Escalate repeated or systemic posting failures to finance and sales for process and authorization review
- Update monitoring parameters (e.g. lookback, reference date field, duration filter, organizational scope) to align with revenue recognition and close requirements
- Document posting outcomes and establish recurring EI runs to maintain visibility into unposted billing documents


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

Change date of the billing document. Restricts which billing documents are included by change date; when DATE_REF_FLD = AEDAT, the default range is applied to this field. Also appears in the result.

**BACKDAYS** (Backdays):

Number of days to look back from the current date when building the default monitoring window. When no explicit date range is supplied, the EI uses today minus this value as the start of the window and applies it to the date field selected by DATE_REF_FLD.

**BLOCK** (Indicator: Document preselected for archiving):

Indicator that the document is preselected for archiving. Restricts which billing documents are included; used for filtering and display. Domain BLOCK_VB.

**BLOCK Options:**
- **X**: Set (preselected for archiving).
- ** ** (space or initial): Not set.

**BP1_CODE** (Partner1 - Code):

Partner number for the first partner role. Restricts which billing documents are included by partner; used with BP1_FUNCT and BP1_NAME.

**BP1_FUNCT** (Partner1 - Function):

Partner function for the first partner (e.g. sold-to, payer, bill-to). Restricts which billing documents are included by partner role; also appears in the result.

**BP1_NAME** (Name):

Name for the first partner. Populated from customer master; used for display with BP1_CODE.

**BP2_CODE** (Partner2 - Code):

Partner number for the second partner role. Restricts which billing documents are included by partner; used with BP2_FUNCT and BP2_NAME.

**BP2_FUNCT** (Partner2 - Function):

Partner function for the second partner. Restricts which billing documents are included by partner role; also appears in the result.

**BP2_NAME** (Name):

Name for the second partner. Populated from customer master; used for display with BP2_CODE.

**BP3_CODE** (Partner3 - Code):

Partner number for the third partner role. Restricts which billing documents are included by partner; used with BP3_FUNCT and BP3_NAME.

**BP3_FUNCT** (Partner3 - Function):

Partner function for the third partner. Restricts which billing documents are included by partner role; also appears in the result.

**BP3_NAME** (Name):

Name for the third partner. Populated from customer master; used for display with BP3_CODE.

**BUCHK** (Posting Status):

Posting status. Restricts which billing documents are included by posting status; used to focus on documents not yet posted to FI. Also appears in the result. Domain STATV.

**BZIRK** (Sales district):

Sales district. Restricts which billing documents are included; also appears in the result.

**COSTA** (Confirmation status):

Confirmation status. Restricts which billing documents are included; also appears in the result. Domain COSTA.

**DATE_REF_FLD** (Date Ref Field):

Name of the date field used as the reference for the default date range and for duration calculation. Determines which date (e.g. billing date, creation date, change date) is used when no explicit range is supplied and for computing how long the document has been unposted.

**DATE_REF_FLD Options:**
- **FKDAT**: Billing date (default in code).
- **ERDAT**: Created on.
- **AEDAT**: Changed on.

**DUMMY** (Single-Character Flag):

Single-character flag. Used for technical or display purposes; values are domain-specific (CHAR1).

**DURATION** (Duration In Time Units):

Duration in the unit given by DURATION_UNIT (e.g. days) between the reference date (from DATE_REF_FLD) and the run date. Restricts which billing documents are included when a duration filter is applied; also appears in the result (e.g. how long the document has been unposted).

**DURATION_D** (Duration In Days):

Duration in days. Populated from the duration calculation when DURATION_UNIT is days; used for display and filtering.

**DURATION_UNIT** (Duration Unit):

Unit for DURATION (e.g. days, hours). Used with DURATION and DATE_REF_FLD when computing and filtering by how long the billing document has been unposted.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours.
- **M**: Minutes.

**ERDAT** (Created On):

Creation date of the billing document. Restricts which billing documents are included by creation date; when DATE_REF_FLD = ERDAT, the default range is applied to this field. Also appears in the result.

**ERNAM** (Created by):

User who created the billing document. Restricts which billing documents are included; also appears in the result.

**ERZET** (Time):

Creation time. Appears in the result; used for ordering and display.

**FKART** (Billing Type):

Billing type. Restricts which billing documents are included; also appears in the result.

**FKDAT** (Billing Date):

Billing date. Restricts which billing documents are included; when DATE_REF_FLD = FKDAT (default), the default range is applied to this field. Also appears in the result and is used for duration calculation.

**FKSTO** (Cancelled):

Cancelled indicator. Restricts which billing documents are included; also appears in the result. Domain XFELD.

**FKSTO Options:**
- **X**: Cancelled.
- ** ** (space or initial): Not cancelled.

**FKTYP** (Billing category):

Billing category. Restricts which billing documents are included; also appears in the result. Domain FKTYP.

**FMSTK** (Status Funds Management):

Funds management status. Restricts which billing documents are included; also appears in the result. Domain STATV.

**KDGRP** (Customer group):

Customer group. Restricts which billing documents are included; also appears in the result.

**KUNAG** (Sold-to party):

Sold-to party (customer). Restricts which billing documents are included; also appears in the result.

**KUNRG** (Payer):

Payer (customer). Restricts which billing documents are included; also appears in the result.

**LANGU** (Language for texts):

Language used when retrieving description texts. Determines the language of description fields in the result.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set, date and time used for the monitoring window and comparisons are interpreted in UTC. When not set, system local date/time is used.

**MANAGE_IN_UTC Options:**
- **X**: Use UTC for date/time.
- ** ** (space or initial): Use system local date/time.

**NETWR** (Net Value):

Net value in document currency. Represents the billing document net value; used for display and threshold filtering.

**PAYER_DESC** (Name):

Payer name. Populated from customer master; used for display.

**RELIK** (Inv.list status):

Invoice list status. Restricts which billing documents are included; used to focus on documents not yet fully posted (e.g. invoice list not complete). Also appears in the result. Domain STATV.

**RFBSK** (Posting Status):

Posting block. Restricts which billing documents are included; used to focus on posting-blocked documents. Also appears in the result. Domain RFBSK.

**RRSTA** (Rev. determ. status):

Revenue determination status. Restricts which billing documents are included; used to focus on revenue determination exceptions. Also appears in the result. Domain STATV.

**SOLDTO_DESC** (Name):

Sold-to party name. Populated from customer master; used for display.

**SPART** (Division):

Division. Restricts which billing documents are included; also appears in the result.

**UVK01 - UVK05** (Header reserves 1–5):

Header reserve statuses 1 through 5. Restrict which billing documents are included; also appear in the result. Domain STATV.

**UVS01 - UVS05** (Total reserves 1–5):

Total reserve statuses 1 through 5. Restrict which billing documents are included; also appear in the result. Domain STATV.

**VBELN** (Billing Document):

Billing document number. Restricts which billing documents are included; also appears in the result.

**VBTYP** (SD document categ.):

SD document category. Restricts which billing documents are included; also appears in the result. Domain VBTYP.

**VKORG** (Sales Organization):

Sales organization. Restricts which billing documents are included; also appears in the result.

**VTWEG** (Distribution Channel):

Distribution channel. Restricts which billing documents are included; also appears in the result.

**WAERK** (Document Currency):

Document currency. Represents the currency of the billing document; used for value display and comparison.


### Parameter Relationships

**Time and reference date parameters**

- **BACKDAYS** defines the default number of days to look back when no explicit date range is supplied. It is used together with the reference date field: when no range is supplied, the EI builds a range from today minus BACKDAYS and applies it to the field selected by **DATE_REF_FLD**.
- **DATE_REF_FLD** determines which date field (e.g. billing date, creation date, change date) is used for the default date range and for duration calculation. **BACKDAYS** and DATE_REF_FLD work together: the selection window is built from today minus BACKDAYS and applied to the chosen reference date field. In this EI the default is FKDAT (billing date).
- **DURATION** and **DURATION_UNIT** work together. DURATION_UNIT specifies the unit (e.g. days, hours) used when computing the duration between the reference date (from DATE_REF_FLD) and the run date. The computed duration is compared to the DURATION filter; only records that fall within the DURATION range are returned (e.g. documents unposted for more than 30 days).

**Partner and organizational scope**

- **BP1_FUNCT**, **BP1_CODE**, **BP2_FUNCT**, **BP2_CODE**, **BP3_FUNCT**, **BP3_CODE** together define partner roles (e.g. sold-to, payer, bill-to) and optional partner codes for filtering and display. Partner names (BP1_NAME, BP2_NAME, BP3_NAME) are populated from customer master.
- **VKORG**, **VTWEG**, **SPART**, **BZIRK**, **KDGRP** together define the organizational scope for billing documents: sales organization, distribution channel, division, sales district, and customer group. Used in the selection and appear in the result.

**Posting and status parameters**

- **BUCHK** (posting status), **RELIK** (invoice list status), **RRSTA** (revenue determination status), **BLOCK**, **RFBSK** (posting block), and **FKSTO** (cancelled) together define which billing documents are included. The EI focuses on documents not yet fully posted to FI; BUCHK, RELIK, and RRSTA are used to filter and appear in the result. **FKDAT**, **ERDAT**, **AEDAT** restrict which documents are included by billing date, creation date, and change date.


### Default Values

- **BACKDAYS** — Default: `1` in the code when not supplied; when no date range is supplied, the EI uses today minus BACKDAYS as the start of the window for the reference date field.
- **DATE_REF_FLD** — Default: `FKDAT` (billing date) in the code when not supplied; determines which date field is used for the default range and duration calculation.
- **DURATION_UNIT** — Default: `D` (days) in the code when not supplied; used for computing and filtering by duration.
- **LANGU** — Default: system language when not supplied; used for description texts.
- **MANAGE_IN_UTC** — Default: initial (empty); date and time are interpreted in system local time when not set. When set to 'X', UTC is used for the monitoring window and comparisons.

**Note:** When no explicit date range is supplied, the default range is applied to the date field selected by DATE_REF_FLD (e.g. FKDAT, ERDAT, AEDAT).

### Practical Configuration Examples

**Use Case 1: Billing documents not posted within 30 days (billing date)**

```
BACKDAYS = 60
DATE_REF_FLD = FKDAT
DURATION_UNIT = D
DURATION = 30 to 999999
VKORG = 1000
```

**Purpose:** Find billing documents in sales org 1000 that have been unposted for more than 30 days (based on billing date), for revenue recognition and month-end close review.

**Use Case 2: Last 7 days by creation date**

```
BACKDAYS = 7
DATE_REF_FLD = ERDAT
VKORG = 1000
VTWEG = 10
SPART = 00
```

**Purpose:** Monitor billing documents created in the last 7 days that are not yet posted to FI, for daily posting review in a specific sales org and channel.

**Use Case 3: Specific payer and posting status**

```
KUNRG = (payer range)
BUCHK = (posting status filter)
BACKDAYS = 30
DATE_REF_FLD = FKDAT
```

**Purpose:** Focus on unposted billing documents for specific payers and posting status over the last 30 days, for credit and collections coordination.

**Use Case 4: Reference date = change date**

```
BACKDAYS = 14
DATE_REF_FLD = AEDAT
VKORG = 2000
```

**Purpose:** Review billing documents not posted to FI, using change date (AEDAT) for the last 14 days in sales org 2000, for change-based exception review.


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
 LV_DATE_REF_FLD = 'FKDAT'. "Billing date
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
CONVERT_MULTY: KUNRG ALPHA,
               KUNAG ALPHA,
               VBELN ALPHA,
               BP1_CODE ALPHA,
               BP2_CODE ALPHA,
               BP3_CODE ALPHA.
  ""Tanya 14/11/18 :
  CONVERT_SINGLE:  BP1_FUNCT PARVW ,
                   BP2_FUNCT PARVW ,
                   BP3_FUNCT PARVW .
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
DATA : W_DATA LIKE LINE OF T_DATA .
DATA : WA_VBPA TYPE VBPA.
DATA : LV_VBELN TYPE VBELN,
       LV_POSNR TYPE POSNR,
       LV_PARVW TYPE PARVW,
       LV_KUNNR TYPE  KUNNR,
       LV_KUNNR_NAME TYPE  NAME1_GP,
       LV_LIFNR TYPE  LIFNR,
       LV_LIFNR_NAME TYPE  NAME1_GP,
       LV_PERNR TYPE  PERNR_D,
       LV_PERNR_NAME TYPE  NAME1_GP,
       LV_NRART TYPE NRART.
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
       R_ERDAT[] = R_DATUM[]. "Document created
     WHEN 'AEDAT'.
       R_AEDAT[] = R_DATUM[]. "changed on
     WHEN OTHERS.
       R_FKDAT[] = R_DATUM[]. "Billing date
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
      AND  A~FKART IN R_FKART
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
*********************************************************************************
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
  "--- Get BPs
  IF T_DATA[] IS NOT INITIAL.
    "--- Fill R_BP_FUNCT ----
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
    DELETE T_DATA WHERE BP1_CODE NOT IN R_BP1_CODE.
    DELETE T_DATA WHERE BP2_CODE NOT IN R_BP2_CODE.
    DELETE T_DATA WHERE BP3_CODE NOT IN R_BP3_CODE.
   ENDIF.
  ENDIF.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
*Payer desc
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
*Sold-to party desc
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