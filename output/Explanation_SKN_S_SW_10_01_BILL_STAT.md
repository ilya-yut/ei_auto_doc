# Exception Indicator: SD Billing Doc.- invoice list only -post. stat. ( SW_10_01_INV_L_POST)

## General Overview

This Exception Indicator identifies SD billing documents on invoice lists whose posting status, billing attributes, and age since a chosen reference date match configured criteria, returning billing header data enriched with partner roles and payer and sold-to descriptions.

This EI serves as an essential control for billing and revenue operations by:

- Enabling detection of invoice-list billing documents with posting or transfer status that requires review
- Supporting monitoring of billing dates, creation dates, and document age for timely follow-up
- Providing payer and sold-to context together with partner role attributes on flagged documents
- Enabling segmentation by sales organization, billing type, and posting status for targeted review
- Supporting recurring sampling before period close or invoice-list reconciliation cycles

Typical use includes invoice-list posting status monitoring, delayed billing transfer review, and partner-based billing exception sampling. Results are intended for exception workflows rather than operational billing list reporting.

The routine reads billing document headers joined to overall billing status, applies date-window and age-based filters, enriches partner and customer description data, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor invoice-list billing document posting status creates multiple risks across billing operations, revenue recognition, and financial close:

**Billing and Revenue Risks**

- Billing documents that remain unposted or blocked can delay revenue recognition and accounts receivable updates
- Invoice-list items with incorrect posting or transfer status can accumulate without structured review
- Undetected concentration by payer, sold-to party, or sales organization can leave billing exposure unmanaged

**Operational Risks**

- Monitoring windows misaligned with billing calendars can exclude recent documents or retain stale cases
- Age thresholds set too broadly can hide actionable documents or create reviewer fatigue
- Partner-role scope that is not tuned can mix irrelevant business partners into the review queue

**Control and Audit Risks**

- Weak posting-status monitoring reduces evidence that invoice-list billing was reviewed before close
- Lack of recurring exception review limits accountability for billing operations follow-up on stalled documents
- Missing payer and sold-to context delays escalation of commercially significant billing cases

## Suggested Resolution

**Immediate Response**

- Review flagged billing documents for posting status, billing type, billing date, and payer or sold-to party
- Confirm with billing operations whether unposted or blocked status is correct or requires correction
- Prioritize high-value or long-aged documents for immediate follow-up

**System Assessment**

- Validate lookback window, reference-date field, and age threshold settings against billing review cadence
- Tune sales organization, billing type, and posting status scope so results stay actionable
- Compare exception counts by posting status, sales organization, and billing type to identify systematic gaps

**Corrective Actions**

- Correct posting or transfer issues through standard SD billing processes where review confirms action is required
- Adjust monitoring scope after cleanup so results reflect truly exceptional posting or age cases
- Document review outcomes and schedule recurring runs before invoice-list close or reconciliation milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
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
| 17 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 18 | DUMMY | Single-Character Indicator | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 19 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 20 | DURATION_D | Duration In Days | NUMC | 6 | 0 | /SKN/E_SW_DURATION_D |  |
| 21 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 22 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 23 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 24 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 25 | FKART | Billing Type | CHAR | 4 | 0 | FKART | FKART |
| 26 | FKDAT | Billing Date | DATS | 8 | 0 | FKDAT | DATUM |
| 27 | FKSTO | Cancelled | CHAR | 1 | 0 | FKSTO | XFELD |
| 28 | FKTYP | Billing category | CHAR | 1 | 0 | FKTYP | FKTYP |
| 29 | FMSTK | Status Funds Management | CHAR | 1 | 0 | FMSTK | STATV |
| 30 | KDGRP | Customer group | CHAR | 2 | 0 | KDGRP | KDGRP |
| 31 | KUNAG | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 32 | KUNRG | Payer | CHAR | 10 | 0 | KUNRG | KUNNR |
| 33 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 34 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 35 | NETWR | Net Value | CURR | 15 | 2 | NETWR | WERTV8 |
| 36 | PAYER_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 37 | RELIK | Inv.list status | CHAR | 1 | 0 | RELIK | STATV |
| 38 | RFBSK | Posting Status | CHAR | 1 | 0 | RFBSK | RFBSK |
| 39 | RRSTA | Rev. determ. status | CHAR | 1 | 0 | RR_STATUS | STATV |
| 40 | SOLDTO_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 41 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 42 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 43 | UVK01 | Header reserves 1 | CHAR | 1 | 0 | UVK01 | STATV |
| 44 | UVK02 | Header reserves 2 | CHAR | 1 | 0 | UVK02 | STATV |
| 45 | UVK03 | Header reserves 3 | CHAR | 1 | 0 | UVK03 | STATV |
| 46 | UVK04 | Header reserves 4 | CHAR | 1 | 0 | UVK04 | STATV |
| 47 | UVK05 | Header reserves 5 | CHAR | 1 | 0 | UVK05 | STATV |
| 48 | UVS01 | Total reserves 1 | CHAR | 1 | 0 | UVS01 | STATV |
| 49 | UVS02 | Total reserves 2 | CHAR | 1 | 0 | UVS02 | STATV |
| 50 | UVS03 | Total reserves 3 | CHAR | 1 | 0 | UVS03 | STATV |
| 51 | UVS04 | Total reserves 4 | CHAR | 1 | 0 | UVS04 | STATV |
| 52 | UVS05 | Total reserves 5 | CHAR | 1 | 0 | UVS05 | STATV |
| 53 | VBELN | Billing Document | CHAR | 10 | 0 | VBELN_VF | VBELN |
| 54 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 55 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 56 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 57 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 57 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BLOCK** (Indicator: Document preselected for archiving)

<mark>Blocking indicator showing whether the record is restricted for posting/processing.</mark>

**BP1_CODE** (Partner1 - Code)

<mark>Business partner slot 1 code used to identify the linked partner in multi-partner records.</mark>

**BP1_FUNCT** (Partner1 -  Function)

<mark>Business partner slot 1 function/role used to classify partner responsibility.</mark>

**BP1_NAME** (Name)

<mark>Business partner slot 1 name/description used for readable partner output.</mark>

**BP2_CODE** (Partner2 - Code)

<mark>Business partner slot 2 code used to identify the linked partner in multi-partner records.</mark>

**BP2_FUNCT** (Partner2 - Function)

<mark>Business partner slot 2 function/role used to classify partner responsibility.</mark>

**BP2_NAME** (Name)

<mark>Business partner slot 2 name/description used for readable partner output.</mark>

**BP3_CODE** (Partner3 - Code)

<mark>Business partner slot 3 code used to identify the linked partner in multi-partner records.</mark>

**BP3_FUNCT** (Partner3 -  Function)

<mark>Business partner slot 3 function/role used to classify partner responsibility.</mark>

**BP3_NAME** (Name)

<mark>Business partner slot 3 name/description used for readable partner output.</mark>

**BUCHK** (Posting Status)

<mark>Posting block or control flag on accounting line items preventing update until blocking reason is cleared.</mark>

**BZIRK** (Sales district)

Sales district key used for SD territory-level segmentation.

**COSTA** (Confirmation status)

Confirmation/status indicator used to distinguish processing completion states.

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- AEDAT — Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.
- FKDAT — Billing date used to align SD billing records with accounting/reporting periods.

**DATUM** (DATS)

Explicit monitoring date range supplied by the online monitor; when empty, the lookback window is built from **BACKDAYS** relative to the current day.

**DUMMY** (Single-Character Indicator)

Placeholder single-character field on the billing status structure; not used for selection in this monitor.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_D** (Duration In Days)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in Days

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ERZET** (Time)

Entry time used to refine timestamp windows within a selected day.

**FKART** (Billing Type)

Billing document type used to segment SD billing scenarios.

**FKDAT** (Billing Date)

Billing date used to align SD billing records with accounting/reporting periods.

**FKSTO** (Cancelled)

Indicator that a billing document is cancelled (billing cancellation document versus normal invoice) in SD billing headers.

**FKTYP** (Billing category)

SD billing category or type classifying the billing document's commercial role (invoice, rebate, etc.).

**FMSTK** (Status Funds Management)

Status of funds management assignment or budgeting state when public-sector FM fields are populated.

**KDGRP** (Customer group)

Customer Group, used to categorize customers for pricing, discounts, and statistical analysis.

**KUNAG** (Sold-to party)

Sold-to party/customer field used for SD partner-role based filtering.

**KUNRG** (Payer)

Payer/customer field used to analyze SD/FI records by billing responsibility.

**LANGU** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**Not in use**
**MANAGE_IN_UTC** ('X' - Manage in UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**NETWR** (Net Value)

Net value amount used for commercial threshold and anomaly checks.

**PAYER_DESC** (Name)

Readable name or description of the payer partner for billing and FI accounts receivable context.

**RELIK** (Inv.list status)

Delivery-related invoice list indicator tying deliveries to collective billing or invoice-list runs.

**RFBSK** (Posting Status)

Billing-to-FI transfer status on the SD billing header indicating whether and how invoicing posted to accounting.

**RRSTA** (Rev. determ. status)

Revenue Determination Status tracks the progress of revenue recognition for a document, indicating whether revenue recognition rules have been applied, partially executed, or fully completed for accounting purposes.

**SOLDTO_DESC** (Name)

Sold-to party description text used for readable customer reporting.

**SPART** (Division)

Division key used for SD product-line segmentation.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**UVK01 - UVK05** (Header reserves 1)

Customer Reserve 1: Header Status acts as a customizable status indicator used to hold the evaluation results of user-defined, custom credit or incompletion check logic programmed via system enhancements.

**UVS01 - UVS05** (Total reserves 1)

Customer Reserve 1: Item Status acts as a customizable status indicator used to hold the evaluation results of user-defined, custom credit or incompletion check logic programmed at the line item level.

**VBELN** (Billing Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.

**WAERK** (Document Currency)

Document currency key used for value analysis in transaction currency.

### Parameter Relationships

**Explicit calendar window versus lookback:** **DATUM** supplies explicit from-and-to calendar bounds for the monitoring pass. When **DATUM** is empty, **BACKDAYS** builds the calendar window relative to the evaluation day before documents are read.

**Reference date mapping:** **DATE_REF_FLD** directs the lookback window to the billing date, created-on date, or changed-on date on each billing document.

**Billing selection:** **VBELN**, **FKART**, **FKTYP**, **VBTYP**, **VKORG**, **VTWEG**, **SPART**, **FKDAT**, **KUNRG**, **KUNAG**, **RFBSK**, **BUCHK**, **RELIK**, **RRSTA**, **BLOCK**, and **FKSTO** combine to define which invoice-list billing documents enter the result set.

**Age filter:** After rows are selected, elapsed time from each row's reference date to the evaluation time is calculated using **DURATION_UNIT** and stored in **DURATION**; rows outside the configured duration range are removed.

**Partner roles:** **BP1_FUNCT** / **BP1_CODE**, **BP2_FUNCT** / **BP2_CODE**, and **BP3_FUNCT** / **BP3_CODE** work together to enrich and filter business partner attributes from billing partner data.

**Customer descriptions:** **PAYER_DESC** is filled for the payer and **SOLDTO_DESC** for the sold-to party after partner filtering completes.

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper and the on-premise path below that call is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code

### Practical Example of Parameter Configuration

**Use Case 1: Unposted invoice-list billings**

**Purpose:** Review invoice-list billing documents that have not yet transferred to accounting in one sales organization.

```
RFBSK = A
FKART = LR
VKORG = 1000
VTWEG = 10
```

**Use Case 2: Billing date lookback**

**Purpose:** Sample billing documents with billing dates within the default one-day lookback window.

```
DATE_REF_FLD = FKDAT
BACKDAYS = 1
VKORG = 1000
FKDAT = 20250101 - 20251231
```

**Use Case 3: Payer partner role**

**Purpose:** Review documents for one payer partner code with posting status requiring follow-up.

```
BP1_FUNCT = RG
BP1_CODE = 100000
RFBSK = B
VKORG = 1000
```

**Use Case 4: Exactly seven full days since billing date**

**Purpose:** Return rows whose billing reference date is exactly 7 full days ago for weekly follow-up.

```
DURATION = 7
DURATION_UNIT = F
DATE_REF_FLD = FKDAT
BACKDAYS = 30
```

**Use Case 5: Accounting transfer status focus**

**Purpose:** Monitor billing documents with a specific accounting transfer status in one division.

```
BUCHK = C
SPART = 01
VKORG = 1000
RFBSK = A
FKART = LR
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
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
