# Exception Indicator: Incomplete Sales Documents - Status Indication ( SW_10_01_SD_INCP_IND)

## General Overview

This Exception Indicator identifies sales documents with incomplete data based on configured incompletion status groups and control flags, aggregating item-level incompletion indicators from status records before returning matching order headers.

This EI serves as an essential control for sales document data quality by:

- Detecting sales documents whose incompletion status groups indicate missing general, delivery, billing, pricing, picking, packing, or goods-movement data
- Aggregating incompletion control flags from document status records per status group and document
- Filtering results by configurable control parameters for each incompletion category
- Supporting organizational, customer, document-type, and transaction-group scoping for targeted review
- Enriching results with customer and document category descriptions and partner attributes when partner roles are configured

Typical use includes monitoring open orders and quotations with unresolved incompletion, sampling documents before release or billing, and audit support where incomplete master or transactional data must be cleared. Results are intended for exception workflows rather than full document extracts.

The routine pre-selects documents with matching incompletion status groups, derives aggregated control flags per document, applies control and header incompletion filters, enriches descriptive and partner data, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor incomplete sales documents creates multiple risks across order processing, delivery, billing, and customer service:

**Sales and Operations Risks**

- Documents with unresolved general, delivery, or billing incompletions can block or delay downstream processing without structured review
- Missing pricing, picking, or packing data on open documents can cause fulfillment and invoicing errors
- Concentrations of incompletion exceptions by customer or sales organization can signal training or master-data gaps

**Operational Risks**

- Monitoring windows misaligned with document entry cadence can exclude recent incompletions or retain cleared cases
- Status-group scope that is too broad or too narrow can hide actionable documents or create reviewer fatigue
- Control-flag settings that are not tuned can return documents outside the intended incompletion categories

**Control and Audit Risks**

- Weak incompletion monitoring reduces evidence that flagged documents were reviewed before release or billing
- Lack of recurring exception review limits accountability for sales operations follow-up on stalled documents
- Missing customer and organizational context delays escalation of commercially significant cases

## Suggested Resolution

**Immediate Response**

- Review flagged documents for incompletion control flags, header incompletion status, status group, customer, and sales organization
- Confirm with sales or master-data teams which missing data elements must be completed for each document category
- Prioritize high-value customers and documents blocking delivery or billing for immediate follow-up

**System Assessment**

- Validate lookback window and incompletion control settings against document review cadence
- Tune status group, transaction group, and organizational scope so results stay actionable
- Compare exception counts by incompletion type, sales organization, and customer to identify systematic gaps

**Corrective Actions**

- Complete missing document data through standard SD processes where review confirms action is required
- Adjust monitoring scope after cleanup so results reflect truly exceptional incompletion cases
- Document review outcomes and schedule recurring runs before release or billing milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | ANGDT | Quotation valid from | DATS | 8 | 0 | ANGDT_V | DATUM |
| 3 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 4 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 5 | BACKDAYS | BACKDAYS |  | 0 | 0 |  |  |
| 6 | BNDDT | Quotation valid to | DATS | 8 | 0 | BNDDT | DATUM |
| 7 | BP1_CODE | Partner1 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 8 | BP1_FUNCT | Partner1 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 9 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 10 | BP2_CODE | Partner2 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 11 | BP2_FUNCT | Partner2 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 12 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 13 | BP3_CODE | Partner3 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 14 | BP3_FUNCT | Partner3 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 15 | BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 16 | CNT | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 17 | CTRL_UVALL | 'X' - General Incompl. | CHAR | 1 | 0 | UVALL | XFELD |
| 18 | CTRL_UVFAK | 'X' -Billing document Incompl. | CHAR | 1 | 0 | UVFAK | XFELD |
| 19 | CTRL_UVGEK | 'X' - Not assignment Incompl. | CHAR | 1 | 0 | DUMMY | DUMMY |
| 20 | CTRL_UVPAK | 'X' -Pack Incompl. | CHAR | 1 | 0 | UVPAC | XFELD |
| 21 | CTRL_UVPIK | 'X' -Picking/putaway Incompl. | CHAR | 1 | 0 | UVPIC | XFELD |
| 22 | CTRL_UVPRS | 'X' -Pricing Incompl. | CHAR | 1 | 0 | UVPRS | XFELD |
| 23 | CTRL_UVVLK | 'X' - Delivery Incompl. | CHAR | 1 | 0 | UVVLK | XFELD |
| 24 | CTRL_UVWAK | 'X' -Goods mvmt post. Incompl. | CHAR | 1 | 0 | UVVWA | XFELD |
| 25 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 26 | DATE_REF_FLD | CHAR | 30 | 0 | NAME_FELD | FDNAME |  |
| 27 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 28 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 29 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 30 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 31 | GUEBG | Valid-from date | DATS | 8 | 0 | GUEBG | DATUM |
| 32 | GUEEN | Valid-to date | DATS | 8 | 0 | GUEEN | DATUM |
| 33 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 34 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 35 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 36 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 37 | STATG | Status group |  | 0 | 0 |  |  |
| 38 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 39 | TRVOG | CHAR | 1 | 0 | TRVOG | TRVOG |  |
| 40 | UVALL | Header data | CHAR | 1 | 0 | UVALL_UK | STATV |
| 41 | UVALS | Item data | CHAR | 1 | 0 | UVALL_SU | STATV |
| 42 | UVFAK | Header billing data | CHAR | 1 | 0 | UVFAK_UK | STATV |
| 43 | UVFAS | Item billing data... | CHAR | 1 | 0 | UVFAK_SU | STATV |
| 44 | UVPAK | Head.data packaging | CHAR | 1 | 0 | UVPAK_UK | STATV |
| 45 | UVPIK | Head. data picking/putaway | CHAR | 1 | 0 | UVPIK_UK | STATV |
| 46 | UVPRS | Pricing | CHAR | 1 | 0 | UVPRS_UK | STATV |
| 47 | UVVLK | Header delivery data | CHAR | 1 | 0 | UVVLK_UK | STATV |
| 48 | UVVLS | Item delivery data.. | CHAR | 1 | 0 | UVVLS_SU | STATV |
| 49 | UVWAK | Head. data goods mvmt | CHAR | 1 | 0 | UVWAK_UK | STATV |
| 50 | UVWAS | Item data: goods mvmt | CHAR | 1 | 0 | UVWAK_SU | STATV |
| 51 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 52 | VBOBJ | Document object | CHAR | 1 | 0 | VBOBJ | VBOBJ |
| 53 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 54 | VBTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 55 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 56 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 57 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 58 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 59 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 59 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**ANGDT** (Quotation valid from)

Quotation Valid From represents the exact calendar date when the pricing conditions, terms, and delivery commitments in a sales quotation become legally effective for the customer.

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**AUDAT** (Document Date)

Sales document date (order date) used for period-based SD selection.

**BACKDAYS** (BACKDAYS)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BNDDT** (Quotation valid to)

Quotation Valid To represents the exact calendar date until which the pricing conditions, delivery terms, and material commitments defined in a sales quotation remain legally binding for the customer.

**BP1_CODE** (Partner1 - Code)

<mark>Business partner slot 1 code used to identify the linked partner in multi-partner records.</mark>

**BP1_FUNCT** (Partner1 - Function)

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

**BP3_FUNCT** (Partner3 - Function)

<mark>Business partner slot 3 function/role used to classify partner responsibility.</mark>

**BP3_NAME** (Name)

<mark>Business partner slot 3 name/description used for readable partner output.</mark>

**CNT** (Natural Number)

Supports escalation where natural number on CNT signals ownership for follow-up between Basis and functional teams.

**CTRL_UVALL** ('X' - General Incompl.)

Valuable when comparing health before and after a release—hold 'x' - general incompl. on CTRL_UVALL constant while varying other filters.

**CTRL_UVFAK** ('X' -Billing document Incompl.)

Explains why two monitoring passes differ: only the pass with stricter 'x' -billing document incompl. on CTRL_UVFAK surfaces the disputed rows.

**CTRL_UVGEK** ('X' - Not assignment Incompl.)

Helps monitoring stay readable by requiring 'x' - not assignment incompl. (CTRL_UVGEK) to match organizational or technical selectors when set.

**CTRL_UVPAK** ('X' -Pack Incompl.)

When combined with destination discipline, 'x' -pack incompl. on CTRL_UVPAK keeps both breadth and depth of the extract intentional.

**CTRL_UVPIK** ('X' -Picking/putaway Incompl.)

For distributed landscapes, 'x' -picking/putaway incompl. on CTRL_UVPIK often anchors which application server or destination appears in results.

**CTRL_UVPRS** ('X' -Pricing Incompl.)

Ensures reporting respects 'x' -pricing incompl. constraints carried by CTRL_UVPRS.

**CTRL_UVVLK** ('X' - Delivery Incompl.)

Guards against oversized extracts when 'x' - delivery incompl. on CTRL_UVVLK is narrowed together with client, user, or session filters.

**CTRL_UVWAK** ('X' -Goods mvmt post. Incompl.)

Improves readability of exported lists because 'x' -goods mvmt post. incompl. (CTRL_UVWAK) columns stay aligned with the configured filter intent.

**CUST_DESC** (Name)

Customer description/name text used for readable customer-level reporting.

**DATE_REF_FLD** (CHAR)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- GUEBG — Valid-from Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.
- ANGDT — Quotation Valid From represents the exact calendar date when the pricing conditions, terms, and delivery commitments in a sales quotation become legally effective for the customer.

**DATUM** (DATS)

Documents expected operator behavior—dats on DATUM should be set when that dimension is part of the control objective.

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ERZET** (Time)

Entry time used to refine timestamp windows within a selected day.

**GUEBG** (Valid-from date)

Valid-from Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.

**GUEEN** (Valid-to date)

Valid-to Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.

**KUNNR** (Customer)

Customer account is used to scope records to specific customers across SD/FI flows.

**LANGU** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** ('X' - Manage in UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**SPART** (Division)

Division key used for SD product-line segmentation.

**STATG** (Status group)

Stabilizes week-over-week metrics by fixing status group (STATG) while allowing duration thresholds to move.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TRVOG** (CHAR)

Reflects real administration where char on TRVOG is routinely restricted to a single productive client or object family.

**UVALL** (Header data)

General Incompletion Status for Header indicates whether any critical general data fields are missing at the document header level, restricting subsequent processing until the mandatory information is provided.

**UVALS** (Item data)

Total Incompletion Status General aggregates the completion state of all general data fields across both the header and individual items, confirming if the document is entirely complete.

**UVFAK** (Header billing data)

Header Incompletion Status for Billing indicates whether mandatory billing-related data is missing from the document header, preventing the transaction from being invoiced until resolved.

**UVFAS** (Item billing data...)

Total Incompletion Status for Billing aggregates the billing readiness across both header data and individual items, flagging whether any missing financial or tax information is blocking downstream invoice creation.

**UVPAK** (Head.data packaging)

Header Incompletion Status for Packaging indicates whether mandatory packing instructions or container details are missing from the document header, preventing the creation of outbound logistics paperwork.

**UVPIK** (Head. data picking/putaway)

Header Incompletion Status for Picking or Putaway tracks whether critical storage location or warehouse movement data is missing from the document header, halting immediate warehouse fulfillment actions.

**UVPRS** (Pricing)

Document Incompletion Status for Pricing indicates whether essential price conditions, currency codes, or valuation factors are missing or invalid within the document, blocking downstream billing and financial posting.

**UVVLK** (Header delivery data)

Header Incompletion Status for Delivery indicates whether mandatory shipping or logistical information is missing from the document header, preventing the creation of a outbound delivery document.

**UVVLS** (Item delivery data..)

Total Incompletion Status for Delivery aggregates the delivery readiness across both header data and individual line items, checking if missing shipping details are blocking outbound delivery creation.

**UVWAK** (Head. data goods mvmt)

Header Incompletion Status for Goods Movement tracks whether critical data required for the goods issue or goods receipt process-such as accounting or plant indicators-is missing from the document header.

**UVWAS** (Item data: goods mvmt)

Total Incompletion Status for Goods Movement aggregates the goods movement readiness across both header data and individual line items, flagging whether any missing parameters are blocking inventory updates.

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBOBJ** (Document object)

SD Document Category Object classifies the specific business entity or transactional module type-such as a sales order, inquiry, quotation, or delivery-to control the data validation and processing logic applied to the record.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VBTYP_DESC** (Short Descript.)

<mark>SD document category description.</mark>

**VDATU** (Requested deliv.date)

Requested/validity date used for schedule and due-date based filtering.

**VKBUR** (Sales Office)

Sales office key used for organizational SD segmentation.

**VKGRP** (Sales Group)

Sales group key used for team-level SD analytics.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.

### Parameter Relationships

**Lookback window:** When no explicit date range is supplied on individual date fields, **BACKDAYS** builds a lookback window from the current day before documents are read. The same window is applied to created-on, document, and requested delivery date selections when those ranges are not set explicitly.

**Document selection:** **VBELN**, **KUNNR**, **VKORG**, **VTWEG**, **SPART**, **AUART**, **VBTYP**, **TRVOG**, **ERNAM**, and related date fields narrow which sales documents enter the incompletion pre-selection.

**Status group pre-selection:** **STATG** filters incompletion status records joined to the sales document header before control flags are aggregated per document.

**Incompletion aggregation:** Incompletion status records per document are grouped by status group and mapped through status-group configuration to control flags for general, delivery, billing, pricing, picking, packing, and goods-movement incompletions.

**Control filtering:** **CTRL_UVALL**, **CTRL_UVVLK**, **CTRL_UVFAK**, **CTRL_UVPRS**, **CTRL_UVWAK**, **CTRL_UVPIK**, and **CTRL_UVPAK** filter the aggregated control flags retained for each document after aggregation.

**Header incompletion status:** **UVALL**, **UVVLS**, **UVFAS**, **UVVLK**, **UVFAK**, **UVPRS**, **UVWAS**, **UVPAK**, **UVPIK**, and **UVWAK** filter documents by overall header incompletion status from the sales document status table.

**Partner roles:** **BP1_FUNCT** / **BP1_CODE**, **BP2_FUNCT** / **BP2_CODE**, and **BP3_FUNCT** / **BP3_CODE** work together to enrich and filter business partner attributes on each document.

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper and the on-premise path below that call is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code

### Practical Example of Parameter Configuration

**Use Case 1: General header incompletion**

**Purpose:** Review documents with general incompletion indicated on the aggregated control flag in one sales organization.

```
CTRL_UVALL = X
UVALL = A
VKORG = 1000
BACKDAYS = 7
```

**Use Case 2: Billing incompletion focus**

**Purpose:** Monitor documents with billing incompletion control flagged for follow-up before invoicing.

```
CTRL_UVFAK = X
UVFAK = A
VBTYP = C
VKORG = 1000
BACKDAYS = 14
```

**Use Case 3: Delivery incompletion by status group**

**Purpose:** Sample documents in a specific incompletion status group with delivery incompletion control active.

```
STATG = 01
CTRL_UVVLK = X
VKORG = 1000
BACKDAYS = 14
```

**Use Case 4: Sold-to partner with pricing incompletion**

**Purpose:** Review incomplete pricing data for one sold-to partner.

```
BP1_FUNCT = AG
BP1_CODE = 100000
CTRL_UVPRS = X
UVPRS = A
VKORG = 1000
```

**Use Case 5: Transaction group with packing incompletion**

**Purpose:** Flag documents in a transaction group with packing incompletion control for warehouse review.

```
TRVOG = 0
CTRL_UVPAK = X
VTWEG = 10
VKORG = 1000
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_ORD_INCMPL | AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_ORD_INCMPL | ANGDT | Quotation valid from | DATS(8) | ANGDT_V |
| /SKN/S_SW_10_01_ORD_INCMPL | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_INCMPL | AUDAT | Document Date | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_ORD_INCMPL | BNDDT | Quotation valid to | DATS(8) | BNDDT |
| /SKN/S_SW_10_01_ORD_INCMPL | BP1_CODE | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_INCMPL | BP1_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_INCMPL | BP1_NAME | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_INCMPL | BP2_CODE | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_INCMPL | BP2_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_INCMPL | BP2_NAME | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_INCMPL | BP3_CODE | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_INCMPL | BP3_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_INCMPL | BP3_NAME | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_INCMPL | CNT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_01_ORD_INCMPL | CTRL_UVALL | General | CHAR(1) | UVALL |
| /SKN/S_SW_10_01_ORD_INCMPL | CTRL_UVFAK | Billing document | CHAR(1) | UVFAK |
| /SKN/S_SW_10_01_ORD_INCMPL | CTRL_UVGEK | Dummy function in length 1 | CHAR(1) | DUMMY |
| /SKN/S_SW_10_01_ORD_INCMPL | CTRL_UVPAK | Pack | CHAR(1) | UVPAC |
| /SKN/S_SW_10_01_ORD_INCMPL | CTRL_UVPIK | Picking/putaway | CHAR(1) | UVPIC |
| /SKN/S_SW_10_01_ORD_INCMPL | CTRL_UVPRS | Pricing | CHAR(1) | UVPRS |
| /SKN/S_SW_10_01_ORD_INCMPL | CTRL_UVVLK | Delivery | CHAR(1) | UVVLK |
| /SKN/S_SW_10_01_ORD_INCMPL | CTRL_UVWAK | Goods mvmt posting | CHAR(1) | UVVWA |
| /SKN/S_SW_10_01_ORD_INCMPL | CUST_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_INCMPL | ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_ORD_INCMPL | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_ORD_INCMPL | ERZET | Time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_ORD_INCMPL | GUEBG | Valid-from date | DATS(8) | GUEBG |
| /SKN/S_SW_10_01_ORD_INCMPL | GUEEN | Valid-to date | DATS(8) | GUEEN |
| /SKN/S_SW_10_01_ORD_INCMPL | KUNNR | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_INCMPL | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_INCMPL | UVALL | Header data | CHAR(1) | UVALL_UK |
| /SKN/S_SW_10_01_ORD_INCMPL | UVALS | Item data | CHAR(1) | UVALL_SU |
| /SKN/S_SW_10_01_ORD_INCMPL | UVFAK | Header billing data | CHAR(1) | UVFAK_UK |
| /SKN/S_SW_10_01_ORD_INCMPL | UVFAS | Item billing data... | CHAR(1) | UVFAK_SU |
| /SKN/S_SW_10_01_ORD_INCMPL | UVPAK | Head.data packaging | CHAR(1) | UVPAK_UK |
| /SKN/S_SW_10_01_ORD_INCMPL | UVPIK | Head. data picking/putaway | CHAR(1) | UVPIK_UK |
| /SKN/S_SW_10_01_ORD_INCMPL | UVPRS | Pricing | CHAR(1) | UVPRS_UK |
| /SKN/S_SW_10_01_ORD_INCMPL | UVVLK | Header delivery data | CHAR(1) | UVVLK_UK |
| /SKN/S_SW_10_01_ORD_INCMPL | UVVLS | Item delivery data.. | CHAR(1) | UVVLS_SU |
| /SKN/S_SW_10_01_ORD_INCMPL | UVWAK | Head. data goods mvmt | CHAR(1) | UVWAK_UK |
| /SKN/S_SW_10_01_ORD_INCMPL | UVWAS | Item data: goods mvmt | CHAR(1) | UVWAK_SU |
| /SKN/S_SW_10_01_ORD_INCMPL | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_INCMPL | VBOBJ | Document object | CHAR(1) | VBOBJ |
| /SKN/S_SW_10_01_ORD_INCMPL | VBTYP | SD document categ. | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_INCMPL | VBTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_01_ORD_INCMPL | VDATU | Requested deliv.date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_ORD_INCMPL | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_ORD_INCMPL | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_ORD_INCMPL | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_INCMPL | VTWEG | Distribution Channel | CHAR(2) | VTWEG |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ORD_INCMPL .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_INCMPL OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
             LANGU          LANGU,
             BACKDAYS       INT4,
             BP1_FUNCT      PARVW,
             BP2_FUNCT      PARVW,
             BP3_FUNCT      PARVW,
             DATE_REF_FLD   NAME_FELD.
 LV_BACKDAYS = 1.
 LV_LANGU = SY-LANGU.
 """lv_DATE_REF_FLD = 'ERDAT'."Creation date   'AUDAT'. "Document Date (Date Received/Sent)
 SELECT_SINGLE: MANAGE_IN_UTC,
                LANGU,
                BACKDAYS,
                BP1_FUNCT,
                BP2_FUNCT,
                BP3_FUNCT,
                DATE_REF_FLD.
DATA_MULTY: KUNNR        VBAK-KUNNR,
            VBELN        VBAK-VBELN,
            VKORG        VBAK-VKORG,
            VTWEG        VBAK-VTWEG,
            SPART        VBAK-SPART,
            ERDAT        VBAK-ERDAT,
            AUDAT        VBAK-AUDAT,
            AEDAT        VBAK-AEDAT,
            GUEBG        GUEBG, "Valid-from date contract
            GUEEN        GUEEN, "Valid-to date contract
            ANGDT        ANGDT_V, "Valid-from date quatetion
            BNDDT        BNDDT, "Valid-to date quatetion
            AUART        VBAK-AUART,
            ERNAM        VBAK-ERNAM,
            RFBSK        VBRK-RFBSK,
            DATUM        SY-DATUM,
            VBTYP        VBTYP,
            TRVOG        TRVOG,
*            DURATION_M   /SKN/E_SW_DURATION_M,
*            DURATION_H   /SKN/E_SW_DURATION_H,
*            DURATION_D   /SKN/E_SW_DURATION_D,
            VDATU       EDATU_VBAK,
            UVALS       UVVLS_SU,
            UVVLS       UVVLS_SU,
            UVFAS       UVFAK_SU,
            UVALL       UVALL_UK,
            UVVLK       UVVLK_UK,
            UVFAK       UVFAK_UK,
            UVPRS       UVPRS_UK,
            UVWAS       UVWAK_SU,
            UVPAK       UVPAK_UK,
            UVPIK       UVPIK_UK,
            UVWAK       UVWAK_UK,
            CTRL_UVALL     UVALL,
            CTRL_UVVLK     UVVLK,
            CTRL_UVFAK     UVFAK,
            CTRL_UVPRS     UVPRS,
            CTRL_UVWAK     UVVWA,
            CTRL_UVPIK     UVPIC,
            CTRL_UVPAK     UVPAC,
            STATG          STATG,
            VKGRP       VKGRP,
            VKBUR       VKBUR,
            BP1_CODE    KUNNR,
            BP2_CODE    KUNNR,
            BP3_CODE    KUNNR,
            BP_FUNCT    PARVW.
SELECT_MULTY: KUNNR,
              VBELN,
              VKORG ,
              VTWEG ,
              SPART,
              ERDAT,
              AUDAT,
              AEDAT,
              AUART,
              GUEBG,
              GUEEN,
              ANGDT,
              BNDDT,
              ERNAM,
              RFBSK ,
              DATUM,
              VBTYP,
              TRVOG,
*            DURATION_M,
*            DURATION_H ,
*            DURATION_D,
              VDATU,
              UVALS,
              UVVLS,
              UVFAS ,
              UVALL,
              UVVLK ,
              UVFAK ,
              UVPRS,
              UVWAS,
              UVPAK,
              UVPIK,
              UVWAK,
              CTRL_UVALL,
              CTRL_UVVLK,
              CTRL_UVFAK,
              CTRL_UVPRS,
              CTRL_UVWAK,
              CTRL_UVPIK,
              CTRL_UVPAK,
              STATG,
              VKGRP,
              VKBUR,
              BP1_CODE,
              BP2_CODE,
              BP3_CODE.
CONVERT_MULTY: KUNNR ALPHA,
               VBELN ALPHA,
               BP1_CODE ALPHA,
               BP2_CODE ALPHA,
               BP3_CODE ALPHA.
""Tanya 14/11/18 :
  CONVERT_SINGLE:  BP1_FUNCT PARVW ,
                   BP2_FUNCT PARVW,
                   BP3_FUNCT PARVW .
  """ ?????? """"convert_multy_C:  bp_funct PARVW lv_langu lv_sw_dest
  CONVERT_MULTY:  AUART AUART . """Tanya 14/11/18
RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
         R_FLD_VAL FOR DD03P-FIELDNAME .
DATA :   FLD_NAME TYPE FIELDNAME.
DATA : I TYPE I,
       CI(1) TYPE C,
       NFIELDS TYPE I VALUE 3.   "
DATA : BACKDAYS  TYPE I ,
       DATE_FROM LIKE SY-DATUM,
       DATE_TO LIKE SY-DATUM .
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
DATA: LV_VBTYP TYPE VBTYP.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : FLD(60) TYPE C .
*data: lra_range type range of DD03P-FIELDNAME.
FIELD-SYMBOLS:  TYPE ANY ,
               <FS_V> TYPE ANY .
DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
  INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
DATA : END OF SW_STRUCTURE .
DATA: BEGIN OF LS_VBUV,
       VBELN TYPE VBELN_VA,
       STATG TYPE STATG,
      END OF LS_VBUV.
DATA: LT_VBUV LIKE TABLE OF LS_VBUV.
DATA: BEGIN OF LS_VBUV_STAT,
       VBELN TYPE VBELN_VA,
       STATG TYPE STATG,
       CNT   TYPE I,
       UVALL  TYPE UVALL,
       UVVLK  TYPE UVVLK,
       UVFAK  TYPE UVFAK,
       UVPRS  TYPE UVPRS,
       UVWAK  TYPE UVVWA,
       UVPIK  TYPE UVPIC,
       UVPAK  TYPE UVPAC,
       UVGEK  TYPE DUMMY,
      END OF LS_VBUV_STAT.
DATA: LT_VBUV_STAT LIKE TABLE OF LS_VBUV_STAT.
DATA: LV_VBELN_OLD TYPE VBELN_VA,
      LV_STATG_OLD TYPE STATG,
      LV_CNT   TYPE I.
DATA: LS_TVUVS TYPE TVUVS,
      LT_TVUVS LIKE TABLE OF LS_TVUVS.
DATA: BEGIN OF LS_VBAK_STAT,
       VBELN TYPE VBELN_VA,
       CNT   TYPE I,
       UVALL  TYPE UVALL,
       UVVLK  TYPE UVVLK,
       UVFAK  TYPE UVFAK,
       UVPRS  TYPE UVPRS,
       UVWAK  TYPE UVVWA,
       UVPIK  TYPE UVPIC,
       UVPAK  TYPE UVPAC,
       UVGEK  TYPE DUMMY,
      END OF LS_VBAK_STAT.
DATA: LT_VBAK_STAT LIKE TABLE OF LS_VBAK_STAT.
DATA: BEGIN OF LS_STAT,
       CNT   TYPE I,
       UVALL  TYPE UVALL,
       UVVLK  TYPE UVVLK,
       UVFAK  TYPE UVFAK,
       UVPRS  TYPE UVPRS,
       UVWAK  TYPE UVVWA,
       UVPIK  TYPE UVPIC,
       UVPAK  TYPE UVPAC,
       UVGEK  TYPE DUMMY,
      END OF LS_STAT.
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
    CALL FUNCTION '/SKN/FH_SW_10_01_ORD_INCMPL'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   ELSE.
    CALL FUNCTION '/SKN/FC_SW_10_01_ORD_INCMPL'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   ENDIF.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
***********************************************************************************
*define set_BP_range.
*  if lv_BP&1_FUNCT is not initial.
*    clear RS_BP_FUNCT.
*    RS_BP_FUNCT-sign = 'I'.
*    RS_BP_FUNCT-OPTION = 'EQ'.
*    RS_BP_FUNCT-low = lv_BP&1_FUNCT.
*    append RS_BP_FUNCT to R_BP_FUNCT.
*  endif.
*
*end-of-definition.
*define get_BP_attr.
*  clear: t_data-BP&1_NAME.
*  t_data-BP&1_FUNCT = lv_BP&1_FUNCT.
*  CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*    EXPORTING
*      input         = lv_BP&1_FUNCT
*    IMPORTING
*      OUTPUT        = lv_PARVW.
*  t_data-BP&1_FUNCT = lv_PARVW.
*
*if lv_PARVW is not initial.
*  lv_VBELN = t_data-VBELN.
**  select single *
**    from VBPA
**    into wa_VBPA
**    where VBELN = lv_VBELN
**      and POSNR = lv_POSNR
**      and PARVW = lv_PARVW.
*
*  "--- Get BP for Line
*  clear wa_VBPA.
*  lv_POSNR = lv_data_POSNR.  " t_data-POSNR.
*  if lv_POSNR is not initial.
*    read table lt_VBPA into wa_VBPA
*               with key VBELN = lv_VBELN
*                        POSNR = lv_POSNR
*                       PARVW = lv_PARVW
*               BINARY SEARCH.
*  endif.
*  if wa_VBPA is initial.
*    clear  lv_POSNR.
*    read table lt_VBPA into wa_VBPA
*            with key VBELN = lv_VBELN
*                     POSNR = lv_POSNR
*                     PARVW = lv_PARVW
*            BINARY SEARCH.
*  endif.
*  if wa_VBPA is not initial.
*  "-- Get Partner Type
*  clear lv_NRART.
*  select single NRART
*    from TPAR
*    into lv_NRART
*    where PARVW = lv_PARVW.
*  if sy-subrc is initial.
*    CASE lv_NRART.
*      when 'KU'.
*        t_data-BP&1_CODE = wa_VBPA-KUNNR.
*        lv_KUNNR = wa_VBPA-KUNNR.
*        CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
*          EXPORTING
*            KUNNR                = lv_KUNNR
*          IMPORTING
*            CUST_DESC            = lv_KUNNR_NAME
*          EXCEPTIONS
*            WRONG_CUSTOMER       = 1
*          OTHERS               = 2              .
*        IF SY-SUBRC = 0.
*          t_data-BP&1_NAME = lv_KUNNR_NAME.
*        ENDIF.
*      when 'LI'.
*        t_data-BP&1_CODE = wa_VBPA-LIFNR.
*        lv_LIFNR = wa_VBPA-LIFNR.
*        CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
*          EXPORTING
*            lifnr              = lv_LIFNR
*          IMPORTING
*            VENDOR_DESC        = lv_LIFNR_NAME
*          EXCEPTIONS
*            WRONG_VENDOR       = 1
*            OTHERS             = 2.
*        IF sy-subrc = 0.
*          t_data-BP&1_NAME = lv_LIFNR_NAME.
*        ENDIF.
*      when 'PE'.
*        t_data-BP&1_CODE = wa_VBPA-PERNR.
*        lv_PERNR = wa_VBPA-PERNR.
*        CALL FUNCTION '/SKN/F_SW_10_EMP_DESC'
*          EXPORTING
*            pernr                = lv_PERNR
*          IMPORTING
*            EMP_DESC             = lv_PERNR_NAME
*          EXCEPTIONS
*            WRONG_EMPLOYEE       = 1
*            OTHERS               = 2.
*        IF sy-subrc = 0.
*          t_data-BP&1_NAME = lv_PERNR_NAME.
*        ENDIF.
*
*      WHEN OTHERS.
**        t_data-BP&1_CODE = wa_VBPA-KUNNR.
**        lv_KUNNR = wa_VBPA-KUNNR.
**        CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
**          EXPORTING
**            KUNNR                = lv_KUNNR
**          IMPORTING
**            CUST_DESC            = lv_KUNNR_NAME
**          EXCEPTIONS
**            WRONG_CUSTOMER       = 1
**          OTHERS               = 2              .
**        IF SY-SUBRC = 0.
**          t_data-BP&1_NAME = lv_KUNNR_NAME.
**        ENDIF.
*    ENDCASE.
*  endif.
* endif.
*endif.
*end-of-definition.
*   if R_DATUM[] is initial .
*     RS_DATUM-SIGN = 'I' .
*      RS_DATUM-OPTION = 'GE' .
*       DATE_FROM = sy-datum - lv_BACKDAYS .
*       RS_DATUM-LOW = DATE_FROM .
*        APPEND RS_DATUM to R_DATUM.
*   endif.
*
*   "--- Set Reference Date Field
*   DATE_FROM = sy-datum.
*   read table R_DATUM into RS_DATUM index 1.
*   if sy-subrc is initial.
*     DATE_FROM = RS_DATUM-low.
*     DATE_TO = RS_DATUM-high.
*     if DATE_TO < DATE_FROM.
*       DATE_TO = DATE_FROM.
*     endif.
*   endif.
*   "--- Check Quatetion or Contracts types
*   if lv_DATE_REF_FLD is initial.
*     read table R_VBTYP into RS_VBTYP index 1.
*     if sy-tfill = 1. " the single record only
*       if RS_VBTYP-OPTION = 'EQ'.
*         lv_VBTYP = RS_VBTYP-low.
*       endif.
*     endif.
*     if lv_VBTYP = 'G'.
*       lv_DATE_REF_FLD = 'GUEBG'.
*     if lv_VBTYP = 'B'.
*       lv_DATE_REF_FLD = 'ANGDT'.
*     endif.
*     endif.
*   endif.
*   "---
*   CASE lv_DATE_REF_FLD.
*     when 'ERDAT'.
*       R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
*     when 'AEDAT'.
*       R_AEDAT[] = R_DATUM[]. "Changed On
*     When 'AUDAT'.
*       R_AUDAT[] = R_DATUM[]. "Document Date (Date Received/Sent)
**     when 'GUEBG'.
**       R_GUEBG[] = R_DATUM[]. "Valid-from date
**     when 'GUEEN'.
**       R_GUEEN[] = R_DATUM[]. "Valid-to date
*     When 'GUEBG' or 'GUEEN'.
*       RS_GUEBG-SIGN = 'I' .
*        RS_GUEBG-OPTION = 'LE' .
*         RS_GUEBG-LOW = DATE_FROM .
*          APPEND RS_GUEBG to R_GUEBG.
*       RS_GUEEN-SIGN = 'I' .
*        RS_GUEEN-OPTION = 'GE' .
*         RS_GUEEN-LOW = DATE_TO .
*          APPEND RS_GUEEN to R_GUEEN.
*     When 'ANGDT' or 'BNDDT'.
*       RS_ANGDT-SIGN = 'I' .
*        RS_GUEBG-OPTION = 'LE' .
*         RS_ANGDT-LOW = DATE_FROM .
*          APPEND RS_ANGDT to R_ANGDT.
*       RS_BNDDT-SIGN = 'I' .
*        RS_BNDDT-OPTION = 'GE' .
*         RS_BNDDT-LOW = DATE_TO .
*          APPEND RS_BNDDT to R_BNDDT.
*     WHEN OTHERS.
*       R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
*
*   ENDCASE.
  IF R_DATUM[] IS INITIAL.
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  IF R_ERDAT[] IS INITIAL.
    R_ERDAT[] = R_DATUM[].
  ENDIF.
  IF R_AUDAT[] IS INITIAL.
    R_AUDAT[] = R_DATUM[].
  ENDIF.
  IF R_VDATU[] IS INITIAL .
    R_VDATU[] = R_DATUM[].
  ENDIF.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
*--- Pre Selection
  SELECT *  "    VBAK~VBELN VBUV-STATG
     INTO CORRESPONDING FIELDS OF TABLE LT_VBUV
     FROM VBAK
       INNER JOIN VBUV
             ON VBAK~VBELN = VBUV~VBELN
       WHERE VBAK~VKORG IN R_VKORG
         AND VBAK~VTWEG IN R_VTWEG
         AND VBAK~SPART IN R_SPART
         AND VBAK~VKBUR IN R_VKBUR
         AND VBAK~VKGRP IN R_VKGRP
         AND VBAK~TRVOG IN R_TRVOG
         AND VBAK~ERNAM IN R_ERNAM
         AND VBAK~VBTYP IN R_VBTYP
         AND VBAK~SPART IN R_SPART
         AND VBAK~KUNNR IN R_KUNNR
         AND VBAK~AUART IN R_AUART
         AND VBAK~ERDAT IN R_ERDAT
         AND VBAK~AUDAT IN R_AUDAT
         AND VBAK~VDATU IN R_VDATU
         AND VBUV~STATG IN R_STATG.
  REFRESH LT_VBUV_STAT.
  CLEAR: LV_VBELN_OLD, LV_STATG_OLD.
  SORT LT_VBUV BY VBELN STATG.
  CLEAR LV_CNT.
  LOOP AT LT_VBUV INTO LS_VBUV.
    ADD 1 TO LV_CNT.
    "at end of VBELN.
      AT END OF STATG.
        MOVE-CORRESPONDING LS_VBUV TO LS_VBUV_STAT.
        LS_VBUV_STAT-CNT = LV_CNT.
        APPEND LS_VBUV_STAT TO LT_VBUV_STAT.
        CLEAR LV_CNT.
      ENDAT.
    "endat.
  ENDLOOP.
  SELECT * FROM TVUVS
    INTO TABLE LT_TVUVS.
    SORT LT_TVUVS BY STATG.
  LOOP AT LT_VBUV_STAT INTO LS_VBUV_STAT.
    SY_TABIX = SY-TABIX.
    READ TABLE LT_TVUVS INTO LS_TVUVS
                        WITH KEY STATG = LS_VBUV_STAT-STATG
                        BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LS_TVUVS TO LS_VBUV_STAT.
    ENDIF.
    MODIFY LT_VBUV_STAT FROM LS_VBUV_STAT INDEX SY_TABIX .
  ENDLOOP.
  "----
  REFRESH LT_VBAK_STAT.
  SORT LT_VBUV_STAT BY VBELN.
  LOOP AT LT_VBUV_STAT INTO LS_VBUV_STAT.
    AT NEW VBELN.
      CLEAR LS_STAT.
    ENDAT.
    ADD LS_VBUV_STAT-CNT TO LS_STAT-CNT.
    IF LS_VBUV_STAT-UVALL = 'X'.  LS_STAT-UVALL = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVVLK = 'X'.  LS_STAT-UVVLK = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVFAK = 'X'.  LS_STAT-UVFAK = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVPRS = 'X'.  LS_STAT-UVPRS = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVWAK = 'X'.  LS_STAT-UVWAK = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVPIK = 'X'.  LS_STAT-UVPIK = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVPAK = 'X'.  LS_STAT-UVPAK = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVGEK = 'X'.  LS_STAT-UVGEK = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVALL = 'X'.  LS_STAT-UVALL = 'X'. ENDIF.
    IF LS_VBUV_STAT-UVALL = 'X'.  LS_STAT-UVALL = 'X'. ENDIF.
    AT END OF VBELN.
      MOVE-CORRESPONDING LS_VBUV_STAT TO LS_VBAK_STAT.
      MOVE-CORRESPONDING LS_STAT TO LS_VBAK_STAT.
      APPEND LS_VBAK_STAT TO LT_VBAK_STAT.
    ENDAT.
    SY_TABIX = SY-TABIX.
  ENDLOOP.
  SORT LT_VBAK_STAT BY VBELN.
*DELETE FROM lt_VBAK_STAT not selected statuse controls
DELETE LT_VBAK_STAT WHERE UVALL NOT IN R_CTRL_UVALL.
DELETE LT_VBAK_STAT WHERE UVVLK NOT IN R_CTRL_UVVLK.
DELETE LT_VBAK_STAT WHERE UVFAK NOT IN R_CTRL_UVFAK.
DELETE LT_VBAK_STAT WHERE UVPRS NOT IN R_CTRL_UVPRS.
DELETE LT_VBAK_STAT WHERE UVWAK NOT IN R_CTRL_UVWAK.
DELETE LT_VBAK_STAT WHERE UVPIK NOT IN R_CTRL_UVPIK.
DELETE LT_VBAK_STAT WHERE UVPAK NOT IN R_CTRL_UVPAK.
CHECK LT_VBAK_STAT[] IS NOT INITIAL.
  SELECT *
    FROM VBAK AS A
    INNER JOIN VBUK AS K
    ON A~VBELN = K~VBELN
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    FOR ALL ENTRIES IN LT_VBAK_STAT
    WHERE A~VBELN = LT_VBAK_STAT-VBELN
      AND A~VBELN IN R_VBELN
      AND A~KUNNR IN R_KUNNR
      AND A~VKORG IN R_VKORG
      AND A~VKGRP IN R_VKGRP
      AND A~VKBUR IN R_VKBUR
      AND A~VTWEG IN R_VTWEG
      AND A~SPART IN R_SPART
      AND A~ERDAT IN R_ERDAT
      AND A~AUDAT IN R_AUDAT
      AND A~AEDAT IN R_AEDAT
      AND A~AUART IN R_AUART
      AND A~GUEBG IN R_GUEBG
      AND A~GUEEN IN R_GUEEN
      AND A~VBTYP IN R_VBTYP
      AND A~ERNAM IN R_ERNAM
      AND K~UVALS IN R_UVALS
      AND K~UVVLS IN R_UVVLS
      AND K~UVFAS IN R_UVFAS
      AND K~UVALL IN R_UVALL
      AND K~UVVLK IN R_UVVLK
      AND K~UVFAK IN R_UVFAK
      AND K~UVPRS IN R_UVPRS
      AND K~UVWAS IN R_UVWAS
      AND K~UVPAK IN R_UVPAK
      AND K~UVPIK IN R_UVPIK
      AND K~UVWAK IN R_UVWAK
      .
*****************************************************************************
*Changed on duration
*  loop at t_data .
*    sy_tabix = sy-tabix .
*    if not t_data-AEDAT is initial.
*      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*        EXPORTING
*          D_FROM            = t_data-AEDAT
*          T_FROM            = t_data-ERZET
*          D_TO              = sy-datum
*          T_TO              = sy-uzeit
*          TIME_UNIT         = 'M'
*        IMPORTING
*          TIME_DIFF         = TIME_DIFF
*        EXCEPTIONS
*          WRONG_VALUE       = 1
*          OTHERS            = 2    .
*      IF SY-SUBRC = 0.
*        if TIME_DIFF < '999999'.
*          t_data-DURATION_M = TIME_DIFF .
*          t_data-DURATION_H = t_data-DURATION_M / 60.
*          t_data-DURATION_D = t_data-DURATION_H / 24.
*        else.
*          t_data-DURATION_M = '999999'.
*          CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*           EXPORTING
*             D_FROM            = t_data-AEDAT
*             T_FROM            = t_data-ERZET
*             D_TO              = sy-datum
*             T_TO              = sy-uzeit
*             TIME_UNIT         = 'H'
*           IMPORTING
*             TIME_DIFF         = TIME_DIFF
*           EXCEPTIONS
*             WRONG_VALUE       = 1
*             OTHERS            = 2    .
*          IF SY-SUBRC = 0.
*            if TIME_DIFF < '999999'.
*              t_data-DURATION_H = TIME_DIFF .
*              t_data-DURATION_D = t_data-DURATION_H / 24.
*            else.
*              t_data-DURATION_H = '999999'.
*              CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
*                EXPORTING
*                  D_FROM            = t_data-AEDAT
*                  T_FROM            = t_data-ERZET
*                  D_TO              = sy-datum
*                  T_TO              = sy-uzeit
*                  TIME_UNIT         = 'D'
*                IMPORTING
*                  TIME_DIFF         = TIME_DIFF
*                EXCEPTIONS
*                  WRONG_VALUE       = 1
*                  OTHERS            = 2    .
*              IF SY-SUBRC = 0.
*                if TIME_DIFF < '999999'.
*                  t_data-DURATION_D = TIME_DIFF .
*                else.
*                  t_data-DURATION_D = '999999'.
*                endif.
*              endif.
*            endif.
*          endif.
*          modify t_data index sy_tabix.
*        ENDIF.
*      endif.
*    endif.
*  endloop.
*
*  delete t_data where DURATION_M not in R_DURATION_M.
*  delete t_data where DURATION_H not in R_DURATION_H.
*  delete t_data where DURATION_D not in R_DURATION_D.
**********************************************************************
  "--- Get BPs
    "--- Fill R_BP_FUNCT ----
    REFRESH R_BP_FUNCT.
    SET_BP_RANGE 1.
    SET_BP_RANGE 2.
    SET_BP_RANGE 3.
  IF R_BP_FUNCT[] IS NOT INITIAL.
    SELECT * FROM VBPA
      INTO CORRESPONDING FIELDS OF TABLE LT_VBPA
      FOR ALL ENTRIES IN LT_VBAK_STAT
      WHERE VBELN = LT_VBAK_STAT-VBELN
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
    DELETE T_DATA WHERE BP1_CODE NOT IN R_BP1_CODE.
    DELETE T_DATA WHERE BP2_CODE NOT IN R_BP2_CODE.
    DELETE T_DATA WHERE BP3_CODE NOT IN R_BP3_CODE.
  ENDIF.
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
    CALL FUNCTION '/SKN/F_SW_10_DOC_CAT_DESC'
      EXPORTING
        VBTYP                = T_DATA-VBTYP
       LANGU                = LV_LANGU
     IMPORTING
       CAT_DESC             = T_DATA-VBTYP_DESC
     EXCEPTIONS
       WRONG_CODE           = 1
       OTHERS               = 2
              .
    IF SY-SUBRC <> 0.
    ENDIF.
    READ TABLE LT_VBAK_STAT INTO LS_VBAK_STAT
                             WITH KEY VBELN = T_DATA-VBELN.
    IF SY-SUBRC IS INITIAL.
      T_DATA-CTRL_UVALL  = LS_VBAK_STAT-UVALL.
      T_DATA-CTRL_UVVLK  = LS_VBAK_STAT-UVVLK.
      T_DATA-CTRL_UVFAK  = LS_VBAK_STAT-UVFAK.
      T_DATA-CTRL_UVPRS  = LS_VBAK_STAT-UVPRS.
      T_DATA-CTRL_UVWAK  = LS_VBAK_STAT-UVWAK.
      T_DATA-CTRL_UVPIK  = LS_VBAK_STAT-UVPIK.
      T_DATA-CTRL_UVPAK  = LS_VBAK_STAT-UVPAK.
      T_DATA-CTRL_UVGEK  = LS_VBAK_STAT-UVGEK.
      T_DATA-CNT         = LS_VBAK_STAT-CNT.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
