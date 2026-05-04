# Exception Indicator: CO: Open Commitments - SW_10_05_CO_OPN_CMMT

## General Overview

This Exception Indicator monitors open commitments in Controlling that are still tied to purchase orders or purchase requisitions, using commitment items from CO together with purchasing master and item data.

This EI serves as an essential control for purchasing and cost management by:
- Surfacing commitment lines that remain financially open against live purchase documents
- Highlighting vendors, materials, and cost collectors where open quantities or values warrant release or follow-up
- Supporting month-end and project reviews when commitments should have cleared against goods receipt or final invoice
- Giving finance visibility into aging between reference dates and the monitoring run for escalation
- Helping prevent duplicate funding or overstated open commitment positions across company codes and purchasing groups

Typical use includes operational purchasing reviews, controller checkpoints before period close, and audits of long-running purchase commitments. Teams act on the listed cases in ME23N, ME53N, or CO reporting, then update purchase or commitment data as needed.

The routine reads commitment items with purchase order or requisition joins and standard material and company master lookups.


## Problem Description

Failure to monitor open commitments that remain linked to purchasing documents creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Open commitment values may overstate obligations in management reporting when underlying purchase activity has already progressed
- Unreconciled lines complicate period-end matching between purchasing accruals and controlling commitments
- Currency and valuation differences between commitment and purchase views can distort margin analysis if exceptions are not reviewed
- Late discovery of stale commitments delays correction of overstated project or cost-center burden
- Consolidated group views weaken when some entities clear commitments promptly and others do not

**Operational and Control Risks**
- Buyers may proceed with goods receipt or invoicing while controlling commitments still show open quantities
- Split responsibilities between purchasing and controlling teams leave no shared queue of items requiring joint action
- Master data gaps on vendors, materials, or G/L accounts prolong false open positions
- Approval or release status inconsistencies between header and item data hide which lines truly block closure

**Management Visibility and Decision-Making Risks**
- Executives lack a single exception list to prioritize which vendors or plants drive the largest open commitment overhang
- Project managers cannot trust commitment burn-down charts when open items are not validated against current purchase status
- Strategic sourcing decisions skew when open commitment concentration by organization or category is unknown

## Suggested Resolution

**Immediate Response**
- Review each flagged line for vendor, material, value, and document type to judge business severity and next owner
- Open the underlying purchase order or requisition in the standard display transaction to confirm goods receipt, invoice, and deletion indicators
- Verify whether the commitment should still be open given current delivery or service entry progress
- Capture accountable roles and target dates when amounts are material to close or financial statements

**System Assessment**
- Segment results by company code, purchasing organization, document category, and commitment type to see where volume concentrates
- Compare current open counts and values to prior monitoring cycles after major campaigns or year-end activities
- Examine reference date and duration context so teams understand how long each line has exceeded the configured age threshold
- Validate that organizational and material master data on the documents match active purchasing and controlling setup

**Corrective Actions**
- Complete goods movements, service entry, invoicing, or commitment transfers so controlling reflects the true remaining obligation
- Correct master data or document master inconsistencies that keep lines in an unintended open state
- Coordinate with controlling to adjust or reverse commitment postings when purchase transactions were cancelled or replaced
- Document remediation for audit when open positions touched reported capital projects or statutory commitments
- Schedule recurring monitoring after major releases or catalog changes so new item categories are validated early


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | BEKNZ | Dr/Cr indicator | CHAR | 1 | 0 | BEKNZ | BEKNZ |
| 4 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 5 | BSART | Order Type | CHAR | 4 | 0 | BSART | BSART |
| 6 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 7 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 8 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 9 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 13 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 14 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 15 | EREKZ | Final Invoice | CHAR | 1 | 0 | EREKZ | XFELD |
| 16 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 17 | FRGKE | Release indicator | CHAR | 1 | 0 | FRGKE | FRGKE |
| 18 | FRGRL | Subject to Release | CHAR | 1 | 0 | FRGRL | XFELD |
| 19 | FRGZU | Release State | CHAR | 8 | 0 | FRGZU | FRGZU |
| 20 | GESMNG | Quantity/plan | QUAN | 15 | 3 | MENGE1 | MENGV8 |
| 21 | GJAHR | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 22 | GL_ACC_TXT | Short Text | CHAR | 20 | 0 | TXT20_SKAT | TEXT20 |
| 23 | HRKFT | Origin Group | CHAR | 4 | 0 | HRKFT | HRKFT |
| 24 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 25 | KZWOB | Value-based commt | CHAR | 1 | 0 | KZWOB | X |
| 26 | LEDNR | Ledger | CHAR | 2 | 0 | LEDNR | LEDNR |
| 27 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 28 | LOEKZ | Deletion indicator | CHAR | 1 | 0 | LOEKZ | XFELD |
| 29 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 30 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 31 | MEGBTR | Total Quantity | QUAN | 15 | 3 | MEGXXX | MENGV8 |
| 32 | MEINB | Posted unit of meas. | UNIT | 3 | 0 | MEINB | MEINS |
| 33 | MEINH | Display Unit/Measure | UNIT | 3 | 0 | MEINH | MEINS |
| 34 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 35 | OBJNR | Object | CHAR | 18 | 0 | OBJNR | OBJNR |
| 36 | ORGWTH | Plan/LC | CURR | 15 | 2 | WERTH1 | WERTV8 |
| 37 | ORGWTK | Plan/COCurrency | CURR | 15 | 2 | WERTK1 | WERTV8 |
| 38 | ORGWTT | Plan/TC | CURR | 15 | 2 | WERTT1 | WERTV8 |
| 39 | PARGB | Trading part.BA | CHAR | 4 | 0 | PARGB | GSBER |
| 40 | PERIO | Period | NUMC | 3 | 0 | CO_PERIO | PERBL |
| 41 | POPR_TYPE | PO - PR Type | CHAR | 2 | 0 | /SKN/E_SW_POPR_TYPE |  |
| 42 | REFBN | Ref Document Number | CHAR | 10 | 0 | CO_REFBN | BELNR |
| 43 | REFBT | Reference document category | NUMC | 3 | 0 | CO_REFBTYP | BELEGTYP |
| 44 | RFART | Reference doc. type | CHAR | 1 | 0 | CC_RFART | CHAR1 |
| 45 | RFKNT | Acct assgnmnt number | NUMC | 5 | 0 | CC_RFKNT | NUMC05 |
| 46 | RFORG | Reference org. unit | CHAR | 10 | 0 | AWORG | AWORG |
| 47 | RFPOS | Reference item | NUMC | 5 | 0 | CC_RFPOS | NUMC05 |
| 48 | RFSYS | Log. system source | CHAR | 10 | 0 | AWSYS | LOGSYS |
| 49 | RFTRM | Deadline item | NUMC | 5 | 0 | CC_RFTRM | NUMC05 |
| 50 | RFTYP | Reference procedure | CHAR | 5 | 0 | AWTYP | AWTYP |
| 51 | SAKTO | Cost element | CHAR | 10 | 0 | SAKTO | SAKNR |
| 52 | SGTXT | Text | CHAR | 50 | 0 | SGTXT | TEXT50 |
| 53 | TWAER | Transaction Currency | CUKY | 5 | 0 | TWAER | WAERS |
| 54 | UNAME | User Name | CHAR | 12 | 0 | UNAME | UNAME |
| 55 | VBUND | Partner Company ID | CHAR | 6 | 0 | VBUND | RCOMP |
| 56 | VRGNG | Activity | CHAR | 1 | 0 | VRGNG | XFELD |
| 57 | WEPOS | Goods Receipt | CHAR | 1 | 0 | WEPOS | XFELD |
| 58 | WEUNB | GR non-valuated | CHAR | 1 | 0 | WEUNB | XFELD |
| 59 | WGBEZ | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |
| 60 | WHGBTR | Value/LC | CURR | 15 | 2 | WHGXXX | WERTV8 |
| 61 | WKGBTR | Val/COArea Crcy | CURR | 15 | 2 | WKGXXX | WERTV8 |
| 62 | WKURS | Exchange Rate | DEC | 9 | 5 | WKURS | KURSP |
| 63 | WTGBTR | Value TranCurr | CURR | 15 | 2 | WTGXXX | WERTV8 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 63 parameters listed in the Parameters Reference Table when tuning this EI; each influences selection, enrichment, or display for open commitments on purchase orders or requisitions.

**AEDAT** (Changed On)

Limits open commitment lines where "changed on" (AEDAT) still matches the selection interval after COOI joins.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BEKNZ** (Dr/Cr indicator)

After the primary read, rows are dropped unless "dr/cr indicator" on BEKNZ satisfies the declared filter.

**BEKNZ Options:**
- **S** — Debit posting (Soll); typical for commitment lines that increase the open debit side.
- **H** — Credit posting (Haben); typical for commitment lines that increase the open credit side.

**BLDAT** (Document Date)

Narrows results by comparing each line's BLDAT to the selection table for "document date".

**BSART** (Order Type)

Supports period reviews by enforcing "order type" through BSART alongside organizational filters.

**BUDAT** (Posting Date)

When filled, aligns monitoring with master data because "posting date" is evaluated on the persisted commitment line.

**BUKRS** (Company Code)

Keeps exception lists bounded: without a bound on "company code" (BUKRS), unrelated documents would appear.

**BUTXT** (Company Name)

When left open per framework rules, BUTXT does not restrict "company name"; when set, only matching rows stay.

**DATE_REF_FLD** (Date Ref Field)

Selects which CO commitment date column receives the default lookback window built from BACKDAYS before line-level duration filtering runs.

**DATE_REF_FLD Options:**
- **BUDAT** — Posting date in the document; default branch in code when DATE_REF_FLD is BUDAT.
- **AEDAT** — Date on which the purchasing document was created; maps the lookback to created-on.
- **BLDAT** — Document date; used when DATE_REF_FLD is not BUDAT or AEDAT (OTHERS branch assigns the window to BLDAT).

**DURATION** (Duration In Time Units)

Uses "duration in time units" from the joined purchase object so only rows with DURATION inside the configured range remain.

**DURATION_UNIT** (Duration Unit)

Unit used when computing elapsed time between the reference date chosen by DATE_REF_FLD and the run date for each line.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes.
- **D** — Days (default preset in code before selection read).
- **F** — Full days for specific day filtering.

**EKGRP** (Purchasing Group)

Narrows results by comparing each line's EKGRP to the selection table for "purchasing group".

**EKORG** (Purch. Organization)

Supports period reviews by enforcing "purch. organization" through EKORG alongside organizational filters.

**ELIKZ** (Delivery Completed)

When filled, aligns monitoring with master data because "delivery completed" is evaluated on the persisted commitment line.

**EREKZ** (Final Invoice)

Keeps exception lists bounded: without a bound on "final invoice" (EREKZ), unrelated documents would appear.

**ERNAM** (Created By)

When left open per framework rules, ERNAM does not restrict "created by"; when set, only matching rows stay.

**FRGKE** (Release indicator)

Limits open commitment lines where "release indicator" (FRGKE) still matches the selection interval after COOI joins.

**FRGRL** (Subject to Release)

Uses "subject to release" from the joined purchase object so only rows with FRGRL inside the configured range remain.

**FRGZU** (Release State)

After the primary read, rows are dropped unless "release state" on FRGZU satisfies the declared filter.

**GESMNG** (Quantity/plan)

Narrows results by comparing each line's GESMNG to the selection table for "quantity/plan".

**GJAHR** (Fiscal Year)

Supports period reviews by enforcing "fiscal year" through GJAHR alongside organizational filters.

**GL_ACC_TXT** (Short Text)

When filled, aligns monitoring with master data because "short text" is evaluated on the persisted commitment line.

**HRKFT** (Origin Group)

Keeps exception lists bounded: without a bound on "origin group" (HRKFT), unrelated documents would appear.

**KOKRS** (Controlling Area)

When left open per framework rules, KOKRS does not restrict "controlling area"; when set, only matching rows stay.

**KZWOB** (Value-based commt)

Limits open commitment lines where "value-based commt" (KZWOB) still matches the selection interval after COOI joins.

**LEDNR** (Ledger)

Uses "ledger" from the joined purchase object so only rows with LEDNR inside the configured range remain.

**LIFNR** (Vendor)

After the primary read, rows are dropped unless "vendor" on LIFNR satisfies the declared filter.

**LOEKZ** (Deletion indicator)

Narrows results by comparing each line's LOEKZ to the selection table for "deletion indicator".

**MATKL** (Material Group)

Supports period reviews by enforcing "material group" through MATKL alongside organizational filters.

**MATNR** (Material)

When filled, aligns monitoring with master data because "material" is evaluated on the persisted commitment line.

**MEGBTR** (Total Quantity)

Keeps exception lists bounded: without a bound on "total quantity" (MEGBTR), unrelated documents would appear.

**MEINB** (Posted unit of meas.)

When left open per framework rules, MEINB does not restrict "posted unit of meas."; when set, only matching rows stay.

**MEINH** (Display Unit/Measure)

Limits open commitment lines where "display unit/measure" (MEINH) still matches the selection interval after COOI joins.

**NAME1** (Name)

Uses "name" from the joined purchase object so only rows with NAME1 inside the configured range remain.

**OBJNR** (Object)

After the primary read, rows are dropped unless "object" on OBJNR satisfies the declared filter.

**ORGWTH** (Plan/LC)

Narrows results by comparing each line's ORGWTH to the selection table for "plan/lc".

**ORGWTK** (Plan/COCurrency)

Supports period reviews by enforcing "plan/cocurrency" through ORGWTK alongside organizational filters.

**ORGWTT** (Plan/TC)

When filled, aligns monitoring with master data because "plan/tc" is evaluated on the persisted commitment line.

**PARGB** (Trading part.BA)

Keeps exception lists bounded: without a bound on "trading part.ba" (PARGB), unrelated documents would appear.

**PERIO** (Period)

When left open per framework rules, PERIO does not restrict "period"; when set, only matching rows stay.

**POPR_TYPE** (PO - PR Type)

Limits open commitment lines where "po - pr type" (POPR_TYPE) still matches the selection interval after COOI joins.

**REFBN** (Ref Document Number)

Uses "ref document number" from the joined purchase object so only rows with REFBN inside the configured range remain.

**REFBT** (Reference document category)

After the primary read, rows are dropped unless "reference document category" on REFBT satisfies the declared filter.

**RFART** (Reference doc. type)

Narrows results by comparing each line's RFART to the selection table for "reference doc. type".

**RFKNT** (Acct assgnmnt number)

Supports period reviews by enforcing "acct assgnmnt number" through RFKNT alongside organizational filters.

**RFORG** (Reference org. unit)

When filled, aligns monitoring with master data because "reference org. unit" is evaluated on the persisted commitment line.

**RFPOS** (Reference item)

Keeps exception lists bounded: without a bound on "reference item" (RFPOS), unrelated documents would appear.

**RFSYS** (Log. system source)

When left open per framework rules, RFSYS does not restrict "log. system source"; when set, only matching rows stay.

**RFTRM** (Deadline item)

Limits open commitment lines where "deadline item" (RFTRM) still matches the selection interval after COOI joins.

**RFTYP** (Reference procedure)

Uses "reference procedure" from the joined purchase object so only rows with RFTYP inside the configured range remain.

**SAKTO** (Cost element)

After the primary read, rows are dropped unless "cost element" on SAKTO satisfies the declared filter.

**SGTXT** (Text)

Narrows results by comparing each line's SGTXT to the selection table for "text".

**TWAER** (Transaction Currency)

Supports period reviews by enforcing "transaction currency" through TWAER alongside organizational filters.

**UNAME** (User Name)

When filled, aligns monitoring with master data because "user name" is evaluated on the persisted commitment line.

**VBUND** (Partner Company ID)

Keeps exception lists bounded: without a bound on "partner company id" (VBUND), unrelated documents would appear.

**VRGNG** (Activity)

When left open per framework rules, VRGNG does not restrict "activity"; when set, only matching rows stay.

**WEPOS** (Goods Receipt)

Limits open commitment lines where "goods receipt" (WEPOS) still matches the selection interval after COOI joins.

**WEUNB** (GR non-valuated)

Uses "gr non-valuated" from the joined purchase object so only rows with WEUNB inside the configured range remain.

**WGBEZ** (Material Group Desc.)

After the primary read, rows are dropped unless "material group desc." on WGBEZ satisfies the declared filter.

**WHGBTR** (Value/LC)

Narrows results by comparing each line's WHGBTR to the selection table for "value/lc".

**WKGBTR** (Val/COArea Crcy)

Supports period reviews by enforcing "val/coarea crcy" through WKGBTR alongside organizational filters.

**WKURS** (Exchange Rate)

When filled, aligns monitoring with master data because "exchange rate" is evaluated on the persisted commitment line.

**WTGBTR** (Value TranCurr)

Keeps exception lists bounded: without a bound on "value trancurr" (WTGBTR), unrelated documents would appear.


### Parameter Relationships

How parameter combinations work together

**DATE_REF_FLD** selects which date on each commitment line is treated as the reference point for measuring elapsed time. **BACKDAYS** is fallback when explicit dates are not provided: together they build the default monitoring window applied to that reference column before rows are retrieved. When an explicit run date such as the **DATUM** parameter is supplied, it defines the primary calendar anchor the monitor uses first; **BACKDAYS** is fallback when explicit dates are not provided so the window still has a clear lower bound.

**DURATION** and **DURATION_UNIT** work together. **DURATION_UNIT** specifies the unit (for example days) used when computing elapsed time between the reference date (from **DATE_REF_FLD**) and the run date. That pair is an additional filter after date selection: only rows whose computed age fits the configured duration remain. Both date and duration conditions are applied together, so a line must satisfy the chosen reference window and the duration rule.

**POPR_TYPE** switches the data path between purchase-order-based commitment items and requisition-based items, so filters such as plant, vendor, and material should be interpreted in the context of that document family. Organizational parameters such as company code and purchasing group further narrow which open items appear in one monitoring pass.


### Default Values

- **BACKDAYS** - 1 from the preset before the selection read when the caller does not override it.
- **DATE_REF_FLD** - BUDAT from the preset before the selection read when the caller does not override it.
- **DURATION_UNIT** - D from the preset before the selection read when the caller does not override it.
- **DURATION** - initial — when the duration selection range is left empty, the routine does not filter rows out by computed age until a range is supplied.

### Practical Example of Parameter Configuration

**Use Case 1: Purchasing control tower — PO commitments last week**

**Purpose:** Show purchase-order-backed open commitments for one company with posting-date window and a modest age filter.

```
BUKRS = 1000
POPR_TYPE = PO
BACKDAYS = 7
DATE_REF_FLD = BUDAT
DURATION_UNIT = D
```

**Use Case 2: Requisition pipeline — material-heavy slice**

**Purpose:** Review requisition-backed open items for a material group before quarter close.

```
POPR_TYPE = PR
MATKL = 001
EKORG = 1000
DURATION = 30
DURATION_UNIT = D
```

**Use Case 3: Vendor concentration with document date anchor**

**Purpose:** Track a strategic vendor’s open commitments using document date as the reference and a longer lookback.

```
LIFNR = 0000100001
DATE_REF_FLD = BLDAT
BACKDAYS = 90
BUKRS = 2000
DURATION_UNIT = D
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_05_CO_OPN_CMMT | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | BEKNZ | Debit/credit indicator | CHAR(1) | BEKNZ |
| /SKN/S_SW_10_05_CO_OPN_CMMT | BLDAT | Document Date in Document | DATS(8) | BLDAT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | BSART | Order Type (Purchasing) | CHAR(4) | BSART |
| /SKN/S_SW_10_05_CO_OPN_CMMT | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_05_CO_OPN_CMMT | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_05_CO_OPN_CMMT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_05_CO_OPN_CMMT | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_05_CO_OPN_CMMT | ELIKZ | "Delivery Completed" Indicator | CHAR(1) | ELIKZ |
| /SKN/S_SW_10_05_CO_OPN_CMMT | EREKZ | Final Invoice Indicator | CHAR(1) | EREKZ |
| /SKN/S_SW_10_05_CO_OPN_CMMT | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_05_CO_OPN_CMMT | FRGKE | Release Indicator: Purchasing Document | CHAR(1) | FRGKE |
| /SKN/S_SW_10_05_CO_OPN_CMMT | FRGRL | Release Not Yet Completely Effected | CHAR(1) | FRGRL |
| /SKN/S_SW_10_05_CO_OPN_CMMT | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_05_CO_OPN_CMMT | GESMNG | Planned quantity | QUAN(15,3) | MENGE1 |
| /SKN/S_SW_10_05_CO_OPN_CMMT | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /SKN/S_SW_10_05_CO_OPN_CMMT | GL_ACC_TXT | G/L account short text | CHAR(20) | TXT20_SKAT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | HRKFT | Origin Group as Subdivision of Cost Element | CHAR(4) | HRKFT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | KOKRS | Controlling Area | CHAR(4) | KOKRS |
| /SKN/S_SW_10_05_CO_OPN_CMMT | KZWOB | Value-based commitment indicator | CHAR(1) | KZWOB |
| /SKN/S_SW_10_05_CO_OPN_CMMT | LEDNR | Ledger for Controlling objects | CHAR(2) | LEDNR |
| /SKN/S_SW_10_05_CO_OPN_CMMT | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_05_CO_OPN_CMMT | LOEKZ | Asset class marked for deletion | CHAR(1) | LOEKZ |
| /SKN/S_SW_10_05_CO_OPN_CMMT | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_05_CO_OPN_CMMT | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_05_CO_OPN_CMMT | MEGBTR | Total Quantity | QUAN(15,3) | MEGXXX |
| /SKN/S_SW_10_05_CO_OPN_CMMT | MEINB | Posted Unit of Measure | UNIT(3) | MEINB |
| /SKN/S_SW_10_05_CO_OPN_CMMT | MEINH | Unit of Measure for Display | UNIT(3) | MEINH |
| /SKN/S_SW_10_05_CO_OPN_CMMT | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_05_CO_OPN_CMMT | OBJNR | Object number (forecast) | CHAR(18) | OBJNR |
| /SKN/S_SW_10_05_CO_OPN_CMMT | ORGWTH | Planned value in local currency | CURR(15,2) | WERTH1 |
| /SKN/S_SW_10_05_CO_OPN_CMMT | ORGWTK | Planned value in controlling area currency | CURR(15,2) | WERTK1 |
| /SKN/S_SW_10_05_CO_OPN_CMMT | ORGWTT | Planned value in transaction currency | CURR(15,2) | WERTT1 |
| /SKN/S_SW_10_05_CO_OPN_CMMT | PARGB | Trading partner's business area | CHAR(4) | PARGB |
| /SKN/S_SW_10_05_CO_OPN_CMMT | PERIO | Period | NUMC(3) | CO_PERIO |
| /SKN/S_SW_10_05_CO_OPN_CMMT | POPR_TYPE | PO - PR Type | CHAR(2) | /SKN/E_SW_POPR_TYPE |
| /SKN/S_SW_10_05_CO_OPN_CMMT | REFBN | Reference Document Number | CHAR(10) | CO_REFBN |
| /SKN/S_SW_10_05_CO_OPN_CMMT | REFBT | Reference document category (conversion exit; c.f. REFBTYP) | NUMC(3) | CO_REFBTYP |
| /SKN/S_SW_10_05_CO_OPN_CMMT | RFART | Reference document type | CHAR(1) | CC_RFART |
| /SKN/S_SW_10_05_CO_OPN_CMMT | RFKNT | Account assignment number of reference document | NUMC(5) | CC_RFKNT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | RFORG | Reference Organizational Units | CHAR(10) | AWORG |
| /SKN/S_SW_10_05_CO_OPN_CMMT | RFPOS | Item number of reference document | NUMC(5) | CC_RFPOS |
| /SKN/S_SW_10_05_CO_OPN_CMMT | RFSYS | Logical system of source document | CHAR(10) | AWSYS |
| /SKN/S_SW_10_05_CO_OPN_CMMT | RFTRM | Deadline item of reference document | NUMC(5) | CC_RFTRM |
| /SKN/S_SW_10_05_CO_OPN_CMMT | RFTYP | Reference Transaction | CHAR(5) | AWTYP |
| /SKN/S_SW_10_05_CO_OPN_CMMT | SAKTO | Cost element | CHAR(10) | SAKTO |
| /SKN/S_SW_10_05_CO_OPN_CMMT | SGTXT | Item Text | CHAR(50) | SGTXT |
| /SKN/S_SW_10_05_CO_OPN_CMMT | TWAER | Transaction Currency | CUKY(5) | TWAER |
| /SKN/S_SW_10_05_CO_OPN_CMMT | UNAME | User Name | CHAR(12) | UNAME |
| /SKN/S_SW_10_05_CO_OPN_CMMT | VBUND | Company ID | CHAR(6) | VBUND |
| /SKN/S_SW_10_05_CO_OPN_CMMT | VRGNG | Indicator controls: X = activity " " = element | CHAR(1) | VRGNG |
| /SKN/S_SW_10_05_CO_OPN_CMMT | WEPOS | Goods Receipt Indicator | CHAR(1) | WEPOS |
| /SKN/S_SW_10_05_CO_OPN_CMMT | WEUNB | Goods Receipt, Non-Valuated | CHAR(1) | WEUNB |
| /SKN/S_SW_10_05_CO_OPN_CMMT | WGBEZ | Material Group Description | CHAR(20) | WGBEZ |
| /SKN/S_SW_10_05_CO_OPN_CMMT | WHGBTR | Total value in local currency | CURR(15,2) | WHGXXX |
| /SKN/S_SW_10_05_CO_OPN_CMMT | WKGBTR | Total Value in Controlling Area Currency | CURR(15,2) | WKGXXX |
| /SKN/S_SW_10_05_CO_OPN_CMMT | WKURS | Exchange Rate | DEC(9,5) | WKURS |
| /SKN/S_SW_10_05_CO_OPN_CMMT | WTGBTR | Total Value in Transaction Currency | CURR(15,2) | WTGXXX |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_05_CO_OPN_CMMT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_05_CO_OPN_CMMT OPTIONAL
*"----------------------------------------------------------------------
"DATA t_data2 TYPE STANDARD TABLE OF /SKN/S_SW_10_05_CO_OPN_CMMT.
DATA_SINGLE: REFBT NUMC3,
             BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             POPR_TYPE      /SKN/E_SW_POPR_TYPE
             .
DATA_MULTY: REFBN   CHAR10,
            LIFNR   CHAR10,
            SAKTO   CHAR10,
            VBUND   CHAR6,
            BLDAT   DATS,
            BUDAT   DATS,
            BUKRS   CHAR4,
            MATNR   CHAR18,
            MATKL   CHAR9,
            ORGWTH  WERTH1,
            WHGBTR  WHGXXX,
            ORGWTK  WERTK1,
            WKGBTR  WKGXXX,
            ORGWTT  WERTT1,
            WTGBTR  WTGXXX,
            LOEKZ   CHAR1,
            EKORG   CHAR4,
            EKGRP   CHAR3,
            BSART   CHAR4,
            AEDAT   DATS,
            WEPOS   CHAR1,
            WEUNB   CHAR1,
            ELIKZ   CHAR1,
            EREKZ   CHAR1,
            KZWOB   CHAR1,
            FRGKE	  CHAR1,
            FRGZU	  CHAR8,
            FRGRL   CHAR1,
            TWAER   CHAR5,
            DURATION    /SKN/E_SW_DURATION,
            DATUM   SY-DATUM
            .
"backdays/date section
LV_BACKDAYS = 1.
LV_DATE_REF_FLD = 'BUDAT'.
LV_DURATION_UNIT = 'D'.
LV_POPR_TYPE = 'PO'.
SELECT_SINGLE: REFBT,
               BACKDAYS,
               DATE_REF_FLD,
               DURATION_UNIT,
               POPR_TYPE
             .
SELECT_MULTY: REFBN,
              LIFNR,
              SAKTO,
              VBUND,
              BUKRS,
              MATNR,
              MATKL,
              ORGWTH,
              WHGBTR,
              ORGWTK,
              WKGBTR,
              ORGWTT,
              WTGBTR,
              LOEKZ,
              EKORG,
              EKGRP,
              BSART,
              AEDAT,
              WEPOS,
              WEUNB,
              ELIKZ,
              EREKZ,
              KZWOB,
              FRGKE,
              FRGZU,
              FRGRL,
              TWAER,
              DURATION,
              DATUM
              .
FIELD-SYMBOLS:  TYPE ANY ,
               <FS_V> TYPE ANY,
               <FS_DATA> TYPE /SKN/S_SW_10_05_CO_OPN_CMMT .
DATA : SY_TABIX LIKE SY-TABIX ,
       FLD(60) TYPE C ,
       REF_DATE TYPE D.
DATA : BACKDAYS  TYPE I ,
       DATE_FROM LIKE SY-DATUM,
       TIME_DIFF TYPE  INT4  .
IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
ENDIF.
*"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_05_CO_OPN_CMMT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
"--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
  WHEN 'BUDAT'.
    R_BUDAT[] = R_DATUM[]. "Expected debit date
  WHEN 'AEDAT'.
    R_AEDAT[] = R_DATUM[]. "Date on Which Record Was Created
  WHEN OTHERS.
    R_BLDAT[] = R_DATUM[]. "Document date
  ENDCASE.
*--- Retrieve data
 CLEAR IS_ALERT.
 REFRESH T_DATA.
IF LV_POPR_TYPE = 'PO' OR LV_POPR_TYPE = '' .
 SELECT *
   INTO CORRESPONDING FIELDS OF TABLE T_DATA
   FROM COOI AS A
   INNER JOIN EKKO AS B
      ON A~REFBN = B~EBELN
   INNER JOIN EKPO AS C
      ON A~REFBN = C~EBELN
      AND A~RFPOS = C~EBELP
   INNER JOIN T006 AS T
      ON A~MEINH = T~MSEHI
   WHERE A~REFBT = '020'
   AND A~ORGWTT > 0
   AND A~REFBN IN R_REFBN
   AND A~LIFNR IN R_LIFNR
   AND A~SAKTO IN R_SAKTO
   AND A~VBUND IN R_VBUND
   AND A~BUKRS IN R_BUKRS
   AND A~MATNR IN R_MATNR
   AND A~MATKL IN R_MATKL
   AND A~ORGWTH IN R_ORGWTH
   AND A~WHGBTR IN R_WHGBTR
   AND A~ORGWTK IN R_ORGWTK
   AND A~WKGBTR IN R_WKGBTR
   AND A~ORGWTT IN R_ORGWTT
   AND A~WTGBTR IN R_WTGBTR
   AND A~TWAER IN R_TWAER
   AND A~BUDAT IN R_BUDAT
   AND A~BLDAT IN R_BLDAT
   AND A~LOEKZ IN R_LOEKZ
   AND B~EKORG IN R_EKORG
   AND B~EKGRP IN R_EKGRP
   AND B~BSART IN R_BSART
   AND B~AEDAT IN R_AEDAT
   AND B~FRGKE IN R_FRGKE
   AND B~FRGZU IN R_FRGZU
   AND B~FRGRL IN R_FRGRL
   AND C~WEPOS IN R_WEPOS
   AND C~WEUNB IN R_WEUNB
   AND C~ELIKZ IN R_ELIKZ
   AND C~EREKZ IN R_EREKZ
   AND T~KZWOB IN R_KZWOB
   .
ELSEIF LV_POPR_TYPE = 'PR'.
 SELECT *
   INTO CORRESPONDING FIELDS OF TABLE T_DATA
   FROM COOI AS A
   LEFT OUTER JOIN EBAN AS B
      ON A~REFBN = B~BANFN
      AND A~RFPOS = B~BNFPO
   INNER JOIN T006 AS T
      ON A~MEINH = T~MSEHI
   WHERE A~REFBT = '010'
   AND A~ORGWTT > 0
    AND A~REFBN IN R_REFBN
   AND A~LIFNR IN R_LIFNR
   AND A~SAKTO IN R_SAKTO
   AND A~VBUND IN R_VBUND
   AND A~BUKRS IN R_BUKRS
   AND A~MATNR IN R_MATNR
   AND A~MATKL IN R_MATKL
   AND A~ORGWTH IN R_ORGWTH
   AND A~WHGBTR IN R_WHGBTR
   AND A~ORGWTK IN R_ORGWTK
   AND A~WKGBTR IN R_WKGBTR
   AND A~ORGWTT IN R_ORGWTT
   AND A~WTGBTR IN R_WTGBTR
   AND A~LOEKZ IN R_LOEKZ
   AND A~TWAER IN R_TWAER
   AND A~BUDAT IN R_BUDAT
   AND A~BLDAT IN R_BLDAT
   AND T~KZWOB IN R_KZWOB
   .
ENDIF.
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
*       "--- Get  Decriptions
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
*
    SY_TABIX = SY-TABIX.
    IF <FS_DATA>-LIFNR IS NOT INITIAL.
      "--- Get  Vendor Decriptions
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        =     <FS_DATA>-LIFNR" Account Number of Vendor or Creditor
        IMPORTING
          VENDOR_DESC  =     <FS_DATA>-NAME1" Name 1
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2
        .
      IF SY-SUBRC <> 0.
*       message id sy-msgid type sy-msgty number sy-msgno
*                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
    IF <FS_DATA>-SAKTO IS NOT INITIAL AND <FS_DATA>-BUKRS IS NOT INITIAL.
* G/L Account Description
      CALL FUNCTION '/SKN/F_SW_10_SAKTO_DESC'
        EXPORTING
          SPRAS      = SY-LANGU    " Language Key
          BUKRS      =  <FS_DATA>-BUKRS   " Company Code
*          KTOPL      =     " Chart of Accounts
          SAKNR      =     <FS_DATA>-SAKTO" G/L Account Number
        IMPORTING
          ACC_DESC   =     <FS_DATA>-GL_ACC_TXT" G/L account short text
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2
        .
      IF SY-SUBRC <> 0.
*       message id sy-msgid type sy-msgty number sy-msgno
*                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
    IF <FS_DATA>-BUKRS IS NOT INITIAL.
* Material group desc.
      CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
        EXPORTING
          BUKRS          =     <FS_DATA>-BUKRS" Company Code
        IMPORTING
          COMP_CODE_DESC =     <FS_DATA>-BUTXT " Name of Company Code or Company
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2
        .
      IF SY-SUBRC <> 0.
       " Implement suitable error handling here
      ENDIF.
    ENDIF.
    IF <FS_DATA>-MATKL IS NOT INITIAL.
* Material group desc.
      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
        EXPORTING
          MATKL        =     <FS_DATA>-MATKL" Material Group
          LANGU        = SY-LANGU    " Language Key
        IMPORTING
          MATKL_DESC   =     <FS_DATA>-WGBEZ" Material Group Description
*          MATKL_DESC_L =     " Long text describing the material group
        EXCEPTIONS
          WRONG_CODE   = 1
          OTHERS       = 2
        .
      IF SY-SUBRC <> 0.
*       message id sy-msgid type sy-msgty number sy-msgno
*                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
    IF LV_POPR_TYPE = 'PR'.
      <FS_DATA>-POPR_TYPE = 'PR'.
    ELSE.
      <FS_DATA>-POPR_TYPE = 'PO'.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
