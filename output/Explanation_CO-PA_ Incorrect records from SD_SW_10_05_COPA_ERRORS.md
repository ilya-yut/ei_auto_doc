# Exception Indicator: CO-PA: Incorrect records from SD - SW_10_05_COPA_ERRORS

## General Overview

This Exception Indicator surfaces profitability (CO-PA) update errors that SAP logged while processing Sales and Distribution documents, so finance and SD teams can see which billing-related postings failed costing integration and why.

This EI serves as an essential control for revenue recognition and margin governance by:
- Highlighting document-level profitability update failures before they distort period reporting or account assignments
- Giving controllers traceable links between error messages and the underlying SD objects for investigation and correction
- Supporting prioritization of master data or condition issues that block successful COPA updates
- Enabling audit evidence that exceptional costing messages were reviewed during close or ongoing monitoring
- Reducing silent data gaps where billing documents exist but profitability segments could not be updated

Typical use spans month-end close, post-billing reviews, and reactive monitoring after pricing or master data changes. Teams reconcile the listed cases with billing and condition maintenance, then reprocess or correct data as needed.

The routine reads profitability update error data together with sales order header and item attributes from standard SD tables.


## Problem Description

Failure to monitor profitability update errors tied to SD billing processes creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unposted or partially posted profitability segments can misstate contribution margin by company, product, or customer dimension
- Delayed detection of costing integration failures complicates revenue and discount recognition during period close
- Repeated message patterns may signal systemic pricing or valuation problems that distort management reports
- Auditors may question whether management knew of persistent COPA update exceptions during sensitive reporting windows
- Cross-company views become unreliable when some billing documents never reach profitability reporting

**Operational and Control Risks**
- Billing teams may complete customer invoicing while profitability updates remain broken, hiding margin leakage
- Condition or material master inconsistencies can cascade into many error rows without a single monitoring point
- SD and controlling teams lack a shared queue of exceptions, so root-cause work is duplicated or skipped
- High volumes of technical messages overwhelm local inboxes when no consolidated exception view exists
- Reference document mismatches between SD and controlling stay unresolved when nobody tracks the error table

**Management Visibility and Decision-Making Risks**
- Executives lose confidence in profitability dashboards when exceptions are discovered late in the close cycle
- Strategic pricing or rebate decisions lack a reliable profitability trail when updates fail silently
- Concentrations of errors in specific sales organizations or channels go unnoticed without systematic surveillance
- Customer profitability analyses skew toward documents that updated successfully, masking problem accounts

## Suggested Resolution

**Immediate Response**
- Review the surfaced error lines, message text, and referenced SD documents to confirm business impact and urgency
- Open the relevant billing or sales documents in VA03 or VF03 to validate pricing, account assignment, and partner data
- Classify whether each case is a one-off data entry issue or a recurring configuration gap requiring broader correction
- Capture owner, expected fix date, and linkage to any customer or auditor inquiry when material amounts are involved

**System Assessment**
- Compare current exception volume to prior periods to detect spikes after releases, pricing changes, or master data loads
- Segment findings by company code, sales organization, and distribution channel to localize systemic causes
- Examine message class and number patterns with application help to understand SAP’s intended corrective path
- Validate that organizational and material masters used on the documents align with active profitability characteristics
- Confirm transport and customizing consistency between SD pricing procedures and COPA valuation settings

**Corrective Actions**
- Correct master data, conditions, or account assignments, then re-execute profitability updates or follow SAP’s recommended programs for the message at hand
- Engage controlling owners to adjust characteristic derivation or valuation strategy when legitimate business changes drive new combinations
- Document remediation steps and evidence for audit when errors touched posted revenue or statutory reporting
- Schedule recurring monitoring after major pricing campaigns so new combinations are validated before billing peaks
- Brief key users on how to interpret the monitor’s severity presentation so red items receive timely escalation


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 2 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 3 | AWORG | Reference Org Unit | CHAR | 10 | 0 | AWORG | AWORG |
| 4 | AWREF | Reference Document | CHAR | 10 | 0 | AWREF | AWREF |
| 5 | AWSYS | Logical System | CHAR | 10 | 0 | LOGSYSTEM | LOGSYS |
| 6 | AWTYP | Reference Transact. | CHAR | 5 | 0 | AWTYP | AWTYP |
| 7 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 8 | BMENG | Confirmed Quantity | QUAN | 13 | 3 | BMENG | MENG13 |
| 9 | BRGEW | Gross Weight | QUAN | 15 | 3 | BRGEW_15 | MENG15 |
| 10 | BTCI | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 11 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 12 | EDATU | Delivery Date | DATS | 8 | 0 | EDATU | DATUM |
| 13 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 14 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 15 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 16 | ETENR | Schedule Line Number | NUMC | 4 | 0 | ETENR | ETENR |
| 17 | FAKWR | Billing value | CURR | 15 | 2 | FAKWR | WERTV8 |
| 18 | FKART | Billing Type | CHAR | 4 | 0 | FKART | FKART |
| 19 | FKDAT | Billing Date | DATS | 8 | 0 | FKDAT | DATUM |
| 20 | FPLNR | Billing Plan Number | CHAR | 10 | 0 | FPLNR | FPLNR |
| 21 | FPLTR | Item | NUMC | 6 | 0 | FPLTR | FPLTR |
| 22 | GEWEI | Unit of Weight | UNIT | 3 | 0 | GEWEI | MEINS |
| 23 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 24 | HWAER | Local Currency | CUKY | 5 | 0 | HWAER | WAERS |
| 25 | KBMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KBMENG | MENG15 |
| 26 | KINAK | Inactive condition | CHAR | 1 | 0 | KINAK | KINAK |
| 27 | KLMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KLMENG | MENG15 |
| 28 | KNTYP | Condition category | CHAR | 1 | 0 | KNTYP | KNTYP |
| 29 | KSCHL | Condition Type | CHAR | 4 | 0 | KSCHA | KSCHL |
| 30 | KSTAT | Statistical | CHAR | 1 | 0 | KSTAT | XFELD |
| 31 | KUNAG | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 32 | KWERT | Condition value | CURR | 13 | 2 | KWERT | WERTV7 |
| 33 | KWMENG | Order Quantity | QUAN | 15 | 3 | KWMENG | MENG15 |
| 34 | KZZUAB | Debit/Credit ind | CHAR | 1 | 0 | SHKZG | SHKZG |
| 35 | LMENG | Required quantity | QUAN | 13 | 3 | LMENG | MENG13 |
| 36 | LOEKZ | Deleted record | CHAR | 1 | 0 | QGEL | QKZ |
| 37 | LSMENG | Required deliv. qty | QUAN | 15 | 3 | LSMENG | MENG15 |
| 38 | MANDT | Client | CLNT | 3 | 0 | MANDT | MANDT |
| 39 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 40 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 41 | MSGID | Message Class | CHAR | 20 | 0 | SYMSGID | ARBGB |
| 42 | MSGNO | Message Number | NUMC | 3 | 0 | SYMSGNO | SYMSGNO |
| 43 | MSGTY | Message Type | CHAR | 1 | 0 | SYMSGTY | SYCHAR01 |
| 44 | MSGV1 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 45 | MSGV2 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 46 | MSGV3 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 47 | MSGV4 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 48 | NETWR | Net Value | CURR | 15 | 2 | NETWR | WERTV8 |
| 49 | NTGEW | Net Weight | QUAN | 15 | 3 | NTGEW_15 | MENG15 |
| 50 | PAOBJNR | Profitab. Segmt No. | NUMC | 10 | 0 | RKEOBJNR | RKEOBJNR |
| 51 | PERIV | Fiscal Year Variant | CHAR | 2 | 0 | PERIV | PERIV |
| 52 | POSNR | Item (SD) | NUMC | 6 | 0 | POSNR | POSNR |
| 53 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 54 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 55 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 56 | STATE_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 57 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 58 | STUNR | Step number | NUMC | 3 | 0 | STUNR | STUNR |
| 59 | VBELN | SD Document | CHAR | 10 | 0 | VBELN | VBELN |
| 60 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 61 | VDATU | Update date: stats. | DATS | 8 | 0 | MC_VDATUM | DATUM |
| 62 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 63 | VOLEH | Volume Unit | UNIT | 3 | 0 | VOLEH | MEINS |
| 64 | VOLUM | Volume | QUAN | 15 | 3 | VOLUM_15 | MENG15 |
| 65 | VRGNG | Business Transaction | CHAR | 4 | 0 | CO_VORGANG | J_VORGANG |
| 66 | VRKME | Sales Unit | UNIT | 3 | 0 | VRKME | MEINS |
| 67 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 68 | WAERK | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 69 | WERKS | Plant | CHAR | 4 | 0 | WERKS_EXT | WERKS |
| 70 | WMENG | Order quantity | QUAN | 13 | 3 | WMENG | MENG13 |
| 71 | ZAEHK | Counter | NUMC | 2 | 0 | DZAEHK | ZAEHK |
| 72 | ZAEHLER | Counter | NUMC | 2 | 0 | AEERROR | NUM2 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 72 parameters listed in the Parameters Reference Table when tuning this EI; each influences selection, enrichment, or displayed context for profitability update issues originating in SD.

**AUART** (Sales Document Type)

Limits profitability-update error lines to cases where the related sales header attribute "sales document type" (AUART) matches the operator ranges you maintain.

**AUDAT** (Document Date)

Uses "document date" from the joined billing order context so investigators only see errors whose AUDAT values fall inside the declared intervals.

**AWORG** (Reference Org Unit)

After the primary database selection, rows are discarded unless the AWORG side of the profitability error record still satisfies your "reference org unit" filter.

**AWREF** (Reference Document)

Narrows the exception population by comparing each row's AWREF value against the selection table, using the business label "reference document" as the column meaning.

**AWSYS** (Logical System)

Supports month-end reviews by enforcing "logical system" constraints through AWSYS, independent of how message text is formatted later.

**AWTYP** (Reference Transact.)

Aligns the monitor with organizational master data when AWTYP is filled, because "reference transact." is evaluated against the persisted error line and its SD joins.

**BACKDAYS** (Back days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

Backdays is based on ERDAT

**BMENG** (Confirmed Quantity)

When left open according to framework rules, BMENG does not restrict "confirmed quantity"; when restricted, only matching postings stay in the working set.

**BRGEW** (Gross Weight)

Limits profitability-update error lines to cases where the related sales header attribute "gross weight" (BRGEW) matches the operator ranges you maintain.

**BTCI** (Indicator)

Uses "indicator" from the joined billing order context so investigators only see errors whose BTCI values fall inside the declared intervals.

**BUKRS** (Company Code)

After the primary database selection, rows are discarded unless the BUKRS side of the profitability error record still satisfies your "company code" filter.

**EDATU** (Delivery Date)

Narrows the exception population by comparing each row's EDATU value against the selection table, using the business label "delivery date" as the column meaning.

**ERDAT** (Created On)

Supports month-end reviews by enforcing "created on" constraints through ERDAT, independent of how message text is formatted later.

**ERNAM** (Created by)

Aligns the monitor with organizational master data when ERNAM is filled, because "created by" is evaluated against the persisted error line and its SD joins.

**ERZET** (Time)

Keeps long error lists manageable: without a bound on "time" (ERZET), unrelated document categories would remain visible.

**ETENR** (Schedule Line Number)

When left open according to framework rules, ETENR does not restrict "schedule line number"; when restricted, only matching postings stay in the working set.

**FAKWR** (Billing value)

Limits profitability-update error lines to cases where the related sales header attribute "billing value" (FAKWR) matches the operator ranges you maintain.

**FKART** (Billing Type)

Uses "billing type" from the joined billing order context so investigators only see errors whose FKART values fall inside the declared intervals.

**FKDAT** (Billing Date)

After the primary database selection, rows are discarded unless the FKDAT side of the profitability error record still satisfies your "billing date" filter.

**FPLNR** (Billing Plan Number)

Narrows the exception population by comparing each row's FPLNR value against the selection table, using the business label "billing plan number" as the column meaning.

**FPLTR** (Item)

Supports month-end reviews by enforcing "item" constraints through FPLTR, independent of how message text is formatted later.

**GEWEI** (Unit of Weight)

Aligns the monitor with organizational master data when GEWEI is filled, because "unit of weight" is evaluated against the persisted error line and its SD joins.

**GSBER** (Business Area)

Keeps long error lists manageable: without a bound on "business area" (GSBER), unrelated document categories would remain visible.

**HWAER** (Local Currency)

When left open according to framework rules, HWAER does not restrict "local currency"; when restricted, only matching postings stay in the working set.

**KBMENG** (Cumul.confirmed qty)

Limits profitability-update error lines to cases where the related sales header attribute "cumul.confirmed qty" (KBMENG) matches the operator ranges you maintain.

**KINAK** (Inactive condition)

Uses "inactive condition" from the joined billing order context so investigators only see errors whose KINAK values fall inside the declared intervals.

**KLMENG** (Cumul.confirmed qty)

After the primary database selection, rows are discarded unless the KLMENG side of the profitability error record still satisfies your "cumul.confirmed qty" filter.

**KNTYP** (Condition category)

Narrows the exception population by comparing each row's KNTYP value against the selection table, using the business label "condition category" as the column meaning.

**KSCHL** (Condition Type)

Supports month-end reviews by enforcing "condition type" constraints through KSCHL, independent of how message text is formatted later.

**KSTAT** (Statistical)

Aligns the monitor with organizational master data when KSTAT is filled, because "statistical" is evaluated against the persisted error line and its SD joins.

**KUNAG** (Sold-to party)

Keeps long error lists manageable: without a bound on "sold-to party" (KUNAG), unrelated document categories would remain visible.

**KWERT** (Condition value)

When left open according to framework rules, KWERT does not restrict "condition value"; when restricted, only matching postings stay in the working set.

**KWMENG** (Order Quantity)

Limits profitability-update error lines to cases where the related sales header attribute "order quantity" (KWMENG) matches the operator ranges you maintain.

**KZZUAB** (Debit/Credit ind)

Uses "debit/credit ind" from the joined billing order context so investigators only see errors whose KZZUAB values fall inside the declared intervals.

**LMENG** (Required quantity)

After the primary database selection, rows are discarded unless the LMENG side of the profitability error record still satisfies your "required quantity" filter.

**LOEKZ** (Deleted record)

Narrows the exception population by comparing each row's LOEKZ value against the selection table, using the business label "deleted record" as the column meaning.

**LSMENG** (Required deliv. qty)

Supports month-end reviews by enforcing "required deliv. qty" constraints through LSMENG, independent of how message text is formatted later.

**MANDT** (Client)

Aligns the monitor with organizational master data when MANDT is filled, because "client" is evaluated against the persisted error line and its SD joins.

**MATNR** (Material)

Keeps long error lists manageable: without a bound on "material" (MATNR), unrelated document categories would remain visible.

**MEINS** (Base Unit of Measure)

When left open according to framework rules, MEINS does not restrict "base unit of measure"; when restricted, only matching postings stay in the working set.

**MSGID** (Message Class)

Limits profitability-update error lines to cases where the related sales header attribute "message class" (MSGID) matches the operator ranges you maintain.

**MSGNO** (Message Number)

Uses "message number" from the joined billing order context so investigators only see errors whose MSGNO values fall inside the declared intervals.

**MSGTY** (Message Type)

After the primary database selection, rows are discarded unless the MSGTY side of the profitability error record still satisfies your "message type" filter.

**MSGV1 - MSGV4** (Message Variable)

These four placeholders carry substitution values that SAP inserts into the profitability update message text; filtering on any combination helps reproduce the exact wording shown to users during BAPI-style message expansion.

**NETWR** (Net Value)

When left open according to framework rules, NETWR does not restrict "net value"; when restricted, only matching postings stay in the working set.

**NTGEW** (Net Weight)

Limits profitability-update error lines to cases where the related sales header attribute "net weight" (NTGEW) matches the operator ranges you maintain.

**PAOBJNR** (Profitab. Segmt No.)

Uses "profitab. segmt no." from the joined billing order context so investigators only see errors whose PAOBJNR values fall inside the declared intervals.

**PERIV** (Fiscal Year Variant)

After the primary database selection, rows are discarded unless the PERIV side of the profitability error record still satisfies your "fiscal year variant" filter.

**POSNR** (Item (SD))

Narrows the exception population by comparing each row's POSNR value against the selection table, using the business label "item (sd)" as the column meaning.

**PRCTR** (Profit Center)

Supports month-end reviews by enforcing "profit center" constraints through PRCTR, independent of how message text is formatted later.

**SPART** (Division)

Aligns the monitor with organizational master data when SPART is filled, because "division" is evaluated against the persisted error line and its SD joins.

**STATE_COLOR** (State Color)

Keeps long error lists manageable: without a bound on "state color" (STATE_COLOR), unrelated document categories would remain visible.

**STATE_COLOR Options:**
- **G** — Green-style severity token for presentation in the monitor.
- **Y** — Yellow-style severity token for presentation in the monitor.
- **R** — Red-style severity token; surfaced rows use this during message resolution.

**STATE_DESC** (SW Message)

When left open according to framework rules, STATE_DESC does not restrict "sw message"; when restricted, only matching postings stay in the working set.

**STATE_ICON** (State Icon)

Limits profitability-update error lines to cases where the related sales header attribute "state icon" (STATE_ICON) matches the operator ranges you maintain.

**STUNR** (Step number)

Uses "step number" from the joined billing order context so investigators only see errors whose STUNR values fall inside the declared intervals.

**VBELN** (SD Document)

After the primary database selection, rows are discarded unless the VBELN side of the profitability error record still satisfies your "sd document" filter.

**VBTYP** (SD document categ.)

Narrows the exception population by comparing each row's VBTYP value against the selection table, using the business label "sd document categ." as the column meaning.

**VDATU** (Update date: stats.)

Supports month-end reviews by enforcing "update date: stats." constraints through VDATU, independent of how message text is formatted later.

**VKORG** (Sales Organization)

Aligns the monitor with organizational master data when VKORG is filled, because "sales organization" is evaluated against the persisted error line and its SD joins.

**VOLEH** (Volume Unit)

Keeps long error lists manageable: without a bound on "volume unit" (VOLEH), unrelated document categories would remain visible.

**VOLUM** (Volume)

When left open according to framework rules, VOLUM does not restrict "volume"; when restricted, only matching postings stay in the working set.

**VRGNG** (Business Transaction)

Limits profitability-update error lines to cases where the related sales header attribute "business transaction" (VRGNG) matches the operator ranges you maintain.

**VRKME** (Sales Unit)

Uses "sales unit" from the joined billing order context so investigators only see errors whose VRKME values fall inside the declared intervals.

**VTWEG** (Distribution Channel)

After the primary database selection, rows are discarded unless the VTWEG side of the profitability error record still satisfies your "distribution channel" filter.

**WAERK** (Currency)

Narrows the exception population by comparing each row's WAERK value against the selection table, using the business label "currency" as the column meaning.

**WERKS** (Plant)

Supports month-end reviews by enforcing "plant" constraints through WERKS, independent of how message text is formatted later.

**WMENG** (Order quantity)

Aligns the monitor with organizational master data when WMENG is filled, because "order quantity" is evaluated against the persisted error line and its SD joins.

**ZAEHK** (Counter)

Keeps long error lists manageable: without a bound on "counter" (ZAEHK), unrelated document categories would remain visible.

**ZAEHLER** (Counter)

When left open according to framework rules, ZAEHLER does not restrict "counter"; when restricted, only matching postings stay in the working set.


### Parameter Relationships

How parameter combinations work together

Selection parameters act as a conjunctive filter on the profitability update error population after the database read. Organizational parameters such as company code, sales organization, and distribution channel narrow the geographic and commercial scope, while document-type and billing-type parameters restrict which SD categories remain in view. Message-oriented parameters identify specific SAP message classes, types, and numbers so teams can focus on high-severity or known-problem catalogs.

Reference parameters (logical system, reference transaction, document number, and organizational unit of the reference) tie each error line to its upstream business object, which is useful when the same message appears across many unrelated postings. Material, profit center, division, and creator parameters refine the residual set when those attributes are populated on the joined sales item or header. The lookback window expressed through the back-days style parameter defines how far back creation timestamps are considered whenever no explicit date interval is already supplied through the monitor framework, so long-running queues stay bounded without excluding fresh errors.

Output-oriented fields in the structure (weights, quantities, condition values, descriptive state columns) do not relax the selection logic; they enrich each retained row for display. Taken together, broad organizational filters should be applied first, then message-specific filters, then fine-grained master-data attributes, to keep result sets readable while still capturing every material exception.


### Default Values

- **BACKDAYS** - 365 when the monitor does not pass an explicit lookback row; the routine then applies a greater-or-equal filter on the error creation date using that window.

### Practical Example of Parameter Configuration

**Use Case 1: Corporate billing desk — last year of COPA errors**

**Purpose:** Give a central team a bounded workload of profitability update failures for all company codes they support.

```
BACKDAYS = 365
BUKRS = 1000
VKORG = 1000
VTWEG = 10
MSGTY = E
```

**Use Case 2: Distribution channel pilot — high-severity only**

**Purpose:** After enabling a new channel, quickly see whether any billing documents still cannot update profitability.

```
VKORG = 2000
VTWEG = 20
FKART = F2
MSGTY = E
MSGID = V1
```

**Use Case 3: Deep dive on one customer and billing category**

**Purpose:** Support a dispute where finance needs every COPA update error for a sold-to party and a specific billing type.

```
KUNAG = 0000100001
FKART = RE
AWTYP = VBRK
BACKDAYS = 180
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_05_COPA_ERRORS | .INCLUDE |  |  |  |
| /SKN/S_SW_10_05_COPA_ERRORS | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_05_COPA_ERRORS | AUDAT | Document Date (Date Received/Sent) | DATS(8) | AUDAT |
| /SKN/S_SW_10_05_COPA_ERRORS | AWORG | Reference Organizational Units | CHAR(10) | AWORG |
| /SKN/S_SW_10_05_COPA_ERRORS | AWREF | Reference Document Number | CHAR(10) | AWREF |
| /SKN/S_SW_10_05_COPA_ERRORS | AWSYS | Logical System | CHAR(10) | LOGSYSTEM |
| /SKN/S_SW_10_05_COPA_ERRORS | AWTYP | Reference Transaction | CHAR(5) | AWTYP |
| /SKN/S_SW_10_05_COPA_ERRORS | BMENG | Confirmed Quantity | QUAN(13,3) | BMENG |
| /SKN/S_SW_10_05_COPA_ERRORS | BRGEW | Gross weight | QUAN(15,3) | BRGEW_15 |
| /SKN/S_SW_10_05_COPA_ERRORS | BTCI | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_10_05_COPA_ERRORS | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_05_COPA_ERRORS | EDATU | Schedule line date | DATS(8) | EDATU |
| /SKN/S_SW_10_05_COPA_ERRORS | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_05_COPA_ERRORS | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_05_COPA_ERRORS | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_05_COPA_ERRORS | ETENR | Delivery Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_05_COPA_ERRORS | FAKWR | Value to be billed/calc. on date in billing/invoice plan | CURR(15,2) | FAKWR |
| /SKN/S_SW_10_05_COPA_ERRORS | FKART | Billing Type | CHAR(4) | FKART |
| /SKN/S_SW_10_05_COPA_ERRORS | FKDAT | Billing date for billing index and printout | DATS(8) | FKDAT |
| /SKN/S_SW_10_05_COPA_ERRORS | FPLNR | Billing plan number / invoicing plan number | CHAR(10) | FPLNR |
| /SKN/S_SW_10_05_COPA_ERRORS | FPLTR | Item for billing plan/invoice plan/payment cards | NUMC(6) | FPLTR |
| /SKN/S_SW_10_05_COPA_ERRORS | GEWEI | Weight Unit | UNIT(3) | GEWEI |
| /SKN/S_SW_10_05_COPA_ERRORS | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_05_COPA_ERRORS | HWAER | Local Currency | CUKY(5) | HWAER |
| /SKN/S_SW_10_05_COPA_ERRORS | KBMENG | Cumulative confirmed quantity in sales unit | QUAN(15,3) | KBMENG |
| /SKN/S_SW_10_05_COPA_ERRORS | KINAK | Condition is inactive | CHAR(1) | KINAK |
| /SKN/S_SW_10_05_COPA_ERRORS | KLMENG | Cumulative confirmed quantity in base unit | QUAN(15,3) | KLMENG |
| /SKN/S_SW_10_05_COPA_ERRORS | KNTYP | Condition category (examples: tax, freight, price, cost) | CHAR(1) | KNTYP |
| /SKN/S_SW_10_05_COPA_ERRORS | KSCHL | Condition type | CHAR(4) | KSCHA |
| /SKN/S_SW_10_05_COPA_ERRORS | KSTAT | Condition is used for statistics | CHAR(1) | KSTAT |
| /SKN/S_SW_10_05_COPA_ERRORS | KUNAG | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_05_COPA_ERRORS | KWERT | Condition value | CURR(13,2) | KWERT |
| /SKN/S_SW_10_05_COPA_ERRORS | KWMENG | Cumulative Order Quantity in Sales Units | QUAN(15,3) | KWMENG |
| /SKN/S_SW_10_05_COPA_ERRORS | KZZUAB | Debit/Credit Indicator | CHAR(1) | SHKZG |
| /SKN/S_SW_10_05_COPA_ERRORS | LMENG | Required quantity for mat.management in stockkeeping units | QUAN(13,3) | LMENG |
| /SKN/S_SW_10_05_COPA_ERRORS | LOEKZ | Data Record Was Deleted | CHAR(1) | QGEL |
| /SKN/S_SW_10_05_COPA_ERRORS | LSMENG | Cumulative required delivery qty (all dlv-relev.sched.lines) | QUAN(15,3) | LSMENG |
| /SKN/S_SW_10_05_COPA_ERRORS | MANDT | Client | CLNT(3) | MANDT |
| /SKN/S_SW_10_05_COPA_ERRORS | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_05_COPA_ERRORS | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_05_COPA_ERRORS | MSGID | Message Class | CHAR(20) | SYMSGID |
| /SKN/S_SW_10_05_COPA_ERRORS | MSGNO | Message Number | NUMC(3) | SYMSGNO |
| /SKN/S_SW_10_05_COPA_ERRORS | MSGTY | Message Type | CHAR(1) | SYMSGTY |
| /SKN/S_SW_10_05_COPA_ERRORS | MSGV1 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_10_05_COPA_ERRORS | MSGV2 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_10_05_COPA_ERRORS | MSGV3 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_10_05_COPA_ERRORS | MSGV4 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_10_05_COPA_ERRORS | NETWR | Net Value in Document Currency | CURR(15,2) | NETWR |
| /SKN/S_SW_10_05_COPA_ERRORS | NTGEW | Net weight | QUAN(15,3) | NTGEW_15 |
| /SKN/S_SW_10_05_COPA_ERRORS | PAOBJNR | Profitability Segment Number (CO-PA) | NUMC(10) | RKEOBJNR |
| /SKN/S_SW_10_05_COPA_ERRORS | PERIV | Fiscal Year Variant | CHAR(2) | PERIV |
| /SKN/S_SW_10_05_COPA_ERRORS | POSNR | Item number of the SD document | NUMC(6) | POSNR |
| /SKN/S_SW_10_05_COPA_ERRORS | PRCTR | Profit Center | CHAR(10) | PRCTR |
| /SKN/S_SW_10_05_COPA_ERRORS | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_05_COPA_ERRORS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_10_05_COPA_ERRORS | STATE_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_10_05_COPA_ERRORS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_10_05_COPA_ERRORS | STUNR | Step number | NUMC(3) | STUNR |
| /SKN/S_SW_10_05_COPA_ERRORS | VBELN | Sales and Distribution Document Number | CHAR(10) | VBELN |
| /SKN/S_SW_10_05_COPA_ERRORS | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_05_COPA_ERRORS | VDATU | Date of update for statistics updating | DATS(8) | MC_VDATUM |
| /SKN/S_SW_10_05_COPA_ERRORS | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_05_COPA_ERRORS | VOLEH | Volume unit | UNIT(3) | VOLEH |
| /SKN/S_SW_10_05_COPA_ERRORS | VOLUM | Volume | QUAN(15,3) | VOLUM_15 |
| /SKN/S_SW_10_05_COPA_ERRORS | VRGNG | CO Business Transaction | CHAR(4) | CO_VORGANG |
| /SKN/S_SW_10_05_COPA_ERRORS | VRKME | Sales unit | UNIT(3) | VRKME |
| /SKN/S_SW_10_05_COPA_ERRORS | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_05_COPA_ERRORS | WAERK | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_05_COPA_ERRORS | WERKS | Plant (Own or External) | CHAR(4) | WERKS_EXT |
| /SKN/S_SW_10_05_COPA_ERRORS | WMENG | Order quantity in sales units | QUAN(13,3) | WMENG |
| /SKN/S_SW_10_05_COPA_ERRORS | ZAEHK | Condition counter | NUMC(2) | DZAEHK |
| /SKN/S_SW_10_05_COPA_ERRORS | ZAEHLER | Counter for errors in SD sales orders | NUMC(2) | AEERROR |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_05_COPA_ERRORS .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_05_COPA_ERRORS OPTIONAL
*"--------------------------------------------------------------------
DATA_MULTY: AUART        VBAK-AUART,
            MATNR        MATNR,
            PRCTR        PRCTR,
            SPART        SPART,
            ERNAM        ERNAM,
            VBTYP        VBTYP,
            BUKRS        BUKRS,
            VKORG        VKORG ,
            VTWEG        VTWEG,
            MSGID        SYMSGID,
            MSGTY        SYMSGTY,
            MSGNO        SYMSGNO,
            FKART        FKART,
            KUNAG        KUNAG ,
            AWTYP        AWTYP,
            AWREF        AWREF,
            AWORG        AWORG,
            VRGNG        CO_VORGANG,
            STATE_COLOR   /SKN/E_SW_STATE_COLOR,  " G/Y/R
            DATUM         SYDATUM . " Paased by SW Online Monitor  .
SELECT_MULTY: AUART,
              MATNR,
              PRCTR,
              SPART,
              ERNAM,
              VBTYP,
              BUKRS,
              VKORG,
              VTWEG,
              MSGID,
              MSGTY,
              MSGNO,
              FKART,
              KUNAG,
              AWTYP,
              AWREF,
              AWORG,
              VRGNG,
              STATE_COLOR,
              DATUM.
CONVERT_MULTY: AUART AUART.
CONVERT_MULTY: MATNR MATN1.
CONVERT_MULTY: PRCTR ALPHA.
CONVERT_MULTY: KUNAG ALPHA.
CONVERT_MULTY: AWREF ALPHA.
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA : DATE_FROM TYPE D,
         BACKDAYS  TYPE I.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : LS_DATA LIKE LINE OF T_DATA.
  DATA: LS_BAPIRET2 TYPE BAPIRET2.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_05_COPA_ERRORS'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
  IF R_DATUM[] IS INITIAL .
    LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      BACKDAYS = T_SELECT-LOW .
      DATE_FROM = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
      EXIT.
    ENDLOOP.
    IF R_DATUM[] IS INITIAL .  " Set default value
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
      BACKDAYS = '365' .  "--- Default
      DATE_FROM = SY-DATUM - BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
    ENDIF .
  ENDIF.
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert Data           "
  "-----------------------------------------------
*  SELECT * FROM CEERROR INTO CORRESPONDING FIELDS OF TABLE T_DATA
*    WHERE
*    VBTYP IN R_VBTYP AND
*    BUKRS IN R_BUKRS AND
*    VKORG IN R_VKORG AND
*    VTWEG IN R_VTWEG AND
*    SPART IN R_SPART AND
*    PRCTR IN R_PRCTR AND
*    ERNAM IN R_ERNAM AND
*    MSGID IN R_MSGID AND
*    MSGTY IN R_MSGTY AND
*    MSGNO IN R_MSGNO AND
*    FKART IN R_FKART AND
*    KUNAG IN R_KUNAG AND
*    MATNR IN R_MATNR AND
*    AWTYP IN R_AWTYP AND
*    AWREF IN R_AWREF AND
*    AWORG IN R_AWORG AND
*    VRGNG IN R_VRGNG AND
*    ERDAT IN R_DATUM
*                    .
    SELECT *
    FROM CEERROR AS C
*    inner join VBAK as a
*    on c~VBELN = a~VBELN
    LEFT  OUTER JOIN VBAK AS A
      ON C~VBELN = A~VBELN
    LEFT OUTER JOIN VBAP AS B
      ON C~VBELN = B~VBELN AND
      C~POSNR = B~POSNR
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE
*      a~AUART IN R_AUART AND
      C~VBTYP IN R_VBTYP AND
      C~BUKRS IN R_BUKRS AND
      C~VKORG IN R_VKORG AND
      C~VTWEG IN R_VTWEG AND
*      c~SPART IN R_SPART AND
*      c~PRCTR IN R_PRCTR AND
*      c~ERNAM IN R_ERNAM AND
      C~MSGID IN R_MSGID AND
      C~MSGTY IN R_MSGTY AND
      C~MSGNO IN R_MSGNO AND
      C~FKART IN R_FKART AND
      C~KUNAG IN R_KUNAG AND
*      c~MATNR IN R_MATNR AND
      C~AWTYP IN R_AWTYP AND
      C~AWREF IN R_AWREF AND
      C~AWORG IN R_AWORG AND
      C~VRGNG IN R_VRGNG AND
      C~ERDAT IN R_DATUM
                    .
DELETE T_DATA WHERE AUART  NOT IN R_AUART .
DELETE T_DATA WHERE MATNR  NOT IN R_MATNR .
DELETE T_DATA WHERE PRCTR  NOT IN R_PRCTR .
DELETE T_DATA WHERE SPART  NOT IN R_SPART .
DELETE T_DATA WHERE ERNAM  NOT IN R_ERNAM .
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        TYPE       = T_DATA-MSGTY
        CL         = T_DATA-MSGID
        NUMBER     = T_DATA-MSGNO
        PAR1       = T_DATA-MSGV1
        PAR2       = T_DATA-MSGV2
        PAR3       = T_DATA-MSGV3
        PAR4       = T_DATA-MSGV4
*       LOG_NO     = ' '
*       LOG_MSG_NO = ' '
*       PARAMETER  = ' '
*       ROW        = 0
*       FIELD      = ' '
      IMPORTING
        RETURN     = LS_BAPIRET2.
    T_DATA-STATE_DESC = LS_BAPIRET2-MESSAGE.
    T_DATA-STATE_COLOR = 'R' .
    CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
      EXPORTING
        STATE_COLOR = T_DATA-STATE_COLOR
      IMPORTING
        STATE_ICON  = T_DATA-STATE_ICON.
    MODIFY T_DATA INDEX SY_TABIX .
  ENDLOOP.
*
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
