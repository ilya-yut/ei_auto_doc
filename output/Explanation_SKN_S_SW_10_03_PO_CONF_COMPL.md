# Exception Indicator: Waiting for Complete PO Confirmation ( SW_10_03_PO_CONF_COM)

## General Overview

This Exception Indicator identifies purchase orders whose release-indicator change history shows they are waiting for complete purchase order confirmation, combining change-document evidence on the order header with requisition and item context so buyers can follow up on orders that remain in an incomplete confirmation state.

This EI serves as an essential control for procurement confirmation governance by:

- Enabling detection of purchase orders where release-indicator changes indicate confirmation is still outstanding
- Supporting follow-up on orders linked to purchase requisitions before downstream receipt or invoice activity proceeds on unconfirmed commitments
- Providing visibility into the latest release-indicator change per order, including prior and new values and who posted the change
- Enabling age-based prioritization when confirmation cases remain open after a chosen reference date
- Supporting audit sampling of confirmation backlog by company, vendor, purchasing organization, and release strategy

Typical use includes buyer escalation on stalled confirmations, workflow health checks after release strategy changes, and periodic control samples before close. Results are intended for exception workflows rather than operational MM list reporting.

The routine reads purchase order header and item data joined to change documents and requisition information, retains the most recent relevant release-indicator change per order when configured, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor purchase orders waiting for complete confirmation creates multiple risks across procurement control, operational throughput, and compliance:

**Procurement and Confirmation Risks**

- Orders that remain incompletely confirmed can delay authorized sourcing, goods receipt, and invoice matching when buyers assume release activity finished the case
- Release-indicator changes without completed confirmation can leave commitment visible while downstream processing is not yet safe to proceed
- Undetected confirmation backlog across vendors or purchasing groups can concentrate risk on critical suppliers and high-value lines

**Operational Risks**

- Change-history scope that is too broad or too narrow can hide actionable confirmation cases or overload reviewers with historical noise
- Lookback and age settings misaligned with confirmation cadence can exclude recent cases or retain stale rows no longer relevant
- Release-indicator and processing-state filters that are not tuned can mix closed or irrelevant orders into the confirmation queue

**Control and Audit Risks**

- Weak monitoring reduces evidence that confirmation backlog was reviewed before period close or vendor escalation
- Lack of recurring exception review weakens accountability for buyer follow-up after release-indicator changes
- Missing age-based prioritization limits escalation of long-waiting confirmation cases tied to the same purchase order

## Suggested Resolution

**Immediate Response**

- Review flagged purchase orders for vendor, release group, strategy, release indicator, and change history context
- Contact the responsible buyer or process owner to confirm whether confirmation action is pending or overdue
- Prioritize high-value, critical-material, or long-waiting orders for confirmation completion through standard workflows

**System Assessment**

- Validate lookback window and reference-date choice against how the team reviews confirmation turnaround
- Tune release-indicator, organizational, and requisition scope so results stay actionable for buyers
- Compare exception counts by purchasing group, document type, and vendor to find systematic confirmation gaps

**Corrective Actions**

- Complete pending confirmations or correct order status through standard MM processes where data supports closure
- Adjust monitoring scope after cleanup so results reflect truly open confirmation cases
- Document review outcomes, brief stakeholders on recurring patterns, and schedule recurring runs before close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | AFNAM | Requisitioner | CHAR | 12 | 0 | AFNAM | AFNAM |
| 3 | ANLN1 | Asset | CHAR | 12 | 0 | ANLN1 | ANLN1 |
| 4 | ANLN2 | Subnumber | CHAR | 4 | 0 | ANLN2 | ANLN2 |
| 5 | AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 6 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 7 | BADAT | Requisition Date | DATS | 8 | 0 | BADAT | DATUM |
| 8 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 9 | BATXT | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 10 | BATXT_EBAN | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 11 | BATXT_EKKO | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 12 | BEDAT | Purchase Order Date | DATS | 8 | 0 | ETBDT | DATUM |
| 13 | BNFPO | Item of Requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 14 | BPUMN | Quantity Conversion | DEC | 5 | 0 | BPUMN | UMBSN |
| 15 | BPUMZ | Quantity Conversion | DEC | 5 | 0 | BPUMZ | UMBSZ |
| 16 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 17 | BSART_EBAN | Document Type | CHAR | 4 | 0 | BBSRT | BSART |
| 18 | BSART_EKKO | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 19 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | BSTYP | BSTYP |
| 20 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 21 | BSTYP_EBAN | Purch. Doc. Category | CHAR | 1 | 0 | BSTYP | BSTYP |
| 22 | BSTYP_EBAN_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 23 | BSTYP_EKKO | Purch. Doc. Category | CHAR | 1 | 0 | BSTYP | BSTYP |
| 24 | BSTYP_EKKO_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 25 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 26 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 27 | BWTTY | Valuation Category | CHAR | 1 | 0 | BWTTY_D | BWTTY |
| 28 | CHANGENR | Document number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 29 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 30 | DATE_REF_FIELD | Date ref. field |  | 0 | 0 |  |  |
| 31 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 32 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 33 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 34 | EBELP | Item | NUMC | 5 | 0 | EBELP | EBELP |
| 35 | EINDT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 36 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 37 | EKNAM | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 38 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 39 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 40 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 41 | ERDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 42 | EREKZ | Final Invoice | CHAR | 1 | 0 | EREKZ | XFELD |
| 43 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 44 | ERNAM_EBAN | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 45 | ERNAM_EKKO | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 46 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 47 | ETENR | Schedule Line Number | NUMC | 4 | 0 | ETENR | ETENR |
| 48 | FIELDNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 49 | FIPOS | Commitment Item | CHAR | 14 | 0 | FIPOS | FIPOS |
| 50 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 51 | FRGKE | Release indicator | CHAR | 1 | 0 | FRGKE | FRGKE |
| 52 | FRGKZ | Release indicator | CHAR | 1 | 0 | FRGKZ | FRGKZ |
| 53 | FRGSX | Release Strategy | CHAR | 2 | 0 | FRGSX | FRGSX |
| 54 | GJAHR | Material Doc. Year | NUMC | 4 | 0 | MJAHR | GJAHR |
| 55 | GL_ACC_TXT | G/L Acct Long Text | CHAR | 50 | 0 | TXT50_SKAT | TEXT50 |
| 56 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 57 | KNTTP | Acct Assignment Cat. | CHAR | 1 | 0 | KNTTP | KNTTP |
| 58 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 59 | KOSTL | Cost Center | CHAR | 10 | 0 | KOSTL | KOSTL |
| 60 | KOSTL_DESC | Description | CHAR | 40 | 0 | KLTXT | TEXT40 |
| 61 | KZFAE | Changeabil. | CHAR | 1 | 0 | KZFAE | KZFAE |
| 62 | KZFRE | Released | CHAR | 1 | 0 | KZFRE | XFELD |
| 63 | LFDAT | Delivery Date | DATS | 8 | 0 | EINDT | DATUM |
| 64 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 65 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 66 | MAKTX | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 67 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 68 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 69 | MEINS | Order Unit | UNIT | 3 | 0 | BSTME | MEINS |
| 70 | MENGE | Scheduled Quantity | QUAN | 13 | 3 | ETMEN | MENGE |
| 71 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 72 | NAME_FIRST_EBAN | First name | CHAR | 40 | 0 | NAME_FIRST | TEXT40 |
| 73 | NAME_FIRST_EKKO | First name | CHAR | 40 | 0 | NAME_FIRST | TEXT40 |
| 74 | NAME_FIRST_USER | First name | CHAR | 40 | 0 | NAME_FIRST | TEXT40 |
| 75 | NAME_LAST_EBAN | Last name | CHAR | 40 | 0 | NAME_LAST | TEXT40 |
| 76 | NAME_LAST_EKKO | Last name | CHAR | 40 | 0 | NAME_LAST | TEXT40 |
| 77 | NAME_LAST_USER | Last name | CHAR | 40 | 0 | NAME_LAST | TEXT40 |
| 78 | NAME_TEXT_EBAN | Complete name | CHAR | 80 | 0 | NAME_TEXT | TEXT80 |
| 79 | NAME_TEXT_EKKO | Complete name | CHAR | 80 | 0 | NAME_TEXT | TEXT80 |
| 80 | NAME_TEXT_USER | Complete name | CHAR | 80 | 0 | NAME_TEXT | TEXT80 |
| 81 | NETWR | Net Order Value | CURR | 13 | 2 | BWERT | WERT7 |
| 82 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 83 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 84 | OPEN_ORDER_QUAN | Open Quantity | QUAN | 13 | 3 | OBMNG | MENG13 |
| 85 | PEINH | Price Unit | DEC | 5 | 0 | EPEIN | DEC5 |
| 86 | PLIFZ | Planned Deliv. Time | DEC | 3 | 0 | PLIFZ | DEC3 |
| 87 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 88 | PRCTR_DESC | Long Text | CHAR | 40 | 0 | LTEXT | TEXT40 |
| 89 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 90 | PROCSTAT_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 91 | PS_PSP_PNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 92 | PSTYP | Item Category | CHAR | 1 | 0 | PSTYP | PSTYP |
| 93 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 94 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 95 | SAKTO | G/L Account | CHAR | 10 | 0 | SAKNR | SAKNR |
| 96 | SHKZG | Debit/Credit Ind. | CHAR | 1 | 0 | SHKZG | SHKZG |
| 97 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 98 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 99 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 100 | TLFAE | Val. Change Tolerance | DEC | 4 | 1 | TLFAE | PRZ31 |
| 101 | TXZ01 | Short Text | CHAR | 40 | 0 | TXZ01 | TEXT40 |
| 102 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 103 | UEBTK | Unltd Overdelivery | CHAR | 1 | 0 | UEBTK | XFELD |
| 104 | UEBTO | Overdeliv. Tolerance | DEC | 3 | 1 | UEBTO | PRZ21 |
| 105 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 106 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 107 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 108 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 109 | VBELN | SD Document | CHAR | 10 | 0 | VBELN_CO | VBELN |
| 110 | VBELP | Item | NUMC | 6 | 0 | POSNR_CO | POSNR |
| 111 | VBUND | Trading Partner | CHAR | 6 | 0 | RASSC | RCOMP |
| 112 | VGABE | Trans./event type | CHAR | 1 | 0 | VGABE | VGABE |
| 113 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 114 | WEMNG | Qty Delivered | QUAN | 13 | 3 | WEEMG | MENG13 |
| 115 | WEPOS | Goods Receipt | CHAR | 1 | 0 | WEPOS | XFELD |
| 116 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
| 117 | WGBEZ | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 117 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Created on)

Date on Which Record Was Created (or last changed) is used to filter documents or master records by last maintenance activity.

**AFNAM** (Requisitioner)

Name of Requisitioner/Requester is a standard purchasing field that identifies the specific person or department internally requesting the material or service.

**ANLN1 - ANLN2** (Asset)

Main Asset Number uniquely identifies the core capital asset (e.g., a delivery truck or a building).

**AUFNR** (Order)

Order number key for internal orders or manufacturing orders-primary CO/PP order identifier in many extracts.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERDAT

**BADAT** (Requisition Date)

Requisition date expressing when material is required-drives need-by and replenishment timing in MM.

**BANFN** (Purchase Requisition)

Purchase requisition number, the core PR document key for MM approval and lifecycle checks.

**BATXT** (Doc. Type Descript.)

<mark>Description of Purchasing Document Type provides the short text description for custom or standard purchasing codes (such as "Standard PO" for the NB document type).</mark>

**Not in use**
**BATXT_EBAN** (Doc. Type Descript.)

Gives auditors traceable criteria because doc. type descript. on BATXT_EBAN is applied consistently before any alert flag is raised.

**BATXT_EKKO** (Doc. Type Descript.)

Helps monitoring stay readable by requiring doc. type descript. (BATXT_EKKO) to match organizational or technical selectors when set.

**BEDAT** (Purchase Order Date)

Purchasing document date used to filter procurement documents by document creation period.

**BNFPO** (Item of Requisition)

Purchase requisition item number used to identify PR line-level records.

**BPUMN** (Quantity Conversion)

Denominator for price-unit conversion on purchasing conditions translating condition amounts to order quantities.

**BPUMZ** (Quantity Conversion)

Numerator for price-unit conversion paired with BPUMN to express per-unit purchasing prices correctly.

**BSART** (Purchasing Doc. Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**Not in use**
**BSART_EBAN** (Document Type)

Allows phased rollout: first widen BSART_EBAN for document type, then tighten thresholds once baseline noise is understood.

**BSART_EKKO** (Purchasing Doc. Type)

When combined with destination discipline, purchasing doc. type on BSART_EKKO keeps both breadth and depth of the extract intentional.

**BSTYP** (Purch. Doc. Category)

Purchasing document category (PR/PO/contract etc.) used to segment MM document classes.

**Not in use**
**BSTYP_DESC** (Short Descript.)

Description of purchasing document category for business-readable output.

**Not in use**
**BSTYP_EBAN** (Purch. Doc. Category)

For distributed landscapes, purch. doc. category on BSTYP_EBAN often anchors which application server or destination appears in results.

**BSTYP_EBAN_DESC** (Short Descript.)

Helps distinguish technical versus business attributes when short descript. on BSTYP_EBAN_DESC correlates with counters or status fields.

**BSTYP_EKKO** (Purch. Doc. Category)

When populated, keeps the extract focused so purch. doc. category (BSTYP_EKKO) aligns with the intended triage slice.

**BSTYP_EKKO_DESC** (Short Descript.)

Pairs with duration logic: once BSTYP_EKKO_DESC passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**BWTAR** (Valuation Type)

Valuation type key used in split valuation scenarios (batch/material valuation layers).

**BWTTY** (Valuation Category)

Valuation category distinguishing split valuation types such as sales-order stock versus own stock.

**CHANGENR** (Document number)

Change-document number that uniquely identifies one posted change document for an application object.

**CPUDT** (Entry Date)

Entry/creation date used for technical posting timestamp filtering.

**DATE_REF_FIELD** (Date ref. field)

Reflects real administration where date ref. field on DATE_REF_FIELD is routinely restricted to a single productive client or object family.

**Not in use**
**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**EBELN** (Purchasing Document)

Purchasing document number (typically PO) used as the primary MM document key.

**EBELP** (Item)

Purchasing document item number used for line-level PO analytics.

**EINDT** (Delivery Date)

Item delivery date on purchasing or delivery structures expressing vendor-promised or requested receipt date.

**EKGRP** (Purchasing Group)

Purchasing group (buyer) used for procurement ownership and control segmentation.

**EKNAM** (Description p. group)

Purchasing group description or buyer name text paired with EKGRP for readable procurement ownership lists.

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**EKOTX** (Description)

Description of Purchasing Organisation provides the short text description for custom or standard purchasing organizational units (such as "North America Procurement" for the US01 purchasing organization).

**ELIKZ** (Delivery Completed)

Delivery completed indicator used to identify open versus completed procurement items.

**ERDAT** (Changed on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**EREKZ** (Final Invoice)

Final invoice indicator on the PO item signaling that invoice completion is expected or locked for the line.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ERNAM_EBAN** (Created by)

Gives auditors traceable criteria because created by on ERNAM_EBAN is applied consistently before any alert flag is raised.

**ERNAM_EKKO** (Created by)

When harmonized with related filters, created by on ERNAM_EKKO isolates the highest-risk record families.

**ESTKZ** (Creation Indicator)

Creation indicator for PR/PO source or method, used for process-origin analysis.

**ETENR** (Schedule Line Number)

Schedule line number splitting a sales item into multiple delivery or availability schedule rows.

**FIELDNAME** (Field Name)

Valuable when comparing health before and after a release—hold field name on FIELDNAME constant while varying other filters.

**FIPOS** (Commitment Item)

Commitment Item, which is an alphanumeric key used in Funds Management (FI-FM) to mirror the budget structure for specific revenues and expenditures.

**FRGGR** (Release group)

Release group key controlling the purchasing release strategy framework.

**FRGKE** (Release indicator)

Release status indicator used to distinguish released vs unreleased documents.

**FRGKZ** (Release indicator)

Purchasing release state indicator on requisitions or orders showing whether and how release strategy applies.

**FRGSX** (Release Strategy)

Extended release information or strategy outcome code complementing FRGST on MM release objects.

**GJAHR** (Material Doc. Year)

<mark>Calendar year.</mark>

**GL_ACC_TXT** (G/L Acct Long Text)

Description of G/L Account.

**GSBER** (Business Area)

Business area key used for FI organizational reporting segmentation.

**KNTTP** (Acct Assignment Cat.)

Account assignment category on purchasing items telling whether stock is project, asset, cost-center, or sales-order.

**KOKRS** (Controlling Area)

Controlling area key used for CO-level organizational scoping.

**KOSTL** (Cost Center)

Cost center used as primary CO account assignment for postings and budgets.

**KOSTL_DESC** (Description)

Cost center description.

**KZFAE** (Changeabil.)

Supports escalation where changeabil. on KZFAE signals ownership for follow-up between Basis and functional teams.

**KZFRE** (Released)

For distributed landscapes, released on KZFRE often anchors which application server or destination appears in results.

**LFDAT** (Delivery Date)

Delivery date used for logistics due-date and fulfillment timeliness checks.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**MAKTX** (Material Description)

Valuable when comparing health before and after a release—hold material description on MAKTX constant while varying other filters.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MEINS** (Order Unit)

Base unit of measure used to interpret quantity fields consistently.

**MENGE** (Scheduled Quantity)

Quantity field used for volumetric thresholds and variance analysis.

**NAME1** (Name)

Valuable when comparing health before and after a release—hold name on NAME1 constant while varying other filters.

**NAME_FIRST_EBAN** (First name)

Helps monitoring stay readable by requiring first name (NAME_FIRST_EBAN) to match organizational or technical selectors when set.

**NAME_FIRST_EKKO** (First name)

Valuable when comparing health before and after a release—hold first name on NAME_FIRST_EKKO constant while varying other filters.

**NAME_FIRST_USER** (First name)

Gives auditors traceable criteria because first name on NAME_FIRST_USER is applied consistently before any alert flag is raised.

**NAME_LAST_EBAN** (Last name)

Stabilizes week-over-week metrics by fixing last name (NAME_LAST_EBAN) while allowing duration thresholds to move.

**NAME_LAST_EKKO** (Last name)

When left open per framework rules, NAME_LAST_EKKO does not restrict last name; when set, only matching rows remain.

**NAME_LAST_USER** (Last name)

Ensures reporting respects last name constraints carried by NAME_LAST_USER.

**NAME_TEXT_EBAN** (Complete name)

Helps monitoring stay readable by requiring complete name (NAME_TEXT_EBAN) to match organizational or technical selectors when set.

**NAME_TEXT_EKKO** (Complete name)

When tightened, complete name (NAME_TEXT_EKKO) removes rows that would otherwise dilute attention from failing or stuck cases.

**NAME_TEXT_USER** (Complete name)

For distributed landscapes, complete name on NAME_TEXT_USER often anchors which application server or destination appears in results.

**NETWR** (Net Order Value)

Net value amount used for commercial threshold and anomaly checks.

**OBJECTCLAS** (Change doc. object)

Change-document object class naming which SAP business object type the change log belongs to.

**OBJECTID** (Object value)

When left open per framework rules, OBJECTID does not restrict object value; when set, only matching rows remain.

**OPEN_ORDER_QUAN** (Open Quantity)

<mark>Remaining receipt quantity on each schedule line: scheduled quantity minus quantity already delivered, or zero when nothing is left open. It is written to the output and can also be used as a filter when a selection range is supplied.</mark>

**PEINH** (Price Unit)

Price unit denominator used to interpret per-unit purchasing prices.

**PLIFZ** (Planned Deliv. Time)

Planned delivery time in days from purchasing info records or schedule lines for lead-time analytics.

**PRCTR** (Profit Center)

Profit center used for management accounting segmentation and profitability reporting.

**PRCTR_DESC** (Long Text)

Profit center description.

**PROCSTAT** (Purch. doc. proc. state)

Purchasing document processing state describing lifecycle and processing of MM purchasing objects.

**PROCSTAT_DESC** (Short Descript.)

Readable description of purchasing processing status (PROCSTAT); text expansion for reporting output.

**PS_PSP_PNR** (WBS Element)

WBS element key used for project-system linked cost/procurement monitoring.

**PSTYP** (Item Category)

Purchasing document item category controlling item behavior, account assignment, and goods-receipt rules.

**RESWK** (Supplying Plant)

Supplying/Issuing Plant designates the specific internal plant from which materials are being transferred or procured during a Stock Transport Order. Used in cross-plant logistics analysis.

**RESWK_DESC** (Name 1)

Plant description text used to enrich plant-level reporting.

**SAKTO** (G/L Account)

Cost element used in CO postings for primary or secondary cost capture and account assignment.

**SHKZG** (Debit/Credit Ind.)

Debit/Credit indicator used to separate accounting posting direction.

**SHKZG Options:**
- S: Debit (Soll)
- H: Credit (Haben)

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**STATU_DESC** (Short Descript.)

Status description text used for readable status analytics.

**TABNAME** (Table Name)

Database table name used to scope change/object monitoring to specific tables.

**TLFAE** (Val. Change Tolerance)

Combines with related filters so val. change tolerance on TLFAE refines which records remain for duration or state checks.

**TXZ01** (Short Text)

Description of Short Text provides the readable name or detailed line-item description for a material, service, or component within a purchasing, sales, or production document (such as "Standard 10mm Steel Bolt" for an inventory item).

**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**UEBTK** (Unltd Overdelivery)

Unlimited overdelivery allowed indicator on SD or MM quantity contracts controlling tolerance behavior.

**UEBTO** (Overdeliv. Tolerance)

Overdelivery tolerance percent defining how much quantity overrun is accepted versus the order quantity.

**USERNAME** (User)

<mark>User who posted the change.</mark>

**UTIME** (Time)

Update/change time used with UDATE for precise event windows.

**VALUE_NEW** (New value)

New value in change documents used for before/after comparison.

**VALUE_OLD** (Old value)

Old value in change documents used for before/after comparison.

**VBELN** (SD Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBELP** (Item)

Sales document item number alias on extension extracts; typically mirrors POSNR line indexing.

**VBUND** (Trading Partner)

Trading partner/company field used for intercompany transaction analysis.

**VGABE** (Trans./event type)

Transaction/event type in purchasing history used to classify movement category.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WEMNG** (Qty Delivered)

Goods-receipt quantity on purchasing history or order-related rows for GR-versus-PO variance checks.

**WEPOS** (Goods Receipt)

Goods-receipt indicator on purchasing history rows marking lines created by goods receipt postings.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WGBEZ** (Material Group Desc.)

Material group description used for readable category reporting.

### Parameter Relationships

**Reference-date window:** When no explicit date range is supplied, a lower bound of today minus **BACKDAYS** is applied with a greater-than-or-equal filter on the change-document date axis (**UDATE** by default in code). Explicit **UDATE**, **BEDAT**, **AEDAT**, **ERDAT**, or **BADAT** selections override that fallback window depending on which date fields are populated.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference timestamp to the evaluation date; rows outside the configured duration range are removed.

**Change-document scope:** **OBJECTCLAS**, **TABNAME**, and **FIELDNAME** define which change-document object class, table, and field are read; **VALUE_NEW** and **VALUE_OLD** filter the new and prior values recorded in the change log.

**Release confirmation scope:** Purchase order headers must match **FRGKE** (release indicator) values that align with the confirmation reference table; **FRGSX** must be non-empty and **FRGGR** further narrows release-strategy scope.

**Header and item scope:** **EBELN**, **EBELP**, **BUKRS**, **BSTYP_EKKO**, **BSART_EKKO**, **EKORG**, **EKGRP**, **LIFNR**, **MATNR**, **WERKS**, **STATU**, and **PROCSTAT** combine to define which purchase order lines enter the result set before change-document enrichment.

**Requisition linkage:** **BANFN**, **BNFPO**, **BSTYP_EBAN**, **BSART_EBAN**, **FRGKZ**, **ESTKZ**, **LOEKZ**, and related requisition dates and creators filter the linked purchase requisition side of each order line.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **OBJECTCLAS** - EINKBELEG
- **TABNAME** - EKKO
- **FIELDNAME** - FRGKE

### Practical Example of Parameter Configuration

**Use Case 1: Recent confirmation changes in the last thirty days**

**Purpose:** Review purchase orders with release-indicator changes posted on change-document dates in the last thirty days.

```
BACKDAYS = 30
BUKRS = 1000
FRGKE = 1
EKORG = 1000
```

**Use Case 2: Specific release group and strategy**

**Purpose:** Monitor incomplete confirmation cases under one release group and strategy combination for targeted buyer follow-up.

```
FRGGR = 01
FRGSX = 01
BACKDAYS = 45
BUKRS = 1000
PROCSTAT = 02
```

**Use Case 3: Vendor-specific confirmation backlog**

**Purpose:** Focus on one vendor's purchase orders where release-indicator changes still indicate open confirmation work.

```
LIFNR = 100000
BACKDAYS = 60
FRGKE = 1
EKGRP = 001
BUKRS = 1000
```

**Use Case 4: Requisition-side release filter**

**Purpose:** Narrow results to requisitions with a specific release state linked to flagged purchase order lines.

```
FRGKZ = 2
BACKDAYS = 90
BSTYP_EKKO = F
EKORG = 1000
```

**Use Case 5: Exactly seven full days since reference date**

**Purpose:** Return rows whose reference date is exactly 7 full days ago for weekly confirmation escalation.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
BUKRS = 1000
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_03_PO_CONF_COMPL | AEDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | AFNAM | Requisitioner | CHAR(12) | AFNAM |
| /SKN/S_SW_10_03_PO_CONF_COMPL | ANLN1 | Asset | CHAR(12) | ANLN1 |
| /SKN/S_SW_10_03_PO_CONF_COMPL | ANLN2 | Subnumber | CHAR(4) | ANLN2 |
| /SKN/S_SW_10_03_PO_CONF_COMPL | AUFNR | Order | CHAR(12) | AUFNR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BADAT | Requisition Date | DATS(8) | BADAT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BANFN | Purchase Requisition | CHAR(10) | BANFN |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BATXT_EBAN | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BATXT_EKKO | Doc. Type Descript. | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BEDAT | Purchase Order Date | DATS(8) | ETBDT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BNFPO | Item of Requisition | NUMC(5) | BNFPO |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BPUMN | Quantity Conversion | DEC(5) | BPUMN |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BPUMZ | Quantity Conversion | DEC(5) | BPUMZ |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BSART_EBAN | Document Type | CHAR(4) | BBSRT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BSART_EKKO | Purchasing Doc. Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BSTYP_EBAN | Purch. Doc. Category | CHAR(1) | BSTYP |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BSTYP_EBAN_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BSTYP_EKKO | Purch. Doc. Category | CHAR(1) | BSTYP |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BSTYP_EKKO_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_03_PO_CONF_COMPL | BWTTY | Valuation Category | CHAR(1) | BWTTY_D |
| /SKN/S_SW_10_03_PO_CONF_COMPL | CHANGENR | Document number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | CPUDT | Entry Date | DATS(8) | CPUDT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_CONF_COMPL | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | EBELN | Purchasing Document | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_CONF_COMPL | EBELP | Item | NUMC(5) | EBELP |
| /SKN/S_SW_10_03_PO_CONF_COMPL | EINDT | Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | EKGRP | Purchasing Group | CHAR(3) | EKGRP |
| /SKN/S_SW_10_03_PO_CONF_COMPL | EKNAM | Description p. group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_CONF_COMPL | EKORG | Purch. Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_CONF_COMPL | EKOTX | Description | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_CONF_COMPL | ELIKZ | Delivery Completed | CHAR(1) | ELIKZ |
| /SKN/S_SW_10_03_PO_CONF_COMPL | ERDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | EREKZ | Final Invoice | CHAR(1) | EREKZ |
| /SKN/S_SW_10_03_PO_CONF_COMPL | ERNAM_EBAN | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_CONF_COMPL | ERNAM_EKKO | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_CONF_COMPL | ESTKZ | Creation Indicator | CHAR(1) | ESTKZ |
| /SKN/S_SW_10_03_PO_CONF_COMPL | ETENR | Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | FIELDNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_03_PO_CONF_COMPL | FIPOS | Commitment Item | CHAR(14) | FIPOS |
| /SKN/S_SW_10_03_PO_CONF_COMPL | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | FRGKE | Release indicator | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_PO_CONF_COMPL | FRGKZ | Release indicator | CHAR(1) | FRGKZ |
| /SKN/S_SW_10_03_PO_CONF_COMPL | FRGSX | Release Strategy | CHAR(2) | FRGSX |
| /SKN/S_SW_10_03_PO_CONF_COMPL | GJAHR | Material Doc. Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | GL_ACC_TXT | G/L Acct Long Text | CHAR(50) | TXT50_SKAT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_03_PO_CONF_COMPL | KNTTP | Acct Assignment Cat. | CHAR(1) | KNTTP |
| /SKN/S_SW_10_03_PO_CONF_COMPL | KOKRS | Controlling Area | CHAR(4) | KOKRS |
| /SKN/S_SW_10_03_PO_CONF_COMPL | KOSTL | Cost Center | CHAR(10) | KOSTL |
| /SKN/S_SW_10_03_PO_CONF_COMPL | KOSTL_DESC | Description | CHAR(40) | KLTXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | KZFAE | Changeabil. | CHAR(1) | KZFAE |
| /SKN/S_SW_10_03_PO_CONF_COMPL | KZFRE | Released | CHAR(1) | KZFRE |
| /SKN/S_SW_10_03_PO_CONF_COMPL | LFDAT | Delivery Date | DATS(8) | EINDT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | LIFNR | Vendor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | LOEKZ | Deletion Indicator | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_CONF_COMPL | MAKTX | Material Description | CHAR(40) | MAKTX |
| /SKN/S_SW_10_03_PO_CONF_COMPL | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_03_PO_CONF_COMPL | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | MEINS | Order Unit | UNIT(3) | BSTME |
| /SKN/S_SW_10_03_PO_CONF_COMPL | MENGE | Scheduled Quantity | QUAN(13) | ETMEN |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_FIRST_EBAN | First name | CHAR(40) | NAME_FIRST |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_FIRST_EKKO | First name | CHAR(40) | NAME_FIRST |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_FIRST_USER | First name | CHAR(40) | NAME_FIRST |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_LAST_EBAN | Last name | CHAR(40) | NAME_LAST |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_LAST_EKKO | Last name | CHAR(40) | NAME_LAST |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_LAST_USER | Last name | CHAR(40) | NAME_LAST |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_TEXT_EBAN | Complete name | CHAR(80) | NAME_TEXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_TEXT_EKKO | Complete name | CHAR(80) | NAME_TEXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NAME_TEXT_USER | Complete name | CHAR(80) | NAME_TEXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | NETWR | Net Order Value | CURR(13) | BWERT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | OBJECTCLAS | Change doc. object | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_03_PO_CONF_COMPL | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_03_PO_CONF_COMPL | OPEN_ORDER_QUAN | Open Quantity | QUAN(13) | OBMNG |
| /SKN/S_SW_10_03_PO_CONF_COMPL | PEINH | Price Unit | DEC(5) | EPEIN |
| /SKN/S_SW_10_03_PO_CONF_COMPL | PLIFZ | Planned Deliv. Time | DEC(3) | PLIFZ |
| /SKN/S_SW_10_03_PO_CONF_COMPL | PRCTR | Profit Center | CHAR(10) | PRCTR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | PRCTR_DESC | Long Text | CHAR(40) | LTEXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | PROCSTAT | Purch. doc. proc. state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_PO_CONF_COMPL | PROCSTAT_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | PSTYP | Item Category | CHAR(1) | PSTYP |
| /SKN/S_SW_10_03_PO_CONF_COMPL | PS_PSP_PNR | WBS Element | NUMC(8) | PS_PSP_PNR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | RESWK | Supplying Plant | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PO_CONF_COMPL | RESWK_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PO_CONF_COMPL | SAKTO | G/L Account | CHAR(10) | SAKNR |
| /SKN/S_SW_10_03_PO_CONF_COMPL | SHKZG | Debit/Credit Ind. | CHAR(1) | SHKZG |
| /SKN/S_SW_10_03_PO_CONF_COMPL | STATU | Status | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_CONF_COMPL | STATU_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_03_PO_CONF_COMPL | TLFAE | Val. Change Tolerance | DEC(4) | TLFAE |
| /SKN/S_SW_10_03_PO_CONF_COMPL | TXZ01 | Short Text | CHAR(40) | TXZ01 |
| /SKN/S_SW_10_03_PO_CONF_COMPL | UDATE | Date | DATS(8) | CDDATUM |
| /SKN/S_SW_10_03_PO_CONF_COMPL | UEBTK | Unltd Overdelivery | CHAR(1) | UEBTK |
| /SKN/S_SW_10_03_PO_CONF_COMPL | UEBTO | Overdeliv. Tolerance | DEC(3) | UEBTO |
| /SKN/S_SW_10_03_PO_CONF_COMPL | USERNAME | User | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_03_PO_CONF_COMPL | UTIME | Time | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_03_PO_CONF_COMPL | VALUE_NEW | New value | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_03_PO_CONF_COMPL | VALUE_OLD | Old value | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_03_PO_CONF_COMPL | VBELN | SD Document | CHAR(10) | VBELN_CO |
| /SKN/S_SW_10_03_PO_CONF_COMPL | VBELP | Item | NUMC(6) | POSNR_CO |
| /SKN/S_SW_10_03_PO_CONF_COMPL | VBUND | Trading Partner | CHAR(6) | RASSC |
| /SKN/S_SW_10_03_PO_CONF_COMPL | VGABE | Trans./event type | CHAR(1) | VGABE |
| /SKN/S_SW_10_03_PO_CONF_COMPL | WAERS | Currency | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_CONF_COMPL | WEMNG | Qty Delivered | QUAN(13) | WEEMG |
| /SKN/S_SW_10_03_PO_CONF_COMPL | WEPOS | Goods Receipt | CHAR(1) | WEPOS |
| /SKN/S_SW_10_03_PO_CONF_COMPL | WERKS | Plant | CHAR(4) | EWERK |
| /SKN/S_SW_10_03_PO_CONF_COMPL | WGBEZ | Material Group Desc. | CHAR(20) | WGBEZ |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PO_CONF_COMPL .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_CONF_COMPL OPTIONAL
*"----------------------------------------------------------------------
  DATA: BEGIN OF EKKO.
          INCLUDE STRUCTURE EKKO.
  DATA:   EBELP TYPE EKPO-EBELP,
          MATNR TYPE EKPO-MATNR,
          VBUND TYPE LFA1-VBUND.
  DATA: END OF EKKO.
  DATA: BEGIN OF LS_WRK.
          INCLUDE STRUCTURE /SKN/S_SW_10_03_PO_CONF_COMPL.
  DATA: WRK_OBJECTID TYPE CDOBJECTV.
  DATA: WRK_TABKEY   TYPE CDPOS-TABKEY.
  DATA: END OF LS_WRK.
  DATA_SINGLE: LANGU          LANGU,
               BACKDAYS       INT4,
               DATE_REF_FLD   NAME_FELD,
*               elikz          elikz,
*               loekz          eloek,
*               wepos          wepos,
               OBJECTCLAS     CDOBJECTCL,
               TABNAME        TABNAME,
               FIELDNAME      FIELDNAME,
               LAST_ONLY      CHAR1,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
*
*
  LV_BACKDAYS      = 10.
  LV_DURATION_UNIT = 'D'.
  LV_DATE_REF_FLD  = 'UDATE'.          " PO release date
  LV_OBJECTCLAS    = 'EINKBELEG'.
  LV_TABNAME       = 'EKKO'.
  LV_FIELDNAME     = 'FRGKE'.
  LV_LAST_ONLY     = 'X'.              " Take the last changes of the field
*  lv_elikz         = space.
*  lv_loekz         = space.
*  lv_wepos         = 'X'.
*
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 OBJECTCLAS,
                 TABNAME,
                 FIELDNAME,
                 LAST_ONLY,
                 DURATION_UNIT.
*
*
  DATA_MULTY: EBELN           EBELN,
              EBELP           EBELP,
              BUKRS           BUKRS,
              BSTYP_EKKO      EBSTYP,
              BSTYP_EBAN      BSTYP,
              BSART_EKKO      ESART,
              BSART_EBAN      BBSRT,
              LOEKZ           ELOEK,
              STATU           ESTAK,
              AEDAT           ERDAT,
              ERNAM           ERNAM,
              LIFNR           ELIFN,
              EKORG           EKORG,
              EKGRP           BKGRP,
              FRGGR           FRGGR,
              FRGSX           FRGSX,
*              frgco           frgco,
              FRGKE           FRGKE,
              FRGKZ           FRGKZ,
              FRGRL           FRGRL,
              WAERS           WAERS,
              MATNR           MATNR,
              WERKS           EWERK,
              MATKL           MATKL,
              KNTTP           KNTTP,
              BWTAR           BWTAR_D,
              BWTTY           BWTTY_D,
              ELIKZ           ELIKZ,
              EREKZ           EREKZ,
              PSTYP           PSTYP,
              FIPOS           FIPOS,
              WEPOS           WEPOS,
              BEDAT           EBDAT,
              EINDT           EINDT,
              BANFN           BANFN,
              BNFPO           BNFPO,
              ESTKZ           ESTKZ,
              VBUND           RASSC,
              UEBTO           UEBTO,
              UEBTK           UEBTK,
              SAKTO           SAKTO,
              GSBER           GSBER,
              KOSTL           KOSTL,
              VBELN           VBELN,
              VBELP           POSNR_VA,
              ANLN1           ANLN1,
              ANLN2           ANLN2,
              AUFNR           AUFNR,
              PRCTR           PRCTR,
              BADAT           BADAT,
              LFDAT           EINDT,
              ERDAT           AEDAT,
              USERNAME        CDUSERNAME,
              UDATE           CDDATUM,
              PROCSTAT        MEPROCSTATE,
              WEMNG           WEEMG,
              OPEN_ORDER_QUAN OBMNG,
              PS_PSP_PNR      PS_PSP_PNR,
              VALUE_OLD       CDFLDVALO,
              VALUE_NEW       CDFLDVALN,
              DATUM           SY-DATUM,
              DURATION        /SKN/E_SW_DURATION.
*
  SELECT_MULTY: EBELN,
                EBELP,
                BUKRS,
                BSTYP_EKKO,
                BSTYP_EBAN,
                BSART_EKKO,
                BSART_EBAN,
                LOEKZ,
                STATU,
                AEDAT,
                ERNAM,
                LIFNR,
                EKORG,
                EKGRP,
                FRGGR,
                FRGSX,
*                frgco,
                FRGKE,
                FRGKZ,
                FRGRL,
                WAERS,
                MATNR,
                WERKS,
                MATKL,
                KNTTP,
                BWTAR,
                BWTTY,
                ELIKZ,
                EREKZ,
                PSTYP,
                FIPOS,
                WEPOS,
                BEDAT,
                BANFN,
                BNFPO,
                ESTKZ,
                VBUND,
                UEBTO,
                UEBTK,
                SAKTO,
                GSBER,
                KOSTL,
                VBELN,
                VBELP,
                ANLN1,
                ANLN2,
                AUFNR,
                PRCTR,
                BADAT,
                LFDAT,
                ERDAT,
                USERNAME,
                UDATE,
                PROCSTAT,
                WEMNG,
                OPEN_ORDER_QUAN,
                PS_PSP_PNR,
                VALUE_OLD,
                VALUE_NEW,
                DATUM,
                DURATION.
*
  CONVERT_MULTY: EBELN ALPHA,
                 LIFNR ALPHA.
*
  DATA: DATE_FROM TYPE SY-DATUM.
  DATA: SY_TABIX LIKE SY-TABIX .
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: LV_WEMNG        TYPE EKET-WEMNG,
        LV_MENGE        TYPE EKET-MENGE,
        LV_DOMNAME      TYPE DD07V-DOMNAME,
        LV_DOMVALUE     TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT       TYPE DD07V-DDTEXT,
        LV_KTEXT        TYPE  KTEXT,
        LV_VAL_TMP1     TYPE P DECIMALS 3,
        LV_VAL_TMP2     TYPE P DECIMALS 3,
        LV_WRK_OBJECTID TYPE CDOBJECTV,
        LV_WRK_TABKEY   TYPE CDTABKEY.
  DATA: LS_EKKO  LIKE EKKO,
        LS_EKKN  TYPE EKKN,
        LS_CDPOS TYPE CDPOS,
        LS_DATA  LIKE LINE OF T_DATA[].
  DATA: LT_WRK LIKE STANDARD TABLE OF LS_WRK.
  DATA: LT_EKKO  LIKE TABLE OF EKKO,
        LT_EKKN  TYPE TABLE OF EKKN,
        LT_CDPOS TYPE STANDARD TABLE OF CDPOS.
  FIELD-SYMBOLS: <FS_WRK> LIKE LS_WRK,
                 <FS_DATA> LIKE LINE OF T_DATA[],
                       TYPE ANY.
*
*"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PO_CONF_COMPL'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
*
** Initial Date
  IF R_DATUM[] IS INITIAL.
    RS_DATUM-SIGN   = 'I'.
    RS_DATUM-OPTION = 'GT'.
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM.
*     rs_datum-high   = sy-datum.
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
*
*  IF r_elikz[] IS INITIAL.
*    REFRESH: r_elikz[].
*    CLEAR rs_elikz.
*    rs_elikz-sign   = 'I'.
*    rs_elikz-option = 'EQ'.
*    rs_elikz-low    = lv_elikz.
*    APPEND rs_elikz TO r_elikz[].
*  ENDIF.
*
*  IF r_loekz[] IS INITIAL.
*    REFRESH: r_loekz[].
*    CLEAR rs_loekz.
*    rs_loekz-sign   = 'I'.
*    rs_loekz-option = 'EQ'.
*    rs_loekz-low    = lv_loekz.
*    APPEND rs_loekz TO r_loekz[].
*  ENDIF.
*
*  IF r_wepos[] IS INITIAL.
*    REFRESH: r_wepos[].
*    CLEAR rs_wepos.
*    rs_wepos-sign   = 'I'.
*    rs_wepos-option = 'EQ'.
*    rs_wepos-low    = lv_wepos.
*    APPEND rs_wepos TO r_wepos[].
*  ENDIF.
*
  IF LV_LANGU IS INITIAL.
    LV_LANGU = SY-LANGU.
  ENDIF.
*
* "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'BEDAT'.
      R_BEDAT[] = R_DATUM[].    " Purchasing document
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[].    " Document created
    WHEN 'UDATE'.
      R_UDATE[] = R_DATUM[].    " Release date
*    WHEN 'EINDT'.
*      r_eindt[] = r_datum[]. " Item Delivery
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[].
    WHEN 'BADAT'.
      R_BADAT[] = R_DATUM[].
    WHEN OTHERS.
      R_BEDAT[] = R_DATUM[]. " Purchasing document
  ENDCASE.
*
**--- Retrieve data
  CLEAR IS_ALERT .
  SELECT *
    FROM EKKO AS K INNER JOIN EKPO  AS P  ON  K~EBELN     EQ P~EBELN
                   INNER JOIN T16FB AS T  ON  K~FRGKE     EQ T~FRGKE
                   INNER JOIN CDHDR AS CD ON  CD~OBJECTID EQ K~EBELN
                   INNER JOIN EBAN  AS A  ON  P~BANFN     EQ A~BANFN
                                          AND P~BNFPO     EQ A~BNFPO
    INTO CORRESPONDING FIELDS OF TABLE LT_WRK
* EKPO
    WHERE P~EBELN       IN R_EBELN[]
    AND   P~EBELP       IN R_EBELP[]
    AND   P~MATNR       IN R_MATNR[]
    AND   P~WERKS       IN R_WERKS[]
    AND   P~MATKL       IN R_MATKL[]
    AND   P~KNTTP       IN R_KNTTP[]
    AND   P~BWTAR       IN R_BWTAR[]
    AND   P~BWTTY       IN R_BWTTY[]
    AND   P~EREKZ       IN R_EREKZ[]
    AND   P~PSTYP       IN R_PSTYP[]
    AND   P~FIPOS       IN R_FIPOS[]
    AND   P~WEPOS       IN R_WEPOS[]
    AND   P~UEBTO       IN R_UEBTO[]
    AND   P~UEBTK       IN R_UEBTK[]
* EKKO
    AND   K~FRGRL       IN R_FRGRL[]
    AND   K~FRGGR       IN R_FRGGR[]
    AND   K~FRGSX       IN R_FRGSX[]
    AND   K~BUKRS       IN R_BUKRS[]
    AND   K~BSTYP       IN R_BSTYP_EKKO[]
    AND   K~BSART       IN R_BSART_EKKO[]
    AND   K~STATU       IN R_STATU[]
    AND   K~AEDAT       IN R_AEDAT[]
    AND   K~BEDAT       IN R_BEDAT[]
    AND   K~LIFNR       IN R_LIFNR[]
    AND   K~EKORG       IN R_EKORG[]
    AND   K~EKGRP       IN R_EKGRP[]
    AND   K~FRGSX       NE SPACE
    AND   K~FRGKE       IN R_FRGKE[]
    AND   K~PROCSTAT    IN R_PROCSTAT[]
* CDHDR
    AND   CD~OBJECTCLAS EQ LV_OBJECTCLAS
    AND   CD~USERNAME   IN R_USERNAME
    AND   CD~UDATE      IN R_UDATE[]
* EBAN
    AND   A~BSART IN R_BSART_EBAN[]
    AND   A~BSTYP IN R_BSTYP_EBAN[]
    AND   A~FRGKZ IN R_FRGKZ[]
    AND   A~ESTKZ IN R_ESTKZ[]
    AND   A~ERDAT IN R_ERDAT[]
    AND   A~BADAT IN R_BADAT[]
    AND   A~LFDAT IN R_LFDAT[]
    AND   A~LOEKZ IN R_LOEKZ[]
    AND   A~ERNAM IN R_ERNAM[].
  SORT LT_WRK.
  DELETE ADJACENT DUPLICATES FROM LT_WRK.
  LOOP AT LT_WRK ASSIGNING <FS_WRK>.
    <FS_WRK>-WRK_OBJECTID = LS_WRK-EBELN.
  ENDLOOP.
  IF LT_WRK[] IS NOT INITIAL.
    LOOP AT R_FRGKE INTO RS_FRGKE.
      RS_VALUE_NEW-SIGN   = RS_FRGKE-SIGN.
      RS_VALUE_NEW-OPTION = RS_FRGKE-OPTION.
      RS_VALUE_NEW-LOW    = RS_FRGKE-LOW.
      RS_VALUE_NEW-HIGH   = RS_FRGKE-HIGH.
      APPEND RS_VALUE_NEW TO R_VALUE_NEW.
      CLEAR: RS_VALUE_NEW.
    ENDLOOP.
    SELECT *
      FROM CDPOS
      INTO CORRESPONDING FIELDS OF TABLE LT_CDPOS
      FOR ALL ENTRIES IN LT_WRK
      WHERE OBJECTCLAS EQ LV_OBJECTCLAS         " default - 'EINKBELEG'
      AND   OBJECTID   EQ LT_WRK-WRK_OBJECTID   " EBELN
      AND   TABNAME    EQ LV_TABNAME            " default - 'EKKO'
      AND   FNAME      EQ LV_FIELDNAME          " default - 'FRGKE'
      AND   VALUE_NEW  IN R_VALUE_NEW[]         "
      AND   VALUE_OLD  IN R_VALUE_OLD[].
    IF LT_CDPOS IS NOT INITIAL.
      SORT LT_CDPOS BY VALUE_NEW.
      DELETE LT_CDPOS WHERE VALUE_NEW IS INITIAL.
    ENDIF.
    SORT LT_CDPOS BY OBJECTCLAS OBJECTID CHANGENR DESCENDING TABKEY.
  ENDIF.
  LOOP AT LT_WRK ASSIGNING <FS_WRK>.
    SY_TABIX = SY-TABIX.
    READ TABLE LT_CDPOS INTO LS_CDPOS WITH KEY OBJECTCLAS = LV_OBJECTCLAS
                                               OBJECTID   = <FS_WRK>-WRK_OBJECTID
                                               CHANGENR   = <FS_WRK>-CHANGENR
                                               BINARY SEARCH.
    IF SY-SUBRC IS NOT INITIAL.
      DELETE LT_WRK INDEX SY_TABIX.
    ELSE.
      <FS_WRK>-TABNAME   = LS_CDPOS-TABNAME.
      <FS_WRK>-FIELDNAME = LS_CDPOS-FNAME.
      <FS_WRK>-VALUE_NEW = LS_CDPOS-VALUE_NEW.
      <FS_WRK>-VALUE_OLD = LS_CDPOS-VALUE_OLD.
    ENDIF.
  ENDLOOP.
  SORT LT_WRK BY WRK_OBJECTID UDATE DESCENDING UTIME DESCENDING.
  REFRESH T_DATA.
  IF LV_LAST_ONLY IS NOT INITIAL.               " Only last release is relevant - the rest is deleted.
    CLEAR: LV_WRK_OBJECTID, LV_WRK_TABKEY.
    LOOP AT LT_WRK INTO LS_WRK.
      IF LV_WRK_OBJECTID EQ LS_WRK-WRK_OBJECTID.
        CONTINUE.
      ELSE.
        LV_WRK_OBJECTID = LS_WRK-WRK_OBJECTID.
        MOVE-CORRESPONDING LS_WRK TO T_DATA.
        T_DATA-TABNAME   = LV_TABNAME.
        T_DATA-FIELDNAME = LV_FIELDNAME.
        APPEND T_DATA.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT LT_WRK INTO LS_WRK.
      MOVE-CORRESPONDING LS_WRK TO T_DATA.
      T_DATA-TABNAME   = LV_TABNAME.
      T_DATA-FIELDNAME = LV_FIELDNAME.
      APPEND T_DATA.
    ENDLOOP.
  ENDIF.
  IF T_DATA[] IS NOT INITIAL.
    SORT T_DATA[] BY EBELN EBELP.
  ENDIF.
***********************************************************************************
*
**-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    CHECK FLD IS NOT INITIAL.
    ASSIGN (FLD) TO .
    CHECK  IS ASSIGNED.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
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
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION.
*******************************************************************************
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
*
    SY_TABIX = SY-TABIX.
*    CLEAR: lv_menge, lv_wemng, lv_val_tmp1, lv_val_tmp2.
*    IF <fs_data>-menge > <fs_data>-wemng.
*      <fs_data>-open_order_quan = <fs_data>-menge - <fs_data>-wemng.
*    ELSE.
*      <fs_data>-open_order_quan = 0.
*    ENDIF.
*    IF r_open_order_quan[] IS NOT INITIAL.
*      IF NOT <fs_data>-open_order_quan IN r_open_order_quan[].
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*    READ TABLE lt_ekkn INTO ls_ekkn WITH KEY ebeln = <fs_data>-ebeln
*                                             ebelp = <fs_data>-ebelp
*                                             BINARY SEARCH.
*    IF sy-subrc = 0.
*      IF ls_ekkn-sakto IN r_sakto[] OR r_sakto[] IS INITIAL.
*        <fs_data>-sakto = ls_ekkn-sakto.
*      ELSE.
*        DELETE t_data WHERE sakto      NOT IN r_sakto.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-gsber IN r_gsber[] OR r_gsber[] IS INITIAL.
*        <fs_data>-gsber = ls_ekkn-gsber.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-kostl IN r_kostl[] OR r_kostl[] IS INITIAL.
*        <fs_data>-kostl = ls_ekkn-kostl.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-vbeln IN r_vbeln[] OR r_vbeln[] IS INITIAL.
*        <fs_data>-vbeln = ls_ekkn-vbeln.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-vbelp IN r_vbelp[] OR r_vbelp[] IS INITIAL.
*        <fs_data>-vbelp = ls_ekkn-vbelp.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-anln1 IN r_anln1[] OR r_anln1[] IS INITIAL.
*        <fs_data>-anln1 = ls_ekkn-anln1.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-anln2 IN r_anln2[] OR r_anln2[] IS INITIAL.
*        <fs_data>-anln2 = ls_ekkn-anln2.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-aufnr IN r_aufnr[] OR r_aufnr[] IS INITIAL.
*        <fs_data>-aufnr = ls_ekkn-aufnr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-prctr IN r_prctr[] OR r_prctr[] IS INITIAL.
*        <fs_data>-prctr = ls_ekkn-prctr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*      IF ls_ekkn-ps_psp_pnr IN r_ps_psp_pnr[] OR r_ps_psp_pnr[] IS INITIAL.
*        <fs_data>-ps_psp_pnr = ls_ekkn-ps_psp_pnr.
*      ELSE.
*        DELETE t_data INDEX sy_tabix.
*        CONTINUE.
*      ENDIF.
*
*    ENDIF.
*    IF lv_menge <= lv_wemng.
*      DELETE t_data[] INDEX sy_tabix.
*      CONTINUE.
*    ENDIF.
*
*    <fs_data>-waers_local     = <fs_data>-waers.
*    <fs_data>-menge           = lv_menge.
*    <fs_data>-wemng           = lv_wemng.
*    <fs_data>-open_order_quan = lv_menge - lv_wemng.
*
*    IF <fs_data>-bpumn <> 0 AND <fs_data>-peinh <> 0.
*      lv_val_tmp1 = <fs_data>-netpr * ( <fs_data>-open_order_quan ).
*      lv_val_tmp2 = ( <fs_data>-bpumz / <fs_data>-bpumn ) /
*                      <fs_data>-peinh.
*
*      <fs_data>-open_value  = lv_val_tmp1 * lv_val_tmp2.
*    ELSE.
*      <fs_data>-open_value = 0.
*    ENDIF.
* Get PO Category desc.
    IF <FS_DATA>-BSTYP_EKKO IS NOT INITIAL.
      "-- BSTYP_DESC
      LV_DOMNAME = 'EBSTYP'.
      LV_DOMVALUE = <FS_DATA>-BSTYP_EKKO.
      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
        EXPORTING
          I_DOMNAME  = LV_DOMNAME
          I_DOMVALUE = LV_DOMVALUE
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          E_DDTEXT   = LV_DDTEXT
        EXCEPTIONS
          NOT_EXIST  = 1
          OTHERS     = 2.
      IF SY-SUBRC = 0.
        <FS_DATA>-BSTYP_EKKO_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
* Get PR Category desc.
    IF <FS_DATA>-BSTYP_EBAN IS NOT INITIAL.
      "-- BSTYP_DESC
      LV_DOMNAME = 'BSTYP'.
      LV_DOMVALUE = <FS_DATA>-BSTYP_EBAN.
      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
        EXPORTING
          I_DOMNAME  = LV_DOMNAME
          I_DOMVALUE = LV_DOMVALUE
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          E_DDTEXT   = LV_DDTEXT
        EXCEPTIONS
          NOT_EXIST  = 1
          OTHERS     = 2.
      IF SY-SUBRC = 0.
        <FS_DATA>-BSTYP_EBAN_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
* G/L Account Description
    IF <FS_DATA>-SAKTO IS NOT INITIAL AND <FS_DATA>-BUKRS IS NOT INITIAL.
      DATA: LV_ACC_DESC TYPE  TXT20_SKAT.
      CALL FUNCTION '/SKN/F_SW_10_SAKTO_DESC'
        EXPORTING
          SPRAS      = SY-LANGU
          BUKRS      = <FS_DATA>-BUKRS
*         KTOPL      =
          SAKNR      = <FS_DATA>-SAKTO
        IMPORTING
          ACC_DESC   = LV_ACC_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-GL_ACC_TXT = LV_ACC_DESC.
    ENDIF.
* Cost Center Description
    IF <FS_DATA>-KOSTL IS NOT INITIAL AND <FS_DATA>-KOKRS IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_KOSTL_DESC'
        EXPORTING
          SPRAS      = SY-LANGU
          KOKRS      = <FS_DATA>-KOKRS
          KOSTL      = <FS_DATA>-KOSTL
        IMPORTING
          KOSTL_DESC = LV_KTEXT
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-KOSTL_DESC = LV_KTEXT.
    ENDIF.
* Profit Center Description
    IF <FS_DATA>-PRCTR IS NOT INITIAL AND <FS_DATA>-KOKRS IS NOT INITIAL.
      DATA: LVV_KTEXT TYPE  KTEXT.
      CALL FUNCTION '/SKN/F_SW_10_PRCTR_DESC'
        EXPORTING
          SPRAS      = SY-LANGU
          PRCTR      = <FS_DATA>-PRCTR
          KOKRS      = <FS_DATA>-KOKRS
        IMPORTING
          KTEXT      = LVV_KTEXT
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      <FS_DATA>-PRCTR_DESC = LVV_KTEXT.
    ENDIF.
* Material group desc.
    IF <FS_DATA>-MATKL IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
        EXPORTING
          MATKL      = <FS_DATA>-MATKL
*         LANGU      = SY-LANGU
        IMPORTING
          MATKL_DESC = <FS_DATA>-WGBEZ
*         MATKL_DESC_L       =
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
*    "--- Get  Vendor Decriptions
    IF <FS_DATA>-LIFNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = <FS_DATA>-LIFNR
        IMPORTING
          VENDOR_DESC  = <FS_DATA>-NAME1
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
* Get PO Doc.Type desc.
    IF <FS_DATA>-BSART_EKKO IS NOT INITIAL AND <FS_DATA>-BSTYP_EKKO IS NOT INITIAL.
      "-- BSART_DESC
      CALL FUNCTION '/SKN/FC_SW_10_BSART_DESC'
        EXPORTING
          BSART      = <FS_DATA>-BSART_EKKO
          LANGU      = LV_LANGU
          BSTYP      = <FS_DATA>-BSTYP_EKKO
          SW_DEST    = LV_SW_DEST
        IMPORTING
          TYPE_DESC  = <FS_DATA>-BATXT_EKKO
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
* Get PR Doc.Type desc.
    IF <FS_DATA>-BSART_EBAN IS NOT INITIAL AND <FS_DATA>-BSTYP_EBAN IS NOT INITIAL.
      "-- BSART_DESC
      CALL FUNCTION '/SKN/FC_SW_10_BSART_DESC'
        EXPORTING
          BSART      = <FS_DATA>-BSART_EBAN
          LANGU      = LV_LANGU
          BSTYP      = <FS_DATA>-BSTYP_EBAN
          SW_DEST    = LV_SW_DEST
        IMPORTING
          TYPE_DESC  = <FS_DATA>-BATXT_EBAN
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
*   "-- EKORG_DESC
    IF <FS_DATA>-EKORG IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
        EXPORTING
          EKORG        = <FS_DATA>-EKORG
          "LANGU              = lv_LANGU
        IMPORTING
          PUR_ORG_DESC = <FS_DATA>-EKOTX
        EXCEPTIONS
          WRONG_CODE   = 1
          OTHERS       = 2.
    ENDIF.
    IF <FS_DATA>-MATNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
        EXPORTING
          MATNR         = <FS_DATA>-MATNR
        IMPORTING
          MATERIAL_DESC = <FS_DATA>-MAKTX
        EXCEPTIONS
          WRONG_CODE    = 1
          OTHERS        = 2.
    ENDIF.
*   "-- EKGRP_DESC
    IF <FS_DATA>-EKGRP IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
        EXPORTING
          EKGRP        = <FS_DATA>-EKGRP
*         LANGU        = lv_LANGU
        IMPORTING
          PUR_GRP_DESC = <FS_DATA>-EKNAM
        EXCEPTIONS
          WRONG_CODE   = 1
          OTHERS       = 2.
    ENDIF.
    IF <FS_DATA>-USERNAME IS NOT INITIAL.
* Username (Username(CDHDR))
      CALL FUNCTION '/SKN/F_SW_10_GET_USER_DET_ENH'
        EXPORTING
          BNAME      = <FS_DATA>-USERNAME
        IMPORTING
          NAME_FIRST = <FS_DATA>-NAME_FIRST_USER
          NAME_LAST  = <FS_DATA>-NAME_LAST_USER
          NAME_TEXT  = <FS_DATA>-NAME_TEXT_USER
        EXCEPTIONS
          NO_DATA    = 1
          OTHERS     = 2.
    ENDIF.
    IF <FS_DATA>-ERNAM_EKKO IS NOT INITIAL.
* Username (EKKO-ERNAM)
      CALL FUNCTION '/SKN/F_SW_10_GET_USER_DET_ENH'
        EXPORTING
          BNAME      = <FS_DATA>-ERNAM_EKKO
        IMPORTING
          NAME_FIRST = <FS_DATA>-NAME_FIRST_EKKO
          NAME_LAST  = <FS_DATA>-NAME_LAST_EKKO
          NAME_TEXT  = <FS_DATA>-NAME_TEXT_EKKO
        EXCEPTIONS
          NO_DATA    = 1
          OTHERS     = 2.
    ENDIF.
    IF <FS_DATA>-ERNAM_EBAN IS NOT INITIAL.
* Username (EBAN-ERNAM)
      CALL FUNCTION '/SKN/F_SW_10_GET_USER_DET_ENH'
        EXPORTING
          BNAME      = <FS_DATA>-ERNAM_EBAN
        IMPORTING
          NAME_FIRST = <FS_DATA>-NAME_FIRST_EBAN
          NAME_LAST  = <FS_DATA>-NAME_LAST_EBAN
          NAME_TEXT  = <FS_DATA>-NAME_TEXT_EBAN
        EXCEPTIONS
          NO_DATA    = 1
          OTHERS     = 2.
    ENDIF.
  ENDLOOP.
*
**
***--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
