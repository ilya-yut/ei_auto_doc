### Parameter Configuration Guidelines

IMPORTANT: This EI defines 37 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Created on)

Changed-on date used to filter documents or master records by last maintenance activity.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BANFN** (Purchase Requisition)

Purchase requisition number, the core PR document key for MM approval and lifecycle checks.

**BEDAT** (Purchase Order Date)

Purchasing document date used to filter procurement documents by document creation period.

**BNFPO** (Item of Requisition)

Purchase requisition item number used to identify PR line-level records.

**BSART** (Purchasing Doc. Type)

Purchasing document type that controls PO/PR business scenario and approval behavior.

**BUKRS** (Company Code)

Company code key that scopes data to legal entity/accounting unit level.

**BWTAR** (Valuation Type)

Valuation type key used in split valuation scenarios (batch/material valuation layers).

**BWTTY** (Valuation Category)

Valuation category distinguishing split valuation types such as sales-order stock versus own stock.

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- BEDAT — Purchasing document date used to filter procurement documents by document creation period.
- AEDAT — Changed-on date used to filter documents or master records by last maintenance activity.
- EINDT — Item delivery date on purchasing or delivery structures expressing vendor-promised or requested receipt date.

**DATUM** (DATS)

Helps distinguish technical versus business attributes when dats on DATUM correlates with counters or status fields.

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

**EKGRP** (Purchasing Group)

Purchasing group (buyer) used for procurement ownership and control segmentation.

**EKORG** (Purch. Organization)

Purchasing organization key used to scope procurement flows by organizational unit.

**ELIKZ** (Delivery Completed)

Delivery completed indicator used to identify open versus completed procurement items.

**EREKZ** (Final Invoice)

Final invoice indicator on the PO item signaling that invoice completion is expected or locked for the line.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ESTKZ** (Creation Indicator)

Creation indicator for PR/PO source or method, used for process-origin analysis.

**FIPOS** (Commitment Item)

Commitment Item, which is an alphanumeric key used in Funds Management (FI-FM) to mirror the budget structure for specific revenues and expenditures.

**GRACEDAYS** (Days Grace)

For operations, days grace on GRACEDAYS indicates whether a row belongs in the current monitoring pass versus historical noise.

**KNTTP** (Acct Assignment Cat.)

Account assignment category on purchasing items telling whether stock is project, asset, cost-center, or sales-order.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (Vendor)

Vendor account number used to scope records to supplier-specific flows.

**LOEKZ** (Deletion Indicator)

Deletion indicator used to exclude logically deleted purchasing/material records.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**PSTYP** (Item Category)

Purchasing document item category controlling item behavior, account assignment, and goods-receipt rules.

**STATU** (Status)

Status code used for state-based filtering in process monitoring.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**UEBTK** (Unltd Overdelivery)

Unlimited overdelivery allowed indicator on SD or MM quantity contracts controlling tolerance behavior.

**UEBTO** (Overdeliv. Tolerance)

Overdelivery tolerance percent defining how much quantity overrun is accepted versus the order quantity.

**VBUND** (Trading Partner)

Trading partner/company field used for intercompany transaction analysis.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WEPOS** (Goods Receipt)

Goods-receipt indicator on purchasing history rows marking lines created by goods receipt postings.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.
