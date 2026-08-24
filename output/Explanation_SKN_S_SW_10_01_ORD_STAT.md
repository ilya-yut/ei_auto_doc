# Exception Indicator: SD Order Status (General) ( SW_10_01_ORD_STAT)

## General Overview

This Exception Indicator identifies sales orders whose overall processing, delivery, billing, and credit-check status fields match configured criteria, returning order header status from sales processing together with optional item detail and partner attributes when enabled.

This EI serves as an essential control for sales order operations by:

- Enabling detection of orders with overall or individual status values that require review
- Supporting monitoring of processing, delivery, billing, and confirmation status on flagged documents
- Providing visibility into organizational, customer, and partner context on each order
- Enabling age-based prioritization when orders remain in scope after a chosen reference date
- Supporting optional item-level expansion when detailed line review is required

Typical use includes general order status exception monitoring, blocked-order sampling, and periodic review of orders with specific status combinations before release or billing. Results are intended for exception workflows rather than operational order list reporting.

The routine reads sales order header data joined to overall document status, applies date-window and age-based filters, optionally expands item detail, enriches partner and customer description data, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor sales order status creates multiple risks across order fulfillment, billing, credit control, and customer service:

**Sales and Operations Risks**

- Orders with blocked or incomplete overall status can delay delivery, billing, or credit release without structured review
- Undetected concentration of status exceptions by customer or sales organization can leave backlog exposure unmanaged
- Individual status fields that remain in error or pending state can hide specific control gaps

**Operational Risks**

- Monitoring windows misaligned with order entry cadence can exclude recent exceptions or retain resolved cases
- Status filters that are too broad or too narrow can hide actionable orders or create reviewer fatigue
- Partner-role scope that is not tuned can mix irrelevant business partners into the review queue

**Control and Audit Risks**

- Weak order-status monitoring reduces evidence that flagged documents were reviewed before release decisions
- Lack of recurring exception review limits accountability for sales operations follow-up on stalled orders
- Missing customer and organizational context delays escalation of commercially significant cases

## Suggested Resolution

**Immediate Response**

- Review flagged orders for overall processing status, delivery and billing status, customer, and sales organization
- Confirm with sales or logistics whether the current status is correct or requires correction or release action
- Prioritize high-value customers and long-aged orders for immediate follow-up

**System Assessment**

- Validate lookback window, reference-date field, and age threshold settings against order review cadence
- Tune status, document type, and organizational scope so results stay actionable
- Compare exception counts by status type, sales organization, and customer to identify systematic gaps

**Corrective Actions**

- Resolve status blocks or update orders through standard SD processes where review confirms action is required
- Adjust monitoring scope after cleanup so results reflect truly exceptional status cases
- Document review outcomes and schedule recurring runs before order release or close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ABSTK | Rejection status | CHAR | 1 | 0 | ABSTK | STATV |
| 2 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 3 | ANGDT | Quotation valid from | DATS | 8 | 0 | ANGDT_V | DATUM |
| 4 | ARKTX | Description | CHAR | 40 | 0 | ARKTX | TEXT40 |
| 5 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 6 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 7 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 8 | BESTK | Confirmation status | CHAR | 1 | 0 | BESTK | STATV |
| 9 | BLOCK | BLOCK | CHAR | 1 | 0 | BLOCK_VB | BLOCK_VB |
| 10 | BNDDT | Quotation valid to | DATS | 8 | 0 | BNDDT | DATUM |
| 11 | BP1_CODE | Partner1 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 12 | BP1_FUNCT | Partner1 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 13 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 14 | BP2_CODE | Partner2 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 15 | BP2_FUNCT | Partner2 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 16 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 17 | BP3_CODE | Partner3 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 18 | BP3_FUNCT | Partner3 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 19 | BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 20 | CMGST | Overall status of credit check | CHAR | 1 | 0 | CMGST | CMGST |
| 21 | CMPS0 | Reserve | CHAR | 1 | 0 | CMPS0 | CMPSZ |
| 22 | CMPS1 | Reserve | CHAR | 1 | 0 | CMPS1 | CMPSZ |
| 23 | CMPS2 | Reserve | CHAR | 1 | 0 | CMPS2 | CMPSZ |
| 24 | CMPS_CM | Credit Check SAP Credit Manag | CHAR | 1 | 0 | CMPS_CM | CMPSZ |
| 25 | CMPS_TE | Technic Error SAP Credit Manag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 26 | CMPSA | Static check | CHAR | 1 | 0 | CMPSA | CMPSZ |
| 27 | CMPSB | Dynamic check | CHAR | 1 | 0 | CMPSB | CMPSZ |
| 28 | CMPSC | Maximum value | CHAR | 1 | 0 | CMPSC | CMPSZ |
| 29 | CMPSD | Terms of payment | CHAR | 1 | 0 | CMPSD | CMPSZ |
| 30 | CMPSE | Customer review date | CHAR | 1 | 0 | CMPSE | CMPSZ |
| 31 | CMPSF | Overdue open items | CHAR | 1 | 0 | CMPSF | CMPSZ |
| 32 | CMPSG | Oldest open items | CHAR | 1 | 0 | CMPSG | CMPSZ |
| 33 | CMPSH | Max.dunning level | CHAR | 1 | 0 | CMPSH | CMPSZ |
| 34 | CMPSI | Financial document | CHAR | 1 | 0 | CMPSI | CMPSZ |
| 35 | CMPSJ | Expt cred. insurance | CHAR | 1 | 0 | CMPSJ | CMPSZ |
| 36 | CMPSK | Payment card | CHAR | 1 | 0 | CMPSK | CMPSZ |
| 37 | CMPSL | Reserve | CHAR | 1 | 0 | CMPSL | CMPSZ |
| 38 | CMPSM | Credit check data is obsolete | CHAR | 1 | 0 | CMPSM | CMPSZ |
| 39 | COSTA | Confirmation status for ALE | CHAR | 1 | 0 | COSTA_D | COSTA |
| 40 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 41 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 42 | DCSTK | Delay status | CHAR | 1 | 0 | DCSTK | STATV |
| 43 | DUMMY | Single-Character Indicator | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 44 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 45 | DURATION_D | Duration In Days | NUMC | 6 | 0 | /SKN/E_SW_DURATION_D |  |
| 46 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 47 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 48 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 49 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 50 | FAKSK | Billing block | CHAR | 2 | 0 | FAKSK | FAKSP |
| 51 | FKIVK | Bill tot. stat  intercomp bill | CHAR | 1 | 0 | FKIVK | STATV |
| 52 | FKSAK | Bill.status (order-related) | CHAR | 1 | 0 | FKSAK | STATV |
| 53 | FORWDAYS | Forward Days |  | 0 | 0 |  |  |
| 54 | FSSTK | Overall billing block status | CHAR | 1 | 0 | FSSTK | STATV |
| 55 | GBSTK | Overall processing status | CHAR | 1 | 0 | GBSTK | STATV |
| 56 | GUEBG | Valid-from date | DATS | 8 | 0 | GUEBG | DATUM |
| 57 | GUEEN | Valid-to date | DATS | 8 | 0 | GUEEN | DATUM |
| 58 | ITEM_DETAILS | 'X' - Get Item Details |  | 0 | 0 |  |  |
| 59 | KBMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KBMENG | MENG15 |
| 60 | KLMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KLMENG | MENG15 |
| 61 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 62 | KWMENG | Order Quantity | QUAN | 15 | 3 | KWMENG | MENG15 |
| 63 | KWMENG_INT | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 64 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 65 | LFGSK | Overall dlv.status (all items) | CHAR | 1 | 0 | LKGSK | STATV |
| 66 | LFSTK | Delivery status | CHAR | 1 | 0 | LFSTK | STATV |
| 67 | LIFSK | Delivery block | CHAR | 2 | 0 | LIFSK | LIFSP |
| 68 | LSMENG | Required deliv. qty | QUAN | 15 | 3 | LSMENG | MENG15 |
| 69 | LSSTK | Overal. dlv. blk stat. | CHAR | 1 | 0 | LSSTK_G | STATV |
| 70 | MANAGE_IN_UTC | CHAR | 1 | 0 |  | XFELD |  |
| 71 | MANEK | Manual Completion of Contract | CHAR | 1 | 0 | MANEK | MANEK |
| 72 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 73 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 74 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 75 | MPROK_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 76 | MWSBP | Tax amount | CURR | 13 | 2 | MWSBP | WERTV7 |
| 77 | NETPR | Net price | CURR | 11 | 2 | NETPR | WERTV6 |
| 78 | NETPR_VAT | Net price | CURR | 11 | 2 | NETPR | WERTV6 |
| 79 | NETWR | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 80 | NETWR_VAT | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 81 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 82 | RFGSK | Total ref. stat (all items) | CHAR | 1 | 0 | RFGSK | STATV |
| 83 | RFSTK | Reference document header stat | CHAR | 1 | 0 | RFSTK | STATV |
| 84 | RRSTA | Rev. determ. status | CHAR | 1 | 0 | RR_STATUS | STATV |
| 85 | SAPRL | SAP Release | CHAR | 4 | 0 | SAPRL | SAPRL |
| 86 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 87 | SPSTG | Overall blkd status | CHAR | 1 | 0 | SPSTG | STATV |
| 88 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 89 | TRSTA | Transportation planning status | CHAR | 1 | 0 | TRSTA | TRSTA |
| 90 | UVALL | General incompl. stat(header) | CHAR | 1 | 0 | UVALL_UK | STATV |
| 91 | UVALS | Total incomplet. stat general | CHAR | 1 | 0 | UVALL_SU | STATV |
| 92 | UVFAK | Header incompl. stat. Billing | CHAR | 1 | 0 | UVFAK_UK | STATV |
| 93 | UVFAS | Tot. incomplet. stat Billing | CHAR | 1 | 0 | UVFAK_SU | STATV |
| 94 | UVK01 | Header reserves 1 | CHAR | 1 | 0 | UVK01 | STATV |
| 95 | UVK02 | Header reserves 2 | CHAR | 1 | 0 | UVK02 | STATV |
| 96 | UVK03 | Header reserves 3 | CHAR | 1 | 0 | UVK03 | STATV |
| 97 | UVK04 | Header reserves 4 | CHAR | 1 | 0 | UVK04 | STATV |
| 98 | UVK05 | Header reserves 5 | CHAR | 1 | 0 | UVK05 | STATV |
| 99 | UVPAK | Header incomp stat -packaging | CHAR | 1 | 0 | UVPAK_UK | STATV |
| 100 | UVPIK | Head. incomp. picking/putaway | CHAR | 1 | 0 | UVPIK_UK | STATV |
| 101 | UVPRS | Document is incompl.-pricing | CHAR | 1 | 0 | UVPRS_UK | STATV |
| 102 | UVS01 | Total reserves 1 | CHAR | 1 | 0 | UVS01 | STATV |
| 103 | UVS02 | Total reserves 2 | CHAR | 1 | 0 | UVS02 | STATV |
| 104 | UVS03 | Total reserves 3 | CHAR | 1 | 0 | UVS03 | STATV |
| 105 | UVS04 | Total reserves 4 | CHAR | 1 | 0 | UVS04 | STATV |
| 106 | UVS05 | Total reserves 5 | CHAR | 1 | 0 | UVS05 | STATV |
| 107 | UVVLK | Header incompl. stat -delivery | CHAR | 1 | 0 | UVVLK_UK | STATV |
| 108 | UVVLS | Tot. incomplet. stat. Delivery | CHAR | 1 | 0 | UVVLS_SU | STATV |
| 109 | UVWAK | Post Head. incomp goods mvmt | CHAR | 1 | 0 | UVWAK_UK | STATV |
| 110 | UVWAS | Tot  incomp Item post gds mvmt | CHAR | 1 | 0 | UVWAK_SU | STATV |
| 111 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 112 | VBOBJ | Document object | CHAR | 1 | 0 | VBOBJ | VBOBJ |
| 113 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 114 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 115 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 116 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 117 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 118 | VRKME | Sales unit | UNIT | 3 | 0 | VRKME | MEINS |
| 119 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 120 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 121 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 121 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ABSTK** (Rejection status)

Overall Rejection Status represents the processing state of a sales document item that indicates whether all or part of the items have been rejected or cancelled.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**ANGDT** (Quotation valid from)

Quotation Valid From represents the exact calendar date when the pricing conditions, terms, and delivery commitments in a sales quotation become legally effective for the customer.

**ARKTX** (Description)

Short text for a manufacturing order component or BOM line (material description at order-component level).

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**AUDAT** (Document Date)

Sales document date (order date) used for period-based SD selection.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BESTK** (Confirmation status)

Confirmation Status represents the overall processing state of a sales document or purchasing contract indicating whether the schedule lines or item quantities have been officially confirmed by the supplying party.

**BLOCK** (BLOCK)

Blocking indicator showing whether the record is restricted for posting/processing.

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

**CMGST** (Overall status of credit check)

Credit-management overall status summarizing credit exposure processing for the business partner or document.

**CMPS0 - CMPS2** (Reserve)

Customer Reserve 1 acts as a customizable status indicator used to hold the evaluation results of user-defined, custom credit check logic programmed in system enhancements.

**CMPS_CM** (Credit Check SAP Credit Manag)

Status of SAP Credit Management Check stores the central, consolidated evaluation result transmitted back from the advanced SAP S/4HANA Credit Management engine (FSCM).

**CMPS_TE** (Technic Error SAP Credit Manag)

Status of Technical Error records whether an underlying system communication failure or data connectivity issue interrupted the SAP Credit Management check, blocking document processing.

**CMPSA** (Static check)

Status of static credit limit check; flags if the order value pushes the customer's total liability past their hard credit limit threshold.

**CMPSB** (Dynamic check)

Status of dynamic credit limit check; flags if the order value exceeds the credit limit within a specific, configurable time horizon window.

**CMPSC** (Maximum value)

Status of credit check against maximum document value; flags if a single sales document's net value exceeds the maximum limit allowed per order.

**CMPSD** (Terms of payment)

Terms of Payment Check evaluates whether critical payment conditions or fixed value dates have been manually modified from the master data defaults to bypass standard financial rules.

**CMPSE** (Customer review date)

Next Customer Review Date Check evaluates whether the transaction was created after the customer's master data credit review date has expired, requiring an updated account assessment.

**CMPSF** (Overdue open items)

Overdue Open Items Ratio Check evaluates whether the total amount of overdue open invoices exceeds the maximum allowed percentage of the customer's total overall receivables.

**CMPSG** (Oldest open items)

Oldest Open Item Check evaluates whether the customer has any outstanding invoice that has remained unpaid past the maximum number of allowable overdue days.

**CMPSH** (Max.dunning level)

Maximum Dunning Level Check evaluates whether the customer has open invoices that have reached a critical or maximum dunning stage, triggering an automatic transactional block.

**CMPSI** (Financial document)

Financial Requirements Check evaluates whether the sales document satisfies external security requirements, such as verifying the validity and value limits of a bank guarantee or letter of credit.

**CMPSJ** (Expt cred. insurance)

Export Credit Insurance Check evaluates whether the transaction value is successfully covered under the parameters and maximum limits of an active export credit insurance policy.

**CMPSK** (Payment card)

Payment Card Authorization Check evaluates whether a transaction utilizing a credit card has successfully secured a financial authorization code from the clearinghouse.

**CMPSL** (Reserve)

Status of Credit Check for Customer Reserve serves as a customizable reserve status indicator to hold specific custom or localized credit validation rules.

**CMPSM** (Credit check data is obsolete)

Credit check data is obsolete tracks whether the system's evaluated credit information has expired or is no longer considered valid based on the configuration timeframes.

**COSTA** (Confirmation status for ALE)

Confirmation/status indicator used to distinguish processing completion states.

**CUST_DESC** (Name)

Customer description/name text used for readable customer-level reporting.

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- AEDAT — Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.
- AUDAT — Sales document date (order date) used for period-based SD selection.
- VDATU — Requested/validity date used for schedule and due-date based filtering.
- GUEBG — Valid-from Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.
- GUEEN — Valid-to Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.
- ANGDT — Quotation Valid From represents the exact calendar date when the pricing conditions, terms, and delivery commitments in a sales quotation become legally effective for the customer.
- BNDDT — Quotation Valid To represents the exact calendar date until which the pricing conditions, delivery terms, and material commitments defined in a sales quotation remain legally binding for the customer.

**DCSTK** (Delay status)

Delay Status represents the overall processing state of a sales document indicating whether delivery or shipping execution has been delayed beyond the planned scheduling dates.

**DUMMY** (Single-Character Indicator)

Placeholder single-character field on the order status structure; not used for selection in this monitor.

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

**FAKSK** (Billing block)

Billing block key on SD billing-relevant objects preventing invoicing until the block is removed.

**FKIVK** (Bill tot. stat  intercomp bill)

Intercompany billing variant or billing-type control key on SD billing headers for IC scenarios.

**FKSAK** (Bill.status (order-related))

Billing Status for Order-Related Billing Documents represents the header-level processing state of a sales document that indicates whether all items requiring direct invoicing from the order have been fully billed.

**FORWDAYS** (Forward Days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

Forwdays is based on DATE_REF_FLD field.

**Not in use**
**FSSTK** (Overall billing block status)

Overall Billing Block Status tracks whether an entire document contains any active billing blocks at the header or item level, indicating if the document is released for invoicing.

**GBSTK** (Overall processing status)

Overall Processing Status indicates the cumulative progress of a document, tracking whether it is open, in process, or completely finished based on subsequent activities.

**GUEBG** (Valid-from date)

Valid-from Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.

**GUEEN** (Valid-to date)

Valid-to Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.

**ITEM_DETAILS** ('X' - Get Item Details)

When set, expands results with matching sales order item lines including material, quantity, and value fields from order items.

**KBMENG** (Cumul.confirmed qty)

Cumulative Order Quantity stores the total accumulated quantity of a specific item that a customer has ordered across multiple partial deliveries, allowing the system to track remaining open quantities.

**KLMENG** (Cumul.confirmed qty)

Cumulative schedule or order quantity in the sales item context-confirmed or requested quantity accumulated on schedules.

**KUNNR** (Customer)

Customer account is used to scope records to specific customers across SD/FI flows.

**KWMENG** (Order Quantity)

Cumulative order quantity in sales units on the item-commercial ordered quantity for SD lines.

**KWMENG_INT** (Natural Number)

Internal Order Quantity stores the calculated order quantity formatted as an integer or processed in an internal numeric format for backend program calculations and system validation.

**LANGU** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**LFGSK** (Overall dlv.status (all items))

Overall Delivery Status for All Items aggregates and tracks the cumulative shipping progress across every deliverable line item in a sales document, indicating whether the entire order is outstanding, partially shipped, or fully completed.

**LFSTK** (Delivery status)

Connects to alert semantics: rows removed for failing delivery status on LFSTK never reach downstream filtering.

**LIFSK** (Delivery block)

Delivery Block stores the central configuration key used to withhold or prevent an entire sales document or specific item from being processed for delivery, usually due to credit limits, political checks, or logistical constraints.

**LSMENG** (Required deliv. qty)

Cumulative Required Quantity in Sales Units stores the total target quantity of an item from a scheduling agreement or contract, expressed in the sales unit of measure, used to track cumulative required quantities against actual delivery performance.

**LSSTK** (Overal. dlv. blk stat.)

Overall Delivery Block Status evaluates whether any active delivery restrictions exist at either the header, item, or schedule line level, determining if the document as a whole is officially blocked or released for outbound shipping processing.

**MANAGE_IN_UTC** (CHAR)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MANEK** (Manual Completion of Contract)

Manual Completion of Contract represents the indicator that determines whether an purchasing contract item can be manually marked as closed or fully processed even if the target quantity or value has not been completely reached.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MEINS** (Base Unit of Measure)

Base unit of measure used to interpret quantity fields consistently.

**MPROK** (Manual price)

Material/procurement status key used to identify control-relevant status states.

**MPROK_DESC** (Short text)

Description of material/procurement status for readable reporting.

**MWSBP** (Tax amount)

Tax Amount in Document Currency stores the calculated total tax value for a sales document line item or invoice, expressed in the currency specified for that document.

**NETPR** (Net price)

Net Price is primarily used at the item level in purchasing documents (such as Purchase Orders, Scheduling Agreements, and Info Records) to denote the price per unit of material.

**NETPR_VAT** (Net price)

Net Price Including Value Added Tax calculates and displays the unit price of an item with the applicable tax rates or VAT percentages factored directly into the net rate.

**NETWR** (Net value)

Net value amount used for commercial threshold and anomaly checks.

**NETWR_VAT** (Net value)

Net value Including Value Added Tax stores the accumulated total monetary value of an item or document after all item discounts are applied and with the total VAT amount fully included.

**POSNR** (Sales Document Item)

Document item number used for line-level drilldown and joins.

**RFGSK** (Total ref. stat (all items))

Total Reference Status for All Items aggregates and tracks the cumulative copying or reference progress across every line item in a sales document, indicating whether the entire order has been fully transferred into subsequent documents like deliveries or invoices.

**RFSTK** (Reference document header stat)

Reference Document Header Status indicates whether a preceding document-such as a quotation or inquiry-has been successfully and completely referenced or copied into the current sales order header.

**RRSTA** (Rev. determ. status)

Revenue Determination Status tracks the progress of revenue recognition for a document, indicating whether revenue recognition rules have been applied, partially executed, or fully completed for accounting purposes.

**SAPRL** (SAP Release)

SAP Release represents the specific version or software modification level of the SAP system currently in use.

**SPART** (Division)

Division key used for SD product-line segmentation.

**SPSTG** (Overall blkd status)

Overall Blocked Status evaluates whether a document is withheld from further processing by aggregating the statuses of all active credit blocks, delivery blocks, and billing blocks across the entire transaction.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TRSTA** (Transportation planning status)

Transport status code on TRKORR-style requests summarizing whether the request is released or protected.

**UVALL** (General incompl. stat(header))

General Incompletion Status for Header indicates whether any critical general data fields are missing at the document header level, restricting subsequent processing until the mandatory information is provided.

**UVALS** (Total incomplet. stat general)

Total Incompletion Status General aggregates the completion state of all general data fields across both the header and individual items, confirming if the document is entirely complete.

**UVFAK** (Header incompl. stat. Billing)

Header Incompletion Status for Billing indicates whether mandatory billing-related data is missing from the document header, preventing the transaction from being invoiced until resolved.

**UVFAS** (Tot. incomplet. stat Billing)

Total Incompletion Status for Billing aggregates the billing readiness across both header data and individual items, flagging whether any missing financial or tax information is blocking downstream invoice creation.

**UVK01 - UVK05** (Header reserves 1)

Customer Reserve 1: Header Status acts as a customizable status indicator used to hold the evaluation results of user-defined, custom credit or incompletion check logic programmed via system enhancements.

**UVPAK** (Header incomp stat -packaging)

Header Incompletion Status for Packaging indicates whether mandatory packing instructions or container details are missing from the document header, preventing the creation of outbound logistics paperwork.

**UVPIK** (Head. incomp. picking/putaway)

Header Incompletion Status for Picking or Putaway tracks whether critical storage location or warehouse movement data is missing from the document header, halting immediate warehouse fulfillment actions.

**UVPRS** (Document is incompl.-pricing)

Document Incompletion Status for Pricing indicates whether essential price conditions, currency codes, or valuation factors are missing or invalid within the document, blocking downstream billing and financial posting.

**UVS01 - UVS05** (Total reserves 1)

Customer Reserve 1: Item Status acts as a customizable status indicator used to hold the evaluation results of user-defined, custom credit or incompletion check logic programmed at the line item level.

**UVVLK** (Header incompl. stat -delivery)

Header Incompletion Status for Delivery indicates whether mandatory shipping or logistical information is missing from the document header, preventing the creation of a outbound delivery document.

**UVVLS** (Tot. incomplet. stat. Delivery)

Total Incompletion Status for Delivery aggregates the delivery readiness across both header data and individual line items, checking if missing shipping details are blocking outbound delivery creation.

**UVWAK** (Post Head. incomp goods mvmt)

Header Incompletion Status for Goods Movement tracks whether critical data required for the goods issue or goods receipt process-such as accounting or plant indicators-is missing from the document header.

**UVWAS** (Tot  incomp Item post gds mvmt)

Total Incompletion Status for Goods Movement aggregates the goods movement readiness across both header data and individual line items, flagging whether any missing parameters are blocking inventory updates.

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBOBJ** (Document object)

SD Document Category Object classifies the specific business entity or transactional module type-such as a sales order, inquiry, quotation, or delivery-to control the data validation and processing logic applied to the record.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VDATU** (Requested deliv.date)

Requested/validity date used for schedule and due-date based filtering.

**VKBUR** (Sales Office)

Sales office key used for organizational SD segmentation.

**VKGRP** (Sales Group)

Sales group key used for team-level SD analytics.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VRKME** (Sales unit)

Sales unit of measure for the material in SD documents-unit for commercial sales quantities.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.

**WAERK** (Document Currency)

Document currency key used for value analysis in transaction currency.

**WAVWR** (Cost)

Statistical value amount field used for value-based exception thresholds.

### Parameter Relationships

**Lookback window:** When no explicit date range is supplied on individual date fields, **BACKDAYS** builds a lookback window from the current day before orders are read.

**Reference date mapping:** **DATE_REF_FLD** directs the lookback window to the created-on, changed-on, document, requested delivery, or contract and quotation validity dates. When **DATE_REF_FLD** is initial and a single **VBTYP** value is supplied, the reference field defaults to **GUEBG** for contracts or **ANGDT** for quotations.

**Status selection:** **GBSTK**, **RFSTK**, **LFSTK**, **FKSAK**, **ABSTK**, **BESTK**, **CMGST**, **CMPSA**–**CMPSM**, **BLOCK**, **LIFSK**, **FAKSK**, and related overall status fields filter orders by processing, delivery, billing, and credit-check state from the sales document header status.

**Age filter:** After rows are selected, elapsed time from each row's reference date to the evaluation time is calculated using **DURATION_UNIT** and stored in **DURATION**; rows outside the configured duration range are removed.

**Partner roles:** **BP1_FUNCT** / **BP1_CODE**, **BP2_FUNCT** / **BP2_CODE**, and **BP3_FUNCT** / **BP3_CODE** work together to enrich and filter business partner attributes on each order.

**Item detail:** When **ITEM_DETAILS** is set, matching sales order item rows are appended with material, quantity, and value fields from order items.

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper and the on-premise path below that call is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code

### Practical Example of Parameter Configuration

**Use Case 1: Incomplete overall processing**

**Purpose:** Review orders with overall processing status indicating incomplete processing in one sales organization.

```
GBSTK = B
VKORG = 1000
VTWEG = 10
BACKDAYS = 7
```

**Use Case 2: Delivery status focus**

**Purpose:** Monitor orders with delivery status indicating items not fully delivered.

```
LFSTK = B
SPART = 01
VKORG = 1000
KUNNR = 100000
```

**Use Case 3: Billing block status**

**Purpose:** Sample orders with overall billing block status requiring follow-up.

```
FSSTK = A
AUART = TA
VKORG = 1000
BACKDAYS = 14
```

**Use Case 4: Sold-to partner with status exception**

**Purpose:** Review orders for one sold-to partner with a specific overall rejection status.

```
BP1_FUNCT = AG
BP1_CODE = 100000
ABSTK = A
VKORG = 1000
```

**Use Case 5: Exactly seven full days since created-on date**

**Purpose:** Return rows whose created-on reference date is exactly 7 full days ago for weekly follow-up.

```
DURATION = 7
DURATION_UNIT = F
DATE_REF_FLD = ERDAT
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_ORD_STAT | ABSTK | Rejection status | CHAR(1) | ABSTK |
| /SKN/S_SW_10_01_ORD_STAT | AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_ORD_STAT | ANGDT | Quotation valid from | DATS(8) | ANGDT_V |
| /SKN/S_SW_10_01_ORD_STAT | ARKTX | Description | CHAR(40) | ARKTX |
| /SKN/S_SW_10_01_ORD_STAT | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_STAT | AUDAT | Document Date | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_ORD_STAT | BESTK | Confirmed | CHAR(1) | BESTK |
| /SKN/S_SW_10_01_ORD_STAT | BLOCK | Indicator: Document preselected for archiving | CHAR(1) | BLOCK_VB |
| /SKN/S_SW_10_01_ORD_STAT | BNDDT | Quotation valid to | DATS(8) | BNDDT |
| /SKN/S_SW_10_01_ORD_STAT | BP1_CODE | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_STAT | BP1_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_STAT | BP1_NAME | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_STAT | BP2_CODE | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_STAT | BP2_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_STAT | BP2_NAME | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_STAT | BP3_CODE | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_STAT | BP3_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_STAT | BP3_NAME | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_STAT | CMGST | Overall CreditStatus | CHAR(1) | CMGST |
| /SKN/S_SW_10_01_ORD_STAT | CMPS0 | Reserve | CHAR(1) | CMPS0 |
| /SKN/S_SW_10_01_ORD_STAT | CMPS1 | Reserve | CHAR(1) | CMPS1 |
| /SKN/S_SW_10_01_ORD_STAT | CMPS2 | Reserve | CHAR(1) | CMPS2 |
| /SKN/S_SW_10_01_ORD_STAT | CMPSA | Static check | CHAR(1) | CMPSA |
| /SKN/S_SW_10_01_ORD_STAT | CMPSB | Dynamic check | CHAR(1) | CMPSB |
| /SKN/S_SW_10_01_ORD_STAT | CMPSC | Maximum value | CHAR(1) | CMPSC |
| /SKN/S_SW_10_01_ORD_STAT | CMPSD | Terms of payment | CHAR(1) | CMPSD |
| /SKN/S_SW_10_01_ORD_STAT | CMPSE | Customer review date | CHAR(1) | CMPSE |
| /SKN/S_SW_10_01_ORD_STAT | CMPSF | Overdue open items | CHAR(1) | CMPSF |
| /SKN/S_SW_10_01_ORD_STAT | CMPSG | Oldest open items | CHAR(1) | CMPSG |
| /SKN/S_SW_10_01_ORD_STAT | CMPSH | Max.dunning level | CHAR(1) | CMPSH |
| /SKN/S_SW_10_01_ORD_STAT | CMPSI | Financial document | CHAR(1) | CMPSI |
| /SKN/S_SW_10_01_ORD_STAT | CMPSJ | Expt cred. insurance | CHAR(1) | CMPSJ |
| /SKN/S_SW_10_01_ORD_STAT | CMPSK | Payment card | CHAR(1) | CMPSK |
| /SKN/S_SW_10_01_ORD_STAT | CMPSL | Reserve | CHAR(1) | CMPSL |
| /SKN/S_SW_10_01_ORD_STAT | CMPSM | Obsolete credit data | CHAR(1) | CMPSM |
| /SKN/S_SW_10_01_ORD_STAT | CMPS_CM | SAP Credit Management | CHAR(1) | CMPS_CM |
| /SKN/S_SW_10_01_ORD_STAT | CMPS_TE | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_01_ORD_STAT | COSTA | Confirmation status | CHAR(1) | COSTA_D |
| /SKN/S_SW_10_01_ORD_STAT | CUST_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_STAT | DCSTK | Delay status | CHAR(1) | DCSTK |
| /SKN/S_SW_10_01_ORD_STAT | DUMMY | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_01_ORD_STAT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ORD_STAT | DURATION_D | Duration In Days | NUMC(6) | /SKN/E_SW_DURATION_D |
| /SKN/S_SW_10_01_ORD_STAT | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ORD_STAT | ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_ORD_STAT | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_ORD_STAT | ERZET | Time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_ORD_STAT | FAKSK | Billing block | CHAR(2) | FAKSK |
| /SKN/S_SW_10_01_ORD_STAT | FKIVK | Totals status | CHAR(1) | FKIVK |
| /SKN/S_SW_10_01_ORD_STAT | FKSAK | Bill.stat.order-rel. | CHAR(1) | FKSAK |
| /SKN/S_SW_10_01_ORD_STAT | FSSTK | Overall block status | CHAR(1) | FSSTK |
| /SKN/S_SW_10_01_ORD_STAT | GBSTK | Overall status | CHAR(1) | GBSTK |
| /SKN/S_SW_10_01_ORD_STAT | GUEBG | Valid-from date | DATS(8) | GUEBG |
| /SKN/S_SW_10_01_ORD_STAT | GUEEN | Valid-to date | DATS(8) | GUEEN |
| /SKN/S_SW_10_01_ORD_STAT | KBMENG | Cumul.confirmed qty | QUAN(15) | KBMENG |
| /SKN/S_SW_10_01_ORD_STAT | KLMENG | Cumul.confirmed qty | QUAN(15) | KLMENG |
| /SKN/S_SW_10_01_ORD_STAT | KUNNR | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_STAT | KWMENG | Order Quantity | QUAN(15) | KWMENG |
| /SKN/S_SW_10_01_ORD_STAT | KWMENG_INT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_01_ORD_STAT | LFGSK | Overall dlv.status | CHAR(1) | LKGSK |
| /SKN/S_SW_10_01_ORD_STAT | LFSTK | Delivery status | CHAR(1) | LFSTK |
| /SKN/S_SW_10_01_ORD_STAT | LIFSK | Delivery block | CHAR(2) | LIFSK |
| /SKN/S_SW_10_01_ORD_STAT | LSMENG | Required deliv. qty | QUAN(15) | LSMENG |
| /SKN/S_SW_10_01_ORD_STAT | LSSTK | Over. dlv. blk stat. | CHAR(1) | LSSTK_G |
| /SKN/S_SW_10_01_ORD_STAT | MANEK | Manual Completion of Contract | CHAR(1) | MANEK |
| /SKN/S_SW_10_01_ORD_STAT | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_ORD_STAT | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_01_ORD_STAT | MPROK | Manual price | CHAR(1) | MPROK |
| /SKN/S_SW_10_01_ORD_STAT | MPROK_DESC | Short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_10_01_ORD_STAT | MWSBP | Tax amount | CURR(13) | MWSBP |
| /SKN/S_SW_10_01_ORD_STAT | NETPR | Net price | CURR(11) | NETPR |
| /SKN/S_SW_10_01_ORD_STAT | NETPR_VAT | Net price | CURR(11) | NETPR |
| /SKN/S_SW_10_01_ORD_STAT | NETWR | Net value | CURR(15) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_STAT | NETWR_VAT | Net value | CURR(15) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_STAT | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_ORD_STAT | RFGSK | Total reference stat | CHAR(1) | RFGSK |
| /SKN/S_SW_10_01_ORD_STAT | RFSTK | Reference status | CHAR(1) | RFSTK |
| /SKN/S_SW_10_01_ORD_STAT | RRSTA | Rev. determ. status | CHAR(1) | RR_STATUS |
| /SKN/S_SW_10_01_ORD_STAT | SAPRL | SAP Release | CHAR(4) | SAPRL |
| /SKN/S_SW_10_01_ORD_STAT | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_STAT | SPSTG | Overall blkd status | CHAR(1) | SPSTG |
| /SKN/S_SW_10_01_ORD_STAT | TRSTA | Trns.plan.status | CHAR(1) | TRSTA |
| /SKN/S_SW_10_01_ORD_STAT | UVALL | Header data | CHAR(1) | UVALL_UK |
| /SKN/S_SW_10_01_ORD_STAT | UVALS | Item data | CHAR(1) | UVALL_SU |
| /SKN/S_SW_10_01_ORD_STAT | UVFAK | Header billing data | CHAR(1) | UVFAK_UK |
| /SKN/S_SW_10_01_ORD_STAT | UVFAS | Item billing data... | CHAR(1) | UVFAK_SU |
| /SKN/S_SW_10_01_ORD_STAT | UVK01 | Header reserves 1 | CHAR(1) | UVK01 |
| /SKN/S_SW_10_01_ORD_STAT | UVK02 | Header reserves 2 | CHAR(1) | UVK02 |
| /SKN/S_SW_10_01_ORD_STAT | UVK03 | Header reserves 3 | CHAR(1) | UVK03 |
| /SKN/S_SW_10_01_ORD_STAT | UVK04 | Header reserves 4 | CHAR(1) | UVK04 |
| /SKN/S_SW_10_01_ORD_STAT | UVK05 | Header reserves 5 | CHAR(1) | UVK05 |
| /SKN/S_SW_10_01_ORD_STAT | UVPAK | Head.data packaging | CHAR(1) | UVPAK_UK |
| /SKN/S_SW_10_01_ORD_STAT | UVPIK | Head. data picking/putaway | CHAR(1) | UVPIK_UK |
| /SKN/S_SW_10_01_ORD_STAT | UVPRS | Pricing | CHAR(1) | UVPRS_UK |
| /SKN/S_SW_10_01_ORD_STAT | UVS01 | Total reserves 1 | CHAR(1) | UVS01 |
| /SKN/S_SW_10_01_ORD_STAT | UVS02 | Total reserves 2 | CHAR(1) | UVS02 |
| /SKN/S_SW_10_01_ORD_STAT | UVS03 | Total reserves 3 | CHAR(1) | UVS03 |
| /SKN/S_SW_10_01_ORD_STAT | UVS04 | Total reserves 4 | CHAR(1) | UVS04 |
| /SKN/S_SW_10_01_ORD_STAT | UVS05 | Total reserves 5 | CHAR(1) | UVS05 |
| /SKN/S_SW_10_01_ORD_STAT | UVVLK | Header delivery data | CHAR(1) | UVVLK_UK |
| /SKN/S_SW_10_01_ORD_STAT | UVVLS | Item delivery data.. | CHAR(1) | UVVLS_SU |
| /SKN/S_SW_10_01_ORD_STAT | UVWAK | Head. data goods mvmt | CHAR(1) | UVWAK_UK |
| /SKN/S_SW_10_01_ORD_STAT | UVWAS | Item data: goods mvmt | CHAR(1) | UVWAK_SU |
| /SKN/S_SW_10_01_ORD_STAT | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_STAT | VBOBJ | Document object | CHAR(1) | VBOBJ |
| /SKN/S_SW_10_01_ORD_STAT | VBTYP | SD document categ. | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_STAT | VDATU | Requested deliv.date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_ORD_STAT | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_ORD_STAT | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_ORD_STAT | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_STAT | VRKME | Sales unit | UNIT(3) | VRKME |
| /SKN/S_SW_10_01_ORD_STAT | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_STAT | WAERK | Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_STAT | WAVWR | Cost | CURR(13) | WAVWR |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ORD_STAT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_STAT OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
             LANGU  LANGU,
             BACKDAYS INT4,
             FORWDAYS INT4,
             BP1_FUNCT   PARVW,
             BP2_FUNCT   PARVW,
             BP3_FUNCT   PARVW,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             ITEM_DETAILS   CHAR1 .
 LV_BACKDAYS = 1.
 LV_DURATION_UNIT = 'D'.
 LV_LANGU = SY-LANGU. """27-5-19
 """lv_DATE_REF_FLD = 'ERDAT'."Creation date   'AUDAT'. "Document Date (Date Received/Sent)
 SELECT_SINGLE: MANAGE_IN_UTC,
                LANGU,
                BACKDAYS,
                FORWDAYS,
                BP1_FUNCT,
                BP2_FUNCT,
                BP3_FUNCT,
                DATE_REF_FLD,
                DURATION_UNIT,
                ITEM_DETAILS.
DATA_MULTY: KUNNR        VBAK-KUNNR,
            VBELN        VBAK-VBELN,
            VKORG        VBAK-VKORG,
            VTWEG        VBAK-VTWEG,
            SPART        VBAK-SPART,
            VBTYP        VBTYP,
            AUART        VBAK-AUART,
            ERDAT        VBAK-ERDAT,
            AUDAT        VBAK-AUDAT,
            AEDAT        VBAK-AEDAT,
            GUEBG        GUEBG, "Valid-from date contract
            GUEEN        GUEEN, "Valid-to date contract
            ANGDT        ANGDT_V, "Valid-from date quatetion
            BNDDT        BNDDT, "Valid-to date quatetion
            VDATU        VBAK-VDATU, "Requested delivery date
            ERNAM        VBAK-ERNAM,
            DATUM        SY-DATUM,
"            DURATION_M   /SKN/E_SW_DURATION_M,
"            DURATION_H   /SKN/E_SW_DURATION_H,
"            DURATION_D   /SKN/E_SW_DURATION_D,
            DURATION    /SKN/E_SW_DURATION,
            RFSTK       RFSTK,
            RFGSK       RFGSK,
            BESTK       BESTK,
            LFSTK       LFSTK,
            LFGSK       LKGSK,
            FKSAK       FKSAK,
            ABSTK       ABSTK,
            GBSTK       GBSTK,
            UVALS       UVVLS_SU,
            UVVLS       UVVLS_SU,
            UVFAS       UVFAK_SU,
            UVALL       UVALL_UK,
            UVVLK       UVVLK_UK,
            UVFAK       UVFAK_UK,
            UVPRS       UVPRS_UK,
            CMPSA       CMPSA,
            CMPSB       CMPSB,
            CMPSD       CMPSD,
            CMPSE       CMPSE,
            CMPSF       CMPSF,
            CMPSG       CMPSG,
            CMPSH       CMPSH,
            CMPSI       CMPSI,
            CMPSJ       CMPSJ,
            CMPSK       CMPSK,
            CMPSL       CMPSL,
            CMPS0       CMPS0,
            CMGST       CMGST,
            COSTA       COSTA_D,
            SPSTG       SPSTG,
            FSSTK       FSSTK,
            LSSTK       LSSTK,
            BLOCK       BLOCK_VB,
            FKIVK       FKIVK,
            TRSTA       TRSTA,
            UVWAS       UVWAK_SU,
            UVPAK       UVPAK_UK,
            UVPIK       UVPIK_UK,
            UVWAK       UVWAK_UK,
            CMPSM       CMPSM,
            DCSTK       DCSTK,
            CMPS_CM     CMPS_CM,
            CMPS_TE     CHAR1,
            VKGRP       VKGRP,
            VKBUR       VKBUR,
            LIFSK       LIFSK,
            FAKSK       FAKSK,
            BP1_CODE    KUNNR,
            BP2_CODE    KUNNR,
            BP3_CODE    KUNNR,
            BP_FUNCT    PARVW,
            POSNR       POSNR_VA,
            MATNR       MATNR,
            MPROK       MPROK,  """"27-5-19
            WAVWR       WAVWR
            .
SELECT_MULTY: KUNNR,
            VBELN,
            VKORG ,
            VTWEG ,
            SPART,
            VBTYP,
            AUART,
            ERDAT,
            AUDAT,
            AEDAT,
            GUEBG,
            GUEEN,
            ANGDT,
            BNDDT,
            VDATU,
            ERNAM,
            DATUM,
"            DURATION_M,
"            DURATION_H ,
"            DURATION_D,
            DURATION,
            RFSTK,
            RFGSK,
            BESTK,
            LFSTK,
            LFGSK,
            FKSAK,
            ABSTK,
            GBSTK,
            UVALS,
            UVVLS,
            UVFAS,
            UVALL,
            UVVLK,
            UVFAK,
            UVPRS,
            CMPSA,
            CMPSB,
            CMPSD,
            CMPSE,
            CMPSF,
            CMPSG,
            CMPSH,
            CMPSI,
            CMPSJ,
            CMPSK,
            CMPSL,
            CMPS0,
            CMGST,
            COSTA,
            SPSTG,
            FSSTK,
            LSSTK,
            BLOCK,
            FKIVK,
            TRSTA,
            UVWAS,
            UVPAK,
            UVPIK,
            UVWAK,
            CMPSM,
            DCSTK,
            VKGRP,
            VKBUR,
            CMPS_CM,
            CMPS_TE,
            LIFSK,
            FAKSK,
            BP1_CODE,
            BP2_CODE,
            BP3_CODE,
            POSNR,
            MATNR,
            MPROK,  """ 27-5-19
            WAVWR.
CONVERT_MULTY: KUNNR ALPHA,
               VBELN ALPHA,
               BP1_CODE ALPHA,
               BP2_CODE ALPHA,
               BP3_CODE ALPHA,
               MATNR MATN1. ""3-8-16
  ""Tanya 14/11/18 :
  CONVERT_SINGLE:  BP1_FUNCT PARVW,
                   BP2_FUNCT PARVW ,
                   BP3_FUNCT PARVW .
  CONVERT_MULTY:  AUART AUART . """Tanya 14/11/18
RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
         R_FLD_VAL FOR DD03P-FIELDNAME .
DATA :   FLD_NAME TYPE FIELDNAME.
DATA : I TYPE I,
       CI(1) TYPE C,
       NFIELDS TYPE I VALUE 3.   "
DATA : BACKDAYS  TYPE I ,
       FORWDAYS TYPE I,
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
"--- 02/08/2016
DATA: LS_VBAP TYPE VBAP,
      LT_VBAP LIKE TABLE OF LS_VBAP.
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
""" 27-5-19
DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
      LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
      LV_DDTEXT LIKE  DD07V-DDTEXT.
IF NOT LV_FORWDAYS  IS INITIAL.
    LV_BACKDAYS = LV_FORWDAYS * ( -1 ).
ENDIF.
   IF R_DATUM[] IS INITIAL .
     RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
       DATE_FROM = SY-DATUM - LV_BACKDAYS .
       RS_DATUM-LOW = DATE_FROM .
        APPEND RS_DATUM TO R_DATUM.
   ENDIF.
   "--- Set Reference Date Field
   DATE_FROM = SY-DATUM.
   READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
   IF SY-SUBRC IS INITIAL.
     DATE_FROM = RS_DATUM-LOW.
     DATE_TO = RS_DATUM-HIGH.
     IF DATE_TO < DATE_FROM.
       DATE_TO = DATE_FROM.
     ENDIF.
   ENDIF.
   "--- Check Quatetion or Contracts types
   IF LV_DATE_REF_FLD IS INITIAL.
     READ TABLE R_VBTYP INTO RS_VBTYP INDEX 1.
     IF SY-TFILL = 1. " the single record only
       IF RS_VBTYP-OPTION = 'EQ'.
         LV_VBTYP = RS_VBTYP-LOW.
       ENDIF.
     ENDIF.
     IF LV_VBTYP = 'G'.
       LV_DATE_REF_FLD = 'GUEBG'.
     ELSEIF LV_VBTYP = 'B'.
       LV_DATE_REF_FLD = 'ANGDT'.
     ENDIF.
 "    endif.
   ENDIF.
   "---
   CASE LV_DATE_REF_FLD.
     WHEN 'ERDAT'.
       R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
     WHEN 'AEDAT'.
       R_AEDAT[] = R_DATUM[]. "Changed On
     WHEN 'AUDAT'.
       R_AUDAT[] = R_DATUM[]. "Document Date (Date Received/Sent)
     WHEN 'VDATU'.
       R_VDATU[] = R_DATUM[]. "Requested delivery date
****************************************************************
     WHEN 'GUEBG' OR 'GUEEN'.
***         RS_GUEBG-SIGN = 'I' .
***         RS_GUEBG-OPTION = 'LE'.   "'LE' .
***         RS_GUEBG-LOW = DATE_TO .
***         APPEND RS_GUEBG to R_GUEBG.
***         RS_GUEEN-SIGN = 'I' .
***         RS_GUEEN-OPTION = 'GE'.   "'GE' .
***         RS_GUEEN-LOW = DATE_FROM .
***         APPEND RS_GUEEN to R_GUEEN.
         RS_GUEBG-SIGN = 'I' .
         RS_GUEBG-OPTION = 'LE'.   "'LE' .
         RS_GUEBG-LOW = SY-DATUM .
         APPEND RS_GUEBG TO R_GUEBG.
         RS_GUEEN-SIGN = 'I' .
         RS_GUEEN-OPTION = 'BT'.
         RS_GUEEN-LOW = SY-DATUM .
         RS_GUEEN-HIGH = DATE_TO .
         APPEND RS_GUEEN TO R_GUEEN.
     WHEN 'ANGDT' OR 'BNDDT'.
***         RS_ANGDT-SIGN = 'I' .
***         RS_ANGDT-OPTION = 'LE'.    "'LE' .
***         RS_ANGDT-LOW = DATE_TO .
***         APPEND RS_ANGDT to R_ANGDT.
***         RS_BNDDT-SIGN = 'I' .
***         RS_BNDDT-OPTION = 'GE' .   "'GE' .
***         RS_BNDDT-LOW = DATE_FROM .
***         APPEND RS_BNDDT to R_BNDDT.
         RS_ANGDT-SIGN = 'I' .
         RS_ANGDT-OPTION = 'LE'.    "'LE' .
         RS_ANGDT-LOW = SY-DATUM .
         APPEND RS_ANGDT TO R_ANGDT.
         RS_BNDDT-SIGN = 'I' .
         RS_BNDDT-OPTION = 'BT' .   "'GE' .
         RS_BNDDT-LOW = SY-DATUM .
         RS_BNDDT-HIGH = DATE_TO .
         APPEND RS_BNDDT TO R_BNDDT.
     WHEN OTHERS.
       R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
       IF LV_DATE_REF_FLD IS INITIAL.
         LV_DATE_REF_FLD = 'ERDAT'.
       ENDIF.
   ENDCASE.
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
    CALL FUNCTION '/SKN/FH_SW_10_01_ORD_STAT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   ELSE.
    CALL FUNCTION '/SKN/FC_SW_10_01_ORD_STAT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
   ENDIF.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM VBAK AS A
    INNER JOIN VBUK AS K
    ON A~VBELN = K~VBELN
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE A~VBELN IN R_VBELN
      AND  A~KUNNR IN R_KUNNR
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
      AND A~ANGDT IN R_ANGDT
      AND A~BNDDT IN R_BNDDT
      AND A~VDATU IN R_VDATU
      AND A~LIFSK IN R_LIFSK
      AND A~FAKSK IN R_FAKSK
      AND A~VBTYP IN R_VBTYP
      AND A~ERNAM IN R_ERNAM
      AND K~RFSTK IN R_RFSTK
      AND K~RFGSK IN R_RFGSK
      AND K~BESTK IN R_BESTK
      AND K~LFSTK IN R_LFSTK
      AND K~LFGSK IN R_LFGSK
      AND K~FKSAK IN R_FKSAK
      AND K~ABSTK IN R_ABSTK
      AND K~GBSTK IN R_GBSTK
      AND K~UVALS IN R_UVALS
      AND K~UVVLS IN R_UVVLS
      AND K~UVFAS IN R_UVFAS
      AND K~UVALL IN R_UVALL
      AND K~UVVLK IN R_UVVLK
      AND K~UVFAK IN R_UVFAK
      AND K~UVPRS IN R_UVPRS
      AND K~CMPSA IN R_CMPSA
      AND K~CMPSB IN R_CMPSB
      AND K~CMPSD IN R_CMPSD
      AND K~CMPSE IN R_CMPSE
      AND K~CMPSF IN R_CMPSF
      AND K~CMPSG IN R_CMPSG
      AND K~CMPSH IN R_CMPSH
      AND K~CMPSI IN R_CMPSI
      AND K~CMPSJ IN R_CMPSJ
      AND K~CMPSK IN R_CMPSK
      AND K~CMPSL IN R_CMPSL
      AND K~CMPS0 IN R_CMPS0
      AND K~CMGST IN R_CMGST
      AND K~COSTA IN R_COSTA
      AND K~SPSTG IN R_SPSTG
      AND K~FSSTK IN R_FSSTK
      AND K~LSSTK IN R_LSSTK
      AND K~BLOCK IN R_BLOCK
      AND K~FKIVK IN R_FKIVK
      AND K~TRSTA IN R_TRSTA
      AND K~UVWAS IN R_UVWAS
      AND K~UVPAK IN R_UVPAK
      AND K~UVPIK IN R_UVPIK
      AND K~UVWAK IN R_UVWAK
      AND K~CMPSM IN R_CMPSM
      AND K~DCSTK IN R_DCSTK
      AND K~CMPS_CM IN R_CMPS_CM.
***************************************************************************
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    IF  IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = REF_DATE
          T_FROM            = SY-UZEIT
          D_TO              = SY-DATUM
          T_TO              = SY-UZEIT
          TIME_UNIT         = LV_DURATION_UNIT  "'D'
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
************************************************************************
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
    DELETE T_DATA WHERE BP1_CODE NOT IN R_BP1_CODE.
    DELETE T_DATA WHERE BP2_CODE NOT IN R_BP2_CODE.
    DELETE T_DATA WHERE BP3_CODE NOT IN R_BP3_CODE.
   ENDIF.
  ENDIF.
  "--- Get BPs
"Delete  CMPS_TE
  DELETE T_DATA WHERE CMPS_TE NOT IN R_CMPS_TE.
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
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
"--- 02/08/2016
 IF LV_ITEM_DETAILS IS NOT INITIAL.
  IF T_DATA[] IS NOT INITIAL.
   "--- Add Item details
   REFRESH LT_VBAP.
   SELECT *
     FROM VBAP
     INTO CORRESPONDING FIELDS OF TABLE LT_VBAP
     FOR ALL ENTRIES IN T_DATA
     WHERE VBELN = T_DATA-VBELN
       AND POSNR IN R_POSNR
       AND MATNR IN R_MATNR
       AND MPROK IN R_MPROK  """ 27-5-19
       AND WAVWR IN R_WAVWR.
  REFRESH LT_DATA.
  LT_DATA[] = T_DATA[].
  REFRESH T_DATA.
  SORT LT_DATA BY VBELN.
  LOOP AT LT_VBAP INTO LS_VBAP.
    READ TABLE LT_DATA INTO LS_DATA
                       WITH KEY VBELN = LS_VBAP-VBELN
                       BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      "  Calculations for TADIRAN
**        ls_data-NETPR_VAT = ls_VBAP-NETWR   +  ls_VBAP-MWSBP. " Net price for TADIRAN
**        ls_data-NETWR_VAT = ( ls_VBAP-NETWR   +  ls_VBAP-MWSBP ) * ls_VBAP-KWMENG. "Net Value for TADIRAN
   "***Changed on 15-9-16
      IF LS_VBAP-KWMENG <> 0.
        LS_DATA-NETPR_VAT = ( LS_VBAP-NETWR   +  LS_VBAP-MWSBP ) /  LS_VBAP-KWMENG. " Net price for TADIRAN
      ELSE.
        LS_DATA-NETPR_VAT = 0.
      ENDIF.
        LS_DATA-NETWR_VAT =  LS_VBAP-NETWR   +  LS_VBAP-MWSBP  . "Net Value for TADIRAN
        LS_DATA-KWMENG_INT = LS_VBAP-KWMENG. " Int Quantaty for TADIRAN
      MOVE-CORRESPONDING LS_DATA TO T_DATA.
    ENDIF.
    MOVE-CORRESPONDING LS_VBAP TO T_DATA.
    APPEND T_DATA.
  ENDLOOP.
 """" 27-5-19
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    LV_DOMNAME = 'MPROK'.
    LV_DOMVALUE = T_DATA-MPROK.
      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
        EXPORTING
          I_DOMNAME        = LV_DOMNAME
          I_DOMVALUE       = LV_DOMVALUE
          LANGU            = LV_LANGU
*         SW_DEST          =
       IMPORTING
         E_DDTEXT          = LV_DDTEXT
       EXCEPTIONS
         NOT_EXIST        = 1
         OTHERS           = 2
                .
     IF SY-SUBRC = 0.
      T_DATA-MPROK_DESC = LV_DDTEXT.
     ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
   ENDLOOP.
  ENDIF.
 ENDIF.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
