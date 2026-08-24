# Exception Indicator: S/4 Sales order general ( SW_10_01_ORD_STAT_CH)

## General Overview

This Exception Indicator identifies sales orders that match configurable header, item, and status criteria and, when reference fields are supplied, compares two numeric or amount values on each order line using a comparison operator, optionally converting currencies before evaluation.

This EI serves as an essential control for S/4 sales order operations by:

- Enabling detection of orders whose overall processing, delivery, billing, and credit-check status fields require review
- Supporting configurable comparison of two reference fields from sales order header or item tables against each other or against threshold values
- Converting compared amounts to a target foreign currency when document currency differs from the comparison currency
- Applying posting-date or alternative date windows through configurable lookback and reference-date selection
- Supporting age-based prioritization when orders remain in scope after a chosen reference date
- Enriching results with partner and organizational context

Typical use includes general order exception monitoring, amount or quantity threshold checks between two SD fields, and periodic review of orders with specific status combinations before release or billing. Results are intended for exception workflows rather than operational order list reporting.

The routine reads sales order header and item data, applies dynamic field comparison and currency conversion rules, enriches partner and customer description data, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor sales orders against configured status and field-comparison rules creates multiple risks across order fulfillment, billing, credit control, and customer service:

**Sales and Operations Risks**

- Orders with blocked or incomplete overall status can delay delivery, billing, or credit release without structured review
- Undetected mismatches between two compared amounts or quantities on the same order can hide pricing, quantity, or value inconsistencies
- Individual status fields that remain in error or pending state can conceal specific control gaps on header or item lines

**Operational Risks**

- Monitoring windows misaligned with order entry cadence can exclude recent exceptions or retain resolved cases
- Comparison rules that are too broad or too narrow can hide actionable orders or create reviewer fatigue
- Currency conversion settings that are not aligned with the compared fields can produce misleading threshold results

**Control and Audit Risks**

- Weak order monitoring reduces evidence that flagged documents were reviewed before release decisions
- Lack of recurring exception review limits accountability for sales operations follow-up on stalled orders
- Missing customer and organizational context delays escalation of commercially significant cases

## Suggested Resolution

**Immediate Response**

- Review flagged orders for overall processing status, compared field values, delivery and billing status, customer, and sales organization
- Confirm with sales or logistics whether the current status and compared values are correct or require correction or release action
- Prioritize high-value customers and long-aged orders for immediate follow-up

**System Assessment**

- Validate lookback window, reference-date field, comparison operator, reference tables and fields, and foreign-currency settings against review cadence
- Tune status, document type, and organizational scope so results stay actionable
- Compare exception counts by status type, comparison outcome, sales organization, and customer to identify systematic gaps

**Corrective Actions**

- Resolve status blocks or update orders through standard SD processes where review confirms action is required
- Adjust monitoring scope and comparison thresholds after cleanup so results reflect truly exceptional cases
- Document review outcomes and schedule recurring runs before order release or close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ABGRU | Reason for rejection | CHAR | 2 | 0 | ABGRU | ABGRU |
| 2 | ABSTA | Release status | CHAR | 1 | 0 | ABSTA | ABSTA |
| 3 | ABSTK | Rejection status | CHAR | 1 | 0 | ABSTK | STATV |
| 4 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 5 | ANGDT | Quotation valid from | DATS | 8 | 0 | ANGDT_V | DATUM |
| 6 | ARKTX | Description | CHAR | 40 | 0 | ARKTX | TEXT40 |
| 7 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 8 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 9 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 10 | BESTK | Confirmed | CHAR | 1 | 0 | BESTK | STATV |
| 11 | BLOCK | Indicator: Document preselecte | CHAR | 1 | 0 | BLOCK_VB | BLOCK_VB |
| 12 | BNDDT | Quotation valid to | DATS | 8 | 0 | BNDDT | DATUM |
| 13 | BP1_CODE | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 14 | BP1_FUNCT | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 15 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 16 | BP2_CODE | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 17 | BP2_FUNCT | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 18 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 19 | BP3_CODE | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 20 | BP3_FUNCT | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 21 | BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 22 | CMGST | Overall CreditStatus | CHAR | 1 | 0 | CMGST | CMGST |
| 23 | CMPRE | Credit price | CURR | 11 | 2 | CMPRE | WERTV6 |
| 24 | CMPRE_FLT | Credit price | FLTP | 16 | 16 | CMPRE_FLT | FLTP |
| 25 | CMPS0 | Reserve | CHAR | 1 | 0 | CMPS0 | CMPSZ |
| 26 | CMPS1 | Reserve | CHAR | 1 | 0 | CMPS1 | CMPSZ |
| 27 | CMPS2 | Reserve | CHAR | 1 | 0 | CMPS2 | CMPSZ |
| 28 | CMPS_CM | SAP Credit Management | CHAR | 1 | 0 | CMPS_CM | CMPSZ |
| 29 | CMPS_TE | Single-Character Indicator | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 30 | CMPSA | Static check | CHAR | 1 | 0 | CMPSA | CMPSZ |
| 31 | CMPSB | Dynamic check | CHAR | 1 | 0 | CMPSB | CMPSZ |
| 32 | CMPSC | Maximum value | CHAR | 1 | 0 | CMPSC | CMPSZ |
| 33 | CMPSD | Terms of payment | CHAR | 1 | 0 | CMPSD | CMPSZ |
| 34 | CMPSE | Customer review date | CHAR | 1 | 0 | CMPSE | CMPSZ |
| 35 | CMPSF | Overdue open items | CHAR | 1 | 0 | CMPSF | CMPSZ |
| 36 | CMPSG | Oldest open items | CHAR | 1 | 0 | CMPSG | CMPSZ |
| 37 | CMPSH | Max.dunning level | CHAR | 1 | 0 | CMPSH | CMPSZ |
| 38 | CMPSI | Financial document | CHAR | 1 | 0 | CMPSI | CMPSZ |
| 39 | CMPSJ | Expt cred. insurance | CHAR | 1 | 0 | CMPSJ | CMPSZ |
| 40 | CMPSK | Payment card | CHAR | 1 | 0 | CMPSK | CMPSZ |
| 41 | CMPSL | Reserve | CHAR | 1 | 0 | CMPSL | CMPSZ |
| 42 | CMPSM | Obsolete credit data | CHAR | 1 | 0 | CMPSM | CMPSZ |
| 43 | COMP_OPERATOR | Comparion Operator | CHAR | 2 | 0 | BUCC_OPERATOR | BUCC_OPERATOR |
| 44 | COSTA | Confirmation status | CHAR | 1 | 0 | COSTA_D | COSTA |
| 45 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 46 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 47 | DCSTK | Delay status | CHAR | 1 | 0 | DCSTK | STATV |
| 48 | DOC_TYPE_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 49 | DUMMY | Single-Character Indicator | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 50 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 51 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 52 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 53 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 54 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 55 | FAKSK | Billing block | CHAR | 2 | 0 | FAKSK | FAKSP |
| 56 | FKIVK | Totals status | CHAR | 1 | 0 | FKIVK | STATV |
| 57 | FKREL | Relevant for Billing | CHAR | 1 | 0 | FKREL | FKREL |
| 58 | FKSAK | Bill.stat.order-rel. | CHAR | 1 | 0 | FKSAK | STATV |
| 59 | FORWDAYS | Forward days |  | 0 | 0 |  |  |
| 60 | FSSTA | Billing block status | CHAR | 1 | 0 | FSSTA | STATV |
| 61 | FSSTK | Overall block status | CHAR | 1 | 0 | FSSTK | STATV |
| 62 | GBSTA | Overall status | CHAR | 1 | 0 | GBSTA | STATV |
| 63 | GBSTK | Overall status | CHAR | 1 | 0 | GBSTK | STATV |
| 64 | GUEBG | Valid-from date | DATS | 8 | 0 | GUEBG | DATUM |
| 65 | GUEEN | Valid-to date | DATS | 8 | 0 | GUEEN | DATUM |
| 66 | INC_NOT_CONV_CURR | Include not converted currency | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 67 | ITEM_DETAILS | CHAR | 1 | 0 |  | XFELD |  |
| 68 | KBMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KBMENG | MENG15 |
| 69 | KDMAT | Customer Material | CHAR | 35 | 0 | MATNR_KU | IDNEX |
| 70 | KLMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KLMENG | MENG15 |
| 71 | KOQUA | Pick confirmation | CHAR | 1 | 0 | KOQUA | STATV |
| 72 | KOSTA | Picking status | CHAR | 1 | 0 | KOSTA | STATV |
| 73 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 74 | KWMENG | Order Quantity | QUAN | 15 | 3 | KWMENG | MENG15 |
| 75 | KWMENG_INT | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 76 | KZWI1 | Subtotal 1 | CURR | 13 | 2 | KZWI1 | WERTV7 |
| 77 | KZWI2 | Subtotal 2 | CURR | 13 | 2 | KZWI2 | WERTV7 |
| 78 | KZWI3 | Subtotal 3 | CURR | 13 | 2 | KZWI3 | WERTV7 |
| 79 | KZWI4 | Subtotal 4 | CURR | 13 | 2 | KZWI4 | WERTV7 |
| 80 | KZWI5 | Subtotal 5 | CURR | 13 | 2 | KZWI5 | WERTV7 |
| 81 | KZWI6 | Subtotal 6 | CURR | 13 | 2 | KZWI6 | WERTV7 |
| 82 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 83 | LFGSK | Overall dlv.status | CHAR | 1 | 0 | LKGSK | STATV |
| 84 | LFREL | Itm relev.for deliv. | CHAR | 1 | 0 | LFREL_AP | XFELD |
| 85 | LFSTK | Delivery status | CHAR | 1 | 0 | LFSTK | STATV |
| 86 | LIFSK | Delivery block | CHAR | 2 | 0 | LIFSK | LIFSP |
| 87 | LSMENG | Required deliv. qty | QUAN | 15 | 3 | LSMENG | MENG15 |
| 88 | LSSTA | Delivery block stat. | CHAR | 1 | 0 | LSSTA | STATV |
| 89 | LSSTK | Over. dlv. blk stat. | CHAR | 1 | 0 | LSSTK_G | STATV |
| 90 | MANAGE_IN_UTC | CHAR | 1 | 0 |  | XFELD |  |
| 91 | MANEK | Manual Completion of Contract | CHAR | 1 | 0 | MANEK | MANEK |
| 92 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 93 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 94 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 95 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 96 | MPROK_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 97 | MWSBP | Tax amount | CURR | 13 | 2 | MWSBP | WERTV7 |
| 98 | NETPR | Net price | CURR | 11 | 2 | NETPR | WERTV6 |
| 99 | NETPR_VAT | Net price | CURR | 11 | 2 | NETPR | WERTV6 |
| 100 | NETWR | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 101 | NETWR_VAT | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 102 | PDSTA | Proof of delivery status | CHAR | 1 | 0 | PDSTA | STATV |
| 103 | PKSTA | Packing status | CHAR | 1 | 0 | PKSTA | STATV |
| 104 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 105 | PRODH | Product hierarchy | CHAR | 18 | 0 | PRODH_D | PRODH |
| 106 | REF_FIELD1 | Numeric Field 1 to compare |  | 0 | 0 |  |  |
| 107 | REF_FIELD2 | Numeric Field 2 to compare |  | 0 | 0 |  |  |
| 108 | REF_FIELD_NAME1 | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 109 | REF_FIELD_NAME2 | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 110 | REF_TABNAME1 | Table name(for REF.FIELD1) |  | 0 | 0 |  |  |
| 111 | REF_TABNAME2 | Table name(for REF.FIELD2) |  | 0 | 0 |  |  |
| 112 | RESULT_COMP1 | Numeric val.(for comp. FIELD1) |  | 0 | 0 |  |  |
| 113 | RFGSK | Total reference stat | CHAR | 1 | 0 | RFGSK | STATV |
| 114 | RFSTK | Reference status | CHAR | 1 | 0 | RFSTK | STATV |
| 115 | RRSTA | Rev. determ. status | CHAR | 1 | 0 | RR_STATUS | STATV |
| 116 | SALES_GRP_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 117 | SALES_OFF_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 118 | SALES_ORG_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 119 | SAPRL | SAP Release | CHAR | 4 | 0 | SAPRL | SAPRL |
| 120 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 121 | SPSTG | Overall blkd status | CHAR | 1 | 0 | SPSTG | STATV |
| 122 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 123 | TRSTA | Trns.plan.status | CHAR | 1 | 0 | TRSTA | TRSTA |
| 124 | UEPOS | Higher-level item | NUMC | 6 | 0 | UEPOS | POSNR |
| 125 | UVALL | Header data | CHAR | 1 | 0 | UVALL_UK | STATV |
| 126 | UVALS | Item data | CHAR | 1 | 0 | UVALL_SU | STATV |
| 127 | UVFAK | Header billing data | CHAR | 1 | 0 | UVFAK_UK | STATV |
| 128 | UVFAS | Item billing data... | CHAR | 1 | 0 | UVFAK_SU | STATV |
| 129 | UVK01 | Header reserves 1 | CHAR | 1 | 0 | UVK01 | STATV |
| 130 | UVK02 | Header reserves 2 | CHAR | 1 | 0 | UVK02 | STATV |
| 131 | UVK03 | Header reserves 3 | CHAR | 1 | 0 | UVK03 | STATV |
| 132 | UVK04 | Header reserves 4 | CHAR | 1 | 0 | UVK04 | STATV |
| 133 | UVK05 | Header reserves 5 | CHAR | 1 | 0 | UVK05 | STATV |
| 134 | UVP01 | Item reserves 1 | CHAR | 1 | 0 | UVP01 | STATV |
| 135 | UVP02 | Item reserves 2 | CHAR | 1 | 0 | UVP02 | STATV |
| 136 | UVP03 | Item reserves 3 | CHAR | 1 | 0 | UVP03 | STATV |
| 137 | UVP04 | Item reserves 4 | CHAR | 1 | 0 | UVP04 | STATV |
| 138 | UVP05 | Item reserves 5 | CHAR | 1 | 0 | UVP05 | STATV |
| 139 | UVPAK | Head.data packaging | CHAR | 1 | 0 | UVPAK_UK | STATV |
| 140 | UVPIK | Head. data picking/putaway | CHAR | 1 | 0 | UVPIK_UK | STATV |
| 141 | UVPRS | Pricing | CHAR | 1 | 0 | UVPRS_UK | STATV |
| 142 | UVS01 | Total reserves 1 | CHAR | 1 | 0 | UVS01 | STATV |
| 143 | UVS02 | Total reserves 2 | CHAR | 1 | 0 | UVS02 | STATV |
| 144 | UVS03 | Total reserves 3 | CHAR | 1 | 0 | UVS03 | STATV |
| 145 | UVS04 | Total reserves 4 | CHAR | 1 | 0 | UVS04 | STATV |
| 146 | UVS05 | Total reserves 5 | CHAR | 1 | 0 | UVS05 | STATV |
| 147 | UVVLK | Header delivery data | CHAR | 1 | 0 | UVVLK_UK | STATV |
| 148 | UVVLS | Item delivery data.. | CHAR | 1 | 0 | UVVLS_SU | STATV |
| 149 | UVWAK | Head. data goods mvmt | CHAR | 1 | 0 | UVWAK_UK | STATV |
| 150 | UVWAS | Item data: goods mvmt | CHAR | 1 | 0 | UVWAK_SU | STATV |
| 151 | VALID_ONLY | Valid | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 152 | VALUE_COMP1 | Value 1 to Compare | CURR | 15 | 2 |  |  |
| 153 | VALUE_COMP2 | Value 2 to Compare | CURR | 15 | 2 |  |  |
| 154 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 155 | VBOBJ | Document object | CHAR | 1 | 0 | VBOBJ | VBOBJ |
| 156 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 157 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 158 | VGTYP | Preceding doc.categ. | CHAR | 1 | 0 | VBTYP_V | VBTYP |
| 159 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 160 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 161 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 162 | VRKME | Sales unit | UNIT | 3 | 0 | VRKME | MEINS |
| 163 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 164 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 165 | WAERK_FR | Foreign Currency | CUKY | 5 | 0 |  |  |
| 166 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |
| 167 | WBSTA | Goods movement stat. | CHAR | 1 | 0 | WBSTA | STATV |
| 168 | WERKS | Plant | CHAR | 4 | 0 | WERKS_EXT | WERKS |
| 169 | ZWERT | OA Target Value | CURR | 13 | 2 | DZWERT | WERT7 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 169 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ABGRU** (Reason for rejection)

Reason for Rejection stores the code that explains why a sales document item was canceled or not processed further.

**ABSTA** (Release status)

Helps monitoring stay readable by requiring release status (ABSTA) to match organizational or technical selectors when set.

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

**BESTK** (Confirmed)

Confirmation Status represents the overall processing state of a sales document or purchasing contract indicating whether the schedule lines or item quantities have been officially confirmed by the supplying party.

**BLOCK** (Indicator: Document preselecte)

Blocking indicator showing whether the record is restricted for posting/processing.

**BNDDT** (Quotation valid to)

Quotation Valid To represents the exact calendar date until which the pricing conditions, delivery terms, and material commitments defined in a sales quotation remain legally binding for the customer.

**BP1_CODE** (Customer)

<mark>Business partner slot 1 code used to identify the linked partner in multi-partner records.</mark>

**BP1_FUNCT** (Partner Function)

<mark>Business partner slot 1 function/role used to classify partner responsibility.</mark>

**BP1_NAME** (Name)

<mark>Business partner slot 1 name/description used for readable partner output.</mark>

**BP2_CODE** (Customer)

<mark>Business partner slot 2 code used to identify the linked partner in multi-partner records.</mark>

**BP2_FUNCT** (Partner Function)

<mark>Business partner slot 2 function/role used to classify partner responsibility.</mark>

**BP2_NAME** (Name)

<mark>Business partner slot 2 name/description used for readable partner output.</mark>

**BP3_CODE** (Customer)

<mark>Business partner slot 3 code used to identify the linked partner in multi-partner records.</mark>

**BP3_FUNCT** (Partner Function)

<mark>Business partner slot 3 function/role used to classify partner responsibility.</mark>

**BP3_NAME** (Name)

<mark>Business partner slot 3 name/description used for readable partner output.</mark>

**CMGST** (Overall CreditStatus)

Credit-management overall status summarizing credit exposure processing for the business partner or document.

**CMPRE** (Credit price)

Supports escalation where credit price on CMPRE signals ownership for follow-up between Basis and functional teams.

**CMPRE_FLT** (Credit price)

For operations, credit price on CMPRE_FLT indicates whether a row belongs in the current monitoring pass versus historical noise.

**CMPS0 - CMPS2** (Reserve)

Customer Reserve 1 acts as a customizable status indicator used to hold the evaluation results of user-defined, custom credit check logic programmed in system enhancements.

**CMPS_CM** (SAP Credit Management)

Status of SAP Credit Management Check stores the central, consolidated evaluation result transmitted back from the advanced SAP S/4HANA Credit Management engine (FSCM).

**CMPS_TE** (Single-Character Indicator)

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

**CMPSM** (Obsolete credit data)

Credit check data is obsolete tracks whether the system's evaluated credit information has expired or is no longer considered valid based on the configuration timeframes.

**COMP_OPERATOR** (Comparion Operator)

Comparison operator used to evaluate thresholds (equal, less-than, greater-than, etc.).

**COSTA** (Confirmation status)

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

**DOC_TYPE_DESC** (Description)

Connects to alert semantics: rows removed for failing description on DOC_TYPE_DESC never reach downstream filtering.

**DUMMY** (Single-Character Indicator)

Combines with related filters so single-character indicator on DUMMY refines which records remain for duration or state checks.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

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

**FKIVK** (Totals status)

Intercompany billing variant or billing-type control key on SD billing headers for IC scenarios.

**FKREL** (Relevant for Billing)

Relevant for Billing field determines how an item should be billed, indicating whether the billing document is based on the delivery, the sales order, or a pro forma invoice.

**FKSAK** (Bill.stat.order-rel.)

Billing Status for Order-Related Billing Documents represents the header-level processing state of a sales document that indicates whether all items requiring direct invoicing from the order have been fully billed.

**FORWDAYS** (Forward days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

Forwdays is based on DATE_REF_FLD field.

**Not in use**
**FSSTA** (Billing block status)

Captures edge cases where billing block status (FSSTA) must be non-default to reproduce a customer-specific monitoring scenario.

**FSSTK** (Overall block status)

Overall Billing Block Status tracks whether an entire document contains any active billing blocks at the header or item level, indicating if the document is released for invoicing.

**GBSTA** (Overall status)

Interprets overall status as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on GBSTA.

**GBSTK** (Overall status)

Overall Processing Status indicates the cumulative progress of a document, tracking whether it is open, in process, or completely finished based on subsequent activities.

**GUEBG** (Valid-from date)

Valid-from Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.

**GUEEN** (Valid-to date)

Valid-to Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.

**INC_NOT_CONV_CURR** (Include not converted currency)

Works downstream of the initial read so include not converted currency on INC_NOT_CONV_CURR still participates in row-level deletion rules.

**ITEM_DETAILS** (CHAR)

Separates cross-client noise from in-scope work when char on ITEM_DETAILS correlates with client or user attributes.

**Not in use**
**KBMENG** (Cumul.confirmed qty)

Cumulative Order Quantity stores the total accumulated quantity of a specific item that a customer has ordered across multiple partial deliveries, allowing the system to track remaining open quantities.

**KDMAT** (Customer Material)

Customer Material Number stores the proprietary part or material identification number used by a specific customer, enabling the system to cross-reference and map it to the internal SAP material master number during order processing.

**KLMENG** (Cumul.confirmed qty)

Cumulative schedule or order quantity in the sales item context-confirmed or requested quantity accumulated on schedules.

**KOQUA** (Pick confirmation)

<mark>Capacity-requirement quantity or quota field in PP detailed scheduling extracts for workload analytics.</mark>

**KOSTA** (Picking status)

<mark>Production order header status summarizing whether the order is released, technically completed, or locked.</mark>

**KUNNR** (Sold-to party)

Customer account is used to scope records to specific customers across SD/FI flows.

**KWMENG** (Order Quantity)

Cumulative order quantity in sales units on the item-commercial ordered quantity for SD lines.

**KWMENG_INT** (Natural Number)

Internal Order Quantity stores the calculated order quantity formatted as an integer or processed in an internal numeric format for backend program calculations and system validation.

**KZWI1 - KZWI6** (Subtotal 1)

Explains why two monitoring passes differ: only the pass with stricter subtotal 1 on KZWI1 surfaces the disputed rows.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**LFGSK** (Overall dlv.status)

Overall Delivery Status for All Items aggregates and tracks the cumulative shipping progress across every deliverable line item in a sales document, indicating whether the entire order is outstanding, partially shipped, or fully completed.

**LFREL** (Itm relev.for deliv.)

Delivery Relevance indicates whether a specific item category or schedule line is eligible and required for delivery processing, allowing the system to determine if a delivery document should be generated for the item.

**LFSTK** (Delivery status)

Works downstream of the initial read so delivery status on LFSTK still participates in row-level deletion rules.

**LIFSK** (Delivery block)

Delivery Block stores the central configuration key used to withhold or prevent an entire sales document or specific item from being processed for delivery, usually due to credit limits, political checks, or logistical constraints.

**LSMENG** (Required deliv. qty)

Cumulative Required Quantity in Sales Units stores the total target quantity of an item from a scheduling agreement or contract, expressed in the sales unit of measure, used to track cumulative required quantities against actual delivery performance.

**LSSTA** (Delivery block stat.)

Helps distinguish technical versus business attributes when delivery block stat. on LSSTA correlates with counters or status fields.

**LSSTK** (Over. dlv. blk stat.)

Overall Delivery Block Status evaluates whether any active delivery restrictions exist at either the header, item, or schedule line level, determining if the document as a whole is officially blocked or released for outbound shipping processing.

**MANAGE_IN_UTC** (CHAR)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MANEK** (Manual Completion of Contract)

Manual Completion of Contract represents the indicator that determines whether an purchasing contract item can be manually marked as closed or fully processed even if the target quantity or value has not been completely reached.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

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

**PDSTA** (Proof of delivery status)

<mark>MRP or production planning status on order or requirement rows summarizing planning outcome.</mark>

**PKSTA** (Packing status)

Picking status on warehouse-relevant deliveries showing whether pick, pack, and GI steps are complete.

**POSNR** (Sales Document Item)

Document item number used for line-level drilldown and joins.

**PRODH** (Product hierarchy)

Product Hierarchy stores the alpha-numeric code that structures materials into different levels for sales analysis and pricing.

**REF_FIELD1 - REF_FIELD2** (Numeric Field 1 to compare)

Ensures reporting respects numeric field 1 to compare constraints carried by REF_FIELD1.

**Not in use**
**REF_FIELD_NAME1 - REF_FIELD_NAME2** (Field name)

<mark>Primary DDIC reference field name driving dynamic comparisons, conversions, or check-table semantics.</mark>

**REF_TABNAME1 - REF_TABNAME2** (Table name(for REF.FIELD1))

Connects to alert semantics: rows removed for failing table name(for ref.field1) on REF_TABNAME1 never reach downstream filtering.

**RESULT_COMP1** (Numeric val.(for comp. FIELD1))

Uses numeric val.(for comp. field1) from the source context so only records with RESULT_COMP1 inside declared values proceed.

**RFGSK** (Total reference stat)

Total Reference Status for All Items aggregates and tracks the cumulative copying or reference progress across every line item in a sales document, indicating whether the entire order has been fully transferred into subsequent documents like deliveries or invoices.

**RFSTK** (Reference status)

Reference Document Header Status indicates whether a preceding document-such as a quotation or inquiry-has been successfully and completely referenced or copied into the current sales order header.

**RRSTA** (Rev. determ. status)

Revenue Determination Status tracks the progress of revenue recognition for a document, indicating whether revenue recognition rules have been applied, partially executed, or fully completed for accounting purposes.

**SALES_GRP_DESC** (Description)

Connects to alert semantics: rows removed for failing description on SALES_GRP_DESC never reach downstream filtering.

**SALES_OFF_DESC** (Description)

Interprets description as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on SALES_OFF_DESC.

**SALES_ORG_DESC** (Description)

For distributed landscapes, description on SALES_ORG_DESC often anchors which application server or destination appears in results.

**SAPRL** (SAP Release)

SAP Release represents the specific version or software modification level of the SAP system currently in use.

**SPART** (Division)

Division key used for SD product-line segmentation.

**SPSTG** (Overall blkd status)

Overall Blocked Status evaluates whether a document is withheld from further processing by aggregating the statuses of all active credit blocks, delivery blocks, and billing blocks across the entire transaction.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TRSTA** (Trns.plan.status)

Transport status code on TRKORR-style requests summarizing whether the request is released or protected.

**UEPOS** (Higher-level item)

Higher-Level Item in Bill of Material Structures stores the line item number of the parent material, establishing a hierarchical link between a sub-item or component and its main product during sales order processing.

**UVALL** (Header data)

General Incompletion Status for Header indicates whether any critical general data fields are missing at the document header level, restricting subsequent processing until the mandatory information is provided.

**UVALS** (Item data)

Total Incompletion Status General aggregates the completion state of all general data fields across both the header and individual items, confirming if the document is entirely complete.

**UVFAK** (Header billing data)

Header Incompletion Status for Billing indicates whether mandatory billing-related data is missing from the document header, preventing the transaction from being invoiced until resolved.

**UVFAS** (Item billing data...)

Total Incompletion Status for Billing aggregates the billing readiness across both header data and individual items, flagging whether any missing financial or tax information is blocking downstream invoice creation.

**UVK01 - UVK05** (Header reserves 1)

Customer Reserve 1: Header Status acts as a customizable status indicator used to hold the evaluation results of user-defined, custom credit or incompletion check logic programmed via system enhancements.

**UVP01 - UVP05** (Item reserves 1)

Supports operational control by evaluating item reserves 1 through UVP01 for each candidate record.

**UVPAK** (Head.data packaging)

Header Incompletion Status for Packaging indicates whether mandatory packing instructions or container details are missing from the document header, preventing the creation of outbound logistics paperwork.

**UVPIK** (Head. data picking/putaway)

Header Incompletion Status for Picking or Putaway tracks whether critical storage location or warehouse movement data is missing from the document header, halting immediate warehouse fulfillment actions.

**UVPRS** (Pricing)

Document Incompletion Status for Pricing indicates whether essential price conditions, currency codes, or valuation factors are missing or invalid within the document, blocking downstream billing and financial posting.

**UVS01 - UVS05** (Total reserves 1)

Customer Reserve 1: Item Status acts as a customizable status indicator used to hold the evaluation results of user-defined, custom credit or incompletion check logic programmed at the line item level.

**UVVLK** (Header delivery data)

Header Incompletion Status for Delivery indicates whether mandatory shipping or logistical information is missing from the document header, preventing the creation of a outbound delivery document.

**UVVLS** (Item delivery data..)

Total Incompletion Status for Delivery aggregates the delivery readiness across both header data and individual line items, checking if missing shipping details are blocking outbound delivery creation.

**UVWAK** (Head. data goods mvmt)

Header Incompletion Status for Goods Movement tracks whether critical data required for the goods issue or goods receipt process-such as accounting or plant indicators-is missing from the document header.

**UVWAS** (Item data: goods mvmt)

Total Incompletion Status for Goods Movement aggregates the goods movement readiness across both header data and individual line items, flagging whether any missing parameters are blocking inventory updates.

**VALID_ONLY** (Valid)

Helps monitoring stay readable by requiring valid (VALID_ONLY) to match organizational or technical selectors when set.

**VALUE_COMP1 - VALUE_COMP2** (Value 1 to Compare)

When tightened, value 1 to compare (VALUE_COMP1) removes rows that would otherwise dilute attention from failing or stuck cases.

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBOBJ** (Document object)

SD Document Category Object classifies the specific business entity or transactional module type-such as a sales order, inquiry, quotation, or delivery-to control the data validation and processing logic applied to the record.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VDATU** (Requested deliv.date)

Requested/validity date used for schedule and due-date based filtering.

**VGTYP** (Preceding doc.categ.)

Preceding document category qualifying VGBEL semantics across orders, deliveries, and billing types.

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

**WAERK_FR** (Foreign Currency)

Ensures reporting respects foreign currency constraints carried by WAERK_FR.

**WAVWR** (Cost)

Statistical value amount field used for value-based exception thresholds.

**WBSTA** (Goods movement stat.)

Goods-movement status at header level summarizing posting state of goods issues and receipts.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**ZWERT** (OA Target Value)

Narrows retrieved rows where oa target value (ZWERT) must match the configured selection for this monitor.

### Parameter Relationships

**Lookback window:** When no explicit date range is supplied on individual date fields, **BACKDAYS** builds a lookback window from the current day before orders are read.

**Reference date mapping:** **DATE_REF_FLD** directs the lookback window to the created-on, changed-on, document, requested delivery, or contract and quotation validity dates. When **DATE_REF_FLD** is initial and a single document category value is supplied, the reference field defaults to valid-from for contracts or quotation valid-from for quotations.

**Status selection:** Overall processing, delivery, billing, rejection, confirmation, credit-check, block, and incompletion status fields filter orders by processing, delivery, billing, and credit-check state from the sales document header and item status.

**Field comparison:** **REF_TABNAME1** and **REF_FIELD_NAME1** identify the first value source; **REF_TABNAME2** and **REF_FIELD_NAME2** identify the second. When both field names exist and no explicit result thresholds are set, **COMP_OPERATOR** builds a dynamic comparison between the two field values in the database selection. **RESULT_COMP1** and **RESULT_COMP2** compare a single reference field against configured threshold amounts instead of field-to-field comparison.

**Currency conversion:** When **WAERK_FR** is set and compared fields are currency amounts, values are converted to the target foreign currency before comparison. **INC_NOT_CONV_CURR** controls whether rows with currencies that cannot be converted are retained in the result set.

**Age filter:** After rows are selected, elapsed time from each row's reference date to the evaluation time is calculated using **DURATION_UNIT** and stored in **DURATION**; rows outside the configured duration range are removed.

**Partner roles:** **BP1_FUNCT** / **BP1_CODE**, **BP2_FUNCT** / **BP2_CODE**, and **BP3_FUNCT** / **BP3_CODE** work together to enrich and filter business partner attributes on each order.

**Item data:** Sales order item fields are joined with header data so item-level material, quantity, value, and status attributes appear on each returned line.

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper and the on-premise path below that call is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **COMP_OPERATOR** - initial - treated as > by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **WAERK_FR** - initial - treated as USD by code

### Practical Example of Parameter Configuration

**Use Case 1: Item net value exceeds header net value**

**Purpose:** Review order lines where item net value is greater than header net value in the same document currency.

```
REF_TABNAME1 = VBAP
REF_FIELD_NAME1 = NETWR
REF_TABNAME2 = VBAK
REF_FIELD_NAME2 = NETWR
COMP_OPERATOR = >
VKORG = 1000
BACKDAYS = 7
```

**Use Case 2: Compare item net value to a fixed threshold in USD**

**Purpose:** Monitor item lines whose net value exceeds a configured amount after conversion to USD.

```
REF_TABNAME1 = VBAP
REF_FIELD_NAME1 = NETWR
RESULT_COMP1 = 10000
WAERK_FR = USD
VKORG = 1000
BACKDAYS = 14
```

**Use Case 3: Incomplete overall processing with delivery focus**

**Purpose:** Review orders with incomplete overall processing and delivery status indicating items not fully delivered.

```
GBSTK = B
LFSTK = B
SPART = 01
VKORG = 1000
BACKDAYS = 7
```

**Use Case 4: Sold-to partner with billing block status**

**Purpose:** Sample orders for one sold-to partner with overall billing block status requiring follow-up.

```
BP1_FUNCT = AG
BP1_CODE = 100000
FSSTK = A
AUART = TA
VKORG = 1000
BACKDAYS = 14
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
| /SKN/S_SW_10_01_ORD_STAT_CHK | ABGRU | Reason for rejection of quotations and sales orders | CHAR(2) | ABGRU |
| /SKN/S_SW_10_01_ORD_STAT_CHK | ABSTA | Release status (current release, old release) | CHAR(1) | ABSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | ABSTK | Overall rejection status of all document items | CHAR(1) | ABSTK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_ORD_STAT_CHK | ANGDT | Quotation/Inquiry is valid from | DATS(8) | ANGDT_V |
| /SKN/S_SW_10_01_ORD_STAT_CHK | ARKTX | Short text for sales order item | CHAR(40) | ARKTX |
| /SKN/S_SW_10_01_ORD_STAT_CHK | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_STAT_CHK | AUDAT | Document Date (Date Received/Sent) | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BESTK | Confirmation status | CHAR(1) | BESTK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BLOCK | Indicator: Document preselected for archiving | CHAR(1) | BLOCK_VB |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BNDDT | Date until which bid/quotation is binding (valid-to date) | DATS(8) | BNDDT |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP1_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP1_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP1_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP2_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP2_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP2_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP3_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP3_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ORD_STAT_CHK | BP3_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMGST | Overall status of credit checks | CHAR(1) | CMGST |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPRE | Item credit price | CURR(11,2) | CMPRE |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPRE_FLT | Item credit price | FLTP(16,16) | CMPRE_FLT |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPS0 | Status of credit check for customer reserve 1 | CHAR(1) | CMPS0 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPS1 | Status of credit check for customer reserve 2 | CHAR(1) | CMPS1 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPS2 | Status of credit check for customer reserve 3 | CHAR(1) | CMPS2 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSA | Status of static credit limit check | CHAR(1) | CMPSA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSB | Status of dynamic credit limit check in the credit horizon | CHAR(1) | CMPSB |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSC | Status of credit check against maximum document value | CHAR(1) | CMPSC |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSD | Status of credit check against terms of payment | CHAR(1) | CMPSD |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSE | Status of credit check against customer review date | CHAR(1) | CMPSE |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSF | Status of credit check against open items due | CHAR(1) | CMPSF |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSG | Status of credit check against oldest open items | CHAR(1) | CMPSG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSH | Status of credit check against highest dunning level | CHAR(1) | CMPSH |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSI | Status of credit check against financial document | CHAR(1) | CMPSI |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSJ | Status of credit check against export credit insurance | CHAR(1) | CMPSJ |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSK | Status of credit check against payment card authorization | CHAR(1) | CMPSK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSL | Status of credit check of reserves 4 | CHAR(1) | CMPSL |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPSM | Credit check data is obsolete | CHAR(1) | CMPSM |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPS_CM | Status of Credit Check SAP Credit Management | CHAR(1) | CMPS_CM |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CMPS_TE | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | COMP_OPERATOR | Consistency Checks - Comparison operator | CHAR(2) | BUCC_OPERATOR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | COSTA | Confirmation status for ALE | CHAR(1) | COSTA_D |
| /SKN/S_SW_10_01_ORD_STAT_CHK | CUST_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | DCSTK | Delay status | CHAR(1) | DCSTK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | DOC_TYPE_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | DUMMY | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ORD_STAT_CHK | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ORD_STAT_CHK | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_ORD_STAT_CHK | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_ORD_STAT_CHK | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_ORD_STAT_CHK | FAKSK | Billing block in SD document | CHAR(2) | FAKSK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | FKIVK | Billing totals status for intercompany billing | CHAR(1) | FKIVK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | FKREL | Relevant for Billing | CHAR(1) | FKREL |
| /SKN/S_SW_10_01_ORD_STAT_CHK | FKSAK | Billing status (order-related billing document) | CHAR(1) | FKSAK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | FSSTA | Billing block status for items | CHAR(1) | FSSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | FSSTK | Overall billing block status | CHAR(1) | FSSTK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | GBSTA | Overall processing status of the SD document item | CHAR(1) | GBSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | GBSTK | Overall processing status of document | CHAR(1) | GBSTK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | GUEBG | Valid-from date (outline agreements, product proposals) | DATS(8) | GUEBG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | GUEEN | Valid-to date (outline agreements, product proposals) | DATS(8) | GUEEN |
| /SKN/S_SW_10_01_ORD_STAT_CHK | INC_NOT_CONV_CURR | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KBMENG | Cumulative confirmed quantity in sales unit | QUAN(15,3) | KBMENG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KDMAT | Material Number Used by Customer | CHAR(35) | MATNR_KU |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KLMENG | Cumulative confirmed quantity in base unit | QUAN(15,3) | KLMENG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KOQUA | Confirmation status of picking/putaway | CHAR(1) | KOQUA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KOSTA | Picking status/Putaway status | CHAR(1) | KOSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KWMENG | Cumulative Order Quantity in Sales Units | QUAN(15,3) | KWMENG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KWMENG_INT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KZWI1 | Subtotal 1 from pricing procedure for condition | CURR(13,2) | KZWI1 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KZWI2 | Subtotal 2 from pricing procedure for condition | CURR(13,2) | KZWI2 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KZWI3 | Subtotal 3 from pricing procedure for condition | CURR(13,2) | KZWI3 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KZWI4 | Subtotal 4 from pricing procedure for condition | CURR(13,2) | KZWI4 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KZWI5 | Subtotal 5 from pricing procedure for condition | CURR(13,2) | KZWI5 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | KZWI6 | Subtotal 6 from pricing procedure for condition | CURR(13,2) | KZWI6 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | LFGSK | Overall delivery status for all items | CHAR(1) | LKGSK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | LFREL | Item is relevant for delivery | CHAR(1) | LFREL_AP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | LFSTK | Delivery status | CHAR(1) | LFSTK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | LIFSK | Delivery block (document header) | CHAR(2) | LIFSK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | LSMENG | Cumulative required delivery qty (all dlv-relev.sched.lines) | QUAN(15,3) | LSMENG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | LSSTA | Delivery block status for item | CHAR(1) | LSSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | LSSTK | Overall delivery block status | CHAR(1) | LSSTK_G |
| /SKN/S_SW_10_01_ORD_STAT_CHK | MANEK | Manual Completion of Contract | CHAR(1) | MANEK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_01_ORD_STAT_CHK | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_01_ORD_STAT_CHK | MPROK | Status manual price change | CHAR(1) | MPROK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | MPROK_DESC | Explanatory short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_10_01_ORD_STAT_CHK | MWSBP | Tax amount in document currency | CURR(13,2) | MWSBP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | NETPR | Net price | CURR(11,2) | NETPR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | NETPR_VAT | Net price | CURR(11,2) | NETPR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | NETWR | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | NETWR_VAT | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | PDSTA | POD status on item level | CHAR(1) | PDSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | PKSTA | Packing status of item | CHAR(1) | PKSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | PRODH | Product hierarchy | CHAR(18) | PRODH_D |
| /SKN/S_SW_10_01_ORD_STAT_CHK | REF_FIELD_NAME1 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_01_ORD_STAT_CHK | REF_FIELD_NAME2 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_01_ORD_STAT_CHK | RFGSK | Total reference status of all items | CHAR(1) | RFGSK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | RFSTK | Reference document header status | CHAR(1) | RFSTK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | RRSTA | Revenue determination status | CHAR(1) | RR_STATUS |
| /SKN/S_SW_10_01_ORD_STAT_CHK | SALES_GRP_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | SALES_OFF_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | SALES_ORG_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | SAPRL | SAP Release | CHAR(4) | SAPRL |
| /SKN/S_SW_10_01_ORD_STAT_CHK | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_STAT_CHK | SPSTG | Overall blocked status | CHAR(1) | SPSTG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | TRSTA | Transportation planning status | CHAR(1) | TRSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UEPOS | Higher-level item in bill of material structures | NUMC(6) | UEPOS |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVALL | General incompletion status of the header | CHAR(1) | UVALL_UK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVALS | Total incompletion status of all items in general | CHAR(1) | UVALL_SU |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVFAK | Header incompletion status with respect to billing | CHAR(1) | UVFAK_UK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVFAS | Total incompletion status of all items: Billing | CHAR(1) | UVFAK_SU |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVK01 | Customer reserves 1: Header status | CHAR(1) | UVK01 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVK02 | Customer reserves 2: Header status | CHAR(1) | UVK02 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVK03 | Customer reserves 3: Header status | CHAR(1) | UVK03 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVK04 | Custmer reserves 4: Header status | CHAR(1) | UVK04 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVK05 | Customer reserves 5: Header status | CHAR(1) | UVK05 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVP01 | Customer reserves 1: Item status | CHAR(1) | UVP01 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVP02 | Customer reserves 2: Item status | CHAR(1) | UVP02 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVP03 | Item reserves 3: Item status | CHAR(1) | UVP03 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVP04 | Item reserves 4: Item status | CHAR(1) | UVP04 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVP05 | Customer reserves 5: Item status | CHAR(1) | UVP05 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVPAK | Header incomplete status for packaging | CHAR(1) | UVPAK_UK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVPIK | Header incomplete status for picking/putaway | CHAR(1) | UVPIK_UK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVPRS | Document is incomplete with respect to pricing | CHAR(1) | UVPRS_UK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVS01 | Customer reserves 1: Sum of all items | CHAR(1) | UVS01 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVS02 | Customer reserves 2: Sum of all items | CHAR(1) | UVS02 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVS03 | Customer reserves 3: Sum of all items | CHAR(1) | UVS03 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVS04 | Customer reserves 4: Sum of all items | CHAR(1) | UVS04 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVS05 | Customer reserves 5: Sum of all items | CHAR(1) | UVS05 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVVLK | Header incompletion status concerning delivery | CHAR(1) | UVVLK_UK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVVLS | Total incompletion status of all items: Delivery | CHAR(1) | UVVLS_SU |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVWAK | Post header incomplete status for goods movement | CHAR(1) | UVWAK_UK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | UVWAS | Total incomplete status of all items: post goods movement | CHAR(1) | UVWAK_SU |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VALID_ONLY | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VALUE_COMP1 |  | CURR(15,2) |  |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VALUE_COMP2 |  | CURR(15,2) |  |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VBOBJ | SD document object | CHAR(1) | VBOBJ |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VDATU | Requested delivery date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VGTYP | Document category of preceding SD document | CHAR(1) | VBTYP_V |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VRKME | Sales unit | UNIT(3) | VRKME |
| /SKN/S_SW_10_01_ORD_STAT_CHK | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_STAT_CHK | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_STAT_CHK | WAERK_FR |  | CUKY(5) |  |
| /SKN/S_SW_10_01_ORD_STAT_CHK | WAVWR | Cost in document currency | CURR(13,2) | WAVWR |
| /SKN/S_SW_10_01_ORD_STAT_CHK | WBSTA | Goods movement status | CHAR(1) | WBSTA |
| /SKN/S_SW_10_01_ORD_STAT_CHK | WERKS | Plant (Own or External) | CHAR(4) | WERKS_EXT |
| /SKN/S_SW_10_01_ORD_STAT_CHK | ZWERT | Target Value for Outline Agreement in Document Currency | CURR(13,2) | DZWERT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ORD_STAT_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_STAT_CHK OPTIONAL
*"----------------------------------------------------------------------
  INCLUDE /SKN/PC_SW_AI_TOP.
  TYPES: BEGIN OF TY_WAERK,
           WAERK  TYPE VBAP-WAERK,
         END OF TY_WAERK,
         TT_WAERK TYPE STANDARD TABLE OF TY_WAERK.
  DATA_SINGLE: MANAGE_IN_UTC     CHAR1,
               LANGU             LANGU,
               BACKDAYS          INT4,
               FORWDAYS          INT4,
               BP1_FUNCT         PARVW,
               BP2_FUNCT         PARVW,
               BP3_FUNCT         PARVW,
               DATE_REF_FLD      NAME_FELD,
               DURATION_UNIT     /SKN/E_SW_DURATION_UNIT,
               ITEM_DETAILS      CHAR1,
               REF_TABNAME1      TABNAME,
               REF_TABNAME2      TABNAME,
               REF_FIELD1        NAME_FELD,
               REF_FIELD2        NAME_FELD,
               COMP_OPERATOR     BUCC_OPERATOR,
               WAERK_FR          WAERK,
               INC_NOT_CONV_CURR BOOLE_D.                  " ++ 06.10.20
  LV_BACKDAYS      = 1.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU         = SY-LANGU.
  LV_COMP_OPERATOR = '>'.
  LV_WAERK_FR      = 'USD'.
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
                 ITEM_DETAILS,
                 REF_TABNAME1,
                 REF_TABNAME2,
                 REF_FIELD1,
                 REF_FIELD2,
                 COMP_OPERATOR,
                 WAERK_FR,
                 INC_NOT_CONV_CURR.      " ++ 06.10.20
  DATA_MULTY: KUNNR        VBAK-KUNNR,
              VBELN        VBAK-VBELN,
              VKORG        VBAK-VKORG,
              VTWEG        VBAK-VTWEG,
              VBTYP        VBTYP,
              AUART        VBAK-AUART,
              AUDAT        VBAK-AUDAT,
              AEDAT        VBAK-AEDAT,
              GUEBG        GUEBG,          " Valid-from date contract
              GUEEN        GUEEN,          " Valid-to date contract
              ANGDT        ANGDT_V,        " Valid-from date quatetion
              BNDDT        BNDDT,          " Valid-to date quatetion
              VDATU        VBAK-VDATU,     " Requested delivery date
              ERNAM        VBAK-ERNAM,
              DATUM        SY-DATUM,
              DURATION    /SKN/E_SW_DURATION,
*** VBUK
              RFSTK        RFSTK,
              RFGSK        RFGSK,
              BESTK        BESTK,
              LFSTK        LFSTK,
              LFGSK        LKGSK,
              FKSAK        FKSAK,
              ABSTK        ABSTK,
              GBSTK        GBSTK,
              UVALS        UVVLS_SU,
              UVVLS        UVVLS_SU,
              UVFAS        UVFAK_SU,
              UVALL        UVALL_UK,
              UVVLK        UVVLK_UK,
              UVFAK        UVFAK_UK,
              UVPRS        UVPRS_UK,
              CMPSA        CMPSA,
              CMPSB        CMPSB,
              CMPSD        CMPSD,
              CMPSE        CMPSE,
              CMPSF        CMPSF,
              CMPSG        CMPSG,
              CMPSH        CMPSH,
              CMPSI        CMPSI,
              CMPSJ        CMPSJ,
              CMPSK        CMPSK,
              CMPSL        CMPSL,
              CMPS0        CMPS0,
              CMGST        CMGST,
              COSTA        COSTA_D,
              SPSTG        SPSTG,
              FSSTK        FSSTK,
              LSSTK        LSSTK,
              BLOCK        BLOCK_VB,
              FKIVK        FKIVK,
              TRSTA        TRSTA,
              UVWAS        UVWAK_SU,
              UVPAK        UVPAK_UK,
              UVPIK        UVPIK_UK,
              UVWAK        UVWAK_UK,
              CMPSM        CMPSM,
              DCSTK        DCSTK,
              CMPS_CM      CMPS_CM,
              CMPS_TE      CHAR1,
              VKGRP        VKGRP,
              VKBUR        VKBUR,
              LIFSK        LIFSK,
              FAKSK        FAKSK,
              BP1_CODE     KUNNR,
              BP2_CODE     KUNNR,
              BP3_CODE     KUNNR,
              BP_FUNCT     PARVW,
*** VBUK
**** VBAP
              POSNR        POSNR_VA,
              MATNR        VBAP-MATNR,
              SPART        VBAP-SPART,
              ERDAT        VBAP-ERDAT,
              MPROK        VBAP-MPROK,
              WAVWR        VBAP-WAVWR,
              MATKL        VBAP-MATKL,
              LFREL        LFREL_AP,
              FKREL        VBAP-FKREL,
              UEPOS        VBAP-UEPOS,
              ABGRU        ABGRU_VA,
              PRODH        PRODH_D,
              KDMAT        MATNR_KU,
              WERKS        WERKS_EXT,
              VGTYP        VBTYP_V,
**** VBAP
              RESULT_COMP1 NETWR_AP,
              RESULT_COMP2 NETWR_AP.
  SELECT_MULTY: KUNNR,
                VBELN,
                VKORG,
                VTWEG,
                VBTYP,
                AUART,
                AUDAT,
                AEDAT,
                GUEBG,
                GUEEN,
                ANGDT,
                BNDDT,
                VDATU,
                ERNAM,
                DATUM,
                DURATION,
*** VBUK
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
*** VBUK
*** VBAP
                POSNR,
                MATNR,
                SPART,
                ERDAT,
                MPROK,
                WAVWR,
                MATKL,
                LFREL,
                FKREL,
                UEPOS,
                ABGRU,
                PRODH,
                KDMAT,
                WERKS,
                VGTYP,
*** VBAP
                BP1_CODE,
                BP2_CODE,
                BP3_CODE,
                RESULT_COMP1,
                RESULT_COMP2.
  CONVERT_MULTY: KUNNR    ALPHA,
                 VBELN    ALPHA,
                 BP1_CODE ALPHA,
                 BP2_CODE ALPHA,
                 BP3_CODE ALPHA,
                 MATNR    MATN1,
                 AUART    AUART.
  CONVERT_SINGLE:  BP1_FUNCT PARVW,
                   BP2_FUNCT PARVW,
                   BP3_FUNCT PARVW.
  DATA: FLD_NAME TYPE FIELDNAME.
  DATA: I TYPE I,
         CI(1) TYPE C,
         NFIELDS TYPE I VALUE 3.   "
  DATA: BACKDAYS  TYPE I ,
        FORWDAYS TYPE I,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO LIKE SY-DATUM .
  DATA: LANGU LIKE SY-LANGU .
  DATA: IS_OUT(1) TYPE C.
  DATA: TIME_DIFF TYPE  INT4 .
  DATA : W_DATA LIKE LINE OF T_DATA .
  DATA : WA_VBPA TYPE VBPA.
  DATA : LV_VBELN      TYPE VBELN,
         LV_POSNR      TYPE POSNR,
         LV_PARVW      TYPE PARVW,
         LV_KUNNR      TYPE KUNNR,
         LV_KUNNR_NAME TYPE NAME1_GP,
         LV_LIFNR      TYPE LIFNR,
         LV_LIFNR_NAME TYPE NAME1_GP,
         LV_PERNR      TYPE PERNR_D,
         LV_PERNR_NAME TYPE NAME1_GP,
         LV_NRART      TYPE NRART,
         LV_VBTYP      TYPE VBTYP,
         LV_FROM1      TYPE STRING,
         LV_FROM2      TYPE STRING,
         LV_COND       TYPE STRING,
         LV_FROM       TYPE STRING,
         LV_QUERY_CURR TYPE STRING,
         LV_QUERY1     TYPE /SKN/E_SW_ALIAS,
         LV_QUERY2     TYPE /SKN/E_SW_ALIAS,
         LV_QUERY      TYPE STRING,
         LV_TEXT1      TYPE STRING,
         LV_TEXT2      TYPE STRING,
         LV_ALIAS1     TYPE /SKN/E_SW_ALIAS,
         LV_ALIAS2     TYPE /SKN/E_SW_ALIAS,
         LV_ALIAS_CURR TYPE /SKN/E_SW_ALIAS,
         LV_SUBRC      TYPE SYSUBRC,
         LV_NOT_CONV   TYPE CHAR1,
         LV_EXIT       TYPE BOOLE_D,
         LV_OPEN       TYPE BOOLE_D,
         LV_RATE       TYPE UKURS_CURR,
         LV_TOTAL_SPEC TYPE VBAP-NETWR.
  DATA: LT_OPTION         TYPE TABLE OF RFC_DB_OPT,
        LT_OUT_WHERE_COND TYPE TABLE OF /SKN/S_SW_WHERE_TAB,
        LT_IN_RANGE	      TYPE TABLE OF /SKN/S_SW_RANGE_TAB,
        LT_SEL_FIELDS     TYPE /SKN/TT_SEL_FIELDS.
  DATA: LWA_OUT_WHERE_COND LIKE LINE OF LT_OUT_WHERE_COND,
        LWA_IN_RANGE       LIKE LINE OF LT_IN_RANGE,
        LS_OPTION          LIKE LINE OF LT_OPTION,
        LS_SEL_FIELDS      LIKE LINE OF LT_SEL_FIELDS.
*  DATA: rt_netwr TYPE tt_range_netwr,
*        rs_netwr TYPE ty_range_netwr.
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
  DATA: LS_VBAK TYPE VBAK,
        LT_VBAK LIKE TABLE OF LS_VBAK.
  DATA: LS_DATA LIKE LINE OF T_DATA,
        LT_DATA LIKE TABLE OF LS_DATA.
  DATA: LS_WAERK TYPE TY_WAERK.
  DATA: LT_WAERK TYPE TT_WAERK.
  DATA: LS_DD03L TYPE DD03L.
  DATA: LT_DD03L TYPE TABLE OF DD03L.
  DATA: LV_DOMNAME      LIKE DD07V-DOMNAME,
        LV_DOMVALUE     LIKE DD07V-DOMVALUE_L,
        LV_DDTEXT       LIKE DD07V-DDTEXT,
        LV_RESULT       TYPE NETWR_AP,
        LV_TYPE1        TYPE DATATYPE_D,
        LV_TYPE2        TYPE DATATYPE_D,
        LV_FIELD1_EXIST TYPE BOOLE_D,
        LV_FIELD2_EXIST TYPE BOOLE_D,
        LV_SEL_VBAK     TYPE STRING,
        LV_SEL_VBAP     TYPE STRING,
        LV_SEL_CLAUSE   TYPE STRING,
        LV_VAL          TYPE CHAR21,
        LV_LINES        TYPE I,
        LV_RESULT_TXT   TYPE STRING,
        LV_WHILE        TYPE BOOLE_D,
        LV_TABIX        TYPE SYTABIX,
        LV_TABIX_WHILE  TYPE STRING,
        LV_WAERK        TYPE WAERK,
        LV_AMOUNT       TYPE VBAP-NETWR,
        LV_AMOUNT_FR    LIKE VBAP-NETWR.
  DATA: LR_DATA     TYPE REF TO DATA,
        LR_DATATYPE TYPE REF TO CL_ABAP_DATADESCR.
  FIELD-SYMBOLS: <FS_FIELD> TYPE ANY,
                 <FS_AMOUNT> TYPE ANY,
                 <FS_WAERK>  TYPE TY_WAERK.
  FIELD-SYMBOLS: <FS_DATA>       LIKE LINE OF T_DATA[],
                 <FS_AMOUNT_TAB> TYPE TABLE,
                 <FS_AMOUNT_STR> TYPE ANY.
  IF NOT LV_FORWDAYS IS INITIAL.
    LV_BACKDAYS = LV_FORWDAYS * ( -1 ).
  ENDIF.
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  DATE_FROM = SY-DATUM.
  READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_DATUM-LOW.
    DATE_TO   = RS_DATUM-HIGH.
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
      RS_GUEBG-SIGN   = 'I' .
      RS_GUEBG-OPTION = 'LE'.   "'LE' .
      RS_GUEBG-LOW    = DATE_TO .
      APPEND RS_GUEBG TO R_GUEBG.
      RS_GUEEN-SIGN   = 'I' .
      RS_GUEEN-OPTION = 'GE'.   "'GE' .
      RS_GUEEN-LOW    = DATE_FROM .
      APPEND RS_GUEEN TO R_GUEEN.
    WHEN 'ANGDT' OR 'BNDDT'.
      RS_ANGDT-SIGN   = 'I' .
      RS_ANGDT-OPTION = 'LE'.    "'LE' .
      RS_ANGDT-LOW    = DATE_TO .
      APPEND RS_ANGDT TO R_ANGDT.
      RS_BNDDT-SIGN   = 'I' .
      RS_BNDDT-OPTION = 'GE' .   "'GE' .
      RS_BNDDT-LOW    = DATE_FROM .
      APPEND RS_BNDDT TO R_BNDDT.
    WHEN OTHERS.
      R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
      IF LV_DATE_REF_FLD IS INITIAL.
        LV_DATE_REF_FLD = 'ERDAT'.
      ENDIF.
  ENDCASE.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_ORD_STAT_CHK'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
**** Get all Currencies type of documents from VBAK Table
  IF LV_WAERK_FR IS NOT INITIAL AND
      ( R_RESULT_COMP1[] IS NOT INITIAL OR R_RESULT_COMP2[] IS NOT INITIAL ).
    REFRESH: LT_OUT_WHERE_COND.
*** VBAP
    _RANGE_TO_SEL_TABLE 'POSNR' POSNR.
    _RANGE_TO_SEL_TABLE 'MATNR' MATNR.
    _RANGE_TO_SEL_TABLE 'SPART' SPART.
    _RANGE_TO_SEL_TABLE 'MATKL' MATKL.
    _RANGE_TO_SEL_TABLE 'LFREL' LFREL.
    _RANGE_TO_SEL_TABLE 'FKREL' FKREL.
    _RANGE_TO_SEL_TABLE 'UEPOS' UEPOS.
    _RANGE_TO_SEL_TABLE 'ABGRU' ABGRU.
    _RANGE_TO_SEL_TABLE 'PRODH' PRODH.
    _RANGE_TO_SEL_TABLE 'KDMAT' KDMAT.
    _RANGE_TO_SEL_TABLE 'WERKS' WERKS.
    _RANGE_TO_SEL_TABLE 'VGTYP' VGTYP.
    _RANGE_TO_SEL_TABLE 'ERDAT' ERDAT.
    _RANGE_TO_SEL_TABLE 'AEDAT' AEDAT.
    _RANGE_TO_SEL_TABLE 'MPROK' MPROK.
**** VBAP ****
    LT_OPTION[] = LT_OUT_WHERE_COND[].
    SELECT WAERK
      FROM VBAP
      INTO TABLE LT_WAERK
      WHERE (LT_OPTION)
      GROUP BY WAERK.
  ENDIF.
**** Get all Currencies type of documents from VBAK Table
**************** Get Table field details ******************
  CLEAR: LT_OPTION[], LT_OUT_WHERE_COND[].
  CLEAR: LS_OPTION.
  IF LV_REF_TABNAME1 IS NOT INITIAL.
    IF LT_OPTION IS NOT INITIAL.
      CONCATENATE 'AND' 'TABNAME' 'EQ' INTO LV_QUERY SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'TABNAME' 'EQ' INTO LV_QUERY SEPARATED BY SPACE.
    ENDIF.
    CONCATENATE ''''LV_REF_TABNAME1''''  INTO LV_REF_TABNAME1.
    CONCATENATE LV_QUERY LV_REF_TABNAME1 INTO LS_OPTION-TEXT SEPARATED BY SPACE.
    APPEND LS_OPTION TO LT_OPTION.
  ENDIF.
  CLEAR: LS_OPTION.
  IF LV_REF_TABNAME2 IS NOT INITIAL.
    IF LT_OPTION IS NOT INITIAL.
      CONCATENATE 'AND' 'TABNAME' 'EQ' INTO LV_QUERY SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'TABNAME' 'EQ' INTO LV_QUERY SEPARATED BY SPACE.
    ENDIF.
    CONCATENATE ''''LV_REF_TABNAME2''''  INTO LV_REF_TABNAME2.
    CONCATENATE LV_QUERY LV_REF_TABNAME2 INTO LS_OPTION-TEXT SEPARATED BY SPACE.
    APPEND LS_OPTION TO LT_OPTION.
  ENDIF.
  IF LT_OPTION IS NOT INITIAL.
    SELECT *
      FROM DD03L
      INTO TABLE LT_DD03L
      WHERE (LT_OPTION).
    CLEAR: LS_OPTION,
           LT_OPTION.
    READ TABLE LT_DD03L INTO LS_DD03L WITH KEY FIELDNAME = LV_REF_FIELD1.
    IF SY-SUBRC = 0.
      LV_FIELD1_EXIST = 'X'.
      LV_TYPE1        = LS_DD03L-DATATYPE.
    ENDIF.
    CLEAR: LS_DD03L.
    READ TABLE LT_DD03L INTO LS_DD03L WITH KEY FIELDNAME = LV_REF_FIELD2.
    IF SY-SUBRC = 0.
      LV_FIELD2_EXIST = 'X'.
      LV_TYPE2        = LS_DD03L-DATATYPE.
    ENDIF.
    CLEAR: LV_RESULT, LV_DDTEXT.
    REPLACE ALL OCCURRENCES OF '''' IN LV_REF_TABNAME1 WITH ''.
    REPLACE ALL OCCURRENCES OF '''' IN LV_REF_TABNAME2 WITH ''.
    IF LV_FIELD1_EXIST EQ 'X'.
      CASE LV_REF_TABNAME1.
        WHEN 'VBAK'.
          LV_ALIAS1 = 'K'.
        WHEN 'VBAP'.
          LV_ALIAS1 = 'P'.
        WHEN OTHERS.
      ENDCASE.
      CONCATENATE LV_ALIAS1 LV_REF_FIELD1 INTO LV_QUERY1
        SEPARATED BY '~'.
    ENDIF.
    IF LV_FIELD2_EXIST EQ 'X'.
      IF LV_REF_TABNAME1 <> LV_REF_TABNAME2 AND LV_REF_TABNAME2 IS NOT INITIAL.
        CASE LV_REF_TABNAME2.
          WHEN 'VBAK'.
            LV_ALIAS2 = 'K'.
          WHEN 'VBAP'.
            LV_ALIAS2 = 'P'.
          WHEN OTHERS.
        ENDCASE.
      ELSE.
        LV_ALIAS2 = LV_ALIAS1.
      ENDIF.
      CONCATENATE LV_ALIAS2 LV_REF_FIELD2 INTO LV_QUERY2
        SEPARATED BY '~'.
    ENDIF.
  ENDIF.
**************** Get Table field details ******************
********* Prepare Condition Query *************************
  REFRESH: LT_OPTION[], LT_OUT_WHERE_COND[].
  CLEAR: LV_QUERY.
********* Prepare Condition Query *************************
**** Prepare Selection Clause *********
  _BUILD_SQL_SEL_CLAUSE 'VBAP' '/SKN/S_SW_10_01_ORD_STAT_CHK' 'P' '' LV_SEL_VBAP.
  _BUILD_SQL_SEL_CLAUSE 'VBAK' '/SKN/S_SW_10_01_ORD_STAT_CHK' 'K' '' LV_SEL_VBAK.
  CONCATENATE LV_SEL_VBAK LV_SEL_VBAP
    INTO LV_SEL_CLAUSE SEPARATED BY SPACE.
  SHIFT LV_SEL_CLAUSE LEFT DELETING LEADING SPACE.
**** Prepare FROM statement ***********
  CONCATENATE 'VBAK' 'AS' 'K' INTO LV_FROM1 SEPARATED BY SPACE.
  CONCATENATE 'VBAP' 'AS' 'P' INTO LV_FROM2 SEPARATED BY SPACE.
  CONCATENATE 'ON' 'K~VBELN' 'EQ' 'P~VBELN' INTO LV_COND SEPARATED BY SPACE.
  CONCATENATE LV_FROM1 'INNER JOIN' LV_FROM2 LV_COND INTO LV_FROM
    SEPARATED BY SPACE.
**** VBAK *****
  _RANGE_TO_SEL_TABLE 'K~VBELN'   VBELN.
  _RANGE_TO_SEL_TABLE 'K~KUNNR'   KUNNR.
  _RANGE_TO_SEL_TABLE 'K~VKORG'   VKORG.
  _RANGE_TO_SEL_TABLE 'K~VKGRP'   VKGRP.
  _RANGE_TO_SEL_TABLE 'K~VKBUR'   VKBUR.
  _RANGE_TO_SEL_TABLE 'K~VTWEG'   VTWEG.
*  _range_to_sel_table 'K~SPART'   spart.
*  _range_to_sel_table 'K~ERDAT'   erdat.
  _RANGE_TO_SEL_TABLE 'K~AUDAT'   AUDAT.
  _RANGE_TO_SEL_TABLE 'K~AEDAT'   AEDAT.
  _RANGE_TO_SEL_TABLE 'K~VDATU'   VDATU.
  _RANGE_TO_SEL_TABLE 'K~AUART'   AUART.
  _RANGE_TO_SEL_TABLE 'K~GUEBG'   GUEBG .
  _RANGE_TO_SEL_TABLE 'K~GUEEN'   GUEEN.
  _RANGE_TO_SEL_TABLE 'K~ANGDT'   ANGDT.
  _RANGE_TO_SEL_TABLE 'K~BNDDT'   BNDDT.
  _RANGE_TO_SEL_TABLE 'K~LIFSK'   LIFSK.
  _RANGE_TO_SEL_TABLE 'K~FAKSK'   FAKSK.
  _RANGE_TO_SEL_TABLE 'K~VBTYP'   VBTYP.
  _RANGE_TO_SEL_TABLE 'K~ERNAM'   ERNAM.
  _RANGE_TO_SEL_TABLE 'K~RFSTK'   RFSTK.
  _RANGE_TO_SEL_TABLE 'K~RFGSK'   RFGSK.
  _RANGE_TO_SEL_TABLE 'K~BESTK'   BESTK.
  _RANGE_TO_SEL_TABLE 'K~LFSTK'   LFSTK.
  _RANGE_TO_SEL_TABLE 'K~LFGSK'   LFGSK.
  _RANGE_TO_SEL_TABLE 'K~FKSAK'   FKSAK.
  _RANGE_TO_SEL_TABLE 'K~ABSTK'   ABSTK.
  _RANGE_TO_SEL_TABLE 'K~GBSTK'   GBSTK.
  _RANGE_TO_SEL_TABLE 'K~UVALS'   UVALS.
  _RANGE_TO_SEL_TABLE 'K~UVVLS'   UVVLS.
  _RANGE_TO_SEL_TABLE 'K~UVFAS'   UVFAS.
  _RANGE_TO_SEL_TABLE 'K~UVALL'   UVALL.
  _RANGE_TO_SEL_TABLE 'K~UVVLK'   UVVLK.
  _RANGE_TO_SEL_TABLE 'K~UVFAK'   UVFAK.
  _RANGE_TO_SEL_TABLE 'K~UVPRS'   UVPRS.
  _RANGE_TO_SEL_TABLE 'K~CMPSA'   CMPSA.
  _RANGE_TO_SEL_TABLE 'K~CMPSB'   CMPSB.
  _RANGE_TO_SEL_TABLE 'K~CMPSD'   CMPSD.
  _RANGE_TO_SEL_TABLE 'K~CMPSE'   CMPSE.
  _RANGE_TO_SEL_TABLE 'K~CMPSF'   CMPSF.
  _RANGE_TO_SEL_TABLE 'K~CMPSG'   CMPSG.
  _RANGE_TO_SEL_TABLE 'K~CMPSH'   CMPSH.
  _RANGE_TO_SEL_TABLE 'K~CMPSI'   CMPSI.
  _RANGE_TO_SEL_TABLE 'K~CMPSJ'   CMPSJ.
  _RANGE_TO_SEL_TABLE 'K~CMPSK'   CMPSK.
  _RANGE_TO_SEL_TABLE 'K~CMPSL'   CMPSL.
  _RANGE_TO_SEL_TABLE 'K~CMPS0'   CMPS0.
  _RANGE_TO_SEL_TABLE 'K~CMGST'   CMGST.
  _RANGE_TO_SEL_TABLE 'K~COSTA'   COSTA.
  _RANGE_TO_SEL_TABLE 'K~SPSTG'   SPSTG.
  _RANGE_TO_SEL_TABLE 'K~FSSTK'   FSSTK.
  _RANGE_TO_SEL_TABLE 'K~LSSTK'   LSSTK.
  _RANGE_TO_SEL_TABLE 'K~BLOCK'   BLOCK.
  _RANGE_TO_SEL_TABLE 'K~FKIVK'   FKIVK.
  _RANGE_TO_SEL_TABLE 'K~TRSTA'   TRSTA.
  _RANGE_TO_SEL_TABLE 'K~UVWAS'   UVWAS.
  _RANGE_TO_SEL_TABLE 'K~UVPAK'   UVPAK.
  _RANGE_TO_SEL_TABLE 'K~UVPIK'   UVPIK.
  _RANGE_TO_SEL_TABLE 'K~UVWAK'   UVWAK.
  _RANGE_TO_SEL_TABLE 'K~CMPSM'   CMPSM.
  _RANGE_TO_SEL_TABLE 'K~DCSTK'   DCSTK.
  _RANGE_TO_SEL_TABLE 'K~CMPS_CM' CMPS_CM.
**** VBAK *****
**** VBAP *****
  _RANGE_TO_SEL_TABLE 'P~POSNR'  POSNR.
  _RANGE_TO_SEL_TABLE 'P~MATNR'  MATNR.
  _RANGE_TO_SEL_TABLE 'P~ERDAT'  ERDAT.
  _RANGE_TO_SEL_TABLE 'P~SPART'  SPART.
  _RANGE_TO_SEL_TABLE 'P~MATKL'  MATKL.
  _RANGE_TO_SEL_TABLE 'P~LFREL'  LFREL.
  _RANGE_TO_SEL_TABLE 'P~FKREL'  FKREL.
  _RANGE_TO_SEL_TABLE 'P~UEPOS'  UEPOS.
  _RANGE_TO_SEL_TABLE 'P~ABGRU'  ABGRU.
  _RANGE_TO_SEL_TABLE 'P~PRODH'  PRODH.
  _RANGE_TO_SEL_TABLE 'P~KDMAT'  KDMAT.
  _RANGE_TO_SEL_TABLE 'P~WERKS'  WERKS.
  _RANGE_TO_SEL_TABLE 'P~VGTYP'  VGTYP.
**** VBAP *****
  APPEND LINES OF LT_OUT_WHERE_COND TO LT_OPTION.
  CLEAR: LS_OPTION.
****
  IF LV_FIELD1_EXIST EQ 'X' AND LV_FIELD2_EXIST EQ 'X' AND
       R_RESULT_COMP1[] IS INITIAL AND R_RESULT_COMP2[] IS INITIAL.
* Compare between parameter field1 and field2, with compare operator from parameter screen
    IF LV_COMP_OPERATOR IS NOT INITIAL.
      CLEAR: LV_QUERY.
      CONCATENATE LV_QUERY1 LV_COMP_OPERATOR LV_QUERY2 INTO LV_QUERY
        SEPARATED BY SPACE.
    ENDIF.
    IF LV_QUERY IS NOT INITIAL.
      IF LT_OPTION IS NOT INITIAL.
        CONCATENATE 'AND' LV_QUERY INTO LS_OPTION-TEXT
          SEPARATED BY SPACE.
      ELSE.
        LS_OPTION-TEXT = LV_QUERY.
      ENDIF.
      APPEND LS_OPTION TO LT_OPTION.
    ENDIF.
    SELECT (LV_SEL_CLAUSE)
      FROM (LV_FROM)         " vbak as k inner join vbap as p on k~vbeln EQ p~vbeln
      INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
      WHERE (LT_OPTION).
* Compare parameter field name with result_comp(parameter value) - if just 1 parameter exist
  ELSEIF LV_FIELD1_EXIST EQ 'X' AND R_RESULT_COMP1[] IS NOT INITIAL.
****** Currency conversion
    LV_ALIAS_CURR = 'K'.
    DESCRIBE TABLE LT_VBAK LINES LV_LINES.
    LV_WHILE = 'X'.
    WHILE LV_WHILE EQ 'X'.
      CLEAR: LV_AMOUNT.
      LV_TABIX_WHILE = LV_TABIX_WHILE + 1.
      CONCATENATE 'R_RESULT_COMP' ''LV_TABIX_WHILE'' INTO LV_RESULT_TXT.
      LR_DATATYPE ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( LV_RESULT_TXT ).
      IF SY-SUBRC = 0 AND LR_DATATYPE IS BOUND.
        FREE LR_DATATYPE.
        ASSIGN (LV_RESULT_TXT) TO <FS_AMOUNT_TAB>.
      ELSE.
        CLEAR: LV_WHILE.
        EXIT.
      ENDIF.
      IF <FS_AMOUNT_TAB> IS ASSIGNED AND <FS_AMOUNT_TAB> IS NOT INITIAL.
        READ TABLE <FS_AMOUNT_TAB> ASSIGNING <FS_AMOUNT_STR> INDEX LV_TABIX_WHILE.
        IF SY-SUBRC = 0 AND <FS_AMOUNT_STR> IS ASSIGNED.
          ASSIGN COMPONENT 'LOW' OF STRUCTURE <FS_AMOUNT_STR> TO <FS_FIELD>.
          IF SY-SUBRC = 0 AND <FS_FIELD> IS ASSIGNED.
            LV_AMOUNT = <FS_FIELD>.
          ENDIF.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
      LOOP AT LT_WAERK INTO LS_WAERK.
        LV_TABIX = SY-TABIX.
        CLEAR: LV_WAERK, LT_OPTION, LS_OPTION, LV_SUBRC, LV_EXIT.
        LV_OPEN = 'X'.
        LV_AMOUNT = RS_RESULT_COMP1-LOW.
        LV_WAERK  = LS_WAERK-WAERK.
        APPEND LINES OF LT_OUT_WHERE_COND TO LT_OPTION.
        IF LV_WAERK_FR IS NOT INITIAL AND LV_WAERK IS NOT INITIAL AND
             LV_AMOUNT IS NOT INITIAL.
*    _range_to_sel_table lv_query1 result_comp1.
          LOOP AT R_RESULT_COMP1 INTO RS_RESULT_COMP1.
            CLEAR: LV_AMOUNT_FR, LS_OPTION, LV_QUERY_CURR, LV_TEXT1,
                   LV_TEXT2, LV_VAL, LV_SUBRC.
            IF LV_AMOUNT <> 0.
              IF LV_WAERK <> LV_WAERK_FR.
* Unit conversion for REF_FIELD1
                CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
                  EXPORTING
                    DATE             = SY-DATUM
                    FOREIGN_CURRENCY = LV_WAERK       " Document Curr
                    LOCAL_AMOUNT     = LV_AMOUNT
                    LOCAL_CURRENCY   = LV_WAERK_FR    " Foreign Curr
                  IMPORTING
*                    exchange_rate
                    FOREIGN_AMOUNT   = LV_AMOUNT_FR
                  EXCEPTIONS
                    NO_RATE_FOUND    = 1
                    OVERFLOW         = 2
                    NO_FACTORS_FOUND = 3
                    NO_SPREAD_FOUND  = 4
                    DERIVED_2_TIMES  = 5
                    OTHERS           = 6.
                LV_SUBRC = SY-SUBRC.
              ELSE.
                LV_AMOUNT_FR = LV_AMOUNT.
              ENDIF.
              IF LV_AMOUNT_FR IS NOT INITIAL OR
                   LV_INC_NOT_CONV_CURR EQ 'X'.
*** ++ 06.10.20
                IF LV_AMOUNT_FR IS INITIAL AND LV_INC_NOT_CONV_CURR EQ 'X'.
                  LV_NOT_CONV = 'X'.
                ELSE.
                  CLEAR: LV_NOT_CONV.
                ENDIF.
*** ++ 06.10.20
                IF LT_OPTION IS NOT INITIAL.
                  LS_OPTION-TEXT = 'AND'.
                  APPEND LS_OPTION TO LT_OPTION.
                ENDIF.
                CLEAR: LS_OPTION.
                IF LV_OPEN = 'X'.
                  CLEAR LV_OPEN.
                  LS_OPTION-TEXT = '('.
                  APPEND LS_OPTION TO LT_OPTION.
                ENDIF.
                CLEAR: LS_OPTION.
                LV_VAL = LV_AMOUNT_FR.
                SHIFT LV_VAL RIGHT DELETING TRAILING SPACE.
                SHIFT LV_VAL LEFT DELETING LEADING SPACE.
                CONCATENATE ''''LV_WAERK'''' INTO LV_QUERY_CURR.
                CONCATENATE LV_ALIAS_CURR '~' 'WAERK' INTO LV_TEXT1.
                CONCATENATE LV_TEXT1 'EQ' LV_QUERY_CURR INTO LV_TEXT2
                  SEPARATED BY SPACE.
                CONCATENATE ''''LV_VAL'''' INTO LV_VAL IN CHARACTER MODE.
                CONCATENATE LV_TEXT2 'AND'
                            LV_QUERY1 RS_RESULT_COMP1-OPTION LV_VAL
                  INTO LS_OPTION-TEXT SEPARATED BY SPACE.
****** Conversion 'HIGH' value
                IF <FS_AMOUNT_STR> IS ASSIGNED AND <FS_AMOUNT_STR> IS NOT INITIAL.
                  ASSIGN COMPONENT 'HIGH' OF STRUCTURE <FS_AMOUNT_STR> TO <FS_FIELD>.
                  IF SY-SUBRC = 0 AND <FS_FIELD> IS ASSIGNED.
                    LV_AMOUNT = <FS_FIELD>.
                  ENDIF.
                ENDIF.
                IF LV_AMOUNT <> 0.
                  CLEAR: LV_AMOUNT_FR.
* Unit conversion for REF_FIELD1
                  CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
                    EXPORTING
                      DATE             = SY-DATUM
                      FOREIGN_CURRENCY = LV_WAERK       " Document Curr
                      LOCAL_AMOUNT     = LV_AMOUNT
                      LOCAL_CURRENCY   = LV_WAERK_FR    " Foreign Curr
                    IMPORTING
                      FOREIGN_AMOUNT   = LV_AMOUNT_FR
                    EXCEPTIONS
                      NO_RATE_FOUND    = 1
                      OVERFLOW         = 2
                      NO_FACTORS_FOUND = 3
                      NO_SPREAD_FOUND  = 4
                      DERIVED_2_TIMES  = 5
                      OTHERS           = 6.
                  IF SY-SUBRC = 0 AND LV_AMOUNT_FR IS NOT INITIAL.
                    LV_VAL = LV_AMOUNT_FR.
                    CONCATENATE ''''LV_VAL'''' INTO LV_VAL.
                    CONCATENATE LS_OPTION-TEXT 'AND' LV_VAL "')'
                      INTO LS_OPTION-TEXT SEPARATED BY SPACE.
                  ENDIF.
                ENDIF.
****** Conversion 'HIGH' value
              ELSE.
                LV_SUBRC = SY-SUBRC.
*** ++ 06.10.20
                LV_EXIT = 'X'.
                EXIT.
*** ++ 06.10.20
              ENDIF.
            ENDIF.
            IF LV_OPEN IS INITIAL.
              CONCATENATE LS_OPTION-TEXT ')' INTO LS_OPTION-TEXT
                SEPARATED BY SPACE.
            ENDIF.
            IF LS_OPTION IS NOT INITIAL.
              APPEND LS_OPTION TO LT_OPTION.
            ENDIF.
          ENDLOOP.       " RESULT_COMP P.S.
        ELSE.
        ENDIF.           " Compare Currency
*** ++ 06.10.20
        IF LV_EXIT EQ 'X'.
          CONTINUE.
        ENDIF.
*** ++ 06.10.20
        IF LV_SUBRC = 0.
          SELECT (LV_SEL_CLAUSE)
            FROM (LV_FROM)         " vbak as k inner join vbap as p on k~vbeln EQ p~vbeln
            INTO CORRESPONDING FIELDS OF TABLE LT_DATA
            WHERE (LT_OPTION).
        ENDIF.
        IF LT_DATA IS NOT INITIAL.
*** ++ 06.10.20
          IF LV_NOT_CONV IS NOT INITIAL.
            LOOP AT LT_DATA ASSIGNING <FS_DATA>.
              <FS_DATA>-INC_NOT_CONV_CURR = LV_NOT_CONV.
            ENDLOOP.
          ENDIF.
*** ++ 06.10.20
          APPEND LINES OF LT_DATA TO T_DATA[].
          CLEAR: LT_DATA.
        ENDIF.
      ENDLOOP.        " WAERK
****** Currency conversion **************
    ENDWHILE.
*** 01.12.20 ++
  ELSE.
    SELECT (LV_SEL_CLAUSE)
    FROM (LV_FROM)         " vbak as k inner join vbap as p on k~vbeln EQ p~vbeln
    INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
    WHERE (LT_OPTION).
*** 01.12.20 ++
  ENDIF.
  CHECK T_DATA[] IS NOT INITIAL.
  SORT T_DATA BY VBELN POSNR.
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
          D_FROM      = REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT  "'D'
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
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION .
******************************************************************************
  "--- Get BPs
  IF T_DATA[] IS NOT INITIAL.
*    "--- Fill R_BP_FUNCT ----
    REFRESH R_BP_FUNCT.
    SET_BP_RANGE 1.
    SET_BP_RANGE 2.
    SET_BP_RANGE 3.
    IF R_BP_FUNCT[] IS NOT INITIAL.
*
      SELECT *
        FROM VBPA
        INTO CORRESPONDING FIELDS OF TABLE LT_VBPA
        FOR ALL ENTRIES IN T_DATA[]
        WHERE VBELN EQ T_DATA-VBELN
        AND   PARVW IN R_BP_FUNCT.
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
  CHECK T_DATA[] IS NOT INITIAL.
  REFRESH LT_DATA.
  LT_DATA[] = T_DATA[].
  REFRESH T_DATA.
  SORT LT_DATA BY VBELN.
  READ TABLE R_RESULT_COMP1 INTO RS_RESULT_COMP1 INDEX 1.
  READ TABLE R_RESULT_COMP2 INTO RS_RESULT_COMP2 INDEX 1.
  LOOP AT LT_DATA INTO LS_DATA.
    LS_DATA-REF_FIELD_NAME1 = LV_REF_FIELD1.
    IF RS_RESULT_COMP1 IS NOT INITIAL.
      LS_DATA-VALUE_COMP1    = RS_RESULT_COMP1-LOW.
    ENDIF.
    LS_DATA-REF_FIELD_NAME2 = LV_REF_FIELD2.
    IF RS_RESULT_COMP2 IS NOT INITIAL.
      LS_DATA-VALUE_COMP2    = RS_RESULT_COMP2-LOW.
    ENDIF.
    IF RS_RESULT_COMP1 IS INITIAL AND RS_RESULT_COMP2 IS INITIAL.
      LS_DATA-COMP_OPERATOR = LV_COMP_OPERATOR.
    ELSE.
      LS_DATA-COMP_OPERATOR = RS_RESULT_COMP1-OPTION.
    ENDIF.
    LS_DATA-WAERK_FR = LV_WAERK_FR.
* Get Descriptions
    IF LS_DATA-KUNNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
        EXPORTING
          KUNNR          = T_DATA-KUNNR
        IMPORTING
          CUST_DESC      = LS_DATA-CUST_DESC
        EXCEPTIONS
          WRONG_CUSTOMER = 1
          OTHERS         = 2.
    ENDIF.
    IF LS_DATA-MPROK IS NOT INITIAL.
      LV_DOMNAME = 'MPROK'.
      LV_DOMVALUE = LS_DATA-MPROK.
      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
        EXPORTING
          I_DOMNAME  = LV_DOMNAME
          I_DOMVALUE = LV_DOMVALUE
          LANGU      = LV_LANGU
*         SW_DEST    =
        IMPORTING
          E_DDTEXT   = LV_DDTEXT
        EXCEPTIONS
          NOT_EXIST  = 1
          OTHERS     = 2.
      IF SY-SUBRC = 0.
        LS_DATA-MPROK_DESC = LV_DDTEXT.
      ENDIF.
    ENDIF.
    IF LS_DATA-AUART IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_DOC_TYPE_DESC'
        EXPORTING
          AUART      = LS_DATA-AUART
          LANGU      = LV_LANGU
        IMPORTING
          TYPE_DESC  = LS_DATA-DOC_TYPE_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
    IF LS_DATA-VKORG IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_SALES_ORG_DESC'
        EXPORTING
          VKORG          = LS_DATA-VKORG
          LANGU          = LV_LANGU
        IMPORTING
          SALES_ORG_DESC = LS_DATA-SALES_ORG_DESC
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
    IF LS_DATA-VKGRP IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_SALES_GRP_DESC'
        EXPORTING
          VKGRP          = LS_DATA-VKGRP
          LANGU          = SY-LANGU
        IMPORTING
          SALES_GRP_DESC = LS_DATA-SALES_GRP_DESC
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
    IF LS_DATA-VKBUR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_SALES_OFF_DESC'
        EXPORTING
          VKBUR          = LS_DATA-VKBUR
          LANGU          = LV_LANGU
        IMPORTING
          SALES_OFF_DESC = LS_DATA-SALES_OFF_DESC
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
    APPEND LS_DATA TO T_DATA[].
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
