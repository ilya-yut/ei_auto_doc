# Exception Indicator: SD Delivery Status (General) - SW_10_01_DLV_STAT

## General Overview

This Exception Indicator (EI) monitors delivery documents to identify deliveries that are blocked due to credit problems (e.g. credit block, billing block) in a given sales organization. It provides visibility into blocked deliveries by date, organizational dimension, and status, supporting release decisions, cash flow, and revenue recognition.

This EI serves as an essential control for order-to-cash and credit management by:
- Enabling detection of deliveries blocked by credit or billing blocks that require release or escalation
- Supporting identification of blocked deliveries by sales organization, distribution channel, division, customer, and partner role for accountability and prioritization
- Providing visibility into status duration and reference date (e.g. planned goods movement date, delivery date) for root-cause and aging analysis
- Enabling analysis of blocked delivery value and quantity for impact assessment and management reporting
- Supporting release coordination and month-end close by surfacing blocked deliveries that delay revenue recognition

Monitoring blocked deliveries helps organizations prioritize credit releases, align with dunning and collection processes, and reduce revenue leakage. The EI is particularly valuable for credit management, month-end close, and exception handling in sales and distribution.

The EI uses delivery header (LIKP), delivery item (LIPS), status (VBUK, VBUP), and sales order (VBAK) data and supports filtering by delivery type, sales org, distribution channel, division, date range, and status.


## Problem Description

Failure to monitor deliveries blocked due to credit problems creates multiple risks across revenue recognition, operational control, and compliance:

**Financial and Reporting Issues**
- Unreleased credit-blocked deliveries can delay revenue recognition and distort period-end reporting
- Blocked delivery value and quantity without visibility may cause cash flow and forecasting errors
- Lack of a clear view of block duration and scope complicates month-end close and audit evidence

**Operational and Control Risks**
- Deliveries blocked by credit or billing blocks may go unreleased or unreviewed when not monitored by organization and customer
- Inability to filter by sales org, date range, or status limits effective prioritization and segregation of duties
- Missing visibility into block age and reference date hinders root-cause analysis and release coordination

**Management Visibility and Decision-Making Risks**
- Absence of blocked-delivery monitoring delays management awareness of credit and billing blocks that affect revenue
- Lack of organizational and duration-based analysis limits the ability to allocate credit and collections resources
- Inadequate visibility hinders release workflows and corrective action when revenue or customer issues are reported

## Suggested Resolution

**Immediate Response**
- Review the blocked deliveries flagged by the EI to confirm the scope and nature of the blocks (credit, billing, delivery status)
- Verify high-value or aged blocked deliveries using the relevant transaction (e.g. VL03N for delivery, VKM3 for credit) to confirm release or escalation
- Check reference date and duration to assess how long the block has existed and prioritize by impact
- Identify business context: credit hold, billing block, or process exception requiring release or master data change

**System Assessment**
- Analyze the time window and reference date field used for the run to ensure the monitoring scope matches the control objective
- Compare blocked delivery counts and value to prior periods to detect deterioration or improvement after release actions
- Review sales org, distribution channel, division, and customer filters to focus on the right segments
- Validate that duration and reference date settings align with the intended use (e.g. focus on aged blocks)

**Corrective Actions**
- If blocks are legitimate, coordinate with credit and billing to release or correct master data and document the action
- Escalate repeated or systemic blocks to credit management and sales for process and authorization review
- Update monitoring parameters (e.g. lookback, reference date field, duration filter, organizational scope) to align with release and governance requirements
- Document release outcomes and establish recurring EI runs to maintain visibility into blocked deliveries


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | ARKTX | Description | CHAR | 40 | 0 | ARKTX | TEXT40 |
| 3 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 4 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 5 | BLOCK | Indicator: Document preselected for archiving | CHAR | 1 | 0 | BLOCK_VB | BLOCK_VB |
| 6 | BLOCK_DESC | Billing block desc. | CHAR | 20 | 0 | BEZEI_FAKSP | TEXT20 |
| 7 | BP1_CODE | Partner1 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 8 | BP1_FUNCT | Partner1 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 9 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 10 | BP2_CODE | Partner2 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 11 | BP2_FUNCT | Partner2 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 12 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 13 | BP3_CODE | Partner3 - Code | CHAR | 10 | 0 | KUNNR | KUNNR |
| 14 | BP3_FUNCT | Partner3 - Function | CHAR | 2 | 0 | PARVW | PARVW |
| 15 | BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 16 | BZIRK | Sales district | CHAR | 6 | 0 | BZIRK | BZIRK |
| 17 | CMGST | Overall CreditStatus | CHAR | 1 | 0 | CMGST | CMGST |
| 18 | COSTA | Confirmation status | CHAR | 1 | 0 | COSTA_D | COSTA |
| 19 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 20 | DUMMY | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 21 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 22 | DURATION_D | Duration In Days | NUMC | 6 | 0 | /SKN/E_SW_DURATION_D |  |
| 23 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 24 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 25 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 26 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 27 | FAKSK | Billing block | CHAR | 2 | 0 | FAKSK | FAKSP |
| 28 | FKDAT | Billing Date | DATS | 8 | 0 | FKDAT | DATUM |
| 29 | FKIVK | Totals status | CHAR | 1 | 0 | FKIVK | STATV |
| 30 | FKIVP | Interco. Bill.Status | CHAR | 1 | 0 | FKIVP | STATV |
| 31 | FKSTA | Billing Status | CHAR | 1 | 0 | FKSTA | STATV |
| 32 | FKSTK | Billing status | CHAR | 1 | 0 | FKSTK | STATV |
| 33 | HDALL | On Hold | CHAR | 1 | 0 | /SPE/INB_HDALL | XFELD |
| 34 | HDALS | Pos. Hold | CHAR | 1 | 0 | /SPE/INB_HDALS | XFELD |
| 35 | KDAUF | Sales Order | CHAR | 10 | 0 | KDAUF | VBELN |
| 36 | KDGRP | Customer group | CHAR | 2 | 0 | KDGRP | KDGRP |
| 37 | KDPOS | Sales order item | NUMC | 6 | 0 | KDPOS | NUM06 |
| 38 | KLMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KLMENG | MENG15 |
| 39 | KODAT | Picking Date | DATS | 8 | 0 | KODAT | DATUM |
| 40 | KOQUA | Pick confirmation | CHAR | 1 | 0 | KOQUA | STATV |
| 41 | KOQUK | Pick confirmation | CHAR | 1 | 0 | KOQUK | STATV |
| 42 | KOSTA | Picking status | CHAR | 1 | 0 | KOSTA | STATV |
| 43 | KOSTK | Overall pick.status | CHAR | 1 | 0 | KOSTK | STATV |
| 44 | KUNAG | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 45 | KUNNR | Ship-to party | CHAR | 10 | 0 | KUNWE | KUNNR |
| 46 | KWMENG | Order Quantity | QUAN | 15 | 3 | KWMENG | MENG15 |
| 47 | LANG | Language for texts |  | 0 | 0 |  |  |
| 48 | LDDAT | Loading Date | DATS | 8 | 0 | LDDAT | DATUM |
| 49 | LFART | Delivery Type | CHAR | 4 | 0 | LFART | LFART |
| 50 | LFBNR | Reference Document | CHAR | 10 | 0 | LFBNR | BELNR |
| 51 | LFDAT | Delivery Date | DATS | 8 | 0 | LFDAT_V | DATUM |
| 52 | LFGSA | Overall deliv.status | CHAR | 1 | 0 | LFGSA | STATV |
| 53 | LFIMG | Delivery quantity | QUAN | 13 | 3 | LFIMG | MENG13 |
| 54 | LFPOS | Reference Doc. Item | NUMC | 4 | 0 | LFPOS | MBLPO |
| 55 | LFSTA | Delivery status | CHAR | 1 | 0 | LFSTA | STATV |
| 56 | LGMNG | Actual delivery qty | QUAN | 13 | 3 | LGMNG | MENG13 |
| 57 | LVSTA | WM activity status | CHAR | 1 | 0 | LVSTA | STATV |
| 58 | LVSTK | Overall WM status | CHAR | 1 | 0 | LVSTK | STATV |
| 59 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 60 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 61 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 62 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 63 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 64 | MPROK_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 65 | NETWR_LIPS | Net Value | CURR | 15 | 2 | NETWR | WERTV8 |
| 66 | NETWR_VBAP | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 67 | PDSTA | Proof of delivery status | CHAR | 1 | 0 | PDSTA | STATV |
| 68 | PDSTK | Proof of delivery status | CHAR | 1 | 0 | PDSTK | STATV |
| 69 | PKSTA | Packing status | CHAR | 1 | 0 | PKSTA | STATV |
| 70 | PKSTK | Packing status | CHAR | 1 | 0 | PKSTK | STATV_PKST |
| 71 | PODAT | Proof of delivery date | DATS | 8 | 0 | PODAT | DATUM |
| 72 | POSNR | Item | NUMC | 6 | 0 | POSNR_VL | POSNR |
| 73 | PSTYV | Item category | CHAR | 4 | 0 | PSTYV_VL | PSTYV |
| 74 | ROUTE | Route | CHAR | 6 | 0 | ROUTE | ROUTE |
| 75 | SHIPTO_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 76 | SOLDTO_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 77 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 78 | SPE_TMPID | Temp Inb. | CHAR | 1 | 0 | /SPE/TMPID | /SPE/TMPID |
| 79 | TDDAT | Transptn Plang Date | DATS | 8 | 0 | TDDAT_D | DATUM |
| 80 | TRSTA | Trns.plan.status | CHAR | 1 | 0 | TRSTA | TRSTA |
| 81 | UVK01 | Header reserves 1 | CHAR | 1 | 0 | UVK01 | STATV |
| 82 | UVK02 | Header reserves 2 | CHAR | 1 | 0 | UVK02 | STATV |
| 83 | UVK03 | Header reserves 3 | CHAR | 1 | 0 | UVK03 | STATV |
| 84 | UVK04 | Header reserves 4 | CHAR | 1 | 0 | UVK04 | STATV |
| 85 | UVK05 | Header reserves 5 | CHAR | 1 | 0 | UVK05 | STATV |
| 86 | UVP01 | Item reserves 1 | CHAR | 1 | 0 | UVP01 | STATV |
| 87 | UVP02 | Item reserves 2 | CHAR | 1 | 0 | UVP02 | STATV |
| 88 | UVP03 | Item reserves 3 | CHAR | 1 | 0 | UVP03 | STATV |
| 89 | UVP04 | Item reserves 4 | CHAR | 1 | 0 | UVP04 | STATV |
| 90 | UVP05 | Item reserves 5 | CHAR | 1 | 0 | UVP05 | STATV |
| 91 | UVPAS | It.data packaging | CHAR | 1 | 0 | UVPAK_SU | STATV |
| 92 | UVPIS | It.data picking/putaway | CHAR | 1 | 0 | UVPIK_SU | STATV |
| 93 | UVS01 | Total reserves 1 | CHAR | 1 | 0 | UVS01 | STATV |
| 94 | UVS02 | Total reserves 2 | CHAR | 1 | 0 | UVS02 | STATV |
| 95 | UVS03 | Total reserves 3 | CHAR | 1 | 0 | UVS03 | STATV |
| 96 | UVS04 | Total reserves 4 | CHAR | 1 | 0 | UVS04 | STATV |
| 97 | UVS05 | Total reserves 5 | CHAR | 1 | 0 | UVS05 | STATV |
| 98 | VBELN | Delivery | CHAR | 10 | 0 | VBELN_VL | VBELN |
| 99 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 100 | VESTK | HU placed in stock | CHAR | 1 | 0 | VESTK | VESTK |
| 101 | VGBEL | Reference Document | CHAR | 10 | 0 | VGBEL | VBELN |
| 102 | VGPOS | Reference Item | NUMC | 6 | 0 | VGPOS | POSNR |
| 103 | VGTYP | SD document categ. (prev) | CHAR | 1 | 0 | VBTYP | VBTYP |
| 104 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 105 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 106 | VLSTK | Status Decent. Whse | CHAR | 1 | 0 | VLSTK | VLSTK |
| 107 | VRKME | Sales Unit | UNIT | 3 | 0 | VRKME | MEINS |
| 108 | VSTEL | Shipping Point/Receiving Pt | CHAR | 4 | 0 | VSTEL | VSTEL |
| 109 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 110 | WADAT | Pland Gds Mvmnt Date | DATS | 8 | 0 | WADAK | DATUM |
| 111 | WADAT_IST | Act. Gds Mvmnt Date | DATS | 8 | 0 | WADAT_IST | DATUM |
| 112 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 113 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |
| 114 | WBSTA | Goods movement stat. | CHAR | 1 | 0 | WBSTA | STATV |
| 115 | WBSTK | Total gds mvt stat. | CHAR | 1 | 0 | WBSTK | STATV |
| 116 | WGBEZ | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |
| 117 | XBLNR | Reference | CHAR | 25 | 0 | XBLNR_LIKP | XBLNR_LIKP |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 117 parameters listed in the Parameters Reference Table above.

**AEDAT** (Changed On):

Change date of the sales document. Restricts which deliveries are included by change date; also appears in the result.

**ARKTX** (Description):

Material description. Populated from master data; used for display in the result.

**AUART** (Sales Document Type):

Sales document type. Restricts which deliveries (via reference sales order) are included; also appears in the result.

**BACKDAYS** (Backdays):

Number of days to look back from the current date when building the default monitoring window. When no explicit date range is supplied, the EI uses today minus this value as the start of the window and applies it to the date field selected by DATE_REF_FLD.

**BLOCK** (Indicator: Document preselected for archiving):

Indicator that the document is preselected for archiving. Restricts which deliveries are included; used for filtering and display. Domain BLOCK_VB.

**BLOCK Options:**
- **X**: Set (preselected for archiving).
- ** ** (space or initial): Not set.

**BLOCK_DESC** (Billing block desc.):

Short description of the billing block. Populated from the billing block master; used for display.

**BP1_CODE** (Partner1 - Code):

Partner number for the first partner role. Restricts which deliveries are included by partner; used with BP1_FUNCT and BP1_NAME.

**BP1_FUNCT** (Partner1 - Function):

Partner function for the first partner (e.g. sold-to, ship-to, bill-to). Restricts which deliveries are included by partner role; also appears in the result.

**BP1_NAME** (Name):

Name for the first partner. Populated from customer master; used for display with BP1_CODE.

**BP2_CODE** (Partner2 - Code):

Partner number for the second partner role. Restricts which deliveries are included by partner; used with BP2_FUNCT and BP2_NAME.

**BP2_FUNCT** (Partner2 - Function):

Partner function for the second partner. Restricts which deliveries are included by partner role; also appears in the result.

**BP2_NAME** (Name):

Name for the second partner. Populated from customer master; used for display with BP2_CODE.

**BP3_CODE** (Partner3 - Code):

Partner number for the third partner role. Restricts which deliveries are included by partner; used with BP3_FUNCT and BP3_NAME.

**BP3_FUNCT** (Partner3 - Function):

Partner function for the third partner. Restricts which deliveries are included by partner role; also appears in the result.

**BP3_NAME** (Name):

Name for the third partner. Populated from customer master; used for display with BP3_CODE.

**BZIRK** (Sales district):

Sales district. Restricts which deliveries are included; also appears in the result.

**CMGST** (Overall CreditStatus):

Overall credit status. Restricts which deliveries are included by credit status; used to focus on credit-blocked deliveries. Also appears in the result.

**CMGST Options:**
- Possible values are domain-specific (credit status codes); see domain CMGST.

**COSTA** (Confirmation status):

Confirmation status. Restricts which deliveries are included; also appears in the result. Domain COSTA.

**DATE_REF_FLD** (Date Ref Field):

Name of the date field used as the reference for the default date range and for duration calculation. Determines which date (e.g. planned goods movement date, delivery date, creation date) is used when no explicit range is supplied and for computing status duration.

**DATE_REF_FLD Options:**
- **WADAT**: Planned goods movement date (default in code).
- **WADAT_IST**: Actual goods movement date.
- **LFDAT**: Delivery date.
- **ERDAT**: Created on.
- **AEDAT**: Changed on.
- **LDDAT**: Loading date.
- **TDDAT**: Transportation planning date.
- **KODAT**: Picking date.
- **FKDAT**: Billing date.

**DUMMY** (Single-Character Flag):

Single-character flag. Used for technical or display purposes; values are domain-specific (CHAR1).

**DURATION** (Duration In Time Units):

Duration in the unit given by DURATION_UNIT (e.g. days) between the reference date (from DATE_REF_FLD) and the run date. Restricts which deliveries are included when a duration filter is applied; also appears in the result.

**DURATION_D** (Duration In Days):

Duration in days. Populated from the duration calculation when DURATION_UNIT is days; used for display and filtering.

**DURATION_UNIT** (Duration Unit):

Unit for DURATION (e.g. days, hours). Used with DURATION and DATE_REF_FLD when computing and filtering by how long the delivery has been in a given status.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours.
- **M**: Minutes.

**ERDAT** (Created On):

Creation date of the delivery. Restricts which deliveries are included by creation date; when no explicit range is supplied, the default range can be applied to this field when DATE_REF_FLD = ERDAT. Also appears in the result.

**ERNAM** (Created By):

User who created the delivery. Restricts which deliveries are included; also appears in the result.

**ERZET** (Time):

Creation time. Appears in the result; used for ordering and display.

**FAKSK** (Billing block):

Billing block code. Restricts which deliveries are included by billing block; used to focus on billing-blocked deliveries. Also appears in the result.

**FKDAT** (Billing Date):

Billing date. Restricts which deliveries are included by billing date; also appears in the result when DATE_REF_FLD = FKDAT.

**FKIVK** (Totals status):

Totals status at header level. Restricts which deliveries are included; also appears in the result. Domain STATV.

**FKIVP** (Interco. Bill.Status):

Intercompany billing status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**FKSTA** (Billing Status):

Billing status at item level. Restricts which deliveries are included; also appears in the result. Domain STATV.

**FKSTK** (Billing status):

Overall billing status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**HDALL** (On Hold):

Indicator that the delivery is on hold. Restricts which deliveries are included; also appears in the result. Domain XFELD.

**HDALL Options:**
- **X**: Set (on hold).
- ** ** (space or initial): Not set.

**HDALS** (Pos. Hold):

Position hold indicator. Restricts which deliveries are included; also appears in the result. Domain XFELD.

**HDALS Options:**
- **X**: Set.
- ** ** (space or initial): Not set.

**KDAUF** (Sales Order):

Sales order number. Restricts which deliveries are included (via reference); also appears in the result.

**KDGRP** (Customer group):

Customer group. Restricts which deliveries are included; also appears in the result.

**KDPOS** (Sales order item):

Sales order item number. Appears in the result; used for reference to the sales order.

**KLMENG** (Cumul.confirmed qty):

Cumulative confirmed quantity. Appears in the result; used for quantity-based filtering or display.

**KODAT** (Picking Date):

Picking date. Restricts which deliveries are included by picking date; also appears in the result when DATE_REF_FLD = KODAT.

**KOQUA** (Pick confirmation):

Pick confirmation status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**KOQUK** (Pick confirmation):

Pick confirmation status (alternative). Restricts which deliveries are included; also appears in the result. Domain STATV.

**KOSTA** (Picking status):

Picking status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**KOSTK** (Overall pick.status):

Overall picking status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**KUNAG** (Sold-to party):

Sold-to party (customer). Restricts which deliveries are included; also appears in the result.

**KUNNR** (Ship-to party):

Ship-to party (customer). Restricts which deliveries are included; also appears in the result.

**KWMENG** (Order Quantity):

Order quantity. Appears in the result; used for quantity-based display or filtering.

**LANG** (Language for texts):

Language used when retrieving description texts. Determines the language of BLOCK_DESC, MPROK_DESC, WGBEZ, and other description fields.

**LDDAT** (Loading Date):

Loading date. Restricts which deliveries are included by loading date; also appears in the result when DATE_REF_FLD = LDDAT.

**LFART** (Delivery Type):

Delivery type. Restricts which deliveries are included; also appears in the result.

**LFBNR** (Reference Document):

Reference document number. Appears in the result; used for reference and display.

**LFDAT** (Delivery Date):

Delivery date. Restricts which deliveries are included by delivery date; when DATE_REF_FLD = LFDAT, the default range is applied to this field. Also appears in the result.

**LFGSA** (Overall deliv.status):

Overall delivery status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**LFIMG** (Delivery quantity):

Delivery quantity. Appears in the result; used for quantity-based display or filtering.

**LFPOS** (Reference Doc. Item):

Reference document item. Appears in the result; used for reference and display.

**LFSTA** (Delivery status):

Delivery status at item level. Restricts which deliveries are included; also appears in the result. Domain STATV.

**LGMNG** (Actual delivery qty):

Actual delivery quantity in stockkeeping units. Appears in the result; used for quantity-based display or filtering.

**LVSTA** (WM activity status):

Warehouse management activity status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**LVSTK** (Overall WM status):

Overall warehouse management status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set, date and time used for the monitoring window and comparisons are interpreted in UTC. When not set, system local date/time is used.

**MANAGE_IN_UTC Options:**
- **X**: Use UTC for date/time.
- ** ** (space or initial): Use system local date/time.

**MATKL** (Material Group):

Material group. Restricts which deliveries are included by material group; also appears in the result.

**MATNR** (Material):

Material number. Restricts which deliveries are included; also appears in the result.

**MEINS** (Base Unit of Measure):

Base unit of measure. Appears in the result; used for quantity display.

**MPROK** (Manual price):

Manual price indicator. Restricts which deliveries are included; also appears in the result. Domain MPROK.

**MPROK Options:**
- Possible values are domain-specific; see domain MPROK.

**MPROK_DESC** (Short text):

Short text for manual price. Populated from domain or master data; used for display.

**NETWR_LIPS** (Net Value):

Net value in the delivery item. Represents the net value in document currency at item level; used for display and threshold filtering.

**NETWR_VBAP** (Net value):

Net value from the sales order item. Represents the net value in document currency from the reference sales order; used for display and threshold filtering.

**PDSTA** (Proof of delivery status):

Proof of delivery status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**PDSTK** (Proof of delivery status):

Overall proof of delivery status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**PKSTA** (Packing status):

Packing status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**PKSTK** (Packing status):

Overall packing status. Restricts which deliveries are included; also appears in the result. Domain STATV_PKST.

**PODAT** (Proof of delivery date):

Proof of delivery date. Restricts which deliveries are included; also appears in the result.

**POSNR** (Item):

Delivery item number. Appears in the result; used for item-level identification.

**PSTYV** (Item category):

Item category. Restricts which deliveries are included; also appears in the result.

**ROUTE** (Route):

Route. Restricts which deliveries are included; also appears in the result.

**SHIPTO_DESC** (Name):

Ship-to party name. Populated from customer master; used for display.

**SOLDTO_DESC** (Name):

Sold-to party name. Populated from customer master; used for display.

**SPART** (Division):

Division. Restricts which deliveries are included; also appears in the result.

**SPE_TMPID** (Temp Inb.):

Temporary inbound indicator. Restricts which deliveries are included; also appears in the result. Domain /SPE/TMPID.

**TDDAT** (Transptn Plang Date):

Transportation planning date. Restricts which deliveries are included; also appears in the result when DATE_REF_FLD = TDDAT.

**TRSTA** (Trns.plan.status):

Transportation planning status. Restricts which deliveries are included; also appears in the result. Domain TRSTA.

**UVK01** (Header reserves 1):

Header reserve status 1. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVK02** (Header reserves 2):

Header reserve status 2. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVK03** (Header reserves 3):

Header reserve status 3. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVK04** (Header reserves 4):

Header reserve status 4. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVK05** (Header reserves 5):

Header reserve status 5. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVP01** (Item reserves 1):

Item reserve status 1. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVP02** (Item reserves 2):

Item reserve status 2. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVP03** (Item reserves 3):

Item reserve status 3. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVP04** (Item reserves 4):

Item reserve status 4. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVP05** (Item reserves 5):

Item reserve status 5. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVPAS** (It.data packaging):

Item data packaging status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVPIS** (It.data picking/putaway):

Item data picking/putaway status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVS01** (Total reserves 1):

Total reserve status 1. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVS02** (Total reserves 2):

Total reserve status 2. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVS03** (Total reserves 3):

Total reserve status 3. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVS04** (Total reserves 4):

Total reserve status 4. Restricts which deliveries are included; also appears in the result. Domain STATV.

**UVS05** (Total reserves 5):

Total reserve status 5. Restricts which deliveries are included; also appears in the result. Domain STATV.

**VBELN** (Delivery):

Delivery document number. Restricts which deliveries are included; also appears in the result.

**VBTYP** (SD document categ.):

SD document category. Restricts which deliveries are included; also appears in the result. Domain VBTYP.

**VESTK** (HU placed in stock):

Handling unit placed in stock indicator. Restricts which deliveries are included; also appears in the result. Domain VESTK.

**VGBEL** (Reference Document):

Reference document (e.g. sales order). Appears in the result; used for reference and display.

**VGPOS** (Reference Item):

Reference item number. Appears in the result; used for reference and display.

**VGTYP** (SD document categ. (prev)):

SD document category of the reference document. Appears in the result; used for display.

**VKBUR** (Sales Office):

Sales office. Restricts which deliveries are included; also appears in the result.

**VKORG** (Sales Organization):

Sales organization. Restricts which deliveries are included; also appears in the result. Used to focus on a given sales org (e.g. 1200 for this EI).

**VLSTK** (Status Decent. Whse):

Status decent warehouse. Restricts which deliveries are included; also appears in the result. Domain VLSTK.

**VRKME** (Sales Unit):

Sales unit of measure. Appears in the result; used for quantity display.

**VSTEL** (Shipping Point/Receiving Pt):

Shipping point. Restricts which deliveries are included; also appears in the result.

**VTWEG** (Distribution Channel):

Distribution channel. Restricts which deliveries are included; also appears in the result.

**WADAT** (Pland Gds Mvmnt Date):

Planned goods movement date. Restricts which deliveries are included; when DATE_REF_FLD = WADAT, the default range is applied to this field. Also appears in the result and is used for duration calculation.

**WADAT_IST** (Act. Gds Mvmnt Date):

Actual goods movement date. Restricts which deliveries are included; when DATE_REF_FLD = WADAT_IST, the default range is applied to this field. Also appears in the result.

**WAERK** (Document Currency):

Document currency. Represents the currency of the delivery/sales document; used for value display and comparison.

**WAVWR** (Cost):

Cost in document currency. Represents the cost value; used for display and threshold filtering.

**WBSTA** (Goods movement stat.):

Goods movement status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**WBSTK** (Total gds mvt stat.):

Total goods movement status. Restricts which deliveries are included; also appears in the result. Domain STATV.

**WGBEZ** (Material Group Desc.):

Material group description. Populated from master data; used for display.

**XBLNR** (Reference):

Reference (e.g. customer reference). Appears in the result; used for display.


### Parameter Relationships

**Time and reference date parameters**

- **BACKDAYS** defines the default number of days to look back when no explicit date range is supplied. It is used together with the reference date field: when no range is supplied, the EI builds a range from today minus BACKDAYS and applies it to the field selected by **DATE_REF_FLD**.
- **DATE_REF_FLD** determines which date field (e.g. planned goods movement date, delivery date, creation date) is used for the default date range and for duration calculation. **BACKDAYS** and DATE_REF_FLD work together: the selection window is built from today minus BACKDAYS and applied to the chosen reference date field.
- **DURATION** and **DURATION_UNIT** work together. DURATION_UNIT specifies the unit (e.g. days, hours) used when computing the duration between the reference date (from DATE_REF_FLD) and the run date. The computed duration is compared to the DURATION filter; only records that fall within the DURATION range are returned.

**Partner and organizational scope**

- **BP1_FUNCT**, **BP1_CODE**, **BP2_FUNCT**, **BP2_CODE**, **BP3_FUNCT**, **BP3_CODE** together define partner roles (e.g. sold-to, ship-to, bill-to) and optional partner codes for filtering and display. Partner names (BP1_NAME, BP2_NAME, BP3_NAME) are populated from customer master.
- **VKORG**, **VTWEG**, **SPART**, **BZIRK**, **KDGRP**, **VSTEL**, **ROUTE** together define the organizational scope for deliveries: sales organization, distribution channel, division, sales district, customer group, shipping point, and route. Used in the selection and appear in the result.

**Delivery and status parameters**

- **VBELN**, **LFART**, **VBTYP**, **PSTYV** define delivery number, delivery type, document category, and item category. **FAKSK** (billing block), **WBSTK** / **WBSTA** (goods movement status), **LFSTA** / **LFGSA** (delivery status), **FKSTA** / **FKSTK** (billing status), and related status parameters restrict which deliveries are included and appear in the result. **CMGST** (overall credit status) and **BLOCK** are used to focus on credit and block-related exceptions.


### Default Values

- **BACKDAYS** — Default: `1` in the code when not supplied; when no date range is supplied, the EI uses today minus BACKDAYS as the start of the window for the reference date field.
- **DATE_REF_FLD** — Default: `WADAT` (planned goods movement date) in the code when not supplied; determines which date field is used for the default range and duration calculation.
- **DURATION_UNIT** — Default: `D` (days) in the code when not supplied; used for computing and filtering by duration.
- **LANG** — Default: system language (e.g. from SY-LANGU) when not supplied; used for description texts.
- **MANAGE_IN_UTC** — Default: initial (empty); date and time are interpreted in system local time when not set. When set to 'X', UTC is used for the monitoring window and comparisons.

**Note:** When no explicit date range is supplied, the default range is applied to the date field selected by DATE_REF_FLD (e.g. WADAT, LFDAT, ERDAT).

### Practical Configuration Examples

**Use Case 1: Blocked deliveries in the last 7 days (sales org 1200)**

```
BACKDAYS = 7
VKORG = 1200
DATE_REF_FLD = WADAT
FAKSK = (optional; leave empty for all blocks)
```

**Purpose:** Find deliveries blocked due to credit or billing in sales org 1200 in the last 7 days, using planned goods movement date, for daily release review.

**Use Case 2: Duration filter (blocked more than 5 days)**

```
BACKDAYS = 30
DATE_REF_FLD = WADAT
DURATION_UNIT = D
DURATION = 5 to 999999
VKORG = 1200
```

**Purpose:** Focus on deliveries that have been blocked for more than 5 days (based on WADAT), to prioritize aged blocks in sales org 1200.

**Use Case 3: Reference date = delivery date**

```
BACKDAYS = 14
DATE_REF_FLD = LFDAT
VTWEG = 10
SPART = 00
```

**Purpose:** Monitor blocked deliveries by delivery date (LFDAT) over the last 14 days, for a specific distribution channel and division.

**Use Case 4: Partner and organizational scope**

```
BACKDAYS = 7
BP1_FUNCT = AG (sold-to)
BP2_FUNCT = WE (ship-to)
VKORG = 1200
BZIRK = (optional)
```

**Purpose:** Review blocked deliveries by sold-to and ship-to partner roles in sales org 1200 for the last week, for customer and segment analysis.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_DLV_STAT | .INCLU--AP |  |  |  |
| /SKN/S_SW_10_01_DLV_STAT | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_DLV_STAT | ARKTX | Short text for sales order item | CHAR(40) | ARKTX |
| /SKN/S_SW_10_01_DLV_STAT | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_DLV_STAT | BLOCK | Indicator: Document preselected for archiving | CHAR(1) | BLOCK_VB |
| /SKN/S_SW_10_01_DLV_STAT | BLOCK_DESC | Description | CHAR(20) | BEZEI_FAKSP |
| /SKN/S_SW_10_01_DLV_STAT | BP1_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_DLV_STAT | BP1_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_DLV_STAT | BP1_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_DLV_STAT | BP2_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_DLV_STAT | BP2_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_DLV_STAT | BP2_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_DLV_STAT | BP3_CODE | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_DLV_STAT | BP3_FUNCT | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_DLV_STAT | BP3_NAME | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_DLV_STAT | BZIRK | Sales district | CHAR(6) | BZIRK |
| /SKN/S_SW_10_01_DLV_STAT | CMGST | Overall status of credit checks | CHAR(1) | CMGST |
| /SKN/S_SW_10_01_DLV_STAT | COSTA | Confirmation status for ALE | CHAR(1) | COSTA_D |
| /SKN/S_SW_10_01_DLV_STAT | DUMMY | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_01_DLV_STAT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_DLV_STAT | DURATION_D | SW: Duration In Days | NUMC(6) | /SKN/E_SW_DURATION_D |
| /SKN/S_SW_10_01_DLV_STAT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_DLV_STAT | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_DLV_STAT | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_DLV_STAT | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_DLV_STAT | FAKSK | Billing block in SD document | CHAR(2) | FAKSK |
| /SKN/S_SW_10_01_DLV_STAT | FKDAT | Billing date for billing index and printout | DATS(8) | FKDAT |
| /SKN/S_SW_10_01_DLV_STAT | FKIVK | Billing totals status for intercompany billing | CHAR(1) | FKIVK |
| /SKN/S_SW_10_01_DLV_STAT | FKIVP | Intercompany Billing Status | CHAR(1) | FKIVP |
| /SKN/S_SW_10_01_DLV_STAT | FKSTA | Billing status of delivery-related billing documents | CHAR(1) | FKSTA |
| /SKN/S_SW_10_01_DLV_STAT | FKSTK | Billing status | CHAR(1) | FKSTK |
| /SKN/S_SW_10_01_DLV_STAT | HDALL | Inbound delivery header not yet complete (on Hold) | CHAR(1) | /SPE/INB_HDALL |
| /SKN/S_SW_10_01_DLV_STAT | HDALS | At least one of ID items not yet complete (on Hold) | CHAR(1) | /SPE/INB_HDALS |
| /SKN/S_SW_10_01_DLV_STAT | KDAUF | Sales Order Number | CHAR(10) | KDAUF |
| /SKN/S_SW_10_01_DLV_STAT | KDGRP | Customer group | CHAR(2) | KDGRP |
| /SKN/S_SW_10_01_DLV_STAT | KDPOS | Item Number in Sales Order | NUMC(6) | KDPOS |
| /SKN/S_SW_10_01_DLV_STAT | KLMENG | Cumulative confirmed quantity in base unit | QUAN(15,3) | KLMENG |
| /SKN/S_SW_10_01_DLV_STAT | KODAT | Picking Date | DATS(8) | KODAT |
| /SKN/S_SW_10_01_DLV_STAT | KOQUA | Confirmation status of picking/putaway | CHAR(1) | KOQUA |
| /SKN/S_SW_10_01_DLV_STAT | KOQUK | Status of pick confirmation | CHAR(1) | KOQUK |
| /SKN/S_SW_10_01_DLV_STAT | KOSTA | Picking status/Putaway status | CHAR(1) | KOSTA |
| /SKN/S_SW_10_01_DLV_STAT | KOSTK | Overall picking / putaway status | CHAR(1) | KOSTK |
| /SKN/S_SW_10_01_DLV_STAT | KUNAG | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_DLV_STAT | KUNNR | Ship-to party | CHAR(10) | KUNWE |
| /SKN/S_SW_10_01_DLV_STAT | KWMENG | Cumulative Order Quantity in Sales Units | QUAN(15,3) | KWMENG |
| /SKN/S_SW_10_01_DLV_STAT | LDDAT | Loading Date | DATS(8) | LDDAT |
| /SKN/S_SW_10_01_DLV_STAT | LFART | Delivery Type | CHAR(4) | LFART |
| /SKN/S_SW_10_01_DLV_STAT | LFBNR | Document No. of a Reference Document | CHAR(10) | LFBNR |
| /SKN/S_SW_10_01_DLV_STAT | LFDAT | Delivery Date | DATS(8) | LFDAT_V |
| /SKN/S_SW_10_01_DLV_STAT | LFGSA | Overall delivery status of the item | CHAR(1) | LFGSA |
| /SKN/S_SW_10_01_DLV_STAT | LFIMG | Actual quantity delivered (in sales units) | QUAN(13,3) | LFIMG |
| /SKN/S_SW_10_01_DLV_STAT | LFPOS | Item of a Reference Document | NUMC(4) | LFPOS |
| /SKN/S_SW_10_01_DLV_STAT | LFSTA | Delivery status | CHAR(1) | LFSTA |
| /SKN/S_SW_10_01_DLV_STAT | LGMNG | Actual quantity delivered in stockkeeping units | QUAN(13,3) | LGMNG |
| /SKN/S_SW_10_01_DLV_STAT | LVSTA | Status of warehouse management activities | CHAR(1) | LVSTA |
| /SKN/S_SW_10_01_DLV_STAT | LVSTK | Overall status of warehouse management activities | CHAR(1) | LVSTK |
| /SKN/S_SW_10_01_DLV_STAT | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_01_DLV_STAT | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_DLV_STAT | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_01_DLV_STAT | MPROK | Status manual price change | CHAR(1) | MPROK |
| /SKN/S_SW_10_01_DLV_STAT | MPROK_DESC | Explanatory short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_10_01_DLV_STAT | NETWR_LIPS | Net Value in Document Currency | CURR(15,2) | NETWR |
| /SKN/S_SW_10_01_DLV_STAT | NETWR_VBAP | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_DLV_STAT | PDSTA | POD status on item level | CHAR(1) | PDSTA |
| /SKN/S_SW_10_01_DLV_STAT | PDSTK | POD status on header level | CHAR(1) | PDSTK |
| /SKN/S_SW_10_01_DLV_STAT | PKSTA | Packing status of item | CHAR(1) | PKSTA |
| /SKN/S_SW_10_01_DLV_STAT | PKSTK | Overall packing status of all items | CHAR(1) | PKSTK |
| /SKN/S_SW_10_01_DLV_STAT | PODAT | Date (proof of delivery) | DATS(8) | PODAT |
| /SKN/S_SW_10_01_DLV_STAT | POSNR | Delivery Item | NUMC(6) | POSNR_VL |
| /SKN/S_SW_10_01_DLV_STAT | PSTYV | Delivery item category | CHAR(4) | PSTYV_VL |
| /SKN/S_SW_10_01_DLV_STAT | ROUTE | Route | CHAR(6) | ROUTE |
| /SKN/S_SW_10_01_DLV_STAT | SHIPTO_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_DLV_STAT | SOLDTO_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_DLV_STAT | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_DLV_STAT | SPE_TMPID | Temporary inbound delivery | CHAR(1) | /SPE/TMPID |
| /SKN/S_SW_10_01_DLV_STAT | TDDAT | Transportation Planning Date | DATS(8) | TDDAT_D |
| /SKN/S_SW_10_01_DLV_STAT | TRSTA | Transportation planning status | CHAR(1) | TRSTA |
| /SKN/S_SW_10_01_DLV_STAT | UVK01 | Customer reserves 1: Header status | CHAR(1) | UVK01 |
| /SKN/S_SW_10_01_DLV_STAT | UVK02 | Customer reserves 2: Header status | CHAR(1) | UVK02 |
| /SKN/S_SW_10_01_DLV_STAT | UVK03 | Customer reserves 3: Header status | CHAR(1) | UVK03 |
| /SKN/S_SW_10_01_DLV_STAT | UVK04 | Custmer reserves 4: Header status | CHAR(1) | UVK04 |
| /SKN/S_SW_10_01_DLV_STAT | UVK05 | Customer reserves 5: Header status | CHAR(1) | UVK05 |
| /SKN/S_SW_10_01_DLV_STAT | UVP01 | Customer reserves 1: Item status | CHAR(1) | UVP01 |
| /SKN/S_SW_10_01_DLV_STAT | UVP02 | Customer reserves 2: Item status | CHAR(1) | UVP02 |
| /SKN/S_SW_10_01_DLV_STAT | UVP03 | Item reserves 3: Item status | CHAR(1) | UVP03 |
| /SKN/S_SW_10_01_DLV_STAT | UVP04 | Item reserves 4: Item status | CHAR(1) | UVP04 |
| /SKN/S_SW_10_01_DLV_STAT | UVP05 | Customer reserves 5: Item status | CHAR(1) | UVP05 |
| /SKN/S_SW_10_01_DLV_STAT | UVPAS | Totals incomplete status for all items: packaging | CHAR(1) | UVPAK_SU |
| /SKN/S_SW_10_01_DLV_STAT | UVPIS | Totals incomplete status for all items: Picking | CHAR(1) | UVPIK_SU |
| /SKN/S_SW_10_01_DLV_STAT | UVS01 | Customer reserves 1: Sum of all items | CHAR(1) | UVS01 |
| /SKN/S_SW_10_01_DLV_STAT | UVS02 | Customer reserves 2: Sum of all items | CHAR(1) | UVS02 |
| /SKN/S_SW_10_01_DLV_STAT | UVS03 | Customer reserves 3: Sum of all items | CHAR(1) | UVS03 |
| /SKN/S_SW_10_01_DLV_STAT | UVS04 | Customer reserves 4: Sum of all items | CHAR(1) | UVS04 |
| /SKN/S_SW_10_01_DLV_STAT | UVS05 | Customer reserves 5: Sum of all items | CHAR(1) | UVS05 |
| /SKN/S_SW_10_01_DLV_STAT | VBELN | Delivery | CHAR(10) | VBELN_VL |
| /SKN/S_SW_10_01_DLV_STAT | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_DLV_STAT | VESTK | Handling Unit Placed in Stock | CHAR(1) | VESTK |
| /SKN/S_SW_10_01_DLV_STAT | VGBEL | Document number of the reference document | CHAR(10) | VGBEL |
| /SKN/S_SW_10_01_DLV_STAT | VGPOS | Item number of the reference item | NUMC(6) | VGPOS |
| /SKN/S_SW_10_01_DLV_STAT | VGTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_DLV_STAT | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_DLV_STAT | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_DLV_STAT | VLSTK | Distribution Status (Decentralized Warehouse Processing) | CHAR(1) | VLSTK |
| /SKN/S_SW_10_01_DLV_STAT | VRKME | Sales unit | UNIT(3) | VRKME |
| /SKN/S_SW_10_01_DLV_STAT | VSTEL | Shipping Point/Receiving Point | CHAR(4) | VSTEL |
| /SKN/S_SW_10_01_DLV_STAT | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_DLV_STAT | WADAT | Planned goods movement date | DATS(8) | WADAK |
| /SKN/S_SW_10_01_DLV_STAT | WADAT_IST | Actual Goods Movement Date | DATS(8) | WADAT_IST |
| /SKN/S_SW_10_01_DLV_STAT | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_DLV_STAT | WAVWR | Cost in document currency | CURR(13,2) | WAVWR |
| /SKN/S_SW_10_01_DLV_STAT | WBSTA | Goods movement status | CHAR(1) | WBSTA |
| /SKN/S_SW_10_01_DLV_STAT | WBSTK | Total goods movement status | CHAR(1) | WBSTK |
| /SKN/S_SW_10_01_DLV_STAT | WGBEZ | Material Group Description | CHAR(20) | WGBEZ |
| /SKN/S_SW_10_01_DLV_STAT | XBLNR | Reference Document Number | CHAR(25) | XBLNR_LIKP |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_DLV_STAT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_DLV_STAT OPTIONAL
*"----------------------------------------------------------------------
******* Begin change 29.11.17++
  TYPES: BEGIN OF TY_LIPS,
    VBELN      TYPE LIPS-VBELN,
    POSNR      TYPE LIPS-POSNR,
    LFIMG      TYPE LIPS-LFIMG,
    VRKME      TYPE LIPS-VRKME,
    LGMNG      TYPE LIPS-LGMNG,     "Actual quantity delivered IN stockkeeping units
    MEINS      TYPE LIPS-MEINS,     "Base UNIT OF Measure
    VGTYP      TYPE LIPS-VGTYP,     "SD document category (REFERENCE document)
    VGBEL      TYPE LIPS-VGBEL,     "Document NUMBER OF the REFERENCE document
    VGPOS      TYPE LIPS-VGPOS,     "Item NUMBER OF the REFERENCE item
    NETWR_LIPS TYPE LIPS-NETWR,     "Net VALUE (will be calculated)
    KLMENG     TYPE VBAP-KLMENG,    "Cumulative confirmed quantity IN base UNIT
    WAERK      TYPE VBAP-WAERK,     "SD Document CURRENCY
    NETWR_VBAP TYPE VBAP-NETWR,     "Net VALUE
    MPROK      TYPE VBAP-MPROK, """11-6-19
    WAVWR      TYPE VBAP-WAVWR,
  END OF TY_LIPS,
  TT_LIPS TYPE STANDARD TABLE OF TY_LIPS.
  TYPES: BEGIN OF TY_KNA1,
    KUNNR TYPE KNA1-KUNNR,
    NAME1 TYPE KNA1-NAME1,
  END OF TY_KNA1,
  TT_KNA1 TYPE TABLE OF TY_KNA1.
******* End change 29.11.17++
  DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
               LANGU          LANGU,
               BACKDAYS       INT4,
               BP1_FUNCT      PARVW,
               BP2_FUNCT      PARVW,
               BP3_FUNCT      PARVW,
               DATE_REF_FLD   NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  LV_BACKDAYS = 1.
  LV_DATE_REF_FLD  = 'WADAT'. "Planned goods movement date 'LFDAT'. "Delivery date
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU. """11-6-19
  SELECT_SINGLE: MANAGE_IN_UTC,
                 LANGU,
                 BACKDAYS,
                 BP1_FUNCT,
                 BP2_FUNCT,
                 BP3_FUNCT,
                 DATE_REF_FLD,
                 DURATION_UNIT.
  DATA_MULTY: VBELN        VBELN_VL,
              PSTYV        PSTYV_VL,
              LFART        LFART,
              VBTYP        VBTYP,
              VKORG        VKORG,
              VTWEG        VTWEG,
              SPART        SPART,
              BZIRK        BZIRK,
              KDGRP        KDGRP,
              VSTEL        VSTEL,
              ROUTE        ROUTE,
              ERDAT        ERDAT,
              ERNAM        ERNAM,
              AEDAT        AEDAT,
              LDDAT        LDDAT,
              TDDAT        TDDAT_D,
              LFDAT        LFDAT_V,
              KODAT        KODAT,
              WADAT        WADAK,
              WADAT_IST    WADAT_IST,
              PODAT        PODAT,
              ERDAT_LN     ERDAT,
              AEDAT_LN      AEDAT,
              FAKSK        FAKSK,
              DATUM        SY-DATUM,
              "            DURATION_M   /SKN/E_SW_DURATION_M,
              "            DURATION_H   /SKN/E_SW_DURATION_H,
              "            DURATION_D   /SKN/E_SW_DURATION_D,
              DURATION    /SKN/E_SW_DURATION,
              KUNNR       KUNWE, "Ship-to party
              KUNAG       KUNAG, "Sold-to party
              WBSTK       WBSTK,
              FKSTK       FKSTK,
              KOSTK       KOSTK,
              LVSTK       LVSTK,
              FKIVK       FKIVK,
              PKSTK       PKSTK,
              TRSTA       TRSTA,
              UVPAS       UVPAK_SU,
              UVPIS       UVPIK_SU,
              VESTK       VESTK,
              BLOCK       BLOCK_VB,
              PDSTK       PDSTK,
              LFSTA       LFSTA,
              LFGSA       LFGSA,
              WBSTA       WBSTA,
              FKSTA       FKSTA,
              KOSTA       KOSTA,
              LVSTA       LVSTA,
              FKIVP       FKIVP,
              PKSTA       PKSTA,
              KOQUA       KOQUA,
              PDSTA       PDSTA,
              CMGST       CMGST, ""19-7-16
              POSNR       POSNR_VL, """24-7-16
              MATNR       MATNR,
              BP1_CODE    KUNNR,
              BP2_CODE    KUNNR,
              BP3_CODE    KUNNR,
              BP_FUNCT    PARVW,
              VGTYP       VGTYP,   " 30.11.17++
              FKDAT       FKDAT,   " 30.01.18++
              MATKL       MATKL,   " 04.02.18++
              AUART       AUART,   """ 6/5/19
              MPROK       MPROK,  """"11-6-19
              WAVWR       WAVWR,
              VKBUR       VKBUR.
  SELECT_MULTY: VBELN,
                PSTYV,
                LFART,
                VBTYP,
                VKORG,
                VTWEG,
                SPART,
                BZIRK,
                KDGRP,
                VSTEL,
                ROUTE,
                ERDAT,
                ERNAM,
                AEDAT,
                LDDAT,
                TDDAT,
                LFDAT,
                KODAT,
                WADAT,
                WADAT_IST,
                PODAT,
                ERDAT_LN,
                AEDAT_LN,
                FAKSK,
                DATUM,
                "            DURATION_M,
                "            DURATION_H ,
                "            DURATION_D,
                DURATION,
                KUNNR , "Ship-to party
                KUNAG,  "Sold-to party
                WBSTK,
                FKSTK,
                KOSTK,
                LVSTK,
                FKIVK,
                PKSTK,
                TRSTA,
                UVPAS,
                UVPIS,
                VESTK,
                BLOCK,
                PDSTK,
                LFSTA,
                LFGSA,
                WBSTA,
                FKSTA,
                KOSTA,
                LVSTA,
                FKIVP,
                PKSTA,
                KOQUA,
                PDSTA,
                CMGST,  """19-7-16
                POSNR,  """24-7-16
                MATNR,
                BP1_CODE,
                BP2_CODE,
                BP3_CODE,
                VGTYP,       " 30.11.17
                FKDAT,       " 30.01.18
                MATKL,       " 04.02.18
                AUART,
                MPROK,  """ 11-6-19
                WAVWR,
                VKBUR        " 10.03.22++
                .
  CONVERT_MULTY: KUNNR ALPHA,
                 KUNAG ALPHA,
                 VBELN ALPHA,
                 BP1_CODE ALPHA,
                 BP2_CODE ALPHA,
                 BP3_CODE ALPHA,
                 MATNR MATN1,
                 AUART AUART. """6/5/19
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
  DATA : LV_DATA_POSNR TYPE POSNR.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
*** Begin change 29.11.17++
  DATA: LS_LIPS TYPE TY_LIPS,
        LS_KNA1 TYPE TY_KNA1.
  DATA: LT_LIPS TYPE TT_LIPS,
        LT_KNA1 TYPE TT_KNA1,
        LT_DATA TYPE TABLE OF /SKN/S_SW_10_01_DLV_STAT.
  FIELD-SYMBOLS: <FS_DATA> TYPE /SKN/S_SW_10_01_DLV_STAT.
*** End change 29.11.17++
  FIELD-SYMBOLS:  TYPE ANY ,
  <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA : LS_VBPA TYPE VBPA,
        LT_VBPA LIKE TABLE OF LS_VBPA.
  DATA: BEGIN OF LS_VBELN,
    VBELN TYPE VBELN_VL,
  END OF  LS_VBELN.
  DATA : LT_VBELN LIKE TABLE OF LS_VBELN.
  """ 11-6-19
  DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
        LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
        LV_DDTEXT LIKE  DD07V-DDTEXT.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    DATA: LV_IS_HANA(1) TYPE C.
    CALL FUNCTION '/SKN/F_SW_IS_RFCDEST_HANA'
      EXPORTING
        DEST    = LV_SW_DEST
      IMPORTING
        IS_HANA = LV_IS_HANA.
    IF LV_IS_HANA IS NOT INITIAL.
      CALL FUNCTION '/SKN/FH_SW_10_01_DLV_STAT'
        IMPORTING
          IS_ALERT = IS_ALERT
        TABLES
          T_SELECT = T_SELECT
          T_DATA   = T_DATA.
    ELSE.
      CALL FUNCTION '/SKN/FC_SW_10_01_DLV_STAT'
        IMPORTING
          IS_ALERT = IS_ALERT
        TABLES
          T_SELECT = T_SELECT
          T_DATA   = T_DATA.
    ENDIF.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
*   "----
*   if R_ERDAT[] is initial  and
*      R_AUDAT[] is initial .
*     R_ERDAT[] = R_DATUM[].
*   endif.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[]. "Document created
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "changed on
    WHEN 'LDDAT'.
      R_LDDAT[] = R_DATUM[]. "Loading Date
    WHEN 'TDDAT'.
      R_TDDAT[] = R_DATUM[]. "Transportation Planning Date
    WHEN 'KODAT'.
      R_KODAT[] = R_DATUM[]. "Picking Date
    WHEN 'WADAT'.
      R_WADAT[] = R_DATUM[]. "Planned goods movement date
    WHEN 'WADAT_IST'.
      R_WADAT_IST[] = R_DATUM[].  "Actual Goods Movement Date
    WHEN 'LFDAT'.
      R_LFDAT[] = R_DATUM[]. "Delivery date
*** 30.01.18++
    WHEN 'FKDAT'.
      R_FKDAT[] = R_DATUM[].
*** 30.01.18++
    WHEN OTHERS.
      R_WADAT[] = R_DATUM[]. "Planned goods movement date
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
*    from LIKP as a
*    inner join VBUK as k
  FROM VBUK AS K
  INNER JOIN LIKP AS A
  ON A~VBELN = K~VBELN
  INNER JOIN LIPS AS B
  ON A~VBELN = B~VBELN
  "INNER JOIN  VBAK AS v  """06/05/19
  LEFT JOIN  VBAK AS V  """06/05/19
  ON B~VGBEL = V~VBELN   """06/05/19
  INNER JOIN VBUP AS L
  ON B~VBELN = L~VBELN AND
     B~POSNR = L~POSNR
*** Begin change 05.11.17--
*    INNER JOIN kna1 as k1
*    on a~kunnr = k1~kunnr
*    INNER JOIN kna1 as k2
*    ON a~kunag = k2~kunnr
*** End change 05.11.17--
  INTO CORRESPONDING FIELDS OF TABLE T_DATA
  WHERE A~VBELN IN R_VBELN
  AND   B~PSTYV IN R_PSTYV
  AND   A~LFART IN R_LFART
  AND   A~VBTYP IN R_VBTYP
  AND   A~VKORG IN R_VKORG
  AND   B~VTWEG IN R_VTWEG
  AND   B~SPART IN R_SPART
  AND   A~BZIRK IN R_BZIRK
  AND   A~KDGRP IN R_KDGRP
  AND   A~VSTEL IN R_VSTEL
  AND   A~ROUTE IN R_ROUTE
  AND A~ERDAT IN R_ERDAT
  AND A~ERNAM IN R_ERNAM
  AND A~AEDAT IN R_AEDAT
  AND A~LDDAT IN R_LDDAT
  AND A~TDDAT IN R_TDDAT
  AND A~LFDAT IN R_LFDAT
  AND A~KODAT IN R_KODAT
  AND A~WADAT IN R_WADAT
  AND A~WADAT_IST IN R_WADAT_IST
  AND A~PODAT IN R_PODAT
  AND A~FKDAT IN R_FKDAT   " 30.01.18++
  AND A~KUNNR IN R_KUNNR  "Ship-to party
  AND A~KUNAG IN R_KUNAG  "Sold-to party
  AND A~FAKSK IN R_FAKSK
  AND K~WBSTK IN R_WBSTK
  AND K~FKSTK IN R_FKSTK
  AND K~KOSTK IN R_KOSTK
  AND K~LVSTK IN R_LVSTK
  AND K~FKIVK IN R_FKIVK
  AND K~PKSTK IN R_PKSTK
  AND K~TRSTA IN R_TRSTA
  AND K~UVPAS IN R_UVPAS
  AND K~UVPIS IN R_UVPIS
  AND K~VESTK IN R_VESTK
  AND K~BLOCK IN R_BLOCK
  AND K~PDSTK IN R_PDSTK
  AND K~CMGST IN R_CMGST  ""19-7-16
  AND B~ERDAT IN R_ERDAT_LN " ERDAT from line -LIPS
  AND B~AEDAT IN R_AEDAT_LN
  AND B~POSNR IN R_POSNR  """24-7-16
  AND B~MATNR IN R_MATNR  """24-7-16
  AND B~VGTYP IN R_VGTYP  " 04.02.18++
  AND B~MATKL IN R_MATKL  " 04.02.18++
  AND L~LFSTA IN R_LFSTA
  AND L~LFGSA IN R_LFGSA
  AND L~WBSTA IN R_WBSTA
  AND L~FKSTA IN R_FKSTA
  AND L~KOSTA IN R_KOSTA
  AND L~LVSTA IN R_LVSTA
  AND L~FKIVP IN R_FKIVP
  AND L~PKSTA IN R_PKSTA
  AND L~KOQUA IN R_KOQUA
  AND L~PDSTA IN R_PDSTA
  . " AND v~AUART IN r_AUART .  """6/5/19
  .
  DELETE T_DATA WHERE AUART NOT IN R_AUART.  """6/5/19
  DELETE T_DATA WHERE VKBUR NOT IN R_VKBUR.
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
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
******************************************************************************
  "--- Get BPs
  REFRESH LT_VBELN.
  LOOP AT T_DATA.
**** 03/22++
    SY_TABIX = SY-TABIX.
    IF T_DATA-VKBUR NOT IN R_VKBUR[].
      DELETE T_DATA[] INDEX SY_TABIX.
      CONTINUE.
    ENDIF.
**** 03/22++
    MOVE-CORRESPONDING T_DATA TO LS_VBELN.
    APPEND LS_VBELN TO LT_VBELN.
  ENDLOOP.
  SORT LT_VBELN BY VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_VBELN.
  IF LT_VBELN[] IS NOT INITIAL.
    "--- Fill R_BP_FUNCT ----
    REFRESH R_BP_FUNCT.
    SET_BP_RANGE 1.
    SET_BP_RANGE 2.
    SET_BP_RANGE 3.
    IF R_BP_FUNCT[] IS NOT INITIAL.
      SELECT *
        FROM VBPA
        INTO CORRESPONDING FIELDS OF TABLE LT_VBPA
        FOR ALL ENTRIES IN LT_VBELN
        WHERE VBELN EQ LT_VBELN-VBELN
        AND   PARVW IN R_BP_FUNCT.
      SORT LT_VBPA BY VBELN POSNR PARVW.
      LOOP AT T_DATA.
        SY_TABIX = SY-TABIX .
        LV_DATA_POSNR = T_DATA-POSNR.
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
**** Add additional values to Deliveries 29.11.17++ ****************
  CHECK T_DATA IS NOT INITIAL.
  LT_DATA = T_DATA[].
  SORT LT_DATA BY VBELN POSNR.
  SELECT LIPS~VBELN LIPS~POSNR LIPS~LGMNG LIPS~MEINS
         LIPS~VGTYP LIPS~VGBEL LIPS~VGPOS LIPS~NETWR AS LIPS_NETWR
         VBAP~NETWR AS VBAP_NETWR VBAP~KLMENG VBAP~WAERK
         VBAP~WAVWR VBAP~MPROK """ 11-6-19
    FROM LIPS LEFT OUTER JOIN VBAP ON  LIPS~VGBEL EQ VBAP~VBELN
                                   AND LIPS~VGPOS EQ VBAP~POSNR
    INTO CORRESPONDING FIELDS OF TABLE LT_LIPS
    FOR ALL ENTRIES IN LT_DATA
    WHERE LIPS~VBELN EQ LT_DATA-VBELN
    AND   LIPS~POSNR EQ LT_DATA-POSNR
    AND   LIPS~VGTYP IN R_VGTYP.
**    DELETE lt_data WHERE WAVWR NOT IN  r_WAVWR .  """11/6/19
**    DELETE lt_data WHERE MPROK NOT IN  r_MPROK .  """11/6/19
  IF SY-SUBRC = 0.
    SORT LT_LIPS BY VBELN POSNR.
  ENDIF.
  SORT LT_DATA BY KUNNR KUNAG.
  SELECT KNA1~KUNNR KNA1~NAME1
    FROM KNA1
    INTO TABLE LT_KNA1
    FOR ALL ENTRIES IN T_DATA
    WHERE ( KUNNR EQ T_DATA-KUNNR
    OR      KUNNR EQ T_DATA-KUNAG ).
  IF LT_KNA1 IS NOT INITIAL.
    SORT LT_KNA1 BY KUNNR.
  ENDIF.
**** Add additional values to Deliveries 29.11.17++ ****************
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    SY_TABIX = SY-TABIX .
*Ship-to party
**** Begin change 05.11.17--
*    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
*      EXPORTING
*        kunnr                = t_data-kunnr
*      IMPORTING
*        cust_desc            = t_data-shipto_desc
*      EXCEPTIONS
*        wrong_customer       = 1
*        OTHERS               = 2              .
*    IF sy-subrc <> 0.
*    ENDIF.
*Sold-to party desc
*    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
*      EXPORTING
*        kunnr                = t_data-kunag
*      IMPORTING
*        cust_desc            = t_data-soldto_desc
*      EXCEPTIONS
*        wrong_customer       = 1
*        OTHERS               = 2              .
*    IF sy-subrc <> 0.
*    ENDIF.
**** End change 05.11.17--
*Billing Block desc
    CALL FUNCTION '/SKN/F_SW_10_BIL_BLOCK_DESC'
      EXPORTING
        FAKSK      = <FS_DATA>-FAKSK         "t_data-faksk
        LANGU      = LV_LANGU
      IMPORTING
        BLOCK_DESC = <FS_DATA>-BLOCK_DESC    "t_data-block_desc
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
**** Begin change 29.11.17++
*Ship-to party
    READ TABLE LT_LIPS INTO LS_LIPS WITH KEY VBELN = <FS_DATA>-VBELN
                                             POSNR = <FS_DATA>-POSNR
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_DATA>-LGMNG = LS_LIPS-LGMNG.
      <FS_DATA>-MEINS = LS_LIPS-MEINS.
      <FS_DATA>-WAERK = LS_LIPS-WAERK.
      IF LS_LIPS-KLMENG <> 0.
        <FS_DATA>-NETWR_LIPS = LS_LIPS-LGMNG *
        ( LS_LIPS-NETWR_VBAP / LS_LIPS-KLMENG ).
      ELSE.
        <FS_DATA>-NETWR_LIPS = 0.
      ENDIF.
      <FS_DATA>-MPROK = LS_LIPS-MPROK. """11/6/19
      <FS_DATA>-WAVWR = LS_LIPS-WAVWR. """11/6/19
      IF <FS_DATA>-MPROK NOT IN R_MPROK.
        DELETE T_DATA.
        CONTINUE.
      ENDIF.
      IF <FS_DATA>-WAVWR NOT IN R_WAVWR.
        DELETE T_DATA.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR LS_KNA1.
    READ TABLE LT_KNA1 INTO LS_KNA1 WITH KEY KUNNR = <FS_DATA>-KUNNR
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_DATA>-SHIPTO_DESC = LS_KNA1-NAME1.
    ENDIF.
    CLEAR LS_KNA1.
    READ TABLE LT_KNA1 INTO LS_KNA1 WITH KEY KUNNR = <FS_DATA>-KUNAG
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      <FS_DATA>-SOLDTO_DESC = LS_KNA1-NAME1.
    ENDIF.
**** End change 29.11.17++
**** Begin change 04.02.18++
    IF <FS_DATA>-MATKL IS NOT INITIAL.
* Material group desc.
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
      IF SY-SUBRC <> 0.
*     Implement suitable error handling here
      ENDIF.
    ENDIF.
**** End change 04.02.18++
    "******* 11-6-19
***  IF  <fs_data>-MPROK is  not INITIAL .
    LV_DOMNAME = 'MPROK'.
    LV_DOMVALUE = <FS_DATA>-MPROK. """"t_data-MPROK.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      <FS_DATA>-MPROK_DESC = LV_DDTEXT.
    ENDIF.
***   ENDIF.
*    MODIFY t_data INDEX sy_tabix.  " 05.12.17--
  ENDLOOP.
  DELETE T_DATA WHERE WAVWR NOT IN  R_WAVWR .  """11/6/19
  DELETE T_DATA WHERE MPROK NOT IN  R_MPROK .  """11/6/19
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```