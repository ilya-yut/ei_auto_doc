# Exception Indicator: SD Delivery Status (General) - SW_10_01_DLV_STAT

## General Overview

This Exception Indicator (EI) monitors SD delivery document status and related header/item statuses in Sales and Distribution to identify deliveries that meet configurable date, organizational, status, and partner criteria. It provides visibility into delivery status patterns, goods movement status, billing status, and credit-check status, enabling detection of blocked deliveries, delayed processing, and status anomalies that require management attention.

This EI serves as an essential control for delivery and order fulfillment oversight by:
- Filtering delivery data by a configurable time window (BACKDAYS) and by a reference date field (e.g. planned goods movement date, delivery date, picking date)
- Calculating status duration in selected time units (DURATION, DURATION_UNIT) relative to the reference date for age-in-status analysis
- Supporting filtering by sales organization, distribution channel, division, shipping point, route, customer (ship-to, sold-to), and three configurable business partner roles
- Enabling multi-dimensional filtering by delivery, billing, picking, packing, and goods movement statuses (WBSTK, FKSTK, LFSTA, KOSTK, etc.) and by document characteristics (type, category, material, sales office)
- Providing partner enrichment (BP1/BP2/BP3 code, function, name) from VBPA for relationship and accountability visibility

This monitoring supports identification of deliveries stuck in specific statuses, billing blocks, credit blocks, and delayed goods movements. The EI is valuable for order-to-cash reviews, delivery performance analysis, and exception management in SD.

The EI retrieves delivery and status data from SAP SD tables (LIKP – Delivery Header, LIPS – Delivery Item, VBUK – SD Document Header Status, VBUP – SD Document Item Status, VBAK – Sales Document Header), joins to VBAP for order item data and to VBPA and KNA1 for partner and customer names, then filters by all configured parameters and computes duration per delivery line. Results are returned in the T_DATA structure for reporting and alerting.

## Problem Description

Failure to monitor SD delivery status and related header/item statuses creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected billing blocks or delayed billing status can delay revenue recognition and distort period-end reporting
- Deliveries with prolonged status duration without visibility may indicate disputed or stalled transactions affecting cash flow
- Unmonitored credit-check status (CMGST) on deliveries can mask credit exposure and collection risk
- Late discovery of blocked or incomplete deliveries can disrupt month-end close and reconciliation processes

**Sales Operations and Control Risks**
- Deliveries stuck in picking, packing, or goods movement status without oversight may indicate warehouse or logistics bottlenecks
- Lack of filtering by partner function (ship-to, sold-to, additional BP roles) can obscure customer-specific fulfillment issues
- Unusual concentration of statuses by sales organization, distribution channel, or sales office may reflect process or master data problems
- Missing visibility into delivery type, route, and shipping point patterns can hinder root-cause analysis of delivery delays

**Management Visibility and Decision-Making Risks**
- Absence of status-duration analysis delays awareness of aging deliveries and required escalation
- Unidentified billing or delivery blocks can lead to customer disputes and missed service-level targets
- Lack of multi-dimensional delivery status monitoring limits ability to prioritize corrective actions and resource allocation

## Suggested Resolution

**Immediate Response**
- Review the delivery and status data flagged by the EI to determine the nature of the exception (status duration, block indicators, status combinations)
- Verify high-impact or blocked deliveries using transaction VL03N (Display Delivery) and check header/item status and block reasons
- Check billing block (FAKSK), delivery block, and credit status (CMGST) to confirm whether release or master data change is needed
- Identify business context: customer disputes, credit hold, missing materials, or process errors

**System Assessment**
- Analyze the date and duration parameters (BACKDAYS, DATE_REF_FLD, DURATION, DURATION_UNIT) to ensure the reference date and time window align with the intended monitoring period
- Compare current delivery status distribution to prior runs or periods using the same organizational and status filters
- Review partner parameters (BP1_FUNCT/CODE, BP2_FUNCT/CODE, BP3_FUNCT/CODE) to ensure the correct partner roles are used for filtering and enrichment
- Validate status filters (WBSTK, FKSTK, LFSTA, KOSTK, etc.) and organizational dimensions (VKORG, VTWEG, SPART, VSTEL, VKBUR) against business requirements

**Corrective Actions**
- Release billing or delivery blocks (e.g. via VKM3, VL02N) or update master data (customer, material, credit) as appropriate after validation
- Escalate blocked or aged deliveries to logistics and credit management for resolution
- Adjust EI parameters (BACKDAYS, DURATION range, status filters) to refine future monitoring and alerting
- Document exceptions and resolutions for audit and performance reporting
- Schedule recurring EI execution to maintain continuous visibility into delivery status and duration

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
| 81-85 | UVK01 - UVK05 | Header reserves 1 - Header reserves 5 | CHAR | 1 | 0 | UVK01 | STATV |
| 86-90 | UVP01 - UVP05 | Item reserves 1 - Item reserves 5 | CHAR | 1 | 0 | UVP01 | STATV |
| 91 | UVPAS | It.data packaging | CHAR | 1 | 0 | UVPAK_SU | STATV |
| 92 | UVPIS | It.data picking/putaway | CHAR | 1 | 0 | UVPIK_SU | STATV |
| 93-97 | UVS01 - UVS05 | Total reserves 1 - Total reserves 5 | CHAR | 1 | 0 | UVS01 | STATV |
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

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**ARKTX** (Description):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**AUART** (Sales Document Type):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**BACKDAYS** (Backdays):

Number of days to look back from today. When no date range is supplied, the EI builds the selection using SY-DATUM minus BACKDAYS. Used to limit the delivery date window.

**BLOCK** (Indicator: Document preselected for archiving):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**BLOCK_DESC** (Billing block desc.):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**BP1_CODE** (Partner1 - Code):

Partner code for first partner role. Filters deliveries by this partner and populates the output; BP1_NAME is enriched from customer master.

**BP1_FUNCT** (Partner1 - Function):

Partner function for first business partner (e.g. sold-to, ship-to). Used with BP1_CODE to filter and enrich partner data from VBPA.

**BP1_NAME** (Name):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**BP2_CODE** (Partner2 - Code):

Partner code for second partner role. Filters and enriches output.

**BP2_FUNCT** (Partner2 - Function):

Partner function for second business partner. Works with BP2_CODE and BP2_NAME.

**BP2_NAME** (Name):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**BP3_CODE** (Partner3 - Code):

Partner code for third partner role. Filters and enriches output.

**BP3_FUNCT** (Partner3 - Function):

Partner function for third business partner. Works with BP3_CODE and BP3_NAME.

**BP3_NAME** (Name):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**BZIRK** (Sales district):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**CMGST** (Overall CreditStatus):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**COSTA** (Confirmation status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**DATE_REF_FLD** (Date Ref Field):

Field used as the reference date for filtering and for duration calculation (e.g. WADAT, LFDAT, ERDAT, AEDAT, KODAT). The EI maps this to the corresponding delivery/header date range and uses it to compute status duration.

**DUMMY** (Single-Character Flag):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**DURATION** (Duration In Time Units):

Duration in the selected time unit (see DURATION_UNIT) between the reference date (DATE_REF_FLD) and current date. The EI calculates this per delivery and filters by the supplied range to focus on aged-in-status deliveries.

**DURATION_D** (Duration In Days):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**DURATION_UNIT** (Duration Unit):

Unit for duration calculation (e.g. D = days). Used together with DATE_REF_FLD and DURATION to filter deliveries by how long they have been in the current status.

**ERDAT** (Created On):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**ERNAM** (Created By):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**ERZET** (Time):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**FAKSK** (Billing block):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**FKDAT** (Billing Date):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**FKIVK** (Totals status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**FKIVP** (Interco. Bill.Status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**FKSTA** (Billing Status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**FKSTK** (Billing status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**HDALL** (On Hold):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**HDALS** (Pos. Hold):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KDAUF** (Sales Order):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KDGRP** (Customer group):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KDPOS** (Sales order item):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KLMENG** (Cumul.confirmed qty):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KODAT** (Picking Date):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KOQUA** (Pick confirmation):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KOQUK** (Pick confirmation):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KOSTA** (Picking status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KOSTK** (Overall pick.status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KUNAG** (Sold-to party):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KUNNR** (Ship-to party):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**KWMENG** (Order Quantity):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LANG** (Language for texts):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LDDAT** (Loading Date):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LFART** (Delivery Type):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LFBNR** (Reference Document):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LFDAT** (Delivery Date):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LFGSA** (Overall deliv.status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LFIMG** (Delivery quantity):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LFPOS** (Reference Doc. Item):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LFSTA** (Delivery status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LGMNG** (Actual delivery qty):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LVSTA** (WM activity status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**LVSTK** (Overall WM status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**MATKL** (Material Group):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**MATNR** (Material):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**MEINS** (Base Unit of Measure):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**MPROK** (Manual price):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**MPROK_DESC** (Short text):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**NETWR_LIPS** (Net Value):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**NETWR_VBAP** (Net value):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**PDSTA** (Proof of delivery status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**PDSTK** (Proof of delivery status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**PKSTA** (Packing status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**PKSTK** (Packing status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**PODAT** (Proof of delivery date):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**POSNR** (Item):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**PSTYV** (Item category):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**ROUTE** (Route):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**SHIPTO_DESC** (Name):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**SOLDTO_DESC** (Name):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**SPART** (Division):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**SPE_TMPID** (Temp Inb.):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**TDDAT** (Transptn Plang Date):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**TRSTA** (Trns.plan.status):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**UVK01 - UVK05** (Header reserves 1 - Header reserves 5):

Reserve/status indicators for header (UVK), item (UVP), or total (UVS). Used to filter deliveries by the corresponding status dimension. All five values share the same type and domain.

**UVP01 - UVP05** (Item reserves 1 - Item reserves 5):

Reserve/status indicators for header (UVK), item (UVP), or total (UVS). Used to filter deliveries by the corresponding status dimension. All five values share the same type and domain.

**UVPAS** (It.data packaging):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**UVPIS** (It.data picking/putaway):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**UVS01 - UVS05** (Total reserves 1 - Total reserves 5):

Reserve/status indicators for header (UVK), item (UVP), or total (UVS). Used to filter deliveries by the corresponding status dimension. All five values share the same type and domain.

**VBELN** (Delivery):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VBTYP** (SD document categ.):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VESTK** (HU placed in stock):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VGBEL** (Reference Document):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VGPOS** (Reference Item):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VGTYP** (SD document categ. (prev)):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VKBUR** (Sales Office):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VKORG** (Sales Organization):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VLSTK** (Status Decent. Whse):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VRKME** (Sales Unit):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VSTEL** (Shipping Point/Receiving Pt):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**VTWEG** (Distribution Channel):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**WADAT** (Pland Gds Mvmnt Date):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**WADAT_IST** (Act. Gds Mvmnt Date):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**WAERK** (Document Currency):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**WAVWR** (Cost):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**WBSTA** (Goods movement stat.):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**WBSTK** (Total gds mvt stat.):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**WGBEZ** (Material Group Desc.):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

**XBLNR** (Reference):

Used to filter or control delivery/status data selection or output. See structure and code for field usage and mapping.

### Parameter Relationships

**Time-Based and Duration Parameters:**
- **BACKDAYS** defines the lookback window in days from today; when no date range is supplied, the EI builds the selection range using SY-DATUM minus BACKDAYS.
- **DATE_REF_FLD** determines which date field (e.g. WADAT, LFDAT, ERDAT, AEDAT, KODAT) is used both for filtering the delivery date range and as the reference for **DURATION** calculation.
- **DURATION** and **DURATION_UNIT** work together: the EI computes the time difference between the reference date (from DATE_REF_FLD) and the current date in the selected unit (e.g. D = days); results are filtered by the DURATION range. Use BACKDAYS and DATE_REF_FLD to set the data window, then DURATION/DURATION_UNIT to focus on deliveries aged in status by a certain amount.

**Business Partner Parameters:**
- **BP1_FUNCT**, **BP2_FUNCT**, **BP3_FUNCT** define which partner functions (e.g. sold-to, ship-to, bill-to) are read from VBPA; **BP1_CODE**, **BP2_CODE**, **BP3_CODE** filter and populate the partner codes and **BP1_NAME**, **BP2_NAME**, **BP3_NAME** are enriched from customer master. Configure the three function/code/name pairs together to analyze deliveries by the desired partner dimensions.

**Status and Organizational Parameters:**
- Status parameters (e.g. WBSTK, FKSTK, LFSTA, KOSTK, CMGST) and organizational parameters (VKORG, VTWEG, SPART, VSTEL, VKBUR) are used together to narrow the result set to the relevant delivery statuses and organizational scope. Combine them according to the monitoring scenario (e.g. billing status + sales org, or goods movement status + distribution channel).

### Default Values

- **BACKDAYS** — Default: `1` (used when no date range is supplied to build the selection from SY-DATUM minus BACKDAYS).
- **DATE_REF_FLD** — Default: `WADAT` (planned goods movement date used as reference for filtering and duration calculation when not supplied).
- **DURATION_UNIT** — Default: `D` (days; used for duration calculation when not supplied).
- **LANG** — Default: system language (SY-LANGU); used for description lookups (e.g. billing block, domain values).

**Note:** Other parameters default to initial (empty or 0) when not supplied; the EI then selects all values for that dimension (e.g. no filter on that field).

### Practical Configuration Examples

**Use Case 1: Deliveries in status longer than 7 days (by planned goods movement date)**
```
BACKDAYS = 30
DATE_REF_FLD = WADAT
DURATION = 7
DURATION_UNIT = D
```
**Purpose:** Focus on deliveries whose planned goods movement date is at least 7 days ago, within a 30-day lookback, to identify aged deliveries for follow-up.

**Use Case 2: Billing-blocked deliveries in a sales organization**
```
VKORG = 1010
FAKSK = (relevant billing block codes)
BACKDAYS = 14
DATE_REF_FLD = AEDAT
```
**Purpose:** Monitor deliveries with billing block in sales organization 1010 over the last 14 days by change date, for release or exception handling.

**Use Case 3: Partner-based view (ship-to and sold-to)**
```
BP1_FUNCT = WE
BP2_FUNCT = AG
BP1_CODE = (range of ship-to parties)
BP2_CODE = (range of sold-to parties)
WBSTK = (e.g. C for complete)
```
**Purpose:** Analyze delivery status by ship-to (WE) and sold-to (AG) partner and filter by total goods movement status.

**Use Case 4: Credit status and sales office**
```
CMGST = (e.g. credit block status)
VKBUR = 1000
DURATION = 1
DURATION_UNIT = D
```
**Purpose:** Identify deliveries with specific credit-check status in sales office 1000 and duration of at least 1 day for escalation.

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
FUNCTION /skn/f_sw_10_01_dlv_stat .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_DLV_STAT OPTIONAL
*"----------------------------------------------------------------------
******* Begin change 29.11.17++
  TYPES: BEGIN OF ty_lips,
    vbeln      TYPE lips-vbeln,
    posnr      TYPE lips-posnr,
    lfimg      TYPE lips-lfimg,
    vrkme      TYPE lips-vrkme,
    lgmng      TYPE lips-lgmng,     "Actual quantity delivered IN stockkeeping units
    meins      TYPE lips-meins,     "Base UNIT OF Measure
    vgtyp      TYPE lips-vgtyp,     "SD document category (REFERENCE document)
    vgbel      TYPE lips-vgbel,     "Document NUMBER OF the REFERENCE document
    vgpos      TYPE lips-vgpos,     "Item NUMBER OF the REFERENCE item
    netwr_lips TYPE lips-netwr,     "Net VALUE (will be calculated)
    klmeng     TYPE vbap-klmeng,    "Cumulative confirmed quantity IN base UNIT
    waerk      TYPE vbap-waerk,     "SD Document CURRENCY
    netwr_vbap TYPE vbap-netwr,     "Net VALUE
    mprok      TYPE vbap-mprok, """11-6-19
    wavwr      TYPE vbap-wavwr,
  END OF ty_lips,
  tt_lips TYPE STANDARD TABLE OF ty_lips.
  TYPES: BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1,
  tt_kna1 TYPE TABLE OF ty_kna1.
******* End change 29.11.17++
  data_single: manage_in_utc  char1 ,
               langu          langu,
               backdays       int4,
               bp1_funct      parvw,
               bp2_funct      parvw,
               bp3_funct      parvw,
               date_ref_fld   name_feld,
               duration_unit  /skn/e_sw_duration_unit.
  lv_backdays = 1.
  lv_date_ref_fld  = 'WADAT'. "Planned goods movement date 'LFDAT'. "Delivery date
  lv_duration_unit = 'D'.
  lv_langu = sy-langu. """11-6-19
  select_single: manage_in_utc,
                 langu,
                 backdays,
                 bp1_funct,
                 bp2_funct,
                 bp3_funct,
                 date_ref_fld,
                 duration_unit.
  data_multy: vbeln        vbeln_vl,
              pstyv        pstyv_vl,
              lfart        lfart,
              vbtyp        vbtyp,
              vkorg        vkorg,
              vtweg        vtweg,
              spart        spart,
              bzirk        bzirk,
              kdgrp        kdgrp,
              vstel        vstel,
              route        route,
              erdat        erdat,
              ernam        ernam,
              aedat        aedat,
              lddat        lddat,
              tddat        tddat_d,
              lfdat        lfdat_v,
              kodat        kodat,
              wadat        wadak,
              wadat_ist    wadat_ist,
              podat        podat,
              erdat_ln     erdat,
              aedat_ln      aedat,
              faksk        faksk,
              datum        sy-datum,
              "            DURATION_M   /SKN/E_SW_DURATION_M,
              "            DURATION_H   /SKN/E_SW_DURATION_H,
              "            DURATION_D   /SKN/E_SW_DURATION_D,
              duration    /skn/e_sw_duration,
              kunnr       kunwe, "Ship-to party
              kunag       kunag, "Sold-to party
              wbstk       wbstk,
              fkstk       fkstk,
              kostk       kostk,
              lvstk       lvstk,
              fkivk       fkivk,
              pkstk       pkstk,
              trsta       trsta,
              uvpas       uvpak_su,
              uvpis       uvpik_su,
              vestk       vestk,
              block       block_vb,
              pdstk       pdstk,
              lfsta       lfsta,
              lfgsa       lfgsa,
              wbsta       wbsta,
              fksta       fksta,
              kosta       kosta,
              lvsta       lvsta,
              fkivp       fkivp,
              pksta       pksta,
              koqua       koqua,
              pdsta       pdsta,
              cmgst       cmgst, ""19-7-16
              posnr       posnr_vl, """24-7-16
              matnr       matnr,
              bp1_code    kunnr,
              bp2_code    kunnr,
              bp3_code    kunnr,
              bp_funct    parvw,
              vgtyp       vgtyp,   " 30.11.17++
              fkdat       fkdat,   " 30.01.18++
              matkl       matkl,   " 04.02.18++
              auart       auart,   """ 6/5/19
              mprok       mprok,  """"11-6-19
              wavwr       wavwr,
              vkbur       vkbur.
  select_multy: vbeln,
                pstyv,
                lfart,
                vbtyp,
                vkorg,
                vtweg,
                spart,
                bzirk,
                kdgrp,
                vstel,
                route,
                erdat,
                ernam,
                aedat,
                lddat,
                tddat,
                lfdat,
                kodat,
                wadat,
                wadat_ist,
                podat,
                erdat_ln,
                aedat_ln,
                faksk,
                datum,
                "            DURATION_M,
                "            DURATION_H ,
                "            DURATION_D,
                duration,
                kunnr , "Ship-to party
                kunag,  "Sold-to party
                wbstk,
                fkstk,
                kostk,
                lvstk,
                fkivk,
                pkstk,
                trsta,
                uvpas,
                uvpis,
                vestk,
                block,
                pdstk,
                lfsta,
                lfgsa,
                wbsta,
                fksta,
                kosta,
                lvsta,
                fkivp,
                pksta,
                koqua,
                pdsta,
                cmgst,  """19-7-16
                posnr,  """24-7-16
                matnr,
                bp1_code,
                bp2_code,
                bp3_code,
                vgtyp,       " 30.11.17
                fkdat,       " 30.01.18
                matkl,       " 04.02.18
                auart,
                mprok,  """ 11-6-19
                wavwr,
                vkbur        " 10.03.22++
                .
  convert_multy: kunnr alpha,
                 kunag alpha,
                 vbeln alpha,
                 bp1_code alpha,
                 bp2_code alpha,
                 bp3_code alpha,
                 matnr matn1,
                 auart auart. """6/5/19
  ""Tanya 14/11/18 :
  convert_single:  bp1_funct parvw ,
                   bp2_funct parvw ,
                   bp3_funct parvw .
  RANGES : r_fld_name FOR dd03p-fieldname,
  r_fld_val FOR dd03p-fieldname .
  DATA :   fld_name TYPE fieldname.
  DATA : i TYPE i,
        ci(1) TYPE c,
        nfields TYPE i VALUE 3.   "
  DATA : backdays  TYPE i ,
        date_from LIKE sy-datum .
  DATA : langu LIKE sy-langu .
  DATA : is_out(1) TYPE c.
  DATA : time_diff TYPE  int4 .
  DATA : w_data LIKE LINE OF t_data .
  DATA : wa_vbpa TYPE vbpa.
  DATA : lv_vbeln TYPE vbeln,
        lv_posnr TYPE posnr,
        lv_parvw TYPE parvw,
        lv_kunnr TYPE  kunnr,
        lv_kunnr_name TYPE  name1_gp,
        lv_lifnr TYPE  lifnr,
        lv_lifnr_name TYPE  name1_gp,
        lv_pernr TYPE  pernr_d,
        lv_pernr_name TYPE  name1_gp,
        lv_nrart TYPE nrart.
  DATA : lv_data_posnr TYPE posnr.
  DATA : sy_tabix LIKE sy-tabix .
  DATA : fld(60) TYPE c .
  DATA : ref_date TYPE d.
*data: lra_range type range of DD03P-FIELDNAME.
*** Begin change 29.11.17++
  DATA: ls_lips TYPE ty_lips,
        ls_kna1 TYPE ty_kna1.
  DATA: lt_lips TYPE tt_lips,
        lt_kna1 TYPE tt_kna1,
        lt_data TYPE TABLE OF /skn/s_sw_10_01_dlv_stat.
  FIELD-SYMBOLS: <fs_data> TYPE /skn/s_sw_10_01_dlv_stat.
*** End change 29.11.17++
  FIELD-SYMBOLS: <fs> TYPE any ,
  <fs_v> TYPE any .
  DATA : BEGIN OF sw_structure OCCURS 0.
          INCLUDE STRUCTURE /skn/s_sw_s_fcat .
  DATA : END OF sw_structure .
  DATA : ls_vbpa TYPE vbpa,
        lt_vbpa LIKE TABLE OF ls_vbpa.
  DATA: BEGIN OF ls_vbeln,
    vbeln TYPE vbeln_vl,
  END OF  ls_vbeln.
  DATA : lt_vbeln LIKE TABLE OF ls_vbeln.
  """ 11-6-19
  DATA: lv_domname LIKE  dd07v-domname,
        lv_domvalue LIKE  dd07v-domvalue_l,
        lv_ddtext LIKE  dd07v-ddtext.
  "--- Run Cloud Mode -----
  data_single: sw_dest rfcdest.             .
  select_single: sw_dest.
  IF lv_sw_dest IS NOT INITIAL.
    DATA: lv_is_hana(1) TYPE c.
    CALL FUNCTION '/SKN/F_SW_IS_RFCDEST_HANA'
      EXPORTING
        dest    = lv_sw_dest
      IMPORTING
        is_hana = lv_is_hana.
    IF lv_is_hana IS NOT INITIAL.
      CALL FUNCTION '/SKN/FH_SW_10_01_DLV_STAT'
        IMPORTING
          is_alert = is_alert
        TABLES
          t_select = t_select
          t_data   = t_data.
    ELSE.
      CALL FUNCTION '/SKN/FC_SW_10_01_DLV_STAT'
        IMPORTING
          is_alert = is_alert
        TABLES
          t_select = t_select
          t_data   = t_data.
    ENDIF.
  ENDIF.
  CHECK lv_sw_dest IS INITIAL.
  "--- Run Cloud Mode -----
  IF r_datum[] IS INITIAL .
    rs_datum-sign   = 'I' .
    rs_datum-option = 'GE' .
    date_from       = sy-datum - lv_backdays .
    rs_datum-low    = date_from .
    APPEND rs_datum TO r_datum.
  ENDIF.
*   "----
*   if R_ERDAT[] is initial  and
*      R_AUDAT[] is initial .
*     R_ERDAT[] = R_DATUM[].
*   endif.
  "--- Set Reference Date Field
  CASE lv_date_ref_fld.
    WHEN 'ERDAT'.
      r_erdat[] = r_datum[]. "Document created
    WHEN 'AEDAT'.
      r_aedat[] = r_datum[]. "changed on
    WHEN 'LDDAT'.
      r_lddat[] = r_datum[]. "Loading Date
    WHEN 'TDDAT'.
      r_tddat[] = r_datum[]. "Transportation Planning Date
    WHEN 'KODAT'.
      r_kodat[] = r_datum[]. "Picking Date
    WHEN 'WADAT'.
      r_wadat[] = r_datum[]. "Planned goods movement date
    WHEN 'WADAT_IST'.
      r_wadat_ist[] = r_datum[].  "Actual Goods Movement Date
    WHEN 'LFDAT'.
      r_lfdat[] = r_datum[]. "Delivery date
*** 30.01.18++
    WHEN 'FKDAT'.
      r_fkdat[] = r_datum[].
*** 30.01.18++
    WHEN OTHERS.
      r_wadat[] = r_datum[]. "Planned goods movement date
  ENDCASE.
*--- Retrieve data
  CLEAR is_alert .
  REFRESH t_data.
  SELECT *
*    from LIKP as a
*    inner join VBUK as k
  FROM vbuk AS k
  INNER JOIN likp AS a
  ON a~vbeln = k~vbeln
  INNER JOIN lips AS b
  ON a~vbeln = b~vbeln
  "INNER JOIN  VBAK AS v  """06/05/19
  LEFT JOIN  vbak AS v  """06/05/19
  ON b~vgbel = v~vbeln   """06/05/19
  INNER JOIN vbup AS l
  ON b~vbeln = l~vbeln AND
     b~posnr = l~posnr
*** Begin change 05.11.17--
*    INNER JOIN kna1 as k1
*    on a~kunnr = k1~kunnr
*    INNER JOIN kna1 as k2
*    ON a~kunag = k2~kunnr
*** End change 05.11.17--
  INTO CORRESPONDING FIELDS OF TABLE t_data
  WHERE a~vbeln IN r_vbeln
  AND   b~pstyv IN r_pstyv
  AND   a~lfart IN r_lfart
  AND   a~vbtyp IN r_vbtyp
  AND   a~vkorg IN r_vkorg
  AND   b~vtweg IN r_vtweg
  AND   b~spart IN r_spart
  AND   a~bzirk IN r_bzirk
  AND   a~kdgrp IN r_kdgrp
  AND   a~vstel IN r_vstel
  AND   a~route IN r_route
  AND a~erdat IN r_erdat
  AND a~ernam IN r_ernam
  AND a~aedat IN r_aedat
  AND a~lddat IN r_lddat
  AND a~tddat IN r_tddat
  AND a~lfdat IN r_lfdat
  AND a~kodat IN r_kodat
  AND a~wadat IN r_wadat
  AND a~wadat_ist IN r_wadat_ist
  AND a~podat IN r_podat
  AND a~fkdat IN r_fkdat   " 30.01.18++
  AND a~kunnr IN r_kunnr  "Ship-to party
  AND a~kunag IN r_kunag  "Sold-to party
  AND a~faksk IN r_faksk
  AND k~wbstk IN r_wbstk
  AND k~fkstk IN r_fkstk
  AND k~kostk IN r_kostk
  AND k~lvstk IN r_lvstk
  AND k~fkivk IN r_fkivk
  AND k~pkstk IN r_pkstk
  AND k~trsta IN r_trsta
  AND k~uvpas IN r_uvpas
  AND k~uvpis IN r_uvpis
  AND k~vestk IN r_vestk
  AND k~block IN r_block
  AND k~pdstk IN r_pdstk
  AND k~cmgst IN r_cmgst  ""19-7-16
  AND b~erdat IN r_erdat_ln " ERDAT from line -LIPS
  AND b~aedat IN r_aedat_ln
  AND b~posnr IN r_posnr  """24-7-16
  AND b~matnr IN r_matnr  """24-7-16
  AND b~vgtyp IN r_vgtyp  " 04.02.18++
  AND b~matkl IN r_matkl  " 04.02.18++
  AND l~lfsta IN r_lfsta
  AND l~lfgsa IN r_lfgsa
  AND l~wbsta IN r_wbsta
  AND l~fksta IN r_fksta
  AND l~kosta IN r_kosta
  AND l~lvsta IN r_lvsta
  AND l~fkivp IN r_fkivp
  AND l~pksta IN r_pksta
  AND l~koqua IN r_koqua
  AND l~pdsta IN r_pdsta
  . " AND v~AUART IN r_AUART .  """6/5/19
  .
  DELETE t_data WHERE auart NOT IN r_auart.  """6/5/19
  DELETE t_data WHERE vkbur NOT IN r_vkbur.
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT t_data .
    sy_tabix = sy-tabix .
    CONCATENATE 'T_DATA-' lv_date_ref_fld INTO fld .
    ASSIGN (fld) TO <fs>.
    ref_date = <fs> .
    IF NOT ref_date IS INITIAL.
      t_data-duration_unit = lv_duration_unit.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          d_from      = ref_date
          t_from      = sy-uzeit
          d_to        = sy-datum
          t_to        = sy-uzeit
          time_unit   = lv_duration_unit  "'D'
        IMPORTING
          time_diff   = time_diff
        EXCEPTIONS
          wrong_value = 1
          OTHERS      = 2.
      IF sy-subrc = 0.
        IF time_diff < '999999'.
          t_data-duration  = time_diff .
        ELSE.
          t_data-duration  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY t_data INDEX sy_tabix.
    ENDIF.
  ENDLOOP.
  DELETE t_data WHERE duration  NOT IN r_duration .
******************************************************************************
  "--- Get BPs
  REFRESH lt_vbeln.
  LOOP AT t_data.
**** 03/22++
    sy_tabix = sy-tabix.
    IF NOT t_data-vkbur IN r_vkbur[].
      DELETE t_data[] INDEX sy_tabix.
      CONTINUE.
    ENDIF.
**** 03/22++
    MOVE-CORRESPONDING t_data TO ls_vbeln.
    APPEND ls_vbeln TO lt_vbeln.
  ENDLOOP.
  SORT lt_vbeln BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_vbeln.
  IF lt_vbeln[] IS NOT INITIAL.
    "--- Fill R_BP_FUNCT ----
    REFRESH r_bp_funct.
    set_bp_range 1.
    set_bp_range 2.
    set_bp_range 3.
    IF r_bp_funct[] IS NOT INITIAL.
      SELECT *
        FROM vbpa
        INTO CORRESPONDING FIELDS OF TABLE lt_vbpa
        FOR ALL ENTRIES IN lt_vbeln
        WHERE vbeln EQ lt_vbeln-vbeln
        AND   parvw IN r_bp_funct.
      SORT lt_vbpa BY vbeln posnr parvw.
      LOOP AT t_data.
        sy_tabix = sy-tabix .
        lv_data_posnr = t_data-posnr.
        get_bp_attr 1.
        get_bp_attr 2.
        get_bp_attr 3.
        MODIFY t_data INDEX sy_tabix.
      ENDLOOP.
      DELETE t_data WHERE bp1_code NOT IN r_bp1_code.
      DELETE t_data WHERE bp2_code NOT IN r_bp2_code.
      DELETE t_data WHERE bp3_code NOT IN r_bp3_code.
    ENDIF.
  ENDIF.
  "--- Get BPs
**** Add additional values to Deliveries 29.11.17++ ****************
  CHECK t_data IS NOT INITIAL.
  lt_data = t_data[].
  SORT lt_data BY vbeln posnr.
  SELECT lips~vbeln lips~posnr lips~lgmng lips~meins
         lips~vgtyp lips~vgbel lips~vgpos lips~netwr AS lips_netwr
         vbap~netwr AS vbap_netwr vbap~klmeng vbap~waerk
         vbap~wavwr vbap~mprok """ 11-6-19
    FROM lips LEFT OUTER JOIN vbap ON  lips~vgbel EQ vbap~vbeln
                                   AND lips~vgpos EQ vbap~posnr
    INTO CORRESPONDING FIELDS OF TABLE lt_lips
    FOR ALL ENTRIES IN lt_data
    WHERE lips~vbeln EQ lt_data-vbeln
    AND   lips~posnr EQ lt_data-posnr
    AND   lips~vgtyp IN r_vgtyp.
**    DELETE lt_data WHERE WAVWR NOT IN  r_WAVWR .  """11/6/19
**    DELETE lt_data WHERE MPROK NOT IN  r_MPROK .  """11/6/19
  IF sy-subrc = 0.
    SORT lt_lips BY vbeln posnr.
  ENDIF.
  SORT lt_data BY kunnr kunag.
  SELECT kna1~kunnr kna1~name1
    FROM kna1
    INTO TABLE lt_kna1
    FOR ALL ENTRIES IN t_data
    WHERE ( kunnr EQ t_data-kunnr
    OR      kunnr EQ t_data-kunag ).
  IF lt_kna1 IS NOT INITIAL.
    SORT lt_kna1 BY kunnr.
  ENDIF.
**** Add additional values to Deliveries 29.11.17++ ****************
  LOOP AT t_data ASSIGNING <fs_data>.
    sy_tabix = sy-tabix .
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
        faksk      = <fs_data>-faksk         "t_data-faksk
        langu      = lv_langu
      IMPORTING
        block_desc = <fs_data>-block_desc    "t_data-block_desc
      EXCEPTIONS
        wrong_code = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
    ENDIF.
**** Begin change 29.11.17++
*Ship-to party
    READ TABLE lt_lips INTO ls_lips WITH KEY vbeln = <fs_data>-vbeln
                                             posnr = <fs_data>-posnr
    BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_data>-lgmng = ls_lips-lgmng.
      <fs_data>-meins = ls_lips-meins.
      <fs_data>-waerk = ls_lips-waerk.
      IF ls_lips-klmeng <> 0.
        <fs_data>-netwr_lips = ls_lips-lgmng *
        ( ls_lips-netwr_vbap / ls_lips-klmeng ).
      ELSE.
        <fs_data>-netwr_lips = 0.
      ENDIF.
      <fs_data>-mprok = ls_lips-mprok. """11/6/19
      <fs_data>-wavwr = ls_lips-wavwr. """11/6/19
      IF <fs_data>-mprok NOT IN r_mprok.
        DELETE t_data.
        CONTINUE.
      ENDIF.
      IF <fs_data>-wavwr NOT IN r_wavwr.
        DELETE t_data.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR ls_kna1.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_data>-kunnr
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_data>-shipto_desc = ls_kna1-name1.
    ENDIF.
    CLEAR ls_kna1.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_data>-kunag
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_data>-soldto_desc = ls_kna1-name1.
    ENDIF.
**** End change 29.11.17++
**** Begin change 04.02.18++
    IF <fs_data>-matkl IS NOT INITIAL.
* Material group desc.
      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
        EXPORTING
          matkl      = <fs_data>-matkl
*         LANGU      = SY-LANGU
        IMPORTING
          matkl_desc = <fs_data>-wgbez
*         MATKL_DESC_L       =
        EXCEPTIONS
          wrong_code = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
*     Implement suitable error handling here
      ENDIF.
    ENDIF.
**** End change 04.02.18++
    "******* 11-6-19
***  IF  <fs_data>-MPROK is  not INITIAL .
    lv_domname = 'MPROK'.
    lv_domvalue = <fs_data>-mprok. """"t_data-MPROK.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        i_domname  = lv_domname
        i_domvalue = lv_domvalue
        langu      = lv_langu
*       SW_DEST    =
      IMPORTING
        e_ddtext   = lv_ddtext
      EXCEPTIONS
        not_exist  = 1
        OTHERS     = 2.
    IF sy-subrc = 0.
      <fs_data>-mprok_desc = lv_ddtext.
    ENDIF.
***   ENDIF.
*    MODIFY t_data INDEX sy_tabix.  " 05.12.17--
  ENDLOOP.
  DELETE t_data WHERE wavwr NOT IN  r_wavwr .  """11/6/19
  DELETE t_data WHERE mprok NOT IN  r_mprok .  """11/6/19
*--- Check Alert Information
  READ TABLE t_data INDEX 1.
  CHECK NOT sy-tfill  IS INITIAL .
  is_alert = 'X' .
ENDFUNCTION.
```