# Exception Indicator: SD: Delivery Item - POD status - SW_10_01_DLV_POD_ITM

## General Overview

This Exception Indicator (EI) monitors delivery items in Sales and Distribution (SD) to identify cases where proof of delivery (POD) confirmation is missing for a configurable period (e.g. 30 days), supporting control over delivery and POD status. It provides visibility into deliveries that have not yet received POD confirmation within the monitoring window, across configurable time and organizational dimensions.

This EI serves as an essential control for logistics and revenue recognition by:
- Enabling detection of deliveries without POD confirmation beyond the threshold period, supporting follow-up and exception escalation
- Supporting identification of delivery and POD status patterns by sales organization, distribution channel, route, and customer for operational and audit review
- Providing visibility into planned and actual goods movement dates, delivery status, and billing status for prioritization and root-cause analysis
- Enabling analysis of delivery type, shipping point, and partner roles for accountability and process improvement
- Supporting accountability by sold-to, ship-to, and other partner roles for POD completion and revenue recognition timing

Monitoring POD confirmation missing helps organizations close delivery and billing cycles, enforce POD policies, and prioritize corrections during month-end close and logistics reviews. The EI is valuable for delivery exception management and audit of proof-of-delivery status.

The EI uses delivery and related data (e.g. LIKP, LIPS, VBAP) and compares reference dates (e.g. planned goods movement date) to the run date within a configurable lookback window.


## Problem Description

Failure to monitor deliveries for missing POD confirmation beyond a threshold period creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected deliveries without POD can delay revenue recognition and distort period-end reporting when confirmation is required for revenue cut-off
- Missing POD beyond the threshold may indicate incomplete logistics or billing processes affecting financial statements
- Concentrations of unconfirmed deliveries in specific periods or organizational units can signal systematic process or system issues
- Unidentified POD gaps may delay month-end close when discovered late during delivery or billing review
- Delivery and billing status inconsistencies can affect audit evidence and revenue recognition assertions

**Logistics and Control Risks**
- Deliveries without POD visibility may not receive follow-up, leading to aged open deliveries and customer or carrier disputes
- POD status by route, shipping point, or delivery type could indicate process gaps or carrier non-compliance
- Lack of monitoring by delivery status, picking status, and billing status limits operational correction and prioritization
- Unconfirmed deliveries in specific sales organizations or channels may reflect training or configuration issues
- Missing POD can lead to dispute exposure and incorrect revenue timing when not addressed in time

**Management Visibility and Decision-Making Risks**
- Absence of POD-confirmation monitoring delays awareness of delivery and revenue recognition risk
- Unidentified aged unconfirmed deliveries limit ability to enforce POD and billing policies
- Exceptions may require audit or compliance review but go unnoticed without targeted monitoring
- Inability to attribute missing POD to organization, route, or customer constrains accountability and corrective action

## Suggested Resolution

**Immediate Response**
- Review the delivery items flagged by the EI to confirm POD status and the business context (e.g. carrier pending, customer confirmation pending)
- Verify delivery and POD status in the system (e.g. VL03N, VL06) to confirm current state and follow-up actions
- Check planned goods movement date, delivery date, and billing status to determine whether exception is process-related or data-related
- Identify responsible sales organization, route, and ship-to party for escalation or correction

**System Assessment**
- Analyze the date window and reference date used for selection to ensure the monitoring period (e.g. 30 days) is appropriate
- Compare unconfirmed-delivery volume and count to prior periods and to total deliveries to assess materiality and trend
- Examine distribution by sales org, channel, route, and delivery type to find concentration or systematic issues
- Assess delivery status, picking status, and billing status patterns to distinguish process vs. data issues
- Validate reference date (e.g. planned goods movement date) and POD logic to ensure correct exception detection

**Corrective Actions**
- Obtain or post POD confirmation for deliveries where confirmation was delayed (e.g. carrier or customer confirmation, POD document)
- For deliveries where POD is not yet applicable, document business justification and ensure proper status and follow-up
- Enforce POD and delivery-completion policies: tighten carrier or customer confirmation timelines where appropriate
- Update monitoring parameters (e.g. lookback, reference date, organizational scope) to align with close and logistics calendar
- Establish recurring EI runs and route results to logistics and finance for continuous POD oversight and exception resolution


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
| 103 | VGTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
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

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**ARKTX** (Description):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**AUART** (Sales Document Type):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BACKDAYS** (Backdays):

Controls how many days to look back from today when no date range is supplied. The EI builds the monitoring window from today minus this number of days. Larger values widen the period for detecting deliveries missing POD; smaller values focus on recent activity.

**BLOCK** (Indicator: Document preselected for archiving):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**BLOCK Options:**
- **X**: Set/active.
- ** ** (space) or blank: Not set.

**BLOCK_DESC** (Billing block desc.):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP1_CODE** (Partner1 - Code):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP1_FUNCT** (Partner1 - Function):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP1_NAME** (Name):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP2_CODE** (Partner2 - Code):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP2_FUNCT** (Partner2 - Function):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP2_NAME** (Name):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP3_CODE** (Partner3 - Code):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP3_FUNCT** (Partner3 - Function):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BP3_NAME** (Name):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**BZIRK** (Sales district):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**CMGST** (Overall CreditStatus):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**CMGST Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**COSTA** (Confirmation status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**COSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**DATE_REF_FLD** (Date Ref Field):

Specifies which date field is used as the reference for the monitoring window and for duration calculation. When no date range is supplied, the EI uses this field together with BACKDAYS. Code default: WADAT (planned goods movement date).


**DATE_REF_FLD Options:**
- **WADAT**: Planned goods movement date (code default).
- **LFDAT**: Delivery date.
- **ERDAT**, **AEDAT**: Created on, changed on.
- **PODAT**: Proof of delivery date.
- Other date fields (type D) on delivery header/item may be supported.

**DUMMY** (Single-Character Flag):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**DUMMY Options:**
- **X**: Set/active.
- ** ** (space) or blank: Not set.

**DURATION** (Duration In Time Units):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**DURATION_D** (Duration In Days):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**DURATION_UNIT** (Duration Unit):

Unit in which duration is expressed (e.g. days). Used for duration calculation and for filtering by DURATION.


**DURATION_UNIT Options:**
- **D**: Days.

**ERDAT** (Created On):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**ERNAM** (Created By):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**ERZET** (Time):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**FAKSK** (Billing block):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**FKDAT** (Billing Date):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**FKIVK** (Totals status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**FKIVK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**FKIVP** (Interco. Bill.Status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**FKIVP Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**FKSTA** (Billing Status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**FKSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**FKSTK** (Billing status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**FKSTK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**HDALL** (On Hold):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**HDALL Options:**
- **X**: Set/active.
- ** ** (space) or blank: Not set.

**HDALS** (Pos. Hold):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**HDALS Options:**
- **X**: Set/active.
- ** ** (space) or blank: Not set.

**KDAUF** (Sales Order):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**KDGRP** (Customer group):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**KDPOS** (Sales order item):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**KLMENG** (Cumul.confirmed qty):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**KODAT** (Picking Date):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**KOQUA** (Pick confirmation):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**KOQUA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**KOQUK** (Pick confirmation):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**KOQUK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**KOSTA** (Picking status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**KOSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**KOSTK** (Overall pick.status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**KOSTK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**KUNAG** (Sold-to party):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**KUNNR** (Ship-to party):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**KWMENG** (Order Quantity):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LANG** (Language for texts):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LDDAT** (Loading Date):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LFART** (Delivery Type):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LFBNR** (Reference Document):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LFDAT** (Delivery Date):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LFGSA** (Overall deliv.status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**LFGSA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**LFIMG** (Delivery quantity):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LFPOS** (Reference Doc. Item):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LFSTA** (Delivery status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**LFSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**LGMNG** (Actual delivery qty):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**LVSTA** (WM activity status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**LVSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**LVSTK** (Overall WM status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**LVSTK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**MANAGE_IN_UTC** ('X' - Manage in UTC):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**MANAGE_IN_UTC Options:**
- **X**: Manage dates/times in UTC.
- ** ** (space) or blank: Use local time.

**MATKL** (Material Group):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**MATNR** (Material):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**MEINS** (Base Unit of Measure):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**MPROK** (Manual price):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**MPROK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**MPROK_DESC** (Short text):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**NETWR_LIPS** (Net Value):

Describes the currency or amount in which the value is expressed in the document (document currency, net value, or cost). When supplied as a selection range, restricts which lines are included.

**NETWR_VBAP** (Net value):

Describes the currency or amount in which the value is expressed in the document (document currency, net value, or cost). When supplied as a selection range, restricts which lines are included.

**PDSTA** (Proof of delivery status):

Proof-of-delivery status (item or header) or POD date. Restricts or labels which deliveries are included by POD status. The EI flags deliveries where POD confirmation is missing for the configured period.


**PDSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**PDSTK** (Proof of delivery status):

Proof-of-delivery status (item or header) or POD date. Restricts or labels which deliveries are included by POD status. The EI flags deliveries where POD confirmation is missing for the configured period.


**PDSTK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**PKSTA** (Packing status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**PKSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**PKSTK** (Packing status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**PKSTK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**PODAT** (Proof of delivery date):

Proof-of-delivery status (item or header) or POD date. Restricts or labels which deliveries are included by POD status. The EI flags deliveries where POD confirmation is missing for the configured period.

**POSNR** (Item):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**PSTYV** (Item category):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**ROUTE** (Route):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**SHIPTO_DESC** (Name):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**SOLDTO_DESC** (Name):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**SPART** (Division):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**SPE_TMPID** (Temp Inb.):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**SPE_TMPID Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**TDDAT** (Transptn Plang Date):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**TRSTA** (Trns.plan.status):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**TRSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**UVK01 - UVK05** (Header reserves 1 – Header reserves 5):

Reserve/status indicators for header (UVK01–UVK05). Used to filter deliveries by the corresponding status dimension. All five share the same type and domain.

**UVK01 - UVK05 Options:**
- Values are status-specific (domain STATV); see domain or code for possible values.

**UVP01 - UVP05** (Item reserves 1 – Item reserves 5):

Reserve/status indicators for item (UVP01–UVP05). Used to filter deliveries by the corresponding status dimension. All five share the same type and domain.

**UVP01 - UVP05 Options:**
- Values are status-specific (domain STATV); see domain or code for possible values.

**UVPAS** (It.data packaging):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**UVPIS** (It.data picking/putaway):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**UVS01 - UVS05** (Total reserves 1 – Total reserves 5):

Reserve/status indicators for total (UVS01–UVS05). Used to filter deliveries by the corresponding status dimension. All five share the same type and domain.

**UVS01 - UVS05 Options:**
- Values are status-specific (domain STATV); see domain or code for possible values.

**VBELN** (Delivery):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**VBTYP** (SD document categ.):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**VBTYP Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**VESTK** (HU placed in stock):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**VESTK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**VGBEL** (Reference Document):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**VGPOS** (Reference Item):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**VGTYP** (SD document categ.):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**VGTYP Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**VKBUR** (Sales Office):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**VKORG** (Sales Organization):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**VLSTK** (Status Decent. Whse):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**VLSTK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**VRKME** (Sales Unit):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**VSTEL** (Shipping Point/Receiving Pt):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**VTWEG** (Distribution Channel):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**WADAT** (Pland Gds Mvmnt Date):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**WADAT_IST** (Act. Gds Mvmnt Date):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**WAERK** (Document Currency):

Describes the currency or amount in which the value is expressed in the document (document currency, net value, or cost). When supplied as a selection range, restricts which lines are included.

**WAVWR** (Cost):

Describes the currency or amount in which the value is expressed in the document (document currency, net value, or cost). When supplied as a selection range, restricts which lines are included.

**WBSTA** (Goods movement stat.):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**WBSTA Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**WBSTK** (Total gds mvt stat.):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


**WBSTK Options:**
- Values are status-specific (domain STATV or equivalent); see domain or code for possible values (e.g. not started, in progress, completed).

**WGBEZ** (Material Group Desc.):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.

**XBLNR** (Reference):

Restricts or labels which delivery items are included by this criterion. Used for selection or display in the result set.


### Parameter Relationships

**Time and Duration Parameters:**
- **BACKDAYS** defines the lookback window when no date range is supplied; the EI uses this to build the selection range for the reference date (e.g. planned goods movement date, delivery date).
- **DATE_REF_FLD** specifies which date field is used as the reference for the monitoring window and for duration calculation; it works with BACKDAYS to define the effective date range (e.g. WADAT for planned goods movement date).
- **DURATION** and **DURATION_UNIT** work together: DURATION holds the numeric value and DURATION_UNIT the unit (e.g. days). The EI uses them to compute and filter by age of the delivery relative to the reference date and to populate the output duration field. **DURATION_D** can represent duration in days when applicable.

**Organizational and Selection Parameters:**
- **VKORG**, **VTWEG**, **SPART**, **BZIRK**, **KDGRP**, **VSTEL**, **VKBUR** define sales organization, distribution channel, division, sales district, customer group, shipping point, and sales office; they restrict which deliveries are selected and appear together in the result.
- **LFART**, **VBTYP** define delivery type and SD document category; they narrow the type of deliveries included.
- **ROUTE** restricts selection by route; it works with organizational parameters to scope the result set.

**Partner Parameters:**
- **BP1_FUNCT**, **BP1_CODE**, **BP2_FUNCT**, **BP2_CODE**, **BP3_FUNCT**, **BP3_CODE** define up to three partner function and code pairs; they restrict or label results by sold-to, ship-to, or other partner roles. Use together for accountability by partner.

**Delivery and POD Status Parameters:**
- **PDSTA**, **PDSTK**, **PODAT** relate to proof-of-delivery status (item and header) and POD date; they restrict or label which deliveries are included by POD status. The EI flags deliveries where POD confirmation is missing for the configured period; these parameters support filtering by POD state.
- **WADAT**, **WADAT_IST**, **LFDAT**, **PODAT** define planned goods movement date, actual goods movement date, delivery date, and POD date; they work with DATE_REF_FLD and BACKDAYS to define the monitoring window and duration.

**Reserve and Status Parameters:**
- **UVK01–UVK05**, **UVP01–UVP05**, **UVS01–UVS05** (header, item, and total reserves 1–5) work together to restrict or label results by reserve status; they can be used for status-level analysis of delivery and POD.

**Language and Descriptions:**
- **LANG** (or LANGU when applicable) drives the language for description texts; it affects only output text, not which rows are selected.


### Default Values

- **BACKDAYS** — Default: `1` (when no date range is supplied, the EI uses a lookback of one day from the reference date).
- **DATE_REF_FLD** — Default: `WADAT` (planned goods movement date); used as the reference date for the monitoring window and for duration calculation.
- **DURATION_UNIT** — Default: `D` (days); used for duration calculation and filtering.
- **LANG** — Default: system language (e.g. SY-LANGU); used for description texts.

**Note:** Parameters that are not supplied and have no explicit default are used when initial (empty or 0); e.g. empty date ranges lead to the default window above.

### Practical Configuration Examples

**Use Case 1: POD missing for 30 days (planned goods movement date)**
```
BACKDAYS = 30
DATE_REF_FLD = WADAT
DURATION_UNIT = D
```
**Purpose:** Identify delivery items where proof of delivery confirmation is missing for 30 days from the planned goods movement date for follow-up and revenue recognition.

**Use Case 2: POD missing by sales organization and route**
```
VKORG = 1000, 2000
ROUTE = R001, R002
BACKDAYS = 14
```
**Purpose:** Focus on unconfirmed POD in selected sales organizations and routes over the past two weeks for logistics and carrier follow-up.

**Use Case 3: By delivery type and shipping point**
```
LFART = NLCC, NLFF
VSTEL = 1000, 2000
BACKDAYS = 7
```
**Purpose:** Monitor POD confirmation missing for specific delivery types and shipping points for operational and process review.

**Use Case 4: By partner (ship-to) and POD status**
```
BP1_FUNCT = WE
BP1_CODE = (ship-to range)
BACKDAYS = 1
```
**Purpose:** Review deliveries missing POD for specific ship-to partners for customer or carrier escalation.


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