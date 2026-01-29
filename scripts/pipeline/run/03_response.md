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