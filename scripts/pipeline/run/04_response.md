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