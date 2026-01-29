### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 117 parameters listed in the Parameters Reference Table above.


**AEDAT** (Changed On):

Filters delivery and delivery-item data by the "changed on" date (LIKP/LIPS header and line). The EI applies this to the date range derived from BACKDAYS and DATE_REF_FLD when DATE_REF_FLD is set to AEDAT. Use to focus on recently changed deliveries.

**ARKTX** (Description):

Material description (LIPS-ARKTX). Use to filter deliveries by item description text. Populates the output when in the structure; supports range or single value.

**AUART** (Sales Document Type):

Sales document type from the reference order (VBAK-AUART via LIPS-VGBEL/VGPOS). Filters deliveries by the type of the originating sales order. Use to restrict to specific order types (e.g. standard orders, returns).

**BACKDAYS** (Backdays):

Number of days to look back from today. When no date range is supplied, the EI builds the selection using SY-DATUM minus BACKDAYS. Used to limit the delivery date window.

**BACKDAYS and DATE_REF_FLD Connection:**

BACKDAYS defines how many days back from today the window starts. DATE_REF_FLD selects which date field (e.g. WADAT, LFDAT, ERDAT) is used for that window. Together they determine which deliveries fall into the selection. Set BACKDAYS first, then choose DATE_REF_FLD to match the business date you care about (e.g. planned goods movement vs delivery date).

**BLOCK** (Indicator: Document preselected for archiving):

Indicator from VBUK-BLOCK. Filters deliveries by archiving preselection. Use to include or exclude documents marked for archiving.

**BLOCK_DESC** (Billing block desc.):

Text description for the billing block (FAKSK), resolved via function for display. Populated by the EI from FAKSK; use FAKSK to filter by billing block.

**BP1_CODE** (Partner1 - Code):

Partner number for the first partner role. Filters deliveries by this partner (from VBPA). BP1_NAME is populated from customer master (KNA1). Use with BP1_FUNCT to define the role (e.g. sold-to, ship-to).

**BP1_FUNCT** (Partner1 - Function):

Partner function for the first business partner (PARVW). Used with BP1_CODE to filter and enrich partner data from VBPA.

**BP1_FUNCT Options:**

- **AG**: Sold-to party  
- **WE**: Ship-to party  
- **RE**: Bill-to party  
- **SP**: Contact person  
(Other PARVW values apply; use the value that matches the role you want to filter by.)

**BP1_NAME** (Name):

Name of the first partner (from KNA1-NAME1). Populated by the EI from customer master based on BP1_CODE. Use for display; filter by BP1_CODE.

**BP2_CODE** (Partner2 - Code):

Partner code for the second partner role. Filters and enriches output; works with BP2_FUNCT and BP2_NAME.

**BP2_FUNCT** (Partner2 - Function):

Partner function for the second business partner. Works with BP2_CODE and BP2_NAME.

**BP2_NAME** (Name):

Name of the second partner. Populated from customer master. Filter by BP2_CODE.

**BP3_CODE** (Partner3 - Code):

Partner code for the third partner role. Filters and enriches output.

**BP3_FUNCT** (Partner3 - Function):

Partner function for the third business partner. Works with BP3_CODE and BP3_NAME.

**BP3_NAME** (Name):

Name of the third partner. Populated from customer master. Filter by BP3_CODE.

**BZIRK** (Sales district):

Sales district (LIKP-BZIRK). Filters deliveries by sales district. Supports range selection.

**CMGST** (Overall CreditStatus):

Overall credit status (VBUK-CMGST). Filters deliveries by credit check status. Use to focus on blocked or released deliveries.

**COSTA** (Confirmation status):

Confirmation status at item level. Filters by confirmation status. Supports range or single value.

**DATE_REF_FLD** (Date Ref Field):

Field name used as the reference date for the selection window and for duration calculation. The EI maps this to the corresponding delivery/header date and uses it with BACKDAYS to build the date range and with DURATION/DURATION_UNIT to compute how long the delivery has been in the current status.

**DATE_REF_FLD Options:**

- **ERDAT**: Created on  
- **AEDAT**: Changed on  
- **LDDAT**: Loading date  
- **TDDAT**: Transportation planning date  
- **KODAT**: Picking date  
- **WADAT**: Planned goods movement date (default in code)  
- **WADAT_IST**: Actual goods movement date  
- **LFDAT**: Delivery date  
- **FKDAT**: Billing date  

**DUMMY** (Single-Character Flag):

Single-character flag; technical or placeholder. Use as required by the front end or selection interface.

**DURATION** (Duration In Time Units):

Duration in the selected time unit (see DURATION_UNIT) between the reference date (DATE_REF_FLD) and current date. The EI calculates this per delivery and filters by the supplied range to focus on deliveries that have been in the current status for a given length of time.

**DURATION and DURATION_UNIT Connection:**

DURATION gives the numeric value (e.g. 7); DURATION_UNIT gives the unit (e.g. D = days). Together they define the "age in status" filter. DATE_REF_FLD determines which date is used as the start for the calculation. Set all three when filtering by how long deliveries have been in status.

**DURATION_D** (Duration In Days):

Duration in days (calculated/output). Use to filter or display age in days when the unit is days.

**DURATION_UNIT** (Duration Unit):

Unit for duration calculation. Used with DATE_REF_FLD and DURATION to filter deliveries by how long they have been in the current status.

**DURATION_UNIT Options:**

- **D**: Days (default in code)

**ERDAT** (Created On):

Creation date of the delivery (LIKP-ERDAT). Filters by creation date. Used in the date range when DATE_REF_FLD = ERDAT.

**ERNAM** (Created By):

User who created the delivery (LIKP-ERNAM). Filters by creator. Supports range or single value.

**ERZET** (Time):

Creation time. Filters by time when time-based selection is used.

**FAKSK** (Billing block):

Billing block (LIKP-FAKSK). Filters deliveries by billing block. BLOCK_DESC provides the text for the code.

**FKDAT** (Billing Date):

Billing date (LIKP-FKDAT). Filters by billing date. Used in the date range when DATE_REF_FLD = FKDAT.

**FKIVK** (Totals status):

Totals status (VBUK-FKIVK). Filters by billing totals status.

**FKIVP** (Interco. Bill.Status):

Intercompany billing status (VBUK-FKIVP). Filters by intercompany billing status.

**FKSTA** (Billing Status):

Billing status at item level (VBUP-FKSTA). Filters by line-level billing status.

**FKSTK** (Billing status):

Overall billing status (VBUK-FKSTK). Filters by header billing status.

**HDALL** (On Hold):

Header-level hold indicator. Filters deliveries that are on hold.

**HDALS** (Pos. Hold):

Item-level hold indicator. Filters by position hold.

**KDAUF** (Sales Order):

Sales order number (LIKP/LIPS reference). Filters deliveries by the originating sales order. Supports range selection.

**KDGRP** (Customer group):

Customer group (LIKP-KDGRP). Filters by customer group. Supports range selection.

**KDPOS** (Sales order item):

Sales order item number. Filters by order item when analyzing at item level.

**KLMENG** (Cumul.confirmed qty):

Cumulative confirmed quantity from the sales order (VBAP-KLMENG). Output/display; use for value-based analysis.

**KODAT** (Picking Date):

Picking date (LIKP-KODAT). Filters by picking date. Used in the date range when DATE_REF_FLD = KODAT.

**KOQUA** (Pick confirmation):

Pick confirmation status at item level. Filters by pick confirmation.

**KOQUK** (Pick confirmation):

Pick confirmation (alternate). Filters by pick confirmation status.

**KOSTA** (Picking status):

Picking status at item level (VBUP-KOSTA). Filters by picking status.

**KOSTK** (Overall pick.status):

Overall picking status (VBUK-KOSTK). Filters by header picking status.

**KUNAG** (Sold-to party):

Sold-to party (LIKP-KUNAG). Filters deliveries by sold-to customer. SOLDTO_DESC is populated from KNA1.

**KUNNR** (Ship-to party):

Ship-to party (LIKP-KUNNR, from LIKP-KUNWE in code). Filters deliveries by ship-to customer. SHIPTO_DESC is populated from KNA1.

**KWMENG** (Order Quantity):

Order quantity from the sales order. Output/display; use for quantity-based analysis.

**LANG** (Language for texts):

Language for descriptions (e.g. BLOCK_DESC, MPROK_DESC). Drives text resolution.

**LDDAT** (Loading Date):

Loading date (LIKP-LDDAT). Filters by loading date. Used when DATE_REF_FLD = LDDAT.

**LFART** (Delivery Type):

Delivery type (LIKP-LFART). Filters by delivery type. Supports range selection.

**LFBNR** (Reference Document):

Reference document number. Filters by reference document.

**LFDAT** (Delivery Date):

Delivery date (LIKP-LFDAT). Filters by delivery date. Used when DATE_REF_FLD = LFDAT.

**LFGSA** (Overall deliv.status):

Overall delivery status (VBUK-VBUP). Filters by overall delivery status.

**LFIMG** (Delivery quantity):

Delivery quantity (LIPS-LFIMG). Filters or displays delivered quantity.

**LFPOS** (Reference Doc. Item):

Reference document item. Filters by reference item.

**LFSTA** (Delivery status):

Delivery status at item level (VBUP-LFSTA). Filters by line delivery status.

**LGMNG** (Actual delivery qty):

Actual delivered quantity in stockkeeping units (LIPS-LGMNG). Output; used for quantity analysis.

**LVSTA** (WM activity status):

Warehouse management activity status. Filters by WM status.

**LVSTK** (Overall WM status):

Overall WM status. Filters by warehouse management status.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set (e.g. 'X'), date/time handling uses UTC. Use when working across time zones.

**MATKL** (Material Group):

Material group (LIPS-MATKL). Filters by material group. WGBEZ provides the group description.

**MATNR** (Material):

Material number (LIPS-MATNR). Filters deliveries by material. Supports range selection.

**MEINS** (Base Unit of Measure):

Base unit of measure (LIPS-MEINS). Filters or displays UoM.

**MPROK** (Manual price):

Manual price indicator (VBAP-MPROK). Filters by manual pricing. MPROK_DESC provides the short text.

**MPROK_DESC** (Short text):

Short text for manual price (MPROK). Populated by the EI from domain MPROK; use for display.

**NETWR_LIPS** (Net Value):

Net value at delivery item level (derived from LIPS/VBAP). Output for value analysis.

**NETWR_VBAP** (Net value):

Net value from the sales order (VBAP). Output for value analysis.

**PDSTA** (Proof of delivery status):

Proof of delivery status at item level. Filters by POD status.

**PDSTK** (Proof of delivery status):

Overall proof of delivery status. Filters by POD status.

**PKSTA** (Packing status):

Packing status at item level. Filters by packing status.

**PKSTK** (Packing status):

Overall packing status. Filters by packing status.

**PODAT** (Proof of delivery date):

Proof of delivery date. Filters by POD date.

**POSNR** (Item):

Delivery item number (LIPS-POSNR). Filters by item position.

**PSTYV** (Item category):

Item category (LIPS-PSTYV). Filters by item category.

**ROUTE** (Route):

Route (LIKP-ROUTE). Filters by route. Supports range selection.

**SHIPTO_DESC** (Name):

Ship-to party name (KNA1-NAME1 for KUNNR). Populated by the EI from customer master. Use for display; filter by KUNNR.

**SOLDTO_DESC** (Name):

Sold-to party name (KNA1-NAME1 for KUNAG). Populated by the EI from customer master. Use for display; filter by KUNAG.

**SPART** (Division):

Division (LIPS-SPART). Filters by division. Supports range selection.

**SPE_TMPID** (Temp Inb.):

Temporary inbound indicator. Filters by temporary inbound flag.

**TDDAT** (Transptn Plang Date):

Transportation planning date (LIKP-TDDAT). Filters by transport planning date. Used when DATE_REF_FLD = TDDAT.

**TRSTA** (Trns.plan.status):

Transportation planning status. Filters by transport planning status.

**UVK01 - UVK05** (Header reserves 1 - Header reserves 5):

Reserve/status indicators at header level (VBUK). Used to filter deliveries by the corresponding status dimension. All five share the same type and domain; use the index that matches the dimension you need.

**UVP01 - UVP05** (Item reserves 1 - Item reserves 5):

Reserve/status indicators at item level (VBUP). Used to filter deliveries by the corresponding status dimension. All five share the same type and domain.

**UVPAS** (It.data packaging):

Item data packaging status. Filters by packaging data status.

**UVPIS** (It.data picking/putaway):

Item data picking/putaway status. Filters by picking/putaway status.

**UVS01 - UVS05** (Total reserves 1 - Total reserves 5):

Reserve/status indicators for totals. Used to filter deliveries by the corresponding status dimension. All five share the same type and domain.

**VBELN** (Delivery):

Delivery number (LIKP-VBELN). Filters by delivery document. Supports range selection.

**VBTYP** (SD document categ.):

SD document category (LIKP-VBTYP). Filters by document category.

**VESTK** (HU placed in stock):

Handling unit placed in stock indicator. Filters by HU placement status.

**VGBEL** (Reference Document):

Reference document (e.g. sales order) from LIPS. Filters by reference document.

**VGPOS** (Reference Item):

Reference item number. Filters by reference item.

**VGTYP** (SD document categ. (prev)):

SD document category of the reference document. Filters by reference document category.

**VKBUR** (Sales Office):

Sales office (LIKP-VKBUR). Filters by sales office. Supports range selection.

**VKORG** (Sales Organization):

Sales organization (LIKP-VKORG). Filters by sales organization. Supports range selection.

**VLSTK** (Status Decent. Whse):

Status decentral warehouse. Filters by warehouse status.

**VRKME** (Sales Unit):

Sales unit of measure (LIPS-VRKME). Filters or displays sales UoM.

**VSTEL** (Shipping Point/Receiving Pt):

Shipping point (LIKP-VSTEL). Filters by shipping point. Supports range selection.

**VTWEG** (Distribution Channel):

Distribution channel (LIPS-VTWEG). Filters by distribution channel. Supports range selection.

**WADAT** (Pland Gds Mvmnt Date):

Planned goods movement date (LIKP-WADAT). Filters by planned goods movement date. Default reference date in code when DATE_REF_FLD = WADAT.

**WADAT_IST** (Act. Gds Mvmnt Date):

Actual goods movement date (LIKP-WADAT_IST). Filters by actual goods movement date. Used when DATE_REF_FLD = WADAT_IST.

**WAERK** (Document Currency):

Document currency of the sales order (VBAP-WAERK). Represents the document/transaction currency in which order and delivery values are held. Use for display or currency-based filtering; do not document technical source (e.g. table/join).

**WAVWR** (Cost):

Cost (VBAP-WAVWR). Filters or displays cost. Use for value-based analysis.

**WBSTA** (Goods movement stat.):

Goods movement status at item level. Filters by goods movement status.

**WBSTK** (Total gds mvt stat.):

Total goods movement status. Filters by overall goods movement status.

**WGBEZ** (Material Group Desc.):

Material group description (MATKL text). Populated by the EI for display. Filter by MATKL.

**XBLNR** (Reference):

Reference (LIKP-XBLNR). Filters or displays customer reference.
