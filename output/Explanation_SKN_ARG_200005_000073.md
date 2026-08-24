# Exception Indicator: Sales order items and header ( SW_10_01_AR_000073)

## General Overview

This Exception Indicator returns sales order header and item data for configurable monitoring and analysis, joining order header and line fields and supporting optional currency conversion, date-window selection, and age-based filtering on a chosen reference date.

This EI serves as an essential control for sales operations and financial oversight by:

- Enabling detection of sales order lines that match configured value, quantity, organizational, and material criteria
- Supporting analysis of order header and item attributes together in one result set for exception review
- Providing optional conversion of amounts into a target currency for comparable value thresholds
- Enabling age-based prioritization when order lines remain in scope after a chosen reference date
- Supporting segmentation by sales organization, distribution channel, customer, plant, and material for targeted review

Typical use includes high-value order sampling, organizational exception lists, and periodic monitoring of order lines before billing or fulfillment milestones. Results are intended for exception workflows and analytical review rather than operational SD list reporting.

The routine reads sales order header and item data through a joined extract, applies optional currency conversion and description enrichment, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor sales order header and item data against configured business criteria creates multiple risks across revenue control, fulfillment, and operational governance:

**Sales and Revenue Risks**

- High-value or unusual order lines can proceed without structured review when not surfaced by exception monitoring
- Value thresholds applied only in document currency can miss exposure when orders use foreign currencies without conversion
- Undetected patterns by sales organization, customer, or material can concentrate revenue or fulfillment risk

**Operational Risks**

- Date-window settings misaligned with review cadence can exclude recent orders or retain rows outside the intended monitoring period
- Header and item filters that are too broad or too narrow can hide actionable lines or overload reviewers
- Age-based filters that are not tuned can mix stale and current cases in the same queue

**Control and Audit Risks**

- Weak monitoring reduces evidence that configured order populations were reviewed before close or escalation
- Lack of recurring exception review limits accountability for buyers and sales operations over out-of-policy order lines
- Missing organizational segmentation weakens targeted control over critical customers, plants, or document types

## Suggested Resolution

**Immediate Response**

- Review flagged sales orders for header context, item values, quantities, customer, and organizational attributes
- Confirm whether each line represents an authorized business case or requires correction before further processing
- Prioritize high-value, critical-customer, or long-open lines for sales operations follow-up

**System Assessment**

- Validate lookback, forward-day, and reference-date settings against the team's review cadence
- Tune value, material, and organizational scope so results stay actionable
- Compare exception counts by sales organization, document type, and customer to find systematic gaps

**Corrective Actions**

- Correct order data through standard SD processes where review confirms errors
- Adjust monitoring thresholds and scope after cleanup so results reflect truly exceptional cases
- Document review outcomes and schedule recurring runs for critical sales areas or customers


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ABGRU | Reason for rejection | CHAR | 2 | 0 | ABGRU_VA | ABGRU_VA |
| 2 | ARKTX | Description | CHAR | 40 | 0 | ARKTX | TEXT40 |
| 3 | AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 4 | BACKDAYS | Back Days | INT4 | 10 | 0 | /SKN/E_MN_AN_BACKDAYS | /SKN/D_MN_AN_BACKDAYS |
| 5 | BRGEW | Gross weight | QUAN | 15 | 3 | BRGEW_AP | MENG15 |
| 6 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 7 | CEPOK | Expected price | CHAR | 1 | 0 | CEPOK | CEPOK |
| 8 | DATE_REF_FLD | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 9 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 11 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 12 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 13 | EXC_RATE_TYPE | Exchange Rate Type | CHAR | 4 | 0 | KURST_CURR | KURST |
| 14 | FAKSP | Billing block | CHAR | 2 | 0 | FAKSP_AP | FAKSP |
| 15 | FAKSP_DESC | Billing block desc. | CHAR | 20 | 0 | BEZEI_FAKSP | TEXT20 |
| 16 | FKREL | Relevant for Billing | CHAR | 1 | 0 | FKREL | FKREL |
| 17 | FORWDAYS | Forth Days | INT4 | 10 | 0 | /SKN/E_MN_AN_FORWDAYS | /SKN/D_MN_AN_FORWDAYS |
| 18 | GEWEI | Weight unit | UNIT | 3 | 0 | GEWEI | MEINS |
| 19 | KBMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KBMENG | MENG15 |
| 20 | KDMAT | Customer Material | CHAR | 35 | 0 | MATNR_KU | IDNEX |
| 21 | KLMENG | Cumul.confirmed qty | QUAN | 15 | 3 | KLMENG | MENG15 |
| 22 | KMEIN | Unit of measure | UNIT | 3 | 0 | KMEIN | MEINS |
| 23 | KONDM | Material pricing grp | CHAR | 2 | 0 | KONDM | KONDM |
| 24 | KPEIN | Pricing unit | DEC | 5 | 0 | KPEIN | KPEIN |
| 25 | KTGRM | Acct assignment grp | CHAR | 2 | 0 | KTGRM | KTGRM |
| 26 | KWMENG | Order Quantity | QUAN | 15 | 3 | KWMENG | MENG15 |
| 27 | LANGU | Language Key | LANG | 1 | 0 | LANGU | SPRAS |
| 28 | LFREL | Itm relev.for deliv. | CHAR | 1 | 0 | LFREL_AP | XFELD |
| 29 | LGORT | Storage Location | CHAR | 4 | 0 | LGORT_D | LGORT |
| 30 | LSMENG | Required deliv. qty | QUAN | 15 | 3 | LSMENG | MENG15 |
| 31 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 32 | MATKL_DESC | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |
| 33 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 34 | MATNR_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 35 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 36 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 37 | MVGR1 | Material group 1 | CHAR | 3 | 0 | MVGR1 | MVGR1 |
| 38 | MVGR2 | Material group 2 | CHAR | 3 | 0 | MVGR2 | MVGR2 |
| 39 | MVGR3 | Material group 3 | CHAR | 3 | 0 | MVGR3 | MVGR3 |
| 40 | MVGR4 | Material group 4 | CHAR | 3 | 0 | MVGR4 | MVGR4 |
| 41 | MVGR5 | Material group 5 | CHAR | 3 | 0 | MVGR5 | MVGR5 |
| 42 | NETPR | Net price | CURR | 11 | 2 | NETPR | WERTV6 |
| 43 | NETPR_FOREIGN | Net price | CURR | 11 | 2 | NETPR | WERTV6 |
| 44 | NETWR | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 45 | NETWR_FOREIGN | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 46 | NTGEW | Net weight | QUAN | 15 | 3 | NTGEW_AP | MENG15 |
| 47 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 48 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 49 | PRCTR_DESC | Name | CHAR | 20 | 0 | KTEXT | TEXT20 |
| 50 | PRODH | Product hierarchy | CHAR | 18 | 0 | PRODH_D | PRODH |
| 51 | PS_PSP_PNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 52 | PS_PSP_PNR_DESC | Description | CHAR | 40 | 0 | PS_POST1 | TEXT40 |
| 53 | PSTYV | Item category | CHAR | 4 | 0 | PSTYV | PSTYV |
| 54 | ROUTE | Route | CHAR | 6 | 0 | ROUTE | ROUTE |
| 55 | SHKZG | Returns | CHAR | 1 | 0 | SHKZG_VA | XFELD |
| 56 | SOBKZ | Special Stock | CHAR | 1 | 0 | SOBKZ | SOBKZ |
| 57 | TARGET_CUKY | Target Curr. Key | CUKY | 5 | 0 | /SKN/E_MN_AN_TARGET_CURR | WAERS |
| 58 | UEPOS | Higher-level item | NUMC | 6 | 0 | UEPOS | POSNR |
| 59 | UMVKN | Denominator | DEC | 5 | 0 | UMVKN | UMBSN |
| 60 | UMVKZ | Numerator | DEC | 5 | 0 | UMVKZ | UMBSZ |
| 61 | VBAK_ABRVW | Usage | CHAR | 3 | 0 | ABRVW | ABRVW |
| 62 | VBAK_AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 63 | VBAK_AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 64 | VBAK_AUART_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 65 | VBAK_AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 66 | VBAK_AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 67 | VBAK_AUGRU | Order reason | CHAR | 3 | 0 | AUGRU | AUGRU |
| 68 | VBAK_AUGRU_DESC | Description | CHAR | 40 | 0 | BEZEI40 | TEXT40 |
| 69 | VBAK_BNAME | Name | CHAR | 35 | 0 | BNAME_V | NAME |
| 70 | VBAK_BSARK | Purchase order type | CHAR | 4 | 0 | BSARK | BSARK |
| 71 | VBAK_BSTDK | Purchase order date | DATS | 8 | 0 | BSTDK | DATUM |
| 72 | VBAK_BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 73 | VBAK_BUKRS_VF | CCode to be billed | CHAR | 4 | 0 | BUKRS_VF | BUKRS |
| 74 | VBAK_BUKRS_VF_DESC | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 75 | VBAK_ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 76 | VBAK_ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 77 | VBAK_ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 78 | VBAK_GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 79 | VBAK_KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 80 | VBAK_KOSTL | Cost Center | CHAR | 10 | 0 | KOSTL | KOSTL |
| 81 | VBAK_KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 82 | VBAK_KUNNR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 83 | VBAK_KVGR1 | Customer group 1 | CHAR | 3 | 0 | KVGR1 | KVGR1 |
| 84 | VBAK_KVGR2 | Customer group 2 | CHAR | 3 | 0 | KVGR2 | KVGR2 |
| 85 | VBAK_LIFSK | Delivery block | CHAR | 2 | 0 | LIFSK | LIFSP |
| 86 | VBAK_MANDT | Client | CLNT | 3 | 0 | MANDT | MANDT |
| 87 | VBAK_PS_PSP_PNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 88 | VBAK_PS_PSP_PNR_DESC | Description | CHAR | 40 | 0 | PS_POST1 | TEXT40 |
| 89 | VBAK_TELF1 | Telephone | CHAR | 16 | 0 | TELF1_VP | TELF1 |
| 90 | VBAK_VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 91 | VBAK_VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 92 | VBAK_VBTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 93 | VBAK_VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 94 | VBAK_VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 95 | VBAK_VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 96 | VBAK_VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 97 | VBAK_VKORG_DESC | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 98 | VBAK_VSBED | Shipping Conditions | CHAR | 2 | 0 | VSBED | VSBED |
| 99 | VBAK_VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 100 | VBAK_VTWEG_DESC | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 101 | VBAK_WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 102 | VBAK_XBLNR | Reference | CHAR | 16 | 0 | XBLNR_V1 | XBLNR1 |
| 103 | VBAK_ZUONR | Assignment | CHAR | 18 | 0 | ORDNR_V | ZUONR |
| 104 | VBAP_AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 105 | VGBEL | Reference document | CHAR | 10 | 0 | VGBEL | VBELN |
| 106 | VGPOS | Reference item | NUMC | 6 | 0 | VGPOS | POSNR |
| 107 | VGTYP | Preceding doc.categ. | CHAR | 1 | 0 | VBTYP_V | VBTYP |
| 108 | VGTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 109 | VOLEH | Volume unit | UNIT | 3 | 0 | VOLEH | MEINS |
| 110 | VOLUM | Volume | QUAN | 15 | 3 | VOLUM_AP | MENG15 |
| 111 | VPMAT | Planning material | CHAR | 18 | 0 | VPMAT | MATNR |
| 112 | VPMAT_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 113 | VPWRK | Planning plant | CHAR | 4 | 0 | VPWRK | WERKS |
| 114 | VPWRK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 115 | VRKME | Sales unit | UNIT | 3 | 0 | VRKME | MEINS |
| 116 | VSTEL | Shipping Point/Receiving Pt | CHAR | 4 | 0 | VSTEL | VSTEL |
| 117 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 118 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |
| 119 | WAVWR_FOREIGN | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |
| 120 | WERKS | Plant | CHAR | 4 | 0 | WERKS_EXT | WERKS |
| 121 | WERKS_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 121 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ABGRU** (Reason for rejection)

Reason for Rejection stores the code that explains why a sales document item was canceled or not processed further.

**ARKTX** (Description)

Short text for a manufacturing order component or BOM line (material description at order-component level).

**AUFNR** (Order)

Order number key for internal orders or manufacturing orders-primary CO/PP order identifier in many extracts.

**BACKDAYS** (Back Days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BRGEW** (Gross weight)

Gross weight of the logistics quantity used with GEWEI for shipping and freight calculations.

**BWTAR** (Valuation Type)

Valuation type key used in split valuation scenarios (batch/material valuation layers).

**CEPOK** (Expected price)

Ensures reporting respects expected price constraints carried by CEPOK.

**DATE_REF_FLD** (Field name)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- VBAK_ERDAT — Created on.
- VBAK_AUDAT — Document Date.
- VBAK_VDATU — Requested deliv.date.
- VBAK_BSTDK — Purchase order date.
- VBAK_AEDAT — Changed on.
- VBAP_AEDAT — Changed on.

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

**EXC_RATE_TYPE** (Exchange Rate Type)

Interprets exchange rate type as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on EXC_RATE_TYPE.

**FAKSP** (Billing block)

When left open per framework rules, FAKSP does not restrict billing block; when set, only matching rows remain.

**FAKSP_DESC** (Billing block desc.)

When populated, keeps the extract focused so billing block desc. (FAKSP_DESC) aligns with the intended triage slice.

**FKREL** (Relevant for Billing)

When tightened, relevant for billing (FKREL) removes rows that would otherwise dilute attention from failing or stuck cases.

**FORWDAYS** (Forth Days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

Forwdays is based on DATE_REF_FLD field.

**GEWEI** (Weight unit)

Unit of measure for weight fields such as BRGEW and NTGEW in logistics quantity conversions.

**KBMENG** (Cumul.confirmed qty)

Works downstream of the initial read so cumul.confirmed qty on KBMENG still participates in row-level deletion rules.

**KDMAT** (Customer Material)

Documents expected operator behavior—customer material on KDMAT should be set when that dimension is part of the control objective.

**KLMENG** (Cumul.confirmed qty)

Cumulative schedule or order quantity in the sales item context-confirmed or requested quantity accumulated on schedules.

**KMEIN** (Unit of measure)

Documents expected operator behavior—unit of measure on KMEIN should be set when that dimension is part of the control objective.

**KONDM** (Material pricing grp)

Aligns exception volume with the chosen scope by testing material pricing grp via KONDM before alert evaluation.

**KPEIN** (Pricing unit)

Uses pricing unit from the source context so only records with KPEIN inside declared values proceed.

**KTGRM** (Acct assignment grp)

Connects to alert semantics: rows removed for failing acct assignment grp on KTGRM never reach downstream filtering.

**KWMENG** (Order Quantity)

Cumulative order quantity in sales units on the item-commercial ordered quantity for SD lines.

**LANGU** (Language Key)

Language key used for language-dependent texts and user-language filtering.

**LFREL** (Itm relev.for deliv.)

Works downstream of the initial read so itm relev.for deliv. on LFREL still participates in row-level deletion rules.

**LGORT** (Storage Location)

Storage location used to segment stock/logistics movements by warehouse sub-location.

**LSMENG** (Required deliv. qty)

Ensures reporting respects required deliv. qty constraints carried by LSMENG.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

**MATKL_DESC** (Material Group Desc.)

Documents expected operator behavior—material group desc. on MATKL_DESC should be set when that dimension is part of the control objective.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MATNR_DESC** (Material Description)

Supports escalation where material description on MATNR_DESC signals ownership for follow-up between Basis and functional teams.

**MEINS** (Base Unit of Measure)

Base unit of measure used to interpret quantity fields consistently.

**MPROK** (Manual price)

Material/procurement status key used to identify control-relevant status states.

**MVGR1 - MVGR5** (Material group 1)

Aligns exception volume with the chosen scope by testing material group 1 via MVGR1 before alert evaluation.

**NETPR** (Net price)

Net Price is primarily used at the item level in purchasing documents (such as Purchase Orders, Scheduling Agreements, and Info Records) to denote the price per unit of material.

**NETPR_FOREIGN** (Net price)

For operations, net price on NETPR_FOREIGN indicates whether a row belongs in the current monitoring pass versus historical noise.

**NETWR** (Net value)

Net value amount used for commercial threshold and anomaly checks.

**NETWR_FOREIGN** (Net value)

For distributed landscapes, net value on NETWR_FOREIGN often anchors which application server or destination appears in results.

**NTGEW** (Net weight)

Net weight of the shipped or ordered quantity paired with GEWEI for logistics weight checks.

**POSNR** (Sales Document Item)

Document item number used for line-level drilldown and joins.

**PRCTR** (Profit Center)

Profit center used for management accounting segmentation and profitability reporting.

**PRCTR_DESC** (Name)

Profit center description.

**PRODH** (Product hierarchy)

Product Hierarchy stores the alpha-numeric code that structures materials into different levels for sales analysis and pricing.

**PS_PSP_PNR** (WBS Element)

WBS element key used for project-system linked cost/procurement monitoring.

**PS_PSP_PNR_DESC** (Description)

For operations, description on PS_PSP_PNR_DESC indicates whether a row belongs in the current monitoring pass versus historical noise.

**PSTYV** (Item category)

Sales document item category controlling item behavior, pricing relevance, and delivery rules.

**ROUTE** (Route)

Shipping route code on deliveries grouping legs and carriers for transportation planning analytics.

**SHKZG** (Returns)

Debit/Credit indicator used to separate accounting posting direction.

**SHKZG Options:**
- S: Debit posting
- H: Credit posting

**SOBKZ** (Special Stock)

Special stock indicator used to distinguish stock ownership categories.

**TARGET_CUKY** (Target Curr. Key)

Field used in currency conversion to specify the target currency key (e.g., USD) into which an amount should be translated.

**UEPOS** (Higher-level item)

Captures edge cases where higher-level item (UEPOS) must be non-default to reproduce a customer-specific monitoring scenario.

**UMVKN** (Denominator)

For distributed landscapes, denominator on UMVKN often anchors which application server or destination appears in results.

**UMVKZ** (Numerator)

When harmonized with related filters, numerator on UMVKZ isolates the highest-risk record families.

**VBAK_ABRVW** (Usage)

For distributed landscapes, usage on VBAK_ABRVW often anchors which application server or destination appears in results.

**VBAK_AEDAT** (Changed on)

When combined with destination discipline, changed on on VBAK_AEDAT keeps both breadth and depth of the extract intentional.

**VBAK_AUART** (Sales Document Type)

Works downstream of the initial read so sales document type on VBAK_AUART still participates in row-level deletion rules.

**VBAK_AUART_DESC** (Description)

Narrows retrieved rows where description (VBAK_AUART_DESC) must match the configured selection for this monitor.

**VBAK_AUDAT** (Document Date)

When left open per framework rules, VBAK_AUDAT does not restrict document date; when set, only matching rows remain.

**VBAK_AUFNR** (Order)

Interprets order as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on VBAK_AUFNR.

**VBAK_AUGRU** (Order reason)

After data is read, lines are removed unless order reason on VBAK_AUGRU still satisfies the active multivalued selection.

**VBAK_AUGRU_DESC** (Description)

When populated, keeps the extract focused so description (VBAK_AUGRU_DESC) aligns with the intended triage slice.

**VBAK_BNAME** (Name)

Mirrors how administrators slice operational lists: name (VBAK_BNAME) is one lever that shapes which rows are comparable run over run.

**VBAK_BSARK** (Purchase order type)

Guards against oversized extracts when purchase order type on VBAK_BSARK is narrowed together with client, user, or session filters.

**VBAK_BSTDK** (Purchase order date)

Treats purchase order date as a discriminator between similar rows that would otherwise look identical in a raw extract.

**VBAK_BSTNK** (Purchase order no.)

Reduces false positives during peak windows by tightening purchase order no. through VBAK_BSTNK alongside state filters.

**VBAK_BUKRS_VF** (CCode to be billed)

Stabilizes week-over-week metrics by fixing ccode to be billed (VBAK_BUKRS_VF) while allowing duration thresholds to move.

**VBAK_BUKRS_VF_DESC** (Company Name)

When left open per framework rules, VBAK_BUKRS_VF_DESC does not restrict company name; when set, only matching rows remain.

**VBAK_ERDAT** (Created on)

For distributed landscapes, created on on VBAK_ERDAT often anchors which application server or destination appears in results.

**VBAK_ERNAM** (Created by)

Works downstream of the initial read so created by on VBAK_ERNAM still participates in row-level deletion rules.

**VBAK_ERZET** (Time)

Gives auditors traceable criteria because time on VBAK_ERZET is applied consistently before any alert flag is raised.

**VBAK_GSBER** (Business Area)

Reflects real administration where business area on VBAK_GSBER is routinely restricted to a single productive client or object family.

**VBAK_KOKRS** (Controlling Area)

Stabilizes week-over-week metrics by fixing controlling area (VBAK_KOKRS) while allowing duration thresholds to move.

**VBAK_KOSTL** (Cost Center)

Aligns exception volume with the chosen scope by testing cost center via VBAK_KOSTL before alert evaluation.

**VBAK_KUNNR** (Sold-to party)

Works downstream of the initial read so sold-to party on VBAK_KUNNR still participates in row-level deletion rules.

**VBAK_KUNNR_DESC** (Name)

Treats name as a discriminator between similar rows that would otherwise look identical in a raw extract.

**VBAK_KVGR1 - VBAK_KVGR2** (Customer group 1)

Helps monitoring stay readable by requiring customer group 1 (VBAK_KVGR1) to match organizational or technical selectors when set.

**VBAK_LIFSK** (Delivery block)

Interprets delivery block as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on VBAK_LIFSK.

**VBAK_MANDT** (Client)

Ensures reporting respects client constraints carried by VBAK_MANDT.

**VBAK_PS_PSP_PNR** (WBS Element)

Ensures reporting respects wbs element constraints carried by VBAK_PS_PSP_PNR.

**VBAK_PS_PSP_PNR_DESC** (Description)

Ensures reporting respects description constraints carried by VBAK_PS_PSP_PNR_DESC.

**VBAK_TELF1** (Telephone)

Gives auditors traceable criteria because telephone on VBAK_TELF1 is applied consistently before any alert flag is raised.

**VBAK_VBELN** (Sales Document)

Supports escalation where sales document on VBAK_VBELN signals ownership for follow-up between Basis and functional teams.

**VBAK_VBTYP** (SD document categ.)

Valuable when comparing health before and after a release—hold sd document categ. on VBAK_VBTYP constant while varying other filters.

**VBAK_VBTYP_DESC** (Short Descript.)

When populated, keeps the extract focused so short descript. (VBAK_VBTYP_DESC) aligns with the intended triage slice.

**VBAK_VDATU** (Requested deliv.date)

Combines with related filters so requested deliv.date on VBAK_VDATU refines which records remain for duration or state checks.

**VBAK_VKBUR** (Sales Office)

Aligns exception volume with the chosen scope by testing sales office via VBAK_VKBUR before alert evaluation.

**VBAK_VKGRP** (Sales Group)

Treats sales group as a discriminator between similar rows that would otherwise look identical in a raw extract.

**VBAK_VKORG** (Sales Organization)

Reflects real administration where sales organization on VBAK_VKORG is routinely restricted to a single productive client or object family.

**VBAK_VKORG_DESC** (Name)

When tightened, name (VBAK_VKORG_DESC) removes rows that would otherwise dilute attention from failing or stuck cases.

**VBAK_VSBED** (Shipping Conditions)

Interprets shipping conditions as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on VBAK_VSBED.

**VBAK_VTWEG** (Distribution Channel)

Stabilizes week-over-week metrics by fixing distribution channel (VBAK_VTWEG) while allowing duration thresholds to move.

**VBAK_VTWEG_DESC** (Name)

Ensures reporting respects name constraints carried by VBAK_VTWEG_DESC.

**VBAK_WAERK** (Document Currency)

Ensures reporting respects document currency constraints carried by VBAK_WAERK.

**VBAK_XBLNR** (Reference)

Helps distinguish technical versus business attributes when reference on VBAK_XBLNR correlates with counters or status fields.

**VBAK_ZUONR** (Assignment)

When populated, keeps the extract focused so assignment (VBAK_ZUONR) aligns with the intended triage slice.

**VBAP_AEDAT** (Changed on)

Aligns exception volume with the chosen scope by testing changed on via VBAP_AEDAT before alert evaluation.

**VGBEL** (Reference document)

Preceding SD document number in document flow linking subsequent items to originating sales or delivery.

**VGPOS** (Reference item)

Preceding item number paired with VGBEL for precise document-chain joins on SD logistics data.

**VGTYP** (Preceding doc.categ.)

Preceding document category qualifying VGBEL semantics across orders, deliveries, and billing types.

**VGTYP_DESC** (Short Descript.)

Helps distinguish technical versus business attributes when short descript. on VGTYP_DESC correlates with counters or status fields.

**VOLEH** (Volume unit)

Volume unit of measure paired with VOLUM for freight and packing volume calculations.

**VOLUM** (Volume)

Volume quantity of the logistics object used with VOLEH for capacity and load-planning checks.

**VPMAT** (Planning material)

Helps distinguish technical versus business attributes when planning material on VPMAT correlates with counters or status fields.

**VPMAT_DESC** (Material Description)

For operations, material description on VPMAT_DESC indicates whether a row belongs in the current monitoring pass versus historical noise.

**VPWRK** (Planning plant)

Ensures reporting respects planning plant constraints carried by VPWRK.

**VPWRK_DESC** (Name 1)

For distributed landscapes, name 1 on VPWRK_DESC often anchors which application server or destination appears in results.

**VRKME** (Sales unit)

Sales unit of measure for the material in SD documents-unit for commercial sales quantities.

**VSTEL** (Shipping Point/Receiving Pt)

Shipping point or receiving point controlling loading, transportation planning, and route determination.

**WAERK** (Document Currency)

Document currency key used for value analysis in transaction currency.

**WAVWR** (Cost)

Statistical value amount field used for value-based exception thresholds.

**WAVWR_FOREIGN** (Cost)

Prevents accidental global scans when cost (WAVWR_FOREIGN) is meant to stay within a controlled application slice.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WERKS_DESC** (Name 1)

Plant name or description providing readable site context beside plant keys.


### Parameter Relationships

**Reference-date window:** When **DATUM** is empty, a date range from today minus **BACKDAYS** through today is built; when **FORWDAYS** is also set, the upper bound extends forward by that many days. The window is copied to the date field selected by **DATE_REF_FLD** (default item created-on date). Explicit date selections on the chosen reference field override that fallback window.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference date to the evaluation date; rows outside the configured duration range are removed.

**Currency conversion:** **TARGET_CUKY**, **EXC_RATE_TYPE**, and optional currency conversion date settings work together to translate foreign-currency amount fields such as **NETWR_FOREIGN** and **NETPR_FOREIGN** into the target currency for threshold filtering.

**Header and item scope:** **VBAK_VBELN**, **POSNR**, **VBAK_VKORG**, **VBAK_VTWEG**, **VBAK_AUART**, **VBAK_KUNNR**, **MATNR**, **WERKS**, and related header and item parameters combine to define which sales order lines enter the result set.

**Value and quantity thresholds:** **NETWR_FOREIGN**, **KWMENG**, **LSMENG**, **KBMENG**, **KLMENG**, **BRGEW**, **NTGEW**, and **VOLUM** support filtering on converted net value and logistics quantities after the extract is built.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - ERDAT
- **EXC_RATE_TYPE** - M
- **LANGU** - EN

### Practical Example of Parameter Configuration

**Use Case 1: High net value lines in the last thirty days**

**Purpose:** Review sales order items with net value above a threshold created in the last thirty days.

```
BACKDAYS = 30
DATE_REF_FLD = ERDAT
NETWR_FOREIGN = 100000 - 999999999
VBAK_VKORG = 1000
```

**Use Case 2: Forward-looking order window**

**Purpose:** Include sales order lines from the last ten days through the next five days for near-term fulfillment review.

```
BACKDAYS = 10
FORWDAYS = 5
DATE_REF_FLD = VBAK_VDATU
VBAK_VTWEG = 10
```

**Use Case 3: Material-specific monitoring**

**Purpose:** Monitor order lines for one material in a selected plant.

```
MATNR = 100000000000000001
WERKS = 1000
BACKDAYS = 60
NETWR_FOREIGN = 50000 - 999999999
```

**Use Case 4: Customer and sales organization focus**

**Purpose:** Sample high-value lines for one customer within one sales organization.

```
VBAK_KUNNR = 100000
VBAK_VKORG = 1000
BACKDAYS = 45
TARGET_CUKY = USD
```

**Use Case 5: Exactly seven full days since reference date**

**Purpose:** Return rows whose reference date is exactly 7 full days ago for weekly escalation.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
DATE_REF_FLD = ERDAT
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/ARG_200005_000073 | ABGRU | Reason for rejection | CHAR(2) | ABGRU_VA |
| /SKN/ARG_200005_000073 | ARKTX | Description | CHAR(40) | ARKTX |
| /SKN/ARG_200005_000073 | AUFNR | Order | CHAR(12) | AUFNR |
| /SKN/ARG_200005_000073 | BACKDAYS | Back Days | INT4(10) | /SKN/E_MN_AN_BACKDAYS |
| /SKN/ARG_200005_000073 | BRGEW | Gross weight | QUAN(15) | BRGEW_AP |
| /SKN/ARG_200005_000073 | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/ARG_200005_000073 | CEPOK | Expected price | CHAR(1) | CEPOK |
| /SKN/ARG_200005_000073 | DATE_REF_FLD | Field name | CHAR(30) | NAME_FELD |
| /SKN/ARG_200005_000073 | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/ARG_200005_000073 | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/ARG_200005_000073 | ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/ARG_200005_000073 | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/ARG_200005_000073 | EXC_RATE_TYPE | Exchange Rate Type | CHAR(4) | KURST_CURR |
| /SKN/ARG_200005_000073 | FAKSP | Billing block | CHAR(2) | FAKSP_AP |
| /SKN/ARG_200005_000073 | FAKSP_DESC | Billing block desc. | CHAR(20) | BEZEI_FAKSP |
| /SKN/ARG_200005_000073 | FKREL | Relevant for Billing | CHAR(1) | FKREL |
| /SKN/ARG_200005_000073 | FORWDAYS | Forth Days | INT4(10) | /SKN/E_MN_AN_FORWDAYS |
| /SKN/ARG_200005_000073 | GEWEI | Weight unit | UNIT(3) | GEWEI |
| /SKN/ARG_200005_000073 | KBMENG | Cumul.confirmed qty | QUAN(15) | KBMENG |
| /SKN/ARG_200005_000073 | KDMAT | Customer Material | CHAR(35) | MATNR_KU |
| /SKN/ARG_200005_000073 | KLMENG | Cumul.confirmed qty | QUAN(15) | KLMENG |
| /SKN/ARG_200005_000073 | KMEIN | Unit of measure | UNIT(3) | KMEIN |
| /SKN/ARG_200005_000073 | KONDM | Material pricing grp | CHAR(2) | KONDM |
| /SKN/ARG_200005_000073 | KPEIN | Pricing unit | DEC(5) | KPEIN |
| /SKN/ARG_200005_000073 | KTGRM | Acct assignment grp | CHAR(2) | KTGRM |
| /SKN/ARG_200005_000073 | KWMENG | Order Quantity | QUAN(15) | KWMENG |
| /SKN/ARG_200005_000073 | LANGU | Language Key | LANG(1) | LANGU |
| /SKN/ARG_200005_000073 | LFREL | Itm relev.for deliv. | CHAR(1) | LFREL_AP |
| /SKN/ARG_200005_000073 | LGORT | Storage Location | CHAR(4) | LGORT_D |
| /SKN/ARG_200005_000073 | LSMENG | Required deliv. qty | QUAN(15) | LSMENG |
| /SKN/ARG_200005_000073 | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/ARG_200005_000073 | MATKL_DESC | Material Group Desc. | CHAR(20) | WGBEZ |
| /SKN/ARG_200005_000073 | MATNR | Material | CHAR(18) | MATNR |
| /SKN/ARG_200005_000073 | MATNR_DESC | Material Description | CHAR(40) | MAKTX |
| /SKN/ARG_200005_000073 | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/ARG_200005_000073 | MPROK | Manual price | CHAR(1) | MPROK |
| /SKN/ARG_200005_000073 | MVGR1 | Material group 1 | CHAR(3) | MVGR1 |
| /SKN/ARG_200005_000073 | MVGR2 | Material group 2 | CHAR(3) | MVGR2 |
| /SKN/ARG_200005_000073 | MVGR3 | Material group 3 | CHAR(3) | MVGR3 |
| /SKN/ARG_200005_000073 | MVGR4 | Material group 4 | CHAR(3) | MVGR4 |
| /SKN/ARG_200005_000073 | MVGR5 | Material group 5 | CHAR(3) | MVGR5 |
| /SKN/ARG_200005_000073 | NETPR | Net price | CURR(11) | NETPR |
| /SKN/ARG_200005_000073 | NETPR_FOREIGN | Net price | CURR(11) | NETPR |
| /SKN/ARG_200005_000073 | NETWR | Net value | CURR(15) | NETWR_AP |
| /SKN/ARG_200005_000073 | NETWR_FOREIGN | Net value | CURR(15) | NETWR_AP |
| /SKN/ARG_200005_000073 | NTGEW | Net weight | QUAN(15) | NTGEW_AP |
| /SKN/ARG_200005_000073 | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/ARG_200005_000073 | PRCTR | Profit Center | CHAR(10) | PRCTR |
| /SKN/ARG_200005_000073 | PRCTR_DESC | Name | CHAR(20) | KTEXT |
| /SKN/ARG_200005_000073 | PRODH | Product hierarchy | CHAR(18) | PRODH_D |
| /SKN/ARG_200005_000073 | PSTYV | Item category | CHAR(4) | PSTYV |
| /SKN/ARG_200005_000073 | PS_PSP_PNR | WBS Element | NUMC(8) | PS_PSP_PNR |
| /SKN/ARG_200005_000073 | PS_PSP_PNR_DESC | Description | CHAR(40) | PS_POST1 |
| /SKN/ARG_200005_000073 | ROUTE | Route | CHAR(6) | ROUTE |
| /SKN/ARG_200005_000073 | SHKZG | Returns | CHAR(1) | SHKZG_VA |
| /SKN/ARG_200005_000073 | SOBKZ | Special Stock | CHAR(1) | SOBKZ |
| /SKN/ARG_200005_000073 | TARGET_CUKY | Target Curr. Key | CUKY(5) | /SKN/E_MN_AN_TARGET_CURR |
| /SKN/ARG_200005_000073 | UEPOS | Higher-level item | NUMC(6) | UEPOS |
| /SKN/ARG_200005_000073 | UMVKN | Denominator | DEC(5) | UMVKN |
| /SKN/ARG_200005_000073 | UMVKZ | Numerator | DEC(5) | UMVKZ |
| /SKN/ARG_200005_000073 | VBAK_ABRVW | Usage | CHAR(3) | ABRVW |
| /SKN/ARG_200005_000073 | VBAK_AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/ARG_200005_000073 | VBAK_AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/ARG_200005_000073 | VBAK_AUART_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/ARG_200005_000073 | VBAK_AUDAT | Document Date | DATS(8) | AUDAT |
| /SKN/ARG_200005_000073 | VBAK_AUFNR | Order | CHAR(12) | AUFNR |
| /SKN/ARG_200005_000073 | VBAK_AUGRU | Order reason | CHAR(3) | AUGRU |
| /SKN/ARG_200005_000073 | VBAK_AUGRU_DESC | Description | CHAR(40) | BEZEI40 |
| /SKN/ARG_200005_000073 | VBAK_BNAME | Name | CHAR(35) | BNAME_V |
| /SKN/ARG_200005_000073 | VBAK_BSARK | Purchase order type | CHAR(4) | BSARK |
| /SKN/ARG_200005_000073 | VBAK_BSTDK | Purchase order date | DATS(8) | BSTDK |
| /SKN/ARG_200005_000073 | VBAK_BSTNK | Purchase order no. | CHAR(20) | BSTNK |
| /SKN/ARG_200005_000073 | VBAK_BUKRS_VF | CCode to be billed | CHAR(4) | BUKRS_VF |
| /SKN/ARG_200005_000073 | VBAK_BUKRS_VF_DESC | Company Name | CHAR(25) | BUTXT |
| /SKN/ARG_200005_000073 | VBAK_ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/ARG_200005_000073 | VBAK_ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/ARG_200005_000073 | VBAK_ERZET | Time | TIMS(6) | ERZET |
| /SKN/ARG_200005_000073 | VBAK_GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/ARG_200005_000073 | VBAK_KOKRS | Controlling Area | CHAR(4) | KOKRS |
| /SKN/ARG_200005_000073 | VBAK_KOSTL | Cost Center | CHAR(10) | KOSTL |
| /SKN/ARG_200005_000073 | VBAK_KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/ARG_200005_000073 | VBAK_KUNNR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/ARG_200005_000073 | VBAK_KVGR1 | Customer group 1 | CHAR(3) | KVGR1 |
| /SKN/ARG_200005_000073 | VBAK_KVGR2 | Customer group 2 | CHAR(3) | KVGR2 |
| /SKN/ARG_200005_000073 | VBAK_LIFSK | Delivery block | CHAR(2) | LIFSK |
| /SKN/ARG_200005_000073 | VBAK_MANDT | Client | CLNT(3) | MANDT |
| /SKN/ARG_200005_000073 | VBAK_PS_PSP_PNR | WBS Element | NUMC(8) | PS_PSP_PNR |
| /SKN/ARG_200005_000073 | VBAK_PS_PSP_PNR_DESC | Description | CHAR(40) | PS_POST1 |
| /SKN/ARG_200005_000073 | VBAK_TELF1 | Telephone | CHAR(16) | TELF1_VP |
| /SKN/ARG_200005_000073 | VBAK_VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/ARG_200005_000073 | VBAK_VBTYP | SD document categ. | CHAR(1) | VBTYP |
| /SKN/ARG_200005_000073 | VBAK_VBTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/ARG_200005_000073 | VBAK_VDATU | Requested deliv.date | DATS(8) | EDATU_VBAK |
| /SKN/ARG_200005_000073 | VBAK_VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/ARG_200005_000073 | VBAK_VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/ARG_200005_000073 | VBAK_VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/ARG_200005_000073 | VBAK_VKORG_DESC | Name | CHAR(20) | VTXTK |
| /SKN/ARG_200005_000073 | VBAK_VSBED | Shipping Conditions | CHAR(2) | VSBED |
| /SKN/ARG_200005_000073 | VBAK_VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/ARG_200005_000073 | VBAK_VTWEG_DESC | Name | CHAR(20) | VTXTK |
| /SKN/ARG_200005_000073 | VBAK_WAERK | Document Currency | CUKY(5) | WAERK |
| /SKN/ARG_200005_000073 | VBAK_XBLNR | Reference | CHAR(16) | XBLNR_V1 |
| /SKN/ARG_200005_000073 | VBAK_ZUONR | Assignment | CHAR(18) | ORDNR_V |
| /SKN/ARG_200005_000073 | VBAP_AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/ARG_200005_000073 | VGBEL | Reference document | CHAR(10) | VGBEL |
| /SKN/ARG_200005_000073 | VGPOS | Reference item | NUMC(6) | VGPOS |
| /SKN/ARG_200005_000073 | VGTYP | Preceding doc.categ. | CHAR(1) | VBTYP_V |
| /SKN/ARG_200005_000073 | VGTYP_DESC | Short Descript. | CHAR(60) | VAL_TEXT |
| /SKN/ARG_200005_000073 | VOLEH | Volume unit | UNIT(3) | VOLEH |
| /SKN/ARG_200005_000073 | VOLUM | Volume | QUAN(15) | VOLUM_AP |
| /SKN/ARG_200005_000073 | VPMAT | Planning material | CHAR(18) | VPMAT |
| /SKN/ARG_200005_000073 | VPMAT_DESC | Material Description | CHAR(40) | MAKTX |
| /SKN/ARG_200005_000073 | VPWRK | Planning plant | CHAR(4) | VPWRK |
| /SKN/ARG_200005_000073 | VPWRK_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/ARG_200005_000073 | VRKME | Sales unit | UNIT(3) | VRKME |
| /SKN/ARG_200005_000073 | VSTEL | Shipping Point/Receiving Pt | CHAR(4) | VSTEL |
| /SKN/ARG_200005_000073 | WAERK | Document Currency | CUKY(5) | WAERK |
| /SKN/ARG_200005_000073 | WAVWR | Cost | CURR(13) | WAVWR |
| /SKN/ARG_200005_000073 | WAVWR_FOREIGN | Cost | CURR(13) | WAVWR |
| /SKN/ARG_200005_000073 | WERKS | Plant | CHAR(4) | WERKS_EXT |
| /SKN/ARG_200005_000073 | WERKS_DESC | Name 1 | CHAR(30) | NAME1 |

## ABAP Code

```abap
FUNCTION /SKN/ARG_200005_000065.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/ARG_200005_000073
*"----------------------------------------------------------------------
##NO_HANDLER
##NEEDED
DATA_MULTY: DATUM DATUM.
##NO_HANDLER
##NEEDED
DATA_MULTY: NETWR_FOREIGN NETWR_AP.
##NO_HANDLER
##NEEDED
DATA_MULTY: KWMENG KWMENG.
##NO_HANDLER
##NEEDED
DATA_MULTY: LSMENG LSMENG.
##NO_HANDLER
##NEEDED
DATA_MULTY: KBMENG KBMENG.
##NO_HANDLER
##NEEDED
DATA_MULTY: KLMENG KLMENG.
##NO_HANDLER
##NEEDED
DATA_MULTY: BRGEW BRGEW_AP.
##NO_HANDLER
##NEEDED
DATA_MULTY: NTGEW NTGEW_AP.
##NO_HANDLER
##NEEDED
DATA_MULTY: VOLUM VOLUM_AP.
##NO_HANDLER
##NEEDED
DATA_MULTY: ERDAT ERDAT.
##NO_HANDLER
##NEEDED
DATA_MULTY: NETPR_FOREIGN NETPR.
##NO_HANDLER
##NEEDED
DATA_MULTY: WAVWR_FOREIGN WAVWR.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_MANDT MANDT.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VBELN VBELN_VA.
##NO_HANDLER
##NEEDED
DATA_MULTY: POSNR POSNR_VA.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_ERDAT ERDAT.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_ERZET ERZET.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_ERNAM ERNAM.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_AUDAT AUDAT.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VBTYP VBTYP.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_AUART AUART.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_AUGRU AUGRU.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_LIFSK LIFSK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_WAERK WAERK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VKORG VKORG.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VTWEG VTWEG.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VKGRP VKGRP.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VKBUR VKBUR.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_GSBER GSBER.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VDATU EDATU_VBAK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VSBED VSBED.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_BSTNK BSTNK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_BSARK BSARK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_BSTDK BSTDK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_BNAME BNAME_V.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_TELF1 TELF1_VP.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_KUNNR KUNAG.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_KOSTL KOSTL.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_AEDAT AEDAT.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_KVGR1 KVGR1.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_KVGR2 KVGR2.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_KOKRS KOKRS.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_PS_PSP_PNR PS_PSP_PNR.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_ABRVW ABRVW.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_BUKRS_VF BUKRS_VF.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_XBLNR XBLNR_V1.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_ZUONR ORDNR_V.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_AUFNR AUFNR.
##NO_HANDLER
##NEEDED
DATA_MULTY: MATNR MATNR.
##NO_HANDLER
##NEEDED
DATA_MULTY: MATKL MATKL.
##NO_HANDLER
##NEEDED
DATA_MULTY: ARKTX ARKTX.
##NO_HANDLER
##NEEDED
DATA_MULTY: PSTYV PSTYV.
##NO_HANDLER
##NEEDED
DATA_MULTY: LFREL LFREL_AP.
##NO_HANDLER
##NEEDED
DATA_MULTY: FKREL FKREL.
##NO_HANDLER
##NEEDED
DATA_MULTY: UEPOS UEPOS.
##NO_HANDLER
##NEEDED
DATA_MULTY: ABGRU ABGRU_VA.
##NO_HANDLER
##NEEDED
DATA_MULTY: PRODH PRODH_D.
##NO_HANDLER
##NEEDED
DATA_MULTY: MEINS MEINS.
##NO_HANDLER
##NEEDED
DATA_MULTY: FAKSP FAKSP_AP.
##NO_HANDLER
##NEEDED
DATA_MULTY: NETWR NETWR_AP.
##NO_HANDLER
##NEEDED
DATA_MULTY: WAERK WAERK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VRKME VRKME.
##NO_HANDLER
##NEEDED
DATA_MULTY: UMVKZ UMVKZ.
##NO_HANDLER
##NEEDED
DATA_MULTY: UMVKN UMVKN.
##NO_HANDLER
##NEEDED
DATA_MULTY: GEWEI GEWEI.
##NO_HANDLER
##NEEDED
DATA_MULTY: VOLEH VOLEH.
##NO_HANDLER
##NEEDED
DATA_MULTY: VGBEL VGBEL.
##NO_HANDLER
##NEEDED
DATA_MULTY: VGPOS VGPOS.
##NO_HANDLER
##NEEDED
DATA_MULTY: WERKS WERKS_EXT.
##NO_HANDLER
##NEEDED
DATA_MULTY: LGORT LGORT_D.
##NO_HANDLER
##NEEDED
DATA_MULTY: VSTEL VSTEL.
##NO_HANDLER
##NEEDED
DATA_MULTY: ROUTE ROUTE.
##NO_HANDLER
##NEEDED
DATA_MULTY: ERNAM ERNAM.
##NO_HANDLER
##NEEDED
DATA_MULTY: NETPR NETPR.
##NO_HANDLER
##NEEDED
DATA_MULTY: KPEIN KPEIN.
##NO_HANDLER
##NEEDED
DATA_MULTY: KMEIN KMEIN.
##NO_HANDLER
##NEEDED
DATA_MULTY: SHKZG SHKZG_VA.
##NO_HANDLER
##NEEDED
DATA_MULTY: KONDM KONDM.
##NO_HANDLER
##NEEDED
DATA_MULTY: KTGRM KTGRM.
##NO_HANDLER
##NEEDED
DATA_MULTY: BWTAR BWTAR_D.
##NO_HANDLER
##NEEDED
DATA_MULTY: WAVWR WAVWR.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAP_AEDAT AEDAT.
##NO_HANDLER
##NEEDED
DATA_MULTY: PRCTR PRCTR.
##NO_HANDLER
##NEEDED
DATA_MULTY: MVGR1 MVGR1.
##NO_HANDLER
##NEEDED
DATA_MULTY: MVGR2 MVGR2.
##NO_HANDLER
##NEEDED
DATA_MULTY: MVGR3 MVGR3.
##NO_HANDLER
##NEEDED
DATA_MULTY: MVGR4 MVGR4.
##NO_HANDLER
##NEEDED
DATA_MULTY: MVGR5 MVGR5.
##NO_HANDLER
##NEEDED
DATA_MULTY: SOBKZ SOBKZ.
##NO_HANDLER
##NEEDED
DATA_MULTY: PS_PSP_PNR PS_PSP_PNR.
##NO_HANDLER
##NEEDED
DATA_MULTY: AUFNR AUFNR.
##NO_HANDLER
##NEEDED
DATA_MULTY: VPMAT VPMAT.
##NO_HANDLER
##NEEDED
DATA_MULTY: VPWRK VPWRK.
##NO_HANDLER
##NEEDED
DATA_MULTY: CEPOK CEPOK.
##NO_HANDLER
##NEEDED
DATA_MULTY: KDMAT MATNR_KU.
##NO_HANDLER
##NEEDED
DATA_MULTY: MPROK MPROK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VGTYP VBTYP_V.
##NO_HANDLER
##NEEDED
DATA_MULTY: FAKSP_DESC BEZEI_FAKSP.
##NO_HANDLER
##NEEDED
DATA_MULTY: MATKL_DESC WGBEZ.
##NO_HANDLER
##NEEDED
DATA_MULTY: MATNR_DESC MAKTX.
##NO_HANDLER
##NEEDED
DATA_MULTY: PRCTR_DESC KTEXT.
##NO_HANDLER
##NEEDED
DATA_MULTY: PS_PSP_PNR_DESC PS_POST1.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_AUART_DESC BEZEI20.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_AUGRU_DESC BEZEI40.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_BUKRS_VF_DESC BUTXT.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_KUNNR_DESC NAME1_GP.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_PS_PSP_PNR_DESC PS_POST1.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VBTYP_DESC VAL_TEXT.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VKORG_DESC VTXTK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VBAK_VTWEG_DESC VTXTK.
##NO_HANDLER
##NEEDED
DATA_MULTY: VGTYP_DESC VAL_TEXT.
##NO_HANDLER
##NEEDED
DATA_MULTY: VPMAT_DESC MAKTX.
##NO_HANDLER
##NEEDED
DATA_MULTY: VPWRK_DESC NAME1.
##NO_HANDLER
##NEEDED
DATA_MULTY: WERKS_DESC NAME1.
##NO_HANDLER
##NEEDED
DATA_SINGLE: TARGET_CUKY /SKN/E_MN_AN_TARGET_CURR.
##NO_HANDLER
##NEEDED
DATA_SINGLE: BACKDAYS /SKN/E_MN_AN_BACKDAYS.
LV_BACKDAYS = '10'.
##NO_HANDLER
##NEEDED
DATA_SINGLE: DATE_REF_FLD NAME_FELD.
LV_DATE_REF_FLD = 'ERDAT'.
##NO_HANDLER
##NEEDED
DATA_SINGLE: CURRENCY_CONV_DATE /SKN/E_MN_AN_CUR_CONV_DATE_FLD.
##NO_HANDLER
##NEEDED
DATA_MULTY: DURATION /SKN/E_SW_DURATION.
##NO_HANDLER
##NEEDED
DATA_SINGLE: DURATION_UNIT /SKN/E_SW_DURATION_UNIT.
LV_DURATION_UNIT = 'D'.
##NO_HANDLER
##NEEDED
DATA_SINGLE: EXC_RATE_TYPE KURST_CURR.
LV_EXC_RATE_TYPE = 'M'.
##NO_HANDLER
##NEEDED
DATA_SINGLE: FORWDAYS /SKN/E_MN_AN_FORWDAYS.
##NO_HANDLER
##NEEDED
DATA_SINGLE: LANGU LANGU.
LV_LANGU = 'EN'.
##NO_HANDLER
##NEEDED
DATA_SINGLE: TIME_REF_FLD NAME_FELD.
##NEEDED
DATA SY_DATLO LIKE SY-DATLO.
##NEEDED
DATA SY_TIMLO LIKE SY-TIMLO.
##NEEDED
DATA DATE_FROM LIKE SY-DATUM.
##NEEDED
DATA DATE_TO LIKE SY-DATUM.
##NEEDED
DATA LV_TAB TYPE DDOBJNAME.
##NEEDED
DATA LV_STRUC TYPE DDOBJNAME.
##NEEDED
DATA LS_LIST TYPE /SKN/S_TABLES.
##NEEDED
DATA LT_DATA_TMP LIKE T_DATA[].
##NO_HANDLER
##NEEDED
SELECT_MULTY: NETWR_FOREIGN.
##NO_HANDLER
##NEEDED
SELECT_MULTY: KWMENG.
##NO_HANDLER
##NEEDED
SELECT_MULTY: LSMENG.
##NO_HANDLER
##NEEDED
SELECT_MULTY: KBMENG.
##NO_HANDLER
##NEEDED
SELECT_MULTY: KLMENG.
##NO_HANDLER
##NEEDED
SELECT_MULTY: BRGEW.
##NO_HANDLER
##NEEDED
SELECT_MULTY: NTGEW.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VOLUM.
##NO_HANDLER
##NEEDED
SELECT_MULTY: ERDAT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: NETPR_FOREIGN.
##NO_HANDLER
##NEEDED
SELECT_MULTY: WAVWR_FOREIGN.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_MANDT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VBELN.
CONVERT_MULTY: VBAK_VBELN ALPHA.
##NO_HANDLER
##NEEDED
SELECT_MULTY: POSNR.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_ERDAT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_ERZET.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_ERNAM.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_AUDAT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VBTYP.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_AUART.
CONVERT_MULTY: VBAK_AUART AUART.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_AUGRU.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_LIFSK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_WAERK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VKORG.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VTWEG.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VKGRP.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VKBUR.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_GSBER.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VDATU.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VSBED.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_BSTNK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_BSARK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_BSTDK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_BNAME.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_TELF1.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_KUNNR.
CONVERT_MULTY: VBAK_KUNNR ALPHA.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_KOSTL.
CONVERT_MULTY: VBAK_KOSTL ALPHA.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_AEDAT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_KVGR1.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_KVGR2.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_KOKRS.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_PS_PSP_PNR.
CONVERT_MULTY: VBAK_PS_PSP_PNR ABPSP.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_ABRVW.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_BUKRS_VF.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_XBLNR.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_ZUONR.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_AUFNR.
CONVERT_MULTY: VBAK_AUFNR ALPHA.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MATNR.
CONVERT_MULTY: MATNR MATN1.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MATKL.
##NO_HANDLER
##NEEDED
SELECT_MULTY: ARKTX.
##NO_HANDLER
##NEEDED
SELECT_MULTY: PSTYV.
##NO_HANDLER
##NEEDED
SELECT_MULTY: LFREL.
##NO_HANDLER
##NEEDED
SELECT_MULTY: FKREL.
##NO_HANDLER
##NEEDED
SELECT_MULTY: UEPOS.
##NO_HANDLER
##NEEDED
SELECT_MULTY: ABGRU.
##NO_HANDLER
##NEEDED
SELECT_MULTY: PRODH.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MEINS.
CONVERT_MULTY: MEINS CUNIT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: FAKSP.
##NO_HANDLER
##NEEDED
SELECT_MULTY: NETWR.
##NO_HANDLER
##NEEDED
SELECT_MULTY: WAERK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VRKME.
CONVERT_MULTY: VRKME CUNIT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: UMVKZ.
##NO_HANDLER
##NEEDED
SELECT_MULTY: UMVKN.
##NO_HANDLER
##NEEDED
SELECT_MULTY: GEWEI.
CONVERT_MULTY: GEWEI CUNIT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VOLEH.
CONVERT_MULTY: VOLEH CUNIT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VGBEL.
CONVERT_MULTY: VGBEL ALPHA.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VGPOS.
##NO_HANDLER
##NEEDED
SELECT_MULTY: WERKS.
##NO_HANDLER
##NEEDED
SELECT_MULTY: LGORT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VSTEL.
##NO_HANDLER
##NEEDED
SELECT_MULTY: ROUTE.
##NO_HANDLER
##NEEDED
SELECT_MULTY: ERNAM.
##NO_HANDLER
##NEEDED
SELECT_MULTY: NETPR.
##NO_HANDLER
##NEEDED
SELECT_MULTY: KPEIN.
##NO_HANDLER
##NEEDED
SELECT_MULTY: KMEIN.
CONVERT_MULTY: KMEIN CUNIT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: SHKZG.
##NO_HANDLER
##NEEDED
SELECT_MULTY: KONDM.
##NO_HANDLER
##NEEDED
SELECT_MULTY: KTGRM.
##NO_HANDLER
##NEEDED
SELECT_MULTY: BWTAR.
##NO_HANDLER
##NEEDED
SELECT_MULTY: WAVWR.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAP_AEDAT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: PRCTR.
CONVERT_MULTY: PRCTR ALPHA.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MVGR1.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MVGR2.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MVGR3.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MVGR4.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MVGR5.
##NO_HANDLER
##NEEDED
SELECT_MULTY: SOBKZ.
##NO_HANDLER
##NEEDED
SELECT_MULTY: PS_PSP_PNR.
CONVERT_MULTY: PS_PSP_PNR ABPSP.
##NO_HANDLER
##NEEDED
SELECT_MULTY: AUFNR.
CONVERT_MULTY: AUFNR ALPHA.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VPMAT.
CONVERT_MULTY: VPMAT MATN1.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VPWRK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: CEPOK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: KDMAT.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MPROK.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VGTYP.
##NO_HANDLER
##NEEDED
SELECT_MULTY: FAKSP_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MATKL_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: MATNR_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: PRCTR_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: PS_PSP_PNR_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_AUART_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_AUGRU_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_BUKRS_VF_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_KUNNR_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_PS_PSP_PNR_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VBTYP_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VKORG_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VBAK_VTWEG_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VGTYP_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VPMAT_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: VPWRK_DESC.
##NO_HANDLER
##NEEDED
SELECT_MULTY: WERKS_DESC.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: TARGET_CUKY.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: BACKDAYS.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: DATE_REF_FLD.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: CURRENCY_CONV_DATE.
##NO_HANDLER
##NEEDED
SELECT_MULTY: DURATION.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: DURATION_UNIT.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: EXC_RATE_TYPE.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: FORWDAYS.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: LANGU.
CONVERT_SINGLE: LANGU ISOLA.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: TIME_REF_FLD.
##NEEDED
DATA LV_SW_DEST TYPE RFCDEST.
##NEEDED
DATA LV_DELIMITER TYPE SONV-FLAG.
##NEEDED
DATA LV_NO_DATA TYPE SONV-FLAG.
##NEEDED
DATA LV_ROWSKIPS TYPE SOID-ACCNT.
##NEEDED
DATA LV_ROWCOUNT TYPE SOID-ACCNT.
##NEEDED
DATA LV_REC_CNT_ONLY TYPE FLAG.
##NEEDED
DATA LV_ROWCOUNT2 TYPE SOID-ACCNT.
##NEEDED
DATA LT_OPTIONS TYPE TABLE OF RFC_DB_OPT.
##NEEDED
DATA LT_DATA TYPE TABLE OF /SKN/S_SW_TAB2000.
##NEEDED
DATA LT_TABLES_LIST TYPE /SKN/TT_TABLES.
##NEEDED
DATA LT_JOIN_CONDITION TYPE /SKN/TT_TABLE_JOIN.
##NEEDED
DATA LT_SEL_FIELDS TYPE /SKN/TT_SEL_FIELDS.
##NEEDED
DATA LT_SORT_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT.
##NEEDED
DATA LT_GROUP_BY_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT.
##NEEDED
DATA LT_HAVING_OPTIONS TYPE TABLE OF RFC_DB_OPT.
##NEEDED
DATA LT_OUTPUT_FIELDS TYPE /SKN/TT_RFC_DB_FLD_EXTEND.
##NEEDED
DATA LT_DFIES TYPE TABLE OF DFIES.
##NEEDED
DATA LT_RETURN TYPE BAPIRET2_T.
##NEEDED
DATA LT_ALL_ENTRIES_TAB TYPE TABLE OF /SKN/S_SW_TAB6000.
##NEEDED
DATA LT_ALL_ENTRIES_COND TYPE TABLE OF /SKN/S_TABLE_JOIN.
##NEEDED
DATA LT_ALL_ENTRIES_DFIES TYPE TABLE OF DFIES.
##NO_HANDLER
##NEEDED
SELECT_MULTY: NETWR_FOREIGN.
##NO_HANDLER
##NEEDED
SELECT_MULTY: NETPR_FOREIGN.
##NO_HANDLER
##NEEDED
SELECT_MULTY: WAVWR_FOREIGN.
##NEEDED
DATA LV_D_FROM TYPE SY-DATUM.
##NEEDED
DATA LV_T_FROM TYPE SY-UZEIT.
##NEEDED
DATA LV_D_TO TYPE SY-DATUM.
##NEEDED
DATA LV_T_TO TYPE SY-UZEIT.
##NEEDED
DATA LV_TIME_UNIT TYPE /SKN/E_SW_SCHEDL_UNIT.
##NEEDED
DATA LV_TIME_DIFF TYPE INT4.
##NEEDED
DATA LV_FAKSK TYPE FAKSK.
##NEEDED
DATA LV_SW_DEST2 TYPE RFCDEST.
##NEEDED
DATA LV_BLOCK_DESC TYPE BEZEI_FAKSP.
##NEEDED
DATA LV_MATKL TYPE MATKL.
##NEEDED
DATA LV_MATKL_DESC TYPE WGBEZ.
##NEEDED
DATA LV_MATKL_DESC_L TYPE WGBEZ60.
##NEEDED
DATA LV_MATNR TYPE MATNR.
##NEEDED
DATA LV_MATERIAL_DESC TYPE MAKTX.
##NEEDED
DATA LV_DATE TYPE SYST-DATUM.
##NEEDED
DATA LV_FOREIGN_CURRENCY TYPE WAERS.
##NEEDED
DATA LV_LOCAL_CURRENCY TYPE WAERS.
##NEEDED
DATA LV_TYPE_OF_RATE TYPE KURST.
##NEEDED
DATA LV_DERIVED_RATE_TYPE TYPE TCURR-KURST.
##NEEDED
DATA LV_PSPNR TYPE PS_POSNR.
##NEEDED
DATA LV_BLOCK_SIZE TYPE SOID-ACCNT.
##NEEDED
DATA LV_WBS_DESC TYPE PS_POST1.
##NEEDED
DATA LV_AUART TYPE AUART.
##NEEDED
DATA LV_TYPE_DESC TYPE BEZEI20.
##NEEDED
DATA LV_AUGRU TYPE AUGRU.
##NEEDED
DATA LV_AUGRU_DESC TYPE BEZEI40.
##NEEDED
DATA LV_BUKRS TYPE BUKRS.
##NEEDED
DATA LV_COMP_CODE_DESC TYPE BUTXT.
##NEEDED
DATA LV_KUNNR TYPE KUNNR.
##NEEDED
DATA LV_CUST_DESC TYPE NAME1_GP.
##NEEDED
DATA LV_LAND1 TYPE LAND1.
##NEEDED
DATA LV_VBTYP TYPE VBTYP.
##NEEDED
DATA LV_CAT_DESC TYPE VAL_TEXT.
##NEEDED
DATA LV_VKORG TYPE VKORG.
##NEEDED
DATA LV_SALES_ORG_DESC TYPE VTXTK.
##NEEDED
DATA LV_VTWEG TYPE VTWEG.
##NEEDED
DATA LV_DISTR_CHAN_DESC TYPE VTXTK.
##NEEDED
DATA LV_WERKS TYPE WERKS_D.
##NEEDED
DATA LV_PLANT_DESC TYPE NAME1.
CLEAR IS_ALERT.
REFRESH T_DATA.
REFRESH LT_OPTIONS.
REFRESH LT_OUT_WHERE_COND.
REFRESH LT_TABLES_LIST.
CLEAR: LV_LINES, LS_OPTION,
       LT_OPTIONS_CURR, LT_COND_CURR, LT_OPTIONS_MAIN.
##NO_HANDLER
SELECT_SINGLE: SW_DEST.
##NO_HANDLER
_GET_CURRENT_DATE_TIME LV_MANAGE_IN_UTC LV_SW_DEST SY_DATLO SY_TIMLO.
IF R_DATUM[] IS INITIAL.
  RS_DATUM-SIGN   = 'I'.
  IF LV_FORWDAYS IS INITIAL.
    DATE_FROM = SY_DATLO - LV_BACKDAYS.
    DATE_TO   = SY_DATLO.
    RS_DATUM-OPTION = 'BT'.
  ELSE.
    IF LV_BACKDAYS IS NOT INITIAL.
      DATE_FROM = SY_DATLO - LV_BACKDAYS.
      DATE_TO   = SY_DATLO + LV_FORWDAYS.
      RS_DATUM-OPTION = 'BT'.
    ELSE.
      DATE_FROM = SY_DATLO + LV_FORWDAYS.
      RS_DATUM-OPTION = 'GE'.
    ENDIF.
  ENDIF.
  RS_DATUM-LOW  = DATE_FROM.
  RS_DATUM-HIGH = DATE_TO.
  APPEND RS_DATUM TO R_DATUM.
ENDIF.
CASE LV_DATE_REF_FLD.
WHEN 'ERDAT'.
IF R_ERDAT[] IS INITIAL.
R_ERDAT[] = R_DATUM[].
ENDIF.
WHEN 'VBAK_ERDAT'.
IF R_VBAK_ERDAT[] IS INITIAL.
R_VBAK_ERDAT[] = R_DATUM[].
ENDIF.
WHEN 'VBAK_AUDAT'.
IF R_VBAK_AUDAT[] IS INITIAL.
R_VBAK_AUDAT[] = R_DATUM[].
ENDIF.
WHEN 'VBAK_VDATU'.
IF R_VBAK_VDATU[] IS INITIAL.
R_VBAK_VDATU[] = R_DATUM[].
ENDIF.
WHEN 'VBAK_BSTDK'.
IF R_VBAK_BSTDK[] IS INITIAL.
R_VBAK_BSTDK[] = R_DATUM[].
ENDIF.
WHEN 'VBAK_AEDAT'.
IF R_VBAK_AEDAT[] IS INITIAL.
R_VBAK_AEDAT[] = R_DATUM[].
ENDIF.
WHEN 'VBAP_AEDAT'.
IF R_VBAP_AEDAT[] IS INITIAL.
R_VBAP_AEDAT[] = R_DATUM[].
ENDIF.
ENDCASE.
REFRESH R_DATUM.
##NO_HANDLER
_APPEND_TABLES_LIST 'VBAK' '' 'A'.
##NO_HANDLER
_APPEND_TABLES_LIST 'VBAP' '' 'B'.
LV_RANGE = 'B~POSNR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE POSNR.
LV_RANGE = 'A~ERDAT'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE ERDAT.
LV_RANGE = 'A~ERNAM'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE ERNAM.
LV_RANGE = 'A~WAERK'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE WAERK.
LV_RANGE = 'A~PS_PSP_PNR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE PS_PSP_PNR.
LV_RANGE = 'A~AUFNR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE AUFNR.
LV_RANGE = 'B~MATNR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MATNR.
LV_RANGE = 'B~MATKL'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MATKL.
LV_RANGE = 'B~ARKTX'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE ARKTX.
LV_RANGE = 'B~PSTYV'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE PSTYV.
LV_RANGE = 'B~LFREL'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE LFREL.
LV_RANGE = 'B~FKREL'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE FKREL.
LV_RANGE = 'B~UEPOS'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE UEPOS.
LV_RANGE = 'B~ABGRU'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE ABGRU.
LV_RANGE = 'B~PRODH'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE PRODH.
LV_RANGE = 'B~MEINS'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MEINS.
LV_RANGE = 'B~FAKSP'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE FAKSP.
LV_RANGE = 'B~NETWR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE NETWR.
LV_RANGE = 'B~WAERK'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE WAERK.
LV_RANGE = 'B~KWMENG'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE KWMENG.
LV_RANGE = 'B~LSMENG'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE LSMENG.
LV_RANGE = 'B~KBMENG'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE KBMENG.
LV_RANGE = 'B~KLMENG'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE KLMENG.
LV_RANGE = 'B~VRKME'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VRKME.
LV_RANGE = 'B~UMVKZ'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE UMVKZ.
LV_RANGE = 'B~UMVKN'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE UMVKN.
LV_RANGE = 'B~BRGEW'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE BRGEW.
LV_RANGE = 'B~NTGEW'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE NTGEW.
LV_RANGE = 'B~GEWEI'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE GEWEI.
LV_RANGE = 'B~VOLUM'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VOLUM.
LV_RANGE = 'B~VOLEH'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VOLEH.
LV_RANGE = 'B~VGBEL'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VGBEL.
LV_RANGE = 'B~VGPOS'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VGPOS.
LV_RANGE = 'B~WERKS'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE WERKS.
LV_RANGE = 'B~LGORT'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE LGORT.
LV_RANGE = 'B~VSTEL'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VSTEL.
LV_RANGE = 'B~ROUTE'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE ROUTE.
LV_RANGE = 'B~ERDAT'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE ERDAT.
LV_RANGE = 'B~ERNAM'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE ERNAM.
LV_RANGE = 'B~NETPR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE NETPR.
LV_RANGE = 'B~KPEIN'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE KPEIN.
LV_RANGE = 'B~KMEIN'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE KMEIN.
LV_RANGE = 'B~SHKZG'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE SHKZG.
LV_RANGE = 'B~KONDM'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE KONDM.
LV_RANGE = 'B~KTGRM'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE KTGRM.
LV_RANGE = 'B~BWTAR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE BWTAR.
LV_RANGE = 'B~WAVWR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE WAVWR.
LV_RANGE = 'B~PRCTR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE PRCTR.
LV_RANGE = 'B~MVGR1'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MVGR1.
LV_RANGE = 'B~MVGR2'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MVGR2.
LV_RANGE = 'B~MVGR3'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MVGR3.
LV_RANGE = 'B~MVGR4'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MVGR4.
LV_RANGE = 'B~MVGR5'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MVGR5.
LV_RANGE = 'B~SOBKZ'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE SOBKZ.
LV_RANGE = 'B~PS_PSP_PNR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE PS_PSP_PNR.
LV_RANGE = 'B~AUFNR'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE AUFNR.
LV_RANGE = 'B~VPMAT'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VPMAT.
LV_RANGE = 'B~VPWRK'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VPWRK.
LV_RANGE = 'B~CEPOK'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE CEPOK.
LV_RANGE = 'B~KDMAT'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE KDMAT.
LV_RANGE = 'B~MPROK'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE MPROK.
LV_RANGE = 'B~VGTYP'.
##NO_HANDLER
_RANGE_TO_SEL_TABLE LV_RANGE VGTYP.
LT_OPTIONS[] = LT_OUT_WHERE_COND[].
LT_OPTIONS_MAIN[] = LT_OPTIONS[].
LV_LEFTTAB = 'A'.
LV_LEFTFIELD = 'VBELN'.
LV_RIGHTTAB = 'B'.
LV_RIGHTFIELD = 'VBELN'.
_JOIN_CONDITION LV_LEFTTAB LV_LEFTFIELD LV_RIGHTTAB LV_RIGHTFIELD.
CLEAR LV_AMOUNT_FIELD.
CLEAR LV_CURRENCY_FIELD.
CLEAR LV_RATE_CONV.
CLEAR LV_DATE_CONV.
CLEAR LV_CURRENCY_FROM.
CLEAR LV_CURRENCY_TO.
CLEAR LV_AMOUNT_FROM.
CLEAR LV_AMOUNT_TO.
CLEAR LWA_OUT_WHERE_COND.
CLEAR LWA_OUT_WHERE_COND.
CLEAR LV_AMOUNT_FIELD_FR.
CLEAR LT_DATA_RFC.
CLEAR LV_SORT_BY.
CLEAR LT_SEL_FIELDS.
CLEAR LV_DATE_FIELD.
##NO_HANDLER.
_APPEND_LT_GROUP_BY_FIELDS 'VBAK~WAERK'.
IF LV_CURRENCY_CONV_DATE IS NOT INITIAL.
##NO_HANDLER.
_APPEND_LT_GROUP_BY_FIELDS LV_CURRENCY_CONV_DATE.
ELSE.
##NO_HANDLER.
_APPEND_LT_GROUP_BY_FIELDS 'VBAK~ERDAT'.
ENDIF.
LS_SEL_FIELDS-TABLE = 'VBAK'.
LS_SEL_FIELDS-FIELD = 'WAERK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
LS_SORT_OPT-TEXT = 'VBAK~WAERK'.
APPEND LS_SORT_OPT  TO LT_SORT_OPTIONS.
CLEAR LS_SEL_FIELDS.
CLEAR LS_SORT_OPT.
IF LT_OPTIONS IS NOT INITIAL.
LS_OPTION-TEXT = 'AND'.
APPEND LS_OPTION  TO LT_OPTIONS.
ENDIF.
LS_OPTION-TEXT = '( VBAK~WAERK NE space )'.
APPEND LS_OPTION  TO LT_OPTIONS.
LS_SEL_FIELDS-TABLE = 'VBAK'.
IF LV_CURRENCY_CONV_DATE IS NOT INITIAL.
LS_SEL_FIELDS-FIELD = LV_CURRENCY_CONV_DATE.
ELSE.
LS_SEL_FIELDS-FIELD = 'ERDAT'.
ENDIF.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
IF LV_CURRENCY_CONV_DATE IS NOT INITIAL.
CONCATENATE LS_SEL_FIELDS-TABLE LV_CURRENCY_CONV_DATE
INTO LS_SORT_OPT-TEXT SEPARATED BY '~'.
ELSE.
LS_SORT_OPT-TEXT = 'VBAK~ERDAT'.
ENDIF.
APPEND LS_SORT_OPT  TO LT_SORT_OPTIONS.
CLEAR LS_SEL_FIELDS.
CLEAR LS_SORT_OPT.
IF LT_OPTIONS IS NOT INITIAL.
LS_OPTION-TEXT = 'AND'.
APPEND LS_OPTION  TO LT_OPTIONS.
ENDIF.
IF LV_CURRENCY_CONV_DATE IS NOT INITIAL.
CONCATENATE 'VBAK' LV_CURRENCY_CONV_DATE
INTO LS_OPTION-TEXT SEPARATED BY '~'.
CONCATENATE '(' LS_OPTION-TEXT 'NE space' ')'
INTO LS_OPTION-TEXT SEPARATED BY SPACE.
ELSE.
LS_OPTION-TEXT = '( VBAK~ERDAT NE space )'.
ENDIF.
APPEND LS_OPTION  TO LT_OPTIONS.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: SW_DEST.
CLEAR: LS_SEL_FIELDS, LT_DATA_TMP.
CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
  DESTINATION LV_SW_DEST
*   EXPORTING
*     delimiter            = ' '
*     NO_DATA              = ' '
*     ROWSKIPS             = 0
*     rowcount             = im_number
*     REC_CNT_ONLY         =
  IMPORTING
    ##NEEDED
    ROWCOUNT             = LV_ROWCOUNT
  TABLES
    OPTIONS              = LT_OPTIONS
    DATA                 = LT_DATA
    TABLES_LIST          = LT_TABLES_LIST ##ENH_OK
    JOIN_CONDITION       = LT_JOIN_CONDITION ##ENH_OK
    SEL_FIELDS           = LT_SEL_FIELDS ##ENH_OK
    SORT_OPTIONS         = LT_SORT_OPTIONS ##ENH_OK
    GROUP_BY_OPTIONS     = LT_GROUP_BY_OPTIONS ##ENH_OK
    HAVING_OPTIONS       = LT_HAVING_OPTIONS ##ENH_OK
    OUTPUT_FIELDS        = LT_OUTPUT_FIELDS ##ENH_OK
    DFIES                = LT_DFIES ##ENH_OK
    RETURN               = LT_RETURN ##ENH_OK
    ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB ##ENH_OK
    ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND ##ENH_OK
    ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES ##ENH_OK
  EXCEPTIONS
    TABLE_NOT_AVAILABLE  = 1
    TABLE_WITHOUT_DATA   = 2
    OPTION_NOT_VALID     = 3
    FIELD_NOT_VALID      = 4
    NOT_AUTHORIZED       = 5
    DATA_BUFFER_EXCEEDED = 6
    OTHERS               = 7.
CLEAR: LV_ROWCOUNT, LT_JOIN_CONDITION, LT_SEL_FIELDS,
       LT_SORT_OPTIONS, LT_GROUP_BY_OPTIONS, LT_TABLES_LIST.
IF SY-SUBRC IS NOT INITIAL OR LT_RETURN IS NOT INITIAL.
CLEAR LT_DATA.
ELSE.
REFRESH LT_DATA_TMP.
_RFC_TO_T_DATA_INDEX LT_DATA LT_DATA_TMP LT_OUTPUT_FIELDS 1.
ENDIF.
IF LT_DATA_TMP[] IS NOT INITIAL.
APPEND LINES OF LT_DATA_TMP[] TO T_DATA[].
ENDIF.
CHECK T_DATA[] IS NOT INITIAL.
CLEAR LT_DATA_RFC.
CLEAR LT_SORT_OPTIONS.
REFRESH LT_SEL_FIELDS.
CLEAR LT_DFIES.
CLEAR LT_GROUP_BY_OPTIONS.
REFRESH LT_OUTPUT_FIELDS.
CLEAR LT_OUT_WHERE_COND.
CLEAR LT_OPTIONS[].
.
LV_AMOUNT_FIELD_CURR = 'VBAP~NETWR'.
LV_AMOUNT_FIELD_FR = 'NETWR_FOREIGN'.
LV_CURRENCY_FIELD_FR = 'TARGET_CUKY'.
LV_DATE_CONV = SY-DATLO.
LV_CURRENCY_FIELD = 'WAERK'.
LV_CURRENCY_FIELD_CURR = 'VBAK~WAERK'.
LV_CURRENCY_FROM = 'EUR'.
LV_RATE_CONV = 'M'.
LV_CURR_CONV_DATE_FIELD = 'LV_CURRENCY_CONV_DATE'.
LV_SOURCE_CUKY_FIELD = 'LV_NETWR_CUKY_SOURCE'.
LV_TARGET_CUKY_FIELD = 'LV_TARGET_CUKY'.
LV_EXC_RATE_TYPE_FIELD = 'LV_EXC_RATE_TYPE'.
CLEAR: LT_CURR.
IF T_DATA[] IS NOT INITIAL.
  DELETE ADJACENT DUPLICATES FROM T_DATA[] COMPARING (LV_CURRENCY_FIELD)
.
ENDIF.
* Set 'CURRENCY_CONV_DATE' parameter
ASSIGN (LV_CURR_CONV_DATE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_FIELD_DATE.
  LV_FIELD_DATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
IF LV_TYPE_OF_RATE IS INITIAL.
  LV_TYPE_OF_RATE = 'M'.
ENDIF.
* Set 'EXP_RATE_TYPE' Parameter field
ASSIGN (LV_EXC_RATE_TYPE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_TYPE_OF_RATE.
  LV_TYPE_OF_RATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
*** Get Document currency(Local) by currency and date field
IF T_DATA[] IS NOT INITIAL AND
   LT_RETURN IS INITIAL.
  CLEAR: LT_SORT_BY, LWA_OUT_WHERE_COND,
         LT_OUT_WHERE_COND, LT_OPTIONS.
*  IF lv_currency_field IS NOT INITIAL.
*    DELETE ADJACENT DUPLICATES FROM t_data
*      COMPARING (lv_currency_field).
*  ENDIF.
*************************** TARGET AMOUNT
**************************************
* Set Target Amount Parameter field(Alert)
  IF LV_AMOUNT_FIELD_FR IS NOT INITIAL.
    CONCATENATE 'R_' LV_AMOUNT_FIELD_FR INTO LV_RESULT_TXT.
    IF LV_RESULT_TXT IS NOT INITIAL.
      ASSIGN (LV_RESULT_TXT) TO <FS_AMOUNT_TAB>.
    ENDIF.
  ENDIF.
*************************** TARGET AMOUNT
**************************************
  CLEAR: LV_RESULT_TXT.
*************************** TARGET CUKY
**************************************
  IF LV_CURRENCY_FIELD_FR IS NOT INITIAL.
* Set Target CUKY Parameter field
    CONCATENATE 'LV_' LV_CURRENCY_FIELD_FR INTO LV_RESULT_TXT.
    IF LV_RESULT_TXT IS NOT INITIAL.
      ASSIGN (LV_RESULT_TXT) TO <FS_CURRENCY_STR>.
    ENDIF.
  ENDIF.
*************************** TARGET CUKY
**************************************
  IF <FS_AMOUNT_TAB> IS ASSIGNED AND
     <FS_AMOUNT_TAB> IS NOT INITIAL.
    LV_OPEN = 'X'.
* Loop on Local Currency table data
    LOOP AT T_DATA.
      CLEAR: LT_OPTION.
      LOOP AT <FS_AMOUNT_TAB> ASSIGNING <FS_AMOUNT_STR>.
        CLEAR: LS_OPTION, LV_QUERY_CURR, LV_TEXT1,
               LV_AMOUNT_TO, LV_TEXT2, LV_VAL, LV_RC,
               LV_AMOUNT_FROM, LV_CURR_FROM, LV_CURR_TO,
               LV_SIGN, LV_OPTION, LV_DATE_CONV,
               LV_LOW, LV_HIGH.
******
* Set SIGN amount for conversion
        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <FS_AMOUNT_STR>
          TO <FS_FIELD>.
        IF SY-SUBRC = 0 AND
            <FS_FIELD> IS ASSIGNED.
          LV_SIGN = <FS_FIELD>.
          UNASSIGN <FS_FIELD>.
        ENDIF.
******
* Set LOW amount for conversion
        ASSIGN COMPONENT 'LOW' OF STRUCTURE <FS_AMOUNT_STR>
          TO <FS_FIELD>.
        IF SY-SUBRC = 0 AND
            <FS_FIELD> IS ASSIGNED.
          LV_AMOUNT_FROM = <FS_FIELD>.
          UNASSIGN <FS_FIELD>.
        ENDIF.
        IF LV_AMOUNT_FROM NE 0 AND
             LV_AMOUNT_FROM IS NOT INITIAL.
******
* Currency value from condition setting
* Default setting(from AG)
          IF LV_CURRENCY_FROM IS NOT INITIAL.
            LV_CURR_FROM = LV_CURRENCY_FROM.
          ENDIF.
          IF <FS_CURRENCY_STR> IS ASSIGNED AND
             <FS_CURRENCY_STR> IS NOT INITIAL.
* Set Local Currency conversion
            LV_CURR_FROM = <FS_CURRENCY_STR>.
          ENDIF.
******
* Currency value of the original document
          IF LV_CURRENCY_TO IS INITIAL.
            IF LV_CURRENCY_FIELD IS NOT INITIAL.
              ASSIGN COMPONENT LV_CURRENCY_FIELD
                OF STRUCTURE T_DATA TO <FS_VAL>.
            ENDIF.
            IF SY-SUBRC IS INITIAL AND
                <FS_VAL> IS ASSIGNED.
              LV_CURR_TO = <FS_VAL>.
              UNASSIGN <FS_VAL>.
            ENDIF.
          ELSE.
            LV_CURR_TO = LV_CURRENCY_TO.
          ENDIF.
* Set Currency Conversion Date Parameter field
          IF LV_DATE_CONV IS INITIAL.
            IF LV_CURRENCY_CONV_DATE IS NOT INITIAL.
              LV_FIELD_DATE = LV_CURRENCY_CONV_DATE.
            ENDIF.
            IF LV_DATE_FIELD IS NOT INITIAL AND
                NOT LV_FIELD_DATE CA '0123456789'.
              ASSIGN COMPONENT LV_DATE_FIELD
              OF STRUCTURE T_DATA TO <FS_VAL>.
              IF SY-SUBRC IS INITIAL
                  AND <FS_VAL> IS ASSIGNED.
                LV_DATE_CONV = <FS_VAL>.
                UNASSIGN <FS_VAL>.
              ENDIF.
            ELSE.
              LV_DATE_CONV = SY-DATLO.
            ENDIF.
          ENDIF.
          IF LV_CURR_TO NE LV_CURR_FROM AND
               LV_CURR_FROM IS NOT INITIAL AND
                 LV_CURR_TO IS NOT INITIAL.
            CLEAR: LV_EXC_RATE_CONV, LV_AMOUNT_TO.
* Unit conversion for LOW
            CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
              EXPORTING
*               CLIENT           = SY-MANDT
                DATE             = LV_DATE_CONV
                FOREIGN_CURRENCY = LV_CURR_TO
                LOCAL_AMOUNT     = LV_AMOUNT_FROM
                LOCAL_CURRENCY   = LV_CURR_FROM
                TYPE_OF_RATE     = LV_RATE_CONV
                SW_DEST          = LV_SW_DEST
              IMPORTING
                EXCHANGE_RATE    = LV_EXC_RATE_CONV
                FOREIGN_AMOUNT   = LV_AMOUNT_TO
              EXCEPTIONS
                NO_RATE_FOUND    = 1
                OVERFLOW         = 2
                NO_FACTORS_FOUND = 3
                NO_SPREAD_FOUND  = 4
                DERIVED_2_TIMES  = 5.
            IF SY-SUBRC IS NOT INITIAL OR
                 LV_EXC_RATE_CONV IS INITIAL.
* Implement suitable error handling here
              CONTINUE.
            ELSEIF LV_AMOUNT_TO IS INITIAL.
              LV_AMOUNT_TO = ABS( LV_AMOUNT_FROM / LV_EXC_RATE_CONV ).
            ENDIF.
          ELSE.
            LV_AMOUNT_TO = LV_AMOUNT_FROM.
          ENDIF.
          IF LV_AMOUNT_TO IS NOT INITIAL.
            IF LV_INSERT_CURR IS INITIAL.
              LV_INSERT_CURR = 'X'.
            ENDIF.
            LOOP AT LT_CURR INTO LS_CURR WHERE WAERS EQ LV_CURR_TO.
              IF LV_AMOUNT_TO EQ LS_CURR-AMOUNT.
                IF LV_DATE_FIELD IS NOT INITIAL.
                  IF LV_DATE_CONV EQ LS_CURR-DATE.
                    LV_CONT = 'X'.
                    EXIT.
                  ENDIF.
                ELSE.
                  LV_CONT = 'X'.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.
            IF SY-SUBRC IS NOT INITIAL OR LV_CONT IS INITIAL.
              LS_CURR-AMOUNT = LV_AMOUNT_TO.
              LS_CURR-WAERS  = LV_CURR_TO.
              IF LV_DATE_FIELD IS NOT INITIAL.
                LS_CURR-DATE   = LV_DATE_CONV.
              ENDIF.
              APPEND LS_CURR TO LT_CURR.
            ELSEIF LV_CONT EQ 'X'.
              CLEAR LV_CONT.
              CONTINUE.
            ENDIF.
            LV_LOW = LV_AMOUNT_TO.
            CONDENSE LV_LOW.
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
******* Date of Currency conversion
            IF LV_DATE_FIELD_CURR IS NOT INITIAL.
              CONCATENATE '''' LV_DATE_CONV '''' INTO LV_VAL.
              CONCATENATE LV_DATE_FIELD_CURR 'EQ' LV_VAL
                INTO LV_QUERY_DATE SEPARATED BY SPACE.
              CONCATENATE LV_QUERY_DATE 'AND' INTO
                LS_OPTION-TEXT SEPARATED BY SPACE.
              APPEND LS_OPTION TO LT_OPTION.
            ENDIF.
            CLEAR: LS_OPTION, LV_QUERY_DATE, LV_VAL.
******* Date of Currency conversion
******* Reference Field(CUKY) of Currency
            IF LV_CURRENCY_FIELD_CURR IS NOT INITIAL AND
               LV_AMOUNT_FIELD_CURR   IS NOT INITIAL.
              CONCATENATE '''' LV_CURR_TO '''' INTO LV_QUERY_CURR.
              LV_TEXT1 = LV_CURRENCY_FIELD_CURR.
              CONCATENATE LV_TEXT1 'EQ' LV_QUERY_CURR INTO LV_TEXT2
                SEPARATED BY SPACE.
******* Reference Field(CUKY) of Currency
******* Foreign Amount value
              LV_VAL = LV_AMOUNT_TO.
              SHIFT LV_VAL RIGHT DELETING TRAILING SPACE.
              SHIFT LV_VAL LEFT DELETING LEADING SPACE.
              CONCATENATE '''' LV_VAL '''' INTO LV_VAL
                IN CHARACTER MODE.
******* Foreign Amount value
              ASSIGN COMPONENT 'OPTION' OF STRUCTURE <FS_AMOUNT_STR>
                TO <FS_FIELD>.
              CLEAR LV_TEXT1.
              LV_TEXT1 = LV_AMOUNT_FIELD_CURR.
              IF <FS_FIELD> IS ASSIGNED AND
                   <FS_FIELD> IS NOT INITIAL.
                LV_OPTION = <FS_FIELD>.
                IF LV_OPTION EQ 'BT'.
                  LV_TEXT_OPT = 'BETWEEN'.
                ELSE.
                  LV_TEXT_OPT = <FS_FIELD>.
                ENDIF.
                CONCATENATE LV_TEXT2 'AND'
                            LV_TEXT1 LV_TEXT_OPT LV_VAL
                  INTO LS_OPTION-TEXT SEPARATED BY SPACE.
                UNASSIGN <FS_FIELD>.
              ENDIF.
****** Conversion 'HIGH' value
              CLEAR: LV_AMOUNT_FROM.
              IF <FS_AMOUNT_STR> IS ASSIGNED AND
                   <FS_AMOUNT_STR> IS NOT INITIAL.
                ASSIGN COMPONENT 'HIGH' OF STRUCTURE <FS_AMOUNT_STR>
                  TO <FS_FIELD>.
                IF SY-SUBRC = 0 AND <FS_FIELD> IS ASSIGNED.
                  LV_AMOUNT_FROM = <FS_FIELD>.
                ENDIF.
              ENDIF.
              IF LV_AMOUNT_FROM NE 0 AND
                   LV_AMOUNT_FROM IS NOT INITIAL.
                CLEAR: LV_AMOUNT_TO.
                IF LV_CURR_FROM NE LV_CURR_TO  AND
                   LV_CURR_FROM IS NOT INITIAL AND
                   LV_CURR_TO   IS NOT INITIAL.
                  CLEAR: LV_EXC_RATE_CONV, LV_AMOUNT_TO.
                  CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
                    EXPORTING
*                     CLIENT           = SY-MANDT
                      DATE             = LV_DATE_CONV
                      FOREIGN_CURRENCY = LV_CURR_TO
                      LOCAL_AMOUNT     = LV_AMOUNT_FROM
                      LOCAL_CURRENCY   = LV_CURR_FROM
                      TYPE_OF_RATE     = LV_RATE_CONV
                      SW_DEST          = LV_SW_DEST
                    IMPORTING
                      EXCHANGE_RATE    = LV_EXC_RATE_CONV
                      FOREIGN_AMOUNT   = LV_AMOUNT_TO
                    EXCEPTIONS
                      NO_RATE_FOUND    = 1
                      OVERFLOW         = 2
                      NO_FACTORS_FOUND = 3
                      NO_SPREAD_FOUND  = 4
                      DERIVED_2_TIMES  = 5.
                  IF SY-SUBRC IS INITIAL AND LV_AMOUNT_TO IS INITIAL.
                    LV_AMOUNT_TO = ABS( LV_AMOUNT_FROM /
LV_EXC_RATE_CONV ).
                  ENDIF.
                ELSE.
                  LV_AMOUNT_TO = LV_AMOUNT_FROM.
                ENDIF.
                IF SY-SUBRC IS INITIAL AND
                     LV_AMOUNT_TO IS NOT INITIAL.
                  LV_HIGH = LV_AMOUNT_TO.
                  CONDENSE LV_HIGH.
                  LV_VAL = LV_AMOUNT_TO.
                  CONDENSE LV_VAL.
                  CONCATENATE '''' LV_VAL '''' INTO LV_VAL.
                  CONCATENATE LS_OPTION-TEXT 'AND' LV_VAL
                    INTO LS_OPTION-TEXT SEPARATED BY SPACE.
                ELSE.
                  CONTINUE.
                ENDIF.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF LV_OPEN IS INITIAL.
            CONCATENATE LS_OPTION-TEXT ')' INTO LS_OPTION-TEXT
              SEPARATED BY SPACE.
          ENDIF.
          IF LS_OPTION IS NOT INITIAL.
            LV_LINES = LV_LINES + 1.
            APPEND LS_OPTION TO LT_OPTION.
            IF LV_LINES GT 1.
              LS_OPTION-TEXT = 'OR ('.
              APPEND LS_OPTION TO LT_OPTIONS.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF LT_OPTION TO LT_OPTIONS.
      CLEAR: LS_OPTION, LT_OPTION.
    ENDLOOP.
    IF LV_CLOSE IS INITIAL.
      LS_OPTION-TEXT = ')'.
      APPEND LS_OPTION TO LT_OPTIONS.
    ENDIF.
  ENDIF.
  IF LV_INSERT_CURR IS NOT INITIAL.
    APPEND LINES OF LT_OPTIONS TO LT_OPTIONS_CURR.
    DESCRIBE TABLE LT_OPTIONS      LINES LV_LINES_OPT.
    DESCRIBE TABLE LT_OPTIONS_CURR LINES LV_LINES_CURR.
    IF LV_AMOUNT_FIELD_CURR CS '~'.
      SPLIT LV_AMOUNT_FIELD_CURR AT '~' INTO LV_PREF LV_SUFF.
    ELSE.
      LV_SUFF = LV_AMOUNT_FIELD_CURR.
    ENDIF.
    LS_COND_CURR-CURR_FIELD = LV_SUFF.
    IF LV_LINES_CURR IS INITIAL.
      LS_COND_CURR-LINE_BEGIN = 1.
    ELSE.
      LS_COND_CURR-LINE_BEGIN = LV_LINES_CURR - LV_LINES_OPT + 1.
    ENDIF.
    LS_COND_CURR-LINE_END = LS_COND_CURR-LINE_BEGIN + LV_LINES_OPT - 1.
    APPEND LS_COND_CURR TO LT_COND_CURR.
    CLEAR: LV_INSERT_CURR, LV_PREF, LV_SUFF,
           LV_LINES_OPT, LV_LINES_CURR,
           LS_COND_CURR, LS_OPTION.
  ENDIF.
ENDIF.
****** Currency conversion **************
CLEAR LV_AMOUNT_FIELD.
CLEAR LV_CURRENCY_FIELD.
CLEAR LV_RATE_CONV.
CLEAR LV_DATE_CONV.
CLEAR LV_CURRENCY_FROM.
CLEAR LV_CURRENCY_TO.
CLEAR LV_AMOUNT_FROM.
CLEAR LV_AMOUNT_TO.
CLEAR LWA_OUT_WHERE_COND.
CLEAR LWA_OUT_WHERE_COND.
CLEAR LV_AMOUNT_FIELD_FR.
CLEAR LT_DATA_RFC.
CLEAR LV_SORT_BY.
CLEAR LT_SEL_FIELDS.
CLEAR LV_DATE_FIELD.
CLEAR LT_OUT_WHERE_COND.
CLEAR LT_OPTIONS[].
.
LV_AMOUNT_FIELD_CURR = 'VBAP~NETPR'.
LV_AMOUNT_FIELD_FR = 'NETPR_FOREIGN'.
LV_CURRENCY_FIELD_FR = 'TARGET_CUKY'.
LV_DATE_CONV = SY-DATLO.
LV_CURRENCY_FIELD = 'WAERK'.
LV_CURRENCY_FIELD_CURR = 'VBAK~WAERK'.
LV_CURRENCY_FROM = 'EUR'.
LV_RATE_CONV = 'M'.
LV_CURR_CONV_DATE_FIELD = 'LV_CURRENCY_CONV_DATE'.
LV_SOURCE_CUKY_FIELD = 'LV_NETPR_CUKY_SOURCE'.
LV_TARGET_CUKY_FIELD = 'LV_TARGET_CUKY'.
LV_EXC_RATE_TYPE_FIELD = 'LV_EXC_RATE_TYPE'.
CLEAR: LT_CURR.
IF T_DATA[] IS NOT INITIAL.
  DELETE ADJACENT DUPLICATES FROM T_DATA[] COMPARING (LV_CURRENCY_FIELD)
.
ENDIF.
* Set 'CURRENCY_CONV_DATE' parameter
ASSIGN (LV_CURR_CONV_DATE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_FIELD_DATE.
  LV_FIELD_DATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
IF LV_TYPE_OF_RATE IS INITIAL.
  LV_TYPE_OF_RATE = 'M'.
ENDIF.
* Set 'EXP_RATE_TYPE' Parameter field
ASSIGN (LV_EXC_RATE_TYPE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_TYPE_OF_RATE.
  LV_TYPE_OF_RATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
*** Get Document currency(Local) by currency and date field
IF T_DATA[] IS NOT INITIAL AND
   LT_RETURN IS INITIAL.
  CLEAR: LT_SORT_BY, LWA_OUT_WHERE_COND,
         LT_OUT_WHERE_COND, LT_OPTIONS.
*  IF lv_currency_field IS NOT INITIAL.
*    DELETE ADJACENT DUPLICATES FROM t_data
*      COMPARING (lv_currency_field).
*  ENDIF.
*************************** TARGET AMOUNT
**************************************
* Set Target Amount Parameter field(Alert)
  IF LV_AMOUNT_FIELD_FR IS NOT INITIAL.
    CONCATENATE 'R_' LV_AMOUNT_FIELD_FR INTO LV_RESULT_TXT.
    IF LV_RESULT_TXT IS NOT INITIAL.
      ASSIGN (LV_RESULT_TXT) TO <FS_AMOUNT_TAB>.
    ENDIF.
  ENDIF.
*************************** TARGET AMOUNT
**************************************
  CLEAR: LV_RESULT_TXT.
*************************** TARGET CUKY
**************************************
  IF LV_CURRENCY_FIELD_FR IS NOT INITIAL.
* Set Target CUKY Parameter field
    CONCATENATE 'LV_' LV_CURRENCY_FIELD_FR INTO LV_RESULT_TXT.
    IF LV_RESULT_TXT IS NOT INITIAL.
      ASSIGN (LV_RESULT_TXT) TO <FS_CURRENCY_STR>.
    ENDIF.
  ENDIF.
*************************** TARGET CUKY
**************************************
  IF <FS_AMOUNT_TAB> IS ASSIGNED AND
     <FS_AMOUNT_TAB> IS NOT INITIAL.
    LV_OPEN = 'X'.
* Loop on Local Currency table data
    LOOP AT T_DATA.
      CLEAR: LT_OPTION.
      LOOP AT <FS_AMOUNT_TAB> ASSIGNING <FS_AMOUNT_STR>.
        CLEAR: LS_OPTION, LV_QUERY_CURR, LV_TEXT1,
               LV_AMOUNT_TO, LV_TEXT2, LV_VAL, LV_RC,
               LV_AMOUNT_FROM, LV_CURR_FROM, LV_CURR_TO,
               LV_SIGN, LV_OPTION, LV_DATE_CONV,
               LV_LOW, LV_HIGH.
******
* Set SIGN amount for conversion
        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <FS_AMOUNT_STR>
          TO <FS_FIELD>.
        IF SY-SUBRC = 0 AND
            <FS_FIELD> IS ASSIGNED.
          LV_SIGN = <FS_FIELD>.
          UNASSIGN <FS_FIELD>.
        ENDIF.
******
* Set LOW amount for conversion
        ASSIGN COMPONENT 'LOW' OF STRUCTURE <FS_AMOUNT_STR>
          TO <FS_FIELD>.
        IF SY-SUBRC = 0 AND
            <FS_FIELD> IS ASSIGNED.
          LV_AMOUNT_FROM = <FS_FIELD>.
          UNASSIGN <FS_FIELD>.
        ENDIF.
        IF LV_AMOUNT_FROM NE 0 AND
             LV_AMOUNT_FROM IS NOT INITIAL.
******
* Currency value from condition setting
* Default setting(from AG)
          IF LV_CURRENCY_FROM IS NOT INITIAL.
            LV_CURR_FROM = LV_CURRENCY_FROM.
          ENDIF.
          IF <FS_CURRENCY_STR> IS ASSIGNED AND
             <FS_CURRENCY_STR> IS NOT INITIAL.
* Set Local Currency conversion
            LV_CURR_FROM = <FS_CURRENCY_STR>.
          ENDIF.
******
* Currency value of the original document
          IF LV_CURRENCY_TO IS INITIAL.
            IF LV_CURRENCY_FIELD IS NOT INITIAL.
              ASSIGN COMPONENT LV_CURRENCY_FIELD
                OF STRUCTURE T_DATA TO <FS_VAL>.
            ENDIF.
            IF SY-SUBRC IS INITIAL AND
                <FS_VAL> IS ASSIGNED.
              LV_CURR_TO = <FS_VAL>.
              UNASSIGN <FS_VAL>.
            ENDIF.
          ELSE.
            LV_CURR_TO = LV_CURRENCY_TO.
          ENDIF.
* Set Currency Conversion Date Parameter field
          IF LV_DATE_CONV IS INITIAL.
            IF LV_CURRENCY_CONV_DATE IS NOT INITIAL.
              LV_FIELD_DATE = LV_CURRENCY_CONV_DATE.
            ENDIF.
            IF LV_DATE_FIELD IS NOT INITIAL AND
                NOT LV_FIELD_DATE CA '0123456789'.
              ASSIGN COMPONENT LV_DATE_FIELD
              OF STRUCTURE T_DATA TO <FS_VAL>.
              IF SY-SUBRC IS INITIAL
                  AND <FS_VAL> IS ASSIGNED.
                LV_DATE_CONV = <FS_VAL>.
                UNASSIGN <FS_VAL>.
              ENDIF.
            ELSE.
              LV_DATE_CONV = SY-DATLO.
            ENDIF.
          ENDIF.
          IF LV_CURR_TO NE LV_CURR_FROM AND
               LV_CURR_FROM IS NOT INITIAL AND
                 LV_CURR_TO IS NOT INITIAL.
            CLEAR: LV_EXC_RATE_CONV, LV_AMOUNT_TO.
* Unit conversion for LOW
            CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
              EXPORTING
*               CLIENT           = SY-MANDT
                DATE             = LV_DATE_CONV
                FOREIGN_CURRENCY = LV_CURR_TO
                LOCAL_AMOUNT     = LV_AMOUNT_FROM
                LOCAL_CURRENCY   = LV_CURR_FROM
                TYPE_OF_RATE     = LV_RATE_CONV
                SW_DEST          = LV_SW_DEST
              IMPORTING
                EXCHANGE_RATE    = LV_EXC_RATE_CONV
                FOREIGN_AMOUNT   = LV_AMOUNT_TO
              EXCEPTIONS
                NO_RATE_FOUND    = 1
                OVERFLOW         = 2
                NO_FACTORS_FOUND = 3
                NO_SPREAD_FOUND  = 4
                DERIVED_2_TIMES  = 5.
            IF SY-SUBRC IS NOT INITIAL OR
                 LV_EXC_RATE_CONV IS INITIAL.
* Implement suitable error handling here
              CONTINUE.
            ELSEIF LV_AMOUNT_TO IS INITIAL.
              LV_AMOUNT_TO = ABS( LV_AMOUNT_FROM / LV_EXC_RATE_CONV ).
            ENDIF.
          ELSE.
            LV_AMOUNT_TO = LV_AMOUNT_FROM.
          ENDIF.
          IF LV_AMOUNT_TO IS NOT INITIAL.
            IF LV_INSERT_CURR IS INITIAL.
              LV_INSERT_CURR = 'X'.
            ENDIF.
            LOOP AT LT_CURR INTO LS_CURR WHERE WAERS EQ LV_CURR_TO.
              IF LV_AMOUNT_TO EQ LS_CURR-AMOUNT.
                IF LV_DATE_FIELD IS NOT INITIAL.
                  IF LV_DATE_CONV EQ LS_CURR-DATE.
                    LV_CONT = 'X'.
                    EXIT.
                  ENDIF.
                ELSE.
                  LV_CONT = 'X'.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.
            IF SY-SUBRC IS NOT INITIAL OR LV_CONT IS INITIAL.
              LS_CURR-AMOUNT = LV_AMOUNT_TO.
              LS_CURR-WAERS  = LV_CURR_TO.
              IF LV_DATE_FIELD IS NOT INITIAL.
                LS_CURR-DATE   = LV_DATE_CONV.
              ENDIF.
              APPEND LS_CURR TO LT_CURR.
            ELSEIF LV_CONT EQ 'X'.
              CLEAR LV_CONT.
              CONTINUE.
            ENDIF.
            LV_LOW = LV_AMOUNT_TO.
            CONDENSE LV_LOW.
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
******* Date of Currency conversion
            IF LV_DATE_FIELD_CURR IS NOT INITIAL.
              CONCATENATE '''' LV_DATE_CONV '''' INTO LV_VAL.
              CONCATENATE LV_DATE_FIELD_CURR 'EQ' LV_VAL
                INTO LV_QUERY_DATE SEPARATED BY SPACE.
              CONCATENATE LV_QUERY_DATE 'AND' INTO
                LS_OPTION-TEXT SEPARATED BY SPACE.
              APPEND LS_OPTION TO LT_OPTION.
            ENDIF.
            CLEAR: LS_OPTION, LV_QUERY_DATE, LV_VAL.
******* Date of Currency conversion
******* Reference Field(CUKY) of Currency
            IF LV_CURRENCY_FIELD_CURR IS NOT INITIAL AND
               LV_AMOUNT_FIELD_CURR   IS NOT INITIAL.
              CONCATENATE '''' LV_CURR_TO '''' INTO LV_QUERY_CURR.
              LV_TEXT1 = LV_CURRENCY_FIELD_CURR.
              CONCATENATE LV_TEXT1 'EQ' LV_QUERY_CURR INTO LV_TEXT2
                SEPARATED BY SPACE.
******* Reference Field(CUKY) of Currency
******* Foreign Amount value
              LV_VAL = LV_AMOUNT_TO.
              SHIFT LV_VAL RIGHT DELETING TRAILING SPACE.
              SHIFT LV_VAL LEFT DELETING LEADING SPACE.
              CONCATENATE '''' LV_VAL '''' INTO LV_VAL
                IN CHARACTER MODE.
******* Foreign Amount value
              ASSIGN COMPONENT 'OPTION' OF STRUCTURE <FS_AMOUNT_STR>
                TO <FS_FIELD>.
              CLEAR LV_TEXT1.
              LV_TEXT1 = LV_AMOUNT_FIELD_CURR.
              IF <FS_FIELD> IS ASSIGNED AND
                   <FS_FIELD> IS NOT INITIAL.
                LV_OPTION = <FS_FIELD>.
                IF LV_OPTION EQ 'BT'.
                  LV_TEXT_OPT = 'BETWEEN'.
                ELSE.
                  LV_TEXT_OPT = <FS_FIELD>.
                ENDIF.
                CONCATENATE LV_TEXT2 'AND'
                            LV_TEXT1 LV_TEXT_OPT LV_VAL
                  INTO LS_OPTION-TEXT SEPARATED BY SPACE.
                UNASSIGN <FS_FIELD>.
              ENDIF.
****** Conversion 'HIGH' value
              CLEAR: LV_AMOUNT_FROM.
              IF <FS_AMOUNT_STR> IS ASSIGNED AND
                   <FS_AMOUNT_STR> IS NOT INITIAL.
                ASSIGN COMPONENT 'HIGH' OF STRUCTURE <FS_AMOUNT_STR>
                  TO <FS_FIELD>.
                IF SY-SUBRC = 0 AND <FS_FIELD> IS ASSIGNED.
                  LV_AMOUNT_FROM = <FS_FIELD>.
                ENDIF.
              ENDIF.
              IF LV_AMOUNT_FROM NE 0 AND
                   LV_AMOUNT_FROM IS NOT INITIAL.
                CLEAR: LV_AMOUNT_TO.
                IF LV_CURR_FROM NE LV_CURR_TO  AND
                   LV_CURR_FROM IS NOT INITIAL AND
                   LV_CURR_TO   IS NOT INITIAL.
                  CLEAR: LV_EXC_RATE_CONV, LV_AMOUNT_TO.
                  CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
                    EXPORTING
*                     CLIENT           = SY-MANDT
                      DATE             = LV_DATE_CONV
                      FOREIGN_CURRENCY = LV_CURR_TO
                      LOCAL_AMOUNT     = LV_AMOUNT_FROM
                      LOCAL_CURRENCY   = LV_CURR_FROM
                      TYPE_OF_RATE     = LV_RATE_CONV
                      SW_DEST          = LV_SW_DEST
                    IMPORTING
                      EXCHANGE_RATE    = LV_EXC_RATE_CONV
                      FOREIGN_AMOUNT   = LV_AMOUNT_TO
                    EXCEPTIONS
                      NO_RATE_FOUND    = 1
                      OVERFLOW         = 2
                      NO_FACTORS_FOUND = 3
                      NO_SPREAD_FOUND  = 4
                      DERIVED_2_TIMES  = 5.
                  IF SY-SUBRC IS INITIAL AND LV_AMOUNT_TO IS INITIAL.
                    LV_AMOUNT_TO = ABS( LV_AMOUNT_FROM /
LV_EXC_RATE_CONV ).
                  ENDIF.
                ELSE.
                  LV_AMOUNT_TO = LV_AMOUNT_FROM.
                ENDIF.
                IF SY-SUBRC IS INITIAL AND
                     LV_AMOUNT_TO IS NOT INITIAL.
                  LV_HIGH = LV_AMOUNT_TO.
                  CONDENSE LV_HIGH.
                  LV_VAL = LV_AMOUNT_TO.
                  CONDENSE LV_VAL.
                  CONCATENATE '''' LV_VAL '''' INTO LV_VAL.
                  CONCATENATE LS_OPTION-TEXT 'AND' LV_VAL
                    INTO LS_OPTION-TEXT SEPARATED BY SPACE.
                ELSE.
                  CONTINUE.
                ENDIF.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF LV_OPEN IS INITIAL.
            CONCATENATE LS_OPTION-TEXT ')' INTO LS_OPTION-TEXT
              SEPARATED BY SPACE.
          ENDIF.
          IF LS_OPTION IS NOT INITIAL.
            LV_LINES = LV_LINES + 1.
            APPEND LS_OPTION TO LT_OPTION.
            IF LV_LINES GT 1.
              LS_OPTION-TEXT = 'OR ('.
              APPEND LS_OPTION TO LT_OPTIONS.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF LT_OPTION TO LT_OPTIONS.
      CLEAR: LS_OPTION, LT_OPTION.
    ENDLOOP.
    IF LV_CLOSE IS INITIAL.
      LS_OPTION-TEXT = ')'.
      APPEND LS_OPTION TO LT_OPTIONS.
    ENDIF.
  ENDIF.
  IF LV_INSERT_CURR IS NOT INITIAL.
    APPEND LINES OF LT_OPTIONS TO LT_OPTIONS_CURR.
    DESCRIBE TABLE LT_OPTIONS      LINES LV_LINES_OPT.
    DESCRIBE TABLE LT_OPTIONS_CURR LINES LV_LINES_CURR.
    IF LV_AMOUNT_FIELD_CURR CS '~'.
      SPLIT LV_AMOUNT_FIELD_CURR AT '~' INTO LV_PREF LV_SUFF.
    ELSE.
      LV_SUFF = LV_AMOUNT_FIELD_CURR.
    ENDIF.
    LS_COND_CURR-CURR_FIELD = LV_SUFF.
    IF LV_LINES_CURR IS INITIAL.
      LS_COND_CURR-LINE_BEGIN = 1.
    ELSE.
      LS_COND_CURR-LINE_BEGIN = LV_LINES_CURR - LV_LINES_OPT + 1.
    ENDIF.
    LS_COND_CURR-LINE_END = LS_COND_CURR-LINE_BEGIN + LV_LINES_OPT - 1.
    APPEND LS_COND_CURR TO LT_COND_CURR.
    CLEAR: LV_INSERT_CURR, LV_PREF, LV_SUFF,
           LV_LINES_OPT, LV_LINES_CURR,
           LS_COND_CURR, LS_OPTION.
  ENDIF.
ENDIF.
****** Currency conversion **************
CLEAR LV_AMOUNT_FIELD.
CLEAR LV_CURRENCY_FIELD.
CLEAR LV_RATE_CONV.
CLEAR LV_DATE_CONV.
CLEAR LV_CURRENCY_FROM.
CLEAR LV_CURRENCY_TO.
CLEAR LV_AMOUNT_FROM.
CLEAR LV_AMOUNT_TO.
CLEAR LWA_OUT_WHERE_COND.
CLEAR LWA_OUT_WHERE_COND.
CLEAR LV_AMOUNT_FIELD_FR.
CLEAR LT_DATA_RFC.
CLEAR LV_SORT_BY.
CLEAR LT_SEL_FIELDS.
CLEAR LV_DATE_FIELD.
CLEAR LT_OUT_WHERE_COND.
CLEAR LT_OPTIONS[].
.
LV_AMOUNT_FIELD_CURR = 'VBAP~WAVWR'.
LV_AMOUNT_FIELD_FR = 'WAVWR_FOREIGN'.
LV_CURRENCY_FIELD_FR = 'TARGET_CUKY'.
LV_DATE_CONV = SY-DATLO.
LV_CURRENCY_FIELD = 'WAERK'.
LV_CURRENCY_FIELD_CURR = 'VBAK~WAERK'.
LV_CURRENCY_FROM = 'EUR'.
LV_RATE_CONV = 'M'.
LV_CURR_CONV_DATE_FIELD = 'LV_CURRENCY_CONV_DATE'.
LV_SOURCE_CUKY_FIELD = 'LV_WAVWR_CUKY_SOURCE'.
LV_TARGET_CUKY_FIELD = 'LV_TARGET_CUKY'.
LV_EXC_RATE_TYPE_FIELD = 'LV_EXC_RATE_TYPE'.
CLEAR: LT_CURR.
IF T_DATA[] IS NOT INITIAL.
  DELETE ADJACENT DUPLICATES FROM T_DATA[] COMPARING (LV_CURRENCY_FIELD)
.
ENDIF.
* Set 'CURRENCY_CONV_DATE' parameter
ASSIGN (LV_CURR_CONV_DATE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_FIELD_DATE.
  LV_FIELD_DATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
IF LV_TYPE_OF_RATE IS INITIAL.
  LV_TYPE_OF_RATE = 'M'.
ENDIF.
* Set 'EXP_RATE_TYPE' Parameter field
ASSIGN (LV_EXC_RATE_TYPE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_TYPE_OF_RATE.
  LV_TYPE_OF_RATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
*** Get Document currency(Local) by currency and date field
IF T_DATA[] IS NOT INITIAL AND
   LT_RETURN IS INITIAL.
  CLEAR: LT_SORT_BY, LWA_OUT_WHERE_COND,
         LT_OUT_WHERE_COND, LT_OPTIONS.
*  IF lv_currency_field IS NOT INITIAL.
*    DELETE ADJACENT DUPLICATES FROM t_data
*      COMPARING (lv_currency_field).
*  ENDIF.
*************************** TARGET AMOUNT
**************************************
* Set Target Amount Parameter field(Alert)
  IF LV_AMOUNT_FIELD_FR IS NOT INITIAL.
    CONCATENATE 'R_' LV_AMOUNT_FIELD_FR INTO LV_RESULT_TXT.
    IF LV_RESULT_TXT IS NOT INITIAL.
      ASSIGN (LV_RESULT_TXT) TO <FS_AMOUNT_TAB>.
    ENDIF.
  ENDIF.
*************************** TARGET AMOUNT
**************************************
  CLEAR: LV_RESULT_TXT.
*************************** TARGET CUKY
**************************************
  IF LV_CURRENCY_FIELD_FR IS NOT INITIAL.
* Set Target CUKY Parameter field
    CONCATENATE 'LV_' LV_CURRENCY_FIELD_FR INTO LV_RESULT_TXT.
    IF LV_RESULT_TXT IS NOT INITIAL.
      ASSIGN (LV_RESULT_TXT) TO <FS_CURRENCY_STR>.
    ENDIF.
  ENDIF.
*************************** TARGET CUKY
**************************************
  IF <FS_AMOUNT_TAB> IS ASSIGNED AND
     <FS_AMOUNT_TAB> IS NOT INITIAL.
    LV_OPEN = 'X'.
* Loop on Local Currency table data
    LOOP AT T_DATA.
      CLEAR: LT_OPTION.
      LOOP AT <FS_AMOUNT_TAB> ASSIGNING <FS_AMOUNT_STR>.
        CLEAR: LS_OPTION, LV_QUERY_CURR, LV_TEXT1,
               LV_AMOUNT_TO, LV_TEXT2, LV_VAL, LV_RC,
               LV_AMOUNT_FROM, LV_CURR_FROM, LV_CURR_TO,
               LV_SIGN, LV_OPTION, LV_DATE_CONV,
               LV_LOW, LV_HIGH.
******
* Set SIGN amount for conversion
        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <FS_AMOUNT_STR>
          TO <FS_FIELD>.
        IF SY-SUBRC = 0 AND
            <FS_FIELD> IS ASSIGNED.
          LV_SIGN = <FS_FIELD>.
          UNASSIGN <FS_FIELD>.
        ENDIF.
******
* Set LOW amount for conversion
        ASSIGN COMPONENT 'LOW' OF STRUCTURE <FS_AMOUNT_STR>
          TO <FS_FIELD>.
        IF SY-SUBRC = 0 AND
            <FS_FIELD> IS ASSIGNED.
          LV_AMOUNT_FROM = <FS_FIELD>.
          UNASSIGN <FS_FIELD>.
        ENDIF.
        IF LV_AMOUNT_FROM NE 0 AND
             LV_AMOUNT_FROM IS NOT INITIAL.
******
* Currency value from condition setting
* Default setting(from AG)
          IF LV_CURRENCY_FROM IS NOT INITIAL.
            LV_CURR_FROM = LV_CURRENCY_FROM.
          ENDIF.
          IF <FS_CURRENCY_STR> IS ASSIGNED AND
             <FS_CURRENCY_STR> IS NOT INITIAL.
* Set Local Currency conversion
            LV_CURR_FROM = <FS_CURRENCY_STR>.
          ENDIF.
******
* Currency value of the original document
          IF LV_CURRENCY_TO IS INITIAL.
            IF LV_CURRENCY_FIELD IS NOT INITIAL.
              ASSIGN COMPONENT LV_CURRENCY_FIELD
                OF STRUCTURE T_DATA TO <FS_VAL>.
            ENDIF.
            IF SY-SUBRC IS INITIAL AND
                <FS_VAL> IS ASSIGNED.
              LV_CURR_TO = <FS_VAL>.
              UNASSIGN <FS_VAL>.
            ENDIF.
          ELSE.
            LV_CURR_TO = LV_CURRENCY_TO.
          ENDIF.
* Set Currency Conversion Date Parameter field
          IF LV_DATE_CONV IS INITIAL.
            IF LV_CURRENCY_CONV_DATE IS NOT INITIAL.
              LV_FIELD_DATE = LV_CURRENCY_CONV_DATE.
            ENDIF.
            IF LV_DATE_FIELD IS NOT INITIAL AND
                NOT LV_FIELD_DATE CA '0123456789'.
              ASSIGN COMPONENT LV_DATE_FIELD
              OF STRUCTURE T_DATA TO <FS_VAL>.
              IF SY-SUBRC IS INITIAL
                  AND <FS_VAL> IS ASSIGNED.
                LV_DATE_CONV = <FS_VAL>.
                UNASSIGN <FS_VAL>.
              ENDIF.
            ELSE.
              LV_DATE_CONV = SY-DATLO.
            ENDIF.
          ENDIF.
          IF LV_CURR_TO NE LV_CURR_FROM AND
               LV_CURR_FROM IS NOT INITIAL AND
                 LV_CURR_TO IS NOT INITIAL.
            CLEAR: LV_EXC_RATE_CONV, LV_AMOUNT_TO.
* Unit conversion for LOW
            CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
              EXPORTING
*               CLIENT           = SY-MANDT
                DATE             = LV_DATE_CONV
                FOREIGN_CURRENCY = LV_CURR_TO
                LOCAL_AMOUNT     = LV_AMOUNT_FROM
                LOCAL_CURRENCY   = LV_CURR_FROM
                TYPE_OF_RATE     = LV_RATE_CONV
                SW_DEST          = LV_SW_DEST
              IMPORTING
                EXCHANGE_RATE    = LV_EXC_RATE_CONV
                FOREIGN_AMOUNT   = LV_AMOUNT_TO
              EXCEPTIONS
                NO_RATE_FOUND    = 1
                OVERFLOW         = 2
                NO_FACTORS_FOUND = 3
                NO_SPREAD_FOUND  = 4
                DERIVED_2_TIMES  = 5.
            IF SY-SUBRC IS NOT INITIAL OR
                 LV_EXC_RATE_CONV IS INITIAL.
* Implement suitable error handling here
              CONTINUE.
            ELSEIF LV_AMOUNT_TO IS INITIAL.
              LV_AMOUNT_TO = ABS( LV_AMOUNT_FROM / LV_EXC_RATE_CONV ).
            ENDIF.
          ELSE.
            LV_AMOUNT_TO = LV_AMOUNT_FROM.
          ENDIF.
          IF LV_AMOUNT_TO IS NOT INITIAL.
            IF LV_INSERT_CURR IS INITIAL.
              LV_INSERT_CURR = 'X'.
            ENDIF.
            LOOP AT LT_CURR INTO LS_CURR WHERE WAERS EQ LV_CURR_TO.
              IF LV_AMOUNT_TO EQ LS_CURR-AMOUNT.
                IF LV_DATE_FIELD IS NOT INITIAL.
                  IF LV_DATE_CONV EQ LS_CURR-DATE.
                    LV_CONT = 'X'.
                    EXIT.
                  ENDIF.
                ELSE.
                  LV_CONT = 'X'.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.
            IF SY-SUBRC IS NOT INITIAL OR LV_CONT IS INITIAL.
              LS_CURR-AMOUNT = LV_AMOUNT_TO.
              LS_CURR-WAERS  = LV_CURR_TO.
              IF LV_DATE_FIELD IS NOT INITIAL.
                LS_CURR-DATE   = LV_DATE_CONV.
              ENDIF.
              APPEND LS_CURR TO LT_CURR.
            ELSEIF LV_CONT EQ 'X'.
              CLEAR LV_CONT.
              CONTINUE.
            ENDIF.
            LV_LOW = LV_AMOUNT_TO.
            CONDENSE LV_LOW.
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
******* Date of Currency conversion
            IF LV_DATE_FIELD_CURR IS NOT INITIAL.
              CONCATENATE '''' LV_DATE_CONV '''' INTO LV_VAL.
              CONCATENATE LV_DATE_FIELD_CURR 'EQ' LV_VAL
                INTO LV_QUERY_DATE SEPARATED BY SPACE.
              CONCATENATE LV_QUERY_DATE 'AND' INTO
                LS_OPTION-TEXT SEPARATED BY SPACE.
              APPEND LS_OPTION TO LT_OPTION.
            ENDIF.
            CLEAR: LS_OPTION, LV_QUERY_DATE, LV_VAL.
******* Date of Currency conversion
******* Reference Field(CUKY) of Currency
            IF LV_CURRENCY_FIELD_CURR IS NOT INITIAL AND
               LV_AMOUNT_FIELD_CURR   IS NOT INITIAL.
              CONCATENATE '''' LV_CURR_TO '''' INTO LV_QUERY_CURR.
              LV_TEXT1 = LV_CURRENCY_FIELD_CURR.
              CONCATENATE LV_TEXT1 'EQ' LV_QUERY_CURR INTO LV_TEXT2
                SEPARATED BY SPACE.
******* Reference Field(CUKY) of Currency
******* Foreign Amount value
              LV_VAL = LV_AMOUNT_TO.
              SHIFT LV_VAL RIGHT DELETING TRAILING SPACE.
              SHIFT LV_VAL LEFT DELETING LEADING SPACE.
              CONCATENATE '''' LV_VAL '''' INTO LV_VAL
                IN CHARACTER MODE.
******* Foreign Amount value
              ASSIGN COMPONENT 'OPTION' OF STRUCTURE <FS_AMOUNT_STR>
                TO <FS_FIELD>.
              CLEAR LV_TEXT1.
              LV_TEXT1 = LV_AMOUNT_FIELD_CURR.
              IF <FS_FIELD> IS ASSIGNED AND
                   <FS_FIELD> IS NOT INITIAL.
                LV_OPTION = <FS_FIELD>.
                IF LV_OPTION EQ 'BT'.
                  LV_TEXT_OPT = 'BETWEEN'.
                ELSE.
                  LV_TEXT_OPT = <FS_FIELD>.
                ENDIF.
                CONCATENATE LV_TEXT2 'AND'
                            LV_TEXT1 LV_TEXT_OPT LV_VAL
                  INTO LS_OPTION-TEXT SEPARATED BY SPACE.
                UNASSIGN <FS_FIELD>.
              ENDIF.
****** Conversion 'HIGH' value
              CLEAR: LV_AMOUNT_FROM.
              IF <FS_AMOUNT_STR> IS ASSIGNED AND
                   <FS_AMOUNT_STR> IS NOT INITIAL.
                ASSIGN COMPONENT 'HIGH' OF STRUCTURE <FS_AMOUNT_STR>
                  TO <FS_FIELD>.
                IF SY-SUBRC = 0 AND <FS_FIELD> IS ASSIGNED.
                  LV_AMOUNT_FROM = <FS_FIELD>.
                ENDIF.
              ENDIF.
              IF LV_AMOUNT_FROM NE 0 AND
                   LV_AMOUNT_FROM IS NOT INITIAL.
                CLEAR: LV_AMOUNT_TO.
                IF LV_CURR_FROM NE LV_CURR_TO  AND
                   LV_CURR_FROM IS NOT INITIAL AND
                   LV_CURR_TO   IS NOT INITIAL.
                  CLEAR: LV_EXC_RATE_CONV, LV_AMOUNT_TO.
                  CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
                    EXPORTING
*                     CLIENT           = SY-MANDT
                      DATE             = LV_DATE_CONV
                      FOREIGN_CURRENCY = LV_CURR_TO
                      LOCAL_AMOUNT     = LV_AMOUNT_FROM
                      LOCAL_CURRENCY   = LV_CURR_FROM
                      TYPE_OF_RATE     = LV_RATE_CONV
                      SW_DEST          = LV_SW_DEST
                    IMPORTING
                      EXCHANGE_RATE    = LV_EXC_RATE_CONV
                      FOREIGN_AMOUNT   = LV_AMOUNT_TO
                    EXCEPTIONS
                      NO_RATE_FOUND    = 1
                      OVERFLOW         = 2
                      NO_FACTORS_FOUND = 3
                      NO_SPREAD_FOUND  = 4
                      DERIVED_2_TIMES  = 5.
                  IF SY-SUBRC IS INITIAL AND LV_AMOUNT_TO IS INITIAL.
                    LV_AMOUNT_TO = ABS( LV_AMOUNT_FROM /
LV_EXC_RATE_CONV ).
                  ENDIF.
                ELSE.
                  LV_AMOUNT_TO = LV_AMOUNT_FROM.
                ENDIF.
                IF SY-SUBRC IS INITIAL AND
                     LV_AMOUNT_TO IS NOT INITIAL.
                  LV_HIGH = LV_AMOUNT_TO.
                  CONDENSE LV_HIGH.
                  LV_VAL = LV_AMOUNT_TO.
                  CONDENSE LV_VAL.
                  CONCATENATE '''' LV_VAL '''' INTO LV_VAL.
                  CONCATENATE LS_OPTION-TEXT 'AND' LV_VAL
                    INTO LS_OPTION-TEXT SEPARATED BY SPACE.
                ELSE.
                  CONTINUE.
                ENDIF.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF LV_OPEN IS INITIAL.
            CONCATENATE LS_OPTION-TEXT ')' INTO LS_OPTION-TEXT
              SEPARATED BY SPACE.
          ENDIF.
          IF LS_OPTION IS NOT INITIAL.
            LV_LINES = LV_LINES + 1.
            APPEND LS_OPTION TO LT_OPTION.
            IF LV_LINES GT 1.
              LS_OPTION-TEXT = 'OR ('.
              APPEND LS_OPTION TO LT_OPTIONS.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF LT_OPTION TO LT_OPTIONS.
      CLEAR: LS_OPTION, LT_OPTION.
    ENDLOOP.
    IF LV_CLOSE IS INITIAL.
      LS_OPTION-TEXT = ')'.
      APPEND LS_OPTION TO LT_OPTIONS.
    ENDIF.
  ENDIF.
  IF LV_INSERT_CURR IS NOT INITIAL.
    APPEND LINES OF LT_OPTIONS TO LT_OPTIONS_CURR.
    DESCRIBE TABLE LT_OPTIONS      LINES LV_LINES_OPT.
    DESCRIBE TABLE LT_OPTIONS_CURR LINES LV_LINES_CURR.
    IF LV_AMOUNT_FIELD_CURR CS '~'.
      SPLIT LV_AMOUNT_FIELD_CURR AT '~' INTO LV_PREF LV_SUFF.
    ELSE.
      LV_SUFF = LV_AMOUNT_FIELD_CURR.
    ENDIF.
    LS_COND_CURR-CURR_FIELD = LV_SUFF.
    IF LV_LINES_CURR IS INITIAL.
      LS_COND_CURR-LINE_BEGIN = 1.
    ELSE.
      LS_COND_CURR-LINE_BEGIN = LV_LINES_CURR - LV_LINES_OPT + 1.
    ENDIF.
    LS_COND_CURR-LINE_END = LS_COND_CURR-LINE_BEGIN + LV_LINES_OPT - 1.
    APPEND LS_COND_CURR TO LT_COND_CURR.
    CLEAR: LV_INSERT_CURR, LV_PREF, LV_SUFF,
           LV_LINES_OPT, LV_LINES_CURR,
           LS_COND_CURR, LS_OPTION.
  ENDIF.
ENDIF.
****** Currency conversion **************
CLEAR IS_ALERT.
REFRESH T_DATA.
REFRESH LT_SORT_OPTIONS.
REFRESH LT_OUT_WHERE_COND.
REFRESH LT_GROUP_BY_OPTIONS.
CLEAR LT_DATA.
CLEAR LT_DATA_RFC.
CLEAR LT_SEL_FIELDS.
CLEAR LT_RETURN.
##NO_HANDLER
_APPEND_TABLES_LIST 'VBAK' '' 'A'.
##NO_HANDLER
_APPEND_TABLES_LIST 'VBAP' '' 'B'.
LV_LEFTTAB = 'A'.
LV_LEFTFIELD = 'VBELN'.
LV_RIGHTTAB = 'B'.
LV_RIGHTFIELD = 'VBELN'.
_JOIN_CONDITION LV_LEFTTAB LV_LEFTFIELD LV_RIGHTTAB LV_RIGHTFIELD.
IF LT_OPTIONS_MAIN IS NOT INITIAL.
CLEAR LT_OPTIONS.
LT_OPTIONS = LT_OPTIONS_MAIN.
ENDIF.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'MANDT'.
LS_SEL_FIELDS-ALIAS = 'VBAK_MANDT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'VBELN'.
LS_SEL_FIELDS-ALIAS = 'VBAK_VBELN'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'POSNR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'ERDAT'.
LS_SEL_FIELDS-ALIAS = 'VBAK_ERDAT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'ERZET'.
LS_SEL_FIELDS-ALIAS = 'VBAK_ERZET'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'ERNAM'.
LS_SEL_FIELDS-ALIAS = 'VBAK_ERNAM'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'AUDAT'.
LS_SEL_FIELDS-ALIAS = 'VBAK_AUDAT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'VBTYP'.
LS_SEL_FIELDS-ALIAS = 'VBAK_VBTYP'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'AUART'.
LS_SEL_FIELDS-ALIAS = 'VBAK_AUART'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'AUGRU'.
LS_SEL_FIELDS-ALIAS = 'VBAK_AUGRU'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'LIFSK'.
LS_SEL_FIELDS-ALIAS = 'VBAK_LIFSK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'WAERK'.
LS_SEL_FIELDS-ALIAS = 'VBAK_WAERK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'VKORG'.
LS_SEL_FIELDS-ALIAS = 'VBAK_VKORG'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'VTWEG'.
LS_SEL_FIELDS-ALIAS = 'VBAK_VTWEG'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'VKGRP'.
LS_SEL_FIELDS-ALIAS = 'VBAK_VKGRP'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'VKBUR'.
LS_SEL_FIELDS-ALIAS = 'VBAK_VKBUR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'GSBER'.
LS_SEL_FIELDS-ALIAS = 'VBAK_GSBER'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'VDATU'.
LS_SEL_FIELDS-ALIAS = 'VBAK_VDATU'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'VSBED'.
LS_SEL_FIELDS-ALIAS = 'VBAK_VSBED'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'BSTNK'.
LS_SEL_FIELDS-ALIAS = 'VBAK_BSTNK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'BSARK'.
LS_SEL_FIELDS-ALIAS = 'VBAK_BSARK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'BSTDK'.
LS_SEL_FIELDS-ALIAS = 'VBAK_BSTDK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'BNAME'.
LS_SEL_FIELDS-ALIAS = 'VBAK_BNAME'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'TELF1'.
LS_SEL_FIELDS-ALIAS = 'VBAK_TELF1'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'KUNNR'.
LS_SEL_FIELDS-ALIAS = 'VBAK_KUNNR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'KOSTL'.
LS_SEL_FIELDS-ALIAS = 'VBAK_KOSTL'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'AEDAT'.
LS_SEL_FIELDS-ALIAS = 'VBAK_AEDAT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'KVGR1'.
LS_SEL_FIELDS-ALIAS = 'VBAK_KVGR1'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'KVGR2'.
LS_SEL_FIELDS-ALIAS = 'VBAK_KVGR2'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'KOKRS'.
LS_SEL_FIELDS-ALIAS = 'VBAK_KOKRS'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'PS_PSP_PNR'.
LS_SEL_FIELDS-ALIAS = 'VBAK_PS_PSP_PNR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'ABRVW'.
LS_SEL_FIELDS-ALIAS = 'VBAK_ABRVW'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'BUKRS_VF'.
LS_SEL_FIELDS-ALIAS = 'VBAK_BUKRS_VF'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'XBLNR'.
LS_SEL_FIELDS-ALIAS = 'VBAK_XBLNR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'ZUONR'.
LS_SEL_FIELDS-ALIAS = 'VBAK_ZUONR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'A'.
LS_SEL_FIELDS-FIELD = 'AUFNR'.
LS_SEL_FIELDS-ALIAS = 'VBAK_AUFNR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MATNR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MATKL'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'ARKTX'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'PSTYV'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'LFREL'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'FKREL'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'UEPOS'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'ABGRU'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'PRODH'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MEINS'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'FAKSP'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'NETWR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'WAERK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'KWMENG'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'LSMENG'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'KBMENG'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'KLMENG'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VRKME'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'UMVKZ'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'UMVKN'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'BRGEW'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'NTGEW'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'GEWEI'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VOLUM'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VOLEH'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VGBEL'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VGPOS'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'WERKS'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'LGORT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VSTEL'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'ROUTE'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'ERDAT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'ERNAM'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'NETPR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'KPEIN'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'KMEIN'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'SHKZG'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'KONDM'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'KTGRM'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'BWTAR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'WAVWR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'AEDAT'.
LS_SEL_FIELDS-ALIAS = 'VBAP_AEDAT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'PRCTR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MVGR1'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MVGR2'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MVGR3'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MVGR4'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MVGR5'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'SOBKZ'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'PS_PSP_PNR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'AUFNR'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VPMAT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VPWRK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'CEPOK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'KDMAT'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'MPROK'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
CLEAR LS_SEL_FIELDS.
LS_SEL_FIELDS-TABLE = 'B'.
LS_SEL_FIELDS-FIELD = 'VGTYP'.
APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
IF LT_COND_CURR IS NOT INITIAL.
LOOP AT LT_COND_CURR INTO LS_COND_CURR.
CLEAR: LS_SEL_FIELDS, LT_DATA_TMP.
LT_OPTIONS = LT_OPTIONS_MAIN.
IF LT_OPTIONS IS NOT INITIAL.
  LS_OPTION-TEXT = 'AND ('.
  APPEND LS_OPTION TO LT_OPTIONS.
ENDIF.
LOOP AT LT_OPTIONS_CURR INTO LS_OPTION
  FROM LS_COND_CURR-LINE_BEGIN TO LS_COND_CURR-LINE_END.
  APPEND LS_OPTION TO LT_OPTIONS.
ENDLOOP.
CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
  DESTINATION LV_SW_DEST
*   EXPORTING
*     delimiter            = ' '
*     NO_DATA              = ' '
*     ROWSKIPS             = 0
*     rowcount             = im_number
*     REC_CNT_ONLY         =
  IMPORTING
    ##NEEDED
    ROWCOUNT             = LV_ROWCOUNT
  TABLES
    OPTIONS              = LT_OPTIONS
    DATA                 = LT_DATA
    TABLES_LIST          = LT_TABLES_LIST ##ENH_OK
    JOIN_CONDITION       = LT_JOIN_CONDITION ##ENH_OK
    SEL_FIELDS           = LT_SEL_FIELDS ##ENH_OK
    SORT_OPTIONS         = LT_SORT_OPTIONS ##ENH_OK
    GROUP_BY_OPTIONS     = LT_GROUP_BY_OPTIONS ##ENH_OK
    HAVING_OPTIONS       = LT_HAVING_OPTIONS ##ENH_OK
    OUTPUT_FIELDS        = LT_OUTPUT_FIELDS ##ENH_OK
    DFIES                = LT_DFIES ##ENH_OK
    RETURN               = LT_RETURN ##ENH_OK
    ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB ##ENH_OK
    ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND ##ENH_OK
    ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES ##ENH_OK
  EXCEPTIONS
    TABLE_NOT_AVAILABLE  = 1
    TABLE_WITHOUT_DATA   = 2
    OPTION_NOT_VALID     = 3
    FIELD_NOT_VALID      = 4
    NOT_AUTHORIZED       = 5
    DATA_BUFFER_EXCEEDED = 6
    OTHERS               = 7.
CLEAR: LV_ROWCOUNT, LT_JOIN_CONDITION, LT_SEL_FIELDS,
       LT_SORT_OPTIONS, LT_GROUP_BY_OPTIONS, LT_TABLES_LIST.
IF SY-SUBRC IS NOT INITIAL OR LT_RETURN IS NOT INITIAL.
CLEAR LT_DATA_RFC.
ELSE.
_RFC_TO_T_DATA_INDEX LT_DATA LT_DATA_TMP LT_OUTPUT_FIELDS 2.
IF LT_DATA_TMP[] IS NOT INITIAL.
APPEND LINES OF LT_DATA_TMP[] TO T_DATA[].
ENDIF.
ENDIF.
ENDLOOP.
ELSE.
CLEAR: LS_SEL_FIELDS, LT_DATA_TMP.
CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
  DESTINATION LV_SW_DEST
*   EXPORTING
*     delimiter            = ' '
*     NO_DATA              = ' '
*     ROWSKIPS             = 0
*     rowcount             = im_number
*     REC_CNT_ONLY         =
  IMPORTING
    ##NEEDED
    ROWCOUNT             = LV_ROWCOUNT
  TABLES
    OPTIONS              = LT_OPTIONS
    DATA                 = LT_DATA
    TABLES_LIST          = LT_TABLES_LIST ##ENH_OK
    JOIN_CONDITION       = LT_JOIN_CONDITION ##ENH_OK
    SEL_FIELDS           = LT_SEL_FIELDS ##ENH_OK
    SORT_OPTIONS         = LT_SORT_OPTIONS ##ENH_OK
    GROUP_BY_OPTIONS     = LT_GROUP_BY_OPTIONS ##ENH_OK
    HAVING_OPTIONS       = LT_HAVING_OPTIONS ##ENH_OK
    OUTPUT_FIELDS        = LT_OUTPUT_FIELDS ##ENH_OK
    DFIES                = LT_DFIES ##ENH_OK
    RETURN               = LT_RETURN ##ENH_OK
    ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB ##ENH_OK
    ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND ##ENH_OK
    ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES ##ENH_OK
  EXCEPTIONS
    TABLE_NOT_AVAILABLE  = 1
    TABLE_WITHOUT_DATA   = 2
    OPTION_NOT_VALID     = 3
    FIELD_NOT_VALID      = 4
    NOT_AUTHORIZED       = 5
    DATA_BUFFER_EXCEEDED = 6
    OTHERS               = 7.
CLEAR: LV_ROWCOUNT, LT_JOIN_CONDITION, LT_SEL_FIELDS,
       LT_SORT_OPTIONS, LT_GROUP_BY_OPTIONS, LT_TABLES_LIST.
IF SY-SUBRC IS NOT INITIAL OR LT_RETURN IS NOT INITIAL.
CLEAR LT_DATA_RFC.
ELSE.
_RFC_TO_T_DATA_INDEX LT_DATA LT_DATA_TMP LT_OUTPUT_FIELDS 3.
IF LT_DATA_TMP[] IS NOT INITIAL.
APPEND LINES OF LT_DATA_TMP[] TO T_DATA[].
ENDIF.
ENDIF.
ENDIF.
CHECK T_DATA[] IS NOT INITIAL.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: D_FROM.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: T_FROM.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: D_TO.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: T_TO.
* The parameter field 'lv_date_ref_fld'
* and 'lv_time_ref_fld' is declared
* at '/SKN/T_AR_FIELDS' custom. table
* and is initialized on the user screen
##NEEDED
DATA: SY_TABIX LIKE SY-TABIX .
##NEEDED
FIELD-SYMBOLS:  TYPE ANY,
##NEEDED
               <FS_DURATION> TYPE ANY,
##NEEDED
               <FS_DU>       TYPE ANY.
CLEAR: LV_FLD, SY_TABIX.
LV_T_FROM = SY_TIMLO.
LV_D_TO   = SY_DATLO.
LV_T_TO   = SY_TIMLO.
*-- Calculate Status Duration
LOOP AT T_DATA.
  SY_TABIX = SY-TABIX.
* Set field 'date_from' by date reference field
* which is determined on the user screen
  CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO LV_FLD.
  ASSIGN (LV_FLD) TO .
  IF  IS NOT ASSIGNED.
    CONTINUE.
  ELSE.
    LV_D_FROM = .
    UNASSIGN .
  ENDIF.
  CLEAR: LV_FLD.
* Set field 'time_from' by time reference field
* which is determined on the user screen
  IF LV_TIME_REF_FLD IS NOT INITIAL.
    CONCATENATE 'T_DATA-' LV_TIME_REF_FLD INTO LV_FLD.
    ASSIGN (LV_FLD) TO .
    IF  IS ASSIGNED.
      LV_T_FROM = .
    ENDIF.
  ENDIF.
  IF NOT LV_D_FROM IS INITIAL.
    ASSIGN COMPONENT 'DURATION_UNIT' OF STRUCTURE T_DATA TO <FS_DU>.
    IF SY-SUBRC EQ 0 AND <FS_DU> IS ASSIGNED.
*      t_data-duration_unit = lv_duration_unit.
      <FS_DU> = LV_DURATION_UNIT.
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = LV_D_FROM
        T_FROM      = LV_T_FROM
        D_TO        = LV_D_TO
        T_TO        = LV_T_TO
        TIME_UNIT   = LV_DURATION_UNIT
      IMPORTING
        TIME_DIFF   = LV_TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT 'DURATION' OF STRUCTURE T_DATA TO <FS_DURATION>.
      IF SY-SUBRC EQ 0 AND <FS_DURATION> IS ASSIGNED.
        <FS_DURATION> = LV_TIME_DIFF.
      ENDIF.
    ELSE.
      ASSIGN COMPONENT 'DURATION' OF STRUCTURE T_DATA TO <FS_DURATION>.
      IF SY-SUBRC EQ 0 AND <FS_DURATION> IS ASSIGNED.
        <FS_DURATION> = '999999'.
      ENDIF.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDIF.
ENDLOOP.
DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
LOOP AT T_DATA.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'FAKSP'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'FAKSP'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: FAKSK.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: SW_DEST2.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_FAKSK, LV_BLOCK_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_FAKSK = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_BIL_BLOCK_DESC'
      EXPORTING
        FAKSK      = LV_FAKSK
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        BLOCK_DESC = LV_BLOCK_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_BLOCK_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'MATKL'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'MATKL'.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_MATKL, LV_MATKL_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_MATKL = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
      EXPORTING
        MATKL      = LV_MATKL
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        MATKL_DESC = LV_MATKL_DESC
*       MATKL_DESC_L       =
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC IS INITIAL.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_MATKL_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'MATNR'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'MATNR'.
* The parameter 'lv_fieldname' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_MATNR, LV_MATERIAL_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_MATNR = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
      EXPORTING
        MATNR         = LV_MATNR
        LANGU         = LV_LANGU
        SW_DEST       = LV_SW_DEST
      IMPORTING
        MATERIAL_DESC = LV_MATERIAL_DESC
      EXCEPTIONS
        WRONG_CODE    = 1
        OTHERS        = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_MATERIAL_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
CLEAR LV_FIELD_AMT.
CLEAR LV_FIELD_DATE.
CLEAR LV_LOCAL_CURRENCY.
CLEAR LV_FIELD_LCURR.
CLEAR LV_FOREIGN_CURRENCY.
LV_FIELD_AMT = 'NETPR'.
LV_FIELD_DATE = SY_DATLO.
LV_FIELD_LCURR = 'WAERK'.
LV_FOREIGN_CURRENCY = 'EUR'.
LV_SOURCE_CUKY_FIELD = 'LV_NETPR_CUKY_SOURCE'.
LV_TARGET_CUKY_FIELD = 'LV_TARGET_CUKY'.
LV_TYPE_OF_RATE = 'M'.
LV_EXC_RATE_TYPE_FIELD = 'LV_EXC_RATE_TYPE'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: DATE.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: FOREIGN_CURRENCY.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: LOCAL_AMOUNT.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: LOCAL_CURRENCY.
IF <FS_FIELD> IS ASSIGNED.
  UNASSIGN <FS_FIELD>.
ENDIF.
IF <FS_VAL> IS ASSIGNED.
  UNASSIGN: <FS_VAL>.
ENDIF.
* Set AI 'CURRENCY_CONV_DATE' parameter
IF LV_CURR_CONV_DATE_FIELD IS NOT INITIAL.
  ASSIGN (LV_CURR_CONV_DATE_FIELD) TO <FS_FIELD>.
  IF <FS_FIELD> IS ASSIGNED    AND
     <FS_FIELD> IS NOT INITIAL AND
     <FS_FIELD> NE LV_FIELD_DATE.
    LV_FIELD_DATE = <FS_FIELD>.
    UNASSIGN <FS_FIELD>.
  ENDIF.
ENDIF.
* Set currency date value
IF LV_FIELD_DATE IS NOT INITIAL.
  IF NOT LV_FIELD_DATE CA '0123456789'.
    CONCATENATE 'T_DATA-' LV_FIELD_DATE INTO LV_FLD.
    ASSIGN (LV_FLD) TO <FS_VAL>.
  ENDIF.
  IF <FS_VAL> IS ASSIGNED.
    LV_DATE = <FS_VAL>.
    UNASSIGN <FS_VAL>.
  ELSE.
    LV_DATE = LV_FIELD_DATE.
  ENDIF.
ENDIF.
IF LV_TYPE_OF_RATE IS INITIAL.
  LV_TYPE_OF_RATE = 'M'.
ENDIF.
* Set AI 'EXP_RATE_TYPE' parameter
ASSIGN (LV_EXC_RATE_TYPE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_TYPE_OF_RATE.
  LV_TYPE_OF_RATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
* Set AI Parameter value for Source Currency
ASSIGN (LV_SOURCE_CUKY_FIELD) TO <FS_CURRENCY_STR>.
IF <FS_CURRENCY_STR> IS ASSIGNED    AND
   <FS_CURRENCY_STR> IS NOT INITIAL AND
   <FS_CURRENCY_STR> NE LV_FOREIGN_CURRENCY.
  LV_LOCAL_CURRENCY = <FS_CURRENCY_STR>.
  UNASSIGN <FS_CURRENCY_STR>.
ENDIF.
* Set AI Parameter value for Target Currency
ASSIGN (LV_TARGET_CUKY_FIELD) TO <FS_CURRENCY_STR>.
IF <FS_CURRENCY_STR> IS ASSIGNED    AND
   <FS_CURRENCY_STR> IS NOT INITIAL AND
   <FS_CURRENCY_STR> NE LV_FOREIGN_CURRENCY.
  LV_FOREIGN_CURRENCY = <FS_CURRENCY_STR>.
  UNASSIGN <FS_CURRENCY_STR>.
ENDIF.
* Set document source value of amount field
IF LV_FIELD_AMT IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELD_AMT INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED.
    LV_LOCAL_AMOUNT = <FS_VAL>.
    UNASSIGN: <FS_VAL>.
  ENDIF.
ENDIF.
CLEAR: LV_FLD.
* Set document source value of currency field
IF LV_FIELD_LCURR IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELD_LCURR INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED.
    LV_LOCAL_CURRENCY = <FS_VAL>.
    UNASSIGN: <FS_VAL>.
  ENDIF.
ENDIF.
CLEAR: LV_FLD.
IF LV_LOCAL_CURRENCY IS NOT INITIAL.
* Set Local currency to appropriate field
  CONCATENATE LV_FIELD_AMT 'CUKY_SOURCE' INTO LV_FLD
    SEPARATED BY '_'.
  IF LV_FLD IS NOT INITIAL.
    ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
    IF <FS_VAL> IS ASSIGNED.
      <FS_VAL> = LV_LOCAL_CURRENCY.
    ENDIF.
  ENDIF.
  CLEAR: LV_FLD.
ENDIF.
IF <FS_VAL> IS ASSIGNED.
  UNASSIGN: <FS_VAL>.
ENDIF.
IF LV_LOCAL_CURRENCY NE LV_FOREIGN_CURRENCY.
  IF LV_DATE             IS NOT INITIAL AND
     LV_FOREIGN_CURRENCY IS NOT INITIAL AND
     LV_LOCAL_AMOUNT     IS NOT INITIAL AND
     LV_LOCAL_CURRENCY   IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
      EXPORTING
*       CLIENT           = SY-MANDT
        DATE             = LV_DATE
        FOREIGN_CURRENCY = LV_FOREIGN_CURRENCY
        LOCAL_AMOUNT     = LV_LOCAL_AMOUNT
        LOCAL_CURRENCY   = LV_LOCAL_CURRENCY
        TYPE_OF_RATE     = LV_TYPE_OF_RATE
        SW_DEST          = LV_SW_DEST
      IMPORTING
        FOREIGN_AMOUNT   = LV_FOREIGN_AMOUNT
      EXCEPTIONS
        NO_RATE_FOUND    = 1
        OVERFLOW         = 2
        NO_FACTORS_FOUND = 3
        NO_SPREAD_FOUND  = 4
        DERIVED_2_TIMES  = 5.
    IF SY-SUBRC = 0.
* Set Amount value to Target Amount field(Foreign)
      IF LV_FOREIGN_AMOUNT IS NOT INITIAL.
* Set Foreign amount result to appropriate field
        CONCATENATE LV_FIELD_AMT 'FOREIGN' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_AMOUNT.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
* Set Target Currency to appropriate field
        LV_FLD = 'TARGET_CUKY'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
      ENDIF.
      IF LV_FIELD_LCURR IS NOT INITIAL.
* Set Local currency to appropriate field
        LV_FLD = LV_FIELD_LCURR.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_LOCAL_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
* Set Foreign currency to appropriate field
        CONCATENATE LV_FIELD_LCURR 'FOREIGN' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
      ELSE.
* Set Local currency to appropriate field
        CONCATENATE LV_FIELD_AMT 'CUKY_SOURCE' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_LOCAL_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
* Set Target Currency to appropriate field
        LV_FLD = 'TARGET_CUKY'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
CLEAR LV_FIELD_AMT.
CLEAR LV_FIELD_DATE.
CLEAR LV_LOCAL_CURRENCY.
CLEAR LV_FIELD_LCURR.
CLEAR LV_FOREIGN_CURRENCY.
LV_FIELD_AMT = 'NETWR'.
LV_FIELD_DATE = SY_DATLO.
LV_FIELD_LCURR = 'WAERK'.
LV_FOREIGN_CURRENCY = 'EUR'.
LV_SOURCE_CUKY_FIELD = 'LV_NETWR_CUKY_SOURCE'.
LV_TARGET_CUKY_FIELD = 'LV_TARGET_CUKY'.
LV_TYPE_OF_RATE = 'M'.
LV_EXC_RATE_TYPE_FIELD = 'LV_EXC_RATE_TYPE'.
IF <FS_FIELD> IS ASSIGNED.
  UNASSIGN <FS_FIELD>.
ENDIF.
IF <FS_VAL> IS ASSIGNED.
  UNASSIGN: <FS_VAL>.
ENDIF.
* Set AI 'CURRENCY_CONV_DATE' parameter
IF LV_CURR_CONV_DATE_FIELD IS NOT INITIAL.
  ASSIGN (LV_CURR_CONV_DATE_FIELD) TO <FS_FIELD>.
  IF <FS_FIELD> IS ASSIGNED    AND
     <FS_FIELD> IS NOT INITIAL AND
     <FS_FIELD> NE LV_FIELD_DATE.
    LV_FIELD_DATE = <FS_FIELD>.
    UNASSIGN <FS_FIELD>.
  ENDIF.
ENDIF.
* Set currency date value
IF LV_FIELD_DATE IS NOT INITIAL.
  IF NOT LV_FIELD_DATE CA '0123456789'.
    CONCATENATE 'T_DATA-' LV_FIELD_DATE INTO LV_FLD.
    ASSIGN (LV_FLD) TO <FS_VAL>.
  ENDIF.
  IF <FS_VAL> IS ASSIGNED.
    LV_DATE = <FS_VAL>.
    UNASSIGN <FS_VAL>.
  ELSE.
    LV_DATE = LV_FIELD_DATE.
  ENDIF.
ENDIF.
IF LV_TYPE_OF_RATE IS INITIAL.
  LV_TYPE_OF_RATE = 'M'.
ENDIF.
* Set AI 'EXP_RATE_TYPE' parameter
ASSIGN (LV_EXC_RATE_TYPE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_TYPE_OF_RATE.
  LV_TYPE_OF_RATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
* Set AI Parameter value for Source Currency
ASSIGN (LV_SOURCE_CUKY_FIELD) TO <FS_CURRENCY_STR>.
IF <FS_CURRENCY_STR> IS ASSIGNED    AND
   <FS_CURRENCY_STR> IS NOT INITIAL AND
   <FS_CURRENCY_STR> NE LV_FOREIGN_CURRENCY.
  LV_LOCAL_CURRENCY = <FS_CURRENCY_STR>.
  UNASSIGN <FS_CURRENCY_STR>.
ENDIF.
* Set AI Parameter value for Target Currency
ASSIGN (LV_TARGET_CUKY_FIELD) TO <FS_CURRENCY_STR>.
IF <FS_CURRENCY_STR> IS ASSIGNED    AND
   <FS_CURRENCY_STR> IS NOT INITIAL AND
   <FS_CURRENCY_STR> NE LV_FOREIGN_CURRENCY.
  LV_FOREIGN_CURRENCY = <FS_CURRENCY_STR>.
  UNASSIGN <FS_CURRENCY_STR>.
ENDIF.
* Set document source value of amount field
IF LV_FIELD_AMT IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELD_AMT INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED.
    LV_LOCAL_AMOUNT = <FS_VAL>.
    UNASSIGN: <FS_VAL>.
  ENDIF.
ENDIF.
CLEAR: LV_FLD.
* Set document source value of currency field
IF LV_FIELD_LCURR IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELD_LCURR INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED.
    LV_LOCAL_CURRENCY = <FS_VAL>.
    UNASSIGN: <FS_VAL>.
  ENDIF.
ENDIF.
CLEAR: LV_FLD.
IF LV_LOCAL_CURRENCY IS NOT INITIAL.
* Set Local currency to appropriate field
  CONCATENATE LV_FIELD_AMT 'CUKY_SOURCE' INTO LV_FLD
    SEPARATED BY '_'.
  IF LV_FLD IS NOT INITIAL.
    ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
    IF <FS_VAL> IS ASSIGNED.
      <FS_VAL> = LV_LOCAL_CURRENCY.
    ENDIF.
  ENDIF.
  CLEAR: LV_FLD.
ENDIF.
IF <FS_VAL> IS ASSIGNED.
  UNASSIGN: <FS_VAL>.
ENDIF.
IF LV_LOCAL_CURRENCY NE LV_FOREIGN_CURRENCY.
  IF LV_DATE             IS NOT INITIAL AND
     LV_FOREIGN_CURRENCY IS NOT INITIAL AND
     LV_LOCAL_AMOUNT     IS NOT INITIAL AND
     LV_LOCAL_CURRENCY   IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
      EXPORTING
*       CLIENT           = SY-MANDT
        DATE             = LV_DATE
        FOREIGN_CURRENCY = LV_FOREIGN_CURRENCY
        LOCAL_AMOUNT     = LV_LOCAL_AMOUNT
        LOCAL_CURRENCY   = LV_LOCAL_CURRENCY
        TYPE_OF_RATE     = LV_TYPE_OF_RATE
        SW_DEST          = LV_SW_DEST
      IMPORTING
        FOREIGN_AMOUNT   = LV_FOREIGN_AMOUNT
      EXCEPTIONS
        NO_RATE_FOUND    = 1
        OVERFLOW         = 2
        NO_FACTORS_FOUND = 3
        NO_SPREAD_FOUND  = 4
        DERIVED_2_TIMES  = 5.
    IF SY-SUBRC = 0.
* Set Amount value to Target Amount field(Foreign)
      IF LV_FOREIGN_AMOUNT IS NOT INITIAL.
* Set Foreign amount result to appropriate field
        CONCATENATE LV_FIELD_AMT 'FOREIGN' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_AMOUNT.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
* Set Target Currency to appropriate field
        LV_FLD = 'TARGET_CUKY'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
      ENDIF.
      IF LV_FIELD_LCURR IS NOT INITIAL.
* Set Local currency to appropriate field
        LV_FLD = LV_FIELD_LCURR.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_LOCAL_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
* Set Foreign currency to appropriate field
        CONCATENATE LV_FIELD_LCURR 'FOREIGN' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
      ELSE.
* Set Local currency to appropriate field
        CONCATENATE LV_FIELD_AMT 'CUKY_SOURCE' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_LOCAL_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
* Set Target Currency to appropriate field
        LV_FLD = 'TARGET_CUKY'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'PRCTR'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'PRCTR'.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'PS_PSP_PNR'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'PS_PSP_PNR'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: PSPNR.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_PSPNR, LV_WBS_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
* Set field value of the "lv_fieldtab"
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_PSPNR = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_WBS_DESC'
      EXPORTING
        PSPNR          = LV_PSPNR
        SW_DEST        = LV_SW_DEST
      IMPORTING
        WBS_DESC      = LV_WBS_DESC
      EXCEPTIONS
        WRONG_WBS = 1
        OTHERS         = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_WBS_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VBAK_AUART'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VBAK_AUART'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: AUART.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_AUART, LV_TYPE_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
*    LV_AUART = <fs_val>.
    MOVE <FS_VAL> TO LV_AUART.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_DOC_TYPE_DESC'
      EXPORTING
        AUART      = LV_AUART
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        TYPE_DESC  = LV_TYPE_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_TYPE_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VBAK_AUGRU'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VBAK_AUGRU'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: AUGRU.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_AUGRU, LV_AUGRU_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_AUGRU = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_AUGRU_DESC'
      EXPORTING
        AUGRU      = LV_AUGRU
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        AUGRU_DESC = LV_AUGRU_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_AUGRU_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VBAK_BUKRS_VF'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VBAK_BUKRS_VF'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: BUKRS.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_BUKRS, LV_COMP_CODE_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_BUKRS = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
      EXPORTING
        BUKRS          = LV_BUKRS
        SW_DEST        = LV_SW_DEST
      IMPORTING
        COMP_CODE_DESC = LV_COMP_CODE_DESC
      EXCEPTIONS
        WRONG_CODE     = 1
        OTHERS         = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_COMP_CODE_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VBAK_KUNNR'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VBAK_KUNNR'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: KUNNR.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_KUNNR, LV_CUST_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
* Set field value of the "lv_fieldtab"
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_KUNNR = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR          = LV_KUNNR
        SW_DEST        = LV_SW_DEST
      IMPORTING
        CUST_DESC      = LV_CUST_DESC
*       LAND1          =
      EXCEPTIONS
        WRONG_CUSTOMER = 1
        OTHERS         = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_CUST_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VBAK_PS_PSP_PNR'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VBAK_PS_PSP_PNR'.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_PSPNR, LV_WBS_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
* Set field value of the "lv_fieldtab"
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_PSPNR = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_WBS_DESC'
      EXPORTING
        PSPNR          = LV_PSPNR
        SW_DEST        = LV_SW_DEST
      IMPORTING
        WBS_DESC      = LV_WBS_DESC
      EXCEPTIONS
        WRONG_WBS = 1
        OTHERS         = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_WBS_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VBAK_VBTYP'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VBAK_VBTYP'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: VBTYP.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_VBTYP, LV_CAT_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_VBTYP = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_DOC_CAT_DESC'
      EXPORTING
        VBTYP      = LV_VBTYP
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        CAT_DESC   = LV_CAT_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_CAT_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VBAK_VKORG'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VBAK_VKORG'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: VKORG.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_VKORG, LV_SALES_ORG_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_VKORG = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_SALES_ORG_DESC'
      EXPORTING
        VKORG          = LV_VKORG
        LANGU          = LV_LANGU
        SW_DEST        = LV_SW_DEST
      IMPORTING
        SALES_ORG_DESC = LV_SALES_ORG_DESC
      EXCEPTIONS
        WRONG_CODE     = 1
        OTHERS         = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_SALES_ORG_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VBAK_VTWEG'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VBAK_VTWEG'.
##NO_HANDLER
##NEEDED
SELECT_SINGLE: VTWEG.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_VTWEG, LV_DISTR_CHAN_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_VTWEG = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_DISTR_CHAN_DESC'
      EXPORTING
        VTWEG           = LV_VTWEG
        LANGU           = LV_LANGU
        SW_DEST         = LV_SW_DEST
      IMPORTING
        DISTR_CHAN_DESC = LV_DISTR_CHAN_DESC
      EXCEPTIONS
        WRONG_CODE      = 1
        OTHERS          = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_DISTR_CHAN_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VGTYP'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VGTYP'.
* The parameter 'lv_fieldtab' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_VBTYP, LV_CAT_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_VBTYP = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_DOC_CAT_DESC'
      EXPORTING
        VBTYP      = LV_VBTYP
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        CAT_DESC   = LV_CAT_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_CAT_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VPMAT'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VPMAT'.
* The parameter 'lv_fieldname' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_MATNR, LV_MATERIAL_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_MATNR = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
      EXPORTING
        MATNR         = LV_MATNR
        LANGU         = LV_LANGU
        SW_DEST       = LV_SW_DEST
      IMPORTING
        MATERIAL_DESC = LV_MATERIAL_DESC
      EXCEPTIONS
        WRONG_CODE    = 1
        OTHERS        = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_MATERIAL_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'VPWRK'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'VPWRK'.
* The parameter 'lv_fieldname' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_WERKS, LV_PLANT_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
* Set field value of the "lv_fieldtab"
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_WERKS = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = LV_WERKS
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        PLANT_DESC = LV_PLANT_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_PLANT_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
CLEAR LV_FIELD_AMT.
CLEAR LV_FIELD_DATE.
CLEAR LV_LOCAL_CURRENCY.
CLEAR LV_FIELD_LCURR.
CLEAR LV_FOREIGN_CURRENCY.
LV_FIELD_AMT = 'WAVWR'.
LV_FIELD_DATE = SY_DATLO.
LV_FIELD_LCURR = 'WAERK'.
LV_FOREIGN_CURRENCY = 'EUR'.
LV_SOURCE_CUKY_FIELD = 'LV_WAVWR_CUKY_SOURCE'.
LV_TARGET_CUKY_FIELD = 'LV_TARGET_CUKY'.
LV_TYPE_OF_RATE = 'M'.
LV_EXC_RATE_TYPE_FIELD = 'LV_EXC_RATE_TYPE'.
IF <FS_FIELD> IS ASSIGNED.
  UNASSIGN <FS_FIELD>.
ENDIF.
IF <FS_VAL> IS ASSIGNED.
  UNASSIGN: <FS_VAL>.
ENDIF.
* Set AI 'CURRENCY_CONV_DATE' parameter
IF LV_CURR_CONV_DATE_FIELD IS NOT INITIAL.
  ASSIGN (LV_CURR_CONV_DATE_FIELD) TO <FS_FIELD>.
  IF <FS_FIELD> IS ASSIGNED    AND
     <FS_FIELD> IS NOT INITIAL AND
     <FS_FIELD> NE LV_FIELD_DATE.
    LV_FIELD_DATE = <FS_FIELD>.
    UNASSIGN <FS_FIELD>.
  ENDIF.
ENDIF.
* Set currency date value
IF LV_FIELD_DATE IS NOT INITIAL.
  IF NOT LV_FIELD_DATE CA '0123456789'.
    CONCATENATE 'T_DATA-' LV_FIELD_DATE INTO LV_FLD.
    ASSIGN (LV_FLD) TO <FS_VAL>.
  ENDIF.
  IF <FS_VAL> IS ASSIGNED.
    LV_DATE = <FS_VAL>.
    UNASSIGN <FS_VAL>.
  ELSE.
    LV_DATE = LV_FIELD_DATE.
  ENDIF.
ENDIF.
IF LV_TYPE_OF_RATE IS INITIAL.
  LV_TYPE_OF_RATE = 'M'.
ENDIF.
* Set AI 'EXP_RATE_TYPE' parameter
ASSIGN (LV_EXC_RATE_TYPE_FIELD) TO <FS_FIELD>.
IF <FS_FIELD> IS ASSIGNED    AND
   <FS_FIELD> IS NOT INITIAL AND
   <FS_FIELD> NE LV_TYPE_OF_RATE.
  LV_TYPE_OF_RATE = <FS_FIELD>.
  UNASSIGN <FS_FIELD>.
ENDIF.
* Set AI Parameter value for Source Currency
ASSIGN (LV_SOURCE_CUKY_FIELD) TO <FS_CURRENCY_STR>.
IF <FS_CURRENCY_STR> IS ASSIGNED    AND
   <FS_CURRENCY_STR> IS NOT INITIAL AND
   <FS_CURRENCY_STR> NE LV_FOREIGN_CURRENCY.
  LV_LOCAL_CURRENCY = <FS_CURRENCY_STR>.
  UNASSIGN <FS_CURRENCY_STR>.
ENDIF.
* Set AI Parameter value for Target Currency
ASSIGN (LV_TARGET_CUKY_FIELD) TO <FS_CURRENCY_STR>.
IF <FS_CURRENCY_STR> IS ASSIGNED    AND
   <FS_CURRENCY_STR> IS NOT INITIAL AND
   <FS_CURRENCY_STR> NE LV_FOREIGN_CURRENCY.
  LV_FOREIGN_CURRENCY = <FS_CURRENCY_STR>.
  UNASSIGN <FS_CURRENCY_STR>.
ENDIF.
* Set document source value of amount field
IF LV_FIELD_AMT IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELD_AMT INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED.
    LV_LOCAL_AMOUNT = <FS_VAL>.
    UNASSIGN: <FS_VAL>.
  ENDIF.
ENDIF.
CLEAR: LV_FLD.
* Set document source value of currency field
IF LV_FIELD_LCURR IS NOT INITIAL.
  CONCATENATE 'T_DATA-' LV_FIELD_LCURR INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED.
    LV_LOCAL_CURRENCY = <FS_VAL>.
    UNASSIGN: <FS_VAL>.
  ENDIF.
ENDIF.
CLEAR: LV_FLD.
IF LV_LOCAL_CURRENCY IS NOT INITIAL.
* Set Local currency to appropriate field
  CONCATENATE LV_FIELD_AMT 'CUKY_SOURCE' INTO LV_FLD
    SEPARATED BY '_'.
  IF LV_FLD IS NOT INITIAL.
    ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
    IF <FS_VAL> IS ASSIGNED.
      <FS_VAL> = LV_LOCAL_CURRENCY.
    ENDIF.
  ENDIF.
  CLEAR: LV_FLD.
ENDIF.
IF <FS_VAL> IS ASSIGNED.
  UNASSIGN: <FS_VAL>.
ENDIF.
IF LV_LOCAL_CURRENCY NE LV_FOREIGN_CURRENCY.
  IF LV_DATE             IS NOT INITIAL AND
     LV_FOREIGN_CURRENCY IS NOT INITIAL AND
     LV_LOCAL_AMOUNT     IS NOT INITIAL AND
     LV_LOCAL_CURRENCY   IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_CONV_TO_FR_CURR'
      EXPORTING
*       CLIENT           = SY-MANDT
        DATE             = LV_DATE
        FOREIGN_CURRENCY = LV_FOREIGN_CURRENCY
        LOCAL_AMOUNT     = LV_LOCAL_AMOUNT
        LOCAL_CURRENCY   = LV_LOCAL_CURRENCY
        TYPE_OF_RATE     = LV_TYPE_OF_RATE
        SW_DEST          = LV_SW_DEST
      IMPORTING
        FOREIGN_AMOUNT   = LV_FOREIGN_AMOUNT
      EXCEPTIONS
        NO_RATE_FOUND    = 1
        OVERFLOW         = 2
        NO_FACTORS_FOUND = 3
        NO_SPREAD_FOUND  = 4
        DERIVED_2_TIMES  = 5.
    IF SY-SUBRC = 0.
* Set Amount value to Target Amount field(Foreign)
      IF LV_FOREIGN_AMOUNT IS NOT INITIAL.
* Set Foreign amount result to appropriate field
        CONCATENATE LV_FIELD_AMT 'FOREIGN' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_AMOUNT.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
* Set Target Currency to appropriate field
        LV_FLD = 'TARGET_CUKY'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
      ENDIF.
      IF LV_FIELD_LCURR IS NOT INITIAL.
* Set Local currency to appropriate field
        LV_FLD = LV_FIELD_LCURR.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_LOCAL_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
* Set Foreign currency to appropriate field
        CONCATENATE LV_FIELD_LCURR 'FOREIGN' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
      ELSE.
* Set Local currency to appropriate field
        CONCATENATE LV_FIELD_AMT 'CUKY_SOURCE' INTO LV_FLD
          SEPARATED BY '_'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_LOCAL_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
        CLEAR: LV_FLD.
* Set Target Currency to appropriate field
        LV_FLD = 'TARGET_CUKY'.
        IF LV_FLD IS NOT INITIAL.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_FOREIGN_CURRENCY.
            MODIFY T_DATA.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
CLEAR LV_FIELDTAB.
CLEAR LV_FIELDTAB2.
LV_FIELDTAB = 'WERKS'.
IF LV_LANGU IS INITIAL.
LV_LANGU = 'E'.
ENDIF.
LV_DESC_FIELD_PR = 'WERKS'.
* The parameter 'lv_fieldname' is declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
CLEAR: LV_FLD, LV_WERKS, LV_PLANT_DESC.
IF LV_FIELDTAB IS NOT INITIAL.
* Set field value of the "lv_fieldtab"
  CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
  ASSIGN (LV_FLD) TO <FS_VAL>.
  IF <FS_VAL> IS ASSIGNED AND <FS_VAL> IS NOT INITIAL.
    LV_WERKS = <FS_VAL>.
    UNASSIGN <FS_VAL>.
    CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = LV_WERKS
        LANGU      = LV_LANGU
        SW_DEST    = LV_SW_DEST
      IMPORTING
        PLANT_DESC = LV_PLANT_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC EQ 0.
      CLEAR: LV_FLD.
      CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
      ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
      IF SY-SUBRC IS INITIAL AND <FS_VAL> IS ASSIGNED.
        <FS_VAL> = LV_PLANT_DESC.
        MODIFY T_DATA.
        UNASSIGN <FS_VAL>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
MODIFY T_DATA[] FROM  T_DATA.
ENDLOOP.
DELETE T_DATA[] WHERE VBAK_VBTYP_DESC NOT IN  R_VBAK_VBTYP_DESC[].
DELETE T_DATA[] WHERE VBAK_AUART_DESC NOT IN  R_VBAK_AUART_DESC[].
DELETE T_DATA[] WHERE VBAK_AUGRU_DESC NOT IN  R_VBAK_AUGRU_DESC[].
DELETE T_DATA[] WHERE VBAK_VKORG_DESC NOT IN  R_VBAK_VKORG_DESC[].
DELETE T_DATA[] WHERE VBAK_VTWEG_DESC NOT IN  R_VBAK_VTWEG_DESC[].
DELETE T_DATA[] WHERE VBAK_KUNNR_DESC NOT IN  R_VBAK_KUNNR_DESC[].
DELETE T_DATA[] WHERE VBAK_PS_PSP_PNR_DESC NOT IN
R_VBAK_PS_PSP_PNR_DESC[].
DELETE T_DATA[] WHERE VBAK_BUKRS_VF_DESC NOT IN  R_VBAK_BUKRS_VF_DESC[].
DELETE T_DATA[] WHERE MATNR_DESC NOT IN  R_MATNR_DESC[].
DELETE T_DATA[] WHERE MATKL_DESC NOT IN  R_MATKL_DESC[].
DELETE T_DATA[] WHERE FAKSP_DESC NOT IN  R_FAKSP_DESC[].
DELETE T_DATA[] WHERE WERKS_DESC NOT IN  R_WERKS_DESC[].
DELETE T_DATA[] WHERE PRCTR_DESC NOT IN  R_PRCTR_DESC[].
DELETE T_DATA[] WHERE PS_PSP_PNR_DESC NOT IN  R_PS_PSP_PNR_DESC[].
DELETE T_DATA[] WHERE VPMAT_DESC NOT IN  R_VPMAT_DESC[].
DELETE T_DATA[] WHERE VPWRK_DESC NOT IN  R_VPWRK_DESC[].
DELETE T_DATA[] WHERE VGTYP_DESC NOT IN  R_VGTYP_DESC[].
CHECK T_DATA[] IS NOT INITIAL.
*--- Check Alert Information
READ TABLE T_DATA INDEX 1.
CHECK NOT SY-TFILL IS INITIAL .
IS_ALERT = 'X' .
ENDFUNCTION.
```
