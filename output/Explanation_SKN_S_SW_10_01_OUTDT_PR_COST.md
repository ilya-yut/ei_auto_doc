# Exception Indicator: Outdated product costing ( SW_10_01_OUTDT_PR_C)

## General Overview

This Exception Indicator identifies sales order items where material pricing and costing may be outdated because the material valuation price was last updated too far away from the order line creation timing. It gives sales, controlling, and pricing teams a focused review list for orders that may need margin validation or repricing before fulfillment or billing.

This EI serves as an essential control for sales and product costing operations by:
- Enabling detection of sales order lines that may carry stale product costing assumptions
- Supporting margin protection by highlighting gaps between material price maintenance and order entry
- Providing visibility into potential pricing and valuation mismatches by organization, customer, material, and plant
- Helping controlling teams prioritize commercially significant orders for review before downstream processing
- Supporting audit evidence that orders affected by old costing inputs are monitored and followed up

This monitoring is useful after standard cost changes, moving-average price updates, and recurring sales order reviews where product cost accuracy affects margin analysis. It is especially relevant for month-end margin checks, pricing governance, and exception management around open sales orders.

The EI uses SAP SD sales order data together with material valuation and pricing date information.


## Problem Description

Failure to monitor sales order lines against material price update timing creates risks across pricing accuracy, margin analysis, and order fulfillment:

**Sales and Costing Risks**

- Sales order lines can remain based on outdated product cost assumptions after material price maintenance
- Outdated product costing can distort order margin, profitability reporting, and commercial decision-making
- Repeated gaps by plant, material group, or sales area can indicate systematic costing or pricing governance issues

**Operational Risks**

- Review windows that do not match order entry and cost update cycles can miss recent exceptions or retain stale ones
- Thresholds that are too wide or too narrow can hide real margin risks or create unnecessary review volume
- Untuned organizational and material scope can mix low-risk lines into the same queue as commercially significant cases

**Control and Audit Risks**

- Weak monitoring reduces evidence that sales orders affected by old costing inputs were reviewed before downstream processing
- Lack of recurring exception review limits accountability between sales, controlling, and master-data teams
- Missing customer and product context can delay escalation of high-value or recurring issues

## Suggested Resolution

**Immediate Response**

- Review flagged order lines for material, customer, plant, pricing timing, cost update timing, and commercial value
- Confirm with sales and controlling whether each flagged line requires repricing, cost review, or documented acceptance
- Prioritize high-value customers, large-value order lines, and recurring material or plant patterns for immediate follow-up

**System Assessment**

- Validate the monitoring window and price-update age threshold against the organization’s costing and order entry cadence
- Compare exception volume by sales area, plant, material type, and customer to identify systematic gaps
- Confirm that the monitored scope reflects active sales order processes rather than obsolete or irrelevant order types

**Corrective Actions**

- Reprocess or reprice affected orders through standard business procedures where review confirms outdated costing risk
- Correct material valuation, pricing, or master-data maintenance practices when recurring root causes are identified
- Document review outcomes and schedule recurring runs after cost updates, pricing cycles, and month-end margin reviews


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ABGRU | Reason for rejection | CHAR | 2 | 0 | ABGRU_VA | ABGRU_VA |
| 2 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 3 | ARKTX | Description | CHAR | 40 | 0 | ARKTX | TEXT40 |
| 4 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 5 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 6 | BWKEY | Valuation Area | CHAR | 4 | 0 | BWKEY | BWKEY |
| 7 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 8 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 9 | DATE_REF_FLD | CHAR | 30 | 0 | NAME_FELD | FDNAME |  |
| 10 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 11 | DOC_TYPE_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 12 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 13 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 14 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 15 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 16 | LAEPR | Last price change | DATS | 8 | 0 | LAEPR | DATUM |
| 17 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 18 | LGORT | Storage Location | CHAR | 4 | 0 | LGORT_D | LGORT |
| 19 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 20 | MATKL_DESC | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |
| 21 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 22 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 23 | MPROK_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 24 | MTART | Material Type | CHAR | 4 | 0 | MTART | MTART |
| 25 | NETWR | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 26 | PLANT_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 27 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 28 | PR_UPD_DATE_DIFF | Duration to last price change | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 29 | PRSDT | Pricing date | DATS | 8 | 0 | PRSDT | DATUM |
| 30 | PRSDT_H | Pricing date | DATS | 8 | 0 | PRSDT | DATUM |
| 31 | PRSDT_L | Pricing date | DATS | 8 | 0 | PRSDT | DATUM |
| 32 | PSTYV | Item category | CHAR | 4 | 0 | PSTYV | PSTYV |
| 33 | SALES_GRP_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 34 | SALES_OFF_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 35 | SALES_ORG_DESC | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 36 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 37 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 38 | UEPOS | Higher-level item | NUMC | 6 | 0 | UEPOS | POSNR |
| 39 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 40 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 41 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 42 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 43 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 44 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 45 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 46 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |
| 47 | WERKS | Plant | CHAR | 4 | 0 | WERKS_EXT | WERKS |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 47 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ABGRU** (Reason for rejection)

Reason for Rejection stores the code that explains why a sales document item was canceled or not processed further.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**ARKTX** (Description)

Short text for a manufacturing order component or BOM line (material description at order-component level).

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BWKEY** (Valuation Area)

Valuation area key joining material valuation to plant/company rules for moving-average or standard price.

**BWTAR** (Valuation Type)

Valuation type key used in split valuation scenarios (batch/material valuation layers).

**CUST_DESC** (Name)

Customer description/name text used for readable customer-level reporting.

**DATE_REF_FLD** (CHAR)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- AEDAT — Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**DATUM** (DATS)

When harmonized with related filters, dats on DATUM isolates the highest-risk record families.

**DOC_TYPE_DESC** (Description)

Document type description.

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

**KUNNR** (Sold-to party)

Customer account is used to scope records to specific customers across SD/FI flows.

**LAEPR** (Last price change)

Date of the last price change indicates the exact calendar date when a material valuation price was most recently updated by price change transactions, such as standard costing runs or manual revaluations

**LANGU** (Language for texts)

Language key used for language-dependent texts and user-language filtering.

**LGORT** (Storage Location)

Storage location used to segment stock/logistics movements by warehouse sub-location.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

**MATKL_DESC** (Material Group Desc.)

Material group description.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MPROK** (Manual price)

Material/procurement status key used to identify control-relevant status states.

**MPROK_DESC** (Short text)

Description of material/procurement status for readable reporting.

**MTART** (Material Type)

Material type classifying procurement, production, and valuation behavior of material master records.

**NETWR** (Net value)

Net value amount used for commercial threshold and anomaly checks.

**PLANT_DESC** (Name 1)

Plant name or description text paired with WERKS; readable master-data label, not the plant key field.

**POSNR** (Sales Document Item)

Document item number used for line-level drilldown and joins.

**PR_UPD_DATE_DIFF** (Duration to last price change)

Ensures reporting respects duration to last price change constraints carried by PR_UPD_DATE_DIFF.

**PRSDT** (Pricing date)

Date for pricing and exchange rate indicates the specific calendar date used by the system to determine the valid condition records and currency conversion rates for a pricing procedure.

**PRSDT_H** (Pricing date)

Pricing date header indicates the master date at the sales or delivery document header level that dictates pricing validity for all items, unless overridden at the line level.

**PRSDT_L** (Pricing date)

Pricing date item indicates the specific date at the line item level used for pricing calculation, which can either inherit from the header date or be set independently.

**PSTYV** (Item category)

Sales document item category controlling item behavior, pricing relevance, and delivery rules.

**SALES_GRP_DESC** (Description)

Sales group description.

**SALES_OFF_DESC** (Description)

Sales office description.

**SALES_ORG_DESC** (Name)

Sales organization description.

**SPART** (Division)

Division key used for SD product-line segmentation.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**UEPOS** (Higher-level item)

Higher-Level Item in Bill of Material Structures stores the line item number of the parent material, establishing a hierarchical link between a sub-item or component and its main product during sales order processing.

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VKBUR** (Sales Office)

Sales office key used for organizational SD segmentation.

**VKGRP** (Sales Group)

Sales group key used for team-level SD analytics.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.

**WAERK** (Document Currency)

Document currency key used for value analysis in transaction currency.

**WAVWR** (Cost)

Statistical value amount field used for value-based exception thresholds.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.


### Parameter Relationships

**Lookback window:** When no explicit date range is supplied, **BACKDAYS** builds the initial sales order lookback window before order lines are read.

**Date selection:** **DATUM** works with order header and item creation or change dates to determine which order lines enter the initial selection. Explicit date ranges override the fallback lookback window.

**Order-line age:** After the initial date selection, the EI calculates elapsed time from the order line reference date to the evaluation time using **DURATION_UNIT** and stores it in **DURATION**. Rows outside the configured **DURATION** range are removed, so this is an additional filter after the initial date window.

**Price-update age:** The EI calculates the elapsed time from the material last price change date to the order item creation date using **DURATION_UNIT** and stores it in **PR_UPD_DATE_DIFF**. Rows outside the configured **PR_UPD_DATE_DIFF** range are removed.

**Sales and material scope:** **VBELN**, **AUART**, **VBTYP**, **VKORG**, **VTWEG**, **SPART**, **VKGRP**, **VKBUR**, **KUNNR**, **MATNR**, **MATKL**, **MTART**, **WERKS**, **LGORT**, **PSTYV**, **MPROK**, **ABGRU**, and **UEPOS** define which sales order lines and material contexts are included.

**Value and currency scope:** **NETWR**, **WAVWR**, and **WAERK** allow review to focus on commercial value and transaction currency context.

**Description language:** **LANGU** controls language-dependent descriptions for document type, sales organization, sales group, sales office, customer, material group, and manual pricing status.

**Remote execution:** When **SW_DEST** is set, processing delegates to the remote/cloud implementation and the local retrieval path is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DATE_REF_FLD** - initial - treated as AEDAT by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **LANGU** - initial - treated as the logon language by code

### Practical Example of Parameter Configuration

**Use Case 1: Price update more than 30 days before order creation**

**Purpose:** Review order items where material valuation was last updated more than 30 days before the line was created.

```
PR_UPD_DATE_DIFF = GT 30
VKORG = 1000
BACKDAYS = 7
```

**Use Case 2: Finished goods in one plant**

**Purpose:** Sample outdated costing cases for finished goods in plant 1000.

```
MTART = FERT
WERKS = 1000
PR_UPD_DATE_DIFF = GT 14
BACKDAYS = 14
```

**Use Case 3: Customer-specific review**

**Purpose:** Review price-update age exceptions for one sold-to customer.

```
KUNNR = 100000
PR_UPD_DATE_DIFF = GT 7
VKORG = 1000
BACKDAYS = 30
```

**Use Case 4: Manual pricing status with aged valuation**

**Purpose:** Flag items with manual pricing status and a large gap between price update and order creation.

```
MPROK = A
PR_UPD_DATE_DIFF = GT 21
AUART = TA
VKORG = 1000
BACKDAYS = 14
```

**Use Case 5: Exactly seven full days since item changed-on date**

**Purpose:** Return rows whose item changed-on reference date is exactly 7 full days ago when DURATION_UNIT = F and DURATION = 7.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_OUTDT_PR_COST | ABGRU | Reason for rejection | CHAR(2) | ABGRU_VA |
| /SKN/S_SW_10_01_OUTDT_PR_COST | AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_OUTDT_PR_COST | ARKTX | Description | CHAR(40) | ARKTX |
| /SKN/S_SW_10_01_OUTDT_PR_COST | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_OUTDT_PR_COST | BWKEY | Valuation Area | CHAR(4) | BWKEY |
| /SKN/S_SW_10_01_OUTDT_PR_COST | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_01_OUTDT_PR_COST | CUST_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_OUTDT_PR_COST | DOC_TYPE_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_OUTDT_PR_COST | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_OUTDT_PR_COST | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_OUTDT_PR_COST | ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_OUTDT_PR_COST | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_OUTDT_PR_COST | LAEPR | Last price change | DATS(8) | LAEPR |
| /SKN/S_SW_10_01_OUTDT_PR_COST | LGORT | Storage Location | CHAR(4) | LGORT_D |
| /SKN/S_SW_10_01_OUTDT_PR_COST | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_01_OUTDT_PR_COST | MATKL_DESC | Material Group Desc. | CHAR(20) | WGBEZ |
| /SKN/S_SW_10_01_OUTDT_PR_COST | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_OUTDT_PR_COST | MPROK | Manual price | CHAR(1) | MPROK |
| /SKN/S_SW_10_01_OUTDT_PR_COST | MPROK_DESC | Short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_10_01_OUTDT_PR_COST | MTART | Material Type | CHAR(4) | MTART |
| /SKN/S_SW_10_01_OUTDT_PR_COST | NETWR | Net value | CURR(15) | NETWR_AP |
| /SKN/S_SW_10_01_OUTDT_PR_COST | PLANT_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_01_OUTDT_PR_COST | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_OUTDT_PR_COST | PRSDT | Pricing date | DATS(8) | PRSDT |
| /SKN/S_SW_10_01_OUTDT_PR_COST | PRSDT_H | Pricing date | DATS(8) | PRSDT |
| /SKN/S_SW_10_01_OUTDT_PR_COST | PRSDT_L | Pricing date | DATS(8) | PRSDT |
| /SKN/S_SW_10_01_OUTDT_PR_COST | PR_UPD_DATE_DIFF | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_OUTDT_PR_COST | PSTYV | Item category | CHAR(4) | PSTYV |
| /SKN/S_SW_10_01_OUTDT_PR_COST | SALES_GRP_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_OUTDT_PR_COST | SALES_OFF_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_OUTDT_PR_COST | SALES_ORG_DESC | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_01_OUTDT_PR_COST | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_OUTDT_PR_COST | UEPOS | Higher-level item | NUMC(6) | UEPOS |
| /SKN/S_SW_10_01_OUTDT_PR_COST | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_OUTDT_PR_COST | VBTYP | SD document categ. | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_OUTDT_PR_COST | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_OUTDT_PR_COST | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_OUTDT_PR_COST | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_OUTDT_PR_COST | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_OUTDT_PR_COST | WAERK | Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_OUTDT_PR_COST | WAVWR | Cost | CURR(13) | WAVWR |
| /SKN/S_SW_10_01_OUTDT_PR_COST | WERKS | Plant | CHAR(4) | WERKS_EXT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_OUTDT_PR_COST .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_OUTDT_PR_COST OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS INT4,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               DATE_REF_FLD NAME_FELD
               .
** Default values
  LV_BACKDAYS = 1.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  LV_DATE_REF_FLD = 'AEDAT'.
  SELECT_SINGLE:
                 LANGU,
                 BACKDAYS,
                 DURATION_UNIT,
                 DATE_REF_FLD
                 .
  DATA_MULTY:
              VBELN        VBELN_VA,
              AUART        AUART,
              VBTYP        VBTYP,
              VKORG        VKORG,
              VTWEG        VTWEG,
              SPART        SPART,
              VKGRP        VKGRP,
              VKBUR        VKBUR,
              KUNNR        KUNAG,
              WAERK        WAERK,
              MATNR        MATNR,
              MATKL        MATKL,
              UEPOS        UEPOS,
              ABGRU        ABGRU_VA,
              WERKS        WERKS_EXT,
              LGORT        LGORT_D,
              NETWR        NETWR_AP,
              WAVWR        WAVWR,
              PSTYV        PSTYV,
              MPROK        MPROK,
              MTART        MTART,
              DURATION     /SKN/E_SW_DURATION,
              DATUM        SY-DATUM,
              PR_UPD_DATE_DIFF  /SKN/E_SW_DURATION.
  SELECT_MULTY:
              VBELN,
              AUART,
              VBTYP,
              VKORG ,
              VTWEG ,
              SPART,
              VKGRP,
              VKBUR,
              KUNNR,
              WAERK,
              MATNR,
              MATKL,
              UEPOS,
              ABGRU,
              WERKS,
              LGORT,
              NETWR,
              WAVWR,
              PSTYV,
              MPROK,
              MTART,
              DURATION,
              DATUM,
              PR_UPD_DATE_DIFF.
  CONVERT_MULTY:  VBELN ALPHA.
  CONVERT_MULTY:  MATNR MATN1.
  CONVERT_MULTY:  AUART AUART .
  CONVERT_MULTY:  KUNNR ALPHA.
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM,
         DATE_TO LIKE SY-DATUM .
  DATA : LANGU LIKE SY-LANGU .
  DATA : TIME_DIFF TYPE  INT4 .
  DATA:LV_TABIX LIKE SY-TABIX.
  DATA: LV_WERKS TYPE WERKS_D.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : REF_DATE TYPE D.
  DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
        LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
        LV_DDTEXT LIKE  DD07V-DDTEXT.
  DATA: LV_SEL_VBAK TYPE STRING,
        LV_SEL_VBAP TYPE STRING,
        LV_SEL_T001W TYPE STRING,
        LV_SEL_MBEW TYPE STRING,
        LV_SEL_VBKD TYPE STRING,
        LV_SEL_CLAUSE TYPE STRING,
        LV_SEL_MARA TYPE STRING.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_OUTDT_PR_COST'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  IF R_MATNR[] IS INITIAL .
    RS_MATNR-SIGN = 'I' .
    RS_MATNR-OPTION = 'GT' .
    RS_MATNR-LOW = ''.
    APPEND RS_MATNR TO R_MATNR.
  ENDIF.
*--- Retrieve data
  CLEAR IS_ALERT .
   _BUILD_SQL_SEL_CLAUSE 'VBAK' '/SKN/S_SW_10_01_OUTDT_PR_COST' 'VBAK' ' ' LV_SEL_VBAK.
   _BUILD_SQL_SEL_CLAUSE 'VBAP' '/SKN/S_SW_10_01_OUTDT_PR_COST' 'VBAP' ' ' LV_SEL_VBAP.
    LV_SEL_T001W = 'T001W~BWKEY'.
    LV_SEL_MBEW = 'MBEW~LAEPR'.
    LV_SEL_VBKD = 'dh~PRSDT as PRSDT_H dl~PRSDT as PRSDT_L'.
    LV_SEL_MARA = 'MARA~MTART'.
   CONCATENATE LV_SEL_VBAK LV_SEL_VBAP LV_SEL_T001W  LV_SEL_MBEW LV_SEL_VBKD LV_SEL_MARA
                                                        INTO LV_SEL_CLAUSE SEPARATED BY ' '.
 """  concatenate lv_SEL_CLAUSE 'a~USNAM as USNAM_HD' into lv_SEL_CLAUSE SEPARATED BY ' '.
  SELECT (LV_SEL_CLAUSE) """"""*
    FROM VBAK
    INNER JOIN VBAP ON
    VBAK~VBELN = VBAP~VBELN
    INNER JOIN T001W ON
    VBAP~WERKS = T001W~WERKS
    INNER JOIN MBEW ON
    VBAP~MATNR = MBEW~MATNR
    AND VBAP~BWTAR = MBEW~BWTAR
    INNER JOIN VBKD AS DH ON
    VBAP~VBELN = DH~VBELN
    LEFT OUTER JOIN VBKD AS DL ON
    VBAP~VBELN = DL~VBELN
    AND VBAP~POSNR = DL~POSNR
    INNER JOIN MARA ON
    VBAP~MATNR = MARA~MATNR
    INTO CORRESPONDING FIELDS OF TABLE  T_DATA
    WHERE  VBAK~VBELN IN R_VBELN
      AND AUART IN R_AUART
      AND VBTYP IN R_VBTYP
      AND VBAK~VKORG IN R_VKORG
      AND VBAK~VTWEG IN R_VTWEG
"      AND vbak~SPART IN R_SPART   ""???
      AND VKGRP IN R_VKGRP
      AND VKBUR IN R_VKBUR
      AND VBAK~KUNNR IN R_KUNNR
      AND VBAP~SPART IN R_SPART   ""???
      AND VBAP~WAERK IN R_WAERK
      AND VBAP~MATNR IN R_MATNR
      AND VBAP~MATKL IN R_MATKL
      AND UEPOS IN R_UEPOS
      AND ABGRU IN R_ABGRU
      AND VBAP~WERKS IN R_WERKS
      AND LGORT IN R_LGORT
      AND VBAP~NETWR IN R_NETWR
      AND WAVWR IN R_WAVWR
      AND PSTYV IN R_PSTYV
      AND MPROK IN R_MPROK
      AND ( VBAK~ERDAT IN R_DATUM OR VBAK~AEDAT IN R_DATUM )
      AND ( VBAP~ERDAT IN R_DATUM OR VBAP~AEDAT IN R_DATUM )
      AND  MBEW~BWKEY = T001W~BWKEY
      AND  DH~POSNR = '000000'
      AND  MARA~MTART IN R_MTART
       .
**********************************************************************
**********************************************************************
  LOOP AT T_DATA . """"?????????
    SY_TABIX = SY-TABIX .
***    CONCATENATE 'T_DATA-' lv_date_ref_fld INTO fld .
***    ASSIGN (fld) TO .
***    IF  IS NOT ASSIGNED.
***      CONTINUE.
***    ENDIF.
***    ref_date =  .
    REF_DATE = T_DATA-AEDAT.
    IF REF_DATE IS INITIAL.
      REF_DATE = T_DATA-ERDAT.
    ENDIF.
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          T_DATA-DURATION = TIME_DIFF .
        ELSE.
          T_DATA-DURATION = '999999'.
        ENDIF.
      ENDIF.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = T_DATA-LAEPR  """ref_date   """" ???
          T_FROM      = SY-UZEIT
          D_TO        = T_DATA-ERDAT   """ref_date   """T_DATA-LAEPR """" ????
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          T_DATA-PR_UPD_DATE_DIFF = TIME_DIFF .
        ELSE.
          T_DATA-PR_UPD_DATE_DIFF = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  DELETE T_DATA WHERE PR_UPD_DATE_DIFF NOT IN R_PR_UPD_DATE_DIFF.
**********************************************************************
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_DOC_TYPE_DESC'
      EXPORTING
        AUART            = T_DATA-AUART
        LANGU            = LV_LANGU
     IMPORTING
        TYPE_DESC        = T_DATA-DOC_TYPE_DESC
     EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_SALES_ORG_DESC'
      EXPORTING
        VKORG                = T_DATA-VKORG
        LANGU                = LV_LANGU
     IMPORTING
        SALES_ORG_DESC       = T_DATA-SALES_ORG_DESC
     EXCEPTIONS
       WRONG_CODE            = 1
       OTHERS                = 2
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_SALES_GRP_DESC'
      EXPORTING
        VKGRP                = T_DATA-VKGRP
        LANGU                = SY-LANGU
     IMPORTING
       SALES_GRP_DESC       = T_DATA-SALES_GRP_DESC
     EXCEPTIONS
       WRONG_CODE           = 1
       OTHERS               = 2
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_SALES_OFF_DESC'
      EXPORTING
        VKBUR                = T_DATA-VKBUR
        LANGU                = LV_LANGU
     IMPORTING
       SALES_OFF_DESC       =  T_DATA-SALES_OFF_DESC
     EXCEPTIONS
       WRONG_CODE           = 1
       OTHERS               = 2
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
      EXPORTING
        MATKL              = T_DATA-MATKL
        LANGU              = LV_LANGU
     IMPORTING
        MATKL_DESC         = T_DATA-MATKL_DESC
*       MATKL_DESC_L       =
     EXCEPTIONS
       WRONG_CODE         = 1
       OTHERS             = 2
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    LV_WERKS = T_DATA-WERKS.
      CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
        EXPORTING
          WERKS            =  LV_WERKS
         LANGU             =  LV_LANGU
       IMPORTING
        PLANT_DESC       = T_DATA-PLANT_DESC
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
        EXPORTING
          KUNNR                = T_DATA-KUNNR
       IMPORTING
         CUST_DESC            =  T_DATA-CUST_DESC
       EXCEPTIONS
         WRONG_CUSTOMER       = 1
         OTHERS               = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
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
     IF T_DATA-PRSDT_L IS NOT INITIAL.
       T_DATA-PRSDT = T_DATA-PRSDT_L.
     ELSE.
       T_DATA-PRSDT = T_DATA-PRSDT_H.
     ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
**********************************************************************
*********************************************************************
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
