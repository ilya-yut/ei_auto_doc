# Exception Indicator: Negative Profit Deal - SW_10_01_SALES_REV

## General Overview

This Exception Indicator (EI) monitors sales document items in Sales and Distribution (SD) where the net value is lower than cost, identifying deals with negative profit that require management attention. It provides visibility into unprofitable or mispriced sales lines across configurable time periods and organizational dimensions.

This EI serves as an essential control for sales and margin oversight by:
- Enabling detection of sales order lines sold below cost that may indicate pricing errors, urgent clearance, or margin leakage
- Supporting identification of negative-margin deals for pricing review, rebate validation, and revenue recognition checks
- Providing visibility into unprofitable patterns by sales organization, distribution channel, material group, and customer for corrective action
- Enabling analysis of manual price and document-type concentration among loss-making lines
- Supporting accountability by sold-to party, sales office, and sales group for margin governance and exception escalation

Monitoring negative-profit deals helps organizations catch pricing mistakes, enforce margin policies, and prioritize corrections during month-end close and sales performance reviews. The EI is valuable for margin analysis, exception management, and audit of sales document values against cost.

The EI uses sales order header and item data (VBAK, VBAP), comparing item net value to cost (WAVWR) within a configurable date window.


## Problem Description

Failure to monitor sales document items where net value is below cost creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected negative-margin deals can distort period profit and loss and margin reporting
- Loss-making lines may indicate revenue recognition or cost-allocation issues requiring adjustment
- Concentrations of below-cost sales in specific periods or currencies can signal pricing or hedging gaps
- Unidentified negative-profit volume may delay month-end close when discovered late during review
- Aggregated margin analysis can mask item-level losses that require separate correction or write-down

**Sales Operations and Control Risks**
- Lines sold below cost without visibility may reflect unauthorized discounts, incorrect condition types, or master data errors
- Negative-margin patterns by customer or sales channel could indicate credit or commercial policy violations
- Unusual concentration by document type, item category, or manual-price flag may point to process or system misuse
- High volume of loss-making items in specific plants or divisions may reflect operational or transfer-pricing issues
- Lack of item-level exception reporting can allow repeated pricing errors or clearance abuse

**Management Visibility and Decision-Making Risks**
- Absence of negative-profit monitoring delays awareness of margin erosion and pricing effectiveness
- Unidentified loss-making deals limit ability to adjust pricing, promotions, or rebates in a timely way
- Exceptions may require audit or compliance review but go unnoticed without targeted monitoring
- Inability to attribute negative profit to organization, customer, or product constrains accountability and corrective action

## Suggested Resolution

**Immediate Response**
- Review the sales document items flagged by the EI to confirm net value below cost and the business context (e.g. clearance, campaign, error)
- Verify selected orders and items in the system to confirm values and authorization (e.g. VA03, VA05)
- Check document type, item category, and manual price status to determine whether exceptions are policy-based or erroneous
- Identify responsible sales organization, distribution channel, and customer for escalation or correction

**System Assessment**
- Analyze the time window and date basis used for selection to ensure the monitoring period is appropriate
- Compare negative-profit volume and count to prior periods and to total sales to assess materiality and trend
- Examine distribution by sales org, channel, material group, and currency to find concentration or systematic issues
- Assess manual price and rejection-reason patterns to distinguish intentional vs. inadvertent below-cost sales
- Validate cost (WAVWR) and net value sources to ensure data quality and correct comparison logic

**Corrective Actions**
- Correct erroneous prices, conditions, or master data using VA02 (Change Sales Order) or VK12/VK13 where applicable
- For authorized below-cost deals, document business justification and ensure proper approval and audit trail
- Adjust rebates, discounts, or pricing procedures to prevent recurring negative-margin exceptions where inappropriate
- Update monitoring parameters (e.g. time window, organizational scope) to align with margin policies and close calendar
- Establish recurring EI runs and route results to sales and finance for continuous margin oversight and exception resolution


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ABGRU | Reason for rejection | CHAR | 2 | 0 | ABGRU_VA | ABGRU_VA |
| 2 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 3 | ARKTX | Description | CHAR | 40 | 0 | ARKTX | TEXT40 |
| 4 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 5 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 6 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 7 | DOC_TYPE_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 8 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 9 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 10 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 11 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 12 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 13 | LGORT | Storage Location | CHAR | 4 | 0 | LGORT_D | LGORT |
| 14 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 15 | MATKL_DESC | Material Group Desc. | CHAR | 20 | 0 | WGBEZ | TEXT20 |
| 16 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 17 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 18 | MPROK_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 19 | NETWR | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 20 | PLANT_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 21 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 22 | PSTYV | Item category | CHAR | 4 | 0 | PSTYV | PSTYV |
| 23 | SALES_GRP_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 24 | SALES_OFF_DESC | Description | CHAR | 20 | 0 | BEZEI20 | TEXT20 |
| 25 | SALES_ORG_DESC | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 26 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 27 | UEPOS | Higher-level item | NUMC | 6 | 0 | UEPOS | POSNR |
| 28 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 29 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 30 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 31 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 32 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 33 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 34 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 35 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |
| 36 | WERKS | Plant | CHAR | 4 | 0 | WERKS_EXT | WERKS |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 36 parameters listed in the Parameters Reference Table above.

**ABGRU** (Reason for rejection):

Restricts which sales order items are included by reason for rejection (e.g. quotation or order rejection codes). When supplied, only items whose rejection reason falls within the given values are selected. Left empty, no restriction by rejection reason is applied.

**AEDAT** (Changed On):

Date or range used to restrict items by change date. The EI compares document dates (e.g. header/item changed on or created on) to the monitoring window; this parameter can narrow selection to a specific changed-on range. Used together with the lookback window when no explicit range is given.

**ARKTX** (Description):

Item short text. When provided as a selection criterion, restricts which items are included by description content. Can be used to focus on specific product or campaign descriptions in the result set.

**AUART** (Sales Document Type):

Restricts selection to specific sales document types (e.g. order, quotation, returns). Only documents whose type is in the supplied list are included. Use standard SD document type codes.

**BACKDAYS** (Backdays):

Controls how many days to look back from today when no date range is supplied. The EI builds the monitoring window from today minus this number of days. Larger values widen the period for negative-profit detection; smaller values focus on recent activity.

**CUST_DESC** (Name):

Customer name (sold-to party description). When used as a criterion, it restricts or labels results by customer name. Typically derived from the sold-to party (KUNNR) for display or filtering in the result set.

**DOC_TYPE_DESC** (Description):

Description of the sales document type. Used for display or filtering in the result set; populated from the document type (AUART) in the language specified by LANGU.

**DURATION** (Duration In Time Units):

Numeric duration used to filter or display how long ago the document date falls relative to a reference date. The EI calculates duration from the reference date (e.g. changed on or created on) to the run date and can filter by this value. Use together with DURATION_UNIT.

**DURATION_UNIT** (Duration Unit):

Unit in which duration is expressed (e.g. days). Used for duration calculation and for filtering by DURATION so that age of the document is interpreted consistently.

**DURATION_UNIT Options:**
- **D**: Days.

**ERDAT** (Created On):

Date or range for document creation date. Restricts which items are included by creation date. The EI uses creation and change dates to build the selection window and to compute duration.

**KUNNR** (Sold-to party):

Restricts selection to specific sold-to customers. Only sales documents and items for the given customer numbers are included. Use standard customer number format (with or without leading zeros depending on conversion).

**LANGU** (Language for texts):

Language key used when retrieving descriptions for document type, sales organization, sales group, sales office, material group, plant, customer, and manual price. Affects only the language of description fields in the output; does not change which rows are selected.

**LGORT** (Storage Location):

Restricts which items are included by storage location. Only items whose plant/storage location matches the supplied values are selected.

**MATKL** (Material Group):

Restricts selection to specific material groups. Only items whose material group is in the supplied list are included. Supports product-line or category-level analysis of negative-profit deals.

**MATKL_DESC** (Material Group Desc.):

Material group description. Populated from the material group (MATKL) in the language specified by LANGU for display or filtering in the result set.

**MATNR** (Material):

Restricts selection to specific materials. Only items with the given material numbers are included. Use standard material number format (with or without leading zeros depending on conversion).

**MPROK** (Manual price):

Indicates whether the item has manual price change. When supplied, restricts results to items with the given manual-price status. Used to distinguish manually adjusted prices in negative-profit analysis.

**MPROK Options:**
- **X**: Manual price change (or equivalent active status).
- ** ** (space) or blank: No manual price change / standard.

**MPROK_DESC** (Short text):

Short text for the manual price status. Populated from the MPROK domain in the language specified by LANGU for display in the result set.

**NETWR** (Net value):

Net value of the order item in document currency. When used as a selection range, restricts which items are included by net value. The EI’s core logic selects items where net value is less than cost; this parameter can further restrict the value range considered.

**PLANT_DESC** (Name 1):

Plant name. Populated from the plant (WERKS) in the language specified by LANGU for display or filtering in the result set.

**POSNR** (Sales Document Item):

Item number of the sales document. When supplied, restricts which items are included by position. Used for detailed or single-item analysis.

**PSTYV** (Item category):

Restricts selection to specific item categories. Only items whose item category is in the supplied list are included. Supports analysis by item type (e.g. standard, free goods, returns).

**SALES_GRP_DESC** (Description):

Sales group description. Populated from the sales group (VKGRP) for display or filtering in the result set.

**SALES_OFF_DESC** (Description):

Sales office description. Populated from the sales office (VKBUR) for display or filtering in the result set.

**SALES_ORG_DESC** (Name):

Sales organization name. Populated from the sales organization (VKORG) for display or filtering in the result set.

**SPART** (Division):

Restricts selection to specific divisions. Only items whose division is in the supplied list are included. Supports division-level margin and negative-profit analysis.

**UEPOS** (Higher-level item):

Higher-level item number in BOM or configurable item structures. When supplied, restricts or identifies items by their parent item for hierarchical analysis.

**VBELN** (Sales Document):

Restricts selection to specific sales documents. Only the given document numbers (and their items) are included. Use for focused review of known orders.

**VBTYP** (SD document categ.):

SD document category (e.g. order, delivery, billing). Restricts which document types are included at the category level. Use together with AUART for precise document-type control.

**VBTYP Options:**
- **A**: Order (e.g. sales order).
- **C**: Billing document.
- **J**: Delivery.
- Other domain values as in VBTYP; exact list is system-dependent.

**VKBUR** (Sales Office):

Restricts selection to specific sales offices. Only documents and items assigned to the given sales offices are included. Supports accountability by sales office.

**VKGRP** (Sales Group):

Restricts selection to specific sales groups. Only documents and items assigned to the given sales groups are included. Supports accountability by sales group.

**VKORG** (Sales Organization):

Restricts selection to specific sales organizations. Only documents and items belonging to the given sales organizations are included. Use for organizational scoping of negative-profit monitoring.

**VTWEG** (Distribution Channel):

Restricts selection to specific distribution channels. Only documents and items with the given distribution channels are included. Use for channel-level margin analysis.

**WAERK** (Document Currency):

Document currency of the sales document/item. When supplied, restricts results to items in the given currencies. Describes the currency in which net value and cost are expressed in the document.

**WAVWR** (Cost):

Cost (e.g. moving average or standard cost) of the item in document currency. When used as a selection range, can restrict which items are included by cost. The EI selects items where net value is less than cost; this parameter can narrow the cost range considered.

**WERKS** (Plant):

Restricts selection to specific plants. Only items whose delivering or relevant plant is in the supplied list are included. Supports plant-level margin and negative-profit analysis.


### Parameter Relationships

**Time and Duration Parameters:**
- **BACKDAYS** defines the lookback window (e.g. from today minus BACKDAYS) when no date range is supplied; the EI uses this to build the selection range for document dates (ERDAT, AEDAT).
- **DURATION** and **DURATION_UNIT** work together: DURATION holds the numeric value and DURATION_UNIT the unit (e.g. days). The EI uses them to compute and filter by age/duration of the document relative to the reference date, and to populate the output duration field.

**Organizational and Selection Parameters:**
- **VKORG**, **VTWEG**, **SPART**, **VKGRP**, **VKBUR** define sales organization, distribution channel, division, sales group, and sales office; they restrict which sales documents and items are selected and appear together in the result.
- **AUART**, **VBTYP**, **PSTYV** define document type, SD document category, and item category; they narrow the type of sales documents and lines included.
- **KUNNR**, **MATNR**, **MATKL**, **WERKS**, **LGORT** restrict sold-to party, material, material group, plant, and storage location and are used together to scope the result set.

**Value and Margin Parameters:**
- **NETWR** and **WAVWR** (and the underlying comparison in the code) define the margin logic: the EI selects items where net value is less than cost; these parameters can restrict the value ranges considered in selection.

**Language and Descriptions:**
- **LANGU** drives the language for all description lookups (document type, sales org, sales group, sales office, material group, plant, customer, manual price); it affects only output text, not which rows are selected.


### Default Values

- **BACKDAYS** — Default: `1` (when no date range is supplied, the EI uses a lookback of one day from today).
- **DURATION_UNIT** — Default: `D` (days); used for duration calculation and filtering.
- **LANGU** — Default: system language (e.g. SY-LANGU); used for description texts.
- **BACKDAYS** (effective when no range supplied) — Default: one-day lookback; the code sets LV_BACKDAYS = 1 when no date range is supplied.

### Practical Configuration Examples

**Use Case 1: Recent negative-profit deals (last 30 days)**
```
BACKDAYS = 30
DURATION_UNIT = D
```
**Purpose:** Identify sales order items with net value below cost in the last 30 days for quick margin review and correction.

**Use Case 2: Negative profit by sales organization and channel**
```
VKORG = 1000, 2000
VTWEG = 10, 20
BACKDAYS = 7
```
**Purpose:** Focus on below-cost items in selected sales organizations and distribution channels over the past week for organizational accountability.

**Use Case 3: Negative margin by material group and plant**
```
MATKL = 001, 002
WERKS = 1000, 2000
BACKDAYS = 14
```
**Purpose:** Analyze loss-making lines by material group and plant for product and location-level margin control.

**Use Case 4: Document type and item category focus**
```
AUART = ZOR, ZOR2
PSTYV = TAN, TAE
BACKDAYS = 1
```
**Purpose:** Monitor negative-profit items for specific order types and item categories (e.g. standard orders, free goods) for process and pricing checks.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_SALES_REV | ABGRU | Reason for rejection of quotations and sales orders | CHAR(2) | ABGRU_VA |
| /SKN/S_SW_10_01_SALES_REV | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_SALES_REV | ARKTX | Short text for sales order item | CHAR(40) | ARKTX |
| /SKN/S_SW_10_01_SALES_REV | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_SALES_REV | CUST_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_SALES_REV | DOC_TYPE_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_SALES_REV | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_SALES_REV | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_SALES_REV | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_SALES_REV | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_SALES_REV | LGORT | Storage Location | CHAR(4) | LGORT_D |
| /SKN/S_SW_10_01_SALES_REV | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_01_SALES_REV | MATKL_DESC | Material Group Description | CHAR(20) | WGBEZ |
| /SKN/S_SW_10_01_SALES_REV | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_SALES_REV | MPROK | Status manual price change | CHAR(1) | MPROK |
| /SKN/S_SW_10_01_SALES_REV | MPROK_DESC | Explanatory short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_10_01_SALES_REV | NETWR | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_SALES_REV | PLANT_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_01_SALES_REV | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_SALES_REV | PSTYV | Sales document item category | CHAR(4) | PSTYV |
| /SKN/S_SW_10_01_SALES_REV | SALES_GRP_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_SALES_REV | SALES_OFF_DESC | Description | CHAR(20) | BEZEI20 |
| /SKN/S_SW_10_01_SALES_REV | SALES_ORG_DESC | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_01_SALES_REV | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_SALES_REV | UEPOS | Higher-level item in bill of material structures | NUMC(6) | UEPOS |
| /SKN/S_SW_10_01_SALES_REV | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_SALES_REV | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_SALES_REV | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_SALES_REV | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_SALES_REV | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_SALES_REV | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_SALES_REV | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_SALES_REV | WAVWR | Cost in document currency | CURR(13,2) | WAVWR |
| /SKN/S_SW_10_01_SALES_REV | WERKS | Plant (Own or External) | CHAR(4) | WERKS_EXT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_SALES_REV .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_SALES_REV OPTIONAL
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
              DURATION     /SKN/E_SW_DURATION,
              DATUM        SY-DATUM.
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
              DURATION,
              DATUM.
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
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_SALES_REV'
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
  SELECT *
    FROM VBAK
    INNER JOIN VBAP ON
    VBAK~VBELN = VBAP~VBELN
    INTO CORRESPONDING FIELDS OF TABLE  T_DATA
    WHERE  VBAK~VBELN IN R_VBELN
      AND AUART IN R_AUART
      AND VBTYP IN R_VBTYP
      AND VKORG IN R_VKORG
      AND VTWEG IN R_VTWEG
"      AND vbak~SPART IN R_SPART   ""???
      AND VKGRP IN R_VKGRP
      AND VKBUR IN R_VKBUR
      AND KUNNR IN R_KUNNR
      AND VBAP~SPART IN R_SPART   ""???
      AND VBAP~WAERK IN R_WAERK
      AND MATNR IN R_MATNR
      AND MATKL IN R_MATKL
      AND UEPOS IN R_UEPOS
      AND ABGRU IN R_ABGRU
      AND WERKS IN R_WERKS
      AND LGORT IN R_LGORT
      AND VBAP~NETWR IN R_NETWR
      AND WAVWR IN R_WAVWR
      AND PSTYV IN R_PSTYV
      AND MPROK IN R_MPROK
      AND ( VBAK~ERDAT IN R_DATUM OR VBAK~AEDAT IN R_DATUM )
      AND ( VBAP~ERDAT IN R_DATUM OR VBAP~AEDAT IN R_DATUM )
      AND VBAP~NETWR LT VBAP~WAVWR
      """AND VBAP~NETWR GT VBAP~WAVWR
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
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
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