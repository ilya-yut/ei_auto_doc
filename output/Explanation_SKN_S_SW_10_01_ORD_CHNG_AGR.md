# Exception Indicator: SD Order Changes - Aggregative ( SW_10_01_ORD_CHNG_AG)

## General Overview

This Exception Indicator aggregates sales order SLA change results into summary counts and net values by customer, sales organization, plant, import category, and change type, splitting activity across free-period, sold-period, and other timing bands. It gives sales, planning, and customer-service teams a compact view of where SLA-relevant order changes concentrate.

This EI serves as an essential control for sales order SLA governance by:
- Enabling detection of customers, plants, and change types with elevated free-period or sold-period change volumes
- Supporting prioritization by net value so commercially significant SLA change clusters are reviewed first
- Helping planners compare cancellation, quantity, delay, and advance patterns without reviewing every detail line
- Providing visibility into free versus sold period mix for service-agreement and margin follow-up
- Supporting recurring management reporting of SLA change activity before peak shipping and customer reviews

This monitoring is useful for SLA compliance dashboards, customer-service performance reviews, and audit sampling of high-impact change clusters. It is especially relevant where teams need an aggregated view of free-period versus sold-period order changes rather than line-level detail.

The EI uses the detailed sales order SLA change analysis as its source and summarizes results for exception review.


## Problem Description

Failure to monitor aggregated sales order SLA changes creates risks across customer service, planning, and commercial commitments:

**Sales and Fulfillment Risks**

- High volumes of cancellations, quantity changes, delays, or advances can breach free or sold SLA expectations without a clear summary view
- Concentrations by customer, plant, or change type can hide systemic planning or service issues behind many detail lines
- Commercially significant change clusters can go unnoticed when only line-level extracts are available

**Operational Risks**

- Monitoring windows that do not match order-change volume can miss recent clusters or retain resolved history
- Free and sold period thresholds that are too wide or too narrow can distort aggregated counts and values
- Missing change-type or period-band separation reduces the ability to target the right follow-up actions

**Control and Audit Risks**

- Weak aggregated SLA monitoring reduces evidence that management reviewed free-period versus sold-period change patterns
- Lack of recurring summary review limits accountability between sales, planning, and logistics teams
- Missing customer and organizational context delays escalation of high-value SLA exception clusters

## Suggested Resolution

**Immediate Response**

- Review aggregated rows for customer, sales organization, plant, change type, free/sold/other counts, and related net values
- Confirm with sales and planning whether elevated free-period or sold-period volumes are authorized under SLA rules
- Prioritize high sold-period counts, high net-value clusters, and recurring customer or plant patterns

**System Assessment**

- Validate lookback window, reference schedule date, and free versus sold period thresholds against the agreed SLA model
- Tune change-type and organizational scope so aggregated results stay actionable
- Compare free-period, sold-period, and other counts by customer and plant to identify systematic gaps

**Corrective Actions**

- Correct unauthorized schedule or quantity change practices through standard sales order processes where review confirms action is required
- Adjust free and sold period thresholds and monitoring scope after cleanup so summaries reflect true SLA exceptions
- Document review outcomes and schedule recurring aggregated runs before peak shipping periods and customer SLA reviews


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AUART | Sales Document Type |  | 0 | 0 |  |  |
| 2 | BACKDAYS | Days Bacward from now |  | 0 | 0 |  |  |
| 3 | CHECK_IMPORTED | 'X' - Check |  | 0 | 0 |  |  |
| 4 | CHNG_TYP | Doc Change type (C/Q/D/A/L) | CHAR | 2 | 0 | CHAR2 | CHAR2 |
| 5 | CHNG_TYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 6 | CNT_FR | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 7 | CNT_OT | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 8 | CNT_SL | Natural Number | INT4 | 10 | 0 | INT4 | INT4 |
| 9 | DATE_REF_FLD | Date Field for period calc |  | 0 | 0 |  |  |
| 10 | DESC | X - show descriptions |  | 0 | 0 |  |  |
| 11 | DURATION | Duration In Time Units |  | 0 | 0 |  |  |
| 12 | DURATION_UNIT | Duration Unit |  | 0 | 0 |  |  |
| 13 | FR_A1_PERIOD | Advance FR period |  | 0 | 0 |  |  |
| 14 | FR_A2_PERIOD | Advance FR period for Import |  | 0 | 0 |  |  |
| 15 | FR_C1_PERIOD | Cancel FR period |  | 0 | 0 |  |  |
| 16 | FR_C2_PERIOD | Cancel FR period for Import |  | 0 | 0 |  |  |
| 17 | FR_D1_PERIOD | Delay FR period |  | 0 | 0 |  |  |
| 18 | FR_D2_PERIOD | Delay FR period for Import |  | 0 | 0 |  |  |
| 19 | FR_Q1_PERIOD | Qty change FR period |  | 0 | 0 |  |  |
| 20 | FR_Q2_PERIOD | Qty change FR period for Impor |  | 0 | 0 |  |  |
| 21 | IMP_TYP | Version Number Component | CHAR | 2 | 0 | CHAR2 | CHAR2 |
| 22 | IMP_TYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 23 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 24 | KUNNR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 25 | LAND1 | Country | CHAR | 3 | 0 | LAND1_GP | LAND1 |
| 26 | LAND_DESC | Name | CHAR | 15 | 0 | LANDX | TEXT15 |
| 27 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 28 | MANAGE_IN_UTC |  | 0 | 0 |  |  |  |
| 29 | MATNR | Material |  | 0 | 0 |  |  |
| 30 | NETWR_FR | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 31 | NETWR_OT | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 32 | NETWR_SL | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 33 | PERIOD_TYP | FR/SL |  | 0 | 0 |  |  |
| 34 | SL_A1_PERIOD | Advance SL period |  | 0 | 0 |  |  |
| 35 | SL_A2_PERIOD | Advance SL period for Import |  | 0 | 0 |  |  |
| 36 | SL_C1_PERIOD | Cancel SL period |  | 0 | 0 |  |  |
| 37 | SL_C2_PERIOD | Cancel SL period for Import |  | 0 | 0 |  |  |
| 38 | SL_D1_PERIOD | Delay SL period |  | 0 | 0 |  |  |
| 39 | SL_D2_PERIOD | Delay SL period for Import |  | 0 | 0 |  |  |
| 40 | SL_Q1_PERIOD | Qty change SL period |  | 0 | 0 |  |  |
| 41 | SL_Q2_PERIOD | Qty change SL period for Impor |  | 0 | 0 |  |  |
| 42 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 43 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 44 | VALUE_CANCEL | Value for Cancel definition |  | 0 | 0 |  |  |
| 45 | VBELN | Sales Document |  | 0 | 0 |  |  |
| 46 | VBTYP | SD document categ. |  | 0 | 0 |  |  |
| 47 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 48 | VKORG_DESC | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 49 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 50 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 51 | WERKS | Plant | CHAR | 4 | 0 | WERKS_EXT | WERKS |
| 52 | WERKS_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 52 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**BACKDAYS** (Days Bacward from now)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**CHECK_IMPORTED** ('X' - Check)

Flag that determines whether imported-material handling is applied when classifying free versus sold SLA periods on the detail lines that feed aggregation.

**CHECK_IMPORTED Options:**
- **X** — Apply imported-material period classification where relevant.
- Empty or blank — Use standard period classification.

**CHNG_TYP** (Doc Change type (C/Q/D/A/L))

Sales order change classification used to separate cancellations, quantity changes, delays, advances, and related logistics timing moves before aggregation.

**CHNG_TYP Options:**
- **C** — Cancellation / rejection-related change.
- **Q** — Quantity change.
- **D** — Delay (delivery date moved later).
- **A** — Advance (delivery date moved earlier).
- **L** — Logistics/timing consolidation used in imported-material handling.

**CHNG_TYP_DESC** (Short Descript.)

Readable description of the classified change type on aggregated result rows.

**CNT_FR** (Natural Number)

Count of detail changes classified in the free SLA period for the aggregation key.

**CNT_OT** (Natural Number)

Count of detail changes that are not classified as free or sold for the aggregation key.

**CNT_SL** (Natural Number)

Count of detail changes classified in the sold SLA period for the aggregation key.

**DATE_REF_FLD** (Date Field for period calc)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- Use a field name from the EI's source structure that carries a valid calendar date for the monitored object.
- Values are system-specific; choose the field the ABAP selection uses for the primary date axis.

**DESC** (X - show descriptions)

Flag that controls whether customer, plant, sales organization, country, change-type, and import-type description texts are enriched on aggregated rows.

**DESC Options:**
- **X** — Enrich description fields for review.
- Empty or blank — Skip description enrichment.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**FR_A1_PERIOD** (Advance FR period)

Free SLA period threshold for advanced delivery-date changes; marks earlier date moves that still fall inside the free change window.

**FR_A2_PERIOD** (Advance FR period for Import)

Interprets advance fr period for import as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on FR_A2_PERIOD.

**Not in use**
**FR_C1_PERIOD** (Cancel FR period)

Free SLA period threshold for cancellation changes; marks cancellations that still fall inside the free change window.

**FR_C2_PERIOD** (Cancel FR period for Import)

Helps monitoring stay readable by requiring cancel fr period for import (FR_C2_PERIOD) to match organizational or technical selectors when set.

**Not in use**
**FR_D1_PERIOD** (Delay FR period)

Free SLA period threshold for delayed delivery-date changes; marks delays that still fall inside the free change window.

**FR_D2_PERIOD** (Delay FR period for Import)

Interprets delay fr period for import as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on FR_D2_PERIOD.

**Not in use**
**FR_Q1_PERIOD** (Qty change FR period)

Free SLA period threshold for quantity changes; marks quantity updates that still fall inside the free change window.

**FR_Q2_PERIOD** (Qty change FR period for Impor)

Mirrors how administrators slice operational lists: qty change fr period for impor (FR_Q2_PERIOD) is one lever that shapes which rows are comparable run over run.

**Not in use**
**IMP_TYP** (Version Number Component)

Material import category used in SLA analysis to distinguish standard materials from imported materials when classifying free versus sold period handling.
IMP_TYP Options:
1 - Standard (non-imported) material
2 - Imported material

**IMP_TYP_DESC** (Short Descript.)

Material import category  (type) description.

**KUNNR** (Sold-to party)

Customer account is used to scope records to specific customers across SD/FI flows.

**KUNNR_DESC** (Name)

Customer account description.

**LAND1** (Country)

Country key used for legal/geographic segmentation of business partners or plants.

**LAND_DESC** (Name)

Country description.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** (MANAGE_IN_UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**NETWR_FR** (Net value)

Aggregated net value of detail changes classified in the free SLA period for the aggregation key.

**NETWR_OT** (Net value)

Aggregated net value of detail changes that are not classified as free or sold for the aggregation key.

**NETWR_SL** (Net value)

Aggregated net value of detail changes classified in the sold SLA period for the aggregation key.

**PERIOD_TYP** (FR/SL)

SLA period classification that marks whether a detail change falls in the free-period band or the sold-period band before aggregation.

**PERIOD_TYP Options:**
- **FR** — Free-period change.
- **SL** — Sold-period change.

**SL_A1_PERIOD** (Advance SL period)

Sold SLA period threshold for advanced delivery-date changes; marks earlier date moves that fall inside the sold change window.

**SL_A2_PERIOD** (Advance SL period for Import)

Works downstream of the initial read so advance sl period for import on SL_A2_PERIOD still participates in row-level deletion rules.

**Not in use**
**SL_C1_PERIOD** (Cancel SL period)

Sold SLA period threshold for cancellation changes; marks cancellations that fall inside the sold change window.

**SL_C2_PERIOD** (Cancel SL period for Import)

Helps monitoring stay readable by requiring cancel sl period for import (SL_C2_PERIOD) to match organizational or technical selectors when set.

**Not in use**
**SL_D1_PERIOD** (Delay SL period)

Sold SLA period threshold for delayed delivery-date changes; marks delays that fall inside the sold change window.

**SL_D2_PERIOD** (Delay SL period for Import)

Supports operational control by evaluating delay sl period for import through SL_D2_PERIOD for each candidate record.

**Not in use**
**SL_Q1_PERIOD** (Qty change SL period)

Sold SLA period threshold for quantity changes; marks quantity updates that fall inside the sold change window.

**SL_Q2_PERIOD** (Qty change SL period for Impor)

Separates cross-client noise from in-scope work when qty change sl period for impor on SL_Q2_PERIOD correlates with client or user attributes.

**Not in use**
**SPART** (Division)

Division key used for SD product-line segmentation.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**Not in use**
**VALUE_CANCEL** (Value for Cancel definition)

Rejection or cancel values used to recognize cancellation-type sales order changes during SLA classification. When not supplied, standard cancel values 08 and 10 are applied.

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VKORG_DESC** (Name)

Sales organization description.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.

**WAERK** (Document Currency)

Document currency key used for value analysis in transaction currency.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WERKS_DESC** (Name 1)

Plant name or description providing readable site context beside plant keys.

### Parameter Relationships

**Underlying detail analysis:** Selection parameters such as **BACKDAYS**, **DATE_REF_FLD**, **DURATION**, **DURATION_UNIT**, **CHNG_TYP**, **VALUE_CANCEL**, free/sold period thresholds, and sales scope filters are passed through to the detailed sales order SLA change analysis before aggregation.

**Aggregation key:** Detail rows are summarized by **KUNNR**, **VKORG**, **WERKS**, **IMP_TYP**, **CHNG_TYP**, and **WAERK**.

**Period split:** Within each aggregation key, counts and net values are split by period classification into **CNT_FR** / **NETWR_FR** (free), **CNT_SL** / **NETWR_SL** (sold), and **CNT_OT** / **NETWR_OT** (other or unclassified).

**SLA free and sold bands:** Type-specific **FR_*1_PERIOD** and **SL_*1_PERIOD** ranges classify detail changes before aggregation. Separate thresholds exist for cancellation, quantity change, delay, and advance.

**Imported-material check:** When **CHECK_IMPORTED** is set, imported-material handling can influence period classification on the detail lines that feed the summary.

**Descriptions:** When **DESC** is set, customer, plant, sales organization, country, change-type, and import-type descriptions are enriched on aggregated rows.

**Sales scope:** **VBELN**, **KUNNR**, **MATNR**, **VKORG**, **VTWEG**, **SPART**, **AUART**, **VBTYP**, and **WERKS** narrow which sales order changes enter the underlying analysis and therefore the summary.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DATE_REF_FLD** - initial - treated as LDDAT by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **LANGU** - initial - treated as the logon language by code
- **VALUE_CANCEL** - initial - treated as 08 and 10 by code

### Practical Example of Parameter Configuration

**Use Case 1: Weekly aggregated SLA review**

**Purpose:** Summarize sales order SLA changes from the last seven days by customer and plant.

```
BACKDAYS = 7
DATE_REF_FLD = LDDAT
DURATION_UNIT = D
VKORG = 1000
```

**Use Case 2: Delay and advance summary**

**Purpose:** Aggregate only delivery-date delays and advances into free versus sold counts.

```
CHNG_TYP = D
CHNG_TYP = A
BACKDAYS = 14
FR_D1_PERIOD = LE 56
SL_D1_PERIOD = BT 56 84
```

**Use Case 3: Cancellation free-period clusters**

**Purpose:** Highlight customers with cancellations classified in the free SLA window.

```
CHNG_TYP = C
PERIOD_TYP = FR
VKORG = 1000
BACKDAYS = 30
VALUE_CANCEL = 08
```

**Use Case 4: Quantity changes with descriptions**

**Purpose:** Review aggregated quantity-change volumes with readable customer and plant descriptions.

```
CHNG_TYP = Q
DESC = X
BACKDAYS = 14
FR_Q1_PERIOD = LE 56
SL_Q1_PERIOD = BT 56 84
```

**Use Case 5: Exactly seven full days from reference date**

**Purpose:** Return rows whose scope is exactly 7 full days ago when DURATION_UNIT = F and DURATION = 7.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 30
DATE_REF_FLD = LDDAT
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_ORD_CHNG_AGR | CHNG_TYP | Version Number Component | CHAR(2) | CHAR2 |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | CHNG_TYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | CNT_FR | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | CNT_OT | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | CNT_SL | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | IMP_TYP | Version Number Component | CHAR(2) | CHAR2 |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | IMP_TYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | KUNNR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | LAND1 | Country Key | CHAR(3) | LAND1_GP |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | LAND_DESC | Country Name | CHAR(15) | LANDX |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | NETWR_FR | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | NETWR_OT | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | NETWR_SL | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | VKORG_DESC | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | WERKS | Plant (Own or External) | CHAR(4) | WERKS_EXT |
| /SKN/S_SW_10_01_ORD_CHNG_AGR | WERKS_DESC | Name | CHAR(30) | NAME1 |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_10_01_ORD_CHNG_AGR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_CHNG_AGR OPTIONAL
*"----------------------------------------------------------------------
DATA: LS_DET TYPE /SKN/S_SW_10_01_ORD_CHNG,
      LT_DET LIKE TABLE OF LS_DET.
DATA: BEGIN OF LS_DET_AGR,
       CNT TYPE INT4.
  INCLUDE STRUCTURE /SKN/S_SW_10_01_ORD_CHNG.
DATA: END OF LS_DET_AGR.
DATA: LT_DET_AGR LIKE TABLE OF LS_DET_AGR.
DATA: LS_DATA LIKE LINE OF T_DATA,
      LT_DATA LIKE TABLE OF LS_DATA.
TYPES: BEGIN OF TY_AGR_CROSS,
        KUNNR TYPE KUNAG,
        VKORG TYPE VKORG,
        WERKS TYPE WERKS_EXT,
        IMP_TYP LIKE LS_DET-IMP_TYP,
        CHNG_TYP LIKE LS_DET-CHNG_TYP,
        WAERK TYPE WAERK,
      END OF TY_AGR_CROSS.
DATA: LS_CROSS_KEY TYPE TY_AGR_CROSS,
      LS_CROSS_KEY_OLD TYPE TY_AGR_CROSS.
TYPES: BEGIN OF TY_AGR_KEY,
        KUNNR TYPE KUNAG,
        VKORG TYPE VKORG,
        WERKS TYPE WERKS_EXT,
        IMP_TYP LIKE LS_DET-IMP_TYP,
        CHNG_TYP LIKE LS_DET-CHNG_TYP,
        WAERK TYPE WAERK,
        PERIOD_TYP  LIKE LS_DET-PERIOD_TYP,
      END OF TY_AGR_KEY.
DATA: LS_AGR_KEY TYPE TY_AGR_KEY,
      LS_AGR_KEY_OLD  TYPE TY_AGR_KEY.
DATA: SY_TABIX LIKE SY-TABIX.
 DATA_SINGLE: SW_DEST RFCDEST.             .
 SELECT_SINGLE: SW_DEST.
 DATA_SINGLE: DESC   CHAR1,
              LANGU  LANGU .
 LV_LANGU = SY-LANGU.
 SELECT_SINGLE: DESC,
                LANGU.
   CALL FUNCTION '/SKN/FC_SW_10_01_ORD_CHNG'
     IMPORTING
       IS_ALERT       = IS_ALERT
     TABLES
       T_SELECT       = T_SELECT
       T_DATA         = LT_DET.
   SORT LT_DET BY KUNNR VKORG WERKS IMP_TYP CHNG_TYP PERIOD_TYP WAERK.
   CLEAR: LS_AGR_KEY, LS_AGR_KEY_OLD.
   REFRESH LT_DET_AGR.
   LOOP AT LT_DET INTO LS_DET.
     SY_TABIX = SY-TABIX.
     MOVE-CORRESPONDING LS_DET TO LS_AGR_KEY.
     IF LS_AGR_KEY = LS_AGR_KEY_OLD.
       "--- Aggregate
       ADD 1 TO LS_DET_AGR-CNT.
       ADD LS_DET-NETWR TO LS_DET_AGR-NETWR.
     ELSE.
       "--- Append aggregated record
       IF SY_TABIX > 1.
         APPEND LS_DET_AGR TO LT_DET_AGR.
       ENDIF.
       "---  Init aggregation
       CLEAR LS_DET_AGR.
       MOVE-CORRESPONDING LS_DET TO LS_DET_AGR.
       LS_DET_AGR-CNT = 1.
       "--- Set old key
       LS_AGR_KEY_OLD = LS_AGR_KEY.
     ENDIF.
   ENDLOOP.
   "--- save the last aggregatin
   APPEND LS_DET_AGR TO LT_DET_AGR.
   "--- Cross transformation
   SORT LT_DET_AGR BY KUNNR VKORG WERKS IMP_TYP CHNG_TYP WAERK.
   CLEAR: LS_CROSS_KEY, LS_CROSS_KEY_OLD.
   REFRESH LT_DATA.
   LOOP AT LT_DET_AGR INTO LS_DET_AGR.
     SY_TABIX = SY-TABIX.
     MOVE-CORRESPONDING LS_DET_AGR TO LS_CROSS_KEY.
     IF LS_CROSS_KEY = LS_CROSS_KEY_OLD.
       "--- Aggregate
       CASE LS_DET_AGR-PERIOD_TYP.
         WHEN 'FR'.
           ADD LS_DET_AGR-CNT TO LS_DATA-CNT_FR.
           ADD LS_DET_AGR-NETWR TO LS_DATA-NETWR_FR.
         WHEN 'SL'.
           ADD LS_DET_AGR-CNT TO LS_DATA-CNT_SL.
           ADD LS_DET_AGR-NETWR TO LS_DATA-NETWR_SL.
         WHEN OTHERS.
           ADD LS_DET_AGR-CNT TO LS_DATA-CNT_OT.
           ADD LS_DET_AGR-NETWR TO LS_DATA-NETWR_OT.
       ENDCASE.
     ELSE.
       "--- Append aggregated record
       IF SY_TABIX > 1.
         APPEND LS_DATA TO LT_DATA.
       ENDIF.
       "---  Init aggregation
       CLEAR LS_DATA.
       MOVE-CORRESPONDING LS_DET_AGR TO LS_DATA.
       CASE LS_DET_AGR-PERIOD_TYP.
         WHEN 'FR'.
           LS_DATA-CNT_FR = LS_DET_AGR-CNT.
           LS_DATA-NETWR_FR = LS_DET_AGR-NETWR.
         WHEN 'SL'.
           LS_DATA-CNT_SL = LS_DET_AGR-CNT.
           LS_DATA-NETWR_SL = LS_DET_AGR-NETWR.
         WHEN OTHERS.
           LS_DATA-CNT_OT = LS_DET_AGR-CNT.
           LS_DATA-NETWR_OT = LS_DET_AGR-NETWR.
       ENDCASE.
       "--- Set old key
       LS_CROSS_KEY_OLD = LS_CROSS_KEY.
     ENDIF.
   ENDLOOP.
   "--- save the last aggregatin
   IF LS_CROSS_KEY IS NOT INITIAL.
     APPEND LS_DATA TO LT_DATA.
   ENDIF.
******************************************************************************
*  Get Descriptions
******************************************************************************
IF LV_DESC IS NOT INITIAL.
  LOOP AT LT_DATA INTO LS_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/FC_SW_10_CUST_DESC'
      EXPORTING
        KUNNR                = LS_DATA-KUNNR
        SW_DEST              = LV_SW_DEST
      IMPORTING
        CUST_DESC            = LS_DATA-KUNNR_DESC
      EXCEPTIONS
        WRONG_CUSTOMER       = 1
        OTHERS               = 2              .
    IF SY-SUBRC <> 0.
    ENDIF.
    CALL FUNCTION '/SKN/FC_SW_10_PLANT_DESC'
      EXPORTING
        WERKS            = LS_DATA-WERKS
        LANGU            = LV_LANGU
        SW_DEST          = LV_SW_DEST
      IMPORTING
        PLANT_DESC       = LS_DATA-WERKS_DESC
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2              .
    IF SY-SUBRC <> 0.
    ENDIF.
    CALL FUNCTION '/SKN/FC_SW_10_SALES_ORG_DESC'
      EXPORTING
        VKORG                = LS_DATA-VKORG
        LANGU                = LV_LANGU
        SW_DEST              = LV_SW_DEST
      IMPORTING
        SALES_ORG_DESC       = LS_DATA-VKORG_DESC
      EXCEPTIONS
        WRONG_CODE           = 1
        OTHERS               = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    PERFORM GET_CHNG_TYP_DESC USING LS_DATA-CHNG_TYP
                              CHANGING LS_DATA-CHNG_TYP_DESC.
     CALL FUNCTION '/SKN/F_SW_10_COUNTRY_DESC'
       EXPORTING
         LAND1            = LS_DATA-LAND1
         LANGU            = LV_LANGU
         SW_DEST          = LV_SW_DEST
       IMPORTING
         LANDX            = LS_DATA-LAND_DESC
*        NATIO            =
*        LANDX50          =
*        NATIO50          =
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2.
     IF SY-SUBRC <> 0.
     ENDIF.
    PERFORM GET_IMP_TYP_DESC USING LS_DATA-IMP_TYP
                              CHANGING LS_DATA-IMP_TYP_DESC.
    MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
ENDIF.
  T_DATA[] = LT_DATA[].
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
