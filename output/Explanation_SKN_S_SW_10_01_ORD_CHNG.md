# Exception Indicator: Sales Order SLA Agreement Analysis - detailed ( SW_10_01_ORD_CHNG)

## General Overview

This Exception Indicator analyzes sales order schedule-line changes against configurable free and sold SLA windows, classifying cancellations, quantity changes, delays, and advances so operations teams can see whether order changes happened inside or outside agreed timing bands. It returns detailed change lines enriched with customer, material, plant, and sales organization context for SLA exception review.

This EI serves as an essential control for sales order SLA governance by:
- Enabling detection of order cancellations, quantity changes, and delivery-date moves that fall inside free or sold SLA periods
- Supporting accountability by showing which sales documents and schedule lines were changed, when, and how the change was classified
- Helping planners prioritize late versus early schedule moves that may breach customer service agreements
- Providing visibility into change patterns by customer, material, plant, and sales organization for SLA root-cause analysis
- Supporting recurring monitoring of sales order change behavior before fulfillment commits and customer escalations

This monitoring is useful for SLA compliance reviews, customer service performance checks, and audit sampling of high-impact order changes. It is especially relevant where sales and logistics teams need evidence that free-period versus sold-period change rules were applied consistently.

The EI uses SAP sales order change documents together with sales order header, item, and schedule-line data.


## Problem Description

Failure to monitor sales order changes against SLA free and sold timing windows creates risks across customer service, planning, and commercial commitments:

**Sales and Fulfillment Risks**

- Cancellations, quantity changes, and delivery-date moves can breach free or sold SLA windows without timely review
- Late or early schedule changes can disrupt allocation, shipping, and customer promise dates
- Concentrations of SLA-relevant changes by customer, plant, or material can signal systematic planning or master-data issues

**Operational Risks**

- Monitoring windows that do not match order-change volume can miss recent exceptions or retain resolved cases
- Free and sold period thresholds that are too wide or too narrow can hide breaches or create reviewer fatigue
- Missing change-type classification reduces the ability to separate cancellations, quantity moves, delays, and advances

**Control and Audit Risks**

- Weak SLA change monitoring reduces evidence that customer service agreements were reviewed after order changes
- Lack of recurring exception review limits accountability between sales, planning, and logistics follow-up teams
- Missing customer and document context delays escalation of commercially significant SLA breaches

## Suggested Resolution

**Immediate Response**

- Review flagged changes for sales document, item, schedule line, change type, free or sold period classification, and timing relative to the reference schedule date
- Confirm with sales and planning whether each change was authorized under the applicable SLA rules
- Prioritize delays, cancellations, and high-value customers for immediate follow-up

**System Assessment**

- Validate lookback window, reference schedule date, and free versus sold period thresholds against the agreed SLA model
- Tune change-type and organizational scope so results stay actionable
- Compare exception counts by customer, plant, material, and change type to identify systematic gaps

**Corrective Actions**

- Correct unauthorized schedule or quantity changes through standard sales order processes where review confirms action is required
- Adjust free and sold period thresholds and monitoring scope after cleanup so results reflect true SLA exceptions
- Document review outcomes and schedule recurring runs before peak shipping periods and customer SLA reviews


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 2 | AUGRU | Order reason | CHAR | 3 | 0 | AUGRU | AUGRU |
| 3 | AUGRU_DESC | Description | CHAR | 40 | 0 | BEZEI40 | TEXT40 |
| 4 | BACKDAYS | Days Bacward from now |  | 0 | 0 |  |  |
| 5 | CHECK_IMPORTED | 'X' - Check |  | 0 | 0 |  |  |
| 6 | CHNG_TYP | Doc Change type (C/Q/D/A/L) | CHAR | 2 | 0 | CHAR2 | CHAR2 |
| 7 | CHNG_TYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 8 | DATE_REF_FLD | Date Field for period calc |  | 0 | 0 |  |  |
| 9 | DESC | X - show descriptions |  | 0 | 0 |  |  |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | EDATU | Delivery Date | DATS | 8 | 0 | EDATU | DATUM |
| 13 | ETENR | Schedule Line Number | NUMC | 4 | 0 | ETENR | ETENR |
| 14 | FR_A1_PERIOD | Advance FR period |  | 0 | 0 |  |  |
| 15 | FR_A2_PERIOD | Advance FR period for Import |  | 0 | 0 |  |  |
| 16 | FR_C1_PERIOD | Cancel FR period |  | 0 | 0 |  |  |
| 17 | FR_C2_PERIOD | Cancel FR period for Import |  | 0 | 0 |  |  |
| 18 | FR_D1_PERIOD | Delay FR period |  | 0 | 0 |  |  |
| 19 | FR_D2_PERIOD | Delay FR period for Import |  | 0 | 0 |  |  |
| 20 | FR_PERIOD | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 21 | FR_Q1_PERIOD | Qty change FR period |  | 0 | 0 |  |  |
| 22 | FR_Q2_PERIOD | Qty change FR period for Impor |  | 0 | 0 |  |  |
| 23 | IMP_TYP | Version Number Component | CHAR | 2 | 0 | CHAR2 | CHAR2 |
| 24 | IMP_TYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 25 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 26 | KUNNR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 27 | LAND1 | Country | CHAR | 3 | 0 | LAND1_GP | LAND1 |
| 28 | LAND_DESC | Name | CHAR | 15 | 0 | LANDX | TEXT15 |
| 29 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 30 | MANAGE_IN_UTC |  | 0 | 0 |  |  |  |
| 31 | MATERIAL_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 32 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 33 | NETWR | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 34 | PERIOD_TYP | FR/SL | CHAR | 2 | 0 | CHAR2 | CHAR2 |
| 35 | PERIOD_TYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 36 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 37 | REF_DATE | Date | DATS | 8 | 0 | DATUM | DATUM |
| 38 | REF_DATE_ORG | Date | DATS | 8 | 0 | DATUM | DATUM |
| 39 | SL_A1_PERIOD | Advance SL period |  | 0 | 0 |  |  |
| 40 | SL_A2_PERIOD | Advance SL period for Import |  | 0 | 0 |  |  |
| 41 | SL_C1_PERIOD | Cancel SL period |  | 0 | 0 |  |  |
| 42 | SL_C2_PERIOD | Cancel SL period for Import |  | 0 | 0 |  |  |
| 43 | SL_D1_PERIOD | Delay SL period |  | 0 | 0 |  |  |
| 44 | SL_D2_PERIOD | Delay SL period for Import |  | 0 | 0 |  |  |
| 45 | SL_PERIOD | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 46 | SL_Q1_PERIOD | Qty change SL period |  | 0 | 0 |  |  |
| 47 | SL_Q2_PERIOD | Qty change SL period for Impor |  | 0 | 0 |  |  |
| 48 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 49 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 50 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 51 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 52 | VALUE_CANCEL | Value for Cancel definition |  | 0 | 0 |  |  |
| 53 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 54 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 55 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 56 | VKORG_DESC | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 57 | VRKME | Sales unit | UNIT | 3 | 0 | VRKME | MEINS |
| 58 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 59 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 60 | WERKS | Plant | CHAR | 4 | 0 | WERKS_EXT | WERKS |
| 61 | WERKS_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 62 | WMENG | Order quantity | QUAN | 13 | 3 | WMENG | MENG13 |
| 63 | YAUGRU | Order reason | CHAR | 3 | 0 | AUGRU | AUGRU |
| 64 | YAUGRU_DESC | Description | CHAR | 40 | 0 | BEZEI40 | TEXT40 |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 64 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**AUGRU** (Order reason)

Order Reason indicates the underlying business motive for creating a sales document, such as a return, a price correction, or a damage claim, to track performance and control subsequent billing or credit processing.

**AUGRU_DESC** (Description)

Order Reason description.

**BACKDAYS** (Days Bacward from now)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**CHECK_IMPORTED** ('X' - Check)

Flag that determines whether imported-material handling is applied when classifying free versus sold SLA periods for qualifying materials.

**CHECK_IMPORTED Options:**
- **X** — Apply imported-material period classification where relevant.
- Empty or blank — Use standard period classification.

**CHNG_TYP** (Doc Change type (C/Q/D/A/L))

Sales order change classification used to separate cancellations, quantity changes, delays, advances, and related logistics timing moves for SLA analysis.

**CHNG_TYP Options:**
- **C** — Cancellation / rejection-related change.
- **Q** — Quantity change.
- **D** — Delay (delivery date moved later).
- **A** — Advance (delivery date moved earlier).
- **L** — Logistics/timing consolidation used in imported-material handling.

**CHNG_TYP_DESC** (Short Descript.)

Readable description of the classified change type for review and reporting.

**DATE_REF_FLD** (Date Field for period calc)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- LDDAT — Loading deadline date on deliveries expressing last acceptable date for goods issue or pick/pack completion.

**DESC** (X - show descriptions)

Flag that controls whether customer, material, plant, sales organization, country, and change/period description texts are enriched on result rows.

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

**EDATU** (Delivery Date)

Schedule line date in SD deliveries and sales schedules for availability and transportation planning windows.

**ETENR** (Schedule Line Number)

Schedule line number splitting a sales item into multiple delivery or availability schedule rows.

**FR_A1_PERIOD** (Advance FR period)

Aligns exception volume with the chosen scope by testing advance fr period via FR_A1_PERIOD before alert evaluation.

**FR_A2_PERIOD** (Advance FR period for Import)

For distributed landscapes, advance fr period for import on FR_A2_PERIOD often anchors which application server or destination appears in results.

**Not in use**
**FR_C1_PERIOD** (Cancel FR period)

Captures edge cases where cancel fr period (FR_C1_PERIOD) must be non-default to reproduce a customer-specific monitoring scenario.

**FR_C2_PERIOD** (Cancel FR period for Import)

Mirrors how administrators slice operational lists: cancel fr period for import (FR_C2_PERIOD) is one lever that shapes which rows are comparable run over run.

**Not in use**
**FR_D1_PERIOD** (Delay FR period)

Treats delay fr period as a discriminator between similar rows that would otherwise look identical in a raw extract.

**FR_D2_PERIOD** (Delay FR period for Import)

For distributed landscapes, delay fr period for import on FR_D2_PERIOD often anchors which application server or destination appears in results.

**Not in use**
**FR_PERIOD** (Short Descript.)

Guards against oversized extracts when short descript. on FR_PERIOD is narrowed together with client, user, or session filters.

**FR_Q1_PERIOD** (Qty change FR period)

Supports operational control by evaluating qty change fr period through FR_Q1_PERIOD for each candidate record.

**FR_Q2_PERIOD** (Qty change FR period for Impor)

For operations, qty change fr period for impor on FR_Q2_PERIOD indicates whether a row belongs in the current monitoring pass versus historical noise.

**Not in use**
**IMP_TYP** (Version Number Component)

Mirrors how administrators slice operational lists: version number component (IMP_TYP) is one lever that shapes which rows are comparable run over run.

**IMP_TYP_DESC** (Short Descript.)

Reflects real administration where short descript. on IMP_TYP_DESC is routinely restricted to a single productive client or object family.

**KUNNR** (Sold-to party)

Customer account is used to scope records to specific customers across SD/FI flows.

**KUNNR_DESC** (Name)

Customer account description.

**LAND1** (Country)

Country key used for legal/geographic segmentation of business partners or plants.

**LAND_DESC** (Name)

When populated, keeps the extract focused so name (LAND_DESC) aligns with the intended triage slice.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** (MANAGE_IN_UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MATERIAL_DESC** (Material Description)

Guards against oversized extracts when material description on MATERIAL_DESC is narrowed together with client, user, or session filters.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**NETWR** (Net value)

Net value amount used for commercial threshold and anomaly checks.

**PERIOD_TYP** (FR/SL)

SLA period classification that marks whether the calculated duration falls in the free-period band or the sold-period band for the change type.

**PERIOD_TYP Options:**
- **FR** — Free-period change.
- **SL** — Sold-period change.

**PERIOD_TYP_DESC** (Short Descript.)

Readable description of the free or sold period classification for review and reporting.

**POSNR** (Sales Document Item)

Document item number used for line-level drilldown and joins.

**REF_DATE** (Date)

When harmonized with related filters, date on REF_DATE isolates the highest-risk record families.

**REF_DATE_ORG** (Date)

After data is read, lines are removed unless date on REF_DATE_ORG still satisfies the active multivalued selection.

**SL_A1_PERIOD** (Advance SL period)

Supports escalation where advance sl period on SL_A1_PERIOD signals ownership for follow-up between Basis and functional teams.

**SL_A2_PERIOD** (Advance SL period for Import)

Supports escalation where advance sl period for import on SL_A2_PERIOD signals ownership for follow-up between Basis and functional teams.

**Not in use**
**SL_C1_PERIOD** (Cancel SL period)

For distributed landscapes, cancel sl period on SL_C1_PERIOD often anchors which application server or destination appears in results.

**SL_C2_PERIOD** (Cancel SL period for Import)

Narrows retrieved rows where cancel sl period for import (SL_C2_PERIOD) must match the configured selection for this monitor.

**Not in use**
**SL_D1_PERIOD** (Delay SL period)

Narrows retrieved rows where delay sl period (SL_D1_PERIOD) must match the configured selection for this monitor.

**SL_D2_PERIOD** (Delay SL period for Import)

Improves readability of exported lists because delay sl period for import (SL_D2_PERIOD) columns stay aligned with the configured filter intent.

**Not in use**
**SL_PERIOD** (Short Descript.)

Reflects real administration where short descript. on SL_PERIOD is routinely restricted to a single productive client or object family.

**SL_Q1_PERIOD** (Qty change SL period)

When populated, keeps the extract focused so qty change sl period (SL_Q1_PERIOD) aligns with the intended triage slice.

**SL_Q2_PERIOD** (Qty change SL period for Impor)

Gives auditors traceable criteria because qty change sl period for impor on SL_Q2_PERIOD is applied consistently before any alert flag is raised.

**Not in use**
**SPART** (Division)

Division key used for SD product-line segmentation.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**Not in use**
**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**UTIME** (Time)

Update/change time used with UDATE for precise event windows.

**VALUE_CANCEL** (Value for Cancel definition)

Rejection or cancel values used to recognize cancellation-type sales order changes during SLA classification. When not supplied, standard cancel values 08 and 10 are applied.

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VKORG_DESC** (Name)

<mark>Sales organization description.</mark>

**VRKME** (Sales unit)

Sales unit of measure for the material in SD documents-unit for commercial sales quantities.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.

**WAERK** (Document Currency)

Document currency key used for value analysis in transaction currency.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WERKS_DESC** (Name 1)

Plant name or description providing readable site context beside plant keys.

**WMENG** (Order quantity)

When combined with destination discipline, order quantity on WMENG keeps both breadth and depth of the extract intentional.

**YAUGRU** (Order reason)

Ensures reporting respects order reason constraints carried by YAUGRU.

**YAUGRU_DESC** (Description)

Supports operational control by evaluating description through YAUGRU_DESC for each candidate record.

### Parameter Relationships

**Change lookback window:** When no explicit date range is supplied, **BACKDAYS** builds the monitoring window used to read sales order change headers. Explicit date ranges override that fallback.

**Reference schedule date:** **DATE_REF_FLD** names the schedule-line date used for SLA period calculation. The default reference is the loading date. Elapsed time from that reference date is stored in **DURATION** using **DURATION_UNIT**.

**SLA free and sold bands:** For each change type, the corresponding **FR_*1_PERIOD** and **SL_*1_PERIOD** ranges classify the row as free-period (**PERIOD_TYP** = FR) or sold-period (**PERIOD_TYP** = SL). Separate thresholds exist for cancellation, quantity change, delay, and advance.

**Change classification:** Changed fields are classified into **CHNG_TYP** values for cancellation, quantity change, delay, advance, and related logistics timing. **VALUE_CANCEL** defines which rejection or cancel values qualify as cancellations. Results can be filtered further by **CHNG_TYP** and **PERIOD_TYP**.

**Imported-material check:** When **CHECK_IMPORTED** is set, imported-material handling can influence period classification for qualifying materials.

**Descriptions:** When **DESC** is set, customer, material, plant, sales organization, country, change-type, import-type, period-type, and order-reason descriptions are enriched for review.

**Sales scope:** **VBELN**, **POSNR**, **KUNNR**, **MATNR**, **VKORG**, **VTWEG**, **SPART**, **AUART**, **VBTYP**, **WERKS**, and related schedule attributes narrow which sales order changes enter the analysis.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DATE_REF_FLD** - initial - treated as LDDAT by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **LANGU** - initial - treated as the logon language by code
- **VALUE_CANCEL** - initial - treated as 08 and 10 by code

### Practical Example of Parameter Configuration

**Use Case 1: Recent SLA change review**

**Purpose:** Monitor sales order schedule changes in the last seven days and classify free versus sold periods.

```
BACKDAYS = 7
DATE_REF_FLD = LDDAT
DURATION_UNIT = D
VKORG = 1000
```

**Use Case 2: Delay and advance only**

**Purpose:** Review only delivery-date delays and advances against SLA bands.

```
CHNG_TYP = D
CHNG_TYP = A
BACKDAYS = 14
FR_D1_PERIOD = LE 56
SL_D1_PERIOD = BT 56 84
```

**Use Case 3: Cancellation free-period check**

**Purpose:** Flag cancellations that fall inside the free SLA window for one sales organization.

```
CHNG_TYP = C
PERIOD_TYP = FR
VKORG = 1000
BACKDAYS = 30
VALUE_CANCEL = 08
```

**Use Case 4: Quantity changes with descriptions**

**Purpose:** Review quantity changes with readable customer and material descriptions.

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
| /SKN/S_SW_10_01_ORD_CHNG | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_CHNG | AUGRU | Order reason (reason for the business transaction) | CHAR(3) | AUGRU |
| /SKN/S_SW_10_01_ORD_CHNG | AUGRU_DESC | Description | CHAR(40) | BEZEI40 |
| /SKN/S_SW_10_01_ORD_CHNG | CHNG_TYP | Version Number Component | CHAR(2) | CHAR2 |
| /SKN/S_SW_10_01_ORD_CHNG | CHNG_TYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_01_ORD_CHNG | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ORD_CHNG | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ORD_CHNG | EDATU | Schedule line date | DATS(8) | EDATU |
| /SKN/S_SW_10_01_ORD_CHNG | ETENR | Delivery Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_01_ORD_CHNG | FR_PERIOD | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_01_ORD_CHNG | IMP_TYP | Version Number Component | CHAR(2) | CHAR2 |
| /SKN/S_SW_10_01_ORD_CHNG | IMP_TYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_01_ORD_CHNG | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_ORD_CHNG | KUNNR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_CHNG | LAND1 | Country Key | CHAR(3) | LAND1_GP |
| /SKN/S_SW_10_01_ORD_CHNG | LAND_DESC | Country Name | CHAR(15) | LANDX |
| /SKN/S_SW_10_01_ORD_CHNG | MATERIAL_DESC | Material Description (Short Text) | CHAR(40) | MAKTX |
| /SKN/S_SW_10_01_ORD_CHNG | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_ORD_CHNG | NETWR | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_CHNG | PERIOD_TYP | Version Number Component | CHAR(2) | CHAR2 |
| /SKN/S_SW_10_01_ORD_CHNG | PERIOD_TYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_01_ORD_CHNG | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_ORD_CHNG | REF_DATE | Date | DATS(8) | DATUM |
| /SKN/S_SW_10_01_ORD_CHNG | REF_DATE_ORG | Date | DATS(8) | DATUM |
| /SKN/S_SW_10_01_ORD_CHNG | SL_PERIOD | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_01_ORD_CHNG | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_CHNG | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_01_ORD_CHNG | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_01_ORD_CHNG | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_CHNG | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_CHNG | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_CHNG | VKORG_DESC | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_01_ORD_CHNG | VRKME | Sales unit | UNIT(3) | VRKME |
| /SKN/S_SW_10_01_ORD_CHNG | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_CHNG | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_CHNG | WERKS | Plant (Own or External) | CHAR(4) | WERKS_EXT |
| /SKN/S_SW_10_01_ORD_CHNG | WERKS_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_01_ORD_CHNG | WMENG | Order quantity in sales units | QUAN(13,3) | WMENG |
| /SKN/S_SW_10_01_ORD_CHNG | YAUGRU | Order reason (reason for the business transaction) | CHAR(3) | AUGRU |
| /SKN/S_SW_10_01_ORD_CHNG | YAUGRU_DESC | Description | CHAR(40) | BEZEI40 |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_10_01_ORD_CHNG .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_CHNG OPTIONAL
*"----------------------------------------------------------------------
  INCLUDE /SKN/PC_SQL_DATA.
  DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
               LANGU  LANGU,
               BACKDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               DESC   CHAR1.
  LV_BACKDAYS = 1.
  LV_DURATION_UNIT = 'D'.
  LV_DATE_REF_FLD = 'LDDAT'.
  LV_LANGU = SY-LANGU. """Tanya 07/11/18
  SELECT_SINGLE: MANAGE_IN_UTC,
                 LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT,
                 DESC.
  DATA_MULTY: KUNNR        VBAK-KUNNR,
              VBELN        VBAK-VBELN,
              VKORG        VBAK-VKORG,
              WERKS        WERKS_EXT,
              VTWEG        VBAK-VTWEG,
              SPART        VBAK-SPART,
              VBTYP        VBTYP,
              AUART        VBAK-AUART,
              BDDAT        VBEP-BDDAT,
              RSDAT        VBEP-RSDAT,
              TDDAT        VBEP-TDDAT,
              MBDAT        VBEP-MBDAT,
              LDDAT        VBEP-LDDAT,
              WADAT        VBEP-WADAT,
              EDATU        VBEP-EDATU,
              ERNAM        VBAK-ERNAM,
              DATUM        SY-DATUM,
              DURATION    /SKN/E_SW_DURATION,
              POSNR       POSNR_VA,
              MATNR       MATNR.
  SELECT_MULTY: KUNNR,
              VBELN,
              VKORG ,
              VTWEG ,
              SPART,
              VBTYP,
              AUART,
              BDDAT,
              RSDAT,
              TDDAT,
              MBDAT,
              LDDAT,
              WADAT,
              EDATU,
              ERNAM,
              DATUM,
              DURATION,
              POSNR,
              MATNR.
  DATA_MULTY: VALUE_CANCEL CDFLDVALN, " Cancelletion values
              CHNG_TYP  CHAR2,
              PERIOD_TYP CHAR2,
              FR_C1_PERIOD INT2,
              SL_C1_PERIOD INT2,
              FR_Q1_PERIOD INT2,
              SL_Q1_PERIOD INT2,
              FR_D1_PERIOD INT2,
              SL_D1_PERIOD INT2,
              FR_A1_PERIOD INT2,
              SL_A1_PERIOD INT2,
              FR_C2_PERIOD INT2,
              SL_C2_PERIOD INT2,
              FR_Q2_PERIOD INT2,
              SL_Q2_PERIOD INT2,
              FR_D2_PERIOD INT2,
              SL_D2_PERIOD INT2,
              FR_A2_PERIOD INT2,
              SL_A2_PERIOD INT2.
  SELECT_MULTY: VALUE_CANCEL,
                CHNG_TYP,
                PERIOD_TYP,
                FR_C1_PERIOD,
                SL_C1_PERIOD,
                FR_Q1_PERIOD,
                SL_Q1_PERIOD,
                FR_D1_PERIOD,
                SL_D1_PERIOD,
                FR_A1_PERIOD,
                SL_A1_PERIOD,
                FR_C2_PERIOD,
                SL_C2_PERIOD,
                FR_Q2_PERIOD,
                SL_Q2_PERIOD,
                FR_D2_PERIOD,
                SL_D2_PERIOD,
                FR_A2_PERIOD,
                SL_A2_PERIOD.
  DATA_SINGLE: CHECK_IMPORTED CHAR1,
               PERIOD_UNIT  /SKN/E_SW_DURATION_UNIT.
  LV_PERIOD_UNIT = 'W'.
  SELECT_SINGLE: CHECK_IMPORTED,
                 PERIOD_UNIT.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  DATA : SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  CONVERT_MULTY: KUNNR ALPHA,
                 VBELN ALPHA,
                 MATNR MATN1. ""3-8-16
  CONVERT_MULTY_C:  AUART AUART LV_LANGU LV_SW_DEST. """Tanya 07/11/18
***ranges : R_FLD_NAME for DD03P-FIELDNAME,
***         R_FLD_VAL for DD03P-FIELDNAME .
***
***data :   FLD_NAME type FIELDNAME.
***data : i type I,
***       ci(1) type c,
***       nfields type I value 3.   "
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM,
         DATE_TO LIKE SY-DATUM .
  DATA : LANGU LIKE SY-LANGU .
  DATA : TIME_DIFF TYPE  INT4 .
***data : lv_VBELN type VBELN,
***       lv_POSNR type POSNR,
***       lv_PARVW type PARVW,
***       lv_KUNNR TYPE  KUNNR,
***       lv_KUNNR_NAME TYPE  NAME1_GP.
***data: lv_VBTYP type VBTYP.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA : LV_DATA_POSNR TYPE POSNR.
  DATA: LS_DATA LIKE LINE OF T_DATA,
        LT_DATA LIKE TABLE OF LS_DATA.
  DATA: IS_SUBST(1) TYPE C.
  DATA: LV_TABKEY LIKE CDPOS-TABKEY.
  DATA: LV_SL_PERIOD LIKE LS_DATA-SL_PERIOD,
        LV_FR_PERIOD LIKE LS_DATA-FR_PERIOD,
        LV_PERIOD_FROM LIKE LS_DATA-SL_PERIOD,
        LV_PERIOD_TO LIKE LS_DATA-SL_PERIOD.
  DATA_MULTY: VBELN_FLT        VBAK-VBELN.
  DEFINE CALC_PERIOD_TYP.
    "-- &1 - Type (C/Q/D/S)
    "-- &2 - Mat Category (1/2)
    """if ls_data-DURATION in R_FR_C1_PERIOD.
    IF LS_DATA-DURATION IN R_FR_&1&2_PERIOD.
      LS_DATA-PERIOD_TYP = 'FR'.
    ELSEIF LS_DATA-DURATION IN R_SL_&1&2_PERIOD.
      LS_DATA-PERIOD_TYP = 'SL'.
    ELSE.
    ENDIF.
    "--- Get SL / FR period strings
    GET_PERIOD_RANGE &1 &2 FR.
    GET_PERIOD_RANGE &1 &2 SL.
  END-OF-DEFINITION.
  DEFINE GET_PERIOD_RANGE.
    "-- &1 - Type (C/Q/D/S)
    "-- &2 - Mat Category (1/2)
    "-- &3 - FR/SL
    CLEAR LV_&3_PERIOD.
    READ TABLE R_&3_&1&2_PERIOD INTO RS_&3_&1&2_PERIOD INDEX 1.
    IF SY-SUBRC IS INITIAL.
      LV_PERIOD_FROM = RS_&3_&1&2_PERIOD-LOW.
        SHIFT LV_PERIOD_FROM LEFT DELETING LEADING ' '.
      LV_PERIOD_TO = RS_&3_&1&2_PERIOD-HIGH.
        SHIFT LV_PERIOD_TO LEFT DELETING LEADING ' '.
     CONCATENATE LV_PERIOD_FROM '-' LV_PERIOD_TO INTO LV_&3_PERIOD SEPARATED BY ' '.
    ENDIF.
  END-OF-DEFINITION.
  "--- Set Default values
  DATA_MULTY: FR_PERIOD INT2,
              SL_PERIOD INT2.
  RS_FR_PERIOD-SIGN = 'I'.
  RS_FR_PERIOD-OPTION = 'LE'.
  RS_FR_PERIOD-LOW = '56'.  "8 Weeks in Days
  APPEND RS_FR_PERIOD TO R_FR_PERIOD.
  RS_SL_PERIOD-SIGN = 'I'.
  RS_SL_PERIOD-OPTION = 'BT'.
  RS_SL_PERIOD-LOW = '56'.  "8 Weeks in Days
  RS_SL_PERIOD-HIGH = '84'.  "12 Weeks in Days
  APPEND RS_SL_PERIOD TO R_SL_PERIOD.
  IF R_FR_C1_PERIOD[] IS INITIAL.
    R_FR_C1_PERIOD[] = R_FR_PERIOD[].
  ENDIF.
  IF R_SL_C1_PERIOD IS INITIAL.
    R_SL_C1_PERIOD[] = R_SL_PERIOD.
  ENDIF.
  IF R_FR_Q1_PERIOD[] IS INITIAL.
    R_FR_Q1_PERIOD[] = R_FR_PERIOD[].
  ENDIF.
  IF R_SL_Q1_PERIOD IS INITIAL.
    R_SL_Q1_PERIOD[] = R_SL_PERIOD.
  ENDIF.
  IF R_FR_D1_PERIOD[] IS INITIAL.
    R_FR_D1_PERIOD[] = R_FR_PERIOD[].
  ENDIF.
  IF R_SL_D1_PERIOD IS INITIAL.
    R_SL_D1_PERIOD[] = R_SL_PERIOD.
  ENDIF.
  IF R_FR_A1_PERIOD[] IS INITIAL.
    R_FR_A1_PERIOD[] = R_FR_PERIOD[].
  ENDIF.
  IF R_SL_A1_PERIOD IS INITIAL.
    R_SL_A1_PERIOD[] = R_SL_PERIOD.
  ENDIF.
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  DATE_FROM = SY-DATUM.
  READ TABLE R_DATUM INTO RS_DATUM INDEX 1.
  IF SY-SUBRC IS INITIAL.
    DATE_FROM = RS_DATUM-LOW.
    DATE_TO = RS_DATUM-HIGH.
    IF DATE_TO < DATE_FROM.
      DATE_TO = DATE_FROM.
    ENDIF.
  ENDIF.
  "---
***   CASE lv_DATE_REF_FLD.
***     when 'ERDAT'.
***       R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
***     when 'AEDAT'.
***       R_AEDAT[] = R_DATUM[]. "Changed On
***     When 'AUDAT'.
***       R_AUDAT[] = R_DATUM[]. "Document Date (Date Received/Sent)
***   ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  "==> 1  Get CDHDR
  CHECK R_DATUM[] IS NOT INITIAL.  "!!!
  DATA: LS_CDHDR TYPE CDHDR,
        LT_CDHDR LIKE TABLE OF LS_CDHDR.
***   select *
***     from CDHDR
***     into CORRESPONDING FIELDS OF table lt_CDHDR
***     where OBJECTCLAS = 'VERKBELEG'
***       and UDATE in R_DATUM.
* Table List
  REFRESH LT_TABLES_LIST[].
  _APPEND_TABLES_LIST 'CDHDR' '' ''.
* Join condition
  REFRESH LT_JOIN_CONDITION[].
* Selection Fields
  REFRESH LT_SEL_FIELDS[].
  _ADAPT_SEL_FIELDS 'CDHDR' 'CDHDR'  LT_SEL_FIELDS LV_SW_DEST.
* Select's Condition
  CLEAR: LT_OPTIONS.
  _RANGE_TO_SEL_TABLE 'UDATE' DATUM.
  LT_OPTIONS[] = LT_OUT_WHERE_COND[].
  CLEAR LS_OPTIONS.
  LS_OPTIONS-TEXT = '( OBJECTCLAS = ''VERKBELEG'' )'.
  IF LT_OPTIONS IS NOT INITIAL.
    CONCATENATE 'AND' LS_OPTIONS-TEXT INTO LS_OPTIONS-TEXT SEPARATED BY SPACE.
  ENDIF.
  APPEND LS_OPTIONS TO LT_OPTIONS.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
    TABLES
      OPTIONS              = LT_OPTIONS[]
      DATA                 = LT_DATA_RFC[]
      TABLES_LIST          = LT_TABLES_LIST[]
      JOIN_CONDITION       = LT_JOIN_CONDITION
      SEL_FIELDS           = LT_SEL_FIELDS[]
      SORT_OPTIONS         = LT_SORT_OPTIONS[]
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
      DFIES                = LT_DFIES[]
      RETURN               = LT_RETURN[]
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  IF SY-SUBRC <> 0.
    CLEAR LT_DATA_RFC[].
  ELSE.
    _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_CDHDR LT_OUTPUT_FIELDS 1.
  ENDIF.
  SORT LT_CDHDR BY OBJECTCLAS OBJECTID CHANGENR.
  "==> 2  Get CDPOS
  CHECK LT_CDHDR[] IS NOT INITIAL.
  DATA: LS_CDPOS TYPE CDPOS,
        LT_CDPOS LIKE TABLE OF LS_CDPOS.
***   select *
***     from CDPOS
***     into CORRESPONDING FIELDS OF table lt_CDPOS
***     FOR ALL ENTRIES IN lt_CDHDR
***     where OBJECTCLAS = lt_CDHDR-OBJECTCLAS
***       and OBJECTID   = lt_CDHDR-OBJECTID
***       and CHANGENR   = lt_CDHDR-CHANGENR
***       and TABNAME  in ( 'VBAP' , 'VBEP' )
***       and FNAME in ('ABGRU' , 'WMENG' , 'EDATU')
***        and CHNGIND    = 'U'.
  CLEAR: LT_OPTIONS[], LT_DATA_RFC[], LT_TABLES_LIST[], LT_SEL_FIELDS[], LT_OUTPUT_FIELDS[],
         LT_DFIES[], LT_RETURN[], LT_OUT_WHERE_COND[].
* Table List
  REFRESH LT_TABLES_LIST[].
  _APPEND_TABLES_LIST 'CDPOS' '' ''.
* Join condition
  REFRESH LT_JOIN_CONDITION[].
  _ALL_ENTRIES_CONDITION 'OBJECTCLAS' 'OBJECTCLAS' ''.
  _ALL_ENTRIES_CONDITION 'OBJECTID' 'OBJECTID' ''.
  _ALL_ENTRIES_CONDITION 'CHANGENR' 'CHANGENR' ''.
* Selection Fields
  REFRESH LT_SEL_FIELDS[].
  _ADAPT_SEL_FIELDS 'CDPOS' 'CDPOS'  LT_SEL_FIELDS LV_SW_DEST.
* Select's Condition
  LT_OPTIONS[] = LT_OUT_WHERE_COND.
  CLEAR LS_OPTIONS.
  LS_OPTIONS-TEXT = '( TABNAME  in ( ''VBAP'' , ''VBEP'' ) )'.
  APPEND LS_OPTIONS TO LT_OPTIONS.
  LS_OPTIONS-TEXT = '( FNAME in ( ''ABGRU'' , ''WMENG'' ,  ''BMENG'',  ''EDATU'' ) )'.
  CONCATENATE 'AND' LS_OPTIONS-TEXT INTO LS_OPTIONS-TEXT SEPARATED BY SPACE.
  APPEND LS_OPTIONS TO LT_OPTIONS.
  LS_OPTIONS-TEXT = '( CHNGIND    = ''U'')'.
  CONCATENATE 'AND' LS_OPTIONS-TEXT INTO LS_OPTIONS-TEXT SEPARATED BY SPACE.
  APPEND LS_OPTIONS TO LT_OPTIONS.
*    Convert lt_CDHDR to string table
  _ALL_ENTRIES_CONVERT  LT_CDHDR  'CDHDR'  1.
*   'For All Entries' Fields
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = 'CDHDR'
    TABLES
      DFIES_TAB      = LT_ALL_ENTRIES_DFIES
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    CLEAR LT_ALL_ENTRIES_DFIES[].
  ENDIF.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
    TABLES
      OPTIONS              = LT_OPTIONS[]
      DATA                 = LT_DATA_RFC[]
      TABLES_LIST          = LT_TABLES_LIST[]
      SEL_FIELDS           = LT_SEL_FIELDS[]
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
      DFIES                = LT_DFIES[]
      RETURN               = LT_RETURN[]
      ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB[]
      ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND[]
      ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES[]
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  IF SY-SUBRC <> 0 .
    CLEAR LT_DATA_RFC[].
  ELSE.
    _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_CDPOS LT_OUTPUT_FIELDS 2.
  ENDIF.
  "==> 3  Clean CDPOS
  IF R_VALUE_CANCEL[] IS INITIAL.
    RS_VALUE_CANCEL-SIGN = 'I'.
    RS_VALUE_CANCEL-OPTION = 'EQ'.
    RS_VALUE_CANCEL-LOW = '08'.
    APPEND RS_VALUE_CANCEL TO R_VALUE_CANCEL.
    RS_VALUE_CANCEL-SIGN = 'I'.
    RS_VALUE_CANCEL-OPTION = 'EQ'.
    RS_VALUE_CANCEL-LOW = '10'.
    APPEND RS_VALUE_CANCEL TO R_VALUE_CANCEL.
  ENDIF.
  DELETE LT_CDPOS WHERE FNAME = 'ABGRU'
                   AND VALUE_NEW NOT IN R_VALUE_CANCEL.
  "==> 4  Aggregate CDPOS
  "==> 5  Get Keys
  DATA: BEGIN OF LS_OBJECT_KEY,
          OBJECTCLAS  TYPE CDOBJECTCL,
          OBJECTID  TYPE CDOBJECTV,
        END OF LS_OBJECT_KEY.
  DATA: LT_OBJECT_KEY LIKE TABLE OF LS_OBJECT_KEY.
  REFRESH LT_OBJECT_KEY.
  LOOP AT LT_CDPOS INTO LS_CDPOS.
    MOVE-CORRESPONDING LS_CDPOS TO LS_OBJECT_KEY.
    APPEND LS_OBJECT_KEY TO LT_OBJECT_KEY.
  ENDLOOP.
  SORT LT_OBJECT_KEY BY OBJECTCLAS OBJECTID.
  DELETE ADJACENT DUPLICATES FROM LT_OBJECT_KEY.
  "==> 6  Get First Date (from log)
  "  Min Ghange NR for Object Class + Object ID + FNAME
  DATA: LT_CDPOS_OBJ LIKE TABLE OF LS_CDPOS.
  IF LT_OBJECT_KEY[] IS NOT INITIAL.
***     select *
***       from CDPOS
***       into CORRESPONDING FIELDS OF table lt_CDPOS_obj
***       FOR ALL ENTRIES IN lt_OBJECT_Key
***       where OBJECTCLAS = lt_OBJECT_Key-OBJECTCLAS
***         and OBJECTID   = lt_OBJECT_Key-OBJECTID
***         and TABNAME  = 'VBEP'    "    in ( 'VBAP' , 'VBEP' )
***          and FNAME = 'LDDAT'       "  in ('ABGRU' , 'MENGE' , 'EDATU')
***         .  " and CHNGIND    = 'U'.
    CLEAR: LT_OPTIONS[], LT_DATA_RFC[], LT_TABLES_LIST[], LT_SEL_FIELDS[], LT_OUTPUT_FIELDS[],
           LT_DFIES[], LT_RETURN[], LT_OUT_WHERE_COND[].
    REFRESH: LT_ALL_ENTRIES_TAB, LT_ALL_ENTRIES_COND, LT_ALL_ENTRIES_DFIES.
* Table List
    REFRESH LT_TABLES_LIST[].
    _APPEND_TABLES_LIST 'CDPOS' '' ''.
* Join condition
    REFRESH LT_JOIN_CONDITION[].
    _ALL_ENTRIES_CONDITION 'OBJECTCLAS' 'OBJECTCLAS' ''.
    _ALL_ENTRIES_CONDITION 'OBJECTID' 'OBJECTID' ''.
* Selection Fields
    REFRESH LT_SEL_FIELDS[].
    _ADAPT_SEL_FIELDS 'CDPOS' 'CDPOS'  LT_SEL_FIELDS LV_SW_DEST.
* Select's Condition
    LT_OPTIONS[] = LT_OUT_WHERE_COND.
    CLEAR LS_OPTIONS.
    LS_OPTIONS-TEXT = '( TABNAME  =  ''VBEP'' )'.
    APPEND LS_OPTIONS TO LT_OPTIONS.
    "ls_options-text = '( FNAME = ''LDDAT'' )'.
    CONCATENATE  '''' LV_DATE_REF_FLD ''''  INTO LS_OPTIONS-TEXT.
    CONCATENATE  '( FNAME = ' LS_OPTIONS-TEXT ' )'  INTO LS_OPTIONS-TEXT SEPARATED BY SPACE.
    CONCATENATE 'AND' LS_OPTIONS-TEXT INTO LS_OPTIONS-TEXT SEPARATED BY SPACE.
    APPEND LS_OPTIONS TO LT_OPTIONS.
*    Convert lt_CDHDR to string table
    _ALL_ENTRIES_CONVERT  LT_OBJECT_KEY  'CDPOS'  7.
*   'For All Entries' Fields
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        TABNAME        = 'CDPOS'
      TABLES
        DFIES_TAB      = LT_ALL_ENTRIES_DFIES
      EXCEPTIONS
        NOT_FOUND      = 1
        INTERNAL_ERROR = 2
        OTHERS         = 3.
    IF SY-SUBRC NE 0.
      CLEAR LT_ALL_ENTRIES_DFIES[].
    ENDIF.
    CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
      DESTINATION LV_SW_DEST
      TABLES
        OPTIONS              = LT_OPTIONS[]
        DATA                 = LT_DATA_RFC[]
        TABLES_LIST          = LT_TABLES_LIST[]
        SEL_FIELDS           = LT_SEL_FIELDS[]
        OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
        DFIES                = LT_DFIES[]
        RETURN               = LT_RETURN[]
        ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB[]
        ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND[]
        ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES[]
      EXCEPTIONS
        TABLE_NOT_AVAILABLE  = 1
        TABLE_WITHOUT_DATA   = 2
        OPTION_NOT_VALID     = 3
        FIELD_NOT_VALID      = 4
        NOT_AUTHORIZED       = 5
        DATA_BUFFER_EXCEEDED = 6
        OTHERS               = 7.
    IF SY-SUBRC <> 0 .
      CLEAR LT_DATA_RFC[].
    ELSE.
      _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_CDPOS_OBJ  LT_OUTPUT_FIELDS 7.
    ENDIF.
    SORT LT_CDPOS_OBJ BY TABKEY TABNAME FNAME CHANGENR.
    DELETE ADJACENT DUPLICATES FROM LT_CDPOS_OBJ COMPARING TABKEY TABNAME FNAME.
    SORT LT_CDPOS_OBJ BY TABKEY TABNAME FNAME.
  ENDIF.
  "==> 7  Complete First Date (from VBEP) if no change detected
  "==> 8  Prepare Key
  DATA: BEGIN OF LS_CHANGED_KEY.
          INCLUDE STRUCTURE CDPOS.
  DATA:   VBELN  TYPE VBELN_VA,
          POSNR  TYPE POSNR_VA,
          ETENR  TYPE ETENR,
        END OF LS_CHANGED_KEY.
  DATA:  LT_CHANGED_KEY LIKE TABLE OF LS_CHANGED_KEY.
  DATA: BEGIN OF LS_KEY,
          MANDT  TYPE MANDT,
          VBELN  TYPE VBELN_VA,
          POSNR  TYPE POSNR_VA,
          ETENR  TYPE ETENR,
        END OF LS_KEY.
  DATA: LT_KEY LIKE TABLE OF LS_KEY.
  LOOP AT LT_CDPOS INTO LS_CDPOS.
    MOVE-CORRESPONDING LS_CDPOS TO LS_CHANGED_KEY.
    LS_KEY = LS_CDPOS-TABKEY.
    MOVE-CORRESPONDING LS_KEY TO LS_CHANGED_KEY.
    APPEND LS_CHANGED_KEY TO LT_CHANGED_KEY.
    APPEND LS_KEY TO LT_KEY.
  ENDLOOP.
  "==> 9  Get SD Documents (VBAK/VBAP/VBEP) --> Only requiered fields
  DATA: LS_VBAK TYPE VBAK,    " To bechanged
        LT_VBAK LIKE TABLE OF LS_VBAK.
  DATA: BEGIN OF LS_VBAK_KEY,
          VBELN  TYPE VBELN_VA,
        END OF LS_VBAK_KEY.
  DATA: LT_VBAK_KEY LIKE TABLE OF LS_VBAK_KEY.
  "data: ls_VBAP type VBAP,    " To bechanged
  DATA: BEGIN OF LS_VBAP.
          INCLUDE STRUCTURE VBAP.
  """ data: YLDDAT type DATUM,
      DATA YAUGRU TYPE AUGRU.
  DATA:      END OF LS_VBAP.
  DATA: LT_VBAP LIKE TABLE OF LS_VBAP.
  DATA: BEGIN OF LS_VBAP_KEY,
          VBELN  TYPE VBELN_VA,
          POSNR  TYPE POSNR_VA,
        END OF LS_VBAP_KEY.
  DATA: LT_VBAP_KEY LIKE TABLE OF LS_VBAP_KEY.
  DATA: LS_VBEP TYPE VBEP,    " To bechanged
        LT_VBEP LIKE TABLE OF LS_VBEP,
        LS_VBEP_TMP TYPE VBEP.
  DATA: VBEP_TABIX LIKE SY-TABIX.
  DATA: BEGIN OF LS_VBEP_KEY,
          VBELN  TYPE VBELN_VA,
          POSNR  TYPE POSNR_VA,
          ETENR  TYPE ETENR,
        END OF LS_VBEP_KEY.
  DATA: LT_VBEP_KEY LIKE TABLE OF LS_VBEP_KEY.
  REFRESH: LT_VBAK_KEY, LT_VBAP_KEY, LT_VBEP_KEY.
  DELETE LT_KEY WHERE VBELN NOT IN R_VBELN.
  LOOP AT LT_KEY INTO LS_KEY.
    MOVE-CORRESPONDING LS_KEY TO LS_VBAK_KEY.
    APPEND LS_VBAK_KEY TO LT_VBAK_KEY.
    "--
    MOVE-CORRESPONDING LS_KEY TO LS_VBAP_KEY.
    APPEND LS_VBAP_KEY TO LT_VBAP_KEY.
    "--
    MOVE-CORRESPONDING LS_KEY TO LS_VBEP_KEY.
    APPEND LS_VBEP_KEY TO LT_VBEP_KEY.
  ENDLOOP.
  SORT LT_VBAK_KEY.
  DELETE ADJACENT DUPLICATES FROM LT_VBAK_KEY.
  SORT LT_VBAP_KEY.
  DELETE ADJACENT DUPLICATES FROM LT_VBAP_KEY.
  SORT LT_VBEP_KEY.
  DELETE ADJACENT DUPLICATES FROM LT_VBEP_KEY.
  CHECK LT_VBAK_KEY[] IS NOT INITIAL. "!!!
***  select *
***    from VBAK
***    into CORRESPONDING FIELDS OF table lt_VBAK
***    FOR ALL ENTRIES IN lt_VBAK_Key
***    where VBELN = lt_VBAK_Key-VBELN.
  CLEAR: LT_OPTIONS[], LT_DATA_RFC[], LT_TABLES_LIST[], LT_SEL_FIELDS[], LT_OUTPUT_FIELDS[],
         LT_DFIES[], LT_RETURN[], LT_OUT_WHERE_COND[].
* Table List
  REFRESH LT_TABLES_LIST[].
  _APPEND_TABLES_LIST 'VBAK' '' ''.
* Join condition
  REFRESH LT_JOIN_CONDITION[].
  REFRESH LT_ALL_ENTRIES_COND.
  _ALL_ENTRIES_CONDITION 'VBELN' 'VBELN' ''.
* Selection Fields
  REFRESH LT_SEL_FIELDS[].
  _ADAPT_SEL_FIELDS 'VBAK' 'VBAK'  LT_SEL_FIELDS LV_SW_DEST.
  "_adapt_sel_fields 'VBAK' '/SKN/S_SW_10_01_ORD_CHNG'  lt_sel_fields lv_sw_dest.
* Select's Condition
*    Convert lt_CDHDR to string table
  _ALL_ENTRIES_CONVERT  LT_VBAK_KEY  'VBAK'  3.
*   'For All Entries' Fields
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = 'VBAK'
    TABLES
      DFIES_TAB      = LT_ALL_ENTRIES_DFIES
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    CLEAR LT_ALL_ENTRIES_DFIES[].
  ENDIF.
  "--- Take Key Only
  DELETE LT_ALL_ENTRIES_DFIES WHERE KEYFLAG IS INITIAL.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
    TABLES
      OPTIONS              = LT_OPTIONS[]
      DATA                 = LT_DATA_RFC[]
      TABLES_LIST          = LT_TABLES_LIST[]
      SEL_FIELDS           = LT_SEL_FIELDS[]
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
      DFIES                = LT_DFIES[]
      RETURN               = LT_RETURN[]
      ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB[]
      ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND[]
      ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES[]
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  IF SY-SUBRC <> 0 .
    CLEAR LT_DATA_RFC[].
  ELSE.
    _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_VBAK LT_OUTPUT_FIELDS 3.
  ENDIF.
  DELETE LT_VBAK WHERE KUNNR NOT IN R_KUNNR.
  DELETE LT_VBAK WHERE VBELN NOT IN R_VBELN.
  DELETE LT_VBAK WHERE VKORG NOT IN R_VKORG.
  DELETE LT_VBAK WHERE VTWEG NOT IN R_VTWEG.
  DELETE LT_VBAK WHERE SPART NOT IN R_SPART.
  DELETE LT_VBAK WHERE VBTYP NOT IN R_VBTYP.
  DELETE LT_VBAK WHERE AUART NOT IN R_AUART.
  SORT LT_VBAK BY VBELN.
  "--- Remove unused Docs
  REFRESH R_VBELN_FLT.
  LOOP AT LT_VBAK INTO LS_VBAK.
    RS_VBELN_FLT-SIGN = 'I'.
    RS_VBELN_FLT-OPTION = 'EQ'.
    RS_VBELN_FLT-LOW = LS_VBAK-VBELN.
    APPEND RS_VBELN_FLT TO R_VBELN_FLT.
  ENDLOOP.
  DELETE LT_VBAP_KEY WHERE VBELN NOT IN R_VBELN_FLT.
  "--- Remove unused Docs
***  select *
***    from VBAP
***    into CORRESPONDING FIELDS OF table lt_VBAP
***    FOR ALL ENTRIES IN lt_VBAP_Key
***    where VBELN = lt_VBAP_Key-VBELN
***      and POSNR = lt_VBAP_Key-POSNR.
  CLEAR: LT_OPTIONS[], LT_DATA_RFC[], LT_TABLES_LIST[], LT_SEL_FIELDS[], LT_OUTPUT_FIELDS[],
         LT_DFIES[], LT_RETURN[], LT_OUT_WHERE_COND[].
* Table List
  REFRESH LT_TABLES_LIST[].
  _APPEND_TABLES_LIST 'VBAP' '' ''.
* Join condition
  REFRESH LT_JOIN_CONDITION[].
  REFRESH LT_ALL_ENTRIES_COND.
  _ALL_ENTRIES_CONDITION 'VBELN' 'VBELN' ''.
  _ALL_ENTRIES_CONDITION 'POSNR' 'POSNR' ''.
* Selection Fields
  REFRESH LT_SEL_FIELDS[].
  "_adapt_sel_fields 'VBAP' 'VBAP'  lt_sel_fields lv_sw_dest.
  _ADAPT_SEL_FIELDS 'VBAP' '/SKN/S_SW_10_01_ORD_CHNG'  LT_SEL_FIELDS LV_SW_DEST.
* Select's Condition
*    Convert lt_VBAK_Key to string table
  _ALL_ENTRIES_CONVERT  LT_VBAP_KEY  'VBAP'  4.
*   'For All Entries' Fields
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = 'VBAP'
    TABLES
      DFIES_TAB      = LT_ALL_ENTRIES_DFIES
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    CLEAR LT_ALL_ENTRIES_DFIES[].
  ENDIF.
  "--- Take Key Only
  DELETE LT_ALL_ENTRIES_DFIES WHERE KEYFLAG IS INITIAL.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
    TABLES
      OPTIONS              = LT_OPTIONS[]
      DATA                 = LT_DATA_RFC[]
      TABLES_LIST          = LT_TABLES_LIST[]
      SEL_FIELDS           = LT_SEL_FIELDS[]
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
      DFIES                = LT_DFIES[]
      RETURN               = LT_RETURN[]
      ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB[]
      ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND[]
      ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES[]
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  IF SY-SUBRC <> 0 .
    CLEAR LT_DATA_RFC[].
  ELSE.
    _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_VBAP LT_OUTPUT_FIELDS 4.
  ENDIF.
  SORT LT_VBAP BY VBELN POSNR.
***  select *
***    from VBEP
***    into CORRESPONDING FIELDS OF table lt_VBEP
***    FOR ALL ENTRIES IN lt_VBEP_Key
***    where VBELN = lt_VBEP_Key-VBELN
***      and POSNR = lt_VBEP_Key-POSNR
***      """and ETENR  = lt_VBEP_Key-ETENR.
  "--- Remove unused Docs
  DELETE LT_VBEP_KEY WHERE VBELN NOT IN R_VBELN_FLT.
  "--- Remove unused Docs
  CLEAR: LT_OPTIONS[], LT_DATA_RFC[], LT_TABLES_LIST[], LT_SEL_FIELDS[], LT_OUTPUT_FIELDS[],
         LT_DFIES[], LT_RETURN[], LT_OUT_WHERE_COND[].
* Table List
  REFRESH LT_TABLES_LIST[].
  _APPEND_TABLES_LIST 'VBEP' '' ''.
* Join condition
  REFRESH LT_JOIN_CONDITION[].
  REFRESH LT_ALL_ENTRIES_COND.
  _ALL_ENTRIES_CONDITION 'VBELN' 'VBELN' ''.
  _ALL_ENTRIES_CONDITION 'POSNR' 'POSNR' ''.
  """"_all_entries_condition 'ETENR' 'ETENR' ''.
* Selection Fields
  REFRESH LT_SEL_FIELDS[].
  _ADAPT_SEL_FIELDS 'VBEP' 'VBEP'  LT_SEL_FIELDS LV_SW_DEST.
  "_adapt_sel_fields 'VBEP' '/SKN/S_SW_10_01_ORD_CHNG'  lt_sel_fields lv_sw_dest.
* Select's Condition
*    Convert lt_VBAK_Key to string table
  _ALL_ENTRIES_CONVERT  LT_VBEP_KEY  'VBEP'  5.
*   'For All Entries' Fields
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = 'VBEP'
    TABLES
      DFIES_TAB      = LT_ALL_ENTRIES_DFIES
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    CLEAR LT_ALL_ENTRIES_DFIES[].
  ENDIF.
  "--- Take Key Only
  DELETE LT_ALL_ENTRIES_DFIES WHERE KEYFLAG IS INITIAL.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
    TABLES
      OPTIONS              = LT_OPTIONS[]
      DATA                 = LT_DATA_RFC[]
      TABLES_LIST          = LT_TABLES_LIST[]
      SEL_FIELDS           = LT_SEL_FIELDS[]
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
      DFIES                = LT_DFIES[]
      RETURN               = LT_RETURN[]
      ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB[]
      ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND[]
      ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES[]
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  IF SY-SUBRC <> 0 .
    CLEAR LT_DATA_RFC[].
  ELSE.
    _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_VBEP LT_OUTPUT_FIELDS 5.
  ENDIF.
  SORT LT_VBEP BY VBELN POSNR ETENR.
  "==> 10  Prepare Calculations
  REFRESH LT_DATA.
  LOOP AT LT_CHANGED_KEY INTO LS_CHANGED_KEY.
    CLEAR LS_DATA.
    MOVE-CORRESPONDING LS_CHANGED_KEY TO LS_DATA.
    "--- Set CHNG_TYP
    CASE LS_CHANGED_KEY-FNAME.
      WHEN 'ABGRU'.
        LS_DATA-CHNG_TYP = 'C'.
      WHEN 'WMENG'.
        LS_DATA-CHNG_TYP = 'Q'.
      WHEN 'BMENG'.
        LS_DATA-CHNG_TYP = 'Q'.
        CONTINUE .  "!!!
      WHEN 'EDATU'.
        IF LS_CHANGED_KEY-VALUE_NEW > LS_CHANGED_KEY-VALUE_OLD.
          LS_DATA-CHNG_TYP = 'D'.
        ELSE.
          LS_DATA-CHNG_TYP = 'A'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    CLEAR LS_VBEP.
    CLEAR VBEP_TABIX.
    IF LS_CHANGED_KEY-ETENR > 0.
      READ TABLE LT_VBEP INTO LS_VBEP
                         WITH KEY VBELN  = LS_CHANGED_KEY-VBELN
                                  POSNR  = LS_CHANGED_KEY-POSNR
                                  ETENR  = LS_CHANGED_KEY-ETENR
                         BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        VBEP_TABIX = SY-TABIX.
      ENDIF.
    ELSE.  " Cancelletion (no Scheduling Line)
      READ TABLE LT_VBEP INTO LS_VBEP
                         WITH KEY VBELN  = LS_CHANGED_KEY-VBELN
                                  POSNR  = LS_CHANGED_KEY-POSNR
                         BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LOOP AT LT_VBEP INTO LS_VBEP FROM SY-TABIX
                        WHERE VBELN = LS_CHANGED_KEY-VBELN
                          AND POSNR = LS_CHANGED_KEY-POSNR.
          IF LS_VBEP-WMENG <> 0.
            VBEP_TABIX = SY-TABIX.
            " Complete the TABKEY !!!
            LV_TABKEY = LS_CHANGED_KEY-TABKEY.
            LV_TABKEY+19 = LS_VBEP-ETENR.
            LS_CHANGED_KEY-TABKEY = LV_TABKEY.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    CLEAR LS_CDHDR.
    READ TABLE LT_CDHDR INTO LS_CDHDR
                       WITH KEY OBJECTCLAS  = LS_CHANGED_KEY-OBJECTCLAS
                                OBJECTID    = LS_CHANGED_KEY-OBJECTID
                                CHANGENR    = LS_CHANGED_KEY-CHANGENR
                       BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LS_CDHDR TO LS_DATA.
    ENDIF.
    CLEAR LS_VBAP.
    READ TABLE LT_VBAP INTO LS_VBAP
                       WITH KEY VBELN  = LS_CHANGED_KEY-VBELN
                                POSNR  = LS_CHANGED_KEY-POSNR
                       BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LS_VBAP TO LS_DATA.
    ENDIF.
    CLEAR LS_VBAK.
    READ TABLE LT_VBAK INTO LS_VBAK
                       WITH KEY VBELN  = LS_CHANGED_KEY-VBELN
                       BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LS_VBAK TO LS_DATA.
    ENDIF.
    IF LS_VBEP IS NOT INITIAL.
      MOVE-CORRESPONDING LS_VBEP TO LS_DATA.
    ENDIF.
    "--- Set ref Date
    "ls_data-REF_DATE = ls_VBEP-LDDAT.   " !!!
    CONCATENATE 'ls_VBEP-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    IF  IS ASSIGNED.
      LS_DATA-REF_DATE =  .
      LS_DATA-REF_DATE_ORG =  .
    ENDIF.
    "--- Correct VBEP atttributes id it was substituted
    CLEAR IS_SUBST.
    IF VBEP_TABIX > 1.
      IF LS_VBEP-WMENG = 0.  " Suuposed to substituted
        "--- Take Ordered qty and ref date from the previous record
        DO.
          VBEP_TABIX = VBEP_TABIX - 1.
          READ TABLE LT_VBEP INTO LS_VBEP_TMP INDEX VBEP_TABIX.
          IF LS_VBEP_TMP-VBELN = LS_VBEP-VBELN AND
             LS_VBEP_TMP-POSNR = LS_VBEP-POSNR AND
             LS_VBEP_TMP-WMENG > 0.
            MOVE LS_VBEP_TMP-WMENG TO LS_DATA-WMENG.
            CONCATENATE 'ls_VBEP_tmp-' LV_DATE_REF_FLD INTO FLD .
            ASSIGN (FLD) TO .
            IF  IS ASSIGNED.
              LS_DATA-REF_DATE =  .
            ENDIF.
            IS_SUBST = 'X'.
            LV_TABKEY = LS_CHANGED_KEY-TABKEY.
            LV_TABKEY+19 = LS_VBEP_TMP-ETENR.
            EXIT.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
    "-- Get the Change log oroginal value.
    IF IS_SUBST IS INITIAL.
      LV_TABKEY = LS_CHANGED_KEY-TABKEY.
    ENDIF.
    READ TABLE LT_CDPOS_OBJ INTO LS_CDPOS
                             WITH KEY TABKEY = LV_TABKEY    "ls_Changed_Key-TABKEY
                                     TABNAME = 'VBEP'   "ls_Changed_Key-TABNAME
                                     FNAME   =  LV_DATE_REF_FLD   "  'LDDAT'        "ls_Changed_Key-FNAME
                             BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      LS_DATA-REF_DATE = LS_CDPOS-VALUE_OLD.
    ENDIF.
    APPEND LS_DATA TO LT_DATA.
  ENDLOOP.
  DELETE LT_DATA WHERE CHNG_TYP NOT IN R_CHNG_TYP.
***  loop at lt_VBEP into ls_VBEP.
***
***    read table lt_VBAP into ls_VBAP
***                       with key VBELN  = ls_VBEP-VBELN
***                                POSNR  = ls_VBEP-POSNR
***                       BINARY SEARCH.
***    if sy-subrc is initial.
***      move-CORRESPONDING ls_VBAP to ls_data.
***    endif.
***    read table lt_VBAK into ls_VBAK
***                       with key VBELN  = ls_VBEP-VBELN
***                       BINARY SEARCH.
***    if sy-subrc is initial.
***      move-CORRESPONDING ls_VBAK to ls_data.
***    endif.
***    move-CORRESPONDING ls_VBEP to ls_data.
***    "--- Set ref Date
***    ls_data-REF_DATE = ls_VBEP-LDDAT.   " !!!
***    append ls_data to lt_data.
***  endloop.
  LOOP AT LT_DATA INTO LS_DATA .
    SY_TABIX = SY-TABIX .
    LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = LS_DATA-UDATE
        T_FROM      = SY-UZEIT
        D_TO        = LS_DATA-REF_DATE
        T_TO        = SY-UZEIT
        TIME_UNIT   = LV_DURATION_UNIT  "'D'
      IMPORTING
        TIME_DIFF   = TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      IF TIME_DIFF < '999999'.
        LS_DATA-DURATION  = TIME_DIFF .
      ELSE.
        LS_DATA-DURATION  = '999999'.
      ENDIF.
    ENDIF.
    MODIFY LT_DATA FROM LS_DATA  INDEX SY_TABIX.
  ENDLOOP.
  DELETE LT_DATA WHERE KUNNR NOT IN R_KUNNR.
  DELETE LT_DATA WHERE VBELN NOT IN R_VBELN.
  DELETE LT_DATA WHERE VKORG NOT IN R_VKORG.
  DELETE LT_DATA WHERE VTWEG NOT IN R_VTWEG.
  DELETE LT_DATA WHERE SPART NOT IN R_SPART.
  DELETE LT_DATA WHERE VBTYP NOT IN R_VBTYP.
  DELETE LT_DATA WHERE AUART NOT IN R_AUART.
  DELETE LT_DATA WHERE MATNR NOT IN R_MATNR.
  DELETE LT_DATA WHERE WERKS NOT IN R_WERKS.
  DELETE LT_DATA WHERE MATNR IS INITIAL.
  "==> 10  Get Material Attr
  DATA: BEGIN OF LS_MAT_PLANT,
           MATNR TYPE MATNR,
           WERKS TYPE WERKS_D,
           IS_IMPORTED TYPE CHAR1,
          END OF LS_MAT_PLANT.
  DATA: LT_MAT_PLANT LIKE TABLE OF LS_MAT_PLANT.
  DATA: LV_IS_IMPORTED TYPE CHAR1,
        LV_IMPORTED_TYP TYPE CHAR1.
  DATA: LV_FM LIKE  RS38L-NAME,
        LV_FM_IS_EXIST TYPE CHAR1.
  LOOP AT LT_VBAP INTO LS_VBAP.
    MOVE-CORRESPONDING LS_VBAP TO LS_MAT_PLANT.
    APPEND LS_MAT_PLANT TO LT_MAT_PLANT.
  ENDLOOP.
  SORT LT_MAT_PLANT BY MATNR WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_MAT_PLANT.
  "-- Calculate Material Attributes
  IF LV_CHECK_IMPORTED IS NOT INITIAL .
    LV_FM = '/SKN/F_SW_200005_IS_IMP_COMP'.  "!!!!!
    LV_FM_IS_EXIST = 'X'.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        FUNCNAME              = LV_FM
***     IMPORTING
***       GROUP                    = GROUP
***       INCLUDE                  = INCLUDE
***       NAMESPACE                = NAMESPACE
***       STR_AREA                 = STR_AREA
      EXCEPTIONS
        FUNCTION_NOT_EXIST    = 1
        COMMUNICATION_FAILURE = 11
        SYSTEM_FAILURE        = 12
        OTHERS                = 9.
    IF SY-SUBRC <> 0.
      CLEAR LV_FM_IS_EXIST.
    ENDIF.
    IF LV_FM_IS_EXIST IS NOT INITIAL.
      LOOP AT LT_MAT_PLANT INTO LS_MAT_PLANT.
        SY_TABIX = SY-TABIX.
        CLEAR LV_IS_IMPORTED.
        "--- Call FM for MAT, Plant
        CALL FUNCTION LV_FM     """   '/SKN/F_SW_200005_IS_IMP_COMP'
          EXPORTING
            SW_DEST               = LV_SW_DEST
            MATNR                 = LS_MAT_PLANT-MATNR
            WERKS                 = LS_MAT_PLANT-WERKS
          IMPORTING
            IS_IMPORTED           = LV_IS_IMPORTED
          EXCEPTIONS
            WRONG_PARAMETERS      = 1
            MISSING_AUTHORIZATION = 2
            OTHERS                = 3.
        IF SY-SUBRC <> 0.
        ENDIF.
        LS_MAT_PLANT-IS_IMPORTED = LV_IS_IMPORTED.
        MODIFY LT_MAT_PLANT FROM LS_MAT_PLANT INDEX SY_TABIX..
      ENDLOOP.
    ENDIF.
  ENDIF.
  "--- Calculate PERIOD_TYP
  LOOP AT LT_DATA INTO LS_DATA .
    SY_TABIX = SY-TABIX .
    IF LS_DATA-DURATION < 0.
      IF LS_DATA-CHNG_TYP =  'A' OR  LS_DATA-CHNG_TYP =  'D'.
        LS_DATA-CHNG_TYP = 'L'.    "!!!
      ENDIF.
    ENDIF.
    CLEAR LS_DATA-PERIOD_TYP.
    LV_IMPORTED_TYP = '1'.
    CLEAR LV_IS_IMPORTED.
    READ TABLE LT_MAT_PLANT INTO LS_MAT_PLANT
                            WITH KEY MATNR = LS_DATA-MATNR
                                     WERKS = LS_DATA-WERKS
                            BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IF LS_MAT_PLANT-IS_IMPORTED IS NOT INITIAL.
        LV_IMPORTED_TYP = '2'.
      ENDIF.
    ENDIF.
    IF LV_IS_IMPORTED IS INITIAL.
      CASE LS_DATA-CHNG_TYP.
        WHEN 'C'.
***        if ls_data-DURATION in R_FR_C1_PERIOD.
***          ls_data-PERIOD_TYP = 'FR'.
***        elseif ls_data-DURATION in R_SL_C1_PERIOD.
***          ls_data-PERIOD_TYP = 'SL'.
***        else.
***        endif.
          CALC_PERIOD_TYP C 1.
        WHEN 'Q'.
          CALC_PERIOD_TYP Q 1.
        WHEN 'D'.
          CALC_PERIOD_TYP D 1.
        WHEN 'A'.
          CALC_PERIOD_TYP A 1.
        WHEN OTHERS.
      ENDCASE.
    ELSE.
      CASE LS_DATA-CHNG_TYP.
        WHEN 'C'.
          CALC_PERIOD_TYP C 2.
        WHEN 'Q'.
          CALC_PERIOD_TYP Q 2.
        WHEN 'D'.
          CALC_PERIOD_TYP D 2.
        WHEN 'A'.
          CALC_PERIOD_TYP A 2.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
    LS_DATA-SL_PERIOD = LV_SL_PERIOD.
    LS_DATA-FR_PERIOD = LV_FR_PERIOD.
    LS_DATA-IMP_TYP = LV_IMPORTED_TYP.
    MODIFY LT_DATA FROM LS_DATA  INDEX SY_TABIX.
  ENDLOOP.
  DELETE LT_DATA WHERE CHNG_TYP NOT IN R_CHNG_TYP.
  DELETE LT_DATA WHERE PERIOD_TYP NOT IN R_PERIOD_TYP.
  """delete t_data where DURATION  not in R_DURATION .
******************************************************************************
*  Get Descriptions
************************************************************************
  IF LV_DESC IS NOT INITIAL.
    LOOP AT LT_DATA INTO LS_DATA .
      SY_TABIX = SY-TABIX .
      CALL FUNCTION '/SKN/FC_SW_10_CUST_DESC'
        EXPORTING
          KUNNR          = LS_DATA-KUNNR
          SW_DEST        = LV_SW_DEST
        IMPORTING
          CUST_DESC      = LS_DATA-KUNNR_DESC
          LAND1          = LS_DATA-LAND1
        EXCEPTIONS
          WRONG_CUSTOMER = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
      ENDIF.
      MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
    ENDLOOP.
    LOOP AT LT_DATA INTO LS_DATA .
      SY_TABIX = SY-TABIX .
      CALL FUNCTION '/SKN/FC_SW_10_MATERIAL_DESC'
        EXPORTING
          MATNR         = LS_DATA-MATNR
          LANGU         = LV_LANGU
          SW_DEST       = LV_SW_DEST
        IMPORTING
          MATERIAL_DESC = LS_DATA-MATERIAL_DESC
        EXCEPTIONS
          WRONG_CODE    = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
      ENDIF.
      CALL FUNCTION '/SKN/FC_SW_10_PLANT_DESC'
        EXPORTING
          WERKS      = LS_DATA-WERKS
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          PLANT_DESC = LS_DATA-WERKS_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
      IF SY-SUBRC <> 0.
      ENDIF.
      CALL FUNCTION '/SKN/FC_SW_10_SALES_ORG_DESC'
        EXPORTING
          VKORG          = LS_DATA-VKORG
          LANGU          = LV_LANGU
          SW_DEST        = LV_SW_DEST
        IMPORTING
          SALES_ORG_DESC = LS_DATA-VKORG_DESC
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
      ENDIF.
      PERFORM GET_CHNG_TYP_DESC USING LS_DATA-CHNG_TYP
                                CHANGING LS_DATA-CHNG_TYP_DESC.
      CALL FUNCTION '/SKN/F_SW_10_COUNTRY_DESC'
        EXPORTING
          LAND1      = LS_DATA-LAND1
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          LANDX      = LS_DATA-LAND_DESC
*         NATIO      =
*         LANDX50    =
*         NATIO50    =
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
      IF SY-SUBRC <> 0.
      ENDIF.
      PERFORM GET_IMP_TYP_DESC USING LS_DATA-IMP_TYP
                                CHANGING LS_DATA-IMP_TYP_DESC.
      PERFORM GET_PERIOD_TYP_DESC USING LS_DATA-PERIOD_TYP
                                    CHANGING LS_DATA-PERIOD_TYP_DESC.
      CALL FUNCTION '/SKN/FC_SW_10_AUGRU_DESC'
        EXPORTING
          AUGRU            = LS_DATA-AUGRU
          LANGU            = LV_LANGU
          SW_DEST          = LV_SW_DEST
        IMPORTING
          AUGRU_DESC       = LS_DATA-AUGRU_DESC
        EXCEPTIONS
          WRONG_CODE       = 1
          OTHERS           = 2.
      IF SY-SUBRC <> 0.
      ENDIF.
      CALL FUNCTION '/SKN/FC_SW_10_AUGRU_DESC'
        EXPORTING
          AUGRU            = LS_DATA-YAUGRU
          LANGU            = LV_LANGU
          SW_DEST          = LV_SW_DEST
        IMPORTING
          AUGRU_DESC       = LS_DATA-YAUGRU_DESC
        EXCEPTIONS
          WRONG_CODE       = 1
          OTHERS           = 2.
      IF SY-SUBRC <> 0.
      ENDIF.
      MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
    ENDLOOP.
*MATERIAL_DESC
  ENDIF.
  T_DATA[] = LT_DATA[].
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
