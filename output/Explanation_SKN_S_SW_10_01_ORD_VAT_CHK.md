# Exception Indicator: SD Order - VAT Check ( SW_10_01_ORD_VAT_CHK)

## General Overview

This Exception Indicator identifies sales order items whose posted tax amount differs from the tax amount calculated from item net value and a configured conventional VAT rate, surfacing VAT mismatches for review.

This EI serves as an essential control for sales order tax validation by:

- Reading sales order header and item data for orders in the selected monitoring window
- Calculating expected tax per item from item net value and the conventional VAT rate setting
- Comparing calculated tax to the posted tax amount on each line and retaining rows where the difference matches configured tolerance
- Supporting explicit handling of zero-tax lines when no posted tax is present
- Enriching results with customer and material pricing status descriptions for reviewer context

Typical use includes periodic VAT consistency checks on outbound orders, sampling orders after pricing or tax condition changes, and audit support where posted item tax must align with expected rate-based tax. Results are intended for exception workflows rather than full order extracts.

The routine selects order lines from header and item data, applies age filtering on the chosen reference date, computes tax difference per line, filters by configured difference criteria, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor sales order item tax against expected VAT amounts creates multiple risks across billing, compliance, and customer account management:

**Sales and Tax Compliance Risks**

- Posted item tax that diverges from rate-based expectations can indicate incorrect tax conditions or manual overrides
- Zero-tax lines without explicit zero-VAT handling can either be missed or incorrectly flagged in bulk reviews
- Concentrations of tax differences by customer or sales organization can signal systematic configuration gaps

**Operational Risks**

- Monitoring windows misaligned with order entry cadence can exclude recent tax exceptions or retain resolved cases
- Difference thresholds that are too wide or too narrow can hide material mismatches or create reviewer fatigue
- Item lines with manual pricing status exceptions may need separate scoping to keep review queues actionable

**Control and Audit Risks**

- Weak tax-difference monitoring reduces evidence that flagged orders were reviewed before billing or period close
- Lack of recurring exception review limits accountability for sales and finance follow-up on tax inconsistencies
- Missing customer and organizational context delays escalation of commercially significant cases

## Suggested Resolution

**Immediate Response**

- Review flagged order lines for posted tax, calculated tax, difference amount, customer, and sales organization
- Confirm with sales or finance whether the posted tax is correct or requires correction through standard SD pricing and tax processes
- Prioritize high-value customers and large absolute tax differences for immediate follow-up

**System Assessment**

- Validate lookback window, reference-date field, and age threshold settings against order review cadence
- Review zero-tax handling and tax-difference tolerance so results reflect truly exceptional mismatches
- Compare exception counts by sales organization, document type, and customer to identify systematic gaps

**Corrective Actions**

- Correct tax conditions or order data through standard processes where review confirms action is required
- Adjust monitoring scope and difference criteria after cleanup so results stay actionable
- Document review outcomes and schedule recurring runs before billing or period-close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 3 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 4 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 5 | BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 6 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 7 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 8 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 9 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 10 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 11 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 12 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 13 | FORWDAYS | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |  |
| 14 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 15 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 16 | MANAGE_IN_UTC | CHAR | 1 | 0 |  | XFELD |  |
| 17 | MPROK | Manual price | CHAR | 1 | 0 | MPROK | MPROK |
| 18 | MPROK_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 19 | MWSBP | Tax amount | CURR | 13 | 2 | MWSBP | WERTV7 |
| 20 | MWSBP_CALC | Tax amount | CURR | 13 | 2 | MWSBP | WERTV7 |
| 21 | MWSBP_DIFF | Tax amount | CURR | 13 | 2 | MWSBP | WERTV7 |
| 22 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 23 | NETWR_ITEM | Net value | CURR | 15 | 2 | NETWR_AP | WERTV8 |
| 24 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 25 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 26 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 27 | TAX_DIFF | Tax Diff |  | 0 | 0 |  |  |
| 28 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 29 | VAT_CONV | Conventional VAT Rate |  | 0 | 0 |  |  |
| 30 | VAT_RATE | VAT (%) | DEC | 5 | 2 | /SKN/E_VAT | /SKN/D_VAT |
| 31 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 32 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 33 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 34 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 35 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 36 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 37 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 38 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 39 | WAVWR | Cost | CURR | 13 | 2 | WAVWR | WERTV7 |
| 40 | ZERO_VAT_EXPLICITLY | VAT zero explicitly specified |  | 0 | 0 |  |  |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 40 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**AUDAT** (Document Date)

Sales document date (order date) used for period-based SD selection.

**BACKDAYS** (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BSTNK** (Purchase order no.)

Customer or external PO reference number used for cross-system document matching.

**CUST_DESC** (Name)

Customer description/name text used for readable customer-level reporting.

**DATE_REF_FLD** (Date reference field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- AEDAT — Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.
- AUDAT — Sales document date (order date) used for period-based SD selection.
- VDATU — Requested/validity date used for schedule and due-date based filtering.
- GUEBG — Valid-from Date stores the specific calendar date on which a contract, scheduling agreement, quote, or condition record officially becomes active and legally binding.
- ANGDT — Quotation Valid From represents the exact calendar date when the pricing conditions, terms, and delivery commitments in a sales quotation become legally effective for the customer.

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

**ERZET** (Time)

Entry time used to refine timestamp windows within a selected day.

**FORWDAYS** (INT4)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

Forwdays is based on DATE_REF_FLD field.

**Not in use**
**KUNNR** (Sold-to party)

Customer account is used to scope records to specific customers across SD/FI flows.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**MANAGE_IN_UTC** (CHAR)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MPROK** (Manual price)

Material/procurement status key used to identify control-relevant status states.

**MPROK_DESC** (Short text)

Description of material/procurement status for readable reporting.

**MWSBP** (Tax amount)

Tax Amount in Document Currency stores the calculated total tax value for a sales document line item or invoice, expressed in the currency specified for that document.

**MWSBP_CALC** (Tax amount)

Treats tax amount as a discriminator between similar rows that would otherwise look identical in a raw extract.

**MWSBP_DIFF** (Tax amount)

Narrows retrieved rows where tax amount (MWSBP_DIFF) must match the configured selection for this monitor.

**NETWR** (Net value)

Net value amount used for commercial threshold and anomaly checks.

**NETWR_ITEM** (Net value)

Aligns exception volume with the chosen scope by testing net value via NETWR_ITEM before alert evaluation.

**POSNR** (Sales Document Item)

Document item number used for line-level drilldown and joins.

**SPART** (Division)

Division key used for SD product-line segmentation.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TAX_DIFF** (Tax Diff)

After data is read, lines are removed unless tax diff on TAX_DIFF still satisfies the active multivalued selection.

**USER_FLD** (Dynamic Recipient User Field)

The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.
How DRL Works:
When USER_FLD is specified, the system extracts values from that field in the monitoring result set
These extracted values are then used as recipient addresses for alert notifications
This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored
The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users

**Not in use**
**VAT_CONV** (Conventional VAT Rate)

Guards against oversized extracts when conventional vat rate on VAT_CONV is narrowed together with client, user, or session filters.

**Not in use**
**VAT_RATE** (VAT (%))

Stabilizes week-over-week metrics by fixing vat (%) (VAT_RATE) while allowing duration thresholds to move.

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VDATU** (Requested deliv.date)

Requested/validity date used for schedule and due-date based filtering.

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

**ZERO_VAT_EXPLICITLY** (VAT zero explicitly specified)

Ensures reporting respects vat zero explicitly specified constraints carried by ZERO_VAT_EXPLICITLY.

### Parameter Relationships

**Lookback window:** When no explicit date range is supplied on individual date fields, **BACKDAYS** builds a lookback window from the current day before orders are read.

**Reference date mapping:** **DATE_REF_FLD** directs the lookback window to the created-on, changed-on, document, or requested delivery date fields. When **DATE_REF_FLD** is initial and a single document category value is supplied, the reference field defaults to valid-from for contracts or quotation valid-from for quotations.

**Order selection:** **VBELN**, **KUNNR**, **VKORG**, **VTWEG**, **BSTNK**, **AUART**, **VBTYP**, and related header and item date fields narrow which sales order lines are retrieved before tax calculation runs.

**Tax validation:** For each selected item line, expected tax is derived from item net value using the conventional VAT rate configuration. Posted item tax is compared to the calculated amount; **TAX_DIFF** filters which difference values are retained in the result set.

**Zero-tax handling:** When posted item tax is initial, **ZERO_VAT_EXPLICITLY** controls whether the line is still evaluated using the configured conventional rate or treated as not requiring an explicit zero-VAT definition.

**Age filter:** After rows are selected, elapsed time from each row's reference date to the evaluation time is calculated using **DURATION_UNIT** and stored in **DURATION**; rows outside the configured duration range are removed.

**Item attributes:** **MPROK** and **WAVWR** filter item lines by manual pricing status and statistical cost value where those dimensions are relevant to the review scope.

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper and the on-premise path below that call is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 0 by code
- **DATE_REF_FLD** - initial - treated as VDATU by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code

### Practical Example of Parameter Configuration

**Use Case 1: Non-zero tax difference in one sales organization**

**Purpose:** Review order items where posted tax differs from calculated tax in sales organization 1000.

```
TAX_DIFF = NE 0
VKORG = 1000
BACKDAYS = 7
```

**Use Case 2: Zero posted tax with explicit zero-VAT handling**

**Purpose:** Flag lines with no posted tax when zero-VAT must be explicitly defined for evaluation.

```
ZERO_VAT_EXPLICITLY = X
VKORG = 1000
VTWEG = 10
BACKDAYS = 14
```

**Use Case 3: Customer-specific tax review**

**Purpose:** Sample tax differences for one sold-to customer over the last month.

```
TAX_DIFF = NE 0
KUNNR = 100000
BACKDAYS = 30
```

**Use Case 4: Manual pricing status with tax mismatch**

**Purpose:** Review items with manual pricing status and non-zero tax difference.

```
MPROK = A
TAX_DIFF = NE 0
AUART = TA
VKORG = 1000
BACKDAYS = 14
```

**Use Case 5: Exactly seven full days since requested delivery date**

**Purpose:** Return rows whose requested delivery reference date is exactly 7 full days ago for weekly follow-up.

```
DURATION = 7
DURATION_UNIT = F
DATE_REF_FLD = VDATU
BACKDAYS = 30
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_ORD_VAT_CHK | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_ORD_VAT_CHK | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_ORD_VAT_CHK | AUDAT | Document Date (Date Received/Sent) | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_ORD_VAT_CHK | BSTNK | Customer purchase order number | CHAR(20) | BSTNK |
| /SKN/S_SW_10_01_ORD_VAT_CHK | CUST_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ORD_VAT_CHK | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ORD_VAT_CHK | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ORD_VAT_CHK | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_ORD_VAT_CHK | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_ORD_VAT_CHK | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_ORD_VAT_CHK | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_ORD_VAT_CHK | MPROK | Status manual price change | CHAR(1) | MPROK |
| /SKN/S_SW_10_01_ORD_VAT_CHK | MPROK_DESC | Explanatory short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_10_01_ORD_VAT_CHK | MWSBP | Tax amount in document currency | CURR(13,2) | MWSBP |
| /SKN/S_SW_10_01_ORD_VAT_CHK | MWSBP_CALC | Tax amount in document currency | CURR(13,2) | MWSBP |
| /SKN/S_SW_10_01_ORD_VAT_CHK | MWSBP_DIFF | Tax amount in document currency | CURR(13,2) | MWSBP |
| /SKN/S_SW_10_01_ORD_VAT_CHK | NETWR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_ORD_VAT_CHK | NETWR_ITEM | Net value of the order item in document currency | CURR(15,2) | NETWR_AP |
| /SKN/S_SW_10_01_ORD_VAT_CHK | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_ORD_VAT_CHK | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_ORD_VAT_CHK | VAT_RATE | VAT (%) | DEC(5,2) | /SKN/E_VAT |
| /SKN/S_SW_10_01_ORD_VAT_CHK | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_ORD_VAT_CHK | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_ORD_VAT_CHK | VDATU | Requested delivery date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_ORD_VAT_CHK | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_ORD_VAT_CHK | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_ORD_VAT_CHK | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_ORD_VAT_CHK | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_ORD_VAT_CHK | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_ORD_VAT_CHK | WAVWR | Cost in document currency | CURR(13,2) | WAVWR |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ORD_VAT_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_VAT_CHK OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
               LANGU  LANGU,
               BACKDAYS INT4,
               FORWDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               VAT_CONV /SKN/E_VAT,  " Convential Vat to be Defined Mandatory
               ZERO_VAT_EXPLICITLY CHAR1.  " if 'X' - required 0-VAT definition by vat_conv parameter
** Default values
  LV_BACKDAYS = 0.
  LV_DATE_REF_FLD = 'VDATU'.  "'ERDAT'.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU. """5-6-19
  SELECT_SINGLE: MANAGE_IN_UTC,
                 LANGU,
                 BACKDAYS,
                 FORWDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT,
                 VAT_CONV,
                 ZERO_VAT_EXPLICITLY.
  DATA_MULTY: KUNNR        VBAK-KUNNR,
              VBELN        VBAK-VBELN,
              VKORG        VBAK-VKORG,
              VTWEG        VBAK-VTWEG,
              BSTNK        VBAK-BSTNK,
              VBTYP        VBAK-VBTYP,
*            NETWR_USD    VBAK-NETWR,
              AUART        VBAK-AUART,
              ERDAT        VBAK-ERDAT,
              AEDAT        VBAK-AEDAT,
              AUDAT        VBAK-AUDAT,
              VDATU        VBAK-VDATU,
              DATUM        SY-DATUM,
              VAT_RATE     /SKN/E_VAT,  " Not in Use !!!
              TAX_DIFF    MWSBP,
              DURATION    /SKN/E_SW_DURATION,
              MPROK       MPROK,  """"5-6-19
              WAVWR       WAVWR
              .
  SELECT_MULTY: KUNNR,
              VBELN,
              VKORG ,
              VTWEG ,
              BSTNK,
              VBTYP,
*              netwr_usd,
              AUART,
              ERDAT,
              AEDAT,
              AUDAT,
              VDATU,
              DATUM,
              VAT_RATE,
              DURATION,
              TAX_DIFF,
              MPROK,  """ 5-6-19
              WAVWR.
  CONVERT_MULTY: KUNNR ALPHA,
                 VBELN ALPHA.
  CONVERT_MULTY:  AUART AUART . """Tanya 14/11/18
""" 5-6-19
DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
      LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
      LV_DDTEXT LIKE  DD07V-DDTEXT.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_ORD_VAT_CHK'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
  RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
           R_FLD_VAL FOR DD03P-FIELDNAME .
  DATA :   FLD_NAME TYPE FIELDNAME.
  DATA : I TYPE I,
         CI(1) TYPE C,
         NFIELDS TYPE I VALUE 3.   "
  DATA : BACKDAYS  TYPE I ,
         FORWDAYS TYPE I,
         DATE_FROM LIKE SY-DATUM,
         DATE_TO LIKE SY-DATUM .
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
  DATA: LV_VBTYP TYPE VBTYP.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA : LS_VBPA TYPE VBPA,
         LT_VBPA LIKE TABLE OF LS_VBPA.
  DATA : LV_DATA_POSNR TYPE POSNR.
  IF NOT LV_FORWDAYS  IS INITIAL.
    LV_BACKDAYS = LV_FORWDAYS * ( -1 ).
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
  "--- Check Quatetion or Contracts types
  IF LV_DATE_REF_FLD IS INITIAL.
    READ TABLE R_VBTYP INTO RS_VBTYP INDEX 1.
    IF SY-TFILL = 1. " the single record only
      IF RS_VBTYP-OPTION = 'EQ'.
        LV_VBTYP = RS_VBTYP-LOW.
      ENDIF.
    ENDIF.
    IF LV_VBTYP = 'G'.
      LV_DATE_REF_FLD = 'GUEBG'.
    ELSEIF LV_VBTYP = 'B'.
      LV_DATE_REF_FLD = 'ANGDT'.
    ENDIF.
    "    endif.
  ENDIF.
  "---
  CASE LV_DATE_REF_FLD.
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "Changed On
    WHEN 'AUDAT'.
      R_AUDAT[] = R_DATUM[]. "Document Date (Date Received/Sent)
    WHEN 'VDATU'.
      R_VDATU[] = R_DATUM[].
    WHEN OTHERS.
      R_ERDAT[] = R_DATUM[]. "Document created
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT
    VBAK~VBELN VBAP~POSNR VBAK~ERDAT VBAK~ERZET VBAK~ERNAM VBAK~AUDAT VBAK~VBTYP VBAK~AUART VBAK~AEDAT VBAK~NETWR
    VBAK~WAERK VBAK~KUNNR VBAK~VKORG VBAK~VTWEG VBAK~SPART VBAK~VKGRP VBAK~VKBUR VBAK~BSTNK VBAP~MWSBP VBAK~AEDAT VBAK~VDATU
    VBAP~MPROK VBAP~WAVWR VBAP~NETWR AS NETWR_ITEM
    FROM VBAK
    INNER JOIN VBAP ON
    VBAK~VBELN = VBAP~VBELN
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE VBAK~VBELN IN R_VBELN
      AND KUNNR IN R_KUNNR
      AND VKORG IN R_VKORG
      AND VTWEG IN R_VTWEG
      AND BSTNK IN R_BSTNK
      AND AUART IN R_AUART
      AND VBAK~ERDAT IN R_ERDAT
      AND VBTYP IN R_VBTYP
      AND VBAK~AUDAT IN R_AUDAT
      AND VBAK~AEDAT IN R_AEDAT
      AND VBAK~VDATU IN R_VDATU
      AND MPROK IN R_MPROK  """ 5-6-19
      AND WAVWR IN R_WAVWR.
**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    IF  IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.
    REF_DATE =  .
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
**********************************************************************
  DATA: LT_DATA LIKE T_DATA[],
        LWA_DATA LIKE LINE OF T_DATA,
        LV_NETWR LIKE VBAP-NETWR,
        LV_VAT TYPE MWSBP,
        LV_VAT_RATE_CONV TYPE /SKN/E_VAT.
  LT_DATA[] = T_DATA[].
  REFRESH T_DATA[].
  "--- Calculate Vat Values
  LOOP AT LT_DATA INTO LWA_DATA.
    CLEAR LV_VAT.
    LV_VAT_RATE_CONV = LV_VAT_CONV.
    LV_VAT = ( LWA_DATA-NETWR_ITEM * LV_VAT_CONV ) / 100.
    IF LWA_DATA-MWSBP IS INITIAL.
      IF LV_ZERO_VAT_EXPLICITLY IS INITIAL.
        CLEAR LV_VAT.   " Based on Existing content
        CLEAR LV_VAT_RATE_CONV.
      ENDIF.
    ENDIF.
    LWA_DATA-MWSBP_CALC = LV_VAT.
    LWA_DATA-MWSBP_DIFF = LWA_DATA-MWSBP - LWA_DATA-MWSBP_CALC.
    IF LWA_DATA-MWSBP_DIFF IN R_TAX_DIFF.
      LWA_DATA-VAT_RATE = LV_VAT_RATE_CONV.
      APPEND LWA_DATA TO T_DATA.
    ENDIF.
  ENDLOOP.
***  LOOP AT lt_data INTO lwa_data.
***
*****  Calculate vat rate
***    CLEAR lv_netwr.
***    lv_netwr = ( lwa_data-mwsbp / lwa_data-netwr_item ) * 100.
***    lwa_data-vat_rate = lv_netwr.
***
***    IF lwa_data-vat_rate IN r_vat_rate.
*****   Stay with the records that the VAT rate isn't in range
***      APPEND lwa_data TO t_data.
***    ENDIF.
***
***  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR          = T_DATA-KUNNR
      IMPORTING
        CUST_DESC      = T_DATA-CUST_DESC
      EXCEPTIONS
        WRONG_CUSTOMER = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
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
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
