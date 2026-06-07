# Exception Indicator: Material master data change Exception indicator ( SW_10_02_MAT_MD_CH)

## General Overview

This Exception Indicator identifies materials whose master or valuation attributes fall within configured selection criteria and that have change-document updates on a specified table field within a lookback window, then presents old and new values with descriptive texts for materials management review.

This EI serves as an essential control for inventory valuation and master data governance by:
- Surfacing materials with documented updates to a chosen master-data field so reviewers can see what changed and when
- Enabling detection of valuation, price, or status adjustments on filtered material populations and plants
- Supporting materials accountants with material, plant, and material-type descriptions alongside change user and date context
- Helping audit teams demonstrate that sensitive master-data fields were monitored on a repeatable schedule
- Complementing manual change-document browsing with a consolidated exception list tied to current material snapshots

Typical use includes reviews after price updates, valuation class changes, or master-data corrections in selected plants or material groups. Results are intended for exception workflows rather than full change-document archives.

The routine reads current material and valuation data for the filtered population, resolves related change headers and items for the configured table and field, enriches rows with descriptions, and raises an alert when qualifying change lines remain.


## Problem Description

Failure to monitor material master and valuation changes on critical fields creates multiple risks across inventory accounting, procurement, and compliance.

**Financial and Valuation Risks**
- Standard price or moving-average updates may post to the general ledger without timely review of the underlying master-data change
- Valuation class or price control adjustments can affect COGS and inventory balances when not detected early
- Planned price or costing-relevant fields may change outside approved windows

**Master Data and Operations Risks**
- Plant-level material status or procurement indicators may be altered without operations awareness
- Concentrations of changes by user or material group are harder to see without a filtered exception population

**Compliance and Audit Risks**
- Evidence of review over sensitive material fields is weaker when auditors must search change documents manually
- Cross-plant comparisons are delayed when material and plant context is not assembled with each change line

## Suggested Resolution

**Immediate Response**
- Review each flagged material together with plant, old and new values, change user, and change date shown in the exception
- Confirm with materials management or controlling whether the update was approved and posted as intended
- Escalate large valuation movements on high-value materials before the next costing or closing run

**System Assessment**
- Compare this cycle to prior runs after price roll-ups, migration projects, or bulk uploads
- Look for concentrations by user, material type, valuation class, or plant to see whether one job or team drives most changes
- Revisit the chosen table and field name when the queue contains noise from unrelated change types

**Corrective Actions**
- Reverse or correct erroneous master-data changes through standard material maintenance with required approvals
- Adjust monitoring scope after root cause so the queue stays actionable for reviewers
- Update written procedures when specific fields require mandatory secondary review
- Route repeat bulk-change issues into defect or change management when interfaces or programs require fixes


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | back days |  | 0 | 0 |  |  |
| 2 | BESKZ | Procurement type | CHAR | 1 | 0 | BESKZ | BESKZ |
| 3 | BKLAS | Valuation Class | CHAR | 4 | 0 | BKLAS | BKLAS |
| 4 | BWKEY | Valuation Area | CHAR | 4 | 0 | BWKEY | BWKEY |
| 5 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 6 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 7 | EKALR | With Qty Structure | CHAR | 1 | 0 | CK_EKALREL | XFELD |
| 8 | FNAME | field name |  | 0 | 0 |  |  |
| 9 | LAEDA | Last Change | DATS | 8 | 0 | LAEDA | DATUM |
| 10 | LBKUM | Total Stock | QUAN | 13 | 3 | LBKUM | MENG13V |
| 11 | LOSGR | Planned lot size | QUAN | 13 | 3 | LOSGR | MENG13 |
| 12 | MAT_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 13 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 14 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 15 | MMSTA | Plant-sp.matl status | CHAR | 2 | 0 | MMSTA | MMSTA |
| 16 | MTART | Material Type | CHAR | 4 | 0 | MTART | MTART |
| 17 | MTBEZ | Material type descr. | CHAR | 25 | 0 | MTBEZ | TEXT25 |
| 18 | NCOST | Do Not Cost | CHAR | 1 | 0 | CK_NO_COSTING | CK_NO_COSTING |
| 19 | PEINH | Price Unit | DEC | 5 | 0 | PEINH | PACK3 |
| 20 | PLANT_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 21 | SALK3 | Total Value | CURR | 13 | 2 | SALK3 | WERT13N |
| 22 | SOBSK | SpecProcurem Costing | CHAR | 2 | 0 | CK_SOBSL | SOBSL |
| 23 | SOBSL | Special procurement | CHAR | 2 | 0 | SOBSL | SOBSL |
| 24 | STPRS | Standard price | CURR | 11 | 2 | STPRS | WERT11V |
| 25 | TABNAME | md relevant table name |  | 0 | 0 |  |  |
| 26 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 27 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 28 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 29 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 30 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 31 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 32 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 33 | VERPR | Moving price | CURR | 11 | 2 | VERPR | WERT11V |
| 34 | VPRSV | Price control | CHAR | 1 | 0 | VPRSV | VPRSV |
| 35 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 36 | WGBEZ | Description | CHAR | 20 | 0 | TEXT20 | TEXT20 |
| 37 | ZPLD1 | Planned price date 1 | DATS | 8 | 0 | DZPLD1 | DATUM |
| 38 | ZPLD2 | Planned price date 1 | DATS | 8 | 0 | DZPLD1 | DATUM |
| 39 | ZPLD3 | Planned price date 1 | DATS | 8 | 0 | DZPLD1 | DATUM |
| 40 | ZPLP1 | Planned price 1 | CURR | 11 | 2 | DZPLP1 | WERT11 |
| 41 | ZPLP2 | Planned price 2 | CURR | 11 | 2 | DZPLP2 | WERT11 |
| 42 | ZPLP3 | Planned price 3 | CURR | 11 | 2 | DZPLP3 | WERT11 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 42 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (back days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on LAEDA

**BESKZ** (Procurement type)

Guards against oversized extracts when procurement type on BESKZ is narrowed together with client, user, or session filters.

**BKLAS** (Valuation Class)

Valuation class on the material controlling how inventory is valued and which accounts receive postings.

**BWKEY** (Valuation Area)

Valuation area key joining material valuation to plant/company rules for moving-average or standard price.

**CUKY_NEW** (CUKY)

New currency key in change-log comparisons to detect currency master changes.

**CUKY_OLD** (CUKY)

Previous currency key in change-log comparisons for before/after analysis.

**EKALR** (With Qty Structure)

Ensures reporting respects with qty structure constraints carried by EKALR.

**FNAME** (field name)

Field name key in change documents used to filter by changed attribute.

**LAEDA** (Last Change)

Last changed date on archive-relevant or info-structure rows tracking when the aggregate row was refreshed.

**LBKUM** (Total Stock)

Valuated stock quantity in movement or enhanced inventory extracts-quantity leg of valuated stock snapshots.

**LOSGR** (Planned lot size)

Prevents accidental global scans when planned lot size (LOSGR) is meant to stay within a controlled application slice.

**MAT_DESC** (Material Description)

Material description text used to provide readable product context.

**MATKL** (Material Group)

Material group key used for product-category segmentation in MM/SD analytics.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MMSTA** (Plant-sp.matl status)

Works downstream of the initial read so plant-sp.matl status on MMSTA still participates in row-level deletion rules.

**MTART** (Material Type)

Material type classifying procurement, production, and valuation behavior of material master records.

**MTBEZ** (Material type descr.)

When left open per framework rules, MTBEZ does not restrict material type descr.; when set, only matching rows remain.

**NCOST** (Do Not Cost)

When left open per framework rules, NCOST does not restrict do not cost; when set, only matching rows remain.

**PEINH** (Price Unit)

Price unit denominator used to interpret per-unit purchasing prices.

**PLANT_DESC** (Name 1)

Plant name or description text paired with WERKS; readable master-data label, not the plant key field.

**SALK3** (Total Value)

Total valuated inventory value in company-code currency from material valuation aggregates.

**SOBSK** (SpecProcurem Costing)

Treats specprocurem costing as a discriminator between similar rows that would otherwise look identical in a raw extract.

**SOBSL** (Special procurement)

After data is read, lines are removed unless special procurement on SOBSL still satisfies the active multivalued selection.

**STPRS** (Standard price)

Standard price on material valuation controlling inventory valuation at standard-cost organizations.

**TABNAME** (md relevant table name)

Database table name used to scope change/object monitoring to specific tables.

**TEXT_CASE** (Text flag)

Text case/normalization selector used for case-sensitive text filtering behavior.

**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**UNIT_NEW** (Unit)

Unit of measure after change on quantity fields-pairs with NEW_VAL in old/new quantity comparisons on change items.

**UNIT_OLD** (Unit)

Unit of measure before change on quantity fields-pairs with OLD_VAL for before/after quantity analysis.

**USERNAME** (User)

User name display field used for readable identity reporting.

**VALUE_NEW** (New value)

New value in change documents used for after-change analysis.

**VALUE_OLD** (Old value)

Old value in change documents used for before/after comparison.

**VERPR** (Moving price)

Moving-average price on material valuation rows for inventory value-at-current-price analytics.

**VPRSV** (Price control)

Price control indicator S or V choosing standard price versus moving-average inventory valuation.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**WGBEZ** (Description)

Material group description used for readable category reporting.

**ZPLD1 - ZPLD3** (Planned price date 1)

Interprets planned price date 1 as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on ZPLD1.

**ZPLP1 - ZPLP3** (Planned price 1)

Guards against oversized extracts when planned price 1 on ZPLP1 is narrowed together with client, user, or session filters.


### Parameter Relationships

How parameter combinations work together

**Change-document scope:** **TABNAME** and **FNAME** define which change-document table and field are read for update (`U`) lines; only changes on that field drive alert rows after headers are matched.

**Monitoring window:** **BACKDAYS** sets how far back from the evaluation date the change-date selection reaches when no explicit date range is supplied; material last-change date filtering on the initial stock read uses the same date range logic.

**Material population:** **MATNR**, **MTART**, **MATKL**, **WERKS**, valuation and quantity fields (**VPRSV**, **VERPR**, **STPRS**, **LBKUM**, **SALK3**, **BKLAS**, **PEINH**, and related selectors), and plant or procurement indicators (**MMSTA**, **BESKZ**, **SOBSL**, **SOBSK**, **LOSGR**, **NCOST**, **EKALR**, planned price fields) narrow which materials are loaded before change documents are evaluated.

**Language for texts:** **LANGU** controls the language used for material, plant, and material-group descriptions on output rows.

**Final selection:** Material and valuation filters, the backward day window, the configured change field, and matching change-document headers apply together—only materials with qualifying updates on that field appear in the final alert population.


### Default Values

- **BACKDAYS** - initial - treated as 100 by code

### Practical Example of Parameter Configuration

**Use Case 1: Standard price changes in one plant**

**Purpose:** Detect updates to the standard price field on finished materials in a single plant over the last thirty days.
```
TABNAME = MBEW
FNAME = STPRS
BACKDAYS = 30
WERKS = 1000
MTART = FERT
```

**Use Case 2: Valuation class adjustments**

**Purpose:** Review moving-average materials where valuation class was changed recently across a company code population.
```
TABNAME = MBEW
FNAME = BKLAS
BACKDAYS = 14
VPRSV = V
BKLAS = 3000 - 3099
MATKL = *
```

**Use Case 3: Plant material status review**

**Purpose:** Highlight plant-level material status changes for procured items in selected storage locations.
```
TABNAME = MARC
FNAME = MMSTA
BACKDAYS = 7
BESKZ = F
MMSTA = 01 - 99
WERKS = 1000 - 1999
```

**Use Case 4: Planned price update monitoring**

**Purpose:** Track planned price field changes on configured material types with English descriptions.
```
TABNAME = MBEW
FNAME = ZPLP1
BACKDAYS = 60
MTART = ROH
LANGU = E
MATNR = 10000000 - 19999999
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_02_MAT_MD_CHANGE | BACKDAYS | back days | CHAR(0) | BACKDAYS |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | BESKZ | Procurement type | CHAR(1) | BESKZ |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | BKLAS | Valuation Class | CHAR(4) | BKLAS |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | BWKEY | Valuation Area | CHAR(4) | BWKEY |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | CUKY_NEW | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | CUKY_OLD | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | EKALR | With Qty Structure | CHAR(1) | CK_EKALREL |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | FNAME | field name | CHAR(0) | FNAME |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | LAEDA | Last Change | DATS(8) | LAEDA |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | LBKUM | Total Stock | QUAN(13) | LBKUM |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | LOSGR | Planned lot size | QUAN(13) | LOSGR |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | MAT_DESC | Material Description | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | MMSTA | Plant-sp.matl status | CHAR(2) | MMSTA |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | MTART | Material Type | CHAR(4) | MTART |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | MTBEZ | Material type descr. | CHAR(25) | MTBEZ |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | NCOST | Do Not Cost | CHAR(1) | CK_NO_COSTING |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | PEINH | Price Unit | DEC(5) | PEINH |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | PLANT_DESC | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | SALK3 | Total Value | CURR(13) | SALK3 |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | SOBSK | SpecProcurem Costing | CHAR(2) | CK_SOBSL |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | SOBSL | Special procurement | CHAR(2) | SOBSL |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | STPRS | Standard price | CURR(11) | STPRS |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | TABNAME | md relevant table name | CHAR(0) | TABNAME |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | TEXT_CASE | Text flag | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | UDATE | Date | DATS(8) | CDDATUM |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | UNIT_NEW | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | UNIT_OLD | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | USERNAME | User | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | VALUE_NEW | New value | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | VALUE_OLD | Old value | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | VERPR | Moving price | CURR(11) | VERPR |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | VPRSV | Price control | CHAR(1) | VPRSV |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | WERKS | Plant | CHAR(4) | WERKS_D |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | WGBEZ | Description | CHAR(20) | TEXT20 |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | ZPLD1 | Planned price date 1 | DATS(8) | DZPLD1 |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | ZPLD2 | Planned price date 1 | DATS(8) | DZPLD1 |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | ZPLD3 | Planned price date 1 | DATS(8) | DZPLD1 |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | ZPLP1 | Planned price 1 | CURR(11) | DZPLP1 |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | ZPLP2 | Planned price 2 | CURR(11) | DZPLP2 |
| /SKN/S_SW_10_02_MAT_MD_CHANGE | ZPLP3 | Planned price 3 | CURR(11) | DZPLP3 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_MAT_MD_CHANGE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_MAT_MD_CHANGE
*"----------------------------------------------------------------------
  DATA : SPRAS_T TYPE SPRAS .
  DATA : LT_POS TYPE TABLE OF CDPOS,
         LS_POS LIKE LINE OF LT_POS,
         LT_HDR TYPE TABLE OF CDHDR,
         LS_HDR LIKE LINE OF LT_HDR,
         SY_TABIX LIKE SY-TABIX,
         DATE_FROM LIKE SY-DATUM .
  DATA_MULTY:  OBJECTID          CDOBJECTV,
               CHANGENR          CDCHANGENR.
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_MULTY: MATNR  MATNR,
              MTART MTART,
              MATKL  MATKL,
              LBKUM LBKUM,
              SALK3 SALK3,
              VPRSV VPRSV,
              VERPR VERPR,
              STPRS STPRS,
              PEINH PEINH,
              BKLAS BKLAS ,
              EKALR CK_EKALREL,
              ZPLP1 DZPLP1,
              ZPLD1 DZPLD1,
              ZPLP2 DZPLP2,
              ZPLD2	DZPLD2,
              ZPLP3 DZPLP3,
              ZPLD3	DZPLD3,
              WERKS WERKS_D,
              SOBSK CK_SOBSL,
              LOSGR CK_LOSGR,
              NCOST CK_NO_COSTING,
              MMSTA MMSTA,
              BESKZ BESKZ,
              SOBSL	SOBSL ,
              DATUM SY-DATUM.
  DATA_SINGLE: TABNAME TABNAME,
               FNAME   FIELDNAME,
               LANGU   LANGU,
               BACKDAYS I.
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA : TMP_DATA TYPE TABLE OF  /SKN/S_SW_10_02_MAT_MD_CHANGE,
         LS_DATA LIKE LINE OF T_DATA.
  FIELD-SYMBOLS: <ALERT_FIELDS>  TYPE /SKN/S_SW_10_02_MAT_MD_CHANGE.
  "-----------------------------------------------
  " 2. Extracting & Populating Parameters        "
  "-----------------------------------------------
  SELECT_SINGLE: TABNAME,
                 FNAME,
                 LANGU,
                 BACKDAYS.
  IF LV_BACKDAYS IS INITIAL.
    LV_BACKDAYS = 100.
  ENDIF.
  IF R_DATUM[] IS INITIAL .  " Set default value
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF .
  SELECT_MULTY: MATNR ,"
                MTART,
                MATKL  ,"
                LBKUM ,
                SALK3 ,
                VPRSV ,"
                VERPR ,
                STPRS ,
                PEINH ,"
                BKLAS ,"
                EKALR ,"
                ZPLP1 ,"
                ZPLD1 ,"
                ZPLP2 ,"
                ZPLD2	,"
                ZPLP3 ,"
                ZPLD3	,"
                WERKS ,"
                SOBSK ,"
                LOSGR ,"
                NCOST ,
                MMSTA ,
                BESKZ ,
                SOBSL	."
  CONVERT_MULTY: MATNR MATN1.
  CONVERT_SINGLE: LANGU ISOLA.
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "  refresh tmp_data.
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert Data           "
  "-----------------------------------------------
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_MAT_MD_CHANGE'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  REFRESH TMP_DATA.
  SELECT *
      FROM MARC AS A
      INNER JOIN T001W AS B ON A~WERKS = B~WERKS
      INNER JOIN MBEW AS C ON A~MATNR = C~MATNR
             AND B~BWKEY = C~BWKEY
      INNER JOIN MARA AS D ON D~MATNR = A~MATNR
      INTO CORRESPONDING FIELDS OF TABLE TMP_DATA
      WHERE D~MATKL IN R_MATKL AND
            D~MTART IN R_MTART AND
  "          a~werks IN r_werks AND
            A~SOBSK IN R_SOBSK AND
            A~LOSGR IN R_LOSGR AND
            A~NCOST IN R_NCOST AND
            A~MMSTA IN R_MMSTA AND
            A~BESKZ IN R_BESKZ AND
            A~SOBSL IN R_SOBSL AND
            D~MATNR IN R_MATNR AND
            C~MATNR IN R_MATNR AND
            A~MATNR IN R_MATNR AND
            C~LBKUM IN R_LBKUM AND
            C~SALK3 IN R_SALK3 AND
            C~STPRS IN R_STPRS AND
            C~VERPR IN R_VERPR AND
            C~VPRSV IN R_VPRSV AND
            C~PEINH IN R_PEINH AND
            C~BKLAS IN R_BKLAS AND
            C~EKALR IN R_EKALR AND
            C~ZPLP1 IN R_ZPLP1 AND
            C~ZPLD1 IN R_ZPLD1 AND
            C~ZPLP2 IN R_ZPLP2 AND
            C~ZPLD2 IN R_ZPLD2 AND
            C~ZPLP3 IN R_ZPLP3 AND
            C~ZPLD3 IN R_ZPLD3 AND
            D~LAEDA IN R_DATUM
    ORDER BY A~MATNR.
  LOOP AT TMP_DATA INTO LS_DATA.
    RS_OBJECTID-LOW = LS_DATA-MATNR.
    RS_OBJECTID-OPTION = 'EQ'.
    RS_OBJECTID-SIGN = 'I'.
    APPEND RS_OBJECTID TO R_OBJECTID.
  ENDLOOP.
  SELECT *
     FROM CDHDR
     INTO CORRESPONDING FIELDS OF TABLE LT_HDR
     WHERE OBJECTCLAS EQ 'MATERIAL'
       AND OBJECTID IN R_OBJECTID
       AND CHANGE_IND EQ 'U'
       AND UDATE IN R_DATUM.
  LOOP AT LT_HDR INTO LS_HDR.
    RS_CHANGENR-LOW = LS_HDR-CHANGENR.
    RS_CHANGENR-OPTION = 'EQ'.
    RS_CHANGENR-SIGN = 'I'.
    APPEND RS_CHANGENR TO R_CHANGENR.
  ENDLOOP.
  SELECT *
     FROM CDPOS
     INTO CORRESPONDING FIELDS OF TABLE LT_POS
   "  for all entries in lt_hdr
     WHERE OBJECTCLAS EQ 'MATERIAL'
       AND OBJECTID IN R_OBJECTID
       AND CHNGIND EQ 'U'
       AND TABNAME EQ LV_TABNAME
       AND FNAME   EQ LV_FNAME . "
  SORT LT_POS BY OBJECTID.
  DELETE LT_POS WHERE  CHANGENR NOT IN R_CHANGENR.
  "check langu
  SELECT SINGLE SPRAS INTO SPRAS_T
    FROM T002
    WHERE SPRAS = LV_LANGU.
  IF SY-SUBRC <> 0.
    LV_LANGU = 'E'.
  ENDIF.
  "-----------------------------------------------
  " 5. Post retrieving manipulations             "
  "-----------------------------------------------
  LOOP AT LT_POS INTO LS_POS.
    DATA: IND LIKE SY-TABIX,
          TMP_MATNR LIKE T_DATA-MATNR.
    TMP_MATNR = LS_POS-OBJECTID.
    READ TABLE TMP_DATA BINARY SEARCH WITH KEY MATNR = TMP_MATNR
     INTO LS_DATA.
    IF SY-SUBRC = 0.
      IND = SY-TABIX.
      LOOP AT TMP_DATA  ASSIGNING  <ALERT_FIELDS> FROM IND.
        IF LS_DATA-MATNR = <ALERT_FIELDS>-MATNR.
          <ALERT_FIELDS>-VALUE_OLD = LS_POS-VALUE_OLD.
          <ALERT_FIELDS>-VALUE_NEW = LS_POS-VALUE_NEW.
          APPEND <ALERT_FIELDS> TO T_DATA.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
ENDLOOP.
"-----------------------------------------------
" 6. Post retrieving filtering                 "
"-----------------------------------------------
*  DELETE T_DATA WHERE STATE_COLOR NOT IN R_STATE_COLOR.
SORT T_DATA BY LAEDA MATNR.
LOOP AT T_DATA.
**Material desc
  SY_TABIX = SY-TABIX .
  CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
    EXPORTING
      MATNR         = T_DATA-MATNR
      LANGU         = LV_LANGU
    IMPORTING
      MATERIAL_DESC = T_DATA-MAT_DESC
    EXCEPTIONS
      WRONG_CODE    = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  " plant description
  CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
    EXPORTING
      WERKS      = T_DATA-WERKS
      LANGU      = LV_LANGU
    IMPORTING
      PLANT_DESC = T_DATA-PLANT_DESC
    EXCEPTIONS
      WRONG_CODE = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
    EXPORTING
      MATKL      = T_DATA-MATKL
      LANGU      = LV_LANGU
    IMPORTING
      MATKL_DESC = T_DATA-WGBEZ
    EXCEPTIONS
      WRONG_CODE = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  CALL FUNCTION '/SKN/F_SW_10_MAT_TYP_DESC'
    EXPORTING
      MTART      = T_DATA-MTART
      LANGU      = LV_LANGU
    IMPORTING
      MTBEZ      = T_DATA-MTBEZ
    EXCEPTIONS
      WRONG_CODE = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  MODIFY T_DATA INDEX SY_TABIX.
ENDLOOP.
"-----------------------------------------------
" 7. Finishing (Set IS_ALERT parameter)        "
"-----------------------------------------------
READ TABLE T_DATA INTO LS_DATA INDEX 1.
CHECK SY-TFILL IS NOT INITIAL .
IS_ALERT = 'X' .
ENDFUNCTION.
```
