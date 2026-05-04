# Exception Indicator: Material costing (Master data) - SW_10_06_MAT_COSTING

## General Overview

This Exception Indicator (EI) monitors material valuation data to identify materials where the standard price and the moving average price differ substantially (e.g. by a configurable ratio such as 2×). It compares standard price (STPRS) and moving average price (VERPR) from material valuation, computes a ratio, and flags materials whose ratio falls outside the configured range for management review.

This EI serves as an essential control for material costing and master data by:
- Enabling detection of materials with large gaps between standard and moving average prices that may indicate costing errors or obsolete standard prices
- Supporting identification of valuation anomalies for month-end close and inventory valuation review
- Providing visibility into price control (standard vs moving average) and valuation class by plant and material group
- Enabling analysis of planned prices and total stock value for costing and audit
- Supporting accountability for material master and valuation data quality

This monitoring helps organizations detect costing exceptions, update standard prices where appropriate, and maintain reliable inventory valuation. The EI is particularly valuable for cost accounting, material master maintenance, and audit of valuation data.

The EI uses material valuation (MBEW), plant data (T001W), plant-level material (MARC), and material master (MARA) to compute the standard-to-moving-average ratio and return materials that meet the configured criteria.


## Problem Description

Failure to monitor materials where standard and moving average prices differ substantially creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected large gaps between standard and moving average prices can distort inventory valuation and cost of goods sold
- Obsolete or incorrect standard prices may lead to misstated period-end closing and financial statements
- Unmonitored valuation anomalies can delay month-end close when discovered late during review
- Incorrect material valuation affects product costing and profitability analysis

**Operational and Control Risks**
- Materials with extreme standard-to-moving-average ratios may indicate master data errors, missing price updates, or procurement price changes not reflected in standards
- Lack of visibility into price control (standard vs moving average) by plant hinders costing policy enforcement
- Unmonitored valuation class and procurement type patterns can mask data quality issues
- Absence of monitoring limits the ability to prioritize materials for standard price revision

**Management Visibility and Decision-Making Risks**
- Management may be unaware of costing exceptions until audit or period-end issues arise
- Unidentified price gaps delay corrective action and standard price maintenance
- Lack of consolidated view by plant, material group, or material type limits resource allocation for master data cleanup
- Insufficient monitoring undermines accountability for material costing and valuation data quality

## Suggested Resolution

**Immediate Response**
- Review the materials flagged by the EI to confirm whether the standard-to-moving-average ratio is justified (e.g. recent procurement price changes, one-time adjustments) or indicates an error
- Verify high-value or high-volume materials with extreme ratios for correctness of standard price and moving average
- Check price control (standard vs moving average) and valuation class for flagged materials to ensure alignment with policy
- Identify business context: planned price update pending, data entry error, or systemic costing issue

**System Assessment**
- Analyze the time window (e.g. last change date) and organizational scope (plants, material groups) of the results
- Compare current ratios to prior periods to identify worsening or recurring materials
- Examine distribution by plant, material type, and valuation class to find patterns
- Assess whether standard prices or moving averages require bulk update or process change
- Validate the lookback period and date basis used for the monitoring window

**Corrective Actions**
- Update standard prices (e.g. CK24, MR21) where the moving average reflects current reality and policy allows
- For materials that should remain on moving average, ensure price control and valuation settings are correct
- Correct erroneous material master or valuation data and document exceptions for audit
- Schedule recurring EI runs and route results to cost accounting and master data owners
- Use EI output to prioritize materials for standard price revision and to support costing and audit reviews


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | days back |  | 0 | 0 |  |  |
| 2 | BESKZ | Procurement type | CHAR | 1 | 0 | BESKZ | BESKZ |
| 3 | BKLAS | Valuation Class | CHAR | 4 | 0 | BKLAS | BKLAS |
| 4 | BWKEY | Valuation Area | CHAR | 4 | 0 | BWKEY | BWKEY |
| 5 | EKALR | With Qty Structure | CHAR | 1 | 0 | CK_EKALREL | XFELD |
| 6 | LAEDA | Last Change | DATS | 8 | 0 | LAEDA | DATUM |
| 7 | LANGU | Language |  | 0 | 0 |  |  |
| 8 | LBKUM | Total Stock | QUAN | 13 | 3 | LBKUM | MENG13V |
| 9 | LOSGR | Planned lot size | QUAN | 13 | 3 | LOSGR | MENG13 |
| 10 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 11 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 12 | MAT_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 13 | MMSTA | Plant-sp.matl status | CHAR | 2 | 0 | MMSTA | MMSTA |
| 14 | MTART | Material Type | CHAR | 4 | 0 | MTART | MTART |
| 15 | MTBEZ | Material type descr. | CHAR | 25 | 0 | MTBEZ | TEXT25 |
| 16 | NCOST | Do Not Cost | CHAR | 1 | 0 | CK_NO_COSTING | CK_NO_COSTING |
| 17 | PEINH | Price Unit | DEC | 5 | 0 | PEINH | PACK3 |
| 18 | PLANT_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 19 | SALK3 | Total Value | CURR | 13 | 2 | SALK3 | WERT13N |
| 20 | SMRATIO | Standard vs Moving av. ratio | FLTP | 16 | 16 | FLOAT | FLTP |
| 21 | SOBSK | SpecProcurem Costing | CHAR | 2 | 0 | CK_SOBSL | SOBSL |
| 22 | SOBSL | Special procurement | CHAR | 2 | 0 | SOBSL | SOBSL |
| 23 | STPRS | Standard Price | CURR | 11 | 2 | STPRS | WERT11V |
| 24 | VERPR | Moving Average Price | CURR | 11 | 2 | VERPR | WERT11V |
| 25 | VPRSV | Price control | CHAR | 1 | 0 | VPRSV | VPRSV |
| 26 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 27 | WGBEZ | Description | CHAR | 20 | 0 | TEXT20 | TEXT20 |
| 28 | ZPLD1 | Planned price date 1 | DATS | 8 | 0 | DZPLD1 | DATUM |
| 29 | ZPLD2 | Planned price date 2 | DATS | 8 | 0 | DZPLD1 | DATUM |
| 30 | ZPLD3 | Planned price date 3 | DATS | 8 | 0 | DZPLD1 | DATUM |
| 31 | ZPLP1 | Planned price 1 | CURR | 11 | 2 | DZPLP1 | WERT11 |
| 32 | ZPLP2 | Planned price 2 | CURR | 11 | 2 | DZPLP2 | WERT11 |
| 33 | ZPLP3 | Planned price 3 | CURR | 11 | 2 | DZPLP3 | WERT11 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 33 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (days back):

Number of days to look back from today for the material last-change date. When no date range is supplied, the EI uses today minus this value as the start of the window for the material master last-change date (LAEDA). Default in code: 100.

**BESKZ** (Procurement type):

Procurement type of the material at plant level (e.g. in-house production, external procurement). The EI uses it when selecting plant-level material data.

**BESKZ Options:**
- **E**: External procurement
- **F**: In-house production
- (other domain values as in BESKZ)

**BKLAS** (Valuation Class):

Valuation class of the material for valuation area. The EI uses it when selecting material valuation.

**BWKEY** (Valuation Area):

Valuation area (linking plant and valuation view). The EI derives it from plant and uses it when reading material valuation.

**EKALR** (With Qty Structure):

Indicator whether the material is costed with quantity structure. The EI uses it when selecting material valuation and populates it in the result.

**EKALR Options:**
- **X**: With quantity structure
- ** **: Without quantity structure

**LAEDA** (Last Change):

Last change date of the material master. The EI uses it as the date reference for the lookback window; materials whose last-change date falls within the window are included.

**LANGU** (Language):

Language for descriptions (material, plant, material group, material type). The EI uses it when resolving texts; default in code: E when system language is not found.

**LBKUM** (Total Stock):

Total stock quantity in the valuation area. The EI uses it when selecting material valuation.

**LOSGR** (Planned lot size):

Planned lot size for the material at plant. The EI uses it when selecting plant-level material data.

**MATKL** (Material Group):

Material group of the material. The EI uses it when selecting material master.

**MATNR** (Material):

Material number. The EI uses it when selecting material master, plant-level material, and material valuation; it identifies each result row.

**MAT_DESC** (Material Description):

Material description. Populated by the EI from material master when building the result row.

**MMSTA** (Plant-sp.matl status):

Plant-specific material status. The EI uses it when selecting plant-level material.

**MTART** (Material Type):

Material type. The EI uses it when selecting material master and populates it in the result.

**MTBEZ** (Material type descr.):

Material type description. Populated by the EI from master data when building the result row.

**NCOST** (Do Not Cost):

Indicator that the material is not costed. The EI uses it when selecting plant-level material.

**NCOST Options:**
- **X** or **1**: Do not cost
- ** ** or **0**: Cost

**PEINH** (Price Unit):

Price unit for the material valuation. The EI uses it when selecting material valuation.

**PLANT_DESC** (Name 1):

Plant name. Populated by the EI from plant master when building the result row.

**SALK3** (Total Value):

Total value of the material in the valuation area. The EI uses it when selecting material valuation.

**SMRATIO** (Standard vs Moving av. ratio):

Ratio of standard price to moving average price (STPRS/VERPR). The EI computes it for each material and uses it to filter by the configured range.

**SOBSK** (SpecProcurem Costing):

Special procurement type for costing. The EI uses it when selecting plant-level material.

**SOBSL** (Special procurement):

Special procurement type. The EI uses it when selecting plant-level material.

**STPRS** (Standard Price):

Standard price of the material in the valuation area. The EI uses it when selecting material valuation and when computing the standard-to-moving-average ratio.

**VERPR** (Moving Average Price):

Moving average price of the material in the valuation area. The EI uses it when selecting material valuation and when computing the ratio; when VERPR is zero the ratio is not computed.

**VPRSV** (Price control):

Price control (standard vs moving average). The EI uses it when selecting material valuation.

**VPRSV Options:**
- **S**: Standard price
- **V**: Moving average price

**WERKS** (Plant):

Plant. The EI uses it when selecting plant-level material and material valuation.

**WGBEZ** (Description):

Material group description. Populated by the EI from material group master when building the result row.

**ZPLD1 - ZPLD3** (Planned price date 1 – Planned price date 3):

Planned price dates 1–3 for the material valuation. The EI uses them when selecting material valuation.

**ZPLP1 - ZPLP3** (Planned price 1 – Planned price 3):

Planned prices 1–3 for the material valuation. The EI uses them when selecting material valuation.

**SMRATIO and STPRS/VERPR Connection:** SMRATIO is computed as STPRS/VERPR; the EI filters by the configured SMRATIO range to return only materials whose standard-to-moving-average ratio falls within that range. STPRS and VERPR are the inputs to the ratio.


### Parameter Relationships

**Time and date parameters**

- **BACKDAYS** defines how many days to look back from today for the material last-change date (LAEDA). When no date range is supplied, the EI uses today minus BACKDAYS as the start of the window; LAEDA is then used to filter materials whose last change falls within that window.

**Standard vs moving average ratio**

- **SMRATIO**, **STPRS**, and **VERPR** work together: the EI computes SMRATIO as STPRS/VERPR for each material and filters by the configured SMRATIO range. STPRS (standard price) and VERPR (moving average price) are read from material valuation; SMRATIO is the filter criterion for the exception (e.g. ratio outside a given range such as 0.5–2.0).

**Material and plant scope**

- **MATNR**, **WERKS**, **MATKL**, and **MTART** define the material and organizational scope: material number, plant, material group, and material type. The EI uses them together when selecting material master, plant-level material (MARC), and material valuation (MBEW).

**Valuation and costing parameters**

- **BKLAS**, **BWKEY**, **VPRSV**, **PEINH**, **LBKUM**, and **SALK3** are valuation-area and valuation-class attributes; the EI uses them when selecting material valuation and when building the result. **ZPLP1–ZPLP3** and **ZPLD1–ZPLD3** (planned prices and planned price dates) work together as planned-price fields in material valuation.


### Default Values

- **BACKDAYS** — Default: `100` (when not supplied, the EI uses today minus 100 days as the start of the last-change date window).
- **LANGU** — Default: `E` (when system language is not found or not supplied).

**Note:** When no date range is supplied for LAEDA, the EI builds the window from today minus BACKDAYS.

### Practical Configuration Examples

**Use Case 1: Standard vs moving average ratio – last 100 days**
```
BACKDAYS = 100
SMRATIO = 0.5 - 2.0
WERKS = 1010
```
**Purpose:** Monitor materials in plant 1010 whose standard-to-moving-average ratio falls outside 0.5–2.0 (e.g. ratio &lt; 0.5 or &gt; 2.0), using the default 100-day lookback on last-change date.

**Use Case 2: One material group and price control**
```
BACKDAYS = 60
MATKL = 001
VPRSV = S
SMRATIO = 0.8 - 1.25
```
**Purpose:** Focus on materials in material group 001 with standard price control (VPRSV = S) and ratio outside 0.8–1.25 over the last 60 days.

**Use Case 3: Multi-plant and valuation class**
```
BACKDAYS = 90
WERKS = 1010 1020
BKLAS = 3000
SMRATIO = 0.5 - 2.0
MTART = FERT
```
**Purpose:** Review finished goods (MTART = FERT) in plants 1010 and 1020 with valuation class 3000 whose standard vs moving average ratio is outside 0.5–2.0 over the last 90 days.

**Use Case 4: Narrow ratio and language**
```
BACKDAYS = 30
SMRATIO = 0.9 - 1.1
LANGU = E
MATNR = 000000000010000000
WERKS = 1010
```
**Purpose:** Tight ratio band (0.9–1.1) for a specific material and plant over 30 days, with English descriptions.

**Use Case 5: Procurement type and special procurement**
```
BACKDAYS = 45
BESKZ = E
SOBSL = 10
SMRATIO = 0.7 1.5
WERKS = 1010
MATKL = 002
```
**Purpose:** External procurement materials (BESKZ = E) with special procurement 10 in plant 1010 and material group 002, ratio outside 0.7–1.5, last 45 days.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_02_MAT_VALUE | BESKZ | Procurement Type | CHAR(1) | BESKZ |
| /SKN/S_SW_10_02_MAT_VALUE | BKLAS | Valuation Class | CHAR(4) | BKLAS |
| /SKN/S_SW_10_02_MAT_VALUE | BWKEY | Valuation Area | CHAR(4) | BWKEY |
| /SKN/S_SW_10_02_MAT_VALUE | EKALR | Material Is Costed with Quantity Structure | CHAR(1) | CK_EKALREL |
| /SKN/S_SW_10_02_MAT_VALUE | LAEDA | Date of Last Change | DATS(8) | LAEDA |
| /SKN/S_SW_10_02_MAT_VALUE | LBKUM | Total Valuated Stock | QUAN(13,3) | LBKUM |
| /SKN/S_SW_10_02_MAT_VALUE | LOSGR | Planned lot size | QUAN(13,3) | LOSGR |
| /SKN/S_SW_10_02_MAT_VALUE | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_02_MAT_VALUE | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_MAT_VALUE | MAT_DESC | Material Description (Short Text) | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_MAT_VALUE | MMSTA | Plant-Specific Material Status | CHAR(2) | MMSTA |
| /SKN/S_SW_10_02_MAT_VALUE | MTART | Material Type | CHAR(4) | MTART |
| /SKN/S_SW_10_02_MAT_VALUE | MTBEZ | Description of material type | CHAR(25) | MTBEZ |
| /SKN/S_SW_10_02_MAT_VALUE | NCOST | Do Not Cost | CHAR(1) | CK_NO_COSTING |
| /SKN/S_SW_10_02_MAT_VALUE | PEINH | Price Unit | DEC(5) | PEINH |
| /SKN/S_SW_10_02_MAT_VALUE | PLANT_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_02_MAT_VALUE | SALK3 | Value of Total Valuated Stock | CURR(13,2) | SALK3 |
| /SKN/S_SW_10_02_MAT_VALUE | SMRATIO | Field of type FLTP | FLTP(16,16) | FLOAT |
| /SKN/S_SW_10_02_MAT_VALUE | SOBSK | Special Procurement Type for Costing | CHAR(2) | CK_SOBSL |
| /SKN/S_SW_10_02_MAT_VALUE | SOBSL | Special procurement type | CHAR(2) | SOBSL |
| /SKN/S_SW_10_02_MAT_VALUE | STPRS | Standard price | CURR(11,2) | STPRS |
| /SKN/S_SW_10_02_MAT_VALUE | VERPR | Moving Average Price/Periodic Unit Price | CURR(11,2) | VERPR |
| /SKN/S_SW_10_02_MAT_VALUE | VPRSV | Price control indicator | CHAR(1) | VPRSV |
| /SKN/S_SW_10_02_MAT_VALUE | WERKS | Plant | CHAR(4) | WERKS_D |
| /SKN/S_SW_10_02_MAT_VALUE | WGBEZ | Text (20 Characters) | CHAR(20) | TEXT20 |
| /SKN/S_SW_10_02_MAT_VALUE | ZPLD1 | Date from Which Future Planned Price 1 Is Valid | DATS(8) | DZPLD1 |
| /SKN/S_SW_10_02_MAT_VALUE | ZPLD2 | Date from Which Future Planned Price 1 Is Valid | DATS(8) | DZPLD1 |
| /SKN/S_SW_10_02_MAT_VALUE | ZPLD3 | Date from Which Future Planned Price 1 Is Valid | DATS(8) | DZPLD1 |
| /SKN/S_SW_10_02_MAT_VALUE | ZPLP1 | Future Planned Price 1 | CURR(11,2) | DZPLP1 |
| /SKN/S_SW_10_02_MAT_VALUE | ZPLP2 | Future Planned Price 2 | CURR(11,2) | DZPLP2 |
| /SKN/S_SW_10_02_MAT_VALUE | ZPLP3 | Future Planned Price 3 | CURR(11,2) | DZPLP3 |

## ABAP Code

`bap
FUNCTION /SKN/F_SW_10_02_MAT_VALUE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_MAT_VALUE
*"----------------------------------------------------------------------
  DATA : SPRAS_T TYPE SPRAS .
  DATA : SY_TABIX LIKE SY-TABIX,
         DATE_FROM LIKE SY-DATUM .
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
              SMRATIO  F       ,
             " price_val wert11v,
             " laeda laeda,
              DATUM SY-DATUM.
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS I.
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA : LS_DATA LIKE LINE OF T_DATA.
  FIELD-SYMBOLS: <ALERT_FIELDS>  TYPE /SKN/S_SW_10_02_MAT_VALUE.
  "-----------------------------------------------
  " 2. Extracting & Populating Parameters        "
  "-----------------------------------------------
  SELECT_SINGLE: LANGU,
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
                SOBSL	,
                SMRATIO."
              "  price_val ."
              "  laeda ."
  CONVERT_MULTY: MATNR MATN1.
  CONVERT_SINGLE: LANGU ISOLA.
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert Data           "
  "-----------------------------------------------
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_MAT_VALUE'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
SELECT *
    FROM MARC AS A
    INNER JOIN T001W AS B ON A~WERKS = B~WERKS
    INNER JOIN MBEW AS C ON A~MATNR = C~MATNR
           AND B~BWKEY = C~BWKEY
    INNER JOIN MARA AS D ON D~MATNR = A~MATNR
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE D~MATKL IN R_MATKL AND
          D~MTART IN R_MTART AND
          A~WERKS IN R_WERKS AND
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
          D~LAEDA IN R_DATUM .
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
*  LOOP AT t_data INTO ls_data.
*
*
*  ENDLOOP.
  "-----------------------------------------------
  " 6. Post retrieving filtering                 "
  "-----------------------------------------------
*  DELETE T_DATA WHERE STATE_COLOR NOT IN R_STATE_COLOR.
  LOOP AT T_DATA ASSIGNING <ALERT_FIELDS> ."INTO ls_data.
    CLEAR <ALERT_FIELDS>-SMRATIO.
    IF <ALERT_FIELDS>-VERPR <> 0 .
      <ALERT_FIELDS>-SMRATIO = <ALERT_FIELDS>-STPRS / <ALERT_FIELDS>-VERPR.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE SMRATIO NOT IN R_SMRATIO.
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
        WERKS            = T_DATA-WERKS
       LANGU            = LV_LANGU
     IMPORTING
       PLANT_DESC       = T_DATA-PLANT_DESC
     EXCEPTIONS
       WRONG_CODE       = 1
       OTHERS           = 2     .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
      EXPORTING
        MATKL            = T_DATA-MATKL
       LANGU            = LV_LANGU
     IMPORTING
       MATKL_DESC       = T_DATA-WGBEZ
     EXCEPTIONS
       WRONG_CODE       = 1
       OTHERS           = 2 .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_MAT_TYP_DESC'
      EXPORTING
       MTART            = T_DATA-MTART
       LANGU            = LV_LANGU
     IMPORTING
       MTBEZ       = T_DATA-MTBEZ
     EXCEPTIONS
       WRONG_CODE       = 1
       OTHERS           = 2 .
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
`
