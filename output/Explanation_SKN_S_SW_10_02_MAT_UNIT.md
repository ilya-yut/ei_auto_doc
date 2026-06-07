# Exception Indicator: Material master UoM per Sales Organization ( SW_10_02_MAT_UOM_SO)

## General Overview

This Exception Indicator reviews materials active in a sales organization and checks whether required alternate units of measure exist in the material unit-of-measure table—or whether a one-to-one conversion is recorded—for each unit of measure you specify.

This EI serves as an essential control for materials management and order-to-cash master data by:
- Surfacing materials sold in a sales organization that lack an expected alternate unit of measure
- Enabling detection of trivial one-to-one unit conversions that may indicate incomplete or placeholder unit definitions
- Supporting sales and logistics teams with material and material-type descriptions for the flagged combinations
- Helping master-data governance ensure order and delivery units align with commercial requirements by distribution channel
- Complementing manual MARM reviews with a repeatable, parameterized exception list tied to recent material changes

Typical use includes reviews after introducing new pack sizes, validating alternate units for a distribution channel, or periodic hygiene on materials changed within a lookback window. Results are intended for exception workflows rather than full material master extracts.

The routine reads general and sales data for the sales organization, expands the requested alternate units of measure, compares against unit-of-measure records for each material, and raises an alert when gaps or one-to-one conversions are found.


## Problem Description

Failure to monitor alternate units of measure for materials in a sales organization creates multiple risks across logistics, sales order processing, and master data quality.

**Sales and Logistics Risks**
- Order and delivery processing may fail or default to the base unit when an expected alternate unit is missing
- Pricing or quantity conversion errors can occur when one-to-one unit records mask missing real conversion factors
- New materials may reach the sales organization without complete unit-of-measure setup for required pack or sales units

**Master Data and Operations Risks**
- Bulk material changes within a lookback window can leave unit tables out of sync with commercial packaging decisions
- Concentrations by distribution channel or material group are harder to see without a filtered exception population

**Compliance and Audit Risks**
- Evidence of periodic unit-of-measure review is weaker when checks rely on ad hoc table browsing
- Cross-organization comparisons delay when sales-organization scope is not applied consistently

## Suggested Resolution

**Immediate Response**
- Review each flagged material together with base unit, alternate unit, sales organization, and conversion numerators and denominators shown in the exception
- Confirm with materials management whether the alternate unit should be created, corrected, or is intentionally absent
- Prioritize high-volume or customer-facing materials before the next order cycle

**System Assessment**
- Compare this cycle to prior runs after product launches, packaging changes, or mass uploads
- Look for concentrations by material type, distribution channel, or material group to see whether one project drives most items
- Revisit the list of alternate units requested when the queue contains units not used commercially

**Corrective Actions**
- Maintain or correct unit-of-measure records through standard material master processes with required approvals
- Adjust monitoring scope after root cause so the queue stays actionable for master-data teams
- Update packaging or sales-unit standards when specific alternate units must always exist
- Route repeat interface or migration defects into change management when unit records are systematically incomplete


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | back days |  | 0 | 0 |  |  |
| 2 | BREIT | Width | QUAN | 13 | 3 | BREIT | MENG13 |
| 3 | BRGEW | Gross Weight | QUAN | 13 | 3 | BRGEW | MENG13 |
| 4 | EAN11 | EAN/UPC | CHAR | 18 | 0 | EAN11 | EAN11 |
| 5 | GEWEI | Weight unit | UNIT | 3 | 0 | GEWEI | MEINS |
| 6 | HOEHE | Height | QUAN | 13 | 3 | HOEHE | MENG13 |
| 7 | LAENG | Length | QUAN | 13 | 3 | LAENG | MENG13 |
| 8 | MAT_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 9 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 10 | MEABM | Unit of Dimension | UNIT | 3 | 0 | MEABM | MEINS |
| 11 | MEINH | Alternative Unit of Measure | UNIT | 3 | 0 | LRMEI | MEINS |
| 12 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 13 | MSEHI | Int. meas. unit | UNIT | 3 | 0 | MSEHI | MEINS |
| 14 | MTART | Material Type | CHAR | 4 | 0 | MTART | MTART |
| 15 | MTBEZ | Material type descr. | CHAR | 25 | 0 | MTBEZ | TEXT25 |
| 16 | MTPOS | Item category group | CHAR | 4 | 0 | MTPOS | MTPOS |
| 17 | MVGR1 | Material group 1 | CHAR | 3 | 0 | MVGR1 | MVGR1 |
| 18 | MVGR2 | Material group 2 | CHAR | 3 | 0 | MVGR2 | MVGR2 |
| 19 | MVGR3 | Material group 3 | CHAR | 3 | 0 | MVGR3 | MVGR3 |
| 20 | NTGEW | Net Weight | QUAN | 13 | 3 | NTGEW | MENG13 |
| 21 | NUMTP | EAN Category | CHAR | 2 | 0 | NUMTP | NUMTP |
| 22 | PRODH | Product hierarchy | CHAR | 18 | 0 | PRODH_D | PRODH |
| 23 | UMREN | Denominator | DEC | 5 | 0 | UMREN | UMBSN |
| 24 | UMREZ | Counter | DEC | 5 | 0 | UMREZ | UMBSZ |
| 25 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 26 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 26 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**BACKDAYS** (back days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on LAEDA

**BREIT** (Width)

Valuable when comparing health before and after a release—hold width on BREIT constant while varying other filters.

**BRGEW** (Gross Weight)

Gross weight of the logistics quantity used with GEWEI for shipping and freight calculations.

**EAN11** (EAN/UPC)

Interprets ean/upc as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on EAN11.

**GEWEI** (Weight unit)

Unit of measure for weight fields such as BRGEW and NTGEW in logistics quantity conversions.

**HOEHE** (Height)

Mirrors how administrators slice operational lists: height (HOEHE) is one lever that shapes which rows are comparable run over run.

**LAENG** (Length)

Prevents accidental global scans when length (LAENG) is meant to stay within a controlled application slice.

**MAT_DESC** (Material Description)

Material description text used to provide readable product context.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MEABM** (Unit of Dimension)

Supports escalation where unit of dimension on MEABM signals ownership for follow-up between Basis and functional teams.

**MEINH** (Alternative Unit of Measure)

Stabilizes week-over-week metrics by fixing alternative unit of measure (MEINH) while allowing duration thresholds to move.

**MEINS** (Base Unit of Measure)

Base unit of measure used to interpret quantity fields consistently.

**MSEHI** (Int. meas. unit)

Narrows retrieved rows where int. meas. unit (MSEHI) must match the configured selection for this monitor.

**MTART** (Material Type)

Material type classifying procurement, production, and valuation behavior of material master records.

**MTBEZ** (Material type descr.)

Material Type Description stores the text name that describes a specific material type code.

**MTPOS** (Item category group)

Narrows retrieved rows where item category group (MTPOS) must match the configured selection for this monitor.

**MVGR1 - MVGR3** (Material group 1)

Stabilizes week-over-week metrics by fixing material group 1 (MVGR1) while allowing duration thresholds to move.

**NTGEW** (Net Weight)

Net weight of the shipped or ordered quantity paired with GEWEI for logistics weight checks.

**NUMTP** (EAN Category)

When combined with destination discipline, ean category on NUMTP keeps both breadth and depth of the extract intentional.

**PRODH** (Product hierarchy)

When left open per framework rules, PRODH does not restrict product hierarchy; when set, only matching rows remain.

**UMREN** (Denominator)

Guards against oversized extracts when denominator on UMREN is narrowed together with client, user, or session filters.

**UMREZ** (Counter)

Helps distinguish technical versus business attributes when counter on UMREZ correlates with counters or status fields.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.


### Parameter Relationships

How parameter combinations work together

**Sales organization scope:** **VKORG** sets the sales organization used when reading sales-material data; **VTWEG**, **MTPOS**, **PRODH**, and material group fields (**MVGR1**, **MVGR2**, **MVGR3**) further narrow the commercial population.

**Material selection:** **MATNR**, **MTART**, **MEINS**, weight fields (**NTGEW**, **GEWEI**), and **BACKDAYS** (with last-change date on the general material record) define which materials enter the check before alternate units are evaluated.

**Alternate unit list:** **MEINH** supplies the alternate units of measure to verify for each material; the routine resolves valid units against the units table and tests each material for missing entries or one-to-one conversions.

**Conversion attributes:** When a unit record exists, **UMREZ**, **UMREN**, **BRGEW**, **EAN11**, **NUMTP**, and dimension fields (**LAENG**, **BREIT**, **HOEHE**, **MEABM**) participate in matching existing unit records.

**Final selection:** Sales-organization scope, material filters, the lookback window, and the requested alternate units apply together—rows appear when a material lacks the expected unit or carries a one-to-one conversion for a listed alternate unit.


### Default Values

- **BACKDAYS** - initial - treated as 100 by code

### Practical Example of Parameter Configuration

**Use Case 1: Missing sales unit (EA) for finished goods**

**Purpose:** Find finished materials changed in the last thirty days in a sales organization that lack an each alternate unit or show a one-to-one conversion.
```
VKORG = 1000
MTART = FERT
MEINH = EA
BACKDAYS = 30
VTWEG = 10
```

**Use Case 2: Case unit review for a distribution channel**

**Purpose:** Check raw materials in one distribution channel for missing or trivial case conversions.
```
VKORG = 2000
MTART = ROH
MEINH = CS
BACKDAYS = 14
VTWEG = 20
MTPOS = NORM
```

**Use Case 3: Pallet unit on a material group**

**Purpose:** Monitor a material group for pallet alternate units on recently changed materials.
```
VKORG = 1000
MVGR1 = 01
MEINH = PAL
BACKDAYS = 7
MATNR = 30000000 - 39999999
```

**Use Case 4: Multiple alternate units for a product hierarchy**

**Purpose:** Validate both box and kilogram alternate units for products in a selected hierarchy node.
```
VKORG = 1000
PRODH = 0000100001*
MEINH = BOX
BACKDAYS = 60
MEINS = KG
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_02_MAT_UNIT | BACKDAYS | back days | CHAR(0) | BACKDAYS |
| /SKN/S_SW_10_02_MAT_UNIT | BREIT | Width | QUAN(13) | BREIT |
| /SKN/S_SW_10_02_MAT_UNIT | BRGEW | Gross Weight | QUAN(13) | BRGEW |
| /SKN/S_SW_10_02_MAT_UNIT | EAN11 | EAN/UPC | CHAR(18) | EAN11 |
| /SKN/S_SW_10_02_MAT_UNIT | GEWEI | Weight unit | UNIT(3) | GEWEI |
| /SKN/S_SW_10_02_MAT_UNIT | HOEHE | Height | QUAN(13) | HOEHE |
| /SKN/S_SW_10_02_MAT_UNIT | LAENG | Length | QUAN(13) | LAENG |
| /SKN/S_SW_10_02_MAT_UNIT | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_MAT_UNIT | MAT_DESC | Material Description | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_MAT_UNIT | MEABM | Unit of Dimension | UNIT(3) | MEABM |
| /SKN/S_SW_10_02_MAT_UNIT | MEINH | Alternative Unit of Measure | UNIT(3) | LRMEI |
| /SKN/S_SW_10_02_MAT_UNIT | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_02_MAT_UNIT | MSEHI | Int. meas. unit | UNIT(3) | MSEHI |
| /SKN/S_SW_10_02_MAT_UNIT | MTART | Material Type | CHAR(4) | MTART |
| /SKN/S_SW_10_02_MAT_UNIT | MTBEZ | Material type descr. | CHAR(25) | MTBEZ |
| /SKN/S_SW_10_02_MAT_UNIT | MTPOS | Item category group | CHAR(4) | MTPOS |
| /SKN/S_SW_10_02_MAT_UNIT | MVGR1 | Material group 1 | CHAR(3) | MVGR1 |
| /SKN/S_SW_10_02_MAT_UNIT | MVGR2 | Material group 2 | CHAR(3) | MVGR2 |
| /SKN/S_SW_10_02_MAT_UNIT | MVGR3 | Material group 3 | CHAR(3) | MVGR3 |
| /SKN/S_SW_10_02_MAT_UNIT | NTGEW | Net Weight | QUAN(13) | NTGEW |
| /SKN/S_SW_10_02_MAT_UNIT | NUMTP | EAN Category | CHAR(2) | NUMTP |
| /SKN/S_SW_10_02_MAT_UNIT | PRODH | Product hierarchy | CHAR(18) | PRODH_D |
| /SKN/S_SW_10_02_MAT_UNIT | UMREN | Denominator | DEC(5) | UMREN |
| /SKN/S_SW_10_02_MAT_UNIT | UMREZ | Counter | DEC(5) | UMREZ |
| /SKN/S_SW_10_02_MAT_UNIT | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_02_MAT_UNIT | VTWEG | Distribution Channel | CHAR(2) | VTWEG |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_MAT_UOM_SO .
"----------------------------------------------------------------------
"*"Local Interface:
"  EXPORTING
"     VALUE(IS_ALERT) TYPE  CHAR1
"  TABLES
"      T_SELECT STRUCTURE  RSSELECT
"      T_DATA STRUCTURE  /SKN/S_SW_10_02_MAT_UNIT
"----------------------------------------------------------------------
  DATA : SPRAS_T TYPE SPRAS .
  DATA : SY_TABIX LIKE SY-TABIX,
         DATE_FROM LIKE SY-DATUM .
  TYPES: BEGIN OF MARM_FIELDS_TYPE,
          MEINH TYPE LRMEI,
          UMREZ TYPE UMREZ,
          UMREN TYPE UMREN,
          BRGEW TYPE BRGEW,
          GEWEI TYPE GEWEI,
          EAN11 TYPE EAN11,
          NUMTP TYPE NUMTP,
          LAENG	TYPE LAENG,
          BREIT	TYPE BREIT,
          HOEHE	TYPE HOEHE,
          MEABM	TYPE MEABM,
        END OF MARM_FIELDS_TYPE.
  DATA:  MARM_FIELDS TYPE MARM_FIELDS_TYPE.
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_MULTY: MATNR  MATNR,
              MTART  MTART,
              MEINS MEINS,
              NTGEW NTGEW,   " !!!!
              GEWEI GEWEI,   "MARM-GEWEI  Weight Unit??
              VTWEG VTWEG,
              MTPOS MTPOS,
              PRODH  PRODH_D,
              MVGR1  MVGR1,
              MVGR2  MVGR2,
              MVGR3  MVGR3,
              MEINH  LRMEI,
              UMREZ UMREZ,
              UMREN  UMREN,
              BRGEW BRGEW,
              EAN11 EAN11,
              NUMTP NUMTP,
              LAENG LAENG,
              BREIT BREIT,
              HOEHE HOEHE,
              MEABM MEABM,
              DATUM SY-DATUM.
  " T006 -  is value table for unit s of mesure
  DATA_MULTY:  UOM          LRMEI.
  DATA_SINGLE: UOM          LRMEI.
  DATA_SINGLE: VKORG VKORG,
               LANGU  LANGU.
  "-----------------------------------------------
  " Additional Definition                        "
  "-----------------------------------------------
  DATA: LS_MARM TYPE MARM,
        LT_MARM LIKE TABLE OF LS_MARM.
  DATA: LS_DATA LIKE LINE OF T_DATA,
        LT_DATA LIKE TABLE OF LS_DATA WITH HEADER LINE.
  FIELD-SYMBOLS: <ALERT_FIELDS>  TYPE /SKN/S_SW_10_02_MAT_UNIT.
  "-----------------------------------------------
  " 2. Extracting & Populating Parameters        "
  "-----------------------------------------------
  LV_LANGU = 'E'.
  SELECT_SINGLE: VKORG,
                 LANGU.   " ratio ????
  SELECT_MULTY: MATNR ,"
                MTART,
                MEINS,
                NTGEW,   " !!!!
                GEWEI,   "!!!!
                VTWEG,
                MTPOS,
                PRODH,
                MVGR1,
                MVGR2,
                MVGR3,
                MEINH,
                UMREZ,
                UMREN,
                BRGEW,
                EAN11,
                NUMTP,
                LAENG,
                BREIT,
                HOEHE,
                MEABM  ."
  CHECK R_MEINH[] IS NOT INITIAL.
  CONVERT_MULTY: MATNR MATN1,
          MEINS CUNIT,
          GEWEI CUNIT,
          MEINH CUNIT,
          EAN11 EAN11,
          MEABM CUNIT.
  CONVERT_SINGLE: LANGU ISOLA.
  "-----------------------------------------------
  " 3. Initiating Output Table(Mandatory!!!)     "
  "-----------------------------------------------
  CLEAR IS_ALERT .
  REFRESH T_DATA .
  "-----------------------------------------------
  " 4. Retrieving/preparing Alert DaMATNR MATN1,ta           "
  "-----------------------------------------------
  "--- Run Cloud Mode -----
  "--- Prepare UOM list
  REFRESH R_UOM.
  SELECT MSEHI
     INTO LV_UOM
     FROM T006
     WHERE MSEHI IN R_MEINH.
    RS_UOM-SIGN = 'I'.
    RS_UOM-OPTION = 'EQ'.
    RS_UOM-LOW = LV_UOM.
    APPEND RS_UOM TO R_UOM.
  ENDSELECT.
  CHECK R_UOM[] IS NOT INITIAL.
  DATA_SINGLE: SW_DEST RFCDEST,
               BACKDAYS I.
  SELECT_SINGLE: SW_DEST,
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
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_MAT_UOM_SO'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH LT_DATA.
  REFRESH T_DATA.
  SELECT *
    FROM MARA AS A
    INNER JOIN MVKE AS B
    ON A~MATNR = B~MATNR
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE  A~MATNR IN R_MATNR AND
           A~MTART IN R_MTART AND
           A~MEINS IN R_MEINS AND
           A~NTGEW IN R_NTGEW AND   " !!!!
           A~GEWEI IN R_GEWEI AND   "!!!!
           A~LAEDA IN R_DATUM AND " backdays parameter
           B~VKORG = LV_VKORG AND
           B~VTWEG IN R_VTWEG AND
           B~MTPOS IN R_MTPOS AND
           B~PRODH IN R_PRODH AND
           B~MVGR1  IN R_MVGR1 AND
           B~MVGR2  IN R_MVGR2 AND
           B~MVGR3  IN R_MVGR3.
**************
  IF LT_DATA[] IS NOT INITIAL.
    SELECT *
     FROM MARM
     INTO CORRESPONDING FIELDS OF TABLE LT_MARM
     FOR ALL ENTRIES IN LT_DATA
     WHERE   MATNR = LT_DATA-MATNR  AND
             MEINH IN R_UOM AND
             UMREZ IN R_UMREZ AND
             UMREN  IN R_UMREN AND
             BRGEW IN R_BRGEW AND
             EAN11 IN R_EAN11 AND
             NUMTP IN R_NUMTP AND
             LAENG IN R_LAENG AND
             BREIT IN R_BREIT AND
             HOEHE IN R_HOEHE AND
             MEABM IN R_MEABM.
  ENDIF.
  SORT LT_MARM BY MATNR MEINH.
  "--- Fill T_DATA
  REFRESH T_DATA.
  LOOP AT LT_DATA INTO LS_DATA.
    LOOP AT R_UOM INTO RS_UOM.
      LV_UOM = RS_UOM-LOW.
      READ TABLE LT_MARM INTO LS_MARM
                         WITH KEY MATNR = LS_DATA-MATNR
                                  MEINH = LV_UOM
                          BINARY SEARCH.
      IF SY-SUBRC <> 0. "--No Reqired UOM
        LS_DATA-MEINH = LV_UOM.
        APPEND LS_DATA TO T_DATA.
      ELSE." delete t_data where umres <> 1 or umren <> 1.
        IF LS_MARM-UMREN  =  1 AND LS_MARM-UMREZ = 1  .
          MOVE-CORRESPONDING LS_MARM TO MARM_FIELDS.
          MOVE-CORRESPONDING MARM_FIELDS TO LS_DATA.
          APPEND LS_DATA TO T_DATA.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  SORT T_DATA BY MATNR MEINH.
  DELETE ADJACENT DUPLICATES FROM T_DATA COMPARING MATNR MEINH.
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
