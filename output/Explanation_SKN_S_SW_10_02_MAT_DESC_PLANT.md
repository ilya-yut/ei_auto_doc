# Exception Indicator: Missing Material Description in Plant Language ( SW_10_02_MAT_DS_PL_M)

## General Overview

This Exception Indicator finds plant–material combinations where the material description is missing in the plant’s language, while still returning a presentation-language description for comparison.

This EI serves as an essential control for material master and logistics quality by:

- Detecting gaps between plant language maintained on the plant record and available material texts
- Surfacing materials that would display without a plant-language description in operational reports
- Supporting plant and material-status scoping so remediation teams can focus on active sites
- Providing a fallback presentation-language description when the plant-language text is blank
- Reducing picking, production, and inventory errors caused by incomplete material names

Typical use includes master-data cleanup before go-live at a plant, periodic plant-language text reviews, and pre-audit checks on critical materials. Results support exception workflows rather than full material plant extracts.

The routine reads material plant data joined to plant master language, tests descriptions through the shared material-description service, and raises an alert when qualifying gaps remain.


## Problem Description

When plant-language material descriptions are missing, warehouse and manufacturing users may see blank or fallback text that does not match the language configured for the plant.

**Master Data and Logistics Risks**

- Incomplete texts slow identification of materials during goods movements and physical inventory
- New plant extensions or material plant records may be used operationally before texts exist in the plant language
- Plant-specific material status settings can include records that are active but still lack proper descriptions

**Operational Risks**

- Reliance on a presentation language that differs from the plant language can confuse floor and logistics staff
- Broad plant lists without material or status filters can produce queues that are hard to clear

**Control and Audit Risks**

- Weak monitoring reduces evidence that plant-language texts were verified before operational use
- Undocumented fallback-language choices can mask systematic translation gaps

## Suggested Resolution

**Immediate Response**

- Review flagged materials, plants, plant languages, and presentation-language descriptions
- Confirm with master-data owners whether plant-language texts should be created or copied from a reference language
- Prioritize high-movement materials and strategic plants

**System Assessment**

- Compare exception counts by plant and language to find systemic gaps
- Validate material-status filters so obsolete or blocked records are excluded when appropriate
- Check whether presentation-language defaults match the organization’s master-data standards

**Corrective Actions**

- Maintain missing texts in the material master for each plant language through standard MM processes
- Adjust monitoring scope after remediation to keep the queue actionable
- Document review results and schedule recurring runs for key plants


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | DEF_PRES_LANGU | Mat Descr. Present. Lang |  | 0 | 0 |  |  |
| 2 | LANGU | Plant Language Key |  | 0 | 0 |  |  |
| 3 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 4 | MMSTA | Plant-sp.matl status | CHAR | 2 | 0 | MMSTA | MMSTA |
| 5 | SW_DEST |  | 0 | 0 |  |  |  |
| 6 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 6 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**DEF_PRES_LANGU** (Mat Descr. Present. Lang)

Language used to load the presentation material description on output when the plant-language text is missing (default English in code).

**DEF_PRES_LANGU Options:**
- Any valid language key (for example **E** for English) — Presentation description retrieved through the shared material-description service for that language.

**LANGU** (Plant Language Key)

Language key used for language-dependent texts and user-language filtering.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MMSTA** (Plant-sp.matl status)

Material Status restricts how a material can be used in purchasing or inventory based on its current status.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.


### Parameter Relationships

How parameter combinations work together

**Plant scope:** **WERKS**, **MATNR**, and **MMSTA** limit which material plant records are selected from inventory management views joined to plant master data.

**Plant language:** **LANGU** filters the plant language key taken from the plant master record used as the description language for each row.

**Missing-text logic:** For each selected plant–material row, the routine requests a description in the plant language; when that text is empty, it keeps the row and fills **MAT_DESC** from **DEF_PRES_LANGU**. When plant-language text exists, the row is removed from the result.

**Execution path:** **SW_DEST** delegates to the cloud function when set; otherwise the on-premise logic above runs locally.


### Default Values

- **DEF_PRES_LANGU** - initial - treated as E by code

### Practical Example of Parameter Configuration

**Use Case 1: Plant-language gaps with English presentation**

**Purpose:** List materials at selected plants where the plant-language description is missing and show English as the presentation-language text.

```
WERKS = 1000
DEF_PRES_LANGU = E
```

**Use Case 2: Single material at one plant**

**Purpose:** Verify one material’s plant-language text before release to production.

```
MATNR = 12345678
WERKS = 2000
MMSTA = 01
```

**Use Case 3: German plant language filter**

**Purpose:** Monitor plants configured with German as the plant language key.

```
LANGU = D
WERKS = 3000
DEF_PRES_LANGU = E
```

**Use Case 4: Multi-plant review with status filter**

**Purpose:** Compare missing-description exceptions for active materials across two plants.

```
WERKS = 4000
MMSTA = 01
MATNR = 87654321
DEF_PRES_LANGU = E
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_02_MAT_DESC_PLANT | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_MAT_DESC_PLANT | MAT_DESC | Material Description | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_MAT_DESC_PLANT | MMSTA | Plant-sp.matl status | CHAR(2) | MMSTA |
| /SKN/S_SW_10_02_MAT_DESC_PLANT | NAME1 | Name 1 | CHAR(30) | NAME1 |
| /SKN/S_SW_10_02_MAT_DESC_PLANT | SPRAS | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_10_02_MAT_DESC_PLANT | WERKS | Plant | CHAR(4) | WERKS_D |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_MAT_DESC_PLANT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_MAT_DESC_PLANT OPTIONAL
*"----------------------------------------------------------------------
DATA : SPRAS_T TYPE SPRAS .
DATA : SY_TABIX LIKE SY-TABIX .
DATA : LV_MAT_DESC_PLANT LIKE MAKT-MAKTX.
 DATA_SINGLE: DEF_PRES_LANGU  LANGU.   " Language for Mat Description Presentation
 LV_DEF_PRES_LANGU = 'E'.              " Default Value 
 SELECT_SINGLE: DEF_PRES_LANGU.
DATA_MULTY: MATNR        MATNR,
            WERKS        WERKS_D,
            LANGU        LAISO, "LANGU,
            MMSTA        MMSTA .
SELECT_MULTY:
            MATNR,
            WERKS,
            LANGU,
            MMSTA .
CONVERT_MULTY: MATNR MATN1,
               LANGU ISOLA.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_MAT_DESC_PLNT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM MARC AS A
    INNER JOIN T001W AS B
    ON A~WERKS = B~WERKS
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE  A~MATNR IN R_MATNR
       AND A~WERKS IN R_WERKS
       AND A~MMSTA IN R_MMSTA
       AND B~SPRAS IN R_LANGU.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    CLEAR LV_MAT_DESC_PLANT.
    CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
       EXPORTING
          MATNR              = T_DATA-MATNR
          LANGU              = T_DATA-SPRAS
       IMPORTING
         MATERIAL_DESC       = LV_MAT_DESC_PLANT
       EXCEPTIONS
         WRONG_CODE          = 1
         OTHERS              = 2.
        IF SY-SUBRC <> 0.
*       Implement suitable error handling here
        ENDIF.
    IF LV_MAT_DESC_PLANT IS INITIAL .
        "- Material desc EN
        CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
          EXPORTING
            MATNR               = T_DATA-MATNR
            LANGU               = LV_DEF_PRES_LANGU
         IMPORTING
           MATERIAL_DESC       = T_DATA-MAT_DESC
         EXCEPTIONS
           WRONG_CODE          = 1
         OTHERS                = 2.
        IF SY-SUBRC <> 0.
*       Implement suitable error handling here
        ENDIF.
        MODIFY T_DATA INDEX SY_TABIX.
    ELSE.
        DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
