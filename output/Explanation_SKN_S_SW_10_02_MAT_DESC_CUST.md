# Exception Indicator: Missing Material Description in Customer Language ( SW_10_02_MAT_DS_CS_M)

## General Overview

This Exception Indicator identifies materials in selected sales areas where the material description is missing in a customer’s language, while still exposing the presentation-language description used for comparison.

This EI serves as an essential control for master data and order-to-cash quality by:

- Detecting gaps between customer language requirements and maintained material texts in sales views
- Highlighting material–sales-area combinations that would ship or quote without a customer-language description
- Supporting sales-organization and distribution-channel scoping so teams can focus on relevant commercial territories
- Enabling optional expansion to customer-level detail when accountability per account is required
- Reducing downstream order-entry and customer-service errors caused by blank or incomplete material texts

Typical use includes master-data remediation queues, pre-release checks for new materials, and periodic reviews before catalog or pricing updates. Results support exception workflows rather than full material master extracts.

The routine reads customer languages from sales master data, evaluates material sales-area records, and calls the shared material-description service for each required language.


## Problem Description

When materials lack descriptions in customer languages for active sales areas, order processing, customer communications, and analytics can show incomplete or misleading product information.

**Master Data and Commercial Risks**

- Blank customer-language texts can delay quoting, ordering, and fulfillment while users search for correct material names
- New or extended sales-area assignments may go live before texts exist in every language used by customers in that area
- Cross-distribution-chain or distribution-chain-specific status settings can hide materials that are commercially active but textually incomplete

**Operational Risks**

- Customer-service and inside-sales teams may rely on a fallback language that does not match the customer’s maintained language key
- Without customer-level detail, it is harder to assign remediation to the right account team when many customers share one language per sales area

**Control and Audit Risks**

- Inconsistent monitoring weakens evidence that language gaps were reviewed before catalog or campaign releases
- Undocumented fallback-language choices can create false confidence that customer-facing text is complete

## Suggested Resolution

**Immediate Response**

- Review flagged materials, sales organizations, distribution channels, languages, and presentation-language descriptions
- Confirm with master-data owners whether customer-language texts should be created, copied, or translated
- Prioritize high-volume or strategic materials and sales areas with the largest customer exposure

**System Assessment**

- Compare exception volume across sales areas and languages to spot systematic translation gaps
- Validate whether customer-detail mode is needed for accountability versus sales-area-level lists only
- Reconcile status filters so blocked or obsolete materials are not mixed into active remediation queues

**Corrective Actions**

- Maintain missing texts in the material master for each required language via standard MM/SD processes
- Adjust monitoring filters after cleanup so the queue remains actionable
- Document remediation outcomes and schedule recurring runs for critical sales organizations


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | CUST_DET | 'X' - Present Customer Details |  | 0 | 0 |  |  |
| 2 | DEF_PRES_LANGU | Mat Descr. Presentation Lang. |  | 0 | 0 |  |  |
| 3 | LANGU | Customer Language Key |  | 0 | 0 |  |  |
| 4 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 5 | MSTAV | X-distr.chain status | CHAR | 2 | 0 | MSTAV | VMSTA |
| 6 | SW_DEST |  | 0 | 0 |  |  |  |
| 7 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 8 | VMSTA | DChain-spec. status | CHAR | 2 | 0 | VMSTA | VMSTA |
| 9 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 9 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**CUST_DET** ('X' - Present Customer Details)

When set, rebuilds results with one output row per customer in the sales area whose language matches the missing-description case, including customer number and name. When empty, output stays at material, sales organization, distribution channel, and customer-language level only.

**CUST_DET Options:**
- **X** — Populate customer details from sales master for each qualifying language in the sales area.
- Empty or blank — Do not expand to customer rows; list material and sales-area context only.

**DEF_PRES_LANGU** (Mat Descr. Presentation Lang.)

Language used to load the presentation material description written to output when the customer-language text is missing (default English in code).

**DEF_PRES_LANGU Options:**
- Any valid language key (for example **E** for English) — Presentation description retrieved through the shared material-description service for that language.

**LANGU** (Customer Language Key)

Language key used for language-dependent texts and user-language filtering.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MSTAV** (X-distr.chain status)

Cross-distribution-chain material status applies a global restriction or block on a material across all sales organizations and distribution channels simultaneously.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VMSTA** (DChain-spec. status)

Distribution-chain-specific material status defines whether a material is available, restricted, or blocked for specific sales activities within a designated sales organization and distribution channel.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.


### Parameter Relationships

How parameter combinations work together

**Sales-area scope:** **VKORG**, **VTWEG**, **MATNR**, **VMSTA**, and **MSTAV** narrow which material sales-area records are read before language checks run.

**Customer languages:** **LANGU** filters which customer language keys from sales master data are considered when building the per-area language list.

**Missing-text detection:** For each material sales-area row, the routine requests a description in each qualifying customer language; when that text is empty, it fills **MAT_DESC** from **DEF_PRES_LANGU** and adds an output row.

**Customer detail mode:** When **CUST_DET** is set, output is rebuilt with one row per customer in the sales area that uses the flagged language, including **KUNNR** and **NAME1**; when empty, rows stay at material–sales-area–language level without customer breakdown.

**Execution path:** **SW_DEST** delegates processing to the cloud function when set; otherwise the on-premise logic described above runs locally.


### Default Values

- **DEF_PRES_LANGU** - initial - treated as E by code
- **CUST_DET** - initial - treated as blank by code

### Practical Example of Parameter Configuration

**Use Case 1: Sales-area gaps with English presentation text**

**Purpose:** List materials in a sales organization and channel where any customer language in that area has no description, showing English as the presentation-language text for comparison.

```
VKORG = 1000
VTWEG = 10
DEF_PRES_LANGU = E
```

**Use Case 2: Single material review**

**Purpose:** Check one material across selected sales areas and status values before releasing to customers.

```
MATNR = 12345678
VKORG = 2000
VTWEG = 20
VMSTA = 01
MSTAV = 01
```

**Use Case 3: Customer-level accountability**

**Purpose:** Expand each missing-language hit to individual customers in the sales area so account teams can follow up.

```
CUST_DET = X
VKORG = 3000
VTWEG = 30
LANGU = D
DEF_PRES_LANGU = E
```

**Use Case 4: German customer language filter**

**Purpose:** Monitor only German-language customer requirements in a defined sales area while keeping English presentation descriptions on output rows.

```
LANGU = D
VKORG = 4000
VTWEG = 10
MATNR = 87654321
DEF_PRES_LANGU = E
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_02_MAT_DESC_CUST | KUNNR | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_02_MAT_DESC_CUST | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_MAT_DESC_CUST | MAT_DESC | Material Description | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_MAT_DESC_CUST | MSTAV | X-distr.chain status | CHAR(2) | MSTAV |
| /SKN/S_SW_10_02_MAT_DESC_CUST | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_02_MAT_DESC_CUST | SPRAS | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_10_02_MAT_DESC_CUST | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_02_MAT_DESC_CUST | VMSTA | DChain-spec. status | CHAR(2) | VMSTA |
| /SKN/S_SW_10_02_MAT_DESC_CUST | VTWEG | Distribution Channel | CHAR(2) | VTWEG |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_MAT_DESC_CUST .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_MAT_DESC_CUST OPTIONAL
*"----------------------------------------------------------------------
DATA : SPRAS_T TYPE SPRAS .
DATA : SY_TABIX LIKE SY-TABIX .
DATA : LV_MAT_DESC_CUST LIKE MAKT-MAKTX.
DATA: BEGIN OF LS_CUST_LANGUE,
       VKORG TYPE VKORG,
       VTWEG TYPE VTWEG,
       SPRAS TYPE SPRAS,
      END OF LS_CUST_LANGUE.
DATA: LT_CUST_LANGUE LIKE TABLE OF LS_CUST_LANGUE.
DATA: BEGIN OF LS_MVKE.
INCLUDE STRUCTURE MVKE.
DATA: MSTAV TYPE MSTAV,
      END OF LS_MVKE.
DATA: LT_MVKE LIKE TABLE OF LS_MVKE.
DATA: LS_DATA LIKE LINE OF T_DATA.
DATA: LT_DATA LIKE TABLE OF LS_DATA.
DATA: BEGIN OF LS_CUST_DET,
       VKORG TYPE VKORG,
       VTWEG TYPE VTWEG,
       SPRAS TYPE SPRAS,
       KUNNR TYPE KUNNR,
       NAME1 TYPE NAME1_GP,
      END OF LS_CUST_DET.
DATA: LT_CUST_DET LIKE TABLE OF LS_CUST_DET.
 DATA_SINGLE: DEF_PRES_LANGU  LANGU,   " Language for Mat Description Presentation
              CUST_DET CHAR1.
 LV_DEF_PRES_LANGU = 'E'.              " Default Value 
 CLEAR LV_CUST_DET.                    " do not populate Customers data
 SELECT_SINGLE: DEF_PRES_LANGU,
                CUST_DET.
DATA_MULTY: MATNR        MATNR,
            VKORG        VKORG,
            VTWEG        VTWEG,
            LANGU        LAISO, " LANGU,
            VMSTA        VMSTA,
            MSTAV        MSTAV.
SELECT_MULTY:
            MATNR,
            VKORG,
            VTWEG,
            LANGU,
            VMSTA,
            MSTAV .
CONVERT_MULTY: MATNR MATN1,
               LANGU ISOLA.
DATA:  VKORG_OLD TYPE VKORG,
       VTWEG_OLD TYPE VTWEG,
       SPRAS_OLD TYPE SPRAS.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_MAT_DESC_CUST'
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
 "--- Get sales area existing languages
  SELECT DISTINCT V~VKORG V~VTWEG K~SPRAS
    INTO CORRESPONDING FIELDS OF TABLE LT_CUST_LANGUE
    FROM KNVV AS V
      INNER JOIN KNA1 AS K ON ( V~KUNNR = K~KUNNR )
    WHERE V~VKORG IN R_VKORG
      AND V~VTWEG IN R_VTWEG
      AND K~SPRAS IN R_LANGU
      AND K~SPRAS > ' '.
 "--- Get material sales area
  SELECT *
    FROM MVKE AS M
       INNER JOIN MARA AS R ON ( M~MATNR = R~MATNR )
    INTO CORRESPONDING FIELDS OF TABLE LT_MVKE
    WHERE  M~MATNR IN R_MATNR
       AND M~VKORG IN R_VKORG
       AND M~VTWEG IN R_VTWEG
       AND M~VMSTA IN R_VMSTA
       AND R~MSTAV IN R_MSTAV.
"--- Check existing Customer languages(By sales area) for each Material - Sales area
  LOOP AT LT_MVKE INTO LS_MVKE.
    SY_TABIX = SY-TABIX .
    CLEAR LV_MAT_DESC_CUST.
    LOOP AT LT_CUST_LANGUE INTO LS_CUST_LANGUE
                           WHERE VKORG = LS_MVKE-VKORG
                             AND VTWEG = LS_MVKE-VTWEG.
      CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
         EXPORTING
            MATNR              = LS_MVKE-MATNR
            LANGU               = LS_CUST_LANGUE-SPRAS
         IMPORTING
           MATERIAL_DESC       = LV_MAT_DESC_CUST
         EXCEPTIONS
           WRONG_CODE          = 1
           OTHERS              = 2.
      IF SY-SUBRC <> 0.
        CLEAR LV_MAT_DESC_CUST.
*       Implement suitable error handling here
      ENDIF.
      IF LV_MAT_DESC_CUST IS INITIAL .
        MOVE-CORRESPONDING LS_MVKE TO LS_DATA.
        MOVE-CORRESPONDING LS_CUST_LANGUE TO LS_DATA.
        "- Material desc EN
        CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
          EXPORTING
            MATNR               = LS_MVKE-MATNR
            LANGU               = LV_DEF_PRES_LANGU
         IMPORTING
           MATERIAL_DESC        = LS_DATA-MAT_DESC
         EXCEPTIONS
           WRONG_CODE          = 1
         OTHERS                = 2.
        IF SY-SUBRC <> 0.
          CLEAR LS_DATA-MAT_DESC.
*       Implement suitable error handling here
        ENDIF.
       APPEND LS_DATA TO T_DATA.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
*        refresh lt_cust_det.
  IF LV_CUST_DET IS NOT INITIAL. " Populate customers data
    REFRESH LT_DATA.
    LT_DATA[] = T_DATA[].
    REFRESH T_DATA.
    SORT LT_DATA BY VKORG VTWEG SPRAS.
    LOOP AT LT_DATA INTO LS_DATA.
      IF LS_DATA-VKORG <> VKORG_OLD OR
         LS_DATA-VTWEG <> VTWEG_OLD OR
         LS_DATA-SPRAS <> SPRAS_OLD.
        REFRESH LT_CUST_DET.
        SELECT  V~VKORG V~VTWEG V~KUNNR K~SPRAS K~NAME1
          INTO CORRESPONDING FIELDS OF TABLE LT_CUST_DET
          FROM KNVV AS V
            INNER JOIN KNA1 AS K ON ( V~KUNNR = K~KUNNR )
          WHERE V~VKORG = LS_DATA-VKORG
            AND V~VTWEG = LS_DATA-VTWEG
            AND K~SPRAS = LS_DATA-SPRAS.
        VKORG_OLD = LS_DATA-VKORG.
        VTWEG_OLD = LS_DATA-VTWEG.
        SPRAS_OLD = LS_DATA-SPRAS.
      ELSE.
      ENDIF.
      LOOP AT LT_CUST_DET INTO LS_CUST_DET.
        MOVE-CORRESPONDING LS_CUST_DET TO LS_DATA.
        APPEND LS_DATA TO T_DATA.
      ENDLOOP.
    ENDLOOP.
*        else.
*          clear ls_cust_det.
*          MOVE-CORRESPONDING ls_data to ls_cust_det.
*          append ls_cust_det to lt_cust_det.
*        endif.
*        loop at lt_cust_det into ls_cust_det.
*          MOVE-CORRESPONDING ls_cust_det to ls_data.
*          append ls_data to t_data.
*        endloop.
    "t_data[] = lt_DATA[].
  ENDIF.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
