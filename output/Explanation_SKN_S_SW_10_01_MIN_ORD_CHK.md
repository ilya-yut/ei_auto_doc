# Exception Indicator: Minimum order quantity violation ( SW_10_01_MIN_ORD_CHK)

## General Overview

This Exception Indicator identifies sales order items where the cumulative order quantity violates minimum order quantity rules from material sales data, returning order line detail enriched with customer master attributes.

This EI serves as an essential control for sales order compliance by:

- Enabling detection of order lines below the configured minimum order quantity for a material
- Supporting an alternate check for cumulative quantities that are not evenly divisible by the minimum order quantity
- Providing sold-to customer, material, plant, and order context on flagged lines
- Enabling segmentation by sales document, item category, and organizational data for targeted follow-up
- Supporting recurring monitoring of order entry quality before release or billing

Typical use includes minimum order quantity compliance review, order entry validation sampling, and material-specific quantity rule surveillance. Results are intended for exception workflows rather than operational order list reporting.

The routine reads sales order headers and items joined to material sales data and item status, applies minimum-quantity or divisibility rules, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor minimum order quantity violations on sales order lines creates multiple risks across order management, pricing compliance, and customer fulfillment:

**Sales and Commercial Risks**

- Order lines below the material minimum quantity can breach commercial terms or discount agreements
- Cumulative quantities that are not multiples of the minimum order quantity can cause fulfillment or pricing inconsistencies
- Undetected violations by material, plant, or customer can leave order entry quality unmanaged

**Operational Risks**

- Monitoring windows misaligned with order entry cadence can exclude recent violations or retain resolved cases
- Scope that is not tuned to document type, item category, or plant can mix irrelevant lines into the review queue
- Lack of customer and material context on flagged lines slows correction of recurring entry errors

**Control and Audit Risks**

- Weak minimum-quantity monitoring reduces evidence that order lines were reviewed against material sales rules
- Lack of recurring exception review limits accountability for sales operations follow-up on non-compliant quantities
- Missing sold-to and material detail delays escalation of repeated violations on high-volume products

## Suggested Resolution

**Immediate Response**

- Review flagged order lines for material, plant, cumulative quantity, and minimum order quantity
- Confirm with sales operations whether the quantity is correct or requires correction before release
- Prioritize high-volume materials and repeat offenders for immediate follow-up

**System Assessment**

- Validate lookback window and reference-date settings against order review cadence
- Confirm whether below-minimum or divisibility checking matches the intended business rule
- Compare exception counts by material, plant, and sold-to party to identify systematic gaps

**Corrective Actions**

- Correct order quantities through standard SD order maintenance where review confirms action is required
- Adjust monitoring scope after cleanup so results reflect truly exceptional quantity violations
- Document review outcomes and schedule recurring runs before order release or close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ABSTA | Rejection status | CHAR | 1 | 0 | ABSTA_VB | STATV |
| 2 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 3 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 4 | AUMNG | Minimum order | QUAN | 13 | 0 | /SKN/E_SW_MINAU | /SKN/D_SW_MINAU |
| 5 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 6 | BESTA | Confirmed | CHAR | 1 | 0 | BESTA | STATV |
| 7 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 8 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 9 | DIV_ALERT_CHK | Division alert param. |  | 0 | 0 |  |  |
| 10 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 11 | KLMENG | Cumul.confirmed qty | QUAN | 15 | 0 | /SKN/E_SW_KLMENG | /SKN/D_SW_KLMENG |
| 12 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 13 | KWMENG | Order Quantity | QUAN | 15 | 0 | /SKN/E_SW_KWMENG | /SKN/D_SW_KWMENG |
| 14 | LAND1 | Country | CHAR | 3 | 0 | LAND1_GP | LAND1 |
| 15 | MANAGE_IN_UTC | CHAR | 1 | 0 |  | XFELD |  |
| 16 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 17 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 18 | NAME2 | Name 2 | CHAR | 35 | 0 | NAME2_GP | NAME |
| 19 | ORT01 | City | CHAR | 35 | 0 | ORT01_GP | TEXT35 |
| 20 | POSNR | Item (SD) | NUMC | 6 | 0 | POSNR | POSNR |
| 21 | PSTLZ | Postal Code | CHAR | 10 | 0 | PSTLZ | PSTLZ |
| 22 | PSTYV | Item category | CHAR | 4 | 0 | PSTYV | PSTYV |
| 23 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 24 | TELF1 | Telephone 1 | CHAR | 16 | 0 | TELF1 | TEXT16 |
| 25 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 26 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 27 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 28 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 29 | WERKS | Plant | CHAR | 4 | 0 | WERKS_EXT | WERKS |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 29 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ABSTA** (Rejection status)

Overall rejection status on the order item; only items that are not completely rejected are retained by the selection logic.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**AUMNG** (Minimum order)

Minimum order quantity from material sales data for the sales organization and distribution channel; compared against cumulative confirmed quantity on each order line.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BESTA** (Confirmed)

Confirmation status on the order item; only items that are not fully confirmed are retained by the selection logic.

**DATE_REF_FLD** (Date reference field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- AEDAT — Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**DATUM** (DATS)

Explicit monitoring date range supplied by the online monitor; when empty, the lookback window is built from **BACKDAYS** relative to the current day.

**DIV_ALERT_CHK** (Division alert param.)

Switches the minimum-quantity rule between below-minimum checking and divisibility checking on cumulative confirmed quantity.

**DIV_ALERT_CHK Options:**
- **X** — keep lines where cumulative confirmed quantity is not evenly divisible by the material minimum order quantity.
- Empty or blank — keep lines where cumulative confirmed quantity is below the material minimum order quantity.

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**KLMENG** (Cumul.confirmed qty)

Cumulative schedule or order quantity in the sales item context-confirmed or requested quantity accumulated on schedules.

**KUNNR** (Sold-to party)

Customer account is used to scope records to specific customers across SD/FI flows.

**KWMENG** (Order Quantity)

Cumulative order quantity in sales units on the item-commercial ordered quantity for SD lines.

**LAND1** (Country)

Country key used for legal/geographic segmentation of business partners or plants.

**MANAGE_IN_UTC** (CHAR)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**Not in use**
**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**NAME1 - NAME2** (Name)

First and second name lines of the sold-to customer returned from customer master data on each order line.

**ORT01** (City)

Separates cross-client noise from in-scope work when city on ORT01 correlates with client or user attributes.

**POSNR** (Item (SD))

Document item number used for line-level drilldown and joins.

**PSTLZ** (Postal Code)

Supports escalation where postal code on PSTLZ signals ownership for follow-up between Basis and functional teams.

**PSTYV** (Item category)

Sales document item category controlling item behavior, pricing relevance, and delivery rules.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TELF1** (Telephone 1)

First Telephone Number of a customer, vendor, or contact person within their master data record for direct communication and contact management purposes.

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

### Parameter Relationships

**Explicit calendar window versus lookback:** **DATUM** supplies explicit from-and-to calendar bounds for the monitoring pass. When **DATUM** is empty and neither **ERDAT** nor **AEDAT** ranges are supplied, **BACKDAYS** builds the calendar window relative to the evaluation day before orders are read.

**Reference date mapping:** **DATE_REF_FLD** directs the lookback window to the item created-on date or changed-on date.

**Check mode:** When **DIV_ALERT_CHK** is set, the routine keeps lines whose cumulative confirmed quantity is not evenly divisible by the material minimum order quantity. When it is blank, the routine keeps lines whose cumulative confirmed quantity is below the minimum order quantity.

**Order selection:** **VBELN**, **MATNR**, **WERKS**, **VBTYP**, **AUART**, **KUNNR**, **PSTYV**, **ERDAT**, and **AEDAT** narrow which sales order items are read. Only materials with minimum order quantity greater than one and items that are not fully confirmed or rejected are considered.

**Output context:** **AUMNG** comes from material sales data; **KLMENG** is the cumulative confirmed quantity compared against it. Customer fields such as **NAME1**, **NAME2**, **LAND1**, **ORT01**, **PSTLZ**, and **TELF1** are returned from the sold-to master record on each line.

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper and the on-premise path below that call is skipped.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code

### Practical Example of Parameter Configuration

**Use Case 1: Below minimum order quantity**

**Purpose:** Flag order lines where cumulative confirmed quantity is below the material minimum order quantity.

```
MATNR = 10000001
WERKS = 1000
BACKDAYS = 7
ERDAT = 20250101 - 20251231
```

**Use Case 2: Divisibility violation**

**Purpose:** Detect order lines whose cumulative quantity is not an even multiple of the minimum order quantity.

```
DIV_ALERT_CHK = X
VKORG = 1000
VTWEG = 10
BACKDAYS = 14
```

**Use Case 3: Specific sales order review**

**Purpose:** Review one sales document for minimum quantity compliance on all qualifying items.

```
VBELN = 10000001
BACKDAYS = 30
DATE_REF_FLD = ERDAT
```

**Use Case 4: Item category scope**

**Purpose:** Monitor standard order items for a sold-to party within the default lookback window.

```
PSTYV = TAN
KUNNR = 100000
AUART = TA
BACKDAYS = 1
```

**Use Case 5: Plant and material combination**

**Purpose:** Sample recent order lines for one material at one plant using changed-on date filtering.

```
MATNR = 20000002
WERKS = 2000
DATE_REF_FLD = AEDAT
AEDAT = 20250101 - 20251231
BACKDAYS = 7
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_MIN_ORD_CHK | ABSTA | Rejection status for SD item | CHAR(1) | ABSTA_VB |
| /SKN/S_SW_10_01_MIN_ORD_CHK | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_MIN_ORD_CHK | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_MIN_ORD_CHK | AUMNG | Minimum order quantity in base unit of measure | QUAN(13) | /SKN/E_SW_MINAU |
| /SKN/S_SW_10_01_MIN_ORD_CHK | BESTA | Confirmation Status of Document Item | CHAR(1) | BESTA |
| /SKN/S_SW_10_01_MIN_ORD_CHK | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_MIN_ORD_CHK | KLMENG | Cumulative confirmed quantity in base unit | QUAN(15) | /SKN/E_SW_KLMENG |
| /SKN/S_SW_10_01_MIN_ORD_CHK | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_MIN_ORD_CHK | KWMENG | Cumulative Order Quantity in Sales Units | QUAN(15) | /SKN/E_SW_KWMENG |
| /SKN/S_SW_10_01_MIN_ORD_CHK | LAND1 | Country Key | CHAR(3) | LAND1_GP |
| /SKN/S_SW_10_01_MIN_ORD_CHK | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_01_MIN_ORD_CHK | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_MIN_ORD_CHK | NAME2 | Name 2 | CHAR(35) | NAME2_GP |
| /SKN/S_SW_10_01_MIN_ORD_CHK | ORT01 | City | CHAR(35) | ORT01_GP |
| /SKN/S_SW_10_01_MIN_ORD_CHK | POSNR | Item number of the SD document | NUMC(6) | POSNR |
| /SKN/S_SW_10_01_MIN_ORD_CHK | PSTLZ | Postal Code | CHAR(10) | PSTLZ |
| /SKN/S_SW_10_01_MIN_ORD_CHK | PSTYV | Sales document item category | CHAR(4) | PSTYV |
| /SKN/S_SW_10_01_MIN_ORD_CHK | TELF1 | First telephone number | CHAR(16) | TELF1 |
| /SKN/S_SW_10_01_MIN_ORD_CHK | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_MIN_ORD_CHK | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_MIN_ORD_CHK | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_MIN_ORD_CHK | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_MIN_ORD_CHK | WERKS | Plant (Own or External) | CHAR(4) | WERKS_EXT |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_MIN_ORD_CHK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_MIN_ORD_CHK OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: SW_DEST       RFCDEST,
               BACKDAYS      INT4,
               MANAGE_IN_UTC CHAR1,
               DATE_REF_FLD  NAME_FELD,
               DIV_ALERT_CHK FLAG.
  LV_BACKDAYS     = 1.
  LV_DATE_REF_FLD = 'ERDAT'. " Created on
  SELECT_SINGLE: SW_DEST,
                 BACKDAYS,
                 MANAGE_IN_UTC,
                 DATE_REF_FLD,
                 DIV_ALERT_CHK.
  DATA_MULTY: MATNR  MATNR,
              VBELN  VBELN_VA,
              WERKS  WERKS_EXT,
              VBTYP  VBTYP,
              AUART  AUART,
              KUNNR  KUNAG,
              PSTYV  PSTYV,
              ERDAT  ERDAT_RF,
              AEDAT  AEDAT,
              DATUM  SY-DATUM.
  SELECT_MULTY: MATNR,
                VBELN,
                WERKS,
                ERDAT,
                AEDAT,
                DATUM.
  CONVERT_MULTY:  AUART AUART . """Tanya 14/11/18
*  convert_multy: lifnr alpha.
  DATA: LV_MOD   TYPE I,
        LV_TABIX TYPE SYTABIX.
  DATA: LS_DATA  LIKE LINE OF T_DATA[].
  DATA: BACKDAYS  TYPE I,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM,
        REF_DATE  TYPE D.
  FIELD-SYMBOLS: <FS_DATA> TYPE /SKN/S_SW_10_01_MIN_ORD_CHK.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    DATA: LV_IS_HANA(1) TYPE C.
    CALL FUNCTION '/SKN/F_SW_IS_RFCDEST_HANA'
      EXPORTING
        DEST          = LV_SW_DEST
      IMPORTING
        IS_HANA       =  LV_IS_HANA.
   IF LV_IS_HANA IS NOT INITIAL.
    CALL FUNCTION '/SKN/FH_SW_10_01_MIN_ORD_CHK'
      EXPORTING
        DIV_ALERT_CHK = LV_DIV_ALERT_CHK
      IMPORTING
        IS_ALERT      = IS_ALERT
      TABLES
        T_SELECT      = T_SELECT
        T_DATA        = T_DATA.
   ELSE.
    CALL FUNCTION '/SKN/FC_SW_10_01_MIN_ORD_CHK'
      EXPORTING
        DIV_ALERT_CHK = LV_DIV_ALERT_CHK
      IMPORTING
        IS_ALERT      = IS_ALERT
      TABLES
        T_SELECT      = T_SELECT
        T_DATA        = T_DATA.
   ENDIF.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
  IF R_ERDAT[] IS INITIAL AND R_AEDAT[] IS INITIAL.
    IF R_DATUM[] IS INITIAL .
      RS_DATUM-SIGN   = 'I' .
      RS_DATUM-OPTION = 'BT' .
      DATE_FROM       = SY-DATUM - LV_BACKDAYS .
      RS_DATUM-LOW    = DATE_FROM .
      RS_DATUM-HIGH   = SY-DATUM.
      APPEND RS_DATUM TO R_DATUM.
    ENDIF.
    "--- Set Reference Date Field
    CASE LV_DATE_REF_FLD.
      WHEN 'ERDAT'.
        R_ERDAT[] = R_DATUM[]. "Document created
      WHEN 'AEDAT'.
        R_AEDAT[] = R_DATUM[]. "changed on
    ENDCASE.
  ENDIF.
*--- Retrieve data
  CLEAR: IS_ALERT. "lt_vbak, lt_mvke.
  REFRESH T_DATA.
  IF LV_DIV_ALERT_CHK EQ 'X'.
    SELECT VBAK~VBELN VBAK~VBTYP VBAK~AUART VBAK~KUNNR
           VBAK~ERDAT VBAK~VKORG VBAK~VTWEG
           VBAP~MATNR VBAP~WERKS VBAP~KWMENG VBAP~KLMENG VBAP~PSTYV VBAP~AEDAT
           VBUP~BESTA VBUP~ABSTA
           MVKE~AUMNG
           KNA1~LAND1 KNA1~NAME1 KNA1~NAME2 KNA1~ORT01 KNA1~PSTLZ
           KNA1~TELF1
      FROM VBAK
      INNER JOIN VBAP ON VBAK~VBELN EQ VBAP~VBELN
      INNER JOIN VBUP ON VBAP~VBELN EQ VBUP~VBELN
                     AND VBAP~POSNR EQ VBUP~POSNR
      INNER JOIN MVKE ON VBAP~MATNR EQ MVKE~MATNR
      INNER JOIN KNA1 ON VBAK~KUNNR EQ KNA1~KUNNR
      INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
      WHERE VBAK~VKORG  EQ MVKE~VKORG
      AND   VBAK~VTWEG  EQ MVKE~VTWEG
      AND   VBAK~VBELN  IN R_VBELN
      AND   VBAK~VBTYP  IN R_VBTYP
      AND   VBAK~AUART  IN R_AUART
      AND   VBAK~KUNNR  IN R_KUNNR
      AND   VBAP~MATNR  IN R_MATNR
      AND   VBAP~PSTYV  IN R_PSTYV
      AND   VBAP~WERKS  IN R_WERKS
      AND   VBAP~ERDAT  IN R_ERDAT
      AND   VBAP~AEDAT  IN R_AEDAT
      AND   VBUP~BESTA  NE 'A'
      AND   VBUP~ABSTA  NE 'C'
      AND   MVKE~AUMNG  >  1.
** Check if Cumulative orders quan.is divisible by min. order quan.
** by no remainder
    LOOP AT T_DATA ASSIGNING <FS_DATA>.
      LV_TABIX = SY-TABIX.
      IF <FS_DATA>-KLMENG IS NOT INITIAL AND
            <FS_DATA>-AUMNG IS NOT INITIAL.
        CLEAR LV_MOD.
        LV_MOD = <FS_DATA>-KLMENG MOD <FS_DATA>-AUMNG.
        IF LV_MOD = 0.
          DELETE T_DATA INDEX LV_TABIX.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    SELECT VBAK~VBELN VBAK~VBTYP VBAK~AUART VBAK~KUNNR
           VBAK~ERDAT VBAK~VKORG VBAK~VTWEG
           VBAP~MATNR VBAP~WERKS VBAP~KWMENG VBAP~KLMENG VBAP~PSTYV VBAP~AEDAT
           VBUP~BESTA VBUP~ABSTA
           MVKE~AUMNG
           KNA1~LAND1 KNA1~NAME1 KNA1~NAME2 KNA1~ORT01 KNA1~PSTLZ
           KNA1~TELF1
      FROM VBAK
      INNER JOIN VBAP ON VBAK~VBELN EQ VBAP~VBELN
      INNER JOIN VBUP ON VBAP~VBELN EQ VBUP~VBELN
                     AND VBAP~POSNR EQ VBUP~POSNR
      INNER JOIN MVKE ON VBAP~MATNR EQ MVKE~MATNR
      INNER JOIN KNA1 ON VBAK~KUNNR EQ KNA1~KUNNR
      INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
      WHERE VBAK~VKORG  EQ MVKE~VKORG
      AND   VBAK~VTWEG  EQ MVKE~VTWEG
      AND   VBAK~VBELN  IN R_VBELN
      AND   VBAP~MATNR  IN R_MATNR
      AND   VBAP~WERKS  IN R_WERKS
      AND   VBAP~ERDAT  IN R_ERDAT
      AND   ( VBAP~ERDAT IN R_ERDAT OR VBAP~AEDAT IN R_ERDAT )
      AND   VBUP~BESTA  NE 'A'
      AND   VBUP~ABSTA  NE 'C'
      AND   MVKE~AUMNG  >  1
      AND   VBAP~KLMENG <  MVKE~AUMNG.
  ENDIF.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
