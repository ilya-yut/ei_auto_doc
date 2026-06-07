# Exception Indicator: SO: Sales document details change log ( SW_10_06_SO_CHNG_LOG)

## General Overview

This Exception Indicator monitors sales order change-log activity by combining configuration change-document results with sales order header and item data, then enriching alerts with customer and organizational context.

This EI serves as an essential control for SD and audit teams by:

- Surfacing field-level changes recorded for sales-related configuration objects before they are tied to live order data
- Enabling review of which sales documents, materials, and sold-to parties are affected by each change line
- Supporting accountability through user and object filters on the underlying change log
- Helping detect amendments that may affect pricing, rejection reasons, or organizational assignment on order items
- Providing a consolidated exception list for follow-up instead of manual change-document navigation

Typical use includes audit sampling after master data or pricing adjustments, investigation of user activity on sensitive objects, and periodic monitoring of sales document change patterns by organization.

The routine first invokes the shared master-data change-log function, then retains only results where configuration changes were found, enriches them with sales order item and header attributes, and adds sold-to party descriptions where available.


## Problem Description

Unmonitored sales order change activity creates risks for revenue recognition, pricing integrity, and segregation of duties in SD processes.

**Financial and Reporting Risks**

- Price, quantity, or rejection-reason changes on order items may distort margin or delivery commitments if discovered late
- Changes concentrated in closing periods can affect period-end revenue or backlog reporting
- Lack of structured change review weakens evidence for audit and management sign-off

**Operational and Control Risks**

- Unauthorized or unexplained amendments may bypass approval paths when change documents are not monitored
- Teams cannot easily see which documents and materials are touched by a given configuration change
- Object-level filters without sales context leave investigators to reconcile logs manually

**Management Visibility Risks**

- Leadership lacks a repeatable view of sales change patterns by organization, user, or document type
- Cross-functional follow-up between master data, SD operations, and audit is slower without a scoped exception queue

## Suggested Resolution

**Immediate Response**

- Review each alert for object identifier, user, changed field values, and linked sales document attributes
- Validate whether the change is authorized for the affected order type and sales organization
- Escalate high-risk combinations (for example pricing-related fields on high-value documents) to process owners

**System Assessment**

- Confirm **OBJECTID** and **USERNAME** scope matches the change objects and users you intend to govern
- Align **BACKDAYS** and change-log date behavior with the underlying configuration monitor settings
- Check that sales filters (**VBELN**, **AUART**, **VKORG**, **MATNR**, **KUNNR**) reflect the population under review

**Corrective Actions**

- Tighten monitoring parameters after root-cause analysis to keep the queue actionable
- Document review outcomes and recurring change patterns for audit trail
- Coordinate with master data and SD owners on preventive controls for frequent change types


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ABGRU | Reason for rejection | CHAR | 2 | 0 | ABGRU_VA | ABGRU_VA |
| 2 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 3 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 4 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 5 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 6 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 7 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 8 | SW_DEST | RFC Destination |  |  |  |  |  |
| 9 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 10 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 11 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 12 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 13 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 14 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 15 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 15 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**ABGRU** (Reason for rejection)

Reason for rejection on the sales order item; limits which enriched change lines remain when populated.

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.


**GSBER** (Business Area)

Business area key used for FI organizational reporting segmentation.

**KUNNR** (Sold-to party)

Customer account and is used to scope records to specific customers across SD/FI flows.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**OBJECTID** (Object value)

Change-log object identifier passed to the configuration change-log step; scopes which change entries are retrieved before sales-order enrichment.

**SW_DEST** (RFC Destination)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**USERNAME** (User)

<mark>User who posted the change.</mark>

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.

**VBTYP** (SD document categ.)

SD document category used to segment SD document classes.

**VKBUR** (Sales Office)

Sales office key used for organizational SD segmentation.

**VKGRP** (Sales Group)

Sales group key used for team-level SD analytics.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.


### Parameter Relationships

**Change-log phase:** **OBJECTID** and **USERNAME** (with **BACKDAYS** and other change-log parameters supplied through the shared configuration function) define which change-document lines are retrieved before any sales-order enrichment runs.

**Sales-order enrichment:** After configuration changes are confirmed, **VBELN**, **VBTYP**, **AUART**, **VKORG**, **VTWEG**, **VKGRP**, **VKBUR**, **GSBER**, **MATNR**, **KUNNR**, and **ABGRU** narrow the enriched result set to the intended SD scope.

**Customer description:** **KUNNR** drives a description lookup on populated sold-to party values in the result.

**Execution path:** **SW_DEST** selects cloud versus on-premise execution; the configuration change-log call and on-premise enrichment run only when the cloud path is not selected.


### Default Values

- **BACKDAYS** - initial - treated as unconstrained by code
- **SW_DEST** - initial - treated as on-premise execution by code

### Practical Example of Parameter Configuration

**Use Case 1: Recent configuration changes by user**

**Purpose:** Monitor change-log entries for a specific object type posted by selected users in the last 30 days.

```
OBJECTID = CONFIG_OBJECT_EXAMPLE
USERNAME = SMITHJ
BACKDAYS = 30
```

**Use Case 2: Sales organization and order type scope**

**Purpose:** Limit enriched results to standard orders in one sales organization.

```
VKORG = 1000
AUART = TA
VTWEG = 10
BACKDAYS = 14
```

**Use Case 3: Material and document focus**

**Purpose:** Review changes linked to specific materials and sales documents.

```
VBELN = 0000012345
MATNR = 100-200-300
KUNNR = 0000100001
BACKDAYS = 90
```

**Use Case 4: Rejection-reason changes**

**Purpose:** Highlight change lines for items with selected rejection reasons after enrichment.

```
ABGRU = 10
VKORG = 2000
VBTYP = C
BACKDAYS = 7
USERNAME = JONESA
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_06_SO_CHNG_LOG | ABGRU | Reason for rejection | CHAR(2) | ABGRU_VA |
| /SKN/S_SW_10_06_SO_CHNG_LOG | ACT_CHNGNO | Document number | CHAR(10) | CD_CHNGNO |
| /SKN/S_SW_10_06_SO_CHNG_LOG | ARKTX | Description | CHAR(40) | ARKTX |
| /SKN/S_SW_10_06_SO_CHNG_LOG | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_06_SO_CHNG_LOG | AUFNR | Order | CHAR(12) | AUFNR |
| /SKN/S_SW_10_06_SO_CHNG_LOG | BSTDK | Purchase order date | DATS(8) | BSTDK |
| /SKN/S_SW_10_06_SO_CHNG_LOG | BSTNK | Purchase order no. | CHAR(20) | BSTNK |
| /SKN/S_SW_10_06_SO_CHNG_LOG | CHANGENR | Document number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_06_SO_CHNG_LOG | CHANGE_IND | Appl. object change | CHAR(1) | CDCHNGINDH |
| /SKN/S_SW_10_06_SO_CHNG_LOG | CHANGE_IND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | CHARG | Batch | CHAR(10) | CHARG_D |
| /SKN/S_SW_10_06_SO_CHNG_LOG | CHNGIND | Change Indicator | CHAR(1) | CDCHNGIND |
| /SKN/S_SW_10_06_SO_CHNG_LOG | CHNGIND_DESC | Domain name | CHAR(30) | DOMNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | CUKY_NEW | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_SO_CHNG_LOG | CUKY_OLD | CUKY | CUKY(5) | CDCUKY |
| /SKN/S_SW_10_06_SO_CHNG_LOG | EDATU | Delivery Date | DATS(8) | EDATU |
| /SKN/S_SW_10_06_SO_CHNG_LOG | ETENR | Schedule Line Number | NUMC(4) | ETENR |
| /SKN/S_SW_10_06_SO_CHNG_LOG | FIELD_DESC | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | FNAME | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY1 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY10 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY10_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY10_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY1_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY1_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY2 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY2_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY2_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY3 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY3_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY3_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY4 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY4_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY4_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY5 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY5_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY5_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY6 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY6_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY6_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY7 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY7_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY7_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY8 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY8_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY8_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY9 | Field Name | CHAR(30) | FIELDNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY9_DS | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KEY9_V | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KUNNR | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_06_SO_CHNG_LOG | KWMENG | Order Quantity | QUAN(15) | /SKN/E_SW_KWMENG |
| /SKN/S_SW_10_06_SO_CHNG_LOG | LMENG | Required quantity | QUAN(13) | LMENG |
| /SKN/S_SW_10_06_SO_CHNG_LOG | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_06_SO_CHNG_LOG | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_06_SO_CHNG_LOG | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_SO_CHNG_LOG | NAME_FIRST | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_06_SO_CHNG_LOG | NAME_LAST | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_06_SO_CHNG_LOG | NAME_TEXT | Full Name | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | NETWR | Net value | CURR(15) | NETWR_AP |
| /SKN/S_SW_10_06_SO_CHNG_LOG | OBJECTCLAS | Change doc. object | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_06_SO_CHNG_LOG | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_06_SO_CHNG_LOG | OBJECT_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_SO_CHNG_LOG | PLANCHNGNR | Change number | CHAR(12) | PLANCHNGNR |
| /SKN/S_SW_10_06_SO_CHNG_LOG | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_06_SO_CHNG_LOG | PSMNG | char13 | CHAR(13) | CHAR13 |
| /SKN/S_SW_10_06_SO_CHNG_LOG | PSTYV | Item category | CHAR(4) | PSTYV |
| /SKN/S_SW_10_06_SO_CHNG_LOG | REPETITIVE | Repetitive Change | CHAR(1) | /SKN/E_REPEAT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | TABKEY | Table Key | CHAR(70) | CDTABKEY |
| /SKN/S_SW_10_06_SO_CHNG_LOG | TABNAME | Table Name | CHAR(30) | TABNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | TAB_DESC | Short Description | CHAR(60) | AS4TEXT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | TCODE | Transaction Code | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_06_SO_CHNG_LOG | TEXT_CASE | Text flag | CHAR(1) | CDXFELD |
| /SKN/S_SW_10_06_SO_CHNG_LOG | UDATE | Date | DATS(8) | CDDATUM |
| /SKN/S_SW_10_06_SO_CHNG_LOG | UNIT_NEW | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | UNIT_OLD | Unit | UNIT(3) | CDUNIT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | USERNAME | User | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | UTIME | Time | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VALUE_NEW | New value | CHAR(254) | CDFLDVALN |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VALUE_OLD | Old value | CHAR(254) | CDFLDVALO |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VBTYP | SD document categ. | CHAR(1) | VBTYP |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VRKME | Sales unit | UNIT(3) | VRKME |
| /SKN/S_SW_10_06_SO_CHNG_LOG | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_06_SO_CHNG_LOG | WAERK | Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_06_SO_CHNG_LOG | WAS_PLANND | gen from plan. changes | CHAR(1) | CD_PLANNED |
| /SKN/S_SW_10_06_SO_CHNG_LOG | WEMNG | char13 | CHAR(13) | CHAR13 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_SO_CHNG_LOG .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_SO_CHNG_LOG
*"----------------------------------------------------------------------
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: BACKDAYS         INT4,
               SW_DEST          RFCDEST
               .
  DATA_MULTY: OBJECTID  IB_RECNO,
              VBELN     VBELN_VA,
              VBTYP     VBTYP,
              AUART     AUART,
              VKORG     VKORG,
              VTWEG     VTWEG,
              VKGRP     VKGRP,
              VKBUR     VKBUR,
              GSBER     GSBER,
              MATNR     MATNR,
              KUNNR     KUNAG,
              ABGRU     ABGRU_VA,
              USERNAME  CDUSERNAME.
  DATA: LV_FIELDNAME TYPE FIELDNAME,
        LV_SHIFT     TYPE DDLENG,
        LV_LENG      TYPE DDLENG.
  DATA: LV_TABKEY_LEN TYPE I VALUE '70',    "!!!
        LV_ILEN       TYPE I.
  DATA: LS_DATA LIKE LINE OF T_DATA[].
  FIELD-SYMBOLS: <FS_OLD> TYPE ANY,
                 <FS_NEW> TYPE ANY.
  DATA : FLD TYPE FIELDNAME,
         IFLD TYPE I,
         CTMP(2) TYPE C.
  DATA: LT_DATA TYPE TABLE OF /SKN/S_SW_10_06_MD_CHNG_LOG.
  DATA: LS_DATA_MD LIKE LINE OF LT_DATA.
  FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF T_DATA[].
  SELECT_SINGLE: BACKDAYS,
                 SW_DEST.
  SELECT_MULTY: OBJECTID,
                USERNAME.
  CONVERT_MULTY:  AUART AUART . """Tanya 14/11/18
* Configuration Alert
  CALL FUNCTION '/SKN/F_SW_10_06_MD_CHNG_LOG'
    IMPORTING
      IS_ALERT = IS_ALERT
    TABLES
      T_SELECT = T_SELECT
      T_DATA   = LT_DATA.
* Check if found some change in configuration log
  CHECK IS_ALERT EQ 'X'.
  SELECT_MULTY: VBELN,
                VBTYP,
                AUART,
                VKORG,
                VTWEG,
                VKGRP,
                VKBUR,
                GSBER,
                MATNR,
                KUNNR,
                ABGRU.
* Move data change's log to main tab.
  LOOP AT LT_DATA INTO LS_DATA_MD.
    MOVE-CORRESPONDING LS_DATA_MD TO LS_DATA.
    APPEND LS_DATA TO T_DATA[].
  ENDLOOP.
* if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_SO_CHNG_LOG'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    FROM IBIN INNER JOIN VBAP ON  IBIN~INSTANCE EQ VBAP~CUOBJ
              INNER JOIN VBAK ON  VBAP~VBELN    EQ VBAK~VBELN
              INNER JOIN VBEP ON  VBAP~VBELN    EQ VBEP~VBELN
                              AND VBAP~POSNR    EQ VBEP~POSNR
              LEFT OUTER JOIN AFPO ON  VBEP~VBELN EQ AFPO~KDAUF
                                   AND VBEP~POSNR EQ AFPO~KDPOS
                                   AND VBEP~ETENR EQ AFPO~KDEIN
    FOR ALL ENTRIES IN T_DATA
    WHERE IN_RECNO   EQ T_DATA-OBJECTID+0(22)
    AND   VBAP~VBELN IN R_VBELN
    AND   VBAP~MATNR IN R_MATNR
    AND   VBAP~ABGRU IN R_ABGRU
    AND   VBAK~VBTYP IN R_VBTYP
    AND   VBAK~AUART IN R_AUART
    AND   VBAK~VKORG IN R_VKORG
    AND   VBAK~VTWEG IN R_VTWEG
    AND   VBAK~VKGRP IN R_VKGRP
    AND   VBAK~VKBUR IN R_VKBUR
    AND   VBAK~GSBER IN R_GSBER
    AND   VBAK~KUNNR IN R_KUNNR.
* Customer Description Name
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    IF <FS_DATA>-KUNNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
        EXPORTING
          KUNNR          = <FS_DATA>-KUNNR
        IMPORTING
          CUST_DESC      = <FS_DATA>-NAME1
        EXCEPTIONS
          WRONG_CUSTOMER = 1
          OTHERS         = 2.
    ENDIF.
  ENDLOOP.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  READ TABLE T_DATA INTO LS_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
