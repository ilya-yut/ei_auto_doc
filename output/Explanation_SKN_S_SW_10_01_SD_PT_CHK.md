# Exception Indicator: Billing Data Payment Terms check ( SW_10_01_BD_PT_CHK)

## General Overview

This Exception Indicator identifies billing documents where payment terms on the invoice differ from the payment terms maintained for the payer in the customer master sales area, surfacing mismatches that can affect cash collection and order-to-cash control.

This EI serves as an essential control for billing and accounts receivable by:

- Enabling detection of payment terms on billing documents that do not match the payer's customer master terms for the same sales organization and distribution channel
- Supporting identification of data entry errors, outdated customer master data, or unauthorized overrides on billing documents
- Providing visibility into payer, sales organization, division, and document context alongside both payment terms values
- Enabling time-based review of recently created or changed billing documents within a configurable lookback window
- Supporting audit sampling of payment terms consistency before collection and cash application

Typical use includes billing review before invoice release, customer master data quality checks, and periodic AR control samples. Results are intended for exception workflows rather than operational billing list reporting.

The routine reads billing document header data joined to payer customer master and user information, retains rows where billing payment terms differ from master payment terms, and raises an alert when qualifying documents remain.


## Problem Description

Failure to monitor payment terms mismatches on billing documents creates multiple risks across order-to-cash, cash collection, and master-data quality:

**Financial and Collection Risks**

- Billing documents with payment terms that differ from the payer master can produce incorrect due dates and collection timing
- Undetected mismatches can distort cash flow forecasts and DSO analysis when invoices use non-standard terms
- Concentrated exceptions by payer or sales organization can signal systemic billing or master-data issues

**Operational Risks**

- Lookback windows misaligned with billing review cadence can exclude recent invoices or retain stale cases
- Organizational scope that is too broad or too narrow can hide actionable mismatches or overload reviewers
- Lack of visibility by division or document category can mask repeated inconsistencies in specific billing flows

**Control and Audit Risks**

- Weak monitoring reduces evidence that billing payment terms were reviewed against customer master before collection
- Unaddressed mismatches weaken segregation between billing operations and customer master maintenance
- Missing recurring exception review limits accountability for correcting payer master or billing document terms

## Suggested Resolution

**Immediate Response**

- Review flagged billing documents for payer, billing payment terms, and customer master payment terms
- Confirm with billing or AR whether the mismatch is authorized or requires correction
- Prioritize high-value payers or recent invoices for follow-up before payment runs

**System Assessment**

- Validate lookback window and reference-date choice against billing review cadence
- Compare exception counts by sales organization, distribution channel, and payer to find systematic gaps
- Determine whether mismatches correlate with specific document categories or creation users

**Corrective Actions**

- Correct billing document or customer master payment terms through standard SD/FI processes where errors are confirmed
- Update payer master data when repeated exceptions show outdated terms
- Document review outcomes and schedule recurring runs before close or major billing cycles


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 3 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 4 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 5 | BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 6 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 7 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 8 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 9 | KNUMV | Doc. condition no. | CHAR | 10 | 0 | KNUMV | KNUMV |
| 10 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 11 | KUNRG | Payer | CHAR | 10 | 0 | KUNRG | KUNNR |
| 12 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 13 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 14 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 15 | PARVW | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 16 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 17 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 18 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 19 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 20 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 21 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 22 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 23 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 24 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 25 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 26 | ZTERM_KNVV | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
| 27 | ZTERM_VBKD | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
| 28 | ZTERM_VBRK | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 28 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**AUART** (Sales Document Type)

Sales document type controlling order category and therefore the SD process slice included in analysis.

**AUDAT** (Document Date)

Sales document date (order date) used for period-based SD selection.

**BACKDAYS** (Days Back)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERDAT

**BSTNK** (Purchase order no.)

Customer or external PO reference number used for cross-system document matching.

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ERZET** (Time)

Entry time used to refine timestamp windows within a selected day.

**KNUMV** (Doc. condition no.)

Document conditions number tying SD/MM pricing procedure results to header-level condition records.

**KUNNR** (Customer)

Customer account is used to scope records to specific customers across SD/FI flows.

**KUNRG** (Payer)

Payer/customer field used to analyze SD/FI records by billing responsibility.

**NAME1** (Name)

Name of the payer customer from customer master data.

**NAME_TEXT** (Full Name)

<mark>Full name of the user who posted the change.</mark>

**NETWR** (Net value)

Net value amount used for commercial threshold and anomaly checks.

**PARVW** (Partner Function)

SD partner function such as sold-to, ship-to, or payer defining partner roles on documents.

**POSNR** (Sales Document Item)

Document item number used for line-level drilldown and joins.

**SPART** (Division)

Division key used for SD product-line segmentation.

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

**ZTERM_KNVV** (Terms of Payment)

Payment terms from the payer's customer master sales-area record used for comparison.

**ZTERM_VBKD** (Terms of Payment)

Payment terms field from the shared structure; not populated by the billing payment terms check logic.

**ZTERM_VBRK** (Terms of Payment)

Payment terms on the billing document; rows are returned only when this value differs from **ZTERM_KNVV**.


### Parameter Relationships

**Reference-date window:** When no explicit date range is supplied, a range from today minus **BACKDAYS** through today is applied to **ERDAT** or **AEDAT** depending on the configured date reference field in code (default creation date). Explicit **ERDAT** or **AEDAT** selections override that fallback window.

**Payment terms comparison:** The selection returns billing documents only where **ZTERM_VBRK** (payment terms on the billing document) differs from **ZTERM_KNVV** (payment terms on the payer's customer master sales-area record). Both values are returned for review.

**Billing document scope:** **VBELN**, **VKORG**, **VTWEG**, **SPART**, **VBTYP**, and **KUNRG** combine to define which billing documents enter the result set.

**Enrichment fields:** **NAME1** shows the payer name from customer master; **NAME_TEXT** shows the full name of the billing document creator from user master data.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code

### Practical Example of Parameter Configuration

**Use Case 1: Recent billing documents with terms mismatch**

**Purpose:** Review billing documents created in the last seven days where payment terms differ from payer master.

```
BACKDAYS = 7
VKORG = 1000
VTWEG = 10
```

**Use Case 2: Payer-specific review**

**Purpose:** Focus on one payer's billing documents with payment terms mismatches.

```
KUNRG = 100000
BACKDAYS = 30
SPART = 01
```

**Use Case 3: Changed billing documents**

**Purpose:** Sample billing documents changed in the last fourteen days for AR control review.

```
BACKDAYS = 14
AEDAT = 20250101
VKORG = 1000
VBTYP = M
```

**Use Case 4: Division and sales organization scope**

**Purpose:** Monitor payment terms mismatches for one division within a sales organization.

```
SPART = 01
VKORG = 1000
VTWEG = 10
BACKDAYS = 45
```

**Use Case 5: Single billing document check**

**Purpose:** Verify payment terms consistency on one billing document.

```
VBELN = 9000000001
BACKDAYS = 365
VKORG = 1000
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_SD_PT_CHK | AEDAT | Changed on | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_SD_PT_CHK | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_SD_PT_CHK | AUDAT | Document Date | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_SD_PT_CHK | BSTNK | Purchase order no. | CHAR(20) | BSTNK |
| /SKN/S_SW_10_01_SD_PT_CHK | ERDAT | Created on | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_SD_PT_CHK | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_SD_PT_CHK | ERZET | Time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_SD_PT_CHK | KNUMV | Doc. condition no. | CHAR(10) | KNUMV |
| /SKN/S_SW_10_01_SD_PT_CHK | KUNNR | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_SD_PT_CHK | KUNRG | Payer | CHAR(10) | KUNRG |
| /SKN/S_SW_10_01_SD_PT_CHK | NAME1 | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_SD_PT_CHK | NAME_TEXT | Full Name | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_01_SD_PT_CHK | NETWR | Net value | CURR(15) | NETWR_AK |
| /SKN/S_SW_10_01_SD_PT_CHK | PARVW | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_SD_PT_CHK | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_SD_PT_CHK | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_SD_PT_CHK | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_SD_PT_CHK | VBTYP | SD document categ. | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_SD_PT_CHK | VDATU | Requested deliv.date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_SD_PT_CHK | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_SD_PT_CHK | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_SD_PT_CHK | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_SD_PT_CHK | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_SD_PT_CHK | WAERK | Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_SD_PT_CHK | ZTERM_KNVV | Terms of Payment | CHAR(4) | DZTERM |
| /SKN/S_SW_10_01_SD_PT_CHK | ZTERM_VBKD | Terms of Payment | CHAR(4) | DZTERM |
| /SKN/S_SW_10_01_SD_PT_CHK | ZTERM_VBRK | Terms of Payment | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_BD_PT_CHK .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_SD_PT_CHK OPTIONAL
*"----------------------------------------------------------------------
TYPES: BEGIN OF TY_VBKD,
         VBELN TYPE VBKD-VBELN,
         POSNR TYPE VBKD-POSNR,
         ZTERM TYPE VBKD-ZTERM,
       END OF TY_VBKD,
       TT_VBKD TYPE STANDARD TABLE OF TY_VBKD.
  DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
               LANGU          LANGU,
               BACKDAYS       INT4,
               DATE_REF_FLD   NAME_FELD.
** Default values
  LV_BACKDAYS     = 1.
  LV_DATE_REF_FLD = 'ERDAT'.
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD.
  DATA_MULTY: VBELN        VBRK-VBELN,
              VKORG        VBRK-VKORG,
              VTWEG        VBRK-VTWEG,
              SPART        VBRK-SPART,
              VBTYP        VBRK-VBTYP,
              ERDAT        VBRK-ERDAT,
              AEDAT        VBRK-AEDAT,
              KUNRG        VBRK-KUNRG,
              ZTERM_VBRK   VBRK-ZTERM,
              ZTERM_KNVV   KNVV-ZTERM,
              DATUM        SY-DATUM.
  SELECT_MULTY: VBELN,
                VKORG,
                VTWEG,
                SPART,
                VBTYP,
                ERDAT,
                AEDAT,
                KUNRG,
                ZTERM_VBRK,
                ZTERM_KNVV,
                DATUM.
  CONVERT_MULTY: KUNRG ALPHA,
                 VBELN ALPHA.
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM,
         DATE_TO   LIKE SY-DATUM .
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : REF_DATE TYPE D.
  DATA : LV_VBELN TYPE VBELN,
         LV_POSNR TYPE POSNR,
         LV_PARVW TYPE PARVW,
         LV_KUNNR TYPE KUNNR,
         LV_TABIX LIKE SY-TABIX,
         LV_VBTYP TYPE VBTYP.
  DATA: LS_DATA LIKE LINE OF T_DATA,
        LS_VBKD TYPE TY_VBKD.
  DATA: LT_VBKD TYPE TT_VBKD.
  FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF T_DATA.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST
               RFCDEST.
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_BD_PT_CHK'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'BT' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM.
    RS_DATUM-HIGH   = SY-DATUM.
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
*  date_from = sy-datum.
*  READ TABLE r_datum INTO rs_datum INDEX 1.
*  IF sy-subrc IS INITIAL.
*    date_from = rs_datum-low.
*    date_to = rs_datum-high.
*    IF date_to < date_from.
*      date_to = date_from.
*    ENDIF.
*  ENDIF.
  "---
  CASE LV_DATE_REF_FLD.
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[]. "Date on Which Record Was Created
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "Changed On
*    WHEN 'EDATU'.
*      r_edatu = r_datum[].
    WHEN OTHERS.
      R_ERDAT[] = R_DATUM[]. "Document created
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
*  REFRESH: lt_data.
  SELECT VBRK~VBELN VBRK~VKORG VBRK~VTWEG VBRK~SPART VBRK~ERDAT VBRK~ERZET
         VBRK~ERNAM VBRK~VBTYP VBRK~AEDAT VBRK~NETWR VBRK~WAERK
         VBRK~ZTERM AS ZTERM_VBRK
         KNA1~NAME1
         KNVV~ZTERM AS ZTERM_KNVV
         ADRP~NAME_TEXT
    FROM VBRK       INNER JOIN KNA1  ON  VBRK~KUNRG       EQ KNA1~KUNNR
                    INNER JOIN KNVV  ON  VBRK~VKORG       EQ KNVV~VKORG
                                     AND VBRK~VTWEG       EQ KNVV~VTWEG
                                     AND VBRK~KUNRG       EQ KNVV~KUNNR
                                     AND VBRK~ZTERM       NE KNVV~ZTERM
                    INNER JOIN USR21 ON  VBRK~ERNAM       EQ USR21~BNAME
                    INNER JOIN ADRP  ON  USR21~PERSNUMBER EQ ADRP~PERSNUMBER
    INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
    WHERE VBRK~VBELN IN R_VBELN
      AND VBRK~VKORG IN R_VKORG
      AND VBRK~VTWEG IN R_VTWEG
      AND VBRK~SPART IN R_SPART
      AND VBRK~ERDAT IN R_ERDAT
      AND VBRK~AEDAT IN R_AEDAT
      AND VBRK~VBTYP IN R_VBTYP
      AND VBRK~KUNRG IN R_KUNRG.
  CHECK T_DATA[] IS NOT INITIAL.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
