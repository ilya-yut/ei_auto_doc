# Exception Indicator: Sales Data Payment Terms check - SW_10_01_SD_PT_CHK

## General Overview

This Exception Indicator (EI) monitors sales orders where the payment terms on the sales document differ from the payment terms in the customer master data. It compares the document-level payment terms (from the sales document) with the customer-master payment terms (from the customer–sales-area combination) and flags mismatches that may indicate data quality issues, incorrect order entry, or customer-master inconsistencies requiring correction.

This EI serves as an essential control for sales operations and order-to-cash integrity by:
- Enabling detection of payment terms mismatches between sales documents and customer master data that can affect cash flow and collection timing
- Supporting identification of data quality issues in sales order entry or customer master maintenance
- Providing visibility into payment terms consistency by sales organization, distribution channel, division, and customer for accountability and process improvement
- Enabling analysis by document type, partner function, and time period for root-cause and trend analysis
- Supporting audit readiness by surfacing exceptions that require business justification or master data correction

The EI is valuable for order-to-cash monitoring, customer master data quality reviews, and sales operations control. It helps ensure that payment terms on sales documents align with the customer master so that invoicing, collection, and cash application proceed correctly.


## Problem Description

Failure to monitor payment terms mismatches between sales documents and customer master data creates multiple risks across order-to-cash, financial reporting, and operational control:

**Financial and Reporting Issues**
- Undetected payment terms mismatches can distort cash flow forecasts and collection timing when invoices are sent with terms that differ from the customer master
- Inconsistent payment terms may lead to incorrect due-date calculations and late-payment penalties or early-payment discounts applied incorrectly
- Unreported mismatches can cause reconciliation gaps between expected and actual payment dates during month-end close
- Concentrated exceptions in specific sales organizations or customer segments can signal systemic data quality or process failures requiring management intervention

**Sales Operations and Control Risks**
- Payment terms mismatches without visibility may indicate unauthorized overrides, data entry errors, or inadequate master data governance
- Lack of monitoring by document type or partner function can mask repeated inconsistencies by specific order types or customer roles
- Exceptions by sales organization, distribution channel, or division may reveal delegation or training gaps in order entry
- Unchecked mismatches can undermine pricing and payment policies and create audit findings
- High volumes of exceptions could indicate integration or master data synchronization failures requiring immediate correction

**Management Visibility and Decision-Making Risks**
- Absence of monitoring delays executive awareness of data quality and order-entry control weaknesses
- Unidentified payment terms patterns can lead to missed opportunities for process improvement or customer master cleanup
- Exceptions may require additional audit scrutiny or compliance review but go unnoticed without the EI
- Lack of visibility by organizational dimension limits ability to assign accountability and remediate root causes

## Suggested Resolution

**Immediate Response**
- Review the flagged sales orders to confirm that document payment terms differ from customer master payment terms and understand the business context
- Verify high-value or high-volume exceptions using transaction VA03 (Display Sales Order) and VD03 (Display Customer) to confirm terms and legitimacy
- Check whether the mismatch stems from a legitimate override, data entry error, or outdated customer master
- Identify whether exceptions correlate with specific document types, sales organizations, or customer segments

**System Assessment**
- Analyze the reference date used (e.g. creation date, document date, requested delivery date) and the lookback window to ensure the monitoring scope is appropriate
- Compare current exception counts and patterns to prior periods to identify trends or one-time spikes
- Examine distribution by sales organization, distribution channel, division, and customer to pinpoint concentration or process issues
- Assess partner function (e.g. sold-to) distribution to determine if exceptions correlate with specific roles or configurations
- Validate that the date range and organizational filters align with the intended control objective

**Corrective Actions**
- Where mismatches are erroneous, correct the sales document (VA02) or update the customer master (VD02) to align payment terms
- For legitimate overrides, document business justification and consider process or policy updates to reduce future exceptions
- Update customer master data if exceptions point to outdated or incorrect payment terms in the customer–sales-area combination
- Adjust monitoring parameters (e.g. lookback days, reference date field, organizational scope) to focus on material exceptions and reduce noise
- Establish recurring EI execution and alert routing to sales and finance stakeholders for continuous control monitoring


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 3 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 4 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 5 | BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 6 | DATE_REF_FIELD | Date reference field |  | 0 | 0 |  |  |
| 7 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 8 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 9 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 10 | KNUMV | Doc. condition no. | CHAR | 10 | 0 | KNUMV | KNUMV |
| 11 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 12 | KUNRG | Payer | CHAR | 10 | 0 | KUNRG | KUNNR |
| 13 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 14 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 15 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 16 | PARVW | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 17 | POSNR | Sales Document Item | NUMC | 6 | 0 | POSNR_VA | POSNR |
| 18 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 19 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 20 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 21 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 22 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 23 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 24 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 25 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 26 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 27 | ZTERM_KNVV | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
| 28 | ZTERM_VBKD | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
| 29 | ZTERM_VBRK | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 29 parameters listed in the Parameters Reference Table above.

**AEDAT** (Changed On):

Date when the sales document was last changed. The EI uses it as the reference date for the monitoring window when DATE_REF_FIELD is set to AEDAT; the chosen date field is restricted to the lookback window when reading sales order data.

**AUART** (Sales Document Type):

Sales document type (e.g. OR, RE). The EI restricts which order types are read; each type can have different payment terms handling and business semantics.

**AUDAT** (Document Date):

Document date (date received/sent) on the sales order. Can be used as the reference date for the monitoring window when DATE_REF_FIELD is set to AUDAT; the EI then restricts this field to the lookback window.

**BACKDAYS** (Days Back):

Number of days used to build the monitoring window. When no date range is supplied, the EI uses today minus this value as the start date and today as the end date; the chosen reference date field (ERDAT, AEDAT, AUDAT, or VDATU) is restricted to that window when reading sales order data.

**BSTNK** (Purchase order no.):

Customer purchase order number on the sales document. The EI includes it in selection so configurations can focus on specific customer PO references.

**DATE_REF_FIELD** (Date reference field):

Selects which date field on the sales order is used for the monitoring window: creation date (ERDAT), changed-on date (AEDAT), document date (AUDAT), or requested delivery date (VDATU). The EI applies the lookback window to the chosen field when reading data.

**DATE_REF_FIELD Options:**
- **ERDAT**: Date on which record was created; default in the code.
- **AEDAT**: Changed on.
- **AUDAT**: Document date (date received/sent).
- **VDATU**: Requested delivery date.

**BACKDAYS and DATE_REF_FIELD Connection:** BACKDAYS defines the lookback length; DATE_REF_FIELD defines which date field is restricted to that window. Set both when configuring the monitoring window (e.g. last 7 days by creation date).

**ERDAT** (Created on):

Date when the sales document was created. The EI uses it as the default reference date for the monitoring window when DATE_REF_FIELD is not supplied; when set to ERDAT, the EI restricts creation date to the lookback window.

**ERNAM** (Created By):

User who created the sales document. The EI includes it in selection and result for creator-based scope or accountability.

**ERZET** (Time):

Entry time when the sales document was created. The EI includes it in the result for timestamp context.

**KNUMV** (Doc. condition no.):

Document condition number (pricing/conditions). The EI includes it in the result for condition-related context when available.

**KUNNR** (Customer):

Customer number (sold-to or partner per PARVW). The EI joins sales document to customer master (KNA1, KNVV) and uses this to scope which customers are evaluated and to compare payment terms between document and master.

**KUNRG** (Payer):

Payer customer number. The EI can include it in the result for payer-based context when available in the structure.

**NAME1** (Name):

Customer name from master data (KNA1).

**NAME_TEXT** (Full Name):

Full name of the person who created the document (from user master), for display.

**NETWR** (Net value):

Net value of the sales order in document currency; used for value-based analysis in the EI.

**PARVW** (Partner Function):

Partner function that determines which partner (e.g. sold-to) is used for the customer–sales-area lookup. The EI defaults to 'RG' (sold-to) when not supplied; it uses this to join VBPA and KNVV for payment terms comparison.

**PARVW Options:**
- **RG**: Sold-to party (default when not supplied).
- **RE**: Bill-to party; **WE**: Ship-to party; **AG**: Payer. Other partner function values as in standard SAP; use values relevant to payment terms lookup.

**POSNR** (Sales Document Item):

Sales document item number. The EI works at header and item level for the VBKD join; this identifies the item.

**SPART** (Division):

Division. The EI restricts and reports by division for organizational scope and for the customer–sales-area (KNVV) lookup.

**VBELN** (Sales Document):

Sales document number. The EI reads order header and item data keyed by this identifier; values scope which orders are evaluated for payment terms mismatch.

**VBTYP** (SD document categ.):

SD document category (e.g. order, contract). The EI includes it in selection and result for scope by category.

**VBTYP Options:**
- **C**: Order; **A**: Contract; **B**: Scheduling agreement. Other domain values as in standard SAP; use values relevant to sales documents.

**VDATU** (Requested deliv.date):

Requested delivery date. Can be used as the reference date for the monitoring window when DATE_REF_FIELD is set to VDATU; the EI then restricts this field to the lookback window.

**VKBUR** (Sales Office):

Sales office. The EI can include it in selection or result for organizational scope when available in the structure.

**VKGRP** (Sales Group):

Sales group. The EI includes it in the result for responsibility-based scope.

**VKORG** (Sales Organization):

Sales organization. The EI restricts and reports by sales organization; it is used for the customer–sales-area (KNVV) lookup so payment terms are compared at the correct sales-area level.

**VTWEG** (Distribution Channel):

Distribution channel. The EI restricts and reports by distribution channel; it is used for the KNVV lookup so payment terms are compared at the correct sales-area level.

**WAERK** (Document Currency):

Document currency of the sales order; amounts such as NETWR are expressed in this currency.

**ZTERM_KNVV** (Terms of Payment):

Payment terms from the customer master (KNVV) for the customer–sales-area combination. The EI compares this with ZTERM_VBKD; when they differ, the record is flagged. This field is populated from the KNVV lookup.

**ZTERM_VBKD** (Terms of Payment):

Payment terms from the sales document (VBKD). The EI compares this with ZTERM_KNVV; when they differ, the record is flagged. This is the core field used in the mismatch detection logic.

**ZTERM_VBRK** (Terms of Payment):

Payment terms from billing document (if applicable). The EI can include it in the result for billing context when available in the structure.


### Parameter Relationships

**Time-Based Parameters:**
- **BACKDAYS** defines how many days to look back from today when no date range is supplied; the EI builds the monitoring window from today minus BACKDAYS through today.
- **DATE_REF_FIELD** selects which date field on the sales order is used for that window: ERDAT (creation), AEDAT (changed on), AUDAT (document date), or VDATU (requested delivery date). The chosen field is restricted to the window when the EI reads order data.
- Set both when configuring the monitoring window (e.g. last 7 days by creation date).

**Payment Terms Comparison:**
- **ZTERM_VBKD** (document payment terms) and **ZTERM_KNVV** (customer master payment terms) are the core of the EI logic: the EI flags records where they differ. **KUNNR**, **VKORG**, **VTWEG**, and **SPART** define the customer–sales-area used for the KNVV lookup; **PARVW** defines which partner (e.g. sold-to) is used for that lookup. Use these together when focusing on specific customers or sales areas.

**Organizational Scope:**
- **VKORG**, **VTWEG**, **SPART**, **VKGRP**, and **VKBUR** scope the sales organizational dimension. The EI uses VKORG, VTWEG, and SPART for the KNVV join; use them together for sales-area-level monitoring.


### Default Values

- **BACKDAYS** — Default: `1` (when no date range is supplied, the EI uses a 1-day lookback from today for the monitoring window).
- **DATE_REF_FIELD** — Default: `ERDAT` (creation date is used as the reference date for the monitoring window when not supplied).
- **PARVW** — Default: `RG` (sold-to party); when not supplied, the EI restricts to partner function RG for the customer and payment terms lookup.

**Note:** When no date range is supplied, the EI builds the monitoring window from today minus BACKDAYS through today and applies it to the date field selected by DATE_REF_FIELD (ERDAT, AEDAT, AUDAT, or VDATU).

### Practical Configuration Examples

**Use Case 1: Last 7 days by creation date (default lookback)**

```
BACKDAYS = 7
DATE_REF_FIELD = ERDAT
```

**Purpose:** Monitor sales orders created in the last 7 days for payment terms mismatches. Suitable for routine weekly checks and order-to-cash control.

**Use Case 2: By sales organization and distribution channel**

```
VKORG = 1000, 2000
VTWEG = 10, 20
SPART = 00
```

**Purpose:** Limit results to specific sales organizations, distribution channels, and division. Supports regional or channel-specific payment terms consistency monitoring.

**Use Case 3: Sold-to party, document type, and customer range**

```
PARVW = RG
AUART = OR, RE
KUNNR = 0000100001–0000100500
BACKDAYS = 14
```

**Purpose:** Focus on standard orders and credit memos for a customer range over the last 14 days, using sold-to party for the payment terms comparison. Supports customer-master quality review and order-entry control.

**Use Case 4: Division, sales group, and purchase order reference**

```
SPART = 01, 02
VKGRP = 001, 002
BSTNK = 45000*
```

**Purpose:** Monitor payment terms mismatches by division and sales group for orders with specific customer PO number patterns. Supports delegation and process analysis.


### EI Function Structure

## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_SD_PT_CHK | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_SD_PT_CHK | AUART | Sales Document Type | CHAR(4) | AUART |
| /SKN/S_SW_10_01_SD_PT_CHK | AUDAT | Document Date (Date Received/Sent) | DATS(8) | AUDAT |
| /SKN/S_SW_10_01_SD_PT_CHK | BSTNK | Customer purchase order number | CHAR(20) | BSTNK |
| /SKN/S_SW_10_01_SD_PT_CHK | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_SD_PT_CHK | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_SD_PT_CHK | ERZET | Entry time | TIMS(6) | ERZET |
| /SKN/S_SW_10_01_SD_PT_CHK | KNUMV | Number of the document condition | CHAR(10) | KNUMV |
| /SKN/S_SW_10_01_SD_PT_CHK | KUNNR | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_SD_PT_CHK | KUNRG | Payer | CHAR(10) | KUNRG |
| /SKN/S_SW_10_01_SD_PT_CHK | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_SD_PT_CHK | NAME_TEXT | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_10_01_SD_PT_CHK | NETWR | Net Value of the Sales Order in Document Currency | CURR(15,2) | NETWR_AK |
| /SKN/S_SW_10_01_SD_PT_CHK | PARVW | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_SD_PT_CHK | POSNR | Sales Document Item | NUMC(6) | POSNR_VA |
| /SKN/S_SW_10_01_SD_PT_CHK | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_SD_PT_CHK | VBELN | Sales Document | CHAR(10) | VBELN_VA |
| /SKN/S_SW_10_01_SD_PT_CHK | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_SD_PT_CHK | VDATU | Requested delivery date | DATS(8) | EDATU_VBAK |
| /SKN/S_SW_10_01_SD_PT_CHK | VKBUR | Sales Office | CHAR(4) | VKBUR |
| /SKN/S_SW_10_01_SD_PT_CHK | VKGRP | Sales Group | CHAR(3) | VKGRP |
| /SKN/S_SW_10_01_SD_PT_CHK | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_SD_PT_CHK | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_SD_PT_CHK | WAERK | SD Document Currency | CUKY(5) | WAERK |
| /SKN/S_SW_10_01_SD_PT_CHK | ZTERM_KNVV | Terms of Payment Key | CHAR(4) | DZTERM |
| /SKN/S_SW_10_01_SD_PT_CHK | ZTERM_VBKD | Terms of Payment Key | CHAR(4) | DZTERM |
| /SKN/S_SW_10_01_SD_PT_CHK | ZTERM_VBRK | Terms of Payment Key | CHAR(4) | DZTERM |

### ABAP Code

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_SD_PT_CHK .
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
  DATA_MULTY: VBELN        VBAK-VBELN,
              VKORG        VBAK-VKORG,
              VTWEG        VBAK-VTWEG,
              SPART        VBAK-SPART,
              BSTNK        VBAK-BSTNK,
              VBTYP        VBAK-VBTYP,
              AUART        VBAK-AUART,
              ERDAT        VBAK-ERDAT,
              AEDAT        VBAK-AEDAT,
              AUDAT        VBAK-AUDAT,
              VDATU        VBAK-VDATU,
              KUNNR        VBPA-KUNNR,
              PARVW        VBPA-PARVW,
              ZTERM_VBKD   VBKD-ZTERM,
              ZTERM_KNVV   KNVV-ZTERM,
              DATUM        SY-DATUM.
  SELECT_MULTY: VBELN,
                VKORG ,
                VTWEG ,
                SPART,
                BSTNK,
                VBTYP,
                AUART,
                ERDAT,
                AEDAT,
                AUDAT,
                VDATU,
                KUNNR,
                PARVW,
                ZTERM_VBKD,
                ZTERM_KNVV,
                DATUM.
  CONVERT_MULTY: KUNNR ALPHA,
                 VBELN ALPHA.
  """Tanya 14/11/18 :
  CONVERT_MULTY:  AUART AUART ,
                  PARVW PARVW .
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
  DATA_SINGLE: SW_DEST RFCDEST.
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_SD_PT_CHK'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
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
  IF R_PARVW IS INITIAL.
    RS_PARVW-SIGN   = 'I' .
    RS_PARVW-OPTION = 'EQ' .
    RS_PARVW-LOW    = 'RG'.
    APPEND RS_PARVW TO R_PARVW.
  ENDIF.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[].
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[].
    WHEN 'AUDAT'.
      R_AUDAT[] = R_DATUM[].
    WHEN 'VDATU'.
      R_VDATU[] = R_DATUM[].
    WHEN OTHERS.
      R_ERDAT[] = R_DATUM[].
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  SELECT VBAK~VBELN VBAK~ERDAT VBAK~ERZET VBAK~ERNAM VBAK~AUDAT VBAK~VBTYP VBAK~AUART
         VBAK~AEDAT VBAK~NETWR VBAK~WAERK VBAK~VKORG VBAK~VTWEG VBAK~BSTNK VBAK~VKGRP VBAK~SPART
         VBPA~KUNNR
         KNA1~NAME1
         VBKD~ZTERM AS ZTERM_VBKD
         KNVV~ZTERM AS ZTERM_KNVV
         ADRP~NAME_TEXT
    FROM VBAK INNER JOIN VBPA  ON  VBAK~VBELN       EQ VBPA~VBELN
              INNER JOIN KNA1  ON  VBPA~KUNNR       EQ KNA1~KUNNR
              INNER JOIN VBKD  ON  VBAK~VBELN       EQ VBKD~VBELN
                               AND VBPA~POSNR       EQ VBKD~POSNR
              INNER JOIN KNVV  ON  VBPA~KUNNR       EQ KNVV~KUNNR
                               AND VBAK~VKORG       EQ KNVV~VKORG
                               AND VBAK~VTWEG       EQ KNVV~VTWEG
                               AND VBAK~SPART       EQ KNVV~SPART
                               AND VBKD~ZTERM       NE KNVV~ZTERM
              INNER JOIN USR21 ON  VBAK~ERNAM       EQ USR21~BNAME
              INNER JOIN ADRP  ON  USR21~PERSNUMBER EQ ADRP~PERSNUMBER
    INTO CORRESPONDING FIELDS OF TABLE T_DATA[]
    WHERE VBAK~VBELN IN R_VBELN
      AND VBAK~VKORG IN R_VKORG
      AND VBAK~VTWEG IN R_VTWEG
      AND VBAK~SPART IN R_SPART
      AND VBAK~BSTNK IN R_BSTNK
      AND VBAK~AUART IN R_AUART
      AND VBAK~ERDAT IN R_ERDAT
      AND VBAK~AUDAT IN R_AUDAT
      AND VBAK~AEDAT IN R_AEDAT
      AND VBAK~VDATU IN R_VDATU
      AND VBAK~VBTYP IN R_VBTYP
      AND VBPA~KUNNR IN R_KUNNR
      AND VBPA~PARVW IN R_PARVW.
  CHECK T_DATA[] IS NOT INITIAL.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
