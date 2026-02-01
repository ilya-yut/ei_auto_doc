# Exception Indicator: Customer Payment Terms comparison - SW_10_06_CUST_PTRM_C

## General Overview

This Exception Indicator (EI) monitors customer master data to identify cases where payment terms in the sales area (customer–sales organization–distribution channel–division) differ from payment terms in the company code. It provides visibility into inconsistent payment term settings that can affect cash flow, dunning, and reconciliation.

This EI serves as an essential control for credit management and master data governance by:
- Enabling detection of payment term mismatches between sales and company code views that may cause incorrect dunning or payment timing
- Supporting identification of customers with inconsistent terms for cleanup and alignment with credit policy
- Providing visibility into how long discrepancies have existed (duration since customer creation or change) for prioritization
- Enabling analysis by sales organization, distribution channel, division, and company code for accountability and process ownership
- Supporting compliance and audit by surfacing customers whose payment terms differ across organizational dimensions

Monitoring payment term consistency helps organizations avoid dunning errors, align terms with contracts, and maintain reliable master data for collections. The EI is particularly valuable for month-end close, credit reviews, and master data quality initiatives.

The EI uses customer sales (KNVV), customer general (KNA1), customer company code (KNB1), and organizational data (TVKO, T001) and compares payment terms between sales area and company code.


## Problem Description

Failure to monitor customer payment term consistency creates multiple risks across credit management, collections, and compliance:

**Financial and Reporting Issues**
- Unreconciled payment term differences between sales area and company code can lead to incorrect dunning run behavior and disputed dunning notices
- Inconsistent terms may cause cash flow forecasts to be wrong when one view is used for planning and another for execution
- Lack of visibility into how long discrepancies have existed complicates period-end reconciliation and audit evidence

**Operational and Control Risks**
- Payment term mismatches can trigger inappropriate dunning or block releases when company code and sales terms are out of sync
- Inability to filter by sales organization, company code, or customer limits effective cleanup and segregation of duties
- Missing visibility into duration of discrepancies increases the risk of prolonged master data quality issues

**Management Visibility and Decision-Making Risks**
- Absence of payment term comparison monitoring delays management awareness of master data inconsistencies that affect collections
- Lack of organizational and duration-based analysis limits the ability to prioritize cleanup and assign data ownership
- Inadequate visibility hinders root-cause analysis and corrective action when dunning or payment issues are reported

## Suggested Resolution

**Immediate Response**
- Review the customers flagged by the EI to confirm the payment term mismatch (sales area vs. company code) and scope (sales org, company code, customer)
- Verify high-impact or high-volume customers using the relevant transactions (e.g. VD02 for customer sales, XD02 for company code) to confirm intended terms
- Check creation date and duration of the discrepancy to assess whether it is long-standing or recent
- Identify business context: intentional override, data entry error, or missing alignment process

**System Assessment**
- Analyze the time window and organizational filters used for the run to ensure the monitoring scope matches the control objective
- Compare mismatch counts and patterns to prior periods to detect deterioration or improvement after cleanup
- Review sales organization, distribution channel, division, and company code filters to focus on the right segments
- Validate that duration and duration unit settings align with the intended use (e.g. focus on long-standing discrepancies)

**Corrective Actions**
- If payment terms are wrong in one view, correct customer master (sales area and/or company code) through the appropriate transaction and document the change
- Escalate repeated or systemic mismatches to master data owners and credit management for process and authorization review
- Update monitoring parameters (e.g. lookback, duration filter, organizational scope) to align with credit and governance requirements
- Document cleanup outcomes and establish recurring EI runs to maintain visibility into payment term consistency


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 3 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 4 | DURATION | Duration | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 5 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 6 | ERDAT | Created on | DATS | 8 | 0 | ERDAT_RF | DATUM |
| 7 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 8 | LANGU | Language |  | 0 | 0 |  |  |
| 9 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 10 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 11 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 12 | VTEXT_SPART | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 13 | VTEXT_TERM_BUKRS | Description | CHAR | 30 | 0 | DZTERM_BEZ | TEXT30 |
| 14 | VTEXT_TERM_VKORG | Description | CHAR | 30 | 0 | DZTERM_BEZ | TEXT30 |
| 15 | VTEXT_VKORG | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 16 | VTEXT_VTWEG | Name | CHAR | 20 | 0 | VTXTK | TEXT20 |
| 17 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 18 | ZTERM_BKRS | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
| 19 | ZTERM_VKRG | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 19 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Backdays):

Number of days to look back from the current date when building the default date range for customer creation date. When no explicit date range is supplied for creation date, the EI uses today minus this value as the start of the window. Used to restrict which customers are considered based on when the record was created.

**BUKRS** (Company Code):

Company code. Restricts which customers are included by assigning company code; used together with sales organization (via TVKO) and ensures the comparison is limited to the selected company codes.

**BUTXT** (Company Name):

Name of the company code or company. Populated from the company code master; used for display in the result.

**DURATION** (Duration):

Duration in the unit given by DURATION_UNIT (e.g. days) between the reference date (e.g. customer creation date) and the run date. Restricts which records are included when a duration filter is applied; also appears in the result. Used to focus on discrepancies that have existed for a specified time.

**DURATION_UNIT** (Duration Unit):

Unit for DURATION (e.g. days, hours). Used with DURATION when computing and filtering by how long the record has existed or the discrepancy has been present. Determines the unit passed to the duration calculation.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours.
- **M**: Minutes.

**ERDAT** (Created on):

Date on which the customer record was created. Restricts which customers are included by creation date; when no explicit range is supplied, the default range is derived from the lookback period. Also appears in the result.

**KUNNR** (Customer):

Customer number. Restricts which customers are included; used to focus on specific customers or a range of customer numbers.

**LANGU** (Language):

Language used when retrieving descriptions for payment terms, sales organization, distribution channel, and division. Determines the language of the description fields in the result.

**NAME1** (Name):

Customer name. Populated from customer master; used for display in the result.

**SPART** (Division):

Division. Restricts which customer–sales area combinations are included; used together with sales organization and distribution channel.

**VKORG** (Sales Organization):

Sales organization. Restricts which customer–sales area combinations are included; used together with distribution channel and division.

**VTEXT_SPART** (Name):

Short description of the division. Populated from master data; used for display.

**VTEXT_TERM_BUKRS** (Description):

Description of the payment terms in the company code view. Populated from the terms of payment master; used for display.

**VTEXT_TERM_VKORG** (Description):

Description of the payment terms in the sales area view. Populated from the terms of payment master; used for display.

**VTEXT_VKORG** (Name):

Short description of the sales organization. Populated from master data; used for display.

**VTEXT_VTWEG** (Name):

Short description of the distribution channel. Populated from master data; used for display.

**VTWEG** (Distribution Channel):

Distribution channel. Restricts which customer–sales area combinations are included; used together with sales organization and division.

**ZTERM_BKRS** (Terms of Payment):

Payment terms key in the company code view. Populated from customer company code data; used for comparison and display. The EI flags records where this differs from the sales area payment terms.

**ZTERM_VKRG** (Terms of Payment):

Payment terms key in the sales area view. Populated from customer sales data; used for comparison and display. The EI flags records where this differs from the company code payment terms.


### Parameter Relationships

**Time and duration parameters**

- **BACKDAYS** defines the default number of days to look back when no explicit creation date range is supplied. It is used together with **ERDAT**: when the creation date range is empty, the EI builds a range from today minus BACKDAYS and applies it to ERDAT.
- **ERDAT** restricts which customers are included by creation date. The default range for ERDAT is derived from BACKDAYS when no range is provided.

**Duration filtering**

- **DURATION** and **DURATION_UNIT** work together. DURATION_UNIT specifies the unit (e.g. days) used when computing the duration between the reference date (e.g. customer creation date) and the run date. The computed duration is compared to the DURATION filter; only records that fall within the DURATION range are returned.

**Organizational scope**

- **VKORG**, **VTWEG**, **SPART**, and **BUKRS** together define the organizational scope: sales organization, distribution channel, division, and company code. The EI compares payment terms between the sales area (VKORG, VTWEG, SPART) and the company code (BUKRS); all are used in the selection and appear in the result.
- **KUNNR** narrows the scope to specific customers; used together with the organizational parameters to focus the comparison.


### Default Values

- **BACKDAYS** — Default: initial (0); when no creation date range is supplied, the EI uses today minus BACKDAYS as the start of the window. If BACKDAYS is initial, no default range is built and the caller must supply ERDAT.
- **DURATION_UNIT** — Default: `D` (days) in the code when not supplied; used for computing and filtering by duration.
- **LANGU** — Default: system language (e.g. from SY-LANGU) when not supplied; used for description texts.

**Note:** When ERDAT is not supplied and BACKDAYS is set, the creation date range is built from today minus BACKDAYS.

### Practical Configuration Examples

**Use Case 1: Payment term mismatch by sales organization (recent customers)**

```
BACKDAYS = 30
VKORG = 1000
BUKRS = 1000
```

**Purpose:** Find customers created in the last 30 days whose payment terms in sales org 1000 differ from company code 1000, for quick cleanup after go-live or bulk load.

**Use Case 2: Duration filter (long-standing discrepancies)**

```
ERDAT = last 2 years (or wide range)
DURATION_UNIT = D
DURATION = 90 to 999999
VKORG = (optional)
BUKRS = (optional)
```

**Purpose:** Focus on customers whose payment term mismatch has existed for at least 90 days, to prioritize long-standing data quality issues.

**Use Case 3: Specific customer and company code**

```
KUNNR = 0000100001
BUKRS = 2000
VKORG = 2000
VTWEG = 10
SPART = 00
```

**Purpose:** Check payment term consistency for a single customer across sales area and company code, for audit or dispute resolution.

**Use Case 4: Multiple organizational dimensions**

```
VKORG = 1000, 2000
VTWEG = 10
SPART = 00
BUKRS = 1000, 2000
BACKDAYS = 7
```

**Purpose:** Review payment term mismatches across two sales organizations and company codes for the last week, for periodic governance review.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | ERDAT | Date on which the Record Was Created | DATS(8) | ERDAT_RF |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | KUNNR | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | VTEXT_SPART | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | VTEXT_TERM_BUKRS | Description of terms of payment | CHAR(30) | DZTERM_BEZ |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | VTEXT_TERM_VKORG | Description of terms of payment | CHAR(30) | DZTERM_BEZ |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | VTEXT_VKORG | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | VTEXT_VTWEG | Name | CHAR(20) | VTXTK |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | ZTERM_BKRS | Terms of Payment Key | CHAR(4) | DZTERM |
| /SKN/S_SW_10_06_CUST_ZTERM_CHK | ZTERM_VKRG | Terms of Payment Key | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_06_CUST_TERM_CHK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_CUST_ZTERM_CHK
*"----------------------------------------------------------------------
  DATA_SINGLE: SW_DEST   RFCDEST,
               LANGU     LANGU,
               BACKDAYS  INT4,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  DATA_MULTY: KUNNR KUNNR,
              VKORG VKORG,
              VTWEG VTWEG,
              SPART SPART,
              LAND1 LAND1,
              BUKRS BUKRS,
              ERDAT ERDAT,
              DURATION    /SKN/E_SW_DURATION.
  SELECT_MULTY: KUNNR,
                VKORG,
                VTWEG,
                SPART,
                LAND1,
                BUKRS,
                ERDAT,
                DURATION.
  "lv_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  SELECT_SINGLE: SW_DEST, LANGU, BACKDAYS, DURATION_UNIT.
  CONVERT_MULTY: KUNNR ALPHA.
  DATA: LS_DATA LIKE LINE OF T_DATA,
        LT_DATA LIKE TABLE OF LS_DATA.
 DATA : LV_TIME_DIFF TYPE  INT4 .
 DATA : SY_TABIX LIKE SY-TABIX.
 DATA : DATE_FROM LIKE SY-DATUM .
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_06_CUST_TERM_CHK'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
   IF R_ERDAT[] IS INITIAL .
     IF LV_BACKDAYS IS NOT INITIAL.
       RS_ERDAT-SIGN = 'I' .
        RS_ERDAT-OPTION = 'GE' .
         DATE_FROM = SY-DATUM - LV_BACKDAYS .
         RS_ERDAT-LOW = DATE_FROM .
          APPEND RS_ERDAT TO R_ERDAT.
      ENDIF.
   ENDIF.
  SELECT KNVV~KUNNR KNVV~VKORG KNVV~VTWEG KNVV~SPART KNVV~ZTERM AS ZTERM_VKRG
    KNA1~NAME1 KNA1~LAND1 KNA1~ERDAT
    KNB1~ZTERM AS ZTERM_BKRS
    TVKO~BUKRS "t024e~ekotx
    T001~BUTXT
    FROM KNVV
    INNER JOIN KNA1      ON KNVV~KUNNR  EQ KNA1~KUNNR
    INNER JOIN KNB1      ON KNVV~KUNNR  EQ KNB1~KUNNR
    INNER JOIN TVKO      ON KNVV~VKORG  EQ TVKO~VKORG
    INNER JOIN T001      ON TVKO~BUKRS  EQ T001~BUKRS
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE KNVV~KUNNR IN R_KUNNR
    AND   KNVV~VKORG IN R_VKORG
    AND   KNVV~VTWEG IN R_VTWEG
    AND   KNVV~SPART IN R_SPART
    AND   KNA1~LAND1 IN R_LAND1
    AND   KNA1~ERDAT IN R_ERDAT
    AND   TVKO~BUKRS IN R_BUKRS
    AND   TVKO~BUKRS = KNB1~BUKRS
    AND   KNVV~ZTERM <> KNB1~ZTERM.
  CHECK LT_DATA IS NOT INITIAL.
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX.
    LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = LS_DATA-ERDAT
          T_FROM            = SY-UZEIT
          D_TO              = SY-DATUM
          T_TO              = SY-UZEIT
          TIME_UNIT         = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF         = LV_TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
      IF SY-SUBRC = 0.
          LS_DATA-DURATION  = LV_TIME_DIFF .
      ENDIF.
      MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
  ENDLOOP.
  DELETE LT_DATA WHERE DURATION  NOT IN R_DURATION .
  LOOP AT LT_DATA INTO LS_DATA.
    CALL FUNCTION '/SKN/F_SW_10_ZTERM_DESC'
      EXPORTING
        ZTERM            = LS_DATA-ZTERM_VKRG
        LANGU            = LV_LANGU
      IMPORTING
        ZTERM_DESC       = LS_DATA-VTEXT_TERM_VKORG
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_ZTERM_DESC'
      EXPORTING
        ZTERM            = LS_DATA-ZTERM_BKRS
        LANGU            = LV_LANGU
      IMPORTING
        ZTERM_DESC       = LS_DATA-VTEXT_TERM_BUKRS
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
****************Sales Org
    CALL FUNCTION '/SKN/F_SW_10_SALES_ORG_DESC'
      EXPORTING
        VKORG            = LS_DATA-VKORG
        LANGU            = LV_LANGU
      IMPORTING
        SALES_ORG_DESC       = LS_DATA-VTEXT_VKORG
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
*********Distribution Channel Description
   CALL FUNCTION '/SKN/F_SW_10_DISTR_CHAN_DESC'
      EXPORTING
        VTWEG            = LS_DATA-VTWEG
        LANGU            = LV_LANGU
      IMPORTING
        DISTR_CHAN_DESC  = LS_DATA-VTEXT_VTWEG
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
**********************Division
    CALL FUNCTION '/SKN/F_SW_10_DIVISION_DESC'
      EXPORTING
        SPART            = LS_DATA-SPART
        LANGU            = LV_LANGU
      IMPORTING
        DIV_DESC         = LS_DATA-VTEXT_SPART
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
**********************
    MOVE-CORRESPONDING LS_DATA TO T_DATA.
    APPEND T_DATA.
  ENDLOOP.
  IF T_DATA[] IS NOT INITIAL.
    IS_ALERT = ABAP_TRUE.
  ENDIF.
ENDFUNCTION.
```