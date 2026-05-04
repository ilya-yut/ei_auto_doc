# Exception Indicator: SD Billing Document – Details - SW_10_01_INV_DET

## General Overview

This Exception Indicator (EI) monitors SD billing document details in Sales and Distribution (SD), providing record-level visibility into billing documents (VBRK) with configurable selection by billing date, organizational dimensions, document type, payer and sold-to party, net value, and posting status. It supports analysis of how long billing documents have been in a given state by calculating duration from a configurable reference date and time to the current date.

This EI serves as an essential control for billing and revenue oversight by:
- Enabling detection of billing documents that meet configurable criteria (date range, sales organization, distribution channel, billing type, payer, net value, and posting status) for follow-up and exception handling
- Supporting identification of billing documents by creation date, creator, and billing date for audit and process review
- Providing visibility into time elapsed since a reference date (e.g. billing date or creation date) in configurable units for prioritization and aging analysis
- Enabling analysis by payer and sold-to party with resolved customer descriptions for accountability and dispute management
- Supporting filtering by document category, billing category, cancellation status, and transfer-to-accounting status for focused exception review

This detail-level monitoring helps organizations track individual billing documents that require attention, reconcile posting status, and manage billing and revenue assurance. The EI is particularly valuable for month-end close, billing backlog review, and exception management of SD billing data.

The EI reads billing document header data from SAP SD table VBRK and enriches results with payer and sold-to customer descriptions via customer master lookup.


## Problem Description

Failure to monitor SD billing document details at record level creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unmonitored billing documents may delay revenue recognition when posting to accounting is pending or blocked, and exceptions go unnoticed until period close
- Billing documents with unusual net value ranges or concentrations in specific periods may indicate pricing errors, duplicate billing, or missed reversals requiring adjustment
- Lack of visibility into billing document age (time since billing or creation date) can delay identification of stuck or unposted documents that affect period-end closing
- Billing documents cancelled or with specific posting status may require follow-up for correct financial reporting and audit trail

**Sales Operations and Control Risks**
- Billing documents created by specific users or in specific time windows without monitoring may indicate unauthorized or erroneous billing activity
- Payer and sold-to party concentration or anomalies in billing detail may signal master data issues, incorrect partner assignment, or dispute patterns requiring process review
- Unfiltered view by sales organization, distribution channel, division, or billing type can obscure operational bottlenecks or channel-specific billing problems
- Missing or delayed visibility into cancellation status and posting status can lead to incorrect assumptions about document state and downstream posting

**Management Visibility and Decision-Making Risks**
- Lack of record-level billing monitoring delays awareness of documents that need release, correction, or escalation for revenue and compliance
- Unidentified aging of billing documents (e.g. long duration since billing date) can lead to missed prioritization and root-cause analysis
- Billing detail exceptions that require audit or compliance review may go unnoticed without targeted monitoring by organizational and document dimensions
- Absence of configurable detail-level analysis limits the ability to assign accountability (e.g. by creator, payer, sold-to) and to align follow-up with business responsibility

## Suggested Resolution

**Immediate Response**
- Review the billing document records flagged by the EI to understand which selection criteria (date range, organization, document type, payer, net value, posting status) drove the result set
- Verify high-value or atypical billing documents using the appropriate SD billing display transaction to confirm legitimacy, correct payer and sold-to, and proper billing type and category
- Check posting status and cancellation status of flagged documents to ensure no blocked transfers to accounting or erroneous cancellations
- Identify business context: planned billing runs, corrections, reversals, or possible data or process errors

**System Assessment**
- Analyze the date and time basis used for selection and for duration calculation to ensure the monitoring window and reference date align with policy (e.g. billing date vs creation date)
- Compare current billing document counts and values by sales organization, distribution channel, and period to prior runs to spot trends or one-off spikes
- Review payer and sold-to distribution and customer descriptions to identify misallocated billings or master data issues
- Assess document category, billing type, and cancellation status to confirm the result set matches the intended scope (e.g. excluding cancelled, or focusing on unposted)
- Validate net value ranges and duration filters so that only relevant exceptions are in scope for follow-up

**Corrective Actions**
- Correct or reverse erroneous billing documents via the appropriate SD billing and accounting transactions (e.g. billing reversal, posting release)
- Escalate documents that are blocked or require approval to finance and sales for release and posting
- Update customer master (e.g. sold-to, payer) and partner procedures if assignment errors drive repeated exceptions
- Adjust monitoring parameters (date range, organizational scope, net value, duration) so that future runs focus on the intended exception set
- Document findings and approvals for audit and management reporting
- Establish recurring EI execution to provide continuous visibility into billing document details and aging
- Route alerts to responsible sales, billing, or revenue teams based on organizational responsibility


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 4 | BZIRK | Sales district | CHAR | 6 | 0 | BZIRK | BZIRK |
| 5 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 6 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 7 | ERDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 8 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 9 | ERZET | Time | TIMS | 6 | 0 | UZEIT | UZEIT |
| 10 | FKART | Billing Type | CHAR | 4 | 0 | FKART | FKART |
| 11 | FKDAT | Billing Date | DATS | 8 | 0 | FKDAT | DATUM |
| 12 | FKSTO | Cancelled | CHAR | 1 | 0 | FKSTO | XFELD |
| 13 | FKTYP | Billing category | CHAR | 1 | 0 | FKTYP | FKTYP |
| 14 | KDGRP | Customer group | CHAR | 2 | 0 | KDGRP | KDGRP |
| 15 | KUNAG | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 16 | KUNRG | Payer | CHAR | 10 | 0 | KUNRG | KUNNR |
| 17 | NETWR | Net Value | CURR | 15 | 2 | NETWR | WERTV8 |
| 18 | PAYER_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 19 | RFBSK | Posting Status | CHAR | 1 | 0 | RFBSK | RFBSK |
| 20 | SOLDTO_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 21 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 22 | VBELN | Billing Document | CHAR | 10 | 0 | VBELN_VF | VBELN |
| 23 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 24 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 25 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 26 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 26 parameters listed in the Parameters Reference Table above.

**AEDAT** (Changed on):

Company code and billing-document context: the date on which the billing document was last changed. Used to restrict which billing documents are included in the result set by change date. Values are provided as a range; only documents whose change date falls within that range are selected from VBRK.

**BACKDAYS** (Backdays):

Number of days to look back from today when no date range is supplied by the user. The EI uses this value to build a default lower bound for the date range (today minus BACKDAYS) so that recent billing documents are included. When no range is passed and BACKDAYS is set, the selection uses this lookback; when BACKDAYS is initial, the selection uses today only.

**DATE_REF_FLD** (Date reference field):

Name of the date field on the billing document that is used for two purposes: (1) the default date range built from BACKDAYS is applied to this field for document selection (e.g. only documents whose chosen date falls in the range are selected); (2) the same field (together with creation time when relevant) is used as the reference for calculating duration from that date/time to the current date/time. So DATE_REF_FLD determines both which date drives the selection range and which date drives the duration value in the output.

**DATE_REF_FLD Options:**
- **FKDAT**: Billing date — use billing date for selection and for duration calculation (default in code).
- **ERDAT**: Created on — use document creation date for selection and for duration calculation.

**BACKDAYS and DATE_REF_FLD Connection:** When no date range is supplied, BACKDAYS defines how far back the range goes (today minus BACKDAYS to today). That range is then applied to the date field named by DATE_REF_FLD (e.g. FKDAT or ERDAT). Set DATE_REF_FLD to match the business meaning you want (e.g. FKDAT for “age since billing”, ERDAT for “age since creation”).

**BZIRK** (Sales district):

Sales district from the billing document context. Used to restrict results to billing documents belonging to the specified sales district(s). Values are provided as a range; only documents in the given district(s) are included.

**DURATION** (Duration In Time Units):

Length of time, in the unit defined by DURATION_UNIT, between the reference date/time (e.g. billing date and creation time) and the current date/time. The EI calculates this per billing document and populates the output; the parameter is used to filter the result set so that only documents whose calculated duration falls within the supplied range are returned. Used together with DURATION_UNIT to express the monitoring window (e.g. documents older than 30 days).

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed and in which the EI computes the time difference from the reference date/time to now. Determines how duration is calculated and how the DURATION filter is interpreted.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** DURATION and DURATION_UNIT work together: DURATION supplies the numeric range (e.g. 0–90) and DURATION_UNIT defines whether that is in hours, minutes, days, or full days. Set both consistently (e.g. DURATION_UNIT = D and DURATION = 30 - 365 for documents between 30 and 365 days old).

**ERDAT** (Created on):

Date on which the billing document was created. Used to restrict which billing documents are included by creation date. Values are provided as a range; only documents whose creation date falls within that range are selected.

**ERNAM** (Created by):

User who created the billing document. Used to restrict results to documents created by the specified user(s). Values are provided as a range; only documents created by those users are included.

**ERZET** (Time):

Creation time of the billing document. Used to restrict which billing documents are included by creation time. Values are provided as a range; only documents whose creation time falls within that range are selected.

**FKART** (Billing Type):

Billing type (e.g. F2, F8) from the billing document. Used to restrict results to specific billing document types. Values are provided as a range; only documents with the given billing type(s) are included.

**FKDAT** (Billing Date):

Billing date of the document. Used to restrict which billing documents are included by billing date. Values are provided as a range; only documents whose billing date falls within that range are selected. The EI can use this date (or creation date, depending on configuration) as the reference for duration calculation.

**FKSTO** (Cancelled):

Indicates whether the billing document is cancelled. Used to include or exclude cancelled documents in the result set.

**FKSTO Options:**
- **X**: Billing document is cancelled
- ** ** (space): Billing document is not cancelled

**FKTYP** (Billing category):

Billing category from the billing document. Used to restrict results to specific billing categories. Values are provided as a range; only documents with the given category/categories are included.

**KDGRP** (Customer group):

Customer group from the billing document context. Used to restrict results to billing documents for customers in the specified customer group(s). Values are provided as a range; only documents with the given customer group(s) are included.

**KUNAG** (Sold-to party):

Sold-to party (customer) number from the billing document. Used to restrict results to specific sold-to parties. Values are provided as a range; only documents for those sold-to parties are included. The EI resolves the sold-to party to a customer description (SOLDTO_DESC) in the output.

**KUNRG** (Payer):

Payer (customer) number from the billing document. Used to restrict results to specific payers. Values are provided as a range; only documents for those payers are included. The EI resolves the payer to a customer description (PAYER_DESC) in the output.

**NETWR** (Net Value):

Net value of the billing document in document currency. Used to restrict results to documents whose net value falls within the supplied range. Only billing documents with net value in that range are included.

**PAYER_DESC** (Name):

Payer customer name (description) from customer master. The EI populates this in the output by looking up the payer (KUNRG) in the customer master. Used to display the payer name alongside the payer number in the result set.

**RFBSK** (Posting Status):

Status for transfer to accounting (e.g. posted, not yet transferred). Used to restrict results to billing documents with specific posting status. Values are provided as a range; only documents with the given status(es) are included.

**SOLDTO_DESC** (Name):

Sold-to party customer name (description) from customer master. The EI populates this in the output by looking up the sold-to party (KUNAG) in the customer master. Used to display the sold-to name alongside the sold-to number in the result set.

**SPART** (Division):

Division from the billing document context. Used to restrict results to specific divisions. Values are provided as a range; only documents with the given division(s) are included.

**VBELN** (Billing Document):

Billing document number. Used to restrict results to specific billing document(s). Values are provided as a range; only the specified document(s) are included.

**VBTYP** (SD document categ.):

SD document category from the billing document. Used to restrict results to specific document categories. Values are provided as a range; only documents with the given category/categories are included.

**VKORG** (Sales Organization):

Sales organization from the billing document. Used to restrict results to specific sales organizations. Values are provided as a range; only documents for those sales organizations are included.

**VTWEG** (Distribution Channel):

Distribution channel from the billing document. Used to restrict results to specific distribution channels. Values are provided as a range; only documents for those distribution channels are included.

**WAERK** (Document Currency):

Document currency of the billing document. Used to restrict results to documents in specific currencies. Values are provided as a range; only documents in the given currency/currencies are included. This is the currency in which NETWR is expressed in the output.


### Parameter Relationships

**Time and Duration Parameters:**

- **BACKDAYS** defines the lookback window when no date range is supplied; the EI builds the selection range from today minus BACKDAYS to today.
- **DATE_REF_FLD** specifies which date field on the billing document receives this range (FKDAT or ERDAT) and is also used as the reference for duration calculation. BACKDAYS and DATE_REF_FLD work together to define both the selection window and which date “starts the clock” for duration.
- **DURATION** and **DURATION_UNIT** work together: the EI calculates the time elapsed from the reference date/time (the field named by DATE_REF_FLD, plus creation time) on each billing document to the current date/time, expresses it in the unit given by DURATION_UNIT, and then filters the result set using the DURATION parameter range. Configure DATE_REF_FLD (e.g. FKDAT for billing date, ERDAT for creation date), then DURATION_UNIT (e.g. D for days), then set DURATION to the desired range (e.g. 30–365 to see documents between 30 and 365 days old).

**Organizational and Document Dimension Parameters:**

- **VKORG**, **VTWEG**, **SPART**, **BZIRK**, **KDGRP**, **FKART**, **FKTYP**, and **VBTYP** define the organizational and document scope. They can be used together to focus on a specific sales organization, distribution channel, division, sales district, customer group, billing type, billing category, or document category. Combining these narrows the result set to the intended business scope (e.g. one sales org and one billing type).

**Partner and Value Parameters:**

- **KUNRG** (payer) and **KUNAG** (sold-to party) restrict results by customer. **NETWR** restricts by net value range. These can be combined with organizational parameters to analyze billing detail by payer, sold-to, and value band (e.g. high-value billing documents for a specific payer).


### Default Values

- **BACKDAYS** — Default: `1` (when not supplied; used to build the default date range as today minus 1 day to today).
- **DATE_REF_FLD** — Default: `FKDAT` (billing date is used for selection and for duration calculation when not supplied).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).

### Practical Configuration Examples

**Use Case 1: Recent billing documents by sales organization and distribution channel**
```
VKORG = 1000
VTWEG = 10
BACKDAYS = 30
```
**Purpose:** Focus on billing documents from the last 30 days for sales organization 1000 and distribution channel 10, for period-end and channel performance review.

**Use Case 2: High-value billing documents for a specific payer**
```
KUNRG = 0000100001
NETWR = 100000 - 999999999
VKORG = 1000
VTWEG = 10
SPART = 00
```
**Purpose:** Identify high-value billing documents (net value between 100,000 and 999,999,999 in document currency) for payer 0000100001 in a given sales org, distribution channel, and division for audit and concentration review.

**Use Case 3: Billing documents by duration (age) in days**
```
DURATION_UNIT = D
DURATION = 30 - 90
VKORG = 1000
FKART = F2
RFBSK = 
```
**Purpose:** Find billing documents that are between 30 and 90 days old (based on the configured reference date), for sales org 1000 and billing type F2, to prioritize follow-up on aged documents. Posting status can be left open or set as needed.

**Use Case 4: Billing documents created by a specific user in a date range**
```
ERNAM = USER01
ERDAT = 20250101 - 20250131
ERZET = 000000 - 235959
FKDAT = 20250101 - 20250131
```
**Purpose:** Review billing documents created by USER01 in January 2025, with creation date and time and billing date in the same period, for creator-based accountability and audit sampling.

**Use Case 5: Full-day duration filter for specific day age**
```
DURATION_UNIT = F
DURATION = 7
VKORG = 1000
VTWEG = 10
FKSTO =  
```
**Purpose:** Restrict to billing documents that are exactly 7 full days old (in the configured unit F), for sales org 1000 and distribution channel 10, excluding cancelled documents, to target documents that have aged exactly one week.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_01_INV_DET | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_01_INV_DET | BZIRK | Sales district | CHAR(6) | BZIRK |
| /SKN/S_SW_10_01_INV_DET | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_INV_DET | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_INV_DET | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_01_INV_DET | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_01_INV_DET | ERZET | Time | TIMS(6) | UZEIT |
| /SKN/S_SW_10_01_INV_DET | FKART | Billing Type | CHAR(4) | FKART |
| /SKN/S_SW_10_01_INV_DET | FKDAT | Billing date for billing index and printout | DATS(8) | FKDAT |
| /SKN/S_SW_10_01_INV_DET | FKSTO | Billing document is cancelled | CHAR(1) | FKSTO |
| /SKN/S_SW_10_01_INV_DET | FKTYP | Billing category | CHAR(1) | FKTYP |
| /SKN/S_SW_10_01_INV_DET | KDGRP | Customer group | CHAR(2) | KDGRP |
| /SKN/S_SW_10_01_INV_DET | KUNAG | Sold-to party | CHAR(10) | KUNAG |
| /SKN/S_SW_10_01_INV_DET | KUNRG | Payer | CHAR(10) | KUNRG |
| /SKN/S_SW_10_01_INV_DET | NETWR | Net Value in Document Currency | CURR(15,2) | NETWR |
| /SKN/S_SW_10_01_INV_DET | PAYER_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_INV_DET | RFBSK | Status for transfer to accounting | CHAR(1) | RFBSK |
| /SKN/S_SW_10_01_INV_DET | SOLDTO_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_INV_DET | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_01_INV_DET | VBELN | Billing Document | CHAR(10) | VBELN_VF |
| /SKN/S_SW_10_01_INV_DET | VBTYP | SD document category | CHAR(1) | VBTYP |
| /SKN/S_SW_10_01_INV_DET | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_01_INV_DET | VTWEG | Distribution Channel | CHAR(2) | VTWEG |
| /SKN/S_SW_10_01_INV_DET | WAERK | SD Document Currency | CUKY(5) | WAERK |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_INV_DET.
*"--------------------------------------.--------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_INV_DET OPTIONAL
*"----------------------------------------------------------------------
DATA : SY_TABIX LIKE SY-TABIX .
DATA : DATE_FROM LIKE SY-DATUM .
DATA : REF_DATE TYPE D .
DATA : LV_TIME_DIFF TYPE INT4 .
DATA : FLD(60) TYPE C .
FIELD-SYMBOLS:  TYPE ANY .
DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
             LANGU  LANGU,
             BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
 LV_BACKDAYS = 1.
 LV_DATE_REF_FLD = 'FKDAT'.
 LV_DURATION_UNIT = 'D'.
 SELECT_SINGLE: MANAGE_IN_UTC,
                LANGU,
                BACKDAYS,
                DATE_REF_FLD,
                DURATION_UNIT.
DATA_MULTY: FKDAT        FKDAT,
            DATUM        SY-DATUM,
            ERDAT        ERDAT,
            ERZET        UZEIT,
            VBELN        VBELN_VF,
            NETWR        NETWR_AK,
            DURATION     /SKN/E_SW_DURATION,
            FKART        FKART,
            FKTYP        FKTYP,
            VBTYP        VBTYP,
            VKORG        VKORG,
            VTWEG        VTWEG,
            KDGRP        KDGRP,
            BZIRK        BZIRK,
            KUNRG        KUNRG,
            KUNAG        KUNAG,
            ERNAM        ERNAM,
            AEDAT        AEDAT,
            RFBSK        RFBSK,
            FKSTO        FKSTO
            .
SELECT_MULTY:
            FKDAT,
            DATUM,
            ERDAT,
            ERZET,
            VBELN,
            NETWR,
            DURATION,
            FKART,
            FKTYP,
            VBTYP,
            VKORG,
            VTWEG,
            KDGRP,
            BZIRK,
            KUNRG,
            KUNAG,
            ERNAM,
            AEDAT,
            RFBSK,
            FKSTO
            .
CONVERT_MULTY: KUNRG ALPHA,
               KUNAG ALPHA,
               VBELN ALPHA.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_INV_DET'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
   IF R_DATUM[] IS INITIAL .
     IF LV_BACKDAYS IS NOT INITIAL.
       RS_DATUM-SIGN   = 'I' .
       RS_DATUM-OPTION = 'GE' .
       DATE_FROM       = SY-DATUM - LV_BACKDAYS .
       RS_DATUM-LOW    = DATE_FROM .
       APPEND RS_DATUM TO R_DATUM.
     ELSE.
       RS_DATUM-SIGN   = 'I' .
       RS_DATUM-OPTION = 'EQ' .
       RS_DATUM-LOW    = SY-DATUM .
       APPEND RS_DATUM TO R_DATUM.
     ENDIF.
   ENDIF.
   CASE LV_DATE_REF_FLD.
     WHEN 'ERDAT'.
       R_ERDAT[] = R_DATUM[].
     WHEN OTHERS.
       R_FKDAT[] = R_DATUM[].
   ENDCASE.
*--- Retrieve data (detail only, VBRK only)
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM VBRK
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE VBELN IN R_VBELN
      AND FKART IN R_FKART
      AND FKTYP IN R_FKTYP
      AND VBTYP IN R_VBTYP
      AND VKORG IN R_VKORG
      AND VTWEG IN R_VTWEG
      AND KDGRP IN R_KDGRP
      AND BZIRK IN R_BZIRK
      AND FKDAT IN R_FKDAT
      AND ERDAT IN R_ERDAT
      AND ERZET IN R_ERZET
      AND ERNAM IN R_ERNAM
      AND AEDAT IN R_AEDAT
      AND KUNRG IN R_KUNRG
      AND KUNAG IN R_KUNAG
      AND NETWR IN R_NETWR
      AND RFBSK IN R_RFBSK
      AND FKSTO IN R_FKSTO.
*--- Calculate duration (reference date/time to now)
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX.
    T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD.
    ASSIGN (FLD) TO .
    REF_DATE = .
    IF NOT REF_DATE IS INITIAL.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM    = REF_DATE
          T_FROM    = T_DATA-ERZET
          D_TO      = SY-DATUM
          T_TO      = SY-UZEIT
          TIME_UNIT = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF = LV_TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF LV_TIME_DIFF < '999999'.
          T_DATA-DURATION = LV_TIME_DIFF.
        ELSE.
          T_DATA-DURATION = '999999'.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
  IF R_DURATION[] IS NOT INITIAL.
    DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  ENDIF.
*--- Payer and Sold-to descriptions (business value)
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX.
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR     = T_DATA-KUNRG
      IMPORTING
        CUST_DESC = T_DATA-PAYER_DESC
      EXCEPTIONS
        WRONG_CUSTOMER = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
      CLEAR T_DATA-PAYER_DESC.
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR     = T_DATA-KUNAG
      IMPORTING
        CUST_DESC = T_DATA-SOLDTO_DESC
      EXCEPTIONS
        WRONG_CUSTOMER = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
      CLEAR T_DATA-SOLDTO_DESC.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL IS INITIAL.
  IS_ALERT = 'X'.
ENDFUNCTION.
```
