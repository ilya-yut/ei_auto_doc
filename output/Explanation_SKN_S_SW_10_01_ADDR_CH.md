# Exception Indicator: Manually changed addresses in Sales document ( SW_10_01_ADDR_CH)

## General Overview

This Exception Indicator identifies sales documents where partner addresses were manually changed, comparing the modified address on the document partner record with the original customer master address and surfacing cases recorded in address change documents from sales or delivery transactions.

This EI serves as an essential control for sales order integrity and customer master discipline by:

- Enabling detection of manual address changes on sales document partners that differ from the customer master address
- Supporting review of who changed an address, when, and through which sales or delivery transaction
- Providing side-by-side visibility into modified and original name, street, city, and country values
- Enabling age-based prioritization when address changes remain open for follow-up after a chosen reference date
- Supporting audit sampling by customer, partner function, sales document, and user

Typical use includes fraud and compliance review, customer master governance, and periodic sampling of ship-to or sold-to address overrides before billing or delivery. Results are intended for exception workflows rather than operational SD list reporting.

The routine reads address change documents for address objects, links them to sales document partners and customer master addresses, enriches results with customer descriptions, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor manually changed addresses on sales documents creates multiple risks across order fulfillment, billing accuracy, and compliance:

**Sales and Fulfillment Risks**

- Ship-to or sold-to address overrides can redirect goods or invoices to unintended locations when not reviewed
- Manual address changes that differ from customer master data can cause delivery errors or customer disputes
- Undetected address overrides on high-volume customers can concentrate operational and fraud risk

**Operational Risks**

- Change-document scope that is too broad or too narrow can hide actionable address overrides or create reviewer fatigue
- Lookback and age settings misaligned with review cadence can exclude recent changes or retain stale rows
- Partner-function scope that is not tuned can mix irrelevant partner roles into the address-change queue

**Control and Audit Risks**

- Weak monitoring reduces evidence that manual address changes were reviewed before shipment or billing
- Lack of recurring exception review weakens accountability for who may alter customer delivery data on orders
- Missing user- and transaction-based sampling limits detection of unauthorized address maintenance

## Suggested Resolution

**Immediate Response**

- Review flagged sales documents for modified versus original address fields, customer, partner function, and change user
- Confirm with sales or customer service whether the address override is authorized and documented
- Prioritize high-value customers or ship-to changes for immediate validation before goods issue or billing

**System Assessment**

- Validate lookback window and age settings against how the team reviews address maintenance
- Tune customer, partner-function, and transaction scope so results stay actionable
- Compare exception counts by user, transaction type, and partner function to find systematic override patterns

**Corrective Actions**

- Revert unauthorized address changes or update customer master through standard SD and master-data processes
- Brief users on policy for document-level address changes versus master-data updates
- Document review outcomes and schedule recurring runs for critical customers or sales organizations


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ADDRNUMBER_MOD | Address number | CHAR | 10 | 0 | AD_ADDRNUM | AD_ADDRNUM |
| 2 | ADDRNUMBER_ORGNL | Address number | CHAR | 10 | 0 | AD_ADDRNUM | AD_ADDRNUM |
| 3 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 4 | CITY1_MOD | City | CHAR | 40 | 0 | AD_CITY1 | TEXT40 |
| 5 | CITY1_ORGNL | City | CHAR | 40 | 0 | AD_CITY1 | TEXT40 |
| 6 | COUNTRY_MOD | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 7 | COUNTRY_ORGNL | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 8 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 9 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 10 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 11 | KUNNR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 12 | LANGU_MOD | Language Key | LANG | 1 | 0 | SPRAS | SPRAS |
| 13 | LANGU_ORGNL | Language Key | LANG | 1 | 0 | SPRAS | SPRAS |
| 14 | NAME1_MOD | Name | CHAR | 40 | 0 | AD_NAME1 | TEXT40 |
| 15 | NAME1_ORGNL | Name | CHAR | 40 | 0 | AD_NAME1 | TEXT40 |
| 16 | NAME2_MOD | Name 2 | CHAR | 40 | 0 | AD_NAME2 | TEXT40 |
| 17 | NAME2_ORGNL | Name 2 | CHAR | 40 | 0 | AD_NAME2 | TEXT40 |
| 18 | NAME3_MOD | Name 3 | CHAR | 40 | 0 | AD_NAME3 | TEXT40 |
| 19 | NAME3_ORGNL | Name 3 | CHAR | 40 | 0 | AD_NAME3 | TEXT40 |
| 20 | NAME4_MOD | Name 4 | CHAR | 40 | 0 | AD_NAME4 | TEXT40 |
| 21 | NAME4_ORGNL | Name 4 | CHAR | 40 | 0 | AD_NAME4 | TEXT40 |
| 22 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 23 | OBJECTID | Object value | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 24 | PARVW | Partner Function | CHAR | 2 | 0 | PARVW | PARVW |
| 25 | POSNR | Item (SD) | NUMC | 6 | 0 | POSNR | POSNR |
| 26 | STREET_MOD | Street | CHAR | 60 | 0 | AD_STREET | TEXT60 |
| 27 | STREET_ORGNL | Street | CHAR | 60 | 0 | AD_STREET | TEXT60 |
| 28 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 29 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 30 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 31 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN | VBELN |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 31 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ADDRNUMBER_MOD** (Address number)

Address number of the modified partner address on the sales document partner record.

**ADDRNUMBER_ORGNL** (Address number)

Address number of the original customer master address used for comparison.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on UDATE

**CITY1_MOD** (City)

City on the modified partner address from the sales document.

**CITY1_ORGNL** (City)

City on the original customer master address.

**COUNTRY_MOD** (Country Key)

Country on the modified partner address from the sales document.

**COUNTRY_ORGNL** (Country Key)

Country on the original customer master address.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**KUNNR** (Customer)

Customer account and is used to scope records to specific customers across SD/FI flows.

**KUNNR_DESC** (Name)

Customer name looked up from the customer master for readable reporting.

**LANGU_MOD** (Language Key)

Language key on the modified partner address record.

**LANGU_ORGNL** (Language Key)

Language key on the original customer master address record.

**NAME1_MOD** (Name)

Name line 1 on the modified partner address from the sales document.

**NAME1_ORGNL** (Name)

Name line 1 on the original customer master address.

**NAME2_MOD** (Name 2)

Name line 2 on the modified partner address from the sales document.

**NAME2_ORGNL** (Name 2)

Name line 2 on the original customer master address.

**NAME3_MOD** (Name 3)

Name line 3 on the modified partner address from the sales document.

**NAME3_ORGNL** (Name 3)

Name line 3 on the original customer master address.

**NAME4_MOD** (Name 4)

Name line 4 on the modified partner address from the sales document.

**NAME4_ORGNL** (Name 4)

Name line 4 on the original customer master address.

**OBJECTCLAS** (Change doc. object)

Change-document object class naming which SAP business object type the change log belongs to.

**OBJECTID** (Object value)

Change-document object value identifying the changed address instance linked to the sales partner.

**PARVW** (Partner Function)

SD partner function such as sold-to, ship-to, or payer defining partner roles on documents.

**PARVW Options:**
- AG: Sold-to party
- WE: Ship-to party
- RE: Bill-to party
- RG: Payer

**POSNR** (Item (SD))

Document item number used for line-level drilldown and joins.

**STREET_MOD** (Street)

Street on the modified partner address from the sales document.

**STREET_ORGNL** (Street)

Street on the original customer master address.

**TCODE** (Transaction Code)

SAP Transaction code

**UDATE** (Date)

Update/change date used for technical recency and change-window filtering.

**USERNAME** (User)

<mark>User who posted the change.</mark>

**VBELN** (Sales Document)

SD document number used as primary key for sales/billing/delivery documents.


### Parameter Relationships

**Change-document date window:** When **DATUM** is empty, a lower bound of today minus **BACKDAYS** is applied with a greater-than-or-equal filter on **UDATE** (change-document date). Explicit **UDATE** selections override that fallback window.

**Age filter:** After rows are selected, **DURATION** with **DURATION_UNIT** measures elapsed time from each row's reference date to the evaluation date; rows outside the configured duration range are removed.

**Change-document scope:** **OBJECTCLAS**, **USERNAME**, **TCODE**, and **UDATE** restrict which address change documents are read; the code targets address object changes from sales and delivery transactions.

**Sales document linkage:** **VBELN**, **POSNR**, **PARVW**, and **KUNNR** define which sales document partners are linked to the changed address and returned in the result.

**Address comparison output:** **ADDRNUMBER_MOD** and address fields suffixed **_MOD** show the modified partner address; **ADDRNUMBER_ORGNL** and fields suffixed **_ORGNL** show the original customer master address for the same partner role.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code

### Practical Example of Parameter Configuration

**Use Case 1: Address changes in the last seven days**

**Purpose:** Review manually changed sales document addresses posted in the last seven days.

```
BACKDAYS = 7
PARVW = WE
KUNNR = 100000
```

**Use Case 2: Sold-to partner changes**

**Purpose:** Focus on sold-to party address overrides on sales orders.

```
PARVW = AG
BACKDAYS = 14
VBELN = 1000000001
```

**Use Case 3: Changes by a specific user**

**Purpose:** Audit address maintenance performed by one user across sales documents.

```
USERNAME = USER01
BACKDAYS = 30
TCODE = VA02
```

**Use Case 4: Delivery transaction changes**

**Purpose:** Monitor address changes recorded from delivery-related transactions in the last month.

```
TCODE = VL02N
BACKDAYS = 30
OBJECTCLAS = ADRESSE
```

**Use Case 5: Exactly seven full days since change date**

**Purpose:** Return rows whose change date is exactly 7 full days ago for weekly follow-up.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_ADDR_CH | ADDRNUMBER_MOD | Address number | CHAR(10) | AD_ADDRNUM |
| /SKN/S_SW_10_01_ADDR_CH | ADDRNUMBER_ORGNL | Address number | CHAR(10) | AD_ADDRNUM |
| /SKN/S_SW_10_01_ADDR_CH | CITY1_MOD | City | CHAR(40) | AD_CITY1 |
| /SKN/S_SW_10_01_ADDR_CH | CITY1_ORGNL | City | CHAR(40) | AD_CITY1 |
| /SKN/S_SW_10_01_ADDR_CH | COUNTRY_MOD | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_01_ADDR_CH | COUNTRY_ORGNL | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_01_ADDR_CH | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_01_ADDR_CH | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_01_ADDR_CH | KUNNR | Customer | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_ADDR_CH | KUNNR_DESC | Name | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_ADDR_CH | LANGU_MOD | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_10_01_ADDR_CH | LANGU_ORGNL | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_10_01_ADDR_CH | NAME1_MOD | Name | CHAR(40) | AD_NAME1 |
| /SKN/S_SW_10_01_ADDR_CH | NAME1_ORGNL | Name | CHAR(40) | AD_NAME1 |
| /SKN/S_SW_10_01_ADDR_CH | NAME2_MOD | Name 2 | CHAR(40) | AD_NAME2 |
| /SKN/S_SW_10_01_ADDR_CH | NAME2_ORGNL | Name 2 | CHAR(40) | AD_NAME2 |
| /SKN/S_SW_10_01_ADDR_CH | NAME3_MOD | Name 3 | CHAR(40) | AD_NAME3 |
| /SKN/S_SW_10_01_ADDR_CH | NAME3_ORGNL | Name 3 | CHAR(40) | AD_NAME3 |
| /SKN/S_SW_10_01_ADDR_CH | NAME4_MOD | Name 4 | CHAR(40) | AD_NAME4 |
| /SKN/S_SW_10_01_ADDR_CH | NAME4_ORGNL | Name 4 | CHAR(40) | AD_NAME4 |
| /SKN/S_SW_10_01_ADDR_CH | OBJECTCLAS | Change doc. object | CHAR(15) | CDOBJECTCL |
| /SKN/S_SW_10_01_ADDR_CH | OBJECTID | Object value | CHAR(90) | CDOBJECTV |
| /SKN/S_SW_10_01_ADDR_CH | PARVW | Partner Function | CHAR(2) | PARVW |
| /SKN/S_SW_10_01_ADDR_CH | POSNR | Item (SD) | NUMC(6) | POSNR |
| /SKN/S_SW_10_01_ADDR_CH | STREET_MOD | Street | CHAR(60) | AD_STREET |
| /SKN/S_SW_10_01_ADDR_CH | STREET_ORGNL | Street | CHAR(60) | AD_STREET |
| /SKN/S_SW_10_01_ADDR_CH | TCODE | Transaction Code | CHAR(20) | CDTCODE |
| /SKN/S_SW_10_01_ADDR_CH | UDATE | Date | DATS(8) | CDDATUM |
| /SKN/S_SW_10_01_ADDR_CH | USERNAME | User | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_01_ADDR_CH | VBELN | Sales Document | CHAR(10) | VBELN |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_ADDR_CH .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ADDR_CH OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE:
             BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD,
             LANGU  SPRAS,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT
             .
LV_BACKDAYS = 1.
LV_DATE_REF_FLD = 'UDATE'.
LV_LANGU = 'EN'.
LV_DURATION_UNIT = 'D'.
SELECT_SINGLE:
             BACKDAYS,
             DATE_REF_FLD,
             LANGU,
             DURATION_UNIT
             .
DATA_MULTY:   OBJECTCLAS   CDOBJECTCL,
              USERNAME     CDUSERNAME,
              UDATE        CDDATUM,
              TCODE        CDTCODE,
              ADDR_GROUP   CHAR4,
              ADDRNUMBER   ADRNR,
              VBPA_ADRNR   ADRNR,
              KUNNR        KUNNR,
              KUNNR2       KUNNR,
              PARVW        PARVW,
              VBELN        VBELN,
              POSNR        POSNR,
              DURATION  /SKN/E_SW_DURATION,
              DATUM    SY-DATUM
              .
SELECT_MULTY:
              OBJECTCLAS,
              USERNAME,
              UDATE,
              TCODE,
              KUNNR,
              PARVW,
              VBELN,
              POSNR,
*              ADDR_GROUP,
*              ADDRNUMBER,
              DURATION,
              DATUM
              .
FIELD-SYMBOLS:  TYPE ANY ,
               <FS_V> TYPE ANY,
               <FS_DATA> TYPE /SKN/S_SW_10_01_ADDR_CH .
DATA : SY_TABIX LIKE SY-TABIX ,
       FLD(60) TYPE C ,
       REF_DATE TYPE D.
DATA : BACKDAYS  TYPE I ,
       DATE_FROM LIKE SY-DATUM,
       TIME_DIFF TYPE  INT4  .
DATA: LT_DATA1      TYPE STANDARD TABLE OF /SKN/S_SW_10_01_ADDR_CH,
      LS_DATA1      TYPE  /SKN/S_SW_10_01_ADDR_CH,
      LT_DATA2      TYPE STANDARD TABLE OF /SKN/S_SW_10_01_ADDR_CH,
      LS_DATA2      TYPE  /SKN/S_SW_10_01_ADDR_CH,
      LT_DATA3      TYPE STANDARD TABLE OF /SKN/S_SW_10_01_ADDR_CH,
      LS_DATA3      TYPE  /SKN/S_SW_10_01_ADDR_CH,
      LS_DATA       TYPE  /SKN/S_SW_10_01_ADDR_CH,
      LT_CDHDR      TYPE  STANDARD TABLE OF CDHDR,
      LS_CDHDR      TYPE  CDHDR,
      LT_VBPA       TYPE  STANDARD TABLE OF VBPA,
      LT_VBPA2      TYPE  STANDARD TABLE OF VBPA,
      LS_VBPA2      TYPE  VBPA,
      LT_KUNNR2     TYPE  STANDARD TABLE OF VBPA,
      LS_KUNNR2     TYPE  VBPA,
      LS_VBPA       TYPE  VBPA
      .
IF R_DATUM[] IS INITIAL .
   RS_DATUM-SIGN   = 'I' .
   RS_DATUM-OPTION = 'GE' .
   DATE_FROM       = SY-DATUM - LV_BACKDAYS .
   RS_DATUM-LOW    = DATE_FROM .
   APPEND RS_DATUM TO R_DATUM.
ENDIF.
*  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_ADDR_CH'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
*  "--- Run Cloud Mode -----
 "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'UDATE'.
      R_UDATE[] = R_DATUM[]. "Creation date of the change document
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT.
  REFRESH T_DATA.
" ----  Get changes in CDHDR ----
  "SELECT DISTINCT OBJECTID
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_CDHDR
    FROM CDHDR AS C
    WHERE C~OBJECTCLAS = 'ADRESSE'
      AND ( TCODE LIKE 'VA%' OR TCODE LIKE 'VL%' )
      AND C~UDATE IN R_UDATE
      AND C~OBJECTCLAS IN R_OBJECTCLAS
      AND C~USERNAME   IN R_USERNAME
      AND C~TCODE      IN R_TCODE
    .
    SORT LT_CDHDR BY OBJECTID.
    IF R_UDATE IS NOT INITIAL.
      DELETE LT_CDHDR WHERE UDATE NOT IN R_UDATE.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM LT_CDHDR COMPARING OBJECTID.
" ----  Split address and fill relevant ranges ----
  LOOP AT LT_CDHDR INTO LS_CDHDR.
   RS_ADDR_GROUP-SIGN   = 'I' .
   RS_ADDR_GROUP-OPTION = 'EQ' .
   RS_ADDR_GROUP-LOW    =  LS_CDHDR-OBJECTID(4) .
   APPEND RS_ADDR_GROUP TO R_ADDR_GROUP.
   RS_ADDRNUMBER-SIGN   = 'I' .
   RS_ADDRNUMBER-OPTION = 'EQ' .
   RS_ADDRNUMBER-LOW    =  LS_CDHDR-OBJECTID+4(86) .
   APPEND RS_ADDRNUMBER TO R_ADDRNUMBER.
  ENDLOOP.
  SORT R_ADDR_GROUP BY LOW ASCENDING.
  SORT R_ADDRNUMBER BY LOW ASCENDING.
  DELETE ADJACENT DUPLICATES FROM R_ADDR_GROUP COMPARING LOW.
  DELETE ADJACENT DUPLICATES FROM R_ADDRNUMBER COMPARING LOW.
  "modified  & original addresses
  SELECT A~ADDRNUMBER AS ADDRNUMBER_MOD
       A~NAME1 AS NAME1_MOD
       A~NAME2 AS NAME2_MOD
       A~NAME3 AS NAME3_MOD
       A~NAME4 AS NAME4_MOD
       A~STREET AS STREET_MOD
       A~COUNTRY AS COUNTRY_MOD
       A~LANGU AS LANGU_MOD
       A2~ADDRNUMBER AS ADDRNUMBER_ORGNL
       A2~NAME1 AS NAME1_ORGNL
       A2~NAME2 AS NAME2_ORGNL
       A2~NAME3 AS NAME3_ORGNL
       A2~NAME4 AS NAME4_ORGNL
       V~VBELN
       V~POSNR
       V~KUNNR
       V~PARVW
       K~KUNNR
  INTO CORRESPONDING FIELDS OF TABLE LT_DATA2
  FROM ADRC AS A
  INNER JOIN VBPA AS V
   ON A~ADDRNUMBER = V~ADRNR
  INNER JOIN KNA1 AS K
   ON V~KUNNR = K~KUNNR
  INNER JOIN ADRC AS A2
   ON A2~ADDRNUMBER = K~ADRNR
  WHERE
          V~ADRNR IN R_ADDRNUMBER
      AND V~ADRDA IN ('B', 'C', 'E', 'F') " temporarily commented out to bring any data from SKD
      AND V~KUNNR IS NOT NULL
      AND ( K~XCPDK IS NULL  OR K~XCPDK EQ '' ) " temporarily commented out to bring any data from SKD
      AND K~KUNNR IN R_KUNNR
      .
 SORT LT_DATA2 BY VBELN PARVW KUNNR.
" Unite address data with fields from CDHDR
 LOOP AT LT_DATA2 INTO LS_DATA2.
   READ TABLE LT_CDHDR INTO LS_CDHDR
    WITH KEY OBJECTID+4(86) = LS_DATA2-ADDRNUMBER_MOD
    BINARY SEARCH.
   IF SY-SUBRC = 0.
     LS_DATA2-OBJECTID    = LS_CDHDR-OBJECTID.
     LS_DATA2-OBJECTCLAS  = LS_CDHDR-OBJECTCLAS.
     LS_DATA2-USERNAME    = LS_CDHDR-USERNAME.
     LS_DATA2-UDATE       = LS_CDHDR-UDATE.
     LS_DATA2-TCODE       = LS_CDHDR-TCODE.
   ENDIF.
   APPEND LS_DATA2 TO LT_DATA3.
 ENDLOOP.
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  READ TABLE LT_DATA3 INDEX 1 TRANSPORTING NO FIELDS.
  CHECK NOT SY-TFILL  IS INITIAL .
  LOOP AT LT_DATA3 ASSIGNING <FS_DATA>.
   SY_TABIX = SY-TABIX .
   FLD = LV_DATE_REF_FLD.
   ASSIGN COMPONENT FLD OF STRUCTURE <FS_DATA> TO .
   REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      <FS_DATA>-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          <FS_DATA>-DURATION  = TIME_DIFF .
        ELSE.
          <FS_DATA>-DURATION  = '999999'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE LT_DATA3 WHERE DURATION  NOT IN R_DURATION .
 "--- Get Decriptions
 " --- Populate global tables for descriprions ---
  "Customer description - popuplate list of KUNNR in the global table
  SELECT *
  FROM KNA1 AS K
  INTO CORRESPONDING FIELDS OF TABLE  GT_KNA1
  FOR ALL ENTRIES IN LT_DATA3
  WHERE K~KUNNR = LT_DATA3-KUNNR.
  SORT GT_KNA1 BY KUNNR.
    "--- Get  Customer Decriptions  KUNNR
  LOOP AT LT_DATA3 INTO LS_DATA3.
   IF LS_DATA3-KUNNR IS NOT INITIAL.
     CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR                = LS_DATA3-KUNNR
      IMPORTING
        CUST_DESC            = LS_DATA3-KUNNR_DESC
      EXCEPTIONS
        WRONG_CUSTOMER       = 1
        OTHERS               = 2              .
     IF SY-SUBRC <> 0.
     ENDIF.
   ENDIF.
   APPEND LS_DATA3 TO T_DATA.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
