# Exception Indicator: Comparison month sales(purch) volume by cust(vend) - SW_10_07_CM_MN_VL_CV

## General Overview

This Exception Indicator (EI) compares fiscal-period sales or purchase totals between a target month and a comparison month derived from BACKMONTHS and COMPMONTHS, for either customers or vendors depending on DC_IND.

This EI helps by:
- Stepping calendar months backward to anchor target and comparison dates before fiscal conversion
- Pulling twelve monthly UM buckets from KNC1 or LFC1 for the two fiscal years in scope
- Computing period-over-period percentage change and enriching rows with account and company descriptions

The function resolves fiscal year and posting period per company, reads transaction figures, and raises an alert when result data remains populated after processing.


## Problem Description

Large unexplained swings in monthly customer or vendor transaction totals can hide master-data issues, pricing errors, or process drift until period closing, especially when year-over-year context is not reviewed systematically.

**Operational and Process Risks**
- Month-pair selection errors can misstate true year-over-year movement
- Mixing debtor and creditor paths can confuse which master tables were evaluated
- Missing transaction figures silently drop accounts from the result set

**Control and Compliance Risks**
- Variance evidence may be challenged if fiscal period resolution per company code is unclear
- Block and deletion flags need consistent treatment across customer and vendor branches

**Management Visibility Risks**
- Without automated comparison, leadership may lack ranked visibility into largest relative changes

### Suggested Resolution

**Immediate Response**
- Review rows with the largest absolute PERC_VARI together with UMXXU_TGT and UMXXU_CMP
- Confirm DC_IND matches the intended customer versus vendor evaluation path

**System Assessment**
- Validate BACKMONTHS and COMPMONTHS against the intended calendar story for target and comparison months
- Reconcile fiscal year and posting period fields (GJAHR_TGT, MONAT_TGT, GJAHR_CMP, MONAT_CMP) with the corporate calendar

**Corrective Actions**
- Standardize monitoring defaults for month offsets and variance thresholds
- Tighten master-data governance for account groups in recurring alert populations
- Document how DURATION and DURATION_UNIT interact with external selection when time windows are applied


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACCT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 2 | ACCT_GRP | Account group | CHAR | 4 | 0 | /ABC4C/E_SW_ACCT_GRP | CHAR4 |
| 3 | ACCT_NUM | Account Number | CHAR | 10 | 0 | /ABC4C/E_SW_ACCT_NUM | CHAR10 |
| 4 | BACKMONTHS | Months Backwards | NUMC | 2 | 0 | /ABC4C/E_SW_BACKMONTHS | MONAT |
| 5 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 6 | COMPMONTHS | Month to Compare (count from BACKMONTH) | NUMC | 2 | 0 | /ABC4C/E_SW_COMPMONTHS | MONAT |
| 7 | COMP_CODE_DESC | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 8 | DC_IND | Debtor/Creditor Indicator | CHAR | 1 | 0 | /ABC4C/E_SW_DC_IND | CHAR1 |
| 9 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 11 | GJAHR_CMP | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 12 | GJAHR_TGT | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 13 | KNA1_CASSD | Central sales block | CHAR | 2 | 0 | CASSD_X | CASSP |
| 14 | KNA1_LOEVM | Central deletion flag | CHAR | 1 | 0 | LOEVM_X | XFELD |
| 15 | KNA1_SPERR | Central posting block | CHAR | 1 | 0 | SPERB_X | XFELD |
| 16 | KNB1_LOEVM | Deletion flag for company code | CHAR | 1 | 0 | LOEVM_B | XFELD |
| 17 | KNB1_SPERR | Posting block for company code | CHAR | 1 | 0 | SPERB_B | XFELD |
| 18 | LFA1_LOEVM | Central deletion flag | CHAR | 1 | 0 | LOEVM_X | XFELD |
| 19 | LFA1_SPERM | Central purchasing block | CHAR | 1 | 0 | SPERM_X | XFELD |
| 20 | LFA1_SPERR | Central posting block | CHAR | 1 | 0 | SPERB_X | XFELD |
| 21 | LFB1_LOEVM | Deletion flag for company code | CHAR | 1 | 0 | LOEVM_B | XFELD |
| 22 | LFB1_SPERR | Posting block for company code | CHAR | 1 | 0 | SPERB_B | XFELD |
| 23 | MONAT_CMP | Posting period | NUMC | 2 | 0 | MONAT | MONAT |
| 24 | MONAT_TGT | Posting period | NUMC | 2 | 0 | MONAT | MONAT |
| 25 | PERC_VARI | Percent vari. of fiscal period tran vol. | DEC | 6 | 2 | /ABC4C/E_SW_PERC_VARI |  |
| 26 | UMXXU_CMP | Sales | CURR | 15 | 2 | UMXXU | UMXXX |
| 27 | UMXXU_TGT | Sales | CURR | 15 | 2 | UMXXU | UMXXX |
| 28 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 28 parameters listed in the Parameters Reference Table above.

**ACCT_DESC** (Name):

ACCT_DESC is filled on each result line with the resolved customer or vendor short text so operational readers see the business name beside numeric period comparisons.

**ACCT_GRP** (Account group):

ACCT_GRP filters master-data extraction to the account-group values relevant for either customer (KTOKD) or vendor (KTOKK) processing depending on the active DC_IND path.

**ACCT_NUM** (Account Number):

ACCT_NUM narrows the population to explicit customer or vendor numbers when the job should not evaluate the full range allowed by company and group filters alone.

**BACKMONTHS** (Months Backwards):

BACKMONTHS controls how many month boundaries the routine steps backward from the evaluation anchor date to reach the target month whose fiscal period drives UMXXU_TGT selection.

**BUKRS** (Company Code):

BUKRS scopes both master-data selection and fiscal-calendar calls so each row’s posting-period resolution uses the correct company-specific fiscal variant.

**COMPMONTHS** (Month to Compare (count from BACKMONTH)):

COMPMONTHS defines how many additional month steps are taken backward from the target month to position the comparison month whose fiscal year supplies UMXXU_CMP.

**COMP_CODE_DESC** (Company Name):

COMP_CODE_DESC is enriched from company-code master data so multi-company result sets remain interpretable without looking up BUKRS codes separately.

**DC_IND** (Debtor/Creditor Indicator):

DC_IND selects whether the program reads customer masters with KNC1 totals or vendor masters with LFC1 totals while keeping the same month-pair comparison mechanics.

**DC_IND Options:**
- **C**: Creditor mode — vendor master (LFA1/LFB1) and vendor transaction figures (LFC1)
- **D**: Debtor mode — customer master (KNA1/KNB1) and customer transaction figures (KNC1)

**DURATION** (Duration In Time Units):

DURATION supplies the numeric width for optional duration-based selection when the surrounding Skywatch layer applies posting-date windows together with DURATION_UNIT semantics.

**DURATION_UNIT** (Duration Unit):

DURATION_UNIT tells the selection layer whether DURATION counts hours, days, or other supported calendar units so time filters stay unambiguous across landscapes.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**GJAHR_CMP** (Fiscal Year):

GJAHR_CMP stores the fiscal year returned for the comparison anchor date and keys the LFC1 or KNC1 row whose monthly UM columns feed UMXXU_CMP.

**GJAHR_TGT** (Fiscal Year):

GJAHR_TGT stores the fiscal year returned for the target anchor month and aligns dynamic access to the correct UM##U bucket for UMXXU_TGT.

**KNA1_CASSD** (Central sales block):

KNA1_CASSD applies the central customer sales-area block indicator when DC_IND runs the debtor branch so centrally sales-blocked accounts follow your inclusion policy.

**KNA1_LOEVM** (Central deletion flag):

KNA1_LOEVM filters centrally flagged customer deletions on the debtor path so deleted customer masters do not skew month-to-month sales variance results.

**KNA1_SPERR** (Central posting block):

KNA1_SPERR applies the central customer posting block so FI-frozen customers can be excluded from or included in the comparison cohort deliberately.

**KNB1_LOEVM** (Deletion flag for company code):

KNB1_LOEVM applies company-code-level customer deletion on KNB1 rows paired to each KUNNR and BUKRS combination selected for analysis.

**KNB1_SPERR** (Posting block for company code):

KNB1_SPERR applies company-code posting block status for customers so entity-specific FI restrictions are visible in the filtered account list.

**LFA1_LOEVM** (Central deletion flag):

LFA1_LOEVM applies central vendor deletion filtering when DC_IND is creditor mode so removed vendor masters do not appear in purchase-volume comparisons.

**LFA1_SPERM** (Central purchasing block):

LFA1_SPERM applies the central purchasing block on vendors so procurement-suspended suppliers can be separated from active sourcing comparisons on the creditor path.

**LFA1_SPERR** (Central posting block):

LFA1_SPERR applies the central vendor posting block so centrally FI-blocked vendors are handled consistently with your exception population rules.

**LFB1_LOEVM** (Deletion flag for company code):

LFB1_LOEVM applies vendor company-code deletion on LFB1 relative to each vendor and company pair pulled from the joint LFA1/LFB1 selection.

**LFB1_SPERR** (Posting block for company code):

LFB1_SPERR applies vendor company-code posting block filtering so company-local FI restrictions complement central vendor flags in the result set.

**MONAT_CMP** (Posting period):

MONAT_CMP records the fiscal period index resolved for the comparison month and determines which UM##U component is read from the comparison fiscal year row.

**MONAT_TGT** (Posting period):

MONAT_TGT records the fiscal period index resolved for the target month and drives the dynamic field name used to move the correct monthly total into UMXXU_TGT.

**PERC_VARI** (Percent vari. of fiscal period tran vol.):

PERC_VARI is the configured variance control read from selection; the routine also overwrites the result field with the computed percentage change when both monthly totals are non-zero.

**UMXXU_CMP** (Sales):

UMXXU_CMP carries the monetary posting-period total taken from the comparison fiscal year row for the fiscal period in MONAT_CMP.

**UMXXU_TGT** (Sales):

UMXXU_TGT carries the monetary posting-period total taken from the target fiscal year row for the fiscal period in MONAT_TGT and forms the primary numerator for variance review.

**WAERS** (Currency):

WAERS propagates the company-code currency from T001 so monetary columns on each line share a consistent currency key for reporting.


### Parameter Relationship

How parameter combinations work together

BACKMONTHS positions the target month by rewinding whole month boundaries from the evaluation anchor, and COMPMONTHS then rewinds further month boundaries from that target to reach the comparison month whose fiscal period supplies the baseline UM total.

DC_IND must stay aligned with ACCT_GRP, ACCT_NUM, and the block or deletion flags: creditor mode drives LFA1, LFB1, and LFC1, while debtor mode drives KNA1, KNB1, and KNC1 with the same month-pair mechanics.

BUKRS ties each row to a company code so fiscal calendar resolution and T001 currency lookup use the correct variant and WAERS for that entity.

DURATION together with DURATION_UNIT is only meaningful when the surrounding selection framework applies a posting-reference time window; it does not replace the BACKMONTHS and COMPMONTHS month-step logic inside this function.


### Default Values

- **BACKMONTHS** - 1 (code assigns one month backward from the anchor before fiscal conversion)
- **COMPMONTHS** - 12 (code assigns twelve month steps from the target month for the comparison anchor)
- **DC_IND** - D (code default debtor branch; use C for creditor/vendor branch)
- **DURATION** - initial — not set in the excerpted ABAP; remains type-initial until the selection layer supplies a duration for time-window filtering
- **DURATION_UNIT** - H

### Practical Example of Parameter Configuration

**Use Case 1: Customer debtor analysis with variance threshold**

**Purpose:** Compare current-month customer sales to the year-ago month for selected company codes and require a minimum configured variance.

```
DC_IND = D
BUKRS = 1000 / 3000
BACKMONTHS = 1
COMPMONTHS = 12
PERC_VARI = 20
DURATION = 0
DURATION_UNIT = D
```

**Use Case 2: Creditor vendor population with master hygiene filters**

**Purpose:** Evaluate vendors for purchase-volume movement while excluding centrally deleted or blocked vendors.

```
DC_IND = C
BUKRS = 1000
LFA1_LOEVM = 
LFB1_LOEVM = 
LFA1_SPERR = 
BACKMONTHS = 2
COMPMONTHS = 12
PERC_VARI = 35
```

**Use Case 3: Narrowed account list with account group scoping**

**Purpose:** Focus on a handful of trading partners by explicit account numbers and groups under one company code.

```
DC_IND = D
ACCT_NUM = 0000100001 / 0000100002
ACCT_GRP = KUNA / KUNB
BUKRS = 2000
KNA1_SPERR = 
BACKMONTHS = 1
COMPMONTHS = 12
PERC_VARI = 15
```


## EI Function Structure

This table lists all output fields returned by the EI.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | ACCT_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | ACCT_GRP | Account group | CHAR(4) | /ABC4C/E_SW_ACCT_GRP |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | ACCT_NUM | Account Number | CHAR(10) | /ABC4C/E_SW_ACCT_NUM |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | BACKMONTHS | Months Backwards | NUMC(2) | /ABC4C/E_SW_BACKMONTHS |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | BUKRS | Company Code | CHAR(4) | BUKRS |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | COMPMONTHS | Month to Compare (count from BACKMONTH) | NUMC(2) | /ABC4C/E_SW_COMPMONTHS |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | COMP_CODE_DESC | Name of Company Code or Company | CHAR(25) | BUTXT |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | DC_IND | Debtor/Creditor Indicator | CHAR(1) | /ABC4C/E_SW_DC_IND |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | GJAHR_CMP | Fiscal Year | NUMC(4) | GJAHR |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | GJAHR_TGT | Fiscal Year | NUMC(4) | GJAHR |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | KNA1_CASSD | Central sales block for customer | CHAR(2) | CASSD_X |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | KNA1_LOEVM | Central Deletion Flag for Master Record | CHAR(1) | LOEVM_X |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | KNA1_SPERR | Central posting block | CHAR(1) | SPERB_X |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | KNB1_LOEVM | Deletion Flag for Master Record (Company Code Level) | CHAR(1) | LOEVM_B |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | KNB1_SPERR | Posting block for company code | CHAR(1) | SPERB_B |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | LFA1_LOEVM | Central Deletion Flag for Master Record | CHAR(1) | LOEVM_X |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | LFA1_SPERM | Centrally imposed purchasing block | CHAR(1) | SPERM_X |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | LFA1_SPERR | Central posting block | CHAR(1) | SPERB_X |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | LFB1_LOEVM | Deletion Flag for Master Record (Company Code Level) | CHAR(1) | LOEVM_B |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | LFB1_SPERR | Posting block for company code | CHAR(1) | SPERB_B |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | MONAT_CMP | Fiscal Period | NUMC(2) | MONAT |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | MONAT_TGT | Fiscal Period | NUMC(2) | MONAT |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | PERC_VARI | Percent variance of fiscal period transaction volumes | DEC(6,2) | /ABC4C/E_SW_PERC_VARI |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | UMXXU_CMP | Sales in the Posting Period | CURR(15,2) | UMXXU |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | UMXXU_TGT | Sales in the Posting Period | CURR(15,2) | UMXXU |
| /ABC4C/S_SW_10_07_CM_MN_VL_CV | WAERS | Currency Key | CUKY(5) | WAERS |

## ABAP Code

```abap
FUNCTION ZABC4C_F_SW_10_07_CM_MN_VL_CV.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /ABC4C/S_SW_10_07_CM_MN_VL_CV OPTIONAL
*"--------------------------------------------------------------------
*-----------------------------------------------------------------------
*  Date(dd-mmm-yyyy): 29-Jul-2019
*  Author           : Taotsu Nakamura - 50023710
*-----------------------------------------------------------------------
*  Alert Definition : This alert detects exceptional sales(purchase) volume increase (decrease)
*                     by comparing fiscal periods between current and last year.
*                     It could be checked on customer(vendor) level.
*-----------------------------------------------------------------------
*  CHANGE HISTORY
* ----------------------------------------------------------------------
*  DATE(DMY)     TP Request#   Programmer   Description
*  29-Jul-2019   SD3K900118    50023710     Create initial version
*  04-Aug-2019   SD3K900124    50023710     Correction by QA process
* ----------------------------------------------------------------------
* ----------------------------------------------------------------------
* Local Type definition
* ----------------------------------------------------------------------
  TYPES:
    BEGIN OF TYP_FIG,
      ACCT_NUM   TYPE /ABC4C/E_SW_ACCT_NUM,         " Account Number
      BUKRS      TYPE BUKRS,                        " Company Code
      GJAHR      TYPE GJAHR,                        " Fiscal Year
      UM01U      TYPE UMXXU,                        " Sales in the Posting Period
      UM02U      TYPE UMXXU,                        " Sales in the Posting Period
      UM03U      TYPE UMXXU,                        " Sales in the Posting Period
      UM04U      TYPE UMXXU,                        " Sales in the Posting Period
      UM05U      TYPE UMXXU,                        " Sales in the Posting Period
      UM06U      TYPE UMXXU,                        " Sales in the Posting Period
      UM07U      TYPE UMXXU,                        " Sales in the Posting Period
      UM08U      TYPE UMXXU,                        " Sales in the Posting Period
      UM09U      TYPE UMXXU,                        " Sales in the Posting Period
      UM10U      TYPE UMXXU,                        " Sales in the Posting Period
      UM11U      TYPE UMXXU,                        " Sales in the Posting Period
      UM12U      TYPE UMXXU,                        " Sales in the Posting Period
    END   OF TYP_FIG.
* ----------------------------------------------------------------------
* Local Data definition
* ----------------------------------------------------------------------
* - single DATA
  DATA_SINGLE:
    LANGU                LANGU,                    " Language (not in use)
    DATUM                SY-DATUM,                 " System Date
    TARGET               SY-DATUM,                 " Target Date
    COMPARE              SY-DATUM,                 " Date to Compare
    FNAME                FNAME,                    " Field name
    TABIX                SY-TABIX                  " table index
    .
* - range DATA
*  data_multy:
*    datum                datum,                    " Date
*    budat                budat,                    " Posting Date
*    aedat                aedat,                    " Date on Which Record Was Created
*    cpudt                cpudt,                    " Day On Which Accounting Document Was Entered
*    upddt                upddt,                    " Date of the Last Document Update
*    bldat                bldat                     " Document Date
*    .
  DATA:
    LS_DATA              TYPE /ABC4C/S_SW_10_07_CM_MN_VL_CV,
    LS_FIG               TYPE TYP_FIG,
    LT_FIG               TYPE TABLE OF TYP_FIG
    .
  FIELD-SYMBOLS:
    <FS_DATA>            TYPE /ABC4C/S_SW_10_07_CM_MN_VL_CV,
    <FS_UMXXU>           TYPE UMXXU
    .
* ----------------------------------------------------------------------
* Parameters Definition
* ----------------------------------------------------------------------
* Define Special Parameters
* - single DATA
  DATA_SINGLE:
    SW_DEST              RFCDEST,                  " RFC destination
    BACKDAYS             INT4,                     " BACKDAYS
    DATE_REF_FLD         NAME_FELD,                " DATE_REF_FLD
    DURATION_UNIT        /SKN/E_SW_DURATION_UNIT   " Duration unit
    .
* - range DATA
  DATA_MULTY:
    DURATION             /SKN/E_SW_DURATION        " Duration
    .
* Define EI specific Parameters
* - single DATA
  DATA_SINGLE:
    BACKMONTHS           /ABC4C/E_SW_BACKMONTHS,   " Months Backwards
    COMPMONTHS           /ABC4C/E_SW_COMPMONTHS,   " Months to Compare
    DC_IND               /ABC4C/E_SW_DC_IND,       " Debtor/Creditor Indicator
    PERC_VARI            /ABC4C/E_SW_PERC_VARI     " Percent Variance of fiscal period transaction volumes
    .
* - range DATA
  DATA_MULTY:
    ACCT_GRP             /ABC4C/E_SW_ACCT_GRP,     " Account Group
    ACCT_NUM             /ABC4C/E_SW_ACCT_NUM,     " Account Number
    BUKRS                BUKRS,                    " Company Code
    KNA1_LOEVM           LOEVM_X,                  " Central deletion flag
    KNA1_CASSD           CASSD_X,                  " Central sales block
    KNA1_SPERR           SPERB_X,                  " Central posting block
    KNB1_LOEVM           LOEVM_B,                  " Delete flag for company code
    KNB1_SPERR           SPERB_B,                  " Posting block for company code
    LFA1_LOEVM           LOEVM_X,                  " Central deletion flag
    LFA1_SPERM           CASSD_X,                  " Central purchasing block
    LFA1_SPERR           SPERB_X,                  " Central posting block
    LFB1_LOEVM           LOEVM_B,                  " Delete flag for company code
    LFB1_SPERR           SPERB_B                   " Posting block for company code
    .
* ----------------------------------------------------------------------
* Extracting parameters’ value and populating variables
* ----------------------------------------------------------------------
* Set initial value
  LV_DATUM               = SY-DATUM." System date
  LV_LANGU               = 'E'.     " English
  LV_DATE_REF_FLD        = 'BUDAT'. " Document date
  LV_BACKMONTHS          = 1.       " Months Backwards
  LV_COMPMONTHS          = 12.      " Months to Compare
  LV_DC_IND              = 'D'.     " Debtor/Creditor Indicator
* Extract Special Parameters
* - single value
  SELECT_SINGLE:
    SW_DEST,                        " RFC destination
    BACKDAYS,                       " BACKDAYS
    DATE_REF_FLD,                   " DATE_REF_FLD
    DURATION_UNIT                   " Duration unit
    .
* - range value
  SELECT_MULTY:
    DURATION                        " Duration
    .
* Extract EI specific Parameters
* - single value
  SELECT_SINGLE:
    BACKMONTHS,                     " Months Backwards
    COMPMONTHS,                     " Months to Compare
    DC_IND,                         " Debtor/Creditor Indicator
    PERC_VARI                       " Percent Variance of fiscal period transaction volumes
    .
* - range value
  SELECT_MULTY:
    ACCT_GRP,                       " Account Group
    ACCT_NUM,                       " Account Number
    BUKRS,                          " Company Code
    KNA1_LOEVM,                     " Central deletion flag
    KNA1_CASSD,                     " Central sales block
    KNA1_SPERR,                     " Central posting block
    KNB1_LOEVM,                     " Delete flag for company code
    KNB1_SPERR,                     " Posting block for company code
    LFA1_LOEVM,                     " Central deletion flag
    LFA1_SPERM,                     " Central purchasing block
    LFA1_SPERR,                     " Central posting block
    LFB1_LOEVM,                     " Delete flag for company code
    LFB1_SPERR                      " Posting block for company code
    .
* ----------------------------------------------------------------------
* Initiating
* ----------------------------------------------------------------------
  CLEAR:
    IS_ALERT
    .
  REFRESH:
    T_DATA,
    LT_FIG
    .
* ----------------------------------------------------------------------
* Retrieving alert data
* ----------------------------------------------------------------------
  "--- Run Cloud Mode -----
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION 'ZABC4C_FC_SW_10_07_CM_MN_VL_CV'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
* Calculate the Target Date from BACKMONTHS parameter.
  LV_TARGET = LV_DATUM.
  LV_TARGET+6(2) = '01'. " Get the first day of this month
  DO LV_BACKMONTHS TIMES.
    LV_TARGET = LV_TARGET - 1.
    LV_TARGET+6(2) = '01'.
  ENDDO.
* Calculate the Date to Cpmpare from COMPMONTHS parameter.
  LV_COMPARE = LV_TARGET.
  DO LV_COMPMONTHS TIMES.
    LV_COMPARE = LV_COMPARE - 1.
    LV_COMPARE+6(2) = '01'.
  ENDDO.
  IF LV_DC_IND = 'C'.
*   Extract vendor master records from Vendor Master (LFA1/LFB1).
    SELECT
            A~LIFNR AS ACCT_NUM
            A~KTOKK AS ACCT_GRP
            A~LOEVM AS LFA1_LOEVM
            A~SPERM AS LFA1_SPERM
            A~SPERR AS LFA1_SPERR
            B~BUKRS
            B~LOEVM AS LFB1_LOEVM
            B~SPERR AS LFB1_SPERR
            C~WAERS
      INTO CORRESPONDING FIELDS OF TABLE T_DATA
      FROM LFA1 AS A
      INNER JOIN LFB1 AS B ON A~LIFNR EQ B~LIFNR
      INNER JOIN T001 AS C ON B~BUKRS EQ C~BUKRS
      WHERE A~LIFNR  IN R_ACCT_NUM
      AND   A~KTOKK  IN R_ACCT_GRP
      AND   B~BUKRS  IN R_BUKRS
      AND   A~SPERR  IN R_LFA1_SPERR
      AND   A~SPERM  IN R_LFA1_SPERM
      AND   A~LOEVM  IN R_LFA1_LOEVM
      AND   B~SPERR  IN R_LFB1_SPERR
      AND   B~LOEVM  IN R_LFB1_LOEVM
      .
  ELSE.
*   Extract customer master records from Customer Master (KNA1/KNB1).
    SELECT
            A~KUNNR AS ACCT_NUM
            A~KTOKD AS ACCT_GRP
            A~LOEVM AS KNA1_LOEVM
            A~CASSD AS KNA1_CASSD
            A~SPERR AS KNA1_SPERR
            B~BUKRS
            B~LOEVM AS KNB1_LOEVM
            B~SPERR AS KNB1_SPERR
            C~WAERS
      INTO CORRESPONDING FIELDS OF TABLE T_DATA
      FROM KNA1 AS A
      INNER JOIN KNB1 AS B ON A~KUNNR EQ B~KUNNR
      INNER JOIN T001 AS C ON B~BUKRS EQ C~BUKRS
      WHERE A~KUNNR  IN R_ACCT_NUM
      AND   A~KTOKD  IN R_ACCT_GRP
      AND   B~BUKRS  IN R_BUKRS
      AND   A~SPERR  IN R_KNA1_SPERR
      AND   A~CASSD  IN R_KNA1_CASSD
      AND   A~LOEVM  IN R_KNA1_LOEVM
      AND   B~SPERR  IN R_KNB1_SPERR
      AND   B~LOEVM  IN R_KNB1_LOEVM
      .
  ENDIF.
*<<< If there is no Account Maste Data, the processing is terminated
  CHECK T_DATA[] IS NOT INITIAL.
* Calculate the fiscal year and fiscal period of Target Date
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
*   Get fiscal year: Target Date
    CALL FUNCTION '/ABC4C/F_SW_10_GET_CUR_YEAR'
      EXPORTING
        BUKRS             = <FS_DATA>-BUKRS
        DATUM             = LV_TARGET
      IMPORTING
        MONAT             = <FS_DATA>-MONAT_TGT
        GJAHR             = <FS_DATA>-GJAHR_TGT
      EXCEPTIONS
        WRONG_CODE        = 1
        WRONG_VALUE       = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0 OR <FS_DATA>-GJAHR_TGT IS INITIAL.
      DELETE T_DATA INDEX SY-TABIX.
      CONTINUE.
    ENDIF.
*   Get fiscal year: Date to Compare
    CALL FUNCTION '/ABC4C/F_SW_10_GET_CUR_YEAR'
      EXPORTING
        BUKRS             = <FS_DATA>-BUKRS
        DATUM             = LV_COMPARE
      IMPORTING
        MONAT             = <FS_DATA>-MONAT_CMP
        GJAHR             = <FS_DATA>-GJAHR_CMP
      EXCEPTIONS
        WRONG_CODE        = 1
        WRONG_VALUE       = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0 OR <FS_DATA>-GJAHR_CMP IS INITIAL.
      DELETE T_DATA INDEX SY-TABIX.
      CONTINUE.
    ENDIF.
    <FS_DATA>-BACKMONTHS = LV_BACKMONTHS.
    <FS_DATA>-COMPMONTHS = LV_COMPMONTHS.
    <FS_DATA>-DC_IND     = LV_DC_IND.
  ENDLOOP.
  IF LV_DC_IND = 'C'.
*   Extract vendor transaction figures from Vendor Master (Transaction Figures) (LFC1)
    SELECT LIFNR AS ACCT_NUM BUKRS GJAHR UM01U UM02U UM03U UM04U UM05U UM06U UM07U UM08U UM09U UM10U UM11U UM12U
      INTO CORRESPONDING FIELDS OF TABLE LT_FIG
      FROM LFC1
      FOR ALL ENTRIES IN T_DATA
      WHERE LIFNR = T_DATA-ACCT_NUM
      AND   BUKRS = T_DATA-BUKRS
      AND ( GJAHR = T_DATA-GJAHR_TGT OR GJAHR = T_DATA-GJAHR_CMP ).
    SORT LT_FIG BY ACCT_NUM BUKRS GJAHR.
  ELSE.
*   Extract customer transaction figures from Customer Master (Transaction Figures) (KNC1)
    SELECT KUNNR AS ACCT_NUM BUKRS GJAHR UM01U UM02U UM03U UM04U UM05U UM06U UM07U UM08U UM09U UM10U UM11U UM12U
      INTO CORRESPONDING FIELDS OF TABLE LT_FIG
      FROM KNC1
      FOR ALL ENTRIES IN T_DATA
      WHERE KUNNR = T_DATA-ACCT_NUM
      AND   BUKRS = T_DATA-BUKRS
      AND ( GJAHR = T_DATA-GJAHR_TGT OR GJAHR = T_DATA-GJAHR_CMP ).
  ENDIF.
*<<< If there is no Figure Data, the processing is terminated
  CHECK LT_FIG[] IS NOT INITIAL.
  SORT LT_FIG BY ACCT_NUM BUKRS GJAHR.
* ----------------------------------------------------------------------
* Post retrieving manipulations
* ----------------------------------------------------------------------
*	Detect the customers(vendors) whose variances exceed the threshold(percentage) from the comparison of fiscal period transaction volumes.
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    LV_TABIX = SY-TABIX.
    CLEAR LS_FIG.
*   Get target amount
    READ TABLE LT_FIG INTO LS_FIG WITH KEY ACCT_NUM = <FS_DATA>-ACCT_NUM
                                           BUKRS    = <FS_DATA>-BUKRS
                                           GJAHR    = <FS_DATA>-GJAHR_TGT
                                           BINARY SEARCH.
    IF SY-SUBRC = 0.
      CONCATENATE 'UM' <FS_DATA>-MONAT_TGT 'U' INTO LV_FNAME.
      ASSIGN COMPONENT LV_FNAME OF STRUCTURE LS_FIG TO <FS_UMXXU>.
      <FS_DATA>-UMXXU_TGT = <FS_UMXXU>.
    ELSE.
      DELETE T_DATA INDEX LV_TABIX.
      CONTINUE.
    ENDIF.
    CLEAR LS_FIG.
*   Get compare amount
    READ TABLE LT_FIG INTO LS_FIG WITH KEY ACCT_NUM = <FS_DATA>-ACCT_NUM
                                           BUKRS    = <FS_DATA>-BUKRS
                                           GJAHR    = <FS_DATA>-GJAHR_CMP
                                           BINARY SEARCH.
    IF SY-SUBRC = 0.
      CONCATENATE 'UM' <FS_DATA>-MONAT_CMP 'U' INTO LV_FNAME.
      ASSIGN COMPONENT LV_FNAME OF STRUCTURE LS_FIG TO <FS_UMXXU>.
      <FS_DATA>-UMXXU_CMP = <FS_UMXXU>.
    ELSE.
      DELETE T_DATA INDEX LV_TABIX.
      CONTINUE.
    ENDIF.
*   Caliculate percent variance
    IF <FS_DATA>-UMXXU_TGT <> 0 AND <FS_DATA>-UMXXU_CMP <> 0.
      <FS_DATA>-PERC_VARI = <FS_DATA>-UMXXU_TGT / <FS_DATA>-UMXXU_CMP * 100 - 100.
    ENDIF.
*   Get descriptions
    IF LV_DC_IND = 'C'.
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR              = <FS_DATA>-ACCT_NUM
        IMPORTING
          VENDOR_DESC        = <FS_DATA>-ACCT_DESC
        EXCEPTIONS
          WRONG_VENDOR       = 1
          OTHERS             = 2.
      IF SY-SUBRC <> 0.
*       Implement suitable error handling here
      ENDIF.
    ELSE.
      CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
        EXPORTING
          KUNNR              = <FS_DATA>-ACCT_NUM
        IMPORTING
          CUST_DESC          = <FS_DATA>-ACCT_DESC
        EXCEPTIONS
          WRONG_CUSTOMER     = 1
          OTHERS             = 2.
      IF SY-SUBRC <> 0.
*       Implement suitable error handling here
      ENDIF.
    ENDIF.
*   Get descriptions
    CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
      EXPORTING
        BUKRS                = <FS_DATA>-BUKRS
      IMPORTING
        COMP_CODE_DESC       = <FS_DATA>-COMP_CODE_DESC
      EXCEPTIONS
        WRONG_CODE           = 1
        OTHERS               = 2.
    IF SY-SUBRC <> 0.
*     Implement suitable error handling here
    ENDIF.
  ENDLOOP.
* ----------------------------------------------------------------------
* Post retrieving filtering
* ----------------------------------------------------------------------
* no action
* ----------------------------------------------------------------------
* Finishing
* ----------------------------------------------------------------------
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
