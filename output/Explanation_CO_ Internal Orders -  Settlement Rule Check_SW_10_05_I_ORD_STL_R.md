# Exception Indicator: CO: Internal orders - Settlement Rule Check - SW_10_05_I_ORD_STL_R

## General Overview

This Exception Indicator monitors internal orders in Controlling for missing or incomplete settlement rules, focusing on orders that should have cost-center or WBS-based settlement lines and on how completely those lines cover the order.

This EI serves as an essential control for controlling and project accounting by:
- Surfacing orders that lack any settlement rule when one is expected from the settlement table join
- Highlighting cases where settlement percentages do not reach full coverage so residual risk is visible before period close
- Supporting reviews of open-ended validity on settlement lines versus orders that should already be fully settled
- Giving managers a time-based view of how long issues have persisted relative to order master dates
- Helping internal audit and operational teams prioritize which plants, company codes, or order types drive the largest exception volume

Typical use includes month-end controlling reviews, project settlement readiness checks, and follow-up after master-data or settlement-profile changes. Teams act on results in order maintenance and settlement configuration, then rerun the monitor to confirm cleanup.

The routine reads order header data from AUFK together with settlement rule lines from COBRB.


## Problem Description

Failure to monitor internal orders for missing or incomplete settlement rules creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Project or cost-collector results can be misstated when orders remain without a complete settlement path to downstream receivers
- Period-end allocations may be delayed when controllers only discover gaps during settlement batch runs
- Capitalization or overhead flows can be inconsistent when percentage coverage on settlement lines is not validated early
- Management reporting on order profitability weakens when settlement completeness is assumed rather than checked
- Consolidated views of controlling health skew when some entities enforce settlement discipline and others do not

**Operational and Control Risks**
- Order types that require receiver lines may proceed operationally while master settlement data is still incomplete
- Split ownership between order administrators and settlement teams leaves no shared queue of structural exceptions
- Changes to cost centers or WBS elements may not be reflected in settlement rules promptly, prolonging incorrect receiver assignments
- Long-running internal orders can accumulate undetected configuration drift between header attributes and settlement rows

**Management Visibility and Decision-Making Risks**
- Executives lack a concise exception list to see which company codes or plants concentrate settlement gaps
- Project managers cannot trust completion indicators when settlement percentages behind the scenes are below full coverage
- Strategic reviews of internal order portfolios miss which categories drive the highest remediation workload

## Suggested Resolution

**Immediate Response**
- Review each flagged order for business materiality, responsible cost object, and whether settlement should already exist
- Open the internal order in KO02 or the appropriate order display to validate settlement profile, receivers, and percentage distribution
- Confirm whether missing rules are intentional for the order category or indicate a master-data defect
- Capture accountable roles and target dates when amounts or project phases are sensitive to reporting deadlines

**System Assessment**
- Segment results by company code, plant, controlling area, and order type to see where exception volume concentrates
- Compare current counts to prior monitoring cycles after major reorganizations or template changes to settlement profiles
- Examine how long each case has been outstanding relative to recent changes on the order master for prioritization
- Validate that organizational master data on the order matches active controlling and settlement setup

**Corrective Actions**
- Create or correct settlement rules and receiver lines so percentages and validity windows reflect the intended controlling model
- Adjust order master attributes when the wrong order type or cost collector was used, then rebuild settlement data as appropriate
- Coordinate with project accounting to close or reclassify orders that no longer require settlement activity
- Document remediation for audit when orders touched regulated or capitalized projects
- Schedule recurring monitoring after template releases so new order categories inherit correct settlement behavior early


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Change date for order master | DATS | 8 | 0 | AUFAEDAT | DATUM |
| 2 | AENAM | Last Changed By | CHAR | 12 | 0 | AUFAENAM | USNAM |
| 3 | AUART | Order Type | CHAR | 4 | 0 | AUFART | AUFART |
| 4 | AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 5 | AUTYP | Order category | NUMC | 2 | 0 | AUFTYP | AUFTYP |
| 6 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 7 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 8 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 9 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 10 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 11 | ERDAT | Created on | DATS | 8 | 0 | AUFERFDAT | DATUM |
| 12 | ERNAM | Entered by | CHAR | 12 | 0 | AUFERFNAM | USNAM |
| 13 | ERR_MSG | Err. Msg. | CHAR | 200 | 0 | /SKN/E_SW_ERR_MSG | /SKN/D_SW_ERR_MSG |
| 14 | ERR_TYPE | Error Type for detection | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 15 | FIELD_FOR_CHECK | Field for Check |  | 0 | 0 |  |  |
| 16 | GABJA | Valid-from year | NUMC | 4 | 0 | GABJA | GJAHR |
| 17 | GABPE | Valid-from period | NUMC | 3 | 0 | GABPE | POPER |
| 18 | GBISJ | Valid-to year | NUMC | 4 | 0 | GBISJ | GJAHR |
| 19 | GBISP | Valid to | NUMC | 3 | 0 | GBISP | POPER |
| 20 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 21 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 22 | KOSTL | Cost center | CHAR | 10 | 0 | BRGKOSTL | KOSTL |
| 23 | OBJNR | Object number | CHAR | 22 | 0 | J_OBJNR | J_OBJNR |
| 24 | PERBZ | Settlement type | CHAR | 3 | 0 | PERBZ_LD | PERBZ_LD |
| 25 | PROZS | Percent | DEC | 5 | 2 | BRGPROZS | PROZS |
| 26 | PS_PSP_PNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 27 | STDAT | Status change | DATS | 8 | 0 | AUFSTDAT | DATUM |
| 28 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 29 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 29 parameters listed in the Parameters Reference Table when tuning this EI; each influences which internal orders are tested for settlement rules, completeness, and timing.

**AEDAT** (Change date for order master)

Narrows internal order settlement-rule checks where "change date for order master" (AEDAT) must match the selection interval.

**AENAM** (Last Changed By)

Uses "last changed by" from AUFK/COBRB context so only orders with AENAM inside the configured range proceed.

**AUART** (Order Type)

After retrieval, rows are excluded unless "order type" on AUART still satisfies the monitor filters.

**AUFNR** (Order)

Aligns alerts with organizational scope by evaluating "order" through AUFNR for each candidate order.

**AUTYP** (Order category)

Supports controller review cycles by enforcing "order category" via AUTYP together with settlement attributes.

**BACKDAYS** (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BUKRS** (Company Code)

Combines with other filters so "company code" (BUKRS) refines which orders receive error typing and duration logic.

**DATE_REF_FLD** (Date reference field)

Chooses which order master date column receives the default lookback window from BACKDAYS before duration filtering.

**DATE_REF_FLD Options:**
- **ERDAT** — Created-on date; maps the lookback range to order creation.
- **STDAT** — Date of last status change; maps the lookback to status history.
- **AEDAT** — Changed-on date; default branch in code when DATE_REF_FLD is AEDAT or OTHERS.

**DURATION** (Duration In Time Units)

Narrows internal order settlement-rule checks where "duration in time units" (DURATION) must match the selection interval.

**DURATION_UNIT** (Duration Unit)

Unit for elapsed time between the reference date from DATE_REF_FLD and the evaluation run.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes.
- **D** — Days (default preset in code before selection read).
- **F** — Full days for specific day filtering.

**ERDAT** (Created on)

After retrieval, rows are excluded unless "created on" on ERDAT still satisfies the monitor filters.

**ERNAM** (Entered by)

Aligns alerts with organizational scope by evaluating "entered by" through ERNAM for each candidate order.

**ERR_MSG** (Err. Msg.)

Supports controller review cycles by enforcing "err. msg." via ERR_MSG together with settlement attributes.

**ERR_TYPE** (Error Type for detection)

Restricts which generated diagnostic rows remain after settlement and completeness checks.

**ERR_TYPE Options:**
- **S** — Information-style finding used when no settlement rule exists on the joined data.
- **C** — Completeness-style finding used when summed settlement percentage is not 100%.

**FIELD_FOR_CHECK** (Field for Check)

Selects which settlement-relevant field must be populated for an order to stay in the refined population.

**FIELD_FOR_CHECK Options:**
- **KOSTL** — Cost center must be present when this branch is active (default preset in code).
- **PS_PSP_PNR** — WBS element must be present when this branch is active.

**GABJA** (Valid-from year)

When left open per framework rules, GABJA does not restrict "valid-from year"; when set, only matching orders remain.

**GABPE** (Valid-from period)

Narrows internal order settlement-rule checks where "valid-from period" (GABPE) must match the selection interval.

**GBISJ** (Valid-to year)

Uses "valid-to year" from AUFK/COBRB context so only orders with GBISJ inside the configured range proceed.

**GBISP** (Valid to)

After retrieval, rows are excluded unless "valid to" on GBISP still satisfies the monitor filters.

**GSBER** (Business Area)

Aligns alerts with organizational scope by evaluating "business area" through GSBER for each candidate order.

**KOKRS** (Controlling Area)

Supports controller review cycles by enforcing "controlling area" via KOKRS together with settlement attributes.

**KOSTL** (Cost center)

When restricted, keeps the working set readable by requiring "cost center" on KOSTL to match declared values.

**OBJNR** (Object number)

Combines with other filters so "object number" (OBJNR) refines which orders receive error typing and duration logic.

**PERBZ** (Settlement type)

When left open per framework rules, PERBZ does not restrict "settlement type"; when set, only matching orders remain.

**PROZS** (Percent)

Narrows internal order settlement-rule checks where "percent" (PROZS) must match the selection interval.

**PS_PSP_PNR** (WBS Element)

Uses "wbs element" from AUFK/COBRB context so only orders with PS_PSP_PNR inside the configured range proceed.

**STDAT** (Status change)

After retrieval, rows are excluded unless "status change" on STDAT still satisfies the monitor filters.

**USER_FLD** (Dynamic Recipient User Field)

Optional dynamic recipient or extension field passed through the monitor framework when populated.

**USER_FLD Options:**
No fixed USER_FLD value list is defined in the available code for this EI.

**WERKS** (Plant)

Supports controller review cycles by enforcing "plant" via WERKS together with settlement attributes.


### Parameter Relationships

How parameter combinations work together

**DATE_REF_FLD** selects which order master date column receives the monitoring window built from **BACKDAYS**; the code maps that choice to the corresponding date range before the main select on internal orders. This aligns with the fixed Parameter Configuration wording for **BACKDAYS** and **DATE_REF_FLD**: the lookback window is anchored on the date reference field.

**DURATION** and **DURATION_UNIT** operate after rows are flagged. They measure elapsed time from the reference date implied by **DATE_REF_FLD** on each result line through the evaluation run, using the unit you configure, and only rows whose computed age fits the duration selection remain.

**FIELD_FOR_CHECK** determines whether the routine expects a populated cost center or WBS receiver on the settlement side before deeper completeness checks; it works together with the settlement line fields such as **KOSTL** and **PS_PSP_PNR** filters so the population matches the intended receiver type.

**ERR_TYPE** limits which diagnostic categories remain in the final list after the routine assigns information-style versus completeness-style findings, so monitoring passes can focus on the severity mix you need.


### Default Values

- **BACKDAYS** - 1 from the preset before the selection read when the caller does not override it.
- **DATE_REF_FLD** - AEDAT from the preset before the selection read when the caller does not override it.
- **DURATION_UNIT** - D from the preset before the selection read when the caller does not override it.
- **FIELD_FOR_CHECK** - KOSTL from the preset before the selection read when the caller does not override it.
- **DURATION** - initial — when the duration selection range is left empty, the routine does not filter rows out by computed age until a range is supplied.

### Practical Example of Parameter Configuration

**Use Case 1: Cost-center settlement hygiene for one company**

**Purpose:** Catch orders changed in the last week that still lack complete settlement coverage, using change date as the reference.

```
BUKRS = 1000
BACKDAYS = 7
DATE_REF_FLD = AEDAT
FIELD_FOR_CHECK = KOSTL
DURATION_UNIT = D
DURATION = 5
```

**Use Case 2: Plant slice with error-type focus**

**Purpose:** Review plant 0001 for completeness-style findings only after standard settlement checks.

```
WERKS = 0001
ERR_TYPE = C
AUART = YB01
```

**Use Case 3: WBS-based projects with creation-date window**

**Purpose:** Monitor project-style orders using creation date as the anchor and a modest lookback.

```
DATE_REF_FLD = ERDAT
BACKDAYS = 14
FIELD_FOR_CHECK = PS_PSP_PNR
KOKRS = 1000
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_05_INT_ORD_STL_RL | AEDAT | Change date for Order Master | DATS(8) | AUFAEDAT |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | AENAM | Last changed by | CHAR(12) | AUFAENAM |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | AUART | Order Type | CHAR(4) | AUFART |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | AUFNR | Order Number | CHAR(12) | AUFNR |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | AUTYP | Order category | NUMC(2) | AUFTYP |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | ERDAT | Created on | DATS(8) | AUFERFDAT |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | ERNAM | Entered by | CHAR(12) | AUFERFNAM |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | ERR_MSG | SW: Error Message | CHAR(200) | /SKN/E_SW_ERR_MSG |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | ERR_TYPE | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | GABJA | Valid-from year | NUMC(4) | GABJA |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | GABPE | Valid-from period | NUMC(3) | GABPE |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | GBISJ | Valid-to year | NUMC(4) | GBISJ |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | GBISP | Valid to | NUMC(3) | GBISP |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | KOKRS | Controlling Area | CHAR(4) | KOKRS |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | KOSTL | Receiver cost center | CHAR(10) | BRGKOSTL |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | OBJNR | Object number | CHAR(22) | J_OBJNR |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | PERBZ | Settlement type | CHAR(3) | PERBZ_LD |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | PROZS | Settlement percentage rate | DEC(5,2) | BRGPROZS |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | PS_PSP_PNR | Work Breakdown Structure Element (WBS Element) | NUMC(8) | PS_PSP_PNR |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | STDAT | Date of last status change | DATS(8) | AUFSTDAT |
| /SKN/S_SW_10_05_INT_ORD_STL_RL | WERKS | Plant | CHAR(4) | WERKS_D |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_05_INT_ORD_STL_RL .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_05_INT_ORD_STL_RL OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: MANAGE_IN_UTC     CHAR1 ,
             LANGU             LANGU,
             BACKDAYS          INT4,
             DATE_REF_FLD      NAME_FELD,
             DURATION_UNIT     /SKN/E_SW_DURATION_UNIT,
             FIELD_FOR_CHECK   NAME_FELD "FIELDNAME,
             .
CONSTANTS: LC_NO_SETTLEMENT(200) TYPE C VALUE 'Order does not have any settlement rule'.
CONSTANTS: LC_NO_COMPLETENESS(200) TYPE C VALUE 'Order has Settlement rule lower then 100%'.
 LV_BACKDAYS = 1.
 LV_DATE_REF_FLD = 'AEDAT'.  "Change order master date
 LV_DURATION_UNIT = 'D'.
 LV_FIELD_FOR_CHECK = 'KOSTL'.
 SELECT_SINGLE: MANAGE_IN_UTC,
                LANGU,
                BACKDAYS,
                DATE_REF_FLD,
                DURATION_UNIT,
                FIELD_FOR_CHECK
                .
DATA_MULTY: AUFNR        AUFNR,
            AUART        AUFART,
            AUTYP        AUFTYP,
            OBJNR        J_OBJNR,
            KOSTL           BRGKOSTL,
            GABJA           GABJA,
            GABPE           GABPE,
            GBISJ           GBISJ,
            GBISP           GBISP,
            PS_PSP_PNR      PS_PSP_PNR,
            SUM_PROZS       BRGPROZS,
            BUKRS           BUKRS,
            WERKS           WERKS_D,
            GSBER           GSBER,
            KOKRS           KOKRS,
            AEDAT           AUFAEDAT, "Changed on
            ERDAT           AUFERFDAT, "Date on Which Record Was Created
            STDAT           AUFSTDAT, "Date of last status change
            DATUM            SY-DATUM,
            DURATION        /SKN/E_SW_DURATION,
            ERR_TYPE        CHAR1
             .
SELECT_MULTY: AUFNR ,
            AUART,
            AUTYP,
            OBJNR,
            KOSTL,
            GABJA,
            GABPE,
            GBISJ,
            GBISP,
            PS_PSP_PNR,
            SUM_PROZS,
            BUKRS,
            WERKS,
            GSBER,
            KOKRS,
            AEDAT, "Changed on
            ERDAT, "Date on Which Record Was Created
            STDAT, "Date of last status change
            DATUM,
            DURATION,
            ERR_TYPE
            .
CONVERT_MULTY: AUFNR ALPHA,
               "GJAHR GJAHR,
               KOSTL ALPHA.
CONVERT_MULTY:  AUART AUART . """Tanya 14/11/18
*convert_multy: KUNNR ALPHA,
*               VBELN ALPHA,
*               BP1_CODE ALPHA,
*               BP2_CODE ALPHA,
*               BP3_CODE ALPHA.
*ranges : R_FLD_NAME for DD03P-FIELDNAME,
*         R_FLD_VAL for DD03P-FIELDNAME .
*
*data :   FLD_NAME type FIELDNAME.
*data : i type I,
*       ci(1) type c,
*       nfields type I value 3.   "
DATA : BACKDAYS  TYPE I ,
       FORWDAYS TYPE I,
       DATE_FROM LIKE SY-DATUM,
       DATE_TO LIKE SY-DATUM .
DATA : LANGU LIKE SY-LANGU .
DATA : IS_OUT(1) TYPE C.
DATA : TIME_DIFF TYPE  INT4 .
*data : W_DATA like line of T_DATA .
*data : wa_VBPA type VBPA.
*data : lv_VBELN type VBELN,
*       lv_POSNR type POSNR,
*       lv_PARVW type PARVW,
*       lv_KUNNR TYPE  KUNNR,
*       lv_KUNNR_NAME TYPE  NAME1_GP,
*       lv_LIFNR TYPE  LIFNR,
*       lv_LIFNR_NAME TYPE  NAME1_GP,
*       lv_PERNR TYPE  PERNR_D,
*       lv_PERNR_NAME TYPE  NAME1_GP,
*       lv_NRART type NRART.
*data: lv_VBTYP type VBTYP.
  DATA: LT_DATA LIKE T_DATA[],
        LWA_DATA LIKE LINE OF T_DATA,
        LTW_DATA LIKE TABLE OF LWA_DATA.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : FLD(60) TYPE C .
DATA : REF_DATE TYPE D.
FIELD-SYMBOLS:  TYPE ANY ,
               <FS_V> TYPE ANY .
DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
  INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
DATA : END OF SW_STRUCTURE .
DATA : LS_VBPA TYPE VBPA,
       LT_VBPA LIKE TABLE OF LS_VBPA.
DATA : LV_DATA_POSNR TYPE POSNR.
DATA: LS_STATUS TYPE JSTAT,
      LT_STATUS LIKE TABLE OF LS_STATUS,
      LV_STATUS TYPE J_STATUS,
      LV_STATUS_OK(1) TYPE C.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_05_IN_ORD_STL_RL'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
   IF R_DATUM[] IS INITIAL .
     RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
       DATE_FROM = SY-DATUM - LV_BACKDAYS .
       RS_DATUM-LOW = DATE_FROM .
        APPEND RS_DATUM TO R_DATUM.
   ENDIF.
 "--- Set Reference Date Field
   CASE LV_DATE_REF_FLD.
     WHEN 'ERDAT'.
       R_ERDAT[] = R_DATUM[]. "Document created
     WHEN  'STDAT'.
       R_STDAT[] = R_DATUM[]. ""Date of last status change
     WHEN 'AEDAT'.
       R_AEDAT[] = R_DATUM[]. "changed on
     WHEN OTHERS.
       R_AEDAT[] = R_DATUM[]. "changed on
   ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
     SELECT A~AUFNR A~AUART A~AUTYP C~OBJNR C~KOSTL C~GABJA C~GABPE C~GBISJ C~GBISP C~PS_PSP_PNR
            C~PROZS A~ERNAM A~ERDAT A~AENAM A~AEDAT A~STDAT A~BUKRS A~WERKS A~GSBER A~KOKRS
      FROM AUFK AS A
      LEFT OUTER JOIN COBRB AS C
      ON A~OBJNR = C~OBJNR
      INTO CORRESPONDING FIELDS OF TABLE LT_DATA
      WHERE  A~OBJNR IN R_OBJNR
         AND A~AUFNR IN R_AUFNR
         AND A~AUART IN R_AUART
         AND A~AUTYP IN R_AUTYP
         AND A~ERDAT IN R_ERDAT
         AND A~AEDAT IN R_AEDAT
         AND A~STDAT IN R_STDAT
         AND A~BUKRS IN R_BUKRS
         AND A~WERKS IN R_WERKS
         AND A~GSBER IN R_GSBER
         AND A~KOKRS IN R_KOKRS.
*         AND c~KOSTL in R_KOSTL
*         AND c~GABJA in R_GABJA
*         AND c~GABPE in R_GABPE
*         AND c~GBISJ in R_GBISJ
*         AND c~GBISP in R_GBISP.
***************************************************************************
  "--- Get no settlement population
   LOOP AT LT_DATA INTO LWA_DATA WHERE OBJNR IS INITIAL.
     SY_TABIX = SY-TABIX.
     LWA_DATA-ERR_TYPE = 'S'.
     LWA_DATA-ERR_MSG = LC_NO_SETTLEMENT.
     APPEND LWA_DATA TO T_DATA.
     DELETE LT_DATA INDEX SY_TABIX.
   ENDLOOP.
  "--- Remove non relevant population
  CASE LV_FIELD_FOR_CHECK.
       WHEN 'KOSTL'.
         DELETE LT_DATA WHERE KOSTL IS INITIAL .
       WHEN 'PS_PSP_PNR'.
         DELETE LT_DATA WHERE PS_PSP_PNR IS INITIAL .
       WHEN OTHERS.
     ENDCASE.
     DELETE LT_DATA WHERE GBISJ IS NOT INITIAL .
     DELETE LT_DATA WHERE GBISP IS NOT INITIAL .
     DELETE LT_DATA WHERE KOSTL  NOT IN R_KOSTL .
     DELETE LT_DATA WHERE PS_PSP_PNR NOT IN R_PS_PSP_PNR.
     DELETE LT_DATA WHERE GABJA NOT IN R_GABJA.
     DELETE LT_DATA WHERE GABPE NOT IN R_GABPE.
  "--- Check no settlement completness (100%)
  REFRESH LTW_DATA.
   SORT LT_DATA BY AUFNR OBJNR.
   LOOP AT LT_DATA INTO LWA_DATA.
     CLEAR LWA_DATA-PERBZ.
     COLLECT LWA_DATA INTO LTW_DATA.
   ENDLOOP.
   "delete ltw_data where PROZS <> 100.
   LOOP AT LTW_DATA INTO LWA_DATA WHERE PROZS <> 100.
     SY_TABIX = SY-TABIX.
     LWA_DATA-ERR_TYPE = 'C'.
     LWA_DATA-ERR_MSG = LC_NO_COMPLETENESS.
     APPEND LWA_DATA TO T_DATA.
   ENDLOOP.
   DELETE T_DATA WHERE ERR_TYPE NOT IN R_ERR_TYPE.
*  loop at lt_data into lwa_data.
*     CASE lv_FIELD_FOR_CHECK.
*       WHEN 'KOSTL'.
*          "if lwa_data-AUART in R_AUART.
*            if lwa_data-KOSTL IS NOT INITIAL .
*               if ( lwa_data-GBISJ IS INITIAL
*                   AND lwa_data-GBISP IS INITIAL ).
*                 "--- Relevant population
*                 if lwa_data-PROZS is initial .
*                   " No settlement
*                 else.
*                   "== check 100
*                 endif.
*               else.
*                 "if lwa_data-ERR_MSG is INITIAL.
*                 "ERR_MSG :"Order & doesn't have any settlement rule"
*               endif.
*             endif.
*          "endif.
*       WHEN 'PS_PSP_PNR'.
*           if lwa_data-AUART in R_AUART.
*              if lwa_data-PS_PSP_PNR IS NOT INITIAL .
*               if ( lwa_data-GABJA IS INITIAL
*                   AND  lwa_data-GABPE IS INITIAL
*                   AND lwa_data-GBISJ IS INITIAL
*                   AND lwa_data-GBISP IS INITIAL )
*                 OR
*                  ( lwa_data-GABJA IS NOT INITIAL
*                   AND  lwa_data-GABPE IS NOT INITIAL
*                   AND lwa_data-GBISJ IS INITIAL
*                   AND lwa_data-GBISP IS INITIAL ).
*               else.
*                 "ERR_MSG :"Order & doesn't have any settlement rule"
*               endif.
*             endif.
*           endif.
*       WHEN OTHERS.
*     ENDCASE.
*  endloop.
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    IF  IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = REF_DATE
          T_FROM            = SY-UZEIT
          D_TO              = SY-DATUM
          T_TO              = SY-UZEIT
          TIME_UNIT         = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
      IF SY-SUBRC = 0.
        T_DATA-DURATION = TIME_DIFF .
      ELSE.
         T_DATA-DURATION = '999999'.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
******************************************************************************
************************************************************************
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
