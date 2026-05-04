# Exception Indicator: CO: Internal orders - Settlement Rules - SW_10_05_INT_ORD_BAL

## General Overview

This Exception Indicator reads commitment-style settlement amounts from Controlling for internal orders, combining order master data with cost totals from the periodic value fields on CO line items, and surfaces orders where the summed settlement exposure is non-zero for the selected fiscal year and posting periods.

This EI serves as an essential control for CO and project accounting by:
- Giving controllers a consolidated view of which internal orders still carry non-zero settlement amounts across the configured posting periods
- Supporting period-end reviews when settlement rules should have cleared values to receivers
- Highlighting concentration of residual amounts by order type, company, or descriptive attributes before financial close
- Enabling follow-up with order owners when aggregated settlement buckets contradict expectations for completed work
- Reducing manual comparison of order master against COSS and COSP extracts during audits or restructuring exercises

Typical use includes month-end controlling reviews, project closure checks, and investigations after major posting or settlement configuration changes. Teams reconcile highlighted orders in KO03 or related CO transactions and adjust settlement or master data as appropriate.

The routine reads internal order headers with joined COSS and COSP totals for plan/actual category relevant to settlement.


## Problem Description

Failure to monitor non-zero internal order settlement aggregates across fiscal periods creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Residual settlement amounts may overstate or understate project or cost-center burden when not reconciled against current receiver rules
- Delayed detection of uncleared buckets distorts period comparisons for management reporting on internal orders
- Cross-system views of order status and settlement values diverge when controlling extracts are not routinely validated
- Year-end accruals or capitalization decisions may rely on incomplete pictures of open settlement pressure

**Operational and Control Risks**
- Order managers may mark work complete while controlling still shows material settlement totals in periodic columns
- Split accountability between project management and controlling leaves no shared exception queue for orders that should be fully settled
- Master data drift on order type, short text, or fiscal assignment prolongs false positives or hides true outliers

**Management Visibility and Decision-Making Risks**
- Executives lack a simple ranked view of which orders drive the largest unexplained settlement totals
- Portfolio decisions on internal orders skew when settlement concentration by organization or order category is unknown

## Suggested Resolution

**Immediate Response**
- Review each alerted order for fiscal year, posting periods in scope, and the aggregated total shown by the monitor
- Open the internal order in the standard display transaction to confirm settlement rules, receivers, and last settlement runs
- Classify whether the balance is an expected timing effect or a configuration or posting error requiring correction

**System Assessment**
- Segment findings by order type, fiscal year, and period pattern to see whether issues cluster after releases or master data loads
- Compare current results to prior monitoring cycles for the same selection to detect new outliers
- Validate that the fiscal year and period parameters match the controlling calendar used for settlement posting

**Corrective Actions**
- Execute or repeat settlement where business rules allow so periodic buckets clear to the intended receivers
- Correct settlement rules or receiver master data when misconfiguration drives persistent non-zero totals
- Document remediation for audit when amounts affected statutory or management reporting lines
- Schedule recurring monitoring after major CO customizing transports


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AUART | AUART |  | 0 | 0 |  |  |
| 2 | AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 3 | GJAHR | GJAHR |  | 0 | 0 |  |  |
| 4 | KTEXT | Name | CHAR | 20 | 0 | KTEXT | TEXT20 |
| 5 | PERIO |  |  | 0 | 0 |  |  |
| 6 | XXXXX | Natural number | INT4 | 10 | 0 | INT4 | INT4 |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 6 parameters listed in the Parameters Reference Table when tuning this EI; each influences which internal orders contribute settlement totals and how they appear in the output.

**AUART** (AUART)

Restricts internal order settlement lines where "auart" (AUART) must fall inside the configured selection for this monitor.

**AUFNR** (Order)

Uses "order" from the joined order master so only rows with AUFNR matching the declared interval remain after COSS/COSP reads.

**GJAHR** (GJAHR)

After totals are built per order, rows are discarded unless "gjahr" on GJAHR still satisfies the selection table.

**KTEXT** (Name)

Narrows the population by comparing each result line's KTEXT to the filter for "name".

**PERIO** (PERIO)

Supports fiscal reviews by enforcing "perio" through PERIO together with year and period filters.

**XXXXX** (Natural number)

When filled, aligns the alert with controlling master data because "natural number" is evaluated on aggregated settlement buckets.


### Parameter Relationships

How parameter combinations work together

**GJAHR** and **PERIO** define the fiscal year and posting period window that bounds which CO periodic columns are summed into each order’s total. **AUART** further restricts which internal order types participate, so the same fiscal slice can be analyzed separately for maintenance, capital, or overhead orders.

**AUFNR** and **KTEXT** refine the working set to specific orders or short-text patterns when investigations target known outliers rather than the full population. The summarized total field in the output reflects only rows that satisfy all active filters together, so tightening one dimension without relaxing others narrows the alert set accordingly.


### Default Values

No default values are defined for this EI in the analyzed ABAP beyond standard selection semantics for the listed parameters.

### Practical Example of Parameter Configuration

**Use Case 1: Fiscal year slice — all PM orders**

**Purpose:** Monitor maintenance-style internal orders for the current fiscal year across all posting periods in the parameter list.

```
GJAHR = 2025
AUART = PM01
PERIO = 1 - 12
```

**Use Case 2: Single order deep dive**

**Purpose:** Validate settlement bucket totals for one capital project order.

```
AUFNR = 0000123456
GJAHR = 2025
PERIO = 1 - 3
```

**Use Case 3: Text-based sweep**

**Purpose:** Find orders whose short text matches a program code while limiting the fiscal window.

```
KTEXT = *CAPEX*
GJAHR = 2024
AUART = YB01
XXXXX = 
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_05_INT_ORD_BAL | AUFNR | Order Number | CHAR(12) | AUFNR |
| /SKN/S_SW_10_05_INT_ORD_BAL | KTEXT | General Name | CHAR(20) | KTEXT |
| /SKN/S_SW_10_05_INT_ORD_BAL | XXXXX | Natural Number | INT4(10) | INT4 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_05_INT_ORD_BAL .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_05_INT_ORD_BAL OPTIONAL
*"----------------------------------------------------------------------
  " variables definition
  DATA_MULTY:   AUART        AUFART,
                GJAHR        GJAHR,
                PERIO        PERIO.
  DATA_SINGLE:  LANGU         LANGU,
                MANAGE_IN_UTC CHAR1,
                SW_DEST       RFCDEST.
  TYPES : BEGIN OF Y_FIELDNAME,
            FIELDNAME(10) TYPE C,
          END OF Y_FIELDNAME,
          BEGIN OF AUFK_TYPE,
            AUFNR   TYPE AUFK-AUFNR,
            AUTYP   TYPE AUFK-AUTYP,
            OBJNR   TYPE AUFK-OBJNR,
          END OF AUFK_TYPE,
          BEGIN OF T_COSS,
            AUFNR   TYPE AUFK-AUFNR,
            AUTYP   TYPE AUFK-AUTYP,
            GJAHR   TYPE COSS-GJAHR,"Fiscal Year
            KSTAR   TYPE COSS-KSTAR,"Cost Element
            WOG001  TYPE COSS-WOG001,
            WOG002  TYPE COSS-WOG002,
            WOG003  TYPE COSS-WOG003,
            WOG004  TYPE COSS-WOG004,
            WOG005  TYPE COSS-WOG005,
            WOG006  TYPE COSS-WOG006,
            WOG007  TYPE COSS-WOG007,
            WOG008  TYPE COSS-WOG008,
            WOG009  TYPE COSS-WOG009,
            WOG010  TYPE COSS-WOG010,
            WOG011  TYPE COSS-WOG011,
            WOG012  TYPE COSS-WOG012,
            WOG013  TYPE COSS-WOG013,
            WOG014  TYPE COSS-WOG014,
            WOG015  TYPE COSS-WOG015,
            WOG016  TYPE COSS-WOG016,
          END OF T_COSS,
          BEGIN OF T_PERIO,
            PERIO(3) TYPE N,
          END OF T_PERIO.
  DATA :        SY_TABIX        LIKE SY-TABIX,
                SY_DATLO        LIKE SY-DATLO,
                SY_TIMLO        LIKE SY-TIMLO,
                WOGS_TAB        TYPE TABLE OF T_COSS,
                WOGS_WA         TYPE T_COSS,
                T_DATA_WA       LIKE LINE OF T_DATA,
                T_FIELDNAME     TYPE STANDARD TABLE OF Y_FIELDNAME,
                T_FIELDNAME_WA  LIKE LINE OF T_FIELDNAME,
                PERIO_WA        LIKE LINE OF R_PERIO,
                PERIO_VAL       TYPE NUMC2,
                AUFK_ITAB       TYPE TABLE OF AUFK_TYPE,
                OBJNR_ITAB      TYPE TABLE OF AUFK_TYPE,
                OBJNR_WA        LIKE LINE OF OBJNR_ITAB,
                I_PERIO         TYPE T_PERIO,
                IT_PERIO        TYPE TABLE OF T_PERIO.
  " variables population
  SELECT_MULTY: AUART,
                GJAHR,
                PERIO.
  LV_LANGU = SY-LANGU.
  SELECT_SINGLE: MANAGE_IN_UTC,
                 LANGU,
                 SW_DEST.
  CONVERT_MULTY:  AUART AUART . """Tanya 14/11/18
  " if sw_dest is empty then on premise, else on cloud
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_05_INT_ORD_BAL'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  DO 16 TIMES.
    I_PERIO-PERIO = SY-INDEX.
    APPEND I_PERIO TO IT_PERIO.
  ENDDO.
*  " time filling and shifting
*  set_sy_time lv_manage_in_utc sy_datlo sy_timlo.
*  time_shift sy_datlo sy_timlo.
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  DATA: ITER_IDX TYPE NUMC2,
        ITER_TABIX TYPE I.
*  " prepare for the dynamic quering
*  READ TABLE r_perio INDEX 1 INTO perio_wa.
*  perio_val = perio_wa-low.
*  iter_tabix = 1.
*
*  t_fieldname_wa = 'OBJNR'.
*  APPEND t_fieldname_wa TO t_fieldname.
*  DO perio_val TIMES.
*
*    iter_idx = iter_tabix.
*    CONCATENATE 'WOG0' iter_idx INTO t_fieldname_wa.
*    APPEND t_fieldname_wa TO t_fieldname.
*    iter_tabix = iter_tabix + 1.
*
*  ENDDO.
  " retrieve the data
  REFRESH WOGS_TAB.
  SELECT T1~AUFNR T1~AUTYP
    T2~GJAHR "Fiscal Year
    T2~KSTAR "Cost Element
         T2~WOG001 T2~WOG002 T2~WOG003 T2~WOG004 T2~WOG005 T2~WOG006 T2~WOG007 T2~WOG008
         T2~WOG009 T2~WOG010 T2~WOG011 T2~WOG012 T2~WOG013 T2~WOG014 T2~WOG015 T2~WOG016
    INTO CORRESPONDING FIELDS OF TABLE WOGS_TAB
  FROM ( AUFK AS T1 INNER JOIN COSS AS T2 ON T1~OBJNR = T2~OBJNR )
    WHERE T2~GJAHR IN R_GJAHR
      AND WRTTP = '04'.
  SELECT T1~AUFNR T1~AUTYP
    T2~GJAHR "Fiscal Year
    T2~KSTAR "Cost Element
         T2~WOG001 T2~WOG002 T2~WOG003 T2~WOG004 T2~WOG005 T2~WOG006 T2~WOG007 T2~WOG008
         T2~WOG009 T2~WOG010 T2~WOG011 T2~WOG012 T2~WOG013 T2~WOG014 T2~WOG015 T2~WOG016
    APPENDING CORRESPONDING FIELDS OF TABLE  WOGS_TAB
  FROM ( AUFK AS T1 INNER JOIN COSP AS T2 ON T1~OBJNR = T2~OBJNR )
    WHERE T2~GJAHR IN R_GJAHR
      AND WRTTP = '04'.
  DATA:
        TMP_VAL     TYPE STRING.
  FIELD-SYMBOLS: <FS_COMP> TYPE ANY.
  " summarize all amounts (from both tables) selected above for every AUFNR
  LOOP AT WOGS_TAB INTO WOGS_WA.
    CLEAR T_DATA_WA.
    LOOP AT IT_PERIO INTO I_PERIO
      WHERE PERIO IN R_PERIO.
      CONCATENATE 'WOG' I_PERIO-PERIO INTO T_FIELDNAME_WA.
      "ASSIGN COMPONENT t_fieldname_wa OF STRUCTURE wogs_wa TO FIELD-SYMBOL().
      ASSIGN COMPONENT T_FIELDNAME_WA OF STRUCTURE WOGS_WA TO <FS_COMP>.
      "t_data_wa-xxxxx = t_data_wa-xxxxx + .
      T_DATA_WA-XXXXX = T_DATA_WA-XXXXX + <FS_COMP>.
    ENDLOOP.
*      t_data_wa-aufnr = wogs_wa-objnr.
*      t_data_wa-xxxxx = wogs_wa-wog001 + wogs_wa-wog002 + wogs_wa-wog003 + wogs_wa-wog004
*                        + wogs_wa-wog005 + wogs_wa-wog006 + wogs_wa-wog007 + wogs_wa-wog008
*                        + wogs_wa-wog009 + wogs_wa-wog010 + wogs_wa-wog011 + wogs_wa-wog012
*                        + wogs_wa-wog013 + wogs_wa-wog014 + wogs_wa-wog015 + wogs_wa-wog016.
    " delete AUFNRs which has zero total
    IF T_DATA_WA-XXXXX <> 0.
      APPEND T_DATA_WA TO T_DATA.
    ENDIF.
  ENDLOOP.
  " check alert information
  READ TABLE T_DATA   INDEX 1.
  CHECK NOT  SY-TFILL IS INITIAL.
  IS_ALERT = 'X'.
ENDFUNCTION.
```
