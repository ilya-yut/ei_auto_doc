# Exception Indicator: MM: Material Price Change - SW_10_02_MAT_PRC_CHG

## General Overview

This Exception Indicator (EI) monitors material ledger and price change documents in Materials Management (MM) to identify moving average price changes that exceed configurable percentage thresholds (e.g. ±20%). It provides visibility into periodic unit price, standard price, and price unit changes by material, valuation area, and period for audit and control.

This EI serves as an essential control for valuation and material ledger oversight by:
- Enabling detection of material price changes that exceed percentage thresholds and may require investigation or approval
- Supporting identification of exceptional moving average or standard price changes by material, plant, or period for prioritization and root-cause analysis
- Providing visibility into price control (moving average vs standard) and business transaction type for valuation and process review
- Enabling analysis of price change patterns by valuation area, company code, and document type for process and control improvement
- Supporting accountability by user and transaction for audit and compliance

This monitoring enables organizations to detect unusual price movements, erroneous or unauthorized price changes, and concentration patterns that may indicate errors or fraud. The EI is particularly valuable for period-end valuation review, material ledger reconciliation, and exception management in costing and inventory valuation.

The EI uses material ledger and price change data (e.g. via F_SW_10_02_MAT_PRICE_CNG and F_SW_10_02_MAT_PRCE_CNG2); filters include valuation type (VGART), business transaction (GLVOR), and percentage thresholds for price unit, periodic unit price, and standard price.


## Problem Description

Failure to monitor material moving average price changes that exceed configurable thresholds creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected large price changes can distort inventory valuations and cost of goods sold in period-end closing
- Significant periodic unit price or standard price movements may indicate valuation errors, manual overrides, or fraud requiring adjustment
- Unreconciled price change exceptions can lead to misstated balance sheet and audit findings
- Delayed identification of threshold violations may complicate month-end close and require restatements
- Concentration of large price changes in certain materials or valuation areas can mask systemic issues in controls

**Operational and Control Risks**
- Price change documents with percentage changes above threshold may indicate incomplete or erroneous postings requiring release or correction
- Lack of visibility by material, valuation area, or business transaction limits ability to prioritize reviews and resource allocation
- Unmonitored price control (moving average vs standard) or manual price override indicators can delay detection of process or master data issues
- Missing filtering by percentage thresholds (price unit, periodic unit price, standard price) can lead to noise or missed exceptions
- Insufficient visibility by period or document type restricts actionable reporting for auditors and operations

**Management Visibility and Decision-Making Risks**
- Absence of exception monitoring delays awareness of significant price changes and threshold violations
- Unidentified concentration of price changes by material or valuation area can lead to missed process improvements or fraud indicators
- Lack of visibility by user or transaction limits ability to escalate unauthorized or erroneous price changes for review
- Insufficient filtering by organizational and document dimensions restricts actionable reporting for auditors and operations

## Suggested Resolution

**Immediate Response**
- Review the price change lines flagged by the EI to understand scope and magnitude (percentage threshold violations, material or period concentration)
- Verify high-change documents using material ledger or price change transactions (e.g. CKMLCP, MR21) to confirm postings and legitimacy
- Check price control and manual override indicators and processing progress to determine if manual correction or approval is pending
- Identify business context for exceptions: material, valuation area, period, or user responsible

**System Assessment**
- Analyze the percentage thresholds used for monitoring (price unit, periodic unit price, standard price) to ensure they align with policy and material significance
- Examine valuation type (VGART) and business transaction (GLVOR) filters to confirm the result set supports the intended analysis
- Assess material, valuation area, and period distribution to identify patterns or master data issues
- Validate that price control and manual price override indicators are interpreted correctly for prioritization
- Review document type and period for price-change-specific patterns

**Corrective Actions**
- Post corrections or reversing entries through standard material ledger or price change transactions where errors or illegitimate changes are confirmed
- For incomplete or erroneous price changes, complete or correct postings as per process
- Update material master or valuation parameters if misconfiguration or data quality issues are found
- Adjust monitoring parameters (percentage thresholds, valuation type, business transaction) to align with policy and re-run the EI
- Document exceptions and resolutions for audit trail; establish recurring EI runs for continuous visibility into price change exceptions


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ASCHEMA | Matl Update Struct. | CHAR | 4 | 0 | CKML_ASCHEMA | CKML_ASCHEMA |
| 2 | AWORG | Reference org. unit | CHAR | 10 | 0 | AWORG | AWORG |
| 3 | AWREF | Reference document | CHAR | 10 | 0 | AWREF | AWREF |
| 4 | AWSYS | Logical System | CHAR | 10 | 0 | LOGSYSTEM | LOGSYS |
| 5 | AWTYP | Reference procedure | CHAR | 5 | 0 | AWTYP | AWTYP |
| 6 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 7 | BDATJ | Year | NUMC | 4 | 0 | BDATJ | GJAHR |
| 8 | BELNR | Document Number | CHAR | 10 | 0 | CK_BELNR | BELNR |
| 9 | BELNR_EA | Document Number | CHAR | 10 | 0 | CK_BELNR | BELNR |
| 10 | BELNR_MA | Document Number | CHAR | 10 | 0 | CK_BELNR | BELNR |
| 11 | BEWARTGRP | Movement Type Group | CHAR | 2 | 0 | CKML_MLBWG | CKML_MLBWG |
| 12 | BKLAS | Valuation Class | CHAR | 4 | 0 | BKLAS | BKLAS |
| 13 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 14 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 15 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 16 | BVALT | Procure.alt./process | NUMC | 12 | 0 | CKML_ALPROCNR | CK_KALNR |
| 17 | BWKEY | Valuation Area | CHAR | 4 | 0 | BWKEY | BWKEY |
| 18 | BWMOD | Valuation Grpg Code | CHAR | 4 | 0 | BWMOD | CHAR4 |
| 19 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 20 | COMP_CODE_DESC | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 21 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 22 | CPUTM | Time of Entry | TIMS | 6 | 0 | CPUTM | UZEIT |
| 23 | CSPLIT | Apportionment Struct | CHAR | 4 | 0 | CSPLIT | CSPLIT |
| 24 | CURTP | Crcy type/val.view | CHAR | 2 | 0 | CURTP | CURTP |
| 25 | DATE_REF_FLD | DATE reference field |  | 0 | 0 |  |  |
| 26 | ERZKALK | Std Cost Est Price | CHAR | 1 | 0 | CK_ERZKALK | XFELD |
| 27 | GLVOR | Business Transaction | CHAR | 4 | 0 | GLVOR | CHAR4 |
| 28 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 29 | KALNR | Cost Estimate Number | NUMC | 12 | 0 | CK_KALNR | CK_KALNR |
| 30 | KATEGORIE | Category | CHAR | 2 | 0 | CKML_KATEGORIE | CKML_KATEGORIE |
| 31 | KJAHR | Document Year | NUMC | 4 | 0 | CK_KJAHR | GJAHR |
| 32 | KJAHR_EA | Document Year | NUMC | 4 | 0 | CK_KJAHR | GJAHR |
| 33 | KJAHR_MA | Document Year | NUMC | 4 | 0 | CK_KJAHR | GJAHR |
| 34 | KTOPL | Chart of Accounts | CHAR | 4 | 0 | KTOPL | KTOPL |
| 35 | KZBWS | Spec. stk valuation | CHAR | 1 | 0 | KZBWS | KZBWS |
| 36 | LANGU | Language |  | 0 | 0 |  |  |
| 37 | LBKUM | Quantity | QUAN | 15 | 3 | CK_LBKUMD | MENGV8 |
| 38 | LBKUM_OLD | Total stock | QUAN | 15 | 3 | CK_LBKUM | MENGV8 |
| 39 | LIFNR | Supplier | CHAR | 10 | 0 | LIFNR | LIFNR |
| 40 | LOGSYS | Logical System | CHAR | 10 | 0 | LOGSYSTEM | LOGSYS |
| 41 | MANPAE_S | Manual price change | CHAR | 1 | 0 | CK_MANPAE | XFELD |
| 42 | MANPAE_V | Manual price change | CHAR | 1 | 0 | CK_MANPAE | XFELD |
| 43 | MATERIAL_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 44 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 45 | MEINS | Unit of Measure | UNIT | 3 | 0 | CKML_MEINS | MEINS |
| 46 | MLAST | Material price determination: control | CHAR | 1 | 0 | CK_ML_ABST | CK_ML_ABST |
| 47 | MODIF | Type of BOM item | CHAR | 1 | 0 | CK_MODIF | CK_MODIF |
| 48 | NAME_FIRST | First name | CHAR | 40 | 0 | NAME_FIRST | TEXT40 |
| 49 | NAME_LAST | Last name | CHAR | 40 | 0 | NAME_LAST | TEXT40 |
| 50 | NAME_TEXT | Complete name | CHAR | 80 | 0 | NAME_TEXT | TEXT80 |
| 51 | PEINH | Price unit | DEC | 5 | 0 | CK_PEINH_1 | PACK3 |
| 52 | PEINH_DIFF | Price unit | DEC | 5 | 0 | CK_PEINH_1 | PACK3 |
| 53 | PEINH_DIFF_PRCT | Price unit diff % | DEC | 5 | 2 | PERCT | DEC3_2 |
| 54 | PEINH_MBEW | Price unit | DEC | 5 | 0 | PEINH | PACK3 |
| 55 | PEINH_NEW | New price unit | DEC | 5 | 0 | CK_PEINH_NEW | PACK3 |
| 56 | PERART | Period type | CHAR | 2 | 0 | CK_PER_ART | CK_PER_ART |
| 57 | POPER | Posting Period | NUMC | 3 | 0 | POPER | POPER |
| 58 | POSNR | Item | NUMC | 6 | 0 | CK_MLPOS | NUMC6 |
| 59 | PROCESS | Production Process No | NUMC | 12 | 0 | CKML_F_PROCNR | CK_KALNR |
| 60 | PSART | Item type | CHAR | 2 | 0 | CK_PSART | CK_VGART |
| 61 | PSPNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 62 | PTYP | Orig. process catego | CHAR | 4 | 0 | CK_PTYP_ORG | CKML_PROZESS_TYP |
| 63 | PTYP_BVALT | PrcCat- pro/cons.alt | CHAR | 4 | 0 | CK_PTYP_BVALT | CKML_PROZESS_TYP |
| 64 | PTYP_KAT | Cat-process category | CHAR | 4 | 0 | CK_PTYP_KAT | CKML_PROZESS_TYP |
| 65 | PTYP_PROC | Process-Process Cat. | CHAR | 4 | 0 | CK_PTYP_PROC | CKML_PROZESS_TYP |
| 66 | PVERS | SAP Release | CHAR | 4 | 0 | SYSAPRL | SYCHAR04 |
| 67 | PVPRS_DIFF | Periodic unit price | CURR | 11 | 2 | CK_PVP_OLD | WERT11 |
| 68 | PVPRS_DIFF_PERCT | Periodic unit price diff % | DEC | 5 | 2 | PERCT | DEC3_2 |
| 69 | PVPRS_NEW | Periodic unit price | CURR | 11 | 2 | CK_PVP_NEW | WERT11 |
| 70 | PVPRS_OLD | Periodic unit price | CURR | 11 | 2 | CK_PVP_OLD | WERT11 |
| 71 | SALK3 | Amount | CURR | 15 | 2 | CK_SALK3D | WERTV8 |
| 72 | SALK3UM | Sub.adj.- change PUP | CURR | 15 | 2 | CK_SALUM | WERTV8 |
| 73 | SALK3UMS | SubAdj change std pr | CURR | 15 | 2 | CK_SALUMS | WERTV8 |
| 74 | SALK3_OLD | Total Inventory Val. | CURR | 15 | 2 | CK_SALK3_1 | WERTV8 |
| 75 | SALKV | Amount (information) | CURR | 15 | 2 | CK_SALKVD | WERTV8 |
| 76 | SALKV_OLD | Value/periodic unit price | CURR | 15 | 2 | CK_SALKV_1 | WERTV8 |
| 77 | SOBKZ | Special Stock | CHAR | 1 | 0 | SOBKZ | SOBKZ |
| 78 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 79 | STATUS | Period status | NUMC | 2 | 0 | CK_MLSTAT | CK_MLSTAT |
| 80 | STORNO | Reversal document | CHAR | 1 | 0 | CK_STORNO | XFELD |
| 81 | STPRS | Standard price | CURR | 11 | 2 | STPRS | WERT11V |
| 82 | STPRS_DIFF | Standard price | CURR | 11 | 2 | CK_STP_OLD | WERT11 |
| 83 | STPRS_DIFF_PERCT | Standard price diff % | DEC | 5 | 2 | PERCT | DEC3_2 |
| 84 | STPRS_NEW | Standard price | CURR | 11 | 2 | CK_STP_NEW | WERT11 |
| 85 | STPRS_OLD | Standard price | CURR | 11 | 2 | CK_STP_OLD | WERT11 |
| 86 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 87 | TRANS_DESC | Text | CHAR | 30 | 0 | TEXT30 | TEXT30 |
| 88 | UMBEW | Revaluate material | CHAR | 1 | 0 | CK_UMBEWERTEN | XFELD |
| 89 | UNTPER | Value structure type | NUMC | 3 | 0 | CK_UNTPER | CK_UNTPER |
| 90 | URZEILE | Original item in material or invoice document | NUMC | 6 | 0 | CK_URZEILE | NUMC6 |
| 91 | USNAM | User name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 92 | VBELN | SD Document | CHAR | 10 | 0 | VBELN | VBELN |
| 93 | VBELP | Item (SD) | NUMC | 6 | 0 | POSNR | POSNR |
| 94 | VERPR | Moving price | CURR | 11 | 2 | VERPR | WERT11V |
| 95 | VGART | ML transaction type | CHAR | 2 | 0 | CK_VGART | CK_VGART |
| 96 | VPRSV | Price control | CHAR | 1 | 0 | VPRSV | VPRSV |
| 97 | VPRSV_CHANGED | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 98 | VPRSV_NEW | Price control | CHAR | 1 | 0 | VPRSV | VPRSV |
| 99 | VPRSV_OLD | Price control | CHAR | 1 | 0 | VPRSV | VPRSV |
| 100 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 101 | XABRERR | Errors with PrDeterm. | CHAR | 1 | 0 | CK_XABRERR | CK_XERROR |
| 102 | XKONCHK | Switch off consistency chck | CHAR | 1 | 0 | CK_XKONCHK | CK_XKONCHK |
| 103 | XOBEW | Vendor stock valuation | CHAR | 1 | 0 | XOBEW | XFELD |
| 104 | ZUKBEW | Future val. price | CHAR | 1 | 0 | CK_ZUKBEW | XFELD |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 104 parameters listed in the Parameters Reference Table above.

**ASCHEMA** (Matl Update Struct.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**AWORG** (Reference org. unit):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**AWREF** (Reference document):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**AWSYS** (Logical System):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**AWTYP** (Reference procedure):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BACKDAYS** (Backdays):

Number of days to look back from today when building the default monitoring window. When no date range is supplied, the EI uses today minus this value as the start of the window and applies it to the date field selected by DATE_REF_FLD.

**BDATJ** (Year):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BELNR** (Document Number):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BELNR_EA** (Document Number):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BELNR_MA** (Document Number):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BEWARTGRP** (Movement Type Group):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BKLAS** (Valuation Class):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BLDAT** (Document Date):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUDAT** (Posting Date):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUKRS** (Company Code):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BVALT** (Procure.alt./process):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BWKEY** (Valuation Area):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BWMOD** (Valuation Grpg Code):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BWTAR** (Valuation Type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**COMP_CODE_DESC** (Company Name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CPUDT** (Entry Date):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CPUTM** (Time of Entry):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CSPLIT** (Apportionment Struct):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CURTP** (Crcy type/val.view):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**DATE_REF_FLD** (DATE reference field):

Name of the date field used as the reference for the default date range. Determines which date (e.g. posting date, entry date) is used when no explicit range is supplied.

**DATE_REF_FLD Options:**
- Values are function-specific; common examples include BUDAT, CPUDT, BLDAT (see underlying material ledger/price change function).

**ERZKALK** (Std Cost Est Price):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**ERZKALK Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**GLVOR** (Business Transaction):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**GSBER** (Business Area):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KALNR** (Cost Estimate Number):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KATEGORIE** (Category):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KJAHR** (Document Year):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KJAHR_EA** (Document Year):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KJAHR_MA** (Document Year):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KTOPL** (Chart of Accounts):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KZBWS** (Spec. stk valuation):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**KZBWS Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**LANGU** (Language):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LBKUM** (Quantity):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LBKUM_OLD** (Total stock):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LIFNR** (Supplier):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LOGSYS** (Logical System):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MANPAE_S** (Manual price change):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**MANPAE_S Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**MANPAE_V** (Manual price change):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**MANPAE_V Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**MATERIAL_DESC** (Material Description):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MATNR** (Material):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MEINS** (Unit of Measure):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MLAST** (Material price determination: control):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**MLAST Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**MODIF** (Type of BOM item):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**MODIF Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**NAME_FIRST** (First name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**NAME_LAST** (Last name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**NAME_TEXT** (Complete name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PEINH** (Price unit):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PEINH_DIFF** (Price unit):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PEINH_DIFF_PRCT** (Price unit diff %):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PEINH_MBEW** (Price unit):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PEINH_NEW** (New price unit):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PERART** (Period type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**POPER** (Posting Period):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**POSNR** (Item):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PROCESS** (Production Process No):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PSART** (Item type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PSPNR** (WBS Element):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PTYP** (Orig. process catego):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PTYP_BVALT** (PrcCat- pro/cons.alt):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PTYP_KAT** (Cat-process category):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PTYP_PROC** (Process-Process Cat.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PVERS** (SAP Release):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PVPRS_DIFF** (Periodic unit price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PVPRS_DIFF_PERCT** (Periodic unit price diff %):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PVPRS_NEW** (Periodic unit price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PVPRS_OLD** (Periodic unit price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SALK3** (Amount):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SALK3UM** (Sub.adj.- change PUP):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SALK3UMS** (SubAdj change std pr):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SALK3_OLD** (Total Inventory Val.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SALKV** (Amount (information)):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SALKV_OLD** (Value/periodic unit price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SOBKZ** (Special Stock):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**SOBKZ Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**SPART** (Division):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**STATUS** (Period status):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**STORNO** (Reversal document):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**STORNO Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**STPRS** (Standard price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**STPRS_DIFF** (Standard price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**STPRS_DIFF_PERCT** (Standard price diff %):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**STPRS_NEW** (Standard price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**STPRS_OLD** (Standard price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**TCODE** (Transaction Code):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**TRANS_DESC** (Text):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMBEW** (Revaluate material):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**UMBEW Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**UNTPER** (Value structure type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**URZEILE** (Original item in material or invoice document):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**USNAM** (User name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VBELN** (SD Document):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VBELP** (Item (SD)):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VERPR** (Moving price):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VGART** (ML transaction type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VPRSV** (Price control):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**VPRSV Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**VPRSV_CHANGED** (Single-Character Flag):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**VPRSV_CHANGED Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**VPRSV_NEW** (Price control):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**VPRSV_NEW Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**VPRSV_OLD** (Price control):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**VPRSV_OLD Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**WAERS** (Currency):

Company code local currency. Business meaning: currency in which amounts are stored for the plant/company code.

**XABRERR** (Errors with PrDeterm.):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**XABRERR Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XKONCHK** (Switch off consistency chck):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**XKONCHK Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XOBEW** (Vendor stock valuation):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**XOBEW Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**ZUKBEW** (Future val. price):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**ZUKBEW Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.


### Parameter Relationships

**Percentage threshold parameters:**
- PEINH_DIFF_PRCT (price unit difference percentage), PVPRS_DIFF_PERCT (periodic unit price difference percentage), and STPRS_DIFF_PERCT (standard price difference percentage) work together to filter which price change lines are included. Each represents the percentage change in the respective dimension; the EI calculates these from before/after values and filters by the configured ranges. Use them to focus on material ledger documents where the price unit, moving average price, or standard price changed by more than a given percentage.

**Valuation type and business transaction:**
- VGART (material ledger transaction type) and GLVOR (business transaction) restrict which documents are selected from the underlying material ledger/price change function. In the wrapper logic, VGART is set to 'PC' (price change) for the first call and excluded for the second; GLVOR defaults to 'RMWE' when not supplied. They work together to scope the document set (e.g. price change documents, material ledger revaluation).

**Price and price control:**
- VPRSV (price control), VERPR (moving average price), STPRS (standard price), and PEINH_MBEW (price unit from material master) are used together for selection or display of price-related fields. They restrict or populate the result so that analysis can focus on moving average vs standard price and on specific price or price unit ranges.


### Default Values

- **VGART** — When not supplied, the EI uses 'PC' (price change) for the first call to the underlying price change function; for the second call it excludes 'PC' so both price change and other material ledger documents can be considered for comparison.
- **GLVOR** — Default: `RMWE` (when no range is supplied); business transaction for material ledger revaluation.

**Note:** The wrapper function does not expose BACKDAYS or DATE_REF_FLD; the underlying function F_SW_10_02_MAT_PRICE_CNG / F_SW_10_02_MAT_PRCE_CNG2 may use their own defaults for the date window. Percentage thresholds (PEINH_DIFF_PRCT, PVPRS_DIFF_PERCT, STPRS_DIFF_PERCT) have no default in the wrapper; configure them to filter by percentage change.

### Practical Configuration Examples

**Use Case 1: Moving average price change above 20%**
```
VGART = PC
GLVOR = RMWE
PVPRS_DIFF_PERCT = 20 (or range GE 20)
```
**Purpose:** Focus on material ledger price change documents where the periodic (moving average) unit price changed by at least 20% for exception review and approval.

**Use Case 2: Price unit and standard price percentage thresholds**
```
PEINH_DIFF_PRCT = 15
STPRS_DIFF_PERCT = 10
VPRSV = S
```
**Purpose:** Monitor price changes where the price unit changed by at least 15% and the standard price by at least 10%, for materials with standard price control.

**Use Case 3: Multiple percentage dimensions**
```
PEINH_DIFF_PRCT = 5
PVPRS_DIFF_PERCT = 20
STPRS_DIFF_PERCT = 20
GLVOR = RMWE
```
**Purpose:** Identify documents with material price unit change (5%+) and either moving average or standard price change (20%+) for valuation and audit review.

**Use Case 4: Valuation type and business transaction**
```
VGART = PC
GLVOR = RMWE
VERPR = (range)
STPRS = (range)
```
**Purpose:** Restrict to price change transactions and revaluation, with optional moving average and standard price ranges for detailed analysis.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_02_MAT_PRICE_CNG | ASCHEMA | Material Update Structure | CHAR(4) | CKML_ASCHEMA |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | AWORG | Reference Organizational Units | CHAR(10) | AWORG |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | AWREF | Reference Document Number | CHAR(10) | AWREF |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | AWSYS | Logical System | CHAR(10) | LOGSYSTEM |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | AWTYP | Reference Transaction | CHAR(5) | AWTYP |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BDATJ | Posting Date YYYY | NUMC(4) | BDATJ |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BELNR | Number of a Material Ledger Document/Price Change Document | CHAR(10) | CK_BELNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BELNR_EA | Number of a Material Ledger Document/Price Change Document | CHAR(10) | CK_BELNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BELNR_MA | Number of a Material Ledger Document/Price Change Document | CHAR(10) | CK_BELNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BEWARTGRP | Movement Type Group for ML Update | CHAR(2) | CKML_MLBWG |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BKLAS | Valuation Class | CHAR(4) | BKLAS |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BLDAT | Document Date in Document | DATS(8) | BLDAT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BVALT | Procurement alternative/process | NUMC(12) | CKML_ALPROCNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BWKEY | Valuation Area | CHAR(4) | BWKEY |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BWMOD | Valuation Grouping Code | CHAR(4) | BWMOD |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | COMP_CODE_DESC | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | CPUTM | Time of Entry | TIMS(6) | CPUTM |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | CSPLIT | Apportionment Structure | CHAR(4) | CSPLIT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | CURTP | Currency Type and Valuation View | CHAR(2) | CURTP |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | ERZKALK | Price from Standard Cost Estimate | CHAR(1) | CK_ERZKALK |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | GLVOR | Business Transaction | CHAR(4) | GLVOR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | KALNR | Cost Estimate Number for Cost Est. w/o Qty Structure | NUMC(12) | CK_KALNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | KATEGORIE | Category in Material Update Structure | CHAR(2) | CKML_KATEGORIE |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | KJAHR | Material Ledger Document/Price Change Document: Storage Year | NUMC(4) | CK_KJAHR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | KJAHR_EA | Material Ledger Document/Price Change Document: Storage Year | NUMC(4) | CK_KJAHR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | KJAHR_MA | Material Ledger Document/Price Change Document: Storage Year | NUMC(4) | CK_KJAHR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | KTOPL | Chart of Accounts | CHAR(4) | KTOPL |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | KZBWS | Valuation of Special Stock | CHAR(1) | KZBWS |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | LBKUM | Quantity by which the total valuated inventory has changed | QUAN(15,3) | CK_LBKUMD |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | LBKUM_OLD | Total valuated stock | QUAN(15,3) | CK_LBKUM |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | LOGSYS | Logical System | CHAR(10) | LOGSYSTEM |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | MANPAE_S | Indicator: price manually overridden | CHAR(1) | CK_MANPAE |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | MANPAE_V | Indicator: price manually overridden | CHAR(1) | CK_MANPAE |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | MATERIAL_DESC | Material Description (Short Text) | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | MEINS | Unit of Measure for Material Valuation | UNIT(3) | CKML_MEINS |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | MLAST | Material Price Determination: Control | CHAR(1) | CK_ML_ABST |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | MODIF | Type of BOM item | CHAR(1) | CK_MODIF |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | NAME_FIRST | First name | CHAR(40) | NAME_FIRST |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | NAME_LAST | Last name | CHAR(40) | NAME_LAST |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | NAME_TEXT | Full Name of Person | CHAR(80) | NAME_TEXT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PEINH | Price unit | DEC(5) | CK_PEINH_1 |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PEINH_DIFF | Price unit | DEC(5) | CK_PEINH_1 |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PEINH_DIFF_PRCT | Percentage | DEC(5,2) | PERCT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PEINH_MBEW | Price Unit | DEC(5) | PEINH |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PEINH_NEW | New price unit for the material ledger | DEC(5) | CK_PEINH_NEW |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PERART | Type of period | CHAR(2) | CK_PER_ART |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | POPER | Posting period | NUMC(3) | POPER |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | POSNR | Item in Material Ledger Document | NUMC(6) | CK_MLPOS |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PROCESS | Production Process | NUMC(12) | CKML_F_PROCNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PSART | Item type in material ledger document | CHAR(2) | CK_PSART |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PSPNR | Work Breakdown Structure Element (WBS Element) | NUMC(8) | PS_PSP_PNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PTYP | Original process category | CHAR(4) | CK_PTYP_ORG |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PTYP_BVALT | Process category for procurement alt. or consuption alt. | CHAR(4) | CK_PTYP_BVALT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PTYP_KAT | Process category for category determination | CHAR(4) | CK_PTYP_KAT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PTYP_PROC | Process category of the process | CHAR(4) | CK_PTYP_PROC |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PVERS | Release of SAP System | CHAR(4) | SYSAPRL |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PVPRS_DIFF | Old periodic unit price | CURR(11,2) | CK_PVP_OLD |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PVPRS_DIFF_PERCT | Percentage | DEC(5,2) | PERCT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PVPRS_NEW | New periodic unit price | CURR(11,2) | CK_PVP_NEW |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | PVPRS_OLD | Old periodic unit price | CURR(11,2) | CK_PVP_OLD |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | SALK3 | Amount by which the total valuated inventory changes | CURR(15,2) | CK_SALK3D |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | SALK3UM | Sub. Adjust. to Stock Value due to Change in Per. Unit Price | CURR(15,2) | CK_SALUM |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | SALK3UMS | Sub. Adj. to Inv. Value due to a Change in the Std Price | CURR(15,2) | CK_SALUMS |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | SALK3_OLD | Value of total valuated stock | CURR(15,2) | CK_SALK3_1 |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | SALKV | Amount by which the total valuated inventory (info.) changes | CURR(15,2) | CK_SALKVD |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | SALKV_OLD | Value based on the periodic unit price (only with pr.ctrl S) | CURR(15,2) | CK_SALKV_1 |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | SOBKZ | Special Stock Indicator | CHAR(1) | SOBKZ |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | STATUS | Material ledger period status | NUMC(2) | CK_MLSTAT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | STORNO | Indicator: material ledger reversal document | CHAR(1) | CK_STORNO |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | STPRS | Standard price | CURR(11,2) | STPRS |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | STPRS_DIFF | Old standard price | CURR(11,2) | CK_STP_OLD |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | STPRS_DIFF_PERCT | Percentage | DEC(5,2) | PERCT |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | STPRS_NEW | New standard price | CURR(11,2) | CK_STP_NEW |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | STPRS_OLD | Old standard price | CURR(11,2) | CK_STP_OLD |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | TCODE | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | TRANS_DESC | Text (30 Characters) | CHAR(30) | TEXT30 |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | UMBEW | The materials should be revaluated | CHAR(1) | CK_UMBEWERTEN |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | UNTPER | Value Structure Type | NUMC(3) | CK_UNTPER |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | URZEILE | Original item in material or invoice document | NUMC(6) | CK_URZEILE |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | USNAM | User name | CHAR(12) | USNAM |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | VBELN | Sales and Distribution Document Number | CHAR(10) | VBELN |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | VBELP | Item number of the SD document | NUMC(6) | POSNR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | VERPR | Moving Average Price/Periodic Unit Price | CURR(11,2) | VERPR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | VGART | Material ledger: transaction type | CHAR(2) | CK_VGART |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | VPRSV | Price control indicator | CHAR(1) | VPRSV |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | VPRSV_CHANGED | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | VPRSV_NEW | Price control indicator | CHAR(1) | VPRSV |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | VPRSV_OLD | Price control indicator | CHAR(1) | VPRSV |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | XABRERR | Indicates that error(s) occurred during last price determ. | CHAR(1) | CK_XABRERR |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | XKONCHK | Switch off consistency check | CHAR(1) | CK_XKONCHK |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | XOBEW | Vendor Stock Valuation Indicator | CHAR(1) | XOBEW |
| /SKN/S_SW_10_02_MAT_PRICE_CNG | ZUKBEW | Indicator: future valuation price activated | CHAR(1) | CK_ZUKBEW |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_MAT_PR_CNG_UN .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_MAT_PRICE_CNG OPTIONAL
*"----------------------------------------------------------------------
  DATA : SY_TABIX LIKE SY-TABIX .
*--- Check Alert Information
  DATA: LS_DATA LIKE LINE OF T_DATA,
        LT_DATA LIKE TABLE OF LS_DATA.
  DATA: LS_SELECT LIKE LINE OF T_SELECT,
        LT_SELECT LIKE TABLE OF LS_SELECT.
  DATA: LV_ALERT TYPE CHAR1.
  DATA: LV_PER TYPE F.
  DATA: LV_PVPRS_NEW TYPE F.
  ""data_multy: TCODE         TCODE.
  DATA_MULTY: VGART             CK_VGART,
              GLVOR             GLVOR,
              PEINH_DIFF_PRCT   PERCT,
              PVPRS_DIFF_PERCT  PERCT,
              STPRS_DIFF_PERCT  PERCT,
*** 15.09.22++
              VPRSV             VPRSV,
              VERPR             VERPR,
              STPRS             STPRS,
              PEINH_MBEW        PEINH.
*** 15.09.22++
  SELECT_MULTY: GLVOR,
                PEINH_DIFF_PRCT,
                PVPRS_DIFF_PERCT,
                STPRS_DIFF_PERCT,
**** 15.09.22++
                VPRSV,
                VERPR,
                STPRS,
                PEINH_MBEW.
**** 15.09.22++
  LT_SELECT[] = T_SELECT[].
**  refresh r_TCODE.
**  rs_TCODE-sign = 'I'.
**   rs_TCODE-option = 'EQ'.
**    rs_TCODE-low = 'MR21'.
**     append rs_TCODE to r_TCODE.
**
**  loop at r_TCODE into rs_TCODE.
**    MOVE-CORRESPONDING rs_TCODE to ls_SELECT.
**    ls_SELECT-FIELDNM = 'TCODE'.
**    append ls_SELECT to lt_SELECT.
**  endloop.
  REFRESH R_VGART.
  RS_VGART-SIGN = 'I'.
  RS_VGART-OPTION = 'EQ'.
  RS_VGART-LOW = 'PC'.
  APPEND RS_VGART TO R_VGART.
  LOOP AT R_VGART INTO RS_VGART.
    MOVE-CORRESPONDING RS_VGART TO LS_SELECT.
    LS_SELECT-FIELDNM = 'VGART'.
    APPEND LS_SELECT TO LT_SELECT.
  ENDLOOP.
  CALL FUNCTION '/SKN/F_SW_10_02_MAT_PRICE_CNG'
    IMPORTING
      IS_ALERT = LV_ALERT
    TABLES
      T_SELECT = LT_SELECT
      T_DATA   = LT_DATA.
  T_DATA[] = LT_DATA[].
**  lt_SELECT[] = T_SELECT[].
**  refresh r_TCODE.
**  rs_TCODE-sign = 'E'.
**   rs_TCODE-option = 'EQ'.
**    rs_TCODE-low = 'MR21'.
**     append rs_TCODE to r_TCODE.
**
**  loop at r_TCODE into rs_TCODE.
**    MOVE-CORRESPONDING rs_TCODE to ls_SELECT.
**    ls_SELECT-FIELDNM = 'TCODE'.
**    append ls_SELECT to lt_SELECT.
**  endloop.
  LT_SELECT[] = T_SELECT[].
  REFRESH R_VGART.
  RS_VGART-SIGN   = 'E'.
  RS_VGART-OPTION = 'EQ'.
  RS_VGART-LOW    = 'PC'.
  APPEND RS_VGART TO R_VGART.
  LOOP AT R_VGART INTO RS_VGART.
    MOVE-CORRESPONDING RS_VGART TO LS_SELECT.
    LS_SELECT-FIELDNM = 'VGART'.
    APPEND LS_SELECT TO LT_SELECT.
  ENDLOOP.
  " loop at r_GLVOR into rs_GLVOR.
  IF R_GLVOR[] IS INITIAL.
    REFRESH R_GLVOR.
    RS_GLVOR-SIGN   = 'I'.
    RS_GLVOR-OPTION = 'EQ'.
    RS_GLVOR-LOW    = 'RMWE'.
    APPEND RS_GLVOR TO R_GLVOR.
    LOOP AT R_GLVOR INTO RS_GLVOR.
      MOVE-CORRESPONDING RS_GLVOR TO LS_SELECT.
      LS_SELECT-FIELDNM = 'GLVOR'.
      APPEND LS_SELECT TO LT_SELECT.
    ENDLOOP.
  ENDIF.
  REFRESH LT_DATA.
  CALL FUNCTION '/SKN/F_SW_10_02_MAT_PRCE_CNG2'
    IMPORTING
      IS_ALERT = LV_ALERT
    TABLES
      T_SELECT = LT_SELECT
      T_DATA   = LT_DATA.
  "--- Calculation aggregated values for Material
  DATA: BEGIN OF LS_MAT_AGGR,
         BELNR  TYPE CK_BELNR,
         KJAHR  TYPE CK_KJAHR,
         POSNR  TYPE CK_MLPOS,
         BDATJ  TYPE BDATJ,
         POPER  TYPE POPER,
         CURTP  TYPE CURTP,
         MATNR  TYPE MATNR,
         SALK3  TYPE CK_SALK3D,
         WAERS  TYPE WAERS,
         LBKUM  TYPE CK_LBKUMD,
         MEINS  TYPE MEINS,
         CNT   TYPE I,
        END OF LS_MAT_AGGR.
  DATA: LT_MAT_AGGR LIKE TABLE OF LS_MAT_AGGR.
  LOOP AT LT_DATA INTO LS_DATA.
    MOVE-CORRESPONDING LS_DATA TO LS_MAT_AGGR.
    CLEAR LS_MAT_AGGR-POSNR.  "!!!
    LS_MAT_AGGR-CNT = 1.
    COLLECT LS_MAT_AGGR INTO LT_MAT_AGGR.
  ENDLOOP.
  DELETE LT_MAT_AGGR WHERE CNT < 2.
  SORT LT_MAT_AGGR BY BELNR KJAHR BDATJ POPER CURTP MATNR.
  .
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX.
    READ TABLE LT_MAT_AGGR INTO LS_MAT_AGGR
               WITH KEY  BELNR = LS_DATA-BELNR
                         KJAHR = LS_DATA-KJAHR
                         BDATJ = LS_DATA-BDATJ
                         POPER = LS_DATA-POPER
                         CURTP = LS_DATA-CURTP
                         MATNR = LS_DATA-MATNR
               BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      LS_DATA-SALK3 = LS_MAT_AGGR-SALK3.
      LS_DATA-LBKUM = LS_MAT_AGGR-LBKUM.
      IF ( LS_DATA-LBKUM_OLD + LS_DATA-LBKUM ) <> 0.
        LV_PVPRS_NEW =  LS_DATA-PEINH * ( LS_DATA-SALK3_OLD + LS_DATA-SALK3 )  / ( LS_DATA-LBKUM_OLD + LS_DATA-LBKUM ).
        LS_DATA-PVPRS_NEW = LV_PVPRS_NEW.
        LS_DATA-PVPRS_DIFF = ABS( LS_DATA-PVPRS_NEW - LS_DATA-PVPRS_OLD ) .
        IF LS_DATA-PVPRS_OLD <> 0.
          LV_PER = ( LS_DATA-PVPRS_DIFF / LS_DATA-PVPRS_OLD ) * 100.
          IF LV_PER > '999.99'.
            LV_PER = '999.99'.
          ENDIF.
          LS_DATA-PVPRS_DIFF_PERCT = LV_PER.
        ENDIF.
      ENDIF.
      MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE LT_DATA WHERE  PEINH_DIFF_PRCT  NOT IN R_PEINH_DIFF_PRCT .
  DELETE LT_DATA WHERE  PVPRS_DIFF_PERCT NOT IN R_PVPRS_DIFF_PERCT.
  DELETE LT_DATA WHERE  STPRS_DIFF_PERCT NOT IN R_STPRS_DIFF_PERCT.
  APPEND LINES OF LT_DATA TO T_DATA.
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```