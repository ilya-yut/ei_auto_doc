# Exception Indicator: Material Movements - SW_10_02_MAT_MOV

## General Overview

This Exception Indicator (EI) monitors material document lines in Materials Management (MM) to identify material conversions or movements with significant nominal value change (e.g. 10K+ threshold) across configurable time windows. It provides visibility into conversion postings, amounts in local and foreign currency, and duration for audit and control.

This EI serves as an essential control for inventory and valuation oversight by:
- Enabling detection of material conversions or movements with large amount or balance changes that may require investigation or approval
- Supporting identification of exceptional postings by plant, material, document type, or user for prioritization and root-cause analysis
- Providing visibility into posting and entry dates and duration for aging and follow-up decisions
- Enabling analysis of conversion patterns (receiving/issuing material, plant, storage location, batch) for process and control improvement
- Supporting accountability by user and transaction code for audit and compliance

This monitoring enables organizations to detect unusual conversion values, aging or high-value postings requiring review, and concentration patterns that may indicate errors or fraud. The EI is particularly valuable for period-end reconciliation, material movement audits, and exception management in inventory and valuation.

The EI uses material document data (MSEG, MKPF) and material master (MARA); amounts can be converted to a foreign currency for threshold comparison.


## Problem Description

Failure to monitor material conversions or movements with significant nominal value change creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected high-value conversion postings can distort inventory valuations and cost of goods sold in period-end closing
- Significant amount or balance changes in specific documents may indicate valuation errors, reclassifications, or fraud requiring adjustment
- Unreconciled conversion exceptions can lead to misstated balance sheet and audit findings
- Delayed identification of high-value movements may complicate month-end close and require restatements
- Concentration of large conversions in certain plants or materials can mask systemic issues in controls

**Operational and Control Risks**
- Conversion documents with large nominal value changes or prolonged age may indicate incomplete or erroneous postings requiring release or correction
- Lack of visibility by plant, material, movement type, or user limits ability to prioritize reviews and resource allocation
- Unmonitored conversion patterns (receiving/issuing material, batch, storage location) can delay detection of process or master data issues
- Missing filtering by date reference (posting date vs entry date) can lead to wrong monitoring windows and missed exceptions
- Insufficient currency or amount threshold visibility restricts actionable reporting for auditors and operations

**Management Visibility and Decision-Making Risks**
- Absence of exception monitoring delays awareness of significant conversion values and aging postings
- Unidentified concentration of high-value movements by plant or document can lead to missed process improvements or fraud indicators
- Lack of duration and date visibility limits ability to escalate aging or high-value items for review
- Insufficient filtering by organizational and movement dimensions restricts actionable reporting for auditors and operations

## Suggested Resolution

**Immediate Response**
- Review the conversion or movement lines flagged by the EI to understand scope and magnitude (threshold violations, plant or document concentration)
- Verify high-value documents using material document transactions (e.g. MB03, MIGO) to confirm postings and legitimacy
- Check posting and entry dates and processing progress to determine if manual correction or approval is pending
- Identify business context for exceptions: movement type, material, plant, storage location, or user responsible

**System Assessment**
- Analyze the time window and date reference used for monitoring to ensure the lookback period and date field align with posting and entry cycles
- Examine amount and balance thresholds and currency conversion setting to ensure material exceptions are captured without noise
- Assess plant, material, movement type, and user distribution to identify patterns or master data issues
- Validate duration unit and reference date field so that status duration is interpreted correctly for prioritization
- Review receiving/issuing material, plant, and storage location for conversion-specific patterns

**Corrective Actions**
- Post corrections or reversing entries through standard MM transactions where errors or illegitimate conversions are confirmed
- For incomplete or erroneous conversions, complete or correct postings as per process
- Update master data (e.g. material, valuation type) if misconfiguration or data quality issues are found
- Adjust monitoring parameters (lookback, threshold, date reference, currency) to align with policy and re-run the EI
- Document exceptions and resolutions for audit trail; establish recurring EI runs for continuous visibility into conversion value exceptions


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ABLAD | Unloading Point | CHAR | 25 | 0 | ABLAD | TEXT25 |
| 2 | ANLN1 | Asset | CHAR | 12 | 0 | ANLN1 | ANLN1 |
| 3 | ANLN2 | Subnumber | CHAR | 4 | 0 | ANLN2 | ANLN2 |
| 4 | AUFNR | Order | CHAR | 12 | 0 | AUFNR | AUFNR |
| 5 | AUFPL | Opertn task list no. | NUMC | 10 | 0 | CO_AUFPL | AUFPL |
| 6 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 7 | BALANCE | Balance in LC | CURR | 13 | 2 | /SKN/BALANCE | WERT7 |
| 8 | BALANCE_FR | Balance in FR | CURR | 13 | 2 | /SKN/BALANCE_FR | WERT7 |
| 9 | BELNR | Document Number | CHAR | 10 | 0 | BELNR_D | BELNR |
| 10 | BELUM | Document Number | CHAR | 10 | 0 | BELNR_D | BELNR |
| 11 | BNBTR | Delivery costs | CURR | 13 | 2 | BNBTR | WERT13N |
| 12 | BPMNG | Qty in OPUn | QUAN | 13 | 3 | BPMNG | MENG13 |
| 13 | BPRME | Order Price Unit | UNIT | 3 | 0 | BBPRM | MEINS |
| 14 | BTEXT | Stock type | CHAR | 20 | 0 | BTEXT | BTEXT |
| 15 | BUALT | Amount | CURR | 13 | 2 | BUALT | WERTV7 |
| 16 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 17 | BUDAT_MKPF | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 18 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 19 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 20 | BUZEI | Line item | NUMC | 3 | 0 | BUZEI | BUZEI |
| 21 | BUZUM | Line item | NUMC | 3 | 0 | BUZEI | BUZEI |
| 22 | BWART | Movement Type | CHAR | 3 | 0 | BWART | BWART |
| 23 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 24 | CFLAG | Flag | CHAR | 1 | 0 | /SKN/E_SW_FLAG | CHECKBOX |
| 25 | CHARG | Batch | CHAR | 10 | 0 | CHARG_D | CHARG |
| 26 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 27 | CPUDT_MKPF | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 28 | CPUTM_MKPF | Time of Entry | TIMS | 6 | 0 | CPUTM | UZEIT |
| 29 | DATE_REF_FLD | DATE reference field |  | 0 | 0 |  |  |
| 30 | DMBTR | Amount LC | CURR | 13 | 2 | DMBTR | WERT7 |
| 31 | DMBTR_FR | Amount FR | CURR | 13 | 2 | /SKN/DMBTR_FR | WERT7 |
| 32 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 33 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 34 | EBELN | Purchase Order | CHAR | 10 | 0 | BSTNR | EBELN |
| 35 | EBELP | Item | NUMC | 5 | 0 | EBELP | EBELP |
| 36 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 37 | EQUNR | Equipment | CHAR | 18 | 0 | EQUNR | EQUNR |
| 38 | ERFME | Unit of Entry | UNIT | 3 | 0 | ERFME | MEINS |
| 39 | ERFMG | Qty in unit of entry | QUAN | 13 | 3 | ERFMG | MENG13 |
| 40 | FISTL | Funds Center | CHAR | 16 | 0 | FISTL | FISTL |
| 41 | FKBER | Functional Area | CHAR | 16 | 0 | FKBER | FKBER |
| 42 | GEBER | Fund | CHAR | 10 | 0 | BP_GEBER | BP_GEBER |
| 43 | GRBEW | Reason for Movement | NUMC | 4 | 0 | MB_GRBEW | MB_GRBEW |
| 44 | GRUND | Reason for man. val. | CHAR | 3 | 0 | GRUND | GRUND |
| 45 | GSBER | Business Area | CHAR | 4 | 0 | GSBER | GSBER |
| 46 | INSMK | Stock Type | CHAR | 1 | 0 | MB_INSMK | MB_INSMK |
| 47 | KDAUF | Sales Order | CHAR | 10 | 0 | KDAUF | VBELN |
| 48 | KDEIN | Sales order schedule | NUMC | 4 | 0 | KDEIN | NUM04 |
| 49 | KDPOS | Sales order item | NUMC | 6 | 0 | KDPOS | NUM06 |
| 50 | KOKRS | Controlling Area | CHAR | 4 | 0 | KOKRS | CACCD |
| 51 | KOSTL | Cost Center | CHAR | 10 | 0 | KOSTL | KOSTL |
| 52 | KTEXT | Name | CHAR | 20 | 0 | KTEXT | TEXT20 |
| 53 | KUNNR | Customer | CHAR | 10 | 0 | EKUNN | KUNNR |
| 54 | KUNNR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 55 | KZBEW | Movement indicator | CHAR | 1 | 0 | KZBEW | KZBEW |
| 56 | KZEAR | Final Issue | CHAR | 1 | 0 | KZEAR | XFELD |
| 57 | KZVBR | Consumption | CHAR | 1 | 0 | KZVBR | KZVBR |
| 58 | KZZUG | Receipt Indicator | CHAR | 1 | 0 | KZZUG | KZZUG |
| 59 | LANGU | Language |  | 0 | 0 |  |  |
| 60 | LBKUM | Valuated stock | QUAN | 13 | 3 | LBKUM_MSEG | MENGV13 |
| 61 | LGORT | Storage Location | CHAR | 4 | 0 | LGORT_D | LGORT |
| 62 | LGORT_DESC | Descr. of Storage Loc. | CHAR | 16 | 0 | LGOBE | TEXT16 |
| 63 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 64 | LIFNR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 65 | LINE_ID | Identification | NUMC | 6 | 0 | MB_LINE_ID | MB_LINE_ID |
| 66 | MAKTX | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 67 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 68 | MBLNR | Material Document | CHAR | 10 | 0 | MBLNR | BELNR |
| 69 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 70 | MENGE | Quantity | QUAN | 13 | 3 | MENGE_D | MENG13 |
| 71 | MJAHR | Material Doc. Year | NUMC | 4 | 0 | MJAHR | GJAHR |
| 72 | MTART | Material Type | CHAR | 4 | 0 | MTART | MTART |
| 73 | NPLNR | Network | CHAR | 12 | 0 | NPLNR | AUFNR |
| 74 | PARBU | Clearing CoCode | CHAR | 4 | 0 | PARBU | BUKRS |
| 75 | PARENT_ID | Parent line ID | NUMC | 6 | 0 | MB_PARENT_ID | MB_LINE_ID |
| 76 | PARGB | Trading part.BA | CHAR | 4 | 0 | PARGB | GSBER |
| 77 | PLPLA | Distr. differences | CHAR | 10 | 0 | PLPLA | PLPLA |
| 78 | PRCTR | Profit Center | CHAR | 10 | 0 | PRCTR | PRCTR |
| 79 | PS_PSP_PNR | WBS Element | NUMC | 8 | 0 | PS_PSP_PNR | PS_POSNR |
| 80 | RSNUM | Reservation | NUMC | 10 | 0 | RSNUM | RSNUM |
| 81 | RSPOS | Item No.Stock Transfer Reserv. | NUMC | 4 | 0 | RSPOS | RSPOS |
| 82 | SAKTO | G/L Account | CHAR | 10 | 0 | SAKNR | SAKNR |
| 83 | SALK3 | Totl val. bf.posting | CURR | 13 | 2 | SALK3_MSEG | WERT13N |
| 84 | SGTXT | Text | CHAR | 50 | 0 | SGTXT | TEXT50 |
| 85 | SHKZG | Debit/Credit Ind. | CHAR | 1 | 0 | SHKZG | SHKZG |
| 86 | SOBKZ | Special Stock | CHAR | 1 | 0 | SOBKZ | SOBKZ |
| 87 | TCODE2_MKPF | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 88 | UMBAR | Val. Type Tfr Batch | CHAR | 10 | 0 | UMBAR | BWTAR |
| 89 | UMCHA | Receiving Batch | CHAR | 10 | 0 | UMCHA | CHARG |
| 90 | UMLGO | Receiving stor. loc. | CHAR | 4 | 0 | UMLGO | LGORT |
| 91 | UMLGO_DESC | Descr. of Storage Loc. | CHAR | 16 | 0 | LGOBE | TEXT16 |
| 92 | UMMAT | Receiving Material | CHAR | 18 | 0 | UMMAT | MATNR |
| 93 | UMSOK | Sp. ind. stock tfr. | CHAR | 1 | 0 | UMSOK | SOBKZ |
| 94 | UMWRK | Receiving plant | CHAR | 4 | 0 | UMWRK | WERKS |
| 95 | UMWRK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 96 | UMZST | Restricted-Use | CHAR | 1 | 0 | UMZST | ZUSTD |
| 97 | UMZUS | Stat. key tfr. batch | CHAR | 1 | 0 | UMZUS | ZUSCH |
| 98 | USNAM_MKPF | User name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 99 | VBELN_IM | Delivery | CHAR | 10 | 0 | VBELN_VL | VBELN |
| 100 | VBELP_IM | Item | NUMC | 6 | 0 | POSNR_VL | POSNR |
| 101 | VFDAT | SLED/BBD | DATS | 8 | 0 | VFDAT | DATUM |
| 102 | VPRSV | Price control | CHAR | 1 | 0 | VPRSV | VPRSV |
| 103 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 104 | WAERS_FR | Foreign Currency | CUKY | 5 | 0 | /SKN/WAERS_FR | WAERS |
| 105 | WEMPF | Goods recipient | CHAR | 12 | 0 | WEMPF | WEMPF |
| 106 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 107 | WERKS_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 108 | XAUTO | Item automatically created | CHAR | 1 | 0 | MB_XAUTO | MB_XAUTO |
| 109 | XBLNR_MKPF | Reference | CHAR | 16 | 0 | XBLNR | XBLNR |
| 110 | ZEILE | Material Doc.Item | NUMC | 4 | 0 | MBLPO | MBLPO |
| 111 | ZUSCH | Status key | CHAR | 1 | 0 | DZUSCH | ZUSCH |
| 112 | ZUSTD | Batch Restricted | CHAR | 1 | 0 | DZUSTD | XFELD |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 112 parameters listed in the Parameters Reference Table above.

**ABLAD** (Unloading Point):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ANLN1 - ANLN2** (Asset – Subnumber):

Main asset number and subnumber. Restrict which document lines are included or appear in the result for account assignment. Used in selection criteria or output structure as defined in the function.

**AUFNR** (Order):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**AUFPL** (Opertn task list no.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BACKDAYS** (Backdays):

Number of days to look back from today when building the default monitoring window. When no date range is supplied, the EI uses today minus this value as the start of the window and applies it to the date field selected by DATE_REF_FLD.

**BALANCE** (Balance in LC):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BALANCE_FR** (Balance in FR):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BELNR** (Document Number):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BELUM** (Document Number):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BNBTR** (Delivery costs):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BPMNG** (Qty in OPUn):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BPRME** (Order Price Unit):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BTEXT** (Stock type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUALT** (Amount):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUDAT** (Posting Date):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUDAT_MKPF** (Posting Date):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUKRS** (Company Code):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUTXT** (Company Name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUZEI** (Line item):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BUZUM** (Line item):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BWART** (Movement Type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BWTAR** (Valuation Type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CFLAG** (Flag):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**CFLAG Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**CHARG** (Batch):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CPUDT** (Entry Date):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CPUDT_MKPF** (Entry Date):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**CPUTM_MKPF** (Time of Entry):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**DATE_REF_FLD** (DATE reference field):

Name of the date field used as the reference for the default date range and for duration calculation. Determines which date (posting date or entry date in material document header) is used when no explicit range is supplied.

**DATE_REF_FLD Options:**
- **CPUDT_MKPF**: Entry date (default in code).
- **BUDAT_MKPF**: Posting date.

**DMBTR** (Amount LC):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**DMBTR_FR** (Amount FR):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**DURATION** (Duration In Time Units):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**DURATION_UNIT** (Duration Unit):

Unit for duration (e.g. days). Used with DURATION and DATE_REF_FLD for duration-based filtering.

**DURATION_UNIT Options:**
- **D**: Days (default in code).

**EBELN** (Purchase Order):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**EBELP** (Item):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ELIKZ** (Delivery Completed):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**ELIKZ Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**EQUNR** (Equipment):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ERFME** (Unit of Entry):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ERFMG** (Qty in unit of entry):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**FISTL** (Funds Center):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**FKBER** (Functional Area):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**GEBER** (Fund):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**GRBEW** (Reason for Movement):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**GRUND** (Reason for man. val.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**GSBER** (Business Area):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**INSMK** (Stock Type):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**INSMK Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**KDAUF** (Sales Order):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KDEIN** (Sales order schedule):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KDPOS** (Sales order item):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KOKRS** (Controlling Area):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KOSTL** (Cost Center):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KTEXT** (Name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KUNNR** (Customer):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KUNNR_DESC** (Name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KZBEW** (Movement indicator):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**KZBEW Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**KZEAR** (Final Issue):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**KZEAR Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**KZVBR** (Consumption):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**KZVBR Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**KZZUG** (Receipt Indicator):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**KZZUG Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**LANGU** (Language):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LBKUM** (Valuated stock):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LGORT** (Storage Location):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LGORT_DESC** (Descr. of Storage Loc.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LIFNR** (Vendor):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LIFNR_DESC** (Name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LINE_ID** (Identification):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MAKTX** (Material Description):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MATNR** (Material):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MBLNR** (Material Document):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MEINS** (Base Unit of Measure):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MENGE** (Quantity):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MJAHR** (Material Doc. Year):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MTART** (Material Type):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**NPLNR** (Network):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PARBU** (Clearing CoCode):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PARENT_ID** (Parent line ID):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PARGB** (Trading part.BA):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PLPLA** (Distr. differences):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PRCTR** (Profit Center):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PS_PSP_PNR** (WBS Element):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**RSNUM** (Reservation):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**RSPOS** (Item No.Stock Transfer Reserv.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SAKTO** (G/L Account):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SALK3** (Totl val. bf.posting):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SGTXT** (Text):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SHKZG** (Debit/Credit Ind.):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**SHKZG Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**SOBKZ** (Special Stock):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**SOBKZ Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**TCODE2_MKPF** (Transaction Code):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMBAR** (Val. Type Tfr Batch):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMCHA** (Receiving Batch):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMLGO** (Receiving stor. loc.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMLGO_DESC** (Descr. of Storage Loc.):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMMAT** (Receiving Material):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMSOK** (Sp. ind. stock tfr.):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**UMSOK Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**UMWRK** (Receiving plant):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMWRK_DESC** (Name 1):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMZST** (Restricted-Use):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**UMZUS** (Stat. key tfr. batch):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**USNAM_MKPF** (User name):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VBELN_IM** (Delivery):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VBELP_IM** (Item):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VFDAT** (SLED/BBD):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VPRSV** (Price control):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**VPRSV Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**WAERS** (Currency):

Company code local currency. Business meaning: currency in which amounts are stored for the plant/company code.

**WAERS_FR** (Foreign Currency):

Foreign/reference currency used for conversion or comparison. Business meaning: currency to which amounts are converted for reporting or threshold comparison.

**WEMPF** (Goods recipient):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**WERKS** (Plant):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**WERKS_DESC** (Name 1):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**XAUTO** (Item automatically created):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**XAUTO Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**XBLNR_MKPF** (Reference):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ZEILE** (Material Doc.Item):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ZUSCH** (Status key):

Restricts which document lines are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ZUSTD** (Batch Restricted):

Restricts which document lines are included or populates the result. CHAR(1) / flag.

**ZUSTD Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.


### Parameter Relationships

**Time and date reference parameters:**
- BACKDAYS defines the lookback window (days from today). When no date range is supplied, the EI builds the range from today minus BACKDAYS.
- DATE_REF_FLD selects which date field is used for that range: BUDAT_MKPF (posting date in material document header) or CPUDT_MKPF (entry date). The chosen field is applied to filter document dates and is also used for duration calculation.
- DURATION_UNIT is used together with the date reference when calculating status duration: the EI computes the time difference from the reference date to today in the selected unit (e.g. days) and filters and outputs duration accordingly.

**Currency conversion:**
- WAERS_FR (foreign currency) is used when amounts are converted for threshold comparison or display. When set, the EI converts local currency amounts to the specified foreign currency (e.g. for reporting or consistent threshold application across plants with different local currencies). DMBTR_FR and BALANCE_FR in the result reflect the converted amounts.

**Conversion-specific parameters (receiving/issuing):**
- UMMAT (receiving/issuing material), UMWRK (receiving/issuing plant), UMLGO (receiving/issuing storage location), UMCHA (receiving/issuing batch), UMZST, UMZUS, UMBAR, UMSOK, and related fields work together to filter and describe material conversion or transfer postings. They restrict which document lines are included and appear in the result for conversion analysis.


### Default Values

- **BACKDAYS** — Default: `1` (lookback window when no date range is supplied).
- **DATE_REF_FLD** — Default: `CPUDT_MKPF` (entry date in material document header).
- **DURATION_UNIT** — Default: `D` (days).
- **LANGU** — Default: `E` (English).

**Note:** When no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window and applies it to the date field selected by DATE_REF_FLD.

### Practical Configuration Examples

**Use Case 1: Recent high-value conversions by entry date**
```
BACKDAYS = 7
DATE_REF_FLD = CPUDT_MKPF
DURATION_UNIT = D
```
**Purpose:** Focus on material conversions or movements entered in the last 7 days, with duration in days for prioritization and audit review.

**Use Case 2: Posting-date-based monitoring**
```
BACKDAYS = 30
DATE_REF_FLD = BUDAT_MKPF
DURATION_UNIT = D
```
**Purpose:** Monitor conversions by posting date for the last 30 days for period-end and reconciliation alignment.

**Use Case 3: Conversion analysis with receiving/issuing dimensions**
```
BACKDAYS = 14
DATE_REF_FLD = CPUDT_MKPF
WERKS = 1000
UMMAT = (range)
UMWRK = (range)
```
**Purpose:** Analyze conversion postings in a specific plant and receiving/issuing material and plant range over the last 14 days by entry date.

**Use Case 4: Currency and amount focus**
```
BACKDAYS = 5
DATE_REF_FLD = BUDAT_MKPF
WAERS_FR = USD
DMBTR = (threshold range)
```
**Purpose:** Review movements in the last 5 days by posting date with amounts converted to a reference currency and filtered by amount threshold.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_02_MAT_MOVMNT | ABLAD | Unloading Point | CHAR(25) | ABLAD |
| /SKN/S_SW_10_02_MAT_MOVMNT | ANLN1 | Main Asset Number | CHAR(12) | ANLN1 |
| /SKN/S_SW_10_02_MAT_MOVMNT | ANLN2 | Asset Subnumber | CHAR(4) | ANLN2 |
| /SKN/S_SW_10_02_MAT_MOVMNT | AUFNR | Order Number | CHAR(12) | AUFNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | AUFPL | Routing number of operations in the order | NUMC(10) | CO_AUFPL |
| /SKN/S_SW_10_02_MAT_MOVMNT | BALANCE | Balance in LC | CURR(13,2) | /SKN/BALANCE |
| /SKN/S_SW_10_02_MAT_MOVMNT | BALANCE_FR | Balance in FR | CURR(13,2) | /SKN/BALANCE_FR |
| /SKN/S_SW_10_02_MAT_MOVMNT | BELNR | Accounting Document Number | CHAR(10) | BELNR_D |
| /SKN/S_SW_10_02_MAT_MOVMNT | BELUM | Accounting Document Number | CHAR(10) | BELNR_D |
| /SKN/S_SW_10_02_MAT_MOVMNT | BNBTR | Delivery costs in local currency | CURR(13,2) | BNBTR |
| /SKN/S_SW_10_02_MAT_MOVMNT | BPMNG | Quantity in Purchase Order Price Unit | QUAN(13,3) | BPMNG |
| /SKN/S_SW_10_02_MAT_MOVMNT | BPRME | Order Price Unit (Purchasing) | UNIT(3) | BBPRM |
| /SKN/S_SW_10_02_MAT_MOVMNT | BTEXT | Text for stock type | CHAR(20) | BTEXT |
| /SKN/S_SW_10_02_MAT_MOVMNT | BUALT | Amount posted in alternative price control | CURR(13,2) | BUALT |
| /SKN/S_SW_10_02_MAT_MOVMNT | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_02_MAT_MOVMNT | BUDAT_MKPF | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_02_MAT_MOVMNT | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_02_MAT_MOVMNT | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_02_MAT_MOVMNT | BUZEI | Number of Line Item Within Accounting Document | NUMC(3) | BUZEI |
| /SKN/S_SW_10_02_MAT_MOVMNT | BUZUM | Number of Line Item Within Accounting Document | NUMC(3) | BUZEI |
| /SKN/S_SW_10_02_MAT_MOVMNT | BWART | Movement Type (Inventory Management) | CHAR(3) | BWART |
| /SKN/S_SW_10_02_MAT_MOVMNT | BWTAR | Valuation Type | CHAR(10) | BWTAR_D |
| /SKN/S_SW_10_02_MAT_MOVMNT | CFLAG | SW: Flag (' '/'X') | CHAR(1) | /SKN/E_SW_FLAG |
| /SKN/S_SW_10_02_MAT_MOVMNT | CHARG | Batch Number | CHAR(10) | CHARG_D |
| /SKN/S_SW_10_02_MAT_MOVMNT | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_02_MAT_MOVMNT | CPUDT_MKPF | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_02_MAT_MOVMNT | CPUTM_MKPF | Time of Entry | TIMS(6) | CPUTM |
| /SKN/S_SW_10_02_MAT_MOVMNT | DMBTR | Amount in Local Currency | CURR(13,2) | DMBTR |
| /SKN/S_SW_10_02_MAT_MOVMNT | DMBTR_FR | Amount in FR | CURR(13,2) | /SKN/DMBTR_FR |
| /SKN/S_SW_10_02_MAT_MOVMNT | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_02_MAT_MOVMNT | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_02_MAT_MOVMNT | EBELN | Purchase Order Number | CHAR(10) | BSTNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | EBELP | Item Number of Purchasing Document | NUMC(5) | EBELP |
| /SKN/S_SW_10_02_MAT_MOVMNT | ELIKZ | "Delivery Completed" Indicator | CHAR(1) | ELIKZ |
| /SKN/S_SW_10_02_MAT_MOVMNT | EQUNR | Equipment Number | CHAR(18) | EQUNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | ERFME | Unit of Entry | UNIT(3) | ERFME |
| /SKN/S_SW_10_02_MAT_MOVMNT | ERFMG | Quantity in Unit of Entry | QUAN(13,3) | ERFMG |
| /SKN/S_SW_10_02_MAT_MOVMNT | FISTL | Funds Center | CHAR(16) | FISTL |
| /SKN/S_SW_10_02_MAT_MOVMNT | FKBER | Functional Area | CHAR(16) | FKBER |
| /SKN/S_SW_10_02_MAT_MOVMNT | GEBER | Fund | CHAR(10) | BP_GEBER |
| /SKN/S_SW_10_02_MAT_MOVMNT | GRBEW | Reason for Movement | NUMC(4) | MB_GRBEW |
| /SKN/S_SW_10_02_MAT_MOVMNT | GRUND | Reason for manual valuation of net assets | CHAR(3) | GRUND |
| /SKN/S_SW_10_02_MAT_MOVMNT | GSBER | Business Area | CHAR(4) | GSBER |
| /SKN/S_SW_10_02_MAT_MOVMNT | INSMK | Stock Type | CHAR(1) | MB_INSMK |
| /SKN/S_SW_10_02_MAT_MOVMNT | KDAUF | Sales Order Number | CHAR(10) | KDAUF |
| /SKN/S_SW_10_02_MAT_MOVMNT | KDEIN | Delivery Schedule for Sales Order | NUMC(4) | KDEIN |
| /SKN/S_SW_10_02_MAT_MOVMNT | KDPOS | Item Number in Sales Order | NUMC(6) | KDPOS |
| /SKN/S_SW_10_02_MAT_MOVMNT | KOKRS | Controlling Area | CHAR(4) | KOKRS |
| /SKN/S_SW_10_02_MAT_MOVMNT | KOSTL | Cost Center | CHAR(10) | KOSTL |
| /SKN/S_SW_10_02_MAT_MOVMNT | KTEXT | General Name | CHAR(20) | KTEXT |
| /SKN/S_SW_10_02_MAT_MOVMNT | KUNNR | Account Number of Customer | CHAR(10) | EKUNN |
| /SKN/S_SW_10_02_MAT_MOVMNT | KUNNR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_02_MAT_MOVMNT | KZBEW | Movement Indicator | CHAR(1) | KZBEW |
| /SKN/S_SW_10_02_MAT_MOVMNT | KZEAR | Final Issue for This Reservation | CHAR(1) | KZEAR |
| /SKN/S_SW_10_02_MAT_MOVMNT | KZVBR | Consumption Posting | CHAR(1) | KZVBR |
| /SKN/S_SW_10_02_MAT_MOVMNT | KZZUG | Receipt Indicator | CHAR(1) | KZZUG |
| /SKN/S_SW_10_02_MAT_MOVMNT | LBKUM | Total valuated stock before the posting | QUAN(13,3) | LBKUM_MSEG |
| /SKN/S_SW_10_02_MAT_MOVMNT | LGORT | Storage Location | CHAR(4) | LGORT_D |
| /SKN/S_SW_10_02_MAT_MOVMNT | LGORT_DESC | Description of Storage Location | CHAR(16) | LGOBE |
| /SKN/S_SW_10_02_MAT_MOVMNT | LIFNR | Vendor Account Number | CHAR(10) | ELIFN |
| /SKN/S_SW_10_02_MAT_MOVMNT | LIFNR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_02_MAT_MOVMNT | LINE_ID | Unique identification of document line | NUMC(6) | MB_LINE_ID |
| /SKN/S_SW_10_02_MAT_MOVMNT | MAKTX | Material Description (Short Text) | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_MAT_MOVMNT | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | MBLNR | Number of Material Document | CHAR(10) | MBLNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_02_MAT_MOVMNT | MENGE | Quantity | QUAN(13,3) | MENGE_D |
| /SKN/S_SW_10_02_MAT_MOVMNT | MJAHR | Material Document Year | NUMC(4) | MJAHR |
| /SKN/S_SW_10_02_MAT_MOVMNT | MTART | Material Type | CHAR(4) | MTART |
| /SKN/S_SW_10_02_MAT_MOVMNT | NPLNR | Network Number for Account Assignment | CHAR(12) | NPLNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | PARBU | Clearing company code | CHAR(4) | PARBU |
| /SKN/S_SW_10_02_MAT_MOVMNT | PARENT_ID | Identifier of immediately superior line | NUMC(6) | MB_PARENT_ID |
| /SKN/S_SW_10_02_MAT_MOVMNT | PARGB | Trading partner's business area | CHAR(4) | PARGB |
| /SKN/S_SW_10_02_MAT_MOVMNT | PLPLA | Distribution of Differences | CHAR(10) | PLPLA |
| /SKN/S_SW_10_02_MAT_MOVMNT | PRCTR | Profit Center | CHAR(10) | PRCTR |
| /SKN/S_SW_10_02_MAT_MOVMNT | PS_PSP_PNR | Work Breakdown Structure Element (WBS Element) | NUMC(8) | PS_PSP_PNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | RSNUM | Number of Reservation/Dependent Requirement | NUMC(10) | RSNUM |
| /SKN/S_SW_10_02_MAT_MOVMNT | RSPOS | Item Number of Reservation/Dependent Requirement | NUMC(4) | RSPOS |
| /SKN/S_SW_10_02_MAT_MOVMNT | SAKTO | G/L Account Number | CHAR(10) | SAKNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | SALK3 | Value of total valuated stock before the posting | CURR(13,2) | SALK3_MSEG |
| /SKN/S_SW_10_02_MAT_MOVMNT | SGTXT | Item Text | CHAR(50) | SGTXT |
| /SKN/S_SW_10_02_MAT_MOVMNT | SHKZG | Debit/Credit Indicator | CHAR(1) | SHKZG |
| /SKN/S_SW_10_02_MAT_MOVMNT | SOBKZ | Special Stock Indicator | CHAR(1) | SOBKZ |
| /SKN/S_SW_10_02_MAT_MOVMNT | TCODE2_MKPF | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMBAR | Valuation Type of Transfer Batch | CHAR(10) | UMBAR |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMCHA | Receiving/Issuing Batch | CHAR(10) | UMCHA |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMLGO | Receiving/Issuing Storage Location | CHAR(4) | UMLGO |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMLGO_DESC | Description of Storage Location | CHAR(16) | LGOBE |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMMAT | Receiving/Issuing Material | CHAR(18) | UMMAT |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMSOK | Special Stock Indicator for Physical Stock Transfer | CHAR(1) | UMSOK |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMWRK | Receiving/Issuing Plant | CHAR(4) | UMWRK |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMWRK_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMZST | Status of Transfer Batch | CHAR(1) | UMZST |
| /SKN/S_SW_10_02_MAT_MOVMNT | UMZUS | Status key of transfer batch | CHAR(1) | UMZUS |
| /SKN/S_SW_10_02_MAT_MOVMNT | USNAM_MKPF | User name | CHAR(12) | USNAM |
| /SKN/S_SW_10_02_MAT_MOVMNT | VBELN_IM | Delivery | CHAR(10) | VBELN_VL |
| /SKN/S_SW_10_02_MAT_MOVMNT | VBELP_IM | Delivery Item | NUMC(6) | POSNR_VL |
| /SKN/S_SW_10_02_MAT_MOVMNT | VFDAT | Shelf Life Expiration or Best-Before Date | DATS(8) | VFDAT |
| /SKN/S_SW_10_02_MAT_MOVMNT | VPRSV | Price control indicator | CHAR(1) | VPRSV |
| /SKN/S_SW_10_02_MAT_MOVMNT | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_02_MAT_MOVMNT | WAERS_FR | Foreign Currency | CUKY(5) | /SKN/WAERS_FR |
| /SKN/S_SW_10_02_MAT_MOVMNT | WEMPF | Goods Recipient/Ship-To Party | CHAR(12) | WEMPF |
| /SKN/S_SW_10_02_MAT_MOVMNT | WERKS | Plant | CHAR(4) | WERKS_D |
| /SKN/S_SW_10_02_MAT_MOVMNT | WERKS_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_02_MAT_MOVMNT | XAUTO | Item automatically created | CHAR(1) | MB_XAUTO |
| /SKN/S_SW_10_02_MAT_MOVMNT | XBLNR_MKPF | Reference Document Number | CHAR(16) | XBLNR |
| /SKN/S_SW_10_02_MAT_MOVMNT | ZEILE | Item in Material Document | NUMC(4) | MBLPO |
| /SKN/S_SW_10_02_MAT_MOVMNT | ZUSCH | Batch status key | CHAR(1) | DZUSCH |
| /SKN/S_SW_10_02_MAT_MOVMNT | ZUSTD | Batch in Restricted-Use Stock | CHAR(1) | DZUSTD |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_MAT_MOVMNT3 .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_MAT_MOVMNT OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_RATE,
           WAERS TYPE MSEG-WAERS,
           RATE  TYPE UKURS_CURR,
         END OF TY_RATE,
         TT_RATE TYPE STANDARD TABLE OF TY_RATE.
  CONSTANTS: C_MSEG TYPE TABNAME VALUE 'MSEG'.
  DATA_SINGLE: BACKDAYS       INT4,
               DATE_REF_FLD   NAME_FELD,
               LANGU          SPRAS,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               WAERS_FR       WAERS                    " Foreign Currency
               .
* Set default parameters
  LV_BACKDAYS      = 1.
  LV_DATE_REF_FLD  = 'CPUDT_MKPF'.
  LV_LANGU         = 'E'.
  LV_DURATION_UNIT = 'D'.
*  lv_waers_fr      = 'USD'.             " --17.09.20 Leon's request
  SELECT_SINGLE: BACKDAYS,
                 DATE_REF_FLD,
                 LANGU,
                 DURATION_UNIT,
                 WAERS_FR.
  DATA_MULTY: BWART       CHAR3,
              XAUTO       CHAR1,
              MJAHR       MJAHR,
              MATNR       CHAR18,
              MBLNR       MBLNR,
              WERKS       CHAR4,
              LGORT       CHAR4,
              INSMK       CHAR1,
              ZUSCH       CHAR1,
              ZUSTD       CHAR1,
              SOBKZ       CHAR1,
              USNAM_MKPF  MSEG-USNAM_MKPF,
              TCODE2_MKPF MSEG-TCODE2_MKPF,
              VBELN_IM    VBELN_VL,
              LIFNR       LIFNR,
              KUNNR       CHAR10,
              KDAUF       KDAUF,
              SHKZG       SHKZG,
              WAERS       WAERS,
              DMBTR       DMBTR,
              DMBTR_FR    DMBTR,
              BALANCE     DMBTR,
              BALANCE_FR  DMBTR,
              BNBTR       BNBTR,
              BUALT       BUALT,
              BWTAR       CHAR10,
              EBELN       BSTNR,
              ELIKZ       ELIKZ,
              EQUNR       EQUNR,
              WEMPF       WEMPF,
              ABLAD       ABLAD,
              GSBER       GSBER,
              KOKRS       KOKRS,
              PARGB       PARGB,
              PARBU       PARBU,
              KOSTL       KOSTL,
              AUFNR       AUFNR,
              ANLN1       ANLN1,
              ANLN2       ANLN2,
              BUKRS       BUKRS,
              BELNR       CHAR10,
              BUZEI       BUZEI,
              BELUM       BELNR_D,
              BUZUM       BUZEI,
              RSNUM       RSNUM,
              RSPOS       RSPOS,
              KZEAR       KZEAR,
              GRUND       GRUND,
              PRCTR       PRCTR,
              PS_PSP_PNR  PS_PSP_PNR,
              NPLNR       NPLNR,
              SAKTO       SAKTO,
              VFDAT       VFDAT,
              GEBER       BP_GEBER,
              FISTL       FISTL,
              LBKUM       LBKUM,
              SALK3       SALK3,
              VPRSV       VPRSV,
              FKBER       FKBER,
              MTART       MTART,
*** 30.01.19++
              UMMAT       UMMAT,
              UMWRK       UMWRK,
              UMLGO       UMLGO,
              UMCHA       UMCHA,
              UMZST       UMZST,
              UMZUS       UMZUS,
              UMBAR       UMBAR,
              UMSOK       UMSOK,
              USNAM       USNAM,
              TCODE2      TCODE,
*** 30.01.19++
              DURATION    /SKN/E_SW_DURATION,
              DATUM       SY-DATUM,
              BUDAT_MKPF  BUDAT,
              CPUDT_MKPF  CPUDT
              .
  SELECT_MULTY: BWART,
                XAUTO,
                MJAHR,
                MATNR,
                MBLNR,
                WERKS,
                LGORT,
                INSMK,
                ZUSCH,
                ZUSTD,
                SOBKZ,
                USNAM_MKPF,
                TCODE2_MKPF,
                VBELN_IM,
                LIFNR,
                KUNNR,
                KDAUF,
                SHKZG,
                WAERS,
                DMBTR,
                DMBTR_FR,
                BALANCE,
                BALANCE_FR,
                BNBTR,
                BUALT,
                BWTAR,
                EBELN,
                ELIKZ,
                EQUNR,
                WEMPF,
                ABLAD,
                GSBER,
                KOKRS,
                PARGB,
                PARBU,
                KOSTL,
                AUFNR,
                ANLN1,
                ANLN2,
                BUKRS,
                BELNR,
                BUZEI,
                BELUM,
                BUZUM,
                RSNUM,
                RSPOS,
                KZEAR,
                GRUND,
                PRCTR,
                PS_PSP_PNR,
                NPLNR,
                SAKTO,
                VFDAT,
                GEBER,
                FISTL,
                LBKUM,
                SALK3,
                VPRSV,
                FKBER,
                MTART,
*** 30.01.19++
                UMMAT,
                UMWRK,
                UMLGO,
                UMCHA,
                UMZST,
                UMZUS,
                UMBAR,
                UMSOK,
*** 30.01.19++
                DURATION,
                DATUM,
                BUDAT_MKPF,
                CPUDT_MKPF
              .
  FIELD-SYMBOLS:    TYPE ANY.
  DATA : SY_TABIX    LIKE SY-TABIX ,
         FLD(60)     TYPE C ,
         REF_DATE    TYPE D
         .
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM,
         TIME_DIFF TYPE  INT4.
  DATA: LT_MSEG      TYPE TABLE OF /SKN/S_SW_10_02_MAT_MOVMNT,
        LT_MSEG_SON  TYPE STANDARD TABLE OF /SKN/S_SW_10_02_MAT_MOVMNT,
        LS_MSEG      TYPE  /SKN/S_SW_10_02_MAT_MOVMNT,
        LS_DATA      TYPE  /SKN/S_SW_10_02_MAT_MOVMNT.
*** Begin 15.01.19++
  DATA: LV_TABIX      TYPE SYTABIX,
        LV_LOC_FACTOR TYPE FFACT_CURR,
        LV_COUNT      TYPE I.
  DATA: LS_RATE  TYPE TY_RATE,
        LS_T001L TYPE T001L.
  DATA: LT_DATA       TYPE TABLE OF /SKN/S_SW_10_02_MAT_MOVMNT,
        LT_MSEG_WAERS TYPE TABLE OF /SKN/S_SW_10_02_MAT_MOVMNT,
        LT_RATE       TYPE TT_RATE,
        LT_T001L      TYPE TABLE OF T001L.
  FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF LT_MSEG .
*** End 15.01.19++
  ""Currency conversion & foreign amount condition
  DATA: BEGIN OF LS_CURR_RATE,
          FCURR TYPE FCURR_CURR, "local currency
          UKURS TYPE UKURS_CURR, "exchange rate
        END OF LS_CURR_RATE.
  DATA: LV_BALANCE    TYPE DMBTR,
        LV_BALANCE_FR TYPE DMBTR.
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_MAT_MOVMNT3'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'BUDAT_MKPF'.
      R_BUDAT_MKPF[] = R_DATUM[]. "Expected debit date
    WHEN 'CPUDT_MKPF'.
      R_CPUDT_MKPF[] = R_DATUM[]. "Day On Which Accounting Document Was Entered
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  " ---- Not Aggregated amount select
*** 31.01.19++
  IF LV_DATE_REF_FLD IS NOT INITIAL.
    SELECT COUNT( * )
      FROM DD03L
      INTO LV_COUNT
      WHERE TABNAME   EQ C_MSEG
      AND   FIELDNAME EQ LV_DATE_REF_FLD.
  ENDIF.
*** 31.01.19++
  IF LV_COUNT IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE LT_MSEG
      FROM MSEG AS A
      INNER JOIN MARA AS B ON  A~MATNR EQ B~MATNR
      WHERE A~BWART       IN R_BWART
      AND   A~XAUTO       IN R_XAUTO
      AND   A~MJAHR       IN R_MJAHR
      AND   A~MATNR       IN R_MATNR
      AND   A~MBLNR       IN R_MBLNR
      AND   A~WERKS       IN R_WERKS
      AND   A~LGORT       IN R_LGORT
      AND   A~INSMK       IN R_INSMK
      AND   A~ZUSCH       IN R_ZUSCH
      AND   A~ZUSTD       IN R_ZUSTD
      AND   A~SOBKZ       IN R_SOBKZ
      AND   A~USNAM_MKPF  IN R_USNAM_MKPF
      AND   A~TCODE2_MKPF IN R_TCODE2_MKPF
      AND   A~VBELN_IM    IN R_VBELN_IM
      AND   A~LIFNR       IN R_LIFNR
      AND   A~KUNNR       IN R_KUNNR
      AND   A~KDAUF       IN R_KDAUF
      AND   A~SHKZG       IN R_SHKZG
*** Begin ++ 26.09.20
      AND   A~WAERS       IN R_WAERS
      AND   A~DMBTR       IN R_DMBTR
*** End   ++ 26.09.20
      AND   A~BNBTR       IN R_BNBTR
      AND   A~BWTAR       IN R_BWTAR
      AND   A~EBELN       IN R_EBELN
      AND   A~ELIKZ       IN R_ELIKZ
      AND   A~EQUNR       IN R_EQUNR
      AND   A~WEMPF       IN R_WEMPF
      AND   A~ABLAD       IN R_ABLAD
      AND   A~GSBER       IN R_GSBER
      AND   A~KOKRS       IN R_KOKRS
      AND   A~PARGB       IN R_PARGB
      AND   A~PARBU       IN R_PARBU
      AND   A~KOSTL       IN R_KOSTL
      AND   A~AUFNR       IN R_AUFNR
      AND   A~ANLN1       IN R_ANLN1
      AND   A~ANLN2       IN R_ANLN2
      AND   A~BUKRS       IN R_BUKRS
      AND   A~BELNR       IN R_BELNR
      AND   A~BUZEI       IN R_BUZEI
      AND   A~BELUM       IN R_BELUM
      AND   A~BUZUM       IN R_BUZUM
      AND   A~RSNUM       IN R_RSNUM
      AND   A~RSPOS       IN R_RSPOS
      AND   A~KZEAR       IN R_KZEAR
      AND   A~GRUND       IN R_GRUND
      AND   A~PRCTR       IN R_PRCTR
      AND   A~PS_PSP_PNR  IN R_PS_PSP_PNR
      AND   A~NPLNR       IN R_NPLNR
      AND   A~SAKTO       IN R_SAKTO
      AND   A~VFDAT       IN R_VFDAT
      AND   A~GEBER       IN R_GEBER
      AND   A~FISTL       IN R_FISTL
      AND   A~LBKUM       IN R_LBKUM
      AND   A~SALK3       IN R_SALK3
      AND   A~VPRSV       IN R_VPRSV
      AND   A~FKBER       IN R_FKBER
*** Begin ++ 29.01.19
      AND   A~UMMAT       IN R_UMMAT
      AND   A~UMWRK       IN R_UMWRK
      AND   A~UMLGO       IN R_UMLGO
      AND   A~UMCHA       IN R_UMCHA
      AND   A~UMZST       IN R_UMZST
      AND   A~UMZUS       IN R_UMZUS
      AND   A~UMBAR       IN R_UMBAR
      AND   A~UMSOK       IN R_UMSOK
*** End   ++ 29.01.19
      AND   A~BUDAT_MKPF IN R_BUDAT_MKPF
      AND   A~CPUDT_MKPF IN R_CPUDT_MKPF
      AND   B~MTART      IN R_MTART.
* Filter by budat/cpudt in MKPF
  ELSE.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE LT_MSEG
      FROM MSEG AS A INNER JOIN MKPF AS B ON  A~MBLNR EQ B~MBLNR
                                          AND A~MJAHR EQ B~MJAHR
                     INNER JOIN MARA AS C ON  A~MATNR EQ C~MATNR
      WHERE A~BWART       IN R_BWART
      AND   A~XAUTO       IN R_XAUTO
      AND   A~MJAHR       IN R_MJAHR
      AND   A~MATNR       IN R_MATNR
      AND   A~MBLNR       IN R_MBLNR
      AND   A~WERKS       IN R_WERKS
      AND   A~LGORT       IN R_LGORT
      AND   A~INSMK       IN R_INSMK
      AND   A~ZUSCH       IN R_ZUSCH
      AND   A~ZUSTD       IN R_ZUSTD
      AND   A~SOBKZ       IN R_SOBKZ
      AND   A~USNAM_MKPF  IN R_USNAM_MKPF
      AND   A~TCODE2_MKPF IN R_TCODE2_MKPF
      AND   A~VBELN_IM    IN R_VBELN_IM
      AND   A~LIFNR       IN R_LIFNR
      AND   A~KUNNR       IN R_KUNNR
      AND   A~KDAUF       IN R_KDAUF
      AND   A~SHKZG       IN R_SHKZG
*** Begin ++ 26.09.20
      AND   A~WAERS       IN R_WAERS
      AND   A~DMBTR       IN R_DMBTR
*** End   ++ 26.09.20
      AND   A~BNBTR       IN R_BNBTR
      AND   A~BWTAR       IN R_BWTAR
      AND   A~EBELN       IN R_EBELN
      AND   A~ELIKZ       IN R_ELIKZ
      AND   A~EQUNR       IN R_EQUNR
      AND   A~WEMPF       IN R_WEMPF
      AND   A~ABLAD       IN R_ABLAD
      AND   A~GSBER       IN R_GSBER
      AND   A~KOKRS       IN R_KOKRS
      AND   A~PARGB       IN R_PARGB
      AND   A~PARBU       IN R_PARBU
      AND   A~KOSTL       IN R_KOSTL
      AND   A~AUFNR       IN R_AUFNR
      AND   A~ANLN1       IN R_ANLN1
      AND   A~ANLN2       IN R_ANLN2
      AND   A~BUKRS       IN R_BUKRS
      AND   A~BELNR       IN R_BELNR
      AND   A~BUZEI       IN R_BUZEI
      AND   A~BELUM       IN R_BELUM
      AND   A~BUZUM       IN R_BUZUM
      AND   A~RSNUM       IN R_RSNUM
      AND   A~RSPOS       IN R_RSPOS
      AND   A~KZEAR       IN R_KZEAR
      AND   A~GRUND       IN R_GRUND
      AND   A~PRCTR       IN R_PRCTR
      AND   A~PS_PSP_PNR  IN R_PS_PSP_PNR
      AND   A~NPLNR       IN R_NPLNR
      AND   A~SAKTO       IN R_SAKTO
      AND   A~VFDAT       IN R_VFDAT
      AND   A~GEBER       IN R_GEBER
      AND   A~FISTL       IN R_FISTL
      AND   A~LBKUM       IN R_LBKUM
      AND   A~SALK3       IN R_SALK3
      AND   A~VPRSV       IN R_VPRSV
      AND   A~FKBER       IN R_FKBER
      AND   A~UMMAT       IN R_UMMAT
      AND   A~UMWRK       IN R_UMWRK
      AND   A~UMLGO       IN R_UMLGO
      AND   A~UMCHA       IN R_UMCHA
      AND   A~UMZST       IN R_UMZST
      AND   A~UMZUS       IN R_UMZUS
      AND   A~UMBAR       IN R_UMBAR
      AND   A~UMSOK       IN R_UMSOK
      AND   B~BUDAT       IN R_BUDAT_MKPF
      AND   B~CPUDT       IN R_CPUDT_MKPF
      AND   C~MTART       IN R_MTART.
  ENDIF.
**** End 30.01.19++
** Begin 13.01.19++
  IF LT_MSEG IS NOT INITIAL.
    SELECT *
      FROM T001L
      INTO TABLE LT_T001L
      FOR ALL ENTRIES IN LT_MSEG
      WHERE ( WERKS EQ LT_MSEG-WERKS OR WERKS EQ LS_MSEG-UMWRK )
      AND   ( LGORT EQ LT_MSEG-LGORT OR LGORT EQ LS_MSEG-UMLGO ).
  ENDIF.
  LT_MSEG_WAERS = LT_MSEG.
  SORT LT_MSEG_WAERS BY WAERS (LV_DATE_REF_FLD).
  DELETE ADJACENT DUPLICATES FROM LT_MSEG_WAERS COMPARING WAERS.
  LOOP AT LT_MSEG_WAERS INTO LS_MSEG.
    CLEAR: LS_RATE.
    LS_RATE-WAERS = LS_MSEG-WAERS.
    IF LS_MSEG-WAERS NE LV_WAERS_FR AND
         LS_MSEG-WAERS IS NOT INITIAL AND LV_WAERS_FR IS NOT INITIAL.          " 17.09.20++
      CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
        EXPORTING
          DATE             = SY-DATUM
          FOREIGN_CURRENCY = LV_WAERS_FR      " Target(Foreign) Currency
          LOCAL_AMOUNT     = LS_MSEG-DMBTR
*         local_amount     = 1
          LOCAL_CURRENCY   = LS_MSEG-WAERS    " Source Currency
        IMPORTING
          EXCHANGE_RATE    = LS_RATE-RATE
          FOREIGN_AMOUNT   = LS_MSEG-DMBTR_FR
          LOCAL_FACTOR     = LV_LOC_FACTOR
        EXCEPTIONS
          NO_RATE_FOUND    = 1
          OVERFLOW         = 2
          NO_FACTORS_FOUND = 3
          NO_SPREAD_FOUND  = 4
          DERIVED_2_TIMES  = 5
          OTHERS           = 6.
      IF SY-SUBRC IS INITIAL.
*        ls_rate-rate = abs( ls_rate-rate / 1000 ).         " 17.09.20--
*** 28.10.20++
        IF LV_LOC_FACTOR NE 0.
          LS_RATE-RATE = LS_RATE-RATE * LV_LOC_FACTOR.
        ELSE.
          LS_RATE-RATE = LS_RATE-RATE.
        ENDIF.
*** 28.10.20++
      ENDIF.
    ELSE.
      LS_RATE-RATE = 1.
    ENDIF.
    APPEND LS_RATE TO LT_RATE.
  ENDLOOP.
  SORT LT_MSEG BY PARENT_ID.
  LT_MSEG_SON = LT_MSEG.
  DELETE LT_MSEG WHERE PARENT_ID IS NOT INITIAL.
  DELETE LT_MSEG_SON WHERE PARENT_ID IS INITIAL.
  SORT LT_MSEG BY MBLNR MJAHR LINE_ID.
  SORT LT_MSEG_SON BY MBLNR MJAHR PARENT_ID.
  LOOP AT LT_MSEG_SON INTO LS_DATA.
    CLEAR: LS_MSEG, LV_BALANCE, LV_BALANCE_FR, LS_RATE, LS_T001L.
    LS_DATA-WAERS_FR = LV_WAERS_FR.
    IF LS_DATA-WAERS <> LV_WAERS_FR AND LS_DATA-DMBTR IS NOT INITIAL AND
         LV_WAERS_FR IS NOT INITIAL.
      READ TABLE LT_RATE INTO LS_RATE WITH KEY WAERS = LS_DATA-WAERS.
      IF SY-SUBRC IS INITIAL AND LS_RATE-RATE NE 0.
*        ls_data-dmbtr_fr = ls_data-dmbtr / ls_rate-rate.
        IF LS_RATE-RATE GT 0.
          LS_DATA-DMBTR_FR = LS_DATA-DMBTR / ABS( LS_RATE-RATE ).
        ELSE.
          LS_DATA-DMBTR_FR = LS_DATA-DMBTR * ABS( LS_RATE-RATE ).
        ENDIF.
      ELSE.
        LS_DATA-DMBTR_FR = 0.
      ENDIF.
    ELSE.
      LS_DATA-DMBTR_FR = LS_DATA-DMBTR.
    ENDIF.
    CHECK LS_DATA-DMBTR_FR IN R_DMBTR_FR.    " ++ 26.09.20
* Check the line id relative to document
    READ TABLE LT_MSEG INTO LS_MSEG WITH KEY MBLNR   = LS_DATA-MBLNR
                                             MJAHR   = LS_DATA-MJAHR
                                             LINE_ID = LS_DATA-PARENT_ID
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_TABIX = SY-TABIX.
      DELETE LT_MSEG INDEX LV_TABIX.        " Delete activated row in order to leave with the single rows
      " what mean the existing single rows not have the son row
      LS_MSEG-WAERS_FR = LV_WAERS_FR.
      IF LS_MSEG-WAERS NE LV_WAERS_FR AND LS_MSEG-DMBTR IS NOT INITIAL AND
           LV_WAERS_FR IS NOT INITIAL.                                             " 17.09.20++
        READ TABLE LT_RATE INTO LS_RATE WITH KEY WAERS = LS_MSEG-WAERS.
        IF SY-SUBRC = 0 AND LS_RATE-RATE <> 0.
*          ls_mseg-dmbtr_fr = ls_mseg-dmbtr / ls_rate-rate.
          IF LS_RATE-RATE GT 0.
            LS_MSEG-DMBTR_FR = LS_MSEG-DMBTR / ABS( LS_RATE-RATE ).
          ELSE.
            LS_MSEG-DMBTR_FR = LS_MSEG-DMBTR * ABS( LS_RATE-RATE ).
          ENDIF.
        ELSE.
          LS_MSEG-DMBTR_FR = 0.
        ENDIF.
      ELSE.
        LS_MSEG-DMBTR_FR = LS_MSEG-DMBTR.
      ENDIF.
      CHECK LS_MSEG-DMBTR_FR IN R_DMBTR_FR.               " ++ 26.09.20
    ELSE.
      CONTINUE.
    ENDIF.
* Calculate abs. balance between debit and credit amount of document
    LV_BALANCE    = ABS( ABS( LS_MSEG-DMBTR )    - ABS( LS_DATA-DMBTR ) ).
    LV_BALANCE_FR = ABS( ABS( LS_MSEG-DMBTR_FR ) - ABS( LS_DATA-DMBTR_FR ) ).
* Check if amount it's within the range of filter parameter
*    CHECK lv_balance_fr IN r_dmbtr.         " -- 26.09.20
    CHECK LV_BALANCE    IN R_BALANCE.      " ++ 26.09.20
    CHECK LV_BALANCE_FR IN R_BALANCE_FR.   " ++ 26.09.20
*** Add Entry and Posting Date to Items Yuri C.++ 15.05.19
    LS_DATA-CPUDT_MKPF = LS_MSEG-CPUDT_MKPF.
    LS_DATA-BUDAT      = LS_MSEG-BUDAT.
*** Add Entry and Posting Date to Items Yuri C.++ 15.05.19
    "--- Get  Vendor Decriptions type LIFNR
    IF LS_DATA-LIFNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = LS_DATA-LIFNR
        IMPORTING
          VENDOR_DESC  = LS_DATA-LIFNR_DESC
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
    "--- Get  Customer Decriptions type KUNNR
    IF LS_DATA-KUNNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
        EXPORTING
          KUNNR          = LS_DATA-KUNNR
        IMPORTING
          CUST_DESC      = LS_DATA-KUNNR_DESC
        EXCEPTIONS
          WRONG_CUSTOMER = 1
          OTHERS         = 2.
    ENDIF.
    "--- Get Company Code Decriptions
    IF LS_DATA-BUKRS IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
        EXPORTING
          BUKRS          = LS_DATA-BUKRS  " Company Code
        IMPORTING
          COMP_CODE_DESC = LS_DATA-BUTXT  " Name of Company Code or Company
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
    ENDIF.
    "--- Get Plant Decriptions
    IF LS_DATA-WERKS IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
        EXPORTING
          WERKS      = LS_DATA-WERKS         "   Plant
          LANGU      = LV_LANGU              " Language Key
        IMPORTING
          PLANT_DESC = LS_DATA-WERKS_DESC   " Description of Plant
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    "--- Get  Material Movement Type Decriptions
    IF LS_DATA-BWART IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_MAT_MOV_DESC'
        EXPORTING
          BWART         = LS_DATA-BWART       " Mat. Movement Type (Inventory Management)
          LANGU         = LV_LANGU            " Language Key
          SOBKZ         = LS_DATA-SOBKZ       " Special Stock Indicator
          KZBEW         = LS_DATA-KZBEW       " Movement Indicator
          KZZUG         = LS_DATA-KZZUG       " Receipt Indicator
          KZVBR         = LS_DATA-KZVBR       " Consumption Posting
        IMPORTING
          BWART_DESC    = LS_DATA-BTEXT  " Movement Type Text (Inventory Management)
        EXCEPTIONS
          WRONG_MAT_MOV = 1
          OTHERS        = 2.
    ENDIF.
*    "--- Get  Material Decriptions Matnr
    IF LS_DATA-MATNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
        EXPORTING
          MATNR         = LS_DATA-MATNR
          LANGU         = LV_LANGU
        IMPORTING
          MATERIAL_DESC = LS_DATA-MAKTX
        EXCEPTIONS
          WRONG_CODE    = 1
          OTHERS        = 2.
    ENDIF.
****
* KOSTL Description
    IF LS_DATA-KOSTL IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_KOSTL_DESC'
        EXPORTING
          SPRAS      = LV_LANGU
          KOKRS      = LS_DATA-KOKRS
          KOSTL      = LS_DATA-KOSTL
        IMPORTING
          KOSTL_DESC = LS_DATA-KTEXT
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF LS_DATA-UMWRK IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
        EXPORTING
          WERKS      = LS_DATA-UMWRK
          LANGU      = LV_LANGU
        IMPORTING
          PLANT_DESC = LS_DATA-UMWRK_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    CLEAR: LS_T001L.
    IF LS_DATA-WERKS IS NOT INITIAL AND LS_DATA-LGORT IS NOT INITIAL.
      READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = LS_DATA-WERKS
                                                 LGORT = LS_DATA-LGORT
                                                 BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DATA-LGORT_DESC = LS_T001L-LGOBE.
      ENDIF.
    ENDIF.
    CLEAR: LS_T001L.
    IF LS_DATA-UMLGO IS NOT INITIAL AND LS_DATA-UMWRK IS NOT INITIAL.
      READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = LS_DATA-UMWRK
                                                 LGORT = LS_DATA-UMLGO
                                                 BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DATA-UMLGO_DESC = LS_T001L-LGOBE.
      ENDIF.
    ENDIF.
****
    APPEND LS_DATA TO T_DATA.
    IF LS_MSEG IS NOT INITIAL.
      "--- Get  Vendor Decriptions type LIFNR
      IF LS_MSEG-LIFNR IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
          EXPORTING
            LIFNR        = LS_MSEG-LIFNR
          IMPORTING
            VENDOR_DESC  = LS_MSEG-LIFNR_DESC
          EXCEPTIONS
            WRONG_VENDOR = 1
            OTHERS       = 2.
      ENDIF.
      "--- Get  Customer Decriptions type KUNNR
      IF LS_MSEG-KUNNR IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
          EXPORTING
            KUNNR          = LS_MSEG-KUNNR
          IMPORTING
            CUST_DESC      = LS_MSEG-KUNNR_DESC
          EXCEPTIONS
            WRONG_CUSTOMER = 1
            OTHERS         = 2.
      ENDIF.
      "--- Get Company Code Decriptions
      IF LS_MSEG-BUKRS IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
          EXPORTING
            BUKRS          = LS_MSEG-BUKRS  " Company Code
          IMPORTING
            COMP_CODE_DESC = LS_MSEG-BUTXT  " Name of Company Code or Company
          EXCEPTIONS
            WRONG_CODE     = 1
            OTHERS         = 2.
      ENDIF.
      "--- Get Plant Decriptions
      IF LS_MSEG-WERKS IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
          EXPORTING
            WERKS      = LS_MSEG-WERKS         "   Plant
            LANGU      = LV_LANGU              " Language Key
          IMPORTING
            PLANT_DESC = LS_MSEG-WERKS_DESC   " Description of Plant
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
      ENDIF.
      "--- Get  Material Movement Type Decriptions
      IF LS_MSEG-BWART IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_MAT_MOV_DESC'
          EXPORTING
            BWART         = LS_MSEG-BWART       " Mat. Movement Type (Inventory Management)
            LANGU         = LV_LANGU            " Language Key
            SOBKZ         = LS_MSEG-SOBKZ       " Special Stock Indicator
            KZBEW         = LS_MSEG-KZBEW       " Movement Indicator
            KZZUG         = LS_MSEG-KZZUG       " Receipt Indicator
            KZVBR         = LS_MSEG-KZVBR       " Consumption Posting
          IMPORTING
            BWART_DESC    = LS_MSEG-BTEXT       " Movement Type Text (Inventory Management)
          EXCEPTIONS
            WRONG_MAT_MOV = 1
            OTHERS        = 2.
      ENDIF.
*    "--- Get  Material Decriptions Matnr
      IF LS_MSEG-MATNR IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
          EXPORTING
            MATNR         = LS_MSEG-MATNR
            LANGU         = LV_LANGU
          IMPORTING
            MATERIAL_DESC = LS_MSEG-MAKTX
          EXCEPTIONS
            WRONG_CODE    = 1
            OTHERS        = 2.
      ENDIF.
****
* KOSTL Description
      IF LS_MSEG-KOSTL IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_KOSTL_DESC'
          EXPORTING
            SPRAS      = LV_LANGU
            KOKRS      = LS_MSEG-KOKRS
            KOSTL      = LS_MSEG-KOSTL
          IMPORTING
            KOSTL_DESC = LS_MSEG-KTEXT
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
      ENDIF.
      IF LS_MSEG-UMWRK IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
          EXPORTING
            WERKS      = LS_MSEG-UMWRK
            LANGU      = LV_LANGU
          IMPORTING
            PLANT_DESC = LS_MSEG-UMWRK_DESC
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
      ENDIF.
      CLEAR: LS_T001L.
      IF LS_MSEG-WERKS IS NOT INITIAL AND LS_MSEG-LGORT IS NOT INITIAL.
        READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = LS_MSEG-WERKS
                                                   LGORT = LS_MSEG-LGORT
                                                   BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_MSEG-LGORT_DESC = LS_T001L-LGOBE.
        ENDIF.
      ENDIF.
      CLEAR: LS_T001L.
      IF LS_MSEG-UMLGO IS NOT INITIAL AND LS_MSEG-UMWRK IS NOT INITIAL.
        READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = LS_MSEG-UMWRK
                                                   LGORT = LS_MSEG-UMLGO
                                                   BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_MSEG-UMLGO_DESC = LS_T001L-LGOBE.
        ENDIF.
      ENDIF.
****
      APPEND LS_MSEG TO T_DATA.
    ENDIF.
  ENDLOOP.
* Check single rows of the document
  LOOP AT LT_MSEG INTO LS_DATA.
    CLEAR: LV_BALANCE, LV_BALANCE_FR, LS_RATE.
    LS_DATA-WAERS_FR = LV_WAERS_FR.
    IF LS_DATA-WAERS <> LV_WAERS_FR AND LS_DATA-DMBTR IS NOT INITIAL AND
         LV_WAERS_FR IS NOT INITIAL.                                           " 17.09.20++
      READ TABLE LT_RATE INTO LS_RATE WITH KEY WAERS = LS_DATA-WAERS.
      IF SY-SUBRC  = 0 AND LS_RATE-RATE <> 0.
*        ls_data-dmbtr_fr = ls_data-dmbtr / ls_rate-rate.
        IF LS_RATE-RATE GT 0.
          LS_DATA-DMBTR_FR = LS_DATA-DMBTR / ABS( LS_RATE-RATE ).
        ELSE.
          LS_DATA-DMBTR_FR = LS_DATA-DMBTR * ABS( LS_RATE-RATE ).
        ENDIF.
      ELSE.
        LS_DATA-DMBTR_FR = 0.
      ENDIF.
    ELSE.
      LS_DATA-DMBTR_FR = LS_DATA-DMBTR.
    ENDIF.
*** 20.06.21++
    CHECK LS_DATA-DMBTR    IN R_DMBTR[].
    CHECK LS_DATA-DMBTR_FR IN R_DMBTR_FR[].
*** 20.06.21++
* Calculate abs. doc. amount
    LV_BALANCE    = ABS( LS_DATA-DMBTR ).
    LV_BALANCE_FR = ABS( LS_DATA-DMBTR_FR ).
* Check if amount it's within the range of filter parameter
*    CHECK lv_balance_fr IN r_dmbtr.        " 20.06.21--
*** 20.06.21++
    CHECK LV_BALANCE IN R_BALANCE[].
    CHECK LV_BALANCE IN R_BALANCE_FR[].
*** 20.06.21++
    LS_DATA-BALANCE    = LV_BALANCE.
    LS_DATA-BALANCE_FR = LV_BALANCE_FR.
    "--- Get  Vendor Decriptions type LIFNR
    IF LS_DATA-LIFNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = LS_DATA-LIFNR
        IMPORTING
          VENDOR_DESC  = LS_DATA-LIFNR_DESC
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
    "--- Get  Customer Decriptions type KUNNR
    IF LS_DATA-KUNNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
        EXPORTING
          KUNNR          = LS_DATA-KUNNR
        IMPORTING
          CUST_DESC      = LS_DATA-KUNNR_DESC
        EXCEPTIONS
          WRONG_CUSTOMER = 1
          OTHERS         = 2.
    ENDIF.
    "--- Get Company Code Decriptions
    IF LS_DATA-BUKRS IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
        EXPORTING
          BUKRS          = LS_DATA-BUKRS  " Company Code
        IMPORTING
          COMP_CODE_DESC = LS_DATA-BUTXT  " Name of Company Code or Company
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
    ENDIF.
    "--- Get Plant Decriptions
    IF LS_DATA-WERKS IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
        EXPORTING
          WERKS      = LS_DATA-WERKS         "   Plant
          LANGU      = LV_LANGU              " Language Key
        IMPORTING
          PLANT_DESC = LS_DATA-WERKS_DESC   " Description of Plant
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    "--- Get  Material Movement Type Decriptions
    IF LS_DATA-BWART IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_MAT_MOV_DESC'
        EXPORTING
          BWART         = LS_DATA-BWART       " Mat. Movement Type (Inventory Management)
          LANGU         = LV_LANGU            " Language Key
          SOBKZ         = LS_DATA-SOBKZ       " Special Stock Indicator
          KZBEW         = LS_DATA-KZBEW       " Movement Indicator
          KZZUG         = LS_DATA-KZZUG       " Receipt Indicator
          KZVBR         = LS_DATA-KZVBR       " Consumption Posting
        IMPORTING
          BWART_DESC    = LS_DATA-BTEXT       " Movement Type Text (Inventory Management)
        EXCEPTIONS
          WRONG_MAT_MOV = 1
          OTHERS        = 2.
    ENDIF.
*    "--- Get  Material Decriptions Matnr
    IF LS_DATA-MATNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
        EXPORTING
          MATNR         = LS_DATA-MATNR
          LANGU         = LV_LANGU
        IMPORTING
          MATERIAL_DESC = LS_DATA-MAKTX
        EXCEPTIONS
          WRONG_CODE    = 1
          OTHERS        = 2.
    ENDIF.
****
* KOSTL Description
    IF LS_DATA-KOSTL IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_KOSTL_DESC'
        EXPORTING
          SPRAS      = LV_LANGU
          KOKRS      = LS_DATA-KOKRS
          KOSTL      = LS_DATA-KOSTL
        IMPORTING
          KOSTL_DESC = LS_DATA-KTEXT
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF LS_DATA-UMWRK IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
        EXPORTING
          WERKS      = LS_DATA-UMWRK
          LANGU      = LV_LANGU
        IMPORTING
          PLANT_DESC = LS_DATA-UMWRK_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    CLEAR: LS_T001L.
    IF LS_DATA-WERKS IS NOT INITIAL AND LS_DATA-LGORT IS NOT INITIAL.
      READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = LS_DATA-WERKS
                                                 LGORT = LS_DATA-LGORT
                                                 BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DATA-LGORT_DESC = LS_T001L-LGOBE.
      ENDIF.
    ENDIF.
    CLEAR: LS_T001L.
    IF LS_DATA-UMLGO IS NOT INITIAL AND LS_DATA-UMWRK IS NOT INITIAL.
      READ TABLE LT_T001L INTO LS_T001L WITH KEY WERKS = LS_DATA-UMWRK
                                                 LGORT = LS_DATA-UMLGO
                                                 BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DATA-UMLGO_DESC = LS_T001L-LGOBE.
      ENDIF.
    ENDIF.
****
    APPEND LS_DATA TO T_DATA.
  ENDLOOP.
*** End Yuri C.++ 13.01.19
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```