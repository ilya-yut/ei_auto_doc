# Exception Indicator: MM: Material Status - SW_10_02_MAT_STAT

## General Overview

This Exception Indicator (EI) monitors material master data in Materials Management (MM) to identify materials that do not have an external material group (EXTWG) assigned, or to filter materials by external material group and other master attributes across a configurable time window. It provides visibility into material status, creation and change dates, and key master fields for data quality and process control.

This EI serves as an essential control for material master data quality and compliance by:
- Enabling detection of materials missing external material group assignment that may require completion for classification, reporting, or procurement
- Supporting identification of materials by creation or change date and by user for prioritization and data governance
- Providing visibility into material type, industry sector, material group, and maintenance status for process and control review
- Enabling analysis of material master completeness by configurable date reference (created on vs last change) for cleanup and audit
- Supporting accountability by creator and last changed-by user for audit and compliance

This monitoring enables organizations to detect incomplete master data, aging or unmaintained materials, and concentration patterns that may indicate process gaps. The EI is particularly valuable for material master data quality initiatives, period-end reconciliation, and exception management in procurement and classification.

The EI uses material master data (MARA) and optional material description; output is filtered by the configured parameters including date range (via BACKDAYS and DATE_REF_FLD), external material group, and other selection criteria.


## Problem Description

Failure to monitor materials without external material group or with incomplete master data creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Materials without external material group may be misclassified or excluded from reporting and procurement analytics
- Incomplete or inconsistent material classification can distort inventory valuation, cost allocation, and period-end closing
- Unreconciled master data gaps can lead to audit findings and misstated reporting
- Delayed identification of missing external material group may complicate data quality initiatives and require bulk updates
- Concentration of incomplete materials in certain material types or industry sectors can mask systemic process issues

**Operational and Control Risks**
- Materials missing external material group may not be correctly routed in procurement, pricing, or quality processes
- Lack of visibility by creation or change date limits ability to prioritize cleanup and data governance
- Unmonitored material status (maintenance status, completion status) can delay detection of obsolete or blocked materials
- Missing filtering by material type, industry sector, or material group can lead to noise or missed exceptions
- Insufficient visibility by creator or last changed-by user restricts accountability and actionable reporting

**Management Visibility and Decision-Making Risks**
- Absence of exception monitoring delays awareness of materials with incomplete or missing external material group
- Unidentified concentration of incomplete materials by type or sector can lead to missed process improvements or compliance gaps
- Lack of visibility by date or user limits ability to escalate aging or unmaintained materials for review
- Insufficient filtering by organizational and master data dimensions restricts actionable reporting for auditors and operations

## Suggested Resolution

**Immediate Response**
- Review the materials flagged by the EI to understand scope (missing external material group, material type or sector concentration)
- Verify material master records using material master transactions (e.g. MM03, MM60) to confirm current state and completeness
- Check maintenance status and completion status to determine if materials are blocked or require update
- Identify business context for exceptions: material type, industry sector, material group, or user responsible

**System Assessment**
- Analyze the time window and date reference used for monitoring (created on vs last change) to ensure alignment with data quality and cleanup cycles
- Examine filters (material type, industry sector, material group, external material group) to confirm the result set supports the intended analysis
- Assess material type, industry sector, and creator/last changed-by distribution to identify patterns or process issues
- Validate that date reference (ERSDA vs LAEDA) is interpreted correctly for prioritization
- Review external material group usage in classification and procurement to align assignment rules

**Corrective Actions**
- Assign external material group to materials where policy requires it, using material master transactions (e.g. MM02)
- For obsolete or blocked materials, update maintenance status or complete master data as per process
- Update material type, industry sector, or material group if misclassification or data quality issues are found
- Adjust monitoring parameters (lookback, date reference, material type, sector, group) to align with policy and re-run the EI
- Document exceptions and resolutions for audit trail; establish recurring EI runs for continuous visibility into material master completeness


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AENAM | Changed by | CHAR | 12 | 0 | AENAM | USNAM |
| 2 | AESZN | Document change no. | CHAR | 6 | 0 | AESZN | AENUM |
| 3 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 4 | BEHVO | Container reqmts | CHAR | 2 | 0 | BEHVO | BEHVO |
| 5 | BISMT | Old material number | CHAR | 18 | 0 | BISMT | BISMT |
| 6 | BLANZ | Number of sheets | NUMC | 3 | 0 | BLANZ | NUM3 |
| 7 | BLATT | Page number | CHAR | 3 | 0 | BLATT | BLATT |
| 8 | BRGEW | Gross Weight | QUAN | 13 | 3 | BRGEW | MENG13 |
| 9 | BSTME | Order Unit | UNIT | 3 | 0 | BSTME | MEINS |
| 10 | BWSCL | Source of supply | CHAR | 1 | 0 | BWSCL | BQSCL |
| 11 | BWVOR | Procurement rule | CHAR | 1 | 0 | BWVOR | BWVOR |
| 12 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 13 | DISST | Low-Level Code | CHAR | 3 | 0 | DISST | DISST |
| 14 | DUMMY | Single-Character Flag | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 15 | EAN11 | EAN/UPC | CHAR | 18 | 0 | EAN11 | EAN11 |
| 16 | EANNR | EAN number | CHAR | 13 | 0 | EANNR | CHAR13 |
| 17 | EKWSL | Purchasing value key | CHAR | 4 | 0 | EKWSL | EKWSL |
| 18 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 19 | ERSDA | Created On | DATS | 8 | 0 | ERSDA | DATUM |
| 20 | EXTWG | Ext. Material Group | CHAR | 18 | 0 | EXTWG | EXTWG |
| 21 | FERTH | Prod./insp. memo | CHAR | 18 | 0 | FERTH | FERTH |
| 22 | FORMT | Page format | CHAR | 4 | 0 | FORMT | DINFO |
| 23 | GEWEI | Unit of Weight | UNIT | 3 | 0 | GEWEI | MEINS |
| 24 | GROES | Size/dimensions | CHAR | 32 | 0 | GROES | CHAR32 |
| 25 | KUNNR | Competitor | CHAR | 10 | 0 | WETTB | KUNNR |
| 26 | LAEDA | Last Change | DATS | 8 | 0 | LAEDA | DATUM |
| 27 | LANGU | Language |  | 0 | 0 |  |  |
| 28 | LVORM | DF at client level | CHAR | 1 | 0 | LVOMA | XFELD |
| 29 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 30 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 31 | MAT_DESC | Material Description | CHAR | 40 | 0 | MAKTX | TEXT40 |
| 32 | MBRSH | Industry sector | CHAR | 1 | 0 | MBRSH | MBRSH |
| 33 | MEINS | Base Unit of Measure | UNIT | 3 | 0 | MEINS | MEINS |
| 34 | MTART | Material Type | CHAR | 4 | 0 | MTART | MTART |
| 35 | NORMT | Industry Std Desc. | CHAR | 18 | 0 | NORMT | NORMT |
| 36 | NTGEW | Net Weight | QUAN | 13 | 3 | NTGEW | MENG13 |
| 37 | PSTAT | Maintenance status | CHAR | 15 | 0 | PSTAT_D | PSTAT |
| 38 | RAUBE | Storage conditions | CHAR | 2 | 0 | RAUBE | RAUBE |
| 39 | SAISO | Season | CHAR | 4 | 0 | SAISO | SAISO |
| 40 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 41 | STOFF | Haz. material number | CHAR | 18 | 0 | STOFF | STOFF |
| 42 | TEMPB | Temp. conditions | CHAR | 2 | 0 | TEMPB | TEMPB |
| 43 | TRAGR | Transportation Group | CHAR | 4 | 0 | TRAGR | TRAGR |
| 44 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 45 | VOLEH | Volume Unit | UNIT | 3 | 0 | VOLEH | MEINS |
| 46 | VOLUM | Volume | QUAN | 13 | 3 | VOLUM | MENG13 |
| 47 | VPSTA | Compl. maint. status | CHAR | 15 | 0 | VPSTA | PSTAT |
| 48 | WRKST | Basic material | CHAR | 48 | 0 | WRKST | WRKST |
| 49 | ZEIAR | Document type | CHAR | 3 | 0 | DZEIAR | ZEIAR |
| 50 | ZEIFO | Page format | CHAR | 4 | 0 | DZEIFO | DINFO |
| 51 | ZEINR | Document | CHAR | 22 | 0 | DZEINR | CHAR22 |
| 52 | ZEIVR | Doc. Version | CHAR | 2 | 0 | DZEIVR | VERSI |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 52 parameters listed in the Parameters Reference Table above.

**AENAM** (Changed by):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**AESZN** (Document change no.):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BACKDAYS** (Days Backward from today):

Number of days to look back from today when building the default monitoring window. When no date range is supplied, the EI uses today minus this value as the start of the window and applies it to the date field selected by DATE_REF_FLD.

**BEHVO** (Container reqmts):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BISMT** (Old material number):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BLANZ** (Number of sheets):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BLATT** (Page number):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BRGEW** (Gross Weight):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BSTME** (Order Unit):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**BWSCL** (Source of supply):

Restricts which material records are included or populates the result. CHAR(1) / flag.

**BWSCL Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**BWVOR** (Procurement rule):

Restricts which material records are included or populates the result. CHAR(1) / flag.

**BWVOR Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**DATE_REF_FLD** (Date Ref Field):

Name of the date field used as the reference for the default date range. Determines which date (created on or last change) is used when no explicit range is supplied.

**DATE_REF_FLD Options:**
- **ERSDA**: Created on (default in code).
- **LAEDA**: Last change.

**DISST** (Low-Level Code):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**DUMMY** (Single-Character Flag):

Restricts which material records are included or populates the result. CHAR(1) / flag.

**DUMMY Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**EAN11** (EAN/UPC):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**EANNR** (EAN number):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**EKWSL** (Purchasing value key):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ERNAM** (Created by):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ERSDA** (Created On):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**EXTWG** (Ext. Material Group):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**FERTH** (Prod./insp. memo):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**FORMT** (Page format):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**GEWEI** (Unit of Weight):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**GROES** (Size/dimensions):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**KUNNR** (Competitor):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LAEDA** (Last Change):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LANGU** (Language):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**LVORM** (DF at client level):

Restricts which material records are included or populates the result. CHAR(1) / flag.

**LVORM Options:**
- **X**: Set/active.
- ** ** (space): Not set/inactive.

**MATKL** (Material Group):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MATNR** (Material):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MAT_DESC** (Material Description):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MBRSH** (Industry sector):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MEINS** (Base Unit of Measure):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**MTART** (Material Type):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**NORMT** (Industry Std Desc.):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**NTGEW** (Net Weight):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**PSTAT** (Maintenance status):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**RAUBE** (Storage conditions):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SAISO** (Season):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**SPART** (Division):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**STOFF** (Haz. material number):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**TEMPB** (Temp. conditions):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**TRAGR** (Transportation Group):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**USER_FLD** (Dynamic Recipient User Field):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VOLEH** (Volume Unit):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VOLUM** (Volume):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**VPSTA** (Compl. maint. status):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**WRKST** (Basic material):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ZEIAR** (Document type):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ZEIFO** (Page format):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ZEINR** (Document):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.

**ZEIVR** (Doc. Version):

Restricts which material records are included or appears in the result. Used in selection criteria or output structure as defined in the function.


### Parameter Relationships

**Time and date reference parameters:**
- BACKDAYS defines the lookback window (days from today). When no date range is supplied, the EI builds the range from today minus BACKDAYS.
- DATE_REF_FLD selects which date field is used for that range: ERSDA (created on) or LAEDA (last change). The chosen field is applied to filter material master records. Use ERSDA to focus on recently created materials; use LAEDA to focus on recently changed materials.

**External material group and classification:**
- EXTWG (external material group) restricts which materials are included. To find materials **without** an external material group, supply a range that includes initial/empty (e.g. leave EXTWG unspecified or use a range that includes blank). To focus on materials with a specific external material group, supply that value or range. EXTWG works together with MATKL (material group), MTART (material type), and MBRSH (industry sector) to scope the result set for classification and data quality analysis.

**Creator and last changed-by:**
- ERNAM (created by) and AENAM (changed by) restrict which materials are included by user. They work together with ERSDA and LAEDA when analyzing material master maintenance and accountability.

**Material status:**
- VPSTA (completion status) and PSTAT (maintenance status) restrict which materials are included by status. Use them together to focus on materials in a given maintenance or completion state (e.g. incomplete, blocked, or released).


### Default Values

- **BACKDAYS** — Default: `1` (lookback window when no date range is supplied).
- **DATE_REF_FLD** — Default: `ERSDA` (created on).
- **LANGU** — Default: `E` (English).

**Note:** When no date range is supplied, the EI uses today minus BACKDAYS as the start of the monitoring window and applies it to the date field selected by DATE_REF_FLD (ERSDA or LAEDA).

### Practical Configuration Examples

**Use Case 1: Materials without external material group (recently created)**
```
BACKDAYS = 30
DATE_REF_FLD = ERSDA
EXTWG = (leave initial or range including blank)
```
**Purpose:** Identify materials created in the last 30 days that do not have an external material group assigned, for data quality and classification follow-up.

**Use Case 2: Materials by last change date**
```
BACKDAYS = 14
DATE_REF_FLD = LAEDA
MTART = FERT
```
**Purpose:** Focus on finished goods (material type FERT) that were changed in the last 14 days, for maintenance status and master data review.

**Use Case 3: Materials by creator and material group**
```
BACKDAYS = 7
DATE_REF_FLD = ERSDA
ERNAM = (user range)
MATKL = (material group range)
```
**Purpose:** Review materials created in the last 7 days by specific creator(s) and material group(s) for accountability and data governance.

**Use Case 4: Materials by status and industry sector**
```
BACKDAYS = 90
DATE_REF_FLD = LAEDA
VPSTA = (status range)
MBRSH = M
```
**Purpose:** Identify materials in a given completion/maintenance status and industry sector (e.g. M = mechanical) that were changed in the last 90 days for cleanup and compliance.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_02_MAT_STAT | .INCLU--AP |  |  |  |
| /SKN/S_SW_10_02_MAT_STAT | AENAM | Name of Person Who Changed Object | CHAR(12) | AENAM |
| /SKN/S_SW_10_02_MAT_STAT | AESZN | Document change number (without document management system) | CHAR(6) | AESZN |
| /SKN/S_SW_10_02_MAT_STAT | BEHVO | Container requirements | CHAR(2) | BEHVO |
| /SKN/S_SW_10_02_MAT_STAT | BISMT | Old material number | CHAR(18) | BISMT |
| /SKN/S_SW_10_02_MAT_STAT | BLANZ | Number of sheets (without Document Management system) | NUMC(3) | BLANZ |
| /SKN/S_SW_10_02_MAT_STAT | BLATT | Page number of document (without Document Management system) | CHAR(3) | BLATT |
| /SKN/S_SW_10_02_MAT_STAT | BRGEW | Gross Weight | QUAN(13,3) | BRGEW |
| /SKN/S_SW_10_02_MAT_STAT | BSTME | Purchase Order Unit of Measure | UNIT(3) | BSTME |
| /SKN/S_SW_10_02_MAT_STAT | BWSCL | Source of Supply | CHAR(1) | BWSCL |
| /SKN/S_SW_10_02_MAT_STAT | BWVOR | Procurement rule | CHAR(1) | BWVOR |
| /SKN/S_SW_10_02_MAT_STAT | DISST | Low-Level Code | CHAR(3) | DISST |
| /SKN/S_SW_10_02_MAT_STAT | DUMMY | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_SW_10_02_MAT_STAT | EAN11 | International Article Number (EAN/UPC) | CHAR(18) | EAN11 |
| /SKN/S_SW_10_02_MAT_STAT | EANNR | European Article Number (EAN) - obsolete!!!!! | CHAR(13) | EANNR |
| /SKN/S_SW_10_02_MAT_STAT | EKWSL | Purchasing Value Key | CHAR(4) | EKWSL |
| /SKN/S_SW_10_02_MAT_STAT | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_02_MAT_STAT | ERSDA | Created On | DATS(8) | ERSDA |
| /SKN/S_SW_10_02_MAT_STAT | EXTWG | External Material Group | CHAR(18) | EXTWG |
| /SKN/S_SW_10_02_MAT_STAT | FERTH | Production/inspection memo | CHAR(18) | FERTH |
| /SKN/S_SW_10_02_MAT_STAT | FORMT | Page Format of Production Memo | CHAR(4) | FORMT |
| /SKN/S_SW_10_02_MAT_STAT | GEWEI | Weight Unit | UNIT(3) | GEWEI |
| /SKN/S_SW_10_02_MAT_STAT | GROES | Size/dimensions | CHAR(32) | GROES |
| /SKN/S_SW_10_02_MAT_STAT | KUNNR | Competitor | CHAR(10) | WETTB |
| /SKN/S_SW_10_02_MAT_STAT | LAEDA | Date of Last Change | DATS(8) | LAEDA |
| /SKN/S_SW_10_02_MAT_STAT | LVORM | Flag Material for Deletion at Client Level | CHAR(1) | LVOMA |
| /SKN/S_SW_10_02_MAT_STAT | MATKL | Material Group | CHAR(9) | MATKL |
| /SKN/S_SW_10_02_MAT_STAT | MATNR | Material Number | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_MAT_STAT | MAT_DESC | Material Description (Short Text) | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_MAT_STAT | MBRSH | Industry sector | CHAR(1) | MBRSH |
| /SKN/S_SW_10_02_MAT_STAT | MEINS | Base Unit of Measure | UNIT(3) | MEINS |
| /SKN/S_SW_10_02_MAT_STAT | MTART | Material Type | CHAR(4) | MTART |
| /SKN/S_SW_10_02_MAT_STAT | NORMT | Industry Standard Description (such as ANSI or ISO) | CHAR(18) | NORMT |
| /SKN/S_SW_10_02_MAT_STAT | NTGEW | Net Weight | QUAN(13,3) | NTGEW |
| /SKN/S_SW_10_02_MAT_STAT | PSTAT | Maintenance status | CHAR(15) | PSTAT_D |
| /SKN/S_SW_10_02_MAT_STAT | RAUBE | Storage conditions | CHAR(2) | RAUBE |
| /SKN/S_SW_10_02_MAT_STAT | SAISO | Season Category | CHAR(4) | SAISO |
| /SKN/S_SW_10_02_MAT_STAT | SPART | Division | CHAR(2) | SPART |
| /SKN/S_SW_10_02_MAT_STAT | STOFF | Hazardous material number | CHAR(18) | STOFF |
| /SKN/S_SW_10_02_MAT_STAT | TEMPB | Temperature conditions indicator | CHAR(2) | TEMPB |
| /SKN/S_SW_10_02_MAT_STAT | TRAGR | Transportation Group | CHAR(4) | TRAGR |
| /SKN/S_SW_10_02_MAT_STAT | VOLEH | Volume unit | UNIT(3) | VOLEH |
| /SKN/S_SW_10_02_MAT_STAT | VOLUM | Volume | QUAN(13,3) | VOLUM |
| /SKN/S_SW_10_02_MAT_STAT | VPSTA | Maintenance status of complete material | CHAR(15) | VPSTA |
| /SKN/S_SW_10_02_MAT_STAT | WRKST | Basic Material | CHAR(48) | WRKST |
| /SKN/S_SW_10_02_MAT_STAT | ZEIAR | Document type (without Document Management system) | CHAR(3) | DZEIAR |
| /SKN/S_SW_10_02_MAT_STAT | ZEIFO | Page format of document (without Document Management system) | CHAR(4) | DZEIFO |
| /SKN/S_SW_10_02_MAT_STAT | ZEINR | Document number (without document management system) | CHAR(22) | DZEINR |
| /SKN/S_SW_10_02_MAT_STAT | ZEIVR | Document version (without Document Management system) | CHAR(2) | DZEIVR |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_MAT_STAT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_MAT_STAT OPTIONAL
*"----------------------------------------------------------------------
DATA : SY_TABIX LIKE SY-TABIX .
DATA : DATE_FROM LIKE SY-DATUM .
DATA_SINGLE: MANAGE_IN_UTC  CHAR1 ,
             LANGU  LANGU,
             BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD.
 LV_BACKDAYS = 1.
 LV_LANGU = 'E'.
 LV_DATE_REF_FLD = 'ERSDA'. "Created on
 SELECT_SINGLE: MANAGE_IN_UTC,
                LANGU,
                BACKDAYS,
                DATE_REF_FLD.
DATA_MULTY: MATNR        MATNR,
            ERSDA        ERSDA,
            LAEDA        LAEDA,
            DATUM        SY-DATUM,
            ERNAM        ERNAM,
            AENAM        AENAM,
            VPSTA        VPSTA,
            PSTAT        PSTAT_D,
            MTART        MTART,
            MBRSH        MBRSH,
            MATKL        MATKL,
            MEINS        MEINS,
            BSTME        BSTME,
            EXTWG        EXTWG,
            EAN11        EAN11
           .
SELECT_MULTY:
            MATNR,
            ERSDA,
            LAEDA,
            DATUM,
            ERNAM,
            AENAM,
            VPSTA,
            PSTAT,
            MTART,
            MBRSH,
            MATKL,
            MEINS,
            BSTME,
            EXTWG,
            EAN11
          .
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_MAT_STAT'
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
   CASE LV_DATE_REF_FLD.
     WHEN 'ERSDA'.
       R_ERSDA[] = R_DATUM[]. " created on
     WHEN 'LAEDA'.
       R_LAEDA[] = R_DATUM[]. "changed on
     WHEN OTHERS.
       R_ERSDA[] = R_DATUM[]. "created on
   ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM MARA
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE  MATNR IN R_MATNR
           AND ERSDA IN R_ERSDA
           AND LAEDA IN R_LAEDA
"           and DATUM in R_DATUM
           AND ERNAM IN R_ERNAM
           AND AENAM IN R_AENAM
           AND VPSTA IN R_VPSTA
           AND PSTAT IN R_PSTAT
           AND MTART IN R_MTART
           AND MBRSH IN R_MBRSH
           AND MATKL IN R_MBRSH
           AND MEINS IN R_MEINS
           AND BSTME IN R_BSTME
           AND EXTWG IN R_EXTWG
           AND EAN11 IN R_EAN11.
LOOP AT T_DATA.
**Material desc
SY_TABIX = SY-TABIX .
  CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
    EXPORTING
      MATNR               = T_DATA-MATNR
      LANGU               = LV_LANGU
   IMPORTING
     MATERIAL_DESC       = T_DATA-MAT_DESC
   EXCEPTIONS
     WRONG_CODE          = 1
   OTHERS              = 2
          .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```