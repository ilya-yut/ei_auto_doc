# Exception Indicator: Retroactively created PO by vendor invoice date - SW_10_03_PO_RETRO_CR

## General Overview

This Exception Indicator (EI) monitors purchase orders that were created after the related vendor invoice document date, highlighting retroactive creation of POs in procurement. It compares the PO creation date with the invoice document date (or another configurable reference date) and flags cases where the order was created later than the invoice, supporting audit and control over backdated or retroactively created purchasing documents.

This EI serves as an essential control for procurement and financial oversight by:
- Enabling detection of purchase orders created after the vendor invoice date, which may indicate control weaknesses or backdating
- Supporting identification of retroactive PO creation patterns by company code, vendor, or document type for compliance review
- Providing visibility into the time gap between invoice entry and PO creation for prioritization and root-cause analysis
- Enabling analysis by purchasing organization, purchasing group, and release strategy for accountability and delegation review
- Supporting audit readiness by surfacing exceptions that require business justification or corrective action

The EI is valuable for period-end close checks, procurement controls monitoring, and audit preparation. It helps management ensure that purchase orders are created in a timely manner relative to vendor invoicing and that retroactive creation is documented and authorized.


## Problem Description

Failure to monitor purchase orders created after the vendor invoice date creates multiple risks across financial reporting, operational control, and compliance:

**Financial and Reporting Issues**
- Undetected retroactive PO creation can distort period-end accruals and expense recognition when orders are backdated into prior periods
- Invoice-to-PO timing exceptions may indicate revenue or expense timing manipulation or inadequate cut-off controls
- Unreported backdated orders can lead to misstated liabilities and reconciliation gaps during month-end close
- Late discovery of retroactive creation patterns may delay financial close and require restatements or adjustments
- Concentrated exceptions in specific company codes or vendors can signal systemic control failures requiring management intervention

**Procurement and Control Risks**
- Retroactive PO creation without visibility may indicate unauthorized commitments or circumvention of approval workflows
- Lack of monitoring by vendor or document type can mask repeated backdating by specific suppliers or document types
- Exceptions by release strategy or purchasing group may reveal delegation or segregation-of-duties issues
- Unchecked retroactive creation can undermine purchasing policies and audit trails for contract and order dates
- High volumes of exceptions could indicate process or system integration failures requiring immediate correction

**Management Visibility and Decision-Making Risks**
- Absence of monitoring delays executive awareness of control weaknesses in procurement and payables
- Unidentified retroactive creation patterns can lead to missed opportunities for process improvement or training
- Exceptions may require additional audit scrutiny or compliance review but go unnoticed without the EI
- Lack of visibility by organizational dimension limits ability to assign accountability and remediate root causes

## Suggested Resolution

**Immediate Response**
- Review the flagged purchase orders and invoices to confirm that the PO creation date is indeed after the invoice document date and understand the business context
- Verify high-value or high-volume exceptions using transaction ME23N (Display PO) and MIRO (Enter Invoice) to confirm dates and legitimacy
- Check document status and processing state to ensure no pending corrections or reversals explain the timing
- Identify whether exceptions stem from legitimate late PO creation (e.g. service entry), data entry errors, or inappropriate backdating

**System Assessment**
- Analyze the reference date used (e.g. invoice entry date, document date, PO date) and the lookback window to ensure the monitoring scope is appropriate
- Compare current exception counts and patterns to prior periods to identify trends or one-time spikes
- Examine distribution by company code, vendor, and document type to pinpoint concentration or process issues
- Assess release strategy and purchasing group distribution to determine if exceptions correlate with specific workflows or delegations
- Validate that document type and invoice status filters (e.g. posted, uncanceled) align with the intended control objective

**Corrective Actions**
- Where retroactive creation is unauthorized or erroneous, follow internal procedures for correction, escalation, and documentation
- For legitimate late PO creation, document business justification and consider process or policy updates to reduce future exceptions
- Update master data or approval workflows if exceptions point to delegation or vendor setup issues
- Adjust monitoring parameters (e.g. lookback days, reference date field, duration) to focus on material exceptions and reduce noise
- Establish recurring EI execution and alert routing to procurement and finance stakeholders for continuous control monitoring


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 3 | BEDAT | Document Date | DATS | 8 | 0 | EBDAT | DATUM |
| 4 | BELNR | Invoice Document No. | CHAR | 10 | 0 | RE_BELNR | BELNR |
| 5 | BLART | Document Type | CHAR | 2 | 0 | BLART | BLART |
| 6 | BLART_DESC | Description | CHAR | 20 | 0 | LTEXT_003T | TEXT20 |
| 7 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 8 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 9 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 10 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 11 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 12 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 13 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 14 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 15 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 16 | DATE_REF_FLD | Date Referebce Field |  | 0 | 0 |  |  |
| 17 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 18 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 19 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 20 | EBELP | Item | NUMC | 5 | 0 | EBELP | EBELP |
| 21 | EKGRP | Purchasing Group | CHAR | 3 | 0 | BKGRP | EKGRP |
| 22 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 23 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 24 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 25 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 26 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 27 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 28 | FRGKE | Release indicator | CHAR | 1 | 0 | FRGKE | FRGKE |
| 29 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 30 | FRGSX | Release Strategy | CHAR | 2 | 0 | FRGSX | FRGSX |
| 31 | FRGZU | Release State | CHAR | 8 | 0 | FRGZU | FRGZU |
| 32 | KDATB | Validity Per. Start | DATS | 8 | 0 | KDATB | DATUM |
| 33 | KDATE | Validity Period End | DATS | 8 | 0 | KDATE | DATUM |
| 34 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 35 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 36 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 37 | PROCSTAT_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 38 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 39 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 40 | RMWWR | Gross invoice amount | CURR | 13 | 2 | RMWWR | WERT7 |
| 41 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 42 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 43 | USER_FLD | User field for Dyn Rec List |  | 0 | 0 |  |  |
| 44 | USNAM | User Name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 45 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 46 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 47 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 47 parameters listed in the Parameters Reference Table above.

**AEDAT** (Created on):

Date when the purchase order was created. The EI compares this date with the invoice document date to detect retroactive creation: when the PO creation date is after the invoice date, the record is flagged. The reference date field (DATE_REF_FLD) can point to CPUDT, AEDAT, or BEDAT for the monitoring window; when set to AEDAT, the EI restricts document-created date to the lookback window.

**BACKDAYS** (Back Days):

Number of days used to build the monitoring window. When no date range is supplied, the EI uses today minus this value as the start date and applies it to the date field chosen by DATE_REF_FLD (e.g. invoice entry date or document date) when reading invoice and PO data.

**BEDAT** (Document Date):

Document date on the purchase order. Can be used as the reference date for the monitoring window when DATE_REF_FLD is set to BEDAT; the EI then restricts this field to the lookback window. The EI also uses PO and invoice dates to determine retroactive creation (PO created after invoice).

**BELNR** (Invoice Document No.):

Invoice document number. The EI reads invoice header data keyed by this field; values scope which invoices are evaluated for retroactive PO creation.

**BLART** (Document Type):

Invoice document type (e.g. RE, KR). The EI resolves allowed document types via customizing (e.g. T169F for MIRO) and restricts which invoice types are included so monitoring focuses on relevant document types.

**BLART Options:**
- Values come from customizing (e.g. T169F for transaction MIRO); typical values include **RE**: invoice, **KR**: credit memo. Configure according to your document type setup.

**BLART_DESC** (Description):

Short description of the invoice document type from master data or customizing.

**BLDAT** (Document Date):

Document date on the invoice. The EI compares the PO creation date (AEDAT) with this date; when AEDAT is after BLDAT, the record is flagged as retroactively created. This field is used in the core business logic of the EI.

**BSAKZ** (Control indicator):

Control indicator for the purchasing document type. The EI includes it in selection and result for differentiation by document-type behavior.

**BSAKZ Options:**
- **X**: set; ** ** (space): not set. Values are domain-specific; use according to document type configuration.

**BSART** (Purchasing Doc. Type):

Purchase order document type. The EI reads PO header data and includes this in selection and result so configurations can focus on specific order types (e.g. standard, framework).

**BSART_DESC** (Doc. Type Descript.):

Short description of the purchasing document type from master data.

**BSTYP** (Purch. Doc. Category):

Purchasing document category (e.g. order vs contract). The EI includes it in selection and result for scope by category.

**BSTYP Options:**
- **B**: Purchase order; **A**: Contract; **C**: Scheduling agreement. Other domain values as in standard SAP; use values relevant to the EI scope.

**BSTYP_DESC** (Short Descript.):

Short text for the purchasing document category from domain or master data.

**BUDAT** (Posting Date):

Posting date of the invoice; used for cut-off or period analysis in the EI’s data.

**BUKRS** (Company Code):

Company code of the invoice. The EI restricts invoice and PO selection by company code so monitoring can be scoped by organization.

**CPUDT** (Entry Date):

Date on which the accounting document (invoice) was entered. The code uses CPUDT as the default reference date for the monitoring window when DATE_REF_FLD is not supplied; the EI restricts this field to the lookback window when building the selection.

**DATE_REF_FLD** (Date Referebce Field):

Selects which date field on the invoice/PO is used for the monitoring window and for duration calculation: entry date (CPUDT), document-created date (AEDAT), or PO document date (BEDAT). The EI applies the lookback window to the chosen field when reading data.

**DATE_REF_FLD Options:**
- **CPUDT**: Entry date (day the accounting document was entered); default in the code.
- **AEDAT**: Document created date.
- **BEDAT**: Purchase order document date.

**BACKDAYS and DATE_REF_FLD Connection:** BACKDAYS defines the lookback length; DATE_REF_FLD defines which date field is restricted to that window. Set both when configuring the monitoring window (e.g. last 10 days by entry date).

**DURATION** (Duration In Time Units):

Elapsed time between the reference date (CPUDT, AEDAT, or BEDAT, as configured) and the evaluation date, in the unit given by DURATION_UNIT. The EI calculates this per invoice/PO record and uses it for duration-based filtering.

**DURATION_UNIT** (Duration Unit):

Unit in which DURATION is expressed and evaluated (hours, minutes, days, or full days for specific-day logic). The EI uses this when computing and comparing duration for each record.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**DURATION and DURATION_UNIT Connection:** DURATION holds the numeric value; DURATION_UNIT defines its meaning. Set both when using duration-based filtering (e.g. “invoices with duration equal to 7 full days”).

**EBELN** (Purchasing Document):

Purchase order number linked to the invoice. The EI joins invoice (RBKP/RSEG) to PO header (EKKO) and uses this to scope which orders are evaluated and to display the order in the result.

**EBELP** (Item):

Purchase order item number. Links the invoice item to the order item in the result.

**EKGRP** (Purchasing Group):

Purchasing group responsible for the order. The EI selects and reports by purchasing group for responsibility-based monitoring.

**EKGRP_DESC** (Description p. group):

Description of the purchasing group from master data.

**EKORG** (Purch. Organization):

Purchasing organization of the order. The EI restricts and reports by purchasing organization for organizational scope.

**EKORG_DESC** (Description):

Description of the purchasing organization from master data.

**ERNAM** (Created by):

User who created the purchase order. The EI uses this in the selection (EKKO~ERNAM) so monitoring can focus on specific creators when analyzing retroactive creation.

**FRGC** (Release code):

Release code that represents the approval or release state of the order. The EI resolves it from FRGGR, FRGSX, and FRGZU and then filters by this code so only orders in certain release states are included.

**FRGGR** (Release group):

Release group that defines the release strategy for the order. The EI uses it to resolve the release code and to scope which orders are evaluated.

**FRGKE** (Release indicator):

Release indicator on the purchasing document. The EI includes it in selection and result for differentiation by release type.

**FRGKE Options:**
- Values are domain-specific (e.g. release-relevant flags). Use according to your release strategy configuration.

**FRGRL** (Subject to release):

Indicates whether the order is subject to release. The EI includes it in selection and result for scope (e.g. only orders subject to release).

**FRGRL Options:**
- **X**: Subject to release; ** ** (space): not subject to release.

**FRGSX** (Release Strategy):

Release strategy code for the order. The EI uses it together with FRGGR and FRGZU to resolve the release code (FRGC) and to filter which orders are included.

**FRGZU** (Release State):

Release status code for the current step. The EI uses it to resolve the release code (FRGC) for filtering.

**KDATB** (Validity Per. Start):

Start of validity period for the order or related object.

**KDATE** (Validity Period End):

End of validity period. Used together with KDATB for validity-window filtering when relevant.

**LIFNR** (Vendor):

Vendor account number on the invoice. The EI restricts invoice and PO selection by vendor so monitoring can focus on specific suppliers.

**LOEKZ** (Deletion Indicator):

Deletion indicator on the purchasing document. The EI typically excludes deleted orders so only active documents are evaluated.

**LOEKZ Options:**
- ** ** (space): Not deleted (active); **L**: Deletion flag. Such records are typically excluded from the EI result.

**PROCSTAT** (Purch. doc. proc. state):

Processing state of the purchasing document. The EI includes it in selection and result to scope or display by processing status.

**PROCSTAT Options:**
- Values are from domain MEPROCSTATE (e.g. pending, released). Use customizing or domain values for the exact list.

**PROCSTAT_DESC** (Short Descript.):

Short text for the processing state from domain or master data.

**RESWK** (Supplying Plant):

Supplying or issuing plant. The EI selects and reports by plant for plant-level monitoring.

**RESWK_DESC** (Name 1):

Name of the supplying plant from master data.

**RMWWR** (Gross invoice amount):

Gross invoice amount in document currency; represents the invoice value used for value-based analysis in the EI.

**STATU** (Status):

Status of the purchasing document. The EI includes it in selection and result to scope or display by status.

**STATU Options:**
- Values are from domain ESTAK (e.g. pending, released). Use customizing or domain values for the exact list.

**STATU_DESC** (Short Descript.):

Short text for the status from domain or master data.

**USER_FLD** (User field for Dyn Rec List):

User-defined field for dynamic recipient list; available in the EI structure for configuration.

**USNAM** (User Name):

User name (e.g. last changed by); used for user-based scope or accountability in the EI.

**VENDOR_DESC** (Name):

Name of the vendor from master data.

**WAERS** (Currency):

Document currency of the invoice; amounts such as RMWWR are expressed in this currency.

**ZTERM** (Terms of Payment):

Terms of payment key on the order or invoice; used for scope by payment terms in the EI.


### Parameter Relationships

**Time-Based and Duration Parameters:**
- **BACKDAYS** defines how many days to look back from today when no date range is supplied; the EI builds the monitoring window from today minus this value.
- **DATE_REF_FLD** selects which date field on the invoice/PO is used for that window: CPUDT (entry date), AEDAT (document created), or BEDAT (PO date). The chosen field is restricted to the window when the EI reads invoice and order data.
- **DURATION** and **DURATION_UNIT** work together: DURATION holds the elapsed time (in the unit given by DURATION_UNIT) between the reference date and the evaluation date; the EI calculates this per record and uses it for duration-based filtering. Set both when filtering by how long ago the invoice or order was created.

**Invoice Document Type and Company Scope:**
- **BLART** (document type) is resolved via customizing (e.g. T169F for MIRO); the EI uses it to restrict to relevant invoice document types. **BUKRS** (company code) and **LIFNR** (vendor) scope the invoice and PO selection; use them together for organizational and vendor-level monitoring.

**Release and Status Parameters:**
- **FRGGR**, **FRGSX**, and **FRGZU** are used by the EI to resolve **FRGC** (release code); the EI filters by release code after resolution. Use these when focusing on specific release strategies or approval states.


### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the EI uses a 10-day lookback from today for the monitoring window).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).
- **DATE_REF_FLD** — Default: `CPUDT` (entry date on the invoice is used as the reference date for the monitoring window and duration calculation when not supplied).

**Note:** When no date range is supplied, the EI builds the monitoring window from today minus BACKDAYS and applies it to the date field selected by DATE_REF_FLD (CPUDT, AEDAT, or BEDAT).

### Practical Configuration Examples

**Use Case 1: Last 10 days by entry date (default lookback)**

```
BACKDAYS = 10
DATE_REF_FLD = CPUDT
```

**Purpose:** Monitor invoices and POs for the last 10 days using the invoice entry date as the reference. Suitable for routine weekly or biweekly checks for retroactive PO creation.

**Use Case 2: By company code, vendor, and document type**

```
BUKRS = 1000, 2000
LIFNR = 0000100001–0000100099
BLART = RE, KR
```

**Purpose:** Limit results to specific company codes, vendor ranges, and invoice document types (e.g. invoice, credit memo). Supports regional or vendor-specific control and audit focus.

**Use Case 3: Duration in full days and reference date**

```
DATE_REF_FLD = CPUDT
DURATION_UNIT = F
DURATION = 7
BACKDAYS = 30
```

**Purpose:** Express duration in full days and flag invoices where the elapsed time since the reference date (entry date) equals 7 full days, within a 30-day lookback. Useful for age-based prioritization (e.g. invoices exactly one week old). DURATION is a single value when using DURATION_UNIT = F.

**Use Case 4: Purchasing organization, release strategy, and creator**

```
EKORG = 1000
FRGGR = 01, 02
ERNAM = USER01–USER99
```

**Purpose:** Focus on a specific purchasing organization, release groups, and a range of PO creators. Supports delegation and segregation-of-duties review for retroactive creation.


### EI Function Structure

## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_03_PO_RETRO_CR | AEDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_RETRO_CR | BEDAT | Purchasing Document Date | DATS(8) | EBDAT |
| /SKN/S_SW_10_03_PO_RETRO_CR | BELNR | Document Number of an Invoice Document | CHAR(10) | RE_BELNR |
| /SKN/S_SW_10_03_PO_RETRO_CR | BLART | Document Type | CHAR(2) | BLART |
| /SKN/S_SW_10_03_PO_RETRO_CR | BLART_DESC | Document Type Description | CHAR(20) | LTEXT_003T |
| /SKN/S_SW_10_03_PO_RETRO_CR | BLDAT | Document Date in Document | DATS(8) | BLDAT |
| /SKN/S_SW_10_03_PO_RETRO_CR | BSAKZ | Control indicator for purchasing document type | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PO_RETRO_CR | BSART | Purchasing Document Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_RETRO_CR | BSART_DESC | Short Description of Purchasing Document Type | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_RETRO_CR | BSTYP | Purchasing Document Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PO_RETRO_CR | BSTYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_RETRO_CR | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_03_PO_RETRO_CR | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_RETRO_CR | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_03_PO_RETRO_CR | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_RETRO_CR | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_RETRO_CR | EBELN | Purchasing Document Number | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_RETRO_CR | EBELP | Item Number of Purchasing Document | NUMC(5) | EBELP |
| /SKN/S_SW_10_03_PO_RETRO_CR | EKGRP | Purchasing Group | CHAR(3) | BKGRP |
| /SKN/S_SW_10_03_PO_RETRO_CR | EKGRP_DESC | Description of purchasing group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_RETRO_CR | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_RETRO_CR | EKORG_DESC | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_RETRO_CR | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_RETRO_CR | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PO_RETRO_CR | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PO_RETRO_CR | FRGKE | Release Indicator: Purchasing Document | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_PO_RETRO_CR | FRGRL | Release Not Yet Completely Effected | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PO_RETRO_CR | FRGSX | Release Strategy | CHAR(2) | FRGSX |
| /SKN/S_SW_10_03_PO_RETRO_CR | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PO_RETRO_CR | KDATB | Start of Validity Period | DATS(8) | KDATB |
| /SKN/S_SW_10_03_PO_RETRO_CR | KDATE | End of Validity Period | DATS(8) | KDATE |
| /SKN/S_SW_10_03_PO_RETRO_CR | LIFNR | Vendor Account Number | CHAR(10) | ELIFN |
| /SKN/S_SW_10_03_PO_RETRO_CR | LOEKZ | Deletion Indicator in Purchasing Document | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_RETRO_CR | PROCSTAT | Purchasing document processing state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_PO_RETRO_CR | PROCSTAT_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_RETRO_CR | RESWK | Supplying (Issuing) Plant in Stock Transport Order | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PO_RETRO_CR | RESWK_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PO_RETRO_CR | RMWWR | Gross invoice amount in document currency | CURR(13,2) | RMWWR |
| /SKN/S_SW_10_03_PO_RETRO_CR | STATU | Status of Purchasing Document | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_RETRO_CR | STATU_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_RETRO_CR | USNAM | User name | CHAR(12) | USNAM |
| /SKN/S_SW_10_03_PO_RETRO_CR | VENDOR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_RETRO_CR | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_RETRO_CR | ZTERM | Terms of Payment Key | CHAR(4) | DZTERM |

### ABAP Code

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PO_RETRO_CR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_RETRO_CR OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: LANGU  LANGU,
             BACKDAYS INT4,
             DATE_REF_FLD NAME_FELD,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
 LV_BACKDAYS = 10.
 LV_DURATION_UNIT = 'D'.
 LV_LANGU = SY-LANGU.
 LV_DATE_REF_FLD = 'CPUDT'. "Entered on
 SELECT_SINGLE: LANGU,
                BACKDAYS,
                DATE_REF_FLD,
                DURATION_UNIT.
DATA_MULTY: FRGRL        FRGRL,
            EBELN        EBELN,
            BUKRS        BUKRS,
            BSTYP        EBSTYP,
            BSART        ESART,
            EKORG        EKORG,
            EKGRP        BKGRP,
            FRGGR        FRGGR,
            FRGSX        FRGSX,
            FRGCO        FRGCO,
            FRGKE        FRGKE,
            LIFNR        ELIFN,
            RESWK        RESWK,
            ZTERM        DZTERM,
            ERNAM        ERNAM,
            AEDAT        ERDAT,
            BEDAT        EBDAT,
            WAERS        WAERS,
            PROCSTAT     MEPROCSTATE,
            DATUM        SY-DATUM,
            CPUDT        CPUDT,
            BLART        BLART,
            BELNR        RE_BELNR,
            DURATION    /SKN/E_SW_DURATION.
SELECT_MULTY:
            FRGRL,
            EBELN,
            BUKRS,
            BSTYP,
            BSART,
            EKORG,
            EKGRP,
            FRGGR,
            FRGSX,
            FRGCO,
            FRGKE,
            LIFNR,
            RESWK,
            ZTERM,
            ERNAM,
            AEDAT,
            BEDAT,
            WAERS,
            PROCSTAT,
            DATUM,
            CPUDT,
            BLART,
            BELNR,
            DURATION.
CONVERT_MULTY: EBELN ALPHA,
               LIFNR ALPHA.
RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
         R_FLD_VAL FOR DD03P-FIELDNAME .
DATA :   FLD_NAME TYPE FIELDNAME.
DATA : I TYPE I,
       CI(1) TYPE C,
       NFIELDS TYPE I VALUE 3.   "
DATA : BACKDAYS  TYPE I ,
       DATE_FROM LIKE SY-DATUM .
DATA : LANGU LIKE SY-LANGU .
DATA : IS_OUT(1) TYPE C.
DATA : TIME_DIFF TYPE  INT4 .
DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
      LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
      LV_DDTEXT LIKE  DD07V-DDTEXT.
DATA: LV_FRGCO  TYPE FRGCO.
DATA: LS_T169F TYPE T169F,
      LT_T169F LIKE TABLE OF LS_T169F.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : FLD(60) TYPE C .
DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
FIELD-SYMBOLS:  TYPE ANY ,
               <FS_V> TYPE ANY .
DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
  INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
DATA : END OF SW_STRUCTURE .
DATA : LS_VBPA TYPE VBPA,
       LT_VBPA LIKE TABLE OF LS_VBPA.
DATA : LV_DATA_POSNR TYPE POSNR.
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PO_RETRO_CR'
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
     WHEN 'CPUDT'.
       R_CPUDT[] = R_DATUM[]. "Entered on
     WHEN 'AEDAT'.
       R_AEDAT[] = R_DATUM[]. "Document created
     WHEN 'BEDAT'.
       R_BEDAT[] = R_DATUM[]. "PO Date
     WHEN OTHERS.
       R_CPUDT[] = R_DATUM[]. "Billing date
   ENDCASE.
 "--- Prepare BLART
   SELECT *
     FROM T169F
     INTO CORRESPONDING FIELDS OF TABLE LT_T169F
     WHERE TCODE = 'MIRO'
       AND BLART IN R_BLART.
     REFRESH R_BLART.
     LOOP AT LT_T169F INTO LS_T169F.
       RS_BLART-SIGN = 'I'.
       RS_BLART-OPTION = 'EQ'.
       RS_BLART-LOW = LS_T169F-BLART.
       APPEND RS_BLART TO R_BLART.
     ENDLOOP.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM RBKP
      INNER JOIN RSEG
        ON RBKP~BELNR = RSEG~BELNR AND
           RBKP~GJAHR = RSEG~GJAHR
      INNER JOIN EKKO
        ON RSEG~EBELN = EKKO~EBELN
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE RBKP~BLART IN R_BLART
      AND RBKP~BUKRS IN R_BUKRS
      AND RBKP~CPUDT IN R_CPUDT
      AND RBKP~LIFNR IN R_LIFNR
      AND RBKP~BELNR IN R_BELNR
      AND RBKP~RBSTAT IN ( '5',' ' )
      AND RBKP~STBLG = ' '
      AND EKKO~AEDAT > RBKP~BLDAT
      AND EKKO~ERNAM IN R_ERNAM
      .
*********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR                   = T_DATA-FRGGR
        FRGSX                   = T_DATA-FRGSX
        FRGZU                   = T_DATA-FRGZU
      IMPORTING
        FRGC                    = T_DATA-FRGC
      EXCEPTIONS
        WRONG_COMBINATION       = 1
        OTHERS                  = 2.
    IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE FRGC NOT IN R_FRGCO.
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = REF_DATE
          T_FROM            = SY-UZEIT
          D_TO              = SY-DATUM
          T_TO              = SY-UZEIT
          TIME_UNIT         = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          T_DATA-DURATION  = TIME_DIFF .
        ELSE.
          T_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
******************************************************************************
********************************************************************************
"--- Set Descriptions
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    "-- BSTYP_DESC
    LV_DOMNAME = 'EBSTYP'.
    LV_DOMVALUE = T_DATA-BSTYP.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME        = LV_DOMNAME
        I_DOMVALUE       = LV_DOMVALUE
        LANGU            = LV_LANGU
      IMPORTING
        E_DDTEXT         = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST        = 1
        OTHERS           = 2.
    IF SY-SUBRC = 0.
      T_DATA-BSTYP_DESC = LV_DDTEXT.
    ENDIF.
    "-- STATU_DESC
    LV_DOMNAME = 'ESTAK'.
    LV_DOMVALUE = T_DATA-STATU.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME        = LV_DOMNAME
        I_DOMVALUE       = LV_DOMVALUE
        LANGU            = LV_LANGU
      IMPORTING
        E_DDTEXT         = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST        = 1
        OTHERS           = 2.
    IF SY-SUBRC = 0.
      T_DATA-STATU_DESC = LV_DDTEXT.
    ENDIF.
    "-- PROCSTAT_DESC
    LV_DOMNAME = 'MEPROCSTATE'.
    LV_DOMVALUE = T_DATA-PROCSTAT.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME        = LV_DOMNAME
        I_DOMVALUE       = LV_DOMVALUE
        LANGU            = LV_LANGU
      IMPORTING
        E_DDTEXT         = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST        = 1
        OTHERS           = 2.
    IF SY-SUBRC = 0.
      T_DATA-PROCSTAT_DESC = LV_DDTEXT.
    ENDIF.
    "-- BSART_DESC
    CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART            = T_DATA-BSART
        LANGU            = LV_LANGU
        BSTYP            = T_DATA-BSTYP
      IMPORTING
        TYPE_DESC        = T_DATA-BSART_DESC
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- Get  Vendor Decriptions
     CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
       EXPORTING
         LIFNR              = T_DATA-LIFNR
       IMPORTING
         VENDOR_DESC        = T_DATA-VENDOR_DESC
       EXCEPTIONS
         WRONG_VENDOR       = 1
         OTHERS             = 2.
     IF SY-SUBRC <> 0.
     ENDIF.
   "-- EKORG_DESC
    CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG              = T_DATA-EKORG
      IMPORTING
        PUR_ORG_DESC       = T_DATA-EKORG_DESC
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
   "-- EKGRP_DESC
    CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP              = T_DATA-EKGRP
      IMPORTING
        PUR_GRP_DESC       = T_DATA-EKGRP_DESC
      EXCEPTIONS
        WRONG_CODE         = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- RESWK_DESC (WERKS)
     CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
       EXPORTING
         WERKS            = T_DATA-RESWK
       IMPORTING
         PLANT_DESC       = T_DATA-RESWK_DESC
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2.
     IF SY-SUBRC <> 0.
     ENDIF.
     CALL FUNCTION '/SKN/F_SW_10_BLART_DESC'
       EXPORTING
         BLART            = T_DATA-BLART
       IMPORTING
         TYPE_DESC        = T_DATA-BLART_DESC
       EXCEPTIONS
         WRONG_CODE       = 1
         OTHERS           = 2.
     IF SY-SUBRC <> 0.
     ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
