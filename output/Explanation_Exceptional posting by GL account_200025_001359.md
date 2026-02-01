# Exception Indicator: FI: Exceptional Postings by GL account - SW_10_07_FI_EXC_POST

## General Overview

This Exception Indicator (EI) monitors FI (Financial Accounting) document postings by company code, document number, fiscal year, and G/L account to identify exceptional postings that may require review. It uses document header and line data from accounting documents (BKPF, BSIS, BSAS, BSEG) and G/L account master (SKA1) to filter by date range, amount, currency, document type, transaction code, and G/L account type, and can return either header-level records or full line-item detail with user name and G/L account description.

This EI serves as an essential control for financial reporting and operational oversight by:
- Enabling detection of exceptional or unusual postings by G/L account that may indicate errors, fraud, or policy violations
- Supporting identification of documents and line items by posting date, entry date, or last change for prioritization and audit review
- Providing visibility into posting duration (time since reference date) for aging and follow-up
- Enabling analysis by company code, document type, transaction code, and user for accountability and root-cause analysis
- Supporting filtering by working days or holidays when a factory calendar is used, for period and calendar-aware monitoring

The EI supports month-end close, internal controls, and exception management by surfacing FI documents and line items that meet configurable selection criteria. Data is drawn from FI document header and line tables (BKPF, BSIS, BSAS, BSEG) and G/L account master (SKA1).


## Problem Description

Failure to monitor exceptional FI postings by G/L account creates multiple risks across financial reporting, operational management, and compliance.

**Financial and Reporting Issues**
- Unreviewed exceptional postings by G/L account can distort period-end financial statements and lead to misstated balances
- Unusual amounts or document types in specific accounts may indicate revenue or expense recognition errors or inappropriate journal entries
- High-value or concentrated postings in particular company codes or currencies may signal exposure or consolidation issues
- Delayed detection of exceptional postings can complicate month-end close and require restatements or adjustments
- Postings that deviate from expected patterns by document type or transaction code may indicate control bypass or manual override

**Operational and Control Risks**
- Lack of visibility into who posted which documents limits accountability and increases the risk of unauthorized or erroneous entries
- Exceptional postings by G/L account without review may hide process failures, misallocations, or duplicate postings
- Inability to focus on working days or specific date ranges can dilute attention and delay identification of true exceptions
- Postings that fall outside normal amount or duration patterns may indicate system or process issues requiring corrective action
- Absence of structured exception monitoring by G/L account and company code weakens internal controls over financial data

**Management Visibility and Decision-Making Risks**
- Executives and controllers lack a single view of exceptional postings for prioritization and resource allocation
- Unidentified concentration of postings in certain accounts, periods, or users can delay audit and compliance responses
- Without duration and date-based filtering, management cannot efficiently target aged or time-sensitive exceptions
- Limited ability to drill down to line-item detail (when needed) hinders root-cause analysis and remediation
- Inadequate monitoring of exceptional postings undermines confidence in financial data quality and control effectiveness

## Suggested Resolution

**Immediate Response**
- Review the FI documents and line items flagged by the EI to confirm whether postings are legitimate or require correction
- Verify high-value or unusual postings using transaction FB03 (Display Document) to check authorization and business justification
- Check document status and posting key to ensure no pending corrections or reversals are overlooked
- Identify the business context: recurring adjustments, period-end entries, corrections, or one-time events that explain the exception

**System Assessment**
- Analyze the selection dimensions (company code, document type, transaction code, date range) to understand what drives the exceptional pattern
- Compare current exception volumes and amounts to prior periods using the same criteria to spot trends
- Review currency and amount distributions to identify exposure or booking concentration
- Assess G/L account and chart of accounts usage to detect misallocations or master data issues
- Validate the date field used for the monitoring window (posting date, entry date, last change) so that the scope matches the control objective

**Corrective Actions**
- For erroneous or unauthorized postings, initiate correction or reversal via FB08 (Parking Document Reversal) or standard FI correction procedures as appropriate
- Escalate unexplained exceptional postings to accounting and internal audit for validation and documentation
- Update G/L account master or chart of accounts (FS00) if misclassification or account misuse is confirmed
- Tighten access or process controls for transaction codes or users associated with repeated exceptions
- Document findings and remediation for audit trail and management reporting
- Schedule recurring EI runs and route results to responsible accountants and controllers for continuous monitoring of exceptional postings by G/L account


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AD_NAMEFIR | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 2 | AD_NAMELAS | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 3 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 4 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 5 | BELNR | Document Number | CHAR | 10 | 0 | BELNR_D | BELNR |
| 6 | BKTXT | Document Header Text | CHAR | 25 | 0 | BKTXT | TEXT25 |
| 7 | BLART | Document Type | CHAR | 2 | 0 | BLART | BLART |
| 8 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 9 | BSCHL | Posting Key | CHAR | 2 | 0 | BSCHL | BSCHL |
| 10 | BSTAT | Doc.status | CHAR | 1 | 0 | BSTAT_D | BSTAT |
| 11 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 12 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 13 | BUZEI | Item | NUMC | 3 | 0 | BUZEI | BUZEI |
| 14 | COMP_OPERATOR | Operator | CHAR | 2 | 0 | BUCC_OPERATOR | BUCC_OPERATOR |
| 15 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 16 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 17 | DMBE2 | Amount in LC2 | CURR | 13 | 2 | DMBE2 | WERT7 |
| 18 | DMBE3 | Amount in LC3 | CURR | 13 | 2 | DMBE3 | WERT7 |
| 19 | DMBTR | Amount in LC | CURR | 13 | 2 | DMBTR | WERT7 |
| 20 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 21 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 22 | FABKL | Factory calendar | CHAR | 2 | 0 | FABKL | WFCID |
| 23 | FACDATE | Factory date | DEC | 5 | 0 | FACDATE | HFDATE |
| 24 | FULL | 'X' - ALL items of FI document | CHAR | 1 | 0 | /SKN/E_SW_FLAG | CHECKBOX |
| 25 | GJAHR | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 26 | GRPID | Session name | CHAR | 12 | 0 | GRPID_BKPF | CHAR12 |
| 27 | GVTYP | P&L statmt acct type | CHAR | 2 | 0 | GVTYP | CHAR2 |
| 28 | HKONT | G/L Account | CHAR | 10 | 0 | HKONT | SAKNR |
| 29 | HKONT_DESC | G/L Acct Long Text | CHAR | 50 | 0 | TXT50_SKAT | TEXT50 |
| 30 | HWAE2 | Local currency 2 | CUKY | 5 | 0 | HWAE2 | WAERS |
| 31 | HWAE3 | Local currency 3 | CUKY | 5 | 0 | HWAE3 | WAERS |
| 32 | HWAER | Local Currency | CUKY | 5 | 0 | HWAER | WAERS |
| 33 | KTOKS | Account Group | CHAR | 4 | 0 | KTOKS | KTOKS |
| 34 | KTOPL | Chart of Accounts | CHAR | 4 | 0 | KTOPL | KTOPL |
| 35 | KTOPL_T001 | Chart of Accounts | CHAR | 4 | 0 | KTOPL | KTOPL |
| 36 | KURS2 | Exchange rate 2 | DEC | 9 | 5 | KURS2 | KURSF |
| 37 | KURS3 | Exchange rate 3 | DEC | 9 | 5 | KURS3 | KURSF |
| 38 | KURSF | Exchange rate | DEC | 9 | 5 | KURSF | KURSF |
| 39 | KZKRS | Group Currency Exchange Rate | DEC | 9 | 5 | KZKRS | KURSF |
| 40 | KZWRS | Group currency | CUKY | 5 | 0 | KZWRS | WAERS |
| 41 | LANGU | Language Key (G/L account) |  | 0 | 0 |  |  |
| 42 | MONAT | Posting period | NUMC | 2 | 0 | MONAT | MONAT |
| 43 | REF_FIELD_NAME1 | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 44 | REF_FIELD_NAME2 | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 45 | RESULT_COMP | Value 1 to Compare | CURR | 15 | 2 |  |  |
| 46 | SGTXT | Text | CHAR | 50 | 0 | SGTXT | TEXT50 |
| 47 | SHKZG | Debit/Credit Ind. | CHAR | 1 | 0 | SHKZG | SHKZG |
| 48 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 49 | UPDDT | Last update | DATS | 8 | 0 | UPDDT | DATUM |
| 50 | USNAM | User name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 51 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 52 | WAERS_FR | Foreign Currency | CUKY | 5 | 0 |  |  |
| 53 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 54 | WFCID | Factory calendar ID |  | 0 | 0 |  |  |
| 55 | WORKING_DAYS | Working Day - Y/N | CHAR | 1 | 0 | CIND | CHAR1 |
| 56 | WRBTR | Amount | CURR | 13 | 2 | WRBTR | WERT7 |
| 57 | XBLNR | Reference | CHAR | 16 | 0 | XBLNR1 | XBLNR1 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 57 parameters listed in the Parameters Reference Table above.

**AD_NAMEFIR** (First Name):

First name of the user who posted the document. Populated from user master when full line detail is requested; used for display.

**AD_NAMELAS** (Last Name):

Last name of the user who posted the document. Populated from user master when full line detail is requested; used for display.

**AEDAT** (Changed On):

Date the record was last changed. Restricts which documents are included by change date; when DATE_REF_FLD = AEDAT, the default date range is applied to this field. Also appears in the result.

**BACKDAYS** (Back days):

Number of days to look back from the current date when building the default monitoring window. When no explicit date range is supplied, the EI uses today minus this value as the start of the window and applies it to the date field selected by DATE_REF_FLD.

**BELNR** (Document Number):

Accounting document number. Restricts which documents are included; also appears in the result.

**BKTXT** (Document Header Text):

Document header text. Restricts which documents are included; also appears in the result.

**BLART** (Document Type):

Document type (e.g. SA, KA). Restricts which documents are included; also appears in the result.

**BLDAT** (Document Date):

Document date in the document. Restricts which documents are included; also appears in the result. When DATE_REF_FLD = BLDAT, the default range is applied to this field.

**BSCHL** (Posting Key):

Posting key. Restricts which line items are included; also appears in the result.

**BSTAT** (Doc.status):

Document status. Restricts which documents are included; also appears in the result.

**BSTAT Options:**
- **X** or other domain values: Set (e.g. posted, parked).
- ** ** (space or initial): Not set / initial.

**BUDAT** (Posting Date):

Posting date in the document. Restricts which documents are included; also appears in the result. When DATE_REF_FLD = BUDAT, the default range is applied to this field.

**BUKRS** (Company Code):

Company code. Restricts which documents are included; also appears in the result.

**BUZEI** (Item):

Line item number within the accounting document. Restricts which line items are included when filtering by item; also appears in the result.

**COMP_OPERATOR** (Operator):

Comparison operator for consistency checks. Used with comparison logic when applicable; values are domain-specific (e.g. EQ, NE, GT, LT).

**CPUDT** (Entry Date):

Day on which the accounting document was entered. Restricts which documents are included; when DATE_REF_FLD = CPUDT (default in code), the default range is applied to this field. Also appears in the result.

**DATE_REF_FLD** (Date reference field):

Name of the date field used as the reference for the default date range and for duration calculation. Determines which date (posting date, entry date, change date, document date, last update) is used when no explicit range is supplied and for computing how long the document has been in the system.

**DATE_REF_FLD Options:**
- **BUDAT**: Posting date in the document.
- **AEDAT**: Changed on.
- **CPUDT**: Entry date (default in code).
- **UPDDT**: Last update.
- **BLDAT**: Document date in the document.

**DMBE2 - DMBE3** (Amount in LC2 – Amount in LC3):

Amount in second and third local currency. Restrict which line items are included by amount; also appear in the result. Business meaning: second and third local currency amounts for reporting.

**DMBTR** (Amount in LC):

Amount in local currency. Restricts which line items are included by amount; also appears in the result. Business meaning: local currency amount of the line item.

**DURATION** (Duration In Time Units):

Duration in the unit given by DURATION_UNIT (e.g. days) between the reference date (from DATE_REF_FLD) and the run date. Restricts which documents are included when a duration filter is applied; also appears in the result.

**DURATION_UNIT** (Duration Unit):

Unit for DURATION (e.g. days, hours). Used with DURATION and DATE_REF_FLD when computing and filtering by how long the document has been in the system.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours (if supported by the duration calculation function).
- **M**: Minutes (if supported).

**FABKL** (Factory calendar):

Factory calendar key. Used with WFCID and working-days logic to determine factory date and working-day indicator; used for filtering by working days or holidays when configured.

**FACDATE** (Factory date):

Factory calendar date. Populated when a factory calendar is used; used for display and for working-days filtering.

**FULL** ('X' - ALL items of FI document):

When set, the EI returns all line items of each FI document with user name and G/L account description; when not set, returns one record per document header.

**FULL Options:**
- **X**: Return full line-item detail (all items per document).
- ** ** (space or initial): Return header-level records only.

**GJAHR** (Fiscal Year):

Fiscal year. Restricts which documents are included; also appears in the result.

**GRPID** (Session name):

Batch input session name. Restricts which documents are included; also appears in the result.

**GVTYP** (P&L statmt acct type):

P&L statement account type (G/L account type). Restricts which line items are included via G/L account master; also appears in the result.

**HKONT** (G/L Account):

G/L account number. Restricts which line items are included; also appears in the result.

**HKONT_DESC** (G/L Acct Long Text):

G/L account long text. Populated from G/L account master (SKA1/SKAT); used for display.

**HWAE2 - HWAE3** (Local currency 2 – Local currency 3):

Second and third local currency. Business meaning: company code second and third local currency for reporting.

**HWAER** (Local Currency):

Document/local currency. Business meaning: company code local currency in which amounts are stored.

**KTOKS** (Account Group):

G/L account group. Restricts which line items are included via G/L account master; also appears in the result.

**KTOPL** (Chart of Accounts):

Chart of accounts. Restricts which line items are included via G/L account master; also appears in the result.

**KTOPL_T001** (Chart of Accounts):

Chart of accounts (from company code). Used for consistency with company code; also appears in the result.

**KURS2 - KURS3** (Exchange rate 2 – Exchange rate 3):

Exchange rates for second and third local currency. Restrict or display; used for multi-currency reporting.

**KURSF** (Exchange rate):

Exchange rate. Restricts or displays; used for document currency conversion.

**KZKRS** (Group Currency Exchange Rate):

Group currency exchange rate. Used for group currency reporting.

**KZWRS** (Group currency):

Group currency. Business meaning: currency used for group-level reporting.

**LANGU** (Language Key (G/L account)):

Language key used when retrieving G/L account descriptions. When not supplied, a default (e.g. system language) may be used.

**MONAT** (Posting period):

Fiscal period (posting period). Restricts which documents are included; also appears in the result.

**REF_FIELD_NAME1 - REF_FIELD_NAME2** (Field name – Field name):

First and second reference field names for comparison or display. Used when consistency or comparison checks reference specific fields.

**RESULT_COMP** (Value 1 to Compare):

Comparison value for consistency checks. Used with operator and reference fields when filtering or validating.

**SGTXT** (Text):

Line item text. Restricts which line items are included; also appears in the result.

**SHKZG** (Debit/Credit Ind.):

Debit/credit indicator. Restricts which line items are included (e.g. debit or credit only); also appears in the result.

**SHKZG Options:**
- **S**: Debit (Soll).
- **H**: Credit (Haben).

**TCODE** (Transaction Code):

Transaction code used to create the document. Restricts which documents are included; also appears in the result.

**UPDDT** (Last update):

Date of the last document update. Restricts which documents are included; when DATE_REF_FLD = UPDDT, the default range is applied. Also appears in the result.

**USNAM** (User name):

User name who posted the document. Restricts which documents are included; also appears in the result.

**WAERS** (Currency):

Document currency key. Business meaning: transaction/document currency in which the document was entered.

**WAERS_FR** (Foreign Currency):

Foreign currency. Business meaning: foreign currency when the document involves a different currency than local currency.

**WERKS** (Plant):

Plant. Restricts or displays when relevant to the document; also appears in the result.

**WFCID** (Factory calendar ID):

Factory calendar ID. When set, the EI uses the factory calendar to determine working days and holidays; used together with WORKING_DAYS to filter by working days only or holidays only.

**WORKING_DAYS** (Working Day - Y/N):

When WFCID is set, filters results to working days only or to holidays only.

**WORKING_DAYS Options:**
- **X** (or C_YES): Include only working days (exclude holidays).
- ** ** (or C_NO): Include only holidays (exclude working days).
- ** ** (initial): No filtering by working day; all dates included.

**WRBTR** (Amount):

Amount in document currency. Restricts which line items are included by amount; also appears in the result. Business meaning: amount in document currency.

**XBLNR** (Reference):

Reference document number. Restricts which documents are included; also appears in the result.


### Parameter Relationships

**Time and duration parameters**

- **BACKDAYS** and **DATE_REF_FLD** work together: BACKDAYS defines how many days to look back from today when no explicit date range is supplied; DATE_REF_FLD selects which date field (BUDAT, AEDAT, CPUDT, UPDDT, BLDAT) is used for that range and for duration calculation. Set BACKDAYS to define the default window length and DATE_REF_FLD to align the window with posting date, entry date, change date, or document date as needed.
- **DURATION** and **DURATION_UNIT** work with **DATE_REF_FLD**: duration is computed between the reference date (from the field named by DATE_REF_FLD) and the run date, in the unit given by DURATION_UNIT (e.g. days). Use DURATION to filter by how long the document has been in the system; DURATION_UNIT defines the unit of measure.

**Factory calendar and working-days filtering**

- **WFCID** and **WORKING_DAYS** work together: when WFCID (factory calendar ID) is set, the EI uses the calendar to determine working days and holidays. WORKING_DAYS then filters results to working days only or to holidays only. Leave WFCID initial to skip working-days filtering.

**Full-detail mode**

- **FULL**: when set to 'X', the EI returns all line items per document with user name and G/L account description; when not set, one record per document header is returned. FULL does not combine with other parameters in a relationship group but controls the level of detail in the result.


### Default Values

- **BACKDAYS** — Default: `1` (when no explicit date range is supplied; the default monitoring window starts one day back from today).
- **DATE_REF_FLD** — Default: `CPUDT` (entry date of the accounting document is used as the reference for the default date range and duration calculation).
- **DURATION_UNIT** — Default: `D` (duration is calculated in days).
- **LANGU** — Default: `E` (English or system default for G/L account descriptions when not supplied).
- **FULL** — Default: initial (empty); when not set, the EI returns one record per document header rather than full line-item detail.
- **WORKING_DAYS** — Default: initial (empty); when not set, no filtering by working days or holidays is applied.
- **WFCID** — Default: initial (empty); when not set, the factory calendar is not used and working-days logic is skipped.

### Practical Configuration Examples

**Use Case 1: Exceptional postings in the last 30 days by entry date**
```
BACKDAYS = 30
DATE_REF_FLD = CPUDT
BUKRS = 1000
```
**Purpose:** Focus on accounting documents entered in the last 30 days in company code 1000, using entry date as the reference for the default window and for duration. Supports period-end review and aging of recent postings.

**Use Case 2: High-value local-currency line items, header view**
```
DMBTR = 100000 (e.g. lower bound via range)
FULL = (initial)
BLART = SA
```
**Purpose:** Identify documents with local-currency line items above a threshold, document type SA (G/L document), without expanding to line-level detail. Useful for high-value exception review.

**Use Case 3: Postings by posting date and duration in days**
```
DATE_REF_FLD = BUDAT
DURATION_UNIT = D
DURATION = 1 to 90 (e.g. range: documents posted 1–90 days ago)
```
**Purpose:** Monitor documents by posting date with duration in days, e.g. to find postings that have been in the system between 1 and 90 days. Supports aging and follow-up prioritization.

**Use Case 4: Full line-item detail with user and G/L account description**
```
FULL = X
BUKRS = 1000
GJAHR = 2024
```
**Purpose:** Retrieve all line items for documents in company code 1000 and fiscal year 2024, with user name and G/L account long text. Supports detailed audit and root-cause analysis.

**Use Case 5: Working days only in a company code**
```
WFCID = 01 (or appropriate factory calendar ID)
WORKING_DAYS = X
BUKRS = 2000
BACKDAYS = 7
DATE_REF_FLD = BUDAT
```
**Purpose:** Restrict results to postings that fall on working days (excluding holidays) in the last seven days by posting date, for company code 2000. Supports calendar-aware exception reporting.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_07_FI_EXC_POST | AD_NAMEFIR | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_07_FI_EXC_POST | AD_NAMELAS | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_07_FI_EXC_POST | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_07_FI_EXC_POST | BELNR | Accounting Document Number | CHAR(10) | BELNR_D |
| /SKN/S_SW_10_07_FI_EXC_POST | BKTXT | Document Header Text | CHAR(25) | BKTXT |
| /SKN/S_SW_10_07_FI_EXC_POST | BLART | Document Type | CHAR(2) | BLART |
| /SKN/S_SW_10_07_FI_EXC_POST | BLDAT | Document Date in Document | DATS(8) | BLDAT |
| /SKN/S_SW_10_07_FI_EXC_POST | BSCHL | Posting Key | CHAR(2) | BSCHL |
| /SKN/S_SW_10_07_FI_EXC_POST | BSTAT | Document Status | CHAR(1) | BSTAT_D |
| /SKN/S_SW_10_07_FI_EXC_POST | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_07_FI_EXC_POST | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_07_FI_EXC_POST | BUZEI | Number of Line Item Within Accounting Document | NUMC(3) | BUZEI |
| /SKN/S_SW_10_07_FI_EXC_POST | COMP_OPERATOR | Consistency Checks - Comparison operator | CHAR(2) | BUCC_OPERATOR |
| /SKN/S_SW_10_07_FI_EXC_POST | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_07_FI_EXC_POST | DMBE2 | Amount in Second Local Currency | CURR(13,2) | DMBE2 |
| /SKN/S_SW_10_07_FI_EXC_POST | DMBE3 | Amount in Third Local Currency | CURR(13,2) | DMBE3 |
| /SKN/S_SW_10_07_FI_EXC_POST | DMBTR | Amount in Local Currency | CURR(13,2) | DMBTR |
| /SKN/S_SW_10_07_FI_EXC_POST | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_07_FI_EXC_POST | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_07_FI_EXC_POST | FABKL | Factory calendar key | CHAR(2) | FABKL |
| /SKN/S_SW_10_07_FI_EXC_POST | FACDATE | Factory calendar: Factory date | DEC(5) | FACDATE |
| /SKN/S_SW_10_07_FI_EXC_POST | FULL | SW: Flag (' '/'X') | CHAR(1) | /SKN/E_SW_FLAG |
| /SKN/S_SW_10_07_FI_EXC_POST | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /SKN/S_SW_10_07_FI_EXC_POST | GRPID | Batch Input Session Name | CHAR(12) | GRPID_BKPF |
| /SKN/S_SW_10_07_FI_EXC_POST | GVTYP | P&L statement account type | CHAR(2) | GVTYP |
| /SKN/S_SW_10_07_FI_EXC_POST | HKONT | General Ledger Account | CHAR(10) | HKONT |
| /SKN/S_SW_10_07_FI_EXC_POST | HKONT_DESC | G/L Account Long Text | CHAR(50) | TXT50_SKAT |
| /SKN/S_SW_10_07_FI_EXC_POST | HWAE2 | Currency Key of Second Local Currency | CUKY(5) | HWAE2 |
| /SKN/S_SW_10_07_FI_EXC_POST | HWAE3 | Currency Key of Third Local Currency | CUKY(5) | HWAE3 |
| /SKN/S_SW_10_07_FI_EXC_POST | HWAER | Local Currency | CUKY(5) | HWAER |
| /SKN/S_SW_10_07_FI_EXC_POST | KTOKS | G/L Account Group | CHAR(4) | KTOKS |
| /SKN/S_SW_10_07_FI_EXC_POST | KTOPL | Chart of Accounts | CHAR(4) | KTOPL |
| /SKN/S_SW_10_07_FI_EXC_POST | KTOPL_T001 | Chart of Accounts | CHAR(4) | KTOPL |
| /SKN/S_SW_10_07_FI_EXC_POST | KURS2 | Exchange Rate for the Second Local Currency | DEC(9,5) | KURS2 |
| /SKN/S_SW_10_07_FI_EXC_POST | KURS3 | Exchange Rate for the Third Local Currency | DEC(9,5) | KURS3 |
| /SKN/S_SW_10_07_FI_EXC_POST | KURSF | Exchange rate | DEC(9,5) | KURSF |
| /SKN/S_SW_10_07_FI_EXC_POST | KZKRS | Group Currency Exchange Rate | DEC(9,5) | KZKRS |
| /SKN/S_SW_10_07_FI_EXC_POST | KZWRS | Currency Key for the Group Currency | CUKY(5) | KZWRS |
| /SKN/S_SW_10_07_FI_EXC_POST | MONAT | Fiscal Period | NUMC(2) | MONAT |
| /SKN/S_SW_10_07_FI_EXC_POST | REF_FIELD_NAME1 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_07_FI_EXC_POST | REF_FIELD_NAME2 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_07_FI_EXC_POST | RESULT_COMP |  | CURR(15,2) |  |
| /SKN/S_SW_10_07_FI_EXC_POST | SGTXT | Item Text | CHAR(50) | SGTXT |
| /SKN/S_SW_10_07_FI_EXC_POST | SHKZG | Debit/Credit Indicator | CHAR(1) | SHKZG |
| /SKN/S_SW_10_07_FI_EXC_POST | TCODE | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_10_07_FI_EXC_POST | UPDDT | Date of the Last Document Update | DATS(8) | UPDDT |
| /SKN/S_SW_10_07_FI_EXC_POST | USNAM | User name | CHAR(12) | USNAM |
| /SKN/S_SW_10_07_FI_EXC_POST | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_07_FI_EXC_POST | WAERS_FR |  | CUKY(5) |  |
| /SKN/S_SW_10_07_FI_EXC_POST | WERKS | Plant | CHAR(4) | WERKS_D |
| /SKN/S_SW_10_07_FI_EXC_POST | WORKING_DAYS | Fatory calendar flag | CHAR(1) | CIND |
| /SKN/S_SW_10_07_FI_EXC_POST | WRBTR | Amount in document currency | CURR(13,2) | WRBTR |
| /SKN/S_SW_10_07_FI_EXC_POST | XBLNR | Reference Document Number | CHAR(16) | XBLNR1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_07_FI_EXC_POST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_07_FI_EXC_POST OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE:
               FULL           CHAR1,
               BACKDAYS       INT4,
               DATE_REF_FLD   NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               LANGU          SPRAS,
               BUZEI          NUMC3,
               GVTYP          CHAR2,
               SHKZG          SHKZG,
               WORKING_DAYS   CIND,
               WFCID          WFCID.
*               ,
*               dmbtr          dmbtr,
*               wrbtr          wrbtr,
*               dmbe2          dmbe2
  DATA_MULTY:
              BUKRS   CHAR10,
              BELNR   CHAR10,
              GJAHR   NUMC4,
              BLART   CHAR10,
              TCODE   CHAR20,
              CPUDT   DATS,
              AEDAT   DATS,
              UPDDT   DATS,
              BUDAT   DATS,
              BLDAT   DATS,
              DMBTR   DMBTR,
              WRBTR   WRBTR,
              DMBE2   DMBE2,
              DMBE3   DMBE3,
              HWAER   HWAER,
              HWAE2   HWAE2,
              HWAE3   HWAE3,
              KTOPL   CHAR4,
              GVTYP   CHAR2,
              USNAM   CHAR12,
              XBLNR   CHAR12,
              MONAT   NUMC2,
              BSTAT   CHAR1,
              GRPID   CHAR12,
              SGXTX   SGTXT,       " Yuri C.++ 01.02.19
              BSCHL   BSCHL,       " Yuri C.++ 01.02.19
              DURATION  /SKN/E_SW_DURATION,
              DATUM    SY-DATUM,
              HKONT    CHAR10,
              XBILK    CHAR1,
              KTOKS    CHAR4,
              SGTXT    TEXT50
              .
  "backdays/date section
  LV_BACKDAYS      = 1.
  LV_DATE_REF_FLD  = 'CPUDT'.    " Documnt Date
  LV_DURATION_UNIT = 'D'.
  LV_LANGU         = 'E'.
  SELECT_SINGLE:
                 BACKDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT,
                 FULL,
                 SHKZG,         " Yuri C.++ 28.09.19
                 WORKING_DAYS,
                 WFCID.
  SELECT_MULTY:
                BUKRS,
                BELNR,
                GJAHR,
                BLART,
                TCODE,
                CPUDT,
                AEDAT,
                UPDDT,
                BUDAT,
                BLDAT,
                DMBTR,
                WRBTR,
                DMBE2,
                DMBE3,
                HWAER,
                HWAE2,
                HWAE3,
                KTOPL,
                GVTYP,
                USNAM,
                XBLNR,
                MONAT,
                BSTAT,
                GRPID,
                SGXTX,          " Yuri C.++ 01.02.19
                BSCHL,          " Yuri C.++ 01.02.19
                DURATION,
                DATUM,
                HKONT,
                XBILK,
                KTOKS,
                SGTXT
                .
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY,
                 <FS_DATA> TYPE /SKN/S_SW_10_07_FI_EXC_POST .
  DATA : SY_TABIX LIKE SY-TABIX ,
         FLD(60) TYPE C ,
         REF_DATE TYPE D.
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM,
         TIME_DIFF TYPE  INT4  .
  DATA: LV_HKONT   TYPE CHAR10,
        LV_INDIC   TYPE SCAL-INDICATOR,
        LV_WORK    TYPE SCAL-INDICATOR,
        LV_FACDATE TYPE SCAL-FACDATE,
        LV_DATE    TYPE DATUM,
        LV_DATE_IN TYPE DATUM,
        LV_TABIX   TYPE SYTABIX,
        LV_KTOPL   TYPE T001-KTOPL.
  DATA: LS_DATA     TYPE /SKN/S_SW_10_07_FI_EXC_POST,
        LS_BKPF     TYPE /SKN/S_SW_10_07_FI_EXC_POST,
        LS_BSEG     TYPE BSEG,
        LS_BSEG_TMP TYPE BSEG,
        LS_SKA1     TYPE SKA1.
*        ls_t001     TYPE ty_t001.
  DATA: LT_BKPF     TYPE STANDARD TABLE OF /SKN/S_SW_10_07_FI_EXC_POST,
        LT_BSEG     TYPE STANDARD TABLE OF BSEG,
        LT_BSEG_TMP TYPE STANDARD TABLE OF BSEG,
        LT_SKA1     TYPE STANDARD TABLE OF SKA1,
        LT_DATA     TYPE STANDARD TABLE OF /SKN/S_SW_10_07_FI_EXC_POST.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_07_FI_EXC_POST'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
*** Begin Yuri C.++ 20.07.19
*  IF lv_working_days IS NOT INITIAL.
*    lv_wfcid = 'X'.
*  ENDIF.
*** End Yuri C.++ 20.07.19
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  CONVERT_MULTY: BELNR ALPHA.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'BUDAT'.
      R_BUDAT[] = R_DATUM[]. "Expected debit date
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "Date on Which Record Was Created
    WHEN 'CPUDT'.
      R_CPUDT[] = R_DATUM[]. "Day On Which Accounting Document Was Entered
    WHEN 'UPDDT'.
      R_UPDDT[] = R_DATUM[]. "Date of the Last Document Update
    WHEN 'BLDAT'.
      R_BLDAT[] = R_DATUM[]. "Document Date in Document
    WHEN OTHERS.
      R_CPUDT[] = R_DATUM[]. "Document date
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_BKPF
    FROM BKPF AS A
    INNER JOIN BSIS AS B       ON  A~BUKRS EQ B~BUKRS
                               AND A~BELNR EQ B~BELNR
                               AND A~GJAHR EQ B~GJAHR
                               AND B~SHKZG EQ LV_SHKZG     " Yuri C++ 28.08.19
    INNER JOIN SKA1  AS C      ON  B~HKONT EQ C~SAKNR
    INNER JOIN T001  AS T      ON  A~BUKRS EQ T~BUKRS        " Yuri C.++ 17.07.19
                               AND C~KTOPL EQ T~KTOPL        " Yuri C.++ 17.07.19
*     INNER JOIN t001w AS t ON b~werks EQ t~werks            " Yuri C.-- 26.01.19
    WHERE A~BUKRS IN R_BUKRS
    AND   A~BELNR IN R_BELNR
    AND   A~GJAHR IN R_GJAHR
    AND   A~BLART IN R_BLART
    AND   A~TCODE IN R_TCODE
    AND   A~HWAER IN R_HWAER
    AND   A~HWAE2 IN R_HWAE2
    AND   A~HWAE3 IN R_HWAE3
    AND   A~BUDAT IN R_BUDAT
    AND   A~AEDAT IN R_AEDAT
    AND   A~CPUDT IN R_CPUDT
    AND   A~UPDDT IN R_UPDDT
    AND   A~BLDAT IN R_BLDAT
    AND   A~XBLNR IN R_XBLNR
    AND   A~MONAT IN R_MONAT
    AND   A~BSTAT IN R_BSTAT
    AND   A~GRPID IN R_GRPID
    AND   B~DMBTR IN R_DMBTR
    AND   B~WRBTR IN R_WRBTR
    AND   B~DMBE2 IN R_DMBE2
    AND   B~DMBE3 IN R_DMBE3
    AND   B~HKONT IN R_HKONT
    AND   B~SGTXT IN R_SGTXT    " Yuri C.++ 01.02.19
    AND   B~BSCHL IN R_BSCHL    " Yuri C.++ 01.02.19
    AND   B~SGTXT IN R_SGTXT
    AND   C~GVTYP IN R_GVTYP
    AND   C~XBILK IN R_XBILK
    AND   C~KTOPL IN R_KTOPL
    AND   C~KTOKS IN R_KTOKS.
  SELECT *
     APPENDING CORRESPONDING FIELDS OF TABLE LT_BKPF
     FROM BKPF AS A INNER JOIN BSAS AS B ON  A~BUKRS EQ B~BUKRS
                                         AND A~BELNR EQ B~BELNR
                                         AND A~GJAHR EQ B~GJAHR
                                         AND B~SHKZG EQ LV_SHKZG     " Yuri C++ 28.08.19
                    INNER JOIN SKA1 AS C ON  B~HKONT EQ C~SAKNR
                    INNER JOIN T001 AS T ON  A~BUKRS EQ T~BUKRS        " Yuri C.++ 17.07.19
                                         AND C~KTOPL EQ T~KTOPL        " Yuri C.++ 17.07.19
*     INNER JOIN t001w AS t ON b~werks EQ t~werks                   " Yuri C.--
     WHERE A~BUKRS IN R_BUKRS
     AND   A~BELNR IN R_BELNR
     AND   A~GJAHR IN R_GJAHR
     AND   A~BLART IN R_BLART
     AND   A~TCODE IN R_TCODE
     AND   A~HWAER IN R_HWAER
     AND   A~HWAE2 IN R_HWAE2
     AND   A~HWAE3 IN R_HWAE3
     AND   A~BUDAT IN R_BUDAT
     AND   A~AEDAT IN R_AEDAT
     AND   A~CPUDT IN R_CPUDT
     AND   A~UPDDT IN R_UPDDT
     AND   A~BLDAT IN R_BLDAT
     AND   A~XBLNR IN R_XBLNR
     AND   A~MONAT IN R_MONAT
     AND   A~BSTAT IN R_BSTAT
     AND   A~GRPID IN R_GRPID
     AND   B~DMBTR IN R_DMBTR
     AND   B~WRBTR IN R_WRBTR
     AND   B~DMBE2 IN R_DMBE2
     AND   B~DMBE3 IN R_DMBE3
     AND   B~HKONT IN R_HKONT
     AND   B~SGTXT IN R_SGTXT
     AND   C~GVTYP IN R_GVTYP
     AND   C~XBILK IN R_XBILK
     AND   C~KTOPL IN R_KTOPL
     AND   C~KTOKS IN R_KTOKS
    .
*** Yuri C.++ 28.12.18
*** Begin Yuri C.-- 11.11.18
*  APPEND LINES OF t_data TO lt_bkpf.
*  REFRESH t_data.
*  LOOP AT lt_bkpf INTO ls_bkpf.
*
*    IF ls_bkpf-usnam IS NOT INITIAL.
*      CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
*        EXPORTING
*          bname      = ls_bkpf-usnam " User Name in User Master Record
*          "SW_DEST    =     " Logical Destination (Specified in Function Call)
*        IMPORTING
*          name_first = ls_bkpf-ad_namefir " First name
*          name_last  = ls_bkpf-ad_namelas " Last name
**         NAME_TEXT  =     " Full Name of Person
**         WA_ADRP    =     " Persons (Business Address Services)
*        EXCEPTIONS
*          no_data    = 1
*          OTHERS     = 2.
*
*      IF sy-subrc <> 0.
**       message id sy-msgid type sy-msgty number sy-msgno
**                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*    ENDIF.
*
*    IF ls_bkpf-hkont IS NOT INITIAL.
*      CALL FUNCTION '/SKN/F_SW_10_GL_DESC'
*        EXPORTING
*          ktopl      = ls_bkpf-ktopl " Chart of Accounts
*          saknr      = ls_bkpf-hkont "  G/L Account Number
*          langu      = lv_langu    " Language Key
**         SW_DEST    =     " Logical Destination (Specified in Function Call)
*        IMPORTING
**         TXT20      =     " G/L account short text
*          txt50      = ls_bkpf-hkont_desc " G/L Account Long Text
*        EXCEPTIONS
*          wrong_code = 1
*          OTHERS     = 2.
*      IF sy-subrc <> 0.
**         message id sy-msgid type sy-msgty number sy-msgno
**                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*    ENDIF.
*
*    APPEND ls_bkpf TO t_data.
*
*  ENDLOOP.
*** End Yuri C.-- 11.11.18
  SORT LT_BKPF BY BUKRS BELNR GJAHR.     " Yuri C.++ 11.11.18
  IF LV_FULL EQ 'X'.
*** Begin Yuri C.-- 11.11.18
*    REFRESH lt_bkpf.
*    APPEND LINES OF t_data TO lt_bkpf.
*    APPEND LINES OF t_data TO lt_data_temp.
*    SORT lt_data_temp BY belnr ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM lt_data_temp COMPARING gjahr belnr.
*** End Yuri C.-- 11.11.18
*** Begin Yuri C.++ 11.11.18
    DELETE ADJACENT DUPLICATES FROM LT_BKPF COMPARING BUKRS BELNR GJAHR.
*** End Yuri C.++ 11.11.18
*    IF lt_data_temp IS NOT INITIAL.    " Yuri C.-- 11.11.18
    IF LT_BKPF IS NOT INITIAL.          " Yuri C.++ 11.11.18
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE LT_BSEG
        FROM BSEG
        FOR ALL ENTRIES IN LT_BKPF
*** Yuri C.-- 11.11.18
*        WHERE a~gjahr EQ lt_data_temp-gjahr
*        AND   a~belnr EQ lt_data_temp-belnr
*** Yuri C.-- 11.11.18
*** Yuri C.++ 11.11.18
        WHERE BUKRS EQ LT_BKPF-BUKRS
        AND   BELNR EQ LT_BKPF-BELNR
        AND   GJAHR EQ LT_BKPF-GJAHR.
      IF LT_BSEG IS NOT INITIAL.
        LT_BSEG_TMP = LT_BSEG.
        SORT LT_BSEG_TMP BY HKONT XBILK GVTYP.
        SELECT *
          FROM SKA1
          INTO TABLE LT_SKA1
          FOR ALL ENTRIES IN LT_BSEG_TMP
          WHERE SAKNR EQ LT_BSEG_TMP-HKONT.
        SORT LT_SKA1 BY SAKNR.
      ENDIF.
*** Yuri C.++ 11.11.18
*** Yuri C.-- 11.11.18
*        AND   a~bukrs IN r_bukrs
*        AND   a~gjahr IN r_gjahr
*** Yuri C.-- 11.11.18
*      SORT lt_bkpf BY gjahr ASCENDING bukrs ASCENDING belnr ASCENDING.
*      SORT lt_bseg BY gjahr ASCENDING bukrs ASCENDING belnr ASCENDING ktopl ASCENDING hkont ASCENDING buzei ASCENDING.
*      DELETE ADJACENT DUPLICATES FROM lt_bkpf COMPARING gjahr bukrs  belnr ktopl hkont buzei.
      REFRESH T_DATA.
      LOOP AT LT_BKPF INTO LS_BKPF.
        CLEAR: LS_DATA.                           " Yuri C.++ 12.11.18
        MOVE-CORRESPONDING LS_BKPF TO LS_DATA.    " Yuri C.++ 12.11.18
        LOOP AT LT_BSEG INTO LS_BSEG WHERE BUKRS EQ LS_BKPF-BUKRS
                                     AND   BELNR EQ LS_BKPF-BELNR
                                     AND   GJAHR EQ LS_BKPF-GJAHR.
*** Yuri C.-- 12.11.18
*          MOVE-CORRESPONDING ls_bkpf TO ls_bseg.
*          lv_dmbtr = ls_bseg-dmbtr.
*          lv_wrbtr = ls_bseg-wrbtr.
*          lv_dmbe2 = ls_bseg-dmbe2.
*          lv_buzei = ls_bseg-buzei.
*          lv_gvtyp = ls_bseg-gvtyp.
*          lv_shkzg = ls_bseg-shkzg.
*          lv_hkont = ls_bseg-hkont.
*          ls_bseg-buzei = lv_buzei.
*          ls_bseg-gvtyp = lv_gvtyp.
*          ls_bseg-shkzg = lv_shkzg.
*          ls_bseg-dmbtr = lv_dmbtr.
*          ls_bseg-dmbe2 = lv_dmbe2.
*          ls_bseg-wrbtr = lv_wrbtr.
*          ls_bseg-hkont = lv_hkont.
*          APPEND ls_bseg TO t_data.
*** Yuri C.-- 12.11.18
*** Yuri C.++ 12.11.18
          MOVE-CORRESPONDING LS_BSEG TO LS_DATA.
          LS_DATA-FULL  = 'X'.
          IF LS_BKPF-USNAM IS NOT INITIAL.
* user name & surname, G/L account description
            CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
              EXPORTING
                BNAME      = LS_BKPF-USNAM " User Name in User Master Record
*               sw_dest    = lv_sw_dest " Logical Destination (Specified in Function Call)
              IMPORTING
                NAME_FIRST = LS_DATA-AD_NAMEFIR " First name
                NAME_LAST  = LS_DATA-AD_NAMELAS " Last name
*               NAME_TEXT  =     " Full Name of Person
*               WA_ADRP    =     " Persons (Business Address Services)
              EXCEPTIONS
                NO_DATA    = 1
                OTHERS     = 2.
          ENDIF.
          CLEAR LS_SKA1.
          READ TABLE LT_SKA1 INTO LS_SKA1 WITH KEY SAKNR = LS_BSEG-HKONT
                                                   BINARY SEARCH.
          IF SY-SUBRC = 0.
            IF LS_SKA1-SAKNR IS NOT INITIAL.
              CALL FUNCTION '/SKN/FC_SW_10_GL_DESC'
                EXPORTING
                  KTOPL      = LS_SKA1-KTOPL " Chart of Accounts
                  SAKNR      = LS_SKA1-SAKNR " G/L Account Number
                  LANGU      = LV_LANGU      " Language Key
*                 sw_dest    = lv_sw_dest    " Logical Destination (Specified in Function Call)
                IMPORTING
*                 TXT20      =                    " G/L account short text
                  TXT50      = LS_DATA-HKONT_DESC " G/L Account Long Text
                EXCEPTIONS
                  WRONG_CODE = 1
                  OTHERS     = 2.
            ENDIF.
          ENDIF.
          APPEND LS_DATA TO T_DATA.
*** Yuri C.++ 12.11.18
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ELSE.
* Not full
    LOOP AT LT_BKPF INTO LS_BKPF.
      IF LS_BKPF-USNAM IS NOT INITIAL.
        CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
          EXPORTING
            BNAME      = LS_BKPF-USNAM " User Name in User Master Record
            "SW_DEST    =     " Logical Destination (Specified in Function Call)
          IMPORTING
            NAME_FIRST = LS_BKPF-AD_NAMEFIR " First name
            NAME_LAST  = LS_BKPF-AD_NAMELAS " Last name
*           NAME_TEXT  =     " Full Name of Person
*           WA_ADRP    =     " Persons (Business Address Services)
          EXCEPTIONS
            NO_DATA    = 1
            OTHERS     = 2.
      ENDIF.
      IF LS_BKPF-HKONT IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_GL_DESC'
          EXPORTING
            KTOPL      = LS_BKPF-KTOPL " Chart of Accounts
            SAKNR      = LS_BKPF-HKONT "  G/L Account Number
            LANGU      = LV_LANGU    " Language Key
*           SW_DEST    =     " Logical Destination (Specified in Function Call)
          IMPORTING
*           TXT20      =     " G/L account short text
            TXT50      = LS_BKPF-HKONT_DESC " G/L Account Long Text
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
      ENDIF.
      APPEND LS_BKPF TO T_DATA.
    ENDLOOP.
  ENDIF.
*** Begin Yuri C.++ 20.07.19
  CHECK T_DATA IS NOT INITIAL.
  IF LV_WFCID IS NOT INITIAL.
    LT_DATA = T_DATA[].
    REFRESH T_DATA.
    FLD = LV_DATE_REF_FLD.
    CASE LV_DATE_REF_FLD.
      WHEN 'BUDAT'.
        SORT LT_DATA BY BUDAT.
      WHEN 'AEDAT'.
        SORT LT_DATA BY AEDAT.
      WHEN 'CPUDT'.
        SORT LT_DATA BY CPUDT.
      WHEN 'UPDDT'.
        SORT LT_DATA BY UPDDT.
      WHEN 'BLDAT'.
        SORT LT_DATA BY BLDAT.
      WHEN OTHERS.
        SORT LT_DATA BY CPUDT.
    ENDCASE.
    LOOP AT LT_DATA ASSIGNING <FS_DATA>.
      ASSIGN COMPONENT FLD OF STRUCTURE <FS_DATA> TO .
      REF_DATE = .
      IF LV_DATE_IN <> REF_DATE.
        CLEAR: LV_FACDATE, LV_WORK, LV_DATE.
        LV_DATE_IN = REF_DATE.
        CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
          EXPORTING
*           correct_option               = lv_indic
            DATE                         = LV_DATE_IN
            FACTORY_CALENDAR_ID          = LV_WFCID
          IMPORTING
            DATE                         = LV_DATE
            FACTORYDATE                  = LV_FACDATE
            WORKINGDAY_INDICATOR         = LV_WORK
          EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
        IF SY-SUBRC <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF LV_WORK IS INITIAL.
        <FS_DATA>-WORKING_DAYS = C_YES.
      ELSE.
        <FS_DATA>-WORKING_DAYS = C_NO.
      ENDIF.
      <FS_DATA>-FACDATE = LV_FACDATE.  " Factory date related to calendar id
* Filter by Working day/Holiday
      IF LV_WORKING_DAYS IS NOT INITIAL.
        IF LV_WORKING_DAYS EQ C_YES.     " Filter by Working Days
          IF LV_WORK IS NOT INITIAL.     " It's a holiday
            CONTINUE.
          ENDIF.
        ELSEIF LV_WORKING_DAYS EQ C_NO.  " Filter by Holiday
          IF LV_WORK IS INITIAL.    " It's a working day
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND <FS_DATA> TO T_DATA[].
    ENDLOOP.
  ENDIF.
  SORT T_DATA BY BUKRS BELNR GJAHR.
*** End Yuri C.++ 20.07.19
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    SY_TABIX = SY-TABIX .
*** Yuri C.-- 28.12.18
*    CONCATENATE 'T_DATA-' lv_date_ref_fld INTO fld .
*    ASSIGN (fld) TO .
*** Yuri C.-- 28.12.18
*** Yuri C.++ 28.12.18
    FLD = LV_DATE_REF_FLD.
    ASSIGN COMPONENT FLD OF STRUCTURE <FS_DATA> TO .
*** Yuri C.++ 28.12.18
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
*      MODIFY t_data INDEX sy_tabix.              " Yuri C.-- 28.12.18
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```