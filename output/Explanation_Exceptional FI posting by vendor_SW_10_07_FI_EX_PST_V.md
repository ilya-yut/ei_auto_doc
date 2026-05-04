# Exception Indicator: Exceptional FI posting by vendor - SW_10_07_FI_EX_PST_V

## General Overview

This Exception Indicator (EI) monitors vendor-related financial postings and open items in Accounts Payable, highlighting amounts or patterns that exceed configured thresholds across company codes, documents, and G/L accounts.

This EI serves as an essential control for finance and vendor master oversight by:
- Surfacing exceptional vendor posting volumes or values before they distort period results or accrual positions
- Supporting vendor-level and account-level views of aggregated exposure for targeted review during close or audit cycles
- Helping teams distinguish document-level anomalies from portfolio-wide concentration across purchasing entities
- Enabling follow-up on sensitive G/L relationships tied to vendor activity without manual sampling of BKPF/BSEG data
- Providing traceable context (document keys, currencies, rates, and texts) that accelerates investigation when thresholds are breached

The EI is suited to month-end financial close, continuous controls over vendor spend, and audit sampling where vendor posting behavior must stay within agreed materiality. It reads vendor master and FI documents (including open vendor items) and applies aggregation rules you configure before raising alerts.



## Problem Description

Failure to monitor exceptional vendor postings and related open-item activity creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Large or unusual vendor postings may distort expense recognition, accruals, or intercompany balances if they surface only after books are closed
- Multi-currency documents can hide FX-driven spikes when local and document currency views are not reviewed together
- Misclassified or duplicated postings can accumulate when no one watches aggregated vendor and G/L combinations during the period
- Threshold breaches tied to specific document types or header texts may go unnoticed without systematic scanning
- Open items that age without resolution can misstate liquidity and vendor liability positions in management reporting

**Operational and Control Risks**
- Accounts Payable may spend disproportionate time on ad hoc queries instead of prioritized exceptions
- Master data inconsistencies between vendor attributes and posting behavior are harder to detect without automated aggregation
- Mass postings or batch sessions can introduce concentrated errors that spread across many documents before manual review catches them
- Debit and credit imbalances on sensitive accounts may indicate process breakdowns in invoice entry or clearing
- Operational teams lack a single view of which vendors or documents drove the largest movements in a period

**Management Visibility and Decision-Making Risks**
- Executives receive lagging indicators when exceptional vendor activity is discovered only during external audit
- Procurement and finance cannot align on supplier risk when posting concentration is invisible until quarter end
- Strategic decisions on payment terms or vendor onboarding suffer when historical posting anomalies are not trended
- Cross-company views are blurred if company-code slices are not monitored with consistent thresholds
- Remediation efforts start late because no early warning tied postings to the responsible document context

## Suggested Resolution

**Immediate Response**
- Review flagged vendor postings with the document display transactions for FI to confirm business substance and supporting approvals
- Compare document, posting, and entry dates to understand whether the exception reflects timing, cut-off, or data entry issues
- Validate vendor master attributes (account group, company assignments) for records that appear repeatedly in alerts
- Check whether the movement ties to expected business events such as period accruals, one-time adjustments, or project closures
- Escalate material findings to the controller and process owner before further payments or clearings proceed

**System Assessment**
- Reconcile aggregated totals back to line items and source documents to confirm the aggregation level matches the intended control design
- Trend results across prior periods with the same parameters to see if the spike is new or recurring seasonal behavior
- Examine currency and parallel-currency amounts together when thresholds are defined in more than one valuation view
- Review G/L accounts involved to determine whether postings belong on those accounts or indicate mapping errors
- Inspect reference and text fields when the logic relies on narrative cues for policy-sensitive postings

**Corrective Actions**
- Post correcting financial documents or reverse erroneous entries after approval through standard FI change procedures
- Update vendor master or G/L master data where the root cause is configuration or attribute mismatch
- Adjust monitoring thresholds or aggregation scope when legitimate business growth explains the pattern
- Document investigation outcomes and control adjustments for audit trail and SOX evidence
- Schedule recurring runs during close windows so exceptions are cleared before management certification
- Coordinate with procurement on vendor communication when repeated posting issues trace to supplier billing practices



## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACC_DESC | Short Text | CHAR | 20 | 0 | TXT20_SKAT | TEXT20 |
| 2 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT | DATUM |
| 3 | AGGKEY | Aggregation Key | CHAR | 32 | 0 | /ABC4C/E_SW_AGGKEY | CHAR32 |
| 4 | AGGLEVEL | Aggregation Level | CHAR | 10 | 0 | /ABC4C/E_SW_AGGLEVEL | CHAR10 |
| 5 | BACKDAYS | Days Backwards |  | 0 | 0 |  |  |
| 6 | BELNR | Document Number | CHAR | 10 | 0 | BELNR_D | BELNR |
| 7 | BKTXT | Description | CHAR | 30 | 0 | BLTXT | TEXT30 |
| 8 | BLART | Document Type | CHAR | 2 | 0 | BLART | BLART |
| 9 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 10 | BSTAT | Doc.status | CHAR | 1 | 0 | BSTAT_D | BSTAT |
| 11 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 12 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 13 | BUZEI | Line item | NUMC | 3 | 0 | BUZEI | BUZEI |
| 14 | COMP_CODE_DESC | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 15 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 16 | DATE_REF_FLD | Date Reference Field |  | 0 | 0 |  |  |
| 17 | DMBE2 | LC2 amount | CURR | 13 | 2 | DMBE2 | WERT7 |
| 18 | DMBE2_AG | Amount in Loc. Cur. 2 Agg. | CURR | 15 | 2 | /ABC4C/E_SW_DMBE2_AG | WRTV8 |
| 19 | DMBE3 | LC3 amount | CURR | 13 | 2 | DMBE3 | WERT7 |
| 20 | DMBE3_AG | Amount in Loc. Cur. 3 Agg. | CURR | 15 | 2 | /ABC4C/E_SW_DMBE3_AG | WRTV8 |
| 21 | DMBTR | Amount in LC | CURR | 13 | 2 | DMBTR | WERT7 |
| 22 | DMBTR_AG | Amount in Loc. Cur. Agg. | CURR | 15 | 2 | /ABC4C/E_SW_DMBTR_AG | WRTV8 |
| 23 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 24 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 25 | FROMDATE | From Date of Extraction | DATS | 8 | 0 | /ABC4C/E_SW_FROM_DATE_EX | DATUM |
| 26 | GJAHR | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 27 | GRPID | Session name | CHAR | 12 | 0 | GRPID_BKPF | CHAR12 |
| 28 | HKONT | G/L Account | CHAR | 10 | 0 | HKONT | SAKNR |
| 29 | HWAE2 | Local currency 2 | CUKY | 5 | 0 | HWAE2 | WAERS |
| 30 | HWAE3 | Local currency 3 | CUKY | 5 | 0 | HWAE3 | WAERS |
| 31 | HWAER | Local Currency | CUKY | 5 | 0 | HWAER | WAERS |
| 32 | KOART | Account Type | CHAR | 1 | 0 | KOART | KOART |
| 33 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 34 | KURS2 | Exchange rate 2 | DEC | 9 | 5 | KURS2 | KURSF |
| 35 | KURS3 | Exchange rate 3 | DEC | 9 | 5 | KURS3 | KURSF |
| 36 | KURSF | Exchange rate | DEC | 9 | 5 | KURSF | KURSF |
| 37 | KZKRS | KZKRS | DEC | 9 | 5 | KZKRS | KURSF |
| 38 | KZWRS | Group currency | CUKY | 5 | 0 | KZWRS | WAERS |
| 39 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 40 | MONAT | Posting Period | NUMC | 2 | 0 | MONAT | MONAT |
| 41 | SGTXT | Text | CHAR | 50 | 0 | SGTXT | TEXT50 |
| 42 | SHKZG | Debit/Credit Ind. | CHAR | 1 | 0 | SHKZG | SHKZG |
| 43 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 44 | TODATE | To Date of Extraction | DATS | 8 | 0 | /ABC4C/E_SW_TO_DATE_EX | DATUM |
| 45 | UPDDT | Last update | DATS | 8 | 0 | UPDDT | DATUM |
| 46 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 47 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 48 | WRBTR | Amount | CURR | 13 | 2 | WRBTR | WERT7 |
| 49 | WRBTR_AG | Amount in Doc. Cur. Agg. | CURR | 15 | 2 | /ABC4C/E_SW_WRBTR_AG | WRTV8 |
| 50 | XBLNR | Reference | CHAR | 16 | 0 | XBLNR1 | XBLNR1 |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 50 parameters listed in the Parameters Reference Table above.

**ACC_DESC** (Short Text):

G/L account short text on the result row helps reviewers recognize the account without opening master data.

**AEDAT** (Changed on):

Changed-on date filtering limits line items to those last modified in the chosen interval, supporting review of recent master or posting changes.

**AGGKEY** (Aggregation Key):

Aggregation key values identify the bucket (vendor, document, or account combination) produced under the selected aggregation mode.

**AGGLEVEL** (Aggregation Level):

Aggregation level selects whether exceptions are evaluated per vendor total, per document, per document and G/L line, or per G/L across documents.

**AGGLEVEL Options:**
- **DOC-GL**: vendor plus document plus G/L aggregation grain
- **GL**: vendor plus G/L across documents
- **DOC**: vendor plus document totals
- Any other value uses vendor-level totals across documents

**BACKDAYS** (Days Backwards):

When no explicit extraction from/to dates are supplied, backward day count establishes how far back open vendor items and documents are read from the current day.

**BELNR** (Document Number):

Accounting document number ranges keep the scan on known postings instead of scanning the entire fiscal year population.

**BKTXT** (Description):

Header text patterns refine which financial document narratives qualify, useful when business meaning is captured in BKTXT.

**BLART** (Document Type):

Document type ranges align the check with journals of interest such as invoices, credit memos, or manual postings.

**BLDAT** (Document Date):

Document date selection focuses on the business transaction date printed on the document rather than posting or entry timing.

**BSTAT** (Doc.status):

Document status values separate cleared, parked, or held documents so alerts reflect the lifecycle state you care about.

**BUDAT** (Posting Date):

Posting date filtering ties results to the period when amounts hit the general ledger, which matters for period close and accrual reviews.

**BUKRS** (Company Code):

Company code scoping ensures vendor open items are evaluated within the legal entity that owns the books.

**BUZEI** (Line item):

Line item number ranges target specific rows inside a document when investigating known anomalies.

**COMP_CODE_DESC** (Company Name):

Company name text on the output supports human-readable reporting next to the company code key.

**CPUDT** (Entry Date):

Entry date filtering highlights documents captured in the system during a window, which can differ from posting or document date.

**DATE_REF_FLD** (Date Reference Field):

This selector maps the configured calendar window to the FI date field that should drive inclusion (posting, document, entry, change, or last update).

**DATE_REF_FLD Options:**
- **BUDAT**: posting date drives the window
- **AEDAT**: change date on the record drives the window
- **CPUDT**: system entry date drives the window
- **UPDDT**: last document update date drives the window
- **BLDAT**: document date drives the window
- Any other value falls back to posting-date behavior in code

**DMBE2** (LC2 amount):

Second local currency amount thresholds detect large movements in parallel valuation currency alongside the primary local amount.

**DMBE2_AG** (Amount in Loc. Cur. 2 Agg.):

Aggregated second-local-currency totals surface combined exposure after summation at vendor, document, or account grain.

**DMBE3** (LC3 amount):

Third local currency amount filters extend the same idea for tertiary group valuation where configured.

**DMBE3_AG** (Amount in Loc. Cur. 3 Agg.):

Aggregated third-local-currency totals complete the multi-currency picture for the chosen aggregation level.

**DMBTR** (Amount in LC):

Local currency line amounts restrict items before aggregation so only economically material postings contribute.

**DMBTR_AG** (Amount in Loc. Cur. Agg.):

Aggregated local currency totals are compared to thresholds after summing by the active aggregation key.

**DURATION** (Duration In Time Units):

Together with its unit, duration caps how old a posting may be relative to the reference date field when age-based filtering is applied.

**DURATION_UNIT** (Duration Unit):

The unit interprets the numeric duration as hours, minutes, days, or full-day semantics so age windows are not misread.

**DURATION_UNIT Options:**
- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**FROMDATE** (From Date of Extraction):

Explicit extraction start date overrides the default rolling window when you need a fixed reporting interval.

**GJAHR** (Fiscal Year):

Fiscal year boundaries keep BSEG/BKPF reads within approved years and avoid scanning unrelated history.

**GRPID** (Session name):

Batch input session name on the header narrows documents created under a specific mass-processing run.

**HKONT** (G/L Account):

G/L account ranges steer the indicator toward sensitive accounts such as expense, suspense, or one-off postings.

**HWAE2 - HWAE3** (Local currency 2 / Local currency 3):

Second and third local currency keys jointly scope documents that use parallel currency slots two and three, so multi-currency reviews stay aligned with how company-code valuation is configured.

**HWAER** (Local Currency):

Local currency key selection ensures comparisons happen in the correct company-code currency context.

**KOART** (Account Type):

Account type on the line (for example vendor or G/L) aligns selection with the subledger dimension under review.

**KTOKK** (Account group):

Vendor account group filters focus the run on strategic supplier categories instead of the entire vendor base.

**KURS2 - KURS3** (Exchange rate 2 / Exchange rate 3):

Exchange rate slots two and three are evaluated together when you need to bound secondary and tertiary conversion factors that appear alongside the primary document rate.

**KURSF** (Exchange rate):

Primary exchange rate selection can isolate postings with unusual conversion factors.

**KZKRS** (KZKRS):

Group currency rate field filtering helps when group valuation rates are stored and need boundary checks.

**KZWRS** (Group currency):

Group currency key narrows to documents valued in a specific group currency.

**LIFNR** (Vendor):

Vendor account numbers limit evaluation to named suppliers for targeted audits or remediation.

**MONAT** (Posting Period):

Posting period selection aligns results with a specific fiscal month for close-focused monitoring.

**SGTXT** (Text):

Line item text patterns catch narrative cues such as project codes or manual explanations tied to sensitive postings.

**SHKZG** (Debit/Credit Ind.):

Debit and credit indicator filtering separates increases and decreases in open items for directional analysis.

**TCODE** (Transaction Code):

Transaction code on the document header ties activity to the user action that created or changed the posting.

**TODATE** (To Date of Extraction):

Explicit extraction end date pairs with the from date to bound a closed interval for regulatory or audit samples.

**UPDDT** (Last update):

Last update date filtering emphasizes documents that were touched recently, which helps chase ongoing corrections.

**VENDOR_DESC** (Name):

Vendor name on the output speeds triage when multiple vendor numbers appear in one alert population.

**WAERS** (Currency):

Document currency key filtering keeps the exception set inside a currency you are defending against FX or policy risk.

**WRBTR** (Amount):

Document currency line amounts bound item-level values before any aggregation logic runs.

**WRBTR_AG** (Amount in Doc. Cur. Agg.):

Aggregated document currency totals highlight vendor-level or document-level exposure in original transaction currency.

**XBLNR** (Reference):

Reference document number patterns match external references such as invoice numbers for traceability.



### Parameter Relationships

How parameter combinations work together

Vendor master filters (company code, vendor, account group) define which suppliers enter the evaluation. Fiscal year and document keys bound the FI read. Date reference, backward days, explicit from/to dates, and duration with unit jointly shape the time window applied to the selected date field. Amount and currency filters on header and line restrict which postings contribute before aggregation. Aggregation level decides whether thresholds apply to vendor totals, document totals, or vendor-plus-account combinations. Threshold ranges on local, document, and parallel-currency amounts determine which aggregated buckets become alerts. Text, status, transaction, and reference parameters add qualitative guards so only policy-relevant postings are considered.



### Default Values

- **DATE_REF_FLD** - BUDAT
- **KOART** - empty range ⇒ default excludes BSEG account type K
- **BACKDAYS** - initial — 0 ⇒ **one** day only: from = to = system date when default date fill runs; if set to **N**, from = today minus **N** days, to = today (inclusive span of **N+1** calendar days)
- **DURATION** - initial — no allowed band from caller; line filter still uses NOT IN R_DURATION—effect with an empty range varies by release (confirm on your system)
- **DURATION_UNIT** - initial — blank ⇒ age from the reference date to “now” is not computed for lines, so duration-based filtering may not apply as intended
- **AGGLEVEL** - initial — blank follows vendor-total aggregation, not DOC, GL, or DOC-GL

### Practical Example of Parameter Configuration

**Use Case 1: Month-end vendor concentration**

**Purpose:** Highlight vendors whose aggregated local-currency posting exceeds a ceiling in the closing week.

```
BACKDAYS = 7
DATE_REF_FLD = BUDAT
DURATION = 24
DURATION_UNIT = H
AGGLEVEL = GL
BUKRS = 1000
FROMDATE = 20260101
TODATE = 20260131
```

**Use Case 2: Document-level audit sample**

**Purpose:** Focus on specific document types and header text for a single vendor.

```
LIFNR = 0000123456
BLART = KR
BKTXT = ACCRUAL
AGGLEVEL = DOC-GL
BUDAT = 20260101
BUKRS = 1000
TCODE = FB60
```

**Use Case 3: Open-item currency watch**

**Purpose:** Combine currency and amount filters with posting period scoping.

```
WAERS = USD
WRBTR = 50000
MONAT = 01
GJAHR = 2026
HKONT = 0000400000
KTOKK = ZVEN
BACKDAYS = 30
```



## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /ABC4C/S_SW_10_07_FI_EX_PST_V | ACC_DESC | G/L account short text | CHAR(20) | TXT20_SKAT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | AEDAT | Changed On | DATS(8) | AEDAT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | AGGKEY | Aggregation Key | CHAR(32) | /ABC4C/E_SW_AGGKEY |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | AGGLEVEL | Aggregation Level | CHAR(10) | /ABC4C/E_SW_AGGLEVEL |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | BELNR | Accounting Document Number | CHAR(10) | BELNR_D |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | BKTXT | Document.Type Description | CHAR(30) | BLTXT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | BLART | Document Type | CHAR(2) | BLART |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | BLDAT | Document Date in Document | DATS(8) | BLDAT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | BSTAT | Document Status | CHAR(1) | BSTAT_D |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | BUKRS | Company Code | CHAR(4) | BUKRS |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | BUZEI | Number of Line Item Within Accounting Document | NUMC(3) | BUZEI |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | COMP_CODE_DESC | Name of Company Code or Company | CHAR(25) | BUTXT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | DMBE2 | Amount in Second Local Currency | CURR(13,2) | DMBE2 |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | DMBE2_AG | Amount in Local Currency 2 (Aggregated) | CURR(15,2) | /ABC4C/E_SW_DMBE2_AG |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | DMBE3 | Amount in Third Local Currency | CURR(13,2) | DMBE3 |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | DMBE3_AG | Amount in Local Currency 3 (Aggregated) | CURR(15,2) | /ABC4C/E_SW_DMBE3_AG |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | DMBTR | Amount in Local Currency | CURR(13,2) | DMBTR |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | DMBTR_AG | Amount in Local Currency (Aggregated) | CURR(15,2) | /ABC4C/E_SW_DMBTR_AG |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | FROMDATE | From Date of Extraction | DATS(8) | /ABC4C/E_SW_FROM_DATE_EX |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | GRPID | Batch Input Session Name | CHAR(12) | GRPID_BKPF |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | HKONT | General Ledger Account | CHAR(10) | HKONT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | HWAE2 | Currency Key of Second Local Currency | CUKY(5) | HWAE2 |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | HWAE3 | Currency Key of Third Local Currency | CUKY(5) | HWAE3 |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | HWAER | Local Currency | CUKY(5) | HWAER |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | KOART | Account Type | CHAR(1) | KOART |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | KTOKK | Vendor account group | CHAR(4) | KTOKK |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | KURS2 | Exchange Rate for the Second Local Currency | DEC(9,5) | KURS2 |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | KURS3 | Exchange Rate for the Third Local Currency | DEC(9,5) | KURS3 |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | KURSF | Exchange rate | DEC(9,5) | KURSF |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | KZKRS | Group Currency Exchange Rate | DEC(9,5) | KZKRS |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | KZWRS | Currency Key for the Group Currency | CUKY(5) | KZWRS |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | MONAT | Fiscal Period | NUMC(2) | MONAT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | SGTXT | Item Text | CHAR(50) | SGTXT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | SHKZG | Debit/Credit Indicator | CHAR(1) | SHKZG |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | TCODE | Transaction Code | CHAR(20) | TCODE |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | TODATE | To Date of Extraction | DATS(8) | /ABC4C/E_SW_TO_DATE_EX |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | UPDDT | Date of the Last Document Update | DATS(8) | UPDDT |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | VENDOR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | WAERS | Currency Key | CUKY(5) | WAERS |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | WRBTR | Amount in document currency | CURR(13,2) | WRBTR |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | WRBTR_AG | Amount in Document Currency (Aggregated) | CURR(15,2) | /ABC4C/E_SW_WRBTR_AG |
| /ABC4C/S_SW_10_07_FI_EX_PST_V | XBLNR | Reference Document Number | CHAR(16) | XBLNR1 |

## ABAP Code

```abap
FUNCTION /ABC4C/F_SW_10_07_FI_EX_PST_V.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /ABC4C/S_SW_10_07_FI_EX_PST_V
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
*  Date(dd-mmm-yyyy): 06-Feb-2019
*  Author           : Taotsu Nakamura - 50023710
*-----------------------------------------------------------------------
*  Alert Definition : The alert detects exceptional vendor’s posting within the specified period.
*                     It could be checked on document or aggregated level:
*                     Vendor item (document level), Specific GL item (document level),
*                     Total by vendor (aggregated), Total by Vendor and GL account (aggregated).
*-----------------------------------------------------------------------
*  CHANGE HISTORY
* ----------------------------------------------------------------------
*  DATE(DMY)     TP Request#   Programmer   Description
*  06-Feb-2019   SD3K900074    50023710     Create initial version
*  06-Feb-2019   SD3K900076    50023710     Correction by QA process
*  07-Feb-2019   SD3K900078    50023710     Correction by QA process
*  07-Feb-2019   SD3K900080    50023710     Correction by QA process
*  13-Feb-2019   SD3K900089    50023710     Correction by QA process
*  02-Mar-2019   SD3K900097    50023710     Correction by QA process
*  22-Jul-2019   SD3K900116    50023710     Correction for handling with multiple vendor item
* ----------------------------------------------------------------------
* ----------------------------------------------------------------------
* Local Type definition
* ----------------------------------------------------------------------
  TYPES:
    BEGIN OF TYP_SUM,
      AGGKEY     TYPE /ABC4C/E_SW_AGGKEY,           " Aggregation Level
      LIFNR      TYPE LIFNR,                        " Vendor Account Number
      BUKRS      TYPE BUKRS,                        " Company Code
      BELNR      TYPE BELNR_D,                      " Document Number
      GJAHR      TYPE GJAHR,                        " Fiscal Year
      HKONT      TYPE HKONT,                        " General Ledger Account
      DMBTR_AG   TYPE /ABC4C/E_SW_DMBTR_AG,         " Amount in local currency (Aggregated)
      WRBTR_AG   TYPE /ABC4C/E_SW_WRBTR_AG,         " Amount in Document Currency (Aggregated)
      DMBE2_AG   TYPE /ABC4C/E_SW_DMBE2_AG,         " Amount in Local Currency 2 (Aggregated)
      DMBE3_AG   TYPE /ABC4C/E_SW_DMBE3_AG,         " Amount in Local Currency 3 (Aggregated)
    END   OF TYP_SUM.
* ----------------------------------------------------------------------
* Local Data definition
* ----------------------------------------------------------------------
* - single DATA
  DATA_SINGLE:
    LANGU                LANGU,                    " Language (not in use)
    DATUM                SY-DATUM,                 " System Date
    DATE_FROM            /ABC4C/E_SW_FROM_DATE_EX, " Date From
    DATE_TO              /ABC4C/E_SW_TO_DATE_EX,   " Date To
    RETURN               SY-SUBRC,                 " return code
    TABIX                SY-TABIX,                 " table index
    REF_DATE             SY-DATUM,                 " Reference Date
    TIME_DIFF            INT4
    .
* - range DATA
  DATA_MULTY:
    DATUM                DATUM,                    " Date
    BUDAT                BUDAT,                    " Posting Date
    AEDAT                AEDAT,                    " Date on Which Record Was Created
    CPUDT                CPUDT,                    " Day On Which Accounting Document Was Entered
    UPDDT                UPDDT,                    " Date of the Last Document Update
    BLDAT                BLDAT                     " Document Date
    .
  DATA:
    LT_LIFNR             TYPE TABLE OF /ABC4C/S_SW_10_07_FI_EX_PST_V,
    LS_LIFNR             TYPE /ABC4C/S_SW_10_07_FI_EX_PST_V,
    LT_BUKRS             TYPE TABLE OF /ABC4C/S_SW_10_07_FI_EX_PST_V,
    LS_BUKRS             TYPE /ABC4C/S_SW_10_07_FI_EX_PST_V,
    LT_GJAHR             TYPE TABLE OF GJAHR,
    LV_GJAHR             TYPE GJAHR,
    LT_BSXK              TYPE TABLE OF /ABC4C/S_SW_10_07_FI_EX_PST_V,
    LS_BSXK              TYPE /ABC4C/S_SW_10_07_FI_EX_PST_V,
    LT_BSEG              TYPE TABLE OF /ABC4C/S_SW_10_07_FI_EX_PST_V,
    LS_BSEG              TYPE /ABC4C/S_SW_10_07_FI_EX_PST_V,
    LT_OPTION            TYPE TABLE OF RFC_DB_OPT,
    LS_OPTION            LIKE LINE OF LT_OPTION[],
    LT_DATA_RFC          TYPE TABLE OF /SKN/S_SW_TAB2000,
    LT_TABLES_LIST       TYPE /SKN/TT_TABLES,
    LWA_TABLES_LIST      LIKE LINE OF LT_TABLES_LIST[],
    LT_JOIN_CONDITION    TYPE /SKN/TT_TABLE_JOIN,
    LWA_JOIN_CONDITION   LIKE LINE OF LT_JOIN_CONDITION[],
    LS_SEL_FIELDS        TYPE /SKN/S_SEL_FIELDS,
    LT_SEL_FIELDS        TYPE /SKN/TT_SEL_FIELDS,
    LT_OUTPUT_FIELDS     TYPE /SKN/TT_RFC_DB_FLD_EXTEND,
    LT_DFIES             TYPE TABLE OF  DFIES,
    LT_RETURN            TYPE BAPIRET2_T,
    LS_SORT_OPTIONS      TYPE /SKN/S_SW_RFC_JOIN_DB_SORT,
    LT_SORT_OPTIONS      TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT,
    LT_IN_RANGE          TYPE TABLE OF /SKN/S_SW_RANGE_TAB,
    LT_OUT_WHERE_COND    TYPE TABLE OF /SKN/S_SW_WHERE_TAB,
    LWA_IN_RANGE         LIKE LINE OF  LT_IN_RANGE,
    LWA_OUT_WHERE_COND   LIKE LINE OF LT_OUT_WHERE_COND,
    LWA_ALL_ENTRIES_TAB  TYPE /SKN/S_SW_TAB6000,
    LT_ALL_ENTRIES_TAB   TYPE TABLE OF /SKN/S_SW_TAB6000,
    LWA_ALL_ENTRIES_COND TYPE /SKN/S_TABLE_JOIN,
    LT_ALL_ENTRIES_COND  TYPE TABLE OF /SKN/S_TABLE_JOIN,
    LT_ALL_ENTRIES_DFIES TYPE TABLE OF DFIES,
    LT_SUM               TYPE TABLE OF TYP_SUM,
    LS_SUM               TYPE TYP_SUM,
    LT_SUM_GL            TYPE TABLE OF TYP_SUM,
    LS_SUM_GL            TYPE TYP_SUM,
    LT_SUM_DOC           TYPE TABLE OF TYP_SUM,
    LS_SUM_DOC           TYPE TYP_SUM,
    LT_SUM_DOC_GL        TYPE TABLE OF TYP_SUM,
    LS_SUM_DOC_GL        TYPE TYP_SUM
    .
  FIELD-SYMBOLS:
                     TYPE ANY,
    <FS_GJAHR>           TYPE GJAHR
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
    AGGLEVEL             /ABC4C/E_SW_AGGLEVEL      " Aggregation Level
    .
* - range DATA
  DATA_MULTY:
    KTOKK                KTOKK,                    " Vendor Account Group
    LIFNR                LIFNR,                    " Vendor Account Number
    BUKRS                BUKRS,                    " Company Code
    GJAHR                GJAHR,                    " Fiscal Year
    BELNR                BELNR_D,                  " Document Number
    TCODE                TCODE,                    " Transaction Code
    BLART                BLART,                    " Document type
    BKTXT                BKTXT,                    " Document Header Text
    DMBTR                DMBTR,                    " Amount in local currency
    WRBTR                WRBTR,                    " Amount in Document Currency
    DMBE2                DMBE2,                    " Amount in Local Currency 2
    DMBE3                DMBE3,                    " Amount in Local Currency 3
    HKONT                HKONT,                    " General Ledger Account
    KOART                KOART,                    " Account Type
    SHKZG                SHKZG,                    " Debit/Credit Indicator
    SGTXT                SGTXT,                    " Item Text
    BUZEI                BUZEI,                    " Line item
    WAERS                WAERS,                    " Currency Key
    HWAER                HWAER,                    " Local Currency
    HWAE2                HWAE2,                    " Second Local Currency
    HWAE3                HWAE3,                    " Third Local Currency
    KURSF                KURSF,                    " Exchange rate
    KURS2                KURS2,                    " Exchange rate 2
    KURS3                KURS3,                    " Exchange rate 3
    KZWRS                KZWRS,                    " Group currency
    KZKRS                KZKRS,                    " Exchange rate in Grp. Currency
    XBLNR                XBLNR,                    " Reference Document Number
    MONAT                MONAT,                    " Fiscal Period
    BSTAT                BSTAT_D,                  " Document Status
    GRPID                GRPID                     " Batch Input Session Name
    .
* ----------------------------------------------------------------------
* Extracting parameters’ value and populating variables
* ----------------------------------------------------------------------
* Set initial value
  LV_LANGU               = 'E'.     " English
  LV_DATE_REF_FLD        = 'BUDAT'. " Document date
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
    AGGLEVEL                        " Aggregation Level
    .
* - range value
  SELECT_MULTY:
    KTOKK,                          " Vendor Account Group
    LIFNR,                          " Vendor Account Number
    BUKRS,                          " Company Code
    GJAHR,                          " Fiscal Year
    BELNR,                          " Document Number
    TCODE,                          " Transaction Code
    BLART,                          " Document type
    BKTXT,                          " Document Header Text
    DMBTR,                          " Amount in local currency
    WRBTR,                          " Amount in Document Currency
    DMBE2,                          " Amount in Local Currency 2
    DMBE3,                          " Amount in Local Currency 3
    HKONT,                          " General Ledger Account
    KOART,                          " Account Type
    SHKZG,                          " Debit/Credit Indicator
    SGTXT,                          " Item Text
    BUZEI,                          " Line item
    WAERS,                          " Currency Key
    HWAER,                          " Local Currency
    HWAE2,                          " Second Local Currency
    HWAE3,                          " Third Local Currency
    KURSF,                          " Exchange rate
    KURS2,                          " Exchange rate 2
    KURS3,                          " Exchange rate 3
    KZWRS,                          " Group currency
    KZKRS,                          " Exchange rate in Grp. Currency
    XBLNR,                          " Reference Document Number
    MONAT,                          " Fiscal Period
    BSTAT,                          " Document Status
    GRPID                           " Batch Input Session Name
    .
* ----------------------------------------------------------------------
* Initiating
* ----------------------------------------------------------------------
  CLEAR:
    IS_ALERT
    .
  REFRESH:
    T_DATA,
    LT_LIFNR,
    LT_BUKRS,
    LT_GJAHR,
    LT_BSXK,
    LT_BSEG
    .
* ----------------------------------------------------------------------
* Retrieving alert data
* ----------------------------------------------------------------------
* According to System date and BACKDAYS parameter, calculate the target period.
  LV_DATUM     = SY-DATUM.               " System date
  LV_DATE_FROM = LV_DATUM - LV_BACKDAYS. " From date
  LV_DATE_TO   = LV_DATUM.               " To date
* Initial defined date
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'BT' .
    RS_DATUM-LOW    = LV_DATE_FROM .
    RS_DATUM-HIGH   = LV_DATE_TO .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
* Initial defined Account Type
  IF R_KOART[] IS INITIAL.
    RS_KOART-SIGN          = 'I'.
    RS_KOART-OPTION        = 'NE'.
    RS_KOART-LOW           = 'K'.
    APPEND RS_KOART TO R_KOART.
  ENDIF.
  "--- Run Cloud Mode -----
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/ABC4C/FC_SW_10_07_FI_EX_PST_V'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
* Determination of the date type from DATE_REF_FLD
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
      R_BUDAT[] = R_DATUM[]. " Posting date
  ENDCASE.
* Extract vendor master records from Vendor Master (LFA1/LFB1).
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_LIFNR
    FROM LFA1 AS A
    INNER JOIN LFB1 AS B ON  A~LIFNR EQ B~LIFNR
    WHERE A~LIFNR IN R_LIFNR
    AND   A~KTOKK IN R_KTOKK
    AND   B~BUKRS IN R_BUKRS.
*<<< If there is no Vendor, the processing is terminated
  CHECK LT_LIFNR[] IS NOT INITIAL.
  SORT LT_LIFNR BY BUKRS LIFNR.
* Get fiscal year range.
  APPEND LINES OF LT_LIFNR TO LT_BUKRS.
  SORT LT_BUKRS BY BUKRS.
  DELETE ADJACENT DUPLICATES FROM LT_BUKRS COMPARING BUKRS.
  LOOP AT LT_BUKRS INTO LS_BUKRS.
*   Get fiscal year: From Date
    CLEAR LV_GJAHR.
    CALL FUNCTION '/ABC4C/F_SW_10_GET_CUR_YEAR'
      EXPORTING
        BUKRS             = LS_BUKRS-BUKRS
        DATUM             = LV_DATE_FROM
      IMPORTING
*       MONAT             =
        GJAHR             = LV_GJAHR
      EXCEPTIONS
        WRONG_CODE        = 1
        WRONG_VALUE       = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0 OR LV_GJAHR IS INITIAL.
      CONTINUE.
    ELSE.
      APPEND LV_GJAHR TO LT_GJAHR.
    ENDIF.
*   Get fiscal year: To Date
    CLEAR LV_GJAHR.
    CALL FUNCTION '/ABC4C/F_SW_10_GET_CUR_YEAR'
      EXPORTING
        BUKRS             = LS_BUKRS-BUKRS
        DATUM             = LV_DATE_TO
      IMPORTING
*       MONAT             =
        GJAHR             = LV_GJAHR
      EXCEPTIONS
        WRONG_CODE        = 1
        WRONG_VALUE       = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0 OR LV_GJAHR IS INITIAL.
      CONTINUE.
    ELSE.
      APPEND LV_GJAHR TO LT_GJAHR.
    ENDIF.
*   Set fiscal year range.
    AT LAST.
      SORT LT_GJAHR BY TABLE_LINE.
      DELETE ADJACENT DUPLICATES FROM LT_GJAHR COMPARING TABLE_LINE.
      LOOP AT LT_GJAHR ASSIGNING <FS_GJAHR>.
        AT FIRST.
          RS_GJAHR-SIGN   = 'I' .
          RS_GJAHR-OPTION = 'BT' .
          RS_GJAHR-LOW    = <FS_GJAHR>.
        ENDAT.
        AT LAST.
          RS_GJAHR-HIGH   = <FS_GJAHR>.
          APPEND RS_GJAHR TO R_GJAHR.
        ENDAT.
      ENDLOOP.
    ENDAT.
  ENDLOOP.
  REFRESH:
    LT_BUKRS[],
    LT_GJAHR[].
* Extract vendor related records from Accounting Document Item (BSIK).
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_BSXK
    FROM BKPF AS A
    INNER JOIN BSIK AS B ON  A~BUKRS EQ B~BUKRS
                         AND A~BELNR EQ B~BELNR
                         AND A~GJAHR EQ B~GJAHR
    FOR ALL ENTRIES IN LT_LIFNR
    WHERE A~BUKRS =  LT_LIFNR-BUKRS
    AND   B~LIFNR =  LT_LIFNR-LIFNR
    AND   A~BELNR IN R_BELNR
    AND   A~GJAHR IN R_GJAHR
    AND   A~BLART IN R_BLART
    AND   A~BKTXT IN R_BKTXT
    AND   A~TCODE IN R_TCODE
    AND   A~WAERS IN R_WAERS
    AND   A~HWAER IN R_HWAER
    AND   A~HWAE2 IN R_HWAE2
    AND   A~HWAE3 IN R_HWAE3
    AND   A~KURSF IN R_KURSF
    AND   A~KURS2 IN R_KURS2
    AND   A~KURS3 IN R_KURS3
    AND   A~KZWRS IN R_KZWRS
    AND   A~KZKRS IN R_KZKRS
    AND   A~BUDAT IN R_BUDAT
    AND   A~AEDAT IN R_AEDAT
    AND   A~CPUDT IN R_CPUDT
    AND   A~UPDDT IN R_UPDDT
    AND   A~BLDAT IN R_BLDAT
    AND   A~XBLNR IN R_XBLNR
    AND   A~MONAT IN R_MONAT
    AND   A~BSTAT IN R_BSTAT
    AND   A~GRPID IN R_GRPID.
* Extract vendor related records from Accounting Document Item (BSAK).
  SELECT *
    APPENDING CORRESPONDING FIELDS OF TABLE LT_BSXK
    FROM BKPF AS A
    INNER JOIN BSAK AS B ON  A~BUKRS EQ B~BUKRS
                         AND A~BELNR EQ B~BELNR
                         AND A~GJAHR EQ B~GJAHR
    FOR ALL ENTRIES IN LT_LIFNR
    WHERE A~BUKRS =  LT_LIFNR-BUKRS
    AND   B~LIFNR =  LT_LIFNR-LIFNR
    AND   A~BELNR IN R_BELNR
    AND   A~GJAHR IN R_GJAHR
    AND   A~BLART IN R_BLART
    AND   A~BKTXT IN R_BKTXT
    AND   A~TCODE IN R_TCODE
    AND   A~WAERS IN R_WAERS
    AND   A~HWAER IN R_HWAER
    AND   A~HWAE2 IN R_HWAE2
    AND   A~HWAE3 IN R_HWAE3
    AND   A~KURSF IN R_KURSF
    AND   A~KURS2 IN R_KURS2
    AND   A~KURS3 IN R_KURS3
    AND   A~KZWRS IN R_KZWRS
    AND   A~KZKRS IN R_KZKRS
    AND   A~BUDAT IN R_BUDAT
    AND   A~AEDAT IN R_AEDAT
    AND   A~CPUDT IN R_CPUDT
    AND   A~UPDDT IN R_UPDDT
    AND   A~BLDAT IN R_BLDAT
    AND   A~XBLNR IN R_XBLNR
    AND   A~MONAT IN R_MONAT
    AND   A~BSTAT IN R_BSTAT
    AND   A~GRPID IN R_GRPID.
*<<< If there is no Vendor related records, the processing is terminated
  CHECK LT_BSXK[] IS NOT INITIAL.
  SORT LT_BSXK BY BUKRS BELNR GJAHR LIFNR.
  DELETE ADJACENT DUPLICATES FROM LT_BSXK COMPARING BUKRS BELNR GJAHR.
* Extract G/L Accounts records from Accounting Document Item (BSEG) according to vendor related records.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_BSEG
    FROM BSEG AS A
    FOR ALL ENTRIES IN LT_BSXK
    WHERE A~BUKRS =  LT_BSXK-BUKRS
    AND   A~BELNR =  LT_BSXK-BELNR
    AND   A~GJAHR =  LT_BSXK-GJAHR
    AND   A~KOART IN R_KOART
    AND   A~HKONT IN R_HKONT
    AND   A~SGTXT IN R_SGTXT
    AND   A~SHKZG IN R_SHKZG
    AND   A~BUZEI IN R_BUZEI.
*<<< If there is no G/L Account records, the processing is terminated
  CHECK LT_BSEG[] IS NOT INITIAL.
* ----------------------------------------------------------------------
* Post retrieving manipulations
* ----------------------------------------------------------------------
  LOOP AT LT_BSEG INTO LS_BSEG.
    LV_TABIX = SY-TABIX .
*   Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
    CONCATENATE 'LT_BSEG-' LV_DATE_REF_FLD INTO LV_DATE_REF_FLD.
    ASSIGN (LV_DATE_REF_FLD) TO .
    CHECK  IS ASSIGNED.
    LV_REF_DATE =  .
    IF NOT LV_REF_DATE IS INITIAL.
      LS_BSEG-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = LV_REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF   = LV_TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF LV_TIME_DIFF < '999999'.
          LS_BSEG-DURATION  = LV_TIME_DIFF .
        ELSE.
          LS_BSEG-DURATION  = '999999'.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY LT_BSEG FROM LS_BSEG INDEX LV_TABIX.
  ENDLOOP.
  DELETE LT_BSEG WHERE DURATION NOT IN R_DURATION .
* Aggregate amount according to Aggregation Level.
* The amount will be aggregated as positive or negative by Debit/Credit Indicator(SHKZG).
  SORT LT_BSEG BY BUKRS BELNR GJAHR HKONT.
  REFRESH:
    LT_SUM[],
    LT_SUM_GL[],
    LT_SUM_DOC[],
    LT_SUM_DOC_GL[].
  LOOP AT LT_BSXK INTO LS_BSXK.
    CLEAR:
      LS_SUM,
      LS_SUM_GL,
      LS_SUM_DOC,
      LS_SUM_DOC_GL.
    LOOP AT LT_BSEG INTO LS_BSEG WHERE BUKRS = LS_BSXK-BUKRS
                                   AND BELNR = LS_BSXK-BELNR
                                   AND GJAHR = LS_BSXK-GJAHR.
*     Aggregate by the specified G/L Account within the Document.
*     - AGGLEVEL: DOC-GL
      IF LS_BSEG-SHKZG = 'S'.
        LS_SUM_DOC_GL-DMBTR_AG = LS_SUM_DOC_GL-DMBTR_AG + LS_BSEG-DMBTR.
        LS_SUM_DOC_GL-WRBTR_AG = LS_SUM_DOC_GL-WRBTR_AG + LS_BSEG-WRBTR.
        LS_SUM_DOC_GL-DMBE2_AG = LS_SUM_DOC_GL-DMBE2_AG + LS_BSEG-DMBE2.
        LS_SUM_DOC_GL-DMBE3_AG = LS_SUM_DOC_GL-DMBE3_AG + LS_BSEG-DMBE3.
      ELSE.
        LS_SUM_DOC_GL-DMBTR_AG = LS_SUM_DOC_GL-DMBTR_AG - LS_BSEG-DMBTR.
        LS_SUM_DOC_GL-WRBTR_AG = LS_SUM_DOC_GL-WRBTR_AG - LS_BSEG-WRBTR.
        LS_SUM_DOC_GL-DMBE2_AG = LS_SUM_DOC_GL-DMBE2_AG - LS_BSEG-DMBE2.
        LS_SUM_DOC_GL-DMBE3_AG = LS_SUM_DOC_GL-DMBE3_AG - LS_BSEG-DMBE3.
      ENDIF.
      AT END OF HKONT.
*       Keep the aggregated record.
*       - AGGLEVEL: DOC-GL
        CONCATENATE LS_BSXK-LIFNR '/' LS_BSEG-BELNR '/' LS_BSEG-HKONT
               INTO LS_SUM_DOC_GL-AGGKEY.
        LS_SUM_DOC_GL-LIFNR    = LS_BSXK-LIFNR.
        LS_SUM_DOC_GL-BUKRS    = LS_BSXK-BUKRS.
        LS_SUM_DOC_GL-BELNR    = LS_BSEG-BELNR.
        LS_SUM_DOC_GL-GJAHR    = LS_BSEG-GJAHR.
        LS_SUM_DOC_GL-HKONT    = LS_BSEG-HKONT.
        IF LV_AGGLEVEL = 'DOC-GL'.
          APPEND LS_SUM_DOC_GL TO LT_SUM_DOC_GL.
        ENDIF.
*       Aggregate and Keep the aggregated record.
*       - AGGLEVEL: GL
        CONCATENATE LS_BSXK-LIFNR '/' LS_BSEG-HKONT
               INTO LS_SUM_GL-AGGKEY.
        LS_SUM_GL-LIFNR        = LS_BSXK-LIFNR.
        LS_SUM_GL-HKONT        = LS_BSEG-HKONT.
        LS_SUM_GL-DMBTR_AG     = LS_SUM_DOC_GL-DMBTR_AG.
        LS_SUM_GL-WRBTR_AG     = LS_SUM_DOC_GL-WRBTR_AG.
        LS_SUM_GL-DMBE2_AG     = LS_SUM_DOC_GL-DMBE2_AG.
        LS_SUM_GL-DMBE3_AG     = LS_SUM_DOC_GL-DMBE3_AG.
        IF LV_AGGLEVEL = 'GL'.
          COLLECT LS_SUM_GL INTO LT_SUM_GL.
        ENDIF.
*       Aggregate within the Document.
*       - AGGLEVEL: DOC
        LS_SUM_DOC-DMBTR_AG    = LS_SUM_DOC-DMBTR_AG + LS_SUM_DOC_GL-DMBTR_AG.
        LS_SUM_DOC-WRBTR_AG    = LS_SUM_DOC-WRBTR_AG + LS_SUM_DOC_GL-WRBTR_AG.
        LS_SUM_DOC-DMBE2_AG    = LS_SUM_DOC-DMBE2_AG + LS_SUM_DOC_GL-DMBE2_AG.
        LS_SUM_DOC-DMBE3_AG    = LS_SUM_DOC-DMBE3_AG + LS_SUM_DOC_GL-DMBE3_AG.
        CLEAR: LS_SUM_GL, LS_SUM_DOC_GL.
      ENDAT.
    ENDLOOP.
*   Keep the aggregated record.
*   - AGGLEVEL: DOC
    CONCATENATE LS_BSXK-LIFNR '/' LS_BSXK-BELNR
           INTO LS_SUM_DOC-AGGKEY.
    LS_SUM_DOC-LIFNR       = LS_BSXK-LIFNR.
    LS_SUM_DOC-BUKRS       = LS_BSXK-BUKRS.
    LS_SUM_DOC-BELNR       = LS_BSXK-BELNR.
    LS_SUM_DOC-GJAHR       = LS_BSXK-GJAHR.
    IF LV_AGGLEVEL = 'DOC'.
      APPEND LS_SUM_DOC TO LT_SUM_DOC.
    ENDIF.
*   Aggregate and Keep the aggregated record.
*   - AGGLEVEL: 
    LS_SUM-AGGKEY          = LS_BSXK-LIFNR.
    LS_SUM-LIFNR           = LS_BSXK-LIFNR.
    LS_SUM-DMBTR_AG        = LS_SUM_DOC-DMBTR_AG.
    LS_SUM-WRBTR_AG        = LS_SUM_DOC-WRBTR_AG.
    LS_SUM-DMBE2_AG        = LS_SUM_DOC-DMBE2_AG.
    LS_SUM-DMBE3_AG        = LS_SUM_DOC-DMBE3_AG.
    IF LV_AGGLEVEL <> 'DOC-GL' AND LV_AGGLEVEL <> 'GL' AND LV_AGGLEVEL <> 'DOC'.
      COLLECT LS_SUM INTO LT_SUM.
    ENDIF.
  ENDLOOP.
* From the aggregated data, detect the vendors who posted
* more than threshold amount to specific account
* and populate them as the alert data.
  CASE LV_AGGLEVEL.
    WHEN 'GL'.
      LOOP AT LT_SUM_GL INTO LS_SUM_GL.
        IF    LS_SUM_GL-DMBTR_AG IN R_DMBTR
          AND LS_SUM_GL-WRBTR_AG IN R_WRBTR
          AND LS_SUM_GL-DMBE2_AG IN R_DMBE2
          AND LS_SUM_GL-DMBE3_AG IN R_DMBE3.
          LOOP AT LT_BSXK INTO LS_BSXK WHERE LIFNR = LS_SUM_GL-LIFNR.
*           Get Vendor Account Group.
            READ TABLE LT_LIFNR INTO LS_LIFNR BINARY SEARCH
              WITH KEY BUKRS = LS_BSXK-BUKRS LIFNR = LS_BSXK-LIFNR.
*           Populate the alart data.
            LOOP AT LT_BSEG INTO LS_BSEG WHERE BUKRS = LS_BSXK-BUKRS
                                           AND GJAHR = LS_BSXK-GJAHR
                                           AND BELNR = LS_BSXK-BELNR
                                           AND HKONT = LS_SUM_GL-HKONT.
              LV_TABIX = SY-TABIX.
              LS_BSEG-AGGLEVEL = LV_AGGLEVEL.
              LS_BSEG-AGGKEY   = LS_SUM_GL-AGGKEY.
              LS_BSEG-FROMDATE = LV_DATE_FROM.
              LS_BSEG-TODATE   = LV_DATE_TO.
              LS_BSEG-KTOKK    = LS_LIFNR-KTOKK.
              LS_BSEG-LIFNR    = LS_LIFNR-LIFNR.
              LS_BSEG-DMBTR_AG = LS_SUM_GL-DMBTR_AG.
              LS_BSEG-WRBTR_AG = LS_SUM_GL-WRBTR_AG.
              LS_BSEG-DMBE2_AG = LS_SUM_GL-DMBE2_AG.
              LS_BSEG-DMBE3_AG = LS_SUM_GL-DMBE3_AG.
*             Set BKPF fields.
              LS_BSEG-TCODE    = LS_BSXK-TCODE.
              LS_BSEG-BUDAT    = LS_BSXK-BUDAT.
              LS_BSEG-BLDAT    = LS_BSXK-BLDAT.
              LS_BSEG-BLART    = LS_BSXK-BLART.
              LS_BSEG-BKTXT    = LS_BSXK-BKTXT.
              LS_BSEG-WAERS    = LS_BSXK-WAERS.
              LS_BSEG-HWAER    = LS_BSXK-HWAER.
              LS_BSEG-HWAE2    = LS_BSXK-HWAE2.
              LS_BSEG-HWAE3    = LS_BSXK-HWAE3.
              LS_BSEG-KURSF    = LS_BSXK-KURSF.
              LS_BSEG-KURS2    = LS_BSXK-KURS2.
              LS_BSEG-KURS3    = LS_BSXK-KURS3.
              LS_BSEG-KZWRS    = LS_BSXK-KZWRS.
              LS_BSEG-KZKRS    = LS_BSXK-KZKRS.
              LS_BSEG-CPUDT    = LS_BSXK-CPUDT.
              LS_BSEG-AEDAT    = LS_BSXK-AEDAT.
              LS_BSEG-UPDDT    = LS_BSXK-UPDDT.
              LS_BSEG-XBLNR    = LS_BSXK-XBLNR.
              LS_BSEG-MONAT    = LS_BSXK-MONAT.
              LS_BSEG-BSTAT    = LS_BSXK-BSTAT.
              LS_BSEG-GRPID    = LS_BSXK-GRPID.
              APPEND LS_BSEG TO T_DATA.
              DELETE LT_BSEG INDEX LV_TABIX.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    WHEN 'DOC'.
      LOOP AT LT_SUM_DOC INTO LS_SUM_DOC.
        IF    LS_SUM_DOC-DMBTR_AG IN R_DMBTR
          AND LS_SUM_DOC-WRBTR_AG IN R_WRBTR
          AND LS_SUM_DOC-DMBE2_AG IN R_DMBE2
          AND LS_SUM_DOC-DMBE3_AG IN R_DMBE3.
*         Get Vendor Account Group.
          READ TABLE LT_LIFNR INTO LS_LIFNR BINARY SEARCH
            WITH KEY BUKRS = LS_SUM_DOC-BUKRS LIFNR = LS_SUM_DOC-LIFNR.
*         Get Vendor Item.
          READ TABLE LT_BSXK INTO LS_BSXK BINARY SEARCH
            WITH KEY BUKRS = LS_SUM_DOC-BUKRS GJAHR = LS_SUM_DOC-GJAHR BELNR = LS_SUM_DOC-BELNR.
*         Populate the alart data.
          LOOP AT LT_BSEG INTO LS_BSEG WHERE BUKRS = LS_SUM_DOC-BUKRS
                                         AND GJAHR = LS_SUM_DOC-GJAHR
                                         AND BELNR = LS_SUM_DOC-BELNR.
            LV_TABIX = SY-TABIX.
            LS_BSEG-AGGLEVEL = LV_AGGLEVEL.
            LS_BSEG-AGGKEY   = LS_SUM_DOC-AGGKEY.
            LS_BSEG-FROMDATE = LV_DATE_FROM.
            LS_BSEG-TODATE   = LV_DATE_TO.
            LS_BSEG-KTOKK    = LS_LIFNR-KTOKK.
            LS_BSEG-LIFNR    = LS_LIFNR-LIFNR.
            LS_BSEG-DMBTR_AG = LS_SUM_DOC-DMBTR_AG.
            LS_BSEG-WRBTR_AG = LS_SUM_DOC-WRBTR_AG.
            LS_BSEG-DMBE2_AG = LS_SUM_DOC-DMBE2_AG.
            LS_BSEG-DMBE3_AG = LS_SUM_DOC-DMBE3_AG.
*           Set BKPF fields.
            LS_BSEG-TCODE    = LS_BSXK-TCODE.
            LS_BSEG-BUDAT    = LS_BSXK-BUDAT.
            LS_BSEG-BLDAT    = LS_BSXK-BLDAT.
            LS_BSEG-BLART    = LS_BSXK-BLART.
            LS_BSEG-BKTXT    = LS_BSXK-BKTXT.
            LS_BSEG-WAERS    = LS_BSXK-WAERS.
            LS_BSEG-HWAER    = LS_BSXK-HWAER.
            LS_BSEG-HWAE2    = LS_BSXK-HWAE2.
            LS_BSEG-HWAE3    = LS_BSXK-HWAE3.
            LS_BSEG-KURSF    = LS_BSXK-KURSF.
            LS_BSEG-KURS2    = LS_BSXK-KURS2.
            LS_BSEG-KURS3    = LS_BSXK-KURS3.
            LS_BSEG-KZWRS    = LS_BSXK-KZWRS.
            LS_BSEG-KZKRS    = LS_BSXK-KZKRS.
            LS_BSEG-CPUDT    = LS_BSXK-CPUDT.
            LS_BSEG-AEDAT    = LS_BSXK-AEDAT.
            LS_BSEG-UPDDT    = LS_BSXK-UPDDT.
            LS_BSEG-XBLNR    = LS_BSXK-XBLNR.
            LS_BSEG-MONAT    = LS_BSXK-MONAT.
            LS_BSEG-BSTAT    = LS_BSXK-BSTAT.
            LS_BSEG-GRPID    = LS_BSXK-GRPID.
            APPEND LS_BSEG TO T_DATA.
            DELETE LT_BSEG INDEX LV_TABIX.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    WHEN 'DOC-GL'.
      LOOP AT LT_SUM_DOC_GL INTO LS_SUM_DOC_GL.
        IF    LS_SUM_DOC_GL-DMBTR_AG IN R_DMBTR
          AND LS_SUM_DOC_GL-WRBTR_AG IN R_WRBTR
          AND LS_SUM_DOC_GL-DMBE2_AG IN R_DMBE2
          AND LS_SUM_DOC_GL-DMBE3_AG IN R_DMBE3.
*         Get Vendor Account Group.
          READ TABLE LT_LIFNR INTO LS_LIFNR BINARY SEARCH
            WITH KEY BUKRS = LS_SUM_DOC_GL-BUKRS LIFNR = LS_SUM_DOC_GL-LIFNR.
*         Get Vendor Item.
          READ TABLE LT_BSXK INTO LS_BSXK BINARY SEARCH
            WITH KEY BUKRS = LS_SUM_DOC_GL-BUKRS GJAHR = LS_SUM_DOC_GL-GJAHR BELNR = LS_SUM_DOC_GL-BELNR.
*         Populate the alart data.
          LOOP AT LT_BSEG INTO LS_BSEG WHERE BUKRS = LS_SUM_DOC_GL-BUKRS
                                         AND GJAHR = LS_SUM_DOC_GL-GJAHR
                                         AND BELNR = LS_SUM_DOC_GL-BELNR
                                         AND HKONT = LS_SUM_DOC_GL-HKONT.
            LV_TABIX = SY-TABIX.
            LS_BSEG-AGGLEVEL = LV_AGGLEVEL.
            LS_BSEG-AGGKEY   = LS_SUM_DOC_GL-AGGKEY.
            LS_BSEG-FROMDATE = LV_DATE_FROM.
            LS_BSEG-TODATE   = LV_DATE_TO.
            LS_BSEG-KTOKK    = LS_LIFNR-KTOKK.
            LS_BSEG-LIFNR    = LS_LIFNR-LIFNR.
            LS_BSEG-DMBTR_AG = LS_SUM_DOC_GL-DMBTR_AG.
            LS_BSEG-WRBTR_AG = LS_SUM_DOC_GL-WRBTR_AG.
            LS_BSEG-DMBE2_AG = LS_SUM_DOC_GL-DMBE2_AG.
            LS_BSEG-DMBE3_AG = LS_SUM_DOC_GL-DMBE3_AG.
*           Set BKPF fields.
            LS_BSEG-TCODE    = LS_BSXK-TCODE.
            LS_BSEG-BUDAT    = LS_BSXK-BUDAT.
            LS_BSEG-BLDAT    = LS_BSXK-BLDAT.
            LS_BSEG-BLART    = LS_BSXK-BLART.
            LS_BSEG-BKTXT    = LS_BSXK-BKTXT.
            LS_BSEG-WAERS    = LS_BSXK-WAERS.
            LS_BSEG-HWAER    = LS_BSXK-HWAER.
            LS_BSEG-HWAE2    = LS_BSXK-HWAE2.
            LS_BSEG-HWAE3    = LS_BSXK-HWAE3.
            LS_BSEG-KURSF    = LS_BSXK-KURSF.
            LS_BSEG-KURS2    = LS_BSXK-KURS2.
            LS_BSEG-KURS3    = LS_BSXK-KURS3.
            LS_BSEG-KZWRS    = LS_BSXK-KZWRS.
            LS_BSEG-KZKRS    = LS_BSXK-KZKRS.
            LS_BSEG-CPUDT    = LS_BSXK-CPUDT.
            LS_BSEG-AEDAT    = LS_BSXK-AEDAT.
            LS_BSEG-UPDDT    = LS_BSXK-UPDDT.
            LS_BSEG-XBLNR    = LS_BSXK-XBLNR.
            LS_BSEG-MONAT    = LS_BSXK-MONAT.
            LS_BSEG-BSTAT    = LS_BSXK-BSTAT.
            LS_BSEG-GRPID    = LS_BSXK-GRPID.
            APPEND LS_BSEG TO T_DATA.
            DELETE LT_BSEG INDEX LV_TABIX.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
      LOOP AT LT_SUM INTO LS_SUM.
        IF    LS_SUM-DMBTR_AG IN R_DMBTR
          AND LS_SUM-WRBTR_AG IN R_WRBTR
          AND LS_SUM-DMBE2_AG IN R_DMBE2
          AND LS_SUM-DMBE3_AG IN R_DMBE3.
          LOOP AT LT_BSXK INTO LS_BSXK WHERE LIFNR = LS_SUM-LIFNR.
*           Get Vendor Account Group.
            READ TABLE LT_LIFNR INTO LS_LIFNR BINARY SEARCH
              WITH KEY BUKRS = LS_BSXK-BUKRS LIFNR = LS_BSXK-LIFNR.
*           Populate the alart data.
            LOOP AT LT_BSEG INTO LS_BSEG WHERE BUKRS = LS_BSXK-BUKRS
                                           AND GJAHR = LS_BSXK-GJAHR
                                           AND BELNR = LS_BSXK-BELNR.
              LV_TABIX = SY-TABIX.
              LS_BSEG-AGGLEVEL = LV_AGGLEVEL.
              LS_BSEG-AGGKEY   = LS_SUM-AGGKEY.
              LS_BSEG-FROMDATE = LV_DATE_FROM.
              LS_BSEG-TODATE   = LV_DATE_TO.
              LS_BSEG-KTOKK    = LS_LIFNR-KTOKK.
              LS_BSEG-LIFNR    = LS_LIFNR-LIFNR.
              LS_BSEG-DMBTR_AG = LS_SUM-DMBTR_AG.
              LS_BSEG-WRBTR_AG = LS_SUM-WRBTR_AG.
              LS_BSEG-DMBE2_AG = LS_SUM-DMBE2_AG.
              LS_BSEG-DMBE3_AG = LS_SUM-DMBE3_AG.
*             Set BKPF fields.
              LS_BSEG-TCODE    = LS_BSXK-TCODE.
              LS_BSEG-BUDAT    = LS_BSXK-BUDAT.
              LS_BSEG-BLDAT    = LS_BSXK-BLDAT.
              LS_BSEG-BLART    = LS_BSXK-BLART.
              LS_BSEG-BKTXT    = LS_BSXK-BKTXT.
              LS_BSEG-WAERS    = LS_BSXK-WAERS.
              LS_BSEG-HWAER    = LS_BSXK-HWAER.
              LS_BSEG-HWAE2    = LS_BSXK-HWAE2.
              LS_BSEG-HWAE3    = LS_BSXK-HWAE3.
              LS_BSEG-KURSF    = LS_BSXK-KURSF.
              LS_BSEG-KURS2    = LS_BSXK-KURS2.
              LS_BSEG-KURS3    = LS_BSXK-KURS3.
              LS_BSEG-KZWRS    = LS_BSXK-KZWRS.
              LS_BSEG-KZKRS    = LS_BSXK-KZKRS.
              LS_BSEG-CPUDT    = LS_BSXK-CPUDT.
              LS_BSEG-AEDAT    = LS_BSXK-AEDAT.
              LS_BSEG-UPDDT    = LS_BSXK-UPDDT.
              LS_BSEG-XBLNR    = LS_BSXK-XBLNR.
              LS_BSEG-MONAT    = LS_BSXK-MONAT.
              LS_BSEG-BSTAT    = LS_BSXK-BSTAT.
              LS_BSEG-GRPID    = LS_BSXK-GRPID.
              APPEND LS_BSEG TO T_DATA.
              DELETE LT_BSEG INDEX LV_TABIX.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
  ENDCASE.
  REFRESH:
    LT_LIFNR[].
* Get descriptions
  LOOP AT T_DATA INTO LS_BSEG.
    LV_TABIX = SY-TABIX.
    CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR              = LS_BSEG-LIFNR
      IMPORTING
        VENDOR_DESC        = LS_BSEG-VENDOR_DESC
      EXCEPTIONS
        WRONG_VENDOR       = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
      EXPORTING
        BUKRS                = LS_BSEG-BUKRS
      IMPORTING
        COMP_CODE_DESC       = LS_BSEG-COMP_CODE_DESC
      EXCEPTIONS
        WRONG_CODE           = 1
        OTHERS               = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION '/SKN/F_SW_10_SAKTO_DESC'
      EXPORTING
        SPRAS            = LV_LANGU
        BUKRS            = LS_BSEG-BUKRS
*       KTOPL            =
        SAKNR            = LS_BSEG-HKONT
      IMPORTING
        ACC_DESC         = LS_BSEG-ACC_DESC
      EXCEPTIONS
        WRONG_CODE       = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    MODIFY T_DATA FROM LS_BSEG INDEX LV_TABIX.
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

