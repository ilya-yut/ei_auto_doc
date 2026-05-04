# Exception Indicator: FI documents posted to previous fiscal period ( SW_10_07_FI_DOC_POSTED)

## General Overview

This Exception Indicator surfaces accounting documents whose posting timing sits in an earlier fiscal period than the business activity dates you treat as current, so finance controllers can see postings that may belong to a closed or prior reporting window. It joins document header and line perspectives, enriches amounts and master descriptions, and applies an optional age test so exception queues stay focused on material items.

This EI serves as an essential control for financial close and operational integrity by:
- Highlighting postings that can distort period comparability when activity dates and fiscal posting periods diverge
- Giving accounts payable and receivable teams a consolidated view of who posted what, in which company code, and with which document types when timing exceptions appear
- Supporting GL and subledger reconciliation by carrying line-level direction, amounts, and account context alongside header identifiers
- Enabling targeted follow-up on clusters tied to specific users, transaction codes, or reference numbers without manual table extracts
- Providing evidence-friendly output for internal control testing around retroactive or late-period postings

Organizations use this style of monitoring during month-end and year-end close, after reopening periods, and when investigating suspected backdating or cut-off errors. Results are intended to feed exception workflows before final sign-off on financial statements.

The routine reads data from standard FI document header and line sources (including secondary index paths when the line table is stored as a cluster structure) together with company-code directory attributes used for currency and chart-of-accounts context.


## Problem Description

Failure to monitor postings that land in prior fiscal periods while business dates suggest current-period activity creates multiple risks across financial reporting, operational control, and audit readiness.

**Financial Reporting and Close Risks**
- Period profit and balance sheet balances can shift without transparent explanation when late or reopened postings are not reviewed in time
- Management reports that rely on posting period slices may misstate trends if timing exceptions accumulate unidentified
- Statutory and management reporting deadlines compress remediation time once exceptions are discovered only during external review
- Cross-company views become inconsistent when some entities correct cut-off issues while others remain unaware of similar patterns

**Operational and Master Data Risks**
- Accounts teams may approve accruals or reversals while unaware that underlying documents still carry prior-period posting dates
- Document type, user, or transaction code concentrations can signal process breakdowns yet stay hidden without automated surfacing
- Line-level debit and credit imbalances or unusual posting keys may indicate training gaps or system integration defects tied to the same timing issue
- Vendor or customer subledger mismatches can linger when clearing documents post outside the expected fiscal window

**Management Visibility and Accountability Risks**
- Executives lose confidence in flash close metrics when unexplained prior-period postings appear late in the cycle
- Internal audit cannot efficiently sample risky populations without a repeatable exception list tied to fiscal period logic
- Escalations between shared service centers and local entities slow when nobody owns a consolidated view of timing outliers

## Suggested Resolution

**Immediate Response**
- Review each surfaced document for company code, fiscal year, document number, and posting date versus the business dates shown in the exception list
- Validate whether the posting was an authorized reopening, a legitimate correction, or an unintended booking using standard FI display transactions your organization permits
- Confirm user and transaction code context with the preparer before reversing or adjusting anything in production
- Capture business commentary where the posting was intentional so close committees can document exceptions

**System Assessment**
- Compare current results with the prior monitoring cycle after period status changes, transports, or automated posting jobs
- Examine concentrations by document type, user, or reference number to see if a single process drives most findings
- Revisit the configured fiscal-period boundary logic relative to your organization’s official close calendar when false positives cluster at month boundaries
- Check whether optional age filters are excluding immaterial noise or, conversely, hiding items that still breach policy thresholds

**Corrective Actions**
- Post corrective or reversal documents through your standard FI change process, with approvals where policy requires them
- Update training, desktop procedures, or scheduling for recurring jobs when root cause is procedural rather than data defect
- Tighten or relax monitoring parameters after root-cause review so the queue remains actionable for controllers and shared services
- Route repeat systemic issues into defect or change management when configuration or integration changes are required
- Retain monitoring extracts and resolution notes when regulators or auditors expect evidence of supervisory review


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Aedat | CHAR | 50 | 0 | AEDAT | AEDAT |
| 2 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 3 | BELNR | Belnr | CHAR | 50 | 0 | BELNR | BELNR |
| 4 | BKTXT | Bktxt | CHAR | 50 | 0 | BKTXT | BKTXT |
| 5 | BLART | Blart | CHAR | 50 | 0 | BLART | BLART |
| 6 | BLDAT | Bldat | CHAR | 50 | 0 | BLDAT | BLDAT |
| 7 | BSCHL | Bschl | CHAR | 50 | 0 | BSCHL | BSCHL |
| 8 | BSTAT | Bstat | CHAR | 50 | 0 | BSTAT | BSTAT |
| 9 | BUDAT | Budat | CHAR | 50 | 0 | BUDAT | BUDAT |
| 10 | BUKRS | Bukrs | CHAR | 50 | 0 | BUKRS | BUKRS |
| 11 | BUZEI | Buzei | CHAR | 50 | 0 | BUZEI | BUZEI |
| 12 | CPUDT | Cpudt | CHAR | 50 | 0 | CPUDT | CPUDT |
| 13 | DATE_REF_FLD | Date Ref Fld | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |
| 14 | DATUM | Datum | CHAR | 50 | 0 | DATUM | DATUM |
| 15 | DMBE2 | Dmbe2 | CHAR | 50 | 0 | DMBE2 | DMBE2 |
| 16 | DMBE3 | Dmbe3 | CHAR | 50 | 0 | DMBE3 | DMBE3 |
| 17 | DMBTR | Dmbtr | CHAR | 50 | 0 | DMBTR | DMBTR |
| 18 | DURATION | Duration | INT4 | 10 | 0 | DURATION | DURATION |
| 19 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | DURATION_UNIT | DURATION_UNIT |
| 20 | FORWDAYS | Forwdays | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |
| 21 | GJAHR | Gjahr | CHAR | 50 | 0 | GJAHR | GJAHR |
| 22 | GRPID | Grpid | CHAR | 50 | 0 | GRPID | GRPID |
| 23 | GVTYP | Gvtyp | CHAR | 50 | 0 | GVTYP | GVTYP |
| 24 | HKONT | Hkont | CHAR | 50 | 0 | HKONT | HKONT |
| 25 | HWAE2 | Hwae2 | CHAR | 50 | 0 | HWAE2 | HWAE2 |
| 26 | HWAE3 | Hwae3 | CHAR | 50 | 0 | HWAE3 | HWAE3 |
| 27 | HWAER | Hwaer | CHAR | 50 | 0 | HWAER | HWAER |
| 28 | KOART | Koart | CHAR | 50 | 0 | KOART | KOART |
| 29 | KTOPL | Ktopl | CHAR | 50 | 0 | KTOPL | KTOPL |
| 30 | KURS2 | Kurs2 | CHAR | 50 | 0 | KURS2 | KURS2 |
| 31 | KURS3 | Kurs3 | CHAR | 50 | 0 | KURS3 | KURS3 |
| 32 | KURSF | Kursf | CHAR | 50 | 0 | KURSF | KURSF |
| 33 | KZBTR | Kzbtr | CHAR | 50 | 0 | KZBTR | KZBTR |
| 34 | LANGU | Langu | CHAR | 1 | 0 | LANGU | LANGU |
| 35 | MONAT | Monat | CHAR | 50 | 0 | MONAT | MONAT |
| 36 | PERIOD_CLOSING_DAY | Period Closing Day | NUMC | 2 | 0 | PERIOD_CLOSING_DAY | PERIOD_CLOSING_DAY |
| 37 | SGTXT | Sgtxt | CHAR | 50 | 0 | SGTXT | SGTXT |
| 38 | SHKZG | Shkzg | CHAR | 1 | 0 | SHKZG | SHKZG |
| 39 | STBLG | Stblg | CHAR | 50 | 0 | STBLG | STBLG |
| 40 | SW_DEST | Sw Dest | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 41 | TCODE | Tcode | CHAR | 50 | 0 | TCODE | TCODE |
| 42 | TIME_REF_FLD | Time Ref Fld | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |
| 43 | UPDDT | Upddt | CHAR | 50 | 0 | UPDDT | UPDDT |
| 44 | USNAM | Usnam | CHAR | 50 | 0 | USNAM | USNAM |
| 45 | WAERS | Waers | CHAR | 50 | 0 | WAERS | WAERS |
| 46 | WAERS_T001 | Waers T001 | CHAR | 50 | 0 | WAERS_T001 | WAERS_T001 |
| 47 | WRBTR | Wrbtr | CHAR | 50 | 0 | WRBTR | WRBTR |
| 48 | XBLNR | Xblnr | CHAR | 50 | 0 | XBLNR | XBLNR |
| 49 | XREVERSAL | Xreversal | CHAR | 50 | 0 | XREVERSAL | XREVERSAL |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 49 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**AEDAT** (Aedat)

Changed-on date used to filter documents or master records by last maintenance activity.


**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.


**BELNR** (Belnr)

Accounting document number, the primary FI document key for journal-level traceability.


**BKTXT** (Bktxt)

Document header text on FI/AA documents carrying user narrative for audit and search filters.


**BLART** (Blart)

FI document type classifying accounting documents such as invoices, payments, or general postings.


**BLDAT** (Bldat)

Document date from the source business document, often used as legal/document reference date.


**BSCHL** (Bschl)

Posting key on the accounting line that controls how amounts post to debits or credits, tax handling, and special posting situations.


**BSTAT** (Bstat)

Overall billing status on SD billing headers summarizing processing state versus cancellation or completion.


**BUDAT** (Budat)

Posting date used to align analysis with accounting period recognition.


**BUKRS** (Bukrs)

Company code key that scopes data to legal entity/accounting unit level.


**BUZEI** (Buzei)

Accounting document line item number uniquely numbering lines within one FI document number.


**CPUDT** (Cpudt)

Entry/creation date used for technical posting timestamp filtering.


**DATE_REF_FLD** (Date Ref Fld)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- CPUDT — System entry date of the document header used when you want monitoring windows aligned to capture time.
- BLDAT — Document date carried on the header for legal or external correspondence timing.
- AEDAT — Last-changed date on the header when maintenance-driven windows matter more than creation.
- UPDDT — Last update date on the header when you need windows keyed to the latest modification cycle.

**DATUM** (Datum)

Explicit calendar bounds for the monitoring pass; when populated, these ranges override the relative lookback built from BACKDAYS.


**DMBE2 - DMBE3** (Dmbe2)

Additional local-currency amount fields on the line used for parallel valuation views; set ranges when you need to narrow lines that carry non-zero values in those valuation buckets.


**DMBTR** (Dmbtr)

Amount in local currency used for FI valuation and threshold checks.


**DURATION** (Duration)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT


**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H — Hours.
- M — Minutes.
- D — Days.
- F — Full-day counting for day-based age thresholds.

**FORWDAYS** (Forwdays)

<mark>FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.</mark>

When supplied together with BACKDAYS, extends the upper calendar bound forward from the evaluation day while still anchoring the lower bound from the backward interval; when BACKDAYS is initial and this value is set, the selection starts forward from the evaluation day instead.


**GJAHR** (Gjahr)

Fiscal year of the accounting document used to pair header and line rows and to scope year-specific reporting.


**GRPID** (Grpid)

<mark>Group id bundling related technical rows such as performance samples or application-log correlation keys.</mark>


**GVTYP** (Gvtyp)

Transaction type on the line that classifies how the line participates in consolidation or tax reporting when you filter special categories.


**HKONT** (Hkont)

General ledger account number on FI line items for account-level selection and financial statement mapping.


**HWAE2 - HWAE3** (Hwae2)

Secondary and tertiary currency keys on the document header used when parallel currency translations are stored; restrict them when monitoring focuses on specific reporting currencies.


**HWAER** (Hwaer)

Local currency key of the company code used to interpret amounts in company-code currency.


**KOART** (Koart)

Account-type selector for cluster-based environments that tells the join whether customer, vendor, or general-ledger secondary index paths should supply line facts.


**KTOPL** (Ktopl)

Chart of accounts governing GL account numbering, groups, and financial statement versions.


**KURS2 - KURS3** (Kurs2)

Secondary and tertiary exchange rates on the header used with the parallel currency fields; narrow them when rate-driven false positives must be suppressed.


**KURSF** (Kursf)

Exchange rate used to convert foreign-currency amounts to local currency on the posting or pricing row.


**KZBTR** (Kzbtr)

Quantity in the posting unit of measure on the line for operational postings that carry physical quantities alongside monetary amounts.


**LANGU** (Langu)

Language key used for language-dependent texts and user-language filtering.


**MONAT** (Monat)

Posting period or calendar month bucket on FI/MM periodic aggregates for period-based reporting.


**PERIOD_CLOSING_DAY** (Period Closing Day)

Calendar day within a month that defines how fiscal periods are split for the document-posting-period helper before header and line selection runs.


**SGTXT** (Sgtxt)

Document line text used for context and free-text pattern filters.


**SHKZG** (Shkzg)

Debit/Credit indicator used to separate accounting posting direction.

**SHKZG Options:**
- S — Line posts on the debit side of the account.
- H — Line posts on the credit side of the account.

**STBLG** (Stblg)

Number of the reversal or referenced document on the header when you need to correlate cancelled postings with their follow-on documents.


**SW_DEST** (Sw Dest)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.


**TCODE** (Tcode)

SAP Transaction code


**TIME_REF_FLD** (Time Ref Fld)

Identifies which time-of-day field should accompany the chosen document date attribute when the runtime measures elapsed age for each line.

<mark>Name of the time field used as the aging anchor-time analogue of DATE_REF_FLD for duration-from-reference logic.</mark>

**TIME_REF_FLD Options:**
- Use a time field that exists on the same structure as the document date reference you configured.
- Values follow the SAP time representation used in your system for that field.

**UPDDT** (Upddt)

<mark>Update date synonym on IDoc or change rows mirroring UPDDAT-style last-changed calendar stamps.</mark>


**USNAM** (Usnam)

SAP changed-by/created-by user field used for accountability filtering.


**WAERS** (Waers)

Currency key used for monetary field interpretation and filtering.


**WAERS_T001** (Waers T001)

Company-code local currency from the financial directory row joined to the document; use it to align document currency rows with the official company-code currency.


**WRBTR** (Wrbtr)

<mark>Amount in document currency.</mark>


**XBLNR** (Xblnr)

Reference document number used for external document matching and traceability.


**XREVERSAL** (Xreversal)

Header-level reversal indicator used to include or exclude documents that represent reversal traffic in the exception population.


### Parameter Relationships

How parameter combinations work together

**Explicit calendar window versus relative lookback:** **DATUM** supplies explicit from-and-to calendar bounds for the monitoring pass. When **DATUM** is empty, **BACKDAYS** (and optionally **FORWDAYS**) builds the calendar window relative to the evaluation day before documents are read.

**Reference date axis:** **DATE_REF_FLD** chooses which header date attribute is mapped into that calendar window for each generated period slice, so the same BACKDAYS span can follow creation, document, change, or update dates depending on configuration.

**Age filter after dates:** **DURATION** with **DURATION_UNIT** is an additional filter applied after date-oriented selection: each candidate line keeps its place in the result only when the computed age from the reference date and clock fields still fits the configured duration band.

**Fiscal period boundary:** **PERIOD_CLOSING_DAY** works with the generated date and posting-date tables to shape how fiscal periods are derived for the selection pass, which indirectly constrains which header lines qualify before line facts are merged.

**Remote execution path:** **SW_DEST** must be populated so the remote join runs in the monitored system; other organizational filters such as **BUKRS**, **HKONT**, or **KOART** only affect which documents are returned once connectivity is established.

**Final selection:** Both the date window logic (explicit **DATUM** or **BACKDAYS**/**FORWDAYS**) and the **DURATION**/**DURATION_UNIT** age test must be satisfied before a row is treated as part of the final exception population for alerting.


### Default Values

- **PERIOD_CLOSING_DAY** - 15
- **BACKDAYS** - 10
- **DATE_REF_FLD** - CPUDT
- **DURATION_UNIT** - D
- **LANGU** - EN
- **DURATION** - initial - treated as empty range keeps rows by code

### Practical Example of Parameter Configuration

**Use Case 1: Company-wide prior-period posting scan**

**Purpose:** Keep month-end focused on all company codes while using the default creation-date reference and day-based aging.
```
BUKRS = 1000 - 1999
BACKDAYS = 14
DATE_REF_FLD = CPUDT
DURATION = 5 - 999999
DURATION_UNIT = D
```

**Use Case 2: Full-day age filter for high-risk accounts**

**Purpose:** Highlight only lines that are at least thirty full days old after the date window is applied.
```
HKONT = 200000 - 299999
BACKDAYS = 30
DURATION = 30
DURATION_UNIT = F
PERIOD_CLOSING_DAY = 25
```

**Use Case 3: Explicit close-week window**

**Purpose:** Anchor the run to a known reopening week instead of relative lookback alone.
```
DATUM = 20250325 - 20250331
BUKRS = 1000
BLART = SA - ZP
DURATION_UNIT = H
DURATION = 0 - 48
```

**Use Case 4: Vendor subledger slice with document-type control**

**Purpose:** Narrow to vendor account-type cluster paths while still applying language and posting-date filters.
```
KOART = K
BUDAT = 20250101 - 20250131
LANGU = EN
TCODE = FB60
```

**Use Case 5: Material document references and user accountability**

**Purpose:** Tie exceptions to external reference numbers and preparers for targeted follow-up.
```
XBLNR = INV2025*
USNAM = BATCH01 - BATCH99
CPUDT = 20250401 - 20250415
WRBTR = 10000 - 999999999
SW_DEST = PROD_FIN
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_07_FI_DOC_POSTED | AEDAT | AEDAT | CHAR(50) | AEDAT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BACKDAYS | BACKDAYS | INT4(10) | BACKDAYS |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BELNR | BELNR | CHAR(50) | BELNR |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BKTXT | BKTXT | CHAR(50) | BKTXT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BLART | BLART | CHAR(50) | BLART |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BLDAT | BLDAT | CHAR(50) | BLDAT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BSCHL | BSCHL | CHAR(50) | BSCHL |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BSTAT | BSTAT | CHAR(50) | BSTAT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BUDAT | BUDAT | CHAR(50) | BUDAT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BUKRS | BUKRS | CHAR(50) | BUKRS |
| /SKN/S_SW_10_07_FI_DOC_POSTED | BUZEI | BUZEI | CHAR(50) | BUZEI |
| /SKN/S_SW_10_07_FI_DOC_POSTED | CPUDT | CPUDT | CHAR(50) | CPUDT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | DATE_REF_FLD | DATE_REF_FLD | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_07_FI_DOC_POSTED | DATUM | DATUM | CHAR(50) | DATUM |
| /SKN/S_SW_10_07_FI_DOC_POSTED | DMBE2 | DMBE2 | CHAR(50) | DMBE2 |
| /SKN/S_SW_10_07_FI_DOC_POSTED | DMBE3 | DMBE3 | CHAR(50) | DMBE3 |
| /SKN/S_SW_10_07_FI_DOC_POSTED | DMBTR | DMBTR | CHAR(50) | DMBTR |
| /SKN/S_SW_10_07_FI_DOC_POSTED | DURATION | DURATION | INT4(10) | DURATION |
| /SKN/S_SW_10_07_FI_DOC_POSTED | DURATION_UNIT | DURATION_UNIT | CHAR(1) | DURATION_UNIT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | FORWDAYS | FORWDAYS | INT4(10) | FORWDAYS |
| /SKN/S_SW_10_07_FI_DOC_POSTED | GJAHR | GJAHR | CHAR(50) | GJAHR |
| /SKN/S_SW_10_07_FI_DOC_POSTED | GRPID | GRPID | CHAR(50) | GRPID |
| /SKN/S_SW_10_07_FI_DOC_POSTED | GVTYP | GVTYP | CHAR(50) | GVTYP |
| /SKN/S_SW_10_07_FI_DOC_POSTED | HKONT | HKONT | CHAR(50) | HKONT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | HWAE2 | HWAE2 | CHAR(50) | HWAE2 |
| /SKN/S_SW_10_07_FI_DOC_POSTED | HWAE3 | HWAE3 | CHAR(50) | HWAE3 |
| /SKN/S_SW_10_07_FI_DOC_POSTED | HWAER | HWAER | CHAR(50) | HWAER |
| /SKN/S_SW_10_07_FI_DOC_POSTED | KOART | KOART | CHAR(50) | KOART |
| /SKN/S_SW_10_07_FI_DOC_POSTED | KTOPL | KTOPL | CHAR(50) | KTOPL |
| /SKN/S_SW_10_07_FI_DOC_POSTED | KURS2 | KURS2 | CHAR(50) | KURS2 |
| /SKN/S_SW_10_07_FI_DOC_POSTED | KURS3 | KURS3 | CHAR(50) | KURS3 |
| /SKN/S_SW_10_07_FI_DOC_POSTED | KURSF | KURSF | CHAR(50) | KURSF |
| /SKN/S_SW_10_07_FI_DOC_POSTED | KZBTR | KZBTR | CHAR(50) | KZBTR |
| /SKN/S_SW_10_07_FI_DOC_POSTED | LANGU | LANGU | CHAR(1) | LANGU |
| /SKN/S_SW_10_07_FI_DOC_POSTED | MONAT | MONAT | CHAR(50) | MONAT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | PERIOD_CLOSING_DAY | PERIOD_CLOSING_DAY | NUMC(2) | PERIOD_CLOSING_DAY |
| /SKN/S_SW_10_07_FI_DOC_POSTED | SGTXT | SGTXT | CHAR(50) | SGTXT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | SHKZG | SHKZG | CHAR(1) | SHKZG |
| /SKN/S_SW_10_07_FI_DOC_POSTED | STBLG | STBLG | CHAR(50) | STBLG |
| /SKN/S_SW_10_07_FI_DOC_POSTED | TCODE | TCODE | CHAR(50) | TCODE |
| /SKN/S_SW_10_07_FI_DOC_POSTED | TIME_REF_FLD | TIME_REF_FLD | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_07_FI_DOC_POSTED | UPDDT | UPDDT | CHAR(50) | UPDDT |
| /SKN/S_SW_10_07_FI_DOC_POSTED | USNAM | USNAM | CHAR(50) | USNAM |
| /SKN/S_SW_10_07_FI_DOC_POSTED | WAERS | WAERS | CHAR(50) | WAERS |
| /SKN/S_SW_10_07_FI_DOC_POSTED | WAERS_T001 | WAERS_T001 | CHAR(50) | WAERS_T001 |
| /SKN/S_SW_10_07_FI_DOC_POSTED | WRBTR | WRBTR | CHAR(50) | WRBTR |
| /SKN/S_SW_10_07_FI_DOC_POSTED | XBLNR | XBLNR | CHAR(50) | XBLNR |
| /SKN/S_SW_10_07_FI_DOC_POSTED | XREVERSAL | XREVERSAL | CHAR(50) | XREVERSAL |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_10_07_FI_DOC_POSTED.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_07_FI_DOC_POSTED
*"----------------------------------------------------------------------
  DATA_MULTY: DATUM DATUM.
  DATA_MULTY: CPUDT CPUDT.
  DATA_MULTY: BUKRS BUKRS.
  DATA_MULTY: BELNR BELNR_D.
  DATA_MULTY: GJAHR GJAHR.
  DATA_MULTY: BLART BLART.
  DATA_MULTY: BLDAT BLDAT.
  DATA_MULTY: BUDAT BUDAT.
  DATA_MULTY: MONAT MONAT.
  DATA_MULTY: AEDAT AEDAT_BKPF.
  DATA_MULTY: UPDDT UPDDT.
  DATA_MULTY: USNAM USNAM.
  DATA_MULTY: TCODE TCODE.
  DATA_MULTY: XBLNR XBLNR1.
  DATA_MULTY: STBLG STBLG.
  DATA_MULTY: BKTXT BKTXT.
  DATA_MULTY: WAERS WAERS.
  DATA_MULTY: WAERS_T001 WAERS.
  DATA_MULTY: KURSF KURSF.
  DATA_MULTY: BSTAT BSTAT_D.
  DATA_MULTY: GRPID GRPID_BKPF.
  DATA_MULTY: HWAE2 HWAE2.
  DATA_MULTY: HWAE3 HWAE3.
  DATA_MULTY: KURS2 KURS2.
  DATA_MULTY: KURS3 KURS3.
  DATA_MULTY: XREVERSAL XREVERSAL.
  DATA_MULTY: BUZEI BUZEI.
  DATA_MULTY: BSCHL BSCHL.
  DATA_MULTY: KTOPL KTOPL.
  DATA_MULTY: HKONT HKONT.
  DATA_MULTY: GVTYP GVTYP.
  DATA_MULTY: SHKZG SHKZG.
  DATA_MULTY: DMBTR DMBTR.
  DATA_MULTY: WRBTR WRBTR.
  DATA_MULTY: DMBE2 DMBE2.
  DATA_MULTY: DMBE3 DMBE3.
  DATA_MULTY: KZBTR KZBTR.
  DATA_MULTY: SGTXT SGTXT.
  DATA_MULTY: HWAER HWAER.
  DATA_MULTY: KOART KOART.
  DATA_SINGLE: PERIOD_CLOSING_DAY NUMC2.
  LV_PERIOD_CLOSING_DAY = 15.
  DATA_SINGLE: BACKDAYS /SKN/E_MN_AN_BACKDAYS.
  LV_BACKDAYS = '10'.
  DATA_SINGLE: DATE_REF_FLD NAME_FELD.
  LV_DATE_REF_FLD = 'CPUDT'.
  DATA_MULTY: DURATION /SKN/E_SW_DURATION.
  DATA_SINGLE: DURATION_UNIT /SKN/E_SW_DURATION_UNIT.
  LV_DURATION_UNIT = 'D'.
  DATA_SINGLE: FORWDAYS /SKN/E_MN_AN_FORWDAYS.
  DATA_SINGLE: LANGU LANGU.
  LV_LANGU = 'EN'.
  DATA_SINGLE: TIME_REF_FLD NAME_FELD.
  DATA: FLD(60)   TYPE C,
        REF_DATE  TYPE D,
        TIME_DIFF TYPE INT4.
  DATA SY_DATLO LIKE SY-DATLO.
  DATA SY_TIMLO LIKE SY-TIMLO.
  DATA DATE_FROM LIKE SY-DATUM.
  DATA DATE_TO LIKE SY-DATUM.
  DATA: LT_DATA     LIKE T_DATA[],
        LT_DATA_TMP LIKE T_DATA[].
  SELECT_MULTY: CPUDT.
  SELECT_MULTY: BUKRS.
  SELECT_MULTY: BELNR.
  CONVERT_MULTY: BELNR ALPHA.
  SELECT_MULTY: GJAHR.
  CONVERT_MULTY: GJAHR GJAHR.
  SELECT_MULTY: BLART.
  SELECT_MULTY: BLDAT.
  SELECT_MULTY: BUDAT.
  SELECT_MULTY: MONAT.
  SELECT_MULTY: AEDAT.
  SELECT_MULTY: UPDDT.
  SELECT_MULTY: USNAM.
  SELECT_MULTY: TCODE.
  SELECT_MULTY: XBLNR.
  SELECT_MULTY: STBLG.
  CONVERT_MULTY: STBLG ALPHA.
  SELECT_MULTY: BKTXT.
  SELECT_MULTY: WAERS.
  SELECT_MULTY: WAERS_T001.
  SELECT_MULTY: KURSF.
  SELECT_MULTY: BSTAT.
  SELECT_MULTY: GRPID.
  SELECT_MULTY: HWAE2.
  SELECT_MULTY: HWAE3.
  SELECT_MULTY: KURS2.
  SELECT_MULTY: KURS3.
  SELECT_MULTY: XREVERSAL.
  SELECT_MULTY: BUZEI.
  SELECT_MULTY: BSCHL.
  SELECT_MULTY: KTOPL.
  SELECT_MULTY: HKONT.
  SELECT_MULTY: GVTYP.
  SELECT_MULTY: SHKZG.
  SELECT_MULTY: DMBTR.
  SELECT_MULTY: WRBTR.
  SELECT_MULTY: DMBE2.
  SELECT_MULTY: DMBE3.
  SELECT_MULTY: KZBTR.
  SELECT_MULTY: SGTXT.
  SELECT_MULTY: KOART.
  SELECT_SINGLE: PERIOD_CLOSING_DAY.
  SELECT_SINGLE: BACKDAYS.
  SELECT_SINGLE: DATE_REF_FLD.
  SELECT_MULTY: DURATION.
  SELECT_SINGLE: DURATION_UNIT.
  SELECT_SINGLE: FORWDAYS.
  SELECT_SINGLE: LANGU.
  CONVERT_SINGLE: LANGU ISOLA.
  SELECT_SINGLE: TIME_REF_FLD.
  DATA: LV_SW_DEST  TYPE RFCDEST,
        LV_TABIX    TYPE I,
        LV_STR_NAME TYPE /SKN/E_MN_AN_SN,
        LV_TABNAME  TYPE TABNAME,
        LV_TAB_CAT  TYPE TABCLASS,
        LV_BSEG     TYPE BOOLE_D,
        LV_ERROR    TYPE BOOLE_D,
        LV_ERR_MSG  TYPE BAPI_MSG,
        LV_KOART    TYPE KOART,
        LV_ROWCOUNT TYPE SOID-ACCNT.
  DATA: LS_OPTION            TYPE RFC_DB_OPT,
        LWA_TABLES_LIST      TYPE /SKN/S_TABLES,
        LWA_IN_RANGE         TYPE /SKN/S_SW_RANGE_TAB,
        LWA_JOIN_CONDITION   TYPE /SKN/S_TABLE_JOIN,
        LS_SEL_FIELDS        TYPE /SKN/S_SEL_FIELDS,
        LS_BUDAT             TYPE RANGE_S_DATS,
        LS_DATA              TYPE /SKN/S_SW_10_07_FI_DOC_POSTED,
        LS_BSEG              TYPE /SKN/S_SW_10_07_FI_DOC_POSTED,
        LWA_ALL_ENTRIES_TAB  TYPE /SKN/S_SW_TAB6000,
        LWA_ALL_ENTRIES_COND TYPE /SKN/S_TABLE_JOIN.
  DATA: LT_OPTIONS      TYPE TABLE OF RFC_DB_OPT,
        LT_OPTIONS_MAIN TYPE TABLE OF RFC_DB_OPT.
  DATA: LT_BUDAT TYPE TABLE OF RANGE_S_DATS.
  DATA: LT_TABLES_LIST    TYPE /SKN/TT_TABLES,
        LT_OUT_WHERE_COND TYPE TABLE OF /SKN/S_SW_WHERE_TAB,
        LT_IN_RANGE       TYPE TABLE OF /SKN/S_SW_RANGE_TAB,
        LT_DATA_RFC       TYPE TABLE OF /SKN/S_SW_TAB2000,
        LT_SEL_FIELDS     TYPE /SKN/TT_SEL_FIELDS,
        LT_SEL_FIELDS2    TYPE /SKN/TT_SEL_FIELDS,
        LT_BSEG           TYPE TABLE OF /SKN/S_SW_10_07_FI_DOC_POSTED.
  DATA LT_JOIN_CONDITION TYPE /SKN/TT_TABLE_JOIN.
  DATA LT_SORT_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT.
  DATA LT_GROUP_BY_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT.
  DATA LT_HAVING_OPTIONS TYPE TABLE OF RFC_DB_OPT.
  DATA LT_OUTPUT_FIELDS TYPE /SKN/TT_RFC_DB_FLD_EXTEND.
  DATA LT_DFIES TYPE TABLE OF DFIES.
  DATA LT_RETURN TYPE BAPIRET2_T.
  DATA LT_ALL_ENTRIES_TAB TYPE TABLE OF /SKN/S_SW_TAB6000.
  DATA LT_ALL_ENTRIES_COND TYPE TABLE OF /SKN/S_TABLE_JOIN.
  DATA LT_ALL_ENTRIES_DFIES TYPE TABLE OF DFIES.
  DATA LV_NAME_FIRST TYPE AD_NAMEFIR.
  DATA LV_NAME_LAST TYPE AD_NAMELAS.
  DATA LV_NAME_TEXT TYPE AD_NAMTEXT.
  DATA LS_WA_ADRP TYPE ADRP.
  FIELD-SYMBOLS: <FS_DATA> TYPE /SKN/S_SW_10_07_FI_DOC_POSTED,
                 <FS_VAL>  TYPE ANY.
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  REFRESH LT_OPTIONS.
  REFRESH LT_OUT_WHERE_COND.
  REFRESH LT_TABLES_LIST.
  CLEAR: LS_OPTION.
  SELECT_SINGLE: SW_DEST.
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  IF R_DATUM[] IS INITIAL.
    RS_DATUM-SIGN   = 'I'.
    IF LV_FORWDAYS IS INITIAL.
      DATE_FROM = SY_DATLO - LV_BACKDAYS.
      DATE_TO   = SY_DATLO.
      RS_DATUM-OPTION = 'BT'.
    ELSE.
      IF LV_BACKDAYS IS NOT INITIAL.
        DATE_FROM = SY_DATLO - LV_BACKDAYS.
        DATE_TO   = SY_DATLO + LV_FORWDAYS.
        RS_DATUM-OPTION = 'BT'.
      ELSE.
        DATE_FROM = SY_DATLO + LV_FORWDAYS.
        RS_DATUM-OPTION = 'GE'.
      ENDIF.
    ENDIF.
  ENDIF.
***************** Get Table Category **********************
  LV_STR_NAME = 'BSEG'.
  LV_TABNAME  = 'DD02L'.
* Get table type
  CALL FUNCTION '/SKN/F_MN_AN_AR_STRUC_CAT_GET'
    EXPORTING
      I_STRUC_NAME      = LV_STR_NAME          " Object name
      I_DEF_TABNAME     = 'DD02L'              " Table selected from
      I_RFCDEST         = LV_SW_DEST           " RFC destination
      I_LANGU           = 'E'
    IMPORTING
      E_CATEGORY        = LV_TAB_CAT           " Table Type:     TRANSP/CLUSTER/POOL/VIEW/APPEND/INTTAB
      E_ERROR           = LV_ERROR
      E_ERR_MSG         = LV_ERR_MSG
    EXCEPTIONS
      STRUC_EMPTY       = 1
      CLIENT_EMPTY      = 2
      SYSTEM_EMPTY      = 3
      RFCDEST_NOT_FOUND = 4
      OTHERS            = 5.
  IF LV_TAB_CAT EQ 'CLUSTER'.
    IF R_KOART[] IS NOT INITIAL.
      READ TABLE R_KOART INTO RS_KOART INDEX 1.
      CHECK RS_KOART-LOW EQ 'D' OR
            RS_KOART-LOW EQ 'K' OR
            RS_KOART-LOW EQ 'S'.
      LV_KOART = RS_KOART-LOW.
    ELSE.
      LV_BSEG = 'X'.
    ENDIF.
  ENDIF.
***************** Get Table Category *********************
* Calculate document periods by closing day
  CALL FUNCTION '/SKN/F_SW_GET_DOC_POST_PERIOD'
    EXPORTING
      IV_DATE_FROM         = DATE_FROM
      IV_DATE_TO           = DATE_TO
      IV_CLOSED_PERIOD_DAY = LV_PERIOD_CLOSING_DAY
    TABLES
      ET_DATUM             = R_DATUM[]       " Accounting Entry Date
      ET_BUDAT             = LT_BUDAT[].     " Posting Date
  CLEAR: LT_DATA_RFC[], LT_OUTPUT_FIELDS[], LT_OPTIONS,
         LT_DFIES[], LT_RETURN[], LT_OUT_WHERE_COND[],
         LT_GROUP_BY_OPTIONS, LT_SORT_OPTIONS.
  CLEAR IS_ALERT.
  REFRESH T_DATA.
* BKPF
  _RANGE_TO_SEL_TABLE 'A~BUKRS'     BUKRS.
  _RANGE_TO_SEL_TABLE 'A~BELNR'     BELNR.
  _RANGE_TO_SEL_TABLE 'A~GJAHR'     GJAHR.
  _RANGE_TO_SEL_TABLE 'A~MONAT'     MONAT.
  _RANGE_TO_SEL_TABLE 'A~BLART'     BLART.
  _RANGE_TO_SEL_TABLE 'A~TCODE'     TCODE.
  _RANGE_TO_SEL_TABLE 'A~USNAM'     USNAM.
  _RANGE_TO_SEL_TABLE 'A~HWAE2'     HWAE2.
  _RANGE_TO_SEL_TABLE 'A~HWAE3'     HWAE3.
  _RANGE_TO_SEL_TABLE 'A~TCODE'     TCODE.
  _RANGE_TO_SEL_TABLE 'A~XBLNR'     XBLNR.
  _RANGE_TO_SEL_TABLE 'A~XREVERSAL' XREVERSAL.
  _RANGE_TO_SEL_TABLE 'A~BSTAT'     BSTAT.
  _RANGE_TO_SEL_TABLE 'A~GRPID'     GRPID.
  _RANGE_TO_SEL_TABLE 'A~STBLG'     STBLG.
  _RANGE_TO_SEL_TABLE 'A~BKTXT'     BKTXT.
  _RANGE_TO_SEL_TABLE 'A~WAERS'     WAERS.
  _RANGE_TO_SEL_TABLE 'A~HWAER'     HWAER.
* BSEG
  IF LV_TAB_CAT EQ 'TRANSP'.
    _RANGE_TO_SEL_TABLE 'B~BUZEI'  BUZEI.
    _RANGE_TO_SEL_TABLE 'B~BSCHL'  BSCHL.
    _RANGE_TO_SEL_TABLE 'B~SHKZG'  SHKZG.
    _RANGE_TO_SEL_TABLE 'B~DMBTR'  DMBTR.
    _RANGE_TO_SEL_TABLE 'B~WRBTR'  WRBTR.
    _RANGE_TO_SEL_TABLE 'B~DMBE2'  DMBE2.
    _RANGE_TO_SEL_TABLE 'B~DMBE3'  DMBE3.
    _RANGE_TO_SEL_TABLE 'B~HKONT'  HKONT.
    _RANGE_TO_SEL_TABLE 'B~GVTYP'  GVTYP.
    _RANGE_TO_SEL_TABLE 'B~KZBTR'  KZBTR.
    _RANGE_TO_SEL_TABLE 'B~KOART'  KOART.
  ELSEIF LV_TAB_CAT EQ 'CLUSTER'.
    _RANGE_TO_SEL_TABLE 'B~BUZEI'  BUZEI.
    _RANGE_TO_SEL_TABLE 'B~BSCHL'  BSCHL.
    _RANGE_TO_SEL_TABLE 'B~SHKZG'  SHKZG.
    _RANGE_TO_SEL_TABLE 'B~DMBTR'  DMBTR.
    _RANGE_TO_SEL_TABLE 'B~WRBTR'  WRBTR.
    _RANGE_TO_SEL_TABLE 'B~DMBE2'  DMBE2.
    _RANGE_TO_SEL_TABLE 'B~DMBE3'  DMBE3.
    _RANGE_TO_SEL_TABLE 'B~HKONT'  HKONT.
  ENDIF.
* T001
  _RANGE_TO_SEL_TABLE 'T~WAERS'     WAERS_T001.
  _RANGE_TO_SEL_TABLE 'T~KTOPL'     KTOPL.
  LT_OPTIONS_MAIN[] = LT_OUT_WHERE_COND[].
*
  REFRESH LT_SEL_FIELDS[].
  _ADAPT_SEL_FIELDS 'BKPF' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS LV_SW_DEST.
  IF LV_TAB_CAT EQ 'TRANSP'.
    _ADAPT_SEL_FIELDS 'BSEG' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS LV_SW_DEST.
  ELSEIF LV_TAB_CAT EQ 'CLUSTER'.
    CASE LV_KOART.
      WHEN 'D'.   " Customers
        _ADAPT_SEL_FIELDS 'BSID' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS LV_SW_DEST.
      WHEN 'K'.   " Vendors
        _ADAPT_SEL_FIELDS 'BSIK' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS LV_SW_DEST.
      WHEN 'S'.   " G/L
        _ADAPT_SEL_FIELDS 'BSIS' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS LV_SW_DEST.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
  CLEAR LS_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'T001'.
  LS_SEL_FIELDS-FIELD = 'KTOPL'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'T001'.
  LS_SEL_FIELDS-FIELD = 'WAERS'.
  LS_SEL_FIELDS-ALIAS = 'WAERS_T001'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  SELECT_SINGLE: SW_DEST.
  LOOP AT R_DATUM INTO RS_DATUM.
    LV_TABIX = SY-TABIX.
    REFRESH: R_CPUDT, R_AEDAT, R_UPDDT, R_BLDAT, R_BUDAT.
    CLEAR: LT_OUT_WHERE_COND[], LT_TABLES_LIST.
    CASE LV_DATE_REF_FLD.
      WHEN 'CPUDT'.
        MOVE-CORRESPONDING RS_DATUM TO RS_CPUDT.
        APPEND RS_CPUDT TO R_CPUDT.
      WHEN 'BLDAT'.
        MOVE-CORRESPONDING RS_DATUM TO RS_BLDAT.
        APPEND RS_BLDAT TO R_BLDAT.
      WHEN 'AEDAT'.
        MOVE-CORRESPONDING RS_DATUM TO RS_AEDAT.
        APPEND RS_AEDAT TO R_AEDAT.
      WHEN 'UPDDT'.
        MOVE-CORRESPONDING RS_DATUM TO RS_UPDDT.
        APPEND RS_UPDDT TO R_UPDDT.
    ENDCASE.
    _RANGE_TO_SEL_TABLE 'A~AEDAT' AEDAT.
    _RANGE_TO_SEL_TABLE 'A~CPUDT' CPUDT.
    _RANGE_TO_SEL_TABLE 'A~UPDDT' UPDDT.
    _RANGE_TO_SEL_TABLE 'A~BLDAT' BLDAT.
* Set BUDAT condition date
    READ TABLE LT_BUDAT INTO LS_BUDAT INDEX LV_TABIX.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LS_BUDAT TO RS_BUDAT.
      APPEND RS_BUDAT TO R_BUDAT.
      CLEAR: LS_BUDAT.
    ENDIF.
    _RANGE_TO_SEL_TABLE 'A~BUDAT' BUDAT.
    LT_OPTIONS[] = LT_OUT_WHERE_COND[].
    IF LT_OPTIONS[]      IS NOT INITIAL AND
       LT_OPTIONS_MAIN[] IS NOT INITIAL.
      LS_OPTION-TEXT = 'AND'.
      APPEND LS_OPTION TO LT_OPTIONS.
    ENDIF.
    APPEND LINES OF LT_OPTIONS_MAIN TO LT_OPTIONS.
    IF LV_TAB_CAT EQ 'TRANSP'.
      _APPEND_TABLES_LIST 'BKPF' '' 'A'.
      _APPEND_TABLES_LIST 'BSEG' '' 'B'.
    ELSEIF LV_TAB_CAT EQ 'CLUSTER'.
      _APPEND_TABLES_LIST 'BKPF' '' 'A'.
      CASE LV_KOART.
        WHEN 'D'.   " Customers
          _APPEND_TABLES_LIST 'BSID' '' 'B'.
        WHEN 'K'.   " Vendors
          _APPEND_TABLES_LIST 'BSIK' '' 'B'.
        WHEN 'S'.   " G/L
          _APPEND_TABLES_LIST 'BSIS' '' 'B'.
      ENDCASE.
    ENDIF.
    _APPEND_TABLES_LIST 'T001' 'X' 'T'.
* Join condition
    REFRESH LT_JOIN_CONDITION[].
    _JOIN_CONDITION 'A' 'BUKRS' 'B' 'BUKRS'.
    _JOIN_CONDITION 'A' 'GJAHR' 'B' 'GJAHR'.
    _JOIN_CONDITION 'A' 'BELNR' 'B' 'BELNR'.
    _JOIN_CONDITION 'A' 'BUKRS' 'T' 'BUKRS'.
******************************* Get data  *************************
    CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
      DESTINATION LV_SW_DEST
      IMPORTING
        ROWCOUNT             = LV_ROWCOUNT
      TABLES
        OPTIONS              = LT_OPTIONS
        DATA                 = LT_DATA_RFC
        TABLES_LIST          = LT_TABLES_LIST
        JOIN_CONDITION       = LT_JOIN_CONDITION
        SEL_FIELDS           = LT_SEL_FIELDS
        SORT_OPTIONS         = LT_SORT_OPTIONS
        GROUP_BY_OPTIONS     = LT_GROUP_BY_OPTIONS
        HAVING_OPTIONS       = LT_HAVING_OPTIONS
        OUTPUT_FIELDS        = LT_OUTPUT_FIELDS
        DFIES                = LT_DFIES
        RETURN               = LT_RETURN
      EXCEPTIONS
        TABLE_NOT_AVAILABLE  = 1
        TABLE_WITHOUT_DATA   = 2
        OPTION_NOT_VALID     = 3
        FIELD_NOT_VALID      = 4
        NOT_AUTHORIZED       = 5
        DATA_BUFFER_EXCEEDED = 6
        OTHERS               = 7.
    IF SY-SUBRC IS NOT INITIAL OR LT_RETURN IS NOT INITIAL.
      CLEAR LT_DATA_RFC.
    ELSE.
      _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_DATA_TMP LT_OUTPUT_FIELDS 1.
    ENDIF.
    APPEND LINES OF LT_DATA_TMP TO LT_DATA.
******************************* Get data  *************************
* For Cluster table - Get data from the second index table(like: bsis and bsas)
    IF LV_TAB_CAT EQ 'CLUSTER' AND
       LV_KOART IS NOT INITIAL.
      CLEAR: LT_TABLES_LIST, LT_SEL_FIELDS2.
      _APPEND_TABLES_LIST 'BKPF' '' 'A'.
      IF LV_TAB_CAT EQ 'TRANSP'.
        _APPEND_TABLES_LIST 'BSEG' '' 'B'.
      ELSEIF LV_TAB_CAT EQ 'CLUSTER'.
        _ADAPT_SEL_FIELDS 'BKPF' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS2 LV_SW_DEST.
        CASE LV_KOART.
          WHEN 'D'.   " Customers
            _APPEND_TABLES_LIST 'BSAD' '' 'B'.
            _ADAPT_SEL_FIELDS 'BSAD' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS2 LV_SW_DEST.
          WHEN 'K'.   " Vendors
            _APPEND_TABLES_LIST 'BSAK' '' 'B'.
            _ADAPT_SEL_FIELDS 'BSAK' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS2 LV_SW_DEST.
          WHEN 'S'.   " G/L
            _APPEND_TABLES_LIST 'BSAS' '' 'B'.
            _ADAPT_SEL_FIELDS 'BSAS' '/SKN/S_SW_10_07_FI_DOC_POSTED'  LT_SEL_FIELDS2 LV_SW_DEST.
        ENDCASE.
      ENDIF.
      _APPEND_TABLES_LIST 'T001' 'X' 'T'.
      CLEAR LS_SEL_FIELDS.
      CLEAR LS_SEL_FIELDS.
      LS_SEL_FIELDS-TABLE = 'T001'.
      LS_SEL_FIELDS-FIELD = 'KTOPL'.
      APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS2.
      CLEAR LS_SEL_FIELDS.
      LS_SEL_FIELDS-TABLE = 'T001'.
      LS_SEL_FIELDS-FIELD = 'WAERS'.
      LS_SEL_FIELDS-ALIAS = 'WAERS_T001'.
      APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS2.
* Join condition
      REFRESH LT_JOIN_CONDITION[].
      _JOIN_CONDITION 'A' 'BUKRS' 'B' 'BUKRS'.
      _JOIN_CONDITION 'A' 'GJAHR' 'B' 'GJAHR'.
      _JOIN_CONDITION 'A' 'BELNR' 'B' 'BELNR'.
      _JOIN_CONDITION 'A' 'BUKRS' 'T' 'BUKRS'.
      CLEAR: LT_DATA_TMP, LT_OUTPUT_FIELDS, LT_DATA_RFC.
*************** Get Data from Secondary Index Table **************************
      CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
        DESTINATION LV_SW_DEST
        IMPORTING
          ROWCOUNT             = LV_ROWCOUNT
        TABLES
          OPTIONS              = LT_OPTIONS
          DATA                 = LT_DATA_RFC
          TABLES_LIST          = LT_TABLES_LIST
          JOIN_CONDITION       = LT_JOIN_CONDITION
          SEL_FIELDS           = LT_SEL_FIELDS2
          SORT_OPTIONS         = LT_SORT_OPTIONS
          GROUP_BY_OPTIONS     = LT_GROUP_BY_OPTIONS
          HAVING_OPTIONS       = LT_HAVING_OPTIONS
          OUTPUT_FIELDS        = LT_OUTPUT_FIELDS
          DFIES                = LT_DFIES
          RETURN               = LT_RETURN
        EXCEPTIONS
          TABLE_NOT_AVAILABLE  = 1
          TABLE_WITHOUT_DATA   = 2
          OPTION_NOT_VALID     = 3
          FIELD_NOT_VALID      = 4
          NOT_AUTHORIZED       = 5
          DATA_BUFFER_EXCEEDED = 6
          OTHERS               = 7.
      IF SY-SUBRC IS NOT INITIAL OR LT_RETURN IS NOT INITIAL.
        CLEAR LT_DATA_RFC.
      ELSE.
        _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_DATA_TMP LT_OUTPUT_FIELDS 2.
      ENDIF.
      APPEND LINES OF LT_DATA_TMP TO LT_DATA.
*************** Get Data from Secondary Index Table **************************
    ENDIF.
    CLEAR: LT_OPTIONS, LT_DATA_RFC, LT_OUTPUT_FIELDS, LT_DATA_TMP, LT_RETURN.
  ENDLOOP.
  CHECK LT_DATA[] IS NOT INITIAL.
  CLEAR: LT_JOIN_CONDITION.
*********************************** Get BSEG *******************************************
  IF LV_TAB_CAT EQ 'CLUSTER' AND
     LV_BSEG    EQ 'X'.
    SORT LT_DATA BY BUKRS BELNR GJAHR.
    REFRESH LT_TABLES_LIST[].
    _APPEND_TABLES_LIST 'BSEG' ''  ''.
* Convert BSEG to string table
    _ALL_ENTRIES_CONVERT LT_DATA '/SKN/S_SW_10_07_FI_DOC_POSTED'  2.
* Selection fields
    REFRESH LT_SEL_FIELDS[].
* BSEG
    CLEAR LS_SEL_FIELDS.
    LS_SEL_FIELDS-TABLE = 'BSEG'.
    LS_SEL_FIELDS-FIELD = 'BUKRS'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'BELNR'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'GJAHR'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'BUZEI'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'BSCHL'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'SHKZG'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'DMBTR'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'WRBTR'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'DMBE2'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'DMBE3'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'HKONT'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'GVTYP'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
    LS_SEL_FIELDS-FIELD = 'KZBTR'.
    APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
* BSEG
    _RANGE_TO_SEL_TABLE 'BUZEI'  BUZEI.
    _RANGE_TO_SEL_TABLE 'BSCHL'  BSCHL.
    _RANGE_TO_SEL_TABLE 'SHKZG'  SHKZG.
    _RANGE_TO_SEL_TABLE 'DMBTR'  DMBTR.
    _RANGE_TO_SEL_TABLE 'WRBTR'  WRBTR.
    _RANGE_TO_SEL_TABLE 'DMBE2'  DMBE2.
    _RANGE_TO_SEL_TABLE 'DMBE3'  DMBE3.
    _RANGE_TO_SEL_TABLE 'HKONT'  HKONT.
    _RANGE_TO_SEL_TABLE 'GVTYP'  GVTYP.
    _RANGE_TO_SEL_TABLE 'KZBTR'  KZBTR.
* Add condition of For All Entries
    _ALL_ENTRIES_CONDITION 'BSEG~BUKRS' 'BUKRS' ''.
    _ALL_ENTRIES_CONDITION 'BSEG~BELNR' 'BELNR' ''.
    _ALL_ENTRIES_CONDITION 'BSEG~GJAHR' 'GJAHR' ''.
*  Get structure fields
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        TABNAME        = '/SKN/S_SW_10_07_FI_DOC_POSTED'
      TABLES
        DFIES_TAB      = LT_ALL_ENTRIES_DFIES
      EXCEPTIONS
        NOT_FOUND      = 1
        INTERNAL_ERROR = 2
        OTHERS         = 3.
    IF SY-SUBRC NE 0.
      CLEAR LT_ALL_ENTRIES_DFIES[].
    ENDIF.
    CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
      DESTINATION LV_SW_DEST
      IMPORTING
        ROWCOUNT             = LV_ROWCOUNT
      TABLES
        OPTIONS              = LT_OPTIONS[]
        DATA                 = LT_DATA_RFC[]
        TABLES_LIST          = LT_TABLES_LIST[]
        JOIN_CONDITION       = LT_JOIN_CONDITION
        SEL_FIELDS           = LT_SEL_FIELDS[]
        SORT_OPTIONS         = LT_SORT_OPTIONS[]
        OUTPUT_FIELDS        = LT_OUTPUT_FIELDS[]
        DFIES                = LT_DFIES[]
        RETURN               = LT_RETURN[]
        ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB[]
        ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND[]
        ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES[]
      EXCEPTIONS
        TABLE_NOT_AVAILABLE  = 1
        TABLE_WITHOUT_DATA   = 2
        OPTION_NOT_VALID     = 3
        FIELD_NOT_VALID      = 4
        NOT_AUTHORIZED       = 5
        DATA_BUFFER_EXCEEDED = 6
        OTHERS               = 7.
    IF SY-SUBRC IS NOT INITIAL.
      CLEAR LT_DATA_RFC[].
      EXIT.
    ELSE.
      _RFC_TO_T_DATA_INDEX LT_DATA_RFC LT_BSEG LT_OUTPUT_FIELDS 3.
    ENDIF.
  ENDIF.
*********************************** Get BSEG *******************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT LT_DATA ASSIGNING <FS_DATA>.
    FLD = LV_DATE_REF_FLD.
    ASSIGN COMPONENT FLD OF STRUCTURE <FS_DATA> TO <FS_VAL>.
    CHECK <FS_VAL> IS ASSIGNED.
    REF_DATE = <FS_VAL> .
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
  DELETE LT_DATA WHERE DURATION NOT IN R_DURATION .
  LOOP AT LT_DATA INTO LS_DATA.
    IF LV_BSEG EQ 'X'.
      LOOP AT LT_BSEG INTO LS_BSEG WHERE BUKRS EQ LS_DATA-BUKRS
                                   AND   BELNR EQ LS_DATA-BELNR
                                   AND   GJAHR EQ LS_DATA-GJAHR.
        LS_DATA-BUZEI = LS_BSEG-BUZEI.
        LS_DATA-BSCHL = LS_BSEG-BSCHL.
        LS_DATA-SHKZG = LS_BSEG-SHKZG.
        LS_DATA-DMBTR = LS_BSEG-DMBTR.
        LS_DATA-WRBTR = LS_BSEG-WRBTR.
        LS_DATA-DMBE2 = LS_BSEG-DMBE2.
        LS_DATA-DMBE3 = LS_BSEG-DMBE3.
        LS_DATA-KZBTR = LS_BSEG-KZBTR.
        LS_DATA-HKONT = LS_BSEG-HKONT.
        LS_DATA-GVTYP = LS_BSEG-GVTYP.
************************************************ Semantics **************************************************
        IF LS_DATA-BLART IS NOT INITIAL.
* Get Document Type desc.
          CALL FUNCTION '/SKN/F_SW_10_BLART_DESC'
            EXPORTING
              BLART      = LS_DATA-BLART
              LANGU      = LV_LANGU
              SW_DEST    = LV_SW_DEST
            IMPORTING
              TYPE_DESC  = LS_DATA-BLART_DESC
            EXCEPTIONS
              WRONG_CODE = 1
              OTHERS     = 2.
        ENDIF.
        IF LS_DATA-BUKRS IS NOT INITIAL.
* Get Company code desc.
          CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
            EXPORTING
              BUKRS          = LS_DATA-BUKRS
              SW_DEST        = LV_SW_DEST
            IMPORTING
              COMP_CODE_DESC = LS_DATA-BUKRS_DESC
            EXCEPTIONS
              WRONG_CODE     = 1
              OTHERS         = 2.
        ENDIF.
        IF LS_DATA-USNAM IS NOT INITIAL.
* Get user name & surname
          CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
            EXPORTING
              BNAME      = LS_DATA-USNAM   " User Name in User Master Record
              SW_DEST    = LV_SW_DEST      " Logical Destination (Specified in Function Call)
            IMPORTING
              NAME_FIRST = LV_NAME_FIRST
              NAME_LAST  = LV_NAME_LAST
              NAME_TEXT  = LV_NAME_TEXT
            EXCEPTIONS
              NO_DATA    = 1
              OTHERS     = 2.
          IF SY-SUBRC IS INITIAL.
            LS_DATA-USNAM_DESC = LV_NAME_TEXT.
          ENDIF.
          CLEAR: LV_NAME_TEXT.
        ENDIF.
************************************************* Semantics **************************************************
        APPEND LS_DATA TO T_DATA.
      ENDLOOP.
    ELSE.
************************************************ Semantics **************************************************
      IF LS_DATA-BLART IS NOT INITIAL.
* Get Document Type desc.
        CALL FUNCTION '/SKN/F_SW_10_BLART_DESC'
          EXPORTING
            BLART      = LS_DATA-BLART
            LANGU      = LV_LANGU
            SW_DEST    = LV_SW_DEST
          IMPORTING
            TYPE_DESC  = LS_DATA-BLART_DESC
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
      ENDIF.
      IF LS_DATA-BUKRS IS NOT INITIAL.
* Get Company code desc.
        CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
          EXPORTING
            BUKRS          = LS_DATA-BUKRS
            SW_DEST        = LV_SW_DEST
          IMPORTING
            COMP_CODE_DESC = LS_DATA-BUKRS_DESC
          EXCEPTIONS
            WRONG_CODE     = 1
            OTHERS         = 2.
      ENDIF.
      IF LS_DATA-USNAM IS NOT INITIAL.
* user name & surname
        CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
          EXPORTING
            BNAME      = LS_DATA-USNAM   " User Name in User Master Record
            SW_DEST    = LV_SW_DEST      " Logical Destination (Specified in Function Call)
          IMPORTING
            NAME_FIRST = LV_NAME_FIRST
            NAME_LAST  = LV_NAME_LAST
            NAME_TEXT  = LV_NAME_TEXT
          EXCEPTIONS
            NO_DATA    = 1
            OTHERS     = 2.
        IF SY-SUBRC IS INITIAL.
          LS_DATA-USNAM_DESC = LV_NAME_TEXT.
        ENDIF.
        CLEAR: LV_NAME_TEXT.
      ENDIF.
      IF LS_DATA-HKONT IS NOT INITIAL.
* Get GL Account Desc.
        CALL FUNCTION '/SKN/FC_SW_10_GL_DESC'
          EXPORTING
            KTOPL      = LS_DATA-KTOPL      " Chart of Accounts
            SAKNR      = LS_DATA-HKONT      " G/L Account Number
            LANGU      = LV_LANGU           " Language Key
            SW_DEST    = LV_SW_DEST         " Logical Destination (Specified in Function Call)
          IMPORTING
*           TXT20      =                    " G/L account short text
            TXT50      = LS_DATA-HKONT_DESC " G/L Account Long Text
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
        IF SY-SUBRC EQ 0.
        ENDIF.
      ENDIF.
************************************************* Semantics **************************************************
      APPEND LS_DATA TO T_DATA.
    ENDIF.
  ENDLOOP.
  CHECK T_DATA[] IS NOT INITIAL.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
