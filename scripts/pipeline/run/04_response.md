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
