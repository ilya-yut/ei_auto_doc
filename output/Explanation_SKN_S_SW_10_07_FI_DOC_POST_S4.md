# Exception Indicator: FI documents posted to previous fiscal period (S/4HANA) ( SW_10_07_FI_DOC_POS_S4)

## General Overview

This Exception Indicator monitors Universal Journal (ACDOCA) activity together with classic financial document header context so organizations can detect postings whose fiscal timing conflicts with close policies on S/4HANA landscapes.

This EI serves as an essential control for finance and compliance by:
- Surfacing document and journal lines that warrant review when posting periods and business dates diverge
- Giving controllers visibility across company code, ledger, account, and operational dimensions without manual extracts
- Supporting reconciliation between line-item detail and header identifiers for escalation and audit evidence
- Enabling prioritization when amounts, currencies, or organizational slices cluster into unusual patterns
- Complementing period-end governance with repeatable, parameterized monitoring

Typical use includes month-end close, statutory reporting windows, and investigations after period reopenings. Results are meant to feed exception queues before final management sign-off.

The routine combines classic document header selection with ACDOCA-based line attributes and enriches results for review in the monitored system.


## Problem Description

Failure to monitor S/4HANA universal-journal postings that fall outside expected fiscal-period boundaries creates multiple risks across financial reporting, operational control, and audit readiness.

**Financial Reporting and Close Risks**
- Late or mis-period postings can distort profitability and balance-sheet views until manual discovery during close
- Consolidated views weaken when subsidiaries use different detection timing for the same policy breach
- Statutory packages may require rework when timing exceptions surface only after external review

**Operational and Data Risks**
- Purchasing, logistics, and revenue dimensions embedded in journal lines can hide process defects when not reviewed jointly with fiscal timing
- Master data inconsistencies across company code, plant, customer, or vendor attributes may accompany the same posting anomalies
- Manual sampling of ACDOCA volumes is impractical at enterprise scale without automated narrowing

**Management Visibility and Accountability Risks**
- Leadership lacks a single exception signal when prior-period postings concentrate by user, transaction, or document type
- Shared service centers cannot align priorities without a common filtered view of timing-sensitive lines

## Suggested Resolution

**Immediate Response**
- Review each flagged line for company code, document, ledger, and posting date context before changing live postings
- Validate intent with the preparer or service owner using the standard display transactions your policy allows
- Capture business justification when postings were authorized exceptions to the usual cut-off

**System Assessment**
- Compare this monitoring cycle to the prior one after period changes, job schedule updates, or major releases
- Examine concentrations by ledger, account, plant, or trading partner to see whether one process drives most items
- Revisit lookback and duration settings when false positives cluster at calendar boundaries

**Corrective Actions**
- Post corrections or reversals through your standard finance change workflow with required approvals
- Adjust monitoring parameters after root cause so the queue stays actionable
- Document outcomes for audit trails and route systemic defects into change management when configuration fixes are required


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
| 7 | BSTAT | Bstat | CHAR | 50 | 0 | BSTAT | BSTAT |
| 8 | BUDAT | Budat | CHAR | 50 | 0 | BUDAT | BUDAT |
| 9 | BUKRS | Bukrs | CHAR | 50 | 0 | BUKRS | BUKRS |
| 10 | CPUDT | Cpudt | CHAR | 50 | 0 | CPUDT | CPUDT |
| 11 | DATE_REF_FLD | Date Ref Fld | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |
| 12 | DATUM | Datum | CHAR | 50 | 0 | DATUM | DATUM |
| 13 | DOCLN | Docln | CHAR | 50 | 0 | DOCLN | DOCLN |
| 14 | DURATION | Duration | INT4 | 10 | 0 | DURATION | DURATION |
| 15 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | DURATION_UNIT | DURATION_UNIT |
| 16 | EBELN | Ebeln | CHAR | 50 | 0 | EBELN | EBELN |
| 17 | EBELP | Ebelp | CHAR | 50 | 0 | EBELP | EBELP |
| 18 | FKART | Fkart | CHAR | 50 | 0 | FKART | FKART |
| 19 | FORWDAYS | Forwdays | INT4 | 10 | 0 | FORWDAYS | FORWDAYS |
| 20 | GJAHR | Gjahr | CHAR | 50 | 0 | GJAHR | GJAHR |
| 21 | GRPID | Grpid | CHAR | 50 | 0 | GRPID | GRPID |
| 22 | HSL | Hsl | CHAR | 50 | 0 | HSL | HSL |
| 23 | HVKWRT | Hvkwrt | CHAR | 50 | 0 | HVKWRT | HVKWRT |
| 24 | KDGRP | Kdgrp | CHAR | 50 | 0 | KDGRP | KDGRP |
| 25 | KOART | Koart | CHAR | 50 | 0 | KOART | KOART |
| 26 | KOKRS | Kokrs | CHAR | 50 | 0 | KOKRS | KOKRS |
| 27 | KSL | Ksl | CHAR | 50 | 0 | KSL | KSL |
| 28 | KTOPL | Ktopl | CHAR | 50 | 0 | KTOPL | KTOPL |
| 29 | KTOSL | Ktosl | CHAR | 50 | 0 | KTOSL | KTOSL |
| 30 | KUNNR | Kunnr | CHAR | 50 | 0 | KUNNR | KUNNR |
| 31 | KURS2 | Kurs2 | CHAR | 50 | 0 | KURS2 | KURS2 |
| 32 | KURS3 | Kurs3 | CHAR | 50 | 0 | KURS3 | KURS3 |
| 33 | KURSF | Kursf | CHAR | 50 | 0 | KURSF | KURSF |
| 34 | LAND1 | Land1 | CHAR | 50 | 0 | LAND1 | LAND1 |
| 35 | LANGU | Langu | CHAR | 1 | 0 | LANGU | LANGU |
| 36 | LIFNR | Lifnr | CHAR | 50 | 0 | LIFNR | LIFNR |
| 37 | MATKL | Matkl | CHAR | 50 | 0 | MATKL | MATKL |
| 38 | MATNR | Matnr | CHAR | 50 | 0 | MATNR | MATNR |
| 39 | MONAT | Monat | CHAR | 50 | 0 | MONAT | MONAT |
| 40 | OSL | Osl | CHAR | 50 | 0 | OSL | OSL |
| 41 | PERIOD_CLOSING_DAY | Period Closing Day | NUMC | 2 | 0 | PERIOD_CLOSING_DAY | PERIOD_CLOSING_DAY |
| 42 | PRCTR | Prctr | CHAR | 50 | 0 | PRCTR | PRCTR |
| 43 | PSL | Psl | CHAR | 50 | 0 | PSL | PSL |
| 44 | RACCT | Racct | CHAR | 50 | 0 | RACCT | RACCT |
| 45 | RBUKRS | Rbukrs | CHAR | 50 | 0 | RBUKRS | RBUKRS |
| 46 | RBUSA | Rbusa | CHAR | 50 | 0 | RBUSA | RBUSA |
| 47 | RCNTR | Rcntr | CHAR | 50 | 0 | RCNTR | RCNTR |
| 48 | RFAREA | Rfarea | CHAR | 50 | 0 | RFAREA | RFAREA |
| 49 | RHCUR | Rhcur | CHAR | 50 | 0 | RHCUR | RHCUR |
| 50 | RKCUR | Rkcur | CHAR | 50 | 0 | RKCUR | RKCUR |
| 51 | RLDNR | Rldnr | CHAR | 50 | 0 | RLDNR | RLDNR |
| 52 | ROCUR | Rocur | CHAR | 50 | 0 | ROCUR | ROCUR |
| 53 | RTCUR | Rtcur | CHAR | 50 | 0 | RTCUR | RTCUR |
| 54 | RVCUR | Rvcur | CHAR | 50 | 0 | RVCUR | RVCUR |
| 55 | RWCUR | Rwcur | CHAR | 50 | 0 | RWCUR | RWCUR |
| 56 | SEGMENT | Segment | CHAR | 50 | 0 | SEGMENT | SEGMENT |
| 57 | SGTXT | Sgtxt | CHAR | 50 | 0 | SGTXT | SGTXT |
| 58 | SPART | Spart | CHAR | 50 | 0 | SPART | SPART |
| 59 | STBLG | Stblg | CHAR | 50 | 0 | STBLG | STBLG |
| 60 | SW_DEST | Sw Dest | CHAR | 32 | 0 | RFCDEST | RFCDEST |
| 61 | TCODE | Tcode | CHAR | 50 | 0 | TCODE | TCODE |
| 62 | TIME_REF_FLD | Time Ref Fld | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |
| 63 | TSL | Tsl | CHAR | 50 | 0 | TSL | TSL |
| 64 | UPDDT | Upddt | CHAR | 50 | 0 | UPDDT | UPDDT |
| 65 | USNAM | Usnam | CHAR | 50 | 0 | USNAM | USNAM |
| 66 | VKORG | Vkorg | CHAR | 50 | 0 | VKORG | VKORG |
| 67 | VSL | Vsl | CHAR | 50 | 0 | VSL | VSL |
| 68 | VTWEG | Vtweg | CHAR | 50 | 0 | VTWEG | VTWEG |
| 69 | WERKS | Werks | CHAR | 50 | 0 | WERKS | WERKS |
| 70 | WSL | Wsl | CHAR | 50 | 0 | WSL | WSL |
| 71 | WSL2 | Wsl2 | CHAR | 50 | 0 | WSL2 | WSL2 |
| 72 | WSL3 | Wsl3 | CHAR | 50 | 0 | WSL3 | WSL3 |
| 73 | XBLNR | Xblnr | CHAR | 50 | 0 | XBLNR | XBLNR |
| 74 | XREVERSAL | Xreversal | CHAR | 50 | 0 | XREVERSAL | XREVERSAL |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 74 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

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

**BSTAT** (Bstat)

Overall billing status on SD billing headers summarizing processing state versus cancellation or completion.

**BUDAT** (Budat)

Posting date used to align analysis with accounting period recognition.

**BUKRS** (Bukrs)

Company code key that scopes data to legal entity/accounting unit level.

**CPUDT** (Cpudt)

Entry/creation date used for technical posting timestamp filtering.

**DATE_REF_FLD** (Date Ref Fld)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- CPUDT — Entry/creation date used for technical posting timestamp filtering.
- BLDAT — Document date from the source business document, often used as legal/document reference date.
- AEDAT — Changed-on date used to filter documents or master records by last maintenance activity.
- UPDDT — Update date synonym on IDoc or change rows mirroring UPDDAT-style last-changed calendar stamps.

**DATUM** (Datum)

Pairs with duration logic: once DATUM passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**DOCLN** (Docln)

Ensures reporting respects docln constraints carried by DOCLN.

**DURATION** (Duration)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes (preset in code before the selection read when not overridden).
- **D** — Days.
- **F** — Full-day style counting where applicable to the duration helper.

**EBELN** (Ebeln)

Purchasing document number (typically PO) used as the primary MM document key.

**EBELP** (Ebelp)

Purchasing document item number used for line-level PO analytics.

**FKART** (Fkart)

Billing document type used to segment SD billing scenarios.

**FORWDAYS** (Forwdays)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

**GJAHR** (Gjahr)

Valuable when comparing health before and after a release—hold gjahr on GJAHR constant while varying other filters.

**GRPID** (Grpid)

<mark>Group id bundling related technical rows such as performance samples or application-log correlation keys.</mark>

**HSL** (Hsl)

Helps distinguish technical versus business attributes when hsl on HSL correlates with counters or status fields.

**HVKWRT** (Hvkwrt)

Gives auditors traceable criteria because hvkwrt on HVKWRT is applied consistently before any alert flag is raised.

**KDGRP** (Kdgrp)

Customer group key used for commercial segmentation in SD analysis.

**KOART** (Koart)

When combined with destination discipline, koart on KOART keeps both breadth and depth of the extract intentional.

**KOKRS** (Kokrs)

Controlling area key used for CO-level organizational scoping.

**KSL** (Ksl)

When harmonized with related filters, ksl on KSL isolates the highest-risk record families.

**KTOPL** (Ktopl)

Chart of accounts governing GL account numbering, groups, and financial statement versions.

**KTOSL** (Ktosl)

For operations, ktosl on KTOSL indicates whether a row belongs in the current monitoring pass versus historical noise.

**KUNNR** (Kunnr)

Customer account and is used to scope records to specific customers across SD/FI flows.

**KURS2 - KURS3** (Kurs2)

Captures edge cases where kurs2 (KURS2) must be non-default to reproduce a customer-specific monitoring scenario.

**KURSF** (Kursf)

Exchange rate used to convert foreign-currency amounts to local currency on the posting or pricing row.

**LAND1** (Land1)

Country key used for legal/geographic segmentation of business partners or plants.

**LANGU** (Langu)

Language key used for language-dependent texts and user-language filtering.

**LIFNR** (Lifnr)

Vendor account number used to scope records to supplier-specific flows.

**MATKL** (Matkl)

Material group key used for product-category segmentation in MM/SD analytics.

**MATNR** (Matnr)

Material number used as the primary product key across MM/SD records.

**MONAT** (Monat)

Posting period or calendar month bucket on FI/MM periodic aggregates for period-based reporting.

**OSL** (Osl)

When tightened, osl (OSL) removes rows that would otherwise dilute attention from failing or stuck cases.

**PERIOD_CLOSING_DAY** (Period Closing Day)

Aligns exception volume with the chosen scope by testing period closing day via PERIOD_CLOSING_DAY before alert evaluation.

**PRCTR** (Prctr)

Profit center used for management accounting segmentation and profitability reporting.

**PSL** (Psl)

Captures edge cases where psl (PSL) must be non-default to reproduce a customer-specific monitoring scenario.

**RACCT** (Racct)

Connects to alert semantics: rows removed for failing racct on RACCT never reach downstream filtering.

**RBUKRS** (Rbukrs)

Mirrors how administrators slice operational lists: rbukrs (RBUKRS) is one lever that shapes which rows are comparable run over run.

**RBUSA** (Rbusa)

Stabilizes week-over-week metrics by fixing rbusa (RBUSA) while allowing duration thresholds to move.

**RCNTR** (Rcntr)

Valuable when comparing health before and after a release—hold rcntr on RCNTR constant while varying other filters.

**RFAREA** (Rfarea)

Aligns exception volume with the chosen scope by testing rfarea via RFAREA before alert evaluation.

**RHCUR** (Rhcur)

Helps monitoring stay readable by requiring rhcur (RHCUR) to match organizational or technical selectors when set.

**RKCUR** (Rkcur)

Helps distinguish technical versus business attributes when rkcur on RKCUR correlates with counters or status fields.

**RLDNR** (Rldnr)

Pairs with duration logic: once RLDNR passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**ROCUR** (Rocur)

When combined with destination discipline, rocur on ROCUR keeps both breadth and depth of the extract intentional.

**RTCUR** (Rtcur)

Treats rtcur as a discriminator between similar rows that would otherwise look identical in a raw extract.

**RVCUR** (Rvcur)

Captures edge cases where rvcur (RVCUR) must be non-default to reproduce a customer-specific monitoring scenario.

**RWCUR** (Rwcur)

Supports operational control by evaluating rwcur through RWCUR for each candidate record.

**SEGMENT** (Segment)

Helps monitoring stay readable by requiring segment (SEGMENT) to match organizational or technical selectors when set.

**SGTXT** (Sgtxt)

Document line text used for context and free-text pattern filters.

**SPART** (Spart)

Division key used for SD product-line segmentation.

**STBLG** (Stblg)

Prevents accidental global scans when stblg (STBLG) is meant to stay within a controlled application slice.

**SW_DEST** (Sw Dest)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**TCODE** (Tcode)

SAP Transaction code

**TIME_REF_FLD** (Time Ref Fld)

Name of the time field used as the aging anchor-time analogue of DATE_REF_FLD for duration-from-reference logic.

**TIME_REF_FLD Options:**
- Use a time field from the same structure as DATE_REF_FLD or as defined in the EI code path.
- Values follow SAP time representation (typically HHMMSS semantics in the domain).

**TSL** (Tsl)

When harmonized with related filters, tsl on TSL isolates the highest-risk record families.

**UPDDT** (Upddt)

Update date synonym on IDoc or change rows mirroring UPDDAT-style last-changed calendar stamps.

**USNAM** (Usnam)

SAP changed-by/created-by user field used for accountability filtering.

**VKORG** (Vkorg)

Sales organization key used for legal/commercial SD scoping.

**VSL** (Vsl)

Reflects real administration where vsl on VSL is routinely restricted to a single productive client or object family.

**VTWEG** (Vtweg)

Distribution channel used for SD market/channel segmentation.

**WERKS** (Werks)

Plant key used to scope logistics/procurement records by site.

**WSL** (Wsl)

Stabilizes week-over-week metrics by fixing wsl (WSL) while allowing duration thresholds to move.

**WSL2 - WSL3** (Wsl2)

Separates cross-client noise from in-scope work when wsl2 on WSL2 correlates with client or user attributes.

**XBLNR** (Xblnr)

Reference document number used for external document matching and traceability.

**XREVERSAL** (Xreversal)

Documents expected operator behavior—xreversal on XREVERSAL should be set when that dimension is part of the control objective.


### Parameter Relationships

How parameter combinations work together

**Explicit calendar window versus relative lookback:** **DATUM** supplies explicit from-and-to calendar bounds for the monitoring pass. When **DATUM** is empty, **BACKDAYS** (and optionally **FORWDAYS**) builds the calendar window relative to the evaluation day before documents and journal lines are read.

**Reference date axis:** **DATE_REF_FLD** chooses which header date attribute is mapped into that calendar window for each generated period slice, so the same backward span can follow creation, document, change, or update dates depending on configuration.

**Age filter after dates:** **DURATION** with **DURATION_UNIT** is an additional filter applied after date-oriented selection: each candidate row remains only when the computed age from the reference date and clock fields still fits the configured duration band.

**Fiscal period boundary:** **PERIOD_CLOSING_DAY** works with the generated date and posting-date tables to shape how fiscal periods are derived for the selection pass before ACDOCA lines are evaluated.

**Remote execution path:** **SW_DEST** must be populated so the remote join runs in the monitored system; organizational filters such as **RBUKRS**, **RLDNR**, **RACCT**, or **PRCTR** only narrow results once connectivity is established.

**Final selection:** Both the date window logic (explicit **DATUM** or **BACKDAYS**/**FORWDAYS**) and the **DURATION**/**DURATION_UNIT** age test must be satisfied before a row is treated as part of the final exception population for alerting.


### Default Values

- **PERIOD_CLOSING_DAY** - 15
- **BACKDAYS** - 10
- **DATE_REF_FLD** - CPUDT
- **DURATION_UNIT** - D
- **LANGU** - EN
- **DURATION** - initial - treated as empty range keeps rows by code

### Practical Example of Parameter Configuration

**Use Case 1: Ledger-wide prior-period scan**

**Purpose:** Monitor a productive ledger with default creation-date reference and day-based aging across company codes.
```
RBUKRS = 1000 - 1999
RLDNR = 0L
BACKDAYS = 14
DATE_REF_FLD = CPUDT
DURATION = 5 - 999999
DURATION_UNIT = D
```

**Use Case 2: Account range with full-day age filter**

**Purpose:** Highlight high-materiality GL accounts where lines are at least thirty full days old after the date window.
```
RACCT = 500000 - 599999
BACKDAYS = 30
DURATION = 30
DURATION_UNIT = F
PERIOD_CLOSING_DAY = 25
```

**Use Case 3: Explicit close-week window**

**Purpose:** Anchor the run to a known reopening week instead of relative lookback alone.
```
DATUM = 20250325 - 20250331
RBUKRS = 1000
BLART = SA - ZP
DURATION_UNIT = H
DURATION = 0 - 48
```

**Use Case 4: Procurement-linked slice**

**Purpose:** Narrow to purchase document references while keeping language and posting-date filters.
```
EBELN = 45*
BUDAT = 20250101 - 20250131
LANGU = EN
TCODE = FB60
```

**Use Case 5: Operational dimensions and remote path**

**Purpose:** Combine plant, sales org, and destination for a targeted operational review.
```
WERKS = 1000
VKORG = 1000
VTWEG = 10
USNAM = BATCH*
SW_DEST = PROD_FIN
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | AEDAT | AEDAT | CHAR(50) | AEDAT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | BACKDAYS | BACKDAYS | INT4(10) | BACKDAYS |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | BELNR | BELNR | CHAR(50) | BELNR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | BKTXT | BKTXT | CHAR(50) | BKTXT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | BLART | BLART | CHAR(50) | BLART |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | BLDAT | BLDAT | CHAR(50) | BLDAT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | BSTAT | BSTAT | CHAR(50) | BSTAT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | BUDAT | BUDAT | CHAR(50) | BUDAT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | BUKRS | BUKRS | CHAR(50) | BUKRS |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | CPUDT | CPUDT | CHAR(50) | CPUDT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | DATE_REF_FLD | DATE_REF_FLD | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | DATUM | DATUM | CHAR(50) | DATUM |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | DOCLN | DOCLN | CHAR(50) | DOCLN |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | DURATION | DURATION | INT4(10) | DURATION |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | DURATION_UNIT | DURATION_UNIT | CHAR(1) | DURATION_UNIT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | EBELN | EBELN | CHAR(50) | EBELN |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | EBELP | EBELP | CHAR(50) | EBELP |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | FKART | FKART | CHAR(50) | FKART |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | FORWDAYS | FORWDAYS | INT4(10) | FORWDAYS |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | GJAHR | GJAHR | CHAR(50) | GJAHR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | GRPID | GRPID | CHAR(50) | GRPID |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | HSL | HSL | CHAR(50) | HSL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | HVKWRT | HVKWRT | CHAR(50) | HVKWRT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KDGRP | KDGRP | CHAR(50) | KDGRP |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KOART | KOART | CHAR(50) | KOART |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KOKRS | KOKRS | CHAR(50) | KOKRS |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KSL | KSL | CHAR(50) | KSL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KTOPL | KTOPL | CHAR(50) | KTOPL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KTOSL | KTOSL | CHAR(50) | KTOSL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KUNNR | KUNNR | CHAR(50) | KUNNR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KURS2 | KURS2 | CHAR(50) | KURS2 |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KURS3 | KURS3 | CHAR(50) | KURS3 |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | KURSF | KURSF | CHAR(50) | KURSF |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | LAND1 | LAND1 | CHAR(50) | LAND1 |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | LANGU | LANGU | CHAR(1) | LANGU |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | LIFNR | LIFNR | CHAR(50) | LIFNR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | MATKL | MATKL | CHAR(50) | MATKL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | MATNR | MATNR | CHAR(50) | MATNR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | MONAT | MONAT | CHAR(50) | MONAT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | OSL | OSL | CHAR(50) | OSL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | PERIOD_CLOSING_DAY | PERIOD_CLOSING_DAY | NUMC(2) | PERIOD_CLOSING_DAY |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | PRCTR | PRCTR | CHAR(50) | PRCTR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | PSL | PSL | CHAR(50) | PSL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RACCT | RACCT | CHAR(50) | RACCT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RBUKRS | RBUKRS | CHAR(50) | RBUKRS |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RBUSA | RBUSA | CHAR(50) | RBUSA |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RCNTR | RCNTR | CHAR(50) | RCNTR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RFAREA | RFAREA | CHAR(50) | RFAREA |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RHCUR | RHCUR | CHAR(50) | RHCUR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RKCUR | RKCUR | CHAR(50) | RKCUR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RLDNR | RLDNR | CHAR(50) | RLDNR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | ROCUR | ROCUR | CHAR(50) | ROCUR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RTCUR | RTCUR | CHAR(50) | RTCUR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RVCUR | RVCUR | CHAR(50) | RVCUR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | RWCUR | RWCUR | CHAR(50) | RWCUR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | SEGMENT | SEGMENT | CHAR(50) | SEGMENT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | SGTXT | SGTXT | CHAR(50) | SGTXT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | SPART | SPART | CHAR(50) | SPART |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | STBLG | STBLG | CHAR(50) | STBLG |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | TCODE | TCODE | CHAR(50) | TCODE |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | TIME_REF_FLD | TIME_REF_FLD | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | TSL | TSL | CHAR(50) | TSL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | UPDDT | UPDDT | CHAR(50) | UPDDT |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | USNAM | USNAM | CHAR(50) | USNAM |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | VKORG | VKORG | CHAR(50) | VKORG |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | VSL | VSL | CHAR(50) | VSL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | VTWEG | VTWEG | CHAR(50) | VTWEG |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | WERKS | WERKS | CHAR(50) | WERKS |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | WSL | WSL | CHAR(50) | WSL |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | WSL2 | WSL2 | CHAR(50) | WSL2 |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | WSL3 | WSL3 | CHAR(50) | WSL3 |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | XBLNR | XBLNR | CHAR(50) | XBLNR |
| /SKN/S_SW_10_07_FI_DOC_POST_S4 | XREVERSAL | XREVERSAL | CHAR(50) | XREVERSAL |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_10_07_FI_DOC_POS_S4.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_07_FI_DOC_POST_S4
*"----------------------------------------------------------------------
  DATA_MULTY: DATUM DATUM.
  DATA_MULTY: CPUDT CPUDT.
  DATA_MULTY: BUKRS BUKRS.
  DATA_MULTY: BELNR BELNR_D.
  DATA_MULTY: GJAHR GJAHR.
  DATA_MULTY: BLDAT BLDAT.
  DATA_MULTY: BUDAT BUDAT.
  DATA_MULTY: MONAT MONAT.
  DATA_MULTY: AEDAT AEDAT_BKPF.
  DATA_MULTY: UPDDT UPDDT.
  DATA_MULTY: SEGMENT	FB_SEGMENT.
  DATA_MULTY: USNAM USNAM.
  DATA_MULTY: TCODE TCODE.
  DATA_MULTY: XBLNR XBLNR1.
  DATA_MULTY: STBLG STBLG.
  DATA_MULTY: BKTXT BKTXT.
  DATA_MULTY: KURSF KURSF.
  DATA_MULTY: BSTAT BSTAT_D.
  DATA_MULTY: GRPID GRPID_BKPF.
  DATA_MULTY: XREVERSAL XREVERSAL.
  DATA_MULTY: KURS2 KURS2.
  DATA_MULTY: KURS3 KURS3.
* ACDOCA
  DATA_MULTY: RBUKRS BUKRS.
  DATA_MULTY: RLDNR  /SKN/E_MN_AN_RLDNR.
  DATA_MULTY: DOCLN  DOCLN6.
  DATA_MULTY: RCNTR  KOSTL.
  DATA_MULTY: PRCTR  PRCTR.
  DATA_MULTY: RFAREA FKBER.
  DATA_MULTY: RBUSA  GSBER.
  DATA_MULTY: SPART  SPART.
  DATA_MULTY: SGTXT  SGTXT.
  DATA_MULTY: KTOPL  KTOPL.
  DATA_MULTY: KOKRS	 KOKRS.
  DATA_MULTY: KTOSL	 KTOSL.
  DATA_MULTY: EBELN  EBELN.
  DATA_MULTY: EBELP  EBELP.
  DATA_MULTY: MATNR  MATNR.
  DATA_MULTY: WERKS  WERKS_D.
  DATA_MULTY: LIFNR  LIFNR.
  DATA_MULTY: KUNNR  KUNNR.
  DATA_MULTY: FKART  FKART.
  DATA_MULTY: VKORG  VKORG.
  DATA_MULTY: VTWEG  VTWEG.
  DATA_MULTY: RACCT  RACCT.
  DATA_MULTY: MATKL  MATKL.
  DATA_MULTY: KDGRP  KDGRP.
  DATA_MULTY: LAND1  LAND1_GP.
  DATA_MULTY: BLART  BLART.
  DATA_MULTY: HSL	   /SKN/E_MN_AN_HSL.
  DATA_MULTY: HVKWRT /SKN/E_MN_AN_HVKWRT.
  DATA_MULTY: KSL	   /SKN/E_MN_AN_KSL.
  DATA_MULTY: OSL  	 /SKN/E_MN_AN_OSL.
  DATA_MULTY: PSL	   /SKN/E_MN_AN_PSL.
  DATA_MULTY: TSL    /SKN/E_MN_AN_TSL.
  DATA_MULTY: VSL    /SKN/E_MN_AN_VSL.
  DATA_MULTY: WSL    /SKN/E_MN_AN_WSL.
  DATA_MULTY: WSL2   /SKN/E_MN_AN_WSL2.
  DATA_MULTY: WSL3   /SKN/E_MN_AN_WSL3.
  DATA_MULTY: RHCUR  /SKN/E_MN_AN_RHCUR.
  DATA_MULTY: RKCUR  /SKN/E_MN_AN_RKCUR.
  DATA_MULTY: ROCUR  /SKN/E_MN_AN_ROCUR.
  DATA_MULTY: RTCUR  /SKN/E_MN_AN_RTCUR.
  DATA_MULTY: RVCUR  /SKN/E_MN_AN_RVCUR.
  DATA_MULTY: RWCUR  /SKN/E_MN_AN_RWCUR.
  DATA_MULTY: KOART KOART.
  DATA_SINGLE: PERIOD_CLOSING_DAY NUMC2.
  LV_PERIOD_CLOSING_DAY = 15.  " Default value
  DATA_SINGLE: BACKDAYS /SKN/E_MN_AN_BACKDAYS.
  LV_BACKDAYS = '10'.          " Default value
  DATA_SINGLE: DATE_REF_FLD NAME_FELD.
  LV_DATE_REF_FLD = 'CPUDT'.   " Default value
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
  SELECT_MULTY:  BELNR.
  CONVERT_MULTY: BELNR ALPHA.
  SELECT_MULTY:  GJAHR.
  CONVERT_MULTY: GJAHR GJAHR.
  SELECT_MULTY: BLDAT.
  SELECT_MULTY: BUDAT.
  SELECT_MULTY: MONAT.
  SELECT_MULTY: AEDAT.
  SELECT_MULTY: UPDDT.
  SELECT_MULTY: USNAM.
  SELECT_MULTY: TCODE.
  SELECT_MULTY: XBLNR.
  SELECT_MULTY:  STBLG.
  CONVERT_MULTY: STBLG ALPHA.
  SELECT_MULTY: BKTXT.
  SELECT_MULTY: KURSF.
  SELECT_MULTY: BSTAT.
  SELECT_MULTY: GRPID.
  SELECT_MULTY: KURS2.
  SELECT_MULTY: KURS3.
  SELECT_MULTY: XREVERSAL.
* ACDOCA
  SELECT_MULTY: RBUKRS.
  SELECT_MULTY: RLDNR.
  SELECT_MULTY: DOCLN.
  SELECT_MULTY: RCNTR.
  SELECT_MULTY: PRCTR.
  SELECT_MULTY: RFAREA.
  SELECT_MULTY: RBUSA.
  SELECT_MULTY: SPART.
  SELECT_MULTY: SGTXT.
  SELECT_MULTY: KTOPL.
  SELECT_MULTY: KOKRS.
  SELECT_MULTY: KTOSL.
  SELECT_MULTY: EBELN.
  SELECT_MULTY: EBELP.
  SELECT_MULTY: MATNR.
  SELECT_MULTY: WERKS.
  SELECT_MULTY: LIFNR.
  SELECT_MULTY: KUNNR.
  SELECT_MULTY: FKART.
  SELECT_MULTY: VKORG.
  SELECT_MULTY: VTWEG.
  SELECT_MULTY: RACCT.
  SELECT_MULTY: MATKL.
  SELECT_MULTY: KDGRP.
  SELECT_MULTY: LAND1.
  SELECT_MULTY: BLART.
  SELECT_MULTY: HSL.
  SELECT_MULTY: HVKWRT.
  SELECT_MULTY: KSL.
  SELECT_MULTY: OSL.
  SELECT_MULTY: PSL.
  SELECT_MULTY: TSL.
  SELECT_MULTY: VSL.
  SELECT_MULTY: WSL.
  SELECT_MULTY: WSL2.
  SELECT_MULTY: WSL3.
  SELECT_MULTY: RHCUR.
  SELECT_MULTY: RKCUR.
  SELECT_MULTY: ROCUR.
  SELECT_MULTY: RTCUR.
  SELECT_MULTY: RVCUR.
  SELECT_MULTY: RWCUR.
  SELECT_MULTY: KOART.
  SELECT_SINGLE: PERIOD_CLOSING_DAY.
  SELECT_SINGLE: BACKDAYS.
  SELECT_SINGLE: FORWDAYS.
  SELECT_SINGLE: DATE_REF_FLD.
  SELECT_SINGLE: TIME_REF_FLD.
  SELECT_SINGLE: DURATION_UNIT.
  SELECT_MULTY:  DURATION.
  SELECT_SINGLE:  LANGU.
  CONVERT_SINGLE: LANGU ISOLA.
  IF R_KOART IS INITIAL.
    RS_KOART-LOW = 'S'.
    APPEND RS_KOART TO R_KOART.
  ENDIF.
  DATA: LV_SW_DEST  TYPE RFCDEST,
        LV_TABIX    TYPE I,
        LV_TAB      TYPE DDOBJNAME,
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
        LS_DATA              TYPE /SKN/S_SW_10_07_FI_DOC_POST_S4,
        LS_X030L             TYPE	X030L,
        LWA_ALL_ENTRIES_TAB  TYPE /SKN/S_SW_TAB6000,
        LWA_ALL_ENTRIES_COND TYPE /SKN/S_TABLE_JOIN.
  DATA: LT_OPTIONS      TYPE TABLE OF RFC_DB_OPT,
        LT_OPTIONS_MAIN TYPE TABLE OF RFC_DB_OPT,
        LT_DFIES_FLD    TYPE TABLE OF DFIES.
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
  FIELD-SYMBOLS: <FS_DATA> TYPE /SKN/S_SW_10_07_FI_DOC_POST_S4,
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
  LV_TAB = 'ACDOCA'.
* Check table exist on the client's system
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    DESTINATION LV_SW_DEST
    EXPORTING
      TABNAME        = LV_TAB
    IMPORTING
      X030L_WA       = LS_X030L
    TABLES
      DFIES_TAB      = LT_DFIES_FLD
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  CHECK SY-SUBRC IS INITIAL.
  CHECK LT_DFIES_FLD IS NOT INITIAL.
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
  _APPEND_TABLES_LIST 'BKPF'   '' 'A'.
  _APPEND_TABLES_LIST 'ACDOCA' '' 'B'.
  _APPEND_TABLES_LIST 'T001' 'X' 'T'.
  REFRESH LT_SEL_FIELDS[].
  _ADAPT_SEL_FIELDS 'BKPF'   '/SKN/S_SW_10_07_FI_DOC_POST_S4'  LT_SEL_FIELDS LV_SW_DEST.
  _ADAPT_SEL_FIELDS 'ACDOCA' '/SKN/S_SW_10_07_FI_DOC_POST_S4'  LT_SEL_FIELDS LV_SW_DEST.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'ACDOCA'.
  LS_SEL_FIELDS-FIELD = 'BUDAT'.
  LS_SEL_FIELDS-ALIAS = 'BUDAT_ACDOCA'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'T001'.
  LS_SEL_FIELDS-FIELD = 'KTOPL'.
  APPEND LS_SEL_FIELDS TO LT_SEL_FIELDS.
* BKPF
  _RANGE_TO_SEL_TABLE 'A~BUKRS'     BUKRS.
  _RANGE_TO_SEL_TABLE 'A~BELNR'     BELNR.
  _RANGE_TO_SEL_TABLE 'A~GJAHR'     GJAHR.
  _RANGE_TO_SEL_TABLE 'A~MONAT'     MONAT.
  _RANGE_TO_SEL_TABLE 'A~TCODE'     TCODE.
  _RANGE_TO_SEL_TABLE 'A~USNAM'     USNAM.
  _RANGE_TO_SEL_TABLE 'A~TCODE'     TCODE.
  _RANGE_TO_SEL_TABLE 'A~XBLNR'     XBLNR.
  _RANGE_TO_SEL_TABLE 'A~XREVERSAL' XREVERSAL.
  _RANGE_TO_SEL_TABLE 'A~BSTAT'     BSTAT.
  _RANGE_TO_SEL_TABLE 'A~GRPID'     GRPID.
  _RANGE_TO_SEL_TABLE 'A~STBLG'     STBLG.
  _RANGE_TO_SEL_TABLE 'A~BKTXT'     BKTXT.
* ACDOCA
  _RANGE_TO_SEL_TABLE 'B~RBUKRS'  RBUKRS.
  _RANGE_TO_SEL_TABLE 'B~RLDNR'   RLDNR.
  _RANGE_TO_SEL_TABLE 'B~DOCLN'   DOCLN.
  _RANGE_TO_SEL_TABLE 'B~RCNTR'   RCNTR.
  _RANGE_TO_SEL_TABLE 'B~PRCTR'   PRCTR.
  _RANGE_TO_SEL_TABLE 'B~RACCT'   RACCT.
  _RANGE_TO_SEL_TABLE 'B~RFAREA'  RFAREA.
  _RANGE_TO_SEL_TABLE 'B~RBUSA'   RBUSA.
  _RANGE_TO_SEL_TABLE 'B~KOKRS'   KOKRS.
  _RANGE_TO_SEL_TABLE 'B~SEGMENT' SEGMENT.
  _RANGE_TO_SEL_TABLE 'B~KTOSL'   KTOSL.
  _RANGE_TO_SEL_TABLE 'B~EBELN'   EBELN.
  _RANGE_TO_SEL_TABLE 'B~EBELP'   EBELP.
  _RANGE_TO_SEL_TABLE 'B~MATNR'   MATNR.
  _RANGE_TO_SEL_TABLE 'B~WERKS'   WERKS.
  _RANGE_TO_SEL_TABLE 'B~LIFNR'   LIFNR.
  _RANGE_TO_SEL_TABLE 'B~KUNNR'   KUNNR.
  _RANGE_TO_SEL_TABLE 'B~FKART'   FKART.
  _RANGE_TO_SEL_TABLE 'B~VKORG'   VKORG.
  _RANGE_TO_SEL_TABLE 'B~VTWEG'   VTWEG.
  _RANGE_TO_SEL_TABLE 'B~SPART'   SPART.
  _RANGE_TO_SEL_TABLE 'B~MATKL'   MATKL.
  _RANGE_TO_SEL_TABLE 'B~KDGRP'   KDGRP.
  _RANGE_TO_SEL_TABLE 'B~LAND1'   LAND1.
  _RANGE_TO_SEL_TABLE 'B~BLART'   BLART.
  _RANGE_TO_SEL_TABLE 'B~HSL'     HSL.
  _RANGE_TO_SEL_TABLE 'B~HVKWRT'  HVKWRT.
  _RANGE_TO_SEL_TABLE 'B~KSL'     KSL.
  _RANGE_TO_SEL_TABLE 'B~OSL'     OSL.
  _RANGE_TO_SEL_TABLE 'B~PSL'     PSL.
  _RANGE_TO_SEL_TABLE 'B~TSL'     TSL.
  _RANGE_TO_SEL_TABLE 'B~VSL'     VSL.
  _RANGE_TO_SEL_TABLE 'B~WSL'     WSL.
  _RANGE_TO_SEL_TABLE 'B~WSL2'    WSL2.
  _RANGE_TO_SEL_TABLE 'B~WSL3'    WSL3.
  _RANGE_TO_SEL_TABLE 'B~RTCUR'  RTCUR.
  _RANGE_TO_SEL_TABLE 'B~RWCUR'  RWCUR.
  _RANGE_TO_SEL_TABLE 'B~RHCUR'  RHCUR.
  _RANGE_TO_SEL_TABLE 'B~RKCUR'  RKCUR.
  _RANGE_TO_SEL_TABLE 'B~ROCUR'  ROCUR.
  _RANGE_TO_SEL_TABLE 'B~RVCUR'  RVCUR.
  _RANGE_TO_SEL_TABLE 'B~KOART'  KOART.
* T001
  _RANGE_TO_SEL_TABLE 'T~KTOPL'  KTOPL.
  LT_OPTIONS_MAIN[] = LT_OUT_WHERE_COND[].
  LOOP AT R_DATUM INTO RS_DATUM.
    LV_TABIX = SY-TABIX.
    REFRESH: R_CPUDT, R_AEDAT, R_UPDDT, R_BLDAT, R_BUDAT.
    CLEAR: LT_OPTIONS, LT_OUT_WHERE_COND[].
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
    _RANGE_TO_SEL_TABLE 'B~BUDAT' BUDAT.
    LT_OPTIONS[] = LT_OUT_WHERE_COND[].
    IF LT_OPTIONS[]      IS NOT INITIAL AND
       LT_OPTIONS_MAIN[] IS NOT INITIAL.
      LS_OPTION-TEXT = 'AND'.
      APPEND LS_OPTION TO LT_OPTIONS.
    ENDIF.
    APPEND LINES OF LT_OPTIONS_MAIN TO LT_OPTIONS.
* Join condition
    REFRESH LT_JOIN_CONDITION[].
    _JOIN_CONDITION 'A' 'BUKRS' 'B' 'RBUKRS'.
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
    CLEAR: LT_DATA_TMP.
  ENDLOOP.
  IF LT_DATA IS NOT INITIAL.
    SORT LT_DATA BY RBUKRS GJAHR BELNR DOCLN.
    DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING RBUKRS GJAHR BELNR DOCLN.
  ENDIF.
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
    IF LS_DATA-RBUKRS IS NOT INITIAL.
* Get Company code desc.
      CALL FUNCTION '/SKN/F_SW_10_COMP_CODE_DESC'
        EXPORTING
          BUKRS          = LS_DATA-RBUKRS
          SW_DEST        = LV_SW_DEST
        IMPORTING
          COMP_CODE_DESC = LS_DATA-RBUKRS_DESC
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
    ENDIF.
    IF LS_DATA-SPART IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_DIVISION_DESC'
        EXPORTING
          SPART      = LS_DATA-SPART
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          DIV_DESC   = LS_DATA-SPART_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
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
    IF LS_DATA-RACCT IS NOT INITIAL AND
       LS_DATA-KTOPL IS NOT INITIAL.
* Get GL Account Desc.
      CALL FUNCTION '/SKN/FC_SW_10_GL_DESC'
        EXPORTING
          KTOPL      = LS_DATA-KTOPL      " Chart of Accounts
          SAKNR      = LS_DATA-RACCT      " G/L Account Number
          LANGU      = LV_LANGU           " Language Key
          SW_DEST    = LV_SW_DEST         " Logical Destination (Specified in Function Call)
        IMPORTING
*         TXT20      =                    " G/L account short text
          TXT50      = LS_DATA-RACCT_DESC " G/L Account Long Text
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
      IF SY-SUBRC EQ 0.
      ENDIF.
    ENDIF.
    IF LS_DATA-VKORG IS NOT INITIAL.
* Get Sales Org. description
      CALL FUNCTION '/SKN/F_SW_10_SALES_ORG_DESC'
        EXPORTING
          VKORG          = LS_DATA-VKORG
          LANGU          = LV_LANGU
          SW_DEST        = LV_SW_DEST
        IMPORTING
          SALES_ORG_DESC = LS_DATA-VKORG_DESC
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
    ENDIF.
    IF LS_DATA-VTWEG_DESC IS NOT INITIAL.
* Get Distr.Channel description
      CALL FUNCTION '/SKN/F_SW_10_DISTR_CHAN_DESC'
        EXPORTING
          VTWEG           = LS_DATA-VTWEG
          LANGU           = LV_LANGU
          SW_DEST         = LV_SW_DEST
        IMPORTING
          DISTR_CHAN_DESC = LS_DATA-VTWEG_DESC
        EXCEPTIONS
          WRONG_CODE      = 1
          OTHERS          = 2.
    ENDIF.
    IF LS_DATA-MATKL IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
        EXPORTING
          MATKL      = LS_DATA-MATKL
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          MATKL_DESC = LS_DATA-MATKL_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF LS_DATA-WERKS IS NOT INITIAL.
* Get Plant description
      CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
        EXPORTING
          WERKS      = LS_DATA-WERKS
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          PLANT_DESC = LS_DATA-WERKS_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF LS_DATA-MATNR IS NOT INITIAL.
    ENDIF.
    IF LS_DATA-PRCTR IS NOT INITIAL AND
       LS_DATA-KOKRS IS NOT INITIAL.
* Get Profit Center description
      CALL FUNCTION '/SKN/F_SW_10_PRCTR_DESC'
        EXPORTING
          SPRAS      = LV_LANGU
          PRCTR      = LS_DATA-PRCTR
          KOKRS      = LS_DATA-KOKRS
          SW_DEST    = LV_SW_DEST
        IMPORTING
          KTEXT      = LS_DATA-PRCTR_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF LS_DATA-LIFNR IS NOT INITIAL.
* Get Vendor description
      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
        EXPORTING
          LIFNR        = LS_DATA-LIFNR
          SW_DEST      = LV_SW_DEST
        IMPORTING
          VENDOR_DESC  = LS_DATA-LIFNR_DESC
        EXCEPTIONS
          WRONG_VENDOR = 1
          OTHERS       = 2.
    ENDIF.
    IF LS_DATA-LAND1 IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_COUNTRY_DESC'
        EXPORTING
          LAND1      = LS_DATA-LAND1
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          LANDX      = LS_DATA-LAND1_DESC
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF LS_DATA-KUNNR IS NOT INITIAL.
      CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
        EXPORTING
          KUNNR          = LS_DATA-KUNNR
          SW_DEST        = LV_SW_DEST
        IMPORTING
          CUST_DESC      = LS_DATA-KUNNR_DESC
*         LAND1          =
        EXCEPTIONS
          WRONG_CUSTOMER = 1
          OTHERS         = 2.
    ENDIF.
************************************************* Semantics **************************************************
    APPEND LS_DATA TO T_DATA.
  ENDLOOP.
  CHECK T_DATA[] IS NOT INITIAL.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
