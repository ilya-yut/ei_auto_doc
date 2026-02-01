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
