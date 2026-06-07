### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 37 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**AGG_LVL** (Agg.Level)

Gives auditors traceable criteria because agg.level on AGG_LVL is applied consistently before any alert flag is raised.

**BACKDAYS** (Days Backward from today)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BLDAT** (Document Date)

Document date from the source business document, often used as legal/document reference date.

**BUDAT** (Posting Date)

Posting date used to align analysis with accounting period recognition.

**BUKRS** (BUKRS)

Company code key that scopes data to legal entity/accounting unit level.

**BWKEY** (BWKEY)

Valuation area key joining material valuation to plant/company rules for moving-average or standard price.

**COMP_OPERATOR** (Operator for comparison)

Comparison operator used to evaluate thresholds (equal, less-than, greater-than, etc.).

**DATE_REF_FLD** (Date Ref Field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- BLDAT — Document date from the source business document, often used as legal/document reference date.
- GIDAT — Goods-issue-related date on delivery or shipment data marking when goods physically left the plant.
- ZLDAT — Count date.
- BUDAT — Posting date used to align analysis with accounting period recognition.

**DATUM** (DATS)

Guards against oversized extracts when dats on DATUM is narrowed together with client, user, or session filters.

**DIFF_AMOUNT** (difference amount)

Captures edge cases where difference amount (DIFF_AMOUNT) must be non-default to reproduce a customer-specific monitoring scenario.

**DSTAT** (Adjustment status)

Dialog or processing-state code on workflow/work-item style rows describing runtime disposition.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**GIDAT** (Planned count date)

<mark>Goods-issue-related date on delivery or shipment data marking when goods physically left the plant.</mark>

**KTOPL** (KTOPL)

Chart of accounts governing GL account numbering, groups, and financial statement versions.

**LANGU** (Language)

Language key used for language-dependent texts and user-language filtering.

**LGORT** (Storage Location)

Storage location used to segment stock/logistics movements by warehouse sub-location.

**LSTAT** ("Delete" status)

Delivery header status summarizing goods-issue, picking, and billing-relevance milestones for the delivery.

**MANAGE_IN_UTC** (MANAGE_IN_UTC)

Framework flag: normalize evaluation timestamps to UTC before comparisons when systems span time zones.

**MANAGE_IN_UTC Options:**
- **X** — UTC mode for the relevant timestamp comparisons.
- Empty or blank — local time / framework default for the application server clock context.

**PRESENT_ZERO** ('X' - Present Zero)

<mark>Include-zero or zero-balance presentation flag controlling whether zero metrics appear in output.</mark>

**REF_FIELD1 - REF_FIELD2** (CHAR)

Allows phased rollout: first widen REF_FIELD1 for char, then tighten thresholds once baseline noise is understood.

**REF_TABNAME1 - REF_TABNAME2** (Table name(for REF.FIELD1))

Ensures reporting respects table name(for ref.field1) constraints carried by REF_TABNAME1.

**RESULT_COMP** (Value to Compare)

<mark>Right-hand comparison operand (literal or bound value) evaluated against extracted metrics in alert logic.</mark>

**SOBKZ** (Special Stock)

Special stock indicator used to distinguish stock ownership categories.

**SPERR** (Posting Block)

Blocking or lock indicator marking master data or transactions as administratively blocked from use.

**SW_DEST** (SW_DEST)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**USNAM** (Changed by(Item lvl))

SAP changed-by/created-by user field used for accountability filtering.

**USNAM_HD** (User name)

<mark>User name on header rows distinguishing header actor fields from item-level user attributes.</mark>

**VGART** (Trans./Event Type)

Transaction type on the material document header classifying the inventory posting category.

**WAERS** (Currency)

Currency key used for monetary field interpretation and filtering.

**WAERS_FR** (Foreign Currency)

Source/from currency key used in currency-change/translation contexts.

**WERKS** (Plant)

Plant key used to scope logistics/procurement records by site.

**XBUFI** (Freeze book invntory)

<mark>Buffer-related indicator on technical performance rows marking buffer involvement in the sample.</mark>

**ZLDAT** (Count date)

Supports operational control by evaluating count date through ZLDAT for each candidate record.

**ZSTAT** (Count status)

Gives auditors traceable criteria because count status on ZSTAT is applied consistently before any alert flag is raised.
