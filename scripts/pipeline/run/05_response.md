### Parameter Relationships

How parameter combinations work together

**Explicit calendar window versus relative lookback:** **DATUM** supplies explicit from-and-to calendar bounds for the monitoring pass. When **DATUM** is empty, **BACKDAYS** (and optionally **FORWDAYS**) builds the calendar window relative to the evaluation day before documents are read.

**Reference date axis:** **DATE_REF_FLD** chooses which header date attribute is mapped into that calendar window for each generated period slice, so the same BACKDAYS span can follow creation, document, change, or update dates depending on configuration.

**Age filter after dates:** **DURATION** with **DURATION_UNIT** is an additional filter applied after date-oriented selection: each candidate line keeps its place in the result only when the computed age from the reference date and clock fields still fits the configured duration band.

**Fiscal period boundary:** **PERIOD_CLOSING_DAY** works with the generated date and posting-date tables to shape how fiscal periods are derived for the selection pass, which indirectly constrains which header lines qualify before line facts are merged.

**Remote execution path:** **SW_DEST** must be populated so the remote join runs in the monitored system; other organizational filters such as **BUKRS**, **HKONT**, or **KOART** only affect which documents are returned once connectivity is established.

**Final selection:** Both the date window logic (explicit **DATUM** or **BACKDAYS**/**FORWDAYS**) and the **DURATION**/**DURATION_UNIT** age test must be satisfied before a row is treated as part of the final exception population for alerting.
