### Default Values

- **BACKDAYS** — Default: `1` (when REPETITIVE = 'X' and BACKDAYS is not supplied by the caller; used to derive the primary UDATE range in repetitive change mode). When REPETITIVE is not set and BACKDAYS is not supplied, BACKDAYS is effectively initial (0); date range is today only.
- **LANGU** — Default: system language (SY-LANGU) when not supplied; used for field and table descriptions (e.g. KEY1_DS–KEY10_DS, FIELD_DESC, TAB_DESC).
- **REPET_BACKDAYS** — Default: initial (0); when not supplied, the repetitive window lower bound is today (SY_DATLO - 0). Used only when REPETITIVE = 'X'.
- **REPETITIVE** — Default: initial (empty); standard (non-repetitive) change document mode when not supplied. Set to `X` for repetitive change analysis.
- **CONVERT_KEY** — Default: initial (empty); key decomposition off when not supplied. When REPETITIVE = 'X', the code forces key conversion on (CONVERT_KEY treated as 'X').
- **MANAGE_IN_UTC** — Default: initial (empty); use system local time when not supplied. Set to `X` to manage timestamps in UTC.

**Note:** When REPETITIVE = 'X' and no date range is passed for UDATE, the EI builds the range from today minus BACKDAYS (or 1 if BACKDAYS is initial). When REPETITIVE is not set, the date range is derived from today minus BACKDAYS; the caller may pass an explicit date range to override.

### Practical Configuration Examples

**Use Case 1: Recent vendor bank changes (last 7 days)**

```
BACKDAYS = 7
TABNAME = LFBK
OBJECTCLAS = LFBK
```

**Purpose:** Retrieves vendor bank (LFBK) change documents from the last 7 days. Suitable for daily or weekly monitoring of bank detail changes for audit or treasury review.

**Use Case 2: Repetitive change analysis over a 30-day window**

```
REPETITIVE = X
REPET_BACKDAYS = 30
BACKDAYS = 7
TABNAME = LFBK
```

**Purpose:** Runs in repetitive change mode: applies a 30-day window for grouping and comparison (REPET_BACKDAYS) and a 7-day primary date filter (BACKDAYS). Use when identifying repeated or related changes to the same vendor bank data over time.

**Use Case 3: Key decomposition for vendor bank audit**

```
CONVERT_KEY = X
TABNAME = LFBK
BACKDAYS = 14
OBJECTID = (vendor number or range)
```

**Purpose:** Decomposes the table key into KEY1–KEY10, KEY1_DS–KEY10_DS, and KEY1_V–KEY10_V for LFBK changes in the last 14 days. Use when you need to see which key components (e.g. vendor, bank, account) changed, with descriptions and old/new values, for audit or reconciliation.

**Use Case 4: Specific vendor and table with change date filter**

```
OBJECTCLAS = LFBK
OBJECTID = (single vendor or range)
TABNAME = LFBK
UDATE = (explicit date range, e.g. last 30 days)
FNAME = (field name filter if needed)
```

**Purpose:** Restricts change documents to a specific vendor (OBJECTID) and table (LFBK), with an explicit change date range. Use for focused review of one vendor’s bank master changes or for feeding downstream processes with a narrow scope.
