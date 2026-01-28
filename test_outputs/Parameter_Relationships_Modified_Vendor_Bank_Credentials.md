### Parameter Relationships

**Time and Repetitive Change Parameters:**

The following parameters control the time window and repetitive-change logic:

- **BACKDAYS** defines the lookback window in days from today (used to derive the lower bound for the change date range when no explicit date range is passed).
- **REPETITIVE** ('X' = repetitive change mode) switches between standard and repetitive change analysis. When set, **REPET_BACKDAYS** is used to build the date range for the repetitive window, and **UDATE_REPET** is aligned with that range; **BACKDAYS** (default 1 if initial) is used for the primary **UDATE** range.
- **UDATE** is the change date used in the CDHDR selection; when REPETITIVE = 'X', records are also filtered by UDATE within the repetitive logic.
- **UDATE_REPET** is used in repetitive mode to restrict which change records are considered (aligned with the REPET_BACKDAYS-derived range).

**Example Configuration:**
- REPETITIVE = 'X', REPET_BACKDAYS = 30, BACKDAYS = 7
- UDATE range derived from last 7 days; UDATE_REPET range from last 30 days

**Result:** Retrieves change documents in the last 7 days for the main date filter, with repetitive logic applied over a 30-day window for grouping and comparison.

**Object and Table Selection Parameters:**

These parameters identify the change document object and the tables to include:

- **OBJECTCLAS** (change document object class) and **OBJECTID** (e.g. vendor number) together identify the object in CDHDR/CDPOS. They are used in the main selection.
- **TABNAME** restricts which tables (e.g. LFBK, LFA1) are read from CDPOS. If **OBJECTCLAS** is not supplied, the code derives object class from TABNAME via TCDOB (one object class per table).

**Example Configuration:**
- OBJECTCLAS = 'LFBK', OBJECTID = &lt;vendor&gt;, TABNAME = 'LFBK'

**Result:** Change documents for the vendor bank object (LFBK) are read; only LFBK table changes are returned.

**Key Decomposition Parameters:**

When key fields are decomposed into separate output columns, these parameters work together:

- **CONVERT_KEY** ('X') turns on decomposition of the table key (TABKEY) into key field names, descriptions, and values. When **REPETITIVE** = 'X', key conversion is forced on.
- **TABNAME** determines which table’s key structure is used; the code resolves key components (DD03L / RPY) per TABNAME.
- **KEY1 - KEY10** hold the key field names (1–10) for the change document table.
- **KEY1_DS - KEY10_DS** hold the short descriptions for those key fields.
- **KEY1_V - KEY10_V** hold the key field values for the record; they are compared (old vs new) when building change rows in CONVERT_KEY_FIELDS.

**Example Configuration:**
- CONVERT_KEY = 'X', TABNAME = 'LFBK'

**Result:** Output includes KEY1–KEY10 (field names), KEY1_DS–KEY10_DS (descriptions), and KEY1_V–KEY10_V (values) for the LFBK key; insert/delete key rows are converted into update-style rows showing old vs new key component values.
