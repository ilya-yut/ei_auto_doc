### Default Values

- **BACKDAYS** — Default: `1` (used when no date range is supplied to build the selection from SY-DATUM minus BACKDAYS).
- **DATE_REF_FLD** — Default: `WADAT` (planned goods movement date used as reference for filtering and duration calculation when not supplied).
- **DURATION_UNIT** — Default: `D` (days; used for duration calculation when not supplied).
- **LANG** — Default: system language (SY-LANGU); used for description lookups (e.g. billing block, domain values).

**Note:** Other parameters default to initial (empty or 0) when not supplied; the EI then selects all values for that dimension (e.g. no filter on that field).

### Practical Configuration Examples

**Use Case 1: Deliveries in status longer than 7 days (by planned goods movement date)**
```
BACKDAYS = 30
DATE_REF_FLD = WADAT
DURATION = 7
DURATION_UNIT = D
```
**Purpose:** Focus on deliveries whose planned goods movement date is at least 7 days ago, within a 30-day lookback, to identify aged deliveries for follow-up.

**Use Case 2: Billing-blocked deliveries in a sales organization**
```
VKORG = 1010
FAKSK = (relevant billing block codes)
BACKDAYS = 14
DATE_REF_FLD = AEDAT
```
**Purpose:** Monitor deliveries with billing block in sales organization 1010 over the last 14 days by change date, for release or exception handling.

**Use Case 3: Partner-based view (ship-to and sold-to)**
```
BP1_FUNCT = WE
BP2_FUNCT = AG
BP1_CODE = (range of ship-to parties)
BP2_CODE = (range of sold-to parties)
WBSTK = (e.g. C for complete)
```
**Purpose:** Analyze delivery status by ship-to (WE) and sold-to (AG) partner and filter by total goods movement status.

**Use Case 4: Credit status and sales office**
```
CMGST = (e.g. credit block status)
VKBUR = 1000
DURATION = 1
DURATION_UNIT = D
```
**Purpose:** Identify deliveries with specific credit-check status in sales office 1000 and duration of at least 1 day for escalation.