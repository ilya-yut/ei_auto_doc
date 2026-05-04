### Default Values

- **BACKDAYS** - initial - treated as 1 by code (SY-DATUM minus one day as the lower bound for the monitoring date range when neither explicit monitoring dates nor a populated back-day interval fill the range tables).
- **DURATION** - initial - treated as unconstrained by code (empty multivalued interval keeps every computed duration value until explicit bounds are supplied on the selection interface).
- **DURATION_UNIT** - initial - treated as D by code (day-based duration math runs before the single-value unit read completes unless the caller overrides the unit afterward).

### Practical Example of Parameter Configuration

**Use Case 1: Explicit calendar window with day-based aging**

**Purpose:** Anchor evaluation to a fiscal-year window and flag destinations unchanged for more than thirty full days.

```
DATUM = 20250101 - 20251231
DURATION = 30
DURATION_UNIT = F
RFCDEST = PRD_*
```

**Use Case 2: Relative lookback with UTC evaluation**

**Purpose:** Use default lookback while forcing UTC-aligned clocks for a productive naming slice.

```
BACKDAYS = 7
MANAGE_IN_UTC = X
RFCDEST = CENTRAL*
```

**Use Case 3: Changer-based slice with minute cap**

**Purpose:** Review destinations last touched by a service account with a short inactivity cap in minutes.

```
MUNAME = SVC_RFC*
DURATION = 360
DURATION_UNIT = M
RFCDEST = EXT_*
```

**Use Case 4: Full bundle for integration review**

**Purpose:** Combine destination, language, duration band, and explicit monitoring dates in one pass.

```
RFCDEST = PARTNER_A
LANGU = E
DATUM = 20250401 - 20250430
DURATION = 14
DURATION_UNIT = D
CUNAME = ADMIN01
```
