### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the EI uses a 10-day lookback from today for the monitoring window).
- **DURATION_UNIT** — Default: `D` (duration is expressed in days when not supplied).
- **LAST_ONLY** — Default: initial (empty); when not supplied, the EI includes all release steps where the approver equals the creator (not only the last release per order).

**Note:** The reference date field used for the monitoring window and for duration calculation is set in the code to a default (e.g. PO date) when not supplied by the caller; other single-value parameters that are used when initial effectively default to "no restriction" where the code allows.

### Practical Configuration Examples

**Use Case 1: Last 10 days, creator = approver (default lookback)**

```
BACKDAYS = 10
```

**Purpose:** Monitor purchase orders where the creator is also the approver, for the last 10 days. Suitable for routine weekly or biweekly segregation-of-duties review.

**Use Case 2: By company code and purchasing organization**

```
BUKRS = 1000, 2000
EKORG = 1000, 2000
```

**Purpose:** Limit results to specific company codes and purchasing organizations. Supports regional or organizational control and delegation review.

**Use Case 3: Only last release per order**

```
LAST_ONLY = X
```

**Purpose:** Keep only the most recent release per order and flag it only if that last approver is the order creator. Reduces duplicate rows and focuses on the current release state.

**Use Case 4: Duration in full days (specific day filtering)**

```
DURATION_UNIT = F
DURATION = 0–30
```

**Purpose:** Express duration in full days and restrict to orders with duration between 0 and 30 full days since the reference date. Useful for age-based prioritization (e.g. recent approvals only).
