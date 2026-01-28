### Default Values

- **BACKMONTHS** — Default: `1` (target month is first day of last month when not supplied; used to calculate target fiscal period).
- **COMPMONTHS** — Default: `12` (compare to same month one year prior when BACKMONTHS = 1; used to calculate compare fiscal period).
- **DC_IND** — Default: `D` (Debtor/customer; when not supplied, analysis uses customer master and KNA1/KNB1/KNC1. Use `C` for Creditor/vendor and LFA1/LFB1/LFC1).

**Note:** Target and compare dates are derived from system date: target = first day of the month BACKMONTHS months back; compare = first day of the month COMPMONTHS months back from target. So default BACKMONTHS = 1, COMPMONTHS = 12 compares last month to the same month in the prior year.

### Practical Configuration Examples

**Use Case 1: Vendor monthly volume comparison (last month vs same month prior year)**

```
BACKMONTHS = 1
COMPMONTHS = 12
DC_IND = C
BUKRS = (company code or range)
```

**Purpose:** Compares vendor purchase volume for last month to the same month one year ago. Identifies vendors with significant volume change. Use for vendor performance review and procurement analysis.

**Use Case 2: Customer volume by company code with custom comparison period**

```
BACKMONTHS = 2
COMPMONTHS = 6
DC_IND = D
BUKRS = (company code range)
ACCT_GRP = (account group filter if needed)
```

**Purpose:** Compares customer sales volume for two months back to the volume six months prior (same relative period). Use for sales trend analysis by company code and account group.

**Use Case 3: High variance vendor detection**

```
BACKMONTHS = 1
COMPMONTHS = 12
DC_IND = C
PERC_VARI = (e.g. 20-999 or -999 to -20 for threshold)
BUKRS = (company code)
```

**Purpose:** Restricts results to vendors whose month-over-same-month-last-year volume change exceeds a percentage threshold (e.g. >20% or <-20%). Use for exception reporting and follow-up on significant volume swings.

**Use Case 4: Multi-company code debtor comparison**

```
BACKMONTHS = 1
COMPMONTHS = 12
DC_IND = D
BUKRS = 1000, 2000, 3000
ACCT_NUM = (customer range optional)
```

**Purpose:** Compares customer sales volume across multiple company codes for last month vs same month prior year. Use for consolidated sales analysis and inter-company comparison.
