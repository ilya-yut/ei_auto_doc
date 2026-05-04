### Default Values

- **PERIOD_CLOSING_DAY** - 15
- **BACKDAYS** - 10
- **DATE_REF_FLD** - CPUDT
- **DURATION_UNIT** - D
- **LANGU** - EN
- **DURATION** - initial - treated as empty range keeps rows by code

### Practical Example of Parameter Configuration

**Use Case 1: Company-wide prior-period posting scan**

**Purpose:** Keep month-end focused on all company codes while using the default creation-date reference and day-based aging.
```
BUKRS = 1000 - 1999
BACKDAYS = 14
DATE_REF_FLD = CPUDT
DURATION = 5 - 999999
DURATION_UNIT = D
```

**Use Case 2: Full-day age filter for high-risk accounts**

**Purpose:** Highlight only lines that are at least thirty full days old after the date window is applied.
```
HKONT = 200000 - 299999
BACKDAYS = 30
DURATION = 30
DURATION_UNIT = F
PERIOD_CLOSING_DAY = 25
```

**Use Case 3: Explicit close-week window**

**Purpose:** Anchor the run to a known reopening week instead of relative lookback alone.
```
DATUM = 20250325 - 20250331
BUKRS = 1000
BLART = SA - ZP
DURATION_UNIT = H
DURATION = 0 - 48
```

**Use Case 4: Vendor subledger slice with document-type control**

**Purpose:** Narrow to vendor account-type cluster paths while still applying language and posting-date filters.
```
KOART = K
BUDAT = 20250101 - 20250131
LANGU = EN
TCODE = FB60
```

**Use Case 5: Material document references and user accountability**

**Purpose:** Tie exceptions to external reference numbers and preparers for targeted follow-up.
```
XBLNR = INV2025*
USNAM = BATCH01 - BATCH99
CPUDT = 20250401 - 20250415
WRBTR = 10000 - 999999999
SW_DEST = PROD_FIN
```
