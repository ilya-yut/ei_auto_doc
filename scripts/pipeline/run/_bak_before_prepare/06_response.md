### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DATE_REF_FLD** - initial - treated as BUDAT by code
- **DURATION_UNIT** - initial - treated as D by code
- **DIFF_AMOUNT** - initial - treated as 0 by code
- **LANGU** - initial - treated as E by code
- **DURATION** - initial - treated as unconstrained by code

### Practical Example of Parameter Configuration

**Use Case 1: Document-level differences since yesterday**

**Purpose:** List inventory documents with differences aggregated at IBLNR level for postings since yesterday.

```
AGG_LVL = IBLNR
VGART = PI
BACKDAYS = 1
DATE_REF_FLD = BUDAT
DURATION_UNIT = D
```

**Use Case 2: Plant-level rollup**

**Purpose:** Find plants and posting dates where summed count differences exceed the default threshold band.

```
AGG_LVL = WERKS
WERKS = 1000
DIFF_AMOUNT = 100
DATE_REF_FLD = BUDAT
BACKDAYS = 7
```

**Use Case 3: Line-level with storage location**

**Purpose:** Return individual count item lines for one plant and storage location.

```
WERKS = 1000
LGORT = 0001
BACKDAYS = 30
```

**Use Case 4: Planned count date window**

**Purpose:** Monitor documents whose planned count date falls in the last fourteen days.

```
DATE_REF_FLD = GIDAT
BACKDAYS = 14
VGART = PI
WERKS = 2000
```

**Use Case 5: Posting date exactly seven full days ago**

**Purpose:** Flag count lines whose reference posting date falls in the scope of exactly 7 full days ago when using full-day duration counting.

```
DATE_REF_FLD = BUDAT
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 30
AGG_LVL = IBLNR
```
