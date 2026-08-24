### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as H by code
- **LANG** - initial - treated as E by code

### Practical Example of Parameter Configuration

**Use Case 1: Recent transported source with table updates**

**Purpose:** Scan last-day transports for database-changing statements in ABAP source.

```
BACKDAYS = 1
STRING_SEARCH = UPDATE
DURATION_UNIT = H
```

**Use Case 2: Released workbench requests**

**Purpose:** Review released workbench transports for a configured source pattern.

```
TRSTATUS = R
TRFUNCTION = K
STRING_SEARCH = INSERT
BACKDAYS = 7
```

**Use Case 3: Specific package and author**

**Purpose:** Scan source for one development package and responsible user.

```
ACTIVITY = ZFI*
AS4USER = DEVELOPER1
STRING_SEARCH = DELETE
BACKDAYS = 14
```

**Use Case 4: Function modules with duration cap**

**Purpose:** Flag matching function-module source that is still within a short age window in hours.

```
OBJECT = FUNC
STRING_SEARCH = MODIFY
DURATION = 24
DURATION_UNIT = H
```

**Use Case 5: Exactly seven full days from transport date**

**Purpose:** Return rows whose scope is exactly 7 full days ago when DURATION_UNIT = F and DURATION = 7.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 30
STRING_SEARCH = SELECT
```
