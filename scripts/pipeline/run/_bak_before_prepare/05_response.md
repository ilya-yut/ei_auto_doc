### Parameter Relationships

How parameter combinations work together

**DATUM** supplies an explicit monitoring-date range when you populate it, so the evaluation clock for duration calculations and any monitor-supplied date context is anchored to calendar bounds you choose instead of relying only on relative lookback.

When **DATUM** is not provided, **BACKDAYS** is the fallback that builds the lower monitoring date from the evaluation day backward for the date axis the online monitor uses before attribute rows are aged.

**DURATION** and **DURATION_UNIT** act as an additional filter after date-oriented selection: only destinations whose computed elapsed interval from last change timestamp to the evaluation moment still fit the configured duration band remain in the extract.

Both the date criteria (explicit **DATUM** or **BACKDAYS**-driven window) and the **DURATION** / **DURATION_UNIT** age test are applied together—rows must satisfy the date side and the duration side before the result set is considered final for alerting.

**MANAGE_IN_UTC** shifts whether the evaluation clock used with **DATUM** and duration math follows UTC semantics versus local application-server time, so calendar and duration results stay consistent with how your landscape runs the monitor.

**RFCDEST** ranges define which logical destinations enter the join; **CUNAME**, **CUDATE**, **MUNAME**, and **MUDATE** filters refine which attribute history rows are considered part of the same evaluation pass.

**LANGU** aligns description lookups and language-sensitive presentation with the monitor session when populated.
