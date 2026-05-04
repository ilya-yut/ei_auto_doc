### EI Function Structure (Structure-Only View)

- Function module: `/SKN/F_SW_01_AUD_SM20_TR_START`
- Output structure: `/SKN/S_SW_01_AUD_SM20_TR_START`
- Purpose: collect and enrich SM20-based transaction-start events, calculate duration from event timestamp to current time, and return alert-ready records.

Core processing flow:
1. Read selection parameters (duration, unit, date/time, user, transaction constraints).
2. Build date windows and package intervals for data retrieval.
3. Call helper logic to retrieve SM20 transaction logs.
4. Apply transaction and user filters.
5. Enrich records with transaction text metadata.
6. Calculate `DURATION` using selected `DURATION_UNIT`.
7. Return filtered rows and set alert flag when at least one row remains.
