# Parameters: SKN_S_SW_01_AUD_SM20_TR_START

### Parameters Reference Table

| # | Parameter | Description |
|---|---|---|
| 1 | DURATION | Time difference between now and the recorded transaction timestamp, calculated in the selected duration unit. |
| 2 | DURATION_UNIT | Unit used to calculate and display `DURATION`. |
| 3 | INSTANCENAME | Application server instance where the transaction start was recorded. |
| 4 | MANDT | SAP client identifier. |
| 5 | MSCDATE | Date of the transaction start event. |
| 6 | MSCTIME | Time of the transaction start event. |
| 7 | TCODE | Transaction code captured by the SM20-based extraction logic. |
| 8 | TRN_BY | Transaction code context field returned by the source log dataset. |
| 9 | TRN_EX | Effective transaction code used for filtering and exclusion handling. |
| 10 | TTEXT | Language-dependent transaction text resolved from transaction metadata. |
| 11 | USER | User ID associated with the transaction start event. |
| 12 | USERID | User name selection/filter field used to limit users included in the result set. |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 12 parameters in alignment with your audit monitoring objective, system landscape, and investigation window.

**DURATION**
- Use this as the primary aging threshold to isolate recent or delayed transaction starts.
- Lower values highlight near-real-time activity; higher values broaden historical analysis.

**DURATION_UNIT**
- Choose the unit used by `DURATION` calculations and thresholds.

**DURATION_UNIT Options:**
- `H` = Hours
- `M` = Minutes
- `D` = Days
- `F` = Full days (calendar-day based filtering)

**INSTANCENAME**
- Restrict to specific application server instances when analysis is scoped to selected hosts.
- Keep unrestricted when you need a system-wide transaction start view.

**MANDT**
- Set the client scope explicitly for cross-client landscapes.
- In single-client deployments, keep the productive client value fixed to avoid mixed results.

**MSCDATE**
- Define the audit date window to match the period under review.
- Align this with operational calendar boundaries used by your security or compliance team.

**MSCTIME**
- Add time-of-day constraints when investigating unusual activity windows (for example, off-hours starts).
- Keep broad when daily-level monitoring is sufficient.

**TCODE**
- Use targeted transaction-code filters for focused investigations.
- Keep broader criteria for baseline monitoring and trend collection.

**TRN_BY**
- Apply when you need to distinguish by source transaction context returned from the extracted records.
- Use together with `TCODE`/`TRN_EX` to separate closely related execution patterns.

**TRN_EX**
- Use this field for effective transaction-code filtering after exclusion handling.
- Configure exclusion-sensitive scenarios here when standard `TCODE` filtering is too broad.

**TTEXT**
- Use for readable reporting outputs and analyst-friendly review screens.
- Ensure language settings are consistent with the audience using the report.

**USER**
- Restrict to specific user IDs for targeted investigations.
- Keep unrestricted for broad anomaly detection across all active users.

**USERID**
- Configure this as a user-name based selection layer to control which users are included.
- Combine with date/time filters to reduce noise during high-volume periods.


### Parameter Relationships

- `DURATION` and `DURATION_UNIT` are a coupled pair: threshold value and measurement unit must be configured together.
- `MSCDATE` and `MSCTIME` jointly define the transaction-start time window; using only one dimension can produce incomplete temporal filtering.
- `TCODE`, `TRN_BY`, and `TRN_EX` are complementary transaction selectors; combining them helps separate direct starts from excluded/derived transaction contexts.
- `USER` and `USERID` should be aligned to the same user population to avoid conflicting inclusion logic.
- `TTEXT` depends on transaction-code resolution and language context; keep language configuration consistent for comparable outputs.


### Default Values and Practical Examples

### Default Values

- `DURATION_UNIT = D` is a practical baseline for daily audit monitoring.
- If no explicit date is provided, a recent-day window is commonly used to keep volume manageable.
- Package/window slicing should match expected event volume to prevent oversized pulls.

### Practical Configuration Examples

Use Case 1: Daily monitoring for critical transactions

```ini
DURATION=1
DURATION_UNIT=D
MSCDATE=20260325-20260326
TCODE=SU01
USER=*
```

Use Case 2: Off-hours transaction start review

```ini
DURATION=12
DURATION_UNIT=H
MSCDATE=20260325-20260325
MSCTIME=200000-235959
TRN_EX=SE38
```

Use Case 3: Fast triage for a specific user set

```ini
DURATION=90
DURATION_UNIT=M
USERID=FIN_AUDITOR_01
TCODE=SM20
```


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
