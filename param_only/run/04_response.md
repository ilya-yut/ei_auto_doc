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
