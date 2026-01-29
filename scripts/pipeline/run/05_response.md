### Parameter Relationships

**Time-Based and Duration Parameters:**
- **BACKDAYS** defines the lookback window in days from today; when no date range is supplied, the EI builds the selection range using SY-DATUM minus BACKDAYS.
- **DATE_REF_FLD** determines which date field (e.g. WADAT, LFDAT, ERDAT, AEDAT, KODAT) is used both for filtering the delivery date range and as the reference for **DURATION** calculation.
- **DURATION** and **DURATION_UNIT** work together: the EI computes the time difference between the reference date (from DATE_REF_FLD) and the current date in the selected unit (e.g. D = days); results are filtered by the DURATION range. Use BACKDAYS and DATE_REF_FLD to set the data window, then DURATION/DURATION_UNIT to focus on deliveries aged in status by a certain amount.

**Business Partner Parameters:**
- **BP1_FUNCT**, **BP2_FUNCT**, **BP3_FUNCT** define which partner functions (e.g. sold-to, ship-to, bill-to) are read from VBPA; **BP1_CODE**, **BP2_CODE**, **BP3_CODE** filter and populate the partner codes and **BP1_NAME**, **BP2_NAME**, **BP3_NAME** are enriched from customer master. Configure the three function/code/name pairs together to analyze deliveries by the desired partner dimensions.

**Status and Organizational Parameters:**
- Status parameters (e.g. WBSTK, FKSTK, LFSTA, KOSTK, CMGST) and organizational parameters (VKORG, VTWEG, SPART, VSTEL, VKBUR) are used together to narrow the result set to the relevant delivery statuses and organizational scope. Combine them according to the monitoring scenario (e.g. billing status + sales org, or goods movement status + distribution channel).