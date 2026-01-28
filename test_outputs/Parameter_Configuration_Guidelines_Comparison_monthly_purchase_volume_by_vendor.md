### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 28 parameters listed in the Parameters Reference Table above.

**ACCT_DESC** (Name):

Shows the customer or vendor name (from KNA1-NAME1 or LFA1-NAME1) for the account identified by ACCT_NUM. Populated automatically by the EI from customer or vendor master depending on DC_IND.

**ACCT_GRP** (Account group):

Account group filter. For customer mode (DC_IND = 'D') filters by customer account group from KNA1-KTOKD. For vendor mode (DC_IND = 'C') filters by vendor account group from LFA1-KTOKK. Use to focus monitoring on specific account group segments (e.g. one-time vendors, key accounts). Supports range selection.

**ACCT_NUM** (Account Number):

Customer or vendor number filter. For customer mode (DC_IND = 'D') filters by KNA1-KUNNR. For vendor mode (DC_IND = 'C') filters by LFA1-LIFNR. Use to analyze sales or purchase volume variance for specific customers or vendors. Supports range selection. The EI compares fiscal-period transaction figures (UM01U–UM12U) from KNC1 (customers) or LFC1 (vendors) between target and compare periods.

**BACKMONTHS** (Months Backwards):

Defines the target period by specifying how many months backward from the current month to use as the target. The EI sets the target date to the first day of that month (e.g. BACKMONTHS = 1 uses the first day of last month). Default: 1.

**BACKMONTHS and COMPMONTHS Connection:**
BACKMONTHS determines the target month (e.g. first day of that month). COMPMONTHS determines how many months back from the target month to use as the compare period (also aligned to first day of month). Together they define: target period = first day of (current month − BACKMONTHS); compare period = first day of (target month − COMPMONTHS). Fiscal year and period for both are derived per company code from the fiscal calendar.

**How They Work Together:**
- BACKMONTHS specifies which month is the "target" (e.g. 1 = last month, 3 = three months ago).
- COMPMONTHS specifies the comparison baseline (e.g. 12 = same month one year ago).
- The EI retrieves transaction figures (UMxxU) for target and compare fiscal year/period and calculates percent variance; PERC_VARI filters results by that variance threshold.

**BACKMONTHS Best Practices:**
- BACKMONTHS = 1, COMPMONTHS = 12: Compare last month to same month prior year (typical YoY variance).
- BACKMONTHS = 3, COMPMONTHS = 12: Compare quarter-ago month to same month prior year.
- BACKMONTHS = 0: Current month (fiscal period must be open for posting).

**Important:** BACKMONTHS and COMPMONTHS work with company-code-specific fiscal calendar; ensure target and compare dates yield valid GJAHR/MONAT for each BUKRS in scope.

**BUKRS** (Company Code):

Company code filter from KNB1-BUKRS (customers) or LFB1-BUKRS (vendors). Use to restrict analysis to specific company codes. Fiscal year and period for target and compare dates are determined per company code from the fiscal calendar. Supports range selection.

**COMPMONTHS** (Month to Compare (count from BACKMONTH)):

Defines the compare period by specifying how many months backward from the target month (determined by BACKMONTHS) to use. The compare date is set to the first day of that month. Default: 12 (compare to same month one year prior when BACKMONTHS = 1).

**COMPMONTHS and BACKMONTHS Connection:**
See BACKMONTHS entry. COMPMONTHS is applied from the target date: compare date = first day of (target month − COMPMONTHS).

**Important:** COMPMONTHS = 12 with BACKMONTHS = 1 gives year-over-year comparison (last month vs. same month previous year). Adjust COMPMONTHS for different comparison baselines (e.g. 6 for half-year comparison).

**COMP_CODE_DESC** (Company Name):

Shows the company code description (from T001). Populated automatically by the EI from the company code master (T001).

**DC_IND** (Debtor/Creditor Indicator):

Switches the EI between customer (debtor) and vendor (creditor) analysis. Determines which master and transaction tables are used (KNA1/KNB1/KNC1 for customers, LFA1/LFB1/LFC1 for vendors) and which block/deletion fields apply. Set DC_IND to match the dimension you want to monitor; all filter parameters and output fields are interpreted in that context.

**DC_IND Options:**
- **D**: Debtor (customer)
- **C**: Creditor (vendor)

**DURATION** (Duration In Time Units):

Defines an aging or time-window filter by time elapsed in the specified unit. Must be used together with DURATION_UNIT. The EI supports range selection for DURATION. For this EI, time scope is primarily driven by BACKMONTHS and COMPMONTHS (fiscal periods); use DURATION and DURATION_UNIT where the EI exposes them for additional time-based filtering.

**DURATION and DURATION_UNIT Connection:**
DURATION provides the numeric value and DURATION_UNIT specifies whether it is in days (D), hours (H), or minutes (M). Together they define the time window or aging threshold applied in the function.

**How They Work Together:**
- DURATION specifies the numeric threshold (e.g. 7, 30).
- DURATION_UNIT specifies the unit (D, H, M).
- Use both when the EI applies duration-based filtering; align unit to the business process (e.g. D for monthly comparison).

**Important:** For this EI, time scope is primarily driven by BACKMONTHS and COMPMONTHS (fiscal periods). Use DURATION/DURATION_UNIT where the EI exposes them for additional time-based filtering.

**DURATION_UNIT** (Duration Unit):

Specifies the time unit for the DURATION parameter. Must be used together with DURATION. For this EI, time scope is primarily driven by BACKMONTHS and COMPMONTHS (fiscal periods); use DURATION_UNIT where the EI uses DURATION for additional time-based filtering.

**DURATION_UNIT Options:**
- **D**: Days
- **H**: Hours
- **M**: Minutes

**GJAHR_CMP** (Fiscal Year):

Fiscal year of the compare period for the record, derived from the compare date and company code using the fiscal calendar. Used together with MONAT_CMP to identify the compare period.

**GJAHR_TGT** (Fiscal Year):

Fiscal year of the target period for the record, derived from the target date and company code using the fiscal calendar. Used together with MONAT_TGT to identify the target period.

**KNA1_CASSD** (Central sales block):

Central sales block filter for customer master (KNA1-CASSD). Used only when DC_IND = 'D'. Use to include or exclude customers with central sales block. Supports range selection (e.g. include only non-blocked).

**KNA1_LOEVM** (Central deletion flag):

Central deletion flag filter for customer master (KNA1-LOEVM). Used only when DC_IND = 'D'. Use to exclude deleted customers or focus on active only. Supports range selection.

**KNA1_SPERR** (Central posting block):

Central posting block filter for customer master (KNA1-SPERR). Used only when DC_IND = 'D'. Use to include or exclude customers with central posting block. Supports range selection.

**KNB1_LOEVM** (Deletion flag for company code):

Deletion flag for company code filter for customer company code data (KNB1-LOEVM). Used only when DC_IND = 'D'. Use to exclude customers deleted at company code level. Supports range selection.

**KNB1_SPERR** (Posting block for company code):

Posting block for company code filter for customer company code data (KNB1-SPERR). Used only when DC_IND = 'D'. Use to include or exclude customers with posting block at company code level. Supports range selection.

**LFA1_LOEVM** (Central deletion flag):

Central deletion flag filter for vendor master (LFA1-LOEVM). Used only when DC_IND = 'C'. Use to exclude deleted vendors or focus on active only. Supports range selection.

**LFA1_SPERM** (Central purchasing block):

Central purchasing block filter for vendor master (LFA1-SPERM). Used only when DC_IND = 'C'. Use to include or exclude vendors with central purchasing block. Supports range selection.

**LFA1_SPERR** (Central posting block):

Central posting block filter for vendor master (LFA1-SPERR). Used only when DC_IND = 'C'. Use to include or exclude vendors with central posting block. Supports range selection.

**LFB1_LOEVM** (Deletion flag for company code):

Deletion flag for company code filter for vendor company code data (LFB1-LOEVM). Used only when DC_IND = 'C'. Use to exclude vendors deleted at company code level. Supports range selection.

**LFB1_SPERR** (Posting block for company code):

Posting block for company code filter for vendor company code data (LFB1-SPERR). Used only when DC_IND = 'C'. Use to include or exclude vendors with posting block at company code level. Supports range selection.

**MONAT_CMP** (Posting period):

Fiscal period (posting period) of the compare period for the record, derived from the compare date and company code using the fiscal calendar. Identifies which period's transaction figures (UMxxU) are used as UMXXU_CMP.

**MONAT_TGT** (Posting period):

Fiscal period (posting period) of the target period for the record, derived from the target date and company code using the fiscal calendar. Identifies which period's transaction figures (UMxxU) are used as UMXXU_TGT.

**PERC_VARI** (Percent vari. of fiscal period tran vol.):

Percent variance threshold for filtering results. The EI calculates percent variance as (UMXXU_TGT / UMXXU_CMP * 100 − 100) when both amounts are non-zero. Use to flag only customers or vendors whose period-over-period variance exceeds (or falls within) the specified range. Supports range selection (e.g. below −20 or above 50 for significant decrease or increase). Single value or range from selection.

**Important:** PERC_VARI filters the result set after variance is calculated. Set a range (e.g. lower–upper) to include only exceptions that meet your reporting threshold.

**UMXXU_CMP** (Sales):

Transaction figure (sales/purchase volume) for the compare period, taken from the relevant UMxxU component (MONAT_CMP) of KNC1 (customers) or LFC1 (vendors). Represents the baseline period amount used in the variance calculation.

**UMXXU_TGT** (Sales):

Transaction figure (sales/purchase volume) for the target period, taken from the relevant UMxxU component (MONAT_TGT) of KNC1 (customers) or LFC1 (vendors). Represents the current period amount used in the variance calculation.

**WAERS** (Currency):

Company code (local) currency. Indicates the currency in which the company code (BUKRS) operates; this is the document/local currency for that company code.
