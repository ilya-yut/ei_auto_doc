## General Overview

This Exception Indicator (EI) monitors comparison of monthly sales or purchase volume by customer or vendor between two fiscal periods (a target period and a comparison period). It identifies accounts whose transaction volume varies beyond a configurable percentage threshold, so that significant volume increases or decreases at customer or vendor level can be reviewed for month-end close, procurement, and sales oversight.

This EI serves as an essential control for financial and procurement oversight by:
- Comparing fiscal period transaction volumes (target month vs. comparison month) by customer or vendor and by company code
- Identifying accounts whose volume variance exceeds a user-defined percentage threshold (percent variance of fiscal period transaction volumes)
- Supporting both debtor (customer) and creditor (vendor) analysis via a single indicator, with filtering by company code, account group, and account number
- Enabling configurable lookback (months backwards) and comparison offset (months to compare) so that current period can be compared to prior month or prior year
- Filtering by master data status (e.g. central deletion flag, posting block, sales/purchasing block) so that only relevant accounts are included

This monitoring enables organizations to detect unusual customer or vendor volume swings, support period-over-period reconciliation, and prioritize follow-up on accounts with material variances. The EI is particularly valuable for month-end close, procurement and sales performance reviews, and exception management of customer and vendor transaction trends.

The EI reads account master data from SAP tables (KNA1 - Customer Master, KNB1 - Customer Company Code for customers; LFA1 - Vendor Master, LFB1 - Vendor Company Code for vendors) and company code data from T001, then retrieves posting period transaction figures from KNC1 (customer) or LFC1 (vendor). It calculates target and comparison period amounts from the posting period fields (e.g. period amounts per fiscal year), computes percent variance between the two periods, and returns only those accounts whose variance exceeds the user-specified threshold.
