## Problem Description

Failure to monitor the comparison of monthly sales and purchase volume by customer or vendor between fiscal periods creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected large volume variances by customer or vendor can distort period-over-period trend analysis and forecasting, especially when percent variance exceeds thresholds without review
- Exceptional volume increases or decreases in specific customers or vendors may indicate revenue or cost recognition timing issues, one-off spikes, or underlying data errors
- Unusual volume patterns by company code or account group can signal misallocated activity or consolidation issues affecting month-end close
- Late discovery of material volume variances may delay reserve postings, management reporting, and audit explanations
- Concentrated volume swings in certain customers or vendors can mask broader trend or process problems in sales or procurement

**Operational and Control Risks**
- High volume variances by customer or vendor without visibility may point to unresolved disputes, contract changes, master data errors, or unauthorized activity
- Customer- or vendor-level concentration in volume swings can signal relationship or process issues requiring contract or pricing review
- Unusual patterns by company code may indicate incorrect account assignment, duplicate postings, or integration errors requiring correction
- Spikes or drops in transaction volume by account group may reflect returns, rebates, procurement changes, or operational issues that need immediate follow-up
- Atypical percent variance patterns may indicate data quality issues in transaction figures (KNC1/LFC1) or fiscal period logic requiring correction

**Management Visibility and Decision-Making Risks**
- Lack of period-over-period volume comparison by customer/vendor delays awareness of significant business shifts and concentration risk
- Unidentified volume variance patterns can lead to missed opportunities for pricing, sourcing, or customer/vendor management actions
- Material variances may require additional audit or compliance review but remain invisible without targeted monitoring
- Absence of customer/vendor-level volume comparison limits the ability to prioritize collections, procurement, and exception management

---

## Suggested Resolution

**Immediate Response**
- Review the customer or vendor records flagged by the EI to see which accounts exceeded the percent variance threshold and over which target vs. comparison periods
- Verify high-variance accounts using standard customer or vendor display (e.g. XD03 for customer, XK03 for vendor) to confirm master data and company code assignment
- Check that target and comparison fiscal periods (BACKMONTHS, COMPMONTHS) and company code filters align with the intended analysis scope
- Clarify business context: planned contract changes, seasonal patterns, one-off orders, or possible data or process errors in transaction figures

**System Assessment**
- Analyze parameter choices (BACKMONTHS, COMPMONTHS, DC_IND, PERC_VARI) to see how the target and comparison periods and variance threshold drive the exceptions
- Compare current volume variances by customer/vendor and company code to prior runs using the same criteria to spot trends or one-off spikes
- Review account group and account number filters to ensure the right population (customers vs. vendors, company codes) is in scope
- Validate fiscal year and period derivation (company-code-dependent) to ensure target and comparison months are correct
- Confirm master data filters (deletion flag, posting block, sales/purchasing block) so that only relevant accounts are included and block status has not changed

**Corrective Actions**
- Correct or explain erroneous transaction figures via the appropriate FI/SD/MM transactions if data quality issues are confirmed
- Escalate large but legitimate volume variances to sales, procurement, or finance for approval and follow-up (contracts, pricing, reserves)
- Update customer or vendor master (VD02, XK02) and company code data if assignment or block errors drive repeated exceptions
- Adjust BACKMONTHS, COMPMONTHS, or PERC_VARI so that the comparison window and variance threshold align with policy (e.g. month-over-month or year-over-year, acceptable variance band)
- Document findings and approvals for exceptions to support audit and management reporting
- Run the EI on a recurring schedule (e.g. monthly) to monitor sales and purchase volume by customer/vendor and period continuously
- Use recipient or routing parameters to route alerts to responsible sales, procurement, or finance teams by company code or organizational unit
