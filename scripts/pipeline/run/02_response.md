## Problem Description

Failure to monitor exceptional FI postings by G/L account creates multiple risks across financial reporting, operational management, and compliance.

**Financial and Reporting Issues**
- Unreviewed exceptional postings by G/L account can distort period-end financial statements and lead to misstated balances
- Unusual amounts or document types in specific accounts may indicate revenue or expense recognition errors or inappropriate journal entries
- High-value or concentrated postings in particular company codes or currencies may signal exposure or consolidation issues
- Delayed detection of exceptional postings can complicate month-end close and require restatements or adjustments
- Postings that deviate from expected patterns by document type or transaction code may indicate control bypass or manual override

**Operational and Control Risks**
- Lack of visibility into who posted which documents limits accountability and increases the risk of unauthorized or erroneous entries
- Exceptional postings by G/L account without review may hide process failures, misallocations, or duplicate postings
- Inability to focus on working days or specific date ranges can dilute attention and delay identification of true exceptions
- Postings that fall outside normal amount or duration patterns may indicate system or process issues requiring corrective action
- Absence of structured exception monitoring by G/L account and company code weakens internal controls over financial data

**Management Visibility and Decision-Making Risks**
- Executives and controllers lack a single view of exceptional postings for prioritization and resource allocation
- Unidentified concentration of postings in certain accounts, periods, or users can delay audit and compliance responses
- Without duration and date-based filtering, management cannot efficiently target aged or time-sensitive exceptions
- Limited ability to drill down to line-item detail (when needed) hinders root-cause analysis and remediation
- Inadequate monitoring of exceptional postings undermines confidence in financial data quality and control effectiveness

## Suggested Resolution

**Immediate Response**
- Review the FI documents and line items flagged by the EI to confirm whether postings are legitimate or require correction
- Verify high-value or unusual postings using transaction FB03 (Display Document) to check authorization and business justification
- Check document status and posting key to ensure no pending corrections or reversals are overlooked
- Identify the business context: recurring adjustments, period-end entries, corrections, or one-time events that explain the exception

**System Assessment**
- Analyze the selection dimensions (company code, document type, transaction code, date range) to understand what drives the exceptional pattern
- Compare current exception volumes and amounts to prior periods using the same criteria to spot trends
- Review currency and amount distributions to identify exposure or booking concentration
- Assess G/L account and chart of accounts usage to detect misallocations or master data issues
- Validate the date field used for the monitoring window (posting date, entry date, last change) so that the scope matches the control objective

**Corrective Actions**
- For erroneous or unauthorized postings, initiate correction or reversal via FB08 (Parking Document Reversal) or standard FI correction procedures as appropriate
- Escalate unexplained exceptional postings to accounting and internal audit for validation and documentation
- Update G/L account master or chart of accounts (FS00) if misclassification or account misuse is confirmed
- Tighten access or process controls for transaction codes or users associated with repeated exceptions
- Document findings and remediation for audit trail and management reporting
- Schedule recurring EI runs and route results to responsible accountants and controllers for continuous monitoring of exceptional postings by G/L account
