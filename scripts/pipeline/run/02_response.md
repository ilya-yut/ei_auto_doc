## Problem Description

Failure to monitor purchase orders approved by their creator creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unmonitored creator-approver same-user cases can indicate weak release controls affecting commitment accuracy and period-end accruals
- Purchase orders released by the creator without independent approval may lead to unauthorized commitments or duplicate payments if discovered late
- Lack of visibility into release timing and value at release can delay identification of control gaps affecting financial statement assertions
- Concentrated exceptions in specific company codes or purchasing organizations may signal systemic release-control weaknesses

**Procurement and Control Risks**
- Same user creating and approving orders violates segregation of duties and may enable fraud or error in the procure-to-pay process
- Unidentified release strategy or release-code patterns can mask missing or misconfigured approval workflows
- Absence of monitoring by vendor, plant, or document type limits ability to enforce approval policies and delegation rules
- High volume of creator-approver exceptions in specific organizational units may reflect process or training issues

**Management Visibility and Decision-Making Risks**
- Lack of consolidated visibility delays management awareness of segregation-of-duties exceptions requiring intervention
- Unmonitored release patterns by organizational dimension limit ability to assign accountability and optimize controls
- Missing link between change document approver and order creator hinders root-cause analysis and corrective action
- Absence of duration-based prioritization (e.g. time since creation or release) limits efficient allocation of review resources

## Suggested Resolution

**Immediate Response**
- Review the purchase orders flagged by the EI to confirm creator and approver user IDs and the release status and value at release
- Verify high-value or high-risk orders using the appropriate display transaction (e.g. ME23N) to confirm legitimacy and whether approval was appropriate
- Check release strategy and release code configuration for the affected document types and organizational units
- Identify business context: delegated approval, emergency procedures, or missing workflow configuration

**System Assessment**
- Analyze the monitoring window and date reference used for duration calculation to ensure the scope aligns with the control objective
- Compare exception volume and patterns to prior periods and to expected activity by company code, purchasing organization, and vendor
- Examine release group, release strategy, and processing status distribution to detect misconfiguration or policy gaps
- Validate that filters (company code, vendor, document type, release group) match the intended control scope

**Corrective Actions**
- Where segregation-of-duties violations are confirmed, escalate to procurement and management; reinforce approval workflows or delegation rules
- Update release strategy or release configuration (e.g. approval limits, release codes) where policy or design gaps are identified
- Adjust master data or organizational assignment where exceptions indicate training or process issues
- Document findings and business justifications for audit and management reporting
- Establish recurring EI runs and alert routing so that creator-approver exceptions are reviewed continuously by responsible roles
