## General Overview

This Exception Indicator (EI) monitors purchase orders that are subject to release and flags those where the same user who created the order also performed the release (approval). It combines purchase order header data with change document data to compare creator and approver, supporting segregation of duties and release-control oversight in procurement.

This EI serves as an essential control for procurement and financial oversight by:
- Enabling detection of purchase orders approved by their creator, which may indicate segregation-of-duties violations or missing approval workflows
- Supporting identification of release strategy and release-status patterns by company code, purchasing organization, and vendor for control design review
- Providing visibility into the timing of creation and approval via configurable date reference and duration for prioritization and audit
- Enabling analysis of release groups, release codes, and processing status for exception management and policy enforcement
- Supporting month-end and audit readiness by surfacing creator-approver same-user exceptions that may require remediation or disclosure

Monitoring creator-approver separation helps organizations enforce segregation of duties in purchasing, reduce risk of unauthorized commitments, and prioritize follow-up on high-value or aged exceptions. The EI is particularly valuable for procurement controls, internal audit, and compliance reviews.

The EI uses purchase order header data (EKKO), release configuration (T16FB), and change document header and item data (CDHDR, CDPOS) to determine release status and compare creator with approver.
