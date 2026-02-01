## General Overview

This Exception Indicator (EI) monitors FI (Financial Accounting) document postings by company code, document number, fiscal year, and G/L account to identify exceptional postings that may require review. It uses document header and line data from accounting documents (BKPF, BSIS, BSAS, BSEG) and G/L account master (SKA1) to filter by date range, amount, currency, document type, transaction code, and G/L account type, and can return either header-level records or full line-item detail with user name and G/L account description.

This EI serves as an essential control for financial reporting and operational oversight by:
- Enabling detection of exceptional or unusual postings by G/L account that may indicate errors, fraud, or policy violations
- Supporting identification of documents and line items by posting date, entry date, or last change for prioritization and audit review
- Providing visibility into posting duration (time since reference date) for aging and follow-up
- Enabling analysis by company code, document type, transaction code, and user for accountability and root-cause analysis
- Supporting filtering by working days or holidays when a factory calendar is used, for period and calendar-aware monitoring

The EI supports month-end close, internal controls, and exception management by surfacing FI documents and line items that meet configurable selection criteria. Data is drawn from FI document header and line tables (BKPF, BSIS, BSAS, BSEG) and G/L account master (SKA1).
