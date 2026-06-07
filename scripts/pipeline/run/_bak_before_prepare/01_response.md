## General Overview

This Exception Indicator monitors physical inventory count documents at inventory-document level and surfaces count lines with material differences based on configurable aggregation, amount thresholds, and date filters.

This EI serves as an essential control for inventory and warehouse operations by:

- Identifying inventory count documents (IKPF/ISEG) with non-zero or threshold-level differences in local currency
- Supporting aggregation at line, plant, or inventory-document level before detail lines are returned
- Enabling comparison of two configurable amount fields from reference tables when currency conversion is required
- Applying posting-date or alternative date windows through backdays and date-reference-field settings
- Supporting duration-based aging on the selected reference date field

Typical use includes cycle-count reviews, post-count reconciliation, and audit sampling of documents with material valuation differences. Results are intended for exception workflows rather than full inventory document extracts.

The routine builds dynamic selections on inventory header and item tables, applies difference and comparison rules, enriches output with material descriptions, and raises an alert when qualifying count lines remain.
