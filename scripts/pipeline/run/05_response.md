### Parameter Relationships

**Time-Based Selection Parameters:**

- When no date range is supplied, the EI builds the monitoring window from today minus the lookback length. The number of days to look back is configured via a single numeric parameter; that value defines the start of the window. The EI then maps this window to a configurable date field (e.g. document created date or PO date) so that purchase orders are selected by the chosen date basis.

**Duration Calculation Parameters:**

- The EI computes a duration (in time units) between a reference date taken from each record and the current date. The reference date is taken from the output record using a configurable date field name. The unit in which duration is expressed (e.g. days) is configured separately. Together, the reference date field and the duration unit determine how duration is calculated; a numeric duration filter can then be used to restrict results (e.g. orders with duration within a range).

**Release Strategy and Release Code Parameters:**

- Release group, release strategy, release indicator, and release code work together to scope which purchase orders are subject to release and which release states are included. The EI reads release status from the change document (field FRGZU) and resolves it to a release code; the release code filter then determines which orders appear in the result. Setting release group, release strategy, and release code in combination focuses the result set on the relevant release configuration and status.

**Creator vs. Approver (LAST_ONLY):**

- When "only last approver" is set, the EI keeps at most one record per order (the most recent change by release date/time) and then checks whether the user who performed that release is the same as the order creator. When not set, the EI includes every release step where the approver equals the creator. This parameter therefore controls whether the result set is limited to the latest release per order or includes all creator-approver same-user release steps.
