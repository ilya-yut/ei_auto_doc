### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 42 parameters listed in the Parameters Reference Table above.

**AEDAT** (Created on):

Date on which the purchase order was created. The EI uses this date (or the document date, depending on configuration) when building the monitoring window for order selection.

**BACKDAYS** (Backdays):

Number of days to look back from today. When no date range is supplied, the EI builds the monitoring window from today minus this value and applies it to the configured date field (e.g. creation date or PO date).

**BEDAT** (Document Date):

Purchase order document date (schedule line order date). The EI can use this date as the basis for the monitoring window and for duration calculation when so configured.

**BSAKZ** (Control indicator):

Control indicator for the purchasing document type. Values are function-specific.

**BSAKZ Options:**

Values are function-specific; see output structure or document type configuration.

**BSART** (Purchasing Doc. Type):

Purchasing document type (e.g. standard order, framework order). Used when the EI reads order header data to scope which document types are considered for the creator-approver check.

**BSART_DESC** (Doc. Type Descript.):

Short description of the purchasing document type; derived from document type and category via description lookup in the EI.

**BSTYP** (Purch. Doc. Category):

Purchasing document category (e.g. standard order, framework order). Used when the EI reads order header data to scope which document categories are considered.

**BSTYP Options:**

Values are function-specific (e.g. standard order, framework order).

**BSTYP_DESC** (Short Descript.):

Short text for the purchasing document category; derived from master data in the EI.

**BUKRS** (Company Code):

Company code of the purchase order. Used when the EI reads order header data to scope which company codes are considered for the creator-approver check.

**CHANGENR** (Document Number):

Change document number identifying the change log record for the release step; used to correlate the approver (USERNAME) with the release status (FRGZU) and to join order data with change document data.

**DURATION** (Duration In Time Units):

Duration in time units between the reference date (e.g. creation or PO date) and current date. The EI calculates this per record; the unit is configured via DURATION_UNIT. Used for age-based prioritization or filtering.

**DURATION_UNIT** (Duration Unit):

Unit in which duration is expressed (e.g. days). Used with DURATION for the duration calculation in the EI and for time-based filtering.

**DURATION_UNIT Options:**

- **H**: Hours
- **M**: Minutes
- **D**: Days
- **F**: Full days for specific day filtering

**EBELN** (Purchasing Document):

Purchasing document number. Used when the EI reads order header data and change documents to scope which orders are considered (by document number).

**EKGRP** (Purchasing Group):

Purchasing group of the order. Description is derived from master data in the EI. Used when the EI reads order header data to scope by purchasing group.

**EKGRP_DESC** (Description p. group):

Description of the purchasing group; from master data.

**EKORG** (Purch. Organization):

Purchasing organization of the order. Used when the EI reads order header data to scope which purchasing organizations are considered. Description is derived from master data in the EI.

**EKORG_DESC** (Description):

Description of the purchasing organization; from master data.

**ERNAM** (Created by):

User who created the purchase order. The EI compares this with the user who performed the release (USERNAME) to flag creator-approver same-user cases.

**FRGC** (Release code):

Release code resolved from release group, release strategy, and release status in the EI. Used after the EI reads release status from the change document to scope which release codes are included in the result.

**FRGC Options:**

Values are function-specific (release code configuration).

**FRGGR** (Release group):

Release group of the order. Used when the EI reads order header data to scope which orders are subject to release and have the specified release group(s).

**FRGKE** (Release indicator):

Release indicator that determines which release configuration applies to the order. Used when the EI reads order header data together with release configuration to scope which orders are subject to release.

**FRGKE Options:**

Values are function-specific (release configuration).

**FRGRL** (Subject to release):

Indicates whether the order is subject to release. Used when the EI reads order header data to scope which orders are subject to release and have a release strategy (creator-approver check applies only to such orders).

**FRGRL Options:**

- **X**: Set/active (subject to release).
- ** ** (space) or blank: Not set.

**FRGSX** (Release Strategy):

Release strategy code of the order. Used when the EI reads order header data to scope which orders have the specified release strategy and are subject to release.

**FRGZU** (Release State):

Release status (e.g. released, partially released). The EI reads this from the change document for the release field and resolves it to a release code (FRGC); the release code is then used to scope which orders are included in the result.

**KDATB** (Validity Per. Start):

Start of validity period for the release strategy or related configuration.

**KDATE** (Validity Period End):

End of validity period for the release strategy or related configuration.

**LAST_ONLY** (Only last approver is checked):

When set, the EI keeps only the most recent release per order and flags it only if that last approver is the order creator. When not set, every release step where the approver equals the creator is included.

**LAST_ONLY Options:**

- **X**: Set/active (only last release per order).
- ** ** (space) or blank: Not set (all creator-approver same-user steps).

**LIFNR** (Vendor):

Vendor (supplier) number of the order. Used when the EI reads order header data to scope which vendors are considered for the creator-approver check.

**LOEKZ** (Deletion Indicator):

Deletion indicator on the order. Used when the EI reads order header data; orders marked for deletion are excluded when this is not set, or scoped by the supplied value(s).

**LOEKZ Options:**

- **X**: Set/active (deletion indicator set).
- ** ** (space) or blank: Not set.

**PROCSTAT** (Purch. doc. proc. state):

Purchasing document processing state. Used when the EI reads order header data to scope which processing states are considered.

**PROCSTAT Options:**

Values are function-specific (e.g. draft, released, partially processed).

**PROCSTAT_DESC** (Short Descript.):

Short text for the processing state; derived from master data in the EI.

**RESWK** (Supplying Plant):

Supplying (issuing) plant of the order. Used when the EI reads order header data to scope which plants are considered.

**RESWK_DESC** (Name 1):

Name or description of the supplying plant; from master data.

**RLWRT** (Total val. upon release):

Total value at time of release. From the order; used for value-based analysis when exposed in the result.

**STATU** (Status):

Status of the purchasing document. Used when the EI reads order header data to scope which statuses are considered.

**STATU Options:**

Values are function-specific (e.g. released, blocked).

**STATU_DESC** (Short Descript.):

Short text for the document status; derived from master data in the EI.

**UDATE** (Date):

Creation date of the change document (release step). Identifies when the release was performed; used together with UTIME to order release steps (e.g. last release per order when LAST_ONLY is set).

**USERNAME** (User):

User who performed the release (approver). The EI compares this with the order creator (ERNAM) to flag creator-approver same-user cases.

**UTIME** (Time):

Time of the change document (release step). Used together with UDATE to order release steps (e.g. last release per order when LAST_ONLY is set).

**VENDOR_DESC** (Name):

Vendor name; derived from vendor master in the EI.

**WAERS** (Currency):

Document currency of the purchase order. Business meaning: currency in which the order is valued.

**ZTERM** (Terms of Payment):

Terms of payment key of the order. Used when the EI reads order header data to scope which terms of payment are considered.
