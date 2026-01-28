### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 68 parameters listed in the Parameters Reference Table above.

**ACT_CHNGNO** (Document Number):

Change document number (activation or planning). Identifies the change document instance in the change log.

**BACKDAYS** (Backdays):

Number of days backward from today used to set the default date range for change document selection (UDATE). When UDATE is not supplied, the EI uses today minus BACKDAYS as the lower bound. Used together with UDATE to define the time window for change documents.

**BACKDAYS and UDATE Connection:**
BACKDAYS defines the default lookback when no date range is specified. UDATE filters change documents by change date. Together they restrict which change documents are read from CDHDR/CDPOS.

**BANKN** (Bank Account compare):

Bank account number (e.g. from LFBK) used when comparing or identifying vendor bank key components in change documents.

**CHANGENR** (Document Number):

Change document number from CDHDR. Filters change document headers by CHANGENR. Supports range selection.

**CHANGE_IND** (Appl. object change-Header Lvl):

Change indicator at header level (CDHDR-CHANGE_IND). Filters by type of change (e.g. object creation, change). Supports range selection.

**CHANGE_IND_DESC** (Domain name):

Short text for the header-level change indicator (CHANGE_IND), derived from domain CDCHNGIND for display.

**CHNGIND** (Change Indicator-Row lvl):

Change indicator at row level (CDPOS-CHNGIND): Insert (I), Update (U), Delete (E). Filters which row-level changes are returned. Supports range selection.

**CHNGIND_DESC** (Domain name):

Short text for the row-level change indicator (CHNGIND), derived from domain CDCHNGIND for display.

**CONVERT_KEY** ('X' - Decompose Key Field):

When set to 'X', the EI decomposes the table key (TABKEY) into key components (KEY1–KEY10, KEY1_V–KEY10_V, KEY1_DS–KEY10_DS) and can convert Insert/Delete key records into Update-style rows showing old vs new key values. Used with REPETITIVE for key comparison.

**CONVERT_KEY Options:**
- **X**: Decompose key; convert key Insert/Delete into comparable key updates where applicable.
- Blank: No key decomposition; key stored as TABKEY only.

**CUKY_NEW** (CUKY):

Currency of the new value after a change. Document or transaction currency of the new amount, depending on the field context.

**CUKY_OLD** (CUKY):

Currency of the old value before a change. Document or transaction currency of the old amount, depending on the field context.

**FIELD_DESC** (Short Description):

Short description of the changed field (FNAME) from the data dictionary for the table (TABNAME).

**FNAME** (Field Name):

Name of the changed field in the change document (CDPOS-FNAME). Filters which field-level changes are returned (e.g. bank account, bank key). Supports range selection.

**KEY1 - KEY10** (Field Name 1 - Field Name 10):

Key field names (1–10) for the table (TABNAME) of the change document. Populated when CONVERT_KEY is used; identifies the key components.

**KEY1_DS - KEY10_DS** (Short Description 1 - Short Description 10):

Short descriptions of the key fields (1–10) from the data dictionary. Populated when CONVERT_KEY is used.

**KEY1_V - KEY10_V** (Short Description 1 - Short Description 10):

Current values of the key components (1–10) in character form (from TABKEY decomposition). Populated when CONVERT_KEY is used.

**LANGU** (Language for texts):

Language for descriptions (FIELD_DESC, TAB_DESC, KEY1_DS–KEY10_DS, CHANGE_IND_DESC, CHNGIND_DESC). Default from system language when not specified.

**MANAGE_IN_UTC** ('X' - Manage in UTC):

When set to 'X', date/time used for BACKDAYS and UDATE is interpreted in UTC. When blank, local date/time is used.

**MANAGE_IN_UTC Options:**
- **X**: Use UTC for date/time evaluation.
- Blank: Use local date/time.

**NAME_FIRST** (First Name):

First name of the user who made the change (from USERNAME), from user master.

**NAME_LAST** (Last Name):

Last name of the user who made the change (from USERNAME), from user master.

**NAME_TEXT** (Full Name):

Full name of the user who made the change (from USERNAME), from user master.

**OBJECTCLAS** (Change doc. object):

Change document object class (CDHDR-OBJECTCLAS). Filters which object types are read (e.g. vendor, vendor bank). Supports range selection. When TABNAME is specified, object class can be derived from TCDOB.

**OBJECTID** (Vendor):

Change document object ID (CDHDR-OBJECTID). For vendor bank change log, typically the vendor number (LIFNR). Filters change documents by object instance. Supports range selection.

**OBJECT_DESC** (Name):

Description of the object (e.g. vendor name when TABNAME is LFA1 and OBJECTID is LIFNR). Populated from master data for display.

**PLANCHNGNR** (Change number):

Planned change number when the change document was created from a planned change. Identifies the source planned change.

**REPETITIVE** ('X' - Repetitive Change):

When set to 'X', the EI runs in repetitive-change mode: it uses REPET_BACKDAYS and UDATE_REPET for the date window and can aggregate or compare changes by key (with CONVERT_KEY). When blank, single-period mode with BACKDAYS and UDATE applies.

**REPETITIVE Options:**
- **X**: Repetitive change mode; use REPET_BACKDAYS and UDATE_REPET.
- Blank: Single-period mode; use BACKDAYS and UDATE.

**REPETITIVE and REPET_BACKDAYS Connection:**
When REPETITIVE = 'X', REPET_BACKDAYS defines the lookback for the repetitive date range; UDATE_REPET filters by repetitive date. When REPETITIVE is blank, BACKDAYS and UDATE define the period.

**REPET_BACKDAYS** (Repetitive Backdays):

Number of days backward used for the repetitive date range when REPETITIVE = 'X'. Used together with UDATE_REPET to set the default repetitive window.

**TABKEY** (Table Key):

Concatenated key of the changed table row (from CDPOS). Used to identify the record; when CONVERT_KEY is used, key components are also exposed in KEY1–KEY10 and KEY1_V–KEY10_V.

**TABNAME** (Table Name):

Table name of the changed object (CDPOS-TABNAME). Filters which tables are included (e.g. LFBK for vendor bank). Supports range selection. Object class can be derived from TCDOB when OBJECTCLAS is not specified.

**TAB_DESC** (Short Description):

Short description of the table (TABNAME) from the data dictionary.

**TCODE** (Transaction Code):

Transaction code that created the change (CDHDR-TCODE). Filters by transaction. Supports range selection.

**TEXT_CASE** (Text flag):

Flag indicating whether the change record is of type text (for display or filtering).

**UDATE** (Date):

Change date (CDHDR-UDATE). Filters change document headers by date. When no UDATE range is supplied, default lower bound is derived from BACKDAYS. Supports range selection.

**UDATE_REPET** (Repetitive Date):

Date used in repetitive-change mode (REPETITIVE = 'X') to filter or group changes. Supports range selection; default range can be derived from REPET_BACKDAYS.

**UNIT_NEW** (Unit):

Unit of measure of the new value after a change (for quantity fields).

**UNIT_OLD** (Unit):

Unit of measure of the old value before a change (for quantity fields).

**USERNAME** (User):

User who made the change (CDHDR-USERNAME). Filters by user. Supports range selection.

**UTIME** (Time):

Time of the change (CDHDR-UTIME). Part of the change document header.

**VALUE_NEW** (New value):

New value of the changed field after the change (CDPOS-VALUE_NEW).

**VALUE_OLD** (Old value):

Old value of the changed field before the change (CDPOS-VALUE_OLD).

**WAS_PLANND** (Created from Planned):

Indicates whether the change document was created from a planned change.
