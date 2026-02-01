### Parameter Relationships

**Time and duration parameters**

- **BACKDAYS** and **DATE_REF_FLD** work together: BACKDAYS defines how many days to look back from today when no explicit date range is supplied; DATE_REF_FLD selects which date field (BUDAT, AEDAT, CPUDT, UPDDT, BLDAT) is used for that range and for duration calculation. Set BACKDAYS to define the default window length and DATE_REF_FLD to align the window with posting date, entry date, change date, or document date as needed.
- **DURATION** and **DURATION_UNIT** work with **DATE_REF_FLD**: duration is computed between the reference date (from the field named by DATE_REF_FLD) and the run date, in the unit given by DURATION_UNIT (e.g. days). Use DURATION to filter by how long the document has been in the system; DURATION_UNIT defines the unit of measure.

**Factory calendar and working-days filtering**

- **WFCID** and **WORKING_DAYS** work together: when WFCID (factory calendar ID) is set, the EI uses the calendar to determine working days and holidays. WORKING_DAYS then filters results to working days only or to holidays only. Leave WFCID initial to skip working-days filtering.

**Full-detail mode**

- **FULL**: when set to 'X', the EI returns all line items per document with user name and G/L account description; when not set, one record per document header is returned. FULL does not combine with other parameters in a relationship group but controls the level of detail in the result.
