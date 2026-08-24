### Parameter Relationships

**Lookback window:** When no explicit date range is supplied, **BACKDAYS** builds the initial date window used to retrieve transport details. Explicit **DATUM** ranges override that fallback.

**Source search:** **STRING_SEARCH** supplies the search strings applied to retrieved ABAP source. Matching lines are written to **LINE_SCAN** with **LINE_NO**. For database-changing commands (MODIFY, UPDATE, INSERT, DELETE), the scan keeps only hits that reference a real table name.

**Age after selection:** After matches are built, elapsed time from the transport date and time to evaluation time is stored in **DURATION** using **DURATION_UNIT**. Rows outside the configured **DURATION** range are removed. This is an additional filter after the date window.

**Transport scope:** **TRKORR**, **TRSTATUS**, **TRFUNCTION**, **STRKORR**, **AS4USER**, **PROJECT**, **TARSYSTEM**, **TARCLI**, **LOCKFLAG**, **IMPSING**, and **ACTFLG** narrow which requests and tasks feed the source scan.

**Object and program scope:** **PGMID**, **OBJECT**, **OBJ_NAME**, **OBJFUNC**, **ACTIVITY**, **SUBC**, **CNAM**, **CDAT**, **UNAM**, **UDAT**, **VERN**, and **AS4DATE** / **AS4TIME** restrict repository objects and program attributes included in the scan.

**Remote execution:** When **SW_DEST** is set, source and domain description lookups run on the specified destination.

**Language:** **LANG** is read for language-dependent domain descriptions such as transport status and request-type descriptions.
