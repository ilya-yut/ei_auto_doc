### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AGG_LVL | Agg.Level | CHAR | 30 | 0 | /SKN/E_SW_AGG_LVL | /SKN/D_SW_AGG_LVL |
| 2 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 3 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 4 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 5 | BUKRS |  |  |  |  |  |  |
| 6 | BWKEY |  |  |  |  |  |  |
| 7 | COMP_OPERATOR | Operator for comparison | CHAR | 2 | 0 | BUCC_OPERATOR | BUCC_OPERATOR |
| 8 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 9 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 10 | DIFF_AMOUNT | difference amount |  | 0 | 0 |  |  |
| 11 | DSTAT | Adjustment status | CHAR | 1 | 0 | DSTAT | DSTAT |
| 12 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 13 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 14 | GIDAT | Planned count date | DATS | 8 | 0 | GIDAT | DATUM |
| 15 | KTOPL |  |  |  |  |  |  |
| 16 | LANGU | Language |  | 0 | 0 |  |  |
| 17 | LGORT | Storage Location | CHAR | 4 | 0 | LGORT_D | LGORT |
| 18 | LSTAT | "Delete" status | CHAR | 1 | 0 | LSTAT | DSTAT |
| 19 | MANAGE_IN_UTC |  | 0 | 0 |  |  |  |
| 20 | PRESENT_ZERO | 'X' - Present Zero |  | 0 | 0 |  |  |
| 21 | REF_FIELD1 | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |  |
| 22 | REF_FIELD2 | CHAR | 30 | 0 | NAME_FELD | NAME_FELD |  |
| 23 | REF_TABNAME1 | Table name(for REF.FIELD1) |  | 0 | 0 |  |  |
| 24 | REF_TABNAME2 | Table name(for REF.FIELD2) |  | 0 | 0 |  |  |
| 25 | RESULT_COMP | Value to Compare | CURR | 15 | 2 |  |  |
| 26 | SOBKZ | Special Stock | CHAR | 1 | 0 | SOBKZ | SOBKZ |
| 27 | SPERR | Posting Block | CHAR | 1 | 0 | ISPER | XFELD |
| 28 | SW_DEST |  | 0 | 0 |  |  |  |
| 29 | USNAM | Changed by(Item lvl) | CHAR | 12 | 0 | USNAA | USNAM |
| 30 | USNAM_HD | User name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 31 | VGART | Trans./Event Type | CHAR | 2 | 0 | VGART | VGART |
| 32 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 33 | WAERS_FR | Foreign Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 34 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 35 | XBUFI | Freeze book invntory | CHAR | 1 | 0 | XBUFI | XFELD |
| 36 | ZLDAT | Count date | DATS | 8 | 0 | DZLDAT | DATUM |
| 37 | ZSTAT | Count status | CHAR | 1 | 0 | DZSTAT | ZSTAT |
