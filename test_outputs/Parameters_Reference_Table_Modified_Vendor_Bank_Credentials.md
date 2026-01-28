### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | ACT_CHNGNO | Document Number | CHAR | 10 | 0 | CD_CHNGNO | CDCHANGENR |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | BANKN | Bank Account compare |  | 0 | 0 |  |  |
| 4 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 5 | CHANGE_IND | Appl. object change-Header Lvl | CHAR | 1 | 0 | CDCHNGINDH | CDCHNGIND |
| 6 | CHANGE_IND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 7 | CHNGIND | Change Indicator-Row lvl | CHAR | 1 | 0 | CDCHNGIND | CDCHNGIND |
| 8 | CHNGIND_DESC | Domain name | CHAR | 30 | 0 | DOMNAME | DOMNAME |
| 9 | CONVERT_KEY | 'X' - Decompose Key Field |  | 0 | 0 |  |  |
| 10 | CUKY_NEW | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 11 | CUKY_OLD | CUKY | CUKY | 5 | 0 | CDCUKY | WAERS |
| 12 | FIELD_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 13 | FNAME | Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 14-23 | KEY1 - KEY10 | Field Name - Field Name | CHAR | 30 | 0 | FIELDNAME | FDNAME |
| 24-33 | KEY1_DS - KEY10_DS | Short Description - Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 34-43 | KEY1_V - KEY10_V | Short Description - Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 44 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 45 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 46 | NAME_FIRST | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 47 | NAME_LAST | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 48 | NAME_TEXT | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 49 | OBJECTCLAS | Change doc. object | CHAR | 15 | 0 | CDOBJECTCL | OBJECTCL |
| 50 | OBJECTID | Vendor | CHAR | 90 | 0 | CDOBJECTV | CHAR90 |
| 51 | OBJECT_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 52 | PLANCHNGNR | Change number | CHAR | 12 | 0 | PLANCHNGNR | PLANCHNGNR |
| 53 | REPETITIVE | 'X' - Repetitive Change | CHAR | 1 | 0 | /SKN/E_REPEAT | XFLAG |
| 54 | REPET_BACKDAYS | Repetitive Backdays |  | 0 | 0 |  |  |
| 55 | TABKEY | Table Key | CHAR | 70 | 0 | CDTABKEY | CHAR70 |
| 56 | TABNAME | Table Name | CHAR | 30 | 0 | TABNAME | AS4TAB |
| 57 | TAB_DESC | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 58 | TCODE | Transaction Code | CHAR | 20 | 0 | CDTCODE | TCODE |
| 59 | TEXT_CASE | Text flag | CHAR | 1 | 0 | CDXFELD | XFELD |
| 60 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 61 | UDATE_REPET | Repetitive Date |  | 0 | 0 |  |  |
| 62 | UNIT_NEW | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 63 | UNIT_OLD | Unit | UNIT | 3 | 0 | CDUNIT | CDUNIT |
| 64 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 65 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 66 | VALUE_NEW | New value | CHAR | 254 | 0 | CDFLDVALN | FIELDVAL |
| 67 | VALUE_OLD | Old value | CHAR | 254 | 0 | CDFLDVALO | FIELDVAL |
| 68 | WAS_PLANND | Created from Planned | CHAR | 1 | 0 | CD_PLANNED | XFLAG |
