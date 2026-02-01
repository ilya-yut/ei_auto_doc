### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AD_NAMEFIR | First Name | CHAR | 40 | 0 | AD_NAMEFIR | TEXT40 |
| 2 | AD_NAMELAS | Last Name | CHAR | 40 | 0 | AD_NAMELAS | TEXT40 |
| 3 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 4 | BACKDAYS | Back days |  | 0 | 0 |  |  |
| 5 | BELNR | Document Number | CHAR | 10 | 0 | BELNR_D | BELNR |
| 6 | BKTXT | Document Header Text | CHAR | 25 | 0 | BKTXT | TEXT25 |
| 7 | BLART | Document Type | CHAR | 2 | 0 | BLART | BLART |
| 8 | BLDAT | Document Date | DATS | 8 | 0 | BLDAT | DATUM |
| 9 | BSCHL | Posting Key | CHAR | 2 | 0 | BSCHL | BSCHL |
| 10 | BSTAT | Doc.status | CHAR | 1 | 0 | BSTAT_D | BSTAT |
| 11 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 12 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 13 | BUZEI | Item | NUMC | 3 | 0 | BUZEI | BUZEI |
| 14 | COMP_OPERATOR | Operator | CHAR | 2 | 0 | BUCC_OPERATOR | BUCC_OPERATOR |
| 15 | CPUDT | Entry Date | DATS | 8 | 0 | CPUDT | DATUM |
| 16 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 17 | DMBE2 | Amount in LC2 | CURR | 13 | 2 | DMBE2 | WERT7 |
| 18 | DMBE3 | Amount in LC3 | CURR | 13 | 2 | DMBE3 | WERT7 |
| 19 | DMBTR | Amount in LC | CURR | 13 | 2 | DMBTR | WERT7 |
| 20 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 21 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 22 | FABKL | Factory calendar | CHAR | 2 | 0 | FABKL | WFCID |
| 23 | FACDATE | Factory date | DEC | 5 | 0 | FACDATE | HFDATE |
| 24 | FULL | 'X' - ALL items of FI document | CHAR | 1 | 0 | /SKN/E_SW_FLAG | CHECKBOX |
| 25 | GJAHR | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 26 | GRPID | Session name | CHAR | 12 | 0 | GRPID_BKPF | CHAR12 |
| 27 | GVTYP | P&L statmt acct type | CHAR | 2 | 0 | GVTYP | CHAR2 |
| 28 | HKONT | G/L Account | CHAR | 10 | 0 | HKONT | SAKNR |
| 29 | HKONT_DESC | G/L Acct Long Text | CHAR | 50 | 0 | TXT50_SKAT | TEXT50 |
| 30 | HWAE2 | Local currency 2 | CUKY | 5 | 0 | HWAE2 | WAERS |
| 31 | HWAE3 | Local currency 3 | CUKY | 5 | 0 | HWAE3 | WAERS |
| 32 | HWAER | Local Currency | CUKY | 5 | 0 | HWAER | WAERS |
| 33 | KTOKS | Account Group | CHAR | 4 | 0 | KTOKS | KTOKS |
| 34 | KTOPL | Chart of Accounts | CHAR | 4 | 0 | KTOPL | KTOPL |
| 35 | KTOPL_T001 | Chart of Accounts | CHAR | 4 | 0 | KTOPL | KTOPL |
| 36 | KURS2 | Exchange rate 2 | DEC | 9 | 5 | KURS2 | KURSF |
| 37 | KURS3 | Exchange rate 3 | DEC | 9 | 5 | KURS3 | KURSF |
| 38 | KURSF | Exchange rate | DEC | 9 | 5 | KURSF | KURSF |
| 39 | KZKRS | Group Currency Exchange Rate | DEC | 9 | 5 | KZKRS | KURSF |
| 40 | KZWRS | Group currency | CUKY | 5 | 0 | KZWRS | WAERS |
| 41 | LANGU | Language Key (G/L account) |  | 0 | 0 |  |  |
| 42 | MONAT | Posting period | NUMC | 2 | 0 | MONAT | MONAT |
| 43 | REF_FIELD_NAME1 | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 44 | REF_FIELD_NAME2 | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 45 | RESULT_COMP | Value 1 to Compare | CURR | 15 | 2 |  |  |
| 46 | SGTXT | Text | CHAR | 50 | 0 | SGTXT | TEXT50 |
| 47 | SHKZG | Debit/Credit Ind. | CHAR | 1 | 0 | SHKZG | SHKZG |
| 48 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 49 | UPDDT | Last update | DATS | 8 | 0 | UPDDT | DATUM |
| 50 | USNAM | User name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 51 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 52 | WAERS_FR | Foreign Currency | CUKY | 5 | 0 |  |  |
| 53 | WERKS | Plant | CHAR | 4 | 0 | WERKS_D | WERKS |
| 54 | WFCID | Factory calendar ID |  | 0 | 0 |  |  |
| 55 | WORKING_DAYS | Working Day - Y/N | CHAR | 1 | 0 | CIND | CHAR1 |
| 56 | WRBTR | Amount | CURR | 13 | 2 | WRBTR | WERT7 |
| 57 | XBLNR | Reference | CHAR | 16 | 0 | XBLNR1 | XBLNR1 |