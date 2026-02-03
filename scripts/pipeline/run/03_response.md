### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 3 | BEDAT | Document Date | DATS | 8 | 0 | EBDAT | DATUM |
| 4 | BSAKZ | Control indicator | CHAR | 1 | 0 | BSAKZ | BSAKZ |
| 5 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 6 | BSART_DESC | Doc. Type Descript. | CHAR | 20 | 0 | BATXT | TEXT20 |
| 7 | BSTYP | Purch. Doc. Category | CHAR | 1 | 0 | EBSTYP | EBSTYP |
| 8 | BSTYP_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 9 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 10 | CHANGENR | Document Number | CHAR | 10 | 0 | CDCHANGENR | CDCHANGENR |
| 11 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 12 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 13 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 14 | EKGRP | Purchasing Group | CHAR | 3 | 0 | BKGRP | EKGRP |
| 15 | EKGRP_DESC | Description p. group | CHAR | 18 | 0 | EKNAM | TEXT18 |
| 16 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 17 | EKORG_DESC | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 18 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 19 | FRGC | Release code | CHAR | 2 | 0 | FRGCO | FRGCO |
| 20 | FRGGR | Release group | CHAR | 2 | 0 | FRGGR | FRGGR |
| 21 | FRGKE | Release indicator | CHAR | 1 | 0 | FRGKE | FRGKE |
| 22 | FRGRL | Subject to release | CHAR | 1 | 0 | FRGRL | XFELD |
| 23 | FRGSX | Release Strategy | CHAR | 2 | 0 | FRGSX | FRGSX |
| 24 | FRGZU | Release State | CHAR | 8 | 0 | FRGZU | FRGZU |
| 25 | KDATB | Validity Per. Start | DATS | 8 | 0 | KDATB | DATUM |
| 26 | KDATE | Validity Period End | DATS | 8 | 0 | KDATE | DATUM |
| 27 | LAST_ONLY | Only last approver is checked |  | 0 | 0 |  |  |
| 28 | LIFNR | Vendor | CHAR | 10 | 0 | ELIFN | LIFNR |
| 29 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 30 | PROCSTAT | Purch. doc. proc. state | CHAR | 2 | 0 | MEPROCSTATE | MEPROCSTATE |
| 31 | PROCSTAT_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 32 | RESWK | Supplying Plant | CHAR | 4 | 0 | RESWK | WERKS |
| 33 | RESWK_DESC | Name 1 | CHAR | 30 | 0 | NAME1 | TEXT30 |
| 34 | RLWRT | Total val. upon release | CURR | 15 | 2 | RLWRT | WERT15 |
| 35 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 36 | STATU_DESC | Short Descript. | CHAR | 60 | 0 | VAL_TEXT | DDTEXT |
| 37 | UDATE | Date | DATS | 8 | 0 | CDDATUM | DATUM |
| 38 | USERNAME | User | CHAR | 12 | 0 | CDUSERNAME | CHAR12 |
| 39 | UTIME | Time | TIMS | 6 | 0 | CDUZEIT | UZEIT |
| 40 | VENDOR_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 41 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 42 | ZTERM | Terms of Payment | CHAR | 4 | 0 | DZTERM | ZTERM |
