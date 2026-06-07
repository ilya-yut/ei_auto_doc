### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Created on | DATS | 8 | 0 | ERDAT | DATUM |
| 2 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 3 | BANFN | Purchase Requisition | CHAR | 10 | 0 | BANFN | BANFN |
| 4 | BEDAT | Purchase Order Date | DATS | 8 | 0 | ETBDT | DATUM |
| 5 | BNFPO | Item of Requisition | NUMC | 5 | 0 | BNFPO | BNFPO |
| 6 | BSART | Purchasing Doc. Type | CHAR | 4 | 0 | ESART | BSART |
| 7 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 8 | BWTAR | Valuation Type | CHAR | 10 | 0 | BWTAR_D | BWTAR |
| 9 | BWTTY | Valuation Category | CHAR | 1 | 0 | BWTTY_D | BWTTY |
| 10 | DATE_REF_FLD | Date Ref Field |  | 0 | 0 |  |  |
| 11 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 12 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 13 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 14 | EBELN | Purchasing Document | CHAR | 10 | 0 | EBELN | EBELN |
| 15 | EKGRP | Purchasing Group | CHAR | 3 | 0 | EKGRP | EKGRP |
| 16 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 17 | ELIKZ | Delivery Completed | CHAR | 1 | 0 | ELIKZ | XFELD |
| 18 | EREKZ | Final Invoice | CHAR | 1 | 0 | EREKZ | XFELD |
| 19 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM | USNAM |
| 20 | ESTKZ | Creation Indicator | CHAR | 1 | 0 | ESTKZ | ESTKZ |
| 21 | FIPOS | Commitment Item | CHAR | 14 | 0 | FIPOS | FIPOS |
| 22 | GRACEDAYS | Days Grace |  | 0 | 0 |  |  |
| 23 | KNTTP | Acct Assignment Cat. | CHAR | 1 | 0 | KNTTP | KNTTP |
| 24 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 25 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 26 | LOEKZ | Deletion Indicator | CHAR | 1 | 0 | ELOEK | ELOEK |
| 27 | MATKL | Material Group | CHAR | 9 | 0 | MATKL | MATKL |
| 28 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 29 | PSTYP | Item Category | CHAR | 1 | 0 | PSTYP | PSTYP |
| 30 | STATU | Status | CHAR | 1 | 0 | ESTAK | ESTAK |
| 31 | SW_DEST |  | 0 | 0 |  |  |  |
| 32 | UEBTK | Unltd Overdelivery | CHAR | 1 | 0 | UEBTK | XFELD |
| 33 | UEBTO | Overdeliv. Tolerance | DEC | 3 | 1 | UEBTO | PRZ21 |
| 34 | VBUND | Trading Partner | CHAR | 6 | 0 | RASSC | RCOMP |
| 35 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 36 | WEPOS | Goods Receipt | CHAR | 1 | 0 | WEPOS | XFELD |
| 37 | WERKS | Plant | CHAR | 4 | 0 | EWERK | WERKS |
