### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AEDAT | Changed On | DATS | 8 | 0 | AEDAT | DATUM |
| 2 | AGGR_FIELDS | Aggregation Fields |  | 0 | 0 |  |  |
| 3 | AGGR_PERIOD | Aggregation Period(Y,Q,M,W) |  | 0 | 0 |  |  |
| 4 | AUART | Sales Document Type | CHAR | 4 | 0 | AUART | AUART |
| 5 | AUDAT | Document Date | DATS | 8 | 0 | AUDAT | DATUM |
| 6 | BACKDAYS | Days Backward from today |  | 0 | 0 |  |  |
| 7 | BP1_CODE | Customer Code 1 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 8 | BP1_FUNCT | Partner Function 1 | CHAR | 2 | 0 | PARVW | PARVW |
| 9 | BP1_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 10 | BP2_CODE | Customer code 2 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 11 | BP2_FUNCT | Partner Function 2 | CHAR | 2 | 0 | PARVW | PARVW |
| 12 | BP2_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 13 | BP3_CODE | Customer code 3 | CHAR | 10 | 0 | KUNNR | KUNNR |
| 14 | BP3_FUNCT | Partner Function 3 | CHAR | 2 | 0 | PARVW | PARVW |
| 15 | BP3_NAME | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 16 | BSTNK | Purchase order no. | CHAR | 20 | 0 | BSTNK | BSTNK |
| 17 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 18 | DATE_REF_FLD | Date reference field |  | 0 | 0 |  |  |
| 19 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 20 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 21 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 22 | ERNAM | Created By | CHAR | 12 | 0 | ERNAM | USNAM |
| 23 | ERZET | Time | TIMS | 6 | 0 | ERZET | UZEIT |
| 24 | KUNNR | Sold-to party | CHAR | 10 | 0 | KUNAG | KUNNR |
| 25 | NETWR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 26 | NETWR_FR | Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 27 | SPART | Division | CHAR | 2 | 0 | SPART | SPART |
| 28 | STAT | Status | CHAR | 5 | 0 | J_STATUS | J_STATUS |
| 29 | TOT_CNT | Natural number | INT4 | 10 | 0 | INT4 | INT4 |
| 30 | TOT_NETWR | Total Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 31 | TOT_NETWR_FR | Tot. Foreign Net value | CURR | 15 | 2 | NETWR_AK | WERTV8 |
| 32 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 33 | VBELN | Sales Document | CHAR | 10 | 0 | VBELN_VA | VBELN |
| 34 | VBTYP | SD document categ. | CHAR | 1 | 0 | VBTYP | VBTYP |
| 35 | VDATU | Requested deliv.date | DATS | 8 | 0 | EDATU_VBAK | DATUM |
| 36 | VKBUR | Sales Office | CHAR | 4 | 0 | VKBUR | VKBUR |
| 37 | VKGRP | Sales Group | CHAR | 3 | 0 | VKGRP | VKGRP |
| 38 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 39 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |
| 40 | WAERK | Document Currency | CUKY | 5 | 0 | WAERK | WAERS |
| 41 | WAERK_FR | Foreign Currency | CUKY | 5 | 0 | WAERK | WAERS |