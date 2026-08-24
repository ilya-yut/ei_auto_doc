### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ACTFLG | tp Active Flag | CHAR | 1 | 0 | TRTPACTFLG |  |
| 2 | ACTIVITY | IMG Activity | CHAR | 20 | 0 | TRACTIVITY | CUS_IMG_AC |
| 3 | AS4DATE | Date | DATS | 8 | 0 | AS4DATE | AS4DATE |
| 4 | AS4POS | Dictionary: Line item | NUMC | 6 | 0 | DDPOSITION | DDPOSITION |
| 5 | AS4TIME | Time | TIMS | 6 | 0 | AS4TIME | AS4TIME |
| 6 | AS4USER | Owner | CHAR | 12 | 0 | TR_AS4USER | AS4USER |
| 7 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 8 | BUFLVL | Counter | NUMC | 1 | 0 | COUNTER | COUNTER |
| 9 | BUFPOS | Dictionary: Line item | NUMC | 6 | 0 | DDPOSITION | DDPOSITION |
| 10 | CDAT | Created on | DATS | 8 | 0 | RDIR_CDATE | SYDATS |
| 11 | CNAM | Created By | CHAR | 12 | 0 | CNAM | SYCHAR12 |
| 12 | DOMNAM | Transport Domain | CHAR | 10 | 0 | TMSDOMNAM | TMSDOMNAM |
| 13 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 14 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 15 | IMPSING | Indicator | CHAR | 1 | 0 | FLAG | FLAG |
| 16 | LANG | Language Key | LANG | 1 | 0 | SPRAS | SPRAS |
| 17 | LINE_NO | Line | NUMC | 6 | 0 | RSROW | RSROW |
| 18 | LINE_SCAN | Line source scan | CHAR | 255 | 0 | /SKN/E_SW_SOURCE_SCAN_STRING | TEXT255 |
| 19 | LOCKFLAG | Lock/Import Status | CHAR | 1 | 0 | LOCKFLAG | TR_IMPORT_STATUS |
| 20 | OBJ_NAME | Obj. Name | CHAR | 120 | 0 | TROBJ_NAME | TROBJ_NAME |
| 21 | OBJECT | Object Type | CHAR | 4 | 0 | TROBJTYPE | OBJECT |
| 22 | OBJFUNC | Function | CHAR | 1 | 0 | OBJFUNC | OBJFUNC |
| 23 | PGMID | Program ID | CHAR | 4 | 0 | PGMID | PGMID |
| 24 | PROJECT | CTS Project | CHAR | 20 | 0 | TRKORR_P | TRKORR |
| 25 | STRING_SCAN | String source scan | CHAR | 255 | 0 | /SKN/E_SW_SOURCE_SCAN_STRING | TEXT255 |
| 26 | STRING_SEARCH | String Source Search | CHAR | 72 | 0 | /SKN/E_SW_SOURCE_SEARCH_STRING | TXLINE |
| 27 | STRKORR | Higher-Level Request | CHAR | 20 | 0 | STRKORR | TRKORR |
| 28 | SUBC | Program Type | CHAR | 1 | 0 | SUBC | SUBC |
| 29 | SYSNAM | System Name | CHAR | 10 | 0 | TMSSYSNAM | SYSNAME |
| 30 | TARCLI | Target client | CHAR | 3 | 0 | TRTARCLI | CHAR3 |
| 31 | TARSYSTEM | Transport Target | CHAR | 10 | 0 | TR_TARGET | TR_TARGET |
| 32 | TEXT | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 33 | TRFUNCTION | Type of request/task | CHAR | 1 | 0 | TRFUNCTION | TRFUNCTION |
| 34 | TRFUNCTION_TEXT | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 35 | TRKORR | Request/Task | CHAR | 20 | 0 | TRKORR | TRKORR |
| 36 | TRSTATUS | Status | CHAR | 1 | 0 | TRSTATUS | TRSTATUS |
| 37 | TRSTATUS_TEXT | Short Description | CHAR | 60 | 0 | AS4TEXT | AS4TEXT |
| 38 | UDAT | Changed On | DATS | 8 | 0 | RDIR_UDATE | SYDATS |
| 39 | UNAM | Last changed by | CHAR | 12 | 0 | UNAM | SYCHAR12 |
| 40 | VERN | Version number | CHAR | 6 | 0 | VERN | SYCHAR06 |
