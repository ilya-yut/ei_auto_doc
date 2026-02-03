## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_03_PO_APPR_BY_CR | AEDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BEDAT | Purchasing Document Date | DATS(8) | EBDAT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSAKZ | Control indicator for purchasing document type | CHAR(1) | BSAKZ |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSART | Purchasing Document Type | CHAR(4) | ESART |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSART_DESC | Short Description of Purchasing Document Type | CHAR(20) | BATXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSTYP | Purchasing Document Category | CHAR(1) | EBSTYP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BSTYP_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | CHANGENR | Document change number | CHAR(10) | CDCHANGENR |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EBELN | Purchasing Document Number | CHAR(10) | EBELN |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKGRP | Purchasing Group | CHAR(3) | BKGRP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKGRP_DESC | Description of purchasing group | CHAR(18) | EKNAM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | EKORG_DESC | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGC | Release code | CHAR(2) | FRGCO |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGGR | Release group | CHAR(2) | FRGGR |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGKE | Release Indicator: Purchasing Document | CHAR(1) | FRGKE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGRL | Release Not Yet Completely Effected | CHAR(1) | FRGRL |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGSX | Release Strategy | CHAR(2) | FRGSX |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | FRGZU | Release status | CHAR(8) | FRGZU |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | KDATB | Start of Validity Period | DATS(8) | KDATB |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | KDATE | End of Validity Period | DATS(8) | KDATE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | LIFNR | Vendor Account Number | CHAR(10) | ELIFN |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | LOEKZ | Deletion Indicator in Purchasing Document | CHAR(1) | ELOEK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | PROCSTAT | Purchasing document processing state | CHAR(2) | MEPROCSTATE |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | PROCSTAT_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RESWK | Supplying (Issuing) Plant in Stock Transport Order | CHAR(4) | RESWK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RESWK_DESC | Name | CHAR(30) | NAME1 |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | RLWRT | Total value at time of release | CURR(15,2) | RLWRT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | STATU | Status of Purchasing Document | CHAR(1) | ESTAK |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | STATU_DESC | Short Text for Fixed Values | CHAR(60) | VAL_TEXT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | UDATE | Creation date of the change document | DATS(8) | CDDATUM |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | USERNAME | User name of the person responsible in change document | CHAR(12) | CDUSERNAME |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | UTIME | Time changed | TIMS(6) | CDUZEIT |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | VENDOR_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_03_PO_APPR_BY_CR | ZTERM | Terms of Payment Key | CHAR(4) | DZTERM |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_03_PO_APPR_BY_CR .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_03_PO_APPR_BY_CR OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE: LANGU  LANGU,
               BACKDAYS INT4,
               DATE_REF_FLD NAME_FELD,
               LAST_ONLY CHAR1,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
  LV_BACKDAYS = 10.
  LV_DURATION_UNIT = 'D'.
  LV_LANGU = SY-LANGU.
  LV_DATE_REF_FLD = 'BEDAT'. "PO date
  SELECT_SINGLE: LANGU,
                 BACKDAYS,
                 DATE_REF_FLD,
                 LAST_ONLY,
                 DURATION_UNIT.
  DATA_MULTY: FRGRL        FRGRL,
              EBELN        EBELN,
              BUKRS        BUKRS,
              BSTYP        EBSTYP,
              BSART        ESART,
              EKORG        EKORG,
              EKGRP        BKGRP,
              FRGGR        FRGGR,
              FRGSX        FRGSX,
              FRGCO        FRGCO,
              FRGKE        FRGKE,
              LIFNR        ELIFN,
              RESWK        RESWK,
              ZTERM        DZTERM,
              ERNAM        ERNAM,
              AEDAT        ERDAT,
              BEDAT        EBDAT,
              WAERS        WAERS,
              PROCSTAT    MEPROCSTATE,
              DATUM        SY-DATUM,
              DURATION    /SKN/E_SW_DURATION.
  SELECT_MULTY:
              FRGRL,
              EBELN,
              BUKRS,
              BSTYP,
              BSART,
              EKORG,
              EKGRP,
              FRGGR,
              FRGSX,
              FRGCO,
              FRGKE,
              LIFNR,
              RESWK,
              ZTERM,
              ERNAM,
              AEDAT,
              BEDAT,
              WAERS,
              PROCSTAT,
              DATUM,
              DURATION.
  CONVERT_MULTY: EBELN ALPHA,
                 LIFNR ALPHA.
  RANGES : R_FLD_NAME FOR DD03P-FIELDNAME,
           R_FLD_VAL FOR DD03P-FIELDNAME .
  DATA :   FLD_NAME TYPE FIELDNAME.
  DATA : I TYPE I,
         CI(1) TYPE C,
         NFIELDS TYPE I VALUE 3.   "
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM .
  DATA : LANGU LIKE SY-LANGU .
  DATA : IS_OUT(1) TYPE C.
  DATA : TIME_DIFF TYPE  INT4 .
  DATA: LV_DOMNAME LIKE  DD07V-DOMNAME,
        LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
        LV_DDTEXT LIKE  DD07V-DDTEXT.
  DATA: LV_FRGCO  TYPE FRGCO.
  DATA: LS_CDPOS TYPE CDPOS,
        LT_CDPOS LIKE TABLE OF LS_CDPOS.
  DATA: BEGIN OF LS_WRK.
          INCLUDE STRUCTURE /SKN/S_SW_10_03_PO_APPR_BY_CR.
  DATA: WRK_OBJECTID  TYPE CDOBJECTV.
  DATA: WRK_TABKEY    TYPE CDPOS-TABKEY.
  DATA: END OF LS_WRK.
  DATA: LT_WRK LIKE TABLE OF LS_WRK.
  DATA: LV_OBJECTCLAS TYPE CDOBJECTCL VALUE 'EINKBELEG'.
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA : FLD(60) TYPE C .
  DATA : REF_DATE TYPE D.
*data: lra_range type range of DD03P-FIELDNAME.
  DATA: WRK_OBJECTID  TYPE CDOBJECTV,
        WRK_TABKEY    TYPE CDPOS-TABKEY.
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY .
  DATA : BEGIN OF SW_STRUCTURE OCCURS 0.
          INCLUDE STRUCTURE /SKN/S_SW_S_FCAT .
  DATA : END OF SW_STRUCTURE .
  DATA : LS_VBPA TYPE VBPA,
         LT_VBPA LIKE TABLE OF LS_VBPA.
  DATA : LV_DATA_POSNR TYPE POSNR.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_03_PO_APPR_BY_CR'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "Document created
    WHEN 'BEDAT'.
      R_BEDAT[] = R_DATUM[]. "PO Date
    WHEN OTHERS.
      R_BEDAT[] = R_DATUM[]. "Billing date
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM EKKO AS E
      INNER JOIN T16FB AS T  ON E~FRGKE = T~FRGKE
      INNER JOIN CDHDR AS CH ON CH~OBJECTID = E~EBELN
      INTO CORRESPONDING FIELDS OF TABLE LT_WRK
      WHERE   T~KZFRE = 'X'
        AND   FRGRL IN R_FRGRL    "  EQ 'X'
        AND FRGGR IN R_FRGGR
        AND FRGSX IN R_FRGSX
        AND EBELN IN R_EBELN
        AND BSTYP IN R_BSTYP
        AND EKORG IN R_EKORG
        AND BUKRS IN R_BUKRS
        AND LIFNR IN R_LIFNR
        AND RESWK IN R_RESWK
        AND BEDAT IN R_BEDAT
        AND AEDAT IN R_AEDAT
        AND BSART IN R_BSART
        AND EKGRP IN R_EKGRP
        AND ERNAM IN R_ERNAM
        AND ZTERM IN R_ZTERM
        AND WAERS IN R_WAERS
        AND LOEKZ EQ SPACE
        AND PROCSTAT IN R_PROCSTAT
     "----
      AND   CH~OBJECTCLAS = LV_OBJECTCLAS  " 'EINKBELEG'
*      AND   e~ernam = ch~username
      AND FRGSX > ''
      AND E~FRGKE IN R_FRGKE.
  SORT LT_WRK.
  DELETE ADJACENT DUPLICATES FROM LT_WRK.
  LOOP AT LT_WRK INTO LS_WRK.
    SY_TABIX = SY-TABIX.
    LS_WRK-WRK_OBJECTID  = LS_WRK-EBELN.
    MODIFY LT_WRK FROM LS_WRK INDEX SY_TABIX.
  ENDLOOP.
  IF LT_WRK[] IS NOT INITIAL.
    SELECT * FROM CDPOS
      INTO CORRESPONDING FIELDS OF TABLE LT_CDPOS
      FOR ALL ENTRIES IN LT_WRK
      WHERE OBJECTCLAS = LV_OBJECTCLAS  "'EINKBELEG'
        AND OBJECTID = LT_WRK-WRK_OBJECTID "  EBELN
        AND TABNAME = 'EKKO'
        AND FNAME = 'FRGZU'.
*** YC++ 18.05.21
    IF LT_CDPOS IS NOT INITIAL.
      SORT LT_CDPOS BY VALUE_NEW.
      DELETE LT_CDPOS WHERE VALUE_NEW IS INITIAL.
    ENDIF.
*** YC++ 18.05.21
*      sort lt_CDPOS by OBJECTCLAS OBJECTID.
    SORT LT_CDPOS BY OBJECTCLAS OBJECTID CHANGENR DESCENDING TABKEY.
*    IF lv_last_only IS NOT INITIAL. "Only last release is relevant - the
*      CLEAR: wrk_objectid, wrk_tabkey.
*      LOOP AT lt_cdpos INTO ls_cdpos.
*        sy_tabix = sy-tabix.
*        IF wrk_objectid EQ ls_cdpos-objectid AND
*           wrk_tabkey   EQ ls_cdpos-tabkey.
*          DELETE lt_cdpos INDEX sy_tabix.
*          CONTINUE.
*        ELSE.
*          wrk_objectid = ls_cdpos-objectid.
*          wrk_tabkey = ls_cdpos-tabkey.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
  ENDIF.
  REFRESH T_DATA.
  DATA: LT_WRK_TABIX TYPE INT4.
  LOOP AT LT_WRK INTO LS_WRK.
    LT_WRK_TABIX = SY-TABIX.
    READ TABLE LT_CDPOS INTO LS_CDPOS   " TRANSPORTING NO FIELDS
                        WITH KEY OBJECTCLAS = LV_OBJECTCLAS
                                 OBJECTID   = LS_WRK-WRK_OBJECTID
                                 CHANGENR   = LS_WRK-CHANGENR
                         BINARY SEARCH.
    IF NOT SY-SUBRC IS INITIAL.
      DELETE LT_WRK INDEX LT_WRK_TABIX.
      CONTINUE.
    ELSE.
      LS_WRK-FRGZU = LS_CDPOS-VALUE_NEW.
      MODIFY LT_WRK FROM LS_WRK INDEX LT_WRK_TABIX.
    ENDIF.
  ENDLOOP.
  SORT LT_WRK BY WRK_OBJECTID UDATE DESCENDING UTIME DESCENDING.
  IF LV_LAST_ONLY IS NOT INITIAL. "Only last release is relevant - the rest is deleted.
    CLEAR: WRK_OBJECTID, WRK_TABKEY.
    LOOP AT LT_WRK INTO LS_WRK.
      IF WRK_OBJECTID EQ LS_WRK-WRK_OBJECTID
*          AND wrk_tabkey   EQ ls_cdpos-tabkey
        .
        CONTINUE.
      ELSE.
        WRK_OBJECTID = LS_WRK-WRK_OBJECTID.
        IF LS_WRK-USERNAME EQ LS_WRK-ERNAM.
          MOVE-CORRESPONDING LS_WRK TO T_DATA.
          APPEND T_DATA.
        ENDIF.
*          wrk_tabkey = ls_cdpos-tabkey.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT LT_WRK INTO LS_WRK.
      IF LS_WRK-USERNAME EQ LS_WRK-ERNAM.
        MOVE-CORRESPONDING LS_WRK TO T_DATA.
        APPEND T_DATA.
      ENDIF.
    ENDLOOP.
  ENDIF.
*********************************************************************************
  "--- Get Release Code
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_PO_GET_FRGC'
      EXPORTING
        FRGGR             = T_DATA-FRGGR
        FRGSX             = T_DATA-FRGSX
        FRGZU             = T_DATA-FRGZU
      IMPORTING
        FRGC              = T_DATA-FRGC
      EXCEPTIONS
        WRONG_COMBINATION = 1
        OTHERS            = 2.
    IF SY-SUBRC = 0.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE FRGC NOT IN R_FRGCO.
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-UZEIT
          D_TO        = SY-DATUM
          T_TO        = SY-UZEIT
          TIME_UNIT   = LV_DURATION_UNIT   "'D'
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          T_DATA-DURATION  = TIME_DIFF .
        ELSE.
          T_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
******************************************************************************
********************************************************************************
  "--- Set Descriptions
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    "-- BSTYP_DESC
    LV_DOMNAME = 'EBSTYP'.
    LV_DOMVALUE = T_DATA-BSTYP.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-BSTYP_DESC = LV_DDTEXT.
    ENDIF.
    "-- STATU_DESC
    LV_DOMNAME = 'ESTAK'.
    LV_DOMVALUE = T_DATA-STATU.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-STATU_DESC = LV_DDTEXT.
    ENDIF.
    "-- PROCSTAT_DESC
    LV_DOMNAME = 'MEPROCSTATE'.
    LV_DOMVALUE = T_DATA-PROCSTAT.
    CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
      EXPORTING
        I_DOMNAME  = LV_DOMNAME
        I_DOMVALUE = LV_DOMVALUE
        LANGU      = LV_LANGU
*       SW_DEST    =
      IMPORTING
        E_DDTEXT   = LV_DDTEXT
      EXCEPTIONS
        NOT_EXIST  = 1
        OTHERS     = 2.
    IF SY-SUBRC = 0.
      T_DATA-PROCSTAT_DESC = LV_DDTEXT.
    ENDIF.
    "-- BSART_DESC
    CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
      EXPORTING
        BSART      = T_DATA-BSART
        LANGU      = LV_LANGU
        BSTYP      = T_DATA-BSTYP
      IMPORTING
        TYPE_DESC  = T_DATA-BSART_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- Get  Vendor Decriptions
    CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
      EXPORTING
        LIFNR        = T_DATA-LIFNR
      IMPORTING
        VENDOR_DESC  = T_DATA-VENDOR_DESC
      EXCEPTIONS
        WRONG_VENDOR = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKORG_DESC
    CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
      EXPORTING
        EKORG        = T_DATA-EKORG
        "LANGU              = lv_LANGU
      IMPORTING
        PUR_ORG_DESC = T_DATA-EKORG_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "-- EKGRP_DESC
    CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
      EXPORTING
        EKGRP        = T_DATA-EKGRP
*       LANGU        = lv_LANGU
      IMPORTING
        PUR_GRP_DESC = T_DATA-EKGRP_DESC
      EXCEPTIONS
        WRONG_CODE   = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    "--- RESWK_DESC (WERKS)
    CALL FUNCTION '/SKN/F_SW_10_PLANT_DESC'
      EXPORTING
        WERKS      = T_DATA-RESWK
*       LANGU      = lv_LANGU
      IMPORTING
        PLANT_DESC = T_DATA-RESWK_DESC
      EXCEPTIONS
        WRONG_CODE = 1
        OTHERS     = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
