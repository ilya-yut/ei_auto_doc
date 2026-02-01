## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_07_FI_EXC_POST | AD_NAMEFIR | First name | CHAR(40) | AD_NAMEFIR |
| /SKN/S_SW_10_07_FI_EXC_POST | AD_NAMELAS | Last name | CHAR(40) | AD_NAMELAS |
| /SKN/S_SW_10_07_FI_EXC_POST | AEDAT | Changed On | DATS(8) | AEDAT |
| /SKN/S_SW_10_07_FI_EXC_POST | BELNR | Accounting Document Number | CHAR(10) | BELNR_D |
| /SKN/S_SW_10_07_FI_EXC_POST | BKTXT | Document Header Text | CHAR(25) | BKTXT |
| /SKN/S_SW_10_07_FI_EXC_POST | BLART | Document Type | CHAR(2) | BLART |
| /SKN/S_SW_10_07_FI_EXC_POST | BLDAT | Document Date in Document | DATS(8) | BLDAT |
| /SKN/S_SW_10_07_FI_EXC_POST | BSCHL | Posting Key | CHAR(2) | BSCHL |
| /SKN/S_SW_10_07_FI_EXC_POST | BSTAT | Document Status | CHAR(1) | BSTAT_D |
| /SKN/S_SW_10_07_FI_EXC_POST | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_07_FI_EXC_POST | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_07_FI_EXC_POST | BUZEI | Number of Line Item Within Accounting Document | NUMC(3) | BUZEI |
| /SKN/S_SW_10_07_FI_EXC_POST | COMP_OPERATOR | Consistency Checks - Comparison operator | CHAR(2) | BUCC_OPERATOR |
| /SKN/S_SW_10_07_FI_EXC_POST | CPUDT | Day On Which Accounting Document Was Entered | DATS(8) | CPUDT |
| /SKN/S_SW_10_07_FI_EXC_POST | DMBE2 | Amount in Second Local Currency | CURR(13,2) | DMBE2 |
| /SKN/S_SW_10_07_FI_EXC_POST | DMBE3 | Amount in Third Local Currency | CURR(13,2) | DMBE3 |
| /SKN/S_SW_10_07_FI_EXC_POST | DMBTR | Amount in Local Currency | CURR(13,2) | DMBTR |
| /SKN/S_SW_10_07_FI_EXC_POST | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_07_FI_EXC_POST | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_07_FI_EXC_POST | FABKL | Factory calendar key | CHAR(2) | FABKL |
| /SKN/S_SW_10_07_FI_EXC_POST | FACDATE | Factory calendar: Factory date | DEC(5) | FACDATE |
| /SKN/S_SW_10_07_FI_EXC_POST | FULL | SW: Flag (' '/'X') | CHAR(1) | /SKN/E_SW_FLAG |
| /SKN/S_SW_10_07_FI_EXC_POST | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /SKN/S_SW_10_07_FI_EXC_POST | GRPID | Batch Input Session Name | CHAR(12) | GRPID_BKPF |
| /SKN/S_SW_10_07_FI_EXC_POST | GVTYP | P&L statement account type | CHAR(2) | GVTYP |
| /SKN/S_SW_10_07_FI_EXC_POST | HKONT | General Ledger Account | CHAR(10) | HKONT |
| /SKN/S_SW_10_07_FI_EXC_POST | HKONT_DESC | G/L Account Long Text | CHAR(50) | TXT50_SKAT |
| /SKN/S_SW_10_07_FI_EXC_POST | HWAE2 | Currency Key of Second Local Currency | CUKY(5) | HWAE2 |
| /SKN/S_SW_10_07_FI_EXC_POST | HWAE3 | Currency Key of Third Local Currency | CUKY(5) | HWAE3 |
| /SKN/S_SW_10_07_FI_EXC_POST | HWAER | Local Currency | CUKY(5) | HWAER |
| /SKN/S_SW_10_07_FI_EXC_POST | KTOKS | G/L Account Group | CHAR(4) | KTOKS |
| /SKN/S_SW_10_07_FI_EXC_POST | KTOPL | Chart of Accounts | CHAR(4) | KTOPL |
| /SKN/S_SW_10_07_FI_EXC_POST | KTOPL_T001 | Chart of Accounts | CHAR(4) | KTOPL |
| /SKN/S_SW_10_07_FI_EXC_POST | KURS2 | Exchange Rate for the Second Local Currency | DEC(9,5) | KURS2 |
| /SKN/S_SW_10_07_FI_EXC_POST | KURS3 | Exchange Rate for the Third Local Currency | DEC(9,5) | KURS3 |
| /SKN/S_SW_10_07_FI_EXC_POST | KURSF | Exchange rate | DEC(9,5) | KURSF |
| /SKN/S_SW_10_07_FI_EXC_POST | KZKRS | Group Currency Exchange Rate | DEC(9,5) | KZKRS |
| /SKN/S_SW_10_07_FI_EXC_POST | KZWRS | Currency Key for the Group Currency | CUKY(5) | KZWRS |
| /SKN/S_SW_10_07_FI_EXC_POST | MONAT | Fiscal Period | NUMC(2) | MONAT |
| /SKN/S_SW_10_07_FI_EXC_POST | REF_FIELD_NAME1 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_07_FI_EXC_POST | REF_FIELD_NAME2 | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_SW_10_07_FI_EXC_POST | RESULT_COMP |  | CURR(15,2) |  |
| /SKN/S_SW_10_07_FI_EXC_POST | SGTXT | Item Text | CHAR(50) | SGTXT |
| /SKN/S_SW_10_07_FI_EXC_POST | SHKZG | Debit/Credit Indicator | CHAR(1) | SHKZG |
| /SKN/S_SW_10_07_FI_EXC_POST | TCODE | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_SW_10_07_FI_EXC_POST | UPDDT | Date of the Last Document Update | DATS(8) | UPDDT |
| /SKN/S_SW_10_07_FI_EXC_POST | USNAM | User name | CHAR(12) | USNAM |
| /SKN/S_SW_10_07_FI_EXC_POST | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_07_FI_EXC_POST | WAERS_FR |  | CUKY(5) |  |
| /SKN/S_SW_10_07_FI_EXC_POST | WERKS | Plant | CHAR(4) | WERKS_D |
| /SKN/S_SW_10_07_FI_EXC_POST | WORKING_DAYS | Fatory calendar flag | CHAR(1) | CIND |
| /SKN/S_SW_10_07_FI_EXC_POST | WRBTR | Amount in document currency | CURR(13,2) | WRBTR |
| /SKN/S_SW_10_07_FI_EXC_POST | XBLNR | Reference Document Number | CHAR(16) | XBLNR1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_07_FI_EXC_POST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_07_FI_EXC_POST OPTIONAL
*"----------------------------------------------------------------------
  DATA_SINGLE:
               FULL           CHAR1,
               BACKDAYS       INT4,
               DATE_REF_FLD   NAME_FELD,
               DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
               LANGU          SPRAS,
               BUZEI          NUMC3,
               GVTYP          CHAR2,
               SHKZG          SHKZG,
               WORKING_DAYS   CIND,
               WFCID          WFCID.
*               ,
*               dmbtr          dmbtr,
*               wrbtr          wrbtr,
*               dmbe2          dmbe2
  DATA_MULTY:
              BUKRS   CHAR10,
              BELNR   CHAR10,
              GJAHR   NUMC4,
              BLART   CHAR10,
              TCODE   CHAR20,
              CPUDT   DATS,
              AEDAT   DATS,
              UPDDT   DATS,
              BUDAT   DATS,
              BLDAT   DATS,
              DMBTR   DMBTR,
              WRBTR   WRBTR,
              DMBE2   DMBE2,
              DMBE3   DMBE3,
              HWAER   HWAER,
              HWAE2   HWAE2,
              HWAE3   HWAE3,
              KTOPL   CHAR4,
              GVTYP   CHAR2,
              USNAM   CHAR12,
              XBLNR   CHAR12,
              MONAT   NUMC2,
              BSTAT   CHAR1,
              GRPID   CHAR12,
              SGXTX   SGTXT,       " Yuri C.++ 01.02.19
              BSCHL   BSCHL,       " Yuri C.++ 01.02.19
              DURATION  /SKN/E_SW_DURATION,
              DATUM    SY-DATUM,
              HKONT    CHAR10,
              XBILK    CHAR1,
              KTOKS    CHAR4,
              SGTXT    TEXT50
              .
  "backdays/date section
  LV_BACKDAYS      = 1.
  LV_DATE_REF_FLD  = 'CPUDT'.    " Documnt Date
  LV_DURATION_UNIT = 'D'.
  LV_LANGU         = 'E'.
  SELECT_SINGLE:
                 BACKDAYS,
                 DATE_REF_FLD,
                 DURATION_UNIT,
                 FULL,
                 SHKZG,         " Yuri C.++ 28.09.19
                 WORKING_DAYS,
                 WFCID.
  SELECT_MULTY:
                BUKRS,
                BELNR,
                GJAHR,
                BLART,
                TCODE,
                CPUDT,
                AEDAT,
                UPDDT,
                BUDAT,
                BLDAT,
                DMBTR,
                WRBTR,
                DMBE2,
                DMBE3,
                HWAER,
                HWAE2,
                HWAE3,
                KTOPL,
                GVTYP,
                USNAM,
                XBLNR,
                MONAT,
                BSTAT,
                GRPID,
                SGXTX,          " Yuri C.++ 01.02.19
                BSCHL,          " Yuri C.++ 01.02.19
                DURATION,
                DATUM,
                HKONT,
                XBILK,
                KTOKS,
                SGTXT
                .
  FIELD-SYMBOLS:  TYPE ANY ,
                 <FS_V> TYPE ANY,
                 <FS_DATA> TYPE /SKN/S_SW_10_07_FI_EXC_POST .
  DATA : SY_TABIX LIKE SY-TABIX ,
         FLD(60) TYPE C ,
         REF_DATE TYPE D.
  DATA : BACKDAYS  TYPE I ,
         DATE_FROM LIKE SY-DATUM,
         TIME_DIFF TYPE  INT4  .
  DATA: LV_HKONT   TYPE CHAR10,
        LV_INDIC   TYPE SCAL-INDICATOR,
        LV_WORK    TYPE SCAL-INDICATOR,
        LV_FACDATE TYPE SCAL-FACDATE,
        LV_DATE    TYPE DATUM,
        LV_DATE_IN TYPE DATUM,
        LV_TABIX   TYPE SYTABIX,
        LV_KTOPL   TYPE T001-KTOPL.
  DATA: LS_DATA     TYPE /SKN/S_SW_10_07_FI_EXC_POST,
        LS_BKPF     TYPE /SKN/S_SW_10_07_FI_EXC_POST,
        LS_BSEG     TYPE BSEG,
        LS_BSEG_TMP TYPE BSEG,
        LS_SKA1     TYPE SKA1.
*        ls_t001     TYPE ty_t001.
  DATA: LT_BKPF     TYPE STANDARD TABLE OF /SKN/S_SW_10_07_FI_EXC_POST,
        LT_BSEG     TYPE STANDARD TABLE OF BSEG,
        LT_BSEG_TMP TYPE STANDARD TABLE OF BSEG,
        LT_SKA1     TYPE STANDARD TABLE OF SKA1,
        LT_DATA     TYPE STANDARD TABLE OF /SKN/S_SW_10_07_FI_EXC_POST.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_07_FI_EXC_POST'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
*** Begin Yuri C.++ 20.07.19
*  IF lv_working_days IS NOT INITIAL.
*    lv_wfcid = 'X'.
*  ENDIF.
*** End Yuri C.++ 20.07.19
  IF R_DATUM[] IS INITIAL .
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'GE' .
    DATE_FROM       = SY-DATUM - LV_BACKDAYS .
    RS_DATUM-LOW    = DATE_FROM .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  CONVERT_MULTY: BELNR ALPHA.
  "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'BUDAT'.
      R_BUDAT[] = R_DATUM[]. "Expected debit date
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "Date on Which Record Was Created
    WHEN 'CPUDT'.
      R_CPUDT[] = R_DATUM[]. "Day On Which Accounting Document Was Entered
    WHEN 'UPDDT'.
      R_UPDDT[] = R_DATUM[]. "Date of the Last Document Update
    WHEN 'BLDAT'.
      R_BLDAT[] = R_DATUM[]. "Document Date in Document
    WHEN OTHERS.
      R_CPUDT[] = R_DATUM[]. "Document date
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_BKPF
    FROM BKPF AS A
    INNER JOIN BSIS AS B       ON  A~BUKRS EQ B~BUKRS
                               AND A~BELNR EQ B~BELNR
                               AND A~GJAHR EQ B~GJAHR
                               AND B~SHKZG EQ LV_SHKZG     " Yuri C++ 28.08.19
    INNER JOIN SKA1  AS C      ON  B~HKONT EQ C~SAKNR
    INNER JOIN T001  AS T      ON  A~BUKRS EQ T~BUKRS        " Yuri C.++ 17.07.19
                               AND C~KTOPL EQ T~KTOPL        " Yuri C.++ 17.07.19
*     INNER JOIN t001w AS t ON b~werks EQ t~werks            " Yuri C.-- 26.01.19
    WHERE A~BUKRS IN R_BUKRS
    AND   A~BELNR IN R_BELNR
    AND   A~GJAHR IN R_GJAHR
    AND   A~BLART IN R_BLART
    AND   A~TCODE IN R_TCODE
    AND   A~HWAER IN R_HWAER
    AND   A~HWAE2 IN R_HWAE2
    AND   A~HWAE3 IN R_HWAE3
    AND   A~BUDAT IN R_BUDAT
    AND   A~AEDAT IN R_AEDAT
    AND   A~CPUDT IN R_CPUDT
    AND   A~UPDDT IN R_UPDDT
    AND   A~BLDAT IN R_BLDAT
    AND   A~XBLNR IN R_XBLNR
    AND   A~MONAT IN R_MONAT
    AND   A~BSTAT IN R_BSTAT
    AND   A~GRPID IN R_GRPID
    AND   B~DMBTR IN R_DMBTR
    AND   B~WRBTR IN R_WRBTR
    AND   B~DMBE2 IN R_DMBE2
    AND   B~DMBE3 IN R_DMBE3
    AND   B~HKONT IN R_HKONT
    AND   B~SGTXT IN R_SGTXT    " Yuri C.++ 01.02.19
    AND   B~BSCHL IN R_BSCHL    " Yuri C.++ 01.02.19
    AND   B~SGTXT IN R_SGTXT
    AND   C~GVTYP IN R_GVTYP
    AND   C~XBILK IN R_XBILK
    AND   C~KTOPL IN R_KTOPL
    AND   C~KTOKS IN R_KTOKS.
  SELECT *
     APPENDING CORRESPONDING FIELDS OF TABLE LT_BKPF
     FROM BKPF AS A INNER JOIN BSAS AS B ON  A~BUKRS EQ B~BUKRS
                                         AND A~BELNR EQ B~BELNR
                                         AND A~GJAHR EQ B~GJAHR
                                         AND B~SHKZG EQ LV_SHKZG     " Yuri C++ 28.08.19
                    INNER JOIN SKA1 AS C ON  B~HKONT EQ C~SAKNR
                    INNER JOIN T001 AS T ON  A~BUKRS EQ T~BUKRS        " Yuri C.++ 17.07.19
                                         AND C~KTOPL EQ T~KTOPL        " Yuri C.++ 17.07.19
*     INNER JOIN t001w AS t ON b~werks EQ t~werks                   " Yuri C.--
     WHERE A~BUKRS IN R_BUKRS
     AND   A~BELNR IN R_BELNR
     AND   A~GJAHR IN R_GJAHR
     AND   A~BLART IN R_BLART
     AND   A~TCODE IN R_TCODE
     AND   A~HWAER IN R_HWAER
     AND   A~HWAE2 IN R_HWAE2
     AND   A~HWAE3 IN R_HWAE3
     AND   A~BUDAT IN R_BUDAT
     AND   A~AEDAT IN R_AEDAT
     AND   A~CPUDT IN R_CPUDT
     AND   A~UPDDT IN R_UPDDT
     AND   A~BLDAT IN R_BLDAT
     AND   A~XBLNR IN R_XBLNR
     AND   A~MONAT IN R_MONAT
     AND   A~BSTAT IN R_BSTAT
     AND   A~GRPID IN R_GRPID
     AND   B~DMBTR IN R_DMBTR
     AND   B~WRBTR IN R_WRBTR
     AND   B~DMBE2 IN R_DMBE2
     AND   B~DMBE3 IN R_DMBE3
     AND   B~HKONT IN R_HKONT
     AND   B~SGTXT IN R_SGTXT
     AND   C~GVTYP IN R_GVTYP
     AND   C~XBILK IN R_XBILK
     AND   C~KTOPL IN R_KTOPL
     AND   C~KTOKS IN R_KTOKS
    .
*** Yuri C.++ 28.12.18
*** Begin Yuri C.-- 11.11.18
*  APPEND LINES OF t_data TO lt_bkpf.
*  REFRESH t_data.
*  LOOP AT lt_bkpf INTO ls_bkpf.
*
*    IF ls_bkpf-usnam IS NOT INITIAL.
*      CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
*        EXPORTING
*          bname      = ls_bkpf-usnam " User Name in User Master Record
*          "SW_DEST    =     " Logical Destination (Specified in Function Call)
*        IMPORTING
*          name_first = ls_bkpf-ad_namefir " First name
*          name_last  = ls_bkpf-ad_namelas " Last name
**         NAME_TEXT  =     " Full Name of Person
**         WA_ADRP    =     " Persons (Business Address Services)
*        EXCEPTIONS
*          no_data    = 1
*          OTHERS     = 2.
*
*      IF sy-subrc <> 0.
**       message id sy-msgid type sy-msgty number sy-msgno
**                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*    ENDIF.
*
*    IF ls_bkpf-hkont IS NOT INITIAL.
*      CALL FUNCTION '/SKN/F_SW_10_GL_DESC'
*        EXPORTING
*          ktopl      = ls_bkpf-ktopl " Chart of Accounts
*          saknr      = ls_bkpf-hkont "  G/L Account Number
*          langu      = lv_langu    " Language Key
**         SW_DEST    =     " Logical Destination (Specified in Function Call)
*        IMPORTING
**         TXT20      =     " G/L account short text
*          txt50      = ls_bkpf-hkont_desc " G/L Account Long Text
*        EXCEPTIONS
*          wrong_code = 1
*          OTHERS     = 2.
*      IF sy-subrc <> 0.
**         message id sy-msgid type sy-msgty number sy-msgno
**                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*    ENDIF.
*
*    APPEND ls_bkpf TO t_data.
*
*  ENDLOOP.
*** End Yuri C.-- 11.11.18
  SORT LT_BKPF BY BUKRS BELNR GJAHR.     " Yuri C.++ 11.11.18
  IF LV_FULL EQ 'X'.
*** Begin Yuri C.-- 11.11.18
*    REFRESH lt_bkpf.
*    APPEND LINES OF t_data TO lt_bkpf.
*    APPEND LINES OF t_data TO lt_data_temp.
*    SORT lt_data_temp BY belnr ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM lt_data_temp COMPARING gjahr belnr.
*** End Yuri C.-- 11.11.18
*** Begin Yuri C.++ 11.11.18
    DELETE ADJACENT DUPLICATES FROM LT_BKPF COMPARING BUKRS BELNR GJAHR.
*** End Yuri C.++ 11.11.18
*    IF lt_data_temp IS NOT INITIAL.    " Yuri C.-- 11.11.18
    IF LT_BKPF IS NOT INITIAL.          " Yuri C.++ 11.11.18
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE LT_BSEG
        FROM BSEG
        FOR ALL ENTRIES IN LT_BKPF
*** Yuri C.-- 11.11.18
*        WHERE a~gjahr EQ lt_data_temp-gjahr
*        AND   a~belnr EQ lt_data_temp-belnr
*** Yuri C.-- 11.11.18
*** Yuri C.++ 11.11.18
        WHERE BUKRS EQ LT_BKPF-BUKRS
        AND   BELNR EQ LT_BKPF-BELNR
        AND   GJAHR EQ LT_BKPF-GJAHR.
      IF LT_BSEG IS NOT INITIAL.
        LT_BSEG_TMP = LT_BSEG.
        SORT LT_BSEG_TMP BY HKONT XBILK GVTYP.
        SELECT *
          FROM SKA1
          INTO TABLE LT_SKA1
          FOR ALL ENTRIES IN LT_BSEG_TMP
          WHERE SAKNR EQ LT_BSEG_TMP-HKONT.
        SORT LT_SKA1 BY SAKNR.
      ENDIF.
*** Yuri C.++ 11.11.18
*** Yuri C.-- 11.11.18
*        AND   a~bukrs IN r_bukrs
*        AND   a~gjahr IN r_gjahr
*** Yuri C.-- 11.11.18
*      SORT lt_bkpf BY gjahr ASCENDING bukrs ASCENDING belnr ASCENDING.
*      SORT lt_bseg BY gjahr ASCENDING bukrs ASCENDING belnr ASCENDING ktopl ASCENDING hkont ASCENDING buzei ASCENDING.
*      DELETE ADJACENT DUPLICATES FROM lt_bkpf COMPARING gjahr bukrs  belnr ktopl hkont buzei.
      REFRESH T_DATA.
      LOOP AT LT_BKPF INTO LS_BKPF.
        CLEAR: LS_DATA.                           " Yuri C.++ 12.11.18
        MOVE-CORRESPONDING LS_BKPF TO LS_DATA.    " Yuri C.++ 12.11.18
        LOOP AT LT_BSEG INTO LS_BSEG WHERE BUKRS EQ LS_BKPF-BUKRS
                                     AND   BELNR EQ LS_BKPF-BELNR
                                     AND   GJAHR EQ LS_BKPF-GJAHR.
*** Yuri C.-- 12.11.18
*          MOVE-CORRESPONDING ls_bkpf TO ls_bseg.
*          lv_dmbtr = ls_bseg-dmbtr.
*          lv_wrbtr = ls_bseg-wrbtr.
*          lv_dmbe2 = ls_bseg-dmbe2.
*          lv_buzei = ls_bseg-buzei.
*          lv_gvtyp = ls_bseg-gvtyp.
*          lv_shkzg = ls_bseg-shkzg.
*          lv_hkont = ls_bseg-hkont.
*          ls_bseg-buzei = lv_buzei.
*          ls_bseg-gvtyp = lv_gvtyp.
*          ls_bseg-shkzg = lv_shkzg.
*          ls_bseg-dmbtr = lv_dmbtr.
*          ls_bseg-dmbe2 = lv_dmbe2.
*          ls_bseg-wrbtr = lv_wrbtr.
*          ls_bseg-hkont = lv_hkont.
*          APPEND ls_bseg TO t_data.
*** Yuri C.-- 12.11.18
*** Yuri C.++ 12.11.18
          MOVE-CORRESPONDING LS_BSEG TO LS_DATA.
          LS_DATA-FULL  = 'X'.
          IF LS_BKPF-USNAM IS NOT INITIAL.
* user name & surname, G/L account description
            CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
              EXPORTING
                BNAME      = LS_BKPF-USNAM " User Name in User Master Record
*               sw_dest    = lv_sw_dest " Logical Destination (Specified in Function Call)
              IMPORTING
                NAME_FIRST = LS_DATA-AD_NAMEFIR " First name
                NAME_LAST  = LS_DATA-AD_NAMELAS " Last name
*               NAME_TEXT  =     " Full Name of Person
*               WA_ADRP    =     " Persons (Business Address Services)
              EXCEPTIONS
                NO_DATA    = 1
                OTHERS     = 2.
          ENDIF.
          CLEAR LS_SKA1.
          READ TABLE LT_SKA1 INTO LS_SKA1 WITH KEY SAKNR = LS_BSEG-HKONT
                                                   BINARY SEARCH.
          IF SY-SUBRC = 0.
            IF LS_SKA1-SAKNR IS NOT INITIAL.
              CALL FUNCTION '/SKN/FC_SW_10_GL_DESC'
                EXPORTING
                  KTOPL      = LS_SKA1-KTOPL " Chart of Accounts
                  SAKNR      = LS_SKA1-SAKNR " G/L Account Number
                  LANGU      = LV_LANGU      " Language Key
*                 sw_dest    = lv_sw_dest    " Logical Destination (Specified in Function Call)
                IMPORTING
*                 TXT20      =                    " G/L account short text
                  TXT50      = LS_DATA-HKONT_DESC " G/L Account Long Text
                EXCEPTIONS
                  WRONG_CODE = 1
                  OTHERS     = 2.
            ENDIF.
          ENDIF.
          APPEND LS_DATA TO T_DATA.
*** Yuri C.++ 12.11.18
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ELSE.
* Not full
    LOOP AT LT_BKPF INTO LS_BKPF.
      IF LS_BKPF-USNAM IS NOT INITIAL.
        CALL FUNCTION '/SKN/FC_SW_01_GET_DETAILES_BUF'
          EXPORTING
            BNAME      = LS_BKPF-USNAM " User Name in User Master Record
            "SW_DEST    =     " Logical Destination (Specified in Function Call)
          IMPORTING
            NAME_FIRST = LS_BKPF-AD_NAMEFIR " First name
            NAME_LAST  = LS_BKPF-AD_NAMELAS " Last name
*           NAME_TEXT  =     " Full Name of Person
*           WA_ADRP    =     " Persons (Business Address Services)
          EXCEPTIONS
            NO_DATA    = 1
            OTHERS     = 2.
      ENDIF.
      IF LS_BKPF-HKONT IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_GL_DESC'
          EXPORTING
            KTOPL      = LS_BKPF-KTOPL " Chart of Accounts
            SAKNR      = LS_BKPF-HKONT "  G/L Account Number
            LANGU      = LV_LANGU    " Language Key
*           SW_DEST    =     " Logical Destination (Specified in Function Call)
          IMPORTING
*           TXT20      =     " G/L account short text
            TXT50      = LS_BKPF-HKONT_DESC " G/L Account Long Text
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
      ENDIF.
      APPEND LS_BKPF TO T_DATA.
    ENDLOOP.
  ENDIF.
*** Begin Yuri C.++ 20.07.19
  CHECK T_DATA IS NOT INITIAL.
  IF LV_WFCID IS NOT INITIAL.
    LT_DATA = T_DATA[].
    REFRESH T_DATA.
    FLD = LV_DATE_REF_FLD.
    CASE LV_DATE_REF_FLD.
      WHEN 'BUDAT'.
        SORT LT_DATA BY BUDAT.
      WHEN 'AEDAT'.
        SORT LT_DATA BY AEDAT.
      WHEN 'CPUDT'.
        SORT LT_DATA BY CPUDT.
      WHEN 'UPDDT'.
        SORT LT_DATA BY UPDDT.
      WHEN 'BLDAT'.
        SORT LT_DATA BY BLDAT.
      WHEN OTHERS.
        SORT LT_DATA BY CPUDT.
    ENDCASE.
    LOOP AT LT_DATA ASSIGNING <FS_DATA>.
      ASSIGN COMPONENT FLD OF STRUCTURE <FS_DATA> TO .
      REF_DATE = .
      IF LV_DATE_IN <> REF_DATE.
        CLEAR: LV_FACDATE, LV_WORK, LV_DATE.
        LV_DATE_IN = REF_DATE.
        CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
          EXPORTING
*           correct_option               = lv_indic
            DATE                         = LV_DATE_IN
            FACTORY_CALENDAR_ID          = LV_WFCID
          IMPORTING
            DATE                         = LV_DATE
            FACTORYDATE                  = LV_FACDATE
            WORKINGDAY_INDICATOR         = LV_WORK
          EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
        IF SY-SUBRC <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF LV_WORK IS INITIAL.
        <FS_DATA>-WORKING_DAYS = C_YES.
      ELSE.
        <FS_DATA>-WORKING_DAYS = C_NO.
      ENDIF.
      <FS_DATA>-FACDATE = LV_FACDATE.  " Factory date related to calendar id
* Filter by Working day/Holiday
      IF LV_WORKING_DAYS IS NOT INITIAL.
        IF LV_WORKING_DAYS EQ C_YES.     " Filter by Working Days
          IF LV_WORK IS NOT INITIAL.     " It's a holiday
            CONTINUE.
          ENDIF.
        ELSEIF LV_WORKING_DAYS EQ C_NO.  " Filter by Holiday
          IF LV_WORK IS INITIAL.    " It's a working day
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND <FS_DATA> TO T_DATA[].
    ENDLOOP.
  ENDIF.
  SORT T_DATA BY BUKRS BELNR GJAHR.
*** End Yuri C.++ 20.07.19
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA ASSIGNING <FS_DATA>.
    SY_TABIX = SY-TABIX .
*** Yuri C.-- 28.12.18
*    CONCATENATE 'T_DATA-' lv_date_ref_fld INTO fld .
*    ASSIGN (fld) TO .
*** Yuri C.-- 28.12.18
*** Yuri C.++ 28.12.18
    FLD = LV_DATE_REF_FLD.
    ASSIGN COMPONENT FLD OF STRUCTURE <FS_DATA> TO .
*** Yuri C.++ 28.12.18
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      <FS_DATA>-DURATION_UNIT = LV_DURATION_UNIT.
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
          <FS_DATA>-DURATION  = TIME_DIFF .
        ELSE.
          <FS_DATA>-DURATION  = '999999'.
        ENDIF.
      ENDIF.
*      MODIFY t_data INDEX sy_tabix.              " Yuri C.-- 28.12.18
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION  NOT IN R_DURATION .
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```