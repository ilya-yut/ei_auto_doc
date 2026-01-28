# Narrative context for Explanation_Credit Memo Monthly volume by Payer _$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.md

- **full_name:** Exceptional sales documents values – Aggregated
- **technical_code:** SW_10_01_ORD_VAL_TOT

## Structure summary (output fields)
AEDAT, AUART, AUDAT, BP1_CODE, BP1_FUNCT, BP1_NAME, BP2_CODE, BP2_FUNCT, BP2_NAME, BP3_CODE, BP3_FUNCT, BP3_NAME, BSTNK, CUST_DESC, DURATION, DURATION_UNIT, ERDAT, ERNAM, ERZET, KUNNR, NETWR, NETWR_FR, OBJNR, SPART, VBELN, VBTYP, VDATU, VKBUR, VKGRP, VKORG, VTWEG, WAERK, WAERK_FR, AEDAT, AUART, AUDAT, BP1_CODE, BP1_FUNCT, BP1_NAME, BP2_CODE, BP2_FUNCT, BP2_NAME, BP3_CODE, BP3_FUNCT, BP3_NAME, BSTNK, CUST_DESC, DURATION, DURATION_UNIT, ERDAT

## Parameters (Name (Description))
- AEDAT (Changed On)
- AGGR_FIELDS (Aggregation Fields)
- AGGR_PERIOD (Aggregation Period(Y,Q,M,W))
- AUART (Sales Document Type)
- AUDAT (Document Date)
- BACKDAYS (Days Backward from today)
- BP1_CODE (Customer Code 1)
- BP1_FUNCT (Partner Function 1)
- BP1_NAME (Name)
- BP2_CODE (Customer code 2)
- BP2_FUNCT (Partner Function 2)
- BP2_NAME (Name)
- BP3_CODE (Customer code 3)
- BP3_FUNCT (Partner Function 3)
- BP3_NAME (Name)
- BSTNK (Purchase order no.)
- CUST_DESC (Name)
- DATE_REF_FLD (Date reference field)
- DURATION (Duration In Time Units)
- DURATION_UNIT (Duration Unit(D/H/M))
- ERDAT (Created On)
- ERNAM (Created By)
- ERZET (Time)
- KUNNR (Sold-to party)
- NETWR (Net value)
- NETWR_FR (Net value)
- SPART (Division)
- STAT (Status)
- TOT_CNT (Natural number)
- TOT_NETWR (Total Net value)
- TOT_NETWR_FR (Tot. Foreign Net value)
- USER_FLD (Dynamic Recipient User Field)
- VBELN (Sales Document)
- VBTYP (SD document categ.)
- VDATU (Requested deliv.date)
- VKBUR (Sales Office)
- VKGRP (Sales Group)
- VKORG (Sales Organization)
- VTWEG (Distribution Channel)
- WAERK (Document Currency)
- WAERK_FR (Foreign Currency)

## Code path or excerpt
Path: C:\vibe code dev\ei_auto_doc\input\Code_Credit Memo Monthly volume by Payer _$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.txt
Excerpt (first 8000 chars):
```
FUNCTION /SKN/F_SW_10_01_ORD_VAL_TOT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_ORD_VAL_TOT OPTIONAL
*"----------------------------------------------------------------------
data: ls_DATA_DET type /SKN/S_SW_10_01_ORD_VAL_NEW,
      lt_DATA_DET like TABLE OF ls_DATA_DET.
*FIELD-SYMBOLS <fs_DATA_DET> type /SKN/S_SW_10_01_ORD_VAL_NEW.
data: ls_DATA like LINE OF T_DATA.
types: ty_aggr_period(8) type N,
       ty_aggr_arg type string.
data: lv_aggr_period_val type ty_aggr_period,
      lv_aggr_arg_val type ty_aggr_arg.
data: begin of ls_DET_AGGR.
  include structure /SKN/S_SW_10_01_ORD_VAL_NEW.
data: aggr_period type ty_aggr_period,
      aggr_arg type ty_aggr_arg.
data: end of ls_DET_AGGR.
data: lt_DET_AGGR like TABLE OF ls_DET_AGGR.
FIELD-SYMBOLS <fs_DET_AGGR> like ls_DET_AGGR.
data: begin of ls_AGGR,
        aggr_period  type ty_aggr_period,
        aggr_arg     type ty_aggr_arg,
        TOT_NETWR	   type NETWR_AK,
        TOT_NETWR_FR type NETWR_AK,
        TOT_CNT      type INT4,
      end of ls_AGGR.
*data: lt_AGGR like  TABLE OF ls_AGGR.
data: lt_AGGR like HASHED TABLE OF ls_AGGR
               WITH UNIQUE KEY aggr_arg aggr_period.
*FIELD-SYMBOLS <fs_AGGR> like ls_AGGR.
*data: begin of ls_AGGR_key,
*        aggr_period  type ty_aggr_period,
*        aggr_arg     type ty_aggr_arg,
*      end of ls_AGGR_key.
data : fld(60) type c .
data : ref_date type D.
FIELD-SYMBOLS: <fs> TYPE ANY ,
               <fs_v> TYPE ANY .
data : ref_string(60) type C.
data: begin of ls_AGGR_FIELDS,
       FIELDNAME type FIELDNAME,
      end of ls_AGGR_FIELDS.
data: lt_AGGR_FIELDS like TABLE OF ls_AGGR_FIELDS.
data:  date_from LIKE sy-datum,
       date_to LIKE sy-datum,
       date_from_start_period LIKE sy-datum.
data_multy: AGGR_FIELDS  NAME_KOMP,
            TOT_NETWR	   NETWR_AK,
            TOT_NETWR_FR NETWR_AK,
            TOT_CNT      INT4,
            DATUM        sy-datum,
            STAT         J_STATUS.
data_single: AGGR_PERIOD  CHAR1,  " M - Month/W - Week/Q - Qurter/Y - Year
             DATE_REF_FLD NAME_FELD,
             BACKDAYS     INT4,
             FORWDAYS     INT4.
select_multy: DATUM,
              AGGR_FIELDS,
              TOT_NETWR,
              TOT_NETWR_FR,
              TOT_CNT,
              STAT.
 lv_DATE_REF_FLD = 'VDATU'."Delivery Date)
 lv_AGGR_PERIOD = 'M'.
 lv_backdays = 0.
 select_single: AGGR_PERIOD,
                DATE_REF_FLD,
                BACKDAYS,
                FORWDAYS.
 IF NOT lv_forwdays  IS INITIAL.
   lv_backdays = lv_forwdays * ( -1 ).
 ENDIF.
 IF r_datum[] IS INITIAL .
   rs_datum-sign = 'I' .
   rs_datum-option = 'GE' .
   date_from = sy-datum - lv_backdays .
   rs_datum-low = date_from .
   APPEND rs_datum TO r_datum.
 ENDIF.
  "--- Set Reference Date Field
  date_from = sy-datum.
  READ TABLE r_datum INTO rs_datum INDEX 1.
  IF sy-subrc IS INITIAL.
    date_from = rs_datum-low.
    date_to = rs_datum-high.
    IF date_to < date_from.
      date_to = date_from.
    ENDIF.
  ENDIF.
 refresh lt_AGGR_FIELDS.
 if R_AGGR_FIELDS[] is not initial.
   select *
     into CORRESPONDING FIELDS OF TABLE lt_AGGR_FIELDS
     from DD03L
     where TABNAME = '/SKN/S_SW_10_01_ORD_VAL_TOT'
       and FIELDNAME in R_AGGR_FIELDS.
 endif.
 "--- Prepare Dates for Aggregation (Month/...)
  "-- Save Orogina Dates Selection
   data_multy: DATUM_SRC    sy-datum. " To save original Selection
   R_DATUM_SRC[] = R_DATUM[].
  "-- Set full Date Period Interval
    perform calc_aggr_period_start_date using date_from lv_AGGR_PERIOD
                                        changing date_from_start_period.
    READ TABLE r_datum INTO rs_datum INDEX 1.
    IF sy-subrc IS INITIAL.
      rs_datum-low = date_from_start_period.
      modify r_datum from rs_datum index sy-tabix.
    endif.
    "--- Substitute DATUM condition in T_SELECT
    delete T_SELECT where FIELDNM = 'DATUM'.
    loop at r_datum INTO rs_datum.
      MOVE-CORRESPONDING rs_datum to T_SELECT.
      T_SELECT-FIELDNM = 'DATUM'.
      append T_SELECT.
    endloop.
 CALL FUNCTION '/SKN/F_SW_10_01_ORD_VAL_NEW'
   IMPORTING
     IS_ALERT       = IS_ALERT
   TABLES
     T_SELECT       = T_SELECT
     T_DATA         = lt_DATA_DET.
  "--- Prepare Aggregation Arguments
  loop at lt_DATA_DET into ls_DATA_DET.
    MOVE-CORRESPONDING ls_DATA_DET to ls_DET_AGGR.
    "--- Fill Aggregation Arguments
     "-- Aggr Period
      concatenate 'ls_DATA_DET-' lv_DATE_REF_FLD into fld .
      ASSIGN (fld) TO <fs>.
      if <fs> is not ASSIGNED.
        continue.
      endif.
      ref_date = <fs> .
      perform calculate_aggr_period_val using ref_date lv_AGGR_PERIOD
                                        changing lv_aggr_period_val.
      ls_DET_AGGR-aggr_period = lv_aggr_period_val.
     "-- Aggr Key
      clear lv_aggr_arg_val.
      loop at lt_AGGR_FIELDS into ls_AGGR_FIELDS.
        concatenate 'ls_DATA_DET-' ls_AGGR_FIELDS-FIELDNAME into fld .
        ASSIGN (fld) TO <fs>.
        if <fs> is not ASSIGNED.
          continue.
        endif.
        write <fs> to ref_string.
        concatenate lv_aggr_arg_val ref_string into lv_aggr_arg_val.
      endloop.
      ls_DET_AGGR-aggr_arg = lv_aggr_arg_val.
    append ls_DET_AGGR to lt_DET_AGGR.
    MOVE-CORRESPONDING ls_DET_AGGR to ls_AGGR.
    ls_AGGR-TOT_CNT = 1.
    ls_AGGR-TOT_NETWR = ls_DET_AGGR-NETWR.
    ls_AGGR-TOT_NETWR_FR = ls_DET_AGGR-NETWR_FR.
    COLLECT ls_AGGR into lt_AGGR.
  endloop.
  "--- Total Filtering
  delete lt_AGGR where TOT_CNT not in R_TOT_CNT.
  delete lt_AGGR where TOT_NETWR not in R_TOT_NETWR.
  delete lt_AGGR where TOT_NETWR_FR not in R_TOT_NETWR_FR.
  "--- Fill result Table
  loop at lt_DET_AGGR into ls_DET_AGGR.
    "--- Filter for Original Date Selection
     "-- Get Date ref Field
      concatenate 'ls_DET_AGGR-' lv_DATE_REF_FLD into fld .
      ASSIGN (fld) TO <fs>.
      if <fs> is not ASSIGNED.
        continue.
      endif.
      ref_date = <fs> .
      "-- Filter
      if ref_date not in R_DATUM_SRC.
        continue.
      endif.
    READ TABLE lt_AGGR into ls_AGGR
                       WITH KEY aggr_period = ls_DET_AGGR-aggr_period
                                aggr_arg    = ls_DET_AGGR-aggr_arg.
    if sy-subrc is initial.
      MOVE-CORRESPONDING ls_DET_AGGR to ls_DATA.
      MOVE-CORRESPONDING ls_AGGR to ls_DATA.
      append ls_DATA to t_data.
    endif.
  endloop.
*--- Check Alert Information
 read table t_data index 1.
 check not sy-tfill  is initial .
 IS_ALERT = 'X' .
ENDFUNCTION.
```

## Benchmark path (for verification)
C:\vibe code dev\ei_auto_doc\reference files\Explanation_Credit Memo Monthly volume by Payer _$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.md

## Instructions for Cursor
Use this context plus reference/sample_format.md and reference/_Parameters.docx to generate General Overview, Problem Description, Suggested Resolution. **Strictly follow all .cursor rules** (abap-function-doc-format-authority, document-authority, anti-hallucination-rules, behavior/verify-after-write). **Verify each drafted section** against (a) .cursor rules, (b) reference/sample_format.md, (c) benchmark (path below) **before** replacing the placeholder. Replace a [TODO: ...] only when the content passes all three. Read the file after each edit to confirm.
