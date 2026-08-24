## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_01_01_SOURCE_SCAN | ACTFLG | tp Active Flag | CHAR(1) | TRTPACTFLG |
| /SKN/S_SW_01_01_SOURCE_SCAN | ACTIVITY | Activity that wrote the entry to the object list | CHAR(20) | TRACTIVITY |
| /SKN/S_SW_01_01_SOURCE_SCAN | AS4DATE | Date of Last Change | DATS(8) | AS4DATE |
| /SKN/S_SW_01_01_SOURCE_SCAN | AS4TIME | Last changed at | TIMS(6) | AS4TIME |
| /SKN/S_SW_01_01_SOURCE_SCAN | AS4USER | Owner of a Request or Task | CHAR(12) | TR_AS4USER |
| /SKN/S_SW_01_01_SOURCE_SCAN | CDAT | Created on | DATS(8) | RDIR_CDATE |
| /SKN/S_SW_01_01_SOURCE_SCAN | CNAM | Author | CHAR(12) | CNAM |
| /SKN/S_SW_01_01_SOURCE_SCAN | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_01_SOURCE_SCAN | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_01_SOURCE_SCAN | IMPSING | General Flag | CHAR(1) | FLAG |
| /SKN/S_SW_01_01_SOURCE_SCAN | LANG | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_01_01_SOURCE_SCAN | LINE_NO | Line number | NUMC(6) | RSROW |
| /SKN/S_SW_01_01_SOURCE_SCAN | LINE_SCAN |  | CHAR(255) | /SKN/E_SW_SOURCE_SCAN_STRING |
| /SKN/S_SW_01_01_SOURCE_SCAN | LOCKFLAG | Lock status or import status of an object entry | CHAR(1) | LOCKFLAG |
| /SKN/S_SW_01_01_SOURCE_SCAN | OBJECT | Object Type | CHAR(4) | TROBJTYPE |
| /SKN/S_SW_01_01_SOURCE_SCAN | OBJFUNC | Object function | CHAR(1) | OBJFUNC |
| /SKN/S_SW_01_01_SOURCE_SCAN | OBJ_NAME | Object Name in Object List | CHAR(120) | TROBJ_NAME |
| /SKN/S_SW_01_01_SOURCE_SCAN | PGMID | Program ID in Requests and Tasks | CHAR(4) | PGMID |
| /SKN/S_SW_01_01_SOURCE_SCAN | PROJECT | Project in Change and Transport System | CHAR(20) | TRKORR_P |
| /SKN/S_SW_01_01_SOURCE_SCAN | STRING_SEARCH |  | CHAR(72) | /SKN/E_SW_SOURCE_SEARCH_STRING |
| /SKN/S_SW_01_01_SOURCE_SCAN | STRKORR | Higher-Level Request | CHAR(20) | STRKORR |
| /SKN/S_SW_01_01_SOURCE_SCAN | SUBC | Program type | CHAR(1) | SUBC |
| /SKN/S_SW_01_01_SOURCE_SCAN | TARCLI | Target client for the request | CHAR(3) | TRTARCLI |
| /SKN/S_SW_01_01_SOURCE_SCAN | TARSYSTEM | Transport Target of Request | CHAR(10) | TR_TARGET |
| /SKN/S_SW_01_01_SOURCE_SCAN | TRFUNCTION | Type of request/task | CHAR(1) | TRFUNCTION |
| /SKN/S_SW_01_01_SOURCE_SCAN | TRFUNCTION_TEXT | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_01_SOURCE_SCAN | TRKORR | Request/Task | CHAR(20) | TRKORR |
| /SKN/S_SW_01_01_SOURCE_SCAN | TRSTATUS | Status of request/task | CHAR(1) | TRSTATUS |
| /SKN/S_SW_01_01_SOURCE_SCAN | TRSTATUS_TEXT | Short Description of Repository Objects | CHAR(60) | AS4TEXT |
| /SKN/S_SW_01_01_SOURCE_SCAN | UDAT | Changed On | DATS(8) | RDIR_UDATE |
| /SKN/S_SW_01_01_SOURCE_SCAN | UNAM | Last changed by | CHAR(12) | UNAM |
| /SKN/S_SW_01_01_SOURCE_SCAN | VERN | Version number | CHAR(6) | VERN |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_01_01_SOURCE_SCAN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_01_SOURCE_SCAN OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_OBJECT,
* TADIR
           PGMID      TYPE PGMID,
           OBJECT     TYPE TROBJTYPE,
           OBJ_NAME   TYPE SOBJ_NAME,
           SRCSYSTEM  TYPE SRCSYSTEM,
           AUTHOR     TYPE RESPONSIBL,
           DEVCLASS   TYPE DEVCLASS,
           CREATED_ON TYPE CREATIONDT,
* PROGDIR
           STATE      TYPE STATE,
           SUBC       TYPE SUBC,
           APPL       TYPE RDIR_APPL,
           CNAM       TYPE CNAM,
           CDAT       TYPE RDIR_CDATE,
           UNAM       TYPE UNAM,
           UDAT       TYPE RDIR_UDATE,
           VERN       TYPE VERN,
* D010INC
           INCLUDE    TYPE INCLUDE,
         END OF TY_OBJECT,
         TT_OBJECT TYPE STANDARD TABLE OF TY_OBJECT.
  DATA_SINGLE: DURATION_UNIT   /SKN/E_SW_DURATION_UNIT ,
               BACKDAYS        INT4,
               LANGU           LANGU,
               SW_DEST         RFCDEST.
  DATA_MULTY:
* TADIR
              PGMID        PGMID,              " Program Id
              OBJTYPE      TROBJTYPE,          " Object Type - FUGR/CLAS/PROG
              OBJNAME      SOBJ_NAME,          " Object Name
              SRCSYSTEM    SRCSYSTEM,          " Original System
              AUTHOR       RESPONSIBL,         " Person Responsible
              DEVCLASS     DEVCLASS,           " Package
              CREATEDON    CREATIONDT,         " Creation Date
*              domnam       tmsdomnam,          "
*              sysnam       tmssysnam,          " System Name(For)
* PROGDIR
              STATE        R3STATE,            " Object Status
              SUBC         SUBC,               " Program type
              CNAM         CNAM,               " Created by
              CDAT         RDIR_CDATE,         " Created on
              UNAM         UNAM,               " Last changed by
              UDAT         RDIR_UDATE,         " Last changed on
              VERN         VERN,               " Version Number
* D010INC/E071
              INCLUDE      INCLUDE,            " Include
              TRKORR       TRKORR,             " Request/task
*              strkorr      strkorr,            " High level request
*              maxrc        trretcode,
*              int_maxrc    int1,
*              tarcli       trtarcli,           " Targ Clnt
*              comsys       tmssysnam,          " System Name(from)
*              srccli       trclient,           " Source client
              STRING_SEARCH /SKN/E_SW_SOURCE_SCAN_STRING,  " String Serach
              TRSTATUS      TRSTATUS,                      " Status of request/task
              TRFUNCTION    TRFUNCTION,                    " Type of request/task
              AS4USER       TR_AS4USER,
*              object        trobjtype,
*              obj_name      trobj_name,
*              state_color   /skn/e_sw_state_color,
              DATUM         SY-DATUM,
              DURATION     /SKN/E_SW_DURATION.
  LV_DURATION_UNIT = 'H'.
  LV_LANGU         = 'E'.
  LV_BACKDAYS      = 1.
  SELECT_SINGLE: BACKDAYS,
                 DURATION_UNIT,
                 LANGU,
                 SW_DEST.
  SELECT_MULTY:
* TADIR
                PGMID,     " Program id
                OBJTYPE,   " Object type
                OBJNAME,   " Object name
                SRCSYSTEM, " Original system
                AUTHOR,    " Person responsible
                CREATEDON, " Creation Date
*                domnam,    "
*                sysnam,    " System Name(For)
* PROGDIR
                STATE,     " Object status
                SUBC,      " Program type
                CNAM,      " Created by
                CDAT,      " Created on
                UNAM,      " Last changed by
                UDAT,      " Last changed on
                VERN,      " Version number
* D010INC/E071
                INCLUDE,       " Include
                TRKORR,        " Request/task
*                strkorr,
*                maxrc,
*                int_maxrc,
*                tarcli,
*                comsys,  " System Name(from)
*                srccli,
*                trstatus,
*                trfunction,
*                as4user,
*                pgmid,
*                object,
*                obj_name,
*                state_color,
                STRING_SEARCH,   " String Search
                DATUM,
                DURATION.
  DATA: R_TAB_COND TYPE RANGE OF SO_TEXT.
*        rs_tab_cond LIKE LINE OF r_tab_cond.
  DATA: SY_DATLO LIKE SY-DATLO,
        SY_TIMLO LIKE SY-TIMLO.
  DATA: IS_GENERAL(1) TYPE C,
        DATE_FROM     LIKE SY-DATUM,
        BACKDAYS      TYPE I,
        SY_TABIX      LIKE SY-TABIX,
        FROM_TABIX    LIKE SY-TABIX,
        ENDDATE       LIKE SY-DATUM,
        ENDTIME       LIKE SY-UZEIT,
        TIME_DIFF     TYPE INT4,
        IS_OUT(1)     TYPE C,
        MAX_RC        TYPE TRRETCODE,
        MAX_RC_N(4)   TYPE N.
  DATA: LV_TRKORR   TYPE TRKORR,
        LV_OBJ      TYPE TADIR-OBJ_NAME,
        LV_TABIX    TYPE I,
        LV_OBJ_NAME TYPE VERSOBJNAM,
        LV_LINE_NUM TYPE I,
        LV_WHILE    TYPE I,
        LV_STRING   TYPE STRING,
        LV_TABNAME  TYPE DDOBJNAME.
  DATA: LS_DATA     LIKE LINE OF T_DATA,
        LS_QUEUE    TYPE TMSBUFFER,
        LS_OBJ      TYPE TY_OBJECT,
        LS_DD07V    TYPE DD07V,
        LS_MAIN_REQ TYPE /SKN/S_SW_TRKOR.
  DATA: LS_SEL_FIELDS TYPE /SKN/S_SEL_FIELDS,
        LS_ABAP       TYPE ABAPTXT255,
        LS_TRDIR      TYPE TRDIR,
        LS_RESULT     TYPE MATCH_RESULT,
        LS_TRANSP_DET TYPE /SKN/S_SW_01_01_TRANSP_DETAILS.
  DATA: LT_MAIN_REQ   LIKE TABLE OF LS_MAIN_REQ,
        LT_STR_SPLIT  TYPE TABLE OF STRING,
        LT_DATA       LIKE TABLE OF LS_DATA,
        LT_QUEUE      LIKE TABLE OF LS_QUEUE,
        LT_OBJECT     TYPE TT_OBJECT,
        LT_ABAP       TYPE ABAPTXT255_TAB,
        LT_TRDIR      TYPE STANDARD TABLE OF TRDIR,
        LT_RESULT     TYPE MATCH_RESULT_TAB,
        LT_DD07V_STAT TYPE STANDARD TABLE OF DD07V,
        LT_DD07V_FUNC TYPE STANDARD TABLE OF DD07V,
        LT_DD07V_N    TYPE STANDARD TABLE OF DD07V,
        LT_TRANSP_DET TYPE STANDARD TABLE OF /SKN/S_SW_01_01_TRANSP_DETAILS.
** Begin OF RFC CALL
  DATA: LT_OPTION TYPE TABLE OF RFC_DB_OPT,
        LT_DATA_RFC TYPE TABLE OF /SKN/S_SW_TAB2000,
        LT_TABLES_LIST  TYPE  /SKN/TT_TABLES,
        LT_JOIN_CONDITION TYPE  /SKN/TT_TABLE_JOIN,
        LWA_JOIN_CONDITION LIKE LINE OF LT_JOIN_CONDITION[],
        LT_SEL_FIELDS TYPE  /SKN/TT_SEL_FIELDS,
        LT_SORT_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT,
        LT_OUTPUT_FIELDS  TYPE  /SKN/TT_RFC_DB_FLD_EXTEND,
        LT_DFIES  TYPE TABLE OF  DFIES,
        LT_RETURN TYPE  BAPIRET2_T,
        LV_ROWCOUNT TYPE SOID-ACCNT,
        LWA_TABLES_LIST LIKE LINE OF LT_TABLES_LIST[],
        LT_IN_RANGE	TYPE TABLE OF	/SKN/S_SW_RANGE_TAB,
        LT_OUT_WHERE_COND	TYPE TABLE OF	/SKN/S_SW_WHERE_TAB,
        LWA_IN_RANGE LIKE LINE OF LT_IN_RANGE,
        LWA_OUT_WHERE_COND LIKE LINE OF LT_OUT_WHERE_COND.
  FIELD-SYMBOLS: <FS_OBJ> TYPE TY_OBJECT.
  _GET_CURRENT_DATE_TIME ' ' LV_SW_DEST SY_DATLO SY_TIMLO.
  IF R_DATUM[] IS INITIAL.
    RS_DATUM-SIGN   = 'I'.
    RS_DATUM-OPTION = 'GE'.
    DATE_FROM       = SY_DATLO - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM.
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
*--- Retrieve data
  CLEAR: IS_ALERT.
  REFRESH: T_DATA, LT_DATA.
******************************************** Get Transport Details **********************************************************************
  CALL FUNCTION '/SKN/FC_SW_01_01_TRANSP_DETAIL'
* IMPORTING
*   IS_ALERT       =
    TABLES
      T_SELECT = T_SELECT[]
      T_DATA   = LT_TRANSP_DET.
******************************************** Get Transport Details **********************************************************************
******************************************** Check and Get Program's Includes ***********************************************************
*  LOOP AT lt_transp_det INTO ls_transp_det.
*
*    CHECK ls_transp_det-object EQ gc_object_reps OR   " Report
*          ls_transp_det-object EQ gc_object_prog OR   " Program/Include
*          ls_transp_det-object EQ gc_object_func OR   " FM
*          ls_transp_det-object EQ gc_object_meth.     " Class Method
*
*    rs_objname-sign   = 'I'.
*    rs_objname-option = 'EQ'.
*    rs_objname-low    = ls_transp_det-obj_name.
*
*    APPEND rs_objname TO r_objname.
*    CLEAR: rs_objname.
*
*  ENDLOOP.
*
*  CHECK r_objname[] IS NOT INITIAL.
*
*  SORT r_objname BY low.
*  DELETE ADJACENT DUPLICATES FROM r_objname COMPARING low.
*
*  REFRESH lt_tables_list[].
*  _append_tables_list 'TADIR'   '' 'T'.    " Repository Table
**  _append_tables_list 'D010INC' '' 'D'.    " Includes Table
*  _append_tables_list 'PROGDIR' '' 'P'.    " General Table
*
** TADIR
*  _range_to_sel_table 'T~PGMID'      pgmid.       " Program Id
*  _range_to_sel_table 'T~OBJECT'     objtype.     " Object Type
*  _range_to_sel_table 'T~OBJ_NAME'   objname.     " Object Name
*  _range_to_sel_table 'T~SRCSYSTEM'  srcsystem.   " Original System
*  _range_to_sel_table 'T~AUTHOR'     author.      " Person Responsible
*  _range_to_sel_table 'T~DEVCLASS'   devclass.    " Package
*  _range_to_sel_table 'T~CREATED_ON' createdon.   " Creation Date
*
** PROGDIR
*  _range_to_sel_table 'P~STATE'      state.      " Object Status
*  _range_to_sel_table 'P~SUBC'       subc.       " Program type
*  _range_to_sel_table 'P~CNAM'       cnam.       " Created by
*  _range_to_sel_table 'P~CDAT'       cdat.       " Created on
*  _range_to_sel_table 'P~UNAM'       unam.       " Last changed by
*  _range_to_sel_table 'P~UDAT'       udat.       " Last changed on
*  _range_to_sel_table 'P~VERN'       vern.       " Version Number
*
** D010INC
**  _range_to_sel_table 'D~INCLUDE'    include.    " Include
*
*  lt_option[] = lt_out_where_cond[].
*
*  REFRESH lt_sel_fields[].
*
** TADIR
*  ls_sel_fields-table = 'TADIR'.
*
*  ls_sel_fields-field = 'OBJECT'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'OBJ_NAME'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'SRCSYSTEM'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'AUTHOR'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'DEVCLASS'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'CREATED_ON'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
** PROGDIR
*  ls_sel_fields-table = 'PROGDIR'.
*
*  ls_sel_fields-field = 'NAME'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'STATE'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'SUBC'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'CNAM'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'CDAT'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'UNAM'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*  ls_sel_fields-field = 'UDAT'.
*  APPEND ls_sel_fields TO lt_sel_fields.
*
*** D010INC
**  ls_sel_fields-table = 'D010INC'.
**
**  ls_sel_fields-field = 'INCLUDE'.
**  APPEND ls_sel_fields TO lt_sel_fields.
*
*  _join_condition 'P' 'NAME'     'T' 'OBJ_NAME'.
**  _join_condition 'T' 'OBJ_NAME' 'D' 'MASTER'.
*
*  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
*    DESTINATION lv_sw_dest
*    IMPORTING
*      rowcount             = lv_rowcount
*    TABLES
*      options              = lt_option[]
*      data                 = lt_data_rfc[]
*      tables_list          = lt_tables_list[]
*      join_condition       = lt_join_condition[]
*      sel_fields           = lt_sel_fields[]
*      sort_options         = lt_sort_options[]
*      output_fields        = lt_output_fields[]
*      dfies                = lt_dfies[]
*      return               = lt_return[]
*    EXCEPTIONS
*      table_not_available  = 1
*      table_without_data   = 2
*      option_not_valid     = 3
*      field_not_valid      = 4
*      not_authorized       = 5
*      data_buffer_exceeded = 6
*      OTHERS               = 7.
*  IF sy-subrc IS NOT INITIAL OR lt_return IS NOT INITIAL.
*
*  ELSE.
*    _rfc_to_t_data lt_data_rfc lt_object lt_output_fields.
*  ENDIF.
******************************************** Check and Get Program's Includes ***********************************************************
*  CHECK lt_object IS NOT INITIAL.
  CHECK LT_TRANSP_DET IS NOT INITIAL.
* Get domain text values
  CALL FUNCTION 'DD_DOMA_GET'
    DESTINATION LV_SW_DEST
    EXPORTING
      DOMAIN_NAME = 'TRSTATUS'
*     GET_STATE   = 'M  '
*     LANGU       = SY-LANGU
    TABLES
      DD07V_TAB_A = LT_DD07V_STAT
      DD07V_TAB_N = LT_DD07V_N.
* Get domain text values
  CALL FUNCTION 'DD_DOMA_GET'
    DESTINATION LV_SW_DEST
    EXPORTING
      DOMAIN_NAME = 'TRFUNCTION'
*     GET_STATE   = 'M  '
*     LANGU       = SY-LANGU
    TABLES
      DD07V_TAB_A = LT_DD07V_FUNC
      DD07V_TAB_N = LT_DD07V_N.
  LOOP AT LT_TRANSP_DET INTO LS_TRANSP_DET.
    CLEAR: LT_RESULT, LT_ABAP, LT_TRDIR.
    LV_OBJ_NAME = LS_TRANSP_DET-OBJ_NAME.
    CHECK LS_TRANSP_DET-OBJECT EQ GC_OBJECT_REPS OR   " Report
          LS_TRANSP_DET-OBJECT EQ GC_OBJECT_REPO OR   " Report
          LS_TRANSP_DET-OBJECT EQ GC_OBJECT_PROG OR   " Program/Include
          LS_TRANSP_DET-OBJECT EQ GC_OBJECT_FUNC OR   " FM
          LS_TRANSP_DET-OBJECT EQ GC_OBJECT_METH.     " Class Method
* due to object type 'PROG' not processed in next FM
    IF LS_TRANSP_DET-OBJECT EQ GC_OBJECT_PROG.
      LS_TRANSP_DET-OBJECT = GC_OBJECT_REPS.
    ENDIF.
* Read object code and additional technical details
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      DESTINATION LV_SW_DEST
      EXPORTING
        OBJECT_NAME = LV_OBJ_NAME
        OBJECT_TYPE = LS_TRANSP_DET-OBJECT
*       versno      =
*       destination =
*       IV_NO_RELEASE_TRANSFORMATION       = ' '
      TABLES
        REPOS_TAB   = LT_ABAP
        TRDIR_TAB   = LT_TRDIR
      EXCEPTIONS
        NO_VERSION  = 1
        OTHERS      = 2.
    IF SY-SUBRC IS INITIAL.
      LOOP AT R_STRING_SEARCH INTO RS_STRING_SEARCH.
        CLEAR: LV_STRING,
               LS_ABAP.
        FIND ALL OCCURRENCES OF REGEX RS_STRING_SEARCH-LOW IN TABLE LT_ABAP
          IN CHARACTER MODE
          IGNORING CASE
          RESULTS LT_RESULT.
        CHECK LT_RESULT IS NOT INITIAL.
        SORT LT_RESULT BY LINE.
        DELETE ADJACENT DUPLICATES FROM LT_RESULT COMPARING LINE.
        IF RS_STRING_SEARCH-LOW(6) EQ 'MODIFY' OR
           RS_STRING_SEARCH-LOW(6) EQ 'UPDATE' OR
           RS_STRING_SEARCH-LOW(6) EQ 'INSERT' OR
           RS_STRING_SEARCH-LOW(6) EQ 'DELETE'.
          LOOP AT LT_RESULT INTO LS_RESULT.
            LV_TABIX = SY-TABIX.
            READ TABLE LT_ABAP INTO LS_ABAP INDEX LS_RESULT-LINE.
            IF SY-SUBRC IS INITIAL.
              SHIFT LS_ABAP-LINE LEFT DELETING LEADING SPACE.
              SPLIT LS_ABAP-LINE AT SPACE INTO TABLE LT_STR_SPLIT.
              READ TABLE LT_STR_SPLIT INTO LV_STRING INDEX 2.
            ENDIF.
* Check if the commands related to change
            IF LV_STRING IS NOT INITIAL.
              LV_TABNAME = LV_STRING.
              CALL FUNCTION 'CHECK_TAB_NAME'
                DESTINATION LV_SW_DEST
                EXPORTING
                  I_TABNAME     = LV_TABNAME
                EXCEPTIONS
                  TAB_NOT_FOUND = 1
                  OTHERS        = 2.
              IF SY-SUBRC IS NOT INITIAL.
                READ TABLE LT_STR_SPLIT INTO LV_STRING INDEX 3.
                IF LV_STRING IS NOT INITIAL.
                  LV_TABNAME = LV_STRING.
                  CALL FUNCTION 'CHECK_TAB_NAME'
                    DESTINATION LV_SW_DEST
                    EXPORTING
                      I_TABNAME     = LV_TABNAME
                    EXCEPTIONS
                      TAB_NOT_FOUND = 1
                      OTHERS        = 2.
                ENDIF.
                IF SY-SUBRC IS NOT INITIAL.
                  DELETE LT_RESULT INDEX LV_TABIX.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        CHECK LT_RESULT IS NOT INITIAL.
        MOVE-CORRESPONDING LS_TRANSP_DET TO LS_DATA.
        READ TABLE LT_TRDIR INTO LS_TRDIR INDEX 1.
        IF SY-SUBRC IS INITIAL.
          MOVE-CORRESPONDING LS_TRDIR TO LS_DATA.
        ENDIF.
        LS_DATA-STRING_SEARCH = RS_STRING_SEARCH-LOW.
* Set TRSTATUS value text
        IF LS_DATA-TRSTATUS IS NOT INITIAL.
          READ TABLE LT_DD07V_STAT INTO LS_DD07V WITH KEY DOMVALUE_L = LS_DATA-TRSTATUS.
          IF SY-SUBRC IS INITIAL.
            LS_DATA-TRSTATUS_TEXT = LS_DD07V-DDTEXT.
          ENDIF.
        ENDIF.
        CLEAR: LS_DD07V.
* Set TRFUNCTION text value
        IF LS_DATA-TRFUNCTION IS NOT INITIAL.
          READ TABLE LT_DD07V_STAT INTO LS_DD07V WITH KEY DOMVALUE_L = LS_DATA-TRFUNCTION.
          IF SY-SUBRC IS INITIAL.
            LS_DATA-TRFUNCTION_TEXT = LS_DD07V-DDTEXT.
          ENDIF.
        ENDIF.
        CLEAR: LS_DD07V.
        LOOP AT LT_RESULT INTO LS_RESULT.
          READ TABLE T_DATA WITH KEY LINE_SCAN = LS_RESULT-LINE
            TRANSPORTING NO FIELDS.
          CHECK SY-SUBRC IS NOT INITIAL.
          LS_DATA-LINE_NO = LS_RESULT-LINE.
          READ TABLE LT_ABAP INTO LS_ABAP INDEX LS_RESULT-LINE.
          IF SY-SUBRC IS INITIAL.
            LS_DATA-LINE_SCAN = LS_ABAP-LINE.
          ENDIF.
          APPEND LS_DATA TO T_DATA.
          CLEAR: LS_DATA-STRING_SEARCH.
        ENDLOOP.
*          ELSEIF rs_string_search-low(6) EQ 'DELETE'.
*
** Check if result is relevant
*            LOOP AT lt_result INTO ls_result.
*
*              lv_tabix = sy-tabix.
*              READ TABLE lt_abap INTO ls_abap INDEX ls_result-line.
*              IF sy-subrc IS INITIAL.
*                SHIFT ls_abap-line LEFT DELETING LEADING space.
*                SPLIT ls_abap-line AT space INTO TABLE lt_str_split.
*                READ TABLE lt_str_split INTO lv_string INDEX 2.
*              ENDIF.
*
** Check if the commands related to DB table change
*              IF lv_string IS NOT INITIAL.
*
*                lv_tabname = lv_string.
*                CALL FUNCTION 'CHECK_TAB_NAME'
*                  DESTINATION lv_sw_dest
*                  EXPORTING
*                    i_tabname   = lv_tabname
*                  EXCEPTIONS
*                    not_found = 1
*                    OTHERS    = 2.
*                IF sy-subrc IS NOT INITIAL.
*                  READ TABLE lt_str_split INTO lv_string INDEX 3.
*                  IF lv_string IS NOT INITIAL.
*                    lv_tabname = lv_string.
*                    CALL FUNCTION 'CHECK_TAB_NAME'
*                      DESTINATION lv_sw_dest
*                      EXPORTING
*                        i_tabname     = lv_tabname
*                      EXCEPTIONS
*                        tab_not_found = 1
*                        OTHERS        = 2.
*                  ENDIF.
*                  IF sy-subrc IS NOT INITIAL.
*                    DELETE lt_result INDEX lv_tabix.
*                  ENDIF.
*                ENDIF.
*        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
*  ENDIF.
*  ENDLOOP.
*  LOOP AT lt_object ASSIGNING <fs_obj>.
*
*    lv_tabix = sy-tabix.
*
*    IF <fs_obj>+30(2) EQ 'CP'. " Class Pool
*      DELETE lt_object INDEX lv_tabix.
*
*    ELSEIF <fs_obj>+30(2) = 'IP'. " Interface Pool
**      DELETE lt_object INDEX lv_tabix.
*
*      <fs_obj>+31(1) = 'U'.
*
*    ENDIF.
*
*    IF <fs_obj>-subc EQ 'I'.
*      EXIT.
*    ENDIF.
*
*  ENDLOOP.
******************************************** Get Source Code **********************************************************
*  _range_to_sel_table 'E~AS4DATE'    datum.       " Date of Last Change
*  _range_to_sel_table 'E~TRKORR'     trkorr.      " Request/Task
*  _range_to_sel_table 'E~TRSTATUS'   trstatus.    " Status:Modifiable, Released
*  _range_to_sel_table 'E~TRFUNCTION' trfunction.  " Type of request/task:Workbench, Customizing
******************************************** Get Source Code **********************************************************
*  REFRESH lt_main_req.
******************************************** Get Transport Details ****************************************************
*-- Fill Duration Value
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX.
    T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
      EXPORTING
        D_FROM      = T_DATA-AS4DATE
        T_FROM      = T_DATA-AS4TIME
        D_TO        = SY_DATLO
        T_TO        = SY_TIMLO
        TIME_UNIT   = LV_DURATION_UNIT
      IMPORTING
        TIME_DIFF   = TIME_DIFF
      EXCEPTIONS
        WRONG_VALUE = 1
        OTHERS      = 2.
    IF SY-SUBRC = 0.
      T_DATA-DURATION = TIME_DIFF .
    ELSE.
      T_DATA-DURATION = '999999'.
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX .
  ENDLOOP .
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL.
  IS_ALERT = 'X'.
ENDFUNCTION.
```
