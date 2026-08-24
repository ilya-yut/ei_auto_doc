# Exception Indicator: Opening and Closing FI periods ( SW_000000_V_T001B)

## General Overview

This Exception Indicator reviews change-log entries related to financial posting period configuration, surfacing who changed period settings, when, and under which organizational posting-period variant.

This EI serves as an essential control for financial administration and audit by:

- Enabling detection of opening and closing activity on posting periods that should follow a controlled calendar
- Supporting accountability for users and transactions that alter period availability
- Providing visibility into period and fiscal-year interval attributes alongside log timestamps
- Enabling follow-up when period changes occur outside expected close or reopen windows
- Supporting segregation-of-duties review when sensitive period maintenance is performed

Typical use includes month-end and year-end close governance, audit sampling of period status changes, and investigation of unauthorized or unexpected period maintenance. Results are intended for exception workflows rather than operational FI reporting extracts.

The routine submits a standard change-protocol report for the configured variant, maps returned rows into the output structure, applies optional age filtering, and raises an alert when qualifying log entries remain.


## Problem Description

Failure to monitor changes to financial posting period configuration creates multiple risks across financial close, compliance, and operational control:

**Financial and Close Risks**

- Periods opened or closed at the wrong time can allow postings into closed intervals or block legitimate close activity
- Undetected period maintenance can distort period-end balances and delay reconciliation
- Changes spanning multiple fiscal years or period intervals may hide material close exceptions until late review

**Operational and Security Risks**

- Users performing period changes without timely review weaken maker-checker discipline on sensitive FI settings
- Transaction and program context that is not monitored reduces traceability when close incidents occur
- Log scope that is too broad or too narrow can miss critical changes or flood reviewers with noise

**Control and Audit Risks**

- Weak evidence of period-change review weakens audit trails for SOX and internal control programs
- Lack of recurring monitoring delays detection of unauthorized maintenance on posting period variants
- Missing age-based prioritization can leave stale period incidents unaddressed

## Suggested Resolution

**Immediate Response**

- Review flagged log entries for user, timestamp, operation type, and affected posting-period variant
- Confirm whether each change was authorized for the current close calendar
- Escalate unexpected opens, closes, or interval adjustments to the FI controller or close manager

**System Assessment**

- Validate lookback and forward window settings against the close calendar and review cadence
- Tune company, account, and period-interval scope so results focus on in-scope variants
- Compare exception volume by user and transaction to identify repeat maintainers or training gaps

**Corrective Actions**

- Reverse or correct unauthorized period settings through standard FI period maintenance
- Document review outcomes and brief administrators on recurring patterns
- Schedule recurring runs before and after period close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ALV_GRID | ALV Grid Display(CB) |  | 0 | 0 |  |  |
| 2 | ALV_VARIANT | Variant ID |  | 0 | 0 |  |  |
| 3 | BACKDAYS | Back Days | INT4 | 10 | 0 | /SKN/E_MN_AN_BACKDAYS | /SKN/D_MN_AN_BACKDAYS |
| 4 | BKONT | To account | CHAR | 10 | 0 | BKONT_001B | MAXKN |
| 5 | BRGRU | Authorization Group | CHAR | 4 | 0 | BRGRU | BRGRU |
| 6 | BUKRS | Pstng period variant | CHAR | 4 | 0 | OPVAR | OPVAR |
| 7 | CURRENCY_CONV_DATE | Fieldname Curr.Conversion Date | CHAR | 30 | 0 | /SKN/E_MN_AN_CUR_CONV_DATE_FLD | FDNAME |
| 8 | CUSOBJ | Customizing Objects/Table Name |  | 0 | 0 |  |  |
| 9 | DATE_REF_FLD | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 10 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 11 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 12 | EXC_RATE_TYPE | Exchange Rate Type | CHAR | 4 | 0 | KURST_CURR | KURST |
| 13 | FORWDAYS | Forth Days | INT4 | 10 | 0 | /SKN/E_MN_AN_FORWDAYS | /SKN/D_MN_AN_FORWDAYS |
| 14 | FRPE1 | From period (interval 1) | NUMC | 3 | 0 | FRPER | POPER |
| 15 | FRPE2 | From period (interval 2) | NUMC | 3 | 0 | FRPE2 | POPER |
| 16 | FRYE1 | From fiscal year (interval 1) | NUMC | 4 | 0 | FRYER | GJAHR |
| 17 | FRYE2 | From fiscal year (period 2) | NUMC | 4 | 0 | FRYE2 | GJAHR |
| 18 | LANGU | Language Key | LANG | 1 | 0 | LANGU | SPRAS |
| 19 | MKOAR | Account type | CHAR | 1 | 0 | MKOAR | MKOAR |
| 20 | OBJFIRST | Customizing Objects(RB) |  | 0 | 0 |  |  |
| 21 | PROGNAME | Program Name | CHAR | 40 | 0 | PROGRAMM | PROGRAMM |
| 22 | RRCTY | Record Type | CHAR | 1 | 0 | RRCTY | RRCTY |
| 23 | TABFIRST | Tables(RB) |  | 0 | 0 |  |  |
| 24 | TCODE | Transaction Code | CHAR | 20 | 0 | TCODE | TCODE |
| 25 | TIME_REF_FLD | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 26 | TLOG_LOGDATE | Date | DATS | 8 | 0 | SWX_DATE | SYDATS |
| 27 | TLOG_LOGTIME | Time | TIMS | 6 | 0 | EU_ZEIT | SYTIME |
| 28 | TLOG_OPTYPE | Single-Character Indicator | CHAR | 1 | 0 | CHAR1 | CHAR1 |
| 29 | TLOG_OPTYPE_TEXT | Char20 | CHAR | 20 | 0 | CHAR20 | CHAR20 |
| 30 | TLOG_USERNAME | User | CHAR | 12 | 0 | XUBNAME | XUBNAME |
| 31 | TOPE1 | To Period (Interval 1) | NUMC | 3 | 0 | TOPER | POPER |
| 32 | TOPE2 | To Period (Interval 2) | NUMC | 3 | 0 | TOPE2 | POPER |
| 33 | TOYE1 | To fiscal year (period 1) | NUMC | 4 | 0 | TOYER | GJAHR |
| 34 | TOYE2 | To fiscal year (period 2) | NUMC | 4 | 0 | TOYE2 | GJAHR |
| 35 | VKONT | From account | CHAR | 10 | 0 | VKONT_001B | MAXKN |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 35 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ALV_GRID** (ALV Grid Display(CB))

When tightened, alv grid display(cb) (ALV_GRID) removes rows that would otherwise dilute attention from failing or stuck cases.

**Not in use**
**ALV_VARIANT** (Variant ID)

Gives auditors traceable criteria because variant id on ALV_VARIANT is applied consistently before any alert flag is raised.

**Not in use**
**BACKDAYS** (Back Days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BKONT** (To account)

Improves readability of exported lists because to account (BKONT) columns stay aligned with the configured filter intent.

**BRGRU** (Authorization Group)

Connects to alert semantics: rows removed for failing authorization group on BRGRU never reach downstream filtering.

**BUKRS** (Pstng period variant)

Company code key that scopes data to legal entity/accounting unit level.

**CURRENCY_CONV_DATE** (Fieldname Curr.Conversion Date)

Aligns exception volume with the chosen scope by testing fieldname curr.conversion date via CURRENCY_CONV_DATE before alert evaluation.

**CUSOBJ** (Customizing Objects/Table Name)

Works downstream of the initial read so customizing objects/table name on CUSOBJ still participates in row-level deletion rules.

**Not in use**
**DATE_REF_FLD** (Date reference field)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- DBEG — Code-defined date reference field.
- DEND — Code-defined date reference field.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**EXC_RATE_TYPE** (Exchange Rate Type)

Improves readability of exported lists because exchange rate type (EXC_RATE_TYPE) columns stay aligned with the configured filter intent.

**FORWDAYS** (Forth Days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

Forwdays is based on DATE_REF_FLD field.

**FRPE1 - FRPE2** (From period (interval 1))

Connects to alert semantics: rows removed for failing from period (interval 1) on FRPE1 never reach downstream filtering.

**FRYE1 - FRYE2** (From fiscal year (interval 1))

Separates cross-client noise from in-scope work when from fiscal year (interval 1) on FRYE1 correlates with client or user attributes.

**LANGU** (Language Key)

Language key used for language-dependent texts and user-language filtering.

**MKOAR** (Account type)

Connects to alert semantics: rows removed for failing account type on MKOAR never reach downstream filtering.

**OBJFIRST** (Customizing Objects(RB))

For operations, customizing objects(rb) on OBJFIRST indicates whether a row belongs in the current monitoring pass versus historical noise.

**Not in use**
**PROGNAME** (Program Name)

ABAP program name.

**RRCTY** (Record Type)

Helps distinguish technical versus business attributes when record type on RRCTY correlates with counters or status fields.

**TABFIRST** (Tables(RB))

Valuable when comparing health before and after a release—hold tables(rb) on TABFIRST constant while varying other filters.

**Not in use**
**TCODE** (Transaction Code)

SAP Transaction code

**TIME_REF_FLD** (Time reference field)

Name of the time field used as the aging anchor-time analogue of DATE_REF_FLD for duration-from-reference logic.

**TIME_REF_FLD Options:**
- Use a time field from the same structure as DATE_REF_FLD or as defined in the EI code path.
- Values follow SAP time representation (typically HHMMSS semantics in the domain).

**TLOG_LOGDATE** (Date)

Narrows retrieved rows where date (TLOG_LOGDATE) must match the configured selection for this monitor.

**TLOG_LOGTIME** (Time)

Improves readability of exported lists because time (TLOG_LOGTIME) columns stay aligned with the configured filter intent.

**TLOG_OPTYPE** (Single-Character Indicator)

For operations, single-character indicator on TLOG_OPTYPE indicates whether a row belongs in the current monitoring pass versus historical noise.

**TLOG_OPTYPE_TEXT** (Char20)

Gives auditors traceable criteria because char20 on TLOG_OPTYPE_TEXT is applied consistently before any alert flag is raised.

**TLOG_USERNAME** (User)

Narrows retrieved rows where user (TLOG_USERNAME) must match the configured selection for this monitor.

**TOPE1 - TOPE2** (To Period (Interval 1))

When harmonized with related filters, to period (interval 1) on TOPE1 isolates the highest-risk record families.

**TOYE1 - TOYE2** (To fiscal year (period 1))

Reduces false positives during peak windows by tightening to fiscal year (period 1) through TOYE1 alongside state filters.

**VKONT** (From account)

Interprets from account as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on VKONT.

### Parameter Relationships

**Date window:** When no explicit date range is supplied, **BACKDAYS** and **FORWDAYS** build a from–to window anchored on the evaluation date; that window is mapped to log begin and end dates when **DATE_REF_FLD** is **DBEG** or **DEND** and those single-value dates are initial.

**Age filter:** After rows are returned, **DURATION** with **DURATION_UNIT** measures elapsed time from the reference date (and optional **TIME_REF_FLD** time) to the evaluation moment; rows outside the configured duration range are removed.

**Report submission:** The fixed change-protocol report name in code is submitted remotely with selection options from the initial read and value parameters built from date defaults.

**Organizational scope:** **BUKRS**, **MKOAR**, **BKONT**, **VKONT**, **BRGRU**, and **RRCTY** narrow which logged customizing rows are retrieved for posting-period maintenance.

**Period intervals:** **FRPE1**, **FRPE2**, **FRYE1**, **FRYE2**, **TOPE1**, **TOPE2**, **TOYE1**, and **TOYE2** work together to restrict log rows to configured from/to period and fiscal-year intervals.

**Log identity:** **TLOG_USERNAME**, **TCODE**, **PROGNAME**, **TLOG_OPTYPE**, and **TLOG_OPTYPE_TEXT** combine to identify who performed the change and through which technical context.


### Default Values

- **BACKDAYS** - initial - treated as 10 by code
- **DURATION** - initial - treated as unconstrained by code
- **DURATION_UNIT** - initial - treated as D by code
- **DATE_REF_FLD** - DBEG
- **EXC_RATE_TYPE** - M
- **LANGU** - EN

### Practical Example of Parameter Configuration

**Use Case 1: Posting period changes in the last thirty days**

**Purpose:** Review change-log entries for posting period maintenance over the last thirty days for one posting-period variant.

```
BACKDAYS = 30
BUKRS = 1000
DATE_REF_FLD = DBEG
```

**Use Case 2: Forward window through close week**

**Purpose:** Include log entries from ten days before today through three days ahead for close-week surveillance.

```
BACKDAYS = 10
FORWDAYS = 3
DATE_REF_FLD = DBEG
```

**Use Case 3: Specific user and transaction**

**Purpose:** Investigate period changes performed by one user through a known maintenance transaction.

```
TLOG_USERNAME = JSMITH
TCODE = OB52
BACKDAYS = 90
```

**Use Case 4: Fiscal year and period interval**

**Purpose:** Limit results to a defined fiscal year and period interval on the logged record.

```
FRYE1 = 2025
FRPE1 = 001
TOYE1 = 2025
TOPE1 = 012
BACKDAYS = 365
```

**Use Case 5: Exactly seven full days since log date**

**Purpose:** Return rows where the reference log date is exactly 7 full days ago.

```
DURATION = 7
DURATION_UNIT = F
BACKDAYS = 180
DATE_REF_FLD = DBEG
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_AG_ALV_RSVTPROT | BACKDAYS | Back Days | INT4(10) | /SKN/E_MN_AN_BACKDAYS |
| /SKN/S_AG_ALV_RSVTPROT | BKONT | To account | CHAR(10) | BKONT_001B |
| /SKN/S_AG_ALV_RSVTPROT | BRGRU | Authorization Group | CHAR(4) | BRGRU |
| /SKN/S_AG_ALV_RSVTPROT | BUKRS | Pstng period variant | CHAR(4) | OPVAR |
| /SKN/S_AG_ALV_RSVTPROT | CURRENCY_CONV_DATE | Fieldname Curr.Conversion Date | CHAR(30) | /SKN/E_MN_AN_CUR_CONV_DATE_FLD |
| /SKN/S_AG_ALV_RSVTPROT | DATE_REF_FLD | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_AG_ALV_RSVTPROT | DURATION | Duration In Time Units | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_AG_ALV_RSVTPROT | DURATION_UNIT | Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_AG_ALV_RSVTPROT | EXC_RATE_TYPE | Exchange Rate Type | CHAR(4) | KURST_CURR |
| /SKN/S_AG_ALV_RSVTPROT | FORWDAYS | Forth Days | INT4(10) | /SKN/E_MN_AN_FORWDAYS |
| /SKN/S_AG_ALV_RSVTPROT | FRPE1 | From period (interval 1) | NUMC(3) | FRPER |
| /SKN/S_AG_ALV_RSVTPROT | FRPE2 | From period (interval 2) | NUMC(3) | FRPE2 |
| /SKN/S_AG_ALV_RSVTPROT | FRYE1 | From fiscal year (interval 1) | NUMC(4) | FRYER |
| /SKN/S_AG_ALV_RSVTPROT | FRYE2 | From fiscal year (period 2) | NUMC(4) | FRYE2 |
| /SKN/S_AG_ALV_RSVTPROT | LANGU | Language Key | LANG(1) | LANGU |
| /SKN/S_AG_ALV_RSVTPROT | MKOAR | Account type | CHAR(1) | MKOAR |
| /SKN/S_AG_ALV_RSVTPROT | PROGNAME | Program Name | CHAR(40) | PROGRAMM |
| /SKN/S_AG_ALV_RSVTPROT | RRCTY | Record Type | CHAR(1) | RRCTY |
| /SKN/S_AG_ALV_RSVTPROT | TCODE | Transaction Code | CHAR(20) | TCODE |
| /SKN/S_AG_ALV_RSVTPROT | TIME_REF_FLD | Field name | CHAR(30) | NAME_FELD |
| /SKN/S_AG_ALV_RSVTPROT | TLOG_LOGDATE | Date | DATS(8) | SWX_DATE |
| /SKN/S_AG_ALV_RSVTPROT | TLOG_LOGTIME | Time | TIMS(6) | EU_ZEIT |
| /SKN/S_AG_ALV_RSVTPROT | TLOG_OPTYPE | Single-Character Indicator | CHAR(1) | CHAR1 |
| /SKN/S_AG_ALV_RSVTPROT | TLOG_OPTYPE_TEXT | Char20 | CHAR(20) | CHAR20 |
| /SKN/S_AG_ALV_RSVTPROT | TLOG_USERNAME | User | CHAR(12) | XUBNAME |
| /SKN/S_AG_ALV_RSVTPROT | TOPE1 | To Period (Interval 1) | NUMC(3) | TOPER |
| /SKN/S_AG_ALV_RSVTPROT | TOPE2 | To Period (Interval 2) | NUMC(3) | TOPE2 |
| /SKN/S_AG_ALV_RSVTPROT | TOYE1 | To fiscal year (period 1) | NUMC(4) | TOYER |
| /SKN/S_AG_ALV_RSVTPROT | TOYE2 | To fiscal year (period 2) | NUMC(4) | TOYE2 |
| /SKN/S_AG_ALV_RSVTPROT | VKONT | From account | CHAR(10) | VKONT_001B |

## ABAP Code

```abap
FUNCTION /SKN/FC_AG_ALV_RSVTPROT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_AG_ALV_RSVTPROT
*"----------------------------------------------------------------------
  CLEAR LT_ALV_VAL_PARAMS[].
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: DATUM DATUM.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: BC_ID SCPR_ID.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: BC_TEXT SCPR_TEXT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: CUSOBJ OB_OBJECT.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: DBEG TLOG_BEGDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: DEND TLOG_ENDDAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: STYPE OB_TYP.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: TBEG TLOG_BEGTIME.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: TEND TLOG_ENDTIME.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: USERS SYST_UNAME.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: ACC_ARCH CHAR1.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: ADDTBLGS ADDTABLOG.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: ALV_GRID CHAR1.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: IGN_UNCH CHAR1.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: OBJFIRST CHAR1.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: TABFIRST CHAR1.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: TLOG_USERNAME CHAR12.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: TCODE CHAR20.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: PROGNAME CHAR40.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: TLOG_OPTYPE CHAR1.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: TLOG_OPTYPE_TEXT CHAR20.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: RRCTY CHAR1.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BUKRS CHAR4.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: MKOAR CHAR1.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BKONT CHAR10.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: VKONT CHAR10.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BRGRU CHAR4.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: BACKDAYS /SKN/E_MN_AN_BACKDAYS.
  LV_BACKDAYS = '10'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: DATE_REF_FLD NAME_FELD.
  LV_DATE_REF_FLD = 'DBEG'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: DURATION_UNIT /SKN/E_SW_DURATION_UNIT.
  LV_DURATION_UNIT = 'D'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: EXC_RATE_TYPE KURST_CURR.
  LV_EXC_RATE_TYPE = 'M'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: FORWDAYS /SKN/E_MN_AN_FORWDAYS.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: LANGU LANGU.
  LV_LANGU = 'EN'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: CURRENCY_CONV_DATE /SKN/E_MN_AN_CUR_CONV_DATE_FLD.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: DURATION /SKN/E_SW_DURATION.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: TIME_REF_FLD NAME_FELD.
  ##NEEDED
  DATA SY_DATLO LIKE SY-DATLO.
  ##NEEDED
  DATA SY_TIMLO LIKE SY-TIMLO.
  ##NEEDED
  DATA DATE_FROM LIKE SY-DATUM.
  ##NEEDED
  DATA DATE_TO LIKE SY-DATUM.
  ##NEEDED
  DATA LV_TAB TYPE DDOBJNAME.
  ##NEEDED
  DATA LV_STRUC TYPE DDOBJNAME.
  ##NEEDED
  DATA LS_LIST TYPE /SKN/S_TABLES.
  ##NEEDED
  DATA LT_DATA_TMP LIKE T_DATA[].
  DATA: LV_STR TYPE STRING.
  DATA: LS_ALV_DATA TYPE /SKN/S_SW_TAB2000,
        LS_DATA     LIKE LINE OF T_DATA,
        LS_OUTPUT   TYPE /SKN/RFC_DB_FLD_EXTEND.
  FIELD-SYMBOLS: <FS_VALUE> TYPE ANY.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: BC_ID.
  _SINGLE_VALUE_PARAM_SET BC_ID.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: BC_TEXT.
  _SINGLE_VALUE_PARAM_SET BC_TEXT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: CUSOBJ.
  _RANGE_VALUE_PARAM_SET CUSOBJ.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: DBEG.
  _SINGLE_VALUE_PARAM_SET DBEG.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: DEND.
  _SINGLE_VALUE_PARAM_SET DEND.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: STYPE.
  _RANGE_VALUE_PARAM_SET STYPE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: TBEG.
  _SINGLE_VALUE_PARAM_SET TBEG.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: TEND.
  _SINGLE_VALUE_PARAM_SET TEND.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: USERS.
  _RANGE_VALUE_PARAM_SET USERS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: ACC_ARCH.
  _SINGLE_VALUE_PARAM_SET ACC_ARCH.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: ADDTBLGS.
  _SINGLE_VALUE_PARAM_SET ADDTBLGS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: ALV_GRID.
  _SINGLE_VALUE_PARAM_SET ALV_GRID.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: IGN_UNCH.
  _SINGLE_VALUE_PARAM_SET IGN_UNCH.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: OBJFIRST.
  _SINGLE_VALUE_PARAM_SET OBJFIRST.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: TABFIRST.
  _SINGLE_VALUE_PARAM_SET TABFIRST.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: TLOG_USERNAME.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: TCODE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: PROGNAME.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: TLOG_OPTYPE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: TLOG_OPTYPE_TEXT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: RRCTY.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BUKRS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: MKOAR.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BKONT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: VKONT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BRGRU.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: BACKDAYS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: DATE_REF_FLD.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: DURATION_UNIT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: EXC_RATE_TYPE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: FORWDAYS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: LANGU.
  CONVERT_SINGLE: LANGU ISOLA.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: CURRENCY_CONV_DATE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: DURATION.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: TIME_REF_FLD.
  ##NEEDED
  DATA LV_SW_DEST TYPE RFCDEST.
  ##NEEDED
  DATA LV_ALV_REPORT_NAME TYPE RALDB_REPO.
  ##NEEDED
  DATA LV_ALV_VARIANT TYPE RALDB_VARI.
  ##NEEDED
  DATA LV_ALV_LAYOUT_REPORT TYPE REPID.
  ##NEEDED
  DATA LV_ALV_LAYOUT_VARIANT TYPE SLIS_VARI.
  ##NEEDED
  DATA LV_ALV_LAYOUT_CNAME TYPE SLIS_ERNAM.
  ##NEEDED
  DATA LV_ALV_LANGU TYPE SY-LANGU.
  ##NEEDED
  DATA LV_ALV_BATCH TYPE CHECKBOX.
  ##NEEDED
  DATA LV_ALV_SUBMIT TYPE BOOLE_D.
  ##NEEDED
  DATA LV_ALV_JOB_SCHED TYPE BOOLE_D.
  ##NEEDED
  DATA LV_ALV_VARIANT_DESC TYPE RVART_VTXT.
  ##NEEDED
  DATA LT_OPTIONS TYPE TABLE OF RFC_DB_OPT.
  ##NEEDED
  DATA LT_ALV_VALUE_PARAMS TYPE TABLE OF RSPARAMSL_255.
  ##NEEDED
  DATA LT_ALV_PARAMS_INFO TYPE TABLE OF RSEL_INFO.
  ##NEEDED
  DATA LT_ALV_PARAMS_TEXT TYPE TABLE OF VANZ.
  ##NEEDED
  DATA LT_ALV_DD04V TYPE TABLE OF DD04V.
  ##NEEDED
  DATA LT_ALV_OUTPUT TYPE TABLE OF /SKN/RFC_DB_COMP_SETT.
  ##NEEDED
  DATA LT_ALV_OUTPUT_EXT TYPE /SKN/TT_RFC_DB_FLD_EXTEND.
  ##NEEDED
  DATA LT_ALV_FCAT TYPE LVC_T_FCAT.
  ##NEEDED
  DATA LT_ALV_DATA TYPE TABLE OF /SKN/S_SW_TAB2000.
  ##NEEDED
  DATA LT_AUTH_ERR_LOG TYPE TABLE OF /SKN/S_SU53_LOGS.
  ##NEEDED
  DATA LT_ALV_RETURN TYPE BAPIRET2_T.
  ##NEEDED
  DATA LV_D_FROM TYPE SY-DATUM.
  ##NEEDED
  DATA LV_T_FROM TYPE SY-UZEIT.
  ##NEEDED
  DATA LV_D_TO TYPE SY-DATUM.
  ##NEEDED
  DATA LV_T_TO TYPE SY-UZEIT.
  ##NEEDED
  DATA LV_TIME_UNIT TYPE /SKN/E_SW_SCHEDL_UNIT.
  ##NEEDED
  DATA LV_TIME_DIFF TYPE INT4.
  REFRESH LT_OPTIONS.
  REFRESH LT_OUT_WHERE_COND.
  CLEAR: LV_LINES, LS_OPTION,
         LT_OPTIONS_CURR, LT_COND_CURR, LT_OPTIONS_MAIN.
  ##NO_HANDLER
  SELECT_SINGLE: SW_DEST.
  ##NO_HANDLER
  _GET_CURRENT_DATE_TIME LV_MANAGE_IN_UTC LV_SW_DEST SY_DATLO SY_TIMLO.
  IF R_DATUM[] IS INITIAL.
    RS_DATUM-SIGN   = 'I'.
    IF LV_FORWDAYS IS INITIAL.
      DATE_FROM = SY_DATLO - LV_BACKDAYS.
      DATE_TO   = SY_DATLO.
      RS_DATUM-OPTION = 'BT'.
    ELSE.
      IF LV_BACKDAYS IS NOT INITIAL.
        DATE_FROM = SY_DATLO - LV_BACKDAYS.
        DATE_TO   = SY_DATLO + LV_FORWDAYS.
        RS_DATUM-OPTION = 'BT'.
      ELSE.
        DATE_FROM = SY_DATLO + LV_FORWDAYS.
        RS_DATUM-OPTION = 'GE'.
      ENDIF.
    ENDIF.
    RS_DATUM-LOW  = DATE_FROM.
    RS_DATUM-HIGH = DATE_TO.
    APPEND RS_DATUM TO R_DATUM.
  ENDIF.
  CASE LV_DATE_REF_FLD.
    WHEN 'DBEG' OR 'DEND'.
      IF LV_DBEG IS INITIAL.
        LS_VAL_PARAMS-SELNAME = LV_DATE_REF_FLD.
        LS_VAL_PARAMS-KIND    = 'D'.
        LS_VAL_PARAMS-SIGN    = 'I'.
        LS_VAL_PARAMS-OPTION  = 'EQ'.
        LS_VAL_PARAMS-LOW     = DATE_FROM.
        APPEND LS_VAL_PARAMS TO LT_ALV_VAL_PARAMS.
        CLEAR: LS_VAL_PARAMS.
      ENDIF.
      IF LV_DEND IS INITIAL.
        LV_DEND = DATE_TO.
        LS_VAL_PARAMS-SELNAME = 'DEND'.
        LS_VAL_PARAMS-KIND    = 'D'.
        LS_VAL_PARAMS-SIGN    = 'I'.
        LS_VAL_PARAMS-OPTION  = 'EQ'.
        LS_VAL_PARAMS-LOW     = DATE_TO.
        APPEND LS_VAL_PARAMS TO LT_ALV_VAL_PARAMS.
        CLEAR: LS_VAL_PARAMS.
      ENDIF.
  ENDCASE.
  REFRESH R_DATUM.
  LV_RANGE = 'DURATION'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE DURATION.
  LT_OPTIONS[] = LT_OUT_WHERE_COND[].
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  REFRESH LT_OUT_WHERE_COND.
  CLEAR LT_ALV_DATA.
  CLEAR LT_ALV_OUTPUT_EXT.
  CLEAR LT_ALV_RETURN.
  LV_ALV_REPORT_NAME = 'RSVTPROT'.
  SELECT_SINGLE: ALV_VARIANT.
  CLEAR: LT_ALV_RETURN.
  ##NO_HANDLER
  ##NEEDED
  LT_ALV_VALUE_PARAMS = LT_ALV_VAL_PARAMS.
  CALL FUNCTION '/SKN/F_MN_AN_AG_SUBMIT_ALV_REP'
    DESTINATION LV_SW_DEST
    EXPORTING
      ALV_REPORT_NAME    = LV_ALV_REPORT_NAME
      ALV_VARIANT        = LV_ALV_VARIANT
      ALV_LAYOUT_REPORT  = LV_ALV_LAYOUT_REPORT
      ALV_LAYOUT_VARIANT = LV_ALV_LAYOUT_VARIANT
      ALV_LANGU          = LV_ALV_LANGU
      ALV_SUBMIT         = 'X'
    TABLES
      OPTIONS            = LT_OPTIONS
      ALV_VALUE_PARAMS   = LT_ALV_VALUE_PARAMS
      ALV_PARAMS_INFO    = LT_ALV_PARAMS_INFO
      ALV_PARAMS_TEXT    = LT_ALV_PARAMS_TEXT
      ALV_OUTPUT         = LT_ALV_OUTPUT
      ALV_OUTPUT_EXT     = LT_ALV_OUTPUT_EXT
      ALV_DATA           = LT_ALV_DATA
      ALV_RETURN         = LT_ALV_RETURN
    EXCEPTIONS
      VARIANT_NOT_EXIST  = 1
      VARIANT_OBSOLETE   = 2
      VARIANT_FAILED     = 3
      OTHERS             = 4.
  IF SY-SUBRC IS NOT INITIAL OR LT_ALV_RETURN IS NOT INITIAL.
    CLEAR LT_ALV_DATA.
  ELSE.
*    _rfc_to_t_data_index lt_alv_data t_data lt_alv_output_ext 1.
    LOOP AT LT_ALV_DATA INTO LS_ALV_DATA.
      CLEAR: LS_DATA.
      LOOP AT LT_ALV_OUTPUT_EXT INTO LS_OUTPUT.
        LV_STR = 'LS_DATA-' && LS_OUTPUT-FIELDNAME.
        ASSIGN (LV_STR) TO <FS_VALUE>.
        CHECK SY-SUBRC = 0.
        <FS_VALUE> = LS_ALV_DATA+LS_OUTPUT-OFFSET(LS_OUTPUT-LENGTH).
      ENDLOOP.
      APPEND LS_DATA TO T_DATA.
    ENDLOOP.
  ENDIF.
  CHECK T_DATA[] IS NOT INITIAL.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: D_FROM.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: T_FROM.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: D_TO.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: T_TO.
* The parameter field 'lv_date_ref_fld'
* and 'lv_time_ref_fld' is declared
* at '/SKN/T_AR_FIELDS' custom. table
* and is initialized on the user screen
  ##NEEDED
  DATA: SY_TABIX LIKE SY-TABIX .
  ##NEEDED
  FIELD-SYMBOLS:  TYPE ANY,
  ##NEEDED
                 <FS_DURATION> TYPE ANY,
  ##NEEDED
                 <FS_DU>       TYPE ANY.
  CLEAR: LV_FLD, SY_TABIX.
  LV_T_FROM = SY_TIMLO.
  LV_D_TO   = SY_DATLO.
  LV_T_TO   = SY_TIMLO.
*-- Calculate Status Duration
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX.
* Set field 'date_from' by date reference field
* which is determined on the user screen
    CONCATENATE 'T_DATA-' LV_DATE_REF_FLD INTO LV_FLD.
    ASSIGN (LV_FLD) TO .
    IF  IS NOT ASSIGNED.
      CONTINUE.
    ELSE.
      LV_D_FROM = .
      UNASSIGN .
    ENDIF.
    CLEAR: LV_FLD.
* Set field 'time_from' by time reference field
* which is determined on the user screen
    IF LV_TIME_REF_FLD IS NOT INITIAL.
      CONCATENATE 'T_DATA-' LV_TIME_REF_FLD INTO LV_FLD.
      ASSIGN (LV_FLD) TO .
      IF  IS ASSIGNED.
        LV_T_FROM = .
      ENDIF.
    ENDIF.
    IF NOT LV_D_FROM IS INITIAL.
      ASSIGN COMPONENT 'DURATION_UNIT' OF STRUCTURE T_DATA TO <FS_DU>.
      IF SY-SUBRC EQ 0 AND <FS_DU> IS ASSIGNED.
*      t_data-duration_unit = lv_duration_unit.
        <FS_DU> = LV_DURATION_UNIT.
      ENDIF.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = LV_D_FROM
          T_FROM      = LV_T_FROM
          D_TO        = LV_D_TO
          T_TO        = LV_T_TO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = LV_TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        ASSIGN COMPONENT 'DURATION' OF STRUCTURE T_DATA TO <FS_DURATION>.
        IF SY-SUBRC EQ 0 AND <FS_DURATION> IS ASSIGNED.
          <FS_DURATION> = LV_TIME_DIFF.
        ENDIF.
      ELSE.
        ASSIGN COMPONENT 'DURATION' OF STRUCTURE T_DATA TO <FS_DURATION>.
        IF SY-SUBRC EQ 0 AND <FS_DURATION> IS ASSIGNED.
          <FS_DURATION> = '999999'.
        ENDIF.
      ENDIF.
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE DURATION NOT IN R_DURATION.
  CHECK T_DATA[] IS NOT INITIAL.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
