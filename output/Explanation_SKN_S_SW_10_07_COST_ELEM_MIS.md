# Exception Indicator: FI G/L account without Cost Element ( SW_10_07_COST_EL_MIS)

## General Overview

This Exception Indicator monitors G/L accounts in Financial Accounting that are missing a required cost element assignment on the linked cost-element side. It surfaces chart-of-accounts and company-code account rows that pass master-data filters yet still have no cost element where the control logic expects one, so finance and controlling teams can correct master data before postings or consolidations drift.

This EI serves as an essential control for the general ledger and controlling interface by:
- Highlighting G/L accounts that would post or report without a proper cost element link, before incorrect CO allocations spread through period activity
- Supporting month-end and project accounting reviews where cost elements drive settlement, commitment, or margin logic tied to specific account groups
- Giving owners of chart of accounts and account groups a prioritized list of exceptions instead of manual reconciliation across long account lists
- Reducing the risk that blocked, deleted, or special-status accounts mask underlying missing cost element assignments when those status dimensions are in scope for the run
- Enabling follow-up with the right creators or maintainers by carrying creation and change context that supports accountability without replacing formal workflow

Organizations use this visibility during chart-of-accounts maintenance cycles, internal control testing, and preparation for audits or migrations where cost element completeness is a prerequisite for reliable CO-PA or internal order postings.

The EI reads G/L master data from SKA1 together with cost element data from CSKA, joined on chart of accounts and the account-to-cost-element relationship.


## Problem Description

Failure to monitor G/L accounts that lack a required cost element assignment creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Postings and allocations can proceed on accounts that look valid in FI while still breaking downstream controlling reports that depend on a cost element
- Management reports that rely on cost element detail can silently omit or misclassify activity tied to the affected accounts
- Period-end adjustments may grow when missing links are discovered only during reconciliation or consolidation
- Intercompany or segment reporting that expects a full CO account backbone can show unexplained gaps or reclassifications
- Audit evidence for cost element completeness becomes harder to assemble when exceptions are found late

**Operational and Master Data Risks**
- Account maintainers may create or extend G/L accounts under time pressure without completing the cost element side of the relationship
- Chart-of-accounts or account-group changes can leave a subset of accounts outside the intended controlling footprint
- Teams that manage deletion or blocking flags may not realize that some “active-looking” accounts still lack a cost element until operational reports fail
- Shared service centers processing many charts of accounts can miss a few stragglers without automated exception lists

**Management Visibility and Decision-Making Risks**
- Controlling and finance leadership lack a single, repeatable view of “FI-ready but CO-incomplete” accounts for prioritization
- Root-cause discussions stall when it is unclear whether the gap is data entry, organizational scope, or an intentional exception never documented
- Investment in automated monitoring is undermined if the same manual spreadsheets are rebuilt each quarter

## Suggested Resolution

**Immediate Response**
- Review the exception list and confirm each account is meant to carry a cost element under your chart and account-group rules
- For each highlighted account, open master-data maintenance (for example FS00 for the company-code view of the G/L account) and verify the relationship to cost accounting objects
- Validate whether the account should be excluded from the check by a documented policy (for example dedicated balance-sheet-only accounts) and update monitoring scope if the policy is approved
- Coordinate with the owner of the relevant chart of accounts and controlling policy so corrections follow your internal approval path

**System Assessment**
- Compare current exceptions to a prior run to see whether the population is shrinking after remediation or growing after chart changes
- Segment exceptions by chart of accounts, account group, and balance-sheet versus P&L classification to see where training or template fixes would have the most impact
- Check whether newly created accounts cluster around specific creators or organizational units, which may indicate process gaps rather than isolated errors
- Confirm that forward-looking monitoring windows still match your close calendar and master-data freeze dates

**Corrective Actions**
- Complete or correct the cost element assignment in master data where the business requires it, and document any approved exceptions
- Retire or consistently block accounts that should not receive postings, following your data lifecycle standards
- Brief account maintainers on the minimum fields required when introducing accounts that participate in CO-relevant processes
- Schedule recurring runs after major chart-of-accounts projects or migrations and track remediation in your control issue log


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Back Days | INT4 | 10 | 0 | /SKN/E_MN_AN_BACKDAYS | /SKN/D_MN_AN_BACKDAYS |
| 2 | BILKT | Group account number | CHAR | 10 | 0 | BILKT | SAKNR |
| 3 | BILKT_DESC | G/L Acct Long Text | CHAR | 50 | 0 | TXT50_SKAT | TEXT50 |
| 4 | DATE_REF_FLD | Field name | CHAR | 30 | 0 | NAME_FELD | FDNAME |
| 5 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 6 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 7 | ERDAT | Created on | DATS | 8 | 0 | ERDAT_RF | DATUM |
| 8 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM_RF | USNAM |
| 9 | ERSDA | Created on | DATS | 8 | 0 | ERFDT | DATUM |
| 10 | EXC_RATE_TYPE | Exchange Rate Type | CHAR | 4 | 0 | KURST_CURR | KURST |
| 11 | FORWDAYS | Forth Days | INT4 | 10 | 0 | /SKN/E_MN_AN_FORWDAYS | /SKN/D_MN_AN_FORWDAYS |
| 12 | GVTYP | P&L statmt acct type | CHAR | 2 | 0 | GVTYP | CHAR2 |
| 13 | KSTAR | Cost Element | CHAR | 10 | 0 | KSTAR | KSTAR |
| 14 | KTOKS | Account Group | CHAR | 4 | 0 | KTOKS | KTOKS |
| 15 | KTOPL | Chart of Accounts | CHAR | 4 | 0 | KTOPL | KTOPL |
| 16 | LANGU | Language Key | LANG | 1 | 0 | LANGU | SPRAS |
| 17 | SAKAN | G/L account | CHAR | 10 | 0 | SAKAN | CHAR10 |
| 18 | SAKNR | G/L Account | CHAR | 10 | 0 | SAKNR | SAKNR |
| 19 | SAKNR_DESC | G/L Acct Long Text | CHAR | 50 | 0 | TXT50_SKAT | TEXT50 |
| 20 | TARGET_CUKY | Target Curr. Key | CUKY | 5 | 0 | /SKN/E_MN_AN_TARGET_CURR | WAERS |
| 21 | USNAM | Created by | CHAR | 12 | 0 | ERFNM | USNAM |
| 22 | XBILK | Balance sheet account | CHAR | 1 | 0 | XBILK | XFELD |
| 23 | XLOEV | Mark for deletion | CHAR | 1 | 0 | XLOEV | XFELD |
| 24 | XSPEA | Blocked for creation | CHAR | 1 | 0 | XSPEA | XFELD |
| 25 | XSPEB | Blocked for posting | CHAR | 1 | 0 | XSPEB | XFELD |
| 26 | XSPEP | Blocked for planning | CHAR | 1 | 0 | XSPEP | XFELD |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 26 parameters listed in the Parameters Reference Table when tuning this EI; each narrows which G/L accounts are read from SKA1 and which cost-element rows from CSKA are considered in the join.

**BACKDAYS** (Back Days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on DATE_REF_FLD field.

**BILKT** (Group account number)

Limits output to G/L accounts whose financial statement item matches the configured list so monitoring stays on the reporting lines you care about.

**BILKT_DESC** (G/L Acct Long Text)

Filters by the long description tied to the financial statement item code, which helps when reviewers recognize the FS wording instead of the numeric BILKT.

**DATE_REF_FLD** (Field name)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — G/L account creation date on the SKA1 row used for the account side of the check.
- ERSDA — Cost-element creation date on the CSKA row used for the cost-element side of the check.

**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- **H** — Hours.
- **M** — Minutes.
- **D** — Days.
- **F** — Full calendar-day style counting for the duration helper used after rows are in the result set.

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**ERSDA** (Created on)

Selects cost-element rows by their creation date on the cost-element record so you can align the monitoring window with when the cost element was introduced rather than when the G/L account was created.

**EXC_RATE_TYPE** (Exchange Rate Type)

Carries the rate type used whenever the framework converts amounts into the target display currency for this monitoring family.

**EXC_RATE_TYPE Options:**
- **M** — Middle rate preset in the supplied ABAP when the caller leaves the field empty.

**FORWDAYS** (Forth Days)

<mark>FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.</mark>

When FORWDAYS is set together with BACKDAYS, the calendar window stretches backward and forward from the evaluation date; when FORWDAYS is set while BACKDAYS is left initial, the window starts at today and moves forward only.

**GVTYP** (P&L statmt acct type)

Narrows SKA1 rows by the profit-and-loss statement account type. The join path in the supplied ABAP also keeps accounts whose completed-business-purpose indicator matches the fixed X rule on SKA1, so your parameter values should reflect the types you expect to coexist with that rule.

**KSTAR** (Cost Element)

Cost Center is used to classify and track expenses or revenues within the Controlling (CO) module. It serves as a bridge between financial accounting (FI) and cost accounting (CO), storing the G/L account number for primary costs or dedicated cost elements for secondary allocations.

**KTOKS** (Account Group)

Restricts G/L accounts to selected account groups, which is useful when policy differs by group (for example balance sheet groups versus operational P&L groups).

**KTOPL** (Chart of Accounts)

Chart of accounts governing GL account numbering, groups, and financial statement versions.

**LANGU** (Language Key)

Language key used for language-dependent texts and user-language filtering.

**SAKAN** (G/L account)

The SAKAN field stores the significant length of a G/L account number (e.g., "1000") as defined in the Chart of Accounts, distinguishing it from the internal 10-digit format that often includes leading zeros.

**SAKNR** (G/L Account)

Identifies the main G/L account number on the SKA1 row so the run can target a known account list or range.

**SAKNR_DESC** (G/L Acct Long Text)

Filters by the long text of the G/L account itself, which supports searches by account wording when the numeric account is not yet known.

**TARGET_CUKY** (Target Curr. Key)

Field used in currency conversion to specify the target currency key (e.g., USD) into which an amount should be translated.

**USNAM** (Created by)

SAP changed-by/created-by user field used for accountability filtering.

**XBILK** (Balance sheet account)

Flag that identifies an account as a Balance Sheet account when checked, or a P&L account when left blank.

**XBILK Options:**
- **X** — Balance sheet accounts only.
- (blank) — Not limited to balance sheet accounts by this flag alone; other filters still apply.

**XLOEV** (Mark for deletion)

Master data flag in that indicates a record is marked for deletion.

**XLOEV Options:**
- **X** — Include only accounts flagged for deletion.
- (blank) — Do not use this flag to require deletion; other selection values still apply.

**XSPEA** (Blocked for creation)

Restricts accounts that are blocked for creation.

**XSPEA Options:**
- **X** — Include only accounts blocked for creation.
- (blank) — Do not require the creation block flag through this parameter.

**XSPEB** (Blocked for posting)

Restricts accounts that are blocked for planning.

**XSPEB Options:**
- **X** — Include only accounts blocked for posting.
- (blank) — Do not require the posting block flag through this parameter.

**XSPEP** (Blocked for planning)

Restricts accounts that are blocked for planning in the company code.

**XSPEP Options:**
- **X** — Include only accounts blocked for planning.
- (blank) — Do not require the planning block flag through this parameter.


### Parameter Relationships

**Time window and reference date**

- **BACKDAYS** and **FORWDAYS** build the calendar window applied when the caller does not pass an explicit date range. **BACKDAYS** reaches backward from the evaluation date; **FORWDAYS** extends the high date forward or, when used alone with **BACKDAYS** initial, shifts the window entirely into the future per the logic in the supplied ABAP.
- **DATE_REF_FLD** chooses whether that window is copied onto **ERDAT** (G/L account creation on SKA1) or **ERSDA** (cost-element creation on CSKA). **BACKDAYS** always anchors to the field named here, not to both dates at once.

**Age filter after the window**

- **DURATION** and **DURATION_UNIT** work together after rows are returned: elapsed time is computed from each row's reference date (and optional time reference when configured in the framework) to the evaluation clock, using **DURATION_UNIT** as the unit of measure. Only rows whose computed duration falls in the **DURATION** selection range remain.

**Currency display**

- **TARGET_CUKY** and **EXC_RATE_TYPE** are read as a pair for the monitoring family's currency conversion path so displayed amounts use a consistent target currency and rate type when conversion runs.

**Text retrieval**

- **LANGU** drives which language is used when the program resolves long texts for financial statement items and G/L accounts in the second processing pass.


### Default Values

- **BACKDAYS** - 10
- **DATE_REF_FLD** - ERDAT
- **DURATION** - initial - treated as no duration cutoff when range empty by code
- **DURATION_UNIT** - D
- **EXC_RATE_TYPE** - M
- **LANGU** - EN

### Practical Example of Parameter Configuration

**Use Case 1: Month-end scan for new P&L accounts missing cost elements**

**Purpose:** After chart-of-accounts maintenance, controllers review accounts created in the last month on a specific chart and let balance-sheet and cost-element filters stay open so every new P&L row that still lacks a cost element surfaces.

```
BACKDAYS = 30
DATE_REF_FLD = ERDAT
KTOPL = YB01
XBILK = 
GVTYP = 
KSTAR = 
```

**Use Case 2: Forward-looking window for upcoming postings**

**Purpose:** Project accounting focuses on cost elements created in the next two weeks for operational account groups, then drops rows that are not yet seven days old to concentrate on items that should already be fully maintained.

```
FORWDAYS = 14
DATE_REF_FLD = ERSDA
KTOKS = YOP1 - YOP9
DURATION = 7
DURATION_UNIT = D
```

**Use Case 3: Full-day aging on blocked posting accounts**

**Purpose:** A three-month lookback on accounts blocked for posting, evaluated with full-day duration counting and results reviewed in USD, highlights long-blocked accounts where a missing cost element may be delaying remediation.

```
BACKDAYS = 90
DATE_REF_FLD = ERDAT
DURATION = 30
DURATION_UNIT = F
XSPEB = X
TARGET_CUKY = USD
```

**Use Case 4: Tight audit sample on financial statement lines**

**Purpose:** Internal audit requests a narrow slice of FS items and description keywords while keeping the standard exchange-rate type and English text resolution for consistent evidence packs.

```
BILKT = 0011000000 - 0011999999
SAKNR_DESC = *lease*
EXC_RATE_TYPE = M
LANGU = EN
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_07_COST_ELEM_MIS | KTOPL | Chart of accounts | CHAR(4) | KTOPL |
| /SKN/S_SW_10_07_COST_ELEM_MIS | SAKNR | G/L account number | CHAR(10) | SAKNR |
| /SKN/S_SW_10_07_COST_ELEM_MIS | XBILK | Indicator: account is a balance sheet account? | CHAR(1) | XBILK |
| /SKN/S_SW_10_07_COST_ELEM_MIS | SAKAN | Alternative account number in company code | CHAR(10) | SAKAN |
| /SKN/S_SW_10_07_COST_ELEM_MIS | BILKT | Financial statement item | CHAR(10) | BILKT |
| /SKN/S_SW_10_07_COST_ELEM_MIS | ERDAT | Date on which the financial account was created | DATS(8) | ERDAT |
| /SKN/S_SW_10_07_COST_ELEM_MIS | ERNAM | Name of person who created the object | CHAR(12) | ERNAM |
| /SKN/S_SW_10_07_COST_ELEM_MIS | GVTYP | Business purpose completed flag (PSM account) | CHAR(1) | GVTYP |
| /SKN/S_SW_10_07_COST_ELEM_MIS | KTOKS | Account group | CHAR(4) | KTOKS |
| /SKN/S_SW_10_07_COST_ELEM_MIS | XLOEV | Indicator: account marked for deletion? | CHAR(1) | XLOEV |
| /SKN/S_SW_10_07_COST_ELEM_MIS | XSPEA | Indicator: account blocked for creation? | CHAR(1) | XSPEA |
| /SKN/S_SW_10_07_COST_ELEM_MIS | XSPEB | Indicator: account blocked for posting? | CHAR(1) | XSPEB |
| /SKN/S_SW_10_07_COST_ELEM_MIS | XSPEP | Indicator: account blocked for planning? | CHAR(1) | XSPEP |
| /SKN/S_SW_10_07_COST_ELEM_MIS | KSTAR | Cost element | CHAR(10) | KSTAR |
| /SKN/S_SW_10_07_COST_ELEM_MIS | ERSDA | Created on (cost element / CSKA) | DATS(8) | ERSDA |
| /SKN/S_SW_10_07_COST_ELEM_MIS | USNAM | User name (CSKA) | CHAR(12) | USNAM |
| /SKN/S_SW_10_07_COST_ELEM_MIS | DURATION | Duration in time units (computed) | INT4(10) | INT4 |
| /SKN/S_SW_10_07_COST_ELEM_MIS | DURATION_UNIT | Duration unit (from selection) | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_07_COST_ELEM_MIS | BILKT_DESC | G/L account long text for FS item (BILKT) | CHAR(50) | TXT50_SKAT |
| /SKN/S_SW_10_07_COST_ELEM_MIS | SAKNR_DESC | G/L account long text for G/L account (SAKNR) | CHAR(50) | TXT50_SKAT |

## ABAP Code

```abap
FUNCTION /SKN/FC_SW_10_07_COST_ELEM_MIS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_07_COST_ELEM_MIS OPTIONAL
*"----------------------------------------------------------------------
  INCLUDE /SKN/P_SW_MN_AN_AR_DATA_DECL.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: DATUM DATUM.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: ERDAT ERDAT_RF.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: KTOPL KTOPL.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: SAKNR SAKNR.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: XBILK XBILK.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: SAKAN SAKAN.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BILKT BILKT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: ERNAM ERNAM_RF.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: GVTYP GVTYP.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: KTOKS KTOKS.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: XLOEV XLOEV.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: XSPEA XSPEA.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: XSPEB XSPEB.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: XSPEP XSPEP.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: KSTAR KSTAR.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: ERSDA ERFDT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: USNAM ERFNM.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: BILKT_DESC TXT50_SKAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: SAKNR_DESC TXT50_SKAT.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: BACKDAYS /SKN/E_MN_AN_BACKDAYS.
  LV_BACKDAYS = '10'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: DATE_REF_FLD NAME_FELD.
  LV_DATE_REF_FLD = 'ERDAT'.
  ##NO_HANDLER
  ##NEEDED
  DATA_SINGLE: CURRENCY_CONV_DATE /SKN/E_MN_AN_CUR_CONV_DATE_FLD.
  ##NO_HANDLER
  ##NEEDED
  DATA_MULTY: DURATION /SKN/E_SW_DURATION.
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
  DATA_SINGLE: TARGET_CUKY /SKN/E_MN_AN_TARGET_CURR.
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
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: ERDAT.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: KTOPL.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: SAKNR.
  CONVERT_MULTY: SAKNR ALPHA.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: XBILK.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: SAKAN.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BILKT.
  CONVERT_MULTY: BILKT ALPHA.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: ERNAM.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: GVTYP.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: KTOKS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: XLOEV.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: XSPEA.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: XSPEB.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: XSPEP.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: KSTAR.
  CONVERT_MULTY: KSTAR ALPHA.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: ERSDA.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: USNAM.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: BILKT_DESC.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: SAKNR_DESC.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: BACKDAYS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: DATE_REF_FLD.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: CURRENCY_CONV_DATE.
  ##NO_HANDLER
  ##NEEDED
  SELECT_MULTY: DURATION.
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
  SELECT_SINGLE: TARGET_CUKY.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: TIME_REF_FLD.
  ##NEEDED
  DATA LV_SW_DEST TYPE RFCDEST.
  ##NEEDED
  DATA LV_DELIMITER TYPE SONV-FLAG.
  ##NEEDED
  DATA LV_NO_DATA TYPE SONV-FLAG.
  ##NEEDED
  DATA LV_ROWSKIPS TYPE SOID-ACCNT.
  ##NEEDED
  DATA LV_ROWCOUNT TYPE SOID-ACCNT.
  ##NEEDED
  DATA LV_REC_CNT_ONLY TYPE FLAG.
  ##NEEDED
  DATA LV_ROWCOUNT2 TYPE SOID-ACCNT.
  ##NEEDED
  DATA LT_OPTIONS TYPE TABLE OF RFC_DB_OPT.
  ##NEEDED
  DATA LT_DATA TYPE TABLE OF /SKN/S_SW_TAB2000.
  ##NEEDED
  DATA LT_TABLES_LIST TYPE /SKN/TT_TABLES.
  ##NEEDED
  DATA LT_JOIN_CONDITION TYPE /SKN/TT_TABLE_JOIN.
  ##NEEDED
  DATA LT_SEL_FIELDS TYPE /SKN/TT_SEL_FIELDS.
  ##NEEDED
  DATA LT_SORT_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT.
  ##NEEDED
  DATA LT_GROUP_BY_OPTIONS TYPE TABLE OF /SKN/S_SW_RFC_JOIN_DB_SORT.
  ##NEEDED
  DATA LT_HAVING_OPTIONS TYPE TABLE OF RFC_DB_OPT.
  ##NEEDED
  DATA LT_OUTPUT_FIELDS TYPE /SKN/TT_RFC_DB_FLD_EXTEND.
  ##NEEDED
  DATA LT_DFIES TYPE TABLE OF DFIES.
  ##NEEDED
  DATA LT_RETURN TYPE BAPIRET2_T.
  ##NEEDED
  DATA LT_ALL_ENTRIES_TAB TYPE TABLE OF /SKN/S_SW_TAB6000.
  ##NEEDED
  DATA LT_ALL_ENTRIES_COND TYPE TABLE OF /SKN/S_TABLE_JOIN.
  ##NEEDED
  DATA LT_ALL_ENTRIES_DFIES TYPE TABLE OF DFIES.
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
  ##NEEDED
  DATA LV_KTOPL TYPE KTOPL.
  ##NEEDED
  DATA LV_SAKNR TYPE SAKNR.
  ##NEEDED
  DATA LV_SW_DEST2 TYPE RFCDEST.
  ##NEEDED
  DATA LV_TXT20 TYPE TXT20_SKAT.
  ##NEEDED
  DATA LV_TXT50 TYPE TXT50_SKAT.
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  REFRESH LT_OPTIONS.
  REFRESH LT_OUT_WHERE_COND.
  REFRESH LT_TABLES_LIST.
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
    WHEN 'ERDAT'.
      IF R_ERDAT[] IS INITIAL.
        R_ERDAT[] = R_DATUM[].
      ENDIF.
    WHEN 'ERSDA'.
      IF R_ERSDA[] IS INITIAL.
        R_ERSDA[] = R_DATUM[].
      ENDIF.
  ENDCASE.
  REFRESH R_DATUM.
  ##NO_HANDLER
  _APPEND_TABLES_LIST 'CSKA' 'X' 'A'.
  ##NO_HANDLER
  _APPEND_TABLES_LIST 'SKA1' '' 'B'.
  LV_RANGE = 'B~KTOPL'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE KTOPL.
  LV_RANGE = 'B~SAKNR'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE SAKNR.
  LV_RANGE = 'B~XBILK'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE XBILK.
  LV_RANGE = 'B~SAKAN'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE SAKAN.
  LV_RANGE = 'B~BILKT'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE BILKT.
  LV_RANGE = 'B~ERDAT'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE ERDAT.
  LV_RANGE = 'B~ERNAM'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE ERNAM.
  LV_RANGE = 'B~GVTYP'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE GVTYP.
  LV_RANGE = 'B~KTOKS'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE KTOKS.
  LV_RANGE = 'B~XLOEV'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE XLOEV.
  LV_RANGE = 'B~XSPEA'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE XSPEA.
  LV_RANGE = 'B~XSPEB'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE XSPEB.
  LV_RANGE = 'B~XSPEP'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE XSPEP.
  LV_RANGE = 'A~KSTAR'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE KSTAR.
  LV_RANGE = 'A~ERSDA'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE ERSDA.
  LV_RANGE = 'A~USNAM'.
  ##NO_HANDLER
  _RANGE_TO_SEL_TABLE LV_RANGE USNAM.
  LT_OPTIONS[] = LT_OUT_WHERE_COND[].
  LT_OPTIONS_MAIN[] = LT_OPTIONS[].
  IF LT_OPTIONS_MAIN IS NOT INITIAL.
    LS_OPTION-TEXT = 'AND'.
    APPEND LS_OPTION  TO LT_OPTIONS_MAIN.
  ENDIF.
  LV_LEFTTAB = 'B'.
  LV_LEFTFIELD = 'KTOPL'.
  LV_RIGHTTAB = 'A'.
  LV_RIGHTFIELD = 'KTOPL'.
  _JOIN_CONDITION LV_LEFTTAB LV_LEFTFIELD LV_RIGHTTAB LV_RIGHTFIELD.
  LV_LEFTTAB = 'B'.
  LV_LEFTFIELD = 'SAKNR'.
  LV_RIGHTTAB = 'A'.
  LV_RIGHTFIELD = 'KSTAR'.
  _JOIN_CONDITION LV_LEFTTAB LV_LEFTFIELD LV_RIGHTTAB LV_RIGHTFIELD.
  LS_OPTION-TEXT = '('.
  APPEND LS_OPTION  TO LT_OPTIONS_MAIN.
  LS_OPTION-TEXT = '( SKA1~GVTYP EQ ''X'' )'.
  APPEND LS_OPTION  TO LT_OPTIONS_MAIN.
  LS_OPTION-TEXT = ')'.
  APPEND LS_OPTION  TO LT_OPTIONS_MAIN.
  CLEAR IS_ALERT.
  REFRESH T_DATA.
  REFRESH LT_SORT_OPTIONS.
  REFRESH LT_OUT_WHERE_COND.
  REFRESH LT_GROUP_BY_OPTIONS.
  CLEAR LT_DATA.
  CLEAR LT_DATA_RFC.
  CLEAR LT_SEL_FIELDS.
  CLEAR LT_RETURN.
  IF LT_OPTIONS_MAIN IS NOT INITIAL.
    CLEAR LT_OPTIONS.
    LT_OPTIONS = LT_OPTIONS_MAIN.
  ENDIF.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'KTOPL'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'SAKNR'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'XBILK'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'SAKAN'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'BILKT'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'ERDAT'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'ERNAM'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'GVTYP'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'KTOKS'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'XLOEV'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'XSPEA'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'XSPEB'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'B'.
  LS_SEL_FIELDS-FIELD = 'XSPEP'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'A'.
  LS_SEL_FIELDS-FIELD = 'KSTAR'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'A'.
  LS_SEL_FIELDS-FIELD = 'ERSDA'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  CLEAR LS_SEL_FIELDS.
  LS_SEL_FIELDS-TABLE = 'A'.
  LS_SEL_FIELDS-FIELD = 'USNAM'.
  APPEND LS_SEL_FIELDS  TO LT_SEL_FIELDS.
  ##NO_HANDLER
  ##NEEDED
  SELECT_SINGLE: SW_DEST.
  CLEAR: LS_SEL_FIELDS, LT_DATA_TMP.
  CALL FUNCTION '/SKN/RFC_JOIN_TABLES'
    DESTINATION LV_SW_DEST
*   EXPORTING
*     delimiter            = ' '
*     NO_DATA              = ' '
*     ROWSKIPS             = 0
*     rowcount             = im_number
*     REC_CNT_ONLY         =
    IMPORTING
      ##NEEDED
      ROWCOUNT             = LV_ROWCOUNT
    TABLES
      OPTIONS              = LT_OPTIONS
      DATA                 = LT_DATA
      TABLES_LIST          = LT_TABLES_LIST ##ENH_OK
      JOIN_CONDITION       = LT_JOIN_CONDITION ##ENH_OK
      SEL_FIELDS           = LT_SEL_FIELDS ##ENH_OK
      SORT_OPTIONS         = LT_SORT_OPTIONS ##ENH_OK
      GROUP_BY_OPTIONS     = LT_GROUP_BY_OPTIONS ##ENH_OK
      HAVING_OPTIONS       = LT_HAVING_OPTIONS ##ENH_OK
      OUTPUT_FIELDS        = LT_OUTPUT_FIELDS ##ENH_OK
      DFIES                = LT_DFIES ##ENH_OK
      RETURN               = LT_RETURN ##ENH_OK
      ALL_ENTRIES_TAB      = LT_ALL_ENTRIES_TAB ##ENH_OK
      ALL_ENTRIES_COND     = LT_ALL_ENTRIES_COND ##ENH_OK
      ALL_ENTRIES_DFIES    = LT_ALL_ENTRIES_DFIES ##ENH_OK
    EXCEPTIONS
      TABLE_NOT_AVAILABLE  = 1
      TABLE_WITHOUT_DATA   = 2
      OPTION_NOT_VALID     = 3
      FIELD_NOT_VALID      = 4
      NOT_AUTHORIZED       = 5
      DATA_BUFFER_EXCEEDED = 6
      OTHERS               = 7.
  CLEAR: LV_ROWCOUNT, LT_JOIN_CONDITION, LT_SEL_FIELDS,
         LT_SORT_OPTIONS, LT_GROUP_BY_OPTIONS, LT_TABLES_LIST.
  IF SY-SUBRC IS NOT INITIAL OR LT_RETURN IS NOT INITIAL.
    CLEAR LT_DATA_RFC.
  ELSE.
    _RFC_TO_T_DATA_INDEX LT_DATA LT_DATA_TMP LT_OUTPUT_FIELDS 1.
    IF LT_DATA_TMP[] IS NOT INITIAL.
      APPEND LINES OF LT_DATA_TMP[] TO T_DATA[].
    ENDIF.
  ENDIF.
  CHECK T_DATA[] IS NOT INITIAL.
  DELETE T_DATA WHERE KSTAR IS NOT INITIAL.
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
  LOOP AT T_DATA.
    CLEAR LV_FIELDTAB.
    CLEAR LV_FIELDTAB2.
    LV_FIELDTAB = 'BILKT'.
    LV_FIELDTAB2 = 'KTOPL'.
    IF LV_LANGU IS INITIAL.
      LV_LANGU = 'E'.
    ENDIF.
    LV_DESC_FIELD_PR = 'BILKT'.
    ##NO_HANDLER
    ##NEEDED
    SELECT_SINGLE: KTOPL.
    ##NO_HANDLER
    ##NEEDED
    SELECT_SINGLE: SW_DEST2.
* The parameter 'lv_fieldtab' and
* 'lv_fieldtab2' are declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
    CLEAR: LV_FLD, LV_SAKNR, LV_TXT50.
    IF LV_FIELDTAB IS NOT INITIAL.
      CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
      ASSIGN (LV_FLD) TO <FS_VAL>.
* Field Value of the "lv_fieldtab"
      IF <FS_VAL> IS ASSIGNED.
        LV_SAKNR = <FS_VAL>.
        UNASSIGN <FS_VAL>.
      ENDIF.
      CLEAR: LV_FLD.
      IF LV_FIELDTAB2 IS NOT INITIAL.
        CONCATENATE 'T_DATA-' LV_FIELDTAB2 INTO LV_FLD.
        ASSIGN (LV_FLD) TO <FS_VAL>.
* Field Value of the "lv_fieldtab2"
        IF <FS_VAL> IS ASSIGNED.
          CLEAR: LV_KTOPL.
          LV_KTOPL = <FS_VAL>.
          UNASSIGN <FS_VAL>.
        ENDIF.
      ENDIF.
      IF LV_SAKNR IS NOT INITIAL AND LV_KTOPL IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_GL_DESC'
          EXPORTING
            KTOPL      = LV_KTOPL           " Chart of Accounts
            SAKNR      = LV_SAKNR           " G/L Account Number
            LANGU      = LV_LANGU           " Language Key
            SW_DEST    = LV_SW_DEST
          IMPORTING
            TXT50      = LV_TXT50           " G/L Account Long Text
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
        IF SY-SUBRC EQ 0.
          CLEAR: LV_FLD.
          CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_TXT50.
            MODIFY T_DATA.
            UNASSIGN <FS_VAL>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR LV_FIELDTAB.
    CLEAR LV_FIELDTAB2.
    LV_FIELDTAB = 'SAKNR'.
    LV_FIELDTAB2 = 'KTOPL'.
    IF LV_LANGU IS INITIAL.
      LV_LANGU = 'E'.
    ENDIF.
    LV_DESC_FIELD_PR = 'SAKNR'.
* The parameter 'lv_fieldtab' and
* 'lv_fieldtab2' are declared
* at '/SKN/P_SW_MN_AN_AR_DATA_DECL' include
* and is initialized at the source code
    CLEAR: LV_FLD, LV_SAKNR, LV_TXT50.
    IF LV_FIELDTAB IS NOT INITIAL.
      CONCATENATE 'T_DATA-' LV_FIELDTAB INTO LV_FLD.
      ASSIGN (LV_FLD) TO <FS_VAL>.
* Field Value of the "lv_fieldtab"
      IF <FS_VAL> IS ASSIGNED.
        LV_SAKNR = <FS_VAL>.
        UNASSIGN <FS_VAL>.
      ENDIF.
      CLEAR: LV_FLD.
      IF LV_FIELDTAB2 IS NOT INITIAL.
        CONCATENATE 'T_DATA-' LV_FIELDTAB2 INTO LV_FLD.
        ASSIGN (LV_FLD) TO <FS_VAL>.
* Field Value of the "lv_fieldtab2"
        IF <FS_VAL> IS ASSIGNED.
          CLEAR: LV_KTOPL.
          LV_KTOPL = <FS_VAL>.
          UNASSIGN <FS_VAL>.
        ENDIF.
      ENDIF.
      IF LV_SAKNR IS NOT INITIAL AND LV_KTOPL IS NOT INITIAL.
        CALL FUNCTION '/SKN/F_SW_10_GL_DESC'
          EXPORTING
            KTOPL      = LV_KTOPL           " Chart of Accounts
            SAKNR      = LV_SAKNR           " G/L Account Number
            LANGU      = LV_LANGU           " Language Key
            SW_DEST    = LV_SW_DEST
          IMPORTING
            TXT50      = LV_TXT50           " G/L Account Long Text
          EXCEPTIONS
            WRONG_CODE = 1
            OTHERS     = 2.
        IF SY-SUBRC EQ 0.
          CLEAR: LV_FLD.
          CONCATENATE LV_FIELDTAB '_DESC' INTO LV_FLD.
          ASSIGN COMPONENT LV_FLD OF STRUCTURE T_DATA TO <FS_VAL>.
          IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
            <FS_VAL> = LV_TXT50.
            MODIFY T_DATA.
            UNASSIGN <FS_VAL>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY T_DATA[] FROM  T_DATA.
  ENDLOOP.
  DELETE T_DATA[] WHERE SAKNR_DESC NOT IN  R_SAKNR_DESC[].
  DELETE T_DATA[] WHERE BILKT_DESC NOT IN  R_BILKT_DESC[].
  CHECK T_DATA[] IS NOT INITIAL.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
