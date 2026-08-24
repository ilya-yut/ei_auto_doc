# Exception Indicator: Customer Credit limit ( SW_10_01_CUST_CR_LIM)

## General Overview

This Exception Indicator identifies customer credit management records where calculated credit exposure or credit-limit utilization meets configured thresholds, returning credit master data enriched with customer description and recomputed open-order exposure.

This EI serves as an essential control for credit management governance by:

- Enabling detection of customers whose total credit exposure or limit utilization exceeds review thresholds
- Supporting monitoring of credit control areas, credit accounts, and individual customers within a forward-looking date window
- Providing visibility into receivables, special liabilities, open order values, and secured receivables on flagged records
- Enabling segmentation by risk category, credit representative group, and review dates for targeted follow-up
- Supporting recurring sampling before credit committee review or period close

Typical use includes credit limit utilization monitoring, exposure threshold alerts, and review-date-driven credit master sampling. Results are intended for exception workflows rather than operational credit list reporting.

The routine reads customer credit management master records, aggregates open order and delivery values, recomputes total exposure and limit utilization percentage, applies configured filters, enriches customer description, and raises an alert when qualifying rows remain.


## Problem Description

Failure to monitor customer credit exposure and limit utilization creates multiple risks across credit control, collections, and revenue protection:

**Credit and Collections Risks**

- Customers approaching or exceeding credit limits may continue to receive orders or deliveries without timely review
- Undetected high exposure relative to approved limits can leave receivables and open order value unmanaged
- Credit master changes and review dates that are overdue may not trigger structured follow-up

**Operational Risks**

- Monitoring windows misaligned with review calendars can exclude near-term review cases or retain stale records
- Exposure and utilization thresholds set too broadly can hide actionable customers or create reviewer fatigue
- Scope that is not tuned to credit control area, customer, or risk category can mix irrelevant records into the review queue

**Control and Audit Risks**

- Weak credit exposure monitoring reduces evidence that limit utilization was reviewed before release decisions
- Lack of recurring exception review limits accountability for credit operations follow-up on high-exposure accounts
- Missing customer and exposure context delays escalation of commercially significant credit cases

## Suggested Resolution

**Immediate Response**

- Review flagged records for customer, credit control area, exposure amount, and limit utilization level
- Confirm with credit management whether current exposure and limit settings remain appropriate
- Prioritize customers with highest utilization or exposure for immediate follow-up

**System Assessment**

- Validate forward monitoring window and reference-date settings against credit review cadence
- Tune credit control area, utilization, and exposure scope so results stay actionable
- Compare exception counts by credit control area, risk category, and customer group to identify systematic gaps

**Corrective Actions**

- Adjust credit limits, blocks, or release decisions through standard credit management processes where review confirms action is required
- Update monitoring scope after cleanup so results reflect truly exceptional exposure or utilization cases
- Document review outcomes and schedule recurring runs before credit committee meetings or close milestones


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | ABSBT | Secured receivables | CURR | 15 | 2 | ABSBT_SUM | WERTV8 |
| 2 | AEDAT | Changed on | DATS | 8 | 0 | AEDAT_CM | DATUM |
| 3 | AENAM | CHAR | 12 | 0 | AENAM_CM | USNAM |  |
| 4 | AETXT | Text changed on | DATS | 8 | 0 | AETXT_CM | DATUM |
| 5 | CASHA | Amnt of last payment | CURR | 13 | 2 | CASHA | WERT7 |
| 6 | CASHC | Currency | CUKY | 5 | 0 | CASHC | WAERS |
| 7 | CASHD | Date of last pmnt | DATS | 8 | 0 | CASHD | DATUM |
| 8 | CRBLB | Blocked | CHAR | 1 | 0 | CRBLB_CM | XFELD |
| 9 | CTLPC | Risk category | CHAR | 3 | 0 | CTLPC_CM | CTLPC_CM |
| 10 | CUST_DESC | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 11 | DATE_REF_FLD | Refference field name for date |  | 0 | 0 |  |  |
| 12 | DATUM | DATS | 8 | 0 | DATUM | DATUM |  |
| 13 | DBEKR | Recmd. Credit Limit | CURR | 15 | 2 | DBEKR_CM | WERT8 |
| 14 | DBMON | Monitoring | DATS | 8 | 0 | DBMON_CM | DATUM |
| 15 | DBPAY | Payment index | CHAR | 3 | 0 | DBPAY_CM | CHAR3 |
| 16 | DBRAT | D&B rating | CHAR | 3 | 0 | DBRAT_CM | CHAR3 |
| 17 | DBRTG | Rating | CHAR | 5 | 0 | DBRTG_CM | CHAR5 |
| 18 | DBWAE | Currency | CUKY | 5 | 0 | DBWAE_CM | WAERS |
| 19 | DTREV | Last internal review | DATS | 8 | 0 | DTREV_CM | DATUM |
| 20 | ERDAT | Created on | DATS | 8 | 0 | ERDAT_RF | DATUM |
| 21 | ERNAM | Created by | CHAR | 12 | 0 | ERNAM_RF | USNAM |
| 22 | FORWDAYS | forward days |  | 0 | 0 |  |  |
| 23 | GRUPP | Cust.cred.group | CHAR | 4 | 0 | GRUPP_CM | GRUPP_CM |
| 24 | KDGRP | CHAR | 2 | 0 | KDGRP | KDGRP |  |
| 25 | KKBER | Credit control area | CHAR | 4 | 0 | KKBER | KKBER |
| 26 | KLIMK | Credit limit | CURR | 15 | 2 | KLIMK | WERT8 |
| 27 | KLPRZ | Credit limit used | DEC | 5 | 2 | KLPRZ_F02L | PRZ32 |
| 28 | KNKLI | Credit account | CHAR | 10 | 0 | KNKLI | KUNNR |
| 29 | KRAUS | Cred.info number | CHAR | 11 | 0 | KRAUS_CM | CHAR11 |
| 30 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 31 | LANGU | LANG | 1 | 0 | LANGU | SPRAS |  |
| 32 | NXTRV | Next internal review | DATS | 8 | 0 | NXTRV_CM | DATUM |
| 33 | OBLIG | Credit exposure | CURR | 15 | 2 | OBLIG_F02L | WRTV8 |
| 34 | PAYDB | D&B indicator | NUMC | 2 | 0 | PAYDB_CM | NUM02 |
| 35 | REVDB | Last ext.review | DATS | 8 | 0 | REVDB_CM | DATUM |
| 36 | SAUFT | Sales value | CURR | 15 | 2 | SAUFT | WRTV8 |
| 37 | SBDAT | DATS | 8 | 0 | SBDAT_CM | DATUM |  |
| 38 | SBGRP | Credit rep.group | CHAR | 3 | 0 | SBGRP_CM | SBGRP_CM |
| 39 | SKFOR | Total receivables | CURR | 15 | 2 | SKFOR | WRTV8 |
| 40 | SSOBL | Special liabil. | CURR | 15 | 2 | SSOBL | WRTV8 |
| 41 | SW_DEST | CHAR | 32 | 0 | RFCDEST | RFCDEST |  |
| 42 | UEDAT | Exceeded on | DATS | 8 | 0 | UEDAT | DATUM |
| 43 | XCHNG | Indicator: credit limit must b | CHAR | 1 | 0 | XCHNG_KNKK | XFELD |


### Parameter Configuration Guidelines

IMPORTANT: This EI defines 43 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ABSBT** (Secured receivables)

Mirrors how administrators slice operational lists: secured receivables (ABSBT) is one lever that shapes which rows are comparable run over run.

**AEDAT** (Changed on)

Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.

**AENAM** (CHAR)

Name of the user who last changed the object; paired with change dates for maker accountability in extracts.

**AETXT** (Text changed on)

Works downstream of the initial read so text changed on on AETXT still participates in row-level deletion rules.

**CASHA** (Amnt of last payment)

When populated, keeps the extract focused so amnt of last payment (CASHA) aligns with the intended triage slice.

**CASHC** (Currency)

Helps monitoring stay readable by requiring currency (CASHC) to match organizational or technical selectors when set.

**CASHD** (Date of last pmnt)

Guards against oversized extracts when date of last pmnt on CASHD is narrowed together with client, user, or session filters.

**CRBLB** (Blocked)

For distributed landscapes, blocked on CRBLB often anchors which application server or destination appears in results.

**CTLPC** (Risk category)

Narrows retrieved rows where risk category (CTLPC) must match the configured selection for this monitor.

**CUST_DESC** (Name)

Customer description/name text used for readable customer-level reporting.

**DATE_REF_FLD** (Refference field name for date)

Names the date field used as the reference for lookback and time-window filtering when explicit from/to dates are not set.

**DATE_REF_FLD Options:**
- ERDAT — Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.
- AEDAT — Date on which record was created (or last changed) is used to filter documents or master records by last maintenance activity.
- DBMON — Monitoring.
- CASHD — Date of last pmnt.
- SBDAT — DATS.
- AETXT — Text changed on.
- REVDB — Last ext.review.
- NXTRV — Next internal review.
- DTREV — Last internal review.
- UEDAT — Exceeded on.

**DATUM** (DATS)

Explicit monitoring date range supplied by the online monitor; when empty, the forward evaluation window is built from **FORWDAYS** relative to the current day.

**Not in use**
**DBEKR** (Recmd. Credit Limit)

Helps monitoring stay readable by requiring recmd. credit limit (DBEKR) to match organizational or technical selectors when set.

**DBMON** (Monitoring)

Combines with related filters so monitoring on DBMON refines which records remain for duration or state checks.

**DBPAY** (Payment index)

Mirrors how administrators slice operational lists: payment index (DBPAY) is one lever that shapes which rows are comparable run over run.

**DBRAT** (D&B rating)

Improves readability of exported lists because d&b rating (DBRAT) columns stay aligned with the configured filter intent.

**DBRTG** (Rating)

Separates cross-client noise from in-scope work when rating on DBRTG correlates with client or user attributes.

**DBWAE** (Currency)

Reduces false positives during peak windows by tightening currency through DBWAE alongside state filters.

**DTREV** (Last internal review)

When harmonized with related filters, last internal review on DTREV isolates the highest-risk record families.

**ERDAT** (Created on)

Created-on date on the persisted business record-standard master/document creation date axis in SAP tables.

**ERNAM** (Created by)

Created-by user ID used for maker-checker and ownership monitoring.

**FORWDAYS** (forward days)

FORWDAYS defines the historical monitoring window by specifying how many days forward from today to retrieve records. 0 - today, 1 - today + tomorrow etc.

Forwdays is based on DATE_REF_FLD field.

**GRUPP** (Cust.cred.group)

Prevents accidental global scans when cust.cred.group (GRUPP) is meant to stay within a controlled application slice.

**KDGRP** (CHAR)

Customer Group, used to categorize customers for pricing, discounts, and statistical analysis.

**KKBER** (Credit control area)

Credit control area key used to scope customer credit management records to one organizational credit segment.

**KLIMK** (Credit limit)

Approved credit limit amount on the customer credit record used as the denominator for utilization calculation.

**KLPRZ** (Credit limit used)

Calculated percentage of total credit exposure relative to the approved credit limit; rows outside the configured range are removed.

**KNKLI** (Credit account)

Combines with related filters so credit account on KNKLI refines which records remain for duration or state checks.

**KRAUS** (Cred.info number)

When populated, keeps the extract focused so cred.info number (KRAUS) aligns with the intended triage slice.

**KUNNR** (Customer)

Customer account is used to scope records to specific customers across SD/FI flows.

**LANGU** (LANG)

Language key used for language-dependent texts and user-language filtering.

**NXTRV** (Next internal review)

Documents expected operator behavior—next internal review on NXTRV should be set when that dimension is part of the control objective.

**OBLIG** (Credit exposure)

Total credit exposure computed as special liabilities plus total receivables plus recomputed open order value; rows outside the configured range are removed.

**PAYDB** (D&B indicator)

Gives auditors traceable criteria because d&b indicator on PAYDB is applied consistently before any alert flag is raised.

**REVDB** (Last ext.review)

Guards against oversized extracts when last ext.review on REVDB is narrowed together with client, user, or session filters.

**SAUFT** (Sales value)

Open order value recomputed from aggregated order and delivery totals before total credit exposure is calculated.

**SBDAT** (Reference date)

Reference date on the customer credit record; can be used as the monitoring reference when **DATE_REF_FLD** is SBDAT.

**SBGRP** (Credit rep.group)

Documents expected operator behavior—credit rep.group on SBGRP should be set when that dimension is part of the control objective.

**SKFOR** (Total receivables)

Mirrors how administrators slice operational lists: total receivables (SKFOR) is one lever that shapes which rows are comparable run over run.

**SSOBL** (Special liabil.)

Reflects real administration where special liabil. on SSOBL is routinely restricted to a single productive client or object family.

**SW_DEST** (CHAR)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**UEDAT** (Exceeded on)

Pairs with duration logic: once UEDAT passes list selection, elapsed time from the reference timestamp still must fit configured duration windows.

**XCHNG** (Indicator: credit limit must b)

Helps monitoring stay readable by requiring indicator: credit limit must b (XCHNG) to match organizational or technical selectors when set.

### Parameter Relationships

**Monitoring window:** When no explicit calendar range is supplied, **FORWDAYS** builds a forward date window from the current day. **DATE_REF_FLD** directs that window to the chosen credit master date field (for example next internal review, last change, or date the limit was exceeded).

**Credit master selection:** **KUNNR**, **KKBER**, **KNKLI**, **KLIMK**, and related credit master attributes filter rows read from customer credit management data before exposure is calculated.

**Exposure calculation:** Open order values from aggregated order and delivery totals replace the initial **SAUFT** value. **OBLIG** is computed as special liabilities plus total receivables plus the recomputed open order value.

**Utilization filter:** **KLPRZ** expresses **OBLIG** as a percentage of **KLIMK**. Rows are removed unless **OBLIG** and **KLPRZ** remain within their configured selections.

**Customer description:** **CUST_DESC** is filled from the customer description function for each retained **KUNNR** after filtering.

**Cloud execution:** When **SW_DEST** is set, processing delegates to the cloud wrapper and the on-premise path below that call is skipped.


### Default Values

- **FORWDAYS** - initial - treated as 7 by code
- **LANGU** - initial - treated as SY-LANGU by code

### Practical Example of Parameter Configuration

**Use Case 1: High credit limit utilization**

**Purpose:** Flag customers in one credit control area whose calculated limit utilization exceeds eighty percent.

```
KKBER = 1000
KLPRZ = 80 - 999
FORWDAYS = 7
```

**Use Case 2: Exposure above threshold**

**Purpose:** Review customers whose total credit exposure exceeds a monetary threshold.

```
OBLIG = 100000 - 999999999
KKBER = 1000
KUNNR = 100000
```

**Use Case 3: Next internal review due**

**Purpose:** Sample credit records whose next internal review date falls within the forward monitoring window.

```
DATE_REF_FLD = NXTRV
FORWDAYS = 14
KKBER = 1000
```

**Use Case 4: Limit exceeded date window**

**Purpose:** Monitor records where the credit limit was exceeded within the configured forward window.

```
DATE_REF_FLD = UEDAT
FORWDAYS = 30
CTLPC = A
KKBER = 1000
```

**Use Case 5: Single customer credit review**

**Purpose:** Review one customer's credit exposure and utilization in a credit control area.

```
KUNNR = 100000
KKBER = 1000
KLPRZ = 50 - 999
OBLIG = 50000 - 999999999
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_01_CUST_CRDT_LMT | ABSBT | Total Secured Receivables | CURR(15,2) | ABSBT_SUM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | AEDAT | Date of Last Change | DATS(8) | AEDAT_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | AETXT | Date of Last Text Change | DATS(8) | AETXT_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | CASHA | Amount of Last Payment | CURR(13,2) | CASHA |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | CASHC | Currency of Last Payment | CUKY(5) | CASHC |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | CASHD | Date of Last Payment | DATS(8) | CASHD |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | CRBLB | Indicator: Blocked by credit management ? | CHAR(1) | CRBLB_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | CTLPC | Credit management: Risk category | CHAR(3) | CTLPC_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | CUST_DESC | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | DBEKR | Recommended credit limit | CURR(15,2) | DBEKR_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | DBMON | Date Monitoring | DATS(8) | DBMON_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | DBPAY | Payment Index | CHAR(3) | DBPAY_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | DBRAT | do not use - replaced by DBRTG_CM | CHAR(3) | DBRAT_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | DBRTG | Rating | CHAR(5) | DBRTG_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | DBWAE | Currency of recommended credit limit | CUKY(5) | DBWAE_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | DTREV | Last internal review | DATS(8) | DTREV_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | ERDAT | Date on which the Record Was Created | DATS(8) | ERDAT_RF |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | ERNAM | Name of Person who Created the Object | CHAR(12) | ERNAM_RF |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | GRUPP | Customer Credit Group | CHAR(4) | GRUPP_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | KKBER | Credit control area | CHAR(4) | KKBER |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | KLIMK | Customer's credit limit | CURR(15,2) | KLIMK |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | KLPRZ | Credit limit used | DEC(5,2) | KLPRZ_F02L |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | KNKLI | Customer's account number with credit limit reference | CHAR(10) | KNKLI |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | KRAUS | Credit information number | CHAR(11) | KRAUS_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | KUNNR | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | NXTRV | Next internal review | DATS(8) | NXTRV_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | OBLIG | Credit exposure (for credit limit check) | CURR(15,2) | OBLIG_F02L |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | PAYDB | do not use - replaced by DBPAY_CM | NUMC(2) | PAYDB_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | REVDB | Last review (external) | DATS(8) | REVDB_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | SAUFT | Total of the sales values for the credit limit check | CURR(15,2) | SAUFT |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | SBGRP | Credit representative group for credit management | CHAR(3) | SBGRP_CM |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | SKFOR | Total receivables (for credit limit check) | CURR(15,2) | SKFOR |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | SSOBL | Relevant special liabilities for credit limit check | CURR(15,2) | SSOBL |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | UEDAT | Date on which the credit limit was exceeded | DATS(8) | UEDAT |
| /SKN/S_SW_10_01_CUST_CRDT_LMT | XCHNG | Indicator: credit limit must be recreated | CHAR(1) | XCHNG_KNKK |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_01_CUST_CRDT_LMT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_01_CUST_CRDT_LMT OPTIONAL
*"----------------------------------------------------------------------
DATA_SINGLE: LANGU  LANGU,
             FORWDAYS INT4,
             DATE_REF_FLD NAME_FELD.
 "lv_FORWDAYS = 100.
 "lv_DATE_REF_FLD = 'NXTRV'. "Default refference date field
LV_LANGU = SY-LANGU.
LV_FORWDAYS = 7.
"lv_DATE_REF_FLD = ' ERDAT '.
 SELECT_SINGLE: LANGU,
                FORWDAYS,
                DATE_REF_FLD.
DATA_MULTY: KUNNR	KUNNR,
            KKBER	KKBER,
            KLIMK	KLIMK,
            KNKLI	KNKLI,
            SAUFT	SAUFT,
            SKFOR	SKFOR,
            SSOBL	SSOBL,
            UEDAT	UEDAT,
            XCHNG	XCHNG_KNKK,
            ERNAM	ERNAM_RF,
            ERDAT	ERDAT_RF,
            CTLPC	CTLPC_CM,
            DTREV	DTREV_CM,
            CRBLB	CRBLB_CM,
            SBGRP	SBGRP_CM,
            NXTRV	NXTRV_CM,
            KRAUS	KRAUS_CM,
            PAYDB	PAYDB_CM,
            DBRAT	DBRAT_CM,
            REVDB	REVDB_CM,
            AEDAT	AEDAT_CM,
            AETXT	AETXT_CM,
            GRUPP	GRUPP_CM,
            AENAM	AENAM_CM,
            SBDAT	SBDAT_CM,
            KDGRP	KDGRP_CM,
            CASHD	CASHD,
            CASHA	CASHA,
            CASHC	CASHC,
            DBPAY	DBPAY_CM,
            DBRTG	DBRTG_CM,
            DBEKR	DBEKR_CM,
            DBWAE	DBWAE_CM,
            DBMON	DBMON_CM,
            ABSBT	ABSBT_SUM,
            KLPRZ	KLPRZ_F02L,
            OBLIG	OBLIG_F02L ,
            DATUM SY-DATUM.
SELECT_MULTY:
            KUNNR,
            KKBER,
            KLIMK,
            KNKLI,
            SAUFT,
            SKFOR,
            SSOBL,
            UEDAT,
            XCHNG,
            ERNAM,
            ERDAT,
            CTLPC,
            DTREV,
            CRBLB,
            SBGRP,
            NXTRV,
            KRAUS,
            PAYDB,
            DBRAT,
            REVDB,
            AEDAT,
            AETXT,
            GRUPP,
            AENAM,
            SBDAT,
            KDGRP,
            CASHD,
            CASHA,
            CASHC,
            DBPAY,
            DBRTG,
            DBEKR,
            DBWAE,
            DBMON,
            ABSBT,
            KLPRZ,
            OBLIG.
CONVERT_MULTY: KUNNR ALPHA.
DATA : S066_T TYPE TABLE OF S066,
       S066_WA LIKE LINE OF S066_T,
       S067_T TYPE TABLE OF S067,
       S067_WA LIKE LINE OF S067_T,
       DATE_FROM LIKE SY-DATUM ,
       DATE_TO LIKE SY-DATUM.
DATA : LANGU LIKE SY-LANGU .
DATA : TIME_DIFF TYPE  INT4 .
DATA : SY_TABIX LIKE SY-TABIX .
DATA : FLD(60) TYPE C .
DATA : REF_DATE TYPE D.
DATA: LD_REFE1(16) TYPE P.
CLEAR: LD_REFE1.
FIELD-SYMBOLS: <RES_FIELDS> TYPE /SKN/S_SW_10_01_CUST_CRDT_LMT ,
               <FS_V> TYPE ANY .
"--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_01_CUST_CRDT_LMT'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
   IF R_DATUM[] IS INITIAL .
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'BT' .
      IF LV_FORWDAYS > 0 .
        DATE_FROM = SY-DATUM .
        DATE_TO = SY-DATUM + LV_FORWDAYS.
      ELSE.
        DATE_FROM = SY-DATUM + LV_FORWDAYS .
        DATE_TO = SY-DATUM .
      ENDIF.
      RS_DATUM-LOW = DATE_FROM .
      RS_DATUM-HIGH = DATE_TO .
      APPEND RS_DATUM TO R_DATUM.
   ENDIF.
*   endif.
 "--- Set Reference Date Field
  CASE LV_DATE_REF_FLD.
    WHEN 'ERDAT'.
      R_ERDAT[] = R_DATUM[]. "Document created
    WHEN 'AEDAT'.
      R_AEDAT[] = R_DATUM[]. "changed on
    WHEN 'DBMON'.
      R_DBMON[] = R_DATUM[]. "DBMON Date Monitoring
    WHEN 'CASHD'.
      R_CASHD[] = R_DATUM[]. "CASHD Date of Last Payment
    WHEN 'SBDAT'.
      R_SBDAT[] = R_DATUM[]. "Reference Date
    WHEN 'AETXT'.
      R_AETXT[] = R_DATUM[].  "Date of Last Text Change
    WHEN 'REVDB'.
      R_REVDB[] = R_DATUM[]. "Last review (external)
    WHEN 'NXTRV'.
       R_NXTRV[] = R_DATUM[]. " Next internal review
    WHEN 'DTREV'.
      R_DTREV[] = R_DATUM[]. "Last internal review
    WHEN 'UEDAT'.
      R_UEDAT[] = R_DATUM[].  "Date on which the credit limit was exceeded
  ENDCASE.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT * FROM KNKK
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE KUNNR	IN R_KUNNR AND
            KKBER	IN R_KKBER AND
            KLIMK	IN R_KLIMK AND
            KNKLI	IN R_KNKLI AND
            SAUFT	IN R_SAUFT AND
            SKFOR	IN R_SKFOR AND
            SSOBL	IN R_SSOBL AND
            UEDAT	IN R_UEDAT AND
            XCHNG	IN R_XCHNG AND
            ERNAM	IN R_ERNAM AND
            ERDAT	IN R_ERDAT AND
            CTLPC	IN R_CTLPC AND
            DTREV IN R_DTREV AND
            CRBLB	IN R_CRBLB AND
            SBGRP	IN R_SBGRP AND
            NXTRV	IN R_NXTRV AND
            KRAUS	IN R_KRAUS AND
            PAYDB	IN R_PAYDB AND
            DBRAT	IN R_DBRAT AND
            REVDB	IN R_REVDB AND
            AEDAT	IN R_AEDAT AND
            AETXT	IN R_AETXT AND
            GRUPP	IN R_GRUPP AND
            AENAM	IN R_AENAM AND
            SBDAT	IN R_SBDAT AND
            KDGRP	IN R_KDGRP AND
            CASHD	IN R_CASHD AND
            CASHA	IN R_CASHA AND
            CASHC	IN R_CASHC AND
            DBPAY	IN R_DBPAY AND
            DBRTG	IN R_DBRTG AND
            DBEKR	IN R_DBEKR AND
            DBWAE	IN R_DBWAE AND
            DBMON	IN R_DBMON AND
            ABSBT	IN R_ABSBT .
  RS_KNKLI-SIGN = 'I' .
  RS_KNKLI-OPTION = 'EQ'.
  LOOP AT T_DATA ASSIGNING <RES_FIELDS>.
    RS_KNKLI-LOW = <RES_FIELDS>-KNKLI.
    APPEND RS_KNKLI TO R_KNKLI.
  ENDLOOP.
 SELECT KNKLI KKBER SUM( OEIKW )  AS OEIKW FROM S066
   INTO CORRESPONDING FIELDS OF TABLE S066_T
     WHERE KKBER IN R_KKBER AND
           KNKLI  IN R_KNKLI
    GROUP BY KNKLI KKBER
   ORDER BY KNKLI KKBER.
  SELECT KNKLI KKBER SUM( OLIKW ) AS OLIKW SUM( OFAKW ) AS OFAKW FROM S067
   INTO CORRESPONDING FIELDS OF TABLE S067_T
     WHERE KKBER IN R_KKBER AND
           KNKLI  IN R_KNKLI
    GROUP BY KNKLI KKBER
   ORDER BY KNKLI KKBER.
*********************************************************************************
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT T_DATA ASSIGNING <RES_FIELDS>.
    SY_TABIX = SY-TABIX .
*    concatenate 'T_DATA-' lv_DATE_REF_FLD into fld .
*    ASSIGN (fld) TO .
*    ref_date =  .
*SAUFT  CURR  15  2 Total of the sales values for the credit limit check
*SKFOR  CURR  15  2 Total receivables (for credit limit check)
*SSOBL  CURR  15  2 Relevant special liabilities for credit limit check
    CLEAR S066_WA.
    CLEAR S067_WA.
    READ TABLE S066_T INTO S066_WA BINARY SEARCH WITH KEY
      KNKLI = <RES_FIELDS>-KNKLI KKBER =  <RES_FIELDS>-KKBER.
    READ TABLE S067_T INTO S067_WA BINARY SEARCH WITH KEY
      KNKLI = <RES_FIELDS>-KNKLI KKBER =  <RES_FIELDS>-KKBER.
    <RES_FIELDS>-SAUFT = S067_WA-OLIKW + S067_WA-OFAKW + S066_WA-OEIKW.
    <RES_FIELDS>-OBLIG = <RES_FIELDS>-SSOBL + <RES_FIELDS>-SKFOR + <RES_FIELDS>-SAUFT.
*      if <res_fields>-KLIMK <> 0.
*        <res_fields>-KLPRZ = <res_fields>-OBLIG / <res_fields>-KLIMK .
*      endif.
      IF <RES_FIELDS>-KLIMK  = 0
      OR <RES_FIELDS>-OBLIG < 0.
        CLEAR: <RES_FIELDS>-KLPRZ.
      ELSE.
        LD_REFE1 = ( <RES_FIELDS>-OBLIG * 10000 ) / <RES_FIELDS>-KLIMK.
      ENDIF.
      IF <RES_FIELDS>-KLIMK = 0
      AND <RES_FIELDS>-OBLIG > 0.
        LD_REFE1 = 999.
      ENDIF.
      IF LD_REFE1 > 999.
        <RES_FIELDS>-KLPRZ = 999.
      ELSE.
        <RES_FIELDS>-KLPRZ = LD_REFE1 / 100.
      ENDIF.
  ENDLOOP.
  DELETE T_DATA WHERE OBLIG  NOT IN R_OBLIG .
  DELETE T_DATA WHERE KLPRZ  NOT IN R_KLPRZ .
******************************************************************************
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
*Ship-to party
    CALL FUNCTION '/SKN/F_SW_10_CUST_DESC'
      EXPORTING
        KUNNR                = T_DATA-KUNNR
      IMPORTING
        CUST_DESC            = T_DATA-CUST_DESC
      EXCEPTIONS
        WRONG_CUSTOMER       = 1
        OTHERS               = 2              .
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
