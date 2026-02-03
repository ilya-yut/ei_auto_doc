# Exception Indicator: Inactive Vendor - SW_10_06_INACT_VEND

## General Overview

This Exception Indicator (EI) monitors vendors that are marked inactive (e.g. central or company-code posting block, central or purchasing-organization deletion flag, or purchasing block) but still have an open balance above a configurable threshold. It uses vendor master (LFA1, LFB1, LFM1), vendor open-item and balance data (LFC1, LFC3), and company code and purchasing organization assignments (T001, T024E) to identify inactive vendors with remaining balances for cleanup, reconciliation, or release.

This EI serves as an essential control for vendor master and payables governance by:
- Enabling detection of inactive vendors with outstanding balances that may require clearing, reclassification, or release before deactivation
- Supporting identification of posting blocks and deletion flags that prevent new transactions while balances remain
- Providing visibility into balance amounts by company code and currency (including foreign currency) for prioritization
- Enabling analysis by company code, purchasing organization, account group, and planning group for accountability
- Supporting configurable lookback and balance thresholds so management can focus on material exceptions (e.g. above a given amount)

The EI supports month-end close, vendor master cleanup, and payables governance by surfacing inactive vendors with balances that meet configurable criteria. Data is drawn from LFA1, LFB1, LFM1, LFC1, LFC3, T001, and T024E.


## Problem Description

Failure to monitor inactive vendors with remaining balances creates multiple risks across financial reporting, operational management, and compliance.

**Financial and Reporting Issues**
- Unreviewed inactive vendors with open balances can distort payables and aging reports and delay period-end close
- Balances on blocked or deletion-flagged vendors may indicate uncleared items, duplicate postings, or misclassified accounts requiring correction
- Lack of visibility into inactive vendors with balances in foreign currency can complicate reconciliation and exposure reporting
- Delayed identification of inactive vendors with material balances may require restatements or adjustments when discovered late
- Absence of structured exception reporting by company code or purchasing organization weakens audit evidence for payables and vendor master controls

**Operational and Control Risks**
- Without visibility into which vendors are inactive and still have balances, cleanup and release procedures are delayed
- Posting blocks and deletion flags intended to prevent new transactions may leave legacy balances unreviewed
- Inability to filter by balance threshold or currency can dilute attention and delay identification of material exceptions
- Unmonitored inactive vendors with balances increase the risk of duplicate payments, unclaimed credits, or unreconciled items
- Lack of balance and period-level detail hinders operational follow-up and corrective action

**Management Visibility and Decision-Making Risks**
- Executives and controllers lack a single view of inactive vendors with balances for prioritization and resource allocation
- Unidentified inactive vendors with material balances can delay audit and compliance responses and increase residual risk
- Without configurable lookback and balance criteria, management cannot efficiently target high-value exceptions for review
- Limited ability to drill down by company code, purchasing organization, or account group restricts root-cause analysis and policy updates
- Inadequate monitoring undermines confidence in vendor master and payables controls and may affect external audit or regulatory outcomes

## Suggested Resolution

**Immediate Response**
- Review the inactive vendors with balances flagged by the EI to confirm whether balances are legitimate (e.g. pending clearance) or require correction
- Verify high-value or unusual balances using transaction FBL1N (Vendor Line Items) or S_ALR_87012082 (Vendor Balances) to check posting details and authorization
- Check posting block and deletion-flag settings (LFA1, LFB1, LFM1) to determine whether vendors should remain inactive or be released after balance clearance
- Identify the business context: planned deactivation with pending clearance, duplicate vendor, or erroneous block requiring release

**System Assessment**
- Analyze the selection dimensions (company code, purchasing organization, account group, balance threshold) to understand which factors drive the exception pattern
- Compare current exception volumes and balance amounts to prior periods using the same criteria to spot trends
- Review balance and currency distributions to identify exposure or concentration
- Assess vendor master and block/deletion-flag usage to detect misconfiguration or process gaps
- Validate the lookback and balance criteria so that the scope matches the control objective

**Corrective Actions**
- For erroneous or duplicate balances, initiate clearing, reclassification, or reversal via appropriate FI or MM transactions
- Escalate unexplained inactive vendors with material balances to accounting and procurement for validation and remediation
- Update vendor master (XD02, XK02) to release blocks or correct deletion flags where balances have been cleared and reactivation is justified
- Implement recurring EI runs and route results to payables and procurement owners for continuous monitoring of inactive vendors with balances
- Document findings and remediation for audit trail and management reporting


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AMOUNT | Credit | CURR | 15 | 2 | UMXXH | WRTV8 |
| 2 | AMOUNT_MAX | Max. Amount |  | 0 | 0 |  |  |
| 3 | AUSBK | Source company code | CHAR | 4 | 0 | AUSBK | BUKRS |
| 4 | BACKDAYS | Days Back |  | 0 | 0 |  |  |
| 5 | BALANCE_FR | Balance Amount in FR currency | CURR | 13 | 2 | ZWRBTR | ZWRBTR |
| 6 | BALANCE_NORMAL | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 7 | BALANCE_NORMAL_FR | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 8 | BALANCE_SPEC | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 9 | BALANCE_SPEC_FR | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 10 | BALANCE_TOTAL | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 11 | BALANCE_TOTAL_FR | Balance Amount | CURR | 15 | 2 | ZWRBTR | ZWRBTR |
| 12 | BELNR | Document Number | CHAR | 10 | 0 | BELNR_D | BELNR |
| 13 | BUDAT | Posting Date | DATS | 8 | 0 | BUDAT | DATUM |
| 14 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 15 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 16 | COUNTER | Number of active periods | INT4 | 10 | 0 | INT4 | INT4 |
| 17 | DMBTR | Amt.in loc.cur. | CURR | 13 | 2 | DMBTR | WERT7 |
| 18 | DMBTR_FR | Amt.in loc.cur. | CURR | 13 | 2 | DMBTR | WERT7 |
| 19 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 20 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 21 | ERDAT | Created on | DATS | 8 | 0 | ERDAT_RF | DATUM |
| 22 | FABKL | Factory calendar | CHAR | 2 | 0 | FABKL | WFCID |
| 23 | FACDATE | Factory date | DEC | 5 | 0 | FACDATE | HFDATE |
| 24 | FDGRV | Planning group | CHAR | 10 | 0 | FDGRV | FDGRP |
| 25 | GJAHR | Fiscal Year | NUMC | 4 | 0 | GJAHR | GJAHR |
| 26 | KONZS | Group key | CHAR | 10 | 0 | KONZS | KONZS |
| 27 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 28 | LFA1_LOEVM | Central deletion flag | CHAR | 1 | 0 | LOEVM_X | XFELD |
| 29 | LFA1_SPERM | Central purchasing block | CHAR | 1 | 0 | SPERM_X | XFELD |
| 30 | LFA1_SPERR | Central posting block | CHAR | 1 | 0 | SPERB_X | XFELD |
| 31 | LFB1_LOEVM | Delete flag for purchasing organization | CHAR | 1 | 0 | LOEVM_M | XFELD |
| 32 | LFB1_SPERR | Posting block for company code | CHAR | 1 | 0 | SPERB_B | XFELD |
| 33 | LFM1_LOEVM | Delete flag for purchasing organization | CHAR | 1 | 0 | LOEVM_M | XFELD |
| 34 | LFM1_SPERM | Purch. block for purchasing organization | CHAR | 1 | 0 | SPERM_M | XFELD |
| 35 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 36 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 37 | PARKED_IVNOICE | Parked Invoice Ind. | CHAR | 1 | 0 | /SKN/E_SW_PARK_INV |  |
| 38 | PERIOD | Active Period | CHAR | 7 | 0 | /SKN/E_SW_ACT_PER |  |
| 39 | POSSIBLE_APP | App Description | CHAR | 200 | 0 | /SKN/E_SW_APP_DESC | /SKN/D_SW_APP_DESC |
| 40 | SIGN | Operator |  | 0 | 0 |  |  |
| 41 | UMSAV | Balance Carryforward | CURR | 15 | 2 | UMSAV | WRTV8 |
| 42 | UMSAV_FR | Balance Carryforward | CURR | 15 | 2 | UMSAV | WRTV8 |
| 43 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 44 | WAERS_FR | Currency to convert balance FR | CUKY | 5 | 0 | WAERS | WAERS |
| 45 | WORKING_DAYS | Flag | CHAR | 1 | 0 | CIND | CHAR1 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 45 parameters listed in the Parameters Reference Table above.

**AMOUNT** (Credit):

Credit amount. Restricts or displays; also appears in the result. Business meaning: credit amount in the relevant currency.

**AMOUNT_MAX** (Max. Amount):

Maximum amount threshold. Restricts which vendors are included by balance (e.g. only vendors with balance above or below this threshold). Used with SIGN for comparison.

**AUSBK** (Source company code):

Source company code. Restricts or displays; also appears in the result when document-level detail is used.

**BACKDAYS** (Days Back):

Number of days to look back from the current date when building the monitoring window (e.g. for determining inactive period or balance as-of date). When no explicit range is supplied, the EI uses today minus this value as the start of the window.

**BALANCE_FR** (Balance Amount in FR currency):

Balance amount in foreign/reference currency. Restricts which vendors are included by balance in the selected currency; also appears in the result. Business meaning: balance in the currency used for conversion (WAERS_FR).

**BALANCE_NORMAL** (Balance Amount):

Balance amount in local currency (normal). Restricts or displays; also appears in the result. Business meaning: balance in company code local currency.

**BALANCE_NORMAL_FR** (Balance Amount):

Balance amount in foreign/reference currency (normal). Restricts or displays; also appears in the result. Business meaning: normal balance in the conversion currency.

**BALANCE_SPEC** (Balance Amount):

Balance amount in local currency (special). Restricts or displays; also appears in the result. Business meaning: special ledger or other balance in local currency.

**BALANCE_SPEC_FR** (Balance Amount):

Balance amount in foreign/reference currency (special). Restricts or displays; also appears in the result. Business meaning: special balance in the conversion currency.

**BALANCE_TOTAL** (Balance Amount):

Total balance amount in local currency. Restricts or displays; also appears in the result. Business meaning: total balance in company code local currency.

**BALANCE_TOTAL_FR** (Balance Amount):

Total balance amount in foreign/reference currency. Restricts or displays; also appears in the result. Business meaning: total balance in the conversion currency.

**BELNR** (Document Number):

Document number. Restricts or displays; also appears in the result when document-level detail is used.

**BUDAT** (Posting Date):

Posting date. Restricts or displays; also appears in the result when document-level detail is used.

**BUKRS** (Company Code):

Company code. Restricts which vendors and balances are included; also appears in the result.

**BUTXT** (Company Name):

Company code name. Populated from company code master; used for display.

**COUNTER** (Number of active periods):

Number of active periods. Populated for display or filtering when period-based logic is used.

**DMBTR** (Amt.in loc.cur.):

Amount in local currency. Restricts or displays; also appears in the result. Business meaning: line or document amount in company code local currency.

**DMBTR_FR** (Amt.in loc.cur.):

Amount in foreign/reference currency. Restricts or displays; also appears in the result. Business meaning: amount in the conversion currency.

**EKORG** (Purch. Organization):

Purchasing organization. Restricts which vendors are included; also appears in the result.

**EKOTX** (Description):

Purchasing organization description. Populated from master; used for display.

**ERDAT** (Created on):

Creation date. Restricts or displays; also appears in the result when used for vendor or document selection.

**FABKL** (Factory calendar):

Factory calendar key. Used with working-days logic when applicable for date or period filtering.

**FACDATE** (Factory date):

Factory calendar date. Populated when a factory calendar is used; used for display or filtering.

**FDGRV** (Planning group):

Planning group. Restricts which vendors are included; also appears in the result when relevant to vendor master.

**GJAHR** (Fiscal Year):

Fiscal year. Restricts or displays; also appears in the result.

**KONZS** (Group key):

Group key (e.g. vendor hierarchy). Restricts which vendors are included; also appears in the result when relevant.

**KTOKK** (Account group):

Vendor account group. Restricts which vendors are included; also appears in the result.

**LFA1_LOEVM** (Central deletion flag):

Central deletion flag in vendor master (LFA1). When set, the vendor is marked for deletion at central level. Restricts which vendors are included (e.g. include only those with or without deletion flag); also appears in the result.

**LFA1_LOEVM Options:**
- **X**: Set (vendor marked for central deletion).
- ** ** (space or initial): Not set.

**LFA1_SPERM** (Central purchasing block):

Central purchasing block in vendor master (LFA1). When set, purchasing is blocked at central level. Restricts which vendors are included; also appears in the result.

**LFA1_SPERM Options:**
- **X**: Set (central purchasing block).
- ** ** (space or initial): Not set.

**LFA1_SPERR** (Central posting block):

Central posting block in vendor master (LFA1). When set, posting is blocked at central level. Restricts which vendors are included; also appears in the result.

**LFA1_SPERR Options:**
- **X**: Set (central posting block).
- ** ** (space or initial): Not set.

**LFB1_LOEVM** (Delete flag for purchasing organization):

Deletion flag for company code (LFB1). When set, the vendor is marked for deletion for that company code. Restricts which vendors are included; also appears in the result.

**LFB1_LOEVM Options:**
- **X**: Set (company code deletion flag).
- ** ** (space or initial): Not set.

**LFB1_SPERR** (Posting block for company code):

Posting block for company code (LFB1). When set, posting is blocked for that company code. Restricts which vendors are included; also appears in the result.

**LFB1_SPERR Options:**
- **X**: Set (company code posting block).
- ** ** (space or initial): Not set.

**LFM1_LOEVM** (Delete flag for purchasing organization):

Deletion flag for purchasing organization (LFM1). When set, the vendor is marked for deletion for that purchasing organization. Restricts which vendors are included; also appears in the result.

**LFM1_LOEVM Options:**
- **X**: Set (purchasing organization deletion flag).
- ** ** (space or initial): Not set.

**LFM1_SPERM** (Purch. block for purchasing organization):

Purchasing block for purchasing organization (LFM1). When set, purchasing is blocked for that purchasing organization. Restricts which vendors are included; also appears in the result.

**LFM1_SPERM Options:**
- **X**: Set (purchasing organization block).
- ** ** (space or initial): Not set.

**LIFNR** (Vendor):

Vendor number. Restricts which vendors are included; also appears in the result.

**NAME1** (Name):

Vendor name. Populated from vendor master; used for display.

**PARKED_IVNOICE** (Parked Invoice Ind.):

Parked invoice indicator. Restricts or displays when document-level or parked-invoice logic is used.

**PARKED_IVNOICE Options:**
- **X**: Set (parked invoice).
- ** ** (space or initial): Not set.

**PERIOD** (Active Period):

Active period (e.g. fiscal period). Populated for display or filtering when period-based logic is used.

**POSSIBLE_APP** (App Description):

Application description. Populated for display (e.g. "Transactions in FI and PUR are possible") when applicable.

**SIGN** (Operator):

Comparison operator for amount or balance filters. Used with AMOUNT_MAX or similar to define threshold comparison (e.g. greater than, less than).

**UMSAV** (Balance Carryforward):

Balance carryforward. Restricts or displays; also appears in the result. Business meaning: carryforward balance in local currency.

**UMSAV_FR** (Balance Carryforward):

Balance carryforward in foreign/reference currency. Restricts or displays; also appears in the result. Business meaning: carryforward balance in the conversion currency.

**WAERS** (Currency):

Company code local currency. Business meaning: currency in which amounts are stored for the company code.

**WAERS_FR** (Currency to convert balance FR):

Foreign/reference currency used for balance conversion. Business meaning: currency to which balances are converted for reporting or threshold comparison (e.g. USD).

**WORKING_DAYS** (Flag):

Working-days flag. When set with a factory calendar, filters or marks results by working days vs holidays.

**WORKING_DAYS Options:**
- **X** (or C_YES): Working days only (or filter by working day).
- ** ** (or C_NO): Holidays only (or filter by holiday).
- ** ** (initial): No filtering by working day.


### Parameter Relationships

**Lookback and balance parameters**

- **BACKDAYS** defines how many days to look back from today when building the monitoring window (e.g. for determining inactive period or balance as-of date). Use with balance and block/deletion-flag parameters to focus on recent inactive vendors with balances.
- **AMOUNT_MAX** and **SIGN** work together: AMOUNT_MAX defines the balance or amount threshold; SIGN defines the comparison operator (e.g. greater than, less than). Use together to restrict which inactive vendors are included by balance (e.g. balance above a given amount).

**Block and deletion-flag parameters**

- **LFA1_SPERR**, **LFA1_SPERM**, **LFA1_LOEVM** (central level): central posting block, central purchasing block, and central deletion flag in vendor master (LFA1). Use together to restrict to vendors that are inactive at central level (e.g. include only those with posting block set).
- **LFB1_SPERR**, **LFB1_LOEVM** (company code level): posting block and deletion flag for company code (LFB1). Use together with BUKRS to restrict to vendors inactive in specific company codes.
- **LFM1_SPERM**, **LFM1_LOEVM** (purchasing organization level): purchasing block and deletion flag for purchasing organization (LFM1). Use together with EKORG to restrict to vendors inactive in specific purchasing organizations.

**Balance and currency parameters**

- **BALANCE_FR**, **BALANCE_TOTAL**, **BALANCE_NORMAL**, **BALANCE_SPEC** and their _FR variants: balance amounts in local and foreign/reference currency. Use with **WAERS_FR** so that BALANCE_FR and _FR fields are in the currency specified by WAERS_FR (e.g. USD) for threshold comparison and reporting.
- **WAERS_FR**: when set, balances are converted or compared in this currency; used with BALANCE_FR and balance-threshold logic.

**Organizational parameters**

- **BUKRS**, **EKORG**: company code and purchasing organization. Use together to restrict which vendors and balances are included by organizational scope (T024E links company codes and purchasing organizations).


### Default Values

- **BACKDAYS** — Default: `1` (when no explicit date range is supplied; the monitoring window starts one day back from today).
- **WAERS_FR** — Default: `USD` (foreign/reference currency used for balance conversion when not supplied).

### Practical Configuration Examples

**Use Case 1: Inactive vendors with balance above $1K in company code**
```
BUKRS = 1000
AMOUNT_MAX = 1000 (or equivalent threshold)
SIGN = GT (or equivalent: greater than)
WAERS_FR = USD
LFA1_SPERR = X (or LFB1_SPERR = X to focus on company code block)
```
**Purpose:** Focus on vendors with central or company-code posting block in company code 1000 that have a balance above the threshold in USD, for cleanup or release review.

**Use Case 2: By purchasing organization and account group**
```
EKORG = 1000
KTOKK = KRED (or specific account group)
BACKDAYS = 30
LFM1_SPERM = X (or LFM1_LOEVM = X)
```
**Purpose:** Restrict to vendors with purchasing block or deletion flag in purchasing organization 1000 and a given account group, with a 30-day lookback, to prioritize inactive vendors by purchasing scope.

**Use Case 3: Balance in local currency above threshold**
```
BUKRS = 2000
AMOUNT_MAX = 5000
SIGN = GT
LFB1_SPERR = X
```
**Purpose:** Find vendors with company-code posting block in company code 2000 and balance above 5,000 in company code local currency, for payables review.

**Use Case 4: Central deletion flag with balance**
```
LFA1_LOEVM = X
BALANCE_TOTAL = (range or threshold via AMOUNT_MAX/SIGN)
BUKRS = 1000
```
**Purpose:** Identify vendors marked for central deletion that still have a balance in company code 1000, for clearance before deletion.

**Use Case 5: Multiple company codes and planning group**
```
BUKRS = 1000, 2000, 3000
FDGRV = (planning group or range)
LFA1_SPERM = X
BACKDAYS = 7
```
**Purpose:** Restrict to vendors with central purchasing block in the given company codes and planning group, with a seven-day lookback, for procurement and payables coordination.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_06_INACT_VEND | AMOUNT | Total of the Credit Postings for the Month | CURR(15,2) | UMXXH |
| /SKN/S_SW_10_06_INACT_VEND | AUSBK | Source Company Code | CHAR(4) | AUSBK |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_NORMAL | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_NORMAL_FR | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_SPEC | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_SPEC_FR | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_TOTAL | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BALANCE_TOTAL_FR | Balance Amount | CURR(15,2) | ZWRBTR |
| /SKN/S_SW_10_06_INACT_VEND | BELNR | Accounting Document Number | CHAR(10) | BELNR_D |
| /SKN/S_SW_10_06_INACT_VEND | BUDAT | Posting Date in the Document | DATS(8) | BUDAT |
| /SKN/S_SW_10_06_INACT_VEND | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_06_INACT_VEND | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_06_INACT_VEND | COUNTER | Natural Number | INT4(10) | INT4 |
| /SKN/S_SW_10_06_INACT_VEND | DMBTR | Amount in Local Currency | CURR(13,2) | DMBTR |
| /SKN/S_SW_10_06_INACT_VEND | DMBTR_FR | Amount in Local Currency | CURR(13,2) | DMBTR |
| /SKN/S_SW_10_06_INACT_VEND | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_06_INACT_VEND | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_06_INACT_VEND | ERDAT | Date on which the Record Was Created | DATS(8) | ERDAT_RF |
| /SKN/S_SW_10_06_INACT_VEND | FABKL | Factory calendar key | CHAR(2) | FABKL |
| /SKN/S_SW_10_06_INACT_VEND | FACDATE | Factory calendar: Factory date | DEC(5) | FACDATE |
| /SKN/S_SW_10_06_INACT_VEND | FDGRV | Planning group | CHAR(10) | FDGRV |
| /SKN/S_SW_10_06_INACT_VEND | GJAHR | Fiscal Year | NUMC(4) | GJAHR |
| /SKN/S_SW_10_06_INACT_VEND | KONZS | Group key | CHAR(10) | KONZS |
| /SKN/S_SW_10_06_INACT_VEND | KTOKK | Vendor account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_06_INACT_VEND | LFA1_LOEVM | Central Deletion Flag for Master Record | CHAR(1) | LOEVM_X |
| /SKN/S_SW_10_06_INACT_VEND | LFA1_SPERM | Centrally imposed purchasing block | CHAR(1) | SPERM_X |
| /SKN/S_SW_10_06_INACT_VEND | LFA1_SPERR | Central posting block | CHAR(1) | SPERB_X |
| /SKN/S_SW_10_06_INACT_VEND | LFB1_LOEVM | Delete flag for vendor at purchasing level | CHAR(1) | LOEVM_M |
| /SKN/S_SW_10_06_INACT_VEND | LFB1_SPERR | Posting block for company code | CHAR(1) | SPERB_B |
| /SKN/S_SW_10_06_INACT_VEND | LFM1_LOEVM | Delete flag for vendor at purchasing level | CHAR(1) | LOEVM_M |
| /SKN/S_SW_10_06_INACT_VEND | LFM1_SPERM | Purchasing block at purchasing organization level | CHAR(1) | SPERM_M |
| /SKN/S_SW_10_06_INACT_VEND | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_06_INACT_VEND | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_06_INACT_VEND | PARKED_IVNOICE | Parking Invoice Ind. | CHAR(1) | /SKN/E_SW_PARK_INV |
| /SKN/S_SW_10_06_INACT_VEND | PERIOD | Active Period | CHAR(7) | /SKN/E_SW_ACT_PER |
| /SKN/S_SW_10_06_INACT_VEND | POSSIBLE_APP | SW: App Description | CHAR(200) | /SKN/E_SW_APP_DESC |
| /SKN/S_SW_10_06_INACT_VEND | UMSAV | Balance Carried Forward in Local Currency | CURR(15,2) | UMSAV |
| /SKN/S_SW_10_06_INACT_VEND | UMSAV_FR | Balance Carried Forward in Local Currency | CURR(15,2) | UMSAV |
| /SKN/S_SW_10_06_INACT_VEND | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_06_INACT_VEND | WAERS_FR | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_06_INACT_VEND | WORKING_DAYS | Fatory calendar flag | CHAR(1) | CIND |

## ABAP Code

```abap
  FUNCTION /SKN/F_SW_10_06_INACT_VEND.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_06_INACT_VEND OPTIONAL
*"----------------------------------------------------------------------
**** 07/22++
    CONSTANTS: C_POSSIBLE_APP_TXT TYPE /SKN/E_SW_APP_DESC VALUE 'Transactions in FI and PUR are possible'.
    TYPES: BEGIN OF TY_VBKPF,
             AUSBK TYPE VBKPF-AUSBK,
             BELNR TYPE VBKPF-BELNR,
             GJAHR TYPE VBKPF-GJAHR,
             BUDAT TYPE VBKPF-BUDAT,
             BZKEY TYPE VBSEGK-BZKEY,
             BUKRS TYPE VBSEGK-BUKRS,
             SHKZG TYPE VBSEGK-SHKZG,
             DMBTR TYPE VBSEGK-DMBTR,
             LIFNR TYPE VBSEGK-LIFNR,
           END OF TY_VBKPF,
           TT_VBKPF TYPE STANDARD TABLE OF TY_VBKPF.
    TYPES: BEGIN OF TY_DATA_SEL,
             LIFNR TYPE LFA1-LIFNR,
             BUKRS TYPE T001-BUKRS,
           END OF TY_DATA_SEL.
    TYPES: BEGIN OF TY_T024E,
             BUKRS TYPE T001-BUKRS,
             EKORG TYPE T024E-EKORG,
           END OF TY_T024E,
           TT_T024E TYPE STANDARD TABLE OF TY_T024E.
**** 07/22++
    DATA_SINGLE: SW_DEST       RFCDEST,
                 BACKDAYS      INT4,
                 LFA1_SPERR    SPERB_X,
                 LFA1_SPERM    SPERM_X,
                 LFA1_LOEVM    LOEVM_X,
                 LFB1_SPERR    SPERB_B,
                 LFB1_LOEVM    LOEVM_M,
                 LFM1_SPERM    SPERM_M,
                 LFM1_LOEVM    LOEVM_M,
                 WAERS_FR      WAERS.          " Yuri C.++ 11.01.19  Foreign Currency
    DATA_MULTY: LIFNR      LIFNR,
                BUKRS      BUKRS,
                EKORG      EKORG,
                KTOKK      KTOKK,
                KONZS      KONZS,
                FDGRV      FDGRV,
                ERDAT      ERDAT_RF,
                BALANCE_FR DMBTR,              " Yuri C.++ 02.02.19  Balance on foreign currency
                DATUM      SY-DATUM,
                GJAHR      GJAHR,
**** 07/22++
                AUSBK      AUSBK,
                BELNR      BELNR_D,
                BZKEY      BUZEI,
                BUDAT      BUDAT.
**** 07/22++
* Set default param.
    LV_BACKDAYS = 1.
    LV_WAERS_FR = 'USD'.
    SELECT_MULTY: LIFNR,
                  BUKRS,
                  EKORG,
                  KTOKK,
                  KONZS,
                  FDGRV,
                  BALANCE_FR,
                  DATUM,
**** 07/22++
                  AUSBK,
                  BELNR,
                  BZKEY,
                  BUDAT.
**** 07/22++
    SELECT_SINGLE: SW_DEST,
                   BACKDAYS,
                   LFA1_SPERM,
                   LFA1_LOEVM,
                   LFB1_SPERR,
                   LFB1_LOEVM,
                   LFM1_SPERM,
                   LFM1_LOEVM,
                   WAERS_FR.
    CONVERT_MULTY: BUKRS ALPHA,
                   LIFNR ALPHA.
    TYPES: BEGIN OF TY_LFC1,
      LIFNR TYPE LFB1-LIFNR,
      BUKRS TYPE LFB1-BUKRS,
      GJAHR TYPE LFC1-GJAHR,
      UMSAV TYPE LFC1-UMSAV,
      UM01S TYPE LFC1-UM01S,
      UM01H TYPE LFC1-UM01H,
      UM02S TYPE LFC1-UM02S,
      UM02H TYPE LFC1-UM02H,
      UM03S TYPE LFC1-UM03S,
      UM03H TYPE LFC1-UM03H,
      UM04S TYPE LFC1-UM04S,
      UM04H TYPE LFC1-UM04H,
      UM05S TYPE LFC1-UM05S,
      UM05H TYPE LFC1-UM05H,
      UM06S TYPE LFC1-UM06S,
      UM06H TYPE LFC1-UM06H,
      UM07S TYPE LFC1-UM07S,
      UM07H TYPE LFC1-UM07H,
      UM08S TYPE LFC1-UM08S,
      UM08H TYPE LFC1-UM08H,
      UM09S TYPE LFC1-UM09S,
      UM09H TYPE LFC1-UM09H,
      UM10S TYPE LFC1-UM10S,
      UM10H TYPE LFC1-UM10H,
      UM11S TYPE LFC1-UM11S,
      UM11H TYPE LFC1-UM11H,
      UM12S TYPE LFC1-UM12S,
      UM12H TYPE LFC1-UM12H,
      WAERS TYPE T001-WAERS,
    END OF TY_LFC1,
    TT_LFC1 TYPE STANDARD TABLE OF TY_LFC1.
    TYPES: BEGIN OF TY_LFC3,
      BUKRS TYPE LFC3-BUKRS,
      LIFNR TYPE LFC3-LIFNR,
      GJAHR TYPE LFC3-GJAHR,
      SHBKZ TYPE LFC3-SHBKZ,
      SALDV TYPE LFC3-SALDV,
      SOLLL TYPE LFC3-SOLLL,
      HABNL TYPE LFC3-HABNL,
      WAERS TYPE T001-WAERS,
    END OF TY_LFC3,
    TT_LFC3 TYPE STANDARD TABLE OF TY_LFC3.
    DATA: LV_START_DATE   TYPE ERDAT,
          LV_START_YEAR   TYPE GJAHR,
          LV_FIRST_YEAR   TYPE GJAHR,
          LV_START_MONTH  TYPE MONTH,
          LV_CURR_YEAR    TYPE GJAHR,
          LV_CURR_MONTH   TYPE MONTH,
          LV_COMP         TYPE CHAR10,
          LV_ACT          TYPE FLAG,
          LV_TABIX        TYPE I,
          LV_WHILE        TYPE STRING,
          LV_QUERY        TYPE STRING,
          LV_VAL          TYPE STRING,
          LV_WHERE        TYPE RFC_DB_OPT,
          LV_CHAR         TYPE CHAR20,
          LV_VALUE_HIGH   TYPE STRING VALUE '''0.00''',
          LV_VALUE        TYPE STRING VALUE '''0.00''',
          LV_COUNTER_TMP  TYPE I,
          LV_TOTAL_NORMAL TYPE ZWRBTR,
          LV_TOTAL_SPEC   TYPE ZWRBTR,
          LV_TOTAL        TYPE ZWRBTR,
          LV_TOTAL_FR     TYPE ZWRBTR,
          LV_DEBIT        TYPE SOLLL,
          LV_CREDIT       TYPE HABNL,
**** 07/22++
          LV_ROW          TYPE I,
          LV_DELETE       TYPE BOOLE_D,
          LV_BUKRS_EXIST  TYPE BOOLE_D,
          LV_EKORG_EXIST  TYPE BOOLE_D.
**** 07/22++
    DATA: LS_DATA  LIKE LINE OF T_DATA[],
          LS_LFC1  TYPE TY_LFC1,
          LS_LFC3  TYPE TY_LFC3,
**** 07/22++
          LS_VBKPF     TYPE TY_VBKPF,
          LS_T024E     TYPE T024E,
          LS_T024E_EXT TYPE TY_T024E,
          LS_DATA_SEL  TYPE TY_DATA_SEL,
          LS_T001      TYPE T001.
**** 07/22++
    DATA: LT_DATA     LIKE TABLE OF T_DATA,
          LT_DATA_TMP LIKE TABLE OF T_DATA,
          LT_LFC1     TYPE TT_LFC1,
          LT_LFC1_TMP TYPE TT_LFC1,
          LT_QUERY    TYPE TABLE OF RFC_DB_OPT,
          LT_LFC3     TYPE TT_LFC3,
**** 07/22++
          LT_VBKPF       TYPE TT_VBKPF,
          LT_T024E       TYPE STANDARD TABLE OF T024E,
          LT_T024E_FREE  TYPE STANDARD TABLE OF T024E,
          LT_T024E_BUKRS TYPE STANDARD TABLE OF T024E,
          LT_T024E_EXT   TYPE TT_T024E,
          LT_T001        TYPE STANDARD TABLE OF T001.
**** 07/22++
    DATA: BACKDAYS  TYPE I,
          DATE_FROM LIKE SY-DATUM,
          DATE_TO   LIKE SY-DATUM,
          REF_DATE  TYPE D.
    DATA: TIME_DIFF TYPE  INT4 .
    DATA: FLD(60) TYPE C.
    FIELD-SYMBOLS: <FS_VAL>  TYPE ANY,
                   <FS_DATA> LIKE LINE OF T_DATA[].
* if sw_dest is empty then on premise, else on cloud
    IF LV_SW_DEST IS NOT INITIAL.
      CALL FUNCTION '/SKN/FC_SW_10_06_INACT_VEND'
        IMPORTING
          IS_ALERT = IS_ALERT
        TABLES
          T_SELECT = T_SELECT
          T_DATA   = T_DATA.
    ENDIF.
    CHECK LV_SW_DEST IS INITIAL.
*"--- Run Cloud Mode -----
*--- Retrieve data
    CLEAR IS_ALERT .
    REFRESH T_DATA.
*** Yuri C.++ 30.12.18
    CONVERT_MULTY: LIFNR ALPHA.
    IF R_BUKRS IS INITIAL.
**************************** 07/22-- ************************************
*      SELECT lfa1~lifnr t001~bukrs
*        FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr EQ lfb1~lifnr
*                  INNER JOIN t001 ON lfb1~bukrs EQ t001~bukrs
*        INTO  TABLE lt_t001
*        WHERE lfa1~lifnr IN r_lifnr.
*
*      IF lt_t001 IS NOT INITIAL.
*        SORT lt_t001 BY bukrs.
*        DELETE ADJACENT DUPLICATES FROM lt_t001 COMPARING bukrs.
*        LOOP AT lt_t001 INTO ls_t001.
*          rs_bukrs-sign   = 'I'.
*          rs_bukrs-option = 'EQ'.
*          rs_bukrs-low    = ls_t001-bukrs.
*
*          APPEND rs_bukrs TO r_bukrs.
*        ENDLOOP.
*
*        SORT r_bukrs BY low.
*      ENDIF.
**************************** 07/22-- ************************************
**** 07/22++
      SELECT *
        FROM T024E
        INTO TABLE LT_T024E
        WHERE EKORG IN R_EKORG[]
        AND   BUKRS IN R_BUKRS[].
      SELECT *
        FROM T001
        INTO TABLE LT_T001
        WHERE BUKRS IN R_BUKRS[].
      LOOP AT LT_T024E INTO LS_T024E.
        IF LS_T024E-BUKRS IS NOT INITIAL.
          APPEND LS_T024E TO LT_T024E_BUKRS.
          DELETE LT_T001 WHERE BUKRS EQ LS_T024E-BUKRS.
        ELSE.
          APPEND LS_T024E TO LT_T024E_FREE.
        ENDIF.
      ENDLOOP.
      LOOP AT LT_T001 INTO LS_T001.
        CLEAR: LS_T024E_EXT.
        LS_T024E_EXT-BUKRS = LS_T001-BUKRS.
        LOOP AT LT_T024E_FREE INTO LS_T024E.
          LS_T024E_EXT-EKORG = LS_T024E-EKORG.
          APPEND LS_T024E_EXT TO LT_T024E_EXT.
        ENDLOOP.
      ENDLOOP.
      LOOP AT LT_T024E_BUKRS INTO LS_T024E.
        READ TABLE LT_T024E_EXT WITH KEY EKORG = LS_T024E-EKORG
                                         BUKRS = LS_T024E-BUKRS
                                         TRANSPORTING NO FIELDS.
        IF SY-SUBRC IS NOT INITIAL.
          LS_T024E_EXT-EKORG = LS_T024E-EKORG.
          LS_T024E_EXT-BUKRS = LS_T024E-BUKRS.
          APPEND LS_T024E_EXT TO LT_T024E_EXT.
          CLEAR: LS_T024E_EXT.
        ENDIF.
      ENDLOOP.
**** 07/22++
    ENDIF.
*** Yuri C.++ 30.12.18
    SELECT LFB1~LIFNR LFB1~BUKRS LFB1~FDGRV LFB1~SPERR AS LFB1_SPERR LFB1~LOEVM AS LFB1_LOEVM
           LFM1~EKORG LFM1~SPERM AS LFM1_SPERM LFM1~LOEVM AS LFM1_LOEVM
           LFA1~NAME1 LFA1~SPERR AS LFA1_SPERR LFA1~SPERM AS LFA1_SPERM
           LFA1~LOEVM AS LFA1_LOEVM LFA1~KTOKK LFA1~KONZS
           T001~BUTXT
      FROM LFB1 INNER JOIN LFA1      ON  LFB1~LIFNR EQ LFA1~LIFNR
                INNER JOIN T001      ON  LFB1~BUKRS EQ T001~BUKRS
                LEFT OUTER JOIN LFM1 ON  LFA1~LIFNR EQ LFM1~LIFNR     " 07/22--
                                     AND LFM1~SPERM EQ LV_LFM1_SPERM
      INTO CORRESPONDING FIELDS OF TABLE LT_DATA
      WHERE LFB1~LIFNR  IN R_LIFNR
      AND   LFB1~SPERR  EQ LV_LFB1_SPERR
      AND   LFB1~LOEVM  EQ LV_LFB1_LOEVM
      AND   LFA1~SPERR  EQ LV_LFA1_SPERR
      AND   LFA1~SPERM  EQ LV_LFA1_SPERM
      AND   LFA1~LOEVM  EQ LV_LFA1_LOEVM.
**** 07/22++
*      AND   lfm1~ekorg  EQ t_t024e_ext-ekorg
*      AND   lfm1~sperm  EQ lv_lfm1_sperm.
**** 07/22++
    IF LT_DATA IS NOT INITIAL.
      SORT LT_DATA[] BY LIFNR BUKRS.
      DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING LIFNR BUKRS.
    ENDIF.
    CHECK LT_DATA IS NOT INITIAL.
**** 07/22++
*************************************************** Restriction data by Alert parameters ****************************************
    LT_DATA_TMP = LT_DATA.
    SORT LT_T024E_EXT BY BUKRS EKORG.
    LV_ROW = 0.
    LOOP AT LT_DATA_TMP INTO LS_DATA.
      LV_TABIX = SY-TABIX.
      LV_ROW = LV_ROW + 1.
      IF LV_ROW EQ 1.
        LS_DATA_SEL-BUKRS = LS_DATA-BUKRS.
        LS_DATA_SEL-LIFNR = LS_DATA-LIFNR.
      ELSE.
        IF LS_DATA_SEL-BUKRS EQ LS_DATA-BUKRS AND
           LS_DATA_SEL-LIFNR EQ LS_DATA-LIFNR.
          READ TABLE LT_T024E_EXT WITH KEY BUKRS = LS_DATA-BUKRS
                                           EKORG = LS_DATA-EKORG
                                           BINARY SEARCH
                                           TRANSPORTING NO FIELDS.
          IF SY-SUBRC IS NOT INITIAL.
            LV_DELETE = 'X'.
          ELSEIF LS_DATA-LFM1_SPERM NE LV_LFM1_SPERM OR
                 LS_DATA-LFM1_LOEVM NE LV_LFM1_LOEVM.
            LV_DELETE = 'X'.
          ELSEIF SY-SUBRC IS INITIAL AND LV_ROW GE 2.
            LV_DELETE = 'X'.
          ENDIF.
          IF LV_DELETE EQ 'X'.
            DELETE LT_DATA_TMP INDEX LV_TABIX.
            IF SY-SUBRC IS INITIAL.
              READ TABLE LT_DATA_TMP ASSIGNING <FS_DATA> INDEX LV_TABIX - 1.
              IF SY-SUBRC IS INITIAL AND <FS_DATA> IS ASSIGNED.
                <FS_DATA>-LFM1_SPERM = LV_LFM1_SPERM.
                <FS_DATA>-LFM1_LOEVM = LV_LFM1_LOEVM.
              ENDIF.
            ENDIF.
            CLEAR: LV_DELETE.
          ENDIF.
        ELSE.
          LV_ROW = 1.
          LS_DATA_SEL-BUKRS = LS_DATA-BUKRS.
          LS_DATA_SEL-LIFNR = LS_DATA-LIFNR.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF LT_DATA_TMP IS NOT INITIAL.
      LT_DATA = LT_DATA_TMP.
    ENDIF.
*************************************************** Restriction data by Alert parameters ****************************************
    IF R_BUDAT[] IS INITIAL.
      RS_BUDAT-SIGN   = 'I' .
      RS_BUDAT-OPTION = 'BT'.
      DATE_FROM       = SY-DATUM - LV_BACKDAYS.
      RS_BUDAT-LOW    = DATE_FROM.
      RS_BUDAT-HIGH   = SY-DATUM.
      APPEND RS_BUDAT TO R_BUDAT.
    ENDIF.
    SELECT VBKPF~AUSBK VBKPF~BELNR VBKPF~GJAHR VBKPF~BUDAT
           VBSEGK~BZKEY VBSEGK~BUKRS VBSEGK~SHKZG VBSEGK~DMBTR VBSEGK~LIFNR
      FROM VBKPF INNER JOIN VBSEGK ON  VBKPF~AUSBK EQ VBSEGK~AUSBK
                                   AND VBKPF~BELNR EQ VBSEGK~BELNR
                                   AND VBKPF~GJAHR EQ VBSEGK~GJAHR
                                   AND VBKPF~BUKRS EQ VBSEGK~BUKRS
      INTO TABLE LT_VBKPF
      FOR ALL ENTRIES IN LT_DATA
      WHERE VBKPF~BUKRS  EQ LT_DATA-BUKRS
      AND   VBSEGK~LIFNR EQ LT_DATA-LIFNR
      AND   VBKPF~AUSBK  IN R_AUSBK[]
      AND   VBKPF~BELNR  IN R_BELNR[]
      AND   VBKPF~GJAHR  IN R_GJAHR[]
      AND   VBKPF~BUDAT  IN R_BUDAT[]
      AND   VBSEGK~BZKEY IN R_BZKEY[]
      AND   VBSEGK~BUKRS IN R_BUKRS[]
      AND   VBSEGK~LIFNR IN R_LIFNR[].
    IF SY-SUBRC IS INITIAL.
      SORT LT_VBKPF BY BUKRS LIFNR ASCENDING BUDAT DESCENDING.
      DELETE ADJACENT DUPLICATES FROM LT_VBKPF COMPARING BUKRS LIFNR.
    ENDIF.
**** 07/22++
    LV_START_DATE  = SY-DATUM - LV_BACKDAYS.
    LV_START_YEAR  = LV_START_DATE(4).
    LV_START_MONTH = LV_START_DATE+4(2).
    LV_CURR_YEAR  = SY-DATUM(4).
    LV_CURR_MONTH = SY-DATUM+4(2).
    LV_START_DATE  = SY-DATUM - LV_BACKDAYS.
    LV_START_YEAR  = LV_START_DATE(4).
    LV_FIRST_YEAR  = LV_START_YEAR.
    LV_START_MONTH = LV_START_DATE+4(2).
    RS_GJAHR-OPTION = 'EQ'.
    RS_GJAHR-SIGN   = 'I'.
    RS_GJAHR-LOW    = LV_START_YEAR.
    APPEND RS_GJAHR TO R_GJAHR.
*** Yuri C.++ 06.01.19
*    IF rs_amount_max-low > 0.
*      WRITE rs_amount_max-low TO lv_char NO-GROUPING LEFT-JUSTIFIED.
*      lv_value = lv_char.
*    ENDIF.
*    IF rs_amount_max-high > 0.
*      CLEAR lv_char.
*      WRITE rs_amount_max-high TO lv_char NO-GROUPING LEFT-JUSTIFIED.
*      lv_value_high = lv_char.
*    ENDIF.
*** Yuri C.++ 06.01.19
    LV_WHILE = 12.
    WHILE LV_START_YEAR <= LV_CURR_YEAR.
*      CHECK lt_data IS NOT INITIAL.
      IF LT_DATA IS INITIAL.
        EXIT.
      ENDIF.
      CLEAR: LV_WHERE.
      CONCATENATE 'LFC1~LIFNR' 'EQ' 'lt_data-lifnr'     INTO LV_WHERE
      SEPARATED BY SPACE.
      APPEND LV_WHERE TO LT_QUERY.
      CLEAR LV_WHERE.
      CONCATENATE 'AND LFC1~BUKRS' 'EQ' 'lt_data-bukrs' INTO LV_WHERE
      SEPARATED BY SPACE.
      APPEND LV_WHERE TO LT_QUERY.
      IF LV_START_YEAR = LV_CURR_YEAR.
        LV_WHILE = LV_CURR_MONTH.
      ENDIF.
      CLEAR: LV_WHERE, LV_TABIX.
      WHILE LV_START_MONTH <= LV_WHILE .
        LV_TABIX = LV_TABIX + 1.
        CLEAR: LV_COMP.
*
        IF LV_TABIX = 1.
          CONCATENATE LV_WHERE 'AND' INTO LV_WHERE SEPARATED BY SPACE.
          CONCATENATE LV_WHERE 'LFC1~GJAHR' 'EQ' ''''LV_START_YEAR'''' INTO LV_WHERE
            SEPARATED BY SPACE.
          CONDENSE LV_WHERE.
          APPEND LV_WHERE TO LT_QUERY.
          CLEAR LV_WHERE.
          CONCATENATE 'AND' '(' INTO LV_WHERE SEPARATED BY SPACE.
        ELSE.
          CONCATENATE LV_WHERE 'OR' INTO LV_WHERE SEPARATED BY SPACE.
        ENDIF.
        CONCATENATE LV_WHERE '(' INTO LV_WHERE SEPARATED BY SPACE.
        CONCATENATE 'LFC1~' 'UM' LV_START_MONTH 'S'  INTO LV_COMP.
        CONCATENATE LV_WHERE LV_COMP 'NE' LV_VAL INTO LV_WHERE
          SEPARATED BY SPACE.
        CONCATENATE LV_WHERE 'OR' INTO LV_WHERE SEPARATED BY SPACE.
        CLEAR LV_COMP.
        CONCATENATE 'LFC1~' 'UM' LV_START_MONTH 'H' INTO LV_COMP.
        CONCATENATE LV_WHERE LV_COMP 'NE' LV_VAL INTO LV_WHERE
          SEPARATED BY SPACE.
        CONCATENATE LV_WHERE ')' INTO LV_WHERE SEPARATED BY SPACE.
        IF LV_WHERE IS NOT INITIAL.
          APPEND LV_WHERE TO LT_QUERY.
          CLEAR LV_WHERE.
        ENDIF.
        LV_START_MONTH = LV_START_MONTH + 1.
      ENDWHILE.
      CHECK LT_DATA IS NOT INITIAL.
***********************************
      IF LT_QUERY IS NOT INITIAL.
        LV_WHERE = ')'.
        APPEND LV_WHERE TO LT_QUERY.
        IF LT_DATA IS NOT INITIAL.
          SELECT LFC1~LIFNR LFC1~BUKRS LFC1~GJAHR
                 LFC1~UMSAV
                 LFC1~UM01S LFC1~UM01H LFC1~UM02S LFC1~UM02H LFC1~UM03S LFC1~UM03H
                 LFC1~UM04S LFC1~UM04H LFC1~UM05S LFC1~UM05H LFC1~UM06S LFC1~UM06H
                 LFC1~UM07S LFC1~UM07H LFC1~UM08S LFC1~UM08H LFC1~UM09S LFC1~UM09H
                 LFC1~UM10S LFC1~UM10H LFC1~UM11S LFC1~UM11H LFC1~UM12S LFC1~UM12H
            FROM LFC1 LEFT OUTER JOIN T001 ON LFC1~BUKRS EQ T001~BUKRS
            INTO CORRESPONDING FIELDS OF TABLE LT_LFC1
            FOR ALL ENTRIES IN LT_DATA[]
            WHERE (LT_QUERY).
        ENDIF.
        IF SY-SUBRC = 0.
          SORT LT_LFC1 BY BUKRS LIFNR GJAHR.
          LOOP AT LT_LFC1 INTO LS_LFC1.
            DELETE LT_DATA WHERE LIFNR EQ LS_LFC1-LIFNR
                           AND   BUKRS EQ LS_LFC1-BUKRS.
          ENDLOOP.
          CHECK LT_DATA IS NOT INITIAL.
        ENDIF.
      ENDIF.
      LV_START_YEAR  = LV_START_YEAR + 1.
      LV_START_MONTH = 1.
      CLEAR: LT_QUERY.
    ENDWHILE.
    IF LT_DATA[] IS NOT INITIAL.
      SORT LT_DATA BY LIFNR BUKRS.
*** Leon request to change BSIK select by LFC3
*** 04.02.19--
*        SELECT bsik~bukrs bsik~lifnr bsik~belnr bsik~shkzg bsik~dmbtr
*               t001~waers
*          FROM bsik LEFT OUTER JOIN t001 ON bsik~bukrs EQ t001~bukrs
*          INTO TABLE lt_bsik
*          FOR ALL ENTRIES IN lt_data
*          WHERE bsik~lifnr EQ lt_data-lifnr
*          AND   bsik~bukrs EQ lt_data-bukrs.
*** 04.02.19--
*** Begin 04.02.19++
* Select all vendors related to T_DATA and for current year
      CLEAR: LT_LFC1.
      LV_CURR_YEAR = SY-DATUM(4).
      SELECT LFC1~LIFNR LFC1~BUKRS LFC1~GJAHR LFC1~UMSAV
             LFC1~UM01S LFC1~UM01H LFC1~UM02S LFC1~UM02H LFC1~UM03S LFC1~UM03H
             LFC1~UM04S LFC1~UM04H LFC1~UM05S LFC1~UM05H LFC1~UM06S LFC1~UM06H
             LFC1~UM07S LFC1~UM07H LFC1~UM08S LFC1~UM08H LFC1~UM09S LFC1~UM09H
             LFC1~UM10S LFC1~UM10H LFC1~UM11S LFC1~UM11H LFC1~UM12S LFC1~UM12H
             T001~WAERS
        FROM LFC1 LEFT OUTER JOIN T001 ON LFC1~BUKRS EQ T001~BUKRS
        INTO CORRESPONDING FIELDS OF TABLE LT_LFC1
        FOR ALL ENTRIES IN LT_DATA
        WHERE LFC1~LIFNR EQ LT_DATA-LIFNR
        AND   LFC1~BUKRS EQ LT_DATA-BUKRS
        AND   LFC1~GJAHR EQ LV_CURR_YEAR.
      SELECT LFC3~LIFNR LFC3~BUKRS LFC3~GJAHR LFC3~SHBKZ LFC3~SALDV LFC3~SOLLL LFC3~HABNL
             T001~WAERS
        FROM LFC3 LEFT OUTER JOIN T001 ON LFC3~BUKRS EQ T001~BUKRS
        INTO CORRESPONDING FIELDS OF TABLE LT_LFC3
        FOR ALL ENTRIES IN LT_DATA
        WHERE LFC3~LIFNR EQ LT_DATA-LIFNR
        AND   LFC3~BUKRS EQ LT_DATA-BUKRS
        AND   LFC3~GJAHR EQ LV_CURR_YEAR.
      IF SY-SUBRC = 0.
        SORT LT_LFC3 BY LIFNR BUKRS GJAHR.
      ENDIF.
*** End 04.02.19++
    ENDIF.
    LV_CURR_YEAR = SY-DATUM(4).
    SORT LT_LFC1 BY LIFNR BUKRS GJAHR.
* Calculate debit, credit and balance of current year
    LOOP AT LT_DATA ASSIGNING <FS_DATA>.
      CLEAR: LV_TOTAL_NORMAL, LV_TOTAL_SPEC, LS_LFC1, LS_LFC3, LV_TOTAL.
* LFC3
* Calculate balance of spec. G/L
      LOOP AT LT_LFC3 INTO LS_LFC3 WHERE LIFNR EQ <FS_DATA>-LIFNR
                                   AND   BUKRS EQ <FS_DATA>-BUKRS
                                   AND   GJAHR EQ LV_CURR_YEAR.
        IF <FS_DATA>-WAERS IS INITIAL.
          <FS_DATA>-WAERS = LS_LFC3-WAERS.
        ENDIF.
        LV_TOTAL_SPEC = LS_LFC3-SALDV + LS_LFC3-SOLLL - LS_LFC3-HABNL.
      ENDLOOP.
      IF LV_WAERS_FR IS NOT INITIAL AND LS_LFC3 IS NOT INITIAL AND
         LV_WAERS_FR NE LS_LFC3-WAERS AND LV_TOTAL_SPEC IS NOT INITIAL.
        CLEAR: LV_TOTAL_FR.
        <FS_DATA>-WAERS_FR = LV_WAERS_FR.
        CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
          EXPORTING
            DATE             = SY-DATUM
            FOREIGN_CURRENCY = LV_WAERS_FR   " 'USD'
*           local_amount     = ls_bsik-dmbtr
*           local_currency   = ls_bsik-waers
            LOCAL_AMOUNT     = LV_TOTAL_SPEC
            LOCAL_CURRENCY   = LS_LFC3-WAERS
          IMPORTING
            FOREIGN_AMOUNT   = LV_TOTAL_FR
          EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND  = 4
            DERIVED_2_TIMES  = 5
            OTHERS           = 6.
      ELSE.
        LV_TOTAL_FR = LV_TOTAL_SPEC.
      ENDIF.
      <FS_DATA>-BALANCE_SPEC    = <FS_DATA>-BALANCE_SPEC    + LV_TOTAL_SPEC.
      <FS_DATA>-BALANCE_SPEC_FR = <FS_DATA>-BALANCE_SPEC_FR + LV_TOTAL_FR.
* LFC1
      LOOP AT LT_LFC1 INTO LS_LFC1 WHERE LIFNR EQ <FS_DATA>-LIFNR
                                   AND   BUKRS EQ <FS_DATA>-BUKRS
                                   AND   GJAHR EQ LV_CURR_YEAR.
        CLEAR: LV_CREDIT, LV_DEBIT.
        IF <FS_DATA>-WAERS IS INITIAL.
          <FS_DATA>-WAERS = LS_LFC1-WAERS.
        ENDIF.
        LV_CURR_MONTH = 1.
        LV_WHILE      = 1.
        WHILE LV_WHILE <= 12 .
          CONCATENATE 'UM' LV_CURR_MONTH 'H' INTO LV_COMP.
          ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_LFC1 TO <FS_VAL>.
          IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
            LV_CREDIT = LV_CREDIT + <FS_VAL>.
            UNASSIGN <FS_VAL>.
          ENDIF.
          CLEAR: LV_COMP.
          CONCATENATE 'UM' LV_CURR_MONTH 'S' INTO LV_COMP.
          ASSIGN COMPONENT LV_COMP OF STRUCTURE LS_LFC1 TO <FS_VAL>.
          IF SY-SUBRC = 0 AND <FS_VAL> IS ASSIGNED.
            LV_DEBIT = LV_DEBIT + <FS_VAL>.
            UNASSIGN <FS_VAL>.
          ENDIF.
          ADD 1 TO LV_CURR_MONTH.
          ADD 1 TO LV_WHILE.
        ENDWHILE.
*        lv_total_normal = lv_total_normal + ( ls_lfc1-umsav + lv_credit - lv_debit ).
        LV_TOTAL_NORMAL = LV_TOTAL_NORMAL + ( LS_LFC1-UMSAV + LV_DEBIT - LV_CREDIT ).
      ENDLOOP.
******** 22.02.19
      IF LV_WAERS_FR IS NOT INITIAL AND LS_LFC1 IS NOT INITIAL AND
         LV_WAERS_FR NE LS_LFC1-WAERS AND LV_TOTAL_NORMAL IS NOT INITIAL.
        CLEAR: LV_TOTAL_FR.
        <FS_DATA>-WAERS_FR = LV_WAERS_FR.
        CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
          EXPORTING
            DATE             = SY-DATUM
            FOREIGN_CURRENCY = LV_WAERS_FR   " 'USD'
            LOCAL_AMOUNT     = LV_TOTAL_NORMAL
            LOCAL_CURRENCY   = LS_LFC1-WAERS
          IMPORTING
            FOREIGN_AMOUNT   = LV_TOTAL_FR
          EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND  = 4
            DERIVED_2_TIMES  = 5
            OTHERS           = 6.
      ELSE.
        LV_TOTAL_FR = LV_TOTAL_NORMAL.
      ENDIF.
      <FS_DATA>-BALANCE_NORMAL    = <FS_DATA>-BALANCE_NORMAL    + LV_TOTAL_NORMAL. " Balance total normal   of LFC1
      <FS_DATA>-BALANCE_NORMAL_FR = <FS_DATA>-BALANCE_NORMAL_FR + LV_TOTAL_FR.
* Total balance
      LV_TOTAL = <FS_DATA>-BALANCE_NORMAL + <FS_DATA>-BALANCE_SPEC.
      IF LV_WAERS_FR IS NOT INITIAL AND <FS_DATA>-WAERS IS NOT INITIAL AND
         LV_WAERS_FR NE LS_LFC1-WAERS AND LV_TOTAL IS NOT INITIAL.
        CLEAR: LV_TOTAL_FR.
        <FS_DATA>-WAERS_FR = LV_WAERS_FR.
        CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
          EXPORTING
            DATE             = SY-DATUM
            FOREIGN_CURRENCY = LV_WAERS_FR   " 'USD'
            LOCAL_AMOUNT     = LV_TOTAL
            LOCAL_CURRENCY   = <FS_DATA>-WAERS
          IMPORTING
            FOREIGN_AMOUNT   = LV_TOTAL_FR
          EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND  = 4
            DERIVED_2_TIMES  = 5
            OTHERS           = 6.
      ELSE.
        IF <FS_DATA>-WAERS_FR IS INITIAL.
          <FS_DATA>-WAERS_FR = LV_WAERS_FR.
        ENDIF.
        LV_TOTAL_FR = LV_TOTAL.
      ENDIF.
******** 22.02.19
      <FS_DATA>-BALANCE_TOTAL = LV_TOTAL.
**** 07/22++
      IF <FS_DATA>-EKORG IS NOT INITIAL.
        <FS_DATA>-POSSIBLE_APP = C_POSSIBLE_APP_TXT.
      ENDIF.
      READ TABLE LT_VBKPF INTO LS_VBKPF WITH KEY BUKRS = <FS_DATA>-BUKRS
                                                 LIFNR = <FS_DATA>-LIFNR.
      IF SY-SUBRC IS INITIAL.
        <FS_DATA>-PARKED_IVNOICE = 'X'.
      ENDIF.
**** 07/22++
      IF LV_TOTAL_FR IN R_BALANCE_FR[].
        <FS_DATA>-BALANCE_TOTAL_FR = LV_TOTAL_FR.
        APPEND <FS_DATA> TO T_DATA.
      ENDIF.
    ENDLOOP.
    READ TABLE T_DATA INDEX 1.
    CHECK NOT SY-TFILL  IS INITIAL .
    IS_ALERT = 'X' .
  ENDFUNCTION.
```