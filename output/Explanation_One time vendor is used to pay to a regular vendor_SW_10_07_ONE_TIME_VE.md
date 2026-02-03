# Exception Indicator: One time vendor is used to pay regular vendor - SW_10_07_ONE_TIME_VE

## General Overview

This Exception Indicator (EI) monitors payment run data in Financial Accounting (FI) and Materials Management (MM) to identify cases where a one-time vendor is used as the payee on a payment, enabling control over payments that involve one-time accounts. It provides visibility into payment runs that reference one-time vendors within configurable date windows and organizational dimensions.

This EI serves as an essential control for treasury and vendor master oversight by:
- Enabling detection of payments where the payee is a one-time vendor, supporting review of payment security and vendor master usage
- Supporting identification of payment runs and company codes that use one-time vendors for audit and segregation-of-duties checks
- Providing visibility into payee, bank-account, and payment-document patterns for reconciliation and exception escalation
- Enabling analysis of one-time vendor usage by company code, bank details, and run date for policy compliance
- Supporting accountability by paying company code and payment recipient for payment and vendor master governance

Monitoring one-time vendor usage in payments helps organizations manage payment risk, enforce vendor master policies, and prioritize review during close and audit. The EI is valuable for treasury exception management and vendor-master controls.

The EI uses payment run header data (REGUH), vendor master (LFA1) for one-time indicator, and bank details (LFBK), within a configurable date window.


## Problem Description

Failure to monitor payments where a one-time vendor is used as the payee creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Unidentified payments to one-time vendors can increase fraud and mispayment risk and distort payables reconciliation
- Payment runs that use one-time accounts without visibility may indicate inappropriate use of one-time vendor master for regular payments
- Concentrations of one-time vendor payments in specific company codes or periods can signal control weaknesses
- Lack of monitoring may delay month-end close or audit when exceptions are discovered late
- Payment and vendor master inconsistencies can affect financial statement assertions and audit evidence

**Operational and Control Risks**
- Payments to one-time vendors without oversight may bypass normal vendor approval and bank-account verification
- One-time vendor usage in payment runs could indicate master data errors, duplicate vendors, or misuse of one-time accounts for recurring payees
- Bank-account and payee combinations that are not routinely monitored may allow erroneous or fraudulent payments
- Central deletion or blocking flags on vendor master may go unconsidered when one-time vendors appear in payment runs
- Lack of visibility by company code, run date, and bank details limits operational correction and process improvement

**Management Visibility and Decision-Making Risks**
- Absence of one-time vendor payment monitoring delays awareness of payment and vendor master risk
- Unidentified use of one-time vendors in payment runs limits ability to enforce vendor master and payment policies
- Exceptions may require audit or compliance review but go unnoticed without targeted monitoring
- Inability to attribute one-time vendor payments to company code, run, or bank constrains accountability and corrective action

## Suggested Resolution

**Immediate Response**
- Review the payment run lines flagged by the EI to confirm payee is a one-time vendor and that the payment and bank details are correct
- Verify payment documents and vendor master (e.g. XK01, FBL1N, REGUH display) to confirm legitimacy and authorization
- Check company code, run date, and bank-account details to determine whether one-time vendor usage is appropriate or erroneous
- Identify responsible company code and payment recipient for escalation or correction

**System Assessment**
- Analyze the date window and reference date used for selection to ensure the monitoring period is appropriate
- Compare one-time vendor payment volume and count to prior periods and to total payment runs to assess materiality and trend
- Examine distribution by company code, bank country, and payee to find concentration or systematic issues
- Assess vendor account group, deletion flag, and alternative payee patterns to distinguish policy-compliant vs. inappropriate usage
- Validate payment run and vendor master data sources to ensure correct join and one-time indicator logic

**Corrective Actions**
- Correct erroneous vendor or bank master data (e.g. change payee to regular vendor, update LFBK/LFA1) where one-time vendor was used in error
- For legitimate one-time vendor payments, document business justification and ensure proper approval and audit trail
- Enforce vendor master and payment policies: restrict one-time vendor usage where inappropriate and update training or process
- Update monitoring parameters (e.g. lookback, company code scope) to align with treasury and close calendar
- Establish recurring EI runs and route results to treasury and accounting for continuous oversight and exception resolution


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 2 | BANKL | Bank Key | CHAR | 15 | 0 | BANKK | BANKK |
| 3 | BANKN | Bank Account | CHAR | 18 | 0 | BANKN | BANKN |
| 4 | BANKS | Bank Country | CHAR | 3 | 0 | BANKS | LAND1 |
| 5 | BUKRS | Company Code | CHAR | 4 | 0 | BUKRS | BUKRS |
| 6 | BUTXT | Company Name | CHAR | 25 | 0 | BUTXT | TEXT25 |
| 7 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 8 | DURATION_UNIT | Duration Unit | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 9 | EKORG | Purch. Organization | CHAR | 4 | 0 | EKORG | EKORG |
| 10 | EKOTX | Description | CHAR | 20 | 0 | EKOTX | TEXT20 |
| 11 | EMPFG | Payment recipient | CHAR | 16 | 0 | EMPFG | EMPFG |
| 12 | ERDAT | Created On | DATS | 8 | 0 | ERDAT | DATUM |
| 13 | KOBIS | Eff.to | DATS | 8 | 0 | KOBIS | DATUM |
| 14 | KONZS | Group key | CHAR | 10 | 0 | KONZS | KONZS |
| 15 | KOVON | Valid from | DATS | 8 | 0 | KOVON | DATUM |
| 16 | KTOKK | Account group | CHAR | 4 | 0 | KTOKK | KTOKK |
| 17 | KUNNR | Customer | CHAR | 10 | 0 | KUNNR | KUNNR |
| 18 | LAND1 | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 19 | LAND1_EMPFG | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 20 | LAND1_LFBK | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 21 | LAND1_PAYEE | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 22 | LANDX | Name | CHAR | 15 | 0 | LANDX | TEXT15 |
| 23 | LANDX_EMPFG | Name | CHAR | 15 | 0 | LANDX | TEXT15 |
| 24 | LANDX_LFBK | Name | CHAR | 15 | 0 | LANDX | TEXT15 |
| 25 | LANDX_PAYEE | Name | CHAR | 15 | 0 | LANDX | TEXT15 |
| 26 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 27 | LAUFD | Run Date | DATS | 8 | 0 | LAUFD | DATUM |
| 28 | LAUFI | Identification | CHAR | 6 | 0 | LAUFI | LAUFI |
| 29 | LIFNR | Vendor | CHAR | 10 | 0 | LIFNR | LIFNR |
| 30 | LIFNR_EMPFG | Supplier | CHAR | 10 | 0 | LIFNR | LIFNR |
| 31 | LIFNR_LFBK | Supplier | CHAR | 10 | 0 | LIFNR | LIFNR |
| 32 | LNRZA | Alternative payee | CHAR | 10 | 0 | LNRZA | LIFNR |
| 33 | LOEVM | Central deletion flag | CHAR | 1 | 0 | LOEVM_X | XFELD |
| 34 | NAME1 | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 35 | NAME1_EMPFG | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 36 | NAME1_LFBK | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 37 | NAME1_PAYEE | Name | CHAR | 35 | 0 | NAME1_GP | NAME |
| 38 | PAYEE | Supplier | CHAR | 10 | 0 | LIFNR | LIFNR |
| 39 | RWBTR | Amount paid | CURR | 13 | 2 | RWBTR | WRTV7 |
| 40 | SPERM | Central purchasing block | CHAR | 1 | 0 | SPERM_X | XFELD |
| 41 | SPERR | Central posting block | CHAR | 1 | 0 | SPERB_X | XFELD |
| 42 | SPRAS | Language Key | LANG | 1 | 0 | SPRAS | SPRAS |
| 43 | VBLNR | Payment document no. | CHAR | 10 | 0 | VBLNR | BELNR |
| 44 | WAERS | Currency | CUKY | 5 | 0 | WAERS | WAERS |
| 45 | XCPDK | One-time account | CHAR | 1 | 0 | XCPDK | XFELD |
| 46 | XVORL | Indicator: Only Proposal Run? | CHAR | 1 | 0 | XVORL | XFELD |
| 47 | ZBNKL | Bank number | CHAR | 15 | 0 | DZBNKL | BANKL |
| 48 | ZBNKN | Payee's bank acct number | CHAR | 18 | 0 | DZBNKN | BANKN |
| 49 | ZBNKS | Country Key | CHAR | 3 | 0 | DZBNKS | LAND1 |
| 50 | ZBUKR | Paying Company Code | CHAR | 4 | 0 | DZBUKR | BUKRS |
| 51 | ZNME1 | Payee name | CHAR | 35 | 0 | DZNME1 | TEXT35 |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 51 parameters listed in the Parameters Reference Table above.

**BACKDAYS** (Backdays):

Controls how many days to look back from the reference date when no date range is supplied. The EI builds the monitoring window from the reference date minus this number of days. Larger values widen the period for detecting one-time vendor payments; smaller values focus on recent payment runs.

**BANKL** (Bank Key):

Restricts which payment run lines are included by bank key (e.g. payee bank or bank master). Only lines whose bank key matches the supplied values are selected. Use together with BANKS and BANKN for bank-level scoping.

**BANKN** (Bank Account):

Restricts selection by bank account number. Only lines whose bank account matches the supplied values are selected. Supports reconciliation and exception handling by bank account.

**BANKS** (Bank Country):

Restricts selection by bank country key. Only lines whose bank country matches the supplied values are selected. Use for country-level analysis of one-time vendor payments.

**BUKRS** (Company Code):

Restricts selection to specific company codes (paying company code). Only payment run lines belonging to the given company codes are included. Use for organizational scoping of one-time vendor payment monitoring.

**BUTXT** (Company Name):

Company code name. Populated from the paying company code for display or filtering in the result set.

**DURATION** (Duration In Time Units):

Numeric duration used to filter or display how long ago the payment run date falls relative to a reference date. The EI calculates duration from the reference date (e.g. run date) to the run date and can filter by this value. Use together with DURATION_UNIT.

**DURATION_UNIT** (Duration Unit):

Unit in which duration is expressed (e.g. days). Used for duration calculation and for filtering by DURATION so that age of the payment run is interpreted consistently.

**DURATION_UNIT Options:**
- **D**: Days.

**EKORG** (Purch. Organization):

Purchasing organization. When used as a criterion, restricts or labels results by purchasing organization. Populated where applicable for display or filtering in the result set.

**EKOTX** (Description):

Purchasing organization description. Populated from the purchasing organization for display in the result set.

**EMPFG** (Payment recipient):

Restricts selection by payment recipient (payee code). Only lines whose payment recipient matches the supplied values are included. Supports analysis by payee type or code.

**ERDAT** (Created On):

Date or range for record creation. When used as a criterion, restricts which lines are included by creation date. Used together with the date window when applicable.

**KOBIS** (Eff.to):

Bank details valid-to date. When used as a criterion, restricts which lines are included by bank-detail validity end date. Supports filtering by period when bank details are valid.

**KONZS** (Group key):

Group key. When supplied, restricts or labels results by group key for display or filtering in the result set.

**KOVON** (Valid from):

Bank details valid-from date. When used as a criterion, restricts which lines are included by bank-detail validity start date. Supports filtering by period when bank details are valid.

**KTOKK** (Account group):

Restricts selection to specific vendor account groups. Only payees whose account group is in the supplied list are included. Supports control by vendor type.

**KUNNR** (Customer):

Restricts selection by customer number where the payment run references a customer. Only lines whose customer matches the supplied values are included when applicable.

**LAND1** (Country Key):

Restricts selection by country key. Only lines whose country (e.g. payee or bank country) matches the supplied values are included. Use for country-level scoping.

**LAND1_EMPFG** (Country Key):

Country key for payment recipient. When used as a criterion, restricts or labels results by recipient country for display or filtering in the result set.

**LAND1_LFBK** (Country Key):

Country key for vendor bank (LFBK). When used as a criterion, restricts or labels results by bank master country for display or filtering in the result set.

**LAND1_PAYEE** (Country Key):

Country key for payee. When used as a criterion, restricts or labels results by payee country for display or filtering in the result set.

**LANDX** (Name):

Country name. Populated from the country key in the language specified by LANGU for display or filtering in the result set.

**LANDX_EMPFG** (Name):

Country name for payment recipient. Populated for display or filtering in the result set.

**LANDX_LFBK** (Name):

Country name for vendor bank. Populated for display or filtering in the result set.

**LANDX_PAYEE** (Name):

Country name for payee. Populated for display or filtering in the result set.

**LANGU** (Language for texts):

Language key used when retrieving descriptions for country and company code. Affects only the language of description fields in the output; does not change which rows are selected.

**LAUFD** (Run Date):

Payment run date or range. Restricts which payment run lines are included by the date on which the program is run. The EI uses this (or a configurable reference date) to build the selection window and to compute duration.

**LAUFI** (Identification):

Payment run identification. Restricts selection to specific run IDs. Only lines whose run identification matches the supplied values are included. Use for run-level analysis.

**LIFNR** (Vendor):

Restricts selection to specific vendors (payees). Only payment run lines for the given vendor numbers are included. Use standard vendor number format (with or without leading zeros depending on conversion).

**LIFNR_EMPFG** (Supplier):

Vendor/supplier for payment recipient. When used as a criterion, restricts or labels results by recipient vendor for display or filtering in the result set.

**LIFNR_LFBK** (Supplier):

Vendor/supplier from bank details (LFBK). When used as a criterion, restricts or labels results by the vendor linked to the bank account for display or filtering in the result set.

**LNRZA** (Alternative payee):

Restricts selection by alternative payee. Only lines whose alternative payee matches the supplied values are included when applicable. Supports analysis of payee and alternative payee usage.

**LOEVM** (Central deletion flag):

Indicates whether the vendor master record is marked for deletion. When supplied, restricts results to payees with the given deletion-flag status. Used to exclude or include deleted vendors in the result set.

**LOEVM Options:**
- **X**: Central deletion flag set (marked for deletion).
- ** ** (space) or blank: Not set (active vendor).

**NAME1** (Name):

Name (e.g. vendor or company name). Populated for display or filtering in the result set.

**NAME1_EMPFG** (Name):

Name for payment recipient. Populated for display or filtering in the result set.

**NAME1_LFBK** (Name):

Name for vendor bank. Populated for display or filtering in the result set.

**NAME1_PAYEE** (Name):

Name for payee. Populated from the payee vendor master for display or filtering in the result set.

**PAYEE** (Supplier):

Payee vendor (account number of vendor or creditor). When used as a criterion, restricts or labels results by payee for display or filtering in the result set. The EI selects payment run lines where the payee is a one-time vendor.

**RWBTR** (Amount paid):

Amount paid in payment currency. When used as a selection range, restricts which lines are included by amount. Describes the payment amount in the currency of the payment document.

**SPERM** (Central purchasing block):

Indicates whether a central purchasing block is set. When supplied, restricts or labels results by purchasing block status. Populated for display or filtering in the result set.

**SPERM Options:**
- **X**: Block set.
- ** ** (space) or blank: Not set.

**SPERR** (Central posting block):

Indicates whether a central posting block is set. When supplied, restricts or labels results by posting block status. Populated for display or filtering in the result set.

**SPERR Options:**
- **X**: Block set.
- ** ** (space) or blank: Not set.

**SPRAS** (Language Key):

Language key. When used as a criterion, restricts or labels results by language for display or filtering in the result set.

**VBLNR** (Payment document no.):

Payment document number. When used as a criterion, restricts or labels results by payment document for display or filtering in the result set.

**WAERS** (Currency):

Currency key of the payment. When supplied, restricts results to lines in the given currencies. Describes the currency in which the amount paid is expressed.

**XCPDK** (One-time account):

Indicates whether the account is a one-time account. The EI uses this to filter for payees that are one-time vendors. When set, only payment run lines where the payee is a one-time vendor are included.

**XCPDK Options:**
- **X**: One-time account (one-time vendor).
- ** ** (space) or blank: Regular vendor (not one-time).

**XVORL** (Indicator: Only Proposal Run?):

Indicates whether the payment run is a proposal run only. When supplied, restricts selection to proposal or live runs. Only lines matching the supplied value are included.

**XVORL Options:**
- **X**: Proposal run only.
- ** ** (space) or blank: Live/actual run.

**ZBNKL** (Bank number):

Payee bank number (bank key of the payee's bank). Restricts selection by payee bank number. Only lines whose payee bank number matches the supplied values are included.

**ZBNKN** (Payee's bank acct number):

Payee's bank account number. Restricts selection by payee bank account. Only lines whose payee bank account matches the supplied values are included.

**ZBNKS** (Country Key):

Country key for payee bank. Restricts selection by payee bank country. Only lines whose payee bank country matches the supplied values are included.

**ZBUKR** (Paying Company Code):

Paying company code. Restricts selection to specific paying company codes. Only payment run lines belonging to the given company codes are included. Use for organizational scoping.

**ZNME1** (Payee name):

Payee name. When used as a criterion, restricts or labels results by payee name for display or filtering in the result set.


### Parameter Relationships

**Time and Duration Parameters:**
- **BACKDAYS** (and forward days when applicable) define the date window when no date range is supplied; the EI uses this to build the selection range for the payment run date (e.g. LAUFD).
- **DURATION** and **DURATION_UNIT** work together: DURATION holds the numeric value and DURATION_UNIT the unit (e.g. days). The EI uses them to compute and filter by age of the payment run relative to the reference date and to populate the output duration field.

**Payment Run and Organizational Parameters:**
- **LAUFD**, **LAUFI**, **XVORL** define payment run date, identification, and proposal-run indicator; they restrict which payment run lines are selected and appear together in the result.
- **BUKRS** (or paying company code) defines the company code; it restricts which payment runs and payees are included.
- **LIFNR**, **LIFNR_LFBK**, **PAYEE** relate to vendor (payee in REGUH), vendor on bank details (LFBK), and payee; they narrow the vendor and bank-account scope.

**Bank and Payee Parameters:**
- **ZBNKS**, **ZBNKN**, **ZBNKL**, **BANKS**, **BANKL**, **BANKN** define payee bank country, account number, and bank key, and bank master keys; they restrict selection by bank details used in the payment run.
- **KOVON**, **KOBIS** (valid from/to for bank details) can restrict which bank-account validity period is considered.

**Vendor Master and One-Time Indicator:**
- **XCPDK** (one-time account indicator) is used in the code to filter payees that are one-time vendors; it works with vendor selection to scope the result set.
- **KTOKK**, **LOEVM**, **VBUND**, **STKZN**, **LNRZA** restrict vendor account group, deletion flag, group, blocking, and alternative payee; they work together to define which vendor master attributes are included.

**Language and Descriptions:**
- **LANGU** drives the language for country and company code descriptions; it affects only output text, not which rows are selected.


### Default Values

- **BACKDAYS** — Default: `10` (when no date range is supplied, the EI uses a lookback of 10 days from the reference date).
- **DURATION_UNIT** — Default: `D` (days); used for duration calculation and filtering.
- **LANGU** — Default: `E` (English); used for description texts.
- **XCPDK** — Default: `X` (one-time account); the EI filters for payees that are one-time vendors when this is set.

**Note:** Parameters that are not supplied and have no explicit default in the code are used when initial (empty or 0); e.g. empty date ranges lead to the default window above.

### Practical Configuration Examples

**Use Case 1: One-time vendor payments in the last 30 days**
```
BACKDAYS = 30
DURATION_UNIT = D
```
**Purpose:** Identify payment run lines where the payee is a one-time vendor in the last 30 days for treasury and vendor master review.

**Use Case 2: One-time vendor payments by company code**
```
BUKRS = 1000, 2000
BACKDAYS = 14
```
**Purpose:** Focus on one-time vendor payees in selected company codes over the past two weeks for company-code-level control.

**Use Case 3: By payment run date and run ID**
```
LAUFD = 20240101 - 20240131
LAUFI = (specific run IDs if needed)
```
**Purpose:** Review one-time vendor usage for specific payment run periods or runs for audit and reconciliation.

**Use Case 4: By bank and vendor scope**
```
LIFNR = (vendor range)
ZBNKS = US
BANKS = US
BACKDAYS = 7
```
**Purpose:** Monitor one-time vendor payments involving specific vendors or US bank details for focused exception handling.


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_10_07_ONE_TIME_VEND | BANKL | Bank Keys | CHAR(15) | BANKK |
| /SKN/S_SW_10_07_ONE_TIME_VEND | BANKN | Bank account number | CHAR(18) | BANKN |
| /SKN/S_SW_10_07_ONE_TIME_VEND | BANKS | Bank country key | CHAR(3) | BANKS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | BUKRS | Company Code | CHAR(4) | BUKRS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | BUTXT | Name of Company Code or Company | CHAR(25) | BUTXT |
| /SKN/S_SW_10_07_ONE_TIME_VEND | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_10_07_ONE_TIME_VEND | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_10_07_ONE_TIME_VEND | EKORG | Purchasing Organization | CHAR(4) | EKORG |
| /SKN/S_SW_10_07_ONE_TIME_VEND | EKOTX | Description of Purchasing Organization | CHAR(20) | EKOTX |
| /SKN/S_SW_10_07_ONE_TIME_VEND | EMPFG | Payee code | CHAR(16) | EMPFG |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ERDAT | Date on Which Record Was Created | DATS(8) | ERDAT |
| /SKN/S_SW_10_07_ONE_TIME_VEND | KOBIS | Bank details valid to | DATS(8) | KOBIS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | KONZS | Group key | CHAR(10) | KONZS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | KOVON | Bank Details Valid From | DATS(8) | KOVON |
| /SKN/S_SW_10_07_ONE_TIME_VEND | KTOKK | Vendor account group | CHAR(4) | KTOKK |
| /SKN/S_SW_10_07_ONE_TIME_VEND | KUNNR | Customer Number | CHAR(10) | KUNNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAND1 | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAND1_EMPFG | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAND1_LFBK | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAND1_PAYEE | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LANDX | Country Name | CHAR(15) | LANDX |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LANDX_EMPFG | Country Name | CHAR(15) | LANDX |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LANDX_LFBK | Country Name | CHAR(15) | LANDX |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LANDX_PAYEE | Country Name | CHAR(15) | LANDX |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAUFD | Date on Which the Program Is to Be Run | DATS(8) | LAUFD |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LAUFI | Additional Identification | CHAR(6) | LAUFI |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LIFNR | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LIFNR_EMPFG | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LIFNR_LFBK | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LNRZA | Account Number of the Alternative Payee | CHAR(10) | LNRZA |
| /SKN/S_SW_10_07_ONE_TIME_VEND | LOEVM | Central Deletion Flag for Master Record | CHAR(1) | LOEVM_X |
| /SKN/S_SW_10_07_ONE_TIME_VEND | NAME1 | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_07_ONE_TIME_VEND | NAME1_EMPFG | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_07_ONE_TIME_VEND | NAME1_LFBK | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_07_ONE_TIME_VEND | NAME1_PAYEE | Name 1 | CHAR(35) | NAME1_GP |
| /SKN/S_SW_10_07_ONE_TIME_VEND | PAYEE | Account Number of Vendor or Creditor | CHAR(10) | LIFNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | RWBTR | Amount Paid in the Payment Currency | CURR(13,2) | RWBTR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | SPERM | Centrally imposed purchasing block | CHAR(1) | SPERM_X |
| /SKN/S_SW_10_07_ONE_TIME_VEND | SPERR | Central posting block | CHAR(1) | SPERB_X |
| /SKN/S_SW_10_07_ONE_TIME_VEND | SPRAS | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | VBLNR | Document Number of the Payment Document | CHAR(10) | VBLNR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | WAERS | Currency Key | CUKY(5) | WAERS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | XCPDK | Indicator: Is the account a one-time account? | CHAR(1) | XCPDK |
| /SKN/S_SW_10_07_ONE_TIME_VEND | XVORL | Indicator: Only Proposal Run? | CHAR(1) | XVORL |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZBNKL | Bank number of the payee's bank | CHAR(15) | DZBNKL |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZBNKN | Bank account number of the payee | CHAR(18) | DZBNKN |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZBNKS | Country Key | CHAR(3) | DZBNKS |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZBUKR | Paying company code | CHAR(4) | DZBUKR |
| /SKN/S_SW_10_07_ONE_TIME_VEND | ZNME1 | Name of the payee | CHAR(35) | DZNME1 |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_07_ONE_TIME_VEND .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT
*"      T_DATA STRUCTURE  /SKN/S_SW_10_07_ONE_TIME_VEND
*"----------------------------------------------------------------------
  INCLUDE /SKN/PC_SQL_DATA.
  TYPES: BEGIN OF TY_DATA,
* REGUH fields
           LAUFD         TYPE REGUH-LAUFD,
           LAUFI         TYPE REGUH-LAUFI,
           XVORL         TYPE REGUH-XVORL,
           ZBUKR         TYPE REGUH-ZBUKR,
           LIFNR         TYPE REGUH-LIFNR,
           KUNNR         TYPE REGUH-KUNNR,
           EMPFG         TYPE REGUH-EMPFG,
           VBLNR         TYPE REGUH-VBLNR,
           WAERS         TYPE REGUH-WAERS,
           ZBNKS         TYPE REGUH-ZBNKS,
           ZBNKN         TYPE REGUH-ZBNKN,
           ZBNKL         TYPE REGUH-ZBNKL,
           RWBTR         TYPE REGUH-RWBTR,
* LFA1 fields
           PAYEE         TYPE LFA1-LIFNR,
           LAND1_PAYEE   TYPE LFA1-LAND1,
           NAME1_PAYEE   TYPE LFA1-NAME1,
           ERDAT         TYPE LFA1-ERDAT,
           LNRZA         TYPE LFA1-LNRZA,
           LOEVM         TYPE LFA1-LOEVM,
           KTOKK         TYPE LFA1-KTOKK,
           XCPDK         TYPE LFA1-XCPDK,    " One time vendor indic.
           VBUND         TYPE LFA1-VBUND,
           STKZN         TYPE LFA1-STKZN,
           SPRAS         TYPE LFA1-SPRAS,
* LFBK fields
           LIFNR_LFBK    TYPE LFBK-LIFNR,
           BANKS         TYPE LFBK-BANKS,
           BANKL         TYPE LFBK-BANKL,
           BANKN         TYPE LFBK-BANKN,
           KOVON         TYPE LFBK-KOVON,
           KOBIS         TYPE LFBK-KOBIS,
* Additional fields
           DURATION      TYPE /SKN/E_SW_DURATION,
           DURATION_UNIT TYPE /SKN/E_SW_DURATION_UNIT,
         END OF TY_DATA,
         TT_DATA TYPE TABLE OF TY_DATA.
  TYPES: BEGIN OF TY_LFA1,
             LIFNR         TYPE LFA1-LIFNR,
             LAND1         TYPE LFA1-LAND1,
             NAME1         TYPE LFA1-NAME1,
             ERDAT         TYPE LFA1-ERDAT,
             LNRZA         TYPE LFA1-LNRZA,
             LOEVM         TYPE LFA1-LOEVM,
             KTOKK         TYPE LFA1-KTOKK,
             XCPDK         TYPE LFA1-XCPDK,    " One time vendor indic.
             VBUND         TYPE LFA1-VBUND,
             STKZN         TYPE LFA1-STKZN,
             SPRAS         TYPE LFA1-SPRAS,
         END OF TY_LFA1,
         TT_LFA1 TYPE TABLE OF TY_LFA1.
  "-----------------------------------------------
  " 1. Parameters Definition                     "
  "-----------------------------------------------
  DATA_SINGLE: SW_DEST             RFCDEST,
               LANGU               LANGU,
               BACKDAYS            INT4,
               FORWDAYS            INT4,
               XCPDK               XCPDK,
               LOEVM               LOEVM,
               DURATION_UNIT       /SKN/E_SW_DURATION_UNIT,
               DATE_REF_FLD        NAME_FELD,
               CONVERT_KEY         CHAR1.
  DATA_MULTY:   LIFNR             LIFNR,
                BUKRS             DZBUKR,
                LAND1             LAND1_GP,
                LAUFD             LAUFD,
                LAUFI             LAUFI,
                XVORL             XVORL,
                VBUND             RASSC,
                KTOKK             KTOKK,
                STKZN             STKZN,
                LNRZA             LNRZA,
                EMPFG             EMPFG,
                ZBNKS             DZBNKS,
                ZBNKN             DZBNKN,
                ZBNKL             DZBNKL,
                LIFNR_LFBK        LIFNR,
                BANKS             BANKS,
                BANKL             BANKK,
                BANKN             BANKN,
                VALID_FROM        KOVON,
                VALID_TO          KOBIS,
                DURATION          /SKN/E_SW_DURATION,
                DATUM             SYDATUM,
                COUNTER           I.
  DATA: SY_DATLO LIKE SY-DATLO ,
        SY_TIMLO LIKE SY-TIMLO .
  DATA: TIME_DIFF TYPE INT4.
  DATA: FLD(60) TYPE C.
  DATA: REF_DATE TYPE D.
  DATA: SY_TABIX  LIKE SY-TABIX,
        DATE_FROM LIKE SY-DATUM,
        DATE_TO   LIKE SY-DATUM.
  DATA: LV_SHIFT      TYPE DDLENG,
        LV_LENG       TYPE DDLENG,
        LV_DOMNAME    TYPE DD07V-DOMNAME,
        LV_DOMVALUE   TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT     TYPE DD07V-DDTEXT,
        LV_OBJECT     TYPE CDOBJECTV,
        LV_LIFNR      TYPE LIFNR,
        LV_STRUCTURE  TYPE DDOBJNAME,
        LV_INDEX      TYPE I,
        LV_OBJECTCLAS TYPE CDOBJECTCL,
        LV_DOC        TYPE CDCHANGENR,
        LV_COUNT_TMP  TYPE I,
        LV_LINES      TYPE I.
  DATA: LS_DATA  TYPE TY_DATA,
        LS_LFA1  TYPE TY_LFA1,
        LS_LIFNR TYPE /SKN/S_SW_10_LIFNR.
  DATA: LT_DATA  TYPE TT_DATA,
        LT_LFA1  TYPE TT_LFA1,
        LT_LIFNR TYPE TABLE OF /SKN/S_SW_10_LIFNR.
  FIELD-SYMBOLS: <FS_DATA>    LIKE LINE OF T_DATA[],
                          TYPE ANY.
* Set default parameter
  LV_BACKDAYS       = 10.
  LV_FORWDAYS       = 10.
  LV_DURATION_UNIT  = 'D'.
  LV_DATE_REF_FLD   = 'LAUFD'.
  LV_LANGU          = 'E'.
  LV_XCPDK          = 'X'.     " One time vendor
  SELECT_MULTY:  LIFNR,
                 BUKRS,
                 LAND1,
                 LAUFD,
                 VBUND,
                 KTOKK,
                 STKZN,
                 LNRZA,
                 EMPFG,
                 ZBNKS,
                 ZBNKN,
                 ZBNKL,
                 LIFNR_LFBK,
                 BANKS,
                 BANKL,
                 BANKN,
                 VALID_FROM,
                 VALID_TO,
                 COUNTER,
                 DATUM .
  SELECT_SINGLE: SW_DEST,
                 LANGU,
                 BACKDAYS,
                 FORWDAYS,
                 XCPDK,
                 LOEVM,
                 DATE_REF_FLD,
                 CONVERT_KEY,
                 DATE_REF_FLD,
                 DURATION_UNIT.
  "--- Run Cloud Mode -----
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_07_ONE_TIME_VEND'
      IMPORTING
        IS_ALERT = IS_ALERT
      TABLES
        T_SELECT = T_SELECT
        T_DATA   = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
  "--- Run Cloud Mode -----
  CONVERT_MULTY: LIFNR ALPHA.
  CONVERT_MULTY: LIFNR_LFBK ALPHA.
  _SET_SYS_DATE_TIME LV_SW_DEST SY_DATLO SY_TIMLO.
  " Set default value
  IF R_DATUM[] IS INITIAL .
* Backdays
    RS_DATUM-SIGN   = 'I' .
    RS_DATUM-OPTION = 'BT' .
    DATE_FROM       = SY_DATLO - LV_BACKDAYS.
    RS_DATUM-LOW    = DATE_FROM .
    DATE_TO         = SY_DATLO + LV_FORWDAYS.
    RS_DATUM-HIGH   = DATE_TO .
    APPEND RS_DATUM TO R_DATUM.
  ENDIF .
* Date definition
  CASE LV_DATE_REF_FLD.
    WHEN 'LAUFD'.
      IF R_LAUFD[] IS INITIAL AND R_DATUM[] IS NOT INITIAL.
        R_LAUFD[] = R_DATUM[].
      ENDIF.
*    WHEN 'ERDAT'.
*      IF r_erdat[] IS INITIAL AND r_datum[] IS NOT INITIAL.
*        r_erdat[] = r_datum[].
*      ENDIF.
  ENDCASE.
  SELECT R~LAUFD R~LAUFI R~XVORL R~ZBUKR R~LIFNR
         R~KUNNR R~EMPFG R~VBLNR R~WAERS R~ZBNKS R~ZBNKN R~ZBNKL R~RWBTR
         LF~LIFNR AS LIFNR_LFBK LF~BANKS LF~BANKL LF~BANKN LF~KOVON LF~KOBIS
         L~LIFNR AS PAYEE L~LAND1 AS LAND1_PAYEE L~NAME1 AS NAME1_PAYEE L~ERDAT L~LNRZA L~LOEVM
         L~KTOKK L~XCPDK L~VBUND L~STKZN L~SPRAS
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    FROM REGUH AS R INNER JOIN      LFA1  AS L  ON  R~LIFNR  EQ L~LIFNR   " check for One time vendor
                    INNER JOIN      LFBK  AS LF ON  R~ZBNKS  EQ LF~BANKS
                                                AND R~ZBNKN  EQ LF~BANKN
                                                AND R~ZBNKL  EQ LF~BANKL
*                    INNER JOIN      lfa1  AS l2 ON  lf~lifnr EQ l2~lifnr
    WHERE R~LAUFD  IN R_LAUFD[]
    AND   R~LAUFI  IN R_LAUFI[]
    AND   R~XVORL  IN R_XVORL[]
    AND   R~LIFNR  IN R_LIFNR[]
    AND   R~ZBUKR  IN R_BUKRS[]
    AND   R~ZBNKS  IN R_ZBNKS[]
    AND   R~ZBNKN  IN R_ZBNKN[]
    AND   R~ZBNKL  IN R_ZBNKL[]
    AND   LF~LIFNR IN R_LIFNR_LFBK[]
    AND   LF~BANKS IN R_BANKS[]
    AND   LF~BANKN IN R_BANKN[]
    AND   LF~BANKL IN R_BANKL[]
    AND   L~KTOKK  IN R_KTOKK[]
    AND   L~XCPDK  EQ LV_XCPDK             " as One Time Vendor
    AND   L~VBUND  IN R_VBUND[]
    AND   L~STKZN  IN R_STKZN[].
  CHECK LT_DATA IS NOT INITIAL.
  SORT LT_DATA BY LIFNR_LFBK.
*-- Calculate Status Duration (associating to Reference Field (DATE_REF_FLD)
  LOOP AT LT_DATA INTO LS_DATA.
    SY_TABIX = SY-TABIX .
    CONCATENATE 'LS_DATA-' LV_DATE_REF_FLD INTO FLD .
    ASSIGN (FLD) TO .
    CHECK  IS ASSIGNED.
    REF_DATE =  .
    IF NOT REF_DATE IS INITIAL.
      LS_DATA-DURATION_UNIT = LV_DURATION_UNIT.
      CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM      = REF_DATE
          T_FROM      = SY-TIMLO
          D_TO        = SY-DATLO
          T_TO        = SY-TIMLO
          TIME_UNIT   = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF   = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE = 1
          OTHERS      = 2.
      IF SY-SUBRC = 0.
        IF TIME_DIFF < '999999'.
          LS_DATA-DURATION  = TIME_DIFF .
        ELSE.
          LS_DATA-DURATION  = '999999'.
        ENDIF.
      ENDIF.
      MODIFY LT_DATA FROM LS_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE LT_DATA WHERE DURATION  NOT IN R_DURATION .
  CHECK LT_DATA IS NOT INITIAL.
  SELECT LIFNR LAND1 NAME1 ERDAT LNRZA LOEVM
         KTOKK XCPDK VBUND STKZN SPRAS
    INTO CORRESPONDING FIELDS OF TABLE LT_LFA1
    FROM LFA1
    FOR ALL ENTRIES IN LT_DATA
    WHERE LIFNR EQ LT_DATA-LIFNR_LFBK.
*    IF ls_data-matkl IS NOT INITIAL.
** Material group desc.
*      CALL FUNCTION '/SKN/F_SW_10_MAT_GRP_DESC'
*      EXPORTING
*        matkl              = ls_data-matkl
*      IMPORTING
*        matkl_desc         = ls_data-wgbez
**       MATKL_DESC_L       =
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2
*        .
*    ENDIF.
**
*    IF ls_data-bsart IS NOT INITIAL AND ls_data-bstyp IS NOT INITIAL.
**    "-- BSART_DESC
*      CALL FUNCTION '/SKN/F_SW_10_BSART_DESC'
*      EXPORTING
*        bsart            = ls_data-bsart
*        langu            = lv_langu
*        bstyp            = ls_data-bstyp
*      IMPORTING
*        type_desc        = ls_data-batxt
*      EXCEPTIONS
*        wrong_code       = 1
*        OTHERS           = 2.
*    ENDIF.
*
*    IF ls_data-statu IS NOT INITIAL.
*      "-- STATU_DESC
*      lv_domname = 'ESTAK'.
*      lv_domvalue = ls_data-statu.
*
*      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
*      EXPORTING
*        i_domname        = lv_domname
*        i_domvalue       = lv_domvalue
*        langu            = lv_langu
**       SW_DEST          =
*      IMPORTING
*        e_ddtext         = lv_ddtext
*      EXCEPTIONS
*        not_exist        = 1
*        OTHERS           = 2.
*      IF sy-subrc = 0.
*        ls_data-statu_desc = lv_ddtext.
*      ENDIF.
*    ENDIF.
**
*    IF ls_data-bstyp IS NOT INITIAL.
**    "-- BSTYP_DESC
*      lv_domname = 'EBSTYP'.
*      lv_domvalue = <fs_data>-bstyp.
*
*      CALL FUNCTION '/SKN/F_SW_GET_DOMAIN_VALUE'
*      EXPORTING
*        i_domname        = lv_domname
*        i_domvalue       = lv_domvalue
*        langu            = lv_langu
**       SW_DEST          =
*      IMPORTING
*        e_ddtext         = lv_ddtext
*      EXCEPTIONS
*        not_exist        = 1
*        OTHERS           = 2.
*      IF sy-subrc = 0.
*        ls_data-bstyp_desc = lv_ddtext.
*      ENDIF.
*    ENDIF.
**
*    IF ls_data-lifnr IS NOT INITIAL.
**    "--- Get  Vendor Decriptions
*      CALL FUNCTION '/SKN/F_SW_10_VENDOR_DESC'
*      EXPORTING
*        lifnr              = ls_data-lifnr
*      IMPORTING
*        vendor_desc        = ls_data-name1
*      EXCEPTIONS
*        wrong_vendor       = 1
*        OTHERS             = 2.
*
*    ENDIF.
**
*    IF ls_data-ekorg IS NOT INITIAL.
**   "-- EKORG_DESC
*      CALL FUNCTION '/SKN/F_SW_10_PUR_ORG_DESC'
*      EXPORTING
*        ekorg              = ls_data-ekorg
*      IMPORTING
*        pur_org_desc       = ls_data-ekotx
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2.
*
*    ENDIF.
**
**
*    IF ls_data-ekgrp IS NOT INITIAL.
**   "-- EKGRP_DESC
*      CALL FUNCTION '/SKN/F_SW_10_PUR_GRP_DESC'
*      EXPORTING
*        ekgrp              = ls_data-ekgrp
*      IMPORTING
*        pur_grp_desc       = ls_data-eknam
*      EXCEPTIONS
*        wrong_code         = 1
*        OTHERS             = 2.
*    ENDIF.
*    APPEND ls_data TO t_data[].
  SORT LT_LFA1 BY LIFNR.
  LOOP AT LT_DATA INTO LS_DATA.
    CLEAR: T_DATA, LS_LFA1.
    MOVE-CORRESPONDING LS_DATA TO T_DATA.
    READ TABLE LT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = LS_DATA-LIFNR_LFBK
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      T_DATA-LAND1_LFBK = LS_LFA1-LAND1.
      T_DATA-NAME1_LFBK = LS_LFA1-NAME1.
    ENDIF.
    IF T_DATA-LAND1_LFBK IS NOT INITIAL.
      CALL FUNCTION '/SKN/FC_SW_10_COUNTRY_DESC'
        EXPORTING
          LAND1      = T_DATA-LAND1
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          LANDX      = T_DATA-LANDX_LFBK
*         NATIO      =
*         LANDX50    =
*         NATIO50    =
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF T_DATA-LAND1_PAYEE IS NOT INITIAL.
      CALL FUNCTION '/SKN/FC_SW_10_COUNTRY_DESC'
        EXPORTING
          LAND1      = T_DATA-LAND1_PAYEE
          LANGU      = LV_LANGU
          SW_DEST    = LV_SW_DEST
        IMPORTING
          LANDX      = T_DATA-LANDX_PAYEE
*         NATIO      =
*         LANDX50    =
*         NATIO50    =
        EXCEPTIONS
          WRONG_CODE = 1
          OTHERS     = 2.
    ENDIF.
    IF T_DATA-ZBUKR IS NOT INITIAL.
*    "--- Get  BUKRS Decription
      CALL FUNCTION '/SKN/FC_SW_10_COMP_CODE_DESC'
        EXPORTING
          BUKRS          = T_DATA-ZBUKR  " Company Code
          SW_DEST        = LV_SW_DEST
        IMPORTING
          COMP_CODE_DESC = T_DATA-BUTXT  " Name of Company Code or Company
        EXCEPTIONS
          WRONG_CODE     = 1
          OTHERS         = 2.
    ENDIF.
    IF T_DATA-EKORG IS NOT INITIAL.
*   "-- Purch. Org. Desc.
      CALL FUNCTION '/SKN/FC_SW_10_PUR_ORG_DESC'
        EXPORTING
          EKORG        = T_DATA-EKORG
          SW_DEST      = LV_SW_DEST
        IMPORTING
          PUR_ORG_DESC = T_DATA-EKOTX
        EXCEPTIONS
          WRONG_CODE   = 1
          OTHERS       = 2.
    ENDIF.
    APPEND T_DATA.
  ENDLOOP.
  READ TABLE T_DATA INDEX 1.
  CHECK SY-TFILL IS NOT INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```