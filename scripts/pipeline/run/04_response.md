### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 57 parameters listed in the Parameters Reference Table above.

**AD_NAMEFIR** (First Name):

First name of the user who posted the document. Populated from user master when full line detail is requested; used for display.

**AD_NAMELAS** (Last Name):

Last name of the user who posted the document. Populated from user master when full line detail is requested; used for display.

**AEDAT** (Changed On):

Date the record was last changed. Restricts which documents are included by change date; when DATE_REF_FLD = AEDAT, the default date range is applied to this field. Also appears in the result.

**BACKDAYS** (Back days):

Number of days to look back from the current date when building the default monitoring window. When no explicit date range is supplied, the EI uses today minus this value as the start of the window and applies it to the date field selected by DATE_REF_FLD.

**BELNR** (Document Number):

Accounting document number. Restricts which documents are included; also appears in the result.

**BKTXT** (Document Header Text):

Document header text. Restricts which documents are included; also appears in the result.

**BLART** (Document Type):

Document type (e.g. SA, KA). Restricts which documents are included; also appears in the result.

**BLDAT** (Document Date):

Document date in the document. Restricts which documents are included; also appears in the result. When DATE_REF_FLD = BLDAT, the default range is applied to this field.

**BSCHL** (Posting Key):

Posting key. Restricts which line items are included; also appears in the result.

**BSTAT** (Doc.status):

Document status. Restricts which documents are included; also appears in the result.

**BSTAT Options:**
- **X** or other domain values: Set (e.g. posted, parked).
- ** ** (space or initial): Not set / initial.

**BUDAT** (Posting Date):

Posting date in the document. Restricts which documents are included; also appears in the result. When DATE_REF_FLD = BUDAT, the default range is applied to this field.

**BUKRS** (Company Code):

Company code. Restricts which documents are included; also appears in the result.

**BUZEI** (Item):

Line item number within the accounting document. Restricts which line items are included when filtering by item; also appears in the result.

**COMP_OPERATOR** (Operator):

Comparison operator for consistency checks. Used with comparison logic when applicable; values are domain-specific (e.g. EQ, NE, GT, LT).

**CPUDT** (Entry Date):

Day on which the accounting document was entered. Restricts which documents are included; when DATE_REF_FLD = CPUDT (default in code), the default range is applied to this field. Also appears in the result.

**DATE_REF_FLD** (Date reference field):

Name of the date field used as the reference for the default date range and for duration calculation. Determines which date (posting date, entry date, change date, document date, last update) is used when no explicit range is supplied and for computing how long the document has been in the system.

**DATE_REF_FLD Options:**
- **BUDAT**: Posting date in the document.
- **AEDAT**: Changed on.
- **CPUDT**: Entry date (default in code).
- **UPDDT**: Last update.
- **BLDAT**: Document date in the document.

**DMBE2 - DMBE3** (Amount in LC2 – Amount in LC3):

Amount in second and third local currency. Restrict which line items are included by amount; also appear in the result. Business meaning: second and third local currency amounts for reporting.

**DMBTR** (Amount in LC):

Amount in local currency. Restricts which line items are included by amount; also appears in the result. Business meaning: local currency amount of the line item.

**DURATION** (Duration In Time Units):

Duration in the unit given by DURATION_UNIT (e.g. days) between the reference date (from DATE_REF_FLD) and the run date. Restricts which documents are included when a duration filter is applied; also appears in the result.

**DURATION_UNIT** (Duration Unit):

Unit for DURATION (e.g. days, hours). Used with DURATION and DATE_REF_FLD when computing and filtering by how long the document has been in the system.

**DURATION_UNIT Options:**
- **D**: Days.
- **H**: Hours (if supported by the duration calculation function).
- **M**: Minutes (if supported).

**FABKL** (Factory calendar):

Factory calendar key. Used with WFCID and working-days logic to determine factory date and working-day indicator; used for filtering by working days or holidays when configured.

**FACDATE** (Factory date):

Factory calendar date. Populated when a factory calendar is used; used for display and for working-days filtering.

**FULL** ('X' - ALL items of FI document):

When set, the EI returns all line items of each FI document with user name and G/L account description; when not set, returns one record per document header.

**FULL Options:**
- **X**: Return full line-item detail (all items per document).
- ** ** (space or initial): Return header-level records only.

**GJAHR** (Fiscal Year):

Fiscal year. Restricts which documents are included; also appears in the result.

**GRPID** (Session name):

Batch input session name. Restricts which documents are included; also appears in the result.

**GVTYP** (P&L statmt acct type):

P&L statement account type (G/L account type). Restricts which line items are included via G/L account master; also appears in the result.

**HKONT** (G/L Account):

G/L account number. Restricts which line items are included; also appears in the result.

**HKONT_DESC** (G/L Acct Long Text):

G/L account long text. Populated from G/L account master (SKA1/SKAT); used for display.

**HWAE2 - HWAE3** (Local currency 2 – Local currency 3):

Second and third local currency. Business meaning: company code second and third local currency for reporting.

**HWAER** (Local Currency):

Document/local currency. Business meaning: company code local currency in which amounts are stored.

**KTOKS** (Account Group):

G/L account group. Restricts which line items are included via G/L account master; also appears in the result.

**KTOPL** (Chart of Accounts):

Chart of accounts. Restricts which line items are included via G/L account master; also appears in the result.

**KTOPL_T001** (Chart of Accounts):

Chart of accounts (from company code). Used for consistency with company code; also appears in the result.

**KURS2 - KURS3** (Exchange rate 2 – Exchange rate 3):

Exchange rates for second and third local currency. Restrict or display; used for multi-currency reporting.

**KURSF** (Exchange rate):

Exchange rate. Restricts or displays; used for document currency conversion.

**KZKRS** (Group Currency Exchange Rate):

Group currency exchange rate. Used for group currency reporting.

**KZWRS** (Group currency):

Group currency. Business meaning: currency used for group-level reporting.

**LANGU** (Language Key (G/L account)):

Language key used when retrieving G/L account descriptions. When not supplied, a default (e.g. system language) may be used.

**MONAT** (Posting period):

Fiscal period (posting period). Restricts which documents are included; also appears in the result.

**REF_FIELD_NAME1 - REF_FIELD_NAME2** (Field name – Field name):

First and second reference field names for comparison or display. Used when consistency or comparison checks reference specific fields.

**RESULT_COMP** (Value 1 to Compare):

Comparison value for consistency checks. Used with operator and reference fields when filtering or validating.

**SGTXT** (Text):

Line item text. Restricts which line items are included; also appears in the result.

**SHKZG** (Debit/Credit Ind.):

Debit/credit indicator. Restricts which line items are included (e.g. debit or credit only); also appears in the result.

**SHKZG Options:**
- **S**: Debit (Soll).
- **H**: Credit (Haben).

**TCODE** (Transaction Code):

Transaction code used to create the document. Restricts which documents are included; also appears in the result.

**UPDDT** (Last update):

Date of the last document update. Restricts which documents are included; when DATE_REF_FLD = UPDDT, the default range is applied. Also appears in the result.

**USNAM** (User name):

User name who posted the document. Restricts which documents are included; also appears in the result.

**WAERS** (Currency):

Document currency key. Business meaning: transaction/document currency in which the document was entered.

**WAERS_FR** (Foreign Currency):

Foreign currency. Business meaning: foreign currency when the document involves a different currency than local currency.

**WERKS** (Plant):

Plant. Restricts or displays when relevant to the document; also appears in the result.

**WFCID** (Factory calendar ID):

Factory calendar ID. When set, the EI uses the factory calendar to determine working days and holidays; used together with WORKING_DAYS to filter by working days only or holidays only.

**WORKING_DAYS** (Working Day - Y/N):

When WFCID is set, filters results to working days only or to holidays only.

**WORKING_DAYS Options:**
- **X** (or C_YES): Include only working days (exclude holidays).
- ** ** (or C_NO): Include only holidays (exclude working days).
- ** ** (initial): No filtering by working day; all dates included.

**WRBTR** (Amount):

Amount in document currency. Restricts which line items are included by amount; also appears in the result. Business meaning: amount in document currency.

**XBLNR** (Reference):

Reference document number. Restricts which documents are included; also appears in the result.
