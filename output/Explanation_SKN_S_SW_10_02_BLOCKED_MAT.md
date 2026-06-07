# Exception Indicator: MM: Blocked materials ( SW_10_02_BLOCKED_MAT)

## General Overview

This Exception Indicator identifies materials that are blocked or carry blocked status at plant or sales level by reading general material and sales data and enriching results with material descriptions.

This EI serves as an essential control for materials management and order fulfillment by:

- Surfacing materials with cross-distribution-chain or distribution-chain-specific blocked status in scope
- Supporting reviews before procurement, production, or sales processing when blocked materials should not be used
- Enabling filtering by material, sales organization, distribution channel, last-change user, and change date
- Allowing optional removal of the default change-date window when broader historical review is required
- Supporting both on-premise and cloud execution through optional destination routing

Typical use includes periodic checks after status updates, mass maintenance, or interface loads. Results are intended for exception workflows rather than full material master listings.

The routine selects from material general data joined to sales data for the sales view, applies status and organizational filters, resolves material descriptions in the configured language, and raises an alert when matching records exist.


## Problem Description

Failure to monitor blocked materials creates multiple risks across supply chain, sales, and master data stewardship.

**Operational Risks**

- Blocked materials may still appear in planning or availability views until someone notices status flags manually
- Sales or distribution-chain blocks can prevent shipping or billing while plant-level status looks acceptable, or the reverse
- Changes outside the intended monitoring window may be missed when reviews are ad hoc

**Master Data and Control Risks**

- Mass status updates or conversions can leave unintended blocks in place without a targeted exception list
- Inability to filter by sales organization, distribution channel, or change user limits focused cleanup

**Compliance and Visibility Risks**

- Lack of periodic blocked-material reporting weakens evidence that status exceptions were reviewed before downstream use

## Suggested Resolution

**Immediate Response**

- Review each flagged material number, status fields, sales organization, distribution channel, and last-change attributes
- Confirm with materials or sales master owners whether the block is intentional and documented
- Prioritize materials with open demand, production, or customer commitments

**System Assessment**

- Compare exception volume to prior runs using the same status and organizational filters
- Look for concentrations by user, change date, or sales organization that may indicate a project or interface issue
- Validate whether the default change-date window matches the business definition of “recent” blocks

**Corrective Actions**

- Correct erroneous status through standard material maintenance with required approvals
- Tighten monitoring scope after root cause so the queue stays actionable
- Document review outcomes and schedule recurring runs for relevant sales organizations and status values


## Parameters

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
|---|-------|-------------|------|--------|---------|--------------|--------|
| 1 | AENAM | Changed by | CHAR | 12 | 0 | AENAM | USNAM |
| 2 | BACKDAYS | Backdays | INT4 | 10 | 0 | BACKDAYS | BACKDAYS |
| 3 | DATUM | Reference Date | DATS | 8 | 0 | DATUM | DATUM |
| 4 | LAEDA | Last Change | DATS | 8 | 0 | LAEDA | DATUM |
| 5 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 6 | MATNR | Material | CHAR | 18 | 0 | MATNR | MATNR |
| 7 | MSTAV | X-distr.chain status | CHAR | 2 | 0 | MSTAV | VMSTA |
| 8 | NO_DATE_RESTRICTION | 'X' - No date restriction |  | 0 | 0 |  |  |
| 9 | SW_DEST | RFC Destination |  | 0 | 0 |  |  |
| 10 | VKORG | Sales Organization | CHAR | 4 | 0 | VKORG | VKORG |
| 11 | VMSTA | DChain-spec. status | CHAR | 2 | 0 | VMSTA | VMSTA |
| 12 | VTWEG | Distribution Channel | CHAR | 2 | 0 | VTWEG | VTWEG |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 12 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

**AENAM** (Changed by)

Name of the user who last changed the object; paired with change dates for maker accountability in extracts.

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

**DATUM** (Reference Date)

Reference date supplied by the online monitor; used with explicit date selection when deriving the change-date window applied to **LAEDA**.

**LAEDA** (Last Change)

Last changed date of a record.

**LANGU** (Language)

Language key used for language-dependent texts and user-language filtering.

**MATNR** (Material)

Material number used as the primary product key across MM/SD records.

**MSTAV** (X-distr.chain status)

Supports operational control by evaluating x-distr.chain status through MSTAV for each candidate record.

**NO_DATE_RESTRICTION** ('X' - No date restriction)

Flag that disables default date-window filtering when set.

**SW_DEST** (RFC Destination)

SW_DEST selects cloud destination/rfc destination context used for remote execution path.

**VKORG** (Sales Organization)

Sales organization key used for legal/commercial SD scoping.

**VMSTA** (DChain-spec. status)

Treats dchain-spec. status as a discriminator between similar rows that would otherwise look identical in a raw extract.

**VTWEG** (Distribution Channel)

Distribution channel used for SD market/channel segmentation.


### Parameter Relationships

How parameter combinations work together

**Material and status scope:** **MATNR**, **MSTAV** (cross-distribution-chain status on general material data), **VMSTA** (distribution-chain-specific status on the sales view), **VKORG**, and **VTWEG** define which material sales rows are evaluated.

**Change-date window:** **LAEDA** and **DATUM** supply explicit change-date bounds when populated. When the monitor date range is empty, **BACKDAYS** is the fallback that builds a lower bound applied to **LAEDA**; explicit date selections override that fallback. **NO_DATE_RESTRICTION** clears that range when set so selection is not limited by change date.

**Descriptions:** **LANGU** controls the language used when material descriptions are resolved for output.

**Execution path:** **SW_DEST** delegates processing to the cloud function module when populated; otherwise the on-premise selection runs locally.

**User filter:** **AENAM** limits results to materials last changed by selected users when populated.


### Default Values

- **BACKDAYS** - initial - treated as 1 by code
- **LANGU** - initial - treated as E by code

### Practical Example of Parameter Configuration

**Use Case 1: Materials changed in the last day**

**Purpose:** List blocked materials whose general data was changed since yesterday with default backdays.

```
MSTAV = 01
VMSTA = 01
BACKDAYS = 1
LANGU = E
```

**Use Case 2: Sales organization scope**

**Purpose:** Monitor blocked status for one sales organization and distribution channel.

```
VKORG = 1000
VTWEG = 10
MSTAV = Z1
VMSTA = Z1
BACKDAYS = 30
```

**Use Case 3: Specific materials**

**Purpose:** Check selected materials regardless of recent change date.

```
MATNR = 000000000000100001
MATNR = 000000000000100002
NO_DATE_RESTRICTION = X
```

**Use Case 4: Changes by user**

**Purpose:** Review materials last changed by one user in the last seven days.

```
AENAM = JSMITH
BACKDAYS = 7
VKORG = 2000
LANGU = E
```


## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|---|---|---|---|---|
| /SKN/S_SW_10_02_BLOCKED_MAT | AENAM | Changed by | CHAR(12) | AENAM |
| /SKN/S_SW_10_02_BLOCKED_MAT | ERNAM | Created by | CHAR(12) | ERNAM |
| /SKN/S_SW_10_02_BLOCKED_MAT | ERSDA | Created On | DATS(8) | ERSDA |
| /SKN/S_SW_10_02_BLOCKED_MAT | LAEDA | Last Change | DATS(8) | LAEDA |
| /SKN/S_SW_10_02_BLOCKED_MAT | MATNR | Material | CHAR(18) | MATNR |
| /SKN/S_SW_10_02_BLOCKED_MAT | MAT_DESC | Material Description | CHAR(40) | MAKTX |
| /SKN/S_SW_10_02_BLOCKED_MAT | MSTAV | X-distr.chain status | CHAR(2) | MSTAV |
| /SKN/S_SW_10_02_BLOCKED_MAT | MSTDE | Valid from | DATS(8) | MSTDE |
| /SKN/S_SW_10_02_BLOCKED_MAT | VKORG | Sales Organization | CHAR(4) | VKORG |
| /SKN/S_SW_10_02_BLOCKED_MAT | VMSTA | DChain-spec. status | CHAR(2) | VMSTA |
| /SKN/S_SW_10_02_BLOCKED_MAT | VMSTD | Valid from | DATS(8) | VMSTD |
| /SKN/S_SW_10_02_BLOCKED_MAT | VTWEG | Distribution Channel | CHAR(2) | VTWEG |

## ABAP Code

```abap
FUNCTION /SKN/F_SW_10_02_BLOCKED_MAT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_10_02_BLOCKED_MAT OPTIONAL
*"----------------------------------------------------------------------
  DATA : DATE_FROM LIKE SY-DATUM .
  DATA : SPRAS_T TYPE SPRAS .
  DATA : SY_TABIX LIKE SY-TABIX .
  DATA_SINGLE:
               LANGU  LANGU,
               BACKDAYS INT4,
               NO_DATE_RESTRICTION CHAR1.
  LV_BACKDAYS = 1.
  LV_LANGU = 'E'.
  SELECT_SINGLE:
                 LANGU,
                 BACKDAYS,
                 NO_DATE_RESTRICTION.
  DATA_MULTY: MATNR        MATNR,
              MSTAV        MSTAV,
              VMSTA        VMSTA,
              VKORG        VKORG,
              VTWEG        VTWEG,
              AENAM        AENAM,
              LAEDA        LAEDA,
              DATUM        SY-DATUM.
  SELECT_MULTY:
              MATNR,
              MSTAV,
              VMSTA,
              VKORG,
              VTWEG,
              AENAM,
              LAEDA,
              DATUM .
  CONVERT_MULTY: MATNR MATN1.
  CONVERT_SINGLE: LANGU ISOLA.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_10_02_BLOCKED_MAT'
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
  IF LV_NO_DATE_RESTRICTION IS NOT INITIAL.
    REFRESH R_DATUM.
  ENDIF.
  R_LAEDA[] = R_DATUM[].
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA.
  SELECT *
    FROM MARA AS A
    INNER JOIN MVKE AS B
    ON A~MATNR = B~MATNR
    INTO CORRESPONDING FIELDS OF TABLE T_DATA
    WHERE  A~MATNR IN R_MATNR
       AND A~MSTAV IN R_MSTAV
       AND A~LAEDA IN R_LAEDA
       AND A~AENAM IN R_AENAM
       AND B~VMSTA IN R_VMSTA
       AND B~VKORG IN R_VKORG
       AND B~VTWEG IN R_VTWEG
                 .
  "check langu
  SELECT SINGLE SPRAS INTO SPRAS_T
    FROM T002
    WHERE SPRAS = LV_LANGU.
  IF SY-SUBRC <> 0.
    LV_LANGU = 'E'.
  ENDIF.
  LOOP AT T_DATA.
**Material desc
    SY_TABIX = SY-TABIX .
    CALL FUNCTION '/SKN/F_SW_10_MATERIAL_DESC'
      EXPORTING
        MATNR         = T_DATA-MATNR
        LANGU         = LV_LANGU
      IMPORTING
        MATERIAL_DESC = T_DATA-MAT_DESC
      EXCEPTIONS
        WRONG_CODE    = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
*--- Check Alert Information
  READ TABLE T_DATA INDEX 1.
  CHECK NOT SY-TFILL  IS INITIAL .
  IS_ALERT = 'X' .
ENDFUNCTION.
```
