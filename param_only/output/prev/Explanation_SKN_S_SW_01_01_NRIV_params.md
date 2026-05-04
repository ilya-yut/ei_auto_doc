# Parameters: SKN_S_SW_01_01_NRIV

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | AVAILNUMRANGE | Amount of available numbers | CHAR | 20 | 0 | NRTO | CHAR20 |
| 2 | EXTERNIND | External | CHAR | 1 | 0 | NRIND | XFELD |
| 3 | FREENUMABS | Free numbers abs amount | CHAR | 20 | 0 | NRFROM | CHAR20 |
| 4 | FREENUMPERC | Free numbers amount in % | INT1 | 3 | 0 | INT1 | INT1 |
| 5 | FROMNUMBER | From number | CHAR | 20 | 0 | NRFROM | CHAR20 |
| 6 | NRLEVEL | Number Range Status | NUMC | 20 | 0 | NRLEVEL | NUMC20 |
| 7 | NRRANGENR | Number range number | CHAR | 2 | 0 | NRNR | CHAR2 |
| 8 | OBJECT | Object name | CHAR | 10 | 0 | NROBJ | NROBJ |
| 9 | SUBOBJECT | Subobject value | CHAR | 6 | 0 | NRSOBJ | CHAR6 |
| 10 | THRESHOLDABS | Threshold in absolute | CHAR | 20 | 0 | NRFROM | CHAR20 |
| 11 | THRESHOLDPERC | Threshold in % | INT1 | 3 | 0 | INT1 | INT1 |
| 12 | TONUMBER | To number | CHAR | 20 | 0 | NRTO | CHAR20 |
| 13 | TOYEAR | To year | NUMC | 4 | 0 | NRYEAR | GJAHR |


### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 13 parameters listed in the Parameters Reference Table above.

**AVAILNUMRANGE** (Amount of available numbers):

The EI computes this as the interval size (upper bound minus lower bound) for each number range interval. It is used in the percentage calculations for free numbers and threshold and can be used to target intervals of a given size.

**EXTERNIND** (External):

Indicates whether the number range interval is managed as external (customer-assigned numbers) or internal (system-assigned). The EI uses this to distinguish interval types when retrieving and evaluating number range data.

**EXTERNIND Options:**
- **X**: External number range (numbers assigned outside the system).
- ** ** (space): Internal number range (system-assigned numbers).

**FREENUMABS** (Free numbers abs amount):

Free numbers in absolute terms: how many numbers remain available in the interval (upper bound minus current status). The EI calculates this per interval; the parameter supplies the allowed range for this value.

**FREENUMPERC** (Free numbers amount in %):

Free numbers as a percentage of the interval size. The EI derives this from the current status and interval bounds; the parameter supplies the allowed range for this percentage.

**FROMNUMBER** (From number):

Lower bound of the number range interval. It identifies the interval together with object, subobject, number range number, and year, and is used when the EI retrieves and evaluates intervals.

**NRLEVEL** (Number Range Status):

Current status of the number range: the next number to be assigned within the interval. The EI uses it to compute free numbers and threshold (absolute and percentage); the parameter supplies the allowed range for the current status.

**NRRANGENR** (Number range number):

Identifier of the number range interval within an object and subobject for a given year. It distinguishes multiple intervals (e.g. per company code or document type) and is used when the EI retrieves interval data.

**OBJECT** (Object name):

Name of the number range object (e.g. document type or business object for which numbers are assigned). It identifies which number range is evaluated and is used when the EI retrieves intervals.

**SUBOBJECT** (Subobject value):

Subobject value that further qualifies the number range object (e.g. company code or other dimension). It identifies which intervals the EI evaluates together with the object.

**THRESHOLDABS** (Threshold in absolute):

Threshold expressed as the count of numbers already used in the interval (current status minus lower bound). The EI calculates this per interval; the parameter supplies the allowed range for this value.

**THRESHOLDPERC** (Threshold in %):

Threshold as the percentage of the interval already used. The EI derives this from the current status and interval size; the parameter supplies the allowed range for this percentage.

**TONUMBER** (To number):

Upper bound of the number range interval. It defines the interval together with the from number and is used when the EI retrieves and evaluates intervals.

**TOYEAR** (To year):

Fiscal year to which the number range interval belongs. It identifies the interval in time and is used when the EI retrieves interval data.


### Parameter Relationships

**Interval identification parameters:**

OBJECT, SUBOBJECT, NRRANGENR, TOYEAR, TONUMBER, FROMNUMBER, NRLEVEL, and EXTERNIND together define which number range intervals are read. OBJECT and SUBOBJECT identify the number range object; NRRANGENR and TOYEAR identify the interval within it; TONUMBER and FROMNUMBER define the interval bounds; NRLEVEL is the current status; EXTERNIND distinguishes internal vs external intervals. Use these together to target specific objects, years, and interval bounds.

**Free numbers and threshold parameters:**

FREENUMABS, FREENUMPERC, THRESHOLDABS, and THRESHOLDPERC are applied after the EI has computed free numbers and threshold per interval. They restrict which intervals appear in the result (e.g. only intervals with free numbers or usage above or below the given values). Use them together to focus on intervals that are nearly full, nearly empty, or within a given usage range.


### Default Values

- **LANGU** — Default: system language (when not supplied).

### Practical Configuration Examples

**Use Case 1: Monitor specific number range object and subobject**
```
OBJECT = DOCUMENT_TYPE_X
SUBOBJECT = 1000
TOYEAR = 2024
```
**Purpose:** Focus on one number range object and subobject (e.g. document type and company code) for a given fiscal year to review interval usage.

**Use Case 2: Intervals with low free numbers (near exhaustion)**
```
FREENUMABS = 0 - 100
FREENUMPERC = 0 - 10
OBJECT = INV_DOC
```
**Purpose:** Find intervals that have few numbers left in absolute terms and as a percentage, so they can be extended or monitored before exhaustion.

**Use Case 3: High-usage intervals by object and threshold**
```
OBJECT = ORDER_NUM
SUBOBJECT = 2000
THRESHOLDPERC = 80 - 100
THRESHOLDABS = 1000 - 999999
NRRANGENR = 01 - 99
```
**Purpose:** Identify order number intervals for a given subobject that are heavily used (high percentage and high absolute usage), optionally restricting by number range number.

**Use Case 4: Internal intervals in a given year with free-numbers filter**
```
EXTERNIND =  
TOYEAR = 2024
FREENUMABS = 500 - 9999
FREENUMPERC = 20 - 80
OBJECT = MATERIAL_DOC
```
**Purpose:** Review internal (system-assigned) number range intervals for a fiscal year where free numbers and free percentage fall within a band, to plan capacity or consolidation.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_NRIV | AVAILNUMRANGE | To number | CHAR(20) | NRTO |
| /SKN/S_SW_01_01_NRIV | EXTERNIND | Internal (' ') or external ('X') number range flag | CHAR(1) | NRIND |
| /SKN/S_SW_01_01_NRIV | FREENUMABS | From number | CHAR(20) | NRFROM |
| /SKN/S_SW_01_01_NRIV | FREENUMPERC | Dummy for B20 int1 (Local Everywhere) | INT1(3) | INT1 |
| /SKN/S_SW_01_01_NRIV | FROMNUMBER | From number | CHAR(20) | NRFROM |
| /SKN/S_SW_01_01_NRIV | NRLEVEL | Number range status | NUMC(20) | NRLEVEL |
| /SKN/S_SW_01_01_NRIV | NRRANGENR | Number range number | CHAR(2) | NRNR |
| /SKN/S_SW_01_01_NRIV | OBJECT | Name of number range object | CHAR(10) | NROBJ |
| /SKN/S_SW_01_01_NRIV | SUBOBJECT | Number range object subobject value | CHAR(6) | NRSOBJ |
| /SKN/S_SW_01_01_NRIV | THRESHOLDABS | From number | CHAR(20) | NRFROM |
| /SKN/S_SW_01_01_NRIV | THRESHOLDPERC | Dummy for B20 int1 (Local Everywhere) | INT1(3) | INT1 |
| /SKN/S_SW_01_01_NRIV | TONUMBER | To number | CHAR(20) | NRTO |
| /SKN/S_SW_01_01_NRIV | TOYEAR | To fiscal year | NUMC(4) | NRYEAR |
