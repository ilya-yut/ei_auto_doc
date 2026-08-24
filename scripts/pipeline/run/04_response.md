### Parameter Configuration Guidelines

IMPORTANT: This EI defines 40 parameters in the Parameters Reference Table. Configure parameters that affect selection and alerting; parameters marked **Not in use** are declared in the interface but do not change results for this EI.

**ACTFLG** (tp Active Flag)

Activity or processing-active flag on the captured row marking whether the object is live versus completed or inactive.

**ACTIVITY** (IMG Activity)

When harmonized with related filters, img activity on ACTIVITY isolates the highest-risk record families.

**AS4DATE** (Date)

Repository last-changed date of a DDIC or ABAP object for technical object staleness and transport comparisons.

**AS4POS** (Dictionary: Line item)

<mark>Repository object position/index in version-management listings ordering includes or subobjects in a transportable unit.</mark>

**Not in use**
**AS4TIME** (Time)

Repository last-changed time paired with the repository last-changed date for precise DDIC object timestamping.

**AS4USER** (Owner)

<mark>User who last changed a repository object in CTS/SE11-style metadata used for ownership of technical changes.</mark>

**BACKDAYS** (Backdays)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.


**BUFLVL** (Counter)

<mark>Buffer hierarchy level in application-server buffer statistics distinguishing global versus local buffer pools.</mark>

**Not in use**
**BUFPOS** (Dictionary: Line item)

<mark>Buffer entry position or sub-index within a buffer snapshot row for technical buffer-dump analysis.</mark>

**Not in use**
**CDAT** (Created on)

Prevents accidental global scans when created on (CDAT) is meant to stay within a controlled application slice.

**CNAM** (Created By)

When left open per framework rules, CNAM does not restrict created by; when set, only matching rows remain.

**DOMNAM** (Transport Domain)

<mark>Domain name in DDIC describing allowed values for a data element used in technical validation filters.</mark>

**Not in use**
**DURATION** (Duration In Time Units)

Relative-age filter: elapsed interval from the row's reference timestamp to evaluation time, expressed in DURATION_UNIT

**DURATION_UNIT** (Duration Unit)

DURATION_UNIT defines the measurement unit for DURATION calculations.

**DURATION_UNIT Options:**
- H: Hours
- M: Minutes
- D: Days
- F: Full days for specific day filtering

**IMPSING** (Indicator)

Single-record import mode indicator controlling whether IDoc or batch interfaces process one item at a time.

**LANG** (Language Key)

Language key used for language-dependent texts and user-language filtering.

**LINE_NO** (Line)

For operations, line on LINE_NO indicates whether a row belongs in the current monitoring pass versus historical noise.

**LINE_SCAN** (Line source scan)

For operations, line source scan on LINE_SCAN indicates whether a row belongs in the current monitoring pass versus historical noise.

**LOCKFLAG** (Lock/Import Status)

Enqueue lock held flag on technical lock rows indicating whether the lock entry is active.

**OBJ_NAME** (Obj. Name)

<mark>Generic object name key on workflow or technical traces identifying the business object instance.</mark>

**OBJECT** (Object Type)

<mark>Business object discriminator: may hold authorization object, number-range object, BAL object, or other object id per structure.</mark>

**OBJFUNC** (Function)

<mark>Object function or role code describing how a referenced technical object participates in the process.</mark>

**PGMID** (Program ID)

Separates cross-client noise from in-scope work when program id on PGMID correlates with client or user attributes.

**PROJECT** (CTS Project)

<mark>Project id or WBS hierarchy key when extracts join CO-PS project data to operational metrics.</mark>

**STRING_SCAN** (String source scan)

Gives auditors traceable criteria because string source scan on STRING_SCAN is applied consistently before any alert flag is raised.

**Not in use**
**STRING_SEARCH** (String Source Search)

Supports operational control by evaluating string source search through STRING_SEARCH for each candidate record.

**STRKORR** (Higher-Level Request)

<mark>Structure or stack correlation key on short-dump or trace rows grouping related failure events.</mark>

**SUBC** (Program Type)

When harmonized with related filters, program type on SUBC isolates the highest-risk record families.

**SYSNAM** (System Name)

System name in multi-system landscapes identifying the SAP SID or managed system in telemetry exports.

**Not in use**
**TARCLI** (Target client)

Target client for cross-client distribution or transportable changes in CTS-style technical rows.

**TARSYSTEM** (Transport Target)

Target logical system in ALE distribution identifying where replicated objects are sent.

**TEXT** (Short Description)

<mark>General text payload field used for message/contextual filtering.</mark>

**Not in use**
**TRFUNCTION** (Type of request/task)

Transport function code on CTS requests describing import, export, or repair actions on repository.

**TRFUNCTION_TEXT** (Short Description)

Stabilizes week-over-week metrics by fixing short description (TRFUNCTION_TEXT) while allowing duration thresholds to move.

**TRKORR** (Request/Task)

Transport request or task id in CTS identifying a repository change package.

**TRSTATUS** (Status)

Transport request status such as modifiable or released governing CTS workflow.

**TRSTATUS_TEXT** (Short Description)

Connects to alert semantics: rows removed for failing short description on TRSTATUS_TEXT never reach downstream filtering.

**UDAT** (Changed On)

Valuable when comparing health before and after a release—hold changed on on UDAT constant while varying other filters.

**UNAM** (Last changed by)

Mirrors how administrators slice operational lists: last changed by (UNAM) is one lever that shapes which rows are comparable run over run.

**VERN** (Version number)

Captures edge cases where version number (VERN) must be non-default to reproduce a customer-specific monitoring scenario.